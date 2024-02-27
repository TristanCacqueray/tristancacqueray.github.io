---
title: Leveraging Cap'n Proto For Logreduce Reports
date: 2023-10-23
tags: [blog, rust, logreduce]
---

> This post was initially published on the Software Factory blog: https://www.softwarefactory-project.io/leveraging-capn-proto-for-logreduce-reports.html

> This post is a follow-up on the previous [[logreduce-web-interface-wasm]]
> article.

This post describes why and how I used [Cap’n Proto][capnp] for the
[logreduce][logreduce] reports format. In three parts, I present:

- Bincode versioning scheme.
- Cap’n Proto.
- Logreduce report encoder/decoder.


## Context and problem statement

Logreduce is a tool that searches for anomalies in build logs.
It can produce reports displayable on web browsers.
Logreduce used to distribute an HTML file setup with
a compatible rendering client.
However, in the context of the new web service interface,
the client may now display reports that were created
by an older version of logreduce.

The problem is that the report format didn't guarantee
backward compatibility: clients were not able to read
reports saved in a previous version.

I evaluated the following formats to solve this problem:

- [Protobuf][protobuf], introduced by Google in 2001.
- [Thrift][thrift], introduced by Facebook in 2007.
- [Cap’n Proto][capnp], introduced by the former maintainer of Protobuf in 2013.
- [Flatbuffers][flatbuffers], introduced by Google in 2014.

Cap'n Proto and Flatbuffers are modern formats designed for performance-critical applications.
They both enable access to the data without parsing/unpacking, using a process known as zero-copy serialization, which is a great feature for logreduce.
Flatbuffers was originally created for games development and it doesn't perform data validation by default.
Therefore I decided to use Cap'n Proto as discussed in the next sections.

[protobuf]: https://protobuf.dev/
[flatbuffers]: https://flatbuffers.dev/
[thrift]: https://thrift.apache.org/


## Bincode versioning scheme

In this section I present the main challenge of using bincode to save data.
Previously, logreduce used bincode to exchange reports.

### Prepare the playground

For the purpose of this article, we'll create a standalone playground.

> If you don't have `cargo`, see this [install rust](https://www.rust-lang.org/tools/install) documentation.

Setup a new project:
```ShellSession
$ cargo new capnp-playground
$ cd capnp-playground
```

Add dependencies:
```ShellSession
$ cargo add bincode@1.3
```

Add serde with the derive feature to generate the encoder/decoder:
```ShellSession
$ cargo add serde@1.0 --features derive
```

And build everything:
```ShellSession
$ cargo run
Hello, world!
```

### Create the initial report

Add the following code to demonstrate bincode usage in the `src/main.rs` file:

```rust
// Copyright (C) 2023 Red Hat
// SPDX-License-Identifier: Apache-2.0

// This program demonstrates data type serialization.
// It does not handle exceptions and unwrap is used to keep the code short.

use serde::{Deserialize, Serialize};
use std::fs::File;

#[derive(Debug, Serialize, Deserialize)]
struct Report {
    baselines: Vec<Content>,
    // list of anomaly omitted
}

#[derive(Debug, Serialize, Deserialize)]
enum Content {
    Zuul {
        change: u64,
        job: String,
    },
    Prow {
        pr: u64,
        url: String,
    },
}

fn encode(report: &Report, file: &str) {
    println!("{}: saving report", file);
    let file = File::create(file).unwrap();
    bincode::serialize_into(file, report).unwrap();
}

fn decode(file: &str) -> Report {
    println!("{}: loading report", file);
    let file = File::open(file).unwrap();
    bincode::deserialize_from(file).unwrap()
}

fn main() {
    match &std::env::args().collect::<Vec<_>>()[..] {
        [_, cmd, fp] if cmd == "encode" => {
            let report = Report {
                baselines: vec![Content::Zuul {
                    change: 42,
                    job: "test".to_string(),
                }],
            };
            encode(&report, fp);
        }
        [_, cmd, fp] if cmd == "decode" => {
            let report = decode(fp);
            println!("got: {:?}", report);
        }
        _ => eprintln!("usage: encode|decode file"),
    };
}
```

Run the following commands to perform a serialization round trip:

```ShellSession
$ cargo run -- encode report.bin
report.bin: saving report

$ cargo run -- decode report.bin
report.bin: loading report
got: Report { baselines: [Zuul { change: 42, job: "test" }] }
```

### Updating the schema

Update the schema, for example, by adding a new field to the Zuul structure:

```diff
--- a/src/main.rs
+++ b/src/main.rs
@@ -14,6 +14,7 @@ enum Content {
     Zuul {
         change: u64,
         job: String,
+        project: String,
     },
     Prow {
         pr: u64,

@@ -38,6 +38,7 @@ fn main() {
                 baselines: vec![Content::Zuul {
                     change: 42,
                     job: "test".to_string(),
+                    project: "demo".to_string(),
                 }],
             };
             encode(&report, fp);
```

Now, decoding the initial report produces this error:

```ShellSession
$ cargo run -- decode report.bin
report.bin: loading report
thread 'main' panicked at src/main.rs:42:37:
called `Result::unwrap()` on an `Err` value: Io(Error {
  kind: UnexpectedEof,
  message: "failed to fill whole buffer"
})
```

That is expected: bincode is not able to deserialize the previous report
because it now expects that Zuul builds have a project.
To address that, we need to use a versioning scheme, for example with such a data type:

```rust
enum Report {
  V1(ReportV1),
  V2(ReportV2)
}
```

As long as we only append new variants, bincode is able to decode
reports saved in a previous version. However this is not very
practical because any change will introduce a new top level version.

Moreover, bincode doesn't check the enum tag. If we move the `Prow`
variant at the top of the `Content` declaration, then bincode will
happily load the report using the wrong tag because the
existing data fits the shape.

In the next section, I introduce a different format to handle
versioning efficiently.


## Introducing Cap’n Proto

Cap’n Proto is a fast data interchange format. The main benefits are:

- strongly-typed schema with first class support for [algebraic data types][adt-wiki] and generic types.
- backward compatible message.
- zero-copy serialization.

### Schema Language

The data format is defined using a special language.
Here is the schema for the report used in the playground above,
copy this to a file named `schema.capnp` at the root of the project:

```capnp
@0xa0b4401e03756e61;

struct Report {
  baselines @0 :List(Content);
}

struct Content {
  union {
    zuul    @0 :Zuul;
    prow    @1 :Prow;
  }

  struct Zuul {
    change  @0 :UInt64;
    job     @1 :Text;
    project @2 :Text;
  }

  struct Prow {
    pr      @0 :UInt64;
    url     @1 :Text;
  }
}
```

This should be self explanatory.
Checkout the full logreduce report schema in this [report/schema.capnp][report-schema],
and the [language documentation][capnp-lang] to learn more about it.

### Code generation

Cap'n Proto provides a compiler named `capnpc` to generate
code for [various languages][capnp-otherlang].
Copy the following build instructions to a file named `build.rs` at the root of the project:

```rust
fn main() {
    capnpc::CompilerCommand::new()
        .file("schema.capnp")
        .output_path("generated/")
        .run()
        .expect("compiling schema.capnp");
}
```

Get the compiler by installing `capnproto` using your favorite package manager,
then run the following commands to generate the code:

```ShellSession
$ cargo add --build capnpc@0.18 && cargo add capnp@0.18
$ cargo build
```

Integrate the generated code in the `main.rs` file by adding:

```rust
mod schema_capnp {
    #![allow(dead_code, unused_qualifications)]
    include!("../generated/schema_capnp.rs");
}
```

This setup introduces new Reader and Builder data types to
read and write reports according to the schema definition.

In the next section I show how to use the new data types.


## Report Encoder/Decoder

As an example usage of the generated data types, we can implement
an encoder/decoder for the existing report struct.

### Encode a report

Here is how to write a report using the `capnp::message` module:

```rust
// This function write the report to the argument implementing the Write trait.
fn capnp_encode(report: &Report, write: impl capnp::io::Write) {
    // Prepare a report message builder
    let mut message = capnp::message::Builder::new_default();
    let mut report_builder = message.init_root::<schema_capnp::report::Builder>();

    // Write a single content.
    fn write_content(content: &Content, builder: schema_capnp::content::Builder) {
        match content {
            Content::Zuul {
                change,
                job,
                project,
            } => {
                // Prepare a zuul builder.
                let mut builder = builder.init_zuul();
                // Write the fields
                builder.set_change(*change);
                builder.set_job(job.as_str().into());
                builder.set_project(project.as_str().into());
            }
            Content::Prow { pr, url } => {
                // Prepare a prow builder.
                let mut builder = builder.init_prow();
                // Write the fields
                builder.set_pr(*pr);
                builder.set_url(url.as_str().into());
            }
        }
    }

    // Write the baselines vector
    {
        // Prepare the list builder.
        let mut baselines_builder = report_builder
            .reborrow()
            .init_baselines(report.baselines.len() as u32);

        for (idx, content) in report.baselines.iter().enumerate() {
            // Prepare the list element builder.
            let content_builder = baselines_builder.reborrow().get(idx as u32);
            // Write the individual baseline.
            write_content(content, content_builder);
        }
    }

    // Write the message
    capnp::serialize::write_message(write, &message).unwrap();
}
```

Update the encode helper:

```diff
@@ -29,7 +84,7 @@ enum Content {
 fn encode(report: &Report, file: &str) {
     println!("{}: saving report", file);
     let file = File::create(file).unwrap();
-    bincode::serialize_into(file, report).unwrap();
+    capnp_encode(report, file)
 }
```

Run the following command to demonstrate the encoding:

```ShellSession
$ cargo run -- encode report.msg
report.msg: saving report
```

### Decode a report

Here is how to read a report:

```rust
// This function read the report from the argument implementing the BufRead trait.
fn capnp_decode(bufread: impl capnp::io::BufRead) -> Report {
    let message_reader =
        capnp::serialize::read_message(bufread, capnp::message::ReaderOptions::new()).unwrap();

    let report_reader = message_reader
        .get_root::<schema_capnp::report::Reader<'_>>()
        .unwrap();

    fn read_content(reader: &schema_capnp::content::Reader) -> Content {
        use schema_capnp::content::Which;
        // Read the generated union data type
        match reader.which().unwrap() {
            Which::Zuul(reader) => {
                // Prepare the reader
                let reader = reader.unwrap();
                // Read the fields
                let change = reader.get_change();
                let job = reader.get_job().unwrap().to_str().unwrap().into();
                let project = reader.get_project().unwrap().to_str().unwrap().into();
                Content::Zuul {
                    change,
                    job,
                    project,
                }
            }
            Which::Prow(reader) => {
                // Prepare the reader
                let reader = reader.unwrap();
                // Read the fields
                let pr = reader.get_pr();
                let url = reader.get_url().unwrap().to_str().unwrap().into();
                Content::Prow { pr, url }
            }
        }
    }

    // Read the baselines vector
    let baselines = {
        // Prepare the reader
        let reader = report_reader.get_baselines().unwrap();
        // Read the baselines
        let mut vec = Vec::with_capacity(reader.len() as usize);
        for reader in reader.into_iter() {
            vec.push(read_content(&reader));
        }
        vec
    };

    Report { baselines }
}
```

Update the decode helper:

```diff
@@ -90,7 +142,7 @@ fn encode(report: &Report, file: &str) {
 fn decode(file: &str) -> Report {
     println!("{}: loading report", file);
     let file = File::open(file).unwrap();
-    bincode::deserialize_from(file).unwrap()
+    capnp_decode(std::io::BufReader::new(file))
 }
```

Run the following command to demonstrate the decoding:

```ShellSession
$ cargo run -- decode report.msg
report.msg: loading report
got: Report { baselines: [Zuul { change: 42, job: "test", project: "demo" }] }
```

This concludes the serialization round trip demonstration using Cap'n Proto.
In the next section I show how to update the schema.


### Evolving the schema

In this section, we'll perform a schema update like we did earlier.

Cap'n Proto prescribes a list of rules to preserve backward compability.
For example, it is not possible to remove fields, they can
only be marked as obsolete, and their memory location will always be
reserved.

It is of course possible to add new fields.
For example, here is how to add a title field to the report struct:

```diff
diff --git a/schema.capnp b/schema.capnp
index add50b9..cd9e996 100644
--- a/schema.capnp
+++ b/schema.capnp
@@ -2,6 +2,7 @@

 struct Report {
   baselines @0 :List(Content);
+  title     @1 :Text;
 }

diff --git a/src/main.rs b/src/main.rs
index 09fc740..40411ad 100644
--- a/src/main.rs
+++ b/src/main.rs
@@ -15,6 +15,7 @@
 #[derive(Debug, Serialize, Deserialize)]
 struct Report {
     baselines: Vec<Content>,
+    title: String,
     // list of anomaly omitted
 }
@@ -58,6 +58,8 @@ fn capnp_encode(report: &Report, write: impl capnp::io::Write) {
         }
     }

+    report_builder.set_title(report.title.as_str().into());

     // Write the message
     capnp::serialize::write_message(write, &message).unwrap();
 }
@@ -111,12 +113,15 @@ fn capnp_decode(bufread: impl capnp::io::BufRead) -> Report {
         vec
     };

-    Report { baselines }
+    let title = report_reader.get_title().unwrap().to_str().unwrap().into();
+
+    Report { baselines, title }
 }
@@ -149,6 +154,7 @@ fn main() {
     match &std::env::args().collect::<Vec<_>>()[..] {
         [_, cmd, fp] if cmd == "encode" => {
             let report = Report {
+                title: "test title".to_string(),
                 baselines: vec![Content::Zuul {
                     change: 42,
                     job: "test".to_string(),
```

Run this command to demonstrate we can read the report previously saved:

```ShellSession
$ cargo run -- decode ./report.msg
report.msg: loading report
got: Report { baselines: [Zuul { change: 42, job: "test", project: "demo" }], title: "" }
```

The decoding succeeded and the report title field got the default value.


## Benchmark

In this section, I measure the performance of Cap'n Proto using a sample report of 1k lines with 2k lines of context.

### CPU usage

Here are the results of the [benchmark][benchmark-code] running on my thinkpad t14 laptop:

```ShellSession
$ cargo bench # lower is better
Decoder/capnp           time:   [296.55 µs 297.00 µs 297.45 µs]
Decoder/bincode         time:   [278.36 µs 279.11 µs 280.01 µs]
Decoder/json            time:   [954.06 µs 956.90 µs 961.04 µs]

Encoder/capnp           time:   [71.704 µs 71.773 µs 71.875 µs]
Encoder/bincode         time:   [26.368 µs 26.394 µs 26.425 µs]
Encoder/json            time:   [162.20 µs 162.33 µs 162.46 µs]

Read/capnp              time:   [0.1119 µs 0.1120 µs 0.1129 µs]
Read/bincode            time:   [294.48 µs 295.36 µs 296.59 µs]
Read/json               time:   [987.78 µs 990.39 µs 995.78 µs]
```

Note that this is a simple benchmark, and I may have missed some optimizations,
though the results match the public [rust serialization benchmark](https://github.com/djkoloski/rust_serialization_benchmark).

The encoder/decoder benchmark loads the full report struct.
Cap'n Proto encoder/decoder are a bit slower because they perform extra validation work to protect against malicious input (see [security considerations][capnp-sec]).

The read benchmark traverses the report to count the number of lines.
In that case, Cap'n Proto is three orders of magnitude faster because
we can access the data directly from the reading buffer, without perfoming any copy.
This is great for rendering in the browser,
because the dom elements need to copy the data anyway,
so we can avoid decoding the report into an intermediary structure.
Here is how the read benchmark is implemented:

```rust
group.bench_function("capnp", |b| b.iter(|| {
    // Create a message reader
    let mut slice: &[u8] = black_box(&encoded_capnp);
    let message_reader = capnp::serialize::read_message_from_flat_slice(
        &mut slice,
        capnp::message::ReaderOptions::new(),
    )
    .unwrap();
    let reader = message_reader
        .get_root::<logreduce_report::schema_capnp::report::Reader<'_>>()
        .unwrap();

    // Traverse the list of log reports
    let count = reader
        .get_log_reports()
        .unwrap()
        .iter()
        .fold(0, |acc, lr| acc + lr.get_anomalies().unwrap().len());
    assert_eq!(count, 1025);
}));

group.bench_function("bincode", |b| b.iter(|| {
    let slice: &[u8] = black_box(&encoded_bincode);
    let report: Report = bincode::deserialize_from(slice).unwrap();
    let count = report
        .log_reports
        .iter()
        .fold(0, |acc, lr| acc + lr.anomalies.len());
    assert_eq!(count, 1025)
}));
```

### Report file size.

Cap'n Proto wire format is a bit heavier and after compression, about 12% bigger than bincode:

```ShellSession
$ du -b report*
162824	report-capnp.bin
114360	report-capnp-packed.bin
123916	report-bincode.bin
149830	report.json

$ gzip report*; du -b report*
59361	report-capnp.bin.gz
61280	report-capnp-packed.bin.gz
52435	report-bincode.bin.gz
50401	report.json.gz
```

Note that Cap'n Proto also supports a packed format, but it has higher runtime costs and worse gzip compressions.

It is surprising that compression works so well on JSON for this schema.
I guess this is because the report is mostly a list of list of text with few structure fields.

### Client code size

Lastly the runtime code is similar, here is the WASM size before and after the [PR introducing capnp][pr-57]:

```ShellSession
$ nix build -o capnp   github:logreduce/logreduce/fb4f69e#web
$ nix build -o bincode github:logreduce/logreduce/2578019#web
$ du -b capnp/*.wasm bincode/*.wasm
529322	capnp/logreduce-web.wasm
531327	bincode/logreduce-web.wasm
```

I guess the runtime code is smaller because capnp does not use the serde machinery.


## Conclusion

Cap'n Proto works well for logreduce. The schema language is simple
to understand and the generated code is easy to work with.
Being able to read the data directly from memory is a great capability that can
enable blazingly fast processing.

Writing the encoder and decoder is a bit of fairly mechanical work.
However doing this work manually enables adding customization, for example, deduplicating the data using a process known as [string interning][interning-wiki].
Future work in rust introspection may enable deriving this work automatically, checkout the [Shepherd’s Oasis blog post][thephd-post] to learn more.

In conclusion, replacing bincode with Cap'n Proto future proofs logreduce reports.
This format adds some negligible storage and processing costs,
in exchange for a backward compatible schema and more efficient data access.
Flatbuffers is also worth considering as it has a lower storage cost, but it requires
more work to verify that the data is safe to be processed.

[logreduce-wasm]: https://www.softwarefactory-project.io/logreduce-wasm-based-web-interface.html
[logreduce]: https://github.com/logreduce/logreduce#readme
[capnp]: https://capnproto.org/
[capnp-otherlang]: https://capnproto.org/otherlang.html
[capnp-lang]: https://capnproto.org/language.html
[capnp-sec]: https://capnproto.org/encoding.html#security-considerations
[adt-wiki]: https://en.wikipedia.org/wiki/Algebraic_data_type
[thephd-post]: https://soasis.org/posts/a-mirror-for-rust-a-plan-for-generic-compile-time-introspection-in-rust/
[interning-wiki]: https://en.wikipedia.org/wiki/String_interning
[pr-57]: https://github.com/logreduce/logreduce/pull/57
[report-schema]: https://github.com/logreduce/logreduce/blob/main/crates/report/schema.capnp
[benchmark-code]: https://github.com/logreduce/logreduce/blob/main/crates/report/benches/bench-report.rs
