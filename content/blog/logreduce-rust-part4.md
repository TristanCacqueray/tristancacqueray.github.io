---
title: Completing the first release of logreduce-rust
date: 2022-03-29
tags: [blog, rust, logreduce]
---

> This post was initially published on the Software Factory blog: https://www.softwarefactory-project.io/completing-the-first-release-of-logreduce-rust.html

I am happy to announce that the
[logreduce-rust](https://github.com/logreduce/logreduce-rust) project
now implements a minimum viable product. It can be used to compare two
remote directories like this: [logreduce diff build-log-url1
build-log-url2]{.title-ref}. This article introduces the latest
features.

In this post I will write about:

-   My choices regarding user input processing.
-   Data type models using static dispatch.
-   Threadpool based concurrency.
-   What my thoughts are on Rust in retrospect.


This article is part of a blog post series about the latest logreduce
improvements using the Rust programing language. Please see the series\'
earlier articles:

-   Part1: [[logreduce-rust-part1]]
-   Part2: [[logreduce-rust-part2]]
-   Part2: [[logreduce-rust-part3]]

## Implementing the model processor

The model processor is the main part that was missing from the new code
base. The goal is to provide a flexible API that combines the log line
iterator, the tokenizer, and the nearest neighbors model. The API
requirements are:

-   The processing needs to work in chunk to leverage efficient
    vectorized operations.
-   The context lines surrounding an anomaly need to be collected so
    that the output can be analyzed offline, without access to the
    content.
-   Duplicated lines should be removed.

Addressing these requirements involves a complex algorithm with a few
edge cases, for example, to keep track of the lines preceeding an
anomaly. Using the beloved Iterator interface I was able to implement a
simple abstraction. The resulting API can be used by simply providing an
Index and a Read value:

``` rust
pub struct ChunkProcessor<'index, R: Read> {
    reader: logreduce_iterator::BytesLines<R>,
    index: &'index ChunkIndex,
    // The raw log line with their global position
    buffer: Vec<(logreduce_iterator::LogLine, usize)>,
    ...
}

impl<'index, R: Read> Iterator for ChunkProcessor<'index, R> {
   type Item = Result<AnomalyContext>;
}
```

You can check the full implementation in the
[process.rs](https://github.com/logreduce/logreduce-rust/blob/main/model/src/process.rs)
module.

## Using static dispatch for the data model

With all the core modules in place, I needed to define a data model to
implement the frontend logic. Logreduce can work with a variety of
content, which can be accessed through different source types:

-   Local path.
-   Remote url.
-   Journald socket.

I initially created a ContentSource trait to define how to get the
sources of a given Content:

``` rust
trait ContentSource {
  fn get_sources(&self) -> Vec<Source>;
}

impl ContentSource for File {
 fn get_sources(&self) -> Vec<Source> {
   if self.path().ends_with('/') {
      // read dir
   } else {
      // a single file
   }
 }
}

impl ContentSource for Build {
 fn get_sources(&self) -> Vec<Source> {
   // list remote sources
 }
}
```

However this adds a bit of complexity to the report implementation. For
example, the state contains the list of baselines sources:

``` rust
struct Report {
  baselines: Vec<dyn ContentSource>,
  ...
}
```

... but this does not work because of this error:

``` text
error[E0277]: the size for values of type `(dyn ContentSource + 'static)` cannot be known at compilation time
   --> model/src/model.rs:25:16
    |
25  |     baselines: Vec<dyn ContentSource>,
    |                ^^^^^^^^^^^^^^^^^^^^^^ doesn't have a size known at compile-time
    |
    = help: the trait `Sized` is not implemented for `(dyn ContentSource + 'static)`
note: required by a bound in `Vec`
```

This makes sense because any type can implement ContentSource, and the
compiler needs to know how much memory they need. Thus we can use a Box
to fix that, which is how most languages solve this problem:

``` rust
struct Report {
    baselines: Vec<Box<dyn ContentSource>>,
}
```

Alternatively, we can use a technique called static dispatch with an
enum\'s pattern match:

``` rust
enum Content {
  File(Path),
  CI(BuildInfo),
  ...
}

impl Content {
  fn get_sources(&self) -> Vec<Source> {
    match self {
      File(fp) => files::get_sources(fp),
      CI(build) => ci::get_sources(build),
      ...
    }
  }
}

struct Report {
  baselines: Vec<Content>
}
```

The content data type is currently defined using static dispatch, which
is simpler for the project. However this means that new types can\'t be
added dynamically.

I documented the complete model in an [Architectural Decision
Record](https://adr.github.io/) you can find here:
[0001-architecture-cli.md](https://github.com/logreduce/logreduce-rust/blob/main/doc/adr/0001-architecture-cli.md).
You can check the implementation in the
[model.rs](https://github.com/logreduce/logreduce-rust/blob/main/model/src/model.rs)
module.

## Crawling logs in parallel using a threadpool

Another interesting feature of logreduce is that it can seamlessly
process remote directories. The goal is to be able to handle a build log
url, served as *Index Of* pages, as if it was a local directory. Thus, I
looked into collecting the log files concurrently so that the tree could
be traversed quickly.

I initially created an AsyncIterator using the
[tokio.rs](https://tokio.rs) library. To limit the amount of workers, I
used the
[FuturesUnordered](https://docs.rs/futures/latest/futures/stream/futures_unordered/struct.FuturesUnordered.html)
structure as explained in this [max number of active futures at a
time](https://users.rust-lang.org/t/batch-execution-of-futures-in-the-tokio-runtime-or-max-number-of-active-futures-at-a-time/47659/4)
discussion. That seemed to work great, but implementing the *handle
response* part was a bit complicated. Some footguns need to be avoided
according to this
[issue](https://github.com/rust-lang/futures-rs/issues/2387). To learn
more about async Rust, check its working group
[wg-async](https://rust-lang.github.io/wg-async/) page.

From my understanding, Tokio is great for long running tasks like
building a server. But for short tasks, such as crawling an http
directory, I find it easier to use a threadpool with
[mpsc](https://doc.rust-lang.org/std/sync/mpsc/), a Multi Producer,
Single Consumer FIFO queue. Thus, here is the main function of the
logreduce\'s httpdir library:

``` rust
fn process(
    visitor: &Visitor,
    client: &Client,
    pool: &ThreadPool,
    tx: &Sender<Message>,
    url: Url,
) {
    if let Some(visitor) = visitor.visit(&url) {
        // Increase reference counts.
        let tx = tx.clone();
        let sub_pool = pool.clone();
        let client = client.clone();

        // Submit the work.
        pool.execute(move || match http_list(&client, url) {
            // We decoded some urls.
            Ok(urls) => {
                for url in urls {
                    if url.path().ends_with('/') {
                        // Recursively call the handler on sub directory.
                        Crawler::process(&visitor, &client, &sub_pool, &tx, url)
                    } else {
                        // Send file location to the mpsc channel.
                        tx.send(Some(Ok(url))).unwrap()
                    }
                }
                // Indicate we are done.
                tx.send(None).unwrap()
            }

            // An error happened, propagates it.
            Err(e) => tx.send(Some(Err(e))).unwrap(),
        });
    }
}
```

You can check the complete implementation in the
[httpdir.rs](https://github.com/logreduce/logreduce-rust/blob/main/httpdir/src/httpdir.rs)
module.

## Completing my first project in Rust

Logreduce is the first project that I wrote using Rust. Here are my
initial impressions of the language.

Pros:

-   Reliable: When working on the model processor, I went through
    multiple iterations, and the code worked after it compiled
    everytime.
-   Network effect: The language attracts many talented developers. For
    example, the [regex](https://docs.rs/regex) and
    [hyper](https://hyper.rs) crates look amazing.
-   Stellar toolchain: Everything looks tightly integrated and snappy. I
    particularly enjoy the workspace feature to structure the code base
    in multiple libraries with their own dependencies.

Cons:

-   Lifetimes are notoriously difficult to understand and I avoided them
    as much as possible to keep the code simple.
-   Macros are appealing but they can be rather cryptic and hard to
    debug.
-   Sometimes the type inference does not work and it needs extra
    annotations. For example, to convert a list of result to a result
    list we can use the turbofish syntax:
    *collect::\<Result\<Vec\<\_\>\>\>()*. In Haskell, this is
    implemented with the *traverse :: (a -\> f b) -\> t a -\> f (t b)*
    function, which I find less complicated.

I am mainly interested in Rust\'s expressive static types. They
generally work the same as in Haskell and OCaml, or any other language
featuring [Algebraic Data
Types](https://doc.rust-lang.org/book/ch06-00-enums.html). Such types
let me fearlessly perform heart surgery on complex code. As explained in
the [Why Functional Programming Doesn\'t
Matter](https://www.youtube.com/watch?v=kZ1P8cHN3pY) talk, expressive
static types give us the dexterity to extend our system in a fairly safe
way. In particular, by making illegal states unrepresentable, we don\'t
have to worry about many kinds of errors. The type system statically
verifies a significant part of our program, enabling us to move fast by
focusing on the most important part.

## Conclusion

Coming from Haskell, the main challenge of using Rust is to be more
careful about the values and their memory. And after going through the
initial bumps, I must say it\'s getting a little easier and I now mostly
understand what the compiler wants.

The Rust implementation of logreduce is now almost feature complete with
the legacy Python code, and I\'m looking forward adding the last
remaining parts:

-   Discovery of baselines for CI build.
-   Supporting systemd-journal sources.
-   Handling tarball transparently.

I always welcome feedback, if you would like to contribute, please join
the [logreduce:matrix.org](https://matrix.to/#/#logreduce:matrix.org)
chat room.

Thank you for reading!
