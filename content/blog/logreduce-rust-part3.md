---
title: Introducing the BytesLines iterator
date: 2022-03-16
tags: [blog, rust, logreduce]
---

> This post was initially published on the Software Factory blog: https://www.softwarefactory-project.io/introducing-the-byteslines-iterator.html

The BytesLines iterator\'s goal is to provide an API for processing logs
line by line. It processes logs by:

-   Splitting sub line to treat cmd output embedded as a long oneliner.
-   Working with Read objects, such as file decompressors or network
    endpoints.
-   Using zero copy slices to optimize memory usage.
-   Limiting line length to prevent overflow of invalid data.

This blog post presents:

-   Evaluation criterias to compare different implementations.
-   A simple implementation using readline.
-   Why iterators can\'t easily produce pointers.
-   A zero copy implementation using the [bytes](https://docs.rs/bytes/)
    library.

This article is part of a blog post series about the latest logreduce improvements using the Rust programing language. Please see the series\' earlier articles:

-   Part1: [[logreduce-rust-part1]]
-   Part2: [[logreduce-rust-part2]]


## Evaluation criterias

I evaluate the execution time and memory usage to process a 91MB file.
Performances are measured with:

-   [/bin/time -v]{.title-ref} to measure the maximum memory usage.
-   [valgrind]{.title-ref} to collect heap usage.

For example, [grep]{.title-ref} performance is:

``` text
$ /bin/time --format "Run time: %e sec, Max RSS: %M KB" grep anomaly < test.txt
Run time: 0.05 sec, Max RSS: 2380 KB

$ valgrind grep "anomaly" < test.txt |& grep "heap usage"
total heap usage: 305 allocs, 265 frees, 146,413 bytes allocated
```

Grep takes about 50 msec and it needs a bit more than 2MB of memory to
do its job. Valgrind shows a reasonable heap usage, confirming that grep
is well optimized.

The next sections present different implementations for the BytesLines
iterator.

## Readline iterator

One of the main goals is to avoid reading the whole file at once.
Instead, the lines are loaded one at a time using readline. Here is a
basic implementation in Python:

``` python
def logfile_iterator(reader):
    line_number = 0
    while True:
        line = reader.readline()
        if not line:
            break
        line = line.rstrip()
        line_number += 1
        for subline in line.split("\\n"):
            yield (subline, line_number)
```

And here is an equivalent implementation in Rust:

``` rust
/// A struct to hold the state of the iterator.
pub struct BufLines<R: Read> {
    reader: BufReader<R>,
    buffer: String,
    line_number: usize,
}

impl<R: Read> Iterator for BufLines<R> {
    type Item = Result<(String, usize)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buffer.is_empty() {
            self.read_line()
        } else {
            Some(Ok(self.get_line()))
        }
    }
}
```

Notice that Rust doesn\'t yet have a special keyword to *yield* a value.
We need to maintain the context in a structure that is passed each time
the *next* method is called. Here are the two functions of this
iterator:

``` rust
// Read a new line and call get_line
fn read_line(&mut self) -> Option<Result<(String, usize)>> {
    match self.reader.read_line(&mut self.buffer) {
        Ok(n) if n > 0 => {
            // The read succeeded
            self.buffer = self.buffer.trim_end().to_owned();
            Some(Ok(self.get_line()))
        }
        Ok(_) => None,
        Err(e) => Some(Err(e)),
    }
}

// Return the first sub line found in the buffer.
fn get_line(&mut self) -> (String, usize) {
    let line = if let Some((sub_line, rest)) = self.buffer.split_once("\\n") {
        let sub_line = sub_line.clone();
        self.buffer = rest.to_owned();
        sub_line
    } else {
        self.line_number += 1;
        let line = self.buffer.clone();
        self.buffer.clear();
        line
    };
    (line, self.line_number)
}
```

Both of these implementations are using the same algorithm, by calling
the *readline()* helper before splitting sub line on litteral *\"\\n\"*.

Here are their performance characteristics using
*python3-3.10.2-1.fc35.x86_64* and *rustc-1.52.1*:

+--------------+--------+---------+---------+---------------+--------+
| Im           | Max    | Allocs  | Frees   | Bytes         | Run    |
| plementation | RSS    |         |         | allocated     | time   |
+==============+========+=========+=========+===============+========+
| readline.py  | > 7420 | 1,      | 1,      | > 475,434,838 | 0.33   |
|              | > KB   | 814,409 | 810,434 |               | sec    |
+--------------+--------+---------+---------+---------------+--------+
| readline.rs  | > 2260 | >       | >       | > 285,799,923 | 0.15   |
|              | > KB   | 692,114 | 692,112 |               | sec    |
+--------------+--------+---------+---------+---------------+--------+

-   Both implementations work in constant memory. Using a bigger file
    does not increase the Max RSS value.
-   The high heap allocations numbers indicate that each individual line
    is duplicated.
-   Rust code is more verbose, but it performs more efficiently and
    safely because it wraps each line with a Result data type to avoid
    throwing exceptions.

The next sections present a technique to reduce the number of
allocations.

## Iterator and item lifetime

Rust provides facilities for manual memory management, thus it should be
possible to avoid the individual line allocation. The line is already
present in the iterator internal structure, and instead of cloning a new
*String* I would like to return a *&str* reference.

``` rust
impl<R: Read> Iterator for BufLines<R> {
   type Item = Result<&str>;
}
```

... but this does not compile because of this error:

``` text
error[E0106]: missing lifetime specifier
  --> readline.rs:17:24
   |
17 |     type Item = Result<&str>;
   |                        ^ expected named lifetime parameter
   |
help: consider introducing a named lifetime parameter
   |
17 |     type Item<'a> = Result<&'a str>;
   |              ^^^^          ^^^
```

Indeed, the *&str* reference needs a lifetime parameter to match the
owner of the underlying memory. This lifetime parameter is here to
ensure the reference is valid as long as the underlying memory is owned.
Unfortunately, adding the suggested fix does not work:

``` rust
impl<R: Read> Iterator for BufLines<R> {
    type Item<'a> = Result<&'a str>;
}
```

... the compilation still fails because of this new error:

``` text
error[E0658]: generic associated types are unstable
  --> readline.rs:17:5
   |
17 |     type Item<'a> = Result<&'a str>;
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |
   = note: see issue #44265 <https://github.com/rust-lang/rust/issues/44265> for more information
```

The Rust type system is presently not expressive enough to implement
such an iterator. You can read more about this limitation in this
article: [Solving the Generalized Streaming Iterator Problem without
GATs](http://lukaskalbertodt.github.io/2018/08/03/solving-the-generalized-streaming-iterator-problem-without-gats.html).

Even then, it is unclear how the users of this iterator would be able to
keep that reference after the iteration. This is a requirement for
logreduce\'s reports to include the surrounding anomalies\' context.

The next section presents an alternative solution using the
[bytes](https://docs.rs/bytes/) library.

## BytesLines iterator

The [bytes](https://docs.rs/bytes/) library provides a data type which
bundles the reference with the underlying buffer using a reference
counter. You can read more about its implementation in the [Bytes memory
layout](https://docs.rs/bytes/latest/bytes/struct.Bytes.html#memory-layout)
documentation.

This lets us return the line location without doing any memory copy, at
the cost of a slight overhead, to keep track of the size and pointer\'s
owners. Here is how the BytesLines iterator is defined:

``` rust
/// The BytesLines struct holds a single buffer
pub struct BytesLines<R: Read> {
    reader: R,
    buf: BytesMut,
    line_count: usize,
}

impl<R: Read> Iterator for BytesLines<R> {
    type Item = Result<(Bytes, usize)>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buf.is_empty() {
            self.read_slice()
        } else {
            self.get_slice()
        }
    }
}
```

Then, similarly to the previous readline implementation, this iterator
uses two main functions:

-   *read_slice* to fill up the buffer.
-   *get_slice* to split the next line.

``` rust
// Read a new chunk and call get_slice
fn read_slice(&mut self) -> Option<Result<(Bytes, usize)>> {
    let pos = self.buf.len();
    self.buf.resize(pos + CHUNK_SIZE, 0);
    match self.reader.read(&mut self.buf[pos..]) {
        // We read some data.
        Ok(n) if n > 0 => {
            self.buf.truncate(pos + n);
            self.get_slice()
        }

        // We reached the end of the reader, this is the end.
        Ok(_) => None,

        // There was a reading error, we return it.
        Err(e) => Some(Err(e)),
    }
}

// Find the next line in the buffer
fn get_slice(&mut self) -> Option<Result<(Bytes, usize)>> {
    match self.find_next_line() {
        // We haven't found the end of the line, we need more data.
        None => {
            // reserve() will attempt to reclaim space in the buffer.
            self.buf.reserve(CHUNK_SIZE);
            self.read_slice()
        }

        // We found the end of the line, we can return it now.
        Some((pos, sep)) => {
            // split_to() creates a new zero copy reference to the buffer.
            let res = self.buf.split_to(pos).freeze();
            self.buf.advance(sep.len());
            Some(Ok((res, self.line_count)))
        }
    }
}
```

By carefully managing this single buffer, the
[bytes](https://docs.rs/bytes/) library takes care of all the references
counting and memory allocations. In particular, the
[reserve](https://docs.rs/bytes/latest/bytes/struct.BytesMut.html#method.reserve)
function will attempt to reclaim the available space in-place.

Here is a sequence diagram for this implementation:

``` text
⭩- the buffer starts here.
[                          ]          < the buffer is empty, we read a chunk.
[aaaaaaaaaaaa\nbbbbb\nccccc]          < there is a line separator.
╰-----------⮡ next slice
             ⭨
[              bbbbb\nccccc]
              ╰----⮡ next slice
                    ⭨
[                     ccccc]          < the line is incomplete.
     ⭩ we reserve more space and move the left-overs at the begining of the buffer.
[ccccc                           ]    < we read another chunk after the left-overs.
[ccccccc\ndddddddddddddd\neeeeeee]
╰------⮡ next slice
        ⭨
[         dddddddddddddd\neeeeeee]
         ╰-------------⮡ next slice
                        ⭨
[                         eeeeeee]    < the line is incomplete.
       ⭩ we reserve more space and move the left-overs at the begining of the buffer.
[eeeeeee                            ] < we read another chunk after the left-overs.
[eeeeeeeee\n                        ] < we reach the end of file.
╰--------⮡ the last slice
```

Here are the final results:

+--------------+--------+---------+---------+---------------+--------+
| Im           | Max    | Allocs  | Frees   | Bytes         | Run    |
| plementation | RSS    |         |         | allocated     | time   |
+==============+========+=========+=========+===============+========+
| readline.py  | > 7420 | 1,      | 1,      | > 475,434,838 | 0.33   |
|              | > KB   | 814,409 | 810,434 |               | sec    |
+--------------+--------+---------+---------+---------------+--------+
| readline.rs  | > 2260 | >       | >       | > 285,799,923 | 0.15   |
|              | > KB   | 692,114 | 692,112 |               | sec    |
+--------------+--------+---------+---------+---------------+--------+
| b            | > 2068 | > 24    | > 22    | > 265,577     | 0.12   |
| yteslines.rs | > KB   |         |         |               | sec    |
+--------------+--------+---------+---------+---------------+--------+

As you can see, this iterator avoids unnecessary memory copy, and even
though it does more work to satisfy the borrow checker, it is still
faster.

You can find the source code of the benchmarks in the
[logreduce/byteslines-demo](https://github.com/logreduce/byteslines-demo)
project, and you can see the complete version which includes a limiter
for the line length in the
[logreduce-iterator](https://github.com/logreduce/logreduce-rust/blob/main/iterator/src/iterator.rs)
library.

## Conclusion

The Rust programming language provides low-level facilities and
high-level features such as [Algebraic Data
Types](https://doc.rust-lang.org/book/ch06-00-enums.html) and
[Traits](https://doc.rust-lang.org/book/ch10-02-traits.html). This lured
me into trying to avoid cloning the memory and learning more about
Rust\'s unique type system.

Thanks to the [bytes](https://docs.rs/bytes/) library I was able to
efficiently implement this log line iterator. I think it is well worth
the effort since this is such a key component for the project, and I
hope this is going to pay off when processing many files in parallel.

I always welcome feedback, and I would love to be proven wrong. If you
would like to contribute, please join the
[logreduce:matrix.org](https://matrix.to/#/#logreduce:matrix.org) chat
room.

Thank you for reading!
