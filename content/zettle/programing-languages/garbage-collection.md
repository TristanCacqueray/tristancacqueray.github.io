# Garbage Collection

It is a form of automatic memory management and often required for high level language.

[[c]] and [[rust]] do not have a garbage collector.

## Erlang optimization

In https://www.erlang.org/blog/optimizations/ , the author says:

```
My own anecdotal evidence suggests that in most cases there are no measurable performance wins by producing less garbage.

I also remember when an optimization that reduced the size of an Erlang term resulted in a benchmark being consistently slower.
It took the author of that optimization several days of investigation to confirm that the slowdown in the benchmark was not
the fault of his optimization, but by creating less garbage, garbage collection happened at a later time
when it happened to be much more expensive.
```
