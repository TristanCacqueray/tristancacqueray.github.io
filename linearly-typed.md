# Linearly Typed

Linear system enables keeping track of resources, for example to ensure they
are released.

- open() -> LinearHandle
- read(LinearHandle)
- close(~LinearHandle) # consume the resources
