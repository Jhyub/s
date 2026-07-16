# S--

Some code is derived from the SNU 4190.310 Programming Languages course.

## Implementation notes

`Mem.load`, `Mem.store`, and `Cache.lookup` use direct mutable-array access and
have O(1) complexity. `Mem.alloc` and `Cache.bind` are amortized O(1) because
their backing arrays occasionally grow. `Cache.merge` is linear in the larger
cache capacity.
