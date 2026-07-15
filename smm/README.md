# S--

Some code is derived from the SNU 4190.310 Programming Languages course.

## Implementation notes

`Mem.load` and `Cache.lookup` both use hash tables and have average O(1)
complexity. `Mem.alloc`, `Mem.store`, and `Cache.bind` are average O(1) as well.
Their constant costs can still differ because the abstractions perform different
bookkeeping.
