..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

[New] Memory model
******************

The ABI should define a memory model and the ordering that is guaranteed for
data transfers and commit/undo actions, or at least refer to another memory
model that needs to be preserved. Without that, the compiler cannot ensure the
memory model specified on the level of the programming language (e.g., by the
C++ TM specification).

For example, if a transactional load is ordered before another load/store, then
the TM runtime must also ensure this ordering when accessing shared state. If
not, this might break the kind of publication safety used in the C++ TM
specification. Likewise, the TM runtime must ensure privatization safety.