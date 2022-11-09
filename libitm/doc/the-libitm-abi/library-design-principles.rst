..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Library design principles
*************************

[No changes] Calling conventions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] TM library algorithms
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Optimized load and store routines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Aligned load and store routines
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Data logging functions
^^^^^^^^^^^^^^^^^^^^^^

The memory locations accessed with transactional loads and stores and the
memory locations whose values are logged must not overlap. This required
separation only extends to the scope of the execution of one transaction
including all the executions of all nested transactions.

The compiler must be consistent (within the scope of a single transaction)
about which memory locations are shared and which are not shared with other
threads (i.e., data must be accessed either transactionally or
nontransactionally). Otherwise, non-write-through TM algorithms would not work.

For memory locations on the stack, this requirement extends to only the
lifetime of the stack frame that the memory location belongs to (or the
lifetime of the transaction, whichever is shorter).  Thus, memory that is
reused for several stack frames could be target of both data logging and
transactional accesses; however, this is harmless because these stack frames'
lifetimes will end before the transaction finishes.

[No changes] Scatter/gather calls
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Serial and irrevocable mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Transaction descriptor
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Store allocation
^^^^^^^^^^^^^^^^

There is no ``getTransaction`` function.

[No changes] Naming conventions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Function pointer encryption
^^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, this is not implemented.
