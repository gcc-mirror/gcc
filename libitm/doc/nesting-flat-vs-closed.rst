..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Nesting: flat vs. closed
************************

We support two different kinds of nesting of transactions. In the case of
*flat nesting*, the nesting structure is flattened and all nested
transactions are subsumed by the enclosing transaction. In contrast,
with *closed nesting*, nested transactions that have not yet committed
can be rolled back separately from the enclosing transactions; when they
commit, they are subsumed by the enclosing transaction, and their effects
will be finally committed when the outermost transaction commits.
*Open nesting* (where nested transactions can commit independently of the
enclosing transactions) are not supported.

Flat nesting is the default nesting mode, but closed nesting is supported and
used when transactions contain user-controlled aborts
(``__transaction_cancel`` statements). We assume that user-controlled
aborts are rare in typical code and used mostly in exceptional situations.
Thus, it makes more sense to use flat nesting by default to avoid the
performance overhead of the additional checkpoints required for closed
nesting. User-controlled aborts will correctly abort the innermost enclosing
transaction, whereas the whole (i.e., outermost) transaction will be restarted
otherwise (e.g., when a transaction encounters data conflicts during
optimistic execution).