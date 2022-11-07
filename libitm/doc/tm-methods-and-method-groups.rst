..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

TM methods and method groups
****************************

libitm supports several ways of synchronizing transactions with each other.
These TM methods (or TM algorithms) are implemented in the form of
subclasses of ``abi_dispatch``, which provide methods for
transactional loads and stores as well as callbacks for rollback and commit.
All methods that are compatible with each other (i.e., that let concurrently
running transactions still synchronize correctly even if different methods
are used) belong to the same TM method group. Pointers to TM methods can be
obtained using the factory methods prefixed with ``dispatch_`` in
:samp:`libitm_i.h`. There are two special methods, ``dispatch_serial`` and
``dispatch_serialirr``, that are compatible with all methods because they
run transactions completely in serial mode.

TM method life cycle
^^^^^^^^^^^^^^^^^^^^

The state of TM methods does not change after construction, but they do alter
the state of transactions that use this method. However, because
per-transaction data gets used by several methods, ``gtm_thread`` is
responsible for setting an initial state that is useful for all methods.
After that, methods are responsible for resetting/clearing this state on each
rollback or commit (of outermost transactions), so that the transaction
executed next is not affected by the previous transaction.

There is also global state associated with each method group, which is
initialized and shut down (``method_group::init()`` and ``fini()``)
when switching between method groups (see :samp:`retry.cc`).

Selecting the default method
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The default method that libitm uses for freshly started transactions (but
not necessarily for restarted transactions) can be set via an environment
variable (:envvar:`ITM_DEFAULT_METHOD`), whose value should be equal to the name
of one of the factory methods returning abi_dispatch subclasses but without
the "dispatch\_" prefix (e.g., "serialirr" instead of
``GTM::dispatch_serialirr()``).

Note that this environment variable is only a hint for libitm and might not
be supported in the future.