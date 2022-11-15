..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Locking conventions
*******************

This section documents the locking scheme and rules for all uses of locking
in libitm. We have to support serial(-irrevocable) mode, which is implemented
using a global lock as explained next (called the *serial lock*). To
simplify the overall design, we use the same lock as catch-all locking
mechanism for other infrequent tasks such as (de)registering clone tables or
threads. Besides the serial lock, there are *per-method-group locks* that
are managed by specific method groups (i.e., groups of similar TM concurrency
control algorithms), and lock-like constructs for quiescence-based operations
such as ensuring privatization safety.

Thus, the actions that participate in the libitm-internal locking are either
*active transactions* that do not run in serial mode, *serial
transactions* (which (are about to) run in serial mode), and management tasks
that do not execute within a transaction but have acquired the serial mode
like a serial transaction would do (e.g., to be able to register threads with
libitm). Transactions become active as soon as they have successfully used the
serial lock to announce this globally (see :ref:`serial-lock-impl`). Likewise, transactions become serial transactions as soon as
they have acquired the exclusive rights provided by the serial lock (i.e.,
serial mode, which also means that there are no other concurrent active or
serial transactions). Note that active transactions can become serial
transactions when they enter serial mode during the runtime of the
transaction.

State-to-lock mapping
^^^^^^^^^^^^^^^^^^^^^

Application data is protected by the serial lock if there is a serial
transaction and no concurrently running active transaction (i.e., non-serial).
Otherwise, application data is protected by the currently selected method
group, which might use per-method-group locks or other mechanisms. Also note
that application data that is about to be privatized might not be allowed to be
accessed by nontransactional code until privatization safety has been ensured;
the details of this are handled by the current method group.

libitm-internal state is either protected by the serial lock or accessed
through custom concurrent code. The latter applies to the public/shared part
of a transaction object and most typical method-group-specific state.

The former category (protected by the serial lock) includes:

* The list of active threads that have used transactions.

* The tables that map functions to their transactional clones.

* The current selection of which method group to use.

* Some method-group-specific data, or invariants of this data. For example,
  resetting a method group to its initial state is handled by switching to the
  same method group, so the serial lock protects such resetting as well.

In general, such state is immutable whenever there exists an active
(non-serial) transaction. If there is no active transaction, a serial
transaction (or a thread that is not currently executing a transaction but has
acquired the serial lock) is allowed to modify this state (but must of course
be careful to not surprise the current method group's implementation with such
modifications).

Lock acquisition order
^^^^^^^^^^^^^^^^^^^^^^

To prevent deadlocks, locks acquisition must happen in a globally agreed-upon
order. Note that this applies to other forms of blocking too, but does not
necessarily apply to lock acquisitions that do not block (e.g., trylock()
calls that do not get retried forever). Note that serial transactions are
never return back to active transactions until the transaction has committed.
Likewise, active transactions stay active until they have committed.
Per-method-group locks are typically also not released before commit.

Lock acquisition / blocking rules:

* Transactions must become active or serial before they are allowed to
  use method-group-specific locks or blocking (i.e., the serial lock must be
  acquired before those other locks, either in serial or nonserial mode).

* Any number of threads that do not currently run active transactions can
  block while trying to get the serial lock in exclusive mode. Note that active
  transactions must not block when trying to upgrade to serial mode unless there
  is no other transaction that is trying that (the latter is ensured by the
  serial lock implementation.

* Method groups must prevent deadlocks on their locks. In particular, they
  must also be prepared for another active transaction that has acquired
  method-group-specific locks but is blocked during an attempt to upgrade to
  being a serial transaction. See below for details.

* Serial transactions can acquire method-group-specific locks because there
  will be no other active nor serial transaction.

There is no single rule for per-method-group blocking because this depends on
when a TM method might acquire locks. If no active transaction can upgrade to
being a serial transaction after it has acquired per-method-group locks (e.g.,
when those locks are only acquired during an attempt to commit), then the TM
method does not need to consider a potential deadlock due to serial mode.

If there can be upgrades to serial mode after the acquisition of
per-method-group locks, then TM methods need to avoid those deadlocks:

* When upgrading to a serial transaction, after acquiring exclusive rights
  to the serial lock but before waiting for concurrent active transactions to
  finish (see :ref:`serial-lock-impl` for details),
  we have to wake up all active transactions waiting on the upgrader's
  per-method-group locks.

* Active transactions blocking on per-method-group locks need to check the
  serial lock and abort if there is a pending serial transaction.

* Lost wake-ups have to be prevented (e.g., by changing a bit in each
  per-method-group lock before doing the wake-up, and only blocking on this lock
  using a futex if this bit is not group).

.. todo:: Can reuse serial lock for gl-\*? And if we can, does it make
  sense to introduce further complexity in the serial lock? For gl-\*, we can
  really only avoid an abort if we do -wb and -vbv.

.. _serial-lock-impl:

Serial lock implementation
^^^^^^^^^^^^^^^^^^^^^^^^^^

The serial lock implementation is optimized towards assuming that serial
transactions are infrequent and not the common case. However, the performance
of entering serial mode can matter because when only few transactions are run
concurrently or if there are few threads, then it can be efficient to run
transactions serially.

The serial lock is similar to a multi-reader-single-writer lock in that there
can be several active transactions but only one serial transaction. However,
we do want to avoid contention (in the lock implementation) between active
transactions, so we split up the reader side of the lock into per-transaction
flags that are true iff the transaction is active. The exclusive writer side
remains a shared single flag, which is acquired using a CAS, for example.
On the fast-path, the serial lock then works similar to Dekker's algorithm but
with several reader flags that a serial transaction would have to check.
A serial transaction thus requires a list of all threads with potentially
active transactions; we can use the serial lock itself to protect this list
(i.e., only threads that have acquired the serial lock can modify this list).

We want starvation-freedom for the serial lock to allow for using it to ensure
progress for potentially starved transactions (see :ref:`progress-guarantees` for details). However, this is currently not enforced by
the implementation of the serial lock.

Here is pseudo-code for the read/write fast paths of acquiring the serial
lock (read-to-write upgrade is similar to write_lock:

.. code-block:: c++

  // read_lock:
  tx->shared_state |= active;
  __sync_synchronize(); // or STLD membar, or C++0x seq-cst fence
  while (!serial_lock.exclusive)
    if (spinning_for_too_long) goto slowpath;

  // write_lock:
  if (CAS(&serial_lock.exclusive, 0, this) != 0)
    goto slowpath; // writer-writer contention
  // need a membar here, but CAS already has full membar semantics
  bool need_blocking = false;
  for (t: all txns)
    {
      for (;t->shared_state & active;)
        if (spinning_for_too_long) { need_blocking = true; break; }
    }
  if (need_blocking) goto slowpath;

Releasing a lock in this spin-lock version then just consists of resetting
``tx->shared_state`` to inactive or clearing ``serial_lock.exclusive``.

However, we can't rely on a pure spinlock because we need to get the OS
involved at some time (e.g., when there are more threads than CPUs to run on).
Therefore, the real implementation falls back to a blocking slow path, either
based on pthread mutexes or Linux futexes.

Reentrancy
^^^^^^^^^^

libitm has to consider the following cases of reentrancy:

* Transaction calls unsafe code that starts a new transaction: The outer
  transaction will become a serial transaction before executing unsafe code.
  Therefore, nesting within serial transactions must work, even if the nested
  transaction is called from within uninstrumented code.

* Transaction calls either a transactional wrapper or safe code, which in
  turn starts a new transaction: It is not yet defined in the specification
  whether this is allowed. Thus, it is undefined whether libitm supports this.

* Code that starts new transactions might be called from within any part
  of libitm: This kind of reentrancy would likely be rather complex and can
  probably be avoided. Therefore, it is not supported.

Privatization safety
^^^^^^^^^^^^^^^^^^^^

Privatization safety is ensured by libitm using a quiescence-based approach.
Basically, a privatizing transaction waits until all concurrent active
transactions will either have finished (are not active anymore) or operate on
a sufficiently recent snapshot to not access the privatized data anymore. This
happens after the privatizing transaction has stopped being an active
transaction, so waiting for quiescence does not contribute to deadlocks.

In method groups that need to ensure publication safety explicitly, active
transactions maintain a flag or timestamp in the public/shared part of the
transaction descriptor. Before blocking, privatizers need to let the other
transactions know that they should wake up the privatizer.

.. todo:: How to implement the waiters? Should those flags be
  per-transaction or at a central place? We want to avoid one wake/wait call
  per active transactions, so we might want to use either a tree or combining
  to reduce the syscall overhead, or rather spin for a long amount of time
  instead of doing blocking. Also, it would be good if only the last transaction
  that the privatizer waits for would do the wake-up.

.. _progress-guarantees:

Progress guarantees
^^^^^^^^^^^^^^^^^^^

Transactions that do not make progress when using the current TM method will
eventually try to execute in serial mode. Thus, the serial lock's progress
guarantees determine the progress guarantees of the whole TM. Obviously, we at
least need deadlock-freedom for the serial lock, but it would also be good to
provide starvation-freedom (informally, all threads will finish executing a
transaction eventually iff they get enough cycles).

However, the scheduling of transactions (e.g., thread scheduling by the OS)
also affects the handling of progress guarantees by the TM. First, the TM
can only guarantee deadlock-freedom if threads do not get stopped. Likewise,
low-priority threads can starve if they do not get scheduled when other
high-priority threads get those cycles instead.

If all threads get scheduled eventually, correct lock implementations will
provide deadlock-freedom, but might not provide starvation-freedom. We can
either enforce the latter in the TM's lock implementation, or assume that
the scheduling is sufficiently random to yield a probabilistic guarantee that
no thread will starve (because eventually, a transaction will encounter a
scheduling that will allow it to run). This can indeed work well in practice
but is not necessarily guaranteed to work (e.g., simple spin locks can be
pretty efficient).

Because enforcing stronger progress guarantees in the TM has a higher runtime
overhead, we focus on deadlock-freedom right now and assume that the threads
will get scheduled eventually by the OS (but don't consider threads with
different priorities). We should support starvation-freedom for serial
transactions in the future. Everything beyond that is highly related to proper
contention management across all of the TM (including with TM method to
choose), and is future work.

**TODO** Handling thread priorities: We want to avoid priority inversion
but it's unclear how often that actually matters in practice. Workloads that
have threads with different priorities will likely also require lower latency
or higher throughput for high-priority threads. Therefore, it probably makes
not that much sense (except for eventual progress guarantees) to use
priority inheritance until the TM has priority-aware contention management.
