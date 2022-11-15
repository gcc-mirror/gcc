..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

Function list
*************

Initialization and finalization functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

These functions are not part of the ABI.

[No changes] Version checking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Error reporting
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] inTransaction call
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

State manipulation functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There is no ``getTransaction`` function. Transaction identifiers for
nested transactions will be ordered but not necessarily sequential (i.e., for
a nested transaction's identifier :samp:`{IN}` and its enclosing transaction's
identifier :samp:`{IE}`, it is guaranteed that IN >= IE).

[No changes] Source locations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Starting a transaction
^^^^^^^^^^^^^^^^^^^^^^

.. _txn-code-properties:

Transaction code properties
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The bit ``hasNoXMMUpdate`` is instead called ``hasNoVectorUpdate``.
Iff it is set, vector register save/restore is not necessary for any target
machine.

The ``hasNoFloatUpdate`` bit (``0x0010``) is new. Iff it is set, floating
point register save/restore is not necessary for any target machine.

``undoLogCode`` is not supported and a fatal runtime error will be raised
if this bit is set. It is not properly defined in the ABI why barriers
other than undo logging are not present; Are they not necessary (e.g., a
transaction operating purely on thread-local data) or have they been omitted by
the compiler because it thinks that some kind of global synchronization
(e.g., serial mode) might perform better? The specification suggests that the
latter might be the case, but the former seems to be more useful.

The ``readOnly`` bit (``0x4000``) is new.

.. todo:: Lexical or dynamic scope?

``hasNoRetry`` is not supported. If this bit is not set, but
``hasNoAbort`` is set, the library can assume that transaction
rollback will not be requested.

It would be useful if the absence of externally-triggered rollbacks would be
reported for the dynamic scope as well, not just for the lexical scope
(``hasNoAbort``). Without this, a library cannot exploit this together
with flat nesting.

``exceptionBlock`` is not supported because exception blocks are not used.

[No changes] Windows exception state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[No changes] Other machine state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[No changes] Results from beginTransaction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Aborting a transaction
^^^^^^^^^^^^^^^^^^^^^^

``_ITM_rollbackTransaction`` is not supported. ``_ITM_abortTransaction``
is supported but the abort reasons ``exceptionBlockAbort``,
``TMConflict``, and ``userRetry`` are not supported. There are no
exception blocks in general, so the related cases also do not have to be
considered. To encode ``__transaction_cancel [[outer]]``, compilers must
set the new ``outerAbort`` bit (``0x10``) additionally to the
``userAbort`` bit in the abort reason.

Committing a transaction
^^^^^^^^^^^^^^^^^^^^^^^^

The exception handling (EH) scheme is different. The Intel ABI requires the
``_ITM_tryCommitTransaction`` function that will return even when the
commit failed and will have to be matched with calls to either
``_ITM_abortTransaction`` or ``_ITM_commitTransaction``. In contrast,
gcc relies on transactional wrappers for the functions of the Exception
Handling ABI and on one additional commit function (shown below). This allows
the TM to keep track of EH internally and thus it does not have to embed the
cleanup of EH state into the existing EH code in the program.
``_ITM_tryCommitTransaction`` is not supported.
``_ITM_commitTransactionToId`` is also not supported because the
propagation of thrown exceptions will not bypass commits of nested
transactions.

.. code-block:: c++

  void _ITM_commitTransactionEH(void *exc_ptr) ITM_REGPARM;
  void *_ITM_cxa_allocate_exception (size_t);
  void _ITM_cxa_free_exception (void *exc_ptr);
  void _ITM_cxa_throw (void *obj, void *tinfo, void (*dest) (void *));
  void *_ITM_cxa_begin_catch (void *exc_ptr);
  void _ITM_cxa_end_catch (void);

The EH scheme changed in version 6 of GCC.  Previously, the compiler
added a call to ``_ITM_commitTransactionEH`` to commit a transaction if
an exception could be in flight at this position in the code; ``exc_ptr`` is
the address of the current exception and must be non-zero.  Now, the
compiler must catch all exceptions that are about to be thrown out of a
transaction and call ``_ITM_commitTransactionEH`` from the catch clause,
with ``exc_ptr`` being zero.

Note that the old EH scheme never worked completely in GCC's implementation;
libitm currently does not try to be compatible with the old scheme.

The ``_ITM_cxa...`` functions are transactional wrappers for the respective
``__cxa...`` functions and must be called instead of these in transactional
code.  ``_ITM_cxa_free_exception`` is new in GCC 6.

To support this EH scheme, libstdc++ needs to provide one additional function
(``_cxa_tm_cleanup``), which is used by the TM to clean up the exception
handling state while rolling back a transaction:

.. code-block:: c++

  void __cxa_tm_cleanup (void *unthrown_obj, void *cleanup_exc,
                         unsigned int caught_count);

Since GCC 6, ``unthrown_obj`` is not used anymore and always null;
prior to that, ``unthrown_obj`` is non-null if the program called
``__cxa_allocate_exception`` for this exception but did not yet called
``__cxa_throw`` for it. ``cleanup_exc`` is non-null if the program is
currently processing a cleanup along an exception path but has not caught this
exception yet. ``caught_count`` is the nesting depth of
``__cxa_begin_catch`` within the transaction (which can be counted by the TM
using ``_ITM_cxa_begin_catch`` and ``_ITM_cxa_end_catch``);
``__cxa_tm_cleanup`` then performs rollback by essentially performing
``__cxa_end_catch`` that many times.

Exception handling support
^^^^^^^^^^^^^^^^^^^^^^^^^^

Currently, there is no support for functionality like
``__transaction_cancel throw`` as described in the C++ TM specification.
Supporting this should be possible with the EH scheme explained previously
because via the transactional wrappers for the EH ABI, the TM is able to
observe and intercept EH.

[No changes] Transition to serial--irrevocable mode
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Data transfer functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Transactional memory copies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Transactional versions of memmove
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If either the source or destination memory region is to be accessed
nontransactionally, then source and destination regions must not be
overlapping. The respective ``_ITM_memmove`` functions are still
available but a fatal runtime error will be raised if such regions do overlap.
To support this functionality, the ABI would have to specify how the
intersection of the regions has to be accessed (i.e., transactionally or
nontransactionally).

[No changes] Transactional versions of memset
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

[No changes] Logging functions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

User-registered commit and undo actions
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Commit actions will get executed in the same order in which the respective
calls to ``_ITM_addUserCommitAction`` happened. Only
``_ITM_noTransactionId`` is allowed as value for the
``resumingTransactionId`` argument. Commit actions get executed after
privatization safety has been ensured.

Undo actions will get executed in reverse order compared to the order in which
the respective calls to ``_ITM_addUserUndoAction`` happened. The ordering of
undo actions w.r.t. the roll-back of other actions (e.g., data transfers or
memory allocations) is undefined.

``_ITM_getThreadnum`` is not supported currently because its only purpose
is to provide a thread ID that matches some assumed performance tuning output,
but this output is not part of the ABI nor further defined by it.

``_ITM_dropReferences`` is not supported currently because its semantics and
the intention behind it is not entirely clear. The
specification suggests that this function is necessary because of certain
orderings of data transfer undos and the releasing of memory regions (i.e.,
privatization). However, this ordering is never defined, nor is the ordering of
dropping references w.r.t. other events.

[New] Transactional indirect calls
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Indirect calls (i.e., calls through a function pointer) within transactions
should execute the transactional clone of the original function (i.e., a clone
of the original that has been fully instrumented to use the TM runtime), if
such a clone is available. The runtime provides two functions to
register/deregister clone tables:

.. code-block:: c++

  struct clone_entry
  {
    void *orig, *clone;
  };

  void _ITM_registerTMCloneTable (clone_entry *table, size_t entries);
  void _ITM_deregisterTMCloneTable (clone_entry *table);

Registered tables must be writable by the TM runtime, and must be live
throughout the life-time of the TM runtime.

.. todo:: The intention was always to drop the registration functions
  entirely, and create a new ELF Phdr describing the linker-sorted table.  Much
  like what currently happens for ``PT_GNU_EH_FRAME``.
  This work kept getting bogged down in how to represent the :samp:`{N}` different
  code generation variants.  We clearly needed at least two---SW and HW
  transactional clones---but there was always a suggestion of more variants for
  different TM assumptions/invariants.

The compiler can then use two TM runtime functions to perform indirect calls in
transactions:

.. code-block:: c++

  void *_ITM_getTMCloneOrIrrevocable (void *function) ITM_REGPARM;
  void *_ITM_getTMCloneSafe (void *function) ITM_REGPARM;

If there is a registered clone for supplied function, both will return a
pointer to the clone. If not, the first runtime function will attempt to switch
to serial--irrevocable mode and return the original pointer, whereas the second
will raise a fatal runtime error.

[New] Transactional dynamic memory management
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: c++

  void *_ITM_malloc (size_t)
         __attribute__((__malloc__)) ITM_PURE;
  void *_ITM_calloc (size_t, size_t)
         __attribute__((__malloc__)) ITM_PURE;
  void _ITM_free (void *) ITM_PURE;

These functions are essentially transactional wrappers for ``malloc``,
``calloc``, and ``free``. Within transactions, the compiler should
replace calls to the original functions with calls to the wrapper functions.

libitm also provides transactional clones of C++ memory management functions
such as global operator new and delete.  They are part of libitm for historic
reasons but do not need to be part of this ABI.
