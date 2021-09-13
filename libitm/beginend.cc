/* Copyright (C) 2008-2021 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libitm_i.h"
#include <pthread.h>


using namespace GTM;

#if !defined(HAVE_ARCH_GTM_THREAD) || !defined(HAVE_ARCH_GTM_THREAD_DISP)
extern __thread gtm_thread_tls _gtm_thr_tls;
#endif

// Put this at the start of a cacheline so that serial_lock's writers and
// htm_fastpath fields are on the same cacheline, so that HW transactions
// only have to pay one cacheline capacity to monitor both.
gtm_rwlock GTM::gtm_thread::serial_lock
  __attribute__((aligned(HW_CACHELINE_SIZE)));
gtm_thread *GTM::gtm_thread::list_of_threads = 0;
unsigned GTM::gtm_thread::number_of_threads = 0;

/* ??? Move elsewhere when we figure out library initialization.  */
uint64_t GTM::gtm_spin_count_var = 1000;

#ifdef HAVE_64BIT_SYNC_BUILTINS
static atomic<_ITM_transactionId_t> global_tid;
#else
static _ITM_transactionId_t global_tid;
static pthread_mutex_t global_tid_lock = PTHREAD_MUTEX_INITIALIZER;
#endif


// Provides a on-thread-exit callback used to release per-thread data.
static pthread_key_t thr_release_key;
static pthread_once_t thr_release_once = PTHREAD_ONCE_INIT;

/* Allocate a transaction structure.  */
void *
GTM::gtm_thread::operator new (size_t s)
{
  void *tx;

  assert(s == sizeof(gtm_thread));

  tx = xmalloc (sizeof (gtm_thread), true);
  memset (tx, 0, sizeof (gtm_thread));

  return tx;
}

/* Free the given transaction. Raises an error if the transaction is still
   in use.  */
void
GTM::gtm_thread::operator delete(void *tx)
{
  free(tx);
}

static void
thread_exit_handler(void *)
{
  gtm_thread *thr = gtm_thr();
  if (thr)
    delete thr;
  set_gtm_thr(0);
}

static void
thread_exit_init()
{
  if (pthread_key_create(&thr_release_key, thread_exit_handler))
    GTM_fatal("Creating thread release TLS key failed.");
}


GTM::gtm_thread::~gtm_thread()
{
  if (nesting > 0)
    GTM_fatal("Thread exit while a transaction is still active.");

  // Deregister this transaction.
  serial_lock.write_lock ();
  gtm_thread **prev = &list_of_threads;
  for (; *prev; prev = &(*prev)->next_thread)
    {
      if (*prev == this)
	{
	  *prev = (*prev)->next_thread;
	  break;
	}
    }
  number_of_threads--;
  number_of_threads_changed(number_of_threads + 1, number_of_threads);
  serial_lock.write_unlock ();
}

GTM::gtm_thread::gtm_thread ()
{
  // This object's memory has been set to zero by operator new, so no need
  // to initialize any of the other primitive-type members that do not have
  // constructors.
  shared_state.store(-1, memory_order_relaxed);

  // Register this transaction with the list of all threads' transactions.
  serial_lock.write_lock ();
  next_thread = list_of_threads;
  list_of_threads = this;
  number_of_threads++;
  number_of_threads_changed(number_of_threads - 1, number_of_threads);
  serial_lock.write_unlock ();

  init_cpp_exceptions ();

  if (pthread_once(&thr_release_once, thread_exit_init))
    GTM_fatal("Initializing thread release TLS key failed.");
  // Any non-null value is sufficient to trigger destruction of this
  // transaction when the current thread terminates.
  if (pthread_setspecific(thr_release_key, this))
    GTM_fatal("Setting thread release TLS key failed.");
}

static inline uint32_t
choose_code_path(uint32_t prop, abi_dispatch *disp)
{
  if ((prop & pr_uninstrumentedCode) && disp->can_run_uninstrumented_code())
    return a_runUninstrumentedCode;
  else
    return a_runInstrumentedCode;
}

#ifdef TARGET_BEGIN_TRANSACTION_ATTRIBUTE
/* This macro can be used to define target specific attributes for this
   function.  For example, S/390 requires floating point to be disabled in
   begin_transaction.  */
TARGET_BEGIN_TRANSACTION_ATTRIBUTE
#endif
uint32_t
GTM::gtm_thread::begin_transaction (uint32_t prop, const gtm_jmpbuf *jb)
{
  static const _ITM_transactionId_t tid_block_size = 1 << 16;

  gtm_thread *tx;
  abi_dispatch *disp;
  uint32_t ret;

  // ??? pr_undoLogCode is not properly defined in the ABI. Are barriers
  // omitted because they are not necessary (e.g., a transaction on thread-
  // local data) or because the compiler thinks that some kind of global
  // synchronization might perform better?
  if (unlikely(prop & pr_undoLogCode))
    GTM_fatal("pr_undoLogCode not supported");

#ifdef USE_HTM_FASTPATH
  // HTM fastpath.  Only chosen in the absence of transaction_cancel to allow
  // using an uninstrumented code path.
  // The fastpath is enabled only by dispatch_htm's method group, which uses
  // serial-mode methods as fallback.  Serial-mode transactions cannot execute
  // concurrently with HW transactions because the latter monitor the serial
  // lock's writer flag and thus abort if another thread is or becomes a
  // serial transaction.  Therefore, if the fastpath is enabled, then a
  // transaction is not executing as a HW transaction iff the serial lock is
  // write-locked.  Also, HW transactions monitor the fastpath control
  // variable, so that they will only execute if dispatch_htm is still the
  // current method group.  This allows us to use htm_fastpath and the serial
  // lock's writers flag to reliable determine whether the current thread runs
  // a HW transaction, and thus we do not need to maintain this information in
  // per-thread state.
  // If an uninstrumented code path is not available, we can still run
  // instrumented code from a HW transaction because the HTM fastpath kicks
  // in early in both begin and commit, and the transaction is not canceled.
  // HW transactions might get requests to switch to serial-irrevocable mode,
  // but these can be ignored because the HTM provides all necessary
  // correctness guarantees.  Transactions cannot detect whether they are
  // indeed in serial mode, and HW transactions should never need serial mode
  // for any internal changes (e.g., they never abort visibly to the STM code
  // and thus do not trigger the standard retry handling).
#ifndef HTM_CUSTOM_FASTPATH
  if (likely(serial_lock.get_htm_fastpath() && (prop & pr_hasNoAbort)))
    {
      // Note that the snapshot of htm_fastpath that we take here could be
      // outdated, and a different method group than dispatch_htm may have
      // been chosen in the meantime.  Therefore, take care not not touch
      // anything besides the serial lock, which is independent of method
      // groups.
      for (uint32_t t = serial_lock.get_htm_fastpath(); t; t--)
	{
	  uint32_t ret = htm_begin();
	  if (htm_begin_success(ret))
	    {
	      // We are executing a transaction now.
	      // Monitor the writer flag in the serial-mode lock, and abort
	      // if there is an active or waiting serial-mode transaction.
	      // Also checks that htm_fastpath is still nonzero and thus
	      // HW transactions are allowed to run.
	      // Note that this can also happen due to an enclosing
	      // serial-mode transaction; we handle this case below.
	      if (unlikely(serial_lock.htm_fastpath_disabled()))
		htm_abort();
	      else
		// We do not need to set a_saveLiveVariables because of HTM.
		return (prop & pr_uninstrumentedCode) ?
		    a_runUninstrumentedCode : a_runInstrumentedCode;
	    }
	  // The transaction has aborted.  Don't retry if it's unlikely that
	  // retrying the transaction will be successful.
	  if (!htm_abort_should_retry(ret))
	    break;
	  // Check whether the HTM fastpath has been disabled.
	  if (!serial_lock.get_htm_fastpath())
	    break;
	  // Wait until any concurrent serial-mode transactions have finished.
	  // This is an empty critical section, but won't be elided.
	  if (serial_lock.htm_fastpath_disabled())
	    {
	      tx = gtm_thr();
	      if (unlikely(tx == NULL))
	        {
	          // See below.
	          tx = new gtm_thread();
	          set_gtm_thr(tx);
	        }
	      // Check whether there is an enclosing serial-mode transaction;
	      // if so, we just continue as a nested transaction and don't
	      // try to use the HTM fastpath.  This case can happen when an
	      // outermost relaxed transaction calls unsafe code that starts
	      // a transaction.
	      if (tx->nesting > 0)
		break;
	      // Another thread is running a serial-mode transaction.  Wait.
	      serial_lock.read_lock(tx);
	      serial_lock.read_unlock(tx);
	      // TODO We should probably reset the retry count t here, unless
	      // we have retried so often that we should go serial to avoid
	      // starvation.
	    }
	}
    }
#else
  // If we have a custom HTM fastpath in ITM_beginTransaction, we implement
  // just the retry policy here.  We communicate with the custom fastpath
  // through additional property bits and return codes, and either transfer
  // control back to the custom fastpath or run the fallback mechanism.  The
  // fastpath synchronization algorithm itself is the same.
  // pr_HTMRetryableAbort states that a HW transaction started by the custom
  // HTM fastpath aborted, and that we thus have to decide whether to retry
  // the fastpath (returning a_tryHTMFastPath) or just proceed with the
  // fallback method.
  if (likely(serial_lock.get_htm_fastpath() && (prop & pr_HTMRetryableAbort)))
    {
      tx = gtm_thr();
      if (unlikely(tx == NULL))
        {
          // See below.
          tx = new gtm_thread();
          set_gtm_thr(tx);
        }
      // If this is the first abort, reset the retry count.  We abuse
      // restart_total for the retry count, which is fine because our only
      // other fallback will use serial transactions, which don't use
      // restart_total but will reset it when committing.
      if (!(prop & pr_HTMRetriedAfterAbort))
	tx->restart_total = gtm_thread::serial_lock.get_htm_fastpath();

      if (--tx->restart_total > 0)
	{
	  // Wait until any concurrent serial-mode transactions have finished.
	  // Essentially the same code as above.
	  if (!serial_lock.get_htm_fastpath())
	    goto stop_custom_htm_fastpath;
	  if (serial_lock.htm_fastpath_disabled())
	    {
	      if (tx->nesting > 0)
		goto stop_custom_htm_fastpath;
	      serial_lock.read_lock(tx);
	      serial_lock.read_unlock(tx);
	    }
	  // Let ITM_beginTransaction retry the custom HTM fastpath.
	  return a_tryHTMFastPath;
	}
    }
 stop_custom_htm_fastpath:
#endif
#endif

  tx = gtm_thr();
  if (unlikely(tx == NULL))
    {
      // Create the thread object. The constructor will also set up automatic
      // deletion on thread termination.
      tx = new gtm_thread();
      set_gtm_thr(tx);
    }

  if (tx->nesting > 0)
    {
      // This is a nested transaction.
      // Check prop compatibility:
      // The ABI requires pr_hasNoFloatUpdate, pr_hasNoVectorUpdate,
      // pr_hasNoIrrevocable, pr_aWBarriersOmitted, pr_RaRBarriersOmitted, and
      // pr_hasNoSimpleReads to hold for the full dynamic scope of a
      // transaction. We could check that these are set for the nested
      // transaction if they are also set for the parent transaction, but the
      // ABI does not require these flags to be set if they could be set,
      // so the check could be too strict.
      // ??? For pr_readOnly, lexical or dynamic scope is unspecified.

      if (prop & pr_hasNoAbort)
	{
	  // We can use flat nesting, so elide this transaction.
	  if (!(prop & pr_instrumentedCode))
	    {
	      if (!(tx->state & STATE_SERIAL) ||
		  !(tx->state & STATE_IRREVOCABLE))
		tx->serialirr_mode();
	    }
	  // Increment nesting level after checking that we have a method that
	  // allows us to continue.
	  tx->nesting++;
	  return choose_code_path(prop, abi_disp());
	}

      // The transaction might abort, so use closed nesting if possible.
      // pr_hasNoAbort has lexical scope, so the compiler should really have
      // generated an instrumented code path.
      assert(prop & pr_instrumentedCode);

      // Create a checkpoint of the current transaction.
      gtm_transaction_cp *cp = tx->parent_txns.push();
      cp->save(tx);
      new (&tx->alloc_actions) aa_tree<uintptr_t, gtm_alloc_action>();

      // Check whether the current method actually supports closed nesting.
      // If we can switch to another one, do so.
      // If not, we assume that actual aborts are infrequent, and rather
      // restart in _ITM_abortTransaction when we really have to.
      disp = abi_disp();
      if (!disp->closed_nesting())
	{
	  // ??? Should we elide the transaction if there is no alternative
	  // method that supports closed nesting? If we do, we need to set
	  // some flag to prevent _ITM_abortTransaction from aborting the
	  // wrong transaction (i.e., some parent transaction).
	  abi_dispatch *cn_disp = disp->closed_nesting_alternative();
	  if (cn_disp)
	    {
	      disp = cn_disp;
	      set_abi_disp(disp);
	    }
	}
    }
  else
    {
      // Outermost transaction
      disp = tx->decide_begin_dispatch (prop);
      set_abi_disp (disp);
    }

  // Initialization that is common for outermost and nested transactions.
  tx->prop = prop;
  tx->nesting++;

  tx->jb = *jb;

  // As long as we have not exhausted a previously allocated block of TIDs,
  // we can avoid an atomic operation on a shared cacheline.
  if (tx->local_tid & (tid_block_size - 1))
    tx->id = tx->local_tid++;
  else
    {
#ifdef HAVE_64BIT_SYNC_BUILTINS
      // We don't really care which block of TIDs we get but only that we
      // acquire one atomically; therefore, relaxed memory order is
      // sufficient.
      tx->id = global_tid.fetch_add(tid_block_size, memory_order_relaxed);
      tx->local_tid = tx->id + 1;
#else
      pthread_mutex_lock (&global_tid_lock);
      global_tid += tid_block_size;
      tx->id = global_tid;
      tx->local_tid = tx->id + 1;
      pthread_mutex_unlock (&global_tid_lock);
#endif
    }

  // Log the number of uncaught exceptions if we might have to roll back this
  // state.
  if (tx->cxa_uncaught_count_ptr != 0)
    tx->cxa_uncaught_count = *tx->cxa_uncaught_count_ptr;

  // Run dispatch-specific restart code. Retry until we succeed.
  GTM::gtm_restart_reason rr;
  while ((rr = disp->begin_or_restart()) != NO_RESTART)
    {
      tx->decide_retry_strategy(rr);
      disp = abi_disp();
    }

  // Determine the code path to run. Only irrevocable transactions cannot be
  // restarted, so all other transactions need to save live variables.
  ret = choose_code_path(prop, disp);
  if (!(tx->state & STATE_IRREVOCABLE))
    ret |= a_saveLiveVariables;
  return ret;
}


void
GTM::gtm_transaction_cp::save(gtm_thread* tx)
{
  // Save everything that we might have to restore on restarts or aborts.
  jb = tx->jb;
  undolog_size = tx->undolog.size();

  /* FIXME!  Assignment of an aatree like alloc_actions is unsafe; if either
   *this or *tx is destroyed, the other ends up pointing to a freed node.  */
#pragma GCC diagnostic warning "-Wdeprecated-copy"
  alloc_actions = tx->alloc_actions;

  user_actions_size = tx->user_actions.size();
  id = tx->id;
  prop = tx->prop;
  cxa_catch_count = tx->cxa_catch_count;
  cxa_uncaught_count = tx->cxa_uncaught_count;
  disp = abi_disp();
  nesting = tx->nesting;
}

void
GTM::gtm_transaction_cp::commit(gtm_thread* tx)
{
  // Restore state that is not persistent across commits. Exception handling,
  // information, nesting level, and any logs do not need to be restored on
  // commits of nested transactions. Allocation actions must be committed
  // before committing the snapshot.
  tx->jb = jb;
  tx->alloc_actions = alloc_actions;
  tx->id = id;
  tx->prop = prop;
}


void
GTM::gtm_thread::rollback (gtm_transaction_cp *cp, bool aborting)
{
  // The undo log is special in that it used for both thread-local and shared
  // data. Because of the latter, we have to roll it back before any
  // dispatch-specific rollback (which handles synchronization with other
  // transactions).
  undolog.rollback (this, cp ? cp->undolog_size : 0);

  // Perform dispatch-specific rollback.
  abi_disp()->rollback (cp);

  // Roll back all actions that are supposed to happen around the transaction.
  rollback_user_actions (cp ? cp->user_actions_size : 0);
  commit_allocations (true, (cp ? &cp->alloc_actions : 0));
  revert_cpp_exceptions (cp);

  if (cp)
    {
      // We do not yet handle restarts of nested transactions. To do that, we
      // would have to restore some state (jb, id, prop, nesting) not to the
      // checkpoint but to the transaction that was started from this
      // checkpoint (e.g., nesting = cp->nesting + 1);
      assert(aborting);
      // Roll back the rest of the state to the checkpoint.
      jb = cp->jb;
      id = cp->id;
      prop = cp->prop;
      if (cp->disp != abi_disp())
	set_abi_disp(cp->disp);
      alloc_actions = cp->alloc_actions;
      nesting = cp->nesting;
    }
  else
    {
      // Roll back to the outermost transaction.
      // Restore the jump buffer and transaction properties, which we will
      // need for the longjmp used to restart or abort the transaction.
      if (parent_txns.size() > 0)
	{
	  jb = parent_txns[0].jb;
	  id = parent_txns[0].id;
	  prop = parent_txns[0].prop;
	}
      // Reset the transaction. Do not reset this->state, which is handled by
      // the callers. Note that if we are not aborting, we reset the
      // transaction to the point after having executed begin_transaction
      // (we will return from it), so the nesting level must be one, not zero.
      nesting = (aborting ? 0 : 1);
      parent_txns.clear();
    }

  if (this->eh_in_flight)
    {
      _Unwind_DeleteException ((_Unwind_Exception *) this->eh_in_flight);
      this->eh_in_flight = NULL;
    }
}

void ITM_REGPARM
_ITM_abortTransaction (_ITM_abortReason reason)
{
  gtm_thread *tx = gtm_thr();

  assert (reason == userAbort || reason == (userAbort | outerAbort));
  assert ((tx->prop & pr_hasNoAbort) == 0);

  if (tx->state & gtm_thread::STATE_IRREVOCABLE)
    abort ();

  // Roll back to innermost transaction.
  if (tx->parent_txns.size() > 0 && !(reason & outerAbort))
    {
      // If the current method does not support closed nesting but we are
      // nested and must only roll back the innermost transaction, then
      // restart with a method that supports closed nesting.
      abi_dispatch *disp = abi_disp();
      if (!disp->closed_nesting())
	tx->restart(RESTART_CLOSED_NESTING);

      // The innermost transaction is a closed nested transaction.
      gtm_transaction_cp *cp = tx->parent_txns.pop();
      uint32_t longjmp_prop = tx->prop;
      gtm_jmpbuf longjmp_jb = tx->jb;

      tx->rollback (cp, true);

      // Jump to nested transaction (use the saved jump buffer).
      GTM_longjmp (a_abortTransaction | a_restoreLiveVariables,
		   &longjmp_jb, longjmp_prop);
    }
  else
    {
      // There is no nested transaction or an abort of the outermost
      // transaction was requested, so roll back to the outermost transaction.
      tx->rollback (0, true);

      // Aborting an outermost transaction finishes execution of the whole
      // transaction. Therefore, reset transaction state.
      if (tx->state & gtm_thread::STATE_SERIAL)
	gtm_thread::serial_lock.write_unlock ();
      else
	gtm_thread::serial_lock.read_unlock (tx);
      tx->state = 0;

      GTM_longjmp (a_abortTransaction | a_restoreLiveVariables,
		   &tx->jb, tx->prop);
    }
}

bool
GTM::gtm_thread::trycommit ()
{
  nesting--;

  // Skip any real commit for elided transactions.
  if (nesting > 0 && (parent_txns.size() == 0 ||
      nesting > parent_txns[parent_txns.size() - 1].nesting))
    return true;

  if (nesting > 0)
    {
      // Commit of a closed-nested transaction. Remove one checkpoint and add
      // any effects of this transaction to the parent transaction.
      gtm_transaction_cp *cp = parent_txns.pop();
      commit_allocations(false, &cp->alloc_actions);
      cp->commit(this);
      return true;
    }

  // Commit of an outermost transaction.
  gtm_word priv_time = 0;
  if (abi_disp()->trycommit (priv_time))
    {
      // The transaction is now finished but we will still access some shared
      // data if we have to ensure privatization safety.
      bool do_read_unlock = false;
      if (state & gtm_thread::STATE_SERIAL)
        {
          gtm_thread::serial_lock.write_unlock ();
          // There are no other active transactions, so there's no need to
          // enforce privatization safety.
          priv_time = 0;
        }
      else
	{
	  // If we have to ensure privatization safety, we must not yet
	  // release the read lock and become inactive because (1) we still
	  // have to go through the list of all transactions, which can be
	  // modified by serial mode threads, and (2) we interpret each
	  // transactions' shared_state in the context of what we believe to
	  // be the current method group (and serial mode transactions can
	  // change the method group).  Therefore, if we have to ensure
	  // privatization safety, delay becoming inactive but set a maximum
	  // snapshot time (we have committed and thus have an empty snapshot,
	  // so it will always be most recent).  Use release MO so that this
	  // synchronizes with other threads observing our snapshot time.
	  if (priv_time)
	    {
	      do_read_unlock = true;
	      shared_state.store((~(typeof gtm_thread::shared_state)0) - 1,
		  memory_order_release);
	    }
	  else
	    gtm_thread::serial_lock.read_unlock (this);
	}
      state = 0;

      // We can commit the undo log after dispatch-specific commit and after
      // making the transaction inactive because we only have to reset
      // gtm_thread state.
      undolog.commit ();
      // Reset further transaction state.
      cxa_catch_count = 0;
      restart_total = 0;

      // Ensure privatization safety, if necessary.
      if (priv_time)
	{
          // There must be a seq_cst fence between the following loads of the
          // other transactions' shared_state and the dispatch-specific stores
          // that signal updates by this transaction (e.g., lock
          // acquisitions).  This ensures that if we read prior to other
          // reader transactions setting their shared_state to 0, then those
          // readers will observe our updates.  We can reuse the seq_cst fence
          // in serial_lock.read_unlock() if we performed that; if not, we
	  // issue the fence.
	  if (do_read_unlock)
	    atomic_thread_fence (memory_order_seq_cst);
	  // TODO Don't just spin but also block using cond vars / futexes
	  // here. Should probably be integrated with the serial lock code.
	  for (gtm_thread *it = gtm_thread::list_of_threads; it != 0;
	      it = it->next_thread)
	    {
	      if (it == this) continue;
	      // We need to load other threads' shared_state using acquire
	      // semantics (matching the release semantics of the respective
	      // updates).  This is necessary to ensure that the other
	      // threads' memory accesses happen before our actions that
	      // assume privatization safety.
	      // TODO Are there any platform-specific optimizations (e.g.,
	      // merging barriers)?
	      while (it->shared_state.load(memory_order_acquire) < priv_time)
		cpu_relax();
	    }
	}

      // After ensuring privatization safety, we are now truly inactive and
      // thus can release the read lock.  We will also execute potentially
      // privatizing actions (e.g., calling free()).  User actions are first.
      if (do_read_unlock)
	gtm_thread::serial_lock.read_unlock (this);
      commit_user_actions ();
      commit_allocations (false, 0);

      return true;
    }
  return false;
}

void ITM_NORETURN
GTM::gtm_thread::restart (gtm_restart_reason r, bool finish_serial_upgrade)
{
  // Roll back to outermost transaction. Do not reset transaction state because
  // we will continue executing this transaction.
  rollback ();

  // If we have to restart while an upgrade of the serial lock is happening,
  // we need to finish this here, after rollback (to ensure privatization
  // safety despite undo writes) and before deciding about the retry strategy
  // (which could switch to/from serial mode).
  if (finish_serial_upgrade)
    gtm_thread::serial_lock.write_upgrade_finish(this);

  decide_retry_strategy (r);

  // Run dispatch-specific restart code. Retry until we succeed.
  abi_dispatch* disp = abi_disp();
  GTM::gtm_restart_reason rr;
  while ((rr = disp->begin_or_restart()) != NO_RESTART)
    {
      decide_retry_strategy(rr);
      disp = abi_disp();
    }

  GTM_longjmp (choose_code_path(prop, disp) | a_restoreLiveVariables,
	       &jb, prop);
}

void ITM_REGPARM
_ITM_commitTransaction(void)
{
#if defined(USE_HTM_FASTPATH)
  // HTM fastpath.  If we are not executing a HW transaction, then we will be
  // a serial-mode transaction.  If we are, then there will be no other
  // concurrent serial-mode transaction.
  // See gtm_thread::begin_transaction.
  if (likely(!gtm_thread::serial_lock.htm_fastpath_disabled()))
    {
      htm_commit();
      return;
    }
#endif
  gtm_thread *tx = gtm_thr();
  if (!tx->trycommit ())
    tx->restart (RESTART_VALIDATE_COMMIT);
}

void ITM_REGPARM
_ITM_commitTransactionEH(void *exc_ptr)
{
#if defined(USE_HTM_FASTPATH)
  // See _ITM_commitTransaction.
  if (likely(!gtm_thread::serial_lock.htm_fastpath_disabled()))
    {
      htm_commit();
      return;
    }
#endif
  gtm_thread *tx = gtm_thr();
  if (!tx->trycommit ())
    {
      tx->eh_in_flight = exc_ptr;
      tx->restart (RESTART_VALIDATE_COMMIT);
    }
}
