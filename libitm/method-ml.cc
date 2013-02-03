/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   Contributed by Torvald Riegel <triegel@redhat.com>.

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

using namespace GTM;

namespace {

// This group consists of all TM methods that synchronize via multiple locks
// (or ownership records).
struct ml_mg : public method_group
{
  static const gtm_word LOCK_BIT = (~(gtm_word)0 >> 1) + 1;
  static const gtm_word INCARNATION_BITS = 3;
  static const gtm_word INCARNATION_MASK = 7;
  // Maximum time is all bits except the lock bit, the overflow reserve bit,
  // and the incarnation bits).
  static const gtm_word TIME_MAX = (~(gtm_word)0 >> (2 + INCARNATION_BITS));
  // The overflow reserve bit is the MSB of the timestamp part of an orec,
  // so we can have TIME_MAX+1 pending timestamp increases before we overflow.
  static const gtm_word OVERFLOW_RESERVE = TIME_MAX + 1;

  static bool is_locked(gtm_word o) { return o & LOCK_BIT; }
  static gtm_word set_locked(gtm_thread *tx)
  {
    return ((uintptr_t)tx >> 1) | LOCK_BIT;
  }
  // Returns a time that includes the lock bit, which is required by both
  // validate() and is_more_recent_or_locked().
  static gtm_word get_time(gtm_word o) { return o >> INCARNATION_BITS; }
  static gtm_word set_time(gtm_word time) { return time << INCARNATION_BITS; }
  static bool is_more_recent_or_locked(gtm_word o, gtm_word than_time)
  {
    // LOCK_BIT is the MSB; thus, if O is locked, it is larger than TIME_MAX.
    return get_time(o) > than_time;
  }
  static bool has_incarnation_left(gtm_word o)
  {
    return (o & INCARNATION_MASK) < INCARNATION_MASK;
  }
  static gtm_word inc_incarnation(gtm_word o) { return o + 1; }

  // The shared time base.
  atomic<gtm_word> time __attribute__((aligned(HW_CACHELINE_SIZE)));

  // The array of ownership records.
  atomic<gtm_word>* orecs __attribute__((aligned(HW_CACHELINE_SIZE)));
  char tailpadding[HW_CACHELINE_SIZE - sizeof(atomic<gtm_word>*)];

  // Location-to-orec mapping.  Stripes of 16B mapped to 2^19 orecs.
  static const gtm_word L2O_ORECS = 1 << 19;
  static const gtm_word L2O_SHIFT = 4;
  static size_t get_orec(const void* addr)
  {
    return ((uintptr_t)addr >> L2O_SHIFT) & (L2O_ORECS - 1);
  }
  static size_t get_next_orec(size_t orec)
  {
    return (orec + 1) & (L2O_ORECS - 1);
  }
  // Returns the next orec after the region.
  static size_t get_orec_end(const void* addr, size_t len)
  {
    return (((uintptr_t)addr + len + (1 << L2O_SHIFT) - 1) >> L2O_SHIFT)
        & (L2O_ORECS - 1);
  }

  virtual void init()
  {
    // We assume that an atomic<gtm_word> is backed by just a gtm_word, so
    // starting with zeroed memory is fine.
    orecs = (atomic<gtm_word>*) xcalloc(
        sizeof(atomic<gtm_word>) * L2O_ORECS, true);
    // This store is only executed while holding the serial lock, so relaxed
    // memory order is sufficient here.
    time.store(0, memory_order_relaxed);
  }

  virtual void fini()
  {
    free(orecs);
  }

  // We only re-initialize when our time base overflows.  Thus, only reset
  // the time base and the orecs but do not re-allocate the orec array.
  virtual void reinit()
  {
    // This store is only executed while holding the serial lock, so relaxed
    // memory order is sufficient here.  Same holds for the memset.
    time.store(0, memory_order_relaxed);
    memset(orecs, 0, sizeof(atomic<gtm_word>) * L2O_ORECS);
  }
};

static ml_mg o_ml_mg;


// The multiple lock, write-through TM method.
// Maps each memory location to one of the orecs in the orec array, and then
// acquires the associated orec eagerly before writing through.
// Writes require undo-logging because we are dealing with several locks/orecs
// and need to resolve deadlocks if necessary by aborting one of the
// transactions.
// Reads do time-based validation with snapshot time extensions.  Incarnation
// numbers are used to decrease contention on the time base (with those,
// aborted transactions do not need to acquire a new version number for the
// data that has been previously written in the transaction and needs to be
// rolled back).
// gtm_thread::shared_state is used to store a transaction's current
// snapshot time (or commit time). The serial lock uses ~0 for inactive
// transactions and 0 for active ones. Thus, we always have a meaningful
// timestamp in shared_state that can be used to implement quiescence-based
// privatization safety.
class ml_wt_dispatch : public abi_dispatch
{
protected:
  static void pre_write(gtm_thread *tx, const void *addr, size_t len)
  {
    gtm_word snapshot = tx->shared_state.load(memory_order_relaxed);
    gtm_word locked_by_tx = ml_mg::set_locked(tx);

    // Lock all orecs that cover the region.
    size_t orec = ml_mg::get_orec(addr);
    size_t orec_end = ml_mg::get_orec_end(addr, len);
    do
      {
        // Load the orec.  Relaxed memory order is sufficient here because
        // either we have acquired the orec or we will try to acquire it with
        // a CAS with stronger memory order.
        gtm_word o = o_ml_mg.orecs[orec].load(memory_order_relaxed);

        // Check whether we have acquired the orec already.
        if (likely (locked_by_tx != o))
          {
            // If not, acquire.  Make sure that our snapshot time is larger or
            // equal than the orec's version to avoid masking invalidations of
            // our snapshot with our own writes.
            if (unlikely (ml_mg::is_locked(o)))
              tx->restart(RESTART_LOCKED_WRITE);

            if (unlikely (ml_mg::get_time(o) > snapshot))
              {
                // We only need to extend the snapshot if we have indeed read
                // from this orec before.  Given that we are an update
                // transaction, we will have to extend anyway during commit.
                // ??? Scan the read log instead, aborting if we have read
                // from data covered by this orec before?
                snapshot = extend(tx);
              }

            // We need acquire memory order here to synchronize with other
            // (ownership) releases of the orec.  We do not need acq_rel order
            // because whenever another thread reads from this CAS'
            // modification, then it will abort anyway and does not rely on
            // any further happens-before relation to be established.
            if (unlikely (!o_ml_mg.orecs[orec].compare_exchange_strong(
                o, locked_by_tx, memory_order_acquire)))
              tx->restart(RESTART_LOCKED_WRITE);

            // We use an explicit fence here to avoid having to use release
            // memory order for all subsequent data stores.  This fence will
            // synchronize with loads of the data with acquire memory order.
            // See post_load() for why this is necessary.
            // Adding require memory order to the prior CAS is not sufficient,
            // at least according to the Batty et al. formalization of the
            // memory model.
            atomic_thread_fence(memory_order_release);

            // We log the previous value here to be able to use incarnation
            // numbers when we have to roll back.
            // ??? Reserve capacity early to avoid capacity checks here?
            gtm_rwlog_entry *e = tx->writelog.push();
            e->orec = o_ml_mg.orecs + orec;
            e->value = o;
          }
        orec = o_ml_mg.get_next_orec(orec);
      }
    while (orec != orec_end);

    // Do undo logging.  We do not know which region prior writes logged
    // (even if orecs have been acquired), so just log everything.
    tx->undolog.log(addr, len);
  }

  static void pre_write(const void *addr, size_t len)
  {
    gtm_thread *tx = gtm_thr();
    pre_write(tx, addr, len);
  }

  // Returns true iff all the orecs in our read log still have the same time
  // or have been locked by the transaction itself.
  static bool validate(gtm_thread *tx)
  {
    gtm_word locked_by_tx = ml_mg::set_locked(tx);
    // ??? This might get called from pre_load() via extend().  In that case,
    // we don't really need to check the new entries that pre_load() is
    // adding.  Stop earlier?
    for (gtm_rwlog_entry *i = tx->readlog.begin(), *ie = tx->readlog.end();
        i != ie; i++)
      {
	// Relaxed memory order is sufficient here because we do not need to
	// establish any new synchronizes-with relationships.  We only need
	// to read a value that is as least as current as enforced by the
	// callers: extend() loads global time with acquire, and trycommit()
	// increments global time with acquire.  Therefore, we will see the
	// most recent orec updates before the global time that we load.
        gtm_word o = i->orec->load(memory_order_relaxed);
        // We compare only the time stamp and the lock bit here.  We know that
        // we have read only committed data before, so we can ignore
        // intermediate yet rolled-back updates presented by the incarnation
        // number bits.
        if (ml_mg::get_time(o) != ml_mg::get_time(i->value)
            && o != locked_by_tx)
          return false;
      }
    return true;
  }

  // Tries to extend the snapshot to a more recent time.  Returns the new
  // snapshot time and updates TX->SHARED_STATE.  If the snapshot cannot be
  // extended to the current global time, TX is restarted.
  static gtm_word extend(gtm_thread *tx)
  {
    // We read global time here, even if this isn't strictly necessary
    // because we could just return the maximum of the timestamps that
    // validate sees.  However, the potential cache miss on global time is
    // probably a reasonable price to pay for avoiding unnecessary extensions
    // in the future.
    // We need acquire memory oder because we have to synchronize with the
    // increment of global time by update transactions, whose lock
    // acquisitions we have to observe (also see trycommit()).
    gtm_word snapshot = o_ml_mg.time.load(memory_order_acquire);
    if (!validate(tx))
      tx->restart(RESTART_VALIDATE_READ);

    // Update our public snapshot time.  Probably useful to decrease waiting
    // due to quiescence-based privatization safety.
    // Use release memory order to establish synchronizes-with with the
    // privatizers; prior data loads should happen before the privatizers
    // potentially modify anything.
    tx->shared_state.store(snapshot, memory_order_release);
    return snapshot;
  }

  // First pass over orecs.  Load and check all orecs that cover the region.
  // Write to read log, extend snapshot time if necessary.
  static gtm_rwlog_entry* pre_load(gtm_thread *tx, const void* addr,
      size_t len)
  {
    // Don't obtain an iterator yet because the log might get resized.
    size_t log_start = tx->readlog.size();
    gtm_word snapshot = tx->shared_state.load(memory_order_relaxed);
    gtm_word locked_by_tx = ml_mg::set_locked(tx);

    size_t orec = ml_mg::get_orec(addr);
    size_t orec_end = ml_mg::get_orec_end(addr, len);
    do
      {
        // We need acquire memory order here so that this load will
        // synchronize with the store that releases the orec in trycommit().
        // In turn, this makes sure that subsequent data loads will read from
        // a visible sequence of side effects that starts with the most recent
        // store to the data right before the release of the orec.
        gtm_word o = o_ml_mg.orecs[orec].load(memory_order_acquire);

        if (likely (!ml_mg::is_more_recent_or_locked(o, snapshot)))
          {
            success:
            gtm_rwlog_entry *e = tx->readlog.push();
            e->orec = o_ml_mg.orecs + orec;
            e->value = o;
          }
        else if (!ml_mg::is_locked(o))
          {
            // We cannot read this part of the region because it has been
            // updated more recently than our snapshot time.  If we can extend
            // our snapshot, then we can read.
            snapshot = extend(tx);
            goto success;
          }
        else
          {
            // If the orec is locked by us, just skip it because we can just
            // read from it.  Otherwise, restart the transaction.
            if (o != locked_by_tx)
              tx->restart(RESTART_LOCKED_READ);
          }
        orec = o_ml_mg.get_next_orec(orec);
      }
    while (orec != orec_end);
    return &tx->readlog[log_start];
  }

  // Second pass over orecs, verifying that the we had a consistent read.
  // Restart the transaction if any of the orecs is locked by another
  // transaction.
  static void post_load(gtm_thread *tx, gtm_rwlog_entry* log)
  {
    for (gtm_rwlog_entry *end = tx->readlog.end(); log != end; log++)
      {
        // Check that the snapshot is consistent.  We expect the previous data
        // load to have acquire memory order, or be atomic and followed by an
        // acquire fence.
        // As a result, the data load will synchronize with the release fence
        // issued by the transactions whose data updates the data load has read
        // from.  This forces the orec load to read from a visible sequence of
        // side effects that starts with the other updating transaction's
        // store that acquired the orec and set it to locked.
        // We therefore either read a value with the locked bit set (and
        // restart) or read an orec value that was written after the data had
        // been written.  Either will allow us to detect inconsistent reads
        // because it will have a higher/different value.
	// Also note that differently to validate(), we compare the raw value
	// of the orec here, including incarnation numbers.  We must prevent
	// returning uncommitted data from loads (whereas when validating, we
	// already performed a consistent load).
        gtm_word o = log->orec->load(memory_order_relaxed);
        if (log->value != o)
          tx->restart(RESTART_VALIDATE_READ);
      }
  }

  template <typename V> static V load(const V* addr, ls_modifier mod)
  {
    // Read-for-write should be unlikely, but we need to handle it or will
    // break later WaW optimizations.
    if (unlikely(mod == RfW))
      {
	pre_write(addr, sizeof(V));
	return *addr;
      }
    if (unlikely(mod == RaW))
      return *addr;
    // ??? Optimize for RaR?

    gtm_thread *tx = gtm_thr();
    gtm_rwlog_entry* log = pre_load(tx, addr, sizeof(V));

    // Load the data.
    // This needs to have acquire memory order (see post_load()).
    // Alternatively, we can put an acquire fence after the data load but this
    // is probably less efficient.
    // FIXME We would need an atomic load with acquire memory order here but
    // we can't just forge an atomic load for nonatomic data because this
    // might not work on all implementations of atomics.  However, we need
    // the acquire memory order and we can only establish this if we link
    // it to the matching release using a reads-from relation between atomic
    // loads.  Also, the compiler is allowed to optimize nonatomic accesses
    // differently than atomic accesses (e.g., if the load would be moved to
    // after the fence, we potentially don't synchronize properly anymore).
    // Instead of the following, just use an ordinary load followed by an
    // acquire fence, and hope that this is good enough for now:
    // V v = atomic_load_explicit((atomic<V>*)addr, memory_order_acquire);
    V v = *addr;
    atomic_thread_fence(memory_order_acquire);

    // ??? Retry the whole load if it wasn't consistent?
    post_load(tx, log);

    return v;
  }

  template <typename V> static void store(V* addr, const V value,
      ls_modifier mod)
  {
    if (likely(mod != WaW))
      pre_write(addr, sizeof(V));
    // FIXME We would need an atomic store here but we can't just forge an
    // atomic load for nonatomic data because this might not work on all
    // implementations of atomics.  However, we need this store to link the
    // release fence in pre_write() to the acquire operation in load, which
    // is only guaranteed if we have a reads-from relation between atomic
    // accesses.  Also, the compiler is allowed to optimize nonatomic accesses
    // differently than atomic accesses (e.g., if the store would be moved
    // to before the release fence in pre_write(), things could go wrong).
    // atomic_store_explicit((atomic<V>*)addr, value, memory_order_relaxed);
    *addr = value;
  }

public:
  static void memtransfer_static(void *dst, const void* src, size_t size,
      bool may_overlap, ls_modifier dst_mod, ls_modifier src_mod)
  {
    gtm_rwlog_entry* log = 0;
    gtm_thread *tx = 0;

    if (src_mod == RfW)
      {
        tx = gtm_thr();
        pre_write(tx, src, size);
      }
    else if (src_mod != RaW && src_mod != NONTXNAL)
      {
        tx = gtm_thr();
        log = pre_load(tx, src, size);
      }
    // ??? Optimize for RaR?

    if (dst_mod != NONTXNAL && dst_mod != WaW)
      {
        if (src_mod != RfW && (src_mod == RaW || src_mod == NONTXNAL))
          tx = gtm_thr();
        pre_write(tx, dst, size);
      }

    // FIXME We should use atomics here (see store()).  Let's just hope that
    // memcpy/memmove are good enough.
    if (!may_overlap)
      ::memcpy(dst, src, size);
    else
      ::memmove(dst, src, size);

    // ??? Retry the whole memtransfer if it wasn't consistent?
    if (src_mod != RfW && src_mod != RaW && src_mod != NONTXNAL)
      {
	// See load() for why we need the acquire fence here.
	atomic_thread_fence(memory_order_acquire);
	post_load(tx, log);
      }
  }

  static void memset_static(void *dst, int c, size_t size, ls_modifier mod)
  {
    if (mod != WaW)
      pre_write(dst, size);
    // FIXME We should use atomics here (see store()).  Let's just hope that
    // memset is good enough.
    ::memset(dst, c, size);
  }

  virtual gtm_restart_reason begin_or_restart()
  {
    // We don't need to do anything for nested transactions.
    gtm_thread *tx = gtm_thr();
    if (tx->parent_txns.size() > 0)
      return NO_RESTART;

    // Read the current time, which becomes our snapshot time.
    // Use acquire memory oder so that we see the lock acquisitions by update
    // transcations that incremented the global time (see trycommit()).
    gtm_word snapshot = o_ml_mg.time.load(memory_order_acquire);
    // Re-initialize method group on time overflow.
    if (snapshot >= o_ml_mg.TIME_MAX)
      return RESTART_INIT_METHOD_GROUP;

    // We don't need to enforce any ordering for the following store. There
    // are no earlier data loads in this transaction, so the store cannot
    // become visible before those (which could lead to the violation of
    // privatization safety). The store can become visible after later loads
    // but this does not matter because the previous value will have been
    // smaller or equal (the serial lock will set shared_state to zero when
    // marking the transaction as active, and restarts enforce immediate
    // visibility of a smaller or equal value with a barrier (see
    // rollback()).
    tx->shared_state.store(snapshot, memory_order_relaxed);
    return NO_RESTART;
  }

  virtual bool trycommit(gtm_word& priv_time)
  {
    gtm_thread* tx = gtm_thr();

    // If we haven't updated anything, we can commit.
    if (!tx->writelog.size())
      {
        tx->readlog.clear();
        return true;
      }

    // Get a commit time.
    // Overflow of o_ml_mg.time is prevented in begin_or_restart().
    // We need acq_rel here because (1) the acquire part is required for our
    // own subsequent call to validate(), and the release part is necessary to
    // make other threads' validate() work as explained there and in extend().
    gtm_word ct = o_ml_mg.time.fetch_add(1, memory_order_acq_rel) + 1;

    // Extend our snapshot time to at least our commit time.
    // Note that we do not need to validate if our snapshot time is right
    // before the commit time because we are never sharing the same commit
    // time with other transactions.
    // No need to reset shared_state, which will be modified by the serial
    // lock right after our commit anyway.
    gtm_word snapshot = tx->shared_state.load(memory_order_relaxed);
    if (snapshot < ct - 1 && !validate(tx))
      return false;

    // Release orecs.
    // See pre_load() / post_load() for why we need release memory order.
    // ??? Can we use a release fence and relaxed stores?
    gtm_word v = ml_mg::set_time(ct);
    for (gtm_rwlog_entry *i = tx->writelog.begin(), *ie = tx->writelog.end();
        i != ie; i++)
      i->orec->store(v, memory_order_release);

    // We're done, clear the logs.
    tx->writelog.clear();
    tx->readlog.clear();

    // Need to ensure privatization safety. Every other transaction must
    // have a snapshot time that is at least as high as our commit time
    // (i.e., our commit must be visible to them).
    priv_time = ct;
    return true;
  }

  virtual void rollback(gtm_transaction_cp *cp)
  {
    // We don't do anything for rollbacks of nested transactions.
    // ??? We could release locks here if we snapshot writelog size.  readlog
    // is similar.  This is just a performance optimization though.  Nested
    // aborts should be rather infrequent, so the additional save/restore
    // overhead for the checkpoints could be higher.
    if (cp != 0)
      return;

    gtm_thread *tx = gtm_thr();
    gtm_word overflow_value = 0;

    // Release orecs.
    for (gtm_rwlog_entry *i = tx->writelog.begin(), *ie = tx->writelog.end();
        i != ie; i++)
      {
        // If possible, just increase the incarnation number.
        // See pre_load() / post_load() for why we need release memory order.
	// ??? Can we use a release fence and relaxed stores?  (Same below.)
        if (ml_mg::has_incarnation_left(i->value))
          i->orec->store(ml_mg::inc_incarnation(i->value),
              memory_order_release);
        else
          {
            // We have an incarnation overflow.  Acquire a new timestamp, and
            // use it from now on as value for each orec whose incarnation
            // number cannot be increased.
            // Overflow of o_ml_mg.time is prevented in begin_or_restart().
            // See pre_load() / post_load() for why we need release memory
            // order.
            if (!overflow_value)
              // Release memory order is sufficient but required here.
              // In contrast to the increment in trycommit(), we need release
              // for the same reason but do not need the acquire because we
              // do not validate subsequently.
              overflow_value = ml_mg::set_time(
                  o_ml_mg.time.fetch_add(1, memory_order_release) + 1);
            i->orec->store(overflow_value, memory_order_release);
          }
      }

    // We need this release fence to ensure that privatizers see the
    // rolled-back original state (not any uncommitted values) when they read
    // the new snapshot time that we write in begin_or_restart().
    atomic_thread_fence(memory_order_release);

    // We're done, clear the logs.
    tx->writelog.clear();
    tx->readlog.clear();
  }

  virtual bool supports(unsigned number_of_threads)
  {
    // Each txn can commit and fail and rollback once before checking for
    // overflow, so this bounds the number of threads that we can support.
    // In practice, this won't be a problem but we check it anyway so that
    // we never break in the occasional weird situation.
    return (number_of_threads * 2 <= ml_mg::OVERFLOW_RESERVE);
  }

  CREATE_DISPATCH_METHODS(virtual, )
  CREATE_DISPATCH_METHODS_MEM()

  ml_wt_dispatch() : abi_dispatch(false, true, false, false, 0, &o_ml_mg)
  { }
};

} // anon namespace

static const ml_wt_dispatch o_ml_wt_dispatch;

abi_dispatch *
GTM::dispatch_ml_wt ()
{
  return const_cast<ml_wt_dispatch *>(&o_ml_wt_dispatch);
}
