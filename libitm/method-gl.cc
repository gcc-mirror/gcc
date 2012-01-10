/* Copyright (C) 2011, 2012 Free Software Foundation, Inc.
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

// This group consists of all TM methods that synchronize via just a single
// global lock (or ownership record).
struct gl_mg : public method_group
{
  static const gtm_word LOCK_BIT = (~(gtm_word)0 >> 1) + 1;
  // We can't use the full bitrange because ~0 in gtm_thread::shared_state has
  // special meaning.
  static const gtm_word VERSION_MAX = (~(gtm_word)0 >> 1) - 1;
  static bool is_locked(gtm_word l) { return l & LOCK_BIT; }
  static gtm_word set_locked(gtm_word l) { return l | LOCK_BIT; }
  static gtm_word clear_locked(gtm_word l) { return l & ~LOCK_BIT; }

  // The global ownership record.
  atomic<gtm_word> orec;

  virtual void init()
  {
    // This store is only executed while holding the serial lock, so relaxed
    // memory order is sufficient here.
    orec.store(0, memory_order_relaxed);
  }
  virtual void fini() { }
};

// TODO cacheline padding
static gl_mg o_gl_mg;


// The global lock, write-through TM method.
// Acquires the orec eagerly before the first write, and then writes through.
// Reads abort if the global orec's version number changed or if it is locked.
// Currently, writes require undo-logging to prevent deadlock between the
// serial lock and the global orec (writer txn acquires orec, reader txn
// upgrades to serial and waits for all other txns, writer tries to upgrade to
// serial too but cannot, writer cannot abort either, deadlock). We could
// avoid this if the serial lock would allow us to prevent other threads from
// going to serial mode, but this probably is too much additional complexity
// just to optimize this TM method.
// gtm_thread::shared_state is used to store a transaction's current
// snapshot time (or commit time). The serial lock uses ~0 for inactive
// transactions and 0 for active ones. Thus, we always have a meaningful
// timestamp in shared_state that can be used to implement quiescence-based
// privatization safety. This even holds if a writing transaction has the
// lock bit set in its shared_state because this is fine for both the serial
// lock (the value will be smaller than ~0) and privatization safety (we
// validate that no other update transaction comitted before we acquired the
// orec, so we have the most recent timestamp and no other transaction can
// commit until we have committed).
// However, we therefore cannot use this method for a serial transaction
// (because shared_state needs to remain at ~0) and we have to be careful
// when switching to serial mode (see the special handling in trycommit() and
// rollback()).
// ??? This sharing adds some complexity wrt. serial mode. Just use a separate
// state variable?
class gl_wt_dispatch : public abi_dispatch
{
protected:
  static void pre_write(const void *addr, size_t len)
  {
    gtm_thread *tx = gtm_thr();
    gtm_word v = tx->shared_state.load(memory_order_relaxed);
    if (unlikely(!gl_mg::is_locked(v)))
      {
	// Check for and handle version number overflow.
	if (unlikely(v >= gl_mg::VERSION_MAX))
	  tx->restart(RESTART_INIT_METHOD_GROUP);

	// This validates that we have a consistent snapshot, which is also
	// for making privatization safety work (see the class' comments).
	// Note that this check here will be performed by the subsequent CAS
	// again, so relaxed memory order is fine.
	gtm_word now = o_gl_mg.orec.load(memory_order_relaxed);
	if (now != v)
	  tx->restart(RESTART_VALIDATE_WRITE);

	// CAS global orec from our snapshot time to the locked state.
	// We need acq_rel memory order here to synchronize with other loads
	// and modifications of orec.
	if (!o_gl_mg.orec.compare_exchange_strong (now, gl_mg::set_locked(now),
						   memory_order_acq_rel))
	  tx->restart(RESTART_LOCKED_WRITE);

	// We use an explicit fence here to avoid having to use release
	// memory order for all subsequent data stores.  This fence will
	// synchronize with loads of the data with acquire memory order.  See
	// validate() for why this is necessary.
	atomic_thread_fence(memory_order_release);

	// Set shared_state to new value.
	tx->shared_state.store(gl_mg::set_locked(now), memory_order_release);
      }

    tx->undolog.log(addr, len);
  }

  static void validate()
  {
    // Check that snapshot is consistent.  We expect the previous data load to
    // have acquire memory order, or be atomic and followed by an acquire
    // fence.
    // As a result, the data load will synchronize with the release fence
    // issued by the transactions whose data updates the data load has read
    // from.  This forces the orec load to read from a visible sequence of side
    // effects that starts with the other updating transaction's store that
    // acquired the orec and set it to locked.
    // We therefore either read a value with the locked bit set (and restart)
    // or read an orec value that was written after the data had been written.
    // Either will allow us to detect inconsistent reads because it will have
    // a higher/different value.
    gtm_thread *tx = gtm_thr();
    gtm_word l = o_gl_mg.orec.load(memory_order_relaxed);
    if (l != tx->shared_state.load(memory_order_relaxed))
      tx->restart(RESTART_VALIDATE_READ);
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

    // We do not have acquired the orec, so we need to load a value and then
    // validate that this was consistent.
    // This needs to have acquire memory order (see validate()).
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
    validate();
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
    if ((dst_mod != WaW && src_mod != RaW)
	&& (dst_mod != NONTXNAL || src_mod == RfW))
      pre_write(dst, size);

    // FIXME We should use atomics here (see store()).  Let's just hope that
    // memcpy/memmove are good enough.
    if (!may_overlap)
      ::memcpy(dst, src, size);
    else
      ::memmove(dst, src, size);

    if (src_mod != RfW && src_mod != RaW && src_mod != NONTXNAL
	&& dst_mod != WaW)
      validate();
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

    // Spin until global orec is not locked.
    // TODO This is not necessary if there are no pure loads (check txn props).
    unsigned i = 0;
    gtm_word v;
    while (1)
      {
        // We need acquire memory order here so that this load will
        // synchronize with the store that releases the orec in trycommit().
        // In turn, this makes sure that subsequent data loads will read from
        // a visible sequence of side effects that starts with the most recent
        // store to the data right before the release of the orec.
        v = o_gl_mg.orec.load(memory_order_acquire);
        if (!gl_mg::is_locked(v))
	  break;
	// TODO need method-specific max spin count
	if (++i > gtm_spin_count_var)
	  return RESTART_VALIDATE_READ;
	cpu_relax();
      }

    // Everything is okay, we have a snapshot time.
    // We don't need to enforce any ordering for the following store. There
    // are no earlier data loads in this transaction, so the store cannot
    // become visible before those (which could lead to the violation of
    // privatization safety). The store can become visible after later loads
    // but this does not matter because the previous value will have been
    // smaller or equal (the serial lock will set shared_state to zero when
    // marking the transaction as active, and restarts enforce immediate
    // visibility of a smaller or equal value with a barrier (see
    // rollback()).
    tx->shared_state.store(v, memory_order_relaxed);
    return NO_RESTART;
  }

  virtual bool trycommit(gtm_word& priv_time)
  {
    gtm_thread* tx = gtm_thr();
    gtm_word v = tx->shared_state.load(memory_order_relaxed);

    // Special case: If shared_state is ~0, then we have acquired the
    // serial lock (tx->state is not updated yet). In this case, the previous
    // value isn't available anymore, so grab it from the global lock, which
    // must have a meaningful value because no other transactions are active
    // anymore. In particular, if it is locked, then we are an update
    // transaction, which is all we care about for commit.
    if (v == ~(typeof v)0)
      v = o_gl_mg.orec.load(memory_order_relaxed);

    // Release the orec but do not reset shared_state, which will be modified
    // by the serial lock right after our commit anyway. Also, resetting
    // shared state here would interfere with the serial lock's use of this
    // location.
    if (gl_mg::is_locked(v))
      {
	// Release the global orec, increasing its version number / timestamp.
        // See begin_or_restart() for why we need release memory order here.
	v = gl_mg::clear_locked(v) + 1;
	o_gl_mg.orec.store(v, memory_order_release);

	// Need to ensure privatization safety. Every other transaction must
	// have a snapshot time that is at least as high as our commit time
	// (i.e., our commit must be visible to them).
	priv_time = v;
      }
    return true;
  }

  virtual void rollback(gtm_transaction_cp *cp)
  {
    // We don't do anything for rollbacks of nested transactions.
    if (cp != 0)
      return;

    gtm_thread *tx = gtm_thr();
    gtm_word v = tx->shared_state.load(memory_order_relaxed);
    // Special case: If shared_state is ~0, then we have acquired the
    // serial lock (tx->state is not updated yet). In this case, the previous
    // value isn't available anymore, so grab it from the global lock, which
    // must have a meaningful value because no other transactions are active
    // anymore. In particular, if it is locked, then we are an update
    // transaction, which is all we care about for rollback.
    bool is_serial = v == ~(typeof v)0;
    if (is_serial)
      v = o_gl_mg.orec.load(memory_order_relaxed);

    // Release lock and increment version number to prevent dirty reads.
    // Also reset shared state here, so that begin_or_restart() can expect a
    // value that is correct wrt. privatization safety.
    if (gl_mg::is_locked(v))
      {
	// Release the global orec, increasing its version number / timestamp.
        // See begin_or_restart() for why we need release memory order here.
	v = gl_mg::clear_locked(v) + 1;
	o_gl_mg.orec.store(v, memory_order_release);

	// Also reset the timestamp published via shared_state.
	// Special case: Only do this if we are not a serial transaction
	// because otherwise, we would interfere with the serial lock.
	if (!is_serial)
	  tx->shared_state.store(v, memory_order_release);

	// We need a store-load barrier after this store to prevent it
	// from becoming visible after later data loads because the
	// previous value of shared_state has been higher than the actual
	// snapshot time (the lock bit had been set), which could break
	// privatization safety. We do not need a barrier before this
	// store (see pre_write() for an explanation).
	// ??? What is the precise reasoning in the C++11 model?
	atomic_thread_fence(memory_order_seq_cst);
      }

  }

  CREATE_DISPATCH_METHODS(virtual, )
  CREATE_DISPATCH_METHODS_MEM()

  gl_wt_dispatch() : abi_dispatch(false, true, false, false, &o_gl_mg)
  { }
};

} // anon namespace

static const gl_wt_dispatch o_gl_wt_dispatch;

abi_dispatch *
GTM::dispatch_gl_wt ()
{
  return const_cast<gl_wt_dispatch *>(&o_gl_wt_dispatch);
}
