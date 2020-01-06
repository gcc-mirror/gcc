/* Copyright (C) 2011-2020 Free Software Foundation, Inc.
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
#include "futex.h"
#include <limits.h>

namespace GTM HIDDEN {

// Acquire a RW lock for reading.

void
gtm_rwlock::read_lock (gtm_thread *tx)
{
  for (;;)
    {
      // Fast path: first announce our intent to read, then check for
      // conflicting intents to write.  The fence ensures that this happens
      // in exactly this order.
      tx->shared_state.store (0, memory_order_relaxed);
      atomic_thread_fence (memory_order_seq_cst);
      if (likely (writers.load (memory_order_relaxed) == 0))
	return;

      // There seems to be an active, waiting, or confirmed writer, so enter
      // the futex-based slow path.

      // Before waiting, we clear our read intent check whether there are any
      // writers that might potentially wait for readers. If so, wake them.
      // We need the barrier here for the same reason that we need it in
      // read_unlock().
      // TODO Potentially too many wake-ups. See comments in read_unlock().
      tx->shared_state.store (-1, memory_order_relaxed);
      atomic_thread_fence (memory_order_seq_cst);
      if (writer_readers.load (memory_order_relaxed) > 0)
	{
	  writer_readers.store (0, memory_order_relaxed);
	  futex_wake(&writer_readers, 1);
	}

      // Signal that there are waiting readers and wait until there is no
      // writer anymore.
      // TODO Spin here on writers for a while. Consider whether we woke
      // any writers before?
      while (writers.load (memory_order_relaxed))
	{
	  // An active writer. Wait until it has finished. To avoid lost
	  // wake-ups, we need to use Dekker-like synchronization.
	  // Note that we cannot reset readers to zero when we see that there
	  // are no writers anymore after the barrier because this pending
	  // store could then lead to lost wake-ups at other readers.
	  readers.store (1, memory_order_relaxed);
	  atomic_thread_fence (memory_order_seq_cst);
	  if (writers.load (memory_order_relaxed))
	    futex_wait(&readers, 1);
	  else
	    {
	      // There is no writer, actually.  However, we can have enabled
	      // a futex_wait in other readers by previously setting readers
	      // to 1, so we have to wake them up because there is no writer
	      // that will do that.  We don't know whether the wake-up is
	      // really necessary, but we can get lost wake-up situations
	      // otherwise.
	      // No additional barrier nor a nonrelaxed load is required due
	      // to coherency constraints.  write_unlock() checks readers to
	      // see if any wake-up is necessary, but it is not possible that
	      // a reader's store prevents a required later writer wake-up;
	      // If the waking reader's store (value 0) is in modification
	      // order after the waiting readers store (value 1), then the
	      // latter will have to read 0 in the futex due to coherency
	      // constraints and the happens-before enforced by the futex
	      // (paragraph 6.10 in the standard, 6.19.4 in the Batty et al
	      // TR); second, the writer will be forced to read in
	      // modification order too due to Dekker-style synchronization
	      // with the waiting reader (see write_unlock()).
	      // ??? Can we avoid the wake-up if readers is zero (like in
	      // write_unlock())?  Anyway, this might happen too infrequently
	      // to improve performance significantly.
	      readers.store (0, memory_order_relaxed);
	      futex_wake(&readers, INT_MAX);
	    }
	}

      // And we try again to acquire a read lock.
    }
}


// Acquire a RW lock for writing. Generic version that also works for
// upgrades.
// Note that an upgrade might fail (and thus waste previous work done during
// this transaction) if there is another thread that tried to go into serial
// mode earlier (i.e., upgrades do not have higher priority than pure writers).
// However, this seems rare enough to not consider it further as we need both
// a non-upgrade writer and a writer to happen to switch to serial mode
// concurrently. If we'd want to handle this, a writer waiting for readers
// would have to coordinate with later arriving upgrades and hand over the
// lock to them, including the the reader-waiting state. We can try to support
// this if this will actually happen often enough in real workloads.

bool
gtm_rwlock::write_lock_generic (gtm_thread *tx)
{
  // Try to acquire the write lock.  Relaxed MO is fine because of the
  // additional fence below.
  int w = 0;
  if (unlikely (!writers.compare_exchange_strong (w, 1, memory_order_relaxed)))
    {
      // If this is an upgrade, we must not wait for other writers or
      // upgrades.
      if (tx != 0)
	return false;

      // There is already a writer. If there are no other waiting writers,
      // switch to contended mode.  We need seq_cst memory order to make the
      // Dekker-style synchronization work.
      if (w != 2)
	w = writers.exchange (2, memory_order_relaxed);
      while (w != 0)
	{
	  futex_wait(&writers, 2);
	  w = writers.exchange (2, memory_order_relaxed);
	}
    }
  // This fence is both required for the Dekker-like synchronization we do
  // here and is the acquire MO required to make us synchronize-with prior
  // writers.
  atomic_thread_fence (memory_order_seq_cst);

  // We have acquired the writer side of the R/W lock. Now wait for any
  // readers that might still be active.
  // TODO In the worst case, this requires one wait/wake pair for each
  // active reader. Reduce this!
  for (gtm_thread *it = gtm_thread::list_of_threads; it != 0;
      it = it->next_thread)
    {
      if (it == tx)
        continue;
      // Use a loop here to check reader flags again after waiting.
      while (it->shared_state.load (memory_order_relaxed)
          != ~(typeof it->shared_state)0)
	{
	  // If this is an upgrade, we have to break deadlocks with
	  // privatization safety.  This may fail on our side, in which
	  // case we need to cancel our attempt to upgrade.  Also, we do not
	  // block but just spin so that we never have to be woken.
	  if (tx != 0)
	    {
	      if (!abi_disp()->snapshot_most_recent ())
		{
		  write_unlock ();
		  return false;
		}
	      continue;
	    }
	  // An active reader. Wait until it has finished. To avoid lost
	  // wake-ups, we need to use Dekker-like synchronization.
	  // Note that we can reset writer_readers to zero when we see after
	  // the barrier that the reader has finished in the meantime;
	  // however, this is only possible because we are the only writer.
	  // TODO Spin for a while on this reader flag.
	  writer_readers.store (1, memory_order_relaxed);
	  atomic_thread_fence (memory_order_seq_cst);
	  if (it->shared_state.load (memory_order_relaxed)
	      != ~(typeof it->shared_state)0)
	    futex_wait(&writer_readers, 1);
	  else
	    writer_readers.store (0, memory_order_relaxed);
	}
    }

  return true;
}

// Acquire a RW lock for writing.

void
gtm_rwlock::write_lock ()
{
  write_lock_generic (0);
}


// Upgrade a RW lock that has been locked for reading to a writing lock.
// Do this without possibility of another writer incoming.  Return false
// if this attempt fails (i.e. another thread also upgraded).

bool
gtm_rwlock::write_upgrade (gtm_thread *tx)
{
  return write_lock_generic (tx);
}


// Has to be called iff the previous upgrade was successful and after it is
// safe for the transaction to not be marked as a reader anymore.

void
gtm_rwlock::write_upgrade_finish (gtm_thread *tx)
{
  // We are not a reader anymore.  This is only safe to do after we have
  // acquired the writer lock.
  tx->shared_state.store (-1, memory_order_release);
}


// Release a RW lock from reading.

void
gtm_rwlock::read_unlock (gtm_thread *tx)
{
  // We only need release memory order here because of privatization safety
  // (this ensures that marking the transaction as inactive happens after
  // any prior data accesses by this transaction, and that neither the
  // compiler nor the hardware order this store earlier).
  // ??? We might be able to avoid this release here if the compiler can't
  // merge the release fence with the subsequent seq_cst fence.
  tx->shared_state.store (-1, memory_order_release);

  // If there is a writer waiting for readers, wake it up.  We need the fence
  // to avoid lost wake-ups.  Furthermore, the privatization safety
  // implementation in gtm_thread::try_commit() relies on the existence of
  // this seq_cst fence.
  // ??? We might not be the last active reader, so the wake-up might happen
  // too early. How do we avoid this without slowing down readers too much?
  // Each reader could scan the list of txns for other active readers but
  // this can result in many cache misses. Use combining instead?
  // TODO Sends out one wake-up for each reader in the worst case.
  atomic_thread_fence (memory_order_seq_cst);
  if (unlikely (writer_readers.load (memory_order_relaxed) > 0))
    {
      // No additional barrier needed here (see write_unlock()).
      writer_readers.store (0, memory_order_relaxed);
      futex_wake(&writer_readers, 1);
    }
}


// Release a RW lock from writing.

void
gtm_rwlock::write_unlock ()
{
  // Release MO so that we synchronize with subsequent writers.
  if (writers.exchange (0, memory_order_release) == 2)
    {
      // There might be waiting writers, so wake them.  If we woke any thread,
      // we assume it to indeed be a writer; waiting writers will never give
      // up, so we can assume that they will take care of anything else such
      // as waking readers.
      if (futex_wake(&writers, 1) > 0)
	return;
      // If we did not wake any waiting writers, we might indeed be the last
      // writer (this can happen because write_lock_generic() exchanges 0 or 1
      // to 2 and thus might go to contended mode even if no other thread
      // holds the write lock currently). Therefore, we have to fall through
      // to the normal reader wake-up code.
    }
  // This fence is required because we do Dekker-like synchronization here.
  atomic_thread_fence (memory_order_seq_cst);
  // No waiting writers, so wake up all waiting readers.
  if (readers.load (memory_order_relaxed) > 0)
    {
      // No additional barrier needed here.  The previous load must be in
      // modification order because of the coherency constraints.  Late stores
      // by a reader are not a problem because readers do Dekker-style
      // synchronization on writers.
      readers.store (0, memory_order_relaxed);
      futex_wake(&readers, INT_MAX);
    }
}

} // namespace GTM
