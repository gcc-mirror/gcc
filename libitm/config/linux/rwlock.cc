/* Copyright (C) 2011 Free Software Foundation, Inc.
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
      // conflicting intents to write. The barrier makes sure that this
      // happens in exactly this order.
      tx->shared_state = 0;
      __sync_synchronize();
      if (likely(writers == 0))
	return;

      // There seems to be an active, waiting, or confirmed writer, so enter
      // the futex-based slow path.

      // Before waiting, we clear our read intent check whether there are any
      // writers that might potentially wait for readers. If so, wake them.
      // We need the barrier here for the same reason that we need it in
      // read_unlock().
      // TODO Potentially too many wake-ups. See comments in read_unlock().
      tx->shared_state = ~(typeof tx->shared_state)0;
      __sync_synchronize();
      if (writer_readers > 0)
	{
	  writer_readers = 0;
	  futex_wake(&writer_readers, 1);
	}

      // Signal that there are waiting readers and wait until there is no
      // writer anymore.
      // TODO Spin here on writers for a while. Consider whether we woke
      // any writers before?
      while (writers)
	{
	  // An active writer. Wait until it has finished. To avoid lost
	  // wake-ups, we need to use Dekker-like synchronization.
	  // Note that we cannot reset readers to zero when we see that there
	  // are no writers anymore after the barrier because this pending
	  // store could then lead to lost wake-ups at other readers.
	  readers = 1;
	  __sync_synchronize();
	  if (writers)
	    futex_wait(&readers, 1);
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
  // Try to acquire the write lock.
  unsigned int w;
  if (unlikely((w = __sync_val_compare_and_swap(&writers, 0, 1)) != 0))
    {
      // If this is an upgrade, we must not wait for other writers or
      // upgrades.
      if (tx != 0)
	return false;

      // There is already a writer. If there are no other waiting writers,
      // switch to contended mode.
      // Note that this is actually an atomic exchange, not a TAS. Also,
      // it's only guaranteed to have acquire semantics, whereas we need a
      // full barrier to make the Dekker-style synchronization work. However,
      // we rely on the xchg being a full barrier on the architectures that we
      // consider here.
      // ??? Use C++0x atomics as soon as they are available.
      if (w != 2)
	w = __sync_lock_test_and_set(&writers, 2);
      while (w != 0)
	{
	  futex_wait(&writers, 2);
	  w = __sync_lock_test_and_set(&writers, 2);
	}
    }

  // We have acquired the writer side of the R/W lock. Now wait for any
  // readers that might still be active.
  // We don't need an extra barrier here because the CAS and the xchg
  // operations have full barrier semantics already.

  // If this is an upgrade, we are not a reader anymore. This is only safe to
  // do after we have acquired the writer lock.
  // TODO In the worst case, this requires one wait/wake pair for each
  // active reader. Reduce this!
  if (tx != 0)
    tx->shared_state = ~(typeof tx->shared_state)0;

  for (gtm_thread *it = gtm_thread::list_of_threads; it != 0;
      it = it->next_thread)
    {
      // Use a loop here to check reader flags again after waiting.
      while (it->shared_state != ~(typeof it->shared_state)0)
	{
	  // An active reader. Wait until it has finished. To avoid lost
	  // wake-ups, we need to use Dekker-like synchronization.
	  // Note that we can reset writer_readers to zero when we see after
	  // the barrier that the reader has finished in the meantime;
	  // however, this is only possible because we are the only writer.
	  // TODO Spin for a while on this reader flag.
	  writer_readers = 1;
	  __sync_synchronize();
	  if (it->shared_state != ~(typeof it->shared_state)0)
	    futex_wait(&writer_readers, 1);
	  else
	    writer_readers = 0;
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


// Release a RW lock from reading.

void
gtm_rwlock::read_unlock (gtm_thread *tx)
{
  tx->shared_state = ~(typeof tx->shared_state)0;

  // If there is a writer waiting for readers, wake it up. We need the barrier
  // to avoid lost wake-ups.
  // ??? We might not be the last active reader, so the wake-up might happen
  // too early. How do we avoid this without slowing down readers too much?
  // Each reader could scan the list of txns for other active readers but
  // this can result in many cache misses. Use combining instead?
  // TODO Sends out one wake-up for each reader in the worst case.
  __sync_synchronize();
  if (unlikely(writer_readers > 0))
    {
      writer_readers = 0;
      futex_wake(&writer_readers, 1);
    }
}


// Release a RW lock from writing.

void
gtm_rwlock::write_unlock ()
{
  // This is supposed to be a full barrier.
  if (__sync_fetch_and_sub(&writers, 1) == 2)
    {
      // There might be waiting writers, so wake them.
      writers = 0;
      if (futex_wake(&writers, 1) == 0)
	{
	  // If we did not wake any waiting writers, we might indeed be the
	  // last writer (this can happen because write_lock_generic()
	  // exchanges 0 or 1 to 2 and thus might go to contended mode even if
	  // no other thread holds the write lock currently). Therefore, we
	  // have to wake up readers here as well.
	  futex_wake(&readers, INT_MAX);
	}
      return;
    }
  // No waiting writers, so wake up all waiting readers.
  // Because the fetch_and_sub is a full barrier already, we don't need
  // another barrier here (as in read_unlock()).
  if (readers > 0)
    {
      readers = 0;
      futex_wake(&readers, INT_MAX);
    }
}

} // namespace GTM
