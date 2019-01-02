/* Copyright (C) 2008-2019 Free Software Foundation, Inc.
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

namespace GTM HIDDEN {

// Initialize a new RW lock.
// ??? Move this back to the header file when constexpr is implemented.

gtm_rwlock::gtm_rwlock()
  : summary (0),
    htm_fastpath (0),
    mutex (PTHREAD_MUTEX_INITIALIZER),
    c_readers (PTHREAD_COND_INITIALIZER),
    c_writers (PTHREAD_COND_INITIALIZER),
    c_confirmed_writers (PTHREAD_COND_INITIALIZER),
    a_readers (0),
    w_readers (0),
    w_writers (0)
{ }

gtm_rwlock::~gtm_rwlock()
{
  pthread_mutex_destroy (&this->mutex);
  pthread_cond_destroy (&this->c_readers);
  pthread_cond_destroy (&this->c_writers);
}

// Acquire a RW lock for reading.

void
gtm_rwlock::read_lock (gtm_thread *tx)
{
  // Fast path: first announce our intent to read, then check for conflicting
  // intents to write.  The fence ensure that this happens in exactly this
  // order.
  tx->shared_state.store (0, memory_order_relaxed);
  atomic_thread_fence (memory_order_seq_cst);
  unsigned int sum = this->summary.load (memory_order_relaxed);
  if (likely(!(sum & (a_writer | w_writer))))
    return;

  // There seems to be an active, waiting, or confirmed writer, so enter the
  // mutex-based slow path. To try to keep the number of readers small that
  // the writer will see, we clear our read flag right away before entering
  // the critical section. Otherwise, the writer would have to wait for us to
  // get into the critical section. (Note that for correctness, this only has
  // to happen before we leave the slow path and before we wait for any
  // writer).
  // ??? Add a barrier to enforce early visibility of this?
  tx->shared_state.store(-1, memory_order_relaxed);

  pthread_mutex_lock (&this->mutex);

  // Read summary again after acquiring the mutex because it might have
  // changed during waiting for the mutex to become free.
  sum = this->summary.load (memory_order_relaxed);

  // If there is a writer waiting for readers, wake it up. Only do that if we
  // might be the last reader that could do the wake-up, otherwise skip the
  // wake-up but decrease a_readers to show that we have entered the slow path.
  // This has to happen before we wait for any writers or upgraders.
  // See write_lock_generic() for further explanations.
  if (this->a_readers > 0)
    {
      this->a_readers--;
      if (this->a_readers == 0)
	pthread_cond_signal(&this->c_confirmed_writers);
    }

  // If there is an active or waiting writer, we must wait.
  while (sum & (a_writer | w_writer))
    {
      this->summary.store (sum | w_reader, memory_order_relaxed);
      this->w_readers++;
      pthread_cond_wait (&this->c_readers, &this->mutex);
      sum = this->summary.load (memory_order_relaxed);
      if (--this->w_readers == 0)
	sum &= ~w_reader;
    }

  // Otherwise we can acquire the lock for read.
  tx->shared_state.store(0, memory_order_relaxed);

  pthread_mutex_unlock(&this->mutex);
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
  pthread_mutex_lock (&this->mutex);

  unsigned int sum = this->summary.load (memory_order_relaxed);

  // If there is an active writer, wait.
  while (sum & a_writer)
    {
      if (tx != 0)
	{
	  // If this is an upgrade, we must not wait for other writers or
	  // upgrades that already have gone in
	  pthread_mutex_unlock (&this->mutex);
	  return false;
	}

      this->summary.store (sum | w_writer, memory_order_relaxed);
      this->w_writers++;
      pthread_cond_wait (&this->c_writers, &this->mutex);
      sum = this->summary.load (memory_order_relaxed);
      if (--this->w_writers == 0)
	sum &= ~w_writer;
    }

  // Otherwise we can acquire the lock for write. As a writer, we have
  // priority, so we don't need to take this back.
  this->summary.store (sum | a_writer, memory_order_relaxed);

  // We still need to wait for active readers to finish. The barrier makes
  // sure that we first set our write intent and check for active readers
  // after that, in strictly this order (similar to the barrier in the fast
  // path of read_lock()).
  atomic_thread_fence(memory_order_seq_cst);

  // Count the number of active readers to be able to decrease the number of
  // wake-ups and wait calls that are necessary.
  //
  // This number is an upper bound of the number of readers that actually
  // are still active and which we need to wait for:
  // - We set our write flag before checking the reader flags, and readers
  //   check our write flag after clearing their read flags in read_unlock().
  //   Therefore, they will enter the slow path whenever we have seen them.
  // - Readers will have cleared their read flags before leaving the slow
  //   path in read_lock() (prevents lost wake-ups), and before waiting for
  //   any writer (prevents deadlocks).
  //
  // However, this number is also just a lower bound of the number of readers
  // that will actually enter the slow path in read_unlock() or read_lock():
  // - Because the read flag is cleared outside of a critical section, writers
  //   can see it as cleared while the reader still goes into the slow path.
  //
  // Therefore, readers can skip (lower bound - 1) wake-ups, but we do need
  // the following loop to check that the readers that we wanted to wait for
  // are actually those that entered the slow path so far (and either skipped
  // or sent a wake-up).
  //
  // ??? Do we need to optimize further? (The writer could publish a list of
  // readers that it suspects to be active. Readers could check this list and
  // only decrement a_readers if they are in this list.)
  for (;;)
    {
      // ??? Keep a list of active readers that we saw and update it on the
      // next retry instead? This might reduce the number of cache misses that
      // we get when checking reader flags.
      int readers = 0;
      for (gtm_thread *it = gtm_thread::list_of_threads; it != 0;
	  it = it->next_thread)
	{
	  // Don't count ourself if this is an upgrade.
          if (it == tx)
            continue;
	  if (it->shared_state.load(memory_order_relaxed) != (gtm_word)-1)
	    readers++;
	}

      // If we have not seen any readers, we will not wait.
      if (readers == 0)
	break;

      // If this is an upgrade, we have to break deadlocks with
      // privatization safety.  This may fail on our side, in which
      // case we need to cancel our attempt to upgrade.  Also, we do not
      // block using the convdar but just spin so that we never have to be
      // woken.
      // FIXME This is horribly inefficient -- but so is not being able
      // to use futexes in this case.
      if (tx != 0)
	{
	  pthread_mutex_unlock (&this->mutex);
	  if (!abi_disp ()->snapshot_most_recent ())
	    {
	      write_unlock ();
	      return false;
	    }
	  pthread_mutex_lock (&this->mutex);
	  continue;
	}


      // We've seen a number of readers, so we publish this number and wait.
      this->a_readers = readers;
      pthread_cond_wait (&this->c_confirmed_writers, &this->mutex);
    }

  pthread_mutex_unlock (&this->mutex);
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
  // We need this seq_cst fence here to avoid lost wake-ups.  Furthermore,
  // the privatization safety implementation in gtm_thread::try_commit()
  // relies on the existence of this seq_cst fence.
  atomic_thread_fence (memory_order_seq_cst);
  unsigned int sum = this->summary.load (memory_order_relaxed);
  if (likely(!(sum & (a_writer | w_writer))))
    return;

  // There is a writer, either active or waiting for other readers or writers.
  // Thus, enter the mutex-based slow path.
  pthread_mutex_lock (&this->mutex);

  // If there is a writer waiting for readers, wake it up. Only do that if we
  // might be the last reader that could do the wake-up, otherwise skip the
  // wake-up and decrease a_readers to publish that we have entered the slow
  // path but skipped the wake-up.
  if (this->a_readers > 0)
    {
      this->a_readers--;
      if (this->a_readers == 0)
	pthread_cond_signal(&this->c_confirmed_writers);
    }

  // We don't need to wake up any writers waiting for other writers. Active
  // writers will take care of that.

  pthread_mutex_unlock (&this->mutex);
}


// Release a RW lock from writing.

void
gtm_rwlock::write_unlock ()
{
  pthread_mutex_lock (&this->mutex);

  unsigned int sum = this->summary.load (memory_order_relaxed);
  this->summary.store (sum & ~a_writer, memory_order_relaxed);

  // If there is a waiting writer, wake it.
  if (unlikely (sum & w_writer))
    pthread_cond_signal (&this->c_writers);

  // If there are waiting readers, wake them.
  else if (unlikely (sum & w_reader))
    pthread_cond_broadcast (&this->c_readers);

  pthread_mutex_unlock (&this->mutex);
}

} // namespace GTM
