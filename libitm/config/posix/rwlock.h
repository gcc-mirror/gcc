/* Copyright (C) 2009-2024 Free Software Foundation, Inc.
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

#ifndef GTM_RWLOCK_H
#define GTM_RWLOCK_H

#include <pthread.h>
#include "local_atomic"

namespace GTM HIDDEN {

struct gtm_thread;

// This datastructure is the blocking, mutex-based side of the Dekker-style
// reader-writer lock used to provide mutual exclusion between active and
// serial transactions. It has similarities to POSIX pthread_rwlock_t except
// that we also provide for upgrading a reader->writer lock, with a
// positive indication of failure (another writer acquired the lock
// before we were able to acquire). While the writer flag (a_writer below) is
// global and protected by the mutex, there are per-transaction reader flags,
// which are stored in a transaction's shared state.
// See libitm's documentation for further details.
//
// In this implementation, writers are given highest priority access but
// read-to-write upgrades do not have a higher priority than writers.
//
// Do not change the layout of this class; it must remain a POD type with
// standard layout, and the summary field must be first (i.e., so the
// assembler code can assume that its address is equal to the address of the
// respective instance of the class), and htm_fastpath must be second.

class gtm_rwlock
{
  static const unsigned a_writer  = 1;	// An active writer.
  static const unsigned w_writer  = 2;	// The w_writers field != 0
  static const unsigned w_reader  = 4;  // The w_readers field != 0

  std::atomic<unsigned int> summary;	// Bitmask of the above.

  // We put the HTM fastpath control variable here so that HTM fastpath
  // transactions can check efficiently whether they are allowed to run.
  // This must be accessed atomically because threads can load this value
  // when they are neither a registered reader nor writer (i.e., when they
  // attempt to execute the HTM fastpath).
  std::atomic<uint32_t> htm_fastpath;

  pthread_mutex_t mutex;	        // Held if manipulating any field.
  pthread_cond_t c_readers;	        // Readers wait here
  pthread_cond_t c_writers;	        // Writers wait here for writers
  pthread_cond_t c_confirmed_writers;	// Writers wait here for readers

  unsigned int a_readers;	// Nr active readers as observed by a writer
  unsigned int w_readers;	// Nr waiting readers
  unsigned int w_writers;	// Nr waiting writers

 public:
  gtm_rwlock();
  ~gtm_rwlock();

  void read_lock (gtm_thread *tx);
  void read_unlock (gtm_thread *tx);

  void write_lock ();
  void write_unlock ();

  bool write_upgrade (gtm_thread *tx);
  void write_upgrade_finish (gtm_thread *tx);

  // Returns true iff there is a concurrent active or waiting writer, or
  // htm_fastpath is zero. This is primarily useful for simple HyTM
  // approaches, and the values being checked are loaded with
  // memory_order_relaxed.
  bool htm_fastpath_disabled ()
  {
    return (summary.load (memory_order_relaxed) & (a_writer | w_writer))
	|| htm_fastpath.load (memory_order_relaxed) == 0;
  }

  // This does not need to return an exact value, hence relaxed MO is
  // sufficient.
  uint32_t get_htm_fastpath ()
  {
    return htm_fastpath.load (memory_order_relaxed);
  }
  // This must only be called while having acquired the write lock, and other
  // threads do not need to load an exact value; hence relaxed MO is
  // sufficient.
  void set_htm_fastpath (uint32_t val)
  {
    htm_fastpath.store (val, memory_order_relaxed);
  }

 protected:
  bool write_lock_generic (gtm_thread *tx);
};

} // namespace GTM

#endif // GTM_RWLOCK_H
