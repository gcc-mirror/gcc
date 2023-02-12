/* Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

#ifndef GTM_RWLOCK_H
#define GTM_RWLOCK_H

#include "local_atomic"
#include "common.h"

namespace GTM HIDDEN {

struct gtm_thread;

// This datastructure is the blocking, futex-based version of the Dekker-style
// reader-writer lock used to provide mutual exclusion between active and
// serial transactions.
// See libitm's documentation for further details.
//
// In this implementation, writers are given highest priority access but
// read-to-write upgrades do not have a higher priority than writers.
//
// Do not change the layout of this class; it must remain a POD type with
// standard layout, and the writers field must be first (i.e., so the
// assembler code can assume that its address is equal to the address of the
// respective instance of the class), and htm_fastpath must be second.

class gtm_rwlock
{
  std::atomic<int> writers;       // Writers' futex.
  // We put the HTM fastpath control variable here so that HTM fastpath
  // transactions can check efficiently whether they are allowed to run.
  // This must be accessed atomically because threads can load this value
  // when they are neither a registered reader nor writer (i.e., when they
  // attempt to execute the HTM fastpath).
  std::atomic<uint32_t> htm_fastpath;
  // TODO Put these futexes on different cachelines?  (writers and htm_fastpath
  // should remain on the same cacheline.
  std::atomic<int> writer_readers;// A confirmed writer waits here for readers.
  std::atomic<int> readers;       // Readers wait here for writers (iff true).

 public:
  gtm_rwlock() : writers(0), htm_fastpath(0), writer_readers(0), readers(0)
  { }

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
    return writers.load (memory_order_relaxed) != 0
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
