/* Copyright (C) 2008, 2009, 2011 Free Software Foundation, Inc.
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

#ifndef LIBITM_TLS_H
#define LIBITM_TLS_H 1

namespace GTM HIDDEN {

#if !defined(HAVE_ARCH_GTM_THREAD) || !defined(HAVE_ARCH_GTM_THREAD_DISP)
// Provides a single place to store all this libraries thread-local data.
struct gtm_thread_tls
{
#ifndef HAVE_ARCH_GTM_THREAD
  // The currently active transaction.  Elided if the target provides
  // some efficient mechanism for storing this.
  gtm_thread *thr;
#endif
#ifndef HAVE_ARCH_GTM_THREAD_DISP
  // The dispatch table for the STM implementation currently in use.  Elided
  // if the target provides some efficient mechanism for storing this.
  abi_dispatch *disp;
#endif
};

extern __thread gtm_thread_tls _gtm_thr_tls;
#endif

#ifndef HAVE_ARCH_GTM_THREAD
// If the target does not provide optimized access to the thread-local
// data, simply access the TLS variable defined above.
static inline gtm_thread *gtm_thr() { return _gtm_thr_tls.thr; }
static inline void set_gtm_thr(gtm_thread *x) { _gtm_thr_tls.thr = x; }
#endif

#ifndef HAVE_ARCH_GTM_THREAD_DISP
// If the target does not provide optimized access to the currently
// active dispatch table, simply access via GTM_THR.
static inline abi_dispatch * abi_disp() { return _gtm_thr_tls.disp; }
static inline void set_abi_disp(abi_dispatch *x) { _gtm_thr_tls.disp = x; }
#endif

} // namespace GTM

#endif // LIBITM_TLS_H
