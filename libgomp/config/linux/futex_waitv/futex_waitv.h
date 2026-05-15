/* Copyright The GNU Toolchain Authors.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
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

#define _GNU_SOURCE
#include <sys/syscall.h>

#ifndef SYS_futex_waitv
#  error SYS_futex_waitv is required for the linux/futex_waitv libgomp target
#endif

#include <linux/futex.h>
#include <stdint.h>
#include <string.h>
#include <errno.h>
#include <time.h>

#pragma GCC visibility push(default)
#include <unistd.h>
#pragma GCC visibility pop

/* Only define the interface we need.  `futex_waitv` can wait on more
   addresses, but the barrier code only needs two.

   See https://docs.kernel.org/userspace-api/futex2.html for documentation on
   futex_waitv syscall.  */
static inline void
futex_waitv (uint32_t *addr, uint32_t val, uint32_t *addr2, uint32_t val2)
{
  struct futex_waitv addrs[2];
  addrs[0].val = val;
  addrs[0].uaddr = (uint64_t) (uintptr_t) addr;
  addrs[0].flags = FUTEX_PRIVATE_FLAG | FUTEX_32;
  addrs[0].__reserved = 0;
  addrs[1].val = val2;
  addrs[1].uaddr = (uint64_t) (uintptr_t) addr2;
  addrs[1].flags = FUTEX_PRIVATE_FLAG | FUTEX_32;
  addrs[1].__reserved = 0;
  int err __attribute__ ((unused))
    = syscall (SYS_futex_waitv, addrs, 2, 0, NULL, CLOCK_MONOTONIC);
  /* If a signal woke us then we simply leave and let the loop outside of us
     handle it.  We never require knowledge about whether anything changed or
     not.  */
  gomp_assert (err >= 0 || errno == EAGAIN || errno == EINTR,
	       "failed with futex_waitv err = %d, message: %s", err,
	       strerror (errno));
}
