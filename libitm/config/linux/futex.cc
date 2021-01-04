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

/* Provide access to the futex system call.  */

#include "libitm_i.h"
#include "futex.h"
#include <futex_bits.h>
#include <errno.h>

namespace GTM HIDDEN {

#define FUTEX_WAIT		0
#define FUTEX_WAKE		1
#define FUTEX_PRIVATE_FLAG	128


static int gtm_futex_wait = FUTEX_WAIT | FUTEX_PRIVATE_FLAG;
static int gtm_futex_wake = FUTEX_WAKE | FUTEX_PRIVATE_FLAG;


void
futex_wait (std::atomic<int> *addr, int val)
{
  long res;

  res = sys_futex0 (addr, gtm_futex_wait, val);
  if (__builtin_expect (res == -ENOSYS, 0))
    {
      gtm_futex_wait = FUTEX_WAIT;
      gtm_futex_wake = FUTEX_WAKE;
      res = sys_futex0 (addr, FUTEX_WAIT, val);
    }
  if (__builtin_expect (res < 0, 0))
    {
      if (res == -EWOULDBLOCK || res == -ETIMEDOUT)
	;
      else if (res == -EFAULT)
	GTM_fatal ("futex failed (EFAULT %p)", addr);
      else
	GTM_fatal ("futex failed (%s)", strerror(-res));
    }
}


long
futex_wake (std::atomic<int> *addr, int count)
{
  long res = sys_futex0 (addr, gtm_futex_wake, count);
  if (__builtin_expect (res == -ENOSYS, 0))
    {
      gtm_futex_wait = FUTEX_WAIT;
      gtm_futex_wake = FUTEX_WAKE;
      res = sys_futex0 (addr, FUTEX_WAKE, count);
    }
  if (__builtin_expect (res < 0, 0))
    GTM_fatal ("futex failed (%s)", strerror(-res));
  else
    return res;
}

} // namespace GTM
