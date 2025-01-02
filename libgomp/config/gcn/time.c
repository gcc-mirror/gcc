/* Copyright (C) 2015-2025 Free Software Foundation, Inc.
   Contributed by Mentor Embedded.

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

/* This file implements timer routines for AMD GCN.  */

#include "libgomp.h"

/* According to AMD:
    dGPU RTC is 27MHz
    AGPU RTC is 100MHz
    RDNA3 ISA manual states "typically 100MHz"
   FIXME: DTRT on an APU.  */
#ifdef __RDNA3__
#define RTC_TICKS (1.0 / 100000000.0) /* 100MHz */
#else
#define RTC_TICKS (1.0 / 27000000.0) /* 27MHz */
#endif

double
omp_get_wtime (void)
{
  uint64_t clock;
#ifdef __RDNA3__
  asm ("s_sendmsg_rtn_b64 %0 0x83 ;Get REALTIME\n\t"
       "s_waitcnt 0" : "=r" (clock));
#else
  asm ("s_memrealtime %0\n\t"
       "s_waitcnt 0" : "=r" (clock));
#endif
  return clock * RTC_TICKS;
}

double
omp_get_wtick (void)
{
  return RTC_TICKS;
}

ialias (omp_get_wtime)
ialias (omp_get_wtick)
