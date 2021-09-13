/* Copyright (C) 2015-2021 Free Software Foundation, Inc.
   Contributed by Dmitry Melnik <dm@ispras.ru>

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

/* This file implements timer routines for NVPTX.  It uses the %clock64 cycle
   counter.  */

#include "libgomp.h"

/* This is set from host in plugin-nvptx.c.  */
double __nvptx_clocktick = 0;

double
omp_get_wtime (void)
{
  uint64_t clock;
  asm ("mov.u64 %0, %%clock64;" : "=r" (clock));
  return clock * __nvptx_clocktick;
}

double
omp_get_wtick (void)
{
  return __nvptx_clocktick;
}

ialias (omp_get_wtime)
ialias (omp_get_wtick)
