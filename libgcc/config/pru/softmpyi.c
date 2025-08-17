/* libgcc routines for PRU
   Copyright (C) 2025 Free Software Foundation, Inc.
   Based on rl78/lib2mul.c

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

typedef unsigned int  uint32_type   __attribute__ ((mode (SI)));

#define C2B(a,b) a##b
#define C2(a,b) C2B(a,b)

#undef UINT_TYPE
#undef NAME_MODE

#define UINT_TYPE       uint32_type
#define NAME_MODE       i

#include "pru-softmpy.h"
