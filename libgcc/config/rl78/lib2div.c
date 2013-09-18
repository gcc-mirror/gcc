/* libgcc routines for RL78
   Copyright (C) 2005-2013 Free Software Foundation, Inc.
   Contributed by Red Hat.

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

typedef          int  sint32_type   __attribute__ ((mode (SI)));
typedef unsigned int  uint32_type   __attribute__ ((mode (SI)));
typedef          int  sint16_type   __attribute__ ((mode (HI)));
typedef unsigned int  uint16_type   __attribute__ ((mode (HI)));
typedef          int  sint08_type   __attribute__ ((mode (QI)));
typedef unsigned int  uint08_type   __attribute__ ((mode (QI)));
typedef int           word_type     __attribute__ ((mode (__word__)));

#define C3B(a,b,c) a##b##c
#define C3(a,b,c) C3B(a,b,c)

#if 0

#define UINT_TYPE	uint32_type
#define SINT_TYPE	sint32_type
#define BITS_MINUS_1	31
#define NAME_MODE	si

#include "rl78-divmod.h"

#undef UINT_TYPE
#undef SINT_TYPE
#undef BITS_MINUS_1
#undef NAME_MODE

#define UINT_TYPE	uint16_type
#define SINT_TYPE	sint16_type
#define BITS_MINUS_1	15
#define NAME_MODE	hi

#include "rl78-divmod.h"

#undef UINT_TYPE
#undef SINT_TYPE
#undef BITS_MINUS_1
#undef NAME_MODE

#define UINT_TYPE	uint08_type
#define SINT_TYPE	sint08_type
#define BITS_MINUS_1	7
#define NAME_MODE	qi

#include "rl78-divmod.h"

#endif

/* See the comment by the definition of LIBGCC2_UNITS_PER_WORD in
   m32c.h for why we are creating extra versions of some of the
   functions defined in libgcc2.c.  */

#define LIBGCC2_UNITS_PER_WORD 2

#define L_clzsi2
#define L_ctzsi2
#define L_ffssi2
#define L_paritysi2
#define L_popcountsi2

#include "libgcc2.c"
