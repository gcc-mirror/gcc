/* Copyright (C) 2010-2019 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License
   and a copy of the GCC Runtime Library Exception along with this
   program; see the files COPYING3 and COPYING.RUNTIME respectively.
   If not, see <http://www.gnu.org/licenses/>.  */

#ifdef __mips_hard_float

/* Flush denormalized numbers to zero.  */
#define _FPU_FLUSH_TZ   0x1000000

/* Rounding control.  */
#define _FPU_RC_NEAREST 0x0     /* RECOMMENDED */
#define _FPU_RC_ZERO    0x1
#define _FPU_RC_UP      0x2
#define _FPU_RC_DOWN    0x3

/* Enable interrupts for IEEE exceptions.  */
#define _FPU_IEEE     0x00000F80

/* Macros for accessing the hardware control word.  */
#define _FPU_GETCW(cw) __asm__ ("cfc1 %0,$31" : "=r" (cw))
#define _FPU_SETCW(cw) __asm__ ("ctc1 %0,$31" : : "r" (cw))

static void __attribute__((constructor,nomips16))
set_fast_math (void)
{
  unsigned int fcr;

  /* Flush to zero, round to nearest, IEEE exceptions disabled.  */
  fcr = _FPU_FLUSH_TZ | _FPU_RC_NEAREST;

  _FPU_SETCW(fcr);
}

#endif /* __mips_hard_float */
