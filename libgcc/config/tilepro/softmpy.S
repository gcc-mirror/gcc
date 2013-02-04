/* 64-bit multiplication support for TILEPro.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* 64-bit multiplication support.  */

	.file "softmpy.S"

/* Parameters */
#define lo0             r9   /* low 32 bits of n0  */
#define hi0             r1   /* high 32 bits of n0 */
#define lo1             r2   /* low 32 bits of n1  */
#define hi1             r3   /* high 32 bits of n1 */

/* temps */
#define result1_a       r4
#define result1_b       r5

#define tmp0            r6
#define tmp0_left_16    r7
#define tmp1            r8

	.section .text.__muldi3, "ax"
	.align 8
	.globl __muldi3
	.type __muldi3, @function
__muldi3:
	{
	 move	     lo0, r0 /* so we can write "out r0" while "in r0" alive */
	 mulhl_uu    tmp0, lo1, r0
	}
	{
	 mulll_uu    result1_a, lo1, hi0
	}
	{
	 move        tmp1, tmp0
	 mulhla_uu   tmp0, lo0, lo1
	}
	{
	 mulhlsa_uu  result1_a, lo1, hi0
	}
	{
	 mulll_uu    result1_b, lo0, hi1
	 slt_u       tmp1, tmp0, tmp1
	}
	{
	 mulhlsa_uu  result1_a, lo0, hi1
	 shli        r0, tmp0, 16
	}
	{
	 move        tmp0_left_16, r0
	 mulhha_uu   result1_b, lo0, lo1
	}
	{
	 mullla_uu   r0, lo1, lo0
	 shli        tmp1, tmp1, 16
	}
	{
	 mulhlsa_uu  result1_b, hi0, lo1
	 inthh       tmp1, tmp1, tmp0
	}
	{
	 mulhlsa_uu  result1_a, hi1, lo0
	 slt_u       tmp0, r0, tmp0_left_16
	}
	/* NOTE: this will stall for a cycle here. Oh well. */
	{
	 add         r1, tmp0, tmp1
	 add         result1_a, result1_a, result1_b
	}
	{
	 add         r1, r1, result1_a
	 jrp         lr
	}
	.size __muldi3,.-__muldi3
