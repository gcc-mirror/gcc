/* Copyright (C) 2009-2014 Free Software Foundation, Inc.
   This file is part of GCC.
   Contributed by Maciej W. Rozycki <macro@linux-mips.org>.

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

#ifdef L_udivsi3
	.text
	.globl	__udivsi3
	.type	__udivsi3, @function
__udivsi3:
	.word	0
	movl	8(%ap), %r1
	blss	0f			/* Check bit #31 of divisor.  */
	movl	4(%ap), %r2
	blss	1f			/* Check bit #31 of dividend.  */

	/* Both zero, do a standard division.  */

	divl3	%r1, %r2, %r0
	ret

	/* MSB of divisor set, only 1 or 0 may result.  */
0:
	decl	%r1
	clrl	%r0
	cmpl	%r1, 4(%ap)
	adwc	$0, %r0
	ret

	/* MSB of dividend set, do an extended division.  */
1:
	clrl	%r3
	ediv	%r1, %r2, %r0, %r3
	ret
	.size	__udivsi3, . - __udivsi3
	.previous
#endif

#ifdef L_umodsi3
	.text
	.globl	__umodsi3
	.type	__umodsi3, @function
__umodsi3:
	.word	0
	movl	8(%ap), %r1
	blss	0f			/* Check bit #31 of divisor.  */
	movl	4(%ap), %r2
	blss	1f			/* Check bit #31 of dividend.  */

	/* Both zero, do a standard division.  */

	divl3	%r1, %r2, %r0
	mull2	%r0, %r1
	subl3	%r1, %r2, %r0
	ret

	/* MSB of divisor set, subtract the divisor at most once.  */
0:
	movl	4(%ap), %r2
	clrl	%r0
	cmpl	%r2, %r1
	sbwc	$0, %r0
	bicl2	%r0, %r1
	subl3	%r1, %r2, %r0
	ret

	/* MSB of dividend set, do an extended division.  */
1:
	clrl	%r3
	ediv	%r1, %r2, %r3, %r0
	ret
	.size	__umodsi3, . - __umodsi3
	.previous
#endif
