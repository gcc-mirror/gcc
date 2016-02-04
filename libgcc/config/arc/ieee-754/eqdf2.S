/* Copyright (C) 2008-2016 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "arc-ieee-754.h"
/* inputs: DBL0, DBL1
   output: z flag
   clobber: r12, flags
   For NaNs, bit 19.. bit 30 of the high word must be set.  */
#if 0 /* DEBUG */
	.global __eqdf2
	.balign 4
	FUNC(__eqdf2)
__eqdf2:
	st.a r11,[sp,-4]` push_s blink` st.a r10,[sp,-4]` st.a r9,[sp,-4]
	st.a r8,[sp,-4]` st.a r7,[sp,-4]` st.a r6,[sp,-4]` st.a r5,[sp,-4]
	st.a r4,[sp,-4]` push_s r3` push_s r2` push_s r1`
	bl.d __eqdf2_c` push_s r0
	mov r11,r0` pop_s r0` pop_s r1` pop_s r2` pop_s r3
	ld.ab r4,[sp,4]` ld.ab r5,[sp,4]` ld.ab r6,[sp,4]`
	ld.ab r7,[sp,4]` ld.ab r8,[sp,4]` ld.ab r9,[sp,4]
	bl.d __eqdf2_asm` ld.ab r10,[sp,4]
	pop_s blink
	breq.d r11,0,0f
	ld.ab r11,[sp,4]
	jne_s [blink]
	bl abort
0:	jeq_s [blink]
	bl abort
	ENDFUNC(__eqdf2)
#define __eqdf2 __eqdf2_asm
#endif /* DEBUG */
	.global __eqdf2
	.balign 4
	HIDDEN_FUNC(__eqdf2)
	/* Good performance as long as the difference in high word is
	   well predictable (as seen from the branch predictor).  */
__eqdf2:
	brne.d DBL0H,DBL1H,.Lhighdiff
	bmsk r12,DBL0H,20
#ifdef DPFP_COMPAT
	or.f 0,DBL0L,DBL1L
	bset.ne r12,r12,21
#endif /* DPFP_COMPAT */
	add1.f	r12,r12,DBL0H /* set c iff NaN; also, clear z if NaN.  */
	j_s.d	[blink]
	cmp.cc	DBL0L,DBL1L
	.balign 4
.Lhighdiff:
	or	r12,DBL0H,DBL1H
	or.f	0,DBL0L,DBL1L
	j_s.d	[blink]
	bmsk.eq.f r12,r12,30
	ENDFUNC(__eqdf2)
/* ??? could we do better by speeding up some 'common' case of inequality?  */
