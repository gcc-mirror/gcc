/* Copyright (C) 2008-2017 Free Software Foundation, Inc.
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

#if 0 /* DEBUG */
	FUNC(__fixunsdfsi)
	.global __fixunsdfsi
	.balign 4
__fixunsdfsi:
	push_s blink
	push_s r0
	bl.d __fixunsdfsi_c
	push_s r1
	mov_s r2,r0
	pop_s r1
	ld r0,[sp]
	bl.d __fixunsdfsi_asm
	st r2,[sp]
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__fixunsdfsi)
#define __fixunsdfsi __fixunsdfsi_asm
#endif /* DEBUG */

	.global __fixunsdfsi
	FUNC(__fixunsdfsi)
	.balign 4
__fixunsdfsi:
	bbit0 DBL0H,30,.Lret0or1
	lsr r2,DBL0H,20
	bmsk_s DBL0H,DBL0H,19
	sub_s r2,r2,19; 0x3ff+20-0x400
	neg_s r3,r2
	btst_s r3,10
	bset_s DBL0H,DBL0H,20
#ifdef __LITTLE_ENDIAN__
	mov.ne DBL0L,DBL0H
	asl DBL0H,DBL0H,r2
#else
	asl.eq DBL0H,DBL0H,r2
	lsr.ne DBL0H,DBL0H,r3
#endif
	lsr DBL0L,DBL0L,r3
	j_s.d [blink]
	add.eq r0,r0,r1
.Lret0:
	j_s.d [blink]
	mov_l r0,0
.Lret0or1:
	add_s DBL0H,DBL0H,0x100000
	lsr_s DBL0H,DBL0H,30
	j_s.d [blink]
	bmsk_l r0,DBL0H,0
	ENDFUNC(__fixunsdfsi)
