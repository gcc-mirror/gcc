/* Copyright (C) 2008-2024 Free Software Foundation, Inc.
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
	FUNC(__fixdfsi)
	.global __fixdfsi
	.balign 4
__fixdfsi:
	push_s blink
	push_s r0
	bl.d __fixdfsi_c
	push_s r1
	mov_s r2,r0
	pop_s r1
	ld r0,[sp]
	bl.d __fixdfsi_asm
	st r2,[sp]
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__fixdfsi)
#define __fixdfsi __fixdfsi_asm
#endif /* DEBUG */

/*      If the fraction has to be shifted left by a positive non-zero amount,
        we have to combine bits from DBL0L and DBL0H.  If we shift right,
        or shift by zero, we only want to have the bits from DBL0H in r0.  */

	.global __fixdfsi
	FUNC(__fixdfsi)
	.balign 4
__fixdfsi:
	bbit0 DBL0H,30,.Lret0or1
	asr r2,DBL0H,20
	bmsk_s DBL0H,DBL0H,19
	sub_s r2,r2,19; 0x3ff+20-0x400
	neg_s r3,r2
	asr.f 0,r3,11
	bset_s DBL0H,DBL0H,20
#ifdef __LITTLE_ENDIAN__
	mov.cs DBL0L,DBL0H
	asl DBL0H,DBL0H,r2
#else
	asl.cc DBL0H,DBL0H,r2
	lsr.cs DBL0H,DBL0H,r3
#endif
	lsr_s DBL0L,DBL0L,r3

	add.cc r0,r0,r1
	j_s.d [blink]
	neg.pl r0,r0
.Lret0or1:
	add.f r0,DBL0H,0x100000
	lsr_s r0,r0,30

	bmsk_s r0,r0,0
	j_s.d [blink]
	neg.mi r0,r0
	ENDFUNC(__fixdfsi)
