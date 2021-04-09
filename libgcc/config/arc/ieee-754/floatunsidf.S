/* Copyright (C) 2008-2021 Free Software Foundation, Inc.
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
	.global __floatunsidf
	.balign 4
	FUNC(__floatunsidf)
__floatunsidf:
	push_s blink
	bl.d __floatunsidf_c
	push_s r0
	ld_s r2,[sp]
	st_s r1,[sp]
	push_s r0
	bl.d __floatunsidf_asm
	mov_s r0,r2
	pop_s r2
	pop_s r3
	pop_s blink
	cmp r0,r2
	cmp.eq r1,r3
	jeq_s [blink]
	bl abort
	ENDFUNC(__floatunsidf)
#define __floatunsidf __floatunsidf_asm
#endif /* DEBUG */

	.global __floatunsidf
	.balign 4
	FUNC(__floatunsidf)
__floatunsidf:
	lsr_s r1,r0
	breq_s r0,0,.Lret0
	norm r2,r1
	mov r12,-0x41d ; -(0x3ff+31-1)
	rsub.f r3,r2,11
	add_s r12,r2,r12
	add_s r2,r2,21
#ifdef __LITTLE_ENDIAN__
	lsr DBL0H,r0,r3
	asl_s DBL0L,r0,r2
#else
	asl DBL0L,r0,r2
	lsr_s DBL0H,r0,r3
#endif
	asl_s r12,r12,20
	mov.lo DBL0H,DBL0L
	sub_s DBL0H,DBL0H,r12
.Lret0:	j_s.d [blink]
	mov.ls DBL0L,0
	ENDFUNC(__floatunsidf)
