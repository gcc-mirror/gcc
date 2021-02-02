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
	.global __floatsisf
	FUNC(__floatsisf)
	.balign 4
__floatsisf:
	push_s blink
	bl.d __floatsisf_c
	push_s r0
	ld_s r1,[sp]
	st_s r0,[sp]
	bl.d __floatsisf_asm
	mov_s r0,r1
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__floatsisf)
	.global __floatunsisf
	FUNC(__floatunsisf)
	.balign 4
__floatunsisf:
	push_s blink
	bl.d __floatunsisf_c
	push_s r0
	ld_s r1,[sp]
	st_s r0,[sp]
	bl.d __floatunsisf_asm
	mov_s r0,r1
	pop_s r1
	pop_s blink
	cmp r0,r1
	jeq_s [blink]
	bl abort
	ENDFUNC(__floatunsisf)
#define __floatsisf __floatsisf_asm
#define __floatunsisf __floatunsisf_asm
#endif /* DEBUG */

	.global __floatunsisf
	.global __floatsisf
	FUNC(__floatsisf)
	FUNC(__floatunsisf)
	.balign 4
__floatunsisf:
	lsr_s r2,r0
	mov_l r12,0x9d ; 0x7f + 31 - 1
	norm r2,r2
	brne_l r0,0,0f
	j_s [blink]
	.balign 4
__floatsisf:
	abs.f r0,r0
	jeq_s [blink]
	lsr_s r2,r0
	mov_s r12,0x9d ; 0x7f + 31 - 1
	norm r2,r2
	bset.cs r12,r12,8
0:	rsub.f r3,r2,8
	bmsk r1,r0,r3
	ror r1,r1,r3
	lsr.pl r0,r0,r3
	neg_s r3,r3
	asl.mi r0,r0,r3
	sub_s r12,r12,r2
	asl_s r12,r12,23
	bxor.pl.f r1,r1,31
	add_s r0,r0,r12
	j_s.d [blink]
	add.pnz r0,r0,1
	ENDFUNC(__floatunsisf)
	ENDFUNC(__floatsisf)
