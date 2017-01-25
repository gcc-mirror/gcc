/* Signed 32 bit division optimized for Epiphany.
   Copyright (C) 2009-2017 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

This file is part of GCC.

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

#include "epiphany-asm.h"

	FSTAB (__divsi3,T_INT)
	.global SYM(__divsi3)
	.balign 4
	HIDDEN_FUNC(__divsi3)
SYM(__divsi3):
	mov r12,0
	sub r2,r12,r0
	movlt r2,r0
	sub r3,r12,r1
	movlt r3,r1
	sub r19,r2,r3
	bltu .Lret0
	movt r12,0x4000
	orr r16,r2,r12
	orr r18,r3,r12
	 fsub r16,r16,r12
	fsub r18,r18,r12
	 movt r12,0x4b80
	lsr r19,r3,23
	lsr r17,r2,23
	movt r17,0x4b80
	fsub r17,r17,r12
	 movt r19,0x4b80
	fsub r19,r19,r12
	 mov r12,%low(.L0step)
	movt r12,%high(.L0step)
	mov r20,0
	mov r21,1
	movne r16,r17
	lsr r17,r3,23
	movne r18,r19
	eor r1,r1,r0 ; save sign
	asr r19,r1,31
	lsr r1,r16,23
	lsr r0,r18,23
	sub r1,r1,r0 ; calculate bit number difference.
	lsl r3,r3,r1
	lsr r16,r3,1
	lsl r0,r21,r1
	lsl r1,r1,3
	sub r12,r12,r1
	sub r3,r2,r3
	movgteu r2,r3
	movgteu r20,r0
	lsr r0,r0,1
	add r17,r0,r20
	sub r3,r2,r16
	movgteu r2,r3
	movgteu r20,r17
	sub r16,r16,1
	jr r12
	.rep 30
	lsl r2,r2,1
	sub r3,r2,r16
	movgteu r2,r3
	.endr
	sub r0,r0,1 ; mask result bits from steps ...
	and r0,r0,r2
	orr r20,r0,r20 ; ... and combine with first bit.
.L0step:eor r0,r20,r19 ; restore sign
	sub r0,r0,r19
	rts
.Lret0:	mov r0,0
	rts
	ENDFUNC(__divsi3)
