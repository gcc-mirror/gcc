/* Unsigned 32 bit division optimized for Epiphany.
   Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

	FSTAB (__udivsi3,T_UINT)
	.global SYM(__udivsi3)
	.balign 4
	HIDDEN_FUNC(__udivsi3)
SYM(__udivsi3):
	sub r3,r0,r1
	bltu .Lret0
	mov r3,0x95
	lsl r12,r3,23 ; 0x4a800000
	lsl r3,r3,30 ; 0x40000000
	orr r16,r0,r3
	orr r2,r1,r3
	 fsub r16,r16,r3
	fsub r2,r2,r3
	 lsr r3,r1,21
	lsr r17,r0,21
	movt r17,0x4a80
	fsub r17,r17,r12
	 movt r3,0x4a80
	fsub r3,r3,r12
	 mov r12,%low(.L0step)
	movt r12,%high(.L0step)
	mov r21,1
	movne r16,r17
	lsr r17,r1,21
	movne r2,r3
	lsr r3,r16,23 ; must mask lower bits of r2 in case op0 was ..
	lsr r2,r2,23 ; .. shifted and op1 was not.
	sub r3,r3,r2 ; calculate bit number difference.
	lsl r1,r1,r3
	lsr r16,r1,1
	lsl r2,r21,r3
	lsl r3,r3,3
	sub r12,r12,r3
	sub r3,r0,r1
	movltu r3,r0
	mov r0,0
	movgteu r0,r2
	lsr r2,r2,1
	add r17,r2,r0
	sub r1,r3,r16
	movgteu r3,r1
	movgteu r0,r17
	sub r16,r16,1
	jr r12
	.rep 30
	lsl r3,r3,1
	sub r1,r3,r16
	movgteu r3,r1
	.endr
	sub r2,r2,1 ; mask result bits from steps ...
	and r3,r3,r2
	orr r0,r0,r3 ; ... and combine with first bits.
	nop
.L0step:rts
.Lret0:	mov r0,0
	rts
	ENDFUNC(__udivsi3)
