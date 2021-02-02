/* Unsigned 32 bit modulo optimized for Epiphany.
   Copyright (C) 2009-2021 Free Software Foundation, Inc.
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

	FSTAB (__umodsi3,T_UINT)
	.global SYM(__umodsi3)
	.balign 4
	HIDDEN_FUNC(__umodsi3)
SYM(__umodsi3):
	mov r2,5
	lsl r2,r2,29 ; 0xa0000000
	orr r3,r2,r0
	lsr r15,r0,16
	movt r15,0xa800
	movne r3,r15
	lsr r16,r2,2 ; 0x28000000
	and r15,r3,r16
	fadd r12,r3,r15
	 orr r3,r2,r1
	lsr r2,r1,16
	movt r2,0xa800
	movne r3,r2
	and r2,r16,r3
	fadd r3,r3,r2
	 sub r2,r0,r1
	bltu .Lret_a
	lsr r12,r12,23
	mov r2,%low(.L0step)
	movt r2,%high(.L0step)
	lsr r3,r3,23
	sub r3,r12,r3 ; calculate bit number difference.
	lsl r3,r3,3
	sub r2,r2,r3
	jr r2
/*              lsl_l r2,r1,n`               sub r2,r0,r2` movgteu r0,r2  */
#define STEP(n) .long 0x0006441f | (n) << 5` sub r2,r0,r2` movgteu r0,r2
	.balign 8,,2
	STEP(31)` STEP(30)` STEP(29)` STEP(28)`
	STEP(27)` STEP(26)` STEP(25)` STEP(24)`
	STEP(23)` STEP(22)` STEP(21)` STEP(20)`
	STEP(19)` STEP(18)` STEP(17)` STEP(16)`
	STEP(15)` STEP(14)` STEP(13)` STEP(12)`
	STEP(11)` STEP(10)` STEP(9)`  STEP(8)`
	STEP(7)` STEP(6)` STEP(5)` STEP(4)` STEP(3)` STEP(2)` STEP(1)
.L0step:STEP(0)
.Lret_a:rts
	ENDFUNC(__umodsi3)
