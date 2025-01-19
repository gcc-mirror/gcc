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

/* Because we handle a divident with bit 31 set with truncating integer
   arithmetic, there is no rounding-related overflow.  */
	FSTAB (__umodsi3,T_UINT)
	.global SYM(__umodsi3)
	.balign 4
	HIDDEN_FUNC(__umodsi3)
SYM(__umodsi3):
	float r2,r0
	  mov TMP1,%low(0xb0800000) ; ??? this would be faster with small data
	float TMP2,r1
	  movt TMP1,%high(0xb0800000)
	asr TMP0,r0,8
	sub TMP0,TMP0,TMP1
	mov TMP1,%low(.L0step)
	movgteu r2,TMP0
	sub r2,r2,TMP2
	blteu .L0step
	asr r2,r2,23
	movt TMP1,%high(.L0step)
	lsl TMP2,r2,3
	lsl r2,r1,r2` sub r2,r0,r2` movgteu r0,r2 ; STEP(r2)
	sub r2,TMP1,TMP2
	jr r2
#define STEP(n) lsl.l r2,r1,n` sub r2,r0,r2` movgteu r0,r2
	.balign 8,,2
	STEP(31)` STEP(30)` STEP(29)` STEP(28)`
	STEP(27)` STEP(26)` STEP(25)` STEP(24)`
	STEP(23)` STEP(22)` STEP(21)` STEP(20)`
	STEP(19)` STEP(18)` STEP(17)` STEP(16)`
	STEP(15)` STEP(14)` STEP(13)` STEP(12)`
	STEP(11)` STEP(10)` STEP(9)`  STEP(8)`
	STEP(7)` STEP(6)` STEP(5)` STEP(4)` STEP(3)` STEP(2)` STEP(1)
.L0step:STEP(0)
.Lret_r0:
	rts
	ENDFUNC(__umodsi3)
