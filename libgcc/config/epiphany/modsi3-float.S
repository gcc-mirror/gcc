/* Unsigned 32 bit division optimized for Epiphany.
   Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

	FSTAB (__modsi3,T_UINT)
	.global SYM(__modsi3)
	.balign 4
	HIDDEN_FUNC(__modsi3)
SYM(__modsi3):
	asr TMP3,r0,31 ; save sign
	 float TMP0,r0
	float TMP1,r1
	 mov r2,0
	sub TMP4,r2,r0
	beq .Lret_r0
	movgt r0,TMP4
	sub TMP2,r2,r1
	movlte TMP2,r1
	sub r2,TMP0,TMP1
	lsl r2,r2,1
	blte .L0step
	asr TMP4,r2,24
	lsl r2,TMP4,3
	mov TMP4,%low(.L0step)
	movt TMP4,%high(.L0step)
	sub r2,TMP4,r2
	jr r2
#define STEP(n) lsl.l r2,TMP2,n` sub r2,r0,r2` movgteu r0,r2
	.balign 8,,2
	STEP(31)` STEP(30)` STEP(29)` STEP(28)`
	STEP(27)` STEP(26)` STEP(25)` STEP(24)`
	STEP(23)` STEP(22)` STEP(21)` STEP(20)`
	STEP(19)` STEP(18)` STEP(17)` STEP(16)`
	STEP(15)` STEP(14)` STEP(13)` STEP(12)`
	STEP(11)` STEP(10)` STEP(9)`  STEP(8)`
	STEP(7)` STEP(6)` STEP(5)` STEP(4)` STEP(3)` STEP(2)` STEP(1)
.L0step:STEP(0)
	eor r0,r0,TMP3 ; restore sign
	sub r0,r0,TMP3
.Lret_r0:
	rts
	ENDFUNC(__modsi3)
