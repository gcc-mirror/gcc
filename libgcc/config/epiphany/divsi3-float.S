/* Signed 32 bit division optimized for Epiphany.
   Copyright (C) 2009-2019 Free Software Foundation, Inc.
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

	FSTAB (__divsi3,T_UINT)
	.global SYM(__divsi3)
	.balign 4
	HIDDEN_FUNC(__divsi3)
SYM(__divsi3):
	float TMP2,r0
	  mov TMP4,0
	float TMP1,r1
	  sub TMP0,TMP4,r0
	beq .Lret_r0
	movgt r0,TMP0
	sub TMP0,TMP4,r1
	movgt r1,TMP0
	mov TMP0,1
	sub TMP2,TMP2,TMP1
	asr TMP3,TMP2,31 ; save sign
	lsl TMP2,TMP2,1
	blt .Lret0
	sub TMP1,TMP2,1 ; rounding compensation, avoid overflow
	movgte TMP2,TMP1
	lsr TMP2,TMP2,24
	lsl r1,r1,TMP2
	lsl TMP0,TMP0,TMP2
	sub TMP1,r0,r1
	movgteu r0,TMP1
	movgteu TMP4,TMP0
	lsl TMP5,TMP0,1
	sub TMP1,r0,r1
	movgteu r0,TMP1
	movgteu TMP4,TMP5
	sub TMP1,r1,1
	mov r1,%low(.L0step)
	movt r1,%high(.L0step)
	lsl TMP2,TMP2,3
	sub r1,r1,TMP2
	jr r1
	.rep 30
	lsl r0,r0,1
	sub.l r1,r0,TMP1
	movgteu r0,r1
	.endr
.L0step:sub r1,TMP0,1 ; mask result bits from steps ...
	and r0,r0,r1
	orr r0,r0,TMP4 ; ... and combine with first bit.
	eor r0,r0,TMP3 ; restore sign
	sub r0,r0,TMP3
.Lret_r0:rts
.Lret0:	mov r0,0
	rts
	ENDFUNC(__divsi3)
