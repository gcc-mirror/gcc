/* Unsigned 32 bit division optimized for Epiphany.
   Copyright (C) 2009-2014 Free Software Foundation, Inc.
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
	sub TMP0,r0,r1
	bltu .Lret0
	float TMP2,r0
	  mov TMP1,%low(0xb0800000) ; ??? this would be faster with small data
	float TMP3,r1
	  movt TMP1,%high(0xb0800000)
	asr TMP0,r0,8
	sub TMP0,TMP0,TMP1
	movt TMP1,%high(0x00810000)
	movgteu TMP2,TMP0
	bblt .Lret1
	sub TMP2,TMP2,TMP1
	sub TMP2,TMP2,TMP3
	mov TMP3,0
	movltu TMP2,TMP3
	lsr TMP2,TMP2,23
	lsl r1,r1,TMP2
	mov TMP0,1
	lsl TMP0,TMP0,TMP2
	sub r0,r0,r1
	bltu .Ladd_back
	add TMP3,TMP3,TMP0
	sub r0,r0,r1
	bltu .Ladd_back
.Lsub_loop:; More than two iterations are rare, so it makes sense to leave
           ; this label here to reduce average branch penalties.
	add TMP3,TMP3,TMP0
	sub r0,r0,r1
	bgteu .Lsub_loop
.Ladd_back:
	add r0,r0,r1
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
	orr r0,r0,TMP3 ; ... and combine with first bits.
	rts
.Lret0:	mov r0,0
	rts
.Lret1:	mov r0,1
	rts
	ENDFUNC(__udivsi3)
