/* Copyright (C) 2008-2025 Free Software Foundation, Inc.
   Contributed by Embecosm on behalf of Adapteva, Inc.

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

#include "../epiphany-asm.h"

	/* Assumptions: NaNs have all bits 10..30 and one of bit 0..9 set.
			after sub: AC = ~Borrow.
	   clobber: TMP0
	   output: gt / gte indicates greater / greater or equal.  */

	FSTAB (__gtesf2,T_INT)
	.global SYM(__gtesf2)
	.balign 4
	HIDDEN_FUNC(__gtesf2)
SYM(__gtesf2):
#ifndef FLOAT_FORMAT_MOTOROLA
	mov	TMP0,0xffff
	movt	TMP0,0x7f
	add	TMP0,TMP0,r0
	eor	TMP0,TMP0,r0
	blt	.Lret
	mov	TMP0,0xffff
	movt	TMP0,0x7f
	add	TMP0,TMP0,r1
#else
	add	TMP0,r0,0x3ff; check for r0 NaN
	eor	TMP0,TMP0,r0
	blt	.Lret
	add	TMP0,r1,0x3ff; check for r1 NaN
#endif
	eor	TMP0,TMP0,r1
	blt	.Lret
	and	TMP0,r0,r1
	blt	.Lneg
	orr	TMP0,r0,r1
	lsl	TMP0,TMP0,1
	beq	.Lret
	sub	TMP0,r0,r1
.Lret:
	rts
	.balign 4
.Lneg:
	sub	TMP0,r1,r0
	rts
	ENDFUNC(__gtesf2)
