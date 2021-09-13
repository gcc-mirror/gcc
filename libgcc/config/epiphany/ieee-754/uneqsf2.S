/* Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

	FSTAB (__uneqsf2,T_INT)
	.global SYM(__uneqsf2)
	.balign 8,,2
	HIDDEN_FUNC(__uneqsf2)
SYM(__uneqsf2):
        sub     TMP0,r0,r1
        beq     .Lret
        orr     TMP0,r0,r1
        add     TMP0,TMP0,TMP0
        beq     .Lret
	mov	TMP0,1
	movt	TMP0,0xff00
	lsl	TMP1,r0,1
	sub	TMP1,TMP0,TMP1
	blteu	.Lret
	lsl	TMP1,r1,1
	sub	TMP1,TMP0,TMP1
.Lret:	rts /* uneq: lteu */
	ENDFUNC(__uneqsf2)
