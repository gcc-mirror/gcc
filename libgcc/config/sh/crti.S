/* Copyright (C) 2000-2019 Free Software Foundation, Inc.
   This file was adapted from glibc sources.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "crt.h"

/* The code in sections .init and .fini is supposed to be a single
   regular function.  The function in .init is called directly from
   start in crt1.S.  The function in .fini is atexit()ed in crt1.S
   too.

   crti.S contributes the prologue of a function to these sections,
   and crtn.S comes up the epilogue.  STARTFILE_SPEC should list
   crti.o before any other object files that might add code to .init
   or .fini sections, and ENDFILE_SPEC should list crtn.o after any
   such object files.  */

	.section .init
/* The alignment below can't be smaller, otherwise the mova below
   breaks.  Yes, we might align just the label, but then we'd be
   exchanging an alignment here for one there, since the code fragment
   below ensures 4-byte alignment on __ELF__.  */
#ifdef __ELF__
	.p2align 2
#else
	.p2align 1
#endif
	.global	 GLOBAL(_init)
GLOBAL(_init):
#ifdef __ELF__
	mov.l	r12,@-r15
	mova	0f,r0
	mov.l	0f,r12
#endif
	mov.l	r14,@-r15
#ifdef __ELF__
	add	r0,r12
#endif
	sts.l	pr,@-r15
#ifdef __ELF__
	bra	1f
#endif
	mov	r15,r14
#ifdef __ELF__
0:	.long	_GLOBAL_OFFSET_TABLE_
1:
#endif

	.section .fini
/* The alignment below can't be smaller, otherwise the mova below
   breaks.  Yes, we might align just the label, but then we'd be
   exchanging an alignment here for one there, since the code fragment
   below ensures 4-byte alignment on __ELF__.  */
#ifdef __ELF__
	.p2align 2
#else
	.p2align 1
#endif
	.global  GLOBAL(_fini)
GLOBAL(_fini):	
#ifdef __ELF__
	mov.l	r12,@-r15
	mova	0f,r0
	mov.l	0f,r12
#endif
	mov.l	r14,@-r15
#ifdef __ELF__
	add	r0,r12
#endif
	sts.l	pr,@-r15
#ifdef __ELF__
	bra	1f
#endif
	mov	r15,r14
#ifdef __ELF__
0:	.long	_GLOBAL_OFFSET_TABLE_
1:
#endif
