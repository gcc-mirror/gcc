/* Copyright (C) 2001-2013 Free Software Foundation, Inc.

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

/* 4 slots for argument spill area.  1 for cpreturn, 1 for stack.
   Return spill offset of 40 and 20.  Aligned to 16 bytes for n32.  */

#ifdef	__mips16
#define RA $7
#else
#define RA $31
#endif

	.section .init,"ax",@progbits
#ifdef __mips64
	ld      RA,40($sp)
	daddu	$sp,$sp,48
#else
	lw	RA,20($sp)
	addu	$sp,$sp,32
#endif
	j	RA

	.section .fini,"ax",@progbits
#ifdef	__mips64
	ld	RA,40($sp)
	daddu	$sp,$sp,48
#else
	lw	RA,20($sp)
	addu	$sp,$sp,32
#endif
	j	RA

