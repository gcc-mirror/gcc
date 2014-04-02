/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Jonah Graham (jgraham@altera.com).
   Contributed by Mentor Graphics, Inc.

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


/* This file just makes sure that the .fini and .init sections do in
   fact return.  Users may put any desired instructions in those sections.
   This file is the last thing linked into any executable.  */	

	.section	".init"
	ldw	ra, 44(sp)
	ldw	r23, 40(sp)
	ldw	r22, 36(sp)
	ldw	r21, 32(sp)
	ldw	r20, 28(sp)
	ldw	r19, 24(sp)
	ldw	r18, 20(sp)
	ldw	r17, 16(sp)
	ldw	r16, 12(sp)
	ldw	fp, 8(sp)
	addi	sp, sp, 48
	ret
	
	.section	".fini"
	ldw	ra, 44(sp)
	ldw	r23, 40(sp)
	ldw	r22, 36(sp)
	ldw	r21, 32(sp)
	ldw	r20, 28(sp)
	ldw	r19, 24(sp)
	ldw	r18, 20(sp)
	ldw	r17, 16(sp)
	ldw	r16, 12(sp)
	ldw	fp, 8(sp)
	addi	sp, sp, 48
	ret

