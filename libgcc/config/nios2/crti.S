/* Copyright (C) 2012-2017 Free Software Foundation, Inc.
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


/* This file just make a stack frame for the contents of the .fini and
   .init sections.  Users may put any desired instructions in those
   sections.

   While technically any code can be put in the init and fini sections
   most stuff will not work other than stuff which obeys the call frame
   and ABI.  All the call-preserved registers are saved, the call clobbered
   registers should have been saved by the code calling init and fini.

   See crtstuff.c for an example of code that inserts itself in the init
   and fini sections. 

   See crt0.s for the code that calls init and fini.  */

	.section	".init"
	.align 2
	.global	_init
_init:
	addi	sp, sp, -48
	stw	ra, 44(sp)
	stw	r23, 40(sp)
	stw	r22, 36(sp)
	stw	r21, 32(sp)
	stw	r20, 28(sp)
	stw	r19, 24(sp)
	stw	r18, 20(sp)
	stw	r17, 16(sp)
	stw	r16, 12(sp)
	stw	fp, 8(sp)
	addi	fp, sp, 8
#ifdef linux
	nextpc	r22
1:	movhi	r2, %hiadj(_gp_got - 1b)
	addi	r2, r2, %lo(_gp_got - 1b)
	add	r22, r22, r2
#endif
	
	
	.section	".fini"
	.align	2
	.global	_fini
_fini:
	addi	sp, sp, -48
	stw	ra, 44(sp)
	stw	r23, 40(sp)
	stw	r22, 36(sp)
	stw	r21, 32(sp)
	stw	r20, 28(sp)
	stw	r19, 24(sp)
	stw	r18, 20(sp)
	stw	r17, 16(sp)
	stw	r16, 12(sp)
	stw	fp, 8(sp)
	addi	fp, sp, 8
#ifdef linux
	nextpc	r22
1:	movhi	r2, %hiadj(_gp_got - 1b)
	addi	r2, r2, %lo(_gp_got - 1b)
	add	r22, r22, r2
#endif

