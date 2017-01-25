/* crti.s for eabi
   Copyright (C) 1996-2017 Free Software Foundation, Inc.
   Written By Michael Meissner

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

/* This file just supplies labeled starting points for the .got* and other
   special sections.  It is linked in first before other modules.  */
 
	.ident	"GNU C crti.s"

#include <ppc-asm.h>

#ifndef __powerpc64__
	.section ".got","aw"
	.globl	__GOT_START__
	.type	__GOT_START__,@object
__GOT_START__:

	.section ".got1","aw"
	.globl	__GOT1_START__
	.type	__GOT1_START__,@object
__GOT1_START__:

	.section ".got2","aw"
	.globl	__GOT2_START__
	.type	__GOT2_START__,@object
__GOT2_START__:

	.section ".fixup","aw"
	.globl	__FIXUP_START__
	.type	__FIXUP_START__,@object
__FIXUP_START__:

	.section ".ctors","aw"
	.globl	__CTOR_LIST__
	.type	__CTOR_LIST__,@object
__CTOR_LIST__:

	.section ".dtors","aw"
	.globl	__DTOR_LIST__
	.type	__DTOR_LIST__,@object
__DTOR_LIST__:

	.section ".sdata","aw"
	.globl	__SDATA_START__
	.type	__SDATA_START__,@object
	.weak	_SDA_BASE_
	.type	_SDA_BASE_,@object
__SDATA_START__:
_SDA_BASE_:

	.section ".sbss","aw",@nobits
	.globl	__SBSS_START__
	.type	__SBSS_START__,@object
__SBSS_START__:

	.section ".sdata2","a"
	.weak	_SDA2_BASE_
	.type	_SDA2_BASE_,@object
	.globl	__SDATA2_START__
	.type	__SDATA2_START__,@object
__SDATA2_START__:
_SDA2_BASE_:

	.section ".sbss2","a"
	.globl	__SBSS2_START__
	.type	__SBSS2_START__,@object
__SBSS2_START__:

	.section ".gcc_except_table","aw"
	.globl	__EXCEPT_START__
	.type	__EXCEPT_START__,@object
__EXCEPT_START__:

	.section ".eh_frame","aw"
	.globl	__EH_FRAME_BEGIN__
	.type	__EH_FRAME_BEGIN__,@object
__EH_FRAME_BEGIN__:

/* Head of __init function used for static constructors.  */
	.section ".init","ax"
	.align 2
FUNC_START(__init)
	stwu 1,-16(1)
	mflr 0
	stw 0,20(1)

/* Head of __fini function used for static destructors.  */
	.section ".fini","ax"
	.align 2
FUNC_START(__fini)
	stwu 1,-16(1)
	mflr 0
	stw 0,20(1)
#endif
