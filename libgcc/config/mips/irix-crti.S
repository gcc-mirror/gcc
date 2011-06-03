/* Copyright (C) 2004, 2008, 2011 Free Software Foundation, Inc.

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

	.abicalls
	.set	noreorder
	.set	nomacro

/* The GNU and SGI linkers differ in their implementation of -init and -fini.
   With the GNU linker, there can only be a single -init option, and the
   linker simply sets DT_INIT to that value.  gcc's initialization and
   finalization code can go directly in .init, with the prologue and
   epilogue of the main initialization routine being provided by external
   object files (*crti.o and *crtn.o in this case).

   The SGI linker instead accepts several -init options.  It will set DT_INIT
   to a linker-created function (placed in .init) that calls each of the -init
   functions in turn.  If there is any user code in .init, this linker-created
   function will be placed after it.  Note that such user code is not treated
   specially; it will only be called if the -init options arrange for it to
   be called.

   In theory, the SGI model should allow the crti, crtn and intermediate code
   to go in .init, just like it can with the GNU linker.  However, doing this
   seems to confuse the linker and triggers an internal error:

      ld32: FATAL   2  : Internal: at ../../ld/mips_code.c mips_code_fixup()
	 text section overflow!

   (seen with MIPSpro 7.30).  We therefore put everything in a special
   .gcc_init section instead.  */

	.section .gcc_init,"ax",@progbits
	.globl	__gcc_init
__gcc_init:
	daddiu	$sp,$sp,-16
	sd	$31,0($sp)
	sd	$28,8($sp)

	.section .gcc_fini,"ax",@progbits
	.globl	__gcc_fini
__gcc_fini:
	daddiu	$sp,$sp,-16
	sd	$31,0($sp)
	sd	$28,8($sp)

/* This object will typically be included in the final link for both
   shared libraries and executable, and we need to hide the symbols to
   prevent possible symbol preemption warnings from the SGI linker.  */
.hidden __gcc_init
.hidden __gcc_fini

