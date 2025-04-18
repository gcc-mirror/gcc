/* Copyright (C) 2010-2025 Free Software Foundation, Inc.
   Contributed by Bernd Schmidt <bernds@codesourcery.com>.

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

/*
 * This file just supplies function prologues for the .init and .fini
 * sections.  It is linked in before crtbegin.o.
 */

	.section .init
	.globl  _init
	.type   _init,@function
_init:
	add	.l2	-8, B15, B15
	stw	.d2t2	B3,*+B15(4)
	.section .fini
	.globl  _fini
	.type   _fini,@function
_fini:
	add	.l2	-8, B15, B15
	stw	.d2t2	B3,*+B15(4)
