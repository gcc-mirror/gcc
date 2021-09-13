# Copyright (C) 2000-2021 Free Software Foundation, Inc.
#   Written By Timothy Wall
#
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3, or (at your option) any
# later version.
#
# This file is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

# This file just make a stack frame for the contents of the .fini and
# .init sections.  Users may put any desired instructions in those
# sections.

	.section	".init"
	.align 16
	.global	_init
_init:
	.prologue 14, 33
	.save ar.pfs, r34
	alloc	r34 = ar.pfs, 0, 4, 0, 0
	.vframe r35
	mov	r35 = r12
	.save rp, r33
	mov	r33 = b0
	.body

	.section	".fini"
	.align	16
	.global	_fini
_fini:
	.prologue 14, 33
	.save ar.pfs, r34
	alloc	r34 = ar.pfs, 0, 4, 0, 0
	.vframe r35
	mov	r35 = r12
	.save rp, r33
	mov	r33 = b0
	.body

# end of crti.S
