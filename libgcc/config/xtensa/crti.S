# Start .init and .fini sections.
# Copyright (C) 2003-2019 Free Software Foundation, Inc.
# 
# This file is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
# 
# GCC is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# Under Section 7 of GPL version 3, you are granted additional
# permissions described in the GCC Runtime Library Exception, version
# 3.1, as published by the Free Software Foundation.
#
# You should have received a copy of the GNU General Public License and
# a copy of the GCC Runtime Library Exception along with this program;
# see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
# <http://www.gnu.org/licenses/>.

# This file just makes a stack frame for the contents of the .fini and
# .init sections.  Users may put any desired instructions in those
# sections.

#include "xtensa-config.h"

	.section .init
	.globl _init
	.type _init,@function
	.align	4
_init:
#if XCHAL_HAVE_WINDOWED && !__XTENSA_CALL0_ABI__
	entry	sp, 64
#else
	addi	sp, sp, -32
	s32i	a0, sp, 0
#endif

	.section .fini
	.globl _fini
	.type _fini,@function
	.align	4
_fini:
#if XCHAL_HAVE_WINDOWED && !__XTENSA_CALL0_ABI__
	entry	sp, 64
#else
	addi	sp, sp, -32
	s32i	a0, sp, 0
#endif
