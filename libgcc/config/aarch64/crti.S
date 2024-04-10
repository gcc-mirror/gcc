# Machine description for AArch64 architecture.
# Copyright (C) 2009-2024 Free Software Foundation, Inc.
# Contributed by ARM Ltd.
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

#include "aarch64-asm.h"

# This file creates a stack frame for the contents of the .fini and
# .init sections.  Users may put any desired instructions in those
# sections.

	# Note - this macro is complemented by the FUNC_END macro
	# in crtn.S.  If you change this macro you must also change
	# that macro match.
.macro FUNC_START
	#  Create a stack frame and save any call-preserved registers
	stp	x29, x30, [sp, #-16]!
	stp	x27, x28, [sp, #-16]!
	stp	x25, x26, [sp, #-16]!
	stp	x23, x24, [sp, #-16]!
	stp	x21, x22, [sp, #-16]!
	stp	x19, x20, [sp, #-16]!
.endm

	.section	".init"
	.align 2
	.global	_init
	SYMBOL_TYPE(_init, function)
_init:
	FUNC_START


	.section	".fini"
	.align	2
	.global	_fini
	SYMBOL_TYPE(_fini, function)
_fini:
	FUNC_START

# end of crti.S
