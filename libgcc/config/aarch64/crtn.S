# Machine description for AArch64 architecture.
# Copyright (C) 2009-2017 Free Software Foundation, Inc.
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

/* An executable stack is *not* required for these functions.  */
#if defined(__ELF__) && defined(__linux__)
.section .note.GNU-stack,"",%progbits
.previous
#endif

# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.

	# Note - this macro is complemented by the FUNC_START macro
	# in crti.S.  If you change this macro you must also change
	# that macro match.
	#
	# Note - we do not try any fancy optimizations of the return
	# sequences here, it is just not worth it.  Instead keep things
	# simple.  Restore all the save resgisters, including the link
	# register and then perform the correct function return instruction.
.macro FUNC_END
	ldp	x19, x20, [sp], #16
	ldp	x21, x22, [sp], #16
	ldp	x23, x24, [sp], #16
	ldp	x25, x26, [sp], #16
	ldp	x27, x28, [sp], #16
	ldp	x29, x30, [sp], #16
	ret
.endm


	.section	".init"
	;;
	FUNC_END

	.section	".fini"
	;;
	FUNC_END

# end of crtn.S
