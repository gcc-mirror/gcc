# Terminate C-SKY .init and .fini sections.
# Copyright (C) 2018 Free Software Foundation, Inc.
# Contributed by C-SKY Microsystems and Mentor Graphics.
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


# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.

	.file    "crtn.S"

# Is this the GLIBC version?
#if defined(__gnu_linux__)
	 .section .init,"ax",@progbits
	 ldw     lr, (sp, 0)
	 addi    sp, 8
	 rts

	 .section .fini,"ax",@progbits
	 ldw     lr, (sp, 0)
	 addi    sp, 8
	 rts

#else /* !defined(__gnu_linux__) */
	.section  ".init"
	ldw     lr, (sp, 12)
	addi    sp, 16
	jmp     lr

	.section  ".fini"
	ldw     lr, (sp, 12)
	addi    sp, 16
	jmp     lr

# Th-th-th-that is all folks!
#endif  /* defined(__gnu_linux__) */
