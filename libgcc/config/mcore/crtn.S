# crtn.S for ELF based systems

# Copyright (C) 1992-2020 Free Software Foundation, Inc.
# Written By David Vinayak Henkel-Wallace, June 1992
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

	.section	".init"
	.align		4

        ldw     r15,(r0, 12)
        addi    r0,16
        jmp     r15
	
	.section	".fini"
	.align		4

        ldw     r15, (r0, 12)
        addi    r0,16
        jmp     r15

# Th-th-th-that is all folks!

