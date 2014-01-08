# crti.s for ELF

# Copyright (C) 1992-2014 Free Software Foundation, Inc.
#   Written By David Vinayak Henkel-Wallace, June 1992
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
	.global	_init
	.type	_init,#function
	.align	4
_init:
	st	rp, @-r15
	enter	#4

	# These nops are here to align the end of this code with a 16 byte
	# boundary.  The linker will start inserting code into the .init
	# section at such a boundary.
	
	nop
	nop
	nop
	nop
	nop
	nop

	
	.section	".fini"
	.global	_fini
	.type	_fini,#function
	.align	4
_fini:
	st	rp, @-r15
	enter	#4
	nop
	nop
	nop
	nop
	nop
	nop
