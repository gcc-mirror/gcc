# crtn.s for sysv4

# Copyright (C) 1996-2023 Free Software Foundation, Inc.
#   Written By Michael Meissner
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

# This file just supplies labeled ending points for the .got* and other
# special sections.  It is linked in last after other modules.
 
	.ident	"GNU C scrtn.s"

#ifndef __powerpc64__
# Default versions of exception handling register/deregister
	.weak	_ex_register
	.weak	_ex_deregister
	.set	_ex_register,0
	.set	_ex_deregister,0

# End list of C++ constructors
	.section ".ctors","aw"
	.globl	__CTOR_END__
	.type	__CTOR_END__,@object
__CTOR_END__:

# End list of C++ destructors
	.section ".dtors","aw"
	.weak	__DTOR_END__
	.type	__DTOR_END__,@object
__DTOR_END__:

	.section ".text"
	.globl	_ex_text1
_ex_text1:

	.section ".exception_ranges","aw"
	.globl	_ex_range1
_ex_range1:

# Tail of _init used for static constructors
	.section ".init","ax"
	lwz	%r0,16(%r1)
	lwz	%r31,12(%r1)
	mtlr	%r0
	addi	%r1,%r1,16
	blr

# Tail of _fini used for static destructors
	.section ".fini","ax"
	lwz	%r0,16(%r1)
	lwz	%r31,12(%r1)
	mtlr	%r0
	addi	%r1,%r1,16
	blr
#endif
