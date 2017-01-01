###################################-*-asm*- 
# 
#    Copyright (C) 2009-2017 Free Software Foundation, Inc.
# 
#
#  Contributed by Michael Eager <eager@eagercon.com>.
#
#  This file is free software; you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation; either version 3, or (at your option) any
#  later version.
#
#  GCC is distributed in the hope that it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
#  License for more details.
#
#  Under Section 7 of GPL version 3, you are granted additional
#  permissions described in the GCC Runtime Library Exception, version
#  3.1, as published by the Free Software Foundation.
#
#  You should have received a copy of the GNU General Public License and
#  a copy of the GCC Runtime Library Exception along with this program;
#  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
#  <http://www.gnu.org/licenses/>.  
# 
#  stack_overflow_exit.S
# 
#  Checks for stack overflows and sets the global variable 
#  stack_overflow_error with the value of current stack pointer
#
#  This routine exits from the program
# 
#######################################

	.globl	_stack_overflow_error
	.data
	.align	2
	.type	_stack_overflow_error,@object
	.size	_stack_overflow_error,4
_stack_overflow_error:
	.data32	0

	.text 
	.globl	_stack_overflow_exit	
	.ent	_stack_overflow_exit
	.type	_stack_overflow_exit,@function

_stack_overflow_exit:
#ifdef __PIC__
	mfs	r20,rpc
	addik	r20,r20,_GLOBAL_OFFSET_TABLE_+8
	swi	r1,r20,_stack_overflow_error@GOTOFF
	bri	exit@PLT
#else
	swi	r1,r0,_stack_overflow_error
	bri	exit
#endif

	.end 	_stack_overflow_exit
	.size	_stack_overflow_exit,. - _stack_overflow_exit
