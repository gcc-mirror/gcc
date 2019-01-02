/* Copyright (C) 2008-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

	.data
	.p2align 7
	.global __cache
__cache:
	.rept __CACHE_SIZE__ * 8
	.fill 128
	.endr

	.p2align 7
	.global __cache_tag_array
__cache_tag_array:
	.rept __CACHE_SIZE__ * 2
	.long 1, 1, 1, 1
	.fill 128-16
	.endr
__end_cache_tag_array:

	.globl __cache_tag_array_size
	.set __cache_tag_array_size, __end_cache_tag_array-__cache_tag_array

