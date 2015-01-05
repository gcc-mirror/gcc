/* c-isr library stuff of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2015 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

.macro SAVE_FPU_REGS_02
	addi	$sp, $sp, #-8
	fsdi.bi	$fd15, [$sp], #-8
	fsdi.bi	$fd14, [$sp], #-8
	fsdi.bi	$fd13, [$sp], #-8
	fsdi.bi	$fd12, [$sp], #-8
	fsdi.bi	$fd11, [$sp], #-8
	fsdi.bi	$fd10, [$sp], #-8
	fsdi.bi	$fd9, [$sp], #-8
	fsdi.bi	$fd8, [$sp], #-8
	fsdi.bi	$fd7, [$sp], #-8
	fsdi.bi	$fd6, [$sp], #-8
	fsdi.bi	$fd5, [$sp], #-8
	fsdi.bi	$fd4, [$sp], #-8
	fsdi.bi	$fd3, [$sp], #-8
	fsdi.bi	$fd2, [$sp], #-8
	fsdi.bi	$fd1, [$sp], #-8
	fsdi.bi	$fd0, [$sp], #-8
	addi	$sp, $sp, #8
.endm
