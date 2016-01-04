/* c-isr library stuff of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2016 Free Software Foundation, Inc.
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

.macro ADJ_INTR_LVL
#if defined(NDS32_NESTED) /* Nested handler.  */
	mfsr	$r3, $PSW
	addi	$r3, $r3, #-0x1
	mtsr	$r3, $PSW
#elif defined(NDS32_NESTED_READY) /* Nested ready handler.  */
	/* Save ipc and ipsw and lower INT level.  */
	mfsr	$r3, $PSW
	addi	$r3, $r3, #-0x2
	mtsr	$r3, $PSW
#else /* Not nested handler.  */
#endif
.endm
