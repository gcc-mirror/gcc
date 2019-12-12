/* libgcc routines for RL78
   Copyright (C) 2011-2019 Free Software Foundation, Inc.
   Contributed by Red Hat.

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

/* RL78 Trampoline support

  Since the RL78's RAM is not in the first 64k, we cannot "just" use a
  function pointer to point to a trampoline on the stack.  So, we
  create N fixed trampolines that read from an array, and allocate
  them as needed.

*/

#include "vregs.h"

	.data
	.p2align	1
trampoline_array:

	.macro stub n

	.text
trampoline_\n:
        .type   trampoline_\n, @function
	movw	ax, !trampoline_chain_\n
	movw	r14, ax
	movw	ax, !trampoline_addr_\n
	br	ax
	.size	trampoline_\n, .-trampoline_\n

	.data
trampoline_frame_\n:
	.short	0
trampoline_stub_\n:
	.short	trampoline_\n
trampoline_chain_\n:
	.short	0
trampoline_addr_\n:
	.short	0

#define TO_FRAME 0
#define TO_STUB  2
#define TO_CHAIN 4
#define TO_ADDR  6
#define TO_SIZE  8

	.endm

	stub	0
	stub	1
	stub	2
	stub	3
	stub	4
	stub	5

trampoline_array_end:

/* Given the function pointer in R8 and the static chain
   pointer in R10, allocate a trampoline and return its address in
   R8. */

START_FUNC ___trampoline_init
	movw	hl, #trampoline_array

1:	movw	ax, [hl + TO_ADDR]
	cmpw	ax, #0
	bz	$2f

	movw	ax, hl
	addw	ax, #TO_SIZE
	movw	hl, ax
	cmpw	ax, #trampoline_array_end
	bnz	$1b
	brk			; no more slots?

2:	movw	ax, r8
	movw	[hl + TO_ADDR], ax
	movw	ax, r10
	movw	[hl + TO_CHAIN], ax
	movw	ax, sp
	movw	[hl + TO_FRAME], ax

	movw	ax, [hl + TO_STUB]
	movw	r8, ax
	ret
END_FUNC ___trampoline_init


START_FUNC ___trampoline_uninit
	movw	hl, #trampoline_array
	movw	ax, sp
	movw	bc, ax

1:	movw	ax, [hl + TO_FRAME]
	cmpw	ax, bc
	bc	$2f

	clrw	ax
	movw	[hl + TO_ADDR], ax

2:	movw	ax, hl
	addw	ax, #TO_SIZE
	movw	hl, ax
	cmpw	ax, #trampoline_array_end
	bnz	$1b

	ret
END_FUNC ___trampoline_uninit
