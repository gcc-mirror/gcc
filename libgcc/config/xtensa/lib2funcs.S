/* Assembly functions for libgcc2.
   Copyright (C) 2001-2017 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

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

#include "xtensa-config.h"

/* __xtensa_libgcc_window_spill: This function flushes out all but the
   current register window.  This is used to set up the stack so that
   arbitrary frames can be accessed.  */

#if XCHAL_HAVE_WINDOWED && !__XTENSA_CALL0_ABI__
	.align	4
	.global	__xtensa_libgcc_window_spill
	.type	__xtensa_libgcc_window_spill,@function
__xtensa_libgcc_window_spill:
	entry	sp, 48
#if XCHAL_NUM_AREGS > 16
	call12	1f
	retw
	.align	4
1:
	.rept	(XCHAL_NUM_AREGS - 24) / 12
	_entry	sp, 48
	mov	a12, a0
	.endr
	_entry	sp, 16
#if XCHAL_NUM_AREGS % 12 == 0
	mov	a4, a4
#elif XCHAL_NUM_AREGS % 12 == 4
	mov	a8, a8
#elif XCHAL_NUM_AREGS % 12 == 8
	mov	a12, a12
#endif
	retw
#else
	mov	a8, a8
	retw
#endif
	.size	__xtensa_libgcc_window_spill, .-__xtensa_libgcc_window_spill
#endif


/* __xtensa_nonlocal_goto: This code does all the hard work of a
   nonlocal goto on Xtensa.  It is here in the library to avoid the
   code size bloat of generating it in-line.  There are two
   arguments:

	a2 = frame pointer for the procedure containing the label
	a3 = goto handler address

  This function never returns to its caller but instead goes directly
  to the address of the specified goto handler.  */

#if XCHAL_HAVE_WINDOWED && !__XTENSA_CALL0_ABI__
	.align	4
	.global	__xtensa_nonlocal_goto
	.type	__xtensa_nonlocal_goto,@function
__xtensa_nonlocal_goto:
	entry	sp, 32

	/* Flush registers.  */
	call8	__xtensa_libgcc_window_spill

	/* Because the save area for a0-a3 is stored one frame below
	   the one identified by a2, the only way to restore those
	   registers is to unwind the stack.  If alloca() were never
	   called, we could just unwind until finding the sp value
	   matching a2.  However, a2 is a frame pointer, not a stack
	   pointer, and may not be encountered during the unwinding.
	   The solution is to unwind until going _past_ the value
	   given by a2.  This involves keeping three stack pointer
	   values during the unwinding:

		next = sp of frame N-1
		cur = sp of frame N
		prev = sp of frame N+1

	   When next > a2, the desired save area is stored relative
	   to prev.  At this point, cur will be the same as a2
	   except in the alloca() case.

	   Besides finding the values to be restored to a0-a3, we also
	   need to find the current window size for the target
	   function.  This can be extracted from the high bits of the
	   return address, initially in a0.  As the unwinding
	   proceeds, the window size is taken from the value of a0
	   saved _two_ frames below the current frame.  */

	addi	a5, sp, -16	/* a5 = prev - save area */
	l32i	a6, a5, 4
	addi	a6, a6, -16	/* a6 = cur - save area */
	mov	a8, a0		/* a8 = return address (for window size) */
	j	.Lfirstframe

.Lnextframe:
	l32i	a8, a5, 0	/* next return address (for window size) */
	mov	a5, a6		/* advance prev */
	addi	a6, a7, -16	/* advance cur */
.Lfirstframe:
	l32i	a7, a6, 4	/* a7 = next */
	bgeu	a2, a7, .Lnextframe

	/* At this point, prev (a5) points to the save area with the saved
	   values of a0-a3.  Copy those values into the save area at the
	   current sp so they will be reloaded when the return from this
	   function underflows.  We don't have to worry about exceptions
	   while updating the current save area, because the windows have
	   already been flushed.  */

	addi	a4, sp, -16	/* a4 = save area of this function */
	l32i	a6, a5, 0
	l32i	a7, a5, 4
	s32i	a6, a4, 0
	s32i	a7, a4, 4
	l32i	a6, a5, 8
	l32i	a7, a5, 12
	s32i	a6, a4, 8
	s32i	a7, a4, 12

	/* Set return address to goto handler.  Use the window size bits
	   from the return address two frames below the target.  */
	extui	a8, a8, 30, 2	/* get window size from return addr. */
	slli	a3, a3, 2	/* get goto handler addr. << 2 */
	ssai	2
	src	a0, a8, a3	/* combine them with a funnel shift */

	retw
	.size	__xtensa_nonlocal_goto, .-__xtensa_nonlocal_goto
#endif


/* __xtensa_sync_caches: This function is called after writing a trampoline
   on the stack to force all the data writes to memory and invalidate the
   instruction cache. a2 is the address of the new trampoline.

   After the trampoline data is written out, it must be flushed out of
   the data cache into memory.  We use DHWB in case we have a writeback
   cache.  At least one DHWB instruction is needed for each data cache
   line which may be touched by the trampoline.  An ISYNC instruction
   must follow the DHWBs.

   We have to flush the i-cache to make sure that the new values get used.
   At least one IHI instruction is needed for each i-cache line which may
   be touched by the trampoline.  An ISYNC instruction is also needed to
   make sure that the modified instructions are loaded into the instruction
   fetch buffer.  */

/* Use the maximum trampoline size.  Flushing a bit extra is OK.  */
#define TRAMPOLINE_SIZE 60

	.text
	.align	4
	.global	__xtensa_sync_caches
	.type	__xtensa_sync_caches,@function
__xtensa_sync_caches:
#if XCHAL_HAVE_WINDOWED && !__XTENSA_CALL0_ABI__
	entry 	sp, 32
#endif
#if XCHAL_DCACHE_SIZE > 0
	/* Flush the trampoline from the data cache.  */
	extui	a4, a2, 0, XCHAL_DCACHE_LINEWIDTH
	addi	a4, a4, TRAMPOLINE_SIZE
	addi	a4, a4, (1 << XCHAL_DCACHE_LINEWIDTH) - 1
	srli	a4, a4, XCHAL_DCACHE_LINEWIDTH
	mov	a3, a2
.Ldcache_loop:
	dhwb	a3, 0
	addi	a3, a3, (1 << XCHAL_DCACHE_LINEWIDTH)
	addi	a4, a4, -1
	bnez	a4, .Ldcache_loop
	isync
#endif
#if XCHAL_ICACHE_SIZE > 0
	/* Invalidate the corresponding lines in the instruction cache.  */
	extui	a4, a2, 0, XCHAL_ICACHE_LINEWIDTH
	addi	a4, a4, TRAMPOLINE_SIZE
	addi	a4, a4, (1 << XCHAL_ICACHE_LINEWIDTH) - 1
	srli	a4, a4, XCHAL_ICACHE_LINEWIDTH
.Licache_loop:
	ihi	a2, 0
	addi	a2, a2, (1 << XCHAL_ICACHE_LINEWIDTH)
	addi	a4, a4, -1
	bnez	a4, .Licache_loop
#endif
	isync
#if XCHAL_HAVE_WINDOWED && !__XTENSA_CALL0_ABI__
	retw
#else
	ret
#endif
	.size	__xtensa_sync_caches, .-__xtensa_sync_caches
