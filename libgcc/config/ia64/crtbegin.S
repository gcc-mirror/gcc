/* Copyright (C) 2000-2013 Free Software Foundation, Inc.
   Contributed by Jes Sorensen, <Jes.Sorensen@cern.ch>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "auto-host.h"

.section .ctors,"aw","progbits"
	.align	8
__CTOR_LIST__:
	data8	-1

.section .dtors,"aw","progbits"
	.align	8
__DTOR_LIST__:
	data8	-1

.section .jcr,"aw","progbits"
	.align	8
__JCR_LIST__:

.section .sdata
	.type dtor_ptr,@object
	.size dtor_ptr,8
dtor_ptr:
	data8	@gprel(__DTOR_LIST__ + 8)

	/* A handle for __cxa_finalize to manage c++ local destructors.  */
	.global __dso_handle
	.type __dso_handle,@object
	.size __dso_handle,8
#ifdef SHARED
	.section .data
__dso_handle:
	data8	__dso_handle
#else
	.section .bss
	.align 8
__dso_handle:
	.skip	8
#endif
	.hidden __dso_handle


#ifdef HAVE_INITFINI_ARRAY_SUPPORT

.section .fini_array, "a"
	data8 @fptr(__do_global_dtors_aux)

.section .init_array, "a"
	data8 @fptr(__do_jv_register_classes)
	data8 @fptr(__do_global_ctors_aux)

#else /* !HAVE_INITFINI_ARRAY_SUPPORT */
/*
 * Fragment of the ELF _fini routine that invokes our dtor cleanup.
 *
 * We make the call by indirection, because in large programs the 
 * .fini and .init sections are not in range of the destination, and
 * we cannot allow the linker to insert a stub at the end of this
 * fragment of the _fini function.  Further, Itanium does not implement
 * the long branch instructions, and we do not wish every program to
 * trap to the kernel for emulation.
 *
 * Note that we require __do_global_dtors_aux to preserve the GP,
 * so that the next fragment in .fini gets the right value.
 */
.section .fini,"ax","progbits"
	{ .mlx
	  movl r2 = @pcrel(__do_global_dtors_aux - 16)
	}
	{ .mii
	  mov r3 = ip
	  ;;
	  add r2 = r2, r3
	  ;;
	}
	{ .mib
	  nop 0
	  mov b6 = r2
	  br.call.sptk.many b0 = b6
	}

/* Likewise for _init.  */

.section .init,"ax","progbits"
	{ .mlx
	  movl r2 = @pcrel(__do_jv_register_classes - 16)
	}
	{ .mii
	  mov r3 = ip
	  ;;
	  add r2 = r2, r3
	  ;;
	}
	{ .mib
	  nop 0
	  mov b6 = r2
	  br.call.sptk.many b0 = b6
	}
#endif /* !HAVE_INITFINI_ARRAY_SUPPORT */

.section .text
	.align	32
	.proc	__do_global_dtors_aux
__do_global_dtors_aux:
	.prologue
#ifndef SHARED
	.save ar.pfs, r35
	alloc loc3 = ar.pfs, 0, 4, 1, 0
	addl loc0 = @gprel(dtor_ptr), gp
	.save rp, loc1
	mov loc1 = rp
	.body

	mov loc2 = gp
	nop 0
	br.sptk.many .entry
#else
	/*
		if (__cxa_finalize)
		  __cxa_finalize(__dso_handle)
	*/
	.save ar.pfs, r35
	alloc loc3 = ar.pfs, 0, 4, 1, 0
	addl loc0 = @gprel(dtor_ptr), gp
	addl r16 = @ltoff(@fptr(__cxa_finalize)), gp
	;;

	ld8 r16 = [r16]
	;;
	addl out0 = @ltoff(__dso_handle), gp
	cmp.ne p7, p0 = r0, r16
	;;

	ld8 out0 = [out0]
(p7)	ld8 r18 = [r16], 8
	.save rp, loc1
	mov loc1 = rp
	.body
	;;

	mov loc2 = gp
(p7)	ld8 gp = [r16]
(p7)	mov b6 = r18

	nop 0
	nop 0
(p7)	br.call.sptk.many rp = b6
	;;

	nop 0
	nop 0
	br.sptk.many .entry
#endif
	/*
		do {
		  dtor_ptr++;
		  (*(dtor_ptr-1)) ();
		} while (dtor_ptr);
	*/
.loop:
	st8 [loc0] = r15		// update dtor_ptr (in memory)
	ld8 r17 = [r16], 8		// r17 <- dtor's entry-point
	nop 0
	;;

	ld8 gp = [r16]			// gp <- dtor's gp
	mov b6 = r17
	br.call.sptk.many rp = b6

.entry:	ld8 r15 = [loc0]		// r15 <- dtor_ptr (gp-relative)
	;;
	add r16 = r15, loc2		// r16 <- dtor_ptr (absolute)
	adds r15 = 8, r15
	;;

	ld8 r16 = [r16]			// r16 <- pointer to dtor's fdesc
	mov rp = loc1
	mov ar.pfs = loc3
	;;

	cmp.ne p6, p0 = r0, r16
(p6)	br.cond.sptk.few .loop
	br.ret.sptk.many rp
	.endp __do_global_dtors_aux

	.align	32
	.proc	__do_jv_register_classes
__do_jv_register_classes:
	.prologue
	.save ar.pfs, r33
	alloc loc1 = ar.pfs, 0, 3, 1, 0
	movl out0 = @gprel(__JCR_LIST__)
	;;

	addl r14 = @ltoff(@fptr(_Jv_RegisterClasses)), gp
	add out0 = out0, gp
	.save rp, loc0
	mov loc0 = rp
	.body
	;;

	ld8 r14 = [r14]
	ld8 r15 = [out0]
	cmp.ne p6, p0 = r0, r0
	;;

	cmp.eq.or p6, p0 = r0, r14
	cmp.eq.or p6, p0 = r0, r15
(p6)	br.ret.sptk.many rp

	ld8 r15 = [r14], 8
	;;
	nop 0
	mov b6 = r15

	mov loc2 = gp
	ld8 gp = [r14]
	br.call.sptk.many rp = b6
	;;

	mov gp = loc2
	mov rp = loc0
	mov ar.pfs = loc1

	nop 0
	nop 0
	br.ret.sptk.many rp
	.endp	__do_jv_register_classes

#ifdef SHARED
.weak __cxa_finalize
#endif
.weak _Jv_RegisterClasses
