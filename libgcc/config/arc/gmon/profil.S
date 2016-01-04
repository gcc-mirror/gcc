/* This file contains code to do profiling.

   Copyright (C) 2007-2016 Free Software Foundation, Inc.
   Contributor: Joern Rennecke <joern.rennecke@embecosm.com>
		on behalf of Synopsys Inc.


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

#include "../asm.h"
#include "auxreg.h"
/*  This file contains code to do profiling.  */
	.weak	__profile_timer_cycles
	.global __profile_timer_cycles
	.set	__profile_timer_cycles, 200

        .section .bss
        .global __profil_offset
        .align 4
        .type	__profil_offset, @object
        .size	__profil_offset, 4
__profil_offset:
        .zero   4

	.text
	.global	__dcache_linesz
	.global __profil
	FUNC(__profil)
#if !defined (__EM__) && !defined (__HS__)
.Lstop_profiling:
	sr	r0,[CONTROL0]
	j_s	[blink]
	.balign	4
__profil:
.Lprofil:
	breq_s	r0,0,.Lstop_profiling
	; r0: buf r1: bufsiz r2: offset r3: scale
	bxor.f	r3,r3,15; scale must be 0x8000, i.e. 1/2; generate 0.
	push_s	blink
	lsr_s	r2,r2,1
	mov_s	r8,r0
	flag.ne	1	; halt if wrong scale
	sub_s	r0,r0,r2
	st	r0,[__profil_offset]
	bl	__dcache_linesz
	pop_s	blink
	bbit1.d	r0,0,nocache
	mov_s	r0,r8
#ifdef __ARC700__
	add_s	r1,r1,31
	lsr.f	lp_count,r1,5
	lpne	2f
	sr	r0,[DC_FLDL]
	add_s	r0,r0,32
#else /* !__ARC700__ */
# FIX ME: set up loop according to cache line size
	lr	r12,[D_CACHE_BUILD]
	sub_s	r0,r0,16
	sub_s	r1,r1,1
	lsr_s	r12,r12,16
	asr_s	r1,r1,4
	bmsk_s	r12,r12,3
	asr_s	r1,r1,r12
	add.f	lp_count,r1,1
	mov_s	r1,16
	asl_s	r1,r1,r12
	lpne	2f
	add	r0,r0,r1
	sr	r0,[DC_FLDL]
#endif /* __ARC700__ */
2:	b_s	.Lcounters_cleared
nocache:
.Lcounters_cleared:
	lr	r1,[INT_VECTOR_BASE] ; disable timer0 interrupts
	sr	r3,[CONTROL0]
	sr	r3,[COUNT0]
0:	ld_s	r0,[pcl,1f-0b+((0b-.Lprofil) & 2)] ; 1f@GOTOFF
0:	ld_s	r12,[pcl,1f+4-0b+((0b-.Lprofil) & 2)] ; 1f@GOTOFF + 4
	st_s	r0,[r1,24]; timer0 uses vector3
	st_s	r12,[r1,24+4]; timer0 uses vector3
	;sr	10000,[LIMIT0]
	sr	__profile_timer_cycles,[LIMIT0]
	mov_s	r12,3	; enable timer interrupts; count only when not halted.
	sr	r12,[CONTROL0]
	lr	r12,[STATUS32]
	bset_s	r12,r12,1 ; allow level 1 interrupts
	flag	r12
	mov_s	r0,0
	j_s	[blink]
	.balign	4
1:	j	__profil_irq
#else
__profil:
	.balign	4
	mov_s	r0,-1
	j_s	[blink]
#endif /* !__EM__ && !__HS__ */
	ENDFUNC(__profil)

	FUNC(__profil_irq)
	.balign 4	; make final jump unaligned to avoid delay penalty
	.balign 32,0,12	; make sure the code spans no more that two cache lines
	nop_s
__profil_irq:
#if !defined (__EM__) && !defined (__HS__)
	push_s	r0
	ld	r0,[__profil_offset]
	push_s	r1
	lsr	r1,ilink1,2
	push_s	r2
	ldw.as.di r2,[r0,r1]
	add1	r0,r0,r1
	ld_s	r1,[sp,4]
	add_s	r2,r2,1
	bbit1	r2,16,nostore
	stw.di	r2,[r0]
nostore:ld.ab	r2,[sp,8]
	pop_s	r0
	j.f	[ilink1]
#else
	rtie
#endif /* !__EM__  && !__HS__ */
	ENDFUNC(__profil_irq)

; could save one cycle if the counters were allocated at link time and
; the contents of __profil_offset were pre-computed at link time, like this:
#if 0
; __profil_offset needs to be PROVIDEd as __profile_base-text/4
	.global	__profil_offset
	.balign 4
__profil_irq:
	push_s	r0
	lsr	r0,ilink1,2
	add1	r0,__profil_offset,r0
	push_s	r1
	ldw.di	r1,[r0]


	add_s	r1,r1,1
	bbit1	r1,16,nostore
	stw.di	r1,[r0]
nostore:pop_s	r1
	pop_s	r0
	j	[ilink1]
#endif /* 0 */
