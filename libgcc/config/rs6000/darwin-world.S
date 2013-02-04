/*  This file contains the exception-handling save_world and
 *  restore_world routines, which need to do a run-time check to see if
 *  they should save and restore the vector registers.
 *
 *   Copyright (C) 2004-2013 Free Software Foundation, Inc.
 * 
 * This file is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * 
 * This file is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * Under Section 7 of GPL version 3, you are granted additional
 * permissions described in the GCC Runtime Library Exception, version
 * 3.1, as published by the Free Software Foundation.
 *
 * You should have received a copy of the GNU General Public License and
 * a copy of the GCC Runtime Library Exception along with this program;
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
 * <http://www.gnu.org/licenses/>.
 */ 

#ifndef __ppc64__

	.machine ppc7400
.data
	.align 2

#ifdef __DYNAMIC__

.non_lazy_symbol_pointer
L_has_vec$non_lazy_ptr:
	.indirect_symbol __cpu_has_altivec
	.long	0
#else

/* For static, "pretend" we have a non-lazy-pointer.  */

L_has_vec$non_lazy_ptr:
	.long __cpu_has_altivec

#endif


.text
	.align 2

/* save_world and rest_world save/restore F14-F31 and possibly V20-V31
   (assuming you have a CPU with vector registers; we use a global var
   provided by the System Framework to determine this.)

   SAVE_WORLD takes R0 (the caller`s caller`s return address) and R11
   (the stack frame size) as parameters.  It returns the updated VRsave
   in R0 if we`re on a CPU with vector regs.

   For gcc3 onward, we need to save and restore CR as well, since scheduled
   prologs can cause comparisons to be moved before calls to save_world.

   USES: R0 R11 R12  */

.private_extern save_world
save_world:
	stw r0,8(r1)
	mflr r0
	bcl 20,31,Ls$pb
Ls$pb:	mflr r12
	addis r12,r12,ha16(L_has_vec$non_lazy_ptr-Ls$pb)
	lwz r12,lo16(L_has_vec$non_lazy_ptr-Ls$pb)(r12)
	mtlr r0
	lwz r12,0(r12)
				/* grab CR  */
	mfcr r0	
				/* test HAS_VEC  */
	cmpwi r12,0
	stfd f14,-144(r1)
	stfd f15,-136(r1)
	stfd f16,-128(r1)
	stfd f17,-120(r1)
	stfd f18,-112(r1)
	stfd f19,-104(r1)
	stfd f20,-96(r1)
	stfd f21,-88(r1)
	stfd f22,-80(r1)
	stfd f23,-72(r1)
	stfd f24,-64(r1)
	stfd f25,-56(r1)
	stfd f26,-48(r1)
	stfd f27,-40(r1)
	stfd f28,-32(r1)
	stfd f29,-24(r1)
	stfd f30,-16(r1)
	stfd f31,-8(r1)
	stmw r13,-220(r1)
				/* stash CR  */
	stw r0,4(r1)
				/* set R12 pointing at Vector Reg save area  */
	addi r12,r1,-224
				/* allocate stack frame  */
	stwux r1,r1,r11
				/* ...but return if HAS_VEC is zero   */
	bne+ L$saveVMX
				/* Not forgetting to restore CR.  */
	mtcr r0
	blr

L$saveVMX:
				/* We're saving Vector regs too.  */
				/* Restore CR from R0.  No More Branches!  */
	mtcr r0

	/* We should really use VRSAVE to figure out which vector regs
	   we actually need to save and restore.  Some other time :-/  */

	li r11,-192
	stvx v20,r11,r12
	li r11,-176
	stvx v21,r11,r12
	li r11,-160
	stvx v22,r11,r12
	li r11,-144
	stvx v23,r11,r12
	li r11,-128
	stvx v24,r11,r12
	li r11,-112
	stvx v25,r11,r12
	li r11,-96
	stvx v26,r11,r12
	li r11,-80
	stvx v27,r11,r12
	li r11,-64
	stvx v28,r11,r12
	li r11,-48
	stvx v29,r11,r12
	li r11,-32
	stvx v30,r11,r12
	mfspr r0,VRsave
	li r11,-16
	stvx v31,r11,r12	
	stw r0,0(r12)		/* VRsave lives at -224(R1).  */
	ori r0,r0,0xfff		/* We just saved these.  */
	mtspr VRsave,r0
	blr

/* rest_world  is jumped to, not called, so no need to worry about LR.
   clobbers R0, R7, R11 and R12.  This just undoes the work done above.  */

	.private_extern rest_world
rest_world:
				
	lwz r11, 0(r1)		/* Pickup previous SP  */
	li r7, 0		/* Stack offset is zero, r10 is ignored.  */
	b Lrest_world_eh_r7

/* eh_rest_world_r10 is jumped to, not called, so no need to worry about LR.
   R10 is the C++ EH stack adjust parameter, we return to the caller`s caller.

   clobbers: R0, R7, R11 and R12
   uses    : R10
   RETURNS : C++ EH Data registers (R3 - R6).  */

	.private_extern eh_rest_world_r10
eh_rest_world_r10:
				
	lwz r11, 0(r1)		/* Pickup previous SP  */
	mr  r7,r10		/* Stack offset.  */
	
	/* pickup the C++ EH data regs (R3 - R6.)  */
	lwz r6,-420(r11)
	lwz r5,-424(r11)
	lwz r4,-428(r11)
	lwz r3,-432(r11)

	/* Fall through to Lrest_world_eh_r7.  */

/* When we are doing the exception-handling epilog, R7 contains the offset to
   add to the SP.

   clobbers: R0, R11 and R12
   uses    : R7.  */

Lrest_world_eh_r7:
	/* See if we have Altivec.  */
	bcl 20,31,Lr7$pb
Lr7$pb: mflr r12

	addis r12,r12,ha16(L_has_vec$non_lazy_ptr-Lr7$pb)
	lwz r12,lo16(L_has_vec$non_lazy_ptr-Lr7$pb)(r12)
	lwz r12,0(r12)		/* R12 := HAS_VEC  */
	cmpwi r12,0
	lmw r13,-220(r11)
	beq L.rest_world_fp_eh

	/* We have Altivec, restore VRsave and V20..V31  */
	lwz r0,-224(r11)
	li r12,-416
	mtspr VRsave,r0
	lvx v20,r11,r12
	li r12,-400
	lvx v21,r11,r12
	li r12,-384
	lvx v22,r11,r12
	li r12,-368
	lvx v23,r11,r12
	li r12,-352
	lvx v24,r11,r12
	li r12,-336
	lvx v25,r11,r12
	li r12,-320
	lvx v26,r11,r12
	li r12,-304
	lvx v27,r11,r12
	li r12,-288
	lvx v28,r11,r12
	li r12,-272
	lvx v29,r11,r12
	li r12,-256
	lvx v30,r11,r12
	li r12,-240
	lvx v31,r11,r12

L.rest_world_fp_eh:
	lwz r0,4(r11)		/* recover saved CR  */
	lfd f14,-144(r11)
	lfd f15,-136(r11)
	lfd f16,-128(r11)
	lfd f17,-120(r11)
	lfd f18,-112(r11)
	lfd f19,-104(r11)
	lfd f20,-96(r11)
	lfd f21,-88(r11)
	lfd f22,-80(r11)
	lfd f23,-72(r11)
	lfd f24,-64(r11)
	lfd f25,-56(r11)
	lfd f26,-48(r11)
	lfd f27,-40(r11)
	lfd f28,-32(r11)
	lfd f29,-24(r11)
	lfd f30,-16(r11)
	mtcr r0			/* restore the saved cr.  */
	lwz r0, 8(r11)		/* Pick up the 'real' return address.  */
	lfd f31,-8(r11)		
	mtctr r0		/* exception-handler ret. address  */
	add r1,r11,r7		/* set SP to original value + R7 offset  */
	bctr
#endif
/* we should never be called on ppc64 for this ... */
/* Done.  */
