/*  This file contains the floating-point save and restore routines.
 *
 *   Copyright (C) 2004-2021 Free Software Foundation, Inc.
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

/* THE SAVE AND RESTORE ROUTINES CAN HAVE ONLY ONE GLOBALLY VISIBLE
   ENTRY POINT - callers have to jump to "saveFP+60" to save f29..f31,
   for example.  For FP reg saves/restores, it takes one instruction
   (4 bytes) to do the operation; for Vector regs, 2 instructions are
   required (8 bytes.)

   MORAL: DO NOT MESS AROUND WITH THESE FUNCTIONS!  */

#include "darwin-asm.h"

.text
	.align 2

/* saveFP saves R0 -- assumed to be the callers LR -- to 8/16(R1).  */

.private_extern saveFP
saveFP:
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
	stg  r0,SAVED_LR_OFFSET(r1)
	blr

/* restFP restores the caller`s LR from 8/16(R1).  Note that the code for
   this starts at the offset of F30 restoration, so calling this
   routine in an attempt to restore only F31 WILL NOT WORK (it would
   be a stupid thing to do, anyway.)  */

.private_extern restFP
restFP:
	lfd f14,-144(r1)
	lfd f15,-136(r1)
	lfd f16,-128(r1)
	lfd f17,-120(r1)
	lfd f18,-112(r1)
	lfd f19,-104(r1)
	lfd f20,-96(r1)
	lfd f21,-88(r1)
	lfd f22,-80(r1)
	lfd f23,-72(r1)
	lfd f24,-64(r1)
	lfd f25,-56(r1)
	lfd f26,-48(r1)
	lfd f27,-40(r1)
	lfd f28,-32(r1)
	lfd f29,-24(r1)
			/* <OFFSET OF F30 RESTORE> restore callers LR  */
	lg r0,SAVED_LR_OFFSET(r1)
	lfd f30,-16(r1)
			/* and prepare for return to caller  */
	mtlr r0	
	lfd f31,-8(r1)
	blr
