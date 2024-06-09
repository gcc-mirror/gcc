/* DWARF2 EH unwinding support for IA64 VMS.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.

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

#define __NEW_STARLET
#include <libicb.h>
#include <chfdef.h>
#include <lib_c/chfctxdef.h>
#include <lib_c/intstkdef.h>

#include <stdio.h>
#include <string.h>

#define UNW_IVMS_MODE(HEADER) (((HEADER) >> 44) & 0x3L)
#define MD_UNW_COMPATIBLE_PERSONALITY_P(HEADER) (!UNW_IVMS_MODE (HEADER))

#define DYN$C_SSENTRY 66
/* ??? would rather get the proper header file.  */

#define MD_FALLBACK_FRAME_STATE_FOR ia64_vms_fallback_frame_state

extern INVO_CONTEXT_BLK * LIB$I64_CREATE_INVO_CONTEXT (void);

extern int LIB$I64_IS_EXC_DISPATCH_FRAME (void *);
extern int LIB$I64_IS_AST_DISPATCH_FRAME (void *);

extern int LIB$I64_INIT_INVO_CONTEXT (INVO_CONTEXT_BLK *, int, int);
extern int LIB$I64_GET_CURR_INVO_CONTEXT (INVO_CONTEXT_BLK *);
extern int LIB$I64_GET_PREV_INVO_CONTEXT (INVO_CONTEXT_BLK *);

typedef unsigned int uint;
typedef unsigned __int64 uw_reg;
typedef uw_reg * uw_loc;

typedef char fp_reg[16];

#define DENOTES_VMS_DISPATCHER_FRAME(icb) \
(LIB$I64_IS_EXC_DISPATCH_FRAME (&(icb)->libicb$ih_pc))

#define DENOTES_BOTTOM_OF_STACK(icb) ((icb)->libicb$v_bottom_of_stack)

#define FAIL_IF(COND) \
   do { if (COND) { context->rp = 0; return _URC_END_OF_STACK; } } while (0)
/* Clearing context->rp is required to prevent the ia64 gcc unwinder from
   attempting to keep on walking the call chain.  */

static int
ia64_vms_fallback_frame_state (struct _Unwind_Context *context,
			       _Unwind_FrameState *fs)
{
  int i, status;

  INVO_CONTEXT_BLK local_icb;
  INVO_CONTEXT_BLK *icb = &local_icb;
    
  CHFCTX * chfctx;
  CHF$MECH_ARRAY * chfmech;
  CHF64$SIGNAL_ARRAY *chfsig64;
  INTSTK * intstk;

  static int eh_debug = -1;
  int try_bs_copy = 0;
  /* Non zero to attempt copy of alternate backing store contents for
     dirty partition in interrupted context. ??? Alpha code, only activated
     on specific request via specific bit in EH_DEBUG.  */

  if (eh_debug == -1)
    {
      char * EH_DEBUG = getenv ("EH_DEBUG");
      const uint try_bs_copy_mask = (1 << 16);

      eh_debug = EH_DEBUG ? atoi (EH_DEBUG) : 0;
      
      /* Fetch and clear the try_bs_copy bit.  */
      try_bs_copy = (uint)eh_debug & try_bs_copy_mask;
      eh_debug &= ~try_bs_copy_mask;
    }

  /* We're called to attempt unwinding through a frame for which no unwind
     info is available, typical of an operating system exception dispatcher
     frame.  The code below knows how to handle this case, and only this one,
     returning a failure code if it finds it is not in this situation.

     Note that we're called from deep down in the exception propagation call
     chain, possibly below an exception dispatcher but for a frame above it
     like some os entry point.  */

  if (eh_debug)
    printf ("FALLBACK - ctxt->rp=0x%lx, sp=0x%lx, psp=0x%lx, bsp=0x%lx\n",
	    context->rp, context->sp, context->psp, context->bsp);

  /* Step 0 :
     -------------------------------------------------------------------------
     VMS-unwind up until we reach a VMS dispatcher frame corresponding to the
     context we are trying to unwind through. Fail if get past this context or
     if we reach the bottom of stack along the way.
     -------------------------------------------------------------------------
  */

  status = LIB$I64_INIT_INVO_CONTEXT (icb, LIBICB$K_INVO_CONTEXT_VERSION, 0);
  FAIL_IF (status == 0);

  status = LIB$I64_GET_CURR_INVO_CONTEXT (icb);

  /* Beware: we might be unwinding through nested condition handlers, so the
     dispatcher frame we seek might not be the first one on the way up.  Loop
     thus.  */     
  do {
    
    /* Seek the next dispatcher frame up the "current" point.  Stop if we
       either get past the target context or hit the bottom-of-stack along
       the way.  */
    status = LIB$I64_GET_PREV_INVO_CONTEXT (icb);
    FAIL_IF (status == 0);
    FAIL_IF ((uw_reg)icb->libicb$ih_sp > (uw_reg)context->psp
	     || DENOTES_BOTTOM_OF_STACK (icb));
    
    if (eh_debug)
      printf ("frame%s sp @ 0x%llx, pc @ 0x%llx bsp=0x%llx\n",
	      DENOTES_VMS_DISPATCHER_FRAME (icb) ? " (dispatcher)" : "",
	      icb->libicb$ih_sp, icb->libicb$ih_pc, icb->libicb$ih_bsp);

    /* Continue until the target frame is found.  */
  } while ((uw_reg)icb->libicb$ih_bsp != (uw_reg)context->bsp);

  /* If this is not a dispatcher frame, this is certainly a frame for a leaf
     subprogram.  Use default unwind information.  */
  if (! DENOTES_VMS_DISPATCHER_FRAME (icb))
    return _URC_END_OF_STACK;

  /* At this point, we know we are really trying to unwind past an exception
     dispatcher frame, and have it described in ICB.  Proceed.  */

  /* Step 1 :
     ------------------------------------------------------------------------
     We have the VMS dispatcher frame ICB handy and know we are trying to
     unwind past it.  Fetch pointers to useful datastructures from there, then
     unwind one step further up to the interrupted user context from which
     some required values will be easily accessible.
     ------------------------------------------------------------------------
  */

  chfctx = icb->libicb$ph_chfctx_addr;
  FAIL_IF (chfctx == 0);
  
  chfmech = (CHF$MECH_ARRAY *)chfctx->chfctx$q_mcharglst;
  FAIL_IF (chfmech == 0);

  chfsig64 = (CHF64$SIGNAL_ARRAY *)chfmech->chf$ph_mch_sig64_addr;
  FAIL_IF (chfsig64 == 0);
 
  intstk = (INTSTK *)chfmech->chf$q_mch_esf_addr;
  FAIL_IF (intstk == 0 || intstk->intstk$b_subtype == DYN$C_SSENTRY);

  status = LIB$I64_GET_PREV_INVO_CONTEXT (icb);
  FAIL_IF (status == 0);

  if (eh_debug)
    printf ("User frame, "
	    "chfmech @ 0x%p, chfsig64 @ 0x%p, intstk @ 0x%p\n",
	    chfmech, chfsig64, intstk);

  /* Step 2 :
     ------------------------------------------------------------------------
     Point the GCC context locations/values required for further unwinding at
     their corresponding locations/values in the datastructures at hand.
     ------------------------------------------------------------------------
  */

  /* Static General Register locations, including scratch registers in case
     the unwinder needs to refer to a value stored in one of them.  */
  {
    uw_reg * ctxregs = (uw_reg *)&intstk->intstk$q_regbase;

    for (i = 2; i <= 3; i++)
      context->ireg[i - 2].loc = (uw_loc)&ctxregs[i];
    for (i = 8; i <= 11; i++)
      context->ireg[i - 2].loc = (uw_loc)&ctxregs[i];
    for (i = 14; i <= 31; i++)
      context->ireg[i - 2].loc = (uw_loc)&ctxregs[i];
  }

  /* Static Floating Point Register locations, as available from the
     mechargs array, which happens to include all the to be preserved
     ones + others.  */
  {
    fp_reg * ctxregs;

    ctxregs = (fp_reg *)&chfmech->chf$fh_mch_savf2;
    for (i = 2; i <= 5 ; i++)
      context->fr_loc[i - 2] = (uw_loc)&ctxregs[i - 2];

    ctxregs = (fp_reg *)&chfmech->chf$fh_mch_savf12;
    for (i = 12; i <= 31 ; i++)
      context->fr_loc[i - 2] = (uw_loc)&ctxregs[i - 12];
  }

  /* Relevant application register locations.  */

  context->fpsr_loc = (uw_loc)&intstk->intstk$q_fpsr;
  context->lc_loc   = (uw_loc)&intstk->intstk$q_lc;
  context->unat_loc = (uw_loc)&intstk->intstk$q_unat;

  /* Branch register locations.  */
  
  {
    uw_reg * ctxregs = (uw_reg *)&intstk->intstk$q_b0;

    for (i = 0; i < 8; i++)
      context->br_loc[i] = (uw_loc)&ctxregs[i];
  }

  /* Necessary register values.  */

  /* ??? Still unclear if we need to account for possible flushes to an
     alternate backing store (maybe the unwinding performed above did the
     trick already) and how this would be handled.  Blind alpha tentative
     below for experimentation purposes in malfunctioning cases.  */
  {
    uw_reg q_bsp      = (uw_reg) intstk->intstk$q_bsp;
    uw_reg q_bspstore = (uw_reg) intstk->intstk$q_bspstore;
    uw_reg q_bspbase  = (uw_reg) intstk->intstk$q_bspbase;
    uw_reg ih_bspbase = (uw_reg) icb->libicb$ih_bspbase;
    
    if (eh_debug)
      printf ("q_bspstore = 0x%lx, q_bsp = 0x%lx, q_bspbase = 0x%lx\n"
	      "ih_bspbase = 0x%lx\n",
	      q_bspstore, q_bsp, q_bspbase, ih_bspbase);

    /* We witness many situations where q_bspbase is set while ih_bspbase is
       null, and every attempt made with q_bspbase badly failed while doing
       nothing resulted in proper behavior.  */
    if (q_bspstore < q_bsp && ih_bspbase && try_bs_copy)
      {
	uw_reg dirty_size = q_bsp - q_bspstore;
	uw_reg q_rnat = (uw_reg) intstk->intstk$q_rnat;

	if (eh_debug)
	  printf ("Attempting an alternate backing store copy ...\n");

	ia64_copy_rbs
	  (context, q_bspstore, ih_bspbase, dirty_size, q_rnat);
	/* Not clear if these are the proper arguments here.  This is what
	   looked the closest to what is performed in the Linux case.  */
      }
    
  }

  context->bsp = (uw_reg)intstk->intstk$q_bsp;
  fs->no_reg_stack_frame = 1;

  context->pr  = (uw_reg)intstk->intstk$q_preds;
  context->gp  = (uw_reg)intstk->intstk$q_gp;

  /* We're directly setting up the "context" for a VMS exception handler.
     The "previous SP" for it is the SP upon the handler's entry, that is
     the SP at the condition/interruption/exception point.  */  
  context->psp = (uw_reg)icb->libicb$ih_sp;

  /* Previous Frame State location.  What eventually ends up in pfs_loc is
     installed with ar.pfs = pfs_loc; br.ret; so setup to target intstk->q_ifs
     to have the interrupted context restored and not that of its caller if
     we happen to have a handler in the interrupted context itself.  */
  fs->curr.reg[UNW_REG_PFS].where = UNW_WHERE_PSPREL;
  fs->curr.reg[UNW_REG_PFS].val
    = (uw_reg)&intstk->intstk$q_ifs - (uw_reg)context->psp;
  fs->curr.reg[UNW_REG_PFS].when = -1;

  /* If we need to unwind further up, past the interrupted context, we need to
     hand out the interrupted context's pfs, still.  */
  context->signal_pfs_loc = (uw_loc) &intstk->intstk$q_pfs;

  /* Finally, rules for RP .  */
  {
    uw_reg * post_sigarray
      = (uw_reg *)chfsig64 + 1 + chfsig64->chf64$l_sig_args;

    uw_reg * ih_pc_loc = post_sigarray - 2;

    fs->curr.reg[UNW_REG_RP].where = UNW_WHERE_PSPREL;
    fs->curr.reg[UNW_REG_RP].val
      = (uw_reg)ih_pc_loc - (uw_reg)context->psp;
    fs->curr.reg[UNW_REG_RP].when = -1;
  }

  return _URC_NO_REASON;
}
     
