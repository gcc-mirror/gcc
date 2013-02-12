/* DWARF2 EH unwinding support for IA64 Linux.
   Copyright (C) 2004-2013 Free Software Foundation, Inc.

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

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-ia64.c for the structs.  */

/* This works only for glibc-2.3 and later, because sigcontext is different
   in glibc-2.2.4.  */

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 3)
#include <signal.h>
#include <sys/ucontext.h>

#define IA64_GATE_AREA_START 0xa000000000000100LL
#define IA64_GATE_AREA_END   0xa000000000030000LL

#define MD_FALLBACK_FRAME_STATE_FOR ia64_fallback_frame_state

static _Unwind_Reason_Code
ia64_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  if (context->rp >= IA64_GATE_AREA_START
      && context->rp < IA64_GATE_AREA_END)
    {
      struct sigframe {
	char scratch[16];
	unsigned long sig_number;
	siginfo_t *info;
	struct sigcontext *sc;
      } *frame_ = (struct sigframe *)context->psp;
      struct sigcontext *sc = frame_->sc;

      /* Restore scratch registers in case the unwinder needs to
	 refer to a value stored in one of them.  */
      {
	int i;

	for (i = 2; i < 4; i++)
	  context->ireg[i - 2].loc = &sc->sc_gr[i];
	for (i = 8; i < 12; i++)
	  context->ireg[i - 2].loc = &sc->sc_gr[i];
	for (i = 14; i < 32; i++)
	  context->ireg[i - 2].loc = &sc->sc_gr[i];
      }

      context->fpsr_loc = &(sc->sc_ar_fpsr);
      context->signal_pfs_loc = &(sc->sc_ar_pfs);
      context->lc_loc = &(sc->sc_ar_lc);
      context->unat_loc = &(sc->sc_ar_unat);
      context->br_loc[0] = &(sc->sc_br[0]);
      context->br_loc[6] = &(sc->sc_br[6]);
      context->br_loc[7] = &(sc->sc_br[7]);
      context->pr = sc->sc_pr;
      context->psp = sc->sc_gr[12];
      context->gp = sc->sc_gr[1];
      /* Signal frame doesn't have an associated reg. stack frame
         other than what we adjust for below.	  */
      fs -> no_reg_stack_frame = 1;

      if (sc->sc_rbs_base)
	{
	  /* Need to switch from alternate register backing store.  */
	  long ndirty, loadrs = sc->sc_loadrs >> 16;
	  unsigned long alt_bspstore = context->bsp - loadrs;
	  unsigned long bspstore;
	  unsigned long *ar_bsp = (unsigned long *)(sc->sc_ar_bsp);

	  ndirty = ia64_rse_num_regs ((unsigned long *) alt_bspstore,
				      (unsigned long *) context->bsp);
	  bspstore = (unsigned long)
	    ia64_rse_skip_regs (ar_bsp, -ndirty);
	  ia64_copy_rbs (context, bspstore, alt_bspstore, loadrs,
			 sc->sc_ar_rnat);
	}

      /* Don't touch the branch registers o.t. b0, b6 and b7.
	 The kernel doesn't pass the preserved branch registers
	 in the sigcontext but leaves them intact, so there's no
	 need to do anything with them here.  */
      {
	unsigned long sof = sc->sc_cfm & 0x7f;
	context->bsp = (unsigned long)
	  ia64_rse_skip_regs ((unsigned long *)(sc->sc_ar_bsp), -sof);
      }

      /* Account for use of br.ret to resume execution of user code.  */
      fs->curr.reg[UNW_REG_RP].where = UNW_WHERE_SPREL;
      fs->curr.reg[UNW_REG_RP].val
	= (unsigned long)&(sc->sc_ip) - context->psp;
      fs->curr.reg[UNW_REG_RP].when = -1;

      fs->curr.reg[UNW_REG_PFS].where = UNW_WHERE_SPREL;
      fs->curr.reg[UNW_REG_PFS].val
	= (unsigned long)&(sc->sc_cfm) - context->psp;
      fs ->curr.reg[UNW_REG_PFS].when = -1;

      return _URC_NO_REASON;
    }
  return _URC_END_OF_STACK;
}

#define MD_HANDLE_UNWABI ia64_handle_unwabi

#define ABI_MARKER_OLD_LINUX_SIGTRAMP	((0 << 8) | 's')
#define ABI_MARKER_OLD_LINUX_INTERRUPT	((0 << 8) | 'i')
#define ABI_MARKER_LINUX_SIGTRAMP	((3 << 8) | 's')
#define ABI_MARKER_LINUX_INTERRUPT	((3 << 8) | 'i')

static void
ia64_handle_unwabi (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  if (fs->unwabi == ABI_MARKER_LINUX_SIGTRAMP
      || fs->unwabi == ABI_MARKER_OLD_LINUX_SIGTRAMP)
    {
      struct sigframe {
	char scratch[16];
	unsigned long sig_number;
	siginfo_t *info;
	struct sigcontext *sc;
      } *frame = (struct sigframe *)context->psp;
      struct sigcontext *sc = frame->sc;

      /* Restore scratch registers in case the unwinder needs to
	 refer to a value stored in one of them.  */
      {
	int i;

	for (i = 2; i < 4; i++)
	  context->ireg[i - 2].loc = &sc->sc_gr[i];
	for (i = 8; i < 12; i++)
	  context->ireg[i - 2].loc = &sc->sc_gr[i];
	for (i = 14; i < 32; i++)
	  context->ireg[i - 2].loc = &sc->sc_gr[i];
      }

      context->signal_pfs_loc = &(sc->sc_ar_pfs);
      context->lc_loc = &(sc->sc_ar_lc);
      context->unat_loc = &(sc->sc_ar_unat);
      context->br_loc[0] = &(sc->sc_br[0]);
      context->br_loc[6] = &(sc->sc_br[6]);
      context->br_loc[7] = &(sc->sc_br[7]);
      context->pr = sc->sc_pr;
      context->gp = sc->sc_gr[1];
      /* Signal frame doesn't have an associated reg. stack frame
         other than what we adjust for below.	  */
      fs -> no_reg_stack_frame = 1;

      if (sc->sc_rbs_base)
	{
	  /* Need to switch from alternate register backing store.  */
	  long ndirty, loadrs = sc->sc_loadrs >> 16;
	  unsigned long alt_bspstore = context->bsp - loadrs;
	  unsigned long bspstore;
	  unsigned long *ar_bsp = (unsigned long *)(sc->sc_ar_bsp);

	  ndirty = ia64_rse_num_regs ((unsigned long *) alt_bspstore,
				      (unsigned long *) context->bsp);
	  bspstore = (unsigned long) ia64_rse_skip_regs (ar_bsp, -ndirty);
	  ia64_copy_rbs (context, bspstore, alt_bspstore, loadrs,
			 sc->sc_ar_rnat);
	}

      /* Don't touch the branch registers o.t. b0, b6 and b7.
	 The kernel doesn't pass the preserved branch registers
	 in the sigcontext but leaves them intact, so there's no
	 need to do anything with them here.  */
      {
	unsigned long sof = sc->sc_cfm & 0x7f;
	context->bsp = (unsigned long)
	  ia64_rse_skip_regs ((unsigned long *)(sc->sc_ar_bsp), -sof);
      }

      /* The use of br.ret to resume execution of user code is already
	 accounted for in the unwind ABI.  */
    }
}
#endif /* glibc-2.3 or better */
