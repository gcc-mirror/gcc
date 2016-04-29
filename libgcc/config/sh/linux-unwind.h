/* DWARF2 EH unwinding support for SH Linux.
   Copyright (C) 2004-2016 Free Software Foundation, Inc.

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


/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.
   Don't use this at all if inhibit_libc is used.  */

#ifndef inhibit_libc

#include <signal.h>
#include <sys/ucontext.h>
#include "insn-constants.h"

#define SH_DWARF_FRAME_GP0	0
#define SH_DWARF_FRAME_FP0	25
#define SH_DWARF_FRAME_XD0	87
#define SH_DWARF_FRAME_PR	17
#define SH_DWARF_FRAME_GBR	18
#define SH_DWARF_FRAME_MACH	20
#define SH_DWARF_FRAME_MACL	21
#define SH_DWARF_FRAME_PC	16
#define SH_DWARF_FRAME_SR	22
#define SH_DWARF_FRAME_FPUL	23
#define SH_DWARF_FRAME_FPSCR	24

#define MD_FALLBACK_FRAME_STATE_FOR sh_fallback_frame_state

static _Unwind_Reason_Code
sh_fallback_frame_state (struct _Unwind_Context *context,
			 _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;
  long new_cfa;
  int i;
#if defined (__SH3E__) || defined (__SH4__)
  int r;
#endif

  /* mov.w 1f,r3; trapa #0x10; 1: .short 0x77  (sigreturn)  */
  /* mov.w 1f,r3; trapa #0x10; 1: .short 0xad  (rt_sigreturn)  */
  /* Newer kernel uses pad instructions to avoid an SH-4 core bug.  */
  /* mov.w 1f,r3; trapa #0x10; or r0,r0; or r0,r0; or r0,r0; or r0,r0;
     or r0,r0; 1: .short 0x77  (sigreturn)  */
  /* mov.w 1f,r3; trapa #0x10; or r0,r0; or r0,r0; or r0,r0; or r0,r0;
     or r0,r0; 1: .short 0xad  (rt_sigreturn)  */
  if (((*(unsigned short *) (pc+0)  == 0x9300)
       && (*(unsigned short *) (pc+2)  == 0xc310)
       && (*(unsigned short *) (pc+4)  == 0x0077))
      || (((*(unsigned short *) (pc+0)  == 0x9305)
	   && (*(unsigned short *) (pc+2)  == 0xc310)
	   && (*(unsigned short *) (pc+14)  == 0x0077))))
    sc = context->cfa;
  else if (((*(unsigned short *) (pc+0) == 0x9300)
	    && (*(unsigned short *) (pc+2)  == 0xc310)
	    && (*(unsigned short *) (pc+4)  == 0x00ad))
	   || (((*(unsigned short *) (pc+0) == 0x9305)
		&& (*(unsigned short *) (pc+2)  == 0xc310)
		&& (*(unsigned short *) (pc+14)  == 0x00ad))))
    {
      struct rt_sigframe {
	siginfo_t info;
	struct ucontext uc;
      } *rt_ = context->cfa;
      /* The void * cast is necessary to avoid an aliasing warning.
         The aliasing warning is correct, but should not be a problem
         because it does not alias anything.  */
      sc = (struct sigcontext *) (void *) &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = sc->sc_regs[15];
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 15;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  for (i = 0; i < 15; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= (long)&(sc->sc_regs[i]) - new_cfa;
    }

  fs->regs.reg[SH_DWARF_FRAME_PR].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_PR].loc.offset
    = (long)&(sc->sc_pr) - new_cfa;
  fs->regs.reg[SH_DWARF_FRAME_SR].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_SR].loc.offset
    = (long)&(sc->sc_sr) - new_cfa;
  fs->regs.reg[SH_DWARF_FRAME_GBR].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_GBR].loc.offset
    = (long)&(sc->sc_gbr) - new_cfa;
  fs->regs.reg[SH_DWARF_FRAME_MACH].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_MACH].loc.offset
    = (long)&(sc->sc_mach) - new_cfa;
  fs->regs.reg[SH_DWARF_FRAME_MACL].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_MACL].loc.offset
    = (long)&(sc->sc_macl) - new_cfa;

#if defined (__SH3E__) || defined (__SH4__)
  r = SH_DWARF_FRAME_FP0;
  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[r+i].how = REG_SAVED_OFFSET;
      fs->regs.reg[r+i].loc.offset
	= (long)&(sc->sc_fpregs[i]) - new_cfa;
    }

  r = SH_DWARF_FRAME_XD0;
  for (i = 0; i < 8; i++)
    {
      fs->regs.reg[r+i].how = REG_SAVED_OFFSET;
      fs->regs.reg[r+i].loc.offset
	= (long)&(sc->sc_xfpregs[2*i]) - new_cfa;
    }

  fs->regs.reg[SH_DWARF_FRAME_FPUL].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_FPUL].loc.offset
    = (long)&(sc->sc_fpul) - new_cfa;
  fs->regs.reg[SH_DWARF_FRAME_FPSCR].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_FPSCR].loc.offset
    = (long)&(sc->sc_fpscr) - new_cfa;
#endif

  fs->regs.reg[SH_DWARF_FRAME_PC].how = REG_SAVED_OFFSET;
  fs->regs.reg[SH_DWARF_FRAME_PC].loc.offset
    = (long)&(sc->sc_pc) - new_cfa;
  fs->retaddr_column = SH_DWARF_FRAME_PC;
  fs->signal_frame = 1;
  return _URC_NO_REASON;
}

#endif /* inhibit_libc */
