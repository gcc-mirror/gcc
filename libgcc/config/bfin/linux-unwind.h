/* DWARF2 EH unwinding support for Blackfin.
   Copyright (C) 2007-2017 Free Software Foundation, Inc.

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

#define MD_FALLBACK_FRAME_STATE_FOR bfin_fallback_frame_state

static _Unwind_Reason_Code
bfin_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;
  long new_cfa;

  /* P0=__NR_rt_sigreturn (X); EXCPT  0x0; */
  if (*(unsigned short *)pc == 0xe128
      && *(unsigned short *)(pc + 2) == 0x00ad
      && *(unsigned short *)(pc + 4) == 0x00a0)
    {
      struct rt_sigframe {
	int sig;
	siginfo_t *pinfo;
	void *puc;
	char retcode[8];
	siginfo_t info;
	struct ucontext uc;
      } *rt_ = context->cfa;

      /* The void * cast is necessary to avoid an aliasing warning.
         The aliasing warning is correct, but should not be a problem
         because it does not alias anything.  */
      sc = (struct sigcontext *)(void *)&rt_->uc.uc_mcontext.gregs;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = sc->sc_usp;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 14;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (long)&sc->sc_r0 - new_cfa;
  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (long)&sc->sc_r1 - new_cfa;
  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (long)&sc->sc_r2 - new_cfa;
  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (long)&sc->sc_r3 - new_cfa;
  fs->regs.reg[4].how = REG_SAVED_OFFSET;
  fs->regs.reg[4].loc.offset = (long)&sc->sc_r4 - new_cfa;
  fs->regs.reg[5].how = REG_SAVED_OFFSET;
  fs->regs.reg[5].loc.offset = (long)&sc->sc_r5 - new_cfa;
  fs->regs.reg[6].how = REG_SAVED_OFFSET;
  fs->regs.reg[6].loc.offset = (long)&sc->sc_r6 - new_cfa;
  fs->regs.reg[7].how = REG_SAVED_OFFSET;
  fs->regs.reg[7].loc.offset = (long)&sc->sc_r7 - new_cfa;
  fs->regs.reg[8].how = REG_SAVED_OFFSET;
  fs->regs.reg[8].loc.offset = (long)&sc->sc_p0 - new_cfa;
  fs->regs.reg[9].how = REG_SAVED_OFFSET;
  fs->regs.reg[9].loc.offset = (long)&sc->sc_p1 - new_cfa;
  fs->regs.reg[10].how = REG_SAVED_OFFSET;
  fs->regs.reg[10].loc.offset = (long)&sc->sc_p2 - new_cfa;
  fs->regs.reg[11].how = REG_SAVED_OFFSET;
  fs->regs.reg[11].loc.offset = (long)&sc->sc_p3 - new_cfa;
  fs->regs.reg[12].how = REG_SAVED_OFFSET;
  fs->regs.reg[12].loc.offset = (long)&sc->sc_p4 - new_cfa;
  fs->regs.reg[13].how = REG_SAVED_OFFSET;
  fs->regs.reg[13].loc.offset = (long)&sc->sc_p5 - new_cfa;

  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (long)&sc->sc_fp - new_cfa;
  fs->regs.reg[16].how = REG_SAVED_OFFSET;
  fs->regs.reg[16].loc.offset = (long)&sc->sc_i0 - new_cfa;
  fs->regs.reg[17].how = REG_SAVED_OFFSET;
  fs->regs.reg[17].loc.offset = (long)&sc->sc_i1 - new_cfa;
  fs->regs.reg[18].how = REG_SAVED_OFFSET;
  fs->regs.reg[18].loc.offset = (long)&sc->sc_i2 - new_cfa;
  fs->regs.reg[19].how = REG_SAVED_OFFSET;
  fs->regs.reg[19].loc.offset = (long)&sc->sc_i3 - new_cfa;
  fs->regs.reg[20].how = REG_SAVED_OFFSET;
  fs->regs.reg[20].loc.offset = (long)&sc->sc_b0 - new_cfa;
  fs->regs.reg[21].how = REG_SAVED_OFFSET;
  fs->regs.reg[21].loc.offset = (long)&sc->sc_b1 - new_cfa;
  fs->regs.reg[22].how = REG_SAVED_OFFSET;
  fs->regs.reg[22].loc.offset = (long)&sc->sc_b2 - new_cfa;
  fs->regs.reg[23].how = REG_SAVED_OFFSET;
  fs->regs.reg[23].loc.offset = (long)&sc->sc_b3 - new_cfa;
  fs->regs.reg[24].how = REG_SAVED_OFFSET;
  fs->regs.reg[24].loc.offset = (long)&sc->sc_l0 - new_cfa;
  fs->regs.reg[25].how = REG_SAVED_OFFSET;
  fs->regs.reg[25].loc.offset = (long)&sc->sc_l1 - new_cfa;
  fs->regs.reg[26].how = REG_SAVED_OFFSET;
  fs->regs.reg[26].loc.offset = (long)&sc->sc_l2 - new_cfa;
  fs->regs.reg[27].how = REG_SAVED_OFFSET;
  fs->regs.reg[27].loc.offset = (long)&sc->sc_l3 - new_cfa;
  fs->regs.reg[28].how = REG_SAVED_OFFSET;
  fs->regs.reg[28].loc.offset = (long)&sc->sc_m0 - new_cfa;
  fs->regs.reg[29].how = REG_SAVED_OFFSET;
  fs->regs.reg[29].loc.offset = (long)&sc->sc_m1 - new_cfa;
  fs->regs.reg[30].how = REG_SAVED_OFFSET;
  fs->regs.reg[30].loc.offset = (long)&sc->sc_m2 - new_cfa;
  fs->regs.reg[31].how = REG_SAVED_OFFSET;
  fs->regs.reg[31].loc.offset = (long)&sc->sc_m3 - new_cfa;
  /* FIXME: Handle A0, A1, CC.  */
  fs->regs.reg[35].how = REG_SAVED_OFFSET;
  fs->regs.reg[35].loc.offset = (long)&sc->sc_rets - new_cfa;
  fs->regs.reg[36].how = REG_SAVED_OFFSET;
  fs->regs.reg[36].loc.offset = (long)&sc->sc_pc - new_cfa;
  fs->regs.reg[37].how = REG_SAVED_OFFSET;
  fs->regs.reg[37].loc.offset = (long)&sc->sc_retx - new_cfa;

  fs->regs.reg[40].how = REG_SAVED_OFFSET;
  fs->regs.reg[40].loc.offset = (long)&sc->sc_astat - new_cfa;
  fs->regs.reg[41].how = REG_SAVED_OFFSET;
  fs->regs.reg[41].loc.offset = (long)&sc->sc_seqstat - new_cfa;

  fs->regs.reg[44].how = REG_SAVED_OFFSET;
  fs->regs.reg[44].loc.offset = (long)&sc->sc_lt0 - new_cfa;
  fs->regs.reg[45].how = REG_SAVED_OFFSET;
  fs->regs.reg[45].loc.offset = (long)&sc->sc_lt1 - new_cfa;
  fs->regs.reg[46].how = REG_SAVED_OFFSET;
  fs->regs.reg[46].loc.offset = (long)&sc->sc_lc0 - new_cfa;
  fs->regs.reg[47].how = REG_SAVED_OFFSET;
  fs->regs.reg[47].loc.offset = (long)&sc->sc_lc1 - new_cfa;
  fs->regs.reg[48].how = REG_SAVED_OFFSET;
  fs->regs.reg[48].loc.offset = (long)&sc->sc_lb0 - new_cfa;
  fs->regs.reg[49].how = REG_SAVED_OFFSET;
  fs->regs.reg[49].loc.offset = (long)&sc->sc_lb1 - new_cfa;
  fs->retaddr_column = 35;

  return _URC_NO_REASON;
}

#endif /* ifdef inhibit_libc */
