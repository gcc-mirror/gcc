/* DWARF2 EH unwinding support for C-SKY Linux.
   Copyright (C) 2018-2021 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

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

#ifndef inhibit_libc

/* Do code reading to identify a signal frame, and set the frame state data
   appropriately.  See unwind-dw2.c for the structs.  */

#include <signal.h>
#include <asm/unistd.h>

#define TRAP0_V1 0x0008
#define MOVI_R1_119_V1 (0x6000 + (119 << 4) + 1)
#define MOVI_R1_127_V1 (0x6000 + (127 << 4) + 1)
#define ADDI_R1_32_V1 (0x2000 + (31 << 4) + 1)
#define ADDI_R1_14_V1 (0x2000 + (13 << 4) + 1)
#define ADDI_R1_12_V1 (0x2000 + (11 << 4) + 1)

#define TRAP0_V2_PART0 0xc000
#define TRAP0_V2_PART1 0x2020
#define MOVI_R7_119_V2_PART0 0xea07
#define MOVI_R7_119_V2_PART1 119
#define MOVI_R7_173_V2_PART0 0xea07
#define MOVI_R7_173_V2_PART1 173
#define MOVI_R7_139_V2_PART0 0xea07
#define MOVI_R7_139_V2_PART1 139

#define sc_pt_regs(x) (sc->sc_pt_regs.x)
#define sc_pt_regs_lr (sc->sc_pt_regs.lr)
#define sc_pt_regs_tls(x) sc_pt_regs(x)

#define MD_FALLBACK_FRAME_STATE_FOR csky_fallback_frame_state

static _Unwind_Reason_Code
csky_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  u_int16_t *pc = (u_int16_t *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa;
  int i;

  /* movi r7, __NR_rt_sigreturn; trap 0  */
  if ((*(pc + 0) == MOVI_R7_139_V2_PART0)
      && (*(pc + 1) == MOVI_R7_139_V2_PART1) && (*(pc + 2) == TRAP0_V2_PART0)
      && (*(pc + 3) == TRAP0_V2_PART1))
    {
      struct rt_sigframe
      {
	int sig;
	struct siginfo *pinfo;
	void *puc;
	siginfo_t info;
	ucontext_t uc;
      } *_rt = context->cfa;
      sc = &(_rt->uc.uc_mcontext);
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = (_Unwind_Ptr) sc_pt_regs (usp);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = STACK_POINTER_REGNUM;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (_Unwind_Ptr) & sc_pt_regs (a0) - new_cfa;

  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (_Unwind_Ptr) & sc_pt_regs (a1) - new_cfa;

  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (_Unwind_Ptr) & sc_pt_regs (a2) - new_cfa;

  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (_Unwind_Ptr) & sc_pt_regs (a3) - new_cfa;

  for (i = 4; i < 14; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset =
	(_Unwind_Ptr) & sc_pt_regs (regs[i - 4]) - new_cfa;
    }

  for (i = 16; i < 31; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset =
	(_Unwind_Ptr) & sc_pt_regs (exregs[i - 16]) - new_cfa;
    }

  fs->regs.reg[31].loc.offset =
    (_Unwind_Ptr) & sc_pt_regs_tls (tls) - new_cfa;
  /* FIXME : hi lo ? */
  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (_Unwind_Ptr) & sc_pt_regs_lr - new_cfa;

  fs->regs.reg[32].how = REG_SAVED_OFFSET;
  fs->regs.reg[32].loc.offset = (_Unwind_Ptr) & sc_pt_regs (pc) - new_cfa;
  fs->retaddr_column = 32;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif
