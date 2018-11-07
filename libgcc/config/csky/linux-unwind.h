/* DWARF2 EH unwinding support for C-SKY Linux.
   Copyright (C) 2018 Free Software Foundation, Inc.
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

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#include <signal.h>
#include <asm/unistd.h>

/* The third parameter to the signal handler points to something with
   this structure defined in asm/ucontext.h, but the name clashes with
   struct ucontext from sys/ucontext.h so this private copy is used.  */
typedef struct _sig_ucontext {
  unsigned long         uc_flags;
  struct _sig_ucontext  *uc_link;
  stack_t               uc_stack;
  struct sigcontext     uc_mcontext;
  sigset_t              uc_sigmask;
} _sig_ucontext_t;

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
  if ((*(pc+0) == 0xea07) && (*(pc+1) == 119)
      && (*(pc+2) == 0xc000) &&  (*(pc+3) == 0x2020))
  {
    struct sigframe
    {
      int sig;
      int code;
      struct sigcontext *psc;
      unsigned long extramask[2]; /* _NSIG_WORDS */
      struct sigcontext sc;
    } *_rt = context->cfa;
    sc = _rt->psc; // &(_rt->sc);
  }
  /* movi r7, __NR_rt_sigreturn; trap 0  */
  else if ((*(pc+0) == 0xea07) && (*(pc+1) == 173)
	   && (*(pc+2) == 0xc000) &&  (*(pc+3) == 0x2020))
  {
    struct rt_sigframe
    {
      int sig;
      struct siginfo *pinfo;
      void* puc;
      siginfo_t info;
      struct ucontext uc;
    } *_rt = context->cfa;
    sc = &(_rt->uc.uc_mcontext);
  }
  else
    return _URC_END_OF_STACK;

  new_cfa = (_Unwind_Ptr) sc->sc_usp;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = STACK_POINTER_REGNUM;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = (_Unwind_Ptr)&(sc->sc_a0) - new_cfa;

  fs->regs.reg[1].how = REG_SAVED_OFFSET;
  fs->regs.reg[1].loc.offset = (_Unwind_Ptr)&(sc->sc_a1) - new_cfa;

  fs->regs.reg[2].how = REG_SAVED_OFFSET;
  fs->regs.reg[2].loc.offset = (_Unwind_Ptr)&(sc->sc_a2) - new_cfa;

  fs->regs.reg[3].how = REG_SAVED_OFFSET;
  fs->regs.reg[3].loc.offset = (_Unwind_Ptr)&(sc->sc_a3) - new_cfa;

  for (i = 4; i < 14; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = ((_Unwind_Ptr)&(sc->sc_regs[i - 4])
				    - new_cfa);
    }

  for (i = 16; i < 32; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = ((_Unwind_Ptr)&(sc->sc_exregs[i - 16])
				    - new_cfa);
    }

  /* FIXME : hi lo ? */
  fs->regs.reg[15].how = REG_SAVED_OFFSET;
  fs->regs.reg[15].loc.offset = (_Unwind_Ptr)&(sc->sc_r15) - new_cfa;

  fs->regs.reg[56].how = REG_SAVED_OFFSET;
  fs->regs.reg[56].loc.offset = (_Unwind_Ptr)&(sc->sc_pc) - new_cfa;
  fs->retaddr_column = 56;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}


#endif
