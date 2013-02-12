/* DWARF2 EH unwinding support for Linux/m68k.
   Copyright (C) 2006-2013 Free Software Foundation, Inc.

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

/* <sys/ucontext.h> is unfortunately broken right now.  */
struct uw_ucontext {
	unsigned long	  uc_flags;
	struct ucontext  *uc_link;
	stack_t		  uc_stack;
	mcontext_t	  uc_mcontext;
	unsigned long	  uc_filler[80];
	__sigset_t	  uc_sigmask;
};

#define MD_FALLBACK_FRAME_STATE_FOR m68k_fallback_frame_state

#ifdef __mcoldfire__
#define M68K_FP_SIZE  8
#else
#define M68K_FP_SIZE  12
#endif

static _Unwind_Reason_Code
m68k_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  unsigned short *pc = context->ra;
  long cfa;

  /* moveq #__NR_sigreturn,%d0; trap #0  */
  if (pc[0] == 0x7077 && pc[1] == 0x4e40)
    {
      struct sigcontext *sc;

      /* Context is passed as the 3rd argument.  */
      sc = *(struct sigcontext **) (context->cfa + 8);

      cfa = sc->sc_usp;
      fs->regs.cfa_how = CFA_REG_OFFSET;
      fs->regs.cfa_reg = 15;
      fs->regs.cfa_offset = cfa - (long) context->cfa;

      fs->regs.reg[0].how = REG_SAVED_OFFSET;
      fs->regs.reg[0].loc.offset = (long) &sc->sc_d0 - cfa;
      fs->regs.reg[1].how = REG_SAVED_OFFSET;
      fs->regs.reg[1].loc.offset = (long) &sc->sc_d1 - cfa;
      fs->regs.reg[8].how = REG_SAVED_OFFSET;
      fs->regs.reg[8].loc.offset = (long) &sc->sc_a0 - cfa;
      fs->regs.reg[9].how = REG_SAVED_OFFSET;
      fs->regs.reg[9].loc.offset = (long) &sc->sc_a1 - cfa;

#ifdef __uClinux__
      fs->regs.reg[13].how = REG_SAVED_OFFSET;
      fs->regs.reg[13].loc.offset = (long) &sc->sc_a5 - cfa;
#endif

      fs->regs.reg[24].how = REG_SAVED_OFFSET;
      fs->regs.reg[24].loc.offset = (long) &sc->sc_pc - cfa;

#ifndef __uClinux__
      if (*(int *) sc->sc_fpstate)
	{
	  int *fpregs = (int *) sc->sc_fpregs;

	  fs->regs.reg[16].how = REG_SAVED_OFFSET;
	  fs->regs.reg[16].loc.offset = (long) &fpregs[0] - cfa;
	  fs->regs.reg[17].how = REG_SAVED_OFFSET;
	  fs->regs.reg[17].loc.offset = (long) &fpregs[M68K_FP_SIZE/4] - cfa;
	}
#elif defined __mcffpu__
# error Implement this when uClinux kernel is ported to an FPU architecture
#endif
    }
#ifdef __mcoldfire__
  /* move.l #__NR_rt_sigreturn,%d0; trap #0 */
  else if (pc[0] == 0x203c && pc[1] == 0x0000 &&
	   pc[2] == 0x00ad && pc[3] == 0x4e40)
#else
  /* moveq #~__NR_rt_sigreturn,%d0; not.b %d0; trap #0 */
  else if (pc[0] == 0x7052 && pc[1] == 0x4600 && pc[2] == 0x4e40)
#endif
    {
      struct uw_ucontext *uc;
      greg_t *gregs;
      int i;

      /* Context is passed as the 3rd argument.  */
      uc = *(struct uw_ucontext **) (context->cfa + 8);

      gregs = uc->uc_mcontext.gregs;
      cfa = gregs[15];
      fs->regs.cfa_how = CFA_REG_OFFSET;
      fs->regs.cfa_reg = 15;
      fs->regs.cfa_offset = cfa - (long) context->cfa;

      /* register %d0-%d7/%a0-%a6  */
      for (i = 0; i <= 14; i++)
	{
	  fs->regs.reg[i].how = REG_SAVED_OFFSET;
	  fs->regs.reg[i].loc.offset = (long) &gregs[i] - cfa;
	}

      /* return address  */
      fs->regs.reg[24].how = REG_SAVED_OFFSET;
      fs->regs.reg[24].loc.offset = (long) &gregs[16] - cfa;

#define uc_fpstate      uc_filler[0]

      if (uc->uc_fpstate)
	{
	  long fpregs = (long) uc->uc_mcontext.fpregs.f_fpregs;

	  /* register %fp0-%fp7  */
	  for (i = 16; i <= 23; i++)
	    {
	      fs->regs.reg[i].how = REG_SAVED_OFFSET;
	      fs->regs.reg[i].loc.offset = fpregs - cfa;
	      fpregs += M68K_FP_SIZE;
	    }
	}
    }
  else
    return _URC_END_OF_STACK;

  fs->retaddr_column = 24;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}
#endif /* ifdef inhibit_libc  */
