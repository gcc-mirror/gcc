/* DWARF2 EH unwinding support for Nios II Linux.
   Copyright (C) 2008-2017 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef inhibit_libc

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.
   The corresponding bits in the Linux kernel are in
   arch/nios2/kernel/signal.c.  */

#include <signal.h>
#include <asm/unistd.h>

/* Exactly the same layout as the kernel structures, unique names.  */
struct nios2_mcontext {
  int version;
  int gregs[32];
};

struct nios2_ucontext {
  unsigned long uc_flags;
  ucontext_t *uc_link;
  stack_t uc_stack;
  struct nios2_mcontext uc_mcontext;
  sigset_t uc_sigmask;	/* mask last for extensibility */
};

#define MD_FALLBACK_FRAME_STATE_FOR nios2_fallback_frame_state

static _Unwind_Reason_Code
nios2_fallback_frame_state (struct _Unwind_Context *context,
			    _Unwind_FrameState *fs)
{
  u_int32_t *pc = (u_int32_t *) context->ra;
  _Unwind_Ptr new_cfa;

  /* The expected sequence of instructions is:
       movi r2,(rt_sigreturn)
       trap
     Check for the trap first.  */
  if (pc[1] != 0x003b683a)
    return _URC_END_OF_STACK;

#define NIOS2_REG(NUM,NAME)						\
  (fs->regs.reg[NUM].how = REG_SAVED_OFFSET,				\
   fs->regs.reg[NUM].loc.offset = (_Unwind_Ptr)&(regs->NAME) - new_cfa)

  if (pc[0] == (0x00800004 | (__NR_rt_sigreturn << 6)))
    {
      struct rt_sigframe {
	siginfo_t info;
	struct nios2_ucontext uc;
      } *rt_ = context->cfa;
      struct nios2_mcontext *regs = &rt_->uc.uc_mcontext;
      int i;

      /* MCONTEXT_VERSION is defined to 2 in the kernel.  */
      if (regs->version != 2)
	return _URC_END_OF_STACK;

      /* The CFA is the user's incoming stack pointer value.  */
      new_cfa = (_Unwind_Ptr)regs->gregs[28];
      fs->regs.cfa_how = CFA_REG_OFFSET;
      fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
      fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

      /* The sequential registers.  */
      for (i = 1; i < 24; i++)
	NIOS2_REG (i, gregs[i-1]);
      
      /* The random registers.  The kernel stores these in a funny order
	 in the gregs array.  */
      NIOS2_REG (RA_REGNO, gregs[23]);
      NIOS2_REG (FP_REGNO, gregs[24]);
      NIOS2_REG (GP_REGNO, gregs[25]);
      NIOS2_REG (EA_REGNO, gregs[27]);
      
      fs->retaddr_column = EA_REGNO;
      fs->signal_frame = 1;
      
      return _URC_NO_REASON;
    }
#undef NIOS2_REG
  return _URC_END_OF_STACK;
}
#endif
