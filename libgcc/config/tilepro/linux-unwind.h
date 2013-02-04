/* DWARF2 EH unwinding support for TILEPro.
   Copyright (C) 2011-2013 Free Software Foundation, Inc.
   Contributed by Walter Lee (walt@tilera.com)

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

#include <arch/abi.h>
#include <signal.h>
#include <sys/ucontext.h>
#include <linux/unistd.h>

/* Macro to define a copy of the kernel's __rt_sigreturn function
   (in arch/tile/kernel/entry.S).  If that function is changed,
   this one needs to be changed to match it.  */
#define _sigreturn_asm(REG, NR) asm(                    \
    ".pushsection .text.__rt_sigreturn,\"a\"\n"         \
    ".global __rt_sigreturn\n"                          \
    ".type __rt_sigreturn,@function\n"                  \
    "__rt_sigreturn:\n"                                 \
    "moveli " #REG ", " #NR "\n"                        \
    "swint1\n"                                          \
    ".size __rt_sigreturn, . - __rt_sigreturn\n"        \
    ".popsection")
#define sigreturn_asm(REG, NR) _sigreturn_asm(REG, NR)
sigreturn_asm (TREG_SYSCALL_NR_NAME, __NR_rt_sigreturn);
#define SIGRETURN_LEN 16
extern char __rt_sigreturn[];

#define MD_FALLBACK_FRAME_STATE_FOR tile_fallback_frame_state

static _Unwind_Reason_Code
tile_fallback_frame_state (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs)
{
  unsigned char *pc = context->ra;
  struct sigcontext *sc;
  long new_cfa;
  int i;

  struct rt_sigframe {
    unsigned char save_area[C_ABI_SAVE_AREA_SIZE];
    siginfo_t info;
    struct ucontext uc;
  } *rt_;

  /* Return if this is not a signal handler.  */
  if (memcmp (pc, __rt_sigreturn, SIGRETURN_LEN) != 0)
    return _URC_END_OF_STACK;

  /* It was a signal handler; update the reported PC to point to our
     copy, since that will be findable with dladdr() and therefore
     somewhat easier to help understand what actually happened. */
  context->ra = __rt_sigreturn;

  rt_ = context->cfa;
  sc = &rt_->uc.uc_mcontext;

  new_cfa = sc->sp;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = STACK_POINTER_REGNUM;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;

  for (i = 0; i < 56; ++i)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= (long)&sc->gregs[i] - new_cfa;
    }

  fs->regs.reg[56].how = REG_SAVED_OFFSET;
  fs->regs.reg[56].loc.offset = (long)&sc->pc - new_cfa;
  fs->retaddr_column = 56;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif /* ifdef inhibit_libc  */
