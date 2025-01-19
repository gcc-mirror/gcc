/* DWARF2 EH unwinding support for OpenRISC Linux.
   Copyright (C) 2018-2025 Free Software Foundation, Inc.

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
#include <sys/ucontext.h>

#define MD_FALLBACK_FRAME_STATE_FOR or1k_fallback_frame_state

static _Unwind_Reason_Code
or1k_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  unsigned int *pc = context->ra;
  struct rt_sigframe {
    siginfo_t info;
    ucontext_t uc;
  } *rt;
  struct sigcontext *sc;
  long new_cfa;
  int i;

  if (pc[0] != 0xa960008b		/* l.ori r11, r0, NR_rt_sigreturn */
      || pc[1] != 0x20000001)		/* l.sys 1 */
    return _URC_END_OF_STACK;
  if (context->cfa == 0)
    return _URC_END_OF_STACK;

  rt = context->cfa;
  sc = (struct sigcontext *) &rt->uc.uc_mcontext;

  new_cfa = sc->regs.gpr[1];
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 1;
  fs->regs.cfa_offset = new_cfa - (long) context->cfa;
  for (i = 2; i < 32; ++i)
    {
      fs->regs.how[i] = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = (long) &sc->regs.gpr[i] - new_cfa;
    }
  fs->regs.how[32] = REG_SAVED_OFFSET;
  fs->regs.reg[32].loc.offset = (long)&sc->regs.pc - new_cfa;
  fs->retaddr_column = 32;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#define MD_FROB_UPDATE_CONTEXT or1k_frob_update_context

/* Fix up for signal handlers that don't have S flag set.  */

static void
or1k_frob_update_context (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs ATTRIBUTE_UNUSED)
{
  unsigned int *pc = context->ra;

  if (pc[0] == 0xa960008b		/* l.ori r11, r0, NR_rt_sigreturn */
      && pc[1] == 0x20000001)		/* l.sys 1 */
    _Unwind_SetSignalFrame (context, 1);
}
#endif
