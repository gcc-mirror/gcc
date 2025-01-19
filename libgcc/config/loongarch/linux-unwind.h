/* DWARF2 EH unwinding support for LoongArch Linux.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.

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
#include <sys/syscall.h>
#include <sys/ucontext.h>

#define MD_FALLBACK_FRAME_STATE_FOR loongarch_fallback_frame_state

static _Unwind_Reason_Code
loongarch_fallback_frame_state (struct _Unwind_Context *context,
				_Unwind_FrameState *fs)
{
  u_int32_t *pc = (u_int32_t *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa;
  int i;

  /* 03822c0b li.d a7, 0x8b (sigreturn)  */
  /* 002b0000 syscall 0  */
  if (pc[1] != 0x002b0000)
    return _URC_END_OF_STACK;
  if (pc[0] == 0x03822c0b)
    {
      struct rt_sigframe
      {
	siginfo_t info;
	ucontext_t uc;
      } *rt_ = context->cfa;
      sc = (struct sigcontext *) (void *) &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (i = 0; i < 32; i++)
    {
      fs->regs.how[i] = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = (_Unwind_Ptr) & (sc->sc_regs[i]) - new_cfa;
    }

  fs->signal_frame = 1;
  fs->regs.how[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__]
    = REG_SAVED_VAL_OFFSET;
  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].loc.offset
    = (_Unwind_Ptr) (sc->sc_pc) - new_cfa;
  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;

  return _URC_NO_REASON;
}
#endif
