/* Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

#include <signal.h>
#include <stdint.h>
#include <sys/ucontext.h>

#define LI_A7_8B 0x08b00893
#define ECALL    0x00000073

#define MD_FALLBACK_FRAME_STATE_FOR riscv_fallback_frame_state

static _Unwind_Reason_Code
riscv_fallback_frame_state (struct _Unwind_Context *context,
			    _Unwind_FrameState * fs)
{
  /* The kernel creates an rt_sigframe on the stack immediately prior
     to delivering a signal.

     This structure must have the same shape as the linux kernel
     equivalent.  */
  struct rt_sigframe
  {
    siginfo_t info;
    ucontext_t uc;
  };

  struct rt_sigframe *rt_;
  _Unwind_Ptr new_cfa;
  uint16_t *pc = context->ra;
  struct sigcontext *sc;
  int i;

  /* A signal frame will have a return address pointing to
     __default_sa_restorer. This code is hardwired as:

     0x08b00893		li	a7,0x8b
     0x00000073		ecall

     Note, the PC might only have 2-byte alignment.
   */
  if (pc[0] != (uint16_t)LI_A7_8B || pc[1] != (uint16_t)(LI_A7_8B >> 16)
      || pc[2] != (uint16_t)ECALL || pc[3] != (uint16_t)(ECALL >> 16))
    return _URC_END_OF_STACK;

  rt_ = context->cfa;
  sc = &rt_->uc.uc_mcontext;

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (i = 0; i < 32; i++)
    {
      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset = (_Unwind_Ptr) &sc->gregs[i] - new_cfa;
    }

  fs->signal_frame = 1;
  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;
  fs->regs.reg[fs->retaddr_column].how = REG_SAVED_VAL_OFFSET;
  fs->regs.reg[fs->retaddr_column].loc.offset =
    (_Unwind_Ptr) sc->gregs[0] - new_cfa;

  return _URC_NO_REASON;
}

#endif
