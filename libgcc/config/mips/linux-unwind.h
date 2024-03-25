/* DWARF2 EH unwinding support for MIPS Linux.
   Copyright (C) 2004-2024 Free Software Foundation, Inc.

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

/* The third parameter to the signal handler points to something with
 * this structure defined in asm/ucontext.h, but the name clashes with
 * struct ucontext from sys/ucontext.h so this private copy is used.  */
typedef struct _sig_ucontext {
    unsigned long         uc_flags;
    struct _sig_ucontext  *uc_link;
    stack_t               uc_stack;
    struct sigcontext uc_mcontext;
    sigset_t      uc_sigmask;
} _sig_ucontext_t;

#define MD_FALLBACK_FRAME_STATE_FOR mips_fallback_frame_state

static _Unwind_Reason_Code
mips_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  u_int32_t *pc = (u_int32_t *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa, reg_offset;
  int i;

  /* A MIPS16 or microMIPS frame.  Signal frames always use the standard
     ISA encoding.  */
  if ((_Unwind_Ptr) pc & 3)
    return _URC_END_OF_STACK;

  /* 24021061 li v0, 0x1061 (rt_sigreturn)*/
  /* 0000000c syscall    */
  /*    or */
  /* 24021017 li v0, 0x1017 (sigreturn) */
  /* 0000000c syscall  */
  if (pc[1] != 0x0000000c)
    return _URC_END_OF_STACK;
#if _MIPS_SIM == _ABIO32
  if (pc[0] == (0x24020000 | __NR_sigreturn))
    {
      struct sigframe {
	u_int32_t ass[4];  /* Argument save space for o32.  */
	u_int32_t trampoline[2];
	struct sigcontext sigctx;
      } *rt_ = context->cfa;
      sc = &rt_->sigctx;
    }
  else
#endif
  if (pc[0] == (0x24020000 | __NR_rt_sigreturn))
    {
      struct rt_sigframe {
	u_int32_t ass[4];  /* Argument save space for o32.  */
	u_int32_t trampoline[2];
	siginfo_t info;
	_sig_ucontext_t uc;
      } *rt_ = context->cfa;
      sc = &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  /* On o32 Linux, the register save slots in the sigcontext are
     eight bytes.  We need the lower half of each register slot,
     so slide our view of the structure back four bytes.  */
#if _MIPS_SIM == _ABIO32 && defined __MIPSEB__
  reg_offset = 4;
#else
  reg_offset = 0;
#endif

  for (i = 0; i < 32; i++) {
    fs->regs.how[i] = REG_SAVED_OFFSET;
    fs->regs.reg[i].loc.offset
      = (_Unwind_Ptr)&(sc->sc_regs[i]) + reg_offset - new_cfa;
  }
  /* "PC & -2" points to the faulting instruction, but the unwind code
     searches for "(ADDR & -2) - 1".  (See MASK_RETURN_ADDR for the source
     of the -2 mask.)  Adding 2 here ensures that "(ADDR & -2) - 1" is the
     address of the second byte of the faulting instruction.

     Note that setting fs->signal_frame would not work.  As the comment
     above MASK_RETURN_ADDR explains, MIPS unwinders must earch for an
     odd-valued address.  */
  fs->regs.how[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__]
    = REG_SAVED_VAL_OFFSET;
  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].loc.offset
    = (_Unwind_Ptr)(sc->sc_pc) + 2 - new_cfa;
  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;

  return _URC_NO_REASON;
}
#endif
