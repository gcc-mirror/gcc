/* DWARF2 EH unwinding support for NDS32 Linux signal frame.
   Copyright (C) 2014-2023 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

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
   arch/nds32/kernel/signal.c.  */

#include <signal.h>
#include <asm/unistd.h>
#include <sys/ucontext.h>

/* Exactly the same layout as the kernel structures, unique names.  */

/* arch/nds32/kernel/signal.c */
struct _rt_sigframe {
  siginfo_t info;
  struct ucontext_t uc;
};

#define RT_SIGRETURN 0x8b00f044

#define MD_FALLBACK_FRAME_STATE_FOR nds32_fallback_frame_state

/* This function is supposed to be invoked by uw_frame_state_for()
   when there is no unwind data available.

   Generally, given the _Unwind_Context CONTEXT for a stack frame,
   we need to look up its caller and decode information into FS.
   However, if the exception handling happens within a signal handler,
   the return address of signal handler is a special module, which
   contains signal return syscall and has no FDE in the .eh_frame section.
   We need to implement MD_FALLBACK_FRAME_STATE_FOR so that we can
   unwind through signal frames.  */
static _Unwind_Reason_Code
nds32_fallback_frame_state (struct _Unwind_Context *context,
			    _Unwind_FrameState *fs)
{
  u_int32_t *pc = (u_int32_t *) context->ra;
  struct sigcontext *sc_;
  _Unwind_Ptr new_cfa;

#ifdef __NDS32_EB__
#error "Signal handler is not supported for force unwind."
#endif

  if ((_Unwind_Ptr) pc & 3)
    return _URC_END_OF_STACK;

  /* Check if we are going through a signal handler.
     See arch/nds32/kernel/signal.c implementation.
     FIXME: Currently we only handle little endian (EL) case.  */
  if (pc[0] == RT_SIGRETURN)
    {
      /* Using '_sigfame' memory address to locate kernal's sigcontext.
	 The sigcontext structures in arch/nds32/include/asm/sigcontext.h.  */
      struct _rt_sigframe *rt_;
      rt_ = context->cfa;
      sc_ = &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  /* Update cfa from sigcontext.  */
  new_cfa = (_Unwind_Ptr) sc_;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = STACK_POINTER_REGNUM;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

#define NDS32_PUT_FS_REG(NUM, NAME) \
  (fs->regs.how[NUM] = REG_SAVED_OFFSET, \
   fs->regs.reg[NUM].loc.offset = (_Unwind_Ptr) &(sc_->NAME) - new_cfa)

  /* Restore all registers value.  */
  NDS32_PUT_FS_REG (0, nds32_r0);
  NDS32_PUT_FS_REG (1, nds32_r1);
  NDS32_PUT_FS_REG (2, nds32_r2);
  NDS32_PUT_FS_REG (3, nds32_r3);
  NDS32_PUT_FS_REG (4, nds32_r4);
  NDS32_PUT_FS_REG (5, nds32_r5);
  NDS32_PUT_FS_REG (6, nds32_r6);
  NDS32_PUT_FS_REG (7, nds32_r7);
  NDS32_PUT_FS_REG (8, nds32_r8);
  NDS32_PUT_FS_REG (9, nds32_r9);
  NDS32_PUT_FS_REG (10, nds32_r10);
  NDS32_PUT_FS_REG (11, nds32_r11);
  NDS32_PUT_FS_REG (12, nds32_r12);
  NDS32_PUT_FS_REG (13, nds32_r13);
  NDS32_PUT_FS_REG (14, nds32_r14);
  NDS32_PUT_FS_REG (15, nds32_r15);
  NDS32_PUT_FS_REG (16, nds32_r16);
  NDS32_PUT_FS_REG (17, nds32_r17);
  NDS32_PUT_FS_REG (18, nds32_r18);
  NDS32_PUT_FS_REG (19, nds32_r19);
  NDS32_PUT_FS_REG (20, nds32_r20);
  NDS32_PUT_FS_REG (21, nds32_r21);
  NDS32_PUT_FS_REG (22, nds32_r22);
  NDS32_PUT_FS_REG (23, nds32_r23);
  NDS32_PUT_FS_REG (24, nds32_r24);
  NDS32_PUT_FS_REG (25, nds32_r25);

  NDS32_PUT_FS_REG (28, nds32_fp);
  NDS32_PUT_FS_REG (29, nds32_gp);
  NDS32_PUT_FS_REG (30, nds32_lp);
  NDS32_PUT_FS_REG (31, nds32_sp);

  /* Restore PC, point to trigger signal instruction.  */
  NDS32_PUT_FS_REG (32, nds32_ipc);

#undef NDS32_PUT_FS_REG

  /* The retaddr is PC, use PC to find FDE.  */
  fs->retaddr_column = 32;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif
