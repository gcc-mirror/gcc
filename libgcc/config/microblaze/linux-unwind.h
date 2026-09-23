/* DWARF2 EH unwinding support for MicroBlaze Linux.
   Copyright (C) 2026 Free Software Foundation, Inc.

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

/* Do code reading to identify a signal frame, and set the frame state
   data appropriately.  See unwind-dw2.c for the structs.  */

#include <signal.h>
#include <sys/ucontext.h>
#include <asm/unistd.h>

#define MD_FALLBACK_FRAME_STATE_FOR microblaze_fallback_frame_state

static _Unwind_Reason_Code
microblaze_fallback_frame_state (struct _Unwind_Context *context,
				 _Unwind_FrameState *fs)
{
  const unsigned int *pc = (const unsigned int *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa;
  int i;

  /* The outermost frame of a thread may leave a null or near-null
     return address; do not dereference it looking for the
     trampoline.  */
  if ((unsigned long) pc < 4096)
    return _URC_END_OF_STACK;

  /* The kernel writes the signal trampoline onto the stack
     (struct rt_sigframe.tramp):

	addik r12, r0, __NR_rt_sigreturn
	brki  r14, 0x8

     and sets the saved r15 to the trampoline address minus 8 (the
     handler returns with "rtsd r15, 8"), so the unwound return
     address points 8 bytes before the trampoline.  */
  if (pc[0] == (0x31800000 | __NR_rt_sigreturn) && pc[1] == 0xb9cc0008)
    ;
  else if (pc[2] == (0x31800000 | __NR_rt_sigreturn) && pc[3] == 0xb9cc0008)
    pc += 2;
  else
    return _URC_END_OF_STACK;

  /* The trampoline is the last member of the kernel's rt_sigframe and
     the ucontext sits directly in front of it.  Anchor there rather
     than at the CFA so the layout of the frame head does not matter
     (the kernel may insert an ABI argument-home gap at the front).
     uClibc's ucontext_t matches the kernel's struct ucontext.  */
  ucontext_t *uc = (ucontext_t *) ((_Unwind_Ptr) pc - sizeof (ucontext_t));

  sc = (struct sigcontext *) &uc->uc_mcontext;

  new_cfa = sc->regs.r1;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = 1;		/* r1, the stack pointer.  */
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  /* pt_regs holds r0..r31 consecutively.  */
  for (i = 0; i < 32; i++)
    {
      fs->regs.how[i] = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= (_Unwind_Ptr) &sc->regs.r0 + i * sizeof (unsigned long) - new_cfa;
    }

  /* The interrupted PC goes into the alternate return column
     (DWARF_ALT_FRAME_RETURN_COLUMN, defined as 36 in
     gcc/config/microblaze/microblaze.h), one past the hard registers;
     column 15 above keeps the interrupted r15.  */
  fs->regs.how[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__] = REG_SAVED_OFFSET;
  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].loc.offset
    = (_Unwind_Ptr) &sc->regs.pc - new_cfa;
  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif
