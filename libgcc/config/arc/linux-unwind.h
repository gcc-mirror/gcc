/* DWARF2 EH unwinding support for ARC Linux.
   Copyright (C) 2017-2021 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License
   and a copy of the GCC Runtime Library Exception along with this
   program; see the files COPYING3 and COPYING.RUNTIME respectively.
   If not, see <http://www.gnu.org/licenses/>.  */

/* This order is defined by a structure in the kernel, in file
   arch/arc/kernel/signal.c.  */

#define REGISTER_IN_STACK(REG_NAME, ID) \
  REG_NAME,
enum registers_stack_order {
  REGISTER_STACK_ORDER_START = -1,
  #include "config/arc/linux-unwind-reg.def"
  REGISTER_STACK_ORDER_SIZE,
};

struct register_position {
  int reg_id;
  int offset_in_stack;
};
#undef REGISTER_IN_STACK

#define REGISTER_SIZE_IN_WORDS 4
#define REGISTER_IN_STACK(REG_NAME, ID) (int) ID,
int
register_id_for_index[REGISTER_STACK_ORDER_SIZE] = {
  #include "config/arc/linux-unwind-reg.def"
};
#undef REGISTER_IN_STACK

#ifndef inhibit_libc
/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#include <signal.h>
#include <asm/unistd.h>

/*
00010edc <__default_rt_sa_restorer>:
   10edc:	208a 12c2		mov     r8,139
   10ee0:	781e			trap_s  0
   10ee2:	7ee0			j_s     [blink]
*/

#if __BIG_ENDIAN__
#define MOV_R8_139	  0x8a20c212
#define TRAP_S_J_S_BLINK  0x1e78e07e
#define SWI		  0x6f223f00
#elif __LITTLE_ENDIAN__
#define MOV_R8_139	  0x12c2208a
#define TRAP_S_J_S_BLINK  0x7ee0781e
#define SWI		  0x003f226f
#endif

#define MD_FALLBACK_FRAME_STATE_FOR arc_fallback_frame_state

static __attribute__((noinline)) _Unwind_Reason_Code
arc_fallback_frame_state (struct _Unwind_Context *context,
			   _Unwind_FrameState *fs)
{
  struct rt_sigframe {
    siginfo_t info;
    ucontext_t uc;
    unsigned int sigret_magic;
  };

  struct rt_sigframe *rt_;
  u_int32_t *pc = (u_int32_t *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa;
  int i;

#ifdef __ARC700__
  if (pc[1] != SWI)
    return _URC_END_OF_STACK;
#else
  if (pc[1] != TRAP_S_J_S_BLINK)
    return _URC_END_OF_STACK;
#endif

  if (pc[0] == MOV_R8_139)
    {
      rt_ = context->cfa;
      sc = &rt_->uc.uc_mcontext;
    }
  else
    return _URC_END_OF_STACK;

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  unsigned long *regs = &sc->regs.scratch.bta;
  for (i = 0; i < REGISTER_STACK_ORDER_SIZE; i++)
    {
      if (register_id_for_index[i] == -1)
	continue;
      fs->regs.reg[register_id_for_index[i]].how = REG_SAVED_OFFSET;
      fs->regs.reg[register_id_for_index[i]].loc.offset
	= ((_Unwind_Ptr) &(regs[i])) - new_cfa;
    }

  fs->regs.reg[31].how = REG_SAVED_VAL_OFFSET;
  fs->regs.reg[31].loc.offset = ((_Unwind_Ptr) (regs[ret])) - new_cfa;

  fs->retaddr_column = 31;

  return _URC_NO_REASON;
}
#endif

#define MD_FROB_UPDATE_CONTEXT arc_frob_update_context
/* Save fp register for unwinding to work.  */

static void
arc_frob_update_context (struct _Unwind_Context *context,
			 _Unwind_FrameState *fs)
{
  _Unwind_Word fp_val;
  asm ("mov %0,fp" : "=r" (fp_val));

  switch (fs->regs.reg[27].how)
    {
    case REG_UNSAVED:
    case REG_UNDEFINED:
      if (context->reg[27] == NULL)
	_Unwind_SetGRValue (context, 27, fp_val);
      break;

    default:
      break;
    }
}
