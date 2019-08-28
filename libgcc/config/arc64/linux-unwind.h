/* DWARF2 EH unwinding support for ARC64 Linux.
   Copyright (C) 2021 Free Software Foundation, Inc.

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

#ifndef inhibit_libc
/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#include <signal.h>
#include <asm/unistd.h>

/* The corresponding index in "reg_offset_map".  */
enum reg_id {
  REG_RET   = 5
};

#define SKIP (-1)

/* This order is defined by a structure in the kernel, in file
   arch/arc/kernel/signal.c.  */

const int
reg_offset_map[] = {
  SKIP,	/* bta	    */
  SKIP,	/* lp_start */
  SKIP,	/* lp_end   */
  SKIP,	/* lp_count */
  SKIP,	/* status32 */
  SKIP,	/* ret	    */
  31,	/* blink    */
  27,	/* fp	    */
  26,	/* gp	    */
  12,	/* r12	    */
  11,	/* r11	    */
  10,	/* r10	    */
  9,	/* r9	    */
  8,	/* r8	    */
  7,	/* r7	    */
  6,	/* r6	    */
  5,	/* r5	    */
  4,	/* r4	    */
  3,	/* r3	    */
  2,	/* r2	    */
  1,	/* r1	    */
  0,	/* r0	    */
  28	/* sp	    */
};

const size_t
reg_offset_map_size = sizeof (reg_offset_map) / sizeof (reg_offset_map[0]);

#define MOV_R8_139	  0x12c2208a
#define TRAP_S_0	  0x781e
#define J_S_BLINK	  0x7ee0

#define MD_FALLBACK_FRAME_STATE_FOR arc_fallback_frame_state

static __attribute__((noinline)) _Unwind_Reason_Code
arc_fallback_frame_state (struct _Unwind_Context *context,
			  _Unwind_FrameState *fs)
{
  /* The kernel creates an rt_sigframe on the stack immediately prior
     to delivering a signal.

     This structure must have the same shape as the linux kernel
     equivalent.  */
  struct rt_sigframe {
    siginfo_t info;
    ucontext_t uc;
    unsigned int sigret_magic;
  };

  struct rt_sigframe *rt_;
  u_int16_t *pc = (u_int16_t *) context->ra;
  struct sigcontext *sc;
  _Unwind_Ptr new_cfa;
  size_t i;


  /* A signal frame will have a return address pointing to
     __default_sa_restorer. This code is hardwired as:

  <__default_rt_sa_restorer>:
     208a 12c2           	mov	r8,139
     781e                	trap_s	0
     7ee0                	j_s	[blink]
  */
  if (pc[0] != (u_int16_t)MOV_R8_139 || pc[1] != (u_int16_t)(MOV_R8_139 >> 16)
      || pc[2] != TRAP_S_0 || pc[3] != J_S_BLINK)
    return _URC_END_OF_STACK;

  rt_ = context->cfa;
  sc = (struct sigcontext *)&rt_->uc.uc_mcontext;

  new_cfa = (_Unwind_Ptr)sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr)context->cfa;

  unsigned long *regs = &sc->regs.scratch.bta;
  for (i = 0; i < reg_offset_map_size; ++i)
    {
      if (reg_offset_map[i] == SKIP)
	continue;
      fs->regs.reg[reg_offset_map[i]].how = REG_SAVED_OFFSET;
      fs->regs.reg[reg_offset_map[i]].loc.offset
	= ((_Unwind_Ptr)&(regs[i])) - new_cfa;
    }

  fs->signal_frame = 1;
  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;
  fs->regs.reg[fs->retaddr_column].how = REG_SAVED_VAL_OFFSET;
  fs->regs.reg[fs->retaddr_column].loc.offset =
    ((_Unwind_Ptr) (regs[REG_RET])) - new_cfa;

  return _URC_NO_REASON;
}

#endif	  /* ifndef inhibit_libc */

/* TODO: There was once an arc_frob_update_context () dwelling here.
   Check if it is still needed. "cleanup" tests are fine without it.
   glibc tests (nptl/tst-* and debug/tst-backtrace*) should shed more
   light on it.  */
