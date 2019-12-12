/* Copyright (C) 2009-2019 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

/* Always include AArch64 unwinder header file.  */
#include "config/aarch64/aarch64-unwind.h"

#ifndef inhibit_libc

#include <signal.h>
#include <sys/ucontext.h>


/* Since insns are always stored LE, on a BE system the opcodes will
   be loaded byte-reversed.  Therefore, define two sets of opcodes,
   one for LE and one for BE.  */

#if __AARCH64EB__
#define MOVZ_X8_8B	0x681180d2
#define SVC_0		0x010000d4
#else
#define MOVZ_X8_8B	0xd2801168
#define SVC_0		0xd4000001
#endif

#define MD_FALLBACK_FRAME_STATE_FOR aarch64_fallback_frame_state

static _Unwind_Reason_Code
aarch64_fallback_frame_state (struct _Unwind_Context *context,
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
  unsigned *pc = context->ra;
  struct sigcontext *sc;
  struct _aarch64_ctx *extension_marker;
  int i;

  /* A signal frame will have a return address pointing to
     __default_sa_restorer. This code is hardwired as:

     0xd2801168         movz x8, #0x8b
     0xd4000001         svc  0x0
   */
  if (pc[0] != MOVZ_X8_8B || pc[1] != SVC_0)
    {
      return _URC_END_OF_STACK;
    }

  rt_ = context->cfa;
  sc = &rt_->uc.uc_mcontext;

/* This define duplicates the definition in aarch64.md */
#define SP_REGNUM 31

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (i = 0; i < AARCH64_DWARF_NUMBER_R; i++)
    {
      fs->regs.reg[AARCH64_DWARF_R0 + i].how = REG_SAVED_OFFSET;
      fs->regs.reg[AARCH64_DWARF_R0 + i].loc.offset =
	(_Unwind_Ptr) & (sc->regs[i]) - new_cfa;
    }

  /* The core context may be extended with an arbitrary set of
     additional contexts appended sequentially. Each additional
     context contains a magic identifier and size in bytes.  The size
     field can be used to skip over unrecognized context extensions.
     The end of the context sequence is marked by a context with magic
     0 or size 0.  */
  for (extension_marker = (struct _aarch64_ctx *) &sc->__reserved;
       extension_marker->magic;
       extension_marker = (struct _aarch64_ctx *)
       ((unsigned char *) extension_marker + extension_marker->size))
    {
      if (extension_marker->magic == FPSIMD_MAGIC)
	{
	  struct fpsimd_context *ctx =
	    (struct fpsimd_context *) extension_marker;
	  int i;

	  for (i = 0; i < AARCH64_DWARF_NUMBER_V; i++)
	    {
	      _Unwind_Sword offset;

	      fs->regs.reg[AARCH64_DWARF_V0 + i].how = REG_SAVED_OFFSET;

	      /* sigcontext contains 32 128bit registers for V0 to
		 V31.  The kernel will have saved the contents of the
		 V registers.  We want to unwind the callee save D
		 registers.  Each D register comprises the least
		 significant half of the corresponding V register.  We
		 need to offset into the saved V register dependent on
		 our endianness to find the saved D register.  */

	      offset = (_Unwind_Ptr) & (ctx->vregs[i]) - new_cfa;

	      /* The endianness adjustment code below expects that a
		 saved V register is 16 bytes.  */
	      gcc_assert (sizeof (ctx->vregs[0]) == 16);
#if defined (__AARCH64EB__)
	      offset = offset + 8;
#endif
	      fs->regs.reg[AARCH64_DWARF_V0 + i].loc.offset = offset;
	    }
	}
      else
	{
	  /* There is context provided that we do not recognize!  */
	}
    }

  fs->regs.reg[31].how = REG_SAVED_OFFSET;
  fs->regs.reg[31].loc.offset = (_Unwind_Ptr) & (sc->sp) - new_cfa;

  fs->signal_frame = 1;

  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].how =
    REG_SAVED_VAL_OFFSET;
  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].loc.offset =
    (_Unwind_Ptr) (sc->pc) - new_cfa;

  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;

  return _URC_NO_REASON;
}

#endif
