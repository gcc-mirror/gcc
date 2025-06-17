/* Copyright (C) 2009-2025 Free Software Foundation, Inc.
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
#include <stdint.h>

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

#ifndef FPSIMD_MAGIC
#define FPSIMD_MAGIC 0x46508001
#endif

#ifndef TPIDR2_MAGIC
#define TPIDR2_MAGIC 0x54504902
#endif

#ifndef ZA_MAGIC
#define ZA_MAGIC 0x54366345
#endif

#ifndef EXTRA_MAGIC
#define EXTRA_MAGIC 0x45585401
#endif

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

  struct tpidr2_block
  {
    uint64_t za_save_buffer;
    uint16_t num_za_save_slices;
    uint8_t reserved[6];
  };

  struct za_block
  {
    struct _aarch64_ctx head;
    uint16_t vl;
    uint16_t reserved[3];
    uint64_t data;
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
  /* Historically, the uc_mcontext member was of type struct sigcontext, but
     glibc uses a different type now with member names in the implementation
     namespace.  */
  sc = (struct sigcontext *) &rt_->uc.uc_mcontext;

/* This define duplicates the definition in aarch64.md */
#define SP_REGNUM 31

  new_cfa = (_Unwind_Ptr) sc;
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __LIBGCC_STACK_POINTER_REGNUM__;
  fs->regs.cfa_offset = new_cfa - (_Unwind_Ptr) context->cfa;

  for (i = 0; i < AARCH64_DWARF_NUMBER_R; i++)
    {
      fs->regs.how[AARCH64_DWARF_R0 + i] = REG_SAVED_OFFSET;
      fs->regs.reg[AARCH64_DWARF_R0 + i].loc.offset =
	(_Unwind_Ptr) & (sc->regs[i]) - new_cfa;
    }

  /* The core context may be extended with an arbitrary set of
     additional contexts appended sequentially. Each additional
     context contains a magic identifier and size in bytes.  The size
     field can be used to skip over unrecognized context extensions.
     The end of the context sequence is marked by a context with magic
     0 or size 0.  */
  struct tpidr2_block *tpidr2 = 0;
  struct za_block *za_ctx = 0;

  for (extension_marker = (struct _aarch64_ctx *) &sc->__reserved;
       extension_marker->magic;
       extension_marker = (struct _aarch64_ctx *)
       ((unsigned char *) extension_marker + extension_marker->size))
    {
    restart:
      if (extension_marker->magic == FPSIMD_MAGIC)
	{
	  struct fpsimd_context *ctx =
	    (struct fpsimd_context *) extension_marker;
	  int i;

	  for (i = 0; i < AARCH64_DWARF_NUMBER_V; i++)
	    {
	      _Unwind_Sword offset;

	      fs->regs.how[AARCH64_DWARF_V0 + i] = REG_SAVED_OFFSET;

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
      else if (extension_marker->magic == TPIDR2_MAGIC)
	{
	  /* A TPIDR2 context.

	     All the casting is to support big-endian ILP32.  We could read
	     directly into TPIDR2 otherwise.  */
	  struct { struct _aarch64_ctx h; uint64_t tpidr2; } *ctx
		  = (void *)extension_marker;
#if defined (__ILP32__)
	  tpidr2 = (struct tpidr2_block *) (uintptr_t) ctx->tpidr2;
#else
	  tpidr2 = (struct tpidr2_block *) ctx->tpidr2;
#endif
	}
      else if (extension_marker->magic == ZA_MAGIC)
	/* A ZA context.  We interpret this later.  */
	za_ctx = (void *)extension_marker;
      else if (extension_marker->magic == EXTRA_MAGIC)
	{
	  /* Extra context.  The ABI guarantees that the next _aarch64_ctx
	     in the current list will be the zero terminator, so we can simply
	     switch to the new list and continue from there.  The new list is
	     also zero-terminated.

	     As above, the casting is to support big-endian ILP32.  */
	  struct { struct _aarch64_ctx h; uint64_t next; } *ctx
		  = (void *)extension_marker;
#if defined (__ILP32__)
	  extension_marker = (struct _aarch64_ctx *) (uintptr_t) ctx->next;
#else
	  extension_marker = (struct _aarch64_ctx *) ctx->next;
#endif
	  goto restart;
	}
      else
	{
	  /* There is context provided that we do not recognize!  */
	}
    }

  /* Signal handlers are entered with ZA in the off state (TPIDR2_ELO==0 and
     PSTATE.ZA==0).  The normal process when transitioning from ZA being
     dormant to ZA being off is to commit the lazy save; see the AAPCS64
     for details.  However, this is not done when entering a signal handler.
     Instead, linux saves the old contents of ZA and TPIDR2_EL0 to the
     sigcontext without interpreting them further.

     Therefore, if a signal handler throws an exception to code outside the
     signal handler, the unwinder must commit the lazy save after the fact.
     Committing a lazy save means:

     (1) Storing the contents of ZA into the buffer provided by TPIDR2_EL0.
     (2) Setting TPIDR2_EL0 to zero.
     (3) Turning ZA off.

     (2) and (3) have already been done by the call to __libgcc_arm_za_disable.
     (1) involves copying data from the ZA sigcontext entry to the
     corresponding lazy save buffer.  */
  if (tpidr2 && za_ctx && tpidr2->za_save_buffer)
    {
      /* There is a 16-bit vector length (measured in bytes) at ZA_CTX + 8.
	 The data itself starts at ZA_CTX + 16.
	 As above, the casting is to support big-endian ILP32.  */
      uint16_t vl = za_ctx->vl;
#if defined (__ILP32__)
      void *save_buffer = (void *) (uintptr_t) tpidr2->za_save_buffer;
      const void *za_buffer = (void *) (uintptr_t) &za_ctx->data;
#else
      void *save_buffer = (void *) tpidr2->za_save_buffer;
      const void *za_buffer = (void *) &za_ctx->data;
#endif
      uint64_t num_slices = tpidr2->num_za_save_slices;
      if (num_slices > vl)
	num_slices = vl;
      memcpy (save_buffer, za_buffer, num_slices * vl);
    }

  fs->regs.how[31] = REG_SAVED_OFFSET;
  fs->regs.reg[31].loc.offset = (_Unwind_Ptr) & (sc->sp) - new_cfa;

  fs->signal_frame = 1;

  fs->regs.how[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__] =
    REG_SAVED_VAL_OFFSET;
  fs->regs.reg[__LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__].loc.offset =
    (_Unwind_Ptr) (sc->pc) - new_cfa;

  fs->retaddr_column = __LIBGCC_DWARF_ALT_FRAME_RETURN_COLUMN__;

  return _URC_NO_REASON;
}

#endif
