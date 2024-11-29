/* Copyright (C) 2017-2024 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#if !defined (AARCH64_UNWIND_H) && !defined (__ILP32__)
#define AARCH64_UNWIND_H

#include "config/aarch64/aarch64-unwind-def.h"

#include "ansidecl.h"
#include <stdbool.h>

#define AARCH64_DWARF_REGNUM_RA_STATE 34
#define AARCH64_DWARF_RA_STATE_MASK   0x1

/* The diversifiers used to sign a function's return address.  */
typedef enum
{
  aarch64_ra_no_signing = 0x0,
  aarch64_ra_signing_sp = 0x1,
} __attribute__((packed)) aarch64_ra_signing_method_t;

#define MD_ARCH_EXTENSION_CIE_AUG_HANDLER(fs, aug) \
  aarch64_cie_aug_handler (fs, aug)

#define MD_ARCH_EXTENSION_FRAME_INIT(context, fs) \
  aarch64_arch_extension_frame_init (context, fs)

#define MD_DEMANGLE_RETURN_ADDR(context, fs, addr) \
  aarch64_demangle_return_addr (context, fs, addr)

#define MD_FRAME_LOCAL_REGISTER_P(reg) \
  aarch64_frame_local_register (reg)

static inline aarch64_ra_signing_method_t
aarch64_context_ra_state_get (struct _Unwind_Context *context)
{
  const int index = AARCH64_DWARF_REGNUM_RA_STATE;
  return _Unwind_GetGR (context, index) & AARCH64_DWARF_RA_STATE_MASK;
}

static inline aarch64_ra_signing_method_t
aarch64_fs_ra_state_get (_Unwind_FrameState const *fs)
{
  const int index = AARCH64_DWARF_REGNUM_RA_STATE;
  return fs->regs.reg[index].loc.offset & AARCH64_DWARF_RA_STATE_MASK;
}

static inline void
aarch64_fs_ra_state_set (_Unwind_FrameState *fs,
			aarch64_ra_signing_method_t signing_method)
{
  fs->regs.reg[AARCH64_DWARF_REGNUM_RA_STATE].loc.offset = signing_method;
}

static inline void
aarch64_fs_ra_state_toggle (_Unwind_FrameState *fs)
{
  /* /!\ Mixing DW_CFA_val_expression with DW_CFA_AARCH64_negate_ra_state will
     result in undefined behavior (likely an unwinding failure), as the
     chronology of the DWARF directives will be broken.  */
  gcc_assert (fs->regs.how[AARCH64_DWARF_REGNUM_RA_STATE] == REG_ARCHEXT);

  aarch64_ra_signing_method_t signing_method = aarch64_fs_ra_state_get (fs);
  gcc_assert (signing_method == aarch64_ra_no_signing
	      || signing_method == aarch64_ra_signing_sp);
  aarch64_fs_ra_state_set (fs, (signing_method == aarch64_ra_no_signing)
			  ? aarch64_ra_signing_sp
			  : aarch64_ra_no_signing);
}

/* CIE handler for custom augmentation string.  */
static inline bool
aarch64_cie_aug_handler (_Unwind_FrameState *fs, unsigned char aug)
{
  /* AArch64 B-key pointer authentication.  */
  if (aug == 'B')
    {
      fs->regs.arch_fs.signing_key = AARCH64_PAUTH_KEY_B;
      return true;
    }
  return false;
}

/* At the entrance of a new frame, some cached information from the CIE/FDE,
   and registers values related to architectural extensions require a default
   initialization.
   If any of those values related to architecture extensions had to be saved
   for the next frame, it should be done via the architecture extensions handler
   MD_FROB_UPDATE_CONTEXT in uw_update_context_1 (libgcc/unwind-dw2.c).  */
static inline void
aarch64_arch_extension_frame_init (struct _Unwind_Context *context ATTRIBUTE_UNUSED,
				   _Unwind_FrameState *fs)
{
  /* By default, DW_CFA_AARCH64_negate_ra_state assumes key A is being used
     for signing.  This can be overridden by adding 'B' to the augmentation
     string.  */
  fs->regs.arch_fs.signing_key = AARCH64_PAUTH_KEY_A;

  /* All registers are initially in state REG_UNSAVED, which indicates that
     they inherit register values from the previous frame.  However, the
     return address starts every frame in the "unsigned" state.  It also
     starts every frame in a state that supports the original toggle-based
     DW_CFA_AARCH64_negate_ra_state method of controlling RA signing.  */
  fs->regs.how[AARCH64_DWARF_REGNUM_RA_STATE] = REG_ARCHEXT;
  aarch64_fs_ra_state_set (fs, aarch64_ra_no_signing);
}

/* Before copying the current context to the target context, check whether
   the register is local to this context and should not be forwarded.  */
static inline bool
aarch64_frame_local_register(long reg)
{
  return (reg == AARCH64_DWARF_REGNUM_RA_STATE);
}

/* Do AArch64 private extraction on ADDR_WORD based on context info CONTEXT and
   unwind frame info FS.  If ADDR_WORD is signed, we do address authentication
   on it using CFA of current frame.

   Note: when DW_CFA_val_expression is used, FS only records the location of the
   associated CFI program, rather than the value of the expression itself.
   The CFI program is executed by uw_update_context when updating the context,
   so the value of the expression must be taken from CONTEXT rather than FS.  */
static inline void *
aarch64_demangle_return_addr (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs,
			      _Unwind_Word addr_word)
{
  void *addr = (void *)addr_word;
  const int reg = AARCH64_DWARF_REGNUM_RA_STATE;

  /* In libgcc, REG_ARCHEXT means that the RA state register was set by an
     AArch64 DWARF instruction and contains a valid value, or is used to
     describe the initial state set in aarch64_arch_extension_frame_init.
     Return-address signing state is normally toggled by DW_CFA_AARCH64_negate
     _ra_state (also knwon by its alias as DW_CFA_GNU_window_save).
     However, RA state register can be set directly via DW_CFA_val_expression
     too.  GCC does not generate such CFI but some other compilers reportedly
     do (see PR104689 for more details).
     Any other value than REG_ARCHEXT should be interpreted as if the RA state
     register is set by another DWARF instruction, and the value is fetchable
     via _Unwind_GetGR.  */
  aarch64_ra_signing_method_t signing_method = aarch64_ra_no_signing;
  if (fs->regs.how[reg] == REG_ARCHEXT)
    signing_method = aarch64_fs_ra_state_get (fs);
  else if (fs->regs.how[reg] != REG_UNSAVED)
    signing_method = aarch64_context_ra_state_get (context);

  if (signing_method == aarch64_ra_signing_sp)
    {
      _Unwind_Word salt = (_Unwind_Word) context->cfa;
      if (fs->regs.arch_fs.signing_key == AARCH64_PAUTH_KEY_B)
	return __builtin_aarch64_autib1716 (addr, salt);
      return __builtin_aarch64_autia1716 (addr, salt);
    }

  return addr;
}

/* GCS enable flag for chkfeat instruction.  */
#define _CHKFEAT_GCS 1
/* SME runtime function local to libgcc, streaming compatible
   and preserves more registers than the base PCS requires, but
   we don't rely on that here.  */
__attribute__ ((visibility ("hidden")))
void __libgcc_arm_za_disable (void);

/* Disable the SME ZA state in case an unwound frame used the ZA
   lazy saving scheme. And unwind the GCS for EH.  */
#undef _Unwind_Frames_Extra
#define _Unwind_Frames_Extra(x)				\
  do							\
    {							\
      __libgcc_arm_za_disable ();			\
      if (__builtin_aarch64_chkfeat (_CHKFEAT_GCS) == 0)	\
	{						\
	  for (_Unwind_Word n = (x); n != 0; n--)	\
	    __builtin_aarch64_gcspopm ();		\
	}						\
    }							\
  while (0)

/* On signal entry the OS places a token on the GCS that can be used to
   verify the integrity of the GCS pointer on signal return.  It also
   places the signal handler return address (the restorer that calls the
   signal return syscall) on the GCS so the handler can return.
   Because of this token, each stack frame visited during unwinding has
   exactly one corresponding entry on the GCS, so the frame count is
   the number of entries that will have to be popped at EH return time.

   Note: This depends on the GCS signal ABI of the OS.

   When unwinding across a stack frame for each frame the corresponding
   entry is checked on the GCS against the computed return address from
   the normal stack.  If they don't match then _URC_FATAL_PHASE2_ERROR
   is returned.  This check is omitted if

   1. GCS is disabled. Note: asynchronous GCS disable is supported here
      if GCSPR and the GCS remains readable.
   2. Non-catchable exception where exception_class == 0.  Note: the
      pthread cancellation implementation in glibc sets exception_class
      to 0 when the unwinder is used for cancellation cleanup handling,
      so this allows the GCS to get out of sync during cancellation.
      This weakens security but avoids an ABI break in glibc.
   3. Zero return address which marks the outermost stack frame.
   4. Signal stack frame, the GCS entry is an OS specific token then
      with the top bit set.
 */
#undef _Unwind_Frames_Increment
#define _Unwind_Frames_Increment(exc, context, frames)	\
  do							\
    {							\
      frames++;						\
      if (__builtin_aarch64_chkfeat (_CHKFEAT_GCS) != 0	\
	  || exc->exception_class == 0			\
	  || _Unwind_GetIP (context) == 0)		\
	break;						\
      const _Unwind_Word *gcs = __builtin_aarch64_gcspr (); \
      if (_Unwind_IsSignalFrame (context))		\
	{						\
	  if (gcs[frames] >> 63 == 0)			\
	    return _URC_FATAL_PHASE2_ERROR;		\
	}						\
      else						\
	{						\
	  if (gcs[frames] != _Unwind_GetIP (context))	\
	    return _URC_FATAL_PHASE2_ERROR;		\
	}						\
    }							\
  while (0)

#endif /* defined AARCH64_UNWIND_H && defined __ILP32__ */
