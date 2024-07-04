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

#define DWARF_REGNUM_AARCH64_RA_STATE 34

#define MD_DEMANGLE_RETURN_ADDR(context, fs, addr) \
  aarch64_demangle_return_addr (context, fs, addr)

static inline int
aarch64_cie_signed_with_b_key (struct _Unwind_Context *context)
{
  const struct dwarf_fde *fde = _Unwind_Find_FDE (context->bases.func,
						  &context->bases);
  if (fde != NULL)
    {
      const struct dwarf_cie *cie = get_cie (fde);
      if (cie != NULL)
	{
	  const unsigned char *aug_str = cie->augmentation;
	  return __builtin_strchr ((const char *) aug_str,
				   'B') == NULL ? 0 : 1;
	}
    }
  return 0;
}

/* Do AArch64 private extraction on ADDR_WORD based on context info CONTEXT and
   unwind frame info FS.  If ADDR_WORD is signed, we do address authentication
   on it using CFA of current frame.  */

static inline void *
aarch64_demangle_return_addr (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs,
			      _Unwind_Word addr_word)
{
  void *addr = (void *)addr_word;
  const int reg = DWARF_REGNUM_AARCH64_RA_STATE;

  if (fs->regs.how[reg] == REG_UNSAVED)
    return addr;

  /* Return-address signing state is toggled by DW_CFA_GNU_window_save (where
     REG_UNSAVED/REG_UNSAVED_ARCHEXT means RA signing is disabled/enabled),
     or set by a DW_CFA_expression.  */
  if (fs->regs.how[reg] == REG_UNSAVED_ARCHEXT
      || (_Unwind_GetGR (context, reg) & 0x1) != 0)
    {
      _Unwind_Word salt = (_Unwind_Word) context->cfa;
      if (aarch64_cie_signed_with_b_key (context) != 0)
	return __builtin_aarch64_autib1716 (addr, salt);
      return __builtin_aarch64_autia1716 (addr, salt);
    }

  return addr;
}

/* SME runtime function local to libgcc, streaming compatible
   and preserves more registers than the base PCS requires, but
   we don't rely on that here.  */
__attribute__ ((visibility ("hidden")))
void __libgcc_arm_za_disable (void);

/* Disable the SME ZA state in case an unwound frame used the ZA
   lazy saving scheme.  */
#undef _Unwind_Frames_Extra
#define _Unwind_Frames_Extra(x)				\
  do							\
    {							\
      __libgcc_arm_za_disable ();			\
    }							\
  while (0)

#endif /* defined AARCH64_UNWIND_H && defined __ILP32__ */
