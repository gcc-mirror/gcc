/* Copyright (C) 2017-2022 Free Software Foundation, Inc.
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
#define MD_FROB_UPDATE_CONTEXT(context, fs) \
  aarch64_frob_update_context (context, fs)

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
	  char *aug_str = cie->augmentation;
	  return strchr (aug_str, 'B') == NULL ? 0 : 1;
	}
    }
  return 0;
}

/* Do AArch64 private extraction on ADDR_WORD based on context info CONTEXT and
   unwind frame info FS.  If ADDR_WORD is signed, we do address authentication
   on it using CFA of current frame.  */

static inline void *
aarch64_demangle_return_addr (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs ATTRIBUTE_UNUSED,
			      _Unwind_Word addr_word)
{
  void *addr = (void *)addr_word;
  if (context->flags & RA_SIGNED_BIT)
    {
      _Unwind_Word salt = (_Unwind_Word) context->cfa;
      if (aarch64_cie_signed_with_b_key (context) != 0)
	return __builtin_aarch64_autib1716 (addr, salt);
      return __builtin_aarch64_autia1716 (addr, salt);
    }
  else
    return addr;
}

/* Do AArch64 private initialization on CONTEXT based on frame info FS.  Mark
   CONTEXT as return address signed if bit 0 of DWARF_REGNUM_AARCH64_RA_STATE is
   set.  */

static inline void
aarch64_frob_update_context (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  const int reg = DWARF_REGNUM_AARCH64_RA_STATE;
  int ra_signed;
  if (fs->regs.how[reg] == REG_UNSAVED)
    ra_signed = fs->regs.reg[reg].loc.offset & 0x1;
  else
    ra_signed = _Unwind_GetGR (context, reg) & 0x1;
  if (ra_signed)
    /* The flag is used for re-authenticating EH handler's address.  */
    context->flags |= RA_SIGNED_BIT;
  else
    context->flags &= ~RA_SIGNED_BIT;

  return;
}

#endif /* defined AARCH64_UNWIND_H && defined __ILP32__ */
