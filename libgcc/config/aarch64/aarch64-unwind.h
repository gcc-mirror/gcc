/* Copyright (C) 2017 Free Software Foundation, Inc.
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

#define MD_POST_EXTRACT_ROOT_ADDR(addr)  __builtin_aarch64_xpaclri (addr)
#define MD_POST_EXTRACT_FRAME_ADDR(context, fs, addr) \
  aarch64_post_extract_frame_addr (context, fs, addr)
#define MD_POST_FROB_EH_HANDLER_ADDR(current, target, addr) \
  aarch64_post_frob_eh_handler_addr (current, target, addr)
#define MD_FROB_UPDATE_CONTEXT(context, fs) \
  aarch64_frob_update_context (context, fs)

/* Do AArch64 private extraction on ADDR based on context info CONTEXT and
   unwind frame info FS.  If ADDR is signed, we do address authentication on it
   using CFA of current frame.  */

static inline void *
aarch64_post_extract_frame_addr (struct _Unwind_Context *context,
				 _Unwind_FrameState *fs, void *addr)
{
  if (fs->regs.reg[DWARF_REGNUM_AARCH64_RA_STATE].loc.offset & 0x1)
    {
      _Unwind_Word salt = (_Unwind_Word) context->cfa;
      return __builtin_aarch64_autia1716 (addr, salt);
    }
  else
    return addr;
}

/* Do AArch64 private frob on exception handler's address HANDLER_ADDR before
   installing it into current context CURRENT.  TARGET is currently not used.
   We need to sign exception handler's address if CURRENT itself is signed.  */

static inline void *
aarch64_post_frob_eh_handler_addr (struct _Unwind_Context *current,
				   struct _Unwind_Context *target
				   ATTRIBUTE_UNUSED,
				   void *handler_addr)
{
  if (current->flags & RA_A_SIGNED_BIT)
    return __builtin_aarch64_pacia1716 (handler_addr,
					(_Unwind_Word) current->cfa);
  else
    return handler_addr;
}

/* Do AArch64 private initialization on CONTEXT based on frame info FS.  Mark
   CONTEXT as return address signed if bit 0 of DWARF_REGNUM_AARCH64_RA_STATE is
   set.  */

static inline void
aarch64_frob_update_context (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  if (fs->regs.reg[DWARF_REGNUM_AARCH64_RA_STATE].loc.offset & 0x1)
    /* The flag is used for re-authenticating EH handler's address.  */
    context->flags |= RA_A_SIGNED_BIT;

  return;
}

#endif /* defined AARCH64_UNWIND_H && defined __ILP32__ */
