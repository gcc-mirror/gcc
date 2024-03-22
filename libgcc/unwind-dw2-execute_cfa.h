/* DWARF2 exception handling CFA execution engine.
   Copyright (C) 1997-2024 Free Software Foundation, Inc.

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

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* This file is included from unwind-dw2.c to specialize the code for certain
   values of DATA_ALIGN and CODE_ALIGN.  These macros must be defined prior to
   including this file.  */

{
  struct frame_state_reg_info *unused_rs = NULL;

  /* Don't allow remember/restore between CIE and FDE programs.  */
  fs->regs.prev = NULL;

  /* The comparison with the return address uses < rather than <= because
     we are only interested in the effects of code before the call; for a
     noreturn function, the return address may point to unrelated code with
     a different stack configuration that we are not interested in.  We
     assume that the call itself is unwind info-neutral; if not, or if
     there are delay instructions that adjust the stack, these must be
     reflected at the point immediately before the call insn.
     In signal frames, return address is after last completed instruction,
     so we add 1 to return address to make the comparison <=.  */
  while (insn_ptr < insn_end
	 && fs->pc < context->ra + _Unwind_IsSignalFrame (context))
    {
      unsigned char insn = *insn_ptr++;
      _uleb128_t reg, utmp;
      _sleb128_t offset, stmp;

      if ((insn & 0xc0) == DW_CFA_advance_loc)
	fs->pc += (insn & 0x3f) * CODE_ALIGN;
      else if ((insn & 0xc0) == DW_CFA_offset)
	{
	  reg = insn & 0x3f;
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Sword) utmp * DATA_ALIGN;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_OFFSET;
	      fs->regs.reg[reg].loc.offset = offset;
	    }
	}
      else if ((insn & 0xc0) == DW_CFA_restore)
	{
	  reg = insn & 0x3f;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    fs->regs.how[reg] = REG_UNSAVED;
	}
      else switch (insn)
	{
	case DW_CFA_set_loc:
	  {
	    _Unwind_Ptr pc;

	    insn_ptr = read_encoded_value (context, fs->fde_encoding,
					   insn_ptr, &pc);
	    fs->pc = (void *) pc;
	  }
	  break;

	case DW_CFA_advance_loc1:
	  fs->pc += read_1u (insn_ptr) * CODE_ALIGN;
	  insn_ptr += 1;
	  break;
	case DW_CFA_advance_loc2:
	  fs->pc += read_2u (insn_ptr) * CODE_ALIGN;
	  insn_ptr += 2;
	  break;
	case DW_CFA_advance_loc4:
	  fs->pc += read_4u (insn_ptr) * CODE_ALIGN;
	  insn_ptr += 4;
	  break;

	case DW_CFA_offset_extended:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Sword) utmp * DATA_ALIGN;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_OFFSET;
	      fs->regs.reg[reg].loc.offset = offset;
	    }
	  break;

	case DW_CFA_restore_extended:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  /* FIXME, this is wrong; the CIE might have said that the
	     register was saved somewhere.  */
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    fs->regs.how[reg] = REG_UNSAVED;
	  break;

	case DW_CFA_same_value:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    fs->regs.how[reg] = REG_UNSAVED;
	  break;

	case DW_CFA_undefined:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    fs->regs.how[reg] = REG_UNDEFINED;
	  break;

	case DW_CFA_nop:
	  break;

	case DW_CFA_register:
	  {
	    _uleb128_t reg2;
	    insn_ptr = read_uleb128 (insn_ptr, &reg);
	    insn_ptr = read_uleb128 (insn_ptr, &reg2);
	    reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	    if (UNWIND_COLUMN_IN_RANGE (reg))
	      {
		fs->regs.how[reg] = REG_SAVED_REG;
	        fs->regs.reg[reg].loc.reg = (_Unwind_Word)reg2;
	      }
	  }
	  break;

	case DW_CFA_remember_state:
	  {
	    struct frame_state_reg_info *new_rs;
	    if (unused_rs)
	      {
		new_rs = unused_rs;
		unused_rs = unused_rs->prev;
	      }
	    else
	      new_rs = alloca (sizeof (struct frame_state_reg_info));

	    *new_rs = fs->regs;
	    fs->regs.prev = new_rs;
	  }
	  break;

	case DW_CFA_restore_state:
	  {
	    struct frame_state_reg_info *old_rs = fs->regs.prev;
	    fs->regs = *old_rs;
	    old_rs->prev = unused_rs;
	    unused_rs = old_rs;
	  }
	  break;

	case DW_CFA_def_cfa:
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  fs->regs.cfa_reg = (_Unwind_Word)utmp;
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  fs->regs.cfa_offset = (_Unwind_Word)utmp;
	  fs->regs.cfa_how = CFA_REG_OFFSET;
	  break;

	case DW_CFA_def_cfa_register:
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  fs->regs.cfa_reg = (_Unwind_Word)utmp;
	  fs->regs.cfa_how = CFA_REG_OFFSET;
	  break;

	case DW_CFA_def_cfa_offset:
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  fs->regs.cfa_offset = utmp;
	  /* cfa_how deliberately not set.  */
	  break;

	case DW_CFA_def_cfa_expression:
	  fs->regs.cfa_exp = insn_ptr;
	  fs->regs.cfa_how = CFA_EXP;
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  insn_ptr += utmp;
	  break;

	case DW_CFA_expression:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_EXP;
	      fs->regs.reg[reg].loc.exp = insn_ptr;
	    }
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  insn_ptr += utmp;
	  break;

	  /* Dwarf3.  */
	case DW_CFA_offset_extended_sf:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_sleb128 (insn_ptr, &stmp);
	  offset = stmp * DATA_ALIGN;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_OFFSET;
	      fs->regs.reg[reg].loc.offset = offset;
	    }
	  break;

	case DW_CFA_def_cfa_sf:
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  fs->regs.cfa_reg = (_Unwind_Word)utmp;
	  insn_ptr = read_sleb128 (insn_ptr, &stmp);
	  fs->regs.cfa_offset = (_Unwind_Sword)stmp;
	  fs->regs.cfa_how = CFA_REG_OFFSET;
	  fs->regs.cfa_offset *= DATA_ALIGN;
	  break;

	case DW_CFA_def_cfa_offset_sf:
	  insn_ptr = read_sleb128 (insn_ptr, &stmp);
	  fs->regs.cfa_offset = (_Unwind_Sword)stmp;
	  fs->regs.cfa_offset *= DATA_ALIGN;
	  /* cfa_how deliberately not set.  */
	  break;

	case DW_CFA_val_offset:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Sword) utmp * DATA_ALIGN;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_VAL_OFFSET;
	      fs->regs.reg[reg].loc.offset = offset;
	    }
	  break;

	case DW_CFA_val_offset_sf:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_sleb128 (insn_ptr, &stmp);
	  offset = stmp * DATA_ALIGN;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_VAL_OFFSET;
	      fs->regs.reg[reg].loc.offset = offset;
	    }
	  break;

	case DW_CFA_val_expression:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_VAL_EXP;
	      fs->regs.reg[reg].loc.exp = insn_ptr;
	    }
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  insn_ptr += utmp;
	  break;

	case DW_CFA_GNU_window_save:
#if defined (__aarch64__) && !defined (__ILP32__)
	  /* This CFA is multiplexed with Sparc.  On AArch64 it's used to toggle
	     return address signing status.  REG_UNSAVED/REG_UNSAVED_ARCHEXT
	     mean RA signing is disabled/enabled.  */
	  reg = DWARF_REGNUM_AARCH64_RA_STATE;
	  gcc_assert (fs->regs.how[reg] == REG_UNSAVED
		      || fs->regs.how[reg] == REG_UNSAVED_ARCHEXT);
	  if (fs->regs.how[reg] == REG_UNSAVED)
	    fs->regs.how[reg] = REG_UNSAVED_ARCHEXT;
	  else
	    fs->regs.how[reg] = REG_UNSAVED;
#else
	  /* ??? Hardcoded for SPARC register window configuration.  */
	  if (__LIBGCC_DWARF_FRAME_REGISTERS__ >= 32)
	    for (reg = 16; reg < 32; ++reg)
	      {
		fs->regs.how[reg] = REG_SAVED_OFFSET;
		fs->regs.reg[reg].loc.offset = (reg - 16) * sizeof (void *);
	      }
#endif
	  break;

	case DW_CFA_GNU_args_size:
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  context->args_size = (_Unwind_Word)utmp;
	  break;

	case DW_CFA_GNU_negative_offset_extended:
	  /* Obsoleted by DW_CFA_offset_extended_sf, but used by
	     older PowerPC code.  */
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Word) utmp * DATA_ALIGN;
	  reg = DWARF_REG_TO_UNWIND_COLUMN (reg);
	  if (UNWIND_COLUMN_IN_RANGE (reg))
	    {
	      fs->regs.how[reg] = REG_SAVED_OFFSET;
	      fs->regs.reg[reg].loc.offset = -offset;
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

#undef DATA_ALIGN
#undef CODE_ALIGN
