/* DWARF2 EH unwinding support for SPARC Linux.
   Copyright (C) 2004-2020 Free Software Foundation, Inc.

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

/* Do code reading to identify a signal frame, and set the frame
   state data appropriately.  See unwind-dw2.c for the structs.  */

#if defined(__arch64__)

#undef STACK_BIAS
#define STACK_BIAS 2047

/* 64-bit SPARC version */
#define MD_FALLBACK_FRAME_STATE_FOR sparc64_fallback_frame_state

static _Unwind_Reason_Code
sparc64_fallback_frame_state (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs)
{
  unsigned int *pc = context->ra;
  long this_cfa = (long) context->cfa;
  long new_cfa, ra_location, shifted_ra_location;
  long regs_off, fpu_save_off;
  long fpu_save;
  int i;

  if (pc[0] != 0x82102065	/* mov NR_rt_sigreturn, %g1 */
      || pc[1] != 0x91d0206d)	/* ta 0x6d */
    return _URC_END_OF_STACK;

  regs_off = 192 + 128;
  fpu_save_off = regs_off + (16 * 8) + (3 * 8) + (2 * 4);

  new_cfa = *(long *)(this_cfa + regs_off + (14 * 8));
  /* The frame address is %sp + STACK_BIAS in 64-bit mode.  */
  new_cfa += STACK_BIAS;
  fpu_save = *(long *)(this_cfa + fpu_save_off);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs->regs.cfa_offset = new_cfa - this_cfa;

  for (i = 1; i < 16; i++)
    {
      /* We never restore %sp as everything is purely CFA-based.  */
      if ((unsigned int) i == __builtin_dwarf_sp_column ())
	continue;

      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= this_cfa + regs_off + (i * 8) - new_cfa;
    }
  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i + 16].how = REG_SAVED_OFFSET;
      fs->regs.reg[i + 16].loc.offset
	= this_cfa + (i * 8) - new_cfa;
    }
  if (fpu_save)
    {
      for (i = 0; i < 64; i++)
	{
	  if (i > 32 && (i & 0x1))
	    continue;
	  fs->regs.reg[i + 32].how = REG_SAVED_OFFSET;
	  fs->regs.reg[i + 32].loc.offset
	    = fpu_save + (i * 4) - new_cfa;
	}
    }

  /* State the rules to find the kernel's code "return address", which is
     the address of the active instruction when the signal was caught.
     On the SPARC, since RETURN_ADDR_OFFSET (essentially 8) is defined, we
     need to preventively subtract it from the purported return address.  */
  ra_location = this_cfa + regs_off + 17 * 8;
  shifted_ra_location = this_cfa + regs_off + 19 * 8; /* Y register */
  *(long *)shifted_ra_location = *(long *)ra_location - 8;
  fs->retaddr_column = 0;
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = shifted_ra_location - new_cfa;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#define MD_FROB_UPDATE_CONTEXT sparc64_frob_update_context

static void
sparc64_frob_update_context (struct _Unwind_Context *context,
			     _Unwind_FrameState *fs)
{
  /* The column of %sp contains the old CFA, not the old value of %sp.
     The CFA offset already comprises the stack bias so, when %sp is the
     CFA register, we must avoid counting the stack bias twice.  Do not
     do that for signal frames as the offset is artificial for them.  */
  if (fs->regs.cfa_reg == __builtin_dwarf_sp_column ()
      && fs->regs.cfa_how == CFA_REG_OFFSET
      && fs->regs.cfa_offset != 0
      && !fs->signal_frame)
    {
      long i;

      context->cfa -= STACK_BIAS;

      for (i = 0; i < __LIBGCC_DWARF_FRAME_REGISTERS__ + 1; ++i)
	if (fs->regs.reg[i].how == REG_SAVED_OFFSET)
	  _Unwind_SetGRPtr (context, i,
			    _Unwind_GetGRPtr (context, i) - STACK_BIAS);
    }
}

#else

/* 32-bit SPARC version */
#define MD_FALLBACK_FRAME_STATE_FOR sparc_fallback_frame_state

static _Unwind_Reason_Code
sparc_fallback_frame_state (struct _Unwind_Context *context,
			    _Unwind_FrameState *fs)
{
  unsigned int *pc = context->ra;
  int this_cfa = (int) context->cfa;
  int new_cfa, ra_location, shifted_ra_location;
  int regs_off, fpu_save_off;
  int fpu_save;
  int old_style, i;

  if (pc[1] != 0x91d02010)	/* ta 0x10 */
    return _URC_END_OF_STACK;

  if (pc[0] == 0x821020d8)	/* mov NR_sigreturn, %g1 */
    old_style = 1;
  else if (pc[0] == 0x82102065)	/* mov NR_rt_sigreturn, %g1 */
    old_style = 0;
  else
    return _URC_END_OF_STACK;

  if (old_style)
    {
      regs_off = 96;
      fpu_save_off = regs_off + (4 * 4) + (16 * 4);
    }
  else
    {
      regs_off = 96 + 128;
      fpu_save_off = regs_off + (4 * 4) + (16 * 4) + (2 * 4);
    }

  new_cfa = *(int *)(this_cfa + regs_off + (4 * 4) + (14 * 4));
  fpu_save = *(int *)(this_cfa + fpu_save_off);
  fs->regs.cfa_how = CFA_REG_OFFSET;
  fs->regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs->regs.cfa_offset = new_cfa - this_cfa;

  for (i = 1; i < 16; i++)
    {
      /* We never restore %sp as everything is purely CFA-based.  */
      if ((unsigned int) i == __builtin_dwarf_sp_column ())
	continue;

      fs->regs.reg[i].how = REG_SAVED_OFFSET;
      fs->regs.reg[i].loc.offset
	= this_cfa + regs_off + (4 * 4) + (i * 4) - new_cfa;
    }
  for (i = 0; i < 16; i++)
    {
      fs->regs.reg[i + 16].how = REG_SAVED_OFFSET;
      fs->regs.reg[i + 16].loc.offset
	= this_cfa + (i * 4) - new_cfa;
    }
  if (fpu_save)
    {
      for (i = 0; i < 32; i++)
	{
	  fs->regs.reg[i + 32].how = REG_SAVED_OFFSET;
	  fs->regs.reg[i + 32].loc.offset
	    = fpu_save + (i * 4) - new_cfa;
	}
    }

  /* State the rules to find the kernel's code "return address", which is
     the address of the active instruction when the signal was caught.
     On the SPARC, since RETURN_ADDR_OFFSET (essentially 8) is defined, we
     need to preventively subtract it from the purported return address.  */
  ra_location = this_cfa + regs_off + 4;
  shifted_ra_location = this_cfa + regs_off + 3 * 4; /* Y register */
  *(int *)shifted_ra_location = *(int *)ra_location - 8;
  fs->retaddr_column = 0;
  fs->regs.reg[0].how = REG_SAVED_OFFSET;
  fs->regs.reg[0].loc.offset = shifted_ra_location - new_cfa;
  fs->signal_frame = 1;

  return _URC_NO_REASON;
}

#endif
