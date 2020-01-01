/* DWARF2 exception handling and frame unwinding for Xtensa.
   Copyright (C) 1997-2020 Free Software Foundation, Inc.

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

#include "tconfig.h"
#include "tsystem.h"
#include "coretypes.h"
#include "tm.h"
#include "libgcc_tm.h"
#include "dwarf2.h"
#include "unwind.h"
#ifdef __USING_SJLJ_EXCEPTIONS__
# define NO_SIZE_OF_ENCODED_VALUE
#endif
#include "unwind-pe.h"
#include "unwind-dw2-fde.h"
#include "unwind-dw2-xtensa.h"

#ifndef __USING_SJLJ_EXCEPTIONS__

/* The standard CIE and FDE structures work fine for Xtensa but the
   variable-size register window save areas are not a good fit for the rest
   of the standard DWARF unwinding mechanism.  Nor is that mechanism
   necessary, since the register save areas are always in fixed locations
   in each stack frame.  This file is a stripped down and customized version
   of the standard DWARF unwinding code.  It needs to be customized to have
   builtin logic for finding the save areas and also to track the stack
   pointer value (besides the CFA) while unwinding since the primary save
   area is located below the stack pointer.  It is stripped down to reduce
   code size and ease the maintenance burden of tracking changes in the
   standard version of the code.  */

#ifndef DWARF_REG_TO_UNWIND_COLUMN
#define DWARF_REG_TO_UNWIND_COLUMN(REGNO) (REGNO)
#endif

#define XTENSA_RA_FIELD_MASK 0x3FFFFFFF

/* This is the register and unwind state for a particular frame.  This
   provides the information necessary to unwind up past a frame and return
   to its caller.  */
struct _Unwind_Context
{
  /* Track register window save areas of 4 registers each, instead of
     keeping separate addresses for the individual registers.  */
  _Unwind_Word *reg[4];

  void *cfa;
  void *sp;
  void *ra;

  /* Cache the 2 high bits to replace the window size in return addresses.  */
  _Unwind_Word ra_high_bits;

  void *lsda;
  struct dwarf_eh_bases bases;
  /* Signal frame context.  */
#define SIGNAL_FRAME_BIT ((~(_Unwind_Word) 0 >> 1) + 1)
  _Unwind_Word flags;
  /* 0 for now, can be increased when further fields are added to
     struct _Unwind_Context.  */
  _Unwind_Word version;
};


/* Read unaligned data from the instruction buffer.  */

union unaligned
{
  void *p;
} __attribute__ ((packed));

static void uw_update_context (struct _Unwind_Context *, _Unwind_FrameState *);
static _Unwind_Reason_Code uw_frame_state_for (struct _Unwind_Context *,
					       _Unwind_FrameState *);

static inline void *
read_pointer (const void *p) { const union unaligned *up = p; return up->p; }

static inline _Unwind_Word
_Unwind_IsSignalFrame (struct _Unwind_Context *context)
{
  return (context->flags & SIGNAL_FRAME_BIT) ? 1 : 0;
}

static inline void
_Unwind_SetSignalFrame (struct _Unwind_Context *context, int val)
{
  if (val)
    context->flags |= SIGNAL_FRAME_BIT;
  else
    context->flags &= ~SIGNAL_FRAME_BIT;
}

/* Get the value of register INDEX as saved in CONTEXT.  */

inline _Unwind_Word
_Unwind_GetGR (struct _Unwind_Context *context, int index)
{
  _Unwind_Word *ptr;

  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  ptr = context->reg[index >> 2] + (index & 3);

  return *ptr;
}

/* Get the value of the CFA as saved in CONTEXT.  */

_Unwind_Word
_Unwind_GetCFA (struct _Unwind_Context *context)
{
  return (_Unwind_Ptr) context->sp;
}

/* Overwrite the saved value for register INDEX in CONTEXT with VAL.  */

inline void
_Unwind_SetGR (struct _Unwind_Context *context, int index, _Unwind_Word val)
{
  _Unwind_Word *ptr;

  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  ptr = context->reg[index >> 2] + (index & 3);

  *ptr = val;
}

/* Retrieve the return address for CONTEXT.  */

inline _Unwind_Ptr
_Unwind_GetIP (struct _Unwind_Context *context)
{
  return (_Unwind_Ptr) context->ra;
}

/* Retrieve the return address and flag whether that IP is before
   or after first not yet fully executed instruction.  */

inline _Unwind_Ptr
_Unwind_GetIPInfo (struct _Unwind_Context *context, int *ip_before_insn)
{
  *ip_before_insn = _Unwind_IsSignalFrame (context);
  return (_Unwind_Ptr) context->ra;
}

/* Overwrite the return address for CONTEXT with VAL.  */

inline void
_Unwind_SetIP (struct _Unwind_Context *context, _Unwind_Ptr val)
{
  context->ra = (void *) val;
}

void *
_Unwind_GetLanguageSpecificData (struct _Unwind_Context *context)
{
  return context->lsda;
}

_Unwind_Ptr
_Unwind_GetRegionStart (struct _Unwind_Context *context)
{
  return (_Unwind_Ptr) context->bases.func;
}

void *
_Unwind_FindEnclosingFunction (void *pc)
{
  struct dwarf_eh_bases bases;
  const struct dwarf_fde *fde = _Unwind_Find_FDE (pc-1, &bases);
  if (fde)
    return bases.func;
  else
    return NULL;
}

_Unwind_Ptr
_Unwind_GetDataRelBase (struct _Unwind_Context *context)
{
  return (_Unwind_Ptr) context->bases.dbase;
}

_Unwind_Ptr
_Unwind_GetTextRelBase (struct _Unwind_Context *context)
{
  return (_Unwind_Ptr) context->bases.tbase;
}

#include "md-unwind-support.h"

/* Extract any interesting information from the CIE for the translation
   unit F belongs to.  Return a pointer to the byte after the augmentation,
   or NULL if we encountered an undecipherable augmentation.  */

static const unsigned char *
extract_cie_info (const struct dwarf_cie *cie, struct _Unwind_Context *context,
		  _Unwind_FrameState *fs)
{
  const unsigned char *aug = cie->augmentation;
  const unsigned char *p = aug + strlen ((const char *)aug) + 1;
  const unsigned char *ret = NULL;
  _uleb128_t utmp;
  _sleb128_t stmp;

  /* g++ v2 "eh" has pointer immediately following augmentation string,
     so it must be handled first.  */
  if (aug[0] == 'e' && aug[1] == 'h')
    {
      fs->eh_ptr = read_pointer (p);
      p += sizeof (void *);
      aug += 2;
    }

  /* Immediately following the augmentation are the code and
     data alignment and return address column.  */
  p = read_uleb128 (p, &utmp);
  p = read_sleb128 (p, &stmp);
  if (cie->version == 1)
    fs->retaddr_column = *p++;
  else
    {
      p = read_uleb128 (p, &utmp);
      fs->retaddr_column = (_Unwind_Word)utmp;
    }
  fs->lsda_encoding = DW_EH_PE_omit;

  /* If the augmentation starts with 'z', then a uleb128 immediately
     follows containing the length of the augmentation field following
     the size.  */
  if (*aug == 'z')
    {
      p = read_uleb128 (p, &utmp);
      ret = p + utmp;

      fs->saw_z = 1;
      ++aug;
    }

  /* Iterate over recognized augmentation subsequences.  */
  while (*aug != '\0')
    {
      /* "L" indicates a byte showing how the LSDA pointer is encoded.  */
      if (aug[0] == 'L')
	{
	  fs->lsda_encoding = *p++;
	  aug += 1;
	}

      /* "R" indicates a byte indicating how FDE addresses are encoded.  */
      else if (aug[0] == 'R')
	{
	  fs->fde_encoding = *p++;
	  aug += 1;
	}

      /* "P" indicates a personality routine in the CIE augmentation.  */
      else if (aug[0] == 'P')
	{
	  _Unwind_Ptr personality;
	  
	  p = read_encoded_value (context, *p, p + 1, &personality);
	  fs->personality = (_Unwind_Personality_Fn) personality;
	  aug += 1;
	}

      /* "S" indicates a signal frame.  */
      else if (aug[0] == 'S')
	{
	  fs->signal_frame = 1;
	  aug += 1;
	}

      /* Otherwise we have an unknown augmentation string.
	 Bail unless we saw a 'z' prefix.  */
      else
	return ret;
    }

  return ret ? ret : p;
}

/* Given the _Unwind_Context CONTEXT for a stack frame, look up the FDE for
   its caller and decode it into FS.  This function also sets the
   lsda member of CONTEXT, as it is really information
   about the caller's frame.  */

static _Unwind_Reason_Code
uw_frame_state_for (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  const struct dwarf_fde *fde;
  const struct dwarf_cie *cie;
  const unsigned char *aug;
  int window_size;
  _Unwind_Word *ra_ptr;

  memset (fs, 0, sizeof (*fs));
  context->lsda = 0;

  fde = _Unwind_Find_FDE (context->ra + _Unwind_IsSignalFrame (context) - 1,
			  &context->bases);
  if (fde == NULL)
    {
#ifdef MD_FALLBACK_FRAME_STATE_FOR
      _Unwind_Reason_Code reason;
      /* Couldn't find frame unwind info for this function.  Try a
	 target-specific fallback mechanism.  This will necessarily
	 not provide a personality routine or LSDA.  */
      reason = MD_FALLBACK_FRAME_STATE_FOR (context, fs);
      if (reason != _URC_END_OF_STACK)
	return reason;
#endif
      /* The frame was not recognized and handled by the fallback function,
	 but it is not really the end of the stack.  Fall through here and
	 unwind it anyway.  */
    }
  else
    {
      cie = get_cie (fde);
      if (extract_cie_info (cie, context, fs) == NULL)
	/* CIE contained unknown augmentation.  */
	return _URC_FATAL_PHASE1_ERROR;

      /* Locate augmentation for the fde.  */
      aug = (const unsigned char *) fde + sizeof (*fde);
      aug += 2 * size_of_encoded_value (fs->fde_encoding);
      if (fs->saw_z)
	{
	  _uleb128_t i;
	  aug = read_uleb128 (aug, &i);
	}
      if (fs->lsda_encoding != DW_EH_PE_omit)
	{
	  _Unwind_Ptr lsda;

	  aug = read_encoded_value (context, fs->lsda_encoding, aug, &lsda);
	  context->lsda = (void *) lsda;
	}
    }

  /* Check for the end of the stack.  This needs to be checked after
     the MD_FALLBACK_FRAME_STATE_FOR check for signal frames because
     the contents of context->reg[0] are undefined at a signal frame,
     and register a0 may appear to be zero.  (The return address in
     context->ra comes from register a4 or a8).  */
  ra_ptr = context->reg[0];
  if (ra_ptr && *ra_ptr == 0)
    return _URC_END_OF_STACK;

  /* Find the window size from the high bits of the return address.  */
  if (ra_ptr)
    window_size = (*ra_ptr >> 30) * 4;
  else
    window_size = 8;

  fs->retaddr_column = window_size;

  return _URC_NO_REASON;
}

static void
uw_update_context_1 (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct _Unwind_Context orig_context = *context;
  _Unwind_Word *sp, *cfa, *next_cfa;
  int i;

  if (fs->signal_regs)
    {
      cfa = (_Unwind_Word *) fs->signal_regs[1];
      next_cfa = (_Unwind_Word *) cfa[-3];

      for (i = 0; i < 4; i++)
	context->reg[i] = fs->signal_regs + (i << 2);
    }
  else
    {
      int window_size = fs->retaddr_column >> 2;

      sp = (_Unwind_Word *) orig_context.sp;
      cfa = (_Unwind_Word *) orig_context.cfa;
      next_cfa = (_Unwind_Word *) cfa[-3];

      /* Registers a0-a3 are in the save area below sp.  */
      context->reg[0] = sp - 4;

      /* Find the extra save area below next_cfa.  */
      for (i = 1; i < window_size; i++)
	context->reg[i] = next_cfa - 4 * (1 + window_size - i);

      /* Remaining registers rotate from previous save areas.  */
      for (i = window_size; i < 4; i++)
	context->reg[i] = orig_context.reg[i - window_size];
    }

  context->sp = cfa;
  context->cfa = next_cfa;

  _Unwind_SetSignalFrame (context, fs->signal_frame);
}

/* CONTEXT describes the unwind state for a frame, and FS describes the FDE
   of its caller.  Update CONTEXT to refer to the caller as well.  Note
   that the lsda member is not updated here, but later in
   uw_frame_state_for.  */

static void
uw_update_context (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  uw_update_context_1 (context, fs);

  /* Compute the return address now, since the return address column
     can change from frame to frame.  */
  if (fs->signal_ra != 0)
    context->ra = (void *) fs->signal_ra;
  else
    context->ra = (void *) ((_Unwind_GetGR (context, fs->retaddr_column)
			     & XTENSA_RA_FIELD_MASK) | context->ra_high_bits);
}

static void
uw_advance_context (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  uw_update_context (context, fs);
}

/* Fill in CONTEXT for top-of-stack.  The only valid registers at this
   level will be the return address and the CFA.  */

#define uw_init_context(CONTEXT)					   \
  do									   \
    {									   \
      __builtin_unwind_init ();						   \
      uw_init_context_1 (CONTEXT, __builtin_dwarf_cfa (),		   \
			 __builtin_return_address (0));			   \
    }									   \
  while (0)

static void __attribute__((noinline))
uw_init_context_1 (struct _Unwind_Context *context, void *outer_cfa,
		   void *outer_ra)
{
  void *ra = __builtin_return_address (0);
  void *cfa = __builtin_dwarf_cfa ();
  _Unwind_FrameState fs;

  memset (context, 0, sizeof (struct _Unwind_Context));
  context->ra = ra;

  memset (&fs, 0, sizeof (fs));
  fs.retaddr_column = 8;
  context->sp = cfa;
  context->cfa = outer_cfa;
  context->ra_high_bits =
    ((_Unwind_Word) uw_init_context_1) & ~XTENSA_RA_FIELD_MASK;
  uw_update_context_1 (context, &fs);

  context->ra = outer_ra;
}


/* Install TARGET into CURRENT so that we can return to it.  This is a
   macro because __builtin_eh_return must be invoked in the context of
   our caller.  */

#define uw_install_context(CURRENT, TARGET, FRAMES)				 \
  do									 \
    {									 \
      long offset = uw_install_context_1 ((CURRENT), (TARGET));		 \
      void *handler = __builtin_frob_return_addr ((TARGET)->ra);	 \
      __builtin_eh_return (offset, handler);				 \
    }									 \
  while (0)

static long
uw_install_context_1 (struct _Unwind_Context *current,
		      struct _Unwind_Context *target)
{
  long i;

  /* The eh_return insn assumes a window size of 8, so don't bother copying
     the save areas for registers a8-a15 since they won't be reloaded.  */
  for (i = 0; i < 2; ++i)
    {
      void *c = current->reg[i];
      void *t = target->reg[i];

      if (t && c && t != c)
	memcpy (c, t, 4 * sizeof (_Unwind_Word));
    }

  return 0;
}

static inline _Unwind_Ptr
uw_identify_context (struct _Unwind_Context *context)
{
  return _Unwind_GetCFA (context);
}


#include "unwind.inc"

#if defined (USE_GAS_SYMVER) && defined (SHARED) && defined (USE_LIBUNWIND_EXCEPTIONS)
alias (_Unwind_Backtrace);
alias (_Unwind_DeleteException);
alias (_Unwind_FindEnclosingFunction);
alias (_Unwind_ForcedUnwind);
alias (_Unwind_GetDataRelBase);
alias (_Unwind_GetTextRelBase);
alias (_Unwind_GetCFA);
alias (_Unwind_GetGR);
alias (_Unwind_GetIP);
alias (_Unwind_GetLanguageSpecificData);
alias (_Unwind_GetRegionStart);
alias (_Unwind_RaiseException);
alias (_Unwind_Resume);
alias (_Unwind_Resume_or_Rethrow);
alias (_Unwind_SetGR);
alias (_Unwind_SetIP);
#endif

#endif /* !USING_SJLJ_EXCEPTIONS */
