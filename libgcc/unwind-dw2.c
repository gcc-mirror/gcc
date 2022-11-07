/* DWARF2 exception handling and frame unwind runtime interface routines.
   Copyright (C) 1997-2022 Free Software Foundation, Inc.

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
#include "gthr.h"
#include "unwind-dw2.h"
#include <stddef.h>

#ifdef HAVE_SYS_SDT_H
#include <sys/sdt.h>
#endif

#ifndef __USING_SJLJ_EXCEPTIONS__

#ifndef __LIBGCC_STACK_GROWS_DOWNWARD__
#define __LIBGCC_STACK_GROWS_DOWNWARD__ 0
#else
#undef __LIBGCC_STACK_GROWS_DOWNWARD__
#define __LIBGCC_STACK_GROWS_DOWNWARD__ 1
#endif

/* Dwarf frame registers used for pre gcc 3.0 compiled glibc.  */
#ifndef PRE_GCC3_DWARF_FRAME_REGISTERS
#define PRE_GCC3_DWARF_FRAME_REGISTERS __LIBGCC_DWARF_FRAME_REGISTERS__
#endif

/* ??? For the public function interfaces, we tend to gcc_assert that the
   column numbers are in range.  For the dwarf2 unwind info this does happen,
   although so far in a case that doesn't actually matter.

   See PR49146, in which a call from x86_64 ms abi to x86_64 unix abi stores
   the call-saved xmm registers and annotates them.  We havn't bothered
   providing support for the xmm registers for the x86_64 port primarily
   because the 64-bit windows targets don't use dwarf2 unwind, using sjlj or
   SEH instead.  Adding the support for unix targets would generally be a
   waste.  However, some runtime libraries supplied with ICC do contain such
   an unorthodox transition, as well as the unwind info to match.  This loss
   of register restoration doesn't matter in practice, because the exception
   is caught in the native unix abi, where all of the xmm registers are 
   call clobbered.

   Ideally, we'd record some bit to notice when we're failing to restore some
   register recorded in the unwind info, but to do that we need annotation on
   the unix->ms abi edge, so that we know when the register data may be
   discarded.  And since this edge is also within the ICC library, we're
   unlikely to be able to get the new annotation.

   Barring a magic solution to restore the ms abi defined 128-bit xmm registers
   (as distictly opposed to the full runtime width) without causing extra
   overhead for normal unix abis, the best solution seems to be to simply
   ignore unwind data for unknown columns.  */

#define UNWIND_COLUMN_IN_RANGE(x) \
    __builtin_expect((x) <= __LIBGCC_DWARF_FRAME_REGISTERS__, 1)

#ifdef REG_VALUE_IN_UNWIND_CONTEXT
typedef _Unwind_Word _Unwind_Context_Reg_Val;

#ifndef ASSUME_EXTENDED_UNWIND_CONTEXT
#define ASSUME_EXTENDED_UNWIND_CONTEXT 1
#endif

static inline _Unwind_Word
_Unwind_Get_Unwind_Word (_Unwind_Context_Reg_Val val)
{
  return val;
}

static inline _Unwind_Context_Reg_Val
_Unwind_Get_Unwind_Context_Reg_Val (_Unwind_Word val)
{
  return val;
}
#else
typedef void *_Unwind_Context_Reg_Val;

static inline _Unwind_Word
_Unwind_Get_Unwind_Word (_Unwind_Context_Reg_Val val)
{
  return (_Unwind_Word) (_Unwind_Internal_Ptr) val;
}

static inline _Unwind_Context_Reg_Val
_Unwind_Get_Unwind_Context_Reg_Val (_Unwind_Word val)
{
  return (_Unwind_Context_Reg_Val) (_Unwind_Internal_Ptr) val;
}
#endif

#ifndef ASSUME_EXTENDED_UNWIND_CONTEXT
#define ASSUME_EXTENDED_UNWIND_CONTEXT 0
#endif

/* This is the register and unwind state for a particular frame.  This
   provides the information necessary to unwind up past a frame and return
   to its caller.  */
struct _Unwind_Context
{
  _Unwind_Context_Reg_Val reg[__LIBGCC_DWARF_FRAME_REGISTERS__+1];
  void *cfa;
  void *ra;
  void *lsda;
  struct dwarf_eh_bases bases;
  /* Signal frame context.  */
#define SIGNAL_FRAME_BIT ((~(_Unwind_Word) 0 >> 1) + 1)
  /* Context which has version/args_size/by_value fields.  */
#define EXTENDED_CONTEXT_BIT ((~(_Unwind_Word) 0 >> 2) + 1)
  /* Bit reserved on AArch64, return address has been signed with A or B
     key.  */
#define RA_SIGNED_BIT ((~(_Unwind_Word) 0 >> 3) + 1)
  _Unwind_Word flags;
  /* 0 for now, can be increased when further fields are added to
     struct _Unwind_Context.  */
  _Unwind_Word version;
  _Unwind_Word args_size;
  char by_value[__LIBGCC_DWARF_FRAME_REGISTERS__+1];
};

/* Byte size of every register managed by these routines.  */
static unsigned char dwarf_reg_size_table[__LIBGCC_DWARF_FRAME_REGISTERS__+1];


/* Read unaligned data from the instruction buffer.  */

union unaligned
{
  void *p;
  unsigned u2 __attribute__ ((mode (HI)));
  unsigned u4 __attribute__ ((mode (SI)));
  unsigned u8 __attribute__ ((mode (DI)));
  signed s2 __attribute__ ((mode (HI)));
  signed s4 __attribute__ ((mode (SI)));
  signed s8 __attribute__ ((mode (DI)));
} __attribute__ ((packed));

static void uw_update_context (struct _Unwind_Context *, _Unwind_FrameState *);
static _Unwind_Reason_Code uw_frame_state_for (struct _Unwind_Context *,
					       _Unwind_FrameState *);

static inline void *
read_pointer (const void *p) { const union unaligned *up = p; return up->p; }

static inline int
read_1u (const void *p) { return *(const unsigned char *) p; }

static inline int
read_1s (const void *p) { return *(const signed char *) p; }

static inline int
read_2u (const void *p) { const union unaligned *up = p; return up->u2; }

static inline int
read_2s (const void *p) { const union unaligned *up = p; return up->s2; }

static inline unsigned int
read_4u (const void *p) { const union unaligned *up = p; return up->u4; }

static inline int
read_4s (const void *p) { const union unaligned *up = p; return up->s4; }

static inline unsigned long
read_8u (const void *p) { const union unaligned *up = p; return up->u8; }

static inline unsigned long
read_8s (const void *p) { const union unaligned *up = p; return up->s8; }

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

static inline _Unwind_Word
_Unwind_IsExtendedContext (struct _Unwind_Context *context)
{
  return (ASSUME_EXTENDED_UNWIND_CONTEXT
	  || (context->flags & EXTENDED_CONTEXT_BIT));
}

/* Get the value of register REGNO as saved in CONTEXT.  */

inline _Unwind_Word
_Unwind_GetGR (struct _Unwind_Context *context, int regno)
{
  int size, index;
  _Unwind_Context_Reg_Val val;

#ifdef DWARF_ZERO_REG
  if (regno == DWARF_ZERO_REG)
    return 0;
#endif

  index = DWARF_REG_TO_UNWIND_COLUMN (regno);
  gcc_assert (index < (int) sizeof(dwarf_reg_size_table));
  size = dwarf_reg_size_table[index];
  val = context->reg[index];

  if (_Unwind_IsExtendedContext (context) && context->by_value[index])
    return _Unwind_Get_Unwind_Word (val);

#ifdef DWARF_LAZY_REGISTER_VALUE
  {
    _Unwind_Word value;
    if (DWARF_LAZY_REGISTER_VALUE (regno, &value))
      return value;
  }
#endif

  /* This will segfault if the register hasn't been saved.  */
  if (size == sizeof(_Unwind_Ptr))
    return * (_Unwind_Ptr *) (_Unwind_Internal_Ptr) val;
  else
    {
      gcc_assert (size == sizeof(_Unwind_Word));
      return * (_Unwind_Word *) (_Unwind_Internal_Ptr) val;
    }
}

static inline void *
_Unwind_GetPtr (struct _Unwind_Context *context, int index)
{
  return (void *)(_Unwind_Ptr) _Unwind_GetGR (context, index);
}

/* Get the value of the CFA as saved in CONTEXT.  */

_Unwind_Word
_Unwind_GetCFA (struct _Unwind_Context *context)
{
  return (_Unwind_Ptr) context->cfa;
}

/* Overwrite the saved value for register INDEX in CONTEXT with VAL.  */

inline void
_Unwind_SetGR (struct _Unwind_Context *context, int index, _Unwind_Word val)
{
  int size;
  void *ptr;

  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  gcc_assert (index < (int) sizeof(dwarf_reg_size_table));
  size = dwarf_reg_size_table[index];

  if (_Unwind_IsExtendedContext (context) && context->by_value[index])
    {
      context->reg[index] = _Unwind_Get_Unwind_Context_Reg_Val (val);
      return;
    }

  ptr = (void *) (_Unwind_Internal_Ptr) context->reg[index];

  if (size == sizeof(_Unwind_Ptr))
    * (_Unwind_Ptr *) ptr = val;
  else
    {
      gcc_assert (size == sizeof(_Unwind_Word));
      * (_Unwind_Word *) ptr = val;
    }
}

/* Get the pointer to a register INDEX as saved in CONTEXT.  */

static inline void *
_Unwind_GetGRPtr (struct _Unwind_Context *context, int index)
{
  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  if (_Unwind_IsExtendedContext (context) && context->by_value[index])
    return &context->reg[index];
  return (void *) (_Unwind_Internal_Ptr) context->reg[index];
}

/* Set the pointer to a register INDEX as saved in CONTEXT.  */

static inline void
_Unwind_SetGRPtr (struct _Unwind_Context *context, int index, void *p)
{
  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  if (_Unwind_IsExtendedContext (context))
    context->by_value[index] = 0;
  context->reg[index] = (_Unwind_Context_Reg_Val) (_Unwind_Internal_Ptr) p;
}

/* Overwrite the saved value for register INDEX in CONTEXT with VAL.  */

static inline void
_Unwind_SetGRValue (struct _Unwind_Context *context, int index,
		    _Unwind_Word val)
{
  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  gcc_assert (index < (int) sizeof(dwarf_reg_size_table));
  /* Return column size may be smaller than _Unwind_Context_Reg_Val.  */
  gcc_assert (dwarf_reg_size_table[index] <= sizeof (_Unwind_Context_Reg_Val));

  context->by_value[index] = 1;
  context->reg[index] = _Unwind_Get_Unwind_Context_Reg_Val (val);
}

/* Return nonzero if register INDEX is stored by value rather than
   by reference.  */

static inline int
_Unwind_GRByValue (struct _Unwind_Context *context, int index)
{
  index = DWARF_REG_TO_UNWIND_COLUMN (index);
  return context->by_value[index];
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

#ifndef __ia64__
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
#endif

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

  /* After the augmentation resp. pointer for "eh" augmentation
     follows for CIE version >= 4 address size byte and
     segment size byte.  */
  if (__builtin_expect (cie->version >= 4, 0))
    {
      if (p[0] != sizeof (void *) || p[1] != 0)
	return NULL;
      p += 2;
    }
  /* Immediately following this are the code and
     data alignment and return address column.  */
  p = read_uleb128 (p, &utmp);
  fs->code_align = (_Unwind_Word)utmp;
  p = read_sleb128 (p, &stmp);
  fs->data_align = (_Unwind_Sword)stmp;
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
      /* aarch64 B-key pointer authentication.  */
      else if (aug[0] == 'B')
	{
	  aug += 1;
      }

      /* Otherwise we have an unknown augmentation string.
	 Bail unless we saw a 'z' prefix.  */
      else
	return ret;
    }

  return ret ? ret : p;
}


/* Decode a DW_OP stack program.  Return the top of stack.  Push INITIAL
   onto the stack to start.  */

static _Unwind_Word
execute_stack_op (const unsigned char *op_ptr, const unsigned char *op_end,
		  struct _Unwind_Context *context, _Unwind_Word initial)
{
  _Unwind_Word stack[64];	/* ??? Assume this is enough.  */
  int stack_elt;

  stack[0] = initial;
  stack_elt = 1;

  while (op_ptr < op_end)
    {
      enum dwarf_location_atom op = *op_ptr++;
      _Unwind_Word result;
      _uleb128_t reg, utmp;
      _sleb128_t offset, stmp;

      switch (op)
	{
	case DW_OP_lit0:
	case DW_OP_lit1:
	case DW_OP_lit2:
	case DW_OP_lit3:
	case DW_OP_lit4:
	case DW_OP_lit5:
	case DW_OP_lit6:
	case DW_OP_lit7:
	case DW_OP_lit8:
	case DW_OP_lit9:
	case DW_OP_lit10:
	case DW_OP_lit11:
	case DW_OP_lit12:
	case DW_OP_lit13:
	case DW_OP_lit14:
	case DW_OP_lit15:
	case DW_OP_lit16:
	case DW_OP_lit17:
	case DW_OP_lit18:
	case DW_OP_lit19:
	case DW_OP_lit20:
	case DW_OP_lit21:
	case DW_OP_lit22:
	case DW_OP_lit23:
	case DW_OP_lit24:
	case DW_OP_lit25:
	case DW_OP_lit26:
	case DW_OP_lit27:
	case DW_OP_lit28:
	case DW_OP_lit29:
	case DW_OP_lit30:
	case DW_OP_lit31:
	  result = op - DW_OP_lit0;
	  break;

	case DW_OP_addr:
	  result = (_Unwind_Word) (_Unwind_Ptr) read_pointer (op_ptr);
	  op_ptr += sizeof (void *);
	  break;

	case DW_OP_GNU_encoded_addr:
	  {
	    _Unwind_Ptr presult;
	    op_ptr = read_encoded_value (context, *op_ptr, op_ptr+1, &presult);
	    result = presult;
	  }
	  break;

	case DW_OP_const1u:
	  result = read_1u (op_ptr);
	  op_ptr += 1;
	  break;
	case DW_OP_const1s:
	  result = read_1s (op_ptr);
	  op_ptr += 1;
	  break;
	case DW_OP_const2u:
	  result = read_2u (op_ptr);
	  op_ptr += 2;
	  break;
	case DW_OP_const2s:
	  result = read_2s (op_ptr);
	  op_ptr += 2;
	  break;
	case DW_OP_const4u:
	  result = read_4u (op_ptr);
	  op_ptr += 4;
	  break;
	case DW_OP_const4s:
	  result = read_4s (op_ptr);
	  op_ptr += 4;
	  break;
	case DW_OP_const8u:
	  result = read_8u (op_ptr);
	  op_ptr += 8;
	  break;
	case DW_OP_const8s:
	  result = read_8s (op_ptr);
	  op_ptr += 8;
	  break;
	case DW_OP_constu:
	  op_ptr = read_uleb128 (op_ptr, &utmp);
	  result = (_Unwind_Word)utmp;
	  break;
	case DW_OP_consts:
	  op_ptr = read_sleb128 (op_ptr, &stmp);
	  result = (_Unwind_Sword)stmp;
	  break;

	case DW_OP_reg0:
	case DW_OP_reg1:
	case DW_OP_reg2:
	case DW_OP_reg3:
	case DW_OP_reg4:
	case DW_OP_reg5:
	case DW_OP_reg6:
	case DW_OP_reg7:
	case DW_OP_reg8:
	case DW_OP_reg9:
	case DW_OP_reg10:
	case DW_OP_reg11:
	case DW_OP_reg12:
	case DW_OP_reg13:
	case DW_OP_reg14:
	case DW_OP_reg15:
	case DW_OP_reg16:
	case DW_OP_reg17:
	case DW_OP_reg18:
	case DW_OP_reg19:
	case DW_OP_reg20:
	case DW_OP_reg21:
	case DW_OP_reg22:
	case DW_OP_reg23:
	case DW_OP_reg24:
	case DW_OP_reg25:
	case DW_OP_reg26:
	case DW_OP_reg27:
	case DW_OP_reg28:
	case DW_OP_reg29:
	case DW_OP_reg30:
	case DW_OP_reg31:
	  result = _Unwind_GetGR (context, op - DW_OP_reg0);
	  break;
	case DW_OP_regx:
	  op_ptr = read_uleb128 (op_ptr, &reg);
	  result = _Unwind_GetGR (context, reg);
	  break;

	case DW_OP_breg0:
	case DW_OP_breg1:
	case DW_OP_breg2:
	case DW_OP_breg3:
	case DW_OP_breg4:
	case DW_OP_breg5:
	case DW_OP_breg6:
	case DW_OP_breg7:
	case DW_OP_breg8:
	case DW_OP_breg9:
	case DW_OP_breg10:
	case DW_OP_breg11:
	case DW_OP_breg12:
	case DW_OP_breg13:
	case DW_OP_breg14:
	case DW_OP_breg15:
	case DW_OP_breg16:
	case DW_OP_breg17:
	case DW_OP_breg18:
	case DW_OP_breg19:
	case DW_OP_breg20:
	case DW_OP_breg21:
	case DW_OP_breg22:
	case DW_OP_breg23:
	case DW_OP_breg24:
	case DW_OP_breg25:
	case DW_OP_breg26:
	case DW_OP_breg27:
	case DW_OP_breg28:
	case DW_OP_breg29:
	case DW_OP_breg30:
	case DW_OP_breg31:
	  op_ptr = read_sleb128 (op_ptr, &offset);
	  result = _Unwind_GetGR (context, op - DW_OP_breg0) + offset;
	  break;
	case DW_OP_bregx:
	  op_ptr = read_uleb128 (op_ptr, &reg);
	  op_ptr = read_sleb128 (op_ptr, &offset);
	  result = _Unwind_GetGR (context, reg) + (_Unwind_Word)offset;
	  break;

	case DW_OP_dup:
	  gcc_assert (stack_elt);
	  result = stack[stack_elt - 1];
	  break;

	case DW_OP_drop:
	  gcc_assert (stack_elt);
	  stack_elt -= 1;
	  goto no_push;

	case DW_OP_pick:
	  offset = *op_ptr++;
	  gcc_assert (offset < stack_elt - 1);
	  result = stack[stack_elt - 1 - offset];
	  break;

	case DW_OP_over:
	  gcc_assert (stack_elt >= 2);
	  result = stack[stack_elt - 2];
	  break;

	case DW_OP_swap:
	  {
	    _Unwind_Word t;
	    gcc_assert (stack_elt >= 2);
	    t = stack[stack_elt - 1];
	    stack[stack_elt - 1] = stack[stack_elt - 2];
	    stack[stack_elt - 2] = t;
	    goto no_push;
	  }

	case DW_OP_rot:
	  {
	    _Unwind_Word t1, t2, t3;

	    gcc_assert (stack_elt >= 3);
	    t1 = stack[stack_elt - 1];
	    t2 = stack[stack_elt - 2];
	    t3 = stack[stack_elt - 3];
	    stack[stack_elt - 1] = t2;
	    stack[stack_elt - 2] = t3;
	    stack[stack_elt - 3] = t1;
	    goto no_push;
	  }

	case DW_OP_deref:
	case DW_OP_deref_size:
	case DW_OP_abs:
	case DW_OP_neg:
	case DW_OP_not:
	case DW_OP_plus_uconst:
	  /* Unary operations.  */
	  gcc_assert (stack_elt);
	  stack_elt -= 1;

	  result = stack[stack_elt];

	  switch (op)
	    {
	    case DW_OP_deref:
	      {
		void *ptr = (void *) (_Unwind_Ptr) result;
		result = (_Unwind_Ptr) read_pointer (ptr);
	      }
	      break;

	    case DW_OP_deref_size:
	      {
		void *ptr = (void *) (_Unwind_Ptr) result;
		switch (*op_ptr++)
		  {
		  case 1:
		    result = read_1u (ptr);
		    break;
		  case 2:
		    result = read_2u (ptr);
		    break;
		  case 4:
		    result = read_4u (ptr);
		    break;
		  case 8:
		    result = read_8u (ptr);
		    break;
		  default:
		    gcc_unreachable ();
		  }
	      }
	      break;

	    case DW_OP_abs:
	      if ((_Unwind_Sword) result < 0)
		result = -result;
	      break;
	    case DW_OP_neg:
	      result = -result;
	      break;
	    case DW_OP_not:
	      result = ~result;
	      break;
	    case DW_OP_plus_uconst:
	      op_ptr = read_uleb128 (op_ptr, &utmp);
	      result += (_Unwind_Word)utmp;
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  break;

	case DW_OP_and:
	case DW_OP_div:
	case DW_OP_minus:
	case DW_OP_mod:
	case DW_OP_mul:
	case DW_OP_or:
	case DW_OP_plus:
	case DW_OP_shl:
	case DW_OP_shr:
	case DW_OP_shra:
	case DW_OP_xor:
	case DW_OP_le:
	case DW_OP_ge:
	case DW_OP_eq:
	case DW_OP_lt:
	case DW_OP_gt:
	case DW_OP_ne:
	  {
	    /* Binary operations.  */
	    _Unwind_Word first, second;
	    gcc_assert (stack_elt >= 2);
	    stack_elt -= 2;

	    second = stack[stack_elt];
	    first = stack[stack_elt + 1];

	    switch (op)
	      {
	      case DW_OP_and:
		result = second & first;
		break;
	      case DW_OP_div:
		result = (_Unwind_Sword) second / (_Unwind_Sword) first;
		break;
	      case DW_OP_minus:
		result = second - first;
		break;
	      case DW_OP_mod:
		result = second % first;
		break;
	      case DW_OP_mul:
		result = second * first;
		break;
	      case DW_OP_or:
		result = second | first;
		break;
	      case DW_OP_plus:
		result = second + first;
		break;
	      case DW_OP_shl:
		result = second << first;
		break;
	      case DW_OP_shr:
		result = second >> first;
		break;
	      case DW_OP_shra:
		result = (_Unwind_Sword) second >> first;
		break;
	      case DW_OP_xor:
		result = second ^ first;
		break;
	      case DW_OP_le:
		result = (_Unwind_Sword) second <= (_Unwind_Sword) first;
		break;
	      case DW_OP_ge:
		result = (_Unwind_Sword) second >= (_Unwind_Sword) first;
		break;
	      case DW_OP_eq:
		result = (_Unwind_Sword) second == (_Unwind_Sword) first;
		break;
	      case DW_OP_lt:
		result = (_Unwind_Sword) second < (_Unwind_Sword) first;
		break;
	      case DW_OP_gt:
		result = (_Unwind_Sword) second > (_Unwind_Sword) first;
		break;
	      case DW_OP_ne:
		result = (_Unwind_Sword) second != (_Unwind_Sword) first;
		break;

	      default:
		gcc_unreachable ();
	      }
	  }
	  break;

	case DW_OP_skip:
	  offset = read_2s (op_ptr);
	  op_ptr += 2;
	  op_ptr += offset;
	  goto no_push;

	case DW_OP_bra:
	  gcc_assert (stack_elt);
	  stack_elt -= 1;

	  offset = read_2s (op_ptr);
	  op_ptr += 2;
	  if (stack[stack_elt] != 0)
	    op_ptr += offset;
	  goto no_push;

	case DW_OP_nop:
	  goto no_push;

	default:
	  gcc_unreachable ();
	}

      /* Most things push a result value.  */
      gcc_assert ((size_t) stack_elt < sizeof(stack)/sizeof(*stack));
      stack[stack_elt++] = result;
    no_push:;
    }

  /* We were executing this program to get a value.  It should be
     at top of stack.  */
  gcc_assert (stack_elt);
  stack_elt -= 1;
  return stack[stack_elt];
}


/* Decode DWARF 2 call frame information. Takes pointers the
   instruction sequence to decode, current register information and
   CIE info, and the PC range to evaluate.  */

static void
execute_cfa_program (const unsigned char *insn_ptr,
		     const unsigned char *insn_end,
		     struct _Unwind_Context *context,
		     _Unwind_FrameState *fs)
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
	fs->pc += (insn & 0x3f) * fs->code_align;
      else if ((insn & 0xc0) == DW_CFA_offset)
	{
	  reg = insn & 0x3f;
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Sword) utmp * fs->data_align;
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
	  fs->pc += read_1u (insn_ptr) * fs->code_align;
	  insn_ptr += 1;
	  break;
	case DW_CFA_advance_loc2:
	  fs->pc += read_2u (insn_ptr) * fs->code_align;
	  insn_ptr += 2;
	  break;
	case DW_CFA_advance_loc4:
	  fs->pc += read_4u (insn_ptr) * fs->code_align;
	  insn_ptr += 4;
	  break;

	case DW_CFA_offset_extended:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Sword) utmp * fs->data_align;
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
	  offset = stmp * fs->data_align;
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
	  fs->regs.cfa_offset *= fs->data_align;
	  break;

	case DW_CFA_def_cfa_offset_sf:
	  insn_ptr = read_sleb128 (insn_ptr, &stmp);
	  fs->regs.cfa_offset = (_Unwind_Sword)stmp;
	  fs->regs.cfa_offset *= fs->data_align;
	  /* cfa_how deliberately not set.  */
	  break;

	case DW_CFA_val_offset:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &utmp);
	  offset = (_Unwind_Sword) utmp * fs->data_align;
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
	  offset = stmp * fs->data_align;
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
	     return address signing status.  */
	  reg = DWARF_REGNUM_AARCH64_RA_STATE;
	  gcc_assert (fs->regs.how[reg] == REG_UNSAVED);
	  fs->regs.reg[reg].loc.offset ^= 1;
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
	  offset = (_Unwind_Word) utmp * fs->data_align;
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

/* Given the _Unwind_Context CONTEXT for a stack frame, look up the FDE for
   its caller and decode it into FS.  This function also sets the
   args_size and lsda members of CONTEXT, as they are really information
   about the caller's frame.  */

static _Unwind_Reason_Code
uw_frame_state_for (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  const struct dwarf_fde *fde;
  const struct dwarf_cie *cie;
  const unsigned char *aug, *insn, *end;

  memset (&fs->regs.how[0], 0,
	  sizeof (*fs) - offsetof (_Unwind_FrameState, regs.how[0]));
  context->args_size = 0;
  context->lsda = 0;

  if (context->ra == 0)
    return _URC_END_OF_STACK;

  fde = _Unwind_Find_FDE (context->ra + _Unwind_IsSignalFrame (context) - 1,
			  &context->bases);
  if (fde == NULL)
    {
#ifdef MD_FALLBACK_FRAME_STATE_FOR
      /* Couldn't find frame unwind info for this function.  Try a
	 target-specific fallback mechanism.  This will necessarily
	 not provide a personality routine or LSDA.  */
      return MD_FALLBACK_FRAME_STATE_FOR (context, fs);
#else
      return _URC_END_OF_STACK;
#endif
    }

  fs->pc = context->bases.func;

  cie = get_cie (fde);
  insn = extract_cie_info (cie, context, fs);
  if (insn == NULL)
    /* CIE contained unknown augmentation.  */
    return _URC_FATAL_PHASE1_ERROR;

  /* First decode all the insns in the CIE.  */
  end = (const unsigned char *) next_fde ((const struct dwarf_fde *) cie);
  execute_cfa_program (insn, end, context, fs);

  /* Locate augmentation for the fde.  */
  aug = (const unsigned char *) fde + sizeof (*fde);
  aug += 2 * size_of_encoded_value (fs->fde_encoding);
  insn = NULL;
  if (fs->saw_z)
    {
      _uleb128_t i;
      aug = read_uleb128 (aug, &i);
      insn = aug + i;
    }
  if (fs->lsda_encoding != DW_EH_PE_omit)
    {
      _Unwind_Ptr lsda;

      aug = read_encoded_value (context, fs->lsda_encoding, aug, &lsda);
      context->lsda = (void *) lsda;
    }

  /* Then the insns in the FDE up to our target PC.  */
  if (insn == NULL)
    insn = aug;
  end = (const unsigned char *) next_fde (fde);
  execute_cfa_program (insn, end, context, fs);

  return _URC_NO_REASON;
}

typedef struct frame_state
{
  void *cfa;
  void *eh_ptr;
  long cfa_offset;
  long args_size;
  long reg_or_offset[PRE_GCC3_DWARF_FRAME_REGISTERS+1];
  unsigned short cfa_reg;
  unsigned short retaddr_column;
  char saved[PRE_GCC3_DWARF_FRAME_REGISTERS+1];
} frame_state;

struct frame_state * __frame_state_for (void *, struct frame_state *);

/* Called from pre-G++ 3.0 __throw to find the registers to restore for
   a given PC_TARGET.  The caller should allocate a local variable of
   `struct frame_state' and pass its address to STATE_IN.  */

struct frame_state *
__frame_state_for (void *pc_target, struct frame_state *state_in)
{
  struct _Unwind_Context context;
  _Unwind_FrameState fs;
  int reg;

  memset (&context, 0, sizeof (struct _Unwind_Context));
  if (!ASSUME_EXTENDED_UNWIND_CONTEXT)
    context.flags = EXTENDED_CONTEXT_BIT;
  context.ra = pc_target + 1;

  if (uw_frame_state_for (&context, &fs) != _URC_NO_REASON)
    return 0;

  /* We have no way to pass a location expression for the CFA to our
     caller.  It wouldn't understand it anyway.  */
  if (fs.regs.cfa_how == CFA_EXP)
    return 0;

  for (reg = 0; reg < PRE_GCC3_DWARF_FRAME_REGISTERS + 1; reg++)
    {
      state_in->saved[reg] = fs.regs.how[reg];
      switch (state_in->saved[reg])
	{
	case REG_SAVED_REG:
	  state_in->reg_or_offset[reg] = fs.regs.reg[reg].loc.reg;
	  break;
	case REG_SAVED_OFFSET:
	  state_in->reg_or_offset[reg] = fs.regs.reg[reg].loc.offset;
	  break;
	default:
	  state_in->reg_or_offset[reg] = 0;
	  break;
	}
    }

  state_in->cfa_offset = fs.regs.cfa_offset;
  state_in->cfa_reg = fs.regs.cfa_reg;
  state_in->retaddr_column = fs.retaddr_column;
  state_in->args_size = context.args_size;
  state_in->eh_ptr = fs.eh_ptr;

  return state_in;
}

typedef union { _Unwind_Ptr ptr; _Unwind_Word word; } _Unwind_SpTmp;

static inline void
_Unwind_SetSpColumn (struct _Unwind_Context *context, void *cfa,
		     _Unwind_SpTmp *tmp_sp)
{
  int size = dwarf_reg_size_table[__builtin_dwarf_sp_column ()];

  if (size == sizeof(_Unwind_Ptr))
    tmp_sp->ptr = (_Unwind_Ptr) cfa;
  else
    {
      gcc_assert (size == sizeof(_Unwind_Word));
      tmp_sp->word = (_Unwind_Ptr) cfa;
    }
  _Unwind_SetGRPtr (context, __builtin_dwarf_sp_column (), tmp_sp);
}

static void
uw_update_context_1 (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct _Unwind_Context orig_context = *context;
  void *cfa;
  long i;

#ifdef __LIBGCC_EH_RETURN_STACKADJ_RTX__
  /* Special handling here: Many machines do not use a frame pointer,
     and track the CFA only through offsets from the stack pointer from
     one frame to the next.  In this case, the stack pointer is never
     stored, so it has no saved address in the context.  What we do
     have is the CFA from the previous stack frame.

     In very special situations (such as unwind info for signal return),
     there may be location expressions that use the stack pointer as well.

     Do this conditionally for one frame.  This allows the unwind info
     for one frame to save a copy of the stack pointer from the previous
     frame, and be able to use much easier CFA mechanisms to do it.
     Always zap the saved stack pointer value for the next frame; carrying
     the value over from one frame to another doesn't make sense.  */

  _Unwind_SpTmp tmp_sp;

  if (!_Unwind_GetGRPtr (&orig_context, __builtin_dwarf_sp_column ()))
    _Unwind_SetSpColumn (&orig_context, context->cfa, &tmp_sp);
  _Unwind_SetGRPtr (context, __builtin_dwarf_sp_column (), NULL);
#endif

  /* Compute this frame's CFA.  */
  switch (fs->regs.cfa_how)
    {
    case CFA_REG_OFFSET:
      cfa = _Unwind_GetPtr (&orig_context, fs->regs.cfa_reg);
      cfa += fs->regs.cfa_offset;
      break;

    case CFA_EXP:
      {
	const unsigned char *exp = fs->regs.cfa_exp;
	_uleb128_t len;

	exp = read_uleb128 (exp, &len);
	cfa = (void *) (_Unwind_Ptr)
	  execute_stack_op (exp, exp + len, &orig_context, 0);
	break;
      }

    default:
      gcc_unreachable ();
    }
  context->cfa = cfa;

  /* Compute the addresses of all registers saved in this frame.  */
  for (i = 0; i < __LIBGCC_DWARF_FRAME_REGISTERS__ + 1; ++i)
    switch (fs->regs.how[i])
      {
      case REG_UNSAVED:
      case REG_UNDEFINED:
	break;

      case REG_SAVED_OFFSET:
	_Unwind_SetGRPtr (context, i,
			  (void *) (cfa + fs->regs.reg[i].loc.offset));
	break;

      case REG_SAVED_REG:
	if (_Unwind_GRByValue (&orig_context, fs->regs.reg[i].loc.reg))
	  _Unwind_SetGRValue (context, i,
			      _Unwind_GetGR (&orig_context,
					     fs->regs.reg[i].loc.reg));
	else
	  _Unwind_SetGRPtr (context, i,
			    _Unwind_GetGRPtr (&orig_context,
					      fs->regs.reg[i].loc.reg));
	break;

      case REG_SAVED_EXP:
	{
	  const unsigned char *exp = fs->regs.reg[i].loc.exp;
	  _uleb128_t len;
	  _Unwind_Ptr val;

	  exp = read_uleb128 (exp, &len);
	  val = execute_stack_op (exp, exp + len, &orig_context,
				  (_Unwind_Ptr) cfa);
	  _Unwind_SetGRPtr (context, i, (void *) val);
	}
	break;

      case REG_SAVED_VAL_OFFSET:
	_Unwind_SetGRValue (context, i,
			    (_Unwind_Internal_Ptr)
			    (cfa + fs->regs.reg[i].loc.offset));
	break;

      case REG_SAVED_VAL_EXP:
	{
	  const unsigned char *exp = fs->regs.reg[i].loc.exp;
	  _uleb128_t len;
	  _Unwind_Ptr val;

	  exp = read_uleb128 (exp, &len);
	  val = execute_stack_op (exp, exp + len, &orig_context,
				  (_Unwind_Ptr) cfa);
	  _Unwind_SetGRValue (context, i, val);
	}
	break;
      }

  _Unwind_SetSignalFrame (context, fs->signal_frame);

#ifdef MD_FROB_UPDATE_CONTEXT
  MD_FROB_UPDATE_CONTEXT (context, fs);
#endif
}

/* CONTEXT describes the unwind state for a frame, and FS describes the FDE
   of its caller.  Update CONTEXT to refer to the caller as well.  Note
   that the args_size and lsda members are not updated here, but later in
   uw_frame_state_for.  */

static void
uw_update_context (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  uw_update_context_1 (context, fs);

  /* In general this unwinder doesn't make any distinction between
     undefined and same_value rule.  Call-saved registers are assumed
     to have same_value rule by default and explicit undefined
     rule is handled like same_value.  The only exception is
     DW_CFA_undefined on retaddr_column which is supposed to
     mark outermost frame in DWARF 3.  */
  if (fs->regs.how[DWARF_REG_TO_UNWIND_COLUMN (fs->retaddr_column)]
      == REG_UNDEFINED)
    /* uw_frame_state_for uses context->ra == 0 check to find outermost
       stack frame.  */
    context->ra = 0;
  else
    {
      /* Compute the return address now, since the return address column
	 can change from frame to frame.  */
      void *ret_addr;
#ifdef MD_DEMANGLE_RETURN_ADDR
      _Unwind_Word ra = _Unwind_GetGR (context, fs->retaddr_column);
      ret_addr = MD_DEMANGLE_RETURN_ADDR (context, fs, ra);
#else
      ret_addr = _Unwind_GetPtr (context, fs->retaddr_column);
#endif
      context->ra = __builtin_extract_return_addr (ret_addr);
    }
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
      /* Do any necessary initialization to access arbitrary stack frames. \
	 On the SPARC, this means flushing the register windows.  */	   \
      __builtin_unwind_init ();						   \
      uw_init_context_1 (CONTEXT, __builtin_dwarf_cfa (),		   \
			 __builtin_return_address (0));			   \
    }									   \
  while (0)

static inline void
init_dwarf_reg_size_table (void)
{
  __builtin_init_dwarf_reg_size_table (dwarf_reg_size_table);
}

static void __attribute__((noinline))
uw_init_context_1 (struct _Unwind_Context *context,
		   void *outer_cfa, void *outer_ra)
{
  void *ra = __builtin_extract_return_addr (__builtin_return_address (0));
  _Unwind_FrameState fs;
  _Unwind_SpTmp sp_slot;
  _Unwind_Reason_Code code;

  memset (context, 0, sizeof (struct _Unwind_Context));
  context->ra = ra;
  if (!ASSUME_EXTENDED_UNWIND_CONTEXT)
    context->flags = EXTENDED_CONTEXT_BIT;

  code = uw_frame_state_for (context, &fs);
  gcc_assert (code == _URC_NO_REASON);

#if __GTHREADS
  {
    static __gthread_once_t once_regsizes = __GTHREAD_ONCE_INIT;
    if (__gthread_once (&once_regsizes, init_dwarf_reg_size_table) != 0
	&& dwarf_reg_size_table[0] == 0)
      init_dwarf_reg_size_table ();
  }
#else
  if (dwarf_reg_size_table[0] == 0)
    init_dwarf_reg_size_table ();
#endif

  /* Force the frame state to use the known cfa value.  */
  _Unwind_SetSpColumn (context, outer_cfa, &sp_slot);
  fs.regs.cfa_how = CFA_REG_OFFSET;
  fs.regs.cfa_reg = __builtin_dwarf_sp_column ();
  fs.regs.cfa_offset = 0;

  uw_update_context_1 (context, &fs);

  /* If the return address column was saved in a register in the
     initialization context, then we can't see it in the given
     call frame data.  So have the initialization context tell us.  */
  context->ra = __builtin_extract_return_addr (outer_ra);
}

static void _Unwind_DebugHook (void *, void *)
  __attribute__ ((__noinline__, __used__, __noclone__));

/* This function is called during unwinding.  It is intended as a hook
   for a debugger to intercept exceptions.  CFA is the CFA of the
   target frame.  HANDLER is the PC to which control will be
   transferred.  */
static void
_Unwind_DebugHook (void *cfa __attribute__ ((__unused__)),
		   void *handler __attribute__ ((__unused__)))
{
  /* We only want to use stap probes starting with v3.  Earlier
     versions added too much startup cost.  */
#if defined (HAVE_SYS_SDT_H) && defined (STAP_PROBE2) && _SDT_NOTE_TYPE >= 3
  STAP_PROBE2 (libgcc, unwind, cfa, handler);
#else
  asm ("");
#endif
}

/* Install TARGET into CURRENT so that we can return to it.  This is a
   macro because __builtin_eh_return must be invoked in the context of
   our caller.  FRAMES is a number of frames to be unwind.
   _Unwind_Frames_Extra is a macro to do additional work during unwinding
   if needed, for example shadow stack pointer adjustment for Intel CET
   technology.  */

#define uw_install_context(CURRENT, TARGET, FRAMES)			\
  do									\
    {									\
      long offset = uw_install_context_1 ((CURRENT), (TARGET));		\
      void *handler = __builtin_frob_return_addr ((TARGET)->ra);	\
      _Unwind_DebugHook ((TARGET)->cfa, handler);			\
      _Unwind_Frames_Extra (FRAMES);					\
      __builtin_eh_return (offset, handler);				\
    }									\
  while (0)

static long
uw_install_context_1 (struct _Unwind_Context *current,
		      struct _Unwind_Context *target)
{
  long i;
  _Unwind_SpTmp sp_slot;

  /* If the target frame does not have a saved stack pointer,
     then set up the target's CFA.  */
  if (!_Unwind_GetGRPtr (target, __builtin_dwarf_sp_column ()))
    _Unwind_SetSpColumn (target, target->cfa, &sp_slot);

  for (i = 0; i < __LIBGCC_DWARF_FRAME_REGISTERS__; ++i)
    {
      void *c = (void *) (_Unwind_Internal_Ptr) current->reg[i];
      void *t = (void *) (_Unwind_Internal_Ptr)target->reg[i];

      gcc_assert (current->by_value[i] == 0);
      if (target->by_value[i] && c)
	{
	  _Unwind_Word w;
	  _Unwind_Ptr p;
	  if (dwarf_reg_size_table[i] == sizeof (_Unwind_Word))
	    {
	      w = (_Unwind_Internal_Ptr) t;
	      memcpy (c, &w, sizeof (_Unwind_Word));
	    }
	  else
	    {
	      gcc_assert (dwarf_reg_size_table[i] == sizeof (_Unwind_Ptr));
	      p = (_Unwind_Internal_Ptr) t;
	      memcpy (c, &p, sizeof (_Unwind_Ptr));
	    }
	}
      else if (t && c && t != c)
	memcpy (c, t, dwarf_reg_size_table[i]);
    }

  /* If the current frame doesn't have a saved stack pointer, then we
     need to rely on EH_RETURN_STACKADJ_RTX to get our target stack
     pointer value reloaded.  */
  if (!_Unwind_GetGRPtr (current, __builtin_dwarf_sp_column ()))
    {
      void *target_cfa;

      target_cfa = _Unwind_GetPtr (target, __builtin_dwarf_sp_column ());

      /* We adjust SP by the difference between CURRENT and TARGET's CFA.  */
      if (__LIBGCC_STACK_GROWS_DOWNWARD__)
	return target_cfa - current->cfa + target->args_size;
      else
	return current->cfa - target_cfa - target->args_size;
    }
  return 0;
}

static inline _Unwind_Ptr
uw_identify_context (struct _Unwind_Context *context)
{
  /* The CFA is not sufficient to disambiguate the context of a function
     interrupted by a signal before establishing its frame and the context
     of the signal itself.  */
  if (__LIBGCC_STACK_GROWS_DOWNWARD__)
    return _Unwind_GetCFA (context) - _Unwind_IsSignalFrame (context);
  else
    return _Unwind_GetCFA (context) + _Unwind_IsSignalFrame (context);
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
