/* DWARF2 exception handling and frame unwind runtime interface routines.
   Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "tconfig.h"
#include "tsystem.h"
#include "dwarf2.h"
#include "unwind.h"
#include "unwind-dw2-fde.h"
#include "gthr.h"


#if !USING_SJLJ_EXCEPTIONS

#ifndef STACK_GROWS_DOWNWARD
#define STACK_GROWS_DOWNWARD 0
#else
#undef STACK_GROWS_DOWNWARD
#define STACK_GROWS_DOWNWARD 1
#endif

/* A target can override (perhaps for backward compatibility) how
   many dwarf2 columns are unwound.  */
#ifndef DWARF_FRAME_REGISTERS
#define DWARF_FRAME_REGISTERS FIRST_PSEUDO_REGISTER
#endif

/* This is the register and unwind state for a particular frame.  */
struct _Unwind_Context
{
  void *reg[DWARF_FRAME_REGISTERS+1];
  void *cfa;
  void *ra;
  void *lsda;
  struct dwarf_eh_bases bases;
  _Unwind_Word args_size;
};

/* Byte size of every register managed by these routines.  */
static unsigned char dwarf_reg_size_table[DWARF_FRAME_REGISTERS];


/* The result of interpreting the frame unwind info for a frame.
   This is all symbolic at this point, as none of the values can
   be resolved until the target pc is located.  */
typedef struct
{
  /* Each register save state can be described in terms of a CFA slot,
     another register, or a location expression.  */
  struct frame_state_reg_info
  {
    struct {
      union {
	unsigned int reg;
	_Unwind_Sword offset;
	unsigned char *exp;
      } loc;
      enum {
	REG_UNSAVED,
	REG_SAVED_OFFSET,
	REG_SAVED_REG,
	REG_SAVED_EXP,
      } how;
    } reg[DWARF_FRAME_REGISTERS+1];

    /* Used to implement DW_CFA_remember_state.  */
    struct frame_state_reg_info *prev;
  } regs;

  /* The CFA can be described in terms of a reg+offset or a
     location expression.  */
  _Unwind_Sword cfa_offset;
  _Unwind_Word cfa_reg;
  unsigned char *cfa_exp;
  enum {
    CFA_UNSET,
    CFA_REG_OFFSET,
    CFA_EXP,
  } cfa_how;

  /* The PC described by the current frame state.  */
  void *pc;

  /* The information we care about from the CIE/FDE.  */
  _Unwind_Personality_Fn personality;
  signed int data_align;
  unsigned int code_align;
  unsigned char retaddr_column;
  unsigned char addr_encoding;
  unsigned char saw_z;
  unsigned char saw_lsda;
} _Unwind_FrameState;

/* Decode the unsigned LEB128 constant at BUF into the variable pointed to
   by R, and return the new value of BUF.  */

static unsigned char *
read_uleb128 (unsigned char *buf, _Unwind_Word *r)
{
  unsigned shift = 0;
  _Unwind_Word result = 0;

  while (1)
    {
      unsigned char byte = *buf++;
      result |= (byte & 0x7f) << shift;
      if ((byte & 0x80) == 0)
	break;
      shift += 7;
    }
  *r = result;
  return buf;
}

/* Decode the signed LEB128 constant at BUF into the variable pointed to
   by R, and return the new value of BUF.  */

static unsigned char *
read_sleb128 (unsigned char *buf, _Unwind_Sword *r)
{
  unsigned shift = 0;
  _Unwind_Sword result = 0;
  unsigned char byte;

  while (1)
    {
      byte = *buf++;
      result |= (byte & 0x7f) << shift;
      shift += 7;
      if ((byte & 0x80) == 0)
	break;
    }
  if (shift < (sizeof (*r) * 8) && (byte & 0x40) != 0)
    result |= - (1 << shift);

  *r = result;
  return buf;
}

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

static inline void *
read_pointer (void *p) { union unaligned *up = p; return up->p; }

static inline int
read_1u (void *p) { return *(unsigned char *)p; }

static inline int
read_1s (void *p) { return *(signed char *)p; }

static inline int
read_2u (void *p) { union unaligned *up = p; return up->u2; }

static inline int
read_2s (void *p) { union unaligned *up = p; return up->s2; }

static inline unsigned int
read_4u (void *p) { union unaligned *up = p; return up->u4; }

static inline int
read_4s (void *p) { union unaligned *up = p; return up->s4; }

static inline unsigned long
read_8u (void *p) { union unaligned *up = p; return up->u8; }

static inline unsigned long
read_8s (void *p) { union unaligned *up = p; return up->s8; }

static unsigned char *
read_encoded_pointer (unsigned char *p, unsigned char encoding,
		      struct dwarf_eh_bases *bases, void **pptr)
{
  signed long val;
  unsigned char *ret;
  
  switch (encoding & 0x0f)
    {
    case DW_EH_PE_absptr:
      val = (_Unwind_Ptr) read_pointer (p);
      ret = p + sizeof (void *);
      break;

    case DW_EH_PE_uleb128:
      ret = read_uleb128 (p, &val);
      break;
    case DW_EH_PE_sleb128:
      ret = read_sleb128 (p, &val);
      break;

    case DW_EH_PE_udata2:
      val = read_2u (p);
      ret = p + 2;
      break;
    case DW_EH_PE_udata4:
      val = read_4u (p);
      ret = p + 4;
      break;
    case DW_EH_PE_udata8:
      val = read_8u (p);
      ret = p + 8;
      break;

    case DW_EH_PE_sdata2:
      val = read_2s (p);
      ret = p + 2;
      break;
    case DW_EH_PE_sdata4:
      val = read_4s (p);
      ret = p + 4;
      break;
    case DW_EH_PE_sdata8:
      val = read_8s (p);
      ret = p + 8;
      break;

    default:
      abort ();
    }

  if (val != 0)
    switch (encoding & 0xf0)
      {
      case DW_EH_PE_absptr:
	break;
      case DW_EH_PE_pcrel:
	val += (_Unwind_Ptr) p;
	break;
      case DW_EH_PE_textrel:
	val += (_Unwind_Ptr) bases->tbase;
	break;
      case DW_EH_PE_datarel:
	val += (_Unwind_Ptr) bases->dbase;
	break;
      case DW_EH_PE_funcrel:
	val += (_Unwind_Ptr) bases->func;
	break;
      default:
	abort ();
      }

  *pptr = (void *) (_Unwind_Ptr) val;
  return ret;
}

/* Get the value of register REG as saved in CONTEXT.  */

inline _Unwind_Word
_Unwind_GetGR (struct _Unwind_Context *context, int index)
{
  /* This will segfault if the register hasn't been saved.  */
  return * (_Unwind_Word *) context->reg[index];
}

/* Overwrite the saved value for register REG in CONTEXT with VAL.  */

inline void
_Unwind_SetGR (struct _Unwind_Context *context, int index, _Unwind_Word val)
{
  * (_Unwind_Word *) context->reg[index] = val;
}

/* Retrieve the return address for CONTEXT.  */

inline _Unwind_Ptr
_Unwind_GetIP (struct _Unwind_Context *context)
{
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


/* Extract any interesting information from the CIE for the translation
   unit F belongs to.  Return a pointer to the byte after the augmentation,
   or NULL if we encountered an undecipherable augmentation.  */

static unsigned char *
extract_cie_info (struct dwarf_cie *cie, struct _Unwind_Context *context,
		  _Unwind_FrameState *fs)
{
  unsigned char *aug = cie->augmentation;
  unsigned char *p = aug + strlen (aug) + 1;
  unsigned char *ret = NULL;
  _Unwind_Word code_align;
  _Unwind_Sword data_align;

  /* Immediately following the augmentation are the code and
     data alignment and return address column.  */
  p = read_uleb128 (p, &code_align);
  p = read_sleb128 (p, &data_align);
  fs->code_align = code_align;
  fs->data_align = data_align;
  fs->retaddr_column = *p++;

  /* If the augmentation starts with 'z', then a uleb128 immediately
     follows containing the length of the augmentation field following
     the size.  */
  if (*aug == 'z')
    {
      _Unwind_Word i;
      p = read_uleb128 (p, &i);
      ret = p + i;

      fs->saw_z = 1;
      ++aug;
    }

  /* Iterate over recognized augmentation subsequences.  */
  while (*aug != '\0')
    {
      /* "eh" was used by g++ v2; recognize and skip.  */
      if (aug[0] == 'e' && aug[1] == 'h')
	{
	  p += sizeof (void *);
	  aug += 2;
	}

      /* "R" indicates a byte indicating how addresses are encoded.  */
      else if (aug[0] == 'R')
	{
	  fs->addr_encoding = *p++;
	  aug += 1;
	}

      /* "P" indicates a personality routine in the CIE augmentation
	 and an lsda pointer in the FDE augmentation.  */
      else if (aug[0] == 'P')
	{
	  p = read_encoded_pointer (p, fs->addr_encoding, &context->bases,
				    (void **) &fs->personality);
	  fs->saw_lsda = 1;
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
execute_stack_op (unsigned char *op_ptr, unsigned char *op_end,
		  struct _Unwind_Context *context, _Unwind_Word initial)
{
  _Unwind_Word stack[64];	/* ??? Assume this is enough. */
  int stack_elt;

  stack[0] = initial;
  stack_elt = 1;

  while (op_ptr < op_end)
    {
      enum dwarf_location_atom op = *op_ptr++;
      _Unwind_Word result, reg;
      _Unwind_Sword offset;

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
	  op_ptr = read_uleb128 (op_ptr, &result);
	  break;
	case DW_OP_consts:
	  op_ptr = read_sleb128 (op_ptr, &offset);
	  result = offset;
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
	  result = _Unwind_GetGR (context, reg) + offset;
	  break;

	case DW_OP_dup:
	  if (stack_elt < 1)
	    abort ();
	  result = stack[stack_elt - 1];
	  break;

	case DW_OP_drop:
	  if (--stack_elt < 0)
	    abort ();
	  goto no_push;

	case DW_OP_pick:
	  offset = *op_ptr++;
	  if (offset >= stack_elt - 1)
	    abort ();
	  result = stack[stack_elt - 1 - offset];
	  break;

	case DW_OP_over:
	  if (stack_elt < 2)
	    abort ();
	  result = stack[stack_elt - 2];
	  break;

	case DW_OP_rot:
	  {
	    _Unwind_Word t1, t2, t3;

	    if (stack_elt < 3)
	      abort ();
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
	  if (--stack_elt < 0)
	    abort ();
	  result = stack[stack_elt];

	  switch (op)
	    {
	    case DW_OP_deref:
	      {
		void *ptr = (void *)(_Unwind_Ptr) result;
		result = (_Unwind_Ptr) read_pointer (ptr);
	      }
	      break;

	    case DW_OP_deref_size:
	      {
		void *ptr = (void *)(_Unwind_Ptr) result;
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
		    abort ();
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
	      op_ptr = read_uleb128 (op_ptr, &reg);
	      result += reg;
	      break;
	    }
	  break;

	case DW_OP_and:
	case DW_OP_div:
	case DW_OP_minus:
	case DW_OP_mod:
	case DW_OP_mul:
	case DW_OP_or:
	case DW_OP_plus:
	case DW_OP_le:
	case DW_OP_ge:
	case DW_OP_eq:
	case DW_OP_lt:
	case DW_OP_gt:
	case DW_OP_ne:
	  {
	    /* Binary operations.  */
	    _Unwind_Word first, second;
	  if ((stack_elt -= 2) < 0)
	    abort ();
	  second = stack[stack_elt];
	  first = stack[stack_elt + 1];

	  switch (op)
	    {
	    case DW_OP_and:
	      result = second & first;
	      break;
	    case DW_OP_div:
	      result = (_Unwind_Sword)second / (_Unwind_Sword)first;
	      break;
	    case DW_OP_minus:
	      result = second - first;
	      break;
	    case DW_OP_mod:
	      result = (_Unwind_Sword)second % (_Unwind_Sword)first;
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
	      result = (_Unwind_Sword)second >> first;
	      break;
	    case DW_OP_xor:
	      result = second ^ first;
	      break;
	    case DW_OP_le:
	      result = (_Unwind_Sword)first <= (_Unwind_Sword)second;
	      break;
	    case DW_OP_ge:
	      result = (_Unwind_Sword)first >= (_Unwind_Sword)second;
	      break;
	    case DW_OP_eq:
	      result = (_Unwind_Sword)first == (_Unwind_Sword)second;
	      break;
	    case DW_OP_lt:
	      result = (_Unwind_Sword)first < (_Unwind_Sword)second;
	      break;
	    case DW_OP_gt:
	      result = (_Unwind_Sword)first > (_Unwind_Sword)second;
	      break;
	    case DW_OP_ne:
	      result = (_Unwind_Sword)first != (_Unwind_Sword)second;
	      break;
	    }
	  }
	  break;

	case DW_OP_skip:
	  offset = read_2s (op_ptr);
	  op_ptr += 2;
	  op_ptr += offset;
	  goto no_push;

	case DW_OP_bra:
	  if (--stack_elt < 0)
	    abort ();
	  offset = read_2s (op_ptr);
	  op_ptr += 2;
	  if (stack[stack_elt] != 0)
	    op_ptr += offset;
	  goto no_push;

	case DW_OP_nop:
	  goto no_push;

	default:
	  abort ();
	}

      /* Most things push a result value.  */
      if ((size_t) stack_elt >= sizeof(stack)/sizeof(*stack))
	abort ();
      stack[++stack_elt] = result;
    no_push:;
    }

  /* We were executing this program to get a value.  It should be
     at top of stack.  */
  if (--stack_elt < 0)
    abort ();
  return stack[stack_elt];
}


/* Decode DWARF 2 call frame information. Takes pointers the
   instruction sequence to decode, current register information and
   CIE info, and the PC range to evaluate.  */

static void
execute_cfa_program (unsigned char *insn_ptr, unsigned char *insn_end,
		     struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct frame_state_reg_info *unused_rs = NULL;

  /* Don't allow remember/restore between CIE and FDE programs.  */
  fs->regs.prev = NULL;

  while (insn_ptr < insn_end && fs->pc < context->ra)
    {
      unsigned char insn = *insn_ptr++;
      _Unwind_Word reg, uoffset;
      _Unwind_Sword offset;

      if (insn & DW_CFA_advance_loc)
	fs->pc += (insn & 0x3f) * fs->code_align;
      else if (insn & DW_CFA_offset)
	{
	  reg = insn & 0x3f;
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  offset = (_Unwind_Sword)uoffset * fs->data_align;
	  fs->regs.reg[reg].how = REG_SAVED_OFFSET;
	  fs->regs.reg[reg].loc.offset = offset;
	}
      else if (insn & DW_CFA_restore)
	{
	  reg = insn & 0x3f;
	  fs->regs.reg[reg].how = REG_UNSAVED;
	}
      else switch (insn)
	{
	case DW_CFA_set_loc:
	  insn_ptr = read_encoded_pointer (insn_ptr, fs->addr_encoding,
					   &context->bases, &fs->pc);
	  break;

	case DW_CFA_advance_loc1:
	  fs->pc += read_1u (insn_ptr);
	  insn_ptr += 1;
	  break;
	case DW_CFA_advance_loc2:
	  fs->pc += read_2u (insn_ptr);
	  insn_ptr += 2;
	  break;
	case DW_CFA_advance_loc4:
	  fs->pc += read_4u (insn_ptr);
	  insn_ptr += 4;
	  break;

	case DW_CFA_offset_extended:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  offset = (_Unwind_Sword)uoffset * fs->data_align;
	  fs->regs.reg[reg].how = REG_SAVED_OFFSET;
	  fs->regs.reg[reg].loc.offset = offset;
	  break;

	case DW_CFA_restore_extended:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  fs->regs.reg[reg].how = REG_UNSAVED;
	  break;

	case DW_CFA_undefined:
	case DW_CFA_same_value:
	case DW_CFA_nop:
	  break;

	case DW_CFA_register:
	  {
	    _Unwind_Word reg2;
	    insn_ptr = read_uleb128 (insn_ptr, &reg);
	    insn_ptr = read_uleb128 (insn_ptr, &reg2);
	    fs->regs.reg[reg].how = REG_SAVED_REG;
	    fs->regs.reg[reg].loc.reg = reg2;
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
	  insn_ptr = read_uleb128 (insn_ptr, &fs->cfa_reg);
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  fs->cfa_offset = uoffset;
	  fs->cfa_how = CFA_REG_OFFSET;
	  break;

	case DW_CFA_def_cfa_register:
	  insn_ptr = read_uleb128 (insn_ptr, &fs->cfa_reg);
	  fs->cfa_how = CFA_REG_OFFSET;
	  break;

	case DW_CFA_def_cfa_offset:
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  fs->cfa_offset = uoffset;
	  /* cfa_how deliberately not set.  */
	  break;

	case DW_CFA_def_cfa_expression:
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  fs->cfa_exp = insn_ptr;
	  fs->cfa_how = CFA_EXP;
	  insn_ptr += uoffset;
	  break;

	case DW_CFA_expression:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  fs->regs.reg[reg].how = REG_SAVED_EXP;
	  fs->regs.reg[reg].loc.exp = insn_ptr;
	  insn_ptr += uoffset;
	  break;

	  /* From the 2.1 draft.  */
	case DW_CFA_offset_extended_sf:
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_sleb128 (insn_ptr, &offset);
	  offset *= fs->data_align;
	  fs->regs.reg[reg].how = REG_SAVED_OFFSET;
	  fs->regs.reg[reg].loc.offset = offset;
	  break;
	  
	case DW_CFA_def_cfa_sf:
	  insn_ptr = read_uleb128 (insn_ptr, &fs->cfa_reg);
	  insn_ptr = read_sleb128 (insn_ptr, &fs->cfa_offset);
	  fs->cfa_how = CFA_REG_OFFSET;
	  break;

	case DW_CFA_def_cfa_offset_sf:
	  insn_ptr = read_uleb128 (insn_ptr, &fs->cfa_offset);
	  /* cfa_how deliberately not set.  */
	  break;

	case DW_CFA_GNU_window_save:
	  /* ??? Hardcoded for SPARC register window configuration.  */
	  for (reg = 16; reg < 32; ++reg)
	    {
	      fs->regs.reg[reg].how = REG_SAVED_OFFSET;
	      fs->regs.reg[reg].loc.offset = (reg - 16) * sizeof (void *);
	    }
	  break;

	case DW_CFA_GNU_args_size:
	  insn_ptr = read_uleb128 (insn_ptr, &context->args_size);
	  break;

	case DW_CFA_GNU_negative_offset_extended:
	  /* Obsoleted by DW_CFA_offset_extended_sf, but used by
	     older PowerPC code.  */
	  insn_ptr = read_uleb128 (insn_ptr, &reg);
	  insn_ptr = read_uleb128 (insn_ptr, &uoffset);
	  offset = (_Unwind_Sword)uoffset * fs->data_align;
	  fs->regs.reg[reg].how = REG_SAVED_OFFSET;
	  fs->regs.reg[reg].loc.offset = -offset;
	  break;

	default:
	  abort ();
	}
    }
}

static _Unwind_Reason_Code
uw_frame_state_for (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct dwarf_fde *fde;
  struct dwarf_cie *cie;
  unsigned char *aug, *insn, *end;

  memset (fs, 0, sizeof (*fs));
  context->args_size = 0;
  context->lsda = 0;

  fde = _Unwind_Find_FDE (context->ra - 1, &context->bases);
  if (fde == NULL)
    {
      /* Couldn't find frame unwind info for this function.  Try a
	 target-specific fallback mechanism.  This will necessarily
	 not profide a personality routine or LSDA.  */
#ifdef MD_FALLBACK_FRAME_STATE_FOR
      MD_FALLBACK_FRAME_STATE_FOR (context, fs, success);
      return _URC_END_OF_STACK;
    success:
      return _URC_NO_REASON;
#else
      return _URC_END_OF_STACK;
#endif
    }

  context->bases.func = fde->pc_begin;
  fs->pc = fde->pc_begin;

  cie = get_cie (fde);
  insn = extract_cie_info (cie, context, fs);
  if (insn == NULL)
    /* CIE contained unknown augmentation.  */
    return _URC_FATAL_PHASE1_ERROR;

  /* First decode all the insns in the CIE.  */
  end = (unsigned char *) next_fde ((struct dwarf_fde *) cie);
  execute_cfa_program (insn, end, context, fs);

  /* Locate augmentation for the fde.  */
  aug = (unsigned char *)fde + sizeof (*fde);
  insn = NULL;
  if (fs->saw_z)
    {
      _Unwind_Word i;
      aug = read_uleb128 (aug, &i);
      insn = aug + i;
    }
  if (fs->saw_lsda)
    aug = read_encoded_pointer (aug, fs->addr_encoding,
				&context->bases, &context->lsda);

  /* Then the insns in the FDE up to our target PC.  */
  if (insn == NULL)
    insn = aug;
  end = (unsigned char *) next_fde (fde);
  execute_cfa_program (insn, end, context, fs);

  return _URC_NO_REASON;
}


static void
uw_update_context_1 (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  struct _Unwind_Context orig_context = *context;
  void *cfa;
  long i;

  /* Compute this frame's CFA.  */
  switch (fs->cfa_how)
    {
    case CFA_REG_OFFSET:
      /* Special handling here: Many machines do not use a frame pointer,
	 and track the CFA only through offsets from the stack pointer from
	 one frame to the next.  In this case, the stack pointer is never
	 stored, so it has no saved address in the context.  What we do 
	 have is the CFA from the previous stack frame.  */
      if (context->reg[fs->cfa_reg] == NULL)
	cfa = context->cfa;
      else
	cfa = (void *) (_Unwind_Ptr) _Unwind_GetGR (context, fs->cfa_reg);
      cfa += fs->cfa_offset;
      break;

    case CFA_EXP:
      /* ??? No way of knowing what register number is the stack pointer
	 to do the same sort of handling as above.  Assume that if the
	 CFA calculation is so complicated as to require a stack program
	 that this will not be a problem.  */
      {
	unsigned char *exp = fs->cfa_exp;
	_Unwind_Word len;

	exp = read_uleb128 (exp, &len);
	cfa = (void *) (_Unwind_Ptr)
	  execute_stack_op (exp, exp + len, context, 0);
	break;
      }

    default:
      abort ();
    }
  context->cfa = cfa;

  /* Compute the addresses of all registers saved in this frame.  */
  for (i = 0; i < DWARF_FRAME_REGISTERS + 1; ++i)
    switch (fs->regs.reg[i].how)
      {
      case REG_UNSAVED:
	break;
      case REG_SAVED_OFFSET:
	context->reg[i] = cfa + fs->regs.reg[i].loc.offset;
	break;
      case REG_SAVED_REG:
	context->reg[i] = orig_context.reg[fs->regs.reg[i].loc.reg];
	break;
      case REG_SAVED_EXP:
	{
	  unsigned char *exp = fs->regs.reg[i].loc.exp;
	  _Unwind_Word len;
	  _Unwind_Ptr val;

	  exp = read_uleb128 (exp, &len);
	  val = execute_stack_op (exp, exp + len, &orig_context,
				  (_Unwind_Ptr) cfa);
	  context->reg[i] = (void *) val;
	}
	break;
      }
}

static void
uw_update_context (struct _Unwind_Context *context, _Unwind_FrameState *fs)
{
  uw_update_context_1 (context, fs);

  /* Compute the return address now, since the return address column
     can change from frame to frame.  */
  context->ra = __builtin_extract_return_addr
    ((void *) (_Unwind_Ptr) _Unwind_GetGR (context, fs->retaddr_column));
}

/* Fill in CONTEXT for top-of-stack.  The only valid registers at this
   level will be the return address and the CFA.  */
   
#define uw_init_context(CONTEXT)					\
do {									\
  /* Do any necessary initialization to access arbitrary stack frames.	\
     On the SPARC, this means flushing the register windows.  */	\
  __builtin_unwind_init ();						\
  uw_init_context_1 (CONTEXT, __builtin_dwarf_cfa (),			\
		     __builtin_return_address (0));			\
} while (0)

static void
uw_init_context_1 (struct _Unwind_Context *context,
		   void *outer_cfa, void *outer_ra)
{
  void *ra = __builtin_extract_return_addr (__builtin_return_address (0));
  _Unwind_FrameState fs;

  memset (context, 0, sizeof (struct _Unwind_Context));
  context->ra = ra;

  if (uw_frame_state_for (context, &fs) != _URC_NO_REASON)
    abort ();

  /* Force the frame state to use the known cfa value.  */
  context->cfa = outer_cfa;
  fs.cfa_how = CFA_REG_OFFSET;
  fs.cfa_reg = 0;
  fs.cfa_offset = 0;

  uw_update_context_1 (context, &fs);

  /* If the return address column was saved in a register in the
     initialization context, then we can't see it in the given
     call frame data.  So have the initialization context tell us.  */
  context->ra = __builtin_extract_return_addr (outer_ra);
}


/* Install TARGET into CURRENT so that we can return to it.  This is a
   macro because __builtin_eh_return must be invoked in the context of
   our caller.  */

#define uw_install_context(CURRENT, TARGET)				\
do {									\
  long offset = uw_install_context_1 ((CURRENT), (TARGET));		\
  void *handler = __builtin_frob_return_addr ((TARGET)->ra);		\
  __builtin_eh_return (offset, handler);				\
} while (0)

static inline void
init_dwarf_reg_size_table (void)
{
  __builtin_init_dwarf_reg_size_table (dwarf_reg_size_table);
}

static long
uw_install_context_1 (struct _Unwind_Context *current,
		      struct _Unwind_Context *target)
{
  long i;

#if __GTHREADS
  {
    static __gthread_once_t once_regsizes = __GTHREAD_ONCE_INIT;
    if (__gthread_once (&once_regsizes, init_dwarf_reg_size_table) != 0
	|| dwarf_reg_size_table[0] == 0)
      init_dwarf_reg_size_table ();
  }
#else
  if (dwarf_reg_size_table[0] == 0)
    init_dwarf_reg_size_table ();
#endif

  for (i = 0; i < DWARF_FRAME_REGISTERS; ++i)
    {
      void *c = current->reg[i];
      void *t = target->reg[i];
      if (t && c && t != c)
	memcpy (c, t, dwarf_reg_size_table[i]);
    }

  /* We adjust SP by the difference between CURRENT and TARGET's CFA.  */
  if (STACK_GROWS_DOWNWARD)
    return target->cfa - current->cfa + target->args_size;
  else
    return current->cfa - target->cfa - target->args_size;
}

static inline _Unwind_Ptr
uw_identify_context (struct _Unwind_Context *context)
{
  return _Unwind_GetIP (context);
}


#include "unwind.inc"

#endif /* !USING_SJLJ_EXCEPTIONS */
