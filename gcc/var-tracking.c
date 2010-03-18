/* Variable tracking routines for the GNU compiler.
   Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This file contains the variable tracking pass.  It computes where
   variables are located (which registers or where in memory) at each position
   in instruction stream and emits notes describing the locations.
   Debug information (DWARF2 location lists) is finally generated from
   these notes.
   With this debug information, it is possible to show variables
   even when debugging optimized code.

   How does the variable tracking pass work?

   First, it scans RTL code for uses, stores and clobbers (register/memory
   references in instructions), for call insns and for stack adjustments
   separately for each basic block and saves them to an array of micro
   operations.
   The micro operations of one instruction are ordered so that
   pre-modifying stack adjustment < use < use with no var < call insn <
     < set < clobber < post-modifying stack adjustment

   Then, a forward dataflow analysis is performed to find out how locations
   of variables change through code and to propagate the variable locations
   along control flow graph.
   The IN set for basic block BB is computed as a union of OUT sets of BB's
   predecessors, the OUT set for BB is copied from the IN set for BB and
   is changed according to micro operations in BB.

   The IN and OUT sets for basic blocks consist of a current stack adjustment
   (used for adjusting offset of variables addressed using stack pointer),
   the table of structures describing the locations of parts of a variable
   and for each physical register a linked list for each physical register.
   The linked list is a list of variable parts stored in the register,
   i.e. it is a list of triplets (reg, decl, offset) where decl is
   REG_EXPR (reg) and offset is REG_OFFSET (reg).  The linked list is used for
   effective deleting appropriate variable parts when we set or clobber the
   register.

   There may be more than one variable part in a register.  The linked lists
   should be pretty short so it is a good data structure here.
   For example in the following code, register allocator may assign same
   register to variables A and B, and both of them are stored in the same
   register in CODE:

     if (cond)
       set A;
     else
       set B;
     CODE;
     if (cond)
       use A;
     else
       use B;

   Finally, the NOTE_INSN_VAR_LOCATION notes describing the variable locations
   are emitted to appropriate positions in RTL code.  Each such a note describes
   the location of one variable at the point in instruction stream where the
   note is.  There is no need to emit a note for each variable before each
   instruction, we only emit these notes where the location of variable changes
   (this means that we also emit notes for changes between the OUT set of the
   previous block and the IN set of the current block).

   The notes consist of two parts:
   1. the declaration (from REG_EXPR or MEM_EXPR)
   2. the location of a variable - it is either a simple register/memory
      reference (for simple variables, for example int),
      or a parallel of register/memory references (for a large variables
      which consist of several parts, for example long long).

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "flags.h"
#include "output.h"
#include "insn-config.h"
#include "reload.h"
#include "sbitmap.h"
#include "alloc-pool.h"
#include "fibheap.h"
#include "hashtab.h"
#include "regs.h"
#include "expr.h"
#include "timevar.h"
#include "tree-pass.h"
#include "tree-flow.h"
#include "cselib.h"
#include "target.h"
#include "toplev.h"
#include "params.h"
#include "diagnostic.h"
#include "pointer-set.h"
#include "recog.h"

/* var-tracking.c assumes that tree code with the same value as VALUE rtx code
   has no chance to appear in REG_EXPR/MEM_EXPRs and isn't a decl.
   Currently the value is the same as IDENTIFIER_NODE, which has such
   a property.  If this compile time assertion ever fails, make sure that
   the new tree code that equals (int) VALUE has the same property.  */
extern char check_value_val[(int) VALUE == (int) IDENTIFIER_NODE ? 1 : -1];

/* Type of micro operation.  */
enum micro_operation_type
{
  MO_USE,	/* Use location (REG or MEM).  */
  MO_USE_NO_VAR,/* Use location which is not associated with a variable
		   or the variable is not trackable.  */
  MO_VAL_USE,	/* Use location which is associated with a value.  */
  MO_VAL_LOC,   /* Use location which appears in a debug insn.  */
  MO_VAL_SET,	/* Set location associated with a value.  */
  MO_SET,	/* Set location.  */
  MO_COPY,	/* Copy the same portion of a variable from one
		   location to another.  */
  MO_CLOBBER,	/* Clobber location.  */
  MO_CALL,	/* Call insn.  */
  MO_ADJUST	/* Adjust stack pointer.  */

};

static const char * const ATTRIBUTE_UNUSED
micro_operation_type_name[] = {
  "MO_USE",
  "MO_USE_NO_VAR",
  "MO_VAL_USE",
  "MO_VAL_LOC",
  "MO_VAL_SET",
  "MO_SET",
  "MO_COPY",
  "MO_CLOBBER",
  "MO_CALL",
  "MO_ADJUST"
};

/* Where shall the note be emitted?  BEFORE or AFTER the instruction.
   Notes emitted as AFTER_CALL are to take effect during the call,
   rather than after the call.  */
enum emit_note_where
{
  EMIT_NOTE_BEFORE_INSN,
  EMIT_NOTE_AFTER_INSN,
  EMIT_NOTE_AFTER_CALL_INSN
};

/* Structure holding information about micro operation.  */
typedef struct micro_operation_def
{
  /* Type of micro operation.  */
  enum micro_operation_type type;

  /* The instruction which the micro operation is in, for MO_USE,
     MO_USE_NO_VAR, MO_CALL and MO_ADJUST, or the subsequent
     instruction or note in the original flow (before any var-tracking
     notes are inserted, to simplify emission of notes), for MO_SET
     and MO_CLOBBER.  */
  rtx insn;

  union {
    /* Location.  For MO_SET and MO_COPY, this is the SET that
       performs the assignment, if known, otherwise it is the target
       of the assignment.  For MO_VAL_USE and MO_VAL_SET, it is a
       CONCAT of the VALUE and the LOC associated with it.  For
       MO_VAL_LOC, it is a CONCAT of the VALUE and the VAR_LOCATION
       associated with it.  */
    rtx loc;

    /* Stack adjustment.  */
    HOST_WIDE_INT adjust;
  } u;
} micro_operation;

DEF_VEC_O(micro_operation);
DEF_VEC_ALLOC_O(micro_operation,heap);

/* A declaration of a variable, or an RTL value being handled like a
   declaration.  */
typedef void *decl_or_value;

/* Structure for passing some other parameters to function
   emit_note_insn_var_location.  */
typedef struct emit_note_data_def
{
  /* The instruction which the note will be emitted before/after.  */
  rtx insn;

  /* Where the note will be emitted (before/after insn)?  */
  enum emit_note_where where;

  /* The variables and values active at this point.  */
  htab_t vars;
} emit_note_data;

/* Description of location of a part of a variable.  The content of a physical
   register is described by a chain of these structures.
   The chains are pretty short (usually 1 or 2 elements) and thus
   chain is the best data structure.  */
typedef struct attrs_def
{
  /* Pointer to next member of the list.  */
  struct attrs_def *next;

  /* The rtx of register.  */
  rtx loc;

  /* The declaration corresponding to LOC.  */
  decl_or_value dv;

  /* Offset from start of DECL.  */
  HOST_WIDE_INT offset;
} *attrs;

/* Structure holding a refcounted hash table.  If refcount > 1,
   it must be first unshared before modified.  */
typedef struct shared_hash_def
{
  /* Reference count.  */
  int refcount;

  /* Actual hash table.  */
  htab_t htab;
} *shared_hash;

/* Structure holding the IN or OUT set for a basic block.  */
typedef struct dataflow_set_def
{
  /* Adjustment of stack offset.  */
  HOST_WIDE_INT stack_adjust;

  /* Attributes for registers (lists of attrs).  */
  attrs regs[FIRST_PSEUDO_REGISTER];

  /* Variable locations.  */
  shared_hash vars;

  /* Vars that is being traversed.  */
  shared_hash traversed_vars;
} dataflow_set;

/* The structure (one for each basic block) containing the information
   needed for variable tracking.  */
typedef struct variable_tracking_info_def
{
  /* The vector of micro operations.  */
  VEC(micro_operation, heap) *mos;

  /* The IN and OUT set for dataflow analysis.  */
  dataflow_set in;
  dataflow_set out;

  /* The permanent-in dataflow set for this block.  This is used to
     hold values for which we had to compute entry values.  ??? This
     should probably be dynamically allocated, to avoid using more
     memory in non-debug builds.  */
  dataflow_set *permp;

  /* Has the block been visited in DFS?  */
  bool visited;

  /* Has the block been flooded in VTA?  */
  bool flooded;

} *variable_tracking_info;

/* Structure for chaining the locations.  */
typedef struct location_chain_def
{
  /* Next element in the chain.  */
  struct location_chain_def *next;

  /* The location (REG, MEM or VALUE).  */
  rtx loc;

  /* The "value" stored in this location.  */
  rtx set_src;

  /* Initialized? */
  enum var_init_status init;
} *location_chain;

/* Structure describing one part of variable.  */
typedef struct variable_part_def
{
  /* Chain of locations of the part.  */
  location_chain loc_chain;

  /* Location which was last emitted to location list.  */
  rtx cur_loc;

  /* The offset in the variable.  */
  HOST_WIDE_INT offset;
} variable_part;

/* Maximum number of location parts.  */
#define MAX_VAR_PARTS 16

/* Structure describing where the variable is located.  */
typedef struct variable_def
{
  /* The declaration of the variable, or an RTL value being handled
     like a declaration.  */
  decl_or_value dv;

  /* Reference count.  */
  int refcount;

  /* Number of variable parts.  */
  char n_var_parts;

  /* True if this variable changed (any of its) cur_loc fields
     during the current emit_notes_for_changes resp.
     emit_notes_for_differences call.  */
  bool cur_loc_changed;

  /* True if this variable_def struct is currently in the
     changed_variables hash table.  */
  bool in_changed_variables;

  /* The variable parts.  */
  variable_part var_part[1];
} *variable;
typedef const struct variable_def *const_variable;

/* Structure for chaining backlinks from referenced VALUEs to
   DVs that are referencing them.  */
typedef struct value_chain_def
{
  /* Next value_chain entry.  */
  struct value_chain_def *next;

  /* The declaration of the variable, or an RTL value
     being handled like a declaration, whose var_parts[0].loc_chain
     references the VALUE owning this value_chain.  */
  decl_or_value dv;

  /* Reference count.  */
  int refcount;
} *value_chain;
typedef const struct value_chain_def *const_value_chain;

/* Pointer to the BB's information specific to variable tracking pass.  */
#define VTI(BB) ((variable_tracking_info) (BB)->aux)

/* Macro to access MEM_OFFSET as an HOST_WIDE_INT.  Evaluates MEM twice.  */
#define INT_MEM_OFFSET(mem) (MEM_OFFSET (mem) ? INTVAL (MEM_OFFSET (mem)) : 0)

/* Alloc pool for struct attrs_def.  */
static alloc_pool attrs_pool;

/* Alloc pool for struct variable_def with MAX_VAR_PARTS entries.  */
static alloc_pool var_pool;

/* Alloc pool for struct variable_def with a single var_part entry.  */
static alloc_pool valvar_pool;

/* Alloc pool for struct location_chain_def.  */
static alloc_pool loc_chain_pool;

/* Alloc pool for struct shared_hash_def.  */
static alloc_pool shared_hash_pool;

/* Alloc pool for struct value_chain_def.  */
static alloc_pool value_chain_pool;

/* Changed variables, notes will be emitted for them.  */
static htab_t changed_variables;

/* Links from VALUEs to DVs referencing them in their current loc_chains.  */
static htab_t value_chains;

/* Shall notes be emitted?  */
static bool emit_notes;

/* Empty shared hashtable.  */
static shared_hash empty_shared_hash;

/* Scratch register bitmap used by cselib_expand_value_rtx.  */
static bitmap scratch_regs = NULL;

/* Variable used to tell whether cselib_process_insn called our hook.  */
static bool cselib_hook_called;

/* Local function prototypes.  */
static void stack_adjust_offset_pre_post (rtx, HOST_WIDE_INT *,
					  HOST_WIDE_INT *);
static void insn_stack_adjust_offset_pre_post (rtx, HOST_WIDE_INT *,
					       HOST_WIDE_INT *);
static bool vt_stack_adjustments (void);
static rtx compute_cfa_pointer (HOST_WIDE_INT);
static hashval_t variable_htab_hash (const void *);
static int variable_htab_eq (const void *, const void *);
static void variable_htab_free (void *);

static void init_attrs_list_set (attrs *);
static void attrs_list_clear (attrs *);
static attrs attrs_list_member (attrs, decl_or_value, HOST_WIDE_INT);
static void attrs_list_insert (attrs *, decl_or_value, HOST_WIDE_INT, rtx);
static void attrs_list_copy (attrs *, attrs);
static void attrs_list_union (attrs *, attrs);

static void **unshare_variable (dataflow_set *set, void **slot, variable var,
				enum var_init_status);
static int vars_copy_1 (void **, void *);
static void vars_copy (htab_t, htab_t);
static tree var_debug_decl (tree);
static void var_reg_set (dataflow_set *, rtx, enum var_init_status, rtx);
static void var_reg_delete_and_set (dataflow_set *, rtx, bool,
				    enum var_init_status, rtx);
static void var_reg_delete (dataflow_set *, rtx, bool);
static void var_regno_delete (dataflow_set *, int);
static void var_mem_set (dataflow_set *, rtx, enum var_init_status, rtx);
static void var_mem_delete_and_set (dataflow_set *, rtx, bool,
				    enum var_init_status, rtx);
static void var_mem_delete (dataflow_set *, rtx, bool);

static void dataflow_set_init (dataflow_set *);
static void dataflow_set_clear (dataflow_set *);
static void dataflow_set_copy (dataflow_set *, dataflow_set *);
static int variable_union_info_cmp_pos (const void *, const void *);
static int variable_union (void **, void *);
static void dataflow_set_union (dataflow_set *, dataflow_set *);
static location_chain find_loc_in_1pdv (rtx, variable, htab_t);
static bool canon_value_cmp (rtx, rtx);
static int loc_cmp (rtx, rtx);
static bool variable_part_different_p (variable_part *, variable_part *);
static bool onepart_variable_different_p (variable, variable);
static bool variable_different_p (variable, variable);
static int dataflow_set_different_1 (void **, void *);
static bool dataflow_set_different (dataflow_set *, dataflow_set *);
static void dataflow_set_destroy (dataflow_set *);

static bool contains_symbol_ref (rtx);
static bool track_expr_p (tree, bool);
static bool same_variable_part_p (rtx, tree, HOST_WIDE_INT);
static int add_uses (rtx *, void *);
static void add_uses_1 (rtx *, void *);
static void add_stores (rtx, const_rtx, void *);
static bool compute_bb_dataflow (basic_block);
static bool vt_find_locations (void);

static void dump_attrs_list (attrs);
static int dump_var_slot (void **, void *);
static void dump_var (variable);
static void dump_vars (htab_t);
static void dump_dataflow_set (dataflow_set *);
static void dump_dataflow_sets (void);

static void variable_was_changed (variable, dataflow_set *);
static void **set_slot_part (dataflow_set *, rtx, void **,
			     decl_or_value, HOST_WIDE_INT,
			     enum var_init_status, rtx);
static void set_variable_part (dataflow_set *, rtx,
			       decl_or_value, HOST_WIDE_INT,
			       enum var_init_status, rtx, enum insert_option);
static void **clobber_slot_part (dataflow_set *, rtx,
				 void **, HOST_WIDE_INT, rtx);
static void clobber_variable_part (dataflow_set *, rtx,
				   decl_or_value, HOST_WIDE_INT, rtx);
static void **delete_slot_part (dataflow_set *, rtx, void **, HOST_WIDE_INT);
static void delete_variable_part (dataflow_set *, rtx,
				  decl_or_value, HOST_WIDE_INT);
static int emit_note_insn_var_location (void **, void *);
static void emit_notes_for_changes (rtx, enum emit_note_where, shared_hash);
static int emit_notes_for_differences_1 (void **, void *);
static int emit_notes_for_differences_2 (void **, void *);
static void emit_notes_for_differences (rtx, dataflow_set *, dataflow_set *);
static void emit_notes_in_bb (basic_block, dataflow_set *);
static void vt_emit_notes (void);

static bool vt_get_decl_and_offset (rtx, tree *, HOST_WIDE_INT *);
static void vt_add_function_parameters (void);
static bool vt_initialize (void);
static void vt_finalize (void);

/* Given a SET, calculate the amount of stack adjustment it contains
   PRE- and POST-modifying stack pointer.
   This function is similar to stack_adjust_offset.  */

static void
stack_adjust_offset_pre_post (rtx pattern, HOST_WIDE_INT *pre,
			      HOST_WIDE_INT *post)
{
  rtx src = SET_SRC (pattern);
  rtx dest = SET_DEST (pattern);
  enum rtx_code code;

  if (dest == stack_pointer_rtx)
    {
      /* (set (reg sp) (plus (reg sp) (const_int))) */
      code = GET_CODE (src);
      if (! (code == PLUS || code == MINUS)
	  || XEXP (src, 0) != stack_pointer_rtx
	  || !CONST_INT_P (XEXP (src, 1)))
	return;

      if (code == MINUS)
	*post += INTVAL (XEXP (src, 1));
      else
	*post -= INTVAL (XEXP (src, 1));
    }
  else if (MEM_P (dest))
    {
      /* (set (mem (pre_dec (reg sp))) (foo)) */
      src = XEXP (dest, 0);
      code = GET_CODE (src);

      switch (code)
	{
	case PRE_MODIFY:
	case POST_MODIFY:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      rtx val = XEXP (XEXP (src, 1), 1);
	      /* We handle only adjustments by constant amount.  */
	      gcc_assert (GET_CODE (XEXP (src, 1)) == PLUS &&
			  CONST_INT_P (val));

	      if (code == PRE_MODIFY)
		*pre -= INTVAL (val);
	      else
		*post -= INTVAL (val);
	      break;
	    }
	  return;

	case PRE_DEC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *pre += GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	case POST_DEC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *post += GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	case PRE_INC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *pre -= GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	case POST_INC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *post -= GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	default:
	  return;
	}
    }
}

/* Given an INSN, calculate the amount of stack adjustment it contains
   PRE- and POST-modifying stack pointer.  */

static void
insn_stack_adjust_offset_pre_post (rtx insn, HOST_WIDE_INT *pre,
				   HOST_WIDE_INT *post)
{
  rtx pattern;

  *pre = 0;
  *post = 0;

  pattern = PATTERN (insn);
  if (RTX_FRAME_RELATED_P (insn))
    {
      rtx expr = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);
      if (expr)
	pattern = XEXP (expr, 0);
    }

  if (GET_CODE (pattern) == SET)
    stack_adjust_offset_pre_post (pattern, pre, post);
  else if (GET_CODE (pattern) == PARALLEL
	   || GET_CODE (pattern) == SEQUENCE)
    {
      int i;

      /* There may be stack adjustments inside compound insns.  Search
	 for them.  */
      for ( i = XVECLEN (pattern, 0) - 1; i >= 0; i--)
	if (GET_CODE (XVECEXP (pattern, 0, i)) == SET)
	  stack_adjust_offset_pre_post (XVECEXP (pattern, 0, i), pre, post);
    }
}

/* Compute stack adjustments for all blocks by traversing DFS tree.
   Return true when the adjustments on all incoming edges are consistent.
   Heavily borrowed from pre_and_rev_post_order_compute.  */

static bool
vt_stack_adjustments (void)
{
  edge_iterator *stack;
  int sp;

  /* Initialize entry block.  */
  VTI (ENTRY_BLOCK_PTR)->visited = true;
  VTI (ENTRY_BLOCK_PTR)->in.stack_adjust = INCOMING_FRAME_SP_OFFSET;
  VTI (ENTRY_BLOCK_PTR)->out.stack_adjust = INCOMING_FRAME_SP_OFFSET;

  /* Allocate stack for back-tracking up CFG.  */
  stack = XNEWVEC (edge_iterator, n_basic_blocks + 1);
  sp = 0;

  /* Push the first edge on to the stack.  */
  stack[sp++] = ei_start (ENTRY_BLOCK_PTR->succs);

  while (sp)
    {
      edge_iterator ei;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      ei = stack[sp - 1];
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;

      /* Check if the edge destination has been visited yet.  */
      if (!VTI (dest)->visited)
	{
	  rtx insn;
	  HOST_WIDE_INT pre, post, offset;
	  VTI (dest)->visited = true;
	  VTI (dest)->in.stack_adjust = offset = VTI (src)->out.stack_adjust;

	  if (dest != EXIT_BLOCK_PTR)
	    for (insn = BB_HEAD (dest);
		 insn != NEXT_INSN (BB_END (dest));
		 insn = NEXT_INSN (insn))
	      if (INSN_P (insn))
		{
		  insn_stack_adjust_offset_pre_post (insn, &pre, &post);
		  offset += pre + post;
		}

	  VTI (dest)->out.stack_adjust = offset;

	  if (EDGE_COUNT (dest->succs) > 0)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack[sp++] = ei_start (dest->succs);
	}
      else
	{
	  /* Check whether the adjustments on the edges are the same.  */
	  if (VTI (dest)->in.stack_adjust != VTI (src)->out.stack_adjust)
	    {
	      free (stack);
	      return false;
	    }

	  if (! ei_one_before_end_p (ei))
	    /* Go to the next edge.  */
	    ei_next (&stack[sp - 1]);
	  else
	    /* Return to previous level if there are no more edges.  */
	    sp--;
	}
    }

  free (stack);
  return true;
}

/* Compute a CFA-based value for the stack pointer.  */

static rtx
compute_cfa_pointer (HOST_WIDE_INT adjustment)
{
  rtx cfa;

#ifdef FRAME_POINTER_CFA_OFFSET
  adjustment -= FRAME_POINTER_CFA_OFFSET (current_function_decl);
  cfa = plus_constant (frame_pointer_rtx, adjustment);
#else
  adjustment -= ARG_POINTER_CFA_OFFSET (current_function_decl);
  cfa = plus_constant (arg_pointer_rtx, adjustment);
#endif

  return cfa;
}

/* Adjustment for hard_frame_pointer_rtx to cfa base reg,
   or -1 if the replacement shouldn't be done.  */
static HOST_WIDE_INT hard_frame_pointer_adjustment = -1;

/* Data for adjust_mems callback.  */

struct adjust_mem_data
{
  bool store;
  enum machine_mode mem_mode;
  HOST_WIDE_INT stack_adjust;
  rtx side_effects;
};

/* Helper function for adjusting used MEMs.  */

static rtx
adjust_mems (rtx loc, const_rtx old_rtx, void *data)
{
  struct adjust_mem_data *amd = (struct adjust_mem_data *) data;
  rtx mem, addr = loc, tem;
  enum machine_mode mem_mode_save;
  bool store_save;
  switch (GET_CODE (loc))
    {
    case REG:
      /* Don't do any sp or fp replacements outside of MEM addresses.  */
      if (amd->mem_mode == VOIDmode)
	return loc;
      if (loc == stack_pointer_rtx
	  && !frame_pointer_needed)
	return compute_cfa_pointer (amd->stack_adjust);
      else if (loc == hard_frame_pointer_rtx
	       && frame_pointer_needed
	       && hard_frame_pointer_adjustment != -1)
	return compute_cfa_pointer (hard_frame_pointer_adjustment);
      return loc;
    case MEM:
      mem = loc;
      if (!amd->store)
	{
	  mem = targetm.delegitimize_address (mem);
	  if (mem != loc && !MEM_P (mem))
	    return simplify_replace_fn_rtx (mem, old_rtx, adjust_mems, data);
	}

      addr = XEXP (mem, 0);
      mem_mode_save = amd->mem_mode;
      amd->mem_mode = GET_MODE (mem);
      store_save = amd->store;
      amd->store = false;
      addr = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      amd->store = store_save;
      amd->mem_mode = mem_mode_save;
      if (mem == loc)
	addr = targetm.delegitimize_address (addr);
      if (addr != XEXP (mem, 0))
	mem = replace_equiv_address_nv (mem, addr);
      if (!amd->store)
	mem = avoid_constant_pool_reference (mem);
      return mem;
    case PRE_INC:
    case PRE_DEC:
      addr = gen_rtx_PLUS (GET_MODE (loc), XEXP (loc, 0),
			   GEN_INT (GET_CODE (loc) == PRE_INC
				    ? GET_MODE_SIZE (amd->mem_mode)
				    : -GET_MODE_SIZE (amd->mem_mode)));
    case POST_INC:
    case POST_DEC:
      if (addr == loc)
	addr = XEXP (loc, 0);
      gcc_assert (amd->mem_mode != VOIDmode && amd->mem_mode != BLKmode);
      addr = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      tem = gen_rtx_PLUS (GET_MODE (loc), XEXP (loc, 0),
			   GEN_INT ((GET_CODE (loc) == PRE_INC
				     || GET_CODE (loc) == POST_INC)
				    ? GET_MODE_SIZE (amd->mem_mode)
				    : -GET_MODE_SIZE (amd->mem_mode)));
      amd->side_effects = alloc_EXPR_LIST (0,
					   gen_rtx_SET (VOIDmode,
							XEXP (loc, 0),
							tem),
					   amd->side_effects);
      return addr;
    case PRE_MODIFY:
      addr = XEXP (loc, 1);
    case POST_MODIFY:
      if (addr == loc)
	addr = XEXP (loc, 0);
      gcc_assert (amd->mem_mode != VOIDmode && amd->mem_mode != BLKmode);
      addr = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      amd->side_effects = alloc_EXPR_LIST (0,
					   gen_rtx_SET (VOIDmode,
							XEXP (loc, 0),
							XEXP (loc, 1)),
					   amd->side_effects);
      return addr;
    case SUBREG:
      /* First try without delegitimization of whole MEMs and
	 avoid_constant_pool_reference, which is more likely to succeed.  */
      store_save = amd->store;
      amd->store = true;
      addr = simplify_replace_fn_rtx (SUBREG_REG (loc), old_rtx, adjust_mems,
				      data);
      amd->store = store_save;
      mem = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      if (mem == SUBREG_REG (loc))
	return loc;
      tem = simplify_gen_subreg (GET_MODE (loc), mem,
				 GET_MODE (SUBREG_REG (loc)),
				 SUBREG_BYTE (loc));
      if (tem)
	return tem;
      tem = simplify_gen_subreg (GET_MODE (loc), addr,
				 GET_MODE (SUBREG_REG (loc)),
				 SUBREG_BYTE (loc));
      if (tem)
	return tem;
      return gen_rtx_raw_SUBREG (GET_MODE (loc), addr, SUBREG_BYTE (loc));
    default:
      break;
    }
  return NULL_RTX;
}

/* Helper function for replacement of uses.  */

static void
adjust_mem_uses (rtx *x, void *data)
{
  rtx new_x = simplify_replace_fn_rtx (*x, NULL_RTX, adjust_mems, data);
  if (new_x != *x)
    validate_change (NULL_RTX, x, new_x, true);
}

/* Helper function for replacement of stores.  */

static void
adjust_mem_stores (rtx loc, const_rtx expr, void *data)
{
  if (MEM_P (loc))
    {
      rtx new_dest = simplify_replace_fn_rtx (SET_DEST (expr), NULL_RTX,
					      adjust_mems, data);
      if (new_dest != SET_DEST (expr))
	{
	  rtx xexpr = CONST_CAST_RTX (expr);
	  validate_change (NULL_RTX, &SET_DEST (xexpr), new_dest, true);
	}
    }
}

/* Simplify INSN.  Remove all {PRE,POST}_{INC,DEC,MODIFY} rtxes,
   replace them with their value in the insn and add the side-effects
   as other sets to the insn.  */

static void
adjust_insn (basic_block bb, rtx insn)
{
  struct adjust_mem_data amd;
  rtx set;
  amd.mem_mode = VOIDmode;
  amd.stack_adjust = -VTI (bb)->out.stack_adjust;
  amd.side_effects = NULL_RTX;

  amd.store = true;
  note_stores (PATTERN (insn), adjust_mem_stores, &amd);

  amd.store = false;
  note_uses (&PATTERN (insn), adjust_mem_uses, &amd);

  /* For read-only MEMs containing some constant, prefer those
     constants.  */
  set = single_set (insn);
  if (set && MEM_P (SET_SRC (set)) && MEM_READONLY_P (SET_SRC (set)))
    {
      rtx note = find_reg_equal_equiv_note (insn);

      if (note && CONSTANT_P (XEXP (note, 0)))
	validate_change (NULL_RTX, &SET_SRC (set), XEXP (note, 0), true);
    }

  if (amd.side_effects)
    {
      rtx *pat, new_pat, s;
      int i, oldn, newn;

      pat = &PATTERN (insn);
      if (GET_CODE (*pat) == COND_EXEC)
	pat = &COND_EXEC_CODE (*pat);
      if (GET_CODE (*pat) == PARALLEL)
	oldn = XVECLEN (*pat, 0);
      else
	oldn = 1;
      for (s = amd.side_effects, newn = 0; s; newn++)
	s = XEXP (s, 1);
      new_pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (oldn + newn));
      if (GET_CODE (*pat) == PARALLEL)
	for (i = 0; i < oldn; i++)
	  XVECEXP (new_pat, 0, i) = XVECEXP (*pat, 0, i);
      else
	XVECEXP (new_pat, 0, 0) = *pat;
      for (s = amd.side_effects, i = oldn; i < oldn + newn; i++, s = XEXP (s, 1))
	XVECEXP (new_pat, 0, i) = XEXP (s, 0);
      free_EXPR_LIST_list (&amd.side_effects);
      validate_change (NULL_RTX, pat, new_pat, true);
    }
}

/* Return true if a decl_or_value DV is a DECL or NULL.  */
static inline bool
dv_is_decl_p (decl_or_value dv)
{
  return !dv || (int) TREE_CODE ((tree) dv) != (int) VALUE;
}

/* Return true if a decl_or_value is a VALUE rtl.  */
static inline bool
dv_is_value_p (decl_or_value dv)
{
  return dv && !dv_is_decl_p (dv);
}

/* Return the decl in the decl_or_value.  */
static inline tree
dv_as_decl (decl_or_value dv)
{
#ifdef ENABLE_CHECKING
  gcc_assert (dv_is_decl_p (dv));
#endif
  return (tree) dv;
}

/* Return the value in the decl_or_value.  */
static inline rtx
dv_as_value (decl_or_value dv)
{
#ifdef ENABLE_CHECKING
  gcc_assert (dv_is_value_p (dv));
#endif
  return (rtx)dv;
}

/* Return the opaque pointer in the decl_or_value.  */
static inline void *
dv_as_opaque (decl_or_value dv)
{
  return dv;
}

/* Return true if a decl_or_value must not have more than one variable
   part.  */
static inline bool
dv_onepart_p (decl_or_value dv)
{
  tree decl;

  if (!MAY_HAVE_DEBUG_INSNS)
    return false;

  if (dv_is_value_p (dv))
    return true;

  decl = dv_as_decl (dv);

  if (!decl)
    return true;

  if (TREE_CODE (decl) == DEBUG_EXPR_DECL)
    return true;

  return (target_for_debug_bind (decl) != NULL_TREE);
}

/* Return the variable pool to be used for dv, depending on whether it
   can have multiple parts or not.  */
static inline alloc_pool
dv_pool (decl_or_value dv)
{
  return dv_onepart_p (dv) ? valvar_pool : var_pool;
}

/* Build a decl_or_value out of a decl.  */
static inline decl_or_value
dv_from_decl (tree decl)
{
  decl_or_value dv;
  dv = decl;
#ifdef ENABLE_CHECKING
  gcc_assert (dv_is_decl_p (dv));
#endif
  return dv;
}

/* Build a decl_or_value out of a value.  */
static inline decl_or_value
dv_from_value (rtx value)
{
  decl_or_value dv;
  dv = value;
#ifdef ENABLE_CHECKING
  gcc_assert (dv_is_value_p (dv));
#endif
  return dv;
}

extern void debug_dv (decl_or_value dv);

void
debug_dv (decl_or_value dv)
{
  if (dv_is_value_p (dv))
    debug_rtx (dv_as_value (dv));
  else
    debug_generic_stmt (dv_as_decl (dv));
}

typedef unsigned int dvuid;

/* Return the uid of DV.  */

static inline dvuid
dv_uid (decl_or_value dv)
{
  if (dv_is_value_p (dv))
    return CSELIB_VAL_PTR (dv_as_value (dv))->uid;
  else
    return DECL_UID (dv_as_decl (dv));
}

/* Compute the hash from the uid.  */

static inline hashval_t
dv_uid2hash (dvuid uid)
{
  return uid;
}

/* The hash function for a mask table in a shared_htab chain.  */

static inline hashval_t
dv_htab_hash (decl_or_value dv)
{
  return dv_uid2hash (dv_uid (dv));
}

/* The hash function for variable_htab, computes the hash value
   from the declaration of variable X.  */

static hashval_t
variable_htab_hash (const void *x)
{
  const_variable const v = (const_variable) x;

  return dv_htab_hash (v->dv);
}

/* Compare the declaration of variable X with declaration Y.  */

static int
variable_htab_eq (const void *x, const void *y)
{
  const_variable const v = (const_variable) x;
  decl_or_value dv = CONST_CAST2 (decl_or_value, const void *, y);

  return (dv_as_opaque (v->dv) == dv_as_opaque (dv));
}

/* Free the element of VARIABLE_HTAB (its type is struct variable_def).  */

static void
variable_htab_free (void *elem)
{
  int i;
  variable var = (variable) elem;
  location_chain node, next;

  gcc_assert (var->refcount > 0);

  var->refcount--;
  if (var->refcount > 0)
    return;

  for (i = 0; i < var->n_var_parts; i++)
    {
      for (node = var->var_part[i].loc_chain; node; node = next)
	{
	  next = node->next;
	  pool_free (loc_chain_pool, node);
	}
      var->var_part[i].loc_chain = NULL;
    }
  pool_free (dv_pool (var->dv), var);
}

/* The hash function for value_chains htab, computes the hash value
   from the VALUE.  */

static hashval_t
value_chain_htab_hash (const void *x)
{
  const_value_chain const v = (const_value_chain) x;

  return dv_htab_hash (v->dv);
}

/* Compare the VALUE X with VALUE Y.  */

static int
value_chain_htab_eq (const void *x, const void *y)
{
  const_value_chain const v = (const_value_chain) x;
  decl_or_value dv = CONST_CAST2 (decl_or_value, const void *, y);

  return dv_as_opaque (v->dv) == dv_as_opaque (dv);
}

/* Initialize the set (array) SET of attrs to empty lists.  */

static void
init_attrs_list_set (attrs *set)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    set[i] = NULL;
}

/* Make the list *LISTP empty.  */

static void
attrs_list_clear (attrs *listp)
{
  attrs list, next;

  for (list = *listp; list; list = next)
    {
      next = list->next;
      pool_free (attrs_pool, list);
    }
  *listp = NULL;
}

/* Return true if the pair of DECL and OFFSET is the member of the LIST.  */

static attrs
attrs_list_member (attrs list, decl_or_value dv, HOST_WIDE_INT offset)
{
  for (; list; list = list->next)
    if (dv_as_opaque (list->dv) == dv_as_opaque (dv) && list->offset == offset)
      return list;
  return NULL;
}

/* Insert the triplet DECL, OFFSET, LOC to the list *LISTP.  */

static void
attrs_list_insert (attrs *listp, decl_or_value dv,
		   HOST_WIDE_INT offset, rtx loc)
{
  attrs list;

  list = (attrs) pool_alloc (attrs_pool);
  list->loc = loc;
  list->dv = dv;
  list->offset = offset;
  list->next = *listp;
  *listp = list;
}

/* Copy all nodes from SRC and create a list *DSTP of the copies.  */

static void
attrs_list_copy (attrs *dstp, attrs src)
{
  attrs n;

  attrs_list_clear (dstp);
  for (; src; src = src->next)
    {
      n = (attrs) pool_alloc (attrs_pool);
      n->loc = src->loc;
      n->dv = src->dv;
      n->offset = src->offset;
      n->next = *dstp;
      *dstp = n;
    }
}

/* Add all nodes from SRC which are not in *DSTP to *DSTP.  */

static void
attrs_list_union (attrs *dstp, attrs src)
{
  for (; src; src = src->next)
    {
      if (!attrs_list_member (*dstp, src->dv, src->offset))
	attrs_list_insert (dstp, src->dv, src->offset, src->loc);
    }
}

/* Combine nodes that are not onepart nodes from SRC and SRC2 into
   *DSTP.  */

static void
attrs_list_mpdv_union (attrs *dstp, attrs src, attrs src2)
{
  gcc_assert (!*dstp);
  for (; src; src = src->next)
    {
      if (!dv_onepart_p (src->dv))
	attrs_list_insert (dstp, src->dv, src->offset, src->loc);
    }
  for (src = src2; src; src = src->next)
    {
      if (!dv_onepart_p (src->dv)
	  && !attrs_list_member (*dstp, src->dv, src->offset))
	attrs_list_insert (dstp, src->dv, src->offset, src->loc);
    }
}

/* Shared hashtable support.  */

/* Return true if VARS is shared.  */

static inline bool
shared_hash_shared (shared_hash vars)
{
  return vars->refcount > 1;
}

/* Return the hash table for VARS.  */

static inline htab_t
shared_hash_htab (shared_hash vars)
{
  return vars->htab;
}

/* Return true if VAR is shared, or maybe because VARS is shared.  */

static inline bool
shared_var_p (variable var, shared_hash vars)
{
  /* Don't count an entry in the changed_variables table as a duplicate.  */
  return ((var->refcount > 1 + (int) var->in_changed_variables)
	  || shared_hash_shared (vars));
}

/* Copy variables into a new hash table.  */

static shared_hash
shared_hash_unshare (shared_hash vars)
{
  shared_hash new_vars = (shared_hash) pool_alloc (shared_hash_pool);
  gcc_assert (vars->refcount > 1);
  new_vars->refcount = 1;
  new_vars->htab
    = htab_create (htab_elements (vars->htab) + 3, variable_htab_hash,
		   variable_htab_eq, variable_htab_free);
  vars_copy (new_vars->htab, vars->htab);
  vars->refcount--;
  return new_vars;
}

/* Increment reference counter on VARS and return it.  */

static inline shared_hash
shared_hash_copy (shared_hash vars)
{
  vars->refcount++;
  return vars;
}

/* Decrement reference counter and destroy hash table if not shared
   anymore.  */

static void
shared_hash_destroy (shared_hash vars)
{
  gcc_assert (vars->refcount > 0);
  if (--vars->refcount == 0)
    {
      htab_delete (vars->htab);
      pool_free (shared_hash_pool, vars);
    }
}

/* Unshare *PVARS if shared and return slot for DV.  If INS is
   INSERT, insert it if not already present.  */

static inline void **
shared_hash_find_slot_unshare_1 (shared_hash *pvars, decl_or_value dv,
				 hashval_t dvhash, enum insert_option ins)
{
  if (shared_hash_shared (*pvars))
    *pvars = shared_hash_unshare (*pvars);
  return htab_find_slot_with_hash (shared_hash_htab (*pvars), dv, dvhash, ins);
}

static inline void **
shared_hash_find_slot_unshare (shared_hash *pvars, decl_or_value dv,
			       enum insert_option ins)
{
  return shared_hash_find_slot_unshare_1 (pvars, dv, dv_htab_hash (dv), ins);
}

/* Return slot for DV, if it is already present in the hash table.
   If it is not present, insert it only VARS is not shared, otherwise
   return NULL.  */

static inline void **
shared_hash_find_slot_1 (shared_hash vars, decl_or_value dv, hashval_t dvhash)
{
  return htab_find_slot_with_hash (shared_hash_htab (vars), dv, dvhash,
				   shared_hash_shared (vars)
				   ? NO_INSERT : INSERT);
}

static inline void **
shared_hash_find_slot (shared_hash vars, decl_or_value dv)
{
  return shared_hash_find_slot_1 (vars, dv, dv_htab_hash (dv));
}

/* Return slot for DV only if it is already present in the hash table.  */

static inline void **
shared_hash_find_slot_noinsert_1 (shared_hash vars, decl_or_value dv,
				  hashval_t dvhash)
{
  return htab_find_slot_with_hash (shared_hash_htab (vars), dv, dvhash,
				   NO_INSERT);
}

static inline void **
shared_hash_find_slot_noinsert (shared_hash vars, decl_or_value dv)
{
  return shared_hash_find_slot_noinsert_1 (vars, dv, dv_htab_hash (dv));
}

/* Return variable for DV or NULL if not already present in the hash
   table.  */

static inline variable
shared_hash_find_1 (shared_hash vars, decl_or_value dv, hashval_t dvhash)
{
  return (variable) htab_find_with_hash (shared_hash_htab (vars), dv, dvhash);
}

static inline variable
shared_hash_find (shared_hash vars, decl_or_value dv)
{
  return shared_hash_find_1 (vars, dv, dv_htab_hash (dv));
}

/* Return true if TVAL is better than CVAL as a canonival value.  We
   choose lowest-numbered VALUEs, using the RTX address as a
   tie-breaker.  The idea is to arrange them into a star topology,
   such that all of them are at most one step away from the canonical
   value, and the canonical value has backlinks to all of them, in
   addition to all the actual locations.  We don't enforce this
   topology throughout the entire dataflow analysis, though.
 */

static inline bool
canon_value_cmp (rtx tval, rtx cval)
{
  return !cval
    || CSELIB_VAL_PTR (tval)->uid < CSELIB_VAL_PTR (cval)->uid;
}

static bool dst_can_be_shared;

/* Return a copy of a variable VAR and insert it to dataflow set SET.  */

static void **
unshare_variable (dataflow_set *set, void **slot, variable var,
		  enum var_init_status initialized)
{
  variable new_var;
  int i;

  new_var = (variable) pool_alloc (dv_pool (var->dv));
  new_var->dv = var->dv;
  new_var->refcount = 1;
  var->refcount--;
  new_var->n_var_parts = var->n_var_parts;
  new_var->cur_loc_changed = var->cur_loc_changed;
  var->cur_loc_changed = false;
  new_var->in_changed_variables = false;

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  for (i = 0; i < var->n_var_parts; i++)
    {
      location_chain node;
      location_chain *nextp;

      new_var->var_part[i].offset = var->var_part[i].offset;
      nextp = &new_var->var_part[i].loc_chain;
      for (node = var->var_part[i].loc_chain; node; node = node->next)
	{
	  location_chain new_lc;

	  new_lc = (location_chain) pool_alloc (loc_chain_pool);
	  new_lc->next = NULL;
	  if (node->init > initialized)
	    new_lc->init = node->init;
	  else
	    new_lc->init = initialized;
	  if (node->set_src && !(MEM_P (node->set_src)))
	    new_lc->set_src = node->set_src;
	  else
	    new_lc->set_src = NULL;
	  new_lc->loc = node->loc;

	  *nextp = new_lc;
	  nextp = &new_lc->next;
	}

      new_var->var_part[i].cur_loc = var->var_part[i].cur_loc;
    }

  dst_can_be_shared = false;
  if (shared_hash_shared (set->vars))
    slot = shared_hash_find_slot_unshare (&set->vars, var->dv, NO_INSERT);
  else if (set->traversed_vars && set->vars != set->traversed_vars)
    slot = shared_hash_find_slot_noinsert (set->vars, var->dv);
  *slot = new_var;
  if (var->in_changed_variables)
    {
      void **cslot
	= htab_find_slot_with_hash (changed_variables, var->dv,
				    dv_htab_hash (var->dv), NO_INSERT);
      gcc_assert (*cslot == (void *) var);
      var->in_changed_variables = false;
      variable_htab_free (var);
      *cslot = new_var;
      new_var->in_changed_variables = true;
    }
  return slot;
}

/* Add a variable from *SLOT to hash table DATA and increase its reference
   count.  */

static int
vars_copy_1 (void **slot, void *data)
{
  htab_t dst = (htab_t) data;
  variable src;
  void **dstp;

  src = (variable) *slot;
  src->refcount++;

  dstp = htab_find_slot_with_hash (dst, src->dv,
				   dv_htab_hash (src->dv),
				   INSERT);
  *dstp = src;

  /* Continue traversing the hash table.  */
  return 1;
}

/* Copy all variables from hash table SRC to hash table DST.  */

static void
vars_copy (htab_t dst, htab_t src)
{
  htab_traverse_noresize (src, vars_copy_1, dst);
}

/* Map a decl to its main debug decl.  */

static inline tree
var_debug_decl (tree decl)
{
  if (decl && DECL_P (decl)
      && DECL_DEBUG_EXPR_IS_FROM (decl) && DECL_DEBUG_EXPR (decl)
      && DECL_P (DECL_DEBUG_EXPR (decl)))
    decl = DECL_DEBUG_EXPR (decl);

  return decl;
}

/* Set the register LOC to contain DV, OFFSET.  */

static void
var_reg_decl_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
		  decl_or_value dv, HOST_WIDE_INT offset, rtx set_src,
		  enum insert_option iopt)
{
  attrs node;
  bool decl_p = dv_is_decl_p (dv);

  if (decl_p)
    dv = dv_from_decl (var_debug_decl (dv_as_decl (dv)));

  for (node = set->regs[REGNO (loc)]; node; node = node->next)
    if (dv_as_opaque (node->dv) == dv_as_opaque (dv)
	&& node->offset == offset)
      break;
  if (!node)
    attrs_list_insert (&set->regs[REGNO (loc)], dv, offset, loc);
  set_variable_part (set, loc, dv, offset, initialized, set_src, iopt);
}

/* Set the register to contain REG_EXPR (LOC), REG_OFFSET (LOC).  */

static void
var_reg_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
	     rtx set_src)
{
  tree decl = REG_EXPR (loc);
  HOST_WIDE_INT offset = REG_OFFSET (loc);

  var_reg_decl_set (set, loc, initialized,
		    dv_from_decl (decl), offset, set_src, INSERT);
}

static enum var_init_status
get_init_value (dataflow_set *set, rtx loc, decl_or_value dv)
{
  variable var;
  int i;
  enum var_init_status ret_val = VAR_INIT_STATUS_UNKNOWN;

  if (! flag_var_tracking_uninit)
    return VAR_INIT_STATUS_INITIALIZED;

  var = shared_hash_find (set->vars, dv);
  if (var)
    {
      for (i = 0; i < var->n_var_parts && ret_val == VAR_INIT_STATUS_UNKNOWN; i++)
	{
	  location_chain nextp;
	  for (nextp = var->var_part[i].loc_chain; nextp; nextp = nextp->next)
	    if (rtx_equal_p (nextp->loc, loc))
	      {
		ret_val = nextp->init;
		break;
	      }
	}
    }

  return ret_val;
}

/* Delete current content of register LOC in dataflow set SET and set
   the register to contain REG_EXPR (LOC), REG_OFFSET (LOC).  If
   MODIFY is true, any other live copies of the same variable part are
   also deleted from the dataflow set, otherwise the variable part is
   assumed to be copied from another location holding the same
   part.  */

static void
var_reg_delete_and_set (dataflow_set *set, rtx loc, bool modify,
			enum var_init_status initialized, rtx set_src)
{
  tree decl = REG_EXPR (loc);
  HOST_WIDE_INT offset = REG_OFFSET (loc);
  attrs node, next;
  attrs *nextp;

  decl = var_debug_decl (decl);

  if (initialized == VAR_INIT_STATUS_UNKNOWN)
    initialized = get_init_value (set, loc, dv_from_decl (decl));

  nextp = &set->regs[REGNO (loc)];
  for (node = *nextp; node; node = next)
    {
      next = node->next;
      if (dv_as_opaque (node->dv) != decl || node->offset != offset)
	{
	  delete_variable_part (set, node->loc, node->dv, node->offset);
	  pool_free (attrs_pool, node);
	  *nextp = next;
	}
      else
	{
	  node->loc = loc;
	  nextp = &node->next;
	}
    }
  if (modify)
    clobber_variable_part (set, loc, dv_from_decl (decl), offset, set_src);
  var_reg_set (set, loc, initialized, set_src);
}

/* Delete the association of register LOC in dataflow set SET with any
   variables that aren't onepart.  If CLOBBER is true, also delete any
   other live copies of the same variable part, and delete the
   association with onepart dvs too.  */

static void
var_reg_delete (dataflow_set *set, rtx loc, bool clobber)
{
  attrs *nextp = &set->regs[REGNO (loc)];
  attrs node, next;

  if (clobber)
    {
      tree decl = REG_EXPR (loc);
      HOST_WIDE_INT offset = REG_OFFSET (loc);

      decl = var_debug_decl (decl);

      clobber_variable_part (set, NULL, dv_from_decl (decl), offset, NULL);
    }

  for (node = *nextp; node; node = next)
    {
      next = node->next;
      if (clobber || !dv_onepart_p (node->dv))
	{
	  delete_variable_part (set, node->loc, node->dv, node->offset);
	  pool_free (attrs_pool, node);
	  *nextp = next;
	}
      else
	nextp = &node->next;
    }
}

/* Delete content of register with number REGNO in dataflow set SET.  */

static void
var_regno_delete (dataflow_set *set, int regno)
{
  attrs *reg = &set->regs[regno];
  attrs node, next;

  for (node = *reg; node; node = next)
    {
      next = node->next;
      delete_variable_part (set, node->loc, node->dv, node->offset);
      pool_free (attrs_pool, node);
    }
  *reg = NULL;
}

/* Set the location of DV, OFFSET as the MEM LOC.  */

static void
var_mem_decl_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
		  decl_or_value dv, HOST_WIDE_INT offset, rtx set_src,
		  enum insert_option iopt)
{
  if (dv_is_decl_p (dv))
    dv = dv_from_decl (var_debug_decl (dv_as_decl (dv)));

  set_variable_part (set, loc, dv, offset, initialized, set_src, iopt);
}

/* Set the location part of variable MEM_EXPR (LOC) in dataflow set
   SET to LOC.
   Adjust the address first if it is stack pointer based.  */

static void
var_mem_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
	     rtx set_src)
{
  tree decl = MEM_EXPR (loc);
  HOST_WIDE_INT offset = INT_MEM_OFFSET (loc);

  var_mem_decl_set (set, loc, initialized,
		    dv_from_decl (decl), offset, set_src, INSERT);
}

/* Delete and set the location part of variable MEM_EXPR (LOC) in
   dataflow set SET to LOC.  If MODIFY is true, any other live copies
   of the same variable part are also deleted from the dataflow set,
   otherwise the variable part is assumed to be copied from another
   location holding the same part.
   Adjust the address first if it is stack pointer based.  */

static void
var_mem_delete_and_set (dataflow_set *set, rtx loc, bool modify,
			enum var_init_status initialized, rtx set_src)
{
  tree decl = MEM_EXPR (loc);
  HOST_WIDE_INT offset = INT_MEM_OFFSET (loc);

  decl = var_debug_decl (decl);

  if (initialized == VAR_INIT_STATUS_UNKNOWN)
    initialized = get_init_value (set, loc, dv_from_decl (decl));

  if (modify)
    clobber_variable_part (set, NULL, dv_from_decl (decl), offset, set_src);
  var_mem_set (set, loc, initialized, set_src);
}

/* Delete the location part LOC from dataflow set SET.  If CLOBBER is
   true, also delete any other live copies of the same variable part.
   Adjust the address first if it is stack pointer based.  */

static void
var_mem_delete (dataflow_set *set, rtx loc, bool clobber)
{
  tree decl = MEM_EXPR (loc);
  HOST_WIDE_INT offset = INT_MEM_OFFSET (loc);

  decl = var_debug_decl (decl);
  if (clobber)
    clobber_variable_part (set, NULL, dv_from_decl (decl), offset, NULL);
  delete_variable_part (set, loc, dv_from_decl (decl), offset);
}

/* Bind a value to a location it was just stored in.  If MODIFIED
   holds, assume the location was modified, detaching it from any
   values bound to it.  */

static void
val_store (dataflow_set *set, rtx val, rtx loc, rtx insn, bool modified)
{
  cselib_val *v = CSELIB_VAL_PTR (val);

  gcc_assert (cselib_preserved_value_p (v));

  if (dump_file)
    {
      fprintf (dump_file, "%i: ", INSN_UID (insn));
      print_inline_rtx (dump_file, val, 0);
      fprintf (dump_file, " stored in ");
      print_inline_rtx (dump_file, loc, 0);
      if (v->locs)
	{
	  struct elt_loc_list *l;
	  for (l = v->locs; l; l = l->next)
	    {
	      fprintf (dump_file, "\n%i: ", INSN_UID (l->setting_insn));
	      print_inline_rtx (dump_file, l->loc, 0);
	    }
	}
      fprintf (dump_file, "\n");
    }

  if (REG_P (loc))
    {
      if (modified)
	var_regno_delete (set, REGNO (loc));
      var_reg_decl_set (set, loc, VAR_INIT_STATUS_INITIALIZED,
			dv_from_value (val), 0, NULL_RTX, INSERT);
    }
  else if (MEM_P (loc))
    var_mem_decl_set (set, loc, VAR_INIT_STATUS_INITIALIZED,
		      dv_from_value (val), 0, NULL_RTX, INSERT);
  else
    set_variable_part (set, loc, dv_from_value (val), 0,
		       VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
}

/* Reset this node, detaching all its equivalences.  Return the slot
   in the variable hash table that holds dv, if there is one.  */

static void
val_reset (dataflow_set *set, decl_or_value dv)
{
  variable var = shared_hash_find (set->vars, dv) ;
  location_chain node;
  rtx cval;

  if (!var || !var->n_var_parts)
    return;

  gcc_assert (var->n_var_parts == 1);

  cval = NULL;
  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE
	&& canon_value_cmp (node->loc, cval))
      cval = node->loc;

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE && cval != node->loc)
      {
	/* Redirect the equivalence link to the new canonical
	   value, or simply remove it if it would point at
	   itself.  */
	if (cval)
	  set_variable_part (set, cval, dv_from_value (node->loc),
			     0, node->init, node->set_src, NO_INSERT);
	delete_variable_part (set, dv_as_value (dv),
			      dv_from_value (node->loc), 0);
      }

  if (cval)
    {
      decl_or_value cdv = dv_from_value (cval);

      /* Keep the remaining values connected, accummulating links
	 in the canonical value.  */
      for (node = var->var_part[0].loc_chain; node; node = node->next)
	{
	  if (node->loc == cval)
	    continue;
	  else if (GET_CODE (node->loc) == REG)
	    var_reg_decl_set (set, node->loc, node->init, cdv, 0,
			      node->set_src, NO_INSERT);
	  else if (GET_CODE (node->loc) == MEM)
	    var_mem_decl_set (set, node->loc, node->init, cdv, 0,
			      node->set_src, NO_INSERT);
	  else
	    set_variable_part (set, node->loc, cdv, 0,
			       node->init, node->set_src, NO_INSERT);
	}
    }

  /* We remove this last, to make sure that the canonical value is not
     removed to the point of requiring reinsertion.  */
  if (cval)
    delete_variable_part (set, dv_as_value (dv), dv_from_value (cval), 0);

  clobber_variable_part (set, NULL, dv, 0, NULL);

  /* ??? Should we make sure there aren't other available values or
     variables whose values involve this one other than by
     equivalence?  E.g., at the very least we should reset MEMs, those
     shouldn't be too hard to find cselib-looking up the value as an
     address, then locating the resulting value in our own hash
     table.  */
}

/* Find the values in a given location and map the val to another
   value, if it is unique, or add the location as one holding the
   value.  */

static void
val_resolve (dataflow_set *set, rtx val, rtx loc, rtx insn)
{
  decl_or_value dv = dv_from_value (val);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (insn)
	fprintf (dump_file, "%i: ", INSN_UID (insn));
      else
	fprintf (dump_file, "head: ");
      print_inline_rtx (dump_file, val, 0);
      fputs (" is at ", dump_file);
      print_inline_rtx (dump_file, loc, 0);
      fputc ('\n', dump_file);
    }

  val_reset (set, dv);

  if (REG_P (loc))
    {
      attrs node, found = NULL;

      for (node = set->regs[REGNO (loc)]; node; node = node->next)
	if (dv_is_value_p (node->dv)
	    && GET_MODE (dv_as_value (node->dv)) == GET_MODE (loc))
	  {
	    found = node;

	    /* Map incoming equivalences.  ??? Wouldn't it be nice if
	     we just started sharing the location lists?  Maybe a
	     circular list ending at the value itself or some
	     such.  */
	    set_variable_part (set, dv_as_value (node->dv),
			       dv_from_value (val), node->offset,
			       VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
	    set_variable_part (set, val, node->dv, node->offset,
			       VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
	  }

      /* If we didn't find any equivalence, we need to remember that
	 this value is held in the named register.  */
      if (!found)
	var_reg_decl_set (set, loc, VAR_INIT_STATUS_INITIALIZED,
			  dv_from_value (val), 0, NULL_RTX, INSERT);
    }
  else if (MEM_P (loc))
    /* ??? Merge equivalent MEMs.  */
    var_mem_decl_set (set, loc, VAR_INIT_STATUS_INITIALIZED,
		      dv_from_value (val), 0, NULL_RTX, INSERT);
  else
    /* ??? Merge equivalent expressions.  */
    set_variable_part (set, loc, dv_from_value (val), 0,
		       VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
}

/* Initialize dataflow set SET to be empty.
   VARS_SIZE is the initial size of hash table VARS.  */

static void
dataflow_set_init (dataflow_set *set)
{
  init_attrs_list_set (set->regs);
  set->vars = shared_hash_copy (empty_shared_hash);
  set->stack_adjust = 0;
  set->traversed_vars = NULL;
}

/* Delete the contents of dataflow set SET.  */

static void
dataflow_set_clear (dataflow_set *set)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_clear (&set->regs[i]);

  shared_hash_destroy (set->vars);
  set->vars = shared_hash_copy (empty_shared_hash);
}

/* Copy the contents of dataflow set SRC to DST.  */

static void
dataflow_set_copy (dataflow_set *dst, dataflow_set *src)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_copy (&dst->regs[i], src->regs[i]);

  shared_hash_destroy (dst->vars);
  dst->vars = shared_hash_copy (src->vars);
  dst->stack_adjust = src->stack_adjust;
}

/* Information for merging lists of locations for a given offset of variable.
 */
struct variable_union_info
{
  /* Node of the location chain.  */
  location_chain lc;

  /* The sum of positions in the input chains.  */
  int pos;

  /* The position in the chain of DST dataflow set.  */
  int pos_dst;
};

/* Buffer for location list sorting and its allocated size.  */
static struct variable_union_info *vui_vec;
static int vui_allocated;

/* Compare function for qsort, order the structures by POS element.  */

static int
variable_union_info_cmp_pos (const void *n1, const void *n2)
{
  const struct variable_union_info *const i1 =
    (const struct variable_union_info *) n1;
  const struct variable_union_info *const i2 =
    ( const struct variable_union_info *) n2;

  if (i1->pos != i2->pos)
    return i1->pos - i2->pos;

  return (i1->pos_dst - i2->pos_dst);
}

/* Compute union of location parts of variable *SLOT and the same variable
   from hash table DATA.  Compute "sorted" union of the location chains
   for common offsets, i.e. the locations of a variable part are sorted by
   a priority where the priority is the sum of the positions in the 2 chains
   (if a location is only in one list the position in the second list is
   defined to be larger than the length of the chains).
   When we are updating the location parts the newest location is in the
   beginning of the chain, so when we do the described "sorted" union
   we keep the newest locations in the beginning.  */

static int
variable_union (void **slot, void *data)
{
  variable src, dst;
  void **dstp;
  dataflow_set *set = (dataflow_set *) data;
  int i, j, k;

  src = (variable) *slot;
  dstp = shared_hash_find_slot (set->vars, src->dv);
  if (!dstp || !*dstp)
    {
      src->refcount++;

      dst_can_be_shared = false;
      if (!dstp)
	dstp = shared_hash_find_slot_unshare (&set->vars, src->dv, INSERT);

      *dstp = src;

      /* Continue traversing the hash table.  */
      return 1;
    }
  else
    dst = (variable) *dstp;

  gcc_assert (src->n_var_parts);

  /* We can combine one-part variables very efficiently, because their
     entries are in canonical order.  */
  if (dv_onepart_p (src->dv))
    {
      location_chain *nodep, dnode, snode;

      gcc_assert (src->n_var_parts == 1);
      gcc_assert (dst->n_var_parts == 1);

      snode = src->var_part[0].loc_chain;
      gcc_assert (snode);

    restart_onepart_unshared:
      nodep = &dst->var_part[0].loc_chain;
      dnode = *nodep;
      gcc_assert (dnode);

      while (snode)
	{
	  int r = dnode ? loc_cmp (dnode->loc, snode->loc) : 1;

	  if (r > 0)
	    {
	      location_chain nnode;

	      if (shared_var_p (dst, set->vars))
		{
		  dstp = unshare_variable (set, dstp, dst,
					   VAR_INIT_STATUS_INITIALIZED);
		  dst = (variable)*dstp;
		  goto restart_onepart_unshared;
		}

	      *nodep = nnode = (location_chain) pool_alloc (loc_chain_pool);
	      nnode->loc = snode->loc;
	      nnode->init = snode->init;
	      if (!snode->set_src || MEM_P (snode->set_src))
		nnode->set_src = NULL;
	      else
		nnode->set_src = snode->set_src;
	      nnode->next = dnode;
	      dnode = nnode;
	    }
#ifdef ENABLE_CHECKING
	  else if (r == 0)
	    gcc_assert (rtx_equal_p (dnode->loc, snode->loc));
#endif

	  if (r >= 0)
	    snode = snode->next;

	  nodep = &dnode->next;
	  dnode = *nodep;
	}

      return 1;
    }

  /* Count the number of location parts, result is K.  */
  for (i = 0, j = 0, k = 0;
       i < src->n_var_parts && j < dst->n_var_parts; k++)
    {
      if (src->var_part[i].offset == dst->var_part[j].offset)
	{
	  i++;
	  j++;
	}
      else if (src->var_part[i].offset < dst->var_part[j].offset)
	i++;
      else
	j++;
    }
  k += src->n_var_parts - i;
  k += dst->n_var_parts - j;

  /* We track only variables whose size is <= MAX_VAR_PARTS bytes
     thus there are at most MAX_VAR_PARTS different offsets.  */
  gcc_assert (dv_onepart_p (dst->dv) ? k == 1 : k <= MAX_VAR_PARTS);

  if (dst->n_var_parts != k && shared_var_p (dst, set->vars))
    {
      dstp = unshare_variable (set, dstp, dst, VAR_INIT_STATUS_UNKNOWN);
      dst = (variable)*dstp;
    }

  i = src->n_var_parts - 1;
  j = dst->n_var_parts - 1;
  dst->n_var_parts = k;

  for (k--; k >= 0; k--)
    {
      location_chain node, node2;

      if (i >= 0 && j >= 0
	  && src->var_part[i].offset == dst->var_part[j].offset)
	{
	  /* Compute the "sorted" union of the chains, i.e. the locations which
	     are in both chains go first, they are sorted by the sum of
	     positions in the chains.  */
	  int dst_l, src_l;
	  int ii, jj, n;
	  struct variable_union_info *vui;

	  /* If DST is shared compare the location chains.
	     If they are different we will modify the chain in DST with
	     high probability so make a copy of DST.  */
	  if (shared_var_p (dst, set->vars))
	    {
	      for (node = src->var_part[i].loc_chain,
		   node2 = dst->var_part[j].loc_chain; node && node2;
		   node = node->next, node2 = node2->next)
		{
		  if (!((REG_P (node2->loc)
			 && REG_P (node->loc)
			 && REGNO (node2->loc) == REGNO (node->loc))
			|| rtx_equal_p (node2->loc, node->loc)))
		    {
		      if (node2->init < node->init)
		        node2->init = node->init;
		      break;
		    }
		}
	      if (node || node2)
		{
		  dstp = unshare_variable (set, dstp, dst,
					   VAR_INIT_STATUS_UNKNOWN);
		  dst = (variable)*dstp;
		}
	    }

	  src_l = 0;
	  for (node = src->var_part[i].loc_chain; node; node = node->next)
	    src_l++;
	  dst_l = 0;
	  for (node = dst->var_part[j].loc_chain; node; node = node->next)
	    dst_l++;

	  if (dst_l == 1)
	    {
	      /* The most common case, much simpler, no qsort is needed.  */
	      location_chain dstnode = dst->var_part[j].loc_chain;
	      dst->var_part[k].loc_chain = dstnode;
	      dst->var_part[k].offset = dst->var_part[j].offset;
	      node2 = dstnode;
	      for (node = src->var_part[i].loc_chain; node; node = node->next)
		if (!((REG_P (dstnode->loc)
		       && REG_P (node->loc)
		       && REGNO (dstnode->loc) == REGNO (node->loc))
		      || rtx_equal_p (dstnode->loc, node->loc)))
		  {
		    location_chain new_node;

		    /* Copy the location from SRC.  */
		    new_node = (location_chain) pool_alloc (loc_chain_pool);
		    new_node->loc = node->loc;
		    new_node->init = node->init;
		    if (!node->set_src || MEM_P (node->set_src))
		      new_node->set_src = NULL;
		    else
		      new_node->set_src = node->set_src;
		    node2->next = new_node;
		    node2 = new_node;
		  }
	      node2->next = NULL;
	    }
	  else
	    {
	      if (src_l + dst_l > vui_allocated)
		{
		  vui_allocated = MAX (vui_allocated * 2, src_l + dst_l);
		  vui_vec = XRESIZEVEC (struct variable_union_info, vui_vec,
					vui_allocated);
		}
	      vui = vui_vec;

	      /* Fill in the locations from DST.  */
	      for (node = dst->var_part[j].loc_chain, jj = 0; node;
		   node = node->next, jj++)
		{
		  vui[jj].lc = node;
		  vui[jj].pos_dst = jj;

		  /* Pos plus value larger than a sum of 2 valid positions.  */
		  vui[jj].pos = jj + src_l + dst_l;
		}

	      /* Fill in the locations from SRC.  */
	      n = dst_l;
	      for (node = src->var_part[i].loc_chain, ii = 0; node;
		   node = node->next, ii++)
		{
		  /* Find location from NODE.  */
		  for (jj = 0; jj < dst_l; jj++)
		    {
		      if ((REG_P (vui[jj].lc->loc)
			   && REG_P (node->loc)
			   && REGNO (vui[jj].lc->loc) == REGNO (node->loc))
			  || rtx_equal_p (vui[jj].lc->loc, node->loc))
			{
			  vui[jj].pos = jj + ii;
			  break;
			}
		    }
		  if (jj >= dst_l)	/* The location has not been found.  */
		    {
		      location_chain new_node;

		      /* Copy the location from SRC.  */
		      new_node = (location_chain) pool_alloc (loc_chain_pool);
		      new_node->loc = node->loc;
		      new_node->init = node->init;
		      if (!node->set_src || MEM_P (node->set_src))
			new_node->set_src = NULL;
		      else
			new_node->set_src = node->set_src;
		      vui[n].lc = new_node;
		      vui[n].pos_dst = src_l + dst_l;
		      vui[n].pos = ii + src_l + dst_l;
		      n++;
		    }
		}

	      if (dst_l == 2)
		{
		  /* Special case still very common case.  For dst_l == 2
		     all entries dst_l ... n-1 are sorted, with for i >= dst_l
		     vui[i].pos == i + src_l + dst_l.  */
		  if (vui[0].pos > vui[1].pos)
		    {
		      /* Order should be 1, 0, 2... */
		      dst->var_part[k].loc_chain = vui[1].lc;
		      vui[1].lc->next = vui[0].lc;
		      if (n >= 3)
			{
			  vui[0].lc->next = vui[2].lc;
			  vui[n - 1].lc->next = NULL;
			}
		      else
			vui[0].lc->next = NULL;
		      ii = 3;
		    }
		  else
		    {
		      dst->var_part[k].loc_chain = vui[0].lc;
		      if (n >= 3 && vui[2].pos < vui[1].pos)
			{
			  /* Order should be 0, 2, 1, 3... */
			  vui[0].lc->next = vui[2].lc;
			  vui[2].lc->next = vui[1].lc;
			  if (n >= 4)
			    {
			      vui[1].lc->next = vui[3].lc;
			      vui[n - 1].lc->next = NULL;
			    }
			  else
			    vui[1].lc->next = NULL;
			  ii = 4;
			}
		      else
			{
			  /* Order should be 0, 1, 2... */
			  ii = 1;
			  vui[n - 1].lc->next = NULL;
			}
		    }
		  for (; ii < n; ii++)
		    vui[ii - 1].lc->next = vui[ii].lc;
		}
	      else
		{
		  qsort (vui, n, sizeof (struct variable_union_info),
			 variable_union_info_cmp_pos);

		  /* Reconnect the nodes in sorted order.  */
		  for (ii = 1; ii < n; ii++)
		    vui[ii - 1].lc->next = vui[ii].lc;
		  vui[n - 1].lc->next = NULL;
		  dst->var_part[k].loc_chain = vui[0].lc;
		}

	      dst->var_part[k].offset = dst->var_part[j].offset;
	    }
	  i--;
	  j--;
	}
      else if ((i >= 0 && j >= 0
		&& src->var_part[i].offset < dst->var_part[j].offset)
	       || i < 0)
	{
	  dst->var_part[k] = dst->var_part[j];
	  j--;
	}
      else if ((i >= 0 && j >= 0
		&& src->var_part[i].offset > dst->var_part[j].offset)
	       || j < 0)
	{
	  location_chain *nextp;

	  /* Copy the chain from SRC.  */
	  nextp = &dst->var_part[k].loc_chain;
	  for (node = src->var_part[i].loc_chain; node; node = node->next)
	    {
	      location_chain new_lc;

	      new_lc = (location_chain) pool_alloc (loc_chain_pool);
	      new_lc->next = NULL;
	      new_lc->init = node->init;
	      if (!node->set_src || MEM_P (node->set_src))
		new_lc->set_src = NULL;
	      else
		new_lc->set_src = node->set_src;
	      new_lc->loc = node->loc;

	      *nextp = new_lc;
	      nextp = &new_lc->next;
	    }

	  dst->var_part[k].offset = src->var_part[i].offset;
	  i--;
	}
      dst->var_part[k].cur_loc = NULL;
    }

  if (flag_var_tracking_uninit)
    for (i = 0; i < src->n_var_parts && i < dst->n_var_parts; i++)
      {
	location_chain node, node2;
	for (node = src->var_part[i].loc_chain; node; node = node->next)
	  for (node2 = dst->var_part[i].loc_chain; node2; node2 = node2->next)
	    if (rtx_equal_p (node->loc, node2->loc))
	      {
		if (node->init > node2->init)
		  node2->init = node->init;
	      }
      }

  /* Continue traversing the hash table.  */
  return 1;
}

/* Compute union of dataflow sets SRC and DST and store it to DST.  */

static void
dataflow_set_union (dataflow_set *dst, dataflow_set *src)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_union (&dst->regs[i], src->regs[i]);

  if (dst->vars == empty_shared_hash)
    {
      shared_hash_destroy (dst->vars);
      dst->vars = shared_hash_copy (src->vars);
    }
  else
    htab_traverse (shared_hash_htab (src->vars), variable_union, dst);
}

/* Whether the value is currently being expanded.  */
#define VALUE_RECURSED_INTO(x) \
  (RTL_FLAG_CHECK2 ("VALUE_RECURSED_INTO", (x), VALUE, DEBUG_EXPR)->used)
/* Whether the value is in changed_variables hash table.  */
#define VALUE_CHANGED(x) \
  (RTL_FLAG_CHECK1 ("VALUE_CHANGED", (x), VALUE)->frame_related)
/* Whether the decl is in changed_variables hash table.  */
#define DECL_CHANGED(x) TREE_VISITED (x)

/* Record that DV has been added into resp. removed from changed_variables
   hashtable.  */

static inline void
set_dv_changed (decl_or_value dv, bool newv)
{
  if (dv_is_value_p (dv))
    VALUE_CHANGED (dv_as_value (dv)) = newv;
  else
    DECL_CHANGED (dv_as_decl (dv)) = newv;
}

/* Return true if DV is present in changed_variables hash table.  */

static inline bool
dv_changed_p (decl_or_value dv)
{
  return (dv_is_value_p (dv)
	  ? VALUE_CHANGED (dv_as_value (dv))
	  : DECL_CHANGED (dv_as_decl (dv)));
}

/* Return a location list node whose loc is rtx_equal to LOC, in the
   location list of a one-part variable or value VAR, or in that of
   any values recursively mentioned in the location lists.  */

static location_chain
find_loc_in_1pdv (rtx loc, variable var, htab_t vars)
{
  location_chain node;

  if (!var)
    return NULL;

  gcc_assert (dv_onepart_p (var->dv));

  if (!var->n_var_parts)
    return NULL;

  gcc_assert (var->var_part[0].offset == 0);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (rtx_equal_p (loc, node->loc))
      return node;
    else if (GET_CODE (node->loc) == VALUE
	     && !VALUE_RECURSED_INTO (node->loc))
      {
	decl_or_value dv = dv_from_value (node->loc);
	variable var = (variable)
		       htab_find_with_hash (vars, dv, dv_htab_hash (dv));

	if (var)
	  {
	    location_chain where;
	    VALUE_RECURSED_INTO (node->loc) = true;
	    if ((where = find_loc_in_1pdv (loc, var, vars)))
	      {
		VALUE_RECURSED_INTO (node->loc) = false;
		return where;
	      }
	    VALUE_RECURSED_INTO (node->loc) = false;
	  }
      }

  return NULL;
}

/* Hash table iteration argument passed to variable_merge.  */
struct dfset_merge
{
  /* The set in which the merge is to be inserted.  */
  dataflow_set *dst;
  /* The set that we're iterating in.  */
  dataflow_set *cur;
  /* The set that may contain the other dv we are to merge with.  */
  dataflow_set *src;
  /* Number of onepart dvs in src.  */
  int src_onepart_cnt;
};

/* Insert LOC in *DNODE, if it's not there yet.  The list must be in
   loc_cmp order, and it is maintained as such.  */

static void
insert_into_intersection (location_chain *nodep, rtx loc,
			  enum var_init_status status)
{
  location_chain node;
  int r;

  for (node = *nodep; node; nodep = &node->next, node = *nodep)
    if ((r = loc_cmp (node->loc, loc)) == 0)
      {
	node->init = MIN (node->init, status);
	return;
      }
    else if (r > 0)
      break;

  node = (location_chain) pool_alloc (loc_chain_pool);

  node->loc = loc;
  node->set_src = NULL;
  node->init = status;
  node->next = *nodep;
  *nodep = node;
}

/* Insert in DEST the intersection the locations present in both
   S1NODE and S2VAR, directly or indirectly.  S1NODE is from a
   variable in DSM->cur, whereas S2VAR is from DSM->src.  dvar is in
   DSM->dst.  */

static void
intersect_loc_chains (rtx val, location_chain *dest, struct dfset_merge *dsm,
		      location_chain s1node, variable s2var)
{
  dataflow_set *s1set = dsm->cur;
  dataflow_set *s2set = dsm->src;
  location_chain found;

  for (; s1node; s1node = s1node->next)
    {
      if (s1node->loc == val)
	continue;

      if ((found = find_loc_in_1pdv (s1node->loc, s2var,
				     shared_hash_htab (s2set->vars))))
	{
	  insert_into_intersection (dest, s1node->loc,
				    MIN (s1node->init, found->init));
	  continue;
	}

      if (GET_CODE (s1node->loc) == VALUE
	  && !VALUE_RECURSED_INTO (s1node->loc))
	{
	  decl_or_value dv = dv_from_value (s1node->loc);
	  variable svar = shared_hash_find (s1set->vars, dv);
	  if (svar)
	    {
	      if (svar->n_var_parts == 1)
		{
		  VALUE_RECURSED_INTO (s1node->loc) = true;
		  intersect_loc_chains (val, dest, dsm,
					svar->var_part[0].loc_chain,
					s2var);
		  VALUE_RECURSED_INTO (s1node->loc) = false;
		}
	    }
	}

      /* ??? if the location is equivalent to any location in src,
	 searched recursively

	   add to dst the values needed to represent the equivalence

     telling whether locations S is equivalent to another dv's
     location list:

       for each location D in the list

         if S and D satisfy rtx_equal_p, then it is present

	 else if D is a value, recurse without cycles

	 else if S and D have the same CODE and MODE

	   for each operand oS and the corresponding oD

	     if oS and oD are not equivalent, then S an D are not equivalent

	     else if they are RTX vectors

	       if any vector oS element is not equivalent to its respective oD,
	       then S and D are not equivalent

   */


    }
}

/* Return -1 if X should be before Y in a location list for a 1-part
   variable, 1 if Y should be before X, and 0 if they're equivalent
   and should not appear in the list.  */

static int
loc_cmp (rtx x, rtx y)
{
  int i, j, r;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;

  if (x == y)
    return 0;

  if (REG_P (x))
    {
      if (!REG_P (y))
	return -1;
      gcc_assert (GET_MODE (x) == GET_MODE (y));
      if (REGNO (x) == REGNO (y))
	return 0;
      else if (REGNO (x) < REGNO (y))
	return -1;
      else
	return 1;
    }

  if (REG_P (y))
    return 1;

  if (MEM_P (x))
    {
      if (!MEM_P (y))
	return -1;
      gcc_assert (GET_MODE (x) == GET_MODE (y));
      return loc_cmp (XEXP (x, 0), XEXP (y, 0));
    }

  if (MEM_P (y))
    return 1;

  if (GET_CODE (x) == VALUE)
    {
      if (GET_CODE (y) != VALUE)
	return -1;
      /* Don't assert the modes are the same, that is true only
	 when not recursing.  (subreg:QI (value:SI 1:1) 0)
	 and (subreg:QI (value:DI 2:2) 0) can be compared,
	 even when the modes are different.  */
      if (canon_value_cmp (x, y))
	return -1;
      else
	return 1;
    }

  if (GET_CODE (y) == VALUE)
    return 1;

  if (GET_CODE (x) == GET_CODE (y))
    /* Compare operands below.  */;
  else if (GET_CODE (x) < GET_CODE (y))
    return -1;
  else
    return 1;

  gcc_assert (GET_MODE (x) == GET_MODE (y));

  if (GET_CODE (x) == DEBUG_EXPR)
    {
      if (DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x))
	  < DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (y)))
	return -1;
#ifdef ENABLE_CHECKING
      gcc_assert (DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x))
		  > DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (y)));
#endif
      return 1;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    switch (fmt[i])
      {
      case 'w':
	if (XWINT (x, i) == XWINT (y, i))
	  break;
	else if (XWINT (x, i) < XWINT (y, i))
	  return -1;
	else
	  return 1;

      case 'n':
      case 'i':
	if (XINT (x, i) == XINT (y, i))
	  break;
	else if (XINT (x, i) < XINT (y, i))
	  return -1;
	else
	  return 1;

      case 'V':
      case 'E':
	/* Compare the vector length first.  */
	if (XVECLEN (x, i) == XVECLEN (y, i))
	  /* Compare the vectors elements.  */;
	else if (XVECLEN (x, i) < XVECLEN (y, i))
	  return -1;
	else
	  return 1;

	for (j = 0; j < XVECLEN (x, i); j++)
	  if ((r = loc_cmp (XVECEXP (x, i, j),
			    XVECEXP (y, i, j))))
	    return r;
	break;

      case 'e':
	if ((r = loc_cmp (XEXP (x, i), XEXP (y, i))))
	  return r;
	break;

      case 'S':
      case 's':
	if (XSTR (x, i) == XSTR (y, i))
	  break;
	if (!XSTR (x, i))
	  return -1;
	if (!XSTR (y, i))
	  return 1;
	if ((r = strcmp (XSTR (x, i), XSTR (y, i))) == 0)
	  break;
	else if (r < 0)
	  return -1;
	else
	  return 1;

      case 'u':
	/* These are just backpointers, so they don't matter.  */
	break;

      case '0':
      case 't':
	break;

	/* It is believed that rtx's at this level will never
	   contain anything but integers and other rtx's,
	   except for within LABEL_REFs and SYMBOL_REFs.  */
      default:
	gcc_unreachable ();
      }

  return 0;
}

/* If decl or value DVP refers to VALUE from *LOC, add backlinks
   from VALUE to DVP.  */

static int
add_value_chain (rtx *loc, void *dvp)
{
  decl_or_value dv, ldv;
  value_chain vc, nvc;
  void **slot;

  if (GET_CODE (*loc) == VALUE)
    ldv = dv_from_value (*loc);
  else if (GET_CODE (*loc) == DEBUG_EXPR)
    ldv = dv_from_decl (DEBUG_EXPR_TREE_DECL (*loc));
  else
    return 0;

  if (dv_as_opaque (ldv) == dvp)
    return 0;

  dv = (decl_or_value) dvp;
  slot = htab_find_slot_with_hash (value_chains, ldv, dv_htab_hash (ldv),
				   INSERT);
  if (!*slot)
    {
      vc = (value_chain) pool_alloc (value_chain_pool);
      vc->dv = ldv;
      vc->next = NULL;
      vc->refcount = 0;
      *slot = (void *) vc;
    }
  else
    {
      for (vc = ((value_chain) *slot)->next; vc; vc = vc->next)
	if (dv_as_opaque (vc->dv) == dv_as_opaque (dv))
	  break;
      if (vc)
	{
	  vc->refcount++;
	  return 0;
	}
    }
  vc = (value_chain) *slot;
  nvc = (value_chain) pool_alloc (value_chain_pool);
  nvc->dv = dv;
  nvc->next = vc->next;
  nvc->refcount = 1;
  vc->next = nvc;
  return 0;
}

/* If decl or value DVP refers to VALUEs from within LOC, add backlinks
   from those VALUEs to DVP.  */

static void
add_value_chains (decl_or_value dv, rtx loc)
{
  if (GET_CODE (loc) == VALUE || GET_CODE (loc) == DEBUG_EXPR)
    {
      add_value_chain (&loc, dv_as_opaque (dv));
      return;
    }
  if (REG_P (loc))
    return;
  if (MEM_P (loc))
    loc = XEXP (loc, 0);
  for_each_rtx (&loc, add_value_chain, dv_as_opaque (dv));
}

/* If CSELIB_VAL_PTR of value DV refer to VALUEs, add backlinks from those
   VALUEs to DV.  */

static void
add_cselib_value_chains (decl_or_value dv)
{
  struct elt_loc_list *l;

  for (l = CSELIB_VAL_PTR (dv_as_value (dv))->locs; l; l = l->next)
    for_each_rtx (&l->loc, add_value_chain, dv_as_opaque (dv));
}

/* If decl or value DVP refers to VALUE from *LOC, remove backlinks
   from VALUE to DVP.  */

static int
remove_value_chain (rtx *loc, void *dvp)
{
  decl_or_value dv, ldv;
  value_chain vc;
  void **slot;

  if (GET_CODE (*loc) == VALUE)
    ldv = dv_from_value (*loc);
  else if (GET_CODE (*loc) == DEBUG_EXPR)
    ldv = dv_from_decl (DEBUG_EXPR_TREE_DECL (*loc));
  else
    return 0;

  if (dv_as_opaque (ldv) == dvp)
    return 0;

  dv = (decl_or_value) dvp;
  slot = htab_find_slot_with_hash (value_chains, ldv, dv_htab_hash (ldv),
				   NO_INSERT);
  for (vc = (value_chain) *slot; vc->next; vc = vc->next)
    if (dv_as_opaque (vc->next->dv) == dv_as_opaque (dv))
      {
	value_chain dvc = vc->next;
	gcc_assert (dvc->refcount > 0);
	if (--dvc->refcount == 0)
	  {
	    vc->next = dvc->next;
	    pool_free (value_chain_pool, dvc);
	    if (vc->next == NULL && vc == (value_chain) *slot)
	      {
		pool_free (value_chain_pool, vc);
		htab_clear_slot (value_chains, slot);
	      }
	  }
	return 0;
      }
  gcc_unreachable ();
}

/* If decl or value DVP refers to VALUEs from within LOC, remove backlinks
   from those VALUEs to DVP.  */

static void
remove_value_chains (decl_or_value dv, rtx loc)
{
  if (GET_CODE (loc) == VALUE || GET_CODE (loc) == DEBUG_EXPR)
    {
      remove_value_chain (&loc, dv_as_opaque (dv));
      return;
    }
  if (REG_P (loc))
    return;
  if (MEM_P (loc))
    loc = XEXP (loc, 0);
  for_each_rtx (&loc, remove_value_chain, dv_as_opaque (dv));
}

#if ENABLE_CHECKING
/* If CSELIB_VAL_PTR of value DV refer to VALUEs, remove backlinks from those
   VALUEs to DV.  */

static void
remove_cselib_value_chains (decl_or_value dv)
{
  struct elt_loc_list *l;

  for (l = CSELIB_VAL_PTR (dv_as_value (dv))->locs; l; l = l->next)
    for_each_rtx (&l->loc, remove_value_chain, dv_as_opaque (dv));
}

/* Check the order of entries in one-part variables.   */

static int
canonicalize_loc_order_check (void **slot, void *data ATTRIBUTE_UNUSED)
{
  variable var = (variable) *slot;
  decl_or_value dv = var->dv;
  location_chain node, next;

#ifdef ENABLE_RTL_CHECKING
  int i;
  for (i = 0; i < var->n_var_parts; i++)
    gcc_assert (var->var_part[0].cur_loc == NULL);
  gcc_assert (!var->cur_loc_changed && !var->in_changed_variables);
#endif

  if (!dv_onepart_p (dv))
    return 1;

  gcc_assert (var->n_var_parts == 1);
  node = var->var_part[0].loc_chain;
  gcc_assert (node);

  while ((next = node->next))
    {
      gcc_assert (loc_cmp (node->loc, next->loc) < 0);
      node = next;
    }

  return 1;
}
#endif

/* Mark with VALUE_RECURSED_INTO values that have neighbors that are
   more likely to be chosen as canonical for an equivalence set.
   Ensure less likely values can reach more likely neighbors, making
   the connections bidirectional.  */

static int
canonicalize_values_mark (void **slot, void *data)
{
  dataflow_set *set = (dataflow_set *)data;
  variable var = (variable) *slot;
  decl_or_value dv = var->dv;
  rtx val;
  location_chain node;

  if (!dv_is_value_p (dv))
    return 1;

  gcc_assert (var->n_var_parts == 1);

  val = dv_as_value (dv);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE)
      {
	if (canon_value_cmp (node->loc, val))
	  VALUE_RECURSED_INTO (val) = true;
	else
	  {
	    decl_or_value odv = dv_from_value (node->loc);
	    void **oslot = shared_hash_find_slot_noinsert (set->vars, odv);

	    oslot = set_slot_part (set, val, oslot, odv, 0,
				   node->init, NULL_RTX);

	    VALUE_RECURSED_INTO (node->loc) = true;
	  }
      }

  return 1;
}

/* Remove redundant entries from equivalence lists in onepart
   variables, canonicalizing equivalence sets into star shapes.  */

static int
canonicalize_values_star (void **slot, void *data)
{
  dataflow_set *set = (dataflow_set *)data;
  variable var = (variable) *slot;
  decl_or_value dv = var->dv;
  location_chain node;
  decl_or_value cdv;
  rtx val, cval;
  void **cslot;
  bool has_value;
  bool has_marks;

  if (!dv_onepart_p (dv))
    return 1;

  gcc_assert (var->n_var_parts == 1);

  if (dv_is_value_p (dv))
    {
      cval = dv_as_value (dv);
      if (!VALUE_RECURSED_INTO (cval))
	return 1;
      VALUE_RECURSED_INTO (cval) = false;
    }
  else
    cval = NULL_RTX;

 restart:
  val = cval;
  has_value = false;
  has_marks = false;

  gcc_assert (var->n_var_parts == 1);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE)
      {
	has_value = true;
	if (VALUE_RECURSED_INTO (node->loc))
	  has_marks = true;
	if (canon_value_cmp (node->loc, cval))
	  cval = node->loc;
      }

  if (!has_value)
    return 1;

  if (cval == val)
    {
      if (!has_marks || dv_is_decl_p (dv))
	return 1;

      /* Keep it marked so that we revisit it, either after visiting a
	 child node, or after visiting a new parent that might be
	 found out.  */
      VALUE_RECURSED_INTO (val) = true;

      for (node = var->var_part[0].loc_chain; node; node = node->next)
	if (GET_CODE (node->loc) == VALUE
	    && VALUE_RECURSED_INTO (node->loc))
	  {
	    cval = node->loc;
	  restart_with_cval:
	    VALUE_RECURSED_INTO (cval) = false;
	    dv = dv_from_value (cval);
	    slot = shared_hash_find_slot_noinsert (set->vars, dv);
	    if (!slot)
	      {
		gcc_assert (dv_is_decl_p (var->dv));
		/* The canonical value was reset and dropped.
		   Remove it.  */
		clobber_variable_part (set, NULL, var->dv, 0, NULL);
		return 1;
	      }
	    var = (variable)*slot;
	    gcc_assert (dv_is_value_p (var->dv));
	    if (var->n_var_parts == 0)
	      return 1;
	    gcc_assert (var->n_var_parts == 1);
	    goto restart;
	  }

      VALUE_RECURSED_INTO (val) = false;

      return 1;
    }

  /* Push values to the canonical one.  */
  cdv = dv_from_value (cval);
  cslot = shared_hash_find_slot_noinsert (set->vars, cdv);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (node->loc != cval)
      {
	cslot = set_slot_part (set, node->loc, cslot, cdv, 0,
			       node->init, NULL_RTX);
	if (GET_CODE (node->loc) == VALUE)
	  {
	    decl_or_value ndv = dv_from_value (node->loc);

	    set_variable_part (set, cval, ndv, 0, node->init, NULL_RTX,
			       NO_INSERT);

	    if (canon_value_cmp (node->loc, val))
	      {
		/* If it could have been a local minimum, it's not any more,
		   since it's now neighbor to cval, so it may have to push
		   to it.  Conversely, if it wouldn't have prevailed over
		   val, then whatever mark it has is fine: if it was to
		   push, it will now push to a more canonical node, but if
		   it wasn't, then it has already pushed any values it might
		   have to.  */
		VALUE_RECURSED_INTO (node->loc) = true;
		/* Make sure we visit node->loc by ensuring we cval is
		   visited too.  */
		VALUE_RECURSED_INTO (cval) = true;
	      }
	    else if (!VALUE_RECURSED_INTO (node->loc))
	      /* If we have no need to "recurse" into this node, it's
		 already "canonicalized", so drop the link to the old
		 parent.  */
	      clobber_variable_part (set, cval, ndv, 0, NULL);
	  }
	else if (GET_CODE (node->loc) == REG)
	  {
	    attrs list = set->regs[REGNO (node->loc)], *listp;

	    /* Change an existing attribute referring to dv so that it
	       refers to cdv, removing any duplicate this might
	       introduce, and checking that no previous duplicates
	       existed, all in a single pass.  */

	    while (list)
	      {
		if (list->offset == 0
		    && (dv_as_opaque (list->dv) == dv_as_opaque (dv)
			|| dv_as_opaque (list->dv) == dv_as_opaque (cdv)))
		  break;

		list = list->next;
	      }

	    gcc_assert (list);
	    if (dv_as_opaque (list->dv) == dv_as_opaque (dv))
	      {
		list->dv = cdv;
		for (listp = &list->next; (list = *listp); listp = &list->next)
		  {
		    if (list->offset)
		      continue;

		    if (dv_as_opaque (list->dv) == dv_as_opaque (cdv))
		      {
			*listp = list->next;
			pool_free (attrs_pool, list);
			list = *listp;
			break;
		      }

		    gcc_assert (dv_as_opaque (list->dv) != dv_as_opaque (dv));
		  }
	      }
	    else if (dv_as_opaque (list->dv) == dv_as_opaque (cdv))
	      {
		for (listp = &list->next; (list = *listp); listp = &list->next)
		  {
		    if (list->offset)
		      continue;

		    if (dv_as_opaque (list->dv) == dv_as_opaque (dv))
		      {
			*listp = list->next;
			pool_free (attrs_pool, list);
			list = *listp;
			break;
		      }

		    gcc_assert (dv_as_opaque (list->dv) != dv_as_opaque (cdv));
		  }
	      }
	    else
	      gcc_unreachable ();

#if ENABLE_CHECKING
	    while (list)
	      {
		if (list->offset == 0
		    && (dv_as_opaque (list->dv) == dv_as_opaque (dv)
			|| dv_as_opaque (list->dv) == dv_as_opaque (cdv)))
		  gcc_unreachable ();

		list = list->next;
	      }
#endif
	  }
      }

  if (val)
    cslot = set_slot_part (set, val, cslot, cdv, 0,
			   VAR_INIT_STATUS_INITIALIZED, NULL_RTX);

  slot = clobber_slot_part (set, cval, slot, 0, NULL);

  /* Variable may have been unshared.  */
  var = (variable)*slot;
  gcc_assert (var->n_var_parts && var->var_part[0].loc_chain->loc == cval
	      && var->var_part[0].loc_chain->next == NULL);

  if (VALUE_RECURSED_INTO (cval))
    goto restart_with_cval;

  return 1;
}

/* Bind one-part variables to the canonical value in an equivalence
   set.  Not doing this causes dataflow convergence failure in rare
   circumstances, see PR42873.  Unfortunately we can't do this
   efficiently as part of canonicalize_values_star, since we may not
   have determined or even seen the canonical value of a set when we
   get to a variable that references another member of the set.  */

static int
canonicalize_vars_star (void **slot, void *data)
{
  dataflow_set *set = (dataflow_set *)data;
  variable var = (variable) *slot;
  decl_or_value dv = var->dv;
  location_chain node;
  rtx cval;
  decl_or_value cdv;
  void **cslot;
  variable cvar;
  location_chain cnode;

  if (!dv_onepart_p (dv) || dv_is_value_p (dv))
    return 1;

  gcc_assert (var->n_var_parts == 1);

  node = var->var_part[0].loc_chain;

  if (GET_CODE (node->loc) != VALUE)
    return 1;

  gcc_assert (!node->next);
  cval = node->loc;

  /* Push values to the canonical one.  */
  cdv = dv_from_value (cval);
  cslot = shared_hash_find_slot_noinsert (set->vars, cdv);
  if (!cslot)
    return 1;
  cvar = (variable)*cslot;
  gcc_assert (cvar->n_var_parts == 1);

  cnode = cvar->var_part[0].loc_chain;

  /* CVAL is canonical if its value list contains non-VALUEs or VALUEs
     that are not “more canonical” than it.  */
  if (GET_CODE (cnode->loc) != VALUE
      || !canon_value_cmp (cnode->loc, cval))
    return 1;

  /* CVAL was found to be non-canonical.  Change the variable to point
     to the canonical VALUE.  */
  gcc_assert (!cnode->next);
  cval = cnode->loc;

  slot = set_slot_part (set, cval, slot, dv, 0,
			node->init, node->set_src);
  slot = clobber_slot_part (set, cval, slot, 0, node->set_src);

  return 1;
}

/* Combine variable or value in *S1SLOT (in DSM->cur) with the
   corresponding entry in DSM->src.  Multi-part variables are combined
   with variable_union, whereas onepart dvs are combined with
   intersection.  */

static int
variable_merge_over_cur (void **s1slot, void *data)
{
  struct dfset_merge *dsm = (struct dfset_merge *)data;
  dataflow_set *dst = dsm->dst;
  void **dstslot;
  variable s1var = (variable) *s1slot;
  variable s2var, dvar = NULL;
  decl_or_value dv = s1var->dv;
  bool onepart = dv_onepart_p (dv);
  rtx val;
  hashval_t dvhash;
  location_chain node, *nodep;

  /* If the incoming onepart variable has an empty location list, then
     the intersection will be just as empty.  For other variables,
     it's always union.  */
  gcc_assert (s1var->n_var_parts);
  gcc_assert (s1var->var_part[0].loc_chain);

  if (!onepart)
    return variable_union (s1slot, dst);

  gcc_assert (s1var->n_var_parts == 1);
  gcc_assert (s1var->var_part[0].offset == 0);

  dvhash = dv_htab_hash (dv);
  if (dv_is_value_p (dv))
    val = dv_as_value (dv);
  else
    val = NULL;

  s2var = shared_hash_find_1 (dsm->src->vars, dv, dvhash);
  if (!s2var)
    {
      dst_can_be_shared = false;
      return 1;
    }

  dsm->src_onepart_cnt--;
  gcc_assert (s2var->var_part[0].loc_chain);
  gcc_assert (s2var->n_var_parts == 1);
  gcc_assert (s2var->var_part[0].offset == 0);

  dstslot = shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash);
  if (dstslot)
    {
      dvar = (variable)*dstslot;
      gcc_assert (dvar->refcount == 1);
      gcc_assert (dvar->n_var_parts == 1);
      gcc_assert (dvar->var_part[0].offset == 0);
      nodep = &dvar->var_part[0].loc_chain;
    }
  else
    {
      nodep = &node;
      node = NULL;
    }

  if (!dstslot && !onepart_variable_different_p (s1var, s2var))
    {
      dstslot = shared_hash_find_slot_unshare_1 (&dst->vars, dv,
						 dvhash, INSERT);
      *dstslot = dvar = s2var;
      dvar->refcount++;
    }
  else
    {
      dst_can_be_shared = false;

      intersect_loc_chains (val, nodep, dsm,
			    s1var->var_part[0].loc_chain, s2var);

      if (!dstslot)
	{
	  if (node)
	    {
	      dvar = (variable) pool_alloc (dv_pool (dv));
	      dvar->dv = dv;
	      dvar->refcount = 1;
	      dvar->n_var_parts = 1;
	      dvar->cur_loc_changed = false;
	      dvar->in_changed_variables = false;
	      dvar->var_part[0].offset = 0;
	      dvar->var_part[0].loc_chain = node;
	      dvar->var_part[0].cur_loc = NULL;

	      dstslot
		= shared_hash_find_slot_unshare_1 (&dst->vars, dv, dvhash,
						   INSERT);
	      gcc_assert (!*dstslot);
	      *dstslot = dvar;
	    }
	  else
	    return 1;
	}
    }

  nodep = &dvar->var_part[0].loc_chain;
  while ((node = *nodep))
    {
      location_chain *nextp = &node->next;

      if (GET_CODE (node->loc) == REG)
	{
	  attrs list;

	  for (list = dst->regs[REGNO (node->loc)]; list; list = list->next)
	    if (GET_MODE (node->loc) == GET_MODE (list->loc)
		&& dv_is_value_p (list->dv))
	      break;

	  if (!list)
	    attrs_list_insert (&dst->regs[REGNO (node->loc)],
			       dv, 0, node->loc);
	  /* If this value became canonical for another value that had
	     this register, we want to leave it alone.  */
	  else if (dv_as_value (list->dv) != val)
	    {
	      dstslot = set_slot_part (dst, dv_as_value (list->dv),
				       dstslot, dv, 0,
				       node->init, NULL_RTX);
	      dstslot = delete_slot_part (dst, node->loc, dstslot, 0);

	      /* Since nextp points into the removed node, we can't
		 use it.  The pointer to the next node moved to nodep.
		 However, if the variable we're walking is unshared
		 during our walk, we'll keep walking the location list
		 of the previously-shared variable, in which case the
		 node won't have been removed, and we'll want to skip
		 it.  That's why we test *nodep here.  */
	      if (*nodep != node)
		nextp = nodep;
	    }
	}
      else
	/* Canonicalization puts registers first, so we don't have to
	   walk it all.  */
	break;
      nodep = nextp;
    }

  if (dvar != (variable)*dstslot)
    dvar = (variable)*dstslot;
  nodep = &dvar->var_part[0].loc_chain;

  if (val)
    {
      /* Mark all referenced nodes for canonicalization, and make sure
	 we have mutual equivalence links.  */
      VALUE_RECURSED_INTO (val) = true;
      for (node = *nodep; node; node = node->next)
	if (GET_CODE (node->loc) == VALUE)
	  {
	    VALUE_RECURSED_INTO (node->loc) = true;
	    set_variable_part (dst, val, dv_from_value (node->loc), 0,
			       node->init, NULL, INSERT);
	  }

      dstslot = shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash);
      gcc_assert (*dstslot == dvar);
      canonicalize_values_star (dstslot, dst);
#ifdef ENABLE_CHECKING
      gcc_assert (dstslot
		  == shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash));
#endif
      dvar = (variable)*dstslot;
    }
  else
    {
      bool has_value = false, has_other = false;

      /* If we have one value and anything else, we're going to
	 canonicalize this, so make sure all values have an entry in
	 the table and are marked for canonicalization.  */
      for (node = *nodep; node; node = node->next)
	{
	  if (GET_CODE (node->loc) == VALUE)
	    {
	      /* If this was marked during register canonicalization,
		 we know we have to canonicalize values.  */
	      if (has_value)
		has_other = true;
	      has_value = true;
	      if (has_other)
		break;
	    }
	  else
	    {
	      has_other = true;
	      if (has_value)
		break;
	    }
	}

      if (has_value && has_other)
	{
	  for (node = *nodep; node; node = node->next)
	    {
	      if (GET_CODE (node->loc) == VALUE)
		{
		  decl_or_value dv = dv_from_value (node->loc);
		  void **slot = NULL;

		  if (shared_hash_shared (dst->vars))
		    slot = shared_hash_find_slot_noinsert (dst->vars, dv);
		  if (!slot)
		    slot = shared_hash_find_slot_unshare (&dst->vars, dv,
							  INSERT);
		  if (!*slot)
		    {
		      variable var = (variable) pool_alloc (dv_pool (dv));
		      var->dv = dv;
		      var->refcount = 1;
		      var->n_var_parts = 1;
		      var->cur_loc_changed = false;
		      var->in_changed_variables = false;
		      var->var_part[0].offset = 0;
		      var->var_part[0].loc_chain = NULL;
		      var->var_part[0].cur_loc = NULL;
		      *slot = var;
		    }

		  VALUE_RECURSED_INTO (node->loc) = true;
		}
	    }

	  dstslot = shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash);
	  gcc_assert (*dstslot == dvar);
	  canonicalize_values_star (dstslot, dst);
#ifdef ENABLE_CHECKING
	  gcc_assert (dstslot
		      == shared_hash_find_slot_noinsert_1 (dst->vars,
							   dv, dvhash));
#endif
	  dvar = (variable)*dstslot;
	}
    }

  if (!onepart_variable_different_p (dvar, s2var))
    {
      variable_htab_free (dvar);
      *dstslot = dvar = s2var;
      dvar->refcount++;
    }
  else if (s2var != s1var && !onepart_variable_different_p (dvar, s1var))
    {
      variable_htab_free (dvar);
      *dstslot = dvar = s1var;
      dvar->refcount++;
      dst_can_be_shared = false;
    }
  else
    dst_can_be_shared = false;

  return 1;
}

/* Copy s2slot (in DSM->src) to DSM->dst if the variable is a
   multi-part variable.  Unions of multi-part variables and
   intersections of one-part ones will be handled in
   variable_merge_over_cur().  */

static int
variable_merge_over_src (void **s2slot, void *data)
{
  struct dfset_merge *dsm = (struct dfset_merge *)data;
  dataflow_set *dst = dsm->dst;
  variable s2var = (variable) *s2slot;
  decl_or_value dv = s2var->dv;
  bool onepart = dv_onepart_p (dv);

  if (!onepart)
    {
      void **dstp = shared_hash_find_slot (dst->vars, dv);
      *dstp = s2var;
      s2var->refcount++;
      return 1;
    }

  dsm->src_onepart_cnt++;
  return 1;
}

/* Combine dataflow set information from SRC2 into DST, using PDST
   to carry over information across passes.  */

static void
dataflow_set_merge (dataflow_set *dst, dataflow_set *src2)
{
  dataflow_set cur = *dst;
  dataflow_set *src1 = &cur;
  struct dfset_merge dsm;
  int i;
  size_t src1_elems, src2_elems;

  src1_elems = htab_elements (shared_hash_htab (src1->vars));
  src2_elems = htab_elements (shared_hash_htab (src2->vars));
  dataflow_set_init (dst);
  dst->stack_adjust = cur.stack_adjust;
  shared_hash_destroy (dst->vars);
  dst->vars = (shared_hash) pool_alloc (shared_hash_pool);
  dst->vars->refcount = 1;
  dst->vars->htab
    = htab_create (MAX (src1_elems, src2_elems), variable_htab_hash,
		   variable_htab_eq, variable_htab_free);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_mpdv_union (&dst->regs[i], src1->regs[i], src2->regs[i]);

  dsm.dst = dst;
  dsm.src = src2;
  dsm.cur = src1;
  dsm.src_onepart_cnt = 0;

  htab_traverse (shared_hash_htab (dsm.src->vars), variable_merge_over_src,
		 &dsm);
  htab_traverse (shared_hash_htab (dsm.cur->vars), variable_merge_over_cur,
		 &dsm);

  if (dsm.src_onepart_cnt)
    dst_can_be_shared = false;

  dataflow_set_destroy (src1);
}

/* Mark register equivalences.  */

static void
dataflow_set_equiv_regs (dataflow_set *set)
{
  int i;
  attrs list, *listp;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      rtx canon[NUM_MACHINE_MODES];

      memset (canon, 0, sizeof (canon));

      for (list = set->regs[i]; list; list = list->next)
	if (list->offset == 0 && dv_is_value_p (list->dv))
	  {
	    rtx val = dv_as_value (list->dv);
	    rtx *cvalp = &canon[(int)GET_MODE (val)];
	    rtx cval = *cvalp;

	    if (canon_value_cmp (val, cval))
	      *cvalp = val;
	  }

      for (list = set->regs[i]; list; list = list->next)
	if (list->offset == 0 && dv_onepart_p (list->dv))
	  {
	    rtx cval = canon[(int)GET_MODE (list->loc)];

	    if (!cval)
	      continue;

	    if (dv_is_value_p (list->dv))
	      {
		rtx val = dv_as_value (list->dv);

		if (val == cval)
		  continue;

		VALUE_RECURSED_INTO (val) = true;
		set_variable_part (set, val, dv_from_value (cval), 0,
				   VAR_INIT_STATUS_INITIALIZED,
				   NULL, NO_INSERT);
	      }

	    VALUE_RECURSED_INTO (cval) = true;
	    set_variable_part (set, cval, list->dv, 0,
			       VAR_INIT_STATUS_INITIALIZED, NULL, NO_INSERT);
	  }

      for (listp = &set->regs[i]; (list = *listp);
	   listp = list ? &list->next : listp)
	if (list->offset == 0 && dv_onepart_p (list->dv))
	  {
	    rtx cval = canon[(int)GET_MODE (list->loc)];
	    void **slot;

	    if (!cval)
	      continue;

	    if (dv_is_value_p (list->dv))
	      {
		rtx val = dv_as_value (list->dv);
		if (!VALUE_RECURSED_INTO (val))
		  continue;
	      }

	    slot = shared_hash_find_slot_noinsert (set->vars, list->dv);
	    canonicalize_values_star (slot, set);
	    if (*listp != list)
	      list = NULL;
	  }
    }
}

/* Remove any redundant values in the location list of VAR, which must
   be unshared and 1-part.  */

static void
remove_duplicate_values (variable var)
{
  location_chain node, *nodep;

  gcc_assert (dv_onepart_p (var->dv));
  gcc_assert (var->n_var_parts == 1);
  gcc_assert (var->refcount == 1);

  for (nodep = &var->var_part[0].loc_chain; (node = *nodep); )
    {
      if (GET_CODE (node->loc) == VALUE)
	{
	  if (VALUE_RECURSED_INTO (node->loc))
	    {
	      /* Remove duplicate value node.  */
	      *nodep = node->next;
	      pool_free (loc_chain_pool, node);
	      continue;
	    }
	  else
	    VALUE_RECURSED_INTO (node->loc) = true;
	}
      nodep = &node->next;
    }

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE)
      {
	gcc_assert (VALUE_RECURSED_INTO (node->loc));
	VALUE_RECURSED_INTO (node->loc) = false;
      }
}


/* Hash table iteration argument passed to variable_post_merge.  */
struct dfset_post_merge
{
  /* The new input set for the current block.  */
  dataflow_set *set;
  /* Pointer to the permanent input set for the current block, or
     NULL.  */
  dataflow_set **permp;
};

/* Create values for incoming expressions associated with one-part
   variables that don't have value numbers for them.  */

static int
variable_post_merge_new_vals (void **slot, void *info)
{
  struct dfset_post_merge *dfpm = (struct dfset_post_merge *)info;
  dataflow_set *set = dfpm->set;
  variable var = (variable)*slot;
  location_chain node;

  if (!dv_onepart_p (var->dv) || !var->n_var_parts)
    return 1;

  gcc_assert (var->n_var_parts == 1);

  if (dv_is_decl_p (var->dv))
    {
      bool check_dupes = false;

    restart:
      for (node = var->var_part[0].loc_chain; node; node = node->next)
	{
	  if (GET_CODE (node->loc) == VALUE)
	    gcc_assert (!VALUE_RECURSED_INTO (node->loc));
	  else if (GET_CODE (node->loc) == REG)
	    {
	      attrs att, *attp, *curp = NULL;

	      if (var->refcount != 1)
		{
		  slot = unshare_variable (set, slot, var,
					   VAR_INIT_STATUS_INITIALIZED);
		  var = (variable)*slot;
		  goto restart;
		}

	      for (attp = &set->regs[REGNO (node->loc)]; (att = *attp);
		   attp = &att->next)
		if (att->offset == 0
		    && GET_MODE (att->loc) == GET_MODE (node->loc))
		  {
		    if (dv_is_value_p (att->dv))
		      {
			rtx cval = dv_as_value (att->dv);
			node->loc = cval;
			check_dupes = true;
			break;
		      }
		    else if (dv_as_opaque (att->dv) == dv_as_opaque (var->dv))
		      curp = attp;
		  }

	      if (!curp)
		{
		  curp = attp;
		  while (*curp)
		    if ((*curp)->offset == 0
			&& GET_MODE ((*curp)->loc) == GET_MODE (node->loc)
			&& dv_as_opaque ((*curp)->dv) == dv_as_opaque (var->dv))
		      break;
		    else
		      curp = &(*curp)->next;
		  gcc_assert (*curp);
		}

	      if (!att)
		{
		  decl_or_value cdv;
		  rtx cval;

		  if (!*dfpm->permp)
		    {
		      *dfpm->permp = XNEW (dataflow_set);
		      dataflow_set_init (*dfpm->permp);
		    }

		  for (att = (*dfpm->permp)->regs[REGNO (node->loc)];
		       att; att = att->next)
		    if (GET_MODE (att->loc) == GET_MODE (node->loc))
		      {
			gcc_assert (att->offset == 0);
			gcc_assert (dv_is_value_p (att->dv));
			val_reset (set, att->dv);
			break;
		      }

		  if (att)
		    {
		      cdv = att->dv;
		      cval = dv_as_value (cdv);
		    }
		  else
		    {
		      /* Create a unique value to hold this register,
			 that ought to be found and reused in
			 subsequent rounds.  */
		      cselib_val *v;
		      gcc_assert (!cselib_lookup (node->loc,
						  GET_MODE (node->loc), 0));
		      v = cselib_lookup (node->loc, GET_MODE (node->loc), 1);
		      cselib_preserve_value (v);
		      cselib_invalidate_rtx (node->loc);
		      cval = v->val_rtx;
		      cdv = dv_from_value (cval);
		      if (dump_file)
			fprintf (dump_file,
				 "Created new value %u:%u for reg %i\n",
				 v->uid, v->hash, REGNO (node->loc));
		    }

		  var_reg_decl_set (*dfpm->permp, node->loc,
				    VAR_INIT_STATUS_INITIALIZED,
				    cdv, 0, NULL, INSERT);

		  node->loc = cval;
		  check_dupes = true;
		}

	      /* Remove attribute referring to the decl, which now
		 uses the value for the register, already existing or
		 to be added when we bring perm in.  */
	      att = *curp;
	      *curp = att->next;
	      pool_free (attrs_pool, att);
	    }
	}

      if (check_dupes)
	remove_duplicate_values (var);
    }

  return 1;
}

/* Reset values in the permanent set that are not associated with the
   chosen expression.  */

static int
variable_post_merge_perm_vals (void **pslot, void *info)
{
  struct dfset_post_merge *dfpm = (struct dfset_post_merge *)info;
  dataflow_set *set = dfpm->set;
  variable pvar = (variable)*pslot, var;
  location_chain pnode;
  decl_or_value dv;
  attrs att;

  gcc_assert (dv_is_value_p (pvar->dv));
  gcc_assert (pvar->n_var_parts == 1);
  pnode = pvar->var_part[0].loc_chain;
  gcc_assert (pnode);
  gcc_assert (!pnode->next);
  gcc_assert (REG_P (pnode->loc));

  dv = pvar->dv;

  var = shared_hash_find (set->vars, dv);
  if (var)
    {
      if (find_loc_in_1pdv (pnode->loc, var, shared_hash_htab (set->vars)))
	return 1;
      val_reset (set, dv);
    }

  for (att = set->regs[REGNO (pnode->loc)]; att; att = att->next)
    if (att->offset == 0
	&& GET_MODE (att->loc) == GET_MODE (pnode->loc)
	&& dv_is_value_p (att->dv))
      break;

  /* If there is a value associated with this register already, create
     an equivalence.  */
  if (att && dv_as_value (att->dv) != dv_as_value (dv))
    {
      rtx cval = dv_as_value (att->dv);
      set_variable_part (set, cval, dv, 0, pnode->init, NULL, INSERT);
      set_variable_part (set, dv_as_value (dv), att->dv, 0, pnode->init,
			 NULL, INSERT);
    }
  else if (!att)
    {
      attrs_list_insert (&set->regs[REGNO (pnode->loc)],
			 dv, 0, pnode->loc);
      variable_union (pslot, set);
    }

  return 1;
}

/* Just checking stuff and registering register attributes for
   now.  */

static void
dataflow_post_merge_adjust (dataflow_set *set, dataflow_set **permp)
{
  struct dfset_post_merge dfpm;

  dfpm.set = set;
  dfpm.permp = permp;

  htab_traverse (shared_hash_htab (set->vars), variable_post_merge_new_vals,
		 &dfpm);
  if (*permp)
    htab_traverse (shared_hash_htab ((*permp)->vars),
		   variable_post_merge_perm_vals, &dfpm);
  htab_traverse (shared_hash_htab (set->vars), canonicalize_values_star, set);
  htab_traverse (shared_hash_htab (set->vars), canonicalize_vars_star, set);
}

/* Return a node whose loc is a MEM that refers to EXPR in the
   location list of a one-part variable or value VAR, or in that of
   any values recursively mentioned in the location lists.  */

static location_chain
find_mem_expr_in_1pdv (tree expr, rtx val, htab_t vars)
{
  location_chain node;
  decl_or_value dv;
  variable var;
  location_chain where = NULL;

  if (!val)
    return NULL;

  gcc_assert (GET_CODE (val) == VALUE);

  gcc_assert (!VALUE_RECURSED_INTO (val));

  dv = dv_from_value (val);
  var = (variable) htab_find_with_hash (vars, dv, dv_htab_hash (dv));

  if (!var)
    return NULL;

  gcc_assert (dv_onepart_p (var->dv));

  if (!var->n_var_parts)
    return NULL;

  gcc_assert (var->var_part[0].offset == 0);

  VALUE_RECURSED_INTO (val) = true;

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (MEM_P (node->loc) && MEM_EXPR (node->loc) == expr
	&& MEM_OFFSET (node->loc) == 0)
      {
	where = node;
	break;
      }
    else if (GET_CODE (node->loc) == VALUE
	     && !VALUE_RECURSED_INTO (node->loc)
	     && (where = find_mem_expr_in_1pdv (expr, node->loc, vars)))
      break;

  VALUE_RECURSED_INTO (val) = false;

  return where;
}

/* Return TRUE if the value of MEM may vary across a call.  */

static bool
mem_dies_at_call (rtx mem)
{
  tree expr = MEM_EXPR (mem);
  tree decl;

  if (!expr)
    return true;

  decl = get_base_address (expr);

  if (!decl)
    return true;

  if (!DECL_P (decl))
    return true;

  return (may_be_aliased (decl)
	  || (!TREE_READONLY (decl) && is_global_var (decl)));
}

/* Remove all MEMs from the location list of a hash table entry for a
   one-part variable, except those whose MEM attributes map back to
   the variable itself, directly or within a VALUE.  */

static int
dataflow_set_preserve_mem_locs (void **slot, void *data)
{
  dataflow_set *set = (dataflow_set *) data;
  variable var = (variable) *slot;

  if (dv_is_decl_p (var->dv) && dv_onepart_p (var->dv))
    {
      tree decl = dv_as_decl (var->dv);
      location_chain loc, *locp;
      bool changed = false;

      if (!var->n_var_parts)
	return 1;

      gcc_assert (var->n_var_parts == 1);

      if (shared_var_p (var, set->vars))
	{
	  for (loc = var->var_part[0].loc_chain; loc; loc = loc->next)
	    {
	      /* We want to remove dying MEMs that doesn't refer to
		 DECL.  */
	      if (GET_CODE (loc->loc) == MEM
		  && (MEM_EXPR (loc->loc) != decl
		      || MEM_OFFSET (loc->loc))
		  && !mem_dies_at_call (loc->loc))
		break;
	      /* We want to move here MEMs that do refer to DECL.  */
	      else if (GET_CODE (loc->loc) == VALUE
		       && find_mem_expr_in_1pdv (decl, loc->loc,
						 shared_hash_htab (set->vars)))
		break;
	    }

	  if (!loc)
	    return 1;

	  slot = unshare_variable (set, slot, var, VAR_INIT_STATUS_UNKNOWN);
	  var = (variable)*slot;
	  gcc_assert (var->n_var_parts == 1);
	}

      for (locp = &var->var_part[0].loc_chain, loc = *locp;
	   loc; loc = *locp)
	{
	  rtx old_loc = loc->loc;
	  if (GET_CODE (old_loc) == VALUE)
	    {
	      location_chain mem_node
		= find_mem_expr_in_1pdv (decl, loc->loc,
					 shared_hash_htab (set->vars));

	      /* ??? This picks up only one out of multiple MEMs that
		 refer to the same variable.  Do we ever need to be
		 concerned about dealing with more than one, or, given
		 that they should all map to the same variable
		 location, their addresses will have been merged and
		 they will be regarded as equivalent?  */
	      if (mem_node)
		{
		  loc->loc = mem_node->loc;
		  loc->set_src = mem_node->set_src;
		  loc->init = MIN (loc->init, mem_node->init);
		}
	    }

	  if (GET_CODE (loc->loc) != MEM
	      || (MEM_EXPR (loc->loc) == decl
		  && MEM_OFFSET (loc->loc) == 0)
	      || !mem_dies_at_call (loc->loc))
	    {
	      if (old_loc != loc->loc && emit_notes)
		{
		  if (old_loc == var->var_part[0].cur_loc)
		    {
		      changed = true;
		      var->var_part[0].cur_loc = NULL;
		      var->cur_loc_changed = true;
		    }
		  add_value_chains (var->dv, loc->loc);
		  remove_value_chains (var->dv, old_loc);
		}
	      locp = &loc->next;
	      continue;
	    }

	  if (emit_notes)
	    {
	      remove_value_chains (var->dv, old_loc);
	      if (old_loc == var->var_part[0].cur_loc)
		{
		  changed = true;
		  var->var_part[0].cur_loc = NULL;
		  var->cur_loc_changed = true;
		}
	    }
	  *locp = loc->next;
	  pool_free (loc_chain_pool, loc);
	}

      if (!var->var_part[0].loc_chain)
	{
	  var->n_var_parts--;
	  changed = true;
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return 1;
}

/* Remove all MEMs from the location list of a hash table entry for a
   value.  */

static int
dataflow_set_remove_mem_locs (void **slot, void *data)
{
  dataflow_set *set = (dataflow_set *) data;
  variable var = (variable) *slot;

  if (dv_is_value_p (var->dv))
    {
      location_chain loc, *locp;
      bool changed = false;

      gcc_assert (var->n_var_parts == 1);

      if (shared_var_p (var, set->vars))
	{
	  for (loc = var->var_part[0].loc_chain; loc; loc = loc->next)
	    if (GET_CODE (loc->loc) == MEM
		&& mem_dies_at_call (loc->loc))
	      break;

	  if (!loc)
	    return 1;

	  slot = unshare_variable (set, slot, var, VAR_INIT_STATUS_UNKNOWN);
	  var = (variable)*slot;
	  gcc_assert (var->n_var_parts == 1);
	}

      for (locp = &var->var_part[0].loc_chain, loc = *locp;
	   loc; loc = *locp)
	{
	  if (GET_CODE (loc->loc) != MEM
	      || !mem_dies_at_call (loc->loc))
	    {
	      locp = &loc->next;
	      continue;
	    }

	  if (emit_notes)
	    remove_value_chains (var->dv, loc->loc);
	  *locp = loc->next;
	  /* If we have deleted the location which was last emitted
	     we have to emit new location so add the variable to set
	     of changed variables.  */
	  if (var->var_part[0].cur_loc == loc->loc)
	    {
	      changed = true;
	      var->var_part[0].cur_loc = NULL;
	      var->cur_loc_changed = true;
	    }
	  pool_free (loc_chain_pool, loc);
	}

      if (!var->var_part[0].loc_chain)
	{
	  var->n_var_parts--;
	  changed = true;
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return 1;
}

/* Remove all variable-location information about call-clobbered
   registers, as well as associations between MEMs and VALUEs.  */

static void
dataflow_set_clear_at_call (dataflow_set *set)
{
  int r;

  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
    if (TEST_HARD_REG_BIT (call_used_reg_set, r))
      var_regno_delete (set, r);

  if (MAY_HAVE_DEBUG_INSNS)
    {
      set->traversed_vars = set->vars;
      htab_traverse (shared_hash_htab (set->vars),
		     dataflow_set_preserve_mem_locs, set);
      set->traversed_vars = set->vars;
      htab_traverse (shared_hash_htab (set->vars), dataflow_set_remove_mem_locs,
		     set);
      set->traversed_vars = NULL;
    }
}

/* Flag whether two dataflow sets being compared contain different data.  */
static bool
dataflow_set_different_value;

static bool
variable_part_different_p (variable_part *vp1, variable_part *vp2)
{
  location_chain lc1, lc2;

  for (lc1 = vp1->loc_chain; lc1; lc1 = lc1->next)
    {
      for (lc2 = vp2->loc_chain; lc2; lc2 = lc2->next)
	{
	  if (REG_P (lc1->loc) && REG_P (lc2->loc))
	    {
	      if (REGNO (lc1->loc) == REGNO (lc2->loc))
		break;
	    }
	  if (rtx_equal_p (lc1->loc, lc2->loc))
	    break;
	}
      if (!lc2)
	return true;
    }
  return false;
}

/* Return true if one-part variables VAR1 and VAR2 are different.
   They must be in canonical order.  */

static bool
onepart_variable_different_p (variable var1, variable var2)
{
  location_chain lc1, lc2;

  if (var1 == var2)
    return false;

  gcc_assert (var1->n_var_parts == 1);
  gcc_assert (var2->n_var_parts == 1);

  lc1 = var1->var_part[0].loc_chain;
  lc2 = var2->var_part[0].loc_chain;

  gcc_assert (lc1);
  gcc_assert (lc2);

  while (lc1 && lc2)
    {
      if (loc_cmp (lc1->loc, lc2->loc))
	return true;
      lc1 = lc1->next;
      lc2 = lc2->next;
    }

  return lc1 != lc2;
}

/* Return true if variables VAR1 and VAR2 are different.  */

static bool
variable_different_p (variable var1, variable var2)
{
  int i;

  if (var1 == var2)
    return false;

  if (var1->n_var_parts != var2->n_var_parts)
    return true;

  for (i = 0; i < var1->n_var_parts; i++)
    {
      if (var1->var_part[i].offset != var2->var_part[i].offset)
	return true;
      /* One-part values have locations in a canonical order.  */
      if (i == 0 && var1->var_part[i].offset == 0 && dv_onepart_p (var1->dv))
	{
	  gcc_assert (var1->n_var_parts == 1);
	  gcc_assert (dv_as_opaque (var1->dv) == dv_as_opaque (var2->dv));
	  return onepart_variable_different_p (var1, var2);
	}
      if (variable_part_different_p (&var1->var_part[i], &var2->var_part[i]))
	return true;
      if (variable_part_different_p (&var2->var_part[i], &var1->var_part[i]))
	return true;
    }
  return false;
}

/* Compare variable *SLOT with the same variable in hash table DATA
   and set DATAFLOW_SET_DIFFERENT_VALUE if they are different.  */

static int
dataflow_set_different_1 (void **slot, void *data)
{
  htab_t htab = (htab_t) data;
  variable var1, var2;

  var1 = (variable) *slot;
  var2 = (variable) htab_find_with_hash (htab, var1->dv,
					 dv_htab_hash (var1->dv));
  if (!var2)
    {
      dataflow_set_different_value = true;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "dataflow difference found: removal of:\n");
	  dump_var (var1);
	}

      /* Stop traversing the hash table.  */
      return 0;
    }

  if (variable_different_p (var1, var2))
    {
      dataflow_set_different_value = true;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "dataflow difference found: old and new follow:\n");
	  dump_var (var1);
	  dump_var (var2);
	}

      /* Stop traversing the hash table.  */
      return 0;
    }

  /* Continue traversing the hash table.  */
  return 1;
}

/* Return true if dataflow sets OLD_SET and NEW_SET differ.  */

static bool
dataflow_set_different (dataflow_set *old_set, dataflow_set *new_set)
{
  if (old_set->vars == new_set->vars)
    return false;

  if (htab_elements (shared_hash_htab (old_set->vars))
      != htab_elements (shared_hash_htab (new_set->vars)))
    return true;

  dataflow_set_different_value = false;

  htab_traverse (shared_hash_htab (old_set->vars), dataflow_set_different_1,
		 shared_hash_htab (new_set->vars));
  /* No need to traverse the second hashtab, if both have the same number
     of elements and the second one had all entries found in the first one,
     then it can't have any extra entries.  */
  return dataflow_set_different_value;
}

/* Free the contents of dataflow set SET.  */

static void
dataflow_set_destroy (dataflow_set *set)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_clear (&set->regs[i]);

  shared_hash_destroy (set->vars);
  set->vars = NULL;
}

/* Return true if RTL X contains a SYMBOL_REF.  */

static bool
contains_symbol_ref (rtx x)
{
  const char *fmt;
  RTX_CODE code;
  int i;

  if (!x)
    return false;

  code = GET_CODE (x);
  if (code == SYMBOL_REF)
    return true;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (contains_symbol_ref (XEXP (x, i)))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (contains_symbol_ref (XVECEXP (x, i, j)))
	      return true;
	}
    }

  return false;
}

/* Shall EXPR be tracked?  */

static bool
track_expr_p (tree expr, bool need_rtl)
{
  rtx decl_rtl;
  tree realdecl;

  if (TREE_CODE (expr) == DEBUG_EXPR_DECL)
    return DECL_RTL_SET_P (expr);

  /* If EXPR is not a parameter or a variable do not track it.  */
  if (TREE_CODE (expr) != VAR_DECL && TREE_CODE (expr) != PARM_DECL)
    return 0;

  /* It also must have a name...  */
  if (!DECL_NAME (expr) && need_rtl)
    return 0;

  /* ... and a RTL assigned to it.  */
  decl_rtl = DECL_RTL_IF_SET (expr);
  if (!decl_rtl && need_rtl)
    return 0;

  /* If this expression is really a debug alias of some other declaration, we
     don't need to track this expression if the ultimate declaration is
     ignored.  */
  realdecl = expr;
  if (DECL_DEBUG_EXPR_IS_FROM (realdecl) && DECL_DEBUG_EXPR (realdecl))
    {
      realdecl = DECL_DEBUG_EXPR (realdecl);
      /* ??? We don't yet know how to emit DW_OP_piece for variable
	 that has been SRA'ed.  */
      if (!DECL_P (realdecl))
	return 0;
    }

  /* Do not track EXPR if REALDECL it should be ignored for debugging
     purposes.  */
  if (DECL_IGNORED_P (realdecl))
    return 0;

  /* Do not track global variables until we are able to emit correct location
     list for them.  */
  if (TREE_STATIC (realdecl))
    return 0;

  /* When the EXPR is a DECL for alias of some variable (see example)
     the TREE_STATIC flag is not used.  Disable tracking all DECLs whose
     DECL_RTL contains SYMBOL_REF.

     Example:
     extern char **_dl_argv_internal __attribute__ ((alias ("_dl_argv")));
     char **_dl_argv;
  */
  if (decl_rtl && MEM_P (decl_rtl)
      && contains_symbol_ref (XEXP (decl_rtl, 0)))
    return 0;

  /* If RTX is a memory it should not be very large (because it would be
     an array or struct).  */
  if (decl_rtl && MEM_P (decl_rtl))
    {
      /* Do not track structures and arrays.  */
      if (GET_MODE (decl_rtl) == BLKmode
	  || AGGREGATE_TYPE_P (TREE_TYPE (realdecl)))
	return 0;
      if (MEM_SIZE (decl_rtl)
	  && INTVAL (MEM_SIZE (decl_rtl)) > MAX_VAR_PARTS)
	return 0;
    }

  DECL_CHANGED (expr) = 0;
  DECL_CHANGED (realdecl) = 0;
  return 1;
}

/* Determine whether a given LOC refers to the same variable part as
   EXPR+OFFSET.  */

static bool
same_variable_part_p (rtx loc, tree expr, HOST_WIDE_INT offset)
{
  tree expr2;
  HOST_WIDE_INT offset2;

  if (! DECL_P (expr))
    return false;

  if (REG_P (loc))
    {
      expr2 = REG_EXPR (loc);
      offset2 = REG_OFFSET (loc);
    }
  else if (MEM_P (loc))
    {
      expr2 = MEM_EXPR (loc);
      offset2 = INT_MEM_OFFSET (loc);
    }
  else
    return false;

  if (! expr2 || ! DECL_P (expr2))
    return false;

  expr = var_debug_decl (expr);
  expr2 = var_debug_decl (expr2);

  return (expr == expr2 && offset == offset2);
}

/* LOC is a REG or MEM that we would like to track if possible.
   If EXPR is null, we don't know what expression LOC refers to,
   otherwise it refers to EXPR + OFFSET.  STORE_REG_P is true if
   LOC is an lvalue register.

   Return true if EXPR is nonnull and if LOC, or some lowpart of it,
   is something we can track.  When returning true, store the mode of
   the lowpart we can track in *MODE_OUT (if nonnull) and its offset
   from EXPR in *OFFSET_OUT (if nonnull).  */

static bool
track_loc_p (rtx loc, tree expr, HOST_WIDE_INT offset, bool store_reg_p,
	     enum machine_mode *mode_out, HOST_WIDE_INT *offset_out)
{
  enum machine_mode mode;

  if (expr == NULL || !track_expr_p (expr, true))
    return false;

  /* If REG was a paradoxical subreg, its REG_ATTRS will describe the
     whole subreg, but only the old inner part is really relevant.  */
  mode = GET_MODE (loc);
  if (REG_P (loc) && !HARD_REGISTER_NUM_P (ORIGINAL_REGNO (loc)))
    {
      enum machine_mode pseudo_mode;

      pseudo_mode = PSEUDO_REGNO_MODE (ORIGINAL_REGNO (loc));
      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (pseudo_mode))
	{
	  offset += byte_lowpart_offset (pseudo_mode, mode);
	  mode = pseudo_mode;
	}
    }

  /* If LOC is a paradoxical lowpart of EXPR, refer to EXPR itself.
     Do the same if we are storing to a register and EXPR occupies
     the whole of register LOC; in that case, the whole of EXPR is
     being changed.  We exclude complex modes from the second case
     because the real and imaginary parts are represented as separate
     pseudo registers, even if the whole complex value fits into one
     hard register.  */
  if ((GET_MODE_SIZE (mode) > GET_MODE_SIZE (DECL_MODE (expr))
       || (store_reg_p
	   && !COMPLEX_MODE_P (DECL_MODE (expr))
	   && hard_regno_nregs[REGNO (loc)][DECL_MODE (expr)] == 1))
      && offset + byte_lowpart_offset (DECL_MODE (expr), mode) == 0)
    {
      mode = DECL_MODE (expr);
      offset = 0;
    }

  if (offset < 0 || offset >= MAX_VAR_PARTS)
    return false;

  if (mode_out)
    *mode_out = mode;
  if (offset_out)
    *offset_out = offset;
  return true;
}

/* Return the MODE lowpart of LOC, or null if LOC is not something we
   want to track.  When returning nonnull, make sure that the attributes
   on the returned value are updated.  */

static rtx
var_lowpart (enum machine_mode mode, rtx loc)
{
  unsigned int offset, reg_offset, regno;

  if (!REG_P (loc) && !MEM_P (loc))
    return NULL;

  if (GET_MODE (loc) == mode)
    return loc;

  offset = byte_lowpart_offset (mode, GET_MODE (loc));

  if (MEM_P (loc))
    return adjust_address_nv (loc, mode, offset);

  reg_offset = subreg_lowpart_offset (mode, GET_MODE (loc));
  regno = REGNO (loc) + subreg_regno_offset (REGNO (loc), GET_MODE (loc),
					     reg_offset, mode);
  return gen_rtx_REG_offset (loc, mode, regno, offset);
}

/* arg_pointer_rtx resp. frame_pointer_rtx if stack_pointer_rtx or
   hard_frame_pointer_rtx is being mapped to it.  */
static rtx cfa_base_rtx;

/* Carry information about uses and stores while walking rtx.  */

struct count_use_info
{
  /* The insn where the RTX is.  */
  rtx insn;

  /* The basic block where insn is.  */
  basic_block bb;

  /* The array of n_sets sets in the insn, as determined by cselib.  */
  struct cselib_set *sets;
  int n_sets;

  /* True if we're counting stores, false otherwise.  */
  bool store_p;
};

/* Find a VALUE corresponding to X.   */

static inline cselib_val *
find_use_val (rtx x, enum machine_mode mode, struct count_use_info *cui)
{
  int i;

  if (cui->sets)
    {
      /* This is called after uses are set up and before stores are
	 processed bycselib, so it's safe to look up srcs, but not
	 dsts.  So we look up expressions that appear in srcs or in
	 dest expressions, but we search the sets array for dests of
	 stores.  */
      if (cui->store_p)
	{
	  for (i = 0; i < cui->n_sets; i++)
	    if (cui->sets[i].dest == x)
	      return cui->sets[i].src_elt;
	}
      else
	return cselib_lookup (x, mode, 0);
    }

  return NULL;
}

/* Helper function to get mode of MEM's address.  */

static inline enum machine_mode
get_address_mode (rtx mem)
{
  enum machine_mode mode = GET_MODE (XEXP (mem, 0));
  if (mode != VOIDmode)
    return mode;
  return targetm.addr_space.address_mode (MEM_ADDR_SPACE (mem));
}

/* Replace all registers and addresses in an expression with VALUE
   expressions that map back to them, unless the expression is a
   register.  If no mapping is or can be performed, returns NULL.  */

static rtx
replace_expr_with_values (rtx loc)
{
  if (REG_P (loc))
    return NULL;
  else if (MEM_P (loc))
    {
      cselib_val *addr = cselib_lookup (XEXP (loc, 0),
					get_address_mode (loc), 0);
      if (addr)
	return replace_equiv_address_nv (loc, addr->val_rtx);
      else
	return NULL;
    }
  else
    return cselib_subst_to_values (loc);
}

/* Determine what kind of micro operation to choose for a USE.  Return
   MO_CLOBBER if no micro operation is to be generated.  */

static enum micro_operation_type
use_type (rtx loc, struct count_use_info *cui, enum machine_mode *modep)
{
  tree expr;

  if (cui && cui->sets)
    {
      if (GET_CODE (loc) == VAR_LOCATION)
	{
	  if (track_expr_p (PAT_VAR_LOCATION_DECL (loc), false))
	    {
	      rtx ploc = PAT_VAR_LOCATION_LOC (loc);
	      if (! VAR_LOC_UNKNOWN_P (ploc))
		{
		  cselib_val *val = cselib_lookup (ploc, GET_MODE (loc), 1);

		  /* ??? flag_float_store and volatile mems are never
		     given values, but we could in theory use them for
		     locations.  */
		  gcc_assert (val || 1);
		}
	      return MO_VAL_LOC;
	    }
	  else
	    return MO_CLOBBER;
	}

      if (REG_P (loc) || MEM_P (loc))
	{
	  if (modep)
	    *modep = GET_MODE (loc);
	  if (cui->store_p)
	    {
	      if (REG_P (loc)
		  || (find_use_val (loc, GET_MODE (loc), cui)
		      && cselib_lookup (XEXP (loc, 0),
					get_address_mode (loc), 0)))
		return MO_VAL_SET;
	    }
	  else
	    {
	      cselib_val *val = find_use_val (loc, GET_MODE (loc), cui);

	      if (val && !cselib_preserved_value_p (val))
		return MO_VAL_USE;
	    }
	}
    }

  if (REG_P (loc))
    {
      gcc_assert (REGNO (loc) < FIRST_PSEUDO_REGISTER);

      if (loc == cfa_base_rtx)
	return MO_CLOBBER;
      expr = REG_EXPR (loc);

      if (!expr)
	return MO_USE_NO_VAR;
      else if (target_for_debug_bind (var_debug_decl (expr)))
	return MO_CLOBBER;
      else if (track_loc_p (loc, expr, REG_OFFSET (loc),
			    false, modep, NULL))
	return MO_USE;
      else
	return MO_USE_NO_VAR;
    }
  else if (MEM_P (loc))
    {
      expr = MEM_EXPR (loc);

      if (!expr)
	return MO_CLOBBER;
      else if (target_for_debug_bind (var_debug_decl (expr)))
	return MO_CLOBBER;
      else if (track_loc_p (loc, expr, INT_MEM_OFFSET (loc),
			    false, modep, NULL))
	return MO_USE;
      else
	return MO_CLOBBER;
    }

  return MO_CLOBBER;
}

/* Log to OUT information about micro-operation MOPT involving X in
   INSN of BB.  */

static inline void
log_op_type (rtx x, basic_block bb, rtx insn,
	     enum micro_operation_type mopt, FILE *out)
{
  fprintf (out, "bb %i op %i insn %i %s ",
	   bb->index, VEC_length (micro_operation, VTI (bb)->mos),
	   INSN_UID (insn), micro_operation_type_name[mopt]);
  print_inline_rtx (out, x, 2);
  fputc ('\n', out);
}

/* Tell whether the CONCAT used to holds a VALUE and its location
   needs value resolution, i.e., an attempt of mapping the location
   back to other incoming values.  */
#define VAL_NEEDS_RESOLUTION(x) \
  (RTL_FLAG_CHECK1 ("VAL_NEEDS_RESOLUTION", (x), CONCAT)->volatil)
/* Whether the location in the CONCAT is a tracked expression, that
   should also be handled like a MO_USE.  */
#define VAL_HOLDS_TRACK_EXPR(x) \
  (RTL_FLAG_CHECK1 ("VAL_HOLDS_TRACK_EXPR", (x), CONCAT)->used)
/* Whether the location in the CONCAT should be handled like a MO_COPY
   as well.  */
#define VAL_EXPR_IS_COPIED(x) \
  (RTL_FLAG_CHECK1 ("VAL_EXPR_IS_COPIED", (x), CONCAT)->jump)
/* Whether the location in the CONCAT should be handled like a
   MO_CLOBBER as well.  */
#define VAL_EXPR_IS_CLOBBERED(x) \
  (RTL_FLAG_CHECK1 ("VAL_EXPR_IS_CLOBBERED", (x), CONCAT)->unchanging)
/* Whether the location is a CONCAT of the MO_VAL_SET expression and
   a reverse operation that should be handled afterwards.  */
#define VAL_EXPR_HAS_REVERSE(x) \
  (RTL_FLAG_CHECK1 ("VAL_EXPR_HAS_REVERSE", (x), CONCAT)->return_val)

/* All preserved VALUEs.  */
static VEC (rtx, heap) *preserved_values;

/* Ensure VAL is preserved and remember it in a vector for vt_emit_notes.  */

static void
preserve_value (cselib_val *val)
{
  cselib_preserve_value (val);
  VEC_safe_push (rtx, heap, preserved_values, val->val_rtx);
}

/* Helper function for MO_VAL_LOC handling.  Return non-zero if
   any rtxes not suitable for CONST use not replaced by VALUEs
   are discovered.  */

static int
non_suitable_const (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  if (*x == NULL_RTX)
    return 0;

  switch (GET_CODE (*x))
    {
    case REG:
    case DEBUG_EXPR:
    case PC:
    case SCRATCH:
    case CC0:
    case ASM_INPUT:
    case ASM_OPERANDS:
      return 1;
    case MEM:
      return !MEM_READONLY_P (*x);
    default:
      return 0;
    }
}

/* Add uses (register and memory references) LOC which will be tracked
   to VTI (bb)->mos.  INSN is instruction which the LOC is part of.  */

static int
add_uses (rtx *ploc, void *data)
{
  rtx loc = *ploc;
  enum machine_mode mode = VOIDmode;
  struct count_use_info *cui = (struct count_use_info *)data;
  enum micro_operation_type type = use_type (loc, cui, &mode);

  if (type != MO_CLOBBER)
    {
      basic_block bb = cui->bb;
      micro_operation mo;

      mo.type = type;
      mo.u.loc = type == MO_USE ? var_lowpart (mode, loc) : loc;
      mo.insn = cui->insn;

      if (type == MO_VAL_LOC)
	{
	  rtx oloc = loc;
	  rtx vloc = PAT_VAR_LOCATION_LOC (oloc);
	  cselib_val *val;

	  gcc_assert (cui->sets);

	  if (MEM_P (vloc)
	      && !REG_P (XEXP (vloc, 0))
	      && !MEM_P (XEXP (vloc, 0))
	      && (GET_CODE (XEXP (vloc, 0)) != PLUS
		  || XEXP (XEXP (vloc, 0), 0) != cfa_base_rtx
		  || !CONST_INT_P (XEXP (XEXP (vloc, 0), 1))))
	    {
	      rtx mloc = vloc;
	      enum machine_mode address_mode = get_address_mode (mloc);
	      cselib_val *val
		= cselib_lookup (XEXP (mloc, 0), address_mode, 0);

	      if (val && !cselib_preserved_value_p (val))
		{
		  micro_operation moa;
		  preserve_value (val);
		  mloc = cselib_subst_to_values (XEXP (mloc, 0));
		  moa.type = MO_VAL_USE;
		  moa.insn = cui->insn;
		  moa.u.loc = gen_rtx_CONCAT (address_mode,
					      val->val_rtx, mloc);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    log_op_type (moa.u.loc, cui->bb, cui->insn,
				 moa.type, dump_file);
		  VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &moa);
		}
	    }

	  if (CONSTANT_P (vloc)
	      && (GET_CODE (vloc) != CONST
		  || for_each_rtx (&vloc, non_suitable_const, NULL)))
	    /* For constants don't look up any value.  */;
	  else if (!VAR_LOC_UNKNOWN_P (vloc)
		   && (val = find_use_val (vloc, GET_MODE (oloc), cui)))
	    {
	      enum machine_mode mode2;
	      enum micro_operation_type type2;
	      rtx nloc = replace_expr_with_values (vloc);

	      if (nloc)
		{
		  oloc = shallow_copy_rtx (oloc);
		  PAT_VAR_LOCATION_LOC (oloc) = nloc;
		}

	      oloc = gen_rtx_CONCAT (mode, val->val_rtx, oloc);

	      type2 = use_type (vloc, 0, &mode2);

	      gcc_assert (type2 == MO_USE || type2 == MO_USE_NO_VAR
			  || type2 == MO_CLOBBER);

	      if (type2 == MO_CLOBBER
		  && !cselib_preserved_value_p (val))
		{
		  VAL_NEEDS_RESOLUTION (oloc) = 1;
		  preserve_value (val);
		}
	    }
	  else if (!VAR_LOC_UNKNOWN_P (vloc))
	    {
	      oloc = shallow_copy_rtx (oloc);
	      PAT_VAR_LOCATION_LOC (oloc) = gen_rtx_UNKNOWN_VAR_LOC ();
	    }

	  mo.u.loc = oloc;
	}
      else if (type == MO_VAL_USE)
	{
	  enum machine_mode mode2 = VOIDmode;
	  enum micro_operation_type type2;
	  cselib_val *val = find_use_val (loc, GET_MODE (loc), cui);
	  rtx vloc, oloc = loc, nloc;

	  gcc_assert (cui->sets);

	  if (MEM_P (oloc)
	      && !REG_P (XEXP (oloc, 0))
	      && !MEM_P (XEXP (oloc, 0))
	      && (GET_CODE (XEXP (oloc, 0)) != PLUS
		  || XEXP (XEXP (oloc, 0), 0) != cfa_base_rtx
		  || !CONST_INT_P (XEXP (XEXP (oloc, 0), 1))))
	    {
	      rtx mloc = oloc;
	      enum machine_mode address_mode = get_address_mode (mloc);
	      cselib_val *val
		= cselib_lookup (XEXP (mloc, 0), address_mode, 0);

	      if (val && !cselib_preserved_value_p (val))
		{
		  micro_operation moa;
		  preserve_value (val);
		  mloc = cselib_subst_to_values (XEXP (mloc, 0));
		  moa.type = MO_VAL_USE;
		  moa.insn = cui->insn;
		  moa.u.loc = gen_rtx_CONCAT (address_mode,
					      val->val_rtx, mloc);
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    log_op_type (moa.u.loc, cui->bb, cui->insn,
				 moa.type, dump_file);
		  VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &moa);
		}
	    }

	  type2 = use_type (loc, 0, &mode2);

	  gcc_assert (type2 == MO_USE || type2 == MO_USE_NO_VAR
		      || type2 == MO_CLOBBER);

	  if (type2 == MO_USE)
	    vloc = var_lowpart (mode2, loc);
	  else
	    vloc = oloc;

	  /* The loc of a MO_VAL_USE may have two forms:

	     (concat val src): val is at src, a value-based
	     representation.

	     (concat (concat val use) src): same as above, with use as
	     the MO_USE tracked value, if it differs from src.

	  */

	  nloc = replace_expr_with_values (loc);
	  if (!nloc)
	    nloc = oloc;

	  if (vloc != nloc)
	    oloc = gen_rtx_CONCAT (mode2, val->val_rtx, vloc);
	  else
	    oloc = val->val_rtx;

	  mo.u.loc = gen_rtx_CONCAT (mode, oloc, nloc);

	  if (type2 == MO_USE)
	    VAL_HOLDS_TRACK_EXPR (mo.u.loc) = 1;
	  if (!cselib_preserved_value_p (val))
	    {
	      VAL_NEEDS_RESOLUTION (mo.u.loc) = 1;
	      preserve_value (val);
	    }
	}
      else
	gcc_assert (type == MO_USE || type == MO_USE_NO_VAR);

      if (dump_file && (dump_flags & TDF_DETAILS))
	log_op_type (mo.u.loc, cui->bb, cui->insn, mo.type, dump_file);
      VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &mo);
    }

  return 0;
}

/* Helper function for finding all uses of REG/MEM in X in insn INSN.  */

static void
add_uses_1 (rtx *x, void *cui)
{
  for_each_rtx (x, add_uses, cui);
}

/* Attempt to reverse the EXPR operation in the debug info.  Say for
   reg1 = reg2 + 6 even when reg2 is no longer live we
   can express its value as VAL - 6.  */

static rtx
reverse_op (rtx val, const_rtx expr)
{
  rtx src, arg, ret;
  cselib_val *v;
  enum rtx_code code;

  if (GET_CODE (expr) != SET)
    return NULL_RTX;

  if (!REG_P (SET_DEST (expr)) || GET_MODE (val) != GET_MODE (SET_DEST (expr)))
    return NULL_RTX;

  src = SET_SRC (expr);
  switch (GET_CODE (src))
    {
    case PLUS:
    case MINUS:
    case XOR:
    case NOT:
    case NEG:
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      break;
    default:
      return NULL_RTX;
    }

  if (!REG_P (XEXP (src, 0)) || !SCALAR_INT_MODE_P (GET_MODE (src)))
    return NULL_RTX;

  v = cselib_lookup (XEXP (src, 0), GET_MODE (XEXP (src, 0)), 0);
  if (!v || !cselib_preserved_value_p (v))
    return NULL_RTX;

  switch (GET_CODE (src))
    {
    case NOT:
    case NEG:
      if (GET_MODE (v->val_rtx) != GET_MODE (val))
	return NULL_RTX;
      ret = gen_rtx_fmt_e (GET_CODE (src), GET_MODE (val), val);
      break;
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      ret = gen_lowpart_SUBREG (GET_MODE (v->val_rtx), val);
      break;
    case XOR:
      code = XOR;
      goto binary;
    case PLUS:
      code = MINUS;
      goto binary;
    case MINUS:
      code = PLUS;
      goto binary;
    binary:
      if (GET_MODE (v->val_rtx) != GET_MODE (val))
	return NULL_RTX;
      arg = XEXP (src, 1);
      if (!CONST_INT_P (arg) && GET_CODE (arg) != SYMBOL_REF)
	{
	  arg = cselib_expand_value_rtx (arg, scratch_regs, 5);
	  if (arg == NULL_RTX)
	    return NULL_RTX;
	  if (!CONST_INT_P (arg) && GET_CODE (arg) != SYMBOL_REF)
	    return NULL_RTX;
	}
      ret = simplify_gen_binary (code, GET_MODE (val), val, arg);
      if (ret == val)
	/* Ensure ret isn't VALUE itself (which can happen e.g. for
	   (plus (reg1) (reg2)) when reg2 is known to be 0), as that
	   breaks a lot of routines during var-tracking.  */
	ret = gen_rtx_fmt_ee (PLUS, GET_MODE (val), val, const0_rtx);
      break;
    default:
      gcc_unreachable ();
    }

  return gen_rtx_CONCAT (GET_MODE (v->val_rtx), v->val_rtx, ret);
}

/* Add stores (register and memory references) LOC which will be tracked
   to VTI (bb)->mos.  EXPR is the RTL expression containing the store.
   CUIP->insn is instruction which the LOC is part of.  */

static void
add_stores (rtx loc, const_rtx expr, void *cuip)
{
  enum machine_mode mode = VOIDmode, mode2;
  struct count_use_info *cui = (struct count_use_info *)cuip;
  basic_block bb = cui->bb;
  micro_operation mo;
  rtx oloc = loc, nloc, src = NULL;
  enum micro_operation_type type = use_type (loc, cui, &mode);
  bool track_p = false;
  cselib_val *v;
  bool resolve, preserve;
  rtx reverse;

  if (type == MO_CLOBBER)
    return;

  mode2 = mode;

  if (REG_P (loc))
    {
      gcc_assert (loc != cfa_base_rtx);
      if ((GET_CODE (expr) == CLOBBER && type != MO_VAL_SET)
	  || !(track_p = use_type (loc, NULL, &mode2) == MO_USE)
	  || GET_CODE (expr) == CLOBBER)
	{
	  mo.type = MO_CLOBBER;
	  mo.u.loc = loc;
	}
      else
	{
	  if (GET_CODE (expr) == SET && SET_DEST (expr) == loc)
	    src = var_lowpart (mode2, SET_SRC (expr));
	  loc = var_lowpart (mode2, loc);

	  if (src == NULL)
	    {
	      mo.type = MO_SET;
	      mo.u.loc = loc;
	    }
	  else
	    {
	      rtx xexpr = gen_rtx_SET (VOIDmode, loc, src);
	      if (same_variable_part_p (src, REG_EXPR (loc), REG_OFFSET (loc)))
		mo.type = MO_COPY;
	      else
		mo.type = MO_SET;
	      mo.u.loc = xexpr;
	    }
	}
      mo.insn = cui->insn;
    }
  else if (MEM_P (loc)
	   && ((track_p = use_type (loc, NULL, &mode2) == MO_USE)
	       || cui->sets))
    {
      if (MEM_P (loc) && type == MO_VAL_SET
	  && !REG_P (XEXP (loc, 0))
	  && !MEM_P (XEXP (loc, 0))
	  && (GET_CODE (XEXP (loc, 0)) != PLUS
	      || XEXP (XEXP (loc, 0), 0) != cfa_base_rtx
	      || !CONST_INT_P (XEXP (XEXP (loc, 0), 1))))
	{
	  rtx mloc = loc;
	  enum machine_mode address_mode = get_address_mode (mloc);
	  cselib_val *val = cselib_lookup (XEXP (mloc, 0),
					   address_mode, 0);

	  if (val && !cselib_preserved_value_p (val))
	    {
	      preserve_value (val);
	      mo.type = MO_VAL_USE;
	      mloc = cselib_subst_to_values (XEXP (mloc, 0));
	      mo.u.loc = gen_rtx_CONCAT (address_mode, val->val_rtx, mloc);
	      mo.insn = cui->insn;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		log_op_type (mo.u.loc, cui->bb, cui->insn,
			     mo.type, dump_file);
	      VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &mo);
	    }
	}

      if (GET_CODE (expr) == CLOBBER || !track_p)
	{
	  mo.type = MO_CLOBBER;
	  mo.u.loc = track_p ? var_lowpart (mode2, loc) : loc;
	}
      else
	{
	  if (GET_CODE (expr) == SET && SET_DEST (expr) == loc)
	    src = var_lowpart (mode2, SET_SRC (expr));
	  loc = var_lowpart (mode2, loc);

	  if (src == NULL)
	    {
	      mo.type = MO_SET;
	      mo.u.loc = loc;
	    }
	  else
	    {
	      rtx xexpr = gen_rtx_SET (VOIDmode, loc, src);
	      if (same_variable_part_p (SET_SRC (xexpr),
					MEM_EXPR (loc),
					INT_MEM_OFFSET (loc)))
		mo.type = MO_COPY;
	      else
		mo.type = MO_SET;
	      mo.u.loc = xexpr;
	    }
	}
      mo.insn = cui->insn;
    }
  else
    return;

  if (type != MO_VAL_SET)
    goto log_and_return;

  v = find_use_val (oloc, mode, cui);

  if (!v)
    goto log_and_return;

  resolve = preserve = !cselib_preserved_value_p (v);

  nloc = replace_expr_with_values (oloc);
  if (nloc)
    oloc = nloc;

  if (GET_CODE (PATTERN (cui->insn)) == COND_EXEC)
    {
      cselib_val *oval = cselib_lookup (oloc, GET_MODE (oloc), 0);

      gcc_assert (oval != v);
      gcc_assert (REG_P (oloc) || MEM_P (oloc));

      if (!cselib_preserved_value_p (oval))
	{
	  micro_operation moa;

	  preserve_value (oval);

	  moa.type = MO_VAL_USE;
	  moa.u.loc = gen_rtx_CONCAT (mode, oval->val_rtx, oloc);
	  VAL_NEEDS_RESOLUTION (moa.u.loc) = 1;
	  moa.insn = cui->insn;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    log_op_type (moa.u.loc, cui->bb, cui->insn,
			 moa.type, dump_file);
	  VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &moa);
	}

      resolve = false;
    }
  else if (resolve && GET_CODE (mo.u.loc) == SET)
    {
      nloc = replace_expr_with_values (SET_SRC (expr));

      /* Avoid the mode mismatch between oexpr and expr.  */
      if (!nloc && mode != mode2)
	{
	  nloc = SET_SRC (expr);
	  gcc_assert (oloc == SET_DEST (expr));
	}

      if (nloc)
	oloc = gen_rtx_SET (GET_MODE (mo.u.loc), oloc, nloc);
      else
	{
	  if (oloc == SET_DEST (mo.u.loc))
	    /* No point in duplicating.  */
	    oloc = mo.u.loc;
	  if (!REG_P (SET_SRC (mo.u.loc)))
	    resolve = false;
	}
    }
  else if (!resolve)
    {
      if (GET_CODE (mo.u.loc) == SET
	  && oloc == SET_DEST (mo.u.loc))
	/* No point in duplicating.  */
	oloc = mo.u.loc;
    }
  else
    resolve = false;

  loc = gen_rtx_CONCAT (mode, v->val_rtx, oloc);

  if (mo.u.loc != oloc)
    loc = gen_rtx_CONCAT (GET_MODE (mo.u.loc), loc, mo.u.loc);

  /* The loc of a MO_VAL_SET may have various forms:

     (concat val dst): dst now holds val

     (concat val (set dst src)): dst now holds val, copied from src

     (concat (concat val dstv) dst): dst now holds val; dstv is dst
     after replacing mems and non-top-level regs with values.

     (concat (concat val dstv) (set dst src)): dst now holds val,
     copied from src.  dstv is a value-based representation of dst, if
     it differs from dst.  If resolution is needed, src is a REG, and
     its mode is the same as that of val.

     (concat (concat val (set dstv srcv)) (set dst src)): src
     copied to dst, holding val.  dstv and srcv are value-based
     representations of dst and src, respectively.

  */

  if (GET_CODE (PATTERN (cui->insn)) != COND_EXEC)
    {
      reverse = reverse_op (v->val_rtx, expr);
      if (reverse)
	{
	  loc = gen_rtx_CONCAT (GET_MODE (mo.u.loc), loc, reverse);
	  VAL_EXPR_HAS_REVERSE (loc) = 1;
	}
    }

  mo.u.loc = loc;

  if (track_p)
    VAL_HOLDS_TRACK_EXPR (loc) = 1;
  if (preserve)
    {
      VAL_NEEDS_RESOLUTION (loc) = resolve;
      preserve_value (v);
    }
  if (mo.type == MO_CLOBBER)
    VAL_EXPR_IS_CLOBBERED (loc) = 1;
  if (mo.type == MO_COPY)
    VAL_EXPR_IS_COPIED (loc) = 1;

  mo.type = MO_VAL_SET;

 log_and_return:
  if (dump_file && (dump_flags & TDF_DETAILS))
    log_op_type (mo.u.loc, cui->bb, cui->insn, mo.type, dump_file);
  VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &mo);
}

/* Callback for cselib_record_sets_hook, that records as micro
   operations uses and stores in an insn after cselib_record_sets has
   analyzed the sets in an insn, but before it modifies the stored
   values in the internal tables, unless cselib_record_sets doesn't
   call it directly (perhaps because we're not doing cselib in the
   first place, in which case sets and n_sets will be 0).  */

static void
add_with_sets (rtx insn, struct cselib_set *sets, int n_sets)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  int n1, n2;
  struct count_use_info cui;
  micro_operation *mos;

  cselib_hook_called = true;

  cui.insn = insn;
  cui.bb = bb;
  cui.sets = sets;
  cui.n_sets = n_sets;

  n1 = VEC_length (micro_operation, VTI (bb)->mos);
  cui.store_p = false;
  note_uses (&PATTERN (insn), add_uses_1, &cui);
  n2 = VEC_length (micro_operation, VTI (bb)->mos) - 1;
  mos = VEC_address (micro_operation, VTI (bb)->mos);

  /* Order the MO_USEs to be before MO_USE_NO_VARs and MO_VAL_USE, and
     MO_VAL_LOC last.  */
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type == MO_USE)
	n1++;
      while (n1 < n2 && mos[n2].type != MO_USE)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }

  n2 = VEC_length (micro_operation, VTI (bb)->mos) - 1;
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type != MO_VAL_LOC)
	n1++;
      while (n1 < n2 && mos[n2].type == MO_VAL_LOC)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }

  if (CALL_P (insn))
    {
      micro_operation mo;

      mo.type = MO_CALL;
      mo.insn = insn;
      mo.u.loc = NULL_RTX;

      if (dump_file && (dump_flags & TDF_DETAILS))
	log_op_type (PATTERN (insn), bb, insn, mo.type, dump_file);
      VEC_safe_push (micro_operation, heap, VTI (bb)->mos, &mo);
    }

  n1 = VEC_length (micro_operation, VTI (bb)->mos);
  /* This will record NEXT_INSN (insn), such that we can
     insert notes before it without worrying about any
     notes that MO_USEs might emit after the insn.  */
  cui.store_p = true;
  note_stores (PATTERN (insn), add_stores, &cui);
  n2 = VEC_length (micro_operation, VTI (bb)->mos) - 1;
  mos = VEC_address (micro_operation, VTI (bb)->mos);

  /* Order the MO_VAL_USEs first (note_stores does nothing
     on DEBUG_INSNs, so there are no MO_VAL_LOCs from this
     insn), then MO_CLOBBERs, then MO_SET/MO_COPY/MO_VAL_SET.  */
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type == MO_VAL_USE)
	n1++;
      while (n1 < n2 && mos[n2].type != MO_VAL_USE)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }

  n2 = VEC_length (micro_operation, VTI (bb)->mos) - 1;
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type == MO_CLOBBER)
	n1++;
      while (n1 < n2 && mos[n2].type != MO_CLOBBER)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }
}

static enum var_init_status
find_src_status (dataflow_set *in, rtx src)
{
  tree decl = NULL_TREE;
  enum var_init_status status = VAR_INIT_STATUS_UNINITIALIZED;

  if (! flag_var_tracking_uninit)
    status = VAR_INIT_STATUS_INITIALIZED;

  if (src && REG_P (src))
    decl = var_debug_decl (REG_EXPR (src));
  else if (src && MEM_P (src))
    decl = var_debug_decl (MEM_EXPR (src));

  if (src && decl)
    status = get_init_value (in, src, dv_from_decl (decl));

  return status;
}

/* SRC is the source of an assignment.  Use SET to try to find what
   was ultimately assigned to SRC.  Return that value if known,
   otherwise return SRC itself.  */

static rtx
find_src_set_src (dataflow_set *set, rtx src)
{
  tree decl = NULL_TREE;   /* The variable being copied around.          */
  rtx set_src = NULL_RTX;  /* The value for "decl" stored in "src".      */
  variable var;
  location_chain nextp;
  int i;
  bool found;

  if (src && REG_P (src))
    decl = var_debug_decl (REG_EXPR (src));
  else if (src && MEM_P (src))
    decl = var_debug_decl (MEM_EXPR (src));

  if (src && decl)
    {
      decl_or_value dv = dv_from_decl (decl);

      var = shared_hash_find (set->vars, dv);
      if (var)
	{
	  found = false;
	  for (i = 0; i < var->n_var_parts && !found; i++)
	    for (nextp = var->var_part[i].loc_chain; nextp && !found;
		 nextp = nextp->next)
	      if (rtx_equal_p (nextp->loc, src))
		{
		  set_src = nextp->set_src;
		  found = true;
		}

	}
    }

  return set_src;
}

/* Compute the changes of variable locations in the basic block BB.  */

static bool
compute_bb_dataflow (basic_block bb)
{
  unsigned int i;
  micro_operation *mo;
  bool changed;
  dataflow_set old_out;
  dataflow_set *in = &VTI (bb)->in;
  dataflow_set *out = &VTI (bb)->out;

  dataflow_set_init (&old_out);
  dataflow_set_copy (&old_out, out);
  dataflow_set_copy (out, in);

  for (i = 0; VEC_iterate (micro_operation, VTI (bb)->mos, i, mo); i++)
    {
      rtx insn = mo->insn;

      switch (mo->type)
	{
	  case MO_CALL:
	    dataflow_set_clear_at_call (out);
	    break;

	  case MO_USE:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_set (out, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	      else if (MEM_P (loc))
		var_mem_set (out, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	    }
	    break;

	  case MO_VAL_LOC:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc;
	      tree var;

	      if (GET_CODE (loc) == CONCAT)
		{
		  val = XEXP (loc, 0);
		  vloc = XEXP (loc, 1);
		}
	      else
		{
		  val = NULL_RTX;
		  vloc = loc;
		}

	      var = PAT_VAR_LOCATION_DECL (vloc);

	      clobber_variable_part (out, NULL_RTX,
				     dv_from_decl (var), 0, NULL_RTX);
	      if (val)
		{
		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (out, val, PAT_VAR_LOCATION_LOC (vloc), insn);
		  set_variable_part (out, val, dv_from_decl (var), 0,
				     VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				     INSERT);
		}
	      else if (!VAR_LOC_UNKNOWN_P (PAT_VAR_LOCATION_LOC (vloc)))
		set_variable_part (out, PAT_VAR_LOCATION_LOC (vloc),
				   dv_from_decl (var), 0,
				   VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				   INSERT);
	    }
	    break;

	  case MO_VAL_USE:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc;

	      vloc = uloc = XEXP (loc, 1);
	      val = XEXP (loc, 0);

	      if (GET_CODE (val) == CONCAT)
		{
		  uloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (VAL_NEEDS_RESOLUTION (loc))
		val_resolve (out, val, vloc, insn);
	      else
		val_store (out, val, uloc, insn, false);

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (GET_CODE (uloc) == REG)
		    var_reg_set (out, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		  else if (GET_CODE (uloc) == MEM)
		    var_mem_set (out, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		}
	    }
	    break;

	  case MO_VAL_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc, reverse = NULL_RTX;

	      vloc = loc;
	      if (VAL_EXPR_HAS_REVERSE (loc))
		{
		  reverse = XEXP (loc, 1);
		  vloc = XEXP (loc, 0);
		}
	      uloc = XEXP (vloc, 1);
	      val = XEXP (vloc, 0);
	      vloc = uloc;

	      if (GET_CODE (val) == CONCAT)
		{
		  vloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (GET_CODE (vloc) == SET)
		{
		  rtx vsrc = SET_SRC (vloc);

		  gcc_assert (val != vsrc);
		  gcc_assert (vloc == uloc || VAL_NEEDS_RESOLUTION (loc));

		  vloc = SET_DEST (vloc);

		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (out, val, vsrc, insn);
		}
	      else if (VAL_NEEDS_RESOLUTION (loc))
		{
		  gcc_assert (GET_CODE (uloc) == SET
			      && GET_CODE (SET_SRC (uloc)) == REG);
		  val_resolve (out, val, SET_SRC (uloc), insn);
		}

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (VAL_EXPR_IS_CLOBBERED (loc))
		    {
		      if (REG_P (uloc))
			var_reg_delete (out, uloc, true);
		      else if (MEM_P (uloc))
			var_mem_delete (out, uloc, true);
		    }
		  else
		    {
		      bool copied_p = VAL_EXPR_IS_COPIED (loc);
		      rtx set_src = NULL;
		      enum var_init_status status = VAR_INIT_STATUS_INITIALIZED;

		      if (GET_CODE (uloc) == SET)
			{
			  set_src = SET_SRC (uloc);
			  uloc = SET_DEST (uloc);
			}

		      if (copied_p)
			{
			  if (flag_var_tracking_uninit)
			    {
			      status = find_src_status (in, set_src);

			      if (status == VAR_INIT_STATUS_UNKNOWN)
				status = find_src_status (out, set_src);
			    }

			  set_src = find_src_set_src (in, set_src);
			}

		      if (REG_P (uloc))
			var_reg_delete_and_set (out, uloc, !copied_p,
						status, set_src);
		      else if (MEM_P (uloc))
			var_mem_delete_and_set (out, uloc, !copied_p,
						status, set_src);
		    }
		}
	      else if (REG_P (uloc))
		var_regno_delete (out, REGNO (uloc));

	      val_store (out, val, vloc, insn, true);

	      if (reverse)
		val_store (out, XEXP (reverse, 0), XEXP (reverse, 1),
			   insn, false);
	    }
	    break;

	  case MO_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (REG_P (loc))
		var_reg_delete_and_set (out, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);
	      else if (MEM_P (loc))
		var_mem_delete_and_set (out, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);
	    }
	    break;

	  case MO_COPY:
	    {
	      rtx loc = mo->u.loc;
	      enum var_init_status src_status;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (! flag_var_tracking_uninit)
		src_status = VAR_INIT_STATUS_INITIALIZED;
	      else
		{
		  src_status = find_src_status (in, set_src);

		  if (src_status == VAR_INIT_STATUS_UNKNOWN)
		    src_status = find_src_status (out, set_src);
		}

	      set_src = find_src_set_src (in, set_src);

	      if (REG_P (loc))
		var_reg_delete_and_set (out, loc, false, src_status, set_src);
	      else if (MEM_P (loc))
		var_mem_delete_and_set (out, loc, false, src_status, set_src);
	    }
	    break;

	  case MO_USE_NO_VAR:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (out, loc, false);
	      else if (MEM_P (loc))
		var_mem_delete (out, loc, false);
	    }
	    break;

	  case MO_CLOBBER:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (out, loc, true);
	      else if (MEM_P (loc))
		var_mem_delete (out, loc, true);
	    }
	    break;

	  case MO_ADJUST:
	    out->stack_adjust += mo->u.adjust;
	    break;
	}
    }

  if (MAY_HAVE_DEBUG_INSNS)
    {
      dataflow_set_equiv_regs (out);
      htab_traverse (shared_hash_htab (out->vars), canonicalize_values_mark,
		     out);
      htab_traverse (shared_hash_htab (out->vars), canonicalize_values_star,
		     out);
#if ENABLE_CHECKING
      htab_traverse (shared_hash_htab (out->vars),
		     canonicalize_loc_order_check, out);
#endif
    }
  changed = dataflow_set_different (&old_out, out);
  dataflow_set_destroy (&old_out);
  return changed;
}

/* Find the locations of variables in the whole function.  */

static bool
vt_find_locations (void)
{
  fibheap_t worklist, pending, fibheap_swap;
  sbitmap visited, in_worklist, in_pending, sbitmap_swap;
  basic_block bb;
  edge e;
  int *bb_order;
  int *rc_order;
  int i;
  int htabsz = 0;
  int htabmax = PARAM_VALUE (PARAM_MAX_VARTRACK_SIZE);
  bool success = true;

  /* Compute reverse completion order of depth first search of the CFG
     so that the data-flow runs faster.  */
  rc_order = XNEWVEC (int, n_basic_blocks - NUM_FIXED_BLOCKS);
  bb_order = XNEWVEC (int, last_basic_block);
  pre_and_rev_post_order_compute (NULL, rc_order, false);
  for (i = 0; i < n_basic_blocks - NUM_FIXED_BLOCKS; i++)
    bb_order[rc_order[i]] = i;
  free (rc_order);

  worklist = fibheap_new ();
  pending = fibheap_new ();
  visited = sbitmap_alloc (last_basic_block);
  in_worklist = sbitmap_alloc (last_basic_block);
  in_pending = sbitmap_alloc (last_basic_block);
  sbitmap_zero (in_worklist);

  FOR_EACH_BB (bb)
    fibheap_insert (pending, bb_order[bb->index], bb);
  sbitmap_ones (in_pending);

  while (success && !fibheap_empty (pending))
    {
      fibheap_swap = pending;
      pending = worklist;
      worklist = fibheap_swap;
      sbitmap_swap = in_pending;
      in_pending = in_worklist;
      in_worklist = sbitmap_swap;

      sbitmap_zero (visited);

      while (!fibheap_empty (worklist))
	{
	  bb = (basic_block) fibheap_extract_min (worklist);
	  RESET_BIT (in_worklist, bb->index);
	  if (!TEST_BIT (visited, bb->index))
	    {
	      bool changed;
	      edge_iterator ei;
	      int oldinsz, oldoutsz;

	      SET_BIT (visited, bb->index);

	      if (VTI (bb)->in.vars)
		{
		  htabsz
		    -= (htab_size (shared_hash_htab (VTI (bb)->in.vars))
			+ htab_size (shared_hash_htab (VTI (bb)->out.vars)));
		  oldinsz
		    = htab_elements (shared_hash_htab (VTI (bb)->in.vars));
		  oldoutsz
		    = htab_elements (shared_hash_htab (VTI (bb)->out.vars));
		}
	      else
		oldinsz = oldoutsz = 0;

	      if (MAY_HAVE_DEBUG_INSNS)
		{
		  dataflow_set *in = &VTI (bb)->in, *first_out = NULL;
		  bool first = true, adjust = false;

		  /* Calculate the IN set as the intersection of
		     predecessor OUT sets.  */

		  dataflow_set_clear (in);
		  dst_can_be_shared = true;

		  FOR_EACH_EDGE (e, ei, bb->preds)
		    if (!VTI (e->src)->flooded)
		      gcc_assert (bb_order[bb->index]
				  <= bb_order[e->src->index]);
		    else if (first)
		      {
			dataflow_set_copy (in, &VTI (e->src)->out);
			first_out = &VTI (e->src)->out;
			first = false;
		      }
		    else
		      {
			dataflow_set_merge (in, &VTI (e->src)->out);
			adjust = true;
		      }

		  if (adjust)
		    {
		      dataflow_post_merge_adjust (in, &VTI (bb)->permp);
#if ENABLE_CHECKING
		      /* Merge and merge_adjust should keep entries in
			 canonical order.  */
		      htab_traverse (shared_hash_htab (in->vars),
				     canonicalize_loc_order_check,
				     in);
#endif
		      if (dst_can_be_shared)
			{
			  shared_hash_destroy (in->vars);
			  in->vars = shared_hash_copy (first_out->vars);
			}
		    }

		  VTI (bb)->flooded = true;
		}
	      else
		{
		  /* Calculate the IN set as union of predecessor OUT sets.  */
		  dataflow_set_clear (&VTI (bb)->in);
		  FOR_EACH_EDGE (e, ei, bb->preds)
		    dataflow_set_union (&VTI (bb)->in, &VTI (e->src)->out);
		}

	      changed = compute_bb_dataflow (bb);
	      htabsz += (htab_size (shared_hash_htab (VTI (bb)->in.vars))
			 + htab_size (shared_hash_htab (VTI (bb)->out.vars)));

	      if (htabmax && htabsz > htabmax)
		{
		  if (MAY_HAVE_DEBUG_INSNS)
		    inform (DECL_SOURCE_LOCATION (cfun->decl),
			    "variable tracking size limit exceeded with "
			    "-fvar-tracking-assignments, retrying without");
		  else
		    inform (DECL_SOURCE_LOCATION (cfun->decl),
			    "variable tracking size limit exceeded");
		  success = false;
		  break;
		}

	      if (changed)
		{
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    {
		      if (e->dest == EXIT_BLOCK_PTR)
			continue;

		      if (TEST_BIT (visited, e->dest->index))
			{
			  if (!TEST_BIT (in_pending, e->dest->index))
			    {
			      /* Send E->DEST to next round.  */
			      SET_BIT (in_pending, e->dest->index);
			      fibheap_insert (pending,
					      bb_order[e->dest->index],
					      e->dest);
			    }
			}
		      else if (!TEST_BIT (in_worklist, e->dest->index))
			{
			  /* Add E->DEST to current round.  */
			  SET_BIT (in_worklist, e->dest->index);
			  fibheap_insert (worklist, bb_order[e->dest->index],
					  e->dest);
			}
		    }
		}

	      if (dump_file)
		fprintf (dump_file,
			 "BB %i: in %i (was %i), out %i (was %i), rem %i + %i, tsz %i\n",
			 bb->index,
			 (int)htab_elements (shared_hash_htab (VTI (bb)->in.vars)),
			 oldinsz,
			 (int)htab_elements (shared_hash_htab (VTI (bb)->out.vars)),
			 oldoutsz,
			 (int)worklist->nodes, (int)pending->nodes, htabsz);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "BB %i IN:\n", bb->index);
		  dump_dataflow_set (&VTI (bb)->in);
		  fprintf (dump_file, "BB %i OUT:\n", bb->index);
		  dump_dataflow_set (&VTI (bb)->out);
		}
	    }
	}
    }

  if (success && MAY_HAVE_DEBUG_INSNS)
    FOR_EACH_BB (bb)
      gcc_assert (VTI (bb)->flooded);

  free (bb_order);
  fibheap_delete (worklist);
  fibheap_delete (pending);
  sbitmap_free (visited);
  sbitmap_free (in_worklist);
  sbitmap_free (in_pending);

  return success;
}

/* Print the content of the LIST to dump file.  */

static void
dump_attrs_list (attrs list)
{
  for (; list; list = list->next)
    {
      if (dv_is_decl_p (list->dv))
	print_mem_expr (dump_file, dv_as_decl (list->dv));
      else
	print_rtl_single (dump_file, dv_as_value (list->dv));
      fprintf (dump_file, "+" HOST_WIDE_INT_PRINT_DEC, list->offset);
    }
  fprintf (dump_file, "\n");
}

/* Print the information about variable *SLOT to dump file.  */

static int
dump_var_slot (void **slot, void *data ATTRIBUTE_UNUSED)
{
  variable var = (variable) *slot;

  dump_var (var);

  /* Continue traversing the hash table.  */
  return 1;
}

/* Print the information about variable VAR to dump file.  */

static void
dump_var (variable var)
{
  int i;
  location_chain node;

  if (dv_is_decl_p (var->dv))
    {
      const_tree decl = dv_as_decl (var->dv);

      if (DECL_NAME (decl))
	{
	  fprintf (dump_file, "  name: %s",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	  if (dump_flags & TDF_UID)
	    fprintf (dump_file, "D.%u", DECL_UID (decl));
	}
      else if (TREE_CODE (decl) == DEBUG_EXPR_DECL)
	fprintf (dump_file, "  name: D#%u", DEBUG_TEMP_UID (decl));
      else
	fprintf (dump_file, "  name: D.%u", DECL_UID (decl));
      fprintf (dump_file, "\n");
    }
  else
    {
      fputc (' ', dump_file);
      print_rtl_single (dump_file, dv_as_value (var->dv));
    }

  for (i = 0; i < var->n_var_parts; i++)
    {
      fprintf (dump_file, "    offset %ld\n",
	       (long) var->var_part[i].offset);
      for (node = var->var_part[i].loc_chain; node; node = node->next)
	{
	  fprintf (dump_file, "      ");
	  if (node->init == VAR_INIT_STATUS_UNINITIALIZED)
	    fprintf (dump_file, "[uninit]");
	  print_rtl_single (dump_file, node->loc);
	}
    }
}

/* Print the information about variables from hash table VARS to dump file.  */

static void
dump_vars (htab_t vars)
{
  if (htab_elements (vars) > 0)
    {
      fprintf (dump_file, "Variables:\n");
      htab_traverse (vars, dump_var_slot, NULL);
    }
}

/* Print the dataflow set SET to dump file.  */

static void
dump_dataflow_set (dataflow_set *set)
{
  int i;

  fprintf (dump_file, "Stack adjustment: " HOST_WIDE_INT_PRINT_DEC "\n",
	   set->stack_adjust);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (set->regs[i])
	{
	  fprintf (dump_file, "Reg %d:", i);
	  dump_attrs_list (set->regs[i]);
	}
    }
  dump_vars (shared_hash_htab (set->vars));
  fprintf (dump_file, "\n");
}

/* Print the IN and OUT sets for each basic block to dump file.  */

static void
dump_dataflow_sets (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      fprintf (dump_file, "\nBasic block %d:\n", bb->index);
      fprintf (dump_file, "IN:\n");
      dump_dataflow_set (&VTI (bb)->in);
      fprintf (dump_file, "OUT:\n");
      dump_dataflow_set (&VTI (bb)->out);
    }
}

/* Add variable VAR to the hash table of changed variables and
   if it has no locations delete it from SET's hash table.  */

static void
variable_was_changed (variable var, dataflow_set *set)
{
  hashval_t hash = dv_htab_hash (var->dv);

  if (emit_notes)
    {
      void **slot;
      bool old_cur_loc_changed = false;

      /* Remember this decl or VALUE has been added to changed_variables.  */
      set_dv_changed (var->dv, true);

      slot = htab_find_slot_with_hash (changed_variables,
				       var->dv,
				       hash, INSERT);

      if (*slot)
	{
	  variable old_var = (variable) *slot;
	  gcc_assert (old_var->in_changed_variables);
	  old_var->in_changed_variables = false;
	  old_cur_loc_changed = old_var->cur_loc_changed;
	  variable_htab_free (*slot);
	}
      if (set && var->n_var_parts == 0)
	{
	  variable empty_var;

	  empty_var = (variable) pool_alloc (dv_pool (var->dv));
	  empty_var->dv = var->dv;
	  empty_var->refcount = 1;
	  empty_var->n_var_parts = 0;
	  empty_var->cur_loc_changed = true;
	  empty_var->in_changed_variables = true;
	  *slot = empty_var;
	  goto drop_var;
	}
      else
	{
	  var->refcount++;
	  var->in_changed_variables = true;
	  /* If within processing one uop a variable is deleted
	     and then readded, we need to assume it has changed.  */
	  if (old_cur_loc_changed)
	    var->cur_loc_changed = true;
	  *slot = var;
	}
    }
  else
    {
      gcc_assert (set);
      if (var->n_var_parts == 0)
	{
	  void **slot;

	drop_var:
	  slot = shared_hash_find_slot_noinsert (set->vars, var->dv);
	  if (slot)
	    {
	      if (shared_hash_shared (set->vars))
		slot = shared_hash_find_slot_unshare (&set->vars, var->dv,
						      NO_INSERT);
	      htab_clear_slot (shared_hash_htab (set->vars), slot);
	    }
	}
    }
}

/* Look for the index in VAR->var_part corresponding to OFFSET.
   Return -1 if not found.  If INSERTION_POINT is non-NULL, the
   referenced int will be set to the index that the part has or should
   have, if it should be inserted.  */

static inline int
find_variable_location_part (variable var, HOST_WIDE_INT offset,
			     int *insertion_point)
{
  int pos, low, high;

  /* Find the location part.  */
  low = 0;
  high = var->n_var_parts;
  while (low != high)
    {
      pos = (low + high) / 2;
      if (var->var_part[pos].offset < offset)
	low = pos + 1;
      else
	high = pos;
    }
  pos = low;

  if (insertion_point)
    *insertion_point = pos;

  if (pos < var->n_var_parts && var->var_part[pos].offset == offset)
    return pos;

  return -1;
}

static void **
set_slot_part (dataflow_set *set, rtx loc, void **slot,
	       decl_or_value dv, HOST_WIDE_INT offset,
	       enum var_init_status initialized, rtx set_src)
{
  int pos;
  location_chain node, next;
  location_chain *nextp;
  variable var;
  bool onepart = dv_onepart_p (dv);

  gcc_assert (offset == 0 || !onepart);
  gcc_assert (loc != dv_as_opaque (dv));

  var = (variable) *slot;

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  if (!var)
    {
      /* Create new variable information.  */
      var = (variable) pool_alloc (dv_pool (dv));
      var->dv = dv;
      var->refcount = 1;
      var->n_var_parts = 1;
      var->cur_loc_changed = false;
      var->in_changed_variables = false;
      var->var_part[0].offset = offset;
      var->var_part[0].loc_chain = NULL;
      var->var_part[0].cur_loc = NULL;
      *slot = var;
      pos = 0;
      nextp = &var->var_part[0].loc_chain;
    }
  else if (onepart)
    {
      int r = -1, c = 0;

      gcc_assert (dv_as_opaque (var->dv) == dv_as_opaque (dv));

      pos = 0;

      if (GET_CODE (loc) == VALUE)
	{
	  for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	       nextp = &node->next)
	    if (GET_CODE (node->loc) == VALUE)
	      {
		if (node->loc == loc)
		  {
		    r = 0;
		    break;
		  }
		if (canon_value_cmp (node->loc, loc))
		  c++;
		else
		  {
		    r = 1;
		    break;
		  }
	      }
	    else if (REG_P (node->loc) || MEM_P (node->loc))
	      c++;
	    else
	      {
		r = 1;
		break;
	      }
	}
      else if (REG_P (loc))
	{
	  for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	       nextp = &node->next)
	    if (REG_P (node->loc))
	      {
		if (REGNO (node->loc) < REGNO (loc))
		  c++;
		else
		  {
		    if (REGNO (node->loc) == REGNO (loc))
		      r = 0;
		    else
		      r = 1;
		    break;
		  }
	      }
	    else
	      {
		r = 1;
		break;
	      }
	}
      else if (MEM_P (loc))
	{
	  for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	       nextp = &node->next)
	    if (REG_P (node->loc))
	      c++;
	    else if (MEM_P (node->loc))
	      {
		if ((r = loc_cmp (XEXP (node->loc, 0), XEXP (loc, 0))) >= 0)
		  break;
		else
		  c++;
	      }
	    else
	      {
		r = 1;
		break;
	      }
	}
      else
	for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	     nextp = &node->next)
	  if ((r = loc_cmp (node->loc, loc)) >= 0)
	    break;
	  else
	    c++;

      if (r == 0)
	return slot;

      if (shared_var_p (var, set->vars))
	{
	  slot = unshare_variable (set, slot, var, initialized);
	  var = (variable)*slot;
	  for (nextp = &var->var_part[0].loc_chain; c;
	       nextp = &(*nextp)->next)
	    c--;
	  gcc_assert ((!node && !*nextp) || node->loc == (*nextp)->loc);
	}
    }
  else
    {
      int inspos = 0;

      gcc_assert (dv_as_decl (var->dv) == dv_as_decl (dv));

      pos = find_variable_location_part (var, offset, &inspos);

      if (pos >= 0)
	{
	  node = var->var_part[pos].loc_chain;

	  if (node
	      && ((REG_P (node->loc) && REG_P (loc)
		   && REGNO (node->loc) == REGNO (loc))
		  || rtx_equal_p (node->loc, loc)))
	    {
	      /* LOC is in the beginning of the chain so we have nothing
		 to do.  */
	      if (node->init < initialized)
		node->init = initialized;
	      if (set_src != NULL)
		node->set_src = set_src;

	      return slot;
	    }
	  else
	    {
	      /* We have to make a copy of a shared variable.  */
	      if (shared_var_p (var, set->vars))
		{
		  slot = unshare_variable (set, slot, var, initialized);
		  var = (variable)*slot;
		}
	    }
	}
      else
	{
	  /* We have not found the location part, new one will be created.  */

	  /* We have to make a copy of the shared variable.  */
	  if (shared_var_p (var, set->vars))
	    {
	      slot = unshare_variable (set, slot, var, initialized);
	      var = (variable)*slot;
	    }

	  /* We track only variables whose size is <= MAX_VAR_PARTS bytes
	     thus there are at most MAX_VAR_PARTS different offsets.  */
	  gcc_assert (var->n_var_parts < MAX_VAR_PARTS
		      && (!var->n_var_parts || !dv_onepart_p (var->dv)));

	  /* We have to move the elements of array starting at index
	     inspos to the next position.  */
	  for (pos = var->n_var_parts; pos > inspos; pos--)
	    var->var_part[pos] = var->var_part[pos - 1];

	  var->n_var_parts++;
	  var->var_part[pos].offset = offset;
	  var->var_part[pos].loc_chain = NULL;
	  var->var_part[pos].cur_loc = NULL;
	}

      /* Delete the location from the list.  */
      nextp = &var->var_part[pos].loc_chain;
      for (node = var->var_part[pos].loc_chain; node; node = next)
	{
	  next = node->next;
	  if ((REG_P (node->loc) && REG_P (loc)
	       && REGNO (node->loc) == REGNO (loc))
	      || rtx_equal_p (node->loc, loc))
	    {
	      /* Save these values, to assign to the new node, before
		 deleting this one.  */
	      if (node->init > initialized)
		initialized = node->init;
	      if (node->set_src != NULL && set_src == NULL)
		set_src = node->set_src;
	      if (var->var_part[pos].cur_loc == node->loc)
		{
		  var->var_part[pos].cur_loc = NULL;
		  var->cur_loc_changed = true;
		}
	      pool_free (loc_chain_pool, node);
	      *nextp = next;
	      break;
	    }
	  else
	    nextp = &node->next;
	}

      nextp = &var->var_part[pos].loc_chain;
    }

  /* Add the location to the beginning.  */
  node = (location_chain) pool_alloc (loc_chain_pool);
  node->loc = loc;
  node->init = initialized;
  node->set_src = set_src;
  node->next = *nextp;
  *nextp = node;

  if (onepart && emit_notes)
    add_value_chains (var->dv, loc);

  /* If no location was emitted do so.  */
  if (var->var_part[pos].cur_loc == NULL)
    variable_was_changed (var, set);

  return slot;
}

/* Set the part of variable's location in the dataflow set SET.  The
   variable part is specified by variable's declaration in DV and
   offset OFFSET and the part's location by LOC.  IOPT should be
   NO_INSERT if the variable is known to be in SET already and the
   variable hash table must not be resized, and INSERT otherwise.  */

static void
set_variable_part (dataflow_set *set, rtx loc,
		   decl_or_value dv, HOST_WIDE_INT offset,
		   enum var_init_status initialized, rtx set_src,
		   enum insert_option iopt)
{
  void **slot;

  if (iopt == NO_INSERT)
    slot = shared_hash_find_slot_noinsert (set->vars, dv);
  else
    {
      slot = shared_hash_find_slot (set->vars, dv);
      if (!slot)
	slot = shared_hash_find_slot_unshare (&set->vars, dv, iopt);
    }
  slot = set_slot_part (set, loc, slot, dv, offset, initialized, set_src);
}

/* Remove all recorded register locations for the given variable part
   from dataflow set SET, except for those that are identical to loc.
   The variable part is specified by variable's declaration or value
   DV and offset OFFSET.  */

static void **
clobber_slot_part (dataflow_set *set, rtx loc, void **slot,
		   HOST_WIDE_INT offset, rtx set_src)
{
  variable var = (variable) *slot;
  int pos = find_variable_location_part (var, offset, NULL);

  if (pos >= 0)
    {
      location_chain node, next;

      /* Remove the register locations from the dataflow set.  */
      next = var->var_part[pos].loc_chain;
      for (node = next; node; node = next)
	{
	  next = node->next;
	  if (node->loc != loc
	      && (!flag_var_tracking_uninit
		  || !set_src
		  || MEM_P (set_src)
		  || !rtx_equal_p (set_src, node->set_src)))
	    {
	      if (REG_P (node->loc))
		{
		  attrs anode, anext;
		  attrs *anextp;

		  /* Remove the variable part from the register's
		     list, but preserve any other variable parts
		     that might be regarded as live in that same
		     register.  */
		  anextp = &set->regs[REGNO (node->loc)];
		  for (anode = *anextp; anode; anode = anext)
		    {
		      anext = anode->next;
		      if (dv_as_opaque (anode->dv) == dv_as_opaque (var->dv)
			  && anode->offset == offset)
			{
			  pool_free (attrs_pool, anode);
			  *anextp = anext;
			}
		      else
			anextp = &anode->next;
		    }
		}

	      slot = delete_slot_part (set, node->loc, slot, offset);
	    }
	}
    }

  return slot;
}

/* Remove all recorded register locations for the given variable part
   from dataflow set SET, except for those that are identical to loc.
   The variable part is specified by variable's declaration or value
   DV and offset OFFSET.  */

static void
clobber_variable_part (dataflow_set *set, rtx loc, decl_or_value dv,
		       HOST_WIDE_INT offset, rtx set_src)
{
  void **slot;

  if (!dv_as_opaque (dv)
      || (!dv_is_value_p (dv) && ! DECL_P (dv_as_decl (dv))))
    return;

  slot = shared_hash_find_slot_noinsert (set->vars, dv);
  if (!slot)
    return;

  slot = clobber_slot_part (set, loc, slot, offset, set_src);
}

/* Delete the part of variable's location from dataflow set SET.  The
   variable part is specified by its SET->vars slot SLOT and offset
   OFFSET and the part's location by LOC.  */

static void **
delete_slot_part (dataflow_set *set, rtx loc, void **slot,
		  HOST_WIDE_INT offset)
{
  variable var = (variable) *slot;
  int pos = find_variable_location_part (var, offset, NULL);

  if (pos >= 0)
    {
      location_chain node, next;
      location_chain *nextp;
      bool changed;

      if (shared_var_p (var, set->vars))
	{
	  /* If the variable contains the location part we have to
	     make a copy of the variable.  */
	  for (node = var->var_part[pos].loc_chain; node;
	       node = node->next)
	    {
	      if ((REG_P (node->loc) && REG_P (loc)
		   && REGNO (node->loc) == REGNO (loc))
		  || rtx_equal_p (node->loc, loc))
		{
		  slot = unshare_variable (set, slot, var,
					   VAR_INIT_STATUS_UNKNOWN);
		  var = (variable)*slot;
		  break;
		}
	    }
	}

      /* Delete the location part.  */
      changed = false;
      nextp = &var->var_part[pos].loc_chain;
      for (node = *nextp; node; node = next)
	{
	  next = node->next;
	  if ((REG_P (node->loc) && REG_P (loc)
	       && REGNO (node->loc) == REGNO (loc))
	      || rtx_equal_p (node->loc, loc))
	    {
	      if (emit_notes && pos == 0 && dv_onepart_p (var->dv))
		remove_value_chains (var->dv, node->loc);
	      /* If we have deleted the location which was last emitted
		 we have to emit new location so add the variable to set
		 of changed variables.  */
	      if (var->var_part[pos].cur_loc == node->loc)
		{
		  changed = true;
		  var->var_part[pos].cur_loc = NULL;
		  var->cur_loc_changed = true;
		}
	      pool_free (loc_chain_pool, node);
	      *nextp = next;
	      break;
	    }
	  else
	    nextp = &node->next;
	}

      if (var->var_part[pos].loc_chain == NULL)
	{
	  changed = true;
	  var->n_var_parts--;
	  if (emit_notes)
	    var->cur_loc_changed = true;
	  while (pos < var->n_var_parts)
	    {
	      var->var_part[pos] = var->var_part[pos + 1];
	      pos++;
	    }
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return slot;
}

/* Delete the part of variable's location from dataflow set SET.  The
   variable part is specified by variable's declaration or value DV
   and offset OFFSET and the part's location by LOC.  */

static void
delete_variable_part (dataflow_set *set, rtx loc, decl_or_value dv,
		      HOST_WIDE_INT offset)
{
  void **slot = shared_hash_find_slot_noinsert (set->vars, dv);
  if (!slot)
    return;

  slot = delete_slot_part (set, loc, slot, offset);
}

/* Structure for passing some other parameters to function
   vt_expand_loc_callback.  */
struct expand_loc_callback_data
{
  /* The variables and values active at this point.  */
  htab_t vars;

  /* True in vt_expand_loc_dummy calls, no rtl should be allocated.
     Non-NULL should be returned if vt_expand_loc would return
     non-NULL in that case, NULL otherwise.  cur_loc_changed should be
     computed and cur_loc recomputed when possible (but just once
     per emit_notes_for_changes call).  */
  bool dummy;

  /* True if expansion of subexpressions had to recompute some
     VALUE/DEBUG_EXPR_DECL's cur_loc or used a VALUE/DEBUG_EXPR_DECL
     whose cur_loc has been already recomputed during current
     emit_notes_for_changes call.  */
  bool cur_loc_changed;
};

/* Callback for cselib_expand_value, that looks for expressions
   holding the value in the var-tracking hash tables.  Return X for
   standard processing, anything else is to be used as-is.  */

static rtx
vt_expand_loc_callback (rtx x, bitmap regs, int max_depth, void *data)
{
  struct expand_loc_callback_data *elcd
    = (struct expand_loc_callback_data *) data;
  bool dummy = elcd->dummy;
  bool cur_loc_changed = elcd->cur_loc_changed;
  decl_or_value dv;
  variable var;
  location_chain loc;
  rtx result, subreg, xret;

  switch (GET_CODE (x))
    {
    case SUBREG:
      if (dummy)
	{
	  if (cselib_dummy_expand_value_rtx_cb (SUBREG_REG (x), regs,
						max_depth - 1,
						vt_expand_loc_callback, data))
	    return pc_rtx;
	  else
	    return NULL;
	}

      subreg = cselib_expand_value_rtx_cb (SUBREG_REG (x), regs,
					   max_depth - 1,
					   vt_expand_loc_callback, data);

      if (!subreg)
	return NULL;

      result = simplify_gen_subreg (GET_MODE (x), subreg,
				    GET_MODE (SUBREG_REG (x)),
				    SUBREG_BYTE (x));

      /* Invalid SUBREGs are ok in debug info.  ??? We could try
	 alternate expansions for the VALUE as well.  */
      if (!result)
	result = gen_rtx_raw_SUBREG (GET_MODE (x), subreg, SUBREG_BYTE (x));

      return result;

    case DEBUG_EXPR:
      dv = dv_from_decl (DEBUG_EXPR_TREE_DECL (x));
      xret = NULL;
      break;

    case VALUE:
      dv = dv_from_value (x);
      xret = x;
      break;

    default:
      return x;
    }

  if (VALUE_RECURSED_INTO (x))
    return NULL;

  var = (variable) htab_find_with_hash (elcd->vars, dv, dv_htab_hash (dv));

  if (!var)
    {
      if (dummy && dv_changed_p (dv))
	elcd->cur_loc_changed = true;
      return xret;
    }

  if (var->n_var_parts == 0)
    {
      if (dummy)
	elcd->cur_loc_changed = true;
      return xret;
    }

  gcc_assert (var->n_var_parts == 1);

  VALUE_RECURSED_INTO (x) = true;
  result = NULL;

  if (var->var_part[0].cur_loc)
    {
      if (dummy)
	{
	  if (cselib_dummy_expand_value_rtx_cb (var->var_part[0].cur_loc, regs,
						max_depth,
						vt_expand_loc_callback, data))
	    result = pc_rtx;
	}
      else
	result = cselib_expand_value_rtx_cb (var->var_part[0].cur_loc, regs,
					     max_depth,
					     vt_expand_loc_callback, data);
      if (result)
	set_dv_changed (dv, false);
    }
  if (!result && dv_changed_p (dv))
    {
      set_dv_changed (dv, false);
      for (loc = var->var_part[0].loc_chain; loc; loc = loc->next)
	if (loc->loc == var->var_part[0].cur_loc)
	  continue;
	else if (dummy)
	  {
	    elcd->cur_loc_changed = cur_loc_changed;
	    if (cselib_dummy_expand_value_rtx_cb (loc->loc, regs, max_depth,
						  vt_expand_loc_callback,
						  data))
	      {
		result = pc_rtx;
		break;
	      }
	    else
	      {
		result = cselib_expand_value_rtx_cb (loc->loc, regs, max_depth,
						     vt_expand_loc_callback,
						     data);
		if (result)
		  break;
	      }
	  }
      if (dummy && (result || var->var_part[0].cur_loc))
	var->cur_loc_changed = true;
      var->var_part[0].cur_loc = loc ? loc->loc : NULL_RTX;
    }
  if (dummy)
    {
      if (var->cur_loc_changed)
	elcd->cur_loc_changed = true;
      else if (!result && var->var_part[0].cur_loc == NULL_RTX)
	elcd->cur_loc_changed = cur_loc_changed;
    }

  VALUE_RECURSED_INTO (x) = false;
  if (result)
    return result;
  else
    return xret;
}

/* Expand VALUEs in LOC, using VARS as well as cselib's equivalence
   tables.  */

static rtx
vt_expand_loc (rtx loc, htab_t vars)
{
  struct expand_loc_callback_data data;

  if (!MAY_HAVE_DEBUG_INSNS)
    return loc;

  data.vars = vars;
  data.dummy = false;
  data.cur_loc_changed = false;
  loc = cselib_expand_value_rtx_cb (loc, scratch_regs, 5,
				    vt_expand_loc_callback, &data);

  if (loc && MEM_P (loc))
    loc = targetm.delegitimize_address (loc);
  return loc;
}

/* Like vt_expand_loc, but only return true/false (whether vt_expand_loc
   would succeed or not, without actually allocating new rtxes.  */

static bool
vt_expand_loc_dummy (rtx loc, htab_t vars, bool *pcur_loc_changed)
{
  struct expand_loc_callback_data data;
  bool ret;

  gcc_assert (MAY_HAVE_DEBUG_INSNS);
  data.vars = vars;
  data.dummy = true;
  data.cur_loc_changed = false;
  ret = cselib_dummy_expand_value_rtx_cb (loc, scratch_regs, 5,
					  vt_expand_loc_callback, &data);
  *pcur_loc_changed = data.cur_loc_changed;
  return ret;
}

#ifdef ENABLE_RTL_CHECKING
/* Used to verify that cur_loc_changed updating is safe.  */
static struct pointer_map_t *emitted_notes;
#endif

/* Emit the NOTE_INSN_VAR_LOCATION for variable *VARP.  DATA contains
   additional parameters: WHERE specifies whether the note shall be emitted
   before or after instruction INSN.  */

static int
emit_note_insn_var_location (void **varp, void *data)
{
  variable var = (variable) *varp;
  rtx insn = ((emit_note_data *)data)->insn;
  enum emit_note_where where = ((emit_note_data *)data)->where;
  htab_t vars = ((emit_note_data *)data)->vars;
  rtx note, note_vl;
  int i, j, n_var_parts;
  bool complete;
  enum var_init_status initialized = VAR_INIT_STATUS_UNINITIALIZED;
  HOST_WIDE_INT last_limit;
  tree type_size_unit;
  HOST_WIDE_INT offsets[MAX_VAR_PARTS];
  rtx loc[MAX_VAR_PARTS];
  tree decl;
  location_chain lc;

  if (dv_is_value_p (var->dv))
    goto value_or_debug_decl;

  decl = dv_as_decl (var->dv);

  if (TREE_CODE (decl) == DEBUG_EXPR_DECL)
    goto value_or_debug_decl;

  complete = true;
  last_limit = 0;
  n_var_parts = 0;
  if (!MAY_HAVE_DEBUG_INSNS)
    {
      for (i = 0; i < var->n_var_parts; i++)
	if (var->var_part[i].cur_loc == NULL && var->var_part[i].loc_chain)
	  {
	    var->var_part[i].cur_loc = var->var_part[i].loc_chain->loc;
	    var->cur_loc_changed = true;
	  }
      if (var->n_var_parts == 0)
	var->cur_loc_changed = true;
    }
#ifndef ENABLE_RTL_CHECKING
  if (!var->cur_loc_changed)
    goto clear;
#endif
  for (i = 0; i < var->n_var_parts; i++)
    {
      enum machine_mode mode, wider_mode;
      rtx loc2;

      if (last_limit < var->var_part[i].offset)
	{
	  complete = false;
	  break;
	}
      else if (last_limit > var->var_part[i].offset)
	continue;
      offsets[n_var_parts] = var->var_part[i].offset;
      if (!var->var_part[i].cur_loc)
	{
	  complete = false;
	  continue;
	}
      loc2 = vt_expand_loc (var->var_part[i].cur_loc, vars);
      if (!loc2)
	{
	  complete = false;
	  continue;
	}
      loc[n_var_parts] = loc2;
      mode = GET_MODE (var->var_part[i].cur_loc);
      if (mode == VOIDmode && dv_onepart_p (var->dv))
	mode = DECL_MODE (decl);
      for (lc = var->var_part[i].loc_chain; lc; lc = lc->next)
	if (var->var_part[i].cur_loc == lc->loc)
	  {
	    initialized = lc->init;
	    break;
	  }
      gcc_assert (lc);
      last_limit = offsets[n_var_parts] + GET_MODE_SIZE (mode);

      /* Attempt to merge adjacent registers or memory.  */
      wider_mode = GET_MODE_WIDER_MODE (mode);
      for (j = i + 1; j < var->n_var_parts; j++)
	if (last_limit <= var->var_part[j].offset)
	  break;
      if (j < var->n_var_parts
	  && wider_mode != VOIDmode
	  && var->var_part[j].cur_loc
	  && mode == GET_MODE (var->var_part[j].cur_loc)
	  && (REG_P (loc[n_var_parts]) || MEM_P (loc[n_var_parts]))
	  && last_limit == var->var_part[j].offset
	  && (loc2 = vt_expand_loc (var->var_part[j].cur_loc, vars))
	  && GET_CODE (loc[n_var_parts]) == GET_CODE (loc2))
	{
	  rtx new_loc = NULL;

	  if (REG_P (loc[n_var_parts])
	      && hard_regno_nregs[REGNO (loc[n_var_parts])][mode] * 2
		 == hard_regno_nregs[REGNO (loc[n_var_parts])][wider_mode]
	      && end_hard_regno (mode, REGNO (loc[n_var_parts]))
		 == REGNO (loc2))
	    {
	      if (! WORDS_BIG_ENDIAN && ! BYTES_BIG_ENDIAN)
		new_loc = simplify_subreg (wider_mode, loc[n_var_parts],
					   mode, 0);
	      else if (WORDS_BIG_ENDIAN && BYTES_BIG_ENDIAN)
		new_loc = simplify_subreg (wider_mode, loc2, mode, 0);
	      if (new_loc)
		{
		  if (!REG_P (new_loc)
		      || REGNO (new_loc) != REGNO (loc[n_var_parts]))
		    new_loc = NULL;
		  else
		    REG_ATTRS (new_loc) = REG_ATTRS (loc[n_var_parts]);
		}
	    }
	  else if (MEM_P (loc[n_var_parts])
		   && GET_CODE (XEXP (loc2, 0)) == PLUS
		   && REG_P (XEXP (XEXP (loc2, 0), 0))
		   && CONST_INT_P (XEXP (XEXP (loc2, 0), 1)))
	    {
	      if ((REG_P (XEXP (loc[n_var_parts], 0))
		   && rtx_equal_p (XEXP (loc[n_var_parts], 0),
				   XEXP (XEXP (loc2, 0), 0))
		   && INTVAL (XEXP (XEXP (loc2, 0), 1))
		      == GET_MODE_SIZE (mode))
		  || (GET_CODE (XEXP (loc[n_var_parts], 0)) == PLUS
		      && CONST_INT_P (XEXP (XEXP (loc[n_var_parts], 0), 1))
		      && rtx_equal_p (XEXP (XEXP (loc[n_var_parts], 0), 0),
				      XEXP (XEXP (loc2, 0), 0))
		      && INTVAL (XEXP (XEXP (loc[n_var_parts], 0), 1))
			 + GET_MODE_SIZE (mode)
			 == INTVAL (XEXP (XEXP (loc2, 0), 1))))
		new_loc = adjust_address_nv (loc[n_var_parts],
					     wider_mode, 0);
	    }

	  if (new_loc)
	    {
	      loc[n_var_parts] = new_loc;
	      mode = wider_mode;
	      last_limit = offsets[n_var_parts] + GET_MODE_SIZE (mode);
	      i = j;
	    }
	}
      ++n_var_parts;
    }
  type_size_unit = TYPE_SIZE_UNIT (TREE_TYPE (decl));
  if ((unsigned HOST_WIDE_INT) last_limit < TREE_INT_CST_LOW (type_size_unit))
    complete = false;

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  note_vl = NULL_RTX;
  if (!complete)
    note_vl = gen_rtx_VAR_LOCATION (VOIDmode, decl, NULL_RTX,
				    (int) initialized);
  else if (n_var_parts == 1)
    {
      rtx expr_list
	= gen_rtx_EXPR_LIST (VOIDmode, loc[0], GEN_INT (offsets[0]));

      note_vl = gen_rtx_VAR_LOCATION (VOIDmode, decl, expr_list,
				      (int) initialized);
    }
  else if (n_var_parts)
    {
      rtx parallel;

      for (i = 0; i < n_var_parts; i++)
	loc[i]
	  = gen_rtx_EXPR_LIST (VOIDmode, loc[i], GEN_INT (offsets[i]));

      parallel = gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec_v (n_var_parts, loc));
      note_vl = gen_rtx_VAR_LOCATION (VOIDmode, decl,
				      parallel, (int) initialized);
    }

#ifdef ENABLE_RTL_CHECKING
  if (note_vl)
    {
      void **note_slot = pointer_map_insert (emitted_notes, decl);
      rtx pnote = (rtx) *note_slot;
      if (!var->cur_loc_changed && (pnote || PAT_VAR_LOCATION_LOC (note_vl)))
	{
	  gcc_assert (pnote);
	  gcc_assert (rtx_equal_p (PAT_VAR_LOCATION_LOC (pnote),
				   PAT_VAR_LOCATION_LOC (note_vl)));
	}
      *note_slot = (void *) note_vl;
    }
  if (!var->cur_loc_changed)
    goto clear;
#endif

  if (where != EMIT_NOTE_BEFORE_INSN)
    {
      note = emit_note_after (NOTE_INSN_VAR_LOCATION, insn);
      if (where == EMIT_NOTE_AFTER_CALL_INSN)
	NOTE_DURING_CALL_P (note) = true;
    }
  else
    note = emit_note_before (NOTE_INSN_VAR_LOCATION, insn);
  NOTE_VAR_LOCATION (note) = note_vl;

 clear:
  set_dv_changed (var->dv, false);
  var->cur_loc_changed = false;
  gcc_assert (var->in_changed_variables);
  var->in_changed_variables = false;
  htab_clear_slot (changed_variables, varp);

  /* Continue traversing the hash table.  */
  return 1;

 value_or_debug_decl:
  if (dv_changed_p (var->dv) && var->n_var_parts)
    {
      location_chain lc;
      bool cur_loc_changed;

      if (var->var_part[0].cur_loc
	  && vt_expand_loc_dummy (var->var_part[0].cur_loc, vars,
				  &cur_loc_changed))
	goto clear;
      for (lc = var->var_part[0].loc_chain; lc; lc = lc->next)
	if (lc->loc != var->var_part[0].cur_loc
	    && vt_expand_loc_dummy (lc->loc, vars, &cur_loc_changed))
	  break;
      var->var_part[0].cur_loc = lc ? lc->loc : NULL_RTX;
    }
  goto clear;
}

DEF_VEC_P (variable);
DEF_VEC_ALLOC_P (variable, heap);

/* Stack of variable_def pointers that need processing with
   check_changed_vars_2.  */

static VEC (variable, heap) *changed_variables_stack;

/* VALUEs with no variables that need set_dv_changed (val, false)
   called before check_changed_vars_3.  */

static VEC (rtx, heap) *changed_values_stack;

/* Helper function for check_changed_vars_1 and check_changed_vars_2.  */

static void
check_changed_vars_0 (decl_or_value dv, htab_t htab)
{
  value_chain vc
    = (value_chain) htab_find_with_hash (value_chains, dv, dv_htab_hash (dv));

  if (vc == NULL)
    return;
  for (vc = vc->next; vc; vc = vc->next)
    if (!dv_changed_p (vc->dv))
      {
	variable vcvar
	  = (variable) htab_find_with_hash (htab, vc->dv,
					    dv_htab_hash (vc->dv));
	if (vcvar)
	  {
	    set_dv_changed (vc->dv, true);
	    VEC_safe_push (variable, heap, changed_variables_stack, vcvar);
	  }
	else if (dv_is_value_p (vc->dv))
	  {
	    set_dv_changed (vc->dv, true);
	    VEC_safe_push (rtx, heap, changed_values_stack,
			   dv_as_value (vc->dv));
	    check_changed_vars_0 (vc->dv, htab);
	  }
      }
}

/* Populate changed_variables_stack with variable_def pointers
   that need variable_was_changed called on them.  */

static int
check_changed_vars_1 (void **slot, void *data)
{
  variable var = (variable) *slot;
  htab_t htab = (htab_t) data;

  if (dv_is_value_p (var->dv)
      || TREE_CODE (dv_as_decl (var->dv)) == DEBUG_EXPR_DECL)
    check_changed_vars_0 (var->dv, htab);
  return 1;
}

/* Add VAR to changed_variables and also for VALUEs add recursively
   all DVs that aren't in changed_variables yet but reference the
   VALUE from its loc_chain.  */

static void
check_changed_vars_2 (variable var, htab_t htab)
{
  variable_was_changed (var, NULL);
  if (dv_is_value_p (var->dv)
      || TREE_CODE (dv_as_decl (var->dv)) == DEBUG_EXPR_DECL)
    check_changed_vars_0 (var->dv, htab);
}

/* For each changed decl (except DEBUG_EXPR_DECLs) recompute
   cur_loc if needed (and cur_loc of all VALUEs and DEBUG_EXPR_DECLs
   it needs and are also in changed variables) and track whether
   cur_loc (or anything it uses to compute location) had to change
   during the current emit_notes_for_changes call.  */

static int
check_changed_vars_3 (void **slot, void *data)
{
  variable var = (variable) *slot;
  htab_t vars = (htab_t) data;
  int i;
  location_chain lc;
  bool cur_loc_changed;

  if (dv_is_value_p (var->dv)
      || TREE_CODE (dv_as_decl (var->dv)) == DEBUG_EXPR_DECL)
    return 1;

  for (i = 0; i < var->n_var_parts; i++)
    {
      if (var->var_part[i].cur_loc
	  && vt_expand_loc_dummy (var->var_part[i].cur_loc, vars,
				  &cur_loc_changed))
	{
	  if (cur_loc_changed)
	    var->cur_loc_changed = true;
	  continue;
	}
      for (lc = var->var_part[i].loc_chain; lc; lc = lc->next)
	if (lc->loc != var->var_part[i].cur_loc
	    && vt_expand_loc_dummy (lc->loc, vars, &cur_loc_changed))
	  break;
      if (lc || var->var_part[i].cur_loc)
	var->cur_loc_changed = true;
      var->var_part[i].cur_loc = lc ? lc->loc : NULL_RTX;
    }
  if (var->n_var_parts == 0)
    var->cur_loc_changed = true;
  return 1;
}

/* Emit NOTE_INSN_VAR_LOCATION note for each variable from a chain
   CHANGED_VARIABLES and delete this chain.  WHERE specifies whether the notes
   shall be emitted before of after instruction INSN.  */

static void
emit_notes_for_changes (rtx insn, enum emit_note_where where,
			shared_hash vars)
{
  emit_note_data data;
  htab_t htab = shared_hash_htab (vars);

  if (!htab_elements (changed_variables))
    return;

  if (MAY_HAVE_DEBUG_INSNS)
    {
      /* Unfortunately this has to be done in two steps, because
	 we can't traverse a hashtab into which we are inserting
	 through variable_was_changed.  */
      htab_traverse (changed_variables, check_changed_vars_1, htab);
      while (VEC_length (variable, changed_variables_stack) > 0)
	check_changed_vars_2 (VEC_pop (variable, changed_variables_stack),
			      htab);
      while (VEC_length (rtx, changed_values_stack) > 0)
	set_dv_changed (dv_from_value (VEC_pop (rtx, changed_values_stack)),
			false);
      htab_traverse (changed_variables, check_changed_vars_3, htab);
    }

  data.insn = insn;
  data.where = where;
  data.vars = htab;

  htab_traverse (changed_variables, emit_note_insn_var_location, &data);
}

/* Add variable *SLOT to the chain CHANGED_VARIABLES if it differs from the
   same variable in hash table DATA or is not there at all.  */

static int
emit_notes_for_differences_1 (void **slot, void *data)
{
  htab_t new_vars = (htab_t) data;
  variable old_var, new_var;

  old_var = (variable) *slot;
  new_var = (variable) htab_find_with_hash (new_vars, old_var->dv,
					    dv_htab_hash (old_var->dv));

  if (!new_var)
    {
      /* Variable has disappeared.  */
      variable empty_var;

      empty_var = (variable) pool_alloc (dv_pool (old_var->dv));
      empty_var->dv = old_var->dv;
      empty_var->refcount = 0;
      empty_var->n_var_parts = 0;
      empty_var->cur_loc_changed = false;
      empty_var->in_changed_variables = false;
      if (dv_onepart_p (old_var->dv))
	{
	  location_chain lc;

	  gcc_assert (old_var->n_var_parts == 1);
	  for (lc = old_var->var_part[0].loc_chain; lc; lc = lc->next)
	    remove_value_chains (old_var->dv, lc->loc);
	}
      variable_was_changed (empty_var, NULL);
      /* Continue traversing the hash table.  */
      return 1;
    }
  if (variable_different_p (old_var, new_var))
    {
      if (dv_onepart_p (old_var->dv))
	{
	  location_chain lc1, lc2;

	  gcc_assert (old_var->n_var_parts == 1);
	  gcc_assert (new_var->n_var_parts == 1);
	  lc1 = old_var->var_part[0].loc_chain;
	  lc2 = new_var->var_part[0].loc_chain;
	  while (lc1
		 && lc2
		 && ((REG_P (lc1->loc) && REG_P (lc2->loc))
		     || rtx_equal_p (lc1->loc, lc2->loc)))
	    {
	      lc1 = lc1->next;
	      lc2 = lc2->next;
	    }
	  for (; lc2; lc2 = lc2->next)
	    add_value_chains (old_var->dv, lc2->loc);
	  for (; lc1; lc1 = lc1->next)
	    remove_value_chains (old_var->dv, lc1->loc);
	}
      variable_was_changed (new_var, NULL);
    }
  /* Update cur_loc.  */
  if (old_var != new_var)
    {
      int i;
      for (i = 0; i < new_var->n_var_parts; i++)
	{
	  new_var->var_part[i].cur_loc = NULL;
	  if (old_var->n_var_parts != new_var->n_var_parts
	      || old_var->var_part[i].offset != new_var->var_part[i].offset)
	    new_var->cur_loc_changed = true;
	  else if (old_var->var_part[i].cur_loc != NULL)
	    {
	      location_chain lc;
	      rtx cur_loc = old_var->var_part[i].cur_loc;

	      for (lc = new_var->var_part[i].loc_chain; lc; lc = lc->next)
		if (lc->loc == cur_loc
		    || rtx_equal_p (cur_loc, lc->loc))
		  {
		    new_var->var_part[i].cur_loc = lc->loc;
		    break;
		  }
	      if (lc == NULL)
		new_var->cur_loc_changed = true;
	    }
	}
    }

  /* Continue traversing the hash table.  */
  return 1;
}

/* Add variable *SLOT to the chain CHANGED_VARIABLES if it is not in hash
   table DATA.  */

static int
emit_notes_for_differences_2 (void **slot, void *data)
{
  htab_t old_vars = (htab_t) data;
  variable old_var, new_var;

  new_var = (variable) *slot;
  old_var = (variable) htab_find_with_hash (old_vars, new_var->dv,
					    dv_htab_hash (new_var->dv));
  if (!old_var)
    {
      int i;
      /* Variable has appeared.  */
      if (dv_onepart_p (new_var->dv))
	{
	  location_chain lc;

	  gcc_assert (new_var->n_var_parts == 1);
	  for (lc = new_var->var_part[0].loc_chain; lc; lc = lc->next)
	    add_value_chains (new_var->dv, lc->loc);
	}
      for (i = 0; i < new_var->n_var_parts; i++)
	new_var->var_part[i].cur_loc = NULL;
      variable_was_changed (new_var, NULL);
    }

  /* Continue traversing the hash table.  */
  return 1;
}

/* Emit notes before INSN for differences between dataflow sets OLD_SET and
   NEW_SET.  */

static void
emit_notes_for_differences (rtx insn, dataflow_set *old_set,
			    dataflow_set *new_set)
{
  htab_traverse (shared_hash_htab (old_set->vars),
		 emit_notes_for_differences_1,
		 shared_hash_htab (new_set->vars));
  htab_traverse (shared_hash_htab (new_set->vars),
		 emit_notes_for_differences_2,
		 shared_hash_htab (old_set->vars));
  emit_notes_for_changes (insn, EMIT_NOTE_BEFORE_INSN, new_set->vars);
}

/* Emit the notes for changes of location parts in the basic block BB.  */

static void
emit_notes_in_bb (basic_block bb, dataflow_set *set)
{
  unsigned int i;
  micro_operation *mo;

  dataflow_set_clear (set);
  dataflow_set_copy (set, &VTI (bb)->in);

  for (i = 0; VEC_iterate (micro_operation, VTI (bb)->mos, i, mo); i++)
    {
      rtx insn = mo->insn;

      switch (mo->type)
	{
	  case MO_CALL:
	    dataflow_set_clear_at_call (set);
	    emit_notes_for_changes (insn, EMIT_NOTE_AFTER_CALL_INSN, set->vars);
	    break;

	  case MO_USE:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_set (set, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	      else
		var_mem_set (set, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN, set->vars);
	    }
	    break;

	  case MO_VAL_LOC:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc;
	      tree var;

	      if (GET_CODE (loc) == CONCAT)
		{
		  val = XEXP (loc, 0);
		  vloc = XEXP (loc, 1);
		}
	      else
		{
		  val = NULL_RTX;
		  vloc = loc;
		}

	      var = PAT_VAR_LOCATION_DECL (vloc);

	      clobber_variable_part (set, NULL_RTX,
				     dv_from_decl (var), 0, NULL_RTX);
	      if (val)
		{
		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (set, val, PAT_VAR_LOCATION_LOC (vloc), insn);
		  set_variable_part (set, val, dv_from_decl (var), 0,
				     VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				     INSERT);
		}
	      else if (!VAR_LOC_UNKNOWN_P (PAT_VAR_LOCATION_LOC (vloc)))
		set_variable_part (set, PAT_VAR_LOCATION_LOC (vloc),
				   dv_from_decl (var), 0,
				   VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				   INSERT);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN, set->vars);
	    }
	    break;

	  case MO_VAL_USE:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc;

	      vloc = uloc = XEXP (loc, 1);
	      val = XEXP (loc, 0);

	      if (GET_CODE (val) == CONCAT)
		{
		  uloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (VAL_NEEDS_RESOLUTION (loc))
		val_resolve (set, val, vloc, insn);
	      else
		val_store (set, val, uloc, insn, false);

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (GET_CODE (uloc) == REG)
		    var_reg_set (set, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		  else if (GET_CODE (uloc) == MEM)
		    var_mem_set (set, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		}

	      emit_notes_for_changes (insn, EMIT_NOTE_BEFORE_INSN, set->vars);
	    }
	    break;

	  case MO_VAL_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc, reverse = NULL_RTX;

	      vloc = loc;
	      if (VAL_EXPR_HAS_REVERSE (loc))
		{
		  reverse = XEXP (loc, 1);
		  vloc = XEXP (loc, 0);
		}
	      uloc = XEXP (vloc, 1);
	      val = XEXP (vloc, 0);
	      vloc = uloc;

	      if (GET_CODE (val) == CONCAT)
		{
		  vloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (GET_CODE (vloc) == SET)
		{
		  rtx vsrc = SET_SRC (vloc);

		  gcc_assert (val != vsrc);
		  gcc_assert (vloc == uloc || VAL_NEEDS_RESOLUTION (loc));

		  vloc = SET_DEST (vloc);

		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (set, val, vsrc, insn);
		}
	      else if (VAL_NEEDS_RESOLUTION (loc))
		{
		  gcc_assert (GET_CODE (uloc) == SET
			      && GET_CODE (SET_SRC (uloc)) == REG);
		  val_resolve (set, val, SET_SRC (uloc), insn);
		}

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (VAL_EXPR_IS_CLOBBERED (loc))
		    {
		      if (REG_P (uloc))
			var_reg_delete (set, uloc, true);
		      else if (MEM_P (uloc))
			var_mem_delete (set, uloc, true);
		    }
		  else
		    {
		      bool copied_p = VAL_EXPR_IS_COPIED (loc);
		      rtx set_src = NULL;
		      enum var_init_status status = VAR_INIT_STATUS_INITIALIZED;

		      if (GET_CODE (uloc) == SET)
			{
			  set_src = SET_SRC (uloc);
			  uloc = SET_DEST (uloc);
			}

		      if (copied_p)
			{
			  status = find_src_status (set, set_src);

			  set_src = find_src_set_src (set, set_src);
			}

		      if (REG_P (uloc))
			var_reg_delete_and_set (set, uloc, !copied_p,
						status, set_src);
		      else if (MEM_P (uloc))
			var_mem_delete_and_set (set, uloc, !copied_p,
						status, set_src);
		    }
		}
	      else if (REG_P (uloc))
		var_regno_delete (set, REGNO (uloc));

	      val_store (set, val, vloc, insn, true);

	      if (reverse)
		val_store (set, XEXP (reverse, 0), XEXP (reverse, 1),
			   insn, false);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (REG_P (loc))
		var_reg_delete_and_set (set, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);
	      else
		var_mem_delete_and_set (set, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_COPY:
	    {
	      rtx loc = mo->u.loc;
	      enum var_init_status src_status;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      src_status = find_src_status (set, set_src);
	      set_src = find_src_set_src (set, set_src);

	      if (REG_P (loc))
		var_reg_delete_and_set (set, loc, false, src_status, set_src);
	      else
		var_mem_delete_and_set (set, loc, false, src_status, set_src);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_USE_NO_VAR:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (set, loc, false);
	      else
		var_mem_delete (set, loc, false);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN, set->vars);
	    }
	    break;

	  case MO_CLOBBER:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (set, loc, true);
	      else
		var_mem_delete (set, loc, true);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_ADJUST:
	    set->stack_adjust += mo->u.adjust;
	    break;
	}
    }
}

/* Emit notes for the whole function.  */

static void
vt_emit_notes (void)
{
  basic_block bb;
  dataflow_set cur;

#ifdef ENABLE_RTL_CHECKING
  emitted_notes = pointer_map_create ();
#endif
  gcc_assert (!htab_elements (changed_variables));

  /* Free memory occupied by the out hash tables, as they aren't used
     anymore.  */
  FOR_EACH_BB (bb)
    dataflow_set_clear (&VTI (bb)->out);

  /* Enable emitting notes by functions (mainly by set_variable_part and
     delete_variable_part).  */
  emit_notes = true;

  if (MAY_HAVE_DEBUG_INSNS)
    {
      unsigned int i;
      rtx val;

      for (i = 0; VEC_iterate (rtx, preserved_values, i, val); i++)
	add_cselib_value_chains (dv_from_value (val));
      changed_variables_stack = VEC_alloc (variable, heap, 40);
      changed_values_stack = VEC_alloc (rtx, heap, 40);
    }

  dataflow_set_init (&cur);

  FOR_EACH_BB (bb)
    {
      /* Emit the notes for changes of variable locations between two
	 subsequent basic blocks.  */
      emit_notes_for_differences (BB_HEAD (bb), &cur, &VTI (bb)->in);

      /* Emit the notes for the changes in the basic block itself.  */
      emit_notes_in_bb (bb, &cur);

      /* Free memory occupied by the in hash table, we won't need it
	 again.  */
      dataflow_set_clear (&VTI (bb)->in);
    }
#ifdef ENABLE_CHECKING
  htab_traverse (shared_hash_htab (cur.vars),
		 emit_notes_for_differences_1,
		 shared_hash_htab (empty_shared_hash));
  if (MAY_HAVE_DEBUG_INSNS)
    {
      unsigned int i;
      rtx val;

      for (i = 0; VEC_iterate (rtx, preserved_values, i, val); i++)
	remove_cselib_value_chains (dv_from_value (val));
      gcc_assert (htab_elements (value_chains) == 0);
    }
#endif
  dataflow_set_destroy (&cur);

  if (MAY_HAVE_DEBUG_INSNS)
    {
      VEC_free (variable, heap, changed_variables_stack);
      VEC_free (rtx, heap, changed_values_stack);
    }

#ifdef ENABLE_RTL_CHECKING
  pointer_map_destroy (emitted_notes);
#endif
  emit_notes = false;
}

/* If there is a declaration and offset associated with register/memory RTL
   assign declaration to *DECLP and offset to *OFFSETP, and return true.  */

static bool
vt_get_decl_and_offset (rtx rtl, tree *declp, HOST_WIDE_INT *offsetp)
{
  if (REG_P (rtl))
    {
      if (REG_ATTRS (rtl))
	{
	  *declp = REG_EXPR (rtl);
	  *offsetp = REG_OFFSET (rtl);
	  return true;
	}
    }
  else if (MEM_P (rtl))
    {
      if (MEM_ATTRS (rtl))
	{
	  *declp = MEM_EXPR (rtl);
	  *offsetp = INT_MEM_OFFSET (rtl);
	  return true;
	}
    }
  return false;
}

/* Insert function parameters to IN and OUT sets of ENTRY_BLOCK.  */

static void
vt_add_function_parameters (void)
{
  tree parm;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm; parm = TREE_CHAIN (parm))
    {
      rtx decl_rtl = DECL_RTL_IF_SET (parm);
      rtx incoming = DECL_INCOMING_RTL (parm);
      tree decl;
      enum machine_mode mode;
      HOST_WIDE_INT offset;
      dataflow_set *out;
      decl_or_value dv;

      if (TREE_CODE (parm) != PARM_DECL)
	continue;

      if (!DECL_NAME (parm))
	continue;

      if (!decl_rtl || !incoming)
	continue;

      if (GET_MODE (decl_rtl) == BLKmode || GET_MODE (incoming) == BLKmode)
	continue;

      if (!vt_get_decl_and_offset (incoming, &decl, &offset))
	{
	  if (REG_P (incoming) || MEM_P (incoming))
	    {
	      /* This means argument is passed by invisible reference.  */
	      offset = 0;
	      decl = parm;
	      incoming = gen_rtx_MEM (GET_MODE (decl_rtl), incoming);
	    }
	  else
	    {
	      if (!vt_get_decl_and_offset (decl_rtl, &decl, &offset))
		continue;
	      offset += byte_lowpart_offset (GET_MODE (incoming),
					     GET_MODE (decl_rtl));
	    }
	}

      if (!decl)
	continue;

      if (parm != decl)
	{
	  /* Assume that DECL_RTL was a pseudo that got spilled to
	     memory.  The spill slot sharing code will force the
	     memory to reference spill_slot_decl (%sfp), so we don't
	     match above.  That's ok, the pseudo must have referenced
	     the entire parameter, so just reset OFFSET.  */
	  gcc_assert (decl == get_spill_slot_decl (false));
	  offset = 0;
	}

      if (!track_loc_p (incoming, parm, offset, false, &mode, &offset))
	continue;

      out = &VTI (ENTRY_BLOCK_PTR)->out;

      dv = dv_from_decl (parm);

      if (target_for_debug_bind (parm)
	  /* We can't deal with these right now, because this kind of
	     variable is single-part.  ??? We could handle parallels
	     that describe multiple locations for the same single
	     value, but ATM we don't.  */
	  && GET_CODE (incoming) != PARALLEL)
	{
	  cselib_val *val;

	  /* ??? We shouldn't ever hit this, but it may happen because
	     arguments passed by invisible reference aren't dealt with
	     above: incoming-rtl will have Pmode rather than the
	     expected mode for the type.  */
	  if (offset)
	    continue;

	  val = cselib_lookup (var_lowpart (mode, incoming), mode, true);

	  /* ??? Float-typed values in memory are not handled by
	     cselib.  */
	  if (val)
	    {
	      preserve_value (val);
	      set_variable_part (out, val->val_rtx, dv, offset,
				 VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
	      dv = dv_from_value (val->val_rtx);
	    }
	}

      if (REG_P (incoming))
	{
	  incoming = var_lowpart (mode, incoming);
	  gcc_assert (REGNO (incoming) < FIRST_PSEUDO_REGISTER);
	  attrs_list_insert (&out->regs[REGNO (incoming)], dv, offset,
			     incoming);
	  set_variable_part (out, incoming, dv, offset,
			     VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
	}
      else if (MEM_P (incoming))
	{
	  incoming = var_lowpart (mode, incoming);
	  set_variable_part (out, incoming, dv, offset,
			     VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
	}
    }

  if (MAY_HAVE_DEBUG_INSNS)
    {
      cselib_preserve_only_values ();
      cselib_reset_table (cselib_get_next_uid ());
    }

}

/* Return true if INSN in the prologue initializes hard_frame_pointer_rtx.  */

static bool
fp_setter (rtx insn)
{
  rtx pat = PATTERN (insn);
  if (RTX_FRAME_RELATED_P (insn))
    {
      rtx expr = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);
      if (expr)
	pat = XEXP (expr, 0);
    }
  if (GET_CODE (pat) == SET)
    return SET_DEST (pat) == hard_frame_pointer_rtx;
  else if (GET_CODE (pat) == PARALLEL)
    {
      int i;
      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
	if (GET_CODE (XVECEXP (pat, 0, i)) == SET
	    && SET_DEST (XVECEXP (pat, 0, i)) == hard_frame_pointer_rtx)
	  return true;
    }
  return false;
}

/* Initialize cfa_base_rtx, create a preserved VALUE for it and
   ensure it isn't flushed during cselib_reset_table.
   Can be called only if frame_pointer_rtx resp. arg_pointer_rtx
   has been eliminated.  */

static void
vt_init_cfa_base (void)
{
  cselib_val *val;

#ifdef FRAME_POINTER_CFA_OFFSET
  cfa_base_rtx = frame_pointer_rtx;
#else
  cfa_base_rtx = arg_pointer_rtx;
#endif
  if (cfa_base_rtx == hard_frame_pointer_rtx
      || !fixed_regs[REGNO (cfa_base_rtx)])
    {
      cfa_base_rtx = NULL_RTX;
      return;
    }
  if (!MAY_HAVE_DEBUG_INSNS)
    return;

  val = cselib_lookup (cfa_base_rtx, GET_MODE (cfa_base_rtx), 1);
  preserve_value (val);
  cselib_preserve_cfa_base_value (val);
  val->locs->setting_insn = get_insns ();
  var_reg_decl_set (&VTI (ENTRY_BLOCK_PTR)->out, cfa_base_rtx,
		    VAR_INIT_STATUS_INITIALIZED, dv_from_value (val->val_rtx),
		    0, NULL_RTX, INSERT);
}

/* Allocate and initialize the data structures for variable tracking
   and parse the RTL to get the micro operations.  */

static bool
vt_initialize (void)
{
  basic_block bb, prologue_bb = NULL;
  HOST_WIDE_INT fp_cfa_offset = -1;

  alloc_aux_for_blocks (sizeof (struct variable_tracking_info_def));

  attrs_pool = create_alloc_pool ("attrs_def pool",
				  sizeof (struct attrs_def), 1024);
  var_pool = create_alloc_pool ("variable_def pool",
				sizeof (struct variable_def)
				+ (MAX_VAR_PARTS - 1)
				* sizeof (((variable)NULL)->var_part[0]), 64);
  loc_chain_pool = create_alloc_pool ("location_chain_def pool",
				      sizeof (struct location_chain_def),
				      1024);
  shared_hash_pool = create_alloc_pool ("shared_hash_def pool",
					sizeof (struct shared_hash_def), 256);
  empty_shared_hash = (shared_hash) pool_alloc (shared_hash_pool);
  empty_shared_hash->refcount = 1;
  empty_shared_hash->htab
    = htab_create (1, variable_htab_hash, variable_htab_eq,
		   variable_htab_free);
  changed_variables = htab_create (10, variable_htab_hash, variable_htab_eq,
				   variable_htab_free);
  if (MAY_HAVE_DEBUG_INSNS)
    {
      value_chain_pool = create_alloc_pool ("value_chain_def pool",
					    sizeof (struct value_chain_def),
					    1024);
      value_chains = htab_create (32, value_chain_htab_hash,
				  value_chain_htab_eq, NULL);
    }

  /* Init the IN and OUT sets.  */
  FOR_ALL_BB (bb)
    {
      VTI (bb)->visited = false;
      VTI (bb)->flooded = false;
      dataflow_set_init (&VTI (bb)->in);
      dataflow_set_init (&VTI (bb)->out);
      VTI (bb)->permp = NULL;
    }

  if (MAY_HAVE_DEBUG_INSNS)
    {
      cselib_init (CSELIB_RECORD_MEMORY | CSELIB_PRESERVE_CONSTANTS);
      scratch_regs = BITMAP_ALLOC (NULL);
      valvar_pool = create_alloc_pool ("small variable_def pool",
				       sizeof (struct variable_def), 256);
      preserved_values = VEC_alloc (rtx, heap, 256);
    }
  else
    {
      scratch_regs = NULL;
      valvar_pool = NULL;
    }

  if (!frame_pointer_needed)
    {
      rtx reg, elim;

      if (!vt_stack_adjustments ())
	return false;

#ifdef FRAME_POINTER_CFA_OFFSET
      reg = frame_pointer_rtx;
#else
      reg = arg_pointer_rtx;
#endif
      elim = eliminate_regs (reg, VOIDmode, NULL_RTX);
      if (elim != reg)
	{
	  if (GET_CODE (elim) == PLUS)
	    elim = XEXP (elim, 0);
	  if (elim == stack_pointer_rtx)
	    vt_init_cfa_base ();
	}
    }
  else if (!crtl->stack_realign_tried)
    {
      rtx reg, elim;

#ifdef FRAME_POINTER_CFA_OFFSET
      reg = frame_pointer_rtx;
      fp_cfa_offset = FRAME_POINTER_CFA_OFFSET (current_function_decl);
#else
      reg = arg_pointer_rtx;
      fp_cfa_offset = ARG_POINTER_CFA_OFFSET (current_function_decl);
#endif
      elim = eliminate_regs (reg, VOIDmode, NULL_RTX);
      if (elim != reg)
	{
	  if (GET_CODE (elim) == PLUS)
	    {
	      fp_cfa_offset -= INTVAL (XEXP (elim, 1));
	      elim = XEXP (elim, 0);
	    }
	  if (elim != hard_frame_pointer_rtx)
	    fp_cfa_offset = -1;
	  else
	    prologue_bb = single_succ (ENTRY_BLOCK_PTR);
	}
    }

  hard_frame_pointer_adjustment = -1;

  FOR_EACH_BB (bb)
    {
      rtx insn;
      HOST_WIDE_INT pre, post = 0;
      basic_block first_bb, last_bb;

      if (MAY_HAVE_DEBUG_INSNS)
	{
	  cselib_record_sets_hook = add_with_sets;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "first value: %i\n",
		     cselib_get_next_uid ());
	}

      first_bb = bb;
      for (;;)
	{
	  edge e;
	  if (bb->next_bb == EXIT_BLOCK_PTR
	      || ! single_pred_p (bb->next_bb))
	    break;
	  e = find_edge (bb, bb->next_bb);
	  if (! e || (e->flags & EDGE_FALLTHRU) == 0)
	    break;
	  bb = bb->next_bb;
	}
      last_bb = bb;

      /* Add the micro-operations to the vector.  */
      FOR_BB_BETWEEN (bb, first_bb, last_bb->next_bb, next_bb)
	{
	  HOST_WIDE_INT offset = VTI (bb)->out.stack_adjust;
	  VTI (bb)->out.stack_adjust = VTI (bb)->in.stack_adjust;
	  for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	       insn = NEXT_INSN (insn))
	    {
	      if (INSN_P (insn))
		{
		  if (!frame_pointer_needed)
		    {
		      insn_stack_adjust_offset_pre_post (insn, &pre, &post);
		      if (pre)
			{
			  micro_operation mo;
			  mo.type = MO_ADJUST;
			  mo.u.adjust = pre;
			  mo.insn = insn;
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    log_op_type (PATTERN (insn), bb, insn,
					 MO_ADJUST, dump_file);
			  VEC_safe_push (micro_operation, heap, VTI (bb)->mos,
					 &mo);
			  VTI (bb)->out.stack_adjust += pre;
			}
		    }

		  cselib_hook_called = false;
		  adjust_insn (bb, insn);
		  if (MAY_HAVE_DEBUG_INSNS)
		    {
		      cselib_process_insn (insn);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  print_rtl_single (dump_file, insn);
			  dump_cselib_table (dump_file);
			}
		    }
		  if (!cselib_hook_called)
		    add_with_sets (insn, 0, 0);
		  cancel_changes (0);

		  if (!frame_pointer_needed && post)
		    {
		      micro_operation mo;
		      mo.type = MO_ADJUST;
		      mo.u.adjust = post;
		      mo.insn = insn;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			log_op_type (PATTERN (insn), bb, insn,
				     MO_ADJUST, dump_file);
		      VEC_safe_push (micro_operation, heap, VTI (bb)->mos,
				     &mo);
		      VTI (bb)->out.stack_adjust += post;
		    }

		  if (bb == prologue_bb
		      && hard_frame_pointer_adjustment == -1
		      && RTX_FRAME_RELATED_P (insn)
		      && fp_setter (insn))
		    {
		      vt_init_cfa_base ();
		      hard_frame_pointer_adjustment = fp_cfa_offset;
		    }
		}
	    }
	  gcc_assert (offset == VTI (bb)->out.stack_adjust);
	}

      bb = last_bb;

      if (MAY_HAVE_DEBUG_INSNS)
	{
	  cselib_preserve_only_values ();
	  cselib_reset_table (cselib_get_next_uid ());
	  cselib_record_sets_hook = NULL;
	}
    }

  hard_frame_pointer_adjustment = -1;
  VTI (ENTRY_BLOCK_PTR)->flooded = true;
  vt_add_function_parameters ();
  cfa_base_rtx = NULL_RTX;
  return true;
}

/* Get rid of all debug insns from the insn stream.  */

static void
delete_debug_insns (void)
{
  basic_block bb;
  rtx insn, next;

  if (!MAY_HAVE_DEBUG_INSNS)
    return;

  FOR_EACH_BB (bb)
    {
      FOR_BB_INSNS_SAFE (bb, insn, next)
	if (DEBUG_INSN_P (insn))
	  delete_insn (insn);
    }
}

/* Run a fast, BB-local only version of var tracking, to take care of
   information that we don't do global analysis on, such that not all
   information is lost.  If SKIPPED holds, we're skipping the global
   pass entirely, so we should try to use information it would have
   handled as well..  */

static void
vt_debug_insns_local (bool skipped ATTRIBUTE_UNUSED)
{
  /* ??? Just skip it all for now.  */
  delete_debug_insns ();
}

/* Free the data structures needed for variable tracking.  */

static void
vt_finalize (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      VEC_free (micro_operation, heap, VTI (bb)->mos);
    }

  FOR_ALL_BB (bb)
    {
      dataflow_set_destroy (&VTI (bb)->in);
      dataflow_set_destroy (&VTI (bb)->out);
      if (VTI (bb)->permp)
	{
	  dataflow_set_destroy (VTI (bb)->permp);
	  XDELETE (VTI (bb)->permp);
	}
    }
  free_aux_for_blocks ();
  htab_delete (empty_shared_hash->htab);
  htab_delete (changed_variables);
  free_alloc_pool (attrs_pool);
  free_alloc_pool (var_pool);
  free_alloc_pool (loc_chain_pool);
  free_alloc_pool (shared_hash_pool);

  if (MAY_HAVE_DEBUG_INSNS)
    {
      htab_delete (value_chains);
      free_alloc_pool (value_chain_pool);
      free_alloc_pool (valvar_pool);
      VEC_free (rtx, heap, preserved_values);
      cselib_finish ();
      BITMAP_FREE (scratch_regs);
      scratch_regs = NULL;
    }

  if (vui_vec)
    XDELETEVEC (vui_vec);
  vui_vec = NULL;
  vui_allocated = 0;
}

/* The entry point to variable tracking pass.  */

static inline unsigned int
variable_tracking_main_1 (void)
{
  bool success;

  if (flag_var_tracking_assignments < 0)
    {
      delete_debug_insns ();
      return 0;
    }

  if (n_basic_blocks > 500 && n_edges / n_basic_blocks >= 20)
    {
      vt_debug_insns_local (true);
      return 0;
    }

  mark_dfs_back_edges ();
  if (!vt_initialize ())
    {
      vt_finalize ();
      vt_debug_insns_local (true);
      return 0;
    }

  success = vt_find_locations ();

  if (!success && flag_var_tracking_assignments > 0)
    {
      vt_finalize ();

      delete_debug_insns ();

      /* This is later restored by our caller.  */
      flag_var_tracking_assignments = 0;

      success = vt_initialize ();
      gcc_assert (success);

      success = vt_find_locations ();
    }

  if (!success)
    {
      vt_finalize ();
      vt_debug_insns_local (false);
      return 0;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_dataflow_sets ();
      dump_flow_info (dump_file, dump_flags);
    }

  vt_emit_notes ();

  vt_finalize ();
  vt_debug_insns_local (false);
  return 0;
}

unsigned int
variable_tracking_main (void)
{
  unsigned int ret;
  int save = flag_var_tracking_assignments;

  ret = variable_tracking_main_1 ();

  flag_var_tracking_assignments = save;

  return ret;
}

static bool
gate_handle_var_tracking (void)
{
  return (flag_var_tracking);
}



struct rtl_opt_pass pass_variable_tracking =
{
 {
  RTL_PASS,
  "vartrack",                           /* name */
  gate_handle_var_tracking,             /* gate */
  variable_tracking_main,               /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_VAR_TRACKING,                      /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_dump_func | TODO_verify_rtl_sharing/* todo_flags_finish */
 }
};
