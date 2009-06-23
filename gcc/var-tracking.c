/* Variable tracking routines for the GNU compiler.
   Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008, 2009
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

/* Type of micro operation.  */
enum micro_operation_type
{
  MO_USE,	/* Use location (REG or MEM).  */
  MO_USE_NO_VAR,/* Use location which is not associated with a variable
		   or the variable is not trackable.  */
  MO_SET,	/* Set location.  */
  MO_COPY,	/* Copy the same portion of a variable from one
		   location to another.  */
  MO_CLOBBER,	/* Clobber location.  */
  MO_CALL,	/* Call insn.  */
  MO_ADJUST	/* Adjust stack pointer.  */
};

/* Where shall the note be emitted?  BEFORE or AFTER the instruction.  */
enum emit_note_where
{
  EMIT_NOTE_BEFORE_INSN,
  EMIT_NOTE_AFTER_INSN
};

/* Structure holding information about micro operation.  */
typedef struct micro_operation_def
{
  /* Type of micro operation.  */
  enum micro_operation_type type;

  union {
    /* Location.  For MO_SET and MO_COPY, this is the SET that performs
       the assignment, if known, otherwise it is the target of the
       assignment.  */
    rtx loc;

    /* Stack adjustment.  */
    HOST_WIDE_INT adjust;
  } u;

  /* The instruction which the micro operation is in, for MO_USE,
     MO_USE_NO_VAR, MO_CALL and MO_ADJUST, or the subsequent
     instruction or note in the original flow (before any var-tracking
     notes are inserted, to simplify emission of notes), for MO_SET
     and MO_CLOBBER.  */
  rtx insn;
} micro_operation;

/* Structure for passing some other parameters to function
   emit_note_insn_var_location.  */
typedef struct emit_note_data_def
{
  /* The instruction which the note will be emitted before/after.  */
  rtx insn;

  /* Where the note will be emitted (before/after insn)?  */
  enum emit_note_where where;
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
  tree decl;

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
} dataflow_set;

/* The structure (one for each basic block) containing the information
   needed for variable tracking.  */
typedef struct variable_tracking_info_def
{
  /* Number of micro operations stored in the MOS array.  */
  int n_mos;

  /* The array of micro operations.  */
  micro_operation *mos;

  /* The IN and OUT set for dataflow analysis.  */
  dataflow_set in;
  dataflow_set out;

  /* Has the block been visited in DFS?  */
  bool visited;
} *variable_tracking_info;

/* Structure for chaining the locations.  */
typedef struct location_chain_def
{
  /* Next element in the chain.  */
  struct location_chain_def *next;

  /* The location (REG or MEM).  */
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
  /* The declaration of the variable.  */
  tree decl;

  /* Reference count.  */
  int refcount;

  /* Number of variable parts.  */
  int n_var_parts;

  /* The variable parts.  */
  variable_part var_part[MAX_VAR_PARTS];
} *variable;
typedef const struct variable_def *const_variable;

/* Hash function for DECL for VARIABLE_HTAB.  */
#define VARIABLE_HASH_VAL(decl) (DECL_UID (decl))

/* Pointer to the BB's information specific to variable tracking pass.  */
#define VTI(BB) ((variable_tracking_info) (BB)->aux)

/* Macro to access MEM_OFFSET as an HOST_WIDE_INT.  Evaluates MEM twice.  */
#define INT_MEM_OFFSET(mem) (MEM_OFFSET (mem) ? INTVAL (MEM_OFFSET (mem)) : 0)

/* Alloc pool for struct attrs_def.  */
static alloc_pool attrs_pool;

/* Alloc pool for struct variable_def.  */
static alloc_pool var_pool;

/* Alloc pool for struct location_chain_def.  */
static alloc_pool loc_chain_pool;

/* Alloc pool for struct shared_hash_def.  */
static alloc_pool shared_hash_pool;

/* Changed variables, notes will be emitted for them.  */
static htab_t changed_variables;

/* Shall notes be emitted?  */
static bool emit_notes;

/* Empty shared hashtable.  */
static shared_hash empty_shared_hash;

/* Local function prototypes.  */
static void stack_adjust_offset_pre_post (rtx, HOST_WIDE_INT *,
					  HOST_WIDE_INT *);
static void insn_stack_adjust_offset_pre_post (rtx, HOST_WIDE_INT *,
					       HOST_WIDE_INT *);
static void bb_stack_adjust_offset (basic_block);
static bool vt_stack_adjustments (void);
static rtx adjust_stack_reference (rtx, HOST_WIDE_INT);
static hashval_t variable_htab_hash (const void *);
static int variable_htab_eq (const void *, const void *);
static void variable_htab_free (void *);

static void init_attrs_list_set (attrs *);
static void attrs_list_clear (attrs *);
static attrs attrs_list_member (attrs, tree, HOST_WIDE_INT);
static void attrs_list_insert (attrs *, tree, HOST_WIDE_INT, rtx);
static void attrs_list_copy (attrs *, attrs);
static void attrs_list_union (attrs *, attrs);

static variable unshare_variable (dataflow_set *set, variable var, 
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
static int variable_canonicalize (void **, void *);
static void dataflow_set_union (dataflow_set *, dataflow_set *);
static bool variable_part_different_p (variable_part *, variable_part *);
static bool variable_different_p (variable, variable, bool);
static int dataflow_set_different_1 (void **, void *);
static bool dataflow_set_different (dataflow_set *, dataflow_set *);
static void dataflow_set_destroy (dataflow_set *);

static bool contains_symbol_ref (rtx);
static bool track_expr_p (tree);
static bool same_variable_part_p (rtx, tree, HOST_WIDE_INT);
static int count_uses (rtx *, void *);
static void count_uses_1 (rtx *, void *);
static void count_stores (rtx, const_rtx, void *);
static int add_uses (rtx *, void *);
static void add_uses_1 (rtx *, void *);
static void add_stores (rtx, const_rtx, void *);
static bool compute_bb_dataflow (basic_block);
static void vt_find_locations (void);

static void dump_attrs_list (attrs);
static int dump_variable (void **, void *);
static void dump_vars (htab_t);
static void dump_dataflow_set (dataflow_set *);
static void dump_dataflow_sets (void);

static void variable_was_changed (variable, dataflow_set *);
static void set_variable_part (dataflow_set *, rtx, tree, HOST_WIDE_INT, 
			       enum var_init_status, rtx);
static void clobber_variable_part (dataflow_set *, rtx, tree, HOST_WIDE_INT, 
				   rtx);
static void delete_variable_part (dataflow_set *, rtx, tree, HOST_WIDE_INT);
static int emit_note_insn_var_location (void **, void *);
static void emit_notes_for_changes (rtx, enum emit_note_where);
static int emit_notes_for_differences_1 (void **, void *);
static int emit_notes_for_differences_2 (void **, void *);
static void emit_notes_for_differences (rtx, dataflow_set *, dataflow_set *);
static void emit_notes_in_bb (basic_block);
static void vt_emit_notes (void);

static bool vt_get_decl_and_offset (rtx, tree *, HOST_WIDE_INT *);
static void vt_add_function_parameters (void);
static void vt_initialize (void);
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

/* Compute stack adjustment in basic block BB.  */

static void
bb_stack_adjust_offset (basic_block bb)
{
  HOST_WIDE_INT offset;
  int i;

  offset = VTI (bb)->in.stack_adjust;
  for (i = 0; i < VTI (bb)->n_mos; i++)
    {
      if (VTI (bb)->mos[i].type == MO_ADJUST)
	offset += VTI (bb)->mos[i].u.adjust;
      else if (VTI (bb)->mos[i].type != MO_CALL)
	{
	  if (MEM_P (VTI (bb)->mos[i].u.loc))
	    {
	      VTI (bb)->mos[i].u.loc
		= adjust_stack_reference (VTI (bb)->mos[i].u.loc, -offset);
	    }
	}
    }
  VTI (bb)->out.stack_adjust = offset;
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
	  VTI (dest)->visited = true;
	  VTI (dest)->in.stack_adjust = VTI (src)->out.stack_adjust;
	  bb_stack_adjust_offset (dest);

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

/* Adjust stack reference MEM by ADJUSTMENT bytes and make it relative
   to the argument pointer.  Return the new rtx.  */

static rtx
adjust_stack_reference (rtx mem, HOST_WIDE_INT adjustment)
{
  rtx addr, cfa, tmp;

#ifdef FRAME_POINTER_CFA_OFFSET
  adjustment -= FRAME_POINTER_CFA_OFFSET (current_function_decl);
  cfa = plus_constant (frame_pointer_rtx, adjustment);
#else
  adjustment -= ARG_POINTER_CFA_OFFSET (current_function_decl);
  cfa = plus_constant (arg_pointer_rtx, adjustment);
#endif

  addr = replace_rtx (copy_rtx (XEXP (mem, 0)), stack_pointer_rtx, cfa);
  tmp = simplify_rtx (addr);
  if (tmp)
    addr = tmp;

  return replace_equiv_address_nv (mem, addr);
}

/* The hash function for variable_htab, computes the hash value
   from the declaration of variable X.  */

static hashval_t
variable_htab_hash (const void *x)
{
  const_variable const v = (const_variable) x;

  return (VARIABLE_HASH_VAL (v->decl));
}

/* Compare the declaration of variable X with declaration Y.  */

static int
variable_htab_eq (const void *x, const void *y)
{
  const_variable const v = (const_variable) x;
  const_tree const decl = (const_tree) y;

  return (VARIABLE_HASH_VAL (v->decl) == VARIABLE_HASH_VAL (decl));
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
  pool_free (var_pool, var);
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
attrs_list_member (attrs list, tree decl, HOST_WIDE_INT offset)
{
  for (; list; list = list->next)
    if (list->decl == decl && list->offset == offset)
      return list;
  return NULL;
}

/* Insert the triplet DECL, OFFSET, LOC to the list *LISTP.  */

static void
attrs_list_insert (attrs *listp, tree decl, HOST_WIDE_INT offset, rtx loc)
{
  attrs list;

  list = (attrs) pool_alloc (attrs_pool);
  list->loc = loc;
  list->decl = decl;
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
      n->decl = src->decl;
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
      if (!attrs_list_member (*dstp, src->decl, src->offset))
	attrs_list_insert (dstp, src->decl, src->offset, src->loc);
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

/* Unshare *PVARS if shared and return slot for DECL.  If INS is
   INSERT, insert it if not already present.  */

static inline void **
shared_hash_find_slot_unshare (shared_hash *pvars, tree decl,
			       enum insert_option ins)
{
  if (shared_hash_shared (*pvars))
    *pvars = shared_hash_unshare (*pvars);
  return htab_find_slot_with_hash (shared_hash_htab (*pvars), decl,
				   VARIABLE_HASH_VAL (decl), ins);
}

/* Return slot for DECL, if it is already present in the hash table.
   If it is not present, insert it only VARS is not shared, otherwise
   return NULL.  */

static inline void **
shared_hash_find_slot (shared_hash vars, tree decl)
{
  return htab_find_slot_with_hash (shared_hash_htab (vars), decl,
				   VARIABLE_HASH_VAL (decl),
				   shared_hash_shared (vars)
				   ? NO_INSERT : INSERT);
}

/* Return slot for DECL only if it is already present in the hash table.  */

static inline void **
shared_hash_find_slot_noinsert (shared_hash vars, tree decl)
{
  return htab_find_slot_with_hash (shared_hash_htab (vars), decl,
				   VARIABLE_HASH_VAL (decl), NO_INSERT);
}

/* Return variable for DECL or NULL if not already present in the hash
   table.  */

static inline variable
shared_hash_find (shared_hash vars, tree decl)
{
  return (variable)
	 htab_find_with_hash (shared_hash_htab (vars), decl,
			      VARIABLE_HASH_VAL (decl));
}

/* Return a copy of a variable VAR and insert it to dataflow set SET.  */

static variable
unshare_variable (dataflow_set *set, variable var, 
		  enum var_init_status initialized)
{
  void **slot;
  variable new_var;
  int i;

  new_var = (variable) pool_alloc (var_pool);
  new_var->decl = var->decl;
  new_var->refcount = 1;
  var->refcount--;
  new_var->n_var_parts = var->n_var_parts;

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

      /* We are at the basic block boundary when copying variable description
	 so set the CUR_LOC to be the first element of the chain.  */
      if (new_var->var_part[i].loc_chain)
	new_var->var_part[i].cur_loc = new_var->var_part[i].loc_chain->loc;
      else
	new_var->var_part[i].cur_loc = NULL;
    }

  slot = shared_hash_find_slot_unshare (&set->vars, new_var->decl, INSERT);
  *slot = new_var;
  return new_var;
}

/* Add a variable from *SLOT to hash table DATA and increase its reference
   count.  */

static int
vars_copy_1 (void **slot, void *data)
{
  htab_t dst = (htab_t) data;
  variable src, *dstp;

  src = *(variable *) slot;
  src->refcount++;

  dstp = (variable *) htab_find_slot_with_hash (dst, src->decl,
						VARIABLE_HASH_VAL (src->decl),
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

/* Set the register to contain REG_EXPR (LOC), REG_OFFSET (LOC).  */

static void
var_reg_set (dataflow_set *set, rtx loc, enum var_init_status initialized, 
	     rtx set_src)
{
  tree decl = REG_EXPR (loc);
  HOST_WIDE_INT offset = REG_OFFSET (loc);
  attrs node;

  decl = var_debug_decl (decl);

  for (node = set->regs[REGNO (loc)]; node; node = node->next)
    if (node->decl == decl && node->offset == offset)
      break;
  if (!node)
    attrs_list_insert (&set->regs[REGNO (loc)], decl, offset, loc);
  set_variable_part (set, loc, decl, offset, initialized, set_src);
}

static enum var_init_status
get_init_value (dataflow_set *set, rtx loc, tree decl)
{
  variable var;
  int i;
  enum var_init_status ret_val = VAR_INIT_STATUS_UNKNOWN;

  if (! flag_var_tracking_uninit)
    return VAR_INIT_STATUS_INITIALIZED;

  var = shared_hash_find (set->vars, decl);
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
    initialized = get_init_value (set, loc, decl);

  nextp = &set->regs[REGNO (loc)];
  for (node = *nextp; node; node = next)
    {
      next = node->next;
      if (node->decl != decl || node->offset != offset)
	{
	  delete_variable_part (set, node->loc, node->decl, node->offset);
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
    clobber_variable_part (set, loc, decl, offset, set_src);
  var_reg_set (set, loc, initialized, set_src);
}

/* Delete current content of register LOC in dataflow set SET.  If
   CLOBBER is true, also delete any other live copies of the same
   variable part.  */

static void
var_reg_delete (dataflow_set *set, rtx loc, bool clobber)
{
  attrs *reg = &set->regs[REGNO (loc)];
  attrs node, next;

  if (clobber)
    {
      tree decl = REG_EXPR (loc);
      HOST_WIDE_INT offset = REG_OFFSET (loc);

      decl = var_debug_decl (decl);

      clobber_variable_part (set, NULL, decl, offset, NULL);
    }

  for (node = *reg; node; node = next)
    {
      next = node->next;
      delete_variable_part (set, node->loc, node->decl, node->offset);
      pool_free (attrs_pool, node);
    }
  *reg = NULL;
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
      delete_variable_part (set, node->loc, node->decl, node->offset);
      pool_free (attrs_pool, node);
    }
  *reg = NULL;
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

  decl = var_debug_decl (decl);

  set_variable_part (set, loc, decl, offset, initialized, set_src);
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
    initialized = get_init_value (set, loc, decl);

  if (modify)
    clobber_variable_part (set, NULL, decl, offset, set_src);
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
    clobber_variable_part (set, NULL, decl, offset, NULL);
  delete_variable_part (set, loc, decl, offset);
}

/* Initialize dataflow set SET to be empty. 
   VARS_SIZE is the initial size of hash table VARS.  */

static void
dataflow_set_init (dataflow_set *set)
{
  init_attrs_list_set (set->regs);
  set->vars = shared_hash_copy (empty_shared_hash);
  set->stack_adjust = 0;
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

  src = *(variable *) slot;
  dstp = shared_hash_find_slot (set->vars, src->decl);
  if (!dstp || !*dstp)
    {
      src->refcount++;

      /* If CUR_LOC of some variable part is not the first element of
	 the location chain we are going to change it so we have to make
	 a copy of the variable.  */
      for (k = 0; k < src->n_var_parts; k++)
	{
	  gcc_assert (!src->var_part[k].loc_chain
		      == !src->var_part[k].cur_loc);
	  if (src->var_part[k].loc_chain)
	    {
	      gcc_assert (src->var_part[k].cur_loc);
	      if (src->var_part[k].cur_loc != src->var_part[k].loc_chain->loc)
		break;
	    }
	}
      if (k < src->n_var_parts)
	{
	  if (dstp)
	    *dstp = (void *) src;
	  unshare_variable (set, src, VAR_INIT_STATUS_UNKNOWN);
	}
      else
	{
	  if (!dstp)
	    dstp = shared_hash_find_slot_unshare (&set->vars, src->decl,
						  INSERT);
	  *dstp = (void *) src;
	}

      /* Continue traversing the hash table.  */
      return 1;
    }
  else
    dst = (variable) *dstp;

  gcc_assert (src->n_var_parts);

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
  gcc_assert (k <= MAX_VAR_PARTS);

  if ((dst->refcount > 1 || shared_hash_shared (set->vars))
      && dst->n_var_parts != k)
    dst = unshare_variable (set, dst, VAR_INIT_STATUS_UNKNOWN);

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
	  if (dst->refcount > 1 || shared_hash_shared (set->vars))
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
		dst = unshare_variable (set, dst, VAR_INIT_STATUS_UNKNOWN);
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

      /* We are at the basic block boundary when computing union
	 so set the CUR_LOC to be the first element of the chain.  */
      if (dst->var_part[k].loc_chain)
	dst->var_part[k].cur_loc = dst->var_part[k].loc_chain->loc;
      else
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

/* Like variable_union, but only used when doing dataflow_set_union
   into an empty hashtab.  To allow sharing, dst is initially shared
   with src (so all variables are "copied" from src to dst hashtab),
   so only unshare_variable for variables that need canonicalization
   are needed.  */

static int
variable_canonicalize (void **slot, void *data)
{
  variable src;
  dataflow_set *set = (dataflow_set *) data;
  int k;

  src = *(variable *) slot;

  /* If CUR_LOC of some variable part is not the first element of
     the location chain we are going to change it so we have to make
     a copy of the variable.  */
  for (k = 0; k < src->n_var_parts; k++)
    {
      gcc_assert (!src->var_part[k].loc_chain == !src->var_part[k].cur_loc);
      if (src->var_part[k].loc_chain)
	{
	  gcc_assert (src->var_part[k].cur_loc);
	  if (src->var_part[k].cur_loc != src->var_part[k].loc_chain->loc)
	    break;
	}
    }
  if (k < src->n_var_parts)
    unshare_variable (set, src, VAR_INIT_STATUS_UNKNOWN);
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
      htab_traverse (shared_hash_htab (src->vars), variable_canonicalize, dst);
    }
  else
    htab_traverse (shared_hash_htab (src->vars), variable_union, dst);
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

/* Return true if variables VAR1 and VAR2 are different.
   If COMPARE_CURRENT_LOCATION is true compare also the cur_loc of each
   variable part.  */

static bool
variable_different_p (variable var1, variable var2,
		      bool compare_current_location)
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
      if (compare_current_location)
	{
	  if (!((REG_P (var1->var_part[i].cur_loc)
		 && REG_P (var2->var_part[i].cur_loc)
		 && (REGNO (var1->var_part[i].cur_loc)
		     == REGNO (var2->var_part[i].cur_loc)))
		|| rtx_equal_p (var1->var_part[i].cur_loc,
				var2->var_part[i].cur_loc)))
	    return true;
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

  var1 = *(variable *) slot;
  var2 = (variable) htab_find_with_hash (htab, var1->decl,
			      VARIABLE_HASH_VAL (var1->decl));
  if (!var2)
    {
      dataflow_set_different_value = true;

      /* Stop traversing the hash table.  */
      return 0;
    }

  if (variable_different_p (var1, var2, false))
    {
      dataflow_set_different_value = true;

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
track_expr_p (tree expr)
{
  rtx decl_rtl;
  tree realdecl;

  /* If EXPR is not a parameter or a variable do not track it.  */
  if (TREE_CODE (expr) != VAR_DECL && TREE_CODE (expr) != PARM_DECL)
    return 0;

  /* It also must have a name...  */
  if (!DECL_NAME (expr))
    return 0;

  /* ... and a RTL assigned to it.  */
  decl_rtl = DECL_RTL_IF_SET (expr);
  if (!decl_rtl)
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
  if (MEM_P (decl_rtl)
      && contains_symbol_ref (XEXP (decl_rtl, 0)))
    return 0;

  /* If RTX is a memory it should not be very large (because it would be
     an array or struct).  */
  if (MEM_P (decl_rtl))
    {
      /* Do not track structures and arrays.  */
      if (GET_MODE (decl_rtl) == BLKmode
	  || AGGREGATE_TYPE_P (TREE_TYPE (realdecl)))
	return 0;
      if (MEM_SIZE (decl_rtl)
	  && INTVAL (MEM_SIZE (decl_rtl)) > MAX_VAR_PARTS)
	return 0;
    }

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

  if (expr == NULL || !track_expr_p (expr))
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

/* Count uses (register and memory references) LOC which will be tracked.
   INSN is instruction which the LOC is part of.  */

static int
count_uses (rtx *loc, void *insn)
{
  basic_block bb = BLOCK_FOR_INSN ((rtx) insn);

  if (REG_P (*loc))
    {
      gcc_assert (REGNO (*loc) < FIRST_PSEUDO_REGISTER);
      VTI (bb)->n_mos++;
    }
  else if (MEM_P (*loc)
	   && track_loc_p (*loc, MEM_EXPR (*loc), INT_MEM_OFFSET (*loc),
			   false, NULL, NULL))
    {
      VTI (bb)->n_mos++;
    }

  return 0;
}

/* Helper function for finding all uses of REG/MEM in X in insn INSN.  */

static void
count_uses_1 (rtx *x, void *insn)
{
  for_each_rtx (x, count_uses, insn);
}

/* Count stores (register and memory references) LOC which will be tracked.
   INSN is instruction which the LOC is part of.  */

static void
count_stores (rtx loc, const_rtx expr ATTRIBUTE_UNUSED, void *insn)
{
  count_uses (&loc, insn);
}

/* Add uses (register and memory references) LOC which will be tracked
   to VTI (bb)->mos.  INSN is instruction which the LOC is part of.  */

static int
add_uses (rtx *loc, void *insn)
{
  enum machine_mode mode;

  if (REG_P (*loc))
    {
      basic_block bb = BLOCK_FOR_INSN ((rtx) insn);
      micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

      if (track_loc_p (*loc, REG_EXPR (*loc), REG_OFFSET (*loc),
		       false, &mode, NULL))
	{
	  mo->type = MO_USE;
	  mo->u.loc = var_lowpart (mode, *loc);
	}
      else
	{
	  mo->type = MO_USE_NO_VAR;
	  mo->u.loc = *loc;
	}
      mo->insn = (rtx) insn;
    }
  else if (MEM_P (*loc)
	   && track_loc_p (*loc, MEM_EXPR (*loc), INT_MEM_OFFSET (*loc),
			   false, &mode, NULL))
    {
      basic_block bb = BLOCK_FOR_INSN ((rtx) insn);
      micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

      mo->type = MO_USE;
      mo->u.loc = var_lowpart (mode, *loc);
      mo->insn = (rtx) insn;
    }

  return 0;
}

/* Helper function for finding all uses of REG/MEM in X in insn INSN.  */

static void
add_uses_1 (rtx *x, void *insn)
{
  for_each_rtx (x, add_uses, insn);
}

/* Add stores (register and memory references) LOC which will be tracked
   to VTI (bb)->mos. EXPR is the RTL expression containing the store.
   INSN is instruction which the LOC is part of.  */

static void
add_stores (rtx loc, const_rtx expr, void *insn)
{
  enum machine_mode mode;

  if (REG_P (loc))
    {
      basic_block bb = BLOCK_FOR_INSN ((rtx) insn);
      micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

      if (GET_CODE (expr) == CLOBBER
	  || !track_loc_p (loc, REG_EXPR (loc), REG_OFFSET (loc),
			   true, &mode, NULL))
	{
	  mo->type = MO_CLOBBER;
	  mo->u.loc = loc;
	}
      else
	{
	  rtx src = NULL;

	  if (GET_CODE (expr) == SET && SET_DEST (expr) == loc)
	    src = var_lowpart (mode, SET_SRC (expr));
	  loc = var_lowpart (mode, loc);

	  if (src == NULL)
	    {
	      mo->type = MO_SET;
	      mo->u.loc = loc;
	    }
	  else
	    {
	      if (SET_SRC (expr) != src)
		expr = gen_rtx_SET (VOIDmode, loc, src);
	      if (same_variable_part_p (src, REG_EXPR (loc), REG_OFFSET (loc)))
		mo->type = MO_COPY;
	      else
		mo->type = MO_SET;
	      mo->u.loc = CONST_CAST_RTX (expr);
	    }
	}
      mo->insn = (rtx) insn;
    }
  else if (MEM_P (loc)
	   && track_loc_p (loc, MEM_EXPR (loc), INT_MEM_OFFSET (loc),
			   false, &mode, NULL))
    {
      basic_block bb = BLOCK_FOR_INSN ((rtx) insn);
      micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

      if (GET_CODE (expr) == CLOBBER)
	{
	  mo->type = MO_CLOBBER;
	  mo->u.loc = var_lowpart (mode, loc);
	}
      else
	{
	  rtx src = NULL;

	  if (GET_CODE (expr) == SET && SET_DEST (expr) == loc)
	    src = var_lowpart (mode, SET_SRC (expr));
	  loc = var_lowpart (mode, loc);

	  if (src == NULL)
	    {
	      mo->type = MO_SET;
	      mo->u.loc = loc;
	    }
	  else
	    {
	      if (SET_SRC (expr) != src)
		expr = gen_rtx_SET (VOIDmode, loc, src);
	      if (same_variable_part_p (SET_SRC (expr),
					MEM_EXPR (loc),
					INT_MEM_OFFSET (loc)))
		mo->type = MO_COPY;
	      else
		mo->type = MO_SET;
	      mo->u.loc = CONST_CAST_RTX (expr);
	    }
	}
      mo->insn = (rtx) insn;
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
    status = get_init_value (in, src, decl);

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
      var = shared_hash_find (set->vars, decl);
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
  int i, n, r;
  bool changed;
  dataflow_set old_out;
  dataflow_set *in = &VTI (bb)->in;
  dataflow_set *out = &VTI (bb)->out;

  dataflow_set_init (&old_out);
  dataflow_set_copy (&old_out, out);
  dataflow_set_copy (out, in);

  n = VTI (bb)->n_mos;
  for (i = 0; i < n; i++)
    {
      switch (VTI (bb)->mos[i].type)
	{
	  case MO_CALL:
	    for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	      if (TEST_HARD_REG_BIT (call_used_reg_set, r))
		var_regno_delete (out, r);
	    break;

	  case MO_USE:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;

	      if (REG_P (loc))
		var_reg_set (out, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	      else if (MEM_P (loc))
		var_mem_set (out, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	    }
	    break;

	  case MO_SET:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;
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
	      rtx loc = VTI (bb)->mos[i].u.loc;
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
	      rtx loc = VTI (bb)->mos[i].u.loc;

	      if (REG_P (loc))
		var_reg_delete (out, loc, false);
	      else if (MEM_P (loc))
		var_mem_delete (out, loc, false);
	    }
	    break;

	  case MO_CLOBBER:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;

	      if (REG_P (loc))
		var_reg_delete (out, loc, true);
	      else if (MEM_P (loc))
		var_mem_delete (out, loc, true);
	    }
	    break;

	  case MO_ADJUST:
	    out->stack_adjust += VTI (bb)->mos[i].u.adjust;
	    break;
	}
    }

  changed = dataflow_set_different (&old_out, out);
  dataflow_set_destroy (&old_out);
  return changed;
}

/* Find the locations of variables in the whole function.  */

static void
vt_find_locations (void)
{
  fibheap_t worklist, pending, fibheap_swap;
  sbitmap visited, in_worklist, in_pending, sbitmap_swap;
  basic_block bb;
  edge e;
  int *bb_order;
  int *rc_order;
  int i;

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

  while (!fibheap_empty (pending))
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

	      SET_BIT (visited, bb->index);

	      /* Calculate the IN set as union of predecessor OUT sets.  */
	      dataflow_set_clear (&VTI (bb)->in);
	      FOR_EACH_EDGE (e, ei, bb->preds)
		{
		  dataflow_set_union (&VTI (bb)->in, &VTI (e->src)->out);
		}

	      changed = compute_bb_dataflow (bb);
	      if (changed)
		{
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    {
		      if (e->dest == EXIT_BLOCK_PTR)
			continue;

		      if (e->dest == bb)
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
	    }
	}
    }

  free (bb_order);
  fibheap_delete (worklist);
  fibheap_delete (pending);
  sbitmap_free (visited);
  sbitmap_free (in_worklist);
  sbitmap_free (in_pending);
}

/* Print the content of the LIST to dump file.  */

static void
dump_attrs_list (attrs list)
{
  for (; list; list = list->next)
    {
      print_mem_expr (dump_file, list->decl);
      fprintf (dump_file, "+" HOST_WIDE_INT_PRINT_DEC, list->offset);
    }
  fprintf (dump_file, "\n");
}

/* Print the information about variable *SLOT to dump file.  */

static int
dump_variable (void **slot, void *data ATTRIBUTE_UNUSED)
{
  variable var = *(variable *) slot;
  int i;
  location_chain node;

  fprintf (dump_file, "  name: %s",
	   IDENTIFIER_POINTER (DECL_NAME (var->decl)));
  if (dump_flags & TDF_UID)
    fprintf (dump_file, " D.%u\n", DECL_UID (var->decl));
  else
    fprintf (dump_file, "\n");

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

  /* Continue traversing the hash table.  */
  return 1;
}

/* Print the information about variables from hash table VARS to dump file.  */

static void
dump_vars (htab_t vars)
{
  if (htab_elements (vars) > 0)
    {
      fprintf (dump_file, "Variables:\n");
      htab_traverse (vars, dump_variable, NULL);
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
  hashval_t hash = VARIABLE_HASH_VAL (var->decl);

  if (emit_notes)
    {
      variable *slot;

      slot = (variable *) htab_find_slot_with_hash (changed_variables,
						    var->decl, hash, INSERT);

      if (set && var->n_var_parts == 0)
	{
	  variable empty_var;

	  empty_var = (variable) pool_alloc (var_pool);
	  empty_var->decl = var->decl;
	  empty_var->refcount = 1;
	  empty_var->n_var_parts = 0;
	  *slot = empty_var;
	  goto drop_var;
	}
      else
	{
	  var->refcount++;
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
	  slot = shared_hash_find_slot_noinsert (set->vars, var->decl);
	  if (slot)
	    {
	      if (shared_hash_shared (set->vars))
		slot = shared_hash_find_slot_unshare (&set->vars, var->decl,
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

/* Set the part of variable's location in the dataflow set SET.  The variable
   part is specified by variable's declaration DECL and offset OFFSET and the
   part's location by LOC.  */

static void
set_variable_part (dataflow_set *set, rtx loc, tree decl, HOST_WIDE_INT offset,
		   enum var_init_status initialized, rtx set_src)
{
  int pos;
  location_chain node, next;
  location_chain *nextp;
  variable var;
  void **slot = shared_hash_find_slot (set->vars, decl);

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  if (!slot || !*slot)
    {
      if (!slot)
	slot = shared_hash_find_slot_unshare (&set->vars, decl, INSERT);
      /* Create new variable information.  */
      var = (variable) pool_alloc (var_pool);
      var->decl = decl;
      var->refcount = 1;
      var->n_var_parts = 1;
      var->var_part[0].offset = offset;
      var->var_part[0].loc_chain = NULL;
      var->var_part[0].cur_loc = NULL;
      *slot = var;
      pos = 0;
    }
  else
    {
      int inspos = 0;

      var = (variable) *slot;

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

	      return;
	    }
	  else
	    {
	      /* We have to make a copy of a shared variable.  */
	      if (var->refcount > 1 || shared_hash_shared (set->vars))
		var = unshare_variable (set, var, initialized);
	    }
	}
      else
	{
	  /* We have not found the location part, new one will be created.  */

	  /* We have to make a copy of the shared variable.  */
	  if (var->refcount > 1 || shared_hash_shared (set->vars))
	    var = unshare_variable (set, var, initialized);

	  /* We track only variables whose size is <= MAX_VAR_PARTS bytes
	     thus there are at most MAX_VAR_PARTS different offsets.  */
	  gcc_assert (var->n_var_parts < MAX_VAR_PARTS);

	  /* We have to move the elements of array starting at index
	     inspos to the next position.  */
	  for (pos = var->n_var_parts; pos > inspos; pos--)
	    var->var_part[pos] = var->var_part[pos - 1];

	  var->n_var_parts++;
	  var->var_part[pos].offset = offset;
	  var->var_part[pos].loc_chain = NULL;
	  var->var_part[pos].cur_loc = NULL;
	}
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
	  pool_free (loc_chain_pool, node);
	  *nextp = next;
	  break;
	}
      else
	nextp = &node->next;
    }

  /* Add the location to the beginning.  */
  node = (location_chain) pool_alloc (loc_chain_pool);
  node->loc = loc;
  node->init = initialized;
  node->set_src = set_src;
  node->next = var->var_part[pos].loc_chain;
  var->var_part[pos].loc_chain = node;

  /* If no location was emitted do so.  */
  if (var->var_part[pos].cur_loc == NULL)
    {
      var->var_part[pos].cur_loc = loc;
      variable_was_changed (var, set);
    }
}

/* Remove all recorded register locations for the given variable part
   from dataflow set SET, except for those that are identical to loc.
   The variable part is specified by variable's declaration DECL and
   offset OFFSET.  */

static void
clobber_variable_part (dataflow_set *set, rtx loc, tree decl,
		       HOST_WIDE_INT offset, rtx set_src)
{
  variable var;

  if (! decl || ! DECL_P (decl))
    return;

  var = shared_hash_find (set->vars, decl);
  if (var)
    {
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
			  if (anode->decl == decl
			      && anode->offset == offset)
			    {
			      pool_free (attrs_pool, anode);
			      *anextp = anext;
			    }
			  else
			    anextp = &anode->next;
			}
		    }

		  delete_variable_part (set, node->loc, decl, offset);
		}
	    }
	}
    }
}

/* Delete the part of variable's location from dataflow set SET.  The variable
   part is specified by variable's declaration DECL and offset OFFSET and the
   part's location by LOC.  */

static void
delete_variable_part (dataflow_set *set, rtx loc, tree decl,
		      HOST_WIDE_INT offset)
{
  variable var = shared_hash_find (set->vars, decl);;
  if (var)
    {
      int pos = find_variable_location_part (var, offset, NULL);

      if (pos >= 0)
	{
	  location_chain node, next;
	  location_chain *nextp;
	  bool changed;

	  if (var->refcount > 1 || shared_hash_shared (set->vars))
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
		      var = unshare_variable (set, var,
					      VAR_INIT_STATUS_UNKNOWN);
		      break;
		    }
		}
	    }

	  /* Delete the location part.  */
	  nextp = &var->var_part[pos].loc_chain;
	  for (node = *nextp; node; node = next)
	    {
	      next = node->next;
	      if ((REG_P (node->loc) && REG_P (loc)
		   && REGNO (node->loc) == REGNO (loc))
		  || rtx_equal_p (node->loc, loc))
		{
		  pool_free (loc_chain_pool, node);
		  *nextp = next;
		  break;
		}
	      else
		nextp = &node->next;
	    }

	  /* If we have deleted the location which was last emitted
	     we have to emit new location so add the variable to set
	     of changed variables.  */
	  if (var->var_part[pos].cur_loc
	      && ((REG_P (loc)
		   && REG_P (var->var_part[pos].cur_loc)
		   && REGNO (loc) == REGNO (var->var_part[pos].cur_loc))
		  || rtx_equal_p (loc, var->var_part[pos].cur_loc)))
	    {
	      changed = true;
	      if (var->var_part[pos].loc_chain)
		var->var_part[pos].cur_loc = var->var_part[pos].loc_chain->loc;
	    }
	  else
	    changed = false;

	  if (var->var_part[pos].loc_chain == NULL)
	    {
	      var->n_var_parts--;
	      while (pos < var->n_var_parts)
		{
		  var->var_part[pos] = var->var_part[pos + 1];
		  pos++;
		}
	    }
	  if (changed)
	    variable_was_changed (var, set);
	}
    }
}

/* Emit the NOTE_INSN_VAR_LOCATION for variable *VARP.  DATA contains
   additional parameters: WHERE specifies whether the note shall be emitted
   before of after instruction INSN.  */

static int
emit_note_insn_var_location (void **varp, void *data)
{
  variable var = *(variable *) varp;
  rtx insn = ((emit_note_data *)data)->insn;
  enum emit_note_where where = ((emit_note_data *)data)->where;
  rtx note;
  int i, j, n_var_parts;
  bool complete;
  enum var_init_status initialized = VAR_INIT_STATUS_UNINITIALIZED;
  HOST_WIDE_INT last_limit;
  tree type_size_unit;
  HOST_WIDE_INT offsets[MAX_VAR_PARTS];
  rtx loc[MAX_VAR_PARTS];

  gcc_assert (var->decl);

  complete = true;
  last_limit = 0;
  n_var_parts = 0;
  for (i = 0; i < var->n_var_parts; i++)
    {
      enum machine_mode mode, wider_mode;

      if (last_limit < var->var_part[i].offset)
	{
	  complete = false;
	  break;
	}
      else if (last_limit > var->var_part[i].offset)
	continue;
      offsets[n_var_parts] = var->var_part[i].offset;
      loc[n_var_parts] = var->var_part[i].loc_chain->loc;
      mode = GET_MODE (loc[n_var_parts]);
      initialized = var->var_part[i].loc_chain->init;
      last_limit = offsets[n_var_parts] + GET_MODE_SIZE (mode);

      /* Attempt to merge adjacent registers or memory.  */
      wider_mode = GET_MODE_WIDER_MODE (mode);
      for (j = i + 1; j < var->n_var_parts; j++)
	if (last_limit <= var->var_part[j].offset)
	  break;
      if (j < var->n_var_parts
	  && wider_mode != VOIDmode
	  && GET_CODE (loc[n_var_parts])
	     == GET_CODE (var->var_part[j].loc_chain->loc)
	  && mode == GET_MODE (var->var_part[j].loc_chain->loc)
	  && last_limit == var->var_part[j].offset)
	{
	  rtx new_loc = NULL;
	  rtx loc2 = var->var_part[j].loc_chain->loc;

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
  type_size_unit = TYPE_SIZE_UNIT (TREE_TYPE (var->decl));
  if ((unsigned HOST_WIDE_INT) last_limit < TREE_INT_CST_LOW (type_size_unit))
    complete = false;

  if (where == EMIT_NOTE_AFTER_INSN)
    note = emit_note_after (NOTE_INSN_VAR_LOCATION, insn);
  else
    note = emit_note_before (NOTE_INSN_VAR_LOCATION, insn);

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  if (!complete)
    {
      NOTE_VAR_LOCATION (note) = gen_rtx_VAR_LOCATION (VOIDmode, var->decl,
						       NULL_RTX, (int) initialized);
    }
  else if (n_var_parts == 1)
    {
      rtx expr_list
	= gen_rtx_EXPR_LIST (VOIDmode, loc[0], GEN_INT (offsets[0]));

      NOTE_VAR_LOCATION (note) = gen_rtx_VAR_LOCATION (VOIDmode, var->decl,
						       expr_list, 
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
      NOTE_VAR_LOCATION (note) = gen_rtx_VAR_LOCATION (VOIDmode, var->decl,
						       parallel, 
						       (int) initialized);
    }

  htab_clear_slot (changed_variables, varp);

  /* Continue traversing the hash table.  */
  return 1;
}

/* Emit NOTE_INSN_VAR_LOCATION note for each variable from a chain
   CHANGED_VARIABLES and delete this chain.  WHERE specifies whether the notes
   shall be emitted before of after instruction INSN.  */

static void
emit_notes_for_changes (rtx insn, enum emit_note_where where)
{
  emit_note_data data;

  data.insn = insn;
  data.where = where;
  htab_traverse (changed_variables, emit_note_insn_var_location, &data);
}

/* Add variable *SLOT to the chain CHANGED_VARIABLES if it differs from the
   same variable in hash table DATA or is not there at all.  */

static int
emit_notes_for_differences_1 (void **slot, void *data)
{
  htab_t new_vars = (htab_t) data;
  variable old_var, new_var;

  old_var = *(variable *) slot;
  new_var = (variable) htab_find_with_hash (new_vars, old_var->decl,
				 VARIABLE_HASH_VAL (old_var->decl));

  if (!new_var)
    {
      /* Variable has disappeared.  */
      variable empty_var;

      empty_var = (variable) pool_alloc (var_pool);
      empty_var->decl = old_var->decl;
      empty_var->refcount = 0;
      empty_var->n_var_parts = 0;
      variable_was_changed (empty_var, NULL);
    }
  else if (variable_different_p (old_var, new_var, true))
    {
      variable_was_changed (new_var, NULL);
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

  new_var = *(variable *) slot;
  old_var = (variable) htab_find_with_hash (old_vars, new_var->decl,
				 VARIABLE_HASH_VAL (new_var->decl));
  if (!old_var)
    {
      /* Variable has appeared.  */
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
  emit_notes_for_changes (insn, EMIT_NOTE_BEFORE_INSN);
}

/* Emit the notes for changes of location parts in the basic block BB.  */

static void
emit_notes_in_bb (basic_block bb)
{
  int i;
  dataflow_set set;

  dataflow_set_init (&set);
  dataflow_set_copy (&set, &VTI (bb)->in);

  for (i = 0; i < VTI (bb)->n_mos; i++)
    {
      rtx insn = VTI (bb)->mos[i].insn;

      switch (VTI (bb)->mos[i].type)
	{
	  case MO_CALL:
	    {
	      int r;

	      for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
		if (TEST_HARD_REG_BIT (call_used_reg_set, r))
		  {
		    var_regno_delete (&set, r);
		  }
	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN);
	    }
	    break;

	  case MO_USE:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;

	      if (REG_P (loc))
		var_reg_set (&set, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	      else
		var_mem_set (&set, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN);
	    }
	    break;

	  case MO_SET:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (REG_P (loc))
		var_reg_delete_and_set (&set, loc, true, VAR_INIT_STATUS_INITIALIZED, 
					set_src);
	      else
		var_mem_delete_and_set (&set, loc, true, VAR_INIT_STATUS_INITIALIZED, 
					set_src);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN);
	    }
	    break;

	  case MO_COPY:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;
	      enum var_init_status src_status;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      src_status = find_src_status (&set, set_src);
	      set_src = find_src_set_src (&set, set_src);

	      if (REG_P (loc))
		var_reg_delete_and_set (&set, loc, false, src_status, set_src);
	      else
		var_mem_delete_and_set (&set, loc, false, src_status, set_src);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN);
	    }
	    break;

	  case MO_USE_NO_VAR:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;

	      if (REG_P (loc))
		var_reg_delete (&set, loc, false);
	      else
		var_mem_delete (&set, loc, false);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN);
	    }
	    break;

	  case MO_CLOBBER:
	    {
	      rtx loc = VTI (bb)->mos[i].u.loc;

	      if (REG_P (loc))
		var_reg_delete (&set, loc, true);
	      else
		var_mem_delete (&set, loc, true);

	      emit_notes_for_changes (NEXT_INSN (insn), EMIT_NOTE_BEFORE_INSN);
	    }
	    break;

	  case MO_ADJUST:
	    set.stack_adjust += VTI (bb)->mos[i].u.adjust;
	    break;
	}
    }
  dataflow_set_destroy (&set);
}

/* Emit notes for the whole function.  */

static void
vt_emit_notes (void)
{
  basic_block bb;
  dataflow_set *last_out;
  dataflow_set empty;

  gcc_assert (!htab_elements (changed_variables));

  /* Enable emitting notes by functions (mainly by set_variable_part and
     delete_variable_part).  */
  emit_notes = true;

  dataflow_set_init (&empty);
  last_out = &empty;

  FOR_EACH_BB (bb)
    {
      /* Emit the notes for changes of variable locations between two
	 subsequent basic blocks.  */
      emit_notes_for_differences (BB_HEAD (bb), last_out, &VTI (bb)->in);

      /* Emit the notes for the changes in the basic block itself.  */
      emit_notes_in_bb (bb);

      last_out = &VTI (bb)->out;
    }
  dataflow_set_destroy (&empty);
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
	  if (!vt_get_decl_and_offset (decl_rtl, &decl, &offset))
	    continue;
	  offset += byte_lowpart_offset (GET_MODE (incoming),
					 GET_MODE (decl_rtl));
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

      if (REG_P (incoming))
	{
	  incoming = var_lowpart (mode, incoming);
	  gcc_assert (REGNO (incoming) < FIRST_PSEUDO_REGISTER);
	  attrs_list_insert (&out->regs[REGNO (incoming)],
			     parm, offset, incoming);
	  set_variable_part (out, incoming, parm, offset, VAR_INIT_STATUS_INITIALIZED, 
			     NULL);
	}
      else if (MEM_P (incoming))
	{
	  incoming = var_lowpart (mode, incoming);
	  set_variable_part (out, incoming, parm, offset,
			     VAR_INIT_STATUS_INITIALIZED, NULL);
	}
    }
}

/* Allocate and initialize the data structures for variable tracking
   and parse the RTL to get the micro operations.  */

static void
vt_initialize (void)
{
  basic_block bb;

  alloc_aux_for_blocks (sizeof (struct variable_tracking_info_def));

  FOR_EACH_BB (bb)
    {
      rtx insn;
      HOST_WIDE_INT pre, post = 0;

      /* Count the number of micro operations.  */
      VTI (bb)->n_mos = 0;
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  if (INSN_P (insn))
	    {
	      if (!frame_pointer_needed)
		{
		  insn_stack_adjust_offset_pre_post (insn, &pre, &post);
		  if (pre)
		    VTI (bb)->n_mos++;
		  if (post)
		    VTI (bb)->n_mos++;
		}
	      note_uses (&PATTERN (insn), count_uses_1, insn);
	      note_stores (PATTERN (insn), count_stores, insn);
	      if (CALL_P (insn))
		VTI (bb)->n_mos++;
	    }
	}

      /* Add the micro-operations to the array.  */
      VTI (bb)->mos = XNEWVEC (micro_operation, VTI (bb)->n_mos);
      VTI (bb)->n_mos = 0;
      for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	   insn = NEXT_INSN (insn))
	{
	  if (INSN_P (insn))
	    {
	      int n1, n2;

	      if (!frame_pointer_needed)
		{
		  insn_stack_adjust_offset_pre_post (insn, &pre, &post);
		  if (pre)
		    {
		      micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

		      mo->type = MO_ADJUST;
		      mo->u.adjust = pre;
		      mo->insn = insn;
		    }
		}

	      n1 = VTI (bb)->n_mos;
	      note_uses (&PATTERN (insn), add_uses_1, insn);
	      n2 = VTI (bb)->n_mos - 1;

	      /* Order the MO_USEs to be before MO_USE_NO_VARs.  */
	      while (n1 < n2)
		{
		  while (n1 < n2 && VTI (bb)->mos[n1].type == MO_USE)
		    n1++;
		  while (n1 < n2 && VTI (bb)->mos[n2].type == MO_USE_NO_VAR)
		    n2--;
		  if (n1 < n2)
		    {
		      micro_operation sw;

		      sw = VTI (bb)->mos[n1];
		      VTI (bb)->mos[n1] = VTI (bb)->mos[n2];
		      VTI (bb)->mos[n2] = sw;
		    }
		}

	      if (CALL_P (insn))
		{
		  micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

		  mo->type = MO_CALL;
		  mo->insn = insn;
		}

	      n1 = VTI (bb)->n_mos;
	      /* This will record NEXT_INSN (insn), such that we can
		 insert notes before it without worrying about any
		 notes that MO_USEs might emit after the insn.  */
	      note_stores (PATTERN (insn), add_stores, insn);
	      n2 = VTI (bb)->n_mos - 1;

	      /* Order the MO_CLOBBERs to be before MO_SETs.  */
	      while (n1 < n2)
		{
		  while (n1 < n2 && VTI (bb)->mos[n1].type == MO_CLOBBER)
		    n1++;
		  while (n1 < n2 && (VTI (bb)->mos[n2].type == MO_SET
				     || VTI (bb)->mos[n2].type == MO_COPY))
		    n2--;
		  if (n1 < n2)
		    {
		      micro_operation sw;

		      sw = VTI (bb)->mos[n1];
		      VTI (bb)->mos[n1] = VTI (bb)->mos[n2];
		      VTI (bb)->mos[n2] = sw;
		    }
		}

	      if (!frame_pointer_needed && post)
		{
		  micro_operation *mo = VTI (bb)->mos + VTI (bb)->n_mos++;

		  mo->type = MO_ADJUST;
		  mo->u.adjust = post;
		  mo->insn = insn;
		}
	    }
	}
    }

  attrs_pool = create_alloc_pool ("attrs_def pool",
				  sizeof (struct attrs_def), 1024);
  var_pool = create_alloc_pool ("variable_def pool",
				sizeof (struct variable_def), 64);
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

  /* Init the IN and OUT sets.  */
  FOR_ALL_BB (bb)
    {
      VTI (bb)->visited = false;
      dataflow_set_init (&VTI (bb)->in);
      dataflow_set_init (&VTI (bb)->out);
    }

  vt_add_function_parameters ();
}

/* Free the data structures needed for variable tracking.  */

static void
vt_finalize (void)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      free (VTI (bb)->mos);
    }

  FOR_ALL_BB (bb)
    {
      dataflow_set_destroy (&VTI (bb)->in);
      dataflow_set_destroy (&VTI (bb)->out);
    }
  free_aux_for_blocks ();
  htab_delete (empty_shared_hash->htab);
  htab_delete (changed_variables);
  free_alloc_pool (attrs_pool);
  free_alloc_pool (var_pool);
  free_alloc_pool (loc_chain_pool);
  free_alloc_pool (shared_hash_pool);
  if (vui_vec)
    free (vui_vec);
  vui_vec = NULL;
  vui_allocated = 0;
}

/* The entry point to variable tracking pass.  */

unsigned int
variable_tracking_main (void)
{
  if (n_basic_blocks > 500 && n_edges / n_basic_blocks >= 20)
    return 0;

  mark_dfs_back_edges ();
  vt_initialize ();
  if (!frame_pointer_needed)
    {
      if (!vt_stack_adjustments ())
	{
	  vt_finalize ();
	  return 0;
	}
    }

  vt_find_locations ();
  vt_emit_notes ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_dataflow_sets ();
      dump_flow_info (dump_file, dump_flags);
    }

  vt_finalize ();
  return 0;
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
