/* SCC value numbering for trees
   Copyright (C) 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-dump.h"
#include "timevar.h"
#include "fibheap.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "real.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "flags.h"
#include "bitmap.h"
#include "langhooks.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-sccvn.h"

/* This algorithm is based on the SCC algorithm presented by Keith
   Cooper and L. Taylor Simpson in "SCC-Based Value numbering"
   (http://citeseer.ist.psu.edu/41805.html).  In
   straight line code, it is equivalent to a regular hash based value
   numbering that is performed in reverse postorder.

   For code with cycles, there are two alternatives, both of which
   require keeping the hashtables separate from the actual list of
   value numbers for SSA names.

   1. Iterate value numbering in an RPO walk of the blocks, removing
   all the entries from the hashtable after each iteration (but
   keeping the SSA name->value number mapping between iterations).
   Iterate until it does not change.

   2. Perform value numbering as part of an SCC walk on the SSA graph,
   iterating only the cycles in the SSA graph until they do not change
   (using a separate, optimistic hashtable for value numbering the SCC
   operands).

   The second is not just faster in practice (because most SSA graph
   cycles do not involve all the variables in the graph), it also has
   some nice properties.

   One of these nice properties is that when we pop an SCC off the
   stack, we are guaranteed to have processed all the operands coming from
   *outside of that SCC*, so we do not need to do anything special to
   ensure they have value numbers.

   Another nice property is that the SCC walk is done as part of a DFS
   of the SSA graph, which makes it easy to perform combining and
   simplifying operations at the same time.

   The code below is deliberately written in a way that makes it easy
   to separate the SCC walk from the other work it does.

   In order to propagate constants through the code, we track which
   expressions contain constants, and use those while folding.  In
   theory, we could also track expressions whose value numbers are
   replaced, in case we end up folding based on expression
   identities.

   In order to value number memory, we assign value numbers to vuses.
   This enables us to note that, for example, stores to the same
   address of the same value from the same starting memory states are
   equivalent.
   TODO:

   1. We can iterate only the changing portions of the SCC's, but
   I have not seen an SCC big enough for this to be a win.
   2. If you differentiate between phi nodes for loops and phi nodes
   for if-then-else, you can properly consider phi nodes in different
   blocks for equivalence.
   3. We could value number vuses in more cases, particularly, whole
   structure copies.
*/

/* The set of hashtables and alloc_pool's for their items.  */

typedef struct vn_tables_s
{
  htab_t nary;
  htab_t phis;
  htab_t references;
  struct obstack nary_obstack;
  alloc_pool phis_pool;
  alloc_pool references_pool;
} *vn_tables_t;

static htab_t constant_to_value_id;
static bitmap constant_value_ids;


/* Valid hashtables storing information we have proven to be
   correct.  */

static vn_tables_t valid_info;

/* Optimistic hashtables storing information we are making assumptions about
   during iterations.  */

static vn_tables_t optimistic_info;

/* Pointer to the set of hashtables that is currently being used.
   Should always point to either the optimistic_info, or the
   valid_info.  */

static vn_tables_t current_info;


/* Reverse post order index for each basic block.  */

static int *rpo_numbers;

#define SSA_VAL(x) (VN_INFO ((x))->valnum)

/* This represents the top of the VN lattice, which is the universal
   value.  */

tree VN_TOP;

/* Unique counter for our value ids.  */

static unsigned int next_value_id;

/* Next DFS number and the stack for strongly connected component
   detection. */

static unsigned int next_dfs_num;
static VEC (tree, heap) *sccstack;

static bool may_insert;


DEF_VEC_P(vn_ssa_aux_t);
DEF_VEC_ALLOC_P(vn_ssa_aux_t, heap);

/* Table of vn_ssa_aux_t's, one per ssa_name.  The vn_ssa_aux_t objects
   are allocated on an obstack for locality reasons, and to free them
   without looping over the VEC.  */

static VEC (vn_ssa_aux_t, heap) *vn_ssa_aux_table;
static struct obstack vn_ssa_aux_obstack;

/* Return the value numbering information for a given SSA name.  */

vn_ssa_aux_t
VN_INFO (tree name)
{
  vn_ssa_aux_t res = VEC_index (vn_ssa_aux_t, vn_ssa_aux_table,
				SSA_NAME_VERSION (name));
  gcc_assert (res);
  return res;
}

/* Set the value numbering info for a given SSA name to a given
   value.  */

static inline void
VN_INFO_SET (tree name, vn_ssa_aux_t value)
{
  VEC_replace (vn_ssa_aux_t, vn_ssa_aux_table,
	       SSA_NAME_VERSION (name), value);
}

/* Initialize the value numbering info for a given SSA name.
   This should be called just once for every SSA name.  */

vn_ssa_aux_t
VN_INFO_GET (tree name)
{
  vn_ssa_aux_t newinfo;

  newinfo = XOBNEW (&vn_ssa_aux_obstack, struct vn_ssa_aux);
  memset (newinfo, 0, sizeof (struct vn_ssa_aux));
  if (SSA_NAME_VERSION (name) >= VEC_length (vn_ssa_aux_t, vn_ssa_aux_table))
    VEC_safe_grow (vn_ssa_aux_t, heap, vn_ssa_aux_table,
		   SSA_NAME_VERSION (name) + 1);
  VEC_replace (vn_ssa_aux_t, vn_ssa_aux_table,
	       SSA_NAME_VERSION (name), newinfo);
  return newinfo;
}


/* Get the representative expression for the SSA_NAME NAME.  Returns
   the representative SSA_NAME if there is no expression associated with it.  */

tree
vn_get_expr_for (tree name)
{
  vn_ssa_aux_t vn = VN_INFO (name);
  gimple def_stmt;
  tree expr = NULL_TREE;

  if (vn->valnum == VN_TOP)
    return name;

  /* If the value-number is a constant it is the representative
     expression.  */
  if (TREE_CODE (vn->valnum) != SSA_NAME)
    return vn->valnum;

  /* Get to the information of the value of this SSA_NAME.  */
  vn = VN_INFO (vn->valnum);

  /* If the value-number is a constant it is the representative
     expression.  */
  if (TREE_CODE (vn->valnum) != SSA_NAME)
    return vn->valnum;

  /* Else if we have an expression, return it.  */
  if (vn->expr != NULL_TREE)
    return vn->expr;

  /* Otherwise use the defining statement to build the expression.  */
  def_stmt = SSA_NAME_DEF_STMT (vn->valnum);

  /* If the value number is a default-definition or a PHI result
     use it directly.  */
  if (gimple_nop_p (def_stmt)
      || gimple_code (def_stmt) == GIMPLE_PHI)
    return vn->valnum;

  if (!is_gimple_assign (def_stmt))
    return vn->valnum;

  /* FIXME tuples.  This is incomplete and likely will miss some
     simplifications.  */
  switch (TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt)))
    {
    case tcc_reference:
      if ((gimple_assign_rhs_code (def_stmt) == VIEW_CONVERT_EXPR
	   || gimple_assign_rhs_code (def_stmt) == REALPART_EXPR
	   || gimple_assign_rhs_code (def_stmt) == IMAGPART_EXPR)
	  && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
	expr = fold_build1 (gimple_assign_rhs_code (def_stmt),
			    gimple_expr_type (def_stmt),
			    TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 0));
      break;

    case tcc_unary:
      expr = fold_build1 (gimple_assign_rhs_code (def_stmt),
			  gimple_expr_type (def_stmt),
			  gimple_assign_rhs1 (def_stmt));
      break;

    case tcc_binary:
      expr = fold_build2 (gimple_assign_rhs_code (def_stmt),
			  gimple_expr_type (def_stmt),
			  gimple_assign_rhs1 (def_stmt),
			  gimple_assign_rhs2 (def_stmt));
      break;

    default:;
    }
  if (expr == NULL_TREE)
    return vn->valnum;

  /* Cache the expression.  */
  vn->expr = expr;

  return expr;
}


/* Free a phi operation structure VP.  */

static void
free_phi (void *vp)
{
  vn_phi_t phi = (vn_phi_t) vp;
  VEC_free (tree, heap, phi->phiargs);
}

/* Free a reference operation structure VP.  */

static void
free_reference (void *vp)
{
  vn_reference_t vr = (vn_reference_t) vp;
  VEC_free (vn_reference_op_s, heap, vr->operands);
}

/* Hash table equality function for vn_constant_t.  */

static int
vn_constant_eq (const void *p1, const void *p2)
{
  const struct vn_constant_s *vc1 = (const struct vn_constant_s *) p1;
  const struct vn_constant_s *vc2 = (const struct vn_constant_s *) p2;

  if (vc1->hashcode != vc2->hashcode)
    return false;

  return vn_constant_eq_with_type (vc1->constant, vc2->constant);
}

/* Hash table hash function for vn_constant_t.  */
   
static hashval_t
vn_constant_hash (const void *p1)
{
  const struct vn_constant_s *vc1 = (const struct vn_constant_s *) p1;
  return vc1->hashcode;
}

/* Lookup a value id for CONSTANT and return it.  If it does not
   exist returns 0.  */

unsigned int
get_constant_value_id (tree constant)
{
  void **slot;
  struct vn_constant_s vc;

  vc.hashcode = vn_hash_constant_with_type (constant);
  vc.constant = constant;
  slot = htab_find_slot_with_hash (constant_to_value_id, &vc,
				   vc.hashcode, NO_INSERT);
  if (slot)
    return ((vn_constant_t)*slot)->value_id;
  return 0;
}

/* Lookup a value id for CONSTANT, and if it does not exist, create a
   new one and return it.  If it does exist, return it.  */

unsigned int
get_or_alloc_constant_value_id (tree constant)
{
  void **slot;
  vn_constant_t vc = XNEW (struct vn_constant_s);
  
  vc->hashcode = vn_hash_constant_with_type (constant);
  vc->constant = constant;
  slot = htab_find_slot_with_hash (constant_to_value_id, vc,
				   vc->hashcode, INSERT);  
  if (*slot)
    {
      free (vc);
      return ((vn_constant_t)*slot)->value_id;
    }
  vc->value_id = get_next_value_id ();
  *slot = vc;
  bitmap_set_bit (constant_value_ids, vc->value_id);
  return vc->value_id;
}

/* Return true if V is a value id for a constant.  */

bool
value_id_constant_p (unsigned int v)
{
  return bitmap_bit_p (constant_value_ids, v);  
}

/* Compare two reference operands P1 and P2 for equality.  Return true if
   they are equal, and false otherwise.  */

static int
vn_reference_op_eq (const void *p1, const void *p2)
{
  const_vn_reference_op_t const vro1 = (const_vn_reference_op_t) p1;
  const_vn_reference_op_t const vro2 = (const_vn_reference_op_t) p2;

  return vro1->opcode == vro2->opcode
    && types_compatible_p (vro1->type, vro2->type)
    && expressions_equal_p (vro1->op0, vro2->op0)
    && expressions_equal_p (vro1->op1, vro2->op1)
    && expressions_equal_p (vro1->op2, vro2->op2);
}

/* Compute the hash for a reference operand VRO1.  */

static hashval_t
vn_reference_op_compute_hash (const vn_reference_op_t vro1)
{
  hashval_t result = 0;
  if (vro1->op0)
    result += iterative_hash_expr (vro1->op0, vro1->opcode);
  if (vro1->op1)
    result += iterative_hash_expr (vro1->op1, vro1->opcode);
  if (vro1->op2)
    result += iterative_hash_expr (vro1->op2, vro1->opcode);
  return result;
}

/* Return the hashcode for a given reference operation P1.  */

static hashval_t
vn_reference_hash (const void *p1)
{
  const_vn_reference_t const vr1 = (const_vn_reference_t) p1;
  return vr1->hashcode;
}

/* Compute a hash for the reference operation VR1 and return it.  */

hashval_t
vn_reference_compute_hash (const vn_reference_t vr1)
{
  hashval_t result = 0;
  tree v;
  int i;
  vn_reference_op_t vro;

  for (i = 0; VEC_iterate (tree, vr1->vuses, i, v); i++)
    result += iterative_hash_expr (v, 0);
  for (i = 0; VEC_iterate (vn_reference_op_s, vr1->operands, i, vro); i++)
    result += vn_reference_op_compute_hash (vro);

  return result;
}

/* Return true if reference operations P1 and P2 are equivalent.  This
   means they have the same set of operands and vuses.  */

int
vn_reference_eq (const void *p1, const void *p2)
{
  tree v;
  int i;
  vn_reference_op_t vro;

  const_vn_reference_t const vr1 = (const_vn_reference_t) p1;
  const_vn_reference_t const vr2 = (const_vn_reference_t) p2;
  if (vr1->hashcode != vr2->hashcode)
    return false;

  if (vr1->vuses == vr2->vuses
      && vr1->operands == vr2->operands)
    return true;

  /* Impossible for them to be equivalent if they have different
     number of vuses.  */
  if (VEC_length (tree, vr1->vuses) != VEC_length (tree, vr2->vuses))
    return false;

  /* We require that address operands be canonicalized in a way that
     two memory references will have the same operands if they are
     equivalent.  */
  if (VEC_length (vn_reference_op_s, vr1->operands)
      != VEC_length (vn_reference_op_s, vr2->operands))
    return false;

  /* The memory state is more often different than the address of the
     store/load, so check it first.  */
  for (i = 0; VEC_iterate (tree, vr1->vuses, i, v); i++)
    {
      if (VEC_index (tree, vr2->vuses, i) != v)
	return false;
    }

  for (i = 0; VEC_iterate (vn_reference_op_s, vr1->operands, i, vro); i++)
    {
      if (!vn_reference_op_eq (VEC_index (vn_reference_op_s, vr2->operands, i),
			       vro))
	return false;
    }
  return true;
}

/* Place the vuses from STMT into *result.  */

static inline void
vuses_to_vec (gimple stmt, VEC (tree, gc) **result)
{
  ssa_op_iter iter;
  tree vuse;

  if (!stmt)
    return;

  VEC_reserve_exact (tree, gc, *result,
		     num_ssa_operands (stmt, SSA_OP_VIRTUAL_USES));

  FOR_EACH_SSA_TREE_OPERAND (vuse, stmt, iter, SSA_OP_VIRTUAL_USES)
    VEC_quick_push (tree, *result, vuse);
}


/* Copy the VUSE names in STMT into a vector, and return
   the vector.  */

static VEC (tree, gc) *
copy_vuses_from_stmt (gimple stmt)
{
  VEC (tree, gc) *vuses = NULL;

  vuses_to_vec (stmt, &vuses);

  return vuses;
}

/* Place the vdefs from STMT into *result.  */

static inline void
vdefs_to_vec (gimple stmt, VEC (tree, gc) **result)
{
  ssa_op_iter iter;
  tree vdef;

  if (!stmt)
    return;

  *result = VEC_alloc (tree, gc, num_ssa_operands (stmt, SSA_OP_VIRTUAL_DEFS));

  FOR_EACH_SSA_TREE_OPERAND (vdef, stmt, iter, SSA_OP_VIRTUAL_DEFS)
    VEC_quick_push (tree, *result, vdef);
}

/* Copy the names of vdef results in STMT into a vector, and return
   the vector.  */

static VEC (tree, gc) *
copy_vdefs_from_stmt (gimple stmt)
{
  VEC (tree, gc) *vdefs = NULL;

  vdefs_to_vec (stmt, &vdefs);

  return vdefs;
}

/* Place for shared_v{uses/defs}_from_stmt to shove vuses/vdefs.  */
static VEC (tree, gc) *shared_lookup_vops;

/* Copy the virtual uses from STMT into SHARED_LOOKUP_VOPS.
   This function will overwrite the current SHARED_LOOKUP_VOPS
   variable.  */

VEC (tree, gc) *
shared_vuses_from_stmt (gimple stmt)
{
  VEC_truncate (tree, shared_lookup_vops, 0);
  vuses_to_vec (stmt, &shared_lookup_vops);

  return shared_lookup_vops;
}

/* Copy the operations present in load/store REF into RESULT, a vector of
   vn_reference_op_s's.  */

void
copy_reference_ops_from_ref (tree ref, VEC(vn_reference_op_s, heap) **result)
{
  if (TREE_CODE (ref) == TARGET_MEM_REF)
    {
      vn_reference_op_s temp;

      memset (&temp, 0, sizeof (temp));
      /* We do not care for spurious type qualifications.  */
      temp.type = TYPE_MAIN_VARIANT (TREE_TYPE (ref));
      temp.opcode = TREE_CODE (ref);
      temp.op0 = TMR_SYMBOL (ref) ? TMR_SYMBOL (ref) : TMR_BASE (ref);
      temp.op1 = TMR_INDEX (ref);
      VEC_safe_push (vn_reference_op_s, heap, *result, &temp);

      memset (&temp, 0, sizeof (temp));
      temp.type = NULL_TREE;
      temp.opcode = TREE_CODE (ref);
      temp.op0 = TMR_STEP (ref);
      temp.op1 = TMR_OFFSET (ref);
      VEC_safe_push (vn_reference_op_s, heap, *result, &temp);
      return;
    }

  /* For non-calls, store the information that makes up the address.  */

  while (ref)
    {
      vn_reference_op_s temp;

      memset (&temp, 0, sizeof (temp));
      /* We do not care for spurious type qualifications.  */
      temp.type = TYPE_MAIN_VARIANT (TREE_TYPE (ref));
      temp.opcode = TREE_CODE (ref);

      switch (temp.opcode)
	{
	case ALIGN_INDIRECT_REF:
	case INDIRECT_REF:
	  /* The only operand is the address, which gets its own
	     vn_reference_op_s structure.  */
	  break;
	case MISALIGNED_INDIRECT_REF:
	  temp.op0 = TREE_OPERAND (ref, 1);
	  break;
	case BIT_FIELD_REF:
	  /* Record bits and position.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  break;
	case COMPONENT_REF:
	  /* The field decl is enough to unambiguously specify the field,
	     a matching type is not necessary and a mismatching type
	     is always a spurious difference.  */
	  temp.type = NULL_TREE;
	  /* If this is a reference to a union member, record the union
	     member size as operand.  Do so only if we are doing
	     expression insertion (during FRE), as PRE currently gets
	     confused with this.  */
	  if (may_insert
	      && TREE_OPERAND (ref, 2) == NULL_TREE
	      && TREE_CODE (DECL_CONTEXT (TREE_OPERAND (ref, 1))) == UNION_TYPE
	      && integer_zerop (DECL_FIELD_OFFSET (TREE_OPERAND (ref, 1)))
	      && integer_zerop (DECL_FIELD_BIT_OFFSET (TREE_OPERAND (ref, 1))))
	    temp.op0 = TYPE_SIZE (TREE_TYPE (TREE_OPERAND (ref, 1)));
	  else
	    {
	      /* Record field as operand.  */
	      temp.op0 = TREE_OPERAND (ref, 1);
	      temp.op1 = TREE_OPERAND (ref, 2);
	    }
	  break;
	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  /* Record index as operand.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  temp.op2 = TREE_OPERAND (ref, 3);
	  break;
	case STRING_CST:
	case INTEGER_CST:
	case COMPLEX_CST:
	case VECTOR_CST:
	case REAL_CST:
	case CONSTRUCTOR:
	case VAR_DECL:
	case PARM_DECL:
	case CONST_DECL:
	case RESULT_DECL:
	case SSA_NAME:
	  temp.op0 = ref;
	  break;
	case ADDR_EXPR:
	  if (is_gimple_min_invariant (ref))
	    {
	      temp.op0 = ref;
	      break;
	    }
	  /* Fallthrough.  */
	  /* These are only interesting for their operands, their
	     existence, and their type.  They will never be the last
	     ref in the chain of references (IE they require an
	     operand), so we don't have to put anything
	     for op* as it will be handled by the iteration  */
	case IMAGPART_EXPR:
	case REALPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  break;
	default:
	  gcc_unreachable ();
	}
      VEC_safe_push (vn_reference_op_s, heap, *result, &temp);

      if (REFERENCE_CLASS_P (ref)
	  || (TREE_CODE (ref) == ADDR_EXPR
	      && !is_gimple_min_invariant (ref)))
	ref = TREE_OPERAND (ref, 0);
      else
	ref = NULL_TREE;
    }
}

/* Re-create a reference tree from the reference ops OPS.
   Returns NULL_TREE if the ops were not handled.
   This routine needs to be kept in sync with copy_reference_ops_from_ref.  */

static tree
get_ref_from_reference_ops (VEC(vn_reference_op_s, heap) *ops)
{
  vn_reference_op_t op;
  unsigned i;
  tree ref, *op0_p = &ref;

  for (i = 0; VEC_iterate (vn_reference_op_s, ops, i, op); ++i)
    {
      switch (op->opcode)
	{
	case CALL_EXPR:
	  return NULL_TREE;

	case ALIGN_INDIRECT_REF:
	case INDIRECT_REF:
	  *op0_p = build1 (op->opcode, op->type, NULL_TREE);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case MISALIGNED_INDIRECT_REF:
	  *op0_p = build2 (MISALIGNED_INDIRECT_REF, op->type,
			   NULL_TREE, op->op0);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case BIT_FIELD_REF:
	  *op0_p = build3 (BIT_FIELD_REF, op->type, NULL_TREE,
			   op->op0, op->op1);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case COMPONENT_REF:
	  *op0_p = build3 (COMPONENT_REF, TREE_TYPE (op->op0), NULL_TREE,
			   op->op0, op->op1);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  *op0_p = build4 (op->opcode, op->type, NULL_TREE,
			   op->op0, op->op1, op->op2);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case STRING_CST:
	case INTEGER_CST:
	case COMPLEX_CST:
	case VECTOR_CST:
	case REAL_CST:
	case CONSTRUCTOR:
	case VAR_DECL:
	case PARM_DECL:
	case CONST_DECL:
	case RESULT_DECL:
	case SSA_NAME:
	  *op0_p = op->op0;
	  break;

	case ADDR_EXPR:
	  if (op->op0 != NULL_TREE)
	    {
	      gcc_assert (is_gimple_min_invariant (op->op0));
	      *op0_p = op->op0;
	      break;
	    }
	  /* Fallthrough.  */
	case IMAGPART_EXPR:
	case REALPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  *op0_p = build1 (op->opcode, op->type, NULL_TREE);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	default:
	  return NULL_TREE;
	}
    }

  return ref;
}

/* Copy the operations present in load/store/call REF into RESULT, a vector of
   vn_reference_op_s's.  */

void
copy_reference_ops_from_call (gimple call,
			      VEC(vn_reference_op_s, heap) **result)
{
  vn_reference_op_s temp;
  unsigned i;

  /* Copy the type, opcode, function being called and static chain.  */
  memset (&temp, 0, sizeof (temp));
  temp.type = gimple_call_return_type (call);
  temp.opcode = CALL_EXPR;
  temp.op0 = gimple_call_fn (call);
  temp.op1 = gimple_call_chain (call);
  VEC_safe_push (vn_reference_op_s, heap, *result, &temp);

  /* Copy the call arguments.  As they can be references as well,
     just chain them together.  */
  for (i = 0; i < gimple_call_num_args (call); ++i)
    {
      tree callarg = gimple_call_arg (call, i);
      copy_reference_ops_from_ref (callarg, result);
    }
}

/* Create a vector of vn_reference_op_s structures from REF, a
   REFERENCE_CLASS_P tree.  The vector is not shared. */

static VEC(vn_reference_op_s, heap) *
create_reference_ops_from_ref (tree ref)
{
  VEC (vn_reference_op_s, heap) *result = NULL;

  copy_reference_ops_from_ref (ref, &result);
  return result;
}

/* Create a vector of vn_reference_op_s structures from CALL, a
   call statement.  The vector is not shared.  */

static VEC(vn_reference_op_s, heap) *
create_reference_ops_from_call (gimple call)
{
  VEC (vn_reference_op_s, heap) *result = NULL;

  copy_reference_ops_from_call (call, &result);
  return result;
}

static VEC(vn_reference_op_s, heap) *shared_lookup_references;

/* Create a vector of vn_reference_op_s structures from REF, a
   REFERENCE_CLASS_P tree.  The vector is shared among all callers of
   this function.  */

static VEC(vn_reference_op_s, heap) *
shared_reference_ops_from_ref (tree ref)
{
  if (!ref)
    return NULL;
  VEC_truncate (vn_reference_op_s, shared_lookup_references, 0);
  copy_reference_ops_from_ref (ref, &shared_lookup_references);
  return shared_lookup_references;
}

/* Create a vector of vn_reference_op_s structures from CALL, a
   call statement.  The vector is shared among all callers of
   this function.  */

static VEC(vn_reference_op_s, heap) *
shared_reference_ops_from_call (gimple call)
{
  if (!call)
    return NULL;
  VEC_truncate (vn_reference_op_s, shared_lookup_references, 0);
  copy_reference_ops_from_call (call, &shared_lookup_references);
  return shared_lookup_references;
}


/* Transform any SSA_NAME's in a vector of vn_reference_op_s
   structures into their value numbers.  This is done in-place, and
   the vector passed in is returned.  */

static VEC (vn_reference_op_s, heap) *
valueize_refs (VEC (vn_reference_op_s, heap) *orig)
{
  vn_reference_op_t vro;
  int i;

  for (i = 0; VEC_iterate (vn_reference_op_s, orig, i, vro); i++)
    {
      if (vro->opcode == SSA_NAME
	  || (vro->op0 && TREE_CODE (vro->op0) == SSA_NAME))
	{
	  vro->op0 = SSA_VAL (vro->op0);
	  /* If it transforms from an SSA_NAME to a constant, update
	     the opcode.  */
	  if (TREE_CODE (vro->op0) != SSA_NAME && vro->opcode == SSA_NAME)
	    vro->opcode = TREE_CODE (vro->op0);
	}
      /* TODO: Do we want to valueize op2 and op1 of
	 ARRAY_REF/COMPONENT_REF for Ada */
      
    }

  return orig;
}

/* Transform any SSA_NAME's in ORIG, a vector of vuse trees, into
   their value numbers. This is done in-place, and the vector passed
   in is returned.  */

static VEC (tree, gc) *
valueize_vuses (VEC (tree, gc) *orig)
{
  bool made_replacement = false;
  tree vuse;
  int i;

  for (i = 0; VEC_iterate (tree, orig, i, vuse); i++)
    {
      if (vuse != SSA_VAL (vuse))
	{
	  made_replacement = true;
	  VEC_replace (tree, orig, i, SSA_VAL (vuse));
	}
    }

  if (made_replacement && VEC_length (tree, orig) > 1)
    sort_vuses (orig);

  return orig;
}

/* Return the single reference statement defining all virtual uses
   in VUSES or NULL_TREE, if there are multiple defining statements.
   Take into account only definitions that alias REF if following
   back-edges.  */

static gimple
get_def_ref_stmt_vuses (tree ref, VEC (tree, gc) *vuses)
{
  gimple def_stmt;
  tree vuse;
  unsigned int i;

  gcc_assert (VEC_length (tree, vuses) >= 1);

  def_stmt = SSA_NAME_DEF_STMT (VEC_index (tree, vuses, 0));
  if (gimple_code (def_stmt) == GIMPLE_PHI)
    {
      /* We can only handle lookups over PHI nodes for a single
	 virtual operand.  */
      if (VEC_length (tree, vuses) == 1)
	{
	  def_stmt = get_single_def_stmt_from_phi (ref, def_stmt);
	  goto cont;
	}
      else
	return NULL;
    }

  /* Verify each VUSE reaches the same defining stmt.  */
  for (i = 1; VEC_iterate (tree, vuses, i, vuse); ++i)
    {
      gimple tmp = SSA_NAME_DEF_STMT (vuse);
      if (tmp != def_stmt)
	return NULL;
    }

  /* Now see if the definition aliases ref, and loop until it does.  */
cont:
  while (def_stmt
	 && is_gimple_assign (def_stmt)
	 && !refs_may_alias_p (ref, gimple_get_lhs (def_stmt)))
    def_stmt = get_single_def_stmt_with_phi (ref, def_stmt);

  return def_stmt;
}

/* Lookup a SCCVN reference operation VR in the current hash table.
   Returns the resulting value number if it exists in the hash table,
   NULL_TREE otherwise.  VNRESULT will be filled in with the actual
   vn_reference_t stored in the hashtable if something is found.  */

static tree
vn_reference_lookup_1 (vn_reference_t vr, vn_reference_t *vnresult)
{
  void **slot;
  hashval_t hash;

  hash = vr->hashcode;
  slot = htab_find_slot_with_hash (current_info->references, vr,
				   hash, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->references, vr,
				     hash, NO_INSERT);
  if (slot)
    {
      if (vnresult)
	*vnresult = (vn_reference_t)*slot;
      return ((vn_reference_t)*slot)->result;
    }
  
  return NULL_TREE;
}


/* Lookup a reference operation by it's parts, in the current hash table.
   Returns the resulting value number if it exists in the hash table,
   NULL_TREE otherwise.  VNRESULT will be filled in with the actual
   vn_reference_t stored in the hashtable if something is found.  */

tree
vn_reference_lookup_pieces (VEC (tree, gc) *vuses,
			    VEC (vn_reference_op_s, heap) *operands,
			    vn_reference_t *vnresult, bool maywalk)
{
  struct vn_reference_s vr1;
  tree result;
  if (vnresult)
    *vnresult = NULL;
  
  vr1.vuses = valueize_vuses (vuses);
  vr1.operands = valueize_refs (operands);
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  result = vn_reference_lookup_1 (&vr1, vnresult);

  /* If there is a single defining statement for all virtual uses, we can
     use that, following virtual use-def chains.  */
  if (!result
      && maywalk
      && vr1.vuses
      && VEC_length (tree, vr1.vuses) >= 1)
    {
      tree ref = get_ref_from_reference_ops (operands);
      gimple def_stmt;
      if (ref
	  && (def_stmt = get_def_ref_stmt_vuses (ref, vr1.vuses))
	  && is_gimple_assign (def_stmt))
	{
	  /* We are now at an aliasing definition for the vuses we want to
	     look up.  Re-do the lookup with the vdefs for this stmt.  */
	  vdefs_to_vec (def_stmt, &vuses);
	  vr1.vuses = valueize_vuses (vuses);
	  vr1.hashcode = vn_reference_compute_hash (&vr1);
	  result = vn_reference_lookup_1 (&vr1, vnresult);
	}
    }

  return result;
}

/* Lookup OP in the current hash table, and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the structure
   was NULL..  VNRESULT will be filled in with the vn_reference_t
   stored in the hashtable if one exists.  */

tree
vn_reference_lookup (tree op, VEC (tree, gc) *vuses, bool maywalk,
		     vn_reference_t *vnresult)
{
  struct vn_reference_s vr1;
  tree result;
  gimple def_stmt;
  if (vnresult)
    *vnresult = NULL;

  vr1.vuses = valueize_vuses (vuses);
  vr1.operands = valueize_refs (shared_reference_ops_from_ref (op));
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  result = vn_reference_lookup_1 (&vr1, vnresult);

  /* If there is a single defining statement for all virtual uses, we can
     use that, following virtual use-def chains.  */
  if (!result
      && maywalk
      && vr1.vuses
      && VEC_length (tree, vr1.vuses) >= 1
      && (def_stmt = get_def_ref_stmt_vuses (op, vr1.vuses))
      && is_gimple_assign (def_stmt))
    {
      /* We are now at an aliasing definition for the vuses we want to
	 look up.  Re-do the lookup with the vdefs for this stmt.  */
      vdefs_to_vec (def_stmt, &vuses);
      vr1.vuses = valueize_vuses (vuses);
      vr1.hashcode = vn_reference_compute_hash (&vr1);
      result = vn_reference_lookup_1 (&vr1, vnresult);
    }

  return result;
}


/* Insert OP into the current hash table with a value number of
   RESULT, and return the resulting reference structure we created.  */

vn_reference_t
vn_reference_insert (tree op, tree result, VEC (tree, gc) *vuses)
{
  void **slot;
  vn_reference_t vr1;

  vr1 = (vn_reference_t) pool_alloc (current_info->references_pool);
  if (TREE_CODE (result) == SSA_NAME)
    vr1->value_id = VN_INFO (result)->value_id;
  else
    vr1->value_id = get_or_alloc_constant_value_id (result);
  vr1->vuses = valueize_vuses (vuses);
  vr1->operands = valueize_refs (create_reference_ops_from_ref (op));
  vr1->hashcode = vn_reference_compute_hash (vr1);
  vr1->result = TREE_CODE (result) == SSA_NAME ? SSA_VAL (result) : result;

  slot = htab_find_slot_with_hash (current_info->references, vr1, vr1->hashcode,
				   INSERT);

  /* Because we lookup stores using vuses, and value number failures
     using the vdefs (see visit_reference_op_store for how and why),
     it's possible that on failure we may try to insert an already
     inserted store.  This is not wrong, there is no ssa name for a
     store that we could use as a differentiator anyway.  Thus, unlike
     the other lookup functions, you cannot gcc_assert (!*slot)
     here.  */

  /* But free the old slot in case of a collision.  */
  if (*slot)
    free_reference (*slot);

  *slot = vr1;
  return vr1;
}

/* Insert a reference by it's pieces into the current hash table with
   a value number of RESULT.  Return the resulting reference
   structure we created.  */

vn_reference_t
vn_reference_insert_pieces (VEC (tree, gc) *vuses,
			    VEC (vn_reference_op_s, heap) *operands,
			    tree result, unsigned int value_id)

{
  void **slot;
  vn_reference_t vr1;

  vr1 = (vn_reference_t) pool_alloc (current_info->references_pool);
  vr1->value_id =  value_id;
  vr1->vuses = valueize_vuses (vuses);
  vr1->operands = valueize_refs (operands);
  vr1->hashcode = vn_reference_compute_hash (vr1);
  if (result && TREE_CODE (result) == SSA_NAME)
    result = SSA_VAL (result);
  vr1->result = result;

  slot = htab_find_slot_with_hash (current_info->references, vr1, vr1->hashcode,
				   INSERT);
  
  /* At this point we should have all the things inserted that we have
  seen before, and we should never try inserting something that
  already exists.  */
  gcc_assert (!*slot);
  if (*slot)
    free_reference (*slot);

  *slot = vr1;
  return vr1;
}

/* Compute and return the hash value for nary operation VBO1.  */

inline hashval_t
vn_nary_op_compute_hash (const vn_nary_op_t vno1)
{
  hashval_t hash = 0;
  unsigned i;

  for (i = 0; i < vno1->length; ++i)
    if (TREE_CODE (vno1->op[i]) == SSA_NAME)
      vno1->op[i] = SSA_VAL (vno1->op[i]);

  if (vno1->length == 2
      && commutative_tree_code (vno1->opcode)
      && tree_swap_operands_p (vno1->op[0], vno1->op[1], false))
    {
      tree temp = vno1->op[0];
      vno1->op[0] = vno1->op[1];
      vno1->op[1] = temp;
    }

  for (i = 0; i < vno1->length; ++i)
    hash += iterative_hash_expr (vno1->op[i], vno1->opcode);

  return hash;
}

/* Return the computed hashcode for nary operation P1.  */

static hashval_t
vn_nary_op_hash (const void *p1)
{
  const_vn_nary_op_t const vno1 = (const_vn_nary_op_t) p1;
  return vno1->hashcode;
}

/* Compare nary operations P1 and P2 and return true if they are
   equivalent.  */

int
vn_nary_op_eq (const void *p1, const void *p2)
{
  const_vn_nary_op_t const vno1 = (const_vn_nary_op_t) p1;
  const_vn_nary_op_t const vno2 = (const_vn_nary_op_t) p2;
  unsigned i;

  if (vno1->hashcode != vno2->hashcode)
    return false;

  if (vno1->opcode != vno2->opcode
      || !types_compatible_p (vno1->type, vno2->type))
    return false;

  for (i = 0; i < vno1->length; ++i)
    if (!expressions_equal_p (vno1->op[i], vno2->op[i]))
      return false;

  return true;
}

/* Lookup a n-ary operation by its pieces and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the operation
   is NULL. VNRESULT will contain the vn_nary_op_t from the hashtable
   if it exists.  */

tree
vn_nary_op_lookup_pieces (unsigned int length, enum tree_code code,
			  tree type, tree op0, tree op1, tree op2,
			  tree op3, vn_nary_op_t *vnresult) 
{
  void **slot;
  struct vn_nary_op_s vno1;
  if (vnresult)
    *vnresult = NULL;
  vno1.opcode = code;
  vno1.length = length;
  vno1.type = type;
  vno1.op[0] = op0;
  vno1.op[1] = op1;
  vno1.op[2] = op2;
  vno1.op[3] = op3;
  vno1.hashcode = vn_nary_op_compute_hash (&vno1);
  slot = htab_find_slot_with_hash (current_info->nary, &vno1, vno1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->nary, &vno1, vno1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;
  if (vnresult)
    *vnresult = (vn_nary_op_t)*slot;
  return ((vn_nary_op_t)*slot)->result;
}

/* Lookup OP in the current hash table, and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the operation
   is NULL. VNRESULT will contain the vn_nary_op_t from the hashtable
   if it exists.  */

tree
vn_nary_op_lookup (tree op, vn_nary_op_t *vnresult)
{
  void **slot;
  struct vn_nary_op_s vno1;
  unsigned i;

  if (vnresult)
    *vnresult = NULL;
  vno1.opcode = TREE_CODE (op);
  vno1.length = TREE_CODE_LENGTH (TREE_CODE (op));
  vno1.type = TREE_TYPE (op);
  for (i = 0; i < vno1.length; ++i)
    vno1.op[i] = TREE_OPERAND (op, i);
  vno1.hashcode = vn_nary_op_compute_hash (&vno1);
  slot = htab_find_slot_with_hash (current_info->nary, &vno1, vno1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->nary, &vno1, vno1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;
  if (vnresult)
    *vnresult = (vn_nary_op_t)*slot;
  return ((vn_nary_op_t)*slot)->result;
}

/* Lookup the rhs of STMT in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table.  VNRESULT will contain the
   vn_nary_op_t from the hashtable if it exists.  */

tree
vn_nary_op_lookup_stmt (gimple stmt, vn_nary_op_t *vnresult)
{
  void **slot;
  struct vn_nary_op_s vno1;
  unsigned i;

  if (vnresult)
    *vnresult = NULL;
  vno1.opcode = gimple_assign_rhs_code (stmt);
  vno1.length = gimple_num_ops (stmt) - 1;
  vno1.type = gimple_expr_type (stmt);
  for (i = 0; i < vno1.length; ++i)
    vno1.op[i] = gimple_op (stmt, i + 1);
  if (vno1.opcode == REALPART_EXPR
      || vno1.opcode == IMAGPART_EXPR
      || vno1.opcode == VIEW_CONVERT_EXPR)
    vno1.op[0] = TREE_OPERAND (vno1.op[0], 0);
  vno1.hashcode = vn_nary_op_compute_hash (&vno1);
  slot = htab_find_slot_with_hash (current_info->nary, &vno1, vno1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->nary, &vno1, vno1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;
  if (vnresult)
    *vnresult = (vn_nary_op_t)*slot;
  return ((vn_nary_op_t)*slot)->result;
}

/* Insert a n-ary operation into the current hash table using it's
   pieces.  Return the vn_nary_op_t structure we created and put in
   the hashtable.  */

vn_nary_op_t
vn_nary_op_insert_pieces (unsigned int length, enum tree_code code,
			  tree type, tree op0,
			  tree op1, tree op2, tree op3,
			  tree result,
			  unsigned int value_id) 
{
  void **slot;
  vn_nary_op_t vno1;

  vno1 = (vn_nary_op_t) obstack_alloc (&current_info->nary_obstack,
				       (sizeof (struct vn_nary_op_s)
					- sizeof (tree) * (4 - length)));
  vno1->value_id = value_id;
  vno1->opcode = code;
  vno1->length = length;
  vno1->type = type;
  if (length >= 1)
    vno1->op[0] = op0;
  if (length >= 2)
    vno1->op[1] = op1;
  if (length >= 3)
    vno1->op[2] = op2;
  if (length >= 4)
    vno1->op[3] = op3;
  vno1->result = result;
  vno1->hashcode = vn_nary_op_compute_hash (vno1);
  slot = htab_find_slot_with_hash (current_info->nary, vno1, vno1->hashcode,
				   INSERT);
  gcc_assert (!*slot);

  *slot = vno1;
  return vno1;
  
}

/* Insert OP into the current hash table with a value number of
   RESULT.  Return the vn_nary_op_t structure we created and put in
   the hashtable.  */

vn_nary_op_t
vn_nary_op_insert (tree op, tree result)
{
  unsigned length = TREE_CODE_LENGTH (TREE_CODE (op));
  void **slot;
  vn_nary_op_t vno1;
  unsigned i;

  vno1 = (vn_nary_op_t) obstack_alloc (&current_info->nary_obstack,
			(sizeof (struct vn_nary_op_s)
			 - sizeof (tree) * (4 - length)));
  vno1->value_id = VN_INFO (result)->value_id;
  vno1->opcode = TREE_CODE (op);
  vno1->length = length;
  vno1->type = TREE_TYPE (op);
  for (i = 0; i < vno1->length; ++i)
    vno1->op[i] = TREE_OPERAND (op, i);
  vno1->result = result;
  vno1->hashcode = vn_nary_op_compute_hash (vno1);
  slot = htab_find_slot_with_hash (current_info->nary, vno1, vno1->hashcode,
				   INSERT);
  gcc_assert (!*slot);

  *slot = vno1;
  return vno1;
}

/* Insert the rhs of STMT into the current hash table with a value number of
   RESULT.  */

vn_nary_op_t
vn_nary_op_insert_stmt (gimple stmt, tree result)
{
  unsigned length = gimple_num_ops (stmt) - 1;
  void **slot;
  vn_nary_op_t vno1;
  unsigned i;

  vno1 = (vn_nary_op_t) obstack_alloc (&current_info->nary_obstack,
				       (sizeof (struct vn_nary_op_s)
					- sizeof (tree) * (4 - length)));
  vno1->value_id = VN_INFO (result)->value_id;
  vno1->opcode = gimple_assign_rhs_code (stmt);
  vno1->length = length;
  vno1->type = gimple_expr_type (stmt);
  for (i = 0; i < vno1->length; ++i)
    vno1->op[i] = gimple_op (stmt, i + 1);
  if (vno1->opcode == REALPART_EXPR
      || vno1->opcode == IMAGPART_EXPR
      || vno1->opcode == VIEW_CONVERT_EXPR)
    vno1->op[0] = TREE_OPERAND (vno1->op[0], 0);
  vno1->result = result;
  vno1->hashcode = vn_nary_op_compute_hash (vno1);
  slot = htab_find_slot_with_hash (current_info->nary, vno1, vno1->hashcode,
				   INSERT);
  gcc_assert (!*slot);

  *slot = vno1;
  return vno1;
}

/* Compute a hashcode for PHI operation VP1 and return it.  */

static inline hashval_t
vn_phi_compute_hash (vn_phi_t vp1)
{
  hashval_t result = 0;
  int i;
  tree phi1op;
  tree type;

  result = vp1->block->index;

  /* If all PHI arguments are constants we need to distinguish
     the PHI node via its type.  */
  type = TREE_TYPE (VEC_index (tree, vp1->phiargs, 0));
  result += (INTEGRAL_TYPE_P (type)
	     + (INTEGRAL_TYPE_P (type)
		? TYPE_PRECISION (type) + TYPE_UNSIGNED (type) : 0));

  for (i = 0; VEC_iterate (tree, vp1->phiargs, i, phi1op); i++)
    {
      if (phi1op == VN_TOP)
	continue;
      result += iterative_hash_expr (phi1op, result);
    }

  return result;
}

/* Return the computed hashcode for phi operation P1.  */

static hashval_t
vn_phi_hash (const void *p1)
{
  const_vn_phi_t const vp1 = (const_vn_phi_t) p1;
  return vp1->hashcode;
}

/* Compare two phi entries for equality, ignoring VN_TOP arguments.  */

static int
vn_phi_eq (const void *p1, const void *p2)
{
  const_vn_phi_t const vp1 = (const_vn_phi_t) p1;
  const_vn_phi_t const vp2 = (const_vn_phi_t) p2;

  if (vp1->hashcode != vp2->hashcode)
    return false;

  if (vp1->block == vp2->block)
    {
      int i;
      tree phi1op;

      /* If the PHI nodes do not have compatible types
	 they are not the same.  */
      if (!types_compatible_p (TREE_TYPE (VEC_index (tree, vp1->phiargs, 0)),
			       TREE_TYPE (VEC_index (tree, vp2->phiargs, 0))))
	return false;

      /* Any phi in the same block will have it's arguments in the
	 same edge order, because of how we store phi nodes.  */
      for (i = 0; VEC_iterate (tree, vp1->phiargs, i, phi1op); i++)
	{
	  tree phi2op = VEC_index (tree, vp2->phiargs, i);
	  if (phi1op == VN_TOP || phi2op == VN_TOP)
	    continue;
	  if (!expressions_equal_p (phi1op, phi2op))
	    return false;
	}
      return true;
    }
  return false;
}

static VEC(tree, heap) *shared_lookup_phiargs;

/* Lookup PHI in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

static tree
vn_phi_lookup (gimple phi)
{
  void **slot;
  struct vn_phi_s vp1;
  unsigned i;

  VEC_truncate (tree, shared_lookup_phiargs, 0);

  /* Canonicalize the SSA_NAME's to their value number.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      VEC_safe_push (tree, heap, shared_lookup_phiargs, def);
    }
  vp1.phiargs = shared_lookup_phiargs;
  vp1.block = gimple_bb (phi);
  vp1.hashcode = vn_phi_compute_hash (&vp1);
  slot = htab_find_slot_with_hash (current_info->phis, &vp1, vp1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->phis, &vp1, vp1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;
  return ((vn_phi_t)*slot)->result;
}

/* Insert PHI into the current hash table with a value number of
   RESULT.  */

static vn_phi_t
vn_phi_insert (gimple phi, tree result)
{
  void **slot;
  vn_phi_t vp1 = (vn_phi_t) pool_alloc (current_info->phis_pool);
  unsigned i;
  VEC (tree, heap) *args = NULL;

  /* Canonicalize the SSA_NAME's to their value number.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      VEC_safe_push (tree, heap, args, def);
    }
  vp1->value_id = VN_INFO (result)->value_id;
  vp1->phiargs = args;
  vp1->block = gimple_bb (phi);
  vp1->result = result;
  vp1->hashcode = vn_phi_compute_hash (vp1);

  slot = htab_find_slot_with_hash (current_info->phis, vp1, vp1->hashcode,
				   INSERT);

  /* Because we iterate over phi operations more than once, it's
     possible the slot might already exist here, hence no assert.*/
  *slot = vp1;
  return vp1;
}


/* Print set of components in strongly connected component SCC to OUT. */

static void
print_scc (FILE *out, VEC (tree, heap) *scc)
{
  tree var;
  unsigned int i;

  fprintf (out, "SCC consists of: ");
  for (i = 0; VEC_iterate (tree, scc, i, var); i++)
    {
      print_generic_expr (out, var, 0);
      fprintf (out, " ");
    }
  fprintf (out, "\n");
}

/* Set the value number of FROM to TO, return true if it has changed
   as a result.  */

static inline bool
set_ssa_val_to (tree from, tree to)
{
  tree currval;

  if (from != to
      && TREE_CODE (to) == SSA_NAME
      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (to))
    to = from;

  /* The only thing we allow as value numbers are VN_TOP, ssa_names
     and invariants.  So assert that here.  */
  gcc_assert (to != NULL_TREE
	      && (to == VN_TOP
		  || TREE_CODE (to) == SSA_NAME
		  || is_gimple_min_invariant (to)));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Setting value number of ");
      print_generic_expr (dump_file, from, 0);
      fprintf (dump_file, " to ");
      print_generic_expr (dump_file, to, 0);
    }

  currval = SSA_VAL (from);

  if (currval != to  && !operand_equal_p (currval, to, OEP_PURE_SAME))
    {
      SSA_VAL (from) = to;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " (changed)\n");
      return true;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n");
  return false;
}

/* Set all definitions in STMT to value number to themselves.
   Return true if a value number changed. */

static bool
defs_to_varying (gimple stmt)
{
  bool changed = false;
  ssa_op_iter iter;
  def_operand_p defp;

  FOR_EACH_SSA_DEF_OPERAND (defp, stmt, iter, SSA_OP_ALL_DEFS)
    {
      tree def = DEF_FROM_PTR (defp);

      VN_INFO (def)->use_processed = true;
      changed |= set_ssa_val_to (def, def);
    }
  return changed;
}

static bool expr_has_constants (tree expr);
static tree valueize_expr (tree expr);

/* Visit a copy between LHS and RHS, return true if the value number
   changed.  */

static bool
visit_copy (tree lhs, tree rhs)
{
  /* Follow chains of copies to their destination.  */
  while (TREE_CODE (rhs) == SSA_NAME
	 && SSA_VAL (rhs) != rhs)
    rhs = SSA_VAL (rhs);

  /* The copy may have a more interesting constant filled expression
     (we don't, since we know our RHS is just an SSA name).  */
  if (TREE_CODE (rhs) == SSA_NAME)
    {
      VN_INFO (lhs)->has_constants = VN_INFO (rhs)->has_constants;
      VN_INFO (lhs)->expr = VN_INFO (rhs)->expr;
    }

  return set_ssa_val_to (lhs, rhs);
}

/* Visit a unary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_unary_op (tree lhs, gimple stmt)
{
  bool changed = false;
  tree result = vn_nary_op_lookup_stmt (stmt, NULL);

  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
    }
  else
    {
      changed = set_ssa_val_to (lhs, lhs);
      vn_nary_op_insert_stmt (stmt, lhs);
    }

  return changed;
}

/* Visit a binary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_binary_op (tree lhs, gimple stmt)
{
  bool changed = false;
  tree result = vn_nary_op_lookup_stmt (stmt, NULL);

  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
    }
  else
    {
      changed = set_ssa_val_to (lhs, lhs);
      vn_nary_op_insert_stmt (stmt, lhs);
    }

  return changed;
}

/* Visit a call STMT storing into LHS.  Return true if the value number
   of the LHS has changed as a result.  */

static bool
visit_reference_op_call (tree lhs, gimple stmt)
{
  bool changed = false;
  struct vn_reference_s vr1;
  tree result;

  vr1.vuses = valueize_vuses (shared_vuses_from_stmt (stmt));
  vr1.operands = valueize_refs (shared_reference_ops_from_call (stmt));
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  result = vn_reference_lookup_1 (&vr1, NULL);
  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
      if (TREE_CODE (result) == SSA_NAME
	  && VN_INFO (result)->has_constants)
	VN_INFO (lhs)->has_constants = true;
    }
  else
    {
      void **slot;
      vn_reference_t vr2;
      changed = set_ssa_val_to (lhs, lhs);
      vr2 = (vn_reference_t) pool_alloc (current_info->references_pool);
      vr2->vuses = valueize_vuses (copy_vuses_from_stmt (stmt));
      vr2->operands = valueize_refs (create_reference_ops_from_call (stmt));
      vr2->hashcode = vr1.hashcode;
      vr2->result = lhs;
      slot = htab_find_slot_with_hash (current_info->references,
				       vr2, vr2->hashcode, INSERT);
      if (*slot)
	free_reference (*slot);
      *slot = vr2;
    }

  return changed;
}

/* Visit a load from a reference operator RHS, part of STMT, value number it,
   and return true if the value number of the LHS has changed as a result.  */

static bool
visit_reference_op_load (tree lhs, tree op, gimple stmt)
{
  bool changed = false;
  tree result = vn_reference_lookup (op, shared_vuses_from_stmt (stmt), true,
				     NULL);

  /* We handle type-punning through unions by value-numbering based
     on offset and size of the access.  Be prepared to handle a
     type-mismatch here via creating a VIEW_CONVERT_EXPR.  */
  if (result
      && !useless_type_conversion_p (TREE_TYPE (result), TREE_TYPE (op)))
    {
      /* We will be setting the value number of lhs to the value number
	 of VIEW_CONVERT_EXPR <TREE_TYPE (result)> (result).
	 So first simplify and lookup this expression to see if it
	 is already available.  */
      tree val = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (op), result);
      if ((CONVERT_EXPR_P (val)
	   || TREE_CODE (val) == VIEW_CONVERT_EXPR)
	  && TREE_CODE (TREE_OPERAND (val, 0)) == SSA_NAME)
        {
	  tree tem = valueize_expr (vn_get_expr_for (TREE_OPERAND (val, 0)));
	  if ((CONVERT_EXPR_P (tem)
	       || TREE_CODE (tem) == VIEW_CONVERT_EXPR)
	      && (tem = fold_unary_ignore_overflow (TREE_CODE (val),
						    TREE_TYPE (val), tem)))
	    val = tem;
	}
      result = val;
      if (!is_gimple_min_invariant (val)
	  && TREE_CODE (val) != SSA_NAME)
	result = vn_nary_op_lookup (val, NULL);
      /* If the expression is not yet available, value-number lhs to
	 a new SSA_NAME we create.  */
      if (!result && may_insert)
        {
	  result = make_ssa_name (SSA_NAME_VAR (lhs), NULL);
	  /* Initialize value-number information properly.  */
	  VN_INFO_GET (result)->valnum = result;
	  VN_INFO (result)->value_id = get_next_value_id ();
	  VN_INFO (result)->expr = val;
	  VN_INFO (result)->has_constants = expr_has_constants (val);
	  VN_INFO (result)->needs_insertion = true;
	  /* As all "inserted" statements are singleton SCCs, insert
	     to the valid table.  This is strictly needed to
	     avoid re-generating new value SSA_NAMEs for the same
	     expression during SCC iteration over and over (the
	     optimistic table gets cleared after each iteration).
	     We do not need to insert into the optimistic table, as
	     lookups there will fall back to the valid table.  */
	  if (current_info == optimistic_info)
	    {
	      current_info = valid_info;
	      vn_nary_op_insert (val, result);
	      current_info = optimistic_info;
	    }
	  else
	    vn_nary_op_insert (val, result);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Inserting name ");
	      print_generic_expr (dump_file, result, 0);
	      fprintf (dump_file, " for expression ");
	      print_generic_expr (dump_file, val, 0);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
      if (TREE_CODE (result) == SSA_NAME
	  && VN_INFO (result)->has_constants)
	{
	  VN_INFO (lhs)->expr = VN_INFO (result)->expr;
	  VN_INFO (lhs)->has_constants = true;
	}
    }
  else
    {
      changed = set_ssa_val_to (lhs, lhs);
      vn_reference_insert (op, lhs, copy_vuses_from_stmt (stmt));
    }

  return changed;
}


/* Visit a store to a reference operator LHS, part of STMT, value number it,
   and return true if the value number of the LHS has changed as a result.  */

static bool
visit_reference_op_store (tree lhs, tree op, gimple stmt)
{
  bool changed = false;
  tree result;
  bool resultsame = false;

  /* First we want to lookup using the *vuses* from the store and see
     if there the last store to this location with the same address
     had the same value.

     The vuses represent the memory state before the store.  If the
     memory state, address, and value of the store is the same as the
     last store to this location, then this store will produce the
     same memory state as that store.

     In this case the vdef versions for this store are value numbered to those
     vuse versions, since they represent the same memory state after
     this store.

     Otherwise, the vdefs for the store are used when inserting into
     the table, since the store generates a new memory state.  */

  result = vn_reference_lookup (lhs, shared_vuses_from_stmt (stmt), false,
				NULL);

  if (result)
    {
      if (TREE_CODE (result) == SSA_NAME)
	result = SSA_VAL (result);
      if (TREE_CODE (op) == SSA_NAME)
	op = SSA_VAL (op);
      resultsame = expressions_equal_p (result, op);
    }

  if (!result || !resultsame)
    {
      VEC(tree, gc) *vdefs = copy_vdefs_from_stmt (stmt);
      int i;
      tree vdef;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "No store match\n");
	  fprintf (dump_file, "Value numbering store ");
	  print_generic_expr (dump_file, lhs, 0);
	  fprintf (dump_file, " to ");
	  print_generic_expr (dump_file, op, 0);
	  fprintf (dump_file, "\n");
	}
      /* Have to set value numbers before insert, since insert is
	 going to valueize the references in-place.  */
      for (i = 0; VEC_iterate (tree, vdefs, i, vdef); i++)
	{
	  VN_INFO (vdef)->use_processed = true;
	  changed |= set_ssa_val_to (vdef, vdef);
	}

      /* Do not insert structure copies into the tables.  */
      if (is_gimple_min_invariant (op)
	  || is_gimple_reg (op))
        vn_reference_insert (lhs, op, vdefs);
    }
  else
    {
      /* We had a match, so value number the vdefs to have the value
	 number of the vuses they came from.  */
      ssa_op_iter op_iter;
      def_operand_p var;
      vuse_vec_p vv;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Store matched earlier value,"
		 "value numbering store vdefs to matching vuses.\n");

      FOR_EACH_SSA_VDEF_OPERAND (var, vv, stmt, op_iter)
	{
	  tree def = DEF_FROM_PTR (var);
	  tree use;

	  /* Uh, if the vuse is a multiuse, we can't really do much
	     here, sadly, since we don't know which value number of
	     which vuse to use.  */
	  if (VUSE_VECT_NUM_ELEM (*vv) != 1)
	    use = def;
	  else
	    use = VUSE_ELEMENT_VAR (*vv, 0);

	  VN_INFO (def)->use_processed = true;
	  changed |= set_ssa_val_to (def, SSA_VAL (use));
	}
    }

  return changed;
}

/* Visit and value number PHI, return true if the value number
   changed.  */

static bool
visit_phi (gimple phi)
{
  bool changed = false;
  tree result;
  tree sameval = VN_TOP;
  bool allsame = true;
  unsigned i;

  /* TODO: We could check for this in init_sccvn, and replace this
     with a gcc_assert.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
    return set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));

  /* See if all non-TOP arguments have the same value.  TOP is
     equivalent to everything, so we can ignore it.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);

      if (TREE_CODE (def) == SSA_NAME)
	def = SSA_VAL (def);
      if (def == VN_TOP)
	continue;
      if (sameval == VN_TOP)
	{
	  sameval = def;
	}
      else
	{
	  if (!expressions_equal_p (def, sameval))
	    {
	      allsame = false;
	      break;
	    }
	}
    }

  /* If all value numbered to the same value, the phi node has that
     value.  */
  if (allsame)
    {
      if (is_gimple_min_invariant (sameval))
	{
	  VN_INFO (PHI_RESULT (phi))->has_constants = true;
	  VN_INFO (PHI_RESULT (phi))->expr = sameval;
	}
      else
	{
	  VN_INFO (PHI_RESULT (phi))->has_constants = false;
	  VN_INFO (PHI_RESULT (phi))->expr = sameval;
	}

      if (TREE_CODE (sameval) == SSA_NAME)
	return visit_copy (PHI_RESULT (phi), sameval);

      return set_ssa_val_to (PHI_RESULT (phi), sameval);
    }

  /* Otherwise, see if it is equivalent to a phi node in this block.  */
  result = vn_phi_lookup (phi);
  if (result)
    {
      if (TREE_CODE (result) == SSA_NAME)
	changed = visit_copy (PHI_RESULT (phi), result);
      else
	changed = set_ssa_val_to (PHI_RESULT (phi), result);
    }
  else
    {
      vn_phi_insert (phi, PHI_RESULT (phi));
      VN_INFO (PHI_RESULT (phi))->has_constants = false;
      VN_INFO (PHI_RESULT (phi))->expr = PHI_RESULT (phi);
      changed = set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));
    }

  return changed;
}

/* Return true if EXPR contains constants.  */

static bool
expr_has_constants (tree expr)
{
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_unary:
      return is_gimple_min_invariant (TREE_OPERAND (expr, 0));

    case tcc_binary:
      return is_gimple_min_invariant (TREE_OPERAND (expr, 0))
	|| is_gimple_min_invariant (TREE_OPERAND (expr, 1));
      /* Constants inside reference ops are rarely interesting, but
	 it can take a lot of looking to find them.  */
    case tcc_reference:
    case tcc_declaration:
      return false;
    default:
      return is_gimple_min_invariant (expr);
    }
  return false;
}

/* Return true if STMT contains constants.  */

static bool
stmt_has_constants (gimple stmt)
{
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
    {
    case GIMPLE_UNARY_RHS:
      return is_gimple_min_invariant (gimple_assign_rhs1 (stmt));

    case GIMPLE_BINARY_RHS:
      return (is_gimple_min_invariant (gimple_assign_rhs1 (stmt))
	      || is_gimple_min_invariant (gimple_assign_rhs2 (stmt)));
    case GIMPLE_SINGLE_RHS:
      /* Constants inside reference ops are rarely interesting, but
	 it can take a lot of looking to find them.  */
      return is_gimple_min_invariant (gimple_assign_rhs1 (stmt));
    default:
      gcc_unreachable ();
    }
  return false;
}

/* Replace SSA_NAMES in expr with their value numbers, and return the
   result.
   This is performed in place. */

static tree
valueize_expr (tree expr)
{
  switch (TREE_CODE_CLASS (TREE_CODE (expr)))
    {
    case tcc_unary:
      if (TREE_CODE (TREE_OPERAND (expr, 0)) == SSA_NAME
	  && SSA_VAL (TREE_OPERAND (expr, 0)) != VN_TOP)
	TREE_OPERAND (expr, 0) = SSA_VAL (TREE_OPERAND (expr, 0));
      break;
    case tcc_binary:
      if (TREE_CODE (TREE_OPERAND (expr, 0)) == SSA_NAME
	  && SSA_VAL (TREE_OPERAND (expr, 0)) != VN_TOP)
	TREE_OPERAND (expr, 0) = SSA_VAL (TREE_OPERAND (expr, 0));
      if (TREE_CODE (TREE_OPERAND (expr, 1)) == SSA_NAME
	  && SSA_VAL (TREE_OPERAND (expr, 1)) != VN_TOP)
	TREE_OPERAND (expr, 1) = SSA_VAL (TREE_OPERAND (expr, 1));
      break;
    default:
      break;
    }
  return expr;
}

/* Simplify the binary expression RHS, and return the result if
   simplified. */

static tree
simplify_binary_expression (gimple stmt)
{
  tree result = NULL_TREE;
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);

  /* This will not catch every single case we could combine, but will
     catch those with constants.  The goal here is to simultaneously
     combine constants between expressions, but avoid infinite
     expansion of expressions during simplification.  */
  if (TREE_CODE (op0) == SSA_NAME)
    {
      if (VN_INFO (op0)->has_constants
	  || TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison)
	op0 = valueize_expr (vn_get_expr_for (op0));
      else if (SSA_VAL (op0) != VN_TOP && SSA_VAL (op0) != op0)
	op0 = SSA_VAL (op0);
    }

  if (TREE_CODE (op1) == SSA_NAME)
    {
      if (VN_INFO (op1)->has_constants)
	op1 = valueize_expr (vn_get_expr_for (op1));
      else if (SSA_VAL (op1) != VN_TOP && SSA_VAL (op1) != op1)
	op1 = SSA_VAL (op1);
    }

  /* Avoid folding if nothing changed.  */
  if (op0 == gimple_assign_rhs1 (stmt)
      && op1 == gimple_assign_rhs2 (stmt))
    return NULL_TREE;

  fold_defer_overflow_warnings ();

  result = fold_binary (gimple_assign_rhs_code (stmt),
		        gimple_expr_type (stmt), op0, op1);
  if (result)
    STRIP_USELESS_TYPE_CONVERSION (result);

  fold_undefer_overflow_warnings (result && valid_gimple_rhs_p (result),
				  stmt, 0);

  /* Make sure result is not a complex expression consisting
     of operators of operators (IE (a + b) + (a + c))
     Otherwise, we will end up with unbounded expressions if
     fold does anything at all.  */
  if (result && valid_gimple_rhs_p (result))
    return result;

  return NULL_TREE;
}

/* Simplify the unary expression RHS, and return the result if
   simplified. */

static tree
simplify_unary_expression (gimple stmt)
{
  tree result = NULL_TREE;
  tree orig_op0, op0 = gimple_assign_rhs1 (stmt);

  /* We handle some tcc_reference codes here that are all
     GIMPLE_ASSIGN_SINGLE codes.  */
  if (gimple_assign_rhs_code (stmt) == REALPART_EXPR
      || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR
      || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR)
    op0 = TREE_OPERAND (op0, 0);

  if (TREE_CODE (op0) != SSA_NAME)
    return NULL_TREE;

  orig_op0 = op0;
  if (VN_INFO (op0)->has_constants)
    op0 = valueize_expr (vn_get_expr_for (op0));
  else if (gimple_assign_cast_p (stmt)
	   || gimple_assign_rhs_code (stmt) == REALPART_EXPR
	   || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR
	   || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR)
    {
      /* We want to do tree-combining on conversion-like expressions.
         Make sure we feed only SSA_NAMEs or constants to fold though.  */
      tree tem = valueize_expr (vn_get_expr_for (op0));
      if (UNARY_CLASS_P (tem)
	  || BINARY_CLASS_P (tem)
	  || TREE_CODE (tem) == VIEW_CONVERT_EXPR
	  || TREE_CODE (tem) == SSA_NAME
	  || is_gimple_min_invariant (tem))
	op0 = tem;
    }

  /* Avoid folding if nothing changed, but remember the expression.  */
  if (op0 == orig_op0)
    return NULL_TREE;

  result = fold_unary_ignore_overflow (gimple_assign_rhs_code (stmt),
				       gimple_expr_type (stmt), op0);
  if (result)
    {
      STRIP_USELESS_TYPE_CONVERSION (result);
      if (valid_gimple_rhs_p (result))
        return result;
    }

  return NULL_TREE;
}

/* Try to simplify RHS using equivalences and constant folding.  */

static tree
try_to_simplify (gimple stmt)
{
  tree tem;

  /* For stores we can end up simplifying a SSA_NAME rhs.  Just return
     in this case, there is no point in doing extra work.  */
  if (gimple_assign_copy_p (stmt)
      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    return NULL_TREE;

  switch (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)))
    {
    case tcc_declaration:
      tem = get_symbol_constant_value (gimple_assign_rhs1 (stmt));
      if (tem)
	return tem;
      break;

    case tcc_reference:
      /* Do not do full-blown reference lookup here, but simplify
	 reads from constant aggregates.  */
      tem = fold_const_aggregate_ref (gimple_assign_rhs1 (stmt));
      if (tem)
	return tem;

      /* Fallthrough for some codes that can operate on registers.  */
      if (!(TREE_CODE (gimple_assign_rhs1 (stmt)) == REALPART_EXPR
	    || TREE_CODE (gimple_assign_rhs1 (stmt)) == IMAGPART_EXPR
	    || TREE_CODE (gimple_assign_rhs1 (stmt)) == VIEW_CONVERT_EXPR))
	break;
      /* We could do a little more with unary ops, if they expand
	 into binary ops, but it's debatable whether it is worth it. */
    case tcc_unary:
      return simplify_unary_expression (stmt);
      break;
    case tcc_comparison:
    case tcc_binary:
      return simplify_binary_expression (stmt);
      break;
    default:
      break;
    }

  return NULL_TREE;
}

/* Visit and value number USE, return true if the value number
   changed. */

static bool
visit_use (tree use)
{
  bool changed = false;
  gimple stmt = SSA_NAME_DEF_STMT (use);

  VN_INFO (use)->use_processed = true;

  gcc_assert (!SSA_NAME_IN_FREE_LIST (use));
  if (dump_file && (dump_flags & TDF_DETAILS)
      && !SSA_NAME_IS_DEFAULT_DEF (use))
    {
      fprintf (dump_file, "Value numbering ");
      print_generic_expr (dump_file, use, 0);
      fprintf (dump_file, " stmt = ");
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  /* Handle uninitialized uses.  */
  if (SSA_NAME_IS_DEFAULT_DEF (use))
    changed = set_ssa_val_to (use, use);
  else
    {
      if (gimple_code (stmt) == GIMPLE_PHI)
	changed = visit_phi (stmt);
      else if (!gimple_has_lhs (stmt)
	       || gimple_has_volatile_ops (stmt)
	       || stmt_could_throw_p (stmt))
	changed = defs_to_varying (stmt);
      else if (is_gimple_assign (stmt))
	{
	  tree lhs = gimple_assign_lhs (stmt);
	  tree simplified;

	  /* Shortcut for copies. Simplifying copies is pointless,
	     since we copy the expression and value they represent.  */
	  if (gimple_assign_copy_p (stmt)
	      && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	      && TREE_CODE (lhs) == SSA_NAME)
	    {
	      changed = visit_copy (lhs, gimple_assign_rhs1 (stmt));
	      goto done;
	    }
	  simplified = try_to_simplify (stmt);
	  if (simplified)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "RHS ");
		  print_gimple_expr (dump_file, stmt, 0, 0);
		  fprintf (dump_file, " simplified to ");
		  print_generic_expr (dump_file, simplified, 0);
		  if (TREE_CODE (lhs) == SSA_NAME)
		    fprintf (dump_file, " has constants %d\n",
			     expr_has_constants (simplified));
		  else
		    fprintf (dump_file, "\n");
		}
	    }
	  /* Setting value numbers to constants will occasionally
	     screw up phi congruence because constants are not
	     uniquely associated with a single ssa name that can be
	     looked up.  */
	  if (simplified
	      && is_gimple_min_invariant (simplified)
	      && TREE_CODE (lhs) == SSA_NAME)
	    {
	      VN_INFO (lhs)->expr = simplified;
	      VN_INFO (lhs)->has_constants = true;
	      changed = set_ssa_val_to (lhs, simplified);
	      goto done;
	    }
	  else if (simplified
		   && TREE_CODE (simplified) == SSA_NAME
		   && TREE_CODE (lhs) == SSA_NAME)
	    {
	      changed = visit_copy (lhs, simplified);
	      goto done;
	    }
	  else if (simplified)
	    {
	      if (TREE_CODE (lhs) == SSA_NAME)
		{
		  VN_INFO (lhs)->has_constants = expr_has_constants (simplified);
		  /* We have to unshare the expression or else
		     valuizing may change the IL stream.  */
		  VN_INFO (lhs)->expr = unshare_expr (simplified);
		}
	    }
	  else if (stmt_has_constants (stmt)
		   && TREE_CODE (lhs) == SSA_NAME)
	    VN_INFO (lhs)->has_constants = true;
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      /* We reset expr and constantness here because we may
		 have been value numbering optimistically, and
		 iterating. They may become non-constant in this case,
		 even if they were optimistically constant. */

	      VN_INFO (lhs)->has_constants = false;
	      VN_INFO (lhs)->expr = NULL_TREE;
	    }

	  if ((TREE_CODE (lhs) == SSA_NAME
	       /* We can substitute SSA_NAMEs that are live over
		  abnormal edges with their constant value.  */
	       && !(gimple_assign_copy_p (stmt)
		    && is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
	       && !(simplified
		    && is_gimple_min_invariant (simplified))
	       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	      /* Stores or copies from SSA_NAMEs that are live over
		 abnormal edges are a problem.  */
	      || (gimple_assign_single_p (stmt)
		  && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (stmt))))
	    changed = defs_to_varying (stmt);
	  else if (REFERENCE_CLASS_P (lhs) || DECL_P (lhs))
	    {
	      changed = visit_reference_op_store (lhs, gimple_assign_rhs1 (stmt), stmt);
	    }
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      if ((gimple_assign_copy_p (stmt)
		   && is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
		  || (simplified
		      && is_gimple_min_invariant (simplified)))
		{
		  VN_INFO (lhs)->has_constants = true;
		  if (simplified)
		    changed = set_ssa_val_to (lhs, simplified);
		  else
		    changed = set_ssa_val_to (lhs, gimple_assign_rhs1 (stmt));
		}
	      else
		{
		  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
		    {
		    case GIMPLE_UNARY_RHS:
		      changed = visit_unary_op (lhs, stmt);
		      break;
		    case GIMPLE_BINARY_RHS:
		      changed = visit_binary_op (lhs, stmt);
		      break;
		    case GIMPLE_SINGLE_RHS:
		      switch (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)))
			{
			case tcc_reference:
			  /* VOP-less references can go through unary case.  */
			  if ((gimple_assign_rhs_code (stmt) == REALPART_EXPR
			       || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR
			       || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR )
			      && TREE_CODE (TREE_OPERAND (gimple_assign_rhs1 (stmt), 0)) == SSA_NAME)
			    {
			      changed = visit_unary_op (lhs, stmt);
			      break;
			    }
			  /* Fallthrough.  */
			case tcc_declaration:
			  changed = visit_reference_op_load
			      (lhs, gimple_assign_rhs1 (stmt), stmt);
			  break;
			case tcc_expression:
			  if (gimple_assign_rhs_code (stmt) == ADDR_EXPR)
			    {
			      changed = visit_unary_op (lhs, stmt);
			      break;
			    }
			  /* Fallthrough.  */
			default:
			  changed = defs_to_varying (stmt);
			}
		      break;
		    default:
		      changed = defs_to_varying (stmt);
		      break;
		    }
		}
	    }
	  else
	    changed = defs_to_varying (stmt);
	}
      else if (is_gimple_call (stmt))
	{
	  tree lhs = gimple_call_lhs (stmt);

	  /* ???  We could try to simplify calls.  */

	  if (stmt_has_constants (stmt)
	      && TREE_CODE (lhs) == SSA_NAME)
	    VN_INFO (lhs)->has_constants = true;
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      /* We reset expr and constantness here because we may
		 have been value numbering optimistically, and
		 iterating. They may become non-constant in this case,
		 even if they were optimistically constant. */
	      VN_INFO (lhs)->has_constants = false;
	      VN_INFO (lhs)->expr = NULL_TREE;
	    }

	  if (TREE_CODE (lhs) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    changed = defs_to_varying (stmt);
	  /* ???  We should handle stores from calls.  */
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      if (gimple_call_flags (stmt) & (ECF_PURE | ECF_CONST))
		changed = visit_reference_op_call (lhs, stmt);
	      else
		changed = defs_to_varying (stmt);
	    }
	  else
	    changed = defs_to_varying (stmt);
	}
    }
 done:
  return changed;
}

/* Compare two operands by reverse postorder index */

static int
compare_ops (const void *pa, const void *pb)
{
  const tree opa = *((const tree *)pa);
  const tree opb = *((const tree *)pb);
  gimple opstmta = SSA_NAME_DEF_STMT (opa);
  gimple opstmtb = SSA_NAME_DEF_STMT (opb);
  basic_block bba;
  basic_block bbb;

  if (gimple_nop_p (opstmta) && gimple_nop_p (opstmtb))
    return 0;
  else if (gimple_nop_p (opstmta))
    return -1;
  else if (gimple_nop_p (opstmtb))
    return 1;

  bba = gimple_bb (opstmta);
  bbb = gimple_bb (opstmtb);

  if (!bba && !bbb)
    return 0;
  else if (!bba)
    return -1;
  else if (!bbb)
    return 1;

  if (bba == bbb)
    {
      if (gimple_code (opstmta) == GIMPLE_PHI
	  && gimple_code (opstmtb) == GIMPLE_PHI)
	return 0;
      else if (gimple_code (opstmta) == GIMPLE_PHI)
	return -1;
      else if (gimple_code (opstmtb) == GIMPLE_PHI)
	return 1;
      return gimple_uid (opstmta) - gimple_uid (opstmtb);
    }
  return rpo_numbers[bba->index] - rpo_numbers[bbb->index];
}

/* Sort an array containing members of a strongly connected component
   SCC so that the members are ordered by RPO number.
   This means that when the sort is complete, iterating through the
   array will give you the members in RPO order.  */

static void
sort_scc (VEC (tree, heap) *scc)
{
  qsort (VEC_address (tree, scc),
	 VEC_length (tree, scc),
	 sizeof (tree),
	 compare_ops);
}

/* Process a strongly connected component in the SSA graph.  */

static void
process_scc (VEC (tree, heap) *scc)
{
  /* If the SCC has a single member, just visit it.  */

  if (VEC_length (tree, scc) == 1)
    {
      tree use = VEC_index (tree, scc, 0);
      if (!VN_INFO (use)->use_processed)
	visit_use (use);
    }
  else
    {
      tree var;
      unsigned int i;
      unsigned int iterations = 0;
      bool changed = true;

      /* Iterate over the SCC with the optimistic table until it stops
	 changing.  */
      current_info = optimistic_info;
      while (changed)
	{
	  changed = false;
	  iterations++;
	  /* As we are value-numbering optimistically we have to
	     clear the expression tables and the simplified expressions
	     in each iteration until we converge.  */
	  htab_empty (optimistic_info->nary);
	  htab_empty (optimistic_info->phis);
	  htab_empty (optimistic_info->references);
	  obstack_free (&optimistic_info->nary_obstack, NULL);
	  gcc_obstack_init (&optimistic_info->nary_obstack);
	  empty_alloc_pool (optimistic_info->phis_pool);
	  empty_alloc_pool (optimistic_info->references_pool);
	  for (i = 0; VEC_iterate (tree, scc, i, var); i++)
	    VN_INFO (var)->expr = NULL_TREE;
	  for (i = 0; VEC_iterate (tree, scc, i, var); i++)
	    changed |= visit_use (var);
	}

      statistics_histogram_event (cfun, "SCC iterations", iterations);

      /* Finally, visit the SCC once using the valid table.  */
      current_info = valid_info;
      for (i = 0; VEC_iterate (tree, scc, i, var); i++)
	visit_use (var);
    }
}

DEF_VEC_O(ssa_op_iter);
DEF_VEC_ALLOC_O(ssa_op_iter,heap);

/* Pop the components of the found SCC for NAME off the SCC stack
   and process them.  Returns true if all went well, false if
   we run into resource limits.  */

static bool
extract_and_process_scc_for_name (tree name)
{
  VEC (tree, heap) *scc = NULL;
  tree x;

  /* Found an SCC, pop the components off the SCC stack and
     process them.  */
  do
    {
      x = VEC_pop (tree, sccstack);

      VN_INFO (x)->on_sccstack = false;
      VEC_safe_push (tree, heap, scc, x);
    } while (x != name);

  /* Bail out of SCCVN in case a SCC turns out to be incredibly large.  */
  if (VEC_length (tree, scc)
      > (unsigned)PARAM_VALUE (PARAM_SCCVN_MAX_SCC_SIZE))
    {
      if (dump_file)
	fprintf (dump_file, "WARNING: Giving up with SCCVN due to "
		 "SCC size %u exceeding %u\n", VEC_length (tree, scc),
		 (unsigned)PARAM_VALUE (PARAM_SCCVN_MAX_SCC_SIZE));
      return false;
    }

  if (VEC_length (tree, scc) > 1)
    sort_scc (scc);

  if (dump_file && (dump_flags & TDF_DETAILS))
    print_scc (dump_file, scc);

  process_scc (scc);

  VEC_free (tree, heap, scc);

  return true;
}

/* Depth first search on NAME to discover and process SCC's in the SSA
   graph.
   Execution of this algorithm relies on the fact that the SCC's are
   popped off the stack in topological order.
   Returns true if successful, false if we stopped processing SCC's due
   to resource constraints.  */

static bool
DFS (tree name)
{
  VEC(ssa_op_iter, heap) *itervec = NULL;
  VEC(tree, heap) *namevec = NULL;
  use_operand_p usep = NULL;
  gimple defstmt;
  tree use;
  ssa_op_iter iter;

start_over:
  /* SCC info */
  VN_INFO (name)->dfsnum = next_dfs_num++;
  VN_INFO (name)->visited = true;
  VN_INFO (name)->low = VN_INFO (name)->dfsnum;

  VEC_safe_push (tree, heap, sccstack, name);
  VN_INFO (name)->on_sccstack = true;
  defstmt = SSA_NAME_DEF_STMT (name);

  /* Recursively DFS on our operands, looking for SCC's.  */
  if (!gimple_nop_p (defstmt))
    {
      /* Push a new iterator.  */
      if (gimple_code (defstmt) == GIMPLE_PHI)
	usep = op_iter_init_phiuse (&iter, defstmt, SSA_OP_ALL_USES);
      else
	usep = op_iter_init_use (&iter, defstmt, SSA_OP_ALL_USES);
    }
  else
    clear_and_done_ssa_iter (&iter);

  while (1)
    {
      /* If we are done processing uses of a name, go up the stack
	 of iterators and process SCCs as we found them.  */
      if (op_iter_done (&iter))
	{
	  /* See if we found an SCC.  */
	  if (VN_INFO (name)->low == VN_INFO (name)->dfsnum)
	    if (!extract_and_process_scc_for_name (name))
	      {
		VEC_free (tree, heap, namevec);
		VEC_free (ssa_op_iter, heap, itervec);
		return false;
	      }

	  /* Check if we are done.  */
	  if (VEC_empty (tree, namevec))
	    {
	      VEC_free (tree, heap, namevec);
	      VEC_free (ssa_op_iter, heap, itervec);
	      return true;
	    }

	  /* Restore the last use walker and continue walking there.  */
	  use = name;
	  name = VEC_pop (tree, namevec);
	  memcpy (&iter, VEC_last (ssa_op_iter, itervec),
		  sizeof (ssa_op_iter));
	  VEC_pop (ssa_op_iter, itervec);
	  goto continue_walking;
	}

      use = USE_FROM_PTR (usep);

      /* Since we handle phi nodes, we will sometimes get
	 invariants in the use expression.  */
      if (TREE_CODE (use) == SSA_NAME)
	{
	  if (! (VN_INFO (use)->visited))
	    {
	      /* Recurse by pushing the current use walking state on
		 the stack and starting over.  */
	      VEC_safe_push(ssa_op_iter, heap, itervec, &iter);
	      VEC_safe_push(tree, heap, namevec, name);
	      name = use;
	      goto start_over;

continue_walking:
	      VN_INFO (name)->low = MIN (VN_INFO (name)->low,
					 VN_INFO (use)->low);
	    }
	  if (VN_INFO (use)->dfsnum < VN_INFO (name)->dfsnum
	      && VN_INFO (use)->on_sccstack)
	    {
	      VN_INFO (name)->low = MIN (VN_INFO (use)->dfsnum,
					 VN_INFO (name)->low);
	    }
	}

      usep = op_iter_next_use (&iter);
    }
}

/* Allocate a value number table.  */

static void
allocate_vn_table (vn_tables_t table)
{
  table->phis = htab_create (23, vn_phi_hash, vn_phi_eq, free_phi);
  table->nary = htab_create (23, vn_nary_op_hash, vn_nary_op_eq, NULL);
  table->references = htab_create (23, vn_reference_hash, vn_reference_eq,
				   free_reference);

  gcc_obstack_init (&table->nary_obstack);
  table->phis_pool = create_alloc_pool ("VN phis",
					sizeof (struct vn_phi_s),
					30);
  table->references_pool = create_alloc_pool ("VN references",
					      sizeof (struct vn_reference_s),
					      30);
}

/* Free a value number table.  */

static void
free_vn_table (vn_tables_t table)
{
  htab_delete (table->phis);
  htab_delete (table->nary);
  htab_delete (table->references);
  obstack_free (&table->nary_obstack, NULL);
  free_alloc_pool (table->phis_pool);
  free_alloc_pool (table->references_pool);
}

static void
init_scc_vn (void)
{
  size_t i;
  int j;
  int *rpo_numbers_temp;

  calculate_dominance_info (CDI_DOMINATORS);
  sccstack = NULL;
  constant_to_value_id = htab_create (23, vn_constant_hash, vn_constant_eq,
				  free);
  
  constant_value_ids = BITMAP_ALLOC (NULL);
  
  next_dfs_num = 1;
  next_value_id = 1;
  
  vn_ssa_aux_table = VEC_alloc (vn_ssa_aux_t, heap, num_ssa_names + 1);
  /* VEC_alloc doesn't actually grow it to the right size, it just
     preallocates the space to do so.  */
  VEC_safe_grow_cleared (vn_ssa_aux_t, heap, vn_ssa_aux_table, num_ssa_names + 1);
  gcc_obstack_init (&vn_ssa_aux_obstack);

  shared_lookup_phiargs = NULL;
  shared_lookup_vops = NULL;
  shared_lookup_references = NULL;
  rpo_numbers = XCNEWVEC (int, last_basic_block + NUM_FIXED_BLOCKS);
  rpo_numbers_temp = XCNEWVEC (int, last_basic_block + NUM_FIXED_BLOCKS);
  pre_and_rev_post_order_compute (NULL, rpo_numbers_temp, false);

  /* RPO numbers is an array of rpo ordering, rpo[i] = bb means that
     the i'th block in RPO order is bb.  We want to map bb's to RPO
     numbers, so we need to rearrange this array.  */
  for (j = 0; j < n_basic_blocks - NUM_FIXED_BLOCKS; j++)
    rpo_numbers[rpo_numbers_temp[j]] = j;

  XDELETE (rpo_numbers_temp);

  VN_TOP = create_tmp_var_raw (void_type_node, "vn_top");

  /* Create the VN_INFO structures, and initialize value numbers to
     TOP.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name)
	{
	  VN_INFO_GET (name)->valnum = VN_TOP;
	  VN_INFO (name)->expr = NULL_TREE;
	  VN_INFO (name)->value_id = 0;
	}
    }

  renumber_gimple_stmt_uids ();

  /* Create the valid and optimistic value numbering tables.  */
  valid_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (valid_info);
  optimistic_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (optimistic_info);
}

void
free_scc_vn (void)
{
  size_t i;

  htab_delete (constant_to_value_id);
  BITMAP_FREE (constant_value_ids);
  VEC_free (tree, heap, shared_lookup_phiargs);
  VEC_free (tree, gc, shared_lookup_vops);
  VEC_free (vn_reference_op_s, heap, shared_lookup_references);
  XDELETEVEC (rpo_numbers);

  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name
	  && VN_INFO (name)->needs_insertion)
	release_ssa_name (name);
    }
  obstack_free (&vn_ssa_aux_obstack, NULL);
  VEC_free (vn_ssa_aux_t, heap, vn_ssa_aux_table);

  VEC_free (tree, heap, sccstack);
  free_vn_table (valid_info);
  XDELETE (valid_info);
  free_vn_table (optimistic_info);
  XDELETE (optimistic_info);
}

/* Set the value ids in the valid hash tables.  */

static void
set_hashtable_value_ids (void)
{
  htab_iterator hi;
  vn_nary_op_t vno;
  vn_reference_t vr;
  vn_phi_t vp;

  /* Now set the value ids of the things we had put in the hash
     table.  */

  FOR_EACH_HTAB_ELEMENT (valid_info->nary,
			 vno, vn_nary_op_t, hi) 
    {
      if (vno->result)
	{
	  if (TREE_CODE (vno->result) == SSA_NAME)
	    vno->value_id = VN_INFO (vno->result)->value_id;
	  else if (is_gimple_min_invariant (vno->result))
	    vno->value_id = get_or_alloc_constant_value_id (vno->result);
	}
    }

  FOR_EACH_HTAB_ELEMENT (valid_info->phis,
			 vp, vn_phi_t, hi) 
    {
      if (vp->result)
	{
	  if (TREE_CODE (vp->result) == SSA_NAME)
	    vp->value_id = VN_INFO (vp->result)->value_id;
	  else if (is_gimple_min_invariant (vp->result))
	    vp->value_id = get_or_alloc_constant_value_id (vp->result);
	}
    }

  FOR_EACH_HTAB_ELEMENT (valid_info->references,
			 vr, vn_reference_t, hi) 
    {
      if (vr->result)
	{
	  if (TREE_CODE (vr->result) == SSA_NAME)
	    vr->value_id = VN_INFO (vr->result)->value_id;
	  else if (is_gimple_min_invariant (vr->result))
	    vr->value_id = get_or_alloc_constant_value_id (vr->result);
	}
    }
}

/* Do SCCVN.  Returns true if it finished, false if we bailed out
   due to resource constraints.  */

bool
run_scc_vn (bool may_insert_arg)
{
  size_t i;
  tree param;
  bool changed = true;
  
  may_insert = may_insert_arg;

  init_scc_vn ();
  current_info = valid_info;

  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = TREE_CHAIN (param))
    {
      if (gimple_default_def (cfun, param) != NULL)
	{
	  tree def = gimple_default_def (cfun, param);
	  SSA_VAL (def) = def;
	}
    }

  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      if (name
	  && VN_INFO (name)->visited == false
	  && !has_zero_uses (name))
	if (!DFS (name))
	  {
	    free_scc_vn ();
	    may_insert = false;
	    return false;
	  }
    }

  /* Initialize the value ids.  */
      
  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      vn_ssa_aux_t info;
      if (!name)
	continue;
      info = VN_INFO (name);
      if (info->valnum == name)
	info->value_id = get_next_value_id ();
      else if (is_gimple_min_invariant (info->valnum))
	info->value_id = get_or_alloc_constant_value_id (info->valnum);
    }
  
  /* Propagate until they stop changing.  */
  while (changed)
    {
      changed = false;
      for (i = 1; i < num_ssa_names; ++i)
	{
	  tree name = ssa_name (i);
	  vn_ssa_aux_t info;
	  if (!name)
	    continue;
	  info = VN_INFO (name);
	  if (TREE_CODE (info->valnum) == SSA_NAME
	      && info->valnum != name
	      && info->value_id != VN_INFO (info->valnum)->value_id)
	    {
	      changed = true;
	      info->value_id = VN_INFO (info->valnum)->value_id;
	    }
	}
    }
  
  set_hashtable_value_ids ();
  
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Value numbers:\n");
      for (i = 0; i < num_ssa_names; i++)
	{
	  tree name = ssa_name (i);
	  if (name
	      && VN_INFO (name)->visited
	      && SSA_VAL (name) != name)
	    {
	      print_generic_expr (dump_file, name, 0);
	      fprintf (dump_file, " = ");
	      print_generic_expr (dump_file, SSA_VAL (name), 0);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  may_insert = false;
  return true;
}

/* Return the maximum value id we have ever seen.  */

unsigned int
get_max_value_id (void) 
{
  return next_value_id;
}

/* Return the next unique value id.  */

unsigned int
get_next_value_id (void)
{
  return next_value_id++;
}


/* Compare two expressions E1 and E2 and return true if they are equal.  */

bool
expressions_equal_p (tree e1, tree e2)
{
  /* The obvious case.  */
  if (e1 == e2)
    return true;

  /* If only one of them is null, they cannot be equal.  */
  if (!e1 || !e2)
    return false;

  /* Recurse on elements of lists.  */
  if (TREE_CODE (e1) == TREE_LIST && TREE_CODE (e2) == TREE_LIST)
    {
      tree lop1 = e1;
      tree lop2 = e2;
      for (lop1 = e1, lop2 = e2;
	   lop1 || lop2;
	   lop1 = TREE_CHAIN (lop1), lop2 = TREE_CHAIN (lop2))
	{
	  if (!lop1 || !lop2)
	    return false;
	  if (!expressions_equal_p (TREE_VALUE (lop1), TREE_VALUE (lop2)))
	    return false;
	}
      return true;
    }

  /* Now perform the actual comparison.  */
  if (TREE_CODE (e1) == TREE_CODE (e2)
      && operand_equal_p (e1, e2, OEP_PURE_SAME))
    return true;

  return false;
}

/* Sort the VUSE array so that we can do equality comparisons
   quicker on two vuse vecs.  */

void
sort_vuses (VEC (tree,gc) *vuses)
{
  if (VEC_length (tree, vuses) > 1)
    qsort (VEC_address (tree, vuses),
	   VEC_length (tree, vuses),
	   sizeof (tree),
	   operand_build_cmp);
}

/* Sort the VUSE array so that we can do equality comparisons
   quicker on two vuse vecs.  */

void
sort_vuses_heap (VEC (tree,heap) *vuses)
{
  if (VEC_length (tree, vuses) > 1)
    qsort (VEC_address (tree, vuses),
	   VEC_length (tree, vuses),
	   sizeof (tree),
	   operand_build_cmp);
}


/* Return true if the nary operation NARY may trap.  This is a copy
   of stmt_could_throw_1_p adjusted to the SCCVN IL.  */

bool
vn_nary_may_trap (vn_nary_op_t nary)
{
  tree type;
  tree rhs2;
  bool honor_nans = false;
  bool honor_snans = false;
  bool fp_operation = false;
  bool honor_trapv = false;
  bool handled, ret;
  unsigned i;

  if (TREE_CODE_CLASS (nary->opcode) == tcc_comparison
      || TREE_CODE_CLASS (nary->opcode) == tcc_unary
      || TREE_CODE_CLASS (nary->opcode) == tcc_binary)
    {
      type = nary->type;
      fp_operation = FLOAT_TYPE_P (type);
      if (fp_operation)
	{
	  honor_nans = flag_trapping_math && !flag_finite_math_only;
	  honor_snans = flag_signaling_nans != 0;
	}
      else if (INTEGRAL_TYPE_P (type)
	       && TYPE_OVERFLOW_TRAPS (type))
	honor_trapv = true;
    }
  rhs2 = nary->op[1];
  ret = operation_could_trap_helper_p (nary->opcode, fp_operation,
				       honor_trapv,
				       honor_nans, honor_snans, rhs2,
				       &handled);
  if (handled
      && ret)
    return true;

  for (i = 0; i < nary->length; ++i)
    if (tree_could_trap_p (nary->op[i]))
      return true;

  return false;
}
