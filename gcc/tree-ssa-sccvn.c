/* SCC value numbering for trees
   Copyright (C) 2006, 2007
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
#include "tree-gimple.h"
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
  htab_t unary;
  htab_t binary;
  htab_t phis;
  htab_t references;
  alloc_pool unary_op_pool;
  alloc_pool binary_op_pool;
  alloc_pool phis_pool;
  alloc_pool references_pool;
} *vn_tables_t;

/* Binary operations in the hashtable consist of two operands, an
   opcode, and a type.  Result is the value number of the operation,
   and hashcode is stored to avoid having to calculate it
   repeatedly.  */

typedef struct vn_binary_op_s
{
  enum tree_code opcode;
  hashval_t hashcode;
  tree type;
  tree op0;
  tree op1;
  tree result;
} *vn_binary_op_t;
typedef const struct vn_binary_op_s *const_vn_binary_op_t;

/* Unary operations in the hashtable consist of a single operand, an
   opcode, and a type.  Result is the value number of the operation,
   and hashcode is stored to avoid having to calculate it repeatedly. */

typedef struct vn_unary_op_s
{
  enum tree_code opcode;
  hashval_t hashcode;
  tree type;
  tree op0;
  tree result;
} *vn_unary_op_t;
typedef const struct vn_unary_op_s *const_vn_unary_op_t;

/* Phi nodes in the hashtable consist of their non-VN_TOP phi
   arguments, and the basic block the phi is in. Result is the value
   number of the operation, and hashcode is stored to avoid having to
   calculate it repeatedly.  Phi nodes not in the same block are never
   considered equivalent.  */

typedef struct vn_phi_s
{
  VEC (tree, heap) *phiargs;
  basic_block block;
  hashval_t hashcode;
  tree result;
} *vn_phi_t;
typedef const struct vn_phi_s *const_vn_phi_t;

/* Reference operands only exist in reference operations structures.
   They consist of an opcode, type, and some number of operands.  For
   a given opcode, some, all, or none of the operands may be used.
   The operands are there to store the information that makes up the
   portion of the addressing calculation that opcode performs.  */

typedef struct vn_reference_op_struct
{
  enum tree_code opcode;
  tree type;
  tree op0;
  tree op1;
  tree op2;
} vn_reference_op_s;
typedef vn_reference_op_s *vn_reference_op_t;
typedef const vn_reference_op_s *const_vn_reference_op_t;

DEF_VEC_O(vn_reference_op_s);
DEF_VEC_ALLOC_O(vn_reference_op_s, heap);

/* A reference operation in the hashtable is representation as a
   collection of vuses, representing the memory state at the time of
   the operation, and a collection of operands that make up the
   addressing calculation.  If two vn_reference_t's have the same set
   of operands, they access the same memory location. We also store
   the resulting value number, and the hashcode.  The vuses are
   always stored in order sorted by ssa name version.  */

typedef struct vn_reference_s
{
  VEC (tree, gc) *vuses;
  VEC (vn_reference_op_s, heap) *operands;
  hashval_t hashcode;
  tree result;
} *vn_reference_t;
typedef const struct vn_reference_s *const_vn_reference_t;

/* Valid hashtables storing information we have proven to be
   correct.  */

static vn_tables_t valid_info;

/* Optimistic hashtables storing information we are making assumptions about
   during iterations.  */

static vn_tables_t optimistic_info;

/* PRE hashtables storing information about mapping from expressions to
   value handles.  */

static vn_tables_t pre_info;

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

/* Next DFS number and the stack for strongly connected component
   detection. */

static unsigned int next_dfs_num;
static VEC (tree, heap) *sccstack;

DEF_VEC_P(vn_ssa_aux_t);
DEF_VEC_ALLOC_P(vn_ssa_aux_t, heap);

/* Table of vn_ssa_aux_t's, one per ssa_name.  */

static VEC (vn_ssa_aux_t, heap) *vn_ssa_aux_table;

/* Return the value numbering information for a given SSA name.  */

vn_ssa_aux_t
VN_INFO (tree name)
{
  return VEC_index (vn_ssa_aux_t, vn_ssa_aux_table,
		    SSA_NAME_VERSION (name));
}

/* Set the value numbering info for a given SSA name to a given
   value.  */

static inline void
VN_INFO_SET (tree name, vn_ssa_aux_t value)
{
  VEC_replace (vn_ssa_aux_t, vn_ssa_aux_table,
	       SSA_NAME_VERSION (name), value);
}

/* Get the value numbering info for a given SSA name, creating it if
   it does not exist.  */

vn_ssa_aux_t
VN_INFO_GET (tree name)
{
  vn_ssa_aux_t newinfo = XCNEW (struct vn_ssa_aux);
  if (SSA_NAME_VERSION (name) >= VEC_length (vn_ssa_aux_t, vn_ssa_aux_table))
    VEC_safe_grow (vn_ssa_aux_t, heap, vn_ssa_aux_table,
		   SSA_NAME_VERSION (name) + 1);
  VEC_replace (vn_ssa_aux_t, vn_ssa_aux_table,
	       SSA_NAME_VERSION (name), newinfo);
  return newinfo;
}


/* Free a phi operation structure VP.  */

static void
free_phi (void *vp)
{
  vn_phi_t phi = vp;
  VEC_free (tree, heap, phi->phiargs);
}

/* Free a reference operation structure VP.  */

static void
free_reference (void *vp)
{
  vn_reference_t vr = vp;
  VEC_free (vn_reference_op_s, heap, vr->operands);
}

/* Compare two reference operands P1 and P2 for equality.  return true if
   they are equal, and false otherwise.  */

static int
vn_reference_op_eq (const void *p1, const void *p2)
{
  const_vn_reference_op_t const vro1 = (const_vn_reference_op_t) p1;
  const_vn_reference_op_t const vro2 = (const_vn_reference_op_t) p2;
  return vro1->opcode == vro2->opcode
    && vro1->type == vro2->type
    && expressions_equal_p (vro1->op0, vro2->op0)
    && expressions_equal_p (vro1->op1, vro2->op1)
    && expressions_equal_p (vro1->op2, vro2->op2);
}

/* Compute the hash for a reference operand VRO1  */

static hashval_t
vn_reference_op_compute_hash (const vn_reference_op_t vro1)
{
  return iterative_hash_expr (vro1->op0, vro1->opcode)
    + iterative_hash_expr (vro1->op1, vro1->opcode)
    + iterative_hash_expr (vro1->op2, vro1->opcode);
}

/* Return the hashcode for a given reference operation P1.  */

static hashval_t
vn_reference_hash (const void *p1)
{
  const_vn_reference_t const vr1 = (const_vn_reference_t) p1;
  return vr1->hashcode;
}

/* Compute a hash for the reference operation VR1 and return it.  */

static inline hashval_t
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

static int
vn_reference_eq (const void *p1, const void *p2)
{
  tree v;
  int i;
  vn_reference_op_t vro;

  const_vn_reference_t const vr1 = (const_vn_reference_t) p1;
  const_vn_reference_t const vr2 = (const_vn_reference_t) p2;

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

/* Place the vuses from STMT into *result */

static inline void
vuses_to_vec (tree stmt, VEC (tree, gc) **result)
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

VEC (tree, gc) *
copy_vuses_from_stmt (tree stmt)
{
  VEC (tree, gc) *vuses = NULL;

  vuses_to_vec (stmt, &vuses);

  return vuses;
}

/* Place the vdefs from STMT into *result */

static inline void
vdefs_to_vec (tree stmt, VEC (tree, gc) **result)
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
copy_vdefs_from_stmt (tree stmt)
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
shared_vuses_from_stmt (tree stmt)
{
  VEC_truncate (tree, shared_lookup_vops, 0);
  vuses_to_vec (stmt, &shared_lookup_vops);

  return shared_lookup_vops;
}

/* Copy the operations present in load/store/call REF into RESULT, a vector of
   vn_reference_op_s's.  */

static void
copy_reference_ops_from_ref (tree ref, VEC(vn_reference_op_s, heap) **result)
{
  /* Calls are different from all other reference operations.  */
  if (TREE_CODE (ref) == CALL_EXPR)
    {
      vn_reference_op_s temp;
      tree callfn;
      call_expr_arg_iterator iter;
      tree callarg;

      /* Copy the call_expr opcode, type, function being called, and
	 arguments.  */
      memset (&temp, 0, sizeof (temp));
      temp.type = TREE_TYPE (ref);
      temp.opcode = CALL_EXPR;
      VEC_safe_push (vn_reference_op_s, heap, *result, &temp);

      callfn = get_callee_fndecl (ref);
      if (!callfn)
	callfn = CALL_EXPR_FN (ref);
      temp.type = TREE_TYPE (callfn);
      temp.opcode = TREE_CODE (callfn);
      temp.op0 = callfn;
      VEC_safe_push (vn_reference_op_s, heap, *result, &temp);

      FOR_EACH_CALL_EXPR_ARG (callarg, iter, ref)
	{
	  memset (&temp, 0, sizeof (temp));
	  temp.type = TREE_TYPE (callarg);
	  temp.opcode = TREE_CODE (callarg);
	  temp.op0 = callarg;
	  VEC_safe_push (vn_reference_op_s, heap, *result, &temp);
	}
      return;
    }

  /* For non-calls, store the information that makes up the address.  */

  while (ref)
    {
      vn_reference_op_s temp;

      memset (&temp, 0, sizeof (temp));
      temp.type = TREE_TYPE (ref);
      temp.opcode = TREE_CODE (ref);

      switch (temp.opcode)
	{
	case ALIGN_INDIRECT_REF:
	case MISALIGNED_INDIRECT_REF:
	case INDIRECT_REF:
	  /* The only operand is the address, which gets its own
	     vn_reference_op_s structure.  */
	  break;
	case BIT_FIELD_REF:
	  /* Record bits and position.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  break;
	case COMPONENT_REF:
	  /* Record field as operand.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
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
	case VALUE_HANDLE:
	case VAR_DECL:
	case PARM_DECL:
	case CONST_DECL:
	case RESULT_DECL:
	case SSA_NAME:
	  temp.op0 = ref;
	  break;
	  /* These are only interesting for their operands, their
	     existence, and their type.  They will never be the last
	     ref in the chain of references (IE they require an
	     operand), so we don't have to put anything
	     for op* as it will be handled by the iteration  */
	case IMAGPART_EXPR:
	case REALPART_EXPR:
	case VIEW_CONVERT_EXPR:
	case ADDR_EXPR:
	  break;
	default:
	  gcc_unreachable ();

	}
      VEC_safe_push (vn_reference_op_s, heap, *result, &temp);

      if (REFERENCE_CLASS_P (ref) || TREE_CODE (ref) == ADDR_EXPR)
	ref = TREE_OPERAND (ref, 0);
      else
	ref = NULL_TREE;
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
	vro->op0 = SSA_VAL (vro->op0);
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

/* Lookup OP in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

tree
vn_reference_lookup (tree op, VEC (tree, gc) *vuses)
{
  void **slot;
  struct vn_reference_s vr1;

  vr1.vuses = valueize_vuses (vuses);
  vr1.operands = valueize_refs (shared_reference_ops_from_ref (op));
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  slot = htab_find_slot_with_hash (current_info->references, &vr1, vr1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->references, &vr1, vr1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;

  return ((vn_reference_t)*slot)->result;
}

/* Insert OP into the current hash table with a value number of
   RESULT.  */

void
vn_reference_insert (tree op, tree result, VEC (tree, gc) *vuses)
{
  void **slot;
  vn_reference_t vr1;

  vr1 = (vn_reference_t) pool_alloc (current_info->references_pool);

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
}


/* Return the stored hashcode for a unary operation.  */

static hashval_t
vn_unary_op_hash (const void *p1)
{
  const_vn_unary_op_t const vuo1 = (const_vn_unary_op_t) p1;
  return vuo1->hashcode;
}

/* Hash a unary operation P1 and return the result.  */

static inline hashval_t
vn_unary_op_compute_hash (const vn_unary_op_t vuo1)
{
  return iterative_hash_expr (vuo1->op0, vuo1->opcode);
}

/* Return true if P1 and P2, two unary operations, are equivalent.  */

static int
vn_unary_op_eq (const void *p1, const void *p2)
{
  const_vn_unary_op_t const vuo1 = (const_vn_unary_op_t) p1;
  const_vn_unary_op_t const vuo2 = (const_vn_unary_op_t) p2;
  return vuo1->opcode == vuo2->opcode
    && vuo1->type == vuo2->type
    && expressions_equal_p (vuo1->op0, vuo2->op0);
}

/* Lookup OP in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

tree
vn_unary_op_lookup (tree op)
{
  void **slot;
  struct vn_unary_op_s vuo1;

  vuo1.opcode = TREE_CODE (op);
  vuo1.type = TREE_TYPE (op);
  vuo1.op0 = TREE_OPERAND (op, 0);

  if (TREE_CODE (vuo1.op0) == SSA_NAME)
    vuo1.op0 = SSA_VAL (vuo1.op0);

  vuo1.hashcode = vn_unary_op_compute_hash (&vuo1);
  slot = htab_find_slot_with_hash (current_info->unary, &vuo1, vuo1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->unary, &vuo1, vuo1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;
  return ((vn_unary_op_t)*slot)->result;
}

/* Insert OP into the current hash table with a value number of
   RESULT.  */

void
vn_unary_op_insert (tree op, tree result)
{
  void **slot;
  vn_unary_op_t vuo1 = (vn_unary_op_t) pool_alloc (current_info->unary_op_pool);

  vuo1->opcode = TREE_CODE (op);
  vuo1->type = TREE_TYPE (op);
  vuo1->op0 = TREE_OPERAND (op, 0);
  vuo1->result = result;

  if (TREE_CODE (vuo1->op0) == SSA_NAME)
    vuo1->op0 = SSA_VAL (vuo1->op0);

  vuo1->hashcode = vn_unary_op_compute_hash (vuo1);
  slot = htab_find_slot_with_hash (current_info->unary, vuo1, vuo1->hashcode,
				   INSERT);
  gcc_assert (!*slot);
  *slot = vuo1;
}

/* Compute and return the hash value for binary operation VBO1.  */

static inline hashval_t
vn_binary_op_compute_hash (const vn_binary_op_t vbo1)
{
  return iterative_hash_expr (vbo1->op0, vbo1->opcode)
    + iterative_hash_expr (vbo1->op1, vbo1->opcode);
}

/* Return the computed hashcode for binary operation P1.  */

static hashval_t
vn_binary_op_hash (const void *p1)
{
  const_vn_binary_op_t const vbo1 = (const_vn_binary_op_t) p1;
  return vbo1->hashcode;
}

/* Compare binary operations P1 and P2 and return true if they are
   equivalent.  */

static int
vn_binary_op_eq (const void *p1, const void *p2)
{
  const_vn_binary_op_t const vbo1 = (const_vn_binary_op_t) p1;
  const_vn_binary_op_t const vbo2 = (const_vn_binary_op_t) p2;
  return vbo1->opcode == vbo2->opcode
    && vbo1->type == vbo2->type
    && expressions_equal_p (vbo1->op0, vbo2->op0)
    && expressions_equal_p (vbo1->op1, vbo2->op1);
}

/* Lookup OP in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

tree
vn_binary_op_lookup (tree op)
{
  void **slot;
  struct vn_binary_op_s vbo1;

  vbo1.opcode = TREE_CODE (op);
  vbo1.type = TREE_TYPE (op);
  vbo1.op0 = TREE_OPERAND (op, 0);
  vbo1.op1 = TREE_OPERAND (op, 1);

  if (TREE_CODE (vbo1.op0) == SSA_NAME)
    vbo1.op0 = SSA_VAL (vbo1.op0);
  if (TREE_CODE (vbo1.op1) == SSA_NAME)
    vbo1.op1 = SSA_VAL (vbo1.op1);

  if (tree_swap_operands_p (vbo1.op0, vbo1.op1, false)
      && commutative_tree_code (vbo1.opcode))
    {
      tree temp = vbo1.op0;
      vbo1.op0 = vbo1.op1;
      vbo1.op1 = temp;
    }

  vbo1.hashcode = vn_binary_op_compute_hash (&vbo1);
  slot = htab_find_slot_with_hash (current_info->binary, &vbo1, vbo1.hashcode,
				   NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = htab_find_slot_with_hash (valid_info->binary, &vbo1, vbo1.hashcode,
				     NO_INSERT);
  if (!slot)
    return NULL_TREE;
  return ((vn_binary_op_t)*slot)->result;
}

/* Insert OP into the current hash table with a value number of
   RESULT.  */

void
vn_binary_op_insert (tree op, tree result)
{
  void **slot;
  vn_binary_op_t vbo1;
  vbo1 = (vn_binary_op_t) pool_alloc (current_info->binary_op_pool);

  vbo1->opcode = TREE_CODE (op);
  vbo1->type = TREE_TYPE (op);
  vbo1->op0 = TREE_OPERAND (op, 0);
  vbo1->op1 = TREE_OPERAND (op, 1);
  vbo1->result = result;

  if (TREE_CODE (vbo1->op0) == SSA_NAME)
    vbo1->op0 = SSA_VAL (vbo1->op0);
  if (TREE_CODE (vbo1->op1) == SSA_NAME)
    vbo1->op1 = SSA_VAL (vbo1->op1);

  if (tree_swap_operands_p (vbo1->op0, vbo1->op1, false)
      && commutative_tree_code (vbo1->opcode))
    {
      tree temp = vbo1->op0;
      vbo1->op0 = vbo1->op1;
      vbo1->op1 = temp;
    }
  vbo1->hashcode = vn_binary_op_compute_hash (vbo1);
  slot = htab_find_slot_with_hash (current_info->binary, vbo1, vbo1->hashcode,
				   INSERT);
  gcc_assert (!*slot);

  *slot = vbo1;
}

/* Compute a hashcode for PHI operation VP1 and return it.  */

static inline hashval_t
vn_phi_compute_hash (vn_phi_t vp1)
{
  hashval_t result = 0;
  int i;
  tree phi1op;

  result = vp1->block->index;

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

  if (vp1->block == vp2->block)
    {
      int i;
      tree phi1op;

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
vn_phi_lookup (tree phi)
{
  void **slot;
  struct vn_phi_s vp1;
  int i;

  VEC_truncate (tree, shared_lookup_phiargs, 0);

  /* Canonicalize the SSA_NAME's to their value number.  */
  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      VEC_safe_push (tree, heap, shared_lookup_phiargs, def);
    }
  vp1.phiargs = shared_lookup_phiargs;
  vp1.block = bb_for_stmt (phi);
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

static void
vn_phi_insert (tree phi, tree result)
{
  void **slot;
  vn_phi_t vp1 = (vn_phi_t) pool_alloc (current_info->phis_pool);
  int i;
  VEC (tree, heap) *args = NULL;

  /* Canonicalize the SSA_NAME's to their value number.  */
  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      VEC_safe_push (tree, heap, args, def);
    }
  vp1->phiargs = args;
  vp1->block = bb_for_stmt (phi);
  vp1->result = result;
  vp1->hashcode = vn_phi_compute_hash (vp1);

  slot = htab_find_slot_with_hash (current_info->phis, vp1, vp1->hashcode,
				   INSERT);

  /* Because we iterate over phi operations more than once, it's
     possible the slot might already exist here, hence no assert.*/
  *slot = vp1;
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
      fprintf (dump_file, "\n");
    }

  currval = SSA_VAL (from);

  if (currval != to  && !operand_equal_p (currval, to, OEP_PURE_SAME))
    {
      SSA_VAL (from) = to;
      return true;
    }
  return false;
}

/* Set all definitions in STMT to value number to themselves.
   Return true if a value number changed. */

static bool
defs_to_varying (tree stmt)
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

/* Visit a copy between LHS and RHS, return true if the value number
   changed.  */

static bool
visit_copy (tree lhs, tree rhs)
{

  /* Follow chains of copies to their destination.  */
  while (SSA_VAL (rhs) != rhs && TREE_CODE (SSA_VAL (rhs)) == SSA_NAME)
    rhs = SSA_VAL (rhs);

  /* The copy may have a more interesting constant filled expression
     (we don't, since we know our RHS is just an SSA name).  */
  VN_INFO (lhs)->has_constants = VN_INFO (rhs)->has_constants;
  VN_INFO (lhs)->expr = VN_INFO (rhs)->expr;

  return set_ssa_val_to (lhs, rhs);
}

/* Visit a unary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_unary_op (tree lhs, tree op)
{
  bool changed = false;
  tree result = vn_unary_op_lookup (op);

  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
    }
  else
    {
      changed = set_ssa_val_to (lhs, lhs);
      vn_unary_op_insert (op, lhs);
    }

  return changed;
}

/* Visit a binary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_binary_op (tree lhs, tree op)
{
  bool changed = false;
  tree result = vn_binary_op_lookup (op);

  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
    }
  else
    {
      changed = set_ssa_val_to (lhs, lhs);
      vn_binary_op_insert (op, lhs);
    }

  return changed;
}

/* Visit a load from a reference operator RHS, part of STMT, value number it,
   and return true if the value number of the LHS has changed as a result.  */

static bool
visit_reference_op_load (tree lhs, tree op, tree stmt)
{
  bool changed = false;
  tree result = vn_reference_lookup (op, shared_vuses_from_stmt (stmt));

  if (result)
    {
      changed = set_ssa_val_to (lhs, result);
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
visit_reference_op_store (tree lhs, tree op, tree stmt)
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

  result = vn_reference_lookup (lhs, shared_vuses_from_stmt (stmt));

  if (result)
    {
      if (TREE_CODE (result) == SSA_NAME)
	result = SSA_VAL (result);
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
visit_phi (tree phi)
{
  bool changed = false;
  tree result;
  tree sameval = VN_TOP;
  bool allsame = true;
  int i;

  /* TODO: We could check for this in init_sccvn, and replace this
     with a gcc_assert.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
    return set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));

  /* See if all non-TOP arguments have the same value.  TOP is
     equivalent to everything, so we can ignore it.  */
  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
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
simplify_binary_expression (tree stmt, tree rhs)
{
  tree result = NULL_TREE;
  tree op0 = TREE_OPERAND (rhs, 0);
  tree op1 = TREE_OPERAND (rhs, 1);

  /* This will not catch every single case we could combine, but will
     catch those with constants.  The goal here is to simultaneously
     combine constants between expressions, but avoid infinite
     expansion of expressions during simplification.  */
  if (TREE_CODE (op0) == SSA_NAME)
    {
      if (VN_INFO (op0)->has_constants)
	op0 = valueize_expr (VN_INFO (op0)->expr);
      else if (SSA_VAL (op0) != VN_TOP && SSA_VAL (op0) != op0)
	op0 = SSA_VAL (op0);
    }

  if (TREE_CODE (op1) == SSA_NAME)
    {
      if (VN_INFO (op1)->has_constants)
	op1 = valueize_expr (VN_INFO (op1)->expr);
      else if (SSA_VAL (op1) != VN_TOP && SSA_VAL (op1) != op1)
	op1 = SSA_VAL (op1);
    }

  /* Avoid folding if nothing changed.  */
  if (op0 == TREE_OPERAND (rhs, 0)
      && op1 == TREE_OPERAND (rhs, 1))
    return NULL_TREE;

  fold_defer_overflow_warnings ();

  result = fold_binary (TREE_CODE (rhs), TREE_TYPE (rhs), op0, op1);

  fold_undefer_overflow_warnings (result && valid_gimple_expression_p (result),
				  stmt, 0);

  /* Make sure result is not a complex expression consisting
     of operators of operators (IE (a + b) + (a + c))
     Otherwise, we will end up with unbounded expressions if
     fold does anything at all.  */
  if (result && valid_gimple_expression_p (result))
    return result;

  return NULL_TREE;
}

/* Simplify the unary expression RHS, and return the result if
   simplified. */

static tree
simplify_unary_expression (tree rhs)
{
  tree result = NULL_TREE;
  tree op0 = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (op0) != SSA_NAME)
    return NULL_TREE;

  if (VN_INFO (op0)->has_constants)
    op0 = valueize_expr (VN_INFO (op0)->expr);
  else if (TREE_CODE (rhs) == NOP_EXPR
	   || TREE_CODE (rhs) == CONVERT_EXPR
	   || TREE_CODE (rhs) == REALPART_EXPR
	   || TREE_CODE (rhs) == IMAGPART_EXPR)
    {
      /* We want to do tree-combining on conversion-like expressions.
         Make sure we feed only SSA_NAMEs or constants to fold though.  */
      tree tem = valueize_expr (VN_INFO (op0)->expr);
      if (UNARY_CLASS_P (tem)
	  || BINARY_CLASS_P (tem)
	  || TREE_CODE (tem) == SSA_NAME
	  || is_gimple_min_invariant (tem))
	op0 = tem;
    }

  /* Avoid folding if nothing changed, but remember the expression.  */
  if (op0 == TREE_OPERAND (rhs, 0))
    return rhs;

  result = fold_unary_ignore_overflow (TREE_CODE (rhs), TREE_TYPE (rhs), op0);
  if (result)
    {
      STRIP_USELESS_TYPE_CONVERSION (result);
      if (valid_gimple_expression_p (result))
        return result;
    }

  return rhs;
}

/* Try to simplify RHS using equivalences and constant folding.  */

static tree
try_to_simplify (tree stmt, tree rhs)
{
  if (TREE_CODE (rhs) == SSA_NAME)
    {
      if (is_gimple_min_invariant (SSA_VAL (rhs)))
	return SSA_VAL (rhs);
      else if (VN_INFO (rhs)->has_constants)
	return VN_INFO (rhs)->expr;
    }
  else
    {
      switch (TREE_CODE_CLASS (TREE_CODE (rhs)))
	{
	  /* For references, see if we find a result for the lookup,
	     and use it if we do.  */
	case tcc_declaration:
	  /* Pull out any truly constant values.  */
	  if (TREE_READONLY (rhs)
	      && TREE_STATIC (rhs)
	      && DECL_INITIAL (rhs)
	      && valid_gimple_expression_p (DECL_INITIAL (rhs)))
	    return DECL_INITIAL (rhs);

	    /* Fallthrough. */
	case tcc_reference:
	  {
	    tree result = vn_reference_lookup (rhs,
					       shared_vuses_from_stmt (stmt));
	    if (result)
	      return result;
	  }
	  /* Fallthrough for some codes.  */
	  if (!(TREE_CODE (rhs) == REALPART_EXPR
	        || TREE_CODE (rhs) == IMAGPART_EXPR))
	    break;
	  /* We could do a little more with unary ops, if they expand
	     into binary ops, but it's debatable whether it is worth it. */
	case tcc_unary:
	  return simplify_unary_expression (rhs);
	  break;
	case tcc_comparison:
	case tcc_binary:
	  return simplify_binary_expression (stmt, rhs);
	  break;
	default:
	  break;
	}
    }
  return rhs;
}

/* Visit and value number USE, return true if the value number
   changed. */

static bool
visit_use (tree use)
{
  bool changed = false;
  tree stmt = SSA_NAME_DEF_STMT (use);
  stmt_ann_t ann;

  VN_INFO (use)->use_processed = true;

  gcc_assert (!SSA_NAME_IN_FREE_LIST (use));
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Value numbering ");
      print_generic_expr (dump_file, use, 0);
      fprintf (dump_file, " stmt = ");
      print_generic_stmt (dump_file, stmt, 0);
    }

  /* RETURN_EXPR may have an embedded MODIFY_STMT.  */
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == GIMPLE_MODIFY_STMT)
    stmt = TREE_OPERAND (stmt, 0);

  ann = stmt_ann (stmt);

  /* Handle uninitialized uses.  */
  if (IS_EMPTY_STMT (stmt))
    {
      changed = set_ssa_val_to (use, use);
    }
  else
    {
      if (TREE_CODE (stmt) == PHI_NODE)
	{
	  changed = visit_phi (stmt);
	}
      else if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT
	       || (ann && ann->has_volatile_ops)
	       || tree_could_throw_p (stmt))
	{
	  changed = defs_to_varying (stmt);
	}
      else
	{
	  tree lhs = GIMPLE_STMT_OPERAND (stmt, 0);
	  tree rhs = GIMPLE_STMT_OPERAND (stmt, 1);
	  tree simplified;

	  STRIP_USELESS_TYPE_CONVERSION (rhs);

	  /* Shortcut for copies. Simplifying copies is pointless,
	     since we copy the expression and value they represent.  */
	  if (TREE_CODE (rhs) == SSA_NAME && TREE_CODE (lhs) == SSA_NAME)
	    {
	      changed = visit_copy (lhs, rhs);
	      goto done;
	    }
	  simplified = try_to_simplify (stmt, rhs);
	  if (simplified && simplified != rhs)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "RHS ");
		  print_generic_expr (dump_file, rhs, 0);
		  fprintf (dump_file, " simplified to ");
		  print_generic_expr (dump_file, simplified, 0);
		  if (TREE_CODE (lhs) == SSA_NAME)
		    fprintf (dump_file, " has constants %d\n",
			     VN_INFO (lhs)->has_constants);
		  else
		    fprintf (dump_file, "\n");

		}
	    }
	  /* Setting value numbers to constants will occasionally
	     screw up phi congruence because constants are not
	     uniquely associated with a single ssa name that can be
	     looked up.  */
	  if (simplified && is_gimple_min_invariant (simplified)
	      && TREE_CODE (lhs) == SSA_NAME
	      && simplified != rhs)
	    {
	      VN_INFO (lhs)->expr = simplified;
	      VN_INFO (lhs)->has_constants = true;
	      changed = set_ssa_val_to (lhs, simplified);
	      goto done;
	    }
	  else if (simplified && TREE_CODE (simplified) == SSA_NAME
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
	      rhs = simplified;
	    }
	  else if (expr_has_constants (rhs) && TREE_CODE (lhs) == SSA_NAME)
	    {
	      VN_INFO (lhs)->has_constants = true;
	      VN_INFO (lhs)->expr = unshare_expr (rhs);
	    }
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      /* We reset expr and constantness here because we may
		 have been value numbering optimistically, and
		 iterating. They may become non-constant in this case,
		 even if they were optimistically constant. */

	      VN_INFO (lhs)->has_constants = false;
	      VN_INFO (lhs)->expr = lhs;
	    }

	  if (TREE_CODE (lhs) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    changed = defs_to_varying (stmt);
	  else if (REFERENCE_CLASS_P (lhs) || DECL_P (lhs))
	    {
	      changed = visit_reference_op_store (lhs, rhs, stmt);
	    }
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      if (is_gimple_min_invariant (rhs))
		{
		  VN_INFO (lhs)->has_constants = true;
		  VN_INFO (lhs)->expr = rhs;
		  changed = set_ssa_val_to (lhs, rhs);
		}
	      else
		{
		  switch (TREE_CODE_CLASS (TREE_CODE (rhs)))
		    {
		    case tcc_unary:
		      changed = visit_unary_op (lhs, rhs);
		      break;
		    case tcc_binary:
		      changed = visit_binary_op (lhs, rhs);
		      break;
		      /* If tcc_vl_expr ever encompasses more than
			 CALL_EXPR, this will need to be changed.  */
		    case tcc_vl_exp:
		      if (call_expr_flags (rhs)  & (ECF_PURE | ECF_CONST))
			changed = visit_reference_op_load (lhs, rhs, stmt);
		      else
			changed = defs_to_varying (stmt);
		      break;
		    case tcc_declaration:
		    case tcc_reference:
		      changed = visit_reference_op_load (lhs, rhs, stmt);
		      break;
		    case tcc_expression:
		      if (TREE_CODE (rhs) == ADDR_EXPR)
			{
			  changed = visit_unary_op (lhs, rhs);
			  goto done;
			}
		      /* Fallthrough.  */
		    default:
		      changed = defs_to_varying (stmt);
		      break;
		    }
		}
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
  tree opstmta = SSA_NAME_DEF_STMT (opa);
  tree opstmtb = SSA_NAME_DEF_STMT (opb);
  basic_block bba;
  basic_block bbb;

  if (IS_EMPTY_STMT (opstmta) && IS_EMPTY_STMT (opstmtb))
    return 0;
  else if (IS_EMPTY_STMT (opstmta))
    return -1;
  else if (IS_EMPTY_STMT (opstmtb))
    return 1;

  bba = bb_for_stmt (opstmta);
  bbb = bb_for_stmt (opstmtb);

  if (!bba && !bbb)
    return 0;
  else if (!bba)
    return -1;
  else if (!bbb)
    return 1;

  if (bba == bbb)
    {
      if (TREE_CODE (opstmta) == PHI_NODE && TREE_CODE (opstmtb) == PHI_NODE)
	return 0;
      else if (TREE_CODE (opstmta) == PHI_NODE)
	return -1;
      else if (TREE_CODE (opstmtb) == PHI_NODE)
	return 1;
      return stmt_ann (opstmta)->uid - stmt_ann (opstmtb)->uid;
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
	  htab_empty (optimistic_info->unary);
	  htab_empty (optimistic_info->binary);
	  htab_empty (optimistic_info->phis);
	  htab_empty (optimistic_info->references);
	  empty_alloc_pool (optimistic_info->unary_op_pool);
	  empty_alloc_pool (optimistic_info->binary_op_pool);
	  empty_alloc_pool (optimistic_info->phis_pool);
	  empty_alloc_pool (optimistic_info->references_pool);
	  for (i = 0; VEC_iterate (tree, scc, i, var); i++)
	    changed |= visit_use (var);
	}

      if (dump_file && (dump_flags & TDF_STATS))
	fprintf (dump_file, "Processing SCC required %d iterations\n",
		 iterations);

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
   to ressource constraints.  */

static bool
DFS (tree name)
{
  VEC(ssa_op_iter, heap) *itervec = NULL;
  VEC(tree, heap) *namevec = NULL;
  use_operand_p usep = NULL;
  tree defstmt, use;
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
  if (!IS_EMPTY_STMT (defstmt))
    {
      /* Push a new iterator.  */
      if (TREE_CODE (defstmt) == PHI_NODE)
	usep = op_iter_init_phiuse (&iter, defstmt, SSA_OP_ALL_USES);
      else
	usep = op_iter_init_use (&iter, defstmt, SSA_OP_ALL_USES);
    }
  else
    iter.done = true;

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
  table->unary = htab_create (23, vn_unary_op_hash, vn_unary_op_eq, NULL);
  table->binary = htab_create (23, vn_binary_op_hash, vn_binary_op_eq, NULL);
  table->references = htab_create (23, vn_reference_hash, vn_reference_eq,
				   free_reference);

  table->unary_op_pool = create_alloc_pool ("VN unary operations",
					    sizeof (struct vn_unary_op_s),
					    30);
  table->binary_op_pool = create_alloc_pool ("VN binary operations",
					     sizeof (struct vn_binary_op_s),
					     30);
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
  htab_delete (table->unary);
  htab_delete (table->binary);
  htab_delete (table->references);
  free_alloc_pool (table->unary_op_pool);
  free_alloc_pool (table->binary_op_pool);
  free_alloc_pool (table->phis_pool);
  free_alloc_pool (table->references_pool);
}

static void
init_scc_vn (void)
{
  size_t i;
  int j;
  int *rpo_numbers_temp;
  basic_block bb;
  size_t id = 0;

  calculate_dominance_info (CDI_DOMINATORS);
  sccstack = NULL;
  next_dfs_num = 1;

  vn_ssa_aux_table = VEC_alloc (vn_ssa_aux_t, heap, num_ssa_names + 1);
  /* VEC_alloc doesn't actually grow it to the right size, it just
     preallocates the space to do so.  */
  VEC_safe_grow (vn_ssa_aux_t, heap, vn_ssa_aux_table, num_ssa_names + 1);
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

  free (rpo_numbers_temp);

  VN_TOP = create_tmp_var_raw (void_type_node, "vn_top");

  /* Create the VN_INFO structures, and initialize value numbers to
     TOP.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name)
	{
	  VN_INFO_GET (name)->valnum = VN_TOP;
	  VN_INFO (name)->expr = name;
	}
    }

  FOR_ALL_BB (bb)
    {
      block_stmt_iterator bsi;
      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);
	  stmt_ann (stmt)->uid = id++;
	}
    }

  /* Create the valid and optimistic value numbering tables.  */
  valid_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (valid_info);
  optimistic_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (optimistic_info);
  pre_info = NULL;
}

void
switch_to_PRE_table (void)
{
  pre_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (pre_info);
  current_info = pre_info;
}

void
free_scc_vn (void)
{
  size_t i;

  VEC_free (tree, heap, shared_lookup_phiargs);
  VEC_free (tree, gc, shared_lookup_vops);
  VEC_free (vn_reference_op_s, heap, shared_lookup_references);
  XDELETEVEC (rpo_numbers);
  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name)
	{
	  XDELETE (VN_INFO (name));
	  if (SSA_NAME_VALUE (name) &&
	      TREE_CODE (SSA_NAME_VALUE (name)) == VALUE_HANDLE)
	    SSA_NAME_VALUE (name) = NULL;
	}
    }

  VEC_free (vn_ssa_aux_t, heap, vn_ssa_aux_table);
  VEC_free (tree, heap, sccstack);
  free_vn_table (valid_info);
  XDELETE (valid_info);
  free_vn_table (optimistic_info);
  XDELETE (optimistic_info);
  if (pre_info)
    {
      free_vn_table (pre_info);
      XDELETE (pre_info);
    }
}

/* Do SCCVN.  Returns true if it finished, false if we bailed out
   due to ressource constraints.  */

bool
run_scc_vn (void)
{
  size_t i;
  tree param;

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

  for (i = num_ssa_names - 1; i > 0; i--)
    {
      tree name = ssa_name (i);
      if (name
	  && VN_INFO (name)->visited == false
	  && !has_zero_uses (name))
	if (!DFS (name))
	  {
	    free_scc_vn ();
	    return false;
	  }
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Value numbers:\n");
      for (i = 0; i < num_ssa_names; i++)
	{
	  tree name = ssa_name (i);
	  if (name && VN_INFO (name)->visited
	      && (SSA_VAL (name) != name
		  || is_gimple_min_invariant (VN_INFO (name)->expr)))
	    {
	      print_generic_expr (dump_file, name, 0);
	      fprintf (dump_file, " = ");
	      if (is_gimple_min_invariant (VN_INFO (name)->expr))
		print_generic_expr (dump_file, VN_INFO (name)->expr, 0);
	      else
		print_generic_expr (dump_file, SSA_VAL (name), 0);
	      fprintf (dump_file, "\n");
	    }
	}
    }

  return true;
}
