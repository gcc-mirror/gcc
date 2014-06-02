/* SCC value numbering for trees
   Copyright (C) 2006-2014 Free Software Foundation, Inc.
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
#include "tree.h"
#include "stor-layout.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-inline.h"
#include "hash-table.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "dumpfile.h"
#include "alloc-pool.h"
#include "flags.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-sccvn.h"
#include "tree-cfg.h"
#include "domwalk.h"

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


/* vn_nary_op hashtable helpers.  */

struct vn_nary_op_hasher : typed_noop_remove <vn_nary_op_s>
{
  typedef vn_nary_op_s value_type;
  typedef vn_nary_op_s compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Return the computed hashcode for nary operation P1.  */

inline hashval_t
vn_nary_op_hasher::hash (const value_type *vno1)
{
  return vno1->hashcode;
}

/* Compare nary operations P1 and P2 and return true if they are
   equivalent.  */

inline bool
vn_nary_op_hasher::equal (const value_type *vno1, const compare_type *vno2)
{
  return vn_nary_op_eq (vno1, vno2);
}

typedef hash_table <vn_nary_op_hasher> vn_nary_op_table_type;
typedef vn_nary_op_table_type::iterator vn_nary_op_iterator_type;


/* vn_phi hashtable helpers.  */

static int
vn_phi_eq (const_vn_phi_t const vp1, const_vn_phi_t const vp2);

struct vn_phi_hasher
{ 
  typedef vn_phi_s value_type;
  typedef vn_phi_s compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Return the computed hashcode for phi operation P1.  */

inline hashval_t
vn_phi_hasher::hash (const value_type *vp1)
{
  return vp1->hashcode;
}

/* Compare two phi entries for equality, ignoring VN_TOP arguments.  */

inline bool
vn_phi_hasher::equal (const value_type *vp1, const compare_type *vp2)
{
  return vn_phi_eq (vp1, vp2);
}

/* Free a phi operation structure VP.  */

inline void
vn_phi_hasher::remove (value_type *phi)
{
  phi->phiargs.release ();
}

typedef hash_table <vn_phi_hasher> vn_phi_table_type;
typedef vn_phi_table_type::iterator vn_phi_iterator_type;


/* Compare two reference operands P1 and P2 for equality.  Return true if
   they are equal, and false otherwise.  */

static int
vn_reference_op_eq (const void *p1, const void *p2)
{
  const_vn_reference_op_t const vro1 = (const_vn_reference_op_t) p1;
  const_vn_reference_op_t const vro2 = (const_vn_reference_op_t) p2;

  return (vro1->opcode == vro2->opcode
	  /* We do not care for differences in type qualification.  */
	  && (vro1->type == vro2->type
	      || (vro1->type && vro2->type
		  && types_compatible_p (TYPE_MAIN_VARIANT (vro1->type),
					 TYPE_MAIN_VARIANT (vro2->type))))
	  && expressions_equal_p (vro1->op0, vro2->op0)
	  && expressions_equal_p (vro1->op1, vro2->op1)
	  && expressions_equal_p (vro1->op2, vro2->op2));
}

/* Free a reference operation structure VP.  */

static inline void
free_reference (vn_reference_s *vr)
{
  vr->operands.release ();
}


/* vn_reference hashtable helpers.  */

struct vn_reference_hasher
{
  typedef vn_reference_s value_type;
  typedef vn_reference_s compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* Return the hashcode for a given reference operation P1.  */

inline hashval_t
vn_reference_hasher::hash (const value_type *vr1)
{
  return vr1->hashcode;
}

inline bool
vn_reference_hasher::equal (const value_type *v, const compare_type *c)
{
  return vn_reference_eq (v, c);
}

inline void
vn_reference_hasher::remove (value_type *v)
{
  free_reference (v);
}

typedef hash_table <vn_reference_hasher> vn_reference_table_type;
typedef vn_reference_table_type::iterator vn_reference_iterator_type;


/* The set of hashtables and alloc_pool's for their items.  */

typedef struct vn_tables_s
{
  vn_nary_op_table_type nary;
  vn_phi_table_type phis;
  vn_reference_table_type references;
  struct obstack nary_obstack;
  alloc_pool phis_pool;
  alloc_pool references_pool;
} *vn_tables_t;


/* vn_constant hashtable helpers.  */

struct vn_constant_hasher : typed_free_remove <vn_constant_s>
{ 
  typedef vn_constant_s value_type;
  typedef vn_constant_s compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
};

/* Hash table hash function for vn_constant_t.  */

inline hashval_t
vn_constant_hasher::hash (const value_type *vc1)
{
  return vc1->hashcode;
}

/* Hash table equality function for vn_constant_t.  */

inline bool
vn_constant_hasher::equal (const value_type *vc1, const compare_type *vc2)
{
  if (vc1->hashcode != vc2->hashcode)
    return false;

  return vn_constant_eq_with_type (vc1->constant, vc2->constant);
}

static hash_table <vn_constant_hasher> constant_to_value_id;
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

/* Return the SSA value of the VUSE x, supporting released VDEFs
   during elimination which will value-number the VDEF to the
   associated VUSE (but not substitute in the whole lattice).  */

static inline tree
vuse_ssa_val (tree x)
{
  if (!x)
    return NULL_TREE;

  do
    {
      x = SSA_VAL (x);
    }
  while (SSA_NAME_IN_FREE_LIST (x));

  return x;
}

/* This represents the top of the VN lattice, which is the universal
   value.  */

tree VN_TOP;

/* Unique counter for our value ids.  */

static unsigned int next_value_id;

/* Next DFS number and the stack for strongly connected component
   detection. */

static unsigned int next_dfs_num;
static vec<tree> sccstack;



/* Table of vn_ssa_aux_t's, one per ssa_name.  The vn_ssa_aux_t objects
   are allocated on an obstack for locality reasons, and to free them
   without looping over the vec.  */

static vec<vn_ssa_aux_t> vn_ssa_aux_table;
static struct obstack vn_ssa_aux_obstack;

/* Return the value numbering information for a given SSA name.  */

vn_ssa_aux_t
VN_INFO (tree name)
{
  vn_ssa_aux_t res = vn_ssa_aux_table[SSA_NAME_VERSION (name)];
  gcc_checking_assert (res);
  return res;
}

/* Set the value numbering info for a given SSA name to a given
   value.  */

static inline void
VN_INFO_SET (tree name, vn_ssa_aux_t value)
{
  vn_ssa_aux_table[SSA_NAME_VERSION (name)] = value;
}

/* Initialize the value numbering info for a given SSA name.
   This should be called just once for every SSA name.  */

vn_ssa_aux_t
VN_INFO_GET (tree name)
{
  vn_ssa_aux_t newinfo;

  newinfo = XOBNEW (&vn_ssa_aux_obstack, struct vn_ssa_aux);
  memset (newinfo, 0, sizeof (struct vn_ssa_aux));
  if (SSA_NAME_VERSION (name) >= vn_ssa_aux_table.length ())
    vn_ssa_aux_table.safe_grow (SSA_NAME_VERSION (name) + 1);
  vn_ssa_aux_table[SSA_NAME_VERSION (name)] = newinfo;
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
  enum tree_code code;

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

  /* If the value number is not an assignment use it directly.  */
  if (!is_gimple_assign (def_stmt))
    return vn->valnum;

  /* Note that we can valueize here because we clear the cached
     simplified expressions after each optimistic iteration.  */
  code = gimple_assign_rhs_code (def_stmt);
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_reference:
      if ((code == REALPART_EXPR
	   || code == IMAGPART_EXPR
	   || code == VIEW_CONVERT_EXPR)
	  && TREE_CODE (TREE_OPERAND (gimple_assign_rhs1 (def_stmt),
				      0)) == SSA_NAME)
	expr = fold_build1 (code,
			    gimple_expr_type (def_stmt),
			    vn_valueize (TREE_OPERAND
					   (gimple_assign_rhs1 (def_stmt), 0)));
      break;

    case tcc_unary:
      expr = fold_build1 (code,
			  gimple_expr_type (def_stmt),
			  vn_valueize (gimple_assign_rhs1 (def_stmt)));
      break;

    case tcc_binary:
      expr = fold_build2 (code,
			  gimple_expr_type (def_stmt),
			  vn_valueize (gimple_assign_rhs1 (def_stmt)),
			  vn_valueize (gimple_assign_rhs2 (def_stmt)));
      break;

    case tcc_exceptional:
      if (code == CONSTRUCTOR
	  && TREE_CODE
	       (TREE_TYPE (gimple_assign_rhs1 (def_stmt))) == VECTOR_TYPE)
	expr = gimple_assign_rhs1 (def_stmt);
      break;

    default:;
    }
  if (expr == NULL_TREE)
    return vn->valnum;

  /* Cache the expression.  */
  vn->expr = expr;

  return expr;
}

/* Return the vn_kind the expression computed by the stmt should be
   associated with.  */

enum vn_kind
vn_get_stmt_kind (gimple stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      return VN_REFERENCE;
    case GIMPLE_PHI:
      return VN_PHI;
    case GIMPLE_ASSIGN:
      {
	enum tree_code code = gimple_assign_rhs_code (stmt);
	tree rhs1 = gimple_assign_rhs1 (stmt);
	switch (get_gimple_rhs_class (code))
	  {
	  case GIMPLE_UNARY_RHS:
	  case GIMPLE_BINARY_RHS:
	  case GIMPLE_TERNARY_RHS:
	    return VN_NARY;
	  case GIMPLE_SINGLE_RHS:
	    switch (TREE_CODE_CLASS (code))
	      {
	      case tcc_reference:
		/* VOP-less references can go through unary case.  */
		if ((code == REALPART_EXPR
		     || code == IMAGPART_EXPR
		     || code == VIEW_CONVERT_EXPR
		     || code == BIT_FIELD_REF)
		    && TREE_CODE (TREE_OPERAND (rhs1, 0)) == SSA_NAME)
		  return VN_NARY;

		/* Fallthrough.  */
	      case tcc_declaration:
		return VN_REFERENCE;

	      case tcc_constant:
		return VN_CONSTANT;

	      default:
		if (code == ADDR_EXPR)
		  return (is_gimple_min_invariant (rhs1)
			  ? VN_CONSTANT : VN_REFERENCE);
		else if (code == CONSTRUCTOR)
		  return VN_NARY;
		return VN_NONE;
	      }
	  default:
	    return VN_NONE;
	  }
      }
    default:
      return VN_NONE;
    }
}

/* Lookup a value id for CONSTANT and return it.  If it does not
   exist returns 0.  */

unsigned int
get_constant_value_id (tree constant)
{
  vn_constant_s **slot;
  struct vn_constant_s vc;

  vc.hashcode = vn_hash_constant_with_type (constant);
  vc.constant = constant;
  slot = constant_to_value_id.find_slot_with_hash (&vc, vc.hashcode, NO_INSERT);
  if (slot)
    return (*slot)->value_id;
  return 0;
}

/* Lookup a value id for CONSTANT, and if it does not exist, create a
   new one and return it.  If it does exist, return it.  */

unsigned int
get_or_alloc_constant_value_id (tree constant)
{
  vn_constant_s **slot;
  struct vn_constant_s vc;
  vn_constant_t vcp;

  vc.hashcode = vn_hash_constant_with_type (constant);
  vc.constant = constant;
  slot = constant_to_value_id.find_slot_with_hash (&vc, vc.hashcode, INSERT);
  if (*slot)
    return (*slot)->value_id;

  vcp = XNEW (struct vn_constant_s);
  vcp->hashcode = vc.hashcode;
  vcp->constant = constant;
  vcp->value_id = get_next_value_id ();
  *slot = vcp;
  bitmap_set_bit (constant_value_ids, vcp->value_id);
  return vcp->value_id;
}

/* Return true if V is a value id for a constant.  */

bool
value_id_constant_p (unsigned int v)
{
  return bitmap_bit_p (constant_value_ids, v);
}

/* Compute the hash for a reference operand VRO1.  */

static hashval_t
vn_reference_op_compute_hash (const vn_reference_op_t vro1, hashval_t result)
{
  result = iterative_hash_hashval_t (vro1->opcode, result);
  if (vro1->op0)
    result = iterative_hash_expr (vro1->op0, result);
  if (vro1->op1)
    result = iterative_hash_expr (vro1->op1, result);
  if (vro1->op2)
    result = iterative_hash_expr (vro1->op2, result);
  return result;
}

/* Compute a hash for the reference operation VR1 and return it.  */

hashval_t
vn_reference_compute_hash (const vn_reference_t vr1)
{
  hashval_t result = 0;
  int i;
  vn_reference_op_t vro;
  HOST_WIDE_INT off = -1;
  bool deref = false;

  FOR_EACH_VEC_ELT (vr1->operands, i, vro)
    {
      if (vro->opcode == MEM_REF)
	deref = true;
      else if (vro->opcode != ADDR_EXPR)
	deref = false;
      if (vro->off != -1)
	{
	  if (off == -1)
	    off = 0;
	  off += vro->off;
	}
      else
	{
	  if (off != -1
	      && off != 0)
	    result = iterative_hash_hashval_t (off, result);
	  off = -1;
	  if (deref
	      && vro->opcode == ADDR_EXPR)
	    {
	      if (vro->op0)
		{
		  tree op = TREE_OPERAND (vro->op0, 0);
		  result = iterative_hash_hashval_t (TREE_CODE (op), result);
		  result = iterative_hash_expr (op, result);
		}
	    }
	  else
	    result = vn_reference_op_compute_hash (vro, result);
	}
    }
  if (vr1->vuse)
    result += SSA_NAME_VERSION (vr1->vuse);

  return result;
}

/* Return true if reference operations VR1 and VR2 are equivalent.  This
   means they have the same set of operands and vuses.  */

bool
vn_reference_eq (const_vn_reference_t const vr1, const_vn_reference_t const vr2)
{
  unsigned i, j;

  /* Early out if this is not a hash collision.  */
  if (vr1->hashcode != vr2->hashcode)
    return false;

  /* The VOP needs to be the same.  */
  if (vr1->vuse != vr2->vuse)
    return false;

  /* If the operands are the same we are done.  */
  if (vr1->operands == vr2->operands)
    return true;

  if (!expressions_equal_p (TYPE_SIZE (vr1->type), TYPE_SIZE (vr2->type)))
    return false;

  if (INTEGRAL_TYPE_P (vr1->type)
      && INTEGRAL_TYPE_P (vr2->type))
    {
      if (TYPE_PRECISION (vr1->type) != TYPE_PRECISION (vr2->type))
	return false;
    }
  else if (INTEGRAL_TYPE_P (vr1->type)
	   && (TYPE_PRECISION (vr1->type)
	       != TREE_INT_CST_LOW (TYPE_SIZE (vr1->type))))
    return false;
  else if (INTEGRAL_TYPE_P (vr2->type)
	   && (TYPE_PRECISION (vr2->type)
	       != TREE_INT_CST_LOW (TYPE_SIZE (vr2->type))))
    return false;

  i = 0;
  j = 0;
  do
    {
      HOST_WIDE_INT off1 = 0, off2 = 0;
      vn_reference_op_t vro1, vro2;
      vn_reference_op_s tem1, tem2;
      bool deref1 = false, deref2 = false;
      for (; vr1->operands.iterate (i, &vro1); i++)
	{
	  if (vro1->opcode == MEM_REF)
	    deref1 = true;
	  if (vro1->off == -1)
	    break;
	  off1 += vro1->off;
	}
      for (; vr2->operands.iterate (j, &vro2); j++)
	{
	  if (vro2->opcode == MEM_REF)
	    deref2 = true;
	  if (vro2->off == -1)
	    break;
	  off2 += vro2->off;
	}
      if (off1 != off2)
	return false;
      if (deref1 && vro1->opcode == ADDR_EXPR)
	{
	  memset (&tem1, 0, sizeof (tem1));
	  tem1.op0 = TREE_OPERAND (vro1->op0, 0);
	  tem1.type = TREE_TYPE (tem1.op0);
	  tem1.opcode = TREE_CODE (tem1.op0);
	  vro1 = &tem1;
	  deref1 = false;
	}
      if (deref2 && vro2->opcode == ADDR_EXPR)
	{
	  memset (&tem2, 0, sizeof (tem2));
	  tem2.op0 = TREE_OPERAND (vro2->op0, 0);
	  tem2.type = TREE_TYPE (tem2.op0);
	  tem2.opcode = TREE_CODE (tem2.op0);
	  vro2 = &tem2;
	  deref2 = false;
	}
      if (deref1 != deref2)
	return false;
      if (!vn_reference_op_eq (vro1, vro2))
	return false;
      ++j;
      ++i;
    }
  while (vr1->operands.length () != i
	 || vr2->operands.length () != j);

  return true;
}

/* Copy the operations present in load/store REF into RESULT, a vector of
   vn_reference_op_s's.  */

void
copy_reference_ops_from_ref (tree ref, vec<vn_reference_op_s> *result)
{
  if (TREE_CODE (ref) == TARGET_MEM_REF)
    {
      vn_reference_op_s temp;

      result->reserve (3);

      memset (&temp, 0, sizeof (temp));
      temp.type = TREE_TYPE (ref);
      temp.opcode = TREE_CODE (ref);
      temp.op0 = TMR_INDEX (ref);
      temp.op1 = TMR_STEP (ref);
      temp.op2 = TMR_OFFSET (ref);
      temp.off = -1;
      result->quick_push (temp);

      memset (&temp, 0, sizeof (temp));
      temp.type = NULL_TREE;
      temp.opcode = ERROR_MARK;
      temp.op0 = TMR_INDEX2 (ref);
      temp.off = -1;
      result->quick_push (temp);

      memset (&temp, 0, sizeof (temp));
      temp.type = NULL_TREE;
      temp.opcode = TREE_CODE (TMR_BASE (ref));
      temp.op0 = TMR_BASE (ref);
      temp.off = -1;
      result->quick_push (temp);
      return;
    }

  /* For non-calls, store the information that makes up the address.  */
  tree orig = ref;
  while (ref)
    {
      vn_reference_op_s temp;

      memset (&temp, 0, sizeof (temp));
      temp.type = TREE_TYPE (ref);
      temp.opcode = TREE_CODE (ref);
      temp.off = -1;

      switch (temp.opcode)
	{
	case MODIFY_EXPR:
	  temp.op0 = TREE_OPERAND (ref, 1);
	  break;
	case WITH_SIZE_EXPR:
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.off = 0;
	  break;
	case MEM_REF:
	  /* The base address gets its own vn_reference_op_s structure.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  if (tree_fits_shwi_p (TREE_OPERAND (ref, 1)))
	    temp.off = tree_to_shwi (TREE_OPERAND (ref, 1));
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
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  {
	    tree this_offset = component_ref_field_offset (ref);
	    if (this_offset
		&& TREE_CODE (this_offset) == INTEGER_CST)
	      {
		tree bit_offset = DECL_FIELD_BIT_OFFSET (TREE_OPERAND (ref, 1));
		if (TREE_INT_CST_LOW (bit_offset) % BITS_PER_UNIT == 0)
		  {
		    offset_int off
		      = (wi::to_offset (this_offset)
			 + wi::lrshift (wi::to_offset (bit_offset),
					LOG2_BITS_PER_UNIT));
		    if (wi::fits_shwi_p (off)
			/* Probibit value-numbering zero offset components
			   of addresses the same before the pass folding
			   __builtin_object_size had a chance to run
			   (checking cfun->after_inlining does the
			   trick here).  */
			&& (TREE_CODE (orig) != ADDR_EXPR
			    || off != 0
			    || cfun->after_inlining))
		      temp.off = off.to_shwi ();
		  }
	      }
	  }
	  break;
	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  /* Record index as operand.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  /* Always record lower bounds and element size.  */
	  temp.op1 = array_ref_low_bound (ref);
	  temp.op2 = array_ref_element_size (ref);
	  if (TREE_CODE (temp.op0) == INTEGER_CST
	      && TREE_CODE (temp.op1) == INTEGER_CST
	      && TREE_CODE (temp.op2) == INTEGER_CST)
	    {
	      offset_int off = ((wi::to_offset (temp.op0)
				 - wi::to_offset (temp.op1))
				* wi::to_offset (temp.op2));
	      if (wi::fits_shwi_p (off))
		temp.off = off.to_shwi();
	    }
	  break;
	case VAR_DECL:
	  if (DECL_HARD_REGISTER (ref))
	    {
	      temp.op0 = ref;
	      break;
	    }
	  /* Fallthru.  */
	case PARM_DECL:
	case CONST_DECL:
	case RESULT_DECL:
	  /* Canonicalize decls to MEM[&decl] which is what we end up with
	     when valueizing MEM[ptr] with ptr = &decl.  */
	  temp.opcode = MEM_REF;
	  temp.op0 = build_int_cst (build_pointer_type (TREE_TYPE (ref)), 0);
	  temp.off = 0;
	  result->safe_push (temp);
	  temp.opcode = ADDR_EXPR;
	  temp.op0 = build1 (ADDR_EXPR, TREE_TYPE (temp.op0), ref);
	  temp.type = TREE_TYPE (temp.op0);
	  temp.off = -1;
	  break;
	case STRING_CST:
	case INTEGER_CST:
	case COMPLEX_CST:
	case VECTOR_CST:
	case REAL_CST:
	case FIXED_CST:
	case CONSTRUCTOR:
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
	case REALPART_EXPR:
	case VIEW_CONVERT_EXPR:
	  temp.off = 0;
	  break;
	case IMAGPART_EXPR:
	  /* This is only interesting for its constant offset.  */
	  temp.off = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (ref)));
	  break;
	default:
	  gcc_unreachable ();
	}
      result->safe_push (temp);

      if (REFERENCE_CLASS_P (ref)
	  || TREE_CODE (ref) == MODIFY_EXPR
	  || TREE_CODE (ref) == WITH_SIZE_EXPR
	  || (TREE_CODE (ref) == ADDR_EXPR
	      && !is_gimple_min_invariant (ref)))
	ref = TREE_OPERAND (ref, 0);
      else
	ref = NULL_TREE;
    }
}

/* Build a alias-oracle reference abstraction in *REF from the vn_reference
   operands in *OPS, the reference alias set SET and the reference type TYPE.
   Return true if something useful was produced.  */

bool
ao_ref_init_from_vn_reference (ao_ref *ref,
			       alias_set_type set, tree type,
			       vec<vn_reference_op_s> ops)
{
  vn_reference_op_t op;
  unsigned i;
  tree base = NULL_TREE;
  tree *op0_p = &base;
  HOST_WIDE_INT offset = 0;
  HOST_WIDE_INT max_size;
  HOST_WIDE_INT size = -1;
  tree size_tree = NULL_TREE;
  alias_set_type base_alias_set = -1;

  /* First get the final access size from just the outermost expression.  */
  op = &ops[0];
  if (op->opcode == COMPONENT_REF)
    size_tree = DECL_SIZE (op->op0);
  else if (op->opcode == BIT_FIELD_REF)
    size_tree = op->op0;
  else
    {
      enum machine_mode mode = TYPE_MODE (type);
      if (mode == BLKmode)
	size_tree = TYPE_SIZE (type);
      else
        size = GET_MODE_BITSIZE (mode);
    }
  if (size_tree != NULL_TREE)
    {
      if (!tree_fits_uhwi_p (size_tree))
	size = -1;
      else
	size = tree_to_uhwi (size_tree);
    }

  /* Initially, maxsize is the same as the accessed element size.
     In the following it will only grow (or become -1).  */
  max_size = size;

  /* Compute cumulative bit-offset for nested component-refs and array-refs,
     and find the ultimate containing object.  */
  FOR_EACH_VEC_ELT (ops, i, op)
    {
      switch (op->opcode)
	{
	/* These may be in the reference ops, but we cannot do anything
	   sensible with them here.  */
	case ADDR_EXPR:
	  /* Apart from ADDR_EXPR arguments to MEM_REF.  */
	  if (base != NULL_TREE
	      && TREE_CODE (base) == MEM_REF
	      && op->op0
	      && DECL_P (TREE_OPERAND (op->op0, 0)))
	    {
	      vn_reference_op_t pop = &ops[i-1];
	      base = TREE_OPERAND (op->op0, 0);
	      if (pop->off == -1)
		{
		  max_size = -1;
		  offset = 0;
		}
	      else
		offset += pop->off * BITS_PER_UNIT;
	      op0_p = NULL;
	      break;
	    }
	  /* Fallthru.  */
	case CALL_EXPR:
	  return false;

	/* Record the base objects.  */
	case MEM_REF:
	  base_alias_set = get_deref_alias_set (op->op0);
	  *op0_p = build2 (MEM_REF, op->type,
			   NULL_TREE, op->op0);
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case VAR_DECL:
	case PARM_DECL:
	case RESULT_DECL:
	case SSA_NAME:
	  *op0_p = op->op0;
	  op0_p = NULL;
	  break;

	/* And now the usual component-reference style ops.  */
	case BIT_FIELD_REF:
	  offset += tree_to_shwi (op->op1);
	  break;

	case COMPONENT_REF:
	  {
	    tree field = op->op0;
	    /* We do not have a complete COMPONENT_REF tree here so we
	       cannot use component_ref_field_offset.  Do the interesting
	       parts manually.  */

	    if (op->op1
		|| !tree_fits_uhwi_p (DECL_FIELD_OFFSET (field)))
	      max_size = -1;
	    else
	      {
		offset += (tree_to_uhwi (DECL_FIELD_OFFSET (field))
			   * BITS_PER_UNIT);
		offset += TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (field));
	      }
	    break;
	  }

	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  /* We recorded the lower bound and the element size.  */
	  if (!tree_fits_shwi_p (op->op0)
	      || !tree_fits_shwi_p (op->op1)
	      || !tree_fits_shwi_p (op->op2))
	    max_size = -1;
	  else
	    {
	      HOST_WIDE_INT hindex = tree_to_shwi (op->op0);
	      hindex -= tree_to_shwi (op->op1);
	      hindex *= tree_to_shwi (op->op2);
	      hindex *= BITS_PER_UNIT;
	      offset += hindex;
	    }
	  break;

	case REALPART_EXPR:
	  break;

	case IMAGPART_EXPR:
	  offset += size;
	  break;

	case VIEW_CONVERT_EXPR:
	  break;

	case STRING_CST:
	case INTEGER_CST:
	case COMPLEX_CST:
	case VECTOR_CST:
	case REAL_CST:
	case CONSTRUCTOR:
	case CONST_DECL:
	  return false;

	default:
	  return false;
	}
    }

  if (base == NULL_TREE)
    return false;

  ref->ref = NULL_TREE;
  ref->base = base;
  ref->offset = offset;
  ref->size = size;
  ref->max_size = max_size;
  ref->ref_alias_set = set;
  if (base_alias_set != -1)
    ref->base_alias_set = base_alias_set;
  else
    ref->base_alias_set = get_alias_set (base);
  /* We discount volatiles from value-numbering elsewhere.  */
  ref->volatile_p = false;

  return true;
}

/* Copy the operations present in load/store/call REF into RESULT, a vector of
   vn_reference_op_s's.  */

void
copy_reference_ops_from_call (gimple call,
			      vec<vn_reference_op_s> *result)
{
  vn_reference_op_s temp;
  unsigned i;
  tree lhs = gimple_call_lhs (call);
  int lr;

  /* If 2 calls have a different non-ssa lhs, vdef value numbers should be
     different.  By adding the lhs here in the vector, we ensure that the
     hashcode is different, guaranteeing a different value number.  */
  if (lhs && TREE_CODE (lhs) != SSA_NAME)
    {
      memset (&temp, 0, sizeof (temp));
      temp.opcode = MODIFY_EXPR;
      temp.type = TREE_TYPE (lhs);
      temp.op0 = lhs;
      temp.off = -1;
      result->safe_push (temp);
    }

  /* Copy the type, opcode, function, static chain and EH region, if any.  */
  memset (&temp, 0, sizeof (temp));
  temp.type = gimple_call_return_type (call);
  temp.opcode = CALL_EXPR;
  temp.op0 = gimple_call_fn (call);
  temp.op1 = gimple_call_chain (call);
  if (stmt_could_throw_p (call) && (lr = lookup_stmt_eh_lp (call)) > 0)
    temp.op2 = size_int (lr);
  temp.off = -1;
  result->safe_push (temp);

  /* Copy the call arguments.  As they can be references as well,
     just chain them together.  */
  for (i = 0; i < gimple_call_num_args (call); ++i)
    {
      tree callarg = gimple_call_arg (call, i);
      copy_reference_ops_from_ref (callarg, result);
    }
}

/* Create a vector of vn_reference_op_s structures from CALL, a
   call statement.  The vector is not shared.  */

static vec<vn_reference_op_s> 
create_reference_ops_from_call (gimple call)
{
  vec<vn_reference_op_s> result = vNULL;

  copy_reference_ops_from_call (call, &result);
  return result;
}

/* Fold *& at position *I_P in a vn_reference_op_s vector *OPS.  Updates
   *I_P to point to the last element of the replacement.  */
void
vn_reference_fold_indirect (vec<vn_reference_op_s> *ops,
			    unsigned int *i_p)
{
  unsigned int i = *i_p;
  vn_reference_op_t op = &(*ops)[i];
  vn_reference_op_t mem_op = &(*ops)[i - 1];
  tree addr_base;
  HOST_WIDE_INT addr_offset = 0;

  /* The only thing we have to do is from &OBJ.foo.bar add the offset
     from .foo.bar to the preceding MEM_REF offset and replace the
     address with &OBJ.  */
  addr_base = get_addr_base_and_unit_offset (TREE_OPERAND (op->op0, 0),
					     &addr_offset);
  gcc_checking_assert (addr_base && TREE_CODE (addr_base) != MEM_REF);
  if (addr_base != TREE_OPERAND (op->op0, 0))
    {
      offset_int off = offset_int::from (mem_op->op0, SIGNED);
      off += addr_offset;
      mem_op->op0 = wide_int_to_tree (TREE_TYPE (mem_op->op0), off);
      op->op0 = build_fold_addr_expr (addr_base);
      if (tree_fits_shwi_p (mem_op->op0))
	mem_op->off = tree_to_shwi (mem_op->op0);
      else
	mem_op->off = -1;
    }
}

/* Fold *& at position *I_P in a vn_reference_op_s vector *OPS.  Updates
   *I_P to point to the last element of the replacement.  */
static void
vn_reference_maybe_forwprop_address (vec<vn_reference_op_s> *ops,
				     unsigned int *i_p)
{
  unsigned int i = *i_p;
  vn_reference_op_t op = &(*ops)[i];
  vn_reference_op_t mem_op = &(*ops)[i - 1];
  gimple def_stmt;
  enum tree_code code;
  offset_int off;

  def_stmt = SSA_NAME_DEF_STMT (op->op0);
  if (!is_gimple_assign (def_stmt))
    return;

  code = gimple_assign_rhs_code (def_stmt);
  if (code != ADDR_EXPR
      && code != POINTER_PLUS_EXPR)
    return;

  off = offset_int::from (mem_op->op0, SIGNED);

  /* The only thing we have to do is from &OBJ.foo.bar add the offset
     from .foo.bar to the preceding MEM_REF offset and replace the
     address with &OBJ.  */
  if (code == ADDR_EXPR)
    {
      tree addr, addr_base;
      HOST_WIDE_INT addr_offset;

      addr = gimple_assign_rhs1 (def_stmt);
      addr_base = get_addr_base_and_unit_offset (TREE_OPERAND (addr, 0),
						 &addr_offset);
      if (!addr_base
	  || TREE_CODE (addr_base) != MEM_REF)
	return;

      off += addr_offset;
      off += mem_ref_offset (addr_base);
      op->op0 = TREE_OPERAND (addr_base, 0);
    }
  else
    {
      tree ptr, ptroff;
      ptr = gimple_assign_rhs1 (def_stmt);
      ptroff = gimple_assign_rhs2 (def_stmt);
      if (TREE_CODE (ptr) != SSA_NAME
	  || TREE_CODE (ptroff) != INTEGER_CST)
	return;

      off += wi::to_offset (ptroff);
      op->op0 = ptr;
    }

  mem_op->op0 = wide_int_to_tree (TREE_TYPE (mem_op->op0), off);
  if (tree_fits_shwi_p (mem_op->op0))
    mem_op->off = tree_to_shwi (mem_op->op0);
  else
    mem_op->off = -1;
  if (TREE_CODE (op->op0) == SSA_NAME)
    op->op0 = SSA_VAL (op->op0);
  if (TREE_CODE (op->op0) != SSA_NAME)
    op->opcode = TREE_CODE (op->op0);

  /* And recurse.  */
  if (TREE_CODE (op->op0) == SSA_NAME)
    vn_reference_maybe_forwprop_address (ops, i_p);
  else if (TREE_CODE (op->op0) == ADDR_EXPR)
    vn_reference_fold_indirect (ops, i_p);
}

/* Optimize the reference REF to a constant if possible or return
   NULL_TREE if not.  */

tree
fully_constant_vn_reference_p (vn_reference_t ref)
{
  vec<vn_reference_op_s> operands = ref->operands;
  vn_reference_op_t op;

  /* Try to simplify the translated expression if it is
     a call to a builtin function with at most two arguments.  */
  op = &operands[0];
  if (op->opcode == CALL_EXPR
      && TREE_CODE (op->op0) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (op->op0, 0)) == FUNCTION_DECL
      && DECL_BUILT_IN (TREE_OPERAND (op->op0, 0))
      && operands.length () >= 2
      && operands.length () <= 3)
    {
      vn_reference_op_t arg0, arg1 = NULL;
      bool anyconst = false;
      arg0 = &operands[1];
      if (operands.length () > 2)
	arg1 = &operands[2];
      if (TREE_CODE_CLASS (arg0->opcode) == tcc_constant
	  || (arg0->opcode == ADDR_EXPR
	      && is_gimple_min_invariant (arg0->op0)))
	anyconst = true;
      if (arg1
	  && (TREE_CODE_CLASS (arg1->opcode) == tcc_constant
	      || (arg1->opcode == ADDR_EXPR
		  && is_gimple_min_invariant (arg1->op0))))
	anyconst = true;
      if (anyconst)
	{
	  tree folded = build_call_expr (TREE_OPERAND (op->op0, 0),
					 arg1 ? 2 : 1,
					 arg0->op0,
					 arg1 ? arg1->op0 : NULL);
	  if (folded
	      && TREE_CODE (folded) == NOP_EXPR)
	    folded = TREE_OPERAND (folded, 0);
	  if (folded
	      && is_gimple_min_invariant (folded))
	    return folded;
	}
    }

  /* Simplify reads from constant strings.  */
  else if (op->opcode == ARRAY_REF
	   && TREE_CODE (op->op0) == INTEGER_CST
	   && integer_zerop (op->op1)
	   && operands.length () == 2)
    {
      vn_reference_op_t arg0;
      arg0 = &operands[1];
      if (arg0->opcode == STRING_CST
	  && (TYPE_MODE (op->type)
	      == TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0->op0))))
	  && GET_MODE_CLASS (TYPE_MODE (op->type)) == MODE_INT
	  && GET_MODE_SIZE (TYPE_MODE (op->type)) == 1
	  && tree_int_cst_sgn (op->op0) >= 0
	  && compare_tree_int (op->op0, TREE_STRING_LENGTH (arg0->op0)) < 0)
	return build_int_cst_type (op->type,
				   (TREE_STRING_POINTER (arg0->op0)
				    [TREE_INT_CST_LOW (op->op0)]));
    }

  return NULL_TREE;
}

/* Transform any SSA_NAME's in a vector of vn_reference_op_s
   structures into their value numbers.  This is done in-place, and
   the vector passed in is returned.  *VALUEIZED_ANYTHING will specify
   whether any operands were valueized.  */

static vec<vn_reference_op_s> 
valueize_refs_1 (vec<vn_reference_op_s> orig, bool *valueized_anything)
{
  vn_reference_op_t vro;
  unsigned int i;

  *valueized_anything = false;

  FOR_EACH_VEC_ELT (orig, i, vro)
    {
      if (vro->opcode == SSA_NAME
	  || (vro->op0 && TREE_CODE (vro->op0) == SSA_NAME))
	{
	  tree tem = SSA_VAL (vro->op0);
	  if (tem != vro->op0)
	    {
	      *valueized_anything = true;
	      vro->op0 = tem;
	    }
	  /* If it transforms from an SSA_NAME to a constant, update
	     the opcode.  */
	  if (TREE_CODE (vro->op0) != SSA_NAME && vro->opcode == SSA_NAME)
	    vro->opcode = TREE_CODE (vro->op0);
	}
      if (vro->op1 && TREE_CODE (vro->op1) == SSA_NAME)
	{
	  tree tem = SSA_VAL (vro->op1);
	  if (tem != vro->op1)
	    {
	      *valueized_anything = true;
	      vro->op1 = tem;
	    }
	}
      if (vro->op2 && TREE_CODE (vro->op2) == SSA_NAME)
	{
	  tree tem = SSA_VAL (vro->op2);
	  if (tem != vro->op2)
	    {
	      *valueized_anything = true;
	      vro->op2 = tem;
	    }
	}
      /* If it transforms from an SSA_NAME to an address, fold with
	 a preceding indirect reference.  */
      if (i > 0
	  && vro->op0
	  && TREE_CODE (vro->op0) == ADDR_EXPR
	  && orig[i - 1].opcode == MEM_REF)
	vn_reference_fold_indirect (&orig, &i);
      else if (i > 0
	       && vro->opcode == SSA_NAME
	       && orig[i - 1].opcode == MEM_REF)
	vn_reference_maybe_forwprop_address (&orig, &i);
      /* If it transforms a non-constant ARRAY_REF into a constant
	 one, adjust the constant offset.  */
      else if (vro->opcode == ARRAY_REF
	       && vro->off == -1
	       && TREE_CODE (vro->op0) == INTEGER_CST
	       && TREE_CODE (vro->op1) == INTEGER_CST
	       && TREE_CODE (vro->op2) == INTEGER_CST)
	{
	  offset_int off = ((wi::to_offset (vro->op0)
			     - wi::to_offset (vro->op1))
			    * wi::to_offset (vro->op2));
	  if (wi::fits_shwi_p (off))
	    vro->off = off.to_shwi ();
	}
    }

  return orig;
}

static vec<vn_reference_op_s> 
valueize_refs (vec<vn_reference_op_s> orig)
{
  bool tem;
  return valueize_refs_1 (orig, &tem);
}

static vec<vn_reference_op_s> shared_lookup_references;

/* Create a vector of vn_reference_op_s structures from REF, a
   REFERENCE_CLASS_P tree.  The vector is shared among all callers of
   this function.  *VALUEIZED_ANYTHING will specify whether any
   operands were valueized.  */

static vec<vn_reference_op_s> 
valueize_shared_reference_ops_from_ref (tree ref, bool *valueized_anything)
{
  if (!ref)
    return vNULL;
  shared_lookup_references.truncate (0);
  copy_reference_ops_from_ref (ref, &shared_lookup_references);
  shared_lookup_references = valueize_refs_1 (shared_lookup_references,
					      valueized_anything);
  return shared_lookup_references;
}

/* Create a vector of vn_reference_op_s structures from CALL, a
   call statement.  The vector is shared among all callers of
   this function.  */

static vec<vn_reference_op_s> 
valueize_shared_reference_ops_from_call (gimple call)
{
  if (!call)
    return vNULL;
  shared_lookup_references.truncate (0);
  copy_reference_ops_from_call (call, &shared_lookup_references);
  shared_lookup_references = valueize_refs (shared_lookup_references);
  return shared_lookup_references;
}

/* Lookup a SCCVN reference operation VR in the current hash table.
   Returns the resulting value number if it exists in the hash table,
   NULL_TREE otherwise.  VNRESULT will be filled in with the actual
   vn_reference_t stored in the hashtable if something is found.  */

static tree
vn_reference_lookup_1 (vn_reference_t vr, vn_reference_t *vnresult)
{
  vn_reference_s **slot;
  hashval_t hash;

  hash = vr->hashcode;
  slot = current_info->references.find_slot_with_hash (vr, hash, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->references.find_slot_with_hash (vr, hash, NO_INSERT);
  if (slot)
    {
      if (vnresult)
	*vnresult = (vn_reference_t)*slot;
      return ((vn_reference_t)*slot)->result;
    }

  return NULL_TREE;
}

static tree *last_vuse_ptr;
static vn_lookup_kind vn_walk_kind;
static vn_lookup_kind default_vn_walk_kind;

/* Callback for walk_non_aliased_vuses.  Adjusts the vn_reference_t VR_
   with the current VUSE and performs the expression lookup.  */

static void *
vn_reference_lookup_2 (ao_ref *op ATTRIBUTE_UNUSED, tree vuse,
		       unsigned int cnt, void *vr_)
{
  vn_reference_t vr = (vn_reference_t)vr_;
  vn_reference_s **slot;
  hashval_t hash;

  /* This bounds the stmt walks we perform on reference lookups
     to O(1) instead of O(N) where N is the number of dominating
     stores.  */
  if (cnt > (unsigned) PARAM_VALUE (PARAM_SCCVN_MAX_ALIAS_QUERIES_PER_ACCESS))
    return (void *)-1;

  if (last_vuse_ptr)
    *last_vuse_ptr = vuse;

  /* Fixup vuse and hash.  */
  if (vr->vuse)
    vr->hashcode = vr->hashcode - SSA_NAME_VERSION (vr->vuse);
  vr->vuse = vuse_ssa_val (vuse);
  if (vr->vuse)
    vr->hashcode = vr->hashcode + SSA_NAME_VERSION (vr->vuse);

  hash = vr->hashcode;
  slot = current_info->references.find_slot_with_hash (vr, hash, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->references.find_slot_with_hash (vr, hash, NO_INSERT);
  if (slot)
    return *slot;

  return NULL;
}

/* Lookup an existing or insert a new vn_reference entry into the
   value table for the VUSE, SET, TYPE, OPERANDS reference which
   has the value VALUE which is either a constant or an SSA name.  */

static vn_reference_t
vn_reference_lookup_or_insert_for_pieces (tree vuse,
					  alias_set_type set,
					  tree type,
					  vec<vn_reference_op_s,
					        va_heap> operands,
					  tree value)
{
  struct vn_reference_s vr1;
  vn_reference_t result;
  unsigned value_id;
  vr1.vuse = vuse;
  vr1.operands = operands;
  vr1.type = type;
  vr1.set = set;
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  if (vn_reference_lookup_1 (&vr1, &result))
    return result;
  if (TREE_CODE (value) == SSA_NAME)
    value_id = VN_INFO (value)->value_id;
  else
    value_id = get_or_alloc_constant_value_id (value);
  return vn_reference_insert_pieces (vuse, set, type,
				     operands.copy (), value, value_id);
}

/* Callback for walk_non_aliased_vuses.  Tries to perform a lookup
   from the statement defining VUSE and if not successful tries to
   translate *REFP and VR_ through an aggregate copy at the definition
   of VUSE.  */

static void *
vn_reference_lookup_3 (ao_ref *ref, tree vuse, void *vr_,
		       bool disambiguate_only)
{
  vn_reference_t vr = (vn_reference_t)vr_;
  gimple def_stmt = SSA_NAME_DEF_STMT (vuse);
  tree base;
  HOST_WIDE_INT offset, maxsize;
  static vec<vn_reference_op_s>
    lhs_ops = vNULL;
  ao_ref lhs_ref;
  bool lhs_ref_ok = false;

  /* First try to disambiguate after value-replacing in the definitions LHS.  */
  if (is_gimple_assign (def_stmt))
    {
      vec<vn_reference_op_s> tem;
      tree lhs = gimple_assign_lhs (def_stmt);
      bool valueized_anything = false;
      /* Avoid re-allocation overhead.  */
      lhs_ops.truncate (0);
      copy_reference_ops_from_ref (lhs, &lhs_ops);
      tem = lhs_ops;
      lhs_ops = valueize_refs_1 (lhs_ops, &valueized_anything);
      gcc_assert (lhs_ops == tem);
      if (valueized_anything)
	{
	  lhs_ref_ok = ao_ref_init_from_vn_reference (&lhs_ref,
						      get_alias_set (lhs),
						      TREE_TYPE (lhs), lhs_ops);
	  if (lhs_ref_ok
	      && !refs_may_alias_p_1 (ref, &lhs_ref, true))
	    return NULL;
	}
      else
	{
	  ao_ref_init (&lhs_ref, lhs);
	  lhs_ref_ok = true;
	}
    }
  else if (gimple_call_builtin_p (def_stmt, BUILT_IN_NORMAL)
	   && gimple_call_num_args (def_stmt) <= 4)
    {
      /* For builtin calls valueize its arguments and call the
         alias oracle again.  Valueization may improve points-to
	 info of pointers and constify size and position arguments.
	 Originally this was motivated by PR61034 which has
	 conditional calls to free falsely clobbering ref because
	 of imprecise points-to info of the argument.  */
      tree oldargs[4];
      bool valueized_anything = false;
      for (unsigned i = 0; i < gimple_call_num_args (def_stmt); ++i)
	{
	  oldargs[i] = gimple_call_arg (def_stmt, i);
	  if (TREE_CODE (oldargs[i]) == SSA_NAME
	      && VN_INFO (oldargs[i])->valnum != oldargs[i])
	    {
	      gimple_call_set_arg (def_stmt, i, VN_INFO (oldargs[i])->valnum);
	      valueized_anything = true;
	    }
	}
      if (valueized_anything)
	{
	  bool res = call_may_clobber_ref_p_1 (def_stmt, ref);
	  for (unsigned i = 0; i < gimple_call_num_args (def_stmt); ++i)
	    gimple_call_set_arg (def_stmt, i, oldargs[i]);
	  if (!res)
	    return NULL;
	}
    }

  if (disambiguate_only)
    return (void *)-1;

  base = ao_ref_base (ref);
  offset = ref->offset;
  maxsize = ref->max_size;

  /* If we cannot constrain the size of the reference we cannot
     test if anything kills it.  */
  if (maxsize == -1)
    return (void *)-1;

  /* We can't deduce anything useful from clobbers.  */
  if (gimple_clobber_p (def_stmt))
    return (void *)-1;

  /* def_stmt may-defs *ref.  See if we can derive a value for *ref
     from that definition.
     1) Memset.  */
  if (is_gimple_reg_type (vr->type)
      && gimple_call_builtin_p (def_stmt, BUILT_IN_MEMSET)
      && integer_zerop (gimple_call_arg (def_stmt, 1))
      && tree_fits_uhwi_p (gimple_call_arg (def_stmt, 2))
      && TREE_CODE (gimple_call_arg (def_stmt, 0)) == ADDR_EXPR)
    {
      tree ref2 = TREE_OPERAND (gimple_call_arg (def_stmt, 0), 0);
      tree base2;
      HOST_WIDE_INT offset2, size2, maxsize2;
      base2 = get_ref_base_and_extent (ref2, &offset2, &size2, &maxsize2);
      size2 = tree_to_uhwi (gimple_call_arg (def_stmt, 2)) * 8;
      if ((unsigned HOST_WIDE_INT)size2 / 8
	  == tree_to_uhwi (gimple_call_arg (def_stmt, 2))
	  && maxsize2 != -1
	  && operand_equal_p (base, base2, 0)
	  && offset2 <= offset
	  && offset2 + size2 >= offset + maxsize)
	{
	  tree val = build_zero_cst (vr->type);
	  return vn_reference_lookup_or_insert_for_pieces
	           (vuse, vr->set, vr->type, vr->operands, val);
	}
    }

  /* 2) Assignment from an empty CONSTRUCTOR.  */
  else if (is_gimple_reg_type (vr->type)
	   && gimple_assign_single_p (def_stmt)
	   && gimple_assign_rhs_code (def_stmt) == CONSTRUCTOR
	   && CONSTRUCTOR_NELTS (gimple_assign_rhs1 (def_stmt)) == 0)
    {
      tree base2;
      HOST_WIDE_INT offset2, size2, maxsize2;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2);
      if (maxsize2 != -1
	  && operand_equal_p (base, base2, 0)
	  && offset2 <= offset
	  && offset2 + size2 >= offset + maxsize)
	{
	  tree val = build_zero_cst (vr->type);
	  return vn_reference_lookup_or_insert_for_pieces
	           (vuse, vr->set, vr->type, vr->operands, val);
	}
    }

  /* 3) Assignment from a constant.  We can use folds native encode/interpret
     routines to extract the assigned bits.  */
  else if (vn_walk_kind == VN_WALKREWRITE
	   && CHAR_BIT == 8 && BITS_PER_UNIT == 8
	   && ref->size == maxsize
	   && maxsize % BITS_PER_UNIT == 0
	   && offset % BITS_PER_UNIT == 0
	   && is_gimple_reg_type (vr->type)
	   && gimple_assign_single_p (def_stmt)
	   && is_gimple_min_invariant (gimple_assign_rhs1 (def_stmt)))
    {
      tree base2;
      HOST_WIDE_INT offset2, size2, maxsize2;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2);
      if (maxsize2 != -1
	  && maxsize2 == size2
	  && size2 % BITS_PER_UNIT == 0
	  && offset2 % BITS_PER_UNIT == 0
	  && operand_equal_p (base, base2, 0)
	  && offset2 <= offset
	  && offset2 + size2 >= offset + maxsize)
	{
	  /* We support up to 512-bit values (for V8DFmode).  */
	  unsigned char buffer[64];
	  int len;

	  len = native_encode_expr (gimple_assign_rhs1 (def_stmt),
				    buffer, sizeof (buffer));
	  if (len > 0)
	    {
	      tree val = native_interpret_expr (vr->type,
						buffer
						+ ((offset - offset2)
						   / BITS_PER_UNIT),
						ref->size / BITS_PER_UNIT);
	      if (val)
		return vn_reference_lookup_or_insert_for_pieces
		         (vuse, vr->set, vr->type, vr->operands, val);
	    }
	}
    }

  /* 4) Assignment from an SSA name which definition we may be able
     to access pieces from.  */
  else if (ref->size == maxsize
	   && is_gimple_reg_type (vr->type)
	   && gimple_assign_single_p (def_stmt)
	   && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
    {
      tree rhs1 = gimple_assign_rhs1 (def_stmt);
      gimple def_stmt2 = SSA_NAME_DEF_STMT (rhs1);
      if (is_gimple_assign (def_stmt2)
	  && (gimple_assign_rhs_code (def_stmt2) == COMPLEX_EXPR
	      || gimple_assign_rhs_code (def_stmt2) == CONSTRUCTOR)
	  && types_compatible_p (vr->type, TREE_TYPE (TREE_TYPE (rhs1))))
	{
	  tree base2;
	  HOST_WIDE_INT offset2, size2, maxsize2, off;
	  base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
					   &offset2, &size2, &maxsize2);
	  off = offset - offset2;
	  if (maxsize2 != -1
	      && maxsize2 == size2
	      && operand_equal_p (base, base2, 0)
	      && offset2 <= offset
	      && offset2 + size2 >= offset + maxsize)
	    {
	      tree val = NULL_TREE;
	      HOST_WIDE_INT elsz
		= TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (TREE_TYPE (rhs1))));
	      if (gimple_assign_rhs_code (def_stmt2) == COMPLEX_EXPR)
		{
		  if (off == 0)
		    val = gimple_assign_rhs1 (def_stmt2);
		  else if (off == elsz)
		    val = gimple_assign_rhs2 (def_stmt2);
		}
	      else if (gimple_assign_rhs_code (def_stmt2) == CONSTRUCTOR
		       && off % elsz == 0)
		{
		  tree ctor = gimple_assign_rhs1 (def_stmt2);
		  unsigned i = off / elsz;
		  if (i < CONSTRUCTOR_NELTS (ctor))
		    {
		      constructor_elt *elt = CONSTRUCTOR_ELT (ctor, i);
		      if (TREE_CODE (TREE_TYPE (rhs1)) == VECTOR_TYPE)
			{
			  if (TREE_CODE (TREE_TYPE (elt->value))
			      != VECTOR_TYPE)
			    val = elt->value;
			}
		    }
		}
	      if (val)
		return vn_reference_lookup_or_insert_for_pieces
		         (vuse, vr->set, vr->type, vr->operands, val);
	    }
	}
    }

  /* 5) For aggregate copies translate the reference through them if
     the copy kills ref.  */
  else if (vn_walk_kind == VN_WALKREWRITE
	   && gimple_assign_single_p (def_stmt)
	   && (DECL_P (gimple_assign_rhs1 (def_stmt))
	       || TREE_CODE (gimple_assign_rhs1 (def_stmt)) == MEM_REF
	       || handled_component_p (gimple_assign_rhs1 (def_stmt))))
    {
      tree base2;
      HOST_WIDE_INT offset2, size2, maxsize2;
      int i, j;
      auto_vec<vn_reference_op_s> rhs;
      vn_reference_op_t vro;
      ao_ref r;

      if (!lhs_ref_ok)
	return (void *)-1;

      /* See if the assignment kills REF.  */
      base2 = ao_ref_base (&lhs_ref);
      offset2 = lhs_ref.offset;
      size2 = lhs_ref.size;
      maxsize2 = lhs_ref.max_size;
      if (maxsize2 == -1
	  || (base != base2 && !operand_equal_p (base, base2, 0))
	  || offset2 > offset
	  || offset2 + size2 < offset + maxsize)
	return (void *)-1;

      /* Find the common base of ref and the lhs.  lhs_ops already
         contains valueized operands for the lhs.  */
      i = vr->operands.length () - 1;
      j = lhs_ops.length () - 1;
      while (j >= 0 && i >= 0
	     && vn_reference_op_eq (&vr->operands[i], &lhs_ops[j]))
	{
	  i--;
	  j--;
	}

      /* ???  The innermost op should always be a MEM_REF and we already
         checked that the assignment to the lhs kills vr.  Thus for
	 aggregate copies using char[] types the vn_reference_op_eq
	 may fail when comparing types for compatibility.  But we really
	 don't care here - further lookups with the rewritten operands
	 will simply fail if we messed up types too badly.  */
      if (j == 0 && i >= 0
	  && lhs_ops[0].opcode == MEM_REF
	  && lhs_ops[0].off != -1
	  && (lhs_ops[0].off == vr->operands[i].off))
	i--, j--;

      /* i now points to the first additional op.
	 ???  LHS may not be completely contained in VR, one or more
	 VIEW_CONVERT_EXPRs could be in its way.  We could at least
	 try handling outermost VIEW_CONVERT_EXPRs.  */
      if (j != -1)
	return (void *)-1;

      /* Now re-write REF to be based on the rhs of the assignment.  */
      copy_reference_ops_from_ref (gimple_assign_rhs1 (def_stmt), &rhs);
      /* We need to pre-pend vr->operands[0..i] to rhs.  */
      if (i + 1 + rhs.length () > vr->operands.length ())
	{
	  vec<vn_reference_op_s> old = vr->operands;
	  vr->operands.safe_grow (i + 1 + rhs.length ());
	  if (old == shared_lookup_references
	      && vr->operands != old)
	    shared_lookup_references = vNULL;
	}
      else
	vr->operands.truncate (i + 1 + rhs.length ());
      FOR_EACH_VEC_ELT (rhs, j, vro)
	vr->operands[i + 1 + j] = *vro;
      vr->operands = valueize_refs (vr->operands);
      vr->hashcode = vn_reference_compute_hash (vr);

      /* Adjust *ref from the new operands.  */
      if (!ao_ref_init_from_vn_reference (&r, vr->set, vr->type, vr->operands))
	return (void *)-1;
      /* This can happen with bitfields.  */
      if (ref->size != r.size)
	return (void *)-1;
      *ref = r;

      /* Do not update last seen VUSE after translating.  */
      last_vuse_ptr = NULL;

      /* Keep looking for the adjusted *REF / VR pair.  */
      return NULL;
    }

  /* 6) For memcpy copies translate the reference through them if
     the copy kills ref.  */
  else if (vn_walk_kind == VN_WALKREWRITE
	   && is_gimple_reg_type (vr->type)
	   /* ???  Handle BCOPY as well.  */
	   && (gimple_call_builtin_p (def_stmt, BUILT_IN_MEMCPY)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMPCPY)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMMOVE))
	   && (TREE_CODE (gimple_call_arg (def_stmt, 0)) == ADDR_EXPR
	       || TREE_CODE (gimple_call_arg (def_stmt, 0)) == SSA_NAME)
	   && (TREE_CODE (gimple_call_arg (def_stmt, 1)) == ADDR_EXPR
	       || TREE_CODE (gimple_call_arg (def_stmt, 1)) == SSA_NAME)
	   && tree_fits_uhwi_p (gimple_call_arg (def_stmt, 2)))
    {
      tree lhs, rhs;
      ao_ref r;
      HOST_WIDE_INT rhs_offset, copy_size, lhs_offset;
      vn_reference_op_s op;
      HOST_WIDE_INT at;


      /* Only handle non-variable, addressable refs.  */
      if (ref->size != maxsize
	  || offset % BITS_PER_UNIT != 0
	  || ref->size % BITS_PER_UNIT != 0)
	return (void *)-1;

      /* Extract a pointer base and an offset for the destination.  */
      lhs = gimple_call_arg (def_stmt, 0);
      lhs_offset = 0;
      if (TREE_CODE (lhs) == SSA_NAME)
	lhs = SSA_VAL (lhs);
      if (TREE_CODE (lhs) == ADDR_EXPR)
	{
	  tree tem = get_addr_base_and_unit_offset (TREE_OPERAND (lhs, 0),
						    &lhs_offset);
	  if (!tem)
	    return (void *)-1;
	  if (TREE_CODE (tem) == MEM_REF
	      && tree_fits_uhwi_p (TREE_OPERAND (tem, 1)))
	    {
	      lhs = TREE_OPERAND (tem, 0);
	      lhs_offset += tree_to_uhwi (TREE_OPERAND (tem, 1));
	    }
	  else if (DECL_P (tem))
	    lhs = build_fold_addr_expr (tem);
	  else
	    return (void *)-1;
	}
      if (TREE_CODE (lhs) != SSA_NAME
	  && TREE_CODE (lhs) != ADDR_EXPR)
	return (void *)-1;

      /* Extract a pointer base and an offset for the source.  */
      rhs = gimple_call_arg (def_stmt, 1);
      rhs_offset = 0;
      if (TREE_CODE (rhs) == SSA_NAME)
	rhs = SSA_VAL (rhs);
      if (TREE_CODE (rhs) == ADDR_EXPR)
	{
	  tree tem = get_addr_base_and_unit_offset (TREE_OPERAND (rhs, 0),
						    &rhs_offset);
	  if (!tem)
	    return (void *)-1;
	  if (TREE_CODE (tem) == MEM_REF
	      && tree_fits_uhwi_p (TREE_OPERAND (tem, 1)))
	    {
	      rhs = TREE_OPERAND (tem, 0);
	      rhs_offset += tree_to_uhwi (TREE_OPERAND (tem, 1));
	    }
	  else if (DECL_P (tem))
	    rhs = build_fold_addr_expr (tem);
	  else
	    return (void *)-1;
	}
      if (TREE_CODE (rhs) != SSA_NAME
	  && TREE_CODE (rhs) != ADDR_EXPR)
	return (void *)-1;

      copy_size = tree_to_uhwi (gimple_call_arg (def_stmt, 2));

      /* The bases of the destination and the references have to agree.  */
      if ((TREE_CODE (base) != MEM_REF
	   && !DECL_P (base))
	  || (TREE_CODE (base) == MEM_REF
	      && (TREE_OPERAND (base, 0) != lhs
		  || !tree_fits_uhwi_p (TREE_OPERAND (base, 1))))
	  || (DECL_P (base)
	      && (TREE_CODE (lhs) != ADDR_EXPR
		  || TREE_OPERAND (lhs, 0) != base)))
	return (void *)-1;

      /* And the access has to be contained within the memcpy destination.  */
      at = offset / BITS_PER_UNIT;
      if (TREE_CODE (base) == MEM_REF)
	at += tree_to_uhwi (TREE_OPERAND (base, 1));
      if (lhs_offset > at
	  || lhs_offset + copy_size < at + maxsize / BITS_PER_UNIT)
	return (void *)-1;

      /* Make room for 2 operands in the new reference.  */
      if (vr->operands.length () < 2)
	{
	  vec<vn_reference_op_s> old = vr->operands;
	  vr->operands.safe_grow_cleared (2);
	  if (old == shared_lookup_references
	      && vr->operands != old)
	    shared_lookup_references.create (0);
	}
      else
	vr->operands.truncate (2);

      /* The looked-through reference is a simple MEM_REF.  */
      memset (&op, 0, sizeof (op));
      op.type = vr->type;
      op.opcode = MEM_REF;
      op.op0 = build_int_cst (ptr_type_node, at - rhs_offset);
      op.off = at - lhs_offset + rhs_offset;
      vr->operands[0] = op;
      op.type = TREE_TYPE (rhs);
      op.opcode = TREE_CODE (rhs);
      op.op0 = rhs;
      op.off = -1;
      vr->operands[1] = op;
      vr->hashcode = vn_reference_compute_hash (vr);

      /* Adjust *ref from the new operands.  */
      if (!ao_ref_init_from_vn_reference (&r, vr->set, vr->type, vr->operands))
	return (void *)-1;
      /* This can happen with bitfields.  */
      if (ref->size != r.size)
	return (void *)-1;
      *ref = r;

      /* Do not update last seen VUSE after translating.  */
      last_vuse_ptr = NULL;

      /* Keep looking for the adjusted *REF / VR pair.  */
      return NULL;
    }

  /* Bail out and stop walking.  */
  return (void *)-1;
}

/* Lookup a reference operation by it's parts, in the current hash table.
   Returns the resulting value number if it exists in the hash table,
   NULL_TREE otherwise.  VNRESULT will be filled in with the actual
   vn_reference_t stored in the hashtable if something is found.  */

tree
vn_reference_lookup_pieces (tree vuse, alias_set_type set, tree type,
			    vec<vn_reference_op_s> operands,
			    vn_reference_t *vnresult, vn_lookup_kind kind)
{
  struct vn_reference_s vr1;
  vn_reference_t tmp;
  tree cst;

  if (!vnresult)
    vnresult = &tmp;
  *vnresult = NULL;

  vr1.vuse = vuse_ssa_val (vuse);
  shared_lookup_references.truncate (0);
  shared_lookup_references.safe_grow (operands.length ());
  memcpy (shared_lookup_references.address (),
	  operands.address (),
	  sizeof (vn_reference_op_s)
	  * operands.length ());
  vr1.operands = operands = shared_lookup_references
    = valueize_refs (shared_lookup_references);
  vr1.type = type;
  vr1.set = set;
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  if ((cst = fully_constant_vn_reference_p (&vr1)))
    return cst;

  vn_reference_lookup_1 (&vr1, vnresult);
  if (!*vnresult
      && kind != VN_NOWALK
      && vr1.vuse)
    {
      ao_ref r;
      vn_walk_kind = kind;
      if (ao_ref_init_from_vn_reference (&r, set, type, vr1.operands))
	*vnresult =
	  (vn_reference_t)walk_non_aliased_vuses (&r, vr1.vuse,
						  vn_reference_lookup_2,
						  vn_reference_lookup_3, &vr1);
      if (vr1.operands != operands)
	vr1.operands.release ();
    }

  if (*vnresult)
     return (*vnresult)->result;

  return NULL_TREE;
}

/* Lookup OP in the current hash table, and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the structure
   was NULL..  VNRESULT will be filled in with the vn_reference_t
   stored in the hashtable if one exists.  */

tree
vn_reference_lookup (tree op, tree vuse, vn_lookup_kind kind,
		     vn_reference_t *vnresult)
{
  vec<vn_reference_op_s> operands;
  struct vn_reference_s vr1;
  tree cst;
  bool valuezied_anything;

  if (vnresult)
    *vnresult = NULL;

  vr1.vuse = vuse_ssa_val (vuse);
  vr1.operands = operands
    = valueize_shared_reference_ops_from_ref (op, &valuezied_anything);
  vr1.type = TREE_TYPE (op);
  vr1.set = get_alias_set (op);
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  if ((cst = fully_constant_vn_reference_p (&vr1)))
    return cst;

  if (kind != VN_NOWALK
      && vr1.vuse)
    {
      vn_reference_t wvnresult;
      ao_ref r;
      /* Make sure to use a valueized reference if we valueized anything.
         Otherwise preserve the full reference for advanced TBAA.  */
      if (!valuezied_anything
	  || !ao_ref_init_from_vn_reference (&r, vr1.set, vr1.type,
					     vr1.operands))
	ao_ref_init (&r, op);
      vn_walk_kind = kind;
      wvnresult =
	(vn_reference_t)walk_non_aliased_vuses (&r, vr1.vuse,
						vn_reference_lookup_2,
						vn_reference_lookup_3, &vr1);
      if (vr1.operands != operands)
	vr1.operands.release ();
      if (wvnresult)
	{
	  if (vnresult)
	    *vnresult = wvnresult;
	  return wvnresult->result;
	}

      return NULL_TREE;
    }

  return vn_reference_lookup_1 (&vr1, vnresult);
}


/* Insert OP into the current hash table with a value number of
   RESULT, and return the resulting reference structure we created.  */

vn_reference_t
vn_reference_insert (tree op, tree result, tree vuse, tree vdef)
{
  vn_reference_s **slot;
  vn_reference_t vr1;
  bool tem;

  vr1 = (vn_reference_t) pool_alloc (current_info->references_pool);
  if (TREE_CODE (result) == SSA_NAME)
    vr1->value_id = VN_INFO (result)->value_id;
  else
    vr1->value_id = get_or_alloc_constant_value_id (result);
  vr1->vuse = vuse ? SSA_VAL (vuse) : NULL_TREE;
  vr1->operands = valueize_shared_reference_ops_from_ref (op, &tem).copy ();
  vr1->type = TREE_TYPE (op);
  vr1->set = get_alias_set (op);
  vr1->hashcode = vn_reference_compute_hash (vr1);
  vr1->result = TREE_CODE (result) == SSA_NAME ? SSA_VAL (result) : result;
  vr1->result_vdef = vdef;

  slot = current_info->references.find_slot_with_hash (vr1, vr1->hashcode,
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
vn_reference_insert_pieces (tree vuse, alias_set_type set, tree type,
			    vec<vn_reference_op_s> operands,
			    tree result, unsigned int value_id)

{
  vn_reference_s **slot;
  vn_reference_t vr1;

  vr1 = (vn_reference_t) pool_alloc (current_info->references_pool);
  vr1->value_id = value_id;
  vr1->vuse = vuse ? SSA_VAL (vuse) : NULL_TREE;
  vr1->operands = valueize_refs (operands);
  vr1->type = type;
  vr1->set = set;
  vr1->hashcode = vn_reference_compute_hash (vr1);
  if (result && TREE_CODE (result) == SSA_NAME)
    result = SSA_VAL (result);
  vr1->result = result;

  slot = current_info->references.find_slot_with_hash (vr1, vr1->hashcode,
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

hashval_t
vn_nary_op_compute_hash (const vn_nary_op_t vno1)
{
  hashval_t hash;
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

  hash = iterative_hash_hashval_t (vno1->opcode, 0);
  for (i = 0; i < vno1->length; ++i)
    hash = iterative_hash_expr (vno1->op[i], hash);

  return hash;
}

/* Compare nary operations VNO1 and VNO2 and return true if they are
   equivalent.  */

bool
vn_nary_op_eq (const_vn_nary_op_t const vno1, const_vn_nary_op_t const vno2)
{
  unsigned i;

  if (vno1->hashcode != vno2->hashcode)
    return false;

  if (vno1->length != vno2->length)
    return false;

  if (vno1->opcode != vno2->opcode
      || !types_compatible_p (vno1->type, vno2->type))
    return false;

  for (i = 0; i < vno1->length; ++i)
    if (!expressions_equal_p (vno1->op[i], vno2->op[i]))
      return false;

  return true;
}

/* Initialize VNO from the pieces provided.  */

static void
init_vn_nary_op_from_pieces (vn_nary_op_t vno, unsigned int length,
			     enum tree_code code, tree type, tree *ops)
{
  vno->opcode = code;
  vno->length = length;
  vno->type = type;
  memcpy (&vno->op[0], ops, sizeof (tree) * length);
}

/* Initialize VNO from OP.  */

static void
init_vn_nary_op_from_op (vn_nary_op_t vno, tree op)
{
  unsigned i;

  vno->opcode = TREE_CODE (op);
  vno->length = TREE_CODE_LENGTH (TREE_CODE (op));
  vno->type = TREE_TYPE (op);
  for (i = 0; i < vno->length; ++i)
    vno->op[i] = TREE_OPERAND (op, i);
}

/* Return the number of operands for a vn_nary ops structure from STMT.  */

static unsigned int
vn_nary_length_from_stmt (gimple stmt)
{
  switch (gimple_assign_rhs_code (stmt))
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      return 1;

    case BIT_FIELD_REF:
      return 3;

    case CONSTRUCTOR:
      return CONSTRUCTOR_NELTS (gimple_assign_rhs1 (stmt));

    default:
      return gimple_num_ops (stmt) - 1;
    }
}

/* Initialize VNO from STMT.  */

static void
init_vn_nary_op_from_stmt (vn_nary_op_t vno, gimple stmt)
{
  unsigned i;

  vno->opcode = gimple_assign_rhs_code (stmt);
  vno->type = gimple_expr_type (stmt);
  switch (vno->opcode)
    {
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case VIEW_CONVERT_EXPR:
      vno->length = 1;
      vno->op[0] = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
      break;

    case BIT_FIELD_REF:
      vno->length = 3;
      vno->op[0] = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
      vno->op[1] = TREE_OPERAND (gimple_assign_rhs1 (stmt), 1);
      vno->op[2] = TREE_OPERAND (gimple_assign_rhs1 (stmt), 2);
      break;

    case CONSTRUCTOR:
      vno->length = CONSTRUCTOR_NELTS (gimple_assign_rhs1 (stmt));
      for (i = 0; i < vno->length; ++i)
	vno->op[i] = CONSTRUCTOR_ELT (gimple_assign_rhs1 (stmt), i)->value;
      break;

    default:
      gcc_checking_assert (!gimple_assign_single_p (stmt));
      vno->length = gimple_num_ops (stmt) - 1;
      for (i = 0; i < vno->length; ++i)
	vno->op[i] = gimple_op (stmt, i + 1);
    }
}

/* Compute the hashcode for VNO and look for it in the hash table;
   return the resulting value number if it exists in the hash table.
   Return NULL_TREE if it does not exist in the hash table or if the
   result field of the operation is NULL.  VNRESULT will contain the
   vn_nary_op_t from the hashtable if it exists.  */

static tree
vn_nary_op_lookup_1 (vn_nary_op_t vno, vn_nary_op_t *vnresult)
{
  vn_nary_op_s **slot;

  if (vnresult)
    *vnresult = NULL;

  vno->hashcode = vn_nary_op_compute_hash (vno);
  slot = current_info->nary.find_slot_with_hash (vno, vno->hashcode, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->nary.find_slot_with_hash (vno, vno->hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  if (vnresult)
    *vnresult = *slot;
  return (*slot)->result;
}

/* Lookup a n-ary operation by its pieces and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the operation
   is NULL. VNRESULT will contain the vn_nary_op_t from the hashtable
   if it exists.  */

tree
vn_nary_op_lookup_pieces (unsigned int length, enum tree_code code,
			  tree type, tree *ops, vn_nary_op_t *vnresult)
{
  vn_nary_op_t vno1 = XALLOCAVAR (struct vn_nary_op_s,
				  sizeof_vn_nary_op (length));
  init_vn_nary_op_from_pieces (vno1, length, code, type, ops);
  return vn_nary_op_lookup_1 (vno1, vnresult);
}

/* Lookup OP in the current hash table, and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the operation
   is NULL. VNRESULT will contain the vn_nary_op_t from the hashtable
   if it exists.  */

tree
vn_nary_op_lookup (tree op, vn_nary_op_t *vnresult)
{
  vn_nary_op_t vno1
    = XALLOCAVAR (struct vn_nary_op_s,
		  sizeof_vn_nary_op (TREE_CODE_LENGTH (TREE_CODE (op))));
  init_vn_nary_op_from_op (vno1, op);
  return vn_nary_op_lookup_1 (vno1, vnresult);
}

/* Lookup the rhs of STMT in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table.  VNRESULT will contain the
   vn_nary_op_t from the hashtable if it exists.  */

tree
vn_nary_op_lookup_stmt (gimple stmt, vn_nary_op_t *vnresult)
{
  vn_nary_op_t vno1
    = XALLOCAVAR (struct vn_nary_op_s,
		  sizeof_vn_nary_op (vn_nary_length_from_stmt (stmt)));
  init_vn_nary_op_from_stmt (vno1, stmt);
  return vn_nary_op_lookup_1 (vno1, vnresult);
}

/* Allocate a vn_nary_op_t with LENGTH operands on STACK.  */

static vn_nary_op_t
alloc_vn_nary_op_noinit (unsigned int length, struct obstack *stack)
{
  return (vn_nary_op_t) obstack_alloc (stack, sizeof_vn_nary_op (length));
}

/* Allocate and initialize a vn_nary_op_t on CURRENT_INFO's
   obstack.  */

static vn_nary_op_t
alloc_vn_nary_op (unsigned int length, tree result, unsigned int value_id)
{
  vn_nary_op_t vno1 = alloc_vn_nary_op_noinit (length,
					       &current_info->nary_obstack);

  vno1->value_id = value_id;
  vno1->length = length;
  vno1->result = result;

  return vno1;
}

/* Insert VNO into TABLE.  If COMPUTE_HASH is true, then compute
   VNO->HASHCODE first.  */

static vn_nary_op_t
vn_nary_op_insert_into (vn_nary_op_t vno, vn_nary_op_table_type table,
			bool compute_hash)
{
  vn_nary_op_s **slot;

  if (compute_hash)
    vno->hashcode = vn_nary_op_compute_hash (vno);

  slot = table.find_slot_with_hash (vno, vno->hashcode, INSERT);
  gcc_assert (!*slot);

  *slot = vno;
  return vno;
}

/* Insert a n-ary operation into the current hash table using it's
   pieces.  Return the vn_nary_op_t structure we created and put in
   the hashtable.  */

vn_nary_op_t
vn_nary_op_insert_pieces (unsigned int length, enum tree_code code,
			  tree type, tree *ops,
			  tree result, unsigned int value_id)
{
  vn_nary_op_t vno1 = alloc_vn_nary_op (length, result, value_id);
  init_vn_nary_op_from_pieces (vno1, length, code, type, ops);
  return vn_nary_op_insert_into (vno1, current_info->nary, true);
}

/* Insert OP into the current hash table with a value number of
   RESULT.  Return the vn_nary_op_t structure we created and put in
   the hashtable.  */

vn_nary_op_t
vn_nary_op_insert (tree op, tree result)
{
  unsigned length = TREE_CODE_LENGTH (TREE_CODE (op));
  vn_nary_op_t vno1;

  vno1 = alloc_vn_nary_op (length, result, VN_INFO (result)->value_id);
  init_vn_nary_op_from_op (vno1, op);
  return vn_nary_op_insert_into (vno1, current_info->nary, true);
}

/* Insert the rhs of STMT into the current hash table with a value number of
   RESULT.  */

vn_nary_op_t
vn_nary_op_insert_stmt (gimple stmt, tree result)
{
  vn_nary_op_t vno1
    = alloc_vn_nary_op (vn_nary_length_from_stmt (stmt),
			result, VN_INFO (result)->value_id);
  init_vn_nary_op_from_stmt (vno1, stmt);
  return vn_nary_op_insert_into (vno1, current_info->nary, true);
}

/* Compute a hashcode for PHI operation VP1 and return it.  */

static inline hashval_t
vn_phi_compute_hash (vn_phi_t vp1)
{
  hashval_t result;
  int i;
  tree phi1op;
  tree type;

  result = vp1->block->index;

  /* If all PHI arguments are constants we need to distinguish
     the PHI node via its type.  */
  type = vp1->type;
  result += vn_hash_type (type);

  FOR_EACH_VEC_ELT (vp1->phiargs, i, phi1op)
    {
      if (phi1op == VN_TOP)
	continue;
      result = iterative_hash_expr (phi1op, result);
    }

  return result;
}

/* Compare two phi entries for equality, ignoring VN_TOP arguments.  */

static int
vn_phi_eq (const_vn_phi_t const vp1, const_vn_phi_t const vp2)
{
  if (vp1->hashcode != vp2->hashcode)
    return false;

  if (vp1->block == vp2->block)
    {
      int i;
      tree phi1op;

      /* If the PHI nodes do not have compatible types
	 they are not the same.  */
      if (!types_compatible_p (vp1->type, vp2->type))
	return false;

      /* Any phi in the same block will have it's arguments in the
	 same edge order, because of how we store phi nodes.  */
      FOR_EACH_VEC_ELT (vp1->phiargs, i, phi1op)
	{
	  tree phi2op = vp2->phiargs[i];
	  if (phi1op == VN_TOP || phi2op == VN_TOP)
	    continue;
	  if (!expressions_equal_p (phi1op, phi2op))
	    return false;
	}
      return true;
    }
  return false;
}

static vec<tree> shared_lookup_phiargs;

/* Lookup PHI in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

static tree
vn_phi_lookup (gimple phi)
{
  vn_phi_s **slot;
  struct vn_phi_s vp1;
  unsigned i;

  shared_lookup_phiargs.truncate (0);

  /* Canonicalize the SSA_NAME's to their value number.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      shared_lookup_phiargs.safe_push (def);
    }
  vp1.type = TREE_TYPE (gimple_phi_result (phi));
  vp1.phiargs = shared_lookup_phiargs;
  vp1.block = gimple_bb (phi);
  vp1.hashcode = vn_phi_compute_hash (&vp1);
  slot = current_info->phis.find_slot_with_hash (&vp1, vp1.hashcode, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->phis.find_slot_with_hash (&vp1, vp1.hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  return (*slot)->result;
}

/* Insert PHI into the current hash table with a value number of
   RESULT.  */

static vn_phi_t
vn_phi_insert (gimple phi, tree result)
{
  vn_phi_s **slot;
  vn_phi_t vp1 = (vn_phi_t) pool_alloc (current_info->phis_pool);
  unsigned i;
  vec<tree> args = vNULL;

  /* Canonicalize the SSA_NAME's to their value number.  */
  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree def = PHI_ARG_DEF (phi, i);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      args.safe_push (def);
    }
  vp1->value_id = VN_INFO (result)->value_id;
  vp1->type = TREE_TYPE (gimple_phi_result (phi));
  vp1->phiargs = args;
  vp1->block = gimple_bb (phi);
  vp1->result = result;
  vp1->hashcode = vn_phi_compute_hash (vp1);

  slot = current_info->phis.find_slot_with_hash (vp1, vp1->hashcode, INSERT);

  /* Because we iterate over phi operations more than once, it's
     possible the slot might already exist here, hence no assert.*/
  *slot = vp1;
  return vp1;
}


/* Print set of components in strongly connected component SCC to OUT. */

static void
print_scc (FILE *out, vec<tree> scc)
{
  tree var;
  unsigned int i;

  fprintf (out, "SCC consists of:");
  FOR_EACH_VEC_ELT (scc, i, var)
    {
      fprintf (out, " ");
      print_generic_expr (out, var, 0);
    }
  fprintf (out, "\n");
}

/* Set the value number of FROM to TO, return true if it has changed
   as a result.  */

static inline bool
set_ssa_val_to (tree from, tree to)
{
  tree currval = SSA_VAL (from);
  HOST_WIDE_INT toff, coff;

  /* The only thing we allow as value numbers are ssa_names
     and invariants.  So assert that here.  We don't allow VN_TOP
     as visiting a stmt should produce a value-number other than
     that.
     ???  Still VN_TOP can happen for unreachable code, so force
     it to varying in that case.  Not all code is prepared to
     get VN_TOP on valueization.  */
  if (to == VN_TOP)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Forcing value number to varying on "
		 "receiving VN_TOP\n");
      to = from;
    }

  gcc_assert (to != NULL_TREE
	      && (TREE_CODE (to) == SSA_NAME
		  || is_gimple_min_invariant (to)));

  if (from != to)
    {
      if (currval == from)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Not changing value number of ");
	      print_generic_expr (dump_file, from, 0);
	      fprintf (dump_file, " from VARYING to ");
	      print_generic_expr (dump_file, to, 0);
	      fprintf (dump_file, "\n");
	    }
	  return false;
	}
      else if (TREE_CODE (to) == SSA_NAME
	       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (to))
	to = from;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Setting value number of ");
      print_generic_expr (dump_file, from, 0);
      fprintf (dump_file, " to ");
      print_generic_expr (dump_file, to, 0);
    }

  if (currval != to
      && !operand_equal_p (currval, to, 0)
      /* ???  For addresses involving volatile objects or types operand_equal_p
         does not reliably detect ADDR_EXPRs as equal.  We know we are only
	 getting invariant gimple addresses here, so can use
	 get_addr_base_and_unit_offset to do this comparison.  */
      && !(TREE_CODE (currval) == ADDR_EXPR
	   && TREE_CODE (to) == ADDR_EXPR
	   && (get_addr_base_and_unit_offset (TREE_OPERAND (currval, 0), &coff)
	       == get_addr_base_and_unit_offset (TREE_OPERAND (to, 0), &toff))
	   && coff == toff))
    {
      VN_INFO (from)->valnum = to;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " (changed)\n");
      return true;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n");
  return false;
}

/* Mark as processed all the definitions in the defining stmt of USE, or
   the USE itself.  */

static void
mark_use_processed (tree use)
{
  ssa_op_iter iter;
  def_operand_p defp;
  gimple stmt = SSA_NAME_DEF_STMT (use);

  if (SSA_NAME_IS_DEFAULT_DEF (use) || gimple_code (stmt) == GIMPLE_PHI)
    {
      VN_INFO (use)->use_processed = true;
      return;
    }

  FOR_EACH_SSA_DEF_OPERAND (defp, stmt, iter, SSA_OP_ALL_DEFS)
    {
      tree def = DEF_FROM_PTR (defp);

      VN_INFO (def)->use_processed = true;
    }
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
      changed |= set_ssa_val_to (def, def);
    }
  return changed;
}

static bool expr_has_constants (tree expr);

/* Visit a copy between LHS and RHS, return true if the value number
   changed.  */

static bool
visit_copy (tree lhs, tree rhs)
{
  /* The copy may have a more interesting constant filled expression
     (we don't, since we know our RHS is just an SSA name).  */
  VN_INFO (lhs)->has_constants = VN_INFO (rhs)->has_constants;
  VN_INFO (lhs)->expr = VN_INFO (rhs)->expr;

  /* And finally valueize.  */
  rhs = SSA_VAL (rhs);

  return set_ssa_val_to (lhs, rhs);
}

/* Visit a nary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_nary_op (tree lhs, gimple stmt)
{
  bool changed = false;
  tree result = vn_nary_op_lookup_stmt (stmt, NULL);

  if (result)
    changed = set_ssa_val_to (lhs, result);
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
  vn_reference_t vnresult = NULL;
  tree vuse = gimple_vuse (stmt);
  tree vdef = gimple_vdef (stmt);

  /* Non-ssa lhs is handled in copy_reference_ops_from_call.  */
  if (lhs && TREE_CODE (lhs) != SSA_NAME)
    lhs = NULL_TREE;

  vr1.vuse = vuse ? SSA_VAL (vuse) : NULL_TREE;
  vr1.operands = valueize_shared_reference_ops_from_call (stmt);
  vr1.type = gimple_expr_type (stmt);
  vr1.set = 0;
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  vn_reference_lookup_1 (&vr1, &vnresult);

  if (vnresult)
    {
      if (vnresult->result_vdef && vdef)
	changed |= set_ssa_val_to (vdef, vnresult->result_vdef);

      if (!vnresult->result && lhs)
	vnresult->result = lhs;

      if (vnresult->result && lhs)
	{
	  changed |= set_ssa_val_to (lhs, vnresult->result);

	  if (VN_INFO (vnresult->result)->has_constants)
	    VN_INFO (lhs)->has_constants = true;
	}
    }
  else
    {
      vn_reference_s **slot;
      vn_reference_t vr2;
      if (vdef)
	changed |= set_ssa_val_to (vdef, vdef);
      if (lhs)
	changed |= set_ssa_val_to (lhs, lhs);
      vr2 = (vn_reference_t) pool_alloc (current_info->references_pool);
      vr2->vuse = vr1.vuse;
      vr2->operands = valueize_refs (create_reference_ops_from_call (stmt));
      vr2->type = vr1.type;
      vr2->set = vr1.set;
      vr2->hashcode = vr1.hashcode;
      vr2->result = lhs;
      vr2->result_vdef = vdef;
      slot = current_info->references.find_slot_with_hash (vr2, vr2->hashcode,
							   INSERT);
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
  tree last_vuse;
  tree result;

  last_vuse = gimple_vuse (stmt);
  last_vuse_ptr = &last_vuse;
  result = vn_reference_lookup (op, gimple_vuse (stmt),
				default_vn_walk_kind, NULL);
  last_vuse_ptr = NULL;

  /* If we have a VCE, try looking up its operand as it might be stored in
     a different type.  */
  if (!result && TREE_CODE (op) == VIEW_CONVERT_EXPR)
    result = vn_reference_lookup (TREE_OPERAND (op, 0), gimple_vuse (stmt),
    				  default_vn_walk_kind, NULL);

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
	  tree tem = vn_get_expr_for (TREE_OPERAND (val, 0));
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
      if (!result)
        {
	  result = make_temp_ssa_name (TREE_TYPE (lhs), gimple_build_nop (),
				       "vntemp");
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
      vn_reference_insert (op, lhs, last_vuse, NULL_TREE);
    }

  return changed;
}


/* Visit a store to a reference operator LHS, part of STMT, value number it,
   and return true if the value number of the LHS has changed as a result.  */

static bool
visit_reference_op_store (tree lhs, tree op, gimple stmt)
{
  bool changed = false;
  vn_reference_t vnresult = NULL;
  tree result, assign;
  bool resultsame = false;
  tree vuse = gimple_vuse (stmt);
  tree vdef = gimple_vdef (stmt);

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

  result = vn_reference_lookup (lhs, vuse, VN_NOWALK, NULL);

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
      assign = build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, op);
      vn_reference_lookup (assign, vuse, VN_NOWALK, &vnresult);
      if (vnresult)
	{
	  VN_INFO (vdef)->use_processed = true;
	  return set_ssa_val_to (vdef, vnresult->result_vdef);
	}
    }

  if (!result || !resultsame)
    {
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
      if (vdef)
	{
	  changed |= set_ssa_val_to (vdef, vdef);
	}

      /* Do not insert structure copies into the tables.  */
      if (is_gimple_min_invariant (op)
	  || is_gimple_reg (op))
        vn_reference_insert (lhs, op, vdef, NULL);

      assign = build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, op);
      vn_reference_insert (assign, lhs, vuse, vdef);
    }
  else
    {
      /* We had a match, so value number the vdef to have the value
	 number of the vuse it came from.  */

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Store matched earlier value,"
		 "value numbering store vdefs to matching vuses.\n");

      changed |= set_ssa_val_to (vdef, SSA_VAL (vuse));
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

  /* TODO: We could check for this in init_sccvn, and replace this
     with a gcc_assert.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
    return set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));

  /* See if all non-TOP arguments have the same value.  TOP is
     equivalent to everything, so we can ignore it.  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    if (e->flags & EDGE_EXECUTABLE)
      {
	tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);

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
	  if (sameval != VN_TOP)
	    VN_INFO (PHI_RESULT (phi))->expr = sameval;
	}
      else
	{
	  VN_INFO (PHI_RESULT (phi))->has_constants = false;
	  if (sameval != VN_TOP)
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
  tree tem;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  switch (get_gimple_rhs_class (gimple_assign_rhs_code (stmt)))
    {
    case GIMPLE_TERNARY_RHS:
      tem = gimple_assign_rhs3 (stmt);
      if (TREE_CODE (tem) == SSA_NAME)
	tem = SSA_VAL (tem);
      if (is_gimple_min_invariant (tem))
	return true;
      /* Fallthru.  */

    case GIMPLE_BINARY_RHS:
      tem = gimple_assign_rhs2 (stmt);
      if (TREE_CODE (tem) == SSA_NAME)
	tem = SSA_VAL (tem);
      if (is_gimple_min_invariant (tem))
	return true;
      /* Fallthru.  */

    case GIMPLE_SINGLE_RHS:
      /* Constants inside reference ops are rarely interesting, but
	 it can take a lot of looking to find them.  */
    case GIMPLE_UNARY_RHS:
      tem = gimple_assign_rhs1 (stmt);
      if (TREE_CODE (tem) == SSA_NAME)
	tem = SSA_VAL (tem);
      return is_gimple_min_invariant (tem);

    default:
      gcc_unreachable ();
    }
  return false;
}

/* Simplify the binary expression RHS, and return the result if
   simplified. */

static tree
simplify_binary_expression (gimple stmt)
{
  tree result = NULL_TREE;
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  enum tree_code code = gimple_assign_rhs_code (stmt);

  /* This will not catch every single case we could combine, but will
     catch those with constants.  The goal here is to simultaneously
     combine constants between expressions, but avoid infinite
     expansion of expressions during simplification.  */
  if (TREE_CODE (op0) == SSA_NAME)
    {
      if (VN_INFO (op0)->has_constants
	  || TREE_CODE_CLASS (code) == tcc_comparison
	  || code == COMPLEX_EXPR)
	op0 = vn_get_expr_for (op0);
      else
	op0 = vn_valueize (op0);
    }

  if (TREE_CODE (op1) == SSA_NAME)
    {
      if (VN_INFO (op1)->has_constants
	  || code == COMPLEX_EXPR)
	op1 = vn_get_expr_for (op1);
      else
	op1 = vn_valueize (op1);
    }

  /* Pointer plus constant can be represented as invariant address.
     Do so to allow further propatation, see also tree forwprop.  */
  if (code == POINTER_PLUS_EXPR
      && tree_fits_uhwi_p (op1)
      && TREE_CODE (op0) == ADDR_EXPR
      && is_gimple_min_invariant (op0))
    return build_invariant_address (TREE_TYPE (op0),
				    TREE_OPERAND (op0, 0),
				    tree_to_uhwi (op1));

  /* Avoid folding if nothing changed.  */
  if (op0 == gimple_assign_rhs1 (stmt)
      && op1 == gimple_assign_rhs2 (stmt))
    return NULL_TREE;

  fold_defer_overflow_warnings ();

  result = fold_binary (code, gimple_expr_type (stmt), op0, op1);
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
  enum tree_code code = gimple_assign_rhs_code (stmt);

  /* We handle some tcc_reference codes here that are all
     GIMPLE_ASSIGN_SINGLE codes.  */
  if (code == REALPART_EXPR
      || code == IMAGPART_EXPR
      || code == VIEW_CONVERT_EXPR
      || code == BIT_FIELD_REF)
    op0 = TREE_OPERAND (op0, 0);

  if (TREE_CODE (op0) != SSA_NAME)
    return NULL_TREE;

  orig_op0 = op0;
  if (VN_INFO (op0)->has_constants)
    op0 = vn_get_expr_for (op0);
  else if (CONVERT_EXPR_CODE_P (code)
	   || code == REALPART_EXPR
	   || code == IMAGPART_EXPR
	   || code == VIEW_CONVERT_EXPR
	   || code == BIT_FIELD_REF)
    {
      /* We want to do tree-combining on conversion-like expressions.
         Make sure we feed only SSA_NAMEs or constants to fold though.  */
      tree tem = vn_get_expr_for (op0);
      if (UNARY_CLASS_P (tem)
	  || BINARY_CLASS_P (tem)
	  || TREE_CODE (tem) == VIEW_CONVERT_EXPR
	  || TREE_CODE (tem) == SSA_NAME
	  || TREE_CODE (tem) == CONSTRUCTOR
	  || is_gimple_min_invariant (tem))
	op0 = tem;
    }

  /* Avoid folding if nothing changed, but remember the expression.  */
  if (op0 == orig_op0)
    return NULL_TREE;

  if (code == BIT_FIELD_REF)
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      result = fold_ternary (BIT_FIELD_REF, TREE_TYPE (rhs),
			     op0, TREE_OPERAND (rhs, 1), TREE_OPERAND (rhs, 2));
    }
  else
    result = fold_unary_ignore_overflow (code, gimple_expr_type (stmt), op0);
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
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree tem;

  /* For stores we can end up simplifying a SSA_NAME rhs.  Just return
     in this case, there is no point in doing extra work.  */
  if (code == SSA_NAME)
    return NULL_TREE;

  /* First try constant folding based on our current lattice.  */
  tem = gimple_fold_stmt_to_constant_1 (stmt, vn_valueize);
  if (tem
      && (TREE_CODE (tem) == SSA_NAME
	  || is_gimple_min_invariant (tem)))
    return tem;

  /* If that didn't work try combining multiple statements.  */
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_reference:
      /* Fallthrough for some unary codes that can operate on registers.  */
      if (!(code == REALPART_EXPR
	    || code == IMAGPART_EXPR
	    || code == VIEW_CONVERT_EXPR
	    || code == BIT_FIELD_REF))
	break;
      /* We could do a little more with unary ops, if they expand
	 into binary ops, but it's debatable whether it is worth it. */
    case tcc_unary:
      return simplify_unary_expression (stmt);

    case tcc_comparison:
    case tcc_binary:
      return simplify_binary_expression (stmt);

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

  mark_use_processed (use);

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
      else if (gimple_has_volatile_ops (stmt))
	changed = defs_to_varying (stmt);
      else if (is_gimple_assign (stmt))
	{
	  enum tree_code code = gimple_assign_rhs_code (stmt);
	  tree lhs = gimple_assign_lhs (stmt);
	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  tree simplified;

	  /* Shortcut for copies. Simplifying copies is pointless,
	     since we copy the expression and value they represent.  */
	  if (code == SSA_NAME
	      && TREE_CODE (lhs) == SSA_NAME)
	    {
	      changed = visit_copy (lhs, rhs1);
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
		    && is_gimple_min_invariant (rhs1))
	       && !(simplified
		    && is_gimple_min_invariant (simplified))
	       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	      /* Stores or copies from SSA_NAMEs that are live over
		 abnormal edges are a problem.  */
	      || (code == SSA_NAME
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1)))
	    changed = defs_to_varying (stmt);
	  else if (REFERENCE_CLASS_P (lhs)
		   || DECL_P (lhs))
	    changed = visit_reference_op_store (lhs, rhs1, stmt);
	  else if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      if ((gimple_assign_copy_p (stmt)
		   && is_gimple_min_invariant (rhs1))
		  || (simplified
		      && is_gimple_min_invariant (simplified)))
		{
		  VN_INFO (lhs)->has_constants = true;
		  if (simplified)
		    changed = set_ssa_val_to (lhs, simplified);
		  else
		    changed = set_ssa_val_to (lhs, rhs1);
		}
	      else
		{
		  /* First try to lookup the simplified expression.  */
		  if (simplified)
		    {
		      enum gimple_rhs_class rhs_class;


		      rhs_class = get_gimple_rhs_class (TREE_CODE (simplified));
		      if ((rhs_class == GIMPLE_UNARY_RHS
			   || rhs_class == GIMPLE_BINARY_RHS
			   || rhs_class == GIMPLE_TERNARY_RHS)
			  && valid_gimple_rhs_p (simplified))
			{
			  tree result = vn_nary_op_lookup (simplified, NULL);
			  if (result)
			    {
			      changed = set_ssa_val_to (lhs, result);
			      goto done;
			    }
			}
		    }

		  /* Otherwise visit the original statement.  */
		  switch (vn_get_stmt_kind (stmt))
		    {
		    case VN_NARY:
		      changed = visit_nary_op (lhs, stmt);
		      break;
		    case VN_REFERENCE:
		      changed = visit_reference_op_load (lhs, rhs1, stmt);
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
	  if (lhs && TREE_CODE (lhs) == SSA_NAME)
	    {
	      /* Try constant folding based on our current lattice.  */
	      tree simplified = gimple_fold_stmt_to_constant_1 (stmt,
								vn_valueize);
	      if (simplified)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "call ");
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
		  && is_gimple_min_invariant (simplified))
		{
		  VN_INFO (lhs)->expr = simplified;
		  VN_INFO (lhs)->has_constants = true;
		  changed = set_ssa_val_to (lhs, simplified);
		  if (gimple_vdef (stmt))
		    changed |= set_ssa_val_to (gimple_vdef (stmt),
					       gimple_vuse (stmt));
		  goto done;
		}
	      else if (simplified
		       && TREE_CODE (simplified) == SSA_NAME)
		{
		  changed = visit_copy (lhs, simplified);
		  if (gimple_vdef (stmt))
		    changed |= set_ssa_val_to (gimple_vdef (stmt),
					       gimple_vuse (stmt));
		  goto done;
		}
	      else
		{
		  if (stmt_has_constants (stmt))
		    VN_INFO (lhs)->has_constants = true;
		  else
		    {
		      /* We reset expr and constantness here because we may
			 have been value numbering optimistically, and
			 iterating.  They may become non-constant in this case,
			 even if they were optimistically constant.  */
		      VN_INFO (lhs)->has_constants = false;
		      VN_INFO (lhs)->expr = NULL_TREE;
		    }

		  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
		    {
		      changed = defs_to_varying (stmt);
		      goto done;
		    }
		}
	    }

	  if (!gimple_call_internal_p (stmt)
	      && (/* Calls to the same function with the same vuse
		     and the same operands do not necessarily return the same
		     value, unless they're pure or const.  */
		  gimple_call_flags (stmt) & (ECF_PURE | ECF_CONST)
		  /* If calls have a vdef, subsequent calls won't have
		     the same incoming vuse.  So, if 2 calls with vdef have the
		     same vuse, we know they're not subsequent.
		     We can value number 2 calls to the same function with the
		     same vuse and the same operands which are not subsequent
		     the same, because there is no code in the program that can
		     compare the 2 values...  */
		  || (gimple_vdef (stmt)
		      /* ... unless the call returns a pointer which does
		         not alias with anything else.  In which case the
			 information that the values are distinct are encoded
			 in the IL.  */
		      && !(gimple_call_return_flags (stmt) & ERF_NOALIAS))))
	    changed = visit_reference_op_call (lhs, stmt);
	  else
	    changed = defs_to_varying (stmt);
	}
      else
	changed = defs_to_varying (stmt);
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
    return SSA_NAME_VERSION (opa) - SSA_NAME_VERSION (opb);
  else if (gimple_nop_p (opstmta))
    return -1;
  else if (gimple_nop_p (opstmtb))
    return 1;

  bba = gimple_bb (opstmta);
  bbb = gimple_bb (opstmtb);

  if (!bba && !bbb)
    return SSA_NAME_VERSION (opa) - SSA_NAME_VERSION (opb);
  else if (!bba)
    return -1;
  else if (!bbb)
    return 1;

  if (bba == bbb)
    {
      if (gimple_code (opstmta) == GIMPLE_PHI
	  && gimple_code (opstmtb) == GIMPLE_PHI)
	return SSA_NAME_VERSION (opa) - SSA_NAME_VERSION (opb);
      else if (gimple_code (opstmta) == GIMPLE_PHI)
	return -1;
      else if (gimple_code (opstmtb) == GIMPLE_PHI)
	return 1;
      else if (gimple_uid (opstmta) != gimple_uid (opstmtb))
        return gimple_uid (opstmta) - gimple_uid (opstmtb);
      else
	return SSA_NAME_VERSION (opa) - SSA_NAME_VERSION (opb);
    }
  return rpo_numbers[bba->index] - rpo_numbers[bbb->index];
}

/* Sort an array containing members of a strongly connected component
   SCC so that the members are ordered by RPO number.
   This means that when the sort is complete, iterating through the
   array will give you the members in RPO order.  */

static void
sort_scc (vec<tree> scc)
{
  scc.qsort (compare_ops);
}

/* Insert the no longer used nary ONARY to the hash INFO.  */

static void
copy_nary (vn_nary_op_t onary, vn_tables_t info)
{
  size_t size = sizeof_vn_nary_op (onary->length);
  vn_nary_op_t nary = alloc_vn_nary_op_noinit (onary->length,
					       &info->nary_obstack);
  memcpy (nary, onary, size);
  vn_nary_op_insert_into (nary, info->nary, false);
}

/* Insert the no longer used phi OPHI to the hash INFO.  */

static void
copy_phi (vn_phi_t ophi, vn_tables_t info)
{
  vn_phi_t phi = (vn_phi_t) pool_alloc (info->phis_pool);
  vn_phi_s **slot;
  memcpy (phi, ophi, sizeof (*phi));
  ophi->phiargs.create (0);
  slot = info->phis.find_slot_with_hash (phi, phi->hashcode, INSERT);
  gcc_assert (!*slot);
  *slot = phi;
}

/* Insert the no longer used reference OREF to the hash INFO.  */

static void
copy_reference (vn_reference_t oref, vn_tables_t info)
{
  vn_reference_t ref;
  vn_reference_s **slot;
  ref = (vn_reference_t) pool_alloc (info->references_pool);
  memcpy (ref, oref, sizeof (*ref));
  oref->operands.create (0);
  slot = info->references.find_slot_with_hash (ref, ref->hashcode, INSERT);
  if (*slot)
    free_reference (*slot);
  *slot = ref;
}

/* Process a strongly connected component in the SSA graph.  */

static void
process_scc (vec<tree> scc)
{
  tree var;
  unsigned int i;
  unsigned int iterations = 0;
  bool changed = true;
  vn_nary_op_iterator_type hin;
  vn_phi_iterator_type hip;
  vn_reference_iterator_type hir;
  vn_nary_op_t nary;
  vn_phi_t phi;
  vn_reference_t ref;

  /* If the SCC has a single member, just visit it.  */
  if (scc.length () == 1)
    {
      tree use = scc[0];
      if (VN_INFO (use)->use_processed)
	return;
      /* We need to make sure it doesn't form a cycle itself, which can
	 happen for self-referential PHI nodes.  In that case we would
	 end up inserting an expression with VN_TOP operands into the
	 valid table which makes us derive bogus equivalences later.
	 The cheapest way to check this is to assume it for all PHI nodes.  */
      if (gimple_code (SSA_NAME_DEF_STMT (use)) == GIMPLE_PHI)
	/* Fallthru to iteration.  */ ;
      else
	{
	  visit_use (use);
	  return;
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    print_scc (dump_file, scc);

  /* Iterate over the SCC with the optimistic table until it stops
     changing.  */
  current_info = optimistic_info;
  while (changed)
    {
      changed = false;
      iterations++;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Starting iteration %d\n", iterations);
      /* As we are value-numbering optimistically we have to
	 clear the expression tables and the simplified expressions
	 in each iteration until we converge.  */
      optimistic_info->nary.empty ();
      optimistic_info->phis.empty ();
      optimistic_info->references.empty ();
      obstack_free (&optimistic_info->nary_obstack, NULL);
      gcc_obstack_init (&optimistic_info->nary_obstack);
      empty_alloc_pool (optimistic_info->phis_pool);
      empty_alloc_pool (optimistic_info->references_pool);
      FOR_EACH_VEC_ELT (scc, i, var)
	VN_INFO (var)->expr = NULL_TREE;
      FOR_EACH_VEC_ELT (scc, i, var)
	changed |= visit_use (var);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing SCC needed %d iterations\n", iterations);
  statistics_histogram_event (cfun, "SCC iterations", iterations);

  /* Finally, copy the contents of the no longer used optimistic
     table to the valid table.  */
  FOR_EACH_HASH_TABLE_ELEMENT (optimistic_info->nary, nary, vn_nary_op_t, hin)
    copy_nary (nary, valid_info);
  FOR_EACH_HASH_TABLE_ELEMENT (optimistic_info->phis, phi, vn_phi_t, hip)
    copy_phi (phi, valid_info);
  FOR_EACH_HASH_TABLE_ELEMENT (optimistic_info->references,
			       ref, vn_reference_t, hir)
    copy_reference (ref, valid_info);

  current_info = valid_info;
}


/* Pop the components of the found SCC for NAME off the SCC stack
   and process them.  Returns true if all went well, false if
   we run into resource limits.  */

static bool
extract_and_process_scc_for_name (tree name)
{
  auto_vec<tree> scc;
  tree x;

  /* Found an SCC, pop the components off the SCC stack and
     process them.  */
  do
    {
      x = sccstack.pop ();

      VN_INFO (x)->on_sccstack = false;
      scc.safe_push (x);
    } while (x != name);

  /* Bail out of SCCVN in case a SCC turns out to be incredibly large.  */
  if (scc.length ()
      > (unsigned)PARAM_VALUE (PARAM_SCCVN_MAX_SCC_SIZE))
    {
      if (dump_file)
	fprintf (dump_file, "WARNING: Giving up with SCCVN due to "
		 "SCC size %u exceeding %u\n", scc.length (),
		 (unsigned)PARAM_VALUE (PARAM_SCCVN_MAX_SCC_SIZE));

      return false;
    }

  if (scc.length () > 1)
    sort_scc (scc);

  process_scc (scc);

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
  vec<ssa_op_iter> itervec = vNULL;
  vec<tree> namevec = vNULL;
  use_operand_p usep = NULL;
  gimple defstmt;
  tree use;
  ssa_op_iter iter;

start_over:
  /* SCC info */
  VN_INFO (name)->dfsnum = next_dfs_num++;
  VN_INFO (name)->visited = true;
  VN_INFO (name)->low = VN_INFO (name)->dfsnum;

  sccstack.safe_push (name);
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
		namevec.release ();
		itervec.release ();
		return false;
	      }

	  /* Check if we are done.  */
	  if (namevec.is_empty ())
	    {
	      namevec.release ();
	      itervec.release ();
	      return true;
	    }

	  /* Restore the last use walker and continue walking there.  */
	  use = name;
	  name = namevec.pop ();
	  memcpy (&iter, &itervec.last (),
		  sizeof (ssa_op_iter));
	  itervec.pop ();
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
	      itervec.safe_push (iter);
	      namevec.safe_push (name);
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
  table->phis.create (23);
  table->nary.create (23);
  table->references.create (23);

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
  table->phis.dispose ();
  table->nary.dispose ();
  table->references.dispose ();
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
  sccstack.create (0);
  constant_to_value_id.create (23);

  constant_value_ids = BITMAP_ALLOC (NULL);

  next_dfs_num = 1;
  next_value_id = 1;

  vn_ssa_aux_table.create (num_ssa_names + 1);
  /* VEC_alloc doesn't actually grow it to the right size, it just
     preallocates the space to do so.  */
  vn_ssa_aux_table.safe_grow_cleared (num_ssa_names + 1);
  gcc_obstack_init (&vn_ssa_aux_obstack);

  shared_lookup_phiargs.create (0);
  shared_lookup_references.create (0);
  rpo_numbers = XNEWVEC (int, last_basic_block_for_fn (cfun));
  rpo_numbers_temp =
    XNEWVEC (int, n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS);
  pre_and_rev_post_order_compute (NULL, rpo_numbers_temp, false);

  /* RPO numbers is an array of rpo ordering, rpo[i] = bb means that
     the i'th block in RPO order is bb.  We want to map bb's to RPO
     numbers, so we need to rearrange this array.  */
  for (j = 0; j < n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS; j++)
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

  constant_to_value_id.dispose ();
  BITMAP_FREE (constant_value_ids);
  shared_lookup_phiargs.release ();
  shared_lookup_references.release ();
  XDELETEVEC (rpo_numbers);

  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name
	  && VN_INFO (name)->needs_insertion)
	release_ssa_name (name);
    }
  obstack_free (&vn_ssa_aux_obstack, NULL);
  vn_ssa_aux_table.release ();

  sccstack.release ();
  free_vn_table (valid_info);
  XDELETE (valid_info);
  free_vn_table (optimistic_info);
  XDELETE (optimistic_info);
}

/* Set *ID according to RESULT.  */

static void
set_value_id_for_result (tree result, unsigned int *id)
{
  if (result && TREE_CODE (result) == SSA_NAME)
    *id = VN_INFO (result)->value_id;
  else if (result && is_gimple_min_invariant (result))
    *id = get_or_alloc_constant_value_id (result);
  else
    *id = get_next_value_id ();
}

/* Set the value ids in the valid hash tables.  */

static void
set_hashtable_value_ids (void)
{
  vn_nary_op_iterator_type hin;
  vn_phi_iterator_type hip;
  vn_reference_iterator_type hir;
  vn_nary_op_t vno;
  vn_reference_t vr;
  vn_phi_t vp;

  /* Now set the value ids of the things we had put in the hash
     table.  */

  FOR_EACH_HASH_TABLE_ELEMENT (valid_info->nary, vno, vn_nary_op_t, hin)
    set_value_id_for_result (vno->result, &vno->value_id);

  FOR_EACH_HASH_TABLE_ELEMENT (valid_info->phis, vp, vn_phi_t, hip)
    set_value_id_for_result (vp->result, &vp->value_id);

  FOR_EACH_HASH_TABLE_ELEMENT (valid_info->references, vr, vn_reference_t, hir)
    set_value_id_for_result (vr->result, &vr->value_id);
}

class cond_dom_walker : public dom_walker
{
public:
  cond_dom_walker () : dom_walker (CDI_DOMINATORS), fail (false) {}

  virtual void before_dom_children (basic_block);

  bool fail;
};

void
cond_dom_walker::before_dom_children (basic_block bb)
{
  edge e;
  edge_iterator ei;

  if (fail)
    return;

  /* If any of the predecessor edges that do not come from blocks dominated
     by us are still marked as possibly executable consider this block
     reachable.  */
  bool reachable = bb == ENTRY_BLOCK_PTR_FOR_FN (cfun);
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!dominated_by_p (CDI_DOMINATORS, e->src, bb))
      reachable |= (e->flags & EDGE_EXECUTABLE);

  /* If the block is not reachable all outgoing edges are not
     executable.  */
  if (!reachable)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Marking all outgoing edges of unreachable "
		 "BB %d as not executable\n", bb->index);

      FOR_EACH_EDGE (e, ei, bb->succs)
	e->flags &= ~EDGE_EXECUTABLE;
      return;
    }

  gimple stmt = last_stmt (bb);
  if (!stmt)
    return;

  enum gimple_code code = gimple_code (stmt);
  if (code != GIMPLE_COND
      && code != GIMPLE_SWITCH
      && code != GIMPLE_GOTO)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Value-numbering operands of stmt ending BB %d: ",
	       bb->index);
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  /* Value-number the last stmts SSA uses.  */
  ssa_op_iter i;
  tree op;
  FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_USE)
    if (VN_INFO (op)->visited == false
	&& !DFS (op))
      {
	fail = true;
	return;
      }

  /* ???  We can even handle stmts with outgoing EH or ABNORMAL edges
     if value-numbering can prove they are not reachable.  Handling
     computed gotos is also possible.  */
  tree val;
  switch (code)
    {
    case GIMPLE_COND:
      {
	tree lhs = gimple_cond_lhs (stmt);
	tree rhs = gimple_cond_rhs (stmt);
	/* Work hard in computing the condition and take into account
	   the valueization of the defining stmt.  */
	if (TREE_CODE (lhs) == SSA_NAME)
	  lhs = vn_get_expr_for (lhs);
	if (TREE_CODE (rhs) == SSA_NAME)
	  rhs = vn_get_expr_for (rhs);
	val = fold_binary (gimple_cond_code (stmt),
			   boolean_type_node, lhs, rhs);
	break;
      }
    case GIMPLE_SWITCH:
      val = gimple_switch_index (stmt);
      break;
    case GIMPLE_GOTO:
      val = gimple_goto_dest (stmt);
      break;
    default:
      gcc_unreachable ();
    }
  if (!val)
    return;

  edge taken = find_taken_edge (bb, vn_valueize (val));
  if (!taken)
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Marking all edges out of BB %d but (%d -> %d) as "
	     "not executable\n", bb->index, bb->index, taken->dest->index);

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e != taken)
      e->flags &= ~EDGE_EXECUTABLE;
}

/* Do SCCVN.  Returns true if it finished, false if we bailed out
   due to resource constraints.  DEFAULT_VN_WALK_KIND_ specifies
   how we use the alias oracle walking during the VN process.  */

bool
run_scc_vn (vn_lookup_kind default_vn_walk_kind_)
{
  basic_block bb;
  size_t i;
  tree param;

  default_vn_walk_kind = default_vn_walk_kind_;

  init_scc_vn ();
  current_info = valid_info;

  for (param = DECL_ARGUMENTS (current_function_decl);
       param;
       param = DECL_CHAIN (param))
    {
      tree def = ssa_default_def (cfun, param);
      if (def)
	{
	  VN_INFO (def)->visited = true;
	  VN_INFO (def)->valnum = def;
	}
    }

  /* Mark all edges as possibly executable.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, bb->succs)
	e->flags |= EDGE_EXECUTABLE;
    }

  /* Walk all blocks in dominator order, value-numbering the last stmts
     SSA uses and decide whether outgoing edges are not executable.  */
  cond_dom_walker walker;
  walker.walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  if (walker.fail)
    {
      free_scc_vn ();
      return false;
    }

  /* Value-number remaining SSA names.  */
  for (i = 1; i < num_ssa_names; ++i)
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

  /* Initialize the value ids.  */

  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      vn_ssa_aux_t info;
      if (!name)
	continue;
      info = VN_INFO (name);
      if (info->valnum == name
	  || info->valnum == VN_TOP)
	info->value_id = get_next_value_id ();
      else if (is_gimple_min_invariant (info->valnum))
	info->value_id = get_or_alloc_constant_value_id (info->valnum);
    }

  /* Propagate.  */
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
	info->value_id = VN_INFO (info->valnum)->value_id;
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

  /* Now perform the actual comparison.  */
  if (TREE_CODE (e1) == TREE_CODE (e2)
      && operand_equal_p (e1, e2, OEP_PURE_SAME))
    return true;

  return false;
}


/* Return true if the nary operation NARY may trap.  This is a copy
   of stmt_could_throw_1_p adjusted to the SCCVN IL.  */

bool
vn_nary_may_trap (vn_nary_op_t nary)
{
  tree type;
  tree rhs2 = NULL_TREE;
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
  if (nary->length >= 2)
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
