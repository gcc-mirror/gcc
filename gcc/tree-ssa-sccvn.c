/* SCC value numbering for trees
   Copyright (C) 2006-2016 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "ssa.h"
#include "expmed.h"
#include "insn-config.h"
#include "emit-rtl.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "tree-inline.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "flags.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "dumpfile.h"
#include "cfgloop.h"
#include "params.h"
#include "tree-ssa-propagate.h"
#include "tree-ssa-sccvn.h"
#include "tree-cfg.h"
#include "domwalk.h"
#include "gimple-iterator.h"
#include "gimple-match.h"

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


static tree *last_vuse_ptr;
static vn_lookup_kind vn_walk_kind;
static vn_lookup_kind default_vn_walk_kind;
bitmap const_parms;

/* vn_nary_op hashtable helpers.  */

struct vn_nary_op_hasher : nofree_ptr_hash <vn_nary_op_s>
{
  typedef vn_nary_op_s *compare_type;
  static inline hashval_t hash (const vn_nary_op_s *);
  static inline bool equal (const vn_nary_op_s *, const vn_nary_op_s *);
};

/* Return the computed hashcode for nary operation P1.  */

inline hashval_t
vn_nary_op_hasher::hash (const vn_nary_op_s *vno1)
{
  return vno1->hashcode;
}

/* Compare nary operations P1 and P2 and return true if they are
   equivalent.  */

inline bool
vn_nary_op_hasher::equal (const vn_nary_op_s *vno1, const vn_nary_op_s *vno2)
{
  return vn_nary_op_eq (vno1, vno2);
}

typedef hash_table<vn_nary_op_hasher> vn_nary_op_table_type;
typedef vn_nary_op_table_type::iterator vn_nary_op_iterator_type;


/* vn_phi hashtable helpers.  */

static int
vn_phi_eq (const_vn_phi_t const vp1, const_vn_phi_t const vp2);

struct vn_phi_hasher : pointer_hash <vn_phi_s>
{ 
  static inline hashval_t hash (const vn_phi_s *);
  static inline bool equal (const vn_phi_s *, const vn_phi_s *);
  static inline void remove (vn_phi_s *);
};

/* Return the computed hashcode for phi operation P1.  */

inline hashval_t
vn_phi_hasher::hash (const vn_phi_s *vp1)
{
  return vp1->hashcode;
}

/* Compare two phi entries for equality, ignoring VN_TOP arguments.  */

inline bool
vn_phi_hasher::equal (const vn_phi_s *vp1, const vn_phi_s *vp2)
{
  return vn_phi_eq (vp1, vp2);
}

/* Free a phi operation structure VP.  */

inline void
vn_phi_hasher::remove (vn_phi_s *phi)
{
  phi->phiargs.release ();
}

typedef hash_table<vn_phi_hasher> vn_phi_table_type;
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

struct vn_reference_hasher : pointer_hash <vn_reference_s>
{
  static inline hashval_t hash (const vn_reference_s *);
  static inline bool equal (const vn_reference_s *, const vn_reference_s *);
  static inline void remove (vn_reference_s *);
};

/* Return the hashcode for a given reference operation P1.  */

inline hashval_t
vn_reference_hasher::hash (const vn_reference_s *vr1)
{
  return vr1->hashcode;
}

inline bool
vn_reference_hasher::equal (const vn_reference_s *v, const vn_reference_s *c)
{
  return vn_reference_eq (v, c);
}

inline void
vn_reference_hasher::remove (vn_reference_s *v)
{
  free_reference (v);
}

typedef hash_table<vn_reference_hasher> vn_reference_table_type;
typedef vn_reference_table_type::iterator vn_reference_iterator_type;


/* The set of hashtables and alloc_pool's for their items.  */

typedef struct vn_tables_s
{
  vn_nary_op_table_type *nary;
  vn_phi_table_type *phis;
  vn_reference_table_type *references;
  struct obstack nary_obstack;
  object_allocator<vn_phi_s> *phis_pool;
  object_allocator<vn_reference_s> *references_pool;
} *vn_tables_t;


/* vn_constant hashtable helpers.  */

struct vn_constant_hasher : free_ptr_hash <vn_constant_s>
{ 
  static inline hashval_t hash (const vn_constant_s *);
  static inline bool equal (const vn_constant_s *, const vn_constant_s *);
};

/* Hash table hash function for vn_constant_t.  */

inline hashval_t
vn_constant_hasher::hash (const vn_constant_s *vc1)
{
  return vc1->hashcode;
}

/* Hash table equality function for vn_constant_t.  */

inline bool
vn_constant_hasher::equal (const vn_constant_s *vc1, const vn_constant_s *vc2)
{
  if (vc1->hashcode != vc2->hashcode)
    return false;

  return vn_constant_eq_with_type (vc1->constant, vc2->constant);
}

static hash_table<vn_constant_hasher> *constant_to_value_id;
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

/* Return whether there is value numbering information for a given SSA name.  */

bool
has_VN_INFO (tree name)
{
  if (SSA_NAME_VERSION (name) < vn_ssa_aux_table.length ())
    return vn_ssa_aux_table[SSA_NAME_VERSION (name)] != NULL;
  return false;
}

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

  gcc_assert (SSA_NAME_VERSION (name) >= vn_ssa_aux_table.length ()
	      || vn_ssa_aux_table[SSA_NAME_VERSION (name)] == NULL);
  newinfo = XOBNEW (&vn_ssa_aux_obstack, struct vn_ssa_aux);
  memset (newinfo, 0, sizeof (struct vn_ssa_aux));
  if (SSA_NAME_VERSION (name) >= vn_ssa_aux_table.length ())
    vn_ssa_aux_table.safe_grow_cleared (SSA_NAME_VERSION (name) + 1);
  vn_ssa_aux_table[SSA_NAME_VERSION (name)] = newinfo;
  return newinfo;
}


/* Return the vn_kind the expression computed by the stmt should be
   associated with.  */

enum vn_kind
vn_get_stmt_kind (gimple *stmt)
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
  slot = constant_to_value_id->find_slot (&vc, NO_INSERT);
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
  slot = constant_to_value_id->find_slot (&vc, INSERT);
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

static void
vn_reference_op_compute_hash (const vn_reference_op_t vro1, inchash::hash &hstate)
{
  hstate.add_int (vro1->opcode);
  if (vro1->op0)
    inchash::add_expr (vro1->op0, hstate);
  if (vro1->op1)
    inchash::add_expr (vro1->op1, hstate);
  if (vro1->op2)
    inchash::add_expr (vro1->op2, hstate);
}

/* Compute a hash for the reference operation VR1 and return it.  */

static hashval_t
vn_reference_compute_hash (const vn_reference_t vr1)
{
  inchash::hash hstate;
  hashval_t result;
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
	    hstate.add_int (off);
	  off = -1;
	  if (deref
	      && vro->opcode == ADDR_EXPR)
	    {
	      if (vro->op0)
		{
		  tree op = TREE_OPERAND (vro->op0, 0);
		  hstate.add_int (TREE_CODE (op));
		  inchash::add_expr (op, hstate);
		}
	    }
	  else
	    vn_reference_op_compute_hash (vro, hstate);
	}
    }
  result = hstate.end ();
  /* ??? We would ICE later if we hash instead of adding that in. */
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
	  /* Do not look through a storage order barrier.  */
	  else if (vro1->opcode == VIEW_CONVERT_EXPR && vro1->reverse)
	    return false;
	  if (vro1->off == -1)
	    break;
	  off1 += vro1->off;
	}
      for (; vr2->operands.iterate (j, &vro2); j++)
	{
	  if (vro2->opcode == MEM_REF)
	    deref2 = true;
	  /* Do not look through a storage order barrier.  */
	  else if (vro2->opcode == VIEW_CONVERT_EXPR && vro2->reverse)
	    return false;
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

static void
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
      temp.clique = MR_DEPENDENCE_CLIQUE (ref);
      temp.base = MR_DEPENDENCE_BASE (ref);
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
	    {
	      offset_int off = mem_ref_offset (ref);
	      if (wi::fits_shwi_p (off))
		temp.off = off.to_shwi ();
	    }
	  temp.clique = MR_DEPENDENCE_CLIQUE (ref);
	  temp.base = MR_DEPENDENCE_BASE (ref);
	  temp.reverse = REF_REVERSE_STORAGE_ORDER (ref);
	  break;
	case BIT_FIELD_REF:
	  /* Record bits, position and storage order.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  if (tree_fits_shwi_p (TREE_OPERAND (ref, 2)))
	    {
	      HOST_WIDE_INT off = tree_to_shwi (TREE_OPERAND (ref, 2));
	      if (off % BITS_PER_UNIT == 0)
		temp.off = off / BITS_PER_UNIT;
	    }
	  temp.reverse = REF_REVERSE_STORAGE_ORDER (ref);
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
			 + (wi::to_offset (bit_offset) >> LOG2_BITS_PER_UNIT));
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
	  break;
	  /* These are only interesting for their operands, their
	     existence, and their type.  They will never be the last
	     ref in the chain of references (IE they require an
	     operand), so we don't have to put anything
	     for op* as it will be handled by the iteration  */
	case REALPART_EXPR:
	  temp.off = 0;
	  break;
	case VIEW_CONVERT_EXPR:
	  temp.off = 0;
	  temp.reverse = storage_order_barrier_p (ref);
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
  offset_int offset = 0;
  offset_int max_size;
  offset_int size = -1;
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
      machine_mode mode = TYPE_MODE (type);
      if (mode == BLKmode)
	size_tree = TYPE_SIZE (type);
      else
	size = int (GET_MODE_BITSIZE (mode));
    }
  if (size_tree != NULL_TREE
      && TREE_CODE (size_tree) == INTEGER_CST)
    size = wi::to_offset (size_tree);

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
	  MR_DEPENDENCE_CLIQUE (*op0_p) = op->clique;
	  MR_DEPENDENCE_BASE (*op0_p) = op->base;
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
	  offset += wi::to_offset (op->op1);
	  break;

	case COMPONENT_REF:
	  {
	    tree field = op->op0;
	    /* We do not have a complete COMPONENT_REF tree here so we
	       cannot use component_ref_field_offset.  Do the interesting
	       parts manually.  */
	    tree this_offset = DECL_FIELD_OFFSET (field);

	    if (op->op1 || TREE_CODE (this_offset) != INTEGER_CST)
	      max_size = -1;
	    else
	      {
		offset_int woffset = (wi::to_offset (this_offset)
				      << LOG2_BITS_PER_UNIT);
		woffset += wi::to_offset (DECL_FIELD_BIT_OFFSET (field));
		offset += woffset;
	      }
	    break;
	  }

	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  /* We recorded the lower bound and the element size.  */
	  if (TREE_CODE (op->op0) != INTEGER_CST
	      || TREE_CODE (op->op1) != INTEGER_CST
	      || TREE_CODE (op->op2) != INTEGER_CST)
	    max_size = -1;
	  else
	    {
	      offset_int woffset
		= wi::sext (wi::to_offset (op->op0) - wi::to_offset (op->op1),
			    TYPE_PRECISION (TREE_TYPE (op->op0)));
	      woffset *= wi::to_offset (op->op2);
	      woffset <<= LOG2_BITS_PER_UNIT;
	      offset += woffset;
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
  ref->ref_alias_set = set;
  if (base_alias_set != -1)
    ref->base_alias_set = base_alias_set;
  else
    ref->base_alias_set = get_alias_set (base);
  /* We discount volatiles from value-numbering elsewhere.  */
  ref->volatile_p = false;

  if (!wi::fits_shwi_p (size) || wi::neg_p (size))
    {
      ref->offset = 0;
      ref->size = -1;
      ref->max_size = -1;
      return true;
    }

  ref->size = size.to_shwi ();

  if (!wi::fits_shwi_p (offset))
    {
      ref->offset = 0;
      ref->max_size = -1;
      return true;
    }

  ref->offset = offset.to_shwi ();

  if (!wi::fits_shwi_p (max_size) || wi::neg_p (max_size))
    ref->max_size = -1;
  else
    ref->max_size = max_size.to_shwi ();

  return true;
}

/* Copy the operations present in load/store/call REF into RESULT, a vector of
   vn_reference_op_s's.  */

static void
copy_reference_ops_from_call (gcall *call,
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
  if (gimple_call_with_bounds_p (call))
    temp.with_bounds = 1;
  result->safe_push (temp);

  /* Copy the call arguments.  As they can be references as well,
     just chain them together.  */
  for (i = 0; i < gimple_call_num_args (call); ++i)
    {
      tree callarg = gimple_call_arg (call, i);
      copy_reference_ops_from_ref (callarg, result);
    }
}

/* Fold *& at position *I_P in a vn_reference_op_s vector *OPS.  Updates
   *I_P to point to the last element of the replacement.  */
static bool
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
      return true;
    }
  return false;
}

/* Fold *& at position *I_P in a vn_reference_op_s vector *OPS.  Updates
   *I_P to point to the last element of the replacement.  */
static bool
vn_reference_maybe_forwprop_address (vec<vn_reference_op_s> *ops,
				     unsigned int *i_p)
{
  unsigned int i = *i_p;
  vn_reference_op_t op = &(*ops)[i];
  vn_reference_op_t mem_op = &(*ops)[i - 1];
  gimple *def_stmt;
  enum tree_code code;
  offset_int off;

  def_stmt = SSA_NAME_DEF_STMT (op->op0);
  if (!is_gimple_assign (def_stmt))
    return false;

  code = gimple_assign_rhs_code (def_stmt);
  if (code != ADDR_EXPR
      && code != POINTER_PLUS_EXPR)
    return false;

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
      /* If that didn't work because the address isn't invariant propagate
         the reference tree from the address operation in case the current
	 dereference isn't offsetted.  */
      if (!addr_base
	  && *i_p == ops->length () - 1
	  && off == 0
	  /* This makes us disable this transform for PRE where the
	     reference ops might be also used for code insertion which
	     is invalid.  */
	  && default_vn_walk_kind == VN_WALKREWRITE)
	{
	  auto_vec<vn_reference_op_s, 32> tem;
	  copy_reference_ops_from_ref (TREE_OPERAND (addr, 0), &tem);
	  ops->pop ();
	  ops->pop ();
	  ops->safe_splice (tem);
	  --*i_p;
	  return true;
	}
      if (!addr_base
	  || TREE_CODE (addr_base) != MEM_REF)
	return false;

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
	return false;

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
  return true;
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

  /* Simplify reads from constants or constant initializers.  */
  else if (BITS_PER_UNIT == 8
	   && is_gimple_reg_type (ref->type)
	   && (!INTEGRAL_TYPE_P (ref->type)
	       || TYPE_PRECISION (ref->type) % BITS_PER_UNIT == 0))
    {
      HOST_WIDE_INT off = 0;
      HOST_WIDE_INT size;
      if (INTEGRAL_TYPE_P (ref->type))
	size = TYPE_PRECISION (ref->type);
      else
	size = tree_to_shwi (TYPE_SIZE (ref->type));
      if (size % BITS_PER_UNIT != 0
	  || size > MAX_BITSIZE_MODE_ANY_MODE)
	return NULL_TREE;
      size /= BITS_PER_UNIT;
      unsigned i;
      for (i = 0; i < operands.length (); ++i)
	{
	  if (operands[i].off == -1)
	    return NULL_TREE;
	  off += operands[i].off;
	  if (operands[i].opcode == MEM_REF)
	    {
	      ++i;
	      break;
	    }
	}
      vn_reference_op_t base = &operands[--i];
      tree ctor = error_mark_node;
      tree decl = NULL_TREE;
      if (TREE_CODE_CLASS (base->opcode) == tcc_constant)
	ctor = base->op0;
      else if (base->opcode == MEM_REF
	       && base[1].opcode == ADDR_EXPR
	       && (TREE_CODE (TREE_OPERAND (base[1].op0, 0)) == VAR_DECL
		   || TREE_CODE (TREE_OPERAND (base[1].op0, 0)) == CONST_DECL))
	{
	  decl = TREE_OPERAND (base[1].op0, 0);
	  ctor = ctor_for_folding (decl);
	}
      if (ctor == NULL_TREE)
	return build_zero_cst (ref->type);
      else if (ctor != error_mark_node)
	{
	  if (decl)
	    {
	      tree res = fold_ctor_reference (ref->type, ctor,
					      off * BITS_PER_UNIT,
					      size * BITS_PER_UNIT, decl);
	      if (res)
		{
		  STRIP_USELESS_TYPE_CONVERSION (res);
		  if (is_gimple_min_invariant (res))
		    return res;
		}
	    }
	  else
	    {
	      unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
	      int len = native_encode_expr (ctor, buf, size, off);
	      if (len > 0)
		return native_interpret_expr (ref->type, buf, len);
	    }
	}
    }

  return NULL_TREE;
}

/* Return true if OPS contain a storage order barrier.  */

static bool
contains_storage_order_barrier_p (vec<vn_reference_op_s> ops)
{
  vn_reference_op_t op;
  unsigned i;

  FOR_EACH_VEC_ELT (ops, i, op)
    if (op->opcode == VIEW_CONVERT_EXPR && op->reverse)
      return true;

  return false;
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
	{
	  if (vn_reference_fold_indirect (&orig, &i))
	    *valueized_anything = true;
	}
      else if (i > 0
	       && vro->opcode == SSA_NAME
	       && orig[i - 1].opcode == MEM_REF)
	{
	  if (vn_reference_maybe_forwprop_address (&orig, &i))
	    *valueized_anything = true;
	}
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
valueize_shared_reference_ops_from_call (gcall *call)
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
  slot = current_info->references->find_slot_with_hash (vr, hash, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->references->find_slot_with_hash (vr, hash, NO_INSERT);
  if (slot)
    {
      if (vnresult)
	*vnresult = (vn_reference_t)*slot;
      return ((vn_reference_t)*slot)->result;
    }

  return NULL_TREE;
}

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
  slot = current_info->references->find_slot_with_hash (vr, hash, NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->references->find_slot_with_hash (vr, hash, NO_INSERT);
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
  vn_reference_s vr1;
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

static vn_nary_op_t vn_nary_op_insert_stmt (gimple *stmt, tree result);

/* Hook for maybe_push_res_to_seq, lookup the expression in the VN tables.  */

static tree
vn_lookup_simplify_result (code_helper rcode, tree type, tree *ops)
{
  if (!rcode.is_tree_code ())
    return NULL_TREE;
  vn_nary_op_t vnresult = NULL;
  return vn_nary_op_lookup_pieces (TREE_CODE_LENGTH ((tree_code) rcode),
				   (tree_code) rcode, type, ops, &vnresult);
}

/* Return a value-number for RCODE OPS... either by looking up an existing
   value-number for the simplified result or by inserting the operation.  */

static tree
vn_nary_build_or_lookup (code_helper rcode, tree type, tree *ops)
{
  tree result = NULL_TREE;
  /* We will be creating a value number for
       ROCDE (OPS...).
     So first simplify and lookup this expression to see if it
     is already available.  */
  mprts_hook = vn_lookup_simplify_result;
  bool res = false;
  switch (TREE_CODE_LENGTH ((tree_code) rcode))
    {
    case 1:
      res = gimple_resimplify1 (NULL, &rcode, type, ops, vn_valueize);
      break;
    case 2:
      res = gimple_resimplify2 (NULL, &rcode, type, ops, vn_valueize);
      break;
    case 3:
      res = gimple_resimplify3 (NULL, &rcode, type, ops, vn_valueize);
      break;
    }
  mprts_hook = NULL;
  gimple *new_stmt = NULL;
  if (res
      && gimple_simplified_result_is_gimple_val (rcode, ops))
    /* The expression is already available.  */
    result = ops[0];
  else
    {
      tree val = vn_lookup_simplify_result (rcode, type, ops);
      if (!val)
	{
	  gimple_seq stmts = NULL;
	  result = maybe_push_res_to_seq (rcode, type, ops, &stmts);
	  if (result)
	    {
	      gcc_assert (gimple_seq_singleton_p (stmts));
	      new_stmt = gimple_seq_first_stmt (stmts);
	    }
	}
      else
	/* The expression is already available.  */
	result = val;
    }
  if (new_stmt)
    {
      /* The expression is not yet available, value-number lhs to
	 the new SSA_NAME we created.  */
      /* Initialize value-number information properly.  */
      VN_INFO_GET (result)->valnum = result;
      VN_INFO (result)->value_id = get_next_value_id ();
      gimple_seq_add_stmt_without_update (&VN_INFO (result)->expr,
					  new_stmt);
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
	  vn_nary_op_insert_stmt (new_stmt, result);
	  current_info = optimistic_info;
	}
      else
	vn_nary_op_insert_stmt (new_stmt, result);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Inserting name ");
	  print_generic_expr (dump_file, result, 0);
	  fprintf (dump_file, " for expression ");
	  print_gimple_expr (dump_file, new_stmt, 0, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
    }
  return result;
}

/* Callback for walk_non_aliased_vuses.  Tries to perform a lookup
   from the statement defining VUSE and if not successful tries to
   translate *REFP and VR_ through an aggregate copy at the definition
   of VUSE.  If *DISAMBIGUATE_ONLY is true then do not perform translation
   of *REF and *VR.  If only disambiguation was performed then
   *DISAMBIGUATE_ONLY is set to true.  */

static void *
vn_reference_lookup_3 (ao_ref *ref, tree vuse, void *vr_,
		       bool *disambiguate_only)
{
  vn_reference_t vr = (vn_reference_t)vr_;
  gimple *def_stmt = SSA_NAME_DEF_STMT (vuse);
  tree base = ao_ref_base (ref);
  HOST_WIDE_INT offset, maxsize;
  static vec<vn_reference_op_s>
    lhs_ops = vNULL;
  ao_ref lhs_ref;
  bool lhs_ref_ok = false;

  /* If the reference is based on a parameter that was determined as
     pointing to readonly memory it doesn't change.  */
  if (TREE_CODE (base) == MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (base, 0))
      && bitmap_bit_p (const_parms,
		       SSA_NAME_VERSION (TREE_OPERAND (base, 0))))
    {
      *disambiguate_only = true;
      return NULL;
    }

  /* First try to disambiguate after value-replacing in the definitions LHS.  */
  if (is_gimple_assign (def_stmt))
    {
      tree lhs = gimple_assign_lhs (def_stmt);
      bool valueized_anything = false;
      /* Avoid re-allocation overhead.  */
      lhs_ops.truncate (0);
      copy_reference_ops_from_ref (lhs, &lhs_ops);
      lhs_ops = valueize_refs_1 (lhs_ops, &valueized_anything);
      if (valueized_anything)
	{
	  lhs_ref_ok = ao_ref_init_from_vn_reference (&lhs_ref,
						      get_alias_set (lhs),
						      TREE_TYPE (lhs), lhs_ops);
	  if (lhs_ref_ok
	      && !refs_may_alias_p_1 (ref, &lhs_ref, true))
	    {
	      *disambiguate_only = true;
	      return NULL;
	    }
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
	  bool res = call_may_clobber_ref_p_1 (as_a <gcall *> (def_stmt),
					       ref);
	  for (unsigned i = 0; i < gimple_call_num_args (def_stmt); ++i)
	    gimple_call_set_arg (def_stmt, i, oldargs[i]);
	  if (!res)
	    {
	      *disambiguate_only = true;
	      return NULL;
	    }
	}
    }

  if (*disambiguate_only)
    return (void *)-1;

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
      bool reverse;
      base2 = get_ref_base_and_extent (ref2, &offset2, &size2, &maxsize2,
				       &reverse);
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
      bool reverse;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2, &reverse);
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
  else if (ref->size == maxsize
	   && is_gimple_reg_type (vr->type)
	   && !contains_storage_order_barrier_p (vr->operands)
	   && gimple_assign_single_p (def_stmt)
	   && CHAR_BIT == 8 && BITS_PER_UNIT == 8
	   && maxsize % BITS_PER_UNIT == 0
	   && offset % BITS_PER_UNIT == 0
	   && (is_gimple_min_invariant (gimple_assign_rhs1 (def_stmt))
	       || (TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
		   && is_gimple_min_invariant (SSA_VAL (gimple_assign_rhs1 (def_stmt))))))
    {
      tree base2;
      HOST_WIDE_INT offset2, size2, maxsize2;
      bool reverse;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2, &reverse);
      if (!reverse
	  && maxsize2 != -1
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

	  tree rhs = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (rhs) == SSA_NAME)
	    rhs = SSA_VAL (rhs);
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
	   && !contains_storage_order_barrier_p (vr->operands)
	   && gimple_assign_single_p (def_stmt)
	   && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
    {
      tree base2;
      HOST_WIDE_INT offset2, size2, maxsize2;
      bool reverse;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2,
				       &reverse);
      if (!reverse
	  && maxsize2 != -1
	  && maxsize2 == size2
	  && operand_equal_p (base, base2, 0)
	  && offset2 <= offset
	  && offset2 + size2 >= offset + maxsize
	  /* ???  We can't handle bitfield precision extracts without
	     either using an alternate type for the BIT_FIELD_REF and
	     then doing a conversion or possibly adjusting the offset
	     according to endianess.  */
	  && (! INTEGRAL_TYPE_P (vr->type)
	      || ref->size == TYPE_PRECISION (vr->type))
	  && ref->size % BITS_PER_UNIT == 0)
	{
	  code_helper rcode = BIT_FIELD_REF;
	  tree ops[3];
	  ops[0] = SSA_VAL (gimple_assign_rhs1 (def_stmt));
	  ops[1] = bitsize_int (ref->size);
	  ops[2] = bitsize_int (offset - offset2);
	  tree val = vn_nary_build_or_lookup (rcode, vr->type, ops);
	  if (val)
	    {
	      vn_reference_t res = vn_reference_lookup_or_insert_for_pieces
		  (vuse, vr->set, vr->type, vr->operands, val);
	      return res;
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
      HOST_WIDE_INT maxsize2;
      int i, j, k;
      auto_vec<vn_reference_op_s> rhs;
      vn_reference_op_t vro;
      ao_ref r;

      if (!lhs_ref_ok)
	return (void *)-1;

      /* See if the assignment kills REF.  */
      base2 = ao_ref_base (&lhs_ref);
      maxsize2 = lhs_ref.max_size;
      if (maxsize2 == -1
	  || (base != base2
	      && (TREE_CODE (base) != MEM_REF
		  || TREE_CODE (base2) != MEM_REF
		  || TREE_OPERAND (base, 0) != TREE_OPERAND (base2, 0)
		  || !tree_int_cst_equal (TREE_OPERAND (base, 1),
					  TREE_OPERAND (base2, 1))))
	  || !stmt_kills_ref_p (def_stmt, ref))
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
      HOST_WIDE_INT extra_off = 0;
      if (j == 0 && i >= 0
	  && lhs_ops[0].opcode == MEM_REF
	  && lhs_ops[0].off != -1)
	{
	  if (lhs_ops[0].off == vr->operands[i].off)
	    i--, j--;
	  else if (vr->operands[i].opcode == MEM_REF
		   && vr->operands[i].off != -1)
	    {
	      extra_off = vr->operands[i].off - lhs_ops[0].off;
	      i--, j--;
	    }
	}

      /* i now points to the first additional op.
	 ???  LHS may not be completely contained in VR, one or more
	 VIEW_CONVERT_EXPRs could be in its way.  We could at least
	 try handling outermost VIEW_CONVERT_EXPRs.  */
      if (j != -1)
	return (void *)-1;

      /* Punt if the additional ops contain a storage order barrier.  */
      for (k = i; k >= 0; k--)
	{
	  vro = &vr->operands[k];
	  if (vro->opcode == VIEW_CONVERT_EXPR && vro->reverse)
	    return (void *)-1;
	}

      /* Now re-write REF to be based on the rhs of the assignment.  */
      copy_reference_ops_from_ref (gimple_assign_rhs1 (def_stmt), &rhs);

      /* Apply an extra offset to the inner MEM_REF of the RHS.  */
      if (extra_off != 0)
	{
	  if (rhs.length () < 2
	      || rhs[0].opcode != MEM_REF
	      || rhs[0].off == -1)
	    return (void *)-1;
	  rhs[0].off += extra_off;
	  rhs[0].op0 = int_const_binop (PLUS_EXPR, rhs[0].op0,
					build_int_cst (TREE_TYPE (rhs[0].op0),
						       extra_off));
	}

      /* We need to pre-pend vr->operands[0..i] to rhs.  */
      vec<vn_reference_op_s> old = vr->operands;
      if (i + 1 + rhs.length () > vr->operands.length ())
	{
	  vr->operands.safe_grow (i + 1 + rhs.length ());
	  if (old == shared_lookup_references)
	    shared_lookup_references = vr->operands;
	}
      else
	vr->operands.truncate (i + 1 + rhs.length ());
      FOR_EACH_VEC_ELT (rhs, j, vro)
	vr->operands[i + 1 + j] = *vro;
      vr->operands = valueize_refs (vr->operands);
      if (old == shared_lookup_references)
	shared_lookup_references = vr->operands;
      vr->hashcode = vn_reference_compute_hash (vr);

      /* Try folding the new reference to a constant.  */
      tree val = fully_constant_vn_reference_p (vr);
      if (val)
	return vn_reference_lookup_or_insert_for_pieces
		 (vuse, vr->set, vr->type, vr->operands, val);

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
	{
	  lhs = SSA_VAL (lhs);
	  if (TREE_CODE (lhs) == SSA_NAME)
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (lhs);
	      if (gimple_assign_single_p (def_stmt)
		  && gimple_assign_rhs_code (def_stmt) == ADDR_EXPR)
		lhs = gimple_assign_rhs1 (def_stmt);
	    }
	}
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
	      if (TREE_CODE (lhs) == SSA_NAME)
		lhs = SSA_VAL (lhs);
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

      at = offset / BITS_PER_UNIT;
      if (TREE_CODE (base) == MEM_REF)
	at += tree_to_uhwi (TREE_OPERAND (base, 1));
      /* If the access is completely outside of the memcpy destination
	 area there is no aliasing.  */
      if (lhs_offset >= at + maxsize / BITS_PER_UNIT
	  || lhs_offset + copy_size <= at)
	return NULL;
      /* And the access has to be contained within the memcpy destination.  */
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
	    shared_lookup_references = vr->operands;
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

      /* Try folding the new reference to a constant.  */
      tree val = fully_constant_vn_reference_p (vr);
      if (val)
	return vn_reference_lookup_or_insert_for_pieces
		 (vuse, vr->set, vr->type, vr->operands, val);

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
						  vn_reference_lookup_3,
						  vuse_ssa_val, &vr1);
      gcc_checking_assert (vr1.operands == shared_lookup_references);
    }

  if (*vnresult)
     return (*vnresult)->result;

  return NULL_TREE;
}

/* Lookup OP in the current hash table, and return the resulting value
   number if it exists in the hash table.  Return NULL_TREE if it does
   not exist in the hash table or if the result field of the structure
   was NULL..  VNRESULT will be filled in with the vn_reference_t
   stored in the hashtable if one exists.  When TBAA_P is false assume
   we are looking up a store and treat it as having alias-set zero.  */

tree
vn_reference_lookup (tree op, tree vuse, vn_lookup_kind kind,
		     vn_reference_t *vnresult, bool tbaa_p)
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
  vr1.set = tbaa_p ? get_alias_set (op) : 0;
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
      if (! tbaa_p)
	r.ref_alias_set = r.base_alias_set = 0;
      vn_walk_kind = kind;
      wvnresult =
	(vn_reference_t)walk_non_aliased_vuses (&r, vr1.vuse,
						vn_reference_lookup_2,
						vn_reference_lookup_3,
						vuse_ssa_val, &vr1);
      gcc_checking_assert (vr1.operands == shared_lookup_references);
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

/* Lookup CALL in the current hash table and return the entry in
   *VNRESULT if found.  Populates *VR for the hashtable lookup.  */

void
vn_reference_lookup_call (gcall *call, vn_reference_t *vnresult,
			  vn_reference_t vr)
{
  if (vnresult)
    *vnresult = NULL;

  tree vuse = gimple_vuse (call);

  vr->vuse = vuse ? SSA_VAL (vuse) : NULL_TREE;
  vr->operands = valueize_shared_reference_ops_from_call (call);
  vr->type = gimple_expr_type (call);
  vr->set = 0;
  vr->hashcode = vn_reference_compute_hash (vr);
  vn_reference_lookup_1 (vr, vnresult);
}

/* Insert OP into the current hash table with a value number of
   RESULT, and return the resulting reference structure we created.  */

static vn_reference_t
vn_reference_insert (tree op, tree result, tree vuse, tree vdef)
{
  vn_reference_s **slot;
  vn_reference_t vr1;
  bool tem;

  vr1 = current_info->references_pool->allocate ();
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

  slot = current_info->references->find_slot_with_hash (vr1, vr1->hashcode,
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

  vr1 = current_info->references_pool->allocate ();
  vr1->value_id = value_id;
  vr1->vuse = vuse ? SSA_VAL (vuse) : NULL_TREE;
  vr1->operands = valueize_refs (operands);
  vr1->type = type;
  vr1->set = set;
  vr1->hashcode = vn_reference_compute_hash (vr1);
  if (result && TREE_CODE (result) == SSA_NAME)
    result = SSA_VAL (result);
  vr1->result = result;

  slot = current_info->references->find_slot_with_hash (vr1, vr1->hashcode,
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

static hashval_t
vn_nary_op_compute_hash (const vn_nary_op_t vno1)
{
  inchash::hash hstate;
  unsigned i;

  for (i = 0; i < vno1->length; ++i)
    if (TREE_CODE (vno1->op[i]) == SSA_NAME)
      vno1->op[i] = SSA_VAL (vno1->op[i]);

  if (((vno1->length == 2
	&& commutative_tree_code (vno1->opcode))
       || (vno1->length == 3
	   && commutative_ternary_tree_code (vno1->opcode)))
      && tree_swap_operands_p (vno1->op[0], vno1->op[1], false))
    std::swap (vno1->op[0], vno1->op[1]);
  else if (TREE_CODE_CLASS (vno1->opcode) == tcc_comparison
	   && tree_swap_operands_p (vno1->op[0], vno1->op[1], false))
    {
      std::swap (vno1->op[0], vno1->op[1]);
      vno1->opcode = swap_tree_comparison  (vno1->opcode);
    }

  hstate.add_int (vno1->opcode);
  for (i = 0; i < vno1->length; ++i)
    inchash::add_expr (vno1->op[i], hstate);

  return hstate.end ();
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
vn_nary_length_from_stmt (gimple *stmt)
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
init_vn_nary_op_from_stmt (vn_nary_op_t vno, gimple *stmt)
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
  slot = current_info->nary->find_slot_with_hash (vno, vno->hashcode,
						  NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->nary->find_slot_with_hash (vno, vno->hashcode,
						  NO_INSERT);
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
vn_nary_op_lookup_stmt (gimple *stmt, vn_nary_op_t *vnresult)
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
vn_nary_op_insert_into (vn_nary_op_t vno, vn_nary_op_table_type *table,
			bool compute_hash)
{
  vn_nary_op_s **slot;

  if (compute_hash)
    vno->hashcode = vn_nary_op_compute_hash (vno);

  slot = table->find_slot_with_hash (vno, vno->hashcode, INSERT);
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

static vn_nary_op_t
vn_nary_op_insert_stmt (gimple *stmt, tree result)
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
  inchash::hash hstate (vp1->phiargs.length () > 2
			? vp1->block->index : vp1->phiargs.length ());
  tree phi1op;
  tree type;
  edge e;
  edge_iterator ei;

  /* If all PHI arguments are constants we need to distinguish
     the PHI node via its type.  */
  type = vp1->type;
  hstate.merge_hash (vn_hash_type (type));

  FOR_EACH_EDGE (e, ei, vp1->block->preds)
    {
      /* Don't hash backedge values they need to be handled as VN_TOP
         for optimistic value-numbering.  */
      if (e->flags & EDGE_DFS_BACK)
	continue;

      phi1op = vp1->phiargs[e->dest_idx];
      if (phi1op == VN_TOP)
	continue;
      inchash::add_expr (phi1op, hstate);
    }

  return hstate.end ();
}


/* Return true if COND1 and COND2 represent the same condition, set
   *INVERTED_P if one needs to be inverted to make it the same as
   the other.  */

static bool
cond_stmts_equal_p (gcond *cond1, gcond *cond2, bool *inverted_p)
{
  enum tree_code code1 = gimple_cond_code (cond1);
  enum tree_code code2 = gimple_cond_code (cond2);
  tree lhs1 = gimple_cond_lhs (cond1);
  tree lhs2 = gimple_cond_lhs (cond2);
  tree rhs1 = gimple_cond_rhs (cond1);
  tree rhs2 = gimple_cond_rhs (cond2);

  *inverted_p = false;
  if (code1 == code2)
    ;
  else if (code1 == swap_tree_comparison (code2))
    std::swap (lhs2, rhs2);
  else if (code1 == invert_tree_comparison (code2, HONOR_NANS (lhs2)))
    *inverted_p = true;
  else if (code1 == invert_tree_comparison
	   	      (swap_tree_comparison (code2), HONOR_NANS (lhs2)))
    {
      std::swap (lhs2, rhs2);
      *inverted_p = true;
    }
  else
    return false;

  lhs1 = vn_valueize (lhs1);
  rhs1 = vn_valueize (rhs1);
  lhs2 = vn_valueize (lhs2);
  rhs2 = vn_valueize (rhs2);
  return ((expressions_equal_p (lhs1, lhs2)
	   && expressions_equal_p (rhs1, rhs2))
	  || (commutative_tree_code (code1)
	      && expressions_equal_p (lhs1, rhs2)
	      && expressions_equal_p (rhs1, lhs2)));
}

/* Compare two phi entries for equality, ignoring VN_TOP arguments.  */

static int
vn_phi_eq (const_vn_phi_t const vp1, const_vn_phi_t const vp2)
{
  if (vp1->hashcode != vp2->hashcode)
    return false;

  if (vp1->block != vp2->block)
    {
      if (vp1->phiargs.length () != vp2->phiargs.length ())
	return false;

      switch (vp1->phiargs.length ())
	{
	case 1:
	  /* Single-arg PHIs are just copies.  */
	  break;

	case 2:
	  {
	    /* Rule out backedges into the PHI.  */
	    if (vp1->block->loop_father->header == vp1->block
		|| vp2->block->loop_father->header == vp2->block)
	      return false;

	    /* If the PHI nodes do not have compatible types
	       they are not the same.  */
	    if (!types_compatible_p (vp1->type, vp2->type))
	      return false;

	    basic_block idom1
	      = get_immediate_dominator (CDI_DOMINATORS, vp1->block);
	    basic_block idom2
	      = get_immediate_dominator (CDI_DOMINATORS, vp2->block);
	    /* If the immediate dominator end in switch stmts multiple
	       values may end up in the same PHI arg via intermediate
	       CFG merges.  */
	    if (EDGE_COUNT (idom1->succs) != 2
		|| EDGE_COUNT (idom2->succs) != 2)
	      return false;

	    /* Verify the controlling stmt is the same.  */
	    gimple *last1 = last_stmt (idom1);
	    gimple *last2 = last_stmt (idom2);
	    if (gimple_code (last1) != GIMPLE_COND
		|| gimple_code (last2) != GIMPLE_COND)
	      return false;
	    bool inverted_p;
	    if (! cond_stmts_equal_p (as_a <gcond *> (last1),
				      as_a <gcond *> (last2), &inverted_p))
	      return false;

	    /* Get at true/false controlled edges into the PHI.  */
	    edge te1, te2, fe1, fe2;
	    if (! extract_true_false_controlled_edges (idom1, vp1->block,
						       &te1, &fe1)
		|| ! extract_true_false_controlled_edges (idom2, vp2->block,
							  &te2, &fe2))
	      return false;

	    /* Swap edges if the second condition is the inverted of the
	       first.  */
	    if (inverted_p)
	      std::swap (te2, fe2);

	    /* ???  Handle VN_TOP specially.  */
	    if (! expressions_equal_p (vp1->phiargs[te1->dest_idx],
				       vp2->phiargs[te2->dest_idx])
		|| ! expressions_equal_p (vp1->phiargs[fe1->dest_idx],
					  vp2->phiargs[fe2->dest_idx]))
	      return false;

	    return true;
	  }

	default:
	  return false;
	}
    }

  /* If the PHI nodes do not have compatible types
     they are not the same.  */
  if (!types_compatible_p (vp1->type, vp2->type))
    return false;

  /* Any phi in the same block will have it's arguments in the
     same edge order, because of how we store phi nodes.  */
  int i;
  tree phi1op;
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

static vec<tree> shared_lookup_phiargs;

/* Lookup PHI in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

static tree
vn_phi_lookup (gimple *phi)
{
  vn_phi_s **slot;
  struct vn_phi_s vp1;
  edge e;
  edge_iterator ei;

  shared_lookup_phiargs.truncate (0);
  shared_lookup_phiargs.safe_grow (gimple_phi_num_args (phi));

  /* Canonicalize the SSA_NAME's to their value number.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    {
      tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      shared_lookup_phiargs[e->dest_idx] = def;
    }
  vp1.type = TREE_TYPE (gimple_phi_result (phi));
  vp1.phiargs = shared_lookup_phiargs;
  vp1.block = gimple_bb (phi);
  vp1.hashcode = vn_phi_compute_hash (&vp1);
  slot = current_info->phis->find_slot_with_hash (&vp1, vp1.hashcode,
						  NO_INSERT);
  if (!slot && current_info == optimistic_info)
    slot = valid_info->phis->find_slot_with_hash (&vp1, vp1.hashcode,
						  NO_INSERT);
  if (!slot)
    return NULL_TREE;
  return (*slot)->result;
}

/* Insert PHI into the current hash table with a value number of
   RESULT.  */

static vn_phi_t
vn_phi_insert (gimple *phi, tree result)
{
  vn_phi_s **slot;
  vn_phi_t vp1 = current_info->phis_pool->allocate ();
  vec<tree> args = vNULL;
  edge e;
  edge_iterator ei;

  args.safe_grow (gimple_phi_num_args (phi));

  /* Canonicalize the SSA_NAME's to their value number.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    {
      tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);
      def = TREE_CODE (def) == SSA_NAME ? SSA_VAL (def) : def;
      args[e->dest_idx] = def;
    }
  vp1->value_id = VN_INFO (result)->value_id;
  vp1->type = TREE_TYPE (gimple_phi_result (phi));
  vp1->phiargs = args;
  vp1->block = gimple_bb (phi);
  vp1->result = result;
  vp1->hashcode = vn_phi_compute_hash (vp1);

  slot = current_info->phis->find_slot_with_hash (vp1, vp1->hashcode, INSERT);

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

/* Return true if BB1 is dominated by BB2 taking into account edges
   that are not executable.  */

static bool
dominated_by_p_w_unex (basic_block bb1, basic_block bb2)
{
  edge_iterator ei;
  edge e;

  if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
    return true;

  /* Before iterating we'd like to know if there exists a
     (executable) path from bb2 to bb1 at all, if not we can
     directly return false.  For now simply iterate once.  */

  /* Iterate to the single executable bb1 predecessor.  */
  if (EDGE_COUNT (bb1->preds) > 1)
    {
      edge prede = NULL;
      FOR_EACH_EDGE (e, ei, bb1->preds)
	if (e->flags & EDGE_EXECUTABLE)
	  {
	    if (prede)
	      {
		prede = NULL;
		break;
	      }
	    prede = e;
	  }
      if (prede)
	{
	  bb1 = prede->src;

	  /* Re-do the dominance check with changed bb1.  */
	  if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
	    return true;
	}
    }

  /* Iterate to the single executable bb2 successor.  */
  edge succe = NULL;
  FOR_EACH_EDGE (e, ei, bb2->succs)
    if (e->flags & EDGE_EXECUTABLE)
      {
	if (succe)
	  {
	    succe = NULL;
	    break;
	  }
	succe = e;
      }
  if (succe)
    {
      /* Verify the reached block is only reached through succe.
	 If there is only one edge we can spare us the dominator
	 check and iterate directly.  */
      if (EDGE_COUNT (succe->dest->preds) > 1)
	{
	  FOR_EACH_EDGE (e, ei, succe->dest->preds)
	    if (e != succe
		&& (e->flags & EDGE_EXECUTABLE))
	      {
		succe = NULL;
		break;
	      }
	}
      if (succe)
	{
	  bb2 = succe->dest;

	  /* Re-do the dominance check with changed bb2.  */
	  if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
	    return true;
	}
    }

  /* We could now iterate updating bb1 / bb2.  */
  return false;
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
	      && ((TREE_CODE (to) == SSA_NAME
		   && (to == from || SSA_VAL (to) == to))
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
      /* If we equate two SSA names we have to make the side-band info
         of the leader conservative (and remember whatever original value
	 was present).  */
      if (TREE_CODE (to) == SSA_NAME)
	{
	  if (INTEGRAL_TYPE_P (TREE_TYPE (to))
	      && SSA_NAME_RANGE_INFO (to))
	    {
	      if (SSA_NAME_IS_DEFAULT_DEF (to)
		  || dominated_by_p_w_unex
			(gimple_bb (SSA_NAME_DEF_STMT (from)),
			 gimple_bb (SSA_NAME_DEF_STMT (to))))
		/* Keep the info from the dominator.  */
		;
	      else if (SSA_NAME_IS_DEFAULT_DEF (from)
		       || dominated_by_p_w_unex
			    (gimple_bb (SSA_NAME_DEF_STMT (to)),
			     gimple_bb (SSA_NAME_DEF_STMT (from))))
		{
		  /* Save old info.  */
		  if (! VN_INFO (to)->info.range_info)
		    {
		      VN_INFO (to)->info.range_info = SSA_NAME_RANGE_INFO (to);
		      VN_INFO (to)->range_info_anti_range_p
			= SSA_NAME_ANTI_RANGE_P (to);
		    }
		  /* Use that from the dominator.  */
		  SSA_NAME_RANGE_INFO (to) = SSA_NAME_RANGE_INFO (from);
		  SSA_NAME_ANTI_RANGE_P (to) = SSA_NAME_ANTI_RANGE_P (from);
		}
	      else
		{
		  /* Save old info.  */
		  if (! VN_INFO (to)->info.range_info)
		    {
		      VN_INFO (to)->info.range_info = SSA_NAME_RANGE_INFO (to);
		      VN_INFO (to)->range_info_anti_range_p
			= SSA_NAME_ANTI_RANGE_P (to);
		    }
		  /* Rather than allocating memory and unioning the info
		     just clear it.  */
		  SSA_NAME_RANGE_INFO (to) = NULL;
		}
	    }
	  else if (POINTER_TYPE_P (TREE_TYPE (to))
		   && SSA_NAME_PTR_INFO (to))
	    {
	      if (SSA_NAME_IS_DEFAULT_DEF (to)
		  || dominated_by_p_w_unex
			(gimple_bb (SSA_NAME_DEF_STMT (from)),
			 gimple_bb (SSA_NAME_DEF_STMT (to))))
		/* Keep the info from the dominator.  */
		;
	      else if (SSA_NAME_IS_DEFAULT_DEF (from)
		       || dominated_by_p_w_unex
			    (gimple_bb (SSA_NAME_DEF_STMT (to)),
			     gimple_bb (SSA_NAME_DEF_STMT (from))))
		{
		  /* Save old info.  */
		  if (! VN_INFO (to)->info.ptr_info)
		    VN_INFO (to)->info.ptr_info = SSA_NAME_PTR_INFO (to);
		  /* Use that from the dominator.  */
		  SSA_NAME_PTR_INFO (to) = SSA_NAME_PTR_INFO (from);
		}
	      else if (! SSA_NAME_PTR_INFO (from)
		       /* Handle the case of trivially equivalent info.  */
		       || memcmp (SSA_NAME_PTR_INFO (to),
				  SSA_NAME_PTR_INFO (from),
				  sizeof (ptr_info_def)) != 0)
		{
		  /* Save old info.  */
		  if (! VN_INFO (to)->info.ptr_info)
		    VN_INFO (to)->info.ptr_info = SSA_NAME_PTR_INFO (to);
		  /* Rather than allocating memory and unioning the info
		     just clear it.  */
		  SSA_NAME_PTR_INFO (to) = NULL;
		}
	    }
	}

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
  gimple *stmt = SSA_NAME_DEF_STMT (use);

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
defs_to_varying (gimple *stmt)
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

/* Visit a copy between LHS and RHS, return true if the value number
   changed.  */

static bool
visit_copy (tree lhs, tree rhs)
{
  /* Valueize.  */
  rhs = SSA_VAL (rhs);

  return set_ssa_val_to (lhs, rhs);
}

/* Visit a nary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_nary_op (tree lhs, gimple *stmt)
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
visit_reference_op_call (tree lhs, gcall *stmt)
{
  bool changed = false;
  struct vn_reference_s vr1;
  vn_reference_t vnresult = NULL;
  tree vdef = gimple_vdef (stmt);

  /* Non-ssa lhs is handled in copy_reference_ops_from_call.  */
  if (lhs && TREE_CODE (lhs) != SSA_NAME)
    lhs = NULL_TREE;

  vn_reference_lookup_call (stmt, &vnresult, &vr1);
  if (vnresult)
    {
      if (vnresult->result_vdef && vdef)
	changed |= set_ssa_val_to (vdef, vnresult->result_vdef);

      if (!vnresult->result && lhs)
	vnresult->result = lhs;

      if (vnresult->result && lhs)
	changed |= set_ssa_val_to (lhs, vnresult->result);
    }
  else
    {
      vn_reference_t vr2;
      vn_reference_s **slot;
      if (vdef)
	changed |= set_ssa_val_to (vdef, vdef);
      if (lhs)
	changed |= set_ssa_val_to (lhs, lhs);
      vr2 = current_info->references_pool->allocate ();
      vr2->vuse = vr1.vuse;
      /* As we are not walking the virtual operand chain we know the
	 shared_lookup_references are still original so we can re-use
	 them here.  */
      vr2->operands = vr1.operands.copy ();
      vr2->type = vr1.type;
      vr2->set = vr1.set;
      vr2->hashcode = vr1.hashcode;
      vr2->result = lhs;
      vr2->result_vdef = vdef;
      slot = current_info->references->find_slot_with_hash (vr2, vr2->hashcode,
							    INSERT);
      gcc_assert (!*slot);
      *slot = vr2;
    }

  return changed;
}

/* Visit a load from a reference operator RHS, part of STMT, value number it,
   and return true if the value number of the LHS has changed as a result.  */

static bool
visit_reference_op_load (tree lhs, tree op, gimple *stmt)
{
  bool changed = false;
  tree last_vuse;
  tree result;

  last_vuse = gimple_vuse (stmt);
  last_vuse_ptr = &last_vuse;
  result = vn_reference_lookup (op, gimple_vuse (stmt),
				default_vn_walk_kind, NULL, true);
  last_vuse_ptr = NULL;

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
      code_helper rcode = VIEW_CONVERT_EXPR;
      tree ops[3] = { result };
      result = vn_nary_build_or_lookup (rcode, TREE_TYPE (op), ops);
    }

  if (result)
    changed = set_ssa_val_to (lhs, result);
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
visit_reference_op_store (tree lhs, tree op, gimple *stmt)
{
  bool changed = false;
  vn_reference_t vnresult = NULL;
  tree result, assign;
  bool resultsame = false;
  tree vuse = gimple_vuse (stmt);
  tree vdef = gimple_vdef (stmt);

  if (TREE_CODE (op) == SSA_NAME)
    op = SSA_VAL (op);

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

  result = vn_reference_lookup (lhs, vuse, VN_NOWALK, NULL, false);

  if (result)
    {
      if (TREE_CODE (result) == SSA_NAME)
	result = SSA_VAL (result);
      resultsame = expressions_equal_p (result, op);
    }

  if ((!result || !resultsame)
      /* Only perform the following when being called from PRE
	 which embeds tail merging.  */
      && default_vn_walk_kind == VN_WALK)
    {
      assign = build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, op);
      vn_reference_lookup (assign, vuse, VN_NOWALK, &vnresult, false);
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

      /* Only perform the following when being called from PRE
	 which embeds tail merging.  */
      if (default_vn_walk_kind == VN_WALK)
	{
	  assign = build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, op);
	  vn_reference_insert (assign, lhs, vuse, vdef);
	}
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
visit_phi (gimple *phi)
{
  bool changed = false;
  tree result;
  tree sameval = VN_TOP;
  bool allsame = true;
  unsigned n_executable = 0;

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

	++n_executable;
	if (TREE_CODE (def) == SSA_NAME)
	  def = SSA_VAL (def);
	if (def == VN_TOP)
	  continue;
	if (sameval == VN_TOP)
	  sameval = def;
	else if (!expressions_equal_p (def, sameval))
	  {
	    allsame = false;
	    break;
	  }
      }
  
  /* If none of the edges was executable or all incoming values are
     undefined keep the value-number at VN_TOP.  If only a single edge
     is exectuable use its value.  */
  if (sameval == VN_TOP
      || n_executable == 1)
    return set_ssa_val_to (PHI_RESULT (phi), sameval);

  /* First see if it is equivalent to a phi node in this block.  We prefer
     this as it allows IV elimination - see PRs 66502 and 67167.  */
  result = vn_phi_lookup (phi);
  if (result)
    changed = set_ssa_val_to (PHI_RESULT (phi), result);
  /* Otherwise all value numbered to the same value, the phi node has that
     value.  */
  else if (allsame)
    changed = set_ssa_val_to (PHI_RESULT (phi), sameval);
  else
    {
      vn_phi_insert (phi, PHI_RESULT (phi));
      changed = set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));
    }

  return changed;
}

/* Try to simplify RHS using equivalences and constant folding.  */

static tree
try_to_simplify (gassign *stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree tem;

  /* For stores we can end up simplifying a SSA_NAME rhs.  Just return
     in this case, there is no point in doing extra work.  */
  if (code == SSA_NAME)
    return NULL_TREE;

  /* First try constant folding based on our current lattice.  */
  mprts_hook = vn_lookup_simplify_result;
  tem = gimple_fold_stmt_to_constant_1 (stmt, vn_valueize, vn_valueize);
  mprts_hook = NULL;
  if (tem
      && (TREE_CODE (tem) == SSA_NAME
	  || is_gimple_min_invariant (tem)))
    return tem;

  return NULL_TREE;
}

/* Visit and value number USE, return true if the value number
   changed. */

static bool
visit_use (tree use)
{
  bool changed = false;
  gimple *stmt = SSA_NAME_DEF_STMT (use);

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
  else if (gimple_code (stmt) == GIMPLE_PHI)
    changed = visit_phi (stmt);
  else if (gimple_has_volatile_ops (stmt))
    changed = defs_to_varying (stmt);
  else if (gassign *ass = dyn_cast <gassign *> (stmt))
    {
      enum tree_code code = gimple_assign_rhs_code (ass);
      tree lhs = gimple_assign_lhs (ass);
      tree rhs1 = gimple_assign_rhs1 (ass);
      tree simplified;

      /* Shortcut for copies. Simplifying copies is pointless,
	 since we copy the expression and value they represent.  */
      if (code == SSA_NAME
	  && TREE_CODE (lhs) == SSA_NAME)
	{
	  changed = visit_copy (lhs, rhs1);
	  goto done;
	}
      simplified = try_to_simplify (ass);
      if (simplified)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "RHS ");
	      print_gimple_expr (dump_file, ass, 0, 0);
	      fprintf (dump_file, " simplified to ");
	      print_generic_expr (dump_file, simplified, 0);
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

      if ((TREE_CODE (lhs) == SSA_NAME
	   /* We can substitute SSA_NAMEs that are live over
	      abnormal edges with their constant value.  */
	   && !(gimple_assign_copy_p (ass)
		&& is_gimple_min_invariant (rhs1))
	   && !(simplified
		&& is_gimple_min_invariant (simplified))
	   && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	  /* Stores or copies from SSA_NAMEs that are live over
	     abnormal edges are a problem.  */
	  || (code == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1)))
	changed = defs_to_varying (ass);
      else if (REFERENCE_CLASS_P (lhs)
	       || DECL_P (lhs))
	changed = visit_reference_op_store (lhs, rhs1, ass);
      else if (TREE_CODE (lhs) == SSA_NAME)
	{
	  if ((gimple_assign_copy_p (ass)
	       && is_gimple_min_invariant (rhs1))
	      || (simplified
		  && is_gimple_min_invariant (simplified)))
	    {
	      if (simplified)
		changed = set_ssa_val_to (lhs, simplified);
	      else
		changed = set_ssa_val_to (lhs, rhs1);
	    }
	  else
	    {
	      /* Visit the original statement.  */
	      switch (vn_get_stmt_kind (ass))
		{
		case VN_NARY:
		changed = visit_nary_op (lhs, ass);
		break;
		case VN_REFERENCE:
		changed = visit_reference_op_load (lhs, rhs1, ass);
		break;
		default:
		changed = defs_to_varying (ass);
		break;
		}
	    }
	}
      else
	changed = defs_to_varying (ass);
    }
  else if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
    {
      tree lhs = gimple_call_lhs (call_stmt);
      if (lhs && TREE_CODE (lhs) == SSA_NAME)
	{
	  /* Try constant folding based on our current lattice.  */
	  tree simplified = gimple_fold_stmt_to_constant_1 (call_stmt,
							    vn_valueize);
	  if (simplified)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "call ");
		  print_gimple_expr (dump_file, call_stmt, 0, 0);
		  fprintf (dump_file, " simplified to ");
		  print_generic_expr (dump_file, simplified, 0);
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
	      changed = set_ssa_val_to (lhs, simplified);
	      if (gimple_vdef (call_stmt))
		changed |= set_ssa_val_to (gimple_vdef (call_stmt),
					   SSA_VAL (gimple_vuse (call_stmt)));
	      goto done;
	    }
	  else if (simplified
		   && TREE_CODE (simplified) == SSA_NAME)
	    {
	      changed = visit_copy (lhs, simplified);
	      if (gimple_vdef (call_stmt))
		changed |= set_ssa_val_to (gimple_vdef (call_stmt),
					   SSA_VAL (gimple_vuse (call_stmt)));
	      goto done;
	    }
	  else if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    {
	      changed = defs_to_varying (call_stmt);
	      goto done;
	    }
	}

      if (!gimple_call_internal_p (call_stmt)
	  && (/* Calls to the same function with the same vuse
		 and the same operands do not necessarily return the same
		 value, unless they're pure or const.  */
	      gimple_call_flags (call_stmt) & (ECF_PURE | ECF_CONST)
	      /* If calls have a vdef, subsequent calls won't have
		 the same incoming vuse.  So, if 2 calls with vdef have the
		 same vuse, we know they're not subsequent.
		 We can value number 2 calls to the same function with the
		 same vuse and the same operands which are not subsequent
		 the same, because there is no code in the program that can
		 compare the 2 values...  */
	      || (gimple_vdef (call_stmt)
		  /* ... unless the call returns a pointer which does
		     not alias with anything else.  In which case the
		     information that the values are distinct are encoded
		     in the IL.  */
		  && !(gimple_call_return_flags (call_stmt) & ERF_NOALIAS)
		  /* Only perform the following when being called from PRE
		     which embeds tail merging.  */
		  && default_vn_walk_kind == VN_WALK)))
	changed = visit_reference_op_call (lhs, call_stmt);
      else
	changed = defs_to_varying (call_stmt);
    }
  else
    changed = defs_to_varying (stmt);
 done:
  return changed;
}

/* Compare two operands by reverse postorder index */

static int
compare_ops (const void *pa, const void *pb)
{
  const tree opa = *((const tree *)pa);
  const tree opb = *((const tree *)pb);
  gimple *opstmta = SSA_NAME_DEF_STMT (opa);
  gimple *opstmtb = SSA_NAME_DEF_STMT (opb);
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
  vn_phi_t phi = info->phis_pool->allocate ();
  vn_phi_s **slot;
  memcpy (phi, ophi, sizeof (*phi));
  ophi->phiargs.create (0);
  slot = info->phis->find_slot_with_hash (phi, phi->hashcode, INSERT);
  gcc_assert (!*slot);
  *slot = phi;
}

/* Insert the no longer used reference OREF to the hash INFO.  */

static void
copy_reference (vn_reference_t oref, vn_tables_t info)
{
  vn_reference_t ref;
  vn_reference_s **slot;
  ref = info->references_pool->allocate ();
  memcpy (ref, oref, sizeof (*ref));
  oref->operands.create (0);
  slot = info->references->find_slot_with_hash (ref, ref->hashcode, INSERT);
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
      optimistic_info->nary->empty ();
      optimistic_info->phis->empty ();
      optimistic_info->references->empty ();
      obstack_free (&optimistic_info->nary_obstack, NULL);
      gcc_obstack_init (&optimistic_info->nary_obstack);
      optimistic_info->phis_pool->release ();
      optimistic_info->references_pool->release ();
      FOR_EACH_VEC_ELT (scc, i, var)
	gcc_assert (!VN_INFO (var)->needs_insertion
		    && VN_INFO (var)->expr == NULL);
      FOR_EACH_VEC_ELT (scc, i, var)
	changed |= visit_use (var);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Processing SCC needed %d iterations\n", iterations);
  statistics_histogram_event (cfun, "SCC iterations", iterations);

  /* Finally, copy the contents of the no longer used optimistic
     table to the valid table.  */
  FOR_EACH_HASH_TABLE_ELEMENT (*optimistic_info->nary, nary, vn_nary_op_t, hin)
    copy_nary (nary, valid_info);
  FOR_EACH_HASH_TABLE_ELEMENT (*optimistic_info->phis, phi, vn_phi_t, hip)
    copy_phi (phi, valid_info);
  FOR_EACH_HASH_TABLE_ELEMENT (*optimistic_info->references,
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
  gimple *defstmt;
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
      if (gphi *phi = dyn_cast <gphi *> (defstmt))
	usep = op_iter_init_phiuse (&iter, phi, SSA_OP_ALL_USES);
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
  table->phis = new vn_phi_table_type (23);
  table->nary = new vn_nary_op_table_type (23);
  table->references = new vn_reference_table_type (23);

  gcc_obstack_init (&table->nary_obstack);
  table->phis_pool = new object_allocator<vn_phi_s> ("VN phis");
  table->references_pool = new object_allocator<vn_reference_s>
    ("VN references");
}

/* Free a value number table.  */

static void
free_vn_table (vn_tables_t table)
{
  delete table->phis;
  table->phis = NULL;
  delete table->nary;
  table->nary = NULL;
  delete table->references;
  table->references = NULL;
  obstack_free (&table->nary_obstack, NULL);
  delete table->phis_pool;
  delete table->references_pool;
}

static void
init_scc_vn (void)
{
  size_t i;
  int j;
  int *rpo_numbers_temp;

  calculate_dominance_info (CDI_DOMINATORS);
  mark_dfs_back_edges ();

  sccstack.create (0);
  constant_to_value_id = new hash_table<vn_constant_hasher> (23);

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

  renumber_gimple_stmt_uids ();

  /* Create the valid and optimistic value numbering tables.  */
  valid_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (valid_info);
  optimistic_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (optimistic_info);
  current_info = valid_info;

  /* Create the VN_INFO structures, and initialize value numbers to
     TOP or VARYING for parameters.  */
  for (i = 1; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (!name)
	continue;

      VN_INFO_GET (name)->valnum = VN_TOP;
      VN_INFO (name)->needs_insertion = false;
      VN_INFO (name)->expr = NULL;
      VN_INFO (name)->value_id = 0;

      if (!SSA_NAME_IS_DEFAULT_DEF (name))
	continue;

      switch (TREE_CODE (SSA_NAME_VAR (name)))
	{
	case VAR_DECL:
	  /* Undefined vars keep TOP.  */
	  break;

	case PARM_DECL:
	  /* Parameters are VARYING but we can record a condition
	     if we know it is a non-NULL pointer.  */
	  VN_INFO (name)->visited = true;
	  VN_INFO (name)->valnum = name; 
	  if (POINTER_TYPE_P (TREE_TYPE (name))
	      && nonnull_arg_p (SSA_NAME_VAR (name)))
	    {
	      tree ops[2];
	      ops[0] = name;
	      ops[1] = build_int_cst (TREE_TYPE (name), 0);
	      vn_nary_op_insert_pieces (2, NE_EXPR, boolean_type_node, ops,
					boolean_true_node, 0);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Recording ");
		  print_generic_expr (dump_file, name, TDF_SLIM);
		  fprintf (dump_file, " != 0\n");
		}
	    }
	  break;

	case RESULT_DECL:
	  /* If the result is passed by invisible reference the default
	     def is initialized, otherwise it's uninitialized.  */
	  if (DECL_BY_REFERENCE (SSA_NAME_VAR (name)))
	    {
	      VN_INFO (name)->visited = true;
	      VN_INFO (name)->valnum = name; 
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Restore SSA info that has been reset on value leaders.  */

void
scc_vn_restore_ssa_info (void)
{
  for (unsigned i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name
	  && has_VN_INFO (name))
	{
	  if (VN_INFO (name)->needs_insertion)
	    ;
	  else if (POINTER_TYPE_P (TREE_TYPE (name))
		   && VN_INFO (name)->info.ptr_info)
	    SSA_NAME_PTR_INFO (name) = VN_INFO (name)->info.ptr_info;
	  else if (INTEGRAL_TYPE_P (TREE_TYPE (name))
		   && VN_INFO (name)->info.range_info)
	    {
	      SSA_NAME_RANGE_INFO (name) = VN_INFO (name)->info.range_info;
	      SSA_NAME_ANTI_RANGE_P (name)
		= VN_INFO (name)->range_info_anti_range_p;
	    }
	}
    }
}

void
free_scc_vn (void)
{
  size_t i;

  delete constant_to_value_id;
  constant_to_value_id = NULL;
  BITMAP_FREE (constant_value_ids);
  shared_lookup_phiargs.release ();
  shared_lookup_references.release ();
  XDELETEVEC (rpo_numbers);

  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (name
	  && has_VN_INFO (name)
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

  BITMAP_FREE (const_parms);
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

  FOR_EACH_HASH_TABLE_ELEMENT (*valid_info->nary, vno, vn_nary_op_t, hin)
    set_value_id_for_result (vno->result, &vno->value_id);

  FOR_EACH_HASH_TABLE_ELEMENT (*valid_info->phis, vp, vn_phi_t, hip)
    set_value_id_for_result (vp->result, &vp->value_id);

  FOR_EACH_HASH_TABLE_ELEMENT (*valid_info->references, vr, vn_reference_t,
			       hir)
    set_value_id_for_result (vr->result, &vr->value_id);
}

class sccvn_dom_walker : public dom_walker
{
public:
  sccvn_dom_walker ()
    : dom_walker (CDI_DOMINATORS, true), fail (false), cond_stack (vNULL) {}
  ~sccvn_dom_walker ();

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

  void record_cond (basic_block,
		    enum tree_code code, tree lhs, tree rhs, bool value);
  void record_conds (basic_block,
		     enum tree_code code, tree lhs, tree rhs, bool value);

  bool fail;
  vec<std::pair <basic_block, std::pair <vn_nary_op_t, vn_nary_op_t> > >
    cond_stack;
};

sccvn_dom_walker::~sccvn_dom_walker ()
{
  cond_stack.release ();
}

/* Record a temporary condition for the BB and its dominated blocks.  */

void
sccvn_dom_walker::record_cond (basic_block bb,
			       enum tree_code code, tree lhs, tree rhs,
			       bool value)
{
  tree ops[2] = { lhs, rhs };
  vn_nary_op_t old = NULL;
  if (vn_nary_op_lookup_pieces (2, code, boolean_type_node, ops, &old))
    current_info->nary->remove_elt_with_hash (old, old->hashcode);
  vn_nary_op_t cond
    = vn_nary_op_insert_pieces (2, code, boolean_type_node, ops,
				value
				? boolean_true_node
				: boolean_false_node, 0);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Recording temporarily ");
      print_generic_expr (dump_file, ops[0], TDF_SLIM);
      fprintf (dump_file, " %s ", get_tree_code_name (code));
      print_generic_expr (dump_file, ops[1], TDF_SLIM);
      fprintf (dump_file, " == %s%s\n",
	       value ? "true" : "false",
	       old ? " (old entry saved)" : "");
    }
  cond_stack.safe_push (std::make_pair (bb, std::make_pair (cond, old)));
}

/* Record temporary conditions for the BB and its dominated blocks
   according to LHS CODE RHS == VALUE and its dominated conditions.  */

void
sccvn_dom_walker::record_conds (basic_block bb,
				enum tree_code code, tree lhs, tree rhs,
				bool value)
{
  /* Record the original condition.  */
  record_cond (bb, code, lhs, rhs, value);

  if (!value)
    return;

  /* Record dominated conditions if the condition is true.  Note that
     the inversion is already recorded.  */
  switch (code)
    {
    case LT_EXPR:
    case GT_EXPR:
      record_cond (bb, code == LT_EXPR ? LE_EXPR : GE_EXPR, lhs, rhs, true);
      record_cond (bb, NE_EXPR, lhs, rhs, true);
      record_cond (bb, EQ_EXPR, lhs, rhs, false);
      break;

    case EQ_EXPR:
      record_cond (bb, LE_EXPR, lhs, rhs, true);
      record_cond (bb, GE_EXPR, lhs, rhs, true);
      record_cond (bb, LT_EXPR, lhs, rhs, false);
      record_cond (bb, GT_EXPR, lhs, rhs, false);
      break;

    default:
      break;
    }
}
 
/* Restore expressions and values derived from conditionals.  */

void
sccvn_dom_walker::after_dom_children (basic_block bb)
{
  while (!cond_stack.is_empty ()
	 && cond_stack.last ().first == bb)
    {
      vn_nary_op_t cond = cond_stack.last ().second.first;
      vn_nary_op_t old = cond_stack.last ().second.second;
      current_info->nary->remove_elt_with_hash (cond, cond->hashcode);
      if (old)
	vn_nary_op_insert_into (old, current_info->nary, false);
      cond_stack.pop ();
    }
}

/* Value number all statements in BB.  */

edge
sccvn_dom_walker::before_dom_children (basic_block bb)
{
  edge e;
  edge_iterator ei;

  if (fail)
    return NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Visiting BB %d\n", bb->index);

  /* If we have a single predecessor record the equivalence from a
     possible condition on the predecessor edge.  */
  edge pred_e = NULL;
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* Ignore simple backedges from this to allow recording conditions
         in loop headers.  */
      if (dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
	continue;
      if (! pred_e)
	pred_e = e;
      else
	{
	  pred_e = NULL;
	  break;
	}
    }
  if (pred_e)
    {
      /* Check if there are multiple executable successor edges in
	 the source block.  Otherwise there is no additional info
	 to be recorded.  */
      edge e2;
      FOR_EACH_EDGE (e2, ei, pred_e->src->succs)
	if (e2 != pred_e
	    && e2->flags & EDGE_EXECUTABLE)
	  break;
      if (e2 && (e2->flags & EDGE_EXECUTABLE))
	{
	  gimple *stmt = last_stmt (pred_e->src);
	  if (stmt
	      && gimple_code (stmt) == GIMPLE_COND)
	    {
	      enum tree_code code = gimple_cond_code (stmt);
	      tree lhs = gimple_cond_lhs (stmt);
	      tree rhs = gimple_cond_rhs (stmt);
	      record_conds (bb, code, lhs, rhs,
			    (pred_e->flags & EDGE_TRUE_VALUE) != 0);
	      code = invert_tree_comparison (code, HONOR_NANS (lhs));
	      if (code != ERROR_MARK)
		record_conds (bb, code, lhs, rhs,
			      (pred_e->flags & EDGE_TRUE_VALUE) == 0);
	    }
	}
    }

  /* Value-number all defs in the basic-block.  */
  for (gphi_iterator gsi = gsi_start_phis (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree res = PHI_RESULT (phi);
      if (!VN_INFO (res)->visited
	  && !DFS (res))
	{
	  fail = true;
	  return NULL;
	}
    }
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      ssa_op_iter i;
      tree op;
      FOR_EACH_SSA_TREE_OPERAND (op, gsi_stmt (gsi), i, SSA_OP_ALL_DEFS)
	if (!VN_INFO (op)->visited
	    && !DFS (op))
	  {
	    fail = true;
	    return NULL;
	  }
    }

  /* Finally look at the last stmt.  */
  gimple *stmt = last_stmt (bb);
  if (!stmt)
    return NULL;

  enum gimple_code code = gimple_code (stmt);
  if (code != GIMPLE_COND
      && code != GIMPLE_SWITCH
      && code != GIMPLE_GOTO)
    return NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Visiting control stmt ending BB %d: ", bb->index);
      print_gimple_stmt (dump_file, stmt, 0, 0);
    }

  /* ???  We can even handle stmts with outgoing EH or ABNORMAL edges
     if value-numbering can prove they are not reachable.  Handling
     computed gotos is also possible.  */
  tree val;
  switch (code)
    {
    case GIMPLE_COND:
      {
	tree lhs = vn_valueize (gimple_cond_lhs (stmt));
	tree rhs = vn_valueize (gimple_cond_rhs (stmt));
	val = gimple_simplify (gimple_cond_code (stmt),
			       boolean_type_node, lhs, rhs,
			       NULL, vn_valueize);
	/* If that didn't simplify to a constant see if we have recorded
	   temporary expressions from taken edges.  */
	if (!val || TREE_CODE (val) != INTEGER_CST)
	  {
	    tree ops[2];
	    ops[0] = lhs;
	    ops[1] = rhs;
	    val = vn_nary_op_lookup_pieces (2, gimple_cond_code (stmt),
					    boolean_type_node, ops, NULL);
	  }
	break;
      }
    case GIMPLE_SWITCH:
      val = gimple_switch_index (as_a <gswitch *> (stmt));
      break;
    case GIMPLE_GOTO:
      val = gimple_goto_dest (stmt);
      break;
    default:
      gcc_unreachable ();
    }
  if (!val)
    return NULL;

  edge taken = find_taken_edge (bb, vn_valueize (val));
  if (!taken)
    return NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Marking all edges out of BB %d but (%d -> %d) as "
	     "not executable\n", bb->index, bb->index, taken->dest->index);

  return taken;
}

/* Do SCCVN.  Returns true if it finished, false if we bailed out
   due to resource constraints.  DEFAULT_VN_WALK_KIND_ specifies
   how we use the alias oracle walking during the VN process.  */

bool
run_scc_vn (vn_lookup_kind default_vn_walk_kind_)
{
  size_t i;

  default_vn_walk_kind = default_vn_walk_kind_;

  init_scc_vn ();

  /* Collect pointers we know point to readonly memory.  */
  const_parms = BITMAP_ALLOC (NULL);
  tree fnspec = lookup_attribute ("fn spec",
				  TYPE_ATTRIBUTES (TREE_TYPE (cfun->decl)));
  if (fnspec)
    {
      fnspec = TREE_VALUE (TREE_VALUE (fnspec));
      i = 1;
      for (tree arg = DECL_ARGUMENTS (cfun->decl);
	   arg; arg = DECL_CHAIN (arg), ++i)
	{
	  if (i >= (unsigned) TREE_STRING_LENGTH (fnspec))
	    break;
	  if (TREE_STRING_POINTER (fnspec)[i]  == 'R'
	      || TREE_STRING_POINTER (fnspec)[i] == 'r')
	    {
	      tree name = ssa_default_def (cfun, arg);
	      if (name)
		bitmap_set_bit (const_parms, SSA_NAME_VERSION (name));
	    }
	}
    }

  /* Walk all blocks in dominator order, value-numbering stmts
     SSA defs and decide whether outgoing edges are not executable.  */
  sccvn_dom_walker walker;
  walker.walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  if (walker.fail)
    {
      free_scc_vn ();
      return false;
    }

  /* Initialize the value ids and prune out remaining VN_TOPs
     from dead code.  */
  for (i = 1; i < num_ssa_names; ++i)
    {
      tree name = ssa_name (i);
      vn_ssa_aux_t info;
      if (!name)
	continue;
      info = VN_INFO (name);
      if (!info->visited)
	info->valnum = name;
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

  /* If either one is VN_TOP consider them equal.  */
  if (e1 == VN_TOP || e2 == VN_TOP)
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
