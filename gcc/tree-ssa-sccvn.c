/* SCC value numbering for trees
   Copyright (C) 2006-2018 Free Software Foundation, Inc.
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
#include "ssa.h"
#include "expmed.h"
#include "insn-config.h"
#include "memmodel.h"
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
#include "tree-cfg.h"
#include "domwalk.h"
#include "gimple-iterator.h"
#include "gimple-match.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-pass.h"
#include "statistics.h"
#include "langhooks.h"
#include "ipa-utils.h"
#include "dbgcnt.h"
#include "tree-cfgcleanup.h"
#include "tree-ssa-loop.h"
#include "tree-scalar-evolution.h"
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

/* There's no BB_EXECUTABLE but we can use BB_VISITED.  */
#define BB_EXECUTABLE BB_VISITED

static tree *last_vuse_ptr;
static vn_lookup_kind vn_walk_kind;
static vn_lookup_kind default_vn_walk_kind;

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
  return vno1 == vno2 || vn_nary_op_eq (vno1, vno2);
}

typedef hash_table<vn_nary_op_hasher> vn_nary_op_table_type;
typedef vn_nary_op_table_type::iterator vn_nary_op_iterator_type;


/* vn_phi hashtable helpers.  */

static int
vn_phi_eq (const_vn_phi_t const vp1, const_vn_phi_t const vp2);

struct vn_phi_hasher : nofree_ptr_hash <vn_phi_s>
{ 
  static inline hashval_t hash (const vn_phi_s *);
  static inline bool equal (const vn_phi_s *, const vn_phi_s *);
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
  return vp1 == vp2 || vn_phi_eq (vp1, vp2);
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

struct vn_reference_hasher : nofree_ptr_hash <vn_reference_s>
{
  static inline hashval_t hash (const vn_reference_s *);
  static inline bool equal (const vn_reference_s *, const vn_reference_s *);
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
  return v == c || vn_reference_eq (v, c);
}

typedef hash_table<vn_reference_hasher> vn_reference_table_type;
typedef vn_reference_table_type::iterator vn_reference_iterator_type;


/* The set of VN hashtables.  */

typedef struct vn_tables_s
{
  vn_nary_op_table_type *nary;
  vn_phi_table_type *phis;
  vn_reference_table_type *references;
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


/* Obstack we allocate the vn-tables elements from.  */
static obstack vn_tables_obstack;
/* Special obstack we never unwind.  */
static obstack vn_tables_insert_obstack;

static vn_reference_t last_inserted_ref;
static vn_phi_t last_inserted_phi;
static vn_nary_op_t last_inserted_nary;

/* Valid hashtables storing information we have proven to be
   correct.  */
static vn_tables_t valid_info;


/* Valueization hook.  Valueize NAME if it is an SSA name, otherwise
   just return it.  */
tree (*vn_valueize) (tree);


/* This represents the top of the VN lattice, which is the universal
   value.  */

tree VN_TOP;

/* Unique counter for our value ids.  */

static unsigned int next_value_id;


/* Table of vn_ssa_aux_t's, one per ssa_name.  The vn_ssa_aux_t objects
   are allocated on an obstack for locality reasons, and to free them
   without looping over the vec.  */

struct vn_ssa_aux_hasher : typed_noop_remove <vn_ssa_aux_t>
{
  typedef vn_ssa_aux_t value_type;
  typedef tree compare_type;
  static inline hashval_t hash (const value_type &);
  static inline bool equal (const value_type &, const compare_type &);
  static inline void mark_deleted (value_type &) {}
  static inline void mark_empty (value_type &e) { e = NULL; }
  static inline bool is_deleted (value_type &) { return false; }
  static inline bool is_empty (value_type &e) { return e == NULL; }
};

hashval_t
vn_ssa_aux_hasher::hash (const value_type &entry)
{
  return SSA_NAME_VERSION (entry->name);
}

bool
vn_ssa_aux_hasher::equal (const value_type &entry, const compare_type &name)
{
  return name == entry->name;
}

static hash_table<vn_ssa_aux_hasher> *vn_ssa_aux_hash;
typedef hash_table<vn_ssa_aux_hasher>::iterator vn_ssa_aux_iterator_type;
static struct obstack vn_ssa_aux_obstack;

static vn_nary_op_t vn_nary_op_insert_stmt (gimple *, tree);
static unsigned int vn_nary_length_from_stmt (gimple *);
static vn_nary_op_t alloc_vn_nary_op_noinit (unsigned int, obstack *);
static vn_nary_op_t vn_nary_op_insert_into (vn_nary_op_t,
					    vn_nary_op_table_type *, bool);
static void init_vn_nary_op_from_stmt (vn_nary_op_t, gimple *);
static void init_vn_nary_op_from_pieces (vn_nary_op_t, unsigned int,
					 enum tree_code, tree, tree *);
static tree vn_lookup_simplify_result (gimple_match_op *);

/* Return whether there is value numbering information for a given SSA name.  */

bool
has_VN_INFO (tree name)
{
  return vn_ssa_aux_hash->find_with_hash (name, SSA_NAME_VERSION (name));
}

vn_ssa_aux_t
VN_INFO (tree name)
{
  vn_ssa_aux_t *res
    = vn_ssa_aux_hash->find_slot_with_hash (name, SSA_NAME_VERSION (name),
					    INSERT);
  if (*res != NULL)
    return *res;

  vn_ssa_aux_t newinfo = *res = XOBNEW (&vn_ssa_aux_obstack, struct vn_ssa_aux);
  memset (newinfo, 0, sizeof (struct vn_ssa_aux));
  newinfo->name = name;
  newinfo->valnum = VN_TOP;
  /* We are using the visited flag to handle uses with defs not within the
     region being value-numbered.  */
  newinfo->visited = false;

  /* Given we create the VN_INFOs on-demand now we have to do initialization
     different than VN_TOP here.  */
  if (SSA_NAME_IS_DEFAULT_DEF (name))
    switch (TREE_CODE (SSA_NAME_VAR (name)))
      {
      case VAR_DECL:
        /* All undefined vars are VARYING.  */
        newinfo->valnum = name;
	newinfo->visited = true;
	break;

      case PARM_DECL:
	/* Parameters are VARYING but we can record a condition
	   if we know it is a non-NULL pointer.  */
	newinfo->visited = true;
	newinfo->valnum = name;
	if (POINTER_TYPE_P (TREE_TYPE (name))
	    && nonnull_arg_p (SSA_NAME_VAR (name)))
	  {
	    tree ops[2];
	    ops[0] = name;
	    ops[1] = build_int_cst (TREE_TYPE (name), 0);
	    vn_nary_op_t nary;
	    /* Allocate from non-unwinding stack.  */
	    nary = alloc_vn_nary_op_noinit (2, &vn_tables_insert_obstack);
	    init_vn_nary_op_from_pieces (nary, 2, NE_EXPR,
					 boolean_type_node, ops);
	    nary->predicated_values = 0;
	    nary->u.result = boolean_true_node;
	    vn_nary_op_insert_into (nary, valid_info->nary, true);
	    gcc_assert (nary->unwind_to == NULL);
	    /* Also do not link it into the undo chain.  */
	    last_inserted_nary = nary->next;
	    nary->next = (vn_nary_op_t)(void *)-1;
	    nary = alloc_vn_nary_op_noinit (2, &vn_tables_insert_obstack);
	    init_vn_nary_op_from_pieces (nary, 2, EQ_EXPR,
					 boolean_type_node, ops);
	    nary->predicated_values = 0;
	    nary->u.result = boolean_false_node;
	    vn_nary_op_insert_into (nary, valid_info->nary, true);
	    gcc_assert (nary->unwind_to == NULL);
	    last_inserted_nary = nary->next;
	    nary->next = (vn_nary_op_t)(void *)-1;
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
	   def is initialized, otherwise it's uninitialized.  Still
	   undefined is varying.  */
	newinfo->visited = true;
	newinfo->valnum = name;
	break;

      default:
	gcc_unreachable ();
      }
  return newinfo;
}

/* Return the SSA value of X.  */

inline tree
SSA_VAL (tree x, bool *visited = NULL)
{
  vn_ssa_aux_t tem = vn_ssa_aux_hash->find_with_hash (x, SSA_NAME_VERSION (x));
  if (visited)
    *visited = tem && tem->visited;
  return tem && tem->visited ? tem->valnum : x;
}

/* Return whether X was visited.  */

inline bool
SSA_VISITED (tree x)
{
  vn_ssa_aux_t tem = vn_ssa_aux_hash->find_with_hash (x, SSA_NAME_VERSION (x));
  return tem && tem->visited;
}

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
      gcc_assert (x != VN_TOP);
    }
  while (SSA_NAME_IN_FREE_LIST (x));

  return x;
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

  /* If the hashtable isn't initialized we're not running from PRE and thus
     do not need value-ids.  */
  if (!constant_to_value_id)
    return 0;

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
  poly_int64 off = -1;
  bool deref = false;

  FOR_EACH_VEC_ELT (vr1->operands, i, vro)
    {
      if (vro->opcode == MEM_REF)
	deref = true;
      else if (vro->opcode != ADDR_EXPR)
	deref = false;
      if (maybe_ne (vro->off, -1))
	{
	  if (known_eq (off, -1))
	    off = 0;
	  off += vro->off;
	}
      else
	{
	  if (maybe_ne (off, -1)
	      && maybe_ne (off, 0))
	    hstate.add_poly_int (off);
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
      poly_int64 off1 = 0, off2 = 0;
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
	  if (known_eq (vro1->off, -1))
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
	  if (known_eq (vro2->off, -1))
	    break;
	  off2 += vro2->off;
	}
      if (maybe_ne (off1, off2))
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
	  if (!mem_ref_offset (ref).to_shwi (&temp.off))
	    temp.off = -1;
	  temp.clique = MR_DEPENDENCE_CLIQUE (ref);
	  temp.base = MR_DEPENDENCE_BASE (ref);
	  temp.reverse = REF_REVERSE_STORAGE_ORDER (ref);
	  break;
	case BIT_FIELD_REF:
	  /* Record bits, position and storage order.  */
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  if (!multiple_p (bit_field_offset (ref), BITS_PER_UNIT, &temp.off))
	    temp.off = -1;
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
		&& poly_int_tree_p (this_offset))
	      {
		tree bit_offset = DECL_FIELD_BIT_OFFSET (TREE_OPERAND (ref, 1));
		if (TREE_INT_CST_LOW (bit_offset) % BITS_PER_UNIT == 0)
		  {
		    poly_offset_int off
		      = (wi::to_poly_offset (this_offset)
			 + (wi::to_offset (bit_offset) >> LOG2_BITS_PER_UNIT));
		    /* Probibit value-numbering zero offset components
		       of addresses the same before the pass folding
		       __builtin_object_size had a chance to run
		       (checking cfun->after_inlining does the
		       trick here).  */
		    if (TREE_CODE (orig) != ADDR_EXPR
			|| maybe_ne (off, 0)
			|| cfun->after_inlining)
		      off.to_shwi (&temp.off);
		  }
	      }
	  }
	  break;
	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  {
	    tree eltype = TREE_TYPE (TREE_TYPE (TREE_OPERAND (ref, 0)));
	    /* Record index as operand.  */
	    temp.op0 = TREE_OPERAND (ref, 1);
	    /* Always record lower bounds and element size.  */
	    temp.op1 = array_ref_low_bound (ref);
	    /* But record element size in units of the type alignment.  */
	    temp.op2 = TREE_OPERAND (ref, 3);
	    temp.align = eltype->type_common.align;
	    if (! temp.op2)
	      temp.op2 = size_binop (EXACT_DIV_EXPR, TYPE_SIZE_UNIT (eltype),
				     size_int (TYPE_ALIGN_UNIT (eltype)));
	    if (poly_int_tree_p (temp.op0)
		&& poly_int_tree_p (temp.op1)
		&& TREE_CODE (temp.op2) == INTEGER_CST)
	      {
		poly_offset_int off = ((wi::to_poly_offset (temp.op0)
					- wi::to_poly_offset (temp.op1))
				       * wi::to_offset (temp.op2)
				       * vn_ref_op_align_unit (&temp));
		off.to_shwi (&temp.off);
	      }
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
  poly_offset_int offset = 0;
  poly_offset_int max_size;
  poly_offset_int size = -1;
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
	size = GET_MODE_BITSIZE (mode);
    }
  if (size_tree != NULL_TREE
      && poly_int_tree_p (size_tree))
    size = wi::to_poly_offset (size_tree);

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
	      if (known_eq (pop->off, -1))
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
	  offset += wi::to_poly_offset (op->op1);
	  break;

	case COMPONENT_REF:
	  {
	    tree field = op->op0;
	    /* We do not have a complete COMPONENT_REF tree here so we
	       cannot use component_ref_field_offset.  Do the interesting
	       parts manually.  */
	    tree this_offset = DECL_FIELD_OFFSET (field);

	    if (op->op1 || !poly_int_tree_p (this_offset))
	      max_size = -1;
	    else
	      {
		poly_offset_int woffset = (wi::to_poly_offset (this_offset)
					   << LOG2_BITS_PER_UNIT);
		woffset += wi::to_offset (DECL_FIELD_BIT_OFFSET (field));
		offset += woffset;
	      }
	    break;
	  }

	case ARRAY_RANGE_REF:
	case ARRAY_REF:
	  /* We recorded the lower bound and the element size.  */
	  if (!poly_int_tree_p (op->op0)
	      || !poly_int_tree_p (op->op1)
	      || TREE_CODE (op->op2) != INTEGER_CST)
	    max_size = -1;
	  else
	    {
	      poly_offset_int woffset
		= wi::sext (wi::to_poly_offset (op->op0)
			    - wi::to_poly_offset (op->op1),
			    TYPE_PRECISION (TREE_TYPE (op->op0)));
	      woffset *= wi::to_offset (op->op2) * vn_ref_op_align_unit (op);
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

  if (!size.to_shwi (&ref->size) || maybe_lt (ref->size, 0))
    {
      ref->offset = 0;
      ref->size = -1;
      ref->max_size = -1;
      return true;
    }

  if (!offset.to_shwi (&ref->offset))
    {
      ref->offset = 0;
      ref->max_size = -1;
      return true;
    }

  if (!max_size.to_shwi (&ref->max_size) || maybe_lt (ref->max_size, 0))
    ref->max_size = -1;

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
  poly_int64 addr_offset = 0;

  /* The only thing we have to do is from &OBJ.foo.bar add the offset
     from .foo.bar to the preceding MEM_REF offset and replace the
     address with &OBJ.  */
  addr_base = get_addr_base_and_unit_offset (TREE_OPERAND (op->op0, 0),
					     &addr_offset);
  gcc_checking_assert (addr_base && TREE_CODE (addr_base) != MEM_REF);
  if (addr_base != TREE_OPERAND (op->op0, 0))
    {
      poly_offset_int off
	= (poly_offset_int::from (wi::to_poly_wide (mem_op->op0),
				  SIGNED)
	   + addr_offset);
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
  poly_offset_int off;

  def_stmt = SSA_NAME_DEF_STMT (op->op0);
  if (!is_gimple_assign (def_stmt))
    return false;

  code = gimple_assign_rhs_code (def_stmt);
  if (code != ADDR_EXPR
      && code != POINTER_PLUS_EXPR)
    return false;

  off = poly_offset_int::from (wi::to_poly_wide (mem_op->op0), SIGNED);

  /* The only thing we have to do is from &OBJ.foo.bar add the offset
     from .foo.bar to the preceding MEM_REF offset and replace the
     address with &OBJ.  */
  if (code == ADDR_EXPR)
    {
      tree addr, addr_base;
      poly_int64 addr_offset;

      addr = gimple_assign_rhs1 (def_stmt);
      addr_base = get_addr_base_and_unit_offset (TREE_OPERAND (addr, 0),
						 &addr_offset);
      /* If that didn't work because the address isn't invariant propagate
         the reference tree from the address operation in case the current
	 dereference isn't offsetted.  */
      if (!addr_base
	  && *i_p == ops->length () - 1
	  && known_eq (off, 0)
	  /* This makes us disable this transform for PRE where the
	     reference ops might be also used for code insertion which
	     is invalid.  */
	  && default_vn_walk_kind == VN_WALKREWRITE)
	{
	  auto_vec<vn_reference_op_s, 32> tem;
	  copy_reference_ops_from_ref (TREE_OPERAND (addr, 0), &tem);
	  /* Make sure to preserve TBAA info.  The only objects not
	     wrapped in MEM_REFs that can have their address taken are
	     STRING_CSTs.  */
	  if (tem.length () >= 2
	      && tem[tem.length () - 2].opcode == MEM_REF)
	    {
	      vn_reference_op_t new_mem_op = &tem[tem.length () - 2];
	      new_mem_op->op0
		= wide_int_to_tree (TREE_TYPE (mem_op->op0),
				    wi::to_poly_wide (new_mem_op->op0));
	    }
	  else
	    gcc_assert (tem.last ().opcode == STRING_CST);
	  ops->pop ();
	  ops->pop ();
	  ops->safe_splice (tem);
	  --*i_p;
	  return true;
	}
      if (!addr_base
	  || TREE_CODE (addr_base) != MEM_REF
	  || (TREE_CODE (TREE_OPERAND (addr_base, 0)) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND (addr_base, 0))))
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
	  || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (ptr)
	  /* Make sure to not endlessly recurse.
	     See gcc.dg/tree-ssa/20040408-1.c for an example.  Can easily
	     happen when we value-number a PHI to its backedge value.  */
	  || SSA_VAL (ptr) == op->op0
	  || !poly_int_tree_p (ptroff))
	return false;

      off += wi::to_poly_offset (ptroff);
      op->op0 = ptr;
    }

  mem_op->op0 = wide_int_to_tree (TREE_TYPE (mem_op->op0), off);
  if (tree_fits_shwi_p (mem_op->op0))
    mem_op->off = tree_to_shwi (mem_op->op0);
  else
    mem_op->off = -1;
  /* ???  Can end up with endless recursion here!?
     gcc.c-torture/execute/strcmp-1.c  */
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
      && fndecl_built_in_p (TREE_OPERAND (op->op0, 0))
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
	   && COMPLETE_TYPE_P (ref->type)
	   && is_gimple_reg_type (ref->type))
    {
      poly_int64 off = 0;
      HOST_WIDE_INT size;
      if (INTEGRAL_TYPE_P (ref->type))
	size = TYPE_PRECISION (ref->type);
      else if (tree_fits_shwi_p (TYPE_SIZE (ref->type)))
	size = tree_to_shwi (TYPE_SIZE (ref->type));
      else
	return NULL_TREE;
      if (size % BITS_PER_UNIT != 0
	  || size > MAX_BITSIZE_MODE_ANY_MODE)
	return NULL_TREE;
      size /= BITS_PER_UNIT;
      unsigned i;
      for (i = 0; i < operands.length (); ++i)
	{
	  if (TREE_CODE_CLASS (operands[i].opcode) == tcc_constant)
	    {
	      ++i;
	      break;
	    }
	  if (known_eq (operands[i].off, -1))
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
		   || TREE_CODE (TREE_OPERAND (base[1].op0, 0)) == CONST_DECL
		   || TREE_CODE (TREE_OPERAND (base[1].op0, 0)) == STRING_CST))
	{
	  decl = TREE_OPERAND (base[1].op0, 0);
	  if (TREE_CODE (decl) == STRING_CST)
	    ctor = decl;
	  else
	    ctor = ctor_for_folding (decl);
	}
      if (ctor == NULL_TREE)
	return build_zero_cst (ref->type);
      else if (ctor != error_mark_node)
	{
	  HOST_WIDE_INT const_off;
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
	  else if (off.is_constant (&const_off))
	    {
	      unsigned char buf[MAX_BITSIZE_MODE_ANY_MODE / BITS_PER_UNIT];
	      int len = native_encode_expr (ctor, buf, size, const_off);
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
valueize_refs_1 (vec<vn_reference_op_s> orig, bool *valueized_anything,
		 bool with_avail = false)
{
  vn_reference_op_t vro;
  unsigned int i;

  *valueized_anything = false;

  FOR_EACH_VEC_ELT (orig, i, vro)
    {
      if (vro->opcode == SSA_NAME
	  || (vro->op0 && TREE_CODE (vro->op0) == SSA_NAME))
	{
	  tree tem = with_avail ? vn_valueize (vro->op0) : SSA_VAL (vro->op0);
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
	  tree tem = with_avail ? vn_valueize (vro->op1) : SSA_VAL (vro->op1);
	  if (tem != vro->op1)
	    {
	      *valueized_anything = true;
	      vro->op1 = tem;
	    }
	}
      if (vro->op2 && TREE_CODE (vro->op2) == SSA_NAME)
	{
	  tree tem = with_avail ? vn_valueize (vro->op2) : SSA_VAL (vro->op2);
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
	       && known_eq (vro->off, -1)
	       && poly_int_tree_p (vro->op0)
	       && poly_int_tree_p (vro->op1)
	       && TREE_CODE (vro->op2) == INTEGER_CST)
	{
	  poly_offset_int off = ((wi::to_poly_offset (vro->op0)
				  - wi::to_poly_offset (vro->op1))
				 * wi::to_offset (vro->op2)
				 * vn_ref_op_align_unit (vro));
	  off.to_shwi (&vro->off);
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
  vr1.vuse = vuse ? SSA_VAL (vuse) : NULL_TREE;
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

/* Return a value-number for RCODE OPS... either by looking up an existing
   value-number for the simplified result or by inserting the operation if
   INSERT is true.  */

static tree
vn_nary_build_or_lookup_1 (gimple_match_op *res_op, bool insert)
{
  tree result = NULL_TREE;
  /* We will be creating a value number for
       RCODE (OPS...).
     So first simplify and lookup this expression to see if it
     is already available.  */
  mprts_hook = vn_lookup_simplify_result;
  bool res = false;
  switch (TREE_CODE_LENGTH ((tree_code) res_op->code))
    {
    case 1:
      res = gimple_resimplify1 (NULL, res_op, vn_valueize);
      break;
    case 2:
      res = gimple_resimplify2 (NULL, res_op, vn_valueize);
      break;
    case 3:
      res = gimple_resimplify3 (NULL, res_op, vn_valueize);
      break;
    }
  mprts_hook = NULL;
  gimple *new_stmt = NULL;
  if (res
      && gimple_simplified_result_is_gimple_val (res_op))
    {
      /* The expression is already available.  */
      result = res_op->ops[0];
      /* Valueize it, simplification returns sth in AVAIL only.  */
      if (TREE_CODE (result) == SSA_NAME)
	result = SSA_VAL (result);
    }
  else
    {
      tree val = vn_lookup_simplify_result (res_op);
      if (!val && insert)
	{
	  gimple_seq stmts = NULL;
	  result = maybe_push_res_to_seq (res_op, &stmts);
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
      vn_ssa_aux_t result_info = VN_INFO (result);
      result_info->valnum = result;
      result_info->value_id = get_next_value_id ();
      result_info->visited = 1;
      gimple_seq_add_stmt_without_update (&VN_INFO (result)->expr,
					  new_stmt);
      result_info->needs_insertion = true;
      /* ???  PRE phi-translation inserts NARYs without corresponding
         SSA name result.  Re-use those but set their result according
	 to the stmt we just built.  */
      vn_nary_op_t nary = NULL;
      vn_nary_op_lookup_stmt (new_stmt, &nary);
      if (nary)
	{
	  gcc_assert (! nary->predicated_values && nary->u.result == NULL_TREE);
	  nary->u.result = gimple_assign_lhs (new_stmt);
	}
      /* As all "inserted" statements are singleton SCCs, insert
	 to the valid table.  This is strictly needed to
	 avoid re-generating new value SSA_NAMEs for the same
	 expression during SCC iteration over and over (the
	 optimistic table gets cleared after each iteration).
	 We do not need to insert into the optimistic table, as
	 lookups there will fall back to the valid table.  */
      else
	{
	  unsigned int length = vn_nary_length_from_stmt (new_stmt);
	  vn_nary_op_t vno1
	    = alloc_vn_nary_op_noinit (length, &vn_tables_insert_obstack);
	  vno1->value_id = result_info->value_id;
	  vno1->length = length;
	  vno1->predicated_values = 0;
	  vno1->u.result = result;
	  init_vn_nary_op_from_stmt (vno1, new_stmt);
	  vn_nary_op_insert_into (vno1, valid_info->nary, true);
	  /* Also do not link it into the undo chain.  */
	  last_inserted_nary = vno1->next;
	  vno1->next = (vn_nary_op_t)(void *)-1;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Inserting name ");
	  print_generic_expr (dump_file, result);
	  fprintf (dump_file, " for expression ");
	  print_gimple_expr (dump_file, new_stmt, 0, TDF_SLIM);
	  fprintf (dump_file, "\n");
	}
    }
  return result;
}

/* Return a value-number for RCODE OPS... either by looking up an existing
   value-number for the simplified result or by inserting the operation.  */

static tree
vn_nary_build_or_lookup (gimple_match_op *res_op)
{
  return vn_nary_build_or_lookup_1 (res_op, true);
}

/* Try to simplify the expression RCODE OPS... of type TYPE and return
   its value if present.  */

tree
vn_nary_simplify (vn_nary_op_t nary)
{
  if (nary->length > gimple_match_op::MAX_NUM_OPS)
    return NULL_TREE;
  gimple_match_op op (gimple_match_cond::UNCOND, nary->opcode,
		      nary->type, nary->length);
  memcpy (op.ops, nary->op, sizeof (tree) * nary->length);
  return vn_nary_build_or_lookup_1 (&op, false);
}

basic_block vn_context_bb;

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
  HOST_WIDE_INT offseti, maxsizei;
  static vec<vn_reference_op_s> lhs_ops;
  ao_ref lhs_ref;
  bool lhs_ref_ok = false;
  poly_int64 copy_size;

  /* First try to disambiguate after value-replacing in the definitions LHS.  */
  if (is_gimple_assign (def_stmt))
    {
      tree lhs = gimple_assign_lhs (def_stmt);
      bool valueized_anything = false;
      /* Avoid re-allocation overhead.  */
      lhs_ops.truncate (0);
      basic_block saved_rpo_bb = vn_context_bb;
      vn_context_bb = gimple_bb (def_stmt);
      copy_reference_ops_from_ref (lhs, &lhs_ops);
      lhs_ops = valueize_refs_1 (lhs_ops, &valueized_anything, true);
      vn_context_bb = saved_rpo_bb;
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

      /* If we reach a clobbering statement try to skip it and see if
         we find a VN result with exactly the same value as the
	 possible clobber.  In this case we can ignore the clobber
	 and return the found value.
	 Note that we don't need to worry about partial overlapping
	 accesses as we then can use TBAA to disambiguate against the
	 clobbering statement when looking up a load (thus the
	 VN_WALKREWRITE guard).  */
      if (vn_walk_kind == VN_WALKREWRITE
	  && is_gimple_reg_type (TREE_TYPE (lhs))
	  && types_compatible_p (TREE_TYPE (lhs), vr->type))
	{
	  tree *saved_last_vuse_ptr = last_vuse_ptr;
	  /* Do not update last_vuse_ptr in vn_reference_lookup_2.  */
	  last_vuse_ptr = NULL;
	  tree saved_vuse = vr->vuse;
	  hashval_t saved_hashcode = vr->hashcode;
	  void *res = vn_reference_lookup_2 (ref,
					     gimple_vuse (def_stmt), 0, vr);
	  /* Need to restore vr->vuse and vr->hashcode.  */
	  vr->vuse = saved_vuse;
	  vr->hashcode = saved_hashcode;
	  last_vuse_ptr = saved_last_vuse_ptr;
	  if (res && res != (void *)-1)
	    {
	      vn_reference_t vnresult = (vn_reference_t) res;
	      if (vnresult->result
		  && operand_equal_p (vnresult->result,
				      gimple_assign_rhs1 (def_stmt), 0))
		return res;
	    }
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
	  tree val = vn_valueize (oldargs[i]);
	  if (val != oldargs[i])
	    {
	      gimple_call_set_arg (def_stmt, i, val);
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

  /* If we cannot constrain the size of the reference we cannot
     test if anything kills it.  */
  if (!ref->max_size_known_p ())
    return (void *)-1;

  poly_int64 offset = ref->offset;
  poly_int64 maxsize = ref->max_size;

  /* We can't deduce anything useful from clobbers.  */
  if (gimple_clobber_p (def_stmt))
    return (void *)-1;

  /* def_stmt may-defs *ref.  See if we can derive a value for *ref
     from that definition.
     1) Memset.  */
  if (is_gimple_reg_type (vr->type)
      && gimple_call_builtin_p (def_stmt, BUILT_IN_MEMSET)
      && (integer_zerop (gimple_call_arg (def_stmt, 1))
	  || ((TREE_CODE (gimple_call_arg (def_stmt, 1)) == INTEGER_CST
	       || (INTEGRAL_TYPE_P (vr->type) && known_eq (ref->size, 8)))
	      && CHAR_BIT == 8 && BITS_PER_UNIT == 8
	      && offset.is_constant (&offseti)
	      && offseti % BITS_PER_UNIT == 0))
      && poly_int_tree_p (gimple_call_arg (def_stmt, 2))
      && (TREE_CODE (gimple_call_arg (def_stmt, 0)) == ADDR_EXPR
	  || TREE_CODE (gimple_call_arg (def_stmt, 0)) == SSA_NAME))
    {
      tree base2;
      poly_int64 offset2, size2, maxsize2;
      bool reverse;
      tree ref2 = gimple_call_arg (def_stmt, 0);
      if (TREE_CODE (ref2) == SSA_NAME)
	{
	  ref2 = SSA_VAL (ref2);
	  if (TREE_CODE (ref2) == SSA_NAME
	      && (TREE_CODE (base) != MEM_REF
		  || TREE_OPERAND (base, 0) != ref2))
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (ref2);
	      if (gimple_assign_single_p (def_stmt)
		  && gimple_assign_rhs_code (def_stmt) == ADDR_EXPR)
		ref2 = gimple_assign_rhs1 (def_stmt);
	    }
	}
      if (TREE_CODE (ref2) == ADDR_EXPR)
	{
	  ref2 = TREE_OPERAND (ref2, 0);
	  base2 = get_ref_base_and_extent (ref2, &offset2, &size2, &maxsize2,
					   &reverse);
	  if (!known_size_p (maxsize2)
	      || !known_eq (maxsize2, size2)
	      || !operand_equal_p (base, base2, OEP_ADDRESS_OF))
	    return (void *)-1;
	}
      else if (TREE_CODE (ref2) == SSA_NAME)
	{
	  poly_int64 soff;
	  if (TREE_CODE (base) != MEM_REF
	      || !(mem_ref_offset (base) << LOG2_BITS_PER_UNIT).to_shwi (&soff))
	    return (void *)-1;
	  offset += soff;
	  offset2 = 0;
	  if (TREE_OPERAND (base, 0) != ref2)
	    {
	      gimple *def = SSA_NAME_DEF_STMT (ref2);
	      if (is_gimple_assign (def)
		  && gimple_assign_rhs_code (def) == POINTER_PLUS_EXPR
		  && gimple_assign_rhs1 (def) == TREE_OPERAND (base, 0)
		  && poly_int_tree_p (gimple_assign_rhs2 (def))
		  && (wi::to_poly_offset (gimple_assign_rhs2 (def))
		      << LOG2_BITS_PER_UNIT).to_shwi (&offset2))
		{
		  ref2 = gimple_assign_rhs1 (def);
		  if (TREE_CODE (ref2) == SSA_NAME)
		    ref2 = SSA_VAL (ref2);
		}
	      else
		return (void *)-1;
	    }
	}
      else
	return (void *)-1;
      tree len = gimple_call_arg (def_stmt, 2);
      if (known_subrange_p (offset, maxsize, offset2,
			    wi::to_poly_offset (len) << LOG2_BITS_PER_UNIT))
	{
	  tree val;
	  if (integer_zerop (gimple_call_arg (def_stmt, 1)))
	    val = build_zero_cst (vr->type);
	  else if (INTEGRAL_TYPE_P (vr->type)
		   && known_eq (ref->size, 8))
	    {
	      gimple_match_op res_op (gimple_match_cond::UNCOND, NOP_EXPR,
				      vr->type, gimple_call_arg (def_stmt, 1));
	      val = vn_nary_build_or_lookup (&res_op);
	      if (!val
		  || (TREE_CODE (val) == SSA_NAME
		      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val)))
		return (void *)-1;
	    }
	  else
	    {
	      unsigned len = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (vr->type));
	      unsigned char *buf = XALLOCAVEC (unsigned char, len);
	      memset (buf, TREE_INT_CST_LOW (gimple_call_arg (def_stmt, 1)),
		      len);
	      val = native_interpret_expr (vr->type, buf, len);
	      if (!val)
		return (void *)-1;
	    }
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
      poly_int64 offset2, size2, maxsize2;
      bool reverse;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2, &reverse);
      if (known_size_p (maxsize2)
	  && operand_equal_p (base, base2, 0)
	  && known_subrange_p (offset, maxsize, offset2, size2))
	{
	  tree val = build_zero_cst (vr->type);
	  return vn_reference_lookup_or_insert_for_pieces
	           (vuse, vr->set, vr->type, vr->operands, val);
	}
    }

  /* 3) Assignment from a constant.  We can use folds native encode/interpret
     routines to extract the assigned bits.  */
  else if (known_eq (ref->size, maxsize)
	   && is_gimple_reg_type (vr->type)
	   && !contains_storage_order_barrier_p (vr->operands)
	   && gimple_assign_single_p (def_stmt)
	   && CHAR_BIT == 8 && BITS_PER_UNIT == 8
	   /* native_encode and native_decode operate on arrays of bytes
	      and so fundamentally need a compile-time size and offset.  */
	   && maxsize.is_constant (&maxsizei)
	   && maxsizei % BITS_PER_UNIT == 0
	   && offset.is_constant (&offseti)
	   && offseti % BITS_PER_UNIT == 0
	   && (is_gimple_min_invariant (gimple_assign_rhs1 (def_stmt))
	       || (TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
		   && is_gimple_min_invariant (SSA_VAL (gimple_assign_rhs1 (def_stmt))))))
    {
      tree base2;
      HOST_WIDE_INT offset2, size2;
      bool reverse;
      base2 = get_ref_base_and_extent_hwi (gimple_assign_lhs (def_stmt),
					   &offset2, &size2, &reverse);
      if (base2
	  && !reverse
	  && size2 % BITS_PER_UNIT == 0
	  && offset2 % BITS_PER_UNIT == 0
	  && operand_equal_p (base, base2, 0)
	  && known_subrange_p (offseti, maxsizei, offset2, size2))
	{
	  /* We support up to 512-bit values (for V8DFmode).  */
	  unsigned char buffer[64];
	  int len;

	  tree rhs = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (rhs) == SSA_NAME)
	    rhs = SSA_VAL (rhs);
	  len = native_encode_expr (gimple_assign_rhs1 (def_stmt),
				    buffer, sizeof (buffer),
				    (offseti - offset2) / BITS_PER_UNIT);
	  if (len > 0 && len * BITS_PER_UNIT >= maxsizei)
	    {
	      tree type = vr->type;
	      /* Make sure to interpret in a type that has a range
	         covering the whole access size.  */
	      if (INTEGRAL_TYPE_P (vr->type)
		  && maxsizei != TYPE_PRECISION (vr->type))
		type = build_nonstandard_integer_type (maxsizei,
						       TYPE_UNSIGNED (type));
	      tree val = native_interpret_expr (type, buffer,
						maxsizei / BITS_PER_UNIT);
	      /* If we chop off bits because the types precision doesn't
		 match the memory access size this is ok when optimizing
		 reads but not when called from the DSE code during
		 elimination.  */
	      if (val
		  && type != vr->type)
		{
		  if (! int_fits_type_p (val, vr->type))
		    val = NULL_TREE;
		  else
		    val = fold_convert (vr->type, val);
		}

	      if (val)
		return vn_reference_lookup_or_insert_for_pieces
			 (vuse, vr->set, vr->type, vr->operands, val);
	    }
	}
    }

  /* 4) Assignment from an SSA name which definition we may be able
     to access pieces from.  */
  else if (known_eq (ref->size, maxsize)
	   && is_gimple_reg_type (vr->type)
	   && !contains_storage_order_barrier_p (vr->operands)
	   && gimple_assign_single_p (def_stmt)
	   && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
    {
      tree base2;
      poly_int64 offset2, size2, maxsize2;
      bool reverse;
      base2 = get_ref_base_and_extent (gimple_assign_lhs (def_stmt),
				       &offset2, &size2, &maxsize2,
				       &reverse);
      if (!reverse
	  && known_size_p (maxsize2)
	  && known_eq (maxsize2, size2)
	  && operand_equal_p (base, base2, 0)
	  && known_subrange_p (offset, maxsize, offset2, size2)
	  /* ???  We can't handle bitfield precision extracts without
	     either using an alternate type for the BIT_FIELD_REF and
	     then doing a conversion or possibly adjusting the offset
	     according to endianness.  */
	  && (! INTEGRAL_TYPE_P (vr->type)
	      || known_eq (ref->size, TYPE_PRECISION (vr->type)))
	  && multiple_p (ref->size, BITS_PER_UNIT))
	{
	  gimple_match_op op (gimple_match_cond::UNCOND,
			      BIT_FIELD_REF, vr->type,
			      vn_valueize (gimple_assign_rhs1 (def_stmt)),
			      bitsize_int (ref->size),
			      bitsize_int (offset - offset2));
	  tree val = vn_nary_build_or_lookup (&op);
	  if (val
	      && (TREE_CODE (val) != SSA_NAME
		  || ! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val)))
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
      int i, j, k;
      auto_vec<vn_reference_op_s> rhs;
      vn_reference_op_t vro;
      ao_ref r;

      if (!lhs_ref_ok)
	return (void *)-1;

      /* See if the assignment kills REF.  */
      base2 = ao_ref_base (&lhs_ref);
      if (!lhs_ref.max_size_known_p ()
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
      poly_int64 extra_off = 0;
      if (j == 0 && i >= 0
	  && lhs_ops[0].opcode == MEM_REF
	  && maybe_ne (lhs_ops[0].off, -1))
	{
	  if (known_eq (lhs_ops[0].off, vr->operands[i].off))
	    i--, j--;
	  else if (vr->operands[i].opcode == MEM_REF
		   && maybe_ne (vr->operands[i].off, -1))
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
      if (maybe_ne (extra_off, 0))
	{
	  if (rhs.length () < 2)
	    return (void *)-1;
	  int ix = rhs.length () - 2;
	  if (rhs[ix].opcode != MEM_REF
	      || known_eq (rhs[ix].off, -1))
	    return (void *)-1;
	  rhs[ix].off += extra_off;
	  rhs[ix].op0 = int_const_binop (PLUS_EXPR, rhs[ix].op0,
					 build_int_cst (TREE_TYPE (rhs[ix].op0),
							extra_off));
	}

      /* We need to pre-pend vr->operands[0..i] to rhs.  */
      vec<vn_reference_op_s> old = vr->operands;
      if (i + 1 + rhs.length () > vr->operands.length ())
	vr->operands.safe_grow (i + 1 + rhs.length ());
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
      if (maybe_ne (ref->size, r.size))
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
	   && poly_int_tree_p (gimple_call_arg (def_stmt, 2), &copy_size))
    {
      tree lhs, rhs;
      ao_ref r;
      poly_int64 rhs_offset, lhs_offset;
      vn_reference_op_s op;
      poly_uint64 mem_offset;
      poly_int64 at, byte_maxsize;

      /* Only handle non-variable, addressable refs.  */
      if (maybe_ne (ref->size, maxsize)
	  || !multiple_p (offset, BITS_PER_UNIT, &at)
	  || !multiple_p (maxsize, BITS_PER_UNIT, &byte_maxsize))
	return (void *)-1;

      /* Extract a pointer base and an offset for the destination.  */
      lhs = gimple_call_arg (def_stmt, 0);
      lhs_offset = 0;
      if (TREE_CODE (lhs) == SSA_NAME)
	{
	  lhs = vn_valueize (lhs);
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
	      && poly_int_tree_p (TREE_OPERAND (tem, 1), &mem_offset))
	    {
	      lhs = TREE_OPERAND (tem, 0);
	      if (TREE_CODE (lhs) == SSA_NAME)
		lhs = vn_valueize (lhs);
	      lhs_offset += mem_offset;
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
	rhs = vn_valueize (rhs);
      if (TREE_CODE (rhs) == ADDR_EXPR)
	{
	  tree tem = get_addr_base_and_unit_offset (TREE_OPERAND (rhs, 0),
						    &rhs_offset);
	  if (!tem)
	    return (void *)-1;
	  if (TREE_CODE (tem) == MEM_REF
	      && poly_int_tree_p (TREE_OPERAND (tem, 1), &mem_offset))
	    {
	      rhs = TREE_OPERAND (tem, 0);
	      rhs_offset += mem_offset;
	    }
	  else if (DECL_P (tem)
		   || TREE_CODE (tem) == STRING_CST)
	    rhs = build_fold_addr_expr (tem);
	  else
	    return (void *)-1;
	}
      if (TREE_CODE (rhs) != SSA_NAME
	  && TREE_CODE (rhs) != ADDR_EXPR)
	return (void *)-1;

      /* The bases of the destination and the references have to agree.  */
      if (TREE_CODE (base) == MEM_REF)
	{
	  if (TREE_OPERAND (base, 0) != lhs
	      || !poly_int_tree_p (TREE_OPERAND (base, 1), &mem_offset))
	    return (void *) -1;
	  at += mem_offset;
	}
      else if (!DECL_P (base)
	       || TREE_CODE (lhs) != ADDR_EXPR
	       || TREE_OPERAND (lhs, 0) != base)
	return (void *)-1;

      /* If the access is completely outside of the memcpy destination
	 area there is no aliasing.  */
      if (!ranges_maybe_overlap_p (lhs_offset, copy_size, at, byte_maxsize))
	return NULL;
      /* And the access has to be contained within the memcpy destination.  */
      if (!known_subrange_p (at, byte_maxsize, lhs_offset, copy_size))
	return (void *)-1;

      /* Make room for 2 operands in the new reference.  */
      if (vr->operands.length () < 2)
	{
	  vec<vn_reference_op_s> old = vr->operands;
	  vr->operands.safe_grow_cleared (2);
	  if (old == shared_lookup_references)
	    shared_lookup_references = vr->operands;
	}
      else
	vr->operands.truncate (2);

      /* The looked-through reference is a simple MEM_REF.  */
      memset (&op, 0, sizeof (op));
      op.type = vr->type;
      op.opcode = MEM_REF;
      op.op0 = build_int_cst (ptr_type_node, at - lhs_offset + rhs_offset);
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
      if (maybe_ne (ref->size, r.size))
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

/* Return a reference op vector from OP that can be used for
   vn_reference_lookup_pieces.  The caller is responsible for releasing
   the vector.  */

vec<vn_reference_op_s>
vn_reference_operands_for_lookup (tree op)
{
  bool valueized;
  return valueize_shared_reference_ops_from_ref (op, &valueized).copy ();
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

/* Insert OP into the current hash table with a value number of RESULT.  */

static void 
vn_reference_insert (tree op, tree result, tree vuse, tree vdef)
{
  vn_reference_s **slot;
  vn_reference_t vr1;
  bool tem;

  vr1 = XOBNEW (&vn_tables_obstack, vn_reference_s);
  if (TREE_CODE (result) == SSA_NAME)
    vr1->value_id = VN_INFO (result)->value_id;
  else
    vr1->value_id = get_or_alloc_constant_value_id (result);
  vr1->vuse = vuse_ssa_val (vuse);
  vr1->operands = valueize_shared_reference_ops_from_ref (op, &tem).copy ();
  vr1->type = TREE_TYPE (op);
  vr1->set = get_alias_set (op);
  vr1->hashcode = vn_reference_compute_hash (vr1);
  vr1->result = TREE_CODE (result) == SSA_NAME ? SSA_VAL (result) : result;
  vr1->result_vdef = vdef;

  slot = valid_info->references->find_slot_with_hash (vr1, vr1->hashcode,
						      INSERT);

  /* Because IL walking on reference lookup can end up visiting
     a def that is only to be visited later in iteration order
     when we are about to make an irreducible region reducible
     the def can be effectively processed and its ref being inserted
     by vn_reference_lookup_3 already.  So we cannot assert (!*slot)
     but save a lookup if we deal with already inserted refs here.  */
  if (*slot)
    {
      /* We cannot assert that we have the same value either because
         when disentangling an irreducible region we may end up visiting
	 a use before the corresponding def.  That's a missed optimization
	 only though.  See gcc.dg/tree-ssa/pr87126.c for example.  */
      if (dump_file && (dump_flags & TDF_DETAILS)
	  && !operand_equal_p ((*slot)->result, vr1->result, 0))
	{
	  fprintf (dump_file, "Keeping old value ");
	  print_generic_expr (dump_file, (*slot)->result);
	  fprintf (dump_file, " because of collision\n");
	}
      free_reference (vr1);
      obstack_free (&vn_tables_obstack, vr1);
      return;
    }

  *slot = vr1;
  vr1->next = last_inserted_ref;
  last_inserted_ref = vr1;
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

  vr1 = XOBNEW (&vn_tables_obstack, vn_reference_s);
  vr1->value_id = value_id;
  vr1->vuse = vuse_ssa_val (vuse);
  vr1->operands = valueize_refs (operands);
  vr1->type = type;
  vr1->set = set;
  vr1->hashcode = vn_reference_compute_hash (vr1);
  if (result && TREE_CODE (result) == SSA_NAME)
    result = SSA_VAL (result);
  vr1->result = result;

  slot = valid_info->references->find_slot_with_hash (vr1, vr1->hashcode,
						      INSERT);

  /* At this point we should have all the things inserted that we have
     seen before, and we should never try inserting something that
     already exists.  */
  gcc_assert (!*slot);

  *slot = vr1;
  vr1->next = last_inserted_ref;
  last_inserted_ref = vr1;
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
      && tree_swap_operands_p (vno1->op[0], vno1->op[1]))
    std::swap (vno1->op[0], vno1->op[1]);
  else if (TREE_CODE_CLASS (vno1->opcode) == tcc_comparison
	   && tree_swap_operands_p (vno1->op[0], vno1->op[1]))
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

  /* BIT_INSERT_EXPR has an implict operand as the type precision
     of op1.  Need to check to make sure they are the same.  */
  if (vno1->opcode == BIT_INSERT_EXPR
      && TREE_CODE (vno1->op[1]) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (vno1->op[1]))
	 != TYPE_PRECISION (TREE_TYPE (vno2->op[1])))
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
  slot = valid_info->nary->find_slot_with_hash (vno, vno->hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  if (vnresult)
    *vnresult = *slot;
  return (*slot)->predicated_values ? NULL_TREE : (*slot)->u.result;
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
  vn_nary_op_t vno1 = alloc_vn_nary_op_noinit (length, &vn_tables_obstack);

  vno1->value_id = value_id;
  vno1->length = length;
  vno1->predicated_values = 0;
  vno1->u.result = result;

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
    {
      vno->hashcode = vn_nary_op_compute_hash (vno);
      gcc_assert (! vno->predicated_values
		  || (! vno->u.values->next
		      && vno->u.values->valid_dominated_by_p[0] != EXIT_BLOCK
		      && vno->u.values->valid_dominated_by_p[1] == EXIT_BLOCK));
    }

  slot = table->find_slot_with_hash (vno, vno->hashcode, INSERT);
  vno->unwind_to = *slot;
  if (*slot)
    {
      /* Prefer non-predicated values.
         ???  Only if those are constant, otherwise, with constant predicated
	 value, turn them into predicated values with entry-block validity
	 (???  but we always find the first valid result currently).  */
      if ((*slot)->predicated_values
	  && ! vno->predicated_values)
	{
	  /* ???  We cannot remove *slot from the unwind stack list.
	     For the moment we deal with this by skipping not found
	     entries but this isn't ideal ...  */
	  *slot = vno;
	  /* ???  Maintain a stack of states we can unwind in
	     vn_nary_op_s?  But how far do we unwind?  In reality
	     we need to push change records somewhere...  Or not
	     unwind vn_nary_op_s and linking them but instead
	     unwind the results "list", linking that, which also
	     doesn't move on hashtable resize.  */
	  /* We can also have a ->unwind_to recording *slot there.
	     That way we can make u.values a fixed size array with
	     recording the number of entries but of course we then
	     have always N copies for each unwind_to-state.  Or we
             make sure to only ever append and each unwinding will
	     pop off one entry (but how to deal with predicated
	     replaced with non-predicated here?)  */
	  vno->next = last_inserted_nary;
	  last_inserted_nary = vno;
	  return vno;
	}
      else if (vno->predicated_values
	       && ! (*slot)->predicated_values)
	return *slot;
      else if (vno->predicated_values
	       && (*slot)->predicated_values)
	{
	  /* ???  Factor this all into a insert_single_predicated_value
	     routine.  */
	  gcc_assert (!vno->u.values->next && vno->u.values->n == 1);
	  basic_block vno_bb
	    = BASIC_BLOCK_FOR_FN (cfun, vno->u.values->valid_dominated_by_p[0]);
	  vn_pval *nval = vno->u.values;
	  vn_pval **next = &vno->u.values;
	  bool found = false;
	  for (vn_pval *val = (*slot)->u.values; val; val = val->next)
	    {
	      if (expressions_equal_p (val->result, vno->u.values->result))
		{
		  found = true;
		  for (unsigned i = 0; i < val->n; ++i)
		    {
		      basic_block val_bb
			= BASIC_BLOCK_FOR_FN (cfun,
					      val->valid_dominated_by_p[i]);
		      if (dominated_by_p (CDI_DOMINATORS, vno_bb, val_bb))
			/* Value registered with more generic predicate.  */
			return *slot;
		      else if (dominated_by_p (CDI_DOMINATORS, val_bb, vno_bb))
			/* Shouldn't happen, we insert in RPO order.  */
			gcc_unreachable ();
		    }
		  /* Append value.  */
		  *next = (vn_pval *) obstack_alloc (&vn_tables_obstack,
						     sizeof (vn_pval)
						     + val->n * sizeof (int));
		  (*next)->next = NULL;
		  (*next)->result = val->result;
		  (*next)->n = val->n + 1;
		  memcpy ((*next)->valid_dominated_by_p,
			  val->valid_dominated_by_p,
			  val->n * sizeof (int));
		  (*next)->valid_dominated_by_p[val->n] = vno_bb->index;
		  next = &(*next)->next;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Appending predicate to value.\n");
		  continue;
		}
	      /* Copy other predicated values.  */
	      *next = (vn_pval *) obstack_alloc (&vn_tables_obstack,
						 sizeof (vn_pval)
						 + (val->n-1) * sizeof (int));
	      memcpy (*next, val, sizeof (vn_pval) + (val->n-1) * sizeof (int));
	      (*next)->next = NULL;
	      next = &(*next)->next;
	    }
	  if (!found)
	    *next = nval;

	  *slot = vno;
	  vno->next = last_inserted_nary;
	  last_inserted_nary = vno;
	  return vno;
	}

      /* While we do not want to insert things twice it's awkward to
	 avoid it in the case where visit_nary_op pattern-matches stuff
	 and ends up simplifying the replacement to itself.  We then
	 get two inserts, one from visit_nary_op and one from
	 vn_nary_build_or_lookup.
	 So allow inserts with the same value number.  */
      if ((*slot)->u.result == vno->u.result)
	return *slot;
    }

  /* ???  There's also optimistic vs. previous commited state merging
     that is problematic for the case of unwinding.  */

  /* ???  We should return NULL if we do not use 'vno' and have the
     caller release it.  */
  gcc_assert (!*slot);

  *slot = vno;
  vno->next = last_inserted_nary;
  last_inserted_nary = vno;
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
  return vn_nary_op_insert_into (vno1, valid_info->nary, true);
}

static vn_nary_op_t
vn_nary_op_insert_pieces_predicated (unsigned int length, enum tree_code code,
				     tree type, tree *ops,
				     tree result, unsigned int value_id,
				     edge pred_e)
{
  /* ???  Currently tracking BBs.  */
  if (! single_pred_p (pred_e->dest))
    {
      /* Never record for backedges.  */
      if (pred_e->flags & EDGE_DFS_BACK)
	return NULL;
      edge_iterator ei;
      edge e;
      int cnt = 0;
      /* Ignore backedges.  */
      FOR_EACH_EDGE (e, ei, pred_e->dest->preds)
	if (! dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
	  cnt++;
      if (cnt != 1)
	return NULL;
    }
  if (dump_file && (dump_flags & TDF_DETAILS)
      /* ???  Fix dumping, but currently we only get comparisons.  */
      && TREE_CODE_CLASS (code) == tcc_comparison)
    {
      fprintf (dump_file, "Recording on edge %d->%d ", pred_e->src->index,
	       pred_e->dest->index);
      print_generic_expr (dump_file, ops[0], TDF_SLIM);
      fprintf (dump_file, " %s ", get_tree_code_name (code));
      print_generic_expr (dump_file, ops[1], TDF_SLIM);
      fprintf (dump_file, " == %s\n",
	       integer_zerop (result) ? "false" : "true");
    }
  vn_nary_op_t vno1 = alloc_vn_nary_op (length, NULL_TREE, value_id);
  init_vn_nary_op_from_pieces (vno1, length, code, type, ops);
  vno1->predicated_values = 1;
  vno1->u.values = (vn_pval *) obstack_alloc (&vn_tables_obstack,
					      sizeof (vn_pval));
  vno1->u.values->next = NULL;
  vno1->u.values->result = result;
  vno1->u.values->n = 1;
  vno1->u.values->valid_dominated_by_p[0] = pred_e->dest->index;
  vno1->u.values->valid_dominated_by_p[1] = EXIT_BLOCK;
  return vn_nary_op_insert_into (vno1, valid_info->nary, true);
}

static bool
dominated_by_p_w_unex (basic_block bb1, basic_block bb2);

static tree
vn_nary_op_get_predicated_value (vn_nary_op_t vno, basic_block bb)
{
  if (! vno->predicated_values)
    return vno->u.result;
  for (vn_pval *val = vno->u.values; val; val = val->next)
    for (unsigned i = 0; i < val->n; ++i)
      if (dominated_by_p_w_unex (bb,
			  BASIC_BLOCK_FOR_FN
			    (cfun, val->valid_dominated_by_p[i])))
	return val->result;
  return NULL_TREE;
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
  return vn_nary_op_insert_into (vno1, valid_info->nary, true);
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
  return vn_nary_op_insert_into (vno1, valid_info->nary, true);
}

/* Compute a hashcode for PHI operation VP1 and return it.  */

static inline hashval_t
vn_phi_compute_hash (vn_phi_t vp1)
{
  inchash::hash hstate (EDGE_COUNT (vp1->block->preds) > 2
			? vp1->block->index : EDGE_COUNT (vp1->block->preds));
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
cond_stmts_equal_p (gcond *cond1, tree lhs1, tree rhs1,
		    gcond *cond2, tree lhs2, tree rhs2, bool *inverted_p)
{
  enum tree_code code1 = gimple_cond_code (cond1);
  enum tree_code code2 = gimple_cond_code (cond2);

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
      if (EDGE_COUNT (vp1->block->preds) != EDGE_COUNT (vp2->block->preds))
	return false;

      switch (EDGE_COUNT (vp1->block->preds))
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
	    gcond *last1 = safe_dyn_cast <gcond *> (last_stmt (idom1));
	    gcond *last2 = safe_dyn_cast <gcond *> (last_stmt (idom2));
	    if (! last1 || ! last2)
	      return false;
	    bool inverted_p;
	    if (! cond_stmts_equal_p (last1, vp1->cclhs, vp1->ccrhs,
				      last2, vp2->cclhs, vp2->ccrhs,
				      &inverted_p))
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
  for (unsigned i = 0; i < EDGE_COUNT (vp1->block->preds); ++i)
    {
      tree phi1op = vp1->phiargs[i];
      tree phi2op = vp2->phiargs[i];
      if (phi1op == VN_TOP || phi2op == VN_TOP)
	continue;
      if (!expressions_equal_p (phi1op, phi2op))
	return false;
    }

  return true;
}

/* Lookup PHI in the current hash table, and return the resulting
   value number if it exists in the hash table.  Return NULL_TREE if
   it does not exist in the hash table. */

static tree
vn_phi_lookup (gimple *phi, bool backedges_varying_p)
{
  vn_phi_s **slot;
  struct vn_phi_s *vp1;
  edge e;
  edge_iterator ei;

  vp1 = XALLOCAVAR (struct vn_phi_s,
		    sizeof (struct vn_phi_s)
		    + (gimple_phi_num_args (phi) - 1) * sizeof (tree));

  /* Canonicalize the SSA_NAME's to their value number.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    {
      tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);
      if (TREE_CODE (def) == SSA_NAME
	  && (!backedges_varying_p || !(e->flags & EDGE_DFS_BACK)))
	def = SSA_VAL (def);
      vp1->phiargs[e->dest_idx] = def;
    }
  vp1->type = TREE_TYPE (gimple_phi_result (phi));
  vp1->block = gimple_bb (phi);
  /* Extract values of the controlling condition.  */
  vp1->cclhs = NULL_TREE;
  vp1->ccrhs = NULL_TREE;
  basic_block idom1 = get_immediate_dominator (CDI_DOMINATORS, vp1->block);
  if (EDGE_COUNT (idom1->succs) == 2)
    if (gcond *last1 = safe_dyn_cast <gcond *> (last_stmt (idom1)))
      {
	/* ???  We want to use SSA_VAL here.  But possibly not
	   allow VN_TOP.  */
	vp1->cclhs = vn_valueize (gimple_cond_lhs (last1));
	vp1->ccrhs = vn_valueize (gimple_cond_rhs (last1));
      }
  vp1->hashcode = vn_phi_compute_hash (vp1);
  slot = valid_info->phis->find_slot_with_hash (vp1, vp1->hashcode, NO_INSERT);
  if (!slot)
    return NULL_TREE;
  return (*slot)->result;
}

/* Insert PHI into the current hash table with a value number of
   RESULT.  */

static vn_phi_t
vn_phi_insert (gimple *phi, tree result, bool backedges_varying_p)
{
  vn_phi_s **slot;
  vn_phi_t vp1 = (vn_phi_t) obstack_alloc (&vn_tables_obstack,
					   sizeof (vn_phi_s)
					   + ((gimple_phi_num_args (phi) - 1)
					      * sizeof (tree)));
  edge e;
  edge_iterator ei;

  /* Canonicalize the SSA_NAME's to their value number.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    {
      tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);
      if (TREE_CODE (def) == SSA_NAME
	  && (!backedges_varying_p || !(e->flags & EDGE_DFS_BACK)))
	def = SSA_VAL (def);
      vp1->phiargs[e->dest_idx] = def;
    }
  vp1->value_id = VN_INFO (result)->value_id;
  vp1->type = TREE_TYPE (gimple_phi_result (phi));
  vp1->block = gimple_bb (phi);
  /* Extract values of the controlling condition.  */
  vp1->cclhs = NULL_TREE;
  vp1->ccrhs = NULL_TREE;
  basic_block idom1 = get_immediate_dominator (CDI_DOMINATORS, vp1->block);
  if (EDGE_COUNT (idom1->succs) == 2)
    if (gcond *last1 = safe_dyn_cast <gcond *> (last_stmt (idom1)))
      {
	/* ???  We want to use SSA_VAL here.  But possibly not
	   allow VN_TOP.  */
	vp1->cclhs = vn_valueize (gimple_cond_lhs (last1));
	vp1->ccrhs = vn_valueize (gimple_cond_rhs (last1));
      }
  vp1->result = result;
  vp1->hashcode = vn_phi_compute_hash (vp1);

  slot = valid_info->phis->find_slot_with_hash (vp1, vp1->hashcode, INSERT);
  gcc_assert (!*slot);

  *slot = vp1;
  vp1->next = last_inserted_phi;
  last_inserted_phi = vp1;
  return vp1;
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
  vn_ssa_aux_t from_info = VN_INFO (from);
  tree currval = from_info->valnum; // SSA_VAL (from)
  poly_int64 toff, coff;

  /* The only thing we allow as value numbers are ssa_names
     and invariants.  So assert that here.  We don't allow VN_TOP
     as visiting a stmt should produce a value-number other than
     that.
     ???  Still VN_TOP can happen for unreachable code, so force
     it to varying in that case.  Not all code is prepared to
     get VN_TOP on valueization.  */
  if (to == VN_TOP)
    {
      /* ???  When iterating and visiting PHI <undef, backedge-value>
         for the first time we rightfully get VN_TOP and we need to
	 preserve that to optimize for example gcc.dg/tree-ssa/ssa-sccvn-2.c.
	 With SCCVN we were simply lucky we iterated the other PHI
	 cycles first and thus visited the backedge-value DEF.  */
      if (currval == VN_TOP)
	goto set_and_exit;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Forcing value number to varying on "
		 "receiving VN_TOP\n");
      to = from;
    }

  gcc_checking_assert (to != NULL_TREE
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
	      print_generic_expr (dump_file, from);
	      fprintf (dump_file, " from VARYING to ");
	      print_generic_expr (dump_file, to);
	      fprintf (dump_file, "\n");
	    }
	  return false;
	}
      else if (currval != VN_TOP
	       && ! is_gimple_min_invariant (currval)
	       && ! ssa_undefined_value_p (currval, false)
	       && is_gimple_min_invariant (to))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Forcing VARYING instead of changing "
		       "value number of ");
	      print_generic_expr (dump_file, from);
	      fprintf (dump_file, " from ");
	      print_generic_expr (dump_file, currval);
	      fprintf (dump_file, " (non-constant) to ");
	      print_generic_expr (dump_file, to);
	      fprintf (dump_file, " (constant)\n");
	    }
	  to = from;
	}
      else if (TREE_CODE (to) == SSA_NAME
	       && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (to))
	to = from;
    }

set_and_exit:
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Setting value number of ");
      print_generic_expr (dump_file, from);
      fprintf (dump_file, " to ");
      print_generic_expr (dump_file, to);
    }

  if (currval != to
      && !operand_equal_p (currval, to, 0)
      /* Different undefined SSA names are not actually different.  See
         PR82320 for a testcase were we'd otherwise not terminate iteration.  */
      && !(TREE_CODE (currval) == SSA_NAME
	   && TREE_CODE (to) == SSA_NAME
	   && ssa_undefined_value_p (currval, false)
	   && ssa_undefined_value_p (to, false))
      /* ???  For addresses involving volatile objects or types operand_equal_p
         does not reliably detect ADDR_EXPRs as equal.  We know we are only
	 getting invariant gimple addresses here, so can use
	 get_addr_base_and_unit_offset to do this comparison.  */
      && !(TREE_CODE (currval) == ADDR_EXPR
	   && TREE_CODE (to) == ADDR_EXPR
	   && (get_addr_base_and_unit_offset (TREE_OPERAND (currval, 0), &coff)
	       == get_addr_base_and_unit_offset (TREE_OPERAND (to, 0), &toff))
	   && known_eq (coff, toff)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " (changed)\n");
      from_info->valnum = to;
      return true;
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n");
  return false;
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

/* Lookup a value for OP in type WIDE_TYPE where the value in type of OP
   is the same.  */

static tree
valueized_wider_op (tree wide_type, tree op)
{
  if (TREE_CODE (op) == SSA_NAME)
    op = vn_valueize (op);

  /* Either we have the op widened available.  */
  tree ops[3] = {};
  ops[0] = op;
  tree tem = vn_nary_op_lookup_pieces (1, NOP_EXPR,
				       wide_type, ops, NULL);
  if (tem)
    return tem;

  /* Or the op is truncated from some existing value.  */
  if (TREE_CODE (op) == SSA_NAME)
    {
      gimple *def = SSA_NAME_DEF_STMT (op);
      if (is_gimple_assign (def)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def)))
	{
	  tem = gimple_assign_rhs1 (def);
	  if (useless_type_conversion_p (wide_type, TREE_TYPE (tem)))
	    {
	      if (TREE_CODE (tem) == SSA_NAME)
		tem = vn_valueize (tem);
	      return tem;
	    }
	}
    }

  /* For constants simply extend it.  */
  if (TREE_CODE (op) == INTEGER_CST)
    return wide_int_to_tree (wide_type, wi::to_wide (op));

  return NULL_TREE;
}

/* Visit a nary operator RHS, value number it, and return true if the
   value number of LHS has changed as a result.  */

static bool
visit_nary_op (tree lhs, gassign *stmt)
{
  vn_nary_op_t vnresult;
  tree result = vn_nary_op_lookup_stmt (stmt, &vnresult);
  if (! result && vnresult)
    result = vn_nary_op_get_predicated_value (vnresult, gimple_bb (stmt));
  if (result)
    return set_ssa_val_to (lhs, result);

  /* Do some special pattern matching for redundancies of operations
     in different types.  */
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree type = TREE_TYPE (lhs);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  switch (code)
    {
    CASE_CONVERT:
      /* Match arithmetic done in a different type where we can easily
         substitute the result from some earlier sign-changed or widened
	 operation.  */
      if (INTEGRAL_TYPE_P (type)
	  && TREE_CODE (rhs1) == SSA_NAME
	  /* We only handle sign-changes or zero-extension -> & mask.  */
	  && ((TYPE_UNSIGNED (TREE_TYPE (rhs1))
	       && TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (rhs1)))
	      || TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (rhs1))))
	{
	  gassign *def = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (rhs1));
	  if (def
	      && (gimple_assign_rhs_code (def) == PLUS_EXPR
		  || gimple_assign_rhs_code (def) == MINUS_EXPR
		  || gimple_assign_rhs_code (def) == MULT_EXPR))
	    {
	      tree ops[3] = {};
	      /* Either we have the op widened available.  */
	      ops[0] = valueized_wider_op (type,
					   gimple_assign_rhs1 (def));
	      if (ops[0])
		ops[1] = valueized_wider_op (type,
					     gimple_assign_rhs2 (def));
	      if (ops[0] && ops[1])
		{
		  ops[0] = vn_nary_op_lookup_pieces
		      (2, gimple_assign_rhs_code (def), type, ops, NULL);
		  /* We have wider operation available.  */
		  if (ops[0])
		    {
		      unsigned lhs_prec = TYPE_PRECISION (type);
		      unsigned rhs_prec = TYPE_PRECISION (TREE_TYPE (rhs1));
		      if (lhs_prec == rhs_prec)
			{
			  gimple_match_op match_op (gimple_match_cond::UNCOND,
						    NOP_EXPR, type, ops[0]);
			  result = vn_nary_build_or_lookup (&match_op);
			  if (result)
			    {
			      bool changed = set_ssa_val_to (lhs, result);
			      vn_nary_op_insert_stmt (stmt, result);
			      return changed;
			    }
			}
		      else
			{
			  tree mask = wide_int_to_tree
			    (type, wi::mask (rhs_prec, false, lhs_prec));
			  gimple_match_op match_op (gimple_match_cond::UNCOND,
						    BIT_AND_EXPR,
						    TREE_TYPE (lhs),
						    ops[0], mask);
			  result = vn_nary_build_or_lookup (&match_op);
			  if (result)
			    {
			      bool changed = set_ssa_val_to (lhs, result);
			      vn_nary_op_insert_stmt (stmt, result);
			      return changed;
			    }
			}
		    }
		}
	    }
	}
    default:;
    }

  bool changed = set_ssa_val_to (lhs, lhs);
  vn_nary_op_insert_stmt (stmt, lhs);
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
      else if (vdef)
	/* If the call was discovered to be pure or const reflect
	   that as far as possible.  */
	changed |= set_ssa_val_to (vdef, vuse_ssa_val (gimple_vuse (stmt)));

      if (!vnresult->result && lhs)
	vnresult->result = lhs;

      if (vnresult->result && lhs)
	changed |= set_ssa_val_to (lhs, vnresult->result);
    }
  else
    {
      vn_reference_t vr2;
      vn_reference_s **slot;
      tree vdef_val = vdef;
      if (vdef)
	{
	  /* If we value numbered an indirect functions function to
	     one not clobbering memory value number its VDEF to its
	     VUSE.  */
	  tree fn = gimple_call_fn (stmt);
	  if (fn && TREE_CODE (fn) == SSA_NAME)
	    {
	      fn = SSA_VAL (fn);
	      if (TREE_CODE (fn) == ADDR_EXPR
		  && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL
		  && (flags_from_decl_or_type (TREE_OPERAND (fn, 0))
		      & (ECF_CONST | ECF_PURE)))
		vdef_val = vuse_ssa_val (gimple_vuse (stmt));
	    }
	  changed |= set_ssa_val_to (vdef, vdef_val);
	}
      if (lhs)
	changed |= set_ssa_val_to (lhs, lhs);
      vr2 = XOBNEW (&vn_tables_obstack, vn_reference_s);
      vr2->vuse = vr1.vuse;
      /* As we are not walking the virtual operand chain we know the
	 shared_lookup_references are still original so we can re-use
	 them here.  */
      vr2->operands = vr1.operands.copy ();
      vr2->type = vr1.type;
      vr2->set = vr1.set;
      vr2->hashcode = vr1.hashcode;
      vr2->result = lhs;
      vr2->result_vdef = vdef_val;
      slot = valid_info->references->find_slot_with_hash (vr2, vr2->hashcode,
							  INSERT);
      gcc_assert (!*slot);
      *slot = vr2;
      vr2->next = last_inserted_ref;
      last_inserted_ref = vr2;
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
      gimple_match_op res_op (gimple_match_cond::UNCOND,
			      VIEW_CONVERT_EXPR, TREE_TYPE (op), result);
      result = vn_nary_build_or_lookup (&res_op);
      /* When building the conversion fails avoid inserting the reference
         again.  */
      if (!result)
	return set_ssa_val_to (lhs, lhs);
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
  tree assign;
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

  vn_reference_lookup (lhs, vuse, VN_NOWALK, &vnresult, false);
  if (vnresult
      && vnresult->result)
    {
      tree result = vnresult->result;
      gcc_checking_assert (TREE_CODE (result) != SSA_NAME
			   || result == SSA_VAL (result));
      resultsame = expressions_equal_p (result, op);
      if (resultsame)
	{
	  /* If the TBAA state isn't compatible for downstream reads
	     we cannot value-number the VDEFs the same.  */
	  alias_set_type set = get_alias_set (lhs);
	  if (vnresult->set != set
	      && ! alias_set_subset_of (set, vnresult->set))
	    resultsame = false;
	}
    }

  if (!resultsame)
    {
      /* Only perform the following when being called from PRE
	 which embeds tail merging.  */
      if (default_vn_walk_kind == VN_WALK)
	{
	  assign = build2 (MODIFY_EXPR, TREE_TYPE (lhs), lhs, op);
	  vn_reference_lookup (assign, vuse, VN_NOWALK, &vnresult, false);
	  if (vnresult)
	    {
	      VN_INFO (vdef)->visited = true;
	      return set_ssa_val_to (vdef, vnresult->result_vdef);
	    }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "No store match\n");
	  fprintf (dump_file, "Value numbering store ");
	  print_generic_expr (dump_file, lhs);
	  fprintf (dump_file, " to ");
	  print_generic_expr (dump_file, op);
	  fprintf (dump_file, "\n");
	}
      /* Have to set value numbers before insert, since insert is
	 going to valueize the references in-place.  */
      if (vdef)
	changed |= set_ssa_val_to (vdef, vdef);

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
	fprintf (dump_file, "Store matched earlier value, "
		 "value numbering store vdefs to matching vuses.\n");

      changed |= set_ssa_val_to (vdef, SSA_VAL (vuse));
    }

  return changed;
}

/* Visit and value number PHI, return true if the value number
   changed.  When BACKEDGES_VARYING_P is true then assume all
   backedge values are varying.  When INSERTED is not NULL then
   this is just a ahead query for a possible iteration, set INSERTED
   to true if we'd insert into the hashtable.  */

static bool
visit_phi (gimple *phi, bool *inserted, bool backedges_varying_p)
{
  tree result, sameval = VN_TOP, seen_undef = NULL_TREE;
  tree backedge_val = NULL_TREE;
  bool seen_non_backedge = false;
  tree sameval_base = NULL_TREE;
  poly_int64 soff, doff;
  unsigned n_executable = 0;
  edge_iterator ei;
  edge e;

  /* TODO: We could check for this in initialization, and replace this
     with a gcc_assert.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
    return set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));

  /* We track whether a PHI was CSEd to to avoid excessive iterations
     that would be necessary only because the PHI changed arguments
     but not value.  */
  if (!inserted)
    gimple_set_plf (phi, GF_PLF_1, false);

  /* See if all non-TOP arguments have the same value.  TOP is
     equivalent to everything, so we can ignore it.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
    if (e->flags & EDGE_EXECUTABLE)
      {
	tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);

	++n_executable;
	if (TREE_CODE (def) == SSA_NAME)
	  {
	    if (!backedges_varying_p || !(e->flags & EDGE_DFS_BACK))
	      def = SSA_VAL (def);
	    if (e->flags & EDGE_DFS_BACK)
	      backedge_val = def;
	  }
	if (!(e->flags & EDGE_DFS_BACK))
	  seen_non_backedge = true;
	if (def == VN_TOP)
	  ;
	/* Ignore undefined defs for sameval but record one.  */
	else if (TREE_CODE (def) == SSA_NAME
		 && ! virtual_operand_p (def)
		 && ssa_undefined_value_p (def, false))
	  seen_undef = def;
	else if (sameval == VN_TOP)
	  sameval = def;
	else if (!expressions_equal_p (def, sameval))
	  {
	    /* We know we're arriving only with invariant addresses here,
	       try harder comparing them.  We can do some caching here
	       which we cannot do in expressions_equal_p.  */
	    if (TREE_CODE (def) == ADDR_EXPR
		&& TREE_CODE (sameval) == ADDR_EXPR
		&& sameval_base != (void *)-1)
	      {
		if (!sameval_base)
		  sameval_base = get_addr_base_and_unit_offset
				   (TREE_OPERAND (sameval, 0), &soff);
		if (!sameval_base)
		  sameval_base = (tree)(void *)-1;
		else if ((get_addr_base_and_unit_offset
			    (TREE_OPERAND (def, 0), &doff) == sameval_base)
			 && known_eq (soff, doff))
		  continue;
	      }
	    sameval = NULL_TREE;
	    break;
	  }
      }

  /* If the value we want to use is the backedge and that wasn't visited
     yet or if we should take it as VARYING but it has a non-VARYING
     value drop to VARYING.  This only happens when not iterating.
     If we value-number a virtual operand never value-number to the
     value from the backedge as that confuses the alias-walking code.
     See gcc.dg/torture/pr87176.c.  If the value is the same on a
     non-backedge everything is OK though.  */
  if (backedge_val
      && !seen_non_backedge
      && TREE_CODE (backedge_val) == SSA_NAME
      && sameval == backedge_val
      && (SSA_NAME_IS_VIRTUAL_OPERAND (backedge_val)
	  || !SSA_VISITED (backedge_val)
	  || SSA_VAL (backedge_val) != backedge_val))
    /* Note this just drops to VARYING without inserting the PHI into
       the hashes.  */
    result = PHI_RESULT (phi);
  /* If none of the edges was executable keep the value-number at VN_TOP,
     if only a single edge is exectuable use its value.  */
  else if (n_executable <= 1)
    result = seen_undef ? seen_undef : sameval;
  /* If we saw only undefined values and VN_TOP use one of the
     undefined values.  */
  else if (sameval == VN_TOP)
    result = seen_undef ? seen_undef : sameval;
  /* First see if it is equivalent to a phi node in this block.  We prefer
     this as it allows IV elimination - see PRs 66502 and 67167.  */
  else if ((result = vn_phi_lookup (phi, backedges_varying_p)))
    {
      if (!inserted
	  && TREE_CODE (result) == SSA_NAME
	  && gimple_code (SSA_NAME_DEF_STMT (result)) == GIMPLE_PHI)
	{
	  gimple_set_plf (SSA_NAME_DEF_STMT (result), GF_PLF_1, true);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Marking CSEd to PHI node ");
	      print_gimple_expr (dump_file, SSA_NAME_DEF_STMT (result),
				 0, TDF_SLIM);
	      fprintf (dump_file, "\n");
	    }
	}
    }
  /* If all values are the same use that, unless we've seen undefined
     values as well and the value isn't constant.
     CCP/copyprop have the same restriction to not remove uninit warnings.  */
  else if (sameval
	   && (! seen_undef || is_gimple_min_invariant (sameval)))
    result = sameval;
  else
    {
      result = PHI_RESULT (phi);
      /* Only insert PHIs that are varying, for constant value numbers
         we mess up equivalences otherwise as we are only comparing
	 the immediate controlling predicates.  */
      vn_phi_insert (phi, result, backedges_varying_p);
      if (inserted)
	*inserted = true;
    }

  return set_ssa_val_to (PHI_RESULT (phi), result);
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

/* Visit and value number STMT, return true if the value number
   changed.  */

static bool
visit_stmt (gimple *stmt, bool backedges_varying_p = false)
{
  bool changed = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Value numbering stmt = ");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  if (gimple_code (stmt) == GIMPLE_PHI)
    changed = visit_phi (stmt, NULL, backedges_varying_p);
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
	      print_gimple_expr (dump_file, ass, 0);
	      fprintf (dump_file, " simplified to ");
	      print_generic_expr (dump_file, simplified);
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
		  print_gimple_expr (dump_file, call_stmt, 0);
		  fprintf (dump_file, " simplified to ");
		  print_generic_expr (dump_file, simplified);
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

      /* Pick up flags from a devirtualization target.  */
      tree fn = gimple_call_fn (stmt);
      int extra_fnflags = 0;
      if (fn && TREE_CODE (fn) == SSA_NAME)
	{
	  fn = SSA_VAL (fn);
	  if (TREE_CODE (fn) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (fn, 0)) == FUNCTION_DECL)
	    extra_fnflags = flags_from_decl_or_type (TREE_OPERAND (fn, 0));
	}
      if (!gimple_call_internal_p (call_stmt)
	  && (/* Calls to the same function with the same vuse
		 and the same operands do not necessarily return the same
		 value, unless they're pure or const.  */
	      ((gimple_call_flags (call_stmt) | extra_fnflags)
	       & (ECF_PURE | ECF_CONST))
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


/* Allocate a value number table.  */

static void
allocate_vn_table (vn_tables_t table, unsigned size)
{
  table->phis = new vn_phi_table_type (size);
  table->nary = new vn_nary_op_table_type (size);
  table->references = new vn_reference_table_type (size);
}

/* Free a value number table.  */

static void
free_vn_table (vn_tables_t table)
{
  /* Walk over elements and release vectors.  */
  vn_reference_iterator_type hir;
  vn_reference_t vr;
  FOR_EACH_HASH_TABLE_ELEMENT (*table->references, vr, vn_reference_t, hir)
    vr->operands.release ();
  delete table->phis;
  table->phis = NULL;
  delete table->nary;
  table->nary = NULL;
  delete table->references;
  table->references = NULL;
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
    if (! vno->predicated_values)
      set_value_id_for_result (vno->u.result, &vno->value_id);

  FOR_EACH_HASH_TABLE_ELEMENT (*valid_info->phis, vp, vn_phi_t, hip)
    set_value_id_for_result (vp->result, &vp->value_id);

  FOR_EACH_HASH_TABLE_ELEMENT (*valid_info->references, vr, vn_reference_t,
			       hir)
    set_value_id_for_result (vr->result, &vr->value_id);
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


class eliminate_dom_walker : public dom_walker
{
public:
  eliminate_dom_walker (cdi_direction, bitmap);
  ~eliminate_dom_walker ();

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

  virtual tree eliminate_avail (basic_block, tree op);
  virtual void eliminate_push_avail (basic_block, tree op);
  tree eliminate_insert (basic_block, gimple_stmt_iterator *gsi, tree val);

  void eliminate_stmt (basic_block, gimple_stmt_iterator *);

  unsigned eliminate_cleanup (bool region_p = false);

  bool do_pre;
  unsigned int el_todo;
  unsigned int eliminations;
  unsigned int insertions;

  /* SSA names that had their defs inserted by PRE if do_pre.  */
  bitmap inserted_exprs;

  /* Blocks with statements that have had their EH properties changed.  */
  bitmap need_eh_cleanup;

  /* Blocks with statements that have had their AB properties changed.  */
  bitmap need_ab_cleanup;

  /* Local state for the eliminate domwalk.  */
  auto_vec<gimple *> to_remove;
  auto_vec<gimple *> to_fixup;
  auto_vec<tree> avail;
  auto_vec<tree> avail_stack;
};

eliminate_dom_walker::eliminate_dom_walker (cdi_direction direction,
					    bitmap inserted_exprs_)
  : dom_walker (direction), do_pre (inserted_exprs_ != NULL),
    el_todo (0), eliminations (0), insertions (0),
    inserted_exprs (inserted_exprs_)
{
  need_eh_cleanup = BITMAP_ALLOC (NULL);
  need_ab_cleanup = BITMAP_ALLOC (NULL);
}

eliminate_dom_walker::~eliminate_dom_walker ()
{
  BITMAP_FREE (need_eh_cleanup);
  BITMAP_FREE (need_ab_cleanup);
}

/* Return a leader for OP that is available at the current point of the
   eliminate domwalk.  */

tree
eliminate_dom_walker::eliminate_avail (basic_block, tree op)
{
  tree valnum = VN_INFO (op)->valnum;
  if (TREE_CODE (valnum) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (valnum))
	return valnum;
      if (avail.length () > SSA_NAME_VERSION (valnum))
	return avail[SSA_NAME_VERSION (valnum)];
    }
  else if (is_gimple_min_invariant (valnum))
    return valnum;
  return NULL_TREE;
}

/* At the current point of the eliminate domwalk make OP available.  */

void
eliminate_dom_walker::eliminate_push_avail (basic_block, tree op)
{
  tree valnum = VN_INFO (op)->valnum;
  if (TREE_CODE (valnum) == SSA_NAME)
    {
      if (avail.length () <= SSA_NAME_VERSION (valnum))
	avail.safe_grow_cleared (SSA_NAME_VERSION (valnum) + 1);
      tree pushop = op;
      if (avail[SSA_NAME_VERSION (valnum)])
	pushop = avail[SSA_NAME_VERSION (valnum)];
      avail_stack.safe_push (pushop);
      avail[SSA_NAME_VERSION (valnum)] = op;
    }
}

/* Insert the expression recorded by SCCVN for VAL at *GSI.  Returns
   the leader for the expression if insertion was successful.  */

tree
eliminate_dom_walker::eliminate_insert (basic_block bb,
					gimple_stmt_iterator *gsi, tree val)
{
  /* We can insert a sequence with a single assignment only.  */
  gimple_seq stmts = VN_INFO (val)->expr;
  if (!gimple_seq_singleton_p (stmts))
    return NULL_TREE;
  gassign *stmt = dyn_cast <gassign *> (gimple_seq_first_stmt (stmts));
  if (!stmt
      || (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
	  && gimple_assign_rhs_code (stmt) != VIEW_CONVERT_EXPR
	  && gimple_assign_rhs_code (stmt) != BIT_FIELD_REF
	  && (gimple_assign_rhs_code (stmt) != BIT_AND_EXPR
	      || TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST)))
    return NULL_TREE;

  tree op = gimple_assign_rhs1 (stmt);
  if (gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR
      || gimple_assign_rhs_code (stmt) == BIT_FIELD_REF)
    op = TREE_OPERAND (op, 0);
  tree leader = TREE_CODE (op) == SSA_NAME ? eliminate_avail (bb, op) : op;
  if (!leader)
    return NULL_TREE;

  tree res;
  stmts = NULL;
  if (gimple_assign_rhs_code (stmt) == BIT_FIELD_REF)
    res = gimple_build (&stmts, BIT_FIELD_REF,
			TREE_TYPE (val), leader,
			TREE_OPERAND (gimple_assign_rhs1 (stmt), 1),
			TREE_OPERAND (gimple_assign_rhs1 (stmt), 2));
  else if (gimple_assign_rhs_code (stmt) == BIT_AND_EXPR)
    res = gimple_build (&stmts, BIT_AND_EXPR,
			TREE_TYPE (val), leader, gimple_assign_rhs2 (stmt));
  else
    res = gimple_build (&stmts, gimple_assign_rhs_code (stmt),
			TREE_TYPE (val), leader);
  if (TREE_CODE (res) != SSA_NAME
      || SSA_NAME_IS_DEFAULT_DEF (res)
      || gimple_bb (SSA_NAME_DEF_STMT (res)))
    {
      gimple_seq_discard (stmts);

      /* During propagation we have to treat SSA info conservatively
         and thus we can end up simplifying the inserted expression
	 at elimination time to sth not defined in stmts.  */
      /* But then this is a redundancy we failed to detect.  Which means
         res now has two values.  That doesn't play well with how
	 we track availability here, so give up.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (TREE_CODE (res) == SSA_NAME)
	    res = eliminate_avail (bb, res);
	  if (res)
	    {
	      fprintf (dump_file, "Failed to insert expression for value ");
	      print_generic_expr (dump_file, val);
	      fprintf (dump_file, " which is really fully redundant to ");
	      print_generic_expr (dump_file, res);
	      fprintf (dump_file, "\n");
	    }
	}

      return NULL_TREE;
    }
  else
    {
      gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
      VN_INFO (res)->valnum = val;
      VN_INFO (res)->visited = true;
    }

  insertions++;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Inserted ");
      print_gimple_stmt (dump_file, SSA_NAME_DEF_STMT (res), 0);
    }

  return res;
}

void
eliminate_dom_walker::eliminate_stmt (basic_block b, gimple_stmt_iterator *gsi)
{
  tree sprime = NULL_TREE;
  gimple *stmt = gsi_stmt (*gsi);
  tree lhs = gimple_get_lhs (stmt);
  if (lhs && TREE_CODE (lhs) == SSA_NAME
      && !gimple_has_volatile_ops (stmt)
      /* See PR43491.  Do not replace a global register variable when
	 it is a the RHS of an assignment.  Do replace local register
	 variables since gcc does not guarantee a local variable will
	 be allocated in register.
	 ???  The fix isn't effective here.  This should instead
	 be ensured by not value-numbering them the same but treating
	 them like volatiles?  */
      && !(gimple_assign_single_p (stmt)
	   && (TREE_CODE (gimple_assign_rhs1 (stmt)) == VAR_DECL
	       && DECL_HARD_REGISTER (gimple_assign_rhs1 (stmt))
	       && is_global_var (gimple_assign_rhs1 (stmt)))))
    {
      sprime = eliminate_avail (b, lhs);
      if (!sprime)
	{
	  /* If there is no existing usable leader but SCCVN thinks
	     it has an expression it wants to use as replacement,
	     insert that.  */
	  tree val = VN_INFO (lhs)->valnum;
	  if (val != VN_TOP
	      && TREE_CODE (val) == SSA_NAME
	      && VN_INFO (val)->needs_insertion
	      && VN_INFO (val)->expr != NULL
	      && (sprime = eliminate_insert (b, gsi, val)) != NULL_TREE)
	    eliminate_push_avail (b, sprime);
	}

      /* If this now constitutes a copy duplicate points-to
	 and range info appropriately.  This is especially
	 important for inserted code.  See tree-ssa-copy.c
	 for similar code.  */
      if (sprime
	  && TREE_CODE (sprime) == SSA_NAME)
	{
	  basic_block sprime_b = gimple_bb (SSA_NAME_DEF_STMT (sprime));
	  if (POINTER_TYPE_P (TREE_TYPE (lhs))
	      && SSA_NAME_PTR_INFO (lhs)
	      && ! SSA_NAME_PTR_INFO (sprime))
	    {
	      duplicate_ssa_name_ptr_info (sprime,
					   SSA_NAME_PTR_INFO (lhs));
	      if (b != sprime_b)
		mark_ptr_info_alignment_unknown
		    (SSA_NAME_PTR_INFO (sprime));
	    }
	  else if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
		   && SSA_NAME_RANGE_INFO (lhs)
		   && ! SSA_NAME_RANGE_INFO (sprime)
		   && b == sprime_b)
	    duplicate_ssa_name_range_info (sprime,
					   SSA_NAME_RANGE_TYPE (lhs),
					   SSA_NAME_RANGE_INFO (lhs));
	}

      /* Inhibit the use of an inserted PHI on a loop header when
	 the address of the memory reference is a simple induction
	 variable.  In other cases the vectorizer won't do anything
	 anyway (either it's loop invariant or a complicated
	 expression).  */
      if (sprime
	  && TREE_CODE (sprime) == SSA_NAME
	  && do_pre
	  && (flag_tree_loop_vectorize || flag_tree_parallelize_loops > 1)
	  && loop_outer (b->loop_father)
	  && has_zero_uses (sprime)
	  && bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (sprime))
	  && gimple_assign_load_p (stmt))
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (sprime);
	  basic_block def_bb = gimple_bb (def_stmt);
	  if (gimple_code (def_stmt) == GIMPLE_PHI
	      && def_bb->loop_father->header == def_bb)
	    {
	      loop_p loop = def_bb->loop_father;
	      ssa_op_iter iter;
	      tree op;
	      bool found = false;
	      FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
		{
		  affine_iv iv;
		  def_bb = gimple_bb (SSA_NAME_DEF_STMT (op));
		  if (def_bb
		      && flow_bb_inside_loop_p (loop, def_bb)
		      && simple_iv (loop, loop, op, &iv, true))
		    {
		      found = true;
		      break;
		    }
		}
	      if (found)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Not replacing ");
		      print_gimple_expr (dump_file, stmt, 0);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, sprime);
		      fprintf (dump_file, " which would add a loop"
			       " carried dependence to loop %d\n",
			       loop->num);
		    }
		  /* Don't keep sprime available.  */
		  sprime = NULL_TREE;
		}
	    }
	}

      if (sprime)
	{
	  /* If we can propagate the value computed for LHS into
	     all uses don't bother doing anything with this stmt.  */
	  if (may_propagate_copy (lhs, sprime))
	    {
	      /* Mark it for removal.  */
	      to_remove.safe_push (stmt);

	      /* ???  Don't count copy/constant propagations.  */
	      if (gimple_assign_single_p (stmt)
		  && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
		      || gimple_assign_rhs1 (stmt) == sprime))
		return;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Replaced ");
		  print_gimple_expr (dump_file, stmt, 0);
		  fprintf (dump_file, " with ");
		  print_generic_expr (dump_file, sprime);
		  fprintf (dump_file, " in all uses of ");
		  print_gimple_stmt (dump_file, stmt, 0);
		}

	      eliminations++;
	      return;
	    }

	  /* If this is an assignment from our leader (which
	     happens in the case the value-number is a constant)
	     then there is nothing to do.  */
	  if (gimple_assign_single_p (stmt)
	      && sprime == gimple_assign_rhs1 (stmt))
	    return;

	  /* Else replace its RHS.  */
	  bool can_make_abnormal_goto
	      = is_gimple_call (stmt)
	      && stmt_can_make_abnormal_goto (stmt);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Replaced ");
	      print_gimple_expr (dump_file, stmt, 0);
	      fprintf (dump_file, " with ");
	      print_generic_expr (dump_file, sprime);
	      fprintf (dump_file, " in ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }

	  eliminations++;
	  gimple *orig_stmt = stmt;
	  if (!useless_type_conversion_p (TREE_TYPE (lhs),
					  TREE_TYPE (sprime)))
	    sprime = fold_convert (TREE_TYPE (lhs), sprime);
	  tree vdef = gimple_vdef (stmt);
	  tree vuse = gimple_vuse (stmt);
	  propagate_tree_value_into_stmt (gsi, sprime);
	  stmt = gsi_stmt (*gsi);
	  update_stmt (stmt);
	  /* In case the VDEF on the original stmt was released, value-number
	     it to the VUSE.  This is to make vuse_ssa_val able to skip
	     released virtual operands.  */
	  if (vdef != gimple_vdef (stmt))
	    {
	      gcc_assert (SSA_NAME_IN_FREE_LIST (vdef));
	      VN_INFO (vdef)->valnum = vuse;
	    }

	  /* If we removed EH side-effects from the statement, clean
	     its EH information.  */
	  if (maybe_clean_or_replace_eh_stmt (orig_stmt, stmt))
	    {
	      bitmap_set_bit (need_eh_cleanup,
			      gimple_bb (stmt)->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Removed EH side-effects.\n");
	    }

	  /* Likewise for AB side-effects.  */
	  if (can_make_abnormal_goto
	      && !stmt_can_make_abnormal_goto (stmt))
	    {
	      bitmap_set_bit (need_ab_cleanup,
			      gimple_bb (stmt)->index);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  Removed AB side-effects.\n");
	    }

	  return;
	}
    }

  /* If the statement is a scalar store, see if the expression
     has the same value number as its rhs.  If so, the store is
     dead.  */
  if (gimple_assign_single_p (stmt)
      && !gimple_has_volatile_ops (stmt)
      && !is_gimple_reg (gimple_assign_lhs (stmt))
      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
    {
      tree val;
      tree rhs = gimple_assign_rhs1 (stmt);
      vn_reference_t vnresult;
      val = vn_reference_lookup (lhs, gimple_vuse (stmt), VN_WALKREWRITE,
				 &vnresult, false);
      if (TREE_CODE (rhs) == SSA_NAME)
	rhs = VN_INFO (rhs)->valnum;
      if (val
	  && operand_equal_p (val, rhs, 0))
	{
	  /* We can only remove the later store if the former aliases
	     at least all accesses the later one does or if the store
	     was to readonly memory storing the same value.  */
	  alias_set_type set = get_alias_set (lhs);
	  if (! vnresult
	      || vnresult->set == set
	      || alias_set_subset_of (set, vnresult->set))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "Deleted redundant store ");
		  print_gimple_stmt (dump_file, stmt, 0);
		}

	      /* Queue stmt for removal.  */
	      to_remove.safe_push (stmt);
	      return;
	    }
	}
    }

  /* If this is a control statement value numbering left edges
     unexecuted on force the condition in a way consistent with
     that.  */
  if (gcond *cond = dyn_cast <gcond *> (stmt))
    {
      if ((EDGE_SUCC (b, 0)->flags & EDGE_EXECUTABLE)
	  ^ (EDGE_SUCC (b, 1)->flags & EDGE_EXECUTABLE))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Removing unexecutable edge from ");
	      print_gimple_stmt (dump_file, stmt, 0);
	    }
	  if (((EDGE_SUCC (b, 0)->flags & EDGE_TRUE_VALUE) != 0)
	      == ((EDGE_SUCC (b, 0)->flags & EDGE_EXECUTABLE) != 0))
	    gimple_cond_make_true (cond);
	  else
	    gimple_cond_make_false (cond);
	  update_stmt (cond);
	  el_todo |= TODO_cleanup_cfg;
	  return;
	}
    }

  bool can_make_abnormal_goto = stmt_can_make_abnormal_goto (stmt);
  bool was_noreturn = (is_gimple_call (stmt)
		       && gimple_call_noreturn_p (stmt));
  tree vdef = gimple_vdef (stmt);
  tree vuse = gimple_vuse (stmt);

  /* If we didn't replace the whole stmt (or propagate the result
     into all uses), replace all uses on this stmt with their
     leaders.  */
  bool modified = false;
  use_operand_p use_p;
  ssa_op_iter iter;
  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);
      /* ???  The call code above leaves stmt operands un-updated.  */
      if (TREE_CODE (use) != SSA_NAME)
	continue;
      tree sprime;
      if (SSA_NAME_IS_DEFAULT_DEF (use))
	/* ???  For default defs BB shouldn't matter, but we have to
	   solve the inconsistency between rpo eliminate and
	   dom eliminate avail valueization first.  */
	sprime = eliminate_avail (b, use);
      else
	/* Look for sth available at the definition block of the argument.
	   This avoids inconsistencies between availability there which
	   decides if the stmt can be removed and availability at the
	   use site.  The SSA property ensures that things available
	   at the definition are also available at uses.  */
	sprime = eliminate_avail (gimple_bb (SSA_NAME_DEF_STMT (use)), use);
      if (sprime && sprime != use
	  && may_propagate_copy (use, sprime)
	  /* We substitute into debug stmts to avoid excessive
	     debug temporaries created by removed stmts, but we need
	     to avoid doing so for inserted sprimes as we never want
	     to create debug temporaries for them.  */
	  && (!inserted_exprs
	      || TREE_CODE (sprime) != SSA_NAME
	      || !is_gimple_debug (stmt)
	      || !bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (sprime))))
	{
	  propagate_value (use_p, sprime);
	  modified = true;
	}
    }

  /* Fold the stmt if modified, this canonicalizes MEM_REFs we propagated
     into which is a requirement for the IPA devirt machinery.  */
  gimple *old_stmt = stmt;
  if (modified)
    {
      /* If a formerly non-invariant ADDR_EXPR is turned into an
	 invariant one it was on a separate stmt.  */
      if (gimple_assign_single_p (stmt)
	  && TREE_CODE (gimple_assign_rhs1 (stmt)) == ADDR_EXPR)
	recompute_tree_invariant_for_addr_expr (gimple_assign_rhs1 (stmt));
      gimple_stmt_iterator prev = *gsi;
      gsi_prev (&prev);
      if (fold_stmt (gsi))
	{
	  /* fold_stmt may have created new stmts inbetween
	     the previous stmt and the folded stmt.  Mark
	     all defs created there as varying to not confuse
	     the SCCVN machinery as we're using that even during
	     elimination.  */
	  if (gsi_end_p (prev))
	    prev = gsi_start_bb (b);
	  else
	    gsi_next (&prev);
	  if (gsi_stmt (prev) != gsi_stmt (*gsi))
	    do
	      {
		tree def;
		ssa_op_iter dit;
		FOR_EACH_SSA_TREE_OPERAND (def, gsi_stmt (prev),
					   dit, SSA_OP_ALL_DEFS)
		    /* As existing DEFs may move between stmts
		       only process new ones.  */
		    if (! has_VN_INFO (def))
		      {
			VN_INFO (def)->valnum = def;
			VN_INFO (def)->visited = true;
		      }
		if (gsi_stmt (prev) == gsi_stmt (*gsi))
		  break;
		gsi_next (&prev);
	      }
	    while (1);
	}
      stmt = gsi_stmt (*gsi);
      /* In case we folded the stmt away schedule the NOP for removal.  */
      if (gimple_nop_p (stmt))
	to_remove.safe_push (stmt);
    }

  /* Visit indirect calls and turn them into direct calls if
     possible using the devirtualization machinery.  Do this before
     checking for required EH/abnormal/noreturn cleanup as devird
     may expose more of those.  */
  if (gcall *call_stmt = dyn_cast <gcall *> (stmt))
    {
      tree fn = gimple_call_fn (call_stmt);
      if (fn
	  && flag_devirtualize
	  && virtual_method_call_p (fn))
	{
	  tree otr_type = obj_type_ref_class (fn);
	  unsigned HOST_WIDE_INT otr_tok
	      = tree_to_uhwi (OBJ_TYPE_REF_TOKEN (fn));
	  tree instance;
	  ipa_polymorphic_call_context context (current_function_decl,
						fn, stmt, &instance);
	  context.get_dynamic_type (instance, OBJ_TYPE_REF_OBJECT (fn),
				    otr_type, stmt);
	  bool final;
	  vec <cgraph_node *> targets
	      = possible_polymorphic_call_targets (obj_type_ref_class (fn),
						   otr_tok, context, &final);
	  if (dump_file)
	    dump_possible_polymorphic_call_targets (dump_file,
						    obj_type_ref_class (fn),
						    otr_tok, context);
	  if (final && targets.length () <= 1 && dbg_cnt (devirt))
	    {
	      tree fn;
	      if (targets.length () == 1)
		fn = targets[0]->decl;
	      else
		fn = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
				   "converting indirect call to "
				   "function %s\n",
				   lang_hooks.decl_printable_name (fn, 2));
		}
	      gimple_call_set_fndecl (call_stmt, fn);
	      /* If changing the call to __builtin_unreachable
		 or similar noreturn function, adjust gimple_call_fntype
		 too.  */
	      if (gimple_call_noreturn_p (call_stmt)
		  && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fn)))
		  && TYPE_ARG_TYPES (TREE_TYPE (fn))
		  && (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fn)))
		      == void_type_node))
		gimple_call_set_fntype (call_stmt, TREE_TYPE (fn));
	      maybe_remove_unused_call_args (cfun, call_stmt);
	      modified = true;
	    }
	}
    }

  if (modified)
    {
      /* When changing a call into a noreturn call, cfg cleanup
	 is needed to fix up the noreturn call.  */
      if (!was_noreturn
	  && is_gimple_call (stmt) && gimple_call_noreturn_p (stmt))
	to_fixup.safe_push  (stmt);
      /* When changing a condition or switch into one we know what
	 edge will be executed, schedule a cfg cleanup.  */
      if ((gimple_code (stmt) == GIMPLE_COND
	   && (gimple_cond_true_p (as_a <gcond *> (stmt))
	       || gimple_cond_false_p (as_a <gcond *> (stmt))))
	  || (gimple_code (stmt) == GIMPLE_SWITCH
	      && TREE_CODE (gimple_switch_index
			    (as_a <gswitch *> (stmt))) == INTEGER_CST))
	el_todo |= TODO_cleanup_cfg;
      /* If we removed EH side-effects from the statement, clean
	 its EH information.  */
      if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	{
	  bitmap_set_bit (need_eh_cleanup,
			  gimple_bb (stmt)->index);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Removed EH side-effects.\n");
	}
      /* Likewise for AB side-effects.  */
      if (can_make_abnormal_goto
	  && !stmt_can_make_abnormal_goto (stmt))
	{
	  bitmap_set_bit (need_ab_cleanup,
			  gimple_bb (stmt)->index);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Removed AB side-effects.\n");
	}
      update_stmt (stmt);
      /* In case the VDEF on the original stmt was released, value-number
         it to the VUSE.  This is to make vuse_ssa_val able to skip
	 released virtual operands.  */
      if (vdef && SSA_NAME_IN_FREE_LIST (vdef))
	VN_INFO (vdef)->valnum = vuse;
    }

  /* Make new values available - for fully redundant LHS we
     continue with the next stmt above and skip this.  */
  def_operand_p defp;
  FOR_EACH_SSA_DEF_OPERAND (defp, stmt, iter, SSA_OP_DEF)
    eliminate_push_avail (b, DEF_FROM_PTR (defp));
}

/* Perform elimination for the basic-block B during the domwalk.  */

edge
eliminate_dom_walker::before_dom_children (basic_block b)
{
  /* Mark new bb.  */
  avail_stack.safe_push (NULL_TREE);

  /* Skip unreachable blocks marked unreachable during the SCCVN domwalk.  */
  if (!(b->flags & BB_EXECUTABLE))
    return NULL;

  vn_context_bb = b;

  for (gphi_iterator gsi = gsi_start_phis (b); !gsi_end_p (gsi);)
    {
      gphi *phi = gsi.phi ();
      tree res = PHI_RESULT (phi);

      if (virtual_operand_p (res))
	{
	  gsi_next (&gsi);
	  continue;
	}

      tree sprime = eliminate_avail (b, res);
      if (sprime
	  && sprime != res)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Replaced redundant PHI node defining ");
	      print_generic_expr (dump_file, res);
	      fprintf (dump_file, " with ");
	      print_generic_expr (dump_file, sprime);
	      fprintf (dump_file, "\n");
	    }

	  /* If we inserted this PHI node ourself, it's not an elimination.  */
	  if (! inserted_exprs
	      || ! bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (res)))
	    eliminations++;

	  /* If we will propagate into all uses don't bother to do
	     anything.  */
	  if (may_propagate_copy (res, sprime))
	    {
	      /* Mark the PHI for removal.  */
	      to_remove.safe_push (phi);
	      gsi_next (&gsi);
	      continue;
	    }

	  remove_phi_node (&gsi, false);

	  if (!useless_type_conversion_p (TREE_TYPE (res), TREE_TYPE (sprime)))
	    sprime = fold_convert (TREE_TYPE (res), sprime);
	  gimple *stmt = gimple_build_assign (res, sprime);
	  gimple_stmt_iterator gsi2 = gsi_after_labels (b);
	  gsi_insert_before (&gsi2, stmt, GSI_NEW_STMT);
	  continue;
	}

      eliminate_push_avail (b, res);
      gsi_next (&gsi);
    }

  for (gimple_stmt_iterator gsi = gsi_start_bb (b);
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    eliminate_stmt (b, &gsi);

  /* Replace destination PHI arguments.  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, b->succs)
    if (e->flags & EDGE_EXECUTABLE)
      for (gphi_iterator gsi = gsi_start_phis (e->dest);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  tree arg = USE_FROM_PTR (use_p);
	  if (TREE_CODE (arg) != SSA_NAME
	      || virtual_operand_p (arg))
	    continue;
	  tree sprime = eliminate_avail (b, arg);
	  if (sprime && may_propagate_copy (arg, sprime))
	    propagate_value (use_p, sprime);
	}

  vn_context_bb = NULL;

  return NULL;
}

/* Make no longer available leaders no longer available.  */

void
eliminate_dom_walker::after_dom_children (basic_block)
{
  tree entry;
  while ((entry = avail_stack.pop ()) != NULL_TREE)
    {
      tree valnum = VN_INFO (entry)->valnum;
      tree old = avail[SSA_NAME_VERSION (valnum)];
      if (old == entry)
	avail[SSA_NAME_VERSION (valnum)] = NULL_TREE;
      else
	avail[SSA_NAME_VERSION (valnum)] = entry;
    }
}

/* Remove queued stmts and perform delayed cleanups.  */

unsigned
eliminate_dom_walker::eliminate_cleanup (bool region_p)
{
  statistics_counter_event (cfun, "Eliminated", eliminations);
  statistics_counter_event (cfun, "Insertions", insertions);

  /* We cannot remove stmts during BB walk, especially not release SSA
     names there as this confuses the VN machinery.  The stmts ending
     up in to_remove are either stores or simple copies.
     Remove stmts in reverse order to make debug stmt creation possible.  */
  while (!to_remove.is_empty ())
    {
      bool do_release_defs = true;
      gimple *stmt = to_remove.pop ();

      /* When we are value-numbering a region we do not require exit PHIs to
	 be present so we have to make sure to deal with uses outside of the
	 region of stmts that we thought are eliminated.
	 ??? Note we may be confused by uses in dead regions we didn't run
	 elimination on.  Rather than checking individual uses we accept
	 dead copies to be generated here (gcc.c-torture/execute/20060905-1.c
	 contains such example).  */
      if (region_p)
	{
	  if (gphi *phi = dyn_cast <gphi *> (stmt))
	    {
	      tree lhs = gimple_phi_result (phi);
	      if (!has_zero_uses (lhs))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Keeping eliminated stmt live "
			     "as copy because of out-of-region uses\n");
		  tree sprime = eliminate_avail (gimple_bb (stmt), lhs);
		  gimple *copy = gimple_build_assign (lhs, sprime);
		  gimple_stmt_iterator gsi
		    = gsi_after_labels (gimple_bb (stmt));
		  gsi_insert_before (&gsi, copy, GSI_SAME_STMT);
		  do_release_defs = false;
		}
	    }
	  else if (tree lhs = gimple_get_lhs (stmt))
	    if (TREE_CODE (lhs) == SSA_NAME
		&& !has_zero_uses (lhs))
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "Keeping eliminated stmt live "
			   "as copy because of out-of-region uses\n");
		tree sprime = eliminate_avail (gimple_bb (stmt), lhs);
		gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
		if (is_gimple_assign (stmt))
		  {
		    gimple_assign_set_rhs_from_tree (&gsi, sprime);
		    update_stmt (gsi_stmt (gsi));
		    continue;
		  }
		else
		  {
		    gimple *copy = gimple_build_assign (lhs, sprime);
		    gsi_insert_before (&gsi, copy, GSI_SAME_STMT);
		    do_release_defs = false;
		  }
	      }
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Removing dead stmt ");
	  print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);
	}

      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
      if (gimple_code (stmt) == GIMPLE_PHI)
	remove_phi_node (&gsi, do_release_defs);
      else
	{
	  basic_block bb = gimple_bb (stmt);
	  unlink_stmt_vdef (stmt);
	  if (gsi_remove (&gsi, true))
	    bitmap_set_bit (need_eh_cleanup, bb->index);
	  if (is_gimple_call (stmt) && stmt_can_make_abnormal_goto (stmt))
	    bitmap_set_bit (need_ab_cleanup, bb->index);
	  if (do_release_defs)
	    release_defs (stmt);
	}

      /* Removing a stmt may expose a forwarder block.  */
      el_todo |= TODO_cleanup_cfg;
    }

  /* Fixup stmts that became noreturn calls.  This may require splitting
     blocks and thus isn't possible during the dominator walk.  Do this
     in reverse order so we don't inadvertedly remove a stmt we want to
     fixup by visiting a dominating now noreturn call first.  */
  while (!to_fixup.is_empty ())
    {
      gimple *stmt = to_fixup.pop ();

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Fixing up noreturn call ");
	  print_gimple_stmt (dump_file, stmt, 0);
	}

      if (fixup_noreturn_call (stmt))
	el_todo |= TODO_cleanup_cfg;
    }

  bool do_eh_cleanup = !bitmap_empty_p (need_eh_cleanup);
  bool do_ab_cleanup = !bitmap_empty_p (need_ab_cleanup);

  if (do_eh_cleanup)
    gimple_purge_all_dead_eh_edges (need_eh_cleanup);

  if (do_ab_cleanup)
    gimple_purge_all_dead_abnormal_call_edges (need_ab_cleanup);

  if (do_eh_cleanup || do_ab_cleanup)
    el_todo |= TODO_cleanup_cfg;

  return el_todo;
}

/* Eliminate fully redundant computations.  */

unsigned
eliminate_with_rpo_vn (bitmap inserted_exprs)
{
  eliminate_dom_walker walker (CDI_DOMINATORS, inserted_exprs);

  walker.walk (cfun->cfg->x_entry_block_ptr);
  return walker.eliminate_cleanup ();
}

static unsigned
do_rpo_vn (function *fn, edge entry, bitmap exit_bbs,
	   bool iterate, bool eliminate);

void
run_rpo_vn (vn_lookup_kind kind)
{
  default_vn_walk_kind = kind;
  do_rpo_vn (cfun, NULL, NULL, true, false);

  /* ???  Prune requirement of these.  */
  constant_to_value_id = new hash_table<vn_constant_hasher> (23);
  constant_value_ids = BITMAP_ALLOC (NULL);

  /* Initialize the value ids and prune out remaining VN_TOPs
     from dead code.  */
  tree name;
  unsigned i;
  FOR_EACH_SSA_NAME (i, name, cfun)
    {
      vn_ssa_aux_t info = VN_INFO (name);
      if (!info->visited
	  || info->valnum == VN_TOP)
	info->valnum = name;
      if (info->valnum == name)
	info->value_id = get_next_value_id ();
      else if (is_gimple_min_invariant (info->valnum))
	info->value_id = get_or_alloc_constant_value_id (info->valnum);
    }

  /* Propagate.  */
  FOR_EACH_SSA_NAME (i, name, cfun)
    {
      vn_ssa_aux_t info = VN_INFO (name);
      if (TREE_CODE (info->valnum) == SSA_NAME
	  && info->valnum != name
	  && info->value_id != VN_INFO (info->valnum)->value_id)
	info->value_id = VN_INFO (info->valnum)->value_id;
    }

  set_hashtable_value_ids ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Value numbers:\n");
      FOR_EACH_SSA_NAME (i, name, cfun)
	{
	  if (VN_INFO (name)->visited
	      && SSA_VAL (name) != name)
	    {
	      print_generic_expr (dump_file, name);
	      fprintf (dump_file, " = ");
	      print_generic_expr (dump_file, SSA_VAL (name));
	      fprintf (dump_file, " (%04d)\n", VN_INFO (name)->value_id);
	    }
	}
    }
}

/* Free VN associated data structures.  */

void
free_rpo_vn (void)
{
  free_vn_table (valid_info);
  XDELETE (valid_info);
  obstack_free (&vn_tables_obstack, NULL);
  obstack_free (&vn_tables_insert_obstack, NULL);

  vn_ssa_aux_iterator_type it;
  vn_ssa_aux_t info;
  FOR_EACH_HASH_TABLE_ELEMENT (*vn_ssa_aux_hash, info, vn_ssa_aux_t, it)
    if (info->needs_insertion)
      release_ssa_name (info->name);
  obstack_free (&vn_ssa_aux_obstack, NULL);
  delete vn_ssa_aux_hash;

  delete constant_to_value_id;
  constant_to_value_id = NULL;
  BITMAP_FREE (constant_value_ids);
}

/* Adaptor to the elimination engine using RPO availability.  */

class rpo_elim : public eliminate_dom_walker
{
public:
  rpo_elim(basic_block entry_)
    : eliminate_dom_walker (CDI_DOMINATORS, NULL), entry (entry_) {}
  ~rpo_elim();

  virtual tree eliminate_avail (basic_block, tree op);

  virtual void eliminate_push_avail (basic_block, tree);

  basic_block entry;
  /* Instead of having a local availability lattice for each
     basic-block and availability at X defined as union of
     the local availabilities at X and its dominators we're
     turning this upside down and track availability per
     value given values are usually made available at very
     few points (at least one).
     So we have a value -> vec<location, leader> map where
     LOCATION is specifying the basic-block LEADER is made
     available for VALUE.  We push to this vector in RPO
     order thus for iteration we can simply pop the last
     entries.
     LOCATION is the basic-block index and LEADER is its
     SSA name version.  */
  /* ???  We'd like to use auto_vec here with embedded storage
     but that doesn't play well until we can provide move
     constructors and use std::move on hash-table expansion.
     So for now this is a bit more expensive than necessary.
     We eventually want to switch to a chaining scheme like
     for hashtable entries for unwinding which would make
     making the vector part of the vn_ssa_aux structure possible.  */
  typedef hash_map<tree, vec<std::pair<int, int> > > rpo_avail_t;
  rpo_avail_t m_rpo_avail;
};

/* Global RPO state for access from hooks.  */
static rpo_elim *rpo_avail;

/* Hook for maybe_push_res_to_seq, lookup the expression in the VN tables.  */

static tree
vn_lookup_simplify_result (gimple_match_op *res_op)
{
  if (!res_op->code.is_tree_code ())
    return NULL_TREE;
  tree *ops = res_op->ops;
  unsigned int length = res_op->num_ops;
  if (res_op->code == CONSTRUCTOR
      /* ???  We're arriving here with SCCVNs view, decomposed CONSTRUCTOR
         and GIMPLEs / match-and-simplifies, CONSTRUCTOR as GENERIC tree.  */
      && TREE_CODE (res_op->ops[0]) == CONSTRUCTOR)
    {
      length = CONSTRUCTOR_NELTS (res_op->ops[0]);
      ops = XALLOCAVEC (tree, length);
      for (unsigned i = 0; i < length; ++i)
	ops[i] = CONSTRUCTOR_ELT (res_op->ops[0], i)->value;
    }
  vn_nary_op_t vnresult = NULL;
  tree res = vn_nary_op_lookup_pieces (length, (tree_code) res_op->code,
				       res_op->type, ops, &vnresult);
  /* If this is used from expression simplification make sure to
     return an available expression.  */
  if (res && TREE_CODE (res) == SSA_NAME && mprts_hook && rpo_avail)
    res = rpo_avail->eliminate_avail (vn_context_bb, res);
  return res;
}

rpo_elim::~rpo_elim ()
{
  /* Release the avail vectors.  */
  for (rpo_avail_t::iterator i = m_rpo_avail.begin ();
       i != m_rpo_avail.end (); ++i)
    (*i).second.release ();
}

/* Return a leader for OPs value that is valid at BB.  */

tree
rpo_elim::eliminate_avail (basic_block bb, tree op)
{
  bool visited;
  tree valnum = SSA_VAL (op, &visited);
  /* If we didn't visit OP then it must be defined outside of the
     region we process and also dominate it.  So it is available.  */
  if (!visited)
    return op;
  if (TREE_CODE (valnum) == SSA_NAME)
    {
      if (SSA_NAME_IS_DEFAULT_DEF (valnum))
	return valnum;
      vec<std::pair<int, int> > *av = m_rpo_avail.get (valnum);
      if (!av || av->is_empty ())
	return NULL_TREE;
      int i = av->length () - 1;
      if ((*av)[i].first == bb->index)
	/* On tramp3d 90% of the cases are here.  */
	return ssa_name ((*av)[i].second);
      do
	{
	  basic_block abb = BASIC_BLOCK_FOR_FN (cfun, (*av)[i].first);
	  /* ???  During elimination we have to use availability at the
	     definition site of a use we try to replace.  This
	     is required to not run into inconsistencies because
	     of dominated_by_p_w_unex behavior and removing a definition
	     while not replacing all uses.
	     ???  We could try to consistently walk dominators
	     ignoring non-executable regions.  The nearest common
	     dominator of bb and abb is where we can stop walking.  We
	     may also be able to "pre-compute" (bits of) the next immediate
	     (non-)dominator during the RPO walk when marking edges as
	     executable.  */
	  if (dominated_by_p_w_unex (bb, abb))
	    {
	      tree leader = ssa_name ((*av)[i].second);
	      /* Prevent eliminations that break loop-closed SSA.  */
	      if (loops_state_satisfies_p (LOOP_CLOSED_SSA)
		  && ! SSA_NAME_IS_DEFAULT_DEF (leader)
		  && ! flow_bb_inside_loop_p (gimple_bb (SSA_NAME_DEF_STMT
							 (leader))->loop_father,
					      bb))
		return NULL_TREE;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  print_generic_expr (dump_file, leader);
		  fprintf (dump_file, " is available for ");
		  print_generic_expr (dump_file, valnum);
		  fprintf (dump_file, "\n");
		}
	      /* On tramp3d 99% of the _remaining_ cases succeed at
	         the first enty.  */
	      return leader;
	    }
	  /* ???  Can we somehow skip to the immediate dominator
	     RPO index (bb_to_rpo)?  Again, maybe not worth, on
	     tramp3d the worst number of elements in the vector is 9.  */
	}
      while (--i >= 0);
    }
  else if (valnum != VN_TOP)
    /* valnum is is_gimple_min_invariant.  */
    return valnum;
  return NULL_TREE;
}

/* Make LEADER a leader for its value at BB.  */

void
rpo_elim::eliminate_push_avail (basic_block bb, tree leader)
{
  tree valnum = VN_INFO (leader)->valnum;
  if (valnum == VN_TOP)
    return;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Making available beyond BB%d ", bb->index);
      print_generic_expr (dump_file, leader);
      fprintf (dump_file, " for value ");
      print_generic_expr (dump_file, valnum);
      fprintf (dump_file, "\n");
    }
  bool existed;
  vec<std::pair<int, int> > &av = m_rpo_avail.get_or_insert (valnum, &existed);
  if (!existed)
    {
      new (&av) vec<std::pair<int, int> >;
      av.reserve_exact (2);
    }
  av.safe_push (std::make_pair (bb->index, SSA_NAME_VERSION (leader)));
}

/* Valueization hook for RPO VN plus required state.  */

tree
rpo_vn_valueize (tree name)
{
  if (TREE_CODE (name) == SSA_NAME)
    {
      vn_ssa_aux_t val = VN_INFO (name);
      if (val)
	{
	  tree tem = val->valnum;
	  if (tem != VN_TOP && tem != name)
	    {
	      if (TREE_CODE (tem) != SSA_NAME)
		return tem;
	      /* For all values we only valueize to an available leader
		 which means we can use SSA name info without restriction.  */
	      tem = rpo_avail->eliminate_avail (vn_context_bb, tem);
	      if (tem)
		return tem;
	    }
	}
    }
  return name;
}

/* Insert on PRED_E predicates derived from CODE OPS being true besides the
   inverted condition.  */

static void
insert_related_predicates_on_edge (enum tree_code code, tree *ops, edge pred_e)
{
  switch (code)
    {
    case LT_EXPR:
      /* a < b -> a {!,<}= b */
      vn_nary_op_insert_pieces_predicated (2, NE_EXPR, boolean_type_node,
					   ops, boolean_true_node, 0, pred_e);
      vn_nary_op_insert_pieces_predicated (2, LE_EXPR, boolean_type_node,
					   ops, boolean_true_node, 0, pred_e);
      /* a < b -> ! a {>,=} b */
      vn_nary_op_insert_pieces_predicated (2, GT_EXPR, boolean_type_node,
					   ops, boolean_false_node, 0, pred_e);
      vn_nary_op_insert_pieces_predicated (2, EQ_EXPR, boolean_type_node,
					   ops, boolean_false_node, 0, pred_e);
      break;
    case GT_EXPR:
      /* a > b -> a {!,>}= b */
      vn_nary_op_insert_pieces_predicated (2, NE_EXPR, boolean_type_node,
					   ops, boolean_true_node, 0, pred_e);
      vn_nary_op_insert_pieces_predicated (2, GE_EXPR, boolean_type_node,
					   ops, boolean_true_node, 0, pred_e);
      /* a > b -> ! a {<,=} b */
      vn_nary_op_insert_pieces_predicated (2, LT_EXPR, boolean_type_node,
					   ops, boolean_false_node, 0, pred_e);
      vn_nary_op_insert_pieces_predicated (2, EQ_EXPR, boolean_type_node,
					   ops, boolean_false_node, 0, pred_e);
      break;
    case EQ_EXPR:
      /* a == b -> ! a {<,>} b */
      vn_nary_op_insert_pieces_predicated (2, LT_EXPR, boolean_type_node,
					   ops, boolean_false_node, 0, pred_e);
      vn_nary_op_insert_pieces_predicated (2, GT_EXPR, boolean_type_node,
					   ops, boolean_false_node, 0, pred_e);
      break;
    case LE_EXPR:
    case GE_EXPR:
    case NE_EXPR:
      /* Nothing besides inverted condition.  */
      break;
    default:;
    }
}

/* Main stmt worker for RPO VN, process BB.  */

static unsigned
process_bb (rpo_elim &avail, basic_block bb,
	    bool bb_visited, bool iterate_phis, bool iterate, bool eliminate,
	    bool do_region, bitmap exit_bbs)
{
  unsigned todo = 0;
  edge_iterator ei;
  edge e;

  vn_context_bb = bb;

  /* If we are in loop-closed SSA preserve this state.  This is
     relevant when called on regions from outside of FRE/PRE.  */
  bool lc_phi_nodes = false;
  if (loops_state_satisfies_p (LOOP_CLOSED_SSA))
    FOR_EACH_EDGE (e, ei, bb->preds)
      if (e->src->loop_father != e->dest->loop_father
	  && flow_loop_nested_p (e->dest->loop_father,
				 e->src->loop_father))
	{
	  lc_phi_nodes = true;
	  break;
	}

  /* Value-number all defs in the basic-block.  */
  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      tree res = PHI_RESULT (phi);
      vn_ssa_aux_t res_info = VN_INFO (res);
      if (!bb_visited)
	{
	  gcc_assert (!res_info->visited);
	  res_info->valnum = VN_TOP;
	  res_info->visited = true;
	}

      /* When not iterating force backedge values to varying.  */
      visit_stmt (phi, !iterate_phis);
      if (virtual_operand_p (res))
	continue;

      /* Eliminate */
      /* The interesting case is gcc.dg/tree-ssa/pr22230.c for correctness
	 how we handle backedges and availability.
	 And gcc.dg/tree-ssa/ssa-sccvn-2.c for optimization.  */
      tree val = res_info->valnum;
      if (res != val && !iterate && eliminate)
	{
	  if (tree leader = avail.eliminate_avail (bb, res))
	    {
	      if (leader != res
		  /* Preserve loop-closed SSA form.  */
		  && (! lc_phi_nodes
		      || is_gimple_min_invariant (leader)))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "Replaced redundant PHI node "
			       "defining ");
		      print_generic_expr (dump_file, res);
		      fprintf (dump_file, " with ");
		      print_generic_expr (dump_file, leader);
		      fprintf (dump_file, "\n");
		    }
		  avail.eliminations++;

		  if (may_propagate_copy (res, leader))
		    {
		      /* Schedule for removal.  */
		      avail.to_remove.safe_push (phi);
		      continue;
		    }
		  /* ???  Else generate a copy stmt.  */
		}
	    }
	}
      /* Only make defs available that not already are.  But make
	 sure loop-closed SSA PHI node defs are picked up for
	 downstream uses.  */
      if (lc_phi_nodes
	  || res == val
	  || ! avail.eliminate_avail (bb, res))
	avail.eliminate_push_avail (bb, res);
    }

  /* For empty BBs mark outgoing edges executable.  For non-empty BBs
     we do this when processing the last stmt as we have to do this
     before elimination which otherwise forces GIMPLE_CONDs to
     if (1 != 0) style when seeing non-executable edges.  */
  if (gsi_end_p (gsi_start_bb (bb)))
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if (e->flags & EDGE_EXECUTABLE)
	    continue;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "marking outgoing edge %d -> %d executable\n",
		     e->src->index, e->dest->index);
	  gcc_checking_assert (iterate || !(e->flags & EDGE_DFS_BACK));
	  e->flags |= EDGE_EXECUTABLE;
	  e->dest->flags |= BB_EXECUTABLE;
	}
    }
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      ssa_op_iter i;
      tree op;
      if (!bb_visited)
	{
	  FOR_EACH_SSA_TREE_OPERAND (op, gsi_stmt (gsi), i, SSA_OP_ALL_DEFS)
	    {
	      vn_ssa_aux_t op_info = VN_INFO (op);
	      gcc_assert (!op_info->visited);
	      op_info->valnum = VN_TOP;
	      op_info->visited = true;
	    }

	  /* We somehow have to deal with uses that are not defined
	     in the processed region.  Forcing unvisited uses to
	     varying here doesn't play well with def-use following during
	     expression simplification, so we deal with this by checking
	     the visited flag in SSA_VAL.  */
	}

      visit_stmt (gsi_stmt (gsi));

      gimple *last = gsi_stmt (gsi);
      e = NULL;
      switch (gimple_code (last))
	{
	case GIMPLE_SWITCH:
	  e = find_taken_edge (bb, vn_valueize (gimple_switch_index
						(as_a <gswitch *> (last))));
	  break;
	case GIMPLE_COND:
	  {
	    tree lhs = vn_valueize (gimple_cond_lhs (last));
	    tree rhs = vn_valueize (gimple_cond_rhs (last));
	    tree val = gimple_simplify (gimple_cond_code (last),
					boolean_type_node, lhs, rhs,
					NULL, vn_valueize);
	    /* If the condition didn't simplfy see if we have recorded
	       an expression from sofar taken edges.  */
	    if (! val || TREE_CODE (val) != INTEGER_CST)
	      {
		vn_nary_op_t vnresult;
		tree ops[2];
		ops[0] = lhs;
		ops[1] = rhs;
		val = vn_nary_op_lookup_pieces (2, gimple_cond_code (last),
						boolean_type_node, ops,
						&vnresult);
		/* Did we get a predicated value?  */
		if (! val && vnresult && vnresult->predicated_values)
		  {
		    val = vn_nary_op_get_predicated_value (vnresult, bb);
		    if (val && dump_file && (dump_flags & TDF_DETAILS))
		      {
			fprintf (dump_file, "Got predicated value ");
			print_generic_expr (dump_file, val, TDF_NONE);
			fprintf (dump_file, " for ");
			print_gimple_stmt (dump_file, last, TDF_SLIM);
		      }
		  }
	      }
	    if (val)
	      e = find_taken_edge (bb, val);
	    if (! e)
	      {
		/* If we didn't manage to compute the taken edge then
		   push predicated expressions for the condition itself
		   and related conditions to the hashtables.  This allows
		   simplification of redundant conditions which is
		   important as early cleanup.  */
		edge true_e, false_e;
		extract_true_false_edges_from_block (bb, &true_e, &false_e);
		enum tree_code code = gimple_cond_code (last);
		enum tree_code icode
		  = invert_tree_comparison (code, HONOR_NANS (lhs));
		tree ops[2];
		ops[0] = lhs;
		ops[1] = rhs;
		if (do_region
		    && bitmap_bit_p (exit_bbs, true_e->dest->index))
		  true_e = NULL;
		if (do_region
		    && bitmap_bit_p (exit_bbs, false_e->dest->index))
		  false_e = NULL;
		if (true_e)
		  vn_nary_op_insert_pieces_predicated
		    (2, code, boolean_type_node, ops,
		     boolean_true_node, 0, true_e);
		if (false_e)
		  vn_nary_op_insert_pieces_predicated
		    (2, code, boolean_type_node, ops,
		     boolean_false_node, 0, false_e);
		if (icode != ERROR_MARK)
		  {
		    if (true_e)
		      vn_nary_op_insert_pieces_predicated
			(2, icode, boolean_type_node, ops,
			 boolean_false_node, 0, true_e);
		    if (false_e)
		      vn_nary_op_insert_pieces_predicated
			(2, icode, boolean_type_node, ops,
			 boolean_true_node, 0, false_e);
		  }
		/* Relax for non-integers, inverted condition handled
		   above.  */
		if (INTEGRAL_TYPE_P (TREE_TYPE (lhs)))
		  {
		    if (true_e)
		      insert_related_predicates_on_edge (code, ops, true_e);
		    if (false_e)
		      insert_related_predicates_on_edge (icode, ops, false_e);
		  }
	      }
	    break;
	  }
	case GIMPLE_GOTO:
	  e = find_taken_edge (bb, vn_valueize (gimple_goto_dest (last)));
	  break;
	default:
	  e = NULL;
	}
      if (e)
	{
	  todo = TODO_cleanup_cfg;
	  if (!(e->flags & EDGE_EXECUTABLE))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "marking known outgoing %sedge %d -> %d executable\n",
			 e->flags & EDGE_DFS_BACK ? "back-" : "",
			 e->src->index, e->dest->index);
	      gcc_checking_assert (iterate || !(e->flags & EDGE_DFS_BACK));
	      e->flags |= EDGE_EXECUTABLE;
	      e->dest->flags |= BB_EXECUTABLE;
	    }
	}
      else if (gsi_one_before_end_p (gsi))
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (e->flags & EDGE_EXECUTABLE)
		continue;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "marking outgoing edge %d -> %d executable\n",
			 e->src->index, e->dest->index);
	      gcc_checking_assert (iterate || !(e->flags & EDGE_DFS_BACK));
	      e->flags |= EDGE_EXECUTABLE;
	      e->dest->flags |= BB_EXECUTABLE;
	    }
	}

      /* Eliminate.  That also pushes to avail.  */
      if (eliminate && ! iterate)
	avail.eliminate_stmt (bb, &gsi);
      else
	/* If not eliminating, make all not already available defs
	   available.  */
	FOR_EACH_SSA_TREE_OPERAND (op, gsi_stmt (gsi), i, SSA_OP_DEF)
	  if (! avail.eliminate_avail (bb, op))
	    avail.eliminate_push_avail (bb, op);
    }

  /* Eliminate in destination PHI arguments.  Always substitute in dest
     PHIs, even for non-executable edges.  This handles region
     exits PHIs.  */
  if (!iterate && eliminate)
    FOR_EACH_EDGE (e, ei, bb->succs)
      for (gphi_iterator gsi = gsi_start_phis (e->dest);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  use_operand_p use_p = PHI_ARG_DEF_PTR_FROM_EDGE (phi, e);
	  tree arg = USE_FROM_PTR (use_p);
	  if (TREE_CODE (arg) != SSA_NAME
	      || virtual_operand_p (arg))
	    continue;
	  tree sprime;
	  if (SSA_NAME_IS_DEFAULT_DEF (arg))
	    {
	      sprime = SSA_VAL (arg);
	      gcc_assert (TREE_CODE (sprime) != SSA_NAME
			  || SSA_NAME_IS_DEFAULT_DEF (sprime));
	    }
	  else
	    /* Look for sth available at the definition block of the argument.
	       This avoids inconsistencies between availability there which
	       decides if the stmt can be removed and availability at the
	       use site.  The SSA property ensures that things available
	       at the definition are also available at uses.  */
	    sprime = avail.eliminate_avail (gimple_bb (SSA_NAME_DEF_STMT (arg)),
					    arg);
	  if (sprime
	      && sprime != arg
	      && may_propagate_copy (arg, sprime))
	    propagate_value (use_p, sprime);
	}

  vn_context_bb = NULL;
  return todo;
}

/* Unwind state per basic-block.  */

struct unwind_state
{
  /* Times this block has been visited.  */
  unsigned visited;
  /* Whether to handle this as iteration point or whether to treat
     incoming backedge PHI values as varying.  */
  bool iterate;
  void *ob_top;
  vn_reference_t ref_top;
  vn_phi_t phi_top;
  vn_nary_op_t nary_top;
};

/* Unwind the RPO VN state for iteration.  */

static void
do_unwind (unwind_state *to, int rpo_idx, rpo_elim &avail, int *bb_to_rpo)
{
  gcc_assert (to->iterate);
  for (; last_inserted_nary != to->nary_top;
       last_inserted_nary = last_inserted_nary->next)
    {
      vn_nary_op_t *slot;
      slot = valid_info->nary->find_slot_with_hash
	(last_inserted_nary, last_inserted_nary->hashcode, NO_INSERT);
      /* Predication causes the need to restore previous state.  */
      if ((*slot)->unwind_to)
	*slot = (*slot)->unwind_to;
      else
	valid_info->nary->clear_slot (slot);
    }
  for (; last_inserted_phi != to->phi_top;
       last_inserted_phi = last_inserted_phi->next)
    {
      vn_phi_t *slot;
      slot = valid_info->phis->find_slot_with_hash
	(last_inserted_phi, last_inserted_phi->hashcode, NO_INSERT);
      valid_info->phis->clear_slot (slot);
    }
  for (; last_inserted_ref != to->ref_top;
       last_inserted_ref = last_inserted_ref->next)
    {
      vn_reference_t *slot;
      slot = valid_info->references->find_slot_with_hash
	(last_inserted_ref, last_inserted_ref->hashcode, NO_INSERT);
      (*slot)->operands.release ();
      valid_info->references->clear_slot (slot);
    }
  obstack_free (&vn_tables_obstack, to->ob_top);

  /* Prune [rpo_idx, ] from avail.  */
  /* ???  This is O(number-of-values-in-region) which is
     O(region-size) rather than O(iteration-piece).  */
  for (rpo_elim::rpo_avail_t::iterator i
       = avail.m_rpo_avail.begin ();
       i != avail.m_rpo_avail.end (); ++i)
    {
      while (! (*i).second.is_empty ())
	{
	  if (bb_to_rpo[(*i).second.last ().first] < rpo_idx)
	    break;
	  (*i).second.pop ();
	}
    }
}

/* Do VN on a SEME region specified by ENTRY and EXIT_BBS in FN.
   If ITERATE is true then treat backedges optimistically as not
   executed and iterate.  If ELIMINATE is true then perform
   elimination, otherwise leave that to the caller.  */

static unsigned
do_rpo_vn (function *fn, edge entry, bitmap exit_bbs,
	   bool iterate, bool eliminate)
{
  unsigned todo = 0;

  /* We currently do not support region-based iteration when
     elimination is requested.  */
  gcc_assert (!entry || !iterate || !eliminate);
  /* When iterating we need loop info up-to-date.  */
  gcc_assert (!iterate || !loops_state_satisfies_p (LOOPS_NEED_FIXUP));

  bool do_region = entry != NULL;
  if (!do_region)
    {
      entry = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (fn));
      exit_bbs = BITMAP_ALLOC (NULL);
      bitmap_set_bit (exit_bbs, EXIT_BLOCK);
    }

  int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fn) - NUM_FIXED_BLOCKS);
  int n = rev_post_order_and_mark_dfs_back_seme (fn, entry, exit_bbs,
						 iterate, rpo);
  /* rev_post_order_and_mark_dfs_back_seme fills RPO in reverse order.  */
  for (int i = 0; i < n / 2; ++i)
    std::swap (rpo[i], rpo[n-i-1]);

  if (!do_region)
    BITMAP_FREE (exit_bbs);

  int *bb_to_rpo = XNEWVEC (int, last_basic_block_for_fn (fn));
  for (int i = 0; i < n; ++i)
    bb_to_rpo[rpo[i]] = i;

  unwind_state *rpo_state = XNEWVEC (unwind_state, n);

  rpo_elim avail (entry->dest);
  rpo_avail = &avail;

  /* Verify we have no extra entries into the region.  */
  if (flag_checking && do_region)
    {
      auto_bb_flag bb_in_region (fn);
      for (int i = 0; i < n; ++i)
	{
	  basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[i]);
	  bb->flags |= bb_in_region;
	}
      /* We can't merge the first two loops because we cannot rely
         on EDGE_DFS_BACK for edges not within the region.  But if
	 we decide to always have the bb_in_region flag we can
	 do the checking during the RPO walk itself (but then it's
	 also easy to handle MEME conservatively).  */
      for (int i = 0; i < n; ++i)
	{
	  basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[i]);
	  edge e;
	  edge_iterator ei;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    gcc_assert (e == entry || (e->src->flags & bb_in_region));
	}
      for (int i = 0; i < n; ++i)
	{
	  basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[i]);
	  bb->flags &= ~bb_in_region;
	}
    }

  /* Create the VN state.  For the initial size of the various hashtables
     use a heuristic based on region size and number of SSA names.  */
  unsigned region_size = (((unsigned HOST_WIDE_INT)n * num_ssa_names)
			  / (n_basic_blocks_for_fn (fn) - NUM_FIXED_BLOCKS));
  VN_TOP = create_tmp_var_raw (void_type_node, "vn_top");

  vn_ssa_aux_hash = new hash_table <vn_ssa_aux_hasher> (region_size * 2);
  gcc_obstack_init (&vn_ssa_aux_obstack);

  gcc_obstack_init (&vn_tables_obstack);
  gcc_obstack_init (&vn_tables_insert_obstack);
  valid_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (valid_info, region_size);
  last_inserted_ref = NULL;
  last_inserted_phi = NULL;
  last_inserted_nary = NULL;

  vn_valueize = rpo_vn_valueize;

  /* Initialize the unwind state and edge/BB executable state.  */
  for (int i = 0; i < n; ++i)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[i]);
      rpo_state[i].visited = 0;
      bb->flags &= ~BB_EXECUTABLE;
      bool has_backedges = false;
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->flags & EDGE_DFS_BACK)
	    has_backedges = true;
	  if (! iterate && (e->flags & EDGE_DFS_BACK))
	    {
	      e->flags |= EDGE_EXECUTABLE;
	      /* ???  Strictly speaking we only need to unconditionally
		 process a block when it is in an irreducible region,
		 thus when it may be reachable via the backedge only.  */
	      bb->flags |= BB_EXECUTABLE;
	    }
	  else
	    e->flags &= ~EDGE_EXECUTABLE;
	}
      rpo_state[i].iterate = iterate && has_backedges;
    }
  entry->flags |= EDGE_EXECUTABLE;
  entry->dest->flags |= BB_EXECUTABLE;

  /* As heuristic to improve compile-time we handle only the N innermost
     loops and the outermost one optimistically.  */
  if (iterate)
    {
      loop_p loop;
      unsigned max_depth = PARAM_VALUE (PARAM_RPO_VN_MAX_LOOP_DEPTH);
      FOR_EACH_LOOP (loop, LI_ONLY_INNERMOST)
	if (loop_depth (loop) > max_depth)
	  for (unsigned i = 2;
	       i < loop_depth (loop) - max_depth; ++i)
	    {
	      basic_block header = superloop_at_depth (loop, i)->header;
	      bool non_latch_backedge = false;
	      edge e;
	      edge_iterator ei;
	      FOR_EACH_EDGE (e, ei, header->preds)
		if (e->flags & EDGE_DFS_BACK)
		  {
		    e->flags |= EDGE_EXECUTABLE;
		    e->dest->flags |= BB_EXECUTABLE;
		    /* There can be a non-latch backedge into the header
		       which is part of an outer irreducible region.  We
		       cannot avoid iterating this block then.  */
		    if (!dominated_by_p (CDI_DOMINATORS,
					 e->src, e->dest))
		      {
			if (dump_file && (dump_flags & TDF_DETAILS))
			  fprintf (dump_file, "non-latch backedge %d -> %d "
				   "forces iteration of loop %d\n",
				   e->src->index, e->dest->index, loop->num);
			non_latch_backedge = true;
		      }
		  }
	      rpo_state[bb_to_rpo[header->index]].iterate = non_latch_backedge;
	    }
    }

  /* Go and process all blocks, iterating as necessary.  */
  int idx = 0;
  uint64_t nblk = 0;
  do
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[idx]);

      /* If the block has incoming backedges remember unwind state.  This
         is required even for non-executable blocks since in irreducible
	 regions we might reach them via the backedge and re-start iterating
	 from there.
	 Note we can individually mark blocks with incoming backedges to
	 not iterate where we then handle PHIs conservatively.  We do that
	 heuristically to reduce compile-time for degenerate cases.  */
      if (rpo_state[idx].iterate)
	{
	  rpo_state[idx].ob_top = obstack_alloc (&vn_tables_obstack, 0);
	  rpo_state[idx].ref_top = last_inserted_ref;
	  rpo_state[idx].phi_top = last_inserted_phi;
	  rpo_state[idx].nary_top = last_inserted_nary;
	}

      if (!(bb->flags & BB_EXECUTABLE))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Block %d: BB%d found not executable\n",
		     idx, bb->index);
	  idx++;
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Processing block %d: BB%d\n", idx, bb->index);
      nblk++;
      todo |= process_bb (avail, bb,
			  rpo_state[idx].visited != 0,
			  rpo_state[idx].iterate,
			  iterate, eliminate, do_region, exit_bbs);
      rpo_state[idx].visited++;

      if (iterate)
	{
	  /* Verify if changed values flow over executable outgoing backedges
	     and those change destination PHI values (that's the thing we
	     can easily verify).  Reduce over all such edges to the farthest
	     away PHI.  */
	  int iterate_to = -1;
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if ((e->flags & (EDGE_DFS_BACK|EDGE_EXECUTABLE))
		== (EDGE_DFS_BACK|EDGE_EXECUTABLE)
		&& rpo_state[bb_to_rpo[e->dest->index]].iterate)
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "Looking for changed values of backedge "
			   "%d->%d destination PHIs\n",
			   e->src->index, e->dest->index);
		vn_context_bb = e->dest;
		gphi_iterator gsi;
		for (gsi = gsi_start_phis (e->dest);
		     !gsi_end_p (gsi); gsi_next (&gsi))
		  {
		    bool inserted = false;
		    /* While we'd ideally just iterate on value changes
		       we CSE PHIs and do that even across basic-block
		       boundaries.  So even hashtable state changes can
		       be important (which is roughly equivalent to
		       PHI argument value changes).  To not excessively
		       iterate because of that we track whether a PHI
		       was CSEd to with GF_PLF_1.  */
		    bool phival_changed;
		    if ((phival_changed = visit_phi (gsi.phi (),
						     &inserted, false))
			|| (inserted && gimple_plf (gsi.phi (), GF_PLF_1)))
		      {
			if (!phival_changed
			    && dump_file && (dump_flags & TDF_DETAILS))
			  fprintf (dump_file, "PHI was CSEd and hashtable "
				   "state (changed)\n");
			int destidx = bb_to_rpo[e->dest->index];
			if (iterate_to == -1
			    || destidx < iterate_to)
			  iterate_to = destidx;
			break;
		      }
		  }
		vn_context_bb = NULL;
	      }
	  if (iterate_to != -1)
	    {
	      do_unwind (&rpo_state[iterate_to], iterate_to,
			 avail, bb_to_rpo);
	      idx = iterate_to;
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Iterating to %d BB%d\n",
			 iterate_to, rpo[iterate_to]);
	      continue;
	    }
	}

      idx++;
    }
  while (idx < n);

  /* If statistics or dump file active.  */
  int nex = 0;
  unsigned max_visited = 1;
  for (int i = 0; i < n; ++i)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[i]);
      if (bb->flags & BB_EXECUTABLE)
	nex++;
      statistics_histogram_event (cfun, "RPO block visited times",
				  rpo_state[i].visited);
      if (rpo_state[i].visited > max_visited)
	max_visited = rpo_state[i].visited;
    }
  unsigned nvalues = 0, navail = 0;
  for (rpo_elim::rpo_avail_t::iterator i = avail.m_rpo_avail.begin ();
       i != avail.m_rpo_avail.end (); ++i)
    {
      nvalues++;
      navail += (*i).second.length ();
    }
  statistics_counter_event (cfun, "RPO blocks", n);
  statistics_counter_event (cfun, "RPO blocks visited", nblk);
  statistics_counter_event (cfun, "RPO blocks executable", nex);
  statistics_histogram_event (cfun, "RPO iterations", 10*nblk / nex);
  statistics_histogram_event (cfun, "RPO num values", nvalues);
  statistics_histogram_event (cfun, "RPO num avail", navail);
  statistics_histogram_event (cfun, "RPO num lattice",
			      vn_ssa_aux_hash->elements ());
  if (dump_file && (dump_flags & (TDF_DETAILS|TDF_STATS)))
    {
      fprintf (dump_file, "RPO iteration over %d blocks visited %" PRIu64
	       " blocks in total discovering %d executable blocks iterating "
	       "%d.%d times, a block was visited max. %u times\n",
	       n, nblk, nex,
	       (int)((10*nblk / nex)/10), (int)((10*nblk / nex)%10),
	       max_visited);
      fprintf (dump_file, "RPO tracked %d values available at %d locations "
	       "and %" PRIu64 " lattice elements\n",
	       nvalues, navail, (uint64_t) vn_ssa_aux_hash->elements ());
    }

  if (eliminate)
    {
      /* When !iterate we already performed elimination during the RPO
         walk.  */
      if (iterate)
	{
	  /* Elimination for region-based VN needs to be done within the
	     RPO walk.  */
	  gcc_assert (! do_region);
	  /* Note we can't use avail.walk here because that gets confused
	     by the existing availability and it will be less efficient
	     as well.  */
	  todo |= eliminate_with_rpo_vn (NULL);
	}
      else
	todo |= avail.eliminate_cleanup (do_region);
    }

  vn_valueize = NULL;
  rpo_avail = NULL;

  XDELETEVEC (bb_to_rpo);
  XDELETEVEC (rpo);

  return todo;
}

/* Region-based entry for RPO VN.  Performs value-numbering and elimination
   on the SEME region specified by ENTRY and EXIT_BBS.  */

unsigned
do_rpo_vn (function *fn, edge entry, bitmap exit_bbs)
{
  default_vn_walk_kind = VN_WALKREWRITE;
  unsigned todo = do_rpo_vn (fn, entry, exit_bbs, false, true);
  free_rpo_vn ();
  return todo;
}


namespace {

const pass_data pass_data_fre =
{
  GIMPLE_PASS, /* type */
  "fre", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_FRE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_fre : public gimple_opt_pass
{
public:
  pass_fre (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_fre, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_fre (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_fre != 0; }
  virtual unsigned int execute (function *);

}; // class pass_fre

unsigned int
pass_fre::execute (function *fun)
{
  unsigned todo = 0;

  /* At -O[1g] use the cheap non-iterating mode.  */
  calculate_dominance_info (CDI_DOMINATORS);
  if (optimize > 1)
    loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  default_vn_walk_kind = VN_WALKREWRITE;
  todo = do_rpo_vn (fun, NULL, NULL, optimize > 1, true);
  free_rpo_vn ();

  if (optimize > 1)
    loop_optimizer_finalize ();

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_fre (gcc::context *ctxt)
{
  return new pass_fre (ctxt);
}

#undef BB_EXECUTABLE
