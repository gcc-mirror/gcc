/* SCC value numbering for trees
   Copyright (C) 2006-2025 Free Software Foundation, Inc.
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
#include "splay-tree-utils.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "cfganal.h"
#include "tree-inline.h"
#include "internal-fn.h"
#include "gimple-iterator.h"
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
#include "tree-ssa-propagate.h"
#include "tree-cfg.h"
#include "domwalk.h"
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
#include "tree-ssa-loop-niter.h"
#include "builtins.h"
#include "fold-const-call.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "tree-ssa-sccvn.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "target.h"

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
	  && expressions_equal_p (vro1->op2, vro2->op2)
	  && (vro1->opcode != CALL_EXPR || vro1->clique == vro2->clique));
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

/* Pretty-print OPS to OUTFILE.  */

void
print_vn_reference_ops (FILE *outfile, const vec<vn_reference_op_s> ops)
{
  vn_reference_op_t vro;
  unsigned int i;
  fprintf (outfile, "{");
  for (i = 0; ops.iterate (i, &vro); i++)
    {
      bool closebrace = false;
      if (vro->opcode != SSA_NAME
	  && TREE_CODE_CLASS (vro->opcode) != tcc_declaration)
	{
	  fprintf (outfile, "%s", get_tree_code_name (vro->opcode));
	  if (vro->op0 || vro->opcode == CALL_EXPR)
	    {
	      fprintf (outfile, "<");
	      closebrace = true;
	    }
	}
      if (vro->op0 || vro->opcode == CALL_EXPR)
	{
	  if (!vro->op0)
	    fprintf (outfile, internal_fn_name ((internal_fn)vro->clique));
	  else
	    print_generic_expr (outfile, vro->op0);
	  if (vro->op1)
	    {
	      fprintf (outfile, ",");
	      print_generic_expr (outfile, vro->op1);
	    }
	  if (vro->op2)
	    {
	      fprintf (outfile, ",");
	      print_generic_expr (outfile, vro->op2);
	    }
	}
      if (closebrace)
	fprintf (outfile, ">");
      if (i != ops.length () - 1)
	fprintf (outfile, ",");
    }
  fprintf (outfile, "}");
}

DEBUG_FUNCTION void
debug_vn_reference_ops (const vec<vn_reference_op_s> ops)
{
  print_vn_reference_ops (stderr, ops);
  fputc ('\n', stderr);
}

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


/* Obstack we allocate the vn-tables elements from.  */
static obstack vn_tables_obstack;
/* Special obstack we never unwind.  */
static obstack vn_tables_insert_obstack;

static vn_reference_t last_inserted_ref;
static vn_phi_t last_inserted_phi;
static vn_nary_op_t last_inserted_nary;
static vn_ssa_aux_t last_pushed_avail;

/* Valid hashtables storing information we have proven to be
   correct.  */
static vn_tables_t valid_info;

/* Global RPO state for access from hooks.  */
static class eliminate_dom_walker *rpo_avail;
basic_block vn_context_bb;


/* Valueization hook for simplify_replace_tree.  Valueize NAME if it is
   an SSA name, otherwise just return it.  */
tree (*vn_valueize) (tree);
static tree
vn_valueize_for_srt (tree t, void* context ATTRIBUTE_UNUSED)
{
  basic_block saved_vn_context_bb = vn_context_bb;
  /* Look for sth available at the definition block of the argument.
     This avoids inconsistencies between availability there which
     decides if the stmt can be removed and availability at the
     use site.  The SSA property ensures that things available
     at the definition are also available at uses.  */
  if (!SSA_NAME_IS_DEFAULT_DEF (t))
    vn_context_bb = gimple_bb (SSA_NAME_DEF_STMT (t));
  tree res = vn_valueize (t);
  vn_context_bb = saved_vn_context_bb;
  return res;
}


/* This represents the top of the VN lattice, which is the universal
   value.  */

tree VN_TOP;

/* Unique counter for our value ids.  */

static unsigned int next_value_id;
static int next_constant_value_id;


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
  static const bool empty_zero_p = true;
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
static vn_nary_op_t vn_nary_op_insert_into (vn_nary_op_t,
					    vn_nary_op_table_type *);
static void init_vn_nary_op_from_pieces (vn_nary_op_t, unsigned int,
					 enum tree_code, tree, tree *);
static tree vn_lookup_simplify_result (gimple_match_op *);
static vn_reference_t vn_reference_lookup_or_insert_for_pieces
	  (tree, alias_set_type, alias_set_type, poly_int64, poly_int64, tree,
	   vec<vn_reference_op_s, va_heap>, tree);

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
	    vn_nary_op_insert_into (nary, valid_info->nary);
	    gcc_assert (nary->unwind_to == NULL);
	    /* Also do not link it into the undo chain.  */
	    last_inserted_nary = nary->next;
	    nary->next = (vn_nary_op_t)(void *)-1;
	    nary = alloc_vn_nary_op_noinit (2, &vn_tables_insert_obstack);
	    init_vn_nary_op_from_pieces (nary, 2, EQ_EXPR,
					 boolean_type_node, ops);
	    nary->predicated_values = 0;
	    nary->u.result = boolean_false_node;
	    vn_nary_op_insert_into (nary, valid_info->nary);
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

/* Similar to the above but used as callback for walk_non_aliased_vuses
   and thus should stop at unvisited VUSE to not walk across region
   boundaries.  */

static tree
vuse_valueize (tree vuse)
{
  do
    {
      bool visited;
      vuse = SSA_VAL (vuse, &visited);
      if (!visited)
	return NULL_TREE;
      gcc_assert (vuse != VN_TOP);
    }
  while (SSA_NAME_IN_FREE_LIST (vuse));
  return vuse;
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
		    && (TREE_CODE (TREE_OPERAND (rhs1, 0)) == SSA_NAME
			|| is_gimple_min_invariant (TREE_OPERAND (rhs1, 0))))
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
  vcp->value_id = get_next_constant_value_id ();
  *slot = vcp;
  return vcp->value_id;
}

/* Compute the hash for a reference operand VRO1.  */

static void
vn_reference_op_compute_hash (const vn_reference_op_t vro1, inchash::hash &hstate)
{
  hstate.add_int (vro1->opcode);
  if (vro1->opcode == CALL_EXPR && !vro1->op0)
    hstate.add_int (vro1->clique);
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
  /* Do not hash vr1->offset or vr1->max_size, we want to get collisions
     to be able to identify compatible results.  */
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

  /* The offset/max_size used for the ao_ref during lookup has to be
     the same.  */
  if (maybe_ne (vr1->offset, vr2->offset)
      || maybe_ne (vr1->max_size, vr2->max_size))
    {
      /* But nothing known in the prevailing entry is OK to be used.  */
      if (maybe_ne (vr1->offset, 0) || known_size_p (vr1->max_size))
	return false;
    }

  /* If the operands are the same we are done.  */
  if (vr1->operands == vr2->operands)
    return true;

  if (!vr1->type || !vr2->type)
    {
      if (vr1->type != vr2->type)
	return false;
    }
  else if (vr1->type == vr2->type)
    ;
  else if (COMPLETE_TYPE_P (vr1->type) != COMPLETE_TYPE_P (vr2->type)
	   || (COMPLETE_TYPE_P (vr1->type)
	       && !expressions_equal_p (TYPE_SIZE (vr1->type),
					TYPE_SIZE (vr2->type))))
    return false;
  else if (vr1->operands[0].opcode == CALL_EXPR
	   && !types_compatible_p (vr1->type, vr2->type))
    return false;
  else if (INTEGRAL_TYPE_P (vr1->type)
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
  else if (VECTOR_BOOLEAN_TYPE_P (vr1->type)
	   && VECTOR_BOOLEAN_TYPE_P (vr2->type))
    {
      /* Vector boolean types can have padding, verify we are dealing with
	 the same number of elements, aka the precision of the types.
	 For example, In most architecture the precision_size of vbool*_t
	 types are caculated like below:
	 precision_size = type_size * 8

	 Unfortunately, the RISC-V will adjust the precision_size for the
	 vbool*_t in order to align the ISA as below:
	 type_size      = [1, 1, 1, 1,  2,  4,  8]
	 precision_size = [1, 2, 4, 8, 16, 32, 64]

	 Then the precision_size of RISC-V vbool*_t will not be the multiple
	 of the type_size.  We take care of this case consolidated here.  */
      if (maybe_ne (TYPE_VECTOR_SUBPARTS (vr1->type),
		    TYPE_VECTOR_SUBPARTS (vr2->type)))
	return false;
    }
  else if (TYPE_MODE (vr1->type) != TYPE_MODE (vr2->type)
	   && (!mode_can_transfer_bits (TYPE_MODE (vr1->type))
	       || !mode_can_transfer_bits (TYPE_MODE (vr2->type))))
    return false;

  i = 0;
  j = 0;
  do
    {
      poly_int64 off1 = 0, off2 = 0;
      vn_reference_op_t vro1, vro2;
      vn_reference_op_s tem1, tem2;
      bool deref1 = false, deref2 = false;
      bool reverse1 = false, reverse2 = false;
      for (; vr1->operands.iterate (i, &vro1); i++)
	{
	  if (vro1->opcode == MEM_REF)
	    deref1 = true;
	  /* Do not look through a storage order barrier.  */
	  else if (vro1->opcode == VIEW_CONVERT_EXPR && vro1->reverse)
	    return false;
	  reverse1 |= vro1->reverse;
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
	  reverse2 |= vro2->reverse;
	  if (known_eq (vro2->off, -1))
	    break;
	  off2 += vro2->off;
	}
      if (maybe_ne (off1, off2) || reverse1 != reverse2)
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
	case TARGET_MEM_REF:
	  /* The base address gets its own vn_reference_op_s structure.  */
	  temp.op0 = TMR_INDEX (ref);
	  temp.op1 = TMR_STEP (ref);
	  temp.op2 = TMR_OFFSET (ref);
	  temp.clique = MR_DEPENDENCE_CLIQUE (ref);
	  temp.base = MR_DEPENDENCE_BASE (ref);
	  result->safe_push (temp);
	  memset (&temp, 0, sizeof (temp));
	  temp.type = NULL_TREE;
	  temp.opcode = ERROR_MARK;
	  temp.op0 = TMR_INDEX2 (ref);
	  temp.off = -1;
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
	     so use its type here.  */
	  temp.type = TREE_TYPE (TREE_OPERAND (ref, 1));
	  temp.op0 = TREE_OPERAND (ref, 1);
	  temp.op1 = TREE_OPERAND (ref, 2);
	  temp.reverse = (AGGREGATE_TYPE_P (TREE_TYPE (TREE_OPERAND (ref, 0)))
			  && TYPE_REVERSE_STORAGE_ORDER
			       (TREE_TYPE (TREE_OPERAND (ref, 0))));
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
		    /* Prohibit value-numbering zero offset components
		       of addresses the same before the pass folding
		       __builtin_object_size had a chance to run.  Likewise
		       for components of zero size at arbitrary offset.  */
		    if (TREE_CODE (orig) != ADDR_EXPR
			|| (TYPE_SIZE (temp.type)
			    && integer_nonzerop (TYPE_SIZE (temp.type))
			    && maybe_ne (off, 0))
			|| (cfun->curr_properties & PROP_objsz))
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
	    /* Prohibit value-numbering addresses of one-after-the-last
	       element ARRAY_REFs the same as addresses of other components
	       before the pass folding __builtin_object_size had a chance
	       to run.  */
	    bool avoid_oob = true;
	    if (TREE_CODE (orig) != ADDR_EXPR
		|| cfun->curr_properties & PROP_objsz)
	      avoid_oob = false;
	    else if (poly_int_tree_p (temp.op0))
	      {
		tree ub = array_ref_up_bound (ref);
		if (ub
		    && poly_int_tree_p (ub)
		    /* ???  The C frontend for T[0] uses [0:] and the
		       C++ frontend [0:-1U].  See layout_type for how
		       awkward this is.  */
		    && !integer_minus_onep (ub)
		    && known_le (wi::to_poly_offset (temp.op0),
				 wi::to_poly_offset (ub)))
		  avoid_oob = false;
	      }
	    if (poly_int_tree_p (temp.op0)
		&& poly_int_tree_p (temp.op1)
		&& TREE_CODE (temp.op2) == INTEGER_CST
		&& !avoid_oob)
	      {
		poly_offset_int off = ((wi::to_poly_offset (temp.op0)
					- wi::to_poly_offset (temp.op1))
				       * wi::to_offset (temp.op2)
				       * vn_ref_op_align_unit (&temp));
		off.to_shwi (&temp.off);
	      }
	    temp.reverse = (AGGREGATE_TYPE_P (TREE_TYPE (TREE_OPERAND (ref, 0)))
			    && TYPE_REVERSE_STORAGE_ORDER
				 (TREE_TYPE (TREE_OPERAND (ref, 0))));
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
	case POLY_INT_CST:
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
			       alias_set_type set, alias_set_type base_set,
			       tree type, const vec<vn_reference_op_s> &ops)
{
  unsigned i;
  tree base = NULL_TREE;
  tree *op0_p = &base;
  poly_offset_int offset = 0;
  poly_offset_int max_size;
  poly_offset_int size = -1;
  tree size_tree = NULL_TREE;

  /* We don't handle calls.  */
  if (!type)
    return false;

  machine_mode mode = TYPE_MODE (type);
  if (mode == BLKmode)
    size_tree = TYPE_SIZE (type);
  else
    size = GET_MODE_BITSIZE (mode);
  if (size_tree != NULL_TREE
      && poly_int_tree_p (size_tree))
    size = wi::to_poly_offset (size_tree);

  /* Lower the final access size from the outermost expression.  */
  const_vn_reference_op_t cst_op = &ops[0];
  /* Cast away constness for the sake of the const-unsafe
     FOR_EACH_VEC_ELT().  */
  vn_reference_op_t op = const_cast<vn_reference_op_t>(cst_op);
  size_tree = NULL_TREE;
  if (op->opcode == COMPONENT_REF)
    size_tree = DECL_SIZE (op->op0);
  else if (op->opcode == BIT_FIELD_REF)
    size_tree = op->op0;
  if (size_tree != NULL_TREE
      && poly_int_tree_p (size_tree)
      && (!known_size_p (size)
	  || known_lt (wi::to_poly_offset (size_tree), size)))
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
	case CALL_EXPR:
	  return false;

	/* Record the base objects.  */
	case MEM_REF:
	  *op0_p = build2 (MEM_REF, op->type,
			   NULL_TREE, op->op0);
	  MR_DEPENDENCE_CLIQUE (*op0_p) = op->clique;
	  MR_DEPENDENCE_BASE (*op0_p) = op->base;
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  break;

	case TARGET_MEM_REF:
	  *op0_p = build5 (TARGET_MEM_REF, op->type,
			   NULL_TREE, op->op2, op->op0,
			   op->op1, ops[i+1].op0);
	  MR_DEPENDENCE_CLIQUE (*op0_p) = op->clique;
	  MR_DEPENDENCE_BASE (*op0_p) = op->base;
	  op0_p = &TREE_OPERAND (*op0_p, 0);
	  ++i;
	  break;

	/* Unwrap some of the wrapped decls.  */
	case ADDR_EXPR:
	  /* Apart from ADDR_EXPR arguments to MEM_REF.  */
	  if (base != NULL_TREE
	      && TREE_CODE (base) == MEM_REF
	      && op->op0
	      && DECL_P (TREE_OPERAND (op->op0, 0)))
	    {
	      const_vn_reference_op_t pop = &ops[i-1];
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
	case PARM_DECL:
	case CONST_DECL:
	case RESULT_DECL:
	  /* ???  We shouldn't see these, but un-canonicalize what
	     copy_reference_ops_from_ref does when visiting MEM_REF.  */
	case VAR_DECL:
	  /* ???  And for this only have DECL_HARD_REGISTER.  */
	case STRING_CST:
	  /* This can show up in ARRAY_REF bases.  */
	case INTEGER_CST:
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
	  /* Use the recorded constant offset.  */
	  if (maybe_eq (op->off, -1))
	    max_size = -1;
	  else
	    offset += op->off * BITS_PER_UNIT;
	  break;

	case REALPART_EXPR:
	  break;

	case IMAGPART_EXPR:
	  offset += size;
	  break;

	case VIEW_CONVERT_EXPR:
	  break;

	case POLY_INT_CST:
	case COMPLEX_CST:
	case VECTOR_CST:
	case REAL_CST:
	case FIXED_CST:
	case CONSTRUCTOR:
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
  ref->base_alias_set = base_set;
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
  temp.type = gimple_call_fntype (call);
  temp.opcode = CALL_EXPR;
  temp.op0 = gimple_call_fn (call);
  if (gimple_call_internal_p (call))
    temp.clique = gimple_call_internal_fn (call);
  temp.op1 = gimple_call_chain (call);
  if (stmt_could_throw_p (cfun, call) && (lr = lookup_stmt_eh_lp (call)) > 0)
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
  addr_base = get_addr_base_and_unit_offset_1 (TREE_OPERAND (op->op0, 0),
					       &addr_offset, vn_valueize);
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
  bool changed = false;
  vn_reference_op_t op;

  do
    {
      unsigned int i = *i_p;
      op = &(*ops)[i];
      vn_reference_op_t mem_op = &(*ops)[i - 1];
      gimple *def_stmt;
      enum tree_code code;
      poly_offset_int off;

      def_stmt = SSA_NAME_DEF_STMT (op->op0);
      if (!is_gimple_assign (def_stmt))
	return changed;

      code = gimple_assign_rhs_code (def_stmt);
      if (code != ADDR_EXPR
	  && code != POINTER_PLUS_EXPR)
	return changed;

      off = poly_offset_int::from (wi::to_poly_wide (mem_op->op0), SIGNED);

      /* The only thing we have to do is from &OBJ.foo.bar add the offset
	 from .foo.bar to the preceding MEM_REF offset and replace the
	 address with &OBJ.  */
      if (code == ADDR_EXPR)
	{
	  tree addr, addr_base;
	  poly_int64 addr_offset;

	  addr = gimple_assign_rhs1 (def_stmt);
	  addr_base = get_addr_base_and_unit_offset_1 (TREE_OPERAND (addr, 0),
						       &addr_offset,
						       vn_valueize);
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
		  && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (TREE_OPERAND (addr_base,
								    0))))
	    return changed;

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
	    return changed;

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

      changed = true;
    }
  /* Tail-recurse.  */
  while (TREE_CODE (op->op0) == SSA_NAME);

  /* Fold a remaining *&.  */
  if (TREE_CODE (op->op0) == ADDR_EXPR)
    vn_reference_fold_indirect (ops, i_p);

  return changed;
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
      && (!op->op0
	  || (TREE_CODE (op->op0) == ADDR_EXPR
	      && TREE_CODE (TREE_OPERAND (op->op0, 0)) == FUNCTION_DECL
	      && fndecl_built_in_p (TREE_OPERAND (op->op0, 0),
				    BUILT_IN_NORMAL)))
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
	  combined_fn fn;
	  if (op->op0)
	    fn = as_combined_fn (DECL_FUNCTION_CODE
					(TREE_OPERAND (op->op0, 0)));
	  else
	    fn = as_combined_fn ((internal_fn) op->clique);
	  tree folded;
	  if (arg1)
	    folded = fold_const_call (fn, ref->type, arg0->op0, arg1->op0);
	  else
	    folded = fold_const_call (fn, ref->type, arg0->op0);
	  if (folded
	      && is_gimple_min_invariant (folded))
	    return folded;
	}
    }

  /* Simplify reads from constants or constant initializers.  */
  else if (BITS_PER_UNIT == 8
	   && ref->type
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
	       && (VAR_P (TREE_OPERAND (base[1].op0, 0))
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

/* Return true if OPS represent an access with reverse storage order.  */

static bool
reverse_storage_order_for_component_p (vec<vn_reference_op_s> ops)
{
  unsigned i = 0;
  if (ops[i].opcode == REALPART_EXPR || ops[i].opcode == IMAGPART_EXPR)
    ++i;
  switch (ops[i].opcode)
    {
    case ARRAY_REF:
    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case MEM_REF:
      return ops[i].reverse;
    default:
      return false;
    }
}

/* Transform any SSA_NAME's in a vector of vn_reference_op_s
   structures into their value numbers.  This is done in-place, and
   the vector passed in is returned.  *VALUEIZED_ANYTHING will specify
   whether any operands were valueized.  */

static void
valueize_refs_1 (vec<vn_reference_op_s> *orig, bool *valueized_anything,
		 bool with_avail = false)
{
  *valueized_anything = false;

  for (unsigned i = 0; i < orig->length (); ++i)
    {
re_valueize:
      vn_reference_op_t vro = &(*orig)[i];
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
	  && (*orig)[i - 1].opcode == MEM_REF)
	{
	  if (vn_reference_fold_indirect (orig, &i))
	    *valueized_anything = true;
	}
      else if (i > 0
	       && vro->opcode == SSA_NAME
	       && (*orig)[i - 1].opcode == MEM_REF)
	{
	  if (vn_reference_maybe_forwprop_address (orig, &i))
	    {
	      *valueized_anything = true;
	      /* Re-valueize the current operand.  */
	      goto re_valueize;
	    }
	}
      /* If it transforms a non-constant ARRAY_REF into a constant
	 one, adjust the constant offset.  */
      else if ((vro->opcode == ARRAY_REF
		|| vro->opcode == ARRAY_RANGE_REF)
	       && known_eq (vro->off, -1)
	       && poly_int_tree_p (vro->op0)
	       && poly_int_tree_p (vro->op1)
	       && TREE_CODE (vro->op2) == INTEGER_CST)
	{
	    /* Prohibit value-numbering addresses of one-after-the-last
	       element ARRAY_REFs the same as addresses of other components
	       before the pass folding __builtin_object_size had a chance
	       to run.  */
	  if (!(cfun->curr_properties & PROP_objsz)
	      && (*orig)[0].opcode == ADDR_EXPR)
	    {
	      tree dom = TYPE_DOMAIN ((*orig)[i + 1].type);
	      if (!dom
		  || !TYPE_MAX_VALUE (dom)
		  || !poly_int_tree_p (TYPE_MAX_VALUE (dom))
		  || integer_minus_onep (TYPE_MAX_VALUE (dom)))
		continue;
	      if (!known_le (wi::to_poly_offset (vro->op0),
			     wi::to_poly_offset (TYPE_MAX_VALUE (dom))))
		continue;
	    }

	  poly_offset_int off = ((wi::to_poly_offset (vro->op0)
				  - wi::to_poly_offset (vro->op1))
				 * wi::to_offset (vro->op2)
				 * vn_ref_op_align_unit (vro));
	  off.to_shwi (&vro->off);
	}
    }
}

static void
valueize_refs (vec<vn_reference_op_s> *orig)
{
  bool tem;
  valueize_refs_1 (orig, &tem);
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
  valueize_refs_1 (&shared_lookup_references, valueized_anything);
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
  valueize_refs (&shared_lookup_references);
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


/* Partial definition tracking support.  */

struct pd_range
{
  HOST_WIDE_INT offset;
  HOST_WIDE_INT size;
  pd_range *m_children[2];
};

struct pd_data
{
  tree rhs;
  HOST_WIDE_INT rhs_off;
  HOST_WIDE_INT offset;
  HOST_WIDE_INT size;
};

/* Context for alias walking.  */

struct vn_walk_cb_data
{
  vn_walk_cb_data (vn_reference_t vr_, tree orig_ref_, tree *last_vuse_ptr_,
		   vn_lookup_kind vn_walk_kind_, bool tbaa_p_, tree mask_,
		   bool redundant_store_removal_p_)
    : vr (vr_), last_vuse_ptr (last_vuse_ptr_), last_vuse (NULL_TREE),
      mask (mask_), masked_result (NULL_TREE), same_val (NULL_TREE),
      vn_walk_kind (vn_walk_kind_),
      tbaa_p (tbaa_p_), redundant_store_removal_p (redundant_store_removal_p_),
      saved_operands (vNULL), first_range (), first_set (-2),
      first_base_set (-2)
  {
    if (!last_vuse_ptr)
      last_vuse_ptr = &last_vuse;
    ao_ref_init (&orig_ref, orig_ref_);
    if (mask)
      {
	wide_int w = wi::to_wide (mask);
	unsigned int pos = 0, prec = w.get_precision ();
	pd_data pd;
	pd.rhs = build_constructor (NULL_TREE, NULL);
	pd.rhs_off = 0;
	/* When bitwise and with a constant is done on a memory load,
	   we don't really need all the bits to be defined or defined
	   to constants, we don't really care what is in the position
	   corresponding to 0 bits in the mask.
	   So, push the ranges of those 0 bits in the mask as artificial
	   zero stores and let the partial def handling code do the
	   rest.  */
	while (pos < prec)
	  {
	    int tz = wi::ctz (w);
	    if (pos + tz > prec)
	      tz = prec - pos;
	    if (tz)
	      {
		if (BYTES_BIG_ENDIAN)
		  pd.offset = prec - pos - tz;
		else
		  pd.offset = pos;
		pd.size = tz;
		void *r = push_partial_def (pd, 0, 0, 0, prec);
		gcc_assert (r == NULL_TREE);
	      }
	    pos += tz;
	    if (pos == prec)
	      break;
	    w = wi::lrshift (w, tz);
	    tz = wi::ctz (wi::bit_not (w));
	    if (pos + tz > prec)
	      tz = prec - pos;
	    pos += tz;
	    w = wi::lrshift (w, tz);
	  }
      }
  }
  ~vn_walk_cb_data ();
  void *finish (alias_set_type, alias_set_type, tree);
  void *push_partial_def (pd_data pd,
			  alias_set_type, alias_set_type, HOST_WIDE_INT,
			  HOST_WIDE_INT);

  vn_reference_t vr;
  ao_ref orig_ref;
  tree *last_vuse_ptr;
  tree last_vuse;
  tree mask;
  tree masked_result;
  tree same_val;
  vn_lookup_kind vn_walk_kind;
  bool tbaa_p;
  bool redundant_store_removal_p;
  vec<vn_reference_op_s> saved_operands;

  /* The VDEFs of partial defs we come along.  */
  auto_vec<pd_data, 2> partial_defs;
  /* The first defs range to avoid splay tree setup in most cases.  */
  pd_range first_range;
  alias_set_type first_set;
  alias_set_type first_base_set;
  default_splay_tree<pd_range *> known_ranges;
  obstack ranges_obstack;
  static constexpr HOST_WIDE_INT bufsize = 64;
};

vn_walk_cb_data::~vn_walk_cb_data ()
{
  if (known_ranges)
    obstack_free (&ranges_obstack, NULL);
  saved_operands.release ();
}

void *
vn_walk_cb_data::finish (alias_set_type set, alias_set_type base_set, tree val)
{
  if (first_set != -2)
    {
      set = first_set;
      base_set = first_base_set;
    }
  if (mask)
    {
      masked_result = val;
      return (void *) -1;
    }
  if (same_val && !operand_equal_p (val, same_val))
    return (void *) -1;
  vec<vn_reference_op_s> &operands
    = saved_operands.exists () ? saved_operands : vr->operands;
  return vn_reference_lookup_or_insert_for_pieces (last_vuse, set, base_set,
						   vr->offset, vr->max_size,
						   vr->type, operands, val);
}

/* Push PD to the vector of partial definitions returning a
   value when we are ready to combine things with VUSE, SET and MAXSIZEI,
   NULL when we want to continue looking for partial defs or -1
   on failure.  */

void *
vn_walk_cb_data::push_partial_def (pd_data pd,
				   alias_set_type set, alias_set_type base_set,
				   HOST_WIDE_INT offseti,
				   HOST_WIDE_INT maxsizei)
{
  /* We're using a fixed buffer for encoding so fail early if the object
     we want to interpret is bigger.  */
  if (maxsizei > bufsize * BITS_PER_UNIT
      || CHAR_BIT != 8
      || BITS_PER_UNIT != 8
      /* Not prepared to handle PDP endian.  */
      || BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    return (void *)-1;

  /* Turn too large constant stores into non-constant stores.  */
  if (CONSTANT_CLASS_P (pd.rhs) && pd.size > bufsize * BITS_PER_UNIT)
    pd.rhs = error_mark_node;

  /* And for non-constant or CONSTRUCTOR stores shrink them to only keep at
     most a partial byte before and/or after the region.  */
  if (!CONSTANT_CLASS_P (pd.rhs))
    {
      if (pd.offset < offseti)
	{
	  HOST_WIDE_INT o = ROUND_DOWN (offseti - pd.offset, BITS_PER_UNIT);
	  gcc_assert (pd.size > o);
	  pd.size -= o;
	  pd.offset += o;
	}
      if (pd.size > maxsizei)
	pd.size = maxsizei + ((pd.size - maxsizei) % BITS_PER_UNIT);
    }

  pd.offset -= offseti;

  bool pd_constant_p = (TREE_CODE (pd.rhs) == CONSTRUCTOR
			|| CONSTANT_CLASS_P (pd.rhs));
  pd_range *r;
  if (partial_defs.is_empty ())
    {
      /* If we get a clobber upfront, fail.  */
      if (TREE_CLOBBER_P (pd.rhs))
	return (void *)-1;
      if (!pd_constant_p)
	return (void *)-1;
      partial_defs.safe_push (pd);
      first_range.offset = pd.offset;
      first_range.size = pd.size;
      first_set = set;
      first_base_set = base_set;
      last_vuse_ptr = NULL;
      r = &first_range;
      /* Go check if the first partial definition was a full one in case
	 the caller didn't optimize for this.  */
    }
  else
    {
      if (!known_ranges)
	{
	  /* ???  Optimize the case where the 2nd partial def completes
	     things.  */
	  gcc_obstack_init (&ranges_obstack);
	  known_ranges.insert_max_node (&first_range);
	}
      /* Lookup the offset and see if we need to merge.  */
      int comparison = known_ranges.lookup_le
	([&] (pd_range *r) { return pd.offset < r->offset; },
	 [&] (pd_range *r) { return pd.offset > r->offset; });
      r = known_ranges.root ();
      if (comparison >= 0
	  && ranges_known_overlap_p (r->offset, r->size + 1,
				     pd.offset, pd.size))
	{
	  /* Ignore partial defs already covered.  Here we also drop shadowed
	     clobbers arriving here at the floor.  */
	  if (known_subrange_p (pd.offset, pd.size, r->offset, r->size))
	    return NULL;
	  r->size = MAX (r->offset + r->size, pd.offset + pd.size) - r->offset;
	}
      else
	{
	  /* pd.offset wasn't covered yet, insert the range.  */
	  void *addr = XOBNEW (&ranges_obstack, pd_range);
	  r = new (addr) pd_range { pd.offset, pd.size, {} };
	  known_ranges.insert_relative (comparison, r);
	}
      /* Merge r which now contains pd's range and is a member of the splay
	 tree with adjacent overlapping ranges.  */
      if (known_ranges.splay_next_node ())
	do
	  {
	    pd_range *rafter = known_ranges.root ();
	    if (!ranges_known_overlap_p (r->offset, r->size + 1,
					 rafter->offset, rafter->size))
	      break;
	    r->size = MAX (r->offset + r->size,
			   rafter->offset + rafter->size) - r->offset;
	  }
	while (known_ranges.remove_root_and_splay_next ());
      /* If we get a clobber, fail.  */
      if (TREE_CLOBBER_P (pd.rhs))
	return (void *)-1;
      /* Non-constants are OK as long as they are shadowed by a constant.  */
      if (!pd_constant_p)
	return (void *)-1;
      partial_defs.safe_push (pd);
    }

  /* Now we have merged pd's range into the range tree.  When we have covered
     [offseti, sizei] then the tree will contain exactly one node which has
     the desired properties and it will be 'r'.  */
  if (!known_subrange_p (0, maxsizei, r->offset, r->size))
    /* Continue looking for partial defs.  */
    return NULL;

  /* Now simply native encode all partial defs in reverse order.  */
  unsigned ndefs = partial_defs.length ();
  /* We support up to 512-bit values (for V8DFmode).  */
  unsigned char buffer[bufsize + 1];
  unsigned char this_buffer[bufsize + 1];
  int len;

  memset (buffer, 0, bufsize + 1);
  unsigned needed_len = ROUND_UP (maxsizei, BITS_PER_UNIT) / BITS_PER_UNIT;
  while (!partial_defs.is_empty ())
    {
      pd_data pd = partial_defs.pop ();
      unsigned int amnt;
      if (TREE_CODE (pd.rhs) == CONSTRUCTOR)
	{
	  /* Empty CONSTRUCTOR.  */
	  if (pd.size >= needed_len * BITS_PER_UNIT)
	    len = needed_len;
	  else
	    len = ROUND_UP (pd.size, BITS_PER_UNIT) / BITS_PER_UNIT;
	  memset (this_buffer, 0, len);
	}
      else if (pd.rhs_off >= 0)
	{
	  len = native_encode_expr (pd.rhs, this_buffer, bufsize,
				    (MAX (0, -pd.offset)
				     + pd.rhs_off) / BITS_PER_UNIT);
	  if (len <= 0
	      || len < (ROUND_UP (pd.size, BITS_PER_UNIT) / BITS_PER_UNIT
			- MAX (0, -pd.offset) / BITS_PER_UNIT))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Failed to encode %u "
			 "partial definitions\n", ndefs);
	      return (void *)-1;
	    }
	}
      else /* negative pd.rhs_off indicates we want to chop off first bits */
	{
	  if (-pd.rhs_off >= bufsize)
	    return (void *)-1;
	  len = native_encode_expr (pd.rhs,
				    this_buffer + -pd.rhs_off / BITS_PER_UNIT,
				    bufsize - -pd.rhs_off / BITS_PER_UNIT,
				    MAX (0, -pd.offset) / BITS_PER_UNIT);
	  if (len <= 0
	      || len < (ROUND_UP (pd.size, BITS_PER_UNIT) / BITS_PER_UNIT
			- MAX (0, -pd.offset) / BITS_PER_UNIT))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Failed to encode %u "
			 "partial definitions\n", ndefs);
	      return (void *)-1;
	    }
	}

      unsigned char *p = buffer;
      HOST_WIDE_INT size = pd.size;
      if (pd.offset < 0)
	size -= ROUND_DOWN (-pd.offset, BITS_PER_UNIT);
      this_buffer[len] = 0;
      if (BYTES_BIG_ENDIAN)
	{
	  /* LSB of this_buffer[len - 1] byte should be at
	     pd.offset + pd.size - 1 bits in buffer.  */
	  amnt = ((unsigned HOST_WIDE_INT) pd.offset
		  + pd.size) % BITS_PER_UNIT;
	  if (amnt)
	    shift_bytes_in_array_right (this_buffer, len + 1, amnt);
	  unsigned char *q = this_buffer;
	  unsigned int off = 0;
	  if (pd.offset >= 0)
	    {
	      unsigned int msk;
	      off = pd.offset / BITS_PER_UNIT;
	      gcc_assert (off < needed_len);
	      p = buffer + off;
	      if (size <= amnt)
		{
		  msk = ((1 << size) - 1) << (BITS_PER_UNIT - amnt);
		  *p = (*p & ~msk) | (this_buffer[len] & msk);
		  size = 0;
		}
	      else
		{
		  if (TREE_CODE (pd.rhs) != CONSTRUCTOR)
		    q = (this_buffer + len
			 - (ROUND_UP (size - amnt, BITS_PER_UNIT)
			    / BITS_PER_UNIT));
		  if (pd.offset % BITS_PER_UNIT)
		    {
		      msk = -1U << (BITS_PER_UNIT
				    - (pd.offset % BITS_PER_UNIT));
		      *p = (*p & msk) | (*q & ~msk);
		      p++;
		      q++;
		      off++;
		      size -= BITS_PER_UNIT - (pd.offset % BITS_PER_UNIT);
		      gcc_assert (size >= 0);
		    }
		}
	    }
	  else if (TREE_CODE (pd.rhs) != CONSTRUCTOR)
	    {
	      q = (this_buffer + len
		   - (ROUND_UP (size - amnt, BITS_PER_UNIT)
		      / BITS_PER_UNIT));
	      if (pd.offset % BITS_PER_UNIT)
		{
		  q++;
		  size -= BITS_PER_UNIT - ((unsigned HOST_WIDE_INT) pd.offset
					   % BITS_PER_UNIT);
		  gcc_assert (size >= 0);
		}
	    }
	  if ((unsigned HOST_WIDE_INT) size / BITS_PER_UNIT + off
	      > needed_len)
	    size = (needed_len - off) * BITS_PER_UNIT;
	  memcpy (p, q, size / BITS_PER_UNIT);
	  if (size % BITS_PER_UNIT)
	    {
	      unsigned int msk
		= -1U << (BITS_PER_UNIT - (size % BITS_PER_UNIT));
	      p += size / BITS_PER_UNIT;
	      q += size / BITS_PER_UNIT;
	      *p = (*q & msk) | (*p & ~msk);
	    }
	}
      else
	{
	  if (pd.offset >= 0)
	    {
	      /* LSB of this_buffer[0] byte should be at pd.offset bits
		 in buffer.  */
	      unsigned int msk;
	      size = MIN (size, (HOST_WIDE_INT) needed_len * BITS_PER_UNIT);
	      amnt = pd.offset % BITS_PER_UNIT;
	      if (amnt)
		shift_bytes_in_array_left (this_buffer, len + 1, amnt);
	      unsigned int off = pd.offset / BITS_PER_UNIT;
	      gcc_assert (off < needed_len);
	      size = MIN (size,
			  (HOST_WIDE_INT) (needed_len - off) * BITS_PER_UNIT);
	      p = buffer + off;
	      if (amnt + size < BITS_PER_UNIT)
		{
		  /* Low amnt bits come from *p, then size bits
		     from this_buffer[0] and the remaining again from
		     *p.  */
		  msk = ((1 << size) - 1) << amnt;
		  *p = (*p & ~msk) | (this_buffer[0] & msk);
		  size = 0;
		}
	      else if (amnt)
		{
		  msk = -1U << amnt;
		  *p = (*p & ~msk) | (this_buffer[0] & msk);
		  p++;
		  size -= (BITS_PER_UNIT - amnt);
		}
	    }
	  else
	    {
	      amnt = (unsigned HOST_WIDE_INT) pd.offset % BITS_PER_UNIT;
	      if (amnt)
		size -= BITS_PER_UNIT - amnt;
	      size = MIN (size, (HOST_WIDE_INT) needed_len * BITS_PER_UNIT);
	      if (amnt)
		shift_bytes_in_array_left (this_buffer, len + 1, amnt);
	    }
	  memcpy (p, this_buffer + (amnt != 0), size / BITS_PER_UNIT);
	  p += size / BITS_PER_UNIT;
	  if (size % BITS_PER_UNIT)
	    {
	      unsigned int msk = -1U << (size % BITS_PER_UNIT);
	      *p = (this_buffer[(amnt != 0) + size / BITS_PER_UNIT]
		    & ~msk) | (*p & msk);
	    }
	}
    }

  tree type = vr->type;
  /* Make sure to interpret in a type that has a range covering the whole
     access size.  */
  if (INTEGRAL_TYPE_P (vr->type) && maxsizei != TYPE_PRECISION (vr->type))
    type = build_nonstandard_integer_type (maxsizei, TYPE_UNSIGNED (type));
  tree val;
  if (BYTES_BIG_ENDIAN)
    {
      unsigned sz = needed_len;
      if (maxsizei % BITS_PER_UNIT)
	shift_bytes_in_array_right (buffer, needed_len,
				    BITS_PER_UNIT
				    - (maxsizei % BITS_PER_UNIT));
      if (INTEGRAL_TYPE_P (type))
	{
	  if (TYPE_MODE (type) != BLKmode)
	    sz = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type));
	  else
	    sz = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
	}
      if (sz > needed_len)
	{
	  memcpy (this_buffer + (sz - needed_len), buffer, needed_len);
	  val = native_interpret_expr (type, this_buffer, sz);
	}
      else
	val = native_interpret_expr (type, buffer, needed_len);
    }
  else
    val = native_interpret_expr (type, buffer, bufsize);
  /* If we chop off bits because the types precision doesn't match the memory
     access size this is ok when optimizing reads but not when called from
     the DSE code during elimination.  */
  if (val && type != vr->type)
    {
      if (! int_fits_type_p (val, vr->type))
	val = NULL_TREE;
      else
	val = fold_convert (vr->type, val);
    }

  if (val)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Successfully combined %u partial definitions\n", ndefs);
      /* We are using the alias-set of the first store we encounter which
	 should be appropriate here.  */
      return finish (first_set, first_base_set, val);
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Failed to interpret %u encoded partial definitions\n", ndefs);
      return (void *)-1;
    }
}

/* Callback for walk_non_aliased_vuses.  Adjusts the vn_reference_t VR_
   with the current VUSE and performs the expression lookup.  */

static void *
vn_reference_lookup_2 (ao_ref *op, tree vuse, void *data_)
{
  vn_walk_cb_data *data = (vn_walk_cb_data *)data_;
  vn_reference_t vr = data->vr;
  vn_reference_s **slot;
  hashval_t hash;

  /* If we have partial definitions recorded we have to go through
     vn_reference_lookup_3.  */
  if (!data->partial_defs.is_empty ())
    return NULL;

  if (data->last_vuse_ptr)
    {
      *data->last_vuse_ptr = vuse;
      data->last_vuse = vuse;
    }

  /* Fixup vuse and hash.  */
  if (vr->vuse)
    vr->hashcode = vr->hashcode - SSA_NAME_VERSION (vr->vuse);
  vr->vuse = vuse_ssa_val (vuse);
  if (vr->vuse)
    vr->hashcode = vr->hashcode + SSA_NAME_VERSION (vr->vuse);

  hash = vr->hashcode;
  slot = valid_info->references->find_slot_with_hash (vr, hash, NO_INSERT);
  if (slot)
    {
      if ((*slot)->result && data->saved_operands.exists ())
	return data->finish (vr->set, vr->base_set, (*slot)->result);
      return *slot;
    }

  if (SSA_NAME_IS_DEFAULT_DEF (vuse))
    {
      HOST_WIDE_INT op_offset, op_size;
      tree v = NULL_TREE;
      tree base = ao_ref_base (op);

      if (base
	  && op->offset.is_constant (&op_offset)
	  && op->size.is_constant (&op_size)
	  && op->max_size_known_p ()
	  && known_eq (op->size, op->max_size))
	{
	  if (TREE_CODE (base) == PARM_DECL)
	    v = ipcp_get_aggregate_const (cfun, base, false, op_offset,
					  op_size);
	  else if (TREE_CODE (base) == MEM_REF
		   && integer_zerop (TREE_OPERAND (base, 1))
		   && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME
		   && SSA_NAME_IS_DEFAULT_DEF (TREE_OPERAND (base, 0))
		   && (TREE_CODE (SSA_NAME_VAR (TREE_OPERAND (base, 0)))
		       == PARM_DECL))
	    v = ipcp_get_aggregate_const (cfun,
					  SSA_NAME_VAR (TREE_OPERAND (base, 0)),
					  true, op_offset, op_size);
	}
      if (v)
	return data->finish (vr->set, vr->base_set, v);
    }

  return NULL;
}

/* Lookup an existing or insert a new vn_reference entry into the
   value table for the VUSE, SET, TYPE, OPERANDS reference which
   has the value VALUE which is either a constant or an SSA name.  */

static vn_reference_t
vn_reference_lookup_or_insert_for_pieces (tree vuse,
					  alias_set_type set,
					  alias_set_type base_set,
					  poly_int64 offset,
					  poly_int64 max_size,
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
  vr1.base_set = base_set;
  vr1.offset = offset;
  vr1.max_size = max_size;
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  if (vn_reference_lookup_1 (&vr1, &result))
    return result;

  if (TREE_CODE (value) == SSA_NAME)
    value_id = VN_INFO (value)->value_id;
  else
    value_id = get_or_alloc_constant_value_id (value);
  return vn_reference_insert_pieces (vuse, set, base_set, offset, max_size,
				     type, operands.copy (), value, value_id);
}

/* Return a value-number for RCODE OPS... either by looking up an existing
   value-number for the possibly simplified result or by inserting the
   operation if INSERT is true.  If SIMPLIFY is false, return a value
   number for the unsimplified expression.  */

static tree
vn_nary_build_or_lookup_1 (gimple_match_op *res_op, bool insert,
			   bool simplify)
{
  tree result = NULL_TREE;
  /* We will be creating a value number for
       RCODE (OPS...).
     So first simplify and lookup this expression to see if it
     is already available.  */
  /* For simplification valueize.  */
  unsigned i = 0;
  if (simplify)
    for (i = 0; i < res_op->num_ops; ++i)
      if (TREE_CODE (res_op->ops[i]) == SSA_NAME)
	{
	  tree tem = vn_valueize (res_op->ops[i]);
	  if (!tem)
	    break;
	  res_op->ops[i] = tem;
	}
  /* If valueization of an operand fails (it is not available), skip
     simplification.  */
  bool res = false;
  if (i == res_op->num_ops)
    {
      /* Do not leak not available operands into the simplified expression
	 when called from PRE context.  */
      if (rpo_avail)
	mprts_hook = vn_lookup_simplify_result;
      res = res_op->resimplify (NULL, vn_valueize);
      mprts_hook = NULL;
    }
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
	  init_vn_nary_op_from_stmt (vno1, as_a <gassign *> (new_stmt));
	  vn_nary_op_insert_into (vno1, valid_info->nary);
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
  return vn_nary_build_or_lookup_1 (res_op, true, true);
}

/* Try to simplify the expression RCODE OPS... of type TYPE and return
   its value if present.  Update NARY with a simplified expression if
   it fits.  */

tree
vn_nary_simplify (vn_nary_op_t nary)
{
  if (nary->length > gimple_match_op::MAX_NUM_OPS
      /* For CONSTRUCTOR the vn_nary_op_t and gimple_match_op representation
	 does not match.  */
      || nary->opcode == CONSTRUCTOR)
    return NULL_TREE;
  gimple_match_op op (gimple_match_cond::UNCOND, nary->opcode,
		      nary->type, nary->length);
  memcpy (op.ops, nary->op, sizeof (tree) * nary->length);
  tree res = vn_nary_build_or_lookup_1 (&op, false, true);
  if (op.code.is_tree_code ()
      && op.num_ops <= nary->length
      && (tree_code) op.code != CONSTRUCTOR)
    {
      nary->opcode = (tree_code) op.code;
      nary->length = op.num_ops;
      for (unsigned i = 0; i < op.num_ops; ++i)
	nary->op[i] = op.ops[i];
    }
  return res;
}

/* Elimination engine.  */

class eliminate_dom_walker : public dom_walker
{
public:
  eliminate_dom_walker (cdi_direction, bitmap);
  ~eliminate_dom_walker ();

  edge before_dom_children (basic_block) final override;
  void after_dom_children (basic_block) final override;

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

/* Adaptor to the elimination engine using RPO availability.  */

class rpo_elim : public eliminate_dom_walker
{
public:
  rpo_elim(basic_block entry_)
    : eliminate_dom_walker (CDI_DOMINATORS, NULL), entry (entry_),
      m_avail_freelist (NULL) {}

  tree eliminate_avail (basic_block, tree op) final override;

  void eliminate_push_avail (basic_block, tree) final override;

  basic_block entry;
  /* Freelist of avail entries which are allocated from the vn_ssa_aux
     obstack.  */
  vn_avail *m_avail_freelist;
};

/* Return true if BASE1 and BASE2 can be adjusted so they have the
   same address and adjust *OFFSET1 and *OFFSET2 accordingly.
   Otherwise return false.  */

static bool
adjust_offsets_for_equal_base_address (tree base1, poly_int64 *offset1,
				       tree base2, poly_int64 *offset2)
{
  poly_int64 soff;
  if (TREE_CODE (base1) == MEM_REF
      && TREE_CODE (base2) == MEM_REF)
    {
      if (mem_ref_offset (base1).to_shwi (&soff))
	{
	  base1 = TREE_OPERAND (base1, 0);
	  *offset1 += soff * BITS_PER_UNIT;
	}
      if (mem_ref_offset (base2).to_shwi (&soff))
	{
	  base2 = TREE_OPERAND (base2, 0);
	  *offset2 += soff * BITS_PER_UNIT;
	}
      return operand_equal_p (base1, base2, 0);
    }
  return operand_equal_p (base1, base2, OEP_ADDRESS_OF);
}

/* Callback for walk_non_aliased_vuses.  Tries to perform a lookup
   from the statement defining VUSE and if not successful tries to
   translate *REFP and VR_ through an aggregate copy at the definition
   of VUSE.  If *DISAMBIGUATE_ONLY is true then do not perform translation
   of *REF and *VR.  If only disambiguation was performed then
   *DISAMBIGUATE_ONLY is set to true.  */

static void *
vn_reference_lookup_3 (ao_ref *ref, tree vuse, void *data_,
		       translate_flags *disambiguate_only)
{
  vn_walk_cb_data *data = (vn_walk_cb_data *)data_;
  vn_reference_t vr = data->vr;
  gimple *def_stmt = SSA_NAME_DEF_STMT (vuse);
  tree base = ao_ref_base (ref);
  HOST_WIDE_INT offseti = 0, maxsizei, sizei = 0;
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
      if (*disambiguate_only <= TR_VALUEIZE_AND_DISAMBIGUATE)
	{
	  copy_reference_ops_from_ref (lhs, &lhs_ops);
	  valueize_refs_1 (&lhs_ops, &valueized_anything, true);
	}
      vn_context_bb = saved_rpo_bb;
      ao_ref_init (&lhs_ref, lhs);
      lhs_ref_ok = true;
      if (valueized_anything
	  && ao_ref_init_from_vn_reference
	       (&lhs_ref, ao_ref_alias_set (&lhs_ref),
		ao_ref_base_alias_set (&lhs_ref), TREE_TYPE (lhs), lhs_ops)
	  && !refs_may_alias_p_1 (ref, &lhs_ref, data->tbaa_p))
	{
	  *disambiguate_only = TR_VALUEIZE_AND_DISAMBIGUATE;
	  return NULL;
	}

      /* When the def is a CLOBBER we can optimistically disambiguate
	 against it since any overlap it would be undefined behavior.
	 Avoid this for obvious must aliases to save compile-time though.
	 We also may not do this when the query is used for redundant
	 store removal.  */
      if (!data->redundant_store_removal_p
	  && gimple_clobber_p (def_stmt)
	  && !operand_equal_p (ao_ref_base (&lhs_ref), base, OEP_ADDRESS_OF))
	{
	  *disambiguate_only = TR_DISAMBIGUATE;
	  return NULL;
	}

      /* Besides valueizing the LHS we can also use access-path based
         disambiguation on the original non-valueized ref.  */
      if (!ref->ref
	  && lhs_ref_ok
	  && data->orig_ref.ref)
	{
	  /* We want to use the non-valueized LHS for this, but avoid redundant
	     work.  */
	  ao_ref *lref = &lhs_ref;
	  ao_ref lref_alt;
	  if (valueized_anything)
	    {
	      ao_ref_init (&lref_alt, lhs);
	      lref = &lref_alt;
	    }
	  if (!refs_may_alias_p_1 (&data->orig_ref, lref, data->tbaa_p))
	    {
	      *disambiguate_only = (valueized_anything
				    ? TR_VALUEIZE_AND_DISAMBIGUATE
				    : TR_DISAMBIGUATE);
	      return NULL;
	    }
	}

      /* If we reach a clobbering statement try to skip it and see if
         we find a VN result with exactly the same value as the
	 possible clobber.  In this case we can ignore the clobber
	 and return the found value.  */
      if (is_gimple_reg_type (TREE_TYPE (lhs))
	  && types_compatible_p (TREE_TYPE (lhs), vr->type)
	  && (ref->ref || data->orig_ref.ref)
	  && !data->mask
	  && data->partial_defs.is_empty ()
	  && multiple_p (get_object_alignment
			   (ref->ref ? ref->ref : data->orig_ref.ref),
			   ref->size)
	  && multiple_p (get_object_alignment (lhs), ref->size))
	{
	  tree rhs = gimple_assign_rhs1 (def_stmt);
	  /* ???  We may not compare to ahead values which might be from
	     a different loop iteration but only to loop invariants.  Use
	     CONSTANT_CLASS_P (unvalueized!) as conservative approximation.
	     The one-hop lookup below doesn't have this issue since there's
	     a virtual PHI before we ever reach a backedge to cross.
	     We can skip multiple defs as long as they are from the same
	     value though.  */
	  if (data->same_val
	      && !operand_equal_p (data->same_val, rhs))
	    ;
	  else if (CONSTANT_CLASS_P (rhs))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file,
			   "Skipping possible redundant definition ");
		  print_gimple_stmt (dump_file, def_stmt, 0);
		}
	      /* Delay the actual compare of the values to the end of the walk
		 but do not update last_vuse from here.  */
	      data->last_vuse_ptr = NULL;
	      data->same_val = rhs;
	      return NULL;
	    }
	  else
	    {
	      tree saved_vuse = vr->vuse;
	      hashval_t saved_hashcode = vr->hashcode;
	      if (vr->vuse)
		vr->hashcode = vr->hashcode - SSA_NAME_VERSION (vr->vuse);
	      vr->vuse = vuse_ssa_val (gimple_vuse (def_stmt));
	      if (vr->vuse)
		vr->hashcode = vr->hashcode + SSA_NAME_VERSION (vr->vuse);
	      vn_reference_t vnresult = NULL;
	      /* Do not use vn_reference_lookup_2 since that might perform
		 expression hashtable insertion but this lookup crosses
		 a possible may-alias making such insertion conditionally
		 invalid.  */
	      vn_reference_lookup_1 (vr, &vnresult);
	      /* Need to restore vr->vuse and vr->hashcode.  */
	      vr->vuse = saved_vuse;
	      vr->hashcode = saved_hashcode;
	      if (vnresult)
		{
		  if (TREE_CODE (rhs) == SSA_NAME)
		    rhs = SSA_VAL (rhs);
		  if (vnresult->result
		      && operand_equal_p (vnresult->result, rhs, 0))
		    return vnresult;
		}
	    }
	}
    }
  else if (*disambiguate_only <= TR_VALUEIZE_AND_DISAMBIGUATE
	   && gimple_call_builtin_p (def_stmt, BUILT_IN_NORMAL)
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
					       ref, data->tbaa_p);
	  for (unsigned i = 0; i < gimple_call_num_args (def_stmt); ++i)
	    gimple_call_set_arg (def_stmt, i, oldargs[i]);
	  if (!res)
	    {
	      *disambiguate_only = TR_VALUEIZE_AND_DISAMBIGUATE;
	      return NULL;
	    }
	}
    }

  if (*disambiguate_only > TR_TRANSLATE)
    return (void *)-1;

  /* If we cannot constrain the size of the reference we cannot
     test if anything kills it.  */
  if (!ref->max_size_known_p ())
    return (void *)-1;

  poly_int64 offset = ref->offset;
  poly_int64 maxsize = ref->max_size;

  /* def_stmt may-defs *ref.  See if we can derive a value for *ref
     from that definition.
     1) Memset.  */
  if (is_gimple_reg_type (vr->type)
      && (gimple_call_builtin_p (def_stmt, BUILT_IN_MEMSET)
	  || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMSET_CHK))
      && (integer_zerop (gimple_call_arg (def_stmt, 1))
	  || ((TREE_CODE (gimple_call_arg (def_stmt, 1)) == INTEGER_CST
	       || (INTEGRAL_TYPE_P (vr->type) && known_eq (ref->size, 8)))
	      && CHAR_BIT == 8
	      && BITS_PER_UNIT == 8
	      && BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
	      && offset.is_constant (&offseti)
	      && ref->size.is_constant (&sizei)
	      && (offseti % BITS_PER_UNIT == 0
		  || TREE_CODE (gimple_call_arg (def_stmt, 1)) == INTEGER_CST)))
      && (poly_int_tree_p (gimple_call_arg (def_stmt, 2))
	  || (TREE_CODE (gimple_call_arg (def_stmt, 2)) == SSA_NAME
	      && poly_int_tree_p (SSA_VAL (gimple_call_arg (def_stmt, 2)))))
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
	      || !(mem_ref_offset (base)
		   << LOG2_BITS_PER_UNIT).to_shwi (&soff))
	    return (void *)-1;
	  offset += soff;
	  offset2 = 0;
	  if (TREE_OPERAND (base, 0) != ref2)
	    {
	      gimple *def = SSA_NAME_DEF_STMT (ref2);
	      if (is_gimple_assign (def)
		  && gimple_assign_rhs_code (def) == POINTER_PLUS_EXPR
		  && gimple_assign_rhs1 (def) == TREE_OPERAND (base, 0)
		  && poly_int_tree_p (gimple_assign_rhs2 (def)))
		{
		  tree rhs2 = gimple_assign_rhs2 (def);
		  if (!(poly_offset_int::from (wi::to_poly_wide (rhs2),
					       SIGNED)
			<< LOG2_BITS_PER_UNIT).to_shwi (&offset2))
		    return (void *)-1;
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
      HOST_WIDE_INT leni, offset2i;
      if (TREE_CODE (len) == SSA_NAME)
	len = SSA_VAL (len);
      /* Sometimes the above trickery is smarter than alias analysis.  Take
         advantage of that.  */
      if (!ranges_maybe_overlap_p (offset, maxsize, offset2,
				   (wi::to_poly_offset (len)
				    << LOG2_BITS_PER_UNIT)))
	return NULL;
      if (data->partial_defs.is_empty ()
	  && known_subrange_p (offset, maxsize, offset2,
			       wi::to_poly_offset (len) << LOG2_BITS_PER_UNIT))
	{
	  tree val;
	  if (integer_zerop (gimple_call_arg (def_stmt, 1)))
	    val = build_zero_cst (vr->type);
	  else if (INTEGRAL_TYPE_P (vr->type)
		   && known_eq (ref->size, 8)
		   && offseti % BITS_PER_UNIT == 0)
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
	      unsigned buflen
		= TREE_INT_CST_LOW (TYPE_SIZE_UNIT (vr->type)) + 1;
	      if (INTEGRAL_TYPE_P (vr->type)
		  && TYPE_MODE (vr->type) != BLKmode)
		buflen = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (vr->type)) + 1;
	      unsigned char *buf = XALLOCAVEC (unsigned char, buflen);
	      memset (buf, TREE_INT_CST_LOW (gimple_call_arg (def_stmt, 1)),
		      buflen);
	      if (BYTES_BIG_ENDIAN)
		{
		  unsigned int amnt
		    = (((unsigned HOST_WIDE_INT) offseti + sizei)
		       % BITS_PER_UNIT);
		  if (amnt)
		    {
		      shift_bytes_in_array_right (buf, buflen,
						  BITS_PER_UNIT - amnt);
		      buf++;
		      buflen--;
		    }
		}
	      else if (offseti % BITS_PER_UNIT != 0)
		{
		  unsigned int amnt
		    = BITS_PER_UNIT - ((unsigned HOST_WIDE_INT) offseti
				       % BITS_PER_UNIT);
		  shift_bytes_in_array_left (buf, buflen, amnt);
		  buf++;
		  buflen--;
		}
	      val = native_interpret_expr (vr->type, buf, buflen);
	      if (!val)
		return (void *)-1;
	    }
	  return data->finish (0, 0, val);
	}
      /* For now handle clearing memory with partial defs.  */
      else if (known_eq (ref->size, maxsize)
	       && integer_zerop (gimple_call_arg (def_stmt, 1))
	       && tree_fits_poly_int64_p (len)
	       && tree_to_poly_int64 (len).is_constant (&leni)
	       && leni <= INTTYPE_MAXIMUM (HOST_WIDE_INT) / BITS_PER_UNIT
	       && offset.is_constant (&offseti)
	       && offset2.is_constant (&offset2i)
	       && maxsize.is_constant (&maxsizei)
	       && ranges_known_overlap_p (offseti, maxsizei, offset2i,
					  leni << LOG2_BITS_PER_UNIT))
	{
	  pd_data pd;
	  pd.rhs = build_constructor (NULL_TREE, NULL);
	  pd.rhs_off = 0;
	  pd.offset = offset2i;
	  pd.size = leni << LOG2_BITS_PER_UNIT;
	  return data->push_partial_def (pd, 0, 0, offseti, maxsizei);
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
      HOST_WIDE_INT offset2i, size2i;
      gcc_assert (lhs_ref_ok);
      base2 = ao_ref_base (&lhs_ref);
      offset2 = lhs_ref.offset;
      size2 = lhs_ref.size;
      maxsize2 = lhs_ref.max_size;
      if (known_size_p (maxsize2)
	  && known_eq (maxsize2, size2)
	  && adjust_offsets_for_equal_base_address (base, &offset,
						    base2, &offset2))
	{
	  if (data->partial_defs.is_empty ()
	      && known_subrange_p (offset, maxsize, offset2, size2))
	    {
	      /* While technically undefined behavior do not optimize
	         a full read from a clobber.  */
	      if (gimple_clobber_p (def_stmt))
		return (void *)-1;
	      tree val = build_zero_cst (vr->type);
	      return data->finish (ao_ref_alias_set (&lhs_ref),
				   ao_ref_base_alias_set (&lhs_ref), val);
	    }
	  else if (known_eq (ref->size, maxsize)
		   && maxsize.is_constant (&maxsizei)
		   && offset.is_constant (&offseti)
		   && offset2.is_constant (&offset2i)
		   && size2.is_constant (&size2i)
		   && ranges_known_overlap_p (offseti, maxsizei,
					      offset2i, size2i))
	    {
	      /* Let clobbers be consumed by the partial-def tracker
	         which can choose to ignore them if they are shadowed
		 by a later def.  */
	      pd_data pd;
	      pd.rhs = gimple_assign_rhs1 (def_stmt);
	      pd.rhs_off = 0;
	      pd.offset = offset2i;
	      pd.size = size2i;
	      return data->push_partial_def (pd, ao_ref_alias_set (&lhs_ref),
					     ao_ref_base_alias_set (&lhs_ref),
					     offseti, maxsizei);
	    }
	}
    }

  /* 3) Assignment from a constant.  We can use folds native encode/interpret
     routines to extract the assigned bits.  */
  else if (known_eq (ref->size, maxsize)
	   && is_gimple_reg_type (vr->type)
	   && !reverse_storage_order_for_component_p (vr->operands)
	   && !contains_storage_order_barrier_p (vr->operands)
	   && gimple_assign_single_p (def_stmt)
	   && CHAR_BIT == 8
	   && BITS_PER_UNIT == 8
	   && BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
	   /* native_encode and native_decode operate on arrays of bytes
	      and so fundamentally need a compile-time size and offset.  */
	   && maxsize.is_constant (&maxsizei)
	   && offset.is_constant (&offseti)
	   && (is_gimple_min_invariant (gimple_assign_rhs1 (def_stmt))
	       || (TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
		   && is_gimple_min_invariant (SSA_VAL (gimple_assign_rhs1 (def_stmt))))))
    {
      tree lhs = gimple_assign_lhs (def_stmt);
      tree base2;
      poly_int64 offset2, size2, maxsize2;
      HOST_WIDE_INT offset2i, size2i;
      bool reverse;
      gcc_assert (lhs_ref_ok);
      base2 = ao_ref_base (&lhs_ref);
      offset2 = lhs_ref.offset;
      size2 = lhs_ref.size;
      maxsize2 = lhs_ref.max_size;
      reverse = reverse_storage_order_for_component_p (lhs);
      if (base2
	  && !reverse
	  && !storage_order_barrier_p (lhs)
	  && known_eq (maxsize2, size2)
	  && adjust_offsets_for_equal_base_address (base, &offset,
						    base2, &offset2)
	  && offset.is_constant (&offseti)
	  && offset2.is_constant (&offset2i)
	  && size2.is_constant (&size2i))
	{
	  if (data->partial_defs.is_empty ()
	      && known_subrange_p (offseti, maxsizei, offset2, size2))
	    {
	      /* We support up to 512-bit values (for V8DFmode).  */
	      unsigned char buffer[65];
	      int len;

	      tree rhs = gimple_assign_rhs1 (def_stmt);
	      if (TREE_CODE (rhs) == SSA_NAME)
		rhs = SSA_VAL (rhs);
	      len = native_encode_expr (rhs,
					buffer, sizeof (buffer) - 1,
					(offseti - offset2i) / BITS_PER_UNIT);
	      if (len > 0 && len * BITS_PER_UNIT >= maxsizei)
		{
		  tree type = vr->type;
		  unsigned char *buf = buffer;
		  unsigned int amnt = 0;
		  /* Make sure to interpret in a type that has a range
		     covering the whole access size.  */
		  if (INTEGRAL_TYPE_P (vr->type)
		      && maxsizei != TYPE_PRECISION (vr->type))
		    type = build_nonstandard_integer_type (maxsizei,
							   TYPE_UNSIGNED (type));
		  if (BYTES_BIG_ENDIAN)
		    {
		      /* For big-endian native_encode_expr stored the rhs
			 such that the LSB of it is the LSB of buffer[len - 1].
			 That bit is stored into memory at position
			 offset2 + size2 - 1, i.e. in byte
			 base + (offset2 + size2 - 1) / BITS_PER_UNIT.
			 E.g. for offset2 1 and size2 14, rhs -1 and memory
			 previously cleared that is:
			 0        1
			 01111111|11111110
			 Now, if we want to extract offset 2 and size 12 from
			 it using native_interpret_expr (which actually works
			 for integral bitfield types in terms of byte size of
			 the mode), the native_encode_expr stored the value
			 into buffer as
			 XX111111|11111111
			 and returned len 2 (the X bits are outside of
			 precision).
			 Let sz be maxsize / BITS_PER_UNIT if not extracting
			 a bitfield, and GET_MODE_SIZE otherwise.
			 We need to align the LSB of the value we want to
			 extract as the LSB of buf[sz - 1].
			 The LSB from memory we need to read is at position
			 offset + maxsize - 1.  */
		      HOST_WIDE_INT sz = maxsizei / BITS_PER_UNIT;
		      if (INTEGRAL_TYPE_P (type))
			{
			  if (TYPE_MODE (type) != BLKmode)
			    sz = GET_MODE_SIZE (SCALAR_INT_TYPE_MODE (type));
			  else
			    sz = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
			}
		      amnt = ((unsigned HOST_WIDE_INT) offset2i + size2i
			      - offseti - maxsizei) % BITS_PER_UNIT;
		      if (amnt)
			shift_bytes_in_array_right (buffer, len, amnt);
		      amnt = ((unsigned HOST_WIDE_INT) offset2i + size2i
			      - offseti - maxsizei - amnt) / BITS_PER_UNIT;
		      if ((unsigned HOST_WIDE_INT) sz + amnt > (unsigned) len)
			len = 0;
		      else
			{
			  buf = buffer + len - sz - amnt;
			  len -= (buf - buffer);
			}
		    }
		  else
		    {
		      amnt = ((unsigned HOST_WIDE_INT) offset2i
			      - offseti) % BITS_PER_UNIT;
		      if (amnt)
			{
			  buffer[len] = 0;
			  shift_bytes_in_array_left (buffer, len + 1, amnt);
			  buf = buffer + 1;
			}
		    }
		  tree val = native_interpret_expr (type, buf, len);
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
		    return data->finish (ao_ref_alias_set (&lhs_ref),
					 ao_ref_base_alias_set (&lhs_ref), val);
		}
	    }
	  else if (ranges_known_overlap_p (offseti, maxsizei, offset2i,
					   size2i))
	    {
	      pd_data pd;
	      tree rhs = gimple_assign_rhs1 (def_stmt);
	      if (TREE_CODE (rhs) == SSA_NAME)
		rhs = SSA_VAL (rhs);
	      pd.rhs = rhs;
	      pd.rhs_off = 0;
	      pd.offset = offset2i;
	      pd.size = size2i;
	      return data->push_partial_def (pd, ao_ref_alias_set (&lhs_ref),
					     ao_ref_base_alias_set (&lhs_ref),
					     offseti, maxsizei);
	    }
	}
    }

  /* 4) Assignment from an SSA name which definition we may be able
     to access pieces from or we can combine to a larger entity.  */
  else if (known_eq (ref->size, maxsize)
	   && is_gimple_reg_type (vr->type)
	   && !reverse_storage_order_for_component_p (vr->operands)
	   && !contains_storage_order_barrier_p (vr->operands)
	   && gimple_assign_single_p (def_stmt)
	   && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
    {
      tree lhs = gimple_assign_lhs (def_stmt);
      tree base2;
      poly_int64 offset2, size2, maxsize2;
      HOST_WIDE_INT offset2i, size2i, offseti;
      bool reverse;
      gcc_assert (lhs_ref_ok);
      base2 = ao_ref_base (&lhs_ref);
      offset2 = lhs_ref.offset;
      size2 = lhs_ref.size;
      maxsize2 = lhs_ref.max_size;
      reverse = reverse_storage_order_for_component_p (lhs);
      tree def_rhs = gimple_assign_rhs1 (def_stmt);
      if (!reverse
	  && !storage_order_barrier_p (lhs)
	  && known_size_p (maxsize2)
	  && known_eq (maxsize2, size2)
	  && adjust_offsets_for_equal_base_address (base, &offset,
						    base2, &offset2))
	{
	  if (data->partial_defs.is_empty ()
	      && known_subrange_p (offset, maxsize, offset2, size2)
	      /* ???  We can't handle bitfield precision extracts without
		 either using an alternate type for the BIT_FIELD_REF and
		 then doing a conversion or possibly adjusting the offset
		 according to endianness.  */
	      && (! INTEGRAL_TYPE_P (vr->type)
		  || known_eq (ref->size, TYPE_PRECISION (vr->type)))
	      && multiple_p (ref->size, BITS_PER_UNIT))
	    {
	      tree val = NULL_TREE;
	      if (! INTEGRAL_TYPE_P (TREE_TYPE (def_rhs))
		  || type_has_mode_precision_p (TREE_TYPE (def_rhs)))
		{
		  gimple_match_op op (gimple_match_cond::UNCOND,
				      BIT_FIELD_REF, vr->type,
				      SSA_VAL (def_rhs),
				      bitsize_int (ref->size),
				      bitsize_int (offset - offset2));
		  val = vn_nary_build_or_lookup (&op);
		}
	      else if (known_eq (ref->size, size2))
		{
		  gimple_match_op op (gimple_match_cond::UNCOND,
				      VIEW_CONVERT_EXPR, vr->type,
				      SSA_VAL (def_rhs));
		  val = vn_nary_build_or_lookup (&op);
		}
	      if (val
		  && (TREE_CODE (val) != SSA_NAME
		      || ! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val)))
		return data->finish (ao_ref_alias_set (&lhs_ref),
				     ao_ref_base_alias_set (&lhs_ref), val);
	    }
	  else if (maxsize.is_constant (&maxsizei)
		   && offset.is_constant (&offseti)
		   && offset2.is_constant (&offset2i)
		   && size2.is_constant (&size2i)
		   && ranges_known_overlap_p (offset, maxsize, offset2, size2))
	    {
	      pd_data pd;
	      pd.rhs = SSA_VAL (def_rhs);
	      pd.rhs_off = 0;
	      pd.offset = offset2i;
	      pd.size = size2i;
	      return data->push_partial_def (pd, ao_ref_alias_set (&lhs_ref),
					     ao_ref_base_alias_set (&lhs_ref),
					     offseti, maxsizei);
	    }
	}
    }

  /* 4b) Assignment done via one of the vectorizer internal store
     functions where we may be able to access pieces from or we can
     combine to a larger entity.  */
  else if (known_eq (ref->size, maxsize)
	   && is_gimple_reg_type (vr->type)
	   && !reverse_storage_order_for_component_p (vr->operands)
	   && !contains_storage_order_barrier_p (vr->operands)
	   && is_gimple_call (def_stmt)
	   && gimple_call_internal_p (def_stmt)
	   && internal_store_fn_p (gimple_call_internal_fn (def_stmt)))
    {
      gcall *call = as_a <gcall *> (def_stmt);
      internal_fn fn = gimple_call_internal_fn (call);

      tree mask = NULL_TREE, len = NULL_TREE, bias = NULL_TREE;
      switch (fn)
	{
	case IFN_MASK_STORE:
	  mask = gimple_call_arg (call, internal_fn_mask_index (fn));
	  mask = vn_valueize (mask);
	  if (TREE_CODE (mask) != VECTOR_CST)
	    return (void *)-1;
	  break;
	case IFN_LEN_STORE:
	  {
	    int len_index = internal_fn_len_index (fn);
	    len = gimple_call_arg (call, len_index);
	    bias = gimple_call_arg (call, len_index + 1);
	    if (!tree_fits_uhwi_p (len) || !tree_fits_shwi_p (bias))
	      return (void *) -1;
	    break;
	  }
	default:
	  return (void *)-1;
	}
      tree def_rhs = gimple_call_arg (call,
				      internal_fn_stored_value_index (fn));
      def_rhs = vn_valueize (def_rhs);
      if (TREE_CODE (def_rhs) != VECTOR_CST)
	return (void *)-1;

      ao_ref_init_from_ptr_and_size (&lhs_ref,
				     vn_valueize (gimple_call_arg (call, 0)),
				     TYPE_SIZE_UNIT (TREE_TYPE (def_rhs)));
      tree base2;
      poly_int64 offset2, size2, maxsize2;
      HOST_WIDE_INT offset2i, size2i, offseti;
      base2 = ao_ref_base (&lhs_ref);
      offset2 = lhs_ref.offset;
      size2 = lhs_ref.size;
      maxsize2 = lhs_ref.max_size;
      if (known_size_p (maxsize2)
	  && known_eq (maxsize2, size2)
	  && adjust_offsets_for_equal_base_address (base, &offset,
						    base2, &offset2)
	  && maxsize.is_constant (&maxsizei)
	  && offset.is_constant (&offseti)
	  && offset2.is_constant (&offset2i)
	  && size2.is_constant (&size2i))
	{
	  if (!ranges_maybe_overlap_p (offset, maxsize, offset2, size2))
	    /* Poor-mans disambiguation.  */
	    return NULL;
	  else if (ranges_known_overlap_p (offset, maxsize, offset2, size2))
	    {
	      pd_data pd;
	      pd.rhs = def_rhs;
	      tree aa = gimple_call_arg (call, 1);
	      alias_set_type set = get_deref_alias_set (TREE_TYPE (aa));
	      tree vectype = TREE_TYPE (def_rhs);
	      unsigned HOST_WIDE_INT elsz
		= tree_to_uhwi (TYPE_SIZE (TREE_TYPE (vectype)));
	      if (mask)
		{
		  HOST_WIDE_INT start = 0, length = 0;
		  unsigned mask_idx = 0;
		  do
		    {
		      if (integer_zerop (VECTOR_CST_ELT (mask, mask_idx)))
			{
			  if (length != 0)
			    {
			      pd.rhs_off = start;
			      pd.offset = offset2i + start;
			      pd.size = length;
			      if (ranges_known_overlap_p
				    (offset, maxsize, pd.offset, pd.size))
				{
				  void *res = data->push_partial_def
					      (pd, set, set, offseti, maxsizei);
				  if (res != NULL)
				    return res;
				}
			    }
			  start = (mask_idx + 1) * elsz;
			  length = 0;
			}
		      else
			length += elsz;
		      mask_idx++;
		    }
		  while (known_lt (mask_idx, TYPE_VECTOR_SUBPARTS (vectype)));
		  if (length != 0)
		    {
		      pd.rhs_off = start;
		      pd.offset = offset2i + start;
		      pd.size = length;
		      if (ranges_known_overlap_p (offset, maxsize,
						  pd.offset, pd.size))
			return data->push_partial_def (pd, set, set,
						       offseti, maxsizei);
		    }
		}
	      else if (fn == IFN_LEN_STORE)
		{
		  pd.offset = offset2i;
		  pd.size = (tree_to_uhwi (len)
			     + -tree_to_shwi (bias)) * BITS_PER_UNIT;
		  if (BYTES_BIG_ENDIAN)
		    pd.rhs_off = pd.size - tree_to_uhwi (TYPE_SIZE (vectype));
		  else
		    pd.rhs_off = 0;
		  if (ranges_known_overlap_p (offset, maxsize,
					      pd.offset, pd.size))
		    return data->push_partial_def (pd, set, set,
						   offseti, maxsizei);
		}
	      else
		gcc_unreachable ();
	      return NULL;
	    }
	}
    }

  /* 5) For aggregate copies translate the reference through them if
     the copy kills ref.  */
  else if (data->vn_walk_kind == VN_WALKREWRITE
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

      gcc_assert (lhs_ref_ok);

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
      tree rhs1 = gimple_assign_rhs1 (def_stmt);
      copy_reference_ops_from_ref (rhs1, &rhs);

      /* Apply an extra offset to the inner MEM_REF of the RHS.  */
      bool force_no_tbaa = false;
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
	  /* When we have offsetted the RHS, reading only parts of it,
	     we can no longer use the original TBAA type, force alias-set
	     zero.  */
	  force_no_tbaa = true;
	}

      /* Save the operands since we need to use the original ones for
	 the hash entry we use.  */
      if (!data->saved_operands.exists ())
	data->saved_operands = vr->operands.copy ();

      /* We need to pre-pend vr->operands[0..i] to rhs.  */
      vec<vn_reference_op_s> old = vr->operands;
      if (i + 1 + rhs.length () > vr->operands.length ())
	vr->operands.safe_grow (i + 1 + rhs.length (), true);
      else
	vr->operands.truncate (i + 1 + rhs.length ());
      FOR_EACH_VEC_ELT (rhs, j, vro)
	vr->operands[i + 1 + j] = *vro;
      valueize_refs (&vr->operands);
      if (old == shared_lookup_references)
	shared_lookup_references = vr->operands;
      vr->hashcode = vn_reference_compute_hash (vr);

      /* Try folding the new reference to a constant.  */
      tree val = fully_constant_vn_reference_p (vr);
      if (val)
	{
	  if (data->partial_defs.is_empty ())
	    return data->finish (ao_ref_alias_set (&lhs_ref),
				 ao_ref_base_alias_set (&lhs_ref), val);
	  /* This is the only interesting case for partial-def handling
	     coming from targets that like to gimplify init-ctors as
	     aggregate copies from constant data like aarch64 for
	     PR83518.  */
	  if (maxsize.is_constant (&maxsizei) && known_eq (ref->size, maxsize))
	    {
	      pd_data pd;
	      pd.rhs = val;
	      pd.rhs_off = 0;
	      pd.offset = 0;
	      pd.size = maxsizei;
	      return data->push_partial_def (pd, ao_ref_alias_set (&lhs_ref),
					     ao_ref_base_alias_set (&lhs_ref),
					     0, maxsizei);
	    }
	}

      /* Continuing with partial defs isn't easily possible here, we
         have to find a full def from further lookups from here.  Probably
	 not worth the special-casing everywhere.  */
      if (!data->partial_defs.is_empty ())
	return (void *)-1;

      /* Adjust *ref from the new operands.  */
      ao_ref rhs1_ref;
      ao_ref_init (&rhs1_ref, rhs1);
      if (!ao_ref_init_from_vn_reference (&r,
					  force_no_tbaa ? 0
					  : ao_ref_alias_set (&rhs1_ref),
					  force_no_tbaa ? 0
					  : ao_ref_base_alias_set (&rhs1_ref),
					  vr->type, vr->operands))
	return (void *)-1;
      /* This can happen with bitfields.  */
      if (maybe_ne (ref->size, r.size))
	{
	  /* If the access lacks some subsetting simply apply that by
	     shortening it.  That in the end can only be successful
	     if we can pun the lookup result which in turn requires
	     exact offsets.  */
	  if (known_eq (r.size, r.max_size)
	      && known_lt (ref->size, r.size))
	    r.size = r.max_size = ref->size;
	  else
	    return (void *)-1;
	}
      *ref = r;
      vr->offset = r.offset;
      vr->max_size = r.max_size;

      /* Do not update last seen VUSE after translating.  */
      data->last_vuse_ptr = NULL;
      /* Invalidate the original access path since it now contains
         the wrong base.  */
      data->orig_ref.ref = NULL_TREE;
      /* Use the alias-set of this LHS for recording an eventual result.  */
      if (data->first_set == -2)
	{
	  data->first_set = ao_ref_alias_set (&lhs_ref);
	  data->first_base_set = ao_ref_base_alias_set (&lhs_ref);
	}

      /* Keep looking for the adjusted *REF / VR pair.  */
      return NULL;
    }

  /* 6) For memcpy copies translate the reference through them if the copy
     kills ref.  But we cannot (easily) do this translation if the memcpy is
     a storage order barrier, i.e. is equivalent to a VIEW_CONVERT_EXPR that
     can modify the storage order of objects (see storage_order_barrier_p).  */
  else if (data->vn_walk_kind == VN_WALKREWRITE
	   && is_gimple_reg_type (vr->type)
	   /* ???  Handle BCOPY as well.  */
	   && (gimple_call_builtin_p (def_stmt, BUILT_IN_MEMCPY)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMCPY_CHK)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMPCPY)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMPCPY_CHK)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMMOVE)
	       || gimple_call_builtin_p (def_stmt, BUILT_IN_MEMMOVE_CHK))
	   && (TREE_CODE (gimple_call_arg (def_stmt, 0)) == ADDR_EXPR
	       || TREE_CODE (gimple_call_arg (def_stmt, 0)) == SSA_NAME)
	   && (TREE_CODE (gimple_call_arg (def_stmt, 1)) == ADDR_EXPR
	       || TREE_CODE (gimple_call_arg (def_stmt, 1)) == SSA_NAME)
	   && (poly_int_tree_p (gimple_call_arg (def_stmt, 2), &copy_size)
	       || (TREE_CODE (gimple_call_arg (def_stmt, 2)) == SSA_NAME
		   && poly_int_tree_p (SSA_VAL (gimple_call_arg (def_stmt, 2)),
				       &copy_size)))
	   /* Handling this is more complicated, give up for now.  */
	   && data->partial_defs.is_empty ())
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
	  if (AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (lhs)))
	      && TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (TREE_TYPE (lhs))))
	    return (void *)-1;
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
	  if (AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (rhs)))
	      && TYPE_REVERSE_STORAGE_ORDER (TREE_TYPE (TREE_TYPE (rhs))))
	    return (void *)-1;
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
      if (TREE_CODE (rhs) == SSA_NAME)
	rhs = SSA_VAL (rhs);
      else if (TREE_CODE (rhs) != ADDR_EXPR)
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

      /* Save the operands since we need to use the original ones for
	 the hash entry we use.  */
      if (!data->saved_operands.exists ())
	data->saved_operands = vr->operands.copy ();

      /* Make room for 2 operands in the new reference.  */
      if (vr->operands.length () < 2)
	{
	  vec<vn_reference_op_s> old = vr->operands;
	  vr->operands.safe_grow_cleared (2, true);
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
	return data->finish (0, 0, val);

      /* Adjust *ref from the new operands.  */
      if (!ao_ref_init_from_vn_reference (&r, 0, 0, vr->type, vr->operands))
	return (void *)-1;
      /* This can happen with bitfields.  */
      if (maybe_ne (ref->size, r.size))
	return (void *)-1;
      *ref = r;
      vr->offset = r.offset;
      vr->max_size = r.max_size;

      /* Do not update last seen VUSE after translating.  */
      data->last_vuse_ptr = NULL;
      /* Invalidate the original access path since it now contains
         the wrong base.  */
      data->orig_ref.ref = NULL_TREE;
      /* Use the alias-set of this stmt for recording an eventual result.  */
      if (data->first_set == -2)
	{
	  data->first_set = 0;
	  data->first_base_set = 0;
	}

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
vn_reference_lookup_pieces (tree vuse, alias_set_type set,
			    alias_set_type base_set, tree type,
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
  shared_lookup_references.safe_grow (operands.length (), true);
  memcpy (shared_lookup_references.address (),
	  operands.address (),
	  sizeof (vn_reference_op_s)
	  * operands.length ());
  bool valueized_p;
  valueize_refs_1 (&shared_lookup_references, &valueized_p);
  vr1.operands = shared_lookup_references;
  vr1.type = type;
  vr1.set = set;
  vr1.base_set = base_set;
  /* We can pretend there's no extra info fed in since the ao_refs offset
     and max_size are computed only from the VN reference ops.  */
  vr1.offset = 0;
  vr1.max_size = -1;
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  if ((cst = fully_constant_vn_reference_p (&vr1)))
    return cst;

  vn_reference_lookup_1 (&vr1, vnresult);
  if (!*vnresult
      && kind != VN_NOWALK
      && vr1.vuse)
    {
      ao_ref r;
      unsigned limit = param_sccvn_max_alias_queries_per_access;
      vn_walk_cb_data data (&vr1, NULL_TREE, NULL, kind, true, NULL_TREE,
			    false);
      vec<vn_reference_op_s> ops_for_ref;
      if (!valueized_p)
	ops_for_ref = vr1.operands;
      else
	{
	  /* For ao_ref_from_mem we have to ensure only available SSA names
	     end up in base and the only convenient way to make this work
	     for PRE is to re-valueize with that in mind.  */
	  ops_for_ref.create (operands.length ());
	  ops_for_ref.quick_grow (operands.length ());
	  memcpy (ops_for_ref.address (),
		  operands.address (),
		  sizeof (vn_reference_op_s)
		  * operands.length ());
	  valueize_refs_1 (&ops_for_ref, &valueized_p, true);
	}
      if (ao_ref_init_from_vn_reference (&r, set, base_set, type,
					 ops_for_ref))
	*vnresult
	  = ((vn_reference_t)
	     walk_non_aliased_vuses (&r, vr1.vuse, true, vn_reference_lookup_2,
				     vn_reference_lookup_3, vuse_valueize,
				     limit, &data));
      if (ops_for_ref != shared_lookup_references)
	ops_for_ref.release ();
      gcc_checking_assert (vr1.operands == shared_lookup_references);
      if (*vnresult
	  && data.same_val
	  && (!(*vnresult)->result
	      || !operand_equal_p ((*vnresult)->result, data.same_val)))
	{
	  *vnresult = NULL;
	  return NULL_TREE;
	}
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
   we are looking up a store and treat it as having alias-set zero.
   *LAST_VUSE_PTR will be updated with the VUSE the value lookup succeeded.
   MASK is either NULL_TREE, or can be an INTEGER_CST if the result of the
   load is bitwise anded with MASK and so we are only interested in a subset
   of the bits and can ignore if the other bits are uninitialized or
   not initialized with constants.  When doing redundant store removal
   the caller has to set REDUNDANT_STORE_REMOVAL_P.  */

tree
vn_reference_lookup (tree op, tree vuse, vn_lookup_kind kind,
		     vn_reference_t *vnresult, bool tbaa_p,
		     tree *last_vuse_ptr, tree mask,
		     bool redundant_store_removal_p)
{
  vec<vn_reference_op_s> operands;
  struct vn_reference_s vr1;
  bool valueized_anything;

  if (vnresult)
    *vnresult = NULL;

  vr1.vuse = vuse_ssa_val (vuse);
  vr1.operands = operands
    = valueize_shared_reference_ops_from_ref (op, &valueized_anything);

  /* Handle &MEM[ptr + 5].b[1].c as POINTER_PLUS_EXPR.  Avoid doing
     this before the pass folding __builtin_object_size had a chance to run.  */
  if ((cfun->curr_properties & PROP_objsz)
      && operands[0].opcode == ADDR_EXPR
      && operands.last ().opcode == SSA_NAME)
    {
      poly_int64 off = 0;
      vn_reference_op_t vro;
      unsigned i;
      for (i = 1; operands.iterate (i, &vro); ++i)
	{
	  if (vro->opcode == SSA_NAME)
	    break;
	  else if (known_eq (vro->off, -1))
	    break;
	  off += vro->off;
	}
      if (i == operands.length () - 1
	  /* Make sure we the offset we accumulated in a 64bit int
	     fits the address computation carried out in target
	     offset precision.  */
	  && (off.coeffs[0]
	      == sext_hwi (off.coeffs[0], TYPE_PRECISION (sizetype))))
	{
	  gcc_assert (operands[i-1].opcode == MEM_REF);
	  tree ops[2];
	  ops[0] = operands[i].op0;
	  ops[1] = wide_int_to_tree (sizetype, off);
	  tree res = vn_nary_op_lookup_pieces (2, POINTER_PLUS_EXPR,
					       TREE_TYPE (op), ops, NULL);
	  if (res)
	    return res;
	  return NULL_TREE;
	}
    }

  vr1.type = TREE_TYPE (op);
  ao_ref op_ref;
  ao_ref_init (&op_ref, op);
  vr1.set = ao_ref_alias_set (&op_ref);
  vr1.base_set = ao_ref_base_alias_set (&op_ref);
  vr1.offset = 0;
  vr1.max_size = -1;
  vr1.hashcode = vn_reference_compute_hash (&vr1);
  if (mask == NULL_TREE)
    if (tree cst = fully_constant_vn_reference_p (&vr1))
      return cst;

  if (kind != VN_NOWALK && vr1.vuse)
    {
      vn_reference_t wvnresult;
      ao_ref r;
      unsigned limit = param_sccvn_max_alias_queries_per_access;
      auto_vec<vn_reference_op_s> ops_for_ref;
      if (valueized_anything)
	{
	  copy_reference_ops_from_ref (op, &ops_for_ref);
	  bool tem;
	  valueize_refs_1 (&ops_for_ref, &tem, true);
	}
      /* Make sure to use a valueized reference if we valueized anything.
         Otherwise preserve the full reference for advanced TBAA.  */
      if (!valueized_anything
	  || !ao_ref_init_from_vn_reference (&r, vr1.set, vr1.base_set,
					     vr1.type, ops_for_ref))
	{
	  ao_ref_init (&r, op);
	  /* Record the extra info we're getting from the full ref.  */
	  ao_ref_base (&r);
	  vr1.offset = r.offset;
	  vr1.max_size = r.max_size;
	}
      vn_walk_cb_data data (&vr1, r.ref ? NULL_TREE : op,
			    last_vuse_ptr, kind, tbaa_p, mask,
			    redundant_store_removal_p);

      wvnresult
	= ((vn_reference_t)
	   walk_non_aliased_vuses (&r, vr1.vuse, tbaa_p, vn_reference_lookup_2,
				   vn_reference_lookup_3, vuse_valueize, limit,
				   &data));
      gcc_checking_assert (vr1.operands == shared_lookup_references);
      if (wvnresult)
	{
	  gcc_assert (mask == NULL_TREE);
	  if (data.same_val
	      && (!wvnresult->result
		  || !operand_equal_p (wvnresult->result, data.same_val)))
	    return NULL_TREE;
	  if (vnresult)
	    *vnresult = wvnresult;
	  return wvnresult->result;
	}
      else if (mask)
	return data.masked_result;

      return NULL_TREE;
    }

  if (last_vuse_ptr)
    *last_vuse_ptr = vr1.vuse;
  if (mask)
    return NULL_TREE;
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
  tree lhs = gimple_call_lhs (call);
  /* For non-SSA return values the referece ops contain the LHS.  */
  vr->type = ((lhs && TREE_CODE (lhs) == SSA_NAME)
	      ? TREE_TYPE (lhs) : NULL_TREE);
  vr->punned = false;
  vr->set = 0;
  vr->base_set = 0;
  vr->offset = 0;
  vr->max_size = -1;
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

  vec<vn_reference_op_s> operands
    = valueize_shared_reference_ops_from_ref (op, &tem);
  /* Handle &MEM[ptr + 5].b[1].c as POINTER_PLUS_EXPR.  Avoid doing this
     before the pass folding __builtin_object_size had a chance to run.  */
  if ((cfun->curr_properties & PROP_objsz)
      && operands[0].opcode == ADDR_EXPR
      && operands.last ().opcode == SSA_NAME)
    {
      poly_int64 off = 0;
      vn_reference_op_t vro;
      unsigned i;
      for (i = 1; operands.iterate (i, &vro); ++i)
	{
	  if (vro->opcode == SSA_NAME)
	    break;
	  else if (known_eq (vro->off, -1))
	    break;
	  off += vro->off;
	}
      if (i == operands.length () - 1
	  /* Make sure we the offset we accumulated in a 64bit int
	     fits the address computation carried out in target
	     offset precision.  */
	  && (off.coeffs[0]
	      == sext_hwi (off.coeffs[0], TYPE_PRECISION (sizetype))))
	{
	  gcc_assert (operands[i-1].opcode == MEM_REF);
	  tree ops[2];
	  ops[0] = operands[i].op0;
	  ops[1] = wide_int_to_tree (sizetype, off);
	  vn_nary_op_insert_pieces (2, POINTER_PLUS_EXPR,
				    TREE_TYPE (op), ops, result,
				    VN_INFO (result)->value_id);
	  return;
	}
    }

  vr1 = XOBNEW (&vn_tables_obstack, vn_reference_s);
  if (TREE_CODE (result) == SSA_NAME)
    vr1->value_id = VN_INFO (result)->value_id;
  else
    vr1->value_id = get_or_alloc_constant_value_id (result);
  vr1->vuse = vuse_ssa_val (vuse);
  vr1->operands = operands.copy ();
  vr1->type = TREE_TYPE (op);
  vr1->punned = false;
  ao_ref op_ref;
  ao_ref_init (&op_ref, op);
  vr1->set = ao_ref_alias_set (&op_ref);
  vr1->base_set = ao_ref_base_alias_set (&op_ref);
  /* Specifically use an unknown extent here, we're not doing any lookup
     and assume the caller didn't either (or it went VARYING).  */
  vr1->offset = 0;
  vr1->max_size = -1;
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
vn_reference_insert_pieces (tree vuse, alias_set_type set,
			    alias_set_type base_set,
			    poly_int64 offset, poly_int64 max_size, tree type,
			    vec<vn_reference_op_s> operands,
			    tree result, unsigned int value_id)

{
  vn_reference_s **slot;
  vn_reference_t vr1;

  vr1 = XOBNEW (&vn_tables_obstack, vn_reference_s);
  vr1->value_id = value_id;
  vr1->vuse = vuse_ssa_val (vuse);
  vr1->operands = operands;
  valueize_refs (&vr1->operands);
  vr1->type = type;
  vr1->punned = false;
  vr1->set = set;
  vr1->base_set = base_set;
  vr1->offset = offset;
  vr1->max_size = max_size;
  vr1->hashcode = vn_reference_compute_hash (vr1);
  if (result && TREE_CODE (result) == SSA_NAME)
    result = SSA_VAL (result);
  vr1->result = result;
  vr1->result_vdef = NULL_TREE;

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

hashval_t
vn_nary_op_compute_hash (const vn_nary_op_t vno1)
{
  inchash::hash hstate;
  unsigned i;

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

/* Return the number of operands for a vn_nary ops structure from STMT.  */

unsigned int
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

void
init_vn_nary_op_from_stmt (vn_nary_op_t vno, gassign *stmt)
{
  unsigned i;

  vno->opcode = gimple_assign_rhs_code (stmt);
  vno->type = TREE_TYPE (gimple_assign_lhs (stmt));
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

  for (unsigned i = 0; i < vno->length; ++i)
    if (TREE_CODE (vno->op[i]) == SSA_NAME)
      vno->op[i] = SSA_VAL (vno->op[i]);

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
  init_vn_nary_op_from_stmt (vno1, as_a <gassign *> (stmt));
  return vn_nary_op_lookup_1 (vno1, vnresult);
}

/* Allocate a vn_nary_op_t with LENGTH operands on STACK.  */

vn_nary_op_t
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

/* Insert VNO into TABLE.  */

static vn_nary_op_t
vn_nary_op_insert_into (vn_nary_op_t vno, vn_nary_op_table_type *table)
{
  vn_nary_op_s **slot;

  gcc_assert (! vno->predicated_values
	      || (! vno->u.values->next
		  && vno->u.values->n == 1));

  for (unsigned i = 0; i < vno->length; ++i)
    if (TREE_CODE (vno->op[i]) == SSA_NAME)
      vno->op[i] = SSA_VAL (vno->op[i]);

  vno->hashcode = vn_nary_op_compute_hash (vno);
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
	      if (expressions_equal_p (val->result, nval->result))
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
		      else if (flag_checking)
			/* Shouldn't happen, we insert in RPO order.  */
			gcc_assert (!dominated_by_p (CDI_DOMINATORS,
						     val_bb, vno_bb));
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
  return vn_nary_op_insert_into (vno1, valid_info->nary);
}

/* Return whether we can track a predicate valid when PRED_E is executed.  */

static bool
can_track_predicate_on_edge (edge pred_e)
{
  /* ???  As we are currently recording the destination basic-block index in
     vn_pval.valid_dominated_by_p and using dominance for the
     validity check we cannot track predicates on all edges.  */
  if (single_pred_p (pred_e->dest))
    return true;
  /* Never record for backedges.  */
  if (pred_e->flags & EDGE_DFS_BACK)
    return false;
  /* When there's more than one predecessor we cannot track
     predicate validity based on the destination block.  The
     exception is when all other incoming edges sources are
     dominated by the destination block.  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, pred_e->dest->preds)
    if (e != pred_e && ! dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
      return false;
  return true;
}

static vn_nary_op_t
vn_nary_op_insert_pieces_predicated (unsigned int length, enum tree_code code,
				     tree type, tree *ops,
				     tree result, unsigned int value_id,
				     edge pred_e)
{
  gcc_assert (can_track_predicate_on_edge (pred_e));

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
  return vn_nary_op_insert_into (vno1, valid_info->nary);
}

static bool
dominated_by_p_w_unex (basic_block bb1, basic_block bb2, bool);

static tree
vn_nary_op_get_predicated_value (vn_nary_op_t vno, basic_block bb,
				 edge e = NULL)
{
  if (! vno->predicated_values)
    return vno->u.result;
  for (vn_pval *val = vno->u.values; val; val = val->next)
    for (unsigned i = 0; i < val->n; ++i)
      {
	basic_block cand
	  = BASIC_BLOCK_FOR_FN (cfun, val->valid_dominated_by_p[i]);
	/* Do not handle backedge executability optimistically since
	   when figuring out whether to iterate we do not consider
	   changed predication.
	   When asking for predicated values on an edge avoid looking
	   at edge executability for edges forward in our iteration
	   as well.  */
	if (e && (e->flags & EDGE_DFS_BACK))
	  {
	    if (dominated_by_p (CDI_DOMINATORS, bb, cand))
	      return val->result;
	  }
	else if (dominated_by_p_w_unex (bb, cand, false))
	  return val->result;
      }
  return NULL_TREE;
}

static tree
vn_nary_op_get_predicated_value (vn_nary_op_t vno, edge e)
{
  return vn_nary_op_get_predicated_value (vno, e->src, e);
}

/* Insert the rhs of STMT into the current hash table with a value number of
   RESULT.  */

static vn_nary_op_t
vn_nary_op_insert_stmt (gimple *stmt, tree result)
{
  vn_nary_op_t vno1
    = alloc_vn_nary_op (vn_nary_length_from_stmt (stmt),
			result, VN_INFO (result)->value_id);
  init_vn_nary_op_from_stmt (vno1, as_a <gassign *> (stmt));
  return vn_nary_op_insert_into (vno1, valid_info->nary);
}

/* Compute a hashcode for PHI operation VP1 and return it.  */

static inline hashval_t
vn_phi_compute_hash (vn_phi_t vp1)
{
  inchash::hash hstate;
  tree phi1op;
  tree type;
  edge e;
  edge_iterator ei;

  hstate.add_int (EDGE_COUNT (vp1->block->preds));
  switch (EDGE_COUNT (vp1->block->preds))
    {
    case 1:
      break;
    case 2:
      /* When this is a PHI node subject to CSE for different blocks
	 avoid hashing the block index.  */
      if (vp1->cclhs)
	break;
      /* Fallthru.  */
    default:
      hstate.add_int (vp1->block->index);
    }

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
	    /* Make sure both PHIs are classified as CSEable.  */
	    if (! vp1->cclhs || ! vp2->cclhs)
	      return false;

	    /* Rule out backedges into the PHI.  */
	    gcc_checking_assert
	      (vp1->block->loop_father->header != vp1->block
	       && vp2->block->loop_father->header != vp2->block);

	    /* If the PHI nodes do not have compatible types
	       they are not the same.  */
	    if (!types_compatible_p (vp1->type, vp2->type))
	      return false;

	    /* If the immediate dominator end in switch stmts multiple
	       values may end up in the same PHI arg via intermediate
	       CFG merges.  */
	    basic_block idom1
	      = get_immediate_dominator (CDI_DOMINATORS, vp1->block);
	    basic_block idom2
	      = get_immediate_dominator (CDI_DOMINATORS, vp2->block);
	    gcc_checking_assert (EDGE_COUNT (idom1->succs) == 2
				 && EDGE_COUNT (idom2->succs) == 2);

	    /* Verify the controlling stmt is the same.  */
	    gcond *last1 = as_a <gcond *> (*gsi_last_bb (idom1));
	    gcond *last2 = as_a <gcond *> (*gsi_last_bb (idom2));
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

	    /* Since we do not know which edge will be executed we have
	       to be careful when matching VN_TOP.  Be conservative and
	       only match VN_TOP == VN_TOP for now, we could allow
	       VN_TOP on the not prevailing PHI though.  See for example
	       PR102920.  */
	    if (! expressions_equal_p (vp1->phiargs[te1->dest_idx],
				       vp2->phiargs[te2->dest_idx], false)
		|| ! expressions_equal_p (vp1->phiargs[fe1->dest_idx],
					  vp2->phiargs[fe2->dest_idx], false))
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
  unsigned nargs = EDGE_COUNT (vp1->block->preds);
  for (unsigned i = 0; i < nargs; ++i)
    {
      tree phi1op = vp1->phiargs[i];
      tree phi2op = vp2->phiargs[i];
      if (phi1op == phi2op)
	continue;
      if (!expressions_equal_p (phi1op, phi2op, false))
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
	{
	  if (!virtual_operand_p (def)
	      && ssa_undefined_value_p (def, false))
	    def = VN_TOP;
	  else
	    def = SSA_VAL (def);
	}
      vp1->phiargs[e->dest_idx] = def;
    }
  vp1->type = TREE_TYPE (gimple_phi_result (phi));
  vp1->block = gimple_bb (phi);
  /* Extract values of the controlling condition.  */
  vp1->cclhs = NULL_TREE;
  vp1->ccrhs = NULL_TREE;
  if (EDGE_COUNT (vp1->block->preds) == 2
      && vp1->block->loop_father->header != vp1->block)
    {
      basic_block idom1 = get_immediate_dominator (CDI_DOMINATORS, vp1->block);
      if (EDGE_COUNT (idom1->succs) == 2)
	if (gcond *last1 = safe_dyn_cast <gcond *> (*gsi_last_bb (idom1)))
	  {
	    /* ???  We want to use SSA_VAL here.  But possibly not
	       allow VN_TOP.  */
	    vp1->cclhs = vn_valueize (gimple_cond_lhs (last1));
	    vp1->ccrhs = vn_valueize (gimple_cond_rhs (last1));
	  }
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
	{
	  if (!virtual_operand_p (def)
	      && ssa_undefined_value_p (def, false))
	    def = VN_TOP;
	  else
	    def = SSA_VAL (def);
	}
      vp1->phiargs[e->dest_idx] = def;
    }
  vp1->value_id = VN_INFO (result)->value_id;
  vp1->type = TREE_TYPE (gimple_phi_result (phi));
  vp1->block = gimple_bb (phi);
  /* Extract values of the controlling condition.  */
  vp1->cclhs = NULL_TREE;
  vp1->ccrhs = NULL_TREE;
  if (EDGE_COUNT (vp1->block->preds) == 2
      && vp1->block->loop_father->header != vp1->block)
    {
      basic_block idom1 = get_immediate_dominator (CDI_DOMINATORS, vp1->block);
      if (EDGE_COUNT (idom1->succs) == 2)
	if (gcond *last1 = safe_dyn_cast <gcond *> (*gsi_last_bb (idom1)))
	  {
	    /* ???  We want to use SSA_VAL here.  But possibly not
	       allow VN_TOP.  */
	    vp1->cclhs = vn_valueize (gimple_cond_lhs (last1));
	    vp1->ccrhs = vn_valueize (gimple_cond_rhs (last1));
	  }
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
   that are not executable.  When ALLOW_BACK is false consider not
   executable backedges as executable.  */

static bool
dominated_by_p_w_unex (basic_block bb1, basic_block bb2, bool allow_back)
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
	if ((e->flags & EDGE_EXECUTABLE)
	    || (!allow_back && (e->flags & EDGE_DFS_BACK)))
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
  if (EDGE_COUNT (bb2->succs) > 1)
    {
      edge succe = NULL;
      FOR_EACH_EDGE (e, ei, bb2->succs)
	if ((e->flags & EDGE_EXECUTABLE)
	    || (!allow_back && (e->flags & EDGE_DFS_BACK)))
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
		    && ((e->flags & EDGE_EXECUTABLE)
			|| (!allow_back && (e->flags & EDGE_DFS_BACK))))
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
    }
  /* Iterate to the single successor of bb2 with only a single executable
     incoming edge.  */
  else if (EDGE_COUNT (bb2->succs) == 1
	   && EDGE_COUNT (single_succ (bb2)->preds) > 1
	   /* Limit the number of edges we check, we should bring in
	      context from the iteration and compute the single
	      executable incoming edge when visiting a block.  */
	   && EDGE_COUNT (single_succ (bb2)->preds) < 8)
    {
      edge prede = NULL;
      FOR_EACH_EDGE (e, ei, single_succ (bb2)->preds)
	if ((e->flags & EDGE_EXECUTABLE)
	    || (!allow_back && (e->flags & EDGE_DFS_BACK)))
	  {
	    if (prede)
	      {
		prede = NULL;
		break;
	      }
	    prede = e;
	  }
      /* We might actually get to a query with BB2 not visited yet when
	 we're querying for a predicated value.  */
      if (prede && prede->src == bb2)
	{
	  bb2 = prede->dest;

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
  bool curr_undefined = false;
  bool curr_invariant = false;

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
      curr_invariant = is_gimple_min_invariant (currval);
      curr_undefined = (TREE_CODE (currval) == SSA_NAME
			&& !virtual_operand_p (currval)
			&& ssa_undefined_value_p (currval, false));
      if (currval != VN_TOP
	  && !curr_invariant
	  && !curr_undefined
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
      else if (currval != VN_TOP
	       && !curr_undefined
	       && TREE_CODE (to) == SSA_NAME
	       && !virtual_operand_p (to)
	       && ssa_undefined_value_p (to, false))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Forcing VARYING instead of changing "
		       "value number of ");
	      print_generic_expr (dump_file, from);
	      fprintf (dump_file, " from ");
	      print_generic_expr (dump_file, currval);
	      fprintf (dump_file, " (non-undefined) to ");
	      print_generic_expr (dump_file, to);
	      fprintf (dump_file, " (undefined)\n");
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
      && !(curr_undefined
	   && TREE_CODE (to) == SSA_NAME
	   && !virtual_operand_p (to)
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
      if (to != from
	  && currval != VN_TOP
	  && !curr_undefined
	  /* We do not want to allow lattice transitions from one value
	     to another since that may lead to not terminating iteration
	     (see PR95049).  Since there's no convenient way to check
	     for the allowed transition of VAL -> PHI (loop entry value,
	     same on two PHIs, to same PHI result) we restrict the check
	     to invariants.  */
	  && curr_invariant
	  && is_gimple_min_invariant (to))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, " forced VARYING");
	  to = from;
	}
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
valueized_wider_op (tree wide_type, tree op, bool allow_truncate)
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
  if (allow_truncate && TREE_CODE (op) == SSA_NAME)
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
    return wide_int_to_tree (wide_type, wi::to_widest (op));

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
	  /* We only handle sign-changes, zero-extension -> & mask or
	     sign-extension if we know the inner operation doesn't
	     overflow.  */
	  && (((TYPE_UNSIGNED (TREE_TYPE (rhs1))
		|| (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
		    && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (rhs1))))
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
	      /* When requiring a sign-extension we cannot model a
		 previous truncation with a single op so don't bother.  */
	      bool allow_truncate = TYPE_UNSIGNED (TREE_TYPE (rhs1));
	      /* Either we have the op widened available.  */
	      ops[0] = valueized_wider_op (type, gimple_assign_rhs1 (def),
					   allow_truncate);
	      if (ops[0])
		ops[1] = valueized_wider_op (type, gimple_assign_rhs2 (def),
					     allow_truncate);
	      if (ops[0] && ops[1])
		{
		  ops[0] = vn_nary_op_lookup_pieces
		      (2, gimple_assign_rhs_code (def), type, ops, NULL);
		  /* We have wider operation available.  */
		  if (ops[0]
		      /* If the leader is a wrapping operation we can
		         insert it for code hoisting w/o introducing
			 undefined overflow.  If it is not it has to
			 be available.  See PR86554.  */
		      && (TYPE_OVERFLOW_WRAPS (TREE_TYPE (ops[0]))
			  || (rpo_avail && vn_context_bb
			      && rpo_avail->eliminate_avail (vn_context_bb,
							     ops[0]))))
		    {
		      unsigned lhs_prec = TYPE_PRECISION (type);
		      unsigned rhs_prec = TYPE_PRECISION (TREE_TYPE (rhs1));
		      if (lhs_prec == rhs_prec
			  || (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
			      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (rhs1))))
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
      break;
    case BIT_AND_EXPR:
      if (INTEGRAL_TYPE_P (type)
	  && TREE_CODE (rhs1) == SSA_NAME
	  && TREE_CODE (gimple_assign_rhs2 (stmt)) == INTEGER_CST
	  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1)
	  && default_vn_walk_kind != VN_NOWALK
	  && CHAR_BIT == 8
	  && BITS_PER_UNIT == 8
	  && BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
	  && TYPE_PRECISION (type) <= vn_walk_cb_data::bufsize * BITS_PER_UNIT
	  && !integer_all_onesp (gimple_assign_rhs2 (stmt))
	  && !integer_zerop (gimple_assign_rhs2 (stmt)))
	{
	  gassign *ass = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (rhs1));
	  if (ass
	      && !gimple_has_volatile_ops (ass)
	      && vn_get_stmt_kind (ass) == VN_REFERENCE)
	    {
	      tree last_vuse = gimple_vuse (ass);
	      tree op = gimple_assign_rhs1 (ass);
	      tree result = vn_reference_lookup (op, gimple_vuse (ass),
						 default_vn_walk_kind,
						 NULL, true, &last_vuse,
						 gimple_assign_rhs2 (stmt));
	      if (result
		  && useless_type_conversion_p (TREE_TYPE (result),
						TREE_TYPE (op)))
		return set_ssa_val_to (lhs, result);
	    }
	}
      break;
    case TRUNC_DIV_EXPR:
      if (TYPE_UNSIGNED (type))
	break;
      /* Fallthru.  */
    case RDIV_EXPR:
    case MULT_EXPR:
      /* Match up ([-]a){/,*}([-])b with v=a{/,*}b, replacing it with -v.  */
      if (! HONOR_SIGN_DEPENDENT_ROUNDING (type))
	{
	  tree rhs[2];
	  rhs[0] = rhs1;
	  rhs[1] = gimple_assign_rhs2 (stmt);
	  for (unsigned i = 0; i <= 1; ++i)
	    {
	      unsigned j = i == 0 ? 1 : 0;
	      tree ops[2];
	      gimple_match_op match_op (gimple_match_cond::UNCOND,
					NEGATE_EXPR, type, rhs[i]);
	      ops[i] = vn_nary_build_or_lookup_1 (&match_op, false, true);
	      ops[j] = rhs[j];
	      if (ops[i]
		  && (ops[0] = vn_nary_op_lookup_pieces (2, code,
							 type, ops, NULL)))
		{
		  gimple_match_op match_op (gimple_match_cond::UNCOND,
					    NEGATE_EXPR, type, ops[0]);
		  result = vn_nary_build_or_lookup_1 (&match_op, true, false);
		  if (result)
		    {
		      bool changed = set_ssa_val_to (lhs, result);
		      vn_nary_op_insert_stmt (stmt, result);
		      return changed;
		    }
		}
	    }
	}
      break;
    case LSHIFT_EXPR:
      /* For X << C, use the value number of X * (1 << C).  */
      if (INTEGRAL_TYPE_P (type)
	  && TYPE_OVERFLOW_WRAPS (type)
	  && !TYPE_SATURATING (type))
	{
	  tree rhs2 = gimple_assign_rhs2 (stmt);
	  if (TREE_CODE (rhs2) == INTEGER_CST
	      && tree_fits_uhwi_p (rhs2)
	      && tree_to_uhwi (rhs2) < TYPE_PRECISION (type))
	    {
	      wide_int w = wi::set_bit_in_zero (tree_to_uhwi (rhs2),
						TYPE_PRECISION (type));
	      gimple_match_op match_op (gimple_match_cond::UNCOND,
					MULT_EXPR, type, rhs1,
					wide_int_to_tree (type, w));
	      result = vn_nary_build_or_lookup (&match_op);
	      if (result)
		{
		  bool changed = set_ssa_val_to (lhs, result);
		  if (TREE_CODE (result) == SSA_NAME)
		    vn_nary_op_insert_stmt (stmt, result);
		  return changed;
		}
	    }
	}
      break;
    default:
      break;
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
  modref_summary *summary;

  /* Non-ssa lhs is handled in copy_reference_ops_from_call.  */
  if (lhs && TREE_CODE (lhs) != SSA_NAME)
    lhs = NULL_TREE;

  vn_reference_lookup_call (stmt, &vnresult, &vr1);

  /* If the lookup did not succeed for pure functions try to use
     modref info to find a candidate to CSE to.  */
  const unsigned accesses_limit = 8;
  if (!vnresult
      && !vdef
      && lhs
      && gimple_vuse (stmt)
      && (((summary = get_modref_function_summary (stmt, NULL))
	   && !summary->global_memory_read
	   && summary->load_accesses < accesses_limit)
	  || gimple_call_flags (stmt) & ECF_CONST))
    {
      /* First search if we can do someting useful and build a
	 vector of all loads we have to check.  */
      bool unknown_memory_access = false;
      auto_vec<ao_ref, accesses_limit> accesses;
      unsigned load_accesses = summary ? summary->load_accesses : 0;
      if (!unknown_memory_access)
	/* Add loads done as part of setting up the call arguments.
	   That's also necessary for CONST functions which will
	   not have a modref summary.  */
	for (unsigned i = 0; i < gimple_call_num_args (stmt); ++i)
	  {
	    tree arg = gimple_call_arg (stmt, i);
	    if (TREE_CODE (arg) != SSA_NAME
		&& !is_gimple_min_invariant (arg))
	      {
		if (accesses.length () >= accesses_limit - load_accesses)
		  {
		    unknown_memory_access = true;
		    break;
		  }
		accesses.quick_grow (accesses.length () + 1);
		ao_ref_init (&accesses.last (), arg);
	      }
	  }
      if (summary && !unknown_memory_access)
	{
	  /* Add loads as analyzed by IPA modref.  */
	  for (auto base_node : summary->loads->bases)
	    if (unknown_memory_access)
	      break;
	    else for (auto ref_node : base_node->refs)
	      if (unknown_memory_access)
		break;
	      else for (auto access_node : ref_node->accesses)
		{
		  accesses.quick_grow (accesses.length () + 1);
		  ao_ref *r = &accesses.last ();
		  if (!access_node.get_ao_ref (stmt, r))
		    {
		      /* Initialize a ref based on the argument and
			 unknown offset if possible.  */
		      tree arg = access_node.get_call_arg (stmt);
		      if (arg && TREE_CODE (arg) == SSA_NAME)
			arg = SSA_VAL (arg);
		      if (arg
			  && TREE_CODE (arg) == ADDR_EXPR
			  && (arg = get_base_address (arg))
			  && DECL_P (arg))
			{
			  ao_ref_init (r, arg);
			  r->ref = NULL_TREE;
			  r->base = arg;
			}
		      else
			{
			  unknown_memory_access = true;
			  break;
			}
		    }
		  r->base_alias_set = base_node->base;
		  r->ref_alias_set = ref_node->ref;
		}
	}

      /* Walk the VUSE->VDEF chain optimistically trying to find an entry
	 for the call in the hashtable.  */
      unsigned limit = (unknown_memory_access
			? 0
			: (param_sccvn_max_alias_queries_per_access
			   / (accesses.length () + 1)));
      tree saved_vuse = vr1.vuse;
      hashval_t saved_hashcode = vr1.hashcode;
      while (limit > 0 && !vnresult && !SSA_NAME_IS_DEFAULT_DEF (vr1.vuse))
	{
	  vr1.hashcode = vr1.hashcode - SSA_NAME_VERSION (vr1.vuse);
	  gimple *def = SSA_NAME_DEF_STMT (vr1.vuse);
	  /* ???  We could use fancy stuff like in walk_non_aliased_vuses, but
	     do not bother for now.  */
	  if (is_a <gphi *> (def))
	    break;
	  vr1.vuse = vuse_ssa_val (gimple_vuse (def));
	  vr1.hashcode = vr1.hashcode + SSA_NAME_VERSION (vr1.vuse);
	  vn_reference_lookup_1 (&vr1, &vnresult);
	  limit--;
	}

      /* If we found a candidate to CSE to verify it is valid.  */
      if (vnresult && !accesses.is_empty ())
	{
	  tree vuse = vuse_ssa_val (gimple_vuse (stmt));
	  while (vnresult && vuse != vr1.vuse)
	    {
	      gimple *def = SSA_NAME_DEF_STMT (vuse);
	      for (auto &ref : accesses)
		{
		  /* ???  stmt_may_clobber_ref_p_1 does per stmt constant
		     analysis overhead that we might be able to cache.  */
		  if (stmt_may_clobber_ref_p_1 (def, &ref, true))
		    {
		      vnresult = NULL;
		      break;
		    }
		}
	      vuse = vuse_ssa_val (gimple_vuse (def));
	    }
	}
      vr1.vuse = saved_vuse;
      vr1.hashcode = saved_hashcode;
    }

  if (vnresult)
    {
      if (vdef)
	{
	  if (vnresult->result_vdef)
	    changed |= set_ssa_val_to (vdef, vnresult->result_vdef);
	  else if (!lhs && gimple_call_lhs (stmt))
	    /* If stmt has non-SSA_NAME lhs, value number the vdef to itself,
	       as the call still acts as a lhs store.  */
	    changed |= set_ssa_val_to (vdef, vdef);
	  else
	    /* If the call was discovered to be pure or const reflect
	       that as far as possible.  */
	    changed |= set_ssa_val_to (vdef,
				       vuse_ssa_val (gimple_vuse (stmt)));
	}

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
		      & (ECF_CONST | ECF_PURE))
		  /* If stmt has non-SSA_NAME lhs, value number the
		     vdef to itself, as the call still acts as a lhs
		     store.  */
		  && (lhs || gimple_call_lhs (stmt) == NULL_TREE))
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
      vr2->punned = vr1.punned;
      vr2->set = vr1.set;
      vr2->offset = vr1.offset;
      vr2->max_size = vr1.max_size;
      vr2->base_set = vr1.base_set;
      vr2->hashcode = vr1.hashcode;
      vr2->result = lhs;
      vr2->result_vdef = vdef_val;
      vr2->value_id = 0;
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
  tree result;
  vn_reference_t res;

  tree vuse = gimple_vuse (stmt);
  tree last_vuse = vuse;
  result = vn_reference_lookup (op, vuse, default_vn_walk_kind, &res, true, &last_vuse);

  /* We handle type-punning through unions by value-numbering based
     on offset and size of the access.  Be prepared to handle a
     type-mismatch here via creating a VIEW_CONVERT_EXPR.  */
  if (result
      && !useless_type_conversion_p (TREE_TYPE (result), TREE_TYPE (op)))
    {
      if (CONSTANT_CLASS_P (result))
	result = const_unop (VIEW_CONVERT_EXPR, TREE_TYPE (op), result);
      else
	{
	  /* We will be setting the value number of lhs to the value number
	     of VIEW_CONVERT_EXPR <TREE_TYPE (result)> (result).
	     So first simplify and lookup this expression to see if it
	     is already available.  */
	  gimple_match_op res_op (gimple_match_cond::UNCOND,
				  VIEW_CONVERT_EXPR, TREE_TYPE (op), result);
	  result = vn_nary_build_or_lookup (&res_op);
	  if (result
	      && TREE_CODE (result) == SSA_NAME
	      && VN_INFO (result)->needs_insertion)
	    /* Track whether this is the canonical expression for different
	       typed loads.  We use that as a stopgap measure for code
	       hoisting when dealing with floating point loads.  */
	    res->punned = true;
	}

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
      if (vuse && SSA_VAL (last_vuse) != SSA_VAL (vuse))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Using extra use virtual operand ");
	      print_generic_expr (dump_file, last_vuse);
	      fprintf (dump_file, "\n");
	    }
	  vn_reference_insert (op, lhs, vuse, NULL_TREE);
	}
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
	  ao_ref lhs_ref;
	  ao_ref_init (&lhs_ref, lhs);
	  alias_set_type set = ao_ref_alias_set (&lhs_ref);
	  alias_set_type base_set = ao_ref_base_alias_set (&lhs_ref);
	  if ((vnresult->set != set
	       && ! alias_set_subset_of (set, vnresult->set))
	      || (vnresult->base_set != base_set
		  && ! alias_set_subset_of (base_set, vnresult->base_set)))
	    resultsame = false;
	}
    }

  if (!resultsame)
    {
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
	  vn_reference_lookup (assign, vuse, VN_NOWALK, &vnresult, false);
	  if (!vnresult)
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
  bool seen_undef_visited = false;
  tree backedge_val = NULL_TREE;
  bool seen_non_backedge = false;
  tree sameval_base = NULL_TREE;
  poly_int64 soff, doff;
  unsigned n_executable = 0;
  edge sameval_e = NULL;

  /* TODO: We could check for this in initialization, and replace this
     with a gcc_assert.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (PHI_RESULT (phi)))
    return set_ssa_val_to (PHI_RESULT (phi), PHI_RESULT (phi));

  /* We track whether a PHI was CSEd to avoid excessive iterations
     that would be necessary only because the PHI changed arguments
     but not value.  */
  if (!inserted)
    gimple_set_plf (phi, GF_PLF_1, false);

  basic_block bb = gimple_bb (phi);

  /* For the equivalence handling below make sure to first process an
     edge with a non-constant.  */
  auto_vec<edge, 2> preds;
  preds.reserve_exact (EDGE_COUNT (bb->preds));
  bool seen_nonconstant = false;
  for (unsigned i = 0; i < EDGE_COUNT (bb->preds); ++i)
    {
      edge e = EDGE_PRED (bb, i);
      preds.quick_push (e);
      if (!seen_nonconstant)
	{
	  tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);
	  if (TREE_CODE (def) == SSA_NAME)
	    {
	      seen_nonconstant = true;
	      if (i != 0)
		std::swap (preds[0], preds[i]);
	    }
	}
    }

  /* See if all non-TOP arguments have the same value.  TOP is
     equivalent to everything, so we can ignore it.  */
  for (edge e : preds)
    if (e->flags & EDGE_EXECUTABLE)
      {
	tree def = PHI_ARG_DEF_FROM_EDGE (phi, e);

	if (def == PHI_RESULT (phi))
	  continue;
	++n_executable;
	bool visited = true;
	if (TREE_CODE (def) == SSA_NAME)
	  {
	    tree val = SSA_VAL (def, &visited);
	    if (SSA_NAME_IS_DEFAULT_DEF (def))
	      visited = true;
	    if (!backedges_varying_p || !(e->flags & EDGE_DFS_BACK))
	      def = val;
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
	  {
	    if (!seen_undef
		/* Avoid having not visited undefined defs if we also have
		   a visited one.  */
		|| (!seen_undef_visited && visited))
	      {
		seen_undef = def;
		seen_undef_visited = visited;
	      }
	  }
	else if (sameval == VN_TOP)
	  {
	    sameval = def;
	    sameval_e = e;
	  }
	else if (expressions_equal_p (def, sameval))
	  sameval_e = NULL;
	else if (virtual_operand_p (def))
	  {
	    sameval = NULL_TREE;
	    break;
	  }
	else
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
	    /* There's also the possibility to use equivalences.  */
	    if (!FLOAT_TYPE_P (TREE_TYPE (def))
		/* But only do this if we didn't force any of sameval or
		   val to VARYING because of backedge processing rules.  */
		&& (TREE_CODE (sameval) != SSA_NAME
		    || SSA_VAL (sameval) == sameval)
		&& (TREE_CODE (def) != SSA_NAME || SSA_VAL (def) == def))
	      {
		vn_nary_op_t vnresult;
		tree ops[2];
		ops[0] = def;
		ops[1] = sameval;
		/* Canonicalize the operands order for eq below. */
		if (tree_swap_operands_p (ops[0], ops[1]))
		  std::swap (ops[0], ops[1]);
		tree val = vn_nary_op_lookup_pieces (2, EQ_EXPR,
						     boolean_type_node,
						     ops, &vnresult);
		if (! val && vnresult && vnresult->predicated_values)
		  {
		    val = vn_nary_op_get_predicated_value (vnresult, e);
		    if (val && integer_truep (val)
			&& !(sameval_e && (sameval_e->flags & EDGE_DFS_BACK)))
		      {
			if (dump_file && (dump_flags & TDF_DETAILS))
			  {
			    fprintf (dump_file, "Predication says ");
			    print_generic_expr (dump_file, def, TDF_NONE);
			    fprintf (dump_file, " and ");
			    print_generic_expr (dump_file, sameval, TDF_NONE);
			    fprintf (dump_file, " are equal on edge %d -> %d\n",
				     e->src->index, e->dest->index);
			  }
			continue;
		      }
		  }
	      }
	    sameval = NULL_TREE;
	    break;
	  }
      }

  /* If the value we want to use is flowing over the backedge and we
     should take it as VARYING but it has a non-VARYING value drop to
     VARYING.
     If we value-number a virtual operand never value-number to the
     value from the backedge as that confuses the alias-walking code.
     See gcc.dg/torture/pr87176.c.  If the value is the same on a
     non-backedge everything is OK though.  */
  bool visited_p;
  if ((backedge_val
       && !seen_non_backedge
       && TREE_CODE (backedge_val) == SSA_NAME
       && sameval == backedge_val
       && (SSA_NAME_IS_VIRTUAL_OPERAND (backedge_val)
	   || SSA_VAL (backedge_val) != backedge_val))
      /* Do not value-number a virtual operand to sth not visited though
	 given that allows us to escape a region in alias walking.  */
      || (sameval
	  && TREE_CODE (sameval) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (sameval)
	  && SSA_NAME_IS_VIRTUAL_OPERAND (sameval)
	  && (SSA_VAL (sameval, &visited_p), !visited_p)))
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
    result = (seen_undef && seen_undef_visited) ? seen_undef : sameval;
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
      if ((/* Calls to the same function with the same vuse
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
	       && default_vn_walk_kind == VN_WALK))
	  /* Do not process .DEFERRED_INIT since that confuses uninit
	     analysis.  */
	  && !gimple_call_internal_p (call_stmt, IFN_DEFERRED_INIT))
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

/* Return the maximum constant value id we have ever seen.  */

unsigned int
get_max_constant_value_id (void)
{
  return -next_constant_value_id;
}

/* Return the next unique value id.  */

unsigned int
get_next_value_id (void)
{
  gcc_checking_assert ((int)next_value_id > 0);
  return next_value_id++;
}

/* Return the next unique value id for constants.  */

unsigned int
get_next_constant_value_id (void)
{
  gcc_checking_assert (next_constant_value_id < 0);
  return next_constant_value_id--;
}


/* Compare two expressions E1 and E2 and return true if they are equal.
   If match_vn_top_optimistically is true then VN_TOP is equal to anything,
   otherwise VN_TOP only matches VN_TOP.  */

bool
expressions_equal_p (tree e1, tree e2, bool match_vn_top_optimistically)
{
  /* The obvious case.  */
  if (e1 == e2)
    return true;

  /* If either one is VN_TOP consider them equal.  */
  if (match_vn_top_optimistically
      && (e1 == VN_TOP || e2 == VN_TOP))
    return true;

  /* If only one of them is null, they cannot be equal.  While in general
     this should not happen for operations like TARGET_MEM_REF some
     operands are optional and an identity value we could substitute
     has differing semantics.  */
  if (!e1 || !e2)
    return false;

  /* SSA_NAME compare pointer equal.  */
  if (TREE_CODE (e1) == SSA_NAME || TREE_CODE (e2) == SSA_NAME)
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
      else if (INTEGRAL_TYPE_P (type) && TYPE_OVERFLOW_TRAPS (type))
	honor_trapv = true;
    }
  if (nary->length >= 2)
    rhs2 = nary->op[1];
  ret = operation_could_trap_helper_p (nary->opcode, fp_operation,
				       honor_trapv, honor_nans, honor_snans,
				       rhs2, &handled);
  if (handled && ret)
    return true;

  for (i = 0; i < nary->length; ++i)
    if (tree_could_trap_p (nary->op[i]))
      return true;

  return false;
}

/* Return true if the reference operation REF may trap.  */

bool
vn_reference_may_trap (vn_reference_t ref)
{
  switch (ref->operands[0].opcode)
    {
    case MODIFY_EXPR:
    case CALL_EXPR:
      /* We do not handle calls.  */
      return true;
    case ADDR_EXPR:
      /* And toplevel address computations never trap.  */
      return false;
    default:;
    }

  vn_reference_op_t op;
  unsigned i;
  FOR_EACH_VEC_ELT (ref->operands, i, op)
    {
      switch (op->opcode)
	{
	case WITH_SIZE_EXPR:
	case TARGET_MEM_REF:
	  /* Always variable.  */
	  return true;
	case COMPONENT_REF:
	  if (op->op1 && TREE_CODE (op->op1) == SSA_NAME)
	    return true;
	  break;
	case ARRAY_RANGE_REF:
	  if (TREE_CODE (op->op0) == SSA_NAME)
	    return true;
	  break;
	case ARRAY_REF:
	  {
	    if (TREE_CODE (op->op0) != INTEGER_CST)
	      return true;

	    /* !in_array_bounds   */
	    tree domain_type = TYPE_DOMAIN (ref->operands[i+1].type);
	    if (!domain_type)
	      return true;

	    tree min = op->op1;
	    tree max = TYPE_MAX_VALUE (domain_type);
	    if (!min
		|| !max
		|| TREE_CODE (min) != INTEGER_CST
		|| TREE_CODE (max) != INTEGER_CST)
	      return true;

	    if (tree_int_cst_lt (op->op0, min)
		|| tree_int_cst_lt (max, op->op0))
	      return true;

	    break;
	  }
	case MEM_REF:
	  /* Nothing interesting in itself, the base is separate.  */
	  break;
	/* The following are the address bases.  */
	case SSA_NAME:
	  return true;
	case ADDR_EXPR:
	  if (op->op0)
	    return tree_could_trap_p (TREE_OPERAND (op->op0, 0));
	  return false;
	default:;
	}
    }
  return false;
}

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
	{
	  tree av = avail[SSA_NAME_VERSION (valnum)];
	  /* When PRE discovers a new redundancy there's no way to unite
	     the value classes so it instead inserts a copy old-val = new-val.
	     Look through such copies here, providing one more level of
	     simplification at elimination time.  */
	  gassign *ass;
	  if (av && (ass = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (av))))
	    if (gimple_assign_rhs_class (ass) == GIMPLE_SINGLE_RHS)
	      {
		tree rhs1 = gimple_assign_rhs1 (ass);
		if (CONSTANT_CLASS_P (rhs1)
		    || (TREE_CODE (rhs1) == SSA_NAME
			&& !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs1)))
		  av = rhs1;
	      }
	  return av;
	}
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
	avail.safe_grow_cleared (SSA_NAME_VERSION (valnum) + 1, true);
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
	  && gimple_assign_rhs_code (stmt) != NEGATE_EXPR
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
      vn_ssa_aux_t vn_info = VN_INFO (res);
      vn_info->valnum = val;
      vn_info->visited = true;
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
	  vn_ssa_aux_t vn_info;
	  if (val != VN_TOP
	      && TREE_CODE (val) == SSA_NAME
	      && (vn_info = VN_INFO (val), true)
	      && vn_info->needs_insertion
	      && vn_info->expr != NULL
	      && (sprime = eliminate_insert (b, gsi, val)) != NULL_TREE)
	    eliminate_push_avail (b, sprime);
	}

      /* If this now constitutes a copy duplicate points-to
	 and range info appropriately.  This is especially
	 important for inserted code.  */
      if (sprime
	  && TREE_CODE (sprime) == SSA_NAME)
	maybe_duplicate_ssa_info_at_copy (lhs, sprime);

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
	     then there is nothing to do.  Likewise if we run into
	     inserted code that needed a conversion because of
	     our type-agnostic value-numbering of loads.  */
	  if ((gimple_assign_single_p (stmt)
	       || (is_gimple_assign (stmt)
		   && (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
		       || gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR)))
	      && sprime == gimple_assign_rhs1 (stmt))
	    return;

	  /* Else replace its RHS.  */
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

	  bool can_make_abnormal_goto = (is_gimple_call (stmt)
					 && stmt_can_make_abnormal_goto (stmt));
	  gimple *orig_stmt = stmt;
	  if (!useless_type_conversion_p (TREE_TYPE (lhs),
					  TREE_TYPE (sprime)))
	    {
	      /* We preserve conversions to but not from function or method
		 types.  This asymmetry makes it necessary to re-instantiate
		 conversions here.  */
	      if (POINTER_TYPE_P (TREE_TYPE (lhs))
		  && FUNC_OR_METHOD_TYPE_P (TREE_TYPE (TREE_TYPE (lhs))))
		sprime = fold_convert (TREE_TYPE (lhs), sprime);
	      else
		gcc_unreachable ();
	    }
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
      && (TREE_CODE (gimple_assign_lhs (stmt)) != VAR_DECL
	  || !DECL_HARD_REGISTER (gimple_assign_lhs (stmt)))
      && (TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME
	  || is_gimple_min_invariant (gimple_assign_rhs1 (stmt))))
    {
      tree rhs = gimple_assign_rhs1 (stmt);
      vn_reference_t vnresult;
      /* ???  gcc.dg/torture/pr91445.c shows that we lookup a boolean
         typed load of a byte known to be 0x11 as 1 so a store of
	 a boolean 1 is detected as redundant.  Because of this we
	 have to make sure to lookup with a ref where its size
	 matches the precision.  */
      tree lookup_lhs = lhs;
      if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  && (TREE_CODE (lhs) != COMPONENT_REF
	      || !DECL_BIT_FIELD_TYPE (TREE_OPERAND (lhs, 1)))
	  && !type_has_mode_precision_p (TREE_TYPE (lhs)))
	{
	  if (TREE_CODE (TREE_TYPE (lhs)) == BITINT_TYPE
	      && TYPE_PRECISION (TREE_TYPE (lhs)) > MAX_FIXED_MODE_SIZE)
	    lookup_lhs = NULL_TREE;
	  else if (TREE_CODE (lhs) == COMPONENT_REF
		   || TREE_CODE (lhs) == MEM_REF)
	    {
	      tree ltype = build_nonstandard_integer_type
				(TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (lhs))),
				 TYPE_UNSIGNED (TREE_TYPE (lhs)));
	      if (TREE_CODE (lhs) == COMPONENT_REF)
		{
		  tree foff = component_ref_field_offset (lhs);
		  tree f = TREE_OPERAND (lhs, 1);
		  if (!poly_int_tree_p (foff))
		    lookup_lhs = NULL_TREE;
		  else
		    lookup_lhs = build3 (BIT_FIELD_REF, ltype,
					 TREE_OPERAND (lhs, 0),
					 TYPE_SIZE (TREE_TYPE (lhs)),
					 bit_from_pos
					   (foff, DECL_FIELD_BIT_OFFSET (f)));
		}
	      else
		lookup_lhs = build2 (MEM_REF, ltype,
				     TREE_OPERAND (lhs, 0),
				     TREE_OPERAND (lhs, 1));
	    }
	  else
	    lookup_lhs = NULL_TREE;
	}
      tree val = NULL_TREE;
      if (lookup_lhs)
	val = vn_reference_lookup (lookup_lhs, gimple_vuse (stmt),
				   VN_WALKREWRITE, &vnresult, false,
				   NULL, NULL_TREE, true);
      if (TREE_CODE (rhs) == SSA_NAME)
	rhs = VN_INFO (rhs)->valnum;
      if (val
	  && (operand_equal_p (val, rhs, 0)
	      /* Due to the bitfield lookups above we can get bit
		 interpretations of the same RHS as values here.  Those
		 are redundant as well.  */
	      || (TREE_CODE (val) == SSA_NAME
		  && gimple_assign_single_p (SSA_NAME_DEF_STMT (val))
		  && (val = gimple_assign_rhs1 (SSA_NAME_DEF_STMT (val)))
		  && TREE_CODE (val) == VIEW_CONVERT_EXPR
		  && TREE_OPERAND (val, 0) == rhs)))
	{
	  /* We can only remove the later store if the former aliases
	     at least all accesses the later one does or if the store
	     was to readonly memory storing the same value.  */
	  ao_ref lhs_ref;
	  ao_ref_init (&lhs_ref, lhs);
	  alias_set_type set = ao_ref_alias_set (&lhs_ref);
	  alias_set_type base_set = ao_ref_base_alias_set (&lhs_ref);
	  if (! vnresult
	      || ((vnresult->set == set
		   || alias_set_subset_of (set, vnresult->set))
		  && (vnresult->base_set == base_set
		      || alias_set_subset_of (base_set, vnresult->base_set))))
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
	  && may_propagate_copy (use, sprime, true)
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
      if (fold_stmt (gsi, follow_all_ssa_edges))
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
			vn_ssa_aux_t vn_info = VN_INFO (def);
			vn_info->valnum = def;
			vn_info->visited = true;
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
				    otr_type, stmt, NULL);
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
		fn = builtin_decl_unreachable ();
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
     continue with the next stmt above and skip this.
     But avoid picking up dead defs.  */
  tree def;
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
    if (! has_zero_uses (def)
	|| (inserted_exprs
	    && bitmap_bit_p (inserted_exprs, SSA_NAME_VERSION (def))))
      eliminate_push_avail (b, def);
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
	  if (sprime && may_propagate_copy (arg, sprime,
					    !(e->flags & EDGE_ABNORMAL)))
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
		    stmt = gsi_stmt (gsi);
		    update_stmt (stmt);
		    if (maybe_clean_or_replace_eh_stmt (stmt, stmt))
		      bitmap_set_bit (need_eh_cleanup, gimple_bb (stmt)->index);
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

  eliminate_dom_walker *saved_rpo_avail = rpo_avail;
  rpo_avail = &walker;
  walker.walk (cfun->cfg->x_entry_block_ptr);
  rpo_avail = saved_rpo_avail;

  return walker.eliminate_cleanup ();
}

static unsigned
do_rpo_vn_1 (function *fn, edge entry, bitmap exit_bbs,
	     bool iterate, bool eliminate, bool skip_entry_phis,
	     vn_lookup_kind kind);

void
run_rpo_vn (vn_lookup_kind kind)
{
  do_rpo_vn_1 (cfun, NULL, NULL, true, false, false, kind);

  /* ???  Prune requirement of these.  */
  constant_to_value_id = new hash_table<vn_constant_hasher> (23);

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
}

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
      vn_ssa_aux_t valnum_info = VN_INFO (valnum);
      vn_avail *av = valnum_info->avail;
      if (!av)
	{
	  /* See above.  But when there's availability info prefer
	     what we recorded there for example to preserve LC SSA.  */
	  if (!valnum_info->visited)
	    return valnum;
	  return NULL_TREE;
	}
      if (av->location == bb->index)
	/* On tramp3d 90% of the cases are here.  */
	return ssa_name (av->leader);
      do
	{
	  basic_block abb = BASIC_BLOCK_FOR_FN (cfun, av->location);
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
	  if (dominated_by_p_w_unex (bb, abb, true))
	    {
	      tree leader = ssa_name (av->leader);
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
	  av = av->next;
	}
      while (av);
      /* While we prefer avail we have to fallback to using the value
	 directly if defined outside of the region when none of the
	 available defs suit.  */
      if (!valnum_info->visited)
	return valnum;
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
  if (valnum == VN_TOP
      || is_gimple_min_invariant (valnum))
    return;
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Making available beyond BB%d ", bb->index);
      print_generic_expr (dump_file, leader);
      fprintf (dump_file, " for value ");
      print_generic_expr (dump_file, valnum);
      fprintf (dump_file, "\n");
    }
  vn_ssa_aux_t value = VN_INFO (valnum);
  vn_avail *av;
  if (m_avail_freelist)
    {
      av = m_avail_freelist;
      m_avail_freelist = m_avail_freelist->next;
    }
  else
    av = XOBNEW (&vn_ssa_aux_obstack, vn_avail);
  av->location = bb->index;
  av->leader = SSA_NAME_VERSION (leader);
  av->next = value->avail;
  av->next_undo = last_pushed_avail;
  last_pushed_avail = value;
  value->avail = av;
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

/* Insert on the TRUE_E true and FALSE_E false predicates
   derived from LHS CODE RHS.  */

static void
insert_predicates_for_cond (tree_code code, tree lhs, tree rhs,
			    edge true_e, edge false_e)
{
  /* If both edges are null, then there is nothing to be done. */
  if (!true_e && !false_e)
    return;

  /* Canonicalize the comparison if needed, putting
     the constant in the rhs.  */
  if (tree_swap_operands_p (lhs, rhs))
    {
      std::swap (lhs, rhs);
      code = swap_tree_comparison (code);
    }

  /* If the lhs is not a ssa name, don't record anything. */
  if (TREE_CODE (lhs) != SSA_NAME)
    return;

  tree_code icode = invert_tree_comparison (code, HONOR_NANS (lhs));
  tree ops[2];
  ops[0] = lhs;
  ops[1] = rhs;
  if (true_e)
    vn_nary_op_insert_pieces_predicated (2, code, boolean_type_node, ops,
					 boolean_true_node, 0, true_e);
  if (false_e)
    vn_nary_op_insert_pieces_predicated (2, code, boolean_type_node, ops,
					 boolean_false_node, 0, false_e);
  if (icode != ERROR_MARK)
    {
      if (true_e)
	vn_nary_op_insert_pieces_predicated (2, icode, boolean_type_node, ops,
					     boolean_false_node, 0, true_e);
      if (false_e)
	vn_nary_op_insert_pieces_predicated (2, icode, boolean_type_node, ops,
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
  if (integer_zerop (rhs)
      && (code == NE_EXPR || code == EQ_EXPR))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (lhs);
      /* (A CMP B) != 0 is the same as (A CMP B).
	 (A CMP B) == 0 is just (A CMP B) with the edges swapped.  */
      if (is_gimple_assign (def_stmt)
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (def_stmt)) == tcc_comparison)
	  {
	    tree_code nc = gimple_assign_rhs_code (def_stmt);
	    tree nlhs = vn_valueize (gimple_assign_rhs1 (def_stmt));
	    tree nrhs = vn_valueize (gimple_assign_rhs2 (def_stmt));
	    edge nt = true_e;
	    edge nf = false_e;
	    if (code == EQ_EXPR)
	      std::swap (nt, nf);
	    if (lhs != nlhs)
	      insert_predicates_for_cond (nc, nlhs, nrhs, nt, nf);
	  }
      /* (a | b) == 0 ->
	    on true edge assert: a == 0 & b == 0. */
      /* (a | b) != 0 ->
	    on false edge assert: a == 0 & b == 0. */
      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == BIT_IOR_EXPR)
	{
	  edge e = code == EQ_EXPR ? true_e : false_e;
	  tree nlhs;

	  nlhs = vn_valueize (gimple_assign_rhs1 (def_stmt));
	  /* A valueization of the `a` might return the old lhs
	     which is already handled above. */
	  if (nlhs != lhs)
	    insert_predicates_for_cond (EQ_EXPR, nlhs, rhs, e, nullptr);

	  /* A valueization of the `b` might return the old lhs
	     which is already handled above. */
	  nlhs = vn_valueize (gimple_assign_rhs2 (def_stmt));
	  if (nlhs != lhs)
	    insert_predicates_for_cond (EQ_EXPR, nlhs, rhs, e, nullptr);
	}
    }
}

/* Main stmt worker for RPO VN, process BB.  */

static unsigned
process_bb (rpo_elim &avail, basic_block bb,
	    bool bb_visited, bool iterate_phis, bool iterate, bool eliminate,
	    bool do_region, bitmap exit_bbs, bool skip_phis)
{
  unsigned todo = 0;
  edge_iterator ei;
  edge e;

  vn_context_bb = bb;

  /* If we are in loop-closed SSA preserve this state.  This is
     relevant when called on regions from outside of FRE/PRE.  */
  bool lc_phi_nodes = false;
  if (!skip_phis
      && loops_state_satisfies_p (LOOP_CLOSED_SSA))
    FOR_EACH_EDGE (e, ei, bb->preds)
      if (e->src->loop_father != e->dest->loop_father
	  && flow_loop_nested_p (e->dest->loop_father,
				 e->src->loop_father))
	{
	  lc_phi_nodes = true;
	  break;
	}

  /* When we visit a loop header substitute into loop info.  */
  if (!iterate && eliminate && bb->loop_father->header == bb)
    {
      /* Keep fields in sync with substitute_in_loop_info.  */
      if (bb->loop_father->nb_iterations)
	bb->loop_father->nb_iterations
	  = simplify_replace_tree (bb->loop_father->nb_iterations,
				   NULL_TREE, NULL_TREE, &vn_valueize_for_srt);
    }

  /* Value-number all defs in the basic-block.  */
  if (!skip_phis)
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
	  if (!(e->flags & EDGE_EXECUTABLE))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "marking outgoing edge %d -> %d executable\n",
			 e->src->index, e->dest->index);
	      e->flags |= EDGE_EXECUTABLE;
	      e->dest->flags |= BB_EXECUTABLE;
	    }
	  else if (!(e->dest->flags & BB_EXECUTABLE))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "marking destination block %d reachable\n",
			 e->dest->index);
	      e->dest->flags |= BB_EXECUTABLE;
	    }
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
	    tree_code cmpcode = gimple_cond_code (last);
	    /* Canonicalize the comparison if needed, putting
	       the constant in the rhs.  */
	    if (tree_swap_operands_p (lhs, rhs))
	      {
		std::swap (lhs, rhs);
		cmpcode = swap_tree_comparison (cmpcode);
	       }
	    tree val = gimple_simplify (cmpcode,
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
		val = vn_nary_op_lookup_pieces (2, cmpcode,
						boolean_type_node, ops,
						&vnresult);
		/* Got back a ssa name, then try looking up `val != 0`
		   as it might have been recorded that way.  */
		if (val && TREE_CODE (val) == SSA_NAME)
		  {
		    ops[0] = val;
		    ops[1] = build_zero_cst (TREE_TYPE (val));
		    val = vn_nary_op_lookup_pieces (2, NE_EXPR,
						    boolean_type_node, ops,
						    &vnresult);
		  }
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
		if ((do_region && bitmap_bit_p (exit_bbs, true_e->dest->index))
		    || !can_track_predicate_on_edge (true_e))
		  true_e = NULL;
		if ((do_region && bitmap_bit_p (exit_bbs, false_e->dest->index))
		    || !can_track_predicate_on_edge (false_e))
		  false_e = NULL;
		insert_predicates_for_cond (cmpcode, lhs, rhs, true_e, false_e);
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
	      e->flags |= EDGE_EXECUTABLE;
	      e->dest->flags |= BB_EXECUTABLE;
	    }
	  else if (!(e->dest->flags & BB_EXECUTABLE))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "marking destination block %d reachable\n",
			 e->dest->index);
	      e->dest->flags |= BB_EXECUTABLE;
	    }
	}
      else if (gsi_one_before_end_p (gsi))
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    {
	      if (!(e->flags & EDGE_EXECUTABLE))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "marking outgoing edge %d -> %d executable\n",
			     e->src->index, e->dest->index);
		  e->flags |= EDGE_EXECUTABLE;
		  e->dest->flags |= BB_EXECUTABLE;
		}
	      else if (!(e->dest->flags & BB_EXECUTABLE))
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file,
			     "marking destination block %d reachable\n",
			     e->dest->index);
		  e->dest->flags |= BB_EXECUTABLE;
		}
	    }
	}

      /* Eliminate.  That also pushes to avail.  */
      if (eliminate && ! iterate)
	avail.eliminate_stmt (bb, &gsi);
      else
	/* If not eliminating, make all not already available defs
	   available.  But avoid picking up dead defs.  */
	FOR_EACH_SSA_TREE_OPERAND (op, gsi_stmt (gsi), i, SSA_OP_DEF)
	  if (! has_zero_uses (op)
	      && ! avail.eliminate_avail (bb, op))
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
	      && may_propagate_copy (arg, sprime, !(e->flags & EDGE_ABNORMAL)))
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
  /* Maximum RPO index this block is reachable from.  */
  int max_rpo;
  /* Unwind state.  */
  void *ob_top;
  vn_reference_t ref_top;
  vn_phi_t phi_top;
  vn_nary_op_t nary_top;
  vn_avail *avail_top;
};

/* Unwind the RPO VN state for iteration.  */

static void
do_unwind (unwind_state *to, rpo_elim &avail)
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
  for (; last_pushed_avail && last_pushed_avail->avail != to->avail_top;)
    {
      vn_ssa_aux_t val = last_pushed_avail;
      vn_avail *av = val->avail;
      val->avail = av->next;
      last_pushed_avail = av->next_undo;
      av->next = avail.m_avail_freelist;
      avail.m_avail_freelist = av;
    }
}

/* Do VN on a SEME region specified by ENTRY and EXIT_BBS in FN.
   If ITERATE is true then treat backedges optimistically as not
   executed and iterate.  If ELIMINATE is true then perform
   elimination, otherwise leave that to the caller.  If SKIP_ENTRY_PHIS
   is true then force PHI nodes in ENTRY->dest to VARYING.  */

static unsigned
do_rpo_vn_1 (function *fn, edge entry, bitmap exit_bbs,
	     bool iterate, bool eliminate, bool skip_entry_phis,
	     vn_lookup_kind kind)
{
  unsigned todo = 0;
  default_vn_walk_kind = kind;

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

  /* Clear EDGE_DFS_BACK on "all" entry edges, RPO order compute will
     re-mark those that are contained in the region.  */
  edge_iterator ei;
  edge e;
  FOR_EACH_EDGE (e, ei, entry->dest->preds)
    e->flags &= ~EDGE_DFS_BACK;

  int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fn) - NUM_FIXED_BLOCKS);
  auto_vec<std::pair<int, int> > toplevel_scc_extents;
  int n = rev_post_order_and_mark_dfs_back_seme
    (fn, entry, exit_bbs, true, rpo, !iterate ? &toplevel_scc_extents : NULL);

  if (!do_region)
    BITMAP_FREE (exit_bbs);

  /* If there are any non-DFS_BACK edges into entry->dest skip
     processing PHI nodes for that block.  This supports
     value-numbering loop bodies w/o the actual loop.  */
  FOR_EACH_EDGE (e, ei, entry->dest->preds)
    if (e != entry
	&& !(e->flags & EDGE_DFS_BACK))
      break;
  if (e != NULL && dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Region does not contain all edges into "
	     "the entry block, skipping its PHIs.\n");
  skip_entry_phis |= e != NULL;

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
	    gcc_assert (e == entry
			|| (skip_entry_phis && bb == entry->dest)
			|| (e->src->flags & bb_in_region));
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
  next_value_id = 1;
  next_constant_value_id = -1;

  vn_ssa_aux_hash = new hash_table <vn_ssa_aux_hasher> (region_size * 2);
  gcc_obstack_init (&vn_ssa_aux_obstack);

  gcc_obstack_init (&vn_tables_obstack);
  gcc_obstack_init (&vn_tables_insert_obstack);
  valid_info = XCNEW (struct vn_tables_s);
  allocate_vn_table (valid_info, region_size);
  last_inserted_ref = NULL;
  last_inserted_phi = NULL;
  last_inserted_nary = NULL;
  last_pushed_avail = NULL;

  vn_valueize = rpo_vn_valueize;

  /* Initialize the unwind state and edge/BB executable state.  */
  unsigned curr_scc = 0;
  for (int i = 0; i < n; ++i)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[i]);
      rpo_state[i].visited = 0;
      rpo_state[i].max_rpo = i;
      if (!iterate && curr_scc < toplevel_scc_extents.length ())
	{
	  if (i >= toplevel_scc_extents[curr_scc].first
	      && i <= toplevel_scc_extents[curr_scc].second)
	    rpo_state[i].max_rpo = toplevel_scc_extents[curr_scc].second;
	  if (i == toplevel_scc_extents[curr_scc].second)
	    curr_scc++;
	}
      bb->flags &= ~BB_EXECUTABLE;
      bool has_backedges = false;
      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->flags & EDGE_DFS_BACK)
	    has_backedges = true;
	  e->flags &= ~EDGE_EXECUTABLE;
	  if (iterate || e == entry || (skip_entry_phis && bb == entry->dest))
	    continue;
	}
      rpo_state[i].iterate = iterate && has_backedges;
    }
  entry->flags |= EDGE_EXECUTABLE;
  entry->dest->flags |= BB_EXECUTABLE;

  /* As heuristic to improve compile-time we handle only the N innermost
     loops and the outermost one optimistically.  */
  if (iterate)
    {
      unsigned max_depth = param_rpo_vn_max_loop_depth;
      for (auto loop : loops_list (cfun, LI_ONLY_INNERMOST))
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
		    else
		      e->flags |= EDGE_EXECUTABLE;
		  }
	      rpo_state[bb_to_rpo[header->index]].iterate = non_latch_backedge;
	    }
    }

  uint64_t nblk = 0;
  int idx = 0;
  if (iterate)
    /* Go and process all blocks, iterating as necessary.  */
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
	    rpo_state[idx].avail_top
	      = last_pushed_avail ? last_pushed_avail->avail : NULL;
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
			    iterate, eliminate, do_region, exit_bbs, false);
	rpo_state[idx].visited++;

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
	      int destidx = bb_to_rpo[e->dest->index];
	      if (!rpo_state[destidx].visited)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "Unvisited destination %d\n",
			     e->dest->index);
		  if (iterate_to == -1 || destidx < iterate_to)
		    iterate_to = destidx;
		  continue;
		}
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Looking for changed values of backedge"
			 " %d->%d destination PHIs\n",
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
		      if (iterate_to == -1 || destidx < iterate_to)
			iterate_to = destidx;
		      break;
		    }
		}
	      vn_context_bb = NULL;
	    }
	if (iterate_to != -1)
	  {
	    do_unwind (&rpo_state[iterate_to], avail);
	    idx = iterate_to;
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Iterating to %d BB%d\n",
		       iterate_to, rpo[iterate_to]);
	    continue;
	  }

	idx++;
      }
    while (idx < n);

  else /* !iterate */
    {
      /* Process all blocks greedily with a worklist that enforces RPO
         processing of reachable blocks.  */
      auto_bitmap worklist;
      bitmap_set_bit (worklist, 0);
      while (!bitmap_empty_p (worklist))
	{
	  int idx = bitmap_clear_first_set_bit (worklist);
	  basic_block bb = BASIC_BLOCK_FOR_FN (fn, rpo[idx]);
	  gcc_assert ((bb->flags & BB_EXECUTABLE)
		      && !rpo_state[idx].visited);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "Processing block %d: BB%d\n", idx, bb->index);

	  /* When we run into predecessor edges where we cannot trust its
	     executable state mark them executable so PHI processing will
	     be conservative.
	     ???  Do we need to force arguments flowing over that edge
	     to be varying or will they even always be?  */
	  edge_iterator ei;
	  edge e;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    if (!(e->flags & EDGE_EXECUTABLE)
		&& (bb == entry->dest
		    || (!rpo_state[bb_to_rpo[e->src->index]].visited
			&& (rpo_state[bb_to_rpo[e->src->index]].max_rpo
			    >= (int)idx))))
	      {
		if (dump_file && (dump_flags & TDF_DETAILS))
		  fprintf (dump_file, "Cannot trust state of predecessor "
			   "edge %d -> %d, marking executable\n",
			   e->src->index, e->dest->index);
		e->flags |= EDGE_EXECUTABLE;
	      }

	  nblk++;
	  todo |= process_bb (avail, bb, false, false, false, eliminate,
			      do_region, exit_bbs,
			      skip_entry_phis && bb == entry->dest);
	  rpo_state[idx].visited++;

	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if ((e->flags & EDGE_EXECUTABLE)
		&& e->dest->index != EXIT_BLOCK
		&& (!do_region || !bitmap_bit_p (exit_bbs, e->dest->index))
		&& !rpo_state[bb_to_rpo[e->dest->index]].visited)
	      bitmap_set_bit (worklist, bb_to_rpo[e->dest->index]);
	}
    }

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
  for (hash_table<vn_ssa_aux_hasher>::iterator i = vn_ssa_aux_hash->begin ();
       i != vn_ssa_aux_hash->end (); ++i)
    {
      nvalues++;
      vn_avail *av = (*i)->avail;
      while (av)
	{
	  navail++;
	  av = av->next;
	}
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
  XDELETEVEC (rpo_state);

  return todo;
}

/* Region-based entry for RPO VN.  Performs value-numbering and elimination
   on the SEME region specified by ENTRY and EXIT_BBS.  If ENTRY is not
   the only edge into the region at ENTRY->dest PHI nodes in ENTRY->dest
   are not considered.
   If ITERATE is true then treat backedges optimistically as not
   executed and iterate.  If ELIMINATE is true then perform
   elimination, otherwise leave that to the caller.
   If SKIP_ENTRY_PHIS is true then force PHI nodes in ENTRY->dest to VARYING.
   KIND specifies the amount of work done for handling memory operations.  */

unsigned
do_rpo_vn (function *fn, edge entry, bitmap exit_bbs,
	   bool iterate, bool eliminate, bool skip_entry_phis,
	   vn_lookup_kind kind)
{
  auto_timevar tv (TV_TREE_RPO_VN);
  unsigned todo = do_rpo_vn_1 (fn, entry, exit_bbs, iterate, eliminate,
			       skip_entry_phis, kind);
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
    : gimple_opt_pass (pass_data_fre, ctxt), may_iterate (true)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override { return new pass_fre (m_ctxt); }
  void set_pass_param (unsigned int n, bool param) final override
    {
      gcc_assert (n == 0);
      may_iterate = param;
    }
  bool gate (function *) final override
    {
      return flag_tree_fre != 0 && (may_iterate || optimize > 1);
    }
  unsigned int execute (function *) final override;

private:
  bool may_iterate;
}; // class pass_fre

unsigned int
pass_fre::execute (function *fun)
{
  unsigned todo = 0;

  /* At -O[1g] use the cheap non-iterating mode.  */
  bool iterate_p = may_iterate && (optimize > 1);
  calculate_dominance_info (CDI_DOMINATORS);
  if (iterate_p)
    loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  todo = do_rpo_vn_1 (fun, NULL, NULL, iterate_p, true, false, VN_WALKREWRITE);
  free_rpo_vn ();

  if (iterate_p)
    loop_optimizer_finalize ();

  if (scev_initialized_p ())
    scev_reset_htab ();

  /* For late FRE after IVOPTs and unrolling, see if we can
     remove some TREE_ADDRESSABLE and rewrite stuff into SSA.  */
  if (!may_iterate)
    todo |= TODO_update_address_taken;

  return todo;
}

} // anon namespace

gimple_opt_pass *
make_pass_fre (gcc::context *ctxt)
{
  return new pass_fre (ctxt);
}

#undef BB_EXECUTABLE
