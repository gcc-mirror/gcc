/* SLP - Basic Block Vectorization
   Copyright (C) 2007-2024 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>
   and Ira Rosen <irar@il.ibm.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_ALGORITHM
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "insn-config.h"
#include "recog.h"		/* FIXME: for insn_data */
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "langhooks.h"
#include "gimple-walk.h"
#include "dbgcnt.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "gimple-fold.h"
#include "internal-fn.h"
#include "dump-context.h"
#include "cfganal.h"
#include "tree-eh.h"
#include "tree-cfg.h"
#include "alloc-pool.h"
#include "sreal.h"
#include "predict.h"

static bool vect_transform_slp_perm_load_1 (vec_info *, slp_tree,
					    load_permutation_t &,
					    const vec<tree> &,
					    gimple_stmt_iterator *,
					    poly_uint64, bool, bool,
					    unsigned *,
					    unsigned * = nullptr,
					    bool = false);
static int vectorizable_slp_permutation_1 (vec_info *, gimple_stmt_iterator *,
					   slp_tree, lane_permutation_t &,
					   vec<slp_tree> &, bool);
static bool vectorizable_slp_permutation (vec_info *, gimple_stmt_iterator *,
					  slp_tree, stmt_vector_for_cost *);
static void vect_print_slp_tree (dump_flags_t, dump_location_t, slp_tree);

static object_allocator<_slp_tree> *slp_tree_pool;
static slp_tree slp_first_node;

void
vect_slp_init (void)
{
  slp_tree_pool = new object_allocator<_slp_tree> ("SLP nodes");
}

void
vect_slp_fini (void)
{
  while (slp_first_node)
    delete slp_first_node;
  delete slp_tree_pool;
  slp_tree_pool = NULL;
}

void *
_slp_tree::operator new (size_t n)
{
  gcc_assert (n == sizeof (_slp_tree));
  return slp_tree_pool->allocate_raw ();
}

void
_slp_tree::operator delete (void *node, size_t n)
{
  gcc_assert (n == sizeof (_slp_tree));
  slp_tree_pool->remove_raw (node);
}


/* Initialize a SLP node.  */

_slp_tree::_slp_tree ()
{
  this->prev_node = NULL;
  if (slp_first_node)
    slp_first_node->prev_node = this;
  this->next_node = slp_first_node;
  slp_first_node = this;
  SLP_TREE_SCALAR_STMTS (this) = vNULL;
  SLP_TREE_SCALAR_OPS (this) = vNULL;
  SLP_TREE_VEC_DEFS (this) = vNULL;
  SLP_TREE_NUMBER_OF_VEC_STMTS (this) = 0;
  SLP_TREE_CHILDREN (this) = vNULL;
  SLP_TREE_LOAD_PERMUTATION (this) = vNULL;
  SLP_TREE_LANE_PERMUTATION (this) = vNULL;
  SLP_TREE_SIMD_CLONE_INFO (this) = vNULL;
  SLP_TREE_DEF_TYPE (this) = vect_uninitialized_def;
  SLP_TREE_CODE (this) = ERROR_MARK;
  this->ldst_lanes = false;
  SLP_TREE_VECTYPE (this) = NULL_TREE;
  SLP_TREE_REPRESENTATIVE (this) = NULL;
  SLP_TREE_MEMORY_ACCESS_TYPE (this) = VMAT_INVARIANT;
  SLP_TREE_REF_COUNT (this) = 1;
  this->failed = NULL;
  this->max_nunits = 1;
  this->lanes = 0;
}

/* Tear down a SLP node.  */

_slp_tree::~_slp_tree ()
{
  if (this->prev_node)
    this->prev_node->next_node = this->next_node;
  else
    slp_first_node = this->next_node;
  if (this->next_node)
    this->next_node->prev_node = this->prev_node;
  SLP_TREE_CHILDREN (this).release ();
  SLP_TREE_SCALAR_STMTS (this).release ();
  SLP_TREE_SCALAR_OPS (this).release ();
  SLP_TREE_VEC_DEFS (this).release ();
  SLP_TREE_LOAD_PERMUTATION (this).release ();
  SLP_TREE_LANE_PERMUTATION (this).release ();
  SLP_TREE_SIMD_CLONE_INFO (this).release ();
  if (this->failed)
    free (failed);
}

/* Push the single SSA definition in DEF to the vector of vector defs.  */

void
_slp_tree::push_vec_def (gimple *def)
{
  if (gphi *phi = dyn_cast <gphi *> (def))
    vec_defs.quick_push (gimple_phi_result (phi));
  else
    {
      def_operand_p defop = single_ssa_def_operand (def, SSA_OP_ALL_DEFS);
      vec_defs.quick_push (get_def_from_ptr (defop));
    }
}

/* Recursively free the memory allocated for the SLP tree rooted at NODE.  */

void
vect_free_slp_tree (slp_tree node)
{
  int i;
  slp_tree child;

  if (--SLP_TREE_REF_COUNT (node) != 0)
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      vect_free_slp_tree (child);

  /* If the node defines any SLP only patterns then those patterns are no
     longer valid and should be removed.  */
  stmt_vec_info rep_stmt_info = SLP_TREE_REPRESENTATIVE (node);
  if (rep_stmt_info && STMT_VINFO_SLP_VECT_ONLY_PATTERN (rep_stmt_info))
    {
      stmt_vec_info stmt_info = vect_orig_stmt (rep_stmt_info);
      STMT_VINFO_IN_PATTERN_P (stmt_info) = false;
      STMT_SLP_TYPE (stmt_info) = STMT_SLP_TYPE (rep_stmt_info);
    }

  delete node;
}

/* Return a location suitable for dumpings related to the SLP instance.  */

dump_user_location_t
_slp_instance::location () const
{
  if (!root_stmts.is_empty ())
    return root_stmts[0]->stmt;
  else
    return SLP_TREE_SCALAR_STMTS (root)[0]->stmt;
}


/* Free the memory allocated for the SLP instance.  */

void
vect_free_slp_instance (slp_instance instance)
{
  vect_free_slp_tree (SLP_INSTANCE_TREE (instance));
  SLP_INSTANCE_LOADS (instance).release ();
  SLP_INSTANCE_ROOT_STMTS (instance).release ();
  SLP_INSTANCE_REMAIN_DEFS (instance).release ();
  instance->subgraph_entries.release ();
  instance->cost_vec.release ();
  free (instance);
}


/* Create an SLP node for SCALAR_STMTS.  */

slp_tree
vect_create_new_slp_node (unsigned nops, tree_code code)
{
  slp_tree node = new _slp_tree;
  SLP_TREE_SCALAR_STMTS (node) = vNULL;
  SLP_TREE_CHILDREN (node).create (nops);
  SLP_TREE_DEF_TYPE (node) = vect_internal_def;
  SLP_TREE_CODE (node) = code;
  return node;
}
/* Create an SLP node for SCALAR_STMTS.  */

static slp_tree
vect_create_new_slp_node (slp_tree node,
			  vec<stmt_vec_info> scalar_stmts, unsigned nops)
{
  SLP_TREE_SCALAR_STMTS (node) = scalar_stmts;
  SLP_TREE_CHILDREN (node).create (nops);
  SLP_TREE_DEF_TYPE (node) = vect_internal_def;
  SLP_TREE_REPRESENTATIVE (node) = scalar_stmts[0];
  SLP_TREE_LANES (node) = scalar_stmts.length ();
  return node;
}

/* Create an SLP node for SCALAR_STMTS.  */

static slp_tree
vect_create_new_slp_node (vec<stmt_vec_info> scalar_stmts, unsigned nops)
{
  return vect_create_new_slp_node (new _slp_tree, scalar_stmts, nops);
}

/* Create an SLP node for OPS.  */

static slp_tree
vect_create_new_slp_node (slp_tree node, vec<tree> ops)
{
  SLP_TREE_SCALAR_OPS (node) = ops;
  SLP_TREE_DEF_TYPE (node) = vect_external_def;
  SLP_TREE_LANES (node) = ops.length ();
  return node;
}

/* Create an SLP node for OPS.  */

static slp_tree
vect_create_new_slp_node (vec<tree> ops)
{
  return vect_create_new_slp_node (new _slp_tree, ops);
}


/* This structure is used in creation of an SLP tree.  Each instance
   corresponds to the same operand in a group of scalar stmts in an SLP
   node.  */
typedef struct _slp_oprnd_info
{
  /* Def-stmts for the operands.  */
  vec<stmt_vec_info> def_stmts;
  /* Operands.  */
  vec<tree> ops;
  /* Information about the first statement, its vector def-type, type, the
     operand itself in case it's constant, and an indication if it's a pattern
     stmt and gather/scatter info.  */
  tree first_op_type;
  enum vect_def_type first_dt;
  bool any_pattern;
  bool first_gs_p;
  gather_scatter_info first_gs_info;
} *slp_oprnd_info;


/* Allocate operands info for NOPS operands, and GROUP_SIZE def-stmts for each
   operand.  */
static vec<slp_oprnd_info>
vect_create_oprnd_info (int nops, int group_size)
{
  int i;
  slp_oprnd_info oprnd_info;
  vec<slp_oprnd_info> oprnds_info;

  oprnds_info.create (nops);
  for (i = 0; i < nops; i++)
    {
      oprnd_info = XNEW (struct _slp_oprnd_info);
      oprnd_info->def_stmts.create (group_size);
      oprnd_info->ops.create (group_size);
      oprnd_info->first_dt = vect_uninitialized_def;
      oprnd_info->first_op_type = NULL_TREE;
      oprnd_info->any_pattern = false;
      oprnd_info->first_gs_p = false;
      oprnds_info.quick_push (oprnd_info);
    }

  return oprnds_info;
}


/* Free operands info.  */

static void
vect_free_oprnd_info (vec<slp_oprnd_info> &oprnds_info)
{
  int i;
  slp_oprnd_info oprnd_info;

  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      oprnd_info->def_stmts.release ();
      oprnd_info->ops.release ();
      XDELETE (oprnd_info);
    }

  oprnds_info.release ();
}

/* Return the execution frequency of NODE (so that a higher value indicates
   a "more important" node when optimizing for speed).  */

static sreal
vect_slp_node_weight (slp_tree node)
{
  stmt_vec_info stmt_info = vect_orig_stmt (SLP_TREE_REPRESENTATIVE (node));
  basic_block bb = gimple_bb (stmt_info->stmt);
  return bb->count.to_sreal_scale (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count);
}

/* Return true if STMTS contains a pattern statement.  */

static bool
vect_contains_pattern_stmt_p (vec<stmt_vec_info> stmts)
{
  stmt_vec_info stmt_info;
  unsigned int i;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    if (stmt_info && is_pattern_stmt_p (stmt_info))
      return true;
  return false;
}

/* Return true when all lanes in the external or constant NODE have
   the same value.  */

static bool
vect_slp_tree_uniform_p (slp_tree node)
{
  gcc_assert (SLP_TREE_DEF_TYPE (node) == vect_constant_def
	      || SLP_TREE_DEF_TYPE (node) == vect_external_def);

  /* Pre-exsting vectors.  */
  if (SLP_TREE_SCALAR_OPS (node).is_empty ())
    return false;

  unsigned i;
  tree op, first = NULL_TREE;
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
    if (!first)
      first = op;
    else if (!operand_equal_p (first, op, 0))
      return false;

  return true;
}

/* Find the place of the data-ref in STMT_INFO in the interleaving chain
   that starts from FIRST_STMT_INFO.  Return -1 if the data-ref is not a part
   of the chain.  */

int
vect_get_place_in_interleaving_chain (stmt_vec_info stmt_info,
				      stmt_vec_info first_stmt_info)
{
  stmt_vec_info next_stmt_info = first_stmt_info;
  int result = 0;

  if (first_stmt_info != DR_GROUP_FIRST_ELEMENT (stmt_info))
    return -1;

  do
    {
      if (next_stmt_info == stmt_info)
	return result;
      next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
      if (next_stmt_info)
	result += DR_GROUP_GAP (next_stmt_info);
    }
  while (next_stmt_info);

  return -1;
}

/* Check whether it is possible to load COUNT elements of type ELT_TYPE
   using the method implemented by duplicate_and_interleave.  Return true
   if so, returning the number of intermediate vectors in *NVECTORS_OUT
   (if nonnull) and the type of each intermediate vector in *VECTOR_TYPE_OUT
   (if nonnull).  */

bool
can_duplicate_and_interleave_p (vec_info *vinfo, unsigned int count,
				tree elt_type, unsigned int *nvectors_out,
				tree *vector_type_out,
				tree *permutes)
{
  tree base_vector_type = get_vectype_for_scalar_type (vinfo, elt_type, count);
  if (!base_vector_type || !VECTOR_MODE_P (TYPE_MODE (base_vector_type)))
    return false;

  machine_mode base_vector_mode = TYPE_MODE (base_vector_type);
  poly_int64 elt_bytes = count * GET_MODE_UNIT_SIZE (base_vector_mode);
  unsigned int nvectors = 1;
  for (;;)
    {
      scalar_int_mode int_mode;
      poly_int64 elt_bits = elt_bytes * BITS_PER_UNIT;
      if (int_mode_for_size (elt_bits, 1).exists (&int_mode))
	{
	  /* Get the natural vector type for this SLP group size.  */
	  tree int_type = build_nonstandard_integer_type
	    (GET_MODE_BITSIZE (int_mode), 1);
	  tree vector_type
	    = get_vectype_for_scalar_type (vinfo, int_type, count);
	  poly_int64 half_nelts;
	  if (vector_type
	      && VECTOR_MODE_P (TYPE_MODE (vector_type))
	      && known_eq (GET_MODE_SIZE (TYPE_MODE (vector_type)),
			   GET_MODE_SIZE (base_vector_mode))
	      && multiple_p (GET_MODE_NUNITS (TYPE_MODE (vector_type)),
			     2, &half_nelts))
	    {
	      /* Try fusing consecutive sequences of COUNT / NVECTORS elements
		 together into elements of type INT_TYPE and using the result
		 to build NVECTORS vectors.  */
	      poly_uint64 nelts = GET_MODE_NUNITS (TYPE_MODE (vector_type));
	      vec_perm_builder sel1 (nelts, 2, 3);
	      vec_perm_builder sel2 (nelts, 2, 3);

	      for (unsigned int i = 0; i < 3; ++i)
		{
		  sel1.quick_push (i);
		  sel1.quick_push (i + nelts);
		  sel2.quick_push (half_nelts + i);
		  sel2.quick_push (half_nelts + i + nelts);
		}
	      vec_perm_indices indices1 (sel1, 2, nelts);
	      vec_perm_indices indices2 (sel2, 2, nelts);
	      machine_mode vmode = TYPE_MODE (vector_type);
	      if (can_vec_perm_const_p (vmode, vmode, indices1)
		  && can_vec_perm_const_p (vmode, vmode, indices2))
		{
		  if (nvectors_out)
		    *nvectors_out = nvectors;
		  if (vector_type_out)
		    *vector_type_out = vector_type;
		  if (permutes)
		    {
		      permutes[0] = vect_gen_perm_mask_checked (vector_type,
								indices1);
		      permutes[1] = vect_gen_perm_mask_checked (vector_type,
								indices2);
		    }
		  return true;
		}
	    }
	}
      if (!multiple_p (elt_bytes, 2, &elt_bytes))
	return false;
      nvectors *= 2;
    }
}

/* Return true if DTA and DTB match.  */

static bool
vect_def_types_match (enum vect_def_type dta, enum vect_def_type dtb)
{
  return (dta == dtb
	  || ((dta == vect_external_def || dta == vect_constant_def)
	      && (dtb == vect_external_def || dtb == vect_constant_def)));
}

static const int cond_expr_maps[3][5] = {
  { 4, -1, -2, 1, 2 },
  { 4, -2, -1, 1, 2 },
  { 4, -1, -2, 2, 1 }
};
static const int no_arg_map[] = { 0 };
static const int arg0_map[] = { 1, 0 };
static const int arg1_map[] = { 1, 1 };
static const int arg2_map[] = { 1, 2 };
static const int arg1_arg4_map[] = { 2, 1, 4 };
static const int arg3_arg2_map[] = { 2, 3, 2 };
static const int op1_op0_map[] = { 2, 1, 0 };
static const int off_map[] = { 1, -3 };
static const int off_op0_map[] = { 2, -3, 0 };
static const int off_arg2_map[] = { 2, -3, 2 };
static const int off_arg3_arg2_map[] = { 3, -3, 3, 2 };
static const int mask_call_maps[6][7] = {
  { 1, 1, },
  { 2, 1, 2, },
  { 3, 1, 2, 3, },
  { 4, 1, 2, 3, 4, },
  { 5, 1, 2, 3, 4, 5, },
  { 6, 1, 2, 3, 4, 5, 6 },
};

/* For most SLP statements, there is a one-to-one mapping between
   gimple arguments and child nodes.  If that is not true for STMT,
   return an array that contains:

   - the number of child nodes, followed by
   - for each child node, the index of the argument associated with that node.
     The special index -1 is the first operand of an embedded comparison and
     the special index -2 is the second operand of an embedded comparison.
     The special indes -3 is the offset of a gather as analyzed by
     vect_check_gather_scatter.

   SWAP is as for vect_get_and_check_slp_defs.  */

static const int *
vect_get_operand_map (const gimple *stmt, bool gather_scatter_p = false,
		      unsigned char swap = 0)
{
  if (auto assign = dyn_cast<const gassign *> (stmt))
    {
      if (gimple_assign_rhs_code (assign) == COND_EXPR
	  && COMPARISON_CLASS_P (gimple_assign_rhs1 (assign)))
	return cond_expr_maps[swap];
      if (TREE_CODE_CLASS (gimple_assign_rhs_code (assign)) == tcc_comparison
	  && swap)
	return op1_op0_map;
      if (gather_scatter_p)
	return (TREE_CODE (gimple_assign_lhs (assign)) != SSA_NAME
		? off_op0_map : off_map);
    }
  gcc_assert (!swap);
  if (auto call = dyn_cast<const gcall *> (stmt))
    {
      if (gimple_call_internal_p (call))
	switch (gimple_call_internal_fn (call))
	  {
	  case IFN_MASK_LOAD:
	    return gather_scatter_p ? off_arg2_map : arg2_map;

	  case IFN_GATHER_LOAD:
	    return arg1_map;

	  case IFN_MASK_GATHER_LOAD:
	  case IFN_MASK_LEN_GATHER_LOAD:
	    return arg1_arg4_map;

	  case IFN_MASK_STORE:
	    return gather_scatter_p ? off_arg3_arg2_map : arg3_arg2_map;

	  case IFN_MASK_CALL:
	    {
	      unsigned nargs = gimple_call_num_args (call);
	      if (nargs >= 2 && nargs <= 7)
		return mask_call_maps[nargs-2];
	      else
		return nullptr;
	    }

	  case IFN_CLZ:
	  case IFN_CTZ:
	    return arg0_map;

	  case IFN_GOMP_SIMD_LANE:
	    return no_arg_map;

	  default:
	    break;
	  }
    }
  return nullptr;
}

/* Return the SLP node child index for operand OP of STMT.  */

int
vect_slp_child_index_for_operand (const gimple *stmt, int op,
				  bool gather_scatter_p)
{
  const int *opmap = vect_get_operand_map (stmt, gather_scatter_p);
  if (!opmap)
    return op;
  for (int i = 1; i < 1 + opmap[0]; ++i)
    if (opmap[i] == op)
      return i - 1;
  gcc_unreachable ();
}

/* Get the defs for the rhs of STMT (collect them in OPRNDS_INFO), check that
   they are of a valid type and that they match the defs of the first stmt of
   the SLP group (stored in OPRNDS_INFO).  This function tries to match stmts
   by swapping operands of STMTS[STMT_NUM] when possible.  Non-zero SWAP
   indicates swap is required for cond_expr stmts.  Specifically, SWAP
   is 1 if STMT is cond and operands of comparison need to be swapped;
   SWAP is 2 if STMT is cond and code of comparison needs to be inverted.

   If there was a fatal error return -1; if the error could be corrected by
   swapping operands of father node of this one, return 1; if everything is
   ok return 0.  */
static int
vect_get_and_check_slp_defs (vec_info *vinfo, unsigned char swap,
			     bool *skip_args,
			     vec<stmt_vec_info> stmts, unsigned stmt_num,
			     vec<slp_oprnd_info> *oprnds_info)
{
  stmt_vec_info stmt_info = stmts[stmt_num];
  tree oprnd;
  unsigned int i, number_of_oprnds;
  enum vect_def_type dt = vect_uninitialized_def;
  slp_oprnd_info oprnd_info;
  gather_scatter_info gs_info;
  unsigned int gs_op = -1u;
  unsigned int commutative_op = -1U;
  bool first = stmt_num == 0;

  if (!is_a<gcall *> (stmt_info->stmt)
      && !is_a<gassign *> (stmt_info->stmt)
      && !is_a<gphi *> (stmt_info->stmt))
    return -1;

  number_of_oprnds = gimple_num_args (stmt_info->stmt);
  const int *map
    = vect_get_operand_map (stmt_info->stmt,
			    STMT_VINFO_GATHER_SCATTER_P (stmt_info), swap);
  if (map)
    number_of_oprnds = *map++;
  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    {
      if (gimple_call_internal_p (stmt))
	{
	  internal_fn ifn = gimple_call_internal_fn (stmt);
	  commutative_op = first_commutative_argument (ifn);
	}
    }
  else if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      if (commutative_tree_code (gimple_assign_rhs_code (stmt)))
	commutative_op = 0;
    }

  bool swapped = (swap != 0);
  bool backedge = false;
  enum vect_def_type *dts = XALLOCAVEC (enum vect_def_type, number_of_oprnds);
  for (i = 0; i < number_of_oprnds; i++)
    {
      oprnd_info = (*oprnds_info)[i];
      int opno = map ? map[i] : int (i);
      if (opno == -3)
	{
	  gcc_assert (STMT_VINFO_GATHER_SCATTER_P (stmt_info));
	  if (!is_a <loop_vec_info> (vinfo)
	      || !vect_check_gather_scatter (stmt_info,
					     as_a <loop_vec_info> (vinfo),
					     first ? &oprnd_info->first_gs_info
					     : &gs_info))
	    return -1;

	  if (first)
	    {
	      oprnd_info->first_gs_p = true;
	      oprnd = oprnd_info->first_gs_info.offset;
	    }
	  else
	    {
	      gs_op = i;
	      oprnd = gs_info.offset;
	    }
	}
      else if (opno < 0)
	oprnd = TREE_OPERAND (gimple_arg (stmt_info->stmt, 0), -1 - opno);
      else
	{
	  oprnd = gimple_arg (stmt_info->stmt, opno);
	  if (gphi *stmt = dyn_cast <gphi *> (stmt_info->stmt))
	    {
	      edge e = gimple_phi_arg_edge (stmt, opno);
	      backedge = (is_a <bb_vec_info> (vinfo)
			  ? e->flags & EDGE_DFS_BACK
			  : dominated_by_p (CDI_DOMINATORS, e->src,
					    gimple_bb (stmt_info->stmt)));
	    }
	}
      if (TREE_CODE (oprnd) == VIEW_CONVERT_EXPR)
	oprnd = TREE_OPERAND (oprnd, 0);

      stmt_vec_info def_stmt_info;
      if (!vect_is_simple_use (oprnd, vinfo, &dts[i], &def_stmt_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: can't analyze def for %T\n",
			     oprnd);

	  return -1;
	}

      if (skip_args[i])
	{
	  oprnd_info->def_stmts.quick_push (NULL);
	  oprnd_info->ops.quick_push (NULL_TREE);
	  oprnd_info->first_dt = vect_uninitialized_def;
	  continue;
	}

      oprnd_info->def_stmts.quick_push (def_stmt_info);
      oprnd_info->ops.quick_push (oprnd);

      if (def_stmt_info
	  && is_pattern_stmt_p (def_stmt_info))
	{
	  if (STMT_VINFO_RELATED_STMT (vect_orig_stmt (def_stmt_info))
	      != def_stmt_info)
	    oprnd_info->any_pattern = true;
	  else
	    /* If we promote this to external use the original stmt def.  */
	    oprnd_info->ops.last ()
	      = gimple_get_lhs (vect_orig_stmt (def_stmt_info)->stmt);
	}

      /* If there's a extern def on a backedge make sure we can
	 code-generate at the region start.
	 ???  This is another case that could be fixed by adjusting
	 how we split the function but at the moment we'd have conflicting
	 goals there.  */
      if (backedge
	  && dts[i] == vect_external_def
	  && is_a <bb_vec_info> (vinfo)
	  && TREE_CODE (oprnd) == SSA_NAME
	  && !SSA_NAME_IS_DEFAULT_DEF (oprnd)
	  && !dominated_by_p (CDI_DOMINATORS, vinfo->bbs[0],
			      gimple_bb (SSA_NAME_DEF_STMT (oprnd))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: extern def %T only defined "
			     "on backedge\n", oprnd);
	  return -1;
	}

      if (first)
	{
	  tree type = TREE_TYPE (oprnd);
	  dt = dts[i];

	  /* For the swapping logic below force vect_reduction_def
	     for the reduction op in a SLP reduction group.  */
	  if (!STMT_VINFO_DATA_REF (stmt_info)
	      && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
	      && (int)i == STMT_VINFO_REDUC_IDX (stmt_info)
	      && def_stmt_info)
	    dts[i] = dt = vect_reduction_def;

	  /* Check the types of the definition.  */
	  switch (dt)
	    {
	    case vect_external_def:
	    case vect_constant_def:
	    case vect_internal_def:
	    case vect_reduction_def:
	    case vect_double_reduction_def:
	    case vect_induction_def:
	    case vect_nested_cycle:
	    case vect_first_order_recurrence:
	      break;

	    default:
	      /* FORNOW: Not supported.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: illegal type of def %T\n",
				 oprnd);
	      return -1;
	    }

	  oprnd_info->first_dt = dt;
	  oprnd_info->first_op_type = type;
	}
    }
  if (first)
    return 0;

  /* Now match the operand definition types to that of the first stmt.  */
  for (i = 0; i < number_of_oprnds;)
    {
      if (skip_args[i])
	{
	  ++i;
	  continue;
	}

      oprnd_info = (*oprnds_info)[i];
      dt = dts[i];
      stmt_vec_info def_stmt_info = oprnd_info->def_stmts[stmt_num];
      oprnd = oprnd_info->ops[stmt_num];
      tree type = TREE_TYPE (oprnd);

      if (!types_compatible_p (oprnd_info->first_op_type, type))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: different operand types\n");
	  return 1;
	}

      if ((gs_op == i) != oprnd_info->first_gs_p)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: mixed gather and non-gather\n");
	  return 1;
	}
      else if (gs_op == i)
	{
	  if (!operand_equal_p (oprnd_info->first_gs_info.base,
				gs_info.base))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different gather base\n");
	      return 1;
	    }
	  if (oprnd_info->first_gs_info.scale != gs_info.scale)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different gather scale\n");
	      return 1;
	    }
	}

      /* Not first stmt of the group, check that the def-stmt/s match
	 the def-stmt/s of the first stmt.  Allow different definition
	 types for reduction chains: the first stmt must be a
	 vect_reduction_def (a phi node), and the rest
	 end in the reduction chain.  */
      if ((!vect_def_types_match (oprnd_info->first_dt, dt)
	   && !(oprnd_info->first_dt == vect_reduction_def
		&& !STMT_VINFO_DATA_REF (stmt_info)
		&& REDUC_GROUP_FIRST_ELEMENT (stmt_info)
		&& def_stmt_info
		&& !STMT_VINFO_DATA_REF (def_stmt_info)
		&& (REDUC_GROUP_FIRST_ELEMENT (def_stmt_info)
		    == REDUC_GROUP_FIRST_ELEMENT (stmt_info))))
	  || (!STMT_VINFO_DATA_REF (stmt_info)
	      && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
	      && ((!def_stmt_info
		   || STMT_VINFO_DATA_REF (def_stmt_info)
		   || (REDUC_GROUP_FIRST_ELEMENT (def_stmt_info)
		       != REDUC_GROUP_FIRST_ELEMENT (stmt_info)))
		  != (oprnd_info->first_dt != vect_reduction_def))))
	{
	  /* Try swapping operands if we got a mismatch.  For BB
	     vectorization only in case it will clearly improve things.  */
	  if (i == commutative_op && !swapped
	      && (!is_a <bb_vec_info> (vinfo)
		  || (!vect_def_types_match ((*oprnds_info)[i+1]->first_dt,
					     dts[i+1])
		      && (vect_def_types_match (oprnd_info->first_dt, dts[i+1])
			  || vect_def_types_match
			       ((*oprnds_info)[i+1]->first_dt, dts[i])))))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "trying swapped operands\n");
	      std::swap (dts[i], dts[i+1]);
	      std::swap ((*oprnds_info)[i]->def_stmts[stmt_num],
			 (*oprnds_info)[i+1]->def_stmts[stmt_num]);
	      std::swap ((*oprnds_info)[i]->ops[stmt_num],
			 (*oprnds_info)[i+1]->ops[stmt_num]);
	      /* After swapping some operands we lost track whether an
		 operand has any pattern defs so be conservative here.  */
	      if ((*oprnds_info)[i]->any_pattern
		  || (*oprnds_info)[i+1]->any_pattern)
		(*oprnds_info)[i]->any_pattern
		  = (*oprnds_info)[i+1]->any_pattern = true;
	      swapped = true;
	      continue;
	    }

	  if (is_a <bb_vec_info> (vinfo)
	      && !oprnd_info->any_pattern
	      && number_of_oprnds > 1)
	    {
	      /* Now for commutative ops we should see whether we can
		 make the other operand matching.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "treating operand as external\n");
	      oprnd_info->first_dt = dt = vect_external_def;
	    }
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different types\n");
	      return 1;
	    }
	}

      /* Make sure to demote the overall operand to external.  */
      if (dt == vect_external_def)
	oprnd_info->first_dt = vect_external_def;
      /* For a SLP reduction chain we want to duplicate the reduction to
	 each of the chain members.  That gets us a sane SLP graph (still
	 the stmts are not 100% correct wrt the initial values).  */
      else if ((dt == vect_internal_def
		|| dt == vect_reduction_def)
	       && oprnd_info->first_dt == vect_reduction_def
	       && !STMT_VINFO_DATA_REF (stmt_info)
	       && REDUC_GROUP_FIRST_ELEMENT (stmt_info)
	       && !STMT_VINFO_DATA_REF (def_stmt_info)
	       && (REDUC_GROUP_FIRST_ELEMENT (def_stmt_info)
		   == REDUC_GROUP_FIRST_ELEMENT (stmt_info)))
	{
	  oprnd_info->def_stmts[stmt_num] = oprnd_info->def_stmts[0];
	  oprnd_info->ops[stmt_num] = oprnd_info->ops[0];
	}

      ++i;
    }

  /* Swap operands.  */
  if (swapped)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "swapped operands to match def types in %G",
			 stmt_info->stmt);
    }

  return 0;
}

/* Return true if call statements CALL1 and CALL2 are similar enough
   to be combined into the same SLP group.  */

bool
compatible_calls_p (gcall *call1, gcall *call2)
{
  unsigned int nargs = gimple_call_num_args (call1);
  if (nargs != gimple_call_num_args (call2))
    return false;

  if (gimple_call_combined_fn (call1) != gimple_call_combined_fn (call2))
    return false;

  if (gimple_call_internal_p (call1))
    {
      if (!types_compatible_p (TREE_TYPE (gimple_call_lhs (call1)),
			       TREE_TYPE (gimple_call_lhs (call2))))
	return false;
      for (unsigned int i = 0; i < nargs; ++i)
	if (!types_compatible_p (TREE_TYPE (gimple_call_arg (call1, i)),
				 TREE_TYPE (gimple_call_arg (call2, i))))
	  return false;
    }
  else
    {
      if (!operand_equal_p (gimple_call_fn (call1),
			    gimple_call_fn (call2), 0))
	return false;

      if (gimple_call_fntype (call1) != gimple_call_fntype (call2))
	return false;
    }

  /* Check that any unvectorized arguments are equal.  */
  if (const int *map = vect_get_operand_map (call1))
    {
      unsigned int nkept = *map++;
      unsigned int mapi = 0;
      for (unsigned int i = 0; i < nargs; ++i)
	if (mapi < nkept && map[mapi] == int (i))
	  mapi += 1;
	else if (!operand_equal_p (gimple_call_arg (call1, i),
				   gimple_call_arg (call2, i)))
	  return false;
    }

  return true;
}

/* A subroutine of vect_build_slp_tree for checking VECTYPE, which is the
   caller's attempt to find the vector type in STMT_INFO with the narrowest
   element type.  Return true if VECTYPE is nonnull and if it is valid
   for STMT_INFO.  When returning true, update MAX_NUNITS to reflect the
   number of units in VECTYPE.  GROUP_SIZE and MAX_NUNITS are as for
   vect_build_slp_tree.  */

static bool
vect_record_max_nunits (vec_info *vinfo, stmt_vec_info stmt_info,
			unsigned int group_size,
			tree vectype, poly_uint64 *max_nunits)
{
  if (!vectype)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Build SLP failed: unsupported data-type in %G\n",
			 stmt_info->stmt);
      /* Fatal mismatch.  */
      return false;
    }

  /* If populating the vector type requires unrolling then fail
     before adjusting *max_nunits for basic-block vectorization.  */
  if (is_a <bb_vec_info> (vinfo)
      && !multiple_p (group_size, TYPE_VECTOR_SUBPARTS (vectype)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Build SLP failed: unrolling required "
			 "in basic block SLP\n");
      /* Fatal mismatch.  */
      return false;
    }

  /* In case of multiple types we need to detect the smallest type.  */
  vect_update_max_nunits (max_nunits, vectype);
  return true;
}

/* Verify if the scalar stmts STMTS are isomorphic, require data
   permutation or are of unsupported types of operation.  Return
   true if they are, otherwise return false and indicate in *MATCHES
   which stmts are not isomorphic to the first one.  If MATCHES[0]
   is false then this indicates the comparison could not be
   carried out or the stmts will never be vectorized by SLP.

   Note COND_EXPR is possibly isomorphic to another one after swapping its
   operands.  Set SWAP[i] to 1 if stmt I is COND_EXPR and isomorphic to
   the first stmt by swapping the two operands of comparison; set SWAP[i]
   to 2 if stmt I is isormorphic to the first stmt by inverting the code
   of comparison.  Take A1 >= B1 ? X1 : Y1 as an exmple, it can be swapped
   to (B1 <= A1 ? X1 : Y1); or be inverted to (A1 < B1) ? Y1 : X1.  */

static bool
vect_build_slp_tree_1 (vec_info *vinfo, unsigned char *swap,
		       vec<stmt_vec_info> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits, bool *matches,
		       bool *two_operators, tree *node_vectype)
{
  unsigned int i;
  stmt_vec_info first_stmt_info = stmts[0];
  code_helper first_stmt_code = ERROR_MARK;
  code_helper alt_stmt_code = ERROR_MARK;
  code_helper first_cond_code = ERROR_MARK;
  tree lhs;
  bool need_same_oprnds = false;
  tree first_op1 = NULL_TREE;
  stmt_vec_info first_load = NULL, prev_first_load = NULL;
  bool first_stmt_ldst_p = false;
  bool first_stmt_phi_p = false;
  int first_reduc_idx = -1;
  bool maybe_soft_fail = false;
  tree soft_fail_nunits_vectype = NULL_TREE;

  /* For every stmt in NODE find its def stmt/s.  */
  stmt_vec_info stmt_info;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    {
      bool ldst_p = false;
      bool phi_p = false;
      code_helper rhs_code = ERROR_MARK;

      swap[i] = 0;
      matches[i] = false;
      if (!stmt_info)
	{
	  matches[i] = true;
	  continue;
	}

      gimple *stmt = stmt_info->stmt;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "Build SLP for %G", stmt);

      /* Fail to vectorize statements marked as unvectorizable, throw
	 or are volatile.  */
      if (!STMT_VINFO_VECTORIZABLE (stmt_info)
	  || stmt_can_throw_internal (cfun, stmt)
	  || gimple_has_volatile_ops (stmt))
        {
          if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: unvectorizable statement %G",
			     stmt);
	  /* ???  For BB vectorization we want to commutate operands in a way
	     to shuffle all unvectorizable defs into one operand and have
	     the other still vectorized.  The following doesn't reliably
	     work for this though but it's the easiest we can do here.  */
	  if (is_a <bb_vec_info> (vinfo) && i != 0)
	    continue;
	  /* Fatal mismatch.  */
	  matches[0] = false;
          return false;
        }

      gcall *call_stmt = dyn_cast <gcall *> (stmt);
      lhs = gimple_get_lhs (stmt);
      if (lhs == NULL_TREE
	  && (!call_stmt
	      || !gimple_call_internal_p (stmt)
	      || !internal_store_fn_p (gimple_call_internal_fn (stmt))))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Build SLP failed: not GIMPLE_ASSIGN nor "
			     "GIMPLE_CALL %G", stmt);
	  if (is_a <bb_vec_info> (vinfo) && i != 0)
	    continue;
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}

      tree vectype, nunits_vectype;
      if (!vect_get_vector_types_for_stmt (vinfo, stmt_info, &vectype,
					   &nunits_vectype, group_size))
	{
	  if (is_a <bb_vec_info> (vinfo) && i != 0)
	    continue;
	  /* Fatal mismatch.  */
	  matches[0] = false;
	  return false;
	}
      /* Record nunits required but continue analysis, producing matches[]
	 as if nunits was not an issue.  This allows splitting of groups
	 to happen.  */
      if (nunits_vectype
	  && !vect_record_max_nunits (vinfo, stmt_info, group_size,
				      nunits_vectype, max_nunits))
	{
	  gcc_assert (is_a <bb_vec_info> (vinfo));
	  maybe_soft_fail = true;
	  soft_fail_nunits_vectype = nunits_vectype;
	}

      gcc_assert (vectype);

      if (call_stmt)
	{
	  combined_fn cfn = gimple_call_combined_fn (call_stmt);
	  if (cfn != CFN_LAST && cfn != CFN_MASK_CALL)
	    rhs_code = cfn;
	  else
	    rhs_code = CALL_EXPR;

	  if (cfn == CFN_MASK_LOAD
	      || cfn == CFN_GATHER_LOAD
	      || cfn == CFN_MASK_GATHER_LOAD
	      || cfn == CFN_MASK_LEN_GATHER_LOAD)
	    ldst_p = true;
	  else if (cfn == CFN_MASK_STORE)
	    {
	      ldst_p = true;
	      rhs_code = CFN_MASK_STORE;
	    }
	  else if (cfn == CFN_GOMP_SIMD_LANE)
	    ;
	  else if ((cfn != CFN_LAST
		    && cfn != CFN_MASK_CALL
		    && internal_fn_p (cfn)
		    && !vectorizable_internal_fn_p (as_internal_fn (cfn)))
		   || gimple_call_tail_p (call_stmt)
		   || gimple_call_noreturn_p (call_stmt)
		   || gimple_call_chain (call_stmt))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: unsupported call type %G",
				 (gimple *) call_stmt);
	      if (is_a <bb_vec_info> (vinfo) && i != 0)
		continue;
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }
	}
      else if (gimple_code (stmt) == GIMPLE_PHI)
	{
	  rhs_code = ERROR_MARK;
	  phi_p = true;
	}
      else
	{
	  rhs_code = gimple_assign_rhs_code (stmt);
	  ldst_p = STMT_VINFO_DATA_REF (stmt_info) != nullptr;
	}

      /* Check the operation.  */
      if (i == 0)
	{
	  *node_vectype = vectype;
	  first_stmt_code = rhs_code;
	  first_stmt_ldst_p = ldst_p;
	  first_stmt_phi_p = phi_p;
	  first_reduc_idx = STMT_VINFO_REDUC_IDX (stmt_info);

	  /* Shift arguments should be equal in all the packed stmts for a
	     vector shift with scalar shift operand.  */
	  if (rhs_code == LSHIFT_EXPR || rhs_code == RSHIFT_EXPR
	      || rhs_code == LROTATE_EXPR
	      || rhs_code == RROTATE_EXPR)
	    {
	      /* First see if we have a vector/vector shift.  */
	      if (!directly_supported_p (rhs_code, vectype, optab_vector))
		{
		  /* No vector/vector shift, try for a vector/scalar shift.  */
		  if (!directly_supported_p (rhs_code, vectype, optab_scalar))
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
					 "Build SLP failed: "
					 "op not supported by target.\n");
		      if (is_a <bb_vec_info> (vinfo) && i != 0)
			continue;
		      /* Fatal mismatch.  */
		      matches[0] = false;
		      return false;
		    }
		  need_same_oprnds = true;
		  first_op1 = gimple_assign_rhs2 (stmt);
		}
	    }
	  else if (rhs_code == WIDEN_LSHIFT_EXPR)
            {
              need_same_oprnds = true;
              first_op1 = gimple_assign_rhs2 (stmt);
            }
	  else if (!ldst_p
		   && rhs_code == BIT_FIELD_REF)
	    {
	      tree vec = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
	      if (!is_a <bb_vec_info> (vinfo)
		  || TREE_CODE (vec) != SSA_NAME
		  /* When the element types are not compatible we pun the
		     source to the target vectype which requires equal size.  */
		  || ((!VECTOR_TYPE_P (TREE_TYPE (vec))
		       || !types_compatible_p (TREE_TYPE (vectype),
					       TREE_TYPE (TREE_TYPE (vec))))
		      && !operand_equal_p (TYPE_SIZE (vectype),
					   TYPE_SIZE (TREE_TYPE (vec)))))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: "
				     "BIT_FIELD_REF not supported\n");
		  /* Fatal mismatch.  */
		  matches[0] = false;
		  return false;
		}
	    }
	  else if (rhs_code == CFN_DIV_POW2)
	    {
	      need_same_oprnds = true;
	      first_op1 = gimple_call_arg (call_stmt, 1);
	    }
	  else if (rhs_code == CFN_GOMP_SIMD_LANE)
	    {
	      need_same_oprnds = true;
	      first_op1 = gimple_call_arg (call_stmt, 1);
	    }
	}
      else
	{
	  if (first_reduc_idx != STMT_VINFO_REDUC_IDX (stmt_info)
	      /* For SLP reduction groups the index isn't necessarily
		 uniform but only that of the first stmt matters.  */
	      && !(first_reduc_idx != -1
		   && STMT_VINFO_REDUC_IDX (stmt_info) != -1
		   && REDUC_GROUP_FIRST_ELEMENT (stmt_info)))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: different reduc_idx "
				   "%d instead of %d in %G",
				   STMT_VINFO_REDUC_IDX (stmt_info),
				   first_reduc_idx, stmt);
		}
	      /* Mismatch.  */
	      continue;
	    }
	  if (!ldst_p
	      && first_stmt_code != rhs_code
	      && alt_stmt_code == ERROR_MARK)
	    alt_stmt_code = rhs_code;
	  if ((!ldst_p
	       && first_stmt_code != rhs_code
	       && (first_stmt_code != IMAGPART_EXPR
		   || rhs_code != REALPART_EXPR)
	       && (first_stmt_code != REALPART_EXPR
		   || rhs_code != IMAGPART_EXPR)
	       /* Handle mismatches in plus/minus by computing both
		  and merging the results.  */
	       && !((first_stmt_code == PLUS_EXPR
		     || first_stmt_code == MINUS_EXPR)
		    && (alt_stmt_code == PLUS_EXPR
			|| alt_stmt_code == MINUS_EXPR)
		    && rhs_code == alt_stmt_code)
	       && !(first_stmt_code.is_tree_code ()
		    && rhs_code.is_tree_code ()
		    && (TREE_CODE_CLASS (tree_code (first_stmt_code))
			== tcc_comparison)
		    && (swap_tree_comparison (tree_code (first_stmt_code))
			== tree_code (rhs_code))))
	      || (ldst_p
		  && (STMT_VINFO_GROUPED_ACCESS (stmt_info)
		      != STMT_VINFO_GROUPED_ACCESS (first_stmt_info)))
	      || (ldst_p
		  && (STMT_VINFO_GATHER_SCATTER_P (stmt_info)
		      != STMT_VINFO_GATHER_SCATTER_P (first_stmt_info)))
	      || first_stmt_ldst_p != ldst_p
	      || first_stmt_phi_p != phi_p)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "Build SLP failed: different operation "
				   "in stmt %G", stmt);
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "original stmt %G", first_stmt_info->stmt);
		}
	      /* Mismatch.  */
	      continue;
	    }

	  if (!ldst_p
	      && first_stmt_code == BIT_FIELD_REF
	      && (TREE_OPERAND (gimple_assign_rhs1 (first_stmt_info->stmt), 0)
		  != TREE_OPERAND (gimple_assign_rhs1 (stmt_info->stmt), 0)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different BIT_FIELD_REF "
				 "arguments in %G", stmt);
	      /* Mismatch.  */
	      continue;
	    }

	  if (call_stmt
	      && first_stmt_code != CFN_MASK_LOAD
	      && first_stmt_code != CFN_MASK_STORE)
	    {
	      if (!is_a <gcall *> (stmts[0]->stmt)
		  || !compatible_calls_p (as_a <gcall *> (stmts[0]->stmt),
					  call_stmt))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different calls in %G",
				     stmt);
		  /* Mismatch.  */
		  continue;
		}
	    }

	  if ((phi_p || gimple_could_trap_p (stmt_info->stmt))
	      && (gimple_bb (first_stmt_info->stmt)
		  != gimple_bb (stmt_info->stmt)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different BB for PHI "
				 "or possibly trapping operation in %G", stmt);
	      /* Mismatch.  */
	      continue;
	    }

	  if (need_same_oprnds)
	    {
	      tree other_op1 = gimple_arg (stmt, 1);
	      if (!operand_equal_p (first_op1, other_op1, 0))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different shift "
				     "arguments in %G", stmt);
		  /* Mismatch.  */
		  continue;
		}
	    }

	  if (!types_compatible_p (vectype, *node_vectype))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different vector type "
				 "in %G", stmt);
	      /* Mismatch.  */
	      continue;
	    }
	}

      /* Grouped store or load.  */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
	{
	  gcc_assert (ldst_p);
	  if (DR_IS_WRITE (STMT_VINFO_DATA_REF (stmt_info)))
	    {
	      /* Store.  */
	      gcc_assert (rhs_code == CFN_MASK_STORE
			  || REFERENCE_CLASS_P (lhs)
			  || DECL_P (lhs));
	    }
	  else
	    {
	      /* Load.  */
	      first_load = DR_GROUP_FIRST_ELEMENT (stmt_info);
              if (prev_first_load)
                {
                  /* Check that there are no loads from different interleaving
                     chains in the same node.  */
                  if (prev_first_load != first_load)
                    {
                      if (dump_enabled_p ())
			dump_printf_loc (MSG_MISSED_OPTIMIZATION,
					 vect_location,
					 "Build SLP failed: different "
					 "interleaving chains in one node %G",
					 stmt);
		      /* Mismatch.  */
		      continue;
                    }
                }
              else
                prev_first_load = first_load;
           }
	}
      /* Non-grouped store or load.  */
      else if (ldst_p)
	{
	  if (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info))
	      && rhs_code != CFN_GATHER_LOAD
	      && rhs_code != CFN_MASK_GATHER_LOAD
	      && rhs_code != CFN_MASK_LEN_GATHER_LOAD
	      && !STMT_VINFO_GATHER_SCATTER_P (stmt_info)
	      /* Not grouped loads are handled as externals for BB
		 vectorization.  For loop vectorization we can handle
		 splats the same we handle single element interleaving.  */
	      && (is_a <bb_vec_info> (vinfo)
		  || stmt_info != first_stmt_info))
	    {
	      /* Not grouped load.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: not grouped load %G", stmt);

	      if (i != 0)
		continue;
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }
	}
      /* Not memory operation.  */
      else
	{
	  if (!phi_p
	      && rhs_code.is_tree_code ()
	      && TREE_CODE_CLASS (tree_code (rhs_code)) != tcc_binary
	      && TREE_CODE_CLASS (tree_code (rhs_code)) != tcc_unary
	      && TREE_CODE_CLASS (tree_code (rhs_code)) != tcc_expression
	      && TREE_CODE_CLASS (tree_code (rhs_code)) != tcc_comparison
	      && rhs_code != VIEW_CONVERT_EXPR
	      && rhs_code != CALL_EXPR
	      && rhs_code != BIT_FIELD_REF)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: operation unsupported %G",
				 stmt);
	      if (is_a <bb_vec_info> (vinfo) && i != 0)
		continue;
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }

	  if (rhs_code == COND_EXPR)
	    {
	      tree cond_expr = gimple_assign_rhs1 (stmt);
	      enum tree_code cond_code = TREE_CODE (cond_expr);
	      enum tree_code swap_code = ERROR_MARK;
	      enum tree_code invert_code = ERROR_MARK;

	      if (i == 0)
		first_cond_code = TREE_CODE (cond_expr);
	      else if (TREE_CODE_CLASS (cond_code) == tcc_comparison)
		{
		  bool honor_nans = HONOR_NANS (TREE_OPERAND (cond_expr, 0));
		  swap_code = swap_tree_comparison (cond_code);
		  invert_code = invert_tree_comparison (cond_code, honor_nans);
		}

	      if (first_cond_code == cond_code)
		;
	      /* Isomorphic can be achieved by swapping.  */
	      else if (first_cond_code == swap_code)
		swap[i] = 1;
	      /* Isomorphic can be achieved by inverting.  */
	      else if (first_cond_code == invert_code)
		swap[i] = 2;
	      else
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different"
				     " operation %G", stmt);
		  /* Mismatch.  */
		  continue;
		}
	    }

	  if (rhs_code.is_tree_code ()
	      && TREE_CODE_CLASS ((tree_code)rhs_code) == tcc_comparison
	      && (swap_tree_comparison ((tree_code)first_stmt_code)
		  == (tree_code)rhs_code))
	    swap[i] = 1;
	}

      matches[i] = true;
    }

  for (i = 0; i < group_size; ++i)
    if (!matches[i])
      return false;

  /* If we allowed a two-operation SLP node verify the target can cope
     with the permute we are going to use.  */
  if (alt_stmt_code != ERROR_MARK
      && (!alt_stmt_code.is_tree_code ()
	  || (TREE_CODE_CLASS (tree_code (alt_stmt_code)) != tcc_reference
	      && TREE_CODE_CLASS (tree_code (alt_stmt_code)) != tcc_comparison)))
    {
      *two_operators = true;
    }

  if (maybe_soft_fail)
    {
      unsigned HOST_WIDE_INT const_nunits;
      if (!TYPE_VECTOR_SUBPARTS
	    (soft_fail_nunits_vectype).is_constant (&const_nunits)
	  || const_nunits > group_size)
	matches[0] = false;
      else
	{
	  /* With constant vector elements simulate a mismatch at the
	     point we need to split.  */
	  unsigned tail = group_size & (const_nunits - 1);
	  memset (&matches[group_size - tail], 0, sizeof (bool) * tail);
	}
      return false;
    }

  return true;
}

/* Traits for the hash_set to record failed SLP builds for a stmt set.
   Note we never remove apart from at destruction time so we do not
   need a special value for deleted that differs from empty.  */
struct bst_traits
{
  typedef vec <stmt_vec_info> value_type;
  typedef vec <stmt_vec_info> compare_type;
  static inline hashval_t hash (value_type);
  static inline bool equal (value_type existing, value_type candidate);
  static inline bool is_empty (value_type x) { return !x.exists (); }
  static inline bool is_deleted (value_type x) { return !x.exists (); }
  static const bool empty_zero_p = true;
  static inline void mark_empty (value_type &x) { x.release (); }
  static inline void mark_deleted (value_type &x) { x.release (); }
  static inline void remove (value_type &x) { x.release (); }
};
inline hashval_t
bst_traits::hash (value_type x)
{
  inchash::hash h;
  for (unsigned i = 0; i < x.length (); ++i)
    h.add_int (x[i] ? gimple_uid (x[i]->stmt) : -1);
  return h.end ();
}
inline bool
bst_traits::equal (value_type existing, value_type candidate)
{
  if (existing.length () != candidate.length ())
    return false;
  for (unsigned i = 0; i < existing.length (); ++i)
    if (existing[i] != candidate[i])
      return false;
  return true;
}

typedef hash_map <vec <stmt_vec_info>, slp_tree,
		  simple_hashmap_traits <bst_traits, slp_tree> >
  scalar_stmts_to_slp_tree_map_t;

/* Release BST_MAP.  */

static void
release_scalar_stmts_to_slp_tree_map (scalar_stmts_to_slp_tree_map_t *bst_map)
{
  /* The map keeps a reference on SLP nodes built, release that.  */
  for (scalar_stmts_to_slp_tree_map_t::iterator it = bst_map->begin ();
       it != bst_map->end (); ++it)
    if ((*it).second)
      vect_free_slp_tree ((*it).second);
  delete bst_map;
}

/* ???  This was std::pair<std::pair<tree_code, vect_def_type>, tree>
   but then vec::insert does memmove and that's not compatible with
   std::pair.  */
struct chain_op_t
{
  chain_op_t (tree_code code_, vect_def_type dt_, tree op_)
      : code (code_), dt (dt_), op (op_) {}
  tree_code code;
  vect_def_type dt;
  tree op;
};

/* Comparator for sorting associatable chains.  */

static int
dt_sort_cmp (const void *op1_, const void *op2_, void *)
{
  auto *op1 = (const chain_op_t *) op1_;
  auto *op2 = (const chain_op_t *) op2_;
  if (op1->dt != op2->dt)
    return (int)op1->dt - (int)op2->dt;
  return (int)op1->code - (int)op2->code;
}

/* Linearize the associatable expression chain at START with the
   associatable operation CODE (where PLUS_EXPR also allows MINUS_EXPR),
   filling CHAIN with the result and using WORKLIST as intermediate storage.
   CODE_STMT and ALT_CODE_STMT are filled with the first stmt using CODE
   or MINUS_EXPR.  *CHAIN_STMTS if not NULL is filled with all computation
   stmts, starting with START.  */

static void
vect_slp_linearize_chain (vec_info *vinfo,
			  vec<std::pair<tree_code, gimple *> > &worklist,
			  vec<chain_op_t> &chain,
			  enum tree_code code, gimple *start,
			  gimple *&code_stmt, gimple *&alt_code_stmt,
			  vec<gimple *> *chain_stmts)
{
  /* For each lane linearize the addition/subtraction (or other
     uniform associatable operation) expression tree.  */
  worklist.safe_push (std::make_pair (code, start));
  while (!worklist.is_empty ())
    {
      auto entry = worklist.pop ();
      gassign *stmt = as_a <gassign *> (entry.second);
      enum tree_code in_code = entry.first;
      enum tree_code this_code = gimple_assign_rhs_code (stmt);
      /* Pick some stmts suitable for SLP_TREE_REPRESENTATIVE.  */
      if (!code_stmt
	  && gimple_assign_rhs_code (stmt) == code)
	code_stmt = stmt;
      else if (!alt_code_stmt
	       && gimple_assign_rhs_code (stmt) == MINUS_EXPR)
	alt_code_stmt = stmt;
      if (chain_stmts)
	chain_stmts->safe_push (stmt);
      for (unsigned opnum = 1; opnum <= 2; ++opnum)
	{
	  tree op = gimple_op (stmt, opnum);
	  vect_def_type dt;
	  stmt_vec_info def_stmt_info;
	  bool res = vect_is_simple_use (op, vinfo, &dt, &def_stmt_info);
	  gcc_assert (res);
	  if (dt == vect_internal_def
	      && is_pattern_stmt_p (def_stmt_info))
	    op = gimple_get_lhs (def_stmt_info->stmt);
	  gimple *use_stmt;
	  use_operand_p use_p;
	  if (dt == vect_internal_def
	      && single_imm_use (op, &use_p, &use_stmt)
	      && is_gimple_assign (def_stmt_info->stmt)
	      && (gimple_assign_rhs_code (def_stmt_info->stmt) == code
		  || (code == PLUS_EXPR
		      && (gimple_assign_rhs_code (def_stmt_info->stmt)
			  == MINUS_EXPR))))
	    {
	      tree_code op_def_code = this_code;
	      if (op_def_code == MINUS_EXPR && opnum == 1)
		op_def_code = PLUS_EXPR;
	      if (in_code == MINUS_EXPR)
		op_def_code = op_def_code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR;
	      worklist.safe_push (std::make_pair (op_def_code,
						  def_stmt_info->stmt));
	    }
	  else
	    {
	      tree_code op_def_code = this_code;
	      if (op_def_code == MINUS_EXPR && opnum == 1)
		op_def_code = PLUS_EXPR;
	      if (in_code == MINUS_EXPR)
		op_def_code = op_def_code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR;
	      chain.safe_push (chain_op_t (op_def_code, dt, op));
	    }
	}
    }
}

static slp_tree
vect_build_slp_tree_2 (vec_info *vinfo, slp_tree node,
		       vec<stmt_vec_info> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits,
		       bool *matches, unsigned *limit, unsigned *tree_size,
		       scalar_stmts_to_slp_tree_map_t *bst_map);

static slp_tree
vect_build_slp_tree (vec_info *vinfo,
		     vec<stmt_vec_info> stmts, unsigned int group_size,
		     poly_uint64 *max_nunits,
		     bool *matches, unsigned *limit, unsigned *tree_size,
		     scalar_stmts_to_slp_tree_map_t *bst_map)
{
  if (slp_tree *leader = bst_map->get (stmts))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "re-using %sSLP tree %p\n",
			 !(*leader)->failed ? "" : "failed ",
			 (void *) *leader);
      if (!(*leader)->failed)
	{
	  SLP_TREE_REF_COUNT (*leader)++;
	  vect_update_max_nunits (max_nunits, (*leader)->max_nunits);
	  stmts.release ();
	  return *leader;
	}
      memcpy (matches, (*leader)->failed, sizeof (bool) * group_size);
      return NULL;
    }

  /* Single-lane SLP doesn't have the chance of run-away, do not account
     it to the limit.  */
  if (stmts.length () > 1)
    {
      if (*limit == 0)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "SLP discovery limit exceeded\n");
	  memset (matches, 0, sizeof (bool) * group_size);
	  return NULL;
	}
      --*limit;
    }

  /* Seed the bst_map with a stub node to be filled by vect_build_slp_tree_2
     so we can pick up backedge destinations during discovery.  */
  slp_tree res = new _slp_tree;
  SLP_TREE_DEF_TYPE (res) = vect_internal_def;
  SLP_TREE_SCALAR_STMTS (res) = stmts;
  bst_map->put (stmts.copy (), res);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "starting SLP discovery for node %p\n", (void *) res);

  poly_uint64 this_max_nunits = 1;
  slp_tree res_ = vect_build_slp_tree_2 (vinfo, res, stmts, group_size,
					&this_max_nunits,
					matches, limit, tree_size, bst_map);
  if (!res_)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "SLP discovery for node %p failed\n", (void *) res);
      /* Mark the node invalid so we can detect those when still in use
	 as backedge destinations.  */
      SLP_TREE_SCALAR_STMTS (res) = vNULL;
      SLP_TREE_DEF_TYPE (res) = vect_uninitialized_def;
      res->failed = XNEWVEC (bool, group_size);
      if (flag_checking)
	{
	  unsigned i;
	  for (i = 0; i < group_size; ++i)
	    if (!matches[i])
	      break;
	  gcc_assert (i < group_size);
	}
      memcpy (res->failed, matches, sizeof (bool) * group_size);
    }
  else
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "SLP discovery for node %p succeeded\n",
			 (void *) res);
      gcc_assert (res_ == res);
      res->max_nunits = this_max_nunits;
      vect_update_max_nunits (max_nunits, this_max_nunits);
      /* Keep a reference for the bst_map use.  */
      SLP_TREE_REF_COUNT (res)++;
    }
  return res_;
}

/* Helper for building an associated SLP node chain.  */

static void
vect_slp_build_two_operator_nodes (slp_tree perm, tree vectype,
				   slp_tree op0, slp_tree op1,
				   stmt_vec_info oper1, stmt_vec_info oper2,
				   vec<std::pair<unsigned, unsigned> > lperm)
{
  unsigned group_size = SLP_TREE_LANES (op1);

  slp_tree child1 = new _slp_tree;
  SLP_TREE_DEF_TYPE (child1) = vect_internal_def;
  SLP_TREE_VECTYPE (child1) = vectype;
  SLP_TREE_LANES (child1) = group_size;
  SLP_TREE_CHILDREN (child1).create (2);
  SLP_TREE_CHILDREN (child1).quick_push (op0);
  SLP_TREE_CHILDREN (child1).quick_push (op1);
  SLP_TREE_REPRESENTATIVE (child1) = oper1;

  slp_tree child2 = new _slp_tree;
  SLP_TREE_DEF_TYPE (child2) = vect_internal_def;
  SLP_TREE_VECTYPE (child2) = vectype;
  SLP_TREE_LANES (child2) = group_size;
  SLP_TREE_CHILDREN (child2).create (2);
  SLP_TREE_CHILDREN (child2).quick_push (op0);
  SLP_TREE_REF_COUNT (op0)++;
  SLP_TREE_CHILDREN (child2).quick_push (op1);
  SLP_TREE_REF_COUNT (op1)++;
  SLP_TREE_REPRESENTATIVE (child2) = oper2;

  SLP_TREE_DEF_TYPE (perm) = vect_internal_def;
  SLP_TREE_CODE (perm) = VEC_PERM_EXPR;
  SLP_TREE_VECTYPE (perm) = vectype;
  SLP_TREE_LANES (perm) = group_size;
  /* ???  We should set this NULL but that's not expected.  */
  SLP_TREE_REPRESENTATIVE (perm) = oper1;
  SLP_TREE_LANE_PERMUTATION (perm) = lperm;
  SLP_TREE_CHILDREN (perm).quick_push (child1);
  SLP_TREE_CHILDREN (perm).quick_push (child2);
}

/* Recursively build an SLP tree starting from NODE.
   Fail (and return a value not equal to zero) if def-stmts are not
   isomorphic, require data permutation or are of unsupported types of
   operation.  Otherwise, return 0.
   The value returned is the depth in the SLP tree where a mismatch
   was found.  */

static slp_tree
vect_build_slp_tree_2 (vec_info *vinfo, slp_tree node,
		       vec<stmt_vec_info> stmts, unsigned int group_size,
		       poly_uint64 *max_nunits,
		       bool *matches, unsigned *limit, unsigned *tree_size,
		       scalar_stmts_to_slp_tree_map_t *bst_map)
{
  unsigned nops, i, this_tree_size = 0;
  poly_uint64 this_max_nunits = *max_nunits;

  matches[0] = false;

  stmt_vec_info stmt_info = stmts[0];
  if (!is_a<gcall *> (stmt_info->stmt)
      && !is_a<gassign *> (stmt_info->stmt)
      && !is_a<gphi *> (stmt_info->stmt))
    return NULL;

  nops = gimple_num_args (stmt_info->stmt);
  if (const int *map = vect_get_operand_map (stmt_info->stmt,
					     STMT_VINFO_GATHER_SCATTER_P
					       (stmt_info)))
    nops = map[0];

  /* If the SLP node is a PHI (induction or reduction), terminate
     the recursion.  */
  bool *skip_args = XALLOCAVEC (bool, nops);
  memset (skip_args, 0, sizeof (bool) * nops);
  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    if (gphi *stmt = dyn_cast <gphi *> (stmt_info->stmt))
      {
	tree scalar_type = TREE_TYPE (PHI_RESULT (stmt));
	tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type,
						    group_size);
	if (!vect_record_max_nunits (vinfo, stmt_info, group_size, vectype,
				     max_nunits))
	  return NULL;

	vect_def_type def_type = STMT_VINFO_DEF_TYPE (stmt_info);
	if (def_type == vect_induction_def)
	  {
	    /* Induction PHIs are not cycles but walk the initial
	       value.  Only for inner loops through, for outer loops
	       we need to pick up the value from the actual PHIs
	       to more easily support peeling and epilogue vectorization.  */
	    class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
	    if (!nested_in_vect_loop_p (loop, stmt_info))
	      skip_args[loop_preheader_edge (loop)->dest_idx] = true;
	    else
	      loop = loop->inner;
	    skip_args[loop_latch_edge (loop)->dest_idx] = true;
	  }
	else if (def_type == vect_reduction_def
		 || def_type == vect_double_reduction_def
		 || def_type == vect_nested_cycle
		 || def_type == vect_first_order_recurrence)
	  {
	    /* Else def types have to match.  */
	    stmt_vec_info other_info;
	    bool all_same = true;
	    FOR_EACH_VEC_ELT (stmts, i, other_info)
	      {
		if (STMT_VINFO_DEF_TYPE (other_info) != def_type)
		  return NULL;
		if (other_info != stmt_info)
		  all_same = false;
	      }
	    class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
	    /* Reduction initial values are not explicitely represented.  */
	    if (def_type != vect_first_order_recurrence
		&& gimple_bb (stmt_info->stmt) == loop->header)
	      skip_args[loop_preheader_edge (loop)->dest_idx] = true;
	    /* Reduction chain backedge defs are filled manually.
	       ???  Need a better way to identify a SLP reduction chain PHI.
	       Or a better overall way to SLP match those.  */
	    if (stmts.length () > 1
		&& all_same && def_type == vect_reduction_def)
	      skip_args[loop_latch_edge (loop)->dest_idx] = true;
	  }
	else if (def_type != vect_internal_def)
	  return NULL;
      }


  bool two_operators = false;
  unsigned char *swap = XALLOCAVEC (unsigned char, group_size);
  tree vectype = NULL_TREE;
  if (!vect_build_slp_tree_1 (vinfo, swap, stmts, group_size,
			      &this_max_nunits, matches, &two_operators,
			      &vectype))
    return NULL;

  /* If the SLP node is a load, terminate the recursion unless masked.  */
  if (STMT_VINFO_DATA_REF (stmt_info)
      && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
    {
      if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
	gcc_assert (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)));
      else
	{
	  *max_nunits = this_max_nunits;
	  (*tree_size)++;
	  node = vect_create_new_slp_node (node, stmts, 0);
	  SLP_TREE_VECTYPE (node) = vectype;
	  /* And compute the load permutation.  Whether it is actually
	     a permutation depends on the unrolling factor which is
	     decided later.  */
	  vec<unsigned> load_permutation;
	  int j;
	  stmt_vec_info load_info;
	  load_permutation.create (group_size);
	  stmt_vec_info first_stmt_info
	    = DR_GROUP_FIRST_ELEMENT (SLP_TREE_SCALAR_STMTS (node)[0]);
	  bool any_permute = false;
	  bool any_null = false;
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    {
	      int load_place;
	      if (! load_info)
		{
		  load_place = j;
		  any_null = true;
		}
	      else if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
		load_place = vect_get_place_in_interleaving_chain
		    (load_info, first_stmt_info);
	      else
		load_place = 0;
	      gcc_assert (load_place != -1);
	      any_permute |= load_place != j;
	      load_permutation.quick_push (load_place);
	    }
	  if (any_null)
	    {
	      gcc_assert (!any_permute);
	      load_permutation.release ();
	    }

	  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
	    {
	      gcc_assert (gimple_call_internal_p (stmt, IFN_MASK_LOAD)
			  || gimple_call_internal_p (stmt, IFN_GATHER_LOAD)
			  || gimple_call_internal_p (stmt, IFN_MASK_GATHER_LOAD)
			  || gimple_call_internal_p (stmt,
						     IFN_MASK_LEN_GATHER_LOAD));
	      bool has_gaps = false;
	      if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
		for (stmt_vec_info si = DR_GROUP_NEXT_ELEMENT (first_stmt_info);
		     si; si = DR_GROUP_NEXT_ELEMENT (si))
		  if (DR_GROUP_GAP (si) != 1)
		    has_gaps = true;
	      /* We cannot handle permuted masked loads directly, see
		 PR114375.  We cannot handle strided masked loads or masked
		 loads with gaps.  */
	      if ((STMT_VINFO_GROUPED_ACCESS (stmt_info)
		   && (DR_GROUP_GAP (first_stmt_info) != 0 || has_gaps))
		  || STMT_VINFO_STRIDED_P (stmt_info)
		  || (!STMT_VINFO_GROUPED_ACCESS (stmt_info) && any_permute))
		{
		  load_permutation.release ();
		  matches[0] = false;
		  return NULL;
		}

	      /* For permuted masked loads do an unpermuted masked load of
		 the whole group followed by a SLP permute node.  */
	      if (any_permute
		  || (STMT_VINFO_GROUPED_ACCESS (stmt_info)
		      && DR_GROUP_SIZE (first_stmt_info) != group_size))
		{
		  /* Discover the whole unpermuted load.  */
		  vec<stmt_vec_info> stmts2;
		  stmts2.create (DR_GROUP_SIZE (first_stmt_info));
		  stmts2.quick_grow_cleared (DR_GROUP_SIZE (first_stmt_info));
		  unsigned i = 0;
		  for (stmt_vec_info si = first_stmt_info;
		       si; si = DR_GROUP_NEXT_ELEMENT (si))
		    stmts2[i++] = si;
		  bool *matches2
		    = XALLOCAVEC (bool, DR_GROUP_SIZE (first_stmt_info));
		  slp_tree unperm_load
		    = vect_build_slp_tree (vinfo, stmts2,
					   DR_GROUP_SIZE (first_stmt_info),
					   &this_max_nunits, matches2, limit,
					   &this_tree_size, bst_map);
		  /* When we are able to do the full masked load emit that
		     followed by 'node' being the desired final permutation.  */
		  if (unperm_load)
		    {
		      lane_permutation_t lperm;
		      lperm.create (group_size);
		      for (unsigned j = 0; j < load_permutation.length (); ++j)
			lperm.quick_push
			  (std::make_pair (0, load_permutation[j]));
		      SLP_TREE_CODE (node) = VEC_PERM_EXPR;
		      SLP_TREE_CHILDREN (node).safe_push (unperm_load);
		      SLP_TREE_LANE_PERMUTATION (node) = lperm;
		      load_permutation.release ();
		      return node;
		    }
		  stmts2.release ();
		  load_permutation.release ();
		  matches[0] = false;
		  return NULL;
		}
	      load_permutation.release ();
	    }
	  else
	    {
	      SLP_TREE_LOAD_PERMUTATION (node) = load_permutation;
	      return node;
	    }
	}
    }
  else if (gimple_assign_single_p (stmt_info->stmt)
	   && !gimple_vuse (stmt_info->stmt)
	   && gimple_assign_rhs_code (stmt_info->stmt) == BIT_FIELD_REF)
    {
      /* vect_build_slp_tree_2 determined all BIT_FIELD_REFs reference
	 the same SSA name vector of a compatible type to vectype.  */
      vec<std::pair<unsigned, unsigned> > lperm = vNULL;
      tree vec = TREE_OPERAND (gimple_assign_rhs1 (stmt_info->stmt), 0);
      stmt_vec_info estmt_info;
      FOR_EACH_VEC_ELT (stmts, i, estmt_info)
	{
	  gassign *estmt = as_a <gassign *> (estmt_info->stmt);
	  tree bfref = gimple_assign_rhs1 (estmt);
	  HOST_WIDE_INT lane;
	  if (!known_eq (bit_field_size (bfref),
			 tree_to_poly_uint64 (TYPE_SIZE (TREE_TYPE (vectype))))
	      || !constant_multiple_p (bit_field_offset (bfref),
				       bit_field_size (bfref), &lane))
	    {
	      lperm.release ();
	      matches[0] = false;
	      return NULL;
	    }
	  lperm.safe_push (std::make_pair (0, (unsigned)lane));
	}
      slp_tree vnode = vect_create_new_slp_node (vNULL);
      if (operand_equal_p (TYPE_SIZE (vectype), TYPE_SIZE (TREE_TYPE (vec))))
	/* ???  We record vectype here but we hide eventually necessary
	   punning and instead rely on code generation to materialize
	   VIEW_CONVERT_EXPRs as necessary.  We instead should make
	   this explicit somehow.  */
	SLP_TREE_VECTYPE (vnode) = vectype;
      else
	{
	  /* For different size but compatible elements we can still
	     use VEC_PERM_EXPR without punning.  */
	  gcc_assert (VECTOR_TYPE_P (TREE_TYPE (vec))
		      && types_compatible_p (TREE_TYPE (vectype),
					     TREE_TYPE (TREE_TYPE (vec))));
	  SLP_TREE_VECTYPE (vnode) = TREE_TYPE (vec);
	}
      auto nunits = TYPE_VECTOR_SUBPARTS (SLP_TREE_VECTYPE (vnode));
      unsigned HOST_WIDE_INT const_nunits;
      if (nunits.is_constant (&const_nunits))
	SLP_TREE_LANES (vnode) = const_nunits;
      SLP_TREE_VEC_DEFS (vnode).safe_push (vec);
      /* We are always building a permutation node even if it is an identity
	 permute to shield the rest of the vectorizer from the odd node
	 representing an actual vector without any scalar ops.
	 ???  We could hide it completely with making the permute node
	 external?  */
      node = vect_create_new_slp_node (node, stmts, 1);
      SLP_TREE_CODE (node) = VEC_PERM_EXPR;
      SLP_TREE_LANE_PERMUTATION (node) = lperm;
      SLP_TREE_VECTYPE (node) = vectype;
      SLP_TREE_CHILDREN (node).quick_push (vnode);
      return node;
    }
  /* When discovery reaches an associatable operation see whether we can
     improve that to match up lanes in a way superior to the operand
     swapping code which at most looks at two defs.
     ???  For BB vectorization we cannot do the brute-force search
     for matching as we can succeed by means of builds from scalars
     and have no good way to "cost" one build against another.  */
  else if (is_a <loop_vec_info> (vinfo)
	   /* Do not bother for single-lane SLP.  */
	   && group_size > 1
	   /* ???  We don't handle !vect_internal_def defs below.  */
	   && STMT_VINFO_DEF_TYPE (stmt_info) == vect_internal_def
	   /* ???  Do not associate a reduction, this will wreck REDUC_IDX
	      mapping as long as that exists on the stmt_info level.  */
	   && STMT_VINFO_REDUC_IDX (stmt_info) == -1
	   && is_gimple_assign (stmt_info->stmt)
	   && (associative_tree_code (gimple_assign_rhs_code (stmt_info->stmt))
	       || gimple_assign_rhs_code (stmt_info->stmt) == MINUS_EXPR)
	   && ((FLOAT_TYPE_P (vectype) && flag_associative_math)
	       || (INTEGRAL_TYPE_P (TREE_TYPE (vectype))
		   && TYPE_OVERFLOW_WRAPS (TREE_TYPE (vectype)))))
    {
      /* See if we have a chain of (mixed) adds or subtracts or other
	 associatable ops.  */
      enum tree_code code = gimple_assign_rhs_code (stmt_info->stmt);
      if (code == MINUS_EXPR)
	code = PLUS_EXPR;
      stmt_vec_info other_op_stmt_info = NULL;
      stmt_vec_info op_stmt_info = NULL;
      unsigned chain_len = 0;
      auto_vec<chain_op_t> chain;
      auto_vec<std::pair<tree_code, gimple *> > worklist;
      auto_vec<vec<chain_op_t> > chains (group_size);
      auto_vec<slp_tree, 4> children;
      bool hard_fail = true;
      for (unsigned lane = 0; lane < group_size; ++lane)
	{
	  /* For each lane linearize the addition/subtraction (or other
	     uniform associatable operation) expression tree.  */
	  gimple *op_stmt = NULL, *other_op_stmt = NULL;
	  vect_slp_linearize_chain (vinfo, worklist, chain, code,
				    stmts[lane]->stmt, op_stmt, other_op_stmt,
				    NULL);
	  if (!op_stmt_info && op_stmt)
	    op_stmt_info = vinfo->lookup_stmt (op_stmt);
	  if (!other_op_stmt_info && other_op_stmt)
	    other_op_stmt_info = vinfo->lookup_stmt (other_op_stmt);
	  if (chain.length () == 2)
	    {
	      /* In a chain of just two elements resort to the regular
		 operand swapping scheme.  Likewise if we run into a
		 length mismatch process regularly as well as we did not
		 process the other lanes we cannot report a good hint what
		 lanes to try swapping in the parent.  */
	      hard_fail = false;
	      break;
	    }
	  else if (chain_len == 0)
	    chain_len = chain.length ();
	  else if (chain.length () != chain_len)
	    {
	      /* ???  Here we could slip in magic to compensate with
		 neutral operands.  */
	      matches[lane] = false;
	      if (lane != group_size - 1)
		matches[0] = false;
	      break;
	    }
	  chains.quick_push (chain.copy ());
	  chain.truncate (0);
	}
      if (chains.length () == group_size)
	{
	  /* We cannot yet use SLP_TREE_CODE to communicate the operation.  */
	  if (!op_stmt_info)
	    {
	      hard_fail = false;
	      goto out;
	    }
	  /* Now we have a set of chains with the same length.  */
	  /* 1. pre-sort according to def_type and operation.  */
	  for (unsigned lane = 0; lane < group_size; ++lane)
	    chains[lane].stablesort (dt_sort_cmp, vinfo);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "pre-sorted chains of %s\n",
			       get_tree_code_name (code));
	      for (unsigned lane = 0; lane < group_size; ++lane)
		{
		  for (unsigned opnum = 0; opnum < chain_len; ++opnum)
		    dump_printf (MSG_NOTE, "%s %T ",
				 get_tree_code_name (chains[lane][opnum].code),
				 chains[lane][opnum].op);
		  dump_printf (MSG_NOTE, "\n");
		}
	    }
	  /* 2. try to build children nodes, associating as necessary.  */
	  for (unsigned n = 0; n < chain_len; ++n)
	    {
	      vect_def_type dt = chains[0][n].dt;
	      unsigned lane;
	      for (lane = 0; lane < group_size; ++lane)
		if (chains[lane][n].dt != dt)
		  {
		    if (dt == vect_constant_def
			&& chains[lane][n].dt == vect_external_def)
		      dt = vect_external_def;
		    else if (dt == vect_external_def
			     && chains[lane][n].dt == vect_constant_def)
		      ;
		    else
		      break;
		  }
	      if (lane != group_size)
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "giving up on chain due to mismatched "
				     "def types\n");
		  matches[lane] = false;
		  if (lane != group_size - 1)
		    matches[0] = false;
		  goto out;
		}
	      if (dt == vect_constant_def
		  || dt == vect_external_def)
		{
		  /* Check whether we can build the invariant.  If we can't
		     we never will be able to.  */
		  tree type = TREE_TYPE (chains[0][n].op);
		  if (!GET_MODE_SIZE (vinfo->vector_mode).is_constant ()
		      && (TREE_CODE (type) == BOOLEAN_TYPE
			  || !can_duplicate_and_interleave_p (vinfo, group_size,
							      type)))
		    {
		      matches[0] = false;
		      goto out;
		    }
		  vec<tree> ops;
		  ops.create (group_size);
		  for (lane = 0; lane < group_size; ++lane)
		    ops.quick_push (chains[lane][n].op);
		  slp_tree child = vect_create_new_slp_node (ops);
		  SLP_TREE_DEF_TYPE (child) = dt;
		  children.safe_push (child);
		}
	      else if (dt != vect_internal_def)
		{
		  /* Not sure, we might need sth special.
		     gcc.dg/vect/pr96854.c,
		     gfortran.dg/vect/fast-math-pr37021.f90
		     and gfortran.dg/vect/pr61171.f trigger.  */
		  /* Soft-fail for now.  */
		  hard_fail = false;
		  goto out;
		}
	      else
		{
		  vec<stmt_vec_info> op_stmts;
		  op_stmts.create (group_size);
		  slp_tree child = NULL;
		  /* Brute-force our way.  We have to consider a lane
		     failing after fixing an earlier fail up in the
		     SLP discovery recursion.  So track the current
		     permute per lane.  */
		  unsigned *perms = XALLOCAVEC (unsigned, group_size);
		  memset (perms, 0, sizeof (unsigned) * group_size);
		  do
		    {
		      op_stmts.truncate (0);
		      for (lane = 0; lane < group_size; ++lane)
			op_stmts.quick_push
			  (vinfo->lookup_def (chains[lane][n].op));
		      child = vect_build_slp_tree (vinfo, op_stmts,
						   group_size, &this_max_nunits,
						   matches, limit,
						   &this_tree_size, bst_map);
		      /* ???  We're likely getting too many fatal mismatches
			 here so maybe we want to ignore them (but then we
			 have no idea which lanes fatally mismatched).  */
		      if (child || !matches[0])
			break;
		      /* Swap another lane we have not yet matched up into
			 lanes that did not match.  If we run out of
			 permute possibilities for a lane terminate the
			 search.  */
		      bool term = false;
		      for (lane = 1; lane < group_size; ++lane)
			if (!matches[lane])
			  {
			    if (n + perms[lane] + 1 == chain_len)
			      {
				term = true;
				break;
			      }
			    std::swap (chains[lane][n],
				       chains[lane][n + perms[lane] + 1]);
			    perms[lane]++;
			  }
		      if (term)
			break;
		    }
		  while (1);
		  if (!child)
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_NOTE, vect_location,
					 "failed to match up op %d\n", n);
		      op_stmts.release ();
		      if (lane != group_size - 1)
			matches[0] = false;
		      else
			matches[lane] = false;
		      goto out;
		    }
		  if (dump_enabled_p ())
		    {
		      dump_printf_loc (MSG_NOTE, vect_location,
				       "matched up op %d to\n", n);
		      vect_print_slp_tree (MSG_NOTE, vect_location, child);
		    }
		  children.safe_push (child);
		}
	    }
	  /* 3. build SLP nodes to combine the chain.  */
	  for (unsigned lane = 0; lane < group_size; ++lane)
	    if (chains[lane][0].code != code)
	      {
		/* See if there's any alternate all-PLUS entry.  */
		unsigned n;
		for (n = 1; n < chain_len; ++n)
		  {
		    for (lane = 0; lane < group_size; ++lane)
		      if (chains[lane][n].code != code)
			break;
		    if (lane == group_size)
		      break;
		  }
		if (n != chain_len)
		  {
		    /* Swap that in at first position.  */
		    std::swap (children[0], children[n]);
		    for (lane = 0; lane < group_size; ++lane)
		      std::swap (chains[lane][0], chains[lane][n]);
		  }
		else
		  {
		    /* ???  When this triggers and we end up with two
		       vect_constant/external_def up-front things break (ICE)
		       spectacularly finding an insertion place for the
		       all-constant op.  We should have a fully
		       vect_internal_def operand though(?) so we can swap
		       that into first place and then prepend the all-zero
		       constant.  */
		    if (dump_enabled_p ())
		      dump_printf_loc (MSG_NOTE, vect_location,
				       "inserting constant zero to compensate "
				       "for (partially) negated first "
				       "operand\n");
		    chain_len++;
		    for (lane = 0; lane < group_size; ++lane)
		      chains[lane].safe_insert
			(0, chain_op_t (code, vect_constant_def, NULL_TREE));
		    vec<tree> zero_ops;
		    zero_ops.create (group_size);
		    zero_ops.quick_push (build_zero_cst (TREE_TYPE (vectype)));
		    for (lane = 1; lane < group_size; ++lane)
		      zero_ops.quick_push (zero_ops[0]);
		    slp_tree zero = vect_create_new_slp_node (zero_ops);
		    SLP_TREE_DEF_TYPE (zero) = vect_constant_def;
		    children.safe_insert (0, zero);
		  }
		break;
	      }
	  for (unsigned i = 1; i < children.length (); ++i)
	    {
	      slp_tree op0 = children[i - 1];
	      slp_tree op1 = children[i];
	      bool this_two_op = false;
	      for (unsigned lane = 0; lane < group_size; ++lane)
		if (chains[lane][i].code != chains[0][i].code)
		  {
		    this_two_op = true;
		    break;
		  }
	      slp_tree child;
	      if (i == children.length () - 1)
		child = vect_create_new_slp_node (node, stmts, 2);
	      else
		child = vect_create_new_slp_node (2, ERROR_MARK);
	      if (this_two_op)
		{
		  vec<std::pair<unsigned, unsigned> > lperm;
		  lperm.create (group_size);
		  for (unsigned lane = 0; lane < group_size; ++lane)
		    lperm.quick_push (std::make_pair
		      (chains[lane][i].code != chains[0][i].code, lane));
		  vect_slp_build_two_operator_nodes (child, vectype, op0, op1,
						     (chains[0][i].code == code
						      ? op_stmt_info
						      : other_op_stmt_info),
						     (chains[0][i].code == code
						      ? other_op_stmt_info
						      : op_stmt_info),
						     lperm);
		}
	      else
		{
		  SLP_TREE_DEF_TYPE (child) = vect_internal_def;
		  SLP_TREE_VECTYPE (child) = vectype;
		  SLP_TREE_LANES (child) = group_size;
		  SLP_TREE_CHILDREN (child).quick_push (op0);
		  SLP_TREE_CHILDREN (child).quick_push (op1);
		  SLP_TREE_REPRESENTATIVE (child)
		    = (chains[0][i].code == code
		       ? op_stmt_info : other_op_stmt_info);
		}
	      children[i] = child;
	    }
	  *tree_size += this_tree_size + 1;
	  *max_nunits = this_max_nunits;
	  while (!chains.is_empty ())
	    chains.pop ().release ();
	  return node;
	}
out:
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "failed to line up SLP graph by re-associating "
			 "operations in lanes%s\n",
			 !hard_fail ? " trying regular discovery" : "");
      while (!children.is_empty ())
	vect_free_slp_tree (children.pop ());
      while (!chains.is_empty ())
	chains.pop ().release ();
      /* Hard-fail, otherwise we might run into quadratic processing of the
	 chains starting one stmt into the chain again.  */
      if (hard_fail)
	return NULL;
      /* Fall thru to normal processing.  */
    }

  /* Get at the operands, verifying they are compatible.  */
  vec<slp_oprnd_info> oprnds_info = vect_create_oprnd_info (nops, group_size);
  slp_oprnd_info oprnd_info;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    {
      int res = vect_get_and_check_slp_defs (vinfo, swap[i], skip_args,
					     stmts, i, &oprnds_info);
      if (res != 0)
	matches[(res == -1) ? 0 : i] = false;
      if (!matches[0])
	break;
    }
  for (i = 0; i < group_size; ++i)
    if (!matches[i])
      {
	vect_free_oprnd_info (oprnds_info);
	return NULL;
      }
  swap = NULL;

  bool has_two_operators_perm = false;
  auto_vec<unsigned> two_op_perm_indices[2];
  vec<stmt_vec_info> two_op_scalar_stmts[2] = {vNULL, vNULL};

  if (two_operators && oprnds_info.length () == 2 && group_size > 2)
    {
      unsigned idx = 0;
      hash_map<gimple *, unsigned> seen;
      vec<slp_oprnd_info> new_oprnds_info
	= vect_create_oprnd_info (1, group_size);
      bool success = true;

      enum tree_code code = ERROR_MARK;
      if (oprnds_info[0]->def_stmts[0]
	  && is_a<gassign *> (oprnds_info[0]->def_stmts[0]->stmt))
	code = gimple_assign_rhs_code (oprnds_info[0]->def_stmts[0]->stmt);

      for (unsigned j = 0; j < group_size; ++j)
	{
	  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
	    {
	      stmt_vec_info stmt_info = oprnd_info->def_stmts[j];
	      if (!stmt_info || !stmt_info->stmt
		  || !is_a<gassign *> (stmt_info->stmt)
		  || gimple_assign_rhs_code (stmt_info->stmt) != code
		  || skip_args[i])
		{
		  success = false;
		  break;
		}

	      bool exists;
	      unsigned &stmt_idx
		= seen.get_or_insert (stmt_info->stmt, &exists);

	      if (!exists)
		{
		  new_oprnds_info[0]->def_stmts.safe_push (stmt_info);
		  new_oprnds_info[0]->ops.safe_push (oprnd_info->ops[j]);
		  stmt_idx = idx;
		  idx++;
		}

	      two_op_perm_indices[i].safe_push (stmt_idx);
	    }

	  if (!success)
	    break;
	}

      if (success && idx == group_size)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Replace two_operators operands:\n");

	      FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
				   "Operand %u:\n", i);
		  for (unsigned j = 0; j < group_size; j++)
		    dump_printf_loc (MSG_NOTE, vect_location, "\tstmt %u %G",
				     j, oprnd_info->def_stmts[j]->stmt);
		}

	      dump_printf_loc (MSG_NOTE, vect_location,
			       "With a single operand:\n");
	      for (unsigned j = 0; j < group_size; j++)
		dump_printf_loc (MSG_NOTE, vect_location, "\tstmt %u %G",
				 j, new_oprnds_info[0]->def_stmts[j]->stmt);
	    }

	  two_op_scalar_stmts[0].safe_splice (oprnds_info[0]->def_stmts);
	  two_op_scalar_stmts[1].safe_splice (oprnds_info[1]->def_stmts);

	  new_oprnds_info[0]->first_op_type = oprnds_info[0]->first_op_type;
	  new_oprnds_info[0]->first_dt = oprnds_info[0]->first_dt;
	  new_oprnds_info[0]->any_pattern = oprnds_info[0]->any_pattern;
	  new_oprnds_info[0]->first_gs_p = oprnds_info[0]->first_gs_p;
	  new_oprnds_info[0]->first_gs_info = oprnds_info[0]->first_gs_info;

	  vect_free_oprnd_info (oprnds_info);
	  oprnds_info = new_oprnds_info;
	  nops = 1;
	  has_two_operators_perm = true;
	}
    }

  auto_vec<slp_tree, 4> children;

  stmt_info = stmts[0];

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      slp_tree child = nullptr;
      unsigned int j;

      /* We're skipping certain operands from processing, for example
	 outer loop reduction initial defs.  */
      if (skip_args[i])
	{
	  children.safe_push (NULL);
	  continue;
	}

      if (oprnd_info->first_dt == vect_uninitialized_def)
	{
	  /* COND_EXPR have one too many eventually if the condition
	     is a SSA name.  */
	  gcc_assert (i == 3 && nops == 4);
	  continue;
	}

      if (is_a <bb_vec_info> (vinfo)
	  && oprnd_info->first_dt == vect_internal_def
	  && !oprnd_info->any_pattern)
	{
	  /* For BB vectorization, if all defs are the same do not
	     bother to continue the build along the single-lane
	     graph but use a splat of the scalar value.  */
	  stmt_vec_info first_def = oprnd_info->def_stmts[0];
	  for (j = 1; j < group_size; ++j)
	    if (oprnd_info->def_stmts[j] != first_def)
	      break;
	  if (j == group_size
	      /* But avoid doing this for loads where we may be
		 able to CSE things, unless the stmt is not
		 vectorizable.  */
	      && (!STMT_VINFO_VECTORIZABLE (first_def)
		  || !gimple_vuse (first_def->stmt)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Using a splat of the uniform operand %G",
				 first_def->stmt);
	      oprnd_info->first_dt = vect_external_def;
	    }
	}

      if (oprnd_info->first_dt == vect_external_def
	  || oprnd_info->first_dt == vect_constant_def)
	{
	  if (!GET_MODE_SIZE (vinfo->vector_mode).is_constant ())
	    {
	      tree op0;
	      tree uniform_val = op0 = oprnd_info->ops[0];
	      for (j = 1; j < oprnd_info->ops.length (); ++j)
		if (!operand_equal_p (uniform_val, oprnd_info->ops[j]))
		  {
		    uniform_val = NULL_TREE;
		    break;
		  }
	      if (!uniform_val
		  && !can_duplicate_and_interleave_p (vinfo,
						      oprnd_info->ops.length (),
						      TREE_TYPE (op0)))
		{
		  matches[j] = false;
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: invalid type of def "
				     "for variable-length SLP %T\n", op0);
		  goto fail;
		}
	    }
	  slp_tree invnode = vect_create_new_slp_node (oprnd_info->ops);
	  SLP_TREE_DEF_TYPE (invnode) = oprnd_info->first_dt;
	  oprnd_info->ops = vNULL;
	  children.safe_push (invnode);
	  continue;
	}

      if ((child = vect_build_slp_tree (vinfo, oprnd_info->def_stmts,
					group_size, &this_max_nunits,
					matches, limit,
					&this_tree_size, bst_map)) != NULL)
	{
	  oprnd_info->def_stmts = vNULL;
	  children.safe_push (child);
	  continue;
	}

      /* If the SLP build for operand zero failed and operand zero
	 and one can be commutated try that for the scalar stmts
	 that failed the match.  */
      if (i == 0
	  /* A first scalar stmt mismatch signals a fatal mismatch.  */
	  && matches[0]
	  /* ???  For COND_EXPRs we can swap the comparison operands
	     as well as the arms under some constraints.  */
	  && nops == 2
	  && oprnds_info[1]->first_dt == vect_internal_def
	  && is_gimple_assign (stmt_info->stmt)
	  /* Swapping operands for reductions breaks assumptions later on.  */
	  && STMT_VINFO_REDUC_IDX (stmt_info) == -1)
	{
	  /* See whether we can swap the matching or the non-matching
	     stmt operands.  */
	  bool swap_not_matching = true;
	  do
	    {
	      for (j = 0; j < group_size; ++j)
		{
		  if (matches[j] != !swap_not_matching)
		    continue;
		  stmt_vec_info stmt_info = stmts[j];
		  /* Verify if we can swap operands of this stmt.  */
		  gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt);
		  if (!stmt
		      || !commutative_tree_code (gimple_assign_rhs_code (stmt)))
		    {
		      if (!swap_not_matching)
			goto fail;
		      swap_not_matching = false;
		      break;
		    }
		}
	    }
	  while (j != group_size);

	  /* Swap mismatched definition stmts.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Re-trying with swapped operands of stmts ");
	  for (j = 0; j < group_size; ++j)
	    if (matches[j] == !swap_not_matching)
	      {
		std::swap (oprnds_info[0]->def_stmts[j],
			   oprnds_info[1]->def_stmts[j]);
		std::swap (oprnds_info[0]->ops[j],
			   oprnds_info[1]->ops[j]);
		if (dump_enabled_p ())
		  dump_printf (MSG_NOTE, "%d ", j);
	      }
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "\n");
	  /* After swapping some operands we lost track whether an
	     operand has any pattern defs so be conservative here.  */
	  if (oprnds_info[0]->any_pattern || oprnds_info[1]->any_pattern)
	    oprnds_info[0]->any_pattern = oprnds_info[1]->any_pattern = true;
	  /* And try again with scratch 'matches' ... */
	  bool *tem = XALLOCAVEC (bool, group_size);
	  if ((child = vect_build_slp_tree (vinfo, oprnd_info->def_stmts,
					    group_size, &this_max_nunits,
					    tem, limit,
					    &this_tree_size, bst_map)) != NULL)
	    {
	      oprnd_info->def_stmts = vNULL;
	      children.safe_push (child);
	      continue;
	    }
	}
fail:

      /* If the SLP build failed and we analyze a basic-block
	 simply treat nodes we fail to build as externally defined
	 (and thus build vectors from the scalar defs).
	 The cost model will reject outright expensive cases.
	 ???  This doesn't treat cases where permutation ultimatively
	 fails (or we don't try permutation below).  Ideally we'd
	 even compute a permutation that will end up with the maximum
	 SLP tree size...  */
      if (is_a <bb_vec_info> (vinfo)
	  /* ???  Rejecting patterns this way doesn't work.  We'd have to
	     do extra work to cancel the pattern so the uses see the
	     scalar version.  */
	  && !is_pattern_stmt_p (stmt_info)
	  && !oprnd_info->any_pattern)
	{
	  /* But if there's a leading vector sized set of matching stmts
	     fail here so we can split the group.  This matches the condition
	     vect_analyze_slp_instance uses.  */
	  /* ???  We might want to split here and combine the results to support
	     multiple vector sizes better.  */
	  for (j = 0; j < group_size; ++j)
	    if (!matches[j])
	      break;
	  if (!known_ge (j, TYPE_VECTOR_SUBPARTS (vectype)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Building vector operands from scalars\n");
	      this_tree_size++;
	      child = vect_create_new_slp_node (oprnd_info->ops);
	      children.safe_push (child);
	      oprnd_info->ops = vNULL;
	      continue;
	    }
	}

      gcc_assert (child == NULL);
      FOR_EACH_VEC_ELT (children, j, child)
	if (child)
	  vect_free_slp_tree (child);
      vect_free_oprnd_info (oprnds_info);
      return NULL;
    }

  vect_free_oprnd_info (oprnds_info);

  /* If we have all children of a child built up from uniform scalars
     or does more than one possibly expensive vector construction then
     just throw that away, causing it built up from scalars.
     The exception is the SLP node for the vector store.  */
  if (is_a <bb_vec_info> (vinfo)
      && !STMT_VINFO_GROUPED_ACCESS (stmt_info)
      /* ???  Rejecting patterns this way doesn't work.  We'd have to
	 do extra work to cancel the pattern so the uses see the
	 scalar version.  */
      && !is_pattern_stmt_p (stmt_info))
    {
      slp_tree child;
      unsigned j;
      bool all_uniform_p = true;
      unsigned n_vector_builds = 0;
      FOR_EACH_VEC_ELT (children, j, child)
	{
	  if (!child)
	    ;
	  else if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	    all_uniform_p = false;
	  else if (!vect_slp_tree_uniform_p (child))
	    {
	      all_uniform_p = false;
	      if (SLP_TREE_DEF_TYPE (child) == vect_external_def)
		n_vector_builds++;
	    }
	}
      if (all_uniform_p
	  || n_vector_builds > 1
	  || (n_vector_builds == children.length ()
	      && is_a <gphi *> (stmt_info->stmt)))
	{
	  /* Roll back.  */
	  matches[0] = false;
	  FOR_EACH_VEC_ELT (children, j, child)
	    if (child)
	      vect_free_slp_tree (child);

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Building parent vector operands from "
			     "scalars instead\n");
	  return NULL;
	}
    }

  *tree_size += this_tree_size + 1;
  *max_nunits = this_max_nunits;

  if (two_operators)
    {
      /* ???  We'd likely want to either cache in bst_map sth like
	 { a+b, NULL, a+b, NULL } and { NULL, a-b, NULL, a-b } or
	 the true { a+b, a+b, a+b, a+b } ... but there we don't have
	 explicit stmts to put in so the keying on 'stmts' doesn't
	 work (but we have the same issue with nodes that use 'ops').  */

      if (has_two_operators_perm)
	{
	  slp_tree child = children[0];
	  children.truncate (0);
	  for (i = 0; i < 2; i++)
	    {
	      slp_tree pnode
		= vect_create_new_slp_node (two_op_scalar_stmts[i], 2);
	      SLP_TREE_CODE (pnode) = VEC_PERM_EXPR;
	      SLP_TREE_VECTYPE (pnode) = vectype;
	      SLP_TREE_CHILDREN (pnode).quick_push (child);
	      SLP_TREE_CHILDREN (pnode).quick_push (child);
	      lane_permutation_t& perm = SLP_TREE_LANE_PERMUTATION (pnode);
	      children.safe_push (pnode);

	      for (unsigned j = 0; j < stmts.length (); j++)
		perm.safe_push (std::make_pair (0, two_op_perm_indices[i][j]));
	    }

	  SLP_TREE_REF_COUNT (child) += 4;
	}

      slp_tree one = new _slp_tree;
      slp_tree two = new _slp_tree;
      SLP_TREE_DEF_TYPE (one) = vect_internal_def;
      SLP_TREE_DEF_TYPE (two) = vect_internal_def;
      SLP_TREE_VECTYPE (one) = vectype;
      SLP_TREE_VECTYPE (two) = vectype;
      SLP_TREE_CHILDREN (one).safe_splice (children);
      SLP_TREE_CHILDREN (two).safe_splice (children);
      slp_tree child;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (two), i, child)
	SLP_TREE_REF_COUNT (child)++;

      /* Here we record the original defs since this
	 node represents the final lane configuration.  */
      node = vect_create_new_slp_node (node, stmts, 2);
      SLP_TREE_VECTYPE (node) = vectype;
      SLP_TREE_CODE (node) = VEC_PERM_EXPR;
      SLP_TREE_CHILDREN (node).quick_push (one);
      SLP_TREE_CHILDREN (node).quick_push (two);
      gassign *stmt = as_a <gassign *> (stmts[0]->stmt);
      enum tree_code code0 = gimple_assign_rhs_code (stmt);
      enum tree_code ocode = ERROR_MARK;
      stmt_vec_info ostmt_info;
      unsigned j = 0;
      FOR_EACH_VEC_ELT (stmts, i, ostmt_info)
	{
	  gassign *ostmt = as_a <gassign *> (ostmt_info->stmt);
	  if (gimple_assign_rhs_code (ostmt) != code0)
	    {
	      SLP_TREE_LANE_PERMUTATION (node).safe_push (std::make_pair (1, i));
	      ocode = gimple_assign_rhs_code (ostmt);
	      j = i;
	    }
	  else
	    SLP_TREE_LANE_PERMUTATION (node).safe_push (std::make_pair (0, i));
	}

      SLP_TREE_CODE (one) = code0;
      SLP_TREE_CODE (two) = ocode;
      SLP_TREE_LANES (one) = stmts.length ();
      SLP_TREE_LANES (two) = stmts.length ();
      SLP_TREE_REPRESENTATIVE (one) = stmts[0];
      SLP_TREE_REPRESENTATIVE (two) = stmts[j];

      return node;
    }

  node = vect_create_new_slp_node (node, stmts, nops);
  SLP_TREE_VECTYPE (node) = vectype;
  SLP_TREE_CHILDREN (node).splice (children);
  return node;
}

/* Dump a single SLP tree NODE.  */

static void
vect_print_slp_tree (dump_flags_t dump_kind, dump_location_t loc,
		     slp_tree node)
{
  unsigned i, j;
  slp_tree child;
  stmt_vec_info stmt_info;
  tree op;

  dump_metadata_t metadata (dump_kind, loc.get_impl_location ());
  dump_user_location_t user_loc = loc.get_user_location ();
  dump_printf_loc (metadata, user_loc,
		   "node%s %p (max_nunits=" HOST_WIDE_INT_PRINT_UNSIGNED
		   ", refcnt=%u)",
		   SLP_TREE_DEF_TYPE (node) == vect_external_def
		   ? " (external)"
		   : (SLP_TREE_DEF_TYPE (node) == vect_constant_def
		      ? " (constant)"
		      : ""), (void *) node,
		   estimated_poly_value (node->max_nunits),
					 SLP_TREE_REF_COUNT (node));
  if (SLP_TREE_VECTYPE (node))
    dump_printf (metadata, " %T", SLP_TREE_VECTYPE (node));
  dump_printf (metadata, "\n");
  if (SLP_TREE_DEF_TYPE (node) == vect_internal_def)
    {
      if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
	dump_printf_loc (metadata, user_loc, "op: VEC_PERM_EXPR\n");
      else
	dump_printf_loc (metadata, user_loc, "op template: %G",
			 SLP_TREE_REPRESENTATIVE (node)->stmt);
    }
  if (SLP_TREE_SCALAR_STMTS (node).exists ())
    FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
      if (stmt_info)
	dump_printf_loc (metadata, user_loc, "\t%sstmt %u %G",
			 STMT_VINFO_LIVE_P (stmt_info) ? "[l] " : "",
			 i, stmt_info->stmt);
      else
	dump_printf_loc (metadata, user_loc, "\tstmt %u ---\n", i);
  else
    {
      dump_printf_loc (metadata, user_loc, "\t{ ");
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (node), i, op)
	dump_printf (metadata, "%T%s ", op,
		     i < SLP_TREE_SCALAR_OPS (node).length () - 1 ? "," : "");
      dump_printf (metadata, "}\n");
    }
  if (SLP_TREE_LOAD_PERMUTATION (node).exists ())
    {
      dump_printf_loc (metadata, user_loc, "\tload permutation {");
      FOR_EACH_VEC_ELT (SLP_TREE_LOAD_PERMUTATION (node), i, j)
	dump_printf (dump_kind, " %u", j);
      dump_printf (dump_kind, " }\n");
    }
  if (SLP_TREE_LANE_PERMUTATION (node).exists ())
    {
      dump_printf_loc (metadata, user_loc, "\tlane permutation {");
      for (i = 0; i < SLP_TREE_LANE_PERMUTATION (node).length (); ++i)
	dump_printf (dump_kind, " %u[%u]",
		     SLP_TREE_LANE_PERMUTATION (node)[i].first,
		     SLP_TREE_LANE_PERMUTATION (node)[i].second);
      dump_printf (dump_kind, " }%s\n",
		   node->ldst_lanes ? " (load-lanes)" : "");
    }
  if (SLP_TREE_CHILDREN (node).is_empty ())
    return;
  dump_printf_loc (metadata, user_loc, "\tchildren");
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    dump_printf (dump_kind, " %p", (void *)child);
  dump_printf (dump_kind, "%s\n",
	       node->ldst_lanes && !SLP_TREE_LANE_PERMUTATION (node).exists ()
	       ? " (store-lanes)" : "");
}

DEBUG_FUNCTION void
debug (slp_tree node)
{
  debug_dump_context ctx;
  vect_print_slp_tree (MSG_NOTE,
		       dump_location_t::from_location_t (UNKNOWN_LOCATION),
		       node);
}

/* Recursive helper for the dot producer below.  */

static void
dot_slp_tree (FILE *f, slp_tree node, hash_set<slp_tree> &visited)
{
  if (visited.add (node))
    return;

  fprintf (f, "\"%p\" [label=\"", (void *)node);
  vect_print_slp_tree (MSG_NOTE,
		       dump_location_t::from_location_t (UNKNOWN_LOCATION),
		       node);
  fprintf (f, "\"];\n");


  for (slp_tree child : SLP_TREE_CHILDREN (node))
    fprintf (f, "\"%p\" -> \"%p\";", (void *)node, (void *)child);

  for (slp_tree child : SLP_TREE_CHILDREN (node))
    if (child)
      dot_slp_tree (f, child, visited);
}

DEBUG_FUNCTION void
dot_slp_tree (const char *fname, slp_tree node)
{
  FILE *f = fopen (fname, "w");
  fprintf (f, "digraph {\n");
  fflush (f);
    {
      debug_dump_context ctx (f);
      hash_set<slp_tree> visited;
      dot_slp_tree (f, node, visited);
    }
  fflush (f);
  fprintf (f, "}\n");
  fclose (f);
}

DEBUG_FUNCTION void
dot_slp_tree (const char *fname, const vec<slp_instance> &slp_instances)
{
  FILE *f = fopen (fname, "w");
  fprintf (f, "digraph {\n");
  fflush (f);
    {
      debug_dump_context ctx (f);
      hash_set<slp_tree> visited;
      for (auto inst : slp_instances)
	dot_slp_tree (f, SLP_INSTANCE_TREE (inst), visited);
    }
  fflush (f);
  fprintf (f, "}\n");
  fclose (f);
}

/* Dump a slp tree NODE using flags specified in DUMP_KIND.  */

static void
vect_print_slp_graph (dump_flags_t dump_kind, dump_location_t loc,
		      slp_tree node, hash_set<slp_tree> &visited)
{
  unsigned i;
  slp_tree child;

  if (visited.add (node))
    return;

  vect_print_slp_tree (dump_kind, loc, node);

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      vect_print_slp_graph (dump_kind, loc, child, visited);
}

static void
vect_print_slp_graph (dump_flags_t dump_kind, dump_location_t loc,
		      slp_tree entry)
{
  hash_set<slp_tree> visited;
  vect_print_slp_graph (dump_kind, loc, entry, visited);
}

DEBUG_FUNCTION void
debug (slp_instance instance)
{
  debug_dump_context ctx;
  vect_print_slp_graph (MSG_NOTE,
			dump_location_t::from_location_t (UNKNOWN_LOCATION),
			SLP_INSTANCE_TREE (instance));
}

/* Mark the tree rooted at NODE with PURE_SLP.  */

static void
vect_mark_slp_stmts (slp_tree node, hash_set<slp_tree> &visited)
{
  int i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    if (stmt_info)
      STMT_SLP_TYPE (stmt_info) = pure_slp;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      vect_mark_slp_stmts (child, visited);
}

static void
vect_mark_slp_stmts (slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_mark_slp_stmts (node, visited);
}

/* Mark the statements of the tree rooted at NODE as relevant (vect_used).  */

static void
vect_mark_slp_stmts_relevant (slp_tree node, hash_set<slp_tree> &visited)
{
  int i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    if (stmt_info)
      {
	gcc_assert (!STMT_VINFO_RELEVANT (stmt_info)
		    || STMT_VINFO_RELEVANT (stmt_info) == vect_used_in_scope);
	STMT_VINFO_RELEVANT (stmt_info) = vect_used_in_scope;
      }

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      vect_mark_slp_stmts_relevant (child, visited);
}

static void
vect_mark_slp_stmts_relevant (slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_mark_slp_stmts_relevant (node, visited);
}


/* Gather loads in the SLP graph NODE and populate the INST loads array.  */

static void
vect_gather_slp_loads (vec<slp_tree> &loads, slp_tree node,
		       hash_set<slp_tree> &visited)
{
  if (!node || visited.add (node))
    return;

  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (SLP_TREE_CODE (node) != VEC_PERM_EXPR)
    {
      stmt_vec_info stmt_info = SLP_TREE_REPRESENTATIVE (node);
      if (STMT_VINFO_DATA_REF (stmt_info)
	  && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
	loads.safe_push (node);
    }

  unsigned i;
  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_gather_slp_loads (loads, child, visited);
}


/* Find the last store in SLP INSTANCE.  */

stmt_vec_info
vect_find_last_scalar_stmt_in_slp (slp_tree node)
{
  stmt_vec_info last = NULL;
  stmt_vec_info stmt_vinfo;

  for (int i = 0; SLP_TREE_SCALAR_STMTS (node).iterate (i, &stmt_vinfo); i++)
    if (stmt_vinfo)
      {
	stmt_vinfo = vect_orig_stmt (stmt_vinfo);
	last = last ? get_later_stmt (stmt_vinfo, last) : stmt_vinfo;
      }

  return last;
}

/* Find the first stmt in NODE.  */

stmt_vec_info
vect_find_first_scalar_stmt_in_slp (slp_tree node)
{
  stmt_vec_info first = NULL;
  stmt_vec_info stmt_vinfo;

  for (int i = 0; SLP_TREE_SCALAR_STMTS (node).iterate (i, &stmt_vinfo); i++)
    if (stmt_vinfo)
      {
	stmt_vinfo = vect_orig_stmt (stmt_vinfo);
	if (!first
	    || get_later_stmt (stmt_vinfo, first) == first)
	  first = stmt_vinfo;
      }

  return first;
}

/* Splits a group of stores, currently beginning at FIRST_VINFO, into
   two groups: one (still beginning at FIRST_VINFO) of size GROUP1_SIZE
   (also containing the first GROUP1_SIZE stmts, since stores are
   consecutive), the second containing the remainder.
   Return the first stmt in the second group.  */

static stmt_vec_info
vect_split_slp_store_group (stmt_vec_info first_vinfo, unsigned group1_size)
{
  gcc_assert (DR_GROUP_FIRST_ELEMENT (first_vinfo) == first_vinfo);
  gcc_assert (group1_size > 0);
  int group2_size = DR_GROUP_SIZE (first_vinfo) - group1_size;
  gcc_assert (group2_size > 0);
  DR_GROUP_SIZE (first_vinfo) = group1_size;

  stmt_vec_info stmt_info = first_vinfo;
  for (unsigned i = group1_size; i > 1; i--)
    {
      stmt_info = DR_GROUP_NEXT_ELEMENT (stmt_info);
      gcc_assert (DR_GROUP_GAP (stmt_info) == 1);
    }
  /* STMT is now the last element of the first group.  */
  stmt_vec_info group2 = DR_GROUP_NEXT_ELEMENT (stmt_info);
  DR_GROUP_NEXT_ELEMENT (stmt_info) = 0;

  DR_GROUP_SIZE (group2) = group2_size;
  for (stmt_info = group2; stmt_info;
       stmt_info = DR_GROUP_NEXT_ELEMENT (stmt_info))
    {
      DR_GROUP_FIRST_ELEMENT (stmt_info) = group2;
      gcc_assert (DR_GROUP_GAP (stmt_info) == 1);
    }

  /* For the second group, the DR_GROUP_GAP is that before the original group,
     plus skipping over the first vector.  */
  DR_GROUP_GAP (group2) = DR_GROUP_GAP (first_vinfo) + group1_size;

  /* DR_GROUP_GAP of the first group now has to skip over the second group too.  */
  DR_GROUP_GAP (first_vinfo) += group2_size;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Split group into %d and %d\n",
		     group1_size, group2_size);

  return group2;
}

/* Calculate the unrolling factor for an SLP instance with GROUP_SIZE
   statements and a vector of NUNITS elements.  */

static poly_uint64
calculate_unrolling_factor (poly_uint64 nunits, unsigned int group_size)
{
  return exact_div (common_multiple (nunits, group_size), group_size);
}

/* Helper that checks to see if a node is a load node.  */

static inline bool
vect_is_slp_load_node  (slp_tree root)
{
  return (SLP_TREE_CODE (root) != VEC_PERM_EXPR
	  && SLP_TREE_DEF_TYPE (root) == vect_internal_def
	  && STMT_VINFO_GROUPED_ACCESS (SLP_TREE_REPRESENTATIVE (root))
	  && DR_IS_READ (STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (root))));
}


/* Helper function of optimize_load_redistribution that performs the operation
   recursively.  */

static slp_tree
optimize_load_redistribution_1 (scalar_stmts_to_slp_tree_map_t *bst_map,
				vec_info *vinfo, unsigned int group_size,
				hash_map<slp_tree, slp_tree> *load_map,
				slp_tree root)
{
  if (slp_tree *leader = load_map->get (root))
    return *leader;

  slp_tree node;
  unsigned i;

  /* For now, we don't know anything about externals so do not do anything.  */
  if (!root || SLP_TREE_DEF_TYPE (root) != vect_internal_def)
    return NULL;
  else if (SLP_TREE_CODE (root) == VEC_PERM_EXPR)
    {
      /* First convert this node into a load node and add it to the leaves
	 list and flatten the permute from a lane to a load one.  If it's
	 unneeded it will be elided later.  */
      vec<stmt_vec_info> stmts;
      stmts.create (SLP_TREE_LANES (root));
      lane_permutation_t lane_perm = SLP_TREE_LANE_PERMUTATION (root);
      for (unsigned j = 0; j < lane_perm.length (); j++)
	{
	  std::pair<unsigned, unsigned> perm = lane_perm[j];
	  node = SLP_TREE_CHILDREN (root)[perm.first];

	  if (!vect_is_slp_load_node (node)
	      || SLP_TREE_CHILDREN (node).exists ())
	    {
	      stmts.release ();
	      goto next;
	    }

	  stmts.quick_push (SLP_TREE_SCALAR_STMTS (node)[perm.second]);
	}

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "converting stmts on permute node %p\n",
			 (void *) root);

      bool *matches = XALLOCAVEC (bool, group_size);
      poly_uint64 max_nunits = 1;
      unsigned tree_size = 0, limit = 1;
      node = vect_build_slp_tree (vinfo, stmts, group_size, &max_nunits,
				  matches, &limit, &tree_size, bst_map);
      if (!node)
	stmts.release ();

      load_map->put (root, node);
      return node;
    }

next:
  load_map->put (root, NULL);

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (root), i , node)
    {
      slp_tree value
	= optimize_load_redistribution_1 (bst_map, vinfo, group_size, load_map,
					  node);
      if (value)
	{
	  SLP_TREE_REF_COUNT (value)++;
	  SLP_TREE_CHILDREN (root)[i] = value;
	  /* ???  We know the original leafs of the replaced nodes will
	     be referenced by bst_map, only the permutes created by
	     pattern matching are not.  */
	  if (SLP_TREE_REF_COUNT (node) == 1)
	    load_map->remove (node);
	  vect_free_slp_tree (node);
	}
    }

  return NULL;
}

/* Temporary workaround for loads not being CSEd during SLP build.  This
   function will traverse the SLP tree rooted in ROOT for INSTANCE and find
   VEC_PERM nodes that blend vectors from multiple nodes that all read from the
   same DR such that the final operation is equal to a permuted load.  Such
   NODES are then directly converted into LOADS themselves.  The nodes are
   CSEd using BST_MAP.  */

static void
optimize_load_redistribution (scalar_stmts_to_slp_tree_map_t *bst_map,
			      vec_info *vinfo, unsigned int group_size,
			      hash_map<slp_tree, slp_tree> *load_map,
			      slp_tree root)
{
  slp_tree node;
  unsigned i;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (root), i , node)
    {
      slp_tree value
	= optimize_load_redistribution_1 (bst_map, vinfo, group_size, load_map,
					  node);
      if (value)
	{
	  SLP_TREE_REF_COUNT (value)++;
	  SLP_TREE_CHILDREN (root)[i] = value;
	  /* ???  We know the original leafs of the replaced nodes will
	     be referenced by bst_map, only the permutes created by
	     pattern matching are not.  */
	  if (SLP_TREE_REF_COUNT (node) == 1)
	    load_map->remove (node);
	  vect_free_slp_tree (node);
	}
    }
}

/* Helper function of vect_match_slp_patterns.

   Attempts to match patterns against the slp tree rooted in REF_NODE using
   VINFO.  Patterns are matched in post-order traversal.

   If matching is successful the value in REF_NODE is updated and returned, if
   not then it is returned unchanged.  */

static bool
vect_match_slp_patterns_2 (slp_tree *ref_node, vec_info *vinfo,
			   slp_tree_to_load_perm_map_t *perm_cache,
			   slp_compat_nodes_map_t *compat_cache,
			   hash_set<slp_tree> *visited)
{
  unsigned i;
  slp_tree node = *ref_node;
  bool found_p = false;
  if (!node || visited->add (node))
    return false;

  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    found_p |= vect_match_slp_patterns_2 (&SLP_TREE_CHILDREN (node)[i],
					  vinfo, perm_cache, compat_cache,
					  visited);

  for (unsigned x = 0; x < num__slp_patterns; x++)
    {
      vect_pattern *pattern
	= slp_patterns[x] (perm_cache, compat_cache, ref_node);
      if (pattern)
	{
	  pattern->build (vinfo);
	  delete pattern;
	  found_p = true;
	}
    }

  return found_p;
}

/* Applies pattern matching to the given SLP tree rooted in REF_NODE using
   vec_info VINFO.

   The modified tree is returned.  Patterns are tried in order and multiple
   patterns may match.  */

static bool
vect_match_slp_patterns (slp_instance instance, vec_info *vinfo,
			 hash_set<slp_tree> *visited,
			 slp_tree_to_load_perm_map_t *perm_cache,
			 slp_compat_nodes_map_t *compat_cache)
{
  DUMP_VECT_SCOPE ("vect_match_slp_patterns");
  slp_tree *ref_node = &SLP_INSTANCE_TREE (instance);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Analyzing SLP tree %p for patterns\n",
		     (void *) SLP_INSTANCE_TREE (instance));

  return vect_match_slp_patterns_2 (ref_node, vinfo, perm_cache, compat_cache,
				    visited);
}

/* STMT_INFO is a store group of size GROUP_SIZE that we are considering
   vectorizing with VECTYPE that might be NULL.  MASKED_P indicates whether
   the stores are masked.
   Return true if we could use IFN_STORE_LANES instead and if that appears
   to be the better approach.  */

static bool
vect_slp_prefer_store_lanes_p (vec_info *vinfo, stmt_vec_info stmt_info,
			       tree vectype, bool masked_p,
			       unsigned int group_size,
			       unsigned int new_group_size)
{
  if (!vectype)
    {
      tree scalar_type = TREE_TYPE (DR_REF (STMT_VINFO_DATA_REF (stmt_info)));
      vectype = get_vectype_for_scalar_type (vinfo, scalar_type);
    }
  if (!vectype)
    return false;
  /* Allow the split if one of the two new groups would operate on full
     vectors *within* rather than across one scalar loop iteration.
     This is purely a heuristic, but it should work well for group
     sizes of 3 and 4, where the possible splits are:

       3->2+1:  OK if the vector has exactly two elements
       4->2+2:  Likewise
       4->3+1:  Less clear-cut.  */
  if (multiple_p (group_size - new_group_size, TYPE_VECTOR_SUBPARTS (vectype))
      || multiple_p (new_group_size, TYPE_VECTOR_SUBPARTS (vectype)))
    return false;
  return vect_store_lanes_supported (vectype, group_size, masked_p) != IFN_LAST;
}

/* Analyze an SLP instance starting from a group of grouped stores.  Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (vec_info *vinfo,
			   scalar_stmts_to_slp_tree_map_t *bst_map,
			   stmt_vec_info stmt_info, slp_instance_kind kind,
			   unsigned max_tree_size, unsigned *limit,
			   bool force_single_lane);

/* Build an interleaving scheme for the store sources RHS_NODES from
   SCALAR_STMTS.  */

static slp_tree
vect_build_slp_store_interleaving (vec<slp_tree> &rhs_nodes,
				   vec<stmt_vec_info> &scalar_stmts)
{
  unsigned int group_size = scalar_stmts.length ();
  slp_tree node = vect_create_new_slp_node (scalar_stmts,
					    SLP_TREE_CHILDREN
					      (rhs_nodes[0]).length ());
  SLP_TREE_VECTYPE (node) = SLP_TREE_VECTYPE (rhs_nodes[0]);
  for (unsigned l = 0;
       l < SLP_TREE_CHILDREN (rhs_nodes[0]).length (); ++l)
    {
      /* And a permute merging all RHS SLP trees.  */
      slp_tree perm = vect_create_new_slp_node (rhs_nodes.length (),
						VEC_PERM_EXPR);
      SLP_TREE_CHILDREN (node).quick_push (perm);
      SLP_TREE_LANE_PERMUTATION (perm).create (group_size);
      SLP_TREE_VECTYPE (perm) = SLP_TREE_VECTYPE (node);
      SLP_TREE_LANES (perm) = group_size;
      /* ???  We should set this NULL but that's not expected.  */
      SLP_TREE_REPRESENTATIVE (perm)
	= SLP_TREE_REPRESENTATIVE (SLP_TREE_CHILDREN (rhs_nodes[0])[l]);
      for (unsigned j = 0; j < rhs_nodes.length (); ++j)
	{
	  SLP_TREE_CHILDREN (perm)
	    .quick_push (SLP_TREE_CHILDREN (rhs_nodes[j])[l]);
	  SLP_TREE_CHILDREN (rhs_nodes[j])[l]->refcnt++;
	  for (unsigned k = 0;
	       k < SLP_TREE_SCALAR_STMTS (rhs_nodes[j]).length (); ++k)
	    {
	      /* ???  We should populate SLP_TREE_SCALAR_STMTS
		 or SLP_TREE_SCALAR_OPS but then we might have
		 a mix of both in our children.  */
	      SLP_TREE_LANE_PERMUTATION (perm)
		.quick_push (std::make_pair (j, k));
	    }
	}

      /* Now we have a single permute node but we cannot code-generate
	 the case with more than two inputs.
	 Perform pairwise reduction, reducing the two inputs
	 with the least number of lanes to one and then repeat until
	 we end up with two inputs.  That scheme makes sure we end
	 up with permutes satisfying the restriction of requiring at
	 most two vector inputs to produce a single vector output
	 when the number of lanes is even.  */
      while (SLP_TREE_CHILDREN (perm).length () > 2)
	{
	  /* When we have three equal sized groups left the pairwise
	     reduction does not result in a scheme that avoids using
	     three vectors.  Instead merge the first two groups
	     to the final size with do-not-care elements (chosen
	     from the first group) and then merge with the third.
		  { A0, B0,  x, A1, B1,  x, ... }
	       -> { A0, B0, C0, A1, B1, C1, ... }
	     This handles group size of three (and at least
	     power-of-two multiples of that).  */
	  if (SLP_TREE_CHILDREN (perm).length () == 3
	      && (SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[0])
		  == SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[1]))
	      && (SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[0])
		  == SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[2])))
	    {
	      int ai = 0;
	      int bi = 1;
	      slp_tree a = SLP_TREE_CHILDREN (perm)[ai];
	      slp_tree b = SLP_TREE_CHILDREN (perm)[bi];
	      unsigned n = SLP_TREE_LANES (perm);

	      slp_tree permab = vect_create_new_slp_node (2, VEC_PERM_EXPR);
	      SLP_TREE_LANES (permab) = n;
	      SLP_TREE_LANE_PERMUTATION (permab).create (n);
	      SLP_TREE_VECTYPE (permab) = SLP_TREE_VECTYPE (perm);
	      /* ???  Should be NULL but that's not expected.  */
	      SLP_TREE_REPRESENTATIVE (permab) = SLP_TREE_REPRESENTATIVE (perm);
	      SLP_TREE_CHILDREN (permab).quick_push (a);
	      for (unsigned k = 0; k < SLP_TREE_LANES (a); ++k)
		SLP_TREE_LANE_PERMUTATION (permab)
		  .quick_push (std::make_pair (0, k));
	      SLP_TREE_CHILDREN (permab).quick_push (b);
	      for (unsigned k = 0; k < SLP_TREE_LANES (b); ++k)
		SLP_TREE_LANE_PERMUTATION (permab)
		  .quick_push (std::make_pair (1, k));
	      /* Push the do-not-care lanes.  */
	      for (unsigned k = 0; k < SLP_TREE_LANES (a); ++k)
		SLP_TREE_LANE_PERMUTATION (permab)
		  .quick_push (std::make_pair (0, k));

	      /* Put the merged node into 'perm', in place of a.  */
	      SLP_TREE_CHILDREN (perm)[ai] = permab;
	      /* Adjust the references to b in the permutation
		 of perm and to the later children which we'll
		 remove.  */
	      for (unsigned k = 0; k < SLP_TREE_LANES (perm); ++k)
		{
		  std::pair<unsigned, unsigned> &p
		    = SLP_TREE_LANE_PERMUTATION (perm)[k];
		  if (p.first == (unsigned) bi)
		    {
		      p.first = ai;
		      p.second += SLP_TREE_LANES (a);
		    }
		  else if (p.first > (unsigned) bi)
		    p.first--;
		}
	      SLP_TREE_CHILDREN (perm).ordered_remove (bi);
	      break;
	    }

	  /* Pick the two nodes with the least number of lanes,
	     prefer the earliest candidate and maintain ai < bi.  */
	  int ai = -1;
	  int bi = -1;
	  for (unsigned ci = 0; ci < SLP_TREE_CHILDREN (perm).length (); ++ci)
	    {
	      if (ai == -1)
		ai = ci;
	      else if (bi == -1)
		bi = ci;
	      else if ((SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[ci])
			< SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[ai]))
		       || (SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[ci])
			   < SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[bi])))
		{
		  if (SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[ai])
		      <= SLP_TREE_LANES (SLP_TREE_CHILDREN (perm)[bi]))
		    bi = ci;
		  else
		    {
		      ai = bi;
		      bi = ci;
		    }
		}
	    }

	  /* Produce a merge of nodes ai and bi.  */
	  slp_tree a = SLP_TREE_CHILDREN (perm)[ai];
	  slp_tree b = SLP_TREE_CHILDREN (perm)[bi];
	  unsigned n = SLP_TREE_LANES (a) + SLP_TREE_LANES (b);
	  slp_tree permab = vect_create_new_slp_node (2, VEC_PERM_EXPR);
	  SLP_TREE_LANES (permab) = n;
	  SLP_TREE_LANE_PERMUTATION (permab).create (n);
	  SLP_TREE_VECTYPE (permab) = SLP_TREE_VECTYPE (perm);
	  /* ???  Should be NULL but that's not expected.  */
	  SLP_TREE_REPRESENTATIVE (permab) = SLP_TREE_REPRESENTATIVE (perm);
	  SLP_TREE_CHILDREN (permab).quick_push (a);
	  for (unsigned k = 0; k < SLP_TREE_LANES (a); ++k)
	    SLP_TREE_LANE_PERMUTATION (permab)
	      .quick_push (std::make_pair (0, k));
	  SLP_TREE_CHILDREN (permab).quick_push (b);
	  for (unsigned k = 0; k < SLP_TREE_LANES (b); ++k)
	    SLP_TREE_LANE_PERMUTATION (permab)
	      .quick_push (std::make_pair (1, k));

	  /* Put the merged node into 'perm', in place of a.  */
	  SLP_TREE_CHILDREN (perm)[ai] = permab;
	  /* Adjust the references to b in the permutation
	     of perm and to the later children which we'll
	     remove.  */
	  for (unsigned k = 0; k < SLP_TREE_LANES (perm); ++k)
	    {
	      std::pair<unsigned, unsigned> &p
		= SLP_TREE_LANE_PERMUTATION (perm)[k];
	      if (p.first == (unsigned) bi)
		{
		  p.first = ai;
		  p.second += SLP_TREE_LANES (a);
		}
	      else if (p.first > (unsigned) bi)
		p.first--;
	    }
	  SLP_TREE_CHILDREN (perm).ordered_remove (bi);
	}
    }

  return node;
}

/* Analyze an SLP instance starting from SCALAR_STMTS which are a group
   of KIND.  Return true if successful.  */

static bool
vect_build_slp_instance (vec_info *vinfo,
			 slp_instance_kind kind,
			 vec<stmt_vec_info> &scalar_stmts,
			 vec<stmt_vec_info> &root_stmt_infos,
			 vec<tree> &remain,
			 unsigned max_tree_size, unsigned *limit,
			 scalar_stmts_to_slp_tree_map_t *bst_map,
			 /* ???  We need stmt_info for group splitting.  */
			 stmt_vec_info stmt_info_,
			 bool force_single_lane)
{
  /* If there's no budget left bail out early.  */
  if (*limit == 0)
    return false;

  if (kind == slp_inst_kind_ctor)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Analyzing vectorizable constructor: %G\n",
			 root_stmt_infos[0]->stmt);
    }
  else if (kind == slp_inst_kind_gcond)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Analyzing vectorizable control flow: %G",
			 root_stmt_infos[0]->stmt);
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "Starting SLP discovery for\n");
      for (unsigned i = 0; i < scalar_stmts.length (); ++i)
	dump_printf_loc (MSG_NOTE, vect_location,
			 "  %G", scalar_stmts[i]->stmt);
    }

  /* Build the tree for the SLP instance.  */
  unsigned int group_size = scalar_stmts.length ();
  bool *matches = XALLOCAVEC (bool, group_size);
  poly_uint64 max_nunits = 1;
  unsigned tree_size = 0;
  unsigned i;

  slp_tree node = NULL;
  if (group_size > 1 && force_single_lane)
    {
      matches[0] = true;
      matches[1] = false;
    }
  else
    node = vect_build_slp_tree (vinfo, scalar_stmts, group_size,
				&max_nunits, matches, limit,
				&tree_size, bst_map);
  if (node != NULL)
    {
      /* Calculate the unrolling factor based on the smallest type.  */
      poly_uint64 unrolling_factor
	= calculate_unrolling_factor (max_nunits, group_size);

      if (maybe_ne (unrolling_factor, 1U)
	  && is_a <bb_vec_info> (vinfo))
	{
	  unsigned HOST_WIDE_INT const_max_nunits;
	  if (!max_nunits.is_constant (&const_max_nunits)
	      || const_max_nunits > group_size)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: store group "
				 "size not a multiple of the vector size "
				 "in basic block SLP\n");
	      vect_free_slp_tree (node);
	      return false;
	    }
	  /* Fatal mismatch.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "SLP discovery succeeded but node needs "
			     "splitting\n");
	  memset (matches, true, group_size);
	  matches[group_size / const_max_nunits * const_max_nunits] = false;
	  vect_free_slp_tree (node);
	}
      else
	{
	  /* Create a new SLP instance.  */
	  slp_instance new_instance = XNEW (class _slp_instance);
	  SLP_INSTANCE_TREE (new_instance) = node;
	  SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = unrolling_factor;
	  SLP_INSTANCE_LOADS (new_instance) = vNULL;
	  SLP_INSTANCE_ROOT_STMTS (new_instance) = root_stmt_infos;
	  SLP_INSTANCE_REMAIN_DEFS (new_instance) = remain;
	  SLP_INSTANCE_KIND (new_instance) = kind;
	  new_instance->reduc_phis = NULL;
	  new_instance->cost_vec = vNULL;
	  new_instance->subgraph_entries = vNULL;

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "SLP size %u vs. limit %u.\n",
			     tree_size, max_tree_size);

	  /* Fixup SLP reduction chains.  */
	  if (kind == slp_inst_kind_reduc_chain)
	    {
	      /* If this is a reduction chain with a conversion in front
		 amend the SLP tree with a node for that.  */
	      gimple *scalar_def
		= vect_orig_stmt (scalar_stmts[group_size - 1])->stmt;
	      if (STMT_VINFO_DEF_TYPE (scalar_stmts[0]) != vect_reduction_def)
		{
		  /* Get at the conversion stmt - we know it's the single use
		     of the last stmt of the reduction chain.  */
		  use_operand_p use_p;
		  bool r = single_imm_use (gimple_assign_lhs (scalar_def),
					   &use_p, &scalar_def);
		  gcc_assert (r);
		  stmt_vec_info next_info = vinfo->lookup_stmt (scalar_def);
		  next_info = vect_stmt_to_vectorize (next_info);
		  scalar_stmts = vNULL;
		  scalar_stmts.create (group_size);
		  for (unsigned i = 0; i < group_size; ++i)
		    scalar_stmts.quick_push (next_info);
		  slp_tree conv = vect_create_new_slp_node (scalar_stmts, 1);
		  SLP_TREE_VECTYPE (conv) = STMT_VINFO_VECTYPE (next_info);
		  SLP_TREE_CHILDREN (conv).quick_push (node);
		  SLP_INSTANCE_TREE (new_instance) = conv;
		  /* We also have to fake this conversion stmt as SLP reduction
		     group so we don't have to mess with too much code
		     elsewhere.  */
		  REDUC_GROUP_FIRST_ELEMENT (next_info) = next_info;
		  REDUC_GROUP_NEXT_ELEMENT (next_info) = NULL;
		}
	      /* Fill the backedge child of the PHI SLP node.  The
		 general matching code cannot find it because the
		 scalar code does not reflect how we vectorize the
		 reduction.  */
	      use_operand_p use_p;
	      imm_use_iterator imm_iter;
	      class loop *loop = LOOP_VINFO_LOOP (as_a <loop_vec_info> (vinfo));
	      FOR_EACH_IMM_USE_FAST (use_p, imm_iter,
				     gimple_get_lhs (scalar_def))
		/* There are exactly two non-debug uses, the reduction
		   PHI and the loop-closed PHI node.  */
		if (!is_gimple_debug (USE_STMT (use_p))
		    && gimple_bb (USE_STMT (use_p)) == loop->header)
		  {
		    auto_vec<stmt_vec_info, 64> phis (group_size);
		    stmt_vec_info phi_info
		      = vinfo->lookup_stmt (USE_STMT (use_p));
		    for (unsigned i = 0; i < group_size; ++i)
		      phis.quick_push (phi_info);
		    slp_tree *phi_node = bst_map->get (phis);
		    unsigned dest_idx = loop_latch_edge (loop)->dest_idx;
		    SLP_TREE_CHILDREN (*phi_node)[dest_idx]
		      = SLP_INSTANCE_TREE (new_instance);
		    SLP_INSTANCE_TREE (new_instance)->refcnt++;
		  }
	    }

	  vinfo->slp_instances.safe_push (new_instance);

	  /* ???  We've replaced the old SLP_INSTANCE_GROUP_SIZE with
	     the number of scalar stmts in the root in a few places.
	     Verify that assumption holds.  */
	  gcc_assert (SLP_TREE_SCALAR_STMTS (SLP_INSTANCE_TREE (new_instance))
			.length () == group_size);

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Final SLP tree for instance %p:\n",
			       (void *) new_instance);
	      vect_print_slp_graph (MSG_NOTE, vect_location,
				    SLP_INSTANCE_TREE (new_instance));
	    }

	  return true;
	}
    }
  /* Failed to SLP.  */

  stmt_vec_info stmt_info = stmt_info_;
  /* Try to break the group up into pieces.  */
  if (*limit > 0 && kind == slp_inst_kind_store)
    {
      /* ???  We could delay all the actual splitting of store-groups
	 until after SLP discovery of the original group completed.
	 Then we can recurse to vect_build_slp_instance directly.  */
      for (i = 0; i < group_size; i++)
	if (!matches[i])
	  break;

      /* For basic block SLP, try to break the group up into multiples of
	 a vector size.  */
      if (is_a <bb_vec_info> (vinfo)
	  && (i > 1 && i < group_size))
	{
	  /* Free the allocated memory.  */
	  scalar_stmts.release ();

	  tree scalar_type
	    = TREE_TYPE (DR_REF (STMT_VINFO_DATA_REF (stmt_info)));
	  tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type,
						      1 << floor_log2 (i));
	  unsigned HOST_WIDE_INT const_nunits;
	  if (vectype
	      && TYPE_VECTOR_SUBPARTS (vectype).is_constant (&const_nunits))
	    {
	      /* Split into two groups at the first vector boundary.  */
	      gcc_assert ((const_nunits & (const_nunits - 1)) == 0);
	      unsigned group1_size = i & ~(const_nunits - 1);

	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Splitting SLP group at stmt %u\n", i);
	      stmt_vec_info rest = vect_split_slp_store_group (stmt_info,
							       group1_size);
	      bool res = vect_analyze_slp_instance (vinfo, bst_map, stmt_info,
						    kind, max_tree_size,
						    limit, false);
	      /* Split the rest at the failure point and possibly
		 re-analyze the remaining matching part if it has
		 at least two lanes.  */
	      if (group1_size < i
		  && (i + 1 < group_size
		      || i - group1_size > 1))
		{
		  stmt_vec_info rest2 = rest;
		  rest = vect_split_slp_store_group (rest, i - group1_size);
		  if (i - group1_size > 1)
		    res |= vect_analyze_slp_instance (vinfo, bst_map, rest2,
						      kind, max_tree_size,
						      limit, false);
		}
	      /* Re-analyze the non-matching tail if it has at least
		 two lanes.  */
	      if (i + 1 < group_size)
		res |= vect_analyze_slp_instance (vinfo, bst_map,
						  rest, kind, max_tree_size,
						  limit, false);
	      return res;
	    }
	}

      /* For loop vectorization split the RHS into arbitrary pieces of
	 size >= 1.  */
      else if (is_a <loop_vec_info> (vinfo)
	       && (group_size != 1 && i < group_size))
	{
	  gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
	  bool masked_p = call
	      && gimple_call_internal_p (call)
	      && internal_fn_mask_index (gimple_call_internal_fn (call)) != -1;
	  /* There are targets that cannot do even/odd interleaving schemes
	     so they absolutely need to use load/store-lanes.  For now
	     force single-lane SLP for them - they would be happy with
	     uniform power-of-two lanes (but depending on element size),
	     but even if we can use 'i' as indicator we would need to
	     backtrack when later lanes fail to discover with the same
	     granularity.  We cannot turn any of strided or scatter store
	     into store-lanes.  */
	  /* ???  If this is not in sync with what get_load_store_type
	     later decides the SLP representation is not good for other
	     store vectorization methods.  */
	  bool want_store_lanes
	    = (! STMT_VINFO_GATHER_SCATTER_P (stmt_info)
	       && ! STMT_VINFO_STRIDED_P (stmt_info)
	       && ! STMT_VINFO_SLP_VECT_ONLY (stmt_info)
	       && compare_step_with_zero (vinfo, stmt_info) > 0
	       && vect_slp_prefer_store_lanes_p (vinfo, stmt_info, NULL_TREE,
						 masked_p, group_size, 1));
	  if (want_store_lanes || force_single_lane)
	    i = 1;

	  /* A fatal discovery fail doesn't always mean single-lane SLP
	     isn't a possibility, so try.  */
	  if (i == 0)
	    i = 1;

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Splitting SLP group at stmt %u\n", i);

	  /* Analyze the stored values and pinch them together with
	     a permute node so we can preserve the whole store group.  */
	  auto_vec<slp_tree> rhs_nodes;

	  /* Calculate the unrolling factor based on the smallest type.  */
	  poly_uint64 unrolling_factor = 1;

	  unsigned int rhs_common_nlanes = 0;
	  unsigned int start = 0, end = i;
	  while (start < group_size)
	    {
	      gcc_assert (end - start >= 1);
	      vec<stmt_vec_info> substmts;
	      substmts.create (end - start);
	      for (unsigned j = start; j < end; ++j)
		substmts.quick_push (scalar_stmts[j]);
	      max_nunits = 1;
	      node = vect_build_slp_tree (vinfo, substmts, end - start,
					  &max_nunits,
					  matches, limit, &tree_size, bst_map);
	      if (node)
		{
		  /* ???  Possibly not safe, but not sure how to check
		     and fail SLP build?  */
		  unrolling_factor
		    = force_common_multiple (unrolling_factor,
					     calculate_unrolling_factor
					       (max_nunits, end - start));
		  rhs_nodes.safe_push (node);
		  if (start == 0)
		    rhs_common_nlanes = SLP_TREE_LANES (node);
		  else if (rhs_common_nlanes != SLP_TREE_LANES (node))
		    rhs_common_nlanes = 0;
		  start = end;
		  if (want_store_lanes || force_single_lane)
		    end = start + 1;
		  else
		    end = group_size;
		}
	      else
		{
		  substmts.release ();
		  if (end - start == 1)
		    {
		      /* Single-lane discovery failed.  Free ressources.  */
		      for (auto node : rhs_nodes)
			vect_free_slp_tree (node);
		      scalar_stmts.release ();
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_NOTE, vect_location,
					 "SLP discovery failed\n");
		      return false;
		    }

		  /* ???  It really happens that we soft-fail SLP
		     build at a mismatch but the matching part hard-fails
		     later.  As we know we arrived here with a group
		     larger than one try a group of size one!  */
		  if (!matches[0])
		    end = start + 1;
		  else
		    for (unsigned j = start; j < end; j++)
		      if (!matches[j - start])
			{
			  end = j;
			  break;
			}
		}
	    }

	  /* Now re-assess whether we want store lanes in case the
	     discovery ended up producing all single-lane RHSs.  */
	  if (! want_store_lanes
	      && rhs_common_nlanes == 1
	      && ! STMT_VINFO_GATHER_SCATTER_P (stmt_info)
	      && ! STMT_VINFO_STRIDED_P (stmt_info)
	      && ! STMT_VINFO_SLP_VECT_ONLY (stmt_info)
	      && compare_step_with_zero (vinfo, stmt_info) > 0
	      && (vect_store_lanes_supported (SLP_TREE_VECTYPE (rhs_nodes[0]),
					      group_size, masked_p)
		  != IFN_LAST))
	    want_store_lanes = true;

	  /* Now we assume we can build the root SLP node from all stores.  */
	  if (want_store_lanes)
	    {
	      /* For store-lanes feed the store node with all RHS nodes
		 in order.  */
	      node = vect_create_new_slp_node (scalar_stmts,
					       SLP_TREE_CHILDREN
						 (rhs_nodes[0]).length ());
	      SLP_TREE_VECTYPE (node) = SLP_TREE_VECTYPE (rhs_nodes[0]);
	      node->ldst_lanes = true;
	      SLP_TREE_CHILDREN (node)
		.reserve_exact (SLP_TREE_CHILDREN (rhs_nodes[0]).length ()
				+ rhs_nodes.length () - 1);
	      /* First store value and possibly mask.  */
	      SLP_TREE_CHILDREN (node)
		.splice (SLP_TREE_CHILDREN (rhs_nodes[0]));
	      /* Rest of the store values.  All mask nodes are the same,
		 this should be guaranteed by dataref group discovery.  */
	      for (unsigned j = 1; j < rhs_nodes.length (); ++j)
		SLP_TREE_CHILDREN (node)
		  .quick_push (SLP_TREE_CHILDREN (rhs_nodes[j])[0]);
	      for (slp_tree child : SLP_TREE_CHILDREN (node))
		child->refcnt++;
	    }
	  else
	    node = vect_build_slp_store_interleaving (rhs_nodes, scalar_stmts);

	  while (!rhs_nodes.is_empty ())
	    vect_free_slp_tree (rhs_nodes.pop ());

	  /* Create a new SLP instance.  */
	  slp_instance new_instance = XNEW (class _slp_instance);
	  SLP_INSTANCE_TREE (new_instance) = node;
	  SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = unrolling_factor;
	  SLP_INSTANCE_LOADS (new_instance) = vNULL;
	  SLP_INSTANCE_ROOT_STMTS (new_instance) = root_stmt_infos;
	  SLP_INSTANCE_REMAIN_DEFS (new_instance) = remain;
	  SLP_INSTANCE_KIND (new_instance) = kind;
	  new_instance->reduc_phis = NULL;
	  new_instance->cost_vec = vNULL;
	  new_instance->subgraph_entries = vNULL;

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "SLP size %u vs. limit %u.\n",
			     tree_size, max_tree_size);

	  vinfo->slp_instances.safe_push (new_instance);

	  /* ???  We've replaced the old SLP_INSTANCE_GROUP_SIZE with
	     the number of scalar stmts in the root in a few places.
	     Verify that assumption holds.  */
	  gcc_assert (SLP_TREE_SCALAR_STMTS (SLP_INSTANCE_TREE (new_instance))
			.length () == group_size);

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Final SLP tree for instance %p:\n",
			       (void *) new_instance);
	      vect_print_slp_graph (MSG_NOTE, vect_location,
				    SLP_INSTANCE_TREE (new_instance));
	    }
	  return true;
	}
      else
	/* Free the allocated memory.  */
	scalar_stmts.release ();

      /* Even though the first vector did not all match, we might be able to SLP
	 (some) of the remainder.  FORNOW ignore this possibility.  */
    }
  else
    /* Free the allocated memory.  */
    scalar_stmts.release ();

  /* Failed to SLP.  */
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "SLP discovery failed\n");
  return false;
}


/* Analyze an SLP instance starting from a group of grouped stores.  Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (vec_info *vinfo,
			   scalar_stmts_to_slp_tree_map_t *bst_map,
			   stmt_vec_info stmt_info,
			   slp_instance_kind kind,
			   unsigned max_tree_size, unsigned *limit,
			   bool force_single_lane)
{
  vec<stmt_vec_info> scalar_stmts;

  if (is_a <bb_vec_info> (vinfo))
    vect_location = stmt_info->stmt;

  stmt_vec_info next_info = stmt_info;
  if (kind == slp_inst_kind_store)
    {
      /* Collect the stores and store them in scalar_stmts.  */
      scalar_stmts.create (DR_GROUP_SIZE (stmt_info));
      while (next_info)
	{
	  scalar_stmts.quick_push (vect_stmt_to_vectorize (next_info));
	  next_info = DR_GROUP_NEXT_ELEMENT (next_info);
	}
    }
  else if (kind == slp_inst_kind_reduc_chain)
    {
      /* Collect the reduction stmts and store them in scalar_stmts.  */
      scalar_stmts.create (REDUC_GROUP_SIZE (stmt_info));
      while (next_info)
	{
	  scalar_stmts.quick_push (vect_stmt_to_vectorize (next_info));
	  next_info = REDUC_GROUP_NEXT_ELEMENT (next_info);
	}
      /* Mark the first element of the reduction chain as reduction to properly
	 transform the node.  In the reduction analysis phase only the last
	 element of the chain is marked as reduction.  */
      STMT_VINFO_DEF_TYPE (stmt_info)
	= STMT_VINFO_DEF_TYPE (scalar_stmts.last ());
      STMT_VINFO_REDUC_DEF (vect_orig_stmt (stmt_info))
	= STMT_VINFO_REDUC_DEF (vect_orig_stmt (scalar_stmts.last ()));
    }
  else
    gcc_unreachable ();

  vec<stmt_vec_info> roots = vNULL;
  vec<tree> remain = vNULL;
  /* Build the tree for the SLP instance.  */
  bool res = vect_build_slp_instance (vinfo, kind, scalar_stmts,
				      roots, remain,
				      max_tree_size, limit, bst_map,
				      kind == slp_inst_kind_store
				      ? stmt_info : NULL, force_single_lane);

  /* ???  If this is slp_inst_kind_store and the above succeeded here's
     where we should do store group splitting.  */

  return res;
}

/* qsort comparator ordering SLP load nodes.  */

static int
vllp_cmp (const void *a_, const void *b_)
{
  const slp_tree a = *(const slp_tree *)a_;
  const slp_tree b = *(const slp_tree *)b_;
  stmt_vec_info a0 = SLP_TREE_SCALAR_STMTS (a)[0];
  stmt_vec_info b0 = SLP_TREE_SCALAR_STMTS (b)[0];
  if (STMT_VINFO_GROUPED_ACCESS (a0)
      && STMT_VINFO_GROUPED_ACCESS (b0)
      && DR_GROUP_FIRST_ELEMENT (a0) == DR_GROUP_FIRST_ELEMENT (b0))
    {
      /* Same group, order after lanes used.  */
      if (SLP_TREE_LANES (a) < SLP_TREE_LANES (b))
	return 1;
      else if (SLP_TREE_LANES (a) > SLP_TREE_LANES (b))
	return -1;
      else
	{
	  /* Try to order loads using the same lanes together, breaking
	     the tie with the lane number that first differs.  */
	  if (!SLP_TREE_LOAD_PERMUTATION (a).exists ()
	      && !SLP_TREE_LOAD_PERMUTATION (b).exists ())
	    return 0;
	  else if (SLP_TREE_LOAD_PERMUTATION (a).exists ()
		   && !SLP_TREE_LOAD_PERMUTATION (b).exists ())
	    return 1;
	  else if (!SLP_TREE_LOAD_PERMUTATION (a).exists ()
		   && SLP_TREE_LOAD_PERMUTATION (b).exists ())
	    return -1;
	  else
	    {
	      for (unsigned i = 0; i < SLP_TREE_LANES (a); ++i)
		if (SLP_TREE_LOAD_PERMUTATION (a)[i]
		    != SLP_TREE_LOAD_PERMUTATION (b)[i])
		  {
		    /* In-order lane first, that's what the above case for
		       no permutation does.  */
		    if (SLP_TREE_LOAD_PERMUTATION (a)[i] == i)
		      return -1;
		    else if (SLP_TREE_LOAD_PERMUTATION (b)[i] == i)
		      return 1;
		    else if (SLP_TREE_LOAD_PERMUTATION (a)[i]
			     < SLP_TREE_LOAD_PERMUTATION (b)[i])
		      return -1;
		    else
		      return 1;
		  }
	      return 0;
	    }
	}
    }
  else /* Different groups or non-groups.  */
    {
      /* Order groups as their first element to keep them together.  */
      if (STMT_VINFO_GROUPED_ACCESS (a0))
	a0 = DR_GROUP_FIRST_ELEMENT (a0);
      if (STMT_VINFO_GROUPED_ACCESS (b0))
	b0 = DR_GROUP_FIRST_ELEMENT (b0);
      if (a0 == b0)
	return 0;
      /* Tie using UID.  */
      else if (gimple_uid (STMT_VINFO_STMT (a0))
	       < gimple_uid (STMT_VINFO_STMT (b0)))
	return -1;
      else
	{
	  gcc_assert (gimple_uid (STMT_VINFO_STMT (a0))
		      != gimple_uid (STMT_VINFO_STMT (b0)));
	  return 1;
	}
    }
}

/* Process the set of LOADS that are all from the same dataref group.  */

static void
vect_lower_load_permutations (loop_vec_info loop_vinfo,
			      scalar_stmts_to_slp_tree_map_t *bst_map,
			      const array_slice<slp_tree> &loads)
{
  /* We at this point want to lower without a fixed VF or vector
     size in mind which means we cannot actually compute whether we
     need three or more vectors for a load permutation yet.  So always
     lower.  */
  stmt_vec_info first
    = DR_GROUP_FIRST_ELEMENT (SLP_TREE_SCALAR_STMTS (loads[0])[0]);
  unsigned group_lanes = DR_GROUP_SIZE (first);

  /* Verify if all load permutations can be implemented with a suitably
     large element load-lanes operation.  */
  unsigned ld_lanes_lanes = SLP_TREE_LANES (loads[0]);
  if (STMT_VINFO_STRIDED_P (first)
      || compare_step_with_zero (loop_vinfo, first) <= 0
      || exact_log2 (ld_lanes_lanes) == -1
      /* ???  For now only support the single-lane case as there is
	 missing support on the store-lane side and code generation
	 isn't up to the task yet.  */
      || ld_lanes_lanes != 1
      || vect_load_lanes_supported (SLP_TREE_VECTYPE (loads[0]),
				    group_lanes / ld_lanes_lanes,
				    false) == IFN_LAST)
    ld_lanes_lanes = 0;
  else
    /* Verify the loads access the same number of lanes aligned to
       ld_lanes_lanes.  */
    for (slp_tree load : loads)
      {
	if (SLP_TREE_LANES (load) != ld_lanes_lanes)
	  {
	    ld_lanes_lanes = 0;
	    break;
	  }
	unsigned first = SLP_TREE_LOAD_PERMUTATION (load)[0];
	if (first % ld_lanes_lanes != 0)
	  {
	    ld_lanes_lanes = 0;
	    break;
	  }
	for (unsigned i = 1; i < SLP_TREE_LANES (load); ++i)
	  if (SLP_TREE_LOAD_PERMUTATION (load)[i] != first + i)
	    {
	      ld_lanes_lanes = 0;
	      break;
	    }
      }

  /* Only a power-of-two number of lanes matches interleaving with N levels.
     ???  An even number of lanes could be reduced to 1<<ceil_log2(N)-1 lanes
     at each step.  */
  if (ld_lanes_lanes == 0 && exact_log2 (group_lanes) == -1 && group_lanes != 3)
    return;

  for (slp_tree load : loads)
    {
      /* Leave masked or gather loads alone for now.  */
      if (!SLP_TREE_CHILDREN (load).is_empty ())
	continue;

      /* We want to pattern-match special cases here and keep those
	 alone.  Candidates are splats and load-lane.  */

      /* We need to lower only loads of less than half of the groups
	 lanes, including duplicate lanes.  Note this leaves nodes
	 with a non-1:1 load permutation around instead of canonicalizing
	 those into a load and a permute node.  Removing this early
	 check would do such canonicalization.  */
      if (SLP_TREE_LANES (load) >= (group_lanes + 1) / 2
	  && ld_lanes_lanes == 0)
	continue;

      /* Build the permute to get the original load permutation order.  */
      bool contiguous = true;
      lane_permutation_t final_perm;
      final_perm.create (SLP_TREE_LANES (load));
      for (unsigned i = 0; i < SLP_TREE_LANES (load); ++i)
	{
	  final_perm.quick_push
	    (std::make_pair (0, SLP_TREE_LOAD_PERMUTATION (load)[i]));
	  if (i != 0
	      && (SLP_TREE_LOAD_PERMUTATION (load)[i]
		  != SLP_TREE_LOAD_PERMUTATION (load)[i-1] + 1))
	    contiguous = false;
	}

      /* When the load permutation accesses a contiguous unpermuted,
	 power-of-two aligned and sized chunk leave the load alone.
	 We can likely (re-)load it more efficiently rather than
	 extracting it from the larger load.
	 ???  Long-term some of the lowering should move to where
	 the vector types involved are fixed.  */
      if (ld_lanes_lanes == 0
	  && contiguous
	  && (SLP_TREE_LANES (load) > 1 || loads.size () == 1)
	  && pow2p_hwi (SLP_TREE_LANES (load))
	  && SLP_TREE_LOAD_PERMUTATION (load)[0] % SLP_TREE_LANES (load) == 0
	  && group_lanes % SLP_TREE_LANES (load) == 0)
	{
	  final_perm.release ();
	  continue;
	}

      /* First build (and possibly re-use) a load node for the
	 unpermuted group.  Gaps in the middle and on the end are
	 represented with NULL stmts.  */
      vec<stmt_vec_info> stmts;
      stmts.create (group_lanes);
      for (stmt_vec_info s = first; s; s = DR_GROUP_NEXT_ELEMENT (s))
	{
	  if (s != first)
	    for (unsigned i = 1; i < DR_GROUP_GAP (s); ++i)
	      stmts.quick_push (NULL);
	  stmts.quick_push (s);
	}
      for (unsigned i = 0; i < DR_GROUP_GAP (first); ++i)
	stmts.quick_push (NULL);
      poly_uint64 max_nunits = 1;
      bool *matches = XALLOCAVEC (bool, group_lanes);
      unsigned limit = 1;
      unsigned tree_size = 0;
      slp_tree l0 = vect_build_slp_tree (loop_vinfo, stmts,
					 group_lanes,
					 &max_nunits, matches, &limit,
					 &tree_size, bst_map);

      if (ld_lanes_lanes != 0)
	{
	  /* ???  If this is not in sync with what get_load_store_type
	     later decides the SLP representation is not good for other
	     store vectorization methods.  */
	  l0->ldst_lanes = true;
	  load->ldst_lanes = true;
	}

      while (1)
	{
	  unsigned group_lanes = SLP_TREE_LANES (l0);
	  if (ld_lanes_lanes != 0
	      || SLP_TREE_LANES (load) >= (group_lanes + 1) / 2)
	    break;

	  /* Try to lower by reducing the group to half its size using an
	     interleaving scheme.  For this try to compute whether all
	     elements needed for this load are in even or odd elements of
	     an even/odd decomposition with N consecutive elements.
	     Thus { e, e, o, o, e, e, o, o } woud be an even/odd decomposition
	     with N == 2.  */
	  /* ???  Only an even number of lanes can be handed this way, but the
	     fallback below could work for any number.  We have to make sure
	     to round up in that case.  */
	  gcc_assert ((group_lanes & 1) == 0 || group_lanes == 3);
	  unsigned even = 0, odd = 0;
	  if ((group_lanes & 1) == 0)
	    {
	      even = (1 << ceil_log2 (group_lanes)) - 1;
	      odd = even;
	      for (auto l : final_perm)
		{
		  even &= ~l.second;
		  odd &= l.second;
		}
	    }

	  /* Now build an even or odd extraction from the unpermuted load.  */
	  lane_permutation_t perm;
	  perm.create ((group_lanes + 1) / 2);
	  unsigned even_level = even ? 1 << ctz_hwi (even) : 0;
	  unsigned odd_level = odd ? 1 << ctz_hwi (odd) : 0;
	  if (even_level
	      && group_lanes % (2 * even_level) == 0
	      /* ???  When code generating permutes we do not try to pun
		 to larger component modes so level != 1 isn't a natural
		 even/odd extract.  Prefer one if possible.  */
	      && (even_level == 1 || !odd_level || odd_level != 1))
	    {
	      /* { 0, 1, ... 4, 5 ..., } */
	      for (unsigned i = 0; i < group_lanes / 2 / even_level; ++i)
		for (unsigned j = 0; j < even_level; ++j)
		  perm.quick_push (std::make_pair (0, 2 * i * even_level + j));
	    }
	  else if (odd_level)
	    {
	      /* { ..., 2, 3, ... 6, 7 } */
	      gcc_assert (group_lanes % (2 * odd_level) == 0);
	      for (unsigned i = 0; i < group_lanes / 2 / odd_level; ++i)
		for (unsigned j = 0; j < odd_level; ++j)
		  perm.quick_push
		    (std::make_pair (0, (2 * i + 1) * odd_level + j));
	    }
	  else
	    {
	      /* As fallback extract all used lanes and fill to half the
		 group size by repeating the last element.
		 ???  This is quite a bad strathegy for re-use - we could
		 brute force our way to find more optimal filling lanes to
		 maximize re-use when looking at all loads from the group.  */
	      auto_bitmap l;
	      for (auto p : final_perm)
		bitmap_set_bit (l, p.second);
	      unsigned i = 0;
	      bitmap_iterator bi;
	      EXECUTE_IF_SET_IN_BITMAP (l, 0, i, bi)
		  perm.quick_push (std::make_pair (0, i));
	      while (perm.length () < (group_lanes + 1) / 2)
		perm.quick_push (perm.last ());
	    }

	  /* Update final_perm with the intermediate permute.  */
	  for (unsigned i = 0; i < final_perm.length (); ++i)
	    {
	      unsigned l = final_perm[i].second;
	      unsigned j;
	      for (j = 0; j < perm.length (); ++j)
		if (perm[j].second == l)
		  {
		    final_perm[i].second = j;
		    break;
		  }
	      gcc_assert (j < perm.length ());
	    }

	  /* And create scalar stmts.  */
	  vec<stmt_vec_info> perm_stmts;
	  perm_stmts.create (perm.length ());
	  for (unsigned i = 0; i < perm.length (); ++i)
	    perm_stmts.quick_push (SLP_TREE_SCALAR_STMTS (l0)[perm[i].second]);

	  slp_tree p = vect_create_new_slp_node (1, VEC_PERM_EXPR);
	  SLP_TREE_CHILDREN (p).quick_push (l0);
	  SLP_TREE_LANE_PERMUTATION (p) = perm;
	  SLP_TREE_VECTYPE (p) = SLP_TREE_VECTYPE (load);
	  SLP_TREE_LANES (p) = perm.length ();
	  SLP_TREE_REPRESENTATIVE (p) = SLP_TREE_REPRESENTATIVE (load);
	  /* ???  As we have scalar stmts for this intermediate permute we
	     could CSE it via bst_map but we do not want to pick up
	     another SLP node with a load permutation.  We instead should
	     have a "local" CSE map here.  */
	  SLP_TREE_SCALAR_STMTS (p) = perm_stmts;

	  /* We now have a node for (group_lanes + 1) / 2 lanes.  */
	  l0 = p;
	}

      /* And finally from the ordered reduction node create the
	 permute to shuffle the lanes into the original load-permutation
	 order.  We replace the original load node with this.  */
      SLP_TREE_CODE (load) = VEC_PERM_EXPR;
      SLP_TREE_LOAD_PERMUTATION (load).release ();
      SLP_TREE_LANE_PERMUTATION (load) = final_perm;
      SLP_TREE_CHILDREN (load).create (1);
      SLP_TREE_CHILDREN (load).quick_push (l0);
    }
}

/* Transform SLP loads in the SLP graph created by SLP discovery to
   group loads from the same group and lower load permutations that
   are unlikely to be supported into a series of permutes.
   In the degenerate case of having only single-lane SLP instances
   this should result in a series of permute nodes emulating an
   interleaving scheme.  */

static void
vect_lower_load_permutations (loop_vec_info loop_vinfo,
			      scalar_stmts_to_slp_tree_map_t *bst_map)
{
  /* Gather and sort loads across all instances.  */
  hash_set<slp_tree> visited;
  auto_vec<slp_tree> loads;
  for (auto inst : loop_vinfo->slp_instances)
    vect_gather_slp_loads (loads, SLP_INSTANCE_TREE (inst), visited);
  if (loads.is_empty ())
    return;
  loads.qsort (vllp_cmp);

  /* Now process each dataref group separately.  */
  unsigned firsti = 0;
  for (unsigned i = 1; i < loads.length (); ++i)
    {
      slp_tree first = loads[firsti];
      slp_tree next = loads[i];
      stmt_vec_info a0 = SLP_TREE_SCALAR_STMTS (first)[0];
      stmt_vec_info b0 = SLP_TREE_SCALAR_STMTS (next)[0];
      if (STMT_VINFO_GROUPED_ACCESS (a0)
	  && STMT_VINFO_GROUPED_ACCESS (b0)
	  && DR_GROUP_FIRST_ELEMENT (a0) == DR_GROUP_FIRST_ELEMENT (b0))
	continue;
      /* Now we have one or multiple SLP loads of the same group from
	 firsti to i - 1.  */
      if (STMT_VINFO_GROUPED_ACCESS (a0))
	vect_lower_load_permutations (loop_vinfo, bst_map,
				      make_array_slice (&loads[firsti],
							i - firsti));
      firsti = i;
    }
  if (firsti < loads.length ()
      && STMT_VINFO_GROUPED_ACCESS (SLP_TREE_SCALAR_STMTS (loads[firsti])[0]))
    vect_lower_load_permutations (loop_vinfo, bst_map,
				  make_array_slice (&loads[firsti],
						    loads.length () - firsti));
}

/* Check if there are stmts in the loop can be vectorized using SLP.  Build SLP
   trees of packed scalar stmts if SLP is possible.  */

opt_result
vect_analyze_slp (vec_info *vinfo, unsigned max_tree_size,
		  bool force_single_lane)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  unsigned int i;
  stmt_vec_info first_element;
  slp_instance instance;

  DUMP_VECT_SCOPE ("vect_analyze_slp");

  unsigned limit = max_tree_size;

  scalar_stmts_to_slp_tree_map_t *bst_map
    = new scalar_stmts_to_slp_tree_map_t ();

  /* Find SLP sequences starting from groups of grouped stores.  */
  FOR_EACH_VEC_ELT (vinfo->grouped_stores, i, first_element)
    vect_analyze_slp_instance (vinfo, bst_map, first_element,
			       slp_inst_kind_store, max_tree_size, &limit,
			       force_single_lane);

  /* For loops also start SLP discovery from non-grouped stores.  */
  if (loop_vinfo)
    {
      data_reference_p dr;
      FOR_EACH_VEC_ELT (vinfo->shared->datarefs, i, dr)
	if (DR_IS_WRITE (dr))
	  {
	    stmt_vec_info stmt_info = vinfo->lookup_dr (dr)->stmt;
	    /* Grouped stores are already handled above.  */
	    if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
	      continue;
	    vec<stmt_vec_info> stmts;
	    vec<stmt_vec_info> roots = vNULL;
	    vec<tree> remain = vNULL;
	    stmts.create (1);
	    stmts.quick_push (stmt_info);
	    vect_build_slp_instance (vinfo, slp_inst_kind_store,
				     stmts, roots, remain, max_tree_size,
				     &limit, bst_map, NULL, force_single_lane);
	  }
    }

  if (bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo))
    {
      for (unsigned i = 0; i < bb_vinfo->roots.length (); ++i)
	{
	  vect_location = bb_vinfo->roots[i].roots[0]->stmt;
	  /* Apply patterns.  */
	  for (unsigned j = 0; j < bb_vinfo->roots[i].stmts.length (); ++j)
	    bb_vinfo->roots[i].stmts[j]
	      = vect_stmt_to_vectorize (bb_vinfo->roots[i].stmts[j]);
	  if (vect_build_slp_instance (bb_vinfo, bb_vinfo->roots[i].kind,
				       bb_vinfo->roots[i].stmts,
				       bb_vinfo->roots[i].roots,
				       bb_vinfo->roots[i].remain,
				       max_tree_size, &limit, bst_map, NULL,
				       false))
	    {
	      bb_vinfo->roots[i].stmts = vNULL;
	      bb_vinfo->roots[i].roots = vNULL;
	      bb_vinfo->roots[i].remain = vNULL;
	    }
	}
    }

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      /* Find SLP sequences starting from reduction chains.  */
      FOR_EACH_VEC_ELT (loop_vinfo->reduction_chains, i, first_element)
	if (! STMT_VINFO_RELEVANT_P (first_element)
	    && ! STMT_VINFO_LIVE_P (first_element))
	  ;
	else if (force_single_lane
		 || ! vect_analyze_slp_instance (vinfo, bst_map, first_element,
						 slp_inst_kind_reduc_chain,
						 max_tree_size, &limit,
						 force_single_lane))
	  {
	    /* Dissolve reduction chain group.  */
	    stmt_vec_info vinfo = first_element;
	    stmt_vec_info last = NULL;
	    while (vinfo)
	      {
		stmt_vec_info next = REDUC_GROUP_NEXT_ELEMENT (vinfo);
		REDUC_GROUP_FIRST_ELEMENT (vinfo) = NULL;
		REDUC_GROUP_NEXT_ELEMENT (vinfo) = NULL;
		last = vinfo;
		vinfo = next;
	      }
	    STMT_VINFO_DEF_TYPE (first_element) = vect_internal_def;
	    /* It can be still vectorized as part of an SLP reduction.  */
	    loop_vinfo->reductions.safe_push (last);
	  }

      /* Find SLP sequences starting from groups of reductions.  */
      if (loop_vinfo->reductions.length () > 0)
	{
	  /* Collect reduction statements we can combine into
	     a SLP reduction.  */
	  vec<stmt_vec_info> scalar_stmts;
	  scalar_stmts.create (loop_vinfo->reductions.length ());
	  for (auto next_info : loop_vinfo->reductions)
	    {
	      next_info = vect_stmt_to_vectorize (next_info);
	      if ((STMT_VINFO_RELEVANT_P (next_info)
		   || STMT_VINFO_LIVE_P (next_info))
		  /* ???  Make sure we didn't skip a conversion around a
		     reduction path.  In that case we'd have to reverse
		     engineer that conversion stmt following the chain using
		     reduc_idx and from the PHI using reduc_def.  */
		  && (STMT_VINFO_DEF_TYPE (next_info) == vect_reduction_def
		      || (STMT_VINFO_DEF_TYPE (next_info)
			  == vect_double_reduction_def)))
		{
		  /* Do not discover SLP reductions combining lane-reducing
		     ops, that will fail later.  */
		  if (!force_single_lane
		      && !lane_reducing_stmt_p (STMT_VINFO_STMT (next_info)))
		    scalar_stmts.quick_push (next_info);
		  else
		    {
		      /* Do SLP discovery for single-lane reductions.  */
		      vec<stmt_vec_info> stmts;
		      vec<stmt_vec_info> roots = vNULL;
		      vec<tree> remain = vNULL;
		      stmts.create (1);
		      stmts.quick_push (next_info);
		      vect_build_slp_instance (vinfo,
					       slp_inst_kind_reduc_group,
					       stmts, roots, remain,
					       max_tree_size, &limit,
					       bst_map, NULL,
					       force_single_lane);
		    }
		}
	    }
	  /* Save for re-processing on failure.  */
	  vec<stmt_vec_info> saved_stmts = scalar_stmts.copy ();
	  vec<stmt_vec_info> roots = vNULL;
	  vec<tree> remain = vNULL;
	  if (scalar_stmts.length () <= 1
	      || !vect_build_slp_instance (loop_vinfo,
					   slp_inst_kind_reduc_group,
					   scalar_stmts, roots, remain,
					   max_tree_size, &limit, bst_map,
					   NULL, force_single_lane))
	    {
	      if (scalar_stmts.length () <= 1)
		scalar_stmts.release ();
	      /* Do SLP discovery for single-lane reductions.  */
	      for (auto stmt_info : saved_stmts)
		{
		  vec<stmt_vec_info> stmts;
		  vec<stmt_vec_info> roots = vNULL;
		  vec<tree> remain = vNULL;
		  stmts.create (1);
		  stmts.quick_push (vect_stmt_to_vectorize (stmt_info));
		  vect_build_slp_instance (vinfo,
					   slp_inst_kind_reduc_group,
					   stmts, roots, remain,
					   max_tree_size, &limit,
					   bst_map, NULL, force_single_lane);
		}
	      saved_stmts.release ();
	    }
	}

      /* Make sure to vectorize only-live stmts, usually inductions.  */
      for (edge e : get_loop_exit_edges (LOOP_VINFO_LOOP (loop_vinfo)))
	for (auto gsi = gsi_start_phis (e->dest); !gsi_end_p (gsi);
	     gsi_next (&gsi))
	  {
	    gphi *lc_phi = *gsi;
	    tree def = gimple_phi_arg_def_from_edge (lc_phi, e);
	    stmt_vec_info stmt_info;
	    if (TREE_CODE (def) == SSA_NAME
		&& !virtual_operand_p (def)
		&& (stmt_info = loop_vinfo->lookup_def (def))
		&& ((stmt_info = vect_stmt_to_vectorize (stmt_info)), true)
		&& STMT_VINFO_RELEVANT (stmt_info) == vect_used_only_live
		&& STMT_VINFO_LIVE_P (stmt_info)
		&& (STMT_VINFO_DEF_TYPE (stmt_info) == vect_induction_def
		    || (STMT_VINFO_DEF_TYPE (stmt_info) == vect_internal_def
			&& STMT_VINFO_REDUC_IDX (stmt_info) == -1)))
	      {
		vec<stmt_vec_info> stmts;
		vec<stmt_vec_info> roots = vNULL;
		vec<tree> remain = vNULL;
		stmts.create (1);
		stmts.quick_push (vect_stmt_to_vectorize (stmt_info));
		vect_build_slp_instance (vinfo,
					 slp_inst_kind_reduc_group,
					 stmts, roots, remain,
					 max_tree_size, &limit,
					 bst_map, NULL, force_single_lane);
	      }
	  }

      /* Find SLP sequences starting from gconds.  */
      for (auto cond : LOOP_VINFO_LOOP_CONDS (loop_vinfo))
	{
	  auto cond_info = loop_vinfo->lookup_stmt (cond);

	  cond_info = vect_stmt_to_vectorize (cond_info);
	  vec<stmt_vec_info> roots = vNULL;
	  roots.safe_push (cond_info);
	  gimple *stmt = STMT_VINFO_STMT (cond_info);
	  tree args0 = gimple_cond_lhs (stmt);
	  tree args1 = gimple_cond_rhs (stmt);

	  /* These should be enforced by cond lowering.  */
	  gcc_assert (gimple_cond_code (stmt) == NE_EXPR);
	  gcc_assert (zerop (args1));

	  /* An argument without a loop def will be codegened from vectorizing the
	     root gcond itself.  As such we don't need to try to build an SLP tree
	     from them.  It's highly likely that the resulting SLP tree here if both
	     arguments have a def will be incompatible, but we rely on it being split
	     later on.  */
	  if (auto varg = loop_vinfo->lookup_def (args0))
	    {
	      vec<stmt_vec_info> stmts;
	      vec<tree> remain = vNULL;
	      stmts.create (1);
	      stmts.quick_push (vect_stmt_to_vectorize (varg));

	      vect_build_slp_instance (vinfo, slp_inst_kind_gcond,
				       stmts, roots, remain,
				       max_tree_size, &limit,
				       bst_map, NULL, force_single_lane);
	    }
	  else
	    {
	      /* Create a new SLP instance.  */
	      slp_instance new_instance = XNEW (class _slp_instance);
	      vec<tree> ops;
	      ops.create (1);
	      ops.quick_push (args0);
	      slp_tree invnode = vect_create_new_slp_node (ops);
	      SLP_TREE_DEF_TYPE (invnode) = vect_external_def;
	      SLP_INSTANCE_TREE (new_instance) = invnode;
	      SLP_INSTANCE_UNROLLING_FACTOR (new_instance) = 1;
	      SLP_INSTANCE_LOADS (new_instance) = vNULL;
	      SLP_INSTANCE_ROOT_STMTS (new_instance) = roots;
	      SLP_INSTANCE_REMAIN_DEFS (new_instance) = vNULL;
	      SLP_INSTANCE_KIND (new_instance) = slp_inst_kind_gcond;
	      new_instance->reduc_phis = NULL;
	      new_instance->cost_vec = vNULL;
	      new_instance->subgraph_entries = vNULL;
	      vinfo->slp_instances.safe_push (new_instance);
	    }
	}

	/* Find and create slp instances for inductions that have been forced
	   live due to early break.  */
	edge latch_e = loop_latch_edge (LOOP_VINFO_LOOP (loop_vinfo));
	for (auto stmt_info : LOOP_VINFO_EARLY_BREAKS_LIVE_IVS (loop_vinfo))
	  {
	    vec<stmt_vec_info> stmts;
	    vec<stmt_vec_info> roots = vNULL;
	    vec<tree> remain = vNULL;
	    gphi *lc_phi = as_a<gphi *> (STMT_VINFO_STMT (stmt_info));
	    tree def = gimple_phi_arg_def_from_edge (lc_phi, latch_e);
	    stmt_vec_info lc_info = loop_vinfo->lookup_def (def);
	    stmts.create (1);
	    stmts.quick_push (vect_stmt_to_vectorize (lc_info));
	    vect_build_slp_instance (vinfo, slp_inst_kind_reduc_group,
				     stmts, roots, remain,
				     max_tree_size, &limit,
				     bst_map, NULL, force_single_lane);
	  }
    }

  hash_set<slp_tree> visited_patterns;
  slp_tree_to_load_perm_map_t perm_cache;
  slp_compat_nodes_map_t compat_cache;

  /* See if any patterns can be found in the SLP tree.  */
  bool pattern_found = false;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (vinfo), i, instance)
    pattern_found |= vect_match_slp_patterns (instance, vinfo,
					      &visited_patterns, &perm_cache,
					      &compat_cache);

  /* If any were found optimize permutations of loads.  */
  if (pattern_found)
    {
      hash_map<slp_tree, slp_tree> load_map;
      FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (vinfo), i, instance)
	{
	  slp_tree root = SLP_INSTANCE_TREE (instance);
	  optimize_load_redistribution (bst_map, vinfo, SLP_TREE_LANES (root),
					&load_map, root);
	}
    }

  /* Check whether we should force some SLP instances to use load/store-lanes
     and do so by forcing SLP re-discovery with single lanes.  We used
     to cancel SLP when this applied to all instances in a loop but now
     we decide this per SLP instance.  It's important to do this only
     after SLP pattern recognition.  */
  if (is_a <loop_vec_info> (vinfo))
    FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (vinfo), i, instance)
      if (SLP_INSTANCE_KIND (instance) == slp_inst_kind_store
	  && !SLP_INSTANCE_TREE (instance)->ldst_lanes)
	{
	  slp_tree slp_root = SLP_INSTANCE_TREE (instance);
	  int group_size = SLP_TREE_LANES (slp_root);
	  tree vectype = SLP_TREE_VECTYPE (slp_root);

	  stmt_vec_info rep_info = SLP_TREE_REPRESENTATIVE (slp_root);
	  gimple *rep = STMT_VINFO_STMT (rep_info);
	  bool masked = (is_gimple_call (rep)
			 && gimple_call_internal_p (rep)
			 && internal_fn_mask_index
			      (gimple_call_internal_fn (rep)) != -1);
	  if (!STMT_VINFO_GROUPED_ACCESS (rep_info)
	      || slp_root->ldst_lanes
	      || (vect_store_lanes_supported (vectype, group_size, masked)
		  == IFN_LAST))
	    continue;

	  auto_vec<slp_tree> loads;
	  hash_set<slp_tree> visited;
	  vect_gather_slp_loads (loads, slp_root, visited);

	  /* Check whether any load in the SLP instance is possibly
	     permuted.  */
	  bool loads_permuted = false;
	  slp_tree load_node;
	  unsigned j;
	  FOR_EACH_VEC_ELT (loads, j, load_node)
	    {
	      if (!SLP_TREE_LOAD_PERMUTATION (load_node).exists ())
		continue;
	      unsigned k;
	      stmt_vec_info load_info;
	      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (load_node), k, load_info)
		if (SLP_TREE_LOAD_PERMUTATION (load_node)[k] != k)
		  {
		    loads_permuted = true;
		    break;
		  }
	    }

	  /* If the loads and stores can use load/store-lanes force re-discovery
	     with single lanes.  */
	  if (loads_permuted)
	    {
	      bool can_use_lanes = true;
	      FOR_EACH_VEC_ELT (loads, j, load_node)
		if (STMT_VINFO_GROUPED_ACCESS
		      (SLP_TREE_REPRESENTATIVE (load_node)))
		  {
		    stmt_vec_info stmt_vinfo = DR_GROUP_FIRST_ELEMENT
			(SLP_TREE_REPRESENTATIVE (load_node));
		    rep = STMT_VINFO_STMT (stmt_vinfo);
		    masked = (is_gimple_call (rep)
			      && gimple_call_internal_p (rep)
			      && internal_fn_mask_index
				   (gimple_call_internal_fn (rep)));
		    /* Use SLP for strided accesses (or if we can't
		       load-lanes).  */
		    if (STMT_VINFO_STRIDED_P (stmt_vinfo)
			|| compare_step_with_zero (vinfo, stmt_vinfo) <= 0
			|| vect_load_lanes_supported
			     (STMT_VINFO_VECTYPE (stmt_vinfo),
			      DR_GROUP_SIZE (stmt_vinfo), masked) == IFN_LAST
			/* ???  During SLP re-discovery with a single lane
			   a masked grouped load will appear permuted and
			   discovery will fail.  We have to rework this
			   on the discovery side - for now avoid ICEing.  */
			|| masked)
		      {
			can_use_lanes = false;
			break;
		      }
		  }

	      if (can_use_lanes)
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_NOTE, vect_location,
				     "SLP instance %p can use load/store-lanes,"
				     " re-discovering with single-lanes\n",
				     (void *) instance);

		  stmt_vec_info stmt_info = SLP_TREE_REPRESENTATIVE (slp_root);

		  vect_free_slp_instance (instance);
		  limit = max_tree_size;
		  bool res = vect_analyze_slp_instance (vinfo, bst_map,
							stmt_info,
							slp_inst_kind_store,
							max_tree_size, &limit,
							true);
		  gcc_assert (res);
		  auto new_inst = LOOP_VINFO_SLP_INSTANCES (vinfo).pop ();
		  LOOP_VINFO_SLP_INSTANCES (vinfo)[i] = new_inst;
		}
	    }
	}

  /* When we end up with load permutations that we cannot possibly handle,
     like those requiring three vector inputs, lower them using interleaving
     like schemes.  */
  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      vect_lower_load_permutations (loop_vinfo, bst_map);
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "SLP graph after lowering permutations:\n");
	  hash_set<slp_tree> visited;
	  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (vinfo), i, instance)
	    vect_print_slp_graph (MSG_NOTE, vect_location,
				  SLP_INSTANCE_TREE (instance), visited);
	}
    }

  release_scalar_stmts_to_slp_tree_map (bst_map);

  if (pattern_found && dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "Pattern matched SLP tree\n");
      hash_set<slp_tree> visited;
      FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (vinfo), i, instance)
	vect_print_slp_graph (MSG_NOTE, vect_location,
			      SLP_INSTANCE_TREE (instance), visited);
    }

  return opt_result::success ();
}

/* Estimates the cost of inserting layout changes into the SLP graph.
   It can also say that the insertion is impossible.  */

struct slpg_layout_cost
{
  slpg_layout_cost () = default;
  slpg_layout_cost (sreal, bool);

  static slpg_layout_cost impossible () { return { sreal::max (), 0 }; }
  bool is_possible () const { return depth != sreal::max (); }

  bool operator== (const slpg_layout_cost &) const;
  bool operator!= (const slpg_layout_cost &) const;

  bool is_better_than (const slpg_layout_cost &, bool) const;

  void add_parallel_cost (const slpg_layout_cost &);
  void add_serial_cost (const slpg_layout_cost &);
  void split (unsigned int);

  /* The longest sequence of layout changes needed during any traversal
     of the partition dag, weighted by execution frequency.

     This is the most important metric when optimizing for speed, since
     it helps to ensure that we keep the number of operations on
     critical paths to a minimum.  */
  sreal depth = 0;

  /* An estimate of the total number of operations needed.  It is weighted by
     execution frequency when optimizing for speed but not when optimizing for
     size.  In order to avoid double-counting, a node with a fanout of N will
     distribute 1/N of its total cost to each successor.

     This is the most important metric when optimizing for size, since
     it helps to keep the total number of operations to a minimum,  */
  sreal total = 0;
};

/* Construct costs for a node with weight WEIGHT.  A higher weight
   indicates more frequent execution.  IS_FOR_SIZE is true if we are
   optimizing for size rather than speed.  */

slpg_layout_cost::slpg_layout_cost (sreal weight, bool is_for_size)
  : depth (weight), total (is_for_size && weight > 0 ? 1 : weight)
{
}

bool
slpg_layout_cost::operator== (const slpg_layout_cost &other) const
{
  return depth == other.depth && total == other.total;
}

bool
slpg_layout_cost::operator!= (const slpg_layout_cost &other) const
{
  return !operator== (other);
}

/* Return true if these costs are better than OTHER.  IS_FOR_SIZE is
   true if we are optimizing for size rather than speed.  */

bool
slpg_layout_cost::is_better_than (const slpg_layout_cost &other,
				  bool is_for_size) const
{
  if (is_for_size)
    {
      if (total != other.total)
	return total < other.total;
      return depth < other.depth;
    }
  else
    {
      if (depth != other.depth)
	return depth < other.depth;
      return total < other.total;
    }
}

/* Increase the costs to account for something with cost INPUT_COST
   happening in parallel with the current costs.  */

void
slpg_layout_cost::add_parallel_cost (const slpg_layout_cost &input_cost)
{
  depth = std::max (depth, input_cost.depth);
  total += input_cost.total;
}

/* Increase the costs to account for something with cost INPUT_COST
   happening in series with the current costs.  */

void
slpg_layout_cost::add_serial_cost (const slpg_layout_cost &other)
{
  depth += other.depth;
  total += other.total;
}

/* Split the total cost among TIMES successors or predecessors.  */

void
slpg_layout_cost::split (unsigned int times)
{
  if (times > 1)
    total /= times;
}

/* Information about one node in the SLP graph, for use during
   vect_optimize_slp_pass.  */

struct slpg_vertex
{
  slpg_vertex (slp_tree node_) : node (node_) {}

  /* The node itself.  */
  slp_tree node;

  /* Which partition the node belongs to, or -1 if none.  Nodes outside of
     partitions are flexible; they can have whichever layout consumers
     want them to have.  */
  int partition = -1;

  /* The number of nodes that directly use the result of this one
     (i.e. the number of nodes that count this one as a child).  */
  unsigned int out_degree = 0;

  /* The execution frequency of the node.  */
  sreal weight = 0;

  /* The total execution frequency of all nodes that directly use the
     result of this one.  */
  sreal out_weight = 0;
};

/* Information about one partition of the SLP graph, for use during
   vect_optimize_slp_pass.  */

struct slpg_partition_info
{
  /* The nodes in the partition occupy indices [NODE_BEGIN, NODE_END)
     of m_partitioned_nodes.  */
  unsigned int node_begin = 0;
  unsigned int node_end = 0;

  /* Which layout we've chosen to use for this partition, or -1 if
     we haven't picked one yet.  */
  int layout = -1;

  /* The number of predecessors and successors in the partition dag.
     The predecessors always have lower partition numbers and the
     successors always have higher partition numbers.

     Note that the directions of these edges are not necessarily the
     same as in the data flow graph.  For example, if an SCC has separate
     partitions for an inner loop and an outer loop, the inner loop's
     partition will have at least two incoming edges from the outer loop's
     partition: one for a live-in value and one for a live-out value.
     In data flow terms, one of these edges would also be from the outer loop
     to the inner loop, but the other would be in the opposite direction.  */
  unsigned int in_degree = 0;
  unsigned int out_degree = 0;
};

/* Information about the costs of using a particular layout for a
   particular partition.  It can also say that the combination is
   impossible.  */

struct slpg_partition_layout_costs
{
  bool is_possible () const { return internal_cost.is_possible (); }
  void mark_impossible () { internal_cost = slpg_layout_cost::impossible (); }

  /* The costs inherited from predecessor partitions.  */
  slpg_layout_cost in_cost;

  /* The inherent cost of the layout within the node itself.  For example,
     this is nonzero for a load if choosing a particular layout would require
     the load to permute the loaded elements.  It is nonzero for a
     VEC_PERM_EXPR if the permutation cannot be eliminated or converted
     to full-vector moves.  */
  slpg_layout_cost internal_cost;

  /* The costs inherited from successor partitions.  */
  slpg_layout_cost out_cost;
};

/* This class tries to optimize the layout of vectors in order to avoid
   unnecessary shuffling.  At the moment, the set of possible layouts are
   restricted to bijective permutations.

   The goal of the pass depends on whether we're optimizing for size or
   for speed.  When optimizing for size, the goal is to reduce the overall
   number of layout changes (including layout changes implied by things
   like load permutations).  When optimizing for speed, the goal is to
   reduce the maximum latency attributable to layout changes on any
   non-cyclical path through the data flow graph.

   For example, when optimizing a loop nest for speed, we will prefer
   to make layout changes outside of a loop rather than inside of a loop,
   and will prefer to make layout changes in parallel rather than serially,
   even if that increases the overall number of layout changes.

   The high-level procedure is:

   (1) Build a graph in which edges go from uses (parents) to definitions
       (children).

   (2) Divide the graph into a dag of strongly-connected components (SCCs).

   (3) When optimizing for speed, partition the nodes in each SCC based
       on their containing cfg loop.  When optimizing for size, treat
       each SCC as a single partition.

       This gives us a dag of partitions.  The goal is now to assign a
       layout to each partition.

   (4) Construct a set of vector layouts that are worth considering.
       Record which nodes must keep their current layout.

   (5) Perform a forward walk over the partition dag (from loads to stores)
       accumulating the "forward" cost of using each layout.  When visiting
       each partition, assign a tentative choice of layout to the partition
       and use that choice when calculating the cost of using a different
       layout in successor partitions.

   (6) Perform a backward walk over the partition dag (from stores to loads),
       accumulating the "backward" cost of using each layout.  When visiting
       each partition, make a final choice of layout for that partition based
       on the accumulated forward costs (from (5)) and backward costs
       (from (6)).

   (7) Apply the chosen layouts to the SLP graph.

   For example, consider the SLP statements:

   S1:      a_1 = load
       loop:
   S2:      a_2 = PHI<a_1, a_3>
   S3:      b_1 = load
   S4:      a_3 = a_2 + b_1
       exit:
   S5:      a_4 = PHI<a_3>
   S6:      store a_4

   S2 and S4 form an SCC and are part of the same loop.  Every other
   statement is in a singleton SCC.  In this example there is a one-to-one
   mapping between SCCs and partitions and the partition dag looks like this;

	S1     S3
	 \     /
	  S2+S4
	    |
	   S5
	    |
	   S6

   S2, S3 and S4 will have a higher execution frequency than the other
   statements, so when optimizing for speed, the goal is to avoid any
   layout changes:

   - within S3
   - within S2+S4
   - on the S3->S2+S4 edge

   For example, if S3 was originally a reversing load, the goal of the
   pass is to make it an unreversed load and change the layout on the
   S1->S2+S4 and S2+S4->S5 edges to compensate.  (Changing the layout
   on S1->S2+S4 and S5->S6 would also be acceptable.)

   The difference between SCCs and partitions becomes important if we
   add an outer loop:

   S1:      a_1 = ...
       loop1:
   S2:      a_2 = PHI<a_1, a_6>
   S3:      b_1 = load
   S4:      a_3 = a_2 + b_1
       loop2:
   S5:      a_4 = PHI<a_3, a_5>
   S6:      c_1 = load
   S7:      a_5 = a_4 + c_1
       exit2:
   S8:      a_6 = PHI<a_5>
   S9:      store a_6
       exit1:

   Here, S2, S4, S5, S7 and S8 form a single SCC.  However, when optimizing
   for speed, we usually do not want restrictions in the outer loop to "infect"
   the decision for the inner loop.  For example, if an outer-loop node
   in the SCC contains a statement with a fixed layout, that should not
   prevent the inner loop from using a different layout.  Conversely,
   the inner loop should not dictate a layout to the outer loop: if the
   outer loop does a lot of computation, then it may not be efficient to
   do all of that computation in the inner loop's preferred layout.

   So when optimizing for speed, we partition the SCC into S2+S4+S8 (outer)
   and S5+S7 (inner).  We also try to arrange partitions so that:

   - the partition for an outer loop comes before the partition for
     an inner loop

   - if a sibling loop A dominates a sibling loop B, A's partition
     comes before B's

   This gives the following partition dag for the example above:

	S1        S3
	 \        /
	  S2+S4+S8   S6
	   |   \\    /
	   |    S5+S7
	   |
	  S9

   There are two edges from S2+S4+S8 to S5+S7: one for the edge S4->S5 and
   one for a reversal of the edge S7->S8.

   The backward walk picks a layout for S5+S7 before S2+S4+S8.  The choice
   for S2+S4+S8 therefore has to balance the cost of using the outer loop's
   preferred layout against the cost of changing the layout on entry to the
   inner loop (S4->S5) and on exit from the inner loop (S7->S8 reversed).

   Although this works well when optimizing for speed, it has the downside
   when optimizing for size that the choice of layout for S5+S7 is completely
   independent of S9, which lessens the chance of reducing the overall number
   of permutations.  We therefore do not partition SCCs when optimizing
   for size.

   To give a concrete example of the difference between optimizing
   for size and speed, consider:

   a[0] = (b[1] << c[3]) - d[1];
   a[1] = (b[0] << c[2]) - d[0];
   a[2] = (b[3] << c[1]) - d[3];
   a[3] = (b[2] << c[0]) - d[2];

   There are three different layouts here: one for a, one for b and d,
   and one for c.  When optimizing for speed it is better to permute each
   of b, c and d into the order required by a, since those permutations
   happen in parallel.  But when optimizing for size, it is better to:

   - permute c into the same order as b
   - do the arithmetic
   - permute the result into the order required by a

   This gives 2 permutations rather than 3.  */

class vect_optimize_slp_pass
{
public:
  vect_optimize_slp_pass (vec_info *vinfo) : m_vinfo (vinfo) {}
  void run ();

private:
  /* Graph building.  */
  struct loop *containing_loop (slp_tree);
  bool is_cfg_latch_edge (graph_edge *);
  void build_vertices (hash_set<slp_tree> &, slp_tree);
  void build_vertices ();
  void build_graph ();

  /* Partitioning.  */
  void create_partitions ();
  template<typename T> void for_each_partition_edge (unsigned int, T);

  /* Layout selection.  */
  bool is_compatible_layout (slp_tree, unsigned int);
  int change_layout_cost (slp_tree, unsigned int, unsigned int);
  slpg_partition_layout_costs &partition_layout_costs (unsigned int,
						       unsigned int);
  void change_vec_perm_layout (slp_tree, lane_permutation_t &,
			       int, unsigned int);
  int internal_node_cost (slp_tree, int, unsigned int);
  void start_choosing_layouts ();

  /* Cost propagation.  */
  slpg_layout_cost edge_layout_cost (graph_edge *, unsigned int,
				     unsigned int, unsigned int);
  slpg_layout_cost total_in_cost (unsigned int);
  slpg_layout_cost forward_cost (graph_edge *, unsigned int, unsigned int);
  slpg_layout_cost backward_cost (graph_edge *, unsigned int, unsigned int);
  void forward_pass ();
  void backward_pass ();

  /* Rematerialization.  */
  slp_tree get_result_with_layout (slp_tree, unsigned int);
  void materialize ();

  /* Clean-up.  */
  void remove_redundant_permutations ();

  void dump ();

  vec_info *m_vinfo;

  /* True if we should optimize the graph for size, false if we should
     optimize it for speed.  (It wouldn't be easy to make this decision
     more locally.)  */
  bool m_optimize_size;

  /* A graph of all SLP nodes, with edges leading from uses to definitions.
     In other words, a node's predecessors are its slp_tree parents and
     a node's successors are its slp_tree children.  */
  graph *m_slpg = nullptr;

  /* The vertices of M_SLPG, indexed by slp_tree::vertex.  */
  auto_vec<slpg_vertex> m_vertices;

  /* The list of all leaves of M_SLPG. such as external definitions, constants,
     and loads.  */
  auto_vec<int> m_leafs;

  /* This array has one entry for every vector layout that we're considering.
     Element 0 is null and indicates "no change".  Other entries describe
     permutations that are inherent in the current graph and that we would
     like to reverse if possible.

     For example, a permutation { 1, 2, 3, 0 } means that something has
     effectively been permuted in that way, such as a load group
     { a[1], a[2], a[3], a[0] } (viewed as a permutation of a[0:3]).
     We'd then like to apply the reverse permutation { 3, 0, 1, 2 }
     in order to put things "back" in order.  */
  auto_vec<vec<unsigned> > m_perms;

  /* A partitioning of the nodes for which a layout must be chosen.
     Each partition represents an <SCC, cfg loop> pair; that is,
     nodes in different SCCs belong to different partitions, and nodes
     within an SCC can be further partitioned according to a containing
     cfg loop.  Partition <SCC1, L1> comes before <SCC2, L2> if:

     - SCC1 != SCC2 and SCC1 is a predecessor of SCC2 in a forward walk
       from leaves (such as loads) to roots (such as stores).

     - SCC1 == SCC2 and L1's header strictly dominates L2's header.  */
  auto_vec<slpg_partition_info> m_partitions;

  /* The list of all nodes for which a layout must be chosen.  Nodes for
     partition P come before the nodes for partition P+1.  Nodes within a
     partition are in reverse postorder.  */
  auto_vec<unsigned int> m_partitioned_nodes;

  /* Index P * num-layouts + L contains the cost of using layout L
     for partition P.  */
  auto_vec<slpg_partition_layout_costs> m_partition_layout_costs;

  /* Index N * num-layouts + L, if nonnull, is a node that provides the
     original output of node N adjusted to have layout L.  */
  auto_vec<slp_tree> m_node_layouts;
};

/* Fill the vertices and leafs vector with all nodes in the SLP graph.
   Also record whether we should optimize anything for speed rather
   than size.  */

void
vect_optimize_slp_pass::build_vertices (hash_set<slp_tree> &visited,
					slp_tree node)
{
  unsigned i;
  slp_tree child;

  if (visited.add (node))
    return;

  if (stmt_vec_info rep = SLP_TREE_REPRESENTATIVE (node))
    {
      basic_block bb = gimple_bb (vect_orig_stmt (rep)->stmt);
      if (optimize_bb_for_speed_p (bb))
	m_optimize_size = false;
    }

  node->vertex = m_vertices.length ();
  m_vertices.safe_push (slpg_vertex (node));

  bool leaf = true;
  bool force_leaf = false;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      {
	leaf = false;
	build_vertices (visited, child);
      }
    else
      force_leaf = true;
  /* Since SLP discovery works along use-def edges all cycles have an
     entry - but there's the exception of cycles where we do not handle
     the entry explicitely (but with a NULL SLP node), like some reductions
     and inductions.  Force those SLP PHIs to act as leafs to make them
     backwards reachable.  */
  if (leaf || force_leaf)
    m_leafs.safe_push (node->vertex);
}

/* Fill the vertices and leafs vector with all nodes in the SLP graph.  */

void
vect_optimize_slp_pass::build_vertices ()
{
  hash_set<slp_tree> visited;
  unsigned i;
  slp_instance instance;
  FOR_EACH_VEC_ELT (m_vinfo->slp_instances, i, instance)
    build_vertices (visited, SLP_INSTANCE_TREE (instance));
}

/* Apply (reverse) bijectite PERM to VEC.  */

template <class T>
static void
vect_slp_permute (vec<unsigned> perm,
		  vec<T> &vec, bool reverse)
{
  auto_vec<T, 64> saved;
  saved.create (vec.length ());
  for (unsigned i = 0; i < vec.length (); ++i)
    saved.quick_push (vec[i]);

  if (reverse)
    {
      for (unsigned i = 0; i < vec.length (); ++i)
	vec[perm[i]] = saved[i];
      for (unsigned i = 0; i < vec.length (); ++i)
	gcc_assert (vec[perm[i]] == saved[i]);
    }
  else
    {
      for (unsigned i = 0; i < vec.length (); ++i)
	vec[i] = saved[perm[i]];
      for (unsigned i = 0; i < vec.length (); ++i)
	gcc_assert (vec[i] == saved[perm[i]]);
    }
}

/* Return the cfg loop that contains NODE.  */

struct loop *
vect_optimize_slp_pass::containing_loop (slp_tree node)
{
  stmt_vec_info rep = SLP_TREE_REPRESENTATIVE (node);
  if (!rep)
    return ENTRY_BLOCK_PTR_FOR_FN (cfun)->loop_father;
  return gimple_bb (vect_orig_stmt (rep)->stmt)->loop_father;
}

/* Return true if UD (an edge from a use to a definition) is associated
   with a loop latch edge in the cfg.  */

bool
vect_optimize_slp_pass::is_cfg_latch_edge (graph_edge *ud)
{
  slp_tree use = m_vertices[ud->src].node;
  slp_tree def = m_vertices[ud->dest].node;
  if ((SLP_TREE_DEF_TYPE (use) != vect_internal_def
       || SLP_TREE_CODE (use) == VEC_PERM_EXPR)
      || SLP_TREE_DEF_TYPE (def) != vect_internal_def)
    return false;

  stmt_vec_info use_rep = vect_orig_stmt (SLP_TREE_REPRESENTATIVE (use));
  return (is_a<gphi *> (use_rep->stmt)
	  && bb_loop_header_p (gimple_bb (use_rep->stmt))
	  && containing_loop (def) == containing_loop (use));
}

/* Build the graph.  Mark edges that correspond to cfg loop latch edges with
   a nonnull data field.  */

void
vect_optimize_slp_pass::build_graph ()
{
  m_optimize_size = true;
  build_vertices ();

  m_slpg = new_graph (m_vertices.length ());
  for (slpg_vertex &v : m_vertices)
    for (slp_tree child : SLP_TREE_CHILDREN (v.node))
      if (child)
	{
	  graph_edge *ud = add_edge (m_slpg, v.node->vertex, child->vertex);
	  if (is_cfg_latch_edge (ud))
	    ud->data = this;
	}
}

/* Return true if E corresponds to a loop latch edge in the cfg.  */

static bool
skip_cfg_latch_edges (graph_edge *e)
{
  return e->data;
}

/* Create the node partitions.  */

void
vect_optimize_slp_pass::create_partitions ()
{
  /* Calculate a postorder of the graph, ignoring edges that correspond
     to natural latch edges in the cfg.  Reading the vector from the end
     to the beginning gives the reverse postorder.  */
  auto_vec<int> initial_rpo;
  graphds_dfs (m_slpg, &m_leafs[0], m_leafs.length (), &initial_rpo,
	       false, NULL, skip_cfg_latch_edges);
  gcc_assert (initial_rpo.length () == m_vertices.length ());

  /* Calculate the strongly connected components of the graph.  */
  auto_vec<int> scc_grouping;
  unsigned int num_sccs = graphds_scc (m_slpg, NULL, NULL, &scc_grouping);

  /* Create a new index order in which all nodes from the same SCC are
     consecutive.  Use scc_pos to record the index of the first node in
     each SCC.  */
  auto_vec<unsigned int> scc_pos (num_sccs);
  int last_component = -1;
  unsigned int node_count = 0;
  for (unsigned int node_i : scc_grouping)
    {
      if (last_component != m_slpg->vertices[node_i].component)
	{
	  last_component = m_slpg->vertices[node_i].component;
	  gcc_assert (last_component == int (scc_pos.length ()));
	  scc_pos.quick_push (node_count);
	}
      node_count += 1;
    }
  gcc_assert (node_count == initial_rpo.length ()
	      && last_component + 1 == int (num_sccs));

  /* Use m_partitioned_nodes to group nodes into SCC order, with the nodes
     inside each SCC following the RPO we calculated above.  The fact that
     we ignored natural latch edges when calculating the RPO should ensure
     that, for natural loop nests:

     - the first node that we encounter in a cfg loop is the loop header phi
     - the loop header phis are in dominance order

     Arranging for this is an optimization (see below) rather than a
     correctness issue.  Unnatural loops with a tangled mess of backedges
     will still work correctly, but might give poorer results.

     Also update scc_pos so that it gives 1 + the index of the last node
     in the SCC.  */
  m_partitioned_nodes.safe_grow (node_count);
  for (unsigned int old_i = initial_rpo.length (); old_i-- > 0;)
    {
      unsigned int node_i = initial_rpo[old_i];
      unsigned int new_i = scc_pos[m_slpg->vertices[node_i].component]++;
      m_partitioned_nodes[new_i] = node_i;
    }

  /* When optimizing for speed, partition each SCC based on the containing
     cfg loop. The order we constructed above should ensure that, for natural
     cfg loops, we'll create sub-SCC partitions for outer loops before
     the corresponding sub-SCC partitions for inner loops.  Similarly,
     when one sibling loop A dominates another sibling loop B, we should
     create a sub-SCC partition for A before a sub-SCC partition for B.

     As above, nothing depends for correctness on whether this achieves
     a natural nesting, but we should get better results when it does.  */
  m_partitions.reserve (m_vertices.length ());
  unsigned int next_partition_i = 0;
  hash_map<struct loop *, int> loop_partitions;
  unsigned int rpo_begin = 0;
  unsigned int num_partitioned_nodes = 0;
  for (unsigned int rpo_end : scc_pos)
    {
      loop_partitions.empty ();
      unsigned int partition_i = next_partition_i;
      for (unsigned int rpo_i = rpo_begin; rpo_i < rpo_end; ++rpo_i)
	{
	  /* Handle externals and constants optimistically throughout.
	     But treat existing vectors as fixed since we do not handle
	     permuting them.  */
	  unsigned int node_i = m_partitioned_nodes[rpo_i];
	  auto &vertex = m_vertices[node_i];
	  if ((SLP_TREE_DEF_TYPE (vertex.node) == vect_external_def
	       && !SLP_TREE_VEC_DEFS (vertex.node).exists ())
	      || SLP_TREE_DEF_TYPE (vertex.node) == vect_constant_def)
	    vertex.partition = -1;
	  else
	    {
	      bool existed;
	      if (m_optimize_size)
		existed = next_partition_i > partition_i;
	      else
		{
		  struct loop *loop = containing_loop (vertex.node);
		  auto &entry = loop_partitions.get_or_insert (loop, &existed);
		  if (!existed)
		    entry = next_partition_i;
		  partition_i = entry;
		}
	      if (!existed)
		{
		  m_partitions.quick_push (slpg_partition_info ());
		  next_partition_i += 1;
		}
	      vertex.partition = partition_i;
	      num_partitioned_nodes += 1;
	      m_partitions[partition_i].node_end += 1;
	    }
	}
      rpo_begin = rpo_end;
    }

  /* Assign ranges of consecutive node indices to each partition,
     in partition order.  Start with node_end being the same as
     node_begin so that the next loop can use it as a counter.  */
  unsigned int node_begin = 0;
  for (auto &partition : m_partitions)
    {
      partition.node_begin = node_begin;
      node_begin += partition.node_end;
      partition.node_end = partition.node_begin;
    }
  gcc_assert (node_begin == num_partitioned_nodes);

  /* Finally build the list of nodes in partition order.  */
  m_partitioned_nodes.truncate (num_partitioned_nodes);
  for (unsigned int node_i = 0; node_i < m_vertices.length (); ++node_i)
    {
      int partition_i = m_vertices[node_i].partition;
      if (partition_i >= 0)
	{
	  unsigned int order_i = m_partitions[partition_i].node_end++;
	  m_partitioned_nodes[order_i] = node_i;
	}
    }
}

/* Look for edges from earlier partitions into node NODE_I and edges from
   node NODE_I into later partitions.  Call:

      FN (ud, other_node_i)

   for each such use-to-def edge ud, where other_node_i is the node at the
   other end of the edge.  */

template<typename T>
void
vect_optimize_slp_pass::for_each_partition_edge (unsigned int node_i, T fn)
{
  int partition_i = m_vertices[node_i].partition;
  for (graph_edge *pred = m_slpg->vertices[node_i].pred;
       pred; pred = pred->pred_next)
    {
      int src_partition_i = m_vertices[pred->src].partition;
      if (src_partition_i >= 0 && src_partition_i != partition_i)
	fn (pred, pred->src);
    }
  for (graph_edge *succ = m_slpg->vertices[node_i].succ;
       succ; succ = succ->succ_next)
    {
      int dest_partition_i = m_vertices[succ->dest].partition;
      if (dest_partition_i >= 0 && dest_partition_i != partition_i)
	fn (succ, succ->dest);
    }
}

/* Return true if layout LAYOUT_I is compatible with the number of SLP lanes
   that NODE would operate on.  This test is independent of NODE's actual
   operation.  */

bool
vect_optimize_slp_pass::is_compatible_layout (slp_tree node,
					      unsigned int layout_i)
{
  if (layout_i == 0)
    return true;

  if (SLP_TREE_LANES (node) != m_perms[layout_i].length ())
    return false;

  return true;
}

/* Return the cost (in arbtirary units) of going from layout FROM_LAYOUT_I
   to layout TO_LAYOUT_I for a node like NODE.  Return -1 if either of the
   layouts is incompatible with NODE or if the change is not possible for
   some other reason.

   The properties taken from NODE include the number of lanes and the
   vector type.  The actual operation doesn't matter.  */

int
vect_optimize_slp_pass::change_layout_cost (slp_tree node,
					    unsigned int from_layout_i,
					    unsigned int to_layout_i)
{
  if (!is_compatible_layout (node, from_layout_i)
      || !is_compatible_layout (node, to_layout_i))
    return -1;

  if (from_layout_i == to_layout_i)
    return 0;

  auto_vec<slp_tree, 1> children (1);
  children.quick_push (node);
  auto_lane_permutation_t perm (SLP_TREE_LANES (node));
  if (from_layout_i > 0)
    for (unsigned int i : m_perms[from_layout_i])
      perm.quick_push ({ 0, i });
  else
    for (unsigned int i = 0; i < SLP_TREE_LANES (node); ++i)
      perm.quick_push ({ 0, i });
  if (to_layout_i > 0)
    vect_slp_permute (m_perms[to_layout_i], perm, true);
  auto count = vectorizable_slp_permutation_1 (m_vinfo, nullptr, node, perm,
					       children, false);
  if (count >= 0)
    return MAX (count, 1);

  /* ??? In principle we could try changing via layout 0, giving two
     layout changes rather than 1.  Doing that would require
     corresponding support in get_result_with_layout.  */
  return -1;
}

/* Return the costs of assigning layout LAYOUT_I to partition PARTITION_I.  */

inline slpg_partition_layout_costs &
vect_optimize_slp_pass::partition_layout_costs (unsigned int partition_i,
						unsigned int layout_i)
{
  return m_partition_layout_costs[partition_i * m_perms.length () + layout_i];
}

/* Change PERM in one of two ways:

   - if IN_LAYOUT_I < 0, accept input operand I in the layout that has been
     chosen for child I of NODE.

   - if IN_LAYOUT >= 0, accept all inputs operands with that layout.

   In both cases, arrange for the output to have layout OUT_LAYOUT_I  */

void
vect_optimize_slp_pass::
change_vec_perm_layout (slp_tree node, lane_permutation_t &perm,
			int in_layout_i, unsigned int out_layout_i)
{
  for (auto &entry : perm)
    {
      int this_in_layout_i = in_layout_i;
      if (this_in_layout_i < 0)
	{
	  slp_tree in_node = SLP_TREE_CHILDREN (node)[entry.first];
	  unsigned int in_partition_i = m_vertices[in_node->vertex].partition;
	  if (in_partition_i == -1u)
	    continue;
	  this_in_layout_i = m_partitions[in_partition_i].layout;
	}
      if (this_in_layout_i > 0)
	entry.second = m_perms[this_in_layout_i][entry.second];
    }
  if (out_layout_i > 0)
    vect_slp_permute (m_perms[out_layout_i], perm, true);
}

/* Check whether the target allows NODE to be rearranged so that the node's
   output has layout OUT_LAYOUT_I.  Return the cost of the change if so,
   in the same arbitrary units as for change_layout_cost.  Return -1 otherwise.

   If NODE is a VEC_PERM_EXPR and IN_LAYOUT_I < 0, also check whether
   NODE can adapt to the layout changes that have (perhaps provisionally)
   been chosen for NODE's children, so that no extra permutations are
   needed on either the input or the output of NODE.

   If NODE is a VEC_PERM_EXPR and IN_LAYOUT_I >= 0, instead assume
   that all inputs will be forced into layout IN_LAYOUT_I beforehand.

   IN_LAYOUT_I has no meaning for other types of node.

   Keeping the node as-is is always valid.  If the target doesn't appear
   to support the node as-is, but might realistically support other layouts,
   then layout 0 instead has the cost of a worst-case permutation.  On the
   one hand, this ensures that every node has at least one valid layout,
   avoiding what would otherwise be an awkward special case.  On the other,
   it still encourages the pass to change an invalid pre-existing layout
   choice into a valid one.  */

int
vect_optimize_slp_pass::internal_node_cost (slp_tree node, int in_layout_i,
					    unsigned int out_layout_i)
{
  const int fallback_cost = 1;

  if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
    {
      auto_lane_permutation_t tmp_perm;
      tmp_perm.safe_splice (SLP_TREE_LANE_PERMUTATION (node));

      /* Check that the child nodes support the chosen layout.  Checking
	 the first child is enough, since any second child would have the
	 same shape.  */
      auto first_child = SLP_TREE_CHILDREN (node)[0];
      if (in_layout_i > 0
	  && !is_compatible_layout (first_child, in_layout_i))
	return -1;

      change_vec_perm_layout (node, tmp_perm, in_layout_i, out_layout_i);
      int count = vectorizable_slp_permutation_1 (m_vinfo, nullptr,
						  node, tmp_perm,
						  SLP_TREE_CHILDREN (node),
						  false);
      if (count < 0)
	{
	  if (in_layout_i == 0 && out_layout_i == 0)
	    {
	      /* Use the fallback cost if the node could in principle support
		 some nonzero layout for both the inputs and the outputs.
		 Otherwise assume that the node will be rejected later
		 and rebuilt from scalars.  */
	      if (SLP_TREE_LANES (node) == SLP_TREE_LANES (first_child))
		return fallback_cost;
	      return 0;
	    }
	  return -1;
	}

      /* We currently have no way of telling whether the new layout is cheaper
	 or more expensive than the old one.  But at least in principle,
	 it should be worth making zero permutations (whole-vector shuffles)
	 cheaper than real permutations, in case the pass is able to remove
	 the latter.  */
      return count == 0 ? 0 : 1;
    }

  stmt_vec_info rep = SLP_TREE_REPRESENTATIVE (node);
  if (rep
      && STMT_VINFO_DATA_REF (rep)
      && DR_IS_READ (STMT_VINFO_DATA_REF (rep))
      && SLP_TREE_LOAD_PERMUTATION (node).exists ())
    {
      auto_load_permutation_t tmp_perm;
      tmp_perm.safe_splice (SLP_TREE_LOAD_PERMUTATION (node));
      if (out_layout_i > 0)
	vect_slp_permute (m_perms[out_layout_i], tmp_perm, true);

      poly_uint64 vf = 1;
      if (auto loop_vinfo = dyn_cast<loop_vec_info> (m_vinfo))
	vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      unsigned int n_perms;
      if (!vect_transform_slp_perm_load_1 (m_vinfo, node, tmp_perm, vNULL,
					   nullptr, vf, true, false, &n_perms))
	{
	  auto rep = SLP_TREE_REPRESENTATIVE (node);
	  if (out_layout_i == 0)
	    {
	      /* Use the fallback cost if the load is an N-to-N permutation.
		 Otherwise assume that the node will be rejected later
		 and rebuilt from scalars.  */
	      if (STMT_VINFO_GROUPED_ACCESS (rep)
		  && (DR_GROUP_SIZE (DR_GROUP_FIRST_ELEMENT (rep))
		      == SLP_TREE_LANES (node)))
		return fallback_cost;
	      return 0;
	    }
	  return -1;
	}

      /* See the comment above the corresponding VEC_PERM_EXPR handling.  */
      return n_perms == 0 ? 0 : 1;
    }

  return 0;
}

/* Decide which element layouts we should consider using.  Calculate the
   weights associated with inserting layout changes on partition edges.
   Also mark partitions that cannot change layout, by setting their
   layout to zero.  */

void
vect_optimize_slp_pass::start_choosing_layouts ()
{
  /* Used to assign unique permutation indices.  */
  using perm_hash = unbounded_hashmap_traits<
    vec_free_hash_base<int_hash_base<unsigned>>,
    int_hash<int, -1, -2>
  >;
  hash_map<vec<unsigned>, int, perm_hash> layout_ids;

  /* Layout 0 is "no change".  */
  m_perms.safe_push (vNULL);

  /* Create layouts from existing permutations.  */
  auto_load_permutation_t tmp_perm;
  for (unsigned int node_i : m_partitioned_nodes)
    {
      /* Leafs also double as entries to the reverse graph.  Allow the
	 layout of those to be changed.  */
      auto &vertex = m_vertices[node_i];
      auto &partition = m_partitions[vertex.partition];
      if (!m_slpg->vertices[node_i].succ)
	partition.layout = 0;

      /* Loads and VEC_PERM_EXPRs are the only things generating permutes.  */
      slp_tree node = vertex.node;
      stmt_vec_info dr_stmt = SLP_TREE_REPRESENTATIVE (node);
      slp_tree child;
      unsigned HOST_WIDE_INT imin, imax = 0;
      bool any_permute = false;
      tmp_perm.truncate (0);
      if (SLP_TREE_LOAD_PERMUTATION (node).exists ())
	{
	  /* If splitting out a SLP_TREE_LANE_PERMUTATION can make the node
	     unpermuted, record a layout that reverses this permutation.

	     We would need more work to cope with loads that are internally
	     permuted and also have inputs (such as masks for
	     IFN_MASK_LOADs).  */
	  gcc_assert (partition.layout == 0 && !m_slpg->vertices[node_i].succ);
	  if (!STMT_VINFO_GROUPED_ACCESS (dr_stmt))
	    {
	      partition.layout = -1;
	      continue;
	    }
	  dr_stmt = DR_GROUP_FIRST_ELEMENT (dr_stmt);
	  imin = DR_GROUP_SIZE (dr_stmt) + 1;
	  tmp_perm.safe_splice (SLP_TREE_LOAD_PERMUTATION (node));
	}
      else if (SLP_TREE_CODE (node) == VEC_PERM_EXPR
	       && SLP_TREE_CHILDREN (node).length () == 1
	       && (child = SLP_TREE_CHILDREN (node)[0])
	       && (TYPE_VECTOR_SUBPARTS (SLP_TREE_VECTYPE (child))
		   .is_constant (&imin)))
	{
	  /* If the child has the same vector size as this node,
	     reversing the permutation can make the permutation a no-op.
	     In other cases it can change a true permutation into a
	     full-vector extract.  */
	  tmp_perm.reserve (SLP_TREE_LANES (node));
	  for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	    tmp_perm.quick_push (SLP_TREE_LANE_PERMUTATION (node)[j].second);
	}
      else
	continue;

      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	{
	  unsigned idx = tmp_perm[j];
	  imin = MIN (imin, idx);
	  imax = MAX (imax, idx);
	  if (idx - tmp_perm[0] != j)
	    any_permute = true;
	}
      /* If the span doesn't match we'd disrupt VF computation, avoid
	 that for now.  */
      if (imax - imin + 1 != SLP_TREE_LANES (node))
	continue;
      /* If there's no permute no need to split one out.  In this case
	 we can consider turning a load into a permuted load, if that
	 turns out to be cheaper than alternatives.  */
      if (!any_permute)
	{
	  partition.layout = -1;
	  continue;
	}

      /* For now only handle true permutes, like
	 vect_attempt_slp_rearrange_stmts did.  This allows us to be lazy
	 when permuting constants and invariants keeping the permute
	 bijective.  */
      auto_sbitmap load_index (SLP_TREE_LANES (node));
      bitmap_clear (load_index);
      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	bitmap_set_bit (load_index, tmp_perm[j] - imin);
      unsigned j;
      for (j = 0; j < SLP_TREE_LANES (node); ++j)
	if (!bitmap_bit_p (load_index, j))
	  break;
      if (j != SLP_TREE_LANES (node))
	continue;

      vec<unsigned> perm = vNULL;
      perm.safe_grow (SLP_TREE_LANES (node), true);
      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	perm[j] = tmp_perm[j] - imin;

      if (int (m_perms.length ()) >= param_vect_max_layout_candidates)
	{
	  /* Continue to use existing layouts, but don't add any more.  */
	  int *entry = layout_ids.get (perm);
	  partition.layout = entry ? *entry : 0;
	  perm.release ();
	}
      else
	{
	  bool existed;
	  int &layout_i = layout_ids.get_or_insert (perm, &existed);
	  if (existed)
	    perm.release ();
	  else
	    {
	      layout_i = m_perms.length ();
	      m_perms.safe_push (perm);
	    }
	  partition.layout = layout_i;
	}
    }

  /* Initially assume that every layout is possible and has zero cost
     in every partition.  */
  m_partition_layout_costs.safe_grow_cleared (m_partitions.length ()
					      * m_perms.length ());

  /* We have to mark outgoing permutations facing non-associating-reduction
     graph entries that are not represented as to be materialized.
     slp_inst_kind_bb_reduc currently only covers associatable reductions.  */
  for (slp_instance instance : m_vinfo->slp_instances)
    if (SLP_INSTANCE_KIND (instance) == slp_inst_kind_ctor)
      {
	unsigned int node_i = SLP_INSTANCE_TREE (instance)->vertex;
	m_partitions[m_vertices[node_i].partition].layout = 0;
      }
    else if (SLP_INSTANCE_KIND (instance) == slp_inst_kind_reduc_chain)
      {
	stmt_vec_info stmt_info
	  = SLP_TREE_REPRESENTATIVE (SLP_INSTANCE_TREE (instance));
	stmt_vec_info reduc_info = info_for_reduction (m_vinfo, stmt_info);
	if (needs_fold_left_reduction_p (TREE_TYPE
					   (gimple_get_lhs (stmt_info->stmt)),
					 STMT_VINFO_REDUC_CODE (reduc_info)))
	  {
	    unsigned int node_i = SLP_INSTANCE_TREE (instance)->vertex;
	    m_partitions[m_vertices[node_i].partition].layout = 0;
	  }
      }

  /* Check which layouts each node and partition can handle.  Calculate the
     weights associated with inserting layout changes on edges.  */
  for (unsigned int node_i : m_partitioned_nodes)
    {
      auto &vertex = m_vertices[node_i];
      auto &partition = m_partitions[vertex.partition];
      slp_tree node = vertex.node;

      if (stmt_vec_info rep = SLP_TREE_REPRESENTATIVE (node))
	{
	  vertex.weight = vect_slp_node_weight (node);

	  /* We do not handle stores with a permutation, so all
	     incoming permutations must have been materialized.

	     We also don't handle masked grouped loads, which lack a
	     permutation vector.  In this case the memory locations
	     form an implicit second input to the loads, on top of the
	     explicit mask input, and the memory input's layout cannot
	     be changed.

	     On the other hand, we do support permuting gather loads and
	     masked gather loads, where each scalar load is independent
	     of the others.  This can be useful if the address/index input
	     benefits from permutation.  */
	  if (STMT_VINFO_DATA_REF (rep)
	      && STMT_VINFO_GROUPED_ACCESS (rep)
	      && !SLP_TREE_LOAD_PERMUTATION (node).exists ())
	    partition.layout = 0;

	  /* We cannot change the layout of an operation that is
	     not independent on lanes.  Note this is an explicit
	     negative list since that's much shorter than the respective
	     positive one but it's critical to keep maintaining it.  */
	  if (is_gimple_call (STMT_VINFO_STMT (rep)))
	    switch (gimple_call_combined_fn (STMT_VINFO_STMT (rep)))
	      {
	      case CFN_COMPLEX_ADD_ROT90:
	      case CFN_COMPLEX_ADD_ROT270:
	      case CFN_COMPLEX_MUL:
	      case CFN_COMPLEX_MUL_CONJ:
	      case CFN_VEC_ADDSUB:
	      case CFN_VEC_FMADDSUB:
	      case CFN_VEC_FMSUBADD:
		partition.layout = 0;
	      default:;
	      }
	}

      auto process_edge = [&](graph_edge *ud, unsigned int other_node_i)
	{
	  auto &other_vertex = m_vertices[other_node_i];

	  /* Count the number of edges from earlier partitions and the number
	     of edges to later partitions.  */
	  if (other_vertex.partition < vertex.partition)
	    partition.in_degree += 1;
	  else
	    partition.out_degree += 1;

	  /* If the current node uses the result of OTHER_NODE_I, accumulate
	     the effects of that.  */
	  if (ud->src == int (node_i))
	    {
	      other_vertex.out_weight += vertex.weight;
	      other_vertex.out_degree += 1;
	    }
	};
      for_each_partition_edge (node_i, process_edge);
    }
}

/* Return the incoming costs for node NODE_I, assuming that each input keeps
   its current (provisional) choice of layout.  The inputs do not necessarily
   have the same layout as each other.  */

slpg_layout_cost
vect_optimize_slp_pass::total_in_cost (unsigned int node_i)
{
  auto &vertex = m_vertices[node_i];
  slpg_layout_cost cost;
  auto add_cost = [&](graph_edge *, unsigned int other_node_i)
    {
      auto &other_vertex = m_vertices[other_node_i];
      if (other_vertex.partition < vertex.partition)
	{
	  auto &other_partition = m_partitions[other_vertex.partition];
	  auto &other_costs = partition_layout_costs (other_vertex.partition,
						      other_partition.layout);
	  slpg_layout_cost this_cost = other_costs.in_cost;
	  this_cost.add_serial_cost (other_costs.internal_cost);
	  this_cost.split (other_partition.out_degree);
	  cost.add_parallel_cost (this_cost);
	}
    };
  for_each_partition_edge (node_i, add_cost);
  return cost;
}

/* Return the cost of switching between layout LAYOUT1_I (at node NODE1_I)
   and layout LAYOUT2_I on cross-partition use-to-def edge UD.  Return
   slpg_layout_cost::impossible () if the change isn't possible.  */

slpg_layout_cost
vect_optimize_slp_pass::
edge_layout_cost (graph_edge *ud, unsigned int node1_i, unsigned int layout1_i,
		  unsigned int layout2_i)
{
  auto &def_vertex = m_vertices[ud->dest];
  auto &use_vertex = m_vertices[ud->src];
  auto def_layout_i = ud->dest == int (node1_i) ? layout1_i : layout2_i;
  auto use_layout_i = ud->dest == int (node1_i) ? layout2_i : layout1_i;
  auto factor = change_layout_cost (def_vertex.node, def_layout_i,
				    use_layout_i);
  if (factor < 0)
    return slpg_layout_cost::impossible ();

  /* We have a choice of putting the layout change at the site of the
     definition or at the site of the use.  Prefer the former when
     optimizing for size or when the execution frequency of the
     definition is no greater than the combined execution frequencies of
     the uses.  When putting the layout change at the site of the definition,
     divvy up the cost among all consumers.  */
  if (m_optimize_size || def_vertex.weight <= def_vertex.out_weight)
    {
      slpg_layout_cost cost = { def_vertex.weight * factor, m_optimize_size };
      cost.split (def_vertex.out_degree);
      return cost;
    }
  return { use_vertex.weight * factor, m_optimize_size };
}

/* UD represents a use-def link between FROM_NODE_I and a node in a later
   partition; FROM_NODE_I could be the definition node or the use node.
   The node at the other end of the link wants to use layout TO_LAYOUT_I.
   Return the cost of any necessary fix-ups on edge UD, or return
   slpg_layout_cost::impossible () if the change isn't possible.

   At this point, FROM_NODE_I's partition has chosen the cheapest
   layout based on the information available so far, but this choice
   is only provisional.  */

slpg_layout_cost
vect_optimize_slp_pass::forward_cost (graph_edge *ud, unsigned int from_node_i,
				      unsigned int to_layout_i)
{
  auto &from_vertex = m_vertices[from_node_i];
  unsigned int from_partition_i = from_vertex.partition;
  slpg_partition_info &from_partition = m_partitions[from_partition_i];
  gcc_assert (from_partition.layout >= 0);

  /* First calculate the cost on the assumption that FROM_PARTITION sticks
     with its current layout preference.  */
  slpg_layout_cost cost = slpg_layout_cost::impossible ();
  auto edge_cost = edge_layout_cost (ud, from_node_i,
				     from_partition.layout, to_layout_i);
  if (edge_cost.is_possible ())
    {
      auto &from_costs = partition_layout_costs (from_partition_i,
						 from_partition.layout);
      cost = from_costs.in_cost;
      cost.add_serial_cost (from_costs.internal_cost);
      cost.split (from_partition.out_degree);
      cost.add_serial_cost (edge_cost);
    }
  else if (from_partition.layout == 0)
    /* We must allow the source partition to have layout 0 as a fallback,
       in case all other options turn out to be impossible.  */
    return cost;

  /* Take the minimum of that cost and the cost that applies if
     FROM_PARTITION instead switches to TO_LAYOUT_I.  */
  auto &direct_layout_costs = partition_layout_costs (from_partition_i,
						      to_layout_i);
  if (direct_layout_costs.is_possible ())
    {
      slpg_layout_cost direct_cost = direct_layout_costs.in_cost;
      direct_cost.add_serial_cost (direct_layout_costs.internal_cost);
      direct_cost.split (from_partition.out_degree);
      if (!cost.is_possible ()
	  || direct_cost.is_better_than (cost, m_optimize_size))
	cost = direct_cost;
    }

  return cost;
}

/* UD represents a use-def link between TO_NODE_I and a node in an earlier
   partition; TO_NODE_I could be the definition node or the use node.
   The node at the other end of the link wants to use layout FROM_LAYOUT_I;
   return the cost of any necessary fix-ups on edge UD, or
   slpg_layout_cost::impossible () if the choice cannot be made.

   At this point, TO_NODE_I's partition has a fixed choice of layout.  */

slpg_layout_cost
vect_optimize_slp_pass::backward_cost (graph_edge *ud, unsigned int to_node_i,
				       unsigned int from_layout_i)
{
  auto &to_vertex = m_vertices[to_node_i];
  unsigned int to_partition_i = to_vertex.partition;
  slpg_partition_info &to_partition = m_partitions[to_partition_i];
  gcc_assert (to_partition.layout >= 0);

  /* If TO_NODE_I is a VEC_PERM_EXPR consumer, see whether it can be
     adjusted for this input having layout FROM_LAYOUT_I.  Assume that
     any other inputs keep their current choice of layout.  */
  auto &to_costs = partition_layout_costs (to_partition_i,
					   to_partition.layout);
  if (ud->src == int (to_node_i)
      && SLP_TREE_CODE (to_vertex.node) == VEC_PERM_EXPR)
    {
      auto &from_partition = m_partitions[m_vertices[ud->dest].partition];
      auto old_layout = from_partition.layout;
      from_partition.layout = from_layout_i;
      int factor = internal_node_cost (to_vertex.node, -1,
				       to_partition.layout);
      from_partition.layout = old_layout;
      if (factor >= 0)
	{
	  slpg_layout_cost cost = to_costs.out_cost;
	  cost.add_serial_cost ({ to_vertex.weight * factor,
				  m_optimize_size });
	  cost.split (to_partition.in_degree);
	  return cost;
	}
    }

  /* Compute the cost if we insert any necessary layout change on edge UD.  */
  auto edge_cost = edge_layout_cost (ud, to_node_i,
				     to_partition.layout, from_layout_i);
  if (edge_cost.is_possible ())
    {
      slpg_layout_cost cost = to_costs.out_cost;
      cost.add_serial_cost (to_costs.internal_cost);
      cost.split (to_partition.in_degree);
      cost.add_serial_cost (edge_cost);
      return cost;
    }

  return slpg_layout_cost::impossible ();
}

/* Make a forward pass through the partitions, accumulating input costs.
   Make a tentative (provisional) choice of layout for each partition,
   ensuring that this choice still allows later partitions to keep
   their original layout.  */

void
vect_optimize_slp_pass::forward_pass ()
{
  for (unsigned int partition_i = 0; partition_i < m_partitions.length ();
       ++partition_i)
    {
      auto &partition = m_partitions[partition_i];

      /* If the partition consists of a single VEC_PERM_EXPR, precompute
	 the incoming cost that would apply if every predecessor partition
	 keeps its current layout.  This is used within the loop below.  */
      slpg_layout_cost in_cost;
      slp_tree single_node = nullptr;
      if (partition.node_end == partition.node_begin + 1)
	{
	  unsigned int node_i = m_partitioned_nodes[partition.node_begin];
	  single_node = m_vertices[node_i].node;
	  if (SLP_TREE_CODE (single_node) == VEC_PERM_EXPR)
	    in_cost = total_in_cost (node_i);
	}

      /* Go through the possible layouts.  Decide which ones are valid
	 for this partition and record which of the valid layouts has
	 the lowest cost.  */
      unsigned int min_layout_i = 0;
      slpg_layout_cost min_layout_cost = slpg_layout_cost::impossible ();
      for (unsigned int layout_i = 0; layout_i < m_perms.length (); ++layout_i)
	{
	  auto &layout_costs = partition_layout_costs (partition_i, layout_i);
	  if (!layout_costs.is_possible ())
	    continue;

	  /* If the recorded layout is already 0 then the layout cannot
	     change.  */
	  if (partition.layout == 0 && layout_i != 0)
	    {
	      layout_costs.mark_impossible ();
	      continue;
	    }

	  bool is_possible = true;
	  for (unsigned int order_i = partition.node_begin;
	       order_i < partition.node_end; ++order_i)
	    {
	      unsigned int node_i = m_partitioned_nodes[order_i];
	      auto &vertex = m_vertices[node_i];

	      /* Reject the layout if it is individually incompatible
		 with any node in the partition.  */
	      if (!is_compatible_layout (vertex.node, layout_i))
		{
		  is_possible = false;
		  break;
		}

	      auto add_cost = [&](graph_edge *ud, unsigned int other_node_i)
		{
		  auto &other_vertex = m_vertices[other_node_i];
		  if (other_vertex.partition < vertex.partition)
		    {
		      /* Accumulate the incoming costs from earlier
			 partitions, plus the cost of any layout changes
			 on UD itself.  */
		      auto cost = forward_cost (ud, other_node_i, layout_i);
		      if (!cost.is_possible ())
			is_possible = false;
		      else
			layout_costs.in_cost.add_parallel_cost (cost);
		    }
		  else
		    /* Reject the layout if it would make layout 0 impossible
		       for later partitions.  This amounts to testing that the
		       target supports reversing the layout change on edges
		       to later partitions.

		       In principle, it might be possible to push a layout
		       change all the way down a graph, so that it never
		       needs to be reversed and so that the target doesn't
		       need to support the reverse operation.  But it would
		       be awkward to bail out if we hit a partition that
		       does not support the new layout, especially since
		       we are not dealing with a lattice.  */
		    is_possible &= edge_layout_cost (ud, other_node_i, 0,
						     layout_i).is_possible ();
		};
	      for_each_partition_edge (node_i, add_cost);

	      /* Accumulate the cost of using LAYOUT_I within NODE,
		 both for the inputs and the outputs.  */
	      int factor = internal_node_cost (vertex.node, layout_i,
					       layout_i);
	      if (factor < 0)
		{
		  is_possible = false;
		  break;
		}
	      else if (factor)
		layout_costs.internal_cost.add_serial_cost
		  ({ vertex.weight * factor, m_optimize_size });
	    }
	  if (!is_possible)
	    {
	      layout_costs.mark_impossible ();
	      continue;
	    }

	  /* Combine the incoming and partition-internal costs.  */
	  slpg_layout_cost combined_cost = layout_costs.in_cost;
	  combined_cost.add_serial_cost (layout_costs.internal_cost);

	  /* If this partition consists of a single VEC_PERM_EXPR, see
	     if the VEC_PERM_EXPR can be changed to support output layout
	     LAYOUT_I while keeping all the provisional choices of input
	     layout.  */
	  if (single_node
	      && SLP_TREE_CODE (single_node) == VEC_PERM_EXPR)
	    {
	      int factor = internal_node_cost (single_node, -1, layout_i);
	      if (factor >= 0)
		{
		  auto weight = m_vertices[single_node->vertex].weight;
		  slpg_layout_cost internal_cost
		    = { weight * factor, m_optimize_size };

		  slpg_layout_cost alt_cost = in_cost;
		  alt_cost.add_serial_cost (internal_cost);
		  if (alt_cost.is_better_than (combined_cost, m_optimize_size))
		    {
		      combined_cost = alt_cost;
		      layout_costs.in_cost = in_cost;
		      layout_costs.internal_cost = internal_cost;
		    }
		}
	    }

	  /* Record the layout with the lowest cost.  Prefer layout 0 in
	     the event of a tie between it and another layout.  */
	  if (!min_layout_cost.is_possible ()
	      || combined_cost.is_better_than (min_layout_cost,
					       m_optimize_size))
	    {
	      min_layout_i = layout_i;
	      min_layout_cost = combined_cost;
	    }
	}

      /* This loop's handling of earlier partitions should ensure that
	 choosing the original layout for the current partition is no
	 less valid than it was in the original graph, even with the
	 provisional layout choices for those earlier partitions.  */
      gcc_assert (min_layout_cost.is_possible ());
      partition.layout = min_layout_i;
    }
}

/* Make a backward pass through the partitions, accumulating output costs.
   Make a final choice of layout for each partition.  */

void
vect_optimize_slp_pass::backward_pass ()
{
  for (unsigned int partition_i = m_partitions.length (); partition_i-- > 0;)
    {
      auto &partition = m_partitions[partition_i];

      unsigned int min_layout_i = 0;
      slpg_layout_cost min_layout_cost = slpg_layout_cost::impossible ();
      for (unsigned int layout_i = 0; layout_i < m_perms.length (); ++layout_i)
	{
	  auto &layout_costs = partition_layout_costs (partition_i, layout_i);
	  if (!layout_costs.is_possible ())
	    continue;

	  /* Accumulate the costs from successor partitions.  */
	  bool is_possible = true;
	  for (unsigned int order_i = partition.node_begin;
	       order_i < partition.node_end; ++order_i)
	    {
	      unsigned int node_i = m_partitioned_nodes[order_i];
	      auto &vertex = m_vertices[node_i];
	      auto add_cost = [&](graph_edge *ud, unsigned int other_node_i)
		{
		  auto &other_vertex = m_vertices[other_node_i];
		  auto &other_partition = m_partitions[other_vertex.partition];
		  if (other_vertex.partition > vertex.partition)
		    {
		      /* Accumulate the incoming costs from later
			 partitions, plus the cost of any layout changes
			 on UD itself.  */
		      auto cost = backward_cost (ud, other_node_i, layout_i);
		      if (!cost.is_possible ())
			is_possible = false;
		      else
			layout_costs.out_cost.add_parallel_cost (cost);
		    }
		  else
		    /* Make sure that earlier partitions can (if necessary
		       or beneficial) keep the layout that they chose in
		       the forward pass.  This ensures that there is at
		       least one valid choice of layout.  */
		    is_possible &= edge_layout_cost (ud, other_node_i,
						     other_partition.layout,
						     layout_i).is_possible ();
		};
	      for_each_partition_edge (node_i, add_cost);
	    }
	  if (!is_possible)
	    {
	      layout_costs.mark_impossible ();
	      continue;
	    }

	  /* Locally combine the costs from the forward and backward passes.
	     (This combined cost is not passed on, since that would lead
	     to double counting.)  */
	  slpg_layout_cost combined_cost = layout_costs.in_cost;
	  combined_cost.add_serial_cost (layout_costs.internal_cost);
	  combined_cost.add_serial_cost (layout_costs.out_cost);

	  /* Record the layout with the lowest cost.  Prefer layout 0 in
	     the event of a tie between it and another layout.  */
	  if (!min_layout_cost.is_possible ()
	      || combined_cost.is_better_than (min_layout_cost,
					       m_optimize_size))
	    {
	      min_layout_i = layout_i;
	      min_layout_cost = combined_cost;
	    }
	}

      gcc_assert (min_layout_cost.is_possible ());
      partition.layout = min_layout_i;
    }
}

/* Return a node that applies layout TO_LAYOUT_I to the original form of NODE.
   NODE already has the layout that was selected for its partition.  */

slp_tree
vect_optimize_slp_pass::get_result_with_layout (slp_tree node,
						unsigned int to_layout_i)
{
  unsigned int result_i = node->vertex * m_perms.length () + to_layout_i;
  slp_tree result = m_node_layouts[result_i];
  if (result)
    return result;

  if (SLP_TREE_DEF_TYPE (node) == vect_constant_def
      || (SLP_TREE_DEF_TYPE (node) == vect_external_def
	  /* We can't permute vector defs in place.  */
	  && SLP_TREE_VEC_DEFS (node).is_empty ()))
    {
      /* If the vector is uniform or unchanged, there's nothing to do.  */
      if (to_layout_i == 0 || vect_slp_tree_uniform_p (node))
	result = node;
      else
	{
	  auto scalar_ops = SLP_TREE_SCALAR_OPS (node).copy ();
	  result = vect_create_new_slp_node (scalar_ops);
	  vect_slp_permute (m_perms[to_layout_i], scalar_ops, true);
	}
    }
  else
    {
      unsigned int partition_i = m_vertices[node->vertex].partition;
      unsigned int from_layout_i = m_partitions[partition_i].layout;
      if (from_layout_i == to_layout_i)
	return node;

      /* If NODE is itself a VEC_PERM_EXPR, try to create a parallel
	 permutation instead of a serial one.  Leave the new permutation
	 in TMP_PERM on success.  */
      auto_lane_permutation_t tmp_perm;
      unsigned int num_inputs = 1;
      if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
	{
	  tmp_perm.safe_splice (SLP_TREE_LANE_PERMUTATION (node));
	  if (from_layout_i != 0)
	    vect_slp_permute (m_perms[from_layout_i], tmp_perm, false);
	  if (to_layout_i != 0)
	    vect_slp_permute (m_perms[to_layout_i], tmp_perm, true);
	  if (vectorizable_slp_permutation_1 (m_vinfo, nullptr, node,
					      tmp_perm,
					      SLP_TREE_CHILDREN (node),
					      false) >= 0)
	    num_inputs = SLP_TREE_CHILDREN (node).length ();
	  else
	    tmp_perm.truncate (0);
	}

      if (dump_enabled_p ())
	{
	  if (tmp_perm.length () > 0)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "duplicating permutation node %p with"
			     " layout %d\n",
			     (void *) node, to_layout_i);
	  else
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "inserting permutation node in place of %p\n",
			     (void *) node);
	}

      unsigned int num_lanes = SLP_TREE_LANES (node);
      result = vect_create_new_slp_node (num_inputs, VEC_PERM_EXPR);
      if (SLP_TREE_SCALAR_STMTS (node).length ())
	{
	  auto &stmts = SLP_TREE_SCALAR_STMTS (result);
	  stmts.safe_splice (SLP_TREE_SCALAR_STMTS (node));
	  if (from_layout_i != 0)
	    vect_slp_permute (m_perms[from_layout_i], stmts, false);
	  if (to_layout_i != 0)
	    vect_slp_permute (m_perms[to_layout_i], stmts, true);
	}
      SLP_TREE_REPRESENTATIVE (result) = SLP_TREE_REPRESENTATIVE (node);
      SLP_TREE_LANES (result) = num_lanes;
      SLP_TREE_VECTYPE (result) = SLP_TREE_VECTYPE (node);
      result->vertex = -1;

      auto &lane_perm = SLP_TREE_LANE_PERMUTATION (result);
      if (tmp_perm.length ())
	{
	  lane_perm.safe_splice (tmp_perm);
	  SLP_TREE_CHILDREN (result).safe_splice (SLP_TREE_CHILDREN (node));
	}
      else
	{
	  lane_perm.create (num_lanes);
	  for (unsigned j = 0; j < num_lanes; ++j)
	    lane_perm.quick_push ({ 0, j });
	  if (from_layout_i != 0)
	    vect_slp_permute (m_perms[from_layout_i], lane_perm, false);
	  if (to_layout_i != 0)
	    vect_slp_permute (m_perms[to_layout_i], lane_perm, true);
	  SLP_TREE_CHILDREN (result).safe_push (node);
	}
      for (slp_tree child : SLP_TREE_CHILDREN (result))
	child->refcnt++;
    }
  m_node_layouts[result_i] = result;
  return result;
}

/* Apply the chosen vector layouts to the SLP graph.  */

void
vect_optimize_slp_pass::materialize ()
{
  /* We no longer need the costs, so avoid having two O(N * P) arrays
     live at the same time.  */
  m_partition_layout_costs.release ();
  m_node_layouts.safe_grow_cleared (m_vertices.length () * m_perms.length ());

  auto_sbitmap fully_folded (m_vertices.length ());
  bitmap_clear (fully_folded);
  for (unsigned int node_i : m_partitioned_nodes)
    {
      auto &vertex = m_vertices[node_i];
      slp_tree node = vertex.node;
      int layout_i = m_partitions[vertex.partition].layout;
      gcc_assert (layout_i >= 0);

      /* Rearrange the scalar statements to match the chosen layout.  */
      if (layout_i > 0)
	vect_slp_permute (m_perms[layout_i],
			  SLP_TREE_SCALAR_STMTS (node), true);

      /* Update load and lane permutations.  */
      if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
	{
	  /* First try to absorb the input vector layouts.  If that fails,
	     force the inputs to have layout LAYOUT_I too.  We checked that
	     that was possible before deciding to use nonzero output layouts.
	     (Note that at this stage we don't really have any guarantee that
	     the target supports the original VEC_PERM_EXPR.)  */
	  auto &perm = SLP_TREE_LANE_PERMUTATION (node);
	  auto_lane_permutation_t tmp_perm;
	  tmp_perm.safe_splice (perm);
	  change_vec_perm_layout (node, tmp_perm, -1, layout_i);
	  if (vectorizable_slp_permutation_1 (m_vinfo, nullptr, node,
					      tmp_perm,
					      SLP_TREE_CHILDREN (node),
					      false) >= 0)
	    {
	      if (dump_enabled_p ()
		  && !std::equal (tmp_perm.begin (), tmp_perm.end (),
				  perm.begin ()))
		dump_printf_loc (MSG_NOTE, vect_location,
				 "absorbing input layouts into %p\n",
				 (void *) node);
	      std::copy (tmp_perm.begin (), tmp_perm.end (), perm.begin ());
	      bitmap_set_bit (fully_folded, node_i);
	    }
	  else
	    {
	      /* Not MSG_MISSED because it would make no sense to users.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "failed to absorb input layouts into %p\n",
				 (void *) node);
	      change_vec_perm_layout (nullptr, perm, layout_i, layout_i);
	    }
	}
      else
	{
	  gcc_assert (!SLP_TREE_LANE_PERMUTATION (node).exists ());
	  auto &load_perm = SLP_TREE_LOAD_PERMUTATION (node);
	  if (layout_i > 0)
	    /* ???  When we handle non-bijective permutes the idea
	       is that we can force the load-permutation to be
	       { min, min + 1, min + 2, ... max }.  But then the
	       scalar defs might no longer match the lane content
	       which means wrong-code with live lane vectorization.
	       So we possibly have to have NULL entries for those.  */
	    vect_slp_permute (m_perms[layout_i], load_perm, true);
	}
    }

  /* Do this before any nodes disappear, since it involves a walk
     over the leaves.  */
  remove_redundant_permutations ();

  /* Replace each child with a correctly laid-out version.  */
  for (unsigned int node_i : m_partitioned_nodes)
    {
      /* Skip nodes that have already been handled above.  */
      if (bitmap_bit_p (fully_folded, node_i))
	continue;

      auto &vertex = m_vertices[node_i];
      int in_layout_i = m_partitions[vertex.partition].layout;
      gcc_assert (in_layout_i >= 0);

      unsigned j;
      slp_tree child;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (vertex.node), j, child)
	{
	  if (!child)
	    continue;

	  slp_tree new_child = get_result_with_layout (child, in_layout_i);
	  if (new_child != child)
	    {
	      vect_free_slp_tree (child);
	      SLP_TREE_CHILDREN (vertex.node)[j] = new_child;
	      new_child->refcnt += 1;
	    }
	}
    }
}

/* Elide load permutations that are not necessary.  Such permutations might
   be pre-existing, rather than created by the layout optimizations.  */

void
vect_optimize_slp_pass::remove_redundant_permutations ()
{
  for (unsigned int node_i : m_leafs)
    {
      slp_tree node = m_vertices[node_i].node;
      if (!SLP_TREE_LOAD_PERMUTATION (node).exists ())
	continue;

      /* In basic block vectorization we allow any subchain of an interleaving
	 chain.
	 FORNOW: not in loop SLP because of realignment complications.  */
      if (is_a <bb_vec_info> (m_vinfo))
	{
	  bool subchain_p = true;
	  stmt_vec_info next_load_info = NULL;
	  stmt_vec_info load_info;
	  unsigned j;
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    {
	      if (j != 0
		  && (next_load_info != load_info
		      || ! load_info
		      || DR_GROUP_GAP (load_info) != 1))
		{
		  subchain_p = false;
		  break;
		}
	      next_load_info = DR_GROUP_NEXT_ELEMENT (load_info);
	    }
	  if (subchain_p)
	    {
	      SLP_TREE_LOAD_PERMUTATION (node).release ();
	      continue;
	    }
	}
      else
	{
	  loop_vec_info loop_vinfo = as_a<loop_vec_info> (m_vinfo);
	  stmt_vec_info load_info;
	  bool this_load_permuted = false;
	  unsigned j;
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    if (SLP_TREE_LOAD_PERMUTATION (node)[j] != j)
	      {
		this_load_permuted = true;
		break;
	      }
	  /* When this isn't a grouped access we know it's single element
	     and contiguous.  */
	  if (!STMT_VINFO_GROUPED_ACCESS (SLP_TREE_SCALAR_STMTS (node)[0]))
	    {
	      if (!this_load_permuted
		  && (known_eq (LOOP_VINFO_VECT_FACTOR (loop_vinfo), 1U)
		      || SLP_TREE_LANES (node) == 1))
		SLP_TREE_LOAD_PERMUTATION (node).release ();
	      continue;
	    }
	  stmt_vec_info first_stmt_info
	    = DR_GROUP_FIRST_ELEMENT (SLP_TREE_SCALAR_STMTS (node)[0]);
	  if (!this_load_permuted
	      /* The load requires permutation when unrolling exposes
		 a gap either because the group is larger than the SLP
		 group-size or because there is a gap between the groups.  */
	      && (known_eq (LOOP_VINFO_VECT_FACTOR (loop_vinfo), 1U)
		  || ((SLP_TREE_LANES (node) == DR_GROUP_SIZE (first_stmt_info))
		      && DR_GROUP_GAP (first_stmt_info) == 0)))
	    {
	      SLP_TREE_LOAD_PERMUTATION (node).release ();
	      continue;
	    }
	}
    }
}

/* Print the partition graph and layout information to the dump file.  */

void
vect_optimize_slp_pass::dump ()
{
  dump_printf_loc (MSG_NOTE, vect_location,
		   "SLP optimize permutations:\n");
  for (unsigned int layout_i = 1; layout_i < m_perms.length (); ++layout_i)
    {
      dump_printf_loc (MSG_NOTE, vect_location, "  %d: { ", layout_i);
      const char *sep = "";
      for (unsigned int idx : m_perms[layout_i])
	{
	  dump_printf (MSG_NOTE, "%s%d", sep, idx);
	  sep = ", ";
	}
      dump_printf (MSG_NOTE, " }\n");
    }
  dump_printf_loc (MSG_NOTE, vect_location,
		   "SLP optimize partitions:\n");
  for (unsigned int partition_i = 0; partition_i < m_partitions.length ();
       ++partition_i)
    {
      auto &partition = m_partitions[partition_i];
      dump_printf_loc (MSG_NOTE, vect_location,  "  -------------\n");
      dump_printf_loc (MSG_NOTE, vect_location,
		       "  partition %d (layout %d):\n",
		       partition_i, partition.layout);
      dump_printf_loc (MSG_NOTE, vect_location, "    nodes:\n");
      for (unsigned int order_i = partition.node_begin;
	   order_i < partition.node_end; ++order_i)
	{
	  auto &vertex = m_vertices[m_partitioned_nodes[order_i]];
	  dump_printf_loc (MSG_NOTE, vect_location, "      - %p:\n",
			   (void *) vertex.node);
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "          weight: %f\n",
			   vertex.weight.to_double ());
	  if (vertex.out_degree)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "          out weight: %f (degree %d)\n",
			     vertex.out_weight.to_double (),
			     vertex.out_degree);
	  if (SLP_TREE_CODE (vertex.node) == VEC_PERM_EXPR)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "          op: VEC_PERM_EXPR\n");
	  else if (auto rep = SLP_TREE_REPRESENTATIVE (vertex.node))
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "          op template: %G", rep->stmt);
	}
      dump_printf_loc (MSG_NOTE, vect_location, "    edges:\n");
      for (unsigned int order_i = partition.node_begin;
	   order_i < partition.node_end; ++order_i)
	{
	  unsigned int node_i = m_partitioned_nodes[order_i];
	  auto &vertex = m_vertices[node_i];
	  auto print_edge = [&](graph_edge *, unsigned int other_node_i)
	    {
	      auto &other_vertex = m_vertices[other_node_i];
	      if (other_vertex.partition < vertex.partition)
		dump_printf_loc (MSG_NOTE, vect_location,
				 "      - %p [%d] --> %p\n",
				 (void *) other_vertex.node,
				 other_vertex.partition,
				 (void *) vertex.node);
	      else
		dump_printf_loc (MSG_NOTE, vect_location,
				 "      - %p --> [%d] %p\n",
				 (void *) vertex.node,
				 other_vertex.partition,
				 (void *) other_vertex.node);
	    };
	  for_each_partition_edge (node_i, print_edge);
	}

      for (unsigned int layout_i = 0; layout_i < m_perms.length (); ++layout_i)
	{
	  auto &layout_costs = partition_layout_costs (partition_i, layout_i);
	  if (layout_costs.is_possible ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "    layout %d:%s\n", layout_i,
			       partition.layout == int (layout_i)
			       ? " (*)" : "");
	      slpg_layout_cost combined_cost = layout_costs.in_cost;
	      combined_cost.add_serial_cost (layout_costs.internal_cost);
	      combined_cost.add_serial_cost (layout_costs.out_cost);
#define TEMPLATE "{depth: %f, total: %f}"
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "        " TEMPLATE "\n",
			       layout_costs.in_cost.depth.to_double (),
			       layout_costs.in_cost.total.to_double ());
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "      + " TEMPLATE "\n",
			       layout_costs.internal_cost.depth.to_double (),
			       layout_costs.internal_cost.total.to_double ());
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "      + " TEMPLATE "\n",
			       layout_costs.out_cost.depth.to_double (),
			       layout_costs.out_cost.total.to_double ());
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "      = " TEMPLATE "\n",
			       combined_cost.depth.to_double (),
			       combined_cost.total.to_double ());
#undef TEMPLATE
	    }
	  else
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "    layout %d: rejected\n", layout_i);
	}
    }
}

/* Main entry point for the SLP graph optimization pass.  */

void
vect_optimize_slp_pass::run ()
{
  build_graph ();
  create_partitions ();
  start_choosing_layouts ();
  if (m_perms.length () > 1)
    {
      forward_pass ();
      backward_pass ();
      if (dump_enabled_p ())
	dump ();
      materialize ();
      while (!m_perms.is_empty ())
	m_perms.pop ().release ();
    }
  else
    remove_redundant_permutations ();
  free_graph (m_slpg);
}

/* Apply CSE to NODE and its children using BST_MAP.  */

static void
vect_cse_slp_nodes (scalar_stmts_to_slp_tree_map_t *bst_map, slp_tree& node)
{
  bool put_p = false;
  if (SLP_TREE_DEF_TYPE (node) == vect_internal_def
      /* Besides some VEC_PERM_EXPR, two-operator nodes also
	 lack scalar stmts and thus CSE doesn't work via bst_map.  Ideally
	 we'd have sth that works for all internal and external nodes.  */
      && !SLP_TREE_SCALAR_STMTS (node).is_empty ())
    {
      slp_tree *leader = bst_map->get (SLP_TREE_SCALAR_STMTS (node));
      if (leader)
	{
	  /* We've visited this node already.  */
	  if (!*leader || *leader == node)
	    return;

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "re-using SLP tree %p for %p\n",
			     (void *)*leader, (void *)node);
	  vect_free_slp_tree (node);
	  (*leader)->refcnt += 1;
	  node = *leader;
	  return;
	}

      /* Avoid creating a cycle by populating the map only after recursion.  */
      bst_map->put (SLP_TREE_SCALAR_STMTS (node).copy (), nullptr);
      node->refcnt += 1;
      put_p = true;
      /* And recurse.  */
    }

  for (slp_tree &child : SLP_TREE_CHILDREN (node))
    if (child)
      vect_cse_slp_nodes (bst_map, child);

  /* Now record the node for CSE in other siblings.  */
  if (put_p)
    *bst_map->get (SLP_TREE_SCALAR_STMTS (node)) = node;
}

/* Optimize the SLP graph of VINFO.  */

void
vect_optimize_slp (vec_info *vinfo)
{
  if (vinfo->slp_instances.is_empty ())
    return;
  vect_optimize_slp_pass (vinfo).run ();

  /* Apply CSE again to nodes after permute optimization.  */
  scalar_stmts_to_slp_tree_map_t *bst_map
    = new scalar_stmts_to_slp_tree_map_t ();

  for (auto inst : vinfo->slp_instances)
    vect_cse_slp_nodes (bst_map, SLP_INSTANCE_TREE (inst));

  release_scalar_stmts_to_slp_tree_map (bst_map);
}

/* Gather loads reachable from the individual SLP graph entries.  */

void
vect_gather_slp_loads (vec_info *vinfo)
{
  unsigned i;
  slp_instance instance;
  FOR_EACH_VEC_ELT (vinfo->slp_instances, i, instance)
    {
      hash_set<slp_tree> visited;
      vect_gather_slp_loads (SLP_INSTANCE_LOADS (instance),
			     SLP_INSTANCE_TREE (instance), visited);
    }
}


/* For each possible SLP instance decide whether to SLP it and calculate overall
   unrolling factor needed to SLP the loop.  Return TRUE if decided to SLP at
   least one instance.  */

bool
vect_make_slp_decision (loop_vec_info loop_vinfo)
{
  unsigned int i;
  poly_uint64 unrolling_factor = 1;
  const vec<slp_instance> &slp_instances
    = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
  slp_instance instance;
  int decided_to_slp = 0;

  DUMP_VECT_SCOPE ("vect_make_slp_decision");

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      /* FORNOW: SLP if you can.  */
      /* All unroll factors have the form:

	   GET_MODE_SIZE (vinfo->vector_mode) * X

	 for some rational X, so they must have a common multiple.  */
      unrolling_factor
	= force_common_multiple (unrolling_factor,
				 SLP_INSTANCE_UNROLLING_FACTOR (instance));

      /* Mark all the stmts that belong to INSTANCE as PURE_SLP stmts.  Later we
	 call vect_detect_hybrid_slp () to find stmts that need hybrid SLP and
	 loop-based vectorization.  Such stmts will be marked as HYBRID.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance));
      decided_to_slp++;
    }

  LOOP_VINFO_SLP_UNROLLING_FACTOR (loop_vinfo) = unrolling_factor;

  if (decided_to_slp && dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "Decided to SLP %d instances. Unrolling factor ",
		       decided_to_slp);
      dump_dec (MSG_NOTE, unrolling_factor);
      dump_printf (MSG_NOTE, "\n");
    }

  return (decided_to_slp > 0);
}

/* Private data for vect_detect_hybrid_slp.  */
struct vdhs_data
{
  loop_vec_info loop_vinfo;
  vec<stmt_vec_info> *worklist;
};

/* Walker for walk_gimple_op.  */

static tree
vect_detect_hybrid_slp (tree *tp, int *, void *data)
{
  walk_stmt_info *wi = (walk_stmt_info *)data;
  vdhs_data *dat = (vdhs_data *)wi->info;

  if (wi->is_lhs)
    return NULL_TREE;

  stmt_vec_info def_stmt_info = dat->loop_vinfo->lookup_def (*tp);
  if (!def_stmt_info)
    return NULL_TREE;
  def_stmt_info = vect_stmt_to_vectorize (def_stmt_info);
  if (PURE_SLP_STMT (def_stmt_info))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "marking hybrid: %G",
			 def_stmt_info->stmt);
      STMT_SLP_TYPE (def_stmt_info) = hybrid;
      dat->worklist->safe_push (def_stmt_info);
    }

  return NULL_TREE;
}

/* Look if STMT_INFO is consumed by SLP indirectly and mark it pure_slp
   if so, otherwise pushing it to WORKLIST.  */

static void
maybe_push_to_hybrid_worklist (vec_info *vinfo,
			       vec<stmt_vec_info> &worklist,
			       stmt_vec_info stmt_info)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Processing hybrid candidate : %G", stmt_info->stmt);
  stmt_vec_info orig_info = vect_orig_stmt (stmt_info);
  imm_use_iterator iter2;
  ssa_op_iter iter1;
  use_operand_p use_p;
  def_operand_p def_p;
  bool any_def = false;
  FOR_EACH_PHI_OR_STMT_DEF (def_p, orig_info->stmt, iter1, SSA_OP_DEF)
    {
      any_def = true;
      FOR_EACH_IMM_USE_FAST (use_p, iter2, DEF_FROM_PTR (def_p))
	{
	  if (is_gimple_debug (USE_STMT (use_p)))
	    continue;
	  stmt_vec_info use_info = vinfo->lookup_stmt (USE_STMT (use_p));
	  /* An out-of loop use means this is a loop_vect sink.  */
	  if (!use_info)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Found loop_vect sink: %G", stmt_info->stmt);
	      worklist.safe_push (stmt_info);
	      return;
	    }
	  else if (!STMT_SLP_TYPE (vect_stmt_to_vectorize (use_info)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Found loop_vect use: %G", use_info->stmt);
	      worklist.safe_push (stmt_info);
	      return;
	    }
	}
    }
  /* No def means this is a loop_vect sink.  Gimple conditionals also don't have a
     def but shouldn't be considered sinks.  */
  if (!any_def && STMT_VINFO_DEF_TYPE (stmt_info) != vect_condition_def)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Found loop_vect sink: %G", stmt_info->stmt);
      worklist.safe_push (stmt_info);
      return;
    }
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Marked SLP consumed stmt pure: %G", stmt_info->stmt);
  STMT_SLP_TYPE (stmt_info) = pure_slp;
}

/* Find stmts that must be both vectorized and SLPed.  */

void
vect_detect_hybrid_slp (loop_vec_info loop_vinfo)
{
  DUMP_VECT_SCOPE ("vect_detect_hybrid_slp");

  /* All stmts participating in SLP are marked pure_slp, all other
     stmts are loop_vect.
     First collect all loop_vect stmts into a worklist.
     SLP patterns cause not all original scalar stmts to appear in
     SLP_TREE_SCALAR_STMTS and thus not all of them are marked pure_slp.
     Rectify this here and do a backward walk over the IL only considering
     stmts as loop_vect when they are used by a loop_vect stmt and otherwise
     mark them as pure_slp.  */
  auto_vec<stmt_vec_info> worklist;
  for (int i = LOOP_VINFO_LOOP (loop_vinfo)->num_nodes - 1; i >= 0; --i)
    {
      basic_block bb = LOOP_VINFO_BBS (loop_vinfo)[i];
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (phi);
	  if (!STMT_SLP_TYPE (stmt_info) && STMT_VINFO_RELEVANT (stmt_info))
	    maybe_push_to_hybrid_worklist (loop_vinfo,
					   worklist, stmt_info);
	}
      for (gimple_stmt_iterator gsi = gsi_last_bb (bb); !gsi_end_p (gsi);
	   gsi_prev (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt))
	    continue;
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (stmt);
	  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
	    {
	      for (gimple_stmt_iterator gsi2
		     = gsi_start (STMT_VINFO_PATTERN_DEF_SEQ (stmt_info));
		   !gsi_end_p (gsi2); gsi_next (&gsi2))
		{
		  stmt_vec_info patt_info
		    = loop_vinfo->lookup_stmt (gsi_stmt (gsi2));
		  if (!STMT_SLP_TYPE (patt_info)
		      && STMT_VINFO_RELEVANT (patt_info))
		    maybe_push_to_hybrid_worklist (loop_vinfo,
						   worklist, patt_info);
		}
	      stmt_info = STMT_VINFO_RELATED_STMT (stmt_info);
	    }
	  if (!STMT_SLP_TYPE (stmt_info) && STMT_VINFO_RELEVANT (stmt_info))
	    maybe_push_to_hybrid_worklist (loop_vinfo,
					   worklist, stmt_info);
	}
    }

  /* Now we have a worklist of non-SLP stmts, follow use->def chains and
     mark any SLP vectorized stmt as hybrid.
     ???  We're visiting def stmts N times (once for each non-SLP and
     once for each hybrid-SLP use).  */
  walk_stmt_info wi;
  vdhs_data dat;
  dat.worklist = &worklist;
  dat.loop_vinfo = loop_vinfo;
  memset (&wi, 0, sizeof (wi));
  wi.info = (void *)&dat;
  while (!worklist.is_empty ())
    {
      stmt_vec_info stmt_info = worklist.pop ();
      /* Since SSA operands are not set up for pattern stmts we need
	 to use walk_gimple_op.  */
      wi.is_lhs = 0;
      walk_gimple_op (stmt_info->stmt, vect_detect_hybrid_slp, &wi);
      /* For gather/scatter make sure to walk the offset operand, that
	 can be a scaling and conversion away.  */
      gather_scatter_info gs_info;
      if (STMT_VINFO_GATHER_SCATTER_P (stmt_info)
	  && vect_check_gather_scatter (stmt_info, loop_vinfo, &gs_info))
	{
	  int dummy;
	  vect_detect_hybrid_slp (&gs_info.offset, &dummy, &wi);
	}
    }
}


/* Initialize a bb_vec_info struct for the statements in BBS basic blocks.  */

_bb_vec_info::_bb_vec_info (vec<basic_block> _bbs, vec_info_shared *shared)
  : vec_info (vec_info::bb, shared),
    roots (vNULL)
{
  /* The region we are operating on.  bbs[0] is the entry, excluding
     its PHI nodes.  In the future we might want to track an explicit
     entry edge to cover bbs[0] PHI nodes and have a region entry
     insert location.  */
  bbs = _bbs.address ();
  nbbs = _bbs.length ();

  for (unsigned i = 0; i < nbbs; ++i)
    {
      if (i != 0)
	for (gphi_iterator si = gsi_start_phis (bbs[i]); !gsi_end_p (si);
	     gsi_next (&si))
	  {
	    gphi *phi = si.phi ();
	    gimple_set_uid (phi, 0);
	    add_stmt (phi);
	  }
      for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, 0);
	  if (is_gimple_debug (stmt))
	    continue;
	  add_stmt (stmt);
	}
    }
}


/* Free BB_VINFO struct, as well as all the stmt_vec_info structs of all the
   stmts in the basic block.  */

_bb_vec_info::~_bb_vec_info ()
{
  /* Reset region marker.  */
  for (unsigned i = 0; i < nbbs; ++i)
    {
      if (i != 0)
	for (gphi_iterator si = gsi_start_phis (bbs[i]); !gsi_end_p (si);
	     gsi_next (&si))
	  {
	    gphi *phi = si.phi ();
	    gimple_set_uid (phi, -1);
	  }
      for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, -1);
	}
    }

  for (unsigned i = 0; i < roots.length (); ++i)
    {
      roots[i].stmts.release ();
      roots[i].roots.release ();
      roots[i].remain.release ();
    }
  roots.release ();
}

/* Subroutine of vect_slp_analyze_node_operations.  Handle the root of NODE,
   given then that child nodes have already been processed, and that
   their def types currently match their SLP node's def type.  */

static bool
vect_slp_analyze_node_operations_1 (vec_info *vinfo, slp_tree node,
				    slp_instance node_instance,
				    stmt_vector_for_cost *cost_vec)
{
  stmt_vec_info stmt_info = SLP_TREE_REPRESENTATIVE (node);

  /* Calculate the number of vector statements to be created for the scalar
     stmts in this node.  It is the number of scalar elements in one scalar
     iteration (DR_GROUP_SIZE) multiplied by VF divided by the number of
     elements in a vector.  For single-defuse-cycle, lane-reducing op, and
     PHI statement that starts reduction comprised of only lane-reducing ops,
     the number is more than effective vector statements actually required.  */
  SLP_TREE_NUMBER_OF_VEC_STMTS (node) = vect_get_num_copies (vinfo, node);

  /* Handle purely internal nodes.  */
  if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
    {
      if (!vectorizable_slp_permutation (vinfo, NULL, node, cost_vec))
	return false;

      stmt_vec_info slp_stmt_info;
      unsigned int i;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, slp_stmt_info)
	{
	  if (slp_stmt_info
	      && STMT_VINFO_LIVE_P (slp_stmt_info)
	      && !vectorizable_live_operation (vinfo, slp_stmt_info, node,
					       node_instance, i,
					       false, cost_vec))
	    return false;
	}
      return true;
    }

  bool dummy;
  return vect_analyze_stmt (vinfo, stmt_info, &dummy,
			    node, node_instance, cost_vec);
}

/* Try to build NODE from scalars, returning true on success.
   NODE_INSTANCE is the SLP instance that contains NODE.  */

static bool
vect_slp_convert_to_external (vec_info *vinfo, slp_tree node,
			      slp_instance node_instance)
{
  stmt_vec_info stmt_info;
  unsigned int i;

  if (!is_a <bb_vec_info> (vinfo)
      || node == SLP_INSTANCE_TREE (node_instance)
      || !SLP_TREE_SCALAR_STMTS (node).exists ()
      || vect_contains_pattern_stmt_p (SLP_TREE_SCALAR_STMTS (node))
      /* Force the mask use to be built from scalars instead.  */
      || VECTOR_BOOLEAN_TYPE_P (SLP_TREE_VECTYPE (node)))
    return false;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    if (!stmt_info)
      return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Building vector operands of %p from scalars instead\n",
		     (void *) node);

  /* Don't remove and free the child nodes here, since they could be
     referenced by other structures.  The analysis and scheduling phases
     (need to) ignore child nodes of anything that isn't vect_internal_def.  */
  unsigned int group_size = SLP_TREE_LANES (node);
  SLP_TREE_DEF_TYPE (node) = vect_external_def;
  /* Invariants get their vector type from the uses.  */
  SLP_TREE_VECTYPE (node) = NULL_TREE;
  SLP_TREE_SCALAR_OPS (node).safe_grow (group_size, true);
  SLP_TREE_LOAD_PERMUTATION (node).release ();
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      tree lhs = gimple_get_lhs (vect_orig_stmt (stmt_info)->stmt);
      SLP_TREE_SCALAR_OPS (node)[i] = lhs;
    }
  return true;
}

/* Return true if all elements of the slice are the same.  */
bool
vect_scalar_ops_slice::all_same_p () const
{
  for (unsigned int i = 1; i < length; ++i)
    if (!operand_equal_p (op (0), op (i)))
      return false;
  return true;
}

hashval_t
vect_scalar_ops_slice_hash::hash (const value_type &s)
{
  hashval_t hash = 0;
  for (unsigned i = 0; i < s.length; ++i)
    hash = iterative_hash_expr (s.op (i), hash);
  return hash;
}

bool
vect_scalar_ops_slice_hash::equal (const value_type &s1,
				   const compare_type &s2)
{
  if (s1.length != s2.length)
    return false;
  for (unsigned i = 0; i < s1.length; ++i)
    if (!operand_equal_p (s1.op (i), s2.op (i)))
      return false;
  return true;
}

/* Compute the prologue cost for invariant or constant operands represented
   by NODE.  */

static void
vect_prologue_cost_for_slp (slp_tree node,
			    stmt_vector_for_cost *cost_vec)
{
  /* There's a special case of an existing vector, that costs nothing.  */
  if (SLP_TREE_SCALAR_OPS (node).length () == 0
      && !SLP_TREE_VEC_DEFS (node).is_empty ())
    return;
  /* Without looking at the actual initializer a vector of
     constants can be implemented as load from the constant pool.
     When all elements are the same we can use a splat.  */
  tree vectype = SLP_TREE_VECTYPE (node);
  unsigned group_size = SLP_TREE_SCALAR_OPS (node).length ();
  unsigned HOST_WIDE_INT const_nunits;
  unsigned nelt_limit;
  auto ops = &SLP_TREE_SCALAR_OPS (node);
  auto_vec<unsigned int> starts (SLP_TREE_NUMBER_OF_VEC_STMTS (node));
  if (TYPE_VECTOR_SUBPARTS (vectype).is_constant (&const_nunits)
      && ! multiple_p (const_nunits, group_size))
    {
      nelt_limit = const_nunits;
      hash_set<vect_scalar_ops_slice_hash> vector_ops;
      for (unsigned int i = 0; i < SLP_TREE_NUMBER_OF_VEC_STMTS (node); ++i)
	if (!vector_ops.add ({ ops, i * nelt_limit, nelt_limit }))
	  starts.quick_push (i * nelt_limit);
    }
  else
    {
      /* If either the vector has variable length or the vectors
	 are composed of repeated whole groups we only need to
	 cost construction once.  All vectors will be the same.  */
      nelt_limit = group_size;
      starts.quick_push (0);
    }
  /* ???  We're just tracking whether vectors in a single node are the same.
     Ideally we'd do something more global.  */
  bool passed = false;
  for (unsigned int start : starts)
    {
      vect_cost_for_stmt kind;
      if (SLP_TREE_DEF_TYPE (node) == vect_constant_def)
	kind = vector_load;
      else if (vect_scalar_ops_slice { ops, start, nelt_limit }.all_same_p ())
	kind = scalar_to_vec;
      else
	kind = vec_construct;
      /* The target cost hook has no idea which part of the SLP node
	 we are costing so avoid passing it down more than once.  Pass
	 it to the first vec_construct or scalar_to_vec part since for those
	 the x86 backend tries to account for GPR to XMM register moves.  */
      record_stmt_cost (cost_vec, 1, kind,
			(kind != vector_load && !passed) ? node : nullptr,
			vectype, 0, vect_prologue);
      if (kind != vector_load)
	passed = true;
    }
}

/* Analyze statements contained in SLP tree NODE after recursively analyzing
   the subtree.  NODE_INSTANCE contains NODE and VINFO contains INSTANCE.

   Return true if the operations are supported.  */

static bool
vect_slp_analyze_node_operations (vec_info *vinfo, slp_tree node,
				  slp_instance node_instance,
				  hash_set<slp_tree> &visited_set,
				  vec<slp_tree> &visited_vec,
				  stmt_vector_for_cost *cost_vec)
{
  int i, j;
  slp_tree child;

  /* Assume we can code-generate all invariants.  */
  if (!node
      || SLP_TREE_DEF_TYPE (node) == vect_constant_def
      || SLP_TREE_DEF_TYPE (node) == vect_external_def)
    return true;

  if (SLP_TREE_DEF_TYPE (node) == vect_uninitialized_def)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Failed cyclic SLP reference in %p\n", (void *) node);
      return false;
    }
  gcc_assert (SLP_TREE_DEF_TYPE (node) == vect_internal_def);

  /* If we already analyzed the exact same set of scalar stmts we're done.
     We share the generated vector stmts for those.  */
  if (visited_set.add (node))
    return true;
  visited_vec.safe_push (node);

  bool res = true;
  unsigned visited_rec_start = visited_vec.length ();
  unsigned cost_vec_rec_start = cost_vec->length ();
  bool seen_non_constant_child = false;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    {
      res = vect_slp_analyze_node_operations (vinfo, child, node_instance,
					      visited_set, visited_vec,
					      cost_vec);
      if (!res)
	break;
      if (child && SLP_TREE_DEF_TYPE (child) != vect_constant_def)
	seen_non_constant_child = true;
    }
  /* We're having difficulties scheduling nodes with just constant
     operands and no scalar stmts since we then cannot compute a stmt
     insertion place.  */
  if (res
      && !seen_non_constant_child
      && SLP_TREE_SCALAR_STMTS (node).is_empty ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Cannot vectorize all-constant op node %p\n",
			 (void *) node);
      res = false;
    }

  if (res)
    res = vect_slp_analyze_node_operations_1 (vinfo, node, node_instance,
					      cost_vec);
  /* If analysis failed we have to pop all recursive visited nodes
     plus ourselves.  */
  if (!res)
    {
      while (visited_vec.length () >= visited_rec_start)
	visited_set.remove (visited_vec.pop ());
      cost_vec->truncate (cost_vec_rec_start);
    }

  /* When the node can be vectorized cost invariant nodes it references.
     This is not done in DFS order to allow the refering node
     vectorizable_* calls to nail down the invariant nodes vector type
     and possibly unshare it if it needs a different vector type than
     other referrers.  */
  if (res)
    FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
      if (child
	  && (SLP_TREE_DEF_TYPE (child) == vect_constant_def
	      || SLP_TREE_DEF_TYPE (child) == vect_external_def)
	  /* Perform usual caching, note code-generation still
	     code-gens these nodes multiple times but we expect
	     to CSE them later.  */
	  && !visited_set.add (child))
	{
	  visited_vec.safe_push (child);
	  /* ???  After auditing more code paths make a "default"
	     and push the vector type from NODE to all children
	     if it is not already set.  */
	  /* Compute the number of vectors to be generated.  */
	  tree vector_type = SLP_TREE_VECTYPE (child);
	  if (!vector_type)
	    {
	      /* For shifts with a scalar argument we don't need
		 to cost or code-generate anything.
		 ???  Represent this more explicitely.  */
	      gcc_assert ((STMT_VINFO_TYPE (SLP_TREE_REPRESENTATIVE (node))
			   == shift_vec_info_type)
			  && j == 1);
	      continue;
	    }

	  SLP_TREE_NUMBER_OF_VEC_STMTS (child)
		= vect_get_num_copies (vinfo, child);
	  /* And cost them.  */
	  vect_prologue_cost_for_slp (child, cost_vec);
	}

  /* If this node or any of its children can't be vectorized, try pruning
     the tree here rather than felling the whole thing.  */
  if (!res && vect_slp_convert_to_external (vinfo, node, node_instance))
    {
      /* We'll need to revisit this for invariant costing and number
	 of vectorized stmt setting.   */
      res = true;
    }

  return res;
}

/* Given a definition DEF, analyze if it will have any live scalar use after
   performing SLP vectorization whose information is represented by BB_VINFO,
   and record result into hash map SCALAR_USE_MAP as cache for later fast
   check.  If recursion DEPTH exceeds a limit, stop analysis and make a
   conservative assumption.  Return 0 if no scalar use, 1 if there is, -1
   means recursion is limited.  */

static int
vec_slp_has_scalar_use (bb_vec_info bb_vinfo, tree def,
			hash_map<tree, int> &scalar_use_map,
			int depth = 0)
{
  const int depth_limit = 2;
  imm_use_iterator use_iter;
  gimple *use_stmt;

  if (int *res = scalar_use_map.get (def))
    return *res;

  int scalar_use = 1;

  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, def)
    {
      if (is_gimple_debug (use_stmt))
	continue;

      stmt_vec_info use_stmt_info = bb_vinfo->lookup_stmt (use_stmt);

      if (!use_stmt_info)
	break;

      if (PURE_SLP_STMT (vect_stmt_to_vectorize (use_stmt_info)))
	continue;

      /* Do not step forward when encounter PHI statement, since it may
	 involve cyclic reference and cause infinite recursive invocation.  */
      if (gimple_code (use_stmt) == GIMPLE_PHI)
	break;

      /* When pattern recognition is involved, a statement whose definition is
	 consumed in some pattern, may not be included in the final replacement
	 pattern statements, so would be skipped when building SLP graph.

	 * Original
	  char a_c = *(char *) a;
	  char b_c = *(char *) b;
	  unsigned short a_s = (unsigned short) a_c;
	  int a_i = (int) a_s;
	  int b_i = (int) b_c;
	  int r_i = a_i - b_i;

	 * After pattern replacement
	  a_s = (unsigned short) a_c;
	  a_i = (int) a_s;

	  patt_b_s = (unsigned short) b_c;    // b_i = (int) b_c
	  patt_b_i = (int) patt_b_s;          // b_i = (int) b_c

	  patt_r_s = widen_minus(a_c, b_c);   // r_i = a_i - b_i
	  patt_r_i = (int) patt_r_s;          // r_i = a_i - b_i

	 The definitions of a_i(original statement) and b_i(pattern statement)
	 are related to, but actually not part of widen_minus pattern.
	 Vectorizing the pattern does not cause these definition statements to
	 be marked as PURE_SLP.  For this case, we need to recursively check
	 whether their uses are all absorbed into vectorized code.  But there
	 is an exception that some use may participate in an vectorized
	 operation via an external SLP node containing that use as an element.
	 The parameter "scalar_use_map" tags such kind of SSA as having scalar
	 use in advance.  */
      tree lhs = gimple_get_lhs (use_stmt);

      if (!lhs || TREE_CODE (lhs) != SSA_NAME)
	break;

      if (depth_limit && depth >= depth_limit)
	return -1;

      if ((scalar_use = vec_slp_has_scalar_use (bb_vinfo, lhs, scalar_use_map,
						depth + 1)))
	break;
    }

  if (end_imm_use_stmt_p (&use_iter))
    scalar_use = 0;

  /* If recursion is limited, do not cache result for non-root defs.  */
  if (!depth || scalar_use >= 0)
    {
      bool added = scalar_use_map.put (def, scalar_use);
      gcc_assert (!added);
    }

  return scalar_use;
}

/* Mark lanes of NODE that are live outside of the basic-block vectorized
   region and that can be vectorized using vectorizable_live_operation
   with STMT_VINFO_LIVE_P.  Not handled live operations will cause the
   scalar code computing it to be retained.  */

static void
vect_bb_slp_mark_live_stmts (bb_vec_info bb_vinfo, slp_tree node,
			     slp_instance instance,
			     stmt_vector_for_cost *cost_vec,
			     hash_map<tree, int> &scalar_use_map,
			     hash_set<stmt_vec_info> &svisited,
			     hash_set<slp_tree> &visited)
{
  if (visited.add (node))
    return;

  unsigned i;
  stmt_vec_info stmt_info;
  stmt_vec_info last_stmt = vect_find_last_scalar_stmt_in_slp (node);
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      if (!stmt_info || svisited.contains (stmt_info))
	continue;
      stmt_vec_info orig_stmt_info = vect_orig_stmt (stmt_info);
      if (STMT_VINFO_IN_PATTERN_P (orig_stmt_info)
	  && STMT_VINFO_RELATED_STMT (orig_stmt_info) != stmt_info)
	/* Only the pattern root stmt computes the original scalar value.  */
	continue;
      bool mark_visited = true;
      gimple *orig_stmt = orig_stmt_info->stmt;
      ssa_op_iter op_iter;
      def_operand_p def_p;
      FOR_EACH_PHI_OR_STMT_DEF (def_p, orig_stmt, op_iter, SSA_OP_DEF)
	{
	  if (vec_slp_has_scalar_use (bb_vinfo, DEF_FROM_PTR (def_p),
				      scalar_use_map))
	    {
	      STMT_VINFO_LIVE_P (stmt_info) = true;
	      if (vectorizable_live_operation (bb_vinfo, stmt_info, node,
					       instance, i, false, cost_vec))
		/* ???  So we know we can vectorize the live stmt from one SLP
		   node.  If we cannot do so from all or none consistently
		   we'd have to record which SLP node (and lane) we want to
		   use for the live operation.  So make sure we can
		   code-generate from all nodes.  */
		mark_visited = false;
	      else
		STMT_VINFO_LIVE_P (stmt_info) = false;
	    }

	  /* We have to verify whether we can insert the lane extract
	     before all uses.  The following is a conservative approximation.
	     We cannot put this into vectorizable_live_operation because
	     iterating over all use stmts from inside a FOR_EACH_IMM_USE_STMT
	     doesn't work.
	     Note that while the fact that we emit code for loads at the
	     first load should make this a non-problem leafs we construct
	     from scalars are vectorized after the last scalar def.
	     ???  If we'd actually compute the insert location during
	     analysis we could use sth less conservative than the last
	     scalar stmt in the node for the dominance check.  */
	  /* ???  What remains is "live" uses in vector CTORs in the same
	     SLP graph which is where those uses can end up code-generated
	     right after their definition instead of close to their original
	     use.  But that would restrict us to code-generate lane-extracts
	     from the latest stmt in a node.  So we compensate for this
	     during code-generation, simply not replacing uses for those
	     hopefully rare cases.  */
	  imm_use_iterator use_iter;
	  gimple *use_stmt;
	  stmt_vec_info use_stmt_info;

	  if (STMT_VINFO_LIVE_P (stmt_info))
	    FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, DEF_FROM_PTR (def_p))
	      if (!is_gimple_debug (use_stmt)
		  && (!(use_stmt_info = bb_vinfo->lookup_stmt (use_stmt))
		      || !PURE_SLP_STMT (vect_stmt_to_vectorize (use_stmt_info)))
		  && !vect_stmt_dominates_stmt_p (last_stmt->stmt, use_stmt))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Cannot determine insertion place for "
				     "lane extract\n");
		  STMT_VINFO_LIVE_P (stmt_info) = false;
		  mark_visited = true;
		}
	}
      if (mark_visited)
	svisited.add (stmt_info);
    }

  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child && SLP_TREE_DEF_TYPE (child) == vect_internal_def)
      vect_bb_slp_mark_live_stmts (bb_vinfo, child, instance, cost_vec,
				   scalar_use_map, svisited, visited);
}

/* Traverse all slp instances of BB_VINFO, and mark lanes of every node that
   are live outside of the basic-block vectorized region and that can be
   vectorized using vectorizable_live_operation with STMT_VINFO_LIVE_P.  */

static void
vect_bb_slp_mark_live_stmts (bb_vec_info bb_vinfo)
{
  if (bb_vinfo->slp_instances.is_empty ())
    return;

  hash_set<stmt_vec_info> svisited;
  hash_set<slp_tree> visited;
  hash_map<tree, int> scalar_use_map;
  auto_vec<slp_tree> worklist;

  for (slp_instance instance : bb_vinfo->slp_instances)
    {
      if (SLP_INSTANCE_KIND (instance) == slp_inst_kind_bb_reduc)
	for (tree op : SLP_INSTANCE_REMAIN_DEFS (instance))
	  if (TREE_CODE (op) == SSA_NAME)
	    scalar_use_map.put (op, 1);
      if (!visited.add (SLP_INSTANCE_TREE (instance)))
	worklist.safe_push (SLP_INSTANCE_TREE (instance));
    }

  do
    {
      slp_tree node = worklist.pop ();

      if (SLP_TREE_DEF_TYPE (node) == vect_external_def)
	{
	  for (tree op : SLP_TREE_SCALAR_OPS (node))
	    if (TREE_CODE (op) == SSA_NAME)
	      scalar_use_map.put (op, 1);
	}
      else
	{
	  for (slp_tree child : SLP_TREE_CHILDREN (node))
	    if (child && !visited.add (child))
	      worklist.safe_push (child);
	}
    }
  while (!worklist.is_empty ());

  visited.empty ();

  for (slp_instance instance : bb_vinfo->slp_instances)
    {
      vect_location = instance->location ();
      vect_bb_slp_mark_live_stmts (bb_vinfo, SLP_INSTANCE_TREE (instance),
				   instance, &instance->cost_vec,
				   scalar_use_map, svisited, visited);
    }
}

/* Determine whether we can vectorize the reduction epilogue for INSTANCE.  */

static bool
vectorizable_bb_reduc_epilogue (slp_instance instance,
				stmt_vector_for_cost *cost_vec)
{
  gassign *stmt = as_a <gassign *> (instance->root_stmts[0]->stmt);
  enum tree_code reduc_code = gimple_assign_rhs_code (stmt);
  if (reduc_code == MINUS_EXPR)
    reduc_code = PLUS_EXPR;
  internal_fn reduc_fn;
  tree vectype = SLP_TREE_VECTYPE (SLP_INSTANCE_TREE (instance));
  if (!vectype
      || !reduction_fn_for_scalar_code (reduc_code, &reduc_fn)
      || reduc_fn == IFN_LAST
      || !direct_internal_fn_supported_p (reduc_fn, vectype, OPTIMIZE_FOR_BOTH)
      || !useless_type_conversion_p (TREE_TYPE (gimple_assign_lhs (stmt)),
				     TREE_TYPE (vectype)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: basic block reduction epilogue "
			 "operation unsupported.\n");
      return false;
    }

  /* There's no way to cost a horizontal vector reduction via REDUC_FN so
     cost log2 vector operations plus shuffles and one extraction.  */
  unsigned steps = floor_log2 (vect_nunits_for_cost (vectype));
  record_stmt_cost (cost_vec, steps, vector_stmt, instance->root_stmts[0],
		    vectype, 0, vect_body);
  record_stmt_cost (cost_vec, steps, vec_perm, instance->root_stmts[0],
		    vectype, 0, vect_body);
  record_stmt_cost (cost_vec, 1, vec_to_scalar, instance->root_stmts[0],
		    vectype, 0, vect_body);

  /* Since we replace all stmts of a possibly longer scalar reduction
     chain account for the extra scalar stmts for that.  */
  record_stmt_cost (cost_vec, instance->remain_defs.length (), scalar_stmt,
		    instance->root_stmts[0], 0, vect_body);
  return true;
}

/* Prune from ROOTS all stmts that are computed as part of lanes of NODE
   and recurse to children.  */

static void
vect_slp_prune_covered_roots (slp_tree node, hash_set<stmt_vec_info> &roots,
			      hash_set<slp_tree> &visited)
{
  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def
      || visited.add (node))
    return;

  stmt_vec_info stmt;
  unsigned i;
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt)
    if (stmt)
      roots.remove (vect_orig_stmt (stmt));

  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      vect_slp_prune_covered_roots (child, roots, visited);
}

/* Analyze statements in SLP instances of VINFO.  Return true if the
   operations are supported. */

bool
vect_slp_analyze_operations (vec_info *vinfo)
{
  slp_instance instance;
  int i;

  DUMP_VECT_SCOPE ("vect_slp_analyze_operations");

  hash_set<slp_tree> visited;
  for (i = 0; vinfo->slp_instances.iterate (i, &instance); )
    {
      auto_vec<slp_tree> visited_vec;
      stmt_vector_for_cost cost_vec;
      cost_vec.create (2);
      if (is_a <bb_vec_info> (vinfo))
	vect_location = instance->location ();
      if (!vect_slp_analyze_node_operations (vinfo,
					     SLP_INSTANCE_TREE (instance),
					     instance, visited, visited_vec,
					     &cost_vec)
	  /* CTOR instances require vectorized defs for the SLP tree root.  */
	  || (SLP_INSTANCE_KIND (instance) == slp_inst_kind_ctor
	      && (SLP_TREE_DEF_TYPE (SLP_INSTANCE_TREE (instance))
		  != vect_internal_def
		  /* Make sure we vectorized with the expected type.  */
		  || !useless_type_conversion_p
			(TREE_TYPE (TREE_TYPE (gimple_assign_rhs1
					      (instance->root_stmts[0]->stmt))),
			 TREE_TYPE (SLP_TREE_VECTYPE
					    (SLP_INSTANCE_TREE (instance))))))
	  /* Check we can vectorize the reduction.  */
	  || (SLP_INSTANCE_KIND (instance) == slp_inst_kind_bb_reduc
	      && !vectorizable_bb_reduc_epilogue (instance, &cost_vec))
	  /* Check we can vectorize the gcond.  */
	  || (SLP_INSTANCE_KIND (instance) == slp_inst_kind_gcond
	      && !vectorizable_early_exit (vinfo,
					   SLP_INSTANCE_ROOT_STMTS (instance)[0],
					   NULL, NULL,
					   SLP_INSTANCE_TREE (instance),
					   &cost_vec)))
        {
	  cost_vec.release ();
	  slp_tree node = SLP_INSTANCE_TREE (instance);
	  stmt_vec_info stmt_info;
	  if (!SLP_INSTANCE_ROOT_STMTS (instance).is_empty ())
	    stmt_info = SLP_INSTANCE_ROOT_STMTS (instance)[0];
	  else
	    stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
	  if (is_a <loop_vec_info> (vinfo))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "unsupported SLP instance starting from: %G",
				 stmt_info->stmt);
	      return false;
	    }
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "removing SLP instance operations starting from: %G",
			     stmt_info->stmt);
	  vect_free_slp_instance (instance);
          vinfo->slp_instances.ordered_remove (i);
	  while (!visited_vec.is_empty ())
	    visited.remove (visited_vec.pop ());
	}
      else
	{
	  i++;
	  if (loop_vec_info loop_vinfo = dyn_cast<loop_vec_info> (vinfo))
	    {
	      add_stmt_costs (loop_vinfo->vector_costs, &cost_vec);
	      cost_vec.release ();
	    }
	  else
	    /* For BB vectorization remember the SLP graph entry
	       cost for later.  */
	    instance->cost_vec = cost_vec;
	}
    }

  /* Now look for SLP instances with a root that are covered by other
     instances and remove them.  */
  hash_set<stmt_vec_info> roots;
  for (i = 0; vinfo->slp_instances.iterate (i, &instance); ++i)
    if (!SLP_INSTANCE_ROOT_STMTS (instance).is_empty ())
      roots.add (SLP_INSTANCE_ROOT_STMTS (instance)[0]);
  if (!roots.is_empty ())
    {
      visited.empty ();
      for (i = 0; vinfo->slp_instances.iterate (i, &instance); ++i)
	vect_slp_prune_covered_roots (SLP_INSTANCE_TREE (instance), roots,
				      visited);
      for (i = 0; vinfo->slp_instances.iterate (i, &instance); )
	if (!SLP_INSTANCE_ROOT_STMTS (instance).is_empty ()
	    && !roots.contains (SLP_INSTANCE_ROOT_STMTS (instance)[0]))
	  {
	    stmt_vec_info root = SLP_INSTANCE_ROOT_STMTS (instance)[0];
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "removing SLP instance operations starting "
			       "from: %G", root->stmt);
	    vect_free_slp_instance (instance);
	    vinfo->slp_instances.ordered_remove (i);
	  }
	else
	  ++i;
    }

  /* Compute vectorizable live stmts.  */
  if (bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo))
    vect_bb_slp_mark_live_stmts (bb_vinfo);

  return !vinfo->slp_instances.is_empty ();
}

/* Get the SLP instance leader from INSTANCE_LEADER thereby transitively
   closing the eventual chain.  */

static slp_instance
get_ultimate_leader (slp_instance instance,
		     hash_map<slp_instance, slp_instance> &instance_leader)
{
  auto_vec<slp_instance *, 8> chain;
  slp_instance *tem;
  while (*(tem = instance_leader.get (instance)) != instance)
    {
      chain.safe_push (tem);
      instance = *tem;
    }
  while (!chain.is_empty ())
    *chain.pop () = instance;
  return instance;
}

namespace {
/* Subroutine of vect_bb_partition_graph_r.  Map KEY to INSTANCE in
   KEY_TO_INSTANCE, making INSTANCE the leader of any previous mapping
   for KEY.  Return true if KEY was already in KEY_TO_INSTANCE.

   INSTANCE_LEADER is as for get_ultimate_leader.  */

template<typename T>
bool
vect_map_to_instance (slp_instance instance, T key,
		      hash_map<T, slp_instance> &key_to_instance,
		      hash_map<slp_instance, slp_instance> &instance_leader)
{
  bool existed_p;
  slp_instance &key_instance = key_to_instance.get_or_insert (key, &existed_p);
  if (!existed_p)
    ;
  else if (key_instance != instance)
    {
      /* If we're running into a previously marked key make us the
	 leader of the current ultimate leader.  This keeps the
	 leader chain acyclic and works even when the current instance
	 connects two previously independent graph parts.  */
      slp_instance key_leader
	= get_ultimate_leader (key_instance, instance_leader);
      if (key_leader != instance)
	instance_leader.put (key_leader, instance);
    }
  key_instance = instance;
  return existed_p;
}
}

/* Worker of vect_bb_partition_graph, recurse on NODE.  */

static void
vect_bb_partition_graph_r (bb_vec_info bb_vinfo,
			   slp_instance instance, slp_tree node,
			   hash_map<stmt_vec_info, slp_instance> &stmt_to_instance,
			   hash_map<slp_tree, slp_instance> &node_to_instance,
			   hash_map<slp_instance, slp_instance> &instance_leader)
{
  stmt_vec_info stmt_info;
  unsigned i;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    if (stmt_info)
      vect_map_to_instance (instance, stmt_info, stmt_to_instance,
			    instance_leader);

  if (vect_map_to_instance (instance, node, node_to_instance,
			    instance_leader))
    return;

  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child && SLP_TREE_DEF_TYPE (child) == vect_internal_def)
      vect_bb_partition_graph_r (bb_vinfo, instance, child, stmt_to_instance,
				 node_to_instance, instance_leader);
}

/* Partition the SLP graph into pieces that can be costed independently.  */

static void
vect_bb_partition_graph (bb_vec_info bb_vinfo)
{
  DUMP_VECT_SCOPE ("vect_bb_partition_graph");

  /* First walk the SLP graph assigning each involved scalar stmt a
     corresponding SLP graph entry and upon visiting a previously
     marked stmt, make the stmts leader the current SLP graph entry.  */
  hash_map<stmt_vec_info, slp_instance> stmt_to_instance;
  hash_map<slp_tree, slp_instance> node_to_instance;
  hash_map<slp_instance, slp_instance> instance_leader;
  slp_instance instance;
  for (unsigned i = 0; bb_vinfo->slp_instances.iterate (i, &instance); ++i)
    {
      instance_leader.put (instance, instance);
      vect_bb_partition_graph_r (bb_vinfo,
				 instance, SLP_INSTANCE_TREE (instance),
				 stmt_to_instance, node_to_instance,
				 instance_leader);
    }

  /* Then collect entries to each independent subgraph.  */
  for (unsigned i = 0; bb_vinfo->slp_instances.iterate (i, &instance); ++i)
    {
      slp_instance leader = get_ultimate_leader (instance, instance_leader);
      leader->subgraph_entries.safe_push (instance);
      if (dump_enabled_p ()
	  && leader != instance)
	dump_printf_loc (MSG_NOTE, vect_location,
			 "instance %p is leader of %p\n",
			 (void *) leader, (void *) instance);
    }
}

/* Compute the set of scalar stmts participating in internal and external
   nodes.  */

static void
vect_slp_gather_vectorized_scalar_stmts (vec_info *vinfo, slp_tree node,
					 hash_set<slp_tree> &visited,
					 hash_set<stmt_vec_info> &vstmts,
					 hash_set<stmt_vec_info> &estmts)
{
  int i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (visited.add (node))
    return;

  if (SLP_TREE_DEF_TYPE (node) == vect_internal_def)
    {
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
	if (stmt_info)
	  vstmts.add (stmt_info);

      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
	if (child)
	  vect_slp_gather_vectorized_scalar_stmts (vinfo, child, visited,
						   vstmts, estmts);
    }
  else
    for (tree def : SLP_TREE_SCALAR_OPS (node))
      {
	stmt_vec_info def_stmt = vinfo->lookup_def (def);
	if (def_stmt)
	  estmts.add (def_stmt);
      }
}


/* Compute the scalar cost of the SLP node NODE and its children
   and return it.  Do not account defs that are marked in LIFE and
   update LIFE according to uses of NODE.  */

static void
vect_bb_slp_scalar_cost (vec_info *vinfo,
			 slp_tree node, vec<bool, va_heap> *life,
			 stmt_vector_for_cost *cost_vec,
			 hash_set<stmt_vec_info> &vectorized_scalar_stmts,
			 hash_set<slp_tree> &visited)
{
  unsigned i;
  stmt_vec_info stmt_info;
  slp_tree child;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      ssa_op_iter op_iter;
      def_operand_p def_p;

      if (!stmt_info || (*life)[i])
	continue;

      stmt_vec_info orig_stmt_info = vect_orig_stmt (stmt_info);
      gimple *orig_stmt = orig_stmt_info->stmt;

      /* If there is a non-vectorized use of the defs then the scalar
         stmt is kept live in which case we do not account it or any
	 required defs in the SLP children in the scalar cost.  This
	 way we make the vectorization more costly when compared to
	 the scalar cost.  */
      if (!STMT_VINFO_LIVE_P (stmt_info))
	{
	  auto_vec<gimple *, 8> worklist;
	  hash_set<gimple *> *worklist_visited = NULL;
	  worklist.quick_push (orig_stmt);
	  do
	    {
	      gimple *work_stmt = worklist.pop ();
	      FOR_EACH_PHI_OR_STMT_DEF (def_p, work_stmt, op_iter, SSA_OP_DEF)
		{
		  imm_use_iterator use_iter;
		  gimple *use_stmt;
		  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter,
					 DEF_FROM_PTR (def_p))
		    if (!is_gimple_debug (use_stmt))
		      {
			stmt_vec_info use_stmt_info
			  = vinfo->lookup_stmt (use_stmt);
			if (!use_stmt_info
			    || !vectorized_scalar_stmts.contains (use_stmt_info))
			  {
			    if (use_stmt_info
				&& STMT_VINFO_IN_PATTERN_P (use_stmt_info))
			      {
				/* For stmts participating in patterns we have
				   to check its uses recursively.  */
				if (!worklist_visited)
				  worklist_visited = new hash_set<gimple *> ();
				if (!worklist_visited->add (use_stmt))
				  worklist.safe_push (use_stmt);
				continue;
			      }
			    (*life)[i] = true;
			    goto next_lane;
			  }
		      }
		}
	    }
	  while (!worklist.is_empty ());
next_lane:
	  if (worklist_visited)
	    delete worklist_visited;
	  if ((*life)[i])
	    continue;
	}

      /* Count scalar stmts only once.  */
      if (gimple_visited_p (orig_stmt))
	continue;
      gimple_set_visited (orig_stmt, true);

      vect_cost_for_stmt kind;
      if (STMT_VINFO_DATA_REF (orig_stmt_info))
	{
	  data_reference_p dr = STMT_VINFO_DATA_REF (orig_stmt_info);
	  tree base = get_base_address (DR_REF (dr));
	  /* When the scalar access is to a non-global not address-taken
	     decl that is not BLKmode assume we can access it with a single
	     non-load/store instruction.  */
	  if (DECL_P (base)
	      && !is_global_var (base)
	      && !TREE_ADDRESSABLE (base)
	      && DECL_MODE (base) != BLKmode)
	    kind = scalar_stmt;
	  else if (DR_IS_READ (STMT_VINFO_DATA_REF (orig_stmt_info)))
	    kind = scalar_load;
	  else
	    kind = scalar_store;
	}
      else if (vect_nop_conversion_p (orig_stmt_info))
	continue;
      /* For single-argument PHIs assume coalescing which means zero cost
	 for the scalar and the vector PHIs.  This avoids artificially
	 favoring the vector path (but may pessimize it in some cases).  */
      else if (is_a <gphi *> (orig_stmt_info->stmt)
	       && gimple_phi_num_args
		    (as_a <gphi *> (orig_stmt_info->stmt)) == 1)
	continue;
      else
	kind = scalar_stmt;
      record_stmt_cost (cost_vec, 1, kind, orig_stmt_info,
			SLP_TREE_VECTYPE (node), 0, vect_body);
    }

  auto_vec<bool, 20> subtree_life;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    {
      if (child && SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	{
	  /* Do not directly pass LIFE to the recursive call, copy it to
	     confine changes in the callee to the current child/subtree.  */
	  if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
	    {
	      subtree_life.safe_grow_cleared (SLP_TREE_LANES (child), true);
	      for (unsigned j = 0;
		   j < SLP_TREE_LANE_PERMUTATION (node).length (); ++j)
		{
		  auto perm = SLP_TREE_LANE_PERMUTATION (node)[j];
		  if (perm.first == i)
		    subtree_life[perm.second] = (*life)[j];
		}
	    }
	  else
	    {
	      gcc_assert (SLP_TREE_LANES (node) == SLP_TREE_LANES (child));
	      subtree_life.safe_splice (*life);
	    }
	  vect_bb_slp_scalar_cost (vinfo, child, &subtree_life, cost_vec,
				   vectorized_scalar_stmts, visited);
	  subtree_life.truncate (0);
	}
    }
}

/* Comparator for the loop-index sorted cost vectors.  */

static int
li_cost_vec_cmp (const void *a_, const void *b_)
{
  auto *a = (const std::pair<unsigned, stmt_info_for_cost *> *)a_;
  auto *b = (const std::pair<unsigned, stmt_info_for_cost *> *)b_;
  if (a->first < b->first)
    return -1;
  else if (a->first == b->first)
    return 0;
  return 1;
}

/* Check if vectorization of the basic block is profitable for the
   subgraph denoted by SLP_INSTANCES.  */

static bool
vect_bb_vectorization_profitable_p (bb_vec_info bb_vinfo,
				    vec<slp_instance> slp_instances,
				    loop_p orig_loop)
{
  slp_instance instance;
  int i;
  unsigned int vec_inside_cost = 0, vec_outside_cost = 0, scalar_cost = 0;
  unsigned int vec_prologue_cost = 0, vec_epilogue_cost = 0;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "Costing subgraph: \n");
      hash_set<slp_tree> visited;
      FOR_EACH_VEC_ELT (slp_instances, i, instance)
	vect_print_slp_graph (MSG_NOTE, vect_location,
			      SLP_INSTANCE_TREE (instance), visited);
    }

  /* Compute the set of scalar stmts we know will go away 'locally' when
     vectorizing.  This used to be tracked with just PURE_SLP_STMT but that's
     not accurate for nodes promoted extern late or for scalar stmts that
     are used both in extern defs and in vectorized defs.  */
  hash_set<stmt_vec_info> vectorized_scalar_stmts;
  hash_set<stmt_vec_info> scalar_stmts_in_externs;
  hash_set<slp_tree> visited;
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      vect_slp_gather_vectorized_scalar_stmts (bb_vinfo,
					       SLP_INSTANCE_TREE (instance),
					       visited,
					       vectorized_scalar_stmts,
					       scalar_stmts_in_externs);
      for (stmt_vec_info rstmt : SLP_INSTANCE_ROOT_STMTS (instance))
	vectorized_scalar_stmts.add (rstmt);
    }
  /* Scalar stmts used as defs in external nodes need to be preseved, so
     remove them from vectorized_scalar_stmts.  */
  for (stmt_vec_info stmt : scalar_stmts_in_externs)
    vectorized_scalar_stmts.remove (stmt);

  /* Calculate scalar cost and sum the cost for the vector stmts
     previously collected.  */
  stmt_vector_for_cost scalar_costs = vNULL;
  stmt_vector_for_cost vector_costs = vNULL;
  visited.empty ();
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      auto_vec<bool, 20> life;
      life.safe_grow_cleared (SLP_TREE_LANES (SLP_INSTANCE_TREE (instance)),
			      true);
      if (!SLP_INSTANCE_ROOT_STMTS (instance).is_empty ())
	record_stmt_cost (&scalar_costs,
			  SLP_INSTANCE_ROOT_STMTS (instance).length (),
			  scalar_stmt,
			  SLP_INSTANCE_ROOT_STMTS (instance)[0], 0, vect_body);
      vect_bb_slp_scalar_cost (bb_vinfo,
			       SLP_INSTANCE_TREE (instance),
			       &life, &scalar_costs, vectorized_scalar_stmts,
			       visited);
      vector_costs.safe_splice (instance->cost_vec);
      instance->cost_vec.release ();
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Cost model analysis: \n");

  /* When costing non-loop vectorization we need to consider each covered
     loop independently and make sure vectorization is profitable.  For
     now we assume a loop may be not entered or executed an arbitrary
     number of iterations (???  static information can provide more
     precise info here) which means we can simply cost each containing
     loops stmts separately.  */

  /* First produce cost vectors sorted by loop index.  */
  auto_vec<std::pair<unsigned, stmt_info_for_cost *> >
    li_scalar_costs (scalar_costs.length ());
  auto_vec<std::pair<unsigned, stmt_info_for_cost *> >
    li_vector_costs (vector_costs.length ());
  stmt_info_for_cost *cost;
  FOR_EACH_VEC_ELT (scalar_costs, i, cost)
    {
      unsigned l = gimple_bb (cost->stmt_info->stmt)->loop_father->num;
      li_scalar_costs.quick_push (std::make_pair (l, cost));
    }
  /* Use a random used loop as fallback in case the first vector_costs
     entry does not have a stmt_info associated with it.  */
  unsigned l = li_scalar_costs[0].first;
  FOR_EACH_VEC_ELT (vector_costs, i, cost)
    {
      /* We inherit from the previous COST, invariants, externals and
	 extracts immediately follow the cost for the related stmt.  */
      if (cost->stmt_info)
	l = gimple_bb (cost->stmt_info->stmt)->loop_father->num;
      li_vector_costs.quick_push (std::make_pair (l, cost));
    }
  li_scalar_costs.qsort (li_cost_vec_cmp);
  li_vector_costs.qsort (li_cost_vec_cmp);

  /* Now cost the portions individually.  */
  unsigned vi = 0;
  unsigned si = 0;
  bool profitable = true;
  while (si < li_scalar_costs.length ()
	 && vi < li_vector_costs.length ())
    {
      unsigned sl = li_scalar_costs[si].first;
      unsigned vl = li_vector_costs[vi].first;
      if (sl != vl)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Scalar %d and vector %d loop part do not "
			     "match up, skipping scalar part\n", sl, vl);
	  /* Skip the scalar part, assuming zero cost on the vector side.  */
	  do
	    {
	      si++;
	    }
	  while (si < li_scalar_costs.length ()
		 && li_scalar_costs[si].first == sl);
	  continue;
	}

      class vector_costs *scalar_target_cost_data = init_cost (bb_vinfo, true);
      do
	{
	  add_stmt_cost (scalar_target_cost_data, li_scalar_costs[si].second);
	  si++;
	}
      while (si < li_scalar_costs.length ()
	     && li_scalar_costs[si].first == sl);
      unsigned dummy;
      finish_cost (scalar_target_cost_data, nullptr,
		   &dummy, &scalar_cost, &dummy);

      /* Complete the target-specific vector cost calculation.  */
      class vector_costs *vect_target_cost_data = init_cost (bb_vinfo, false);
      do
	{
	  add_stmt_cost (vect_target_cost_data, li_vector_costs[vi].second);
	  vi++;
	}
      while (vi < li_vector_costs.length ()
	     && li_vector_costs[vi].first == vl);
      finish_cost (vect_target_cost_data, scalar_target_cost_data,
		   &vec_prologue_cost, &vec_inside_cost, &vec_epilogue_cost);
      delete scalar_target_cost_data;
      delete vect_target_cost_data;

      vec_outside_cost = vec_prologue_cost + vec_epilogue_cost;

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Cost model analysis for part in loop %d:\n", sl);
	  dump_printf (MSG_NOTE, "  Vector cost: %d\n",
		       vec_inside_cost + vec_outside_cost);
	  dump_printf (MSG_NOTE, "  Scalar cost: %d\n", scalar_cost);
	}

      /* Vectorization is profitable if its cost is more than the cost of scalar
	 version.  Note that we err on the vector side for equal cost because
	 the cost estimate is otherwise quite pessimistic (constant uses are
	 free on the scalar side but cost a load on the vector side for
	 example).  */
      if (vec_outside_cost + vec_inside_cost > scalar_cost)
	{
	  profitable = false;
	  break;
	}
    }
  if (profitable && vi < li_vector_costs.length ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Excess vector cost for part in loop %d:\n",
			 li_vector_costs[vi].first);
      profitable = false;
    }

  /* Unset visited flag.  This is delayed when the subgraph is profitable
     and we process the loop for remaining unvectorized if-converted code.  */
  if (!orig_loop || !profitable)
    FOR_EACH_VEC_ELT (scalar_costs, i, cost)
      gimple_set_visited  (cost->stmt_info->stmt, false);

  scalar_costs.release ();
  vector_costs.release ();

  return profitable;
}

/* qsort comparator for lane defs.  */

static int
vld_cmp (const void *a_, const void *b_)
{
  auto *a = (const std::pair<unsigned, tree> *)a_;
  auto *b = (const std::pair<unsigned, tree> *)b_;
  return a->first - b->first;
}

/* Return true if USE_STMT is a vector lane insert into VEC and set
   *THIS_LANE to the lane number that is set.  */

static bool
vect_slp_is_lane_insert (gimple *use_stmt, tree vec, unsigned *this_lane)
{
  gassign *use_ass = dyn_cast <gassign *> (use_stmt);
  if (!use_ass
      || gimple_assign_rhs_code (use_ass) != BIT_INSERT_EXPR
      || (vec
	  ? gimple_assign_rhs1 (use_ass) != vec
	  : ((vec = gimple_assign_rhs1 (use_ass)), false))
      || !useless_type_conversion_p (TREE_TYPE (TREE_TYPE (vec)),
				     TREE_TYPE (gimple_assign_rhs2 (use_ass)))
      || !constant_multiple_p
	    (tree_to_poly_uint64 (gimple_assign_rhs3 (use_ass)),
	     tree_to_poly_uint64 (TYPE_SIZE (TREE_TYPE (TREE_TYPE (vec)))),
	     this_lane))
    return false;
  return true;
}

/* Find any vectorizable constructors and add them to the grouped_store
   array.  */

static void
vect_slp_check_for_roots (bb_vec_info bb_vinfo)
{
  for (unsigned i = 0; i < bb_vinfo->nbbs; ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb_vinfo->bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gassign *assign = dyn_cast<gassign *> (gsi_stmt (gsi));
      /* This can be used to start SLP discovery for early breaks for BB early breaks
	 when we get that far.  */
      if (!assign)
	continue;

      tree rhs = gimple_assign_rhs1 (assign);
      enum tree_code code = gimple_assign_rhs_code (assign);
      use_operand_p use_p;
      gimple *use_stmt;
      if (code == CONSTRUCTOR)
	{
	  if (!VECTOR_TYPE_P (TREE_TYPE (rhs))
	      || maybe_ne (TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs)),
			   CONSTRUCTOR_NELTS (rhs))
	      || VECTOR_TYPE_P (TREE_TYPE (CONSTRUCTOR_ELT (rhs, 0)->value))
	      || uniform_vector_p (rhs))
	    continue;

	  unsigned j;
	  tree val;
	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), j, val)
	    if (TREE_CODE (val) != SSA_NAME
		|| !bb_vinfo->lookup_def (val))
	      break;
	  if (j != CONSTRUCTOR_NELTS (rhs))
	    continue;

	  vec<stmt_vec_info> roots = vNULL;
	  roots.safe_push (bb_vinfo->lookup_stmt (assign));
	  vec<stmt_vec_info> stmts;
	  stmts.create (CONSTRUCTOR_NELTS (rhs));
	  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), j, val)
	    stmts.quick_push
	      (vect_stmt_to_vectorize (bb_vinfo->lookup_def (val)));
	  bb_vinfo->roots.safe_push (slp_root (slp_inst_kind_ctor,
					       stmts, roots));
	}
      else if (code == BIT_INSERT_EXPR
	       && VECTOR_TYPE_P (TREE_TYPE (rhs))
	       && TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs)).is_constant ()
	       && TYPE_VECTOR_SUBPARTS (TREE_TYPE (rhs)).to_constant () > 1
	       && integer_zerop (gimple_assign_rhs3 (assign))
	       && useless_type_conversion_p
		    (TREE_TYPE (TREE_TYPE (rhs)),
		     TREE_TYPE (gimple_assign_rhs2 (assign)))
	       && bb_vinfo->lookup_def (gimple_assign_rhs2 (assign)))
	{
	  /* We start to match on insert to lane zero but since the
	     inserts need not be ordered we'd have to search both
	     the def and the use chains.  */
	  tree vectype = TREE_TYPE (rhs);
	  unsigned nlanes = TYPE_VECTOR_SUBPARTS (vectype).to_constant ();
	  auto_vec<std::pair<unsigned, tree> > lane_defs (nlanes);
	  auto_sbitmap lanes (nlanes);
	  bitmap_clear (lanes);
	  bitmap_set_bit (lanes, 0);
	  tree def = gimple_assign_lhs (assign);
	  lane_defs.quick_push
		      (std::make_pair (0, gimple_assign_rhs2 (assign)));
	  unsigned lanes_found = 1;
	  /* Start with the use chains, the last stmt will be the root.  */
	  stmt_vec_info last = bb_vinfo->lookup_stmt (assign);
	  vec<stmt_vec_info> roots = vNULL;
	  roots.safe_push (last);
	  do
	    {
	      use_operand_p use_p;
	      gimple *use_stmt;
	      if (!single_imm_use (def, &use_p, &use_stmt))
		break;
	      unsigned this_lane;
	      if (!bb_vinfo->lookup_stmt (use_stmt)
		  || !vect_slp_is_lane_insert (use_stmt, def, &this_lane)
		  || !bb_vinfo->lookup_def (gimple_assign_rhs2 (use_stmt)))
		break;
	      if (bitmap_bit_p (lanes, this_lane))
		break;
	      lanes_found++;
	      bitmap_set_bit (lanes, this_lane);
	      gassign *use_ass = as_a <gassign *> (use_stmt);
	      lane_defs.quick_push (std::make_pair
				     (this_lane, gimple_assign_rhs2 (use_ass)));
	      last = bb_vinfo->lookup_stmt (use_ass);
	      roots.safe_push (last);
	      def = gimple_assign_lhs (use_ass);
	    }
	  while (lanes_found < nlanes);
	  if (roots.length () > 1)
	    std::swap(roots[0], roots[roots.length () - 1]);
	  if (lanes_found < nlanes)
	    {
	      /* Now search the def chain.  */
	      def = gimple_assign_rhs1 (assign);
	      do
		{
		  if (TREE_CODE (def) != SSA_NAME
		      || !has_single_use (def))
		    break;
		  gimple *def_stmt = SSA_NAME_DEF_STMT (def);
		  unsigned this_lane;
		  if (!bb_vinfo->lookup_stmt (def_stmt)
		      || !vect_slp_is_lane_insert (def_stmt,
						   NULL_TREE, &this_lane)
		      || !bb_vinfo->lookup_def (gimple_assign_rhs2 (def_stmt)))
		    break;
		  if (bitmap_bit_p (lanes, this_lane))
		    break;
		  lanes_found++;
		  bitmap_set_bit (lanes, this_lane);
		  lane_defs.quick_push (std::make_pair
					  (this_lane,
					   gimple_assign_rhs2 (def_stmt)));
		  roots.safe_push (bb_vinfo->lookup_stmt (def_stmt));
		  def = gimple_assign_rhs1 (def_stmt);
		}
	      while (lanes_found < nlanes);
	    }
	  if (lanes_found == nlanes)
	    {
	      /* Sort lane_defs after the lane index and register the root.  */
	      lane_defs.qsort (vld_cmp);
	      vec<stmt_vec_info> stmts;
	      stmts.create (nlanes);
	      for (unsigned i = 0; i < nlanes; ++i)
		stmts.quick_push (bb_vinfo->lookup_def (lane_defs[i].second));
	      bb_vinfo->roots.safe_push (slp_root (slp_inst_kind_ctor,
						   stmts, roots));
	    }
	  else
	    roots.release ();
	}
      else if (!VECTOR_TYPE_P (TREE_TYPE (rhs))
	       && (associative_tree_code (code) || code == MINUS_EXPR)
	       /* ???  This pessimizes a two-element reduction.  PR54400.
		  ???  In-order reduction could be handled if we only
		  traverse one operand chain in vect_slp_linearize_chain.  */
	       && !needs_fold_left_reduction_p (TREE_TYPE (rhs), code)
	       /* Ops with constants at the tail can be stripped here.  */
	       && TREE_CODE (rhs) == SSA_NAME
	       && TREE_CODE (gimple_assign_rhs2 (assign)) == SSA_NAME
	       /* Should be the chain end.  */
	       && (!single_imm_use (gimple_assign_lhs (assign),
				    &use_p, &use_stmt)
		   || !is_gimple_assign (use_stmt)
		   || (gimple_assign_rhs_code (use_stmt) != code
		       && ((code != PLUS_EXPR && code != MINUS_EXPR)
			   || (gimple_assign_rhs_code (use_stmt)
			       != (code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR))))))
	{
	  /* We start the match at the end of a possible association
	     chain.  */
	  auto_vec<chain_op_t> chain;
	  auto_vec<std::pair<tree_code, gimple *> > worklist;
	  auto_vec<gimple *> chain_stmts;
	  gimple *code_stmt = NULL, *alt_code_stmt = NULL;
	  if (code == MINUS_EXPR)
	    code = PLUS_EXPR;
	  internal_fn reduc_fn;
	  if (!reduction_fn_for_scalar_code (code, &reduc_fn)
	      || reduc_fn == IFN_LAST)
	    continue;
	  vect_slp_linearize_chain (bb_vinfo, worklist, chain, code, assign,
				    /* ??? */
				    code_stmt, alt_code_stmt, &chain_stmts);
	  if (chain.length () > 1)
	    {
	      /* Sort the chain according to def_type and operation.  */
	      chain.sort (dt_sort_cmp, bb_vinfo);
	      /* ???  Now we'd want to strip externals and constants
		 but record those to be handled in the epilogue.  */
	      /* ???  For now do not allow mixing ops or externs/constants.  */
	      bool invalid = false;
	      unsigned remain_cnt = 0;
	      unsigned last_idx = 0;
	      for (unsigned i = 0; i < chain.length (); ++i)
		{
		  if (chain[i].code != code)
		    {
		      invalid = true;
		      break;
		    }
		  if (chain[i].dt != vect_internal_def
		      /* Avoid stmts where the def is not the LHS, like
			 ASMs.  */
		      || (gimple_get_lhs (bb_vinfo->lookup_def
						      (chain[i].op)->stmt)
			  != chain[i].op))
		    remain_cnt++;
		  else
		    last_idx = i;
		}
	      /* Make sure to have an even number of lanes as we later do
		 all-or-nothing discovery, not trying to split further.  */
	      if ((chain.length () - remain_cnt) & 1)
		remain_cnt++;
	      if (!invalid && chain.length () - remain_cnt > 1)
		{
		  vec<stmt_vec_info> stmts;
		  vec<tree> remain = vNULL;
		  stmts.create (chain.length ());
		  if (remain_cnt > 0)
		    remain.create (remain_cnt);
		  for (unsigned i = 0; i < chain.length (); ++i)
		    {
		      stmt_vec_info stmt_info;
		      if (chain[i].dt == vect_internal_def
			  && ((stmt_info = bb_vinfo->lookup_def (chain[i].op)),
			      gimple_get_lhs (stmt_info->stmt) == chain[i].op)
			  && (i != last_idx
			      || (stmts.length () & 1)))
			stmts.quick_push (stmt_info);
		      else
			remain.quick_push (chain[i].op);
		    }
		  vec<stmt_vec_info> roots;
		  roots.create (chain_stmts.length ());
		  for (unsigned i = 0; i < chain_stmts.length (); ++i)
		    roots.quick_push (bb_vinfo->lookup_stmt (chain_stmts[i]));
		  bb_vinfo->roots.safe_push (slp_root (slp_inst_kind_bb_reduc,
						       stmts, roots, remain));
		}
	    }
	}
    }
}

/* Walk the grouped store chains and replace entries with their
   pattern variant if any.  */

static void
vect_fixup_store_groups_with_patterns (vec_info *vinfo)
{
  stmt_vec_info first_element;
  unsigned i;

  FOR_EACH_VEC_ELT (vinfo->grouped_stores, i, first_element)
    {
      /* We also have CTORs in this array.  */
      if (!STMT_VINFO_GROUPED_ACCESS (first_element))
	continue;
      if (STMT_VINFO_IN_PATTERN_P (first_element))
	{
	  stmt_vec_info orig = first_element;
	  first_element = STMT_VINFO_RELATED_STMT (first_element);
	  DR_GROUP_FIRST_ELEMENT (first_element) = first_element;
	  DR_GROUP_SIZE (first_element) = DR_GROUP_SIZE (orig);
	  DR_GROUP_GAP (first_element) = DR_GROUP_GAP (orig);
	  DR_GROUP_NEXT_ELEMENT (first_element) = DR_GROUP_NEXT_ELEMENT (orig);
	  vinfo->grouped_stores[i] = first_element;
	}
      stmt_vec_info prev = first_element;
      while (DR_GROUP_NEXT_ELEMENT (prev))
	{
	  stmt_vec_info elt = DR_GROUP_NEXT_ELEMENT (prev);
	  if (STMT_VINFO_IN_PATTERN_P (elt))
	    {
	      stmt_vec_info orig = elt;
	      elt = STMT_VINFO_RELATED_STMT (elt);
	      DR_GROUP_NEXT_ELEMENT (prev) = elt;
	      DR_GROUP_GAP (elt) = DR_GROUP_GAP (orig);
	      DR_GROUP_NEXT_ELEMENT (elt) = DR_GROUP_NEXT_ELEMENT (orig);
	    }
	  DR_GROUP_FIRST_ELEMENT (elt) = first_element;
	  prev = elt;
	}
    }
}

/* Check if the region described by BB_VINFO can be vectorized, returning
   true if so.  When returning false, set FATAL to true if the same failure
   would prevent vectorization at other vector sizes, false if it is still
   worth trying other sizes.  N_STMTS is the number of statements in the
   region.  */

static bool
vect_slp_analyze_bb_1 (bb_vec_info bb_vinfo, int n_stmts, bool &fatal,
		       vec<int> *dataref_groups)
{
  DUMP_VECT_SCOPE ("vect_slp_analyze_bb");

  slp_instance instance;
  int i;
  poly_uint64 min_vf = 2;

  /* The first group of checks is independent of the vector size.  */
  fatal = true;

  /* Analyze the data references.  */

  if (!vect_analyze_data_refs (bb_vinfo, &min_vf, NULL))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: unhandled data-ref in basic "
			 "block.\n");
      return false;
    }

  if (!vect_analyze_data_ref_accesses (bb_vinfo, dataref_groups))
    {
     if (dump_enabled_p ())
       dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"not vectorized: unhandled data access in "
			"basic block.\n");
      return false;
    }

  vect_slp_check_for_roots (bb_vinfo);

  /* If there are no grouped stores and no constructors in the region
     there is no need to continue with pattern recog as vect_analyze_slp
     will fail anyway.  */
  if (bb_vinfo->grouped_stores.is_empty ()
      && bb_vinfo->roots.is_empty ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: no grouped stores in "
			 "basic block.\n");
      return false;
    }

  /* While the rest of the analysis below depends on it in some way.  */
  fatal = false;

  vect_pattern_recog (bb_vinfo);

  /* Update store groups from pattern processing.  */
  vect_fixup_store_groups_with_patterns (bb_vinfo);

  /* Check the SLP opportunities in the basic block, analyze and build SLP
     trees.  */
  if (!vect_analyze_slp (bb_vinfo, n_stmts, false))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Failed to SLP the basic block.\n");
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "not vectorized: failed to find SLP opportunities "
			   "in basic block.\n");
	}
      return false;
    }

  /* Optimize permutations.  */
  vect_optimize_slp (bb_vinfo);

  /* Gather the loads reachable from the SLP graph entries.  */
  vect_gather_slp_loads (bb_vinfo);

  vect_record_base_alignments (bb_vinfo);

  /* Analyze and verify the alignment of data references and the
     dependence in the SLP instances.  */
  for (i = 0; BB_VINFO_SLP_INSTANCES (bb_vinfo).iterate (i, &instance); )
    {
      vect_location = instance->location ();
      if (! vect_slp_analyze_instance_alignment (bb_vinfo, instance)
	  || ! vect_slp_analyze_instance_dependence (bb_vinfo, instance))
	{
	  slp_tree node = SLP_INSTANCE_TREE (instance);
	  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "removing SLP instance operations starting from: %G",
			     stmt_info->stmt);
	  vect_free_slp_instance (instance);
	  BB_VINFO_SLP_INSTANCES (bb_vinfo).ordered_remove (i);
	  continue;
	}

      /* Mark all the statements that we want to vectorize as pure SLP and
	 relevant.  */
      vect_mark_slp_stmts (SLP_INSTANCE_TREE (instance));
      vect_mark_slp_stmts_relevant (SLP_INSTANCE_TREE (instance));
      unsigned j;
      stmt_vec_info root;
      /* Likewise consider instance root stmts as vectorized.  */
      FOR_EACH_VEC_ELT (SLP_INSTANCE_ROOT_STMTS (instance), j, root)
	STMT_SLP_TYPE (root) = pure_slp;

      i++;
    }
  if (! BB_VINFO_SLP_INSTANCES (bb_vinfo).length ())
    return false;

  if (!vect_slp_analyze_operations (bb_vinfo))
    {
      if (dump_enabled_p ())
        dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: bad operation in basic block.\n");
      return false;
    }

  vect_bb_partition_graph (bb_vinfo);

  return true;
}

/* Subroutine of vect_slp_bb.  Try to vectorize the statements for all
   basic blocks in BBS, returning true on success.
   The region has N_STMTS statements and has the datarefs given by DATAREFS.  */

static bool
vect_slp_region (vec<basic_block> bbs, vec<data_reference_p> datarefs,
		 vec<int> *dataref_groups, unsigned int n_stmts,
		 loop_p orig_loop)
{
  bb_vec_info bb_vinfo;
  auto_vector_modes vector_modes;

  /* Autodetect first vector size we try.  */
  machine_mode next_vector_mode = VOIDmode;
  targetm.vectorize.autovectorize_vector_modes (&vector_modes, false);
  unsigned int mode_i = 0;

  vec_info_shared shared;

  machine_mode autodetected_vector_mode = VOIDmode;
  while (1)
    {
      bool vectorized = false;
      bool fatal = false;
      bb_vinfo = new _bb_vec_info (bbs, &shared);

      bool first_time_p = shared.datarefs.is_empty ();
      BB_VINFO_DATAREFS (bb_vinfo) = datarefs;
      if (first_time_p)
	bb_vinfo->shared->save_datarefs ();
      else
	bb_vinfo->shared->check_datarefs ();
      bb_vinfo->vector_mode = next_vector_mode;

      if (vect_slp_analyze_bb_1 (bb_vinfo, n_stmts, fatal, dataref_groups))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "***** Analysis succeeded with vector mode"
			       " %s\n", GET_MODE_NAME (bb_vinfo->vector_mode));
	      dump_printf_loc (MSG_NOTE, vect_location, "SLPing BB part\n");
	    }

	  bb_vinfo->shared->check_datarefs ();

	  bool force_clear = false;
	  auto_vec<slp_instance> profitable_subgraphs;
	  for (slp_instance instance : BB_VINFO_SLP_INSTANCES (bb_vinfo))
	    {
	      if (instance->subgraph_entries.is_empty ())
		continue;

	      dump_user_location_t saved_vect_location = vect_location;
	      vect_location = instance->location ();
	      if (!unlimited_cost_model (NULL)
		  && !vect_bb_vectorization_profitable_p
			(bb_vinfo, instance->subgraph_entries, orig_loop))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "not vectorized: vectorization is not "
				     "profitable.\n");
		  vect_location = saved_vect_location;
		  continue;
		}

	      vect_location = saved_vect_location;
	      if (!dbg_cnt (vect_slp))
		{
		  force_clear = true;
		  continue;
		}

	      profitable_subgraphs.safe_push (instance);
	    }

	  /* When we're vectorizing an if-converted loop body make sure
	     we vectorized all if-converted code.  */
	  if ((!profitable_subgraphs.is_empty () || force_clear) && orig_loop)
	    {
	      gcc_assert (bb_vinfo->nbbs == 1);
	      for (gimple_stmt_iterator gsi = gsi_start_bb (bb_vinfo->bbs[0]);
		   !gsi_end_p (gsi); gsi_next (&gsi))
		{
		  /* The costing above left us with DCEable vectorized scalar
		     stmts having the visited flag set on profitable
		     subgraphs.  Do the delayed clearing of the flag here.  */
		  if (gimple_visited_p (gsi_stmt (gsi)))
		    {
		      gimple_set_visited (gsi_stmt (gsi), false);
		      continue;
		    }
		  if (flag_vect_cost_model == VECT_COST_MODEL_UNLIMITED)
		    continue;

		  if (gassign *ass = dyn_cast <gassign *> (gsi_stmt (gsi)))
		    if (gimple_assign_rhs_code (ass) == COND_EXPR)
		      {
			if (!profitable_subgraphs.is_empty ()
			    && dump_enabled_p ())
			  dump_printf_loc (MSG_NOTE, vect_location,
					   "not profitable because of "
					   "unprofitable if-converted scalar "
					   "code\n");
			profitable_subgraphs.truncate (0);
		      }
		}
	    }

	  /* Finally schedule the profitable subgraphs.  */
	  for (slp_instance instance : profitable_subgraphs)
	    {
	      if (!vectorized && dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Basic block will be vectorized "
				 "using SLP\n");
	      vectorized = true;

	      /* Dump before scheduling as store vectorization will remove
		 the original stores and mess with the instance tree
		 so querying its location will eventually ICE.  */
	      if (flag_checking)
		for (slp_instance sub : instance->subgraph_entries)
		  gcc_assert (SLP_TREE_VECTYPE (SLP_INSTANCE_TREE (sub)));
	      unsigned HOST_WIDE_INT bytes;
	      if (dump_enabled_p ())
		for (slp_instance sub : instance->subgraph_entries)
		  {
		    tree vtype = SLP_TREE_VECTYPE (SLP_INSTANCE_TREE (sub));
		    if (GET_MODE_SIZE (TYPE_MODE (vtype)).is_constant (&bytes))
		      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS,
				       sub->location (),
				       "basic block part vectorized using %wu "
				       "byte vectors\n", bytes);
		    else
		      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS,
				       sub->location (),
				       "basic block part vectorized using "
				       "variable length vectors\n");
		  }

	      dump_user_location_t saved_vect_location = vect_location;
	      vect_location = instance->location ();

	      vect_schedule_slp (bb_vinfo, instance->subgraph_entries);

	      vect_location = saved_vect_location;
	    }


	  /* Generate the invariant statements.  */
	  if (!gimple_seq_empty_p (bb_vinfo->inv_pattern_def_seq))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
			 "------>generating invariant statements\n");

	      bb_vinfo->insert_seq_on_entry (NULL,
					     bb_vinfo->inv_pattern_def_seq);
	    }
	}
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Analysis failed with vector mode %s\n",
			     GET_MODE_NAME (bb_vinfo->vector_mode));
	}

      if (mode_i == 0)
	autodetected_vector_mode = bb_vinfo->vector_mode;

      if (!fatal)
	while (mode_i < vector_modes.length ()
	       && vect_chooses_same_modes_p (bb_vinfo, vector_modes[mode_i]))
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "***** The result for vector mode %s would"
			       " be the same\n",
			       GET_MODE_NAME (vector_modes[mode_i]));
	    mode_i += 1;
	  }

      delete bb_vinfo;

      if (mode_i < vector_modes.length ()
	  && VECTOR_MODE_P (autodetected_vector_mode)
	  && (related_vector_mode (vector_modes[mode_i],
				   GET_MODE_INNER (autodetected_vector_mode))
	      == autodetected_vector_mode)
	  && (related_vector_mode (autodetected_vector_mode,
				   GET_MODE_INNER (vector_modes[mode_i]))
	      == vector_modes[mode_i]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "***** Skipping vector mode %s, which would"
			     " repeat the analysis for %s\n",
			     GET_MODE_NAME (vector_modes[mode_i]),
			     GET_MODE_NAME (autodetected_vector_mode));
	  mode_i += 1;
	}

      if (vectorized
	  || mode_i == vector_modes.length ()
	  || autodetected_vector_mode == VOIDmode
	  /* If vect_slp_analyze_bb_1 signaled that analysis for all
	     vector sizes will fail do not bother iterating.  */
	  || fatal)
	return vectorized;

      /* Try the next biggest vector size.  */
      next_vector_mode = vector_modes[mode_i++];
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "***** Re-trying analysis with vector mode %s\n",
			 GET_MODE_NAME (next_vector_mode));
    }
}


/* Main entry for the BB vectorizer.  Analyze and transform BBS, returns
   true if anything in the basic-block was vectorized.  */

static bool
vect_slp_bbs (const vec<basic_block> &bbs, loop_p orig_loop)
{
  vec<data_reference_p> datarefs = vNULL;
  auto_vec<int> dataref_groups;
  int insns = 0;
  int current_group = 0;

  for (unsigned i = 0; i < bbs.length (); i++)
    {
      basic_block bb = bbs[i];
      for (gimple_stmt_iterator gsi = gsi_after_labels (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt))
	    continue;

	  insns++;

	  if (gimple_location (stmt) != UNKNOWN_LOCATION)
	    vect_location = stmt;

	  if (!vect_find_stmt_data_reference (NULL, stmt, &datarefs,
					      &dataref_groups, current_group))
	    ++current_group;
	}
      /* New BBs always start a new DR group.  */
      ++current_group;
    }

  return vect_slp_region (bbs, datarefs, &dataref_groups, insns, orig_loop);
}

/* Special entry for the BB vectorizer.  Analyze and transform a single
   if-converted BB with ORIG_LOOPs body being the not if-converted
   representation.  Returns true if anything in the basic-block was
   vectorized.  */

bool
vect_slp_if_converted_bb (basic_block bb, loop_p orig_loop)
{
  auto_vec<basic_block> bbs;
  bbs.safe_push (bb);
  return vect_slp_bbs (bbs, orig_loop);
}

/* Main entry for the BB vectorizer.  Analyze and transform BB, returns
   true if anything in the basic-block was vectorized.  */

bool
vect_slp_function (function *fun)
{
  bool r = false;
  int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fun));
  auto_bitmap exit_bbs;
  bitmap_set_bit (exit_bbs, EXIT_BLOCK);
  edge entry = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (fun));
  unsigned n = rev_post_order_and_mark_dfs_back_seme (fun, entry, exit_bbs,
						      true, rpo, NULL);

  /* For the moment split the function into pieces to avoid making
     the iteration on the vector mode moot.  Split at points we know
     to not handle well which is CFG merges (SLP discovery doesn't
     handle non-loop-header PHIs) and loop exits.  Since pattern
     recog requires reverse iteration to visit uses before defs
     simply chop RPO into pieces.  */
  auto_vec<basic_block> bbs;
  for (unsigned i = 0; i < n; i++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (fun, rpo[i]);
      bool split = false;

      /* Split when a BB is not dominated by the first block.  */
      if (!bbs.is_empty ()
	  && !dominated_by_p (CDI_DOMINATORS, bb, bbs[0]))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "splitting region at dominance boundary bb%d\n",
			     bb->index);
	  split = true;
	}
      /* Split when the loop determined by the first block
	 is exited.  This is because we eventually insert
	 invariants at region begin.  */
      else if (!bbs.is_empty ()
	       && bbs[0]->loop_father != bb->loop_father
	       && !flow_loop_nested_p (bbs[0]->loop_father, bb->loop_father))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "splitting region at loop %d exit at bb%d\n",
			     bbs[0]->loop_father->num, bb->index);
	  split = true;
	}
      else if (!bbs.is_empty ()
	       && bb->loop_father->header == bb
	       && bb->loop_father->dont_vectorize)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "splitting region at dont-vectorize loop %d "
			     "entry at bb%d\n",
			     bb->loop_father->num, bb->index);
	  split = true;
	}

      if (split && !bbs.is_empty ())
	{
	  r |= vect_slp_bbs (bbs, NULL);
	  bbs.truncate (0);
	}

      if (bbs.is_empty ())
	{
	  /* We need to be able to insert at the head of the region which
	     we cannot for region starting with a returns-twice call.  */
	  if (gcall *first = safe_dyn_cast <gcall *> (first_stmt (bb)))
	    if (gimple_call_flags (first) & ECF_RETURNS_TWICE)
	      {
		if (dump_enabled_p ())
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "skipping bb%d as start of region as it "
				   "starts with returns-twice call\n",
				   bb->index);
		continue;
	      }
	  /* If the loop this BB belongs to is marked as not to be vectorized
	     honor that also for BB vectorization.  */
	  if (bb->loop_father->dont_vectorize)
	    continue;
	}

      bbs.safe_push (bb);

      /* When we have a stmt ending this block and defining a
	 value we have to insert on edges when inserting after it for
	 a vector containing its definition.  Avoid this for now.  */
      if (gimple *last = *gsi_last_bb (bb))
	if (gimple_get_lhs (last)
	    && is_ctrl_altering_stmt (last))
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "splitting region at control altering "
			       "definition %G", last);
	    r |= vect_slp_bbs (bbs, NULL);
	    bbs.truncate (0);
	  }
    }

  if (!bbs.is_empty ())
    r |= vect_slp_bbs (bbs, NULL);

  free (rpo);

  return r;
}

/* Build a variable-length vector in which the elements in ELTS are repeated
   to a fill NRESULTS vectors of type VECTOR_TYPE.  Store the vectors in
   RESULTS and add any new instructions to SEQ.

   The approach we use is:

   (1) Find a vector mode VM with integer elements of mode IM.

   (2) Replace ELTS[0:NELTS] with ELTS'[0:NELTS'], where each element of
       ELTS' has mode IM.  This involves creating NELTS' VIEW_CONVERT_EXPRs
       from small vectors to IM.

   (3) Duplicate each ELTS'[I] into a vector of mode VM.

   (4) Use a tree of interleaving VEC_PERM_EXPRs to create VMs with the
       correct byte contents.

   (5) Use VIEW_CONVERT_EXPR to cast the final VMs to the required type.

   We try to find the largest IM for which this sequence works, in order
   to cut down on the number of interleaves.  */

void
duplicate_and_interleave (vec_info *vinfo, gimple_seq *seq, tree vector_type,
			  const vec<tree> &elts, unsigned int nresults,
			  vec<tree> &results)
{
  unsigned int nelts = elts.length ();
  tree element_type = TREE_TYPE (vector_type);

  /* (1) Find a vector mode VM with integer elements of mode IM.  */
  unsigned int nvectors = 1;
  tree new_vector_type;
  tree permutes[2];
  if (!can_duplicate_and_interleave_p (vinfo, nelts, element_type,
				       &nvectors, &new_vector_type,
				       permutes))
    gcc_unreachable ();

  /* Get a vector type that holds ELTS[0:NELTS/NELTS'].  */
  unsigned int partial_nelts = nelts / nvectors;
  tree partial_vector_type = build_vector_type (element_type, partial_nelts);

  tree_vector_builder partial_elts;
  auto_vec<tree, 32> pieces (nvectors * 2);
  pieces.quick_grow_cleared (nvectors * 2);
  for (unsigned int i = 0; i < nvectors; ++i)
    {
      /* (2) Replace ELTS[0:NELTS] with ELTS'[0:NELTS'], where each element of
	     ELTS' has mode IM.  */
      partial_elts.new_vector (partial_vector_type, partial_nelts, 1);
      for (unsigned int j = 0; j < partial_nelts; ++j)
	partial_elts.quick_push (elts[i * partial_nelts + j]);
      tree t = gimple_build_vector (seq, &partial_elts);
      t = gimple_build (seq, VIEW_CONVERT_EXPR,
			TREE_TYPE (new_vector_type), t);

      /* (3) Duplicate each ELTS'[I] into a vector of mode VM.  */
      pieces[i] = gimple_build_vector_from_val (seq, new_vector_type, t);
    }

  /* (4) Use a tree of VEC_PERM_EXPRs to create a single VM with the
	 correct byte contents.

     Conceptually, we need to repeat the following operation log2(nvectors)
     times, where hi_start = nvectors / 2:

	out[i * 2] = VEC_PERM_EXPR (in[i], in[i + hi_start], lo_permute);
	out[i * 2 + 1] = VEC_PERM_EXPR (in[i], in[i + hi_start], hi_permute);

     However, if each input repeats every N elements and the VF is
     a multiple of N * 2, the HI result is the same as the LO result.
     This will be true for the first N1 iterations of the outer loop,
     followed by N2 iterations for which both the LO and HI results
     are needed.  I.e.:

	N1 + N2 = log2(nvectors)

     Each "N1 iteration" doubles the number of redundant vectors and the
     effect of the process as a whole is to have a sequence of nvectors/2**N1
     vectors that repeats 2**N1 times.  Rather than generate these redundant
     vectors, we halve the number of vectors for each N1 iteration.  */
  unsigned int in_start = 0;
  unsigned int out_start = nvectors;
  unsigned int new_nvectors = nvectors;
  for (unsigned int in_repeat = 1; in_repeat < nvectors; in_repeat *= 2)
    {
      unsigned int hi_start = new_nvectors / 2;
      unsigned int out_i = 0;
      for (unsigned int in_i = 0; in_i < new_nvectors; ++in_i)
	{
	  if ((in_i & 1) != 0
	      && multiple_p (TYPE_VECTOR_SUBPARTS (new_vector_type),
			     2 * in_repeat))
	    continue;

	  tree output = make_ssa_name (new_vector_type);
	  tree input1 = pieces[in_start + (in_i / 2)];
	  tree input2 = pieces[in_start + (in_i / 2) + hi_start];
	  gassign *stmt = gimple_build_assign (output, VEC_PERM_EXPR,
					       input1, input2,
					       permutes[in_i & 1]);
	  gimple_seq_add_stmt (seq, stmt);
	  pieces[out_start + out_i] = output;
	  out_i += 1;
	}
      std::swap (in_start, out_start);
      new_nvectors = out_i;
    }

  /* (5) Use VIEW_CONVERT_EXPR to cast the final VM to the required type.  */
  results.reserve (nresults);
  for (unsigned int i = 0; i < nresults; ++i)
    if (i < new_nvectors)
      results.quick_push (gimple_build (seq, VIEW_CONVERT_EXPR, vector_type,
					pieces[in_start + i]));
    else
      results.quick_push (results[i - new_nvectors]);
}


/* For constant and loop invariant defs in OP_NODE this function creates
   vector defs that will be used in the vectorized stmts and stores them
   to SLP_TREE_VEC_DEFS of OP_NODE.  */

static void
vect_create_constant_vectors (vec_info *vinfo, slp_tree op_node)
{
  unsigned HOST_WIDE_INT nunits;
  tree vec_cst;
  unsigned j, number_of_places_left_in_vector;
  tree vector_type;
  tree vop;
  int group_size = op_node->ops.length ();
  unsigned int vec_num, i;
  unsigned number_of_copies = 1;
  bool constant_p;
  gimple_seq ctor_seq = NULL;
  auto_vec<tree, 16> permute_results;

  /* We always want SLP_TREE_VECTYPE (op_node) here correctly set.  */
  vector_type = SLP_TREE_VECTYPE (op_node);

  unsigned int number_of_vectors = SLP_TREE_NUMBER_OF_VEC_STMTS (op_node);
  SLP_TREE_VEC_DEFS (op_node).create (number_of_vectors);
  auto_vec<tree> voprnds (number_of_vectors);

  /* NUMBER_OF_COPIES is the number of times we need to use the same values in
     created vectors. It is greater than 1 if unrolling is performed.

     For example, we have two scalar operands, s1 and s2 (e.g., group of
     strided accesses of size two), while NUNITS is four (i.e., four scalars
     of this type can be packed in a vector).  The output vector will contain
     two copies of each scalar operand: {s1, s2, s1, s2}.  (NUMBER_OF_COPIES
     will be 2).

     If GROUP_SIZE > NUNITS, the scalars will be split into several vectors
     containing the operands.

     For example, NUNITS is four as before, and the group size is 8
     (s1, s2, ..., s8).  We will create two vectors {s1, s2, s3, s4} and
     {s5, s6, s7, s8}.  */

  /* When using duplicate_and_interleave, we just need one element for
     each scalar statement.  */
  if (!TYPE_VECTOR_SUBPARTS (vector_type).is_constant (&nunits))
    nunits = group_size;

  number_of_copies = nunits * number_of_vectors / group_size;

  number_of_places_left_in_vector = nunits;
  constant_p = true;
  tree uniform_elt = NULL_TREE;
  tree_vector_builder elts (vector_type, nunits, 1);
  elts.quick_grow (nunits);
  stmt_vec_info insert_after = NULL;
  for (j = 0; j < number_of_copies; j++)
    {
      tree op;
      for (i = group_size - 1; op_node->ops.iterate (i, &op); i--)
        {
          /* Create 'vect_ = {op0,op1,...,opn}'.  */
	  tree orig_op = op;
	  if (number_of_places_left_in_vector == nunits)
	    uniform_elt = op;
	  else if (uniform_elt && operand_equal_p (uniform_elt, op))
	    op = elts[number_of_places_left_in_vector];
	  else
	    uniform_elt = NULL_TREE;
	  number_of_places_left_in_vector--;
	  if (!types_compatible_p (TREE_TYPE (vector_type), TREE_TYPE (op)))
	    {
	      if (CONSTANT_CLASS_P (op))
		{
		  if (VECTOR_BOOLEAN_TYPE_P (vector_type))
		    {
		      /* Can't use VIEW_CONVERT_EXPR for booleans because
			 of possibly different sizes of scalar value and
			 vector element.  */
		      if (integer_zerop (op))
			op = build_int_cst (TREE_TYPE (vector_type), 0);
		      else if (integer_onep (op))
			op = build_all_ones_cst (TREE_TYPE (vector_type));
		      else
			gcc_unreachable ();
		    }
		  else
		    op = fold_unary (VIEW_CONVERT_EXPR,
				     TREE_TYPE (vector_type), op);
		  gcc_assert (op && CONSTANT_CLASS_P (op));
		}
	      else
		{
		  tree new_temp = make_ssa_name (TREE_TYPE (vector_type));
		  gimple *init_stmt;
		  if (VECTOR_BOOLEAN_TYPE_P (vector_type))
		    {
		      tree true_val
			= build_all_ones_cst (TREE_TYPE (vector_type));
		      tree false_val
			= build_zero_cst (TREE_TYPE (vector_type));
		      gcc_assert (INTEGRAL_TYPE_P (TREE_TYPE (op)));
		      init_stmt = gimple_build_assign (new_temp, COND_EXPR,
						       op, true_val,
						       false_val);
		    }
		  else
		    {
		      op = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (vector_type),
				   op);
		      init_stmt
			= gimple_build_assign (new_temp, VIEW_CONVERT_EXPR,
					       op);
		    }
		  gimple_seq_add_stmt (&ctor_seq, init_stmt);
		  op = new_temp;
		}
	    }
	  elts[number_of_places_left_in_vector] = op;
	  if (!CONSTANT_CLASS_P (op))
	    constant_p = false;
	  /* For BB vectorization we have to compute an insert location
	     when a def is inside the analyzed region since we cannot
	     simply insert at the BB start in this case.  */
	  stmt_vec_info opdef;
	  if (TREE_CODE (orig_op) == SSA_NAME
	      && !SSA_NAME_IS_DEFAULT_DEF (orig_op)
	      && is_a <bb_vec_info> (vinfo)
	      && (opdef = vinfo->lookup_def (orig_op)))
	    {
	      if (!insert_after)
		insert_after = opdef;
	      else
		insert_after = get_later_stmt (insert_after, opdef);
	    }

          if (number_of_places_left_in_vector == 0)
            {
	      auto type_nunits = TYPE_VECTOR_SUBPARTS (vector_type);
	      if (uniform_elt)
		vec_cst = gimple_build_vector_from_val (&ctor_seq, vector_type,
							elts[0]);
	      else if (constant_p
		       ? multiple_p (type_nunits, nunits)
		       : known_eq (type_nunits, nunits))
		vec_cst = gimple_build_vector (&ctor_seq, &elts);
	      else
		{
		  if (permute_results.is_empty ())
		    duplicate_and_interleave (vinfo, &ctor_seq, vector_type,
					      elts, number_of_vectors,
					      permute_results);
		  vec_cst = permute_results[number_of_vectors - j - 1];
		}
	      if (!gimple_seq_empty_p (ctor_seq))
		{
		  if (insert_after)
		    {
		      gimple_stmt_iterator gsi;
		      if (gimple_code (insert_after->stmt) == GIMPLE_PHI)
			{
			  gsi = gsi_after_labels (gimple_bb (insert_after->stmt));
			  gsi_insert_seq_before (&gsi, ctor_seq,
						 GSI_CONTINUE_LINKING);
			}
		      else if (!stmt_ends_bb_p (insert_after->stmt))
			{
			  gsi = gsi_for_stmt (insert_after->stmt);
			  gsi_insert_seq_after (&gsi, ctor_seq,
						GSI_CONTINUE_LINKING);
			}
		      else
			{
			  /* When we want to insert after a def where the
			     defining stmt throws then insert on the fallthru
			     edge.  */
			  edge e = find_fallthru_edge
				     (gimple_bb (insert_after->stmt)->succs);
			  basic_block new_bb
			    = gsi_insert_seq_on_edge_immediate (e, ctor_seq);
			  gcc_assert (!new_bb);
			}
		    }
		  else
		    vinfo->insert_seq_on_entry (NULL, ctor_seq);
		  ctor_seq = NULL;
		}
	      voprnds.quick_push (vec_cst);
	      insert_after = NULL;
              number_of_places_left_in_vector = nunits;
	      constant_p = true;
	      elts.new_vector (vector_type, nunits, 1);
	      elts.quick_grow (nunits);
            }
        }
    }

  /* Since the vectors are created in the reverse order, we should invert
     them.  */
  vec_num = voprnds.length ();
  for (j = vec_num; j != 0; j--)
    {
      vop = voprnds[j - 1];
      SLP_TREE_VEC_DEFS (op_node).quick_push (vop);
    }

  /* In case that VF is greater than the unrolling factor needed for the SLP
     group of stmts, NUMBER_OF_VECTORS to be created is greater than
     NUMBER_OF_SCALARS/NUNITS or NUNITS/NUMBER_OF_SCALARS, and hence we have
     to replicate the vectors.  */
  while (number_of_vectors > SLP_TREE_VEC_DEFS (op_node).length ())
    for (i = 0; SLP_TREE_VEC_DEFS (op_node).iterate (i, &vop) && i < vec_num;
	 i++)
      SLP_TREE_VEC_DEFS (op_node).quick_push (vop);
}

/* Get the Ith vectorized definition from SLP_NODE.  */

tree
vect_get_slp_vect_def (slp_tree slp_node, unsigned i)
{
  return SLP_TREE_VEC_DEFS (slp_node)[i];
}

/* Get the vectorized definitions of SLP_NODE in *VEC_DEFS.  */

void
vect_get_slp_defs (slp_tree slp_node, vec<tree> *vec_defs)
{
  vec_defs->create (SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node));
  vec_defs->splice (SLP_TREE_VEC_DEFS (slp_node));
}

/* Get N vectorized definitions for SLP_NODE.  */

void
vect_get_slp_defs (vec_info *,
		   slp_tree slp_node, vec<vec<tree> > *vec_oprnds, unsigned n)
{
  if (n == -1U)
    n = SLP_TREE_CHILDREN (slp_node).length ();

  for (unsigned i = 0; i < n; ++i)
    {
      slp_tree child = SLP_TREE_CHILDREN (slp_node)[i];
      vec<tree> vec_defs = vNULL;
      vect_get_slp_defs (child, &vec_defs);
      vec_oprnds->quick_push (vec_defs);
    }
}

/* A subroutine of vect_transform_slp_perm_load with two extra arguments:
   - PERM gives the permutation that the caller wants to use for NODE,
     which might be different from SLP_LOAD_PERMUTATION.
   - DUMP_P controls whether the function dumps information.  */

static bool
vect_transform_slp_perm_load_1 (vec_info *vinfo, slp_tree node,
				load_permutation_t &perm,
				const vec<tree> &dr_chain,
				gimple_stmt_iterator *gsi, poly_uint64 vf,
				bool analyze_only, bool dump_p,
				unsigned *n_perms, unsigned int *n_loads,
				bool dce_chain)
{
  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
  int vec_index = 0;
  tree vectype = SLP_TREE_VECTYPE (node);
  unsigned int group_size = SLP_TREE_SCALAR_STMTS (node).length ();
  unsigned int mask_element;
  unsigned dr_group_size;
  machine_mode mode;

  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    dr_group_size = 1;
  else
    {
      stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      dr_group_size = DR_GROUP_SIZE (stmt_info);
    }

  mode = TYPE_MODE (vectype);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  unsigned int nstmts = SLP_TREE_NUMBER_OF_VEC_STMTS (node);

  /* Initialize the vect stmts of NODE to properly insert the generated
     stmts later.  */
  if (! analyze_only)
    for (unsigned i = SLP_TREE_VEC_DEFS (node).length (); i < nstmts; i++)
      SLP_TREE_VEC_DEFS (node).quick_push (NULL_TREE);

  /* Generate permutation masks for every NODE. Number of masks for each NODE
     is equal to GROUP_SIZE.
     E.g., we have a group of three nodes with three loads from the same
     location in each node, and the vector size is 4. I.e., we have a
     a0b0c0a1b1c1... sequence and we need to create the following vectors:
     for a's: a0a0a0a1 a1a1a2a2 a2a3a3a3
     for b's: b0b0b0b1 b1b1b2b2 b2b3b3b3
     ...

     The masks for a's should be: {0,0,0,3} {3,3,6,6} {6,9,9,9}.
     The last mask is illegal since we assume two operands for permute
     operation, and the mask element values can't be outside that range.
     Hence, the last mask must be converted into {2,5,5,5}.
     For the first two permutations we need the first and the second input
     vectors: {a0,b0,c0,a1} and {b1,c1,a2,b2}, and for the last permutation
     we need the second and the third vectors: {b1,c1,a2,b2} and
     {c2,a3,b3,c3}.  */

  int vect_stmts_counter = 0;
  unsigned int index = 0;
  int first_vec_index = -1;
  int second_vec_index = -1;
  bool noop_p = true;
  *n_perms = 0;

  vec_perm_builder mask;
  unsigned int nelts_to_build;
  unsigned int nvectors_per_build;
  unsigned int in_nlanes;
  bool repeating_p = (group_size == dr_group_size
		      && multiple_p (nunits, group_size));
  if (repeating_p)
    {
      /* A single vector contains a whole number of copies of the node, so:
	 (a) all permutes can use the same mask; and
	 (b) the permutes only need a single vector input.  */
      mask.new_vector (nunits, group_size, 3);
      nelts_to_build = mask.encoded_nelts ();
      /* It's possible to obtain zero nstmts during analyze_only, so make
	 it at least one to ensure the later computation for n_perms
	 proceed.  */
      nvectors_per_build = nstmts > 0 ? nstmts : 1;
      in_nlanes = dr_group_size * 3;
    }
  else
    {
      /* We need to construct a separate mask for each vector statement.  */
      unsigned HOST_WIDE_INT const_nunits, const_vf;
      if (!nunits.is_constant (&const_nunits)
	  || !vf.is_constant (&const_vf))
	return false;
      mask.new_vector (const_nunits, const_nunits, 1);
      nelts_to_build = const_vf * group_size;
      nvectors_per_build = 1;
      in_nlanes = const_vf * dr_group_size;
    }
  auto_sbitmap used_in_lanes (in_nlanes);
  bitmap_clear (used_in_lanes);
  auto_bitmap used_defs;

  unsigned int count = mask.encoded_nelts ();
  mask.quick_grow (count);
  vec_perm_indices indices;

  for (unsigned int j = 0; j < nelts_to_build; j++)
    {
      unsigned int iter_num = j / group_size;
      unsigned int stmt_num = j % group_size;
      unsigned int i = (iter_num * dr_group_size + perm[stmt_num]);
      bitmap_set_bit (used_in_lanes, i);
      if (repeating_p)
	{
	  first_vec_index = 0;
	  mask_element = i;
	}
      else
	{
	  /* Enforced before the loop when !repeating_p.  */
	  unsigned int const_nunits = nunits.to_constant ();
	  vec_index = i / const_nunits;
	  mask_element = i % const_nunits;
	  if (vec_index == first_vec_index
	      || first_vec_index == -1)
	    {
	      first_vec_index = vec_index;
	    }
	  else if (vec_index == second_vec_index
		   || second_vec_index == -1)
	    {
	      second_vec_index = vec_index;
	      mask_element += const_nunits;
	    }
	  else
	    {
	      if (dump_p)
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "permutation requires at "
				 "least three vectors %G",
				 stmt_info->stmt);
	      gcc_assert (analyze_only);
	      return false;
	    }

	  gcc_assert (mask_element < 2 * const_nunits);
	}

      if (mask_element != index)
	noop_p = false;
      mask[index++] = mask_element;

      if (index == count)
	{
	  if (!noop_p)
	    {
	      indices.new_vector (mask, second_vec_index == -1 ? 1 : 2, nunits);
	      if (!can_vec_perm_const_p (mode, mode, indices))
		{
		  if (dump_p)
		    {
		      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				       "unsupported vect permute { ");
		      for (i = 0; i < count; ++i)
			{
			  dump_dec (MSG_MISSED_OPTIMIZATION, mask[i]);
			  dump_printf (MSG_MISSED_OPTIMIZATION, " ");
			}
		      dump_printf (MSG_MISSED_OPTIMIZATION, "}\n");
		    }
		  gcc_assert (analyze_only);
		  return false;
		}

	      tree mask_vec = NULL_TREE;
	      if (!analyze_only)
		mask_vec = vect_gen_perm_mask_checked (vectype, indices);

	      if (second_vec_index == -1)
		second_vec_index = first_vec_index;

	      for (unsigned int ri = 0; ri < nvectors_per_build; ++ri)
		{
		  ++*n_perms;
		  if (analyze_only)
		    continue;
		  /* Generate the permute statement if necessary.  */
		  tree first_vec = dr_chain[first_vec_index + ri];
		  tree second_vec = dr_chain[second_vec_index + ri];
		  gassign *stmt = as_a<gassign *> (stmt_info->stmt);
		  tree perm_dest
		    = vect_create_destination_var (gimple_assign_lhs (stmt),
						   vectype);
		  perm_dest = make_ssa_name (perm_dest);
		  gimple *perm_stmt
		    = gimple_build_assign (perm_dest, VEC_PERM_EXPR, first_vec,
					   second_vec, mask_vec);
		  vect_finish_stmt_generation (vinfo, stmt_info, perm_stmt,
					       gsi);
		  if (dce_chain)
		    {
		      bitmap_set_bit (used_defs, first_vec_index + ri);
		      bitmap_set_bit (used_defs, second_vec_index + ri);
		    }

		  /* Store the vector statement in NODE.  */
		  SLP_TREE_VEC_DEFS (node)[vect_stmts_counter++] = perm_dest;
		}
	    }
	  else if (!analyze_only)
	    {
	      for (unsigned int ri = 0; ri < nvectors_per_build; ++ri)
		{
		  tree first_vec = dr_chain[first_vec_index + ri];
		  /* If mask was NULL_TREE generate the requested
		     identity transform.  */
		  if (dce_chain)
		    bitmap_set_bit (used_defs, first_vec_index + ri);

		  /* Store the vector statement in NODE.  */
		  SLP_TREE_VEC_DEFS (node)[vect_stmts_counter++] = first_vec;
		}
	    }

	  index = 0;
	  first_vec_index = -1;
	  second_vec_index = -1;
	  noop_p = true;
	}
    }

  if (n_loads)
    {
      if (repeating_p)
	*n_loads = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
      else
	{
	  /* Enforced above when !repeating_p.  */
	  unsigned int const_nunits = nunits.to_constant ();
	  *n_loads = 0;
	  bool load_seen = false;
	  for (unsigned i = 0; i < in_nlanes; ++i)
	    {
	      if (i % const_nunits == 0)
		{
		  if (load_seen)
		    *n_loads += 1;
		  load_seen = false;
		}
	      if (bitmap_bit_p (used_in_lanes, i))
		load_seen = true;
	    }
	  if (load_seen)
	    *n_loads += 1;
	}
    }

  if (dce_chain)
    for (unsigned i = 0; i < dr_chain.length (); ++i)
      if (!bitmap_bit_p (used_defs, i))
	{
	  tree def = dr_chain[i];
	  do
	    {
	      gimple *stmt = SSA_NAME_DEF_STMT (def);
	      if (is_gimple_assign (stmt)
		  && (gimple_assign_rhs_code (stmt) == VIEW_CONVERT_EXPR
		      || gimple_assign_rhs_code (stmt) == CONSTRUCTOR))
		def = single_ssa_tree_operand (stmt, SSA_OP_USE);
	      else
		def = NULL;
	      gimple_stmt_iterator rgsi = gsi_for_stmt (stmt);
	      gsi_remove (&rgsi, true);
	      release_defs (stmt);
	    }
	  while (def);
	}

  return true;
}

/* Generate vector permute statements from a list of loads in DR_CHAIN.
   If ANALYZE_ONLY is TRUE, only check that it is possible to create valid
   permute statements for the SLP node NODE.  Store the number of vector
   permute instructions in *N_PERMS and the number of vector load
   instructions in *N_LOADS.  If DCE_CHAIN is true, remove all definitions
   that were not needed.  */

bool
vect_transform_slp_perm_load (vec_info *vinfo,
			      slp_tree node, const vec<tree> &dr_chain,
			      gimple_stmt_iterator *gsi, poly_uint64 vf,
			      bool analyze_only, unsigned *n_perms,
			      unsigned int *n_loads, bool dce_chain)
{
  return vect_transform_slp_perm_load_1 (vinfo, node,
					 SLP_TREE_LOAD_PERMUTATION (node),
					 dr_chain, gsi, vf, analyze_only,
					 dump_enabled_p (), n_perms, n_loads,
					 dce_chain);
}

/* Produce the next vector result for SLP permutation NODE by adding a vector
   statement at GSI.  If MASK_VEC is nonnull, add:

      <new SSA name> = VEC_PERM_EXPR <FIRST_DEF, SECOND_DEF, MASK_VEC>

   otherwise add:

      <new SSA name> = FIRST_DEF.  */

static void
vect_add_slp_permutation (vec_info *vinfo, gimple_stmt_iterator *gsi,
			  slp_tree node, tree first_def, tree second_def,
			  tree mask_vec, poly_uint64 identity_offset)
{
  tree vectype = SLP_TREE_VECTYPE (node);

  /* ???  We SLP match existing vector element extracts but
     allow punning which we need to re-instantiate at uses
     but have no good way of explicitly representing.  */
  if (operand_equal_p (TYPE_SIZE (TREE_TYPE (first_def)), TYPE_SIZE (vectype))
      && !types_compatible_p (TREE_TYPE (first_def), vectype))
    {
      gassign *conv_stmt
	= gimple_build_assign (make_ssa_name (vectype),
			       build1 (VIEW_CONVERT_EXPR, vectype, first_def));
      vect_finish_stmt_generation (vinfo, NULL, conv_stmt, gsi);
      first_def = gimple_assign_lhs (conv_stmt);
    }
  gassign *perm_stmt;
  tree perm_dest = make_ssa_name (vectype);
  if (mask_vec)
    {
      if (operand_equal_p (TYPE_SIZE (TREE_TYPE (first_def)),
			   TYPE_SIZE (vectype))
	  && !types_compatible_p (TREE_TYPE (second_def), vectype))
	{
	  gassign *conv_stmt
	    = gimple_build_assign (make_ssa_name (vectype),
				   build1 (VIEW_CONVERT_EXPR,
					   vectype, second_def));
	  vect_finish_stmt_generation (vinfo, NULL, conv_stmt, gsi);
	  second_def = gimple_assign_lhs (conv_stmt);
	}
      perm_stmt = gimple_build_assign (perm_dest, VEC_PERM_EXPR,
				       first_def, second_def,
				       mask_vec);
    }
  else if (!types_compatible_p (TREE_TYPE (first_def), vectype))
    {
      /* For identity permutes we still need to handle the case
	 of offsetted extracts or concats.  */
      unsigned HOST_WIDE_INT c;
      auto first_def_nunits
	= TYPE_VECTOR_SUBPARTS (TREE_TYPE (first_def));
      if (known_le (TYPE_VECTOR_SUBPARTS (vectype), first_def_nunits))
	{
	  unsigned HOST_WIDE_INT elsz
	    = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (TREE_TYPE (first_def))));
	  tree lowpart = build3 (BIT_FIELD_REF, vectype, first_def,
				 TYPE_SIZE (vectype),
				 bitsize_int (identity_offset * elsz));
	  perm_stmt = gimple_build_assign (perm_dest, lowpart);
	}
      else if (constant_multiple_p (TYPE_VECTOR_SUBPARTS (vectype),
				    first_def_nunits, &c) && c == 2)
	{
	  tree ctor = build_constructor_va (vectype, 2, NULL_TREE, first_def,
					    NULL_TREE, second_def);
	  perm_stmt = gimple_build_assign (perm_dest, ctor);
	}
      else
	gcc_unreachable ();
    }
  else
    {
      /* We need a copy here in case the def was external.  */
      perm_stmt = gimple_build_assign (perm_dest, first_def);
    }
  vect_finish_stmt_generation (vinfo, NULL, perm_stmt, gsi);
  /* Store the vector statement in NODE.  */
  node->push_vec_def (perm_stmt);
}

/* Subroutine of vectorizable_slp_permutation.  Check whether the target
   can perform permutation PERM on the (1 or 2) input nodes in CHILDREN.
   If GSI is nonnull, emit the permutation there.

   When GSI is null, the only purpose of NODE is to give properties
   of the result, such as the vector type and number of SLP lanes.
   The node does not need to be a VEC_PERM_EXPR.

   If the target supports the operation, return the number of individual
   VEC_PERM_EXPRs needed, otherwise return -1.  Print information to the
   dump file if DUMP_P is true.  */

static int
vectorizable_slp_permutation_1 (vec_info *vinfo, gimple_stmt_iterator *gsi,
				slp_tree node, lane_permutation_t &perm,
				vec<slp_tree> &children, bool dump_p)
{
  tree vectype = SLP_TREE_VECTYPE (node);

  /* ???  We currently only support all same vector input types
     while the SLP IL should really do a concat + select and thus accept
     arbitrary mismatches.  */
  slp_tree child;
  unsigned i;
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  bool repeating_p = multiple_p (nunits, SLP_TREE_LANES (node));
  /* True if we're permuting a single input of 2N vectors down
     to N vectors.  This case doesn't generalize beyond 2 since
     VEC_PERM_EXPR only takes 2 inputs.  */
  bool pack_p = false;
  /* If we're permuting inputs of N vectors each into X*N outputs,
     this is the value of X, otherwise it is 1.  */
  unsigned int unpack_factor = 1;
  tree op_vectype = NULL_TREE;
  FOR_EACH_VEC_ELT (children, i, child)
    if (SLP_TREE_VECTYPE (child))
      {
	op_vectype = SLP_TREE_VECTYPE (child);
	break;
      }
  if (!op_vectype)
    op_vectype = vectype;
  FOR_EACH_VEC_ELT (children, i, child)
    {
      if ((SLP_TREE_DEF_TYPE (child) != vect_internal_def
	   && !vect_maybe_update_slp_op_vectype (child, op_vectype))
	  || !types_compatible_p (SLP_TREE_VECTYPE (child), op_vectype)
	  || !types_compatible_p (TREE_TYPE (vectype), TREE_TYPE (op_vectype)))
	{
	  if (dump_p)
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Unsupported vector types in lane permutation\n");
	  return -1;
	}
      auto op_nunits = TYPE_VECTOR_SUBPARTS (op_vectype);
      unsigned int this_unpack_factor;
      /* Detect permutations of external, pre-existing vectors.  The external
	 node's SLP_TREE_LANES stores the total number of units in the vector,
	 or zero if the vector has variable length.

	 We are expected to keep the original VEC_PERM_EXPR for such cases.
	 There is no repetition to model.  */
      if (SLP_TREE_DEF_TYPE (child) == vect_external_def
	  && SLP_TREE_SCALAR_OPS (child).is_empty ())
	repeating_p = false;
      /* Check whether the input has twice as many lanes per vector.  */
      else if (children.length () == 1
	       && known_eq (SLP_TREE_LANES (child) * nunits,
			    SLP_TREE_LANES (node) * op_nunits * 2))
	pack_p = true;
      /* Check whether the output has N times as many lanes per vector.  */
      else if (constant_multiple_p (SLP_TREE_LANES (node) * op_nunits,
				    SLP_TREE_LANES (child) * nunits,
				    &this_unpack_factor)
	       && (i == 0 || unpack_factor == this_unpack_factor))
	unpack_factor = this_unpack_factor;
      else
	repeating_p = false;
    }

  gcc_assert (perm.length () == SLP_TREE_LANES (node));

  /* Load-lanes permute.  This permute only acts as a forwarder to
     select the correct vector def of the load-lanes load which
     has the permuted vectors in its vector defs like
     { v0, w0, r0, v1, w1, r1 ... } for a ld3.  */
  if (node->ldst_lanes)
    {
      gcc_assert (children.length () == 1);
      if (!gsi)
	/* This is a trivial op always supported.  */
	return 1;
      slp_tree child = children[0];
      unsigned vec_idx = (SLP_TREE_LANE_PERMUTATION (node)[0].second
			  / SLP_TREE_LANES (node));
      unsigned vec_num = SLP_TREE_LANES (child) / SLP_TREE_LANES (node);
      for (unsigned i = 0; i < SLP_TREE_NUMBER_OF_VEC_STMTS (node); ++i)
	{
	  tree def = SLP_TREE_VEC_DEFS (child)[i * vec_num  + vec_idx];
	  node->push_vec_def (def);
	}
      return 1;
    }

  /* Set REPEATING_P to true if the permutations are cylical wrt UNPACK_FACTOR
     and if we can generate the vectors in a vector-length agnostic way.
     This requires UNPACK_STEP == NUNITS / UNPACK_FACTOR to be known at
     compile time.

     The significance of UNPACK_STEP is that, when PACK_P is false,
     output vector I operates on a window of UNPACK_STEP elements from each
     input, starting at lane UNPACK_STEP * (I % UNPACK_FACTOR).  For example,
     when UNPACK_FACTOR is 2, the first output vector operates on lanes
     [0, NUNITS / 2 - 1] of each input vector and the second output vector
     operates on lanes [NUNITS / 2, NUNITS - 1] of each input vector.

     When REPEATING_P is true, NOUTPUTS holds the total number of outputs
     that we actually need to generate.  */
  uint64_t noutputs = 0;
  poly_uint64 unpack_step = 0;
  loop_vec_info linfo = dyn_cast <loop_vec_info> (vinfo);
  if (!linfo
      || !multiple_p (nunits, unpack_factor, &unpack_step)
      || !constant_multiple_p (LOOP_VINFO_VECT_FACTOR (linfo)
			       * SLP_TREE_LANES (node), nunits, &noutputs))
    repeating_p = false;

  /* We can handle the conditions described for REPEATING_P above for
     both variable- and constant-length vectors.  The fallback requires
     us to generate every element of every permute vector explicitly,
     which is only possible for constant-length permute vectors.

     Set:

     - NPATTERNS and NELTS_PER_PATTERN to the encoding of the permute
       mask vectors that we want to build.

     - NCOPIES to the number of copies of PERM that we need in order
       to build the necessary permute mask vectors.  */
  uint64_t npatterns;
  unsigned nelts_per_pattern;
  uint64_t ncopies;
  if (repeating_p)
    {
      /* We need permute mask vectors that have the form:

	   { X1, ..., Xn, X1 + n, ..., Xn + n, X1 + 2n, ..., Xn + 2n, ... }

	 In other words, the original n-element permute in PERM is
	 "unrolled" to fill a full vector.  The stepped vector encoding
	 that we use for permutes requires 3n elements.  */
      npatterns = SLP_TREE_LANES (node);
      nelts_per_pattern = ncopies = 3;
    }
  else
    {
      /* Calculate every element of every permute mask vector explicitly,
	 instead of relying on the pattern described above.  */
      if (!nunits.is_constant (&npatterns)
	  || !TYPE_VECTOR_SUBPARTS (op_vectype).is_constant ())
	{
	  if (dump_p)
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported permutation %p on variable-length"
			     " vectors\n", (void *) node);
	  return -1;
	}
      nelts_per_pattern = ncopies = 1;
      if (linfo && !LOOP_VINFO_VECT_FACTOR (linfo).is_constant (&ncopies))
	{
	  if (dump_p)
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "unsupported permutation %p for variable VF\n",
			     (void *) node);
	  return -1;
	}
      pack_p = false;
      unpack_factor = 1;
    }
  unsigned olanes = unpack_factor * ncopies * SLP_TREE_LANES (node);
  gcc_assert (repeating_p || multiple_p (olanes, nunits));

  /* Compute the { { SLP operand, vector index}, lane } permutation sequence
     from the { SLP operand, scalar lane } permutation as recorded in the
     SLP node as intermediate step.  This part should already work
     with SLP children with arbitrary number of lanes.  */
  auto_vec<std::pair<std::pair<unsigned, unsigned>, poly_uint64>> vperm;
  auto_vec<poly_uint64> active_lane;
  vperm.create (olanes);
  active_lane.safe_grow_cleared (children.length (), true);
  for (unsigned int ui = 0; ui < unpack_factor; ++ui)
    {
      for (unsigned j = 0; j < children.length (); ++j)
	active_lane[j] = ui * unpack_step;
      for (unsigned i = 0; i < ncopies; ++i)
	{
	  for (unsigned pi = 0; pi < perm.length (); ++pi)
	    {
	      std::pair<unsigned, unsigned> p = perm[pi];
	      tree vtype = SLP_TREE_VECTYPE (children[p.first]);
	      if (repeating_p)
		vperm.quick_push ({{p.first, 0},
				   p.second + active_lane[p.first]});
	      else
		{
		  /* We checked above that the vectors are constant-length.  */
		  unsigned vnunits = TYPE_VECTOR_SUBPARTS (vtype)
		    .to_constant ();
		  unsigned lane = active_lane[p.first].to_constant ();
		  unsigned vi = (lane + p.second) / vnunits;
		  unsigned vl = (lane + p.second) % vnunits;
		  vperm.quick_push ({{p.first, vi}, vl});
		}
	    }
	  /* Advance to the next group.  */
	  for (unsigned j = 0; j < children.length (); ++j)
	    active_lane[j] += SLP_TREE_LANES (children[j]);
	}
    }

  if (dump_p)
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "vectorizing permutation %p", (void *)node);
      for (unsigned i = 0; i < perm.length (); ++i)
	dump_printf (MSG_NOTE, " op%u[%u]", perm[i].first, perm[i].second);
      if (repeating_p)
	dump_printf (MSG_NOTE, " (repeat %d)\n", SLP_TREE_LANES (node));
      dump_printf (MSG_NOTE, "\n");
      dump_printf_loc (MSG_NOTE, vect_location, "as");
      for (unsigned i = 0; i < vperm.length (); ++i)
	{
	  if (i != 0
	      && (repeating_p
		  ? multiple_p (i, npatterns)
		  : multiple_p (i, TYPE_VECTOR_SUBPARTS (vectype))))
	    dump_printf (MSG_NOTE, ",");
	  dump_printf (MSG_NOTE, " vops%u[%u][",
		       vperm[i].first.first, vperm[i].first.second);
	  dump_dec (MSG_NOTE, vperm[i].second);
	  dump_printf (MSG_NOTE, "]");
	}
      dump_printf (MSG_NOTE, "\n");
    }

  /* We can only handle two-vector permutes, everything else should
     be lowered on the SLP level.  The following is closely inspired
     by vect_transform_slp_perm_load and is supposed to eventually
     replace it.
     ???   As intermediate step do code-gen in the SLP tree representation
     somehow?  */
  std::pair<unsigned, unsigned> first_vec = std::make_pair (-1U, -1U);
  std::pair<unsigned, unsigned> second_vec = std::make_pair (-1U, -1U);
  unsigned int index = 0;
  poly_uint64 mask_element;
  vec_perm_builder mask;
  mask.new_vector (nunits, npatterns, nelts_per_pattern);
  unsigned int count = mask.encoded_nelts ();
  mask.quick_grow (count);
  vec_perm_indices indices;
  unsigned nperms = 0;
  /* When REPEATING_P is true, we only have UNPACK_FACTOR unique permute
     vectors to check during analysis, but we need to generate NOUTPUTS
     vectors during transformation.  */
  unsigned total_nelts = olanes;
  if (repeating_p && gsi)
    total_nelts = (total_nelts / unpack_factor) * noutputs;
  for (unsigned i = 0; i < total_nelts; ++i)
    {
      /* VI is the input vector index when generating code for REPEATING_P.  */
      unsigned vi = i / olanes * (pack_p ? 2 : 1);
      unsigned ei = i % olanes;
      mask_element = vperm[ei].second;
      if (pack_p)
	{
	  /* In this case, we have N outputs and the single child provides 2N
	     inputs.  Output X permutes inputs 2X and 2X+1.

	     The mask indices are taken directly from the SLP permutation node.
	     Index X selects from the first vector if (X / NUNITS) % 2 == 0;
	     X selects from the second vector otherwise.  These conditions
	     are only known at compile time for constant-length vectors.  */
	  first_vec = std::make_pair (0, 0);
	  second_vec = std::make_pair (0, 1);
	}
      else if (first_vec.first == -1U
	       || first_vec == vperm[ei].first)
	first_vec = vperm[ei].first;
      else if (second_vec.first == -1U
	       || second_vec == vperm[ei].first)
	{
	  second_vec = vperm[ei].first;
	  mask_element += nunits;
	}
      else
	{
	  if (dump_p)
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "permutation requires at "
			     "least three vectors\n");
	  gcc_assert (!gsi);
	  return -1;
	}

      mask[index++] = mask_element;

      if (index == count)
	{
	  indices.new_vector (mask, second_vec.first == -1U ? 1 : 2,
			      TYPE_VECTOR_SUBPARTS (op_vectype));
	  bool identity_p = (indices.series_p (0, 1, mask[0], 1)
			     && constant_multiple_p (mask[0], nunits));
	  machine_mode vmode = TYPE_MODE (vectype);
	  machine_mode op_vmode = TYPE_MODE (op_vectype);
	  unsigned HOST_WIDE_INT c;
	  if ((!identity_p
	       && !can_vec_perm_const_p (vmode, op_vmode, indices))
	      || (identity_p
		  && !known_le (nunits,
				TYPE_VECTOR_SUBPARTS (op_vectype))
		  && (!constant_multiple_p (nunits,
					    TYPE_VECTOR_SUBPARTS (op_vectype),
					    &c) || c != 2)))
	    {
	      if (dump_p)
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION,
				   vect_location,
				   "unsupported vect permute { ");
		  for (i = 0; i < count; ++i)
		    {
		      dump_dec (MSG_MISSED_OPTIMIZATION, mask[i]);
		      dump_printf (MSG_MISSED_OPTIMIZATION, " ");
		    }
		  dump_printf (MSG_MISSED_OPTIMIZATION, "}\n");
		}
	      gcc_assert (!gsi);
	      return -1;
	    }

	  if (!identity_p)
	    nperms++;
	  if (gsi)
	    {
	      if (second_vec.first == -1U)
		second_vec = first_vec;

	      slp_tree
		first_node = children[first_vec.first],
		second_node = children[second_vec.first];

	      tree mask_vec = NULL_TREE;
	      if (!identity_p)
		mask_vec = vect_gen_perm_mask_checked (vectype, indices);

	      tree first_def
		= vect_get_slp_vect_def (first_node, first_vec.second + vi);
	      tree second_def
		= vect_get_slp_vect_def (second_node, second_vec.second + vi);
	      vect_add_slp_permutation (vinfo, gsi, node, first_def,
					second_def, mask_vec, mask[0]);
	    }

	  index = 0;
	  first_vec = std::make_pair (-1U, -1U);
	  second_vec = std::make_pair (-1U, -1U);
	}
    }

  return nperms;
}

/* Vectorize the SLP permutations in NODE as specified
   in SLP_TREE_LANE_PERMUTATION which is a vector of pairs of SLP
   child number and lane number.
   Interleaving of two two-lane two-child SLP subtrees (not supported):
     [ { 0, 0 }, { 1, 0 }, { 0, 1 }, { 1, 1 } ]
   A blend of two four-lane two-child SLP subtrees:
     [ { 0, 0 }, { 1, 1 }, { 0, 2 }, { 1, 3 } ]
   Highpart of a four-lane one-child SLP subtree (not supported):
     [ { 0, 2 }, { 0, 3 } ]
   Where currently only a subset is supported by code generating below.  */

static bool
vectorizable_slp_permutation (vec_info *vinfo, gimple_stmt_iterator *gsi,
			      slp_tree node, stmt_vector_for_cost *cost_vec)
{
  tree vectype = SLP_TREE_VECTYPE (node);
  lane_permutation_t &perm = SLP_TREE_LANE_PERMUTATION (node);
  int nperms = vectorizable_slp_permutation_1 (vinfo, gsi, node, perm,
					       SLP_TREE_CHILDREN (node),
					       dump_enabled_p ());
  if (nperms < 0)
    return false;

  if (!gsi)
    record_stmt_cost (cost_vec, nperms, vec_perm, node, vectype, 0, vect_body);

  return true;
}

/* Vectorize SLP NODE.  */

static void
vect_schedule_slp_node (vec_info *vinfo,
			slp_tree node, slp_instance instance)
{
  gimple_stmt_iterator si;
  int i;
  slp_tree child;

  /* Vectorize externals and constants.  */
  if (SLP_TREE_DEF_TYPE (node) == vect_constant_def
      || SLP_TREE_DEF_TYPE (node) == vect_external_def)
    {
      /* ???  vectorizable_shift can end up using a scalar operand which is
	 currently denoted as !SLP_TREE_VECTYPE.  No need to vectorize the
	 node in this case.  */
      if (!SLP_TREE_VECTYPE (node))
	return;

      /* There are two reasons vector defs might already exist.  The first
	 is that we are vectorizing an existing vector def.  The second is
	 when performing BB vectorization shared constant/external nodes
	 are not split apart during partitioning so during the code-gen
	 DFS walk we can end up visiting them twice.  */
      if (! SLP_TREE_VEC_DEFS (node).exists ())
	vect_create_constant_vectors (vinfo, node);
      return;
    }

  gcc_assert (SLP_TREE_VEC_DEFS (node).is_empty ());

  stmt_vec_info stmt_info = SLP_TREE_REPRESENTATIVE (node);

  gcc_assert (SLP_TREE_NUMBER_OF_VEC_STMTS (node) != 0);
  SLP_TREE_VEC_DEFS (node).create (SLP_TREE_NUMBER_OF_VEC_STMTS (node));

  if (SLP_TREE_CODE (node) != VEC_PERM_EXPR
      && STMT_VINFO_DATA_REF (stmt_info))
    {
      /* Vectorized loads go before the first scalar load to make it
	 ready early, vectorized stores go before the last scalar
	 stmt which is where all uses are ready.  */
      stmt_vec_info last_stmt_info = NULL;
      if (DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
	last_stmt_info = vect_find_first_scalar_stmt_in_slp (node);
      else /* DR_IS_WRITE */
	last_stmt_info = vect_find_last_scalar_stmt_in_slp (node);
      si = gsi_for_stmt (last_stmt_info->stmt);
    }
  else if (SLP_TREE_CODE (node) != VEC_PERM_EXPR
	   && (STMT_VINFO_TYPE (stmt_info) == cycle_phi_info_type
	       || STMT_VINFO_TYPE (stmt_info) == induc_vec_info_type
	       || STMT_VINFO_TYPE (stmt_info) == phi_info_type))
    {
      /* For PHI node vectorization we do not use the insertion iterator.  */
      si = gsi_none ();
    }
  else
    {
      /* Emit other stmts after the children vectorized defs which is
	 earliest possible.  */
      gimple *last_stmt = NULL;
      bool seen_vector_def = false;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
	if (SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	  {
	    /* For fold-left reductions we are retaining the scalar
	       reduction PHI but we still have SLP_TREE_NUM_VEC_STMTS
	       set so the representation isn't perfect.  Resort to the
	       last scalar def here.  */
	    if (SLP_TREE_VEC_DEFS (child).is_empty ())
	      {
		gcc_assert (STMT_VINFO_TYPE (SLP_TREE_REPRESENTATIVE (child))
			    == cycle_phi_info_type);
		gphi *phi = as_a <gphi *>
			      (vect_find_last_scalar_stmt_in_slp (child)->stmt);
		if (!last_stmt
		    || vect_stmt_dominates_stmt_p (last_stmt, phi))
		  last_stmt = phi;
	      }
	    /* We are emitting all vectorized stmts in the same place and
	       the last one is the last.
	       ???  Unless we have a load permutation applied and that
	       figures to re-use an earlier generated load.  */
	    unsigned j;
	    tree vdef;
	    FOR_EACH_VEC_ELT (SLP_TREE_VEC_DEFS (child), j, vdef)
	      {
		gimple *vstmt = SSA_NAME_DEF_STMT (vdef);
		if (!last_stmt
		    || vect_stmt_dominates_stmt_p (last_stmt, vstmt))
		  last_stmt = vstmt;
	      }
	  }
	else if (!SLP_TREE_VECTYPE (child))
	  {
	    /* For externals we use unvectorized at all scalar defs.  */
	    unsigned j;
	    tree def;
	    FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_OPS (child), j, def)
	      if (TREE_CODE (def) == SSA_NAME
		  && !SSA_NAME_IS_DEFAULT_DEF (def))
		{
		  gimple *stmt = SSA_NAME_DEF_STMT (def);
		  if (!last_stmt
		      || vect_stmt_dominates_stmt_p (last_stmt, stmt))
		    last_stmt = stmt;
		}
	  }
	else
	  {
	    /* For externals we have to look at all defs since their
	       insertion place is decided per vector.  But beware
	       of pre-existing vectors where we need to make sure
	       we do not insert before the region boundary.  */
	    if (SLP_TREE_SCALAR_OPS (child).is_empty ()
		&& !vinfo->lookup_def (SLP_TREE_VEC_DEFS (child)[0]))
	      seen_vector_def = true;
	    else
	      {
		unsigned j;
		tree vdef;
		FOR_EACH_VEC_ELT (SLP_TREE_VEC_DEFS (child), j, vdef)
		  if (TREE_CODE (vdef) == SSA_NAME
		      && !SSA_NAME_IS_DEFAULT_DEF (vdef))
		    {
		      gimple *vstmt = SSA_NAME_DEF_STMT (vdef);
		      if (!last_stmt
			  || vect_stmt_dominates_stmt_p (last_stmt, vstmt))
			last_stmt = vstmt;
		    }
	      }
	  }
      /* This can happen when all children are pre-existing vectors or
	 constants.  */
      if (!last_stmt)
	last_stmt = vect_find_first_scalar_stmt_in_slp (node)->stmt;
      if (!last_stmt)
	{
	  gcc_assert (seen_vector_def);
	  si = gsi_after_labels (vinfo->bbs[0]);
	}
      else if (is_ctrl_altering_stmt (last_stmt))
	{
	  /* We split regions to vectorize at control altering stmts
	     with a definition so this must be an external which
	     we can insert at the start of the region.  */
	  si = gsi_after_labels (vinfo->bbs[0]);
	}
      else if (is_a <bb_vec_info> (vinfo)
	       && SLP_TREE_CODE (node) != VEC_PERM_EXPR
	       && gimple_bb (last_stmt) != gimple_bb (stmt_info->stmt)
	       && gimple_could_trap_p (stmt_info->stmt))
	{
	  /* We've constrained possibly trapping operations to all come
	     from the same basic-block, if vectorized defs would allow earlier
	     scheduling still force vectorized stmts to the original block.
	     This is only necessary for BB vectorization since for loop vect
	     all operations are in a single BB and scalar stmt based
	     placement doesn't play well with epilogue vectorization.  */
	  gcc_assert (dominated_by_p (CDI_DOMINATORS,
				      gimple_bb (stmt_info->stmt),
				      gimple_bb (last_stmt)));
	  si = gsi_after_labels (gimple_bb (stmt_info->stmt));
	}
      else if (is_a <gphi *> (last_stmt))
	si = gsi_after_labels (gimple_bb (last_stmt));
      else
	{
	  si = gsi_for_stmt (last_stmt);
	  gsi_next (&si);

	  /* Avoid scheduling internal defs outside of the loop when
	     we might have only implicitly tracked loop mask/len defs.  */
	  if (auto loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
	    if (LOOP_VINFO_FULLY_MASKED_P (loop_vinfo)
		|| LOOP_VINFO_FULLY_WITH_LENGTH_P (loop_vinfo))
	      {
		gimple_stmt_iterator si2
		  = gsi_after_labels (LOOP_VINFO_LOOP (loop_vinfo)->header);
		if ((gsi_end_p (si2)
		     && (LOOP_VINFO_LOOP (loop_vinfo)->header
			 != gimple_bb (last_stmt))
		     && dominated_by_p (CDI_DOMINATORS,
					LOOP_VINFO_LOOP (loop_vinfo)->header,
					gimple_bb (last_stmt)))
		    || (!gsi_end_p (si2)
			&& last_stmt != *si2
			&& vect_stmt_dominates_stmt_p (last_stmt, *si2)))
		  si = si2;
	      }
	}
    }

  /* Handle purely internal nodes.  */
  if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "------>vectorizing SLP permutation node\n");
      /* ???  the transform kind is stored to STMT_VINFO_TYPE which might
	 be shared with different SLP nodes (but usually it's the same
	 operation apart from the case the stmt is only there for denoting
	 the actual scalar lane defs ...).  So do not call vect_transform_stmt
	 but open-code it here (partly).  */
      bool done = vectorizable_slp_permutation (vinfo, &si, node, NULL);
      gcc_assert (done);
      stmt_vec_info slp_stmt_info;
      unsigned int i;
      FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, slp_stmt_info)
	if (slp_stmt_info && STMT_VINFO_LIVE_P (slp_stmt_info))
	  {
	    done = vectorizable_live_operation (vinfo, slp_stmt_info, node,
						instance, i, true, NULL);
	    gcc_assert (done);
	  }
    }
  else
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "------>vectorizing SLP node starting from: %G",
			 stmt_info->stmt);
      vect_transform_stmt (vinfo, stmt_info, &si, node, instance);
    }
}

/* Replace scalar calls from SLP node NODE with setting of their lhs to zero.
   For loop vectorization this is done in vectorizable_call, but for SLP
   it needs to be deferred until end of vect_schedule_slp, because multiple
   SLP instances may refer to the same scalar stmt.  */

static void
vect_remove_slp_scalar_calls (vec_info *vinfo,
			      slp_tree node, hash_set<slp_tree> &visited)
{
  gimple *new_stmt;
  gimple_stmt_iterator gsi;
  int i;
  slp_tree child;
  tree lhs;
  stmt_vec_info stmt_info;

  if (!node || SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    return;

  if (visited.add (node))
    return;

  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    vect_remove_slp_scalar_calls (vinfo, child, visited);

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      if (!stmt_info)
	continue;
      gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt);
      if (!stmt || gimple_bb (stmt) == NULL)
	continue;
      if (is_pattern_stmt_p (stmt_info)
	  || !PURE_SLP_STMT (stmt_info))
	continue;
      lhs = gimple_call_lhs (stmt);
      if (lhs)
	new_stmt = gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
      else
	{
	  new_stmt = gimple_build_nop ();
	  unlink_stmt_vdef (stmt_info->stmt);
	}
      gsi = gsi_for_stmt (stmt);
      vinfo->replace_stmt (&gsi, stmt_info, new_stmt);
      if (lhs)
	SSA_NAME_DEF_STMT (lhs) = new_stmt;
    }
}

static void
vect_remove_slp_scalar_calls (vec_info *vinfo, slp_tree node)
{
  hash_set<slp_tree> visited;
  vect_remove_slp_scalar_calls (vinfo, node, visited);
}

/* Vectorize the instance root.  */

void
vectorize_slp_instance_root_stmt (vec_info *vinfo, slp_tree node, slp_instance instance)
{
  gassign *rstmt = NULL;

  if (instance->kind == slp_inst_kind_ctor)
    {
      if (SLP_TREE_NUMBER_OF_VEC_STMTS (node) == 1)
	{
	  tree vect_lhs = SLP_TREE_VEC_DEFS (node)[0];
	  tree root_lhs = gimple_get_lhs (instance->root_stmts[0]->stmt);
	  if (!useless_type_conversion_p (TREE_TYPE (root_lhs),
					  TREE_TYPE (vect_lhs)))
	    vect_lhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (root_lhs),
			       vect_lhs);
	  rstmt = gimple_build_assign (root_lhs, vect_lhs);
	}
      else if (SLP_TREE_NUMBER_OF_VEC_STMTS (node) > 1)
	{
	  int nelts = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
	  tree child_def;
	  int j;
	  vec<constructor_elt, va_gc> *v;
	  vec_alloc (v, nelts);

	  /* A CTOR can handle V16HI composition from VNx8HI so we
	     do not need to convert vector elements if the types
	     do not match.  */
	  FOR_EACH_VEC_ELT (SLP_TREE_VEC_DEFS (node), j, child_def)
	    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, child_def);
	  tree lhs = gimple_get_lhs (instance->root_stmts[0]->stmt);
	  tree rtype
	    = TREE_TYPE (gimple_assign_rhs1 (instance->root_stmts[0]->stmt));
	  tree r_constructor = build_constructor (rtype, v);
	  rstmt = gimple_build_assign (lhs, r_constructor);
	}
    }
  else if (instance->kind == slp_inst_kind_bb_reduc)
    {
      /* Largely inspired by reduction chain epilogue handling in
	 vect_create_epilog_for_reduction.  */
      vec<tree> vec_defs = vNULL;
      vect_get_slp_defs (node, &vec_defs);
      enum tree_code reduc_code
	= gimple_assign_rhs_code (instance->root_stmts[0]->stmt);
      /* ???  We actually have to reflect signs somewhere.  */
      if (reduc_code == MINUS_EXPR)
	reduc_code = PLUS_EXPR;
      gimple_seq epilogue = NULL;
      /* We may end up with more than one vector result, reduce them
	 to one vector.  */
      tree vec_def = vec_defs[0];
      tree vectype = TREE_TYPE (vec_def);
      tree compute_vectype = vectype;
      bool pun_for_overflow_p = (ANY_INTEGRAL_TYPE_P (vectype)
				 && TYPE_OVERFLOW_UNDEFINED (vectype)
				 && operation_can_overflow (reduc_code));
      if (pun_for_overflow_p)
	{
	  compute_vectype = unsigned_type_for (vectype);
	  vec_def = gimple_build (&epilogue, VIEW_CONVERT_EXPR,
				  compute_vectype, vec_def);
	}
      for (unsigned i = 1; i < vec_defs.length (); ++i)
	{
	  tree def = vec_defs[i];
	  if (pun_for_overflow_p)
	    def = gimple_build (&epilogue, VIEW_CONVERT_EXPR,
				compute_vectype, def);
	  vec_def = gimple_build (&epilogue, reduc_code, compute_vectype,
				  vec_def, def);
	}
      vec_defs.release ();
      /* ???  Support other schemes than direct internal fn.  */
      internal_fn reduc_fn;
      if (!reduction_fn_for_scalar_code (reduc_code, &reduc_fn)
	  || reduc_fn == IFN_LAST)
	gcc_unreachable ();
      tree scalar_def = gimple_build (&epilogue, as_combined_fn (reduc_fn),
				      TREE_TYPE (compute_vectype), vec_def);
      if (!SLP_INSTANCE_REMAIN_DEFS (instance).is_empty ())
	{
	  tree rem_def = NULL_TREE;
	  for (auto def : SLP_INSTANCE_REMAIN_DEFS (instance))
	    {
	      def = gimple_convert (&epilogue, TREE_TYPE (scalar_def), def);
	      if (!rem_def)
		rem_def = def;
	      else
		rem_def = gimple_build (&epilogue, reduc_code,
					TREE_TYPE (scalar_def),
					rem_def, def);
	    }
	  scalar_def = gimple_build (&epilogue, reduc_code,
				     TREE_TYPE (scalar_def),
				     scalar_def, rem_def);
	}
      scalar_def = gimple_convert (&epilogue,
				   TREE_TYPE (vectype), scalar_def);
      gimple_stmt_iterator rgsi = gsi_for_stmt (instance->root_stmts[0]->stmt);
      gsi_insert_seq_before (&rgsi, epilogue, GSI_SAME_STMT);
      gimple_assign_set_rhs_from_tree (&rgsi, scalar_def);
      update_stmt (gsi_stmt (rgsi));
      return;
    }
  else if (instance->kind == slp_inst_kind_gcond)
    {
      /* Only support a single root for now as we can't codegen CFG yet and so we
	 can't support lane > 1 at this time.  */
      gcc_assert (instance->root_stmts.length () == 1);
      auto root_stmt_info = instance->root_stmts[0];
      auto last_stmt = STMT_VINFO_STMT (root_stmt_info);
      gimple_stmt_iterator rgsi = gsi_for_stmt (last_stmt);
      gimple *vec_stmt = NULL;
      gcc_assert (!SLP_TREE_VEC_DEFS (node).is_empty ());
      bool res = vectorizable_early_exit (vinfo, root_stmt_info, &rgsi,
					  &vec_stmt, node, NULL);
      gcc_assert (res);
      return;
    }
  else
    gcc_unreachable ();

  gcc_assert (rstmt);

  gimple_stmt_iterator rgsi = gsi_for_stmt (instance->root_stmts[0]->stmt);
  gsi_replace (&rgsi, rstmt, true);
}

struct slp_scc_info
{
  bool on_stack;
  int dfs;
  int lowlink;
};

/* Schedule the SLP INSTANCE doing a DFS walk and collecting SCCs.  */

static void
vect_schedule_scc (vec_info *vinfo, slp_tree node, slp_instance instance,
		   hash_map<slp_tree, slp_scc_info> &scc_info,
		   int &maxdfs, vec<slp_tree> &stack)
{
  bool existed_p;
  slp_scc_info *info = &scc_info.get_or_insert (node, &existed_p);
  gcc_assert (!existed_p);
  info->dfs = maxdfs;
  info->lowlink = maxdfs;
  maxdfs++;

  /* Leaf.  */
  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
    {
      info->on_stack = false;
      vect_schedule_slp_node (vinfo, node, instance);
      return;
    }

  info->on_stack = true;
  stack.safe_push (node);

  unsigned i;
  slp_tree child;
  /* DFS recurse.  */
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    {
      if (!child)
	continue;
      slp_scc_info *child_info = scc_info.get (child);
      if (!child_info)
	{
	  vect_schedule_scc (vinfo, child, instance, scc_info, maxdfs, stack);
	  /* Recursion might have re-allocated the node.  */
	  info = scc_info.get (node);
	  child_info = scc_info.get (child);
	  info->lowlink = MIN (info->lowlink, child_info->lowlink);
	}
      else if (child_info->on_stack)
	info->lowlink = MIN (info->lowlink, child_info->dfs);
    }
  if (info->lowlink != info->dfs)
    return;

  auto_vec<slp_tree, 4> phis_to_fixup;

  /* Singleton.  */
  if (stack.last () == node)
    {
      stack.pop ();
      info->on_stack = false;
      vect_schedule_slp_node (vinfo, node, instance);
      if (SLP_TREE_CODE (node) != VEC_PERM_EXPR
	  && is_a <gphi *> (SLP_TREE_REPRESENTATIVE (node)->stmt))
	phis_to_fixup.quick_push (node);
    }
  else
    {
      /* SCC.  */
      int last_idx = stack.length () - 1;
      while (stack[last_idx] != node)
	last_idx--;
      /* We can break the cycle at PHIs who have at least one child
	 code generated.  Then we could re-start the DFS walk until
	 all nodes in the SCC are covered (we might have new entries
	 for only back-reachable nodes).  But it's simpler to just
	 iterate and schedule those that are ready.  */
      unsigned todo = stack.length () - last_idx;
      do
	{
	  for (int idx = stack.length () - 1; idx >= last_idx; --idx)
	    {
	      slp_tree entry = stack[idx];
	      if (!entry)
		continue;
	      bool phi = (SLP_TREE_CODE (entry) != VEC_PERM_EXPR
			  && is_a <gphi *> (SLP_TREE_REPRESENTATIVE (entry)->stmt));
	      bool ready = !phi;
	      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (entry), i, child)
		  if (!child)
		    {
		      gcc_assert (phi);
		      ready = true;
		      break;
		    }
		  else if (scc_info.get (child)->on_stack)
		    {
		      if (!phi)
			{
			  ready = false;
			  break;
			}
		    }
		  else
		    {
		      if (phi)
			{
			  ready = true;
			  break;
			}
		    }
	      if (ready)
		{
		  vect_schedule_slp_node (vinfo, entry, instance);
		  scc_info.get (entry)->on_stack = false;
		  stack[idx] = NULL;
		  todo--;
		  if (phi)
		    phis_to_fixup.safe_push (entry);
		}
	    }
	}
      while (todo != 0);

      /* Pop the SCC.  */
      stack.truncate (last_idx);
    }

  /* Now fixup the backedge def of the vectorized PHIs in this SCC.  */
  slp_tree phi_node;
  FOR_EACH_VEC_ELT (phis_to_fixup, i, phi_node)
    {
      gphi *phi = as_a <gphi *> (SLP_TREE_REPRESENTATIVE (phi_node)->stmt);
      edge_iterator ei;
      edge e;
      FOR_EACH_EDGE (e, ei, gimple_bb (phi)->preds)
	{
	  unsigned dest_idx = e->dest_idx;
	  child = SLP_TREE_CHILDREN (phi_node)[dest_idx];
	  if (!child || SLP_TREE_DEF_TYPE (child) != vect_internal_def)
	    continue;
	  unsigned n = SLP_TREE_VEC_DEFS (phi_node).length ();
	  /* Simply fill all args.  */
	  if (STMT_VINFO_DEF_TYPE (SLP_TREE_REPRESENTATIVE (phi_node))
	      != vect_first_order_recurrence)
	    for (unsigned i = 0; i < n; ++i)
	      {
		tree phidef = SLP_TREE_VEC_DEFS (phi_node)[i];
		gphi *phi = as_a <gphi *> (SSA_NAME_DEF_STMT (phidef));
		add_phi_arg (phi, vect_get_slp_vect_def (child, i),
			     e, gimple_phi_arg_location (phi, dest_idx));
	      }
	  else
	    {
	      /* Unless it is a first order recurrence which needs
		 args filled in for both the PHI node and the permutes.  */
	      gimple *perm
		= SSA_NAME_DEF_STMT (SLP_TREE_VEC_DEFS (phi_node)[0]);
	      gimple *rphi = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (perm));
	      add_phi_arg (as_a <gphi *> (rphi),
			   vect_get_slp_vect_def (child, n - 1),
			   e, gimple_phi_arg_location (phi, dest_idx));
	      for (unsigned i = 0; i < n; ++i)
		{
		  gimple *perm
		    = SSA_NAME_DEF_STMT (SLP_TREE_VEC_DEFS (phi_node)[i]);
		  if (i > 0)
		    gimple_assign_set_rhs1 (perm,
					    vect_get_slp_vect_def (child, i - 1));
		  gimple_assign_set_rhs2 (perm,
					  vect_get_slp_vect_def (child, i));
		  update_stmt (perm);
		}
	    }
	}
    }
}

/* Generate vector code for SLP_INSTANCES in the loop/basic block.  */

void
vect_schedule_slp (vec_info *vinfo, const vec<slp_instance> &slp_instances)
{
  slp_instance instance;
  unsigned int i;

  hash_map<slp_tree, slp_scc_info> scc_info;
  int maxdfs = 0;
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      slp_tree node = SLP_INSTANCE_TREE (instance);
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Vectorizing SLP tree:\n");
	  /* ???  Dump all?  */
	  if (!SLP_INSTANCE_ROOT_STMTS (instance).is_empty ())
	    dump_printf_loc (MSG_NOTE, vect_location, "Root stmt: %G",
			 SLP_INSTANCE_ROOT_STMTS (instance)[0]->stmt);
	  vect_print_slp_graph (MSG_NOTE, vect_location,
				SLP_INSTANCE_TREE (instance));
	}
      /* Schedule the tree of INSTANCE, scheduling SCCs in a way to
	 have a PHI be the node breaking the cycle.  */
      auto_vec<slp_tree> stack;
      if (!scc_info.get (node))
	vect_schedule_scc (vinfo, node, instance, scc_info, maxdfs, stack);

      if (!SLP_INSTANCE_ROOT_STMTS (instance).is_empty ())
	vectorize_slp_instance_root_stmt (vinfo, node, instance);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
                         "vectorizing stmts using SLP.\n");
    }

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      slp_tree root = SLP_INSTANCE_TREE (instance);
      stmt_vec_info store_info;
      unsigned int j;

      /* Remove scalar call stmts.  Do not do this for basic-block
	 vectorization as not all uses may be vectorized.
	 ???  Why should this be necessary?  DCE should be able to
	 remove the stmts itself.
	 ???  For BB vectorization we can as well remove scalar
	 stmts starting from the SLP tree root if they have no
	 uses.  */
      if (is_a <loop_vec_info> (vinfo))
	vect_remove_slp_scalar_calls (vinfo, root);

      /* Remove vectorized stores original scalar stmts.  */
      for (j = 0; SLP_TREE_SCALAR_STMTS (root).iterate (j, &store_info); j++)
        {
	  if (!STMT_VINFO_DATA_REF (store_info)
	      || !DR_IS_WRITE (STMT_VINFO_DATA_REF (store_info)))
	    break;

	  store_info = vect_orig_stmt (store_info);
	  /* Free the attached stmt_vec_info and remove the stmt.  */
	  vinfo->remove_stmt (store_info);

	  /* Invalidate SLP_TREE_REPRESENTATIVE in case we released it
	     to not crash in vect_free_slp_tree later.  */
	  if (SLP_TREE_REPRESENTATIVE (root) == store_info)
	    SLP_TREE_REPRESENTATIVE (root) = NULL;
        }
    }
}
