/* SLP - Basic Block Vectorization
   Copyright (C) 2007-2021 Free Software Foundation, Inc.
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

static bool vectorizable_slp_permutation (vec_info *, gimple_stmt_iterator *,
					  slp_tree, stmt_vector_for_cost *);

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
  SLP_TREE_VEC_STMTS (this) = vNULL;
  SLP_TREE_VEC_DEFS (this) = vNULL;
  SLP_TREE_NUMBER_OF_VEC_STMTS (this) = 0;
  SLP_TREE_CHILDREN (this) = vNULL;
  SLP_TREE_LOAD_PERMUTATION (this) = vNULL;
  SLP_TREE_LANE_PERMUTATION (this) = vNULL;
  SLP_TREE_DEF_TYPE (this) = vect_uninitialized_def;
  SLP_TREE_CODE (this) = ERROR_MARK;
  SLP_TREE_VECTYPE (this) = NULL_TREE;
  SLP_TREE_REPRESENTATIVE (this) = NULL;
  SLP_TREE_REF_COUNT (this) = 1;
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
  SLP_TREE_VEC_STMTS (this).release ();
  SLP_TREE_VEC_DEFS (this).release ();
  SLP_TREE_LOAD_PERMUTATION (this).release ();
  SLP_TREE_LANE_PERMUTATION (this).release ();
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
  if (root_stmt)
    return root_stmt->stmt;
  else
    return SLP_TREE_SCALAR_STMTS (root)[0]->stmt;
}


/* Free the memory allocated for the SLP instance.  */

void
vect_free_slp_instance (slp_instance instance)
{
  vect_free_slp_tree (SLP_INSTANCE_TREE (instance));
  SLP_INSTANCE_LOADS (instance).release ();
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
     stmt.  */
  tree first_op_type;
  enum vect_def_type first_dt;
  bool any_pattern;
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


/* Return true if STMTS contains a pattern statement.  */

static bool
vect_contains_pattern_stmt_p (vec<stmt_vec_info> stmts)
{
  stmt_vec_info stmt_info;
  unsigned int i;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    if (is_pattern_stmt_p (stmt_info))
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
	  if (vector_type
	      && VECTOR_MODE_P (TYPE_MODE (vector_type))
	      && known_eq (GET_MODE_SIZE (TYPE_MODE (vector_type)),
			   GET_MODE_SIZE (base_vector_mode)))
	    {
	      /* Try fusing consecutive sequences of COUNT / NVECTORS elements
		 together into elements of type INT_TYPE and using the result
		 to build NVECTORS vectors.  */
	      poly_uint64 nelts = GET_MODE_NUNITS (TYPE_MODE (vector_type));
	      vec_perm_builder sel1 (nelts, 2, 3);
	      vec_perm_builder sel2 (nelts, 2, 3);
	      poly_int64 half_nelts = exact_div (nelts, 2);
	      for (unsigned int i = 0; i < 3; ++i)
		{
		  sel1.quick_push (i);
		  sel1.quick_push (i + nelts);
		  sel2.quick_push (half_nelts + i);
		  sel2.quick_push (half_nelts + i + nelts);
		}
	      vec_perm_indices indices1 (sel1, 2, nelts);
	      vec_perm_indices indices2 (sel2, 2, nelts);
	      if (can_vec_perm_const_p (TYPE_MODE (vector_type), indices1)
		  && can_vec_perm_const_p (TYPE_MODE (vector_type), indices2))
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

/* Get the defs for the rhs of STMT (collect them in OPRNDS_INFO), check that
   they are of a valid type and that they match the defs of the first stmt of
   the SLP group (stored in OPRNDS_INFO).  This function tries to match stmts
   by swapping operands of STMTS[STMT_NUM] when possible.  Non-zero *SWAP
   indicates swap is required for cond_expr stmts.  Specifically, *SWAP
   is 1 if STMT is cond and operands of comparison need to be swapped;
   *SWAP is 2 if STMT is cond and code of comparison needs to be inverted.
   If there is any operand swap in this function, *SWAP is set to non-zero
   value.
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
  int first_op_idx = 1;
  unsigned int commutative_op = -1U;
  bool first_op_cond = false;
  bool first = stmt_num == 0;

  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    {
      number_of_oprnds = gimple_call_num_args (stmt);
      first_op_idx = 3;
      if (gimple_call_internal_p (stmt))
	{
	  internal_fn ifn = gimple_call_internal_fn (stmt);
	  commutative_op = first_commutative_argument (ifn);

	  /* Masked load, only look at mask.  */
	  if (ifn == IFN_MASK_LOAD)
	    {
	      number_of_oprnds = 1;
	      /* Mask operand index.  */
	      first_op_idx = 5;
	    }
	}
    }
  else if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      enum tree_code code = gimple_assign_rhs_code (stmt);
      number_of_oprnds = gimple_num_ops (stmt) - 1;
      /* Swap can only be done for cond_expr if asked to, otherwise we
	 could result in different comparison code to the first stmt.  */
      if (code == COND_EXPR
	  && COMPARISON_CLASS_P (gimple_assign_rhs1 (stmt)))
	{
	  first_op_cond = true;
	  number_of_oprnds++;
	}
      else
	commutative_op = commutative_tree_code (code) ? 0U : -1U;
    }
  else if (gphi *stmt = dyn_cast <gphi *> (stmt_info->stmt))
    number_of_oprnds = gimple_phi_num_args (stmt);
  else
    return -1;

  bool swapped = (swap != 0);
  bool backedge = false;
  gcc_assert (!swapped || first_op_cond);
  enum vect_def_type *dts = XALLOCAVEC (enum vect_def_type, number_of_oprnds);
  for (i = 0; i < number_of_oprnds; i++)
    {
      if (first_op_cond)
	{
	  /* Map indicating how operands of cond_expr should be swapped.  */
	  int maps[3][4] = {{0, 1, 2, 3}, {1, 0, 2, 3}, {0, 1, 3, 2}};
	  int *map = maps[swap];

	  if (i < 2)
	    oprnd = TREE_OPERAND (gimple_op (stmt_info->stmt,
					     first_op_idx), map[i]);
	  else
	    oprnd = gimple_op (stmt_info->stmt, map[i]);
	}
      else if (gphi *stmt = dyn_cast <gphi *> (stmt_info->stmt))
	{
	  oprnd = gimple_phi_arg_def (stmt, i);
	  backedge = dominated_by_p (CDI_DOMINATORS,
				     gimple_phi_arg_edge (stmt, i)->src,
				     gimple_bb (stmt_info->stmt));
	}
      else
	oprnd = gimple_op (stmt_info->stmt, first_op_idx + (swapped ? !i : i));
      if (TREE_CODE (oprnd) == VIEW_CONVERT_EXPR)
	oprnd = TREE_OPERAND (oprnd, 0);

      oprnd_info = (*oprnds_info)[i];

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
	  && !dominated_by_p (CDI_DOMINATORS,
			      as_a <bb_vec_info> (vinfo)->bbs[0],
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
	  if ((dt == vect_constant_def
	       || dt == vect_external_def)
	      && !GET_MODE_SIZE (vinfo->vector_mode).is_constant ()
	      && (TREE_CODE (type) == BOOLEAN_TYPE
		  || !can_duplicate_and_interleave_p (vinfo, stmts.length (),
						      type)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: invalid type of def "
				 "for variable-length SLP %T\n", oprnd);
	      return -1;
	    }

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
	    case vect_induction_def:
	    case vect_nested_cycle:
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
	      swapped = true;
	      continue;
	    }

	  if (is_a <bb_vec_info> (vinfo)
	      && !oprnd_info->any_pattern)
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

/* Try to assign vector type VECTYPE to STMT_INFO for BB vectorization.
   Return true if we can, meaning that this choice doesn't conflict with
   existing SLP nodes that use STMT_INFO.  */

bool
vect_update_shared_vectype (stmt_vec_info stmt_info, tree vectype)
{
  tree old_vectype = STMT_VINFO_VECTYPE (stmt_info);
  if (old_vectype)
    return useless_type_conversion_p (vectype, old_vectype);

  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      /* We maintain the invariant that if any statement in the group is
	 used, all other members of the group have the same vector type.  */
      stmt_vec_info first_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
      stmt_vec_info member_info = first_info;
      for (; member_info; member_info = DR_GROUP_NEXT_ELEMENT (member_info))
	if (is_pattern_stmt_p (member_info)
	    && !useless_type_conversion_p (vectype,
					   STMT_VINFO_VECTYPE (member_info)))
	  break;

      if (!member_info)
	{
	  for (member_info = first_info; member_info;
	       member_info = DR_GROUP_NEXT_ELEMENT (member_info))
	    STMT_VINFO_VECTYPE (member_info) = vectype;
	  return true;
	}
    }
  else if (!is_pattern_stmt_p (stmt_info))
    {
      STMT_VINFO_VECTYPE (stmt_info) = vectype;
      return true;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		       "Build SLP failed: incompatible vector"
		       " types for: %G", stmt_info->stmt);
      dump_printf_loc (MSG_NOTE, vect_location,
		       "    old vector type: %T\n", old_vectype);
      dump_printf_loc (MSG_NOTE, vect_location,
		       "    new vector type: %T\n", vectype);
    }
  return false;
}

/* Return true if call statements CALL1 and CALL2 are similar enough
   to be combined into the same SLP group.  */

static bool
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
  enum tree_code first_stmt_code = ERROR_MARK;
  enum tree_code alt_stmt_code = ERROR_MARK;
  enum tree_code rhs_code = ERROR_MARK;
  enum tree_code first_cond_code = ERROR_MARK;
  tree lhs;
  bool need_same_oprnds = false;
  tree vectype = NULL_TREE, first_op1 = NULL_TREE;
  optab optab;
  int icode;
  machine_mode optab_op2_mode;
  machine_mode vec_mode;
  stmt_vec_info first_load = NULL, prev_first_load = NULL;
  bool first_stmt_load_p = false, load_p = false;
  bool first_stmt_phi_p = false, phi_p = false;
  bool maybe_soft_fail = false;
  tree soft_fail_nunits_vectype = NULL_TREE;

  /* For every stmt in NODE find its def stmt/s.  */
  stmt_vec_info stmt_info;
  FOR_EACH_VEC_ELT (stmts, i, stmt_info)
    {
      gimple *stmt = stmt_info->stmt;
      swap[i] = 0;
      matches[i] = false;

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

      lhs = gimple_get_lhs (stmt);
      if (lhs == NULL_TREE)
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

      tree nunits_vectype;
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

      gcall *call_stmt = dyn_cast <gcall *> (stmt);
      if (call_stmt)
	{
	  rhs_code = CALL_EXPR;

	  if (gimple_call_internal_p (stmt, IFN_MASK_LOAD))
	    load_p = true;
	  else if ((gimple_call_internal_p (call_stmt)
		    && (!vectorizable_internal_fn_p
			(gimple_call_internal_fn (call_stmt))))
		   || gimple_call_tail_p (call_stmt)
		   || gimple_call_noreturn_p (call_stmt)
		   || !gimple_call_nothrow_p (call_stmt)
		   || gimple_call_chain (call_stmt))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: unsupported call type %G",
				 call_stmt);
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
	  load_p = gimple_vuse (stmt);
	}

      /* Check the operation.  */
      if (i == 0)
	{
	  *node_vectype = vectype;
	  first_stmt_code = rhs_code;
	  first_stmt_load_p = load_p;
	  first_stmt_phi_p = phi_p;

	  /* Shift arguments should be equal in all the packed stmts for a
	     vector shift with scalar shift operand.  */
	  if (rhs_code == LSHIFT_EXPR || rhs_code == RSHIFT_EXPR
	      || rhs_code == LROTATE_EXPR
	      || rhs_code == RROTATE_EXPR)
	    {
	      vec_mode = TYPE_MODE (vectype);

	      /* First see if we have a vector/vector shift.  */
	      optab = optab_for_tree_code (rhs_code, vectype,
					   optab_vector);

	      if (!optab
		  || optab_handler (optab, vec_mode) == CODE_FOR_nothing)
		{
		  /* No vector/vector shift, try for a vector/scalar shift.  */
		  optab = optab_for_tree_code (rhs_code, vectype,
					       optab_scalar);

		  if (!optab)
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
					 "Build SLP failed: no optab.\n");
		      if (is_a <bb_vec_info> (vinfo) && i != 0)
			continue;
		      /* Fatal mismatch.  */
		      matches[0] = false;
		      return false;
		    }
		  icode = (int) optab_handler (optab, vec_mode);
		  if (icode == CODE_FOR_nothing)
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
		  optab_op2_mode = insn_data[icode].operand[2].mode;
		  if (!VECTOR_MODE_P (optab_op2_mode))
		    {
		      need_same_oprnds = true;
		      first_op1 = gimple_assign_rhs2 (stmt);
		    }
		}
	    }
	  else if (rhs_code == WIDEN_LSHIFT_EXPR)
            {
              need_same_oprnds = true;
              first_op1 = gimple_assign_rhs2 (stmt);
            }
	  else if (!load_p
		   && rhs_code == BIT_FIELD_REF)
	    {
	      tree vec = TREE_OPERAND (gimple_assign_rhs1 (stmt), 0);
	      if (!is_a <bb_vec_info> (vinfo)
		  || TREE_CODE (vec) != SSA_NAME
		  || !operand_equal_p (TYPE_SIZE (vectype),
				       TYPE_SIZE (TREE_TYPE (vec))))
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
	  else if (call_stmt
		   && gimple_call_internal_p (call_stmt, IFN_DIV_POW2))
	    {
	      need_same_oprnds = true;
	      first_op1 = gimple_call_arg (call_stmt, 1);
	    }
	}
      else
	{
	  if (first_stmt_code != rhs_code
	      && alt_stmt_code == ERROR_MARK)
	    alt_stmt_code = rhs_code;
	  if ((first_stmt_code != rhs_code
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
	       && !(STMT_VINFO_GROUPED_ACCESS (stmt_info)
		    && (first_stmt_code == ARRAY_REF
			|| first_stmt_code == BIT_FIELD_REF
			|| first_stmt_code == INDIRECT_REF
			|| first_stmt_code == COMPONENT_REF
			|| first_stmt_code == MEM_REF)))
	      || first_stmt_load_p != load_p
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

	  if (need_same_oprnds)
	    {
	      tree other_op1 = (call_stmt
				? gimple_call_arg (call_stmt, 1)
				: gimple_assign_rhs2 (stmt));
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
	  if (!load_p
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

	  if (!load_p && rhs_code == CALL_EXPR)
	    {
	      if (!compatible_calls_p (as_a <gcall *> (stmts[0]->stmt),
				       as_a <gcall *> (stmt)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "Build SLP failed: different calls in %G",
				     stmt);
		  /* Mismatch.  */
		  continue;
		}
	    }

	  if (phi_p
	      && (gimple_bb (first_stmt_info->stmt)
		  != gimple_bb (stmt_info->stmt)))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: different BB for PHI "
				 "in %G", stmt);
	      /* Mismatch.  */
	      continue;
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
	  if (REFERENCE_CLASS_P (lhs))
	    {
	      /* Store.  */
	      ;
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
        } /* Grouped access.  */
      else
	{
	  if (load_p)
	    {
	      /* Not grouped load.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Build SLP failed: not grouped load %G", stmt);

	      /* FORNOW: Not grouped loads are not supported.  */
	      if (is_a <bb_vec_info> (vinfo) && i != 0)
		continue;
	      /* Fatal mismatch.  */
	      matches[0] = false;
	      return false;
	    }

	  /* Not memory operation.  */
	  if (!phi_p
	      && TREE_CODE_CLASS (rhs_code) != tcc_binary
	      && TREE_CODE_CLASS (rhs_code) != tcc_unary
	      && TREE_CODE_CLASS (rhs_code) != tcc_expression
	      && TREE_CODE_CLASS (rhs_code) != tcc_comparison
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
	}

      matches[i] = true;
    }

  for (i = 0; i < group_size; ++i)
    if (!matches[i])
      return false;

  /* If we allowed a two-operation SLP node verify the target can cope
     with the permute we are going to use.  */
  if (alt_stmt_code != ERROR_MARK
      && TREE_CODE_CLASS (alt_stmt_code) != tcc_reference)
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
    h.add_int (gimple_uid (x[i]->stmt));
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
			 *leader ? "" : "failed ", *leader);
      if (*leader)
	{
	  SLP_TREE_REF_COUNT (*leader)++;
	  vect_update_max_nunits (max_nunits, (*leader)->max_nunits);
	  stmts.release ();
	}
      return *leader;
    }

  /* Seed the bst_map with a stub node to be filled by vect_build_slp_tree_2
     so we can pick up backedge destinations during discovery.  */
  slp_tree res = new _slp_tree;
  SLP_TREE_DEF_TYPE (res) = vect_internal_def;
  SLP_TREE_SCALAR_STMTS (res) = stmts;
  bst_map->put (stmts.copy (), res);

  if (*limit == 0)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "SLP discovery limit exceeded\n");
      bool existed_p = bst_map->put (stmts, NULL);
      gcc_assert (existed_p);
      /* Mark the node invalid so we can detect those when still in use
	 as backedge destinations.  */
      SLP_TREE_SCALAR_STMTS (res) = vNULL;
      SLP_TREE_DEF_TYPE (res) = vect_uninitialized_def;
      vect_free_slp_tree (res);
      memset (matches, 0, sizeof (bool) * group_size);
      return NULL;
    }
  --*limit;

  poly_uint64 this_max_nunits = 1;
  slp_tree res_ = vect_build_slp_tree_2 (vinfo, res, stmts, group_size,
					&this_max_nunits,
					matches, limit, tree_size, bst_map);
  if (!res_)
    {
      bool existed_p = bst_map->put (stmts, NULL);
      gcc_assert (existed_p);
      /* Mark the node invalid so we can detect those when still in use
	 as backedge destinations.  */
      SLP_TREE_SCALAR_STMTS (res) = vNULL;
      SLP_TREE_DEF_TYPE (res) = vect_uninitialized_def;
      vect_free_slp_tree (res);
    }
  else
    {
      gcc_assert (res_ == res);
      res->max_nunits = this_max_nunits;
      vect_update_max_nunits (max_nunits, this_max_nunits);
      /* Keep a reference for the bst_map use.  */
      SLP_TREE_REF_COUNT (res)++;
    }
  return res_;
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
  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    nops = gimple_call_num_args (stmt);
  else if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      nops = gimple_num_ops (stmt) - 1;
      if (gimple_assign_rhs_code (stmt) == COND_EXPR)
	nops++;
    }
  else if (gphi *phi = dyn_cast <gphi *> (stmt_info->stmt))
    nops = gimple_phi_num_args (phi);
  else
    return NULL;

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
		 || def_type == vect_nested_cycle)
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
	    if (!nested_in_vect_loop_p (loop, stmt_info))
	      skip_args[loop_preheader_edge (loop)->dest_idx] = true;
	    /* Reduction chain backedge defs are filled manually.
	       ???  Need a better way to identify a SLP reduction chain PHI.
	       Or a better overall way to SLP match those.  */
	    if (all_same && def_type == vect_reduction_def)
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
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
    {
      if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
	{
	  /* Masked load.  */
	  gcc_assert (gimple_call_internal_p (stmt, IFN_MASK_LOAD));
	  nops = 1;
	}
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
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    {
	      int load_place = vect_get_place_in_interleaving_chain
		  (load_info, first_stmt_info);
	      gcc_assert (load_place != -1);
	      load_permutation.safe_push (load_place);
	    }
	  SLP_TREE_LOAD_PERMUTATION (node) = load_permutation;
	  return node;
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
	      return NULL;
	    }
	  lperm.safe_push (std::make_pair (0, (unsigned)lane));
	}
      slp_tree vnode = vect_create_new_slp_node (vNULL);
      /* ???  We record vectype here but we hide eventually necessary
	 punning and instead rely on code generation to materialize
	 VIEW_CONVERT_EXPRs as necessary.  We instead should make
	 this explicit somehow.  */
      SLP_TREE_VECTYPE (vnode) = vectype;
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

  auto_vec<slp_tree, 4> children;

  stmt_info = stmts[0];

  /* Create SLP_TREE nodes for the definition node/s.  */
  FOR_EACH_VEC_ELT (oprnds_info, i, oprnd_info)
    {
      slp_tree child;
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
				 "Using a splat of the uniform operand\n");
	      oprnd_info->first_dt = vect_external_def;
	    }
	}

      if (oprnd_info->first_dt == vect_external_def
	  || oprnd_info->first_dt == vect_constant_def)
	{
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
	  && STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def
	  && STMT_VINFO_DEF_TYPE (stmt_info) != vect_double_reduction_def)
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
  dump_printf_loc (metadata, user_loc, "node%s %p (max_nunits=%u, refcnt=%u)\n",
		   SLP_TREE_DEF_TYPE (node) == vect_external_def
		   ? " (external)"
		   : (SLP_TREE_DEF_TYPE (node) == vect_constant_def
		      ? " (constant)"
		      : ""), node,
		   estimated_poly_value (node->max_nunits),
					 SLP_TREE_REF_COUNT (node));
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
      dump_printf_loc (metadata, user_loc, "\tstmt %u %G", i, stmt_info->stmt);
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
      dump_printf (dump_kind, " }\n");
    }
  if (SLP_TREE_CHILDREN (node).is_empty ())
    return;
  dump_printf_loc (metadata, user_loc, "\tchildren");
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    dump_printf (dump_kind, " %p", (void *)child);
  dump_printf (dump_kind, "\n");
}

DEBUG_FUNCTION void
debug (slp_tree node)
{
  debug_dump_context ctx;
  vect_print_slp_tree (MSG_NOTE,
		       dump_location_t::from_location_t (UNKNOWN_LOCATION),
		       node);
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

  if (SLP_TREE_CHILDREN (node).length () == 0)
    {
      if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
	return;
      stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && DR_IS_READ (STMT_VINFO_DATA_REF (stmt_info)))
	loads.safe_push (node);
    }
  else
    {
      unsigned i;
      slp_tree child;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
	vect_gather_slp_loads (loads, child, visited);
    }
}


/* Find the last store in SLP INSTANCE.  */

stmt_vec_info
vect_find_last_scalar_stmt_in_slp (slp_tree node)
{
  stmt_vec_info last = NULL;
  stmt_vec_info stmt_vinfo;

  for (int i = 0; SLP_TREE_SCALAR_STMTS (node).iterate (i, &stmt_vinfo); i++)
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
  return SLP_TREE_DEF_TYPE (root) == vect_internal_def
	 && STMT_VINFO_GROUPED_ACCESS (SLP_TREE_REPRESENTATIVE (root))
	 && DR_IS_READ (STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (root)));
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
			 "converting stmts on permute node %p\n", root);

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
					  vinfo, perm_cache, visited);

  for (unsigned x = 0; x < num__slp_patterns; x++)
    {
      vect_pattern *pattern = slp_patterns[x] (perm_cache, ref_node);
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
			 slp_tree_to_load_perm_map_t *perm_cache)
{
  DUMP_VECT_SCOPE ("vect_match_slp_patterns");
  slp_tree *ref_node = &SLP_INSTANCE_TREE (instance);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Analyzing SLP tree %p for patterns\n",
		     SLP_INSTANCE_TREE (instance));

  return vect_match_slp_patterns_2 (ref_node, vinfo, perm_cache, visited);
}

/* STMT_INFO is a store group of size GROUP_SIZE that we are considering
   splitting into two, with the first split group having size NEW_GROUP_SIZE.
   Return true if we could use IFN_STORE_LANES instead and if that appears
   to be the better approach.  */

static bool
vect_slp_prefer_store_lanes_p (vec_info *vinfo, stmt_vec_info stmt_info,
			       unsigned int group_size,
			       unsigned int new_group_size)
{
  tree scalar_type = TREE_TYPE (DR_REF (STMT_VINFO_DATA_REF (stmt_info)));
  tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type);
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
  return vect_store_lanes_supported (vectype, group_size, false);
}

/* Analyze an SLP instance starting from a group of grouped stores.  Call
   vect_build_slp_tree to build a tree of packed stmts if possible.
   Return FALSE if it's impossible to SLP any stmt in the loop.  */

static bool
vect_analyze_slp_instance (vec_info *vinfo,
			   scalar_stmts_to_slp_tree_map_t *bst_map,
			   stmt_vec_info stmt_info, slp_instance_kind kind,
			   unsigned max_tree_size, unsigned *limit);

/* Analyze an SLP instance starting from SCALAR_STMTS which are a group
   of KIND.  Return true if successful.  */

static bool
vect_build_slp_instance (vec_info *vinfo,
			 slp_instance_kind kind,
			 vec<stmt_vec_info> &scalar_stmts,
			 stmt_vec_info root_stmt_info,
			 unsigned max_tree_size, unsigned *limit,
			 scalar_stmts_to_slp_tree_map_t *bst_map,
			 /* ???  We need stmt_info for group splitting.  */
			 stmt_vec_info stmt_info_)
{
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
  slp_tree node = vect_build_slp_tree (vinfo, scalar_stmts, group_size,
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
	  SLP_INSTANCE_ROOT_STMT (new_instance) = root_stmt_info;
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
			       "Final SLP tree for instance %p:\n", new_instance);
	      vect_print_slp_graph (MSG_NOTE, vect_location,
				    SLP_INSTANCE_TREE (new_instance));
	    }

	  return true;
	}
    }
  else
    {
      /* Failed to SLP.  */
      /* Free the allocated memory.  */
      scalar_stmts.release ();
    }

  stmt_vec_info stmt_info = stmt_info_;
  /* Try to break the group up into pieces.  */
  if (kind == slp_inst_kind_store)
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
						    limit);
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
						      limit);
		}
	      /* Re-analyze the non-matching tail if it has at least
		 two lanes.  */
	      if (i + 1 < group_size)
		res |= vect_analyze_slp_instance (vinfo, bst_map,
						  rest, kind, max_tree_size,
						  limit);
	      return res;
	    }
	}

      /* For loop vectorization split into arbitrary pieces of size > 1.  */
      if (is_a <loop_vec_info> (vinfo)
	  && (i > 1 && i < group_size)
	  && !vect_slp_prefer_store_lanes_p (vinfo, stmt_info, group_size, i))
	{
	  unsigned group1_size = i;

	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "Splitting SLP group at stmt %u\n", i);

	  stmt_vec_info rest = vect_split_slp_store_group (stmt_info,
							   group1_size);
	  /* Loop vectorization cannot handle gaps in stores, make sure
	     the split group appears as strided.  */
	  STMT_VINFO_STRIDED_P (rest) = 1;
	  DR_GROUP_GAP (rest) = 0;
	  STMT_VINFO_STRIDED_P (stmt_info) = 1;
	  DR_GROUP_GAP (stmt_info) = 0;

	  bool res = vect_analyze_slp_instance (vinfo, bst_map, stmt_info,
						kind, max_tree_size, limit);
	  if (i + 1 < group_size)
	    res |= vect_analyze_slp_instance (vinfo, bst_map,
					      rest, kind, max_tree_size, limit);

	  return res;
	}

      /* Even though the first vector did not all match, we might be able to SLP
	 (some) of the remainder.  FORNOW ignore this possibility.  */
    }

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
			   unsigned max_tree_size, unsigned *limit)
{
  unsigned int i;
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
  else if (kind == slp_inst_kind_ctor)
    {
      tree rhs = gimple_assign_rhs1 (stmt_info->stmt);
      tree val;
      scalar_stmts.create (CONSTRUCTOR_NELTS (rhs));
      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (rhs), i, val)
	{
	  stmt_vec_info def_info = vinfo->lookup_def (val);
	  def_info = vect_stmt_to_vectorize (def_info);
	  scalar_stmts.quick_push (def_info);
	}
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Analyzing vectorizable constructor: %G\n",
			 stmt_info->stmt);
    }
  else if (kind == slp_inst_kind_reduc_group)
    {
      /* Collect reduction statements.  */
      vec<stmt_vec_info> reductions = as_a <loop_vec_info> (vinfo)->reductions;
      scalar_stmts.create (reductions.length ());
      for (i = 0; reductions.iterate (i, &next_info); i++)
	if (STMT_VINFO_RELEVANT_P (next_info)
	    || STMT_VINFO_LIVE_P (next_info))
	  scalar_stmts.quick_push (next_info);
      /* If less than two were relevant/live there's nothing to SLP.  */
      if (scalar_stmts.length () < 2)
	return false;
    }
  else
    gcc_unreachable ();

  /* Build the tree for the SLP instance.  */
  bool res = vect_build_slp_instance (vinfo, kind, scalar_stmts,
				      kind == slp_inst_kind_ctor
				      ? stmt_info : NULL,
				      max_tree_size, limit, bst_map,
				      kind == slp_inst_kind_store
				      ? stmt_info : NULL);

  /* ???  If this is slp_inst_kind_store and the above succeeded here's
     where we should do store group splitting.  */

  return res;
}

/* Check if there are stmts in the loop can be vectorized using SLP.  Build SLP
   trees of packed scalar stmts if SLP is possible.  */

opt_result
vect_analyze_slp (vec_info *vinfo, unsigned max_tree_size)
{
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
			       STMT_VINFO_GROUPED_ACCESS (first_element)
			       ? slp_inst_kind_store : slp_inst_kind_ctor,
			       max_tree_size, &limit);

  if (bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo))
    {
      for (unsigned i = 0; i < bb_vinfo->roots.length (); ++i)
	{
	  vect_location = bb_vinfo->roots[i].root->stmt;
	  if (vect_build_slp_instance (bb_vinfo, bb_vinfo->roots[i].kind,
				       bb_vinfo->roots[i].stmts,
				       bb_vinfo->roots[i].root,
				       max_tree_size, &limit, bst_map, NULL))
	    bb_vinfo->roots[i].stmts = vNULL;
	}
    }

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      /* Find SLP sequences starting from reduction chains.  */
      FOR_EACH_VEC_ELT (loop_vinfo->reduction_chains, i, first_element)
	if (! STMT_VINFO_RELEVANT_P (first_element)
	    && ! STMT_VINFO_LIVE_P (first_element))
	  ;
	else if (! vect_analyze_slp_instance (vinfo, bst_map, first_element,
					      slp_inst_kind_reduc_chain,
					      max_tree_size, &limit))
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
      if (loop_vinfo->reductions.length () > 1)
	vect_analyze_slp_instance (vinfo, bst_map, loop_vinfo->reductions[0],
				   slp_inst_kind_reduc_group, max_tree_size,
				   &limit);
    }

  hash_set<slp_tree> visited_patterns;
  slp_tree_to_load_perm_map_t perm_cache;

  /* See if any patterns can be found in the SLP tree.  */
  bool pattern_found = false;
  FOR_EACH_VEC_ELT (LOOP_VINFO_SLP_INSTANCES (vinfo), i, instance)
    pattern_found |= vect_match_slp_patterns (instance, vinfo,
					      &visited_patterns, &perm_cache);

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



  /* The map keeps a reference on SLP nodes built, release that.  */
  for (scalar_stmts_to_slp_tree_map_t::iterator it = bst_map->begin ();
       it != bst_map->end (); ++it)
    if ((*it).second)
      vect_free_slp_tree ((*it).second);
  delete bst_map;

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

/* Fill the vertices and leafs vector with all nodes in the SLP graph.  */

static void
vect_slp_build_vertices (hash_set<slp_tree> &visited, slp_tree node,
			 vec<slp_tree> &vertices, vec<int> &leafs)
{
  unsigned i;
  slp_tree child;

  if (visited.add (node))
    return;

  node->vertex = vertices.length ();
  vertices.safe_push (node);

  bool leaf = true;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child)
      {
	leaf = false;
	vect_slp_build_vertices (visited, child, vertices, leafs);
      }
  if (leaf)
    leafs.safe_push (node->vertex);
}

/* Fill the vertices and leafs vector with all nodes in the SLP graph.  */

static void
vect_slp_build_vertices (vec_info *info, vec<slp_tree> &vertices,
			 vec<int> &leafs)
{
  hash_set<slp_tree> visited;
  unsigned i;
  slp_instance instance;
  FOR_EACH_VEC_ELT (info->slp_instances, i, instance)
    {
      unsigned n_v = vertices.length ();
      unsigned n_l = leafs.length ();
      vect_slp_build_vertices (visited, SLP_INSTANCE_TREE (instance), vertices,
			       leafs);
      /* If we added vertices but no entries to the reverse graph we've
	 added a cycle that is not backwards-reachable.   Push the entry
	 to mimic as leaf then.  */
      if (vertices.length () > n_v
	  && leafs.length () == n_l)
	leafs.safe_push (SLP_INSTANCE_TREE (instance)->vertex);
    }
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

/* Return whether permutations PERM_A and PERM_B as recorded in the
   PERMS vector are equal.  */

static bool
vect_slp_perms_eq (const vec<vec<unsigned> > &perms,
		   int perm_a, int perm_b)
{
  return (perm_a == perm_b
	  || (perms[perm_a].length () == perms[perm_b].length ()
	      && memcmp (&perms[perm_a][0], &perms[perm_b][0],
			 sizeof (unsigned) * perms[perm_a].length ()) == 0));
}

/* Optimize the SLP graph of VINFO.  */

void
vect_optimize_slp (vec_info *vinfo)
{
  if (vinfo->slp_instances.is_empty ())
    return;

  slp_tree node;
  unsigned i;
  auto_vec<slp_tree> vertices;
  auto_vec<int> leafs;
  vect_slp_build_vertices (vinfo, vertices, leafs);

  struct graph *slpg = new_graph (vertices.length ());
  FOR_EACH_VEC_ELT (vertices, i, node)
    {
      unsigned j;
      slp_tree child;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
	if (child)
	  add_edge (slpg, i, child->vertex);
    }

  /* Compute (reverse) postorder on the inverted graph.  */
  auto_vec<int> ipo;
  graphds_dfs (slpg, &leafs[0], leafs.length (), &ipo, false, NULL, NULL);

  auto_sbitmap n_visited (vertices.length ());
  auto_sbitmap n_materialize (vertices.length ());
  auto_vec<int> n_perm (vertices.length ());
  auto_vec<vec<unsigned> > perms;

  bitmap_clear (n_visited);
  bitmap_clear (n_materialize);
  n_perm.quick_grow_cleared (vertices.length ());
  perms.safe_push (vNULL); /* zero is no permute */

  /* Produce initial permutations.  */
  for (i = 0; i < leafs.length (); ++i)
    {
      int idx = leafs[i];
      slp_tree node = vertices[idx];

      /* Handle externals and constants optimistically throughout the
	 iteration.  */
      if (SLP_TREE_DEF_TYPE (node) == vect_external_def
	  || SLP_TREE_DEF_TYPE (node) == vect_constant_def)
	continue;

      /* Leafs do not change across iterations.  Note leafs also double
	 as entries to the reverse graph.  */
      if (!slpg->vertices[idx].succ)
	bitmap_set_bit (n_visited, idx);
      /* Loads are the only thing generating permutes.  */
      if (!SLP_TREE_LOAD_PERMUTATION (node).exists ())
	continue;

      /* If splitting out a SLP_TREE_LANE_PERMUTATION can make the
	 node unpermuted, record this permute.  */
      stmt_vec_info dr_stmt = SLP_TREE_REPRESENTATIVE (node);
      if (!STMT_VINFO_GROUPED_ACCESS (dr_stmt))
	continue;
      dr_stmt = DR_GROUP_FIRST_ELEMENT (dr_stmt);
      unsigned imin = DR_GROUP_SIZE (dr_stmt) + 1, imax = 0;
      bool any_permute = false;
      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	{
	  unsigned idx = SLP_TREE_LOAD_PERMUTATION (node)[j];
	  imin = MIN (imin, idx);
	  imax = MAX (imax, idx);
	  if (idx - SLP_TREE_LOAD_PERMUTATION (node)[0] != j)
	    any_permute = true;
	}
      /* If there's no permute no need to split one out.  */
      if (!any_permute)
	continue;
      /* If the span doesn't match we'd disrupt VF computation, avoid
	 that for now.  */
      if (imax - imin + 1 != SLP_TREE_LANES (node))
	continue;

      /* For now only handle true permutes, like
	 vect_attempt_slp_rearrange_stmts did.  This allows us to be lazy
	 when permuting constants and invariants keeping the permute
	 bijective.  */
      auto_sbitmap load_index (SLP_TREE_LANES (node));
      bitmap_clear (load_index);
      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	bitmap_set_bit (load_index, SLP_TREE_LOAD_PERMUTATION (node)[j] - imin);
      unsigned j;
      for (j = 0; j < SLP_TREE_LANES (node); ++j)
	if (!bitmap_bit_p (load_index, j))
	  break;
      if (j != SLP_TREE_LANES (node))
	continue;

      vec<unsigned> perm = vNULL;
      perm.safe_grow (SLP_TREE_LANES (node), true);
      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
	perm[j] = SLP_TREE_LOAD_PERMUTATION (node)[j] - imin;
      perms.safe_push (perm);
      n_perm[idx] = perms.length () - 1;
    }

  /* Propagate permutes along the graph and compute materialization points.  */
  bool changed;
  unsigned iteration = 0;
  do
    {
      changed = false;
      ++iteration;

      for (i = vertices.length (); i > 0 ; --i)
	{
	  int idx = ipo[i-1];
	  slp_tree node = vertices[idx];
	  /* For leafs there's nothing to do - we've seeded permutes
	     on those above.  */
	  if (SLP_TREE_DEF_TYPE (node) != vect_internal_def)
	    continue;

	  bitmap_set_bit (n_visited, idx);

	  /* We cannot move a permute across a store.  */
	  if (STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (node))
	      && DR_IS_WRITE
		   (STMT_VINFO_DATA_REF (SLP_TREE_REPRESENTATIVE (node))))
	    continue;

	  int perm = -1;
	  for (graph_edge *succ = slpg->vertices[idx].succ;
	       succ; succ = succ->succ_next)
	    {
	      int succ_idx = succ->dest;
	      /* Handle unvisited nodes optimistically.  */
	      /* ???  But for constants once we want to handle non-bijective
		 permutes we have to verify the permute, when unifying lanes,
		 will not unify different constants.  For example see
		 gcc.dg/vect/bb-slp-14.c for a case that would break.  */
	      if (!bitmap_bit_p (n_visited, succ_idx))
		continue;
	      int succ_perm = n_perm[succ_idx];
	      /* Once we materialize succs permutation its output lanes
		 appear unpermuted to us.  */
	      if (bitmap_bit_p (n_materialize, succ_idx))
		succ_perm = 0;
	      if (perm == -1)
		perm = succ_perm;
	      else if (succ_perm == 0)
		{
		  perm = 0;
		  break;
		}
	      else if (!vect_slp_perms_eq (perms, perm, succ_perm))
		{
		  perm = 0;
		  break;
		}
	    }

	  if (perm == -1)
	    /* Pick up pre-computed leaf values.  */
	    perm = n_perm[idx];
	  else if (!vect_slp_perms_eq (perms, perm, n_perm[idx]))
	    {
	      if (iteration > 1)
		/* Make sure we eventually converge.  */
		gcc_checking_assert (perm == 0);
	      n_perm[idx] = perm;
	      if (perm == 0)
		bitmap_clear_bit (n_materialize, idx);
	      changed = true;
	    }

	  if (perm == 0)
	    continue;

	  /* Elide pruning at materialization points in the first
	     iteration so every node was visited once at least.  */
	  if (iteration == 1)
	    continue;

	  /* Decide on permute materialization.  Look whether there's
	     a use (pred) edge that is permuted differently than us.
	     In that case mark ourselves so the permutation is applied.
	     For VEC_PERM_EXPRs the permutation doesn't carry along
	     from children to parents so force materialization at the
	     point of the VEC_PERM_EXPR.  In principle VEC_PERM_EXPRs
	     are a source of an arbitrary permutation again, similar
	     to constants/externals - that's something we do not yet
	     optimally handle.  */
	  bool all_preds_permuted = (SLP_TREE_CODE (node) != VEC_PERM_EXPR
				     && slpg->vertices[idx].pred != NULL);
	  if (all_preds_permuted)
	    for (graph_edge *pred = slpg->vertices[idx].pred;
		 pred; pred = pred->pred_next)
	      {
		gcc_checking_assert (bitmap_bit_p (n_visited, pred->src));
		int pred_perm = n_perm[pred->src];
		if (!vect_slp_perms_eq (perms, perm, pred_perm))
		  {
		    all_preds_permuted = false;
		    break;
		  }
	      }
	  if (!all_preds_permuted)
	    {
	      if (!bitmap_bit_p (n_materialize, idx))
		changed = true;
	      bitmap_set_bit (n_materialize, idx);
	    }
	}
    }
  while (changed || iteration == 1);

  /* Materialize.  */
  for (i = 0; i < vertices.length (); ++i)
    {
      int perm = n_perm[i];
      if (perm <= 0)
	continue;

      slp_tree node = vertices[i];

      /* First permute invariant/external original successors.  */
      unsigned j;
      slp_tree child;
      FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), j, child)
	{
	  if (!child || SLP_TREE_DEF_TYPE (child) == vect_internal_def)
	    continue;

	  /* If the vector is uniform there's nothing to do.  */
	  if (vect_slp_tree_uniform_p (child))
	    continue;

	  /* We can end up sharing some externals via two_operator
	     handling.  Be prepared to unshare those.  */
	  if (child->refcnt != 1)
	    {
	      gcc_assert (slpg->vertices[child->vertex].pred->pred_next);
	      SLP_TREE_CHILDREN (node)[j] = child
		= vect_create_new_slp_node
		    (SLP_TREE_SCALAR_OPS (child).copy ());
	    }
	  vect_slp_permute (perms[perm],
			    SLP_TREE_SCALAR_OPS (child), true);
	}

      if (bitmap_bit_p (n_materialize, i))
	{
	  if (SLP_TREE_LOAD_PERMUTATION (node).exists ())
	    /* For loads simply drop the permutation, the load permutation
	       already performs the desired permutation.  */
	    ;
	  else if (SLP_TREE_LANE_PERMUTATION (node).exists ())
	    {
	      /* If the node is already a permute node we can apply
		 the permutation to the lane selection, effectively
		 materializing it on the incoming vectors.  */
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "simplifying permute node %p\n",
				 node);

	      for (unsigned k = 0;
		   k < SLP_TREE_LANE_PERMUTATION (node).length (); ++k)
		SLP_TREE_LANE_PERMUTATION (node)[k].second
		  = perms[perm][SLP_TREE_LANE_PERMUTATION (node)[k].second];
	    }
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "inserting permute node in place of %p\n",
				 node);

	      /* Make a copy of NODE and in-place change it to a
		 VEC_PERM node to permute the lanes of the copy.  */
	      slp_tree copy = new _slp_tree;
	      SLP_TREE_CHILDREN (copy) = SLP_TREE_CHILDREN (node);
	      SLP_TREE_CHILDREN (node) = vNULL;
	      SLP_TREE_SCALAR_STMTS (copy)
		= SLP_TREE_SCALAR_STMTS (node).copy ();
	      vect_slp_permute (perms[perm],
				SLP_TREE_SCALAR_STMTS (copy), true);
	      gcc_assert (!SLP_TREE_SCALAR_OPS (node).exists ());
	      SLP_TREE_REPRESENTATIVE (copy) = SLP_TREE_REPRESENTATIVE (node);
	      gcc_assert (!SLP_TREE_LOAD_PERMUTATION (node).exists ());
	      SLP_TREE_LANE_PERMUTATION (copy)
		= SLP_TREE_LANE_PERMUTATION (node);
	      SLP_TREE_LANE_PERMUTATION (node) = vNULL;
	      SLP_TREE_VECTYPE (copy) = SLP_TREE_VECTYPE (node);
	      copy->refcnt = 1;
	      copy->max_nunits = node->max_nunits;
	      SLP_TREE_DEF_TYPE (copy) = SLP_TREE_DEF_TYPE (node);
	      SLP_TREE_LANES (copy) = SLP_TREE_LANES (node);
	      SLP_TREE_CODE (copy) = SLP_TREE_CODE (node);

	      /* Now turn NODE into a VEC_PERM.  */
	      SLP_TREE_CHILDREN (node).safe_push (copy);
	      SLP_TREE_LANE_PERMUTATION (node).create (SLP_TREE_LANES (node));
	      for (unsigned j = 0; j < SLP_TREE_LANES (node); ++j)
		SLP_TREE_LANE_PERMUTATION (node)
		  .quick_push (std::make_pair (0, perms[perm][j]));
	      SLP_TREE_CODE (node) = VEC_PERM_EXPR;
	    }
	}
      else
	{
	  /* Apply the reverse permutation to our stmts.  */
	  vect_slp_permute (perms[perm],
			    SLP_TREE_SCALAR_STMTS (node), true);
	  /* And to the load permutation, which we can simply
	     make regular by design.  */
	  if (SLP_TREE_LOAD_PERMUTATION (node).exists ())
	    {
	      /* ???  When we handle non-bijective permutes the idea
		 is that we can force the load-permutation to be
		 { min, min + 1, min + 2, ... max }.  But then the
		 scalar defs might no longer match the lane content
		 which means wrong-code with live lane vectorization.
		 So we possibly have to have NULL entries for those.  */
	      vect_slp_permute (perms[perm],
				SLP_TREE_LOAD_PERMUTATION (node), true);
	    }
	}
    }

  /* Free the perms vector used for propagation.  */
  while (!perms.is_empty ())
    perms.pop ().release ();
  free_graph (slpg);


  /* Now elide load permutations that are not necessary.  */
  for (i = 0; i < leafs.length (); ++i)
    {
      node = vertices[leafs[i]];
      if (!SLP_TREE_LOAD_PERMUTATION (node).exists ())
	continue;

      /* In basic block vectorization we allow any subchain of an interleaving
	 chain.
	 FORNOW: not in loop SLP because of realignment complications.  */
      if (is_a <bb_vec_info> (vinfo))
	{
	  bool subchain_p = true;
	  stmt_vec_info next_load_info = NULL;
	  stmt_vec_info load_info;
	  unsigned j;
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    {
	      if (j != 0
		  && (next_load_info != load_info
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
	  stmt_vec_info load_info;
	  bool this_load_permuted = false;
	  unsigned j;
	  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), j, load_info)
	    if (SLP_TREE_LOAD_PERMUTATION (node)[j] != j)
	      {
		this_load_permuted = true;
		break;
	      }
	  stmt_vec_info first_stmt_info
	    = DR_GROUP_FIRST_ELEMENT (SLP_TREE_SCALAR_STMTS (node)[0]);
	  if (!this_load_permuted
	      /* The load requires permutation when unrolling exposes
		 a gap either because the group is larger than the SLP
		 group-size or because there is a gap between the groups.  */
	      && (known_eq (LOOP_VINFO_VECT_FACTOR
			      (as_a <loop_vec_info> (vinfo)), 1U)
		  || ((SLP_TREE_LANES (node) == DR_GROUP_SIZE (first_stmt_info))
		      && DR_GROUP_GAP (first_stmt_info) == 0)))
	    {
	      SLP_TREE_LOAD_PERMUTATION (node).release ();
	      continue;
	    }
	}
    }
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
  vec<slp_instance> slp_instances = LOOP_VINFO_SLP_INSTANCES (loop_vinfo);
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
  /* No def means this is a loo_vect sink.  */
  if (!any_def)
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
    }
}


/* Initialize a bb_vec_info struct for the statements in BBS basic blocks.  */

_bb_vec_info::_bb_vec_info (vec<basic_block> _bbs, vec_info_shared *shared)
  : vec_info (vec_info::bb, init_cost (NULL), shared), bbs (_bbs), roots (vNULL)
{
  for (unsigned i = 0; i < bbs.length (); ++i)
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
  for (unsigned i = 0; i < bbs.length (); ++i)
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
    roots[i].stmts.release ();
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

  /* Calculate the number of vector statements to be created for the
     scalar stmts in this node.  For SLP reductions it is equal to the
     number of vector statements in the children (which has already been
     calculated by the recursive call).  Otherwise it is the number of
     scalar elements in one scalar iteration (DR_GROUP_SIZE) multiplied by
     VF divided by the number of elements in a vector.  */
  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info)
      && REDUC_GROUP_FIRST_ELEMENT (stmt_info))
    {
      for (unsigned i = 0; i < SLP_TREE_CHILDREN (node).length (); ++i)
	if (SLP_TREE_DEF_TYPE (SLP_TREE_CHILDREN (node)[i]) == vect_internal_def)
	  {
	    SLP_TREE_NUMBER_OF_VEC_STMTS (node)
	      = SLP_TREE_NUMBER_OF_VEC_STMTS (SLP_TREE_CHILDREN (node)[i]);
	    break;
	  }
    }
  else
    {
      poly_uint64 vf;
      if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
	vf = loop_vinfo->vectorization_factor;
      else
	vf = 1;
      unsigned int group_size = SLP_TREE_LANES (node);
      tree vectype = SLP_TREE_VECTYPE (node);
      SLP_TREE_NUMBER_OF_VEC_STMTS (node)
	= vect_get_num_vectors (vf * group_size, vectype);
    }

  /* Handle purely internal nodes.  */
  if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
    return vectorizable_slp_permutation (vinfo, NULL, node, cost_vec);

  gcc_assert (STMT_SLP_TYPE (stmt_info) != loop_vect);
  if (is_a <bb_vec_info> (vinfo)
      && !vect_update_shared_vectype (stmt_info, SLP_TREE_VECTYPE (node)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "desired vector type conflicts with earlier one "
			 "for %G", stmt_info->stmt);
      return false;
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
      || vect_contains_pattern_stmt_p (SLP_TREE_SCALAR_STMTS (node)))
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Building vector operands of %p from scalars instead\n", node);

  /* Don't remove and free the child nodes here, since they could be
     referenced by other structures.  The analysis and scheduling phases
     (need to) ignore child nodes of anything that isn't vect_internal_def.  */
  unsigned int group_size = SLP_TREE_LANES (node);
  SLP_TREE_DEF_TYPE (node) = vect_external_def;
  SLP_TREE_SCALAR_OPS (node).safe_grow (group_size, true);
  SLP_TREE_LOAD_PERMUTATION (node).release ();
  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      tree lhs = gimple_get_lhs (vect_orig_stmt (stmt_info)->stmt);
      SLP_TREE_SCALAR_OPS (node)[i] = lhs;
    }
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
  unsigned num_vects_to_check;
  unsigned HOST_WIDE_INT const_nunits;
  unsigned nelt_limit;
  if (TYPE_VECTOR_SUBPARTS (vectype).is_constant (&const_nunits)
      && ! multiple_p (const_nunits, group_size))
    {
      num_vects_to_check = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
      nelt_limit = const_nunits;
    }
  else
    {
      /* If either the vector has variable length or the vectors
	 are composed of repeated whole groups we only need to
	 cost construction once.  All vectors will be the same.  */
      num_vects_to_check = 1;
      nelt_limit = group_size;
    }
  tree elt = NULL_TREE;
  unsigned nelt = 0;
  for (unsigned j = 0; j < num_vects_to_check * nelt_limit; ++j)
    {
      unsigned si = j % group_size;
      if (nelt == 0)
	elt = SLP_TREE_SCALAR_OPS (node)[si];
      /* ???  We're just tracking whether all operands of a single
	 vector initializer are the same, ideally we'd check if
	 we emitted the same one already.  */
      else if (elt != SLP_TREE_SCALAR_OPS (node)[si])
	elt = NULL_TREE;
      nelt++;
      if (nelt == nelt_limit)
	{
	  record_stmt_cost (cost_vec, 1,
			    SLP_TREE_DEF_TYPE (node) == vect_external_def
			    ? (elt ? scalar_to_vec : vec_construct)
			    : vector_load,
			    NULL, vectype, 0, vect_prologue);
	  nelt = 0;
	}
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
			 "Failed cyclic SLP reference in %p\n", node);
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
  if (!seen_non_constant_child && SLP_TREE_SCALAR_STMTS (node).is_empty ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Cannot vectorize all-constant op node %p\n", node);
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
	  unsigned group_size = SLP_TREE_LANES (child);
	  poly_uint64 vf = 1;
	  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
	    vf = loop_vinfo->vectorization_factor;
	  SLP_TREE_NUMBER_OF_VEC_STMTS (child)
	    = vect_get_num_vectors (vf * group_size, vector_type);
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


/* Mark lanes of NODE that are live outside of the basic-block vectorized
   region and that can be vectorized using vectorizable_live_operation
   with STMT_VINFO_LIVE_P.  Not handled live operations will cause the
   scalar code computing it to be retained.  */

static void
vect_bb_slp_mark_live_stmts (bb_vec_info bb_vinfo, slp_tree node,
			     slp_instance instance,
			     stmt_vector_for_cost *cost_vec,
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
      if (svisited.contains (stmt_info))
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
	  imm_use_iterator use_iter;
	  gimple *use_stmt;
	  stmt_vec_info use_stmt_info;
	  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, DEF_FROM_PTR (def_p))
	    if (!is_gimple_debug (use_stmt))
	      {
		use_stmt_info = bb_vinfo->lookup_stmt (use_stmt);
		if (!use_stmt_info
		    || !PURE_SLP_STMT (vect_stmt_to_vectorize (use_stmt_info)))
		  {
		    STMT_VINFO_LIVE_P (stmt_info) = true;
		    if (vectorizable_live_operation (bb_vinfo, stmt_info,
						     NULL, node, instance, i,
						     false, cost_vec))
		      /* ???  So we know we can vectorize the live stmt
			 from one SLP node.  If we cannot do so from all
			 or none consistently we'd have to record which
			 SLP node (and lane) we want to use for the live
			 operation.  So make sure we can code-generate
			 from all nodes.  */
		      mark_visited = false;
		    else
		      STMT_VINFO_LIVE_P (stmt_info) = false;
		    break;
		  }
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
      vect_bb_slp_mark_live_stmts (bb_vinfo, child, instance,
				   cost_vec, svisited, visited);
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
	  /* Instances with a root stmt require vectorized defs for the
	     SLP tree root.  */
	  || (SLP_INSTANCE_ROOT_STMT (instance)
	      && (SLP_TREE_DEF_TYPE (SLP_INSTANCE_TREE (instance))
		  != vect_internal_def)))
        {
	  slp_tree node = SLP_INSTANCE_TREE (instance);
	  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "removing SLP instance operations starting from: %G",
			     stmt_info->stmt);
	  vect_free_slp_instance (instance);
          vinfo->slp_instances.ordered_remove (i);
	  cost_vec.release ();
	  while (!visited_vec.is_empty ())
	    visited.remove (visited_vec.pop ());
	}
      else
	{
	  i++;

	  /* For BB vectorization remember the SLP graph entry
	     cost for later.  */
	  if (is_a <bb_vec_info> (vinfo))
	    instance->cost_vec = cost_vec;
	  else
	    {
	      add_stmt_costs (vinfo, vinfo->target_cost_data, &cost_vec);
	      cost_vec.release ();
	    }
	}
    }

  /* Compute vectorizable live stmts.  */
  if (bb_vec_info bb_vinfo = dyn_cast <bb_vec_info> (vinfo))
    {
      hash_set<stmt_vec_info> svisited;
      hash_set<slp_tree> visited;
      for (i = 0; vinfo->slp_instances.iterate (i, &instance); ++i)
	{
	  vect_location = instance->location ();
	  vect_bb_slp_mark_live_stmts (bb_vinfo, SLP_INSTANCE_TREE (instance),
				       instance, &instance->cost_vec, svisited,
				       visited);
	}
    }

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

/* Worker of vect_bb_partition_graph, recurse on NODE.  */

static void
vect_bb_partition_graph_r (bb_vec_info bb_vinfo,
			   slp_instance instance, slp_tree node,
			   hash_map<stmt_vec_info, slp_instance> &stmt_to_instance,
			   hash_map<slp_instance, slp_instance> &instance_leader,
			   hash_set<slp_tree> &visited)
{
  stmt_vec_info stmt_info;
  unsigned i;

  FOR_EACH_VEC_ELT (SLP_TREE_SCALAR_STMTS (node), i, stmt_info)
    {
      bool existed_p;
      slp_instance &stmt_instance
	= stmt_to_instance.get_or_insert (stmt_info, &existed_p);
      if (!existed_p)
	;
      else if (stmt_instance != instance)
	{
	  /* If we're running into a previously marked stmt make us the
	     leader of the current ultimate leader.  This keeps the
	     leader chain acyclic and works even when the current instance
	     connects two previously independent graph parts.  */
	  slp_instance stmt_leader
	    = get_ultimate_leader (stmt_instance, instance_leader);
	  if (stmt_leader != instance)
	    instance_leader.put (stmt_leader, instance);
	}
      stmt_instance = instance;
    }

  if (!SLP_TREE_SCALAR_STMTS (node).is_empty () && visited.add (node))
    return;

  slp_tree child;
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    if (child && SLP_TREE_DEF_TYPE (child) == vect_internal_def)
      vect_bb_partition_graph_r (bb_vinfo, instance, child, stmt_to_instance,
				 instance_leader, visited);
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
  hash_map<slp_instance, slp_instance> instance_leader;
  hash_set<slp_tree> visited;
  slp_instance instance;
  for (unsigned i = 0; bb_vinfo->slp_instances.iterate (i, &instance); ++i)
    {
      instance_leader.put (instance, instance);
      vect_bb_partition_graph_r (bb_vinfo,
				 instance, SLP_INSTANCE_TREE (instance),
				 stmt_to_instance, instance_leader,
				 visited);
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
			 leader, instance);
    }
}

/* Compute the scalar cost of the SLP node NODE and its children
   and return it.  Do not account defs that are marked in LIFE and
   update LIFE according to uses of NODE.  */

static void
vect_bb_slp_scalar_cost (vec_info *vinfo,
			 slp_tree node, vec<bool, va_heap> *life,
			 stmt_vector_for_cost *cost_vec,
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

      if ((*life)[i])
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
	  FOR_EACH_PHI_OR_STMT_DEF (def_p, orig_stmt, op_iter, SSA_OP_DEF)
	    {
	      imm_use_iterator use_iter;
	      gimple *use_stmt;
	      FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, DEF_FROM_PTR (def_p))
		if (!is_gimple_debug (use_stmt))
		  {
		    stmt_vec_info use_stmt_info = vinfo->lookup_stmt (use_stmt);
		    if (!use_stmt_info
			|| !PURE_SLP_STMT
			      (vect_stmt_to_vectorize (use_stmt_info)))
		      {
			(*life)[i] = true;
			break;
		      }
		  }
	    }
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
	  if (DR_IS_READ (STMT_VINFO_DATA_REF (orig_stmt_info)))
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
				   visited);
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
				    vec<slp_instance> slp_instances)
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

  /* Calculate scalar cost and sum the cost for the vector stmts
     previously collected.  */
  stmt_vector_for_cost scalar_costs = vNULL;
  stmt_vector_for_cost vector_costs = vNULL;
  hash_set<slp_tree> visited;
  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    {
      auto_vec<bool, 20> life;
      life.safe_grow_cleared (SLP_TREE_LANES (SLP_INSTANCE_TREE (instance)),
			      true);
      if (SLP_INSTANCE_ROOT_STMT (instance))
	record_stmt_cost (&scalar_costs, 1, scalar_stmt,
			  SLP_INSTANCE_ROOT_STMT (instance), 0, vect_body);
      vect_bb_slp_scalar_cost (bb_vinfo,
			       SLP_INSTANCE_TREE (instance),
			       &life, &scalar_costs, visited);
      vector_costs.safe_splice (instance->cost_vec);
      instance->cost_vec.release ();
    }
  /* Unset visited flag.  */
  stmt_info_for_cost *cost;
  FOR_EACH_VEC_ELT (scalar_costs, i, cost)
    gimple_set_visited  (cost->stmt_info->stmt, false);

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

      void *scalar_target_cost_data = init_cost (NULL);
      do
	{
	  add_stmt_cost (bb_vinfo, scalar_target_cost_data,
			 li_scalar_costs[si].second);
	  si++;
	}
      while (si < li_scalar_costs.length ()
	     && li_scalar_costs[si].first == sl);
      unsigned dummy;
      finish_cost (scalar_target_cost_data, &dummy, &scalar_cost, &dummy);
      destroy_cost_data (scalar_target_cost_data);

      /* Complete the target-specific vector cost calculation.  */
      void *vect_target_cost_data = init_cost (NULL);
      do
	{
	  add_stmt_cost (bb_vinfo, vect_target_cost_data,
			 li_vector_costs[vi].second);
	  vi++;
	}
      while (vi < li_vector_costs.length ()
	     && li_vector_costs[vi].first == vl);
      finish_cost (vect_target_cost_data, &vec_prologue_cost,
		   &vec_inside_cost, &vec_epilogue_cost);
      destroy_cost_data (vect_target_cost_data);

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
	  scalar_costs.release ();
	  vector_costs.release ();
	  return false;
	}
    }
  if (vi < li_vector_costs.length ())
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Excess vector cost for part in loop %d:\n",
			 li_vector_costs[vi].first);
      scalar_costs.release ();
      vector_costs.release ();
      return false;
    }

  scalar_costs.release ();
  vector_costs.release ();
  return true;
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
vect_slp_check_for_constructors (bb_vec_info bb_vinfo)
{
  for (unsigned i = 0; i < bb_vinfo->bbs.length (); ++i)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb_vinfo->bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gassign *assign = dyn_cast<gassign *> (gsi_stmt (gsi));
      if (!assign)
	continue;

      tree rhs = gimple_assign_rhs1 (assign);
      if (gimple_assign_rhs_code (assign) == CONSTRUCTOR)
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

	  stmt_vec_info stmt_info = bb_vinfo->lookup_stmt (assign);
	  BB_VINFO_GROUPED_STORES (bb_vinfo).safe_push (stmt_info);
	}
      else if (gimple_assign_rhs_code (assign) == BIT_INSERT_EXPR
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
	      def = gimple_assign_lhs (use_ass);
	    }
	  while (lanes_found < nlanes);
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
						   stmts, last));
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

  vect_slp_check_for_constructors (bb_vinfo);

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
  if (!vect_analyze_slp (bb_vinfo, n_stmts))
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
      if (stmt_vec_info root = SLP_INSTANCE_ROOT_STMT (instance))
	{
	  STMT_SLP_TYPE (root) = pure_slp;
	  if (is_gimple_assign (root->stmt)
	      && gimple_assign_rhs_code (root->stmt) == BIT_INSERT_EXPR)
	    {
	      /* ???  We should probably record the whole vector of
		 root stmts so we do not have to back-track here...  */
	      for (unsigned n = SLP_TREE_LANES (SLP_INSTANCE_TREE (instance));
		   n != 1; --n)
		{
		  root = bb_vinfo->lookup_def (gimple_assign_rhs1 (root->stmt));
		  STMT_SLP_TYPE (root) = pure_slp;
		}
	    }
	}

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
		 vec<int> *dataref_groups, unsigned int n_stmts)
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

	  unsigned i;
	  slp_instance instance;
	  FOR_EACH_VEC_ELT (BB_VINFO_SLP_INSTANCES (bb_vinfo), i, instance)
	    {
	      if (instance->subgraph_entries.is_empty ())
		continue;

	      vect_location = instance->location ();
	      if (!unlimited_cost_model (NULL)
		  && !vect_bb_vectorization_profitable_p
			(bb_vinfo, instance->subgraph_entries))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "not vectorized: vectorization is not "
				     "profitable.\n");
		  continue;
		}

	      if (!dbg_cnt (vect_slp))
		continue;

	      if (!vectorized && dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "Basic block will be vectorized "
				 "using SLP\n");
	      vectorized = true;

	      vect_schedule_slp (bb_vinfo, instance->subgraph_entries);

	      unsigned HOST_WIDE_INT bytes;
	      if (dump_enabled_p ())
		{
		  if (GET_MODE_SIZE
			(bb_vinfo->vector_mode).is_constant (&bytes))
		    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
				     "basic block part vectorized using %wu "
				     "byte vectors\n", bytes);
		  else
		    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
				     "basic block part vectorized using "
				     "variable length vectors\n");
		}
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
vect_slp_bbs (vec<basic_block> bbs)
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
    }

  return vect_slp_region (bbs, datarefs, &dataref_groups, insns);
}

/* Main entry for the BB vectorizer.  Analyze and transform BB, returns
   true if anything in the basic-block was vectorized.  */

bool
vect_slp_bb (basic_block bb)
{
  auto_vec<basic_block> bbs;
  bbs.safe_push (bb);
  return vect_slp_bbs (bbs);
}

/* Main entry for the BB vectorizer.  Analyze and transform BB, returns
   true if anything in the basic-block was vectorized.  */

bool
vect_slp_function (function *fun)
{
  bool r = false;
  int *rpo = XNEWVEC (int, n_basic_blocks_for_fn (fun));
  unsigned n = pre_and_rev_post_order_compute_fn (fun, NULL, rpo, false);

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

      if (split && !bbs.is_empty ())
	{
	  r |= vect_slp_bbs (bbs);
	  bbs.truncate (0);
	  bbs.quick_push (bb);
	}
      else
	bbs.safe_push (bb);

      /* When we have a stmt ending this block and defining a
	 value we have to insert on edges when inserting after it for
	 a vector containing its definition.  Avoid this for now.  */
      if (gimple *last = last_stmt (bb))
	if (gimple_get_lhs (last)
	    && is_ctrl_altering_stmt (last))
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "splitting region at control altering "
			       "definition %G", last);
	    r |= vect_slp_bbs (bbs);
	    bbs.truncate (0);
	  }
    }

  if (!bbs.is_empty ())
    r |= vect_slp_bbs (bbs);

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
			  vec<tree> elts, unsigned int nresults,
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
  tree_vector_builder elts (vector_type, nunits, 1);
  elts.quick_grow (nunits);
  stmt_vec_info insert_after = NULL;
  for (j = 0; j < number_of_copies; j++)
    {
      tree op;
      for (i = group_size - 1; op_node->ops.iterate (i, &op); i--)
        {
          /* Create 'vect_ = {op0,op1,...,opn}'.  */
          number_of_places_left_in_vector--;
	  tree orig_op = op;
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
	      if (constant_p
		  ? multiple_p (TYPE_VECTOR_SUBPARTS (vector_type), nunits)
		  : known_eq (TYPE_VECTOR_SUBPARTS (vector_type), nunits))
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
  if (SLP_TREE_VEC_STMTS (slp_node).exists ())
    return gimple_get_lhs (SLP_TREE_VEC_STMTS (slp_node)[i]);
  else
    return SLP_TREE_VEC_DEFS (slp_node)[i];
}

/* Get the vectorized definitions of SLP_NODE in *VEC_DEFS.  */

void
vect_get_slp_defs (slp_tree slp_node, vec<tree> *vec_defs)
{
  vec_defs->create (SLP_TREE_NUMBER_OF_VEC_STMTS (slp_node));
  if (SLP_TREE_DEF_TYPE (slp_node) == vect_internal_def)
    {
      unsigned j;
      gimple *vec_def_stmt;
      FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (slp_node), j, vec_def_stmt)
	vec_defs->quick_push (gimple_get_lhs (vec_def_stmt));
    }
  else
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

/* Generate vector permute statements from a list of loads in DR_CHAIN.
   If ANALYZE_ONLY is TRUE, only check that it is possible to create valid
   permute statements for the SLP node NODE.  Store the number of vector
   permute instructions in *N_PERMS and the number of vector load
   instructions in *N_LOADS.  */

bool
vect_transform_slp_perm_load (vec_info *vinfo,
			      slp_tree node, vec<tree> dr_chain,
			      gimple_stmt_iterator *gsi, poly_uint64 vf,
			      bool analyze_only, unsigned *n_perms,
			      unsigned int *n_loads)
{
  stmt_vec_info stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
  int vec_index = 0;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  unsigned int group_size = SLP_TREE_SCALAR_STMTS (node).length ();
  unsigned int mask_element;
  machine_mode mode;

  if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
    return false;

  stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);

  mode = TYPE_MODE (vectype);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  /* Initialize the vect stmts of NODE to properly insert the generated
     stmts later.  */
  if (! analyze_only)
    for (unsigned i = SLP_TREE_VEC_STMTS (node).length ();
	 i < SLP_TREE_NUMBER_OF_VEC_STMTS (node); i++)
      SLP_TREE_VEC_STMTS (node).quick_push (NULL);

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
  bool repeating_p = (group_size == DR_GROUP_SIZE (stmt_info)
		      && multiple_p (nunits, group_size));
  if (repeating_p)
    {
      /* A single vector contains a whole number of copies of the node, so:
	 (a) all permutes can use the same mask; and
	 (b) the permutes only need a single vector input.  */
      mask.new_vector (nunits, group_size, 3);
      nelts_to_build = mask.encoded_nelts ();
      nvectors_per_build = SLP_TREE_VEC_STMTS (node).length ();
      in_nlanes = DR_GROUP_SIZE (stmt_info) * 3;
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
      in_nlanes = const_vf * DR_GROUP_SIZE (stmt_info);
    }
  auto_sbitmap used_in_lanes (in_nlanes);
  bitmap_clear (used_in_lanes);

  unsigned int count = mask.encoded_nelts ();
  mask.quick_grow (count);
  vec_perm_indices indices;

  for (unsigned int j = 0; j < nelts_to_build; j++)
    {
      unsigned int iter_num = j / group_size;
      unsigned int stmt_num = j % group_size;
      unsigned int i = (iter_num * DR_GROUP_SIZE (stmt_info)
			+ SLP_TREE_LOAD_PERMUTATION (node)[stmt_num]);
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
	      if (dump_enabled_p ())
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

      if (index == count && !noop_p)
	{
	  indices.new_vector (mask, second_vec_index == -1 ? 1 : 2, nunits);
	  if (!can_vec_perm_const_p (mode, indices))
	    {
	      if (dump_enabled_p ())
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
	      gcc_assert (analyze_only);
	      return false;
	    }

	  ++*n_perms;
	}

      if (index == count)
	{
	  if (!analyze_only)
	    {
	      tree mask_vec = NULL_TREE;

	      if (! noop_p)
		mask_vec = vect_gen_perm_mask_checked (vectype, indices);

	      if (second_vec_index == -1)
		second_vec_index = first_vec_index;

	      for (unsigned int ri = 0; ri < nvectors_per_build; ++ri)
		{
		  /* Generate the permute statement if necessary.  */
		  tree first_vec = dr_chain[first_vec_index + ri];
		  tree second_vec = dr_chain[second_vec_index + ri];
		  gimple *perm_stmt;
		  if (! noop_p)
		    {
		      gassign *stmt = as_a <gassign *> (stmt_info->stmt);
		      tree perm_dest
			= vect_create_destination_var (gimple_assign_lhs (stmt),
						       vectype);
		      perm_dest = make_ssa_name (perm_dest);
		      perm_stmt
			= gimple_build_assign (perm_dest, VEC_PERM_EXPR,
					       first_vec, second_vec,
					       mask_vec);
		      vect_finish_stmt_generation (vinfo, stmt_info, perm_stmt,
						   gsi);
		    }
		  else
		    /* If mask was NULL_TREE generate the requested
		       identity transform.  */
		    perm_stmt = SSA_NAME_DEF_STMT (first_vec);

		  /* Store the vector statement in NODE.  */
		  SLP_TREE_VEC_STMTS (node)[vect_stmts_counter++] = perm_stmt;
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

  return true;
}

/* Produce the next vector result for SLP permutation NODE by adding a vector
   statement at GSI.  If MASK_VEC is nonnull, add:

      <new SSA name> = VEC_PERM_EXPR <FIRST_DEF, SECOND_DEF, MASK_VEC>

   otherwise add:

      <new SSA name> = FIRST_DEF.  */

static void
vect_add_slp_permutation (vec_info *vinfo, gimple_stmt_iterator *gsi,
			  slp_tree node, tree first_def, tree second_def,
			  tree mask_vec)
{
  tree vectype = SLP_TREE_VECTYPE (node);

  /* ???  We SLP match existing vector element extracts but
     allow punning which we need to re-instantiate at uses
     but have no good way of explicitly representing.  */
  if (!types_compatible_p (TREE_TYPE (first_def), vectype))
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
      if (!types_compatible_p (TREE_TYPE (second_def), vectype))
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
  else
    /* We need a copy here in case the def was external.  */
    perm_stmt = gimple_build_assign (perm_dest, first_def);
  vect_finish_stmt_generation (vinfo, NULL, perm_stmt, gsi);
  /* Store the vector statement in NODE.  */
  SLP_TREE_VEC_STMTS (node).quick_push (perm_stmt);
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

  /* ???  We currently only support all same vector input and output types
     while the SLP IL should really do a concat + select and thus accept
     arbitrary mismatches.  */
  slp_tree child;
  unsigned i;
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);
  bool repeating_p = multiple_p (nunits, SLP_TREE_LANES (node));
  FOR_EACH_VEC_ELT (SLP_TREE_CHILDREN (node), i, child)
    {
      if (!vect_maybe_update_slp_op_vectype (child, vectype)
	  || !types_compatible_p (SLP_TREE_VECTYPE (child), vectype))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Unsupported lane permutation\n");
	  return false;
	}
      if (SLP_TREE_LANES (child) != SLP_TREE_LANES (node))
	repeating_p = false;
    }

  vec<std::pair<unsigned, unsigned> > &perm = SLP_TREE_LANE_PERMUTATION (node);
  gcc_assert (perm.length () == SLP_TREE_LANES (node));
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "vectorizing permutation");
      for (unsigned i = 0; i < perm.length (); ++i)
	dump_printf (MSG_NOTE, " op%u[%u]", perm[i].first, perm[i].second);
      if (repeating_p)
	dump_printf (MSG_NOTE, " (repeat %d)\n", SLP_TREE_LANES (node));
      dump_printf (MSG_NOTE, "\n");
    }

  /* REPEATING_P is true if every output vector is guaranteed to use the
     same permute vector.  We can handle that case for both variable-length
     and constant-length vectors, but we only handle other cases for
     constant-length vectors.

     Set:

     - NPATTERNS and NELTS_PER_PATTERN to the encoding of the permute
       mask vector that we want to build.

     - NCOPIES to the number of copies of PERM that we need in order
       to build the necessary permute mask vectors.

     - NOUTPUTS_PER_MASK to the number of output vectors we want to create
       for each permute mask vector.  This is only relevant when GSI is
       nonnull.  */
  uint64_t npatterns;
  unsigned nelts_per_pattern;
  uint64_t ncopies;
  unsigned noutputs_per_mask;
  if (repeating_p)
    {
      /* We need a single permute mask vector that has the form:

	   { X1, ..., Xn, X1 + n, ..., Xn + n, X1 + 2n, ..., Xn + 2n, ... }

	 In other words, the original n-element permute in PERM is
	 "unrolled" to fill a full vector.  The stepped vector encoding
	 that we use for permutes requires 3n elements.  */
      npatterns = SLP_TREE_LANES (node);
      nelts_per_pattern = ncopies = 3;
      noutputs_per_mask = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
    }
  else
    {
      /* Calculate every element of every permute mask vector explicitly,
	 instead of relying on the pattern described above.  */
      if (!nunits.is_constant (&npatterns))
	return false;
      nelts_per_pattern = ncopies = 1;
      if (loop_vec_info linfo = dyn_cast <loop_vec_info> (vinfo))
	if (!LOOP_VINFO_VECT_FACTOR (linfo).is_constant (&ncopies))
	  return false;
      noutputs_per_mask = 1;
    }
  unsigned olanes = ncopies * SLP_TREE_LANES (node);
  gcc_assert (repeating_p || multiple_p (olanes, nunits));

  /* Compute the { { SLP operand, vector index}, lane } permutation sequence
     from the { SLP operand, scalar lane } permutation as recorded in the
     SLP node as intermediate step.  This part should already work
     with SLP children with arbitrary number of lanes.  */
  auto_vec<std::pair<std::pair<unsigned, unsigned>, unsigned> > vperm;
  auto_vec<unsigned> active_lane;
  vperm.create (olanes);
  active_lane.safe_grow_cleared (SLP_TREE_CHILDREN (node).length (), true);
  for (unsigned i = 0; i < ncopies; ++i)
    {
      for (unsigned pi = 0; pi < perm.length (); ++pi)
	{
	  std::pair<unsigned, unsigned> p = perm[pi];
	  tree vtype = SLP_TREE_VECTYPE (SLP_TREE_CHILDREN (node)[p.first]);
	  if (repeating_p)
	    vperm.quick_push ({{p.first, 0}, p.second + active_lane[p.first]});
	  else
	    {
	      /* We checked above that the vectors are constant-length.  */
	      unsigned vnunits = TYPE_VECTOR_SUBPARTS (vtype).to_constant ();
	      unsigned vi = (active_lane[p.first] + p.second) / vnunits;
	      unsigned vl = (active_lane[p.first] + p.second) % vnunits;
	      vperm.quick_push ({{p.first, vi}, vl});
	    }
	}
      /* Advance to the next group.  */
      for (unsigned j = 0; j < SLP_TREE_CHILDREN (node).length (); ++j)
	active_lane[j] += SLP_TREE_LANES (SLP_TREE_CHILDREN (node)[j]);
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "as");
      for (unsigned i = 0; i < vperm.length (); ++i)
	{
	  if (i != 0
	      && (repeating_p
		  ? multiple_p (i, npatterns)
		  : multiple_p (i, TYPE_VECTOR_SUBPARTS (vectype))))
	    dump_printf (MSG_NOTE, ",");
	  dump_printf (MSG_NOTE, " vops%u[%u][%u]",
		       vperm[i].first.first, vperm[i].first.second,
		       vperm[i].second);
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
  for (unsigned i = 0; i < vperm.length (); ++i)
    {
      mask_element = vperm[i].second;
      if (first_vec.first == -1U
	  || first_vec == vperm[i].first)
	first_vec = vperm[i].first;
      else if (second_vec.first == -1U
	       || second_vec == vperm[i].first)
	{
	  second_vec = vperm[i].first;
	  mask_element += nunits;
	}
      else
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "permutation requires at "
			     "least three vectors\n");
	  gcc_assert (!gsi);
	  return false;
	}

      mask[index++] = mask_element;

      if (index == count)
	{
	  indices.new_vector (mask, second_vec.first == -1U ? 1 : 2, nunits);
	  bool identity_p = indices.series_p (0, 1, 0, 1);
	  if (!identity_p
	      && !can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	    {
	      if (dump_enabled_p ())
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
	      return false;
	    }

	  if (!identity_p)
	    nperms++;
	  if (gsi)
	    {
	      if (second_vec.first == -1U)
		second_vec = first_vec;

	      slp_tree
		first_node = SLP_TREE_CHILDREN (node)[first_vec.first],
		second_node = SLP_TREE_CHILDREN (node)[second_vec.first];

	      tree mask_vec = NULL_TREE;
	      if (!identity_p)
		mask_vec = vect_gen_perm_mask_checked (vectype, indices);

	      for (unsigned int vi = 0; vi < noutputs_per_mask; ++vi)
		{
		  tree first_def
		    = vect_get_slp_vect_def (first_node,
					     first_vec.second + vi);
		  tree second_def
		    = vect_get_slp_vect_def (second_node,
					     second_vec.second + vi);
		  vect_add_slp_permutation (vinfo, gsi, node, first_def,
					    second_def, mask_vec);
		}
	    }

	  index = 0;
	  first_vec = std::make_pair (-1U, -1U);
	  second_vec = std::make_pair (-1U, -1U);
	}
    }

  if (!gsi)
    record_stmt_cost (cost_vec, nperms, vec_perm, NULL, vectype, 0, vect_body);

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

  /* For existing vectors there's nothing to do.  */
  if (SLP_TREE_VEC_DEFS (node).exists ())
    return;

  gcc_assert (SLP_TREE_VEC_STMTS (node).is_empty ());

  /* Vectorize externals and constants.  */
  if (SLP_TREE_DEF_TYPE (node) == vect_constant_def
      || SLP_TREE_DEF_TYPE (node) == vect_external_def)
    {
      /* ???  vectorizable_shift can end up using a scalar operand which is
	 currently denoted as !SLP_TREE_VECTYPE.  No need to vectorize the
	 node in this case.  */
      if (!SLP_TREE_VECTYPE (node))
	return;

      vect_create_constant_vectors (vinfo, node);
      return;
    }

  stmt_vec_info stmt_info = SLP_TREE_REPRESENTATIVE (node);

  gcc_assert (SLP_TREE_NUMBER_OF_VEC_STMTS (node) != 0);
  SLP_TREE_VEC_STMTS (node).create (SLP_TREE_NUMBER_OF_VEC_STMTS (node));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "------>vectorizing SLP node starting from: %G",
		     stmt_info->stmt);

  if (STMT_VINFO_DATA_REF (stmt_info)
      && SLP_TREE_CODE (node) != VEC_PERM_EXPR)
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
  else if ((STMT_VINFO_TYPE (stmt_info) == cycle_phi_info_type
	    || STMT_VINFO_TYPE (stmt_info) == induc_vec_info_type
	    || STMT_VINFO_TYPE (stmt_info) == phi_info_type)
	   && SLP_TREE_CODE (node) != VEC_PERM_EXPR)
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
	    if (SLP_TREE_VEC_STMTS (child).is_empty ())
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
	    gimple *vstmt;
	    FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (child), j, vstmt)
	      if (!last_stmt
		  || vect_stmt_dominates_stmt_p (last_stmt, vstmt))
		last_stmt = vstmt;
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
	  si = gsi_after_labels (as_a <bb_vec_info> (vinfo)->bbs[0]);
	}
      else if (is_a <gphi *> (last_stmt))
	si = gsi_after_labels (gimple_bb (last_stmt));
      else
	{
	  si = gsi_for_stmt (last_stmt);
	  gsi_next (&si);
	}
    }

  bool done_p = false;

  /* Handle purely internal nodes.  */
  if (SLP_TREE_CODE (node) == VEC_PERM_EXPR)
    {
      /* ???  the transform kind is stored to STMT_VINFO_TYPE which might
	 be shared with different SLP nodes (but usually it's the same
	 operation apart from the case the stmt is only there for denoting
	 the actual scalar lane defs ...).  So do not call vect_transform_stmt
	 but open-code it here (partly).  */
      bool done = vectorizable_slp_permutation (vinfo, &si, node, NULL);
      gcc_assert (done);
      done_p = true;
    }
  if (!done_p)
    vect_transform_stmt (vinfo, stmt_info, &si, node, instance);
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
      gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt);
      if (!stmt || gimple_bb (stmt) == NULL)
	continue;
      if (is_pattern_stmt_p (stmt_info)
	  || !PURE_SLP_STMT (stmt_info))
	continue;
      lhs = gimple_call_lhs (stmt);
      new_stmt = gimple_build_assign (lhs, build_zero_cst (TREE_TYPE (lhs)));
      gsi = gsi_for_stmt (stmt);
      vinfo->replace_stmt (&gsi, stmt_info, new_stmt);
      SSA_NAME_DEF_STMT (gimple_assign_lhs (new_stmt)) = new_stmt;
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
vectorize_slp_instance_root_stmt (slp_tree node, slp_instance instance)
{
  gassign *rstmt = NULL;

  if (SLP_TREE_NUMBER_OF_VEC_STMTS (node) == 1)
    {
      gimple *child_stmt;
      int j;

      FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (node), j, child_stmt)
	{
	  tree vect_lhs = gimple_get_lhs (child_stmt);
	  tree root_lhs = gimple_get_lhs (instance->root_stmt->stmt);
	  if (!useless_type_conversion_p (TREE_TYPE (root_lhs),
					  TREE_TYPE (vect_lhs)))
	    vect_lhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (root_lhs),
			       vect_lhs);
	  rstmt = gimple_build_assign (root_lhs, vect_lhs);
	  break;
	}
    }
  else if (SLP_TREE_NUMBER_OF_VEC_STMTS (node) > 1)
    {
      int nelts = SLP_TREE_NUMBER_OF_VEC_STMTS (node);
      gimple *child_stmt;
      int j;
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, nelts);

      FOR_EACH_VEC_ELT (SLP_TREE_VEC_STMTS (node), j, child_stmt)
	{
	  CONSTRUCTOR_APPEND_ELT (v,
				  NULL_TREE,
				  gimple_get_lhs (child_stmt));
	}
      tree lhs = gimple_get_lhs (instance->root_stmt->stmt);
      tree rtype = TREE_TYPE (gimple_assign_rhs1 (instance->root_stmt->stmt));
      tree r_constructor = build_constructor (rtype, v);
      rstmt = gimple_build_assign (lhs, r_constructor);
    }

    gcc_assert (rstmt);

    gimple_stmt_iterator rgsi = gsi_for_stmt (instance->root_stmt->stmt);
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
	  /* Simply fill all args.  */
	  for (unsigned i = 0; i < SLP_TREE_VEC_STMTS (phi_node).length (); ++i)
	    add_phi_arg (as_a <gphi *> (SLP_TREE_VEC_STMTS (phi_node)[i]),
			 vect_get_slp_vect_def (child, i),
			 e, gimple_phi_arg_location (phi, dest_idx));
	}
    }
}

/* Generate vector code for SLP_INSTANCES in the loop/basic block.  */

void
vect_schedule_slp (vec_info *vinfo, vec<slp_instance> slp_instances)
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
	  if (SLP_INSTANCE_ROOT_STMT (instance))
	    dump_printf_loc (MSG_NOTE, vect_location, "Root stmt: %G",
			 SLP_INSTANCE_ROOT_STMT (instance)->stmt);
	  vect_print_slp_graph (MSG_NOTE, vect_location,
				SLP_INSTANCE_TREE (instance));
	}
      /* Schedule the tree of INSTANCE, scheduling SCCs in a way to
	 have a PHI be the node breaking the cycle.  */
      auto_vec<slp_tree> stack;
      if (!scc_info.get (node))
	vect_schedule_scc (vinfo, node, instance, scc_info, maxdfs, stack);

      if (SLP_INSTANCE_ROOT_STMT (instance))
	vectorize_slp_instance_root_stmt (node, instance);

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
