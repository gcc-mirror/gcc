/* Lower vector operations to scalar operations.
   Copyright (C) 2004-2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

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
#include "tree-pass.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs-tree.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "langhooks.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-vector-builder.h"
#include "vec-perm-indices.h"
#include "insn-config.h"
#include "tree-ssa-dce.h"
#include "gimple-fold.h"
#include "gimple-match.h"
#include "recog.h"		/* FIXME: for insn_data */


/* Build a ternary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

static tree
gimplify_build3 (gimple_stmt_iterator *gsi, enum tree_code code,
		 tree type, tree a, tree b, tree c)
{
  location_t loc = gimple_location (gsi_stmt (*gsi));
  return gimple_build (gsi, true, GSI_SAME_STMT, loc, code, type, a, b, c);
}

/* Build a binary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

static tree
gimplify_build2 (gimple_stmt_iterator *gsi, enum tree_code code,
		 tree type, tree a, tree b)
{
  location_t loc = gimple_location (gsi_stmt (*gsi));
  return gimple_build (gsi, true, GSI_SAME_STMT, loc, code, type, a, b);
}

/* Build a unary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

static tree
gimplify_build1 (gimple_stmt_iterator *gsi, enum tree_code code, tree type,
		 tree a)
{
  location_t loc = gimple_location (gsi_stmt (*gsi));
  return gimple_build (gsi, true, GSI_SAME_STMT, loc, code, type, a);
}


static void expand_vector_operations_1 (gimple_stmt_iterator *, bitmap);

/* Return the number of elements in a vector type TYPE that we have
   already decided needs to be expanded piecewise.  We don't support
   this kind of expansion for variable-length vectors, since we should
   always check for target support before introducing uses of those.  */
static unsigned int
nunits_for_known_piecewise_op (const_tree type)
{
  return TYPE_VECTOR_SUBPARTS (type).to_constant ();
}

/* Return true if TYPE1 has more elements than TYPE2, where either
   type may be a vector or a scalar.  */

static inline bool
subparts_gt (tree type1, tree type2)
{
  poly_uint64 n1 = VECTOR_TYPE_P (type1) ? TYPE_VECTOR_SUBPARTS (type1) : 1;
  poly_uint64 n2 = VECTOR_TYPE_P (type2) ? TYPE_VECTOR_SUBPARTS (type2) : 1;
  return known_gt (n1, n2);
}

/* Build a constant of type TYPE, made of VALUE's bits replicated
   every WIDTH bits to fit TYPE's precision.  */
static tree
build_replicated_const (tree type, unsigned int width, HOST_WIDE_INT value)
{
  int n = (TYPE_PRECISION (type) + HOST_BITS_PER_WIDE_INT - 1) 
    / HOST_BITS_PER_WIDE_INT;
  unsigned HOST_WIDE_INT low, mask;
  HOST_WIDE_INT a[WIDE_INT_MAX_ELTS];
  int i;

  gcc_assert (n && n <= WIDE_INT_MAX_ELTS);

  if (width == HOST_BITS_PER_WIDE_INT)
    low = value;
  else
    {
      mask = ((HOST_WIDE_INT)1 << width) - 1;
      low = (unsigned HOST_WIDE_INT) ~0 / mask * (value & mask);
    }

  for (i = 0; i < n; i++)
    a[i] = low;

  gcc_assert (TYPE_PRECISION (type) <= MAX_BITSIZE_MODE_ANY_INT);
  return wide_int_to_tree
    (type, wide_int::from_array (a, n, TYPE_PRECISION (type)));
}

static GTY(()) tree vector_inner_type;
static GTY(()) tree vector_last_type;
static GTY(()) int vector_last_nunits;

/* Return a suitable vector types made of SUBPARTS units each of mode
   "word_mode" (the global variable).  */
static tree
build_word_mode_vector_type (int nunits)
{
  if (!vector_inner_type)
    vector_inner_type = lang_hooks.types.type_for_mode (word_mode, 1);
  else if (vector_last_nunits == nunits)
    {
      gcc_assert (TREE_CODE (vector_last_type) == VECTOR_TYPE);
      return vector_last_type;
    }

  vector_last_nunits = nunits;
  vector_last_type = build_vector_type (vector_inner_type, nunits);
  return vector_last_type;
}

typedef tree (*elem_op_func) (gimple_stmt_iterator *,
			      tree, tree, tree, tree, tree, enum tree_code,
			      tree);

/* Extract the vector element of type TYPE at BITPOS with BITSIZE from T
   and return it.  */

tree
tree_vec_extract (gimple_stmt_iterator *gsi, tree type,
		  tree t, tree bitsize, tree bitpos)
{
  /* We're using the resimplify API and maybe_push_res_to_seq to
     simplify the BIT_FIELD_REF but restrict the simplification to
     a single stmt while at the same time following SSA edges for
     simplification with already emitted CTORs.  */
  gimple_match_op opr;
  opr.set_op (BIT_FIELD_REF, type, t, bitsize, bitpos);
  opr.resimplify (NULL, follow_all_ssa_edges);
  gimple_seq stmts = NULL;
  tree res = maybe_push_res_to_seq (&opr, &stmts);
  gcc_assert (res);
  gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
  return res;
}

static tree
do_unop (gimple_stmt_iterator *gsi, tree inner_type, tree a,
	 tree b ATTRIBUTE_UNUSED, tree bitpos, tree bitsize,
	 enum tree_code code, tree type ATTRIBUTE_UNUSED)
{
  a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  return gimplify_build1 (gsi, code, inner_type, a);
}

static tree
do_binop (gimple_stmt_iterator *gsi, tree inner_type, tree a, tree b,
	  tree bitpos, tree bitsize, enum tree_code code,
	  tree type ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (TREE_TYPE (a)) == VECTOR_TYPE)
    a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  if (TREE_CODE (TREE_TYPE (b)) == VECTOR_TYPE)
    b = tree_vec_extract (gsi, inner_type, b, bitsize, bitpos);
  return gimplify_build2 (gsi, code, inner_type, a, b);
}

/* Construct expression (A[BITPOS] code B[BITPOS]) ? -1 : 0

   INNER_TYPE is the type of A and B elements

   returned expression is of signed integer type with the
   size equal to the size of INNER_TYPE.  */
static tree
do_compare (gimple_stmt_iterator *gsi, tree inner_type, tree a, tree b,
	    tree bitpos, tree bitsize, enum tree_code code, tree type)
{
  tree stype = TREE_TYPE (type);
  tree cst_false = build_zero_cst (stype);
  tree cst_true = build_all_ones_cst (stype);
  tree cmp;

  a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  b = tree_vec_extract (gsi, inner_type, b, bitsize, bitpos);

  cmp = build2 (code, boolean_type_node, a, b);
  return gimplify_build3 (gsi, COND_EXPR, stype, cmp, cst_true, cst_false);
}

/* Expand vector addition to scalars.  This does bit twiddling
   in order to increase parallelism:

   a + b = (((int) a & 0x7f7f7f7f) + ((int) b & 0x7f7f7f7f)) ^
           (a ^ b) & 0x80808080

   a - b =  (((int) a | 0x80808080) - ((int) b & 0x7f7f7f7f)) ^
            (a ^ ~b) & 0x80808080

   -b = (0x80808080 - ((int) b & 0x7f7f7f7f)) ^ (~b & 0x80808080)

   This optimization should be done only if 4 vector items or more
   fit into a word.  */
static tree
do_plus_minus (gimple_stmt_iterator *gsi, tree word_type, tree a, tree b,
	       tree bitpos ATTRIBUTE_UNUSED, tree bitsize ATTRIBUTE_UNUSED,
	       enum tree_code code, tree type ATTRIBUTE_UNUSED)
{
  unsigned int width = vector_element_bits (TREE_TYPE (a));
  tree inner_type = TREE_TYPE (TREE_TYPE (a));
  unsigned HOST_WIDE_INT max;
  tree low_bits, high_bits, a_low, b_low, result_low, signs;

  max = GET_MODE_MASK (TYPE_MODE (inner_type));
  low_bits = build_replicated_const (word_type, width, max >> 1);
  high_bits = build_replicated_const (word_type, width, max & ~(max >> 1));

  a = tree_vec_extract (gsi, word_type, a, bitsize, bitpos);
  b = tree_vec_extract (gsi, word_type, b, bitsize, bitpos);

  signs = gimplify_build2 (gsi, BIT_XOR_EXPR, word_type, a, b);
  b_low = gimplify_build2 (gsi, BIT_AND_EXPR, word_type, b, low_bits);
  if (code == PLUS_EXPR)
    a_low = gimplify_build2 (gsi, BIT_AND_EXPR, word_type, a, low_bits);
  else
    {
      a_low = gimplify_build2 (gsi, BIT_IOR_EXPR, word_type, a, high_bits);
      signs = gimplify_build1 (gsi, BIT_NOT_EXPR, word_type, signs);
    }

  signs = gimplify_build2 (gsi, BIT_AND_EXPR, word_type, signs, high_bits);
  result_low = gimplify_build2 (gsi, code, word_type, a_low, b_low);
  return gimplify_build2 (gsi, BIT_XOR_EXPR, word_type, result_low, signs);
}

static tree
do_negate (gimple_stmt_iterator *gsi, tree word_type, tree b,
	   tree unused ATTRIBUTE_UNUSED, tree bitpos ATTRIBUTE_UNUSED,
	   tree bitsize ATTRIBUTE_UNUSED,
	   enum tree_code code ATTRIBUTE_UNUSED,
	   tree type ATTRIBUTE_UNUSED)
{
  unsigned int width = vector_element_bits (TREE_TYPE (b));
  tree inner_type = TREE_TYPE (TREE_TYPE (b));
  HOST_WIDE_INT max;
  tree low_bits, high_bits, b_low, result_low, signs;

  max = GET_MODE_MASK (TYPE_MODE (inner_type));
  low_bits = build_replicated_const (word_type, width, max >> 1);
  high_bits = build_replicated_const (word_type, width, max & ~(max >> 1));

  b = tree_vec_extract (gsi, word_type, b, bitsize, bitpos);

  b_low = gimplify_build2 (gsi, BIT_AND_EXPR, word_type, b, low_bits);
  signs = gimplify_build1 (gsi, BIT_NOT_EXPR, word_type, b);
  signs = gimplify_build2 (gsi, BIT_AND_EXPR, word_type, signs, high_bits);
  result_low = gimplify_build2 (gsi, MINUS_EXPR, word_type, high_bits, b_low);
  return gimplify_build2 (gsi, BIT_XOR_EXPR, word_type, result_low, signs);
}

/* Expand a vector operation to scalars, by using many operations
   whose type is the vector type's inner type.  */
static tree
expand_vector_piecewise (gimple_stmt_iterator *gsi, elem_op_func f,
			 tree type, tree inner_type,
			 tree a, tree b, enum tree_code code,
			 bool parallel_p, tree ret_type = NULL_TREE)
{
  vec<constructor_elt, va_gc> *v;
  tree part_width = TYPE_SIZE (inner_type);
  tree index = bitsize_int (0);
  int nunits = nunits_for_known_piecewise_op (type);
  int delta = tree_to_uhwi (part_width) / vector_element_bits (type);
  int i;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  if (nunits == 1
      || warning_suppressed_p (gsi_stmt (*gsi),
			       OPT_Wvector_operation_performance))
    /* Do not diagnose decomposing single element vectors or when
       decomposing vectorizer produced operations.  */
    ;
  else if (ret_type || !parallel_p)
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector operation will be expanded piecewise");
  else
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector operation will be expanded in parallel");

  if (!ret_type)
    ret_type = type;
  vec_alloc (v, (nunits + delta - 1) / delta);
  bool constant_p = true;
  for (i = 0; i < nunits;
       i += delta, index = int_const_binop (PLUS_EXPR, index, part_width))
    {
      tree result = f (gsi, inner_type, a, b, index, part_width, code,
		       ret_type);
      if (!CONSTANT_CLASS_P (result))
	constant_p = false;
      constructor_elt ce = {NULL_TREE, result};
      v->quick_push (ce);
    }

  if (constant_p)
    return build_vector_from_ctor (ret_type, v);
  else
    return build_constructor (ret_type, v);
}

/* Expand a vector operation to scalars with the freedom to use
   a scalar integer type, or to use a different size for the items
   in the vector type.  */
static tree
expand_vector_parallel (gimple_stmt_iterator *gsi, elem_op_func f, tree type,
			tree a, tree b, enum tree_code code)
{
  tree result, compute_type;
  int n_words = tree_to_uhwi (TYPE_SIZE_UNIT (type)) / UNITS_PER_WORD;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  /* We have three strategies.  If the type is already correct, just do
     the operation an element at a time.  Else, if the vector is wider than
     one word, do it a word at a time; finally, if the vector is smaller
     than one word, do it as a scalar.  */
  if (TYPE_MODE (TREE_TYPE (type)) == word_mode)
     return expand_vector_piecewise (gsi, f,
				     type, TREE_TYPE (type),
				     a, b, code, true);
  else if (n_words > 1)
    {
      tree word_type = build_word_mode_vector_type (n_words);
      result = expand_vector_piecewise (gsi, f,
				        word_type, TREE_TYPE (word_type),
					a, b, code, true);
      result = force_gimple_operand_gsi (gsi, result, true, NULL, true,
                                         GSI_SAME_STMT);
    }
  else
    {
      /* Use a single scalar operation with a mode no wider than word_mode.  */
      if (!warning_suppressed_p (gsi_stmt (*gsi),
				 OPT_Wvector_operation_performance))
	warning_at (loc, OPT_Wvector_operation_performance,
		    "vector operation will be expanded with a "
		    "single scalar operation");
      scalar_int_mode mode
	= int_mode_for_size (tree_to_uhwi (TYPE_SIZE (type)), 0).require ();
      compute_type = lang_hooks.types.type_for_mode (mode, 1);
      result = f (gsi, compute_type, a, b, bitsize_zero_node,
		  TYPE_SIZE (compute_type), code, type);
    }

  return result;
}

/* Expand a vector operation to scalars; for integer types we can use
   special bit twiddling tricks to do the sums a word at a time, using
   function F_PARALLEL instead of F.  These tricks are done only if
   they can process at least four items, that is, only if the vector
   holds at least four items and if a word can hold four items.  */
static tree
expand_vector_addition (gimple_stmt_iterator *gsi,
			elem_op_func f, elem_op_func f_parallel,
			tree type, tree a, tree b, enum tree_code code)
{
  int parts_per_word = BITS_PER_WORD / vector_element_bits (type);

  if (INTEGRAL_TYPE_P (TREE_TYPE (type))
      && parts_per_word >= 4
      && nunits_for_known_piecewise_op (type) >= 4)
    return expand_vector_parallel (gsi, f_parallel,
				   type, a, b, code);
  else
    return expand_vector_piecewise (gsi, f,
				    type, TREE_TYPE (type),
				    a, b, code, false);
}

static bool
expand_vector_condition (gimple_stmt_iterator *gsi, bitmap dce_ssa_names);

/* Try to expand vector comparison expression OP0 CODE OP1 by
   querying optab if the following expression:
	VEC_COND_EXPR< OP0 CODE OP1, {-1,...}, {0,...}>
   can be expanded.  */
static tree
expand_vector_comparison (gimple_stmt_iterator *gsi, tree type, tree op0,
			  tree op1, enum tree_code code,
			  bitmap dce_ssa_names)
{
  tree lhs = gimple_assign_lhs (gsi_stmt (*gsi));
  use_operand_p use_p;
  imm_use_iterator iterator;
  bool vec_cond_expr_only = true;

  /* As seen in PR95830, we should not expand comparisons that are only
     feeding a VEC_COND_EXPR statement.  */
  auto_vec<gimple *> uses;
  FOR_EACH_IMM_USE_FAST (use_p, iterator, lhs)
    {
      gimple *use = USE_STMT (use_p);
      if (is_gimple_debug (use))
	continue;
      if (is_gimple_assign (use)
	  && gimple_assign_rhs_code (use) == VEC_COND_EXPR
	  && gimple_assign_rhs1 (use) == lhs
	  && gimple_assign_rhs2 (use) != lhs
	  && gimple_assign_rhs3 (use) != lhs)
	uses.safe_push (use);
      else
	vec_cond_expr_only = false;
    }

  if (vec_cond_expr_only)
    for (gimple *use : uses)
      {
	gimple_stmt_iterator it = gsi_for_stmt (use);
	if (!expand_vector_condition (&it, dce_ssa_names))
	  {
	    vec_cond_expr_only = false;
	    break;
	  }
      }

  if (!uses.is_empty () && vec_cond_expr_only)
    return NULL_TREE;

  tree t;
  if (!expand_vec_cmp_expr_p (TREE_TYPE (op0), type, code))
    {
      if (VECTOR_BOOLEAN_TYPE_P (type)
	  && SCALAR_INT_MODE_P (TYPE_MODE (type))
	  && known_lt (GET_MODE_BITSIZE (TYPE_MODE (type)),
		       TYPE_VECTOR_SUBPARTS (type)
		       * GET_MODE_BITSIZE (SCALAR_TYPE_MODE
						(TREE_TYPE (type)))))
	{
	  tree inner_type = TREE_TYPE (TREE_TYPE (op0));
	  tree part_width = vector_element_bits_tree (TREE_TYPE (op0));
	  tree index = bitsize_int (0);
	  int nunits = nunits_for_known_piecewise_op (TREE_TYPE (op0));
	  int prec = GET_MODE_PRECISION (SCALAR_TYPE_MODE (type));
	  tree ret_type = build_nonstandard_integer_type (prec, 1);
	  tree ret_inner_type = boolean_type_node;
	  int i;
	  location_t loc = gimple_location (gsi_stmt (*gsi));
	  t = build_zero_cst (ret_type);

	  if (TYPE_PRECISION (ret_inner_type) != 1)
	    ret_inner_type = build_nonstandard_integer_type (1, 1);
	  if (!warning_suppressed_p (gsi_stmt (*gsi),
				     OPT_Wvector_operation_performance))
	    warning_at (loc, OPT_Wvector_operation_performance,
			"vector operation will be expanded piecewise");
	  for (i = 0; i < nunits;
	       i++, index = int_const_binop (PLUS_EXPR, index, part_width))
	    {
	      tree a = tree_vec_extract (gsi, inner_type, op0, part_width,
					 index);
	      tree b = tree_vec_extract (gsi, inner_type, op1, part_width,
					 index);
	      tree result = gimplify_build2 (gsi, code, ret_inner_type, a, b);
	      t = gimplify_build3 (gsi, BIT_INSERT_EXPR, ret_type, t, result,
				   bitsize_int (i));
	    }
	  t = gimplify_build1 (gsi, VIEW_CONVERT_EXPR, type, t);
	}
      else
	t = expand_vector_piecewise (gsi, do_compare, type,
				     TREE_TYPE (TREE_TYPE (op0)), op0, op1,
				     code, false);
    }
  else
    t = NULL_TREE;

  return t;
}

/* Helper function of expand_vector_divmod.  Gimplify a RSHIFT_EXPR in type
   of OP0 with shift counts in SHIFTCNTS array and return the temporary holding
   the result if successful, otherwise return NULL_TREE.  */
static tree
add_rshift (gimple_stmt_iterator *gsi, tree type, tree op0, int *shiftcnts)
{
  optab op;
  unsigned int i, nunits = nunits_for_known_piecewise_op (type);
  bool scalar_shift = true;

  for (i = 1; i < nunits; i++)
    {
      if (shiftcnts[i] != shiftcnts[0])
	scalar_shift = false;
    }

  if (scalar_shift && shiftcnts[0] == 0)
    return op0;

  if (scalar_shift)
    {
      op = optab_for_tree_code (RSHIFT_EXPR, type, optab_scalar);
      if (op != unknown_optab
	  && optab_handler (op, TYPE_MODE (type)) != CODE_FOR_nothing)
	return gimplify_build2 (gsi, RSHIFT_EXPR, type, op0,
				build_int_cst (NULL_TREE, shiftcnts[0]));
    }

  op = optab_for_tree_code (RSHIFT_EXPR, type, optab_vector);
  if (op != unknown_optab
      && optab_handler (op, TYPE_MODE (type)) != CODE_FOR_nothing)
    {
      tree_vector_builder vec (type, nunits, 1);
      for (i = 0; i < nunits; i++)
	vec.quick_push (build_int_cst (TREE_TYPE (type), shiftcnts[i]));
      return gimplify_build2 (gsi, RSHIFT_EXPR, type, op0, vec.build ());
    }

  return NULL_TREE;
}

/* Try to expand integer vector division by constant using
   widening multiply, shifts and additions.  */
static tree
expand_vector_divmod (gimple_stmt_iterator *gsi, tree type, tree op0,
		      tree op1, enum tree_code code)
{
  bool use_pow2 = true;
  bool has_vector_shift = true;
  bool use_abs_op1 = false;
  int mode = -1, this_mode;
  int pre_shift = -1, post_shift;
  unsigned int nunits = nunits_for_known_piecewise_op (type);
  int *shifts = XALLOCAVEC (int, nunits * 4);
  int *pre_shifts = shifts + nunits;
  int *post_shifts = pre_shifts + nunits;
  int *shift_temps = post_shifts + nunits;
  unsigned HOST_WIDE_INT *mulc = XALLOCAVEC (unsigned HOST_WIDE_INT, nunits);
  int prec = TYPE_PRECISION (TREE_TYPE (type));
  int dummy_int;
  unsigned int i;
  signop sign_p = TYPE_SIGN (TREE_TYPE (type));
  unsigned HOST_WIDE_INT mask = GET_MODE_MASK (TYPE_MODE (TREE_TYPE (type)));
  tree cur_op, mulcst, tem;
  optab op;

  if (prec > HOST_BITS_PER_WIDE_INT)
    return NULL_TREE;

  op = optab_for_tree_code (RSHIFT_EXPR, type, optab_vector);
  if (op == unknown_optab
      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
    has_vector_shift = false;

  /* Analysis phase.  Determine if all op1 elements are either power
     of two and it is possible to expand it using shifts (or for remainder
     using masking).  Additionally compute the multiplicative constants
     and pre and post shifts if the division is to be expanded using
     widening or high part multiplication plus shifts.  */
  for (i = 0; i < nunits; i++)
    {
      tree cst = VECTOR_CST_ELT (op1, i);
      unsigned HOST_WIDE_INT ml;

      if (TREE_CODE (cst) != INTEGER_CST || integer_zerop (cst))
	return NULL_TREE;
      pre_shifts[i] = 0;
      post_shifts[i] = 0;
      mulc[i] = 0;
      if (use_pow2
	  && (!integer_pow2p (cst) || tree_int_cst_sgn (cst) != 1))
	use_pow2 = false;
      if (use_pow2)
	{
	  shifts[i] = tree_log2 (cst);
	  if (shifts[i] != shifts[0]
	      && code == TRUNC_DIV_EXPR
	      && !has_vector_shift)
	    use_pow2 = false;
	}
      if (mode == -2)
	continue;
      if (sign_p == UNSIGNED)
	{
	  unsigned HOST_WIDE_INT mh;
	  unsigned HOST_WIDE_INT d = TREE_INT_CST_LOW (cst) & mask;

	  if (d >= (HOST_WIDE_INT_1U << (prec - 1)))
	    /* FIXME: Can transform this into op0 >= op1 ? 1 : 0.  */
	    return NULL_TREE;

	  if (d <= 1)
	    {
	      mode = -2;
	      continue;
	    }

	  /* Find a suitable multiplier and right shift count
	     instead of multiplying with D.  */
	  mh = choose_multiplier (d, prec, prec, &ml, &post_shift, &dummy_int);

	  /* If the suggested multiplier is more than SIZE bits, we can
	     do better for even divisors, using an initial right shift.  */
	  if ((mh != 0 && (d & 1) == 0)
	      || (!has_vector_shift && pre_shift != -1))
	    {
	      if (has_vector_shift)
		pre_shift = ctz_or_zero (d);
	      else if (pre_shift == -1)
		{
		  unsigned int j;
		  for (j = 0; j < nunits; j++)
		    {
		      tree cst2 = VECTOR_CST_ELT (op1, j);
		      unsigned HOST_WIDE_INT d2;
		      int this_pre_shift;

		      if (!tree_fits_uhwi_p (cst2))
			return NULL_TREE;
		      d2 = tree_to_uhwi (cst2) & mask;
		      if (d2 == 0)
			return NULL_TREE;
		      this_pre_shift = floor_log2 (d2 & -d2);
		      if (pre_shift == -1 || this_pre_shift < pre_shift)
			pre_shift = this_pre_shift;
		    }
		  if (i != 0 && pre_shift != 0)
		    {
		      /* Restart.  */
		      i = -1U;
		      mode = -1;
		      continue;
		    }
		}
	      if (pre_shift != 0)
		{
		  if ((d >> pre_shift) <= 1)
		    {
		      mode = -2;
		      continue;
		    }
		  mh = choose_multiplier (d >> pre_shift, prec,
					  prec - pre_shift,
					  &ml, &post_shift, &dummy_int);
		  gcc_assert (!mh);
		  pre_shifts[i] = pre_shift;
		}
	    }
	  if (!mh)
	    this_mode = 0;
	  else
	    this_mode = 1;
	}
      else
	{
	  HOST_WIDE_INT d = TREE_INT_CST_LOW (cst);
	  unsigned HOST_WIDE_INT abs_d;

	  if (d == -1)
	    return NULL_TREE;

	  /* Since d might be INT_MIN, we have to cast to
	     unsigned HOST_WIDE_INT before negating to avoid
	     undefined signed overflow.  */
	  abs_d = (d >= 0
		  ? (unsigned HOST_WIDE_INT) d
		  : - (unsigned HOST_WIDE_INT) d);

	  /* n rem d = n rem -d */
	  if (code == TRUNC_MOD_EXPR && d < 0)
	    {
	      d = abs_d;
	      use_abs_op1 = true;
	    }
	  if (abs_d == HOST_WIDE_INT_1U << (prec - 1))
	    {
	      /* This case is not handled correctly below.  */
	      mode = -2;
	      continue;
	    }
	  if (abs_d <= 1)
	    {
	      mode = -2;
	      continue;
	    }

	  choose_multiplier (abs_d, prec, prec - 1, &ml,
			     &post_shift, &dummy_int);
	  if (ml >= HOST_WIDE_INT_1U << (prec - 1))
	    {
	      this_mode = 4 + (d < 0);
	      ml |= HOST_WIDE_INT_M1U << (prec - 1);
	    }
	  else
	    this_mode = 2 + (d < 0);
	}
      mulc[i] = ml;
      post_shifts[i] = post_shift;
      if ((i && !has_vector_shift && post_shifts[0] != post_shift)
	  || post_shift >= prec
	  || pre_shifts[i] >= prec)
	this_mode = -2;

      if (i == 0)
	mode = this_mode;
      else if (mode != this_mode)
	mode = -2;
    }

  if (use_pow2)
    {
      tree addend = NULL_TREE;
      if (sign_p == SIGNED)
	{
	  tree uns_type;

	  /* Both division and remainder sequences need
	     op0 < 0 ? mask : 0 computed.  It can be either computed as
	     (type) (((uns_type) (op0 >> (prec - 1))) >> (prec - shifts[i]))
	     if none of the shifts is 0, or as the conditional.  */
	  for (i = 0; i < nunits; i++)
	    if (shifts[i] == 0)
	      break;
	  uns_type
	    = build_vector_type (build_nonstandard_integer_type (prec, 1),
				 nunits);
	  if (i == nunits && TYPE_MODE (uns_type) == TYPE_MODE (type))
	    {
	      for (i = 0; i < nunits; i++)
		shift_temps[i] = prec - 1;
	      cur_op = add_rshift (gsi, type, op0, shift_temps);
	      if (cur_op != NULL_TREE)
		{
		  cur_op = gimplify_build1 (gsi, VIEW_CONVERT_EXPR,
					    uns_type, cur_op);
		  for (i = 0; i < nunits; i++)
		    shift_temps[i] = prec - shifts[i];
		  cur_op = add_rshift (gsi, uns_type, cur_op, shift_temps);
		  if (cur_op != NULL_TREE)
		    addend = gimplify_build1 (gsi, VIEW_CONVERT_EXPR,
					      type, cur_op);
		}
	    }
	  if (addend == NULL_TREE
	      && expand_vec_cond_expr_p (type, type, LT_EXPR))
	    {
	      tree zero, cst, mask_type, mask;
	      gimple *stmt, *cond;

	      mask_type = truth_type_for (type);
	      zero = build_zero_cst (type);
	      mask = make_ssa_name (mask_type);
	      cond = gimple_build_assign (mask, LT_EXPR, op0, zero);
	      gsi_insert_before (gsi, cond, GSI_SAME_STMT);
	      tree_vector_builder vec (type, nunits, 1);
	      for (i = 0; i < nunits; i++)
		vec.quick_push (build_int_cst (TREE_TYPE (type),
					       (HOST_WIDE_INT_1U
						<< shifts[i]) - 1));
	      cst = vec.build ();
	      addend = make_ssa_name (type);
	      stmt
		= gimple_build_assign (addend, VEC_COND_EXPR, mask, cst, zero);
	      gsi_insert_before (gsi, stmt, GSI_SAME_STMT);
	    }
	}
      if (code == TRUNC_DIV_EXPR)
	{
	  if (sign_p == UNSIGNED)
	    {
	      /* q = op0 >> shift;  */
	      cur_op = add_rshift (gsi, type, op0, shifts);
	      if (cur_op != NULL_TREE)
		return cur_op;
	    }
	  else if (addend != NULL_TREE)
	    {
	      /* t1 = op0 + addend;
		 q = t1 >> shift;  */
	      op = optab_for_tree_code (PLUS_EXPR, type, optab_default);
	      if (op != unknown_optab
		  && optab_handler (op, TYPE_MODE (type)) != CODE_FOR_nothing)
		{
		  cur_op = gimplify_build2 (gsi, PLUS_EXPR, type, op0, addend);
		  cur_op = add_rshift (gsi, type, cur_op, shifts);
		  if (cur_op != NULL_TREE)
		    return cur_op;
		}
	    }
	}
      else
	{
	  tree mask;
	  tree_vector_builder vec (type, nunits, 1);
	  for (i = 0; i < nunits; i++)
	    vec.quick_push (build_int_cst (TREE_TYPE (type),
					   (HOST_WIDE_INT_1U
					    << shifts[i]) - 1));
	  mask = vec.build ();
	  op = optab_for_tree_code (BIT_AND_EXPR, type, optab_default);
	  if (op != unknown_optab
	      && optab_handler (op, TYPE_MODE (type)) != CODE_FOR_nothing)
	    {
	      if (sign_p == UNSIGNED)
		/* r = op0 & mask;  */
		return gimplify_build2 (gsi, BIT_AND_EXPR, type, op0, mask);
	      else if (addend != NULL_TREE)
		{
		  /* t1 = op0 + addend;
		     t2 = t1 & mask;
		     r = t2 - addend;  */
		  op = optab_for_tree_code (PLUS_EXPR, type, optab_default);
		  if (op != unknown_optab
		      && optab_handler (op, TYPE_MODE (type))
			 != CODE_FOR_nothing)
		    {
		      cur_op = gimplify_build2 (gsi, PLUS_EXPR, type, op0,
						addend);
		      cur_op = gimplify_build2 (gsi, BIT_AND_EXPR, type,
						cur_op, mask);
		      op = optab_for_tree_code (MINUS_EXPR, type,
						optab_default);
		      if (op != unknown_optab
			  && optab_handler (op, TYPE_MODE (type))
			     != CODE_FOR_nothing)
			return gimplify_build2 (gsi, MINUS_EXPR, type,
						cur_op, addend);
		    }
		}
	    }
	}
    }

  if (mode == -2 || BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    return NULL_TREE;

  if (!can_mult_highpart_p (TYPE_MODE (type), TYPE_UNSIGNED (type)))
    return NULL_TREE;

  cur_op = op0;

  switch (mode)
    {
    case 0:
      gcc_assert (sign_p == UNSIGNED);
      /* t1 = oprnd0 >> pre_shift;
	 t2 = t1 h* ml;
	 q = t2 >> post_shift;  */
      cur_op = add_rshift (gsi, type, cur_op, pre_shifts);
      if (cur_op == NULL_TREE)
	return NULL_TREE;
      break;
    case 1:
      gcc_assert (sign_p == UNSIGNED);
      for (i = 0; i < nunits; i++)
	{
	  shift_temps[i] = 1;
	  post_shifts[i]--;
	}
      break;
    case 2:
    case 3:
    case 4:
    case 5:
      gcc_assert (sign_p == SIGNED);
      for (i = 0; i < nunits; i++)
	shift_temps[i] = prec - 1;
      break;
    default:
      return NULL_TREE;
    }

  tree_vector_builder vec (type, nunits, 1);
  for (i = 0; i < nunits; i++)
    vec.quick_push (build_int_cst (TREE_TYPE (type), mulc[i]));
  mulcst = vec.build ();

  cur_op = gimplify_build2 (gsi, MULT_HIGHPART_EXPR, type, cur_op, mulcst);

  switch (mode)
    {
    case 0:
      /* t1 = oprnd0 >> pre_shift;
	 t2 = t1 h* ml;
	 q = t2 >> post_shift;  */
      cur_op = add_rshift (gsi, type, cur_op, post_shifts);
      break;
    case 1:
      /* t1 = oprnd0 h* ml;
	 t2 = oprnd0 - t1;
	 t3 = t2 >> 1;
	 t4 = t1 + t3;
	 q = t4 >> (post_shift - 1);  */
      op = optab_for_tree_code (MINUS_EXPR, type, optab_default);
      if (op == unknown_optab
	  || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
	return NULL_TREE;
      tem = gimplify_build2 (gsi, MINUS_EXPR, type, op0, cur_op);
      tem = add_rshift (gsi, type, tem, shift_temps);
      op = optab_for_tree_code (PLUS_EXPR, type, optab_default);
      if (op == unknown_optab
	  || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
	return NULL_TREE;
      tem = gimplify_build2 (gsi, PLUS_EXPR, type, cur_op, tem);
      cur_op = add_rshift (gsi, type, tem, post_shifts);
      if (cur_op == NULL_TREE)
	return NULL_TREE;
      break;
    case 2:
    case 3:
    case 4:
    case 5:
      /* t1 = oprnd0 h* ml;
	 t2 = t1; [ iff (mode & 2) != 0 ]
	 t2 = t1 + oprnd0; [ iff (mode & 2) == 0 ]
	 t3 = t2 >> post_shift;
	 t4 = oprnd0 >> (prec - 1);
	 q = t3 - t4; [ iff (mode & 1) == 0 ]
	 q = t4 - t3; [ iff (mode & 1) != 0 ]  */
      if ((mode & 2) == 0)
	{
	  op = optab_for_tree_code (PLUS_EXPR, type, optab_default);
	  if (op == unknown_optab
	      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
	    return NULL_TREE;
	  cur_op = gimplify_build2 (gsi, PLUS_EXPR, type, cur_op, op0);
	}
      cur_op = add_rshift (gsi, type, cur_op, post_shifts);
      if (cur_op == NULL_TREE)
	return NULL_TREE;
      tem = add_rshift (gsi, type, op0, shift_temps);
      if (tem == NULL_TREE)
	return NULL_TREE;
      op = optab_for_tree_code (MINUS_EXPR, type, optab_default);
      if (op == unknown_optab
	  || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
	return NULL_TREE;
      if ((mode & 1) == 0)
	cur_op = gimplify_build2 (gsi, MINUS_EXPR, type, cur_op, tem);
      else
	cur_op = gimplify_build2 (gsi, MINUS_EXPR, type, tem, cur_op);
      break;
    default:
      gcc_unreachable ();
    }

  if (code == TRUNC_DIV_EXPR)
    return cur_op;

  /* We divided.  Now finish by:
     t1 = q * oprnd1;
     r = oprnd0 - t1;  */
  op = optab_for_tree_code (MULT_EXPR, type, optab_default);
  if (op == unknown_optab
      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
    return NULL_TREE;
  if (use_abs_op1)
    {
      tree_vector_builder elts;
      if (!elts.new_unary_operation (type, op1, false))
	return NULL_TREE;
      unsigned int count = elts.encoded_nelts ();
      for (unsigned int i = 0; i < count; ++i)
	{
	  tree elem1 = VECTOR_CST_ELT (op1, i);

	  tree elt = const_unop (ABS_EXPR, TREE_TYPE (elem1), elem1);
	  if (elt == NULL_TREE)
	    return NULL_TREE;
	  elts.quick_push (elt);
	}
      op1 = elts.build ();
    }
  tem = gimplify_build2 (gsi, MULT_EXPR, type, cur_op, op1);
  op = optab_for_tree_code (MINUS_EXPR, type, optab_default);
  if (op == unknown_optab
      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
    return NULL_TREE;
  return gimplify_build2 (gsi, MINUS_EXPR, type, op0, tem);
}

/* Expand a vector condition to scalars, by using many conditions
   on the vector's elements.  */

static bool
expand_vector_condition (gimple_stmt_iterator *gsi, bitmap dce_ssa_names)
{
  gassign *stmt = as_a <gassign *> (gsi_stmt (*gsi));
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  tree a = gimple_assign_rhs1 (stmt);
  tree a1 = a;
  tree a2 = NULL_TREE;
  bool a_is_comparison = false;
  bool a_is_scalar_bitmask = false;
  tree b = gimple_assign_rhs2 (stmt);
  tree c = gimple_assign_rhs3 (stmt);
  vec<constructor_elt, va_gc> *v;
  tree constr;
  tree inner_type = TREE_TYPE (type);
  tree width = vector_element_bits_tree (type);
  tree cond_type = TREE_TYPE (TREE_TYPE (a));
  tree comp_inner_type = cond_type;
  tree index = bitsize_int (0);
  tree comp_width = width;
  tree comp_index = index;
  location_t loc = gimple_location (gsi_stmt (*gsi));
  tree_code code = TREE_CODE (a);
  gassign *assign = NULL;

  if (code == SSA_NAME)
    {
      assign = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (a));
      if (assign != NULL
	  && TREE_CODE_CLASS (gimple_assign_rhs_code (assign)) == tcc_comparison)
	{
	  a_is_comparison = true;
	  a1 = gimple_assign_rhs1 (assign);
	  a2 = gimple_assign_rhs2 (assign);
	  code = gimple_assign_rhs_code (assign);
	  comp_inner_type = TREE_TYPE (TREE_TYPE (a1));
	  comp_width = vector_element_bits_tree (TREE_TYPE (a1));
	}
    }

  if (expand_vec_cond_expr_p (type, TREE_TYPE (a1), code)
      || (integer_all_onesp (b) && integer_zerop (c)
	  && expand_vec_cmp_expr_p (type, TREE_TYPE (a1), code)))
    {
      gcc_assert (TREE_CODE (a) == SSA_NAME || TREE_CODE (a) == VECTOR_CST);
      return true;
    }

  /* Handle vector boolean types with bitmasks.  If there is a comparison
     and we can expand the comparison into the vector boolean bitmask,
     or otherwise if it is compatible with type, we can transform
      vbfld_1 = x_2 < y_3 ? vbfld_4 : vbfld_5;
     into
      tmp_6 = x_2 < y_3;
      tmp_7 = tmp_6 & vbfld_4;
      tmp_8 = ~tmp_6;
      tmp_9 = tmp_8 & vbfld_5;
      vbfld_1 = tmp_7 | tmp_9;
     Similarly for vbfld_10 instead of x_2 < y_3.  */
  if (VECTOR_BOOLEAN_TYPE_P (type)
      && SCALAR_INT_MODE_P (TYPE_MODE (type))
      && known_lt (GET_MODE_BITSIZE (TYPE_MODE (type)),
		   TYPE_VECTOR_SUBPARTS (type)
		   * GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (type))))
      && (a_is_comparison
	  ? useless_type_conversion_p (type, TREE_TYPE (a))
	  : expand_vec_cmp_expr_p (TREE_TYPE (a1), type, TREE_CODE (a))))
    {
      if (a_is_comparison)
	a = gimplify_build2 (gsi, code, type, a1, a2);
      a1 = gimplify_build2 (gsi, BIT_AND_EXPR, type, a, b);
      a2 = gimplify_build1 (gsi, BIT_NOT_EXPR, type, a);
      a2 = gimplify_build2 (gsi, BIT_AND_EXPR, type, a2, c);
      a = gimplify_build2 (gsi, BIT_IOR_EXPR, type, a1, a2);
      gimple_assign_set_rhs_from_tree (gsi, a);
      update_stmt (gsi_stmt (*gsi));
      return true;
    }

  /* TODO: try and find a smaller vector type.  */

  if (!warning_suppressed_p (stmt, OPT_Wvector_operation_performance))
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector condition will be expanded piecewise");

  if (!a_is_comparison
      && VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (a))
      && SCALAR_INT_MODE_P (TYPE_MODE (TREE_TYPE (a)))
      && known_lt (GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (a))),
		   TYPE_VECTOR_SUBPARTS (TREE_TYPE (a))
		   * GET_MODE_BITSIZE (SCALAR_TYPE_MODE
						(TREE_TYPE (TREE_TYPE (a))))))
    {
      a_is_scalar_bitmask = true;
      int prec = GET_MODE_PRECISION (SCALAR_TYPE_MODE (TREE_TYPE (a)));
      tree atype = build_nonstandard_integer_type (prec, 1);
      a = gimplify_build1 (gsi, VIEW_CONVERT_EXPR, atype, a);
    }
  else if (!a_is_comparison
	   && VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (a)))
    comp_width = vector_element_bits_tree (TREE_TYPE (a));

  int nunits = nunits_for_known_piecewise_op (type);
  vec_alloc (v, nunits);
  bool constant_p = true;
  for (int i = 0; i < nunits; i++)
    {
      tree aa, result;
      tree bb = tree_vec_extract (gsi, inner_type, b, width, index);
      tree cc = tree_vec_extract (gsi, inner_type, c, width, index);
      if (a_is_comparison)
	{
	  tree aa1 = tree_vec_extract (gsi, comp_inner_type, a1,
				       comp_width, comp_index);
	  tree aa2 = tree_vec_extract (gsi, comp_inner_type, a2,
				       comp_width, comp_index);
	  aa = gimplify_build2 (gsi, code, cond_type, aa1, aa2);
	}
      else if (a_is_scalar_bitmask)
	{
	  wide_int w = wi::set_bit_in_zero (i, TYPE_PRECISION (TREE_TYPE (a)));
	  result = gimplify_build2 (gsi, BIT_AND_EXPR, TREE_TYPE (a),
				    a, wide_int_to_tree (TREE_TYPE (a), w));
	  aa = gimplify_build2 (gsi, NE_EXPR, boolean_type_node, result,
				build_zero_cst (TREE_TYPE (a)));
	}
      else
	aa = tree_vec_extract (gsi, cond_type, a, comp_width, comp_index);
      result = gimplify_build3 (gsi, COND_EXPR, inner_type, aa, bb, cc);
      if (!CONSTANT_CLASS_P (result))
	constant_p = false;
      constructor_elt ce = {NULL_TREE, result};
      v->quick_push (ce);
      index = int_const_binop (PLUS_EXPR, index, width);
      if (width == comp_width)
	comp_index = index;
      else
	comp_index = int_const_binop (PLUS_EXPR, comp_index, comp_width);
    }

  if (constant_p)
    constr = build_vector_from_ctor (type, v);
  else
    constr = build_constructor (type, v);
  gimple_assign_set_rhs_from_tree (gsi, constr);
  update_stmt (gsi_stmt (*gsi));

  if (a_is_comparison)
    bitmap_set_bit (dce_ssa_names,
		    SSA_NAME_VERSION (gimple_assign_lhs (assign)));

  return false;
}

static tree
expand_vector_operation (gimple_stmt_iterator *gsi, tree type, tree compute_type,
			 gassign *assign, enum tree_code code,
			 bitmap dce_ssa_names)
{
  machine_mode compute_mode = TYPE_MODE (compute_type);

  /* If the compute mode is not a vector mode (hence we are not decomposing
     a BLKmode vector to smaller, hardware-supported vectors), we may want
     to expand the operations in parallel.  */
  if (!VECTOR_MODE_P (compute_mode))
    switch (code)
      {
      case PLUS_EXPR:
      case MINUS_EXPR:
        if (ANY_INTEGRAL_TYPE_P (type) && !TYPE_OVERFLOW_TRAPS (type))
	  return expand_vector_addition (gsi, do_binop, do_plus_minus, type,
					 gimple_assign_rhs1 (assign),
					 gimple_assign_rhs2 (assign), code);
	break;

      case NEGATE_EXPR:
        if (ANY_INTEGRAL_TYPE_P (type) && !TYPE_OVERFLOW_TRAPS (type))
          return expand_vector_addition (gsi, do_unop, do_negate, type,
		      		         gimple_assign_rhs1 (assign),
					 NULL_TREE, code);
	break;

      case BIT_AND_EXPR:
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
        return expand_vector_parallel (gsi, do_binop, type,
		      		       gimple_assign_rhs1 (assign),
				       gimple_assign_rhs2 (assign), code);

      case BIT_NOT_EXPR:
        return expand_vector_parallel (gsi, do_unop, type,
		      		       gimple_assign_rhs1 (assign),
        			       NULL_TREE, code);
      case EQ_EXPR:
      case NE_EXPR:
      case GT_EXPR:
      case LT_EXPR:
      case GE_EXPR:
      case LE_EXPR:
      case UNEQ_EXPR:
      case UNGT_EXPR:
      case UNLT_EXPR:
      case UNGE_EXPR:
      case UNLE_EXPR:
      case LTGT_EXPR:
      case ORDERED_EXPR:
      case UNORDERED_EXPR:
	{
	  tree rhs1 = gimple_assign_rhs1 (assign);
	  tree rhs2 = gimple_assign_rhs2 (assign);

	  return expand_vector_comparison (gsi, type, rhs1, rhs2, code,
					   dce_ssa_names);
	}

      case TRUNC_DIV_EXPR:
      case TRUNC_MOD_EXPR:
	{
	  tree rhs1 = gimple_assign_rhs1 (assign);
	  tree rhs2 = gimple_assign_rhs2 (assign);
	  tree ret;

	  /* Check if the target was going to handle it through the special
	     division callback hook.  */
	  tree cst = uniform_integer_cst_p (rhs2);
	  if (cst &&
	      targetm.vectorize.can_special_div_by_const (code, type,
							  wi::to_wide (cst),
							  NULL,
							  NULL_RTX, NULL_RTX))
	    return NULL_TREE;


	  if (!optimize
	      || !VECTOR_INTEGER_TYPE_P (type)
	      || TREE_CODE (rhs2) != VECTOR_CST
	      || !VECTOR_MODE_P (TYPE_MODE (type)))
	    break;

	  ret = expand_vector_divmod (gsi, type, rhs1, rhs2, code);
	  if (ret != NULL_TREE)
	    return ret;
	  break;
	}

      default:
	break;
      }

  if (TREE_CODE_CLASS (code) == tcc_unary)
    return expand_vector_piecewise (gsi, do_unop, type, compute_type,
				    gimple_assign_rhs1 (assign),
				    NULL_TREE, code, false);
  else
    return expand_vector_piecewise (gsi, do_binop, type, compute_type,
				    gimple_assign_rhs1 (assign),
				    gimple_assign_rhs2 (assign), code, false);
}

/* Try to optimize
   a_5 = { b_7, b_7 + 3, b_7 + 6, b_7 + 9 };
   style stmts into:
   _9 = { b_7, b_7, b_7, b_7 };
   a_5 = _9 + { 0, 3, 6, 9 };
   because vector splat operation is usually more efficient
   than piecewise initialization of the vector.  */

static void
optimize_vector_constructor (gimple_stmt_iterator *gsi)
{
  gassign *stmt = as_a <gassign *> (gsi_stmt (*gsi));
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);
  tree type = TREE_TYPE (rhs);
  unsigned int i, j;
  unsigned HOST_WIDE_INT nelts;
  bool all_same = true;
  constructor_elt *elt;
  gimple *g;
  tree base = NULL_TREE;
  optab op;

  if (!TYPE_VECTOR_SUBPARTS (type).is_constant (&nelts)
      || nelts <= 2
      || CONSTRUCTOR_NELTS (rhs) != nelts)
    return;
  op = optab_for_tree_code (PLUS_EXPR, type, optab_default);
  if (op == unknown_optab
      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
    return;
  FOR_EACH_VEC_SAFE_ELT (CONSTRUCTOR_ELTS (rhs), i, elt)
    if (TREE_CODE (elt->value) != SSA_NAME
	|| TREE_CODE (TREE_TYPE (elt->value)) == VECTOR_TYPE)
      return;
    else
      {
	tree this_base = elt->value;
	if (this_base != CONSTRUCTOR_ELT (rhs, 0)->value)
	  all_same = false;
	for (j = 0; j < nelts + 1; j++)
	  {
	    g = SSA_NAME_DEF_STMT (this_base);
	    if (is_gimple_assign (g)
		&& gimple_assign_rhs_code (g) == PLUS_EXPR
		&& TREE_CODE (gimple_assign_rhs2 (g)) == INTEGER_CST
		&& TREE_CODE (gimple_assign_rhs1 (g)) == SSA_NAME
		&& !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_assign_rhs1 (g)))
	      this_base = gimple_assign_rhs1 (g);
	    else
	      break;
	  }
	if (i == 0)
	  base = this_base;
	else if (this_base != base)
	  return;
      }
  if (all_same)
    return;
  tree_vector_builder cst (type, nelts, 1);
  for (i = 0; i < nelts; i++)
    {
      tree this_base = CONSTRUCTOR_ELT (rhs, i)->value;
      tree elt = build_zero_cst (TREE_TYPE (base));
      while (this_base != base)
	{
	  g = SSA_NAME_DEF_STMT (this_base);
	  elt = fold_binary (PLUS_EXPR, TREE_TYPE (base),
			     elt, gimple_assign_rhs2 (g));
	  if (elt == NULL_TREE
	      || TREE_CODE (elt) != INTEGER_CST
	      || TREE_OVERFLOW (elt))
	    return;
	  this_base = gimple_assign_rhs1 (g);
	}
      cst.quick_push (elt);
    }
  for (i = 0; i < nelts; i++)
    CONSTRUCTOR_ELT (rhs, i)->value = base;
  g = gimple_build_assign (make_ssa_name (type), rhs);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  g = gimple_build_assign (lhs, PLUS_EXPR, gimple_assign_lhs (g),
			   cst.build ());
  gsi_replace (gsi, g, false);
}

/* Return a type for the widest vector mode whose components are of type
   TYPE, or NULL_TREE if none is found.  */

static tree
type_for_widest_vector_mode (tree type, optab op)
{
  machine_mode inner_mode = TYPE_MODE (type);
  machine_mode best_mode = VOIDmode, mode;
  poly_int64 best_nunits = 0;

  if (SCALAR_FLOAT_MODE_P (inner_mode))
    mode = MIN_MODE_VECTOR_FLOAT;
  else if (SCALAR_FRACT_MODE_P (inner_mode))
    mode = MIN_MODE_VECTOR_FRACT;
  else if (SCALAR_UFRACT_MODE_P (inner_mode))
    mode = MIN_MODE_VECTOR_UFRACT;
  else if (SCALAR_ACCUM_MODE_P (inner_mode))
    mode = MIN_MODE_VECTOR_ACCUM;
  else if (SCALAR_UACCUM_MODE_P (inner_mode))
    mode = MIN_MODE_VECTOR_UACCUM;
  else if (inner_mode == BImode)
    mode = MIN_MODE_VECTOR_BOOL;
  else
    mode = MIN_MODE_VECTOR_INT;

  FOR_EACH_MODE_FROM (mode, mode)
    if (GET_MODE_INNER (mode) == inner_mode
	&& maybe_gt (GET_MODE_NUNITS (mode), best_nunits)
	&& optab_handler (op, mode) != CODE_FOR_nothing)
      best_mode = mode, best_nunits = GET_MODE_NUNITS (mode);

  if (best_mode == VOIDmode)
    return NULL_TREE;
  else
    return build_vector_type_for_mode (type, best_mode);
}


/* Build a reference to the element of the vector VECT.  Function
   returns either the element itself, either BIT_FIELD_REF, or an
   ARRAY_REF expression.

   GSI is required to insert temporary variables while building a
   refernece to the element of the vector VECT.

   PTMPVEC is a pointer to the temporary variable for caching
   purposes.  In case when PTMPVEC is NULL new temporary variable
   will be created.  */
static tree
vector_element (gimple_stmt_iterator *gsi, tree vect, tree idx, tree *ptmpvec)
{
  tree vect_type, vect_elt_type;
  gimple *asgn;
  tree tmpvec;
  tree arraytype;
  bool need_asgn = true;
  unsigned int elements;

  vect_type = TREE_TYPE (vect);
  vect_elt_type = TREE_TYPE (vect_type);
  elements = nunits_for_known_piecewise_op (vect_type);

  if (TREE_CODE (idx) == INTEGER_CST)
    {
      unsigned HOST_WIDE_INT index;

      /* Given that we're about to compute a binary modulus,
	 we don't care about the high bits of the value.  */
      index = TREE_INT_CST_LOW (idx);
      if (!tree_fits_uhwi_p (idx) || index >= elements)
	{
	  index &= elements - 1;
	  idx = build_int_cst (TREE_TYPE (idx), index);
	}

      /* When lowering a vector statement sequence do some easy
         simplification by looking through intermediate vector results.  */
      if (TREE_CODE (vect) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (vect);
	  if (is_gimple_assign (def_stmt)
	      && (gimple_assign_rhs_code (def_stmt) == VECTOR_CST
		  || gimple_assign_rhs_code (def_stmt) == CONSTRUCTOR))
	    vect = gimple_assign_rhs1 (def_stmt);
	}

      if (TREE_CODE (vect) == VECTOR_CST)
	return VECTOR_CST_ELT (vect, index);
      else if (TREE_CODE (vect) == CONSTRUCTOR
	       && (CONSTRUCTOR_NELTS (vect) == 0
		   || TREE_CODE (TREE_TYPE (CONSTRUCTOR_ELT (vect, 0)->value))
		      != VECTOR_TYPE))
        {
	  if (index < CONSTRUCTOR_NELTS (vect))
	    return CONSTRUCTOR_ELT (vect, index)->value;
          return build_zero_cst (vect_elt_type);
        }
      else
        {
	  tree size = vector_element_bits_tree (vect_type);
	  tree pos = fold_build2 (MULT_EXPR, bitsizetype, bitsize_int (index),
				  size);
	  return fold_build3 (BIT_FIELD_REF, vect_elt_type, vect, size, pos);
        }
    }

  if (!ptmpvec)
    tmpvec = create_tmp_var (vect_type, "vectmp");
  else if (!*ptmpvec)
    tmpvec = *ptmpvec = create_tmp_var (vect_type, "vectmp");
  else
    {
      tmpvec = *ptmpvec;
      need_asgn = false;
    }

  if (need_asgn)
    {
      TREE_ADDRESSABLE (tmpvec) = 1;
      asgn = gimple_build_assign (tmpvec, vect);
      gsi_insert_before (gsi, asgn, GSI_SAME_STMT);
    }

  arraytype = build_array_type_nelts (vect_elt_type, elements);
  return build4 (ARRAY_REF, vect_elt_type,
                 build1 (VIEW_CONVERT_EXPR, arraytype, tmpvec),
                 idx, NULL_TREE, NULL_TREE);
}

/* Check if VEC_PERM_EXPR within the given setting is supported
   by hardware, or lower it piecewise.

   When VEC_PERM_EXPR has the same first and second operands:
   VEC_PERM_EXPR <v0, v0, mask> the lowered version would be
   {v0[mask[0]], v0[mask[1]], ...}
   MASK and V0 must have the same number of elements.

   Otherwise VEC_PERM_EXPR <v0, v1, mask> is lowered to
   {mask[0] < len(v0) ? v0[mask[0]] : v1[mask[0]], ...}
   V0 and V1 must have the same type.  MASK, V0, V1 must have the
   same number of arguments.  */

static void
lower_vec_perm (gimple_stmt_iterator *gsi)
{
  gassign *stmt = as_a <gassign *> (gsi_stmt (*gsi));
  tree mask = gimple_assign_rhs3 (stmt);
  tree vec0 = gimple_assign_rhs1 (stmt);
  tree vec1 = gimple_assign_rhs2 (stmt);
  tree vect_type = TREE_TYPE (vec0);
  tree mask_type = TREE_TYPE (mask);
  tree vect_elt_type = TREE_TYPE (vect_type);
  tree mask_elt_type = TREE_TYPE (mask_type);
  unsigned HOST_WIDE_INT elements;
  vec<constructor_elt, va_gc> *v;
  tree constr, t, si, i_val;
  tree vec0tmp = NULL_TREE, vec1tmp = NULL_TREE, masktmp = NULL_TREE;
  bool two_operand_p = !operand_equal_p (vec0, vec1, 0);
  location_t loc = gimple_location (gsi_stmt (*gsi));
  unsigned i;

  if (!TYPE_VECTOR_SUBPARTS (vect_type).is_constant (&elements))
    return;

  if (TREE_CODE (mask) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (mask);
      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == VECTOR_CST)
	mask = gimple_assign_rhs1 (def_stmt);
    }

  vec_perm_builder sel_int;

  if (TREE_CODE (mask) == VECTOR_CST
      && tree_to_vec_perm_builder (&sel_int, mask))
    {
      vec_perm_indices indices (sel_int, 2, elements);
      machine_mode vmode = TYPE_MODE (vect_type);
      tree lhs_type = TREE_TYPE (gimple_assign_lhs (stmt));
      machine_mode lhs_mode = TYPE_MODE (lhs_type);
      if (can_vec_perm_const_p (lhs_mode, vmode, indices))
	{
	  gimple_assign_set_rhs3 (stmt, mask);
	  update_stmt (stmt);
	  return;
	}
      /* Also detect vec_shr pattern - VEC_PERM_EXPR with zero
	 vector as VEC1 and a right element shift MASK.  */
      if (optab_handler (vec_shr_optab, TYPE_MODE (vect_type))
	  != CODE_FOR_nothing
	  && TREE_CODE (vec1) == VECTOR_CST
	  && initializer_zerop (vec1)
	  && maybe_ne (indices[0], 0)
	  && known_lt (poly_uint64 (indices[0]), elements))
	{
	  bool ok_p = indices.series_p (0, 1, indices[0], 1);
	  if (!ok_p)
	    {
	      for (i = 1; i < elements; ++i)
		{
		  poly_uint64 actual = indices[i];
		  poly_uint64 expected = i + indices[0];
		  /* Indices into the second vector are all equivalent.  */
		  if (maybe_lt (actual, elements)
		      ? maybe_ne (actual, expected)
		      : maybe_lt (expected, elements))
		    break;
		}
	      ok_p = i == elements;
	    }
	  if (ok_p)
	    {
	      gimple_assign_set_rhs3 (stmt, mask);
	      update_stmt (stmt);
	      return;
	    }
	}
      /* And similarly vec_shl pattern.  */
      if (optab_handler (vec_shl_optab, TYPE_MODE (vect_type))
	  != CODE_FOR_nothing
	  && TREE_CODE (vec0) == VECTOR_CST
	  && initializer_zerop (vec0))
	{
	  unsigned int first = 0;
	  for (i = 0; i < elements; ++i)
	    if (known_eq (poly_uint64 (indices[i]), elements))
	      {
		if (i == 0 || first)
		  break;
		first = i;
	      }
	    else if (first
		     ? maybe_ne (poly_uint64 (indices[i]),
					      elements + i - first)
		     : maybe_ge (poly_uint64 (indices[i]), elements))
	      break;
	  if (first && i == elements)
	    {
	      gimple_assign_set_rhs3 (stmt, mask);
	      update_stmt (stmt);
	      return;
	    }
	}
    }
  else if (can_vec_perm_var_p (TYPE_MODE (vect_type)))
    return;

  if (!warning_suppressed_p (stmt, OPT_Wvector_operation_performance))
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector shuffling operation will be expanded piecewise");

  vec_alloc (v, elements);
  bool constant_p = true;
  for (i = 0; i < elements; i++)
    {
      si = size_int (i);
      i_val = vector_element (gsi, mask, si, &masktmp);

      if (TREE_CODE (i_val) == INTEGER_CST)
        {
	  unsigned HOST_WIDE_INT index;

	  index = TREE_INT_CST_LOW (i_val);
	  if (!tree_fits_uhwi_p (i_val) || index >= elements)
	    i_val = build_int_cst (mask_elt_type, index & (elements - 1));

          if (two_operand_p && (index & elements) != 0)
	    t = vector_element (gsi, vec1, i_val, &vec1tmp);
	  else
	    t = vector_element (gsi, vec0, i_val, &vec0tmp);

          t = force_gimple_operand_gsi (gsi, t, true, NULL_TREE,
					true, GSI_SAME_STMT);
        }
      else
        {
	  tree cond = NULL_TREE, v0_val;

	  if (two_operand_p)
	    {
	      cond = fold_build2 (BIT_AND_EXPR, mask_elt_type, i_val,
			          build_int_cst (mask_elt_type, elements));
	      cond = force_gimple_operand_gsi (gsi, cond, true, NULL_TREE,
					       true, GSI_SAME_STMT);
	    }

	  i_val = fold_build2 (BIT_AND_EXPR, mask_elt_type, i_val,
			       build_int_cst (mask_elt_type, elements - 1));
	  i_val = force_gimple_operand_gsi (gsi, i_val, true, NULL_TREE,
					    true, GSI_SAME_STMT);

	  v0_val = vector_element (gsi, vec0, i_val, &vec0tmp);
	  v0_val = force_gimple_operand_gsi (gsi, v0_val, true, NULL_TREE,
					     true, GSI_SAME_STMT);

	  if (two_operand_p)
	    {
	      tree v1_val;

	      v1_val = vector_element (gsi, vec1, i_val, &vec1tmp);
	      v1_val = force_gimple_operand_gsi (gsi, v1_val, true, NULL_TREE,
						 true, GSI_SAME_STMT);

	      cond = fold_build2 (EQ_EXPR, boolean_type_node,
				  cond, build_zero_cst (mask_elt_type));
	      cond = fold_build3 (COND_EXPR, vect_elt_type,
				  cond, v0_val, v1_val);
              t = force_gimple_operand_gsi (gsi, cond, true, NULL_TREE,
					    true, GSI_SAME_STMT);
            }
	  else
	    t = v0_val;
        }

      if (!CONSTANT_CLASS_P (t))
	constant_p = false;
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, t);
    }

  if (constant_p)
    constr = build_vector_from_ctor (vect_type, v);
  else
    constr = build_constructor (vect_type, v);
  gimple_assign_set_rhs_from_tree (gsi, constr);
  update_stmt (gsi_stmt (*gsi));
}

/* If OP is a uniform vector return the element it is a splat from.  */

static tree
ssa_uniform_vector_p (tree op)
{
  if (TREE_CODE (op) == VECTOR_CST
      || TREE_CODE (op) == VEC_DUPLICATE_EXPR
      || TREE_CODE (op) == CONSTRUCTOR)
    return uniform_vector_p (op);
  if (TREE_CODE (op) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
      if (gimple_assign_single_p (def_stmt))
	return uniform_vector_p (gimple_assign_rhs1 (def_stmt));
    }
  return NULL_TREE;
}

/* Return type in which CODE operation with optab OP can be
   computed.  */

static tree
get_compute_type (enum tree_code code, optab op, tree type)
{
  /* For very wide vectors, try using a smaller vector mode.  */
  tree compute_type = type;
  if (op
      && (!VECTOR_MODE_P (TYPE_MODE (type))
	  || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing))
    {
      tree vector_compute_type
	= type_for_widest_vector_mode (TREE_TYPE (type), op);
      if (vector_compute_type != NULL_TREE
	  && subparts_gt (compute_type, vector_compute_type)
	  && maybe_ne (TYPE_VECTOR_SUBPARTS (vector_compute_type), 1U)
	  && (optab_handler (op, TYPE_MODE (vector_compute_type))
	      != CODE_FOR_nothing))
	compute_type = vector_compute_type;
    }

  /* If we are breaking a BLKmode vector into smaller pieces,
     type_for_widest_vector_mode has already looked into the optab,
     so skip these checks.  */
  if (compute_type == type)
    {
      machine_mode compute_mode = TYPE_MODE (compute_type);
      if (VECTOR_MODE_P (compute_mode))
	{
	  if (op && optab_handler (op, compute_mode) != CODE_FOR_nothing)
	    return compute_type;
	  if (code == MULT_HIGHPART_EXPR
	      && can_mult_highpart_p (compute_mode,
				      TYPE_UNSIGNED (compute_type)))
	    return compute_type;
	}
      /* There is no operation in hardware, so fall back to scalars.  */
      compute_type = TREE_TYPE (type);
    }

  return compute_type;
}

static tree
do_cond (gimple_stmt_iterator *gsi, tree inner_type, tree a, tree b,
	 tree bitpos, tree bitsize, enum tree_code code,
	 tree type ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (TREE_TYPE (a)) == VECTOR_TYPE)
    a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  if (TREE_CODE (TREE_TYPE (b)) == VECTOR_TYPE)
    b = tree_vec_extract (gsi, inner_type, b, bitsize, bitpos);
  tree cond = gimple_assign_rhs1 (gsi_stmt (*gsi));
  return gimplify_build3 (gsi, code, inner_type, unshare_expr (cond), a, b);
}

/* Expand a vector COND_EXPR to scalars, piecewise.  */
static void
expand_vector_scalar_condition (gimple_stmt_iterator *gsi)
{
  gassign *stmt = as_a <gassign *> (gsi_stmt (*gsi));
  tree lhs = gimple_assign_lhs (stmt);
  tree type = TREE_TYPE (lhs);
  tree compute_type = get_compute_type (COND_EXPR, mov_optab, type);
  machine_mode compute_mode = TYPE_MODE (compute_type);
  gcc_assert (compute_mode != BLKmode);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree rhs3 = gimple_assign_rhs3 (stmt);
  tree new_rhs;

  /* If the compute mode is not a vector mode (hence we are not decomposing
     a BLKmode vector to smaller, hardware-supported vectors), we may want
     to expand the operations in parallel.  */
  if (!VECTOR_MODE_P (compute_mode))
    new_rhs = expand_vector_parallel (gsi, do_cond, type, rhs2, rhs3,
				      COND_EXPR);
  else
    new_rhs = expand_vector_piecewise (gsi, do_cond, type, compute_type,
				       rhs2, rhs3, COND_EXPR, false);
  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (new_rhs)))
    new_rhs = gimplify_build1 (gsi, VIEW_CONVERT_EXPR, TREE_TYPE (lhs),
			       new_rhs);

  /* NOTE:  We should avoid using gimple_assign_set_rhs_from_tree. One
     way to do it is change expand_vector_operation and its callees to
     return a tree_code, RHS1 and RHS2 instead of a tree. */
  gimple_assign_set_rhs_from_tree (gsi, new_rhs);
  update_stmt (gsi_stmt (*gsi));
}

/* Callback for expand_vector_piecewise to do VEC_CONVERT ifn call
   lowering.  If INNER_TYPE is not a vector type, this is a scalar
   fallback.  */

static tree
do_vec_conversion (gimple_stmt_iterator *gsi, tree inner_type, tree a,
		   tree decl, tree bitpos, tree bitsize,
		   enum tree_code code, tree type)
{
  a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  if (!VECTOR_TYPE_P (inner_type))
    return gimplify_build1 (gsi, code, TREE_TYPE (type), a);
  if (code == CALL_EXPR)
    {
      gimple *g = gimple_build_call (decl, 1, a);
      tree lhs = make_ssa_name (TREE_TYPE (TREE_TYPE (decl)));
      gimple_call_set_lhs (g, lhs);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      return lhs;
    }
  else
    {
      tree outer_type = build_vector_type (TREE_TYPE (type),
					   TYPE_VECTOR_SUBPARTS (inner_type));
      return gimplify_build1 (gsi, code, outer_type, a);
    }
}

/* Similarly, but for narrowing conversion.  */

static tree
do_vec_narrow_conversion (gimple_stmt_iterator *gsi, tree inner_type, tree a,
			  tree, tree bitpos, tree, enum tree_code code,
			  tree type)
{
  tree itype = build_vector_type (TREE_TYPE (inner_type),
				  exact_div (TYPE_VECTOR_SUBPARTS (inner_type),
					     2));
  tree b = tree_vec_extract (gsi, itype, a, TYPE_SIZE (itype), bitpos);
  tree c = tree_vec_extract (gsi, itype, a, TYPE_SIZE (itype),
			     int_const_binop (PLUS_EXPR, bitpos,
					      TYPE_SIZE (itype)));
  tree outer_type = build_vector_type (TREE_TYPE (type),
				       TYPE_VECTOR_SUBPARTS (inner_type));
  return gimplify_build2 (gsi, code, outer_type, b, c);
}

/* Expand VEC_CONVERT ifn call.  */

static void
expand_vector_conversion (gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  gimple *g;
  tree lhs = gimple_call_lhs (stmt);
  if (lhs == NULL_TREE)
    {
      g = gimple_build_nop ();
      gsi_replace (gsi, g, false);
      return;
    }
  tree arg = gimple_call_arg (stmt, 0);
  tree ret_type = TREE_TYPE (lhs);
  tree arg_type = TREE_TYPE (arg);
  tree new_rhs, compute_type = TREE_TYPE (arg_type);
  enum tree_code code = NOP_EXPR;
  enum tree_code code1 = ERROR_MARK;
  enum { NARROW, NONE, WIDEN } modifier = NONE;
  optab optab1 = unknown_optab;

  gcc_checking_assert (VECTOR_TYPE_P (ret_type) && VECTOR_TYPE_P (arg_type));
  if (INTEGRAL_TYPE_P (TREE_TYPE (ret_type))
      && SCALAR_FLOAT_TYPE_P (TREE_TYPE (arg_type)))
    code = FIX_TRUNC_EXPR;
  else if (INTEGRAL_TYPE_P (TREE_TYPE (arg_type))
	   && SCALAR_FLOAT_TYPE_P (TREE_TYPE (ret_type)))
    code = FLOAT_EXPR;
  unsigned int ret_elt_bits = vector_element_bits (ret_type);
  unsigned int arg_elt_bits = vector_element_bits (arg_type);
  if (ret_elt_bits < arg_elt_bits)
    modifier = NARROW;
  else if (ret_elt_bits > arg_elt_bits)
    modifier = WIDEN;

  if (modifier == NONE && (code == FIX_TRUNC_EXPR || code == FLOAT_EXPR))
    {
      if (supportable_convert_operation (code, ret_type, arg_type, &code1))
	{
	  g = gimple_build_assign (lhs, code1, arg);
	  gsi_replace (gsi, g, false);
	  return;
	}
      /* Can't use get_compute_type here, as supportable_convert_operation
	 doesn't necessarily use an optab and needs two arguments.  */
      tree vec_compute_type
	= type_for_widest_vector_mode (TREE_TYPE (arg_type), mov_optab);
      if (vec_compute_type
	  && VECTOR_MODE_P (TYPE_MODE (vec_compute_type))
	  && subparts_gt (arg_type, vec_compute_type))
	{
	  unsigned HOST_WIDE_INT nelts
	    = constant_lower_bound (TYPE_VECTOR_SUBPARTS (vec_compute_type));
	  while (nelts > 1)
	    {
	      tree ret1_type = build_vector_type (TREE_TYPE (ret_type), nelts);
	      tree arg1_type = build_vector_type (TREE_TYPE (arg_type), nelts);
	      if (supportable_convert_operation (code, ret1_type, arg1_type,
						 &code1))
		{
		  new_rhs = expand_vector_piecewise (gsi, do_vec_conversion,
						     ret_type, arg1_type, arg,
						     NULL_TREE, code1, false);
		  g = gimple_build_assign (lhs, new_rhs);
		  gsi_replace (gsi, g, false);
		  return;
		}
	      nelts = nelts / 2;
	    }
	}
    }
  else if (modifier == NARROW)
    {
      switch (code)
	{
	CASE_CONVERT:
	  code1 = VEC_PACK_TRUNC_EXPR;
	  optab1 = optab_for_tree_code (code1, arg_type, optab_default);
	  break;
	case FIX_TRUNC_EXPR:
	  code1 = VEC_PACK_FIX_TRUNC_EXPR;
	  /* The signedness is determined from output operand.  */
	  optab1 = optab_for_tree_code (code1, ret_type, optab_default);
	  break;
	case FLOAT_EXPR:
	  code1 = VEC_PACK_FLOAT_EXPR;
	  optab1 = optab_for_tree_code (code1, arg_type, optab_default);
	  break;
	default:
	  gcc_unreachable ();
	}

      if (optab1)
	compute_type = get_compute_type (code1, optab1, arg_type);
      enum insn_code icode1;
      if (VECTOR_TYPE_P (compute_type)
	  && ((icode1 = optab_handler (optab1, TYPE_MODE (compute_type)))
	      != CODE_FOR_nothing)
	  && VECTOR_MODE_P (insn_data[icode1].operand[0].mode))
	{
	  tree cretd_type
	    = build_vector_type (TREE_TYPE (ret_type),
				 TYPE_VECTOR_SUBPARTS (compute_type) * 2);
	  if (insn_data[icode1].operand[0].mode == TYPE_MODE (cretd_type))
	    {
	      if (compute_type == arg_type)
		{
		  new_rhs = gimplify_build2 (gsi, code1, cretd_type,
					     arg, build_zero_cst (arg_type));
		  new_rhs = tree_vec_extract (gsi, ret_type, new_rhs,
					      TYPE_SIZE (ret_type),
					      bitsize_int (0));
		  g = gimple_build_assign (lhs, new_rhs);
		  gsi_replace (gsi, g, false);
		  return;
		}
	      tree dcompute_type
		= build_vector_type (TREE_TYPE (compute_type),
				     TYPE_VECTOR_SUBPARTS (compute_type) * 2);
	      if (TYPE_MAIN_VARIANT (dcompute_type)
		  == TYPE_MAIN_VARIANT (arg_type))
		new_rhs = do_vec_narrow_conversion (gsi, dcompute_type, arg,
						    NULL_TREE, bitsize_int (0),
						    NULL_TREE, code1,
						    ret_type);
	      else
		new_rhs = expand_vector_piecewise (gsi,
						   do_vec_narrow_conversion,
						   arg_type, dcompute_type,
						   arg, NULL_TREE, code1,
						   false, ret_type);
	      g = gimple_build_assign (lhs, new_rhs);
	      gsi_replace (gsi, g, false);
	      return;
	    }
	}
    }
  else if (modifier == WIDEN)
    {
      enum tree_code code2 = ERROR_MARK;
      optab optab2 = unknown_optab;
      switch (code)
	{
	CASE_CONVERT:
	  code1 = VEC_UNPACK_LO_EXPR;
          code2 = VEC_UNPACK_HI_EXPR;
	  break;
	case FIX_TRUNC_EXPR:
	  code1 = VEC_UNPACK_FIX_TRUNC_LO_EXPR;
	  code2 = VEC_UNPACK_FIX_TRUNC_HI_EXPR;
	  break;
	case FLOAT_EXPR:
	  code1 = VEC_UNPACK_FLOAT_LO_EXPR;
	  code2 = VEC_UNPACK_FLOAT_HI_EXPR;
	  break;
	default:
	  gcc_unreachable ();
	}
      if (BYTES_BIG_ENDIAN)
	std::swap (code1, code2);

      if (code == FIX_TRUNC_EXPR)
	{
	  /* The signedness is determined from output operand.  */
	  optab1 = optab_for_tree_code (code1, ret_type, optab_default);
	  optab2 = optab_for_tree_code (code2, ret_type, optab_default);
	}
      else
	{
	  optab1 = optab_for_tree_code (code1, arg_type, optab_default);
	  optab2 = optab_for_tree_code (code2, arg_type, optab_default);
	}

      if (optab1 && optab2)
	compute_type = get_compute_type (code1, optab1, arg_type);

      enum insn_code icode1, icode2;
      if (VECTOR_TYPE_P (compute_type)
	  && ((icode1 = optab_handler (optab1, TYPE_MODE (compute_type)))
	      != CODE_FOR_nothing)
	  && ((icode2 = optab_handler (optab2, TYPE_MODE (compute_type)))
	      != CODE_FOR_nothing)
	  && VECTOR_MODE_P (insn_data[icode1].operand[0].mode)
	  && (insn_data[icode1].operand[0].mode
	      == insn_data[icode2].operand[0].mode))
	{
	  poly_uint64 nunits
	    = exact_div (TYPE_VECTOR_SUBPARTS (compute_type), 2);
	  tree cretd_type = build_vector_type (TREE_TYPE (ret_type), nunits);
	  if (insn_data[icode1].operand[0].mode == TYPE_MODE (cretd_type))
	    {
	      vec<constructor_elt, va_gc> *v;
	      tree part_width = TYPE_SIZE (compute_type);
	      tree index = bitsize_int (0);
	      int nunits = nunits_for_known_piecewise_op (arg_type);
	      int delta = tree_to_uhwi (part_width) / arg_elt_bits;
	      int i;
	      location_t loc = gimple_location (gsi_stmt (*gsi));

	      if (compute_type != arg_type)
		{
		  if (!warning_suppressed_p (gsi_stmt (*gsi),
					     OPT_Wvector_operation_performance))
		    warning_at (loc, OPT_Wvector_operation_performance,
				"vector operation will be expanded piecewise");
		}
	      else
		{
		  nunits = 1;
		  delta = 1;
		}

	      vec_alloc (v, (nunits + delta - 1) / delta * 2);
	      bool constant_p = true;
	      for (i = 0; i < nunits;
		   i += delta, index = int_const_binop (PLUS_EXPR, index,
							part_width))
		{
		  tree a = arg;
		  if (compute_type != arg_type)
		    a = tree_vec_extract (gsi, compute_type, a, part_width,
					  index);
		  tree result = gimplify_build1 (gsi, code1, cretd_type, a);
		  constructor_elt ce = { NULL_TREE, result };
		  if (!CONSTANT_CLASS_P (ce.value))
		    constant_p = false;
		  v->quick_push (ce);
		  ce.value = gimplify_build1 (gsi, code2, cretd_type, a);
		  if (!CONSTANT_CLASS_P (ce.value))
		    constant_p = false;
		  v->quick_push (ce);
		}

	      if (constant_p)
		new_rhs = build_vector_from_ctor (ret_type, v);
	      else
		new_rhs = build_constructor (ret_type, v);
	      g = gimple_build_assign (lhs, new_rhs);
	      gsi_replace (gsi, g, false);
	      return;
	    }
	}
    }

  new_rhs = expand_vector_piecewise (gsi, do_vec_conversion, arg_type,
				     TREE_TYPE (arg_type), arg,
				     NULL_TREE, code, false, ret_type);
  g = gimple_build_assign (lhs, new_rhs);
  gsi_replace (gsi, g, false);
}

/* Process one statement.  If we identify a vector operation, expand it.  */

static void
expand_vector_operations_1 (gimple_stmt_iterator *gsi,
			    bitmap dce_ssa_names)
{
  tree lhs, rhs1, rhs2 = NULL, type, compute_type = NULL_TREE;
  enum tree_code code;
  optab op = unknown_optab;
  enum gimple_rhs_class rhs_class;
  tree new_rhs;

  /* Only consider code == GIMPLE_ASSIGN. */
  gassign *stmt = dyn_cast <gassign *> (gsi_stmt (*gsi));
  if (!stmt)
    {
      if (gimple_call_internal_p (gsi_stmt (*gsi), IFN_VEC_CONVERT))
	expand_vector_conversion (gsi);
      return;
    }

  code = gimple_assign_rhs_code (stmt);
  rhs_class = get_gimple_rhs_class (code);
  lhs = gimple_assign_lhs (stmt);

  if (code == VEC_PERM_EXPR)
    {
      lower_vec_perm (gsi);
      return;
    }

  if (code == VEC_COND_EXPR)
    {
      expand_vector_condition (gsi, dce_ssa_names);
      return;
    }

  if (code == COND_EXPR
      && TREE_CODE (TREE_TYPE (gimple_assign_lhs (stmt))) == VECTOR_TYPE
      && TYPE_MODE (TREE_TYPE (gimple_assign_lhs (stmt))) == BLKmode)
    {
      expand_vector_scalar_condition (gsi);
      return;
    }

  if (code == CONSTRUCTOR
      && TREE_CODE (lhs) == SSA_NAME
      && VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (lhs)))
      && !gimple_clobber_p (stmt)
      && optimize)
    {
      optimize_vector_constructor (gsi);
      return;
    }

  if (rhs_class != GIMPLE_UNARY_RHS && rhs_class != GIMPLE_BINARY_RHS)
    return;

  rhs1 = gimple_assign_rhs1 (stmt);
  if (rhs_class == GIMPLE_BINARY_RHS)
    rhs2 = gimple_assign_rhs2 (stmt);

  type = TREE_TYPE (lhs);
  if (!VECTOR_TYPE_P (type)
      || !VECTOR_TYPE_P (TREE_TYPE (rhs1)))
    return;
 
  /* A scalar operation pretending to be a vector one.  */
  if (VECTOR_BOOLEAN_TYPE_P (type)
      && !VECTOR_MODE_P (TYPE_MODE (type))
      && TYPE_MODE (type) != BLKmode
      && (TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) != tcc_comparison
	  || (VECTOR_BOOLEAN_TYPE_P (TREE_TYPE (rhs1))
	      && !VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (rhs1)))
	      && TYPE_MODE (TREE_TYPE (rhs1)) != BLKmode)))
    return;

  /* If the vector operation is operating on all same vector elements
     implement it with a scalar operation and a splat if the target
     supports the scalar operation.  */
  tree srhs1, srhs2 = NULL_TREE;
  if ((srhs1 = ssa_uniform_vector_p (rhs1)) != NULL_TREE
      && (rhs2 == NULL_TREE
	  || (! VECTOR_TYPE_P (TREE_TYPE (rhs2))
	      && (srhs2 = rhs2))
	  || (srhs2 = ssa_uniform_vector_p (rhs2)) != NULL_TREE)
      /* As we query direct optabs restrict to non-convert operations.  */
      && TYPE_MODE (TREE_TYPE (type)) == TYPE_MODE (TREE_TYPE (srhs1)))
    {
      op = optab_for_tree_code (code, TREE_TYPE (type), optab_scalar);
      if (op >= FIRST_NORM_OPTAB && op <= LAST_NORM_OPTAB
	  && optab_handler (op, TYPE_MODE (TREE_TYPE (type))) != CODE_FOR_nothing)
	{
	  tree stype = TREE_TYPE (TREE_TYPE (lhs));
	  tree slhs = (rhs2 != NULL_TREE)
		      ? gimplify_build2 (gsi, code, stype, srhs1, srhs2)
		      : gimplify_build1 (gsi, code, stype, srhs1);
	  gimple_assign_set_rhs_from_tree (gsi,
					   build_vector_from_val (type, slhs));
	  update_stmt (stmt);
	  return;
	}
    }

  if (CONVERT_EXPR_CODE_P (code)
      || code == FLOAT_EXPR
      || code == FIX_TRUNC_EXPR
      || code == VIEW_CONVERT_EXPR)
    return;

  /* The signedness is determined from input argument.  */
  if (code == VEC_UNPACK_FLOAT_HI_EXPR
      || code == VEC_UNPACK_FLOAT_LO_EXPR
      || code == VEC_PACK_FLOAT_EXPR)
    {
      /* We do not know how to scalarize those.  */
      return;
    }

  /* For widening/narrowing vector operations, the relevant type is of the
     arguments, not the widened result.  VEC_UNPACK_FLOAT_*_EXPR is
     calculated in the same way above.  */
  if (code == WIDEN_SUM_EXPR
      || code == VEC_WIDEN_PLUS_HI_EXPR
      || code == VEC_WIDEN_PLUS_LO_EXPR
      || code == VEC_WIDEN_MINUS_HI_EXPR
      || code == VEC_WIDEN_MINUS_LO_EXPR
      || code == VEC_WIDEN_MULT_HI_EXPR
      || code == VEC_WIDEN_MULT_LO_EXPR
      || code == VEC_WIDEN_MULT_EVEN_EXPR
      || code == VEC_WIDEN_MULT_ODD_EXPR
      || code == VEC_UNPACK_HI_EXPR
      || code == VEC_UNPACK_LO_EXPR
      || code == VEC_UNPACK_FIX_TRUNC_HI_EXPR
      || code == VEC_UNPACK_FIX_TRUNC_LO_EXPR
      || code == VEC_PACK_TRUNC_EXPR
      || code == VEC_PACK_SAT_EXPR
      || code == VEC_PACK_FIX_TRUNC_EXPR
      || code == VEC_WIDEN_LSHIFT_HI_EXPR
      || code == VEC_WIDEN_LSHIFT_LO_EXPR)
    {
      /* We do not know how to scalarize those.  */
      return;
    }

  /* Choose between vector shift/rotate by vector and vector shift/rotate by
     scalar */
  if (code == LSHIFT_EXPR
      || code == RSHIFT_EXPR
      || code == LROTATE_EXPR
      || code == RROTATE_EXPR)
    {
      optab opv;

      /* Check whether we have vector <op> {x,x,x,x} where x
         could be a scalar variable or a constant.  Transform
         vector <op> {x,x,x,x} ==> vector <op> scalar.  */
      if (VECTOR_INTEGER_TYPE_P (TREE_TYPE (rhs2)))
        {
          tree first;

          if ((first = ssa_uniform_vector_p (rhs2)) != NULL_TREE)
            {
              gimple_assign_set_rhs2 (stmt, first);
              update_stmt (stmt);
              rhs2 = first;
            }
        }

      opv = optab_for_tree_code (code, type, optab_vector);
      if (VECTOR_INTEGER_TYPE_P (TREE_TYPE (rhs2)))
	op = opv;
      else
	{
          op = optab_for_tree_code (code, type, optab_scalar);

	  compute_type = get_compute_type (code, op, type);
	  if (compute_type == type)
	    return;
	  /* The rtl expander will expand vector/scalar as vector/vector
	     if necessary.  Pick one with wider vector type.  */
	  tree compute_vtype = get_compute_type (code, opv, type);
	  if (subparts_gt (compute_vtype, compute_type))
	    {
	      compute_type = compute_vtype;
	      op = opv;
	    }
	}

      if (code == LROTATE_EXPR || code == RROTATE_EXPR)
	{
	  if (compute_type == NULL_TREE)
	    compute_type = get_compute_type (code, op, type);
	  if (compute_type == type)
	    return;
	  /* Before splitting vector rotates into scalar rotates,
	     see if we can't use vector shifts and BIT_IOR_EXPR
	     instead.  For vector by vector rotates we'd also
	     need to check BIT_AND_EXPR and NEGATE_EXPR, punt there
	     for now, fold doesn't seem to create such rotates anyway.  */
	  if (compute_type == TREE_TYPE (type)
	      && !VECTOR_INTEGER_TYPE_P (TREE_TYPE (rhs2)))
	    {
	      optab oplv = vashl_optab, opl = ashl_optab;
	      optab oprv = vlshr_optab, opr = lshr_optab, opo = ior_optab;
	      tree compute_lvtype = get_compute_type (LSHIFT_EXPR, oplv, type);
	      tree compute_rvtype = get_compute_type (RSHIFT_EXPR, oprv, type);
	      tree compute_otype = get_compute_type (BIT_IOR_EXPR, opo, type);
	      tree compute_ltype = get_compute_type (LSHIFT_EXPR, opl, type);
	      tree compute_rtype = get_compute_type (RSHIFT_EXPR, opr, type);
	      /* The rtl expander will expand vector/scalar as vector/vector
		 if necessary.  Pick one with wider vector type.  */
	      if (subparts_gt (compute_lvtype, compute_ltype))
		{
		  compute_ltype = compute_lvtype;
		  opl = oplv;
		}
	      if (subparts_gt (compute_rvtype, compute_rtype))
		{
		  compute_rtype = compute_rvtype;
		  opr = oprv;
		}
	      /* Pick the narrowest type from LSHIFT_EXPR, RSHIFT_EXPR and
		 BIT_IOR_EXPR.  */
	      compute_type = compute_ltype;
	      if (subparts_gt (compute_type, compute_rtype))
		compute_type = compute_rtype;
	      if (subparts_gt (compute_type, compute_otype))
		compute_type = compute_otype;
	      /* Verify all 3 operations can be performed in that type.  */
	      if (compute_type != TREE_TYPE (type))
		{
		  if (optab_handler (opl, TYPE_MODE (compute_type))
		      == CODE_FOR_nothing
		      || optab_handler (opr, TYPE_MODE (compute_type))
			 == CODE_FOR_nothing
		      || optab_handler (opo, TYPE_MODE (compute_type))
			 == CODE_FOR_nothing)
		    compute_type = TREE_TYPE (type);
		}
	    }
	}
    }
  else
    op = optab_for_tree_code (code, type, optab_default);

  /* Optabs will try converting a negation into a subtraction, so
     look for it as well.  TODO: negation of floating-point vectors
     might be turned into an exclusive OR toggling the sign bit.  */
  if (op == unknown_optab
      && code == NEGATE_EXPR
      && INTEGRAL_TYPE_P (TREE_TYPE (type)))
    op = optab_for_tree_code (MINUS_EXPR, type, optab_default);

  if (compute_type == NULL_TREE)
    compute_type = get_compute_type (code, op, type);
  if (compute_type == type)
    return;

  new_rhs = expand_vector_operation (gsi, type, compute_type, stmt, code,
				     dce_ssa_names);

  /* Leave expression untouched for later expansion.  */
  if (new_rhs == NULL_TREE)
    return;

  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (new_rhs)))
    new_rhs = gimplify_build1 (gsi, VIEW_CONVERT_EXPR, TREE_TYPE (lhs),
                               new_rhs);

  /* NOTE:  We should avoid using gimple_assign_set_rhs_from_tree. One
     way to do it is change expand_vector_operation and its callees to
     return a tree_code, RHS1 and RHS2 instead of a tree. */
  gimple_assign_set_rhs_from_tree (gsi, new_rhs);
  update_stmt (gsi_stmt (*gsi));
}

/* Use this to lower vector operations introduced by the vectorizer,
   if it may need the bit-twiddling tricks implemented in this file.  */

static unsigned int
expand_vector_operations (void)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  bool cfg_changed = false;

  auto_bitmap dce_ssa_names;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  expand_vector_operations_1 (&gsi, dce_ssa_names);
	  /* ???  If we do not cleanup EH then we will ICE in
	     verification.  But in reality we have created wrong-code
	     as we did not properly transition EH info and edges to
	     the piecewise computations.  */
	  if (maybe_clean_eh_stmt (gsi_stmt (gsi))
	      && gimple_purge_dead_eh_edges (bb))
	    cfg_changed = true;
	}
    }

  simple_dce_from_worklist (dce_ssa_names);

  return cfg_changed ? TODO_cleanup_cfg : 0;
}

namespace {

const pass_data pass_data_lower_vector =
{
  GIMPLE_PASS, /* type */
  "veclower", /* name */
  OPTGROUP_VEC, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_gimple_lvec, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_lower_vector : public gimple_opt_pass
{
public:
  pass_lower_vector (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_vector, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
    {
      return !(fun->curr_properties & PROP_gimple_lvec);
    }

  unsigned int execute (function *) final override
    {
      return expand_vector_operations ();
    }

}; // class pass_lower_vector

} // anon namespace

gimple_opt_pass *
make_pass_lower_vector (gcc::context *ctxt)
{
  return new pass_lower_vector (ctxt);
}

namespace {

const pass_data pass_data_lower_vector_ssa =
{
  GIMPLE_PASS, /* type */
  "veclower2", /* name */
  OPTGROUP_VEC, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_gimple_lvec, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_update_ssa
    | TODO_cleanup_cfg ), /* todo_flags_finish */
};

class pass_lower_vector_ssa : public gimple_opt_pass
{
public:
  pass_lower_vector_ssa (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_lower_vector_ssa, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () final override
  {
    return new pass_lower_vector_ssa (m_ctxt);
  }
  unsigned int execute (function *) final override
    {
      return expand_vector_operations ();
    }

}; // class pass_lower_vector_ssa

} // anon namespace

gimple_opt_pass *
make_pass_lower_vector_ssa (gcc::context *ctxt)
{
  return new pass_lower_vector_ssa (ctxt);
}

#include "gt-tree-vect-generic.h"
