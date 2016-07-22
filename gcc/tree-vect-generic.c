/* Lower vector operations to scalar operations.
   Copyright (C) 2004-2016 Free Software Foundation, Inc.

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


static void expand_vector_operations_1 (gimple_stmt_iterator *);


/* Build a constant of type TYPE, made of VALUE's bits replicated
   every TYPE_SIZE (INNER_TYPE) bits to fit TYPE's precision.  */
static tree
build_replicated_const (tree type, tree inner_type, HOST_WIDE_INT value)
{
  int width = tree_to_uhwi (TYPE_SIZE (inner_type));
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

  /* We build a new type, but we canonicalize it nevertheless,
     because it still saves some memory.  */
  vector_last_nunits = nunits;
  vector_last_type = type_hash_canon (nunits,
				      build_vector_type (vector_inner_type,
							 nunits));
  return vector_last_type;
}

typedef tree (*elem_op_func) (gimple_stmt_iterator *,
			      tree, tree, tree, tree, tree, enum tree_code,
			      tree);

static inline tree
tree_vec_extract (gimple_stmt_iterator *gsi, tree type,
		  tree t, tree bitsize, tree bitpos)
{
  if (TREE_CODE (t) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (t);
      if (is_gimple_assign (def_stmt)
	  && (gimple_assign_rhs_code (def_stmt) == VECTOR_CST
	      || (bitpos
		  && gimple_assign_rhs_code (def_stmt) == CONSTRUCTOR)))
	t = gimple_assign_rhs1 (def_stmt);
    }
  if (bitpos)
    {
      if (TREE_CODE (type) == BOOLEAN_TYPE)
	{
	  tree itype
	    = build_nonstandard_integer_type (tree_to_uhwi (bitsize), 0);
	  tree field = gimplify_build3 (gsi, BIT_FIELD_REF, itype, t,
					bitsize, bitpos);
	  return gimplify_build2 (gsi, NE_EXPR, type, field,
				  build_zero_cst (itype));
	}
      else
	return gimplify_build3 (gsi, BIT_FIELD_REF, type, t, bitsize, bitpos);
    }
  else
    return gimplify_build1 (gsi, VIEW_CONVERT_EXPR, type, t);
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
  tree inner_type = TREE_TYPE (TREE_TYPE (a));
  unsigned HOST_WIDE_INT max;
  tree low_bits, high_bits, a_low, b_low, result_low, signs;

  max = GET_MODE_MASK (TYPE_MODE (inner_type));
  low_bits = build_replicated_const (word_type, inner_type, max >> 1);
  high_bits = build_replicated_const (word_type, inner_type, max & ~(max >> 1));

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
  tree inner_type = TREE_TYPE (TREE_TYPE (b));
  HOST_WIDE_INT max;
  tree low_bits, high_bits, b_low, result_low, signs;

  max = GET_MODE_MASK (TYPE_MODE (inner_type));
  low_bits = build_replicated_const (word_type, inner_type, max >> 1);
  high_bits = build_replicated_const (word_type, inner_type, max & ~(max >> 1));

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
			 tree a, tree b, enum tree_code code)
{
  vec<constructor_elt, va_gc> *v;
  tree part_width = TYPE_SIZE (inner_type);
  tree index = bitsize_int (0);
  int nunits = TYPE_VECTOR_SUBPARTS (type);
  int delta = tree_to_uhwi (part_width)
	      / tree_to_uhwi (TYPE_SIZE (TREE_TYPE (type)));
  int i;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  if (types_compatible_p (gimple_expr_type (gsi_stmt (*gsi)), type))
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector operation will be expanded piecewise");
  else
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector operation will be expanded in parallel");

  vec_alloc (v, (nunits + delta - 1) / delta);
  for (i = 0; i < nunits;
       i += delta, index = int_const_binop (PLUS_EXPR, index, part_width))
    {
      tree result = f (gsi, inner_type, a, b, index, part_width, code, type);
      constructor_elt ce = {NULL_TREE, result};
      v->quick_push (ce);
    }

  return build_constructor (type, v);
}

/* Expand a vector operation to scalars with the freedom to use
   a scalar integer type, or to use a different size for the items
   in the vector type.  */
static tree
expand_vector_parallel (gimple_stmt_iterator *gsi, elem_op_func f, tree type,
			tree a, tree b,
			enum tree_code code)
{
  tree result, compute_type;
  machine_mode mode;
  int n_words = tree_to_uhwi (TYPE_SIZE_UNIT (type)) / UNITS_PER_WORD;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  /* We have three strategies.  If the type is already correct, just do
     the operation an element at a time.  Else, if the vector is wider than
     one word, do it a word at a time; finally, if the vector is smaller
     than one word, do it as a scalar.  */
  if (TYPE_MODE (TREE_TYPE (type)) == word_mode)
     return expand_vector_piecewise (gsi, f,
				     type, TREE_TYPE (type),
				     a, b, code);
  else if (n_words > 1)
    {
      tree word_type = build_word_mode_vector_type (n_words);
      result = expand_vector_piecewise (gsi, f,
				        word_type, TREE_TYPE (word_type),
					a, b, code);
      result = force_gimple_operand_gsi (gsi, result, true, NULL, true,
                                         GSI_SAME_STMT);
    }
  else
    {
      /* Use a single scalar operation with a mode no wider than word_mode.  */
      mode = mode_for_size (tree_to_uhwi (TYPE_SIZE (type)), MODE_INT, 0);
      compute_type = lang_hooks.types.type_for_mode (mode, 1);
      result = f (gsi, compute_type, a, b, NULL_TREE, NULL_TREE, code, type);
      warning_at (loc, OPT_Wvector_operation_performance,
	          "vector operation will be expanded with a "
		  "single scalar operation");
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
  int parts_per_word = UNITS_PER_WORD
	  	       / tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (type)));

  if (INTEGRAL_TYPE_P (TREE_TYPE (type))
      && parts_per_word >= 4
      && TYPE_VECTOR_SUBPARTS (type) >= 4)
    return expand_vector_parallel (gsi, f_parallel,
				   type, a, b, code);
  else
    return expand_vector_piecewise (gsi, f,
				    type, TREE_TYPE (type),
				    a, b, code);
}

/* Try to expand vector comparison expression OP0 CODE OP1 by
   querying optab if the following expression:
	VEC_COND_EXPR< OP0 CODE OP1, {-1,...}, {0,...}>
   can be expanded.  */
static tree
expand_vector_comparison (gimple_stmt_iterator *gsi, tree type, tree op0,
                          tree op1, enum tree_code code)
{
  tree t;
  if (!expand_vec_cmp_expr_p (TREE_TYPE (op0), type)
      && !expand_vec_cond_expr_p (type, TREE_TYPE (op0)))
    t = expand_vector_piecewise (gsi, do_compare, type,
				 TREE_TYPE (TREE_TYPE (op0)), op0, op1, code);
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
  unsigned int i, nunits = TYPE_VECTOR_SUBPARTS (type);
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
      tree *vec = XALLOCAVEC (tree, nunits);
      for (i = 0; i < nunits; i++)
	vec[i] = build_int_cst (TREE_TYPE (type), shiftcnts[i]);
      return gimplify_build2 (gsi, RSHIFT_EXPR, type, op0,
			      build_vector (type, vec));
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
  int mode = -1, this_mode;
  int pre_shift = -1, post_shift;
  unsigned int nunits = TYPE_VECTOR_SUBPARTS (type);
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
  tree *vec;
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
		pre_shift = floor_log2 (d & -d);
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
	    d = abs_d;
	  else if (abs_d == HOST_WIDE_INT_1U << (prec - 1))
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

  vec = XALLOCAVEC (tree, nunits);

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
	      && expand_vec_cond_expr_p (type, type))
	    {
	      tree zero, cst, cond, mask_type;
	      gimple *stmt;

	      mask_type = build_same_sized_truth_vector_type (type);
	      zero = build_zero_cst (type);
	      cond = build2 (LT_EXPR, mask_type, op0, zero);
	      for (i = 0; i < nunits; i++)
		vec[i] = build_int_cst (TREE_TYPE (type),
					(HOST_WIDE_INT_1U
					 << shifts[i]) - 1);
	      cst = build_vector (type, vec);
	      addend = make_ssa_name (type);
	      stmt = gimple_build_assign (addend, VEC_COND_EXPR, cond,
					  cst, zero);
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
	  for (i = 0; i < nunits; i++)
	    vec[i] = build_int_cst (TREE_TYPE (type),
				    (HOST_WIDE_INT_1U
				     << shifts[i]) - 1);
	  mask = build_vector (type, vec);
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

  for (i = 0; i < nunits; i++)
    vec[i] = build_int_cst (TREE_TYPE (type), mulc[i]);
  mulcst = build_vector (type, vec);

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
  tem = gimplify_build2 (gsi, MULT_EXPR, type, cur_op, op1);
  op = optab_for_tree_code (MINUS_EXPR, type, optab_default);
  if (op == unknown_optab
      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
    return NULL_TREE;
  return gimplify_build2 (gsi, MINUS_EXPR, type, op0, tem);
}

/* Expand a vector condition to scalars, by using many conditions
   on the vector's elements.  */
static void
expand_vector_condition (gimple_stmt_iterator *gsi)
{
  gassign *stmt = as_a <gassign *> (gsi_stmt (*gsi));
  tree type = gimple_expr_type (stmt);
  tree a = gimple_assign_rhs1 (stmt);
  tree a1 = a;
  tree a2 = NULL_TREE;
  bool a_is_comparison = false;
  tree b = gimple_assign_rhs2 (stmt);
  tree c = gimple_assign_rhs3 (stmt);
  vec<constructor_elt, va_gc> *v;
  tree constr;
  tree inner_type = TREE_TYPE (type);
  tree cond_type = TREE_TYPE (TREE_TYPE (a));
  tree comp_inner_type = cond_type;
  tree width = TYPE_SIZE (inner_type);
  tree index = bitsize_int (0);
  int nunits = TYPE_VECTOR_SUBPARTS (type);
  int i;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  if (!is_gimple_val (a))
    {
      gcc_assert (COMPARISON_CLASS_P (a));
      a_is_comparison = true;
      a1 = TREE_OPERAND (a, 0);
      a2 = TREE_OPERAND (a, 1);
      comp_inner_type = TREE_TYPE (TREE_TYPE (a1));
    }

  if (expand_vec_cond_expr_p (type, TREE_TYPE (a1)))
    return;

  /* TODO: try and find a smaller vector type.  */

  warning_at (loc, OPT_Wvector_operation_performance,
	      "vector condition will be expanded piecewise");

  vec_alloc (v, nunits);
  for (i = 0; i < nunits;
       i++, index = int_const_binop (PLUS_EXPR, index, width))
    {
      tree aa, result;
      tree bb = tree_vec_extract (gsi, inner_type, b, width, index);
      tree cc = tree_vec_extract (gsi, inner_type, c, width, index);
      if (a_is_comparison)
	{
	  tree aa1 = tree_vec_extract (gsi, comp_inner_type, a1, width, index);
	  tree aa2 = tree_vec_extract (gsi, comp_inner_type, a2, width, index);
	  aa = fold_build2 (TREE_CODE (a), cond_type, aa1, aa2);
	}
      else
	aa = tree_vec_extract (gsi, cond_type, a, width, index);
      result = gimplify_build3 (gsi, COND_EXPR, inner_type, aa, bb, cc);
      constructor_elt ce = {NULL_TREE, result};
      v->quick_push (ce);
    }

  constr = build_constructor (type, v);
  gimple_assign_set_rhs_from_tree (gsi, constr);
  update_stmt (gsi_stmt (*gsi));
}

static tree
expand_vector_operation (gimple_stmt_iterator *gsi, tree type, tree compute_type,
			 gassign *assign, enum tree_code code)
{
  machine_mode compute_mode = TYPE_MODE (compute_type);

  /* If the compute mode is not a vector mode (hence we are not decomposing
     a BLKmode vector to smaller, hardware-supported vectors), we may want
     to expand the operations in parallel.  */
  if (GET_MODE_CLASS (compute_mode) != MODE_VECTOR_INT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_FLOAT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_FRACT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_UFRACT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_ACCUM
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_UACCUM)
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

	  return expand_vector_comparison (gsi, type, rhs1, rhs2, code);
	}

      case TRUNC_DIV_EXPR:
      case TRUNC_MOD_EXPR:
	{
	  tree rhs1 = gimple_assign_rhs1 (assign);
	  tree rhs2 = gimple_assign_rhs2 (assign);
	  tree ret;

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
				    NULL_TREE, code);
  else
    return expand_vector_piecewise (gsi, do_binop, type, compute_type,
				    gimple_assign_rhs1 (assign),
				    gimple_assign_rhs2 (assign), code);
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
  unsigned int i, j, nelts = TYPE_VECTOR_SUBPARTS (type);
  bool all_same = true;
  constructor_elt *elt;
  tree *cst;
  gimple *g;
  tree base = NULL_TREE;
  optab op;

  if (nelts <= 2 || CONSTRUCTOR_NELTS (rhs) != nelts)
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
  cst = XALLOCAVEC (tree, nelts);
  for (i = 0; i < nelts; i++)
    {
      tree this_base = CONSTRUCTOR_ELT (rhs, i)->value;;
      cst[i] = build_zero_cst (TREE_TYPE (base));
      while (this_base != base)
	{
	  g = SSA_NAME_DEF_STMT (this_base);
	  cst[i] = fold_binary (PLUS_EXPR, TREE_TYPE (base),
				cst[i], gimple_assign_rhs2 (g));
	  if (cst[i] == NULL_TREE
	      || TREE_CODE (cst[i]) != INTEGER_CST
	      || TREE_OVERFLOW (cst[i]))
	    return;
	  this_base = gimple_assign_rhs1 (g);
	}
    }
  for (i = 0; i < nelts; i++)
    CONSTRUCTOR_ELT (rhs, i)->value = base;
  g = gimple_build_assign (make_ssa_name (type), rhs);
  gsi_insert_before (gsi, g, GSI_SAME_STMT);
  g = gimple_build_assign (lhs, PLUS_EXPR, gimple_assign_lhs (g),
			   build_vector (type, cst));
  gsi_replace (gsi, g, false);
}

/* Return a type for the widest vector mode whose components are of type
   TYPE, or NULL_TREE if none is found.  */

static tree
type_for_widest_vector_mode (tree type, optab op)
{
  machine_mode inner_mode = TYPE_MODE (type);
  machine_mode best_mode = VOIDmode, mode;
  int best_nunits = 0;

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
  else
    mode = MIN_MODE_VECTOR_INT;

  for (; mode != VOIDmode; mode = GET_MODE_WIDER_MODE (mode))
    if (GET_MODE_INNER (mode) == inner_mode
        && GET_MODE_NUNITS (mode) > best_nunits
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
  elements = TYPE_VECTOR_SUBPARTS (vect_type);

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
	  tree size = TYPE_SIZE (vect_elt_type);
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
  unsigned int elements = TYPE_VECTOR_SUBPARTS (vect_type);
  vec<constructor_elt, va_gc> *v;
  tree constr, t, si, i_val;
  tree vec0tmp = NULL_TREE, vec1tmp = NULL_TREE, masktmp = NULL_TREE;
  bool two_operand_p = !operand_equal_p (vec0, vec1, 0);
  location_t loc = gimple_location (gsi_stmt (*gsi));
  unsigned i;

  if (TREE_CODE (mask) == SSA_NAME)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (mask);
      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == VECTOR_CST)
	mask = gimple_assign_rhs1 (def_stmt);
    }

  if (TREE_CODE (mask) == VECTOR_CST)
    {
      unsigned char *sel_int = XALLOCAVEC (unsigned char, elements);

      for (i = 0; i < elements; ++i)
	sel_int[i] = (TREE_INT_CST_LOW (VECTOR_CST_ELT (mask, i))
		      & (2 * elements - 1));

      if (can_vec_perm_p (TYPE_MODE (vect_type), false, sel_int))
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
	  && sel_int[0]
	  && sel_int[0] < elements)
	{
	  for (i = 1; i < elements; ++i)
	    {
	      unsigned int expected = i + sel_int[0];
	      /* Indices into the second vector are all equivalent.  */
	      if (MIN (elements, (unsigned) sel_int[i])
		  != MIN (elements, expected))
 		break;
	    }
	  if (i == elements)
	    {
	      gimple_assign_set_rhs3 (stmt, mask);
	      update_stmt (stmt);
	      return;
	    }
	}
    }
  else if (can_vec_perm_p (TYPE_MODE (vect_type), true, NULL))
    return;
  
  warning_at (loc, OPT_Wvector_operation_performance,
              "vector shuffling operation will be expanded piecewise");

  vec_alloc (v, elements);
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

      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, t);
    }

  constr = build_constructor (vect_type, v);
  gimple_assign_set_rhs_from_tree (gsi, constr);
  update_stmt (gsi_stmt (*gsi));
}

/* If OP is a uniform vector return the element it is a splat from.  */

static tree
ssa_uniform_vector_p (tree op)
{
  if (TREE_CODE (op) == VECTOR_CST
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
	  && (TYPE_VECTOR_SUBPARTS (vector_compute_type)
	      < TYPE_VECTOR_SUBPARTS (compute_type))
	  && TYPE_VECTOR_SUBPARTS (vector_compute_type) > 1
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

/* Helper function of expand_vector_operations_1.  Return number of
   vector elements for vector types or 1 for other types.  */

static inline int
count_type_subparts (tree type)
{
  return VECTOR_TYPE_P (type) ? TYPE_VECTOR_SUBPARTS (type) : 1;
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
  tree type = gimple_expr_type (stmt);
  tree compute_type = get_compute_type (COND_EXPR, mov_optab, type);
  machine_mode compute_mode = TYPE_MODE (compute_type);
  gcc_assert (compute_mode != BLKmode);
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree rhs3 = gimple_assign_rhs3 (stmt);
  tree new_rhs;

  /* If the compute mode is not a vector mode (hence we are not decomposing
     a BLKmode vector to smaller, hardware-supported vectors), we may want
     to expand the operations in parallel.  */
  if (GET_MODE_CLASS (compute_mode) != MODE_VECTOR_INT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_FLOAT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_FRACT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_UFRACT
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_ACCUM
      && GET_MODE_CLASS (compute_mode) != MODE_VECTOR_UACCUM)
    new_rhs = expand_vector_parallel (gsi, do_cond, type, rhs2, rhs3,
				      COND_EXPR);
  else
    new_rhs = expand_vector_piecewise (gsi, do_cond, type, compute_type,
				       rhs2, rhs3, COND_EXPR);
  if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (new_rhs)))
    new_rhs = gimplify_build1 (gsi, VIEW_CONVERT_EXPR, TREE_TYPE (lhs),
			       new_rhs);

  /* NOTE:  We should avoid using gimple_assign_set_rhs_from_tree. One
     way to do it is change expand_vector_operation and its callees to
     return a tree_code, RHS1 and RHS2 instead of a tree. */
  gimple_assign_set_rhs_from_tree (gsi, new_rhs);
  update_stmt (gsi_stmt (*gsi));
}

/* Process one statement.  If we identify a vector operation, expand it.  */

static void
expand_vector_operations_1 (gimple_stmt_iterator *gsi)
{
  tree lhs, rhs1, rhs2 = NULL, type, compute_type = NULL_TREE;
  enum tree_code code;
  optab op = unknown_optab;
  enum gimple_rhs_class rhs_class;
  tree new_rhs;

  /* Only consider code == GIMPLE_ASSIGN. */
  gassign *stmt = dyn_cast <gassign *> (gsi_stmt (*gsi));
  if (!stmt)
    return;

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
      expand_vector_condition (gsi);
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
  type = gimple_expr_type (stmt);
  if (rhs_class == GIMPLE_BINARY_RHS)
    rhs2 = gimple_assign_rhs2 (stmt);

  if (TREE_CODE (type) != VECTOR_TYPE)
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
	  tree slhs = make_ssa_name (TREE_TYPE (srhs1));
	  gimple *repl = gimple_build_assign (slhs, code, srhs1, srhs2);
	  gsi_insert_before (gsi, repl, GSI_SAME_STMT);
	  gimple_assign_set_rhs_from_tree (gsi,
					   build_vector_from_val (type, slhs));
	  update_stmt (stmt);
	  return;
	}
    }
 
  /* A scalar operation pretending to be a vector one.  */
  if (VECTOR_BOOLEAN_TYPE_P (type)
      && !VECTOR_MODE_P (TYPE_MODE (type))
      && TYPE_MODE (type) != BLKmode)
    return;

  if (CONVERT_EXPR_CODE_P (code)
      || code == FLOAT_EXPR
      || code == FIX_TRUNC_EXPR
      || code == VIEW_CONVERT_EXPR)
    return;

  /* The signedness is determined from input argument.  */
  if (code == VEC_UNPACK_FLOAT_HI_EXPR
      || code == VEC_UNPACK_FLOAT_LO_EXPR)
    type = TREE_TYPE (rhs1);

  /* For widening/narrowing vector operations, the relevant type is of the
     arguments, not the widened result.  VEC_UNPACK_FLOAT_*_EXPR is
     calculated in the same way above.  */
  if (code == WIDEN_SUM_EXPR
      || code == VEC_WIDEN_MULT_HI_EXPR
      || code == VEC_WIDEN_MULT_LO_EXPR
      || code == VEC_WIDEN_MULT_EVEN_EXPR
      || code == VEC_WIDEN_MULT_ODD_EXPR
      || code == VEC_UNPACK_HI_EXPR
      || code == VEC_UNPACK_LO_EXPR
      || code == VEC_PACK_TRUNC_EXPR
      || code == VEC_PACK_SAT_EXPR
      || code == VEC_PACK_FIX_TRUNC_EXPR
      || code == VEC_WIDEN_LSHIFT_HI_EXPR
      || code == VEC_WIDEN_LSHIFT_LO_EXPR)
    type = TREE_TYPE (rhs1);

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
	  if (count_type_subparts (compute_vtype)
	      > count_type_subparts (compute_type))
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
	      if (count_type_subparts (compute_lvtype)
		  > count_type_subparts (compute_ltype))
		{
		  compute_ltype = compute_lvtype;
		  opl = oplv;
		}
	      if (count_type_subparts (compute_rvtype)
		  > count_type_subparts (compute_rtype))
		{
		  compute_rtype = compute_rvtype;
		  opr = oprv;
		}
	      /* Pick the narrowest type from LSHIFT_EXPR, RSHIFT_EXPR and
		 BIT_IOR_EXPR.  */
	      compute_type = compute_ltype;
	      if (count_type_subparts (compute_type)
		  > count_type_subparts (compute_rtype))
		compute_type = compute_rtype;
	      if (count_type_subparts (compute_type)
		  > count_type_subparts (compute_otype))
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

  new_rhs = expand_vector_operation (gsi, type, compute_type, stmt, code);

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

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  expand_vector_operations_1 (&gsi);
	  /* ???  If we do not cleanup EH then we will ICE in
	     verification.  But in reality we have created wrong-code
	     as we did not properly transition EH info and edges to
	     the piecewise computations.  */
	  if (maybe_clean_eh_stmt (gsi_stmt (gsi))
	      && gimple_purge_dead_eh_edges (bb))
	    cfg_changed = true;
	}
    }

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
  virtual bool gate (function *fun)
    {
      return !(fun->curr_properties & PROP_gimple_lvec);
    }

  virtual unsigned int execute (function *)
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
  opt_pass * clone () { return new pass_lower_vector_ssa (m_ctxt); }
  virtual unsigned int execute (function *)
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
