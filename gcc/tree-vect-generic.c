/* Lower vector operations to scalar operations.
   Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.

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
#include "tree.h"
#include "tm.h"
#include "langhooks.h"
#include "tree-flow.h"
#include "gimple.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "flags.h"
#include "ggc.h"
#include "diagnostic.h"

/* Need to include rtl.h, expr.h, etc. for optabs.  */
#include "expr.h"
#include "optabs.h"


static void expand_vector_operations_1 (gimple_stmt_iterator *);


/* Build a constant of type TYPE, made of VALUE's bits replicated
   every TYPE_SIZE (INNER_TYPE) bits to fit TYPE's precision.  */
static tree
build_replicated_const (tree type, tree inner_type, HOST_WIDE_INT value)
{
  int width = tree_low_cst (TYPE_SIZE (inner_type), 1);
  int n = HOST_BITS_PER_WIDE_INT / width;
  unsigned HOST_WIDE_INT low, high, mask;
  tree ret;

  gcc_assert (n);

  if (width == HOST_BITS_PER_WIDE_INT)
    low = value;
  else
    {
      mask = ((HOST_WIDE_INT)1 << width) - 1;
      low = (unsigned HOST_WIDE_INT) ~0 / mask * (value & mask);
    }

  if (TYPE_PRECISION (type) < HOST_BITS_PER_WIDE_INT)
    low &= ((HOST_WIDE_INT)1 << TYPE_PRECISION (type)) - 1, high = 0;
  else if (TYPE_PRECISION (type) == HOST_BITS_PER_WIDE_INT)
    high = 0;
  else if (TYPE_PRECISION (type) == 2 * HOST_BITS_PER_WIDE_INT)
    high = low;
  else
    gcc_unreachable ();

  ret = build_int_cst_wide (type, low, high);
  return ret;
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
			      tree, tree, tree, tree, tree, enum tree_code);

static inline tree
tree_vec_extract (gimple_stmt_iterator *gsi, tree type,
		  tree t, tree bitsize, tree bitpos)
{
  if (bitpos)
    return gimplify_build3 (gsi, BIT_FIELD_REF, type, t, bitsize, bitpos);
  else
    return gimplify_build1 (gsi, VIEW_CONVERT_EXPR, type, t);
}

static tree
do_unop (gimple_stmt_iterator *gsi, tree inner_type, tree a,
	 tree b ATTRIBUTE_UNUSED, tree bitpos, tree bitsize,
	 enum tree_code code)
{
  a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  return gimplify_build1 (gsi, code, inner_type, a);
}

static tree
do_binop (gimple_stmt_iterator *gsi, tree inner_type, tree a, tree b,
	  tree bitpos, tree bitsize, enum tree_code code)
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
	  tree bitpos, tree bitsize, enum tree_code code)
{
  tree comp_type;

  a = tree_vec_extract (gsi, inner_type, a, bitsize, bitpos);
  b = tree_vec_extract (gsi, inner_type, b, bitsize, bitpos);

  comp_type = build_nonstandard_integer_type
		      (GET_MODE_BITSIZE (TYPE_MODE (inner_type)), 0);

  return gimplify_build3 (gsi, COND_EXPR, comp_type,
			  fold_build2 (code, boolean_type_node, a, b),
			  build_int_cst (comp_type, -1),
			  build_int_cst (comp_type, 0));
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
	       enum tree_code code)
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
	   enum tree_code code ATTRIBUTE_UNUSED)
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
  VEC(constructor_elt,gc) *v;
  tree part_width = TYPE_SIZE (inner_type);
  tree index = bitsize_int (0);
  int nunits = TYPE_VECTOR_SUBPARTS (type);
  int delta = tree_low_cst (part_width, 1)
	      / tree_low_cst (TYPE_SIZE (TREE_TYPE (type)), 1);
  int i;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  if (types_compatible_p (gimple_expr_type (gsi_stmt (*gsi)), type))
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector operation will be expanded piecewise");
  else
    warning_at (loc, OPT_Wvector_operation_performance,
		"vector operation will be expanded in parallel");

  v = VEC_alloc(constructor_elt, gc, (nunits + delta - 1) / delta);
  for (i = 0; i < nunits;
       i += delta, index = int_const_binop (PLUS_EXPR, index, part_width))
    {
      tree result = f (gsi, inner_type, a, b, index, part_width, code);
      constructor_elt *ce = VEC_quick_push (constructor_elt, v, NULL);
      ce->index = NULL_TREE;
      ce->value = result;
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
  enum machine_mode mode;
  int n_words = tree_low_cst (TYPE_SIZE_UNIT (type), 1) / UNITS_PER_WORD;
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
      mode = mode_for_size (tree_low_cst (TYPE_SIZE (type), 1), MODE_INT, 0);
      compute_type = lang_hooks.types.type_for_mode (mode, 1);
      result = f (gsi, compute_type, a, b, NULL_TREE, NULL_TREE, code);
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
	  	       / tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (type)), 1);

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

/* Check if vector VEC consists of all the equal elements and
   that the number of elements corresponds to the type of VEC.
   The function returns first element of the vector
   or NULL_TREE if the vector is not uniform.  */
static tree
uniform_vector_p (tree vec)
{
  tree first, t, els;
  unsigned i;

  if (vec == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (vec) == VECTOR_CST)
    {
      els = TREE_VECTOR_CST_ELTS (vec);
      first = TREE_VALUE (els);
      els = TREE_CHAIN (els);

      for (t = els; t; t = TREE_CHAIN (t))
	if (!operand_equal_p (first, TREE_VALUE (t), 0))
	  return NULL_TREE;

      return first;
    }

  else if (TREE_CODE (vec) == CONSTRUCTOR)
    {
      first = error_mark_node;

      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (vec), i, t)
        {
          if (i == 0)
            {
              first = t;
              continue;
            }
	  if (!operand_equal_p (first, t, 0))
	    return NULL_TREE;
        }
      if (i != TYPE_VECTOR_SUBPARTS (TREE_TYPE (vec)))
	return NULL_TREE;

      return first;
    }

  return NULL_TREE;
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
  if (! expand_vec_cond_expr_p (type, TREE_TYPE (op0)))
    t = expand_vector_piecewise (gsi, do_compare, type,
				 TREE_TYPE (TREE_TYPE (op0)), op0, op1, code);
  else
    t = NULL_TREE;

  return t;
}

static tree
expand_vector_operation (gimple_stmt_iterator *gsi, tree type, tree compute_type,
			 gimple assign, enum tree_code code)
{
  enum machine_mode compute_mode = TYPE_MODE (compute_type);

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
        if (!TYPE_OVERFLOW_TRAPS (type))
	  return expand_vector_addition (gsi, do_binop, do_plus_minus, type,
					 gimple_assign_rhs1 (assign),
					 gimple_assign_rhs2 (assign), code);
	break;

      case NEGATE_EXPR:
        if (!TYPE_OVERFLOW_TRAPS (type))
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

/* Return a type for the widest vector mode whose components are of mode
   INNER_MODE, or NULL_TREE if none is found.
   SATP is true for saturating fixed-point types.  */

static tree
type_for_widest_vector_mode (enum machine_mode inner_mode, optab op, int satp)
{
  enum machine_mode best_mode = VOIDmode, mode;
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
    {
      /* For fixed-point modes, we need to pass satp as the 2nd parameter.  */
      if (ALL_FIXED_POINT_MODE_P (best_mode))
	return lang_hooks.types.type_for_mode (best_mode, satp);

      return lang_hooks.types.type_for_mode (best_mode, 1);
    }
}


/* Build a reference to the element of the vector VECT.  Function
   returns either the element itself, either BIT_FIELD_REF, or an
   ARRAY_REF expression.

   GSI is requred to insert temporary variables while building a
   refernece to the element of the vector VECT.

   PTMPVEC is a pointer to the temporary variable for caching
   purposes.  In case when PTMPVEC is NULL new temporary variable
   will be created.  */
static tree
vector_element (gimple_stmt_iterator *gsi, tree vect, tree idx, tree *ptmpvec)
{
  tree vect_type, vect_elt_type;
  gimple asgn;
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
      if (!host_integerp (idx, 1) || index >= elements)
	{
	  index &= elements - 1;
	  idx = build_int_cst (TREE_TYPE (idx), index);
	}

      /* When lowering a vector statement sequence do some easy
         simplification by looking through intermediate vector results.  */
      if (TREE_CODE (vect) == SSA_NAME)
	{
	  gimple def_stmt = SSA_NAME_DEF_STMT (vect);
	  if (is_gimple_assign (def_stmt)
	      && (gimple_assign_rhs_code (def_stmt) == VECTOR_CST
		  || gimple_assign_rhs_code (def_stmt) == CONSTRUCTOR))
	    vect = gimple_assign_rhs1 (def_stmt);
	}

      if (TREE_CODE (vect) == VECTOR_CST)
        {
	  unsigned i;
	  tree vals = TREE_VECTOR_CST_ELTS (vect);
	  for (i = 0; vals; vals = TREE_CHAIN (vals), ++i)
	    if (i == index)
	       return TREE_VALUE (vals);
	  return build_zero_cst (vect_elt_type);
        }
      else if (TREE_CODE (vect) == CONSTRUCTOR)
        {
          unsigned i;
          tree elt_i, elt_v;

	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (vect), i, elt_i, elt_v)
            if (operand_equal_p (elt_i, idx, 0))
              return elt_v;
          return build_zero_cst (vect_elt_type);
        }
      else
        {
	  tree size = TYPE_SIZE (vect_elt_type);
          tree pos = fold_build2 (MULT_EXPR, TREE_TYPE (idx), idx, size);
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
  gimple stmt = gsi_stmt (*gsi);
  tree mask = gimple_assign_rhs3 (stmt);
  tree vec0 = gimple_assign_rhs1 (stmt);
  tree vec1 = gimple_assign_rhs2 (stmt);
  tree vect_type = TREE_TYPE (vec0);
  tree mask_type = TREE_TYPE (mask);
  tree vect_elt_type = TREE_TYPE (vect_type);
  tree mask_elt_type = TREE_TYPE (mask_type);
  unsigned int elements = TYPE_VECTOR_SUBPARTS (vect_type);
  VEC(constructor_elt,gc) *v;
  tree constr, t, si, i_val;
  tree vec0tmp = NULL_TREE, vec1tmp = NULL_TREE, masktmp = NULL_TREE;
  bool two_operand_p = !operand_equal_p (vec0, vec1, 0);
  location_t loc = gimple_location (gsi_stmt (*gsi));
  unsigned i;

  if (TREE_CODE (mask) == VECTOR_CST)
    {
      unsigned char *sel_int = XALLOCAVEC (unsigned char, elements);
      tree vals = TREE_VECTOR_CST_ELTS (mask);

      for (i = 0; i < elements; ++i, vals = TREE_CHAIN (vals))
	sel_int[i] = TREE_INT_CST_LOW (TREE_VALUE (vals)) & (2 * elements - 1);

      if (can_vec_perm_p (TYPE_MODE (vect_type), false, sel_int))
	return;
    }
  else if (can_vec_perm_p (TYPE_MODE (vect_type), true, NULL))
    return;
  
  warning_at (loc, OPT_Wvector_operation_performance,
              "vector shuffling operation will be expanded piecewise");

  v = VEC_alloc (constructor_elt, gc, elements);
  for (i = 0; i < elements; i++)
    {
      si = size_int (i);
      i_val = vector_element (gsi, mask, si, &masktmp);

      if (TREE_CODE (i_val) == INTEGER_CST)
        {
	  unsigned HOST_WIDE_INT index;

	  index = TREE_INT_CST_LOW (i_val);
	  if (!host_integerp (i_val, 1) || index >= elements)
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

      CONSTRUCTOR_APPEND_ELT (v, si, t);
    }

  constr = build_constructor (vect_type, v);
  gimple_assign_set_rhs_from_tree (gsi, constr);
  update_stmt (gsi_stmt (*gsi));
}

/* Process one statement.  If we identify a vector operation, expand it.  */

static void
expand_vector_operations_1 (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree lhs, rhs1, rhs2 = NULL, type, compute_type;
  enum tree_code code;
  enum machine_mode compute_mode;
  optab op = NULL;
  enum gimple_rhs_class rhs_class;
  tree new_rhs;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  code = gimple_assign_rhs_code (stmt);
  rhs_class = get_gimple_rhs_class (code);
  lhs = gimple_assign_lhs (stmt);

  if (code == VEC_PERM_EXPR)
    {
      lower_vec_perm (gsi);
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

  if (code == NOP_EXPR
      || code == FLOAT_EXPR
      || code == FIX_TRUNC_EXPR
      || code == VIEW_CONVERT_EXPR)
    return;

  /* These are only created by the vectorizer, after having queried
     the target support.  It's more than just looking at the optab,
     and there's no need to do it again.  */
  if (code == VEC_EXTRACT_EVEN_EXPR
      || code == VEC_EXTRACT_ODD_EXPR)
    return;

  gcc_assert (code != CONVERT_EXPR);

  /* The signedness is determined from input argument.  */
  if (code == VEC_UNPACK_FLOAT_HI_EXPR
      || code == VEC_UNPACK_FLOAT_LO_EXPR)
    type = TREE_TYPE (rhs1);

  /* Choose between vector shift/rotate by vector and vector shift/rotate by
     scalar */
  if (code == LSHIFT_EXPR
      || code == RSHIFT_EXPR
      || code == LROTATE_EXPR
      || code == RROTATE_EXPR)
    {
      /* Check whether we have vector <op> {x,x,x,x} where x
         could be a scalar variable or a constant.  Transform
         vector <op> {x,x,x,x} ==> vector <op> scalar.  */
      if (VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (rhs2))))
        {
          tree first;
          gimple def_stmt;

          if ((TREE_CODE (rhs2) == VECTOR_CST
	       && (first = uniform_vector_p (rhs2)) != NULL_TREE)
	      || (TREE_CODE (rhs2) == SSA_NAME
		  && (def_stmt = SSA_NAME_DEF_STMT (rhs2))
		  && gimple_assign_single_p (def_stmt)
		  && (first = uniform_vector_p
		      (gimple_assign_rhs1 (def_stmt))) != NULL_TREE))
            {
              gimple_assign_set_rhs2 (stmt, first);
              update_stmt (stmt);
              rhs2 = first;
            }
        }

      if (VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (rhs2))))
        op = optab_for_tree_code (code, type, optab_vector);
      else
	{
          op = optab_for_tree_code (code, type, optab_scalar);

	  /* The rtl expander will expand vector/scalar as vector/vector
	     if necessary.  Don't bother converting the stmt here.  */
	  if (op == NULL
	      || optab_handler (op, TYPE_MODE (type)) == CODE_FOR_nothing)
	    op = optab_for_tree_code (code, type, optab_vector);
	}
    }
  else
    op = optab_for_tree_code (code, type, optab_default);

  /* For widening/narrowing vector operations, the relevant type is of the
     arguments, not the widened result.  VEC_UNPACK_FLOAT_*_EXPR is
     calculated in the same way above.  */
  if (code == WIDEN_SUM_EXPR
      || code == VEC_WIDEN_MULT_HI_EXPR
      || code == VEC_WIDEN_MULT_LO_EXPR
      || code == VEC_UNPACK_HI_EXPR
      || code == VEC_UNPACK_LO_EXPR
      || code == VEC_PACK_TRUNC_EXPR
      || code == VEC_PACK_SAT_EXPR
      || code == VEC_PACK_FIX_TRUNC_EXPR
      || code == VEC_WIDEN_LSHIFT_HI_EXPR
      || code == VEC_WIDEN_LSHIFT_LO_EXPR)
    type = TREE_TYPE (rhs1);

  /* Optabs will try converting a negation into a subtraction, so
     look for it as well.  TODO: negation of floating-point vectors
     might be turned into an exclusive OR toggling the sign bit.  */
  if (op == NULL
      && code == NEGATE_EXPR
      && INTEGRAL_TYPE_P (TREE_TYPE (type)))
    op = optab_for_tree_code (MINUS_EXPR, type, optab_default);

  /* For very wide vectors, try using a smaller vector mode.  */
  compute_type = type;
  if (TYPE_MODE (type) == BLKmode && op)
    {
      tree vector_compute_type
        = type_for_widest_vector_mode (TYPE_MODE (TREE_TYPE (type)), op,
				       TYPE_SATURATING (TREE_TYPE (type)));
      if (vector_compute_type != NULL_TREE
	  && (TYPE_VECTOR_SUBPARTS (vector_compute_type)
	      < TYPE_VECTOR_SUBPARTS (compute_type)))
	compute_type = vector_compute_type;
    }

  /* If we are breaking a BLKmode vector into smaller pieces,
     type_for_widest_vector_mode has already looked into the optab,
     so skip these checks.  */
  if (compute_type == type)
    {
      compute_mode = TYPE_MODE (compute_type);
      if (VECTOR_MODE_P (compute_mode)
          && op != NULL
	  && optab_handler (op, compute_mode) != CODE_FOR_nothing)
	return;
      else
	/* There is no operation in hardware, so fall back to scalars.  */
	compute_type = TREE_TYPE (type);
    }

  gcc_assert (code != VEC_LSHIFT_EXPR && code != VEC_RSHIFT_EXPR);
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

static bool
gate_expand_vector_operations_ssa (void)
{
  return optimize == 0;
}

static unsigned int
expand_vector_operations (void)
{
  gimple_stmt_iterator gsi;
  basic_block bb;
  bool cfg_changed = false;

  FOR_EACH_BB (bb)
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

struct gimple_opt_pass pass_lower_vector =
{
 {
  GIMPLE_PASS,
  "veclower",				/* name */
  gate_expand_vector_operations_ssa,    /* gate */
  expand_vector_operations,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa	                /* todo_flags_finish */
    | TODO_verify_ssa
    | TODO_verify_stmts | TODO_verify_flow
    | TODO_cleanup_cfg
 }
};

struct gimple_opt_pass pass_lower_vector_ssa =
{
 {
  GIMPLE_PASS,
  "veclower2",				/* name */
  0,	                                /* gate */
  expand_vector_operations,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_NONE,				/* tv_id */
  PROP_cfg,				/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_update_ssa	                /* todo_flags_finish */
    | TODO_verify_ssa
    | TODO_verify_stmts | TODO_verify_flow
    | TODO_cleanup_cfg
 }
};

#include "gt-tree-vect-generic.h"
