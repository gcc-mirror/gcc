/* Analysis Utilities for Loop Vectorization.
   Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Dorit Nuzman <dorit@il.ibm.com>

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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"
#include "basic-block.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "expr.h"
#include "optabs.h"
#include "params.h"
#include "tree-data-ref.h"
#include "tree-vectorizer.h"
#include "recog.h"
#include "diagnostic-core.h"

/* Pattern recognition functions  */
static gimple vect_recog_widen_sum_pattern (VEC (gimple, heap) **, tree *,
					    tree *);
static gimple vect_recog_widen_mult_pattern (VEC (gimple, heap) **, tree *,
					     tree *);
static gimple vect_recog_dot_prod_pattern (VEC (gimple, heap) **, tree *,
					   tree *);
static gimple vect_recog_pow_pattern (VEC (gimple, heap) **, tree *, tree *);
static gimple vect_recog_over_widening_pattern (VEC (gimple, heap) **, tree *,
                                                 tree *);
static vect_recog_func_ptr vect_vect_recog_func_ptrs[NUM_PATTERNS] = {
	vect_recog_widen_mult_pattern,
	vect_recog_widen_sum_pattern,
	vect_recog_dot_prod_pattern,
	vect_recog_pow_pattern,
        vect_recog_over_widening_pattern};


/* Function widened_name_p

   Check whether NAME, an ssa-name used in USE_STMT,
   is a result of a type-promotion, such that:
     DEF_STMT: NAME = NOP (name0)
   where the type of name0 (HALF_TYPE) is smaller than the type of NAME.
   If CHECK_SIGN is TRUE, check that either both types are signed or both are
   unsigned.  */

static bool
widened_name_p (tree name, gimple use_stmt, tree *half_type, gimple *def_stmt,
		bool check_sign)
{
  tree dummy;
  gimple dummy_gimple;
  loop_vec_info loop_vinfo;
  stmt_vec_info stmt_vinfo;
  tree type = TREE_TYPE (name);
  tree oprnd0;
  enum vect_def_type dt;
  tree def;

  stmt_vinfo = vinfo_for_stmt (use_stmt);
  loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);

  if (!vect_is_simple_use (name, loop_vinfo, NULL, def_stmt, &def, &dt))
    return false;

  if (dt != vect_internal_def
      && dt != vect_external_def && dt != vect_constant_def)
    return false;

  if (! *def_stmt)
    return false;

  if (!is_gimple_assign (*def_stmt))
    return false;

  if (gimple_assign_rhs_code (*def_stmt) != NOP_EXPR)
    return false;

  oprnd0 = gimple_assign_rhs1 (*def_stmt);

  *half_type = TREE_TYPE (oprnd0);
  if (!INTEGRAL_TYPE_P (type) || !INTEGRAL_TYPE_P (*half_type)
      || ((TYPE_UNSIGNED (type) != TYPE_UNSIGNED (*half_type)) && check_sign)
      || (TYPE_PRECISION (type) < (TYPE_PRECISION (*half_type) * 2)))
    return false;

  if (!vect_is_simple_use (oprnd0, loop_vinfo, NULL, &dummy_gimple, &dummy,
                           &dt))
    return false;

  return true;
}

/* Helper to return a new temporary for pattern of TYPE for STMT.  If STMT
   is NULL, the caller must set SSA_NAME_DEF_STMT for the returned SSA var. */

static tree
vect_recog_temp_ssa_var (tree type, gimple stmt)
{
  tree var = create_tmp_var (type, "patt");

  add_referenced_var (var);
  var = make_ssa_name (var, stmt);
  return var;
}

/* Function vect_recog_dot_prod_pattern

   Try to find the following pattern:

     type x_t, y_t;
     TYPE1 prod;
     TYPE2 sum = init;
   loop:
     sum_0 = phi <init, sum_1>
     S1  x_t = ...
     S2  y_t = ...
     S3  x_T = (TYPE1) x_t;
     S4  y_T = (TYPE1) y_t;
     S5  prod = x_T * y_T;
     [S6  prod = (TYPE2) prod;  #optional]
     S7  sum_1 = prod + sum_0;

   where 'TYPE1' is exactly double the size of type 'type', and 'TYPE2' is the
   same size of 'TYPE1' or bigger. This is a special case of a reduction
   computation.

   Input:

   * STMTS: Contains a stmt from which the pattern search begins.  In the
   example, when this function is called with S7, the pattern {S3,S4,S5,S6,S7}
   will be detected.

   Output:

   * TYPE_IN: The type of the input arguments to the pattern.

   * TYPE_OUT: The type of the output  of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        WIDEN_DOT_PRODUCT <x_t, y_t, sum_0>

   Note: The dot-prod idiom is a widening reduction pattern that is
         vectorized without preserving all the intermediate results. It
         produces only N/2 (widened) results (by summing up pairs of
         intermediate results) rather than all N results.  Therefore, we
         cannot allow this pattern when we want to get all the results and in
         the correct order (as is the case when this computation is in an
         inner-loop nested in an outer-loop that us being vectorized).  */

static gimple
vect_recog_dot_prod_pattern (VEC (gimple, heap) **stmts, tree *type_in,
			     tree *type_out)
{
  gimple stmt, last_stmt = VEC_index (gimple, *stmts, 0);
  tree oprnd0, oprnd1;
  tree oprnd00, oprnd01;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  tree type, half_type;
  gimple pattern_stmt;
  tree prod_type;
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_info);
  tree var;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  type = gimple_expr_type (last_stmt);

  /* Look for the following pattern
          DX = (TYPE1) X;
          DY = (TYPE1) Y;
          DPROD = DX * DY;
          DDPROD = (TYPE2) DPROD;
          sum_1 = DDPROD + sum_0;
     In which
     - DX is double the size of X
     - DY is double the size of Y
     - DX, DY, DPROD all have the same type
     - sum is the same size of DPROD or bigger
     - sum has been recognized as a reduction variable.

     This is equivalent to:
       DPROD = X w* Y;          #widen mult
       sum_1 = DPROD w+ sum_0;  #widen summation
     or
       DPROD = X w* Y;          #widen mult
       sum_1 = DPROD + sum_0;   #summation
   */

  /* Starting from LAST_STMT, follow the defs of its uses in search
     of the above pattern.  */

  if (gimple_assign_rhs_code (last_stmt) != PLUS_EXPR)
    return NULL;

  if (STMT_VINFO_IN_PATTERN_P (stmt_vinfo))
    {
      /* Has been detected as widening-summation?  */

      stmt = STMT_VINFO_RELATED_STMT (stmt_vinfo);
      type = gimple_expr_type (stmt);
      if (gimple_assign_rhs_code (stmt) != WIDEN_SUM_EXPR)
        return NULL;
      oprnd0 = gimple_assign_rhs1 (stmt);
      oprnd1 = gimple_assign_rhs2 (stmt);
      half_type = TREE_TYPE (oprnd0);
    }
  else
    {
      gimple def_stmt;

      if (STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_reduction_def)
        return NULL;
      oprnd0 = gimple_assign_rhs1 (last_stmt);
      oprnd1 = gimple_assign_rhs2 (last_stmt);
      if (!types_compatible_p (TREE_TYPE (oprnd0), type)
	  || !types_compatible_p (TREE_TYPE (oprnd1), type))
        return NULL;
      stmt = last_stmt;

      if (widened_name_p (oprnd0, stmt, &half_type, &def_stmt, true))
        {
          stmt = def_stmt;
          oprnd0 = gimple_assign_rhs1 (stmt);
        }
      else
        half_type = type;
    }

  /* So far so good.  Since last_stmt was detected as a (summation) reduction,
     we know that oprnd1 is the reduction variable (defined by a loop-header
     phi), and oprnd0 is an ssa-name defined by a stmt in the loop body.
     Left to check that oprnd0 is defined by a (widen_)mult_expr  */

  prod_type = half_type;
  stmt = SSA_NAME_DEF_STMT (oprnd0);

  /* It could not be the dot_prod pattern if the stmt is outside the loop.  */
  if (!gimple_bb (stmt) || !flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
    return NULL;

  /* FORNOW.  Can continue analyzing the def-use chain when this stmt in a phi
     inside the loop (in case we are analyzing an outer-loop).  */
  if (!is_gimple_assign (stmt))
    return NULL;
  stmt_vinfo = vinfo_for_stmt (stmt);
  gcc_assert (stmt_vinfo);
  if (STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_internal_def)
    return NULL;
  if (gimple_assign_rhs_code (stmt) != MULT_EXPR)
    return NULL;
  if (STMT_VINFO_IN_PATTERN_P (stmt_vinfo))
    {
      /* Has been detected as a widening multiplication?  */

      stmt = STMT_VINFO_RELATED_STMT (stmt_vinfo);
      if (gimple_assign_rhs_code (stmt) != WIDEN_MULT_EXPR)
        return NULL;
      stmt_vinfo = vinfo_for_stmt (stmt);
      gcc_assert (stmt_vinfo);
      gcc_assert (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_internal_def);
      oprnd00 = gimple_assign_rhs1 (stmt);
      oprnd01 = gimple_assign_rhs2 (stmt);
    }
  else
    {
      tree half_type0, half_type1;
      gimple def_stmt;
      tree oprnd0, oprnd1;

      oprnd0 = gimple_assign_rhs1 (stmt);
      oprnd1 = gimple_assign_rhs2 (stmt);
      if (!types_compatible_p (TREE_TYPE (oprnd0), prod_type)
          || !types_compatible_p (TREE_TYPE (oprnd1), prod_type))
        return NULL;
      if (!widened_name_p (oprnd0, stmt, &half_type0, &def_stmt, true))
        return NULL;
      oprnd00 = gimple_assign_rhs1 (def_stmt);
      if (!widened_name_p (oprnd1, stmt, &half_type1, &def_stmt, true))
        return NULL;
      oprnd01 = gimple_assign_rhs1 (def_stmt);
      if (!types_compatible_p (half_type0, half_type1))
        return NULL;
      if (TYPE_PRECISION (prod_type) != TYPE_PRECISION (half_type0) * 2)
	return NULL;
    }

  half_type = TREE_TYPE (oprnd00);
  *type_in = half_type;
  *type_out = type;

  /* Pattern detected. Create a stmt to be used to replace the pattern: */
  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign_with_ops3 (DOT_PROD_EXPR, var,
						oprnd00, oprnd01, oprnd1);

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "vect_recog_dot_prod_pattern: detected: ");
      print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);
    }

  /* We don't allow changing the order of the computation in the inner-loop
     when doing outer-loop vectorization.  */
  gcc_assert (!nested_in_vect_loop_p (loop, last_stmt));

  return pattern_stmt;
}


/* Handle two cases of multiplication by a constant.  The first one is when
   the constant, CONST_OPRND, fits the type (HALF_TYPE) of the second
   operand (OPRND).  In that case, we can peform widen-mult from HALF_TYPE to
   TYPE.

   Otherwise, if the type of the result (TYPE) is at least 4 times bigger than
   HALF_TYPE, and CONST_OPRND fits an intermediate type (2 times smaller than
   TYPE), we can perform widen-mult from the intermediate type to TYPE and
   replace a_T = (TYPE) a_t; with a_it - (interm_type) a_t;  */

static bool
vect_handle_widen_mult_by_const (tree const_oprnd, tree *oprnd,
   			         VEC (gimple, heap) **stmts, tree type,
			         tree *half_type, gimple def_stmt)
{
  tree new_type, new_oprnd, tmp;
  gimple new_stmt;

  if (int_fits_type_p (const_oprnd, *half_type))
    {
      /* CONST_OPRND is a constant of HALF_TYPE.  */
      *oprnd = gimple_assign_rhs1 (def_stmt);
      return true;
    }

  if (TYPE_PRECISION (type) < (TYPE_PRECISION (*half_type) * 4)
      || !vinfo_for_stmt (def_stmt))
    return false;

  /* TYPE is 4 times bigger than HALF_TYPE, try widen-mult for
     a type 2 times bigger than HALF_TYPE.  */
  new_type = build_nonstandard_integer_type (TYPE_PRECISION (type) / 2,
                                             TYPE_UNSIGNED (type));
  if (!int_fits_type_p (const_oprnd, new_type))
    return false;

  /* Use NEW_TYPE for widen_mult.  */
  if (STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt)))
    {
      new_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt));
      /* Check if the already created pattern stmt is what we need.  */
      if (!is_gimple_assign (new_stmt)
          || gimple_assign_rhs_code (new_stmt) != NOP_EXPR
          || TREE_TYPE (gimple_assign_lhs (new_stmt)) != new_type)
        return false;

      *oprnd = gimple_assign_lhs (new_stmt);
    }
  else
    {
      /* Create a_T = (NEW_TYPE) a_t;  */
      *oprnd = gimple_assign_rhs1 (def_stmt);
      tmp = create_tmp_var (new_type, NULL);
      add_referenced_var (tmp);
      new_oprnd = make_ssa_name (tmp, NULL);
      new_stmt = gimple_build_assign_with_ops (NOP_EXPR, new_oprnd, *oprnd,
					       NULL_TREE);
      SSA_NAME_DEF_STMT (new_oprnd) = new_stmt;
      STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt)) = new_stmt;
      VEC_safe_push (gimple, heap, *stmts, def_stmt);
      *oprnd = new_oprnd;
    }

  *half_type = new_type;
  return true;
}


/* Function vect_recog_widen_mult_pattern

   Try to find the following pattern:

     type a_t, b_t;
     TYPE a_T, b_T, prod_T;

     S1  a_t = ;
     S2  b_t = ;
     S3  a_T = (TYPE) a_t;
     S4  b_T = (TYPE) b_t;
     S5  prod_T = a_T * b_T;

   where type 'TYPE' is at least double the size of type 'type'.

   Also detect unsgigned cases:

     unsigned type a_t, b_t;
     unsigned TYPE u_prod_T;
     TYPE a_T, b_T, prod_T;

     S1  a_t = ;
     S2  b_t = ;
     S3  a_T = (TYPE) a_t;
     S4  b_T = (TYPE) b_t;
     S5  prod_T = a_T * b_T;
     S6  u_prod_T = (unsigned TYPE) prod_T;

   and multiplication by constants:

     type a_t;
     TYPE a_T, prod_T;

     S1  a_t = ;
     S3  a_T = (TYPE) a_t;
     S5  prod_T = a_T * CONST;

   A special case of multiplication by constants is when 'TYPE' is 4 times
   bigger than 'type', but CONST fits an intermediate type 2 times smaller
   than 'TYPE'.  In that case we create an additional pattern stmt for S3
   to create a variable of the intermediate type, and perform widen-mult
   on the intermediate type as well:

     type a_t;
     interm_type a_it;
     TYPE a_T, prod_T,  prod_T';

     S1  a_t = ;
     S3  a_T = (TYPE) a_t;
           '--> a_it = (interm_type) a_t;
     S5  prod_T = a_T * CONST;
           '--> prod_T' = a_it w* CONST;

   Input/Output:

   * STMTS: Contains a stmt from which the pattern search begins.  In the
   example, when this function is called with S5, the pattern {S3,S4,S5,(S6)}
   is detected.  In case of unsigned widen-mult, the original stmt (S5) is
   replaced with S6 in STMTS.  In case of multiplication by a constant
   of an intermediate type (the last case above), STMTS also contains S3
   (inserted before S5).

   Output:

   * TYPE_IN: The type of the input arguments to the pattern.

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern.  In this case it will be:
        WIDEN_MULT <a_t, b_t>
*/

static gimple
vect_recog_widen_mult_pattern (VEC (gimple, heap) **stmts,
                               tree *type_in, tree *type_out)
{
  gimple last_stmt = VEC_pop (gimple, *stmts);
  gimple def_stmt0, def_stmt1;
  tree oprnd0, oprnd1;
  tree type, half_type0, half_type1;
  gimple pattern_stmt;
  tree vectype, vectype_out = NULL_TREE;
  tree dummy;
  tree var;
  enum tree_code dummy_code;
  int dummy_int;
  VEC (tree, heap) *dummy_vec;
  bool op0_ok, op1_ok;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  type = gimple_expr_type (last_stmt);

  /* Starting from LAST_STMT, follow the defs of its uses in search
     of the above pattern.  */

  if (gimple_assign_rhs_code (last_stmt) != MULT_EXPR)
    return NULL;

  oprnd0 = gimple_assign_rhs1 (last_stmt);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  if (!types_compatible_p (TREE_TYPE (oprnd0), type)
      || !types_compatible_p (TREE_TYPE (oprnd1), type))
    return NULL;

  /* Check argument 0.  */
  op0_ok = widened_name_p (oprnd0, last_stmt, &half_type0, &def_stmt0, false);
  /* Check argument 1.  */
  op1_ok = widened_name_p (oprnd1, last_stmt, &half_type1, &def_stmt1, false);

  /* In case of multiplication by a constant one of the operands may not match
     the pattern, but not both.  */
  if (!op0_ok && !op1_ok)
    return NULL;

  if (op0_ok && op1_ok)
    {
      oprnd0 = gimple_assign_rhs1 (def_stmt0);
      oprnd1 = gimple_assign_rhs1 (def_stmt1);
    }	       
  else if (!op0_ok)
    {
      if (TREE_CODE (oprnd0) == INTEGER_CST
	  && TREE_CODE (half_type1) == INTEGER_TYPE
          && vect_handle_widen_mult_by_const (oprnd0, &oprnd1, stmts, type,
				 	      &half_type1, def_stmt1))
        half_type0 = half_type1;
      else
	return NULL;
    }
  else if (!op1_ok)
    {
      if (TREE_CODE (oprnd1) == INTEGER_CST
          && TREE_CODE (half_type0) == INTEGER_TYPE
          && vect_handle_widen_mult_by_const (oprnd1, &oprnd0, stmts, type,
					      &half_type0, def_stmt0))
        half_type1 = half_type0;
      else
        return NULL;
    }

  /* Handle unsigned case.  Look for
     S6  u_prod_T = (unsigned TYPE) prod_T;
     Use unsigned TYPE as the type for WIDEN_MULT_EXPR.  */
  if (TYPE_UNSIGNED (type) != TYPE_UNSIGNED (half_type0))
    {
      tree lhs = gimple_assign_lhs (last_stmt), use_lhs;
      imm_use_iterator imm_iter;
      use_operand_p use_p;
      int nuses = 0;
      gimple use_stmt = NULL;
      tree use_type;

      if (TYPE_UNSIGNED (type) == TYPE_UNSIGNED (half_type1))
        return NULL;

      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
        {
	  if (is_gimple_debug (USE_STMT (use_p)))
	    continue;
          use_stmt = USE_STMT (use_p);
          nuses++;
        }

      if (nuses != 1 || !is_gimple_assign (use_stmt)
          || gimple_assign_rhs_code (use_stmt) != NOP_EXPR)
        return NULL;

      use_lhs = gimple_assign_lhs (use_stmt);
      use_type = TREE_TYPE (use_lhs);
      if (!INTEGRAL_TYPE_P (use_type)
          || (TYPE_UNSIGNED (type) == TYPE_UNSIGNED (use_type))
          || (TYPE_PRECISION (type) != TYPE_PRECISION (use_type)))
        return NULL;

      type = use_type;
      last_stmt = use_stmt;
    }

  if (!types_compatible_p (half_type0, half_type1))
    return NULL;

  /* Pattern detected.  */
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "vect_recog_widen_mult_pattern: detected: ");

  /* Check target support  */
  vectype = get_vectype_for_scalar_type (half_type0);
  vectype_out = get_vectype_for_scalar_type (type);
  if (!vectype
      || !vectype_out
      || !supportable_widening_operation (WIDEN_MULT_EXPR, last_stmt,
					  vectype_out, vectype,
					  &dummy, &dummy, &dummy_code,
					  &dummy_code, &dummy_int, &dummy_vec))
    return NULL;

  *type_in = vectype;
  *type_out = vectype_out;

  /* Pattern supported. Create a stmt to be used to replace the pattern: */
  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign_with_ops (WIDEN_MULT_EXPR, var, oprnd0,
					       oprnd1);
  SSA_NAME_DEF_STMT (var) = pattern_stmt;

  if (vect_print_dump_info (REPORT_DETAILS))
    print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);

  VEC_safe_push (gimple, heap, *stmts, last_stmt);
  return pattern_stmt;
}


/* Function vect_recog_pow_pattern

   Try to find the following pattern:

     x = POW (y, N);

   with POW being one of pow, powf, powi, powif and N being
   either 2 or 0.5.

   Input:

   * LAST_STMT: A stmt from which the pattern search begins.

   Output:

   * TYPE_IN: The type of the input arguments to the pattern.

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        x = x * x
   or
	x = sqrt (x)
*/

static gimple
vect_recog_pow_pattern (VEC (gimple, heap) **stmts, tree *type_in,
			tree *type_out)
{
  gimple last_stmt = VEC_index (gimple, *stmts, 0);
  tree fn, base, exp = NULL;
  gimple stmt;
  tree var;

  if (!is_gimple_call (last_stmt) || gimple_call_lhs (last_stmt) == NULL)
    return NULL;

  fn = gimple_call_fndecl (last_stmt);
  if (fn == NULL_TREE || DECL_BUILT_IN_CLASS (fn) != BUILT_IN_NORMAL)
   return NULL;

  switch (DECL_FUNCTION_CODE (fn))
    {
    case BUILT_IN_POWIF:
    case BUILT_IN_POWI:
    case BUILT_IN_POWF:
    case BUILT_IN_POW:
      base = gimple_call_arg (last_stmt, 0);
      exp = gimple_call_arg (last_stmt, 1);
      if (TREE_CODE (exp) != REAL_CST
	  && TREE_CODE (exp) != INTEGER_CST)
        return NULL;
      break;

    default:
      return NULL;
    }

  /* We now have a pow or powi builtin function call with a constant
     exponent.  */

  *type_out = NULL_TREE;

  /* Catch squaring.  */
  if ((host_integerp (exp, 0)
       && tree_low_cst (exp, 0) == 2)
      || (TREE_CODE (exp) == REAL_CST
          && REAL_VALUES_EQUAL (TREE_REAL_CST (exp), dconst2)))
    {
      *type_in = TREE_TYPE (base);

      var = vect_recog_temp_ssa_var (TREE_TYPE (base), NULL);
      stmt = gimple_build_assign_with_ops (MULT_EXPR, var, base, base);
      SSA_NAME_DEF_STMT (var) = stmt;
      return stmt;
    }

  /* Catch square root.  */
  if (TREE_CODE (exp) == REAL_CST
      && REAL_VALUES_EQUAL (TREE_REAL_CST (exp), dconsthalf))
    {
      tree newfn = mathfn_built_in (TREE_TYPE (base), BUILT_IN_SQRT);
      *type_in = get_vectype_for_scalar_type (TREE_TYPE (base));
      if (*type_in)
	{
	  gimple stmt = gimple_build_call (newfn, 1, base);
	  if (vectorizable_function (stmt, *type_in, *type_in)
	      != NULL_TREE)
	    {
	      var = vect_recog_temp_ssa_var (TREE_TYPE (base), stmt);
	      gimple_call_set_lhs (stmt, var);
	      return stmt;
	    }
	}
    }

  return NULL;
}


/* Function vect_recog_widen_sum_pattern

   Try to find the following pattern:

     type x_t;
     TYPE x_T, sum = init;
   loop:
     sum_0 = phi <init, sum_1>
     S1  x_t = *p;
     S2  x_T = (TYPE) x_t;
     S3  sum_1 = x_T + sum_0;

   where type 'TYPE' is at least double the size of type 'type', i.e - we're
   summing elements of type 'type' into an accumulator of type 'TYPE'. This is
   a special case of a reduction computation.

   Input:

   * LAST_STMT: A stmt from which the pattern search begins. In the example,
   when this function is called with S3, the pattern {S2,S3} will be detected.

   Output:

   * TYPE_IN: The type of the input arguments to the pattern.

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        WIDEN_SUM <x_t, sum_0>

   Note: The widening-sum idiom is a widening reduction pattern that is
	 vectorized without preserving all the intermediate results. It
         produces only N/2 (widened) results (by summing up pairs of
	 intermediate results) rather than all N results.  Therefore, we
	 cannot allow this pattern when we want to get all the results and in
	 the correct order (as is the case when this computation is in an
	 inner-loop nested in an outer-loop that us being vectorized).  */

static gimple
vect_recog_widen_sum_pattern (VEC (gimple, heap) **stmts, tree *type_in,
			      tree *type_out)
{
  gimple stmt, last_stmt = VEC_index (gimple, *stmts, 0);
  tree oprnd0, oprnd1;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  tree type, half_type;
  gimple pattern_stmt;
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_info);
  tree var;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  type = gimple_expr_type (last_stmt);

  /* Look for the following pattern
          DX = (TYPE) X;
          sum_1 = DX + sum_0;
     In which DX is at least double the size of X, and sum_1 has been
     recognized as a reduction variable.
   */

  /* Starting from LAST_STMT, follow the defs of its uses in search
     of the above pattern.  */

  if (gimple_assign_rhs_code (last_stmt) != PLUS_EXPR)
    return NULL;

  if (STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_reduction_def)
    return NULL;

  oprnd0 = gimple_assign_rhs1 (last_stmt);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  if (!types_compatible_p (TREE_TYPE (oprnd0), type)
      || !types_compatible_p (TREE_TYPE (oprnd1), type))
    return NULL;

  /* So far so good.  Since last_stmt was detected as a (summation) reduction,
     we know that oprnd1 is the reduction variable (defined by a loop-header
     phi), and oprnd0 is an ssa-name defined by a stmt in the loop body.
     Left to check that oprnd0 is defined by a cast from type 'type' to type
     'TYPE'.  */

  if (!widened_name_p (oprnd0, last_stmt, &half_type, &stmt, true))
    return NULL;

  oprnd0 = gimple_assign_rhs1 (stmt);
  *type_in = half_type;
  *type_out = type;

  /* Pattern detected. Create a stmt to be used to replace the pattern: */
  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign_with_ops (WIDEN_SUM_EXPR, var,
					       oprnd0, oprnd1);
  SSA_NAME_DEF_STMT (var) = pattern_stmt;

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "vect_recog_widen_sum_pattern: detected: ");
      print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);
    }

  /* We don't allow changing the order of the computation in the inner-loop
     when doing outer-loop vectorization.  */
  gcc_assert (!nested_in_vect_loop_p (loop, last_stmt));

  return pattern_stmt;
}


/* Return TRUE if the operation in STMT can be performed on a smaller type.

   Input:
   STMT - a statement to check.
   DEF - we support operations with two operands, one of which is constant.
         The other operand can be defined by a demotion operation, or by a
         previous statement in a sequence of over-promoted operations.  In the
         later case DEF is used to replace that operand.  (It is defined by a
         pattern statement we created for the previous statement in the
         sequence).

   Input/output:
   NEW_TYPE - Output: a smaller type that we are trying to use.  Input: if not
         NULL, it's the type of DEF.
   STMTS - additional pattern statements.  If a pattern statement (type
         conversion) is created in this function, its original statement is
         added to STMTS.

   Output:
   OP0, OP1 - if the operation fits a smaller type, OP0 and OP1 are the new
         operands to use in the new pattern statement for STMT (will be created
         in vect_recog_over_widening_pattern ()).
   NEW_DEF_STMT - in case DEF has to be promoted, we create two pattern
         statements for STMT: the first one is a type promotion and the second
         one is the operation itself.  We return the type promotion statement
         in NEW_DEF_STMT and further store it in STMT_VINFO_PATTERN_DEF_STMT of
         the second pattern statement.  */

static bool
vect_operation_fits_smaller_type (gimple stmt, tree def, tree *new_type,
                                  tree *op0, tree *op1, gimple *new_def_stmt,
                                  VEC (gimple, heap) **stmts)
{
  enum tree_code code;
  tree const_oprnd, oprnd;
  tree interm_type = NULL_TREE, half_type, tmp, new_oprnd, type;
  gimple def_stmt, new_stmt;
  bool first = false;

  *new_def_stmt = NULL;

  if (!is_gimple_assign (stmt))
    return false;

  code = gimple_assign_rhs_code (stmt);
  if (code != LSHIFT_EXPR && code != RSHIFT_EXPR
      && code != BIT_IOR_EXPR && code != BIT_XOR_EXPR && code != BIT_AND_EXPR)
    return false;

  oprnd = gimple_assign_rhs1 (stmt);
  const_oprnd = gimple_assign_rhs2 (stmt);
  type = gimple_expr_type (stmt);

  if (TREE_CODE (oprnd) != SSA_NAME
      || TREE_CODE (const_oprnd) != INTEGER_CST)
    return false;

  /* If we are in the middle of a sequence, we use DEF from a previous
     statement.  Otherwise, OPRND has to be a result of type promotion.  */
  if (*new_type)
    {
      half_type = *new_type;
      oprnd = def;
    }
  else
    {
      first = true;
      if (!widened_name_p (oprnd, stmt, &half_type, &def_stmt, false)
          || !vinfo_for_stmt (def_stmt))
        return false;
    }

  /* Can we perform the operation on a smaller type?  */
  switch (code)
    {
      case BIT_IOR_EXPR:
      case BIT_XOR_EXPR:
      case BIT_AND_EXPR:
        if (!int_fits_type_p (const_oprnd, half_type))
          {
            /* HALF_TYPE is not enough.  Try a bigger type if possible.  */
            if (TYPE_PRECISION (type) < (TYPE_PRECISION (half_type) * 4))
              return false;

            interm_type = build_nonstandard_integer_type (
                        TYPE_PRECISION (half_type) * 2, TYPE_UNSIGNED (type));
            if (!int_fits_type_p (const_oprnd, interm_type))
              return false;
          }

        break;

      case LSHIFT_EXPR:
        /* Try intermediate type - HALF_TYPE is not enough for sure.  */
        if (TYPE_PRECISION (type) < (TYPE_PRECISION (half_type) * 4))
          return false;

        /* Check that HALF_TYPE size + shift amount <= INTERM_TYPE size.
          (e.g., if the original value was char, the shift amount is at most 8
           if we want to use short).  */
        if (compare_tree_int (const_oprnd, TYPE_PRECISION (half_type)) == 1)
          return false;

        interm_type = build_nonstandard_integer_type (
                        TYPE_PRECISION (half_type) * 2, TYPE_UNSIGNED (type));

        if (!vect_supportable_shift (code, interm_type))
          return false;

        break;

      case RSHIFT_EXPR:
        if (vect_supportable_shift (code, half_type))
          break;

        /* Try intermediate type - HALF_TYPE is not supported.  */
        if (TYPE_PRECISION (type) < (TYPE_PRECISION (half_type) * 4))
          return false;

        interm_type = build_nonstandard_integer_type (
                        TYPE_PRECISION (half_type) * 2, TYPE_UNSIGNED (type));

        if (!vect_supportable_shift (code, interm_type))
          return false;

        break;

      default:
        gcc_unreachable ();
    }

  /* There are four possible cases:
     1. OPRND is defined by a type promotion (in that case FIRST is TRUE, it's
        the first statement in the sequence)
        a. The original, HALF_TYPE, is not enough - we replace the promotion
           from HALF_TYPE to TYPE with a promotion to INTERM_TYPE.
        b. HALF_TYPE is sufficient, OPRND is set as the RHS of the original
           promotion.
     2. OPRND is defined by a pattern statement we created.
        a. Its type is not sufficient for the operation, we create a new stmt:
           a type conversion for OPRND from HALF_TYPE to INTERM_TYPE.  We store
           this statement in NEW_DEF_STMT, and it is later put in
           STMT_VINFO_PATTERN_DEF_STMT of the pattern statement for STMT.
        b. OPRND is good to use in the new statement.  */
  if (first)
    {
      if (interm_type)
        {
          /* Replace the original type conversion HALF_TYPE->TYPE with
             HALF_TYPE->INTERM_TYPE.  */
          if (STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt)))
            {
              new_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt));
              /* Check if the already created pattern stmt is what we need.  */
              if (!is_gimple_assign (new_stmt)
                  || gimple_assign_rhs_code (new_stmt) != NOP_EXPR
                  || TREE_TYPE (gimple_assign_lhs (new_stmt)) != interm_type)
                return false;

              oprnd = gimple_assign_lhs (new_stmt);
            }
          else
            {
              /* Create NEW_OPRND = (INTERM_TYPE) OPRND.  */
              oprnd = gimple_assign_rhs1 (def_stmt);
              tmp = create_tmp_reg (interm_type, NULL);
              add_referenced_var (tmp);
              new_oprnd = make_ssa_name (tmp, NULL);
              new_stmt = gimple_build_assign_with_ops (NOP_EXPR, new_oprnd,
                                                       oprnd, NULL_TREE);
              SSA_NAME_DEF_STMT (new_oprnd) = new_stmt;
              STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt)) = new_stmt;
              VEC_safe_push (gimple, heap, *stmts, def_stmt);
              oprnd = new_oprnd;
            }
        }
      else
        {
          /* Retrieve the operand before the type promotion.  */
          oprnd = gimple_assign_rhs1 (def_stmt);
        }
    }
  else
    {
      if (interm_type)
        {
          /* Create a type conversion HALF_TYPE->INTERM_TYPE.  */
          tmp = create_tmp_reg (interm_type, NULL);
          add_referenced_var (tmp);
          new_oprnd = make_ssa_name (tmp, NULL);
          new_stmt = gimple_build_assign_with_ops (NOP_EXPR, new_oprnd,
                                                   oprnd, NULL_TREE);
          SSA_NAME_DEF_STMT (new_oprnd) = new_stmt;
          oprnd = new_oprnd;
          *new_def_stmt = new_stmt;
        }

      /* Otherwise, OPRND is already set.  */
    }

  if (interm_type)
    *new_type = interm_type;
  else
    *new_type = half_type;

  *op0 = oprnd;
  *op1 = fold_convert (*new_type, const_oprnd);

  return true;
}


/* Try to find a statement or a sequence of statements that can be performed
   on a smaller type:

     type x_t;
     TYPE x_T, res0_T, res1_T;
   loop:
     S1  x_t = *p;
     S2  x_T = (TYPE) x_t;
     S3  res0_T = op (x_T, C0);
     S4  res1_T = op (res0_T, C1);
     S5  ... = () res1_T;  - type demotion

   where type 'TYPE' is at least double the size of type 'type', C0 and C1 are
   constants.
   Check if S3 and S4 can be done on a smaller type than 'TYPE', it can either
   be 'type' or some intermediate type.  For now, we expect S5 to be a type
   demotion operation.  We also check that S3 and S4 have only one use.
.

*/
static gimple
vect_recog_over_widening_pattern (VEC (gimple, heap) **stmts,
                                  tree *type_in, tree *type_out)
{
  gimple stmt = VEC_pop (gimple, *stmts);
  gimple pattern_stmt = NULL, new_def_stmt, prev_stmt = NULL, use_stmt = NULL;
  tree op0, op1, vectype = NULL_TREE, lhs, use_lhs, use_type;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  int nuses = 0;
  tree var = NULL_TREE, new_type = NULL_TREE, tmp, new_oprnd;
  bool first;
  struct loop *loop = (gimple_bb (stmt))->loop_father;

  first = true;
  while (1)
    {
      if (!vinfo_for_stmt (stmt)
          || STMT_VINFO_IN_PATTERN_P (vinfo_for_stmt (stmt)))
        return NULL;

      new_def_stmt = NULL;
      if (!vect_operation_fits_smaller_type (stmt, var, &new_type,
                                             &op0, &op1, &new_def_stmt,
                                             stmts))
        {
          if (first)
            return NULL;
          else
            break;
        }

      /* STMT can be performed on a smaller type.  Check its uses.  */
      lhs = gimple_assign_lhs (stmt);
      nuses = 0;
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, lhs)
        {
          if (is_gimple_debug (USE_STMT (use_p)))
            continue;
          use_stmt = USE_STMT (use_p);
          nuses++;
        }

      if (nuses != 1 || !is_gimple_assign (use_stmt)
          || !gimple_bb (use_stmt)
          || !flow_bb_inside_loop_p (loop, gimple_bb (use_stmt)))
        return NULL;

      /* Create pattern statement for STMT.  */
      vectype = get_vectype_for_scalar_type (new_type);
      if (!vectype)
        return NULL;

      /* We want to collect all the statements for which we create pattern
         statetments, except for the case when the last statement in the
         sequence doesn't have a corresponding pattern statement.  In such
         case we associate the last pattern statement with the last statement
         in the sequence.  Therefore, we only add an original statetement to
         the list if we know that it is not the last.  */
      if (prev_stmt)
        VEC_safe_push (gimple, heap, *stmts, prev_stmt);

      var = vect_recog_temp_ssa_var (new_type, NULL);
      pattern_stmt = gimple_build_assign_with_ops (
                          gimple_assign_rhs_code (stmt), var, op0, op1);
      SSA_NAME_DEF_STMT (var) = pattern_stmt;
      STMT_VINFO_RELATED_STMT (vinfo_for_stmt (stmt)) = pattern_stmt;
      STMT_VINFO_PATTERN_DEF_STMT (vinfo_for_stmt (stmt)) = new_def_stmt;

      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "created pattern stmt: ");
          print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);
        }

      prev_stmt = stmt;
      stmt = use_stmt;

      first = false;
    }

  /* We got a sequence.  We expect it to end with a type demotion operation.
     Otherwise, we quit (for now).  There are three possible cases: the
     conversion is to NEW_TYPE (we don't do anything), the conversion is to
     a type bigger than NEW_TYPE and/or the signedness of USE_TYPE and
     NEW_TYPE differs (we create a new conversion statement).  */
  if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (use_stmt)))
    {
      use_lhs = gimple_assign_lhs (use_stmt);
      use_type = TREE_TYPE (use_lhs);
      /* Support only type promotion or signedess change.  */
      if (!INTEGRAL_TYPE_P (use_type)
          || TYPE_PRECISION (new_type) > TYPE_PRECISION (use_type))
        return NULL;

      if (TYPE_UNSIGNED (new_type) != TYPE_UNSIGNED (use_type)
          || TYPE_PRECISION (new_type) != TYPE_PRECISION (use_type))
        {
          /* Create NEW_TYPE->USE_TYPE conversion.  */
          tmp = create_tmp_reg (use_type, NULL);
          add_referenced_var (tmp);
          new_oprnd = make_ssa_name (tmp, NULL);
          pattern_stmt = gimple_build_assign_with_ops (NOP_EXPR, new_oprnd,
                                                       var, NULL_TREE);
          SSA_NAME_DEF_STMT (new_oprnd) = pattern_stmt;
          STMT_VINFO_RELATED_STMT (vinfo_for_stmt (use_stmt)) = pattern_stmt;

          *type_in = get_vectype_for_scalar_type (new_type);
          *type_out = get_vectype_for_scalar_type (use_type);

          /* We created a pattern statement for the last statement in the
             sequence, so we don't need to associate it with the pattern
             statement created for PREV_STMT.  Therefore, we add PREV_STMT
             to the list in order to mark it later in vect_pattern_recog_1.  */
          if (prev_stmt)
            VEC_safe_push (gimple, heap, *stmts, prev_stmt);
        }
      else
        {
          if (prev_stmt)
            STMT_VINFO_PATTERN_DEF_STMT (vinfo_for_stmt (use_stmt))
               = STMT_VINFO_PATTERN_DEF_STMT (vinfo_for_stmt (prev_stmt));

          *type_in = vectype;
          *type_out = NULL_TREE;
        }

      VEC_safe_push (gimple, heap, *stmts, use_stmt);
    }
  else
    /* TODO: support general case, create a conversion to the correct type.  */
    return NULL;

  /* Pattern detected.  */
  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "vect_recog_over_widening_pattern: detected: ");
      print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);
    }

  return pattern_stmt;
}


/* Mark statements that are involved in a pattern.  */

static inline void
vect_mark_pattern_stmts (gimple orig_stmt, gimple pattern_stmt,
                         tree pattern_vectype)
{
  stmt_vec_info pattern_stmt_info, def_stmt_info;
  stmt_vec_info orig_stmt_info = vinfo_for_stmt (orig_stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (orig_stmt_info);
  gimple def_stmt;

  set_vinfo_for_stmt (pattern_stmt,
                      new_stmt_vec_info (pattern_stmt, loop_vinfo, NULL));
  gimple_set_bb (pattern_stmt, gimple_bb (orig_stmt));
  pattern_stmt_info = vinfo_for_stmt (pattern_stmt);

  STMT_VINFO_RELATED_STMT (pattern_stmt_info) = orig_stmt;
  STMT_VINFO_DEF_TYPE (pattern_stmt_info)
	= STMT_VINFO_DEF_TYPE (orig_stmt_info);
  STMT_VINFO_VECTYPE (pattern_stmt_info) = pattern_vectype;
  STMT_VINFO_IN_PATTERN_P (orig_stmt_info) = true;
  STMT_VINFO_RELATED_STMT (orig_stmt_info) = pattern_stmt;
  STMT_VINFO_PATTERN_DEF_STMT (pattern_stmt_info)
	= STMT_VINFO_PATTERN_DEF_STMT (orig_stmt_info);
  if (STMT_VINFO_PATTERN_DEF_STMT (pattern_stmt_info))
    {
      def_stmt = STMT_VINFO_PATTERN_DEF_STMT (pattern_stmt_info);
      set_vinfo_for_stmt (def_stmt,
                          new_stmt_vec_info (def_stmt, loop_vinfo, NULL));
      gimple_set_bb (def_stmt, gimple_bb (orig_stmt));
      def_stmt_info = vinfo_for_stmt (def_stmt);
      STMT_VINFO_RELATED_STMT (def_stmt_info) = orig_stmt;
      STMT_VINFO_DEF_TYPE (def_stmt_info)
	= STMT_VINFO_DEF_TYPE (orig_stmt_info);
      STMT_VINFO_VECTYPE (def_stmt_info) = pattern_vectype;
    }
}

/* Function vect_pattern_recog_1

   Input:
   PATTERN_RECOG_FUNC: A pointer to a function that detects a certain
        computation pattern.
   STMT: A stmt from which the pattern search should start.

   If PATTERN_RECOG_FUNC successfully detected the pattern, it creates an
   expression that computes the same functionality and can be used to
   replace the sequence of stmts that are involved in the pattern.

   Output:
   This function checks if the expression returned by PATTERN_RECOG_FUNC is
   supported in vector form by the target.  We use 'TYPE_IN' to obtain the
   relevant vector type. If 'TYPE_IN' is already a vector type, then this
   indicates that target support had already been checked by PATTERN_RECOG_FUNC.
   If 'TYPE_OUT' is also returned by PATTERN_RECOG_FUNC, we check that it fits
   to the available target pattern.

   This function also does some bookkeeping, as explained in the documentation
   for vect_recog_pattern.  */

static void
vect_pattern_recog_1 (
	gimple (* vect_recog_func) (VEC (gimple, heap) **, tree *, tree *),
	gimple_stmt_iterator si)
{
  gimple stmt = gsi_stmt (si), pattern_stmt;
  stmt_vec_info stmt_info;
  loop_vec_info loop_vinfo;
  tree pattern_vectype;
  tree type_in, type_out;
  enum tree_code code;
  int i;
  gimple next;
  VEC (gimple, heap) *stmts_to_replace = VEC_alloc (gimple, heap, 1);

  VEC_quick_push (gimple, stmts_to_replace, stmt);
  pattern_stmt = (* vect_recog_func) (&stmts_to_replace, &type_in, &type_out);
  if (!pattern_stmt)
    return;

  stmt = VEC_last (gimple, stmts_to_replace);
  stmt_info = vinfo_for_stmt (stmt);
  loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
 
  if (VECTOR_MODE_P (TYPE_MODE (type_in)))
    {
      /* No need to check target support (already checked by the pattern
         recognition function).  */
      if (type_out)
	gcc_assert (VECTOR_MODE_P (TYPE_MODE (type_out)));
      pattern_vectype = type_out ? type_out : type_in;
    }
  else
    {
      enum machine_mode vec_mode;
      enum insn_code icode;
      optab optab;

      /* Check target support  */
      type_in = get_vectype_for_scalar_type (type_in);
      if (!type_in)
	return;
      if (type_out)
	type_out = get_vectype_for_scalar_type (type_out);
      else
	type_out = type_in;
      if (!type_out)
	return;
      pattern_vectype = type_out;

      if (is_gimple_assign (pattern_stmt))
	code = gimple_assign_rhs_code (pattern_stmt);
      else
        {
	  gcc_assert (is_gimple_call (pattern_stmt));
	  code = CALL_EXPR;
	}

      optab = optab_for_tree_code (code, type_in, optab_default);
      vec_mode = TYPE_MODE (type_in);
      if (!optab
          || (icode = optab_handler (optab, vec_mode)) == CODE_FOR_nothing
          || (insn_data[icode].operand[0].mode != TYPE_MODE (type_out)))
	return;
    }

  /* Found a vectorizable pattern.  */
  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "pattern recognized: ");
      print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);
    }

  /* Mark the stmts that are involved in the pattern. */
  vect_mark_pattern_stmts (stmt, pattern_stmt, pattern_vectype);

  /* Patterns cannot be vectorized using SLP, because they change the order of
     computation.  */
  FOR_EACH_VEC_ELT (gimple, LOOP_VINFO_REDUCTIONS (loop_vinfo), i, next)
    if (next == stmt)
      VEC_ordered_remove (gimple, LOOP_VINFO_REDUCTIONS (loop_vinfo), i); 

  /* It is possible that additional pattern stmts are created and inserted in
     STMTS_TO_REPLACE.  We create a stmt_info for each of them, and mark the
     relevant statements.  */
  for (i = 0; VEC_iterate (gimple, stmts_to_replace, i, stmt)
              && (unsigned) i < (VEC_length (gimple, stmts_to_replace) - 1);
       i++)
    {
      stmt_info = vinfo_for_stmt (stmt);
      pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "additional pattern stmt: ");
          print_gimple_stmt (vect_dump, pattern_stmt, 0, TDF_SLIM);
        }

      vect_mark_pattern_stmts (stmt, pattern_stmt, NULL_TREE);
    }

  VEC_free (gimple, heap, stmts_to_replace);
}


/* Function vect_pattern_recog

   Input:
   LOOP_VINFO - a struct_loop_info of a loop in which we want to look for
        computation idioms.

   Output - for each computation idiom that is detected we create a new stmt
        that provides the same functionality and that can be vectorized.  We
        also record some information in the struct_stmt_info of the relevant
        stmts, as explained below:

   At the entry to this function we have the following stmts, with the
   following initial value in the STMT_VINFO fields:

         stmt                     in_pattern_p  related_stmt    vec_stmt
         S1: a_i = ....                 -       -               -
         S2: a_2 = ..use(a_i)..         -       -               -
         S3: a_1 = ..use(a_2)..         -       -               -
         S4: a_0 = ..use(a_1)..         -       -               -
         S5: ... = ..use(a_0)..         -       -               -

   Say the sequence {S1,S2,S3,S4} was detected as a pattern that can be
   represented by a single stmt.  We then:
   - create a new stmt S6 equivalent to the pattern (the stmt is not
     inserted into the code)
   - fill in the STMT_VINFO fields as follows:

                                  in_pattern_p  related_stmt    vec_stmt
         S1: a_i = ....                 -       -               -
         S2: a_2 = ..use(a_i)..         -       -               -
         S3: a_1 = ..use(a_2)..         -       -               -
         S4: a_0 = ..use(a_1)..         true    S6              -
          '---> S6: a_new = ....        -       S4              -
         S5: ... = ..use(a_0)..         -       -               -

   (the last stmt in the pattern (S4) and the new pattern stmt (S6) point
   to each other through the RELATED_STMT field).

   S6 will be marked as relevant in vect_mark_stmts_to_be_vectorized instead
   of S4 because it will replace all its uses.  Stmts {S1,S2,S3} will
   remain irrelevant unless used by stmts other than S4.

   If vectorization succeeds, vect_transform_stmt will skip over {S1,S2,S3}
   (because they are marked as irrelevant).  It will vectorize S6, and record
   a pointer to the new vector stmt VS6 from S6 (as usual).
   S4 will be skipped, and S5 will be vectorized as usual:

                                  in_pattern_p  related_stmt    vec_stmt
         S1: a_i = ....                 -       -               -
         S2: a_2 = ..use(a_i)..         -       -               -
         S3: a_1 = ..use(a_2)..         -       -               -
       > VS6: va_new = ....             -       -               -
         S4: a_0 = ..use(a_1)..         true    S6              VS6
          '---> S6: a_new = ....        -       S4              VS6
       > VS5: ... = ..vuse(va_new)..    -       -               -
         S5: ... = ..use(a_0)..         -       -               -

   DCE could then get rid of {S1,S2,S3,S4,S5} (if their defs are not used
   elsewhere), and we'll end up with:

        VS6: va_new = ....
        VS5: ... = ..vuse(va_new)..

   In case of more than one pattern statements, e.g., widen-mult with
   intermediate type:

     S1  a_t = ;
     S2  a_T = (TYPE) a_t;
           '--> S3: a_it = (interm_type) a_t;
     S4  prod_T = a_T * CONST;
           '--> S5: prod_T' = a_it w* CONST;

   there may be other users of a_T outside the pattern.  In that case S2 will
   be marked as relevant (as well as S3), and both S2 and S3 will be analyzed
   and vectorized.  The vector stmt VS2 will be recorded in S2, and VS3 will
   be recorded in S3.  */

void
vect_pattern_recog (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  gimple_stmt_iterator si;
  unsigned int i, j;
  gimple (* vect_recog_func_ptr) (VEC (gimple, heap) **, tree *, tree *);

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_pattern_recog ===");

  /* Scan through the loop stmts, applying the pattern recognition
     functions starting at each stmt visited:  */
  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
        {
          /* Scan over all generic vect_recog_xxx_pattern functions.  */
          for (j = 0; j < NUM_PATTERNS; j++)
            {
              vect_recog_func_ptr = vect_vect_recog_func_ptrs[j];
              vect_pattern_recog_1 (vect_recog_func_ptr, si);
            }
        }
    }
}
