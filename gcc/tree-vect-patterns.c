/* Analysis Utilities for Loop Vectorization.
   Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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

/* Function prototypes */
static void vect_pattern_recog_1
  (gimple (* ) (gimple, tree *, tree *), gimple_stmt_iterator);
static bool widened_name_p (tree, gimple, tree *, gimple *);

/* Pattern recognition functions  */
static gimple vect_recog_widen_sum_pattern (gimple, tree *, tree *);
static gimple vect_recog_widen_mult_pattern (gimple, tree *, tree *);
static gimple vect_recog_dot_prod_pattern (gimple, tree *, tree *);
static gimple vect_recog_pow_pattern (gimple, tree *, tree *);
static vect_recog_func_ptr vect_vect_recog_func_ptrs[NUM_PATTERNS] = {
	vect_recog_widen_mult_pattern,
	vect_recog_widen_sum_pattern,
	vect_recog_dot_prod_pattern,
	vect_recog_pow_pattern};


/* Function widened_name_p

   Check whether NAME, an ssa-name used in USE_STMT,
   is a result of a type-promotion, such that:
     DEF_STMT: NAME = NOP (name0)
   where the type of name0 (HALF_TYPE) is smaller than the type of NAME.
*/

static bool
widened_name_p (tree name, gimple use_stmt, tree *half_type, gimple *def_stmt)
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
      || (TYPE_UNSIGNED (type) != TYPE_UNSIGNED (*half_type))
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

   * LAST_STMT: A stmt from which the pattern search begins. In the example,
   when this function is called with S7, the pattern {S3,S4,S5,S6,S7} will be
   detected.

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
vect_recog_dot_prod_pattern (gimple last_stmt, tree *type_in, tree *type_out)
{
  gimple stmt;
  tree oprnd0, oprnd1;
  tree oprnd00, oprnd01;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  tree type, half_type;
  gimple pattern_stmt;
  tree prod_type;
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_info);
  tree var, rhs;

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

      if (widened_name_p (oprnd0, stmt, &half_type, &def_stmt))
        {
          stmt = def_stmt;
          oprnd0 = gimple_assign_rhs1 (stmt);
        }
      else
        half_type = type;
    }

  /* So far so good. Since last_stmt was detected as a (summation) reduction,
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
      if (!widened_name_p (oprnd0, stmt, &half_type0, &def_stmt))
        return NULL;
      oprnd00 = gimple_assign_rhs1 (def_stmt);
      if (!widened_name_p (oprnd1, stmt, &half_type1, &def_stmt))
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
  rhs =	build3 (DOT_PROD_EXPR, type, oprnd00, oprnd01, oprnd1),
  pattern_stmt = gimple_build_assign (var, rhs);

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

   Input:

   * LAST_STMT: A stmt from which the pattern search begins. In the example,
   when this function is called with S5, the pattern {S3,S4,S5} is be detected.

   Output:

   * TYPE_IN: The type of the input arguments to the pattern.

   * TYPE_OUT: The type of the output  of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        WIDEN_MULT <a_t, b_t>
*/

static gimple
vect_recog_widen_mult_pattern (gimple last_stmt,
			       tree *type_in,
			       tree *type_out)
{
  gimple def_stmt0, def_stmt1;
  tree oprnd0, oprnd1;
  tree type, half_type0, half_type1;
  gimple pattern_stmt;
  tree vectype, vectype_out;
  tree dummy;
  tree var;
  enum tree_code dummy_code;
  int dummy_int;
  VEC (tree, heap) *dummy_vec;

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

  /* Check argument 0 */
  if (!widened_name_p (oprnd0, last_stmt, &half_type0, &def_stmt0))
    return NULL;
  oprnd0 = gimple_assign_rhs1 (def_stmt0);

  /* Check argument 1 */
  if (!widened_name_p (oprnd1, last_stmt, &half_type1, &def_stmt1))
    return NULL;
  oprnd1 = gimple_assign_rhs1 (def_stmt1);

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
vect_recog_pow_pattern (gimple last_stmt, tree *type_in, tree *type_out)
{
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
vect_recog_widen_sum_pattern (gimple last_stmt, tree *type_in, tree *type_out)
{
  gimple stmt;
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

  /* So far so good. Since last_stmt was detected as a (summation) reduction,
     we know that oprnd1 is the reduction variable (defined by a loop-header
     phi), and oprnd0 is an ssa-name defined by a stmt in the loop body.
     Left to check that oprnd0 is defined by a cast from type 'type' to type
     'TYPE'.  */

  if (!widened_name_p (oprnd0, last_stmt, &half_type, &stmt))
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
	gimple (* vect_recog_func) (gimple, tree *, tree *),
	gimple_stmt_iterator si)
{
  gimple stmt = gsi_stmt (si), pattern_stmt;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  stmt_vec_info pattern_stmt_info;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree pattern_vectype;
  tree type_in, type_out;
  enum tree_code code;
  int i;
  gimple next;

  pattern_stmt = (* vect_recog_func) (stmt, &type_in, &type_out);
  if (!pattern_stmt)
    return;

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
  gsi_insert_before (&si, pattern_stmt, GSI_SAME_STMT);
  set_vinfo_for_stmt (pattern_stmt,
		      new_stmt_vec_info (pattern_stmt, loop_vinfo, NULL));
  pattern_stmt_info = vinfo_for_stmt (pattern_stmt);

  STMT_VINFO_RELATED_STMT (pattern_stmt_info) = stmt;
  STMT_VINFO_DEF_TYPE (pattern_stmt_info) = STMT_VINFO_DEF_TYPE (stmt_info);
  STMT_VINFO_VECTYPE (pattern_stmt_info) = pattern_vectype;
  STMT_VINFO_IN_PATTERN_P (stmt_info) = true;
  STMT_VINFO_RELATED_STMT (stmt_info) = pattern_stmt;

  /* Patterns cannot be vectorized using SLP, because they change the order of
     computation.  */
  FOR_EACH_VEC_ELT (gimple, LOOP_VINFO_REDUCTIONS (loop_vinfo), i, next)
    if (next == stmt)
      VEC_ordered_remove (gimple, LOOP_VINFO_REDUCTIONS (loop_vinfo), i); 
}


/* Function vect_pattern_recog

   Input:
   LOOP_VINFO - a struct_loop_info of a loop in which we want to look for
        computation idioms.

   Output - for each computation idiom that is detected we insert a new stmt
        that provides the same functionality and that can be vectorized. We
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
   represented by a single stmt. We then:
   - create a new stmt S6 that will replace the pattern.
   - insert the new stmt S6 before the last stmt in the pattern
   - fill in the STMT_VINFO fields as follows:

                                  in_pattern_p  related_stmt    vec_stmt
         S1: a_i = ....                 -       -               -
         S2: a_2 = ..use(a_i)..         -       -               -
         S3: a_1 = ..use(a_2)..         -       -               -
       > S6: a_new = ....               -       S4              -
         S4: a_0 = ..use(a_1)..         true    S6              -
         S5: ... = ..use(a_0)..         -       -               -

   (the last stmt in the pattern (S4) and the new pattern stmt (S6) point
    to each other through the RELATED_STMT field).

   S6 will be marked as relevant in vect_mark_stmts_to_be_vectorized instead
   of S4 because it will replace all its uses.  Stmts {S1,S2,S3} will
   remain irrelevant unless used by stmts other than S4.

   If vectorization succeeds, vect_transform_stmt will skip over {S1,S2,S3}
   (because they are marked as irrelevant). It will vectorize S6, and record
   a pointer to the new vector stmt VS6 both from S6 (as usual), and also
   from S4. We do that so that when we get to vectorizing stmts that use the
   def of S4 (like S5 that uses a_0), we'll know where to take the relevant
   vector-def from. S4 will be skipped, and S5 will be vectorized as usual:

                                  in_pattern_p  related_stmt    vec_stmt
         S1: a_i = ....                 -       -               -
         S2: a_2 = ..use(a_i)..         -       -               -
         S3: a_1 = ..use(a_2)..         -       -               -
       > VS6: va_new = ....             -       -               -
         S6: a_new = ....               -       S4              VS6
         S4: a_0 = ..use(a_1)..         true    S6              VS6
       > VS5: ... = ..vuse(va_new)..    -       -               -
         S5: ... = ..use(a_0)..         -       -               -

   DCE could then get rid of {S1,S2,S3,S4,S5,S6} (if their defs are not used
   elsewhere), and we'll end up with:

        VS6: va_new = ....
        VS5: ... = ..vuse(va_new)..

   If vectorization does not succeed, DCE will clean S6 away (its def is
   not used), and we'll end up with the original sequence.
*/

void
vect_pattern_recog (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  gimple_stmt_iterator si;
  unsigned int i, j;
  gimple (* vect_recog_func_ptr) (gimple, tree *, tree *);

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
