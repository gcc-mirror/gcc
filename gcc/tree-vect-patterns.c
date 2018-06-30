/* Analysis Utilities for Loop Vectorization.
   Copyright (C) 2006-2018 Free Software Foundation, Inc.
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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "expmed.h"
#include "optabs-tree.h"
#include "insn-config.h"
#include "recog.h"		/* FIXME: for insn_data */
#include "fold-const.h"
#include "stor-layout.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "dumpfile.h"
#include "builtins.h"
#include "internal-fn.h"
#include "case-cfn-macros.h"
#include "fold-const-call.h"
#include "attribs.h"
#include "cgraph.h"
#include "omp-simd-clone.h"
#include "predict.h"

/* Report that we've found an instance of pattern PATTERN in
   statement STMT.  */

static void
vect_pattern_detected (const char *name, gimple *stmt)
{
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "%s: detected: ", name);
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
    }
}

/* Associate pattern statement PATTERN_STMT with ORIG_STMT_INFO.
   Set its vector type to VECTYPE if it doesn't have one already.  */

static void
vect_init_pattern_stmt (gimple *pattern_stmt, stmt_vec_info orig_stmt_info,
			tree vectype)
{
  stmt_vec_info pattern_stmt_info = vinfo_for_stmt (pattern_stmt);
  if (pattern_stmt_info == NULL)
    {
      pattern_stmt_info = new_stmt_vec_info (pattern_stmt,
					     orig_stmt_info->vinfo);
      set_vinfo_for_stmt (pattern_stmt, pattern_stmt_info);
    }
  gimple_set_bb (pattern_stmt, gimple_bb (orig_stmt_info->stmt));

  STMT_VINFO_RELATED_STMT (pattern_stmt_info) = orig_stmt_info->stmt;
  STMT_VINFO_DEF_TYPE (pattern_stmt_info)
    = STMT_VINFO_DEF_TYPE (orig_stmt_info);
  if (!STMT_VINFO_VECTYPE (pattern_stmt_info))
    STMT_VINFO_VECTYPE (pattern_stmt_info) = vectype;
}

/* Set the pattern statement of ORIG_STMT_INFO to PATTERN_STMT.
   Also set the vector type of PATTERN_STMT to VECTYPE, if it doesn't
   have one already.  */

static void
vect_set_pattern_stmt (gimple *pattern_stmt, stmt_vec_info orig_stmt_info,
		       tree vectype)
{
  STMT_VINFO_IN_PATTERN_P (orig_stmt_info) = true;
  STMT_VINFO_RELATED_STMT (orig_stmt_info) = pattern_stmt;
  vect_init_pattern_stmt (pattern_stmt, orig_stmt_info, vectype);
}

/* Add NEW_STMT to STMT_INFO's pattern definition statements.  If VECTYPE
   is nonnull, record that NEW_STMT's vector type is VECTYPE, which might
   be different from the vector type of the final pattern statement.  */

static inline void
append_pattern_def_seq (stmt_vec_info stmt_info, gimple *new_stmt,
			tree vectype = NULL_TREE)
{
  vec_info *vinfo = stmt_info->vinfo;
  if (vectype)
    {
      gcc_assert (!vinfo_for_stmt (new_stmt));
      stmt_vec_info new_stmt_info = new_stmt_vec_info (new_stmt, vinfo);
      set_vinfo_for_stmt (new_stmt, new_stmt_info);
      STMT_VINFO_VECTYPE (new_stmt_info) = vectype;
    }
  gimple_seq_add_stmt_without_update (&STMT_VINFO_PATTERN_DEF_SEQ (stmt_info),
				      new_stmt);
}

static inline void
new_pattern_def_seq (stmt_vec_info stmt_info, gimple *stmt)
{
  STMT_VINFO_PATTERN_DEF_SEQ (stmt_info) = NULL;
  append_pattern_def_seq (stmt_info, stmt);
}

/* The caller wants to perform new operations on vect_external variable
   VAR, so that the result of the operations would also be vect_external.
   Return the edge on which the operations can be performed, if one exists.
   Return null if the operations should instead be treated as part of
   the pattern that needs them.  */

static edge
vect_get_external_def_edge (vec_info *vinfo, tree var)
{
  edge e = NULL;
  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      e = loop_preheader_edge (loop_vinfo->loop);
      if (!SSA_NAME_IS_DEFAULT_DEF (var))
	{
	  basic_block bb = gimple_bb (SSA_NAME_DEF_STMT (var));
	  if (bb == NULL
	      || !dominated_by_p (CDI_DOMINATORS, e->dest, bb))
	    e = NULL;
	}
    }
  return e;
}

/* Return true if the target supports a vector version of CODE,
   where CODE is known to map to a direct optab.  ITYPE specifies
   the type of (some of) the scalar inputs and OTYPE specifies the
   type of the scalar result.

   If CODE allows the inputs and outputs to have different type
   (such as for WIDEN_SUM_EXPR), it is the input mode rather
   than the output mode that determines the appropriate target pattern.
   Operand 0 of the target pattern then specifies the mode that the output
   must have.

   When returning true, set *VECOTYPE_OUT to the vector version of OTYPE.
   Also set *VECITYPE_OUT to the vector version of ITYPE if VECITYPE_OUT
   is nonnull.  */

static bool
vect_supportable_direct_optab_p (tree otype, tree_code code,
				 tree itype, tree *vecotype_out,
				 tree *vecitype_out = NULL)
{
  tree vecitype = get_vectype_for_scalar_type (itype);
  if (!vecitype)
    return false;

  tree vecotype = get_vectype_for_scalar_type (otype);
  if (!vecotype)
    return false;

  optab optab = optab_for_tree_code (code, vecitype, optab_default);
  if (!optab)
    return false;

  insn_code icode = optab_handler (optab, TYPE_MODE (vecitype));
  if (icode == CODE_FOR_nothing
      || insn_data[icode].operand[0].mode != TYPE_MODE (vecotype))
    return false;

  *vecotype_out = vecotype;
  if (vecitype_out)
    *vecitype_out = vecitype;
  return true;
}

/* Check whether STMT2 is in the same loop or basic block as STMT1.
   Which of the two applies depends on whether we're currently doing
   loop-based or basic-block-based vectorization, as determined by
   the vinfo_for_stmt for STMT1 (which must be defined).

   If this returns true, vinfo_for_stmt for STMT2 is guaranteed
   to be defined as well.  */

static bool
vect_same_loop_or_bb_p (gimple *stmt1, gimple *stmt2)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt1);
  return vect_stmt_in_region_p (stmt_vinfo->vinfo, stmt2);
}

/* If the LHS of DEF_STMT has a single use, and that statement is
   in the same loop or basic block, return it.  */

static gimple *
vect_single_imm_use (gimple *def_stmt)
{
  tree lhs = gimple_assign_lhs (def_stmt);
  use_operand_p use_p;
  gimple *use_stmt;

  if (!single_imm_use (lhs, &use_p, &use_stmt))
    return NULL;

  if (!vect_same_loop_or_bb_p (def_stmt, use_stmt))
    return NULL;

  return use_stmt;
}

/* Round bit precision PRECISION up to a full element.  */

static unsigned int
vect_element_precision (unsigned int precision)
{
  precision = 1 << ceil_log2 (precision);
  return MAX (precision, BITS_PER_UNIT);
}

/* If OP is defined by a statement that's being considered for vectorization,
   return information about that statement, otherwise return NULL.  */

static stmt_vec_info
vect_get_internal_def (vec_info *vinfo, tree op)
{
  vect_def_type dt;
  gimple *def_stmt;
  if (TREE_CODE (op) != SSA_NAME
      || !vect_is_simple_use (op, vinfo, &dt, &def_stmt)
      || dt != vect_internal_def)
    return NULL;

  return vinfo_for_stmt (def_stmt);
}

/* Check whether NAME, an ssa-name used in USE_STMT,
   is a result of a type promotion, such that:
     DEF_STMT: NAME = NOP (name0)
   If CHECK_SIGN is TRUE, check that either both types are signed or both are
   unsigned.  */

static bool
type_conversion_p (tree name, gimple *use_stmt, bool check_sign,
		   tree *orig_type, gimple **def_stmt, bool *promotion)
{
  stmt_vec_info stmt_vinfo;
  tree type = TREE_TYPE (name);
  tree oprnd0;
  enum vect_def_type dt;

  stmt_vinfo = vinfo_for_stmt (use_stmt);
  if (!vect_is_simple_use (name, stmt_vinfo->vinfo, &dt, def_stmt))
    return false;

  if (dt != vect_internal_def
      && dt != vect_external_def && dt != vect_constant_def)
    return false;

  if (!*def_stmt)
    return false;

  if (!is_gimple_assign (*def_stmt))
    return false;

  if (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (*def_stmt)))
    return false;

  oprnd0 = gimple_assign_rhs1 (*def_stmt);

  *orig_type = TREE_TYPE (oprnd0);
  if (!INTEGRAL_TYPE_P (type) || !INTEGRAL_TYPE_P (*orig_type)
      || ((TYPE_UNSIGNED (type) != TYPE_UNSIGNED (*orig_type)) && check_sign))
    return false;

  if (TYPE_PRECISION (type) >= (TYPE_PRECISION (*orig_type) * 2))
    *promotion = true;
  else
    *promotion = false;

  if (!vect_is_simple_use (oprnd0, stmt_vinfo->vinfo, &dt))
    return false;

  return true;
}

/* Holds information about an input operand after some sign changes
   and type promotions have been peeled away.  */
struct vect_unpromoted_value {
  vect_unpromoted_value ();

  void set_op (tree, vect_def_type, stmt_vec_info = NULL);

  /* The value obtained after peeling away zero or more casts.  */
  tree op;

  /* The type of OP.  */
  tree type;

  /* The definition type of OP.  */
  vect_def_type dt;

  /* If OP is the result of peeling at least one cast, and if the cast
     of OP itself is a vectorizable statement, CASTER identifies that
     statement, otherwise it is null.  */
  stmt_vec_info caster;
};

inline vect_unpromoted_value::vect_unpromoted_value ()
  : op (NULL_TREE),
    type (NULL_TREE),
    dt (vect_uninitialized_def),
    caster (NULL)
{
}

/* Set the operand to OP_IN, its definition type to DT_IN, and the
   statement that casts it to CASTER_IN.  */

inline void
vect_unpromoted_value::set_op (tree op_in, vect_def_type dt_in,
			       stmt_vec_info caster_in)
{
  op = op_in;
  type = TREE_TYPE (op);
  dt = dt_in;
  caster = caster_in;
}

/* If OP is a vectorizable SSA name, strip a sequence of integer conversions
   to reach some vectorizable inner operand OP', continuing as long as it
   is possible to convert OP' back to OP using a possible sign change
   followed by a possible promotion P.  Return this OP', or null if OP is
   not a vectorizable SSA name.  If there is a promotion P, describe its
   input in UNPROM, otherwise describe OP' in UNPROM.

   A successful return means that it is possible to go from OP' to OP
   via UNPROM.  The cast from OP' to UNPROM is at most a sign change,
   whereas the cast from UNPROM to OP might be a promotion, a sign
   change, or a nop.

   E.g. say we have:

       signed short *ptr = ...;
       signed short C = *ptr;
       unsigned short B = (unsigned short) C;    // sign change
       signed int A = (signed int) B;            // unsigned promotion
       ...possible other uses of A...
       unsigned int OP = (unsigned int) A;       // sign change

   In this case it's possible to go directly from C to OP using:

       OP = (unsigned int) (unsigned short) C;
	    +------------+ +--------------+
	       promotion      sign change

   so OP' would be C.  The input to the promotion is B, so UNPROM
   would describe B.  */

static tree
vect_look_through_possible_promotion (vec_info *vinfo, tree op,
				      vect_unpromoted_value *unprom)
{
  tree res = NULL_TREE;
  tree op_type = TREE_TYPE (op);
  unsigned int orig_precision = TYPE_PRECISION (op_type);
  stmt_vec_info caster = NULL;
  while (TREE_CODE (op) == SSA_NAME && INTEGRAL_TYPE_P (op_type))
    {
      /* See whether OP is simple enough to vectorize.  */
      gimple *def_stmt;
      vect_def_type dt;
      if (!vect_is_simple_use (op, vinfo, &dt, &def_stmt))
	break;

      /* If OP is the input of a demotion, skip over it to see whether
	 OP is itself the result of a promotion.  If so, the combined
	 effect of the promotion and the demotion might fit the required
	 pattern, otherwise neither operation fits.

	 This copes with cases such as the result of an arithmetic
	 operation being truncated before being stored, and where that
	 arithmetic operation has been recognized as an over-widened one.  */
      if (TYPE_PRECISION (op_type) <= orig_precision)
	{
	  /* Use OP as the UNPROM described above if we haven't yet
	     found a promotion, or if using the new input preserves the
	     sign of the previous promotion.  */
	  if (!res
	      || TYPE_PRECISION (unprom->type) == orig_precision
	      || TYPE_SIGN (unprom->type) == TYPE_SIGN (op_type))
	    unprom->set_op (op, dt, caster);
	  /* Stop if we've already seen a promotion and if this
	     conversion does more than change the sign.  */
	  else if (TYPE_PRECISION (op_type)
		   != TYPE_PRECISION (unprom->type))
	    break;

	  /* The sequence now extends to OP.  */
	  res = op;
	}

      /* See whether OP is defined by a cast.  Record it as CASTER if
	 the cast is potentially vectorizable.  */
      if (!def_stmt)
	break;
      if (dt == vect_internal_def)
	caster = vinfo_for_stmt (def_stmt);
      else
	caster = NULL;
      gassign *assign = dyn_cast <gassign *> (def_stmt);
      if (!assign || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
	break;

      /* Continue with the input to the cast.  */
      op = gimple_assign_rhs1 (def_stmt);
      op_type = TREE_TYPE (op);
    }
  return res;
}

/* OP is an integer operand to an operation that returns TYPE, and we
   want to treat the operation as a widening one.  So far we can treat
   it as widening from *COMMON_TYPE.

   Return true if OP is suitable for such a widening operation,
   either widening from *COMMON_TYPE or from some supertype of it.
   Update *COMMON_TYPE to the supertype in the latter case.

   SHIFT_P is true if OP is a shift amount.  */

static bool
vect_joust_widened_integer (tree type, bool shift_p, tree op,
			    tree *common_type)
{
  /* Calculate the minimum precision required by OP, without changing
     the sign of either operand.  */
  unsigned int precision;
  if (shift_p)
    {
      if (!wi::leu_p (wi::to_widest (op), TYPE_PRECISION (type) / 2))
	return false;
      precision = TREE_INT_CST_LOW (op);
    }
  else
    {
      precision = wi::min_precision (wi::to_widest (op),
				     TYPE_SIGN (*common_type));
      if (precision * 2 > TYPE_PRECISION (type))
	return false;
    }

  /* If OP requires a wider type, switch to that type.  The checks
     above ensure that this is still narrower than the result.  */
  precision = vect_element_precision (precision);
  if (TYPE_PRECISION (*common_type) < precision)
    *common_type = build_nonstandard_integer_type
      (precision, TYPE_UNSIGNED (*common_type));
  return true;
}

/* Return true if the common supertype of NEW_TYPE and *COMMON_TYPE
   is narrower than type, storing the supertype in *COMMON_TYPE if so.  */

static bool
vect_joust_widened_type (tree type, tree new_type, tree *common_type)
{
  if (types_compatible_p (*common_type, new_type))
    return true;

  /* See if *COMMON_TYPE can hold all values of NEW_TYPE.  */
  if ((TYPE_PRECISION (new_type) < TYPE_PRECISION (*common_type))
      && (TYPE_UNSIGNED (new_type) || !TYPE_UNSIGNED (*common_type)))
    return true;

  /* See if NEW_TYPE can hold all values of *COMMON_TYPE.  */
  if (TYPE_PRECISION (*common_type) < TYPE_PRECISION (new_type)
      && (TYPE_UNSIGNED (*common_type) || !TYPE_UNSIGNED (new_type)))
    {
      *common_type = new_type;
      return true;
    }

  /* We have mismatched signs, with the signed type being
     no wider than the unsigned type.  In this case we need
     a wider signed type.  */
  unsigned int precision = MAX (TYPE_PRECISION (*common_type),
				TYPE_PRECISION (new_type));
  precision *= 2;
  if (precision * 2 > TYPE_PRECISION (type))
    return false;

  *common_type = build_nonstandard_integer_type (precision, false);
  return true;
}

/* Check whether STMT_INFO can be viewed as a tree of integer operations
   in which each node either performs CODE or WIDENED_CODE, and where
   each leaf operand is narrower than the result of STMT_INFO.  MAX_NOPS
   specifies the maximum number of leaf operands.  SHIFT_P says whether
   CODE and WIDENED_CODE are some sort of shift.

   If STMT_INFO is such a tree, return the number of leaf operands
   and describe them in UNPROM[0] onwards.  Also set *COMMON_TYPE
   to a type that (a) is narrower than the result of STMT_INFO and
   (b) can hold all leaf operand values.

   Return 0 if STMT_INFO isn't such a tree, or if no such COMMON_TYPE
   exists.  */

static unsigned int
vect_widened_op_tree (stmt_vec_info stmt_info, tree_code code,
		      tree_code widened_code, bool shift_p,
		      unsigned int max_nops,
		      vect_unpromoted_value *unprom, tree *common_type)
{
  /* Check for an integer operation with the right code.  */
  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (!assign)
    return 0;

  tree_code rhs_code = gimple_assign_rhs_code (assign);
  if (rhs_code != code && rhs_code != widened_code)
    return 0;

  tree type = gimple_expr_type (assign);
  if (!INTEGRAL_TYPE_P (type))
    return 0;

  /* Assume that both operands will be leaf operands.  */
  max_nops -= 2;

  /* Check the operands.  */
  unsigned int next_op = 0;
  for (unsigned int i = 0; i < 2; ++i)
    {
      vect_unpromoted_value *this_unprom = &unprom[next_op];
      unsigned int nops = 1;
      tree op = gimple_op (assign, i + 1);
      if (i == 1 && TREE_CODE (op) == INTEGER_CST)
	{
	  /* We already have a common type from earlier operands.
	     Update it to account for OP.  */
	  this_unprom->set_op (op, vect_constant_def);
	  if (!vect_joust_widened_integer (type, shift_p, op, common_type))
	    return 0;
	}
      else
	{
	  /* Only allow shifts by constants.  */
	  if (shift_p && i == 1)
	    return 0;

	  if (!vect_look_through_possible_promotion (stmt_info->vinfo, op,
						     this_unprom))
	    return 0;

	  if (TYPE_PRECISION (this_unprom->type) == TYPE_PRECISION (type))
	    {
	      /* The operand isn't widened.  If STMT_INFO has the code
		 for an unwidened operation, recursively check whether
		 this operand is a node of the tree.  */
	      if (rhs_code != code
		  || max_nops == 0
		  || this_unprom->dt != vect_internal_def)
		return 0;

	      /* Give back the leaf slot allocated above now that we're
		 not treating this as a leaf operand.  */
	      max_nops += 1;

	      /* Recursively process the definition of the operand.  */
	      stmt_vec_info def_stmt_info
		= vinfo_for_stmt (SSA_NAME_DEF_STMT (this_unprom->op));
	      nops = vect_widened_op_tree (def_stmt_info, code, widened_code,
					   shift_p, max_nops, this_unprom,
					   common_type);
	      if (nops == 0)
		return 0;

	      max_nops -= nops;
	    }
	  else
	    {
	      /* Make sure that the operand is narrower than the result.  */
	      if (TYPE_PRECISION (this_unprom->type) * 2
		  > TYPE_PRECISION (type))
		return 0;

	      /* Update COMMON_TYPE for the new operand.  */
	      if (i == 0)
		*common_type = this_unprom->type;
	      else if (!vect_joust_widened_type (type, this_unprom->type,
						 common_type))
		return 0;
	    }
	}
      next_op += nops;
    }
  return next_op;
}

/* Helper to return a new temporary for pattern of TYPE for STMT.  If STMT
   is NULL, the caller must set SSA_NAME_DEF_STMT for the returned SSA var. */

static tree
vect_recog_temp_ssa_var (tree type, gimple *stmt)
{
  return make_temp_ssa_name (type, stmt, "patt");
}

/* Convert UNPROM to TYPE and return the result, adding new statements
   to STMT_INFO's pattern definition statements if no better way is
   available.  VECTYPE is the vector form of TYPE.  */

static tree
vect_convert_input (stmt_vec_info stmt_info, tree type,
		    vect_unpromoted_value *unprom, tree vectype)
{
  /* Check for a no-op conversion.  */
  if (types_compatible_p (type, TREE_TYPE (unprom->op)))
    return unprom->op;

  /* Allow the caller to create constant vect_unpromoted_values.  */
  if (TREE_CODE (unprom->op) == INTEGER_CST)
    return wide_int_to_tree (type, wi::to_widest (unprom->op));

  /* See if we can reuse an existing result.  */
  if (unprom->caster)
    {
      tree lhs = gimple_get_lhs (unprom->caster->stmt);
      if (types_compatible_p (TREE_TYPE (lhs), type))
	return lhs;
    }

  /* We need a new conversion statement.  */
  tree new_op = vect_recog_temp_ssa_var (type, NULL);
  gassign *new_stmt = gimple_build_assign (new_op, NOP_EXPR, unprom->op);

  /* If OP is an external value, see if we can insert the new statement
     on an incoming edge.  */
  if (unprom->dt == vect_external_def)
    if (edge e = vect_get_external_def_edge (stmt_info->vinfo, unprom->op))
      {
	basic_block new_bb = gsi_insert_on_edge_immediate (e, new_stmt);
	gcc_assert (!new_bb);
	return new_op;
      }

  /* As a (common) last resort, add the statement to the pattern itself.  */
  append_pattern_def_seq (stmt_info, new_stmt, vectype);
  return new_op;
}

/* Invoke vect_convert_input for N elements of UNPROM and store the
   result in the corresponding elements of RESULT.  */

static void
vect_convert_inputs (stmt_vec_info stmt_info, unsigned int n,
		     tree *result, tree type, vect_unpromoted_value *unprom,
		     tree vectype)
{
  for (unsigned int i = 0; i < n; ++i)
    {
      unsigned int j;
      for (j = 0; j < i; ++j)
	if (unprom[j].op == unprom[i].op)
	  break;
      if (j < i)
	result[i] = result[j];
      else
	result[i] = vect_convert_input (stmt_info, type, &unprom[i], vectype);
    }
}

/* The caller has created a (possibly empty) sequence of pattern definition
   statements followed by a single statement PATTERN_STMT.  Cast the result
   of this final statement to TYPE.  If a new statement is needed, add
   PATTERN_STMT to the end of STMT_INFO's pattern definition statements
   and return the new statement, otherwise return PATTERN_STMT as-is.
   VECITYPE is the vector form of PATTERN_STMT's result type.  */

static gimple *
vect_convert_output (stmt_vec_info stmt_info, tree type, gimple *pattern_stmt,
		     tree vecitype)
{
  tree lhs = gimple_get_lhs (pattern_stmt);
  if (!types_compatible_p (type, TREE_TYPE (lhs)))
    {
      append_pattern_def_seq (stmt_info, pattern_stmt, vecitype);
      tree cast_var = vect_recog_temp_ssa_var (type, NULL);
      pattern_stmt = gimple_build_assign (cast_var, NOP_EXPR, lhs);
    }
  return pattern_stmt;
}

/* Return true if STMT_VINFO describes a reduction for which reassociation
   is allowed.  If STMT_INFO is part of a group, assume that it's part of
   a reduction chain and optimistically assume that all statements
   except the last allow reassociation.  */

static bool
vect_reassociating_reduction_p (stmt_vec_info stmt_vinfo)
{
  return (STMT_VINFO_DEF_TYPE (stmt_vinfo) == vect_reduction_def
	  ? STMT_VINFO_REDUC_TYPE (stmt_vinfo) != FOLD_LEFT_REDUCTION
	  : REDUC_GROUP_FIRST_ELEMENT (stmt_vinfo) != NULL);
}

/* As above, but also require it to have code CODE and to be a reduction
   in the outermost loop.  When returning true, store the operands in
   *OP0_OUT and *OP1_OUT.  */

static bool
vect_reassociating_reduction_p (stmt_vec_info stmt_info, tree_code code,
				tree *op0_out, tree *op1_out)
{
  loop_vec_info loop_info = STMT_VINFO_LOOP_VINFO (stmt_info);
  if (!loop_info)
    return false;

  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (!assign || gimple_assign_rhs_code (assign) != code)
    return false;

  /* We don't allow changing the order of the computation in the inner-loop
     when doing outer-loop vectorization.  */
  struct loop *loop = LOOP_VINFO_LOOP (loop_info);
  if (loop && nested_in_vect_loop_p (loop, assign))
    return false;

  if (!vect_reassociating_reduction_p (stmt_info))
    return false;

  *op0_out = gimple_assign_rhs1 (assign);
  *op1_out = gimple_assign_rhs2 (assign);
  return true;
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

static gimple *
vect_recog_dot_prod_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = (*stmts)[0];
  tree oprnd0, oprnd1;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  vec_info *vinfo = stmt_vinfo->vinfo;
  tree type, half_type;
  gimple *pattern_stmt;
  tree var;

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

  if (!vect_reassociating_reduction_p (stmt_vinfo, PLUS_EXPR,
				       &oprnd0, &oprnd1))
    return NULL;

  type = gimple_expr_type (last_stmt);

  vect_unpromoted_value unprom_mult;
  oprnd0 = vect_look_through_possible_promotion (vinfo, oprnd0, &unprom_mult);

  /* So far so good.  Since last_stmt was detected as a (summation) reduction,
     we know that oprnd1 is the reduction variable (defined by a loop-header
     phi), and oprnd0 is an ssa-name defined by a stmt in the loop body.
     Left to check that oprnd0 is defined by a (widen_)mult_expr  */
  if (!oprnd0)
    return NULL;

  stmt_vec_info mult_vinfo = vect_get_internal_def (vinfo, oprnd0);
  if (!mult_vinfo)
    return NULL;

  /* FORNOW.  Can continue analyzing the def-use chain when this stmt in a phi
     inside the loop (in case we are analyzing an outer-loop).  */
  vect_unpromoted_value unprom0[2];
  if (!vect_widened_op_tree (mult_vinfo, MULT_EXPR, WIDEN_MULT_EXPR,
			     false, 2, unprom0, &half_type))
    return NULL;

  /* If there are two widening operations, make sure they agree on
     the sign of the extension.  */
  if (TYPE_PRECISION (unprom_mult.type) != TYPE_PRECISION (type)
      && TYPE_SIGN (unprom_mult.type) != TYPE_SIGN (half_type))
    return NULL;

  vect_pattern_detected ("vect_recog_dot_prod_pattern", last_stmt);

  tree half_vectype;
  if (!vect_supportable_direct_optab_p (type, DOT_PROD_EXPR, half_type,
					type_out, &half_vectype))
    return NULL;

  /* Get the inputs in the appropriate types.  */
  STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) = NULL;
  tree mult_oprnd[2];
  vect_convert_inputs (stmt_vinfo, 2, mult_oprnd, half_type,
		       unprom0, half_vectype);

  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign (var, DOT_PROD_EXPR,
				      mult_oprnd[0], mult_oprnd[1], oprnd1);

  return pattern_stmt;
}


/* Function vect_recog_sad_pattern

   Try to find the following Sum of Absolute Difference (SAD) pattern:

     type x_t, y_t;
     signed TYPE1 diff, abs_diff;
     TYPE2 sum = init;
   loop:
     sum_0 = phi <init, sum_1>
     S1  x_t = ...
     S2  y_t = ...
     S3  x_T = (TYPE1) x_t;
     S4  y_T = (TYPE1) y_t;
     S5  diff = x_T - y_T;
     S6  abs_diff = ABS_EXPR <diff>;
     [S7  abs_diff = (TYPE2) abs_diff;  #optional]
     S8  sum_1 = abs_diff + sum_0;

   where 'TYPE1' is at least double the size of type 'type', and 'TYPE2' is the
   same size of 'TYPE1' or bigger. This is a special case of a reduction
   computation.

   Input:

   * STMTS: Contains a stmt from which the pattern search begins.  In the
   example, when this function is called with S8, the pattern
   {S3,S4,S5,S6,S7,S8} will be detected.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        SAD_EXPR <x_t, y_t, sum_0>
  */

static gimple *
vect_recog_sad_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = (*stmts)[0];
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  vec_info *vinfo = stmt_vinfo->vinfo;
  tree half_type;

  /* Look for the following pattern
          DX = (TYPE1) X;
          DY = (TYPE1) Y;
          DDIFF = DX - DY;
          DAD = ABS_EXPR <DDIFF>;
          DDPROD = (TYPE2) DPROD;
          sum_1 = DAD + sum_0;
     In which
     - DX is at least double the size of X
     - DY is at least double the size of Y
     - DX, DY, DDIFF, DAD all have the same type
     - sum is the same size of DAD or bigger
     - sum has been recognized as a reduction variable.

     This is equivalent to:
       DDIFF = X w- Y;          #widen sub
       DAD = ABS_EXPR <DDIFF>;
       sum_1 = DAD w+ sum_0;    #widen summation
     or
       DDIFF = X w- Y;          #widen sub
       DAD = ABS_EXPR <DDIFF>;
       sum_1 = DAD + sum_0;     #summation
   */

  /* Starting from LAST_STMT, follow the defs of its uses in search
     of the above pattern.  */

  tree plus_oprnd0, plus_oprnd1;
  if (!vect_reassociating_reduction_p (stmt_vinfo, PLUS_EXPR,
				       &plus_oprnd0, &plus_oprnd1))
    return NULL;

  tree sum_type = gimple_expr_type (last_stmt);

  /* Any non-truncating sequence of conversions is OK here, since
     with a successful match, the result of the ABS(U) is known to fit
     within the nonnegative range of the result type.  (It cannot be the
     negative of the minimum signed value due to the range of the widening
     MINUS_EXPR.)  */
  vect_unpromoted_value unprom_abs;
  plus_oprnd0 = vect_look_through_possible_promotion (vinfo, plus_oprnd0,
						      &unprom_abs);

  /* So far so good.  Since last_stmt was detected as a (summation) reduction,
     we know that plus_oprnd1 is the reduction variable (defined by a loop-header
     phi), and plus_oprnd0 is an ssa-name defined by a stmt in the loop body.
     Then check that plus_oprnd0 is defined by an abs_expr.  */

  if (!plus_oprnd0)
    return NULL;

  stmt_vec_info abs_stmt_vinfo = vect_get_internal_def (vinfo, plus_oprnd0);
  if (!abs_stmt_vinfo)
    return NULL;

  /* FORNOW.  Can continue analyzing the def-use chain when this stmt in a phi
     inside the loop (in case we are analyzing an outer-loop).  */
  gassign *abs_stmt = dyn_cast <gassign *> (abs_stmt_vinfo->stmt);
  if (!abs_stmt
      || (gimple_assign_rhs_code (abs_stmt) != ABS_EXPR
	  && gimple_assign_rhs_code (abs_stmt) != ABSU_EXPR))
    return NULL;

  tree abs_oprnd = gimple_assign_rhs1 (abs_stmt);
  tree abs_type = TREE_TYPE (abs_oprnd);
  if (TYPE_UNSIGNED (abs_type))
    return NULL;

  /* Peel off conversions from the ABS input.  This can involve sign
     changes (e.g. from an unsigned subtraction to a signed ABS input)
     or signed promotion, but it can't include unsigned promotion.
     (Note that ABS of an unsigned promotion should have been folded
     away before now anyway.)  */
  vect_unpromoted_value unprom_diff;
  abs_oprnd = vect_look_through_possible_promotion (vinfo, abs_oprnd,
						    &unprom_diff);
  if (!abs_oprnd)
    return NULL;
  if (TYPE_PRECISION (unprom_diff.type) != TYPE_PRECISION (abs_type)
      && TYPE_UNSIGNED (unprom_diff.type))
    return NULL;

  /* We then detect if the operand of abs_expr is defined by a minus_expr.  */
  stmt_vec_info diff_stmt_vinfo = vect_get_internal_def (vinfo, abs_oprnd);
  if (!diff_stmt_vinfo)
    return NULL;

  /* FORNOW.  Can continue analyzing the def-use chain when this stmt in a phi
     inside the loop (in case we are analyzing an outer-loop).  */
  vect_unpromoted_value unprom[2];
  if (!vect_widened_op_tree (diff_stmt_vinfo, MINUS_EXPR, MINUS_EXPR,
			     false, 2, unprom, &half_type))
    return NULL;

  vect_pattern_detected ("vect_recog_sad_pattern", last_stmt);

  tree half_vectype;
  if (!vect_supportable_direct_optab_p (sum_type, SAD_EXPR, half_type,
					type_out, &half_vectype))
    return NULL;

  /* Get the inputs to the SAD_EXPR in the appropriate types.  */
  STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) = NULL;
  tree sad_oprnd[2];
  vect_convert_inputs (stmt_vinfo, 2, sad_oprnd, half_type,
		       unprom, half_vectype);

  tree var = vect_recog_temp_ssa_var (sum_type, NULL);
  gimple *pattern_stmt = gimple_build_assign (var, SAD_EXPR, sad_oprnd[0],
					      sad_oprnd[1], plus_oprnd1);

  return pattern_stmt;
}

/* Recognize an operation that performs ORIG_CODE on widened inputs,
   so that it can be treated as though it had the form:

      A_TYPE a;
      B_TYPE b;
      HALF_TYPE a_cast = (HALF_TYPE) a;  // possible no-op
      HALF_TYPE b_cast = (HALF_TYPE) b;  // possible no-op
    | RES_TYPE a_extend = (RES_TYPE) a_cast;  // promotion from HALF_TYPE
    | RES_TYPE b_extend = (RES_TYPE) b_cast;  // promotion from HALF_TYPE
    | RES_TYPE res = a_extend ORIG_CODE b_extend;

   Try to replace the pattern with:

      A_TYPE a;
      B_TYPE b;
      HALF_TYPE a_cast = (HALF_TYPE) a;  // possible no-op
      HALF_TYPE b_cast = (HALF_TYPE) b;  // possible no-op
    | EXT_TYPE ext = a_cast WIDE_CODE b_cast;
    | RES_TYPE res = (EXT_TYPE) ext;  // possible no-op

   where EXT_TYPE is wider than HALF_TYPE but has the same signedness.

   SHIFT_P is true if ORIG_CODE and WIDE_CODE are shifts.  NAME is the
   name of the pattern being matched, for dump purposes.  */

static gimple *
vect_recog_widen_op_pattern (vec<gimple *> *stmts, tree *type_out,
			     tree_code orig_code, tree_code wide_code,
			     bool shift_p, const char *name)
{
  gimple *last_stmt = stmts->pop ();
  stmt_vec_info last_stmt_info = vinfo_for_stmt (last_stmt);

  vect_unpromoted_value unprom[2];
  tree half_type;
  if (!vect_widened_op_tree (last_stmt_info, orig_code, orig_code,
			     shift_p, 2, unprom, &half_type))
    return NULL;

  /* Pattern detected.  */
  vect_pattern_detected (name, last_stmt);

  tree type = gimple_expr_type (last_stmt);
  tree itype = type;
  if (TYPE_PRECISION (type) != TYPE_PRECISION (half_type) * 2
      || TYPE_UNSIGNED (type) != TYPE_UNSIGNED (half_type))
    itype = build_nonstandard_integer_type (TYPE_PRECISION (half_type) * 2,
					    TYPE_UNSIGNED (half_type));

  /* Check target support  */
  tree vectype = get_vectype_for_scalar_type (half_type);
  tree vecitype = get_vectype_for_scalar_type (itype);
  enum tree_code dummy_code;
  int dummy_int;
  auto_vec<tree> dummy_vec;
  if (!vectype
      || !vecitype
      || !supportable_widening_operation (wide_code, last_stmt,
					  vecitype, vectype,
					  &dummy_code, &dummy_code,
					  &dummy_int, &dummy_vec))
    return NULL;

  *type_out = get_vectype_for_scalar_type (type);
  if (!*type_out)
    return NULL;

  STMT_VINFO_PATTERN_DEF_SEQ (last_stmt_info) = NULL;
  tree oprnd[2];
  vect_convert_inputs (last_stmt_info, 2, oprnd, half_type, unprom, vectype);

  tree var = vect_recog_temp_ssa_var (itype, NULL);
  gimple *pattern_stmt = gimple_build_assign (var, wide_code,
					      oprnd[0], oprnd[1]);

  stmts->safe_push (last_stmt);
  return vect_convert_output (last_stmt_info, type, pattern_stmt, vecitype);
}

/* Try to detect multiplication on widened inputs, converting MULT_EXPR
   to WIDEN_MULT_EXPR.  See vect_recog_widen_op_pattern for details.  */

static gimple *
vect_recog_widen_mult_pattern (vec<gimple *> *stmts, tree *type_out)
{
  return vect_recog_widen_op_pattern (stmts, type_out, MULT_EXPR,
				      WIDEN_MULT_EXPR, false,
				      "vect_recog_widen_mult_pattern");
}

/* Function vect_recog_pow_pattern

   Try to find the following pattern:

     x = POW (y, N);

   with POW being one of pow, powf, powi, powif and N being
   either 2 or 0.5.

   Input:

   * LAST_STMT: A stmt from which the pattern search begins.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        x = x * x
   or
	x = sqrt (x)
*/

static gimple *
vect_recog_pow_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = (*stmts)[0];
  tree base, exp;
  gimple *stmt;
  tree var;

  if (!is_gimple_call (last_stmt) || gimple_call_lhs (last_stmt) == NULL)
    return NULL;

  switch (gimple_call_combined_fn (last_stmt))
    {
    CASE_CFN_POW:
    CASE_CFN_POWI:
      break;

    default:
      return NULL;
    }

  base = gimple_call_arg (last_stmt, 0);
  exp = gimple_call_arg (last_stmt, 1);
  if (TREE_CODE (exp) != REAL_CST
      && TREE_CODE (exp) != INTEGER_CST)
    {
      if (flag_unsafe_math_optimizations
	  && TREE_CODE (base) == REAL_CST
	  && !gimple_call_internal_p (last_stmt))
	{
	  combined_fn log_cfn;
	  built_in_function exp_bfn;
	  switch (DECL_FUNCTION_CODE (gimple_call_fndecl (last_stmt)))
	    {
	    case BUILT_IN_POW:
	      log_cfn = CFN_BUILT_IN_LOG;
	      exp_bfn = BUILT_IN_EXP;
	      break;
	    case BUILT_IN_POWF:
	      log_cfn = CFN_BUILT_IN_LOGF;
	      exp_bfn = BUILT_IN_EXPF;
	      break;
	    case BUILT_IN_POWL:
	      log_cfn = CFN_BUILT_IN_LOGL;
	      exp_bfn = BUILT_IN_EXPL;
	      break;
	    default:
	      return NULL;
	    }
	  tree logc = fold_const_call (log_cfn, TREE_TYPE (base), base);
	  tree exp_decl = builtin_decl_implicit (exp_bfn);
	  /* Optimize pow (C, x) as exp (log (C) * x).  Normally match.pd
	     does that, but if C is a power of 2, we want to use
	     exp2 (log2 (C) * x) in the non-vectorized version, but for
	     vectorization we don't have vectorized exp2.  */
	  if (logc
	      && TREE_CODE (logc) == REAL_CST
	      && exp_decl
	      && lookup_attribute ("omp declare simd",
				   DECL_ATTRIBUTES (exp_decl)))
	    {
	      cgraph_node *node = cgraph_node::get_create (exp_decl);
	      if (node->simd_clones == NULL)
		{
		  if (targetm.simd_clone.compute_vecsize_and_simdlen == NULL
		      || node->definition)
		    return NULL;
		  expand_simd_clones (node);
		  if (node->simd_clones == NULL)
		    return NULL;
		}
	      *type_out = get_vectype_for_scalar_type (TREE_TYPE (base));
	      if (!*type_out)
		return NULL;
	      stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
	      tree def = vect_recog_temp_ssa_var (TREE_TYPE (base), NULL);
	      gimple *g = gimple_build_assign (def, MULT_EXPR, exp, logc);
	      new_pattern_def_seq (stmt_vinfo, g);
	      tree res = vect_recog_temp_ssa_var (TREE_TYPE (base), NULL);
	      g = gimple_build_call (exp_decl, 1, def);
	      gimple_call_set_lhs (g, res);
	      return g;
	    }
	}

      return NULL;
    }

  /* We now have a pow or powi builtin function call with a constant
     exponent.  */

  /* Catch squaring.  */
  if ((tree_fits_shwi_p (exp)
       && tree_to_shwi (exp) == 2)
      || (TREE_CODE (exp) == REAL_CST
          && real_equal (&TREE_REAL_CST (exp), &dconst2)))
    {
      if (!vect_supportable_direct_optab_p (TREE_TYPE (base), MULT_EXPR,
					    TREE_TYPE (base), type_out))
	return NULL;

      var = vect_recog_temp_ssa_var (TREE_TYPE (base), NULL);
      stmt = gimple_build_assign (var, MULT_EXPR, base, base);
      return stmt;
    }

  /* Catch square root.  */
  if (TREE_CODE (exp) == REAL_CST
      && real_equal (&TREE_REAL_CST (exp), &dconsthalf))
    {
      *type_out = get_vectype_for_scalar_type (TREE_TYPE (base));
      if (*type_out
	  && direct_internal_fn_supported_p (IFN_SQRT, *type_out,
					     OPTIMIZE_FOR_SPEED))
	{
	  gcall *stmt = gimple_build_call_internal (IFN_SQRT, 1, base);
	  var = vect_recog_temp_ssa_var (TREE_TYPE (base), stmt);
	  gimple_call_set_lhs (stmt, var);
	  gimple_call_set_nothrow (stmt, true);
	  return stmt;
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

static gimple *
vect_recog_widen_sum_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = (*stmts)[0];
  tree oprnd0, oprnd1;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  vec_info *vinfo = stmt_vinfo->vinfo;
  tree type;
  gimple *pattern_stmt;
  tree var;

  /* Look for the following pattern
          DX = (TYPE) X;
          sum_1 = DX + sum_0;
     In which DX is at least double the size of X, and sum_1 has been
     recognized as a reduction variable.
   */

  /* Starting from LAST_STMT, follow the defs of its uses in search
     of the above pattern.  */

  if (!vect_reassociating_reduction_p (stmt_vinfo, PLUS_EXPR,
				       &oprnd0, &oprnd1))
    return NULL;

  type = gimple_expr_type (last_stmt);

  /* So far so good.  Since last_stmt was detected as a (summation) reduction,
     we know that oprnd1 is the reduction variable (defined by a loop-header
     phi), and oprnd0 is an ssa-name defined by a stmt in the loop body.
     Left to check that oprnd0 is defined by a cast from type 'type' to type
     'TYPE'.  */

  vect_unpromoted_value unprom0;
  if (!vect_look_through_possible_promotion (vinfo, oprnd0, &unprom0)
      || TYPE_PRECISION (unprom0.type) * 2 > TYPE_PRECISION (type))
    return NULL;

  vect_pattern_detected ("vect_recog_widen_sum_pattern", last_stmt);

  if (!vect_supportable_direct_optab_p (type, WIDEN_SUM_EXPR, unprom0.type,
					type_out))
    return NULL;

  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign (var, WIDEN_SUM_EXPR, unprom0.op, oprnd1);

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
	 in NEW_DEF_STMT and further store it in STMT_VINFO_PATTERN_DEF_SEQ of
         the second pattern statement.  */

static bool
vect_operation_fits_smaller_type (gimple *stmt, tree def, tree *new_type,
				  tree *op0, tree *op1, gimple **new_def_stmt,
				  vec<gimple *> *stmts)
{
  enum tree_code code;
  tree const_oprnd, oprnd;
  tree interm_type = NULL_TREE, half_type, new_oprnd, type;
  gimple *def_stmt, *new_stmt;
  bool first = false;
  bool promotion;

  *op0 = NULL_TREE;
  *op1 = NULL_TREE;
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

  /* If oprnd has other uses besides that in stmt we cannot mark it
     as being part of a pattern only.  */
  if (!has_single_use (oprnd))
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
      if (!type_conversion_p (oprnd, stmt, false, &half_type, &def_stmt,
			      &promotion)
	  || !promotion
	  || !vect_same_loop_or_bb_p (stmt, def_stmt))
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
	   STMT_VINFO_PATTERN_DEF_SEQ of the pattern statement for STMT.
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
                  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (new_stmt))
                  || TREE_TYPE (gimple_assign_lhs (new_stmt)) != interm_type)
                return false;

	      stmts->safe_push (def_stmt);
              oprnd = gimple_assign_lhs (new_stmt);
            }
          else
            {
              /* Create NEW_OPRND = (INTERM_TYPE) OPRND.  */
              oprnd = gimple_assign_rhs1 (def_stmt);
	      new_oprnd = make_ssa_name (interm_type);
	      new_stmt = gimple_build_assign (new_oprnd, NOP_EXPR, oprnd);
              STMT_VINFO_RELATED_STMT (vinfo_for_stmt (def_stmt)) = new_stmt;
              stmts->safe_push (def_stmt);
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
	  new_oprnd = make_ssa_name (interm_type);
	  new_stmt = gimple_build_assign (new_oprnd, NOP_EXPR, oprnd);
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
   demotion operation.  We also check that S3 and S4 have only one use.  */

static gimple *
vect_recog_over_widening_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *stmt = stmts->pop ();
  gimple *pattern_stmt = NULL, *new_def_stmt, *prev_stmt = NULL,
	 *use_stmt = NULL;
  tree op0, op1, vectype = NULL_TREE, use_lhs, use_type;
  tree var = NULL_TREE, new_type = NULL_TREE, new_oprnd;
  bool first;
  tree type = NULL;

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
      use_stmt = vect_single_imm_use (stmt);
      if (!use_stmt || !is_gimple_assign (use_stmt))
        return NULL;

      /* Create pattern statement for STMT.  */
      vectype = get_vectype_for_scalar_type (new_type);
      if (!vectype)
        return NULL;

      /* We want to collect all the statements for which we create pattern
         statetments, except for the case when the last statement in the
         sequence doesn't have a corresponding pattern statement.  In such
         case we associate the last pattern statement with the last statement
         in the sequence.  Therefore, we only add the original statement to
         the list if we know that it is not the last.  */
      if (prev_stmt)
        stmts->safe_push (prev_stmt);

      var = vect_recog_temp_ssa_var (new_type, NULL);
      pattern_stmt
	= gimple_build_assign (var, gimple_assign_rhs_code (stmt), op0, op1);
      STMT_VINFO_RELATED_STMT (vinfo_for_stmt (stmt)) = pattern_stmt;
      new_pattern_def_seq (vinfo_for_stmt (stmt), new_def_stmt);

      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
                           "created pattern stmt: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, pattern_stmt, 0);
        }

      type = gimple_expr_type (stmt);
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
      /* Support only type demotion or signedess change.  */
      if (!INTEGRAL_TYPE_P (use_type)
	  || TYPE_PRECISION (type) <= TYPE_PRECISION (use_type))
        return NULL;

      /* Check that NEW_TYPE is not bigger than the conversion result.  */
      if (TYPE_PRECISION (new_type) > TYPE_PRECISION (use_type))
	return NULL;

      if (TYPE_UNSIGNED (new_type) != TYPE_UNSIGNED (use_type)
          || TYPE_PRECISION (new_type) != TYPE_PRECISION (use_type))
        {
	  *type_out = get_vectype_for_scalar_type (use_type);
	  if (!*type_out)
	    return NULL;

          /* Create NEW_TYPE->USE_TYPE conversion.  */
	  new_oprnd = make_ssa_name (use_type);
	  pattern_stmt = gimple_build_assign (new_oprnd, NOP_EXPR, var);
          STMT_VINFO_RELATED_STMT (vinfo_for_stmt (use_stmt)) = pattern_stmt;

          /* We created a pattern statement for the last statement in the
             sequence, so we don't need to associate it with the pattern
             statement created for PREV_STMT.  Therefore, we add PREV_STMT
             to the list in order to mark it later in vect_pattern_recog_1.  */
          if (prev_stmt)
            stmts->safe_push (prev_stmt);
        }
      else
        {
          if (prev_stmt)
	    STMT_VINFO_PATTERN_DEF_SEQ (vinfo_for_stmt (use_stmt))
	       = STMT_VINFO_PATTERN_DEF_SEQ (vinfo_for_stmt (prev_stmt));

	  *type_out = vectype;
        }

      stmts->safe_push (use_stmt);
    }
  else
    /* TODO: support general case, create a conversion to the correct type.  */
    return NULL;

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_over_widening_pattern", stmts->last ());

  return pattern_stmt;
}

/* Try to detect a shift left of a widened input, converting LSHIFT_EXPR
   to WIDEN_LSHIFT_EXPR.  See vect_recog_widen_op_pattern for details.  */

static gimple *
vect_recog_widen_shift_pattern (vec<gimple *> *stmts, tree *type_out)
{
  return vect_recog_widen_op_pattern (stmts, type_out, LSHIFT_EXPR,
				      WIDEN_LSHIFT_EXPR, true,
				      "vect_widen_shift_pattern");
}

/* Detect a rotate pattern wouldn't be otherwise vectorized:

   type a_t, b_t, c_t;

   S0 a_t = b_t r<< c_t;

  Input/Output:

  * STMTS: Contains a stmt from which the pattern search begins,
    i.e. the shift/rotate stmt.  The original stmt (S0) is replaced
    with a sequence:

   S1 d_t = -c_t;
   S2 e_t = d_t & (B - 1);
   S3 f_t = b_t << c_t;
   S4 g_t = b_t >> e_t;
   S0 a_t = f_t | g_t;

    where B is element bitsize of type.

  Output:

  * TYPE_OUT: The type of the output of this pattern.

  * Return value: A new stmt that will be used to replace the rotate
    S0 stmt.  */

static gimple *
vect_recog_rotate_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  tree oprnd0, oprnd1, lhs, var, var1, var2, vectype, type, stype, def, def2;
  gimple *pattern_stmt, *def_stmt;
  enum tree_code rhs_code;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  vec_info *vinfo = stmt_vinfo->vinfo;
  enum vect_def_type dt;
  optab optab1, optab2;
  edge ext_def = NULL;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  rhs_code = gimple_assign_rhs_code (last_stmt);
  switch (rhs_code)
    {
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      break;
    default:
      return NULL;
    }

  lhs = gimple_assign_lhs (last_stmt);
  oprnd0 = gimple_assign_rhs1 (last_stmt);
  type = TREE_TYPE (oprnd0);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  if (TREE_CODE (oprnd0) != SSA_NAME
      || TYPE_PRECISION (TREE_TYPE (lhs)) != TYPE_PRECISION (type)
      || !INTEGRAL_TYPE_P (type)
      || !TYPE_UNSIGNED (type))
    return NULL;

  if (!vect_is_simple_use (oprnd1, vinfo, &dt, &def_stmt))
    return NULL;

  if (dt != vect_internal_def
      && dt != vect_constant_def
      && dt != vect_external_def)
    return NULL;

  vectype = get_vectype_for_scalar_type (type);
  if (vectype == NULL_TREE)
    return NULL;

  /* If vector/vector or vector/scalar rotate is supported by the target,
     don't do anything here.  */
  optab1 = optab_for_tree_code (rhs_code, vectype, optab_vector);
  if (optab1
      && optab_handler (optab1, TYPE_MODE (vectype)) != CODE_FOR_nothing)
    return NULL;

  if (is_a <bb_vec_info> (vinfo) || dt != vect_internal_def)
    {
      optab2 = optab_for_tree_code (rhs_code, vectype, optab_scalar);
      if (optab2
	  && optab_handler (optab2, TYPE_MODE (vectype)) != CODE_FOR_nothing)
	return NULL;
    }

  /* If vector/vector or vector/scalar shifts aren't supported by the target,
     don't do anything here either.  */
  optab1 = optab_for_tree_code (LSHIFT_EXPR, vectype, optab_vector);
  optab2 = optab_for_tree_code (RSHIFT_EXPR, vectype, optab_vector);
  if (!optab1
      || optab_handler (optab1, TYPE_MODE (vectype)) == CODE_FOR_nothing
      || !optab2
      || optab_handler (optab2, TYPE_MODE (vectype)) == CODE_FOR_nothing)
    {
      if (! is_a <bb_vec_info> (vinfo) && dt == vect_internal_def)
	return NULL;
      optab1 = optab_for_tree_code (LSHIFT_EXPR, vectype, optab_scalar);
      optab2 = optab_for_tree_code (RSHIFT_EXPR, vectype, optab_scalar);
      if (!optab1
	  || optab_handler (optab1, TYPE_MODE (vectype)) == CODE_FOR_nothing
	  || !optab2
	  || optab_handler (optab2, TYPE_MODE (vectype)) == CODE_FOR_nothing)
	return NULL;
    }

  *type_out = vectype;

  if (dt == vect_external_def
      && TREE_CODE (oprnd1) == SSA_NAME)
    ext_def = vect_get_external_def_edge (vinfo, oprnd1);

  def = NULL_TREE;
  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (type);
  if (TREE_CODE (oprnd1) == INTEGER_CST
      || TYPE_MODE (TREE_TYPE (oprnd1)) == mode)
    def = oprnd1;
  else if (def_stmt && gimple_assign_cast_p (def_stmt))
    {
      tree rhs1 = gimple_assign_rhs1 (def_stmt);
      if (TYPE_MODE (TREE_TYPE (rhs1)) == mode
	  && TYPE_PRECISION (TREE_TYPE (rhs1))
	     == TYPE_PRECISION (type))
	def = rhs1;
    }

  STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) = NULL;
  if (def == NULL_TREE)
    {
      def = vect_recog_temp_ssa_var (type, NULL);
      def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd1);
      if (ext_def)
	{
	  basic_block new_bb
	    = gsi_insert_on_edge_immediate (ext_def, def_stmt);
	  gcc_assert (!new_bb);
	}
      else
	append_pattern_def_seq (stmt_vinfo, def_stmt);
    }
  stype = TREE_TYPE (def);
  scalar_int_mode smode = SCALAR_INT_TYPE_MODE (stype);

  if (TREE_CODE (def) == INTEGER_CST)
    {
      if (!tree_fits_uhwi_p (def)
	  || tree_to_uhwi (def) >= GET_MODE_PRECISION (mode)
	  || integer_zerop (def))
	return NULL;
      def2 = build_int_cst (stype,
			    GET_MODE_PRECISION (mode) - tree_to_uhwi (def));
    }
  else
    {
      tree vecstype = get_vectype_for_scalar_type (stype);
      stmt_vec_info def_stmt_vinfo;

      if (vecstype == NULL_TREE)
	return NULL;
      def2 = vect_recog_temp_ssa_var (stype, NULL);
      def_stmt = gimple_build_assign (def2, NEGATE_EXPR, def);
      if (ext_def)
	{
	  basic_block new_bb
	    = gsi_insert_on_edge_immediate (ext_def, def_stmt);
	  gcc_assert (!new_bb);
	}
      else
	{
	  def_stmt_vinfo = new_stmt_vec_info (def_stmt, vinfo);
	  set_vinfo_for_stmt (def_stmt, def_stmt_vinfo);
	  STMT_VINFO_VECTYPE (def_stmt_vinfo) = vecstype;
	  append_pattern_def_seq (stmt_vinfo, def_stmt);
	}

      def2 = vect_recog_temp_ssa_var (stype, NULL);
      tree mask = build_int_cst (stype, GET_MODE_PRECISION (smode) - 1);
      def_stmt = gimple_build_assign (def2, BIT_AND_EXPR,
				      gimple_assign_lhs (def_stmt), mask);
      if (ext_def)
	{
	  basic_block new_bb
	    = gsi_insert_on_edge_immediate (ext_def, def_stmt);
	  gcc_assert (!new_bb);
	}
      else
	{
	  def_stmt_vinfo = new_stmt_vec_info (def_stmt, vinfo);
	  set_vinfo_for_stmt (def_stmt, def_stmt_vinfo);
	  STMT_VINFO_VECTYPE (def_stmt_vinfo) = vecstype;
	  append_pattern_def_seq (stmt_vinfo, def_stmt);
	}
    }

  var1 = vect_recog_temp_ssa_var (type, NULL);
  def_stmt = gimple_build_assign (var1, rhs_code == LROTATE_EXPR
					? LSHIFT_EXPR : RSHIFT_EXPR,
				  oprnd0, def);
  append_pattern_def_seq (stmt_vinfo, def_stmt);

  var2 = vect_recog_temp_ssa_var (type, NULL);
  def_stmt = gimple_build_assign (var2, rhs_code == LROTATE_EXPR
					? RSHIFT_EXPR : LSHIFT_EXPR,
				  oprnd0, def2);
  append_pattern_def_seq (stmt_vinfo, def_stmt);

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_rotate_pattern", last_stmt);

  /* Pattern supported.  Create a stmt to be used to replace the pattern.  */
  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign (var, BIT_IOR_EXPR, var1, var2);

  stmts->safe_push (last_stmt);
  return pattern_stmt;
}

/* Detect a vector by vector shift pattern that wouldn't be otherwise
   vectorized:

   type a_t;
   TYPE b_T, res_T;

   S1 a_t = ;
   S2 b_T = ;
   S3 res_T = b_T op a_t;

  where type 'TYPE' is a type with different size than 'type',
  and op is <<, >> or rotate.

  Also detect cases:

   type a_t;
   TYPE b_T, c_T, res_T;

   S0 c_T = ;
   S1 a_t = (type) c_T;
   S2 b_T = ;
   S3 res_T = b_T op a_t;

  Input/Output:

  * STMTS: Contains a stmt from which the pattern search begins,
    i.e. the shift/rotate stmt.  The original stmt (S3) is replaced
    with a shift/rotate which has same type on both operands, in the
    second case just b_T op c_T, in the first case with added cast
    from a_t to c_T in STMT_VINFO_PATTERN_DEF_SEQ.

  Output:

  * TYPE_OUT: The type of the output of this pattern.

  * Return value: A new stmt that will be used to replace the shift/rotate
    S3 stmt.  */

static gimple *
vect_recog_vector_vector_shift_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  tree oprnd0, oprnd1, lhs, var;
  gimple *pattern_stmt;
  enum tree_code rhs_code;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  vec_info *vinfo = stmt_vinfo->vinfo;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  rhs_code = gimple_assign_rhs_code (last_stmt);
  switch (rhs_code)
    {
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      break;
    default:
      return NULL;
    }

  lhs = gimple_assign_lhs (last_stmt);
  oprnd0 = gimple_assign_rhs1 (last_stmt);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  if (TREE_CODE (oprnd0) != SSA_NAME
      || TREE_CODE (oprnd1) != SSA_NAME
      || TYPE_MODE (TREE_TYPE (oprnd0)) == TYPE_MODE (TREE_TYPE (oprnd1))
      || !type_has_mode_precision_p (TREE_TYPE (oprnd1))
      || TYPE_PRECISION (TREE_TYPE (lhs))
	 != TYPE_PRECISION (TREE_TYPE (oprnd0)))
    return NULL;

  stmt_vec_info def_vinfo = vect_get_internal_def (vinfo, oprnd1);
  if (!def_vinfo)
    return NULL;

  *type_out = get_vectype_for_scalar_type (TREE_TYPE (oprnd0));
  if (*type_out == NULL_TREE)
    return NULL;

  tree def = NULL_TREE;
  gassign *def_stmt = dyn_cast <gassign *> (def_vinfo->stmt);
  if (def_stmt && gimple_assign_cast_p (def_stmt))
    {
      tree rhs1 = gimple_assign_rhs1 (def_stmt);
      if (TYPE_MODE (TREE_TYPE (rhs1)) == TYPE_MODE (TREE_TYPE (oprnd0))
	  && TYPE_PRECISION (TREE_TYPE (rhs1))
	     == TYPE_PRECISION (TREE_TYPE (oprnd0)))
	{
	  if (TYPE_PRECISION (TREE_TYPE (oprnd1))
	      >= TYPE_PRECISION (TREE_TYPE (rhs1)))
	    def = rhs1;
	  else
	    {
	      tree mask
		= build_low_bits_mask (TREE_TYPE (rhs1),
				       TYPE_PRECISION (TREE_TYPE (oprnd1)));
	      def = vect_recog_temp_ssa_var (TREE_TYPE (rhs1), NULL);
	      def_stmt = gimple_build_assign (def, BIT_AND_EXPR, rhs1, mask);
	      stmt_vec_info new_stmt_info
		= new_stmt_vec_info (def_stmt, vinfo);
	      set_vinfo_for_stmt (def_stmt, new_stmt_info);
	      STMT_VINFO_VECTYPE (new_stmt_info)
		= get_vectype_for_scalar_type (TREE_TYPE (rhs1));
	      new_pattern_def_seq (stmt_vinfo, def_stmt);
	    }
	}
    }

  if (def == NULL_TREE)
    {
      def = vect_recog_temp_ssa_var (TREE_TYPE (oprnd0), NULL);
      def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd1);
      new_pattern_def_seq (stmt_vinfo, def_stmt);
    }

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_vector_vector_shift_pattern", last_stmt);

  /* Pattern supported.  Create a stmt to be used to replace the pattern.  */
  var = vect_recog_temp_ssa_var (TREE_TYPE (oprnd0), NULL);
  pattern_stmt = gimple_build_assign (var, rhs_code, oprnd0, def);

  stmts->safe_push (last_stmt);
  return pattern_stmt;
}

/* Return true iff the target has a vector optab implementing the operation
   CODE on type VECTYPE.  */

static bool
target_has_vecop_for_code (tree_code code, tree vectype)
{
  optab voptab = optab_for_tree_code (code, vectype, optab_vector);
  return voptab
	 && optab_handler (voptab, TYPE_MODE (vectype)) != CODE_FOR_nothing;
}

/* Verify that the target has optabs of VECTYPE to perform all the steps
   needed by the multiplication-by-immediate synthesis algorithm described by
   ALG and VAR.  If SYNTH_SHIFT_P is true ensure that vector addition is
   present.  Return true iff the target supports all the steps.  */

static bool
target_supports_mult_synth_alg (struct algorithm *alg, mult_variant var,
				 tree vectype, bool synth_shift_p)
{
  if (alg->op[0] != alg_zero && alg->op[0] != alg_m)
    return false;

  bool supports_vminus = target_has_vecop_for_code (MINUS_EXPR, vectype);
  bool supports_vplus = target_has_vecop_for_code (PLUS_EXPR, vectype);

  if (var == negate_variant
      && !target_has_vecop_for_code (NEGATE_EXPR, vectype))
    return false;

  /* If we must synthesize shifts with additions make sure that vector
     addition is available.  */
  if ((var == add_variant || synth_shift_p) && !supports_vplus)
    return false;

  for (int i = 1; i < alg->ops; i++)
    {
      switch (alg->op[i])
	{
	case alg_shift:
	  break;
	case alg_add_t_m2:
	case alg_add_t2_m:
	case alg_add_factor:
	  if (!supports_vplus)
	    return false;
	  break;
	case alg_sub_t_m2:
	case alg_sub_t2_m:
	case alg_sub_factor:
	  if (!supports_vminus)
	    return false;
	  break;
	case alg_unknown:
	case alg_m:
	case alg_zero:
	case alg_impossible:
	  return false;
	default:
	  gcc_unreachable ();
	}
    }

  return true;
}

/* Synthesize a left shift of OP by AMNT bits using a series of additions and
   putting the final result in DEST.  Append all statements but the last into
   VINFO.  Return the last statement.  */

static gimple *
synth_lshift_by_additions (tree dest, tree op, HOST_WIDE_INT amnt,
			   stmt_vec_info vinfo)
{
  HOST_WIDE_INT i;
  tree itype = TREE_TYPE (op);
  tree prev_res = op;
  gcc_assert (amnt >= 0);
  for (i = 0; i < amnt; i++)
    {
      tree tmp_var = (i < amnt - 1) ? vect_recog_temp_ssa_var (itype, NULL)
		      : dest;
      gimple *stmt
        = gimple_build_assign (tmp_var, PLUS_EXPR, prev_res, prev_res);
      prev_res = tmp_var;
      if (i < amnt - 1)
	append_pattern_def_seq (vinfo, stmt);
      else
	return stmt;
    }
  gcc_unreachable ();
  return NULL;
}

/* Helper for vect_synth_mult_by_constant.  Apply a binary operation
   CODE to operands OP1 and OP2, creating a new temporary SSA var in
   the process if necessary.  Append the resulting assignment statements
   to the sequence in STMT_VINFO.  Return the SSA variable that holds the
   result of the binary operation.  If SYNTH_SHIFT_P is true synthesize
   left shifts using additions.  */

static tree
apply_binop_and_append_stmt (tree_code code, tree op1, tree op2,
			     stmt_vec_info stmt_vinfo, bool synth_shift_p)
{
  if (integer_zerop (op2)
      && (code == LSHIFT_EXPR
	  || code == PLUS_EXPR))
    {
      gcc_assert (TREE_CODE (op1) == SSA_NAME);
      return op1;
    }

  gimple *stmt;
  tree itype = TREE_TYPE (op1);
  tree tmp_var = vect_recog_temp_ssa_var (itype, NULL);

  if (code == LSHIFT_EXPR
      && synth_shift_p)
    {
      stmt = synth_lshift_by_additions (tmp_var, op1, TREE_INT_CST_LOW (op2),
					 stmt_vinfo);
      append_pattern_def_seq (stmt_vinfo, stmt);
      return tmp_var;
    }

  stmt = gimple_build_assign (tmp_var, code, op1, op2);
  append_pattern_def_seq (stmt_vinfo, stmt);
  return tmp_var;
}

/* Synthesize a multiplication of OP by an INTEGER_CST VAL using shifts
   and simple arithmetic operations to be vectorized.  Record the statements
   produced in STMT_VINFO and return the last statement in the sequence or
   NULL if it's not possible to synthesize such a multiplication.
   This function mirrors the behavior of expand_mult_const in expmed.c but
   works on tree-ssa form.  */

static gimple *
vect_synth_mult_by_constant (tree op, tree val,
			     stmt_vec_info stmt_vinfo)
{
  tree itype = TREE_TYPE (op);
  machine_mode mode = TYPE_MODE (itype);
  struct algorithm alg;
  mult_variant variant;
  if (!tree_fits_shwi_p (val))
    return NULL;

  /* Multiplication synthesis by shifts, adds and subs can introduce
     signed overflow where the original operation didn't.  Perform the
     operations on an unsigned type and cast back to avoid this.
     In the future we may want to relax this for synthesis algorithms
     that we can prove do not cause unexpected overflow.  */
  bool cast_to_unsigned_p = !TYPE_OVERFLOW_WRAPS (itype);

  tree multtype = cast_to_unsigned_p ? unsigned_type_for (itype) : itype;

  /* Targets that don't support vector shifts but support vector additions
     can synthesize shifts that way.  */
  bool synth_shift_p = !vect_supportable_shift (LSHIFT_EXPR, multtype);

  HOST_WIDE_INT hwval = tree_to_shwi (val);
  /* Use MAX_COST here as we don't want to limit the sequence on rtx costs.
     The vectorizer's benefit analysis will decide whether it's beneficial
     to do this.  */
  bool possible = choose_mult_variant (mode, hwval, &alg,
					&variant, MAX_COST);
  if (!possible)
    return NULL;

  tree vectype = get_vectype_for_scalar_type (multtype);

  if (!vectype
      || !target_supports_mult_synth_alg (&alg, variant,
					   vectype, synth_shift_p))
    return NULL;

  tree accumulator;

  /* Clear out the sequence of statements so we can populate it below.  */
  STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) = NULL;
  gimple *stmt = NULL;

  if (cast_to_unsigned_p)
    {
      tree tmp_op = vect_recog_temp_ssa_var (multtype, NULL);
      stmt = gimple_build_assign (tmp_op, CONVERT_EXPR, op);
      append_pattern_def_seq (stmt_vinfo, stmt);
      op = tmp_op;
    }

  if (alg.op[0] == alg_zero)
    accumulator = build_int_cst (multtype, 0);
  else
    accumulator = op;

  bool needs_fixup = (variant == negate_variant)
		      || (variant == add_variant);

  for (int i = 1; i < alg.ops; i++)
    {
      tree shft_log = build_int_cst (multtype, alg.log[i]);
      tree accum_tmp = vect_recog_temp_ssa_var (multtype, NULL);
      tree tmp_var = NULL_TREE;

      switch (alg.op[i])
	{
	case alg_shift:
	  if (synth_shift_p)
	    stmt
	      = synth_lshift_by_additions (accum_tmp, accumulator, alg.log[i],
					    stmt_vinfo);
	  else
	    stmt = gimple_build_assign (accum_tmp, LSHIFT_EXPR, accumulator,
					 shft_log);
	  break;
	case alg_add_t_m2:
	  tmp_var
	    = apply_binop_and_append_stmt (LSHIFT_EXPR, op, shft_log,
					    stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, accumulator,
				       tmp_var);
	  break;
	case alg_sub_t_m2:
	  tmp_var = apply_binop_and_append_stmt (LSHIFT_EXPR, op,
						  shft_log, stmt_vinfo,
						  synth_shift_p);
	  /* In some algorithms the first step involves zeroing the
	     accumulator.  If subtracting from such an accumulator
	     just emit the negation directly.  */
	  if (integer_zerop (accumulator))
	    stmt = gimple_build_assign (accum_tmp, NEGATE_EXPR, tmp_var);
	  else
	    stmt = gimple_build_assign (accum_tmp, MINUS_EXPR, accumulator,
					tmp_var);
	  break;
	case alg_add_t2_m:
	  tmp_var
	    = apply_binop_and_append_stmt (LSHIFT_EXPR, accumulator, shft_log,
					   stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, tmp_var, op);
	  break;
	case alg_sub_t2_m:
	  tmp_var
	    = apply_binop_and_append_stmt (LSHIFT_EXPR, accumulator, shft_log,
					   stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, MINUS_EXPR, tmp_var, op);
	  break;
	case alg_add_factor:
	  tmp_var
	    = apply_binop_and_append_stmt (LSHIFT_EXPR, accumulator, shft_log,
					    stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, accumulator,
				       tmp_var);
	  break;
	case alg_sub_factor:
	  tmp_var
	    = apply_binop_and_append_stmt (LSHIFT_EXPR, accumulator, shft_log,
					   stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, MINUS_EXPR, tmp_var,
				      accumulator);
	  break;
	default:
	  gcc_unreachable ();
	}
      /* We don't want to append the last stmt in the sequence to stmt_vinfo
	 but rather return it directly.  */

      if ((i < alg.ops - 1) || needs_fixup || cast_to_unsigned_p)
	append_pattern_def_seq (stmt_vinfo, stmt);
      accumulator = accum_tmp;
    }
  if (variant == negate_variant)
    {
      tree accum_tmp = vect_recog_temp_ssa_var (multtype, NULL);
      stmt = gimple_build_assign (accum_tmp, NEGATE_EXPR, accumulator);
      accumulator = accum_tmp;
      if (cast_to_unsigned_p)
	append_pattern_def_seq (stmt_vinfo, stmt);
    }
  else if (variant == add_variant)
    {
      tree accum_tmp = vect_recog_temp_ssa_var (multtype, NULL);
      stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, accumulator, op);
      accumulator = accum_tmp;
      if (cast_to_unsigned_p)
	append_pattern_def_seq (stmt_vinfo, stmt);
    }
  /* Move back to a signed if needed.  */
  if (cast_to_unsigned_p)
    {
      tree accum_tmp = vect_recog_temp_ssa_var (itype, NULL);
      stmt = gimple_build_assign (accum_tmp, CONVERT_EXPR, accumulator);
    }

  return stmt;
}

/* Detect multiplication by constant and convert it into a sequence of
   shifts and additions, subtractions, negations.  We reuse the
   choose_mult_variant algorithms from expmed.c

   Input/Output:

   STMTS: Contains a stmt from which the pattern search begins,
   i.e. the mult stmt.

 Output:

  * TYPE_OUT: The type of the output of this pattern.

  * Return value: A new stmt that will be used to replace
    the multiplication.  */

static gimple *
vect_recog_mult_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  tree oprnd0, oprnd1, vectype, itype;
  gimple *pattern_stmt;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);

  if (!is_gimple_assign (last_stmt))
    return NULL;

  if (gimple_assign_rhs_code (last_stmt) != MULT_EXPR)
    return NULL;

  oprnd0 = gimple_assign_rhs1 (last_stmt);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  itype = TREE_TYPE (oprnd0);

  if (TREE_CODE (oprnd0) != SSA_NAME
      || TREE_CODE (oprnd1) != INTEGER_CST
      || !INTEGRAL_TYPE_P (itype)
      || !type_has_mode_precision_p (itype))
    return NULL;

  vectype = get_vectype_for_scalar_type (itype);
  if (vectype == NULL_TREE)
    return NULL;

  /* If the target can handle vectorized multiplication natively,
     don't attempt to optimize this.  */
  optab mul_optab = optab_for_tree_code (MULT_EXPR, vectype, optab_default);
  if (mul_optab != unknown_optab)
    {
      machine_mode vec_mode = TYPE_MODE (vectype);
      int icode = (int) optab_handler (mul_optab, vec_mode);
      if (icode != CODE_FOR_nothing)
       return NULL;
    }

  pattern_stmt = vect_synth_mult_by_constant (oprnd0, oprnd1, stmt_vinfo);
  if (!pattern_stmt)
    return NULL;

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_mult_pattern", last_stmt);

  stmts->safe_push (last_stmt);
  *type_out = vectype;

  return pattern_stmt;
}

/* Detect a signed division by a constant that wouldn't be
   otherwise vectorized:

   type a_t, b_t;

   S1 a_t = b_t / N;

  where type 'type' is an integral type and N is a constant.

  Similarly handle modulo by a constant:

   S4 a_t = b_t % N;

  Input/Output:

  * STMTS: Contains a stmt from which the pattern search begins,
    i.e. the division stmt.  S1 is replaced by if N is a power
    of two constant and type is signed:
  S3  y_t = b_t < 0 ? N - 1 : 0;
  S2  x_t = b_t + y_t;
  S1' a_t = x_t >> log2 (N);

    S4 is replaced if N is a power of two constant and
    type is signed by (where *_T temporaries have unsigned type):
  S9  y_T = b_t < 0 ? -1U : 0U;
  S8  z_T = y_T >> (sizeof (type_t) * CHAR_BIT - log2 (N));
  S7  z_t = (type) z_T;
  S6  w_t = b_t + z_t;
  S5  x_t = w_t & (N - 1);
  S4' a_t = x_t - z_t;

  Output:

  * TYPE_OUT: The type of the output of this pattern.

  * Return value: A new stmt that will be used to replace the division
    S1 or modulo S4 stmt.  */

static gimple *
vect_recog_divmod_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  tree oprnd0, oprnd1, vectype, itype, cond;
  gimple *pattern_stmt, *def_stmt;
  enum tree_code rhs_code;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  vec_info *vinfo = stmt_vinfo->vinfo;
  optab optab;
  tree q;
  int dummy_int, prec;
  stmt_vec_info def_stmt_vinfo;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  rhs_code = gimple_assign_rhs_code (last_stmt);
  switch (rhs_code)
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
      break;
    default:
      return NULL;
    }

  oprnd0 = gimple_assign_rhs1 (last_stmt);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  itype = TREE_TYPE (oprnd0);
  if (TREE_CODE (oprnd0) != SSA_NAME
      || TREE_CODE (oprnd1) != INTEGER_CST
      || TREE_CODE (itype) != INTEGER_TYPE
      || !type_has_mode_precision_p (itype))
    return NULL;

  scalar_int_mode itype_mode = SCALAR_INT_TYPE_MODE (itype);
  vectype = get_vectype_for_scalar_type (itype);
  if (vectype == NULL_TREE)
    return NULL;

  if (optimize_bb_for_size_p (gimple_bb (last_stmt)))
    {
      /* If the target can handle vectorized division or modulo natively,
	 don't attempt to optimize this, since native division is likely
	 to give smaller code.  */
      optab = optab_for_tree_code (rhs_code, vectype, optab_default);
      if (optab != unknown_optab)
	{
	  machine_mode vec_mode = TYPE_MODE (vectype);
	  int icode = (int) optab_handler (optab, vec_mode);
	  if (icode != CODE_FOR_nothing)
	    return NULL;
	}
    }

  prec = TYPE_PRECISION (itype);
  if (integer_pow2p (oprnd1))
    {
      if (TYPE_UNSIGNED (itype) || tree_int_cst_sgn (oprnd1) != 1)
	return NULL;

      /* Pattern detected.  */
      vect_pattern_detected ("vect_recog_divmod_pattern", last_stmt);

      cond = build2 (LT_EXPR, boolean_type_node, oprnd0,
		     build_int_cst (itype, 0));
      if (rhs_code == TRUNC_DIV_EXPR)
	{
	  tree var = vect_recog_temp_ssa_var (itype, NULL);
	  tree shift;
	  def_stmt
	    = gimple_build_assign (var, COND_EXPR, cond,
				   fold_build2 (MINUS_EXPR, itype, oprnd1,
						build_int_cst (itype, 1)),
				   build_int_cst (itype, 0));
	  new_pattern_def_seq (stmt_vinfo, def_stmt);
	  var = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (var, PLUS_EXPR, oprnd0,
				   gimple_assign_lhs (def_stmt));
	  append_pattern_def_seq (stmt_vinfo, def_stmt);

	  shift = build_int_cst (itype, tree_log2 (oprnd1));
	  pattern_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   RSHIFT_EXPR, var, shift);
	}
      else
	{
	  tree signmask;
	  STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) = NULL;
	  if (compare_tree_int (oprnd1, 2) == 0)
	    {
	      signmask = vect_recog_temp_ssa_var (itype, NULL);
	      def_stmt = gimple_build_assign (signmask, COND_EXPR, cond,
					      build_int_cst (itype, 1),
					      build_int_cst (itype, 0));
	      append_pattern_def_seq (stmt_vinfo, def_stmt);
	    }
	  else
	    {
	      tree utype
		= build_nonstandard_integer_type (prec, 1);
	      tree vecutype = get_vectype_for_scalar_type (utype);
	      tree shift
		= build_int_cst (utype, GET_MODE_BITSIZE (itype_mode)
					- tree_log2 (oprnd1));
	      tree var = vect_recog_temp_ssa_var (utype, NULL);

	      def_stmt = gimple_build_assign (var, COND_EXPR, cond,
					      build_int_cst (utype, -1),
					      build_int_cst (utype, 0));
	      def_stmt_vinfo = new_stmt_vec_info (def_stmt, vinfo);
	      set_vinfo_for_stmt (def_stmt, def_stmt_vinfo);
	      STMT_VINFO_VECTYPE (def_stmt_vinfo) = vecutype;
	      append_pattern_def_seq (stmt_vinfo, def_stmt);
	      var = vect_recog_temp_ssa_var (utype, NULL);
	      def_stmt = gimple_build_assign (var, RSHIFT_EXPR,
					      gimple_assign_lhs (def_stmt),
					      shift);
	      def_stmt_vinfo = new_stmt_vec_info (def_stmt, vinfo);
	      set_vinfo_for_stmt (def_stmt, def_stmt_vinfo);
	      STMT_VINFO_VECTYPE (def_stmt_vinfo) = vecutype;
	      append_pattern_def_seq (stmt_vinfo, def_stmt);
	      signmask = vect_recog_temp_ssa_var (itype, NULL);
	      def_stmt
		= gimple_build_assign (signmask, NOP_EXPR, var);
	      append_pattern_def_seq (stmt_vinfo, def_stmt);
	    }
	  def_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   PLUS_EXPR, oprnd0, signmask);
	  append_pattern_def_seq (stmt_vinfo, def_stmt);
	  def_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   BIT_AND_EXPR, gimple_assign_lhs (def_stmt),
				   fold_build2 (MINUS_EXPR, itype, oprnd1,
						build_int_cst (itype, 1)));
	  append_pattern_def_seq (stmt_vinfo, def_stmt);

	  pattern_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   MINUS_EXPR, gimple_assign_lhs (def_stmt),
				   signmask);
	}

      stmts->safe_push (last_stmt);

      *type_out = vectype;
      return pattern_stmt;
    }

  if (prec > HOST_BITS_PER_WIDE_INT
      || integer_zerop (oprnd1))
    return NULL;

  if (!can_mult_highpart_p (TYPE_MODE (vectype), TYPE_UNSIGNED (itype)))
    return NULL;

  STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo) = NULL;

  if (TYPE_UNSIGNED (itype))
    {
      unsigned HOST_WIDE_INT mh, ml;
      int pre_shift, post_shift;
      unsigned HOST_WIDE_INT d = (TREE_INT_CST_LOW (oprnd1)
				  & GET_MODE_MASK (itype_mode));
      tree t1, t2, t3, t4;

      if (d >= (HOST_WIDE_INT_1U << (prec - 1)))
	/* FIXME: Can transform this into oprnd0 >= oprnd1 ? 1 : 0.  */
	return NULL;

      /* Find a suitable multiplier and right shift count
	 instead of multiplying with D.  */
      mh = choose_multiplier (d, prec, prec, &ml, &post_shift, &dummy_int);

      /* If the suggested multiplier is more than SIZE bits, we can do better
	 for even divisors, using an initial right shift.  */
      if (mh != 0 && (d & 1) == 0)
	{
	  pre_shift = ctz_or_zero (d);
	  mh = choose_multiplier (d >> pre_shift, prec, prec - pre_shift,
				  &ml, &post_shift, &dummy_int);
	  gcc_assert (!mh);
	}
      else
	pre_shift = 0;

      if (mh != 0)
	{
	  if (post_shift - 1 >= prec)
	    return NULL;

	  /* t1 = oprnd0 h* ml;
	     t2 = oprnd0 - t1;
	     t3 = t2 >> 1;
	     t4 = t1 + t3;
	     q = t4 >> (post_shift - 1);  */
	  t1 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t1, MULT_HIGHPART_EXPR, oprnd0,
					  build_int_cst (itype, ml));
	  append_pattern_def_seq (stmt_vinfo, def_stmt);

	  t2 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (t2, MINUS_EXPR, oprnd0, t1);
	  append_pattern_def_seq (stmt_vinfo, def_stmt);

	  t3 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (t3, RSHIFT_EXPR, t2, integer_one_node);
	  append_pattern_def_seq (stmt_vinfo, def_stmt);

	  t4 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (t4, PLUS_EXPR, t1, t3);

	  if (post_shift != 1)
	    {
	      append_pattern_def_seq (stmt_vinfo, def_stmt);

	      q = vect_recog_temp_ssa_var (itype, NULL);
	      pattern_stmt
		= gimple_build_assign (q, RSHIFT_EXPR, t4,
				       build_int_cst (itype, post_shift - 1));
	    }
	  else
	    {
	      q = t4;
	      pattern_stmt = def_stmt;
	    }
	}
      else
	{
	  if (pre_shift >= prec || post_shift >= prec)
	    return NULL;

	  /* t1 = oprnd0 >> pre_shift;
	     t2 = t1 h* ml;
	     q = t2 >> post_shift;  */
	  if (pre_shift)
	    {
	      t1 = vect_recog_temp_ssa_var (itype, NULL);
	      def_stmt
		= gimple_build_assign (t1, RSHIFT_EXPR, oprnd0,
				       build_int_cst (NULL, pre_shift));
	      append_pattern_def_seq (stmt_vinfo, def_stmt);
	    }
	  else
	    t1 = oprnd0;

	  t2 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t2, MULT_HIGHPART_EXPR, t1,
					  build_int_cst (itype, ml));

	  if (post_shift)
	    {
	      append_pattern_def_seq (stmt_vinfo, def_stmt);

	      q = vect_recog_temp_ssa_var (itype, NULL);
	      def_stmt
		= gimple_build_assign (q, RSHIFT_EXPR, t2,
				       build_int_cst (itype, post_shift));
	    }
	  else
	    q = t2;

	  pattern_stmt = def_stmt;
	}
    }
  else
    {
      unsigned HOST_WIDE_INT ml;
      int post_shift;
      HOST_WIDE_INT d = TREE_INT_CST_LOW (oprnd1);
      unsigned HOST_WIDE_INT abs_d;
      bool add = false;
      tree t1, t2, t3, t4;

      /* Give up for -1.  */
      if (d == -1)
	return NULL;

      /* Since d might be INT_MIN, we have to cast to
	 unsigned HOST_WIDE_INT before negating to avoid
	 undefined signed overflow.  */
      abs_d = (d >= 0
	       ? (unsigned HOST_WIDE_INT) d
	       : - (unsigned HOST_WIDE_INT) d);

      /* n rem d = n rem -d */
      if (rhs_code == TRUNC_MOD_EXPR && d < 0)
	{
	  d = abs_d;
	  oprnd1 = build_int_cst (itype, abs_d);
	}
      else if (HOST_BITS_PER_WIDE_INT >= prec
	       && abs_d == HOST_WIDE_INT_1U << (prec - 1))
	/* This case is not handled correctly below.  */
	return NULL;

      choose_multiplier (abs_d, prec, prec - 1, &ml, &post_shift, &dummy_int);
      if (ml >= HOST_WIDE_INT_1U << (prec - 1))
	{
	  add = true;
	  ml |= HOST_WIDE_INT_M1U << (prec - 1);
	}
      if (post_shift >= prec)
	return NULL;

      /* t1 = oprnd0 h* ml;  */
      t1 = vect_recog_temp_ssa_var (itype, NULL);
      def_stmt = gimple_build_assign (t1, MULT_HIGHPART_EXPR, oprnd0,
				      build_int_cst (itype, ml));

      if (add)
	{
	  /* t2 = t1 + oprnd0;  */
	  append_pattern_def_seq (stmt_vinfo, def_stmt);
	  t2 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t2, PLUS_EXPR, t1, oprnd0);
	}
      else
	t2 = t1;

      if (post_shift)
	{
	  /* t3 = t2 >> post_shift;  */
	  append_pattern_def_seq (stmt_vinfo, def_stmt);
	  t3 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t3, RSHIFT_EXPR, t2,
					  build_int_cst (itype, post_shift));
	}
      else
	t3 = t2;

      wide_int oprnd0_min, oprnd0_max;
      int msb = 1;
      if (get_range_info (oprnd0, &oprnd0_min, &oprnd0_max) == VR_RANGE)
	{
	  if (!wi::neg_p (oprnd0_min, TYPE_SIGN (itype)))
	    msb = 0;
	  else if (wi::neg_p (oprnd0_max, TYPE_SIGN (itype)))
	    msb = -1;
	}

      if (msb == 0 && d >= 0)
	{
	  /* q = t3;  */
	  q = t3;
	  pattern_stmt = def_stmt;
	}
      else
	{
	  /* t4 = oprnd0 >> (prec - 1);
	     or if we know from VRP that oprnd0 >= 0
	     t4 = 0;
	     or if we know from VRP that oprnd0 < 0
	     t4 = -1;  */
	  append_pattern_def_seq (stmt_vinfo, def_stmt);
	  t4 = vect_recog_temp_ssa_var (itype, NULL);
	  if (msb != 1)
	    def_stmt = gimple_build_assign (t4, INTEGER_CST,
					    build_int_cst (itype, msb));
	  else
	    def_stmt = gimple_build_assign (t4, RSHIFT_EXPR, oprnd0,
					    build_int_cst (itype, prec - 1));
	  append_pattern_def_seq (stmt_vinfo, def_stmt);

	  /* q = t3 - t4;  or q = t4 - t3;  */
	  q = vect_recog_temp_ssa_var (itype, NULL);
	  pattern_stmt = gimple_build_assign (q, MINUS_EXPR, d < 0 ? t4 : t3,
					      d < 0 ? t3 : t4);
	}
    }

  if (rhs_code == TRUNC_MOD_EXPR)
    {
      tree r, t1;

      /* We divided.  Now finish by:
	 t1 = q * oprnd1;
	 r = oprnd0 - t1;  */
      append_pattern_def_seq (stmt_vinfo, pattern_stmt);

      t1 = vect_recog_temp_ssa_var (itype, NULL);
      def_stmt = gimple_build_assign (t1, MULT_EXPR, q, oprnd1);
      append_pattern_def_seq (stmt_vinfo, def_stmt);

      r = vect_recog_temp_ssa_var (itype, NULL);
      pattern_stmt = gimple_build_assign (r, MINUS_EXPR, oprnd0, t1);
    }

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_divmod_pattern", last_stmt);

  stmts->safe_push (last_stmt);

  *type_out = vectype;
  return pattern_stmt;
}

/* Function vect_recog_mixed_size_cond_pattern

   Try to find the following pattern:

     type x_t, y_t;
     TYPE a_T, b_T, c_T;
   loop:
     S1  a_T = x_t CMP y_t ? b_T : c_T;

   where type 'TYPE' is an integral type which has different size
   from 'type'.  b_T and c_T are either constants (and if 'TYPE' is wider
   than 'type', the constants need to fit into an integer type
   with the same width as 'type') or results of conversion from 'type'.

   Input:

   * LAST_STMT: A stmt from which the pattern search begins.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the pattern.
	Additionally a def_stmt is added.

	a_it = x_t CMP y_t ? b_it : c_it;
	a_T = (TYPE) a_it;  */

static gimple *
vect_recog_mixed_size_cond_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = (*stmts)[0];
  tree cond_expr, then_clause, else_clause;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt), def_stmt_info;
  tree type, vectype, comp_vectype, itype = NULL_TREE, vecitype;
  gimple *pattern_stmt, *def_stmt;
  vec_info *vinfo = stmt_vinfo->vinfo;
  tree orig_type0 = NULL_TREE, orig_type1 = NULL_TREE;
  gimple *def_stmt0 = NULL, *def_stmt1 = NULL;
  bool promotion;
  tree comp_scalar_type;

  if (!is_gimple_assign (last_stmt)
      || gimple_assign_rhs_code (last_stmt) != COND_EXPR
      || STMT_VINFO_DEF_TYPE (stmt_vinfo) != vect_internal_def)
    return NULL;

  cond_expr = gimple_assign_rhs1 (last_stmt);
  then_clause = gimple_assign_rhs2 (last_stmt);
  else_clause = gimple_assign_rhs3 (last_stmt);

  if (!COMPARISON_CLASS_P (cond_expr))
    return NULL;

  comp_scalar_type = TREE_TYPE (TREE_OPERAND (cond_expr, 0));
  comp_vectype = get_vectype_for_scalar_type (comp_scalar_type);
  if (comp_vectype == NULL_TREE)
    return NULL;

  type = gimple_expr_type (last_stmt);
  if (types_compatible_p (type, comp_scalar_type)
      || ((TREE_CODE (then_clause) != INTEGER_CST
	   || TREE_CODE (else_clause) != INTEGER_CST)
	  && !INTEGRAL_TYPE_P (comp_scalar_type))
      || !INTEGRAL_TYPE_P (type))
    return NULL;

  if ((TREE_CODE (then_clause) != INTEGER_CST
       && !type_conversion_p (then_clause, last_stmt, false, &orig_type0,
                              &def_stmt0, &promotion))
      || (TREE_CODE (else_clause) != INTEGER_CST
          && !type_conversion_p (else_clause, last_stmt, false, &orig_type1,
                                 &def_stmt1, &promotion)))
    return NULL;

  if (orig_type0 && orig_type1
      && !types_compatible_p (orig_type0, orig_type1))
    return NULL;

  if (orig_type0)
    {
      if (!types_compatible_p (orig_type0, comp_scalar_type))
	return NULL;
      then_clause = gimple_assign_rhs1 (def_stmt0);
      itype = orig_type0;
    }

  if (orig_type1)
    {
      if (!types_compatible_p (orig_type1, comp_scalar_type))
	return NULL;
      else_clause = gimple_assign_rhs1 (def_stmt1);
      itype = orig_type1;
    }


  HOST_WIDE_INT cmp_mode_size
    = GET_MODE_UNIT_BITSIZE (TYPE_MODE (comp_vectype));

  scalar_int_mode type_mode = SCALAR_INT_TYPE_MODE (type);
  if (GET_MODE_BITSIZE (type_mode) == cmp_mode_size)
    return NULL;

  vectype = get_vectype_for_scalar_type (type);
  if (vectype == NULL_TREE)
    return NULL;

  if (expand_vec_cond_expr_p (vectype, comp_vectype, TREE_CODE (cond_expr)))
    return NULL;

  if (itype == NULL_TREE)
    itype = build_nonstandard_integer_type (cmp_mode_size,
  					    TYPE_UNSIGNED (type));

  if (itype == NULL_TREE
      || GET_MODE_BITSIZE (SCALAR_TYPE_MODE (itype)) != cmp_mode_size)
    return NULL;

  vecitype = get_vectype_for_scalar_type (itype);
  if (vecitype == NULL_TREE)
    return NULL;

  if (!expand_vec_cond_expr_p (vecitype, comp_vectype, TREE_CODE (cond_expr)))
    return NULL;

  if (GET_MODE_BITSIZE (type_mode) > cmp_mode_size)
    {
      if ((TREE_CODE (then_clause) == INTEGER_CST
	   && !int_fits_type_p (then_clause, itype))
	  || (TREE_CODE (else_clause) == INTEGER_CST
	      && !int_fits_type_p (else_clause, itype)))
	return NULL;
    }

  def_stmt = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				  COND_EXPR, unshare_expr (cond_expr),
				  fold_convert (itype, then_clause),
				  fold_convert (itype, else_clause));
  pattern_stmt = gimple_build_assign (vect_recog_temp_ssa_var (type, NULL),
				      NOP_EXPR, gimple_assign_lhs (def_stmt));

  new_pattern_def_seq (stmt_vinfo, def_stmt);
  def_stmt_info = new_stmt_vec_info (def_stmt, vinfo);
  set_vinfo_for_stmt (def_stmt, def_stmt_info);
  STMT_VINFO_VECTYPE (def_stmt_info) = vecitype;
  *type_out = vectype;

  vect_pattern_detected ("vect_recog_mixed_size_cond_pattern", last_stmt);

  return pattern_stmt;
}


/* Helper function of vect_recog_bool_pattern.  Called recursively, return
   true if bool VAR can and should be optimized that way.  Assume it shouldn't
   in case it's a result of a comparison which can be directly vectorized into
   a vector comparison.  Fills in STMTS with all stmts visited during the
   walk.  */

static bool
check_bool_pattern (tree var, vec_info *vinfo, hash_set<gimple *> &stmts)
{
  tree rhs1;
  enum tree_code rhs_code;

  stmt_vec_info def_stmt_info = vect_get_internal_def (vinfo, var);
  if (!def_stmt_info)
    return false;

  gassign *def_stmt = dyn_cast <gassign *> (def_stmt_info->stmt);
  if (!def_stmt)
    return false;

  if (stmts.contains (def_stmt))
    return true;

  rhs1 = gimple_assign_rhs1 (def_stmt);
  rhs_code = gimple_assign_rhs_code (def_stmt);
  switch (rhs_code)
    {
    case SSA_NAME:
      if (! check_bool_pattern (rhs1, vinfo, stmts))
	return false;
      break;

    CASE_CONVERT:
      if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (rhs1)))
	return false;
      if (! check_bool_pattern (rhs1, vinfo, stmts))
	return false;
      break;

    case BIT_NOT_EXPR:
      if (! check_bool_pattern (rhs1, vinfo, stmts))
	return false;
      break;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (! check_bool_pattern (rhs1, vinfo, stmts)
	  || ! check_bool_pattern (gimple_assign_rhs2 (def_stmt), vinfo, stmts))
	return false;
      break;

    default:
      if (TREE_CODE_CLASS (rhs_code) == tcc_comparison)
	{
	  tree vecitype, comp_vectype;

	  /* If the comparison can throw, then is_gimple_condexpr will be
	     false and we can't make a COND_EXPR/VEC_COND_EXPR out of it.  */
	  if (stmt_could_throw_p (def_stmt))
	    return false;

	  comp_vectype = get_vectype_for_scalar_type (TREE_TYPE (rhs1));
	  if (comp_vectype == NULL_TREE)
	    return false;

	  tree mask_type = get_mask_type_for_scalar_type (TREE_TYPE (rhs1));
	  if (mask_type
	      && expand_vec_cmp_expr_p (comp_vectype, mask_type, rhs_code))
	    return false;

	  if (TREE_CODE (TREE_TYPE (rhs1)) != INTEGER_TYPE)
	    {
	      scalar_mode mode = SCALAR_TYPE_MODE (TREE_TYPE (rhs1));
	      tree itype
		= build_nonstandard_integer_type (GET_MODE_BITSIZE (mode), 1);
	      vecitype = get_vectype_for_scalar_type (itype);
	      if (vecitype == NULL_TREE)
		return false;
	    }
	  else
	    vecitype = comp_vectype;
	  if (! expand_vec_cond_expr_p (vecitype, comp_vectype, rhs_code))
	    return false;
	}
      else
	return false;
      break;
    }

  bool res = stmts.add (def_stmt);
  /* We can't end up recursing when just visiting SSA defs but not PHIs.  */
  gcc_assert (!res);

  return true;
}


/* Helper function of adjust_bool_pattern.  Add a cast to TYPE to a previous
   stmt (SSA_NAME_DEF_STMT of VAR) adding a cast to STMT_INFOs
   pattern sequence.  */

static tree
adjust_bool_pattern_cast (tree type, tree var, stmt_vec_info stmt_info)
{
  gimple *cast_stmt = gimple_build_assign (vect_recog_temp_ssa_var (type, NULL),
					   NOP_EXPR, var);
  stmt_vec_info patt_vinfo = new_stmt_vec_info (cast_stmt, stmt_info->vinfo);
  set_vinfo_for_stmt (cast_stmt, patt_vinfo);
  STMT_VINFO_VECTYPE (patt_vinfo) = get_vectype_for_scalar_type (type);
  append_pattern_def_seq (stmt_info, cast_stmt);
  return gimple_assign_lhs (cast_stmt);
}

/* Helper function of vect_recog_bool_pattern.  Do the actual transformations.
   VAR is an SSA_NAME that should be transformed from bool to a wider integer
   type, OUT_TYPE is the desired final integer type of the whole pattern.
   STMT_INFO is the info of the pattern root and is where pattern stmts should
   be associated with.  DEFS is a map of pattern defs.  */

static void
adjust_bool_pattern (tree var, tree out_type,
		     stmt_vec_info stmt_info, hash_map <tree, tree> &defs)
{
  gimple *stmt = SSA_NAME_DEF_STMT (var);
  enum tree_code rhs_code, def_rhs_code;
  tree itype, cond_expr, rhs1, rhs2, irhs1, irhs2;
  location_t loc;
  gimple *pattern_stmt, *def_stmt;
  tree trueval = NULL_TREE;

  rhs1 = gimple_assign_rhs1 (stmt);
  rhs2 = gimple_assign_rhs2 (stmt);
  rhs_code = gimple_assign_rhs_code (stmt);
  loc = gimple_location (stmt);
  switch (rhs_code)
    {
    case SSA_NAME:
    CASE_CONVERT:
      irhs1 = *defs.get (rhs1);
      itype = TREE_TYPE (irhs1);
      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
			       SSA_NAME, irhs1);
      break;

    case BIT_NOT_EXPR:
      irhs1 = *defs.get (rhs1);
      itype = TREE_TYPE (irhs1);
      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
			       BIT_XOR_EXPR, irhs1, build_int_cst (itype, 1));
      break;

    case BIT_AND_EXPR:
      /* Try to optimize x = y & (a < b ? 1 : 0); into
	 x = (a < b ? y : 0);

	 E.g. for:
	   bool a_b, b_b, c_b;
	   TYPE d_T;

	   S1  a_b = x1 CMP1 y1;
	   S2  b_b = x2 CMP2 y2;
	   S3  c_b = a_b & b_b;
	   S4  d_T = (TYPE) c_b;

	 we would normally emit:

	   S1'  a_T = x1 CMP1 y1 ? 1 : 0;
	   S2'  b_T = x2 CMP2 y2 ? 1 : 0;
	   S3'  c_T = a_T & b_T;
	   S4'  d_T = c_T;

	 but we can save one stmt by using the
	 result of one of the COND_EXPRs in the other COND_EXPR and leave
	 BIT_AND_EXPR stmt out:

	   S1'  a_T = x1 CMP1 y1 ? 1 : 0;
	   S3'  c_T = x2 CMP2 y2 ? a_T : 0;
	   S4'  f_T = c_T;

	 At least when VEC_COND_EXPR is implemented using masks
	 cond ? 1 : 0 is as expensive as cond ? var : 0, in both cases it
	 computes the comparison masks and ands it, in one case with
	 all ones vector, in the other case with a vector register.
	 Don't do this for BIT_IOR_EXPR, because cond ? 1 : var; is
	 often more expensive.  */
      def_stmt = SSA_NAME_DEF_STMT (rhs2);
      def_rhs_code = gimple_assign_rhs_code (def_stmt);
      if (TREE_CODE_CLASS (def_rhs_code) == tcc_comparison)
	{
	  irhs1 = *defs.get (rhs1);
	  tree def_rhs1 = gimple_assign_rhs1 (def_stmt);
	  if (TYPE_PRECISION (TREE_TYPE (irhs1))
	      == GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (def_rhs1))))
	    {
	      rhs_code = def_rhs_code;
	      rhs1 = def_rhs1;
	      rhs2 = gimple_assign_rhs2 (def_stmt);
	      trueval = irhs1;
	      goto do_compare;
	    }
	  else
	    irhs2 = *defs.get (rhs2);
	  goto and_ior_xor;
	}
      def_stmt = SSA_NAME_DEF_STMT (rhs1);
      def_rhs_code = gimple_assign_rhs_code (def_stmt);
      if (TREE_CODE_CLASS (def_rhs_code) == tcc_comparison)
	{
	  irhs2 = *defs.get (rhs2);
	  tree def_rhs1 = gimple_assign_rhs1 (def_stmt);
	  if (TYPE_PRECISION (TREE_TYPE (irhs2))
	      == GET_MODE_BITSIZE (SCALAR_TYPE_MODE (TREE_TYPE (def_rhs1))))
	    {
	      rhs_code = def_rhs_code;
	      rhs1 = def_rhs1;
	      rhs2 = gimple_assign_rhs2 (def_stmt);
	      trueval = irhs2;
	      goto do_compare;
	    }
	  else
	    irhs1 = *defs.get (rhs1);
	  goto and_ior_xor;
	}
      /* FALLTHRU */
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      irhs1 = *defs.get (rhs1);
      irhs2 = *defs.get (rhs2);
    and_ior_xor:
      if (TYPE_PRECISION (TREE_TYPE (irhs1))
	  != TYPE_PRECISION (TREE_TYPE (irhs2)))
	{
	  int prec1 = TYPE_PRECISION (TREE_TYPE (irhs1));
	  int prec2 = TYPE_PRECISION (TREE_TYPE (irhs2));
	  int out_prec = TYPE_PRECISION (out_type);
	  if (absu_hwi (out_prec - prec1) < absu_hwi (out_prec - prec2))
	    irhs2 = adjust_bool_pattern_cast (TREE_TYPE (irhs1), irhs2,
					      stmt_info);
	  else if (absu_hwi (out_prec - prec1) > absu_hwi (out_prec - prec2))
	    irhs1 = adjust_bool_pattern_cast (TREE_TYPE (irhs2), irhs1,
					      stmt_info);
	  else
	    {
	      irhs1 = adjust_bool_pattern_cast (out_type, irhs1, stmt_info);
	      irhs2 = adjust_bool_pattern_cast (out_type, irhs2, stmt_info);
	    }
	}
      itype = TREE_TYPE (irhs1);
      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
			       rhs_code, irhs1, irhs2);
      break;

    default:
    do_compare:
      gcc_assert (TREE_CODE_CLASS (rhs_code) == tcc_comparison);
      if (TREE_CODE (TREE_TYPE (rhs1)) != INTEGER_TYPE
	  || !TYPE_UNSIGNED (TREE_TYPE (rhs1))
	  || maybe_ne (TYPE_PRECISION (TREE_TYPE (rhs1)),
		       GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (rhs1)))))
	{
	  scalar_mode mode = SCALAR_TYPE_MODE (TREE_TYPE (rhs1));
	  itype
	    = build_nonstandard_integer_type (GET_MODE_BITSIZE (mode), 1);
	}
      else
	itype = TREE_TYPE (rhs1);
      cond_expr = build2_loc (loc, rhs_code, itype, rhs1, rhs2);
      if (trueval == NULL_TREE)
	trueval = build_int_cst (itype, 1);
      else
	gcc_checking_assert (useless_type_conversion_p (itype,
							TREE_TYPE (trueval)));
      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
			       COND_EXPR, cond_expr, trueval,
			       build_int_cst (itype, 0));
      break;
    }

  gimple_set_location (pattern_stmt, loc);
  /* ???  Why does vect_mark_pattern_stmts set the vector type on all
     pattern def seq stmts instead of just letting auto-detection do
     its work?  */
  stmt_vec_info patt_vinfo = new_stmt_vec_info (pattern_stmt, stmt_info->vinfo);
  set_vinfo_for_stmt (pattern_stmt, patt_vinfo);
  STMT_VINFO_VECTYPE (patt_vinfo) = get_vectype_for_scalar_type (itype);
  append_pattern_def_seq (stmt_info, pattern_stmt);
  defs.put (var, gimple_assign_lhs (pattern_stmt));
}

/* Comparison function to qsort a vector of gimple stmts after UID.  */

static int
sort_after_uid (const void *p1, const void *p2)
{
  const gimple *stmt1 = *(const gimple * const *)p1;
  const gimple *stmt2 = *(const gimple * const *)p2;
  return gimple_uid (stmt1) - gimple_uid (stmt2);
}

/* Create pattern stmts for all stmts participating in the bool pattern
   specified by BOOL_STMT_SET and its root STMT with the desired type
   OUT_TYPE.  Return the def of the pattern root.  */

static tree
adjust_bool_stmts (hash_set <gimple *> &bool_stmt_set,
		   tree out_type, gimple *stmt)
{
  /* Gather original stmts in the bool pattern in their order of appearance
     in the IL.  */
  auto_vec<gimple *> bool_stmts (bool_stmt_set.elements ());
  for (hash_set <gimple *>::iterator i = bool_stmt_set.begin ();
       i != bool_stmt_set.end (); ++i)
    bool_stmts.quick_push (*i);
  bool_stmts.qsort (sort_after_uid);

  /* Now process them in that order, producing pattern stmts.  */
  hash_map <tree, tree> defs;
  for (unsigned i = 0; i < bool_stmts.length (); ++i)
    adjust_bool_pattern (gimple_assign_lhs (bool_stmts[i]),
			 out_type, vinfo_for_stmt (stmt), defs);

  /* Pop the last pattern seq stmt and install it as pattern root for STMT.  */
  gimple *pattern_stmt
    = gimple_seq_last_stmt (STMT_VINFO_PATTERN_DEF_SEQ (vinfo_for_stmt (stmt)));
  return gimple_assign_lhs (pattern_stmt);
}

/* Helper for search_type_for_mask.  */

static tree
search_type_for_mask_1 (tree var, vec_info *vinfo,
			hash_map<gimple *, tree> &cache)
{
  tree rhs1;
  enum tree_code rhs_code;
  tree res = NULL_TREE, res2;

  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (var)))
    return NULL_TREE;

  stmt_vec_info def_stmt_info = vect_get_internal_def (vinfo, var);
  if (!def_stmt_info)
    return NULL_TREE;

  gassign *def_stmt = dyn_cast <gassign *> (def_stmt_info->stmt);
  if (!def_stmt)
    return NULL_TREE;

  tree *c = cache.get (def_stmt);
  if (c)
    return *c;

  rhs_code = gimple_assign_rhs_code (def_stmt);
  rhs1 = gimple_assign_rhs1 (def_stmt);

  switch (rhs_code)
    {
    case SSA_NAME:
    case BIT_NOT_EXPR:
    CASE_CONVERT:
      res = search_type_for_mask_1 (rhs1, vinfo, cache);
      break;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      res = search_type_for_mask_1 (rhs1, vinfo, cache);
      res2 = search_type_for_mask_1 (gimple_assign_rhs2 (def_stmt), vinfo,
				     cache);
      if (!res || (res2 && TYPE_PRECISION (res) > TYPE_PRECISION (res2)))
	res = res2;
      break;

    default:
      if (TREE_CODE_CLASS (rhs_code) == tcc_comparison)
	{
	  tree comp_vectype, mask_type;

	  if (VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (rhs1)))
	    {
	      res = search_type_for_mask_1 (rhs1, vinfo, cache);
	      res2 = search_type_for_mask_1 (gimple_assign_rhs2 (def_stmt),
					     vinfo, cache);
	      if (!res || (res2 && TYPE_PRECISION (res) > TYPE_PRECISION (res2)))
		res = res2;
	      break;
	    }

	  comp_vectype = get_vectype_for_scalar_type (TREE_TYPE (rhs1));
	  if (comp_vectype == NULL_TREE)
	    {
	      res = NULL_TREE;
	      break;
	    }

	  mask_type = get_mask_type_for_scalar_type (TREE_TYPE (rhs1));
	  if (!mask_type
	      || !expand_vec_cmp_expr_p (comp_vectype, mask_type, rhs_code))
	    {
	      res = NULL_TREE;
	      break;
	    }

	  if (TREE_CODE (TREE_TYPE (rhs1)) != INTEGER_TYPE
	      || !TYPE_UNSIGNED (TREE_TYPE (rhs1)))
	    {
	      scalar_mode mode = SCALAR_TYPE_MODE (TREE_TYPE (rhs1));
	      res = build_nonstandard_integer_type (GET_MODE_BITSIZE (mode), 1);
	    }
	  else
	    res = TREE_TYPE (rhs1);
	}
    }

  cache.put (def_stmt, res);
  return res;
}

/* Return the proper type for converting bool VAR into
   an integer value or NULL_TREE if no such type exists.
   The type is chosen so that converted value has the
   same number of elements as VAR's vector type.  */

static tree
search_type_for_mask (tree var, vec_info *vinfo)
{
  hash_map<gimple *, tree> cache;
  return search_type_for_mask_1 (var, vinfo, cache);
}

/* Function vect_recog_bool_pattern

   Try to find pattern like following:

     bool a_b, b_b, c_b, d_b, e_b;
     TYPE f_T;
   loop:
     S1  a_b = x1 CMP1 y1;
     S2  b_b = x2 CMP2 y2;
     S3  c_b = a_b & b_b;
     S4  d_b = x3 CMP3 y3;
     S5  e_b = c_b | d_b;
     S6  f_T = (TYPE) e_b;

   where type 'TYPE' is an integral type.  Or a similar pattern
   ending in

     S6  f_Y = e_b ? r_Y : s_Y;

   as results from if-conversion of a complex condition.

   Input:

   * LAST_STMT: A stmt at the end from which the pattern
		search begins, i.e. cast of a bool to
		an integer type.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the pattern.

	Assuming size of TYPE is the same as size of all comparisons
	(otherwise some casts would be added where needed), the above
	sequence we create related pattern stmts:
	S1'  a_T = x1 CMP1 y1 ? 1 : 0;
	S3'  c_T = x2 CMP2 y2 ? a_T : 0;
	S4'  d_T = x3 CMP3 y3 ? 1 : 0;
	S5'  e_T = c_T | d_T;
	S6'  f_T = e_T;

	Instead of the above S3' we could emit:
	S2'  b_T = x2 CMP2 y2 ? 1 : 0;
	S3'  c_T = a_T | b_T;
	but the above is more efficient.  */

static gimple *
vect_recog_bool_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  enum tree_code rhs_code;
  tree var, lhs, rhs, vectype;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  stmt_vec_info new_stmt_info;
  vec_info *vinfo = stmt_vinfo->vinfo;
  gimple *pattern_stmt;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  var = gimple_assign_rhs1 (last_stmt);
  lhs = gimple_assign_lhs (last_stmt);

  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (var)))
    return NULL;

  hash_set<gimple *> bool_stmts;

  rhs_code = gimple_assign_rhs_code (last_stmt);
  if (CONVERT_EXPR_CODE_P (rhs_code))
    {
      if (! INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  || TYPE_PRECISION (TREE_TYPE (lhs)) == 1)
	return NULL;
      vectype = get_vectype_for_scalar_type (TREE_TYPE (lhs));
      if (vectype == NULL_TREE)
	return NULL;

      if (check_bool_pattern (var, vinfo, bool_stmts))
	{
	  rhs = adjust_bool_stmts (bool_stmts, TREE_TYPE (lhs), last_stmt);
	  lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	  if (useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	    pattern_stmt = gimple_build_assign (lhs, SSA_NAME, rhs);
	  else
	    pattern_stmt
	      = gimple_build_assign (lhs, NOP_EXPR, rhs);
	}
      else
	{
	  tree type = search_type_for_mask (var, vinfo);
	  tree cst0, cst1, tmp;

	  if (!type)
	    return NULL;

	  /* We may directly use cond with narrowed type to avoid
	     multiple cond exprs with following result packing and
	     perform single cond with packed mask instead.  In case
	     of widening we better make cond first and then extract
	     results.  */
	  if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (lhs)))
	    type = TREE_TYPE (lhs);

	  cst0 = build_int_cst (type, 0);
	  cst1 = build_int_cst (type, 1);
	  tmp = vect_recog_temp_ssa_var (type, NULL);
	  pattern_stmt = gimple_build_assign (tmp, COND_EXPR, var, cst1, cst0);

	  if (!useless_type_conversion_p (type, TREE_TYPE (lhs)))
	    {
	      tree new_vectype = get_vectype_for_scalar_type (type);
	      new_stmt_info = new_stmt_vec_info (pattern_stmt, vinfo);
	      set_vinfo_for_stmt (pattern_stmt, new_stmt_info);
	      STMT_VINFO_VECTYPE (new_stmt_info) = new_vectype;
	      new_pattern_def_seq (stmt_vinfo, pattern_stmt);

	      lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	      pattern_stmt = gimple_build_assign (lhs, CONVERT_EXPR, tmp);
	    }
	}

      *type_out = vectype;
      stmts->safe_push (last_stmt);
      vect_pattern_detected ("vect_recog_bool_pattern", last_stmt);

      return pattern_stmt;
    }
  else if (rhs_code == COND_EXPR
	   && TREE_CODE (var) == SSA_NAME)
    {
      vectype = get_vectype_for_scalar_type (TREE_TYPE (lhs));
      if (vectype == NULL_TREE)
	return NULL;

      /* Build a scalar type for the boolean result that when
         vectorized matches the vector type of the result in
	 size and number of elements.  */
      unsigned prec
	= vector_element_size (tree_to_poly_uint64 (TYPE_SIZE (vectype)),
			       TYPE_VECTOR_SUBPARTS (vectype));

      tree type
	= build_nonstandard_integer_type (prec,
					  TYPE_UNSIGNED (TREE_TYPE (var)));
      if (get_vectype_for_scalar_type (type) == NULL_TREE)
	return NULL;

      if (!check_bool_pattern (var, vinfo, bool_stmts))
	return NULL;

      rhs = adjust_bool_stmts (bool_stmts, type, last_stmt);

      lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
      pattern_stmt 
	  = gimple_build_assign (lhs, COND_EXPR,
				 build2 (NE_EXPR, boolean_type_node,
					 rhs, build_int_cst (type, 0)),
				 gimple_assign_rhs2 (last_stmt),
				 gimple_assign_rhs3 (last_stmt));
      *type_out = vectype;
      stmts->safe_push (last_stmt);
      vect_pattern_detected ("vect_recog_bool_pattern", last_stmt);

      return pattern_stmt;
    }
  else if (rhs_code == SSA_NAME
	   && STMT_VINFO_DATA_REF (stmt_vinfo))
    {
      stmt_vec_info pattern_stmt_info;
      vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
      gcc_assert (vectype != NULL_TREE);
      if (!VECTOR_MODE_P (TYPE_MODE (vectype)))
	return NULL;

      if (check_bool_pattern (var, vinfo, bool_stmts))
	rhs = adjust_bool_stmts (bool_stmts, TREE_TYPE (vectype), last_stmt);
      else
	{
	  tree type = search_type_for_mask (var, vinfo);
	  tree cst0, cst1, new_vectype;

	  if (!type)
	    return NULL;

	  if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (vectype)))
	    type = TREE_TYPE (vectype);

	  cst0 = build_int_cst (type, 0);
	  cst1 = build_int_cst (type, 1);
	  new_vectype = get_vectype_for_scalar_type (type);

	  rhs = vect_recog_temp_ssa_var (type, NULL);
	  pattern_stmt = gimple_build_assign (rhs, COND_EXPR, var, cst1, cst0);

	  pattern_stmt_info = new_stmt_vec_info (pattern_stmt, vinfo);
	  set_vinfo_for_stmt (pattern_stmt, pattern_stmt_info);
	  STMT_VINFO_VECTYPE (pattern_stmt_info) = new_vectype;
	  append_pattern_def_seq (stmt_vinfo, pattern_stmt);
	}

      lhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (vectype), lhs);
      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	{
	  tree rhs2 = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	  gimple *cast_stmt = gimple_build_assign (rhs2, NOP_EXPR, rhs);
	  append_pattern_def_seq (stmt_vinfo, cast_stmt);
	  rhs = rhs2;
	}
      pattern_stmt = gimple_build_assign (lhs, SSA_NAME, rhs);
      pattern_stmt_info = new_stmt_vec_info (pattern_stmt, vinfo);
      set_vinfo_for_stmt (pattern_stmt, pattern_stmt_info);
      STMT_VINFO_DATA_REF (pattern_stmt_info)
	= STMT_VINFO_DATA_REF (stmt_vinfo);
      STMT_VINFO_DR_WRT_VEC_LOOP (pattern_stmt_info)
	= STMT_VINFO_DR_WRT_VEC_LOOP (stmt_vinfo);
      *type_out = vectype;
      stmts->safe_push (last_stmt);
      vect_pattern_detected ("vect_recog_bool_pattern", last_stmt);

      return pattern_stmt;
    }
  else
    return NULL;
}


/* A helper for vect_recog_mask_conversion_pattern.  Build
   conversion of MASK to a type suitable for masking VECTYPE.
   Built statement gets required vectype and is appended to
   a pattern sequence of STMT_VINFO.

   Return converted mask.  */

static tree
build_mask_conversion (tree mask, tree vectype, stmt_vec_info stmt_vinfo,
		       vec_info *vinfo)
{
  gimple *stmt;
  tree masktype, tmp;
  stmt_vec_info new_stmt_info;

  masktype = build_same_sized_truth_vector_type (vectype);
  tmp = vect_recog_temp_ssa_var (TREE_TYPE (masktype), NULL);
  stmt = gimple_build_assign (tmp, CONVERT_EXPR, mask);
  new_stmt_info = new_stmt_vec_info (stmt, vinfo);
  set_vinfo_for_stmt (stmt, new_stmt_info);
  STMT_VINFO_VECTYPE (new_stmt_info) = masktype;
  append_pattern_def_seq (stmt_vinfo, stmt);

  return tmp;
}


/* Function vect_recog_mask_conversion_pattern

   Try to find statements which require boolean type
   converison.  Additional conversion statements are
   added to handle such cases.  For example:

   bool m_1, m_2, m_3;
   int i_4, i_5;
   double d_6, d_7;
   char c_1, c_2, c_3;

   S1   m_1 = i_4 > i_5;
   S2   m_2 = d_6 < d_7;
   S3   m_3 = m_1 & m_2;
   S4   c_1 = m_3 ? c_2 : c_3;

   Will be transformed into:

   S1   m_1 = i_4 > i_5;
   S2   m_2 = d_6 < d_7;
   S3'' m_2' = (_Bool[bitsize=32])m_2
   S3'  m_3' = m_1 & m_2';
   S4'' m_3'' = (_Bool[bitsize=8])m_3'
   S4'  c_1' = m_3'' ? c_2 : c_3;  */

static gimple *
vect_recog_mask_conversion_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  enum tree_code rhs_code;
  tree lhs = NULL_TREE, rhs1, rhs2, tmp, rhs1_type, rhs2_type;
  tree vectype1, vectype2;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (last_stmt);
  stmt_vec_info pattern_stmt_info;
  vec_info *vinfo = stmt_vinfo->vinfo;

  /* Check for MASK_LOAD ans MASK_STORE calls requiring mask conversion.  */
  if (is_gimple_call (last_stmt)
      && gimple_call_internal_p (last_stmt)
      && (gimple_call_internal_fn (last_stmt) == IFN_MASK_STORE
	  || gimple_call_internal_fn (last_stmt) == IFN_MASK_LOAD))
    {
      gcall *pattern_stmt;
      bool load = (gimple_call_internal_fn (last_stmt) == IFN_MASK_LOAD);

      if (load)
	{
	  lhs = gimple_call_lhs (last_stmt);
	  vectype1 = get_vectype_for_scalar_type (TREE_TYPE (lhs));
	}
      else
	{
	  rhs2 = gimple_call_arg (last_stmt, 3);
	  vectype1 = get_vectype_for_scalar_type (TREE_TYPE (rhs2));
	}

      rhs1 = gimple_call_arg (last_stmt, 2);
      rhs1_type = search_type_for_mask (rhs1, vinfo);
      if (!rhs1_type)
	return NULL;
      vectype2 = get_mask_type_for_scalar_type (rhs1_type);

      if (!vectype1 || !vectype2
	  || known_eq (TYPE_VECTOR_SUBPARTS (vectype1),
		       TYPE_VECTOR_SUBPARTS (vectype2)))
	return NULL;

      tmp = build_mask_conversion (rhs1, vectype1, stmt_vinfo, vinfo);

      if (load)
	{
	  lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	  pattern_stmt
	    = gimple_build_call_internal (IFN_MASK_LOAD, 3,
					  gimple_call_arg (last_stmt, 0),
					  gimple_call_arg (last_stmt, 1),
					  tmp);
	  gimple_call_set_lhs (pattern_stmt, lhs);
	}
      else
	  pattern_stmt
	    = gimple_build_call_internal (IFN_MASK_STORE, 4,
					  gimple_call_arg (last_stmt, 0),
					  gimple_call_arg (last_stmt, 1),
					  tmp,
					  gimple_call_arg (last_stmt, 3));

      gimple_call_set_nothrow (pattern_stmt, true);

      pattern_stmt_info = new_stmt_vec_info (pattern_stmt, vinfo);
      set_vinfo_for_stmt (pattern_stmt, pattern_stmt_info);
      STMT_VINFO_DATA_REF (pattern_stmt_info)
	= STMT_VINFO_DATA_REF (stmt_vinfo);
      STMT_VINFO_DR_WRT_VEC_LOOP (pattern_stmt_info)
	= STMT_VINFO_DR_WRT_VEC_LOOP (stmt_vinfo);
      STMT_VINFO_GATHER_SCATTER_P (pattern_stmt_info)
	= STMT_VINFO_GATHER_SCATTER_P (stmt_vinfo);

      *type_out = vectype1;
      stmts->safe_push (last_stmt);
      vect_pattern_detected ("vect_recog_mask_conversion_pattern", last_stmt);

      return pattern_stmt;
    }

  if (!is_gimple_assign (last_stmt))
    return NULL;

  gimple *pattern_stmt;
  lhs = gimple_assign_lhs (last_stmt);
  rhs1 = gimple_assign_rhs1 (last_stmt);
  rhs_code = gimple_assign_rhs_code (last_stmt);

  /* Check for cond expression requiring mask conversion.  */
  if (rhs_code == COND_EXPR)
    {
      vectype1 = get_vectype_for_scalar_type (TREE_TYPE (lhs));

      if (TREE_CODE (rhs1) == SSA_NAME)
	{
	  rhs1_type = search_type_for_mask (rhs1, vinfo);
	  if (!rhs1_type)
	    return NULL;
	}
      else if (COMPARISON_CLASS_P (rhs1))
	{
	  /* Check whether we're comparing scalar booleans and (if so)
	     whether a better mask type exists than the mask associated
	     with boolean-sized elements.  This avoids unnecessary packs
	     and unpacks if the booleans are set from comparisons of
	     wider types.  E.g. in:

	       int x1, x2, x3, x4, y1, y1;
	       ...
	       bool b1 = (x1 == x2);
	       bool b2 = (x3 == x4);
	       ... = b1 == b2 ? y1 : y2;

	     it is better for b1 and b2 to use the mask type associated
	     with int elements rather bool (byte) elements.  */
	  rhs1_type = search_type_for_mask (TREE_OPERAND (rhs1, 0), vinfo);
	  if (!rhs1_type)
	    rhs1_type = TREE_TYPE (TREE_OPERAND (rhs1, 0));
	}
      else
	return NULL;

      vectype2 = get_mask_type_for_scalar_type (rhs1_type);

      if (!vectype1 || !vectype2)
	return NULL;

      /* Continue if a conversion is needed.  Also continue if we have
	 a comparison whose vector type would normally be different from
	 VECTYPE2 when considered in isolation.  In that case we'll
	 replace the comparison with an SSA name (so that we can record
	 its vector type) and behave as though the comparison was an SSA
	 name from the outset.  */
      if (known_eq (TYPE_VECTOR_SUBPARTS (vectype1),
		    TYPE_VECTOR_SUBPARTS (vectype2))
	  && (TREE_CODE (rhs1) == SSA_NAME
	      || rhs1_type == TREE_TYPE (TREE_OPERAND (rhs1, 0))))
	return NULL;

      /* If rhs1 is invariant and we can promote it leave the COND_EXPR
         in place, we can handle it in vectorizable_condition.  This avoids
	 unnecessary promotion stmts and increased vectorization factor.  */
      if (COMPARISON_CLASS_P (rhs1)
	  && INTEGRAL_TYPE_P (rhs1_type)
	  && known_le (TYPE_VECTOR_SUBPARTS (vectype1),
		       TYPE_VECTOR_SUBPARTS (vectype2)))
	{
	  enum vect_def_type dt;
	  if (vect_is_simple_use (TREE_OPERAND (rhs1, 0), vinfo, &dt)
	      && dt == vect_external_def
	      && vect_is_simple_use (TREE_OPERAND (rhs1, 1), vinfo, &dt)
	      && (dt == vect_external_def
		  || dt == vect_constant_def))
	    {
	      tree wide_scalar_type = build_nonstandard_integer_type
		(tree_to_uhwi (TYPE_SIZE (TREE_TYPE (vectype1))),
		 TYPE_UNSIGNED (rhs1_type));
	      tree vectype3 = get_vectype_for_scalar_type (wide_scalar_type);
	      if (expand_vec_cond_expr_p (vectype1, vectype3, TREE_CODE (rhs1)))
		return NULL;
	    }
	}

      /* If rhs1 is a comparison we need to move it into a
	 separate statement.  */
      if (TREE_CODE (rhs1) != SSA_NAME)
	{
	  tmp = vect_recog_temp_ssa_var (TREE_TYPE (rhs1), NULL);
	  pattern_stmt = gimple_build_assign (tmp, rhs1);
	  rhs1 = tmp;

	  pattern_stmt_info = new_stmt_vec_info (pattern_stmt, vinfo);
	  set_vinfo_for_stmt (pattern_stmt, pattern_stmt_info);
	  STMT_VINFO_VECTYPE (pattern_stmt_info) = vectype2;
	  append_pattern_def_seq (stmt_vinfo, pattern_stmt);
	}

      if (maybe_ne (TYPE_VECTOR_SUBPARTS (vectype1),
		    TYPE_VECTOR_SUBPARTS (vectype2)))
	tmp = build_mask_conversion (rhs1, vectype1, stmt_vinfo, vinfo);
      else
	tmp = rhs1;

      lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
      pattern_stmt = gimple_build_assign (lhs, COND_EXPR, tmp,
					  gimple_assign_rhs2 (last_stmt),
					  gimple_assign_rhs3 (last_stmt));

      *type_out = vectype1;
      stmts->safe_push (last_stmt);
      vect_pattern_detected ("vect_recog_mask_conversion_pattern", last_stmt);

      return pattern_stmt;
    }

  /* Now check for binary boolean operations requiring conversion for
     one of operands.  */
  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (lhs)))
    return NULL;

  if (rhs_code != BIT_IOR_EXPR
      && rhs_code != BIT_XOR_EXPR
      && rhs_code != BIT_AND_EXPR
      && TREE_CODE_CLASS (rhs_code) != tcc_comparison)
    return NULL;

  rhs2 = gimple_assign_rhs2 (last_stmt);

  rhs1_type = search_type_for_mask (rhs1, vinfo);
  rhs2_type = search_type_for_mask (rhs2, vinfo);

  if (!rhs1_type || !rhs2_type
      || TYPE_PRECISION (rhs1_type) == TYPE_PRECISION (rhs2_type))
    return NULL;

  if (TYPE_PRECISION (rhs1_type) < TYPE_PRECISION (rhs2_type))
    {
      vectype1 = get_mask_type_for_scalar_type (rhs1_type);
      if (!vectype1)
	return NULL;
      rhs2 = build_mask_conversion (rhs2, vectype1, stmt_vinfo, vinfo);
    }
  else
    {
      vectype1 = get_mask_type_for_scalar_type (rhs2_type);
      if (!vectype1)
	return NULL;
      rhs1 = build_mask_conversion (rhs1, vectype1, stmt_vinfo, vinfo);
    }

  lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
  pattern_stmt = gimple_build_assign (lhs, rhs_code, rhs1, rhs2);

  *type_out = vectype1;
  stmts->safe_push (last_stmt);
  vect_pattern_detected ("vect_recog_mask_conversion_pattern", last_stmt);

  return pattern_stmt;
}

/* STMT is a load or store.  If the load or store is conditional, return
   the boolean condition under which it occurs, otherwise return null.  */

static tree
vect_get_load_store_mask (gimple *stmt)
{
  if (gassign *def_assign = dyn_cast <gassign *> (stmt))
    {
      gcc_assert (gimple_assign_single_p (def_assign));
      return NULL_TREE;
    }

  if (gcall *def_call = dyn_cast <gcall *> (stmt))
    {
      internal_fn ifn = gimple_call_internal_fn (def_call);
      int mask_index = internal_fn_mask_index (ifn);
      return gimple_call_arg (def_call, mask_index);
    }

  gcc_unreachable ();
}

/* Return the scalar offset type that an internal gather/scatter function
   should use.  GS_INFO describes the gather/scatter operation.  */

static tree
vect_get_gather_scatter_offset_type (gather_scatter_info *gs_info)
{
  tree offset_type = TREE_TYPE (gs_info->offset);
  unsigned int element_bits = tree_to_uhwi (TYPE_SIZE (gs_info->element_type));

  /* Enforced by vect_check_gather_scatter.  */
  unsigned int offset_bits = TYPE_PRECISION (offset_type);
  gcc_assert (element_bits >= offset_bits);

  /* If the offset is narrower than the elements, extend it according
     to its sign.  */
  if (element_bits > offset_bits)
    return build_nonstandard_integer_type (element_bits,
					   TYPE_UNSIGNED (offset_type));

  return offset_type;
}

/* Return MASK if MASK is suitable for masking an operation on vectors
   of type VECTYPE, otherwise convert it into such a form and return
   the result.  Associate any conversion statements with STMT_INFO's
   pattern.  */

static tree
vect_convert_mask_for_vectype (tree mask, tree vectype,
			       stmt_vec_info stmt_info, vec_info *vinfo)
{
  tree mask_type = search_type_for_mask (mask, vinfo);
  if (mask_type)
    {
      tree mask_vectype = get_mask_type_for_scalar_type (mask_type);
      if (mask_vectype
	  && maybe_ne (TYPE_VECTOR_SUBPARTS (vectype),
		       TYPE_VECTOR_SUBPARTS (mask_vectype)))
	mask = build_mask_conversion (mask, vectype, stmt_info, vinfo);
    }
  return mask;
}

/* Return the equivalent of:

     fold_convert (TYPE, VALUE)

   with the expectation that the operation will be vectorized.
   If new statements are needed, add them as pattern statements
   to STMT_INFO.  */

static tree
vect_add_conversion_to_patterm (tree type, tree value,
				stmt_vec_info stmt_info,
				vec_info *vinfo)
{
  if (useless_type_conversion_p (type, TREE_TYPE (value)))
    return value;

  tree new_value = vect_recog_temp_ssa_var (type, NULL);
  gassign *conversion = gimple_build_assign (new_value, CONVERT_EXPR, value);
  stmt_vec_info new_stmt_info = new_stmt_vec_info (conversion, vinfo);
  set_vinfo_for_stmt (conversion, new_stmt_info);
  STMT_VINFO_VECTYPE (new_stmt_info) = get_vectype_for_scalar_type (type);
  append_pattern_def_seq (stmt_info, conversion);
  return new_value;
}

/* Try to convert STMT into a call to a gather load or scatter store
   internal function.  Return the final statement on success and set
   *TYPE_OUT to the vector type being loaded or stored.

   This function only handles gathers and scatters that were recognized
   as such from the outset (indicated by STMT_VINFO_GATHER_SCATTER_P).  */

static gimple *
vect_try_gather_scatter_pattern (gimple *stmt, stmt_vec_info last_stmt_info,
				 tree *type_out)
{
  /* Currently we only support this for loop vectorization.  */
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (stmt_info->vinfo);
  if (!loop_vinfo)
    return NULL;

  /* Make sure that we're looking at a gather load or scatter store.  */
  data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  if (!dr || !STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    return NULL;

  /* Get the boolean that controls whether the load or store happens.
     This is null if the operation is unconditional.  */
  tree mask = vect_get_load_store_mask (stmt);

  /* Make sure that the target supports an appropriate internal
     function for the gather/scatter operation.  */
  gather_scatter_info gs_info;
  if (!vect_check_gather_scatter (stmt, loop_vinfo, &gs_info)
      || gs_info.decl)
    return NULL;

  /* Convert the mask to the right form.  */
  tree gs_vectype = get_vectype_for_scalar_type (gs_info.element_type);
  if (mask)
    mask = vect_convert_mask_for_vectype (mask, gs_vectype, last_stmt_info,
					  loop_vinfo);

  /* Get the invariant base and non-invariant offset, converting the
     latter to the same width as the vector elements.  */
  tree base = gs_info.base;
  tree offset_type = vect_get_gather_scatter_offset_type (&gs_info);
  tree offset = vect_add_conversion_to_patterm (offset_type, gs_info.offset,
						last_stmt_info, loop_vinfo);

  /* Build the new pattern statement.  */
  tree scale = size_int (gs_info.scale);
  gcall *pattern_stmt;
  if (DR_IS_READ (dr))
    {
      if (mask != NULL)
	pattern_stmt = gimple_build_call_internal (gs_info.ifn, 4, base,
						   offset, scale, mask);
      else
	pattern_stmt = gimple_build_call_internal (gs_info.ifn, 3, base,
						   offset, scale);
      tree load_lhs = vect_recog_temp_ssa_var (gs_info.element_type, NULL);
      gimple_call_set_lhs (pattern_stmt, load_lhs);
    }
  else
    {
      tree rhs = vect_get_store_rhs (stmt);
      if (mask != NULL)
	pattern_stmt = gimple_build_call_internal (IFN_MASK_SCATTER_STORE, 5,
						   base, offset, scale, rhs,
						   mask);
      else
	pattern_stmt = gimple_build_call_internal (IFN_SCATTER_STORE, 4,
						   base, offset, scale, rhs);
    }
  gimple_call_set_nothrow (pattern_stmt, true);

  /* Copy across relevant vectorization info and associate DR with the
     new pattern statement instead of the original statement.  */
  stmt_vec_info pattern_stmt_info = new_stmt_vec_info (pattern_stmt,
						       loop_vinfo);
  set_vinfo_for_stmt (pattern_stmt, pattern_stmt_info);
  STMT_VINFO_DATA_REF (pattern_stmt_info) = dr;
  STMT_VINFO_DR_WRT_VEC_LOOP (pattern_stmt_info)
    = STMT_VINFO_DR_WRT_VEC_LOOP (stmt_info);
  STMT_VINFO_GATHER_SCATTER_P (pattern_stmt_info)
    = STMT_VINFO_GATHER_SCATTER_P (stmt_info);

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  *type_out = vectype;
  vect_pattern_detected ("gather/scatter pattern", stmt);

  return pattern_stmt;
}

/* Pattern wrapper around vect_try_gather_scatter_pattern.  */

static gimple *
vect_recog_gather_scatter_pattern (vec<gimple *> *stmts, tree *type_out)
{
  gimple *last_stmt = stmts->pop ();
  stmt_vec_info last_stmt_info = vinfo_for_stmt (last_stmt);
  gimple *pattern_stmt = vect_try_gather_scatter_pattern (last_stmt,
							  last_stmt_info,
							  type_out);
  if (pattern_stmt)
    stmts->safe_push (last_stmt);
  return pattern_stmt;
}

typedef gimple *(*vect_recog_func_ptr) (vec<gimple *> *, tree *);

struct vect_recog_func
{
  vect_recog_func_ptr fn;
  const char *name;
};

/* Note that ordering matters - the first pattern matching on a stmt is
   taken which means usually the more complex one needs to preceed the
   less comples onex (widen_sum only after dot_prod or sad for example).  */
static vect_recog_func vect_vect_recog_func_ptrs[] = {
  { vect_recog_widen_mult_pattern, "widen_mult" },
  { vect_recog_dot_prod_pattern, "dot_prod" },
  { vect_recog_sad_pattern, "sad" },
  { vect_recog_widen_sum_pattern, "widen_sum" },
  { vect_recog_pow_pattern, "pow" },
  { vect_recog_widen_shift_pattern, "widen_shift" },
  { vect_recog_over_widening_pattern, "over_widening" },
  { vect_recog_rotate_pattern, "rotate" },
  { vect_recog_vector_vector_shift_pattern, "vector_vector_shift" },
  { vect_recog_divmod_pattern, "divmod" },
  { vect_recog_mult_pattern, "mult" },
  { vect_recog_mixed_size_cond_pattern, "mixed_size_cond" },
  { vect_recog_bool_pattern, "bool" },
  /* This must come before mask conversion, and includes the parts
     of mask conversion that are needed for gather and scatter
     internal functions.  */
  { vect_recog_gather_scatter_pattern, "gather_scatter" },
  { vect_recog_mask_conversion_pattern, "mask_conversion" }
};

const unsigned int NUM_PATTERNS = ARRAY_SIZE (vect_vect_recog_func_ptrs);

/* Mark statements that are involved in a pattern.  */

static inline void
vect_mark_pattern_stmts (gimple *orig_stmt, gimple *pattern_stmt,
                         tree pattern_vectype)
{
  stmt_vec_info orig_stmt_info = vinfo_for_stmt (orig_stmt);
  gimple *def_seq = STMT_VINFO_PATTERN_DEF_SEQ (orig_stmt_info);

  bool old_pattern_p = is_pattern_stmt_p (orig_stmt_info);
  if (old_pattern_p)
    {
      /* We're replacing a statement in an existing pattern definition
	 sequence.  */
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "replacing earlier pattern ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, orig_stmt, 0);
	}

      /* To keep the book-keeping simple, just swap the lhs of the
	 old and new statements, so that the old one has a valid but
	 unused lhs.  */
      tree old_lhs = gimple_get_lhs (orig_stmt);
      gimple_set_lhs (orig_stmt, gimple_get_lhs (pattern_stmt));
      gimple_set_lhs (pattern_stmt, old_lhs);

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "with ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, pattern_stmt, 0);
	}

      /* Switch to the statement that ORIG replaces.  */
      orig_stmt_info
	= vinfo_for_stmt (STMT_VINFO_RELATED_STMT (orig_stmt_info));

      /* We shouldn't be replacing the main pattern statement.  */
      gcc_assert (STMT_VINFO_RELATED_STMT (orig_stmt_info) != orig_stmt);
    }

  if (def_seq)
    for (gimple_stmt_iterator si = gsi_start (def_seq);
	 !gsi_end_p (si); gsi_next (&si))
      vect_init_pattern_stmt (gsi_stmt (si), orig_stmt_info, pattern_vectype);

  if (old_pattern_p)
    {
      vect_init_pattern_stmt (pattern_stmt, orig_stmt_info, pattern_vectype);

      /* Insert all the new pattern statements before the original one.  */
      gimple_seq *orig_def_seq = &STMT_VINFO_PATTERN_DEF_SEQ (orig_stmt_info);
      gimple_stmt_iterator gsi = gsi_for_stmt (orig_stmt, orig_def_seq);
      gsi_insert_seq_before_without_update (&gsi, def_seq, GSI_SAME_STMT);
      gsi_insert_before_without_update (&gsi, pattern_stmt, GSI_SAME_STMT);
    }
  else
    vect_set_pattern_stmt (pattern_stmt, orig_stmt_info, pattern_vectype);
}

/* Function vect_pattern_recog_1

   Input:
   PATTERN_RECOG_FUNC: A pointer to a function that detects a certain
        computation pattern.
   STMT: A stmt from which the pattern search should start.

   If PATTERN_RECOG_FUNC successfully detected the pattern, it creates
   a sequence of statements that has the same functionality and can be
   used to replace STMT.  It returns the last statement in the sequence
   and adds any earlier statements to STMT's STMT_VINFO_PATTERN_DEF_SEQ.
   PATTERN_RECOG_FUNC also sets *TYPE_OUT to the vector type of the final
   statement, having first checked that the target supports the new operation
   in that type.

   This function also does some bookkeeping, as explained in the documentation
   for vect_recog_pattern.  */

static void
vect_pattern_recog_1 (vect_recog_func *recog_func,
		      gimple_stmt_iterator si,
		      vec<gimple *> *stmts_to_replace)
{
  gimple *stmt = gsi_stmt (si), *pattern_stmt;
  stmt_vec_info stmt_info;
  loop_vec_info loop_vinfo;
  tree pattern_vectype;
  int i;

  /* If this statement has already been replaced with pattern statements,
     leave the original statement alone, since the first match wins.
     Instead try to match against the definition statements that feed
     the main pattern statement.  */
  stmt_info = vinfo_for_stmt (stmt);
  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start (STMT_VINFO_PATTERN_DEF_SEQ (stmt_info));
	   !gsi_end_p (gsi); gsi_next (&gsi))
	vect_pattern_recog_1 (recog_func, gsi, stmts_to_replace);
      return;
    }

  stmts_to_replace->truncate (0);
  stmts_to_replace->quick_push (stmt);
  pattern_stmt = recog_func->fn (stmts_to_replace, &pattern_vectype);
  if (!pattern_stmt)
    {
      /* Clear related stmt info that analysis might have noted for
         to be replaced stmts.  */
      for (i = 0; stmts_to_replace->iterate (i, &stmt)
	   && (unsigned) i < stmts_to_replace->length ();
	   i++)
	{
	  stmt_info = vinfo_for_stmt (stmt);
	  if (!is_pattern_stmt_p (stmt_info))
	    STMT_VINFO_RELATED_STMT (stmt_info) = NULL;
	}
      return;
    }

  stmt = stmts_to_replace->last ();
  stmt_info = vinfo_for_stmt (stmt);
  loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  gcc_assert (pattern_vectype);
 
  /* Found a vectorizable pattern.  */
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
                       "%s pattern recognized: ", recog_func->name);
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, pattern_stmt, 0);
    }

  /* Mark the stmts that are involved in the pattern. */
  vect_mark_pattern_stmts (stmt, pattern_stmt, pattern_vectype);

  /* Patterns cannot be vectorized using SLP, because they change the order of
     computation.  */
  if (loop_vinfo)
    {
      unsigned ix, ix2;
      gimple **elem_ptr;
      VEC_ORDERED_REMOVE_IF (LOOP_VINFO_REDUCTIONS (loop_vinfo), ix, ix2,
			     elem_ptr, *elem_ptr == stmt);
    }

  /* It is possible that additional pattern stmts are created and inserted in
     STMTS_TO_REPLACE.  We create a stmt_info for each of them, and mark the
     relevant statements.  */
  for (i = 0; stmts_to_replace->iterate (i, &stmt)
	      && (unsigned) i < (stmts_to_replace->length () - 1);
       i++)
    {
      stmt_info = vinfo_for_stmt (stmt);
      pattern_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location,
                           "additional pattern stmt: ");
          dump_gimple_stmt (MSG_NOTE, TDF_SLIM, pattern_stmt, 0);
        }

      vect_mark_pattern_stmts (stmt, pattern_stmt, NULL_TREE);
    }

  return;
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
vect_pattern_recog (vec_info *vinfo)
{
  struct loop *loop;
  basic_block *bbs;
  unsigned int nbbs;
  gimple_stmt_iterator si;
  unsigned int i, j;
  auto_vec<gimple *, 1> stmts_to_replace;

  DUMP_VECT_SCOPE ("vect_pattern_recog");

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      bbs = LOOP_VINFO_BBS (loop_vinfo);
      nbbs = loop->num_nodes;

      /* Scan through the loop stmts, applying the pattern recognition
	 functions starting at each stmt visited:  */
      for (i = 0; i < nbbs; i++)
	{
	  basic_block bb = bbs[i];
	  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	    /* Scan over all generic vect_recog_xxx_pattern functions.  */
	    for (j = 0; j < NUM_PATTERNS; j++)
	      vect_pattern_recog_1 (&vect_vect_recog_func_ptrs[j], si,
				    &stmts_to_replace);
	}
    }
  else
    {
      bb_vec_info bb_vinfo = as_a <bb_vec_info> (vinfo);
      for (si = bb_vinfo->region_begin;
	   gsi_stmt (si) != gsi_stmt (bb_vinfo->region_end); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  if (stmt_info && !STMT_VINFO_VECTORIZABLE (stmt_info))
	    continue;

	  /* Scan over all generic vect_recog_xxx_pattern functions.  */
	  for (j = 0; j < NUM_PATTERNS; j++)
	    vect_pattern_recog_1 (&vect_vect_recog_func_ptrs[j], si,
				  &stmts_to_replace);
	}
    }
}
