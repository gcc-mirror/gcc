/* Analysis Utilities for Loop Vectorization.
   Copyright (C) 2006-2024 Free Software Foundation, Inc.
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
#include "gimple-iterator.h"
#include "gimple-fold.h"
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
#include "gimple-fold.h"
#include "gimplify-me.h"
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
#include "tree-vector-builder.h"
#include "tree-ssa-loop-ivopts.h"
#include "vec-perm-indices.h"
#include "gimple-range.h"
#include "alias.h"


/* TODO:  Note the vectorizer still builds COND_EXPRs with GENERIC compares
   in the first operand.  Disentangling this is future work, the
   IL is properly transfered to VEC_COND_EXPRs with separate compares.  */


/* Return true if we have a useful VR_RANGE range for VAR, storing it
   in *MIN_VALUE and *MAX_VALUE if so.  Note the range in the dump files.  */

bool
vect_get_range_info (tree var, wide_int *min_value, wide_int *max_value)
{
  int_range_max vr;
  tree vr_min, vr_max;
  get_range_query (cfun)->range_of_expr (vr, var);
  if (vr.undefined_p ())
    vr.set_varying (TREE_TYPE (var));
  value_range_kind vr_type = get_legacy_range (vr, vr_min, vr_max);
  *min_value = wi::to_wide (vr_min);
  *max_value = wi::to_wide (vr_max);
  wide_int nonzero = get_nonzero_bits (var);
  signop sgn = TYPE_SIGN (TREE_TYPE (var));
  if (intersect_range_with_nonzero_bits (vr_type, min_value, max_value,
					 nonzero, sgn) == VR_RANGE)
    {
      if (dump_enabled_p ())
	{
	  dump_generic_expr_loc (MSG_NOTE, vect_location, TDF_SLIM, var);
	  dump_printf (MSG_NOTE, " has range [");
	  dump_hex (MSG_NOTE, *min_value);
	  dump_printf (MSG_NOTE, ", ");
	  dump_hex (MSG_NOTE, *max_value);
	  dump_printf (MSG_NOTE, "]\n");
	}
      return true;
    }
  else
    {
      if (dump_enabled_p ())
	{
	  dump_generic_expr_loc (MSG_NOTE, vect_location, TDF_SLIM, var);
	  dump_printf (MSG_NOTE, " has no range info\n");
	}
      return false;
    }
}

/* Report that we've found an instance of pattern PATTERN in
   statement STMT.  */

static void
vect_pattern_detected (const char *name, gimple *stmt)
{
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "%s: detected: %G", name, stmt);
}

/* Associate pattern statement PATTERN_STMT with ORIG_STMT_INFO and
   return the pattern statement's stmt_vec_info.  Set its vector type to
   VECTYPE if it doesn't have one already.  */

static stmt_vec_info
vect_init_pattern_stmt (vec_info *vinfo, gimple *pattern_stmt,
			stmt_vec_info orig_stmt_info, tree vectype)
{
  stmt_vec_info pattern_stmt_info = vinfo->lookup_stmt (pattern_stmt);
  if (pattern_stmt_info == NULL)
    pattern_stmt_info = vinfo->add_stmt (pattern_stmt);
  gimple_set_bb (pattern_stmt, gimple_bb (orig_stmt_info->stmt));

  pattern_stmt_info->pattern_stmt_p = true;
  STMT_VINFO_RELATED_STMT (pattern_stmt_info) = orig_stmt_info;
  STMT_VINFO_DEF_TYPE (pattern_stmt_info)
    = STMT_VINFO_DEF_TYPE (orig_stmt_info);
  STMT_VINFO_TYPE (pattern_stmt_info) = STMT_VINFO_TYPE (orig_stmt_info);
  if (!STMT_VINFO_VECTYPE (pattern_stmt_info))
    {
      gcc_assert (!vectype
		  || is_a <gcond *> (pattern_stmt)
		  || (VECTOR_BOOLEAN_TYPE_P (vectype)
		      == vect_use_mask_type_p (orig_stmt_info)));
      STMT_VINFO_VECTYPE (pattern_stmt_info) = vectype;
      pattern_stmt_info->mask_precision = orig_stmt_info->mask_precision;
    }
  return pattern_stmt_info;
}

/* Set the pattern statement of ORIG_STMT_INFO to PATTERN_STMT.
   Also set the vector type of PATTERN_STMT to VECTYPE, if it doesn't
   have one already.  */

static void
vect_set_pattern_stmt (vec_info *vinfo, gimple *pattern_stmt,
		       stmt_vec_info orig_stmt_info, tree vectype)
{
  STMT_VINFO_IN_PATTERN_P (orig_stmt_info) = true;
  STMT_VINFO_RELATED_STMT (orig_stmt_info)
    = vect_init_pattern_stmt (vinfo, pattern_stmt, orig_stmt_info, vectype);
}

/* Add NEW_STMT to STMT_INFO's pattern definition statements.  If VECTYPE
   is nonnull, record that NEW_STMT's vector type is VECTYPE, which might
   be different from the vector type of the final pattern statement.
   If VECTYPE is a mask type, SCALAR_TYPE_FOR_MASK is the scalar type
   from which it was derived.  */

static inline void
append_pattern_def_seq (vec_info *vinfo,
			stmt_vec_info stmt_info, gimple *new_stmt,
			tree vectype = NULL_TREE,
			tree scalar_type_for_mask = NULL_TREE)
{
  gcc_assert (!scalar_type_for_mask
	      == (!vectype || !VECTOR_BOOLEAN_TYPE_P (vectype)));
  if (vectype)
    {
      stmt_vec_info new_stmt_info = vinfo->add_stmt (new_stmt);
      STMT_VINFO_VECTYPE (new_stmt_info) = vectype;
      if (scalar_type_for_mask)
	new_stmt_info->mask_precision
	  = GET_MODE_BITSIZE (SCALAR_TYPE_MODE (scalar_type_for_mask));
    }
  gimple_seq_add_stmt_without_update (&STMT_VINFO_PATTERN_DEF_SEQ (stmt_info),
				      new_stmt);
}


/* Add NEW_STMT to VINFO's invariant pattern definition statements.  These
   statements are not vectorized but are materialized as scalar in the loop
   preheader.  */

static inline void
append_inv_pattern_def_seq (vec_info *vinfo, gimple *new_stmt)
{
  gimple_seq_add_stmt_without_update (&vinfo->inv_pattern_def_seq, new_stmt);
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
   where CODE is known to map to a direct optab with the given SUBTYPE.
   ITYPE specifies the type of (some of) the scalar inputs and OTYPE
   specifies the type of the scalar result.

   If CODE allows the inputs and outputs to have different type
   (such as for WIDEN_SUM_EXPR), it is the input mode rather
   than the output mode that determines the appropriate target pattern.
   Operand 0 of the target pattern then specifies the mode that the output
   must have.

   When returning true, set *VECOTYPE_OUT to the vector version of OTYPE.
   Also set *VECITYPE_OUT to the vector version of ITYPE if VECITYPE_OUT
   is nonnull.  */

static bool
vect_supportable_direct_optab_p (vec_info *vinfo, tree otype, tree_code code,
				 tree itype, tree *vecotype_out,
				 tree *vecitype_out = NULL,
				 enum optab_subtype subtype = optab_default)
{
  tree vecitype = get_vectype_for_scalar_type (vinfo, itype);
  if (!vecitype)
    return false;

  tree vecotype = get_vectype_for_scalar_type (vinfo, otype);
  if (!vecotype)
    return false;

  optab optab = optab_for_tree_code (code, vecitype, subtype);
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

/* Return true if the target supports a vector version of CODE,
   where CODE is known to map to a conversion optab with the given SUBTYPE.
   ITYPE specifies the type of (some of) the scalar inputs and OTYPE
   specifies the type of the scalar result.

   When returning true, set *VECOTYPE_OUT to the vector version of OTYPE.
   Also set *VECITYPE_OUT to the vector version of ITYPE if VECITYPE_OUT
   is nonnull.  */

static bool
vect_supportable_conv_optab_p (vec_info *vinfo, tree otype, tree_code code,
				 tree itype, tree *vecotype_out,
				 tree *vecitype_out = NULL,
				 enum optab_subtype subtype = optab_default)
{
  tree vecitype = get_vectype_for_scalar_type (vinfo, itype);
  tree vecotype = get_vectype_for_scalar_type (vinfo, otype);
  if (!vecitype || !vecotype)
    return false;

  if (!directly_supported_p (code, vecotype, vecitype, subtype))
    return false;

  *vecotype_out = vecotype;
  if (vecitype_out)
    *vecitype_out = vecitype;
  return true;
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
  stmt_vec_info def_stmt_info = vinfo->lookup_def (op);
  if (def_stmt_info
      && STMT_VINFO_DEF_TYPE (def_stmt_info) == vect_internal_def)
    return vect_stmt_to_vectorize (def_stmt_info);
  return NULL;
}

/* Holds information about an input operand after some sign changes
   and type promotions have been peeled away.  */
class vect_unpromoted_value {
public:
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
   input in UNPROM, otherwise describe OP' in UNPROM.  If SINGLE_USE_P
   is nonnull, set *SINGLE_USE_P to false if any of the SSA names involved
   have more than one user.

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
				      vect_unpromoted_value *unprom,
				      bool *single_use_p = NULL)
{
  tree op_type = TREE_TYPE (op);
  if (!INTEGRAL_TYPE_P (op_type))
    return NULL_TREE;

  tree res = NULL_TREE;
  unsigned int orig_precision = TYPE_PRECISION (op_type);
  unsigned int min_precision = orig_precision;
  stmt_vec_info caster = NULL;
  while (TREE_CODE (op) == SSA_NAME && INTEGRAL_TYPE_P (op_type))
    {
      /* See whether OP is simple enough to vectorize.  */
      stmt_vec_info def_stmt_info;
      gimple *def_stmt;
      vect_def_type dt;
      if (!vect_is_simple_use (op, vinfo, &dt, &def_stmt_info, &def_stmt))
	break;

      /* If OP is the input of a demotion, skip over it to see whether
	 OP is itself the result of a promotion.  If so, the combined
	 effect of the promotion and the demotion might fit the required
	 pattern, otherwise neither operation fits.

	 This copes with cases such as the result of an arithmetic
	 operation being truncated before being stored, and where that
	 arithmetic operation has been recognized as an over-widened one.  */
      if (TYPE_PRECISION (op_type) <= min_precision)
	{
	  /* Use OP as the UNPROM described above if we haven't yet
	     found a promotion, or if using the new input preserves the
	     sign of the previous promotion.  */
	  if (!res
	      || TYPE_PRECISION (unprom->type) == orig_precision
	      || TYPE_SIGN (unprom->type) == TYPE_SIGN (op_type)
	      || (TYPE_UNSIGNED (op_type)
		  && TYPE_PRECISION (op_type) < TYPE_PRECISION (unprom->type)))
	    {
	      unprom->set_op (op, dt, caster);
	      min_precision = TYPE_PRECISION (op_type);
	    }
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
      caster = def_stmt_info;

      /* Ignore pattern statements, since we don't link uses for them.  */
      if (caster
	  && single_use_p
	  && !STMT_VINFO_RELATED_STMT (caster)
	  && !has_single_use (res))
	*single_use_p = false;

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

   If SUBTYPE then allow that the signs of the operands
   may differ in signs but not in precision.  SUBTYPE is updated to reflect
   this.

   Return 0 if STMT_INFO isn't such a tree, or if no such COMMON_TYPE
   exists.  */

static unsigned int
vect_widened_op_tree (vec_info *vinfo, stmt_vec_info stmt_info, tree_code code,
		      code_helper widened_code, bool shift_p,
		      unsigned int max_nops,
		      vect_unpromoted_value *unprom, tree *common_type,
		      enum optab_subtype *subtype = NULL)
{
  /* Check for an integer operation with the right code.  */
  gimple* stmt = stmt_info->stmt;
  if (!(is_gimple_assign (stmt) || is_gimple_call (stmt)))
    return 0;

  code_helper rhs_code;
  if (is_gimple_assign (stmt))
    rhs_code = gimple_assign_rhs_code (stmt);
  else if (is_gimple_call (stmt))
    rhs_code = gimple_call_combined_fn (stmt);
  else
    return 0;

  if (rhs_code != code
      && rhs_code != widened_code)
    return 0;

  tree lhs = gimple_get_lhs (stmt);
  tree type = TREE_TYPE (lhs);
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
      tree op = gimple_arg (stmt, i);
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

	  if (rhs_code != code)
	    {
	      /* If rhs_code is widened_code, don't look through further
		 possible promotions, there is a promotion already embedded
		 in the WIDEN_*_EXPR.  */
	      if (TREE_CODE (op) != SSA_NAME
		  || !INTEGRAL_TYPE_P (TREE_TYPE (op)))
		return 0;

	      stmt_vec_info def_stmt_info;
	      gimple *def_stmt;
	      vect_def_type dt;
	      if (!vect_is_simple_use (op, vinfo, &dt, &def_stmt_info,
				       &def_stmt))
		return 0;
	      this_unprom->set_op (op, dt, NULL);
	    }
	  else if (!vect_look_through_possible_promotion (vinfo, op,
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
		= vect_get_internal_def (vinfo, this_unprom->op);

	      nops = vect_widened_op_tree (vinfo, def_stmt_info, code,
					   widened_code, shift_p, max_nops,
					   this_unprom, common_type,
					   subtype);
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
		{
		  if (subtype)
		    {
		      /* See if we can sign extend the smaller type.  */
		      if (TYPE_PRECISION (this_unprom->type)
			  > TYPE_PRECISION (*common_type))
			*common_type = this_unprom->type;
		      *subtype = optab_vector_mixed_sign;
		    }
		  else
		    return 0;
		}
	    }
	}
      next_op += nops;
    }
  return next_op;
}

/* Helper to return a new temporary for pattern of TYPE for STMT.  If STMT
   is NULL, the caller must set SSA_NAME_DEF_STMT for the returned SSA var. */

static tree
vect_recog_temp_ssa_var (tree type, gimple *stmt = NULL)
{
  return make_temp_ssa_name (type, stmt, "patt");
}

/* STMT2_INFO describes a type conversion that could be split into STMT1
   followed by a version of STMT2_INFO that takes NEW_RHS as its first
   input.  Try to do this using pattern statements, returning true on
   success.  */

static bool
vect_split_statement (vec_info *vinfo, stmt_vec_info stmt2_info, tree new_rhs,
		      gimple *stmt1, tree vectype)
{
  if (is_pattern_stmt_p (stmt2_info))
    {
      /* STMT2_INFO is part of a pattern.  Get the statement to which
	 the pattern is attached.  */
      stmt_vec_info orig_stmt2_info = STMT_VINFO_RELATED_STMT (stmt2_info);
      vect_init_pattern_stmt (vinfo, stmt1, orig_stmt2_info, vectype);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Splitting pattern statement: %G", stmt2_info->stmt);

      /* Since STMT2_INFO is a pattern statement, we can change it
	 in-situ without worrying about changing the code for the
	 containing block.  */
      gimple_assign_set_rhs1 (stmt2_info->stmt, new_rhs);

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "into: %G", stmt1);
	  dump_printf_loc (MSG_NOTE, vect_location, "and: %G",
			   stmt2_info->stmt);
	}

      gimple_seq *def_seq = &STMT_VINFO_PATTERN_DEF_SEQ (orig_stmt2_info);
      if (STMT_VINFO_RELATED_STMT (orig_stmt2_info) == stmt2_info)
	/* STMT2_INFO is the actual pattern statement.  Add STMT1
	   to the end of the definition sequence.  */
	gimple_seq_add_stmt_without_update (def_seq, stmt1);
      else
	{
	  /* STMT2_INFO belongs to the definition sequence.  Insert STMT1
	     before it.  */
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt2_info->stmt, def_seq);
	  gsi_insert_before_without_update (&gsi, stmt1, GSI_SAME_STMT);
	}
      return true;
    }
  else
    {
      /* STMT2_INFO doesn't yet have a pattern.  Try to create a
	 two-statement pattern now.  */
      gcc_assert (!STMT_VINFO_RELATED_STMT (stmt2_info));
      tree lhs_type = TREE_TYPE (gimple_get_lhs (stmt2_info->stmt));
      tree lhs_vectype = get_vectype_for_scalar_type (vinfo, lhs_type);
      if (!lhs_vectype)
	return false;

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "Splitting statement: %G", stmt2_info->stmt);

      /* Add STMT1 as a singleton pattern definition sequence.  */
      gimple_seq *def_seq = &STMT_VINFO_PATTERN_DEF_SEQ (stmt2_info);
      vect_init_pattern_stmt (vinfo, stmt1, stmt2_info, vectype);
      gimple_seq_add_stmt_without_update (def_seq, stmt1);

      /* Build the second of the two pattern statements.  */
      tree new_lhs = vect_recog_temp_ssa_var (lhs_type, NULL);
      gassign *new_stmt2 = gimple_build_assign (new_lhs, NOP_EXPR, new_rhs);
      vect_set_pattern_stmt (vinfo, new_stmt2, stmt2_info, lhs_vectype);

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "into pattern statements: %G", stmt1);
	  dump_printf_loc (MSG_NOTE, vect_location, "and: %G",
			   (gimple *) new_stmt2);
	}

      return true;
    }
}

/* Look for the following pattern
	X = x[i]
	Y = y[i]
	DIFF = X - Y
	DAD = ABS_EXPR<DIFF>

   ABS_STMT should point to a statement of code ABS_EXPR or ABSU_EXPR.
   HALF_TYPE and UNPROM will be set should the statement be found to
   be a widened operation.
   DIFF_STMT will be set to the MINUS_EXPR
   statement that precedes the ABS_STMT if it is a MINUS_EXPR..
 */
static bool
vect_recog_absolute_difference (vec_info *vinfo, gassign *abs_stmt,
				tree *half_type,
				vect_unpromoted_value unprom[2],
				gassign **diff_stmt)
{
  if (!abs_stmt)
    return false;

  /* FORNOW.  Can continue analyzing the def-use chain when this stmt in a phi
     inside the loop (in case we are analyzing an outer-loop).  */
  enum tree_code code = gimple_assign_rhs_code (abs_stmt);
  if (code != ABS_EXPR && code != ABSU_EXPR)
    return false;

  tree abs_oprnd = gimple_assign_rhs1 (abs_stmt);
  tree abs_type = TREE_TYPE (abs_oprnd);
  if (!abs_oprnd)
    return false;
  if (!ANY_INTEGRAL_TYPE_P (abs_type)
      || TYPE_OVERFLOW_WRAPS (abs_type)
      || TYPE_UNSIGNED (abs_type))
    return false;

  /* Peel off conversions from the ABS input.  This can involve sign
     changes (e.g. from an unsigned subtraction to a signed ABS input)
     or signed promotion, but it can't include unsigned promotion.
     (Note that ABS of an unsigned promotion should have been folded
     away before now anyway.)  */
  vect_unpromoted_value unprom_diff;
  abs_oprnd = vect_look_through_possible_promotion (vinfo, abs_oprnd,
						    &unprom_diff);
  if (!abs_oprnd)
    return false;
  if (TYPE_PRECISION (unprom_diff.type) != TYPE_PRECISION (abs_type)
      && TYPE_UNSIGNED (unprom_diff.type))
    return false;

  /* We then detect if the operand of abs_expr is defined by a minus_expr.  */
  stmt_vec_info diff_stmt_vinfo = vect_get_internal_def (vinfo, abs_oprnd);
  if (!diff_stmt_vinfo)
    return false;

  gassign *diff = dyn_cast <gassign *> (STMT_VINFO_STMT (diff_stmt_vinfo));
  if (diff_stmt && diff
      && gimple_assign_rhs_code (diff) == MINUS_EXPR
      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (abs_oprnd)))
    *diff_stmt = diff;

  /* FORNOW.  Can continue analyzing the def-use chain when this stmt in a phi
     inside the loop (in case we are analyzing an outer-loop).  */
  if (vect_widened_op_tree (vinfo, diff_stmt_vinfo,
			    MINUS_EXPR, IFN_VEC_WIDEN_MINUS,
			    false, 2, unprom, half_type))
    return true;

  return false;
}

/* Convert UNPROM to TYPE and return the result, adding new statements
   to STMT_INFO's pattern definition statements if no better way is
   available.  VECTYPE is the vector form of TYPE.

   If SUBTYPE then convert the type based on the subtype.  */

static tree
vect_convert_input (vec_info *vinfo, stmt_vec_info stmt_info, tree type,
		    vect_unpromoted_value *unprom, tree vectype,
		    enum optab_subtype subtype = optab_default)
{
  /* Update the type if the signs differ.  */
  if (subtype == optab_vector_mixed_sign)
    {
      gcc_assert (!TYPE_UNSIGNED (type));
      if (TYPE_UNSIGNED (TREE_TYPE (unprom->op)))
	{
	  type = unsigned_type_for (type);
	  vectype = unsigned_type_for (vectype);
	}
    }

  /* Check for a no-op conversion.  */
  if (types_compatible_p (type, TREE_TYPE (unprom->op)))
    return unprom->op;

  /* Allow the caller to create constant vect_unpromoted_values.  */
  if (TREE_CODE (unprom->op) == INTEGER_CST)
    return wide_int_to_tree (type, wi::to_widest (unprom->op));

  tree input = unprom->op;
  if (unprom->caster)
    {
      tree lhs = gimple_get_lhs (unprom->caster->stmt);
      tree lhs_type = TREE_TYPE (lhs);

      /* If the result of the existing cast is the right width, use it
	 instead of the source of the cast.  */
      if (TYPE_PRECISION (lhs_type) == TYPE_PRECISION (type))
	input = lhs;
      /* If the precision we want is between the source and result
	 precisions of the existing cast, try splitting the cast into
	 two and tapping into a mid-way point.  */
      else if (TYPE_PRECISION (lhs_type) > TYPE_PRECISION (type)
	       && TYPE_PRECISION (type) > TYPE_PRECISION (unprom->type))
	{
	  /* In order to preserve the semantics of the original cast,
	     give the mid-way point the same signedness as the input value.

	     It would be possible to use a signed type here instead if
	     TYPE is signed and UNPROM->TYPE is unsigned, but that would
	     make the sign of the midtype sensitive to the order in
	     which we process the statements, since the signedness of
	     TYPE is the signedness required by just one of possibly
	     many users.  Also, unsigned promotions are usually as cheap
	     as or cheaper than signed ones, so it's better to keep an
	     unsigned promotion.  */
	  tree midtype = build_nonstandard_integer_type
	    (TYPE_PRECISION (type), TYPE_UNSIGNED (unprom->type));
	  tree vec_midtype = get_vectype_for_scalar_type (vinfo, midtype);
	  if (vec_midtype)
	    {
	      input = vect_recog_temp_ssa_var (midtype, NULL);
	      gassign *new_stmt = gimple_build_assign (input, NOP_EXPR,
						       unprom->op);
	      if (!vect_split_statement (vinfo, unprom->caster, input, new_stmt,
					 vec_midtype))
		append_pattern_def_seq (vinfo, stmt_info,
					new_stmt, vec_midtype);
	    }
	}

      /* See if we can reuse an existing result.  */
      if (types_compatible_p (type, TREE_TYPE (input)))
	return input;
    }

  /* We need a new conversion statement.  */
  tree new_op = vect_recog_temp_ssa_var (type, NULL);
  gassign *new_stmt = gimple_build_assign (new_op, NOP_EXPR, input);

  /* If OP is an external value, see if we can insert the new statement
     on an incoming edge.  */
  if (input == unprom->op && unprom->dt == vect_external_def)
    if (edge e = vect_get_external_def_edge (vinfo, input))
      {
	basic_block new_bb = gsi_insert_on_edge_immediate (e, new_stmt);
	gcc_assert (!new_bb);
	return new_op;
      }

  /* As a (common) last resort, add the statement to the pattern itself.  */
  append_pattern_def_seq (vinfo, stmt_info, new_stmt, vectype);
  return new_op;
}

/* Invoke vect_convert_input for N elements of UNPROM and store the
   result in the corresponding elements of RESULT.

   If SUBTYPE then convert the type based on the subtype.  */

static void
vect_convert_inputs (vec_info *vinfo, stmt_vec_info stmt_info, unsigned int n,
		     tree *result, tree type, vect_unpromoted_value *unprom,
		     tree vectype, enum optab_subtype subtype = optab_default)
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
	result[i] = vect_convert_input (vinfo, stmt_info,
					type, &unprom[i], vectype, subtype);
    }
}

/* The caller has created a (possibly empty) sequence of pattern definition
   statements followed by a single statement PATTERN_STMT.  Cast the result
   of this final statement to TYPE.  If a new statement is needed, add
   PATTERN_STMT to the end of STMT_INFO's pattern definition statements
   and return the new statement, otherwise return PATTERN_STMT as-is.
   VECITYPE is the vector form of PATTERN_STMT's result type.  */

static gimple *
vect_convert_output (vec_info *vinfo, stmt_vec_info stmt_info, tree type,
		     gimple *pattern_stmt, tree vecitype)
{
  tree lhs = gimple_get_lhs (pattern_stmt);
  if (!types_compatible_p (type, TREE_TYPE (lhs)))
    {
      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt, vecitype);
      tree cast_var = vect_recog_temp_ssa_var (type, NULL);
      pattern_stmt = gimple_build_assign (cast_var, NOP_EXPR, lhs);
    }
  return pattern_stmt;
}

/* Return true if STMT_VINFO describes a reduction for which reassociation
   is allowed.  If STMT_INFO is part of a group, assume that it's part of
   a reduction chain and optimistically assume that all statements
   except the last allow reassociation.
   Also require it to have code CODE and to be a reduction
   in the outermost loop.  When returning true, store the operands in
   *OP0_OUT and *OP1_OUT.  */

static bool
vect_reassociating_reduction_p (vec_info *vinfo,
				stmt_vec_info stmt_info, tree_code code,
				tree *op0_out, tree *op1_out)
{
  loop_vec_info loop_info = dyn_cast <loop_vec_info> (vinfo);
  if (!loop_info)
    return false;

  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (!assign || gimple_assign_rhs_code (assign) != code)
    return false;

  /* We don't allow changing the order of the computation in the inner-loop
     when doing outer-loop vectorization.  */
  class loop *loop = LOOP_VINFO_LOOP (loop_info);
  if (loop && nested_in_vect_loop_p (loop, stmt_info))
    return false;

  if (STMT_VINFO_DEF_TYPE (stmt_info) == vect_reduction_def)
    {
      if (needs_fold_left_reduction_p (TREE_TYPE (gimple_assign_lhs (assign)),
				       code))
	return false;
    }
  else if (REDUC_GROUP_FIRST_ELEMENT (stmt_info) == NULL)
    return false;

  *op0_out = gimple_assign_rhs1 (assign);
  *op1_out = gimple_assign_rhs2 (assign);
  if (commutative_tree_code (code) && STMT_VINFO_REDUC_IDX (stmt_info) == 0)
    std::swap (*op0_out, *op1_out);
  return true;
}

/* match.pd function to match
   (cond (cmp@3 a b) (convert@1 c) (convert@2 d))
   with conditions:
   1) @1, @2, c, d, a, b are all integral type.
   2) There's single_use for both @1 and @2.
   3) a, c have same precision.
   4) c and @1 have different precision.
   5) c, d are the same type or they can differ in sign when convert is
   truncation.

   record a and c and d and @3.  */

extern bool gimple_cond_expr_convert_p (tree, tree*, tree (*)(tree));

/* Function vect_recog_cond_expr_convert

   Try to find the following pattern:

   TYPE_AB A,B;
   TYPE_CD C,D;
   TYPE_E E;
   TYPE_E op_true = (TYPE_E) A;
   TYPE_E op_false = (TYPE_E) B;

   E = C cmp D ? op_true : op_false;

   where
   TYPE_PRECISION (TYPE_E) != TYPE_PRECISION (TYPE_CD);
   TYPE_PRECISION (TYPE_AB) == TYPE_PRECISION (TYPE_CD);
   single_use of op_true and op_false.
   TYPE_AB could differ in sign when (TYPE_E) A is a truncation.

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.
   here it starts with E = c cmp D ? op_true : op_false;

   Output:

   TYPE1 E' = C cmp D ? A : B;
   TYPE3 E = (TYPE3) E';

   There may extra nop_convert for A or B to handle different signness.

   * TYPE_OUT: The vector type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
   E = (TYPE3)E';
   E' = C cmp D ? A : B; is recorded in pattern definition statements;  */

static gimple *
vect_recog_cond_expr_convert_pattern (vec_info *vinfo,
				      stmt_vec_info stmt_vinfo, tree *type_out)
{
  gassign *last_stmt = dyn_cast <gassign *> (stmt_vinfo->stmt);
  tree lhs, match[4], temp, type, new_lhs, op2;
  gimple *cond_stmt;
  gimple *pattern_stmt;

  if (!last_stmt)
    return NULL;

  lhs = gimple_assign_lhs (last_stmt);

  /* Find E = C cmp D ? (TYPE3) A ? (TYPE3) B;
     TYPE_PRECISION (A) == TYPE_PRECISION (C).  */
  if (!gimple_cond_expr_convert_p (lhs, &match[0], NULL))
    return NULL;

  vect_pattern_detected ("vect_recog_cond_expr_convert_pattern", last_stmt);

  op2 = match[2];
  type = TREE_TYPE (match[1]);
  if (TYPE_SIGN (type) != TYPE_SIGN (TREE_TYPE (match[2])))
    {
      op2 = vect_recog_temp_ssa_var (type, NULL);
      gimple* nop_stmt = gimple_build_assign (op2, NOP_EXPR, match[2]);
      append_pattern_def_seq (vinfo, stmt_vinfo, nop_stmt,
			      get_vectype_for_scalar_type (vinfo, type));
    }

  temp = vect_recog_temp_ssa_var (type, NULL);
  cond_stmt = gimple_build_assign (temp, build3 (COND_EXPR, type, match[3],
						 match[1], op2));
  append_pattern_def_seq (vinfo, stmt_vinfo, cond_stmt,
			  get_vectype_for_scalar_type (vinfo, type));
  new_lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
  pattern_stmt = gimple_build_assign (new_lhs, NOP_EXPR, temp);
  *type_out = STMT_VINFO_VECTYPE (stmt_vinfo);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created pattern stmt: %G", pattern_stmt);
  return pattern_stmt;
}

/* Function vect_recog_dot_prod_pattern

   Try to find the following pattern:

     type1a x_t
     type1b y_t;
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

   where 'TYPE1' is exactly double the size of type 'type1a' and 'type1b',
   the sign of 'TYPE1' must be one of 'type1a' or 'type1b' but the sign of
   'type1a' and 'type1b' can differ.

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.  In the
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
vect_recog_dot_prod_pattern (vec_info *vinfo,
			     stmt_vec_info stmt_vinfo, tree *type_out)
{
  tree oprnd0, oprnd1;
  gimple *last_stmt = stmt_vinfo->stmt;
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
     - DX, DY, DPROD all have the same type but the sign
       between X, Y and DPROD can differ.
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

  if (!vect_reassociating_reduction_p (vinfo, stmt_vinfo, PLUS_EXPR,
				       &oprnd0, &oprnd1))
    return NULL;

  type = TREE_TYPE (gimple_get_lhs (last_stmt));

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
  enum optab_subtype subtype = optab_vector;
  if (!vect_widened_op_tree (vinfo, mult_vinfo, MULT_EXPR, WIDEN_MULT_EXPR,
			     false, 2, unprom0, &half_type, &subtype))
    return NULL;

  /* If there are two widening operations, make sure they agree on the sign
     of the extension.  The result of an optab_vector_mixed_sign operation
     is signed; otherwise, the result has the same sign as the operands.  */
  if (TYPE_PRECISION (unprom_mult.type) != TYPE_PRECISION (type)
      && (subtype == optab_vector_mixed_sign
	  ? TYPE_UNSIGNED (unprom_mult.type)
	  : TYPE_SIGN (unprom_mult.type) != TYPE_SIGN (half_type)))
    return NULL;

  vect_pattern_detected ("vect_recog_dot_prod_pattern", last_stmt);

  /* If the inputs have mixed signs, canonicalize on using the signed
     input type for analysis.  This also helps when emulating mixed-sign
     operations using signed operations.  */
  if (subtype == optab_vector_mixed_sign)
    half_type = signed_type_for (half_type);

  tree half_vectype;
  if (!vect_supportable_conv_optab_p (vinfo, type, DOT_PROD_EXPR, half_type,
					type_out, &half_vectype, subtype))
    {
      /* We can emulate a mixed-sign dot-product using a sequence of
	 signed dot-products; see vect_emulate_mixed_dot_prod for details.  */
      if (subtype != optab_vector_mixed_sign
	  || !vect_supportable_conv_optab_p (vinfo, signed_type_for (type),
					       DOT_PROD_EXPR, half_type,
					       type_out, &half_vectype,
					       optab_vector))
	return NULL;

      *type_out = signed_or_unsigned_type_for (TYPE_UNSIGNED (type),
					       *type_out);
    }

  /* Get the inputs in the appropriate types.  */
  tree mult_oprnd[2];
  vect_convert_inputs (vinfo, stmt_vinfo, 2, mult_oprnd, half_type,
		       unprom0, half_vectype, subtype);

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

   * STMT_VINFO: The stmt from which the pattern search begins.  In the
   example, when this function is called with S8, the pattern
   {S3,S4,S5,S6,S7,S8} will be detected.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        SAD_EXPR <x_t, y_t, sum_0>
  */

static gimple *
vect_recog_sad_pattern (vec_info *vinfo,
			stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
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
  if (!vect_reassociating_reduction_p (vinfo, stmt_vinfo, PLUS_EXPR,
				       &plus_oprnd0, &plus_oprnd1))
    return NULL;

  tree sum_type = TREE_TYPE (gimple_get_lhs (last_stmt));

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
  vect_unpromoted_value unprom[2];

  if (!abs_stmt)
    {
      gcall *abd_stmt = dyn_cast <gcall *> (abs_stmt_vinfo->stmt);
      if (!abd_stmt
	  || !gimple_call_internal_p (abd_stmt)
	  || gimple_call_num_args (abd_stmt) != 2)
	return NULL;

      tree abd_oprnd0 = gimple_call_arg (abd_stmt, 0);
      tree abd_oprnd1 = gimple_call_arg (abd_stmt, 1);

      if (gimple_call_internal_fn (abd_stmt) == IFN_ABD)
	{
	  if (!vect_look_through_possible_promotion (vinfo, abd_oprnd0,
						     &unprom[0])
	      || !vect_look_through_possible_promotion (vinfo, abd_oprnd1,
							&unprom[1]))
	    return NULL;
	}
      else if (gimple_call_internal_fn (abd_stmt) == IFN_VEC_WIDEN_ABD)
	{
	  unprom[0].op = abd_oprnd0;
	  unprom[0].type = TREE_TYPE (abd_oprnd0);
	  unprom[1].op = abd_oprnd1;
	  unprom[1].type = TREE_TYPE (abd_oprnd1);
	}
      else
	return NULL;

      half_type = unprom[0].type;
    }
  else if (!vect_recog_absolute_difference (vinfo, abs_stmt, &half_type,
					    unprom, NULL))
    return NULL;

  vect_pattern_detected ("vect_recog_sad_pattern", last_stmt);

  tree half_vectype;
  if (!vect_supportable_direct_optab_p (vinfo, sum_type, SAD_EXPR, half_type,
					type_out, &half_vectype))
    return NULL;

  /* Get the inputs to the SAD_EXPR in the appropriate types.  */
  tree sad_oprnd[2];
  vect_convert_inputs (vinfo, stmt_vinfo, 2, sad_oprnd, half_type,
		       unprom, half_vectype);

  tree var = vect_recog_temp_ssa_var (sum_type, NULL);
  gimple *pattern_stmt = gimple_build_assign (var, SAD_EXPR, sad_oprnd[0],
					      sad_oprnd[1], plus_oprnd1);

  return pattern_stmt;
}

/* Function vect_recog_abd_pattern

   Try to find the following ABsolute Difference (ABD) or
   widening ABD (WIDEN_ABD) pattern:

   TYPE1 x;
   TYPE2 y;
   TYPE3 x_cast = (TYPE3) x;		  // widening or no-op
   TYPE3 y_cast = (TYPE3) y;		  // widening or no-op
   TYPE3 diff = x_cast - y_cast;
   TYPE4 diff_cast = (TYPE4) diff;	  // widening or no-op
   TYPE5 abs = ABS(U)_EXPR <diff_cast>;

   WIDEN_ABD exists to optimize the case where TYPE4 is at least
   twice as wide as TYPE3.

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins

   Output:

   * TYPE_OUT: The type of the output of this pattern

   * Return value: A new stmt that will be used to replace the sequence of
     stmts that constitute the pattern, principally:
	out = IFN_ABD (x, y)
	out = IFN_WIDEN_ABD (x, y)
 */

static gimple *
vect_recog_abd_pattern (vec_info *vinfo,
			stmt_vec_info stmt_vinfo, tree *type_out)
{
  gassign *last_stmt = dyn_cast <gassign *> (STMT_VINFO_STMT (stmt_vinfo));
  if (!last_stmt)
    return NULL;

  tree out_type = TREE_TYPE (gimple_assign_lhs (last_stmt));

  vect_unpromoted_value unprom[2];
  gassign *diff_stmt = NULL;
  tree abd_in_type;
  if (!vect_recog_absolute_difference (vinfo, last_stmt, &abd_in_type,
				       unprom, &diff_stmt))
    {
      /* We cannot try further without having a non-widening MINUS.  */
      if (!diff_stmt)
	return NULL;

      unprom[0].op = gimple_assign_rhs1 (diff_stmt);
      unprom[1].op = gimple_assign_rhs2 (diff_stmt);
      abd_in_type = signed_type_for (out_type);
    }

  tree abd_out_type = abd_in_type;

  tree vectype_in = get_vectype_for_scalar_type (vinfo, abd_in_type);
  if (!vectype_in)
    return NULL;

  internal_fn ifn = IFN_ABD;
  tree vectype_out = vectype_in;

  if (TYPE_PRECISION (out_type) >= TYPE_PRECISION (abd_in_type) * 2
      && stmt_vinfo->min_output_precision >= TYPE_PRECISION (abd_in_type) * 2)
    {
      tree mid_type
	= build_nonstandard_integer_type (TYPE_PRECISION (abd_in_type) * 2,
					  TYPE_UNSIGNED (abd_in_type));
      tree mid_vectype = get_vectype_for_scalar_type (vinfo, mid_type);

      code_helper dummy_code;
      int dummy_int;
      auto_vec<tree> dummy_vec;
      if (mid_vectype
	  && supportable_widening_operation (vinfo, IFN_VEC_WIDEN_ABD,
					     stmt_vinfo, mid_vectype,
					     vectype_in,
					     &dummy_code, &dummy_code,
					     &dummy_int, &dummy_vec))
	{
	  ifn = IFN_VEC_WIDEN_ABD;
	  abd_out_type = mid_type;
	  vectype_out = mid_vectype;
	}
    }

  if (ifn == IFN_ABD
      && !direct_internal_fn_supported_p (ifn, vectype_in,
					  OPTIMIZE_FOR_SPEED))
    return NULL;

  vect_pattern_detected ("vect_recog_abd_pattern", last_stmt);

  tree abd_oprnds[2];
  vect_convert_inputs (vinfo, stmt_vinfo, 2, abd_oprnds,
		       abd_in_type, unprom, vectype_in);

  *type_out = get_vectype_for_scalar_type (vinfo, out_type);

  tree abd_result = vect_recog_temp_ssa_var (abd_out_type, NULL);
  gcall *abd_stmt = gimple_build_call_internal (ifn, 2,
						abd_oprnds[0], abd_oprnds[1]);
  gimple_call_set_lhs (abd_stmt, abd_result);
  gimple_set_location (abd_stmt, gimple_location (last_stmt));

  gimple *stmt = abd_stmt;
  if (TYPE_PRECISION (abd_in_type) == TYPE_PRECISION (abd_out_type)
      && TYPE_PRECISION (abd_out_type) < TYPE_PRECISION (out_type)
      && !TYPE_UNSIGNED (abd_out_type))
    {
      tree unsign = unsigned_type_for (abd_out_type);
      stmt = vect_convert_output (vinfo, stmt_vinfo, unsign, stmt, vectype_out);
      vectype_out = get_vectype_for_scalar_type (vinfo, unsign);
    }

  return vect_convert_output (vinfo, stmt_vinfo, out_type, stmt, vectype_out);
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
vect_recog_widen_op_pattern (vec_info *vinfo,
			     stmt_vec_info last_stmt_info, tree *type_out,
			     tree_code orig_code, code_helper wide_code,
			     bool shift_p, const char *name)
{
  gimple *last_stmt = last_stmt_info->stmt;

  vect_unpromoted_value unprom[2];
  tree half_type;
  if (!vect_widened_op_tree (vinfo, last_stmt_info, orig_code, orig_code,
			     shift_p, 2, unprom, &half_type))

    return NULL;

  /* Pattern detected.  */
  vect_pattern_detected (name, last_stmt);

  tree type = TREE_TYPE (gimple_get_lhs (last_stmt));
  tree itype = type;
  if (TYPE_PRECISION (type) != TYPE_PRECISION (half_type) * 2
      || TYPE_UNSIGNED (type) != TYPE_UNSIGNED (half_type))
    itype = build_nonstandard_integer_type (TYPE_PRECISION (half_type) * 2,
					    TYPE_UNSIGNED (half_type));

  /* Check target support  */
  tree vectype = get_vectype_for_scalar_type (vinfo, half_type);
  tree vecitype = get_vectype_for_scalar_type (vinfo, itype);
  tree ctype = itype;
  tree vecctype = vecitype;
  if (orig_code == MINUS_EXPR
      && TYPE_UNSIGNED (itype)
      && TYPE_PRECISION (type) > TYPE_PRECISION (itype))
    {
      /* Subtraction is special, even if half_type is unsigned and no matter
	 whether type is signed or unsigned, if type is wider than itype,
	 we need to sign-extend from the widening operation result to the
	 result type.
	 Consider half_type unsigned char, operand 1 0xfe, operand 2 0xff,
	 itype unsigned short and type either int or unsigned int.
	 Widened (unsigned short) 0xfe - (unsigned short) 0xff is
	 (unsigned short) 0xffff, but for type int we want the result -1
	 and for type unsigned int 0xffffffff rather than 0xffff.  */
      ctype = build_nonstandard_integer_type (TYPE_PRECISION (itype), 0);
      vecctype = get_vectype_for_scalar_type (vinfo, ctype);
    }

  code_helper dummy_code;
  int dummy_int;
  auto_vec<tree> dummy_vec;
  if (!vectype
      || !vecitype
      || !vecctype
      || !supportable_widening_operation (vinfo, wide_code, last_stmt_info,
					  vecitype, vectype,
					  &dummy_code, &dummy_code,
					  &dummy_int, &dummy_vec))
    return NULL;

  *type_out = get_vectype_for_scalar_type (vinfo, type);
  if (!*type_out)
    return NULL;

  tree oprnd[2];
  vect_convert_inputs (vinfo, last_stmt_info,
		       2, oprnd, half_type, unprom, vectype);

  tree var = vect_recog_temp_ssa_var (itype, NULL);
  gimple *pattern_stmt = vect_gimple_build (var, wide_code, oprnd[0], oprnd[1]);

  if (vecctype != vecitype)
    pattern_stmt = vect_convert_output (vinfo, last_stmt_info, ctype,
					pattern_stmt, vecitype);

  return vect_convert_output (vinfo, last_stmt_info,
			      type, pattern_stmt, vecctype);
}

/* Try to detect multiplication on widened inputs, converting MULT_EXPR
   to WIDEN_MULT_EXPR.  See vect_recog_widen_op_pattern for details.  */

static gimple *
vect_recog_widen_mult_pattern (vec_info *vinfo, stmt_vec_info last_stmt_info,
			       tree *type_out)
{
  return vect_recog_widen_op_pattern (vinfo, last_stmt_info, type_out,
				      MULT_EXPR, WIDEN_MULT_EXPR, false,
				      "vect_recog_widen_mult_pattern");
}

/* Try to detect addition on widened inputs, converting PLUS_EXPR
   to IFN_VEC_WIDEN_PLUS.  See vect_recog_widen_op_pattern for details.  */

static gimple *
vect_recog_widen_plus_pattern (vec_info *vinfo, stmt_vec_info last_stmt_info,
			       tree *type_out)
{
  return vect_recog_widen_op_pattern (vinfo, last_stmt_info, type_out,
				      PLUS_EXPR, IFN_VEC_WIDEN_PLUS,
				      false, "vect_recog_widen_plus_pattern");
}

/* Try to detect subtraction on widened inputs, converting MINUS_EXPR
   to IFN_VEC_WIDEN_MINUS.  See vect_recog_widen_op_pattern for details.  */
static gimple *
vect_recog_widen_minus_pattern (vec_info *vinfo, stmt_vec_info last_stmt_info,
			       tree *type_out)
{
  return vect_recog_widen_op_pattern (vinfo, last_stmt_info, type_out,
				      MINUS_EXPR, IFN_VEC_WIDEN_MINUS,
				      false, "vect_recog_widen_minus_pattern");
}

/* Try to detect abd on widened inputs, converting IFN_ABD
   to IFN_VEC_WIDEN_ABD.  */
static gimple *
vect_recog_widen_abd_pattern (vec_info *vinfo, stmt_vec_info stmt_vinfo,
			      tree *type_out)
{
  gassign *last_stmt = dyn_cast <gassign *> (STMT_VINFO_STMT (stmt_vinfo));
  if (!last_stmt || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (last_stmt)))
    return NULL;

  tree last_rhs = gimple_assign_rhs1 (last_stmt);

  tree in_type = TREE_TYPE (last_rhs);
  tree out_type = TREE_TYPE (gimple_assign_lhs (last_stmt));
  if (!INTEGRAL_TYPE_P (in_type)
      || !INTEGRAL_TYPE_P (out_type)
      || TYPE_PRECISION (in_type) * 2 != TYPE_PRECISION (out_type)
      || !TYPE_UNSIGNED (in_type))
    return NULL;

  vect_unpromoted_value unprom;
  tree op = vect_look_through_possible_promotion (vinfo, last_rhs, &unprom);
  if (!op || TYPE_PRECISION (TREE_TYPE (op)) != TYPE_PRECISION (in_type))
    return NULL;

  stmt_vec_info abd_pattern_vinfo = vect_get_internal_def (vinfo, op);
  if (!abd_pattern_vinfo)
    return NULL;

  gcall *abd_stmt = dyn_cast <gcall *> (STMT_VINFO_STMT (abd_pattern_vinfo));
  if (!abd_stmt
      || !gimple_call_internal_p (abd_stmt)
      || gimple_call_internal_fn (abd_stmt) != IFN_ABD)
    return NULL;

  tree vectype_in = get_vectype_for_scalar_type (vinfo, in_type);
  tree vectype_out = get_vectype_for_scalar_type (vinfo, out_type);

  code_helper dummy_code;
  int dummy_int;
  auto_vec<tree> dummy_vec;
  if (!supportable_widening_operation (vinfo, IFN_VEC_WIDEN_ABD, stmt_vinfo,
				       vectype_out, vectype_in,
				       &dummy_code, &dummy_code,
				       &dummy_int, &dummy_vec))
    return NULL;

  vect_pattern_detected ("vect_recog_widen_abd_pattern", last_stmt);

  *type_out = vectype_out;

  tree abd_oprnd0 = gimple_call_arg (abd_stmt, 0);
  tree abd_oprnd1 = gimple_call_arg (abd_stmt, 1);
  tree widen_abd_result = vect_recog_temp_ssa_var (out_type, NULL);
  gcall *widen_abd_stmt = gimple_build_call_internal (IFN_VEC_WIDEN_ABD, 2,
						      abd_oprnd0, abd_oprnd1);
  gimple_call_set_lhs (widen_abd_stmt, widen_abd_result);
  gimple_set_location (widen_abd_stmt, gimple_location (last_stmt));
  return widen_abd_stmt;
}

/* Function vect_recog_ctz_ffs_pattern

   Try to find the following pattern:

   TYPE1 A;
   TYPE1 B;

   B = __builtin_ctz{,l,ll} (A);

   or

   B = __builtin_ffs{,l,ll} (A);

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.
   here it starts with B = __builtin_* (A);

   Output:

   * TYPE_OUT: The vector type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern, using clz or popcount builtins.  */

static gimple *
vect_recog_ctz_ffs_pattern (vec_info *vinfo, stmt_vec_info stmt_vinfo,
			    tree *type_out)
{
  gimple *call_stmt = stmt_vinfo->stmt;
  gimple *pattern_stmt;
  tree rhs_oprnd, rhs_type, lhs_oprnd, lhs_type, vec_type, vec_rhs_type;
  tree new_var;
  internal_fn ifn = IFN_LAST, ifnnew = IFN_LAST;
  bool defined_at_zero = true, defined_at_zero_new = false;
  int val = 0, val_new = 0, val_cmp = 0;
  int prec;
  int sub = 0, add = 0;
  location_t loc;

  if (!is_gimple_call (call_stmt))
    return NULL;

  if (gimple_call_num_args (call_stmt) != 1
      && gimple_call_num_args (call_stmt) != 2)
    return NULL;

  rhs_oprnd = gimple_call_arg (call_stmt, 0);
  rhs_type = TREE_TYPE (rhs_oprnd);
  lhs_oprnd = gimple_call_lhs (call_stmt);
  if (!lhs_oprnd)
    return NULL;
  lhs_type = TREE_TYPE (lhs_oprnd);
  if (!INTEGRAL_TYPE_P (lhs_type)
      || !INTEGRAL_TYPE_P (rhs_type)
      || !type_has_mode_precision_p (rhs_type)
      || TREE_CODE (rhs_oprnd) != SSA_NAME)
    return NULL;

  switch (gimple_call_combined_fn (call_stmt))
    {
    CASE_CFN_CTZ:
      ifn = IFN_CTZ;
      if (!gimple_call_internal_p (call_stmt)
	  || gimple_call_num_args (call_stmt) != 2)
	defined_at_zero = false;
      else
	val = tree_to_shwi (gimple_call_arg (call_stmt, 1));
      break;
    CASE_CFN_FFS:
      ifn = IFN_FFS;
      break;
    default:
      return NULL;
    }

  prec = TYPE_PRECISION (rhs_type);
  loc = gimple_location (call_stmt);

  vec_type = get_vectype_for_scalar_type (vinfo, lhs_type);
  if (!vec_type)
    return NULL;

  vec_rhs_type = get_vectype_for_scalar_type (vinfo, rhs_type);
  if (!vec_rhs_type)
    return NULL;

  /* Do it only if the backend doesn't have ctz<vector_mode>2 or
     ffs<vector_mode>2 pattern but does have clz<vector_mode>2 or
     popcount<vector_mode>2.  */
  if (!vec_type
      || direct_internal_fn_supported_p (ifn, vec_rhs_type,
					 OPTIMIZE_FOR_SPEED))
    return NULL;

  if (ifn == IFN_FFS
      && direct_internal_fn_supported_p (IFN_CTZ, vec_rhs_type,
					 OPTIMIZE_FOR_SPEED))
    {
      ifnnew = IFN_CTZ;
      defined_at_zero_new
	= CTZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (rhs_type),
				     val_new) == 2;
    }
  else if (direct_internal_fn_supported_p (IFN_CLZ, vec_rhs_type,
					   OPTIMIZE_FOR_SPEED))
    {
      ifnnew = IFN_CLZ;
      defined_at_zero_new
	= CLZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (rhs_type),
				     val_new) == 2;
    }
  if ((ifnnew == IFN_LAST
       || (defined_at_zero && !defined_at_zero_new))
      && direct_internal_fn_supported_p (IFN_POPCOUNT, vec_rhs_type,
					 OPTIMIZE_FOR_SPEED))
    {
      ifnnew = IFN_POPCOUNT;
      defined_at_zero_new = true;
      val_new = prec;
    }
  if (ifnnew == IFN_LAST)
    return NULL;

  vect_pattern_detected ("vec_recog_ctz_ffs_pattern", call_stmt);

  val_cmp = val_new;
  if ((ifnnew == IFN_CLZ
       && defined_at_zero
       && defined_at_zero_new
       && val == prec
       && val_new == prec)
      || (ifnnew == IFN_POPCOUNT && ifn == IFN_CTZ))
    {
      /* .CTZ (X) = PREC - .CLZ ((X - 1) & ~X)
	 .CTZ (X) = .POPCOUNT ((X - 1) & ~X).  */
      if (ifnnew == IFN_CLZ)
	sub = prec;
      val_cmp = prec;

      if (!TYPE_UNSIGNED (rhs_type))
	{
	  rhs_type = unsigned_type_for (rhs_type);
	  vec_rhs_type = get_vectype_for_scalar_type (vinfo, rhs_type);
	  new_var = vect_recog_temp_ssa_var (rhs_type, NULL);
	  pattern_stmt = gimple_build_assign (new_var, NOP_EXPR, rhs_oprnd);
	  append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt,
				  vec_rhs_type);
	  rhs_oprnd = new_var;
	}

      tree m1 = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (m1, PLUS_EXPR, rhs_oprnd,
					  build_int_cst (rhs_type, -1));
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);

      new_var = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (new_var, BIT_NOT_EXPR, rhs_oprnd);
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);
      rhs_oprnd = new_var;

      new_var = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (new_var, BIT_AND_EXPR,
					  m1, rhs_oprnd);
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);
      rhs_oprnd = new_var;
    }
  else if (ifnnew == IFN_CLZ)
    {
      /* .CTZ (X) = (PREC - 1) - .CLZ (X & -X)
	 .FFS (X) = PREC - .CLZ (X & -X).  */
      sub = prec - (ifn == IFN_CTZ);
      val_cmp = sub - val_new;

      tree neg = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (neg, NEGATE_EXPR, rhs_oprnd);
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);

      new_var = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (new_var, BIT_AND_EXPR,
					  rhs_oprnd, neg);
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);
      rhs_oprnd = new_var;
    }
  else if (ifnnew == IFN_POPCOUNT)
    {
      /* .CTZ (X) = PREC - .POPCOUNT (X | -X)
	 .FFS (X) = (PREC + 1) - .POPCOUNT (X | -X).  */
      sub = prec + (ifn == IFN_FFS);
      val_cmp = sub;

      tree neg = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (neg, NEGATE_EXPR, rhs_oprnd);
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);

      new_var = vect_recog_temp_ssa_var (rhs_type, NULL);
      pattern_stmt = gimple_build_assign (new_var, BIT_IOR_EXPR,
					  rhs_oprnd, neg);
      gimple_set_location (pattern_stmt, loc);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_rhs_type);
      rhs_oprnd = new_var;
    }
  else if (ifnnew == IFN_CTZ)
    {
      /* .FFS (X) = .CTZ (X) + 1.  */
      add = 1;
      val_cmp++;
    }

  /* Create B = .IFNNEW (A).  */
  new_var = vect_recog_temp_ssa_var (lhs_type, NULL);
  if ((ifnnew == IFN_CLZ || ifnnew == IFN_CTZ) && defined_at_zero_new)
    pattern_stmt
      = gimple_build_call_internal (ifnnew, 2, rhs_oprnd,
				    build_int_cst (integer_type_node,
						   val_new));
  else
    pattern_stmt = gimple_build_call_internal (ifnnew, 1, rhs_oprnd);
  gimple_call_set_lhs (pattern_stmt, new_var);
  gimple_set_location (pattern_stmt, loc);
  *type_out = vec_type;

  if (sub)
    {
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_type);
      tree ret_var = vect_recog_temp_ssa_var (lhs_type, NULL);
      pattern_stmt = gimple_build_assign (ret_var, MINUS_EXPR,
					  build_int_cst (lhs_type, sub),
					  new_var);
      gimple_set_location (pattern_stmt, loc);
      new_var = ret_var;
    }
  else if (add)
    {
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_type);
      tree ret_var = vect_recog_temp_ssa_var (lhs_type, NULL);
      pattern_stmt = gimple_build_assign (ret_var, PLUS_EXPR, new_var,
					  build_int_cst (lhs_type, add));
      gimple_set_location (pattern_stmt, loc);
      new_var = ret_var;
    }

  if (defined_at_zero
      && (!defined_at_zero_new || val != val_cmp))
    {
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_type);
      tree ret_var = vect_recog_temp_ssa_var (lhs_type, NULL);
      rhs_oprnd = gimple_call_arg (call_stmt, 0);
      rhs_type = TREE_TYPE (rhs_oprnd);
      tree cmp = vect_recog_temp_ssa_var (boolean_type_node, NULL);
      pattern_stmt = gimple_build_assign (cmp, NE_EXPR, rhs_oprnd,
					  build_zero_cst (rhs_type));
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt,
			      truth_type_for (vec_type), rhs_type);
      pattern_stmt = gimple_build_assign (ret_var, COND_EXPR, cmp,
					  new_var,
					  build_int_cst (lhs_type, val));
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created pattern stmt: %G", pattern_stmt);

  return pattern_stmt;
}

/* Function vect_recog_popcount_clz_ctz_ffs_pattern

   Try to find the following pattern:

   UTYPE1 A;
   TYPE1 B;
   UTYPE2 temp_in;
   TYPE3 temp_out;
   temp_in = (UTYPE2)A;

   temp_out = __builtin_popcount{,l,ll} (temp_in);
   B = (TYPE1) temp_out;

   TYPE2 may or may not be equal to TYPE3.
   i.e. TYPE2 is equal to TYPE3 for __builtin_popcount
   i.e. TYPE2 is not equal to TYPE3 for __builtin_popcountll

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.
   here it starts with B = (TYPE1) temp_out;

   Output:

   * TYPE_OUT: The vector type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
   B = .POPCOUNT (A);

   Similarly for clz, ctz and ffs.
*/

static gimple *
vect_recog_popcount_clz_ctz_ffs_pattern (vec_info *vinfo,
					 stmt_vec_info stmt_vinfo,
					 tree *type_out)
{
  gassign *last_stmt = dyn_cast <gassign *> (stmt_vinfo->stmt);
  gimple *call_stmt, *pattern_stmt;
  tree rhs_oprnd, rhs_origin, lhs_oprnd, lhs_type, vec_type, new_var;
  internal_fn ifn = IFN_LAST;
  int addend = 0;

  /* Find B = (TYPE1) temp_out. */
  if (!last_stmt)
    return NULL;
  tree_code code = gimple_assign_rhs_code (last_stmt);
  if (!CONVERT_EXPR_CODE_P (code))
    return NULL;

  lhs_oprnd = gimple_assign_lhs (last_stmt);
  lhs_type = TREE_TYPE (lhs_oprnd);
  if (!INTEGRAL_TYPE_P (lhs_type))
    return NULL;

  rhs_oprnd = gimple_assign_rhs1 (last_stmt);
  if (TREE_CODE (rhs_oprnd) != SSA_NAME
      || !has_single_use (rhs_oprnd))
    return NULL;
  call_stmt = SSA_NAME_DEF_STMT (rhs_oprnd);

  /* Find temp_out = __builtin_popcount{,l,ll} (temp_in);  */
  if (!is_gimple_call (call_stmt))
    return NULL;
  switch (gimple_call_combined_fn (call_stmt))
    {
      int val;
    CASE_CFN_POPCOUNT:
      ifn = IFN_POPCOUNT;
      break;
    CASE_CFN_CLZ:
      ifn = IFN_CLZ;
      /* Punt if call result is unsigned and defined value at zero
	 is negative, as the negative value doesn't extend correctly.  */
      if (TYPE_UNSIGNED (TREE_TYPE (rhs_oprnd))
	  && gimple_call_internal_p (call_stmt)
	  && CLZ_DEFINED_VALUE_AT_ZERO
	       (SCALAR_INT_TYPE_MODE (TREE_TYPE (rhs_oprnd)), val) == 2
	  && val < 0)
	return NULL;
      break;
    CASE_CFN_CTZ:
      ifn = IFN_CTZ;
      /* Punt if call result is unsigned and defined value at zero
	 is negative, as the negative value doesn't extend correctly.  */
      if (TYPE_UNSIGNED (TREE_TYPE (rhs_oprnd))
	  && gimple_call_internal_p (call_stmt)
	  && CTZ_DEFINED_VALUE_AT_ZERO
	       (SCALAR_INT_TYPE_MODE (TREE_TYPE (rhs_oprnd)), val) == 2
	  && val < 0)
	return NULL;
      break;
    CASE_CFN_FFS:
      ifn = IFN_FFS;
      break;
    default:
      return NULL;
    }

  if (gimple_call_num_args (call_stmt) != 1
      && gimple_call_num_args (call_stmt) != 2)
    return NULL;

  rhs_oprnd = gimple_call_arg (call_stmt, 0);
  vect_unpromoted_value unprom_diff;
  rhs_origin
    = vect_look_through_possible_promotion (vinfo, rhs_oprnd, &unprom_diff);

  if (!rhs_origin)
    return NULL;

  /* Input and output of .POPCOUNT should be same-precision integer.  */
  if (TYPE_PRECISION (unprom_diff.type) != TYPE_PRECISION (lhs_type))
    return NULL;

  /* Also A should be unsigned or same precision as temp_in, otherwise
     different builtins/internal functions have different behaviors.  */
  if (TYPE_PRECISION (unprom_diff.type)
      != TYPE_PRECISION (TREE_TYPE (rhs_oprnd)))
    switch (ifn)
      {
      case IFN_POPCOUNT:
	/* For popcount require zero extension, which doesn't add any
	   further bits to the count.  */
	if (!TYPE_UNSIGNED (unprom_diff.type))
	  return NULL;
	break;
      case IFN_CLZ:
	/* clzll (x) == clz (x) + 32 for unsigned x != 0, so ok
	   if it is undefined at zero or if it matches also for the
	   defined value there.  */
	if (!TYPE_UNSIGNED (unprom_diff.type))
	  return NULL;
	if (!type_has_mode_precision_p (lhs_type)
	    || !type_has_mode_precision_p (TREE_TYPE (rhs_oprnd)))
	  return NULL;
	addend = (TYPE_PRECISION (TREE_TYPE (rhs_oprnd))
		  - TYPE_PRECISION (lhs_type));
	if (gimple_call_internal_p (call_stmt)
	    && gimple_call_num_args (call_stmt) == 2)
	  {
	    int val1, val2;
	    val1 = tree_to_shwi (gimple_call_arg (call_stmt, 1));
	    int d2
	      = CLZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (lhs_type),
					   val2);
	    if (d2 != 2 || val1 != val2 + addend)
	      return NULL;
	  }
	break;
      case IFN_CTZ:
	/* ctzll (x) == ctz (x) for unsigned or signed x != 0, so ok
	   if it is undefined at zero or if it matches also for the
	   defined value there.  */
	if (gimple_call_internal_p (call_stmt)
	    && gimple_call_num_args (call_stmt) == 2)
	  {
	    int val1, val2;
	    val1 = tree_to_shwi (gimple_call_arg (call_stmt, 1));
	    int d2
	      = CTZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (lhs_type),
					   val2);
	    if (d2 != 2 || val1 != val2)
	      return NULL;
	  }
	break;
      case IFN_FFS:
	/* ffsll (x) == ffs (x) for unsigned or signed x.  */
	break;
      default:
	gcc_unreachable ();
      }

  vec_type = get_vectype_for_scalar_type (vinfo, lhs_type);
  /* Do it only if the backend has popcount<vector_mode>2 etc. pattern.  */
  if (!vec_type)
    return NULL;

  bool supported
    = direct_internal_fn_supported_p (ifn, vec_type, OPTIMIZE_FOR_SPEED);
  if (!supported)
    switch (ifn)
      {
      case IFN_POPCOUNT:
      case IFN_CLZ:
	return NULL;
      case IFN_FFS:
	/* vect_recog_ctz_ffs_pattern can implement ffs using ctz.  */
	if (direct_internal_fn_supported_p (IFN_CTZ, vec_type,
					    OPTIMIZE_FOR_SPEED))
	  break;
	/* FALLTHRU */
      case IFN_CTZ:
	/* vect_recog_ctz_ffs_pattern can implement ffs or ctz using
	   clz or popcount.  */
	if (direct_internal_fn_supported_p (IFN_CLZ, vec_type,
					    OPTIMIZE_FOR_SPEED))
	  break;
	if (direct_internal_fn_supported_p (IFN_POPCOUNT, vec_type,
					    OPTIMIZE_FOR_SPEED))
	  break;
	return NULL;
      default:
	gcc_unreachable ();
      }

  vect_pattern_detected ("vec_recog_popcount_clz_ctz_ffs_pattern",
			 call_stmt);

  /* Create B = .POPCOUNT (A).  */
  new_var = vect_recog_temp_ssa_var (lhs_type, NULL);
  tree arg2 = NULL_TREE;
  int val;
  if (ifn == IFN_CLZ
      && CLZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (lhs_type),
				    val) == 2)
    arg2 = build_int_cst (integer_type_node, val);
  else if (ifn == IFN_CTZ
	   && CTZ_DEFINED_VALUE_AT_ZERO (SCALAR_INT_TYPE_MODE (lhs_type),
					 val) == 2)
    arg2 = build_int_cst (integer_type_node, val);
  if (arg2)
    pattern_stmt = gimple_build_call_internal (ifn, 2, unprom_diff.op, arg2);
  else
    pattern_stmt = gimple_build_call_internal (ifn, 1, unprom_diff.op);
  gimple_call_set_lhs (pattern_stmt, new_var);
  gimple_set_location (pattern_stmt, gimple_location (last_stmt));
  *type_out = vec_type;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created pattern stmt: %G", pattern_stmt);

  if (addend)
    {
      gcc_assert (supported);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, vec_type);
      tree ret_var = vect_recog_temp_ssa_var (lhs_type, NULL);
      pattern_stmt = gimple_build_assign (ret_var, PLUS_EXPR, new_var,
					  build_int_cst (lhs_type, addend));
    }
  else if (!supported)
    {
      stmt_vec_info new_stmt_info = vinfo->add_stmt (pattern_stmt);
      STMT_VINFO_VECTYPE (new_stmt_info) = vec_type;
      pattern_stmt
	= vect_recog_ctz_ffs_pattern (vinfo, new_stmt_info, type_out);
      if (pattern_stmt == NULL)
	return NULL;
      if (gimple_seq seq = STMT_VINFO_PATTERN_DEF_SEQ (new_stmt_info))
	{
	  gimple_seq *pseq = &STMT_VINFO_PATTERN_DEF_SEQ (stmt_vinfo);
	  gimple_seq_add_seq_without_update (pseq, seq);
	}
    }
  return pattern_stmt;
}

/* Function vect_recog_pow_pattern

   Try to find the following pattern:

     x = POW (y, N);

   with POW being one of pow, powf, powi, powif and N being
   either 2 or 0.5.

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
        x = x * x
   or
	x = sqrt (x)
*/

static gimple *
vect_recog_pow_pattern (vec_info *vinfo,
			stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
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
	  && gimple_call_builtin_p (last_stmt, BUILT_IN_NORMAL))
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
	      *type_out = get_vectype_for_scalar_type (vinfo, TREE_TYPE (base));
	      if (!*type_out)
		return NULL;
	      tree def = vect_recog_temp_ssa_var (TREE_TYPE (base), NULL);
	      gimple *g = gimple_build_assign (def, MULT_EXPR, exp, logc);
	      append_pattern_def_seq (vinfo, stmt_vinfo, g);
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
      if (!vect_supportable_direct_optab_p (vinfo, TREE_TYPE (base), MULT_EXPR,
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
      *type_out = get_vectype_for_scalar_type (vinfo, TREE_TYPE (base));
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

   * STMT_VINFO: The stmt from which the pattern search begins. In the example,
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
vect_recog_widen_sum_pattern (vec_info *vinfo,
			      stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  tree oprnd0, oprnd1;
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

  if (!vect_reassociating_reduction_p (vinfo, stmt_vinfo, PLUS_EXPR,
				       &oprnd0, &oprnd1)
      || TREE_CODE (oprnd0) != SSA_NAME
      || !vinfo->lookup_def (oprnd0))
    return NULL;

  type = TREE_TYPE (gimple_get_lhs (last_stmt));

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

  if (!vect_supportable_direct_optab_p (vinfo, type, WIDEN_SUM_EXPR,
					unprom0.type, type_out))
    return NULL;

  var = vect_recog_temp_ssa_var (type, NULL);
  pattern_stmt = gimple_build_assign (var, WIDEN_SUM_EXPR, unprom0.op, oprnd1);

  return pattern_stmt;
}

/* Function vect_recog_bitfield_ref_pattern

   Try to find the following pattern:

   bf_value = BIT_FIELD_REF (container, bitsize, bitpos);
   result = (type_out) bf_value;

   or

   if (BIT_FIELD_REF (container, bitsize, bitpos) `cmp` <constant>)

   where type_out is a non-bitfield type, that is to say, it's precision matches
   2^(TYPE_SIZE(type_out) - (TYPE_UNSIGNED (type_out) ? 1 : 2)).

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.
   here it starts with:
   result = (type_out) bf_value;

   or

   if (BIT_FIELD_REF (container, bitsize, bitpos) `cmp` <constant>)

   Output:

   * TYPE_OUT: The vector type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. If the precision of type_out is bigger
   than the precision type of _1 we perform the widening before the shifting,
   since the new precision will be large enough to shift the value and moving
   widening operations up the statement chain enables the generation of
   widening loads.  If we are widening and the operation after the pattern is
   an addition then we mask first and shift later, to enable the generation of
   shifting adds.  In the case of narrowing we will always mask first, shift
   last and then perform a narrowing operation.  This will enable the
   generation of narrowing shifts.

   Widening with mask first, shift later:
   container = (type_out) container;
   masked = container & (((1 << bitsize) - 1) << bitpos);
   result = masked >> bitpos;

   Widening with shift first, mask last:
   container = (type_out) container;
   shifted = container >> bitpos;
   result = shifted & ((1 << bitsize) - 1);

   Narrowing:
   masked = container & (((1 << bitsize) - 1) << bitpos);
   result = masked >> bitpos;
   result = (type_out) result;

   If the bitfield is signed and it's wider than type_out, we need to
   keep the result sign-extended:
   container = (type) container;
   masked = container << (prec - bitsize - bitpos);
   result = (type_out) (masked >> (prec - bitsize));

   Here type is the signed variant of the wider of type_out and the type
   of container.

   The shifting is always optional depending on whether bitpos != 0.

   When the original bitfield was inside a gcond then an new gcond is also
   generated with the newly `result` as the operand to the comparison.

*/

static gimple *
vect_recog_bitfield_ref_pattern (vec_info *vinfo, stmt_vec_info stmt_info,
				 tree *type_out)
{
  gimple *bf_stmt = NULL;
  tree lhs = NULL_TREE;
  tree ret_type = NULL_TREE;
  gimple *stmt = STMT_VINFO_STMT (stmt_info);
  if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
    {
      tree op = gimple_cond_lhs (cond_stmt);
      if (TREE_CODE (op) != SSA_NAME)
	return NULL;
      bf_stmt = dyn_cast <gassign *> (SSA_NAME_DEF_STMT (op));
      if (TREE_CODE (gimple_cond_rhs (cond_stmt)) != INTEGER_CST)
	return NULL;
    }
  else if (is_gimple_assign (stmt)
	   && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt))
	   && TREE_CODE (gimple_assign_rhs1 (stmt)) == SSA_NAME)
    {
      gimple *second_stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
      bf_stmt = dyn_cast <gassign *> (second_stmt);
      lhs = gimple_assign_lhs (stmt);
      ret_type = TREE_TYPE (lhs);
    }

  if (!bf_stmt
      || gimple_assign_rhs_code (bf_stmt) != BIT_FIELD_REF)
    return NULL;

  tree bf_ref = gimple_assign_rhs1 (bf_stmt);
  tree container = TREE_OPERAND (bf_ref, 0);
  ret_type = ret_type ? ret_type : TREE_TYPE (container);

  if (!bit_field_offset (bf_ref).is_constant ()
      || !bit_field_size (bf_ref).is_constant ()
      || !tree_fits_uhwi_p (TYPE_SIZE (TREE_TYPE (container))))
    return NULL;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (bf_ref))
      || !INTEGRAL_TYPE_P (TREE_TYPE (container))
      || TYPE_MODE (TREE_TYPE (container)) == E_BLKmode)
    return NULL;

  gimple *use_stmt, *pattern_stmt;
  use_operand_p use_p;
  bool shift_first = true;
  tree container_type = TREE_TYPE (container);
  tree vectype = get_vectype_for_scalar_type (vinfo, container_type);

  /* Calculate shift_n before the adjustments for widening loads, otherwise
     the container may change and we have to consider offset change for
     widening loads on big endianness.  The shift_n calculated here can be
     independent of widening.  */
  unsigned HOST_WIDE_INT shift_n = bit_field_offset (bf_ref).to_constant ();
  unsigned HOST_WIDE_INT mask_width = bit_field_size (bf_ref).to_constant ();
  unsigned HOST_WIDE_INT prec = tree_to_uhwi (TYPE_SIZE (container_type));
  if (BYTES_BIG_ENDIAN)
    shift_n = prec - shift_n - mask_width;

  bool ref_sext = (!TYPE_UNSIGNED (TREE_TYPE (bf_ref)) &&
		   TYPE_PRECISION (ret_type) > mask_width);
  bool load_widen = (TYPE_PRECISION (TREE_TYPE (container)) <
		     TYPE_PRECISION (ret_type));

  /* We move the conversion earlier if the loaded type is smaller than the
     return type to enable the use of widening loads.  And if we need a
     sign extension, we need to convert the loaded value early to a signed
     type as well.  */
  if (ref_sext || load_widen)
    {
      tree type = load_widen ? ret_type : container_type;
      if (ref_sext)
	type = gimple_signed_type (type);
      pattern_stmt = gimple_build_assign (vect_recog_temp_ssa_var (type),
					  NOP_EXPR, container);
      container = gimple_get_lhs (pattern_stmt);
      container_type = TREE_TYPE (container);
      prec = tree_to_uhwi (TYPE_SIZE (container_type));
      vectype = get_vectype_for_scalar_type (vinfo, container_type);
      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt, vectype);
    }
  else if (!useless_type_conversion_p (TREE_TYPE (container), ret_type))
    /* If we are doing the conversion last then also delay the shift as we may
       be able to combine the shift and conversion in certain cases.  */
    shift_first = false;

  /* If the only use of the result of this BIT_FIELD_REF + CONVERT is a
     PLUS_EXPR then do the shift last as some targets can combine the shift and
     add into a single instruction.  */
  if (lhs && !is_pattern_stmt_p (stmt_info)
      && single_imm_use (lhs, &use_p, &use_stmt))
    {
      if (gimple_code (use_stmt) == GIMPLE_ASSIGN
	  && gimple_assign_rhs_code (use_stmt) == PLUS_EXPR)
	shift_first = false;
    }

  /* If we don't have to shift we only generate the mask, so just fix the
     code-path to shift_first.  */
  if (shift_n == 0)
    shift_first = true;

  tree result;
  if (shift_first && !ref_sext)
    {
      tree shifted = container;
      if (shift_n)
	{
	  pattern_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (container_type),
				   RSHIFT_EXPR, container,
				   build_int_cst (sizetype, shift_n));
	  shifted = gimple_assign_lhs (pattern_stmt);
	  append_pattern_def_seq (vinfo, stmt_info, pattern_stmt, vectype);
	}

      tree mask = wide_int_to_tree (container_type,
				    wi::mask (mask_width, false, prec));

      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (container_type),
			       BIT_AND_EXPR, shifted, mask);
      result = gimple_assign_lhs (pattern_stmt);
    }
  else
    {
      tree temp = vect_recog_temp_ssa_var (container_type);
      if (!ref_sext)
	{
	  tree mask = wide_int_to_tree (container_type,
					wi::shifted_mask (shift_n,
							  mask_width,
							  false, prec));
	  pattern_stmt = gimple_build_assign (temp, BIT_AND_EXPR,
					      container, mask);
	}
      else
	{
	  HOST_WIDE_INT shl = prec - shift_n - mask_width;
	  shift_n += shl;
	  pattern_stmt = gimple_build_assign (temp, LSHIFT_EXPR,
					      container,
					      build_int_cst (sizetype,
							     shl));
	}

      tree masked = gimple_assign_lhs (pattern_stmt);
      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt, vectype);
      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (container_type),
			       RSHIFT_EXPR, masked,
			       build_int_cst (sizetype, shift_n));
      result = gimple_assign_lhs (pattern_stmt);
    }

  if (!useless_type_conversion_p (TREE_TYPE (result), ret_type))
    {
      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt, vectype);
      pattern_stmt
	= gimple_build_assign (vect_recog_temp_ssa_var (ret_type),
			       NOP_EXPR, result);
    }

  if (!lhs)
    {
      if (!vectype)
	return NULL;

      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt, vectype);
      vectype = truth_type_for (vectype);

      /* FIXME: This part extracts the boolean value out of the bitfield in the
		same way as vect_recog_gcond_pattern does.  However because
		patterns cannot match the same root twice,  when we handle and
		lower the bitfield in the gcond, vect_recog_gcond_pattern can't
		apply anymore.  We should really fix it so that we don't need to
		duplicate transformations like these.  */
      tree new_lhs = vect_recog_temp_ssa_var (boolean_type_node, NULL);
      gcond *cond_stmt = dyn_cast <gcond *> (stmt_info->stmt);
      tree cond_cst = gimple_cond_rhs (cond_stmt);
      gimple *new_stmt
	= gimple_build_assign (new_lhs, gimple_cond_code (cond_stmt),
			       gimple_get_lhs (pattern_stmt),
			       fold_convert (container_type, cond_cst));
      append_pattern_def_seq (vinfo, stmt_info, new_stmt, vectype, container_type);
      pattern_stmt
	= gimple_build_cond (NE_EXPR, new_lhs,
			     build_zero_cst (TREE_TYPE (new_lhs)),
			     NULL_TREE, NULL_TREE);
    }

  *type_out = STMT_VINFO_VECTYPE (stmt_info);
  vect_pattern_detected ("bitfield_ref pattern", stmt_info->stmt);

  return pattern_stmt;
}

/* Function vect_recog_bit_insert_pattern

   Try to find the following pattern:

   written = BIT_INSERT_EXPR (container, value, bitpos);

   Input:

   * STMT_VINFO: The stmt we want to replace.

   Output:

   * TYPE_OUT: The vector type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the sequence of
   stmts that constitute the pattern. In this case it will be:
   value = (container_type) value;	    // Make sure
   shifted = value << bitpos;		    // Shift value into place
   masked = shifted & (mask << bitpos);	    // Mask off the non-relevant bits in
					    // the 'to-write value'.
   cleared = container & ~(mask << bitpos); // Clearing the bits we want to
					    // write to from the value we want
					    // to write to.
   written = cleared | masked;		    // Write bits.


   where mask = ((1 << TYPE_PRECISION (value)) - 1), a mask to keep the number of
   bits corresponding to the real size of the bitfield value we are writing to.
   The shifting is always optional depending on whether bitpos != 0.

*/

static gimple *
vect_recog_bit_insert_pattern (vec_info *vinfo, stmt_vec_info stmt_info,
			       tree *type_out)
{
  gassign *bf_stmt = dyn_cast <gassign *> (stmt_info->stmt);
  if (!bf_stmt || gimple_assign_rhs_code (bf_stmt) != BIT_INSERT_EXPR)
    return NULL;

  tree container = gimple_assign_rhs1 (bf_stmt);
  tree value = gimple_assign_rhs2 (bf_stmt);
  tree shift = gimple_assign_rhs3 (bf_stmt);

  tree bf_type = TREE_TYPE (value);
  tree container_type = TREE_TYPE (container);

  if (!INTEGRAL_TYPE_P (container_type)
      || !tree_fits_uhwi_p (TYPE_SIZE (container_type)))
    return NULL;

  gimple *pattern_stmt;

  vect_unpromoted_value unprom;
  unprom.set_op (value, vect_internal_def);
  value = vect_convert_input (vinfo, stmt_info, container_type, &unprom,
			      get_vectype_for_scalar_type (vinfo,
							   container_type));

  unsigned HOST_WIDE_INT mask_width = TYPE_PRECISION (bf_type);
  unsigned HOST_WIDE_INT prec = tree_to_uhwi (TYPE_SIZE (container_type));
  unsigned HOST_WIDE_INT shift_n = tree_to_uhwi (shift);
  if (BYTES_BIG_ENDIAN)
    {
      shift_n = prec - shift_n - mask_width;
      shift = build_int_cst (TREE_TYPE (shift), shift_n);
    }

  if (!useless_type_conversion_p (TREE_TYPE (value), container_type))
    {
      pattern_stmt =
	gimple_build_assign (vect_recog_temp_ssa_var (container_type),
			     NOP_EXPR, value);
      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt);
      value = gimple_get_lhs (pattern_stmt);
    }

  /* Shift VALUE into place.  */
  tree shifted = value;
  if (shift_n)
    {
      gimple_seq stmts = NULL;
      shifted
	= gimple_build (&stmts, LSHIFT_EXPR, container_type, value, shift);
      if (!gimple_seq_empty_p (stmts))
	append_pattern_def_seq (vinfo, stmt_info,
				gimple_seq_first_stmt (stmts));
    }

  tree mask_t
    = wide_int_to_tree (container_type,
			wi::shifted_mask (shift_n, mask_width, false, prec));

  /* Clear bits we don't want to write back from SHIFTED.  */
  gimple_seq stmts = NULL;
  tree masked = gimple_build (&stmts, BIT_AND_EXPR, container_type, shifted,
			      mask_t);
  if (!gimple_seq_empty_p (stmts))
    {
      pattern_stmt = gimple_seq_first_stmt (stmts);
      append_pattern_def_seq (vinfo, stmt_info, pattern_stmt);
    }

  /* Mask off the bits in the container that we are to write to.  */
  mask_t = wide_int_to_tree (container_type,
			     wi::shifted_mask (shift_n, mask_width, true, prec));
  tree cleared = vect_recog_temp_ssa_var (container_type);
  pattern_stmt = gimple_build_assign (cleared, BIT_AND_EXPR, container, mask_t);
  append_pattern_def_seq (vinfo, stmt_info, pattern_stmt);

  /* Write MASKED into CLEARED.  */
  pattern_stmt
    = gimple_build_assign (vect_recog_temp_ssa_var (container_type),
			   BIT_IOR_EXPR, cleared, masked);

  *type_out = STMT_VINFO_VECTYPE (stmt_info);
  vect_pattern_detected ("bit_insert pattern", stmt_info->stmt);

  return pattern_stmt;
}


/* Recognize cases in which an operation is performed in one type WTYPE
   but could be done more efficiently in a narrower type NTYPE.  For example,
   if we have:

     ATYPE a;  // narrower than NTYPE
     BTYPE b;  // narrower than NTYPE
     WTYPE aw = (WTYPE) a;
     WTYPE bw = (WTYPE) b;
     WTYPE res = aw + bw;  // only uses of aw and bw

   then it would be more efficient to do:

     NTYPE an = (NTYPE) a;
     NTYPE bn = (NTYPE) b;
     NTYPE resn = an + bn;
     WTYPE res = (WTYPE) resn;

   Other situations include things like:

     ATYPE a;  // NTYPE or narrower
     WTYPE aw = (WTYPE) a;
     WTYPE res = aw + b;

   when only "(NTYPE) res" is significant.  In that case it's more efficient
   to truncate "b" and do the operation on NTYPE instead:

     NTYPE an = (NTYPE) a;
     NTYPE bn = (NTYPE) b;  // truncation
     NTYPE resn = an + bn;
     WTYPE res = (WTYPE) resn;

   All users of "res" should then use "resn" instead, making the final
   statement dead (not marked as relevant).  The final statement is still
   needed to maintain the type correctness of the IR.

   vect_determine_precisions has already determined the minimum
   precison of the operation and the minimum precision required
   by users of the result.  */

static gimple *
vect_recog_over_widening_pattern (vec_info *vinfo,
				  stmt_vec_info last_stmt_info, tree *type_out)
{
  gassign *last_stmt = dyn_cast <gassign *> (last_stmt_info->stmt);
  if (!last_stmt)
    return NULL;

  /* See whether we have found that this operation can be done on a
     narrower type without changing its semantics.  */
  unsigned int new_precision = last_stmt_info->operation_precision;
  if (!new_precision)
    return NULL;

  tree lhs = gimple_assign_lhs (last_stmt);
  tree type = TREE_TYPE (lhs);
  tree_code code = gimple_assign_rhs_code (last_stmt);

  /* Punt for reductions where we don't handle the type conversions.  */
  if (STMT_VINFO_DEF_TYPE (last_stmt_info) == vect_reduction_def)
    return NULL;

  /* Keep the first operand of a COND_EXPR as-is: only the other two
     operands are interesting.  */
  unsigned int first_op = (code == COND_EXPR ? 2 : 1);

  /* Check the operands.  */
  unsigned int nops = gimple_num_ops (last_stmt) - first_op;
  auto_vec <vect_unpromoted_value, 3> unprom (nops);
  unprom.quick_grow_cleared (nops);
  unsigned int min_precision = 0;
  bool single_use_p = false;
  for (unsigned int i = 0; i < nops; ++i)
    {
      tree op = gimple_op (last_stmt, first_op + i);
      if (TREE_CODE (op) == INTEGER_CST)
	unprom[i].set_op (op, vect_constant_def);
      else if (TREE_CODE (op) == SSA_NAME)
	{
	  bool op_single_use_p = true;
	  if (!vect_look_through_possible_promotion (vinfo, op, &unprom[i],
						     &op_single_use_p))
	    return NULL;
	  /* If:

	     (1) N bits of the result are needed;
	     (2) all inputs are widened from M<N bits; and
	     (3) one operand OP is a single-use SSA name

	     we can shift the M->N widening from OP to the output
	     without changing the number or type of extensions involved.
	     This then reduces the number of copies of STMT_INFO.

	     If instead of (3) more than one operand is a single-use SSA name,
	     shifting the extension to the output is even more of a win.

	     If instead:

	     (1) N bits of the result are needed;
	     (2) one operand OP2 is widened from M2<N bits;
	     (3) another operand OP1 is widened from M1<M2 bits; and
	     (4) both OP1 and OP2 are single-use

	     the choice is between:

	     (a) truncating OP2 to M1, doing the operation on M1,
		 and then widening the result to N

	     (b) widening OP1 to M2, doing the operation on M2, and then
		 widening the result to N

	     Both shift the M2->N widening of the inputs to the output.
	     (a) additionally shifts the M1->M2 widening to the output;
	     it requires fewer copies of STMT_INFO but requires an extra
	     M2->M1 truncation.

	     Which is better will depend on the complexity and cost of
	     STMT_INFO, which is hard to predict at this stage.  However,
	     a clear tie-breaker in favor of (b) is the fact that the
	     truncation in (a) increases the length of the operation chain.

	     If instead of (4) only one of OP1 or OP2 is single-use,
	     (b) is still a win over doing the operation in N bits:
	     it still shifts the M2->N widening on the single-use operand
	     to the output and reduces the number of STMT_INFO copies.

	     If neither operand is single-use then operating on fewer than
	     N bits might lead to more extensions overall.  Whether it does
	     or not depends on global information about the vectorization
	     region, and whether that's a good trade-off would again
	     depend on the complexity and cost of the statements involved,
	     as well as things like register pressure that are not normally
	     modelled at this stage.  We therefore ignore these cases
	     and just optimize the clear single-use wins above.

	     Thus we take the maximum precision of the unpromoted operands
	     and record whether any operand is single-use.  */
	  if (unprom[i].dt == vect_internal_def)
	    {
	      min_precision = MAX (min_precision,
				   TYPE_PRECISION (unprom[i].type));
	      single_use_p |= op_single_use_p;
	    }
	}
      else
	return NULL;
    }

  /* Although the operation could be done in operation_precision, we have
     to balance that against introducing extra truncations or extensions.
     Calculate the minimum precision that can be handled efficiently.

     The loop above determined that the operation could be handled
     efficiently in MIN_PRECISION if SINGLE_USE_P; this would shift an
     extension from the inputs to the output without introducing more
     instructions, and would reduce the number of instructions required
     for STMT_INFO itself.

     vect_determine_precisions has also determined that the result only
     needs min_output_precision bits.  Truncating by a factor of N times
     requires a tree of N - 1 instructions, so if TYPE is N times wider
     than min_output_precision, doing the operation in TYPE and truncating
     the result requires N + (N - 1) = 2N - 1 instructions per output vector.
     In contrast:

     - truncating the input to a unary operation and doing the operation
       in the new type requires at most N - 1 + 1 = N instructions per
       output vector

     - doing the same for a binary operation requires at most
       (N - 1) * 2 + 1 = 2N - 1 instructions per output vector

     Both unary and binary operations require fewer instructions than
     this if the operands were extended from a suitable truncated form.
     Thus there is usually nothing to lose by doing operations in
     min_output_precision bits, but there can be something to gain.  */
  if (!single_use_p)
    min_precision = last_stmt_info->min_output_precision;
  else
    min_precision = MIN (min_precision, last_stmt_info->min_output_precision);

  /* Apply the minimum efficient precision we just calculated.  */
  if (new_precision < min_precision)
    new_precision = min_precision;
  new_precision = vect_element_precision (new_precision);
  if (new_precision >= TYPE_PRECISION (type))
    return NULL;

  vect_pattern_detected ("vect_recog_over_widening_pattern", last_stmt);

  *type_out = get_vectype_for_scalar_type (vinfo, type);
  if (!*type_out)
    return NULL;

  /* We've found a viable pattern.  Get the new type of the operation.  */
  bool unsigned_p = (last_stmt_info->operation_sign == UNSIGNED);
  tree new_type = build_nonstandard_integer_type (new_precision, unsigned_p);

  /* If we're truncating an operation, we need to make sure that we
     don't introduce new undefined overflow.  The codes tested here are
     a subset of those accepted by vect_truncatable_operation_p.  */
  tree op_type = new_type;
  if (TYPE_OVERFLOW_UNDEFINED (new_type)
      && (code == PLUS_EXPR || code == MINUS_EXPR || code == MULT_EXPR))
    op_type = build_nonstandard_integer_type (new_precision, true);

  /* We specifically don't check here whether the target supports the
     new operation, since it might be something that a later pattern
     wants to rewrite anyway.  If targets have a minimum element size
     for some optabs, we should pattern-match smaller ops to larger ops
     where beneficial.  */
  tree new_vectype = get_vectype_for_scalar_type (vinfo, new_type);
  tree op_vectype = get_vectype_for_scalar_type (vinfo, op_type);
  if (!new_vectype || !op_vectype)
    return NULL;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "demoting %T to %T\n",
		     type, new_type);

  /* Calculate the rhs operands for an operation on OP_TYPE.  */
  tree ops[3] = {};
  for (unsigned int i = 1; i < first_op; ++i)
    ops[i - 1] = gimple_op (last_stmt, i);
  vect_convert_inputs (vinfo, last_stmt_info, nops, &ops[first_op - 1],
		       op_type, &unprom[0], op_vectype);

  /* Use the operation to produce a result of type OP_TYPE.  */
  tree new_var = vect_recog_temp_ssa_var (op_type, NULL);
  gimple *pattern_stmt = gimple_build_assign (new_var, code,
					      ops[0], ops[1], ops[2]);
  gimple_set_location (pattern_stmt, gimple_location (last_stmt));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created pattern stmt: %G", pattern_stmt);

  /* Convert back to the original signedness, if OP_TYPE is different
     from NEW_TYPE.  */
  if (op_type != new_type)
    pattern_stmt = vect_convert_output (vinfo, last_stmt_info, new_type,
					pattern_stmt, op_vectype);

  /* Promote the result to the original type.  */
  pattern_stmt = vect_convert_output (vinfo, last_stmt_info, type,
				      pattern_stmt, new_vectype);

  return pattern_stmt;
}

/* Recognize the following patterns:

     ATYPE a;  // narrower than TYPE
     BTYPE b;  // narrower than TYPE

   1) Multiply high with scaling
     TYPE res = ((TYPE) a * (TYPE) b) >> c;
     Here, c is bitsize (TYPE) / 2 - 1.

   2) ... or also with rounding
     TYPE res = (((TYPE) a * (TYPE) b) >> d + 1) >> 1;
     Here, d is bitsize (TYPE) / 2 - 2.

   3) Normal multiply high
     TYPE res = ((TYPE) a * (TYPE) b) >> e;
     Here, e is bitsize (TYPE) / 2.

   where only the bottom half of res is used.  */

static gimple *
vect_recog_mulhs_pattern (vec_info *vinfo,
			  stmt_vec_info last_stmt_info, tree *type_out)
{
  /* Check for a right shift.  */
  gassign *last_stmt = dyn_cast <gassign *> (last_stmt_info->stmt);
  if (!last_stmt
      || gimple_assign_rhs_code (last_stmt) != RSHIFT_EXPR)
    return NULL;

  /* Check that the shift result is wider than the users of the
     result need (i.e. that narrowing would be a natural choice).  */
  tree lhs_type = TREE_TYPE (gimple_assign_lhs (last_stmt));
  unsigned int target_precision
    = vect_element_precision (last_stmt_info->min_output_precision);
  if (!INTEGRAL_TYPE_P (lhs_type)
      || target_precision >= TYPE_PRECISION (lhs_type))
    return NULL;

  /* Look through any change in sign on the outer shift input.  */
  vect_unpromoted_value unprom_rshift_input;
  tree rshift_input = vect_look_through_possible_promotion
    (vinfo, gimple_assign_rhs1 (last_stmt), &unprom_rshift_input);
  if (!rshift_input
      || TYPE_PRECISION (TREE_TYPE (rshift_input))
	   != TYPE_PRECISION (lhs_type))
    return NULL;

  /* Get the definition of the shift input.  */
  stmt_vec_info rshift_input_stmt_info
    = vect_get_internal_def (vinfo, rshift_input);
  if (!rshift_input_stmt_info)
    return NULL;
  gassign *rshift_input_stmt
    = dyn_cast <gassign *> (rshift_input_stmt_info->stmt);
  if (!rshift_input_stmt)
    return NULL;

  stmt_vec_info mulh_stmt_info;
  tree scale_term;
  bool rounding_p = false;

  /* Check for the presence of the rounding term.  */
  if (gimple_assign_rhs_code (rshift_input_stmt) == PLUS_EXPR)
    {
      /* Check that the outer shift was by 1.  */
      if (!integer_onep (gimple_assign_rhs2 (last_stmt)))
	return NULL;

      /* Check that the second operand of the PLUS_EXPR is 1.  */
      if (!integer_onep (gimple_assign_rhs2 (rshift_input_stmt)))
	return NULL;

      /* Look through any change in sign on the addition input.  */
      vect_unpromoted_value unprom_plus_input;
      tree plus_input = vect_look_through_possible_promotion
	(vinfo, gimple_assign_rhs1 (rshift_input_stmt), &unprom_plus_input);
      if (!plus_input
	   || TYPE_PRECISION (TREE_TYPE (plus_input))
		!= TYPE_PRECISION (TREE_TYPE (rshift_input)))
	return NULL;

      /* Get the definition of the multiply-high-scale part.  */
      stmt_vec_info plus_input_stmt_info
	= vect_get_internal_def (vinfo, plus_input);
      if (!plus_input_stmt_info)
	return NULL;
      gassign *plus_input_stmt
	= dyn_cast <gassign *> (plus_input_stmt_info->stmt);
      if (!plus_input_stmt
	  || gimple_assign_rhs_code (plus_input_stmt) != RSHIFT_EXPR)
	return NULL;

      /* Look through any change in sign on the scaling input.  */
      vect_unpromoted_value unprom_scale_input;
      tree scale_input = vect_look_through_possible_promotion
	(vinfo, gimple_assign_rhs1 (plus_input_stmt), &unprom_scale_input);
      if (!scale_input
	  || TYPE_PRECISION (TREE_TYPE (scale_input))
	       != TYPE_PRECISION (TREE_TYPE (plus_input)))
	return NULL;

      /* Get the definition of the multiply-high part.  */
      mulh_stmt_info = vect_get_internal_def (vinfo, scale_input);
      if (!mulh_stmt_info)
	return NULL;

      /* Get the scaling term.  */
      scale_term = gimple_assign_rhs2 (plus_input_stmt);
      rounding_p = true;
    }
  else
    {
      mulh_stmt_info = rshift_input_stmt_info;
      scale_term = gimple_assign_rhs2 (last_stmt);
    }

  /* Check that the scaling factor is constant.  */
  if (TREE_CODE (scale_term) != INTEGER_CST)
    return NULL;

  /* Check whether the scaling input term can be seen as two widened
     inputs multiplied together.  */
  vect_unpromoted_value unprom_mult[2];
  tree new_type;
  unsigned int nops
    = vect_widened_op_tree (vinfo, mulh_stmt_info, MULT_EXPR, WIDEN_MULT_EXPR,
			    false, 2, unprom_mult, &new_type);
  if (nops != 2)
    return NULL;

  /* Adjust output precision.  */
  if (TYPE_PRECISION (new_type) < target_precision)
    new_type = build_nonstandard_integer_type
      (target_precision, TYPE_UNSIGNED (new_type));

  unsigned mult_precision = TYPE_PRECISION (new_type);
  internal_fn ifn;
  /* Check that the scaling factor is expected.  Instead of
     target_precision, we should use the one that we actually
     use for internal function.  */
  if (rounding_p)
    {
      /* Check pattern 2).  */
      if (wi::to_widest (scale_term) + mult_precision + 2
	  != TYPE_PRECISION (lhs_type))
	return NULL;

      ifn = IFN_MULHRS;
    }
  else
    {
      /* Check for pattern 1).  */
      if (wi::to_widest (scale_term) + mult_precision + 1
	  == TYPE_PRECISION (lhs_type))
	ifn = IFN_MULHS;
      /* Check for pattern 3).  */
      else if (wi::to_widest (scale_term) + mult_precision
	       == TYPE_PRECISION (lhs_type))
	ifn = IFN_MULH;
      else
	return NULL;
    }

  vect_pattern_detected ("vect_recog_mulhs_pattern", last_stmt);

  /* Check for target support.  */
  tree new_vectype = get_vectype_for_scalar_type (vinfo, new_type);
  if (!new_vectype
      || !direct_internal_fn_supported_p
	    (ifn, new_vectype, OPTIMIZE_FOR_SPEED))
    return NULL;

  /* The IR requires a valid vector type for the cast result, even though
     it's likely to be discarded.  */
  *type_out = get_vectype_for_scalar_type (vinfo, lhs_type);
  if (!*type_out)
    return NULL;

  /* Generate the IFN_MULHRS call.  */
  tree new_var = vect_recog_temp_ssa_var (new_type, NULL);
  tree new_ops[2];
  vect_convert_inputs (vinfo, last_stmt_info, 2, new_ops, new_type,
		       unprom_mult, new_vectype);
  gcall *mulhrs_stmt
    = gimple_build_call_internal (ifn, 2, new_ops[0], new_ops[1]);
  gimple_call_set_lhs (mulhrs_stmt, new_var);
  gimple_set_location (mulhrs_stmt, gimple_location (last_stmt));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created pattern stmt: %G", (gimple *) mulhrs_stmt);

  return vect_convert_output (vinfo, last_stmt_info, lhs_type,
			      mulhrs_stmt, new_vectype);
}

/* Recognize the patterns:

	    ATYPE a;  // narrower than TYPE
	    BTYPE b;  // narrower than TYPE
	(1) TYPE avg = ((TYPE) a + (TYPE) b) >> 1;
     or (2) TYPE avg = ((TYPE) a + (TYPE) b + 1) >> 1;

   where only the bottom half of avg is used.  Try to transform them into:

	(1) NTYPE avg' = .AVG_FLOOR ((NTYPE) a, (NTYPE) b);
     or (2) NTYPE avg' = .AVG_CEIL ((NTYPE) a, (NTYPE) b);

  followed by:

	    TYPE avg = (TYPE) avg';

  where NTYPE is no wider than half of TYPE.  Since only the bottom half
  of avg is used, all or part of the cast of avg' should become redundant.

  If there is no target support available, generate code to distribute rshift
  over plus and add a carry.  */

static gimple *
vect_recog_average_pattern (vec_info *vinfo,
			    stmt_vec_info last_stmt_info, tree *type_out)
{
  /* Check for a shift right by one bit.  */
  gassign *last_stmt = dyn_cast <gassign *> (last_stmt_info->stmt);
  if (!last_stmt
      || gimple_assign_rhs_code (last_stmt) != RSHIFT_EXPR
      || !integer_onep (gimple_assign_rhs2 (last_stmt)))
    return NULL;

  /* Check that the shift result is wider than the users of the
     result need (i.e. that narrowing would be a natural choice).  */
  tree lhs = gimple_assign_lhs (last_stmt);
  tree type = TREE_TYPE (lhs);
  unsigned int target_precision
    = vect_element_precision (last_stmt_info->min_output_precision);
  if (!INTEGRAL_TYPE_P (type) || target_precision >= TYPE_PRECISION (type))
    return NULL;

  /* Look through any change in sign on the shift input.  */
  tree rshift_rhs = gimple_assign_rhs1 (last_stmt);
  vect_unpromoted_value unprom_plus;
  rshift_rhs = vect_look_through_possible_promotion (vinfo, rshift_rhs,
						     &unprom_plus);
  if (!rshift_rhs
      || TYPE_PRECISION (TREE_TYPE (rshift_rhs)) != TYPE_PRECISION (type))
    return NULL;

  /* Get the definition of the shift input.  */
  stmt_vec_info plus_stmt_info = vect_get_internal_def (vinfo, rshift_rhs);
  if (!plus_stmt_info)
    return NULL;

  /* Check whether the shift input can be seen as a tree of additions on
     2 or 3 widened inputs.

     Note that the pattern should be a win even if the result of one or
     more additions is reused elsewhere: if the pattern matches, we'd be
     replacing 2N RSHIFT_EXPRs and N VEC_PACK_*s with N IFN_AVG_*s.  */
  internal_fn ifn = IFN_AVG_FLOOR;
  vect_unpromoted_value unprom[3];
  tree new_type;
  unsigned int nops = vect_widened_op_tree (vinfo, plus_stmt_info, PLUS_EXPR,
					    IFN_VEC_WIDEN_PLUS, false, 3,
					    unprom, &new_type);
  if (nops == 0)
    return NULL;
  if (nops == 3)
    {
      /* Check that one operand is 1.  */
      unsigned int i;
      for (i = 0; i < 3; ++i)
	if (integer_onep (unprom[i].op))
	  break;
      if (i == 3)
	return NULL;
      /* Throw away the 1 operand and keep the other two.  */
      if (i < 2)
	unprom[i] = unprom[2];
      ifn = IFN_AVG_CEIL;
    }

  vect_pattern_detected ("vect_recog_average_pattern", last_stmt);

  /* We know that:

     (a) the operation can be viewed as:

	   TYPE widened0 = (TYPE) UNPROM[0];
	   TYPE widened1 = (TYPE) UNPROM[1];
	   TYPE tmp1 = widened0 + widened1 {+ 1};
	   TYPE tmp2 = tmp1 >> 1;   // LAST_STMT_INFO

     (b) the first two statements are equivalent to:

	   TYPE widened0 = (TYPE) (NEW_TYPE) UNPROM[0];
	   TYPE widened1 = (TYPE) (NEW_TYPE) UNPROM[1];

     (c) vect_recog_over_widening_pattern has already tried to narrow TYPE
	 where sensible;

     (d) all the operations can be performed correctly at twice the width of
	 NEW_TYPE, due to the nature of the average operation; and

     (e) users of the result of the right shift need only TARGET_PRECISION
	 bits, where TARGET_PRECISION is no more than half of TYPE's
	 precision.

     Under these circumstances, the only situation in which NEW_TYPE
     could be narrower than TARGET_PRECISION is if widened0, widened1
     and an addition result are all used more than once.  Thus we can
     treat any widening of UNPROM[0] and UNPROM[1] to TARGET_PRECISION
     as "free", whereas widening the result of the average instruction
     from NEW_TYPE to TARGET_PRECISION would be a new operation.  It's
     therefore better not to go narrower than TARGET_PRECISION.  */
  if (TYPE_PRECISION (new_type) < target_precision)
    new_type = build_nonstandard_integer_type (target_precision,
					       TYPE_UNSIGNED (new_type));

  /* Check for target support.  */
  tree new_vectype = get_vectype_for_scalar_type (vinfo, new_type);
  if (!new_vectype)
    return NULL;

  bool fallback_p = false;

  if (direct_internal_fn_supported_p (ifn, new_vectype, OPTIMIZE_FOR_SPEED))
    ;
  else if (TYPE_UNSIGNED (new_type)
	   && optab_for_tree_code (RSHIFT_EXPR, new_vectype, optab_scalar)
	   && optab_for_tree_code (PLUS_EXPR, new_vectype, optab_default)
	   && optab_for_tree_code (BIT_IOR_EXPR, new_vectype, optab_default)
	   && optab_for_tree_code (BIT_AND_EXPR, new_vectype, optab_default))
    fallback_p = true;
  else
    return NULL;

  /* The IR requires a valid vector type for the cast result, even though
     it's likely to be discarded.  */
  *type_out = get_vectype_for_scalar_type (vinfo, type);
  if (!*type_out)
    return NULL;

  tree new_var = vect_recog_temp_ssa_var (new_type, NULL);
  tree new_ops[2];
  vect_convert_inputs (vinfo, last_stmt_info, 2, new_ops, new_type,
		       unprom, new_vectype);

  if (fallback_p)
    {
      /* As a fallback, generate code for following sequence:

	 shifted_op0 = new_ops[0] >> 1;
	 shifted_op1 = new_ops[1] >> 1;
	 sum_of_shifted = shifted_op0 + shifted_op1;
	 unmasked_carry = new_ops[0] and/or new_ops[1];
	 carry = unmasked_carry & 1;
	 new_var = sum_of_shifted + carry;
      */

      tree one_cst = build_one_cst (new_type);
      gassign *g;

      tree shifted_op0 = vect_recog_temp_ssa_var (new_type, NULL);
      g = gimple_build_assign (shifted_op0, RSHIFT_EXPR, new_ops[0], one_cst);
      append_pattern_def_seq (vinfo, last_stmt_info, g, new_vectype);

      tree shifted_op1 = vect_recog_temp_ssa_var (new_type, NULL);
      g = gimple_build_assign (shifted_op1, RSHIFT_EXPR, new_ops[1], one_cst);
      append_pattern_def_seq (vinfo, last_stmt_info, g, new_vectype);

      tree sum_of_shifted = vect_recog_temp_ssa_var (new_type, NULL);
      g = gimple_build_assign (sum_of_shifted, PLUS_EXPR,
			       shifted_op0, shifted_op1);
      append_pattern_def_seq (vinfo, last_stmt_info, g, new_vectype);

      tree unmasked_carry = vect_recog_temp_ssa_var (new_type, NULL);
      tree_code c = (ifn == IFN_AVG_CEIL) ? BIT_IOR_EXPR : BIT_AND_EXPR;
      g = gimple_build_assign (unmasked_carry, c, new_ops[0], new_ops[1]);
      append_pattern_def_seq (vinfo, last_stmt_info, g, new_vectype);

      tree carry = vect_recog_temp_ssa_var (new_type, NULL);
      g = gimple_build_assign (carry, BIT_AND_EXPR, unmasked_carry, one_cst);
      append_pattern_def_seq (vinfo, last_stmt_info, g, new_vectype);

      g = gimple_build_assign (new_var, PLUS_EXPR, sum_of_shifted, carry);
      return vect_convert_output (vinfo, last_stmt_info, type, g, new_vectype);
    }

  /* Generate the IFN_AVG* call.  */
  gcall *average_stmt = gimple_build_call_internal (ifn, 2, new_ops[0],
						    new_ops[1]);
  gimple_call_set_lhs (average_stmt, new_var);
  gimple_set_location (average_stmt, gimple_location (last_stmt));

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "created pattern stmt: %G", (gimple *) average_stmt);

  return vect_convert_output (vinfo, last_stmt_info,
			      type, average_stmt, new_vectype);
}

/* Recognize cases in which the input to a cast is wider than its
   output, and the input is fed by a widening operation.  Fold this
   by removing the unnecessary intermediate widening.  E.g.:

     unsigned char a;
     unsigned int b = (unsigned int) a;
     unsigned short c = (unsigned short) b;

   -->

     unsigned short c = (unsigned short) a;

   Although this is rare in input IR, it is an expected side-effect
   of the over-widening pattern above.

   This is beneficial also for integer-to-float conversions, if the
   widened integer has more bits than the float, and if the unwidened
   input doesn't.  */

static gimple *
vect_recog_cast_forwprop_pattern (vec_info *vinfo,
				  stmt_vec_info last_stmt_info, tree *type_out)
{
  /* Check for a cast, including an integer-to-float conversion.  */
  gassign *last_stmt = dyn_cast <gassign *> (last_stmt_info->stmt);
  if (!last_stmt)
    return NULL;
  tree_code code = gimple_assign_rhs_code (last_stmt);
  if (!CONVERT_EXPR_CODE_P (code) && code != FLOAT_EXPR)
    return NULL;

  /* Make sure that the rhs is a scalar with a natural bitsize.  */
  tree lhs = gimple_assign_lhs (last_stmt);
  if (!lhs)
    return NULL;
  tree lhs_type = TREE_TYPE (lhs);
  scalar_mode lhs_mode;
  if (VECT_SCALAR_BOOLEAN_TYPE_P (lhs_type)
      || !is_a <scalar_mode> (TYPE_MODE (lhs_type), &lhs_mode))
    return NULL;

  /* Check for a narrowing operation (from a vector point of view).  */
  tree rhs = gimple_assign_rhs1 (last_stmt);
  tree rhs_type = TREE_TYPE (rhs);
  if (!INTEGRAL_TYPE_P (rhs_type)
      || VECT_SCALAR_BOOLEAN_TYPE_P (rhs_type)
      || TYPE_PRECISION (rhs_type) <= GET_MODE_BITSIZE (lhs_mode))
    return NULL;

  /* Try to find an unpromoted input.  */
  vect_unpromoted_value unprom;
  if (!vect_look_through_possible_promotion (vinfo, rhs, &unprom)
      || TYPE_PRECISION (unprom.type) >= TYPE_PRECISION (rhs_type))
    return NULL;

  /* If the bits above RHS_TYPE matter, make sure that they're the
     same when extending from UNPROM as they are when extending from RHS.  */
  if (!INTEGRAL_TYPE_P (lhs_type)
      && TYPE_SIGN (rhs_type) != TYPE_SIGN (unprom.type))
    return NULL;

  /* We can get the same result by casting UNPROM directly, to avoid
     the unnecessary widening and narrowing.  */
  vect_pattern_detected ("vect_recog_cast_forwprop_pattern", last_stmt);

  *type_out = get_vectype_for_scalar_type (vinfo, lhs_type);
  if (!*type_out)
    return NULL;

  tree new_var = vect_recog_temp_ssa_var (lhs_type, NULL);
  gimple *pattern_stmt = gimple_build_assign (new_var, code, unprom.op);
  gimple_set_location (pattern_stmt, gimple_location (last_stmt));

  return pattern_stmt;
}

/* Try to detect a shift left of a widened input, converting LSHIFT_EXPR
   to WIDEN_LSHIFT_EXPR.  See vect_recog_widen_op_pattern for details.  */

static gimple *
vect_recog_widen_shift_pattern (vec_info *vinfo,
				stmt_vec_info last_stmt_info, tree *type_out)
{
  return vect_recog_widen_op_pattern (vinfo, last_stmt_info, type_out,
				      LSHIFT_EXPR, WIDEN_LSHIFT_EXPR, true,
				      "vect_recog_widen_shift_pattern");
}

/* Detect a rotate pattern wouldn't be otherwise vectorized:

   type a_t, b_t, c_t;

   S0 a_t = b_t r<< c_t;

  Input/Output:

  * STMT_VINFO: The stmt from which the pattern search begins,
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
vect_recog_rotate_pattern (vec_info *vinfo,
			   stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  tree oprnd0, oprnd1, lhs, var, var1, var2, vectype, type, stype, def, def2;
  gimple *pattern_stmt, *def_stmt;
  enum tree_code rhs_code;
  enum vect_def_type dt;
  optab optab1, optab2;
  edge ext_def = NULL;
  bool bswap16_p = false;

  if (is_gimple_assign (last_stmt))
    {
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
    }
  else if (gimple_call_builtin_p (last_stmt, BUILT_IN_BSWAP16))
    {
      /* __builtin_bswap16 (x) is another form of x r>> 8.
	 The vectorizer has bswap support, but only if the argument isn't
	 promoted.  */
      lhs = gimple_call_lhs (last_stmt);
      oprnd0 = gimple_call_arg (last_stmt, 0);
      type = TREE_TYPE (oprnd0);
      if (!lhs
	  || TYPE_PRECISION (TREE_TYPE (lhs)) != 16
	  || TYPE_PRECISION (type) <= 16
	  || TREE_CODE (oprnd0) != SSA_NAME
	  || BITS_PER_UNIT != 8)
	return NULL;

      stmt_vec_info def_stmt_info;
      if (!vect_is_simple_use (oprnd0, vinfo, &dt, &def_stmt_info, &def_stmt))
	return NULL;

      if (dt != vect_internal_def)
	return NULL;

      if (gimple_assign_cast_p (def_stmt))
	{
	  def = gimple_assign_rhs1 (def_stmt);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (def))
	      && TYPE_PRECISION (TREE_TYPE (def)) == 16)
	    oprnd0 = def;
	}

      type = TREE_TYPE (lhs);
      vectype = get_vectype_for_scalar_type (vinfo, type);
      if (vectype == NULL_TREE)
	return NULL;

      if (tree char_vectype = get_same_sized_vectype (char_type_node, vectype))
	{
	  /* The encoding uses one stepped pattern for each byte in the
	     16-bit word.  */
	  vec_perm_builder elts (TYPE_VECTOR_SUBPARTS (char_vectype), 2, 3);
	  for (unsigned i = 0; i < 3; ++i)
	    for (unsigned j = 0; j < 2; ++j)
	      elts.quick_push ((i + 1) * 2 - j - 1);

	  vec_perm_indices indices (elts, 1,
				    TYPE_VECTOR_SUBPARTS (char_vectype));
	  machine_mode vmode = TYPE_MODE (char_vectype);
	  if (can_vec_perm_const_p (vmode, vmode, indices))
	    {
	      /* vectorizable_bswap can handle the __builtin_bswap16 if we
		 undo the argument promotion.  */
	      if (!useless_type_conversion_p (type, TREE_TYPE (oprnd0)))
		{
		  def = vect_recog_temp_ssa_var (type, NULL);
		  def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd0);
		  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
		  oprnd0 = def;
		}

	      /* Pattern detected.  */
	      vect_pattern_detected ("vect_recog_rotate_pattern", last_stmt);

	      *type_out = vectype;

	      /* Pattern supported.  Create a stmt to be used to replace the
		 pattern, with the unpromoted argument.  */
	      var = vect_recog_temp_ssa_var (type, NULL);
	      pattern_stmt = gimple_build_call (gimple_call_fndecl (last_stmt),
						1, oprnd0);
	      gimple_call_set_lhs (pattern_stmt, var);
	      gimple_call_set_fntype (as_a <gcall *> (pattern_stmt),
				      gimple_call_fntype (last_stmt));
	      return pattern_stmt;
	    }
	}

      oprnd1 = build_int_cst (integer_type_node, 8);
      rhs_code = LROTATE_EXPR;
      bswap16_p = true;
    }
  else
    return NULL;

  if (TREE_CODE (oprnd0) != SSA_NAME
      || !INTEGRAL_TYPE_P (type)
      || TYPE_PRECISION (TREE_TYPE (lhs)) != TYPE_PRECISION (type))
    return NULL;

  stmt_vec_info def_stmt_info;
  if (!vect_is_simple_use (oprnd1, vinfo, &dt, &def_stmt_info, &def_stmt))
    return NULL;

  if (dt != vect_internal_def
      && dt != vect_constant_def
      && dt != vect_external_def)
    return NULL;

  vectype = get_vectype_for_scalar_type (vinfo, type);
  if (vectype == NULL_TREE)
    return NULL;

  /* If vector/vector or vector/scalar rotate is supported by the target,
     don't do anything here.  */
  optab1 = optab_for_tree_code (rhs_code, vectype, optab_vector);
  if (optab1
      && can_implement_p (optab1, TYPE_MODE (vectype)))
    {
     use_rotate:
      if (bswap16_p)
	{
	  if (!useless_type_conversion_p (type, TREE_TYPE (oprnd0)))
	    {
	      def = vect_recog_temp_ssa_var (type, NULL);
	      def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd0);
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	      oprnd0 = def;
	    }

	  /* Pattern detected.  */
	  vect_pattern_detected ("vect_recog_rotate_pattern", last_stmt);

	  *type_out = vectype;

	  /* Pattern supported.  Create a stmt to be used to replace the
	     pattern.  */
	  var = vect_recog_temp_ssa_var (type, NULL);
	  pattern_stmt = gimple_build_assign (var, LROTATE_EXPR, oprnd0,
					      oprnd1);
	  return pattern_stmt;
	}
      return NULL;
    }

  if (is_a <bb_vec_info> (vinfo) || dt != vect_internal_def)
    {
      optab2 = optab_for_tree_code (rhs_code, vectype, optab_scalar);
      if (optab2
	  && can_implement_p (optab2, TYPE_MODE (vectype)))
	goto use_rotate;
    }

  tree utype = unsigned_type_for (type);
  tree uvectype = get_vectype_for_scalar_type (vinfo, utype);
  if (!uvectype)
    return NULL;

  /* If vector/vector or vector/scalar shifts aren't supported by the target,
     don't do anything here either.  */
  optab1 = optab_for_tree_code (LSHIFT_EXPR, uvectype, optab_vector);
  optab2 = optab_for_tree_code (RSHIFT_EXPR, uvectype, optab_vector);
  if (!optab1
      || !can_implement_p (optab1, TYPE_MODE (uvectype))
      || !optab2
      || !can_implement_p (optab2, TYPE_MODE (uvectype)))
    {
      if (! is_a <bb_vec_info> (vinfo) && dt == vect_internal_def)
	return NULL;
      optab1 = optab_for_tree_code (LSHIFT_EXPR, uvectype, optab_scalar);
      optab2 = optab_for_tree_code (RSHIFT_EXPR, uvectype, optab_scalar);
      if (!optab1
	  || !can_implement_p (optab1, TYPE_MODE (uvectype))
	  || !optab2
	  || !can_implement_p (optab2, TYPE_MODE (uvectype)))
	return NULL;
    }

  *type_out = vectype;

  if (!useless_type_conversion_p (utype, TREE_TYPE (oprnd0)))
    {
      def = vect_recog_temp_ssa_var (utype, NULL);
      def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd0);
      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, uvectype);
      oprnd0 = def;
    }

  if (dt == vect_external_def && TREE_CODE (oprnd1) == SSA_NAME)
    ext_def = vect_get_external_def_edge (vinfo, oprnd1);

  def = NULL_TREE;
  scalar_int_mode mode = SCALAR_INT_TYPE_MODE (utype);
  if (dt != vect_internal_def || TYPE_MODE (TREE_TYPE (oprnd1)) == mode)
    def = oprnd1;
  else if (def_stmt && gimple_assign_cast_p (def_stmt))
    {
      tree rhs1 = gimple_assign_rhs1 (def_stmt);
      if (TYPE_MODE (TREE_TYPE (rhs1)) == mode
	  && TYPE_PRECISION (TREE_TYPE (rhs1))
	     == TYPE_PRECISION (type))
	def = rhs1;
    }

  if (def == NULL_TREE)
    {
      def = vect_recog_temp_ssa_var (utype, NULL);
      def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd1);
      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, uvectype);
    }
  stype = TREE_TYPE (def);

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
      tree vecstype = get_vectype_for_scalar_type (vinfo, stype);

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
	append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vecstype);

      def2 = vect_recog_temp_ssa_var (stype, NULL);
      tree mask = build_int_cst (stype, GET_MODE_PRECISION (mode) - 1);
      def_stmt = gimple_build_assign (def2, BIT_AND_EXPR,
				      gimple_assign_lhs (def_stmt), mask);
      if (ext_def)
	{
	  basic_block new_bb
	    = gsi_insert_on_edge_immediate (ext_def, def_stmt);
	  gcc_assert (!new_bb);
	}
      else
	append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vecstype);
    }

  var1 = vect_recog_temp_ssa_var (utype, NULL);
  def_stmt = gimple_build_assign (var1, rhs_code == LROTATE_EXPR
					? LSHIFT_EXPR : RSHIFT_EXPR,
				  oprnd0, def);
  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, uvectype);

  var2 = vect_recog_temp_ssa_var (utype, NULL);
  def_stmt = gimple_build_assign (var2, rhs_code == LROTATE_EXPR
					? RSHIFT_EXPR : LSHIFT_EXPR,
				  oprnd0, def2);
  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, uvectype);

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_rotate_pattern", last_stmt);

  /* Pattern supported.  Create a stmt to be used to replace the pattern.  */
  var = vect_recog_temp_ssa_var (utype, NULL);
  pattern_stmt = gimple_build_assign (var, BIT_IOR_EXPR, var1, var2);

  if (!useless_type_conversion_p (type, utype))
    {
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, uvectype);
      tree result = vect_recog_temp_ssa_var (type, NULL);
      pattern_stmt = gimple_build_assign (result, NOP_EXPR, var);
    }
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

  * STMT_VINFO: The stmt from which the pattern search begins,
    i.e. the shift/rotate stmt.  The original stmt (S3) is replaced
    with a shift/rotate which has same type on both operands, in the
    second case just b_T op c_T, in the first case with added cast
    from a_t to c_T in STMT_VINFO_PATTERN_DEF_SEQ.

  Output:

  * TYPE_OUT: The type of the output of this pattern.

  * Return value: A new stmt that will be used to replace the shift/rotate
    S3 stmt.  */

static gimple *
vect_recog_vector_vector_shift_pattern (vec_info *vinfo,
					stmt_vec_info stmt_vinfo,
					tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  tree oprnd0, oprnd1, lhs, var;
  gimple *pattern_stmt;
  enum tree_code rhs_code;

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
      || !INTEGRAL_TYPE_P (TREE_TYPE (oprnd0))
      || !type_has_mode_precision_p (TREE_TYPE (oprnd1))
      || TYPE_PRECISION (TREE_TYPE (lhs))
	 != TYPE_PRECISION (TREE_TYPE (oprnd0)))
    return NULL;

  stmt_vec_info def_vinfo = vect_get_internal_def (vinfo, oprnd1);
  if (!def_vinfo)
    return NULL;

  *type_out = get_vectype_for_scalar_type (vinfo, TREE_TYPE (oprnd0));
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
	      tree vecstype = get_vectype_for_scalar_type (vinfo,
							   TREE_TYPE (rhs1));
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vecstype);
	    }
	}
    }

  if (def == NULL_TREE)
    {
      def = vect_recog_temp_ssa_var (TREE_TYPE (oprnd0), NULL);
      def_stmt = gimple_build_assign (def, NOP_EXPR, oprnd1);
      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
    }

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_vector_vector_shift_pattern", last_stmt);

  /* Pattern supported.  Create a stmt to be used to replace the pattern.  */
  var = vect_recog_temp_ssa_var (TREE_TYPE (oprnd0), NULL);
  pattern_stmt = gimple_build_assign (var, rhs_code, oprnd0, def);

  return pattern_stmt;
}

/* Return true iff the target has a vector optab implementing the operation
   CODE on type VECTYPE.  */

static bool
target_has_vecop_for_code (tree_code code, tree vectype)
{
  optab voptab = optab_for_tree_code (code, vectype, optab_vector);
  return voptab
	 && can_implement_p (voptab, TYPE_MODE (vectype));
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
synth_lshift_by_additions (vec_info *vinfo,
			   tree dest, tree op, HOST_WIDE_INT amnt,
			   stmt_vec_info stmt_info)
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
	append_pattern_def_seq (vinfo, stmt_info, stmt);
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
apply_binop_and_append_stmt (vec_info *vinfo,
			     tree_code code, tree op1, tree op2,
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
      stmt = synth_lshift_by_additions (vinfo, tmp_var, op1,
					TREE_INT_CST_LOW (op2), stmt_vinfo);
      append_pattern_def_seq (vinfo, stmt_vinfo, stmt);
      return tmp_var;
    }

  stmt = gimple_build_assign (tmp_var, code, op1, op2);
  append_pattern_def_seq (vinfo, stmt_vinfo, stmt);
  return tmp_var;
}

/* Synthesize a multiplication of OP by an INTEGER_CST VAL using shifts
   and simple arithmetic operations to be vectorized.  Record the statements
   produced in STMT_VINFO and return the last statement in the sequence or
   NULL if it's not possible to synthesize such a multiplication.
   This function mirrors the behavior of expand_mult_const in expmed.cc but
   works on tree-ssa form.  */

static gimple *
vect_synth_mult_by_constant (vec_info *vinfo, tree op, tree val,
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
  tree vectype = get_vectype_for_scalar_type (vinfo, multtype);
  if (!vectype)
    return NULL;

  /* Targets that don't support vector shifts but support vector additions
     can synthesize shifts that way.  */
  bool synth_shift_p = !vect_supportable_shift (vinfo, LSHIFT_EXPR, multtype);

  HOST_WIDE_INT hwval = tree_to_shwi (val);
  /* Use MAX_COST here as we don't want to limit the sequence on rtx costs.
     The vectorizer's benefit analysis will decide whether it's beneficial
     to do this.  */
  bool possible = choose_mult_variant (VECTOR_MODE_P (TYPE_MODE (vectype))
				       ? TYPE_MODE (vectype) : mode,
				       hwval, &alg, &variant, MAX_COST);
  if (!possible)
    return NULL;

  if (!target_supports_mult_synth_alg (&alg, variant, vectype, synth_shift_p))
    return NULL;

  tree accumulator;

  /* Clear out the sequence of statements so we can populate it below.  */
  gimple *stmt = NULL;

  if (cast_to_unsigned_p)
    {
      tree tmp_op = vect_recog_temp_ssa_var (multtype, NULL);
      stmt = gimple_build_assign (tmp_op, CONVERT_EXPR, op);
      append_pattern_def_seq (vinfo, stmt_vinfo, stmt);
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
	      = synth_lshift_by_additions (vinfo, accum_tmp, accumulator,
					   alg.log[i], stmt_vinfo);
	  else
	    stmt = gimple_build_assign (accum_tmp, LSHIFT_EXPR, accumulator,
					 shft_log);
	  break;
	case alg_add_t_m2:
	  tmp_var
	    = apply_binop_and_append_stmt (vinfo, LSHIFT_EXPR, op, shft_log,
					   stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, accumulator,
				       tmp_var);
	  break;
	case alg_sub_t_m2:
	  tmp_var = apply_binop_and_append_stmt (vinfo, LSHIFT_EXPR, op,
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
	    = apply_binop_and_append_stmt (vinfo, LSHIFT_EXPR, accumulator,
					   shft_log, stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, tmp_var, op);
	  break;
	case alg_sub_t2_m:
	  tmp_var
	    = apply_binop_and_append_stmt (vinfo, LSHIFT_EXPR, accumulator,
					   shft_log, stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, MINUS_EXPR, tmp_var, op);
	  break;
	case alg_add_factor:
	  tmp_var
	    = apply_binop_and_append_stmt (vinfo, LSHIFT_EXPR, accumulator,
					   shft_log, stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, accumulator,
				       tmp_var);
	  break;
	case alg_sub_factor:
	  tmp_var
	    = apply_binop_and_append_stmt (vinfo, LSHIFT_EXPR, accumulator,
					   shft_log, stmt_vinfo, synth_shift_p);
	  stmt = gimple_build_assign (accum_tmp, MINUS_EXPR, tmp_var,
				      accumulator);
	  break;
	default:
	  gcc_unreachable ();
	}
      /* We don't want to append the last stmt in the sequence to stmt_vinfo
	 but rather return it directly.  */

      if ((i < alg.ops - 1) || needs_fixup || cast_to_unsigned_p)
	append_pattern_def_seq (vinfo, stmt_vinfo, stmt);
      accumulator = accum_tmp;
    }
  if (variant == negate_variant)
    {
      tree accum_tmp = vect_recog_temp_ssa_var (multtype, NULL);
      stmt = gimple_build_assign (accum_tmp, NEGATE_EXPR, accumulator);
      accumulator = accum_tmp;
      if (cast_to_unsigned_p)
	append_pattern_def_seq (vinfo, stmt_vinfo, stmt);
    }
  else if (variant == add_variant)
    {
      tree accum_tmp = vect_recog_temp_ssa_var (multtype, NULL);
      stmt = gimple_build_assign (accum_tmp, PLUS_EXPR, accumulator, op);
      accumulator = accum_tmp;
      if (cast_to_unsigned_p)
	append_pattern_def_seq (vinfo, stmt_vinfo, stmt);
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
   choose_mult_variant algorithms from expmed.cc

   Input/Output:

   STMT_VINFO: The stmt from which the pattern search begins,
   i.e. the mult stmt.

 Output:

  * TYPE_OUT: The type of the output of this pattern.

  * Return value: A new stmt that will be used to replace
    the multiplication.  */

static gimple *
vect_recog_mult_pattern (vec_info *vinfo,
			 stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  tree oprnd0, oprnd1, vectype, itype;
  gimple *pattern_stmt;

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

  vectype = get_vectype_for_scalar_type (vinfo, itype);
  if (vectype == NULL_TREE)
    return NULL;

  /* If the target can handle vectorized multiplication natively,
     don't attempt to optimize this.  */
  optab mul_optab = optab_for_tree_code (MULT_EXPR, vectype, optab_default);
  if (mul_optab != unknown_optab
      && can_implement_p (mul_optab, TYPE_MODE (vectype)))
    return NULL;

  pattern_stmt = vect_synth_mult_by_constant (vinfo,
					      oprnd0, oprnd1, stmt_vinfo);
  if (!pattern_stmt)
    return NULL;

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_mult_pattern", last_stmt);

  *type_out = vectype;

  return pattern_stmt;
}

extern bool gimple_unsigned_integer_sat_add (tree, tree*, tree (*)(tree));
extern bool gimple_unsigned_integer_sat_sub (tree, tree*, tree (*)(tree));
extern bool gimple_unsigned_integer_sat_trunc (tree, tree*, tree (*)(tree));

extern bool gimple_signed_integer_sat_add (tree, tree*, tree (*)(tree));
extern bool gimple_signed_integer_sat_sub (tree, tree*, tree (*)(tree));
extern bool gimple_signed_integer_sat_trunc (tree, tree*, tree (*)(tree));

static gimple *
vect_recog_build_binary_gimple_stmt (vec_info *vinfo, stmt_vec_info stmt_info,
				     internal_fn fn, tree *type_out,
				     tree lhs, tree op_0, tree op_1)
{
  tree itype = TREE_TYPE (op_0);
  tree otype = TREE_TYPE (lhs);
  tree v_itype = get_vectype_for_scalar_type (vinfo, itype);
  tree v_otype = get_vectype_for_scalar_type (vinfo, otype);

  if (v_itype != NULL_TREE && v_otype != NULL_TREE
    && direct_internal_fn_supported_p (fn, v_itype, OPTIMIZE_FOR_BOTH))
    {
      gcall *call = gimple_build_call_internal (fn, 2, op_0, op_1);
      tree in_ssa = vect_recog_temp_ssa_var (itype, NULL);

      gimple_call_set_lhs (call, in_ssa);
      gimple_call_set_nothrow (call, /* nothrow_p */ false);
      gimple_set_location (call, gimple_location (STMT_VINFO_STMT (stmt_info)));

      *type_out = v_otype;

      if (types_compatible_p (itype, otype))
	return call;
      else
	{
	  append_pattern_def_seq (vinfo, stmt_info, call, v_itype);
	  tree out_ssa = vect_recog_temp_ssa_var (otype, NULL);

	  return gimple_build_assign (out_ssa, NOP_EXPR, in_ssa);
	}
    }

  return NULL;
}

/*
 * Try to detect saturation add pattern (SAT_ADD), aka below gimple:
 *   _7 = _4 + _6;
 *   _8 = _4 > _7;
 *   _9 = (long unsigned int) _8;
 *   _10 = -_9;
 *   _12 = _7 | _10;
 *
 * And then simplied to
 *   _12 = .SAT_ADD (_4, _6);
 */

static gimple *
vect_recog_sat_add_pattern (vec_info *vinfo, stmt_vec_info stmt_vinfo,
			    tree *type_out)
{
  gimple *last_stmt = STMT_VINFO_STMT (stmt_vinfo);

  if (!is_gimple_assign (last_stmt))
    return NULL;

  tree ops[2];
  tree lhs = gimple_assign_lhs (last_stmt);

  if (gimple_unsigned_integer_sat_add (lhs, ops, NULL)
      || gimple_signed_integer_sat_add (lhs, ops, NULL))
    {
      if (TREE_CODE (ops[1]) == INTEGER_CST)
	ops[1] = fold_convert (TREE_TYPE (ops[0]), ops[1]);

      gimple *stmt = vect_recog_build_binary_gimple_stmt (vinfo, stmt_vinfo,
							  IFN_SAT_ADD, type_out,
							  lhs, ops[0], ops[1]);
      if (stmt)
	{
	  vect_pattern_detected ("vect_recog_sat_add_pattern", last_stmt);
	  return stmt;
	}
    }

  return NULL;
}

/*
 * Try to transform the truncation for .SAT_SUB pattern,  mostly occurs in
 * the benchmark zip.  Aka:
 *
 *   unsigned int _1;
 *   unsigned int _2;
 *   unsigned short int _4;
 *   _9 = (unsigned short int).SAT_SUB (_1, _2);
 *
 *   if _1 is known to be in the range of unsigned short int.  For example
 *   there is a def _1 = (unsigned short int)_4.  Then we can transform the
 *   truncation to:
 *
 *   _3 = (unsigned short int) MIN (65535, _2); // aka _3 = .SAT_TRUNC (_2);
 *   _9 = .SAT_SUB (_4, _3);
 *
 *   Then,  we can better vectorized code and avoid the unnecessary narrowing
 *   stmt during vectorization with below stmt(s).
 *
 *   _3 = .SAT_TRUNC(_2); // SI => HI
 *   _9 = .SAT_SUB (_4, _3);
 */
static void
vect_recog_sat_sub_pattern_transform (vec_info *vinfo,
				      stmt_vec_info stmt_vinfo,
				      tree lhs, tree *ops)
{
  tree otype = TREE_TYPE (lhs);
  tree itype = TREE_TYPE (ops[0]);
  unsigned itype_prec = TYPE_PRECISION (itype);
  unsigned otype_prec = TYPE_PRECISION (otype);

  if (types_compatible_p (otype, itype) || otype_prec >= itype_prec)
    return;

  tree v_otype = get_vectype_for_scalar_type (vinfo, otype);
  tree v_itype = get_vectype_for_scalar_type (vinfo, itype);
  tree_pair v_pair = tree_pair (v_otype, v_itype);

  if (v_otype == NULL_TREE || v_itype == NULL_TREE
    || !direct_internal_fn_supported_p (IFN_SAT_TRUNC, v_pair,
					OPTIMIZE_FOR_BOTH))
    return;

  /* 1. Find the _4 and update ops[0] as above example.  */
  vect_unpromoted_value unprom;
  tree tmp = vect_look_through_possible_promotion (vinfo, ops[0], &unprom);

  if (tmp == NULL_TREE || TYPE_PRECISION (unprom.type) != otype_prec)
    return;

  ops[0] = tmp;

  /* 2. Generate _3 = .SAT_TRUNC (_2) and update ops[1] as above example.  */
  tree trunc_lhs_ssa = vect_recog_temp_ssa_var (otype, NULL);
  gcall *call = gimple_build_call_internal (IFN_SAT_TRUNC, 1, ops[1]);

  gimple_call_set_lhs (call, trunc_lhs_ssa);
  gimple_call_set_nothrow (call, /* nothrow_p */ false);
  append_pattern_def_seq (vinfo, stmt_vinfo, call, v_otype);

  ops[1] = trunc_lhs_ssa;
}

/*
 * Try to detect saturation sub pattern (SAT_ADD), aka below gimple:
 * Unsigned:
 *   _7 = _1 >= _2;
 *   _8 = _1 - _2;
 *   _10 = (long unsigned int) _7;
 *   _9 = _8 * _10;
 *
 * And then simplied to
 *   _9 = .SAT_SUB (_1, _2);
 *
 * Signed:
 *   x.0_4 = (unsigned char) x_16;
 *   y.1_5 = (unsigned char) y_18;
 *   _6 = x.0_4 - y.1_5;
 *   minus_19 = (int8_t) _6;
 *   _7 = x_16 ^ y_18;
 *   _8 = x_16 ^ minus_19;
 *   _44 = _7 < 0;
 *   _23 = x_16 < 0;
 *   _24 = (signed char) _23;
 *   _58 = (unsigned char) _24;
 *   _59 = -_58;
 *   _25 = (signed char) _59;
 *   _26 = _25 ^ 127;
 *   _42 = _8 < 0;
 *   _41 = _42 & _44;
 *   iftmp.2_11 = _41 ? _26 : minus_19;
 *
 * And then simplied to
 *   iftmp.2_11 = .SAT_SUB (x_16, y_18);
 */

static gimple *
vect_recog_sat_sub_pattern (vec_info *vinfo, stmt_vec_info stmt_vinfo,
			    tree *type_out)
{
  gimple *last_stmt = STMT_VINFO_STMT (stmt_vinfo);

  if (!is_gimple_assign (last_stmt))
    return NULL;

  tree ops[2];
  tree lhs = gimple_assign_lhs (last_stmt);

  if (gimple_unsigned_integer_sat_sub (lhs, ops, NULL)
      || gimple_signed_integer_sat_sub (lhs, ops, NULL))
    {
      vect_recog_sat_sub_pattern_transform (vinfo, stmt_vinfo, lhs, ops);
      gimple *stmt = vect_recog_build_binary_gimple_stmt (vinfo, stmt_vinfo,
							  IFN_SAT_SUB, type_out,
							  lhs, ops[0], ops[1]);
      if (stmt)
	{
	  vect_pattern_detected ("vect_recog_sat_sub_pattern", last_stmt);
	  return stmt;
	}
    }

  return NULL;
}

/*
 * Try to detect saturation truncation pattern (SAT_TRUNC), aka below gimple:
 *   overflow_5 = x_4(D) > 4294967295;
 *   _1 = (unsigned int) x_4(D);
 *   _2 = (unsigned int) overflow_5;
 *   _3 = -_2;
 *   _6 = _1 | _3;
 *
 * And then simplied to
 *   _6 = .SAT_TRUNC (x_4(D));
 */

static gimple *
vect_recog_sat_trunc_pattern (vec_info *vinfo, stmt_vec_info stmt_vinfo,
			      tree *type_out)
{
  gimple *last_stmt = STMT_VINFO_STMT (stmt_vinfo);

  if (!is_gimple_assign (last_stmt))
    return NULL;

  tree ops[1];
  tree lhs = gimple_assign_lhs (last_stmt);
  tree otype = TREE_TYPE (lhs);

  if ((gimple_unsigned_integer_sat_trunc (lhs, ops, NULL)
       || gimple_signed_integer_sat_trunc (lhs, ops, NULL))
      && type_has_mode_precision_p (otype))
    {
      tree itype = TREE_TYPE (ops[0]);
      tree v_itype = get_vectype_for_scalar_type (vinfo, itype);
      tree v_otype = get_vectype_for_scalar_type (vinfo, otype);
      internal_fn fn = IFN_SAT_TRUNC;

      if (v_itype != NULL_TREE && v_otype != NULL_TREE
	&& direct_internal_fn_supported_p (fn, tree_pair (v_otype, v_itype),
					   OPTIMIZE_FOR_BOTH))
	{
	  gcall *call = gimple_build_call_internal (fn, 1, ops[0]);
	  tree out_ssa = vect_recog_temp_ssa_var (otype, NULL);

	  gimple_call_set_lhs (call, out_ssa);
	  gimple_call_set_nothrow (call, /* nothrow_p */ false);
	  gimple_set_location (call, gimple_location (last_stmt));

	  *type_out = v_otype;

	  return call;
	}
    }

  return NULL;
}

/* Detect a signed division by a constant that wouldn't be
   otherwise vectorized:

   type a_t, b_t;

   S1 a_t = b_t / N;

  where type 'type' is an integral type and N is a constant.

  Similarly handle modulo by a constant:

   S4 a_t = b_t % N;

  Input/Output:

  * STMT_VINFO: The stmt from which the pattern search begins,
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
vect_recog_divmod_pattern (vec_info *vinfo,
			   stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  tree oprnd0, oprnd1, vectype, itype, cond;
  gimple *pattern_stmt, *def_stmt;
  enum tree_code rhs_code;
  optab optab;
  tree q, cst;
  int prec;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  rhs_code = gimple_assign_rhs_code (last_stmt);
  switch (rhs_code)
    {
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
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
  vectype = get_vectype_for_scalar_type (vinfo, itype);
  if (vectype == NULL_TREE)
    return NULL;

  if (optimize_bb_for_size_p (gimple_bb (last_stmt)))
    {
      /* If the target can handle vectorized division or modulo natively,
	 don't attempt to optimize this, since native division is likely
	 to give smaller code.  */
      optab = optab_for_tree_code (rhs_code, vectype, optab_default);
      if (optab != unknown_optab
	  && can_implement_p (optab, TYPE_MODE (vectype)))
	return NULL;
    }

  prec = TYPE_PRECISION (itype);
  if (integer_pow2p (oprnd1))
    {
      if (TYPE_UNSIGNED (itype) || tree_int_cst_sgn (oprnd1) != 1)
	return NULL;

      /* Pattern detected.  */
      vect_pattern_detected ("vect_recog_divmod_pattern", last_stmt);

      *type_out = vectype;

      /* Check if the target supports this internal function.  */
      internal_fn ifn = IFN_DIV_POW2;
      if (direct_internal_fn_supported_p (ifn, vectype, OPTIMIZE_FOR_SPEED))
	{
	  tree shift = build_int_cst (itype, tree_log2 (oprnd1));

	  tree var_div = vect_recog_temp_ssa_var (itype, NULL);
	  gimple *div_stmt = gimple_build_call_internal (ifn, 2, oprnd0, shift);
	  gimple_call_set_lhs (div_stmt, var_div);

	  if (rhs_code == TRUNC_MOD_EXPR)
	    {
	      append_pattern_def_seq (vinfo, stmt_vinfo, div_stmt);
	      def_stmt
		= gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				       LSHIFT_EXPR, var_div, shift);
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	      pattern_stmt
		= gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				       MINUS_EXPR, oprnd0,
				       gimple_assign_lhs (def_stmt));
	    }
	  else
	    pattern_stmt = div_stmt;
	  gimple_set_location (pattern_stmt, gimple_location (last_stmt));

	  return pattern_stmt;
	}

      cond = vect_recog_temp_ssa_var (boolean_type_node, NULL);
      def_stmt = gimple_build_assign (cond, LT_EXPR, oprnd0,
				      build_int_cst (itype, 0));
      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt,
			      truth_type_for (vectype), itype);
      if (rhs_code == TRUNC_DIV_EXPR
	  || rhs_code == EXACT_DIV_EXPR)
	{
	  tree var = vect_recog_temp_ssa_var (itype, NULL);
	  tree shift;
	  def_stmt
	    = gimple_build_assign (var, COND_EXPR, cond,
				   fold_build2 (MINUS_EXPR, itype, oprnd1,
						build_int_cst (itype, 1)),
				   build_int_cst (itype, 0));
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	  var = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (var, PLUS_EXPR, oprnd0,
				   gimple_assign_lhs (def_stmt));
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

	  shift = build_int_cst (itype, tree_log2 (oprnd1));
	  pattern_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   RSHIFT_EXPR, var, shift);
	}
      else
	{
	  tree signmask;
	  if (compare_tree_int (oprnd1, 2) == 0)
	    {
	      signmask = vect_recog_temp_ssa_var (itype, NULL);
	      def_stmt = gimple_build_assign (signmask, COND_EXPR, cond,
					      build_int_cst (itype, 1),
					      build_int_cst (itype, 0));
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	    }
	  else
	    {
	      tree utype
		= build_nonstandard_integer_type (prec, 1);
	      tree vecutype = get_vectype_for_scalar_type (vinfo, utype);
	      tree shift
		= build_int_cst (utype, GET_MODE_BITSIZE (itype_mode)
					- tree_log2 (oprnd1));
	      tree var = vect_recog_temp_ssa_var (utype, NULL);

	      def_stmt = gimple_build_assign (var, COND_EXPR, cond,
					      build_int_cst (utype, -1),
					      build_int_cst (utype, 0));
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vecutype);
	      var = vect_recog_temp_ssa_var (utype, NULL);
	      def_stmt = gimple_build_assign (var, RSHIFT_EXPR,
					      gimple_assign_lhs (def_stmt),
					      shift);
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vecutype);
	      signmask = vect_recog_temp_ssa_var (itype, NULL);
	      def_stmt
		= gimple_build_assign (signmask, NOP_EXPR, var);
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	    }
	  def_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   PLUS_EXPR, oprnd0, signmask);
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	  def_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   BIT_AND_EXPR, gimple_assign_lhs (def_stmt),
				   fold_build2 (MINUS_EXPR, itype, oprnd1,
						build_int_cst (itype, 1)));
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

	  pattern_stmt
	    = gimple_build_assign (vect_recog_temp_ssa_var (itype, NULL),
				   MINUS_EXPR, gimple_assign_lhs (def_stmt),
				   signmask);
	}

      return pattern_stmt;
    }

  if ((cst = uniform_integer_cst_p (oprnd1))
      && TYPE_UNSIGNED (itype)
      && rhs_code == TRUNC_DIV_EXPR
      && vectype
      && targetm.vectorize.preferred_div_as_shifts_over_mult (vectype))
    {
      /* We can use the relationship:

	   x // N == ((x+N+2) // (N+1) + x) // (N+1)  for 0 <= x < N(N+3)

	 to optimize cases where N+1 is a power of 2, and where // (N+1)
	 is therefore a shift right.  When operating in modes that are
	 multiples of a byte in size, there are two cases:

	 (1) N(N+3) is not representable, in which case the question
	     becomes whether the replacement expression overflows.
	     It is enough to test that x+N+2 does not overflow,
	     i.e. that x < MAX-(N+1).

	 (2) N(N+3) is representable, in which case it is the (only)
	     bound that we need to check.

	 ??? For now we just handle the case where // (N+1) is a shift
	 right by half the precision, since some architectures can
	 optimize the associated addition and shift combinations
	 into single instructions.  */

      auto wcst = wi::to_wide (cst);
      int pow = wi::exact_log2 (wcst + 1);
      if (pow == prec / 2)
	{
	  gimple *stmt = SSA_NAME_DEF_STMT (oprnd0);

	  gimple_ranger ranger;
	  int_range_max r;

	  /* Check that no overflow will occur.  If we don't have range
	     information we can't perform the optimization.  */

	  if (ranger.range_of_expr (r, oprnd0, stmt) && !r.undefined_p ())
	    {
	      wide_int max = r.upper_bound ();
	      wide_int one = wi::shwi (1, prec);
	      wide_int adder = wi::add (one, wi::lshift (one, pow));
	      wi::overflow_type ovf;
	      wi::add (max, adder, UNSIGNED, &ovf);
	      if (ovf == wi::OVF_NONE)
		{
		  *type_out = vectype;
		  tree tadder = wide_int_to_tree (itype, adder);
		  tree rshift = wide_int_to_tree (itype, pow);

		  tree new_lhs1 = vect_recog_temp_ssa_var (itype, NULL);
		  gassign *patt1
		    = gimple_build_assign (new_lhs1, PLUS_EXPR, oprnd0, tadder);
		  append_pattern_def_seq (vinfo, stmt_vinfo, patt1, vectype);

		  tree new_lhs2 = vect_recog_temp_ssa_var (itype, NULL);
		  patt1 = gimple_build_assign (new_lhs2, RSHIFT_EXPR, new_lhs1,
					       rshift);
		  append_pattern_def_seq (vinfo, stmt_vinfo, patt1, vectype);

		  tree new_lhs3 = vect_recog_temp_ssa_var (itype, NULL);
		  patt1 = gimple_build_assign (new_lhs3, PLUS_EXPR, new_lhs2,
					       oprnd0);
		  append_pattern_def_seq (vinfo, stmt_vinfo, patt1, vectype);

		  tree new_lhs4 = vect_recog_temp_ssa_var (itype, NULL);
		  pattern_stmt = gimple_build_assign (new_lhs4, RSHIFT_EXPR,
						      new_lhs3, rshift);

		  return pattern_stmt;
		}
	    }
	}
    }

  if (prec > HOST_BITS_PER_WIDE_INT
      || integer_zerop (oprnd1))
    return NULL;

  if (!can_mult_highpart_p (TYPE_MODE (vectype), TYPE_UNSIGNED (itype)))
    return NULL;

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

      /* Find a suitable multiplier and right shift count instead of
	 directly dividing by D.  */
      mh = choose_multiplier (d, prec, prec, &ml, &post_shift);

      /* If the suggested multiplier is more than PREC bits, we can do better
	 for even divisors, using an initial right shift.  */
      if (mh != 0 && (d & 1) == 0)
	{
	  pre_shift = ctz_or_zero (d);
	  mh = choose_multiplier (d >> pre_shift, prec, prec - pre_shift,
				  &ml, &post_shift);
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
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

	  t2 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (t2, MINUS_EXPR, oprnd0, t1);
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

	  t3 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (t3, RSHIFT_EXPR, t2, integer_one_node);
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

	  t4 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt
	    = gimple_build_assign (t4, PLUS_EXPR, t1, t3);

	  if (post_shift != 1)
	    {
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

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
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	    }
	  else
	    t1 = oprnd0;

	  t2 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t2, MULT_HIGHPART_EXPR, t1,
					  build_int_cst (itype, ml));

	  if (post_shift)
	    {
	      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

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
      if (HOST_BITS_PER_WIDE_INT >= prec
	  && abs_d == HOST_WIDE_INT_1U << (prec - 1))
	/* This case is not handled correctly below.  */
	return NULL;

      choose_multiplier (abs_d, prec, prec - 1, &ml, &post_shift);
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
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	  t2 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t2, PLUS_EXPR, t1, oprnd0);
	}
      else
	t2 = t1;

      if (post_shift)
	{
	  /* t3 = t2 >> post_shift;  */
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	  t3 = vect_recog_temp_ssa_var (itype, NULL);
	  def_stmt = gimple_build_assign (t3, RSHIFT_EXPR, t2,
					  build_int_cst (itype, post_shift));
	}
      else
	t3 = t2;

      int msb = 1;
      int_range_max r;
      get_range_query (cfun)->range_of_expr (r, oprnd0);
      if (!r.varying_p () && !r.undefined_p ())
	{
	  if (!wi::neg_p (r.lower_bound (), TYPE_SIGN (itype)))
	    msb = 0;
	  else if (wi::neg_p (r.upper_bound (), TYPE_SIGN (itype)))
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
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);
	  t4 = vect_recog_temp_ssa_var (itype, NULL);
	  if (msb != 1)
	    def_stmt = gimple_build_assign (t4, INTEGER_CST,
					    build_int_cst (itype, msb));
	  else
	    def_stmt = gimple_build_assign (t4, RSHIFT_EXPR, oprnd0,
					    build_int_cst (itype, prec - 1));
	  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

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
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt);

      t1 = vect_recog_temp_ssa_var (itype, NULL);
      def_stmt = gimple_build_assign (t1, MULT_EXPR, q, oprnd1);
      append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt);

      r = vect_recog_temp_ssa_var (itype, NULL);
      pattern_stmt = gimple_build_assign (r, MINUS_EXPR, oprnd0, t1);
    }

  /* Pattern detected.  */
  vect_pattern_detected ("vect_recog_divmod_pattern", last_stmt);

  *type_out = vectype;
  return pattern_stmt;
}

/* Detects pattern with a modulo operation (S1) where both arguments
   are variables of integral type.
   The statement is replaced by division, multiplication, and subtraction.
   The last statement (S4) is returned.

   Example:
   S1 c_t = a_t % b_t;

   is replaced by
   S2 x_t = a_t / b_t;
   S3 y_t = x_t * b_t;
   S4 z_t = a_t - y_t;  */

static gimple *
vect_recog_mod_var_pattern (vec_info *vinfo,
			    stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = STMT_VINFO_STMT (stmt_vinfo);
  tree oprnd0, oprnd1, vectype, itype;
  gimple *pattern_stmt, *def_stmt;
  enum tree_code rhs_code;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  rhs_code = gimple_assign_rhs_code (last_stmt);
  if (rhs_code != TRUNC_MOD_EXPR)
    return NULL;

  oprnd0 = gimple_assign_rhs1 (last_stmt);
  oprnd1 = gimple_assign_rhs2 (last_stmt);
  itype = TREE_TYPE (oprnd0);
  if (TREE_CODE (oprnd0) != SSA_NAME
      || TREE_CODE (oprnd1) != SSA_NAME
      || TREE_CODE (itype) != INTEGER_TYPE)
    return NULL;

  vectype = get_vectype_for_scalar_type (vinfo, itype);

  if (!vectype
      || target_has_vecop_for_code (TRUNC_MOD_EXPR, vectype)
      || !target_has_vecop_for_code (TRUNC_DIV_EXPR, vectype)
      || !target_has_vecop_for_code (MULT_EXPR, vectype)
      || !target_has_vecop_for_code (MINUS_EXPR, vectype))
    return NULL;

  tree q, tmp, r;
  q = vect_recog_temp_ssa_var (itype, NULL);
  def_stmt = gimple_build_assign (q, TRUNC_DIV_EXPR, oprnd0, oprnd1);
  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vectype);

  tmp = vect_recog_temp_ssa_var (itype, NULL);
  def_stmt = gimple_build_assign (tmp, MULT_EXPR, q, oprnd1);
  append_pattern_def_seq (vinfo, stmt_vinfo, def_stmt, vectype);

  r = vect_recog_temp_ssa_var (itype, NULL);
  pattern_stmt = gimple_build_assign (r, MINUS_EXPR, oprnd0, tmp);

  /* Pattern detected.  */
  *type_out = vectype;
  vect_pattern_detected ("vect_recog_mod_var_pattern", last_stmt);

  return pattern_stmt;
}


/* Return the proper type for converting bool VAR into
   an integer value or NULL_TREE if no such type exists.
   The type is chosen so that the converted value has the
   same number of elements as VAR's vector type.  */

static tree
integer_type_for_mask (tree var, vec_info *vinfo)
{
  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (var)))
    return NULL_TREE;

  stmt_vec_info def_stmt_info = vect_get_internal_def (vinfo, var);
  if (!def_stmt_info || !vect_use_mask_type_p (def_stmt_info))
    return NULL_TREE;

  return build_nonstandard_integer_type (def_stmt_info->mask_precision, 1);
}

/* Function vect_recog_gcond_pattern

   Try to find pattern like following:

     if (a op b)

   where operator 'op' is not != and convert it to an adjusted boolean pattern

     mask = a op b
     if (mask != 0)

   and set the mask type on MASK.

   Input:

   * STMT_VINFO: The stmt at the end from which the pattern
		 search begins, i.e. cast of a bool to
		 an integer type.

   Output:

   * TYPE_OUT: The type of the output of this pattern.

   * Return value: A new stmt that will be used to replace the pattern.  */

static gimple *
vect_recog_gcond_pattern (vec_info *vinfo,
			 stmt_vec_info stmt_vinfo, tree *type_out)
{
  /* Currently we only support this for loop vectorization and when multiple
     exits.  */
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  if (!loop_vinfo || !LOOP_VINFO_EARLY_BREAKS (loop_vinfo))
    return NULL;

  gimple *last_stmt = STMT_VINFO_STMT (stmt_vinfo);
  gcond* cond = NULL;
  if (!(cond = dyn_cast <gcond *> (last_stmt)))
    return NULL;

  auto lhs = gimple_cond_lhs (cond);
  auto rhs = gimple_cond_rhs (cond);
  auto code = gimple_cond_code (cond);

  tree scalar_type = TREE_TYPE (lhs);
  if (VECTOR_TYPE_P (scalar_type))
    return NULL;

  /* If the input is a boolean then try to figure out the precision that the
     vector type should use.  We cannot use the scalar precision as this would
     later mismatch.  This is similar to what recog_bool does.  */
  if (VECT_SCALAR_BOOLEAN_TYPE_P (scalar_type))
    {
      if (tree stype = integer_type_for_mask (lhs, vinfo))
	scalar_type = stype;
    }

  tree vectype = get_mask_type_for_scalar_type (vinfo, scalar_type);
  if (vectype == NULL_TREE)
    return NULL;

  tree new_lhs = vect_recog_temp_ssa_var (boolean_type_node, NULL);
  gimple *new_stmt = gimple_build_assign (new_lhs, code, lhs, rhs);
  append_pattern_def_seq (vinfo, stmt_vinfo, new_stmt, vectype, scalar_type);

  gimple *pattern_stmt
    = gimple_build_cond (NE_EXPR, new_lhs,
			 build_int_cst (TREE_TYPE (new_lhs), 0),
			 NULL_TREE, NULL_TREE);
  *type_out = vectype;
  vect_pattern_detected ("vect_recog_gcond_pattern", last_stmt);
  return pattern_stmt;
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

   * STMT_VINFO: The stmt at the end from which the pattern
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
vect_recog_bool_pattern (vec_info *vinfo,
			 stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  enum tree_code rhs_code;
  tree var, lhs, rhs, vectype;
  gimple *pattern_stmt;

  if (!is_gimple_assign (last_stmt))
    return NULL;

  var = gimple_assign_rhs1 (last_stmt);
  lhs = gimple_assign_lhs (last_stmt);
  rhs_code = gimple_assign_rhs_code (last_stmt);

  if (rhs_code == VIEW_CONVERT_EXPR)
    var = TREE_OPERAND (var, 0);

  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (var)))
    return NULL;

  hash_set<gimple *> bool_stmts;

  if (CONVERT_EXPR_CODE_P (rhs_code)
      || rhs_code == VIEW_CONVERT_EXPR)
    {
      if (! INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  || VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (lhs)))
	return NULL;
      vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (lhs));

      tree type = integer_type_for_mask (var, vinfo);
      tree cst0, cst1, tmp;

      if (!type)
	return NULL;

      /* We may directly use cond with narrowed type to avoid multiple cond
	 exprs with following result packing and perform single cond with
	 packed mask instead.  In case of widening we better make cond first
	 and then extract results.  */
      if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (lhs)))
	type = TREE_TYPE (lhs);

      cst0 = build_int_cst (type, 0);
      cst1 = build_int_cst (type, 1);
      tmp = vect_recog_temp_ssa_var (type, NULL);
      pattern_stmt = gimple_build_assign (tmp, COND_EXPR, var, cst1, cst0);

      if (!useless_type_conversion_p (type, TREE_TYPE (lhs)))
	{
	  tree new_vectype = get_vectype_for_scalar_type (vinfo, type);
	  append_pattern_def_seq (vinfo, stmt_vinfo,
				  pattern_stmt, new_vectype);

	  lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	  pattern_stmt = gimple_build_assign (lhs, CONVERT_EXPR, tmp);
	}

      *type_out = vectype;
      vect_pattern_detected ("vect_recog_bool_pattern", last_stmt);

      return pattern_stmt;
    }
  else if (rhs_code == COND_EXPR
	   && TREE_CODE (var) == SSA_NAME)
    {
      vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (lhs));
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
      if (get_vectype_for_scalar_type (vinfo, type) == NULL_TREE)
	return NULL;

      enum vect_def_type dt;
      if (integer_type_for_mask (var, vinfo))
	return NULL;
      else if (TREE_CODE (TREE_TYPE (var)) == BOOLEAN_TYPE
	       && vect_is_simple_use (var, vinfo, &dt)
	       && (dt == vect_external_def
		   || dt == vect_constant_def))
	{
	  /* If the condition is already a boolean then manually convert it to a
	     mask of the given integer type but don't set a vectype.  */
	  tree lhs_ivar = vect_recog_temp_ssa_var (type, NULL);
	  pattern_stmt = gimple_build_assign (lhs_ivar, COND_EXPR, var,
					      build_all_ones_cst (type),
					      build_zero_cst (type));
	  append_inv_pattern_def_seq (vinfo, pattern_stmt);
	  var = lhs_ivar;
	}

      tree lhs_var = vect_recog_temp_ssa_var (boolean_type_node, NULL);
      pattern_stmt = gimple_build_assign (lhs_var, NE_EXPR, var,
					  build_zero_cst (TREE_TYPE (var)));

      tree new_vectype = get_mask_type_for_scalar_type (vinfo, TREE_TYPE (var));
      if (!new_vectype)
	return NULL;

      new_vectype = truth_type_for (new_vectype);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, new_vectype,
			      TREE_TYPE (var));

      lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
      pattern_stmt
	= gimple_build_assign (lhs, COND_EXPR, lhs_var,
			       gimple_assign_rhs2 (last_stmt),
			       gimple_assign_rhs3 (last_stmt));
      *type_out = vectype;
      vect_pattern_detected ("vect_recog_bool_pattern", last_stmt);

      return pattern_stmt;
    }
  else if (rhs_code == SSA_NAME
	   && STMT_VINFO_DATA_REF (stmt_vinfo))
    {
      stmt_vec_info pattern_stmt_info;
      vectype = get_vectype_for_scalar_type (vinfo, TREE_TYPE (lhs));
      if (!vectype || !VECTOR_MODE_P (TYPE_MODE (vectype)))
	return NULL;

      tree type = integer_type_for_mask (var, vinfo);
      tree cst0, cst1, new_vectype;

      if (!type)
	return NULL;

      if (TYPE_MODE (type) == TYPE_MODE (TREE_TYPE (vectype)))
	type = TREE_TYPE (vectype);

      cst0 = build_int_cst (type, 0);
      cst1 = build_int_cst (type, 1);
      new_vectype = get_vectype_for_scalar_type (vinfo, type);

      rhs = vect_recog_temp_ssa_var (type, NULL);
      pattern_stmt = gimple_build_assign (rhs, COND_EXPR, var, cst1, cst0);
      append_pattern_def_seq (vinfo, stmt_vinfo, pattern_stmt, new_vectype);

      lhs = build1 (VIEW_CONVERT_EXPR, TREE_TYPE (vectype), lhs);
      if (!useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (rhs)))
	{
	  tree rhs2 = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	  gimple *cast_stmt = gimple_build_assign (rhs2, NOP_EXPR, rhs);
	  append_pattern_def_seq (vinfo, stmt_vinfo, cast_stmt);
	  rhs = rhs2;
	}
      pattern_stmt = gimple_build_assign (lhs, SSA_NAME, rhs);
      pattern_stmt_info = vinfo->add_stmt (pattern_stmt);
      vinfo->move_dr (pattern_stmt_info, stmt_vinfo);
      *type_out = vectype;
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
build_mask_conversion (vec_info *vinfo,
		       tree mask, tree vectype, stmt_vec_info stmt_vinfo)
{
  gimple *stmt;
  tree masktype, tmp;

  masktype = truth_type_for (vectype);
  tmp = vect_recog_temp_ssa_var (TREE_TYPE (masktype), NULL);
  stmt = gimple_build_assign (tmp, CONVERT_EXPR, mask);
  append_pattern_def_seq (vinfo, stmt_vinfo,
			  stmt, masktype, TREE_TYPE (vectype));

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
vect_recog_mask_conversion_pattern (vec_info *vinfo,
				    stmt_vec_info stmt_vinfo, tree *type_out)
{
  gimple *last_stmt = stmt_vinfo->stmt;
  enum tree_code rhs_code;
  tree lhs = NULL_TREE, rhs1, rhs2, tmp, rhs1_type, rhs2_type;
  tree vectype1, vectype2;
  stmt_vec_info pattern_stmt_info;

  /* Check for MASK_LOAD and MASK_STORE as well as COND_OP calls requiring mask
     conversion.  */
  if (is_gimple_call (last_stmt)
      && gimple_call_internal_p (last_stmt))
    {
      gcall *pattern_stmt;

      internal_fn ifn = gimple_call_internal_fn (last_stmt);
      int mask_argno = internal_fn_mask_index (ifn);
      if (mask_argno < 0)
	return NULL;

      bool store_p = internal_store_fn_p (ifn);
      bool load_p = internal_store_fn_p (ifn);
      if (store_p)
	{
	  int rhs_index = internal_fn_stored_value_index (ifn);
	  tree rhs = gimple_call_arg (last_stmt, rhs_index);
	  vectype1 = get_vectype_for_scalar_type (vinfo, TREE_TYPE (rhs));
	}
      else
	{
	  lhs = gimple_call_lhs (last_stmt);
	  if (!lhs)
	    return NULL;
	  vectype1 = get_vectype_for_scalar_type (vinfo, TREE_TYPE (lhs));
	}

      if (!vectype1)
	return NULL;

      tree mask_arg = gimple_call_arg (last_stmt, mask_argno);
      tree mask_arg_type = integer_type_for_mask (mask_arg, vinfo);
      if (mask_arg_type)
	{
	  vectype2 = get_mask_type_for_scalar_type (vinfo, mask_arg_type);

	  if (!vectype2
	      || known_eq (TYPE_VECTOR_SUBPARTS (vectype1),
			   TYPE_VECTOR_SUBPARTS (vectype2)))
	    return NULL;
	}
      else if (store_p || load_p)
	return NULL;

      tmp = build_mask_conversion (vinfo, mask_arg, vectype1, stmt_vinfo);

      auto_vec<tree, 8> args;
      unsigned int nargs = gimple_call_num_args (last_stmt);
      args.safe_grow (nargs, true);
      for (unsigned int i = 0; i < nargs; ++i)
	args[i] = ((int) i == mask_argno
		   ? tmp
		   : gimple_call_arg (last_stmt, i));
      pattern_stmt = gimple_build_call_internal_vec (ifn, args);

      if (!store_p)
	{
	  lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
	  gimple_call_set_lhs (pattern_stmt, lhs);
	}

      if (load_p || store_p)
	gimple_call_set_nothrow (pattern_stmt, true);

      pattern_stmt_info = vinfo->add_stmt (pattern_stmt);
      if (STMT_VINFO_DATA_REF (stmt_vinfo))
	vinfo->move_dr (pattern_stmt_info, stmt_vinfo);

      *type_out = vectype1;
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
      vectype1 = get_vectype_for_scalar_type (vinfo, TREE_TYPE (lhs));

      gcc_assert (! COMPARISON_CLASS_P (rhs1));
      if (TREE_CODE (rhs1) == SSA_NAME)
	{
	  rhs1_type = integer_type_for_mask (rhs1, vinfo);
	  if (!rhs1_type)
	    return NULL;
	}
      else
	return NULL;

      vectype2 = get_mask_type_for_scalar_type (vinfo, rhs1_type);

      if (!vectype1 || !vectype2)
	return NULL;

      /* Continue if a conversion is needed.  Also continue if we have
	 a comparison whose vector type would normally be different from
	 VECTYPE2 when considered in isolation.  In that case we'll
	 replace the comparison with an SSA name (so that we can record
	 its vector type) and behave as though the comparison was an SSA
	 name from the outset.  */
      if (known_eq (TYPE_VECTOR_SUBPARTS (vectype1),
		    TYPE_VECTOR_SUBPARTS (vectype2)))
	return NULL;

      if (maybe_ne (TYPE_VECTOR_SUBPARTS (vectype1),
		    TYPE_VECTOR_SUBPARTS (vectype2)))
	tmp = build_mask_conversion (vinfo, rhs1, vectype1, stmt_vinfo);
      else
	tmp = rhs1;

      lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
      pattern_stmt = gimple_build_assign (lhs, COND_EXPR, tmp,
					  gimple_assign_rhs2 (last_stmt),
					  gimple_assign_rhs3 (last_stmt));

      *type_out = vectype1;
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

  rhs1_type = integer_type_for_mask (rhs1, vinfo);
  rhs2_type = integer_type_for_mask (rhs2, vinfo);

  if (!rhs1_type || !rhs2_type
      || TYPE_PRECISION (rhs1_type) == TYPE_PRECISION (rhs2_type))
    return NULL;

  if (TYPE_PRECISION (rhs1_type) < TYPE_PRECISION (rhs2_type))
    {
      vectype1 = get_mask_type_for_scalar_type (vinfo, rhs1_type);
      if (!vectype1)
	return NULL;
      rhs2 = build_mask_conversion (vinfo, rhs2, vectype1, stmt_vinfo);
    }
  else
    {
      vectype1 = get_mask_type_for_scalar_type (vinfo, rhs2_type);
      if (!vectype1)
	return NULL;
      rhs1 = build_mask_conversion (vinfo, rhs1, vectype1, stmt_vinfo);
    }

  lhs = vect_recog_temp_ssa_var (TREE_TYPE (lhs), NULL);
  pattern_stmt = gimple_build_assign (lhs, rhs_code, rhs1, rhs2);

  *type_out = vectype1;
  vect_pattern_detected ("vect_recog_mask_conversion_pattern", last_stmt);

  return pattern_stmt;
}

/* STMT_INFO is a load or store.  If the load or store is conditional, return
   the boolean condition under which it occurs, otherwise return null.  */

static tree
vect_get_load_store_mask (stmt_vec_info stmt_info)
{
  if (gassign *def_assign = dyn_cast <gassign *> (stmt_info->stmt))
    {
      gcc_assert (gimple_assign_single_p (def_assign));
      return NULL_TREE;
    }

  if (gcall *def_call = dyn_cast <gcall *> (stmt_info->stmt))
    {
      internal_fn ifn = gimple_call_internal_fn (def_call);
      int mask_index = internal_fn_mask_index (ifn);
      return gimple_call_arg (def_call, mask_index);
    }

  gcc_unreachable ();
}

/* Return MASK if MASK is suitable for masking an operation on vectors
   of type VECTYPE, otherwise convert it into such a form and return
   the result.  Associate any conversion statements with STMT_INFO's
   pattern.  */

static tree
vect_convert_mask_for_vectype (tree mask, tree vectype,
			       stmt_vec_info stmt_info, vec_info *vinfo)
{
  tree mask_type = integer_type_for_mask (mask, vinfo);
  if (mask_type)
    {
      tree mask_vectype = get_mask_type_for_scalar_type (vinfo, mask_type);
      if (mask_vectype
	  && maybe_ne (TYPE_VECTOR_SUBPARTS (vectype),
		       TYPE_VECTOR_SUBPARTS (mask_vectype)))
	mask = build_mask_conversion (vinfo, mask, vectype, stmt_info);
    }
  return mask;
}

/* Return the equivalent of:

     fold_convert (TYPE, VALUE)

   with the expectation that the operation will be vectorized.
   If new statements are needed, add them as pattern statements
   to STMT_INFO.  */

static tree
vect_add_conversion_to_pattern (vec_info *vinfo,
				tree type, tree value, stmt_vec_info stmt_info)
{
  if (useless_type_conversion_p (type, TREE_TYPE (value)))
    return value;

  tree new_value = vect_recog_temp_ssa_var (type, NULL);
  gassign *conversion = gimple_build_assign (new_value, CONVERT_EXPR, value);
  append_pattern_def_seq (vinfo, stmt_info, conversion,
			  get_vectype_for_scalar_type (vinfo, type));
  return new_value;
}

/* Try to convert STMT_INFO into a call to a gather load or scatter store
   internal function.  Return the final statement on success and set
   *TYPE_OUT to the vector type being loaded or stored.

   This function only handles gathers and scatters that were recognized
   as such from the outset (indicated by STMT_VINFO_GATHER_SCATTER_P).  */

static gimple *
vect_recog_gather_scatter_pattern (vec_info *vinfo,
				   stmt_vec_info stmt_info, tree *type_out)
{
  /* Currently we only support this for loop vectorization.  */
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  if (!loop_vinfo)
    return NULL;

  /* Make sure that we're looking at a gather load or scatter store.  */
  data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  if (!dr || !STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    return NULL;

  /* Get the boolean that controls whether the load or store happens.
     This is null if the operation is unconditional.  */
  tree mask = vect_get_load_store_mask (stmt_info);

  /* Make sure that the target supports an appropriate internal
     function for the gather/scatter operation.  */
  gather_scatter_info gs_info;
  if (!vect_check_gather_scatter (stmt_info, loop_vinfo, &gs_info)
      || gs_info.ifn == IFN_LAST)
    return NULL;

  /* Convert the mask to the right form.  */
  tree gs_vectype = get_vectype_for_scalar_type (loop_vinfo,
						 gs_info.element_type);
  if (mask)
    mask = vect_convert_mask_for_vectype (mask, gs_vectype, stmt_info,
					  loop_vinfo);
  else if (gs_info.ifn == IFN_MASK_SCATTER_STORE
	   || gs_info.ifn == IFN_MASK_GATHER_LOAD
	   || gs_info.ifn == IFN_MASK_LEN_SCATTER_STORE
	   || gs_info.ifn == IFN_MASK_LEN_GATHER_LOAD)
    mask = build_int_cst (TREE_TYPE (truth_type_for (gs_vectype)), -1);

  /* Get the invariant base and non-invariant offset, converting the
     latter to the same width as the vector elements.  */
  tree base = gs_info.base;
  tree offset_type = TREE_TYPE (gs_info.offset_vectype);
  tree offset = vect_add_conversion_to_pattern (vinfo, offset_type,
						gs_info.offset, stmt_info);

  /* Build the new pattern statement.  */
  tree scale = size_int (gs_info.scale);
  gcall *pattern_stmt;

  if (DR_IS_READ (dr))
    {
      tree zero = build_zero_cst (gs_info.element_type);
      if (mask != NULL)
	{
	  int elsval = MASK_LOAD_ELSE_ZERO;

	  tree vec_els
	    = vect_get_mask_load_else (elsval, TREE_TYPE (gs_vectype));
	  pattern_stmt = gimple_build_call_internal (gs_info.ifn, 6, base,
						     offset, scale, zero, mask,
						     vec_els);
	}
      else
	pattern_stmt = gimple_build_call_internal (gs_info.ifn, 4, base,
						   offset, scale, zero);
      tree load_lhs = vect_recog_temp_ssa_var (gs_info.element_type, NULL);
      gimple_call_set_lhs (pattern_stmt, load_lhs);
    }
  else
    {
      tree rhs = vect_get_store_rhs (stmt_info);
      if (mask != NULL)
	pattern_stmt = gimple_build_call_internal (gs_info.ifn, 5,
						   base, offset, scale, rhs,
						   mask);
      else
	pattern_stmt = gimple_build_call_internal (gs_info.ifn, 4,
						   base, offset, scale, rhs);
    }
  gimple_call_set_nothrow (pattern_stmt, true);

  /* Copy across relevant vectorization info and associate DR with the
     new pattern statement instead of the original statement.  */
  stmt_vec_info pattern_stmt_info = loop_vinfo->add_stmt (pattern_stmt);
  loop_vinfo->move_dr (pattern_stmt_info, stmt_info);

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  *type_out = vectype;
  vect_pattern_detected ("gather/scatter pattern", stmt_info->stmt);

  return pattern_stmt;
}

/* Helper method of vect_recog_cond_store_pattern,  checks to see if COND_ARG
   is points to a load statement that reads the same data as that of
   STORE_VINFO.  */

static bool
vect_cond_store_pattern_same_ref (vec_info *vinfo,
				  stmt_vec_info store_vinfo, tree cond_arg)
{
  stmt_vec_info load_stmt_vinfo = vinfo->lookup_def (cond_arg);
  if (!load_stmt_vinfo
      || !STMT_VINFO_DATA_REF (load_stmt_vinfo)
      || DR_IS_WRITE (STMT_VINFO_DATA_REF (load_stmt_vinfo))
      || !same_data_refs (STMT_VINFO_DATA_REF (store_vinfo),
			  STMT_VINFO_DATA_REF (load_stmt_vinfo)))
    return false;

  return true;
}

/* Function vect_recog_cond_store_pattern

   Try to find the following pattern:

   x = *_3;
   c = a CMP b;
   y = c ? t_20 : x;
   *_3 = y;

   where the store of _3 happens on a conditional select on a value loaded
   from the same location.  In such case we can elide the initial load if
   MASK_STORE is supported and instead only conditionally write out the result.

   The pattern produces for the above:

   c = a CMP b;
   .MASK_STORE (_3, c, t_20)

   Input:

   * STMT_VINFO: The stmt from which the pattern search begins.  In the
   example, when this function is called with _3 then the search begins.

   Output:

   * TYPE_OUT: The type of the output  of this pattern.

   * Return value: A new stmt that will be used to replace the sequence.  */

static gimple *
vect_recog_cond_store_pattern (vec_info *vinfo,
			       stmt_vec_info stmt_vinfo, tree *type_out)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  if (!loop_vinfo)
    return NULL;

  gimple *store_stmt = STMT_VINFO_STMT (stmt_vinfo);

  /* Needs to be a gimple store where we have DR info for.  */
  if (!STMT_VINFO_DATA_REF (stmt_vinfo)
      || DR_IS_READ (STMT_VINFO_DATA_REF (stmt_vinfo))
      || !gimple_store_p (store_stmt))
    return NULL;

  tree st_rhs = gimple_assign_rhs1 (store_stmt);

  if (TREE_CODE (st_rhs) != SSA_NAME)
    return NULL;

  auto cond_vinfo = vinfo->lookup_def (st_rhs);

  /* If the condition isn't part of the loop then bool recog wouldn't have seen
     it and so this transformation may not be valid.  */
  if (!cond_vinfo)
    return NULL;

  cond_vinfo = vect_stmt_to_vectorize (cond_vinfo);
  gassign *cond_stmt = dyn_cast<gassign *> (STMT_VINFO_STMT (cond_vinfo));
  if (!cond_stmt || gimple_assign_rhs_code (cond_stmt) != COND_EXPR)
    return NULL;

  /* Check if the else value matches the original loaded one.  */
  bool invert = false;
  tree cmp_ls = gimple_arg (cond_stmt, 0);
  if (TREE_CODE (cmp_ls) != SSA_NAME)
    return NULL;

  tree cond_arg1 = gimple_arg (cond_stmt, 1);
  tree cond_arg2 = gimple_arg (cond_stmt, 2);

  if (!vect_cond_store_pattern_same_ref (vinfo, stmt_vinfo, cond_arg2)
      && !(invert = vect_cond_store_pattern_same_ref (vinfo, stmt_vinfo,
						      cond_arg1)))
    return NULL;

  vect_pattern_detected ("vect_recog_cond_store_pattern", store_stmt);

  tree scalar_type = TREE_TYPE (st_rhs);
  if (VECTOR_TYPE_P (scalar_type))
    return NULL;

  tree vectype = get_vectype_for_scalar_type (vinfo, scalar_type);
  if (vectype == NULL_TREE)
    return NULL;

  machine_mode mask_mode;
  machine_mode vecmode = TYPE_MODE (vectype);
  if (!VECTOR_MODE_P (vecmode)
      || targetm.vectorize.conditional_operation_is_expensive (IFN_MASK_STORE)
      || !targetm.vectorize.get_mask_mode (vecmode).exists (&mask_mode)
      || !can_vec_mask_load_store_p (vecmode, mask_mode, false))
    return NULL;

  tree base = DR_REF (STMT_VINFO_DATA_REF (stmt_vinfo));
  if (may_be_nonaddressable_p (base))
    return NULL;

  /* We need to use the false parameter of the conditional select.  */
  tree cond_store_arg = invert ? cond_arg2 : cond_arg1;
  tree cond_load_arg = invert ? cond_arg1 : cond_arg2;
  gimple *load_stmt = SSA_NAME_DEF_STMT (cond_load_arg);

  /* This is a rough estimation to check that there aren't any aliasing stores
     in between the load and store.  It's a bit strict, but for now it's good
     enough.  */
  if (gimple_vuse (load_stmt) != gimple_vuse (store_stmt))
    return NULL;

  /* If we have to invert the condition, i.e. use the true argument rather than
     the false argument, we have to negate the mask.  */
  if (invert)
    {
      tree var = vect_recog_temp_ssa_var (boolean_type_node, NULL);

      /* Invert the mask using ^ 1.  */
      tree itype = TREE_TYPE (cmp_ls);
      gassign *conv = gimple_build_assign (var, BIT_XOR_EXPR, cmp_ls,
					   build_int_cst (itype, 1));

      tree mask_vec_type = get_mask_type_for_scalar_type (vinfo, itype);
      append_pattern_def_seq (vinfo, stmt_vinfo, conv, mask_vec_type, itype);
      cmp_ls= var;
    }

  if (TREE_CODE (base) != MEM_REF)
   base = build_fold_addr_expr (base);

  tree ptr = build_int_cst (reference_alias_ptr_type (base),
			    get_object_alignment (base));

  /* Convert the mask to the right form.  */
  tree mask = vect_convert_mask_for_vectype (cmp_ls, vectype, stmt_vinfo,
					     vinfo);

  gcall *call
    = gimple_build_call_internal (IFN_MASK_STORE, 4, base, ptr, mask,
				  cond_store_arg);
  gimple_set_location (call, gimple_location (store_stmt));

  /* Copy across relevant vectorization info and associate DR with the
     new pattern statement instead of the original statement.  */
  stmt_vec_info pattern_stmt_info = loop_vinfo->add_stmt (call);
  loop_vinfo->move_dr (pattern_stmt_info, stmt_vinfo);

  *type_out = vectype;
  return call;
}

/* Return true if TYPE is a non-boolean integer type.  These are the types
   that we want to consider for narrowing.  */

static bool
vect_narrowable_type_p (tree type)
{
  return INTEGRAL_TYPE_P (type) && !VECT_SCALAR_BOOLEAN_TYPE_P (type);
}

/* Return true if the operation given by CODE can be truncated to N bits
   when only N bits of the output are needed.  This is only true if bit N+1
   of the inputs has no effect on the low N bits of the result.  */

static bool
vect_truncatable_operation_p (tree_code code)
{
  switch (code)
    {
    case NEGATE_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case BIT_NOT_EXPR:
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case COND_EXPR:
      return true;

    default:
      return false;
    }
}

/* Record that STMT_INFO could be changed from operating on TYPE to
   operating on a type with the precision and sign given by PRECISION
   and SIGN respectively.  PRECISION is an arbitrary bit precision;
   it might not be a whole number of bytes.  */

static void
vect_set_operation_type (stmt_vec_info stmt_info, tree type,
			 unsigned int precision, signop sign)
{
  /* Round the precision up to a whole number of bytes.  */
  precision = vect_element_precision (precision);
  if (precision < TYPE_PRECISION (type)
      && (!stmt_info->operation_precision
	  || stmt_info->operation_precision > precision))
    {
      stmt_info->operation_precision = precision;
      stmt_info->operation_sign = sign;
    }
}

/* Record that STMT_INFO only requires MIN_INPUT_PRECISION from its
   non-boolean inputs, all of which have type TYPE.  MIN_INPUT_PRECISION
   is an arbitrary bit precision; it might not be a whole number of bytes.  */

static void
vect_set_min_input_precision (stmt_vec_info stmt_info, tree type,
			      unsigned int min_input_precision)
{
  /* This operation in isolation only requires the inputs to have
     MIN_INPUT_PRECISION of precision,  However, that doesn't mean
     that MIN_INPUT_PRECISION is a natural precision for the chain
     as a whole.  E.g. consider something like:

	 unsigned short *x, *y;
	 *y = ((*x & 0xf0) >> 4) | (*y << 4);

     The right shift can be done on unsigned chars, and only requires the
     result of "*x & 0xf0" to be done on unsigned chars.  But taking that
     approach would mean turning a natural chain of single-vector unsigned
     short operations into one that truncates "*x" and then extends
     "(*x & 0xf0) >> 4", with two vectors for each unsigned short
     operation and one vector for each unsigned char operation.
     This would be a significant pessimization.

     Instead only propagate the maximum of this precision and the precision
     required by the users of the result.  This means that we don't pessimize
     the case above but continue to optimize things like:

	 unsigned char *y;
	 unsigned short *x;
	 *y = ((*x & 0xf0) >> 4) | (*y << 4);

     Here we would truncate two vectors of *x to a single vector of
     unsigned chars and use single-vector unsigned char operations for
     everything else, rather than doing two unsigned short copies of
     "(*x & 0xf0) >> 4" and then truncating the result.  */
  min_input_precision = MAX (min_input_precision,
			     stmt_info->min_output_precision);

  if (min_input_precision < TYPE_PRECISION (type)
      && (!stmt_info->min_input_precision
	  || stmt_info->min_input_precision > min_input_precision))
    stmt_info->min_input_precision = min_input_precision;
}

/* Subroutine of vect_determine_min_output_precision.  Return true if
   we can calculate a reduced number of output bits for STMT_INFO,
   whose result is LHS.  */

static bool
vect_determine_min_output_precision_1 (vec_info *vinfo,
				       stmt_vec_info stmt_info, tree lhs)
{
  /* Take the maximum precision required by users of the result.  */
  unsigned int precision = 0;
  imm_use_iterator iter;
  use_operand_p use;
  FOR_EACH_IMM_USE_FAST (use, iter, lhs)
    {
      gimple *use_stmt = USE_STMT (use);
      if (is_gimple_debug (use_stmt))
	continue;
      stmt_vec_info use_stmt_info = vinfo->lookup_stmt (use_stmt);
      if (!use_stmt_info || !use_stmt_info->min_input_precision)
	return false;
      /* The input precision recorded for COND_EXPRs applies only to the
	 "then" and "else" values.  */
      gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
      if (assign
	  && gimple_assign_rhs_code (assign) == COND_EXPR
	  && use->use != gimple_assign_rhs2_ptr (assign)
	  && use->use != gimple_assign_rhs3_ptr (assign))
	return false;
      precision = MAX (precision, use_stmt_info->min_input_precision);
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "only the low %d bits of %T are significant\n",
		     precision, lhs);
  stmt_info->min_output_precision = precision;
  return true;
}

/* Calculate min_output_precision for STMT_INFO.  */

static void
vect_determine_min_output_precision (vec_info *vinfo, stmt_vec_info stmt_info)
{
  /* We're only interested in statements with a narrowable result.  */
  tree lhs = gimple_get_lhs (stmt_info->stmt);
  if (!lhs
      || TREE_CODE (lhs) != SSA_NAME
      || !vect_narrowable_type_p (TREE_TYPE (lhs)))
    return;

  if (!vect_determine_min_output_precision_1 (vinfo, stmt_info, lhs))
    stmt_info->min_output_precision = TYPE_PRECISION (TREE_TYPE (lhs));
}

/* Use range information to decide whether STMT (described by STMT_INFO)
   could be done in a narrower type.  This is effectively a forward
   propagation, since it uses context-independent information that applies
   to all users of an SSA name.  */

static void
vect_determine_precisions_from_range (stmt_vec_info stmt_info, gassign *stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  if (!lhs || TREE_CODE (lhs) != SSA_NAME)
    return;

  tree type = TREE_TYPE (lhs);
  if (!vect_narrowable_type_p (type))
    return;

  /* First see whether we have any useful range information for the result.  */
  unsigned int precision = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  wide_int min_value, max_value;
  if (!vect_get_range_info (lhs, &min_value, &max_value))
    return;

  tree_code code = gimple_assign_rhs_code (stmt);
  unsigned int nops = gimple_num_ops (stmt);

  if (!vect_truncatable_operation_p (code))
    {
      /* Handle operations that can be computed in type T if all inputs
	 and outputs can be represented in type T.  Also handle left and
	 right shifts, where (in addition) the maximum shift amount must
	 be less than the number of bits in T.  */
      bool is_shift;
      switch (code)
	{
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	  is_shift = true;
	  break;

	case ABS_EXPR:
	case MIN_EXPR:
	case MAX_EXPR:
	case TRUNC_DIV_EXPR:
	case CEIL_DIV_EXPR:
	case FLOOR_DIV_EXPR:
	case ROUND_DIV_EXPR:
	case EXACT_DIV_EXPR:
	  /* Modulus is excluded because it is typically calculated by doing
	     a division, for which minimum signed / -1 isn't representable in
	     the original signed type.  We could take the division range into
	     account instead, if handling modulus ever becomes important.  */
	  is_shift = false;
	  break;

	default:
	  return;
	}
      for (unsigned int i = 1; i < nops; ++i)
	{
	  tree op = gimple_op (stmt, i);
	  wide_int op_min_value, op_max_value;
	  if (TREE_CODE (op) == INTEGER_CST)
	    {
	      unsigned int op_precision = TYPE_PRECISION (TREE_TYPE (op));
	      op_min_value = op_max_value = wi::to_wide (op, op_precision);
	    }
	  else if (TREE_CODE (op) == SSA_NAME)
	    {
	      if (!vect_get_range_info (op, &op_min_value, &op_max_value))
		return;
	    }
	  else
	    return;

	  if (is_shift && i == 2)
	    {
	      /* There needs to be one more bit than the maximum shift amount.

		 If the maximum shift amount is already 1 less than PRECISION
		 then we can't narrow the shift further.  Dealing with that
		 case first ensures that we can safely use an unsigned range
		 below.

		 op_min_value isn't relevant, since shifts by negative amounts
		 are UB.  */
	      if (wi::geu_p (op_max_value, precision - 1))
		return;
	      unsigned int min_bits = op_max_value.to_uhwi () + 1;

	      /* As explained below, we can convert a signed shift into an
		 unsigned shift if the sign bit is always clear.  At this
		 point we've already processed the ranges of the output and
		 the first input.  */
	      auto op_sign = sign;
	      if (sign == SIGNED && !wi::neg_p (min_value))
		op_sign = UNSIGNED;
	      op_min_value = wide_int::from (wi::min_value (min_bits, op_sign),
					     precision, op_sign);
	      op_max_value = wide_int::from (wi::max_value (min_bits, op_sign),
					     precision, op_sign);
	    }
	  min_value = wi::min (min_value, op_min_value, sign);
	  max_value = wi::max (max_value, op_max_value, sign);
	}
    }

  /* Try to switch signed types for unsigned types if we can.
     This is better for two reasons.  First, unsigned ops tend
     to be cheaper than signed ops.  Second, it means that we can
     handle things like:

	signed char c;
	int res = (int) c & 0xff00; // range [0x0000, 0xff00]

     as:

	signed char c;
	unsigned short res_1 = (unsigned short) c & 0xff00;
	int res = (int) res_1;

     where the intermediate result res_1 has unsigned rather than
     signed type.  */
  if (sign == SIGNED && !wi::neg_p (min_value))
    sign = UNSIGNED;

  /* See what precision is required for MIN_VALUE and MAX_VALUE.  */
  unsigned int precision1 = wi::min_precision (min_value, sign);
  unsigned int precision2 = wi::min_precision (max_value, sign);
  unsigned int value_precision = MAX (precision1, precision2);
  if (value_precision >= precision)
    return;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "can narrow to %s:%d"
		     " without loss of precision: %G",
		     sign == SIGNED ? "signed" : "unsigned",
		     value_precision, (gimple *) stmt);

  vect_set_operation_type (stmt_info, type, value_precision, sign);
  vect_set_min_input_precision (stmt_info, type, value_precision);
}

/* Use information about the users of STMT's result to decide whether
   STMT (described by STMT_INFO) could be done in a narrower type.
   This is effectively a backward propagation.  */

static void
vect_determine_precisions_from_users (stmt_vec_info stmt_info, gassign *stmt)
{
  tree_code code = gimple_assign_rhs_code (stmt);
  unsigned int opno = (code == COND_EXPR ? 2 : 1);
  tree type = TREE_TYPE (gimple_op (stmt, opno));
  if (!vect_narrowable_type_p (type))
    return;

  unsigned int precision = TYPE_PRECISION (type);
  unsigned int operation_precision, min_input_precision;
  switch (code)
    {
    CASE_CONVERT:
      /* Only the bits that contribute to the output matter.  Don't change
	 the precision of the operation itself.  */
      operation_precision = precision;
      min_input_precision = stmt_info->min_output_precision;
      break;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      {
	tree shift = gimple_assign_rhs2 (stmt);
	if (TREE_CODE (shift) != INTEGER_CST
	    || !wi::ltu_p (wi::to_widest (shift), precision))
	  return;
	unsigned int const_shift = TREE_INT_CST_LOW (shift);
	if (code == LSHIFT_EXPR)
	  {
	    /* Avoid creating an undefined shift.

	       ??? We could instead use min_output_precision as-is and
	       optimize out-of-range shifts to zero.  However, only
	       degenerate testcases shift away all their useful input data,
	       and it isn't natural to drop input operations in the middle
	       of vectorization.  This sort of thing should really be
	       handled before vectorization.  */
	    operation_precision = MAX (stmt_info->min_output_precision,
				       const_shift + 1);
	    /* We need CONST_SHIFT fewer bits of the input.  */
	    min_input_precision = (MAX (operation_precision, const_shift)
				   - const_shift);
	  }
	else
	  {
	    /* We need CONST_SHIFT extra bits to do the operation.  */
	    operation_precision = (stmt_info->min_output_precision
				   + const_shift);
	    min_input_precision = operation_precision;
	  }
	break;
      }

    default:
      if (vect_truncatable_operation_p (code))
	{
	  /* Input bit N has no effect on output bits N-1 and lower.  */
	  operation_precision = stmt_info->min_output_precision;
	  min_input_precision = operation_precision;
	  break;
	}
      return;
    }

  if (operation_precision < precision)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "can narrow to %s:%d"
			 " without affecting users: %G",
			 TYPE_UNSIGNED (type) ? "unsigned" : "signed",
			 operation_precision, (gimple *) stmt);
      vect_set_operation_type (stmt_info, type, operation_precision,
			       TYPE_SIGN (type));
    }
  vect_set_min_input_precision (stmt_info, type, min_input_precision);
}

/* Return true if the statement described by STMT_INFO sets a boolean
   SSA_NAME and if we know how to vectorize this kind of statement using
   vector mask types.  */

static bool
possible_vector_mask_operation_p (stmt_vec_info stmt_info)
{
  tree lhs = gimple_get_lhs (stmt_info->stmt);
  tree_code code = ERROR_MARK;
  gassign *assign = NULL;
  gcond *cond = NULL;

  if ((assign = dyn_cast <gassign *> (stmt_info->stmt)))
    code = gimple_assign_rhs_code (assign);
  else if ((cond = dyn_cast <gcond *> (stmt_info->stmt)))
    {
      lhs = gimple_cond_lhs (cond);
      code = gimple_cond_code (cond);
    }

  if (!lhs
      || TREE_CODE (lhs) != SSA_NAME
      || !VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (lhs)))
    return false;

  if (code != ERROR_MARK)
    {
      switch (code)
	{
	CASE_CONVERT:
	case SSA_NAME:
	case BIT_NOT_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case BIT_AND_EXPR:
	  return true;

	default:
	  return TREE_CODE_CLASS (code) == tcc_comparison;
	}
    }
  else if (is_a <gphi *> (stmt_info->stmt))
    return true;
  return false;
}

/* If STMT_INFO sets a boolean SSA_NAME, see whether we should use
   a vector mask type instead of a normal vector type.  Record the
   result in STMT_INFO->mask_precision.  */

static void
vect_determine_mask_precision (vec_info *vinfo, stmt_vec_info stmt_info)
{
  if (!possible_vector_mask_operation_p (stmt_info))
    return;

  /* If at least one boolean input uses a vector mask type,
     pick the mask type with the narrowest elements.

     ??? This is the traditional behavior.  It should always produce
     the smallest number of operations, but isn't necessarily the
     optimal choice.  For example, if we have:

       a = b & c

     where:

       - the user of a wants it to have a mask type for 16-bit elements (M16)
       - b also uses M16
       - c uses a mask type for 8-bit elements (M8)

     then picking M8 gives:

       - 1 M16->M8 pack for b
       - 1 M8 AND for a
       - 2 M8->M16 unpacks for the user of a

     whereas picking M16 would have given:

       - 2 M8->M16 unpacks for c
       - 2 M16 ANDs for a

     The number of operations are equal, but M16 would have given
     a shorter dependency chain and allowed more ILP.  */
  unsigned int precision = ~0U;
  gimple *stmt = STMT_VINFO_STMT (stmt_info);

  /* If the statement compares two values that shouldn't use vector masks,
     try comparing the values as normal scalars instead.  */
  tree_code code = ERROR_MARK;
  tree op0_type;
  unsigned int nops = -1;
  unsigned int ops_start = 0;

  if (gassign *assign = dyn_cast <gassign *> (stmt))
    {
      code = gimple_assign_rhs_code (assign);
      op0_type = TREE_TYPE (gimple_assign_rhs1 (assign));
      nops = gimple_num_ops (assign);
      ops_start = 1;
    }
  else if (gcond *cond = dyn_cast <gcond *> (stmt))
    {
      code = gimple_cond_code (cond);
      op0_type = TREE_TYPE (gimple_cond_lhs (cond));
      nops = 2;
      ops_start = 0;
    }

  if (code != ERROR_MARK)
    {
      for (unsigned int i = ops_start; i < nops; ++i)
	{
	  tree rhs = gimple_op (stmt, i);
	  if (!VECT_SCALAR_BOOLEAN_TYPE_P (TREE_TYPE (rhs)))
	    continue;

	  stmt_vec_info def_stmt_info = vinfo->lookup_def (rhs);
	  if (!def_stmt_info)
	    /* Don't let external or constant operands influence the choice.
	       We can convert them to whichever vector type we pick.  */
	    continue;

	  if (def_stmt_info->mask_precision)
	    {
	      if (precision > def_stmt_info->mask_precision)
		precision = def_stmt_info->mask_precision;
	    }
	}

      if (precision == ~0U
	  && TREE_CODE_CLASS (code) == tcc_comparison)
	{
	  scalar_mode mode;
	  tree vectype, mask_type;
	  if (is_a <scalar_mode> (TYPE_MODE (op0_type), &mode)
	      && (vectype = get_vectype_for_scalar_type (vinfo, op0_type))
	      && (mask_type = get_mask_type_for_scalar_type (vinfo, op0_type))
	      && expand_vec_cmp_expr_p (vectype, mask_type, code))
	    precision = GET_MODE_BITSIZE (mode);
	}
    }
  else
    {
      gphi *phi = as_a <gphi *> (stmt_info->stmt);
      for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	{
	  tree rhs = gimple_phi_arg_def (phi, i);

	  stmt_vec_info def_stmt_info = vinfo->lookup_def (rhs);
	  if (!def_stmt_info)
	    /* Don't let external or constant operands influence the choice.
	       We can convert them to whichever vector type we pick.  */
	    continue;

	  if (def_stmt_info->mask_precision)
	    {
	      if (precision > def_stmt_info->mask_precision)
		precision = def_stmt_info->mask_precision;
	    }
	}
    }

  if (dump_enabled_p ())
    {
      if (precision == ~0U)
	dump_printf_loc (MSG_NOTE, vect_location,
			 "using normal nonmask vectors for %G",
			 stmt_info->stmt);
      else
	dump_printf_loc (MSG_NOTE, vect_location,
			 "using boolean precision %d for %G",
			 precision, stmt_info->stmt);
    }

  stmt_info->mask_precision = precision;
}

/* Handle vect_determine_precisions for STMT_INFO, given that we
   have already done so for the users of its result.  */

void
vect_determine_stmt_precisions (vec_info *vinfo, stmt_vec_info stmt_info)
{
  vect_determine_min_output_precision (vinfo, stmt_info);
  if (gassign *stmt = dyn_cast <gassign *> (stmt_info->stmt))
    {
      vect_determine_precisions_from_range (stmt_info, stmt);
      vect_determine_precisions_from_users (stmt_info, stmt);
    }
}

/* Walk backwards through the vectorizable region to determine the
   values of these fields:

   - min_output_precision
   - min_input_precision
   - operation_precision
   - operation_sign.  */

void
vect_determine_precisions (vec_info *vinfo)
{
  basic_block *bbs = vinfo->bbs;
  unsigned int nbbs = vinfo->nbbs;

  DUMP_VECT_SCOPE ("vect_determine_precisions");

  for (unsigned int i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      for (auto gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi.phi ());
	  if (stmt_info && STMT_VINFO_VECTORIZABLE (stmt_info))
	    vect_determine_mask_precision (vinfo, stmt_info);
	}
      for (auto gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (gsi));
	  if (stmt_info && STMT_VINFO_VECTORIZABLE (stmt_info))
	    vect_determine_mask_precision (vinfo, stmt_info);
	}
    }
  for (unsigned int i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[nbbs - i - 1];
      for (auto gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	{
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (gsi));
	  if (stmt_info && STMT_VINFO_VECTORIZABLE (stmt_info))
	    vect_determine_stmt_precisions (vinfo, stmt_info);
	}
      for (auto gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi.phi ());
	  if (stmt_info && STMT_VINFO_VECTORIZABLE (stmt_info))
	    vect_determine_stmt_precisions (vinfo, stmt_info);
	}
    }
}

typedef gimple *(*vect_recog_func_ptr) (vec_info *, stmt_vec_info, tree *);

struct vect_recog_func
{
  vect_recog_func_ptr fn;
  const char *name;
};

/* Note that ordering matters - the first pattern matching on a stmt is
   taken which means usually the more complex one needs to preceed the
   less comples onex (widen_sum only after dot_prod or sad for example).  */
static vect_recog_func vect_vect_recog_func_ptrs[] = {
  { vect_recog_bitfield_ref_pattern, "bitfield_ref" },
  { vect_recog_bit_insert_pattern, "bit_insert" },
  { vect_recog_abd_pattern, "abd" },
  { vect_recog_over_widening_pattern, "over_widening" },
  /* Must come after over_widening, which narrows the shift as much as
     possible beforehand.  */
  { vect_recog_average_pattern, "average" },
  { vect_recog_cond_expr_convert_pattern, "cond_expr_convert" },
  { vect_recog_mulhs_pattern, "mult_high" },
  { vect_recog_cast_forwprop_pattern, "cast_forwprop" },
  { vect_recog_widen_mult_pattern, "widen_mult" },
  { vect_recog_dot_prod_pattern, "dot_prod" },
  { vect_recog_sad_pattern, "sad" },
  { vect_recog_widen_sum_pattern, "widen_sum" },
  { vect_recog_pow_pattern, "pow" },
  { vect_recog_popcount_clz_ctz_ffs_pattern, "popcount_clz_ctz_ffs" },
  { vect_recog_ctz_ffs_pattern, "ctz_ffs" },
  { vect_recog_widen_shift_pattern, "widen_shift" },
  { vect_recog_rotate_pattern, "rotate" },
  { vect_recog_vector_vector_shift_pattern, "vector_vector_shift" },
  { vect_recog_divmod_pattern, "divmod" },
  { vect_recog_mod_var_pattern, "modvar" },
  { vect_recog_mult_pattern, "mult" },
  { vect_recog_sat_add_pattern, "sat_add" },
  { vect_recog_sat_sub_pattern, "sat_sub" },
  { vect_recog_sat_trunc_pattern, "sat_trunc" },
  { vect_recog_gcond_pattern, "gcond" },
  { vect_recog_bool_pattern, "bool" },
  /* This must come before mask conversion, and includes the parts
     of mask conversion that are needed for gather and scatter
     internal functions.  */
  { vect_recog_gather_scatter_pattern, "gather_scatter" },
  { vect_recog_cond_store_pattern, "cond_store" },
  { vect_recog_mask_conversion_pattern, "mask_conversion" },
  { vect_recog_widen_plus_pattern, "widen_plus" },
  { vect_recog_widen_minus_pattern, "widen_minus" },
  { vect_recog_widen_abd_pattern, "widen_abd" },
  /* These must come after the double widening ones.  */
};

/* Mark statements that are involved in a pattern.  */

void
vect_mark_pattern_stmts (vec_info *vinfo,
			 stmt_vec_info orig_stmt_info, gimple *pattern_stmt,
                         tree pattern_vectype)
{
  stmt_vec_info orig_stmt_info_saved = orig_stmt_info;
  gimple *def_seq = STMT_VINFO_PATTERN_DEF_SEQ (orig_stmt_info);

  gimple *orig_pattern_stmt = NULL;
  if (is_pattern_stmt_p (orig_stmt_info))
    {
      /* We're replacing a statement in an existing pattern definition
	 sequence.  */
      orig_pattern_stmt = orig_stmt_info->stmt;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "replacing earlier pattern %G", orig_pattern_stmt);

      /* To keep the book-keeping simple, just swap the lhs of the
	 old and new statements, so that the old one has a valid but
	 unused lhs.  */
      tree old_lhs = gimple_get_lhs (orig_pattern_stmt);
      gimple_set_lhs (orig_pattern_stmt, gimple_get_lhs (pattern_stmt));
      gimple_set_lhs (pattern_stmt, old_lhs);

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location, "with %G", pattern_stmt);

      /* Switch to the statement that ORIG replaces.  */
      orig_stmt_info = STMT_VINFO_RELATED_STMT (orig_stmt_info);

      /* We shouldn't be replacing the main pattern statement.  */
      gcc_assert (STMT_VINFO_RELATED_STMT (orig_stmt_info)->stmt
		  != orig_pattern_stmt);
    }

  if (def_seq)
    for (gimple_stmt_iterator si = gsi_start (def_seq);
	 !gsi_end_p (si); gsi_next (&si))
      {
	if (dump_enabled_p ())
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "extra pattern stmt: %G", gsi_stmt (si));
	stmt_vec_info pattern_stmt_info
	  = vect_init_pattern_stmt (vinfo, gsi_stmt (si),
				    orig_stmt_info, pattern_vectype);
	/* Stmts in the def sequence are not vectorizable cycle or
	   induction defs, instead they should all be vect_internal_def
	   feeding the main pattern stmt which retains this def type.  */
	STMT_VINFO_DEF_TYPE (pattern_stmt_info) = vect_internal_def;
      }

  if (orig_pattern_stmt)
    {
      vect_init_pattern_stmt (vinfo, pattern_stmt,
			      orig_stmt_info, pattern_vectype);

      /* Insert all the new pattern statements before the original one.  */
      gimple_seq *orig_def_seq = &STMT_VINFO_PATTERN_DEF_SEQ (orig_stmt_info);
      gimple_stmt_iterator gsi = gsi_for_stmt (orig_pattern_stmt,
					       orig_def_seq);
      gsi_insert_seq_before_without_update (&gsi, def_seq, GSI_SAME_STMT);
      gsi_insert_before_without_update (&gsi, pattern_stmt, GSI_SAME_STMT);

      /* Remove the pattern statement that this new pattern replaces.  */
      gsi_remove (&gsi, false);
    }
  else
    vect_set_pattern_stmt (vinfo,
			   pattern_stmt, orig_stmt_info, pattern_vectype);

  /* For any conditionals mark them as vect_condition_def.  */
  if (is_a <gcond *> (pattern_stmt))
    STMT_VINFO_DEF_TYPE (STMT_VINFO_RELATED_STMT (orig_stmt_info)) = vect_condition_def;

  /* Transfer reduction path info to the pattern.  */
  if (STMT_VINFO_REDUC_IDX (orig_stmt_info_saved) != -1)
    {
      gimple_match_op op;
      if (!gimple_extract_op (orig_stmt_info_saved->stmt, &op))
	gcc_unreachable ();
      tree lookfor = op.ops[STMT_VINFO_REDUC_IDX (orig_stmt_info)];
      /* Search the pattern def sequence and the main pattern stmt.  Note
         we may have inserted all into a containing pattern def sequence
	 so the following is a bit awkward.  */
      gimple_stmt_iterator si;
      gimple *s;
      if (def_seq)
	{
	  si = gsi_start (def_seq);
	  s = gsi_stmt (si);
	  gsi_next (&si);
	}
      else
	{
	  si = gsi_none ();
	  s = pattern_stmt;
	}
      do
	{
	  bool found = false;
	  if (gimple_extract_op (s, &op))
	    for (unsigned i = 0; i < op.num_ops; ++i)
	      if (op.ops[i] == lookfor)
		{
		  STMT_VINFO_REDUC_IDX (vinfo->lookup_stmt (s)) = i;
		  lookfor = gimple_get_lhs (s);
		  found = true;
		  break;
		}
	  if (s == pattern_stmt)
	    {
	      if (!found && dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "failed to update reduction index.\n");
	      break;
	    }
	  if (gsi_end_p (si))
	    s = pattern_stmt;
	  else
	    {
	      s = gsi_stmt (si);
	      if (s == pattern_stmt)
		/* Found the end inside a bigger pattern def seq.  */
		si = gsi_none ();
	      else
		gsi_next (&si);
	    }
	} while (1);
    }
}

/* Function vect_pattern_recog_1

   Input:
   PATTERN_RECOG_FUNC: A pointer to a function that detects a certain
        computation pattern.
   STMT_INFO: A stmt from which the pattern search should start.

   If PATTERN_RECOG_FUNC successfully detected the pattern, it creates
   a sequence of statements that has the same functionality and can be
   used to replace STMT_INFO.  It returns the last statement in the sequence
   and adds any earlier statements to STMT_INFO's STMT_VINFO_PATTERN_DEF_SEQ.
   PATTERN_RECOG_FUNC also sets *TYPE_OUT to the vector type of the final
   statement, having first checked that the target supports the new operation
   in that type.

   This function also does some bookkeeping, as explained in the documentation
   for vect_recog_pattern.  */

static void
vect_pattern_recog_1 (vec_info *vinfo,
		      const vect_recog_func &recog_func, stmt_vec_info stmt_info)
{
  gimple *pattern_stmt;
  tree pattern_vectype;

  /* If this statement has already been replaced with pattern statements,
     leave the original statement alone, since the first match wins.
     Instead try to match against the definition statements that feed
     the main pattern statement.  */
  if (STMT_VINFO_IN_PATTERN_P (stmt_info))
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start (STMT_VINFO_PATTERN_DEF_SEQ (stmt_info));
	   !gsi_end_p (gsi); gsi_next (&gsi))
	vect_pattern_recog_1 (vinfo, recog_func,
			      vinfo->lookup_stmt (gsi_stmt (gsi)));
      return;
    }

  gcc_assert (!STMT_VINFO_PATTERN_DEF_SEQ (stmt_info));
  pattern_stmt = recog_func.fn (vinfo, stmt_info, &pattern_vectype);
  if (!pattern_stmt)
    {
      /* Clear any half-formed pattern definition sequence.  */
      STMT_VINFO_PATTERN_DEF_SEQ (stmt_info) = NULL;
      return;
    }

  /* Found a vectorizable pattern.  */
  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "%s pattern recognized: %G",
		     recog_func.name, pattern_stmt);

  /* Mark the stmts that are involved in the pattern. */
  vect_mark_pattern_stmts (vinfo, stmt_info, pattern_stmt, pattern_vectype);
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
  basic_block *bbs = vinfo->bbs;
  unsigned int nbbs = vinfo->nbbs;

  vect_determine_precisions (vinfo);

  DUMP_VECT_SCOPE ("vect_pattern_recog");

  /* Scan through the stmts in the region, applying the pattern recognition
     functions starting at each stmt visited.  */
  for (unsigned i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (gsi_stmt (si));

	  if (!stmt_info || !STMT_VINFO_VECTORIZABLE (stmt_info))
	    continue;

	  /* Scan over all generic vect_recog_xxx_pattern functions.  */
	  for (const auto &func_ptr : vect_vect_recog_func_ptrs)
	    vect_pattern_recog_1 (vinfo, func_ptr,
				  stmt_info);
	}
    }

  /* After this no more add_stmt calls are allowed.  */
  vinfo->stmt_vec_info_ro = true;
}

/* Build a GIMPLE_ASSIGN or GIMPLE_CALL with the tree_code,
   or internal_fn contained in ch, respectively.  */
gimple *
vect_gimple_build (tree lhs, code_helper ch, tree op0, tree op1)
{
  gcc_assert (op0 != NULL_TREE);
  if (ch.is_tree_code ())
    return gimple_build_assign (lhs, (tree_code) ch, op0, op1);

  gcc_assert (ch.is_internal_fn ());
  gimple* stmt = gimple_build_call_internal (as_internal_fn ((combined_fn) ch),
					     op1 == NULL_TREE ? 1 : 2,
					     op0, op1);
  gimple_call_set_lhs (stmt, lhs);
  return stmt;
}
