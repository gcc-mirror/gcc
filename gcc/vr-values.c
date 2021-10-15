/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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
#include "insn-codes.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "intl.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "tree-chrec.h"
#include "omp-general.h"
#include "case-cfn-macros.h"
#include "alloc-pool.h"
#include "attribs.h"
#include "range.h"
#include "vr-values.h"
#include "cfghooks.h"
#include "range-op.h"
#include "gimple-range.h"

/* Set value range VR to a non-negative range of type TYPE.  */

static inline void
set_value_range_to_nonnegative (value_range_equiv *vr, tree type)
{
  tree zero = build_int_cst (type, 0);
  vr->update (zero, vrp_val_max (type));
}

/* Set value range VR to a range of a truthvalue of type TYPE.  */

static inline void
set_value_range_to_truthvalue (value_range_equiv *vr, tree type)
{
  if (TYPE_PRECISION (type) == 1)
    vr->set_varying (type);
  else
    vr->update (build_int_cst (type, 0), build_int_cst (type, 1));
}

/* Return the lattice entry for VAR or NULL if it doesn't exist or cannot
   be initialized.  */

value_range_equiv *
vr_values::get_lattice_entry (const_tree var)
{
  value_range_equiv *vr;
  tree sym;
  unsigned ver = SSA_NAME_VERSION (var);

  /* If we query the entry for a new SSA name avoid reallocating the lattice
     since we should get here at most from the substitute-and-fold stage which
     will never try to change values.  */
  if (ver >= num_vr_values)
    return NULL;

  vr = vr_value[ver];
  if (vr)
    return vr;

  /* Create a default value range.  */
  vr = allocate_value_range_equiv ();
  vr_value[ver] = vr;

  /* After propagation finished return varying.  */
  if (values_propagated)
    {
      vr->set_varying (TREE_TYPE (var));
      return vr;
    }

  vr->set_undefined ();

  /* If VAR is a default definition of a parameter, the variable can
     take any value in VAR's type.  */
  if (SSA_NAME_IS_DEFAULT_DEF (var))
    {
      sym = SSA_NAME_VAR (var);
      if (TREE_CODE (sym) == PARM_DECL)
	{
	  /* Try to use the "nonnull" attribute to create ~[0, 0]
	     anti-ranges for pointers.  Note that this is only valid with
	     default definitions of PARM_DECLs.  */
	  if (POINTER_TYPE_P (TREE_TYPE (sym))
	      && (nonnull_arg_p (sym)
		  || (get_global_range_query ()->range_of_expr (*vr,
						const_cast <tree> (var))
		      && vr->nonzero_p ())))
	    {
	      vr->set_nonzero (TREE_TYPE (sym));
	      vr->equiv_clear ();
	    }
	  else if (INTEGRAL_TYPE_P (TREE_TYPE (sym)))
	    {
	      get_global_range_query ()->range_of_expr (*vr, const_cast <tree> (var));
	      if (vr->undefined_p ())
		vr->set_varying (TREE_TYPE (sym));
	    }
	  else
	    vr->set_varying (TREE_TYPE (sym));
	}
      else if (TREE_CODE (sym) == RESULT_DECL
	       && DECL_BY_REFERENCE (sym))
	{
	  vr->set_nonzero (TREE_TYPE (sym));
	  vr->equiv_clear ();
	}
    }

  return vr;
}

/* Return value range information for VAR.

   If we have no values ranges recorded (ie, VRP is not running), then
   return NULL.  Otherwise create an empty range if none existed for VAR.  */

const value_range_equiv *
vr_values::get_value_range (const_tree var,
			    gimple *stmt ATTRIBUTE_UNUSED)
{
  /* If we have no recorded ranges, then return NULL.  */
  if (!vr_value)
    return NULL;

  value_range_equiv *vr = get_lattice_entry (var);

  /* Reallocate the lattice if needed.  */
  if (!vr)
    {
      unsigned int old_sz = num_vr_values;
      num_vr_values = num_ssa_names + num_ssa_names / 10;
      vr_value = XRESIZEVEC (value_range_equiv *, vr_value, num_vr_values);
      for ( ; old_sz < num_vr_values; old_sz++)
        vr_value [old_sz] = NULL;

      /* Now that the lattice has been resized, we should never fail.  */
      vr = get_lattice_entry (var);
      gcc_assert (vr);
    }

  return vr;
}

bool
vr_values::range_of_expr (irange &r, tree expr, gimple *stmt)
{
  if (!gimple_range_ssa_p (expr))
    return get_tree_range (r, expr, stmt);

  if (const value_range *vr = get_value_range (expr, stmt))
    {
      if (vr->undefined_p () || vr->constant_p ())
	r = *vr;
      else
	{
	  value_range tmp = *vr;
	  tmp.normalize_symbolics ();
	  r = tmp;
	}
      return true;
    }
  return false;
}

tree
vr_values::value_of_expr (tree op, gimple *)
{
  return op_with_constant_singleton_value_range (op);
}

tree
vr_values::value_on_edge (edge, tree op)
{
  return op_with_constant_singleton_value_range (op);
}

tree
vr_values::value_of_stmt (gimple *stmt, tree op)
{
  if (!op)
    op = gimple_get_lhs (stmt);

  gcc_checking_assert (!op|| op == gimple_get_lhs (stmt));

  if (op)
    return op_with_constant_singleton_value_range (op);
  return NULL_TREE;
}

/* Set the lattice entry for DEF to VARYING.  */

void
vr_values::set_def_to_varying (const_tree def)
{
  value_range_equiv *vr = get_lattice_entry (def);
  if (vr)
    vr->set_varying (TREE_TYPE (def));
}

/* Set value-ranges of all SSA names defined by STMT to varying.  */

void
vr_values::set_defs_to_varying (gimple *stmt)
{
  ssa_op_iter i;
  tree def;
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, i, SSA_OP_DEF)
    set_def_to_varying (def);
}

/* Update the value range and equivalence set for variable VAR to
   NEW_VR.  Return true if NEW_VR is different from VAR's previous
   value.

   NOTE: This function assumes that NEW_VR is a temporary value range
   object created for the sole purpose of updating VAR's range.  The
   storage used by the equivalence set from NEW_VR will be freed by
   this function.  Do not call update_value_range when NEW_VR
   is the range object associated with another SSA name.  */

bool
vr_values::update_value_range (const_tree var, value_range_equiv *new_vr)
{
  value_range_equiv *old_vr;
  bool is_new;

  /* If there is a value-range on the SSA name from earlier analysis
     factor that in.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (var)))
    {
      value_range_equiv nr;
      get_global_range_query ()->range_of_expr (nr, const_cast <tree> (var));
      if (!nr.undefined_p ())
	new_vr->intersect (&nr);
    }

  /* Update the value range, if necessary.  If we cannot allocate a lattice
     entry for VAR keep it at VARYING.  This happens when DOM feeds us stmts
     with SSA names allocated after setting up the lattice.  */
  old_vr = get_lattice_entry (var);
  if (!old_vr)
    return false;
  is_new = !old_vr->equal_p (*new_vr, /*ignore_equivs=*/false);

  if (is_new)
    {
      /* Do not allow transitions up the lattice.  The following
	 is slightly more awkward than just new_vr->type < old_vr->type
	 because VR_RANGE and VR_ANTI_RANGE need to be considered
	 the same.  We may not have is_new when transitioning to
	 UNDEFINED.  If old_vr->type is VARYING, we shouldn't be
	 called, if we are anyway, keep it VARYING.  */
      if (old_vr->varying_p ())
	{
	  new_vr->set_varying (TREE_TYPE (var));
	  is_new = false;
	}
      else if (new_vr->undefined_p ())
	{
	  old_vr->set_varying (TREE_TYPE (var));
	  new_vr->set_varying (TREE_TYPE (var));
	  return true;
	}
      else
	old_vr->set (new_vr->min (), new_vr->max (), new_vr->equiv (),
		     new_vr->kind ());
    }

  new_vr->equiv_clear ();

  return is_new;
}

/* Return true if value range VR involves exactly one symbol SYM.  */

static bool
symbolic_range_based_on_p (value_range *vr, const_tree sym)
{
  bool neg, min_has_symbol, max_has_symbol;
  tree inv;

  if (is_gimple_min_invariant (vr->min ()))
    min_has_symbol = false;
  else if (get_single_symbol (vr->min (), &neg, &inv) == sym)
    min_has_symbol = true;
  else
    return false;

  if (is_gimple_min_invariant (vr->max ()))
    max_has_symbol = false;
  else if (get_single_symbol (vr->max (), &neg, &inv) == sym)
    max_has_symbol = true;
  else
    return false;

  return (min_has_symbol || max_has_symbol);
}

/* Return true if the result of assignment STMT is know to be non-zero.  */

static bool
gimple_assign_nonzero_p (gimple *stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  bool strict_overflow_p;
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  switch (get_gimple_rhs_class (code))
    {
    case GIMPLE_UNARY_RHS:
      return tree_unary_nonzero_warnv_p (gimple_assign_rhs_code (stmt),
					 type,
					 gimple_assign_rhs1 (stmt),
					 &strict_overflow_p);
    case GIMPLE_BINARY_RHS:
      return tree_binary_nonzero_warnv_p (gimple_assign_rhs_code (stmt),
					  type,
					  gimple_assign_rhs1 (stmt),
					  gimple_assign_rhs2 (stmt),
					  &strict_overflow_p);
    case GIMPLE_TERNARY_RHS:
      return false;
    case GIMPLE_SINGLE_RHS:
      return tree_single_nonzero_warnv_p (gimple_assign_rhs1 (stmt),
					  &strict_overflow_p);
    case GIMPLE_INVALID_RHS:
      gcc_unreachable ();
    default:
      gcc_unreachable ();
    }
}

/* Return true if STMT is known to compute a non-zero value.  */

static bool
gimple_stmt_nonzero_p (gimple *stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      return gimple_assign_nonzero_p (stmt);
    case GIMPLE_CALL:
      {
        gcall *call_stmt = as_a<gcall *> (stmt);
	return (gimple_call_nonnull_result_p (call_stmt)
		|| gimple_call_nonnull_arg (call_stmt));
      }
    default:
      gcc_unreachable ();
    }
}
/* Like tree_expr_nonzero_p, but this function uses value ranges
   obtained so far.  */

bool
vr_values::vrp_stmt_computes_nonzero (gimple *stmt)
{
  if (gimple_stmt_nonzero_p (stmt))
    return true;

  /* If we have an expression of the form &X->a, then the expression
     is nonnull if X is nonnull.  */
  if (is_gimple_assign (stmt)
      && gimple_assign_rhs_code (stmt) == ADDR_EXPR)
    {
      tree expr = gimple_assign_rhs1 (stmt);
      poly_int64 bitsize, bitpos;
      tree offset;
      machine_mode mode;
      int unsignedp, reversep, volatilep;
      tree base = get_inner_reference (TREE_OPERAND (expr, 0), &bitsize,
				       &bitpos, &offset, &mode, &unsignedp,
				       &reversep, &volatilep);

      if (base != NULL_TREE
	  && TREE_CODE (base) == MEM_REF
	  && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
	{
	  poly_offset_int off = 0;
	  bool off_cst = false;
	  if (offset == NULL_TREE || TREE_CODE (offset) == INTEGER_CST)
	    {
	      off = mem_ref_offset (base);
	      if (offset)
		off += poly_offset_int::from (wi::to_poly_wide (offset),
					      SIGNED);
	      off <<= LOG2_BITS_PER_UNIT;
	      off += bitpos;
	      off_cst = true;
	    }
	  /* If &X->a is equal to X and X is ~[0, 0], the result is too.
	     For -fdelete-null-pointer-checks -fno-wrapv-pointer we don't
	     allow going from non-NULL pointer to NULL.  */
	  if ((off_cst && known_eq (off, 0))
	      || (flag_delete_null_pointer_checks
		  && !TYPE_OVERFLOW_WRAPS (TREE_TYPE (expr))))
	    {
	      const value_range_equiv *vr
		= get_value_range (TREE_OPERAND (base, 0), stmt);
	      if (!range_includes_zero_p (vr))
		return true;
	    }
	  /* If MEM_REF has a "positive" offset, consider it non-NULL
	     always, for -fdelete-null-pointer-checks also "negative"
	     ones.  Punt for unknown offsets (e.g. variable ones).  */
	  if (!TYPE_OVERFLOW_WRAPS (TREE_TYPE (expr))
	      && off_cst
	      && known_ne (off, 0)
	      && (flag_delete_null_pointer_checks || known_gt (off, 0)))
	    return true;
	}
    }

  return false;
}

/* Returns true if EXPR is a valid value (as expected by compare_values) --
   a gimple invariant, or SSA_NAME +- CST.  */

static bool
valid_value_p (tree expr)
{
  if (TREE_CODE (expr) == SSA_NAME)
    return true;

  if (TREE_CODE (expr) == PLUS_EXPR
      || TREE_CODE (expr) == MINUS_EXPR)
    return (TREE_CODE (TREE_OPERAND (expr, 0)) == SSA_NAME
	    && TREE_CODE (TREE_OPERAND (expr, 1)) == INTEGER_CST);

  return is_gimple_min_invariant (expr);
}

/* If OP has a value range with a single constant value return that,
   otherwise return NULL_TREE.  This returns OP itself if OP is a
   constant.  */

tree
vr_values::op_with_constant_singleton_value_range (tree op)
{
  if (is_gimple_min_invariant (op))
    return op;

  if (TREE_CODE (op) != SSA_NAME)
    return NULL_TREE;

  tree t;
  if (get_value_range (op)->singleton_p (&t))
    return t;
  return NULL;
}

/* Return true if op is in a boolean [0, 1] value-range.  */

bool
simplify_using_ranges::op_with_boolean_value_range_p (tree op, gimple *s)
{
  if (TYPE_PRECISION (TREE_TYPE (op)) == 1)
    return true;

  if (integer_zerop (op)
      || integer_onep (op))
    return true;

  if (TREE_CODE (op) != SSA_NAME)
    return false;

  /* ?? Errr, this should probably check for [0,0] and [1,1] as well
     as [0,1].  */
  const value_range *vr = query->get_value_range (op, s);
  return *vr == value_range (build_zero_cst (TREE_TYPE (op)),
			     build_one_cst (TREE_TYPE (op)));
}

/* Extract value range information for VAR when (OP COND_CODE LIMIT) is
   true and store it in *VR_P.  */

void
vr_values::extract_range_for_var_from_comparison_expr (tree var,
						       enum tree_code cond_code,
						       tree op, tree limit,
						       value_range_equiv *vr_p)
{
  tree  min, max, type;
  const value_range_equiv *limit_vr;
  type = TREE_TYPE (var);

  /* For pointer arithmetic, we only keep track of pointer equality
     and inequality.  If we arrive here with unfolded conditions like
     _1 > _1 do not derive anything.  */
  if ((POINTER_TYPE_P (type) && cond_code != NE_EXPR && cond_code != EQ_EXPR)
      || limit == var)
    {
      vr_p->set_varying (type);
      return;
    }

  /* If LIMIT is another SSA name and LIMIT has a range of its own,
     try to use LIMIT's range to avoid creating symbolic ranges
     unnecessarily. */
  limit_vr = (TREE_CODE (limit) == SSA_NAME) ? get_value_range (limit) : NULL;

  /* LIMIT's range is only interesting if it has any useful information.  */
  if (! limit_vr
      || limit_vr->undefined_p ()
      || limit_vr->varying_p ()
      || (limit_vr->symbolic_p ()
	  && ! (limit_vr->kind () == VR_RANGE
		&& (limit_vr->min () == limit_vr->max ()
		    || operand_equal_p (limit_vr->min (),
					limit_vr->max (), 0)))))
    limit_vr = NULL;

  /* Initially, the new range has the same set of equivalences of
     VAR's range.  This will be revised before returning the final
     value.  Since assertions may be chained via mutually exclusive
     predicates, we will need to trim the set of equivalences before
     we are done.  */
  gcc_assert (vr_p->equiv () == NULL);
  vr_p->equiv_add (var, get_value_range (var), &vrp_equiv_obstack);

  /* Extract a new range based on the asserted comparison for VAR and
     LIMIT's value range.  Notice that if LIMIT has an anti-range, we
     will only use it for equality comparisons (EQ_EXPR).  For any
     other kind of assertion, we cannot derive a range from LIMIT's
     anti-range that can be used to describe the new range.  For
     instance, ASSERT_EXPR <x_2, x_2 <= b_4>.  If b_4 is ~[2, 10],
     then b_4 takes on the ranges [-INF, 1] and [11, +INF].  There is
     no single range for x_2 that could describe LE_EXPR, so we might
     as well build the range [b_4, +INF] for it.
     One special case we handle is extracting a range from a
     range test encoded as (unsigned)var + CST <= limit.  */
  if (TREE_CODE (op) == NOP_EXPR
      || TREE_CODE (op) == PLUS_EXPR)
    {
      if (TREE_CODE (op) == PLUS_EXPR)
        {
	  min = fold_build1 (NEGATE_EXPR, TREE_TYPE (TREE_OPERAND (op, 1)),
			     TREE_OPERAND (op, 1));
          max = int_const_binop (PLUS_EXPR, limit, min);
	  op = TREE_OPERAND (op, 0);
	}
      else
	{
	  min = build_int_cst (TREE_TYPE (var), 0);
	  max = limit;
	}

      /* Make sure to not set TREE_OVERFLOW on the final type
	 conversion.  We are willingly interpreting large positive
	 unsigned values as negative signed values here.  */
      min = force_fit_type (TREE_TYPE (var), wi::to_widest (min), 0, false);
      max = force_fit_type (TREE_TYPE (var), wi::to_widest (max), 0, false);

      /* We can transform a max, min range to an anti-range or
         vice-versa.  Use set_and_canonicalize which does this for
         us.  */
      if (cond_code == LE_EXPR)
	vr_p->set (min, max, vr_p->equiv ());
      else if (cond_code == GT_EXPR)
	vr_p->set (min, max, vr_p->equiv (), VR_ANTI_RANGE);
      else
	gcc_unreachable ();
    }
  else if (cond_code == EQ_EXPR)
    {
      enum value_range_kind range_kind;

      if (limit_vr)
	{
	  range_kind = limit_vr->kind ();
	  min = limit_vr->min ();
	  max = limit_vr->max ();
	}
      else
	{
	  range_kind = VR_RANGE;
	  min = limit;
	  max = limit;
	}

      vr_p->update (min, max, range_kind);

      /* When asserting the equality VAR == LIMIT and LIMIT is another
	 SSA name, the new range will also inherit the equivalence set
	 from LIMIT.  */
      if (TREE_CODE (limit) == SSA_NAME)
	vr_p->equiv_add (limit, get_value_range (limit), &vrp_equiv_obstack);
    }
  else if (cond_code == NE_EXPR)
    {
      /* As described above, when LIMIT's range is an anti-range and
	 this assertion is an inequality (NE_EXPR), then we cannot
	 derive anything from the anti-range.  For instance, if
	 LIMIT's range was ~[0, 0], the assertion 'VAR != LIMIT' does
	 not imply that VAR's range is [0, 0].  So, in the case of
	 anti-ranges, we just assert the inequality using LIMIT and
	 not its anti-range.

	 If LIMIT_VR is a range, we can only use it to build a new
	 anti-range if LIMIT_VR is a single-valued range.  For
	 instance, if LIMIT_VR is [0, 1], the predicate
	 VAR != [0, 1] does not mean that VAR's range is ~[0, 1].
	 Rather, it means that for value 0 VAR should be ~[0, 0]
	 and for value 1, VAR should be ~[1, 1].  We cannot
	 represent these ranges.

	 The only situation in which we can build a valid
	 anti-range is when LIMIT_VR is a single-valued range
	 (i.e., LIMIT_VR->MIN == LIMIT_VR->MAX).  In that case,
	 build the anti-range ~[LIMIT_VR->MIN, LIMIT_VR->MAX].  */
      if (limit_vr
	  && limit_vr->kind () == VR_RANGE
	  && compare_values (limit_vr->min (), limit_vr->max ()) == 0)
	{
	  min = limit_vr->min ();
	  max = limit_vr->max ();
	}
      else
	{
	  /* In any other case, we cannot use LIMIT's range to build a
	     valid anti-range.  */
	  min = max = limit;
	}

      /* If MIN and MAX cover the whole range for their type, then
	 just use the original LIMIT.  */
      if (INTEGRAL_TYPE_P (type)
	  && vrp_val_is_min (min)
	  && vrp_val_is_max (max))
	min = max = limit;

      vr_p->set (min, max, vr_p->equiv (), VR_ANTI_RANGE);
    }
  else if (cond_code == LE_EXPR || cond_code == LT_EXPR)
    {
      min = TYPE_MIN_VALUE (type);

      if (limit_vr == NULL || limit_vr->kind () == VR_ANTI_RANGE)
	max = limit;
      else
	{
	  /* If LIMIT_VR is of the form [N1, N2], we need to build the
	     range [MIN, N2] for LE_EXPR and [MIN, N2 - 1] for
	     LT_EXPR.  */
	  max = limit_vr->max ();
	}

      /* If the maximum value forces us to be out of bounds, simply punt.
	 It would be pointless to try and do anything more since this
	 all should be optimized away above us.  */
      if (cond_code == LT_EXPR
	  && compare_values (max, min) == 0)
	vr_p->set_varying (TREE_TYPE (min));
      else
	{
	  /* For LT_EXPR, we create the range [MIN, MAX - 1].  */
	  if (cond_code == LT_EXPR)
	    {
	      if (TYPE_PRECISION (TREE_TYPE (max)) == 1
		  && !TYPE_UNSIGNED (TREE_TYPE (max)))
		max = fold_build2 (PLUS_EXPR, TREE_TYPE (max), max,
				   build_int_cst (TREE_TYPE (max), -1));
	      else
		max = fold_build2 (MINUS_EXPR, TREE_TYPE (max), max,
				   build_int_cst (TREE_TYPE (max), 1));
	      /* Signal to compare_values_warnv this expr doesn't overflow.  */
	      if (EXPR_P (max))
		suppress_warning (max, OPT_Woverflow);
	    }

	  vr_p->update (min, max);
	}
    }
  else if (cond_code == GE_EXPR || cond_code == GT_EXPR)
    {
      max = TYPE_MAX_VALUE (type);

      if (limit_vr == NULL || limit_vr->kind () == VR_ANTI_RANGE)
	min = limit;
      else
	{
	  /* If LIMIT_VR is of the form [N1, N2], we need to build the
	     range [N1, MAX] for GE_EXPR and [N1 + 1, MAX] for
	     GT_EXPR.  */
	  min = limit_vr->min ();
	}

      /* If the minimum value forces us to be out of bounds, simply punt.
	 It would be pointless to try and do anything more since this
	 all should be optimized away above us.  */
      if (cond_code == GT_EXPR
	  && compare_values (min, max) == 0)
	vr_p->set_varying (TREE_TYPE (min));
      else
	{
	  /* For GT_EXPR, we create the range [MIN + 1, MAX].  */
	  if (cond_code == GT_EXPR)
	    {
	      if (TYPE_PRECISION (TREE_TYPE (min)) == 1
		  && !TYPE_UNSIGNED (TREE_TYPE (min)))
		min = fold_build2 (MINUS_EXPR, TREE_TYPE (min), min,
				   build_int_cst (TREE_TYPE (min), -1));
	      else
		min = fold_build2 (PLUS_EXPR, TREE_TYPE (min), min,
				   build_int_cst (TREE_TYPE (min), 1));
	      /* Signal to compare_values_warnv this expr doesn't overflow.  */
	      if (EXPR_P (min))
		suppress_warning (min, OPT_Woverflow);
	    }

	  vr_p->update (min, max);
	}
    }
  else
    gcc_unreachable ();

  /* Finally intersect the new range with what we already know about var.  */
  vr_p->intersect (get_value_range (var));
}

/* Extract value range information from an ASSERT_EXPR EXPR and store
   it in *VR_P.  */

void
vr_values::extract_range_from_assert (value_range_equiv *vr_p, tree expr)
{
  tree var = ASSERT_EXPR_VAR (expr);
  tree cond = ASSERT_EXPR_COND (expr);
  tree limit, op;
  enum tree_code cond_code;
  gcc_assert (COMPARISON_CLASS_P (cond));

  /* Find VAR in the ASSERT_EXPR conditional.  */
  if (var == TREE_OPERAND (cond, 0)
      || TREE_CODE (TREE_OPERAND (cond, 0)) == PLUS_EXPR
      || TREE_CODE (TREE_OPERAND (cond, 0)) == NOP_EXPR)
    {
      /* If the predicate is of the form VAR COMP LIMIT, then we just
	 take LIMIT from the RHS and use the same comparison code.  */
      cond_code = TREE_CODE (cond);
      limit = TREE_OPERAND (cond, 1);
      op = TREE_OPERAND (cond, 0);
    }
  else
    {
      /* If the predicate is of the form LIMIT COMP VAR, then we need
	 to flip around the comparison code to create the proper range
	 for VAR.  */
      cond_code = swap_tree_comparison (TREE_CODE (cond));
      limit = TREE_OPERAND (cond, 0);
      op = TREE_OPERAND (cond, 1);
    }
  extract_range_for_var_from_comparison_expr (var, cond_code, op,
					      limit, vr_p);
}

/* Extract range information from SSA name VAR and store it in VR.  If
   VAR has an interesting range, use it.  Otherwise, create the
   range [VAR, VAR] and return it.  This is useful in situations where
   we may have conditionals testing values of VARYING names.  For
   instance,

   	x_3 = y_5;
	if (x_3 > y_5)
	  ...

    Even if y_5 is deemed VARYING, we can determine that x_3 > y_5 is
    always false.  */

void
vr_values::extract_range_from_ssa_name (value_range_equiv *vr, tree var)
{
  const value_range_equiv *var_vr = get_value_range (var);

  if (!var_vr->varying_p ())
    vr->deep_copy (var_vr);
  else
    vr->set (var);

  if (!vr->undefined_p ())
    vr->equiv_add (var, get_value_range (var), &vrp_equiv_obstack);
}

/* Extract range information from a binary expression OP0 CODE OP1 based on
   the ranges of each of its operands with resulting type EXPR_TYPE.
   The resulting range is stored in *VR.  */

void
vr_values::extract_range_from_binary_expr (value_range_equiv *vr,
					   enum tree_code code,
					   tree expr_type, tree op0, tree op1)
{
  /* Get value ranges for each operand.  For constant operands, create
     a new value range with the operand to simplify processing.  */
  value_range vr0, vr1;
  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = *(get_value_range (op0));
  else if (is_gimple_min_invariant (op0))
    vr0.set (op0);
  else
    vr0.set_varying (TREE_TYPE (op0));

  if (TREE_CODE (op1) == SSA_NAME)
    vr1 = *(get_value_range (op1));
  else if (is_gimple_min_invariant (op1))
    vr1.set (op1);
  else
    vr1.set_varying (TREE_TYPE (op1));

  /* If one argument is varying, we can sometimes still deduce a
     range for the output: any + [3, +INF] is in [MIN+3, +INF].  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (op0)))
    {
      if (vr0.varying_p () && !vr1.varying_p ())
	vr0 = value_range (vrp_val_min (expr_type), vrp_val_max (expr_type));
      else if (vr1.varying_p () && !vr0.varying_p ())
	vr1 = value_range (vrp_val_min (expr_type), vrp_val_max (expr_type));
    }

  range_fold_binary_expr (vr, code, expr_type, &vr0, &vr1);

  /* Set value_range for n in following sequence:
     def = __builtin_memchr (arg, 0, sz)
     n = def - arg
     Here the range for n can be set to [0, PTRDIFF_MAX - 1]. */

  if (vr->varying_p ()
      && code == POINTER_DIFF_EXPR
      && TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (op1) == SSA_NAME)
    {
      tree op0_ptype = TREE_TYPE (TREE_TYPE (op0));
      tree op1_ptype = TREE_TYPE (TREE_TYPE (op1));
      gcall *call_stmt = NULL;

      if (TYPE_MODE (op0_ptype) == TYPE_MODE (char_type_node)
	  && TYPE_PRECISION (op0_ptype) == TYPE_PRECISION (char_type_node)
	  && TYPE_MODE (op1_ptype) == TYPE_MODE (char_type_node)
	  && TYPE_PRECISION (op1_ptype) == TYPE_PRECISION (char_type_node)
	  && (call_stmt = dyn_cast<gcall *>(SSA_NAME_DEF_STMT (op0)))
	  && gimple_call_builtin_p (call_stmt, BUILT_IN_MEMCHR)
	  && operand_equal_p (op0, gimple_call_lhs (call_stmt), 0)
	  && operand_equal_p (op1, gimple_call_arg (call_stmt, 0), 0)
	  && integer_zerop (gimple_call_arg (call_stmt, 1)))
	    {
	      tree max = vrp_val_max (ptrdiff_type_node);
	      wide_int wmax = wi::to_wide (max, TYPE_PRECISION (TREE_TYPE (max)));
	      tree range_min = build_zero_cst (expr_type);
	      tree range_max = wide_int_to_tree (expr_type, wmax - 1);
	      vr->set (range_min, range_max);
	      return;
	    }
     }

  /* Try harder for PLUS and MINUS if the range of one operand is symbolic
     and based on the other operand, for example if it was deduced from a
     symbolic comparison.  When a bound of the range of the first operand
     is invariant, we set the corresponding bound of the new range to INF
     in order to avoid recursing on the range of the second operand.  */
  if (vr->varying_p ()
      && (code == PLUS_EXPR || code == MINUS_EXPR)
      && TREE_CODE (op1) == SSA_NAME
      && vr0.kind () == VR_RANGE
      && symbolic_range_based_on_p (&vr0, op1))
    {
      const bool minus_p = (code == MINUS_EXPR);
      value_range n_vr1;

      /* Try with VR0 and [-INF, OP1].  */
      if (is_gimple_min_invariant (minus_p ? vr0.max () : vr0.min ()))
	n_vr1.set (vrp_val_min (expr_type), op1);

      /* Try with VR0 and [OP1, +INF].  */
      else if (is_gimple_min_invariant (minus_p ? vr0.min () : vr0.max ()))
	n_vr1.set (op1, vrp_val_max (expr_type));

      /* Try with VR0 and [OP1, OP1].  */
      else
	n_vr1.set (op1, op1);

      range_fold_binary_expr (vr, code, expr_type, &vr0, &n_vr1);
    }

  if (vr->varying_p ()
      && (code == PLUS_EXPR || code == MINUS_EXPR)
      && TREE_CODE (op0) == SSA_NAME
      && vr1.kind () == VR_RANGE
      && symbolic_range_based_on_p (&vr1, op0))
    {
      const bool minus_p = (code == MINUS_EXPR);
      value_range n_vr0;

      /* Try with [-INF, OP0] and VR1.  */
      if (is_gimple_min_invariant (minus_p ? vr1.max () : vr1.min ()))
	n_vr0.set (vrp_val_min (expr_type), op0);

      /* Try with [OP0, +INF] and VR1.  */
      else if (is_gimple_min_invariant (minus_p ? vr1.min (): vr1.max ()))
	n_vr0.set (op0, vrp_val_max (expr_type));

      /* Try with [OP0, OP0] and VR1.  */
      else
	n_vr0.set (op0);

      range_fold_binary_expr (vr, code, expr_type, &n_vr0, &vr1);
    }

  /* If we didn't derive a range for MINUS_EXPR, and
     op1's range is ~[op0,op0] or vice-versa, then we
     can derive a non-null range.  This happens often for
     pointer subtraction.  */
  if (vr->varying_p ()
      && (code == MINUS_EXPR || code == POINTER_DIFF_EXPR)
      && TREE_CODE (op0) == SSA_NAME
      && ((vr0.kind () == VR_ANTI_RANGE
	   && vr0.min () == op1
	   && vr0.min () == vr0.max ())
	  || (vr1.kind () == VR_ANTI_RANGE
	      && vr1.min () == op0
	      && vr1.min () == vr1.max ())))
    {
      vr->set_nonzero (expr_type);
      vr->equiv_clear ();
    }
}

/* Extract range information from a unary expression CODE OP0 based on
   the range of its operand with resulting type TYPE.
   The resulting range is stored in *VR.  */

void
vr_values::extract_range_from_unary_expr (value_range_equiv *vr,
					  enum tree_code code,
					  tree type, tree op0)
{
  value_range vr0;

  /* Get value ranges for the operand.  For constant operands, create
     a new value range with the operand to simplify processing.  */
  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = *(get_value_range (op0));
  else if (is_gimple_min_invariant (op0))
    vr0.set (op0);
  else
    vr0.set_varying (type);

  range_fold_unary_expr (vr, code, type, &vr0, TREE_TYPE (op0));
}


/* Extract range information from a conditional expression STMT based on
   the ranges of each of its operands and the expression code.  */

void
vr_values::extract_range_from_cond_expr (value_range_equiv *vr, gassign *stmt)
{
  /* Get value ranges for each operand.  For constant operands, create
     a new value range with the operand to simplify processing.  */
  tree op0 = gimple_assign_rhs2 (stmt);
  value_range_equiv tem0;
  const value_range_equiv *vr0 = &tem0;
  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = get_value_range (op0);
  else if (is_gimple_min_invariant (op0))
    tem0.set (op0);
  else
    tem0.set_varying (TREE_TYPE (op0));

  tree op1 = gimple_assign_rhs3 (stmt);
  value_range_equiv tem1;
  const value_range_equiv *vr1 = &tem1;
  if (TREE_CODE (op1) == SSA_NAME)
    vr1 = get_value_range (op1);
  else if (is_gimple_min_invariant (op1))
    tem1.set (op1);
  else
    tem1.set_varying (TREE_TYPE (op1));

  /* The resulting value range is the union of the operand ranges */
  vr->deep_copy (vr0);
  vr->union_ (vr1);
}


/* Extract range information from a comparison expression EXPR based
   on the range of its operand and the expression code.  */

void
vr_values::extract_range_from_comparison (value_range_equiv *vr,
					  gimple *stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);
  tree type = TREE_TYPE (gimple_assign_lhs (stmt));
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  bool sop;
  tree val
    = simplifier.vrp_evaluate_conditional_warnv_with_ops (stmt, code, op0, op1,
							  false, &sop, NULL);
  if (val)
    {
      /* Since this expression was found on the RHS of an assignment,
	 its type may be different from _Bool.  Convert VAL to EXPR's
	 type.  */
      val = fold_convert (type, val);
      if (is_gimple_min_invariant (val))
	vr->set (val);
      else
	vr->update (val, val);
    }
  else
    /* The result of a comparison is always true or false.  */
    set_value_range_to_truthvalue (vr, type);
}

/* Helper function for simplify_internal_call_using_ranges and
   extract_range_basic.  Return true if OP0 SUBCODE OP1 for
   SUBCODE {PLUS,MINUS,MULT}_EXPR is known to never overflow or
   always overflow.  Set *OVF to true if it is known to always
   overflow.  */

static bool
check_for_binary_op_overflow (range_query *query,
			      enum tree_code subcode, tree type,
			      tree op0, tree op1, bool *ovf, gimple *s = NULL)
{
  value_range vr0, vr1;
  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = *query->get_value_range (op0, s);
  else if (TREE_CODE (op0) == INTEGER_CST)
    vr0.set (op0);
  else
    vr0.set_varying (TREE_TYPE (op0));

  if (TREE_CODE (op1) == SSA_NAME)
    vr1 = *query->get_value_range (op1, s);
  else if (TREE_CODE (op1) == INTEGER_CST)
    vr1.set (op1);
  else
    vr1.set_varying (TREE_TYPE (op1));

  tree vr0min = vr0.min (), vr0max = vr0.max ();
  tree vr1min = vr1.min (), vr1max = vr1.max ();
  if (!range_int_cst_p (&vr0)
      || TREE_OVERFLOW (vr0min)
      || TREE_OVERFLOW (vr0max))
    {
      vr0min = vrp_val_min (TREE_TYPE (op0));
      vr0max = vrp_val_max (TREE_TYPE (op0));
    }
  if (!range_int_cst_p (&vr1)
      || TREE_OVERFLOW (vr1min)
      || TREE_OVERFLOW (vr1max))
    {
      vr1min = vrp_val_min (TREE_TYPE (op1));
      vr1max = vrp_val_max (TREE_TYPE (op1));
    }
  *ovf = arith_overflowed_p (subcode, type, vr0min,
			     subcode == MINUS_EXPR ? vr1max : vr1min);
  if (arith_overflowed_p (subcode, type, vr0max,
			  subcode == MINUS_EXPR ? vr1min : vr1max) != *ovf)
    return false;
  if (subcode == MULT_EXPR)
    {
      if (arith_overflowed_p (subcode, type, vr0min, vr1max) != *ovf
	  || arith_overflowed_p (subcode, type, vr0max, vr1min) != *ovf)
	return false;
    }
  if (*ovf)
    {
      /* So far we found that there is an overflow on the boundaries.
	 That doesn't prove that there is an overflow even for all values
	 in between the boundaries.  For that compute widest_int range
	 of the result and see if it doesn't overlap the range of
	 type.  */
      widest_int wmin, wmax;
      widest_int w[4];
      int i;
      w[0] = wi::to_widest (vr0min);
      w[1] = wi::to_widest (vr0max);
      w[2] = wi::to_widest (vr1min);
      w[3] = wi::to_widest (vr1max);
      for (i = 0; i < 4; i++)
	{
	  widest_int wt;
	  switch (subcode)
	    {
	    case PLUS_EXPR:
	      wt = wi::add (w[i & 1], w[2 + (i & 2) / 2]);
	      break;
	    case MINUS_EXPR:
	      wt = wi::sub (w[i & 1], w[2 + (i & 2) / 2]);
	      break;
	    case MULT_EXPR:
	      wt = wi::mul (w[i & 1], w[2 + (i & 2) / 2]);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  if (i == 0)
	    {
	      wmin = wt;
	      wmax = wt;
	    }
	  else
	    {
	      wmin = wi::smin (wmin, wt);
	      wmax = wi::smax (wmax, wt);
	    }
	}
      /* The result of op0 CODE op1 is known to be in range
	 [wmin, wmax].  */
      widest_int wtmin = wi::to_widest (vrp_val_min (type));
      widest_int wtmax = wi::to_widest (vrp_val_max (type));
      /* If all values in [wmin, wmax] are smaller than
	 [wtmin, wtmax] or all are larger than [wtmin, wtmax],
	 the arithmetic operation will always overflow.  */
      if (wmax < wtmin || wmin > wtmax)
	return true;
      return false;
    }
  return true;
}

/* Derive a range from a builtin.  Set range in VR and return TRUE if
   successful.  */

bool
vr_values::extract_range_from_ubsan_builtin (value_range_equiv *vr, gimple *stmt)
{
  gcc_assert (is_gimple_call (stmt));
  enum tree_code subcode = ERROR_MARK;
  combined_fn cfn = gimple_call_combined_fn (stmt);
  scalar_int_mode mode;

  switch (cfn)
    {
    case CFN_UBSAN_CHECK_ADD:
      subcode = PLUS_EXPR;
      break;
    case CFN_UBSAN_CHECK_SUB:
      subcode = MINUS_EXPR;
      break;
    case CFN_UBSAN_CHECK_MUL:
      subcode = MULT_EXPR;
      break;
    default:
      break;
    }
  if (subcode != ERROR_MARK)
    {
      bool saved_flag_wrapv = flag_wrapv;
      /* Pretend the arithmetics is wrapping.  If there is
	 any overflow, we'll complain, but will actually do
	 wrapping operation.  */
      flag_wrapv = 1;
      extract_range_from_binary_expr (vr, subcode,
				      TREE_TYPE (gimple_call_arg (stmt, 0)),
				      gimple_call_arg (stmt, 0),
				      gimple_call_arg (stmt, 1));
      flag_wrapv = saved_flag_wrapv;

      /* If for both arguments vrp_valueize returned non-NULL,
	 this should have been already folded and if not, it
	 wasn't folded because of overflow.  Avoid removing the
	 UBSAN_CHECK_* calls in that case.  */
      if (vr->kind () == VR_RANGE
	  && (vr->min () == vr->max ()
	      || operand_equal_p (vr->min (), vr->max (), 0)))
	vr->set_varying (vr->type ());

      return !vr->varying_p ();
    }
  return false;
}

/* Try to derive a nonnegative or nonzero range out of STMT relying
   primarily on generic routines in fold in conjunction with range data.
   Store the result in *VR */

void
vr_values::extract_range_basic (value_range_equiv *vr, gimple *stmt)
{
  bool sop;

  if (is_gimple_call (stmt))
    {
      combined_fn cfn = gimple_call_combined_fn (stmt);
      switch (cfn)
	{
	case CFN_UBSAN_CHECK_ADD:
	case CFN_UBSAN_CHECK_SUB:
	case CFN_UBSAN_CHECK_MUL:
	  if (extract_range_from_ubsan_builtin (vr, stmt))
	    return;
	  break;
	default:
	  if (fold_range (*vr, stmt, this))
	    {
	      /* The original code nuked equivalences every time a
		 range was found, so do the same here.  */
	      vr->equiv_clear ();
	      return;
	    }
	  break;
	}
    }
  /* Handle extraction of the two results (result of arithmetics and
     a flag whether arithmetics overflowed) from {ADD,SUB,MUL}_OVERFLOW
     internal function.  Similarly from ATOMIC_COMPARE_EXCHANGE.  */
  if (is_gimple_assign (stmt)
      && (gimple_assign_rhs_code (stmt) == REALPART_EXPR
	  || gimple_assign_rhs_code (stmt) == IMAGPART_EXPR)
      && INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_lhs (stmt))))
    {
      enum tree_code code = gimple_assign_rhs_code (stmt);
      tree op = gimple_assign_rhs1 (stmt);
      tree type = TREE_TYPE (gimple_assign_lhs (stmt));
      if (TREE_CODE (op) == code && TREE_CODE (TREE_OPERAND (op, 0)) == SSA_NAME)
	{
	  gimple *g = SSA_NAME_DEF_STMT (TREE_OPERAND (op, 0));
	  if (is_gimple_call (g) && gimple_call_internal_p (g))
	    {
	      enum tree_code subcode = ERROR_MARK;
	      switch (gimple_call_internal_fn (g))
		{
		case IFN_ADD_OVERFLOW:
		  subcode = PLUS_EXPR;
		  break;
		case IFN_SUB_OVERFLOW:
		  subcode = MINUS_EXPR;
		  break;
		case IFN_MUL_OVERFLOW:
		  subcode = MULT_EXPR;
		  break;
		case IFN_ATOMIC_COMPARE_EXCHANGE:
		  if (code == IMAGPART_EXPR)
		    {
		      /* This is the boolean return value whether compare and
			 exchange changed anything or not.  */
		      vr->set (build_int_cst (type, 0),
			       build_int_cst (type, 1));
		      return;
		    }
		  break;
		default:
		  break;
		}
	      if (subcode != ERROR_MARK)
		{
		  tree op0 = gimple_call_arg (g, 0);
		  tree op1 = gimple_call_arg (g, 1);
		  if (code == IMAGPART_EXPR)
		    {
		      bool ovf = false;
		      if (check_for_binary_op_overflow (this, subcode, type,
							op0, op1, &ovf))
			vr->set (build_int_cst (type, ovf));
		      else if (TYPE_PRECISION (type) == 1
			       && !TYPE_UNSIGNED (type))
			vr->set_varying (type);
		      else
			vr->set (build_int_cst (type, 0),
				 build_int_cst (type, 1));
		    }
		  else if (types_compatible_p (type, TREE_TYPE (op0))
			   && types_compatible_p (type, TREE_TYPE (op1)))
		    {
		      bool saved_flag_wrapv = flag_wrapv;
		      /* Pretend the arithmetics is wrapping.  If there is
			 any overflow, IMAGPART_EXPR will be set.  */
		      flag_wrapv = 1;
		      extract_range_from_binary_expr (vr, subcode, type,
						      op0, op1);
		      flag_wrapv = saved_flag_wrapv;
		    }
		  else
		    {
		      value_range_equiv vr0, vr1;
		      bool saved_flag_wrapv = flag_wrapv;
		      /* Pretend the arithmetics is wrapping.  If there is
			 any overflow, IMAGPART_EXPR will be set.  */
		      flag_wrapv = 1;
		      extract_range_from_unary_expr (&vr0, NOP_EXPR,
						     type, op0);
		      extract_range_from_unary_expr (&vr1, NOP_EXPR,
						     type, op1);
		      range_fold_binary_expr (vr, subcode, type, &vr0, &vr1);
		      flag_wrapv = saved_flag_wrapv;
		    }
		  return;
		}
	    }
	}
    }
  /* None of the below should need a 'type', but we are only called
     for assignments and calls with a LHS.  */
  tree type = TREE_TYPE (gimple_get_lhs (stmt));
  if (INTEGRAL_TYPE_P (type)
      && gimple_stmt_nonnegative_warnv_p (stmt, &sop))
    set_value_range_to_nonnegative (vr, type);
  else if (vrp_stmt_computes_nonzero (stmt))
    {
      vr->set_nonzero (type);
      vr->equiv_clear ();
    }
  else
    vr->set_varying (type);
}


/* Try to compute a useful range out of assignment STMT and store it
   in *VR.  */

void
vr_values::extract_range_from_assignment (value_range_equiv *vr, gassign *stmt)
{
  enum tree_code code = gimple_assign_rhs_code (stmt);

  if (code == ASSERT_EXPR)
    extract_range_from_assert (vr, gimple_assign_rhs1 (stmt));
  else if (code == SSA_NAME)
    extract_range_from_ssa_name (vr, gimple_assign_rhs1 (stmt));
  else if (TREE_CODE_CLASS (code) == tcc_binary)
    extract_range_from_binary_expr (vr, gimple_assign_rhs_code (stmt),
				    TREE_TYPE (gimple_assign_lhs (stmt)),
				    gimple_assign_rhs1 (stmt),
				    gimple_assign_rhs2 (stmt));
  else if (TREE_CODE_CLASS (code) == tcc_unary)
    extract_range_from_unary_expr (vr, gimple_assign_rhs_code (stmt),
				   TREE_TYPE (gimple_assign_lhs (stmt)),
				   gimple_assign_rhs1 (stmt));
  else if (code == COND_EXPR)
    extract_range_from_cond_expr (vr, stmt);
  else if (TREE_CODE_CLASS (code) == tcc_comparison)
    extract_range_from_comparison (vr, stmt);
  else if (get_gimple_rhs_class (code) == GIMPLE_SINGLE_RHS
	   && is_gimple_min_invariant (gimple_assign_rhs1 (stmt)))
    vr->set (gimple_assign_rhs1 (stmt));
  else
    vr->set_varying (TREE_TYPE (gimple_assign_lhs (stmt)));

  if (vr->varying_p ())
    extract_range_basic (vr, stmt);
}

/* Given two numeric value ranges VR0, VR1 and a comparison code COMP:

   - Return BOOLEAN_TRUE_NODE if VR0 COMP VR1 always returns true for
     all the values in the ranges.

   - Return BOOLEAN_FALSE_NODE if the comparison always returns false.

   - Return NULL_TREE if it is not always possible to determine the
     value of the comparison.

   Also set *STRICT_OVERFLOW_P to indicate whether comparision evaluation
   assumed signed overflow is undefined.  */


static tree
compare_ranges (enum tree_code comp, const value_range_equiv *vr0,
		const value_range_equiv *vr1, bool *strict_overflow_p)
{
  /* VARYING or UNDEFINED ranges cannot be compared.  */
  if (vr0->varying_p ()
      || vr0->undefined_p ()
      || vr1->varying_p ()
      || vr1->undefined_p ())
    return NULL_TREE;

  /* Anti-ranges need to be handled separately.  */
  if (vr0->kind () == VR_ANTI_RANGE || vr1->kind () == VR_ANTI_RANGE)
    {
      /* If both are anti-ranges, then we cannot compute any
	 comparison.  */
      if (vr0->kind () == VR_ANTI_RANGE && vr1->kind () == VR_ANTI_RANGE)
	return NULL_TREE;

      /* These comparisons are never statically computable.  */
      if (comp == GT_EXPR
	  || comp == GE_EXPR
	  || comp == LT_EXPR
	  || comp == LE_EXPR)
	return NULL_TREE;

      /* Equality can be computed only between a range and an
	 anti-range.  ~[VAL1, VAL2] == [VAL1, VAL2] is always false.  */
      if (vr0->kind () == VR_RANGE)
	/* To simplify processing, make VR0 the anti-range.  */
	std::swap (vr0, vr1);

      gcc_assert (comp == NE_EXPR || comp == EQ_EXPR);

      if (compare_values_warnv (vr0->min (), vr1->min (), strict_overflow_p) == 0
	  && compare_values_warnv (vr0->max (), vr1->max (), strict_overflow_p) == 0)
	return (comp == NE_EXPR) ? boolean_true_node : boolean_false_node;

      return NULL_TREE;
    }

  /* Simplify processing.  If COMP is GT_EXPR or GE_EXPR, switch the
     operands around and change the comparison code.  */
  if (comp == GT_EXPR || comp == GE_EXPR)
    {
      comp = (comp == GT_EXPR) ? LT_EXPR : LE_EXPR;
      std::swap (vr0, vr1);
    }

  if (comp == EQ_EXPR)
    {
      /* Equality may only be computed if both ranges represent
	 exactly one value.  */
      if (compare_values_warnv (vr0->min (), vr0->max (), strict_overflow_p) == 0
	  && compare_values_warnv (vr1->min (), vr1->max (), strict_overflow_p) == 0)
	{
	  int cmp_min = compare_values_warnv (vr0->min (), vr1->min (),
					      strict_overflow_p);
	  int cmp_max = compare_values_warnv (vr0->max (), vr1->max (),
					      strict_overflow_p);
	  if (cmp_min == 0 && cmp_max == 0)
	    return boolean_true_node;
	  else if (cmp_min != -2 && cmp_max != -2)
	    return boolean_false_node;
	}
      /* If [V0_MIN, V1_MAX] < [V1_MIN, V1_MAX] then V0 != V1.  */
      else if (compare_values_warnv (vr0->min (), vr1->max (),
				     strict_overflow_p) == 1
	       || compare_values_warnv (vr1->min (), vr0->max (),
					strict_overflow_p) == 1)
	return boolean_false_node;

      return NULL_TREE;
    }
  else if (comp == NE_EXPR)
    {
      int cmp1, cmp2;

      /* If VR0 is completely to the left or completely to the right
	 of VR1, they are always different.  Notice that we need to
	 make sure that both comparisons yield similar results to
	 avoid comparing values that cannot be compared at
	 compile-time.  */
      cmp1 = compare_values_warnv (vr0->max (), vr1->min (), strict_overflow_p);
      cmp2 = compare_values_warnv (vr0->min (), vr1->max (), strict_overflow_p);
      if ((cmp1 == -1 && cmp2 == -1) || (cmp1 == 1 && cmp2 == 1))
	return boolean_true_node;

      /* If VR0 and VR1 represent a single value and are identical,
	 return false.  */
      else if (compare_values_warnv (vr0->min (), vr0->max (),
				     strict_overflow_p) == 0
	       && compare_values_warnv (vr1->min (), vr1->max (),
					strict_overflow_p) == 0
	       && compare_values_warnv (vr0->min (), vr1->min (),
					strict_overflow_p) == 0
	       && compare_values_warnv (vr0->max (), vr1->max (),
					strict_overflow_p) == 0)
	return boolean_false_node;

      /* Otherwise, they may or may not be different.  */
      else
	return NULL_TREE;
    }
  else if (comp == LT_EXPR || comp == LE_EXPR)
    {
      int tst;

      /* If VR0 is to the left of VR1, return true.  */
      tst = compare_values_warnv (vr0->max (), vr1->min (), strict_overflow_p);
      if ((comp == LT_EXPR && tst == -1)
	  || (comp == LE_EXPR && (tst == -1 || tst == 0)))
	return boolean_true_node;

      /* If VR0 is to the right of VR1, return false.  */
      tst = compare_values_warnv (vr0->min (), vr1->max (), strict_overflow_p);
      if ((comp == LT_EXPR && (tst == 0 || tst == 1))
	  || (comp == LE_EXPR && tst == 1))
	return boolean_false_node;

      /* Otherwise, we don't know.  */
      return NULL_TREE;
    }

  gcc_unreachable ();
}

/* Given a value range VR, a value VAL and a comparison code COMP, return
   BOOLEAN_TRUE_NODE if VR COMP VAL always returns true for all the
   values in VR.  Return BOOLEAN_FALSE_NODE if the comparison
   always returns false.  Return NULL_TREE if it is not always
   possible to determine the value of the comparison.  Also set
   *STRICT_OVERFLOW_P to indicate whether comparision evaluation
   assumed signed overflow is undefined.  */

static tree
compare_range_with_value (enum tree_code comp, const value_range *vr,
			  tree val, bool *strict_overflow_p)
{
  if (vr->varying_p () || vr->undefined_p ())
    return NULL_TREE;

  /* Anti-ranges need to be handled separately.  */
  if (vr->kind () == VR_ANTI_RANGE)
    {
      /* For anti-ranges, the only predicates that we can compute at
	 compile time are equality and inequality.  */
      if (comp == GT_EXPR
	  || comp == GE_EXPR
	  || comp == LT_EXPR
	  || comp == LE_EXPR)
	return NULL_TREE;

      /* ~[VAL_1, VAL_2] OP VAL is known if VAL_1 <= VAL <= VAL_2.  */
      if (!vr->may_contain_p (val))
	return (comp == NE_EXPR) ? boolean_true_node : boolean_false_node;

      return NULL_TREE;
    }

  if (comp == EQ_EXPR)
    {
      /* EQ_EXPR may only be computed if VR represents exactly
	 one value.  */
      if (compare_values_warnv (vr->min (), vr->max (), strict_overflow_p) == 0)
	{
	  int cmp = compare_values_warnv (vr->min (), val, strict_overflow_p);
	  if (cmp == 0)
	    return boolean_true_node;
	  else if (cmp == -1 || cmp == 1 || cmp == 2)
	    return boolean_false_node;
	}
      else if (compare_values_warnv (val, vr->min (), strict_overflow_p) == -1
	       || compare_values_warnv (vr->max (), val, strict_overflow_p) == -1)
	return boolean_false_node;

      return NULL_TREE;
    }
  else if (comp == NE_EXPR)
    {
      /* If VAL is not inside VR, then they are always different.  */
      if (compare_values_warnv (vr->max (), val, strict_overflow_p) == -1
	  || compare_values_warnv (vr->min (), val, strict_overflow_p) == 1)
	return boolean_true_node;

      /* If VR represents exactly one value equal to VAL, then return
	 false.  */
      if (compare_values_warnv (vr->min (), vr->max (), strict_overflow_p) == 0
	  && compare_values_warnv (vr->min (), val, strict_overflow_p) == 0)
	return boolean_false_node;

      /* Otherwise, they may or may not be different.  */
      return NULL_TREE;
    }
  else if (comp == LT_EXPR || comp == LE_EXPR)
    {
      int tst;

      /* If VR is to the left of VAL, return true.  */
      tst = compare_values_warnv (vr->max (), val, strict_overflow_p);
      if ((comp == LT_EXPR && tst == -1)
	  || (comp == LE_EXPR && (tst == -1 || tst == 0)))
	return boolean_true_node;

      /* If VR is to the right of VAL, return false.  */
      tst = compare_values_warnv (vr->min (), val, strict_overflow_p);
      if ((comp == LT_EXPR && (tst == 0 || tst == 1))
	  || (comp == LE_EXPR && tst == 1))
	return boolean_false_node;

      /* Otherwise, we don't know.  */
      return NULL_TREE;
    }
  else if (comp == GT_EXPR || comp == GE_EXPR)
    {
      int tst;

      /* If VR is to the right of VAL, return true.  */
      tst = compare_values_warnv (vr->min (), val, strict_overflow_p);
      if ((comp == GT_EXPR && tst == 1)
	  || (comp == GE_EXPR && (tst == 0 || tst == 1)))
	return boolean_true_node;

      /* If VR is to the left of VAL, return false.  */
      tst = compare_values_warnv (vr->max (), val, strict_overflow_p);
      if ((comp == GT_EXPR && (tst == -1 || tst == 0))
	  || (comp == GE_EXPR && tst == -1))
	return boolean_false_node;

      /* Otherwise, we don't know.  */
      return NULL_TREE;
    }

  gcc_unreachable ();
}

/* Given a VAR in STMT within LOOP, determine the bounds of the
   variable and store it in MIN/MAX and return TRUE.  If no bounds
   could be determined, return FALSE.  */

bool
bounds_of_var_in_loop (tree *min, tree *max, range_query *query,
		       class loop *loop, gimple *stmt, tree var)
{
  tree init, step, chrec, tmin, tmax, type = TREE_TYPE (var);
  enum ev_direction dir;

  chrec = instantiate_parameters (loop, analyze_scalar_evolution (loop, var));

  /* Like in PR19590, scev can return a constant function.  */
  if (is_gimple_min_invariant (chrec))
    {
      *min = *max = chrec;
      goto fix_overflow;
    }

  if (TREE_CODE (chrec) != POLYNOMIAL_CHREC)
    return false;

  init = initial_condition_in_loop_num (chrec, loop->num);
  step = evolution_part_in_loop_num (chrec, loop->num);

  if (!init || !step)
    return false;

  /* If INIT is an SSA with a singleton range, set INIT to said
     singleton, otherwise leave INIT alone.  */
  if (TREE_CODE (init) == SSA_NAME)
    query->get_value_range (init, stmt)->singleton_p (&init);
  /* Likewise for step.  */
  if (TREE_CODE (step) == SSA_NAME)
    query->get_value_range (step, stmt)->singleton_p (&step);

  /* If STEP is symbolic, we can't know whether INIT will be the
     minimum or maximum value in the range.  Also, unless INIT is
     a simple expression, compare_values and possibly other functions
     in tree-vrp won't be able to handle it.  */
  if (step == NULL_TREE
      || !is_gimple_min_invariant (step)
      || !valid_value_p (init))
    return false;

  dir = scev_direction (chrec);
  if (/* Do not adjust ranges if we do not know whether the iv increases
	 or decreases,  ... */
      dir == EV_DIR_UNKNOWN
      /* ... or if it may wrap.  */
      || scev_probably_wraps_p (NULL_TREE, init, step, stmt,
				get_chrec_loop (chrec), true))
    return false;

  if (POINTER_TYPE_P (type) || !TYPE_MIN_VALUE (type))
    tmin = lower_bound_in_type (type, type);
  else
    tmin = TYPE_MIN_VALUE (type);
  if (POINTER_TYPE_P (type) || !TYPE_MAX_VALUE (type))
    tmax = upper_bound_in_type (type, type);
  else
    tmax = TYPE_MAX_VALUE (type);

  /* Try to use estimated number of iterations for the loop to constrain the
     final value in the evolution.  */
  if (TREE_CODE (step) == INTEGER_CST
      && is_gimple_val (init)
      && (TREE_CODE (init) != SSA_NAME
	  || query->get_value_range (init, stmt)->kind () == VR_RANGE))
    {
      widest_int nit;

      /* We are only entering here for loop header PHI nodes, so using
	 the number of latch executions is the correct thing to use.  */
      if (max_loop_iterations (loop, &nit))
	{
	  signop sgn = TYPE_SIGN (TREE_TYPE (step));
	  wi::overflow_type overflow;

	  widest_int wtmp = wi::mul (wi::to_widest (step), nit, sgn,
				     &overflow);
	  /* If the multiplication overflowed we can't do a meaningful
	     adjustment.  Likewise if the result doesn't fit in the type
	     of the induction variable.  For a signed type we have to
	     check whether the result has the expected signedness which
	     is that of the step as number of iterations is unsigned.  */
	  if (!overflow
	      && wi::fits_to_tree_p (wtmp, TREE_TYPE (init))
	      && (sgn == UNSIGNED
		  || wi::gts_p (wtmp, 0) == wi::gts_p (wi::to_wide (step), 0)))
	    {
	      value_range maxvr, vr0, vr1;
	      if (TREE_CODE (init) == SSA_NAME)
		vr0 = *(query->get_value_range (init, stmt));
	      else if (is_gimple_min_invariant (init))
		vr0.set (init);
	      else
		vr0.set_varying (TREE_TYPE (init));
	      tree tem = wide_int_to_tree (TREE_TYPE (init), wtmp);
	      vr1.set (tem, tem);
	      range_fold_binary_expr (&maxvr, PLUS_EXPR,
				      TREE_TYPE (init), &vr0, &vr1);

	      /* Likewise if the addition did.  */
	      if (maxvr.kind () == VR_RANGE)
		{
		  value_range initvr;

		  if (TREE_CODE (init) == SSA_NAME)
		    initvr = *(query->get_value_range (init, stmt));
		  else if (is_gimple_min_invariant (init))
		    initvr.set (init);
		  else
		    return false;

		  /* Check if init + nit * step overflows.  Though we checked
		     scev {init, step}_loop doesn't wrap, it is not enough
		     because the loop may exit immediately.  Overflow could
		     happen in the plus expression in this case.  */
		  if ((dir == EV_DIR_DECREASES
		       && compare_values (maxvr.min (), initvr.min ()) != -1)
		      || (dir == EV_DIR_GROWS
			  && compare_values (maxvr.max (), initvr.max ()) != 1))
		    return false;

		  tmin = maxvr.min ();
		  tmax = maxvr.max ();
		}
	    }
	}
    }

  *min = tmin;
  *max = tmax;
  if (dir == EV_DIR_DECREASES)
    *max = init;
  else
    *min = init;

 fix_overflow:
  /* Even for valid range info, sometimes overflow flag will leak in.
     As GIMPLE IL should have no constants with TREE_OVERFLOW set, we
     drop them.  */
  if (TREE_OVERFLOW_P (*min))
    *min = drop_tree_overflow (*min);
  if (TREE_OVERFLOW_P (*max))
    *max = drop_tree_overflow (*max);

  gcc_checking_assert (compare_values (*min, *max) != 1);
  return true;
}

/* Given a range VR, a LOOP and a variable VAR, determine whether it
   would be profitable to adjust VR using scalar evolution information
   for VAR.  If so, update VR with the new limits.  */

void
vr_values::adjust_range_with_scev (value_range_equiv *vr, class loop *loop,
				   gimple *stmt, tree var)
{
  tree min, max;
  if (bounds_of_var_in_loop (&min, &max, this, loop, stmt, var))
    {
      if (vr->undefined_p () || vr->varying_p ())
	{
	  /* For VARYING or UNDEFINED ranges, just about anything we get
	     from scalar evolutions should be better.  */
	  vr->update (min, max);
	}
      else if (vr->kind () == VR_RANGE)
	{
	  /* Start with the input range... */
	  tree vrmin = vr->min ();
	  tree vrmax = vr->max ();

	  /* ...and narrow it down with what we got from SCEV.  */
	  if (compare_values (min, vrmin) == 1)
	    vrmin = min;
	  if (compare_values (max, vrmax) == -1)
	    vrmax = max;

	  vr->update (vrmin, vrmax);
	}
      else if (vr->kind () == VR_ANTI_RANGE)
	{
	  /* ?? As an enhancement, if VR, MIN, and MAX are constants, one
	     could just intersect VR with a range of [MIN,MAX].  */
	}
    }
}

/* Dump value ranges of all SSA_NAMEs to FILE.  */

void
vr_values::dump (FILE *file)
{
  size_t i;

  for (i = 0; i < num_vr_values; i++)
    {
      if (vr_value[i] && ssa_name (i))
	{
	  print_generic_expr (file, ssa_name (i));
	  fprintf (file, ": ");
	  dump_value_range (file, vr_value[i]);
	  fprintf (file, "\n");
	}
    }

  fprintf (file, "\n");
}

/* Initialize VRP lattice.  */

vr_values::vr_values () : simplifier (this)
{
  values_propagated = false;
  num_vr_values = num_ssa_names * 2;
  vr_value = XCNEWVEC (value_range_equiv *, num_vr_values);
  vr_phi_edge_counts = XCNEWVEC (int, num_ssa_names);
  bitmap_obstack_initialize (&vrp_equiv_obstack);
}

/* Free VRP lattice.  */

vr_values::~vr_values ()
{
  /* Free allocated memory.  */
  free (vr_value);
  free (vr_phi_edge_counts);
  bitmap_obstack_release (&vrp_equiv_obstack);

  /* So that we can distinguish between VRP data being available
     and not available.  */
  vr_value = NULL;
  vr_phi_edge_counts = NULL;
}


/* A hack.  */
static class vr_values *x_vr_values;

/* Return the singleton value-range for NAME or NAME.  */

static inline tree
vrp_valueize (tree name)
{
  if (TREE_CODE (name) == SSA_NAME)
    {
      const value_range_equiv *vr = x_vr_values->get_value_range (name);
      if (vr->kind () == VR_RANGE
	  && (TREE_CODE (vr->min ()) == SSA_NAME
	      || is_gimple_min_invariant (vr->min ()))
	  && vrp_operand_equal_p (vr->min (), vr->max ()))
	return vr->min ();
    }
  return name;
}

/* Return the singleton value-range for NAME if that is a constant
   but signal to not follow SSA edges.  */

static inline tree
vrp_valueize_1 (tree name)
{
  if (TREE_CODE (name) == SSA_NAME)
    {
      /* If the definition may be simulated again we cannot follow
         this SSA edge as the SSA propagator does not necessarily
	 re-visit the use.  */
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      if (!gimple_nop_p (def_stmt)
	  && prop_simulate_again_p (def_stmt))
	return NULL_TREE;
      const value_range_equiv *vr = x_vr_values->get_value_range (name);
      tree singleton;
      if (vr->singleton_p (&singleton))
	return singleton;
    }
  return name;
}

/* Given STMT, an assignment or call, return its LHS if the type
   of the LHS is suitable for VRP analysis, else return NULL_TREE.  */

tree
get_output_for_vrp (gimple *stmt)
{
  if (!is_gimple_assign (stmt) && !is_gimple_call (stmt))
    return NULL_TREE;

  /* We only keep track of ranges in integral and pointer types.  */
  tree lhs = gimple_get_lhs (stmt);
  if (TREE_CODE (lhs) == SSA_NAME
      && ((INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	   /* It is valid to have NULL MIN/MAX values on a type.  See
	      build_range_type.  */
	   && TYPE_MIN_VALUE (TREE_TYPE (lhs))
	   && TYPE_MAX_VALUE (TREE_TYPE (lhs)))
	  || POINTER_TYPE_P (TREE_TYPE (lhs))))
    return lhs;

  return NULL_TREE;
}

/* Visit assignment STMT.  If it produces an interesting range, record
   the range in VR and set LHS to OUTPUT_P.  */

void
vr_values::vrp_visit_assignment_or_call (gimple *stmt, tree *output_p,
					 value_range_equiv *vr)
{
  tree lhs = get_output_for_vrp (stmt);
  *output_p = lhs;

  /* We only keep track of ranges in integral and pointer types.  */
  if (lhs)
    {
      enum gimple_code code = gimple_code (stmt);

      /* Try folding the statement to a constant first.  */
      x_vr_values = this;
      tree tem = gimple_fold_stmt_to_constant_1 (stmt, vrp_valueize,
						 vrp_valueize_1);
      x_vr_values = NULL;
      if (tem)
	{
	  if (TREE_CODE (tem) == SSA_NAME
	      && (SSA_NAME_IS_DEFAULT_DEF (tem)
		  || ! prop_simulate_again_p (SSA_NAME_DEF_STMT (tem))))
	    {
	      extract_range_from_ssa_name (vr, tem);
	      return;
	    }
	  else if (is_gimple_min_invariant (tem))
	    {
	      vr->set (tem);
	      return;
	    }
	}
      /* Then dispatch to value-range extracting functions.  */
      if (code == GIMPLE_CALL)
	extract_range_basic (vr, stmt);
      else
	extract_range_from_assignment (vr, as_a <gassign *> (stmt));
    }
}

/* Helper that gets the value range of the SSA_NAME with version I
   or a symbolic range containing the SSA_NAME only if the value range
   is varying or undefined.  Uses TEM as storage for the alternate range.  */

const value_range_equiv *
simplify_using_ranges::get_vr_for_comparison (int i, value_range_equiv *tem,
					      gimple *s)
{
  /* Shallow-copy equiv bitmap.  */
  const value_range_equiv *vr = query->get_value_range (ssa_name (i), s);

  /* If name N_i does not have a valid range, use N_i as its own
     range.  This allows us to compare against names that may
     have N_i in their ranges.  */
  if (vr->varying_p () || vr->undefined_p ())
    {
      tem->set (ssa_name (i));
      return tem;
    }

  return vr;
}

/* Compare all the value ranges for names equivalent to VAR with VAL
   using comparison code COMP.  Return the same value returned by
   compare_range_with_value, including the setting of
   *STRICT_OVERFLOW_P.  */

tree
simplify_using_ranges::compare_name_with_value
				(enum tree_code comp, tree var, tree val,
				 bool *strict_overflow_p, bool use_equiv_p,
				 gimple *s)
{
  /* Get the set of equivalences for VAR.  */
  bitmap e = query->get_value_range (var, s)->equiv ();

  /* Start at -1.  Set it to 0 if we do a comparison without relying
     on overflow, or 1 if all comparisons rely on overflow.  */
  int used_strict_overflow = -1;

  /* Compare vars' value range with val.  */
  value_range_equiv tem_vr;
  const value_range_equiv *equiv_vr
    = get_vr_for_comparison (SSA_NAME_VERSION (var), &tem_vr, s);
  bool sop = false;
  tree retval = compare_range_with_value (comp, equiv_vr, val, &sop);
  if (retval)
    used_strict_overflow = sop ? 1 : 0;

  /* If the equiv set is empty we have done all work we need to do.  */
  if (e == NULL)
    {
      if (retval && used_strict_overflow > 0)
	*strict_overflow_p = true;
      return retval;
    }

  unsigned i;
  bitmap_iterator bi;
  EXECUTE_IF_SET_IN_BITMAP (e, 0, i, bi)
    {
      tree name = ssa_name (i);
      if (!name)
	continue;

      if (!use_equiv_p
	  && !SSA_NAME_IS_DEFAULT_DEF (name)
	  && prop_simulate_again_p (SSA_NAME_DEF_STMT (name)))
	continue;

      equiv_vr = get_vr_for_comparison (i, &tem_vr, s);
      sop = false;
      tree t = compare_range_with_value (comp, equiv_vr, val, &sop);
      if (t)
	{
	  /* If we get different answers from different members
	     of the equivalence set this check must be in a dead
	     code region.  Folding it to a trap representation
	     would be correct here.  For now just return don't-know.  */
	  if (retval != NULL
	      && t != retval)
	    {
	      retval = NULL_TREE;
	      break;
	    }
	  retval = t;

	  if (!sop)
	    used_strict_overflow = 0;
	  else if (used_strict_overflow < 0)
	    used_strict_overflow = 1;
	}
    }

  if (retval && used_strict_overflow > 0)
    *strict_overflow_p = true;

  return retval;
}


/* Given a comparison code COMP and names N1 and N2, compare all the
   ranges equivalent to N1 against all the ranges equivalent to N2
   to determine the value of N1 COMP N2.  Return the same value
   returned by compare_ranges.  Set *STRICT_OVERFLOW_P to indicate
   whether we relied on undefined signed overflow in the comparison.  */


tree
simplify_using_ranges::compare_names (enum tree_code comp, tree n1, tree n2,
				      bool *strict_overflow_p, gimple *s)
{
  /* Compare the ranges of every name equivalent to N1 against the
     ranges of every name equivalent to N2.  */
  bitmap e1 = query->get_value_range (n1, s)->equiv ();
  bitmap e2 = query->get_value_range (n2, s)->equiv ();

  /* Use the fake bitmaps if e1 or e2 are not available.  */
  static bitmap s_e1 = NULL, s_e2 = NULL;
  static bitmap_obstack *s_obstack = NULL;
  if (s_obstack == NULL)
    {
      s_obstack = XNEW (bitmap_obstack);
      bitmap_obstack_initialize (s_obstack);
      s_e1 = BITMAP_ALLOC (s_obstack);
      s_e2 = BITMAP_ALLOC (s_obstack);
    }
  if (e1 == NULL)
    e1 = s_e1;
  if (e2 == NULL)
    e2 = s_e2;

  /* Add N1 and N2 to their own set of equivalences to avoid
     duplicating the body of the loop just to check N1 and N2
     ranges.  */
  bitmap_set_bit (e1, SSA_NAME_VERSION (n1));
  bitmap_set_bit (e2, SSA_NAME_VERSION (n2));

  /* If the equivalence sets have a common intersection, then the two
     names can be compared without checking their ranges.  */
  if (bitmap_intersect_p (e1, e2))
    {
      bitmap_clear_bit (e1, SSA_NAME_VERSION (n1));
      bitmap_clear_bit (e2, SSA_NAME_VERSION (n2));

      return (comp == EQ_EXPR || comp == GE_EXPR || comp == LE_EXPR)
	     ? boolean_true_node
	     : boolean_false_node;
    }

  /* Start at -1.  Set it to 0 if we do a comparison without relying
     on overflow, or 1 if all comparisons rely on overflow.  */
  int used_strict_overflow = -1;

  /* Otherwise, compare all the equivalent ranges.  First, add N1 and
     N2 to their own set of equivalences to avoid duplicating the body
     of the loop just to check N1 and N2 ranges.  */
  bitmap_iterator bi1;
  unsigned i1;
  EXECUTE_IF_SET_IN_BITMAP (e1, 0, i1, bi1)
    {
      if (!ssa_name (i1))
	continue;

      value_range_equiv tem_vr1;
      const value_range_equiv *vr1 = get_vr_for_comparison (i1, &tem_vr1, s);

      tree t = NULL_TREE, retval = NULL_TREE;
      bitmap_iterator bi2;
      unsigned i2;
      EXECUTE_IF_SET_IN_BITMAP (e2, 0, i2, bi2)
	{
	  if (!ssa_name (i2))
	    continue;

	  bool sop = false;

	  value_range_equiv tem_vr2;
	  const value_range_equiv *vr2 = get_vr_for_comparison (i2, &tem_vr2,
								s);

	  t = compare_ranges (comp, vr1, vr2, &sop);
	  if (t)
	    {
	      /* If we get different answers from different members
		 of the equivalence set this check must be in a dead
		 code region.  Folding it to a trap representation
		 would be correct here.  For now just return don't-know.  */
	      if (retval != NULL && t != retval)
		{
		  bitmap_clear_bit (e1, SSA_NAME_VERSION (n1));
		  bitmap_clear_bit (e2, SSA_NAME_VERSION (n2));
		  return NULL_TREE;
		}
	      retval = t;

	      if (!sop)
		used_strict_overflow = 0;
	      else if (used_strict_overflow < 0)
		used_strict_overflow = 1;
	    }
	}

      if (retval)
	{
	  bitmap_clear_bit (e1, SSA_NAME_VERSION (n1));
	  bitmap_clear_bit (e2, SSA_NAME_VERSION (n2));
	  if (used_strict_overflow > 0)
	    *strict_overflow_p = true;
	  return retval;
	}
    }

  /* None of the equivalent ranges are useful in computing this
     comparison.  */
  bitmap_clear_bit (e1, SSA_NAME_VERSION (n1));
  bitmap_clear_bit (e2, SSA_NAME_VERSION (n2));
  return NULL_TREE;
}

/* Helper function for vrp_evaluate_conditional_warnv & other
   optimizers.  */

tree
simplify_using_ranges::vrp_evaluate_conditional_warnv_with_ops_using_ranges
    (enum tree_code code, tree op0, tree op1, bool * strict_overflow_p,
     gimple *s)
{
  const value_range_equiv *vr0, *vr1;
  vr0 = (TREE_CODE (op0) == SSA_NAME) ? query->get_value_range (op0, s) : NULL;
  vr1 = (TREE_CODE (op1) == SSA_NAME) ? query->get_value_range (op1, s) : NULL;

  tree res = NULL_TREE;
  if (vr0 && vr1)
    res = compare_ranges (code, vr0, vr1, strict_overflow_p);
  if (!res && vr0)
    res = compare_range_with_value (code, vr0, op1, strict_overflow_p);
  if (!res && vr1)
    res = (compare_range_with_value
	    (swap_tree_comparison (code), vr1, op0, strict_overflow_p));
  return res;
}

/* Helper function for vrp_evaluate_conditional_warnv. */

tree
simplify_using_ranges::vrp_evaluate_conditional_warnv_with_ops
						(gimple *stmt,
						 enum tree_code code,
						 tree op0, tree op1,
						 bool use_equiv_p,
						 bool *strict_overflow_p,
						 bool *only_ranges)
{
  tree ret;
  if (only_ranges)
    *only_ranges = true;

  /* We only deal with integral and pointer types.  */
  if (!INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && !POINTER_TYPE_P (TREE_TYPE (op0)))
    return NULL_TREE;

  /* If OP0 CODE OP1 is an overflow comparison, if it can be expressed
     as a simple equality test, then prefer that over its current form
     for evaluation.

     An overflow test which collapses to an equality test can always be
     expressed as a comparison of one argument against zero.  Overflow
     occurs when the chosen argument is zero and does not occur if the
     chosen argument is not zero.  */
  tree x;
  if (overflow_comparison_p (code, op0, op1, use_equiv_p, &x))
    {
      wide_int max = wi::max_value (TYPE_PRECISION (TREE_TYPE (op0)), UNSIGNED);
      /* B = A - 1; if (A < B) -> B = A - 1; if (A == 0)
         B = A - 1; if (A > B) -> B = A - 1; if (A != 0)
         B = A + 1; if (B < A) -> B = A + 1; if (B == 0)
         B = A + 1; if (B > A) -> B = A + 1; if (B != 0) */
      if (integer_zerop (x))
	{
	  op1 = x;
	  code = (code == LT_EXPR || code == LE_EXPR) ? EQ_EXPR : NE_EXPR;
	}
      /* B = A + 1; if (A > B) -> B = A + 1; if (B == 0)
         B = A + 1; if (A < B) -> B = A + 1; if (B != 0)
         B = A - 1; if (B > A) -> B = A - 1; if (A == 0)
         B = A - 1; if (B < A) -> B = A - 1; if (A != 0) */
      else if (wi::to_wide (x) == max - 1)
	{
	  op0 = op1;
	  op1 = wide_int_to_tree (TREE_TYPE (op0), 0);
	  code = (code == GT_EXPR || code == GE_EXPR) ? EQ_EXPR : NE_EXPR;
	}
      else
	{
	  value_range vro, vri;
	  if (code == GT_EXPR || code == GE_EXPR)
	    {
	      vro.set (TYPE_MIN_VALUE (TREE_TYPE (op0)), x, VR_ANTI_RANGE);
	      vri.set (TYPE_MIN_VALUE (TREE_TYPE (op0)), x);
	    }
	  else if (code == LT_EXPR || code == LE_EXPR)
	    {
	      vro.set (TYPE_MIN_VALUE (TREE_TYPE (op0)), x);
	      vri.set (TYPE_MIN_VALUE (TREE_TYPE (op0)), x, VR_ANTI_RANGE);
	    }
	  else
	    gcc_unreachable ();
	  const value_range_equiv *vr0 = query->get_value_range (op0, stmt);
	  /* If vro, the range for OP0 to pass the overflow test, has
	     no intersection with *vr0, OP0's known range, then the
	     overflow test can't pass, so return the node for false.
	     If it is the inverted range, vri, that has no
	     intersection, then the overflow test must pass, so return
	     the node for true.  In other cases, we could proceed with
	     a simplified condition comparing OP0 and X, with LE_EXPR
	     for previously LE_ or LT_EXPR and GT_EXPR otherwise, but
	     the comments next to the enclosing if suggest it's not
	     generally profitable to do so.  */
	  vro.intersect (vr0);
	  if (vro.undefined_p ())
	    return boolean_false_node;
	  vri.intersect (vr0);
	  if (vri.undefined_p ())
	    return boolean_true_node;
	}
    }

  if ((ret = vrp_evaluate_conditional_warnv_with_ops_using_ranges
	       (code, op0, op1, strict_overflow_p, stmt)))
    return ret;
  if (only_ranges)
    *only_ranges = false;
  /* Do not use compare_names during propagation, it's quadratic.  */
  if (TREE_CODE (op0) == SSA_NAME && TREE_CODE (op1) == SSA_NAME
      && use_equiv_p)
    return compare_names (code, op0, op1, strict_overflow_p, stmt);
  else if (TREE_CODE (op0) == SSA_NAME)
    return compare_name_with_value (code, op0, op1,
				    strict_overflow_p, use_equiv_p, stmt);
  else if (TREE_CODE (op1) == SSA_NAME)
    return compare_name_with_value (swap_tree_comparison (code), op1, op0,
				    strict_overflow_p, use_equiv_p, stmt);
  return NULL_TREE;
}

/* Given (CODE OP0 OP1) within STMT, try to simplify it based on value range
   information.  Return NULL if the conditional cannot be evaluated.
   The ranges of all the names equivalent with the operands in COND
   will be used when trying to compute the value.  If the result is
   based on undefined signed overflow, issue a warning if
   appropriate.  */

tree
simplify_using_ranges::vrp_evaluate_conditional (tree_code code, tree op0,
						 tree op1, gimple *stmt)
{
  bool sop;
  tree ret;
  bool only_ranges;

  /* Some passes and foldings leak constants with overflow flag set
     into the IL.  Avoid doing wrong things with these and bail out.  */
  if ((TREE_CODE (op0) == INTEGER_CST
       && TREE_OVERFLOW (op0))
      || (TREE_CODE (op1) == INTEGER_CST
	  && TREE_OVERFLOW (op1)))
    return NULL_TREE;

  sop = false;
  ret = vrp_evaluate_conditional_warnv_with_ops (stmt, code, op0, op1, true,
						 &sop, &only_ranges);

  if (ret && sop)
    {
      enum warn_strict_overflow_code wc;
      const char* warnmsg;

      if (is_gimple_min_invariant (ret))
	{
	  wc = WARN_STRICT_OVERFLOW_CONDITIONAL;
	  warnmsg = G_("assuming signed overflow does not occur when "
		       "simplifying conditional to constant");
	}
      else
	{
	  wc = WARN_STRICT_OVERFLOW_COMPARISON;
	  warnmsg = G_("assuming signed overflow does not occur when "
		       "simplifying conditional");
	}

      if (issue_strict_overflow_warning (wc))
	{
	  location_t location;

	  if (!gimple_has_location (stmt))
	    location = input_location;
	  else
	    location = gimple_location (stmt);
	  warning_at (location, OPT_Wstrict_overflow, "%s", warnmsg);
	}
    }

  if (warn_type_limits
      && ret && only_ranges
      && TREE_CODE_CLASS (code) == tcc_comparison
      && TREE_CODE (op0) == SSA_NAME)
    {
      /* If the comparison is being folded and the operand on the LHS
	 is being compared against a constant value that is outside of
	 the natural range of OP0's type, then the predicate will
	 always fold regardless of the value of OP0.  If -Wtype-limits
	 was specified, emit a warning.  */
      tree type = TREE_TYPE (op0);
      const value_range_equiv *vr0 = query->get_value_range (op0, stmt);

      if (vr0->varying_p ()
	  && INTEGRAL_TYPE_P (type)
	  && is_gimple_min_invariant (op1))
	{
	  location_t location;

	  if (!gimple_has_location (stmt))
	    location = input_location;
	  else
	    location = gimple_location (stmt);

	  warning_at (location, OPT_Wtype_limits,
		      integer_zerop (ret)
		      ? G_("comparison always false "
                           "due to limited range of data type")
		      : G_("comparison always true "
                           "due to limited range of data type"));
	}
    }

  return ret;
}


/* Visit conditional statement STMT.  If we can determine which edge
   will be taken out of STMT's basic block, record it in
   *TAKEN_EDGE_P.  Otherwise, set *TAKEN_EDGE_P to NULL.  */

void
simplify_using_ranges::vrp_visit_cond_stmt (gcond *stmt, edge *taken_edge_p)
{
  tree val;

  *taken_edge_p = NULL;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      tree use;
      ssa_op_iter i;

      fprintf (dump_file, "\nVisiting conditional with predicate: ");
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "\nWith known ranges\n");

      FOR_EACH_SSA_TREE_OPERAND (use, stmt, i, SSA_OP_USE)
	{
	  fprintf (dump_file, "\t");
	  print_generic_expr (dump_file, use);
	  fprintf (dump_file, ": ");
	  dump_value_range (dump_file, query->get_value_range (use, stmt));
	}

      fprintf (dump_file, "\n");
    }

  /* Compute the value of the predicate COND by checking the known
     ranges of each of its operands.

     Note that we cannot evaluate all the equivalent ranges here
     because those ranges may not yet be final and with the current
     propagation strategy, we cannot determine when the value ranges
     of the names in the equivalence set have changed.

     For instance, given the following code fragment

        i_5 = PHI <8, i_13>
	...
     	i_14 = ASSERT_EXPR <i_5, i_5 != 0>
	if (i_14 == 1)
	  ...

     Assume that on the first visit to i_14, i_5 has the temporary
     range [8, 8] because the second argument to the PHI function is
     not yet executable.  We derive the range ~[0, 0] for i_14 and the
     equivalence set { i_5 }.  So, when we visit 'if (i_14 == 1)' for
     the first time, since i_14 is equivalent to the range [8, 8], we
     determine that the predicate is always false.

     On the next round of propagation, i_13 is determined to be
     VARYING, which causes i_5 to drop down to VARYING.  So, another
     visit to i_14 is scheduled.  In this second visit, we compute the
     exact same range and equivalence set for i_14, namely ~[0, 0] and
     { i_5 }.  But we did not have the previous range for i_5
     registered, so vrp_visit_assignment thinks that the range for
     i_14 has not changed.  Therefore, the predicate 'if (i_14 == 1)'
     is not visited again, which stops propagation from visiting
     statements in the THEN clause of that if().

     To properly fix this we would need to keep the previous range
     value for the names in the equivalence set.  This way we would've
     discovered that from one visit to the other i_5 changed from
     range [8, 8] to VR_VARYING.

     However, fixing this apparent limitation may not be worth the
     additional checking.  Testing on several code bases (GCC, DLV,
     MICO, TRAMP3D and SPEC2000) showed that doing this results in
     4 more predicates folded in SPEC.  */

  bool sop;
  val = vrp_evaluate_conditional_warnv_with_ops (stmt,
						 gimple_cond_code (stmt),
						 gimple_cond_lhs (stmt),
						 gimple_cond_rhs (stmt),
						 false, &sop, NULL);
  if (val)
    *taken_edge_p = find_taken_edge (gimple_bb (stmt), val);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPredicate evaluates to: ");
      if (val == NULL_TREE)
	fprintf (dump_file, "DON'T KNOW\n");
      else
	print_generic_stmt (dump_file, val);
    }
}

/* Searches the case label vector VEC for the ranges of CASE_LABELs that are
   used in range VR.  The indices are placed in MIN_IDX1, MAX_IDX, MIN_IDX2 and
   MAX_IDX2.  If the ranges of CASE_LABELs are empty then MAX_IDX1 < MIN_IDX1.
   Returns true if the default label is not needed.  */

static bool
find_case_label_ranges (gswitch *stmt, const value_range *vr,
			size_t *min_idx1, size_t *max_idx1,
			size_t *min_idx2, size_t *max_idx2)
{
  size_t i, j, k, l;
  unsigned int n = gimple_switch_num_labels (stmt);
  bool take_default;
  tree case_low, case_high;
  tree min = vr->min (), max = vr->max ();

  gcc_checking_assert (!vr->varying_p () && !vr->undefined_p ());

  take_default = !find_case_label_range (stmt, min, max, &i, &j);

  /* Set second range to empty.  */
  *min_idx2 = 1;
  *max_idx2 = 0;

  if (vr->kind () == VR_RANGE)
    {
      *min_idx1 = i;
      *max_idx1 = j;
      return !take_default;
    }

  /* Set first range to all case labels.  */
  *min_idx1 = 1;
  *max_idx1 = n - 1;

  if (i > j)
    return false;

  /* Make sure all the values of case labels [i , j] are contained in
     range [MIN, MAX].  */
  case_low = CASE_LOW (gimple_switch_label (stmt, i));
  case_high = CASE_HIGH (gimple_switch_label (stmt, j));
  if (tree_int_cst_compare (case_low, min) < 0)
    i += 1;
  if (case_high != NULL_TREE
      && tree_int_cst_compare (max, case_high) < 0)
    j -= 1;

  if (i > j)
    return false;

  /* If the range spans case labels [i, j], the corresponding anti-range spans
     the labels [1, i - 1] and [j + 1, n -  1].  */
  k = j + 1;
  l = n - 1;
  if (k > l)
    {
      k = 1;
      l = 0;
    }

  j = i - 1;
  i = 1;
  if (i > j)
    {
      i = k;
      j = l;
      k = 1;
      l = 0;
    }

  *min_idx1 = i;
  *max_idx1 = j;
  *min_idx2 = k;
  *max_idx2 = l;
  return false;
}

/* Visit switch statement STMT.  If we can determine which edge
   will be taken out of STMT's basic block, record it in
   *TAKEN_EDGE_P.  Otherwise, *TAKEN_EDGE_P set to NULL.  */

void
vr_values::vrp_visit_switch_stmt (gswitch *stmt, edge *taken_edge_p)
{
  tree op, val;
  const value_range_equiv *vr;
  size_t i = 0, j = 0, k, l;
  bool take_default;

  *taken_edge_p = NULL;
  op = gimple_switch_index (stmt);
  if (TREE_CODE (op) != SSA_NAME)
    return;

  vr = get_value_range (op);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting switch expression with operand ");
      print_generic_expr (dump_file, op);
      fprintf (dump_file, " with known range ");
      dump_value_range (dump_file, vr);
      fprintf (dump_file, "\n");
    }

  if (vr->undefined_p ()
      || vr->varying_p ()
      || vr->symbolic_p ())
    return;

  /* Find the single edge that is taken from the switch expression.  */
  take_default = !find_case_label_ranges (stmt, vr, &i, &j, &k, &l);

  /* Check if the range spans no CASE_LABEL. If so, we only reach the default
     label */
  if (j < i)
    {
      gcc_assert (take_default);
      val = gimple_switch_default_label (stmt);
    }
  else
    {
      /* Check if labels with index i to j and maybe the default label
	 are all reaching the same label.  */

      val = gimple_switch_label (stmt, i);
      if (take_default
	  && CASE_LABEL (gimple_switch_default_label (stmt))
	  != CASE_LABEL (val))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  not a single destination for this "
		     "range\n");
	  return;
	}
      for (++i; i <= j; ++i)
        {
          if (CASE_LABEL (gimple_switch_label (stmt, i)) != CASE_LABEL (val))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  not a single destination for this "
			 "range\n");
	      return;
	    }
        }
      for (; k <= l; ++k)
        {
          if (CASE_LABEL (gimple_switch_label (stmt, k)) != CASE_LABEL (val))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "  not a single destination for this "
			 "range\n");
	      return;
	    }
        }
    }

  *taken_edge_p = find_edge (gimple_bb (stmt),
			     label_to_block (cfun, CASE_LABEL (val)));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "  will take edge to ");
      print_generic_stmt (dump_file, CASE_LABEL (val));
    }
}


/* Evaluate statement STMT.  If the statement produces a useful range,
   set VR and corepsponding OUTPUT_P.

   If STMT is a conditional branch and we can determine its truth
   value, the taken edge is recorded in *TAKEN_EDGE_P.  */

void
vr_values::extract_range_from_stmt (gimple *stmt, edge *taken_edge_p,
				    tree *output_p, value_range_equiv *vr)
{

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nextract_range_from_stmt visiting:\n");
      print_gimple_stmt (dump_file, stmt, 0, dump_flags);
    }

  if (!stmt_interesting_for_vrp (stmt))
    gcc_assert (stmt_ends_bb_p (stmt));
  else if (is_gimple_assign (stmt) || is_gimple_call (stmt))
    vrp_visit_assignment_or_call (stmt, output_p, vr);
  else if (gimple_code (stmt) == GIMPLE_COND)
    simplifier.vrp_visit_cond_stmt (as_a <gcond *> (stmt), taken_edge_p);
  else if (gimple_code (stmt) == GIMPLE_SWITCH)
    vrp_visit_switch_stmt (as_a <gswitch *> (stmt), taken_edge_p);
}

/* Visit all arguments for PHI node PHI that flow through executable
   edges.  If a valid value range can be derived from all the incoming
   value ranges, set a new range in VR_RESULT.  */

void
vr_values::extract_range_from_phi_node (gphi *phi,
					value_range_equiv *vr_result)
{
  tree lhs = PHI_RESULT (phi);
  const value_range_equiv *lhs_vr = get_value_range (lhs);
  bool first = true;
  int old_edges;
  class loop *l;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_gimple_stmt (dump_file, phi, 0, dump_flags);
    }

  bool may_simulate_backedge_again = false;
  int edges = 0;
  for (size_t i = 0; i < gimple_phi_num_args (phi); i++)
    {
      edge e = gimple_phi_arg_edge (phi, i);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
	      "    Argument #%d (%d -> %d %sexecutable)\n",
	      (int) i, e->src->index, e->dest->index,
	      (e->flags & EDGE_EXECUTABLE) ? "" : "not ");
	}

      if (e->flags & EDGE_EXECUTABLE)
	{
	  value_range_equiv vr_arg_tem;
	  const value_range_equiv *vr_arg = &vr_arg_tem;

	  ++edges;

	  tree arg = PHI_ARG_DEF (phi, i);
	  if (TREE_CODE (arg) == SSA_NAME)
	    {
	      /* See if we are eventually going to change one of the args.  */
	      gimple *def_stmt = SSA_NAME_DEF_STMT (arg);
	      if (! gimple_nop_p (def_stmt)
		  && prop_simulate_again_p (def_stmt)
		  && e->flags & EDGE_DFS_BACK)
		may_simulate_backedge_again = true;

	      const value_range_equiv *vr_arg_ = get_value_range (arg);
	      /* Do not allow equivalences or symbolic ranges to leak in from
		 backedges.  That creates invalid equivalencies.
		 See PR53465 and PR54767.  */
	      if (e->flags & EDGE_DFS_BACK)
		{
		  if (!vr_arg_->varying_p () && !vr_arg_->undefined_p ())
		    {
		      vr_arg_tem.set (vr_arg_->min (), vr_arg_->max (), NULL,
				      vr_arg_->kind ());
		      if (vr_arg_tem.symbolic_p ())
			vr_arg_tem.set_varying (TREE_TYPE (arg));
		    }
		  else
		    vr_arg = vr_arg_;
		}
	      /* If the non-backedge arguments range is VR_VARYING then
		 we can still try recording a simple equivalence.  */
	      else if (vr_arg_->varying_p ())
		vr_arg_tem.set (arg);
	      else
		vr_arg = vr_arg_;
	    }
	  else
	    {
	      if (TREE_OVERFLOW_P (arg))
		arg = drop_tree_overflow (arg);

	      vr_arg_tem.set (arg);
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\t");
	      print_generic_expr (dump_file, arg, dump_flags);
	      fprintf (dump_file, ": ");
	      dump_value_range (dump_file, vr_arg);
	      fprintf (dump_file, "\n");
	    }

	  if (first)
	    vr_result->deep_copy (vr_arg);
	  else
	    vr_result->union_ (vr_arg);
	  first = false;

	  if (vr_result->varying_p ())
	    break;
	}
    }

  if (vr_result->varying_p ())
    goto varying;
  else if (vr_result->undefined_p ())
    goto update_range;

  old_edges = vr_phi_edge_counts[SSA_NAME_VERSION (lhs)];
  vr_phi_edge_counts[SSA_NAME_VERSION (lhs)] = edges;

  /* To prevent infinite iterations in the algorithm, derive ranges
     when the new value is slightly bigger or smaller than the
     previous one.  We don't do this if we have seen a new executable
     edge; this helps us avoid an infinity for conditionals
     which are not in a loop.  If the old value-range was VR_UNDEFINED
     use the updated range and iterate one more time.  If we will not
     simulate this PHI again via the backedge allow us to iterate.  */
  if (edges > 0
      && gimple_phi_num_args (phi) > 1
      && edges == old_edges
      && !lhs_vr->undefined_p ()
      && may_simulate_backedge_again)
    {
      /* Compare old and new ranges, fall back to varying if the
         values are not comparable.  */
      int cmp_min = compare_values (lhs_vr->min (), vr_result->min ());
      if (cmp_min == -2)
	goto varying;
      int cmp_max = compare_values (lhs_vr->max (), vr_result->max ());
      if (cmp_max == -2)
	goto varying;

      /* For non VR_RANGE or for pointers fall back to varying if
	 the range changed.  */
      if ((lhs_vr->kind () != VR_RANGE || vr_result->kind () != VR_RANGE
	   || POINTER_TYPE_P (TREE_TYPE (lhs)))
	  && (cmp_min != 0 || cmp_max != 0))
	goto varying;

      /* If the new minimum is larger than the previous one
	 retain the old value.  If the new minimum value is smaller
	 than the previous one and not -INF go all the way to -INF + 1.
	 In the first case, to avoid infinite bouncing between different
	 minimums, and in the other case to avoid iterating millions of
	 times to reach -INF.  Going to -INF + 1 also lets the following
	 iteration compute whether there will be any overflow, at the
	 expense of one additional iteration.  */
      tree new_min = vr_result->min ();
      tree new_max = vr_result->max ();
      if (cmp_min < 0)
	new_min = lhs_vr->min ();
      else if (cmp_min > 0
	       && (TREE_CODE (vr_result->min ()) != INTEGER_CST
		   || tree_int_cst_lt (vrp_val_min (vr_result->type ()),
				       vr_result->min ())))
	new_min = int_const_binop (PLUS_EXPR,
				   vrp_val_min (vr_result->type ()),
				   build_int_cst (vr_result->type (), 1));

      /* Similarly for the maximum value.  */
      if (cmp_max > 0)
	new_max = lhs_vr->max ();
      else if (cmp_max < 0
	       && (TREE_CODE (vr_result->max ()) != INTEGER_CST
		   || tree_int_cst_lt (vr_result->max (),
				       vrp_val_max (vr_result->type ()))))
	new_max = int_const_binop (MINUS_EXPR,
				   vrp_val_max (vr_result->type ()),
				   build_int_cst (vr_result->type (), 1));

      vr_result->update (new_min, new_max, vr_result->kind ());

      /* If we dropped either bound to +-INF then if this is a loop
	 PHI node SCEV may known more about its value-range.  */
      if (cmp_min > 0 || cmp_min < 0
	   || cmp_max < 0 || cmp_max > 0)
	goto scev_check;

      goto infinite_check;
    }

  goto update_range;

varying:
  vr_result->set_varying (TREE_TYPE (lhs));

scev_check:
  /* If this is a loop PHI node SCEV may known more about its value-range.
     scev_check can be reached from two paths, one is a fall through from above
     "varying" label, the other is direct goto from code block which tries to
     avoid infinite simulation.  */
  if (scev_initialized_p ()
      && (l = loop_containing_stmt (phi))
      && l->header == gimple_bb (phi))
    adjust_range_with_scev (vr_result, l, phi, lhs);

infinite_check:
  /* If we will end up with a (-INF, +INF) range, set it to
     VARYING.  Same if the previous max value was invalid for
     the type and we end up with vr_result.min > vr_result.max.  */
  if ((!vr_result->varying_p () && !vr_result->undefined_p ())
      && !((vrp_val_is_max (vr_result->max ()) && vrp_val_is_min (vr_result->min ()))
	   || compare_values (vr_result->min (), vr_result->max ()) > 0))
    ;
  else
    vr_result->set_varying (TREE_TYPE (lhs));

  /* If the new range is different than the previous value, keep
     iterating.  */
update_range:
  return;
}

/* Simplify boolean operations if the source is known
   to be already a boolean.  */
bool
simplify_using_ranges::simplify_truth_ops_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree lhs, op0, op1;
  bool need_conversion;

  /* We handle only !=/== case here.  */
  gcc_assert (rhs_code == EQ_EXPR || rhs_code == NE_EXPR);

  op0 = gimple_assign_rhs1 (stmt);
  if (!op_with_boolean_value_range_p (op0, stmt))
    return false;

  op1 = gimple_assign_rhs2 (stmt);
  if (!op_with_boolean_value_range_p (op1, stmt))
    return false;

  /* Reduce number of cases to handle to NE_EXPR.  As there is no
     BIT_XNOR_EXPR we cannot replace A == B with a single statement.  */
  if (rhs_code == EQ_EXPR)
    {
      if (TREE_CODE (op1) == INTEGER_CST)
	op1 = int_const_binop (BIT_XOR_EXPR, op1,
			       build_int_cst (TREE_TYPE (op1), 1));
      else
	return false;
    }

  lhs = gimple_assign_lhs (stmt);
  need_conversion
    = !useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (op0));

  /* Make sure to not sign-extend a 1-bit 1 when converting the result.  */
  if (need_conversion
      && !TYPE_UNSIGNED (TREE_TYPE (op0))
      && TYPE_PRECISION (TREE_TYPE (op0)) == 1
      && TYPE_PRECISION (TREE_TYPE (lhs)) > 1)
    return false;

  /* For A != 0 we can substitute A itself.  */
  if (integer_zerop (op1))
    gimple_assign_set_rhs_with_ops (gsi,
				    need_conversion
				    ? NOP_EXPR : TREE_CODE (op0), op0);
  /* For A != B we substitute A ^ B.  Either with conversion.  */
  else if (need_conversion)
    {
      tree tem = make_ssa_name (TREE_TYPE (op0));
      gassign *newop
	= gimple_build_assign (tem, BIT_XOR_EXPR, op0, op1);
      gsi_insert_before (gsi, newop, GSI_SAME_STMT);
      if (INTEGRAL_TYPE_P (TREE_TYPE (tem))
	  && TYPE_PRECISION (TREE_TYPE (tem)) > 1)
	set_range_info (tem, VR_RANGE,
			wi::zero (TYPE_PRECISION (TREE_TYPE (tem))),
			wi::one (TYPE_PRECISION (TREE_TYPE (tem))));
      gimple_assign_set_rhs_with_ops (gsi, NOP_EXPR, tem);
    }
  /* Or without.  */
  else
    gimple_assign_set_rhs_with_ops (gsi, BIT_XOR_EXPR, op0, op1);
  update_stmt (gsi_stmt (*gsi));
  fold_stmt (gsi, follow_single_use_edges);

  return true;
}

/* Simplify a division or modulo operator to a right shift or bitwise and
   if the first operand is unsigned or is greater than zero and the second
   operand is an exact power of two.  For TRUNC_MOD_EXPR op0 % op1 with
   constant op1 (op1min = op1) or with op1 in [op1min, op1max] range,
   optimize it into just op0 if op0's range is known to be a subset of
   [-op1min + 1, op1min - 1] for signed and [0, op1min - 1] for unsigned
   modulo.  */

bool
simplify_using_ranges::simplify_div_or_mod_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree val = NULL;
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  tree op0min = NULL_TREE, op0max = NULL_TREE;
  tree op1min = op1;
  const value_range *vr = NULL;

  if (TREE_CODE (op0) == INTEGER_CST)
    {
      op0min = op0;
      op0max = op0;
    }
  else
    {
      vr = query->get_value_range (op0, stmt);
      if (range_int_cst_p (vr))
	{
	  op0min = vr->min ();
	  op0max = vr->max ();
	}
    }

  if (rhs_code == TRUNC_MOD_EXPR
      && TREE_CODE (op1) == SSA_NAME)
    {
      const value_range_equiv *vr1 = query->get_value_range (op1, stmt);
      if (range_int_cst_p (vr1))
	op1min = vr1->min ();
    }
  if (rhs_code == TRUNC_MOD_EXPR
      && TREE_CODE (op1min) == INTEGER_CST
      && tree_int_cst_sgn (op1min) == 1
      && op0max
      && tree_int_cst_lt (op0max, op1min))
    {
      if (TYPE_UNSIGNED (TREE_TYPE (op0))
	  || tree_int_cst_sgn (op0min) >= 0
	  || tree_int_cst_lt (fold_unary (NEGATE_EXPR, TREE_TYPE (op1min), op1min),
			      op0min))
	{
	  /* If op0 already has the range op0 % op1 has,
	     then TRUNC_MOD_EXPR won't change anything.  */
	  gimple_assign_set_rhs_from_tree (gsi, op0);
	  return true;
	}
    }

  if (TREE_CODE (op0) != SSA_NAME)
    return false;

  if (!integer_pow2p (op1))
    {
      /* X % -Y can be only optimized into X % Y either if
	 X is not INT_MIN, or Y is not -1.  Fold it now, as after
	 remove_range_assertions the range info might be not available
	 anymore.  */
      if (rhs_code == TRUNC_MOD_EXPR
	  && fold_stmt (gsi, follow_single_use_edges))
	return true;
      return false;
    }

  if (TYPE_UNSIGNED (TREE_TYPE (op0)))
    val = integer_one_node;
  else
    {
      bool sop = false;

      val = compare_range_with_value (GE_EXPR, vr, integer_zero_node, &sop);

      if (val
	  && sop
	  && integer_onep (val)
	  && issue_strict_overflow_warning (WARN_STRICT_OVERFLOW_MISC))
	{
	  location_t location;

	  if (!gimple_has_location (stmt))
	    location = input_location;
	  else
	    location = gimple_location (stmt);
	  warning_at (location, OPT_Wstrict_overflow,
		      "assuming signed overflow does not occur when "
		      "simplifying %</%> or %<%%%> to %<>>%> or %<&%>");
	}
    }

  if (val && integer_onep (val))
    {
      tree t;

      if (rhs_code == TRUNC_DIV_EXPR)
	{
	  t = build_int_cst (integer_type_node, tree_log2 (op1));
	  gimple_assign_set_rhs_code (stmt, RSHIFT_EXPR);
	  gimple_assign_set_rhs1 (stmt, op0);
	  gimple_assign_set_rhs2 (stmt, t);
	}
      else
	{
	  t = build_int_cst (TREE_TYPE (op1), 1);
	  t = int_const_binop (MINUS_EXPR, op1, t);
	  t = fold_convert (TREE_TYPE (op0), t);

	  gimple_assign_set_rhs_code (stmt, BIT_AND_EXPR);
	  gimple_assign_set_rhs1 (stmt, op0);
	  gimple_assign_set_rhs2 (stmt, t);
	}

      update_stmt (stmt);
      fold_stmt (gsi, follow_single_use_edges);
      return true;
    }

  return false;
}

/* Simplify a min or max if the ranges of the two operands are
   disjoint.   Return true if we do simplify.  */

bool
simplify_using_ranges::simplify_min_or_max_using_ranges
				(gimple_stmt_iterator *gsi,
				 gimple *stmt)
{
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  bool sop = false;
  tree val;

  val = (vrp_evaluate_conditional_warnv_with_ops_using_ranges
	 (LE_EXPR, op0, op1, &sop, stmt));
  if (!val)
    {
      sop = false;
      val = (vrp_evaluate_conditional_warnv_with_ops_using_ranges
	     (LT_EXPR, op0, op1, &sop, stmt));
    }

  if (val)
    {
      if (sop && issue_strict_overflow_warning (WARN_STRICT_OVERFLOW_MISC))
	{
	  location_t location;

	  if (!gimple_has_location (stmt))
	    location = input_location;
	  else
	    location = gimple_location (stmt);
	  warning_at (location, OPT_Wstrict_overflow,
		      "assuming signed overflow does not occur when "
		      "simplifying %<min/max (X,Y)%> to %<X%> or %<Y%>");
	}

      /* VAL == TRUE -> OP0 < or <= op1
	 VAL == FALSE -> OP0 > or >= op1.  */
      tree res = ((gimple_assign_rhs_code (stmt) == MAX_EXPR)
		  == integer_zerop (val)) ? op0 : op1;
      gimple_assign_set_rhs_from_tree (gsi, res);
      return true;
    }

  return false;
}

/* If the operand to an ABS_EXPR is >= 0, then eliminate the
   ABS_EXPR.  If the operand is <= 0, then simplify the
   ABS_EXPR into a NEGATE_EXPR.  */

bool
simplify_using_ranges::simplify_abs_using_ranges (gimple_stmt_iterator *gsi,
						  gimple *stmt)
{
  tree op = gimple_assign_rhs1 (stmt);
  const value_range *vr = query->get_value_range (op, stmt);

  if (vr)
    {
      tree val = NULL;
      bool sop = false;

      val = compare_range_with_value (LE_EXPR, vr, integer_zero_node, &sop);
      if (!val)
	{
	  /* The range is neither <= 0 nor > 0.  Now see if it is
	     either < 0 or >= 0.  */
	  sop = false;
	  val = compare_range_with_value (LT_EXPR, vr, integer_zero_node,
					  &sop);
	}

      if (val)
	{
	  if (sop && issue_strict_overflow_warning (WARN_STRICT_OVERFLOW_MISC))
	    {
	      location_t location;

	      if (!gimple_has_location (stmt))
		location = input_location;
	      else
		location = gimple_location (stmt);
	      warning_at (location, OPT_Wstrict_overflow,
			  "assuming signed overflow does not occur when "
			  "simplifying %<abs (X)%> to %<X%> or %<-X%>");
	    }

	  gimple_assign_set_rhs1 (stmt, op);
	  if (integer_zerop (val))
	    gimple_assign_set_rhs_code (stmt, SSA_NAME);
	  else
	    gimple_assign_set_rhs_code (stmt, NEGATE_EXPR);
	  update_stmt (stmt);
	  fold_stmt (gsi, follow_single_use_edges);
	  return true;
	}
    }

  return false;
}

/* value_range wrapper for wi_set_zero_nonzero_bits.

   Return TRUE if VR was a constant range and we were able to compute
   the bit masks.  */

static bool
vr_set_zero_nonzero_bits (const tree expr_type,
			  const value_range *vr,
			  wide_int *may_be_nonzero,
			  wide_int *must_be_nonzero)
{
  if (range_int_cst_p (vr))
    {
      wi_set_zero_nonzero_bits (expr_type,
				wi::to_wide (vr->min ()),
				wi::to_wide (vr->max ()),
				*may_be_nonzero, *must_be_nonzero);
      return true;
    }
  *may_be_nonzero = wi::minus_one (TYPE_PRECISION (expr_type));
  *must_be_nonzero = wi::zero (TYPE_PRECISION (expr_type));
  return false;
}

/* Optimize away redundant BIT_AND_EXPR and BIT_IOR_EXPR.
   If all the bits that are being cleared by & are already
   known to be zero from VR, or all the bits that are being
   set by | are already known to be one from VR, the bit
   operation is redundant.  */

bool
simplify_using_ranges::simplify_bit_ops_using_ranges
				(gimple_stmt_iterator *gsi,
				 gimple *stmt)
{
  tree op0 = gimple_assign_rhs1 (stmt);
  tree op1 = gimple_assign_rhs2 (stmt);
  tree op = NULL_TREE;
  value_range vr0, vr1;
  wide_int may_be_nonzero0, may_be_nonzero1;
  wide_int must_be_nonzero0, must_be_nonzero1;
  wide_int mask;

  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = *(query->get_value_range (op0, stmt));
  else if (is_gimple_min_invariant (op0))
    vr0.set (op0);
  else
    return false;

  if (TREE_CODE (op1) == SSA_NAME)
    vr1 = *(query->get_value_range (op1, stmt));
  else if (is_gimple_min_invariant (op1))
    vr1.set (op1);
  else
    return false;

  if (!vr_set_zero_nonzero_bits (TREE_TYPE (op0), &vr0, &may_be_nonzero0,
				 &must_be_nonzero0))
    return false;
  if (!vr_set_zero_nonzero_bits (TREE_TYPE (op1), &vr1, &may_be_nonzero1,
				 &must_be_nonzero1))
    return false;

  switch (gimple_assign_rhs_code (stmt))
    {
    case BIT_AND_EXPR:
      mask = wi::bit_and_not (may_be_nonzero0, must_be_nonzero1);
      if (mask == 0)
	{
	  op = op0;
	  break;
	}
      mask = wi::bit_and_not (may_be_nonzero1, must_be_nonzero0);
      if (mask == 0)
	{
	  op = op1;
	  break;
	}
      break;
    case BIT_IOR_EXPR:
      mask = wi::bit_and_not (may_be_nonzero0, must_be_nonzero1);
      if (mask == 0)
	{
	  op = op1;
	  break;
	}
      mask = wi::bit_and_not (may_be_nonzero1, must_be_nonzero0);
      if (mask == 0)
	{
	  op = op0;
	  break;
	}
      break;
    default:
      gcc_unreachable ();
    }

  if (op == NULL_TREE)
    return false;

  gimple_assign_set_rhs_with_ops (gsi, TREE_CODE (op), op);
  update_stmt (gsi_stmt (*gsi));
  return true;
}

/* We are comparing trees OP0 and OP1 using COND_CODE.  OP0 has
   a known value range VR.

   If there is one and only one value which will satisfy the
   conditional, then return that value.  Else return NULL.

   If signed overflow must be undefined for the value to satisfy
   the conditional, then set *STRICT_OVERFLOW_P to true.  */

static tree
test_for_singularity (enum tree_code cond_code, tree op0,
		      tree op1, const value_range *vr)
{
  tree min = NULL;
  tree max = NULL;

  /* Extract minimum/maximum values which satisfy the conditional as it was
     written.  */
  if (cond_code == LE_EXPR || cond_code == LT_EXPR)
    {
      min = TYPE_MIN_VALUE (TREE_TYPE (op0));

      max = op1;
      if (cond_code == LT_EXPR)
	{
	  tree one = build_int_cst (TREE_TYPE (op0), 1);
	  max = fold_build2 (MINUS_EXPR, TREE_TYPE (op0), max, one);
	  /* Signal to compare_values_warnv this expr doesn't overflow.  */
	  if (EXPR_P (max))
	    suppress_warning (max, OPT_Woverflow);
	}
    }
  else if (cond_code == GE_EXPR || cond_code == GT_EXPR)
    {
      max = TYPE_MAX_VALUE (TREE_TYPE (op0));

      min = op1;
      if (cond_code == GT_EXPR)
	{
	  tree one = build_int_cst (TREE_TYPE (op0), 1);
	  min = fold_build2 (PLUS_EXPR, TREE_TYPE (op0), min, one);
	  /* Signal to compare_values_warnv this expr doesn't overflow.  */
	  if (EXPR_P (min))
	    suppress_warning (min, OPT_Woverflow);
	}
    }

  /* Now refine the minimum and maximum values using any
     value range information we have for op0.  */
  if (min && max)
    {
      tree type = TREE_TYPE (op0);
      tree tmin = wide_int_to_tree (type, vr->lower_bound ());
      tree tmax = wide_int_to_tree (type, vr->upper_bound ());
      if (compare_values (tmin, min) == 1)
	min = tmin;
      if (compare_values (tmax, max) == -1)
	max = tmax;

      /* If the new min/max values have converged to a single value,
	 then there is only one value which can satisfy the condition,
	 return that value.  */
      if (operand_equal_p (min, max, 0) && is_gimple_min_invariant (min))
	return min;
    }
  return NULL;
}

/* Return whether the value range *VR fits in an integer type specified
   by PRECISION and UNSIGNED_P.  */

bool
range_fits_type_p (const value_range *vr,
		   unsigned dest_precision, signop dest_sgn)
{
  tree src_type;
  unsigned src_precision;
  widest_int tem;
  signop src_sgn;

  /* We can only handle integral and pointer types.  */
  src_type = vr->type ();
  if (!INTEGRAL_TYPE_P (src_type)
      && !POINTER_TYPE_P (src_type))
    return false;

  /* An extension is fine unless VR is SIGNED and dest_sgn is UNSIGNED,
     and so is an identity transform.  */
  src_precision = TYPE_PRECISION (vr->type ());
  src_sgn = TYPE_SIGN (src_type);
  if ((src_precision < dest_precision
       && !(dest_sgn == UNSIGNED && src_sgn == SIGNED))
      || (src_precision == dest_precision && src_sgn == dest_sgn))
    return true;

  /* Now we can only handle ranges with constant bounds.  */
  if (!range_int_cst_p (vr))
    return false;

  /* For sign changes, the MSB of the wide_int has to be clear.
     An unsigned value with its MSB set cannot be represented by
     a signed wide_int, while a negative value cannot be represented
     by an unsigned wide_int.  */
  if (src_sgn != dest_sgn
      && (wi::lts_p (wi::to_wide (vr->min ()), 0)
	  || wi::lts_p (wi::to_wide (vr->max ()), 0)))
    return false;

  /* Then we can perform the conversion on both ends and compare
     the result for equality.  */
  tem = wi::ext (wi::to_widest (vr->min ()), dest_precision, dest_sgn);
  if (tem != wi::to_widest (vr->min ()))
    return false;
  tem = wi::ext (wi::to_widest (vr->max ()), dest_precision, dest_sgn);
  if (tem != wi::to_widest (vr->max ()))
    return false;

  return true;
}

// Clear edge E of EDGE_EXECUTABLE (it is unexecutable). If it wasn't
// previously clear, propagate to successor blocks if appropriate.

void
simplify_using_ranges::set_and_propagate_unexecutable (edge e)
{
  // If not_executable is already set, we're done.
  // This works in the absence of a flag as well.
  if ((e->flags & m_not_executable_flag) == m_not_executable_flag)
    return;

  e->flags |= m_not_executable_flag;
  m_flag_set_edges.safe_push (e);

  // Check if the destination block needs to propagate the property.
  basic_block bb = e->dest;

  // If any incoming edge is executable, we are done.
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if ((e->flags & m_not_executable_flag) == 0)
      return;

  // This block is also unexecutable, propagate to all exit edges as well.
  FOR_EACH_EDGE (e, ei, bb->succs)
    set_and_propagate_unexecutable (e);
}

/* If COND can be folded entirely as TRUE or FALSE, rewrite the
   conditional as such, and return TRUE.  */

bool
simplify_using_ranges::fold_cond (gcond *cond)
{
  int_range_max r;
  if (query->range_of_stmt (r, cond) && r.singleton_p ())
    {
      // COND has already been folded if arguments are constant.
      if (TREE_CODE (gimple_cond_lhs (cond)) != SSA_NAME
	  && TREE_CODE (gimple_cond_rhs (cond)) != SSA_NAME)
	return false;
      edge e0 = EDGE_SUCC (gimple_bb (cond), 0);
      edge e1 = EDGE_SUCC (gimple_bb (cond), 1);
      if (r.zero_p ())
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nPredicate evaluates to: 0\n");
	  gimple_cond_make_false (cond);
	  if (e0->flags & EDGE_TRUE_VALUE)
	    set_and_propagate_unexecutable (e0);
	  else
	    set_and_propagate_unexecutable (e1);
	}
      else
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nPredicate evaluates to: 1\n");
	  gimple_cond_make_true (cond);
	  if (e0->flags & EDGE_FALSE_VALUE)
	    set_and_propagate_unexecutable (e0);
	  else
	    set_and_propagate_unexecutable (e1);
	}
      update_stmt (cond);
      return true;
    }

  /* ?? vrp_folder::fold_predicate_in() is a superset of this.  At
     some point we should merge all variants of this code.  */
  edge taken_edge;
  vrp_visit_cond_stmt (cond, &taken_edge);

  if (taken_edge)
    {
      if (taken_edge->flags & EDGE_TRUE_VALUE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nVRP Predicate evaluates to: 1\n");
	  gimple_cond_make_true (cond);
	}
      else if (taken_edge->flags & EDGE_FALSE_VALUE)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "\nVRP Predicate evaluates to: 0\n");
	  gimple_cond_make_false (cond);
	}
      else
       gcc_unreachable ();
      update_stmt (cond);
      return true;
    }
  return false;
}

/* Simplify a conditional using a relational operator to an equality
   test if the range information indicates only one value can satisfy
   the original conditional.  */

bool
simplify_using_ranges::simplify_cond_using_ranges_1 (gcond *stmt)
{
  tree op0 = gimple_cond_lhs (stmt);
  tree op1 = gimple_cond_rhs (stmt);
  enum tree_code cond_code = gimple_cond_code (stmt);

  if (fold_cond (stmt))
    return true;

  if (cond_code != NE_EXPR
      && cond_code != EQ_EXPR
      && TREE_CODE (op0) == SSA_NAME
      && INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && is_gimple_min_invariant (op1))
    {
      const value_range *vr = query->get_value_range (op0, stmt);

      /* If we have range information for OP0, then we might be
	 able to simplify this conditional. */
      if (!vr->undefined_p () && !vr->varying_p ())
	{
	  tree new_tree = test_for_singularity (cond_code, op0, op1, vr);
	  if (new_tree)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Simplified relational ");
		  print_gimple_stmt (dump_file, stmt, 0);
		  fprintf (dump_file, " into ");
		}

	      gimple_cond_set_code (stmt, EQ_EXPR);
	      gimple_cond_set_lhs (stmt, op0);
	      gimple_cond_set_rhs (stmt, new_tree);

	      update_stmt (stmt);

	      if (dump_file)
		{
		  print_gimple_stmt (dump_file, stmt, 0);
		  fprintf (dump_file, "\n");
		}

	      return true;
	    }

	  /* Try again after inverting the condition.  We only deal
	     with integral types here, so no need to worry about
	     issues with inverting FP comparisons.  */
	  new_tree = test_for_singularity
		       (invert_tree_comparison (cond_code, false),
			op0, op1, vr);
	  if (new_tree)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Simplified relational ");
		  print_gimple_stmt (dump_file, stmt, 0);
		  fprintf (dump_file, " into ");
		}

	      gimple_cond_set_code (stmt, NE_EXPR);
	      gimple_cond_set_lhs (stmt, op0);
	      gimple_cond_set_rhs (stmt, new_tree);

	      update_stmt (stmt);

	      if (dump_file)
		{
		  print_gimple_stmt (dump_file, stmt, 0);
		  fprintf (dump_file, "\n");
		}

	      return true;
	    }
	}
    }
  return false;
}

/* Simplify a switch statement using the value range of the switch
   argument.  */

bool
simplify_using_ranges::simplify_switch_using_ranges (gswitch *stmt)
{
  tree op = gimple_switch_index (stmt);
  const value_range *vr = NULL;
  bool take_default;
  edge e;
  edge_iterator ei;
  size_t i = 0, j = 0, n, n2;
  tree vec2;
  switch_update su;
  size_t k = 1, l = 0;

  if (TREE_CODE (op) == SSA_NAME)
    {
      vr = query->get_value_range (op, stmt);

      /* We can only handle integer ranges.  */
      if (vr->varying_p ()
	  || vr->undefined_p ()
	  || vr->symbolic_p ())
	return false;

      /* Find case label for min/max of the value range.  */
      take_default = !find_case_label_ranges (stmt, vr, &i, &j, &k, &l);
    }
  else if (TREE_CODE (op) == INTEGER_CST)
    {
      take_default = !find_case_label_index (stmt, 1, op, &i);
      if (take_default)
	{
	  i = 1;
	  j = 0;
	}
      else
	{
	  j = i;
	}
    }
  else
    return false;

  n = gimple_switch_num_labels (stmt);

  /* We can truncate the case label ranges that partially overlap with OP's
     value range.  */
  size_t min_idx = 1, max_idx = 0;
  if (vr != NULL)
    find_case_label_range (stmt, vr->min (), vr->max (), &min_idx, &max_idx);
  if (min_idx <= max_idx)
    {
      tree min_label = gimple_switch_label (stmt, min_idx);
      tree max_label = gimple_switch_label (stmt, max_idx);

      /* Avoid changing the type of the case labels when truncating.  */
      tree case_label_type = TREE_TYPE (CASE_LOW (min_label));
      tree vr_min = fold_convert (case_label_type, vr->min ());
      tree vr_max = fold_convert (case_label_type, vr->max ());

      if (vr->kind () == VR_RANGE)
	{
	  /* If OP's value range is [2,8] and the low label range is
	     0 ... 3, truncate the label's range to 2 .. 3.  */
	  if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) < 0
	      && CASE_HIGH (min_label) != NULL_TREE
	      && tree_int_cst_compare (CASE_HIGH (min_label), vr_min) >= 0)
	    CASE_LOW (min_label) = vr_min;

	  /* If OP's value range is [2,8] and the high label range is
	     7 ... 10, truncate the label's range to 7 .. 8.  */
	  if (tree_int_cst_compare (CASE_LOW (max_label), vr_max) <= 0
	      && CASE_HIGH (max_label) != NULL_TREE
	      && tree_int_cst_compare (CASE_HIGH (max_label), vr_max) > 0)
	    CASE_HIGH (max_label) = vr_max;
	}
      else if (vr->kind () == VR_ANTI_RANGE)
	{
	  tree one_cst = build_one_cst (case_label_type);

	  if (min_label == max_label)
	    {
	      /* If OP's value range is ~[7,8] and the label's range is
		 7 ... 10, truncate the label's range to 9 ... 10.  */
	      if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) == 0
		  && CASE_HIGH (min_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (min_label), vr_max) > 0)
		CASE_LOW (min_label)
		  = int_const_binop (PLUS_EXPR, vr_max, one_cst);

	      /* If OP's value range is ~[7,8] and the label's range is
		 5 ... 8, truncate the label's range to 5 ... 6.  */
	      if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) < 0
		  && CASE_HIGH (min_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (min_label), vr_max) == 0)
		CASE_HIGH (min_label)
		  = int_const_binop (MINUS_EXPR, vr_min, one_cst);
	    }
	  else
	    {
	      /* If OP's value range is ~[2,8] and the low label range is
		 0 ... 3, truncate the label's range to 0 ... 1.  */
	      if (tree_int_cst_compare (CASE_LOW (min_label), vr_min) < 0
		  && CASE_HIGH (min_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (min_label), vr_min) >= 0)
		CASE_HIGH (min_label)
		  = int_const_binop (MINUS_EXPR, vr_min, one_cst);

	      /* If OP's value range is ~[2,8] and the high label range is
		 7 ... 10, truncate the label's range to 9 ... 10.  */
	      if (tree_int_cst_compare (CASE_LOW (max_label), vr_max) <= 0
		  && CASE_HIGH (max_label) != NULL_TREE
		  && tree_int_cst_compare (CASE_HIGH (max_label), vr_max) > 0)
		CASE_LOW (max_label)
		  = int_const_binop (PLUS_EXPR, vr_max, one_cst);
	    }
	}

      /* Canonicalize singleton case ranges.  */
      if (tree_int_cst_equal (CASE_LOW (min_label), CASE_HIGH (min_label)))
	CASE_HIGH (min_label) = NULL_TREE;
      if (tree_int_cst_equal (CASE_LOW (max_label), CASE_HIGH (max_label)))
	CASE_HIGH (max_label) = NULL_TREE;
    }

  /* We can also eliminate case labels that lie completely outside OP's value
     range.  */

  /* Bail out if this is just all edges taken.  */
  if (i == 1
      && j == n - 1
      && take_default)
    return false;

  /* Build a new vector of taken case labels.  */
  vec2 = make_tree_vec (j - i + 1 + l - k + 1 + (int)take_default);
  n2 = 0;

  /* Add the default edge, if necessary.  */
  if (take_default)
    TREE_VEC_ELT (vec2, n2++) = gimple_switch_default_label (stmt);

  for (; i <= j; ++i, ++n2)
    TREE_VEC_ELT (vec2, n2) = gimple_switch_label (stmt, i);

  for (; k <= l; ++k, ++n2)
    TREE_VEC_ELT (vec2, n2) = gimple_switch_label (stmt, k);

  /* Mark needed edges.  */
  for (i = 0; i < n2; ++i)
    {
      e = find_edge (gimple_bb (stmt),
		     label_to_block (cfun,
				     CASE_LABEL (TREE_VEC_ELT (vec2, i))));
      e->aux = (void *)-1;
    }

  /* Queue not needed edges for later removal.  */
  FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
    {
      if (e->aux == (void *)-1)
	{
	  e->aux = NULL;
	  continue;
	}

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "removing unreachable case label\n");
	}
      to_remove_edges.safe_push (e);
      set_and_propagate_unexecutable (e);
      e->flags &= ~EDGE_EXECUTABLE;
      e->flags |= EDGE_IGNORE;
    }

  /* And queue an update for the stmt.  */
  su.stmt = stmt;
  su.vec = vec2;
  to_update_switch_stmts.safe_push (su);
  return true;
}

void
simplify_using_ranges::cleanup_edges_and_switches (void)
{
  int i;
  edge e;
  switch_update *su;

  /* Clear any edges marked as not executable.  */
  if (m_not_executable_flag)
    {
      FOR_EACH_VEC_ELT (m_flag_set_edges, i, e)
	e->flags &= ~m_not_executable_flag;
    }
  /* Remove dead edges from SWITCH_EXPR optimization.  This leaves the
     CFG in a broken state and requires a cfg_cleanup run.  */
  FOR_EACH_VEC_ELT (to_remove_edges, i, e)
    remove_edge (e);

  /* Update SWITCH_EXPR case label vector.  */
  FOR_EACH_VEC_ELT (to_update_switch_stmts, i, su)
    {
      size_t j;
      size_t n = TREE_VEC_LENGTH (su->vec);
      tree label;
      gimple_switch_set_num_labels (su->stmt, n);
      for (j = 0; j < n; j++)
	gimple_switch_set_label (su->stmt, j, TREE_VEC_ELT (su->vec, j));
      /* As we may have replaced the default label with a regular one
	 make sure to make it a real default label again.  This ensures
	 optimal expansion.  */
      label = gimple_switch_label (su->stmt, 0);
      CASE_LOW (label) = NULL_TREE;
      CASE_HIGH (label) = NULL_TREE;
    }

  if (!to_remove_edges.is_empty ())
    {
      free_dominance_info (CDI_DOMINATORS);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  to_remove_edges.release ();
  to_update_switch_stmts.release ();
}

/* Simplify an integral conversion from an SSA name in STMT.  */

static bool
simplify_conversion_using_ranges (gimple_stmt_iterator *gsi, gimple *stmt)
{
  tree innerop, middleop, finaltype;
  gimple *def_stmt;
  signop inner_sgn, middle_sgn, final_sgn;
  unsigned inner_prec, middle_prec, final_prec;
  widest_int innermin, innermed, innermax, middlemin, middlemed, middlemax;

  finaltype = TREE_TYPE (gimple_assign_lhs (stmt));
  if (!INTEGRAL_TYPE_P (finaltype))
    return false;
  middleop = gimple_assign_rhs1 (stmt);
  def_stmt = SSA_NAME_DEF_STMT (middleop);
  if (!is_gimple_assign (def_stmt)
      || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt)))
    return false;
  innerop = gimple_assign_rhs1 (def_stmt);
  if (TREE_CODE (innerop) != SSA_NAME
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (innerop))
    return false;

  /* Get the value-range of the inner operand.  Use global ranges in
     case innerop was created during substitute-and-fold.  */
  wide_int imin, imax;
  value_range vr;
  if (!INTEGRAL_TYPE_P (TREE_TYPE (innerop)))
    return false;
  get_range_query (cfun)->range_of_expr (vr, innerop, stmt);
  if (vr.undefined_p () || vr.varying_p ())
    return false;
  innermin = widest_int::from (vr.lower_bound (), TYPE_SIGN (TREE_TYPE (innerop)));
  innermax = widest_int::from (vr.upper_bound (), TYPE_SIGN (TREE_TYPE (innerop)));

  /* Simulate the conversion chain to check if the result is equal if
     the middle conversion is removed.  */
  inner_prec = TYPE_PRECISION (TREE_TYPE (innerop));
  middle_prec = TYPE_PRECISION (TREE_TYPE (middleop));
  final_prec = TYPE_PRECISION (finaltype);

  /* If the first conversion is not injective, the second must not
     be widening.  */
  if (wi::gtu_p (innermax - innermin,
		 wi::mask <widest_int> (middle_prec, false))
      && middle_prec < final_prec)
    return false;
  /* We also want a medium value so that we can track the effect that
     narrowing conversions with sign change have.  */
  inner_sgn = TYPE_SIGN (TREE_TYPE (innerop));
  if (inner_sgn == UNSIGNED)
    innermed = wi::shifted_mask <widest_int> (1, inner_prec - 1, false);
  else
    innermed = 0;
  if (wi::cmp (innermin, innermed, inner_sgn) >= 0
      || wi::cmp (innermed, innermax, inner_sgn) >= 0)
    innermed = innermin;

  middle_sgn = TYPE_SIGN (TREE_TYPE (middleop));
  middlemin = wi::ext (innermin, middle_prec, middle_sgn);
  middlemed = wi::ext (innermed, middle_prec, middle_sgn);
  middlemax = wi::ext (innermax, middle_prec, middle_sgn);

  /* Require that the final conversion applied to both the original
     and the intermediate range produces the same result.  */
  final_sgn = TYPE_SIGN (finaltype);
  if (wi::ext (middlemin, final_prec, final_sgn)
	 != wi::ext (innermin, final_prec, final_sgn)
      || wi::ext (middlemed, final_prec, final_sgn)
	 != wi::ext (innermed, final_prec, final_sgn)
      || wi::ext (middlemax, final_prec, final_sgn)
	 != wi::ext (innermax, final_prec, final_sgn))
    return false;

  gimple_assign_set_rhs1 (stmt, innerop);
  fold_stmt (gsi, follow_single_use_edges);
  return true;
}

/* Simplify a conversion from integral SSA name to float in STMT.  */

bool
simplify_using_ranges::simplify_float_conversion_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  tree rhs1 = gimple_assign_rhs1 (stmt);
  const value_range *vr = query->get_value_range (rhs1, stmt);
  scalar_float_mode fltmode
    = SCALAR_FLOAT_TYPE_MODE (TREE_TYPE (gimple_assign_lhs (stmt)));
  scalar_int_mode mode;
  tree tem;
  gassign *conv;

  /* We can only handle constant ranges.  */
  if (!range_int_cst_p (vr))
    return false;

  /* First check if we can use a signed type in place of an unsigned.  */
  scalar_int_mode rhs_mode = SCALAR_INT_TYPE_MODE (TREE_TYPE (rhs1));
  if (TYPE_UNSIGNED (TREE_TYPE (rhs1))
      && can_float_p (fltmode, rhs_mode, 0) != CODE_FOR_nothing
      && range_fits_type_p (vr, TYPE_PRECISION (TREE_TYPE (rhs1)), SIGNED))
    mode = rhs_mode;
  /* If we can do the conversion in the current input mode do nothing.  */
  else if (can_float_p (fltmode, rhs_mode,
			TYPE_UNSIGNED (TREE_TYPE (rhs1))) != CODE_FOR_nothing)
    return false;
  /* Otherwise search for a mode we can use, starting from the narrowest
     integer mode available.  */
  else
    {
      mode = NARROWEST_INT_MODE;
      for (;;)
	{
	  /* If we cannot do a signed conversion to float from mode
	     or if the value-range does not fit in the signed type
	     try with a wider mode.  */
	  if (can_float_p (fltmode, mode, 0) != CODE_FOR_nothing
	      && range_fits_type_p (vr, GET_MODE_PRECISION (mode), SIGNED))
	    break;

	  /* But do not widen the input.  Instead leave that to the
	     optabs expansion code.  */
	  if (!GET_MODE_WIDER_MODE (mode).exists (&mode)
	      || GET_MODE_PRECISION (mode) > TYPE_PRECISION (TREE_TYPE (rhs1)))
	    return false;
	}
    }

  /* It works, insert a truncation or sign-change before the
     float conversion.  */
  tem = make_ssa_name (build_nonstandard_integer_type
			  (GET_MODE_PRECISION (mode), 0));
  conv = gimple_build_assign (tem, NOP_EXPR, rhs1);
  gsi_insert_before (gsi, conv, GSI_SAME_STMT);
  gimple_assign_set_rhs1 (stmt, tem);
  fold_stmt (gsi, follow_single_use_edges);

  return true;
}

/* Simplify an internal fn call using ranges if possible.  */

bool
simplify_using_ranges::simplify_internal_call_using_ranges
					(gimple_stmt_iterator *gsi,
					 gimple *stmt)
{
  enum tree_code subcode;
  bool is_ubsan = false;
  bool ovf = false;
  switch (gimple_call_internal_fn (stmt))
    {
    case IFN_UBSAN_CHECK_ADD:
      subcode = PLUS_EXPR;
      is_ubsan = true;
      break;
    case IFN_UBSAN_CHECK_SUB:
      subcode = MINUS_EXPR;
      is_ubsan = true;
      break;
    case IFN_UBSAN_CHECK_MUL:
      subcode = MULT_EXPR;
      is_ubsan = true;
      break;
    case IFN_ADD_OVERFLOW:
      subcode = PLUS_EXPR;
      break;
    case IFN_SUB_OVERFLOW:
      subcode = MINUS_EXPR;
      break;
    case IFN_MUL_OVERFLOW:
      subcode = MULT_EXPR;
      break;
    default:
      return false;
    }

  tree op0 = gimple_call_arg (stmt, 0);
  tree op1 = gimple_call_arg (stmt, 1);
  tree type;
  if (is_ubsan)
    {
      type = TREE_TYPE (op0);
      if (VECTOR_TYPE_P (type))
	return false;
    }
  else if (gimple_call_lhs (stmt) == NULL_TREE)
    return false;
  else
    type = TREE_TYPE (TREE_TYPE (gimple_call_lhs (stmt)));
  if (!check_for_binary_op_overflow (query, subcode, type, op0, op1, &ovf, stmt)
      || (is_ubsan && ovf))
    return false;

  gimple *g;
  location_t loc = gimple_location (stmt);
  if (is_ubsan)
    g = gimple_build_assign (gimple_call_lhs (stmt), subcode, op0, op1);
  else
    {
      int prec = TYPE_PRECISION (type);
      tree utype = type;
      if (ovf
	  || !useless_type_conversion_p (type, TREE_TYPE (op0))
	  || !useless_type_conversion_p (type, TREE_TYPE (op1)))
	utype = build_nonstandard_integer_type (prec, 1);
      if (TREE_CODE (op0) == INTEGER_CST)
	op0 = fold_convert (utype, op0);
      else if (!useless_type_conversion_p (utype, TREE_TYPE (op0)))
	{
	  g = gimple_build_assign (make_ssa_name (utype), NOP_EXPR, op0);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  op0 = gimple_assign_lhs (g);
	}
      if (TREE_CODE (op1) == INTEGER_CST)
	op1 = fold_convert (utype, op1);
      else if (!useless_type_conversion_p (utype, TREE_TYPE (op1)))
	{
	  g = gimple_build_assign (make_ssa_name (utype), NOP_EXPR, op1);
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	  op1 = gimple_assign_lhs (g);
	}
      g = gimple_build_assign (make_ssa_name (utype), subcode, op0, op1);
      gimple_set_location (g, loc);
      gsi_insert_before (gsi, g, GSI_SAME_STMT);
      if (utype != type)
	{
	  g = gimple_build_assign (make_ssa_name (type), NOP_EXPR,
				   gimple_assign_lhs (g));
	  gimple_set_location (g, loc);
	  gsi_insert_before (gsi, g, GSI_SAME_STMT);
	}
      g = gimple_build_assign (gimple_call_lhs (stmt), COMPLEX_EXPR,
			       gimple_assign_lhs (g),
			       build_int_cst (type, ovf));
    }
  gimple_set_location (g, loc);
  gsi_replace (gsi, g, false);
  return true;
}

/* Return true if VAR is a two-valued variable.  Set a and b with the
   two-values when it is true.  Return false otherwise.  */

bool
simplify_using_ranges::two_valued_val_range_p (tree var, tree *a, tree *b,
					       gimple *s)
{
  value_range vr = *query->get_value_range (var, s);
  vr.normalize_symbolics ();
  if (vr.varying_p () || vr.undefined_p ())
    return false;

  if ((vr.num_pairs () == 1 && vr.upper_bound () - vr.lower_bound () == 1)
      || (vr.num_pairs () == 2
	  && vr.lower_bound (0) == vr.upper_bound (0)
	  && vr.lower_bound (1) == vr.upper_bound (1)))
    {
      *a = wide_int_to_tree (TREE_TYPE (var), vr.lower_bound ());
      *b = wide_int_to_tree (TREE_TYPE (var), vr.upper_bound ());
      return true;
    }
  return false;
}

simplify_using_ranges::simplify_using_ranges (range_query *query,
					      int not_executable_flag)
  : query (query)
{
  to_remove_edges = vNULL;
  to_update_switch_stmts = vNULL;
  m_not_executable_flag = not_executable_flag;
  m_flag_set_edges = vNULL;
}

simplify_using_ranges::~simplify_using_ranges ()
{
  cleanup_edges_and_switches ();
}

/* Simplify STMT using ranges if possible.  */

bool
simplify_using_ranges::simplify (gimple_stmt_iterator *gsi)
{
  gcc_checking_assert (query);

  gimple *stmt = gsi_stmt (*gsi);
  if (is_gimple_assign (stmt))
    {
      enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
      tree rhs1 = gimple_assign_rhs1 (stmt);
      tree rhs2 = gimple_assign_rhs2 (stmt);
      tree lhs = gimple_assign_lhs (stmt);
      tree val1 = NULL_TREE, val2 = NULL_TREE;
      use_operand_p use_p;
      gimple *use_stmt;

      /* Convert:
	 LHS = CST BINOP VAR
	 Where VAR is two-valued and LHS is used in GIMPLE_COND only
	 To:
	 LHS = VAR == VAL1 ? (CST BINOP VAL1) : (CST BINOP VAL2)

	 Also handles:
	 LHS = VAR BINOP CST
	 Where VAR is two-valued and LHS is used in GIMPLE_COND only
	 To:
	 LHS = VAR == VAL1 ? (VAL1 BINOP CST) : (VAL2 BINOP CST) */

      if (TREE_CODE_CLASS (rhs_code) == tcc_binary
	  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	  && ((TREE_CODE (rhs1) == INTEGER_CST
	       && TREE_CODE (rhs2) == SSA_NAME)
	      || (TREE_CODE (rhs2) == INTEGER_CST
		  && TREE_CODE (rhs1) == SSA_NAME))
	  && single_imm_use (lhs, &use_p, &use_stmt)
	  && gimple_code (use_stmt) == GIMPLE_COND)

	{
	  tree new_rhs1 = NULL_TREE;
	  tree new_rhs2 = NULL_TREE;
	  tree cmp_var = NULL_TREE;

	  if (TREE_CODE (rhs2) == SSA_NAME
	      && two_valued_val_range_p (rhs2, &val1, &val2, stmt))
	    {
	      /* Optimize RHS1 OP [VAL1, VAL2].  */
	      new_rhs1 = int_const_binop (rhs_code, rhs1, val1);
	      new_rhs2 = int_const_binop (rhs_code, rhs1, val2);
	      cmp_var = rhs2;
	    }
	  else if (TREE_CODE (rhs1) == SSA_NAME
		   && two_valued_val_range_p (rhs1, &val1, &val2, stmt))
	    {
	      /* Optimize [VAL1, VAL2] OP RHS2.  */
	      new_rhs1 = int_const_binop (rhs_code, val1, rhs2);
	      new_rhs2 = int_const_binop (rhs_code, val2, rhs2);
	      cmp_var = rhs1;
	    }

	  /* If we could not find two-vals or the optimzation is invalid as
	     in divide by zero, new_rhs1 / new_rhs will be NULL_TREE.  */
	  if (new_rhs1 && new_rhs2)
	    {
	      tree cond = build2 (EQ_EXPR, boolean_type_node, cmp_var, val1);
	      gimple_assign_set_rhs_with_ops (gsi,
					      COND_EXPR, cond,
					      new_rhs1,
					      new_rhs2);
	      update_stmt (gsi_stmt (*gsi));
	      fold_stmt (gsi, follow_single_use_edges);
	      return true;
	    }
	}

      switch (rhs_code)
	{
	case EQ_EXPR:
	case NE_EXPR:
          /* Transform EQ_EXPR, NE_EXPR into BIT_XOR_EXPR or identity
	     if the RHS is zero or one, and the LHS are known to be boolean
	     values.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_truth_ops_using_ranges (gsi, stmt);
	  break;

      /* Transform TRUNC_DIV_EXPR and TRUNC_MOD_EXPR into RSHIFT_EXPR
	 and BIT_AND_EXPR respectively if the first operand is greater
	 than zero and the second operand is an exact power of two.
	 Also optimize TRUNC_MOD_EXPR away if the second operand is
	 constant and the first operand already has the right value
	 range.  */
	case TRUNC_DIV_EXPR:
	case TRUNC_MOD_EXPR:
	  if ((TREE_CODE (rhs1) == SSA_NAME
	       || TREE_CODE (rhs1) == INTEGER_CST)
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_div_or_mod_using_ranges (gsi, stmt);
	  break;

      /* Transform ABS (X) into X or -X as appropriate.  */
	case ABS_EXPR:
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_abs_using_ranges (gsi, stmt);
	  break;

	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	  /* Optimize away BIT_AND_EXPR and BIT_IOR_EXPR
	     if all the bits being cleared are already cleared or
	     all the bits being set are already set.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_bit_ops_using_ranges (gsi, stmt);
	  break;

	CASE_CONVERT:
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_conversion_using_ranges (gsi, stmt);
	  break;

	case FLOAT_EXPR:
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
	    return simplify_float_conversion_using_ranges (gsi, stmt);
	  break;

	case MIN_EXPR:
	case MAX_EXPR:
	  return simplify_min_or_max_using_ranges (gsi, stmt);

	case RSHIFT_EXPR:
	  {
	    tree op0 = gimple_assign_rhs1 (stmt);
	    tree type = TREE_TYPE (op0);
	    int_range_max range;
	    if (TYPE_SIGN (type) == SIGNED
		&& query->range_of_expr (range, op0, stmt))
	      {
		unsigned prec = TYPE_PRECISION (TREE_TYPE (op0));
		int_range<2> nzm1 (type, wi::minus_one (prec), wi::zero (prec),
				   VR_ANTI_RANGE);
		range.intersect (nzm1);
		// If there are no ranges other than [-1, 0] remove the shift.
		if (range.undefined_p ())
		  {
		    gimple_assign_set_rhs_from_tree (gsi, op0);
		    return true;
		  }
		return false;
	      }
	    break;
	  }
	default:
	  break;
	}
    }
  else if (gimple_code (stmt) == GIMPLE_COND)
    return simplify_cond_using_ranges_1 (as_a <gcond *> (stmt));
  else if (gimple_code (stmt) == GIMPLE_SWITCH)
    return simplify_switch_using_ranges (as_a <gswitch *> (stmt));
  else if (is_gimple_call (stmt)
	   && gimple_call_internal_p (stmt))
    return simplify_internal_call_using_ranges (gsi, stmt);

  return false;
}

/* Set the lattice entry for VAR to VR.  */

void
vr_values::set_vr_value (tree var, value_range_equiv *vr)
{
  if (SSA_NAME_VERSION (var) >= num_vr_values)
    return;
  vr_value[SSA_NAME_VERSION (var)] = vr;
}

/* Swap the lattice entry for VAR with VR and return the old entry.  */

value_range_equiv *
vr_values::swap_vr_value (tree var, value_range_equiv *vr)
{
  if (SSA_NAME_VERSION (var) >= num_vr_values)
    return NULL;
  std::swap (vr_value[SSA_NAME_VERSION (var)], vr);
  return vr;
}
