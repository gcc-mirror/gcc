/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005-2020 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>.

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
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-dfa.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-ssa-loop.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "intl.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "tree-chrec.h"
#include "tree-ssa-threadupdate.h"
#include "tree-ssa-scopedtables.h"
#include "tree-ssa-threadedge.h"
#include "omp-general.h"
#include "target.h"
#include "case-cfn-macros.h"
#include "alloc-pool.h"
#include "domwalk.h"
#include "tree-cfgcleanup.h"
#include "stringpool.h"
#include "attribs.h"
#include "vr-values.h"
#include "builtins.h"
#include "range-op.h"

/* Set of SSA names found live during the RPO traversal of the function
   for still active basic-blocks.  */
static sbitmap *live;

void
value_range_equiv::set_equiv (bitmap equiv)
{
  if (undefined_p () || varying_p ())
    equiv = NULL;
  /* Since updating the equivalence set involves deep copying the
     bitmaps, only do it if absolutely necessary.

     All equivalence bitmaps are allocated from the same obstack.  So
     we can use the obstack associated with EQUIV to allocate vr->equiv.  */
  if (m_equiv == NULL
      && equiv != NULL)
    m_equiv = BITMAP_ALLOC (equiv->obstack);

  if (equiv != m_equiv)
    {
      if (equiv && !bitmap_empty_p (equiv))
	bitmap_copy (m_equiv, equiv);
      else
	bitmap_clear (m_equiv);
    }
}

/* Initialize value_range.  */

void
value_range_equiv::set (tree min, tree max, bitmap equiv,
			value_range_kind kind)
{
  value_range::set (min, max, kind);
  set_equiv (equiv);
  if (flag_checking)
    check ();
}

value_range_equiv::value_range_equiv (tree min, tree max, bitmap equiv,
				      value_range_kind kind)
{
  m_equiv = NULL;
  set (min, max, equiv, kind);
}

value_range_equiv::value_range_equiv (const value_range &other)
{
  m_equiv = NULL;
  set (other.min(), other.max (), NULL, other.kind ());
}

/* Like set, but keep the equivalences in place.  */

void
value_range_equiv::update (tree min, tree max, value_range_kind kind)
{
  set (min, max,
       (kind != VR_UNDEFINED && kind != VR_VARYING) ? m_equiv : NULL, kind);
}

/* Copy value_range in FROM into THIS while avoiding bitmap sharing.

   Note: The code that avoids the bitmap sharing looks at the existing
   this->m_equiv, so this function cannot be used to initalize an
   object.  Use the constructors for initialization.  */

void
value_range_equiv::deep_copy (const value_range_equiv *from)
{
  set (from->min (), from->max (), from->m_equiv, from->m_kind);
}

void
value_range_equiv::move (value_range_equiv *from)
{
  set (from->min (), from->max (), NULL, from->m_kind);
  m_equiv = from->m_equiv;
  from->m_equiv = NULL;
}

void
value_range_equiv::check ()
{
  value_range::check ();
  switch (m_kind)
    {
    case VR_UNDEFINED:
    case VR_VARYING:
      gcc_assert (!m_equiv || bitmap_empty_p (m_equiv));
    default:;
    }
}

/* Return true if the bitmaps B1 and B2 are equal.  */

static bool
vrp_bitmap_equal_p (const_bitmap b1, const_bitmap b2)
{
  return (b1 == b2
	  || ((!b1 || bitmap_empty_p (b1))
	      && (!b2 || bitmap_empty_p (b2)))
	  || (b1 && b2
	      && bitmap_equal_p (b1, b2)));
}

/* Returns TRUE if THIS == OTHER.  Ignores the equivalence bitmap if
   IGNORE_EQUIVS is TRUE.  */

bool
value_range_equiv::equal_p (const value_range_equiv &other,
			    bool ignore_equivs) const
{
  return (value_range::equal_p (other)
	  && (ignore_equivs
	      || vrp_bitmap_equal_p (m_equiv, other.m_equiv)));
}

void
value_range_equiv::set_undefined ()
{
  set (NULL, NULL, NULL, VR_UNDEFINED);
}

void
value_range_equiv::set_varying (tree type)
{
  value_range::set_varying (type);
  equiv_clear ();
}

void
value_range_equiv::equiv_clear ()
{
  if (m_equiv)
    bitmap_clear (m_equiv);
}

/* Add VAR and VAR's equivalence set (VAR_VR) to the equivalence
   bitmap.  If no equivalence table has been created, OBSTACK is the
   obstack to use (NULL for the default obstack).

   This is the central point where equivalence processing can be
   turned on/off.  */

void
value_range_equiv::equiv_add (const_tree var,
			      const value_range_equiv *var_vr,
			      bitmap_obstack *obstack)
{
  if (!m_equiv)
    m_equiv = BITMAP_ALLOC (obstack);
  unsigned ver = SSA_NAME_VERSION (var);
  bitmap_set_bit (m_equiv, ver);
  if (var_vr && var_vr->m_equiv)
    bitmap_ior_into (m_equiv, var_vr->m_equiv);
}

void
value_range_equiv::dump (FILE *file) const
{
  value_range::dump (file);
  if ((m_kind == VR_RANGE || m_kind == VR_ANTI_RANGE)
      && m_equiv)
    {
      bitmap_iterator bi;
      unsigned i, c = 0;

      fprintf (file, "  EQUIVALENCES: { ");

      EXECUTE_IF_SET_IN_BITMAP (m_equiv, 0, i, bi)
	{
	  print_generic_expr (file, ssa_name (i));
	  fprintf (file, " ");
	  c++;
	}

      fprintf (file, "} (%u elements)", c);
    }
}

void
value_range_equiv::dump () const
{
  dump (stderr);
}

void
dump_value_range (FILE *file, const value_range_equiv *vr)
{
  if (!vr)
    fprintf (file, "[]");
  else
    vr->dump (file);
}

DEBUG_FUNCTION void
debug (const value_range_equiv *vr)
{
  dump_value_range (stderr, vr);
}

DEBUG_FUNCTION void
debug (const value_range_equiv &vr)
{
  dump_value_range (stderr, &vr);
}

/* Return true if the SSA name NAME is live on the edge E.  */

static bool
live_on_edge (edge e, tree name)
{
  return (live[e->dest->index]
	  && bitmap_bit_p (live[e->dest->index], SSA_NAME_VERSION (name)));
}

/* Location information for ASSERT_EXPRs.  Each instance of this
   structure describes an ASSERT_EXPR for an SSA name.  Since a single
   SSA name may have more than one assertion associated with it, these
   locations are kept in a linked list attached to the corresponding
   SSA name.  */
struct assert_locus
{
  /* Basic block where the assertion would be inserted.  */
  basic_block bb;

  /* Some assertions need to be inserted on an edge (e.g., assertions
     generated by COND_EXPRs).  In those cases, BB will be NULL.  */
  edge e;

  /* Pointer to the statement that generated this assertion.  */
  gimple_stmt_iterator si;

  /* Predicate code for the ASSERT_EXPR.  Must be COMPARISON_CLASS_P.  */
  enum tree_code comp_code;

  /* Value being compared against.  */
  tree val;

  /* Expression to compare.  */
  tree expr;

  /* Next node in the linked list.  */
  assert_locus *next;
};

/* If bit I is present, it means that SSA name N_i has a list of
   assertions that should be inserted in the IL.  */
static bitmap need_assert_for;

/* Array of locations lists where to insert assertions.  ASSERTS_FOR[I]
   holds a list of ASSERT_LOCUS_T nodes that describe where
   ASSERT_EXPRs for SSA name N_I should be inserted.  */
static assert_locus **asserts_for;

/* VR_TYPE describes a range with mininum value *MIN and maximum
   value *MAX.  Restrict the range to the set of values that have
   no bits set outside NONZERO_BITS.  Update *MIN and *MAX and
   return the new range type.

   SGN gives the sign of the values described by the range.  */

enum value_range_kind
intersect_range_with_nonzero_bits (enum value_range_kind vr_type,
				   wide_int *min, wide_int *max,
				   const wide_int &nonzero_bits,
				   signop sgn)
{
  if (vr_type == VR_ANTI_RANGE)
    {
      /* The VR_ANTI_RANGE is equivalent to the union of the ranges
	 A: [-INF, *MIN) and B: (*MAX, +INF].  First use NONZERO_BITS
	 to create an inclusive upper bound for A and an inclusive lower
	 bound for B.  */
      wide_int a_max = wi::round_down_for_mask (*min - 1, nonzero_bits);
      wide_int b_min = wi::round_up_for_mask (*max + 1, nonzero_bits);

      /* If the calculation of A_MAX wrapped, A is effectively empty
	 and A_MAX is the highest value that satisfies NONZERO_BITS.
	 Likewise if the calculation of B_MIN wrapped, B is effectively
	 empty and B_MIN is the lowest value that satisfies NONZERO_BITS.  */
      bool a_empty = wi::ge_p (a_max, *min, sgn);
      bool b_empty = wi::le_p (b_min, *max, sgn);

      /* If both A and B are empty, there are no valid values.  */
      if (a_empty && b_empty)
	return VR_UNDEFINED;

      /* If exactly one of A or B is empty, return a VR_RANGE for the
	 other one.  */
      if (a_empty || b_empty)
	{
	  *min = b_min;
	  *max = a_max;
	  gcc_checking_assert (wi::le_p (*min, *max, sgn));
	  return VR_RANGE;
	}

      /* Update the VR_ANTI_RANGE bounds.  */
      *min = a_max + 1;
      *max = b_min - 1;
      gcc_checking_assert (wi::le_p (*min, *max, sgn));

      /* Now check whether the excluded range includes any values that
	 satisfy NONZERO_BITS.  If not, switch to a full VR_RANGE.  */
      if (wi::round_up_for_mask (*min, nonzero_bits) == b_min)
	{
	  unsigned int precision = min->get_precision ();
	  *min = wi::min_value (precision, sgn);
	  *max = wi::max_value (precision, sgn);
	  vr_type = VR_RANGE;
	}
    }
  if (vr_type == VR_RANGE)
    {
      *max = wi::round_down_for_mask (*max, nonzero_bits);

      /* Check that the range contains at least one valid value.  */
      if (wi::gt_p (*min, *max, sgn))
	return VR_UNDEFINED;

      *min = wi::round_up_for_mask (*min, nonzero_bits);
      gcc_checking_assert (wi::le_p (*min, *max, sgn));
    }
  return vr_type;
}

void
value_range_equiv::set (tree val)
{
  gcc_assert (TREE_CODE (val) == SSA_NAME || is_gimple_min_invariant (val));
  if (TREE_OVERFLOW_P (val))
    val = drop_tree_overflow (val);
  set (val, val);
}

/* Return true if max and min of VR are INTEGER_CST.  It's not necessary
   a singleton.  */

bool
range_int_cst_p (const value_range *vr)
{
  return (vr->kind () == VR_RANGE && range_has_numeric_bounds_p (vr));
}

/* Return the single symbol (an SSA_NAME) contained in T if any, or NULL_TREE
   otherwise.  We only handle additive operations and set NEG to true if the
   symbol is negated and INV to the invariant part, if any.  */

tree
get_single_symbol (tree t, bool *neg, tree *inv)
{
  bool neg_;
  tree inv_;

  *inv = NULL_TREE;
  *neg = false;

  if (TREE_CODE (t) == PLUS_EXPR
      || TREE_CODE (t) == POINTER_PLUS_EXPR
      || TREE_CODE (t) == MINUS_EXPR)
    {
      if (is_gimple_min_invariant (TREE_OPERAND (t, 0)))
	{
	  neg_ = (TREE_CODE (t) == MINUS_EXPR);
	  inv_ = TREE_OPERAND (t, 0);
	  t = TREE_OPERAND (t, 1);
	}
      else if (is_gimple_min_invariant (TREE_OPERAND (t, 1)))
	{
	  neg_ = false;
	  inv_ = TREE_OPERAND (t, 1);
	  t = TREE_OPERAND (t, 0);
	}
      else
        return NULL_TREE;
    }
  else
    {
      neg_ = false;
      inv_ = NULL_TREE;
    }

  if (TREE_CODE (t) == NEGATE_EXPR)
    {
      t = TREE_OPERAND (t, 0);
      neg_ = !neg_;
    }

  if (TREE_CODE (t) != SSA_NAME)
    return NULL_TREE;

  if (inv_ && TREE_OVERFLOW_P (inv_))
    inv_ = drop_tree_overflow (inv_);

  *neg = neg_;
  *inv = inv_;
  return t;
}

/* The reverse operation: build a symbolic expression with TYPE
   from symbol SYM, negated according to NEG, and invariant INV.  */

static tree
build_symbolic_expr (tree type, tree sym, bool neg, tree inv)
{
  const bool pointer_p = POINTER_TYPE_P (type);
  tree t = sym;

  if (neg)
    t = build1 (NEGATE_EXPR, type, t);

  if (integer_zerop (inv))
    return t;

  return build2 (pointer_p ? POINTER_PLUS_EXPR : PLUS_EXPR, type, t, inv);
}

/* Return
   1 if VAL < VAL2
   0 if !(VAL < VAL2)
   -2 if those are incomparable.  */
int
operand_less_p (tree val, tree val2)
{
  /* LT is folded faster than GE and others.  Inline the common case.  */
  if (TREE_CODE (val) == INTEGER_CST && TREE_CODE (val2) == INTEGER_CST)
    return tree_int_cst_lt (val, val2);
  else if (TREE_CODE (val) == SSA_NAME && TREE_CODE (val2) == SSA_NAME)
    return val == val2 ? 0 : -2;
  else
    {
      int cmp = compare_values (val, val2);
      if (cmp == -1)
	return 1;
      else if (cmp == 0 || cmp == 1)
	return 0;
      else
	return -2;
    }

  return 0;
}

/* Compare two values VAL1 and VAL2.  Return

   	-2 if VAL1 and VAL2 cannot be compared at compile-time,
   	-1 if VAL1 < VAL2,
   	 0 if VAL1 == VAL2,
	+1 if VAL1 > VAL2, and
	+2 if VAL1 != VAL2

   This is similar to tree_int_cst_compare but supports pointer values
   and values that cannot be compared at compile time.

   If STRICT_OVERFLOW_P is not NULL, then set *STRICT_OVERFLOW_P to
   true if the return value is only valid if we assume that signed
   overflow is undefined.  */

int
compare_values_warnv (tree val1, tree val2, bool *strict_overflow_p)
{
  if (val1 == val2)
    return 0;

  /* Below we rely on the fact that VAL1 and VAL2 are both pointers or
     both integers.  */
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (val1))
	      == POINTER_TYPE_P (TREE_TYPE (val2)));

  /* Convert the two values into the same type.  This is needed because
     sizetype causes sign extension even for unsigned types.  */
  if (!useless_type_conversion_p (TREE_TYPE (val1), TREE_TYPE (val2)))
    val2 = fold_convert (TREE_TYPE (val1), val2);

  const bool overflow_undefined
    = INTEGRAL_TYPE_P (TREE_TYPE (val1))
      && TYPE_OVERFLOW_UNDEFINED (TREE_TYPE (val1));
  tree inv1, inv2;
  bool neg1, neg2;
  tree sym1 = get_single_symbol (val1, &neg1, &inv1);
  tree sym2 = get_single_symbol (val2, &neg2, &inv2);

  /* If VAL1 and VAL2 are of the form '[-]NAME [+ CST]', return -1 or +1
     accordingly.  If VAL1 and VAL2 don't use the same name, return -2.  */
  if (sym1 && sym2)
    {
      /* Both values must use the same name with the same sign.  */
      if (sym1 != sym2 || neg1 != neg2)
	return -2;

      /* [-]NAME + CST == [-]NAME + CST.  */
      if (inv1 == inv2)
	return 0;

      /* If overflow is defined we cannot simplify more.  */
      if (!overflow_undefined)
	return -2;

      if (strict_overflow_p != NULL
	  /* Symbolic range building sets TREE_NO_WARNING to declare
	     that overflow doesn't happen.  */
	  && (!inv1 || !TREE_NO_WARNING (val1))
	  && (!inv2 || !TREE_NO_WARNING (val2)))
	*strict_overflow_p = true;

      if (!inv1)
	inv1 = build_int_cst (TREE_TYPE (val1), 0);
      if (!inv2)
	inv2 = build_int_cst (TREE_TYPE (val2), 0);

      return wi::cmp (wi::to_wide (inv1), wi::to_wide (inv2),
		      TYPE_SIGN (TREE_TYPE (val1)));
    }

  const bool cst1 = is_gimple_min_invariant (val1);
  const bool cst2 = is_gimple_min_invariant (val2);

  /* If one is of the form '[-]NAME + CST' and the other is constant, then
     it might be possible to say something depending on the constants.  */
  if ((sym1 && inv1 && cst2) || (sym2 && inv2 && cst1))
    {
      if (!overflow_undefined)
	return -2;

      if (strict_overflow_p != NULL
	  /* Symbolic range building sets TREE_NO_WARNING to declare
	     that overflow doesn't happen.  */
	  && (!sym1 || !TREE_NO_WARNING (val1))
	  && (!sym2 || !TREE_NO_WARNING (val2)))
	*strict_overflow_p = true;

      const signop sgn = TYPE_SIGN (TREE_TYPE (val1));
      tree cst = cst1 ? val1 : val2;
      tree inv = cst1 ? inv2 : inv1;

      /* Compute the difference between the constants.  If it overflows or
	 underflows, this means that we can trivially compare the NAME with
	 it and, consequently, the two values with each other.  */
      wide_int diff = wi::to_wide (cst) - wi::to_wide (inv);
      if (wi::cmp (0, wi::to_wide (inv), sgn)
	  != wi::cmp (diff, wi::to_wide (cst), sgn))
	{
	  const int res = wi::cmp (wi::to_wide (cst), wi::to_wide (inv), sgn);
	  return cst1 ? res : -res;
	}

      return -2;
    }

  /* We cannot say anything more for non-constants.  */
  if (!cst1 || !cst2)
    return -2;

  if (!POINTER_TYPE_P (TREE_TYPE (val1)))
    {
      /* We cannot compare overflowed values.  */
      if (TREE_OVERFLOW (val1) || TREE_OVERFLOW (val2))
	return -2;

      if (TREE_CODE (val1) == INTEGER_CST
	  && TREE_CODE (val2) == INTEGER_CST)
	return tree_int_cst_compare (val1, val2);

      if (poly_int_tree_p (val1) && poly_int_tree_p (val2))
	{
	  if (known_eq (wi::to_poly_widest (val1),
			wi::to_poly_widest (val2)))
	    return 0;
	  if (known_lt (wi::to_poly_widest (val1),
			wi::to_poly_widest (val2)))
	    return -1;
	  if (known_gt (wi::to_poly_widest (val1),
			wi::to_poly_widest (val2)))
	    return 1;
	}

      return -2;
    }
  else
    {
      if (TREE_CODE (val1) == INTEGER_CST && TREE_CODE (val2) == INTEGER_CST)
	{
	  /* We cannot compare overflowed values.  */
	  if (TREE_OVERFLOW (val1) || TREE_OVERFLOW (val2))
	    return -2;

	  return tree_int_cst_compare (val1, val2);
	}

      /* First see if VAL1 and VAL2 are not the same.  */
      if (operand_equal_p (val1, val2, 0))
	return 0;

      fold_defer_overflow_warnings ();

      /* If VAL1 is a lower address than VAL2, return -1.  */
      tree t = fold_binary_to_constant (LT_EXPR, boolean_type_node, val1, val2);
      if (t && integer_onep (t))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return -1;
	}

      /* If VAL1 is a higher address than VAL2, return +1.  */
      t = fold_binary_to_constant (LT_EXPR, boolean_type_node, val2, val1);
      if (t && integer_onep (t))
	{
	  fold_undefer_and_ignore_overflow_warnings ();
	  return 1;
	}

      /* If VAL1 is different than VAL2, return +2.  */
      t = fold_binary_to_constant (NE_EXPR, boolean_type_node, val1, val2);
      fold_undefer_and_ignore_overflow_warnings ();
      if (t && integer_onep (t))
	return 2;

      return -2;
    }
}

/* Compare values like compare_values_warnv.  */

int
compare_values (tree val1, tree val2)
{
  bool sop;
  return compare_values_warnv (val1, val2, &sop);
}

/* If BOUND will include a symbolic bound, adjust it accordingly,
   otherwise leave it as is.

   CODE is the original operation that combined the bounds (PLUS_EXPR
   or MINUS_EXPR).

   TYPE is the type of the original operation.

   SYM_OPn is the symbolic for OPn if it has a symbolic.

   NEG_OPn is TRUE if the OPn was negated.  */

static void
adjust_symbolic_bound (tree &bound, enum tree_code code, tree type,
		       tree sym_op0, tree sym_op1,
		       bool neg_op0, bool neg_op1)
{
  bool minus_p = (code == MINUS_EXPR);
  /* If the result bound is constant, we're done; otherwise, build the
     symbolic lower bound.  */
  if (sym_op0 == sym_op1)
    ;
  else if (sym_op0)
    bound = build_symbolic_expr (type, sym_op0,
				 neg_op0, bound);
  else if (sym_op1)
    {
      /* We may not negate if that might introduce
	 undefined overflow.  */
      if (!minus_p
	  || neg_op1
	  || TYPE_OVERFLOW_WRAPS (type))
	bound = build_symbolic_expr (type, sym_op1,
				     neg_op1 ^ minus_p, bound);
      else
	bound = NULL_TREE;
    }
}

/* Combine OP1 and OP1, which are two parts of a bound, into one wide
   int bound according to CODE.  CODE is the operation combining the
   bound (either a PLUS_EXPR or a MINUS_EXPR).

   TYPE is the type of the combine operation.

   WI is the wide int to store the result.

   OVF is -1 if an underflow occurred, +1 if an overflow occurred or 0
   if over/underflow occurred.  */

static void
combine_bound (enum tree_code code, wide_int &wi, wi::overflow_type &ovf,
	       tree type, tree op0, tree op1)
{
  bool minus_p = (code == MINUS_EXPR);
  const signop sgn = TYPE_SIGN (type);
  const unsigned int prec = TYPE_PRECISION (type);

  /* Combine the bounds, if any.  */
  if (op0 && op1)
    {
      if (minus_p)
	wi = wi::sub (wi::to_wide (op0), wi::to_wide (op1), sgn, &ovf);
      else
	wi = wi::add (wi::to_wide (op0), wi::to_wide (op1), sgn, &ovf);
    }
  else if (op0)
    wi = wi::to_wide (op0);
  else if (op1)
    {
      if (minus_p)
	wi = wi::neg (wi::to_wide (op1), &ovf);
      else
	wi = wi::to_wide (op1);
    }
  else
    wi = wi::shwi (0, prec);
}

/* Given a range in [WMIN, WMAX], adjust it for possible overflow and
   put the result in VR.

   TYPE is the type of the range.

   MIN_OVF and MAX_OVF indicate what type of overflow, if any,
   occurred while originally calculating WMIN or WMAX.  -1 indicates
   underflow.  +1 indicates overflow.  0 indicates neither.  */

static void
set_value_range_with_overflow (value_range_kind &kind, tree &min, tree &max,
			       tree type,
			       const wide_int &wmin, const wide_int &wmax,
			       wi::overflow_type min_ovf,
			       wi::overflow_type max_ovf)
{
  const signop sgn = TYPE_SIGN (type);
  const unsigned int prec = TYPE_PRECISION (type);

  /* For one bit precision if max < min, then the swapped
     range covers all values.  */
  if (prec == 1 && wi::lt_p (wmax, wmin, sgn))
    {
      kind = VR_VARYING;
      return;
    }

  if (TYPE_OVERFLOW_WRAPS (type))
    {
      /* If overflow wraps, truncate the values and adjust the
	 range kind and bounds appropriately.  */
      wide_int tmin = wide_int::from (wmin, prec, sgn);
      wide_int tmax = wide_int::from (wmax, prec, sgn);
      if ((min_ovf != wi::OVF_NONE) == (max_ovf != wi::OVF_NONE))
	{
	  /* If the limits are swapped, we wrapped around and cover
	     the entire range.  */
	  if (wi::gt_p (tmin, tmax, sgn))
	    kind = VR_VARYING;
	  else
	    {
	      kind = VR_RANGE;
	      /* No overflow or both overflow or underflow.  The
		 range kind stays VR_RANGE.  */
	      min = wide_int_to_tree (type, tmin);
	      max = wide_int_to_tree (type, tmax);
	    }
	  return;
	}
      else if ((min_ovf == wi::OVF_UNDERFLOW && max_ovf == wi::OVF_NONE)
	       || (max_ovf == wi::OVF_OVERFLOW && min_ovf == wi::OVF_NONE))
	{
	  /* Min underflow or max overflow.  The range kind
	     changes to VR_ANTI_RANGE.  */
	  bool covers = false;
	  wide_int tem = tmin;
	  tmin = tmax + 1;
	  if (wi::cmp (tmin, tmax, sgn) < 0)
	    covers = true;
	  tmax = tem - 1;
	  if (wi::cmp (tmax, tem, sgn) > 0)
	    covers = true;
	  /* If the anti-range would cover nothing, drop to varying.
	     Likewise if the anti-range bounds are outside of the
	     types values.  */
	  if (covers || wi::cmp (tmin, tmax, sgn) > 0)
	    {
	      kind = VR_VARYING;
	      return;
	    }
	  kind = VR_ANTI_RANGE;
	  min = wide_int_to_tree (type, tmin);
	  max = wide_int_to_tree (type, tmax);
	  return;
	}
      else
	{
	  /* Other underflow and/or overflow, drop to VR_VARYING.  */
	  kind = VR_VARYING;
	  return;
	}
    }
  else
    {
      /* If overflow does not wrap, saturate to the types min/max
	 value.  */
      wide_int type_min = wi::min_value (prec, sgn);
      wide_int type_max = wi::max_value (prec, sgn);
      kind = VR_RANGE;
      if (min_ovf == wi::OVF_UNDERFLOW)
	min = wide_int_to_tree (type, type_min);
      else if (min_ovf == wi::OVF_OVERFLOW)
	min = wide_int_to_tree (type, type_max);
      else
	min = wide_int_to_tree (type, wmin);

      if (max_ovf == wi::OVF_UNDERFLOW)
	max = wide_int_to_tree (type, type_min);
      else if (max_ovf == wi::OVF_OVERFLOW)
	max = wide_int_to_tree (type, type_max);
      else
	max = wide_int_to_tree (type, wmax);
    }
}

/* Fold two value range's of a POINTER_PLUS_EXPR into VR.  */

static void
extract_range_from_pointer_plus_expr (value_range *vr,
				      enum tree_code code,
				      tree expr_type,
				      const value_range *vr0,
				      const value_range *vr1)
{
  gcc_checking_assert (POINTER_TYPE_P (expr_type)
		       && code == POINTER_PLUS_EXPR);
  /* For pointer types, we are really only interested in asserting
     whether the expression evaluates to non-NULL.
     With -fno-delete-null-pointer-checks we need to be more
     conservative.  As some object might reside at address 0,
     then some offset could be added to it and the same offset
     subtracted again and the result would be NULL.
     E.g.
     static int a[12]; where &a[0] is NULL and
     ptr = &a[6];
     ptr -= 6;
     ptr will be NULL here, even when there is POINTER_PLUS_EXPR
     where the first range doesn't include zero and the second one
     doesn't either.  As the second operand is sizetype (unsigned),
     consider all ranges where the MSB could be set as possible
     subtractions where the result might be NULL.  */
  if ((!range_includes_zero_p (vr0)
       || !range_includes_zero_p (vr1))
      && !TYPE_OVERFLOW_WRAPS (expr_type)
      && (flag_delete_null_pointer_checks
	  || (range_int_cst_p (vr1)
	      && !tree_int_cst_sign_bit (vr1->max ()))))
    vr->set_nonzero (expr_type);
  else if (vr0->zero_p () && vr1->zero_p ())
    vr->set_zero (expr_type);
  else
    vr->set_varying (expr_type);
}

/* Extract range information from a PLUS/MINUS_EXPR and store the
   result in *VR.  */

static void
extract_range_from_plus_minus_expr (value_range *vr,
				    enum tree_code code,
				    tree expr_type,
				    const value_range *vr0_,
				    const value_range *vr1_)
{
  gcc_checking_assert (code == PLUS_EXPR || code == MINUS_EXPR);

  value_range vr0 = *vr0_, vr1 = *vr1_;
  value_range vrtem0, vrtem1;

  /* Now canonicalize anti-ranges to ranges when they are not symbolic
     and express ~[] op X as ([]' op X) U ([]'' op X).  */
  if (vr0.kind () == VR_ANTI_RANGE
      && ranges_from_anti_range (&vr0, &vrtem0, &vrtem1))
    {
      extract_range_from_plus_minus_expr (vr, code, expr_type, &vrtem0, vr1_);
      if (!vrtem1.undefined_p ())
	{
	  value_range vrres;
	  extract_range_from_plus_minus_expr (&vrres, code, expr_type,
					      &vrtem1, vr1_);
	  vr->union_ (&vrres);
	}
      return;
    }
  /* Likewise for X op ~[].  */
  if (vr1.kind () == VR_ANTI_RANGE
      && ranges_from_anti_range (&vr1, &vrtem0, &vrtem1))
    {
      extract_range_from_plus_minus_expr (vr, code, expr_type, vr0_, &vrtem0);
      if (!vrtem1.undefined_p ())
	{
	  value_range vrres;
	  extract_range_from_plus_minus_expr (&vrres, code, expr_type,
					      vr0_, &vrtem1);
	  vr->union_ (&vrres);
	}
      return;
    }

  value_range_kind kind;
  value_range_kind vr0_kind = vr0.kind (), vr1_kind = vr1.kind ();
  tree vr0_min = vr0.min (), vr0_max = vr0.max ();
  tree vr1_min = vr1.min (), vr1_max = vr1.max ();
  tree min = NULL_TREE, max = NULL_TREE;

  /* This will normalize things such that calculating
     [0,0] - VR_VARYING is not dropped to varying, but is
     calculated as [MIN+1, MAX].  */
  if (vr0.varying_p ())
    {
      vr0_kind = VR_RANGE;
      vr0_min = vrp_val_min (expr_type);
      vr0_max = vrp_val_max (expr_type);
    }
  if (vr1.varying_p ())
    {
      vr1_kind = VR_RANGE;
      vr1_min = vrp_val_min (expr_type);
      vr1_max = vrp_val_max (expr_type);
    }

  const bool minus_p = (code == MINUS_EXPR);
  tree min_op0 = vr0_min;
  tree min_op1 = minus_p ? vr1_max : vr1_min;
  tree max_op0 = vr0_max;
  tree max_op1 = minus_p ? vr1_min : vr1_max;
  tree sym_min_op0 = NULL_TREE;
  tree sym_min_op1 = NULL_TREE;
  tree sym_max_op0 = NULL_TREE;
  tree sym_max_op1 = NULL_TREE;
  bool neg_min_op0, neg_min_op1, neg_max_op0, neg_max_op1;

  neg_min_op0 = neg_min_op1 = neg_max_op0 = neg_max_op1 = false;

  /* If we have a PLUS or MINUS with two VR_RANGEs, either constant or
     single-symbolic ranges, try to compute the precise resulting range,
     but only if we know that this resulting range will also be constant
     or single-symbolic.  */
  if (vr0_kind == VR_RANGE && vr1_kind == VR_RANGE
      && (TREE_CODE (min_op0) == INTEGER_CST
	  || (sym_min_op0
	      = get_single_symbol (min_op0, &neg_min_op0, &min_op0)))
      && (TREE_CODE (min_op1) == INTEGER_CST
	  || (sym_min_op1
	      = get_single_symbol (min_op1, &neg_min_op1, &min_op1)))
      && (!(sym_min_op0 && sym_min_op1)
	  || (sym_min_op0 == sym_min_op1
	      && neg_min_op0 == (minus_p ? neg_min_op1 : !neg_min_op1)))
      && (TREE_CODE (max_op0) == INTEGER_CST
	  || (sym_max_op0
	      = get_single_symbol (max_op0, &neg_max_op0, &max_op0)))
      && (TREE_CODE (max_op1) == INTEGER_CST
	  || (sym_max_op1
	      = get_single_symbol (max_op1, &neg_max_op1, &max_op1)))
      && (!(sym_max_op0 && sym_max_op1)
	  || (sym_max_op0 == sym_max_op1
	      && neg_max_op0 == (minus_p ? neg_max_op1 : !neg_max_op1))))
    {
      wide_int wmin, wmax;
      wi::overflow_type min_ovf = wi::OVF_NONE;
      wi::overflow_type max_ovf = wi::OVF_NONE;

      /* Build the bounds.  */
      combine_bound (code, wmin, min_ovf, expr_type, min_op0, min_op1);
      combine_bound (code, wmax, max_ovf, expr_type, max_op0, max_op1);

      /* If the resulting range will be symbolic, we need to eliminate any
	 explicit or implicit overflow introduced in the above computation
	 because compare_values could make an incorrect use of it.  That's
	 why we require one of the ranges to be a singleton.  */
      if ((sym_min_op0 != sym_min_op1 || sym_max_op0 != sym_max_op1)
	  && ((bool)min_ovf || (bool)max_ovf
	      || (min_op0 != max_op0 && min_op1 != max_op1)))
	{
	  vr->set_varying (expr_type);
	  return;
	}

      /* Adjust the range for possible overflow.  */
      set_value_range_with_overflow (kind, min, max, expr_type,
				     wmin, wmax, min_ovf, max_ovf);
      if (kind == VR_VARYING)
	{
	  vr->set_varying (expr_type);
	  return;
	}

      /* Build the symbolic bounds if needed.  */
      adjust_symbolic_bound (min, code, expr_type,
			     sym_min_op0, sym_min_op1,
			     neg_min_op0, neg_min_op1);
      adjust_symbolic_bound (max, code, expr_type,
			     sym_max_op0, sym_max_op1,
			     neg_max_op0, neg_max_op1);
    }
  else
    {
      /* For other cases, for example if we have a PLUS_EXPR with two
	 VR_ANTI_RANGEs, drop to VR_VARYING.  It would take more effort
	 to compute a precise range for such a case.
	 ???  General even mixed range kind operations can be expressed
	 by for example transforming ~[3, 5] + [1, 2] to range-only
	 operations and a union primitive:
	 [-INF, 2] + [1, 2]  U  [5, +INF] + [1, 2]
	 [-INF+1, 4]     U    [6, +INF(OVF)]
	 though usually the union is not exactly representable with
	 a single range or anti-range as the above is
	 [-INF+1, +INF(OVF)] intersected with ~[5, 5]
	 but one could use a scheme similar to equivalences for this. */
      vr->set_varying (expr_type);
      return;
    }

  /* If either MIN or MAX overflowed, then set the resulting range to
     VARYING.  */
  if (min == NULL_TREE
      || TREE_OVERFLOW_P (min)
      || max == NULL_TREE
      || TREE_OVERFLOW_P (max))
    {
      vr->set_varying (expr_type);
      return;
    }

  int cmp = compare_values (min, max);
  if (cmp == -2 || cmp == 1)
    {
      /* If the new range has its limits swapped around (MIN > MAX),
	 then the operation caused one of them to wrap around, mark
	 the new range VARYING.  */
      vr->set_varying (expr_type);
    }
  else
    vr->set (min, max, kind);
}

/* Return the range-ops handler for CODE and EXPR_TYPE.  If no
   suitable operator is found, return NULL and set VR to VARYING.  */

static const range_operator *
get_range_op_handler (value_range *vr,
		      enum tree_code code,
		      tree expr_type)
{
  const range_operator *op = range_op_handler (code, expr_type);
  if (!op)
    vr->set_varying (expr_type);
  return op;
}

/* If the types passed are supported, return TRUE, otherwise set VR to
   VARYING and return FALSE.  */

static bool
supported_types_p (value_range *vr,
		   tree type0,
		   tree type1 = NULL)
{
  if (!value_range::supports_type_p (type0)
      || (type1 && !value_range::supports_type_p (type1)))
    {
      vr->set_varying (type0);
      return false;
    }
  return true;
}

/* If any of the ranges passed are defined, return TRUE, otherwise set
   VR to UNDEFINED and return FALSE.  */

static bool
defined_ranges_p (value_range *vr,
		  const value_range *vr0, const value_range *vr1 = NULL)
{
  if (vr0->undefined_p () && (!vr1 || vr1->undefined_p ()))
    {
      vr->set_undefined ();
      return false;
    }
  return true;
}

static value_range
drop_undefines_to_varying (const value_range *vr, tree expr_type)
{
  if (vr->undefined_p ())
    return value_range (expr_type);
  else
    return *vr;
}

/* If any operand is symbolic, perform a binary operation on them and
   return TRUE, otherwise return FALSE.  */

static bool
range_fold_binary_symbolics_p (value_range *vr,
			       tree_code code,
			       tree expr_type,
			       const value_range *vr0, const value_range *vr1)
{
  if (vr0->symbolic_p () || vr1->symbolic_p ())
    {
      if ((code == PLUS_EXPR || code == MINUS_EXPR))
	{
	  extract_range_from_plus_minus_expr (vr, code, expr_type, vr0, vr1);
	  return true;
	}
      if (POINTER_TYPE_P (expr_type) && code == POINTER_PLUS_EXPR)
	{
	  extract_range_from_pointer_plus_expr (vr, code, expr_type, vr0, vr1);
	  return true;
	}
      const range_operator *op = get_range_op_handler (vr, code, expr_type);
      value_range vr0_cst (*vr0), vr1_cst (*vr1);
      vr0_cst.normalize_symbolics ();
      vr1_cst.normalize_symbolics ();
      return op->fold_range (*vr, expr_type, vr0_cst, vr1_cst);
    }
  return false;
}

/* If operand is symbolic, perform a unary operation on it and return
   TRUE, otherwise return FALSE.  */

static bool
range_fold_unary_symbolics_p (value_range *vr,
			      tree_code code,
			      tree expr_type,
			      const value_range *vr0)
{
  if (vr0->symbolic_p ())
    {
      if (code == NEGATE_EXPR)
	{
	  /* -X is simply 0 - X.  */
	  value_range zero;
	  zero.set_zero (vr0->type ());
	  range_fold_binary_expr (vr, MINUS_EXPR, expr_type, &zero, vr0);
	  return true;
	}
      if (code == BIT_NOT_EXPR)
	{
	  /* ~X is simply -1 - X.  */
	  value_range minusone;
	  minusone.set (build_int_cst (vr0->type (), -1));
	  range_fold_binary_expr (vr, MINUS_EXPR, expr_type, &minusone, vr0);
	  return true;
	}
      const range_operator *op = get_range_op_handler (vr, code, expr_type);
      value_range vr0_cst (*vr0);
      vr0_cst.normalize_symbolics ();
      return op->fold_range (*vr, expr_type, vr0_cst, value_range (expr_type));
    }
  return false;
}

/* Perform a binary operation on a pair of ranges.  */

void
range_fold_binary_expr (value_range *vr,
			enum tree_code code,
			tree expr_type,
			const value_range *vr0_,
			const value_range *vr1_)
{
  if (!supported_types_p (vr, expr_type)
      || !defined_ranges_p (vr, vr0_, vr1_))
    return;
  const range_operator *op = get_range_op_handler (vr, code, expr_type);
  if (!op)
    return;

  value_range vr0 = drop_undefines_to_varying (vr0_, expr_type);
  value_range vr1 = drop_undefines_to_varying (vr1_, expr_type);
  if (range_fold_binary_symbolics_p (vr, code, expr_type, &vr0, &vr1))
    return;

  vr0.normalize_addresses ();
  vr1.normalize_addresses ();
  op->fold_range (*vr, expr_type, vr0, vr1);
}

/* Perform a unary operation on a range.  */

void
range_fold_unary_expr (value_range *vr,
		       enum tree_code code, tree expr_type,
		       const value_range *vr0,
		       tree vr0_type)
{
  if (!supported_types_p (vr, expr_type, vr0_type)
      || !defined_ranges_p (vr, vr0))
    return;
  const range_operator *op = get_range_op_handler (vr, code, expr_type);
  if (!op)
    return;

  if (range_fold_unary_symbolics_p (vr, code, expr_type, vr0))
    return;

  value_range vr0_cst (*vr0);
  vr0_cst.normalize_addresses ();
  op->fold_range (*vr, expr_type, vr0_cst, value_range (expr_type));
}

/* Given a COND_EXPR COND of the form 'V OP W', and an SSA name V,
   create a new SSA name N and return the assertion assignment
   'N = ASSERT_EXPR <V, V OP W>'.  */

static gimple *
build_assert_expr_for (tree cond, tree v)
{
  tree a;
  gassign *assertion;

  gcc_assert (TREE_CODE (v) == SSA_NAME
	      && COMPARISON_CLASS_P (cond));

  a = build2 (ASSERT_EXPR, TREE_TYPE (v), v, cond);
  assertion = gimple_build_assign (NULL_TREE, a);

  /* The new ASSERT_EXPR, creates a new SSA name that replaces the
     operand of the ASSERT_EXPR.  Create it so the new name and the old one
     are registered in the replacement table so that we can fix the SSA web
     after adding all the ASSERT_EXPRs.  */
  tree new_def = create_new_def_for (v, assertion, NULL);
  /* Make sure we preserve abnormalness throughout an ASSERT_EXPR chain
     given we have to be able to fully propagate those out to re-create
     valid SSA when removing the asserts.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (v))
    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (new_def) = 1;

  return assertion;
}


/* Return false if EXPR is a predicate expression involving floating
   point values.  */

static inline bool
fp_predicate (gimple *stmt)
{
  GIMPLE_CHECK (stmt, GIMPLE_COND);

  return FLOAT_TYPE_P (TREE_TYPE (gimple_cond_lhs (stmt)));
}

/* If the range of values taken by OP can be inferred after STMT executes,
   return the comparison code (COMP_CODE_P) and value (VAL_P) that
   describes the inferred range.  Return true if a range could be
   inferred.  */

bool
infer_value_range (gimple *stmt, tree op, tree_code *comp_code_p, tree *val_p)
{
  *val_p = NULL_TREE;
  *comp_code_p = ERROR_MARK;

  /* Do not attempt to infer anything in names that flow through
     abnormal edges.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
    return false;

  /* If STMT is the last statement of a basic block with no normal
     successors, there is no point inferring anything about any of its
     operands.  We would not be able to find a proper insertion point
     for the assertion, anyway.  */
  if (stmt_ends_bb_p (stmt))
    {
      edge_iterator ei;
      edge e;

      FOR_EACH_EDGE (e, ei, gimple_bb (stmt)->succs)
	if (!(e->flags & (EDGE_ABNORMAL|EDGE_EH)))
	  break;
      if (e == NULL)
	return false;
    }

  if (infer_nonnull_range (stmt, op))
    {
      *val_p = build_int_cst (TREE_TYPE (op), 0);
      *comp_code_p = NE_EXPR;
      return true;
    }

  return false;
}


void dump_asserts_for (FILE *, tree);
void debug_asserts_for (tree);
void dump_all_asserts (FILE *);
void debug_all_asserts (void);

/* Dump all the registered assertions for NAME to FILE.  */

void
dump_asserts_for (FILE *file, tree name)
{
  assert_locus *loc;

  fprintf (file, "Assertions to be inserted for ");
  print_generic_expr (file, name);
  fprintf (file, "\n");

  loc = asserts_for[SSA_NAME_VERSION (name)];
  while (loc)
    {
      fprintf (file, "\t");
      print_gimple_stmt (file, gsi_stmt (loc->si), 0);
      fprintf (file, "\n\tBB #%d", loc->bb->index);
      if (loc->e)
	{
	  fprintf (file, "\n\tEDGE %d->%d", loc->e->src->index,
	           loc->e->dest->index);
	  dump_edge_info (file, loc->e, dump_flags, 0);
	}
      fprintf (file, "\n\tPREDICATE: ");
      print_generic_expr (file, loc->expr);
      fprintf (file, " %s ", get_tree_code_name (loc->comp_code));
      print_generic_expr (file, loc->val);
      fprintf (file, "\n\n");
      loc = loc->next;
    }

  fprintf (file, "\n");
}


/* Dump all the registered assertions for NAME to stderr.  */

DEBUG_FUNCTION void
debug_asserts_for (tree name)
{
  dump_asserts_for (stderr, name);
}


/* Dump all the registered assertions for all the names to FILE.  */

void
dump_all_asserts (FILE *file)
{
  unsigned i;
  bitmap_iterator bi;

  fprintf (file, "\nASSERT_EXPRs to be inserted\n\n");
  EXECUTE_IF_SET_IN_BITMAP (need_assert_for, 0, i, bi)
    dump_asserts_for (file, ssa_name (i));
  fprintf (file, "\n");
}


/* Dump all the registered assertions for all the names to stderr.  */

DEBUG_FUNCTION void
debug_all_asserts (void)
{
  dump_all_asserts (stderr);
}

/* Dump assert_info structure.  */

void
dump_assert_info (FILE *file, const assert_info &assert)
{
  fprintf (file, "Assert for: ");
  print_generic_expr (file, assert.name);
  fprintf (file, "\n\tPREDICATE: expr=[");
  print_generic_expr (file, assert.expr);
  fprintf (file, "] %s ", get_tree_code_name (assert.comp_code));
  fprintf (file, "val=[");
  print_generic_expr (file, assert.val);
  fprintf (file, "]\n\n");
}

DEBUG_FUNCTION void
debug (const assert_info &assert)
{
  dump_assert_info (stderr, assert);
}

/* Dump a vector of assert_info's.  */

void
dump_asserts_info (FILE *file, const vec<assert_info> &asserts)
{
  for (unsigned i = 0; i < asserts.length (); ++i)
    {
      dump_assert_info (file, asserts[i]);
      fprintf (file, "\n");
    }
}

DEBUG_FUNCTION void
debug (const vec<assert_info> &asserts)
{
  dump_asserts_info (stderr, asserts);
}

/* Push the assert info for NAME, EXPR, COMP_CODE and VAL to ASSERTS.  */

static void
add_assert_info (vec<assert_info> &asserts,
		 tree name, tree expr, enum tree_code comp_code, tree val)
{
  assert_info info;
  info.comp_code = comp_code;
  info.name = name;
  if (TREE_OVERFLOW_P (val))
    val = drop_tree_overflow (val);
  info.val = val;
  info.expr = expr;
  asserts.safe_push (info);
  if (dump_enabled_p ())
    dump_printf (MSG_NOTE | MSG_PRIORITY_INTERNALS,
		 "Adding assert for %T from %T %s %T\n",
		 name, expr, op_symbol_code (comp_code), val);
}

/* If NAME doesn't have an ASSERT_EXPR registered for asserting
   'EXPR COMP_CODE VAL' at a location that dominates block BB or
   E->DEST, then register this location as a possible insertion point
   for ASSERT_EXPR <NAME, EXPR COMP_CODE VAL>.

   BB, E and SI provide the exact insertion point for the new
   ASSERT_EXPR.  If BB is NULL, then the ASSERT_EXPR is to be inserted
   on edge E.  Otherwise, if E is NULL, the ASSERT_EXPR is inserted on
   BB.  If SI points to a COND_EXPR or a SWITCH_EXPR statement, then E
   must not be NULL.  */

static void
register_new_assert_for (tree name, tree expr,
			 enum tree_code comp_code,
			 tree val,
			 basic_block bb,
			 edge e,
			 gimple_stmt_iterator si)
{
  assert_locus *n, *loc, *last_loc;
  basic_block dest_bb;

  gcc_checking_assert (bb == NULL || e == NULL);

  if (e == NULL)
    gcc_checking_assert (gimple_code (gsi_stmt (si)) != GIMPLE_COND
			 && gimple_code (gsi_stmt (si)) != GIMPLE_SWITCH);

  /* Never build an assert comparing against an integer constant with
     TREE_OVERFLOW set.  This confuses our undefined overflow warning
     machinery.  */
  if (TREE_OVERFLOW_P (val))
    val = drop_tree_overflow (val);

  /* The new assertion A will be inserted at BB or E.  We need to
     determine if the new location is dominated by a previously
     registered location for A.  If we are doing an edge insertion,
     assume that A will be inserted at E->DEST.  Note that this is not
     necessarily true.

     If E is a critical edge, it will be split.  But even if E is
     split, the new block will dominate the same set of blocks that
     E->DEST dominates.

     The reverse, however, is not true, blocks dominated by E->DEST
     will not be dominated by the new block created to split E.  So,
     if the insertion location is on a critical edge, we will not use
     the new location to move another assertion previously registered
     at a block dominated by E->DEST.  */
  dest_bb = (bb) ? bb : e->dest;

  /* If NAME already has an ASSERT_EXPR registered for COMP_CODE and
     VAL at a block dominating DEST_BB, then we don't need to insert a new
     one.  Similarly, if the same assertion already exists at a block
     dominated by DEST_BB and the new location is not on a critical
     edge, then update the existing location for the assertion (i.e.,
     move the assertion up in the dominance tree).

     Note, this is implemented as a simple linked list because there
     should not be more than a handful of assertions registered per
     name.  If this becomes a performance problem, a table hashed by
     COMP_CODE and VAL could be implemented.  */
  loc = asserts_for[SSA_NAME_VERSION (name)];
  last_loc = loc;
  while (loc)
    {
      if (loc->comp_code == comp_code
	  && (loc->val == val
	      || operand_equal_p (loc->val, val, 0))
	  && (loc->expr == expr
	      || operand_equal_p (loc->expr, expr, 0)))
	{
	  /* If E is not a critical edge and DEST_BB
	     dominates the existing location for the assertion, move
	     the assertion up in the dominance tree by updating its
	     location information.  */
	  if ((e == NULL || !EDGE_CRITICAL_P (e))
	      && dominated_by_p (CDI_DOMINATORS, loc->bb, dest_bb))
	    {
	      loc->bb = dest_bb;
	      loc->e = e;
	      loc->si = si;
	      return;
	    }
	}

      /* Update the last node of the list and move to the next one.  */
      last_loc = loc;
      loc = loc->next;
    }

  /* If we didn't find an assertion already registered for
     NAME COMP_CODE VAL, add a new one at the end of the list of
     assertions associated with NAME.  */
  n = XNEW (struct assert_locus);
  n->bb = dest_bb;
  n->e = e;
  n->si = si;
  n->comp_code = comp_code;
  n->val = val;
  n->expr = expr;
  n->next = NULL;

  if (last_loc)
    last_loc->next = n;
  else
    asserts_for[SSA_NAME_VERSION (name)] = n;

  bitmap_set_bit (need_assert_for, SSA_NAME_VERSION (name));
}

/* (COND_OP0 COND_CODE COND_OP1) is a predicate which uses NAME.
   Extract a suitable test code and value and store them into *CODE_P and
   *VAL_P so the predicate is normalized to NAME *CODE_P *VAL_P.

   If no extraction was possible, return FALSE, otherwise return TRUE.

   If INVERT is true, then we invert the result stored into *CODE_P.  */

static bool
extract_code_and_val_from_cond_with_ops (tree name, enum tree_code cond_code,
					 tree cond_op0, tree cond_op1,
					 bool invert, enum tree_code *code_p,
					 tree *val_p)
{
  enum tree_code comp_code;
  tree val;

  /* Otherwise, we have a comparison of the form NAME COMP VAL
     or VAL COMP NAME.  */
  if (name == cond_op1)
    {
      /* If the predicate is of the form VAL COMP NAME, flip
	 COMP around because we need to register NAME as the
	 first operand in the predicate.  */
      comp_code = swap_tree_comparison (cond_code);
      val = cond_op0;
    }
  else if (name == cond_op0)
    {
      /* The comparison is of the form NAME COMP VAL, so the
	 comparison code remains unchanged.  */
      comp_code = cond_code;
      val = cond_op1;
    }
  else
    gcc_unreachable ();

  /* Invert the comparison code as necessary.  */
  if (invert)
    comp_code = invert_tree_comparison (comp_code, 0);

  /* VRP only handles integral and pointer types.  */
  if (! INTEGRAL_TYPE_P (TREE_TYPE (val))
      && ! POINTER_TYPE_P (TREE_TYPE (val)))
    return false;

  /* Do not register always-false predicates.
     FIXME:  this works around a limitation in fold() when dealing with
     enumerations.  Given 'enum { N1, N2 } x;', fold will not
     fold 'if (x > N2)' to 'if (0)'.  */
  if ((comp_code == GT_EXPR || comp_code == LT_EXPR)
      && INTEGRAL_TYPE_P (TREE_TYPE (val)))
    {
      tree min = TYPE_MIN_VALUE (TREE_TYPE (val));
      tree max = TYPE_MAX_VALUE (TREE_TYPE (val));

      if (comp_code == GT_EXPR
	  && (!max
	      || compare_values (val, max) == 0))
	return false;

      if (comp_code == LT_EXPR
	  && (!min
	      || compare_values (val, min) == 0))
	return false;
    }
  *code_p = comp_code;
  *val_p = val;
  return true;
}

/* Find out smallest RES where RES > VAL && (RES & MASK) == RES, if any
   (otherwise return VAL).  VAL and MASK must be zero-extended for
   precision PREC.  If SGNBIT is non-zero, first xor VAL with SGNBIT
   (to transform signed values into unsigned) and at the end xor
   SGNBIT back.  */

static wide_int
masked_increment (const wide_int &val_in, const wide_int &mask,
		  const wide_int &sgnbit, unsigned int prec)
{
  wide_int bit = wi::one (prec), res;
  unsigned int i;

  wide_int val = val_in ^ sgnbit;
  for (i = 0; i < prec; i++, bit += bit)
    {
      res = mask;
      if ((res & bit) == 0)
	continue;
      res = bit - 1;
      res = wi::bit_and_not (val + bit, res);
      res &= mask;
      if (wi::gtu_p (res, val))
	return res ^ sgnbit;
    }
  return val ^ sgnbit;
}

/* Helper for overflow_comparison_p

   OP0 CODE OP1 is a comparison.  Examine the comparison and potentially
   OP1's defining statement to see if it ultimately has the form
   OP0 CODE (OP0 PLUS INTEGER_CST)

   If so, return TRUE indicating this is an overflow test and store into
   *NEW_CST an updated constant that can be used in a narrowed range test.

   REVERSED indicates if the comparison was originally:

   OP1 CODE' OP0.

   This affects how we build the updated constant.  */

static bool
overflow_comparison_p_1 (enum tree_code code, tree op0, tree op1,
		         bool follow_assert_exprs, bool reversed, tree *new_cst)
{
  /* See if this is a relational operation between two SSA_NAMES with
     unsigned, overflow wrapping values.  If so, check it more deeply.  */
  if ((code == LT_EXPR || code == LE_EXPR
       || code == GE_EXPR || code == GT_EXPR)
      && TREE_CODE (op0) == SSA_NAME
      && TREE_CODE (op1) == SSA_NAME
      && INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && TYPE_UNSIGNED (TREE_TYPE (op0))
      && TYPE_OVERFLOW_WRAPS (TREE_TYPE (op0)))
    {
      gimple *op1_def = SSA_NAME_DEF_STMT (op1);

      /* If requested, follow any ASSERT_EXPRs backwards for OP1.  */
      if (follow_assert_exprs)
	{
	  while (gimple_assign_single_p (op1_def)
		 && TREE_CODE (gimple_assign_rhs1 (op1_def)) == ASSERT_EXPR)
	    {
	      op1 = TREE_OPERAND (gimple_assign_rhs1 (op1_def), 0);
	      if (TREE_CODE (op1) != SSA_NAME)
		break;
	      op1_def = SSA_NAME_DEF_STMT (op1);
	    }
	}

      /* Now look at the defining statement of OP1 to see if it adds
	 or subtracts a nonzero constant from another operand.  */
      if (op1_def
	  && is_gimple_assign (op1_def)
	  && gimple_assign_rhs_code (op1_def) == PLUS_EXPR
	  && TREE_CODE (gimple_assign_rhs2 (op1_def)) == INTEGER_CST
	  && !integer_zerop (gimple_assign_rhs2 (op1_def)))
	{
	  tree target = gimple_assign_rhs1 (op1_def);

	  /* If requested, follow ASSERT_EXPRs backwards for op0 looking
	     for one where TARGET appears on the RHS.  */
	  if (follow_assert_exprs)
	    {
	      /* Now see if that "other operand" is op0, following the chain
		 of ASSERT_EXPRs if necessary.  */
	      gimple *op0_def = SSA_NAME_DEF_STMT (op0);
	      while (op0 != target
		     && gimple_assign_single_p (op0_def)
		     && TREE_CODE (gimple_assign_rhs1 (op0_def)) == ASSERT_EXPR)
		{
		  op0 = TREE_OPERAND (gimple_assign_rhs1 (op0_def), 0);
		  if (TREE_CODE (op0) != SSA_NAME)
		    break;
		  op0_def = SSA_NAME_DEF_STMT (op0);
		}
	    }

	  /* If we did not find our target SSA_NAME, then this is not
	     an overflow test.  */
	  if (op0 != target)
	    return false;

	  tree type = TREE_TYPE (op0);
	  wide_int max = wi::max_value (TYPE_PRECISION (type), UNSIGNED);
	  tree inc = gimple_assign_rhs2 (op1_def);
	  if (reversed)
	    *new_cst = wide_int_to_tree (type, max + wi::to_wide (inc));
	  else
	    *new_cst = wide_int_to_tree (type, max - wi::to_wide (inc));
	  return true;
	}
    }
  return false;
}

/* OP0 CODE OP1 is a comparison.  Examine the comparison and potentially
   OP1's defining statement to see if it ultimately has the form
   OP0 CODE (OP0 PLUS INTEGER_CST)

   If so, return TRUE indicating this is an overflow test and store into
   *NEW_CST an updated constant that can be used in a narrowed range test.

   These statements are left as-is in the IL to facilitate discovery of
   {ADD,SUB}_OVERFLOW sequences later in the optimizer pipeline.  But
   the alternate range representation is often useful within VRP.  */

bool
overflow_comparison_p (tree_code code, tree name, tree val,
		       bool use_equiv_p, tree *new_cst)
{
  if (overflow_comparison_p_1 (code, name, val, use_equiv_p, false, new_cst))
    return true;
  return overflow_comparison_p_1 (swap_tree_comparison (code), val, name,
				  use_equiv_p, true, new_cst);
}


/* Try to register an edge assertion for SSA name NAME on edge E for
   the condition COND contributing to the conditional jump pointed to by BSI.
   Invert the condition COND if INVERT is true.  */

static void
register_edge_assert_for_2 (tree name, edge e,
			    enum tree_code cond_code,
			    tree cond_op0, tree cond_op1, bool invert,
			    vec<assert_info> &asserts)
{
  tree val;
  enum tree_code comp_code;

  if (!extract_code_and_val_from_cond_with_ops (name, cond_code,
						cond_op0,
						cond_op1,
						invert, &comp_code, &val))
    return;

  /* Queue the assert.  */
  tree x;
  if (overflow_comparison_p (comp_code, name, val, false, &x))
    {
      enum tree_code new_code = ((comp_code == GT_EXPR || comp_code == GE_EXPR)
				 ? GT_EXPR : LE_EXPR);
      add_assert_info (asserts, name, name, new_code, x);
    }
  add_assert_info (asserts, name, name, comp_code, val);

  /* In the case of NAME <= CST and NAME being defined as
     NAME = (unsigned) NAME2 + CST2 we can assert NAME2 >= -CST2
     and NAME2 <= CST - CST2.  We can do the same for NAME > CST.
     This catches range and anti-range tests.  */
  if ((comp_code == LE_EXPR
       || comp_code == GT_EXPR)
      && TREE_CODE (val) == INTEGER_CST
      && TYPE_UNSIGNED (TREE_TYPE (val)))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      tree cst2 = NULL_TREE, name2 = NULL_TREE, name3 = NULL_TREE;

      /* Extract CST2 from the (optional) addition.  */
      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == PLUS_EXPR)
	{
	  name2 = gimple_assign_rhs1 (def_stmt);
	  cst2 = gimple_assign_rhs2 (def_stmt);
	  if (TREE_CODE (name2) == SSA_NAME
	      && TREE_CODE (cst2) == INTEGER_CST)
	    def_stmt = SSA_NAME_DEF_STMT (name2);
	}

      /* Extract NAME2 from the (optional) sign-changing cast.  */
      if (gimple_assign_cast_p (def_stmt))
	{
	  if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt))
	      && ! TYPE_UNSIGNED (TREE_TYPE (gimple_assign_rhs1 (def_stmt)))
	      && (TYPE_PRECISION (gimple_expr_type (def_stmt))
		  == TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (def_stmt)))))
	    name3 = gimple_assign_rhs1 (def_stmt);
	}

      /* If name3 is used later, create an ASSERT_EXPR for it.  */
      if (name3 != NULL_TREE
      	  && TREE_CODE (name3) == SSA_NAME
	  && (cst2 == NULL_TREE
	      || TREE_CODE (cst2) == INTEGER_CST)
	  && INTEGRAL_TYPE_P (TREE_TYPE (name3)))
	{
	  tree tmp;

	  /* Build an expression for the range test.  */
	  tmp = build1 (NOP_EXPR, TREE_TYPE (name), name3);
	  if (cst2 != NULL_TREE)
	    tmp = build2 (PLUS_EXPR, TREE_TYPE (name), tmp, cst2);
	  add_assert_info (asserts, name3, tmp, comp_code, val);
	}

      /* If name2 is used later, create an ASSERT_EXPR for it.  */
      if (name2 != NULL_TREE
      	  && TREE_CODE (name2) == SSA_NAME
	  && TREE_CODE (cst2) == INTEGER_CST
	  && INTEGRAL_TYPE_P (TREE_TYPE (name2)))
	{
	  tree tmp;

	  /* Build an expression for the range test.  */
	  tmp = name2;
	  if (TREE_TYPE (name) != TREE_TYPE (name2))
	    tmp = build1 (NOP_EXPR, TREE_TYPE (name), tmp);
	  if (cst2 != NULL_TREE)
	    tmp = build2 (PLUS_EXPR, TREE_TYPE (name), tmp, cst2);
	  add_assert_info (asserts, name2, tmp, comp_code, val);
	}
    }

  /* In the case of post-in/decrement tests like if (i++) ... and uses
     of the in/decremented value on the edge the extra name we want to
     assert for is not on the def chain of the name compared.  Instead
     it is in the set of use stmts.
     Similar cases happen for conversions that were simplified through
     fold_{sign_changed,widened}_comparison.  */
  if ((comp_code == NE_EXPR
       || comp_code == EQ_EXPR)
      && TREE_CODE (val) == INTEGER_CST)
    {
      imm_use_iterator ui;
      gimple *use_stmt;
      FOR_EACH_IMM_USE_STMT (use_stmt, ui, name)
	{
	  if (!is_gimple_assign (use_stmt))
	    continue;

	  /* Cut off to use-stmts that are dominating the predecessor.  */
	  if (!dominated_by_p (CDI_DOMINATORS, e->src, gimple_bb (use_stmt)))
	    continue;

	  tree name2 = gimple_assign_lhs (use_stmt);
	  if (TREE_CODE (name2) != SSA_NAME)
	    continue;

	  enum tree_code code = gimple_assign_rhs_code (use_stmt);
	  tree cst;
	  if (code == PLUS_EXPR
	      || code == MINUS_EXPR)
	    {
	      cst = gimple_assign_rhs2 (use_stmt);
	      if (TREE_CODE (cst) != INTEGER_CST)
		continue;
	      cst = int_const_binop (code, val, cst);
	    }
	  else if (CONVERT_EXPR_CODE_P (code))
	    {
	      /* For truncating conversions we cannot record
		 an inequality.  */
	      if (comp_code == NE_EXPR
		  && (TYPE_PRECISION (TREE_TYPE (name2))
		      < TYPE_PRECISION (TREE_TYPE (name))))
		continue;
	      cst = fold_convert (TREE_TYPE (name2), val);
	    }
	  else
	    continue;

	  if (TREE_OVERFLOW_P (cst))
	    cst = drop_tree_overflow (cst);
	  add_assert_info (asserts, name2, name2, comp_code, cst);
	}
    }
 
  if (TREE_CODE_CLASS (comp_code) == tcc_comparison
      && TREE_CODE (val) == INTEGER_CST)
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      tree name2 = NULL_TREE, names[2], cst2 = NULL_TREE;
      tree val2 = NULL_TREE;
      unsigned int prec = TYPE_PRECISION (TREE_TYPE (val));
      wide_int mask = wi::zero (prec);
      unsigned int nprec = prec;
      enum tree_code rhs_code = ERROR_MARK;

      if (is_gimple_assign (def_stmt))
	rhs_code = gimple_assign_rhs_code (def_stmt);

      /* In the case of NAME != CST1 where NAME = A +- CST2 we can
         assert that A != CST1 -+ CST2.  */
      if ((comp_code == EQ_EXPR || comp_code == NE_EXPR)
	  && (rhs_code == PLUS_EXPR || rhs_code == MINUS_EXPR))
	{
	  tree op0 = gimple_assign_rhs1 (def_stmt);
	  tree op1 = gimple_assign_rhs2 (def_stmt);
	  if (TREE_CODE (op0) == SSA_NAME
	      && TREE_CODE (op1) == INTEGER_CST)
	    {
	      enum tree_code reverse_op = (rhs_code == PLUS_EXPR
					   ? MINUS_EXPR : PLUS_EXPR);
	      op1 = int_const_binop (reverse_op, val, op1);
	      if (TREE_OVERFLOW (op1))
		op1 = drop_tree_overflow (op1);
	      add_assert_info (asserts, op0, op0, comp_code, op1);
	    }
	}

      /* Add asserts for NAME cmp CST and NAME being defined
	 as NAME = (int) NAME2.  */
      if (!TYPE_UNSIGNED (TREE_TYPE (val))
	  && (comp_code == LE_EXPR || comp_code == LT_EXPR
	      || comp_code == GT_EXPR || comp_code == GE_EXPR)
	  && gimple_assign_cast_p (def_stmt))
	{
	  name2 = gimple_assign_rhs1 (def_stmt);
	  if (CONVERT_EXPR_CODE_P (rhs_code)
	      && TREE_CODE (name2) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (name2))
	      && TYPE_UNSIGNED (TREE_TYPE (name2))
	      && prec == TYPE_PRECISION (TREE_TYPE (name2))
	      && (comp_code == LE_EXPR || comp_code == GT_EXPR
		  || !tree_int_cst_equal (val,
					  TYPE_MIN_VALUE (TREE_TYPE (val)))))
	    {
	      tree tmp, cst;
	      enum tree_code new_comp_code = comp_code;

	      cst = fold_convert (TREE_TYPE (name2),
				  TYPE_MIN_VALUE (TREE_TYPE (val)));
	      /* Build an expression for the range test.  */
	      tmp = build2 (PLUS_EXPR, TREE_TYPE (name2), name2, cst);
	      cst = fold_build2 (PLUS_EXPR, TREE_TYPE (name2), cst,
				 fold_convert (TREE_TYPE (name2), val));
	      if (comp_code == LT_EXPR || comp_code == GE_EXPR)
		{
		  new_comp_code = comp_code == LT_EXPR ? LE_EXPR : GT_EXPR;
		  cst = fold_build2 (MINUS_EXPR, TREE_TYPE (name2), cst,
				     build_int_cst (TREE_TYPE (name2), 1));
		}
	      add_assert_info (asserts, name2, tmp, new_comp_code, cst);
	    }
	}

      /* Add asserts for NAME cmp CST and NAME being defined as
	 NAME = NAME2 >> CST2.

	 Extract CST2 from the right shift.  */
      if (rhs_code == RSHIFT_EXPR)
	{
	  name2 = gimple_assign_rhs1 (def_stmt);
	  cst2 = gimple_assign_rhs2 (def_stmt);
	  if (TREE_CODE (name2) == SSA_NAME
	      && tree_fits_uhwi_p (cst2)
	      && INTEGRAL_TYPE_P (TREE_TYPE (name2))
	      && IN_RANGE (tree_to_uhwi (cst2), 1, prec - 1)
	      && type_has_mode_precision_p (TREE_TYPE (val)))
	    {
	      mask = wi::mask (tree_to_uhwi (cst2), false, prec);
	      val2 = fold_binary (LSHIFT_EXPR, TREE_TYPE (val), val, cst2);
	    }
	}
      if (val2 != NULL_TREE
	  && TREE_CODE (val2) == INTEGER_CST
	  && simple_cst_equal (fold_build2 (RSHIFT_EXPR,
					    TREE_TYPE (val),
					    val2, cst2), val))
	{
	  enum tree_code new_comp_code = comp_code;
	  tree tmp, new_val;

	  tmp = name2;
	  if (comp_code == EQ_EXPR || comp_code == NE_EXPR)
	    {
	      if (!TYPE_UNSIGNED (TREE_TYPE (val)))
		{
		  tree type = build_nonstandard_integer_type (prec, 1);
		  tmp = build1 (NOP_EXPR, type, name2);
		  val2 = fold_convert (type, val2);
		}
	      tmp = fold_build2 (MINUS_EXPR, TREE_TYPE (tmp), tmp, val2);
	      new_val = wide_int_to_tree (TREE_TYPE (tmp), mask);
	      new_comp_code = comp_code == EQ_EXPR ? LE_EXPR : GT_EXPR;
	    }
	  else if (comp_code == LT_EXPR || comp_code == GE_EXPR)
	    {
	      wide_int minval
		= wi::min_value (prec, TYPE_SIGN (TREE_TYPE (val)));
	      new_val = val2;
	      if (minval == wi::to_wide (new_val))
		new_val = NULL_TREE;
	    }
	  else
	    {
	      wide_int maxval
		= wi::max_value (prec, TYPE_SIGN (TREE_TYPE (val)));
	      mask |= wi::to_wide (val2);
	      if (wi::eq_p (mask, maxval))
		new_val = NULL_TREE;
	      else
		new_val = wide_int_to_tree (TREE_TYPE (val2), mask);
	    }

	  if (new_val)
	    add_assert_info (asserts, name2, tmp, new_comp_code, new_val);
	}

      /* If we have a conversion that doesn't change the value of the source
         simply register the same assert for it.  */
      if (CONVERT_EXPR_CODE_P (rhs_code))
	{
	  wide_int rmin, rmax;
	  tree rhs1 = gimple_assign_rhs1 (def_stmt);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (rhs1))
	      && TREE_CODE (rhs1) == SSA_NAME
	      /* Make sure the relation preserves the upper/lower boundary of
	         the range conservatively.  */
	      && (comp_code == NE_EXPR
		  || comp_code == EQ_EXPR
		  || (TYPE_SIGN (TREE_TYPE (name))
		      == TYPE_SIGN (TREE_TYPE (rhs1)))
		  || ((comp_code == LE_EXPR
		       || comp_code == LT_EXPR)
		      && !TYPE_UNSIGNED (TREE_TYPE (rhs1)))
		  || ((comp_code == GE_EXPR
		       || comp_code == GT_EXPR)
		      && TYPE_UNSIGNED (TREE_TYPE (rhs1))))
	      /* And the conversion does not alter the value we compare
	         against and all values in rhs1 can be represented in
		 the converted to type.  */
	      && int_fits_type_p (val, TREE_TYPE (rhs1))
	      && ((TYPE_PRECISION (TREE_TYPE (name))
		   > TYPE_PRECISION (TREE_TYPE (rhs1)))
		  || (get_range_info (rhs1, &rmin, &rmax) == VR_RANGE
		      && wi::fits_to_tree_p (rmin, TREE_TYPE (name))
		      && wi::fits_to_tree_p (rmax, TREE_TYPE (name)))))
	    add_assert_info (asserts, rhs1, rhs1,
		 	     comp_code, fold_convert (TREE_TYPE (rhs1), val));
	}

      /* Add asserts for NAME cmp CST and NAME being defined as
	 NAME = NAME2 & CST2.

	 Extract CST2 from the and.

	 Also handle
	 NAME = (unsigned) NAME2;
	 casts where NAME's type is unsigned and has smaller precision
	 than NAME2's type as if it was NAME = NAME2 & MASK.  */
      names[0] = NULL_TREE;
      names[1] = NULL_TREE;
      cst2 = NULL_TREE;
      if (rhs_code == BIT_AND_EXPR
	  || (CONVERT_EXPR_CODE_P (rhs_code)
	      && INTEGRAL_TYPE_P (TREE_TYPE (val))
	      && TYPE_UNSIGNED (TREE_TYPE (val))
	      && TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (def_stmt)))
		 > prec))
	{
	  name2 = gimple_assign_rhs1 (def_stmt);
	  if (rhs_code == BIT_AND_EXPR)
	    cst2 = gimple_assign_rhs2 (def_stmt);
	  else
	    {
	      cst2 = TYPE_MAX_VALUE (TREE_TYPE (val));
	      nprec = TYPE_PRECISION (TREE_TYPE (name2));
	    }
	  if (TREE_CODE (name2) == SSA_NAME
	      && INTEGRAL_TYPE_P (TREE_TYPE (name2))
	      && TREE_CODE (cst2) == INTEGER_CST
	      && !integer_zerop (cst2)
	      && (nprec > 1
		  || TYPE_UNSIGNED (TREE_TYPE (val))))
	    {
	      gimple *def_stmt2 = SSA_NAME_DEF_STMT (name2);
	      if (gimple_assign_cast_p (def_stmt2))
		{
		  names[1] = gimple_assign_rhs1 (def_stmt2);
		  if (!CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (def_stmt2))
		      || TREE_CODE (names[1]) != SSA_NAME
		      || !INTEGRAL_TYPE_P (TREE_TYPE (names[1]))
		      || (TYPE_PRECISION (TREE_TYPE (name2))
			  != TYPE_PRECISION (TREE_TYPE (names[1]))))
		    names[1] = NULL_TREE;
		}
	      names[0] = name2;
	    }
	}
      if (names[0] || names[1])
	{
	  wide_int minv, maxv, valv, cst2v;
	  wide_int tem, sgnbit;
	  bool valid_p = false, valn, cst2n;
	  enum tree_code ccode = comp_code;

	  valv = wide_int::from (wi::to_wide (val), nprec, UNSIGNED);
	  cst2v = wide_int::from (wi::to_wide (cst2), nprec, UNSIGNED);
	  valn = wi::neg_p (valv, TYPE_SIGN (TREE_TYPE (val)));
	  cst2n = wi::neg_p (cst2v, TYPE_SIGN (TREE_TYPE (val)));
	  /* If CST2 doesn't have most significant bit set,
	     but VAL is negative, we have comparison like
	     if ((x & 0x123) > -4) (always true).  Just give up.  */
	  if (!cst2n && valn)
	    ccode = ERROR_MARK;
	  if (cst2n)
	    sgnbit = wi::set_bit_in_zero (nprec - 1, nprec);
	  else
	    sgnbit = wi::zero (nprec);
	  minv = valv & cst2v;
	  switch (ccode)
	    {
	    case EQ_EXPR:
	      /* Minimum unsigned value for equality is VAL & CST2
		 (should be equal to VAL, otherwise we probably should
		 have folded the comparison into false) and
		 maximum unsigned value is VAL | ~CST2.  */
	      maxv = valv | ~cst2v;
	      valid_p = true;
	      break;

	    case NE_EXPR:
	      tem = valv | ~cst2v;
	      /* If VAL is 0, handle (X & CST2) != 0 as (X & CST2) > 0U.  */
	      if (valv == 0)
		{
		  cst2n = false;
		  sgnbit = wi::zero (nprec);
		  goto gt_expr;
		}
	      /* If (VAL | ~CST2) is all ones, handle it as
		 (X & CST2) < VAL.  */
	      if (tem == -1)
		{
		  cst2n = false;
		  valn = false;
		  sgnbit = wi::zero (nprec);
		  goto lt_expr;
		}
	      if (!cst2n && wi::neg_p (cst2v))
		sgnbit = wi::set_bit_in_zero (nprec - 1, nprec);
	      if (sgnbit != 0)
		{
		  if (valv == sgnbit)
		    {
		      cst2n = true;
		      valn = true;
		      goto gt_expr;
		    }
		  if (tem == wi::mask (nprec - 1, false, nprec))
		    {
		      cst2n = true;
		      goto lt_expr;
		    }
		  if (!cst2n)
		    sgnbit = wi::zero (nprec);
		}
	      break;

	    case GE_EXPR:
	      /* Minimum unsigned value for >= if (VAL & CST2) == VAL
		 is VAL and maximum unsigned value is ~0.  For signed
		 comparison, if CST2 doesn't have most significant bit
		 set, handle it similarly.  If CST2 has MSB set,
		 the minimum is the same, and maximum is ~0U/2.  */
	      if (minv != valv)
		{
		  /* If (VAL & CST2) != VAL, X & CST2 can't be equal to
		     VAL.  */
		  minv = masked_increment (valv, cst2v, sgnbit, nprec);
		  if (minv == valv)
		    break;
		}
	      maxv = wi::mask (nprec - (cst2n ? 1 : 0), false, nprec);
	      valid_p = true;
	      break;

	    case GT_EXPR:
	    gt_expr:
	      /* Find out smallest MINV where MINV > VAL
		 && (MINV & CST2) == MINV, if any.  If VAL is signed and
		 CST2 has MSB set, compute it biased by 1 << (nprec - 1).  */
	      minv = masked_increment (valv, cst2v, sgnbit, nprec);
	      if (minv == valv)
		break;
	      maxv = wi::mask (nprec - (cst2n ? 1 : 0), false, nprec);
	      valid_p = true;
	      break;

	    case LE_EXPR:
	      /* Minimum unsigned value for <= is 0 and maximum
		 unsigned value is VAL | ~CST2 if (VAL & CST2) == VAL.
		 Otherwise, find smallest VAL2 where VAL2 > VAL
		 && (VAL2 & CST2) == VAL2 and use (VAL2 - 1) | ~CST2
		 as maximum.
		 For signed comparison, if CST2 doesn't have most
		 significant bit set, handle it similarly.  If CST2 has
		 MSB set, the maximum is the same and minimum is INT_MIN.  */
	      if (minv == valv)
		maxv = valv;
	      else
		{
		  maxv = masked_increment (valv, cst2v, sgnbit, nprec);
		  if (maxv == valv)
		    break;
		  maxv -= 1;
		}
	      maxv |= ~cst2v;
	      minv = sgnbit;
	      valid_p = true;
	      break;

	    case LT_EXPR:
	    lt_expr:
	      /* Minimum unsigned value for < is 0 and maximum
		 unsigned value is (VAL-1) | ~CST2 if (VAL & CST2) == VAL.
		 Otherwise, find smallest VAL2 where VAL2 > VAL
		 && (VAL2 & CST2) == VAL2 and use (VAL2 - 1) | ~CST2
		 as maximum.
		 For signed comparison, if CST2 doesn't have most
		 significant bit set, handle it similarly.  If CST2 has
		 MSB set, the maximum is the same and minimum is INT_MIN.  */
	      if (minv == valv)
		{
		  if (valv == sgnbit)
		    break;
		  maxv = valv;
		}
	      else
		{
		  maxv = masked_increment (valv, cst2v, sgnbit, nprec);
		  if (maxv == valv)
		    break;
		}
	      maxv -= 1;
	      maxv |= ~cst2v;
	      minv = sgnbit;
	      valid_p = true;
	      break;

	    default:
	      break;
	    }
	  if (valid_p
	      && (maxv - minv) != -1)
	    {
	      tree tmp, new_val, type;
	      int i;

	      for (i = 0; i < 2; i++)
		if (names[i])
		  {
		    wide_int maxv2 = maxv;
		    tmp = names[i];
		    type = TREE_TYPE (names[i]);
		    if (!TYPE_UNSIGNED (type))
		      {
			type = build_nonstandard_integer_type (nprec, 1);
			tmp = build1 (NOP_EXPR, type, names[i]);
		      }
		    if (minv != 0)
		      {
			tmp = build2 (PLUS_EXPR, type, tmp,
				      wide_int_to_tree (type, -minv));
			maxv2 = maxv - minv;
		      }
		    new_val = wide_int_to_tree (type, maxv2);
		    add_assert_info (asserts, names[i], tmp, LE_EXPR, new_val);
		  }
	    }
	}
    }
}

/* OP is an operand of a truth value expression which is known to have
   a particular value.  Register any asserts for OP and for any
   operands in OP's defining statement.

   If CODE is EQ_EXPR, then we want to register OP is zero (false),
   if CODE is NE_EXPR, then we want to register OP is nonzero (true).   */

static void
register_edge_assert_for_1 (tree op, enum tree_code code,
			    edge e, vec<assert_info> &asserts)
{
  gimple *op_def;
  tree val;
  enum tree_code rhs_code;

  /* We only care about SSA_NAMEs.  */
  if (TREE_CODE (op) != SSA_NAME)
    return;

  /* We know that OP will have a zero or nonzero value.  */
  val = build_int_cst (TREE_TYPE (op), 0);
  add_assert_info (asserts, op, op, code, val);

  /* Now look at how OP is set.  If it's set from a comparison,
     a truth operation or some bit operations, then we may be able
     to register information about the operands of that assignment.  */
  op_def = SSA_NAME_DEF_STMT (op);
  if (gimple_code (op_def) != GIMPLE_ASSIGN)
    return;

  rhs_code = gimple_assign_rhs_code (op_def);

  if (TREE_CODE_CLASS (rhs_code) == tcc_comparison)
    {
      bool invert = (code == EQ_EXPR ? true : false);
      tree op0 = gimple_assign_rhs1 (op_def);
      tree op1 = gimple_assign_rhs2 (op_def);

      if (TREE_CODE (op0) == SSA_NAME)
        register_edge_assert_for_2 (op0, e, rhs_code, op0, op1, invert, asserts);
      if (TREE_CODE (op1) == SSA_NAME)
        register_edge_assert_for_2 (op1, e, rhs_code, op0, op1, invert, asserts);
    }
  else if ((code == NE_EXPR
	    && gimple_assign_rhs_code (op_def) == BIT_AND_EXPR)
	   || (code == EQ_EXPR
	       && gimple_assign_rhs_code (op_def) == BIT_IOR_EXPR))
    {
      /* Recurse on each operand.  */
      tree op0 = gimple_assign_rhs1 (op_def);
      tree op1 = gimple_assign_rhs2 (op_def);
      if (TREE_CODE (op0) == SSA_NAME
	  && has_single_use (op0))
	register_edge_assert_for_1 (op0, code, e, asserts);
      if (TREE_CODE (op1) == SSA_NAME
	  && has_single_use (op1))
	register_edge_assert_for_1 (op1, code, e, asserts);
    }
  else if (gimple_assign_rhs_code (op_def) == BIT_NOT_EXPR
	   && TYPE_PRECISION (TREE_TYPE (gimple_assign_lhs (op_def))) == 1)
    {
      /* Recurse, flipping CODE.  */
      code = invert_tree_comparison (code, false);
      register_edge_assert_for_1 (gimple_assign_rhs1 (op_def), code, e, asserts);
    }
  else if (gimple_assign_rhs_code (op_def) == SSA_NAME)
    {
      /* Recurse through the copy.  */
      register_edge_assert_for_1 (gimple_assign_rhs1 (op_def), code, e, asserts);
    }
  else if (CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (op_def)))
    {
      /* Recurse through the type conversion, unless it is a narrowing
	 conversion or conversion from non-integral type.  */
      tree rhs = gimple_assign_rhs1 (op_def);
      if (INTEGRAL_TYPE_P (TREE_TYPE (rhs))
	  && (TYPE_PRECISION (TREE_TYPE (rhs))
	      <= TYPE_PRECISION (TREE_TYPE (op))))
	register_edge_assert_for_1 (rhs, code, e, asserts);
    }
}

/* Check if comparison
     NAME COND_OP INTEGER_CST
   has a form of
     (X & 11...100..0) COND_OP XX...X00...0
   Such comparison can yield assertions like
     X >= XX...X00...0
     X <= XX...X11...1
   in case of COND_OP being EQ_EXPR or
     X < XX...X00...0
     X > XX...X11...1
   in case of NE_EXPR.  */

static bool
is_masked_range_test (tree name, tree valt, enum tree_code cond_code,
		      tree *new_name, tree *low, enum tree_code *low_code,
		      tree *high, enum tree_code *high_code)
{
  gimple *def_stmt = SSA_NAME_DEF_STMT (name);

  if (!is_gimple_assign (def_stmt)
      || gimple_assign_rhs_code (def_stmt) != BIT_AND_EXPR)
    return false;

  tree t = gimple_assign_rhs1 (def_stmt);
  tree maskt = gimple_assign_rhs2 (def_stmt);
  if (TREE_CODE (t) != SSA_NAME || TREE_CODE (maskt) != INTEGER_CST)
    return false;

  wi::tree_to_wide_ref mask = wi::to_wide (maskt);
  wide_int inv_mask = ~mask;
  /* Must have been removed by now so don't bother optimizing.  */
  if (mask == 0 || inv_mask == 0)
    return false;

  /* Assume VALT is INTEGER_CST.  */
  wi::tree_to_wide_ref val = wi::to_wide (valt);

  if ((inv_mask & (inv_mask + 1)) != 0
      || (val & mask) != val)
    return false;

  bool is_range = cond_code == EQ_EXPR;

  tree type = TREE_TYPE (t);
  wide_int min = wi::min_value (type),
    max = wi::max_value (type);

  if (is_range)
    {
      *low_code = val == min ? ERROR_MARK : GE_EXPR;
      *high_code = val == max ? ERROR_MARK : LE_EXPR;
    }
  else
    {
      /* We can still generate assertion if one of alternatives
	 is known to always be false.  */
      if (val == min)
	{
	  *low_code = (enum tree_code) 0;
	  *high_code = GT_EXPR;
	}
      else if ((val | inv_mask) == max)
	{
	  *low_code = LT_EXPR;
	  *high_code = (enum tree_code) 0;
	}
      else
	return false;
    }

  *new_name = t;
  *low = wide_int_to_tree (type, val);
  *high = wide_int_to_tree (type, val | inv_mask);

  return true;
}

/* Try to register an edge assertion for SSA name NAME on edge E for
   the condition COND contributing to the conditional jump pointed to by
   SI.  */

void
register_edge_assert_for (tree name, edge e,
			  enum tree_code cond_code, tree cond_op0,
			  tree cond_op1, vec<assert_info> &asserts)
{
  tree val;
  enum tree_code comp_code;
  bool is_else_edge = (e->flags & EDGE_FALSE_VALUE) != 0;

  /* Do not attempt to infer anything in names that flow through
     abnormal edges.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
    return;

  if (!extract_code_and_val_from_cond_with_ops (name, cond_code,
						cond_op0, cond_op1,
						is_else_edge,
						&comp_code, &val))
    return;

  /* Register ASSERT_EXPRs for name.  */
  register_edge_assert_for_2 (name, e, cond_code, cond_op0,
			      cond_op1, is_else_edge, asserts);


  /* If COND is effectively an equality test of an SSA_NAME against
     the value zero or one, then we may be able to assert values
     for SSA_NAMEs which flow into COND.  */

  /* In the case of NAME == 1 or NAME != 0, for BIT_AND_EXPR defining
     statement of NAME we can assert both operands of the BIT_AND_EXPR
     have nonzero value.  */
  if (((comp_code == EQ_EXPR && integer_onep (val))
       || (comp_code == NE_EXPR && integer_zerop (val))))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);

      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == BIT_AND_EXPR)
	{
	  tree op0 = gimple_assign_rhs1 (def_stmt);
	  tree op1 = gimple_assign_rhs2 (def_stmt);
	  register_edge_assert_for_1 (op0, NE_EXPR, e, asserts);
	  register_edge_assert_for_1 (op1, NE_EXPR, e, asserts);
	}
    }

  /* In the case of NAME == 0 or NAME != 1, for BIT_IOR_EXPR defining
     statement of NAME we can assert both operands of the BIT_IOR_EXPR
     have zero value.  */
  if (((comp_code == EQ_EXPR && integer_zerop (val))
       || (comp_code == NE_EXPR && integer_onep (val))))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (name);

      /* For BIT_IOR_EXPR only if NAME == 0 both operands have
	 necessarily zero value, or if type-precision is one.  */
      if (is_gimple_assign (def_stmt)
	  && (gimple_assign_rhs_code (def_stmt) == BIT_IOR_EXPR
	      && (TYPE_PRECISION (TREE_TYPE (name)) == 1
	          || comp_code == EQ_EXPR)))
	{
	  tree op0 = gimple_assign_rhs1 (def_stmt);
	  tree op1 = gimple_assign_rhs2 (def_stmt);
	  register_edge_assert_for_1 (op0, EQ_EXPR, e, asserts);
	  register_edge_assert_for_1 (op1, EQ_EXPR, e, asserts);
	}
    }

  /* Sometimes we can infer ranges from (NAME & MASK) == VALUE.  */
  if ((comp_code == EQ_EXPR || comp_code == NE_EXPR)
      && TREE_CODE (val) == INTEGER_CST)
    {
      enum tree_code low_code, high_code;
      tree low, high;
      if (is_masked_range_test (name, val, comp_code, &name, &low,
				&low_code, &high, &high_code))
	{
	  if (low_code != ERROR_MARK)
	    register_edge_assert_for_2 (name, e, low_code, name,
					low, /*invert*/false, asserts);
	  if (high_code != ERROR_MARK)
	    register_edge_assert_for_2 (name, e, high_code, name,
					high, /*invert*/false, asserts);
	}
    }
}

/* Finish found ASSERTS for E and register them at GSI.  */

static void
finish_register_edge_assert_for (edge e, gimple_stmt_iterator gsi,
				 vec<assert_info> &asserts)
{
  for (unsigned i = 0; i < asserts.length (); ++i)
    /* Only register an ASSERT_EXPR if NAME was found in the sub-graph
       reachable from E.  */
    if (live_on_edge (e, asserts[i].name))
      register_new_assert_for (asserts[i].name, asserts[i].expr,
			       asserts[i].comp_code, asserts[i].val,
			       NULL, e, gsi);
}



/* Determine whether the outgoing edges of BB should receive an
   ASSERT_EXPR for each of the operands of BB's LAST statement.
   The last statement of BB must be a COND_EXPR.

   If any of the sub-graphs rooted at BB have an interesting use of
   the predicate operands, an assert location node is added to the
   list of assertions for the corresponding operands.  */

static void
find_conditional_asserts (basic_block bb, gcond *last)
{
  gimple_stmt_iterator bsi;
  tree op;
  edge_iterator ei;
  edge e;
  ssa_op_iter iter;

  bsi = gsi_for_stmt (last);

  /* Look for uses of the operands in each of the sub-graphs
     rooted at BB.  We need to check each of the outgoing edges
     separately, so that we know what kind of ASSERT_EXPR to
     insert.  */
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (e->dest == bb)
	continue;

      /* Register the necessary assertions for each operand in the
	 conditional predicate.  */
      auto_vec<assert_info, 8> asserts;
      FOR_EACH_SSA_TREE_OPERAND (op, last, iter, SSA_OP_USE)
	register_edge_assert_for (op, e,
				  gimple_cond_code (last),
				  gimple_cond_lhs (last),
				  gimple_cond_rhs (last), asserts);
      finish_register_edge_assert_for (e, bsi, asserts);
    }
}

struct case_info
{
  tree expr;
  basic_block bb;
};

/* Compare two case labels sorting first by the destination bb index
   and then by the case value.  */

static int
compare_case_labels (const void *p1, const void *p2)
{
  const struct case_info *ci1 = (const struct case_info *) p1;
  const struct case_info *ci2 = (const struct case_info *) p2;
  int idx1 = ci1->bb->index;
  int idx2 = ci2->bb->index;

  if (idx1 < idx2)
    return -1;
  else if (idx1 == idx2)
    {
      /* Make sure the default label is first in a group.  */
      if (!CASE_LOW (ci1->expr))
	return -1;
      else if (!CASE_LOW (ci2->expr))
	return 1;
      else
	return tree_int_cst_compare (CASE_LOW (ci1->expr),
				     CASE_LOW (ci2->expr));
    }
  else
    return 1;
}

/* Determine whether the outgoing edges of BB should receive an
   ASSERT_EXPR for each of the operands of BB's LAST statement.
   The last statement of BB must be a SWITCH_EXPR.

   If any of the sub-graphs rooted at BB have an interesting use of
   the predicate operands, an assert location node is added to the
   list of assertions for the corresponding operands.  */

static void
find_switch_asserts (basic_block bb, gswitch *last)
{
  gimple_stmt_iterator bsi;
  tree op;
  edge e;
  struct case_info *ci;
  size_t n = gimple_switch_num_labels (last);
#if GCC_VERSION >= 4000
  unsigned int idx;
#else
  /* Work around GCC 3.4 bug (PR 37086).  */
  volatile unsigned int idx;
#endif

  bsi = gsi_for_stmt (last);
  op = gimple_switch_index (last);
  if (TREE_CODE (op) != SSA_NAME)
    return;

  /* Build a vector of case labels sorted by destination label.  */
  ci = XNEWVEC (struct case_info, n);
  for (idx = 0; idx < n; ++idx)
    {
      ci[idx].expr = gimple_switch_label (last, idx);
      ci[idx].bb = label_to_block (cfun, CASE_LABEL (ci[idx].expr));
    }
  edge default_edge = find_edge (bb, ci[0].bb);
  qsort (ci, n, sizeof (struct case_info), compare_case_labels);

  for (idx = 0; idx < n; ++idx)
    {
      tree min, max;
      tree cl = ci[idx].expr;
      basic_block cbb = ci[idx].bb;

      min = CASE_LOW (cl);
      max = CASE_HIGH (cl);

      /* If there are multiple case labels with the same destination
	 we need to combine them to a single value range for the edge.  */
      if (idx + 1 < n && cbb == ci[idx + 1].bb)
	{
	  /* Skip labels until the last of the group.  */
	  do {
	    ++idx;
	  } while (idx < n && cbb == ci[idx].bb);
	  --idx;

	  /* Pick up the maximum of the case label range.  */
	  if (CASE_HIGH (ci[idx].expr))
	    max = CASE_HIGH (ci[idx].expr);
	  else
	    max = CASE_LOW (ci[idx].expr);
	}

      /* Can't extract a useful assertion out of a range that includes the
	 default label.  */
      if (min == NULL_TREE)
	continue;

      /* Find the edge to register the assert expr on.  */
      e = find_edge (bb, cbb);

      /* Register the necessary assertions for the operand in the
	 SWITCH_EXPR.  */
      auto_vec<assert_info, 8> asserts;
      register_edge_assert_for (op, e,
				max ? GE_EXPR : EQ_EXPR,
				op, fold_convert (TREE_TYPE (op), min),
				asserts);
      if (max)
	register_edge_assert_for (op, e, LE_EXPR, op,
				  fold_convert (TREE_TYPE (op), max),
				  asserts);
      finish_register_edge_assert_for (e, bsi, asserts);
    }

  XDELETEVEC (ci);

  if (!live_on_edge (default_edge, op))
    return;

  /* Now register along the default label assertions that correspond to the
     anti-range of each label.  */
  int insertion_limit = param_max_vrp_switch_assertions;
  if (insertion_limit == 0)
    return;

  /* We can't do this if the default case shares a label with another case.  */
  tree default_cl = gimple_switch_default_label (last);
  for (idx = 1; idx < n; idx++)
    {
      tree min, max;
      tree cl = gimple_switch_label (last, idx);
      if (CASE_LABEL (cl) == CASE_LABEL (default_cl))
	continue;

      min = CASE_LOW (cl);
      max = CASE_HIGH (cl);

      /* Combine contiguous case ranges to reduce the number of assertions
	 to insert.  */
      for (idx = idx + 1; idx < n; idx++)
	{
	  tree next_min, next_max;
	  tree next_cl = gimple_switch_label (last, idx);
	  if (CASE_LABEL (next_cl) == CASE_LABEL (default_cl))
	    break;

	  next_min = CASE_LOW (next_cl);
	  next_max = CASE_HIGH (next_cl);

	  wide_int difference = (wi::to_wide (next_min)
				 - wi::to_wide (max ? max : min));
	  if (wi::eq_p (difference, 1))
	    max = next_max ? next_max : next_min;
	  else
	    break;
	}
      idx--;

      if (max == NULL_TREE)
	{
	  /* Register the assertion OP != MIN.  */
	  auto_vec<assert_info, 8> asserts;
	  min = fold_convert (TREE_TYPE (op), min);
	  register_edge_assert_for (op, default_edge, NE_EXPR, op, min,
				    asserts);
	  finish_register_edge_assert_for (default_edge, bsi, asserts);
	}
      else
	{
	  /* Register the assertion (unsigned)OP - MIN > (MAX - MIN),
	     which will give OP the anti-range ~[MIN,MAX].  */
	  tree uop = fold_convert (unsigned_type_for (TREE_TYPE (op)), op);
	  min = fold_convert (TREE_TYPE (uop), min);
	  max = fold_convert (TREE_TYPE (uop), max);

	  tree lhs = fold_build2 (MINUS_EXPR, TREE_TYPE (uop), uop, min);
	  tree rhs = int_const_binop (MINUS_EXPR, max, min);
	  register_new_assert_for (op, lhs, GT_EXPR, rhs,
				   NULL, default_edge, bsi);
	}

      if (--insertion_limit == 0)
	break;
    }
}


/* Traverse all the statements in block BB looking for statements that
   may generate useful assertions for the SSA names in their operand.
   If a statement produces a useful assertion A for name N_i, then the
   list of assertions already generated for N_i is scanned to
   determine if A is actually needed.

   If N_i already had the assertion A at a location dominating the
   current location, then nothing needs to be done.  Otherwise, the
   new location for A is recorded instead.

   1- For every statement S in BB, all the variables used by S are
      added to bitmap FOUND_IN_SUBGRAPH.

   2- If statement S uses an operand N in a way that exposes a known
      value range for N, then if N was not already generated by an
      ASSERT_EXPR, create a new assert location for N.  For instance,
      if N is a pointer and the statement dereferences it, we can
      assume that N is not NULL.

   3- COND_EXPRs are a special case of #2.  We can derive range
      information from the predicate but need to insert different
      ASSERT_EXPRs for each of the sub-graphs rooted at the
      conditional block.  If the last statement of BB is a conditional
      expression of the form 'X op Y', then

      a) Remove X and Y from the set FOUND_IN_SUBGRAPH.

      b) If the conditional is the only entry point to the sub-graph
	 corresponding to the THEN_CLAUSE, recurse into it.  On
	 return, if X and/or Y are marked in FOUND_IN_SUBGRAPH, then
	 an ASSERT_EXPR is added for the corresponding variable.

      c) Repeat step (b) on the ELSE_CLAUSE.

      d) Mark X and Y in FOUND_IN_SUBGRAPH.

      For instance,

	    if (a == 9)
	      b = a;
	    else
	      b = c + 1;

      In this case, an assertion on the THEN clause is useful to
      determine that 'a' is always 9 on that edge.  However, an assertion
      on the ELSE clause would be unnecessary.

   4- If BB does not end in a conditional expression, then we recurse
      into BB's dominator children.

   At the end of the recursive traversal, every SSA name will have a
   list of locations where ASSERT_EXPRs should be added.  When a new
   location for name N is found, it is registered by calling
   register_new_assert_for.  That function keeps track of all the
   registered assertions to prevent adding unnecessary assertions.
   For instance, if a pointer P_4 is dereferenced more than once in a
   dominator tree, only the location dominating all the dereference of
   P_4 will receive an ASSERT_EXPR.  */

static void
find_assert_locations_1 (basic_block bb, sbitmap live)
{
  gimple *last;

  last = last_stmt (bb);

  /* If BB's last statement is a conditional statement involving integer
     operands, determine if we need to add ASSERT_EXPRs.  */
  if (last
      && gimple_code (last) == GIMPLE_COND
      && !fp_predicate (last)
      && !ZERO_SSA_OPERANDS (last, SSA_OP_USE))
    find_conditional_asserts (bb, as_a <gcond *> (last));

  /* If BB's last statement is a switch statement involving integer
     operands, determine if we need to add ASSERT_EXPRs.  */
  if (last
      && gimple_code (last) == GIMPLE_SWITCH
      && !ZERO_SSA_OPERANDS (last, SSA_OP_USE))
    find_switch_asserts (bb, as_a <gswitch *> (last));

  /* Traverse all the statements in BB marking used names and looking
     for statements that may infer assertions for their used operands.  */
  for (gimple_stmt_iterator si = gsi_last_bb (bb); !gsi_end_p (si);
       gsi_prev (&si))
    {
      gimple *stmt;
      tree op;
      ssa_op_iter i;

      stmt = gsi_stmt (si);

      if (is_gimple_debug (stmt))
	continue;

      /* See if we can derive an assertion for any of STMT's operands.  */
      FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_USE)
	{
	  tree value;
	  enum tree_code comp_code;

	  /* If op is not live beyond this stmt, do not bother to insert
	     asserts for it.  */
	  if (!bitmap_bit_p (live, SSA_NAME_VERSION (op)))
	    continue;

	  /* If OP is used in such a way that we can infer a value
	     range for it, and we don't find a previous assertion for
	     it, create a new assertion location node for OP.  */
	  if (infer_value_range (stmt, op, &comp_code, &value))
	    {
	      /* If we are able to infer a nonzero value range for OP,
		 then walk backwards through the use-def chain to see if OP
		 was set via a typecast.

		 If so, then we can also infer a nonzero value range
		 for the operand of the NOP_EXPR.  */
	      if (comp_code == NE_EXPR && integer_zerop (value))
		{
		  tree t = op;
		  gimple *def_stmt = SSA_NAME_DEF_STMT (t);

		  while (is_gimple_assign (def_stmt)
			 && CONVERT_EXPR_CODE_P
			     (gimple_assign_rhs_code (def_stmt))
			 && TREE_CODE
			     (gimple_assign_rhs1 (def_stmt)) == SSA_NAME
			 && POINTER_TYPE_P
			     (TREE_TYPE (gimple_assign_rhs1 (def_stmt))))
		    {
		      t = gimple_assign_rhs1 (def_stmt);
		      def_stmt = SSA_NAME_DEF_STMT (t);

		      /* Note we want to register the assert for the
			 operand of the NOP_EXPR after SI, not after the
			 conversion.  */
		      if (bitmap_bit_p (live, SSA_NAME_VERSION (t)))
			register_new_assert_for (t, t, comp_code, value,
						 bb, NULL, si);
		    }
		}

	      register_new_assert_for (op, op, comp_code, value, bb, NULL, si);
	    }
	}

      /* Update live.  */
      FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_USE)
	bitmap_set_bit (live, SSA_NAME_VERSION (op));
      FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_DEF)
	bitmap_clear_bit (live, SSA_NAME_VERSION (op));
    }

  /* Traverse all PHI nodes in BB, updating live.  */
  for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
       gsi_next (&si))
    {
      use_operand_p arg_p;
      ssa_op_iter i;
      gphi *phi = si.phi ();
      tree res = gimple_phi_result (phi);

      if (virtual_operand_p (res))
	continue;

      FOR_EACH_PHI_ARG (arg_p, phi, i, SSA_OP_USE)
	{
	  tree arg = USE_FROM_PTR (arg_p);
	  if (TREE_CODE (arg) == SSA_NAME)
	    bitmap_set_bit (live, SSA_NAME_VERSION (arg));
	}

      bitmap_clear_bit (live, SSA_NAME_VERSION (res));
    }
}

/* Do an RPO walk over the function computing SSA name liveness
   on-the-fly and deciding on assert expressions to insert.  */

static void
find_assert_locations (void)
{
  int *rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));
  int *bb_rpo = XNEWVEC (int, last_basic_block_for_fn (cfun));
  int *last_rpo = XCNEWVEC (int, last_basic_block_for_fn (cfun));
  int rpo_cnt, i;

  live = XCNEWVEC (sbitmap, last_basic_block_for_fn (cfun));
  rpo_cnt = pre_and_rev_post_order_compute (NULL, rpo, false);
  for (i = 0; i < rpo_cnt; ++i)
    bb_rpo[rpo[i]] = i;

  /* Pre-seed loop latch liveness from loop header PHI nodes.  Due to
     the order we compute liveness and insert asserts we otherwise
     fail to insert asserts into the loop latch.  */
  loop_p loop;
  FOR_EACH_LOOP (loop, 0)
    {
      i = loop->latch->index;
      unsigned int j = single_succ_edge (loop->latch)->dest_idx;
      for (gphi_iterator gsi = gsi_start_phis (loop->header);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gphi *phi = gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (phi)))
	    continue;
	  tree arg = gimple_phi_arg_def (phi, j);
	  if (TREE_CODE (arg) == SSA_NAME)
	    {
	      if (live[i] == NULL)
		{
		  live[i] = sbitmap_alloc (num_ssa_names);
		  bitmap_clear (live[i]);
		}
	      bitmap_set_bit (live[i], SSA_NAME_VERSION (arg));
	    }
	}
    }

  for (i = rpo_cnt - 1; i >= 0; --i)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, rpo[i]);
      edge e;
      edge_iterator ei;

      if (!live[rpo[i]])
	{
	  live[rpo[i]] = sbitmap_alloc (num_ssa_names);
	  bitmap_clear (live[rpo[i]]);
	}

      /* Process BB and update the live information with uses in
         this block.  */
      find_assert_locations_1 (bb, live[rpo[i]]);

      /* Merge liveness into the predecessor blocks and free it.  */
      if (!bitmap_empty_p (live[rpo[i]]))
	{
	  int pred_rpo = i;
	  FOR_EACH_EDGE (e, ei, bb->preds)
	    {
	      int pred = e->src->index;
	      if ((e->flags & EDGE_DFS_BACK) || pred == ENTRY_BLOCK)
		continue;

	      if (!live[pred])
		{
		  live[pred] = sbitmap_alloc (num_ssa_names);
		  bitmap_clear (live[pred]);
		}
	      bitmap_ior (live[pred], live[pred], live[rpo[i]]);

	      if (bb_rpo[pred] < pred_rpo)
		pred_rpo = bb_rpo[pred];
	    }

	  /* Record the RPO number of the last visited block that needs
	     live information from this block.  */
	  last_rpo[rpo[i]] = pred_rpo;
	}
      else
	{
	  sbitmap_free (live[rpo[i]]);
	  live[rpo[i]] = NULL;
	}

      /* We can free all successors live bitmaps if all their
         predecessors have been visited already.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (last_rpo[e->dest->index] == i
	    && live[e->dest->index])
	  {
	    sbitmap_free (live[e->dest->index]);
	    live[e->dest->index] = NULL;
	  }
    }

  XDELETEVEC (rpo);
  XDELETEVEC (bb_rpo);
  XDELETEVEC (last_rpo);
  for (i = 0; i < last_basic_block_for_fn (cfun); ++i)
    if (live[i])
      sbitmap_free (live[i]);
  XDELETEVEC (live);
}

/* Create an ASSERT_EXPR for NAME and insert it in the location
   indicated by LOC.  Return true if we made any edge insertions.  */

static bool
process_assert_insertions_for (tree name, assert_locus *loc)
{
  /* Build the comparison expression NAME_i COMP_CODE VAL.  */
  gimple *stmt;
  tree cond;
  gimple *assert_stmt;
  edge_iterator ei;
  edge e;

  /* If we have X <=> X do not insert an assert expr for that.  */
  if (loc->expr == loc->val)
    return false;

  cond = build2 (loc->comp_code, boolean_type_node, loc->expr, loc->val);
  assert_stmt = build_assert_expr_for (cond, name);
  if (loc->e)
    {
      /* We have been asked to insert the assertion on an edge.  This
	 is used only by COND_EXPR and SWITCH_EXPR assertions.  */
      gcc_checking_assert (gimple_code (gsi_stmt (loc->si)) == GIMPLE_COND
			   || (gimple_code (gsi_stmt (loc->si))
			       == GIMPLE_SWITCH));

      gsi_insert_on_edge (loc->e, assert_stmt);
      return true;
    }

  /* If the stmt iterator points at the end then this is an insertion
     at the beginning of a block.  */
  if (gsi_end_p (loc->si))
    {
      gimple_stmt_iterator si = gsi_after_labels (loc->bb);
      gsi_insert_before (&si, assert_stmt, GSI_SAME_STMT);
      return false;

    }
  /* Otherwise, we can insert right after LOC->SI iff the
     statement must not be the last statement in the block.  */
  stmt = gsi_stmt (loc->si);
  if (!stmt_ends_bb_p (stmt))
    {
      gsi_insert_after (&loc->si, assert_stmt, GSI_SAME_STMT);
      return false;
    }

  /* If STMT must be the last statement in BB, we can only insert new
     assertions on the non-abnormal edge out of BB.  Note that since
     STMT is not control flow, there may only be one non-abnormal/eh edge
     out of BB.  */
  FOR_EACH_EDGE (e, ei, loc->bb->succs)
    if (!(e->flags & (EDGE_ABNORMAL|EDGE_EH)))
      {
	gsi_insert_on_edge (e, assert_stmt);
	return true;
      }

  gcc_unreachable ();
}

/* Qsort helper for sorting assert locations.  If stable is true, don't
   use iterative_hash_expr because it can be unstable for -fcompare-debug,
   on the other side some pointers might be NULL.  */

template <bool stable>
static int
compare_assert_loc (const void *pa, const void *pb)
{
  assert_locus * const a = *(assert_locus * const *)pa;
  assert_locus * const b = *(assert_locus * const *)pb;

  /* If stable, some asserts might be optimized away already, sort
     them last.  */
  if (stable)
    {
      if (a == NULL)
	return b != NULL;
      else if (b == NULL)
	return -1;
    }

  if (a->e == NULL && b->e != NULL)
    return 1;
  else if (a->e != NULL && b->e == NULL)
    return -1;

  /* After the above checks, we know that (a->e == NULL) == (b->e == NULL),
     no need to test both a->e and b->e.  */

  /* Sort after destination index.  */
  if (a->e == NULL)
    ;
  else if (a->e->dest->index > b->e->dest->index)
    return 1;
  else if (a->e->dest->index < b->e->dest->index)
    return -1;

  /* Sort after comp_code.  */
  if (a->comp_code > b->comp_code)
    return 1;
  else if (a->comp_code < b->comp_code)
    return -1;

  hashval_t ha, hb;

  /* E.g. if a->val is ADDR_EXPR of a VAR_DECL, iterative_hash_expr
     uses DECL_UID of the VAR_DECL, so sorting might differ between
     -g and -g0.  When doing the removal of redundant assert exprs
     and commonization to successors, this does not matter, but for
     the final sort needs to be stable.  */
  if (stable)
    {
      ha = 0;
      hb = 0;
    }
  else
    {
      ha = iterative_hash_expr (a->expr, iterative_hash_expr (a->val, 0));
      hb = iterative_hash_expr (b->expr, iterative_hash_expr (b->val, 0));
    }

  /* Break the tie using hashing and source/bb index.  */
  if (ha == hb)
    return (a->e != NULL
	    ? a->e->src->index - b->e->src->index
	    : a->bb->index - b->bb->index);
  return ha > hb ? 1 : -1;
}

/* Process all the insertions registered for every name N_i registered
   in NEED_ASSERT_FOR.  The list of assertions to be inserted are
   found in ASSERTS_FOR[i].  */

static void
process_assert_insertions (void)
{
  unsigned i;
  bitmap_iterator bi;
  bool update_edges_p = false;
  int num_asserts = 0;

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_all_asserts (dump_file);

  EXECUTE_IF_SET_IN_BITMAP (need_assert_for, 0, i, bi)
    {
      assert_locus *loc = asserts_for[i];
      gcc_assert (loc);

      auto_vec<assert_locus *, 16> asserts;
      for (; loc; loc = loc->next)
	asserts.safe_push (loc);
      asserts.qsort (compare_assert_loc<false>);

      /* Push down common asserts to successors and remove redundant ones.  */
      unsigned ecnt = 0;
      assert_locus *common = NULL;
      unsigned commonj = 0;
      for (unsigned j = 0; j < asserts.length (); ++j)
	{
	  loc = asserts[j];
	  if (! loc->e)
	    common = NULL;
	  else if (! common
		   || loc->e->dest != common->e->dest
		   || loc->comp_code != common->comp_code
		   || ! operand_equal_p (loc->val, common->val, 0)
		   || ! operand_equal_p (loc->expr, common->expr, 0))
	    {
	      commonj = j;
	      common = loc;
	      ecnt = 1;
	    }
	  else if (loc->e == asserts[j-1]->e)
	    {
	      /* Remove duplicate asserts.  */
	      if (commonj == j - 1)
		{
		  commonj = j;
		  common = loc;
		}
	      free (asserts[j-1]);
	      asserts[j-1] = NULL;
	    }
	  else
	    {
	      ecnt++;
	      if (EDGE_COUNT (common->e->dest->preds) == ecnt)
		{
		  /* We have the same assertion on all incoming edges of a BB.
		     Insert it at the beginning of that block.  */
		  loc->bb = loc->e->dest;
		  loc->e = NULL;
		  loc->si = gsi_none ();
		  common = NULL;
		  /* Clear asserts commoned.  */
		  for (; commonj != j; ++commonj)
		    if (asserts[commonj])
		      {
			free (asserts[commonj]);
			asserts[commonj] = NULL;
		      }
		}
	    }
	}

      /* The asserts vector sorting above might be unstable for
	 -fcompare-debug, sort again to ensure a stable sort.  */
      asserts.qsort (compare_assert_loc<true>);
      for (unsigned j = 0; j < asserts.length (); ++j)
	{
	  loc = asserts[j];
	  if (! loc)
	    break;
	  update_edges_p |= process_assert_insertions_for (ssa_name (i), loc);
	  num_asserts++;
	  free (loc);
	}
    }

  if (update_edges_p)
    gsi_commit_edge_inserts ();

  statistics_counter_event (cfun, "Number of ASSERT_EXPR expressions inserted",
			    num_asserts);
}


/* Traverse the flowgraph looking for conditional jumps to insert range
   expressions.  These range expressions are meant to provide information
   to optimizations that need to reason in terms of value ranges.  They
   will not be expanded into RTL.  For instance, given:

   x = ...
   y = ...
   if (x < y)
     y = x - 2;
   else
     x = y + 3;

   this pass will transform the code into:

   x = ...
   y = ...
   if (x < y)
    {
      x = ASSERT_EXPR <x, x < y>
      y = x - 2
    }
   else
    {
      y = ASSERT_EXPR <y, x >= y>
      x = y + 3
    }

   The idea is that once copy and constant propagation have run, other
   optimizations will be able to determine what ranges of values can 'x'
   take in different paths of the code, simply by checking the reaching
   definition of 'x'.  */

static void
insert_range_assertions (void)
{
  need_assert_for = BITMAP_ALLOC (NULL);
  asserts_for = XCNEWVEC (assert_locus *, num_ssa_names);

  calculate_dominance_info (CDI_DOMINATORS);

  find_assert_locations ();
  if (!bitmap_empty_p (need_assert_for))
    {
      process_assert_insertions ();
      update_ssa (TODO_update_ssa_no_phi);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nSSA form after inserting ASSERT_EXPRs\n");
      dump_function_to_file (current_function_decl, dump_file, dump_flags);
    }

  free (asserts_for);
  BITMAP_FREE (need_assert_for);
}

class vrp_prop : public ssa_propagation_engine
{
 public:
  enum ssa_prop_result visit_stmt (gimple *, edge *, tree *) FINAL OVERRIDE;
  enum ssa_prop_result visit_phi (gphi *) FINAL OVERRIDE;

  void vrp_initialize (void);
  void vrp_finalize (bool);
  void check_all_array_refs (void);
  bool check_array_ref (location_t, tree, bool);
  bool check_mem_ref (location_t, tree, bool);
  void search_for_addr_array (tree, location_t);

  class vr_values vr_values;
  /* Temporary delegator to minimize code churn.  */
  const value_range_equiv *get_value_range (const_tree op)
    { return vr_values.get_value_range (op); }
  void set_def_to_varying (const_tree def)
    { vr_values.set_def_to_varying (def); }
  void set_defs_to_varying (gimple *stmt)
    { vr_values.set_defs_to_varying (stmt); }
  void extract_range_from_stmt (gimple *stmt, edge *taken_edge_p,
				tree *output_p, value_range_equiv *vr)
    { vr_values.extract_range_from_stmt (stmt, taken_edge_p, output_p, vr); }
  bool update_value_range (const_tree op, value_range_equiv *vr)
    { return vr_values.update_value_range (op, vr); }
  void extract_range_basic (value_range_equiv *vr, gimple *stmt)
    { vr_values.extract_range_basic (vr, stmt); }
  void extract_range_from_phi_node (gphi *phi, value_range_equiv *vr)
    { vr_values.extract_range_from_phi_node (phi, vr); }
};
/* Checks one ARRAY_REF in REF, located at LOCUS. Ignores flexible arrays
   and "struct" hacks. If VRP can determine that the
   array subscript is a constant, check if it is outside valid
   range. If the array subscript is a RANGE, warn if it is
   non-overlapping with valid range.
   IGNORE_OFF_BY_ONE is true if the ARRAY_REF is inside a ADDR_EXPR.
   Returns true if a warning has been issued.  */

bool
vrp_prop::check_array_ref (location_t location, tree ref,
			   bool ignore_off_by_one)
{
  if (TREE_NO_WARNING (ref))
    return false;

  tree low_sub = TREE_OPERAND (ref, 1);
  tree up_sub = low_sub;
  tree up_bound = array_ref_up_bound (ref);

  /* Referenced decl if one can be determined.  */
  tree decl = NULL_TREE;

  /* Set for accesses to interior zero-length arrays.  */
  bool interior_zero_len = false;

  tree up_bound_p1;

  if (!up_bound
      || TREE_CODE (up_bound) != INTEGER_CST
      || (warn_array_bounds < 2
	  && array_at_struct_end_p (ref)))
    {
      /* Accesses to trailing arrays via pointers may access storage
	 beyond the types array bounds.  For such arrays, or for flexible
	 array members, as well as for other arrays of an unknown size,
	 replace the upper bound with a more permissive one that assumes
	 the size of the largest object is PTRDIFF_MAX.  */
      tree eltsize = array_ref_element_size (ref);

      if (TREE_CODE (eltsize) != INTEGER_CST
	  || integer_zerop (eltsize))
	{
	  up_bound = NULL_TREE;
	  up_bound_p1 = NULL_TREE;
	}
      else
	{
	  tree ptrdiff_max = TYPE_MAX_VALUE (ptrdiff_type_node);
	  tree maxbound = ptrdiff_max;
	  tree arg = TREE_OPERAND (ref, 0);

	  const bool compref = TREE_CODE (arg) == COMPONENT_REF;
	  if (compref)
	    {
	      /* Try to determine the size of the trailing array from
		 its initializer (if it has one).  */
	      if (tree refsize = component_ref_size (arg, &interior_zero_len))
		if (TREE_CODE (refsize) == INTEGER_CST)
		  maxbound = refsize;
	    }

	  if (maxbound == ptrdiff_max)
	    {
	      /* Try to determine the size of the base object.  Avoid
		 COMPONENT_REF already tried above.  Using its DECL_SIZE
		 size wouldn't necessarily be correct if the reference is
		 to its flexible array member initialized in a different
		 translation unit.  */
	      poly_int64 off;
	      if (tree base = get_addr_base_and_unit_offset (arg, &off))
		{
		  if (!compref && DECL_P (base))
		    if (tree basesize = DECL_SIZE_UNIT (base))
		      if (TREE_CODE (basesize) == INTEGER_CST)
			{
			  maxbound = basesize;
			  decl = base;
			}

		  if (known_gt (off, 0))
		    maxbound = wide_int_to_tree (sizetype,
						 wi::sub (wi::to_wide (maxbound),
							  off));
		}
	    }
	  else
	    maxbound = fold_convert (sizetype, maxbound);

	  up_bound_p1 = int_const_binop (TRUNC_DIV_EXPR, maxbound, eltsize);

	  if (up_bound_p1 != NULL_TREE)
	    up_bound = int_const_binop (MINUS_EXPR, up_bound_p1,
					build_int_cst (ptrdiff_type_node, 1));
	  else
	    up_bound = NULL_TREE;
	}
    }
  else
    up_bound_p1 = int_const_binop (PLUS_EXPR, up_bound,
				   build_int_cst (TREE_TYPE (up_bound), 1));

  tree low_bound = array_ref_low_bound (ref);

  tree artype = TREE_TYPE (TREE_OPERAND (ref, 0));

  bool warned = false;

  /* Empty array.  */
  if (up_bound && tree_int_cst_equal (low_bound, up_bound_p1))
    warned = warning_at (location, OPT_Warray_bounds,
			 "array subscript %E is outside array bounds of %qT",
			 low_sub, artype);

  const value_range_equiv *vr = NULL;
  if (TREE_CODE (low_sub) == SSA_NAME)
    {
      vr = get_value_range (low_sub);
      if (!vr->undefined_p () && !vr->varying_p ())
        {
          low_sub = vr->kind () == VR_RANGE ? vr->max () : vr->min ();
          up_sub = vr->kind () == VR_RANGE ? vr->min () : vr->max ();
        }
    }

  if (warned)
    ; /* Do nothing.  */
  else if (vr && vr->kind () == VR_ANTI_RANGE)
    {
      if (up_bound
	  && TREE_CODE (up_sub) == INTEGER_CST
          && (ignore_off_by_one
	      ? tree_int_cst_lt (up_bound, up_sub)
	      : tree_int_cst_le (up_bound, up_sub))
          && TREE_CODE (low_sub) == INTEGER_CST
          && tree_int_cst_le (low_sub, low_bound))
	warned = warning_at (location, OPT_Warray_bounds,
			     "array subscript [%E, %E] is outside "
			     "array bounds of %qT",
			     low_sub, up_sub, artype);
    }
  else if (up_bound
	   && TREE_CODE (up_sub) == INTEGER_CST
	   && (ignore_off_by_one
	       ? !tree_int_cst_le (up_sub, up_bound_p1)
	       : !tree_int_cst_le (up_sub, up_bound)))
    warned = warning_at (location, OPT_Warray_bounds,
			 "array subscript %E is above array bounds of %qT",
			 up_sub, artype);
  else if (TREE_CODE (low_sub) == INTEGER_CST
           && tree_int_cst_lt (low_sub, low_bound))
    warned = warning_at (location, OPT_Warray_bounds,
			 "array subscript %E is below array bounds of %qT",
			 low_sub, artype);

  if (!warned && interior_zero_len)
    warned = warning_at (location, OPT_Wzero_length_bounds,
			 (TREE_CODE (low_sub) == INTEGER_CST
			  ? G_("array subscript %E is outside the bounds "
			       "of an interior zero-length array %qT")
			  : G_("array subscript %qE is outside the bounds "
			       "of an interior zero-length array %qT")),
			 low_sub, artype);

  if (warned)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, ref);
	  fprintf (dump_file, "\n");
	}

      ref = decl ? decl : TREE_OPERAND (ref, 0);

      tree rec = NULL_TREE;
      if (TREE_CODE (ref) == COMPONENT_REF)
	{
	  /* For a reference to a member of a struct object also mention
	     the object if it's known.  It may be defined in a different
	     function than the out-of-bounds access.  */
	  rec = TREE_OPERAND (ref, 0);
	  if (!VAR_P (rec))
	    rec = NULL_TREE;
	  ref = TREE_OPERAND (ref, 1);
	}

      if (DECL_P (ref))
	inform (DECL_SOURCE_LOCATION (ref), "while referencing %qD", ref);
      if (rec && DECL_P (rec))
	inform (DECL_SOURCE_LOCATION (rec), "defined here %qD", rec);

      TREE_NO_WARNING (ref) = 1;
    }

  return warned;
}

/* Checks one MEM_REF in REF, located at LOCATION, for out-of-bounds
   references to string constants.  If VRP can determine that the array
   subscript is a constant, check if it is outside valid range.
   If the array subscript is a RANGE, warn if it is non-overlapping
   with valid range.
   IGNORE_OFF_BY_ONE is true if the MEM_REF is inside an ADDR_EXPR
   (used to allow one-past-the-end indices for code that takes
   the address of the just-past-the-end element of an array).
   Returns true if a warning has been issued.  */

bool
vrp_prop::check_mem_ref (location_t location, tree ref,
			 bool ignore_off_by_one)
{
  if (TREE_NO_WARNING (ref))
    return false;

  tree arg = TREE_OPERAND (ref, 0);
  /* The constant and variable offset of the reference.  */
  tree cstoff = TREE_OPERAND (ref, 1);
  tree varoff = NULL_TREE;

  const offset_int maxobjsize = tree_to_shwi (max_object_size ());

  /* The array or string constant bounds in bytes.  Initially set
     to [-MAXOBJSIZE - 1, MAXOBJSIZE]  until a tighter bound is
     determined.  */
  offset_int arrbounds[2] = { -maxobjsize - 1, maxobjsize };

  /* The minimum and maximum intermediate offset.  For a reference
     to be valid, not only does the final offset/subscript must be
     in bounds but all intermediate offsets should be as well.
     GCC may be able to deal gracefully with such out-of-bounds
     offsets so the checking is only enbaled at -Warray-bounds=2
     where it may help detect bugs in uses of the intermediate
     offsets that could otherwise not be detectable.  */
  offset_int ioff = wi::to_offset (fold_convert (ptrdiff_type_node, cstoff));
  offset_int extrema[2] = { 0, wi::abs (ioff) };

  /* The range of the byte offset into the reference.  */
  offset_int offrange[2] = { 0, 0 };

  const value_range_equiv *vr = NULL;

  /* Determine the offsets and increment OFFRANGE for the bounds of each.
     The loop computes the range of the final offset for expressions such
     as (A + i0 + ... + iN)[CSTOFF] where i0 through iN are SSA_NAMEs in
     some range.  */
  const unsigned limit = param_ssa_name_def_chain_limit;
  for (unsigned n = 0; TREE_CODE (arg) == SSA_NAME && n < limit; ++n)
    {
      gimple *def = SSA_NAME_DEF_STMT (arg);
      if (!is_gimple_assign (def))
	break;

      tree_code code = gimple_assign_rhs_code (def);
      if (code == POINTER_PLUS_EXPR)
	{
	  arg = gimple_assign_rhs1 (def);
	  varoff = gimple_assign_rhs2 (def);
	}
      else if (code == ASSERT_EXPR)
	{
	  arg = TREE_OPERAND (gimple_assign_rhs1 (def), 0);
	  continue;
	}
      else
	return false;

      /* VAROFF should always be a SSA_NAME here (and not even
	 INTEGER_CST) but there's no point in taking chances.  */
      if (TREE_CODE (varoff) != SSA_NAME)
	break;

      vr = get_value_range (varoff);
      if (!vr || vr->undefined_p () || vr->varying_p ())
	break;

      if (!vr->constant_p ())
        break;

      if (vr->kind () == VR_RANGE)
	{
	  offset_int min
	    = wi::to_offset (fold_convert (ptrdiff_type_node, vr->min ()));
	  offset_int max
	    = wi::to_offset (fold_convert (ptrdiff_type_node, vr->max ()));
	  if (min < max)
	    {
	      offrange[0] += min;
	      offrange[1] += max;
	    }
	  else
	    {
	      /* When MIN >= MAX, the offset is effectively in a union
		 of two ranges: [-MAXOBJSIZE -1, MAX] and [MIN, MAXOBJSIZE].
		 Since there is no way to represent such a range across
		 additions, conservatively add [-MAXOBJSIZE -1, MAXOBJSIZE]
		 to OFFRANGE.  */
	      offrange[0] += arrbounds[0];
	      offrange[1] += arrbounds[1];
	    }
	}
      else
	{
	  /* For an anti-range, analogously to the above, conservatively
	     add [-MAXOBJSIZE -1, MAXOBJSIZE] to OFFRANGE.  */
	  offrange[0] += arrbounds[0];
	  offrange[1] += arrbounds[1];
	}

      /* Keep track of the minimum and maximum offset.  */
      if (offrange[1] < 0 && offrange[1] < extrema[0])
	extrema[0] = offrange[1];
      if (offrange[0] > 0 && offrange[0] > extrema[1])
	extrema[1] = offrange[0];

      if (offrange[0] < arrbounds[0])
	offrange[0] = arrbounds[0];

      if (offrange[1] > arrbounds[1])
	offrange[1] = arrbounds[1];
    }

  if (TREE_CODE (arg) == ADDR_EXPR)
    {
      arg = TREE_OPERAND (arg, 0);
      if (TREE_CODE (arg) != STRING_CST
	  && TREE_CODE (arg) != PARM_DECL
	  && TREE_CODE (arg) != VAR_DECL)
	return false;
    }
  else
    return false;

  /* The type of the object being referred to.  It can be an array,
     string literal, or a non-array type when the MEM_REF represents
     a reference/subscript via a pointer to an object that is not
     an element of an array.  Incomplete types are excluded as well
     because their size is not known.  */
  tree reftype = TREE_TYPE (arg);
  if (POINTER_TYPE_P (reftype)
      || !COMPLETE_TYPE_P (reftype)
      || TREE_CODE (TYPE_SIZE_UNIT (reftype)) != INTEGER_CST)
    return false;

  /* Except in declared objects, references to trailing array members
     of structs and union objects are excluded because MEM_REF doesn't
     make it possible to identify the member where the reference
     originated.  */
  if (RECORD_OR_UNION_TYPE_P (reftype)
      && (!VAR_P (arg)
	  || (DECL_EXTERNAL (arg) && array_at_struct_end_p (ref))))
    return false;

  arrbounds[0] = 0;

  offset_int eltsize;
  if (TREE_CODE (reftype) == ARRAY_TYPE)
    {
      eltsize = wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (reftype)));
      if (tree dom = TYPE_DOMAIN (reftype))
	{
	  tree bnds[] = { TYPE_MIN_VALUE (dom), TYPE_MAX_VALUE (dom) };
	  if (TREE_CODE (arg) == COMPONENT_REF)
	    {
	      offset_int size = maxobjsize;
	      if (tree fldsize = component_ref_size (arg))
		size = wi::to_offset (fldsize);
	      arrbounds[1] = wi::lrshift (size, wi::floor_log2 (eltsize));
	    }
	  else if (array_at_struct_end_p (arg) || !bnds[0] || !bnds[1])
	    arrbounds[1] = wi::lrshift (maxobjsize, wi::floor_log2 (eltsize));
	  else
	    arrbounds[1] = (wi::to_offset (bnds[1]) - wi::to_offset (bnds[0])
			    + 1) * eltsize;
	}
      else
	arrbounds[1] = wi::lrshift (maxobjsize, wi::floor_log2 (eltsize));

      if (TREE_CODE (ref) == MEM_REF)
	{
	  /* For MEM_REF determine a tighter bound of the non-array
	     element type.  */
	  tree eltype = TREE_TYPE (reftype);
	  while (TREE_CODE (eltype) == ARRAY_TYPE)
	    eltype = TREE_TYPE (eltype);
	  eltsize = wi::to_offset (TYPE_SIZE_UNIT (eltype));
	}
    }
  else
    {
      eltsize = 1;
      tree size = TYPE_SIZE_UNIT (reftype);
      if (VAR_P (arg))
	if (tree initsize = DECL_SIZE_UNIT (arg))
	  if (tree_int_cst_lt (size, initsize))
	    size = initsize;

      arrbounds[1] = wi::to_offset (size);
    }

  offrange[0] += ioff;
  offrange[1] += ioff;

  /* Compute the more permissive upper bound when IGNORE_OFF_BY_ONE
     is set (when taking the address of the one-past-last element
     of an array) but always use the stricter bound in diagnostics. */
  offset_int ubound = arrbounds[1];
  if (ignore_off_by_one)
    ubound += 1;

  if (arrbounds[0] == arrbounds[1]
      || offrange[0] >= ubound
      || offrange[1] < arrbounds[0])
    {
      /* Treat a reference to a non-array object as one to an array
	 of a single element.  */
      if (TREE_CODE (reftype) != ARRAY_TYPE)
	reftype = build_array_type_nelts (reftype, 1);

      if (TREE_CODE (ref) == MEM_REF)
	{
	  /* Extract the element type out of MEM_REF and use its size
	     to compute the index to print in the diagnostic; arrays
	     in MEM_REF don't mean anything.  A type with no size like
	     void is as good as having a size of 1.  */
	  tree type = TREE_TYPE (ref);
	  while (TREE_CODE (type) == ARRAY_TYPE)
	    type = TREE_TYPE (type);
	  if (tree size = TYPE_SIZE_UNIT (type))
	    {
	      offrange[0] = offrange[0] / wi::to_offset (size);
	      offrange[1] = offrange[1] / wi::to_offset (size);
	    }
	}
      else
	{
	  /* For anything other than MEM_REF, compute the index to
	     print in the diagnostic as the offset over element size.  */
	  offrange[0] = offrange[0] / eltsize;
	  offrange[1] = offrange[1] / eltsize;
	}

      bool warned;
      if (offrange[0] == offrange[1])
	warned = warning_at (location, OPT_Warray_bounds,
			     "array subscript %wi is outside array bounds "
			     "of %qT",
			     offrange[0].to_shwi (), reftype);
      else
	warned = warning_at (location, OPT_Warray_bounds,
			     "array subscript [%wi, %wi] is outside "
			     "array bounds of %qT",
			     offrange[0].to_shwi (),
			     offrange[1].to_shwi (), reftype);
      if (warned && DECL_P (arg))
	inform (DECL_SOURCE_LOCATION (arg), "while referencing %qD", arg);

      if (warned)
	TREE_NO_WARNING (ref) = 1;
      return warned;
    }

  if (warn_array_bounds < 2)
    return false;

  /* At level 2 check also intermediate offsets.  */
  int i = 0;
  if (extrema[i] < -arrbounds[1] || extrema[i = 1] > ubound)
    {
      HOST_WIDE_INT tmpidx = extrema[i].to_shwi () / eltsize.to_shwi ();

      if (warning_at (location, OPT_Warray_bounds,
		      "intermediate array offset %wi is outside array bounds "
		      "of %qT", tmpidx, reftype))
	{
	  TREE_NO_WARNING (ref) = 1;
	  return true;
	}
    }

  return false;
}

/* Searches if the expr T, located at LOCATION computes
   address of an ARRAY_REF, and call check_array_ref on it.  */

void
vrp_prop::search_for_addr_array (tree t, location_t location)
{
  /* Check each ARRAY_REF and MEM_REF in the reference chain. */
  do
    {
      bool warned = false;
      if (TREE_CODE (t) == ARRAY_REF)
	warned = check_array_ref (location, t, true /*ignore_off_by_one*/);
      else if (TREE_CODE (t) == MEM_REF)
	warned = check_mem_ref (location, t, true /*ignore_off_by_one*/);

      if (warned)
	TREE_NO_WARNING (t) = true;

      t = TREE_OPERAND (t, 0);
    }
  while (handled_component_p (t) || TREE_CODE (t) == MEM_REF);

  if (TREE_CODE (t) != MEM_REF
      || TREE_CODE (TREE_OPERAND (t, 0)) != ADDR_EXPR
      || TREE_NO_WARNING (t))
    return;

  tree tem = TREE_OPERAND (TREE_OPERAND (t, 0), 0);
  tree low_bound, up_bound, el_sz;
  if (TREE_CODE (TREE_TYPE (tem)) != ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (TREE_TYPE (tem))) == ARRAY_TYPE
      || !TYPE_DOMAIN (TREE_TYPE (tem)))
    return;

  low_bound = TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (tem)));
  up_bound = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (tem)));
  el_sz = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (tem)));
  if (!low_bound
      || TREE_CODE (low_bound) != INTEGER_CST
      || !up_bound
      || TREE_CODE (up_bound) != INTEGER_CST
      || !el_sz
      || TREE_CODE (el_sz) != INTEGER_CST)
    return;

  offset_int idx;
  if (!mem_ref_offset (t).is_constant (&idx))
    return;

  bool warned = false;
  idx = wi::sdiv_trunc (idx, wi::to_offset (el_sz));
  if (idx < 0)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, t);
	  fprintf (dump_file, "\n");
	}
      warned = warning_at (location, OPT_Warray_bounds,
			   "array subscript %wi is below "
			   "array bounds of %qT",
			   idx.to_shwi (), TREE_TYPE (tem));
    }
  else if (idx > (wi::to_offset (up_bound)
		  - wi::to_offset (low_bound) + 1))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Array bound warning for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, t);
	  fprintf (dump_file, "\n");
	}
      warned = warning_at (location, OPT_Warray_bounds,
			   "array subscript %wu is above "
			   "array bounds of %qT",
			   idx.to_uhwi (), TREE_TYPE (tem));
    }

  if (warned)
    {
      if (DECL_P (t))
	inform (DECL_SOURCE_LOCATION (t), "while referencing %qD", t);

      TREE_NO_WARNING (t) = 1;
    }
}

/* walk_tree() callback that checks if *TP is
   an ARRAY_REF inside an ADDR_EXPR (in which an array
   subscript one outside the valid range is allowed). Call
   check_array_ref for each ARRAY_REF found. The location is
   passed in DATA.  */

static tree
check_array_bounds (tree *tp, int *walk_subtree, void *data)
{
  tree t = *tp;
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  location_t location;

  if (EXPR_HAS_LOCATION (t))
    location = EXPR_LOCATION (t);
  else
    location = gimple_location (wi->stmt);

  *walk_subtree = TRUE;

  bool warned = false;
  vrp_prop *vrp_prop = (class vrp_prop *)wi->info;
  if (TREE_CODE (t) == ARRAY_REF)
    warned = vrp_prop->check_array_ref (location, t, false/*ignore_off_by_one*/);
  else if (TREE_CODE (t) == MEM_REF)
    warned = vrp_prop->check_mem_ref (location, t, false /*ignore_off_by_one*/);
  else if (TREE_CODE (t) == ADDR_EXPR)
    {
      vrp_prop->search_for_addr_array (t, location);
      *walk_subtree = FALSE;
    }
  /* Propagate the no-warning bit to the outer expression.  */
  if (warned)
    TREE_NO_WARNING (t) = true;

  return NULL_TREE;
}

/* A dom_walker subclass for use by vrp_prop::check_all_array_refs,
   to walk over all statements of all reachable BBs and call
   check_array_bounds on them.  */

class check_array_bounds_dom_walker : public dom_walker
{
 public:
  check_array_bounds_dom_walker (vrp_prop *prop)
    : dom_walker (CDI_DOMINATORS,
		  /* Discover non-executable edges, preserving EDGE_EXECUTABLE
		     flags, so that we can merge in information on
		     non-executable edges from vrp_folder .  */
		  REACHABLE_BLOCKS_PRESERVING_FLAGS),
      m_prop (prop) {}
  ~check_array_bounds_dom_walker () {}

  edge before_dom_children (basic_block) FINAL OVERRIDE;

 private:
  vrp_prop *m_prop;
};

/* Implementation of dom_walker::before_dom_children.

   Walk over all statements of BB and call check_array_bounds on them,
   and determine if there's a unique successor edge.  */

edge
check_array_bounds_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator si;
  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      struct walk_stmt_info wi;
      if (!gimple_has_location (stmt)
	  || is_gimple_debug (stmt))
	continue;

      memset (&wi, 0, sizeof (wi));

      wi.info = m_prop;

      walk_gimple_op (stmt, check_array_bounds, &wi);
    }

  /* Determine if there's a unique successor edge, and if so, return
     that back to dom_walker, ensuring that we don't visit blocks that
     became unreachable during the VRP propagation
     (PR tree-optimization/83312).  */
  return find_taken_edge (bb, NULL_TREE);
}

/* Walk over all statements of all reachable BBs and call check_array_bounds
   on them.  */

void
vrp_prop::check_all_array_refs ()
{
  check_array_bounds_dom_walker w (this);
  w.walk (ENTRY_BLOCK_PTR_FOR_FN (cfun));
}

/* Return true if all imm uses of VAR are either in STMT, or
   feed (optionally through a chain of single imm uses) GIMPLE_COND
   in basic block COND_BB.  */

static bool
all_imm_uses_in_stmt_or_feed_cond (tree var, gimple *stmt, basic_block cond_bb)
{
  use_operand_p use_p, use2_p;
  imm_use_iterator iter;

  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    if (USE_STMT (use_p) != stmt)
      {
	gimple *use_stmt = USE_STMT (use_p), *use_stmt2;
	if (is_gimple_debug (use_stmt))
	  continue;
	while (is_gimple_assign (use_stmt)
	       && TREE_CODE (gimple_assign_lhs (use_stmt)) == SSA_NAME
	       && single_imm_use (gimple_assign_lhs (use_stmt),
				  &use2_p, &use_stmt2))
	  use_stmt = use_stmt2;
	if (gimple_code (use_stmt) != GIMPLE_COND
	    || gimple_bb (use_stmt) != cond_bb)
	  return false;
      }
  return true;
}

/* Handle
   _4 = x_3 & 31;
   if (_4 != 0)
     goto <bb 6>;
   else
     goto <bb 7>;
   <bb 6>:
   __builtin_unreachable ();
   <bb 7>:
   x_5 = ASSERT_EXPR <x_3, ...>;
   If x_3 has no other immediate uses (checked by caller),
   var is the x_3 var from ASSERT_EXPR, we can clear low 5 bits
   from the non-zero bitmask.  */

void
maybe_set_nonzero_bits (edge e, tree var)
{
  basic_block cond_bb = e->src;
  gimple *stmt = last_stmt (cond_bb);
  tree cst;

  if (stmt == NULL
      || gimple_code (stmt) != GIMPLE_COND
      || gimple_cond_code (stmt) != ((e->flags & EDGE_TRUE_VALUE)
				     ? EQ_EXPR : NE_EXPR)
      || TREE_CODE (gimple_cond_lhs (stmt)) != SSA_NAME
      || !integer_zerop (gimple_cond_rhs (stmt)))
    return;

  stmt = SSA_NAME_DEF_STMT (gimple_cond_lhs (stmt));
  if (!is_gimple_assign (stmt)
      || gimple_assign_rhs_code (stmt) != BIT_AND_EXPR
      || TREE_CODE (gimple_assign_rhs2 (stmt)) != INTEGER_CST)
    return;
  if (gimple_assign_rhs1 (stmt) != var)
    {
      gimple *stmt2;

      if (TREE_CODE (gimple_assign_rhs1 (stmt)) != SSA_NAME)
	return;
      stmt2 = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
      if (!gimple_assign_cast_p (stmt2)
	  || gimple_assign_rhs1 (stmt2) != var
	  || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt2))
	  || (TYPE_PRECISION (TREE_TYPE (gimple_assign_rhs1 (stmt)))
			      != TYPE_PRECISION (TREE_TYPE (var))))
	return;
    }
  cst = gimple_assign_rhs2 (stmt);
  set_nonzero_bits (var, wi::bit_and_not (get_nonzero_bits (var),
					  wi::to_wide (cst)));
}

/* Convert range assertion expressions into the implied copies and
   copy propagate away the copies.  Doing the trivial copy propagation
   here avoids the need to run the full copy propagation pass after
   VRP.

   FIXME, this will eventually lead to copy propagation removing the
   names that had useful range information attached to them.  For
   instance, if we had the assertion N_i = ASSERT_EXPR <N_j, N_j > 3>,
   then N_i will have the range [3, +INF].

   However, by converting the assertion into the implied copy
   operation N_i = N_j, we will then copy-propagate N_j into the uses
   of N_i and lose the range information.  We may want to hold on to
   ASSERT_EXPRs a little while longer as the ranges could be used in
   things like jump threading.

   The problem with keeping ASSERT_EXPRs around is that passes after
   VRP need to handle them appropriately.

   Another approach would be to make the range information a first
   class property of the SSA_NAME so that it can be queried from
   any pass.  This is made somewhat more complex by the need for
   multiple ranges to be associated with one SSA_NAME.  */

static void
remove_range_assertions (void)
{
  basic_block bb;
  gimple_stmt_iterator si;
  /* 1 if looking at ASSERT_EXPRs immediately at the beginning of
     a basic block preceeded by GIMPLE_COND branching to it and
     __builtin_trap, -1 if not yet checked, 0 otherwise.  */
  int is_unreachable;

  /* Note that the BSI iterator bump happens at the bottom of the
     loop and no bump is necessary if we're removing the statement
     referenced by the current BSI.  */
  FOR_EACH_BB_FN (bb, cfun)
    for (si = gsi_after_labels (bb), is_unreachable = -1; !gsi_end_p (si);)
      {
	gimple *stmt = gsi_stmt (si);

	if (is_gimple_assign (stmt)
	    && gimple_assign_rhs_code (stmt) == ASSERT_EXPR)
	  {
	    tree lhs = gimple_assign_lhs (stmt);
	    tree rhs = gimple_assign_rhs1 (stmt);
	    tree var;

	    var = ASSERT_EXPR_VAR (rhs);

	    if (TREE_CODE (var) == SSA_NAME
		&& !POINTER_TYPE_P (TREE_TYPE (lhs))
		&& SSA_NAME_RANGE_INFO (lhs))
	      {
		if (is_unreachable == -1)
		  {
		    is_unreachable = 0;
		    if (single_pred_p (bb)
			&& assert_unreachable_fallthru_edge_p
						    (single_pred_edge (bb)))
		      is_unreachable = 1;
		  }
		/* Handle
		   if (x_7 >= 10 && x_7 < 20)
		     __builtin_unreachable ();
		   x_8 = ASSERT_EXPR <x_7, ...>;
		   if the only uses of x_7 are in the ASSERT_EXPR and
		   in the condition.  In that case, we can copy the
		   range info from x_8 computed in this pass also
		   for x_7.  */
		if (is_unreachable
		    && all_imm_uses_in_stmt_or_feed_cond (var, stmt,
							  single_pred (bb)))
		  {
		    set_range_info (var, SSA_NAME_RANGE_TYPE (lhs),
				    SSA_NAME_RANGE_INFO (lhs)->get_min (),
				    SSA_NAME_RANGE_INFO (lhs)->get_max ());
		    maybe_set_nonzero_bits (single_pred_edge (bb), var);
		  }
	      }

	    /* Propagate the RHS into every use of the LHS.  For SSA names
	       also propagate abnormals as it merely restores the original
	       IL in this case (an replace_uses_by would assert).  */
	    if (TREE_CODE (var) == SSA_NAME)
	      {
		imm_use_iterator iter;
		use_operand_p use_p;
		gimple *use_stmt;
		FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
		  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		    SET_USE (use_p, var);
	      }
	    else
	      replace_uses_by (lhs, var);

	    /* And finally, remove the copy, it is not needed.  */
	    gsi_remove (&si, true);
	    release_defs (stmt);
	  }
	else
	  {
	    if (!is_gimple_debug (gsi_stmt (si)))
	      is_unreachable = 0;
	    gsi_next (&si);
	  }
      }
}

/* Return true if STMT is interesting for VRP.  */

bool
stmt_interesting_for_vrp (gimple *stmt)
{
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      tree res = gimple_phi_result (stmt);
      return (!virtual_operand_p (res)
	      && (INTEGRAL_TYPE_P (TREE_TYPE (res))
		  || POINTER_TYPE_P (TREE_TYPE (res))));
    }
  else if (is_gimple_assign (stmt) || is_gimple_call (stmt))
    {
      tree lhs = gimple_get_lhs (stmt);

      /* In general, assignments with virtual operands are not useful
	 for deriving ranges, with the obvious exception of calls to
	 builtin functions.  */
      if (lhs && TREE_CODE (lhs) == SSA_NAME
	  && (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	      || POINTER_TYPE_P (TREE_TYPE (lhs)))
	  && (is_gimple_call (stmt)
	      || !gimple_vuse (stmt)))
	return true;
      else if (is_gimple_call (stmt) && gimple_call_internal_p (stmt))
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_ADD_OVERFLOW:
	  case IFN_SUB_OVERFLOW:
	  case IFN_MUL_OVERFLOW:
	  case IFN_ATOMIC_COMPARE_EXCHANGE:
	    /* These internal calls return _Complex integer type,
	       but are interesting to VRP nevertheless.  */
	    if (lhs && TREE_CODE (lhs) == SSA_NAME)
	      return true;
	    break;
	  default:
	    break;
	  }
    }
  else if (gimple_code (stmt) == GIMPLE_COND
	   || gimple_code (stmt) == GIMPLE_SWITCH)
    return true;

  return false;
}

/* Initialization required by ssa_propagate engine.  */

void
vrp_prop::vrp_initialize ()
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gphi_iterator si = gsi_start_phis (bb); !gsi_end_p (si);
	   gsi_next (&si))
	{
	  gphi *phi = si.phi ();
	  if (!stmt_interesting_for_vrp (phi))
	    {
	      tree lhs = PHI_RESULT (phi);
	      set_def_to_varying (lhs);
	      prop_set_simulate_again (phi, false);
	    }
	  else
	    prop_set_simulate_again (phi, true);
	}

      for (gimple_stmt_iterator si = gsi_start_bb (bb); !gsi_end_p (si);
	   gsi_next (&si))
        {
	  gimple *stmt = gsi_stmt (si);

 	  /* If the statement is a control insn, then we do not
 	     want to avoid simulating the statement once.  Failure
 	     to do so means that those edges will never get added.  */
	  if (stmt_ends_bb_p (stmt))
	    prop_set_simulate_again (stmt, true);
	  else if (!stmt_interesting_for_vrp (stmt))
	    {
	      set_defs_to_varying (stmt);
	      prop_set_simulate_again (stmt, false);
	    }
	  else
	    prop_set_simulate_again (stmt, true);
	}
    }
}

/* Searches the case label vector VEC for the index *IDX of the CASE_LABEL
   that includes the value VAL.  The search is restricted to the range
   [START_IDX, n - 1] where n is the size of VEC.

   If there is a CASE_LABEL for VAL, its index is placed in IDX and true is
   returned.

   If there is no CASE_LABEL for VAL and there is one that is larger than VAL,
   it is placed in IDX and false is returned.

   If VAL is larger than any CASE_LABEL, n is placed on IDX and false is
   returned. */

bool
find_case_label_index (gswitch *stmt, size_t start_idx, tree val, size_t *idx)
{
  size_t n = gimple_switch_num_labels (stmt);
  size_t low, high;

  /* Find case label for minimum of the value range or the next one.
     At each iteration we are searching in [low, high - 1]. */

  for (low = start_idx, high = n; high != low; )
    {
      tree t;
      int cmp;
      /* Note that i != high, so we never ask for n. */
      size_t i = (high + low) / 2;
      t = gimple_switch_label (stmt, i);

      /* Cache the result of comparing CASE_LOW and val.  */
      cmp = tree_int_cst_compare (CASE_LOW (t), val);

      if (cmp == 0)
	{
	  /* Ranges cannot be empty. */
	  *idx = i;
	  return true;
	}
      else if (cmp > 0)
        high = i;
      else
	{
	  low = i + 1;
	  if (CASE_HIGH (t) != NULL
	      && tree_int_cst_compare (CASE_HIGH (t), val) >= 0)
	    {
	      *idx = i;
	      return true;
	    }
        }
    }

  *idx = high;
  return false;
}

/* Searches the case label vector VEC for the range of CASE_LABELs that is used
   for values between MIN and MAX. The first index is placed in MIN_IDX. The
   last index is placed in MAX_IDX. If the range of CASE_LABELs is empty
   then MAX_IDX < MIN_IDX.
   Returns true if the default label is not needed. */

bool
find_case_label_range (gswitch *stmt, tree min, tree max, size_t *min_idx,
		       size_t *max_idx)
{
  size_t i, j;
  bool min_take_default = !find_case_label_index (stmt, 1, min, &i);
  bool max_take_default = !find_case_label_index (stmt, i, max, &j);

  if (i == j
      && min_take_default
      && max_take_default)
    {
      /* Only the default case label reached.
         Return an empty range. */
      *min_idx = 1;
      *max_idx = 0;
      return false;
    }
  else
    {
      bool take_default = min_take_default || max_take_default;
      tree low, high;
      size_t k;

      if (max_take_default)
	j--;

      /* If the case label range is continuous, we do not need
	 the default case label.  Verify that.  */
      high = CASE_LOW (gimple_switch_label (stmt, i));
      if (CASE_HIGH (gimple_switch_label (stmt, i)))
	high = CASE_HIGH (gimple_switch_label (stmt, i));
      for (k = i + 1; k <= j; ++k)
	{
	  low = CASE_LOW (gimple_switch_label (stmt, k));
	  if (!integer_onep (int_const_binop (MINUS_EXPR, low, high)))
	    {
	      take_default = true;
	      break;
	    }
	  high = low;
	  if (CASE_HIGH (gimple_switch_label (stmt, k)))
	    high = CASE_HIGH (gimple_switch_label (stmt, k));
	}

      *min_idx = i;
      *max_idx = j;
      return !take_default;
    }
}

/* Evaluate statement STMT.  If the statement produces a useful range,
   return SSA_PROP_INTERESTING and record the SSA name with the
   interesting range into *OUTPUT_P.

   If STMT is a conditional branch and we can determine its truth
   value, the taken edge is recorded in *TAKEN_EDGE_P.

   If STMT produces a varying value, return SSA_PROP_VARYING.  */

enum ssa_prop_result
vrp_prop::visit_stmt (gimple *stmt, edge *taken_edge_p, tree *output_p)
{
  tree lhs = gimple_get_lhs (stmt);
  value_range_equiv vr;
  extract_range_from_stmt (stmt, taken_edge_p, output_p, &vr);

  if (*output_p)
    {
      if (update_value_range (*output_p, &vr))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Found new range for ");
	      print_generic_expr (dump_file, *output_p);
	      fprintf (dump_file, ": ");
	      dump_value_range (dump_file, &vr);
	      fprintf (dump_file, "\n");
	    }

	  if (vr.varying_p ())
	    return SSA_PROP_VARYING;

	  return SSA_PROP_INTERESTING;
	}
      return SSA_PROP_NOT_INTERESTING;
    }

  if (is_gimple_call (stmt) && gimple_call_internal_p (stmt))
    switch (gimple_call_internal_fn (stmt))
      {
      case IFN_ADD_OVERFLOW:
      case IFN_SUB_OVERFLOW:
      case IFN_MUL_OVERFLOW:
      case IFN_ATOMIC_COMPARE_EXCHANGE:
	/* These internal calls return _Complex integer type,
	   which VRP does not track, but the immediate uses
	   thereof might be interesting.  */
	if (lhs && TREE_CODE (lhs) == SSA_NAME)
	  {
	    imm_use_iterator iter;
	    use_operand_p use_p;
	    enum ssa_prop_result res = SSA_PROP_VARYING;

	    set_def_to_varying (lhs);

	    FOR_EACH_IMM_USE_FAST (use_p, iter, lhs)
	      {
		gimple *use_stmt = USE_STMT (use_p);
		if (!is_gimple_assign (use_stmt))
		  continue;
		enum tree_code rhs_code = gimple_assign_rhs_code (use_stmt);
		if (rhs_code != REALPART_EXPR && rhs_code != IMAGPART_EXPR)
		  continue;
		tree rhs1 = gimple_assign_rhs1 (use_stmt);
		tree use_lhs = gimple_assign_lhs (use_stmt);
		if (TREE_CODE (rhs1) != rhs_code
		    || TREE_OPERAND (rhs1, 0) != lhs
		    || TREE_CODE (use_lhs) != SSA_NAME
		    || !stmt_interesting_for_vrp (use_stmt)
		    || (!INTEGRAL_TYPE_P (TREE_TYPE (use_lhs))
			|| !TYPE_MIN_VALUE (TREE_TYPE (use_lhs))
			|| !TYPE_MAX_VALUE (TREE_TYPE (use_lhs))))
		  continue;

		/* If there is a change in the value range for any of the
		   REALPART_EXPR/IMAGPART_EXPR immediate uses, return
		   SSA_PROP_INTERESTING.  If there are any REALPART_EXPR
		   or IMAGPART_EXPR immediate uses, but none of them have
		   a change in their value ranges, return
		   SSA_PROP_NOT_INTERESTING.  If there are no
		   {REAL,IMAG}PART_EXPR uses at all,
		   return SSA_PROP_VARYING.  */
		value_range_equiv new_vr;
		extract_range_basic (&new_vr, use_stmt);
		const value_range_equiv *old_vr = get_value_range (use_lhs);
		if (!old_vr->equal_p (new_vr, /*ignore_equivs=*/false))
		  res = SSA_PROP_INTERESTING;
		else
		  res = SSA_PROP_NOT_INTERESTING;
		new_vr.equiv_clear ();
		if (res == SSA_PROP_INTERESTING)
		  {
		    *output_p = lhs;
		    return res;
		  }
	      }

	    return res;
	  }
	break;
      default:
	break;
      }

  /* All other statements produce nothing of interest for VRP, so mark
     their outputs varying and prevent further simulation.  */
  set_defs_to_varying (stmt);

  return (*taken_edge_p) ? SSA_PROP_INTERESTING : SSA_PROP_VARYING;
}

void
value_range_equiv::intersect (const value_range_equiv *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Intersecting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }

  /* If THIS is varying we want to pick up equivalences from OTHER.
     Just special-case this here rather than trying to fixup after the
     fact.  */
  if (this->varying_p ())
    this->deep_copy (other);
  else
    {
      value_range tem = intersect_helper (this, other);
      this->update (tem.min (), tem.max (), tem.kind ());

      /* If the result is VR_UNDEFINED there is no need to mess with
	 equivalencies.  */
      if (!undefined_p ())
	{
	  /* The resulting set of equivalences for range intersection
	     is the union of the two sets.  */
	  if (m_equiv && other->m_equiv && m_equiv != other->m_equiv)
	    bitmap_ior_into (m_equiv, other->m_equiv);
	  else if (other->m_equiv && !m_equiv)
	    {
	      /* All equivalence bitmaps are allocated from the same
		 obstack.  So we can use the obstack associated with
		 VR to allocate this->m_equiv.  */
	      m_equiv = BITMAP_ALLOC (other->m_equiv->obstack);
	      bitmap_copy (m_equiv, other->m_equiv);
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

void
value_range_equiv::union_ (const value_range_equiv *other)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Meeting\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\nand\n  ");
      dump_value_range (dump_file, other);
      fprintf (dump_file, "\n");
    }

  /* If THIS is undefined we want to pick up equivalences from OTHER.
     Just special-case this here rather than trying to fixup after the fact.  */
  if (this->undefined_p ())
    this->deep_copy (other);
  else
    {
      value_range tem = union_helper (this, other);
      this->update (tem.min (), tem.max (), tem.kind ());

      /* The resulting set of equivalences is always the intersection of
	 the two sets.  */
      if (this->m_equiv && other->m_equiv && this->m_equiv != other->m_equiv)
	bitmap_and_into (this->m_equiv, other->m_equiv);
      else if (this->m_equiv && !other->m_equiv)
	bitmap_clear (this->m_equiv);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "to\n  ");
      dump_value_range (dump_file, this);
      fprintf (dump_file, "\n");
    }
}

/* Visit all arguments for PHI node PHI that flow through executable
   edges.  If a valid value range can be derived from all the incoming
   value ranges, set a new range for the LHS of PHI.  */

enum ssa_prop_result
vrp_prop::visit_phi (gphi *phi)
{
  tree lhs = PHI_RESULT (phi);
  value_range_equiv vr_result;
  extract_range_from_phi_node (phi, &vr_result);
  if (update_value_range (lhs, &vr_result))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Found new range for ");
	  print_generic_expr (dump_file, lhs);
	  fprintf (dump_file, ": ");
	  dump_value_range (dump_file, &vr_result);
	  fprintf (dump_file, "\n");
	}

      if (vr_result.varying_p ())
	return SSA_PROP_VARYING;

      return SSA_PROP_INTERESTING;
    }

  /* Nothing changed, don't add outgoing edges.  */
  return SSA_PROP_NOT_INTERESTING;
}

class vrp_folder : public substitute_and_fold_engine
{
 public:
  vrp_folder () : substitute_and_fold_engine (/* Fold all stmts.  */ true) {  }
  tree get_value (tree) FINAL OVERRIDE;
  bool fold_stmt (gimple_stmt_iterator *) FINAL OVERRIDE;
  bool fold_predicate_in (gimple_stmt_iterator *);

  class vr_values *vr_values;

  /* Delegators.  */
  tree vrp_evaluate_conditional (tree_code code, tree op0,
				 tree op1, gimple *stmt)
    { return vr_values->vrp_evaluate_conditional (code, op0, op1, stmt); }
  bool simplify_stmt_using_ranges (gimple_stmt_iterator *gsi)
    { return vr_values->simplify_stmt_using_ranges (gsi); }
 tree op_with_constant_singleton_value_range (tree op)
    { return vr_values->op_with_constant_singleton_value_range (op); }
};

/* If the statement pointed by SI has a predicate whose value can be
   computed using the value range information computed by VRP, compute
   its value and return true.  Otherwise, return false.  */

bool
vrp_folder::fold_predicate_in (gimple_stmt_iterator *si)
{
  bool assignment_p = false;
  tree val;
  gimple *stmt = gsi_stmt (*si);

  if (is_gimple_assign (stmt)
      && TREE_CODE_CLASS (gimple_assign_rhs_code (stmt)) == tcc_comparison)
    {
      assignment_p = true;
      val = vrp_evaluate_conditional (gimple_assign_rhs_code (stmt),
				      gimple_assign_rhs1 (stmt),
				      gimple_assign_rhs2 (stmt),
				      stmt);
    }
  else if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
    val = vrp_evaluate_conditional (gimple_cond_code (cond_stmt),
				    gimple_cond_lhs (cond_stmt),
				    gimple_cond_rhs (cond_stmt),
				    stmt);
  else
    return false;

  if (val)
    {
      if (assignment_p)
        val = fold_convert (gimple_expr_type (stmt), val);

      if (dump_file)
	{
	  fprintf (dump_file, "Folding predicate ");
	  print_gimple_expr (dump_file, stmt, 0);
	  fprintf (dump_file, " to ");
	  print_generic_expr (dump_file, val);
	  fprintf (dump_file, "\n");
	}

      if (is_gimple_assign (stmt))
	gimple_assign_set_rhs_from_tree (si, val);
      else
	{
	  gcc_assert (gimple_code (stmt) == GIMPLE_COND);
	  gcond *cond_stmt = as_a <gcond *> (stmt);
	  if (integer_zerop (val))
	    gimple_cond_make_false (cond_stmt);
	  else if (integer_onep (val))
	    gimple_cond_make_true (cond_stmt);
	  else
	    gcc_unreachable ();
	}

      return true;
    }

  return false;
}

/* Callback for substitute_and_fold folding the stmt at *SI.  */

bool
vrp_folder::fold_stmt (gimple_stmt_iterator *si)
{
  if (fold_predicate_in (si))
    return true;

  return simplify_stmt_using_ranges (si);
}

/* If OP has a value range with a single constant value return that,
   otherwise return NULL_TREE.  This returns OP itself if OP is a
   constant.

   Implemented as a pure wrapper right now, but this will change.  */

tree
vrp_folder::get_value (tree op)
{
  return op_with_constant_singleton_value_range (op);
}

/* Return the LHS of any ASSERT_EXPR where OP appears as the first
   argument to the ASSERT_EXPR and in which the ASSERT_EXPR dominates
   BB.  If no such ASSERT_EXPR is found, return OP.  */

static tree
lhs_of_dominating_assert (tree op, basic_block bb, gimple *stmt)
{
  imm_use_iterator imm_iter;
  gimple *use_stmt;
  use_operand_p use_p;

  if (TREE_CODE (op) == SSA_NAME)
    {
      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, op)
	{
	  use_stmt = USE_STMT (use_p);
	  if (use_stmt != stmt
	      && gimple_assign_single_p (use_stmt)
	      && TREE_CODE (gimple_assign_rhs1 (use_stmt)) == ASSERT_EXPR
	      && TREE_OPERAND (gimple_assign_rhs1 (use_stmt), 0) == op
	      && dominated_by_p (CDI_DOMINATORS, bb, gimple_bb (use_stmt)))
	    return gimple_assign_lhs (use_stmt);
	}
    }
  return op;
}

/* A hack.  */
static class vr_values *x_vr_values;

/* A trivial wrapper so that we can present the generic jump threading
   code with a simple API for simplifying statements.  STMT is the
   statement we want to simplify, WITHIN_STMT provides the location
   for any overflow warnings.  */

static tree
simplify_stmt_for_jump_threading (gimple *stmt, gimple *within_stmt,
    class avail_exprs_stack *avail_exprs_stack ATTRIBUTE_UNUSED,
    basic_block bb)
{
  /* First see if the conditional is in the hash table.  */
  tree cached_lhs = avail_exprs_stack->lookup_avail_expr (stmt, false, true);
  if (cached_lhs && is_gimple_min_invariant (cached_lhs))
    return cached_lhs;

  vr_values *vr_values = x_vr_values;
  if (gcond *cond_stmt = dyn_cast <gcond *> (stmt))
    {
      tree op0 = gimple_cond_lhs (cond_stmt);
      op0 = lhs_of_dominating_assert (op0, bb, stmt);

      tree op1 = gimple_cond_rhs (cond_stmt);
      op1 = lhs_of_dominating_assert (op1, bb, stmt);

      return vr_values->vrp_evaluate_conditional (gimple_cond_code (cond_stmt),
						  op0, op1, within_stmt);
    }

  /* We simplify a switch statement by trying to determine which case label
     will be taken.  If we are successful then we return the corresponding
     CASE_LABEL_EXPR.  */
  if (gswitch *switch_stmt = dyn_cast <gswitch *> (stmt))
    {
      tree op = gimple_switch_index (switch_stmt);
      if (TREE_CODE (op) != SSA_NAME)
	return NULL_TREE;

      op = lhs_of_dominating_assert (op, bb, stmt);

      const value_range_equiv *vr = vr_values->get_value_range (op);
      if (vr->undefined_p ()
	  || vr->varying_p ()
	  || vr->symbolic_p ())
	return NULL_TREE;

      if (vr->kind () == VR_RANGE)
	{
	  size_t i, j;
	  /* Get the range of labels that contain a part of the operand's
	     value range.  */
	  find_case_label_range (switch_stmt, vr->min (), vr->max (), &i, &j);

	  /* Is there only one such label?  */
	  if (i == j)
	    {
	      tree label = gimple_switch_label (switch_stmt, i);

	      /* The i'th label will be taken only if the value range of the
		 operand is entirely within the bounds of this label.  */
	      if (CASE_HIGH (label) != NULL_TREE
		  ? (tree_int_cst_compare (CASE_LOW (label), vr->min ()) <= 0
		     && tree_int_cst_compare (CASE_HIGH (label),
					      vr->max ()) >= 0)
		  : (tree_int_cst_equal (CASE_LOW (label), vr->min ())
		     && tree_int_cst_equal (vr->min (), vr->max ())))
		return label;
	    }

	  /* If there are no such labels then the default label will be
	     taken.  */
	  if (i > j)
	    return gimple_switch_label (switch_stmt, 0);
	}

      if (vr->kind () == VR_ANTI_RANGE)
	{
	  unsigned n = gimple_switch_num_labels (switch_stmt);
	  tree min_label = gimple_switch_label (switch_stmt, 1);
	  tree max_label = gimple_switch_label (switch_stmt, n - 1);

	  /* The default label will be taken only if the anti-range of the
	     operand is entirely outside the bounds of all the (non-default)
	     case labels.  */
	  if (tree_int_cst_compare (vr->min (), CASE_LOW (min_label)) <= 0
	      && (CASE_HIGH (max_label) != NULL_TREE
		  ? tree_int_cst_compare (vr->max (),
					  CASE_HIGH (max_label)) >= 0
		  : tree_int_cst_compare (vr->max (),
					  CASE_LOW (max_label)) >= 0))
	  return gimple_switch_label (switch_stmt, 0);
	}

      return NULL_TREE;
    }

  if (gassign *assign_stmt = dyn_cast <gassign *> (stmt))
    {
      tree lhs = gimple_assign_lhs (assign_stmt);
      if (TREE_CODE (lhs) == SSA_NAME
	  && (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	      || POINTER_TYPE_P (TREE_TYPE (lhs)))
	  && stmt_interesting_for_vrp (stmt))
	{
	  edge dummy_e;
	  tree dummy_tree;
	  value_range_equiv new_vr;
	  vr_values->extract_range_from_stmt (stmt, &dummy_e,
					      &dummy_tree, &new_vr);
	  tree singleton;
	  if (new_vr.singleton_p (&singleton))
	    return singleton;
	}
    }

  return NULL_TREE;
}

class vrp_dom_walker : public dom_walker
{
public:
  vrp_dom_walker (cdi_direction direction,
		  class const_and_copies *const_and_copies,
		  class avail_exprs_stack *avail_exprs_stack)
    : dom_walker (direction, REACHABLE_BLOCKS),
      m_const_and_copies (const_and_copies),
      m_avail_exprs_stack (avail_exprs_stack),
      m_dummy_cond (NULL) {}

  virtual edge before_dom_children (basic_block);
  virtual void after_dom_children (basic_block);

  class vr_values *vr_values;

private:
  class const_and_copies *m_const_and_copies;
  class avail_exprs_stack *m_avail_exprs_stack;

  gcond *m_dummy_cond;

};

/* Called before processing dominator children of BB.  We want to look
   at ASSERT_EXPRs and record information from them in the appropriate
   tables.

   We could look at other statements here.  It's not seen as likely
   to significantly increase the jump threads we discover.  */

edge
vrp_dom_walker::before_dom_children (basic_block bb)
{
  gimple_stmt_iterator gsi;

  m_avail_exprs_stack->push_marker ();
  m_const_and_copies->push_marker ();
  for (gsi = gsi_start_nondebug_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_assign_single_p (stmt)
         && TREE_CODE (gimple_assign_rhs1 (stmt)) == ASSERT_EXPR)
	{
	  tree rhs1 = gimple_assign_rhs1 (stmt);
	  tree cond = TREE_OPERAND (rhs1, 1);
	  tree inverted = invert_truthvalue (cond);
	  vec<cond_equivalence> p;
	  p.create (3);
	  record_conditions (&p, cond, inverted);
	  for (unsigned int i = 0; i < p.length (); i++)
	    m_avail_exprs_stack->record_cond (&p[i]);

	  tree lhs = gimple_assign_lhs (stmt);
	  m_const_and_copies->record_const_or_copy (lhs,
						    TREE_OPERAND (rhs1, 0));
	  p.release ();
	  continue;
	}
      break;
    }
  return NULL;
}

/* Called after processing dominator children of BB.  This is where we
   actually call into the threader.  */
void
vrp_dom_walker::after_dom_children (basic_block bb)
{
  if (!m_dummy_cond)
    m_dummy_cond = gimple_build_cond (NE_EXPR,
				      integer_zero_node, integer_zero_node,
				      NULL, NULL);

  x_vr_values = vr_values;
  thread_outgoing_edges (bb, m_dummy_cond, m_const_and_copies,
			 m_avail_exprs_stack, NULL,
			 simplify_stmt_for_jump_threading);
  x_vr_values = NULL;

  m_avail_exprs_stack->pop_to_marker ();
  m_const_and_copies->pop_to_marker ();
}

/* Blocks which have more than one predecessor and more than
   one successor present jump threading opportunities, i.e.,
   when the block is reached from a specific predecessor, we
   may be able to determine which of the outgoing edges will
   be traversed.  When this optimization applies, we are able
   to avoid conditionals at runtime and we may expose secondary
   optimization opportunities.

   This routine is effectively a driver for the generic jump
   threading code.  It basically just presents the generic code
   with edges that may be suitable for jump threading.

   Unlike DOM, we do not iterate VRP if jump threading was successful.
   While iterating may expose new opportunities for VRP, it is expected
   those opportunities would be very limited and the compile time cost
   to expose those opportunities would be significant.

   As jump threading opportunities are discovered, they are registered
   for later realization.  */

static void
identify_jump_threads (class vr_values *vr_values)
{
  /* Ugh.  When substituting values earlier in this pass we can
     wipe the dominance information.  So rebuild the dominator
     information as we need it within the jump threading code.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* We do not allow VRP information to be used for jump threading
     across a back edge in the CFG.  Otherwise it becomes too
     difficult to avoid eliminating loop exit tests.  Of course
     EDGE_DFS_BACK is not accurate at this time so we have to
     recompute it.  */
  mark_dfs_back_edges ();

  /* Allocate our unwinder stack to unwind any temporary equivalences
     that might be recorded.  */
  const_and_copies *equiv_stack = new const_and_copies ();

  hash_table<expr_elt_hasher> *avail_exprs
    = new hash_table<expr_elt_hasher> (1024);
  avail_exprs_stack *avail_exprs_stack
    = new class avail_exprs_stack (avail_exprs);

  vrp_dom_walker walker (CDI_DOMINATORS, equiv_stack, avail_exprs_stack);
  walker.vr_values = vr_values;
  walker.walk (cfun->cfg->x_entry_block_ptr);

  /* We do not actually update the CFG or SSA graphs at this point as
     ASSERT_EXPRs are still in the IL and cfg cleanup code does not yet
     handle ASSERT_EXPRs gracefully.  */
  delete equiv_stack;
  delete avail_exprs;
  delete avail_exprs_stack;
}

/* Traverse all the blocks folding conditionals with known ranges.  */

void
vrp_prop::vrp_finalize (bool warn_array_bounds_p)
{
  size_t i;

  /* We have completed propagating through the lattice.  */
  vr_values.set_lattice_propagation_complete ();

  if (dump_file)
    {
      fprintf (dump_file, "\nValue ranges after VRP:\n\n");
      vr_values.dump_all_value_ranges (dump_file);
      fprintf (dump_file, "\n");
    }

  /* Set value range to non pointer SSA_NAMEs.  */
  for (i = 0; i < num_ssa_names; i++)
    {
      tree name = ssa_name (i);
      if (!name)
	continue;

      const value_range_equiv *vr = get_value_range (name);
      if (!name || !vr->constant_p ())
	continue;

      if (POINTER_TYPE_P (TREE_TYPE (name))
	  && range_includes_zero_p (vr) == 0)
	set_ptr_nonnull (name);
      else if (!POINTER_TYPE_P (TREE_TYPE (name)))
	set_range_info (name, *vr);
    }

  /* If we're checking array refs, we want to merge information on
     the executability of each edge between vrp_folder and the
     check_array_bounds_dom_walker: each can clear the
     EDGE_EXECUTABLE flag on edges, in different ways.

     Hence, if we're going to call check_all_array_refs, set
     the flag on every edge now, rather than in
     check_array_bounds_dom_walker's ctor; vrp_folder may clear
     it from some edges.  */
  if (warn_array_bounds && warn_array_bounds_p)
    set_all_edges_as_executable (cfun);

  class vrp_folder vrp_folder;
  vrp_folder.vr_values = &vr_values;
  vrp_folder.substitute_and_fold ();

  if (warn_array_bounds && warn_array_bounds_p)
    check_all_array_refs ();
}

/* Main entry point to VRP (Value Range Propagation).  This pass is
   loosely based on J. R. C. Patterson, ``Accurate Static Branch
   Prediction by Value Range Propagation,'' in SIGPLAN Conference on
   Programming Language Design and Implementation, pp. 67-78, 1995.
   Also available at http://citeseer.ist.psu.edu/patterson95accurate.html

   This is essentially an SSA-CCP pass modified to deal with ranges
   instead of constants.

   While propagating ranges, we may find that two or more SSA name
   have equivalent, though distinct ranges.  For instance,

     1	x_9 = p_3->a;
     2	p_4 = ASSERT_EXPR <p_3, p_3 != 0>
     3	if (p_4 == q_2)
     4	  p_5 = ASSERT_EXPR <p_4, p_4 == q_2>;
     5	endif
     6	if (q_2)

   In the code above, pointer p_5 has range [q_2, q_2], but from the
   code we can also determine that p_5 cannot be NULL and, if q_2 had
   a non-varying range, p_5's range should also be compatible with it.

   These equivalences are created by two expressions: ASSERT_EXPR and
   copy operations.  Since p_5 is an assertion on p_4, and p_4 was the
   result of another assertion, then we can use the fact that p_5 and
   p_4 are equivalent when evaluating p_5's range.

   Together with value ranges, we also propagate these equivalences
   between names so that we can take advantage of information from
   multiple ranges when doing final replacement.  Note that this
   equivalency relation is transitive but not symmetric.

   In the example above, p_5 is equivalent to p_4, q_2 and p_3, but we
   cannot assert that q_2 is equivalent to p_5 because q_2 may be used
   in contexts where that assertion does not hold (e.g., in line 6).

   TODO, the main difference between this pass and Patterson's is that
   we do not propagate edge probabilities.  We only compute whether
   edges can be taken or not.  That is, instead of having a spectrum
   of jump probabilities between 0 and 1, we only deal with 0, 1 and
   DON'T KNOW.  In the future, it may be worthwhile to propagate
   probabilities to aid branch prediction.  */

static unsigned int
execute_vrp (bool warn_array_bounds_p)
{

  loop_optimizer_init (LOOPS_NORMAL | LOOPS_HAVE_RECORDED_EXITS);
  rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa);
  scev_initialize ();

  /* ???  This ends up using stale EDGE_DFS_BACK for liveness computation.
     Inserting assertions may split edges which will invalidate
     EDGE_DFS_BACK.  */
  insert_range_assertions ();

  threadedge_initialize_values ();

  /* For visiting PHI nodes we need EDGE_DFS_BACK computed.  */
  mark_dfs_back_edges ();

  class vrp_prop vrp_prop;
  vrp_prop.vrp_initialize ();
  vrp_prop.ssa_propagate ();
  vrp_prop.vrp_finalize (warn_array_bounds_p);

  /* We must identify jump threading opportunities before we release
     the datastructures built by VRP.  */
  identify_jump_threads (&vrp_prop.vr_values);

  /* A comparison of an SSA_NAME against a constant where the SSA_NAME
     was set by a type conversion can often be rewritten to use the
     RHS of the type conversion.

     However, doing so inhibits jump threading through the comparison.
     So that transformation is not performed until after jump threading
     is complete.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple *last = last_stmt (bb);
      if (last && gimple_code (last) == GIMPLE_COND)
	vrp_prop.vr_values.simplify_cond_using_ranges_2 (as_a <gcond *> (last));
    }

  free_numbers_of_iterations_estimates (cfun);

  /* ASSERT_EXPRs must be removed before finalizing jump threads
     as finalizing jump threads calls the CFG cleanup code which
     does not properly handle ASSERT_EXPRs.  */
  remove_range_assertions ();

  /* If we exposed any new variables, go ahead and put them into
     SSA form now, before we handle jump threading.  This simplifies
     interactions between rewriting of _DECL nodes into SSA form
     and rewriting SSA_NAME nodes into SSA form after block
     duplication and CFG manipulation.  */
  update_ssa (TODO_update_ssa);

  /* We identified all the jump threading opportunities earlier, but could
     not transform the CFG at that time.  This routine transforms the
     CFG and arranges for the dominator tree to be rebuilt if necessary.

     Note the SSA graph update will occur during the normal TODO
     processing by the pass manager.  */
  thread_through_all_blocks (false);

  vrp_prop.vr_values.cleanup_edges_and_switches ();
  threadedge_finalize_values ();

  scev_finalize ();
  loop_optimizer_finalize ();
  return 0;
}

namespace {

const pass_data pass_data_vrp =
{
  GIMPLE_PASS, /* type */
  "vrp", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_VRP, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_cleanup_cfg | TODO_update_ssa ), /* todo_flags_finish */
};

class pass_vrp : public gimple_opt_pass
{
public:
  pass_vrp (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_vrp, ctxt), warn_array_bounds_p (false)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_vrp (m_ctxt); }
  void set_pass_param (unsigned int n, bool param)
    {
      gcc_assert (n == 0);
      warn_array_bounds_p = param;
    }
  virtual bool gate (function *) { return flag_tree_vrp != 0; }
  virtual unsigned int execute (function *)
    { return execute_vrp (warn_array_bounds_p); }

 private:
  bool warn_array_bounds_p;
}; // class pass_vrp

} // anon namespace

gimple_opt_pass *
make_pass_vrp (gcc::context *ctxt)
{
  return new pass_vrp (ctxt);
}


/* Worker for determine_value_range.  */

static void
determine_value_range_1 (value_range *vr, tree expr)
{
  if (BINARY_CLASS_P (expr))
    {
      value_range vr0, vr1;
      determine_value_range_1 (&vr0, TREE_OPERAND (expr, 0));
      determine_value_range_1 (&vr1, TREE_OPERAND (expr, 1));
      range_fold_binary_expr (vr, TREE_CODE (expr), TREE_TYPE (expr),
			      &vr0, &vr1);
    }
  else if (UNARY_CLASS_P (expr))
    {
      value_range vr0;
      determine_value_range_1 (&vr0, TREE_OPERAND (expr, 0));
      range_fold_unary_expr (vr, TREE_CODE (expr), TREE_TYPE (expr),
			     &vr0, TREE_TYPE (TREE_OPERAND (expr, 0)));
    }
  else if (TREE_CODE (expr) == INTEGER_CST)
    vr->set (expr);
  else
    {
      value_range_kind kind;
      wide_int min, max;
      /* For SSA names try to extract range info computed by VRP.  Otherwise
	 fall back to varying.  */
      if (TREE_CODE (expr) == SSA_NAME
	  && INTEGRAL_TYPE_P (TREE_TYPE (expr))
	  && (kind = get_range_info (expr, &min, &max)) != VR_VARYING)
	vr->set (wide_int_to_tree (TREE_TYPE (expr), min),
		 wide_int_to_tree (TREE_TYPE (expr), max),
		 kind);
      else
	vr->set_varying (TREE_TYPE (expr));
    }
}

/* Compute a value-range for EXPR and set it in *MIN and *MAX.  Return
   the determined range type.  */

value_range_kind
determine_value_range (tree expr, wide_int *min, wide_int *max)
{
  value_range vr;
  determine_value_range_1 (&vr, expr);
  if (vr.constant_p ())
    {
      *min = wi::to_wide (vr.min ());
      *max = wi::to_wide (vr.max ());
      return vr.kind ();
    }

  return VR_VARYING;
}
