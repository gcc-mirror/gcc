/* Support routines for Value Range Propagation (VRP).
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "flags.h"
#include "tree.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "tree-dump.h"
#include "timevar.h"
#include "diagnostic.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-ssa-propagate.h"
#include "tree-chrec.h"

/* Set of SSA names found during the dominator traversal of a
   sub-graph in maybe_add_assert_expr.  */
static sbitmap found;

/* Loop structure of the program.  Used to analyze scalar evolutions
   inside adjust_range_with_scev.  */
static struct loops *cfg_loops;

/* Local functions.  */
static int compare_values (tree val1, tree val2);

/* Given a conditional predicate COND that has WHICH as one of its
   operands, return the other operand.  No error checking is done.
   This helper assumes that COND is a comparison and WHICH is one of
   its operands.  */

static inline tree
get_opposite_operand (tree cond, tree which)
{
  if (TREE_OPERAND (cond, 0) == which)
    return TREE_OPERAND (cond, 1);
  else
    return TREE_OPERAND (cond, 0);
}


/* Given a comparison code, return its opposite.  Note that this is *not*
   the same as inverting its truth value (invert_tree_comparison).  Here we
   just want to literally flip the comparison around.
   
   So, '<' gets '>', '<=' gets '>='.  Both '==' and '!=' are returned
   unchanged.  */

static enum tree_code
opposite_comparison (enum tree_code code)
{
  switch (code)
    {
    case EQ_EXPR:
    case NE_EXPR:
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
    case LTGT_EXPR:
    case UNEQ_EXPR:
      return code;
    case GT_EXPR:
      return LT_EXPR;
    case GE_EXPR:
      return LE_EXPR;
    case LT_EXPR:
      return GT_EXPR;
    case LE_EXPR:
      return GE_EXPR;
    case UNGT_EXPR:
      return UNLT_EXPR;
    case UNGE_EXPR:
      return UNLE_EXPR;
    case UNLT_EXPR:
      return UNGT_EXPR;
    case UNLE_EXPR:
      return UNGE_EXPR;
    default:
      gcc_unreachable ();
    }
}


/* Set value range VR to {T, MIN, MAX}.  */

static inline void
set_value_range (value_range *vr, enum value_range_type t, tree min, tree max)
{
#if defined ENABLE_CHECKING
  if (t == VR_RANGE || t == VR_ANTI_RANGE)
    {
      int cmp;

      gcc_assert (min && max);

      if (INTEGRAL_TYPE_P (TREE_TYPE (min)) && t == VR_ANTI_RANGE)
	gcc_assert (min != TYPE_MIN_VALUE (TREE_TYPE (min))
		    || max != TYPE_MAX_VALUE (TREE_TYPE (max)));

      cmp = compare_values (min, max);
      gcc_assert (cmp == 0 || cmp == -1 || cmp == -2);
    }
#endif

  if (t == VR_RANGE
      && INTEGRAL_TYPE_P (TREE_TYPE (min))
      && min == TYPE_MIN_VALUE (TREE_TYPE (min))
      && max == TYPE_MAX_VALUE (TREE_TYPE (max)))
    {
      /* Ranges that cover all the possible values for the type decay
	 to VARYING.  */
      vr->type = VR_VARYING;
      vr->min = NULL_TREE;
      vr->max = NULL_TREE;
      return;
    }

  vr->type = t;
  vr->min = min;
  vr->max = max;
}


/* Similar to set_value_range but return true if any field of VR
   changed from its previous value.  */

static inline bool
update_value_range (value_range *vr, enum value_range_type t, tree min,
		    tree max)
{
  bool is_new = vr->type != t || vr->min != min || vr->max != max;
  if (is_new)
    set_value_range (vr, t, min, max);

  return is_new;
}


/* Return value range information for VAR.  Create an empty range if
   none existed.  */

value_range *
get_value_range (tree var)
{
  value_range *vr;
  tree sym;

  vr = SSA_NAME_VALUE_RANGE (var);
  if (vr)
    return vr;

  /* Create a default value range.  */
  vr = ggc_alloc (sizeof (*vr));
  memset ((void *) vr, 0, sizeof (*vr));
  SSA_NAME_VALUE_RANGE (var) = vr;

  /* If VAR is a default definition for a PARM_DECL, then we have to
     assume a VARYING range for it.  */
  sym = SSA_NAME_VAR (var);
  if (TREE_CODE (sym) == PARM_DECL && var == var_ann (sym)->default_def)
    set_value_range (vr, VR_VARYING, NULL_TREE, NULL_TREE);

  return vr;
}


/* Return true if value range VR involves at least one symbol.  */

static inline bool
symbolic_range_p (value_range *vr)
{
  return (!is_gimple_min_invariant (vr->min)
          || !is_gimple_min_invariant (vr->max));
}


/* Return true if EXPR computes a non-zero value.  */

bool
expr_computes_nonzero (tree expr)
{
  /* Type casts won't change anything, so just strip it.  */
  STRIP_NOPS (expr);

  /* Calling alloca, guarantees that the value is non-NULL.  */
  if (alloca_call_p (expr))
    return true;

  /* The address of a non-weak symbol is never NULL, unless the user
     has requested not to remove NULL pointer checks.  */
  if (flag_delete_null_pointer_checks
      && TREE_CODE (expr) == ADDR_EXPR
      && DECL_P (TREE_OPERAND (expr, 0))
      && !DECL_WEAK (TREE_OPERAND (expr, 0)))
    return true;

  /* IOR of any value with a nonzero value will result in a nonzero
     value.  */
  if (TREE_CODE (expr) == BIT_IOR_EXPR
      && integer_nonzerop (TREE_OPERAND (expr, 1)))
    return true;

  return false;
}


/* Return true if VR is ~[0, 0].  */

static inline bool
range_is_nonnull (value_range *vr)
{
  return vr->type == VR_ANTI_RANGE
	 && integer_zerop (vr->min)
	 && integer_zerop (vr->max);
}


/* Return true if VR is [0, 0].  */

static inline bool
range_is_null (value_range *vr)
{
  return vr->type == VR_RANGE
	 && integer_zerop (vr->min)
	 && integer_zerop (vr->max);
}


/* Set value range VR to a non-NULL range of type TYPE.  */

static inline void
set_value_range_to_nonnull (value_range *vr, tree type)
{
  tree zero = build_int_cst (type, 0);
  set_value_range (vr, VR_ANTI_RANGE, zero, zero);
}


/* Set value range VR to a NULL range of type TYPE.  */

static inline void
set_value_range_to_null (value_range *vr, tree type)
{
  tree zero = build_int_cst (type, 0);
  set_value_range (vr, VR_RANGE, zero, zero);
}


/* Set value range VR to VR_VARYING.  */

static inline void
set_value_range_to_varying (value_range *vr)
{
  set_value_range (vr, VR_VARYING, NULL_TREE, NULL_TREE);
}


/* Compare two values VAL1 and VAL2.  Return
   
   	-2 if VAL1 and VAL2 cannot be compared at compile-time,
   	-1 if VAL1 < VAL2,
   	 0 if VAL1 == VAL2,
	+1 if VAL1 > VAL2, and
	+2 if VAL1 != VAL2

   This is similar to tree_int_cst_compare but supports pointer values
   and values that cannot be compared at compile time.  */

static int
compare_values (tree val1, tree val2)
{
  if (val1 == val2)
    return 0;

  /* Below we rely on the fact that VAL1 and VAL2 are both pointers or
     both integers.  */
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (val1))
	      == POINTER_TYPE_P (TREE_TYPE (val2)));

  /* Do some limited symbolic comparisons.  */
  if (!POINTER_TYPE_P (TREE_TYPE (val1)))
    {
      /* We can determine some comparisons against +INF and -INF even
	 if the other value is an expression.  */
      if (val1 == TYPE_MAX_VALUE (TREE_TYPE (val1))
	  && TREE_CODE (val2) == MINUS_EXPR)
	{
	  /* +INF > NAME - CST.  */
	  return 1;
	}
      else if (val1 == TYPE_MIN_VALUE (TREE_TYPE (val1))
	       && TREE_CODE (val2) == PLUS_EXPR)
	{
	  /* -INF < NAME + CST.  */
	  return -1;
	}
      else if (TREE_CODE (val1) == MINUS_EXPR
	       && val2 == TYPE_MAX_VALUE (TREE_TYPE (val2)))
	{
	  /* NAME - CST < +INF.  */
	  return -1;
	}
      else if (TREE_CODE (val1) == PLUS_EXPR
	       && val2 == TYPE_MIN_VALUE (TREE_TYPE (val2)))
	{
	  /* NAME + CST > -INF.  */
	  return 1;
	}
    }

  if ((TREE_CODE (val1) == SSA_NAME
       || TREE_CODE (val1) == PLUS_EXPR
       || TREE_CODE (val1) == MINUS_EXPR)
      && (TREE_CODE (val2) == SSA_NAME
	  || TREE_CODE (val2) == PLUS_EXPR
	  || TREE_CODE (val2) == MINUS_EXPR))
    {
      tree n1, c1, n2, c2;
  
      /* If VAL1 and VAL2 are of the form 'NAME [+-] CST' or 'NAME',
	 return -1 or +1 accordingly.  If VAL1 and VAL2 don't use the
	 same name, return -2.  */
      if (TREE_CODE (val1) == SSA_NAME)
	{
	  n1 = val1;
	  c1 = NULL_TREE;
	}
      else
	{
	  n1 = TREE_OPERAND (val1, 0);
	  c1 = TREE_OPERAND (val1, 1);
	}

      if (TREE_CODE (val2) == SSA_NAME)
	{
	  n2 = val2;
	  c2 = NULL_TREE;
	}
      else
	{
	  n2 = TREE_OPERAND (val2, 0);
	  c2 = TREE_OPERAND (val2, 1);
	}

      /* Both values must use the same name.  */
      if (n1 != n2)
	return -2;

      if (TREE_CODE (val1) == SSA_NAME)
	{
	  if (TREE_CODE (val2) == SSA_NAME)
	    /* NAME == NAME  */
	    return 0;
	  else if (TREE_CODE (val2) == PLUS_EXPR)
	    /* NAME < NAME + CST  */
	    return -1;
	  else if (TREE_CODE (val2) == MINUS_EXPR)
	    /* NAME > NAME - CST  */
	    return 1;
	}
      else if (TREE_CODE (val1) == PLUS_EXPR)
	{
	  if (TREE_CODE (val2) == SSA_NAME)
	    /* NAME + CST > NAME  */
	    return 1;
	  else if (TREE_CODE (val2) == PLUS_EXPR)
	    /* NAME + CST1 > NAME + CST2, if CST1 > CST2  */
	    return compare_values (c1, c2);
	  else if (TREE_CODE (val2) == MINUS_EXPR)
	    /* NAME + CST1 > NAME - CST2  */
	    return 1;
	}
      else if (TREE_CODE (val1) == MINUS_EXPR)
	{
	  if (TREE_CODE (val2) == SSA_NAME)
	    /* NAME - CST < NAME  */
	    return -1;
	  else if (TREE_CODE (val2) == PLUS_EXPR)
	    /* NAME - CST1 < NAME + CST2  */
	    return -1;
	  else if (TREE_CODE (val2) == MINUS_EXPR)
	    /* NAME - CST1 > NAME - CST2, if CST1 < CST2.  Notice that
	       C1 and C2 are swapped in the call to compare_values.  */
	    return compare_values (c2, c1);
	}

      gcc_unreachable ();
    }

  /* We cannot compare non-constants.  */
  if (!is_gimple_min_invariant (val1) || !is_gimple_min_invariant (val2))
    return -2;

  if (!POINTER_TYPE_P (TREE_TYPE (val1)))
    return tree_int_cst_compare (val1, val2);
  else
    {
      tree t;

      /* First see if VAL1 and VAL2 are not the same.  */
      if (val1 == val2 || operand_equal_p (val1, val2, 0))
	return 0;
      
      /* If VAL1 is a lower address than VAL2, return -1.  */
      t = fold_binary (LT_EXPR, boolean_type_node, val1, val2);
      if (t == boolean_true_node)
	return -1;

      /* If VAL1 is a higher address than VAL2, return +1.  */
      t = fold_binary (GT_EXPR, boolean_type_node, val1, val2);
      if (t == boolean_true_node)
	return 1;

      /* If VAL1 is different than VAL2, return +2.  */
      t = fold_binary (NE_EXPR, boolean_type_node, val1, val2);
      if (t == boolean_true_node)
	return 2;

      return -2;
    }
}


/* Return 1 if VAL is inside value range VR (VR->MIN <= VAL <= VR->MAX),
          0 if VAL is not inside VR,
	 -2 if we cannot tell either way.  */

static inline int
value_inside_range (tree val, value_range *vr)
{
  int cmp1, cmp2;

  cmp1 = compare_values (val, vr->min);
  if (cmp1 == -2 || cmp1 == 2)
    return -2;

  cmp2 = compare_values (val, vr->max);
  if (cmp2 == -2 || cmp2 == 2)
    return -2;

  return (cmp1 == 0 || cmp1 == 1) && (cmp2 == -1 || cmp2 == 0);
}


/* Return true if value ranges VR0 and VR1 have a non-empty
   intersection.  */

static inline bool
value_ranges_intersect_p (value_range *vr0, value_range *vr1)
{
  return (value_inside_range (vr1->min, vr0) == 1
	  || value_inside_range (vr1->max, vr0) == 1
	  || value_inside_range (vr0->min, vr1) == 1
	  || value_inside_range (vr0->max, vr1) == 1);
}


/* Extract value range information from an ASSERT_EXPR EXPR and store
   it in *VR_P.  */

static void
extract_range_from_assert (value_range *vr_p, tree expr)
{
  tree var, cond, limit, type;
  value_range *var_vr;
  enum tree_code cond_code;

  var = ASSERT_EXPR_VAR (expr);
  cond = ASSERT_EXPR_COND (expr);
  cond_code = TREE_CODE (cond);

  gcc_assert (COMPARISON_CLASS_P (cond));

  /* Find VAR in the ASSERT_EXPR conditional.  */
  limit = get_opposite_operand (cond, var);
  type = TREE_TYPE (limit);

  gcc_assert (limit != var);

  /* For pointer arithmetic, we only keep track of anti-ranges
     (NE_EXPR).  Notice that we don't need to handle EQ_EXPR in these
     cases because assertions with equalities are never generated.
     The assert pass generates straight assignments in those cases.  */
  if (POINTER_TYPE_P (type) && cond_code != NE_EXPR)
    {
      set_value_range_to_varying (vr_p);
      return;
    }

  /* Special handling for integral types with super-types.  Some FEs
     construct integral types derived from other types and restrict
     the range of values these new types may take.

     It may happen that LIMIT is actually smaller than TYPE's minimum
     value.  For instance, the Ada FE is generating code like this
     during bootstrap:

	    D.1480_32 = nam_30 - 300000361;
	    if (D.1480_32 <= 1) goto <L112>; else goto <L52>;
	    <L112>:;
	    D.1480_94 = ASSERT_EXPR <D.1480_32, D.1480_32 <= 1>;

     All the names are of type types__name_id___XDLU_300000000__399999999
     which has min == 300000000 and max == 399999999.  This means that
     the ASSERT_EXPR would try to create the range [3000000, 1] which
     is invalid.

     The fact that the type specifies MIN and MAX values does not
     automatically mean that every variable of that type will always
     be within that range, so the predicate may well be true at run
     time.  If we had symbolic -INF and +INF values, we could
     represent this range, but we currently represent -INF and +INF
     using the type's min and max values.
	 
     So, the only sensible thing we can do for now is set the
     resulting range to VR_VARYING.  TODO, would having symbolic -INF
     and +INF values be worth the trouble?  */
  if (TREE_TYPE (type))
    {
      if (cond_code == LE_EXPR || cond_code == LT_EXPR)
	{
	  tree type_min = TYPE_MIN_VALUE (type);
	  int cmp = compare_values (limit, type_min);

	  /* For < or <= comparisons, if LIMIT is smaller than
	     TYPE_MIN, set the range to VR_VARYING.  */
	  if (cmp == -1 || cmp == 0)
	    {
	      set_value_range_to_varying (vr_p);
	      return;
	    }
	}
      else if (cond_code == GE_EXPR || cond_code == GT_EXPR)
	{
	  tree type_max = TYPE_MIN_VALUE (type);
	  int cmp = compare_values (limit, type_max);

	  /* For > or >= comparisons, if LIMIT is bigger than
	     TYPE_MAX, set the range to VR_VARYING.  */
	  if (cmp == 1 || cmp == 0)
	    {
	      set_value_range_to_varying (vr_p);
	      return;
	    }
	}
    }

  if (TREE_CODE (cond) == NE_EXPR)
    set_value_range (vr_p, VR_ANTI_RANGE, limit, limit);
  else if (TREE_CODE (cond) == LE_EXPR)
    set_value_range (vr_p, VR_RANGE, TYPE_MIN_VALUE (type), limit);
  else if (TREE_CODE (cond) == LT_EXPR)
    {
      tree one = build_int_cst (type, 1);
      set_value_range (vr_p, VR_RANGE, TYPE_MIN_VALUE (type),
		       fold (build (MINUS_EXPR, type, limit, one)));
    }
  else if (TREE_CODE (cond) == GE_EXPR)
    set_value_range (vr_p, VR_RANGE, limit, TYPE_MAX_VALUE (type));
  else if (TREE_CODE (cond) == GT_EXPR)
    {
      tree one = build_int_cst (type, 1);
      set_value_range (vr_p, VR_RANGE,
		       fold (build (PLUS_EXPR, type, limit, one)),
		       TYPE_MAX_VALUE (type));
    }
  else
    gcc_unreachable ();

  /* If VAR already has a known range and the two ranges have a
     non-empty intersection, we can refine the resulting range.
     Since the assert expression creates an equivalency and at the
     same time it asserts a predicate, we can take the intersection of
     the two ranges to get better precision.  */
  var_vr = get_value_range (var);
  if (var_vr->type == VR_RANGE
      && vr_p->type == VR_RANGE
      && value_ranges_intersect_p (var_vr, vr_p))
    {
      tree min, max;

      /* Use the larger of the two minimums.  */
      if (compare_values (vr_p->min, var_vr->min) == -1)
	min = var_vr->min;
      else
	min = vr_p->min;

      /* Use the smaller of the two maximums.  */
      if (compare_values (vr_p->max, var_vr->max) == 1)
	max = var_vr->max;
      else
	max = vr_p->max;

      set_value_range (vr_p, vr_p->type, min, max);
    }
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

static void
extract_range_from_ssa_name (value_range *vr, tree var)
{
  value_range *var_vr = get_value_range (var);

  if (var_vr->type != VR_UNDEFINED && var_vr->type != VR_VARYING)
    *vr = *var_vr;
  else
    set_value_range (vr, VR_RANGE, var, var);
}


/* Extract range information from a binary expression EXPR based on
   the ranges of each of its operands and the expression code.  */

static void
extract_range_from_binary_expr (value_range *vr, tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  tree op0, op1, min, max;
  value_range vr0, vr1;
  int cmp;

  /* Not all binary expressions can be applied to ranges in a
     meaningful way.  Handle only arithmetic operations.  */
  if (code != PLUS_EXPR
      && code != MINUS_EXPR
      && code != MULT_EXPR
      && code != TRUNC_DIV_EXPR
      && code != FLOOR_DIV_EXPR
      && code != CEIL_DIV_EXPR
      && code != EXACT_DIV_EXPR
      && code != ROUND_DIV_EXPR
      && code != MIN_EXPR
      && code != MAX_EXPR)
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* Get value ranges for each operand.  For constant operands, create
     a new value range with the operand to simplify processing.  */
  op0 = TREE_OPERAND (expr, 0);
  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = *(get_value_range (op0));
  else
    {
      if (is_gimple_min_invariant (op0))
	set_value_range (&vr0, VR_RANGE, op0, op0);
      else
	set_value_range_to_varying (&vr0);
    }

  op1 = TREE_OPERAND (expr, 1);
  if (TREE_CODE (op1) == SSA_NAME)
    vr1 = *(get_value_range (op1));
  else
    {
      if (is_gimple_min_invariant (op1))
	set_value_range (&vr1, VR_RANGE, op1, op1);
      else
	set_value_range_to_varying (&vr1);
    }

  /* If either range is UNDEFINED, so is the result.  */
  if (vr0.type == VR_UNDEFINED || vr1.type == VR_UNDEFINED)
    {
      set_value_range (vr, VR_UNDEFINED, NULL_TREE, NULL_TREE);
      return;
    }

  /* If either range is VARYING, so is the result.  */
  if (vr0.type == VR_VARYING || vr1.type == VR_VARYING)
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* If the ranges are of different types, the result is VARYING.  */
  if (vr0.type != vr1.type)
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* TODO.  Refuse to do any symbolic range operations for now.  */
  if (symbolic_range_p (&vr0) || symbolic_range_p (&vr1))
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* Now evaluate the expression to determine the new range.  */
  if (POINTER_TYPE_P (TREE_TYPE (expr))
      || POINTER_TYPE_P (TREE_TYPE (op0))
      || POINTER_TYPE_P (TREE_TYPE (op1)))
    {
      /* For pointer types, we are really only interested in asserting
	 whether the expression evaluates to non-NULL.  FIXME.  We
	 used to gcc_assert (code == PLUS_EXPR || code == MINUS_EXPR),
	 but ivopts is generating expressions with pointer
	 multiplication in them.  */
      if (code == PLUS_EXPR)
	{
	  /* Assume that pointers can never wrap around.  FIXME, Is
	     this always safe?  */
	  tree zero = build_int_cst (TREE_TYPE (expr), 0);
	  set_value_range (vr, VR_ANTI_RANGE, zero, zero);
	}
      else
	{
	  /* Subtracting from a pointer, may yield 0, so just drop the
	     resulting range to varying.  */
	  set_value_range_to_varying (vr);
	}

      return;
    }

  /* For integer ranges, apply the operation to each end of the
     range and see what we end up with.  */
  if (code == PLUS_EXPR
      || code == MULT_EXPR
      || code == MIN_EXPR
      || code == MAX_EXPR)
    {
      /* For operations that make the resulting range directly
	 proportional to the original ranges, apply the operation to
	 the same end of each range.  */
      min = int_const_binop (code, vr0.min, vr1.min, 0);
      max = int_const_binop (code, vr0.max, vr1.max, 0);
    }
  else
    {
      /* For operations that make the resulting range inversely
	 proportional to the original ranges (-, /), apply the
	 operation to the opposite ends of each range.  */
      min = int_const_binop (code, vr0.min, vr1.max, 0);
      max = int_const_binop (code, vr0.max, vr1.min, 0);
    }

  cmp = compare_values (min, max);
  if (cmp == -2 || cmp == 1)
    {
      /* If the new range has its limits swapped around (MIN > MAX),
	 then the operation caused one of them to wrap around, mark
	 the new range VARYING.  */
      set_value_range_to_varying (vr);
    }
  else
    set_value_range (vr, vr0.type, min, max);
}


/* Like expr_computes_nonzero, but this function uses value ranges
   obtained so far.  */

static bool
vrp_expr_computes_nonzero (tree expr)
{
  if (expr_computes_nonzero (expr))
    return true;

  /* If we have an expression of the form &X->a, then the expression
     is nonnull if X is nonnull.  */
  if (TREE_CODE (expr) == ADDR_EXPR)
    {
      tree base = get_base_address (TREE_OPERAND (expr, 0));

      if (base != NULL_TREE
	  && TREE_CODE (base) == INDIRECT_REF
	  && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME)
	{
	  value_range *vr = get_value_range (TREE_OPERAND (base, 0));
	  if (range_is_nonnull (vr))
	    return true;
	}
    }

  return false;
}


/* Extract range information from a unary expression EXPR based on
   the range of its operand and the expression code.  */

static void
extract_range_from_unary_expr (value_range *vr, tree expr)
{
  enum tree_code code = TREE_CODE (expr);
  tree min, max, op0;
  value_range vr0;
  int cmp;

  /* Get value ranges for the operand.  For constant operands, create
     a new value range with the operand to simplify processing.  */
  op0 = TREE_OPERAND (expr, 0);
  if (TREE_CODE (op0) == SSA_NAME)
    vr0 = *(get_value_range (op0));
  else
    {
      if (is_gimple_min_invariant (op0))
	set_value_range (&vr0, VR_RANGE, op0, op0);
      else
	set_value_range_to_varying (&vr0);
    }

  /* If VR0 is UNDEFINED, so is the result.  */
  if (vr0.type == VR_UNDEFINED)
    {
      set_value_range (vr, VR_UNDEFINED, NULL_TREE, NULL_TREE);
      return;
    }

  /* If VR0 is VARYING, so is the result.  */
  if (vr0.type == VR_VARYING)
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* TODO.  Refuse to do any symbolic range operations for now.  */
  if (symbolic_range_p (&vr0))
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* If the operand is neither a pointer nor an integral type, set the
     range to VARYING.  TODO, we may set the range to non-zero.  */
  if (!INTEGRAL_TYPE_P (TREE_TYPE (op0))
      && !POINTER_TYPE_P (TREE_TYPE (op0)))
    {
      set_value_range_to_varying (vr);
      return;
    }

  /* If the expression involves pointers, we are only interested in
     determining if it evaluates to NULL [0, 0] or non-NULL (~[0, 0]).  */
  if (POINTER_TYPE_P (TREE_TYPE (expr)) || POINTER_TYPE_P (TREE_TYPE (op0)))
    {
      if (range_is_nonnull (&vr0) || expr_computes_nonzero (expr))
	set_value_range_to_nonnull (vr, TREE_TYPE (expr));
      else if (range_is_null (&vr0))
	set_value_range_to_null (vr, TREE_TYPE (expr));
      else
	set_value_range_to_varying (vr);

      return;
    }

  /* Handle unary expressions on integer ranges.  */
  if ((code == NOP_EXPR || code == CONVERT_EXPR)
      && (TYPE_SIZE (TREE_TYPE (vr0.min)) != TYPE_SIZE (TREE_TYPE (expr))))
    {
      /* When converting types of different sizes, set the result to
	 VARYING.  Things like sign extensions and precision loss may
	 change the range.  For instance, if x_3 is of type 'long long
	 int' and 'y_5 = (unsigned short) x_3', if x_3 is ~[0, 0], it
	 is impossible to know at compile time whether y_5 will be
	 ~[0, 0].  */
      set_value_range_to_varying (vr);
      return;
    }

  /* Apply the operation to each end of the range and see what we end
     up with.  */
  min = fold_unary_to_constant (code, TREE_TYPE (expr), vr0.min);
  max = fold_unary_to_constant (code, TREE_TYPE (expr), vr0.max);

  cmp = compare_values (min, max);
  if (cmp == -2 || cmp == 1)
    {
      /* If the new range has its limits swapped around (MIN > MAX),
	 then the operation caused one of them to wrap around, mark
	 the new range VARYING.  */
      set_value_range_to_varying (vr);
    }
  else
    set_value_range (vr, vr0.type, min, max);
}


/* Try to compute a useful range out of expression EXPR and store it
   in *VR_P.  */

static void
extract_range_from_expr (value_range *vr, tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  if (code == ASSERT_EXPR)
    extract_range_from_assert (vr, expr);
  else if (code == SSA_NAME)
    extract_range_from_ssa_name (vr, expr);
  else if (TREE_CODE_CLASS (code) == tcc_binary)
    extract_range_from_binary_expr (vr, expr);
  else if (TREE_CODE_CLASS (code) == tcc_unary)
    extract_range_from_unary_expr (vr, expr);
  else if (vrp_expr_computes_nonzero (expr))
    set_value_range_to_nonnull (vr, TREE_TYPE (expr));
  else if (TREE_CODE (expr) == INTEGER_CST)
    set_value_range (vr, VR_RANGE, expr, expr);
  else
    set_value_range_to_varying (vr);
}


/* Given a range VR, a loop L and a variable VAR, determine whether it
   would be profitable to adjust VR using scalar evolution information
   for VAR.  If so, update VR with the new limits.  */

static void
adjust_range_with_scev (value_range *vr, struct loop *l, tree var)
{
  tree init, step, chrec;
  bool init_is_max;

  /* TODO.  Don't adjust anti-ranges.  An anti-range may provide
     better opportunities than a regular range, but I'm not sure.  */
  if (vr->type == VR_ANTI_RANGE)
    return;

  chrec = analyze_scalar_evolution (l, var);
  if (TREE_CODE (chrec) != POLYNOMIAL_CHREC)
    return;

  init = CHREC_LEFT (chrec);
  step = CHREC_RIGHT (chrec);

  /* If STEP is symbolic, we can't know whether INIT will be the
     minimum or maximum value in the range.  */
  if (!is_gimple_min_invariant (step))
    return;

  /* FIXME.  When dealing with unsigned types,
     analyze_scalar_evolution sets STEP to very large unsigned values
     when the evolution goes backwards.  This confuses this analysis
     because we think that INIT is the smallest value that the range
     can take, instead of the largest.  Ignore these chrecs for now.  */
  if (INTEGRAL_TYPE_P (TREE_TYPE (step)) && TYPE_UNSIGNED (TREE_TYPE (step)))
    return;

  /* If STEP is negative, then INIT is the maximum value the range
     will take.  Otherwise, INIT is the minimum value.  */
  init_is_max = (tree_int_cst_sgn (step) < 0);

  if (!POINTER_TYPE_P (TREE_TYPE (init))
      && (vr->type == VR_VARYING || vr->type == VR_UNDEFINED))
    {
      /* For VARYING or UNDEFINED ranges, just about anything we get
	 from scalar evolutions should be better.  */
      if (init_is_max)
	set_value_range (vr, VR_RANGE, TYPE_MIN_VALUE (TREE_TYPE (init)), init);
      else
	set_value_range (vr, VR_RANGE, init, TYPE_MAX_VALUE (TREE_TYPE (init)));
    }
  else if (vr->type == VR_RANGE)
    {
      tree min = vr->min;
      tree max = vr->max;

      if (init_is_max)
	{
	  /* INIT is the maximum value.  If INIT is lower than VR->MAX
	     but no smaller than VR->MIN, set VR->MAX to INIT.  */
	  if (compare_values (init, max) == -1)
	    {
	      max = init;

	      /* If we just created an invalid range with the minimum
		 greater than the maximum, take the minimum all the
		 way to -INF.  */
	      if (compare_values (min, max) == 1)
		min = TYPE_MIN_VALUE (TREE_TYPE (min));
	    }
	}
      else
	{
	  /* If INIT is bigger than VR->MIN, set VR->MIN to INIT.  */
	  if (compare_values (init, min) == 1)
	    {
	      min = init;

	      /* If we just created an invalid range with the minimum
		 greater than the maximum, take the maximum all the
		 way to +INF.  */
	      if (compare_values (min, max) == 1)
		max = TYPE_MAX_VALUE (TREE_TYPE (max));
	    }
	}

      set_value_range (vr, VR_RANGE, min, max);
    }
}


/* Given two numeric value ranges VR0, VR1 and a comparison code COMP:
   
   - Return BOOLEAN_TRUE_NODE if VR0 COMP VR1 always returns true for all the
     values in the ranges.

   - Return BOOLEAN_FALSE_NODE if the comparison always returns false.

   - Return NULL_TREE if it is not always possible to determine the value of
     the comparison.  */

static tree
compare_ranges (enum tree_code comp, value_range *vr0, value_range *vr1)
{
  /* VARYING or UNDEFINED ranges cannot be compared.  */
  if (vr0->type == VR_VARYING
      || vr0->type == VR_UNDEFINED
      || vr1->type == VR_VARYING
      || vr1->type == VR_UNDEFINED)
    return NULL_TREE;

  /* Anti-ranges need to be handled separately.  */
  if (vr0->type == VR_ANTI_RANGE || vr1->type == VR_ANTI_RANGE)
    {
      /* If both are anti-ranges, then we cannot compute any
	 comparison.  */
      if (vr0->type == VR_ANTI_RANGE && vr1->type == VR_ANTI_RANGE)
	return NULL_TREE;

      /* These comparisons are never statically computable.  */
      if (comp == GT_EXPR
	  || comp == GE_EXPR
	  || comp == LT_EXPR
	  || comp == LE_EXPR)
	return NULL_TREE;

      /* Equality can be computed only between a range and an
	 anti-range.  ~[VAL1, VAL2] == [VAL1, VAL2] is always false.  */
      if (vr0->type == VR_RANGE)
	{
	  /* To simplify processing, make VR0 the anti-range.  */
	  value_range *tmp = vr0;
	  vr0 = vr1;
	  vr1 = tmp;
	}

      gcc_assert (comp == NE_EXPR || comp == EQ_EXPR);

      if (compare_values (vr0->min, vr1->min) == 0
	  && compare_values (vr0->max, vr1->max) == 0)
	return (comp == NE_EXPR) ? boolean_true_node : boolean_false_node;

      return NULL_TREE;
    }

  /* Simplify processing.  If COMP is GT_EXPR or GE_EXPR, switch the
     operands around and change the comparison code.  */
  if (comp == GT_EXPR || comp == GE_EXPR)
    {
      value_range *tmp;
      comp = (comp == GT_EXPR) ? LT_EXPR : LE_EXPR;
      tmp = vr0;
      vr0 = vr1;
      vr1 = tmp;
    }

  if (comp == EQ_EXPR)
    {
      /* Equality may only be computed if both ranges represent
	 exactly one value.  */
      if (compare_values (vr0->min, vr0->max) == 0
	  && compare_values (vr1->min, vr1->max) == 0)
	{
	  int cmp_min = compare_values (vr0->min, vr1->min);
	  int cmp_max = compare_values (vr0->max, vr1->max);
	  if (cmp_min == 0 && cmp_max == 0)
	    return boolean_true_node;
	  else if (cmp_min != -2 && cmp_max != -2)
	    return boolean_false_node;
	}

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
      cmp1 = compare_values (vr0->max, vr1->min);
      cmp2 = compare_values (vr0->min, vr1->max);
      if ((cmp1 == -1 && cmp2 == -1) || (cmp1 == 1 && cmp2 == 1))
	return boolean_true_node;

      /* If VR0 and VR1 represent a single value and are identical,
	 return false.  */
      else if (compare_values (vr0->min, vr0->max) == 0
	       && compare_values (vr1->min, vr1->max) == 0
	       && compare_values (vr0->min, vr1->min) == 0
	       && compare_values (vr0->max, vr1->max) == 0)
	return boolean_false_node;

      /* Otherwise, they may or may not be different.  */
      else
	return NULL_TREE;
    }
  else if (comp == LT_EXPR || comp == LE_EXPR)
    {
      int tst;

      /* If VR0 is to the left of VR1, return true.  */
      tst = compare_values (vr0->max, vr1->min);
      if ((comp == LT_EXPR && tst == -1)
	  || (comp == LE_EXPR && (tst == -1 || tst == 0)))
	return boolean_true_node;

      /* If VR0 is to the right of VR1, return false.  */
      tst = compare_values (vr0->min, vr1->max);
      if ((comp == LT_EXPR && (tst == 0 || tst == 1))
	  || (comp == LE_EXPR && tst == 1))
	return boolean_false_node;

      /* Otherwise, we don't know.  */
      return NULL_TREE;
    }
    
  gcc_unreachable ();
}


/* Given a value range VR, a value VAL and a comparison code COMP, return
   BOOLEAN_TRUE_NODE if VR COMP VR1 always returns true for all the
   values in VR.  Return BOOLEAN_FALSE_NODE if the comparison
   always returns false.  Return NULL_TREE if it is not always
   possible to determine the value of the comparison.  */

static tree
compare_range_with_value (enum tree_code comp, value_range *vr, tree val)
{
  if (vr->type == VR_VARYING || vr->type == VR_UNDEFINED)
    return NULL_TREE;

  /* Anti-ranges need to be handled separately.  */
  if (vr->type == VR_ANTI_RANGE)
    {
      /* For anti-ranges, the only predicates that we can compute at
	 compile time are equality and inequality.  */
      if (comp == GT_EXPR
	  || comp == GE_EXPR
	  || comp == LT_EXPR
	  || comp == LE_EXPR)
	return NULL_TREE;

      /* ~[VAL, VAL] == VAL is always false.  */
      if (compare_values (vr->min, val) == 0
	  && compare_values (vr->max, val) == 0)
	return (comp == NE_EXPR) ? boolean_true_node : boolean_false_node;

      return NULL_TREE;
    }

  if (comp == EQ_EXPR)
    {
      /* EQ_EXPR may only be computed if VR represents exactly
	 one value.  */
      if (compare_values (vr->min, vr->max) == 0)
	{
	  int cmp = compare_values (vr->min, val);
	  if (cmp == 0)
	    return boolean_true_node;
	  else if (cmp == -1 || cmp == 1 || cmp == 2)
	    return boolean_false_node;
	}
      else if (compare_values (val, vr->min) == -1
	       || compare_values (vr->max, val) == -1)
	return boolean_false_node;

      return NULL_TREE;
    }
  else if (comp == NE_EXPR)
    {
      /* If VAL is not inside VR, then they are always different.  */
      if (compare_values (vr->max, val) == -1
	  || compare_values (vr->min, val) == 1)
	return boolean_true_node;

      /* If VR represents exactly one value equal to VAL, then return
	 false.  */
      if (compare_values (vr->min, vr->max) == 0
	  && compare_values (vr->min, val) == 0)
	return boolean_false_node;

      /* Otherwise, they may or may not be different.  */
      return NULL_TREE;
    }
  else if (comp == LT_EXPR || comp == LE_EXPR)
    {
      int tst;

      /* If VR is to the left of VAL, return true.  */
      tst = compare_values (vr->max, val);
      if ((comp == LT_EXPR && tst == -1)
	  || (comp == LE_EXPR && (tst == -1 || tst == 0)))
	return boolean_true_node;

      /* If VR is to the right of VAL, return false.  */
      tst = compare_values (vr->min, val);
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
      tst = compare_values (vr->min, val);
      if ((comp == GT_EXPR && tst == 1)
	  || (comp == GE_EXPR && (tst == 0 || tst == 1)))
	return boolean_true_node;

      /* If VR is to the left of VAL, return false.  */
      tst = compare_values (vr->max, val);
      if ((comp == GT_EXPR && (tst == -1 || tst == 0))
	  || (comp == GE_EXPR && tst == -1))
	return boolean_false_node;

      /* Otherwise, we don't know.  */
      return NULL_TREE;
    }

  gcc_unreachable ();
}


/* Debugging dumps.  */

void
dump_value_range (FILE *file, value_range *vr)
{
  if (vr == NULL)
    fprintf (file, "[]");
  else if (vr->type == VR_UNDEFINED)
    fprintf (file, "UNDEFINED");
  else if (vr->type == VR_RANGE || vr->type == VR_ANTI_RANGE)
    {
      fprintf (file, "%s[", (vr->type == VR_ANTI_RANGE) ? "~" : "");
      print_generic_expr (file, vr->min, 0);
      fprintf (file, ", ");
      print_generic_expr (file, vr->max, 0);
      fprintf (file, "]");
    }
  else if (vr->type == VR_VARYING)
    fprintf (file, "VARYING");
  else
    fprintf (file, "INVALID RANGE");
}


/* Dump value range VR to stderr.  */

void
debug_value_range (value_range *vr)
{
  dump_value_range (stderr, vr);
}


/* Dump value ranges of all SSA_NAMEs to FILE.  */

void
dump_all_value_ranges (FILE *file)
{
  size_t i;

  for (i = 0; i < num_ssa_names; i++)
    {
      tree var = ssa_name (i);
      if (var && SSA_NAME_VALUE_RANGE (var))
	{
	  print_generic_expr (file, var, 0);
	  fprintf (file, ": ");
	  dump_value_range (file, SSA_NAME_VALUE_RANGE (var));
	  fprintf (file, "\n");
	}
    }

  fprintf (file, "\n");
}


/* Dump all value ranges to stderr.  */

void
debug_all_value_ranges (void)
{
  dump_all_value_ranges (stderr);
}


/*---------------------------------------------------------------------------
			    Value Range Propagation
---------------------------------------------------------------------------*/

/* Given a COND_EXPR COND of the form 'V OP W', and an SSA name V,
   create a new SSA name N and return the assertion assignment
   'V = ASSERT_EXPR <V, V OP W>'.  */

static tree
build_assert_expr_for (tree cond, tree v)
{
  tree n, assertion;

  gcc_assert (TREE_CODE (v) == SSA_NAME);
  n = duplicate_ssa_name (v, NULL_TREE);

  if (COMPARISON_CLASS_P (cond))
    {
      /* Build N = ASSERT_EXPR <V, COND>.  As a special case, if the
	 conditional is an EQ_EXPR (V == Z), just build the assignment
	 N = Z.  */
      if (TREE_CODE (cond) == EQ_EXPR)
	{
	  tree other = get_opposite_operand (cond, v);
	  assertion = build (MODIFY_EXPR, TREE_TYPE (v), n, other);
	}
      else
	assertion = build (MODIFY_EXPR, TREE_TYPE (v), n,
	                   build (ASSERT_EXPR, TREE_TYPE (v), v, cond));
    }
  else if (TREE_CODE (cond) == TRUTH_NOT_EXPR)
    {
      /* Given !V, build the assignment N = false.  */
      tree op0 = TREE_OPERAND (cond, 0);
      gcc_assert (op0 == v);
      assertion = build (MODIFY_EXPR, TREE_TYPE (v), n, boolean_false_node);
    }
  else if (TREE_CODE (cond) == SSA_NAME)
    {
      /* Given V, build the assignment N = true.  */
      gcc_assert (v == cond);
      assertion = build (MODIFY_EXPR, TREE_TYPE (v), n, boolean_true_node);
    }
  else
    gcc_unreachable ();

  SSA_NAME_DEF_STMT (n) = assertion;

  /* The new ASSERT_EXPR, creates a new SSA name that replaces the
     operand of the ASSERT_EXPR. Register the new name and the old one
     in the replacement table so that we can fix the SSA web after
     adding all the ASSERT_EXPRs.  */
  register_new_name_mapping (n, v);

  return assertion;
}


/* Return false if EXPR is a predicate expression involving floating
   point values.  */

static inline bool
fp_predicate (tree expr)
{
  return (COMPARISON_CLASS_P (expr)
	  && FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (expr, 0))));
}


/* Return an expression predicate that represents the range of values
   that can be taken by operand OP after STMT executes.  */

static tree
infer_value_range (tree stmt, tree op)
{
  /* Do not attempt to infer anything in names that flow through
     abnormal edges.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
    return NULL_TREE;

  if (POINTER_TYPE_P (TREE_TYPE (op)))
    {
      bool is_store;
      unsigned num_uses, num_derefs;

      count_uses_and_derefs (op, stmt, &num_uses, &num_derefs, &is_store);
      if (num_derefs > 0 && flag_delete_null_pointer_checks)
	{
	  /* We can only assume that a pointer dereference will yield
	     non-NULL if -fdelete-null-pointer-checks is enabled.  */
	  tree null = build_int_cst (TREE_TYPE (op), 0);
	  tree t = build (NE_EXPR, boolean_type_node, op, null);
	  return t;
	}
    }

  return NULL_TREE;
}


/* Return true if OP is the result of an ASSERT_EXPR that tests the
   same condition as COND.  */

static bool
has_assert_expr (tree op, tree cond)
{
  tree def_stmt = SSA_NAME_DEF_STMT (op);
  tree assert_expr, other_cond, other_op;

  /* If OP was not generated by an ASSERT_EXPR, return false.  */
  if (TREE_CODE (def_stmt) != MODIFY_EXPR
      || TREE_CODE (TREE_OPERAND (def_stmt, 1)) != ASSERT_EXPR)
    return false;

  assert_expr = TREE_OPERAND (def_stmt, 1);
  other_cond = ASSERT_EXPR_COND (assert_expr);
  other_op = ASSERT_EXPR_VAR (assert_expr);

  if (TREE_CODE (cond) == TREE_CODE (other_cond))
    {
      tree t1, t2;

      /* If COND is not a comparison predicate, something is wrong.  */
      gcc_assert (COMPARISON_CLASS_P (cond));

      /* Note that we only need to compare against one of the operands
	 of OTHER_COND.  
	 
	 Suppose that we are about to insert the assertion ASSERT_EXPR
	 <x_4, x_4 != 0> and the defining statement for x_4 is x_4 =
	 ASSERT_EXPR <x_3, x_3 != 0>.

	 In this case, we don't really want to insert a new
	 ASSERT_EXPR for x_4 because that would be redundant.  We
	 already know that x_4 is not 0.  So, when comparing the
	 conditionals 'x_3 != 0' and 'x_4 != 0', we don't want to
	 compare x_3 and x_4, we just want to compare the predicate's
	 code (!=) and the other operand (0).  */
      if (TREE_OPERAND (cond, 0) == op)
	t1 = TREE_OPERAND (cond, 1);
      else
	t1 = TREE_OPERAND (cond, 0);

      if (TREE_OPERAND (other_cond, 0) == other_op)
	t2 = TREE_OPERAND (other_cond, 1);
      else
	t2 = TREE_OPERAND (other_cond, 0);

      return (t1 == t2 || operand_equal_p (t1, t2, 0));
    }

  return false;
}


/* Traverse all the statements in block BB looking for used variables.
   Variables used in BB are added to bitmap FOUND.  The algorithm
   works in three main parts:

   1- For every statement S in BB, all the variables used by S are
      added to bitmap FOUND.

   2- If statement S uses an operand N in a way that exposes a known
      value range for N, then if N was not already generated by an
      ASSERT_EXPR, create a new ASSERT_EXPR for N.  For instance, if N
      is a pointer and the statement dereferences it, we can assume
      that N is not NULL.

   3- COND_EXPRs are a special case of #2.  We can derive range
      information from the predicate but need to insert different
      ASSERT_EXPRs for each of the sub-graphs rooted at the
      conditional block.  If the last statement of BB is a conditional
      expression of the form 'X op Y', then

      a) Remove X and Y from the set FOUND.

      b) If the conditional dominates its THEN_CLAUSE sub-graph,
	 recurse into it.  On return, if X and/or Y are marked in
	 FOUND, then an ASSERT_EXPR is added for the corresponding
	 variable.

      c) Repeat step (b) on the ELSE_CLAUSE.

      d) Mark X and Y in FOUND.

   4- If BB does not end in a conditional expression, then we recurse
      into BB's dominator children.
   
   At the end of the recursive traversal, ASSERT_EXPRs will have been
   added to the edges of COND_EXPR blocks that have sub-graphs using
   one or both predicate operands.  For instance,

   	if (a == 9)
	  b = a;
	else
	  b = c + 1;

   In this case, an assertion on the THEN clause is useful to
   determine that 'a' is always 9 on that edge.  However, an assertion
   on the ELSE clause would be unnecessary.

   On exit from this function, all the names created by the newly
   inserted ASSERT_EXPRs need to be added to the SSA web by rewriting
   the SSA names that they replace.
   
   TODO.  Handle SWITCH_EXPR.  */

static bool
maybe_add_assert_expr (basic_block bb)
{
  block_stmt_iterator si;
  tree last;
  bool added;

  /* Step 1.  Mark all the SSA names used in BB in bitmap FOUND.  */
  added = false;
  last = NULL_TREE;
  for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
    {
      tree stmt, op;
      ssa_op_iter i;
      
      stmt = bsi_stmt (si);

      /* Mark all the SSA names used by STMT in bitmap FOUND.  If STMT
	 is inside the sub-graph of a conditional block, when we
	 return from this recursive walk, our parent will use the
	 FOUND bitset to determine if one of the operands it was
	 looking for was present in the sub-graph.  */
      FOR_EACH_SSA_TREE_OPERAND (op, stmt, i, SSA_OP_USE)
	{
	  tree cond;

	  /* If OP is used only once, namely in this STMT, don't
	     bother inserting an ASSERT_EXPR for it.  Such an
	     ASSERT_EXPR would do nothing but increase compile time.
	     Experiments show that with this simple check, we can save
	     more than 20% of ASSERT_EXPRs.  */
	  if (has_single_use (op))
	    continue;

	  SET_BIT (found, SSA_NAME_VERSION (op));

	  cond = infer_value_range (stmt, op);
	  if (!cond)
	    continue;

	  /* Step 2.  If OP is used in such a way that we can infer a
	     value range for it, create a new ASSERT_EXPR for OP
	     (unless OP already has an ASSERT_EXPR).  */
	  gcc_assert (!is_ctrl_stmt (stmt));

	  if (has_assert_expr (op, cond))
	    continue;

	  if (!stmt_ends_bb_p (stmt))
	    {
	      /* If STMT does not end the block, we can insert the new
		 assertion right after it.  */
	      tree t = build_assert_expr_for (cond, op);
	      bsi_insert_after (&si, t, BSI_NEW_STMT);
	      added = true;
	    }
	  else
	    {
	      /* STMT must be the last statement in BB.  We can only
		 insert new assertions on the non-abnormal edge out of
		 BB.  Note that since STMT is not control flow, there
		 may only be one non-abnormal edge out of BB.  */
	      edge_iterator ei;
	      edge e;

	      FOR_EACH_EDGE (e, ei, bb->succs)
		if (!(e->flags & EDGE_ABNORMAL))
		  {
		    tree t = build_assert_expr_for (cond, op);
		    bsi_insert_on_edge (e, t);
		    added = true;
		    break;
		  }
	    }
	}

      /* Remember the last statement of the block.  */
      last = stmt;
    }

  /* Step 3.  If BB's last statement is a conditional expression
     involving integer operands, recurse into each of the sub-graphs
     rooted at BB to determine if we need to add ASSERT_EXPRs.
     Notice that we only care about the first operand of the
     conditional.  Adding assertions for both operands may actually 
     hinder VRP.  FIXME, add example.  */
  if (last
      && TREE_CODE (last) == COND_EXPR
      && !fp_predicate (COND_EXPR_COND (last))
      && !ZERO_SSA_OPERANDS (last, SSA_OP_USE))
    {
      edge e;
      edge_iterator ei;
      tree op, cond;
      basic_block son;
      ssa_op_iter iter;
      
      cond = COND_EXPR_COND (last);

      /* Get just the first use operand.  */
      FOR_EACH_SSA_TREE_OPERAND (op, last, iter, SSA_OP_USE)
	break;
      gcc_assert (op != NULL);

      /* Do not attempt to infer anything in names that flow through
	 abnormal edges.  */
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
	return false;

      /* Remove the COND_EXPR operand from the FOUND bitmap.
	 Otherwise, when we finish traversing each of the sub-graphs,
	 we won't know whether the variables were found in the
	 sub-graphs or if they had been found in a block upstream from
	 BB.  */
      RESET_BIT (found, SSA_NAME_VERSION (op));

      /* Look for uses of the operands in each of the sub-graphs
	 rooted at BB.  We need to check each of the outgoing edges
	 separately, so that we know what kind of ASSERT_EXPR to
	 insert.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  /* If BB strictly dominates the sub-graph at E->DEST,
	     recurse into it.  */
	  if (e->dest != bb
	      && dominated_by_p (CDI_DOMINATORS, e->dest, bb))
	    added |= maybe_add_assert_expr (e->dest);

	  /* Once we traversed the sub-graph, check if any block inside
	     used either of the predicate's operands.  If so, add the
	     appropriate ASSERT_EXPR.  */
	  if (TEST_BIT (found, SSA_NAME_VERSION (op)))
	    {
	      /* We found a use of OP in the sub-graph rooted at
		 E->DEST.  Add an ASSERT_EXPR according to whether
		 E goes to THEN_CLAUSE or ELSE_CLAUSE.  */
	      tree c, t;

	      if (e->flags & EDGE_TRUE_VALUE)
		c = unshare_expr (cond);
	      else if (e->flags & EDGE_FALSE_VALUE)
		c = invert_truthvalue (cond);
	      else
		gcc_unreachable ();

	      t = build_assert_expr_for (c, op);
	      bsi_insert_on_edge (e, t);
	      added = true;
	    }
	}

      /* Finally, mark all the COND_EXPR operands as found.  */
      SET_BIT (found, SSA_NAME_VERSION (op));

      /* Recurse into the dominator children of BB that are not BB's
	 immediate successors.  Note that we have already visited BB's
	 other dominator children above.  */
      for (son = first_dom_son (CDI_DOMINATORS, bb);
	   son;
	   son = next_dom_son (CDI_DOMINATORS, son))
	{
	  if (find_edge (bb, son) == NULL)
	    added |= maybe_add_assert_expr (son);
	}
    }
  else
    {
      /* Step 4.  Recurse into the dominator children of BB.  */
      basic_block son;

      for (son = first_dom_son (CDI_DOMINATORS, bb);
	   son;
	   son = next_dom_son (CDI_DOMINATORS, son))
	added |= maybe_add_assert_expr (son);
    }

  return added;
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
      y = ASSERT_EXPR <y, x <= y>
      x = y + 3
    }

   The idea is that once copy and constant propagation have run, other
   optimizations will be able to determine what ranges of values can 'x'
   take in different paths of the code, simply by checking the reaching
   definition of 'x'.  */

static void
insert_range_assertions (void)
{
  edge e;
  edge_iterator ei;
  bool update_ssa_p;
  
  found = sbitmap_alloc (num_ssa_names);
  sbitmap_zero (found);

  calculate_dominance_info (CDI_DOMINATORS);

  update_ssa_p = false;
  FOR_EACH_EDGE (e, ei, ENTRY_BLOCK_PTR->succs)
    if (maybe_add_assert_expr (e->dest))
      update_ssa_p = true;

  if (update_ssa_p)
    {
      bsi_commit_edge_inserts ();
      update_ssa (TODO_update_ssa_no_phi);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nSSA form after inserting ASSERT_EXPRs\n");
      dump_function_to_file (current_function_decl, dump_file, dump_flags);
    }

  sbitmap_free (found);
}


/* Convert range assertion expressions into copies.  FIXME, explain why.  */

static void
remove_range_assertions (void)
{
  basic_block bb;
  block_stmt_iterator si;

  FOR_EACH_BB (bb)
    for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
      {
	tree stmt = bsi_stmt (si);

	if (TREE_CODE (stmt) == MODIFY_EXPR
	    && TREE_CODE (TREE_OPERAND (stmt, 1)) == ASSERT_EXPR)
	  {
	    tree rhs = TREE_OPERAND (stmt, 1);
	    tree cond = fold (ASSERT_EXPR_COND (rhs));
	    gcc_assert (cond != boolean_false_node);
	    TREE_OPERAND (stmt, 1) = ASSERT_EXPR_VAR (rhs);
	    update_stmt (stmt);
	  }
      }
}


/* Return true if STMT is interesting for VRP.  */

static bool
stmt_interesting_for_vrp (tree stmt)
{
  if (TREE_CODE (stmt) == PHI_NODE
      && is_gimple_reg (PHI_RESULT (stmt))
      && (INTEGRAL_TYPE_P (TREE_TYPE (PHI_RESULT (stmt)))
	  || POINTER_TYPE_P (TREE_TYPE (PHI_RESULT (stmt)))))
    return true;
  else if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      tree lhs = TREE_OPERAND (stmt, 0);

      if (TREE_CODE (lhs) == SSA_NAME
	  && (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	      || POINTER_TYPE_P (TREE_TYPE (lhs)))
	  && ZERO_SSA_OPERANDS (stmt, SSA_OP_ALL_VIRTUALS))
	return true;
    }
  else if (TREE_CODE (stmt) == COND_EXPR || TREE_CODE (stmt) == SWITCH_EXPR)
    return true;

  return false;
}


/* Initialize local data structures for VRP.  Return true if VRP
   is worth running (i.e. if we found any statements that could
   benefit from range information).  */

static bool
vrp_initialize (void)
{
  basic_block bb;
  bool do_vrp;

  /* If we don't find any ASSERT_EXPRs in the code, there's no point
     running VRP.  */
  do_vrp = false;

  FOR_EACH_BB (bb)
    {
      block_stmt_iterator si;
      tree phi;

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  if (!stmt_interesting_for_vrp (phi))
	    {
	      tree lhs = PHI_RESULT (phi);
	      set_value_range_to_varying (get_value_range (lhs));
	      DONT_SIMULATE_AGAIN (phi) = true;
	    }
	  else
	    DONT_SIMULATE_AGAIN (phi) = false;
	}

      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
        {
	  tree stmt = bsi_stmt (si);

	  if (!stmt_interesting_for_vrp (stmt))
	    {
	      ssa_op_iter i;
	      tree def;
	      FOR_EACH_SSA_TREE_OPERAND (def, stmt, i, SSA_OP_DEF)
		set_value_range_to_varying (get_value_range (def));
	      DONT_SIMULATE_AGAIN (stmt) = true;
	    }
	  else
	    {
	      if (TREE_CODE (stmt) == MODIFY_EXPR
	           && TREE_CODE (TREE_OPERAND (stmt, 1)) == ASSERT_EXPR)
		do_vrp = true;

	      DONT_SIMULATE_AGAIN (stmt) = false;
	    }
	}
    }

  return do_vrp;
}


/* Visit assignment STMT.  If it produces an interesting range, record
   the SSA name in *OUTPUT_P.  */

static enum ssa_prop_result
vrp_visit_assignment (tree stmt, tree *output_p)
{
  tree lhs, rhs, def;
  ssa_op_iter iter;

  lhs = TREE_OPERAND (stmt, 0);
  rhs = TREE_OPERAND (stmt, 1);

  /* We only keep track of ranges in integral and pointer types.  */
  if (TREE_CODE (lhs) == SSA_NAME
      && (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	  || POINTER_TYPE_P (TREE_TYPE (lhs))))
    {
      value_range *vr, new_vr;
      struct loop *l;
      
      vr = get_value_range (lhs);
      extract_range_from_expr (&new_vr, rhs);

      /* If STMT is inside a loop, we may be able to know something
	 else about the range of LHS by examining scalar evolution
	 information.  */
      if (cfg_loops && (l = loop_containing_stmt (stmt)))
	adjust_range_with_scev (&new_vr, l, lhs);

      if (update_value_range (vr, new_vr.type, new_vr.min, new_vr.max))
	{
	  *output_p = lhs;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Found new range ");
	      dump_value_range (dump_file, &new_vr);
	      fprintf (dump_file, " for ");
	      print_generic_expr (dump_file, lhs, 0);
	      fprintf (dump_file, "\n\n");
	    }

	  if (new_vr.type == VR_VARYING)
	    return SSA_PROP_VARYING;

	  return SSA_PROP_INTERESTING;
	}

      return SSA_PROP_NOT_INTERESTING;
    }
  
  /* Every other statements produces no useful ranges.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
    set_value_range_to_varying (get_value_range (def));

  return SSA_PROP_VARYING;
}


/* Given a conditional predicate COND, try to determine if COND yields
   true or false based on the value ranges of its operands.  */

static tree
vrp_evaluate_conditional (tree cond)
{
  gcc_assert (TREE_CODE (cond) == SSA_NAME
              || TREE_CODE_CLASS (TREE_CODE (cond)) == tcc_comparison);

  if (TREE_CODE (cond) == SSA_NAME)
    {
      /* For SSA names, only return a truth value if the range is
	 known and contains exactly one value.  */
      value_range *vr = SSA_NAME_VALUE_RANGE (cond);
      if (vr && vr->type == VR_RANGE && vr->min == vr->max)
	return vr->min;
    }
  else
    {
      /* For comparisons, evaluate each operand and compare their
	 ranges.  */
      tree op0, op1;
      value_range *vr0, *vr1;

      op0 = TREE_OPERAND (cond, 0);
      vr0 = (TREE_CODE (op0) == SSA_NAME) ? get_value_range (op0) : NULL;

      op1 = TREE_OPERAND (cond, 1);
      vr1 = (TREE_CODE (op1) == SSA_NAME) ? get_value_range (op1) : NULL;

      if (vr0 && vr1)
	return compare_ranges (TREE_CODE (cond), vr0, vr1);
      else if (vr0 && vr1 == NULL)
	return compare_range_with_value (TREE_CODE (cond), vr0, op1);
      else if (vr0 == NULL && vr1)
	return compare_range_with_value (opposite_comparison (TREE_CODE (cond)),
					 vr1, op0);
    }

  /* Anything else cannot be computed statically.  */
  return NULL_TREE;
}


/* Visit conditional statement STMT.  If we can determine which edge
   will be taken out of STMT's basic block, record it in
   *TAKEN_EDGE_P and return SSA_PROP_INTERESTING.  Otherwise, return
   SSA_PROP_VARYING.  */

static enum ssa_prop_result
vrp_visit_cond_stmt (tree stmt, edge *taken_edge_p)
{
  tree cond, val;

  *taken_edge_p = NULL;

  /* FIXME.  Handle SWITCH_EXPRs.  But first, the assert pass needs to
     add ASSERT_EXPRs for them.  */
  if (TREE_CODE (stmt) == SWITCH_EXPR)
    return SSA_PROP_VARYING;

  cond = COND_EXPR_COND (stmt);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      tree use;
      ssa_op_iter i;

      fprintf (dump_file, "\nVisiting conditional with predicate: ");
      print_generic_expr (dump_file, cond, 0);
      fprintf (dump_file, "\nWith known ranges\n");
      
      FOR_EACH_SSA_TREE_OPERAND (use, stmt, i, SSA_OP_USE)
	{
	  fprintf (dump_file, "\t");
	  print_generic_expr (dump_file, use, 0);
	  fprintf (dump_file, ": ");
	  dump_value_range (dump_file, SSA_NAME_VALUE_RANGE (use));
	}

      fprintf (dump_file, "\n");
    }

  /* Compute the value of the predicate COND by checking the known
     ranges of each of its operands.  */
  val = vrp_evaluate_conditional (cond);
  if (val)
    *taken_edge_p = find_taken_edge (bb_for_stmt (stmt), val);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nPredicate evaluates to: ");
      if (val == NULL_TREE)
	fprintf (dump_file, "DON'T KNOW\n");
      else
	print_generic_stmt (dump_file, val, 0);
    }

  return (*taken_edge_p) ? SSA_PROP_INTERESTING : SSA_PROP_VARYING;
}


/* Evaluate statement STMT.  If the statement produces a useful range,
   return SSA_PROP_INTERESTING and record the SSA name with the
   interesting range into *OUTPUT_P.

   If STMT is a conditional branch and we can determine its truth
   value, the taken edge is recorded in *TAKEN_EDGE_P.

   If STMT produces a varying value, return SSA_PROP_VARYING.  */

static enum ssa_prop_result
vrp_visit_stmt (tree stmt, edge *taken_edge_p, tree *output_p)
{
  tree def;
  ssa_op_iter iter;
  stmt_ann_t ann;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting statement:\n");
      print_generic_stmt (dump_file, stmt, dump_flags);
      fprintf (dump_file, "\n");
    }

  ann = stmt_ann (stmt);
  if (TREE_CODE (stmt) == MODIFY_EXPR
      && ZERO_SSA_OPERANDS (stmt, SSA_OP_ALL_VIRTUALS))
    return vrp_visit_assignment (stmt, output_p);
  else if (TREE_CODE (stmt) == COND_EXPR || TREE_CODE (stmt) == SWITCH_EXPR)
    return vrp_visit_cond_stmt (stmt, taken_edge_p);

  /* All other statements produce nothing of interest for VRP, so mark
     their outputs varying and prevent further simulation.  */
  FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
    set_value_range_to_varying (get_value_range (def));

  return SSA_PROP_VARYING;
}


/* Meet operation for value ranges.  Given two value ranges VR0 and
   VR1, store in VR0 the result of meeting VR0 and VR1.
   
   The meeting rules are as follows:

   1- If VR0 and VR1 have an empty intersection, set VR0 to VR_VARYING.

   2- If VR0 and VR1 have a non-empty intersection, set VR0 to the
      union of VR0 and VR1.  */

static void
vrp_meet (value_range *vr0, value_range *vr1)
{
  if (vr0->type == VR_UNDEFINED)
    {
      *vr0 = *vr1;
      return;
    }

  if (vr1->type == VR_UNDEFINED)
    {
      /* Nothing to do.  VR0 already has the resulting range.  */
      return;
    }

  if (vr0->type == VR_VARYING)
    {
      /* Nothing to do.  VR0 already has the resulting range.  */
      return;
    }

  if (vr1->type == VR_VARYING)
    {
      *vr0 = *vr1;
      return;
    }

  /* If either is a symbolic range, drop to VARYING.  */
  if (symbolic_range_p (vr0) || symbolic_range_p (vr1))
    {
      set_value_range_to_varying (vr0);
      return;
    }

  if (vr0->type == VR_RANGE && vr1->type == VR_RANGE)
    {
      /* If VR0 and VR1 have a non-empty intersection, compute the
	 union of both ranges.  */
      if (value_ranges_intersect_p (vr0, vr1))
	{
	  tree min, max;

	  min = vr0->min;
	  max = vr0->max;

	  /* The lower limit of the new range is the minimum of the
	     two ranges.  */
	  if (compare_values (vr0->min, vr1->min) == 1)
	    min = vr1->min;

	  /* The upper limit of the new range is the maximum of the
	     two ranges.  */
	  if (compare_values (vr0->max, vr1->max) == -1)
	    max = vr1->max;

	  set_value_range (vr0, vr0->type, min, max);
	}
      else
	{
	  /* The two ranges don't intersect, set the result to VR_VARYING.  */
	  set_value_range_to_varying (vr0);
	}
    }
  else if (vr0->type == VR_ANTI_RANGE && vr1->type == VR_ANTI_RANGE)
    {
      /* Two anti-ranges meet only if they are both identical.  */
      if (compare_values (vr0->min, vr1->min) == 0
	  && compare_values (vr0->max, vr1->max) == 0
	  && compare_values (vr0->min, vr0->max) == 0)
	/* Nothing to do.  */ ;
      else
	set_value_range_to_varying (vr0);
    }
  else if (vr0->type == VR_ANTI_RANGE || vr1->type == VR_ANTI_RANGE)
    {
      /* A range [VAL1, VAL2] and an anti-range ~[VAL3, VAL4] meet
	 only if the ranges have an empty intersection.  The result of
	 the meet operation is the anti-range.  */
      if (!value_ranges_intersect_p (vr0, vr1))
	{
	  if (vr1->type == VR_ANTI_RANGE)
	    *vr0 = *vr1;
	}
      else
	set_value_range_to_varying (vr0);
    }
  else
    gcc_unreachable ();
}

      
/* Visit all arguments for PHI node PHI that flow through executable
   edges.  If a valid value range can be derived from all the incoming
   value ranges, set a new range for the LHS of PHI.  */

static enum ssa_prop_result
vrp_visit_phi_node (tree phi)
{
  int i;
  tree lhs = PHI_RESULT (phi);
  value_range *lhs_vr = get_value_range (lhs);
  value_range vr_result = *lhs_vr;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "\nVisiting PHI node: ");
      print_generic_expr (dump_file, phi, dump_flags);
    }

  for (i = 0; i < PHI_NUM_ARGS (phi); i++)
    {
      edge e = PHI_ARG_EDGE (phi, i);

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file,
	      "\n    Argument #%d (%d -> %d %sexecutable)\n",
	      i, e->src->index, e->dest->index,
	      (e->flags & EDGE_EXECUTABLE) ? "" : "not ");
	}

      if (e->flags & EDGE_EXECUTABLE)
	{
	  tree arg = PHI_ARG_DEF (phi, i);
	  value_range vr_arg;

	  if (TREE_CODE (arg) == SSA_NAME)
	    vr_arg = *(get_value_range (arg));
	  else
	    {
	      vr_arg.type = VR_RANGE;
	      vr_arg.min = arg;
	      vr_arg.max = arg;
	    }

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "\t");
	      print_generic_expr (dump_file, arg, dump_flags);
	      fprintf (dump_file, "\n\tValue: ");
	      dump_value_range (dump_file, &vr_arg);
	      fprintf (dump_file, "\n");
	    }

	  vrp_meet (&vr_result, &vr_arg);

	  if (vr_result.type == VR_VARYING)
	    break;
	}
    }

  if (vr_result.type == VR_VARYING)
    {
      set_value_range_to_varying (lhs_vr);
      return SSA_PROP_VARYING;
    }

  /* To prevent infinite iterations in the algorithm, derive ranges
     when the new value is slightly bigger or smaller than the
     previous one.  */
  if (lhs_vr->type == VR_RANGE)
    {
      if (!POINTER_TYPE_P (TREE_TYPE (lhs)))
	{
	  int cmp_min = compare_values (lhs_vr->min, vr_result.min);
	  int cmp_max = compare_values (lhs_vr->max, vr_result.max);

	  /* If the new minimum is smaller or larger than the previous
	     one, go all the way to -INF.  In the first case, to avoid
	     iterating millions of times to reach -INF, and in the
	     other case to avoid infinite bouncing between different
	     minimums.  */
	  if (cmp_min > 0 || cmp_min < 0)
	    vr_result.min = TYPE_MIN_VALUE (TREE_TYPE (vr_result.min));

	  /* Similarly, if the new maximum is smaller or larger than
	     the previous one, go all the way to +INF.  */
	  if (cmp_max < 0 || cmp_max > 0)
	    vr_result.max = TYPE_MAX_VALUE (TREE_TYPE (vr_result.max));

	  /* If we ended up with a (-INF, +INF) range, set it to
	     VARYING.  */
	  if (vr_result.min == TYPE_MIN_VALUE (TREE_TYPE (vr_result.min))
	      && vr_result.max == TYPE_MAX_VALUE (TREE_TYPE (vr_result.max)))
	    {
	      set_value_range_to_varying (lhs_vr);
	      return SSA_PROP_VARYING;
	    }
	}
    }

  /* If the new range is different than the previous value, keep
     iterating.  */
  if (update_value_range (lhs_vr, vr_result.type, vr_result.min, vr_result.max))
    return SSA_PROP_INTERESTING;

  /* Nothing changed, don't add outgoing edges.  */
  return SSA_PROP_NOT_INTERESTING;
}


/* Traverse all the blocks folding conditionals with known ranges.  */

static void
vrp_finalize (void)
{
  basic_block bb;
  int num_pred_folded = 0;

  if (dump_file)
    {
      fprintf (dump_file, "\nValue ranges after VRP:\n\n");
      dump_all_value_ranges (dump_file);
      fprintf (dump_file, "\n");
    }

  FOR_EACH_BB (bb)
    {
      tree last = last_stmt (bb);
      if (last && TREE_CODE (last) == COND_EXPR)
	{
	  tree val = vrp_evaluate_conditional (COND_EXPR_COND (last));
	  if (val)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Folding predicate ");
		  print_generic_expr (dump_file, COND_EXPR_COND (last), 0);
		  fprintf (dump_file, " to ");
		  print_generic_expr (dump_file, val, 0);
		  fprintf (dump_file, "\n");
		}

	      num_pred_folded++;
	      COND_EXPR_COND (last) = val;
	      update_stmt (last);
	    }
	}
    }

  if (dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "\nNumber of predicates folded: %d\n\n",
	     num_pred_folded);
}


/* Main entry point to VRP (Value Range Propagation).  This pass is
   loosely based on J. R. C. Patterson, ``Accurate Static Branch
   Prediction by Value Range Propagation,'' in SIGPLAN Conference on
   Programming Language Design and Implementation, pp. 67-78, 1995.
   Also available at http://citeseer.ist.psu.edu/patterson95accurate.html

   This is essentially an SSA-CCP pass modified to deal with ranges
   instead of constants.

   TODO, the main difference between this pass and Patterson's is that
   we do not propagate edge probabilities.  We only compute whether
   edges can be taken or not.  That is, instead of having a spectrum
   of jump probabilities between 0 and 1, we only deal with 0, 1 and
   DON'T KNOW.  In the future, it may be worthwhile to propagate
   probabilities to aid branch prediction.  */

static void
execute_vrp (void)
{
  insert_range_assertions ();

  cfg_loops = loop_optimizer_init (NULL);
  if (cfg_loops)
    scev_initialize (cfg_loops);

  if (vrp_initialize ())
    {
      ssa_propagate (vrp_visit_stmt, vrp_visit_phi_node);
      vrp_finalize ();
    }

  if (cfg_loops)
    {
      scev_finalize ();
      loop_optimizer_finalize (cfg_loops, NULL);
      current_loops = NULL;
    }

  remove_range_assertions ();
}

static bool
gate_vrp (void)
{
  return flag_tree_vrp != 0;
}

struct tree_opt_pass pass_vrp =
{
  "vrp",				/* name */
  gate_vrp,				/* gate */
  execute_vrp,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_VRP,				/* tv_id */
  PROP_ssa | PROP_alias,		/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_cleanup_cfg
    | TODO_ggc_collect
    | TODO_verify_ssa
    | TODO_dump_func
    | TODO_update_ssa,			/* todo_flags_finish */
  0					/* letter */
};
