/* Transformations based on profile information for values.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "value-prof.h"
#include "output.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "optabs.h"
#include "regs.h"

/* In this file value profile based optimizations will be placed (none are
   here just now, but they are hopefully coming soon).

   Every such optimization should add its requirements for profiled values to
   insn_values_to_profile function.  This function is called from branch_prob
   in profile.c and the requested values are instrumented by it in the first
   compilation with -fprofile-arcs.  The optimization may then read the
   gathered data in the second compilation with -fbranch-probabilities.
   The measured data is appended as REG_VALUE_PROFILE note to the instrumented
   insn.  The argument to the note consists of an EXPR_LIST where its
   members have the following meaning (from the first to the last):
   
   -- type of information gathered (HIST_TYPE*)
   -- the expression that is profiled
   -- list of counters starting from the first one.  */

static void insn_divmod_values_to_profile (rtx, unsigned *,
					   struct histogram_value **);
static void insn_values_to_profile (rtx, unsigned *, struct histogram_value **);
static rtx gen_divmod_fixed_value (enum machine_mode, enum rtx_code, rtx, rtx,
				   rtx, gcov_type);
static rtx gen_mod_pow2 (enum machine_mode, enum rtx_code, rtx, rtx, rtx);
static rtx gen_mod_subtract (enum machine_mode, enum rtx_code, rtx, rtx, rtx,
			     int);
static bool divmod_fixed_value_transform (rtx insn);
static bool mod_pow2_value_transform (rtx);
static bool mod_subtract_transform (rtx);

/* Release the list of VALUES of length N_VALUES for that we want to measure
   histograms.  */
void
free_profiled_values (unsigned n_values ATTRIBUTE_UNUSED,
		      struct histogram_value *values)
{
  free (values);
}

/* Find values inside INSN for that we want to measure histograms for
   division/modulo optimization.  */
static void
insn_divmod_values_to_profile (rtx insn, unsigned *n_values,
			       struct histogram_value **values)
{
  rtx set, set_src, op1, op2;
  enum machine_mode mode;

  if (!INSN_P (insn))
    return;

  set = single_set (insn);
  if (!set)
    return;

  mode = GET_MODE (SET_DEST (set));
  if (!INTEGRAL_MODE_P (mode))
    return;

  set_src = SET_SRC (set);
  switch (GET_CODE (set_src))
    {
    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      op1 = XEXP (set_src, 0);
      op2 = XEXP (set_src, 1);
      if (side_effects_p (op2))
	return;

      /* Check for a special case where the divisor is power of 2.  */
      if ((GET_CODE (set_src) == UMOD) && !CONSTANT_P (op2))
	{
	  *values = xrealloc (*values,
			      (*n_values + 1)
				* sizeof (struct histogram_value));
	  (*values)[*n_values].value = op2;
	  (*values)[*n_values].seq = NULL_RTX;
	  (*values)[*n_values].mode = mode;
	  (*values)[*n_values].insn = insn;
	  (*values)[*n_values].type = HIST_TYPE_POW2;
	  (*values)[*n_values].hdata.pow2.may_be_other = 1;
	  (*n_values)++;
	}

      /* Check whether the divisor is not in fact a constant.  */
      if (!CONSTANT_P (op2))
	{
	  *values = xrealloc (*values,
			      (*n_values + 1)
				* sizeof (struct histogram_value));
	  (*values)[*n_values].value = op2;
	  (*values)[*n_values].mode = mode;
	  (*values)[*n_values].seq = NULL_RTX;
	  (*values)[*n_values].insn = insn;
	  (*values)[*n_values].type = HIST_TYPE_SINGLE_VALUE;
	  (*n_values)++;
	}

      /* For mod, check whether it is not often a noop (or replaceable by
	 a few subtractions).  */
      if (GET_CODE (set_src) == UMOD && !side_effects_p (op1))
	{
	  rtx tmp;

	  *values = xrealloc (*values,
			      (*n_values + 1)
				* sizeof (struct histogram_value));
	  start_sequence ();
	  tmp = simplify_gen_binary (DIV, mode, copy_rtx (op1), copy_rtx (op2));
	  (*values)[*n_values].value = force_operand (tmp, NULL_RTX);
	  (*values)[*n_values].seq = get_insns ();
	  end_sequence ();
	  (*values)[*n_values].mode = mode;
	  (*values)[*n_values].insn = insn;
	  (*values)[*n_values].type = HIST_TYPE_INTERVAL;
	  (*values)[*n_values].hdata.intvl.int_start = 0;
	  (*values)[*n_values].hdata.intvl.steps = 2;
	  (*values)[*n_values].hdata.intvl.may_be_less = 1;
	  (*values)[*n_values].hdata.intvl.may_be_more = 1;
	  (*n_values)++;
	}
      return;

    default:
      return;
    }
}

/* Find values inside INSN for that we want to measure histograms and adds
   them to list VALUES (increasing the record of its length in N_VALUES).  */
static void
insn_values_to_profile (rtx insn,
			unsigned *n_values,
			struct histogram_value **values)
{
  if (flag_value_profile_transformations)
    insn_divmod_values_to_profile (insn, n_values, values);
}

/* Find list of values for that we want to measure histograms.  */
void
find_values_to_profile (unsigned *n_values, struct histogram_value **values)
{
  rtx insn;
  unsigned i;

  *n_values = 0;
  *values = NULL;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    insn_values_to_profile (insn, n_values, values);

  for (i = 0; i < *n_values; i++)
    {
      switch ((*values)[i].type)
	{
	case HIST_TYPE_INTERVAL:
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "Interval counter for insn %d, range %d -- %d.\n",
		     INSN_UID ((*values)[i].insn),
		     (*values)[i].hdata.intvl.int_start,
		     ((*values)[i].hdata.intvl.int_start
		      + (*values)[i].hdata.intvl.steps - 1));
	  (*values)[i].n_counters = (*values)[i].hdata.intvl.steps +
		  ((*values)[i].hdata.intvl.may_be_less ? 1 : 0) +
		  ((*values)[i].hdata.intvl.may_be_more ? 1 : 0);
	  break;

	case HIST_TYPE_POW2:
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "Pow2 counter for insn %d.\n",
		     INSN_UID ((*values)[i].insn));
	  (*values)[i].n_counters = GET_MODE_BITSIZE ((*values)[i].mode) +
		  ((*values)[i].hdata.pow2.may_be_other ? 1 : 0);
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "Single value counter for insn %d.\n",
		     INSN_UID ((*values)[i].insn));
	  (*values)[i].n_counters = 3;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  if (rtl_dump_file)
	    fprintf (rtl_dump_file,
		     "Constant delta counter for insn %d.\n",
		     INSN_UID ((*values)[i].insn));
	  (*values)[i].n_counters = 4;
	  break;

	default:
	  abort ();
	}
    }
}

/* Main entry point.  Finds REG_VALUE_PROFILE notes from profiler and uses
   them to identify and exploit properties of values that are hard to analyze
   statically.

   We do following transformations:

   1)

   x = a / b;

   where b is almost always a constant N is transformed to

   if (b == N)
     x = a / N;
   else
     x = a / b;

   Analogically with %

   2)

   x = a % b

   where b is almost always a power of 2 and the division is unsigned
   TODO -- handle signed case as well

   if ((b & (b - 1)) == 0)
     x = a & (b - 1);
   else
     x = x % b;

   Note that when b = 0, no error will occur and x = a; this is correct,
   as result of such operation is undefined.

   3)

   x = a % b

   where a is almost always less then b and the division is unsigned
   TODO -- handle signed case as well

   x = a;
   if (x >= b)
     x %= b;

   4)

   x = a % b

   where a is almost always less then 2 * b and the division is unsigned
   TODO -- handle signed case as well

   x = a;
   if (x >= b)
     x -= b;
   if (x >= b)
     x %= b;

   It would be possible to continue analogically for K * b for other small
   K's, but it is probably not useful.

   TODO:

   There are other useful cases that could be handled by a similar mechanism,
   for example:
   
   for (i = 0; i < n; i++)
     ...
   
   transform to (for constant N):
   
   if (n == N)
     for (i = 0; i < N; i++)
       ...
   else
     for (i = 0; i < n; i++)
       ...
   making unroller happy.  Since this may grow the code significantly,
   we would have to be very careful here.  */

bool
value_profile_transformations (void)
{
  rtx insn, next;
  int changed = false;

  for (insn = get_insns (); insn; insn = next)
    {
      next = NEXT_INSN (insn);

      if (!INSN_P (insn))
	continue;

      /* Scan for insn carrying a histogram.  */
      if (!find_reg_note (insn, REG_VALUE_PROFILE, 0))
	continue;

      /* Ignore cold areas -- we are growing a code.  */
      if (!maybe_hot_bb_p (BLOCK_FOR_INSN (insn)))
	continue;

      if (rtl_dump_file)
	{
	  fprintf (rtl_dump_file, "Trying transformations on insn %d\n",
		   INSN_UID (insn));
	  print_rtl_single (rtl_dump_file, insn);
	}

      /* Transformations:  */
      if (flag_value_profile_transformations
	  && (mod_subtract_transform (insn)
	      || divmod_fixed_value_transform (insn)
	      || mod_pow2_value_transform (insn)))
	changed = true;
    }

  if (changed)
    {
      commit_edge_insertions ();
      allocate_reg_info (max_reg_num (), FALSE, FALSE);
    }

  return changed;
}

/* Generate code for transformation 1 (with MODE and OPERATION, operands OP1
   and OP2 whose value is expected to be VALUE and result TARGET).  */
static rtx
gen_divmod_fixed_value (enum machine_mode mode, enum rtx_code operation,
			rtx target, rtx op1, rtx op2, gcov_type value)
{
  rtx tmp, tmp1;
  rtx neq_label = gen_label_rtx ();
  rtx end_label = gen_label_rtx ();
  rtx sequence;

  start_sequence ();
  
  if (!REG_P (op2))
    {
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, copy_rtx (op2));
    }
  else
    tmp = op2;

  do_compare_rtx_and_jump (tmp, GEN_INT (value), NE, 0, mode, NULL_RTX,
			   NULL_RTX, neq_label);
  tmp1 = simplify_gen_binary (operation, mode, copy_rtx (op1), GEN_INT (value));
  tmp1 = force_operand (tmp1, target);
  if (tmp1 != target)
    emit_move_insn (copy_rtx (target), copy_rtx (tmp1));

  emit_jump_insn (gen_jump (end_label));
  emit_barrier ();

  emit_label (neq_label);
  tmp1 = simplify_gen_binary (operation, mode, copy_rtx (op1), copy_rtx (tmp));
  tmp1 = force_operand (tmp1, target);
  if (tmp1 != target)
    emit_move_insn (copy_rtx (target), copy_rtx (tmp1));
  
  emit_label (end_label);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}

/* Do transform 1) on INSN if applicable.  */
static bool
divmod_fixed_value_transform (rtx insn)
{
  rtx set, set_src, set_dest, op1, op2, value, histogram;
  enum rtx_code code;
  enum machine_mode mode;
  gcov_type val, count, all;
  edge e;

  set = single_set (insn);
  if (!set)
    return false;

  set_src = SET_SRC (set);
  set_dest = SET_DEST (set);
  code = GET_CODE (set_src);
  mode = GET_MODE (set_dest);
  
  if (code != DIV && code != MOD && code != UDIV && code != UMOD)
    return false;
  op1 = XEXP (set_src, false);
  op2 = XEXP (set_src, 1);

  for (histogram = REG_NOTES (insn);
       histogram;
       histogram = XEXP (histogram, 1))
    if (REG_NOTE_KIND (histogram) == REG_VALUE_PROFILE
	&& XEXP (XEXP (histogram, 0), 0) == GEN_INT (HIST_TYPE_SINGLE_VALUE))
      break;

  if (!histogram)
    return false;

  histogram = XEXP (XEXP (histogram, 0), 1);
  value = XEXP (histogram, 0);
  histogram = XEXP (histogram, 1);
  val = INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);
  count = INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);
  all = INTVAL (XEXP (histogram, 0));

  /* We require that count is at least half of all; this means
     that for the transformation to fire the value must be constant
     at least 50% of time (and 75% gives the guarantee of usage).  */
  if (!rtx_equal_p (op2, value) || 2 * count < all)
    return false;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Div/mod by constant transformation on insn %d\n",
	     INSN_UID (insn));

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  delete_insn (insn);
  
  insert_insn_on_edge (
	gen_divmod_fixed_value (mode, code, set_dest, op1, op2, val), e);

  return true;
}

/* Generate code for transformation 2 (with MODE and OPERATION, operands OP1
   and OP2 and result TARGET).  */
static rtx
gen_mod_pow2 (enum machine_mode mode, enum rtx_code operation, rtx target,
	      rtx op1, rtx op2)
{
  rtx tmp, tmp1, tmp2, tmp3;
  rtx neq_label = gen_label_rtx ();
  rtx end_label = gen_label_rtx ();
  rtx sequence;

  start_sequence ();
  
  if (!REG_P (op2))
    {
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, copy_rtx (op2));
    }
  else
    tmp = op2;

  tmp1 = expand_simple_binop (mode, PLUS, tmp, constm1_rtx, NULL_RTX,
			      0, OPTAB_WIDEN);
  tmp2 = expand_simple_binop (mode, AND, tmp, tmp1, NULL_RTX,
			      0, OPTAB_WIDEN);
  do_compare_rtx_and_jump (tmp2, const0_rtx, NE, 0, mode, NULL_RTX,
			   NULL_RTX, neq_label);
  tmp3 = expand_simple_binop (mode, AND, op1, tmp1, target,
			      0, OPTAB_WIDEN);
  if (tmp3 != target)
    emit_move_insn (copy_rtx (target), tmp3);
  emit_jump_insn (gen_jump (end_label));
  emit_barrier ();

  emit_label (neq_label);
  tmp1 = simplify_gen_binary (operation, mode, copy_rtx (op1), copy_rtx (tmp));
  tmp1 = force_operand (tmp1, target);
  if (tmp1 != target)
    emit_move_insn (target, tmp1);
  
  emit_label (end_label);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}

/* Do transform 2) on INSN if applicable.  */
static bool
mod_pow2_value_transform (rtx insn)
{
  rtx set, set_src, set_dest, op1, op2, value, histogram;
  enum rtx_code code;
  enum machine_mode mode;
  gcov_type wrong_values, count;
  edge e;
  int i;

  set = single_set (insn);
  if (!set)
    return false;

  set_src = SET_SRC (set);
  set_dest = SET_DEST (set);
  code = GET_CODE (set_src);
  mode = GET_MODE (set_dest);
  
  if (code != UMOD)
    return false;
  op1 = XEXP (set_src, 0);
  op2 = XEXP (set_src, 1);

  for (histogram = REG_NOTES (insn);
       histogram;
       histogram = XEXP (histogram, 1))
    if (REG_NOTE_KIND (histogram) == REG_VALUE_PROFILE
	&& XEXP (XEXP (histogram, 0), 0) == GEN_INT (HIST_TYPE_POW2))
      break;

  if (!histogram)
    return false;

  histogram = XEXP (XEXP (histogram, 0), 1);
  value = XEXP (histogram, 0);
  histogram = XEXP (histogram, 1);
  wrong_values =INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);

  count = 0;
  for (i = 0; i < GET_MODE_BITSIZE (mode); i++)
    {
      count += INTVAL (XEXP (histogram, 0));
      histogram = XEXP (histogram, 1);
    }

  if (!rtx_equal_p (op2, value))
    return false;

  /* We require that we hit a power of two at least half of all evaluations.  */
  if (count < wrong_values)
    return false;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Mod power of 2 transformation on insn %d\n",
	     INSN_UID (insn));

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  delete_insn (insn);
  
  insert_insn_on_edge (
	gen_mod_pow2 (mode, code, set_dest, op1, op2), e);

  return true;
}

/* Generate code for transformations 3 and 4 (with MODE and OPERATION,
   operands OP1 and OP2, result TARGET and at most SUB subtractions).  */
static rtx
gen_mod_subtract (enum machine_mode mode, enum rtx_code operation,
		  rtx target, rtx op1, rtx op2, int sub)
{
  rtx tmp, tmp1;
  rtx end_label = gen_label_rtx ();
  rtx sequence;
  int i;

  start_sequence ();
  
  if (!REG_P (op2))
    {
      tmp = gen_reg_rtx (mode);
      emit_move_insn (tmp, copy_rtx (op2));
    }
  else
    tmp = op2;

  emit_move_insn (target, copy_rtx (op1));
  do_compare_rtx_and_jump (target, tmp, LTU, 0, mode, NULL_RTX,
			   NULL_RTX, end_label);
  

  for (i = 0; i < sub; i++)
    {
      tmp1 = expand_simple_binop (mode, MINUS, target, tmp, target,
	    			  0, OPTAB_WIDEN);
      if (tmp1 != target)
	emit_move_insn (target, tmp1);
      do_compare_rtx_and_jump (target, tmp, LTU, 0, mode, NULL_RTX,
    			       NULL_RTX, end_label);
    }

  tmp1 = simplify_gen_binary (operation, mode, copy_rtx (target), copy_rtx (tmp));
  tmp1 = force_operand (tmp1, target);
  if (tmp1 != target)
    emit_move_insn (target, tmp1);
  
  emit_label (end_label);

  sequence = get_insns ();
  end_sequence ();
  rebuild_jump_labels (sequence);
  return sequence;
}

/* Do transforms 3) and 4) on INSN if applicable.  */
static bool
mod_subtract_transform (rtx insn)
{
  rtx set, set_src, set_dest, op1, op2, value, histogram;
  enum rtx_code code;
  enum machine_mode mode;
  gcov_type wrong_values, counts[2], count, all;
  edge e;
  int i;

  set = single_set (insn);
  if (!set)
    return false;

  set_src = SET_SRC (set);
  set_dest = SET_DEST (set);
  code = GET_CODE (set_src);
  mode = GET_MODE (set_dest);
  
  if (code != UMOD)
    return false;
  op1 = XEXP (set_src, 0);
  op2 = XEXP (set_src, 1);

  for (histogram = REG_NOTES (insn);
       histogram;
       histogram = XEXP (histogram, 1))
    if (REG_NOTE_KIND (histogram) == REG_VALUE_PROFILE
	&& XEXP (XEXP (histogram, 0), 0) == GEN_INT (HIST_TYPE_INTERVAL))
      break;

  if (!histogram)
    return false;

  histogram = XEXP (XEXP (histogram, 0), 1);
  value = XEXP (histogram, 0);
  histogram = XEXP (histogram, 1);

  all = 0;
  for (i = 0; i < 2; i++)
    {
      counts[i] = INTVAL (XEXP (histogram, 0));
      all += counts[i];
      histogram = XEXP (histogram, 1);
    }
  wrong_values = INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);
  wrong_values += INTVAL (XEXP (histogram, 0));
  all += wrong_values;

  /* We require that we use just subtractions in at least 50% of all
     evaluations.  */
  count = 0;
  for (i = 0; i < 2; i++)
    {
      count += counts[i];
      if (count * 2 >= all)
	break;
    }
  
  if (i == 2)
    return false;

  if (rtl_dump_file)
    fprintf (rtl_dump_file, "Mod subtract transformation on insn %d\n",
	     INSN_UID (insn));

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  delete_insn (insn);
  
  insert_insn_on_edge (
	gen_mod_subtract (mode, code, set_dest, op1, op2, i), e);

  return true;
}
