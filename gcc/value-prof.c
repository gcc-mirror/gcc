/* Transformations based on profile information for values.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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
#include "ggc.h"

static struct value_prof_hooks *value_prof_hooks;

/* In this file value profile based optimizations are placed.  Currently the
   following optimizations are implemented (for more detailed descriptions
   see comments at value_profile_transformations):

   1) Division/modulo specialization.  Provided that we can determine that the
      operands of the division have some special properties, we may use it to
      produce more effective code.
   2) Speculative prefetching.  If we are able to determine that the difference
      between addresses accessed by a memory reference is usually constant, we
      may add the prefetch instructions.

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

/* For speculative prefetching, the range in that we do not prefetch (because
   we assume that it will be in cache anyway).  The asymmetry between min and
   max range is trying to reflect the fact that the sequential prefetching
   of the data is commonly done directly by hardware.  Nevertheless, these
   values are just a guess and should of course be target-specific.  */

#ifndef NOPREFETCH_RANGE_MIN
#define NOPREFETCH_RANGE_MIN (-16)
#endif
#ifndef NOPREFETCH_RANGE_MAX
#define NOPREFETCH_RANGE_MAX 32
#endif

static void insn_divmod_values_to_profile (rtx, histogram_values *);
#ifdef HAVE_prefetch
static bool insn_prefetch_values_to_profile (rtx, histogram_values *);
static int find_mem_reference_1 (rtx *, void *);
static void find_mem_reference_2 (rtx, rtx, void *);
static bool find_mem_reference (rtx, rtx *, int *);
#endif

static void insn_values_to_profile (rtx, histogram_values *);
static rtx gen_divmod_fixed_value (enum machine_mode, enum rtx_code, rtx, rtx,
				   rtx, gcov_type, int);
static rtx gen_mod_pow2 (enum machine_mode, enum rtx_code, rtx, rtx, rtx, int);
static rtx gen_mod_subtract (enum machine_mode, enum rtx_code, rtx, rtx, rtx,
			     int, int, int);
#ifdef HAVE_prefetch
static rtx gen_speculative_prefetch (rtx, gcov_type, int);
#endif
static bool divmod_fixed_value_transform (rtx insn);
static bool mod_pow2_value_transform (rtx);
static bool mod_subtract_transform (rtx);
#ifdef HAVE_prefetch
static bool speculative_prefetching_transform (rtx);
#endif

/* Find values inside INSN for that we want to measure histograms for
   division/modulo optimization and stores them to VALUES.  */
static void
insn_divmod_values_to_profile (rtx insn, histogram_values *values)
{
  rtx set, set_src, op1, op2;
  enum machine_mode mode;
  histogram_value hist;

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
	  hist = ggc_alloc (sizeof (*hist));
	  hist->value = op2;
	  hist->seq = NULL_RTX;
	  hist->mode = mode;
	  hist->insn = insn;
	  hist->type = HIST_TYPE_POW2;
	  hist->hdata.pow2.may_be_other = 1;
	  VEC_safe_push (histogram_value, *values, hist);
	}

      /* Check whether the divisor is not in fact a constant.  */
      if (!CONSTANT_P (op2))
	{
	  hist = ggc_alloc (sizeof (*hist));
	  hist->value = op2;
	  hist->mode = mode;
	  hist->seq = NULL_RTX;
	  hist->insn = insn;
	  hist->type = HIST_TYPE_SINGLE_VALUE;
	  VEC_safe_push (histogram_value, *values, hist);
	}

      /* For mod, check whether it is not often a noop (or replaceable by
	 a few subtractions).  */
      if (GET_CODE (set_src) == UMOD && !side_effects_p (op1))
	{
	  rtx tmp;

	  hist = ggc_alloc (sizeof (*hist));
	  start_sequence ();
	  tmp = simplify_gen_binary (DIV, mode, copy_rtx (op1), copy_rtx (op2));
	  hist->value = force_operand (tmp, NULL_RTX);
	  hist->seq = get_insns ();
	  end_sequence ();
	  hist->mode = mode;
	  hist->insn = insn;
	  hist->type = HIST_TYPE_INTERVAL;
	  hist->hdata.intvl.int_start = 0;
	  hist->hdata.intvl.steps = 2;
	  hist->hdata.intvl.may_be_less = 1;
	  hist->hdata.intvl.may_be_more = 1;
	  VEC_safe_push (histogram_value, *values, hist);
	}
      return;

    default:
      return;
    }
}

#ifdef HAVE_prefetch

/* Called from find_mem_reference through for_each_rtx, finds a memory
   reference.  I.e. if *EXPR is a MEM, the reference to this MEM is stored
   to *RET and the traversing of the expression is interrupted by returning 1.
   Otherwise 0 is returned.  */

static int
find_mem_reference_1 (rtx *expr, void *ret)
{
  rtx *mem = ret;

  if (GET_CODE (*expr) == MEM)
    {
      *mem = *expr;
      return 1;
    }
  return 0;
}

/* Called form find_mem_reference through note_stores to find out whether
   the memory reference MEM is a store.  I.e. if EXPR == MEM, the variable
   FMR2_WRITE is set to true.  */

static int fmr2_write;
static void
find_mem_reference_2 (rtx expr, rtx pat ATTRIBUTE_UNUSED, void *mem)
{
  if (expr == mem)
    fmr2_write = true;
}

/* Find a memory reference inside INSN, return it in MEM. Set WRITE to true
   if it is a write of the mem.  Return false if no memory reference is found,
   true otherwise.  */

static bool
find_mem_reference (rtx insn, rtx *mem, int *write)
{
  *mem = NULL_RTX;
  for_each_rtx (&PATTERN (insn), find_mem_reference_1, mem);

  if (!*mem)
    return false;
  
  fmr2_write = false;
  note_stores (PATTERN (insn), find_mem_reference_2, *mem);
  *write = fmr2_write;
  return true;
}

/* Find values inside INSN for that we want to measure histograms for
   a speculative prefetching.  Add them to the list VALUES.
   Returns true if such we found any such value, false otherwise.  */

static bool
insn_prefetch_values_to_profile (rtx insn, histogram_values *values)
{
  rtx mem, address;
  int write;
  histogram_value hist;

  /* It only makes sense to look for memory references in ordinary insns.  */
  if (GET_CODE (insn) != INSN)
    return false;

  if (!find_mem_reference (insn, &mem, &write))
    return false;

  address = XEXP (mem, 0);
  if (side_effects_p (address))
    return false;
      
  if (CONSTANT_P (address))
    return false;

  hist = ggc_alloc (sizeof (*hist));
  hist->value = address;
  hist->mode = GET_MODE (address);
  hist->seq = NULL_RTX;
  hist->insn = insn;
  hist->type = HIST_TYPE_CONST_DELTA;
  VEC_safe_push (histogram_value, *values, hist);

  return true;
}
#endif
/* Find values inside INSN for that we want to measure histograms and adds
   them to list VALUES (increasing the record of its length in N_VALUES).  */
static void
insn_values_to_profile (rtx insn, histogram_values *values)
{
  if (flag_value_profile_transformations)
    insn_divmod_values_to_profile (insn, values);

#ifdef HAVE_prefetch
  if (flag_speculative_prefetching)
    insn_prefetch_values_to_profile (insn, values);
#endif
}

/* Find list of values for that we want to measure histograms.  */
static void
rtl_find_values_to_profile (histogram_values *values)
{
  rtx insn;
  unsigned i, libcall_level;

  life_analysis (NULL, PROP_DEATH_NOTES);

  *values = VEC_alloc (histogram_value, 0);
  libcall_level = 0;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (find_reg_note (insn, REG_LIBCALL, NULL_RTX))
	libcall_level++;

      /* Do not instrument values inside libcalls (we are going to split block
	 due to instrumentation, and libcall blocks should be local to a single
	 basic block).  */
      if (!libcall_level)
	insn_values_to_profile (insn, values);

      if (find_reg_note (insn, REG_RETVAL, NULL_RTX))
	{
	  gcc_assert (libcall_level > 0);
	  libcall_level--;
	}
    }
  gcc_assert (libcall_level == 0);

  for (i = 0; i < VEC_length (histogram_value, *values); i++)
    {
      histogram_value hist = VEC_index (histogram_value, *values, i);

      switch (hist->type)
	{
	case HIST_TYPE_INTERVAL:
	  if (dump_file)
	    fprintf (dump_file,
		     "Interval counter for insn %d, range %d -- %d.\n",
		     INSN_UID ((rtx)hist->insn),
		     hist->hdata.intvl.int_start,
		     (hist->hdata.intvl.int_start
		      + hist->hdata.intvl.steps - 1));
	  hist->n_counters = hist->hdata.intvl.steps +
		  (hist->hdata.intvl.may_be_less ? 1 : 0) +
		  (hist->hdata.intvl.may_be_more ? 1 : 0);
	  break;

	case HIST_TYPE_POW2:
	  if (dump_file)
	    fprintf (dump_file,
		     "Pow2 counter for insn %d.\n",
		     INSN_UID ((rtx)hist->insn));
	  hist->n_counters 
		= GET_MODE_BITSIZE (hist->mode)
		  +  (hist->hdata.pow2.may_be_other ? 1 : 0);
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  if (dump_file)
	    fprintf (dump_file,
		     "Single value counter for insn %d.\n",
		     INSN_UID ((rtx)hist->insn));
	  hist->n_counters = 3;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  if (dump_file)
	    fprintf (dump_file,
		     "Constant delta counter for insn %d.\n",
		     INSN_UID ((rtx)hist->insn));
	  hist->n_counters = 4;
	  break;

	default:
	  abort ();
	}
    }
  allocate_reg_info (max_reg_num (), FALSE, FALSE);
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

   5)

   Read or write of mem[address], where the value of address changes usually
   by a constant C != 0 between the following accesses to the computation; with
   -fspeculative-prefetching we then add a prefetch of address + C before
   the insn.  This handles prefetching of several interesting cases in addition
   to a simple prefetching for addresses that are induction variables, e. g.
   linked lists allocated sequentially (even in case they are processed
   recursively).

   TODO -- we should also check whether there is not (usually) a small
	   difference with the adjacent memory references, so that we do
	   not issue overlapping prefetches.  Also we should employ some
	   heuristics to eliminate cases where prefetching evidently spoils
	   the code.
	-- it should somehow cooperate with the loop optimizer prefetching

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

static bool
rtl_value_profile_transformations (void)
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

      if (dump_file)
	{
	  fprintf (dump_file, "Trying transformations on insn %d\n",
		   INSN_UID (insn));
	  print_rtl_single (dump_file, insn);
	}

      /* Transformations:  */
      if (flag_value_profile_transformations
	  && (mod_subtract_transform (insn)
	      || divmod_fixed_value_transform (insn)
	      || mod_pow2_value_transform (insn)))
	changed = true;
#ifdef HAVE_prefetch
      if (flag_speculative_prefetching
	  && speculative_prefetching_transform (insn))
	changed = true;
#endif
    }

  if (changed)
    {
      commit_edge_insertions ();
      allocate_reg_info (max_reg_num (), FALSE, FALSE);
    }

  return changed;
}

/* Generate code for transformation 1 (with MODE and OPERATION, operands OP1
   and OP2, whose value is expected to be VALUE, result TARGET and
   probability of taking the optimal path PROB).  */
static rtx
gen_divmod_fixed_value (enum machine_mode mode, enum rtx_code operation,
			rtx target, rtx op1, rtx op2, gcov_type value,
			int prob)
{
  rtx tmp, tmp1, jump;
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

  /* Add branch probability to jump we just created.  */
  jump = get_last_insn ();
  REG_NOTES (jump) = gen_rtx_EXPR_LIST (REG_BR_PROB,
					GEN_INT (REG_BR_PROB_BASE - prob),
					REG_NOTES (jump));

  tmp1 = simplify_gen_binary (operation, mode,
			      copy_rtx (op1), GEN_INT (value));
  tmp1 = force_operand (tmp1, target);
  if (tmp1 != target)
    emit_move_insn (copy_rtx (target), copy_rtx (tmp1));

  emit_jump_insn (gen_jump (end_label));
  emit_barrier ();

  emit_label (neq_label);
  tmp1 = simplify_gen_binary (operation, mode,
			      copy_rtx (op1), copy_rtx (tmp));
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
  int prob;

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

  /* We require that count be at least half of all; this means
     that for the transformation to fire the value must be constant
     at least 50% of time (and 75% gives the guarantee of usage).  */
  if (!rtx_equal_p (op2, value) || 2 * count < all)
    return false;

  if (dump_file)
    fprintf (dump_file, "Div/mod by constant transformation on insn %d\n",
	     INSN_UID (insn));

  /* Compute probability of taking the optimal path.  */
  prob = (count * REG_BR_PROB_BASE + all / 2) / all;

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  delete_insn (insn);
  
  insert_insn_on_edge (
	gen_divmod_fixed_value (mode, code, set_dest,
				op1, op2, val, prob), e);

  return true;
}

/* Generate code for transformation 2 (with MODE and OPERATION, operands OP1
   and OP2, result TARGET and probability of taking the optimal path PROB).  */
static rtx
gen_mod_pow2 (enum machine_mode mode, enum rtx_code operation, rtx target,
	      rtx op1, rtx op2, int prob)
{
  rtx tmp, tmp1, tmp2, tmp3, jump;
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

  /* Add branch probability to jump we just created.  */
  jump = get_last_insn ();
  REG_NOTES (jump) = gen_rtx_EXPR_LIST (REG_BR_PROB,
					GEN_INT (REG_BR_PROB_BASE - prob),
					REG_NOTES (jump));

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
  int i, all, prob;

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

  if (dump_file)
    fprintf (dump_file, "Mod power of 2 transformation on insn %d\n",
	     INSN_UID (insn));

  /* Compute probability of taking the optimal path.  */
  all = count + wrong_values;
  prob = (count * REG_BR_PROB_BASE + all / 2) / all;

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  delete_insn (insn);
  
  insert_insn_on_edge (
	gen_mod_pow2 (mode, code, set_dest, op1, op2, prob), e);

  return true;
}

/* Generate code for transformations 3 and 4 (with MODE and OPERATION,
   operands OP1 and OP2, result TARGET, at most SUB subtractions, and
   probability of taking the optimal path(s) PROB1 and PROB2).  */
static rtx
gen_mod_subtract (enum machine_mode mode, enum rtx_code operation,
		  rtx target, rtx op1, rtx op2, int sub, int prob1, int prob2)
{
  rtx tmp, tmp1, jump;
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

  /* Add branch probability to jump we just created.  */
  jump = get_last_insn ();
  REG_NOTES (jump) = gen_rtx_EXPR_LIST (REG_BR_PROB,
					GEN_INT (prob1), REG_NOTES (jump));

  for (i = 0; i < sub; i++)
    {
      tmp1 = expand_simple_binop (mode, MINUS, target, tmp, target,
	    			  0, OPTAB_WIDEN);
      if (tmp1 != target)
	emit_move_insn (target, tmp1);
      do_compare_rtx_and_jump (target, tmp, LTU, 0, mode, NULL_RTX,
    			       NULL_RTX, end_label);

      /* Add branch probability to jump we just created.  */
      jump = get_last_insn ();
      REG_NOTES (jump) = gen_rtx_EXPR_LIST (REG_BR_PROB,
					    GEN_INT (prob2), REG_NOTES (jump));
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
  int i, prob1, prob2;

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

  if (dump_file)
    fprintf (dump_file, "Mod subtract transformation on insn %d\n",
	     INSN_UID (insn));

  /* Compute probability of taking the optimal path(s).  */
  prob1 = (counts[0] * REG_BR_PROB_BASE + all / 2) / all;
  prob2 = (counts[1] * REG_BR_PROB_BASE + all / 2) / all;

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  delete_insn (insn);
  
  insert_insn_on_edge (
	gen_mod_subtract (mode, code, set_dest,
			  op1, op2, i, prob1, prob2), e);

  return true;
}

#ifdef HAVE_prefetch
/* Generate code for transformation 5 for mem with ADDRESS and a constant
   step DELTA.  WRITE is true if the reference is a store to mem.  */

static rtx
gen_speculative_prefetch (rtx address, gcov_type delta, int write)
{
  rtx tmp;
  rtx sequence;

  /* TODO: we do the prefetching for just one iteration ahead, which
     often is not enough.  */
  start_sequence ();
  if (offsettable_address_p (0, VOIDmode, address))
    tmp = plus_constant (copy_rtx (address), delta);
  else
    {
      tmp = simplify_gen_binary (PLUS, Pmode,
				 copy_rtx (address), GEN_INT (delta));
      tmp = force_operand (tmp, NULL);
    }
  if (! (*insn_data[(int)CODE_FOR_prefetch].operand[0].predicate)
      (tmp, insn_data[(int)CODE_FOR_prefetch].operand[0].mode))
    tmp = force_reg (Pmode, tmp);
  emit_insn (gen_prefetch (tmp, GEN_INT (write), GEN_INT (3)));
  sequence = get_insns ();
  end_sequence ();

  return sequence;
}

/* Do transform 5) on INSN if applicable.  */

static bool
speculative_prefetching_transform (rtx insn)
{
  rtx histogram, value;
  gcov_type val, count, all;
  edge e;
  rtx mem, address;
  int write;

  if (!maybe_hot_bb_p (BLOCK_FOR_INSN (insn)))
    return false;

  if (!find_mem_reference (insn, &mem, &write))
    return false;

  address = XEXP (mem, 0);
  if (side_effects_p (address))
    return false;
      
  if (CONSTANT_P (address))
    return false;

  for (histogram = REG_NOTES (insn);
       histogram;
       histogram = XEXP (histogram, 1))
    if (REG_NOTE_KIND (histogram) == REG_VALUE_PROFILE
	&& XEXP (XEXP (histogram, 0), 0) == GEN_INT (HIST_TYPE_CONST_DELTA))
      break;

  if (!histogram)
    return false;

  histogram = XEXP (XEXP (histogram, 0), 1);
  value = XEXP (histogram, 0);
  histogram = XEXP (histogram, 1);
  /* Skip last value referenced.  */
  histogram = XEXP (histogram, 1);
  val = INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);
  count = INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);
  all = INTVAL (XEXP (histogram, 0));

  /* With that few executions we do not really have a reason to optimize the
     statement, and more importantly, the data about differences of addresses
     are spoiled by the first item that had no previous value to compare
     with.  */
  if (all < 4)
    return false;

  /* We require that count be at least half of all; this means
     that for the transformation to fire the value must be constant
     at least 50% of time (and 75% gives the guarantee of usage).  */
  if (!rtx_equal_p (address, value) || 2 * count < all)
    return false;

  /* If the difference is too small, it does not make too much sense to
     prefetch, as the memory is probably already in cache.  */
  if (val >= NOPREFETCH_RANGE_MIN && val <= NOPREFETCH_RANGE_MAX)
    return false;

  if (dump_file)
    fprintf (dump_file, "Speculative prefetching for insn %d\n",
	     INSN_UID (insn));

  e = split_block (BLOCK_FOR_INSN (insn), PREV_INSN (insn));
  
  insert_insn_on_edge (gen_speculative_prefetch (address, val, write), e);

  return true;
}
#endif  /* HAVE_prefetch */

/* Connection to the outside world.  */
/* Struct for IR-dependent hooks.  */
struct value_prof_hooks {
  /* Find list of values for which we want to measure histograms.  */
  void (*find_values_to_profile) (histogram_values *);

  /* Identify and exploit properties of values that are hard to analyze
     statically.  See value-prof.c for more detail.  */
  bool (*value_profile_transformations) (void);  
};

/* Hooks for RTL-based versions (the only ones that currently work).  */
static struct value_prof_hooks rtl_value_prof_hooks =
{
  rtl_find_values_to_profile,
  rtl_value_profile_transformations
};

void 
rtl_register_value_prof_hooks (void)
{
  value_prof_hooks = &rtl_value_prof_hooks;
  if (ir_type ())
    abort ();
}

/* Tree-based versions are stubs for now.  */
static void
tree_find_values_to_profile (histogram_values *values ATTRIBUTE_UNUSED)
{
  abort ();
}

static bool
tree_value_profile_transformations (void)
{
  abort ();
}

static struct value_prof_hooks tree_value_prof_hooks = {
  tree_find_values_to_profile,
  tree_value_profile_transformations
};

void
tree_register_value_prof_hooks (void)
{
  value_prof_hooks = &tree_value_prof_hooks;
  if (!ir_type ())
    abort ();
}

/* IR-independent entry points.  */
void
find_values_to_profile (histogram_values *values)
{
  (value_prof_hooks->find_values_to_profile) (values);
}

bool
value_profile_transformations (void)
{
  return (value_prof_hooks->value_profile_transformations) ();
}

