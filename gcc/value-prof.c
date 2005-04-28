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
#include "tree-flow.h"
#include "tree-flow-inline.h"
#include "diagnostic.h"
#include "coverage.h"
#include "tree.h"
#include "gcov-io.h"

static struct value_prof_hooks *value_prof_hooks;

/* This is the vector of histograms.  Created in find_values_to_profile.
   During profile generation, freed by instrument_values.
   During profile use, freed by value_profile_transformations.  */

static histogram_values static_values = NULL;

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

   There are currently two versions, RTL-based and tree-based.  Over time
   the RTL-based version may go away.  

   In the RTL-based version, the measured data is appended as REG_VALUE_PROFILE 
   note to the instrumented insn.  The argument to the note consists of an
   EXPR_LIST where its members have the following meaning (from the first to 
   the last):
   
   -- type of information gathered (HIST_TYPE*)
   -- the expression that is profiled
   -- list of counters starting from the first one.

   In the tree-based version, the measured data is pointed to from the histograms
   field of the statement annotation of the instrumented insns.  It is
   kept as a linked list of struct histogram_value_t's, which contain the
   same information as above.  */

/* For speculative prefetching, the range in that we do not prefetch (because
   we assume that it will be in cache anyway).  The asymmetry between min and
   max range is trying to reflect the fact that the sequential prefetching
   of the data is commonly done directly by hardware.  Nevertheless, these
   values are just a guess and should of course be target-specific.  

   FIXME:  There is no tree form of speculative prefetching as yet.

   FIXME:  A better approach to instrumentation in the profile-generation
   pass is to generate calls to magic library functions (to be added to
   libgcc) rather than inline code.  This approach will probably be
   necessary to get tree-based speculative prefetching working in a useful
   fashion, as inline code bloats things so much the rest of the compiler has
   serious problems dealing with it (judging from the rtl behavior).  */

#ifndef NOPREFETCH_RANGE_MIN
#define NOPREFETCH_RANGE_MIN (-16)
#endif
#ifndef NOPREFETCH_RANGE_MAX
#define NOPREFETCH_RANGE_MAX 32
#endif

static void rtl_divmod_values_to_profile (rtx, histogram_values *);
#ifdef HAVE_prefetch
static bool insn_prefetch_values_to_profile (rtx, histogram_values *);
static int find_mem_reference_1 (rtx *, void *);
static void find_mem_reference_2 (rtx, rtx, void *);
static bool find_mem_reference (rtx, rtx *, int *);
#endif

static void rtl_values_to_profile (rtx, histogram_values *);
static rtx rtl_divmod_fixed_value (enum machine_mode, enum rtx_code, rtx, rtx,
				   rtx, gcov_type, int);
static rtx rtl_mod_pow2 (enum machine_mode, enum rtx_code, rtx, rtx, rtx, int);
static rtx rtl_mod_subtract (enum machine_mode, enum rtx_code, rtx, rtx, rtx,
			     int, int, int);
#ifdef HAVE_prefetch
static rtx gen_speculative_prefetch (rtx, gcov_type, int);
#endif
static bool rtl_divmod_fixed_value_transform (rtx);
static bool rtl_mod_pow2_value_transform (rtx);
static bool rtl_mod_subtract_transform (rtx);
#ifdef HAVE_prefetch
static bool speculative_prefetching_transform (rtx);
#endif
static tree tree_divmod_fixed_value (tree, tree, tree, tree, 
				    tree, int, gcov_type, gcov_type);
static tree tree_mod_pow2 (tree, tree, tree, tree, int, gcov_type, gcov_type);
static tree tree_mod_subtract (tree, tree, tree, tree, int, int, int,
				gcov_type, gcov_type, gcov_type);
static bool tree_divmod_fixed_value_transform (tree);
static bool tree_mod_pow2_value_transform (tree);
static bool tree_mod_subtract_transform (tree);


/* Find values inside INSN for that we want to measure histograms for
   division/modulo optimization and stores them to VALUES.  */
static void
rtl_divmod_values_to_profile (rtx insn, histogram_values *values)
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
	  hist->hvalue.rtl.value = op2;
	  hist->hvalue.rtl.seq = NULL_RTX;
	  hist->hvalue.rtl.mode = mode;
	  hist->hvalue.rtl.insn = insn;
	  hist->type = HIST_TYPE_POW2;
	  VEC_safe_push (histogram_value, heap, *values, hist);
	}

      /* Check whether the divisor is not in fact a constant.  */
      if (!CONSTANT_P (op2))
	{
	  hist = ggc_alloc (sizeof (*hist));
	  hist->hvalue.rtl.value = op2;
	  hist->hvalue.rtl.mode = mode;
	  hist->hvalue.rtl.seq = NULL_RTX;
	  hist->hvalue.rtl.insn = insn;
	  hist->type = HIST_TYPE_SINGLE_VALUE;
	  VEC_safe_push (histogram_value, heap, *values, hist);
	}

      /* For mod, check whether it is not often a noop (or replaceable by
	 a few subtractions).  */
      if (GET_CODE (set_src) == UMOD && !side_effects_p (op1))
	{
	  rtx tmp;

	  hist = ggc_alloc (sizeof (*hist));
	  start_sequence ();
	  tmp = simplify_gen_binary (DIV, mode, copy_rtx (op1), copy_rtx (op2));
	  hist->hvalue.rtl.value = force_operand (tmp, NULL_RTX);
	  hist->hvalue.rtl.seq = get_insns ();
	  end_sequence ();
	  hist->hvalue.rtl.mode = mode;
	  hist->hvalue.rtl.insn = insn;
	  hist->type = HIST_TYPE_INTERVAL;
	  hist->hdata.intvl.int_start = 0;
	  hist->hdata.intvl.steps = 2;
	  VEC_safe_push (histogram_value, heap, *values, hist);
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

  if (MEM_P (*expr))
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
insn_prefetch_values_to_profile (rtx insn, histogram_values* values)
{
  rtx mem, address;
  int write;
  histogram_value hist;

  /* It only makes sense to look for memory references in ordinary insns.  */
  if (!NONJUMP_INSN_P (insn))
    return false;

  if (!find_mem_reference (insn, &mem, &write))
    return false;

  address = XEXP (mem, 0);
  if (side_effects_p (address))
    return false;
      
  if (CONSTANT_P (address))
    return false;

  hist = ggc_alloc (sizeof (*hist));
  hist->hvalue.rtl.value = address;
  hist->hvalue.rtl.mode = GET_MODE (address);
  hist->hvalue.rtl.seq = NULL_RTX;
  hist->hvalue.rtl.insn = insn;
  hist->type = HIST_TYPE_CONST_DELTA;
  VEC_safe_push (histogram_value, heap, *values, hist);

  return true;
}
#endif
/* Find values inside INSN for that we want to measure histograms and adds
   them to list VALUES (increasing the record of its length in N_VALUES).  */
static void
rtl_values_to_profile (rtx insn, histogram_values *values)
{
  if (flag_value_profile_transformations)
    rtl_divmod_values_to_profile (insn, values);

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
  histogram_value hist;

  life_analysis (NULL, PROP_DEATH_NOTES);

  *values = NULL;
  libcall_level = 0;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    rtl_values_to_profile (insn, values);
  static_values = *values;

  for (i = 0; VEC_iterate (histogram_value, *values, i, hist); i++)
    {
      switch (hist->type)
	{
	case HIST_TYPE_INTERVAL:
	  if (dump_file)
	    fprintf (dump_file,
		     "Interval counter for insn %d, range %d -- %d.\n",
		     INSN_UID ((rtx)hist->hvalue.rtl.insn),
		     hist->hdata.intvl.int_start,
		     (hist->hdata.intvl.int_start
		      + hist->hdata.intvl.steps - 1));
	  hist->n_counters = hist->hdata.intvl.steps + 2;
	  break;

	case HIST_TYPE_POW2:
	  if (dump_file)
	    fprintf (dump_file,
		     "Pow2 counter for insn %d.\n",
		     INSN_UID ((rtx)hist->hvalue.rtl.insn));
	  hist->n_counters = 2;
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  if (dump_file)
	    fprintf (dump_file,
		     "Single value counter for insn %d.\n",
		     INSN_UID ((rtx)hist->hvalue.rtl.insn));
	  hist->n_counters = 3;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  if (dump_file)
	    fprintf (dump_file,
		     "Constant delta counter for insn %d.\n",
		     INSN_UID ((rtx)hist->hvalue.rtl.insn));
	  hist->n_counters = 4;
	  break;

	default:
	  gcc_unreachable ();
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
	  && (rtl_mod_subtract_transform (insn)
	      || rtl_divmod_fixed_value_transform (insn)
	      || rtl_mod_pow2_value_transform (insn)))
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
rtl_divmod_fixed_value (enum machine_mode mode, enum rtx_code operation,
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
rtl_divmod_fixed_value_transform (rtx insn)
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
	rtl_divmod_fixed_value (mode, code, set_dest,
				op1, op2, val, prob), e);

  return true;
}

/* Generate code for transformation 2 (with MODE and OPERATION, operands OP1
   and OP2, result TARGET and probability of taking the optimal path PROB).  */
static rtx
rtl_mod_pow2 (enum machine_mode mode, enum rtx_code operation, rtx target,
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
rtl_mod_pow2_value_transform (rtx insn)
{
  rtx set, set_src, set_dest, op1, op2, value, histogram;
  enum rtx_code code;
  enum machine_mode mode;
  gcov_type wrong_values, count;
  edge e;
  int all, prob;

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
  wrong_values = INTVAL (XEXP (histogram, 0));
  histogram = XEXP (histogram, 1);
  count = INTVAL (XEXP (histogram, 0));

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
	rtl_mod_pow2 (mode, code, set_dest, op1, op2, prob), e);

  return true;
}

/* Generate code for transformations 3 and 4 (with MODE and OPERATION,
   operands OP1 and OP2, result TARGET, at most SUB subtractions, and
   probability of taking the optimal path(s) PROB1 and PROB2).  */
static rtx
rtl_mod_subtract (enum machine_mode mode, enum rtx_code operation,
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
rtl_mod_subtract_transform (rtx insn)
{
  rtx set, set_src, set_dest, op1, op2, histogram;
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
	rtl_mod_subtract (mode, code, set_dest,
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

/* Tree based transformations. */
static bool
tree_value_profile_transformations (void)
{
  basic_block bb;
  block_stmt_iterator bsi;
  bool changed = false;

  FOR_EACH_BB (bb)
    {
      /* Ignore cold areas -- we are enlarging the code.  */
      if (!maybe_hot_bb_p (bb))
	continue;

      for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	{
	  tree stmt = bsi_stmt (bsi);
	  stmt_ann_t ann = get_stmt_ann (stmt);
	  histogram_value th = ann->histograms;
	  if (!th)
	    continue;

	  if (dump_file)
	    {
	      fprintf (dump_file, "Trying transformations on insn ");
	      print_generic_stmt (dump_file, stmt, TDF_SLIM);
	    }

	  /* Transformations:  */
	  /* The order of things in this conditional controls which
	     transformation is used when more than one is applicable.  */
	  /* It is expected that any code added by the transformations
	     will be added before the current statement, and that the
	     current statement remain valid (although possibly
	     modified) upon return.  */
	  if (flag_value_profile_transformations
	      && (tree_mod_subtract_transform (stmt)
		  || tree_divmod_fixed_value_transform (stmt)
		  || tree_mod_pow2_value_transform (stmt)))
	    {
	      changed = true;
	      /* Original statement may no longer be in the same block. */
	      bb = bb_for_stmt (stmt);
	    }

	  /* Free extra storage from compute_value_histograms.  */
	  while (th)
	    {
	      free (th->hvalue.tree.counters);
	      th = th->hvalue.tree.next;
	    }
	  ann->histograms = 0;
        }
    }

  if (changed)
    {
      counts_to_freqs ();
    }

  return changed;
}

/* Generate code for transformation 1 (with OPERATION, operands OP1
   and OP2, whose value is expected to be VALUE, parent modify-expr STMT and
   probability of taking the optimal path PROB, which is equivalent to COUNT/ALL
   within roundoff error).  This generates the result into a temp and returns 
   the temp; it does not replace or alter the original STMT.  */
static tree
tree_divmod_fixed_value (tree stmt, tree operation, 
			 tree op1, tree op2, tree value, int prob, gcov_type count,
			 gcov_type all)
{
  tree stmt1, stmt2, stmt3;
  tree tmp1, tmp2, tmpv;
  tree label_decl1 = create_artificial_label ();
  tree label_decl2 = create_artificial_label ();
  tree label_decl3 = create_artificial_label ();
  tree label1, label2, label3;
  tree bb1end, bb2end, bb3end;
  basic_block bb, bb2, bb3, bb4;
  tree optype = TREE_TYPE (operation);
  edge e12, e13, e23, e24, e34;
  block_stmt_iterator bsi;

  bb = bb_for_stmt (stmt);
  bsi = bsi_for_stmt (stmt);

  tmpv = create_tmp_var (optype, "PROF");
  tmp1 = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, optype, tmpv, fold_convert (optype, value));
  stmt2 = build2 (MODIFY_EXPR, optype, tmp1, op2);
  stmt3 = build3 (COND_EXPR, void_type_node,
	    build2 (NE_EXPR, boolean_type_node, tmp1, tmpv),
	    build1 (GOTO_EXPR, void_type_node, label_decl2),
	    build1 (GOTO_EXPR, void_type_node, label_decl1));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bb1end = stmt3;

  tmp2 = create_tmp_var (optype, "PROF");
  label1 = build1 (LABEL_EXPR, void_type_node, label_decl1);
  stmt1 = build2 (MODIFY_EXPR, optype, tmp2,
		  build2 (TREE_CODE (operation), optype, op1, tmpv));
  bsi_insert_before (&bsi, label1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb2end = stmt1;

  label2 = build1 (LABEL_EXPR, void_type_node, label_decl2);
  stmt1 = build2 (MODIFY_EXPR, optype, tmp2,
		  build2 (TREE_CODE (operation), optype, op1, op2));
  bsi_insert_before (&bsi, label2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb3end = stmt1;

  label3 = build1 (LABEL_EXPR, void_type_node, label_decl3);
  bsi_insert_before (&bsi, label3, BSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e23 connects bb2 to bb3, etc. */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  bb2->count = count;
  e23 = split_block (bb2, bb2end);
  bb3 = e23->dest;
  bb3->count = all - count;
  e34 = split_block (bb3, bb3end);
  bb4 = e34->dest;
  bb4->count = all;

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  e12->probability = prob;
  e12->count = count;

  e13 = make_edge (bb, bb3, EDGE_TRUE_VALUE);
  e13->probability = REG_BR_PROB_BASE - prob;
  e13->count = all - count;

  remove_edge (e23);
  
  e24 = make_edge (bb2, bb4, EDGE_FALLTHRU);
  e24->probability = REG_BR_PROB_BASE;
  e24->count = count;

  e34->probability = REG_BR_PROB_BASE;
  e34->count = all - count;

  return tmp2;
}

/* Do transform 1) on INSN if applicable.  */
static bool
tree_divmod_fixed_value_transform (tree stmt)
{
  stmt_ann_t ann = get_stmt_ann (stmt);
  histogram_value histogram;
  enum tree_code code;
  gcov_type val, count, all;
  tree modify, op, op1, op2, result, value, tree_val;
  int prob;

  modify = stmt;
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_OPERAND (stmt, 0)
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
    modify = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (modify) != MODIFY_EXPR)
    return false;
  op = TREE_OPERAND (modify, 1);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (op)))
    return false;
  code = TREE_CODE (op);
  
  if (code != TRUNC_DIV_EXPR && code != TRUNC_MOD_EXPR)
    return false;

  op1 = TREE_OPERAND (op, 0);
  op2 = TREE_OPERAND (op, 1);
  if (!ann->histograms)
    return false;

  for (histogram = ann->histograms; histogram; histogram = histogram->hvalue.tree.next)
    if (histogram->type == HIST_TYPE_SINGLE_VALUE)
      break;

  if (!histogram)
    return false;

  value = histogram->hvalue.tree.value;
  val = histogram->hvalue.tree.counters[0];
  count = histogram->hvalue.tree.counters[1];
  all = histogram->hvalue.tree.counters[2];

  /* We require that count is at least half of all; this means
     that for the transformation to fire the value must be constant
     at least 50% of time (and 75% gives the guarantee of usage).  */
  if (simple_cst_equal (op2, value) != 1 || 2 * count < all)
    return false;

  if (dump_file)
    {
      fprintf (dump_file, "Div/mod by constant transformation on insn ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
    }

  /* Compute probability of taking the optimal path.  */
  prob = (count * REG_BR_PROB_BASE + all / 2) / all;

  tree_val = build_int_cst_wide (get_gcov_type (),
				 (unsigned HOST_WIDE_INT) val,
				 val >> (HOST_BITS_PER_WIDE_INT - 1) >> 1);
  result = tree_divmod_fixed_value (stmt, op, op1, op2, tree_val, prob, count, all);

  TREE_OPERAND (modify, 1) = result;

  return true;
}

/* Generate code for transformation 2 (with OPERATION, operands OP1
   and OP2, parent modify-expr STMT and probability of taking the optimal 
   path PROB, which is equivalent to COUNT/ALL within roundoff error).  
   This generates the result into a temp and returns 
   the temp; it does not replace or alter the original STMT.  */
static tree
tree_mod_pow2 (tree stmt, tree operation, tree op1, tree op2, int prob, 
	       gcov_type count, gcov_type all)
{
  tree stmt1, stmt2, stmt3, stmt4;
  tree tmp1, tmp2, tmp3;
  tree label_decl1 = create_artificial_label ();
  tree label_decl2 = create_artificial_label ();
  tree label_decl3 = create_artificial_label ();
  tree label1, label2, label3;
  tree bb1end, bb2end, bb3end;
  basic_block bb, bb2, bb3, bb4;
  tree optype = TREE_TYPE (operation);
  edge e12, e13, e23, e24, e34;
  block_stmt_iterator bsi;
  tree result = create_tmp_var (optype, "PROF");

  bb = bb_for_stmt (stmt);
  bsi = bsi_for_stmt (stmt);

  tmp1 = create_tmp_var (optype, "PROF");
  tmp2 = create_tmp_var (optype, "PROF");
  tmp3 = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, optype, tmp1, fold_convert (optype, op2));
  stmt2 = build2 (MODIFY_EXPR, optype, tmp2, 
		    build2 (PLUS_EXPR, optype, op2, integer_minus_one_node));
  stmt3 = build2 (MODIFY_EXPR, optype, tmp3,
		    build2 (BIT_AND_EXPR, optype, tmp2, tmp1));
  stmt4 = build3 (COND_EXPR, void_type_node,
	    build2 (NE_EXPR, boolean_type_node, tmp3, integer_zero_node),
	    build1 (GOTO_EXPR, void_type_node, label_decl2),
	    build1 (GOTO_EXPR, void_type_node, label_decl1));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt4, BSI_SAME_STMT);
  bb1end = stmt4;

  /* tmp2 == op2-1 inherited from previous block */
  label1 = build1 (LABEL_EXPR, void_type_node, label_decl1);
  stmt1 = build2 (MODIFY_EXPR, optype, result,
		  build2 (BIT_AND_EXPR, optype, op1, tmp2));
  bsi_insert_before (&bsi, label1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb2end = stmt1;

  label2 = build1 (LABEL_EXPR, void_type_node, label_decl2);
  stmt1 = build2 (MODIFY_EXPR, optype, result,
		  build2 (TREE_CODE (operation), optype, op1, op2));
  bsi_insert_before (&bsi, label2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb3end = stmt1;

  label3 = build1 (LABEL_EXPR, void_type_node, label_decl3);
  bsi_insert_before (&bsi, label3, BSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e23 connects bb2 to bb3, etc. */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  bb2->count = count;
  e23 = split_block (bb2, bb2end);
  bb3 = e23->dest;
  bb3->count = all - count;
  e34 = split_block (bb3, bb3end);
  bb4 = e34->dest;
  bb4->count = all;

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  e12->probability = prob;
  e12->count = count;

  e13 = make_edge (bb, bb3, EDGE_TRUE_VALUE);
  e13->probability = REG_BR_PROB_BASE - prob;
  e13->count = all - count;

  remove_edge (e23);
  
  e24 = make_edge (bb2, bb4, EDGE_FALLTHRU);
  e24->probability = REG_BR_PROB_BASE;
  e24->count = count;

  e34->probability = REG_BR_PROB_BASE;
  e34->count = all - count;

  return result;
}

/* Do transform 2) on INSN if applicable.  */
static bool
tree_mod_pow2_value_transform (tree stmt)
{
  stmt_ann_t ann = get_stmt_ann (stmt);
  histogram_value histogram;
  enum tree_code code;
  gcov_type count, wrong_values, all;
  tree modify, op, op1, op2, result, value;
  int prob;

  modify = stmt;
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_OPERAND (stmt, 0)
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
    modify = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (modify) != MODIFY_EXPR)
    return false;
  op = TREE_OPERAND (modify, 1);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (op)))
    return false;
  code = TREE_CODE (op);
  
  if (code != TRUNC_MOD_EXPR || !TYPE_UNSIGNED (TREE_TYPE (op)))
    return false;

  op1 = TREE_OPERAND (op, 0);
  op2 = TREE_OPERAND (op, 1);
  if (!ann->histograms)
    return false;

  for (histogram = ann->histograms; histogram; histogram = histogram->hvalue.tree.next)
    if (histogram->type == HIST_TYPE_POW2)
      break;

  if (!histogram)
    return false;

  value = histogram->hvalue.tree.value;
  wrong_values = histogram->hvalue.tree.counters[0];
  count = histogram->hvalue.tree.counters[1];

  /* We require that we hit a power of 2 at least half of all evaluations.  */
  if (simple_cst_equal (op2, value) != 1 || count < wrong_values)
    return false;

  if (dump_file)
    {
      fprintf (dump_file, "Mod power of 2 transformation on insn ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
    }

  /* Compute probability of taking the optimal path.  */
  all = count + wrong_values;
  prob = (count * REG_BR_PROB_BASE + all / 2) / all;

  result = tree_mod_pow2 (stmt, op, op1, op2, prob, count, all);

  TREE_OPERAND (modify, 1) = result;

  return true;
}

/* Generate code for transformations 3 and 4 (with OPERATION, operands OP1
   and OP2, parent modify-expr STMT, and NCOUNTS the number of cases to
   support.  Currently only NCOUNTS==0 or 1 is supported and this is
   built into this interface.  The probabilities of taking the optimal 
   paths are PROB1 and PROB2, which are equivalent to COUNT1/ALL and
   COUNT2/ALL respectively within roundoff error).  This generates the 
   result into a temp and returns the temp; it does not replace or alter 
   the original STMT.  */
/* FIXME: Generalize the interface to handle NCOUNTS > 1.  */

static tree
tree_mod_subtract (tree stmt, tree operation, tree op1, tree op2, 
		    int prob1, int prob2, int ncounts,
		    gcov_type count1, gcov_type count2, gcov_type all)
{
  tree stmt1, stmt2, stmt3;
  tree tmp1;
  tree label_decl1 = create_artificial_label ();
  tree label_decl2 = create_artificial_label ();
  tree label_decl3 = create_artificial_label ();
  tree label1, label2, label3;
  tree bb1end, bb2end = NULL_TREE, bb3end;
  basic_block bb, bb2, bb3, bb4;
  tree optype = TREE_TYPE (operation);
  edge e12, e23 = 0, e24, e34, e14;
  block_stmt_iterator bsi;
  tree result = create_tmp_var (optype, "PROF");

  bb = bb_for_stmt (stmt);
  bsi = bsi_for_stmt (stmt);

  tmp1 = create_tmp_var (optype, "PROF");
  stmt1 = build2 (MODIFY_EXPR, optype, result, op1);
  stmt2 = build2 (MODIFY_EXPR, optype, tmp1, op2);
  stmt3 = build3 (COND_EXPR, void_type_node,
	    build2 (LT_EXPR, boolean_type_node, result, tmp1),
	    build1 (GOTO_EXPR, void_type_node, label_decl3),
	    build1 (GOTO_EXPR, void_type_node, 
		    ncounts ? label_decl1 : label_decl2));
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt3, BSI_SAME_STMT);
  bb1end = stmt3;

  if (ncounts)	/* Assumed to be 0 or 1 */
    {
      label1 = build1 (LABEL_EXPR, void_type_node, label_decl1);
      stmt1 = build2 (MODIFY_EXPR, optype, result,
		      build2 (MINUS_EXPR, optype, result, tmp1));
      stmt2 = build3 (COND_EXPR, void_type_node,
		build2 (LT_EXPR, boolean_type_node, result, tmp1),
		build1 (GOTO_EXPR, void_type_node, label_decl3),
		build1 (GOTO_EXPR, void_type_node, label_decl2));
      bsi_insert_before (&bsi, label1, BSI_SAME_STMT);
      bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
      bsi_insert_before (&bsi, stmt2, BSI_SAME_STMT);
      bb2end = stmt2;
    }

  /* Fallback case. */
  label2 = build1 (LABEL_EXPR, void_type_node, label_decl2);
  stmt1 = build2 (MODIFY_EXPR, optype, result,
		    build2 (TREE_CODE (operation), optype, result, tmp1));
  bsi_insert_before (&bsi, label2, BSI_SAME_STMT);
  bsi_insert_before (&bsi, stmt1, BSI_SAME_STMT);
  bb3end = stmt1;

  label3 = build1 (LABEL_EXPR, void_type_node, label_decl3);
  bsi_insert_before (&bsi, label3, BSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e23 connects bb2 to bb3, etc. */
  /* However block 3 is optional; if it is not there, references
     to 3 really refer to block 2. */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  bb2->count = all - count1;
    
  if (ncounts)	/* Assumed to be 0 or 1.  */
    {
      e23 = split_block (bb2, bb2end);
      bb3 = e23->dest;
      bb3->count = all - count1 - count2;
    }

  e34 = split_block (ncounts ? bb3 : bb2, bb3end);
  bb4 = e34->dest;
  bb4->count = all;

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  e12->probability = REG_BR_PROB_BASE - prob1;
  e12->count = count1;

  e14 = make_edge (bb, bb4, EDGE_TRUE_VALUE);
  e14->probability = prob1;
  e14->count = all - count1;

  if (ncounts)  /* Assumed to be 0 or 1.  */
    {
      e23->flags &= ~EDGE_FALLTHRU;
      e23->flags |= EDGE_FALSE_VALUE;
      e23->count = all - count1 - count2;
      e23->probability = REG_BR_PROB_BASE - prob2;

      e24 = make_edge (bb2, bb4, EDGE_TRUE_VALUE);
      e24->probability = prob2;
      e24->count = count2;
    }

  e34->probability = REG_BR_PROB_BASE;
  e34->count = all - count1 - count2;

  return result;
}

/* Do transforms 3) and 4) on INSN if applicable.  */
static bool
tree_mod_subtract_transform (tree stmt)
{
  stmt_ann_t ann = get_stmt_ann (stmt);
  histogram_value histogram;
  enum tree_code code;
  gcov_type count, wrong_values, all;
  tree modify, op, op1, op2, result, value;
  int prob1, prob2;
  unsigned int i;

  modify = stmt;
  if (TREE_CODE (stmt) == RETURN_EXPR
      && TREE_OPERAND (stmt, 0)
      && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
    modify = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (modify) != MODIFY_EXPR)
    return false;
  op = TREE_OPERAND (modify, 1);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (op)))
    return false;
  code = TREE_CODE (op);
  
  if (code != TRUNC_MOD_EXPR || !TYPE_UNSIGNED (TREE_TYPE (op)))
    return false;

  op1 = TREE_OPERAND (op, 0);
  op2 = TREE_OPERAND (op, 1);
  if (!ann->histograms)
    return false;

  for (histogram = ann->histograms; histogram; histogram = histogram->hvalue.tree.next)
    if (histogram->type == HIST_TYPE_INTERVAL)
      break;

  if (!histogram)
    return false;

  value = histogram->hvalue.tree.value;
  all = 0;
  wrong_values = 0;
  for (i = 0; i < histogram->hdata.intvl.steps; i++)
    all += histogram->hvalue.tree.counters[i];

  wrong_values += histogram->hvalue.tree.counters[i];
  wrong_values += histogram->hvalue.tree.counters[i+1];
  all += wrong_values;

  /* We require that we use just subtractions in at least 50% of all
     evaluations.  */
  count = 0;
  for (i = 0; i < histogram->hdata.intvl.steps; i++)
    {
      count += histogram->hvalue.tree.counters[i];
      if (count * 2 >= all)
	break;
    }
  if (i == histogram->hdata.intvl.steps)
    return false;

  if (dump_file)
    {
      fprintf (dump_file, "Mod subtract transformation on insn ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
    }

  /* Compute probability of taking the optimal path(s).  */
  prob1 = (histogram->hvalue.tree.counters[0] * REG_BR_PROB_BASE + all / 2) / all;
  prob2 = (histogram->hvalue.tree.counters[1] * REG_BR_PROB_BASE + all / 2) / all;

  /* In practice, "steps" is always 2.  This interface reflects this,
     and will need to be changed if "steps" can change.  */
  result = tree_mod_subtract (stmt, op, op1, op2, prob1, prob2, i,
			    histogram->hvalue.tree.counters[0], 
			    histogram->hvalue.tree.counters[1], all);

  TREE_OPERAND (modify, 1) = result;

  return true;
}

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
  gcc_assert (!ir_type ());
}

/* Find values inside STMT for that we want to measure histograms for
   division/modulo optimization.  */
static void
tree_divmod_values_to_profile (tree stmt, histogram_values *values)
{
  tree assign, lhs, rhs, divisor, op0, type;
  histogram_value hist;

  if (TREE_CODE (stmt) == RETURN_EXPR)
    assign = TREE_OPERAND (stmt, 0);
  else
    assign = stmt;

  if (!assign
      || TREE_CODE (assign) != MODIFY_EXPR)
    return;
  lhs = TREE_OPERAND (assign, 0);
  type = TREE_TYPE (lhs);
  if (!INTEGRAL_TYPE_P (type))
    return;

  rhs = TREE_OPERAND (assign, 1);
  switch (TREE_CODE (rhs))
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
      divisor = TREE_OPERAND (rhs, 1);
      op0 = TREE_OPERAND (rhs, 0);

      VEC_reserve (histogram_value, heap, *values, 3);

      if (is_gimple_reg (divisor))
	{
	  /* Check for a special case where the divisor is power(s) of 2.
	     This is more aggressive than the RTL version, under the
	     assumption that later phases will reduce / or % by power of 2
	     to something clever most of the time.  Signed or unsigned.  */
	  hist = ggc_alloc (sizeof (*hist));
	  hist->hvalue.tree.value = divisor;
	  hist->hvalue.tree.stmt = stmt;
	  hist->type = HIST_TYPE_POW2;
	  VEC_quick_push (histogram_value, *values, hist);

	  /* Check for the case where the divisor is the same value most
	     of the time.  */
	  hist = ggc_alloc (sizeof (*hist));
	  hist->hvalue.tree.value = divisor;
	  hist->hvalue.tree.stmt = stmt;
	  hist->type = HIST_TYPE_SINGLE_VALUE;
	  VEC_quick_push (histogram_value, *values, hist);
	}

      /* For mod, check whether it is not often a noop (or replaceable by
	 a few subtractions).  */
      if (TREE_CODE (rhs) == TRUNC_MOD_EXPR
	  && TYPE_UNSIGNED (type))
	{
	  hist = ggc_alloc (sizeof (*hist));
	  hist->hvalue.tree.stmt = stmt;
	  hist->hvalue.tree.value
		  = build2 (TRUNC_DIV_EXPR, type, op0, divisor);
	  hist->type = HIST_TYPE_INTERVAL;
	  hist->hdata.intvl.int_start = 0;
	  hist->hdata.intvl.steps = 2;
	  VEC_quick_push (histogram_value, *values, hist);
	}
      return;

    default:
      return;
    }
}

/* Find values inside STMT for that we want to measure histograms and adds
   them to list VALUES.  */

static void
tree_values_to_profile (tree stmt, histogram_values *values)
{
  if (flag_value_profile_transformations)
    tree_divmod_values_to_profile (stmt, values);
}

static void
tree_find_values_to_profile (histogram_values *values)
{
  basic_block bb;
  block_stmt_iterator bsi;
  unsigned i;
  histogram_value hist;

  *values = NULL;
  FOR_EACH_BB (bb)
    for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
      tree_values_to_profile (bsi_stmt (bsi), values);
  static_values = *values;
  
  for (i = 0; VEC_iterate (histogram_value, *values, i, hist); i++)
    {
      switch (hist->type)
        {
	case HIST_TYPE_INTERVAL:
	  if (dump_file)
	    {
	      fprintf (dump_file, "Interval counter for tree ");
	      print_generic_expr (dump_file, hist->hvalue.tree.stmt, 
				  TDF_SLIM);
	      fprintf (dump_file, ", range %d -- %d.\n",
		     hist->hdata.intvl.int_start,
		     (hist->hdata.intvl.int_start
		      + hist->hdata.intvl.steps - 1));
	    }
	  hist->n_counters = hist->hdata.intvl.steps + 2;
	  break;

	case HIST_TYPE_POW2:
	  if (dump_file)
	    {
	      fprintf (dump_file, "Pow2 counter for tree ");
	      print_generic_expr (dump_file, hist->hvalue.tree.stmt, TDF_SLIM);
	      fprintf (dump_file, ".\n");
	    }
	  hist->n_counters = 2;
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  if (dump_file)
	    {
	      fprintf (dump_file, "Single value counter for tree ");
	      print_generic_expr (dump_file, hist->hvalue.tree.stmt, TDF_SLIM);
	      fprintf (dump_file, ".\n");
	    }
	  hist->n_counters = 3;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  if (dump_file)
	    {
	      fprintf (dump_file, "Constant delta counter for tree ");
	      print_generic_expr (dump_file, hist->hvalue.tree.stmt, TDF_SLIM);
	      fprintf (dump_file, ".\n");
	    }
	  hist->n_counters = 4;
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

static struct value_prof_hooks tree_value_prof_hooks = {
  tree_find_values_to_profile,
  tree_value_profile_transformations
};

void
tree_register_value_prof_hooks (void)
{
  value_prof_hooks = &tree_value_prof_hooks;
  gcc_assert (ir_type ());
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
  bool retval = (value_prof_hooks->value_profile_transformations) ();
  VEC_free (histogram_value, heap, static_values);
  return retval;
}
