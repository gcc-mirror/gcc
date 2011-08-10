/* Transformations based on profile information for values.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

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
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "coverage.h"
#include "tree.h"
#include "gcov-io.h"
#include "cgraph.h"
#include "timevar.h"
#include "tree-pass.h"
#include "pointer-set.h"
#include "profile.h"

/* In this file value profile based optimizations are placed.  Currently the
   following optimizations are implemented (for more detailed descriptions
   see comments at value_profile_transformations):

   1) Division/modulo specialization.  Provided that we can determine that the
      operands of the division have some special properties, we may use it to
      produce more effective code.
   2) Speculative prefetching.  If we are able to determine that the difference
      between addresses accessed by a memory reference is usually constant, we
      may add the prefetch instructions.
      FIXME: This transformation was removed together with RTL based value
      profiling.

   3) Indirect/virtual call specialization. If we can determine most
      common function callee in indirect/virtual call. We can use this
      information to improve code effectiveness (especially info for
      inliner).

   Every such optimization should add its requirements for profiled values to
   insn_values_to_profile function.  This function is called from branch_prob
   in profile.c and the requested values are instrumented by it in the first
   compilation with -fprofile-arcs.  The optimization may then read the
   gathered data in the second compilation with -fbranch-probabilities.

   The measured data is pointed to from the histograms
   field of the statement annotation of the instrumented insns.  It is
   kept as a linked list of struct histogram_value_t's, which contain the
   same information as above.  */


static tree gimple_divmod_fixed_value (gimple, tree, int, gcov_type, gcov_type);
static tree gimple_mod_pow2 (gimple, int, gcov_type, gcov_type);
static tree gimple_mod_subtract (gimple, int, int, int, gcov_type, gcov_type,
				 gcov_type);
static bool gimple_divmod_fixed_value_transform (gimple_stmt_iterator *);
static bool gimple_mod_pow2_value_transform (gimple_stmt_iterator *);
static bool gimple_mod_subtract_transform (gimple_stmt_iterator *);
static bool gimple_stringops_transform (gimple_stmt_iterator *);
static bool gimple_ic_transform (gimple);

/* Allocate histogram value.  */

static histogram_value
gimple_alloc_histogram_value (struct function *fun ATTRIBUTE_UNUSED,
			      enum hist_type type, gimple stmt, tree value)
{
   histogram_value hist = (histogram_value) xcalloc (1, sizeof (*hist));
   hist->hvalue.value = value;
   hist->hvalue.stmt = stmt;
   hist->type = type;
   return hist;
}

/* Hash value for histogram.  */

static hashval_t
histogram_hash (const void *x)
{
  return htab_hash_pointer (((const_histogram_value)x)->hvalue.stmt);
}

/* Return nonzero if decl_id of die_struct X is the same as UID of decl *Y.  */

static int
histogram_eq (const void *x, const void *y)
{
  return ((const_histogram_value) x)->hvalue.stmt == (const_gimple) y;
}

/* Set histogram for STMT.  */

static void
set_histogram_value (struct function *fun, gimple stmt, histogram_value hist)
{
  void **loc;
  if (!hist && !VALUE_HISTOGRAMS (fun))
    return;
  if (!VALUE_HISTOGRAMS (fun))
    VALUE_HISTOGRAMS (fun) = htab_create (1, histogram_hash,
				           histogram_eq, NULL);
  loc = htab_find_slot_with_hash (VALUE_HISTOGRAMS (fun), stmt,
                                  htab_hash_pointer (stmt),
				  hist ? INSERT : NO_INSERT);
  if (!hist)
    {
      if (loc)
	htab_clear_slot (VALUE_HISTOGRAMS (fun), loc);
      return;
    }
  *loc = hist;
}

/* Get histogram list for STMT.  */

histogram_value
gimple_histogram_value (struct function *fun, gimple stmt)
{
  if (!VALUE_HISTOGRAMS (fun))
    return NULL;
  return (histogram_value) htab_find_with_hash (VALUE_HISTOGRAMS (fun), stmt,
						htab_hash_pointer (stmt));
}

/* Add histogram for STMT.  */

void
gimple_add_histogram_value (struct function *fun, gimple stmt,
			    histogram_value hist)
{
  hist->hvalue.next = gimple_histogram_value (fun, stmt);
  set_histogram_value (fun, stmt, hist);
}


/* Remove histogram HIST from STMT's histogram list.  */

void
gimple_remove_histogram_value (struct function *fun, gimple stmt,
			       histogram_value hist)
{
  histogram_value hist2 = gimple_histogram_value (fun, stmt);
  if (hist == hist2)
    {
      set_histogram_value (fun, stmt, hist->hvalue.next);
    }
  else
    {
      while (hist2->hvalue.next != hist)
	hist2 = hist2->hvalue.next;
      hist2->hvalue.next = hist->hvalue.next;
    }
  free (hist->hvalue.counters);
#ifdef ENABLE_CHECKING
  memset (hist, 0xab, sizeof (*hist));
#endif
  free (hist);
}


/* Lookup histogram of type TYPE in the STMT.  */

histogram_value
gimple_histogram_value_of_type (struct function *fun, gimple stmt,
				enum hist_type type)
{
  histogram_value hist;
  for (hist = gimple_histogram_value (fun, stmt); hist;
       hist = hist->hvalue.next)
    if (hist->type == type)
      return hist;
  return NULL;
}

/* Dump information about HIST to DUMP_FILE.  */

static void
dump_histogram_value (FILE *dump_file, histogram_value hist)
{
  switch (hist->type)
    {
    case HIST_TYPE_INTERVAL:
      fprintf (dump_file, "Interval counter range %d -- %d",
	       hist->hdata.intvl.int_start,
	       (hist->hdata.intvl.int_start
	        + hist->hdata.intvl.steps - 1));
      if (hist->hvalue.counters)
	{
	   unsigned int i;
	   fprintf(dump_file, " [");
           for (i = 0; i < hist->hdata.intvl.steps; i++)
	     fprintf (dump_file, " %d:"HOST_WIDEST_INT_PRINT_DEC,
		      hist->hdata.intvl.int_start + i,
		      (HOST_WIDEST_INT) hist->hvalue.counters[i]);
	   fprintf (dump_file, " ] outside range:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[i]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_POW2:
      fprintf (dump_file, "Pow2 counter ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "pow2:"HOST_WIDEST_INT_PRINT_DEC
		    " nonpow2:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_SINGLE_VALUE:
      fprintf (dump_file, "Single value ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "value:"HOST_WIDEST_INT_PRINT_DEC
		    " match:"HOST_WIDEST_INT_PRINT_DEC
		    " wrong:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1],
		    (HOST_WIDEST_INT) hist->hvalue.counters[2]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_AVERAGE:
      fprintf (dump_file, "Average value ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "sum:"HOST_WIDEST_INT_PRINT_DEC
		    " times:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_IOR:
      fprintf (dump_file, "IOR value ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "ior:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0]);
	}
      fprintf (dump_file, ".\n");
      break;

    case HIST_TYPE_CONST_DELTA:
      fprintf (dump_file, "Constant delta ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "value:"HOST_WIDEST_INT_PRINT_DEC
		    " match:"HOST_WIDEST_INT_PRINT_DEC
		    " wrong:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1],
		    (HOST_WIDEST_INT) hist->hvalue.counters[2]);
	}
      fprintf (dump_file, ".\n");
      break;
    case HIST_TYPE_INDIR_CALL:
      fprintf (dump_file, "Indirect call ");
      if (hist->hvalue.counters)
	{
	   fprintf (dump_file, "value:"HOST_WIDEST_INT_PRINT_DEC
		    " match:"HOST_WIDEST_INT_PRINT_DEC
		    " all:"HOST_WIDEST_INT_PRINT_DEC,
		    (HOST_WIDEST_INT) hist->hvalue.counters[0],
		    (HOST_WIDEST_INT) hist->hvalue.counters[1],
		    (HOST_WIDEST_INT) hist->hvalue.counters[2]);
	}
      fprintf (dump_file, ".\n");
      break;
   }
}

/* Dump all histograms attached to STMT to DUMP_FILE.  */

void
dump_histograms_for_stmt (struct function *fun, FILE *dump_file, gimple stmt)
{
  histogram_value hist;
  for (hist = gimple_histogram_value (fun, stmt); hist; hist = hist->hvalue.next)
   dump_histogram_value (dump_file, hist);
}

/* Remove all histograms associated with STMT.  */

void
gimple_remove_stmt_histograms (struct function *fun, gimple stmt)
{
  histogram_value val;
  while ((val = gimple_histogram_value (fun, stmt)) != NULL)
    gimple_remove_histogram_value (fun, stmt, val);
}

/* Duplicate all histograms associates with OSTMT to STMT.  */

void
gimple_duplicate_stmt_histograms (struct function *fun, gimple stmt,
				  struct function *ofun, gimple ostmt)
{
  histogram_value val;
  for (val = gimple_histogram_value (ofun, ostmt); val != NULL; val = val->hvalue.next)
    {
      histogram_value new_val = gimple_alloc_histogram_value (fun, val->type, NULL, NULL);
      memcpy (new_val, val, sizeof (*val));
      new_val->hvalue.stmt = stmt;
      new_val->hvalue.counters = XNEWVAR (gcov_type, sizeof (*new_val->hvalue.counters) * new_val->n_counters);
      memcpy (new_val->hvalue.counters, val->hvalue.counters, sizeof (*new_val->hvalue.counters) * new_val->n_counters);
      gimple_add_histogram_value (fun, stmt, new_val);
    }
}


/* Move all histograms associated with OSTMT to STMT.  */

void
gimple_move_stmt_histograms (struct function *fun, gimple stmt, gimple ostmt)
{
  histogram_value val = gimple_histogram_value (fun, ostmt);
  if (val)
    {
      /* The following three statements can't be reordered,
         because histogram hashtab relies on stmt field value
	 for finding the exact slot. */
      set_histogram_value (fun, ostmt, NULL);
      for (; val != NULL; val = val->hvalue.next)
	val->hvalue.stmt = stmt;
      set_histogram_value (fun, stmt, val);
    }
}

static bool error_found = false;

/* Helper function for verify_histograms.  For each histogram reachable via htab
   walk verify that it was reached via statement walk.  */

static int
visit_hist (void **slot, void *data)
{
  struct pointer_set_t *visited = (struct pointer_set_t *) data;
  histogram_value hist = *(histogram_value *) slot;
  if (!pointer_set_contains (visited, hist))
    {
      error ("dead histogram");
      dump_histogram_value (stderr, hist);
      debug_gimple_stmt (hist->hvalue.stmt);
      error_found = true;
    }
  return 1;
}


/* Verify sanity of the histograms.  */

DEBUG_FUNCTION void
verify_histograms (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  histogram_value hist;
  struct pointer_set_t *visited_hists;

  error_found = false;
  visited_hists = pointer_set_create ();
  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);

	for (hist = gimple_histogram_value (cfun, stmt); hist;
	     hist = hist->hvalue.next)
	  {
	    if (hist->hvalue.stmt != stmt)
	      {
		error ("Histogram value statement does not correspond to "
		       "the statement it is associated with");
		debug_gimple_stmt (stmt);
		dump_histogram_value (stderr, hist);
		error_found = true;
	      }
            pointer_set_insert (visited_hists, hist);
	  }
      }
  if (VALUE_HISTOGRAMS (cfun))
    htab_traverse (VALUE_HISTOGRAMS (cfun), visit_hist, visited_hists);
  pointer_set_destroy (visited_hists);
  if (error_found)
    internal_error ("verify_histograms failed");
}

/* Helper function for verify_histograms.  For each histogram reachable via htab
   walk verify that it was reached via statement walk.  */

static int
free_hist (void **slot, void *data ATTRIBUTE_UNUSED)
{
  histogram_value hist = *(histogram_value *) slot;
  free (hist->hvalue.counters);
#ifdef ENABLE_CHECKING
  memset (hist, 0xab, sizeof (*hist));
#endif
  free (hist);
  return 1;
}

void
free_histograms (void)
{
  if (VALUE_HISTOGRAMS (cfun))
    {
      htab_traverse (VALUE_HISTOGRAMS (cfun), free_hist, NULL);
      htab_delete (VALUE_HISTOGRAMS (cfun));
      VALUE_HISTOGRAMS (cfun) = NULL;
    }
}


/* The overall number of invocations of the counter should match
   execution count of basic block.  Report it as error rather than
   internal error as it might mean that user has misused the profile
   somehow.  */

static bool
check_counter (gimple stmt, const char * name,
	       gcov_type *count, gcov_type *all, gcov_type bb_count)
{
  if (*all != bb_count || *count > *all)
    {
      location_t locus;
      locus = (stmt != NULL)
              ? gimple_location (stmt)
              : DECL_SOURCE_LOCATION (current_function_decl);
      if (flag_profile_correction)
        {
	  inform (locus, "correcting inconsistent value profile: "
		  "%s profiler overall count (%d) does not match BB count "
                  "(%d)", name, (int)*all, (int)bb_count);
	  *all = bb_count;
	  if (*count > *all)
            *count = *all;
	  return false;
	}
      else
	{
	  error_at (locus, "corrupted value profile: %s "
		    "profile counter (%d out of %d) inconsistent with "
		    "basic-block count (%d)",
		    name,
		    (int) *count,
		    (int) *all,
		    (int) bb_count);
	  return true;
	}
    }

  return false;
}


/* GIMPLE based transformations. */

bool
gimple_value_profile_transformations (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  bool changed = false;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  histogram_value th = gimple_histogram_value (cfun, stmt);
	  if (!th)
	    continue;

	  if (dump_file)
	    {
	      fprintf (dump_file, "Trying transformations on stmt ");
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	      dump_histograms_for_stmt (cfun, dump_file, stmt);
	    }

	  /* Transformations:  */
	  /* The order of things in this conditional controls which
	     transformation is used when more than one is applicable.  */
	  /* It is expected that any code added by the transformations
	     will be added before the current statement, and that the
	     current statement remain valid (although possibly
	     modified) upon return.  */
	  if (flag_value_profile_transformations
	      && (gimple_mod_subtract_transform (&gsi)
		  || gimple_divmod_fixed_value_transform (&gsi)
		  || gimple_mod_pow2_value_transform (&gsi)
		  || gimple_stringops_transform (&gsi)
		  || gimple_ic_transform (stmt)))
	    {
	      stmt = gsi_stmt (gsi);
	      changed = true;
	      /* Original statement may no longer be in the same block. */
	      if (bb != gimple_bb (stmt))
		{
	          bb = gimple_bb (stmt);
		  gsi = gsi_for_stmt (stmt);
		}
	    }
        }
    }

  if (changed)
    {
      counts_to_freqs ();
    }

  return changed;
}


/* Generate code for transformation 1 (with parent gimple assignment
   STMT and probability of taking the optimal path PROB, which is
   equivalent to COUNT/ALL within roundoff error).  This generates the
   result into a temp and returns the temp; it does not replace or
   alter the original STMT.  */

static tree
gimple_divmod_fixed_value (gimple stmt, tree value, int prob, gcov_type count,
			   gcov_type all)
{
  gimple stmt1, stmt2, stmt3;
  tree tmp0, tmp1, tmp2, tmpv;
  gimple bb1end, bb2end, bb3end;
  basic_block bb, bb2, bb3, bb4;
  tree optype, op1, op2;
  edge e12, e13, e23, e24, e34;
  gimple_stmt_iterator gsi;

  gcc_assert (is_gimple_assign (stmt)
	      && (gimple_assign_rhs_code (stmt) == TRUNC_DIV_EXPR
		  || gimple_assign_rhs_code (stmt) == TRUNC_MOD_EXPR));

  optype = TREE_TYPE (gimple_assign_lhs (stmt));
  op1 = gimple_assign_rhs1 (stmt);
  op2 = gimple_assign_rhs2 (stmt);

  bb = gimple_bb (stmt);
  gsi = gsi_for_stmt (stmt);

  tmpv = create_tmp_reg (optype, "PROF");
  tmp0 = make_ssa_name (tmpv, NULL);
  tmp1 = make_ssa_name (tmpv, NULL);
  stmt1 = gimple_build_assign (tmp0, fold_convert (optype, value));
  SSA_NAME_DEF_STMT (tmp0) = stmt1;
  stmt2 = gimple_build_assign (tmp1, op2);
  SSA_NAME_DEF_STMT (tmp1) = stmt2;
  stmt3 = gimple_build_cond (NE_EXPR, tmp1, tmp0, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
  bb1end = stmt3;

  tmp2 = make_rename_temp (optype, "PROF");
  stmt1 = gimple_build_assign_with_ops (gimple_assign_rhs_code (stmt), tmp2,
					op1, tmp0);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb2end = stmt1;

  stmt1 = gimple_build_assign_with_ops (gimple_assign_rhs_code (stmt), tmp2,
					op1, op2);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb3end = stmt1;

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
gimple_divmod_fixed_value_transform (gimple_stmt_iterator *si)
{
  histogram_value histogram;
  enum tree_code code;
  gcov_type val, count, all;
  tree result, value, tree_val;
  gcov_type prob;
  gimple stmt;

  stmt = gsi_stmt (*si);
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_lhs (stmt))))
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (code != TRUNC_DIV_EXPR && code != TRUNC_MOD_EXPR)
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt,
					      HIST_TYPE_SINGLE_VALUE);
  if (!histogram)
    return false;

  value = histogram->hvalue.value;
  val = histogram->hvalue.counters[0];
  count = histogram->hvalue.counters[1];
  all = histogram->hvalue.counters[2];
  gimple_remove_histogram_value (cfun, stmt, histogram);

  /* We require that count is at least half of all; this means
     that for the transformation to fire the value must be constant
     at least 50% of time (and 75% gives the guarantee of usage).  */
  if (simple_cst_equal (gimple_assign_rhs2 (stmt), value) != 1
      || 2 * count < all
      || optimize_bb_for_size_p (gimple_bb (stmt)))
    return false;

  if (check_counter (stmt, "value", &count, &all, gimple_bb (stmt)->count))
    return false;

  /* Compute probability of taking the optimal path.  */
  if (all > 0)
    prob = (count * REG_BR_PROB_BASE + all / 2) / all;
  else
    prob = 0;

  tree_val = build_int_cst_wide (get_gcov_type (),
				 (unsigned HOST_WIDE_INT) val,
				 val >> (HOST_BITS_PER_WIDE_INT - 1) >> 1);
  result = gimple_divmod_fixed_value (stmt, tree_val, prob, count, all);

  if (dump_file)
    {
      fprintf (dump_file, "Div/mod by constant ");
      print_generic_expr (dump_file, value, TDF_SLIM);
      fprintf (dump_file, "=");
      print_generic_expr (dump_file, tree_val, TDF_SLIM);
      fprintf (dump_file, " transformation on insn ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  gimple_assign_set_rhs_from_tree (si, result);
  update_stmt (gsi_stmt (*si));

  return true;
}

/* Generate code for transformation 2 (with parent gimple assign STMT and
   probability of taking the optimal path PROB, which is equivalent to COUNT/ALL
   within roundoff error).  This generates the result into a temp and returns
   the temp; it does not replace or alter the original STMT.  */
static tree
gimple_mod_pow2 (gimple stmt, int prob, gcov_type count, gcov_type all)
{
  gimple stmt1, stmt2, stmt3, stmt4;
  tree tmp2, tmp3, tmpv;
  gimple bb1end, bb2end, bb3end;
  basic_block bb, bb2, bb3, bb4;
  tree optype, op1, op2;
  edge e12, e13, e23, e24, e34;
  gimple_stmt_iterator gsi;
  tree result;

  gcc_assert (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == TRUNC_MOD_EXPR);

  optype = TREE_TYPE (gimple_assign_lhs (stmt));
  op1 = gimple_assign_rhs1 (stmt);
  op2 = gimple_assign_rhs2 (stmt);

  bb = gimple_bb (stmt);
  gsi = gsi_for_stmt (stmt);

  result = make_rename_temp (optype, "PROF");
  tmpv = create_tmp_var (optype, "PROF");
  tmp2 = make_ssa_name (tmpv, NULL);
  tmp3 = make_ssa_name (tmpv, NULL);
  stmt2 = gimple_build_assign_with_ops (PLUS_EXPR, tmp2, op2,
					build_int_cst (optype, -1));
  SSA_NAME_DEF_STMT (tmp2) = stmt2;
  stmt3 = gimple_build_assign_with_ops (BIT_AND_EXPR, tmp3, tmp2, op2);
  SSA_NAME_DEF_STMT (tmp3) = stmt3;
  stmt4 = gimple_build_cond (NE_EXPR, tmp3, build_int_cst (optype, 0),
			     NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt4, GSI_SAME_STMT);
  bb1end = stmt4;

  /* tmp2 == op2-1 inherited from previous block.  */
  stmt1 = gimple_build_assign_with_ops (BIT_AND_EXPR, result, op1, tmp2);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb2end = stmt1;

  stmt1 = gimple_build_assign_with_ops (gimple_assign_rhs_code (stmt), result,
					op1, op2);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb3end = stmt1;

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
gimple_mod_pow2_value_transform (gimple_stmt_iterator *si)
{
  histogram_value histogram;
  enum tree_code code;
  gcov_type count, wrong_values, all;
  tree lhs_type, result, value;
  gcov_type prob;
  gimple stmt;

  stmt = gsi_stmt (*si);
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs_type = TREE_TYPE (gimple_assign_lhs (stmt));
  if (!INTEGRAL_TYPE_P (lhs_type))
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (code != TRUNC_MOD_EXPR || !TYPE_UNSIGNED (lhs_type))
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_POW2);
  if (!histogram)
    return false;

  value = histogram->hvalue.value;
  wrong_values = histogram->hvalue.counters[0];
  count = histogram->hvalue.counters[1];

  gimple_remove_histogram_value (cfun, stmt, histogram);

  /* We require that we hit a power of 2 at least half of all evaluations.  */
  if (simple_cst_equal (gimple_assign_rhs2 (stmt), value) != 1
      || count < wrong_values
      || optimize_bb_for_size_p (gimple_bb (stmt)))
    return false;

  if (dump_file)
    {
      fprintf (dump_file, "Mod power of 2 transformation on insn ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  /* Compute probability of taking the optimal path.  */
  all = count + wrong_values;

  if (check_counter (stmt, "pow2", &count, &all, gimple_bb (stmt)->count))
    return false;

  if (all > 0)
    prob = (count * REG_BR_PROB_BASE + all / 2) / all;
  else
    prob = 0;

  result = gimple_mod_pow2 (stmt, prob, count, all);

  gimple_assign_set_rhs_from_tree (si, result);
  update_stmt (gsi_stmt (*si));

  return true;
}

/* Generate code for transformations 3 and 4 (with parent gimple assign STMT, and
   NCOUNTS the number of cases to support.  Currently only NCOUNTS==0 or 1 is
   supported and this is built into this interface.  The probabilities of taking
   the optimal paths are PROB1 and PROB2, which are equivalent to COUNT1/ALL and
   COUNT2/ALL respectively within roundoff error).  This generates the
   result into a temp and returns the temp; it does not replace or alter
   the original STMT.  */
/* FIXME: Generalize the interface to handle NCOUNTS > 1.  */

static tree
gimple_mod_subtract (gimple stmt, int prob1, int prob2, int ncounts,
		     gcov_type count1, gcov_type count2, gcov_type all)
{
  gimple stmt1, stmt2, stmt3;
  tree tmp1;
  gimple bb1end, bb2end = NULL, bb3end;
  basic_block bb, bb2, bb3, bb4;
  tree optype, op1, op2;
  edge e12, e23 = 0, e24, e34, e14;
  gimple_stmt_iterator gsi;
  tree result;

  gcc_assert (is_gimple_assign (stmt)
	      && gimple_assign_rhs_code (stmt) == TRUNC_MOD_EXPR);

  optype = TREE_TYPE (gimple_assign_lhs (stmt));
  op1 = gimple_assign_rhs1 (stmt);
  op2 = gimple_assign_rhs2 (stmt);

  bb = gimple_bb (stmt);
  gsi = gsi_for_stmt (stmt);

  result = make_rename_temp (optype, "PROF");
  tmp1 = make_ssa_name (create_tmp_var (optype, "PROF"), NULL);
  stmt1 = gimple_build_assign (result, op1);
  stmt2 = gimple_build_assign (tmp1, op2);
  SSA_NAME_DEF_STMT (tmp1) = stmt2;
  stmt3 = gimple_build_cond (LT_EXPR, result, tmp1, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
  bb1end = stmt3;

  if (ncounts)	/* Assumed to be 0 or 1 */
    {
      stmt1 = gimple_build_assign_with_ops (MINUS_EXPR, result, result, tmp1);
      stmt2 = gimple_build_cond (LT_EXPR, result, tmp1, NULL_TREE, NULL_TREE);
      gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
      gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
      bb2end = stmt2;
    }

  /* Fallback case. */
  stmt1 = gimple_build_assign_with_ops (gimple_assign_rhs_code (stmt), result,
					result, tmp1);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb3end = stmt1;

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
  e12->count = all - count1;

  e14 = make_edge (bb, bb4, EDGE_TRUE_VALUE);
  e14->probability = prob1;
  e14->count = count1;

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


/* Do transforms 3) and 4) on the statement pointed-to by SI if applicable.  */

static bool
gimple_mod_subtract_transform (gimple_stmt_iterator *si)
{
  histogram_value histogram;
  enum tree_code code;
  gcov_type count, wrong_values, all;
  tree lhs_type, result;
  gcov_type prob1, prob2;
  unsigned int i, steps;
  gcov_type count1, count2;
  gimple stmt;

  stmt = gsi_stmt (*si);
  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return false;

  lhs_type = TREE_TYPE (gimple_assign_lhs (stmt));
  if (!INTEGRAL_TYPE_P (lhs_type))
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (code != TRUNC_MOD_EXPR || !TYPE_UNSIGNED (lhs_type))
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_INTERVAL);
  if (!histogram)
    return false;

  all = 0;
  wrong_values = 0;
  for (i = 0; i < histogram->hdata.intvl.steps; i++)
    all += histogram->hvalue.counters[i];

  wrong_values += histogram->hvalue.counters[i];
  wrong_values += histogram->hvalue.counters[i+1];
  steps = histogram->hdata.intvl.steps;
  all += wrong_values;
  count1 = histogram->hvalue.counters[0];
  count2 = histogram->hvalue.counters[1];

  /* Compute probability of taking the optimal path.  */
  if (check_counter (stmt, "interval", &count1, &all, gimple_bb (stmt)->count))
    {
      gimple_remove_histogram_value (cfun, stmt, histogram);
      return false;
    }

  if (flag_profile_correction && count1 + count2 > all)
      all = count1 + count2;

  gcc_assert (count1 + count2 <= all);

  /* We require that we use just subtractions in at least 50% of all
     evaluations.  */
  count = 0;
  for (i = 0; i < histogram->hdata.intvl.steps; i++)
    {
      count += histogram->hvalue.counters[i];
      if (count * 2 >= all)
	break;
    }
  if (i == steps
      || optimize_bb_for_size_p (gimple_bb (stmt)))
    return false;

  gimple_remove_histogram_value (cfun, stmt, histogram);
  if (dump_file)
    {
      fprintf (dump_file, "Mod subtract transformation on insn ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  /* Compute probability of taking the optimal path(s).  */
  if (all > 0)
    {
      prob1 = (count1 * REG_BR_PROB_BASE + all / 2) / all;
      prob2 = (count2 * REG_BR_PROB_BASE + all / 2) / all;
    }
  else
    {
      prob1 = prob2 = 0;
    }

  /* In practice, "steps" is always 2.  This interface reflects this,
     and will need to be changed if "steps" can change.  */
  result = gimple_mod_subtract (stmt, prob1, prob2, i, count1, count2, all);

  gimple_assign_set_rhs_from_tree (si, result);
  update_stmt (gsi_stmt (*si));

  return true;
}

static VEC(cgraph_node_ptr, heap) *cgraph_node_map = NULL;

/* Initialize map from FUNCDEF_NO to CGRAPH_NODE.  */

void
init_node_map (void)
{
  struct cgraph_node *n;

  if (get_last_funcdef_no ())
    VEC_safe_grow_cleared (cgraph_node_ptr, heap,
                           cgraph_node_map, get_last_funcdef_no ());

  for (n = cgraph_nodes; n; n = n->next)
    {
      if (DECL_STRUCT_FUNCTION (n->decl))
        VEC_replace (cgraph_node_ptr, cgraph_node_map,
                     DECL_STRUCT_FUNCTION (n->decl)->funcdef_no, n);
    }
}

/* Delete the CGRAPH_NODE_MAP.  */

void
del_node_map (void)
{
   VEC_free (cgraph_node_ptr, heap, cgraph_node_map);
   cgraph_node_map = NULL;
}

/* Return cgraph node for function with pid */

static inline struct cgraph_node*
find_func_by_funcdef_no (int func_id)
{
  int max_id = get_last_funcdef_no ();
  if (func_id >= max_id || VEC_index (cgraph_node_ptr,
                                      cgraph_node_map,
                                      func_id) == NULL)
    {
      if (flag_profile_correction)
        inform (DECL_SOURCE_LOCATION (current_function_decl),
                "Inconsistent profile: indirect call target (%d) does not exist", func_id);
      else
        error ("Inconsistent profile: indirect call target (%d) does not exist", func_id);

      return NULL;
    }

  return VEC_index (cgraph_node_ptr, cgraph_node_map, func_id);
}

/* Perform sanity check on the indirect call target. Due to race conditions,
   false function target may be attributed to an indirect call site. If the
   call expression type mismatches with the target function's type, expand_call
   may ICE. Here we only do very minimal sanity check just to make compiler happy.
   Returns true if TARGET is considered ok for call CALL_STMT.  */

static bool
check_ic_target (gimple call_stmt, struct cgraph_node *target)
{
   location_t locus;
   if (gimple_check_call_matching_types (call_stmt, target->decl))
     return true;

   locus =  gimple_location (call_stmt);
   inform (locus, "Skipping target %s with mismatching types for icall ",
           cgraph_node_name (target));
   return false;
}

/* Do transformation

  if (actual_callee_address == address_of_most_common_function/method)
    do direct call
  else
    old call
 */

static gimple
gimple_ic (gimple icall_stmt, struct cgraph_node *direct_call,
	   int prob, gcov_type count, gcov_type all)
{
  gimple dcall_stmt, load_stmt, cond_stmt;
  tree tmp0, tmp1, tmpv, tmp;
  basic_block cond_bb, dcall_bb, icall_bb, join_bb = NULL;
  tree optype = build_pointer_type (void_type_node);
  edge e_cd, e_ci, e_di, e_dj = NULL, e_ij;
  gimple_stmt_iterator gsi;
  int lp_nr;

  cond_bb = gimple_bb (icall_stmt);
  gsi = gsi_for_stmt (icall_stmt);

  tmpv = create_tmp_reg (optype, "PROF");
  tmp0 = make_ssa_name (tmpv, NULL);
  tmp1 = make_ssa_name (tmpv, NULL);
  tmp = unshare_expr (gimple_call_fn (icall_stmt));
  load_stmt = gimple_build_assign (tmp0, tmp);
  SSA_NAME_DEF_STMT (tmp0) = load_stmt;
  gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);

  tmp = fold_convert (optype, build_addr (direct_call->decl,
					  current_function_decl));
  load_stmt = gimple_build_assign (tmp1, tmp);
  SSA_NAME_DEF_STMT (tmp1) = load_stmt;
  gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);

  cond_stmt = gimple_build_cond (EQ_EXPR, tmp1, tmp0, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);

  gimple_set_vdef (icall_stmt, NULL_TREE);
  gimple_set_vuse (icall_stmt, NULL_TREE);
  update_stmt (icall_stmt);
  dcall_stmt = gimple_copy (icall_stmt);
  gimple_call_set_fndecl (dcall_stmt, direct_call->decl);
  gsi_insert_before (&gsi, dcall_stmt, GSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e_cd connects cond_bb to dcall_bb, etc; note the first letters. */
  e_cd = split_block (cond_bb, cond_stmt);
  dcall_bb = e_cd->dest;
  dcall_bb->count = count;

  e_di = split_block (dcall_bb, dcall_stmt);
  icall_bb = e_di->dest;
  icall_bb->count = all - count;

  /* Do not disturb existing EH edges from the indirect call.  */
  if (!stmt_ends_bb_p (icall_stmt))
    e_ij = split_block (icall_bb, icall_stmt);
  else
    {
      e_ij = find_fallthru_edge (icall_bb->succs);
      /* The indirect call might be noreturn.  */
      if (e_ij != NULL)
	{
	  e_ij->probability = REG_BR_PROB_BASE;
	  e_ij->count = all - count;
	  e_ij = single_pred_edge (split_edge (e_ij));
	}
    }
  if (e_ij != NULL)
    {
      join_bb = e_ij->dest;
      join_bb->count = all;
    }

  e_cd->flags = (e_cd->flags & ~EDGE_FALLTHRU) | EDGE_TRUE_VALUE;
  e_cd->probability = prob;
  e_cd->count = count;

  e_ci = make_edge (cond_bb, icall_bb, EDGE_FALSE_VALUE);
  e_ci->probability = REG_BR_PROB_BASE - prob;
  e_ci->count = all - count;

  remove_edge (e_di);

  if (e_ij != NULL)
    {
      e_dj = make_edge (dcall_bb, join_bb, EDGE_FALLTHRU);
      e_dj->probability = REG_BR_PROB_BASE;
      e_dj->count = count;

      e_ij->probability = REG_BR_PROB_BASE;
      e_ij->count = all - count;
    }

  /* Insert PHI node for the call result if necessary.  */
  if (gimple_call_lhs (icall_stmt)
      && TREE_CODE (gimple_call_lhs (icall_stmt)) == SSA_NAME)
    {
      tree result = gimple_call_lhs (icall_stmt);
      gimple phi = create_phi_node (result, join_bb);
      SSA_NAME_DEF_STMT (result) = phi;
      gimple_call_set_lhs (icall_stmt,
			   make_ssa_name (SSA_NAME_VAR (result), icall_stmt));
      add_phi_arg (phi, gimple_call_lhs (icall_stmt), e_ij, UNKNOWN_LOCATION);
      gimple_call_set_lhs (dcall_stmt,
			   make_ssa_name (SSA_NAME_VAR (result), dcall_stmt));
      add_phi_arg (phi, gimple_call_lhs (dcall_stmt), e_dj, UNKNOWN_LOCATION);
    }

  /* Build an EH edge for the direct call if necessary.  */
  lp_nr = lookup_stmt_eh_lp (icall_stmt);
  if (lp_nr != 0
      && stmt_could_throw_p (dcall_stmt))
    {
      edge e_eh, e;
      edge_iterator ei;
      gimple_stmt_iterator psi;

      add_stmt_to_eh_lp (dcall_stmt, lp_nr);
      FOR_EACH_EDGE (e_eh, ei, icall_bb->succs)
	if (e_eh->flags & EDGE_EH)
	  break;
      e = make_edge (dcall_bb, e_eh->dest, EDGE_EH);
      for (psi = gsi_start_phis (e_eh->dest);
	   !gsi_end_p (psi); gsi_next (&psi))
	{
	  gimple phi = gsi_stmt (psi);
	  SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e),
		   PHI_ARG_DEF_FROM_EDGE (phi, e_eh));
	}
    }

  return dcall_stmt;
}

/*
  For every checked indirect/virtual call determine if most common pid of
  function/class method has probability more than 50%. If yes modify code of
  this call to:
 */

static bool
gimple_ic_transform (gimple stmt)
{
  histogram_value histogram;
  gcov_type val, count, all, bb_all;
  gcov_type prob;
  gimple modify;
  struct cgraph_node *direct_call;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;

  if (gimple_call_fndecl (stmt) != NULL_TREE)
    return false;

  if (gimple_call_internal_p (stmt))
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_INDIR_CALL);
  if (!histogram)
    return false;

  val = histogram->hvalue.counters [0];
  count = histogram->hvalue.counters [1];
  all = histogram->hvalue.counters [2];
  gimple_remove_histogram_value (cfun, stmt, histogram);

  if (4 * count <= 3 * all)
    return false;

  bb_all = gimple_bb (stmt)->count;
  /* The order of CHECK_COUNTER calls is important -
     since check_counter can correct the third parameter
     and we want to make count <= all <= bb_all. */
  if ( check_counter (stmt, "ic", &all, &bb_all, bb_all)
      || check_counter (stmt, "ic", &count, &all, all))
    return false;

  if (all > 0)
    prob = (count * REG_BR_PROB_BASE + all / 2) / all;
  else
    prob = 0;
  direct_call = find_func_by_funcdef_no ((int)val);

  if (direct_call == NULL)
    return false;

  if (!check_ic_target (stmt, direct_call))
    return false;

  modify = gimple_ic (stmt, direct_call, prob, count, all);

  if (dump_file)
    {
      fprintf (dump_file, "Indirect call -> direct call ");
      print_generic_expr (dump_file, gimple_call_fn (stmt), TDF_SLIM);
      fprintf (dump_file, "=> ");
      print_generic_expr (dump_file, direct_call->decl, TDF_SLIM);
      fprintf (dump_file, " transformation on insn ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, " to ");
      print_gimple_stmt (dump_file, modify, 0, TDF_SLIM);
      fprintf (dump_file, "hist->count "HOST_WIDEST_INT_PRINT_DEC
	       " hist->all "HOST_WIDEST_INT_PRINT_DEC"\n", count, all);
    }

  return true;
}

/* Return true if the stringop CALL with FNDECL shall be profiled.
   SIZE_ARG be set to the argument index for the size of the string
   operation.
*/
static bool
interesting_stringop_to_profile_p (tree fndecl, gimple call, int *size_arg)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  if (fcode != BUILT_IN_MEMCPY && fcode != BUILT_IN_MEMPCPY
      && fcode != BUILT_IN_MEMSET && fcode != BUILT_IN_BZERO)
    return false;

  switch (fcode)
    {
     case BUILT_IN_MEMCPY:
     case BUILT_IN_MEMPCPY:
       *size_arg = 2;
       return validate_gimple_arglist (call, POINTER_TYPE, POINTER_TYPE,
				       INTEGER_TYPE, VOID_TYPE);
     case BUILT_IN_MEMSET:
       *size_arg = 2;
       return validate_gimple_arglist (call, POINTER_TYPE, INTEGER_TYPE,
				      INTEGER_TYPE, VOID_TYPE);
     case BUILT_IN_BZERO:
       *size_arg = 1;
       return validate_gimple_arglist (call, POINTER_TYPE, INTEGER_TYPE,
				       VOID_TYPE);
     default:
       gcc_unreachable ();
    }
}

/* Convert   stringop (..., vcall_size)
   into
   if (vcall_size == icall_size)
     stringop (..., icall_size);
   else
     stringop (..., vcall_size);
   assuming we'll propagate a true constant into ICALL_SIZE later.  */

static void
gimple_stringop_fixed_value (gimple vcall_stmt, tree icall_size, int prob,
			     gcov_type count, gcov_type all)
{
  gimple tmp_stmt, cond_stmt, icall_stmt;
  tree tmp0, tmp1, tmpv, vcall_size, optype;
  basic_block cond_bb, icall_bb, vcall_bb, join_bb;
  edge e_ci, e_cv, e_iv, e_ij, e_vj;
  gimple_stmt_iterator gsi;
  tree fndecl;
  int size_arg;

  fndecl = gimple_call_fndecl (vcall_stmt);
  if (!interesting_stringop_to_profile_p (fndecl, vcall_stmt, &size_arg))
    gcc_unreachable();

  cond_bb = gimple_bb (vcall_stmt);
  gsi = gsi_for_stmt (vcall_stmt);

  vcall_size = gimple_call_arg (vcall_stmt, size_arg);
  optype = TREE_TYPE (vcall_size);

  tmpv = create_tmp_var (optype, "PROF");
  tmp0 = make_ssa_name (tmpv, NULL);
  tmp1 = make_ssa_name (tmpv, NULL);
  tmp_stmt = gimple_build_assign (tmp0, fold_convert (optype, icall_size));
  SSA_NAME_DEF_STMT (tmp0) = tmp_stmt;
  gsi_insert_before (&gsi, tmp_stmt, GSI_SAME_STMT);

  tmp_stmt = gimple_build_assign (tmp1, vcall_size);
  SSA_NAME_DEF_STMT (tmp1) = tmp_stmt;
  gsi_insert_before (&gsi, tmp_stmt, GSI_SAME_STMT);

  cond_stmt = gimple_build_cond (EQ_EXPR, tmp1, tmp0, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);

  gimple_set_vdef (vcall_stmt, NULL);
  gimple_set_vuse (vcall_stmt, NULL);
  update_stmt (vcall_stmt);
  icall_stmt = gimple_copy (vcall_stmt);
  gimple_call_set_arg (icall_stmt, size_arg, icall_size);
  gsi_insert_before (&gsi, icall_stmt, GSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e_ci connects cond_bb to icall_bb, etc. */
  e_ci = split_block (cond_bb, cond_stmt);
  icall_bb = e_ci->dest;
  icall_bb->count = count;

  e_iv = split_block (icall_bb, icall_stmt);
  vcall_bb = e_iv->dest;
  vcall_bb->count = all - count;

  e_vj = split_block (vcall_bb, vcall_stmt);
  join_bb = e_vj->dest;
  join_bb->count = all;

  e_ci->flags = (e_ci->flags & ~EDGE_FALLTHRU) | EDGE_TRUE_VALUE;
  e_ci->probability = prob;
  e_ci->count = count;

  e_cv = make_edge (cond_bb, vcall_bb, EDGE_FALSE_VALUE);
  e_cv->probability = REG_BR_PROB_BASE - prob;
  e_cv->count = all - count;

  remove_edge (e_iv);

  e_ij = make_edge (icall_bb, join_bb, EDGE_FALLTHRU);
  e_ij->probability = REG_BR_PROB_BASE;
  e_ij->count = count;

  e_vj->probability = REG_BR_PROB_BASE;
  e_vj->count = all - count;

  /* Insert PHI node for the call result if necessary.  */
  if (gimple_call_lhs (vcall_stmt)
      && TREE_CODE (gimple_call_lhs (vcall_stmt)) == SSA_NAME)
    {
      tree result = gimple_call_lhs (vcall_stmt);
      gimple phi = create_phi_node (result, join_bb);
      SSA_NAME_DEF_STMT (result) = phi;
      gimple_call_set_lhs (vcall_stmt,
			   make_ssa_name (SSA_NAME_VAR (result), vcall_stmt));
      add_phi_arg (phi, gimple_call_lhs (vcall_stmt), e_vj, UNKNOWN_LOCATION);
      gimple_call_set_lhs (icall_stmt,
			   make_ssa_name (SSA_NAME_VAR (result), icall_stmt));
      add_phi_arg (phi, gimple_call_lhs (icall_stmt), e_ij, UNKNOWN_LOCATION);
    }

  /* Because these are all string op builtins, they're all nothrow.  */
  gcc_assert (!stmt_could_throw_p (vcall_stmt));
  gcc_assert (!stmt_could_throw_p (icall_stmt));
}

/* Find values inside STMT for that we want to measure histograms for
   division/modulo optimization.  */
static bool
gimple_stringops_transform (gimple_stmt_iterator *gsi)
{
  gimple stmt = gsi_stmt (*gsi);
  tree fndecl;
  tree blck_size;
  enum built_in_function fcode;
  histogram_value histogram;
  gcov_type count, all, val;
  tree dest, src;
  unsigned int dest_align, src_align;
  gcov_type prob;
  tree tree_val;
  int size_arg;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;
  fndecl = gimple_call_fndecl (stmt);
  if (!fndecl)
    return false;
  fcode = DECL_FUNCTION_CODE (fndecl);
  if (!interesting_stringop_to_profile_p (fndecl, stmt, &size_arg))
    return false;

  blck_size = gimple_call_arg (stmt, size_arg);
  if (TREE_CODE (blck_size) == INTEGER_CST)
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_SINGLE_VALUE);
  if (!histogram)
    return false;
  val = histogram->hvalue.counters[0];
  count = histogram->hvalue.counters[1];
  all = histogram->hvalue.counters[2];
  gimple_remove_histogram_value (cfun, stmt, histogram);
  /* We require that count is at least half of all; this means
     that for the transformation to fire the value must be constant
     at least 80% of time.  */
  if ((6 * count / 5) < all || optimize_bb_for_size_p (gimple_bb (stmt)))
    return false;
  if (check_counter (stmt, "value", &count, &all, gimple_bb (stmt)->count))
    return false;
  if (all > 0)
    prob = (count * REG_BR_PROB_BASE + all / 2) / all;
  else
    prob = 0;
  dest = gimple_call_arg (stmt, 0);
  dest_align = get_pointer_alignment (dest);
  switch (fcode)
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMPCPY:
      src = gimple_call_arg (stmt, 1);
      src_align = get_pointer_alignment (src);
      if (!can_move_by_pieces (val, MIN (dest_align, src_align)))
	return false;
      break;
    case BUILT_IN_MEMSET:
      if (!can_store_by_pieces (val, builtin_memset_read_str,
				gimple_call_arg (stmt, 1),
				dest_align, true))
	return false;
      break;
    case BUILT_IN_BZERO:
      if (!can_store_by_pieces (val, builtin_memset_read_str,
				integer_zero_node,
				dest_align, true))
	return false;
      break;
    default:
      gcc_unreachable ();
    }
  tree_val = build_int_cst_wide (get_gcov_type (),
				 (unsigned HOST_WIDE_INT) val,
				 val >> (HOST_BITS_PER_WIDE_INT - 1) >> 1);
  if (dump_file)
    {
      fprintf (dump_file, "Single value %i stringop transformation on ",
	       (int)val);
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
  gimple_stringop_fixed_value (stmt, tree_val, prob, count, all);

  return true;
}

void
stringop_block_profile (gimple stmt, unsigned int *expected_align,
			HOST_WIDE_INT *expected_size)
{
  histogram_value histogram;
  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_AVERAGE);
  if (!histogram)
    *expected_size = -1;
  else if (!histogram->hvalue.counters[1])
    {
      *expected_size = -1;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
  else
    {
      gcov_type size;
      size = ((histogram->hvalue.counters[0]
	      + histogram->hvalue.counters[1] / 2)
	       / histogram->hvalue.counters[1]);
      /* Even if we can hold bigger value in SIZE, INT_MAX
	 is safe "infinity" for code generation strategies.  */
      if (size > INT_MAX)
	size = INT_MAX;
      *expected_size = size;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_IOR);
  if (!histogram)
    *expected_align = 0;
  else if (!histogram->hvalue.counters[0])
    {
      gimple_remove_histogram_value (cfun, stmt, histogram);
      *expected_align = 0;
    }
  else
    {
      gcov_type count;
      int alignment;

      count = histogram->hvalue.counters[0];
      alignment = 1;
      while (!(count & alignment)
	     && (alignment * 2 * BITS_PER_UNIT))
	alignment <<= 1;
      *expected_align = alignment * BITS_PER_UNIT;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
}


/* Find values inside STMT for that we want to measure histograms for
   division/modulo optimization.  */
static void
gimple_divmod_values_to_profile (gimple stmt, histogram_values *values)
{
  tree lhs, divisor, op0, type;
  histogram_value hist;

  if (gimple_code (stmt) != GIMPLE_ASSIGN)
    return;

  lhs = gimple_assign_lhs (stmt);
  type = TREE_TYPE (lhs);
  if (!INTEGRAL_TYPE_P (type))
    return;

  switch (gimple_assign_rhs_code (stmt))
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
      divisor = gimple_assign_rhs2 (stmt);
      op0 = gimple_assign_rhs1 (stmt);

      VEC_reserve (histogram_value, heap, *values, 3);

      if (is_gimple_reg (divisor))
	/* Check for the case where the divisor is the same value most
	   of the time.  */
	VEC_quick_push (histogram_value, *values,
			gimple_alloc_histogram_value (cfun,
						      HIST_TYPE_SINGLE_VALUE,
						      stmt, divisor));

      /* For mod, check whether it is not often a noop (or replaceable by
	 a few subtractions).  */
      if (gimple_assign_rhs_code (stmt) == TRUNC_MOD_EXPR
	  && TYPE_UNSIGNED (type))
	{
          tree val;
          /* Check for a special case where the divisor is power of 2.  */
	  VEC_quick_push (histogram_value, *values,
			  gimple_alloc_histogram_value (cfun, HIST_TYPE_POW2,
							stmt, divisor));

	  val = build2 (TRUNC_DIV_EXPR, type, op0, divisor);
	  hist = gimple_alloc_histogram_value (cfun, HIST_TYPE_INTERVAL,
					       stmt, val);
	  hist->hdata.intvl.int_start = 0;
	  hist->hdata.intvl.steps = 2;
	  VEC_quick_push (histogram_value, *values, hist);
	}
      return;

    default:
      return;
    }
}

/* Find calls inside STMT for that we want to measure histograms for
   indirect/virtual call optimization. */

static void
gimple_indirect_call_to_profile (gimple stmt, histogram_values *values)
{
  tree callee;

  if (gimple_code (stmt) != GIMPLE_CALL
      || gimple_call_internal_p (stmt)
      || gimple_call_fndecl (stmt) != NULL_TREE)
    return;

  callee = gimple_call_fn (stmt);

  VEC_reserve (histogram_value, heap, *values, 3);

  VEC_quick_push (histogram_value, *values,
		  gimple_alloc_histogram_value (cfun, HIST_TYPE_INDIR_CALL,
						stmt, callee));

  return;
}

/* Find values inside STMT for that we want to measure histograms for
   string operations.  */
static void
gimple_stringops_values_to_profile (gimple stmt, histogram_values *values)
{
  tree fndecl;
  tree blck_size;
  tree dest;
  int size_arg;

  if (gimple_code (stmt) != GIMPLE_CALL)
    return;
  fndecl = gimple_call_fndecl (stmt);
  if (!fndecl)
    return;

  if (!interesting_stringop_to_profile_p (fndecl, stmt, &size_arg))
    return;

  dest = gimple_call_arg (stmt, 0);
  blck_size = gimple_call_arg (stmt, size_arg);

  if (TREE_CODE (blck_size) != INTEGER_CST)
    {
      VEC_safe_push (histogram_value, heap, *values,
		     gimple_alloc_histogram_value (cfun, HIST_TYPE_SINGLE_VALUE,
						   stmt, blck_size));
      VEC_safe_push (histogram_value, heap, *values,
		     gimple_alloc_histogram_value (cfun, HIST_TYPE_AVERAGE,
						   stmt, blck_size));
    }
  if (TREE_CODE (blck_size) != INTEGER_CST)
    VEC_safe_push (histogram_value, heap, *values,
		   gimple_alloc_histogram_value (cfun, HIST_TYPE_IOR,
						 stmt, dest));
}

/* Find values inside STMT for that we want to measure histograms and adds
   them to list VALUES.  */

static void
gimple_values_to_profile (gimple stmt, histogram_values *values)
{
  if (flag_value_profile_transformations)
    {
      gimple_divmod_values_to_profile (stmt, values);
      gimple_stringops_values_to_profile (stmt, values);
      gimple_indirect_call_to_profile (stmt, values);
    }
}

void
gimple_find_values_to_profile (histogram_values *values)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  unsigned i;
  histogram_value hist = NULL;

  *values = NULL;
  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      gimple_values_to_profile (gsi_stmt (gsi), values);

  FOR_EACH_VEC_ELT (histogram_value, *values, i, hist)
    {
      switch (hist->type)
        {
	case HIST_TYPE_INTERVAL:
	  hist->n_counters = hist->hdata.intvl.steps + 2;
	  break;

	case HIST_TYPE_POW2:
	  hist->n_counters = 2;
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  hist->n_counters = 3;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  hist->n_counters = 4;
	  break;

 	case HIST_TYPE_INDIR_CALL:
 	  hist->n_counters = 3;
	  break;

	case HIST_TYPE_AVERAGE:
	  hist->n_counters = 2;
	  break;

	case HIST_TYPE_IOR:
	  hist->n_counters = 1;
	  break;

	default:
	  gcc_unreachable ();
	}
      if (dump_file)
        {
	  fprintf (dump_file, "Stmt ");
          print_gimple_stmt (dump_file, hist->hvalue.stmt, 0, TDF_SLIM);
	  dump_histogram_value (dump_file, hist);
        }
    }
}
