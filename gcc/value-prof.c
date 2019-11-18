/* Transformations based on profile information for values.
   Copyright (C) 2003-2019 Free Software Foundation, Inc.

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
#include "cfghooks.h"
#include "ssa.h"
#include "cgraph.h"
#include "coverage.h"
#include "data-streamer.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "tree-nested.h"
#include "calls.h"
#include "expr.h"
#include "value-prof.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "gimple-pretty-print.h"
#include "dumpfile.h"
#include "builtins.h"

/* In this file value profile based optimizations are placed.  Currently the
   following optimizations are implemented (for more detailed descriptions
   see comments at value_profile_transformations):

   1) Division/modulo specialization.  Provided that we can determine that the
      operands of the division have some special properties, we may use it to
      produce more effective code.

   2) Indirect/virtual call specialization. If we can determine most
      common function callee in indirect/virtual call. We can use this
      information to improve code effectiveness (especially info for
      the inliner).

   3) Speculative prefetching.  If we are able to determine that the difference
      between addresses accessed by a memory reference is usually constant, we
      may add the prefetch instructions.
      FIXME: This transformation was removed together with RTL based value
      profiling.


   Value profiling internals
   ==========================

   Every value profiling transformation starts with defining what values
   to profile.  There are different histogram types (see HIST_TYPE_* in
   value-prof.h) and each transformation can request one or more histogram
   types per GIMPLE statement.  The function gimple_find_values_to_profile()
   collects the values to profile in a vec, and adds the number of counters
   required for the different histogram types.

   For a -fprofile-generate run, the statements for which values should be
   recorded, are instrumented in instrument_values().  The instrumentation
   is done by helper functions that can be found in tree-profile.c, where
   new types of histograms can be added if necessary.

   After a -fprofile-use, the value profiling data is read back in by
   compute_value_histograms() that translates the collected data to
   histograms and attaches them to the profiled statements via
   gimple_add_histogram_value().  Histograms are stored in a hash table
   that is attached to every intrumented function, see VALUE_HISTOGRAMS
   in function.h.
   
   The value-profile transformations driver is the function
   gimple_value_profile_transformations().  It traverses all statements in
   the to-be-transformed function, and looks for statements with one or
   more histograms attached to it.  If a statement has histograms, the
   transformation functions are called on the statement.

   Limitations / FIXME / TODO:
   * Only one histogram of each type can be associated with a statement.
   * Some value profile transformations are done in builtins.c (?!)
   * Updating of histograms needs some TLC.
   * The value profiling code could be used to record analysis results
     from non-profiling (e.g. VRP).
   * Adding new profilers should be simplified, starting with a cleanup
     of what-happens-where and with making gimple_find_values_to_profile
     and gimple_value_profile_transformations table-driven, perhaps...
*/

static bool gimple_divmod_fixed_value_transform (gimple_stmt_iterator *);
static bool gimple_mod_pow2_value_transform (gimple_stmt_iterator *);
static bool gimple_mod_subtract_transform (gimple_stmt_iterator *);
static bool gimple_stringops_transform (gimple_stmt_iterator *);
static bool gimple_ic_transform (gimple_stmt_iterator *);

/* Allocate histogram value.  */

histogram_value
gimple_alloc_histogram_value (struct function *fun ATTRIBUTE_UNUSED,
			      enum hist_type type, gimple *stmt, tree value)
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

/* Return nonzero if statement for histogram_value X is Y.  */

static int
histogram_eq (const void *x, const void *y)
{
  return ((const_histogram_value) x)->hvalue.stmt == (const gimple *) y;
}

/* Set histogram for STMT.  */

static void
set_histogram_value (struct function *fun, gimple *stmt, histogram_value hist)
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
gimple_histogram_value (struct function *fun, gimple *stmt)
{
  if (!VALUE_HISTOGRAMS (fun))
    return NULL;
  return (histogram_value) htab_find_with_hash (VALUE_HISTOGRAMS (fun), stmt,
						htab_hash_pointer (stmt));
}

/* Add histogram for STMT.  */

void
gimple_add_histogram_value (struct function *fun, gimple *stmt,
			    histogram_value hist)
{
  hist->hvalue.next = gimple_histogram_value (fun, stmt);
  set_histogram_value (fun, stmt, hist);
  hist->fun = fun;
}

/* Remove histogram HIST from STMT's histogram list.  */

void
gimple_remove_histogram_value (struct function *fun, gimple *stmt,
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
  if (flag_checking)
    memset (hist, 0xab, sizeof (*hist));
  free (hist);
}

/* Lookup histogram of type TYPE in the STMT.  */

histogram_value
gimple_histogram_value_of_type (struct function *fun, gimple *stmt,
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
      if (hist->hvalue.counters)
	{
	  fprintf (dump_file, "Interval counter range [%d,%d]: [",
		   hist->hdata.intvl.int_start,
		   (hist->hdata.intvl.int_start
		    + hist->hdata.intvl.steps - 1));

	  unsigned int i;
	  for (i = 0; i < hist->hdata.intvl.steps; i++)
	    {
	      fprintf (dump_file, "%d:%" PRId64,
		       hist->hdata.intvl.int_start + i,
		       (int64_t) hist->hvalue.counters[i]);
	      if (i != hist->hdata.intvl.steps - 1)
		fprintf (dump_file, ", ");
	    }
	  fprintf (dump_file, "] outside range: %" PRId64 ".\n",
		   (int64_t) hist->hvalue.counters[i]);
	}
      break;

    case HIST_TYPE_POW2:
      if (hist->hvalue.counters)
	fprintf (dump_file, "Pow2 counter pow2:%" PRId64
		 " nonpow2:%" PRId64 ".\n",
		 (int64_t) hist->hvalue.counters[1],
		 (int64_t) hist->hvalue.counters[0]);
      break;

    case HIST_TYPE_TOPN_VALUES:
    case HIST_TYPE_INDIR_CALL:
      if (hist->hvalue.counters)
	{
	  fprintf (dump_file,
		   (hist->type == HIST_TYPE_TOPN_VALUES
		    ? "Top N value counter " : "Indirect call counter"));
	  if (hist->hvalue.counters)
	    {
	      fprintf (dump_file, "all: %" PRId64 ", values: ",
		       (int64_t) hist->hvalue.counters[0]);
	      for (unsigned i = 0; i < GCOV_TOPN_VALUES; i++)
		{
		  fprintf (dump_file, "[%" PRId64 ":%" PRId64 "]",
			   (int64_t) hist->hvalue.counters[2 * i + 1],
			   (int64_t) hist->hvalue.counters[2 * i + 2]);
		  if (i != GCOV_TOPN_VALUES - 1)
		    fprintf (dump_file, ", ");
		}
	      fprintf (dump_file, ".\n");
	    }
	}
      break;

    case HIST_TYPE_AVERAGE:
      if (hist->hvalue.counters)
	fprintf (dump_file, "Average value sum:%" PRId64
		 " times:%" PRId64 ".\n",
		 (int64_t) hist->hvalue.counters[0],
		 (int64_t) hist->hvalue.counters[1]);
      break;

    case HIST_TYPE_IOR:
      if (hist->hvalue.counters)
	fprintf (dump_file, "IOR value ior:%" PRId64 ".\n",
		 (int64_t) hist->hvalue.counters[0]);
      break;

    case HIST_TYPE_TIME_PROFILE:
      if (hist->hvalue.counters)
	fprintf (dump_file, "Time profile time:%" PRId64 ".\n",
		 (int64_t) hist->hvalue.counters[0]);
      break;
    default:
      gcc_unreachable ();
   }
}

/* Dump information about HIST to DUMP_FILE.  */

void
stream_out_histogram_value (struct output_block *ob, histogram_value hist)
{
  struct bitpack_d bp;
  unsigned int i;

  bp = bitpack_create (ob->main_stream);
  bp_pack_enum (&bp, hist_type, HIST_TYPE_MAX, hist->type);
  bp_pack_value (&bp, hist->hvalue.next != NULL, 1);
  streamer_write_bitpack (&bp);
  switch (hist->type)
    {
    case HIST_TYPE_INTERVAL:
      streamer_write_hwi (ob, hist->hdata.intvl.int_start);
      streamer_write_uhwi (ob, hist->hdata.intvl.steps);
      break;
    default:
      break;
    }
  for (i = 0; i < hist->n_counters; i++)
    {
      /* When user uses an unsigned type with a big value, constant converted
	 to gcov_type (a signed type) can be negative.  */
      gcov_type value = hist->hvalue.counters[i];
      if (hist->type == HIST_TYPE_TOPN_VALUES && i > 0)
	;
      else
	gcc_assert (value >= 0);

      streamer_write_gcov_count (ob, value);
    }
  if (hist->hvalue.next)
    stream_out_histogram_value (ob, hist->hvalue.next);
}

/* Dump information about HIST to DUMP_FILE.  */

void
stream_in_histogram_value (class lto_input_block *ib, gimple *stmt)
{
  enum hist_type type;
  unsigned int ncounters = 0;
  struct bitpack_d bp;
  unsigned int i;
  histogram_value new_val;
  bool next;
  histogram_value *next_p = NULL;

  do
    {
      bp = streamer_read_bitpack (ib);
      type = bp_unpack_enum (&bp, hist_type, HIST_TYPE_MAX);
      next = bp_unpack_value (&bp, 1);
      new_val = gimple_alloc_histogram_value (cfun, type, stmt);
      switch (type)
	{
	case HIST_TYPE_INTERVAL:
	  new_val->hdata.intvl.int_start = streamer_read_hwi (ib);
	  new_val->hdata.intvl.steps = streamer_read_uhwi (ib);
	  ncounters = new_val->hdata.intvl.steps + 2;
	  break;

	case HIST_TYPE_POW2:
	case HIST_TYPE_AVERAGE:
	  ncounters = 2;
	  break;

	case HIST_TYPE_TOPN_VALUES:
	case HIST_TYPE_INDIR_CALL:
	  ncounters = GCOV_TOPN_VALUES_COUNTERS;
	  break;

	case HIST_TYPE_IOR:
        case HIST_TYPE_TIME_PROFILE:
	  ncounters = 1;
	  break;

	default:
	  gcc_unreachable ();
	}
      new_val->hvalue.counters = XNEWVAR (gcov_type, sizeof (*new_val->hvalue.counters) * ncounters);
      new_val->n_counters = ncounters;
      for (i = 0; i < ncounters; i++)
	new_val->hvalue.counters[i] = streamer_read_gcov_count (ib);
      if (!next_p)
	gimple_add_histogram_value (cfun, stmt, new_val);
      else
	*next_p = new_val;
      next_p = &new_val->hvalue.next;
    }
  while (next);
}

/* Dump all histograms attached to STMT to DUMP_FILE.  */

void
dump_histograms_for_stmt (struct function *fun, FILE *dump_file, gimple *stmt)
{
  histogram_value hist;
  for (hist = gimple_histogram_value (fun, stmt); hist; hist = hist->hvalue.next)
    dump_histogram_value (dump_file, hist);
}

/* Remove all histograms associated with STMT.  */

void
gimple_remove_stmt_histograms (struct function *fun, gimple *stmt)
{
  histogram_value val;
  while ((val = gimple_histogram_value (fun, stmt)) != NULL)
    gimple_remove_histogram_value (fun, stmt, val);
}

/* Duplicate all histograms associates with OSTMT to STMT.  */

void
gimple_duplicate_stmt_histograms (struct function *fun, gimple *stmt,
				  struct function *ofun, gimple *ostmt)
{
  histogram_value val;
  for (val = gimple_histogram_value (ofun, ostmt); val != NULL; val = val->hvalue.next)
    {
      histogram_value new_val = gimple_alloc_histogram_value (fun, val->type);
      memcpy (new_val, val, sizeof (*val));
      new_val->hvalue.stmt = stmt;
      new_val->hvalue.counters = XNEWVAR (gcov_type, sizeof (*new_val->hvalue.counters) * new_val->n_counters);
      memcpy (new_val->hvalue.counters, val->hvalue.counters, sizeof (*new_val->hvalue.counters) * new_val->n_counters);
      gimple_add_histogram_value (fun, stmt, new_val);
    }
}

/* Move all histograms associated with OSTMT to STMT.  */

void
gimple_move_stmt_histograms (struct function *fun, gimple *stmt, gimple *ostmt)
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
  hash_set<histogram_value> *visited = (hash_set<histogram_value> *) data;
  histogram_value hist = *(histogram_value *) slot;

  if (!visited->contains (hist)
      && hist->type != HIST_TYPE_TIME_PROFILE)
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

  error_found = false;
  hash_set<histogram_value> visited_hists;
  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);

	for (hist = gimple_histogram_value (cfun, stmt); hist;
	     hist = hist->hvalue.next)
	  {
	    if (hist->hvalue.stmt != stmt)
	      {
		error ("histogram value statement does not correspond to "
		       "the statement it is associated with");
		debug_gimple_stmt (stmt);
		dump_histogram_value (stderr, hist);
		error_found = true;
	      }
            visited_hists.add (hist);
	  }
      }
  if (VALUE_HISTOGRAMS (cfun))
    htab_traverse (VALUE_HISTOGRAMS (cfun), visit_hist, &visited_hists);
  if (error_found)
    internal_error ("%qs failed", __func__);
}

/* Helper function for verify_histograms.  For each histogram reachable via htab
   walk verify that it was reached via statement walk.  */

static int
free_hist (void **slot, void *data ATTRIBUTE_UNUSED)
{
  histogram_value hist = *(histogram_value *) slot;
  free (hist->hvalue.counters);
  free (hist);
  return 1;
}

void
free_histograms (struct function *fn)
{
  if (VALUE_HISTOGRAMS (fn))
    {
      htab_traverse (VALUE_HISTOGRAMS (fn), free_hist, NULL);
      htab_delete (VALUE_HISTOGRAMS (fn));
      VALUE_HISTOGRAMS (fn) = NULL;
    }
}

/* The overall number of invocations of the counter should match
   execution count of basic block.  Report it as error rather than
   internal error as it might mean that user has misused the profile
   somehow.  */

static bool
check_counter (gimple *stmt, const char * name,
	       gcov_type *count, gcov_type *all, profile_count bb_count_d)
{
  gcov_type bb_count = bb_count_d.ipa ().to_gcov_type ();
  if (*all != bb_count || *count > *all)
    {
      dump_user_location_t locus;
      locus = ((stmt != NULL)
	       ? dump_user_location_t (stmt)
	       : dump_user_location_t::from_function_decl
		   (current_function_decl));
      if (flag_profile_correction)
        {
          if (dump_enabled_p ())
            dump_printf_loc (MSG_MISSED_OPTIMIZATION, locus,
                             "correcting inconsistent value profile: %s "
                             "profiler overall count (%d) does not match BB "
                             "count (%d)\n", name, (int)*all, (int)bb_count);
	  *all = bb_count;
	  if (*count > *all)
            *count = *all;
	  return false;
	}
      else
	{
	  error_at (locus.get_location_t (), "corrupted value profile: %s "
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

  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
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
	  if (gimple_mod_subtract_transform (&gsi)
	      || gimple_divmod_fixed_value_transform (&gsi)
	      || gimple_mod_pow2_value_transform (&gsi)
	      || gimple_stringops_transform (&gsi)
	      || gimple_ic_transform (&gsi))
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

  return changed;
}

/* Generate code for transformation 1 (with parent gimple assignment
   STMT and probability of taking the optimal path PROB, which is
   equivalent to COUNT/ALL within roundoff error).  This generates the
   result into a temp and returns the temp; it does not replace or
   alter the original STMT.  */

static tree
gimple_divmod_fixed_value (gassign *stmt, tree value, profile_probability prob,
			   gcov_type count, gcov_type all)
{
  gassign *stmt1, *stmt2;
  gcond *stmt3;
  tree tmp0, tmp1, tmp2;
  gimple *bb1end, *bb2end, *bb3end;
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

  tmp0 = make_temp_ssa_name (optype, NULL, "PROF");
  tmp1 = make_temp_ssa_name (optype, NULL, "PROF");
  stmt1 = gimple_build_assign (tmp0, fold_convert (optype, value));
  stmt2 = gimple_build_assign (tmp1, op2);
  stmt3 = gimple_build_cond (NE_EXPR, tmp1, tmp0, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
  bb1end = stmt3;

  tmp2 = create_tmp_reg (optype, "PROF");
  stmt1 = gimple_build_assign (tmp2, gimple_assign_rhs_code (stmt), op1, tmp0);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb2end = stmt1;

  stmt1 = gimple_build_assign (tmp2, gimple_assign_rhs_code (stmt), op1, op2);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb3end = stmt1;

  /* Fix CFG. */
  /* Edge e23 connects bb2 to bb3, etc. */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  bb2->count = profile_count::from_gcov_type (count);
  e23 = split_block (bb2, bb2end);
  bb3 = e23->dest;
  bb3->count = profile_count::from_gcov_type (all - count);
  e34 = split_block (bb3, bb3end);
  bb4 = e34->dest;
  bb4->count = profile_count::from_gcov_type (all);

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  e12->probability = prob;

  e13 = make_edge (bb, bb3, EDGE_TRUE_VALUE);
  e13->probability = prob.invert ();

  remove_edge (e23);

  e24 = make_edge (bb2, bb4, EDGE_FALLTHRU);
  e24->probability = profile_probability::always ();

  e34->probability = profile_probability::always ();

  return tmp2;
}

/* Return the n-th value count of TOPN_VALUE histogram.  If
   there's a value, return true and set VALUE and COUNT
   arguments.  */

bool
get_nth_most_common_value (gimple *stmt, const char *counter_type,
			   histogram_value hist, gcov_type *value,
			   gcov_type *count, gcov_type *all, unsigned n)
{
  if (hist->hvalue.counters[2] == -1)
    return false;

  gcc_assert (n < GCOV_TOPN_VALUES);

  *count = 0;
  *value = 0;

  gcov_type read_all = hist->hvalue.counters[0];

  gcov_type v = hist->hvalue.counters[2 * n + 1];
  gcov_type c = hist->hvalue.counters[2 * n + 2];

  /* Indirect calls can't be verified.  */
  if (stmt
      && check_counter (stmt, counter_type, &c, &read_all,
			gimple_bb (stmt)->count))
    return false;

  *all = read_all;

  *value = v;
  *count = c;
  return true;
}

/* Do transform 1) on INSN if applicable.  */

static bool
gimple_divmod_fixed_value_transform (gimple_stmt_iterator *si)
{
  histogram_value histogram;
  enum tree_code code;
  gcov_type val, count, all;
  tree result, value, tree_val;
  profile_probability prob;
  gassign *stmt;

  stmt = dyn_cast <gassign *> (gsi_stmt (*si));
  if (!stmt)
    return false;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (gimple_assign_lhs (stmt))))
    return false;

  code = gimple_assign_rhs_code (stmt);

  if (code != TRUNC_DIV_EXPR && code != TRUNC_MOD_EXPR)
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt,
					      HIST_TYPE_TOPN_VALUES);
  if (!histogram)
    return false;

  if (!get_nth_most_common_value (stmt, "divmod", histogram, &val, &count,
				  &all))
    return false;

  value = histogram->hvalue.value;
  gimple_remove_histogram_value (cfun, stmt, histogram);

  /* We require that count is at least half of all.  */
  if (simple_cst_equal (gimple_assign_rhs2 (stmt), value) != 1
      || 2 * count < all
      || optimize_bb_for_size_p (gimple_bb (stmt)))
    return false;

  /* Compute probability of taking the optimal path.  */
  if (all > 0)
    prob = profile_probability::probability_in_gcov_type (count, all);
  else
    prob = profile_probability::never ();

  if (sizeof (gcov_type) == sizeof (HOST_WIDE_INT))
    tree_val = build_int_cst (get_gcov_type (), val);
  else
    {
      HOST_WIDE_INT a[2];
      a[0] = (unsigned HOST_WIDE_INT) val;
      a[1] = val >> (HOST_BITS_PER_WIDE_INT - 1) >> 1;

      tree_val = wide_int_to_tree (get_gcov_type (), wide_int::from_array (a, 2,
	TYPE_PRECISION (get_gcov_type ()), false));
    }
  result = gimple_divmod_fixed_value (stmt, tree_val, prob, count, all);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
		     "Transformation done: div/mod by constant %T\n", tree_val);

  gimple_assign_set_rhs_from_tree (si, result);
  update_stmt (gsi_stmt (*si));

  return true;
}

/* Generate code for transformation 2 (with parent gimple assign STMT and
   probability of taking the optimal path PROB, which is equivalent to COUNT/ALL
   within roundoff error).  This generates the result into a temp and returns
   the temp; it does not replace or alter the original STMT.  */

static tree
gimple_mod_pow2 (gassign *stmt, profile_probability prob, gcov_type count, gcov_type all)
{
  gassign *stmt1, *stmt2, *stmt3;
  gcond *stmt4;
  tree tmp2, tmp3;
  gimple *bb1end, *bb2end, *bb3end;
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

  result = create_tmp_reg (optype, "PROF");
  tmp2 = make_temp_ssa_name (optype, NULL, "PROF");
  tmp3 = make_temp_ssa_name (optype, NULL, "PROF");
  stmt2 = gimple_build_assign (tmp2, PLUS_EXPR, op2,
			       build_int_cst (optype, -1));
  stmt3 = gimple_build_assign (tmp3, BIT_AND_EXPR, tmp2, op2);
  stmt4 = gimple_build_cond (NE_EXPR, tmp3, build_int_cst (optype, 0),
			     NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt4, GSI_SAME_STMT);
  bb1end = stmt4;

  /* tmp2 == op2-1 inherited from previous block.  */
  stmt1 = gimple_build_assign (result, BIT_AND_EXPR, op1, tmp2);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb2end = stmt1;

  stmt1 = gimple_build_assign (result, gimple_assign_rhs_code (stmt),
			       op1, op2);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb3end = stmt1;

  /* Fix CFG. */
  /* Edge e23 connects bb2 to bb3, etc. */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  bb2->count = profile_count::from_gcov_type (count);
  e23 = split_block (bb2, bb2end);
  bb3 = e23->dest;
  bb3->count = profile_count::from_gcov_type (all - count);
  e34 = split_block (bb3, bb3end);
  bb4 = e34->dest;
  bb4->count = profile_count::from_gcov_type (all);

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  e12->probability = prob;

  e13 = make_edge (bb, bb3, EDGE_TRUE_VALUE);
  e13->probability = prob.invert ();

  remove_edge (e23);

  e24 = make_edge (bb2, bb4, EDGE_FALLTHRU);
  e24->probability = profile_probability::always ();

  e34->probability = profile_probability::always ();

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
  profile_probability prob;
  gassign *stmt;

  stmt = dyn_cast <gassign *> (gsi_stmt (*si));
  if (!stmt)
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

  /* Compute probability of taking the optimal path.  */
  all = count + wrong_values;

  if (check_counter (stmt, "pow2", &count, &all, gimple_bb (stmt)->count))
    return false;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
		     "Transformation done: mod power of 2\n");

  if (all > 0)
    prob = profile_probability::probability_in_gcov_type (count, all);
  else
    prob = profile_probability::never ();

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
gimple_mod_subtract (gassign *stmt, profile_probability prob1,
		     profile_probability prob2, int ncounts,
		     gcov_type count1, gcov_type count2, gcov_type all)
{
  gassign *stmt1;
  gimple *stmt2;
  gcond *stmt3;
  tree tmp1;
  gimple *bb1end, *bb2end = NULL, *bb3end;
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

  result = create_tmp_reg (optype, "PROF");
  tmp1 = make_temp_ssa_name (optype, NULL, "PROF");
  stmt1 = gimple_build_assign (result, op1);
  stmt2 = gimple_build_assign (tmp1, op2);
  stmt3 = gimple_build_cond (LT_EXPR, result, tmp1, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
  bb1end = stmt3;

  if (ncounts)	/* Assumed to be 0 or 1 */
    {
      stmt1 = gimple_build_assign (result, MINUS_EXPR, result, tmp1);
      stmt2 = gimple_build_cond (LT_EXPR, result, tmp1, NULL_TREE, NULL_TREE);
      gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
      gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
      bb2end = stmt2;
    }

  /* Fallback case. */
  stmt1 = gimple_build_assign (result, gimple_assign_rhs_code (stmt),
			       result, tmp1);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  bb3end = stmt1;

  /* Fix CFG. */
  /* Edge e23 connects bb2 to bb3, etc. */
  /* However block 3 is optional; if it is not there, references
     to 3 really refer to block 2. */
  e12 = split_block (bb, bb1end);
  bb2 = e12->dest;
  bb2->count = profile_count::from_gcov_type (all - count1);

  if (ncounts)	/* Assumed to be 0 or 1.  */
    {
      e23 = split_block (bb2, bb2end);
      bb3 = e23->dest;
      bb3->count = profile_count::from_gcov_type (all - count1 - count2);
    }

  e34 = split_block (ncounts ? bb3 : bb2, bb3end);
  bb4 = e34->dest;
  bb4->count = profile_count::from_gcov_type (all);

  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_FALSE_VALUE;
  e12->probability = prob1.invert ();

  e14 = make_edge (bb, bb4, EDGE_TRUE_VALUE);
  e14->probability = prob1;

  if (ncounts)  /* Assumed to be 0 or 1.  */
    {
      e23->flags &= ~EDGE_FALLTHRU;
      e23->flags |= EDGE_FALSE_VALUE;
      e23->probability = prob2.invert ();

      e24 = make_edge (bb2, bb4, EDGE_TRUE_VALUE);
      e24->probability = prob2;
    }

  e34->probability = profile_probability::always ();

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
  profile_probability prob1, prob2;
  unsigned int i, steps;
  gcov_type count1, count2;
  gassign *stmt;
  stmt = dyn_cast <gassign *> (gsi_stmt (*si));
  if (!stmt)
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
  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
		     "Transformation done: mod subtract\n");

  /* Compute probability of taking the optimal path(s).  */
  if (all > 0)
    {
      prob1 = profile_probability::probability_in_gcov_type (count1, all);
      prob2 = profile_probability::probability_in_gcov_type (count2, all);
    }
  else
    {
      prob1 = prob2 = profile_probability::never ();
    }

  /* In practice, "steps" is always 2.  This interface reflects this,
     and will need to be changed if "steps" can change.  */
  result = gimple_mod_subtract (stmt, prob1, prob2, i, count1, count2, all);

  gimple_assign_set_rhs_from_tree (si, result);
  update_stmt (gsi_stmt (*si));

  return true;
}

typedef int_hash <unsigned int, 0, UINT_MAX> profile_id_hash;

static hash_map<profile_id_hash, cgraph_node *> *cgraph_node_map = 0;

/* Returns true if node graph is initialized. This
   is used to test if profile_id has been created
   for cgraph_nodes.  */

bool
coverage_node_map_initialized_p (void)
{
  return cgraph_node_map != 0;
}

/* Initialize map from PROFILE_ID to CGRAPH_NODE.
   When LOCAL is true, the PROFILE_IDs are computed.  when it is false we assume
   that the PROFILE_IDs was already assigned.  */

void
init_node_map (bool local)
{
  struct cgraph_node *n;
  cgraph_node_map = new hash_map<profile_id_hash, cgraph_node *>;

  FOR_EACH_DEFINED_FUNCTION (n)
    if (n->has_gimple_body_p () || n->thunk.thunk_p)
      {
	cgraph_node **val;
	dump_user_location_t loc
	  = dump_user_location_t::from_function_decl (n->decl);
	if (local)
	  {
	    n->profile_id = coverage_compute_profile_id (n);
	    while ((val = cgraph_node_map->get (n->profile_id))
		   || !n->profile_id)
	      {
		if (dump_enabled_p ())
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
				   "Local profile-id %i conflict"
				   " with nodes %s %s\n",
				   n->profile_id,
				   n->dump_name (),
				   (*val)->dump_name ());
		n->profile_id = (n->profile_id + 1) & 0x7fffffff;
	      }
	  }
	else if (!n->profile_id)
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			       "Node %s has no profile-id"
			       " (profile feedback missing?)\n",
			       n->dump_name ());
	    continue;
	  }
	else if ((val = cgraph_node_map->get (n->profile_id)))
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, loc,
			       "Node %s has IP profile-id %i conflict. "
			       "Giving up.\n",
			       n->dump_name (), n->profile_id);
	    *val = NULL;
	    continue;
	  }
	cgraph_node_map->put (n->profile_id, n);
      }
}

/* Delete the CGRAPH_NODE_MAP.  */

void
del_node_map (void)
{
  delete cgraph_node_map;
}

/* Return cgraph node for function with pid */

struct cgraph_node*
find_func_by_profile_id (int profile_id)
{
  cgraph_node **val = cgraph_node_map->get (profile_id);
  if (val)
    return *val;
  else
    return NULL;
}

/* Do transformation

  if (actual_callee_address == address_of_most_common_function/method)
    do direct call
  else
    old call
 */

gcall *
gimple_ic (gcall *icall_stmt, struct cgraph_node *direct_call,
	   profile_probability prob)
{
  gcall *dcall_stmt;
  gassign *load_stmt;
  gcond *cond_stmt;
  tree tmp0, tmp1, tmp;
  basic_block cond_bb, dcall_bb, icall_bb, join_bb = NULL;
  edge e_cd, e_ci, e_di, e_dj = NULL, e_ij;
  gimple_stmt_iterator gsi;
  int lp_nr, dflags;
  edge e_eh, e;
  edge_iterator ei;

  cond_bb = gimple_bb (icall_stmt);
  gsi = gsi_for_stmt (icall_stmt);

  tmp0 = make_temp_ssa_name (ptr_type_node, NULL, "PROF");
  tmp1 = make_temp_ssa_name (ptr_type_node, NULL, "PROF");
  tmp = unshare_expr (gimple_call_fn (icall_stmt));
  load_stmt = gimple_build_assign (tmp0, tmp);
  gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);

  tmp = fold_convert (ptr_type_node, build_addr (direct_call->decl));
  load_stmt = gimple_build_assign (tmp1, tmp);
  gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);

  cond_stmt = gimple_build_cond (EQ_EXPR, tmp1, tmp0, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);

  if (TREE_CODE (gimple_vdef (icall_stmt)) == SSA_NAME)
    {
      unlink_stmt_vdef (icall_stmt);
      release_ssa_name (gimple_vdef (icall_stmt));
    }
  gimple_set_vdef (icall_stmt, NULL_TREE);
  gimple_set_vuse (icall_stmt, NULL_TREE);
  update_stmt (icall_stmt);
  dcall_stmt = as_a <gcall *> (gimple_copy (icall_stmt));
  gimple_call_set_fndecl (dcall_stmt, direct_call->decl);
  dflags = flags_from_decl_or_type (direct_call->decl);
  if ((dflags & ECF_NORETURN) != 0
      && should_remove_lhs_p (gimple_call_lhs (dcall_stmt)))
    gimple_call_set_lhs (dcall_stmt, NULL_TREE);
  gsi_insert_before (&gsi, dcall_stmt, GSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e_cd connects cond_bb to dcall_bb, etc; note the first letters. */
  e_cd = split_block (cond_bb, cond_stmt);
  dcall_bb = e_cd->dest;
  dcall_bb->count = cond_bb->count.apply_probability (prob);

  e_di = split_block (dcall_bb, dcall_stmt);
  icall_bb = e_di->dest;
  icall_bb->count = cond_bb->count - dcall_bb->count;

  /* Do not disturb existing EH edges from the indirect call.  */
  if (!stmt_ends_bb_p (icall_stmt))
    e_ij = split_block (icall_bb, icall_stmt);
  else
    {
      e_ij = find_fallthru_edge (icall_bb->succs);
      /* The indirect call might be noreturn.  */
      if (e_ij != NULL)
	{
	  e_ij->probability = profile_probability::always ();
	  e_ij = single_pred_edge (split_edge (e_ij));
	}
    }
  if (e_ij != NULL)
    {
      join_bb = e_ij->dest;
      join_bb->count = cond_bb->count;
    }

  e_cd->flags = (e_cd->flags & ~EDGE_FALLTHRU) | EDGE_TRUE_VALUE;
  e_cd->probability = prob;

  e_ci = make_edge (cond_bb, icall_bb, EDGE_FALSE_VALUE);
  e_ci->probability = prob.invert ();

  remove_edge (e_di);

  if (e_ij != NULL)
    {
      if ((dflags & ECF_NORETURN) == 0)
	{
	  e_dj = make_edge (dcall_bb, join_bb, EDGE_FALLTHRU);
	  e_dj->probability = profile_probability::always ();
	}
      e_ij->probability = profile_probability::always ();
    }

  /* Insert PHI node for the call result if necessary.  */
  if (gimple_call_lhs (icall_stmt)
      && TREE_CODE (gimple_call_lhs (icall_stmt)) == SSA_NAME
      && (dflags & ECF_NORETURN) == 0)
    {
      tree result = gimple_call_lhs (icall_stmt);
      gphi *phi = create_phi_node (result, join_bb);
      gimple_call_set_lhs (icall_stmt,
			   duplicate_ssa_name (result, icall_stmt));
      add_phi_arg (phi, gimple_call_lhs (icall_stmt), e_ij, UNKNOWN_LOCATION);
      gimple_call_set_lhs (dcall_stmt,
			   duplicate_ssa_name (result, dcall_stmt));
      add_phi_arg (phi, gimple_call_lhs (dcall_stmt), e_dj, UNKNOWN_LOCATION);
    }

  /* Build an EH edge for the direct call if necessary.  */
  lp_nr = lookup_stmt_eh_lp (icall_stmt);
  if (lp_nr > 0 && stmt_could_throw_p (cfun, dcall_stmt))
    {
      add_stmt_to_eh_lp (dcall_stmt, lp_nr);
    }

  FOR_EACH_EDGE (e_eh, ei, icall_bb->succs)
    if (e_eh->flags & (EDGE_EH | EDGE_ABNORMAL))
      {
	e = make_edge (dcall_bb, e_eh->dest, e_eh->flags);
	e->probability = e_eh->probability;
	for (gphi_iterator psi = gsi_start_phis (e_eh->dest);
	     !gsi_end_p (psi); gsi_next (&psi))
	  {
	    gphi *phi = psi.phi ();
	    SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e),
		     PHI_ARG_DEF_FROM_EDGE (phi, e_eh));
	  }
       }
  if (!stmt_could_throw_p (cfun, dcall_stmt))
    gimple_purge_dead_eh_edges (dcall_bb);
  return dcall_stmt;
}

/*
  For every checked indirect/virtual call determine if most common pid of
  function/class method has probability more than 50%. If yes modify code of
  this call to:
 */

static bool
gimple_ic_transform (gimple_stmt_iterator *gsi)
{
  gcall *stmt;
  histogram_value histogram;
  gcov_type val, count, all;
  struct cgraph_node *direct_call;

  stmt = dyn_cast <gcall *> (gsi_stmt (*gsi));
  if (!stmt)
    return false;

  if (gimple_call_fndecl (stmt) != NULL_TREE)
    return false;

  if (gimple_call_internal_p (stmt))
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt, HIST_TYPE_INDIR_CALL);
  if (!histogram)
    return false;

  if (!get_nth_most_common_value (NULL, "indirect call", histogram, &val,
				  &count, &all))
    return false;

  if (4 * count <= 3 * all)
    return false;

  direct_call = find_func_by_profile_id ((int)val);

  if (direct_call == NULL)
    {
      if (val)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, stmt,
			     "Indirect call -> direct call from other "
			     "module %T=> %i (will resolve only with LTO)\n",
			     gimple_call_fn (stmt), (int)val);
	}
      return false;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
		       "Indirect call -> direct call "
		       "%T => %T transformation on insn postponed\n",
		       gimple_call_fn (stmt), direct_call->decl);
      dump_printf_loc (MSG_NOTE, stmt,
		       "hist->count %" PRId64
		       " hist->all %" PRId64"\n", count, all);
    }

  return true;
}

/* Return true if the stringop CALL shall be profiled.  SIZE_ARG be
   set to the argument index for the size of the string operation.  */

static bool
interesting_stringop_to_profile_p (gcall *call, int *size_arg)
{
  enum built_in_function fcode;

  fcode = DECL_FUNCTION_CODE (gimple_call_fndecl (call));
  switch (fcode)
    {
     case BUILT_IN_MEMCPY:
     case BUILT_IN_MEMPCPY:
     case BUILT_IN_MEMMOVE:
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
       return false;
    }
}

/* Convert stringop (..., vcall_size)
   into
   if (vcall_size == icall_size)
     stringop (..., icall_size);
   else
     stringop (..., vcall_size);
   assuming we'll propagate a true constant into ICALL_SIZE later.  */

static void
gimple_stringop_fixed_value (gcall *vcall_stmt, tree icall_size, profile_probability prob,
			     gcov_type count, gcov_type all)
{
  gassign *tmp_stmt;
  gcond *cond_stmt;
  gcall *icall_stmt;
  tree tmp0, tmp1, vcall_size, optype;
  basic_block cond_bb, icall_bb, vcall_bb, join_bb;
  edge e_ci, e_cv, e_iv, e_ij, e_vj;
  gimple_stmt_iterator gsi;
  int size_arg;

  if (!interesting_stringop_to_profile_p (vcall_stmt, &size_arg))
    gcc_unreachable ();

  cond_bb = gimple_bb (vcall_stmt);
  gsi = gsi_for_stmt (vcall_stmt);

  vcall_size = gimple_call_arg (vcall_stmt, size_arg);
  optype = TREE_TYPE (vcall_size);

  tmp0 = make_temp_ssa_name (optype, NULL, "PROF");
  tmp1 = make_temp_ssa_name (optype, NULL, "PROF");
  tmp_stmt = gimple_build_assign (tmp0, fold_convert (optype, icall_size));
  gsi_insert_before (&gsi, tmp_stmt, GSI_SAME_STMT);

  tmp_stmt = gimple_build_assign (tmp1, vcall_size);
  gsi_insert_before (&gsi, tmp_stmt, GSI_SAME_STMT);

  cond_stmt = gimple_build_cond (EQ_EXPR, tmp1, tmp0, NULL_TREE, NULL_TREE);
  gsi_insert_before (&gsi, cond_stmt, GSI_SAME_STMT);

  if (TREE_CODE (gimple_vdef (vcall_stmt)) == SSA_NAME)
    {
      unlink_stmt_vdef (vcall_stmt);
      release_ssa_name (gimple_vdef (vcall_stmt));
    }
  gimple_set_vdef (vcall_stmt, NULL);
  gimple_set_vuse (vcall_stmt, NULL);
  update_stmt (vcall_stmt);
  icall_stmt = as_a <gcall *> (gimple_copy (vcall_stmt));
  gimple_call_set_arg (icall_stmt, size_arg,
		       fold_convert (optype, icall_size));
  gsi_insert_before (&gsi, icall_stmt, GSI_SAME_STMT);

  /* Fix CFG. */
  /* Edge e_ci connects cond_bb to icall_bb, etc. */
  e_ci = split_block (cond_bb, cond_stmt);
  icall_bb = e_ci->dest;
  icall_bb->count = profile_count::from_gcov_type (count);

  e_iv = split_block (icall_bb, icall_stmt);
  vcall_bb = e_iv->dest;
  vcall_bb->count = profile_count::from_gcov_type (all - count);

  e_vj = split_block (vcall_bb, vcall_stmt);
  join_bb = e_vj->dest;
  join_bb->count = profile_count::from_gcov_type (all);

  e_ci->flags = (e_ci->flags & ~EDGE_FALLTHRU) | EDGE_TRUE_VALUE;
  e_ci->probability = prob;

  e_cv = make_edge (cond_bb, vcall_bb, EDGE_FALSE_VALUE);
  e_cv->probability = prob.invert ();

  remove_edge (e_iv);

  e_ij = make_edge (icall_bb, join_bb, EDGE_FALLTHRU);
  e_ij->probability = profile_probability::always ();

  e_vj->probability = profile_probability::always ();

  /* Insert PHI node for the call result if necessary.  */
  if (gimple_call_lhs (vcall_stmt)
      && TREE_CODE (gimple_call_lhs (vcall_stmt)) == SSA_NAME)
    {
      tree result = gimple_call_lhs (vcall_stmt);
      gphi *phi = create_phi_node (result, join_bb);
      gimple_call_set_lhs (vcall_stmt,
			   duplicate_ssa_name (result, vcall_stmt));
      add_phi_arg (phi, gimple_call_lhs (vcall_stmt), e_vj, UNKNOWN_LOCATION);
      gimple_call_set_lhs (icall_stmt,
			   duplicate_ssa_name (result, icall_stmt));
      add_phi_arg (phi, gimple_call_lhs (icall_stmt), e_ij, UNKNOWN_LOCATION);
    }

  /* Because these are all string op builtins, they're all nothrow.  */
  gcc_assert (!stmt_could_throw_p (cfun, vcall_stmt));
  gcc_assert (!stmt_could_throw_p (cfun, icall_stmt));
}

/* Find values inside STMT for that we want to measure histograms for
   division/modulo optimization.  */

static bool
gimple_stringops_transform (gimple_stmt_iterator *gsi)
{
  gcall *stmt;
  tree blck_size;
  enum built_in_function fcode;
  histogram_value histogram;
  gcov_type count, all, val;
  tree dest, src;
  unsigned int dest_align, src_align;
  profile_probability prob;
  tree tree_val;
  int size_arg;

  stmt = dyn_cast <gcall *> (gsi_stmt (*gsi));
  if (!stmt)
    return false;

  if (!gimple_call_builtin_p (gsi_stmt (*gsi), BUILT_IN_NORMAL))
    return false;

  if (!interesting_stringop_to_profile_p (stmt, &size_arg))
    return false;

  blck_size = gimple_call_arg (stmt, size_arg);
  if (TREE_CODE (blck_size) == INTEGER_CST)
    return false;

  histogram = gimple_histogram_value_of_type (cfun, stmt,
					      HIST_TYPE_TOPN_VALUES);
  if (!histogram)
    return false;

  if (!get_nth_most_common_value (stmt, "stringops", histogram, &val, &count,
				  &all))
    return false;

  gimple_remove_histogram_value (cfun, stmt, histogram);

  /* We require that count is at least half of all.  */
  if (2 * count < all || optimize_bb_for_size_p (gimple_bb (stmt)))
    return false;
  if (check_counter (stmt, "value", &count, &all, gimple_bb (stmt)->count))
    return false;
  if (all > 0)
    prob = profile_probability::probability_in_gcov_type (count, all);
  else
    prob = profile_probability::never ();

  dest = gimple_call_arg (stmt, 0);
  dest_align = get_pointer_alignment (dest);
  fcode = DECL_FUNCTION_CODE (gimple_call_fndecl (stmt));
  switch (fcode)
    {
    case BUILT_IN_MEMCPY:
    case BUILT_IN_MEMPCPY:
    case BUILT_IN_MEMMOVE:
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

  if (sizeof (gcov_type) == sizeof (HOST_WIDE_INT))
    tree_val = build_int_cst (get_gcov_type (), val);
  else
    {
      HOST_WIDE_INT a[2];
      a[0] = (unsigned HOST_WIDE_INT) val;
      a[1] = val >> (HOST_BITS_PER_WIDE_INT - 1) >> 1;

      tree_val = wide_int_to_tree (get_gcov_type (), wide_int::from_array (a, 2,
	TYPE_PRECISION (get_gcov_type ()), false));
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, stmt,
		     "Transformation done: single value %i stringop for %s\n",
		     (int)val, built_in_names[(int)fcode]);

  gimple_stringop_fixed_value (stmt, tree_val, prob, count, all);

  return true;
}

void
stringop_block_profile (gimple *stmt, unsigned int *expected_align,
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
      unsigned int alignment;

      count = histogram->hvalue.counters[0];
      alignment = 1;
      while (!(count & alignment)
	     && (alignment <= UINT_MAX / 2 / BITS_PER_UNIT))
	alignment <<= 1;
      *expected_align = alignment * BITS_PER_UNIT;
      gimple_remove_histogram_value (cfun, stmt, histogram);
    }
}


/* Find values inside STMT for that we want to measure histograms for
   division/modulo optimization.  */

static void
gimple_divmod_values_to_profile (gimple *stmt, histogram_values *values)
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

      if (TREE_CODE (divisor) == SSA_NAME)
	/* Check for the case where the divisor is the same value most
	   of the time.  */
	values->safe_push (gimple_alloc_histogram_value (cfun,
							 HIST_TYPE_TOPN_VALUES,
							 stmt, divisor));

      /* For mod, check whether it is not often a noop (or replaceable by
	 a few subtractions).  */
      if (gimple_assign_rhs_code (stmt) == TRUNC_MOD_EXPR
	  && TYPE_UNSIGNED (type)
	  && TREE_CODE (divisor) == SSA_NAME)
	{
          tree val;
          /* Check for a special case where the divisor is power of 2.  */
	  values->safe_push (gimple_alloc_histogram_value (cfun,
							   HIST_TYPE_POW2,
							   stmt, divisor));
	  val = build2 (TRUNC_DIV_EXPR, type, op0, divisor);
	  hist = gimple_alloc_histogram_value (cfun, HIST_TYPE_INTERVAL,
					       stmt, val);
	  hist->hdata.intvl.int_start = 0;
	  hist->hdata.intvl.steps = 2;
	  values->safe_push (hist);
	}
      return;

    default:
      return;
    }
}

/* Find calls inside STMT for that we want to measure histograms for
   indirect/virtual call optimization. */

static void
gimple_indirect_call_to_profile (gimple *stmt, histogram_values *values)
{
  tree callee;

  if (gimple_code (stmt) != GIMPLE_CALL
      || gimple_call_internal_p (stmt)
      || gimple_call_fndecl (stmt) != NULL_TREE)
    return;

  callee = gimple_call_fn (stmt);
  histogram_value v = gimple_alloc_histogram_value (cfun, HIST_TYPE_INDIR_CALL,
						    stmt, callee);
  values->safe_push (v);

  return;
}

/* Find values inside STMT for that we want to measure histograms for
   string operations.  */

static void
gimple_stringops_values_to_profile (gimple *gs, histogram_values *values)
{
  gcall *stmt;
  tree blck_size;
  tree dest;
  int size_arg;

  stmt = dyn_cast <gcall *> (gs);
  if (!stmt)
    return;

  if (!gimple_call_builtin_p (gs, BUILT_IN_NORMAL))
    return;

  if (!interesting_stringop_to_profile_p (stmt, &size_arg))
    return;

  dest = gimple_call_arg (stmt, 0);
  blck_size = gimple_call_arg (stmt, size_arg);

  if (TREE_CODE (blck_size) != INTEGER_CST)
    {
      values->safe_push (gimple_alloc_histogram_value (cfun,
						       HIST_TYPE_TOPN_VALUES,
						       stmt, blck_size));
      values->safe_push (gimple_alloc_histogram_value (cfun, HIST_TYPE_AVERAGE,
						       stmt, blck_size));
    }

  if (TREE_CODE (blck_size) != INTEGER_CST)
    values->safe_push (gimple_alloc_histogram_value (cfun, HIST_TYPE_IOR,
						     stmt, dest));
}

/* Find values inside STMT for that we want to measure histograms and adds
   them to list VALUES.  */

static void
gimple_values_to_profile (gimple *stmt, histogram_values *values)
{
  gimple_divmod_values_to_profile (stmt, values);
  gimple_stringops_values_to_profile (stmt, values);
  gimple_indirect_call_to_profile (stmt, values);
}

void
gimple_find_values_to_profile (histogram_values *values)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  unsigned i;
  histogram_value hist = NULL;
  values->create (0);

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      gimple_values_to_profile (gsi_stmt (gsi), values);

  values->safe_push (gimple_alloc_histogram_value (cfun,
						   HIST_TYPE_TIME_PROFILE));

  FOR_EACH_VEC_ELT (*values, i, hist)
    {
      switch (hist->type)
        {
	case HIST_TYPE_INTERVAL:
	  hist->n_counters = hist->hdata.intvl.steps + 2;
	  break;

	case HIST_TYPE_POW2:
	  hist->n_counters = 2;
	  break;

	case HIST_TYPE_TOPN_VALUES:
	case HIST_TYPE_INDIR_CALL:
	  hist->n_counters = GCOV_TOPN_VALUES_COUNTERS;
	  break;

        case HIST_TYPE_TIME_PROFILE:
          hist->n_counters = 1;
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
      if (dump_file && hist->hvalue.stmt != NULL)
        {
	  fprintf (dump_file, "Stmt ");
          print_gimple_stmt (dump_file, hist->hvalue.stmt, 0, TDF_SLIM);
	  dump_histogram_value (dump_file, hist);
        }
    }
}
