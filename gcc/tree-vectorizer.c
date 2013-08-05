/* Vectorizer
   Copyright (C) 2003-2013 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>

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

/* Loop and basic block vectorizer.

  This file contains drivers for the three vectorizers:
  (1) loop vectorizer (inter-iteration parallelism),
  (2) loop-aware SLP (intra-iteration parallelism) (invoked by the loop
      vectorizer)
  (3) BB vectorizer (out-of-loops), aka SLP

  The rest of the vectorizer's code is organized as follows:
  - tree-vect-loop.c - loop specific parts such as reductions, etc. These are
    used by drivers (1) and (2).
  - tree-vect-loop-manip.c - vectorizer's loop control-flow utilities, used by
    drivers (1) and (2).
  - tree-vect-slp.c - BB vectorization specific analysis and transformation,
    used by drivers (2) and (3).
  - tree-vect-stmts.c - statements analysis and transformation (used by all).
  - tree-vect-data-refs.c - vectorizer specific data-refs analysis and
    manipulations (used by all).
  - tree-vect-patterns.c - vectorizable code patterns detector (used by all)

  Here's a poor attempt at illustrating that:

     tree-vectorizer.c:
     loop_vect()  loop_aware_slp()  slp_vect()
          |        /           \          /
          |       /             \        /
          tree-vect-loop.c  tree-vect-slp.c
                | \      \  /      /   |
                |  \      \/      /    |
                |   \     /\     /     |
                |    \   /  \   /      |
         tree-vect-stmts.c  tree-vect-data-refs.c
                       \      /
                    tree-vect-patterns.c
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "dumpfile.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "tree-pretty-print.h"
#include "tree-flow.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "tree-pass.h"

/* Loop or bb location.  */
LOC vect_location;

/* Vector mapping GIMPLE stmt to stmt_vec_info. */
vec<vec_void_p> stmt_vec_info_vec;


/* Function vectorize_loops.

   Entry point to loop vectorization phase.  */

unsigned
vectorize_loops (void)
{
  unsigned int i;
  unsigned int num_vectorized_loops = 0;
  unsigned int vect_loops_num;
  loop_iterator li;
  struct loop *loop;

  vect_loops_num = number_of_loops (cfun);

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    return 0;

  init_stmt_vec_info_vec ();

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number
     than all previously defined loops.  This fact allows us to run
     only over initial loops skipping newly generated ones.  */
  FOR_EACH_LOOP (li, loop, 0)
    if (optimize_loop_nest_for_speed_p (loop))
      {
	loop_vec_info loop_vinfo;
	vect_location = find_loop_location (loop);
        if (LOCATION_LOCUS (vect_location) != UNKNOWN_LOC
	    && dump_enabled_p ())
	  dump_printf (MSG_NOTE, "\nAnalyzing loop at %s:%d\n",
                       LOC_FILE (vect_location), LOC_LINE (vect_location));

	loop_vinfo = vect_analyze_loop (loop);
	loop->aux = loop_vinfo;

	if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
	  continue;

        if (LOCATION_LOCUS (vect_location) != UNKNOWN_LOC
	    && dump_enabled_p ())
          dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                           "Vectorized loop\n");
	vect_transform_loop (loop_vinfo);
	num_vectorized_loops++;
      }

  vect_location = UNKNOWN_LOC;

  statistics_counter_event (cfun, "Vectorized loops", num_vectorized_loops);
  if (dump_enabled_p ()
      || (num_vectorized_loops > 0 && dump_enabled_p ()))
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vectorized %u loops in function.\n",
                     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  for (i = 1; i < vect_loops_num; i++)
    {
      loop_vec_info loop_vinfo;

      loop = get_loop (cfun, i);
      if (!loop)
	continue;
      loop_vinfo = (loop_vec_info) loop->aux;
      destroy_loop_vec_info (loop_vinfo, true);
      loop->aux = NULL;
    }

  free_stmt_vec_info_vec ();

  if (num_vectorized_loops > 0)
    {
      /* If we vectorized any loop only virtual SSA form needs to be updated.
	 ???  Also while we try hard to update loop-closed SSA form we fail
	 to properly do this in some corner-cases (see PR56286).  */
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa_only_virtuals);
      return TODO_cleanup_cfg;
    }

  return 0;
}


/*  Entry point to basic block SLP phase.  */

static unsigned int
execute_vect_slp (void)
{
  basic_block bb;

  init_stmt_vec_info_vec ();

  FOR_EACH_BB (bb)
    {
      vect_location = find_bb_location (bb);

      if (vect_slp_analyze_bb (bb))
        {
          vect_slp_transform_bb (bb);
          if (dump_enabled_p ())
            dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
			     "Vectorized basic-block\n");
        }
    }

  free_stmt_vec_info_vec ();
  return 0;
}

static bool
gate_vect_slp (void)
{
  /* Apply SLP either if the vectorizer is on and the user didn't specify
     whether to run SLP or not, or if the SLP flag was set by the user.  */
  return ((flag_tree_vectorize != 0 && flag_tree_slp_vectorize != 0)
          || flag_tree_slp_vectorize == 1);
}

namespace {

const pass_data pass_data_slp_vectorize =
{
  GIMPLE_PASS, /* type */
  "slp", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_SLP_VECTORIZATION, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_verify_ssa | TODO_update_ssa
    | TODO_verify_stmts ), /* todo_flags_finish */
};

class pass_slp_vectorize : public gimple_opt_pass
{
public:
  pass_slp_vectorize(gcc::context *ctxt)
    : gimple_opt_pass(pass_data_slp_vectorize, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_vect_slp (); }
  unsigned int execute () { return execute_vect_slp (); }

}; // class pass_slp_vectorize

} // anon namespace

gimple_opt_pass *
make_pass_slp_vectorize (gcc::context *ctxt)
{
  return new pass_slp_vectorize (ctxt);
}


/* Increase alignment of global arrays to improve vectorization potential.
   TODO:
   - Consider also structs that have an array field.
   - Use ipa analysis to prune arrays that can't be vectorized?
     This should involve global alignment analysis and in the future also
     array padding.  */

static unsigned int
increase_alignment (void)
{
  struct varpool_node *vnode;

  vect_location = UNKNOWN_LOC;

  /* Increase the alignment of all global arrays for vectorization.  */
  FOR_EACH_DEFINED_VARIABLE (vnode)
    {
      tree vectype, decl = vnode->symbol.decl;
      tree t;
      unsigned int alignment;

      t = TREE_TYPE(decl);
      if (TREE_CODE (t) != ARRAY_TYPE)
        continue;
      vectype = get_vectype_for_scalar_type (strip_array_types (t));
      if (!vectype)
        continue;
      alignment = TYPE_ALIGN (vectype);
      if (DECL_ALIGN (decl) >= alignment)
        continue;

      if (vect_can_force_dr_alignment_p (decl, alignment))
        {
          DECL_ALIGN (decl) = TYPE_ALIGN (vectype);
          DECL_USER_ALIGN (decl) = 1;
          dump_printf (MSG_NOTE, "Increasing alignment of decl: ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM, decl);
          dump_printf (MSG_NOTE, "\n");
        }
    }
  return 0;
}


static bool
gate_increase_alignment (void)
{
  return flag_section_anchors && flag_tree_vectorize;
}


namespace {

const pass_data pass_data_ipa_increase_alignment =
{
  SIMPLE_IPA_PASS, /* type */
  "increase_alignment", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_IPA_OPT, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_increase_alignment : public simple_ipa_opt_pass
{
public:
  pass_ipa_increase_alignment(gcc::context *ctxt)
    : simple_ipa_opt_pass(pass_data_ipa_increase_alignment, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_increase_alignment (); }
  unsigned int execute () { return increase_alignment (); }

}; // class pass_ipa_increase_alignment

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_increase_alignment (gcc::context *ctxt)
{
  return new pass_ipa_increase_alignment (ctxt);
}
