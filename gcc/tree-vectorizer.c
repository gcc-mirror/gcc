/* Vectorizer
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009 Free Software
   Foundation, Inc.
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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "tree-vectorizer.h"
#include "tree-pass.h"

/* vect_dump will be set to stderr or dump_file if exist.  */
FILE *vect_dump;

/* vect_verbosity_level set to an invalid value 
   to mark that it's uninitialized.  */
enum verbosity_levels vect_verbosity_level = MAX_VERBOSITY_LEVEL;

/* Loop location.  */
LOC vect_loop_location;

/* Bitmap of virtual variables to be renamed.  */
bitmap vect_memsyms_to_rename;

/* Vector mapping GIMPLE stmt to stmt_vec_info. */
VEC(vec_void_p,heap) *stmt_vec_info_vec;



/* Function vect_set_verbosity_level.

   Called from toplev.c upon detection of the
   -ftree-vectorizer-verbose=N option.  */

void
vect_set_verbosity_level (const char *val)
{
   unsigned int vl;

   vl = atoi (val);
   if (vl < MAX_VERBOSITY_LEVEL)
     vect_verbosity_level = vl;
   else
     vect_verbosity_level = MAX_VERBOSITY_LEVEL - 1;
}


/* Function vect_set_dump_settings.

   Fix the verbosity level of the vectorizer if the
   requested level was not set explicitly using the flag
   -ftree-vectorizer-verbose=N.
   Decide where to print the debugging information (dump_file/stderr).
   If the user defined the verbosity level, but there is no dump file,
   print to stderr, otherwise print to the dump file.  */

static void
vect_set_dump_settings (void)
{
  vect_dump = dump_file;

  /* Check if the verbosity level was defined by the user:  */
  if (vect_verbosity_level != MAX_VERBOSITY_LEVEL)
    {
      /* If there is no dump file, print to stderr.  */
      if (!dump_file)
        vect_dump = stderr;
      return;
    }

  /* User didn't specify verbosity level:  */
  if (dump_file && (dump_flags & TDF_DETAILS))
    vect_verbosity_level = REPORT_DETAILS;
  else if (dump_file && (dump_flags & TDF_STATS))
    vect_verbosity_level = REPORT_UNVECTORIZED_LOOPS;
  else
    vect_verbosity_level = REPORT_NONE;

  gcc_assert (dump_file || vect_verbosity_level == REPORT_NONE);
}


/* Function debug_loop_details.

   For vectorization debug dumps.  */

bool
vect_print_dump_info (enum verbosity_levels vl)
{
  if (vl > vect_verbosity_level)
    return false;

  if (!current_function_decl || !vect_dump)
    return false;

  if (vect_loop_location == UNKNOWN_LOC)
    fprintf (vect_dump, "\n%s:%d: note: ",
	     DECL_SOURCE_FILE (current_function_decl),
	     DECL_SOURCE_LINE (current_function_decl));
  else
    fprintf (vect_dump, "\n%s:%d: note: ", 
	     LOC_FILE (vect_loop_location), LOC_LINE (vect_loop_location));

  return true;
}


/* Function vectorize_loops.
   
   Entry Point to loop vectorization phase.  */

unsigned
vectorize_loops (void)
{
  unsigned int i;
  unsigned int num_vectorized_loops = 0;
  unsigned int vect_loops_num;
  loop_iterator li;
  struct loop *loop;

  vect_loops_num = number_of_loops ();

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    return 0;

  /* Fix the verbosity level if not defined explicitly by the user.  */
  vect_set_dump_settings ();

  /* Allocate the bitmap that records which virtual variables that 
     need to be renamed.  */
  vect_memsyms_to_rename = BITMAP_ALLOC (NULL);

  init_stmt_vec_info_vec ();

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number 
     than all previously defined loops. This fact allows us to run 
     only over initial loops skipping newly generated ones.  */
  FOR_EACH_LOOP (li, loop, 0)
    if (optimize_loop_nest_for_speed_p (loop))
      {
	loop_vec_info loop_vinfo;

	vect_loop_location = find_loop_location (loop);
	loop_vinfo = vect_analyze_loop (loop);
	loop->aux = loop_vinfo;

	if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
	  continue;

	vect_transform_loop (loop_vinfo);
	num_vectorized_loops++;
      }
  vect_loop_location = UNKNOWN_LOC;

  statistics_counter_event (cfun, "Vectorized loops", num_vectorized_loops);
  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOOPS)
      || (vect_print_dump_info (REPORT_VECTORIZED_LOOPS)
	  && num_vectorized_loops > 0))
    fprintf (vect_dump, "vectorized %u loops in function.\n",
	     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  BITMAP_FREE (vect_memsyms_to_rename);

  for (i = 1; i < vect_loops_num; i++)
    {
      loop_vec_info loop_vinfo;

      loop = get_loop (i);
      if (!loop)
	continue;
      loop_vinfo = (loop_vec_info) loop->aux;
      destroy_loop_vec_info (loop_vinfo, true);
      loop->aux = NULL;
    }

  free_stmt_vec_info_vec ();

  return num_vectorized_loops > 0 ? TODO_cleanup_cfg : 0;
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

  /* Increase the alignment of all global arrays for vectorization.  */
  for (vnode = varpool_nodes_queue;
       vnode;
       vnode = vnode->next_needed)
    {
      tree vectype, decl = vnode->decl;
      unsigned int alignment;

      if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
        continue;
      vectype = get_vectype_for_scalar_type (TREE_TYPE (TREE_TYPE (decl)));
      if (!vectype)
        continue;
      alignment = TYPE_ALIGN (vectype);
      if (DECL_ALIGN (decl) >= alignment)
        continue;

      if (vect_can_force_dr_alignment_p (decl, alignment))
        {
          DECL_ALIGN (decl) = TYPE_ALIGN (vectype);
          DECL_USER_ALIGN (decl) = 1;
          if (dump_file)
            {
              fprintf (dump_file, "Increasing alignment of decl: ");
              print_generic_expr (dump_file, decl, TDF_SLIM);
            }
        }
    }
  return 0;
}


static bool
gate_increase_alignment (void)
{
  return flag_section_anchors && flag_tree_vectorize;
}


struct simple_ipa_opt_pass pass_ipa_increase_alignment =
{
 {
  SIMPLE_IPA_PASS,
  "increase_alignment",                 /* name */
  gate_increase_alignment,              /* gate */
  increase_alignment,                   /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_NONE,                              /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  0                                     /* todo_flags_finish */
 }
};
