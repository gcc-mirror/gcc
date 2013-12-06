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
#include "tree.h"
#include "stor-layout.h"
#include "tree-pretty-print.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssa-loop-manip.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "dbgcnt.h"

/* Loop or bb location.  */
source_location vect_location;

/* Vector mapping GIMPLE stmt to stmt_vec_info. */
vec<vec_void_p> stmt_vec_info_vec;

/* For mapping simduid to vectorization factor.  */

struct simduid_to_vf : typed_free_remove<simduid_to_vf>
{
  unsigned int simduid;
  int vf;

  /* hash_table support.  */
  typedef simduid_to_vf value_type;
  typedef simduid_to_vf compare_type;
  static inline hashval_t hash (const value_type *);
  static inline int equal (const value_type *, const compare_type *);
};

inline hashval_t
simduid_to_vf::hash (const value_type *p)
{
  return p->simduid;
}

inline int
simduid_to_vf::equal (const value_type *p1, const value_type *p2)
{
  return p1->simduid == p2->simduid;
}

/* This hash maps the OMP simd array to the corresponding simduid used
   to index into it.  Like thus,

        _7 = GOMP_SIMD_LANE (simduid.0)
        ...
        ...
        D.1737[_7] = stuff;


   This hash maps from the OMP simd array (D.1737[]) to DECL_UID of
   simduid.0.  */

struct simd_array_to_simduid : typed_free_remove<simd_array_to_simduid>
{
  tree decl;
  unsigned int simduid;

  /* hash_table support.  */
  typedef simd_array_to_simduid value_type;
  typedef simd_array_to_simduid compare_type;
  static inline hashval_t hash (const value_type *);
  static inline int equal (const value_type *, const compare_type *);
};

inline hashval_t
simd_array_to_simduid::hash (const value_type *p)
{
  return DECL_UID (p->decl);
}

inline int
simd_array_to_simduid::equal (const value_type *p1, const value_type *p2)
{
  return p1->decl == p2->decl;
}

/* Fold IFN_GOMP_SIMD_LANE, IFN_GOMP_SIMD_VF and IFN_GOMP_SIMD_LAST_LANE
   into their corresponding constants.  */

static void
adjust_simduid_builtins (hash_table <simduid_to_vf> &htab)
{
  basic_block bb;

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
	{
	  unsigned int vf = 1;
	  enum internal_fn ifn;
	  gimple stmt = gsi_stmt (i);
	  tree t;
	  if (!is_gimple_call (stmt)
	      || !gimple_call_internal_p (stmt))
	    continue;
	  ifn = gimple_call_internal_fn (stmt);
	  switch (ifn)
	    {
	    case IFN_GOMP_SIMD_LANE:
	    case IFN_GOMP_SIMD_VF:
	    case IFN_GOMP_SIMD_LAST_LANE:
	      break;
	    default:
	      continue;
	    }
	  tree arg = gimple_call_arg (stmt, 0);
	  gcc_assert (arg != NULL_TREE);
	  gcc_assert (TREE_CODE (arg) == SSA_NAME);
	  simduid_to_vf *p = NULL, data;
	  data.simduid = DECL_UID (SSA_NAME_VAR (arg));
	  if (htab.is_created ())
	    p = htab.find (&data);
	  if (p)
	    vf = p->vf;
	  switch (ifn)
	    {
	    case IFN_GOMP_SIMD_VF:
	      t = build_int_cst (unsigned_type_node, vf);
	      break;
	    case IFN_GOMP_SIMD_LANE:
	      t = build_int_cst (unsigned_type_node, 0);
	      break;
	    case IFN_GOMP_SIMD_LAST_LANE:
	      t = gimple_call_arg (stmt, 1);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  update_call_from_tree (&i, t);
	}
    }
}

/* Helper structure for note_simd_array_uses.  */

struct note_simd_array_uses_struct
{
  hash_table <simd_array_to_simduid> *htab;
  unsigned int simduid;
};

/* Callback for note_simd_array_uses, called through walk_gimple_op.  */

static tree
note_simd_array_uses_cb (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct note_simd_array_uses_struct *ns
    = (struct note_simd_array_uses_struct *) wi->info;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;
  else if (VAR_P (*tp)
	   && lookup_attribute ("omp simd array", DECL_ATTRIBUTES (*tp))
	   && DECL_CONTEXT (*tp) == current_function_decl)
    {
      simd_array_to_simduid data;
      if (!ns->htab->is_created ())
	ns->htab->create (15);
      data.decl = *tp;
      data.simduid = ns->simduid;
      simd_array_to_simduid **slot = ns->htab->find_slot (&data, INSERT);
      if (*slot == NULL)
	{
	  simd_array_to_simduid *p = XNEW (simd_array_to_simduid);
	  *p = data;
	  *slot = p;
	}
      else if ((*slot)->simduid != ns->simduid)
	(*slot)->simduid = -1U;
      *walk_subtrees = 0;
    }
  return NULL_TREE;
}

/* Find "omp simd array" temporaries and map them to corresponding
   simduid.  */

static void
note_simd_array_uses (hash_table <simd_array_to_simduid> *htab)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  struct walk_stmt_info wi;
  struct note_simd_array_uses_struct ns;

  memset (&wi, 0, sizeof (wi));
  wi.info = &ns;
  ns.htab = htab;

  FOR_EACH_BB (bb)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);
	if (!is_gimple_call (stmt) || !gimple_call_internal_p (stmt))
	  continue;
	switch (gimple_call_internal_fn (stmt))
	  {
	  case IFN_GOMP_SIMD_LANE:
	  case IFN_GOMP_SIMD_VF:
	  case IFN_GOMP_SIMD_LAST_LANE:
	    break;
	  default:
	    continue;
	  }
	tree lhs = gimple_call_lhs (stmt);
	if (lhs == NULL_TREE)
	  continue;
	imm_use_iterator use_iter;
	gimple use_stmt;
	ns.simduid = DECL_UID (SSA_NAME_VAR (gimple_call_arg (stmt, 0)));
	FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, lhs)
	  if (!is_gimple_debug (use_stmt))
	    walk_gimple_op (use_stmt, note_simd_array_uses_cb, &wi);
      }
}

/* A helper function to free data refs.  */

void
vect_destroy_datarefs (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo)
{
  vec<data_reference_p> datarefs;
  struct data_reference *dr;
  unsigned int i;

 if (loop_vinfo)
    datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  else
    datarefs = BB_VINFO_DATAREFS (bb_vinfo);

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    if (dr->aux)
      {
        free (dr->aux);
        dr->aux = NULL;
      }

  free_data_refs (datarefs);
}


/* Function vectorize_loops.

   Entry point to loop vectorization phase.  */

unsigned
vectorize_loops (void)
{
  unsigned int i;
  unsigned int num_vectorized_loops = 0;
  unsigned int vect_loops_num;
  struct loop *loop;
  hash_table <simduid_to_vf> simduid_to_vf_htab;
  hash_table <simd_array_to_simduid> simd_array_to_simduid_htab;

  vect_loops_num = number_of_loops (cfun);

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    {
      if (cfun->has_simduid_loops)
	adjust_simduid_builtins (simduid_to_vf_htab);
      return 0;
    }

  if (cfun->has_simduid_loops)
    note_simd_array_uses (&simd_array_to_simduid_htab);

  init_stmt_vec_info_vec ();

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number
     than all previously defined loops.  This fact allows us to run
     only over initial loops skipping newly generated ones.  */
  FOR_EACH_LOOP (loop, 0)
    if ((flag_tree_loop_vectorize && optimize_loop_nest_for_speed_p (loop))
	|| loop->force_vect)
      {
	loop_vec_info loop_vinfo;
	vect_location = find_loop_location (loop);
        if (LOCATION_LOCUS (vect_location) != UNKNOWN_LOCATION
	    && dump_enabled_p ())
	  dump_printf (MSG_NOTE, "\nAnalyzing loop at %s:%d\n",
                       LOCATION_FILE (vect_location),
		       LOCATION_LINE (vect_location));

	loop_vinfo = vect_analyze_loop (loop);
	loop->aux = loop_vinfo;

	if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
	  continue;

        if (!dbg_cnt (vect_loop))
	  break;

        if (LOCATION_LOCUS (vect_location) != UNKNOWN_LOCATION
	    && dump_enabled_p ())
          dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
                           "loop vectorized\n");
	vect_transform_loop (loop_vinfo);
	num_vectorized_loops++;
	/* Now that the loop has been vectorized, allow it to be unrolled
	   etc.  */
	loop->force_vect = false;

	if (loop->simduid)
	  {
	    simduid_to_vf *simduid_to_vf_data = XNEW (simduid_to_vf);
	    if (!simduid_to_vf_htab.is_created ())
	      simduid_to_vf_htab.create (15);
	    simduid_to_vf_data->simduid = DECL_UID (loop->simduid);
	    simduid_to_vf_data->vf = loop_vinfo->vectorization_factor;
	    *simduid_to_vf_htab.find_slot (simduid_to_vf_data, INSERT)
	      = simduid_to_vf_data;
	  }
      }

  vect_location = UNKNOWN_LOCATION;

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

  /* Fold IFN_GOMP_SIMD_{VF,LANE,LAST_LANE} builtins.  */
  if (cfun->has_simduid_loops)
    adjust_simduid_builtins (simduid_to_vf_htab);

  /* Shrink any "omp array simd" temporary arrays to the
     actual vectorization factors.  */
  if (simd_array_to_simduid_htab.is_created ())
    {
      for (hash_table <simd_array_to_simduid>::iterator iter
	   = simd_array_to_simduid_htab.begin ();
	   iter != simd_array_to_simduid_htab.end (); ++iter)
	if ((*iter).simduid != -1U)
	  {
	    tree decl = (*iter).decl;
	    int vf = 1;
	    if (simduid_to_vf_htab.is_created ())
	      {
		simduid_to_vf *p = NULL, data;
		data.simduid = (*iter).simduid;
		p = simduid_to_vf_htab.find (&data);
		if (p)
		  vf = p->vf;
	      }
	    tree atype
	      = build_array_type_nelts (TREE_TYPE (TREE_TYPE (decl)), vf);
	    TREE_TYPE (decl) = atype;
	    relayout_decl (decl);
	  }

      simd_array_to_simduid_htab.dispose ();
    }
  if (simduid_to_vf_htab.is_created ())
    simduid_to_vf_htab.dispose ();

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
          if (!dbg_cnt (vect_slp))
            break;

          vect_slp_transform_bb (bb);
          if (dump_enabled_p ())
            dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
			     "basic block vectorized\n");
        }
    }

  free_stmt_vec_info_vec ();
  return 0;
}

static bool
gate_vect_slp (void)
{
  return flag_tree_slp_vectorize != 0;
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
  pass_slp_vectorize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_slp_vectorize, ctxt)
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
  varpool_node *vnode;

  vect_location = UNKNOWN_LOCATION;

  /* Increase the alignment of all global arrays for vectorization.  */
  FOR_EACH_DEFINED_VARIABLE (vnode)
    {
      tree vectype, decl = vnode->decl;
      tree t;
      unsigned int alignment;

      t = TREE_TYPE (decl);
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
  return flag_section_anchors && flag_tree_loop_vectorize;
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
  pass_ipa_increase_alignment (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_increase_alignment, ctxt)
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
