/* Vectorizer
   Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "tree-ssa-propagate.h"
#include "dbgcnt.h"
#include "tree-scalar-evolution.h"
#include "stringpool.h"
#include "attribs.h"
#include "gimple-pretty-print.h"


/* Loop or bb location, with hotness information.  */
dump_user_location_t vect_location;

/* Vector mapping GIMPLE stmt to stmt_vec_info. */
vec<stmt_vec_info> *stmt_vec_info_vec;

/* Dump a cost entry according to args to F.  */

void
dump_stmt_cost (FILE *f, void *data, int count, enum vect_cost_for_stmt kind,
		stmt_vec_info stmt_info, int misalign,
		enum vect_cost_model_location where)
{
  fprintf (f, "%p ", data);
  if (stmt_info)
    {
      print_gimple_expr (f, STMT_VINFO_STMT (stmt_info), 0, TDF_SLIM);
      fprintf (f, " ");
    }
  else
    fprintf (f, "<unknown> ");
  fprintf (f, "%d times ", count);
  const char *ks = "unknown";
  switch (kind)
    {
    case scalar_stmt:
      ks = "scalar_stmt";
      break;
    case scalar_load:
      ks = "scalar_load";
      break;
    case scalar_store:
      ks = "scalar_store";
      break;
    case vector_stmt:
      ks = "vector_stmt";
      break;
    case vector_load:
      ks = "vector_load";
      break;
    case vector_gather_load:
      ks = "vector_gather_load";
      break;
    case unaligned_load:
      ks = "unaligned_load";
      break;
    case unaligned_store:
      ks = "unaligned_store";
      break;
    case vector_store:
      ks = "unaligned_store";
      break;
    case vector_scatter_store:
      ks = "unaligned_store";
      break;
    case vec_to_scalar:
      ks = "unaligned_store";
      break;
    case scalar_to_vec:
      ks = "unaligned_store";
      break;
    case cond_branch_not_taken:
      ks = "unaligned_store";
      break;
    case cond_branch_taken:
      ks = "unaligned_store";
      break;
    case vec_perm:
      ks = "unaligned_store";
      break;
    case vec_promote_demote:
      ks = "unaligned_store";
      break;
    case vec_construct:
      ks = "unaligned_store";
      break;
    }
  fprintf (f, "%s ", ks);
  if (kind == unaligned_load || kind == unaligned_store)
    fprintf (f, "(misalign %d) ", misalign);
  const char *ws = "unknown";
  switch (where)
    {
    case vect_prologue:
      ws = "prologue";
      break;
    case vect_body:
      ws = "body";
      break;
    case vect_epilogue:
      ws = "epilogue";
      break;
    }
  fprintf (f, "in %s\n", ws);
}

/* For mapping simduid to vectorization factor.  */

struct simduid_to_vf : free_ptr_hash<simduid_to_vf>
{
  unsigned int simduid;
  poly_uint64 vf;

  /* hash_table support.  */
  static inline hashval_t hash (const simduid_to_vf *);
  static inline int equal (const simduid_to_vf *, const simduid_to_vf *);
};

inline hashval_t
simduid_to_vf::hash (const simduid_to_vf *p)
{
  return p->simduid;
}

inline int
simduid_to_vf::equal (const simduid_to_vf *p1, const simduid_to_vf *p2)
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

struct simd_array_to_simduid : free_ptr_hash<simd_array_to_simduid>
{
  tree decl;
  unsigned int simduid;

  /* hash_table support.  */
  static inline hashval_t hash (const simd_array_to_simduid *);
  static inline int equal (const simd_array_to_simduid *,
			   const simd_array_to_simduid *);
};

inline hashval_t
simd_array_to_simduid::hash (const simd_array_to_simduid *p)
{
  return DECL_UID (p->decl);
}

inline int
simd_array_to_simduid::equal (const simd_array_to_simduid *p1,
			      const simd_array_to_simduid *p2)
{
  return p1->decl == p2->decl;
}

/* Fold IFN_GOMP_SIMD_LANE, IFN_GOMP_SIMD_VF, IFN_GOMP_SIMD_LAST_LANE,
   into their corresponding constants and remove
   IFN_GOMP_SIMD_ORDERED_{START,END}.  */

static void
adjust_simduid_builtins (hash_table<simduid_to_vf> *htab)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); )
	{
	  poly_uint64 vf = 1;
	  enum internal_fn ifn;
	  gimple *stmt = gsi_stmt (i);
	  tree t;
	  if (!is_gimple_call (stmt)
	      || !gimple_call_internal_p (stmt))
	    {
	      gsi_next (&i);
	      continue;
	    }
	  ifn = gimple_call_internal_fn (stmt);
	  switch (ifn)
	    {
	    case IFN_GOMP_SIMD_LANE:
	    case IFN_GOMP_SIMD_VF:
	    case IFN_GOMP_SIMD_LAST_LANE:
	      break;
	    case IFN_GOMP_SIMD_ORDERED_START:
	    case IFN_GOMP_SIMD_ORDERED_END:
	      if (integer_onep (gimple_call_arg (stmt, 0)))
		{
		  enum built_in_function bcode
		    = (ifn == IFN_GOMP_SIMD_ORDERED_START
		       ? BUILT_IN_GOMP_ORDERED_START
		       : BUILT_IN_GOMP_ORDERED_END);
		  gimple *g
		    = gimple_build_call (builtin_decl_explicit (bcode), 0);
		  tree vdef = gimple_vdef (stmt);
		  gimple_set_vdef (g, vdef);
		  SSA_NAME_DEF_STMT (vdef) = g;
		  gimple_set_vuse (g, gimple_vuse (stmt));
		  gsi_replace (&i, g, true);
		  continue;
		}
	      gsi_remove (&i, true);
	      unlink_stmt_vdef (stmt);
	      continue;
	    default:
	      gsi_next (&i);
	      continue;
	    }
	  tree arg = gimple_call_arg (stmt, 0);
	  gcc_assert (arg != NULL_TREE);
	  gcc_assert (TREE_CODE (arg) == SSA_NAME);
	  simduid_to_vf *p = NULL, data;
	  data.simduid = DECL_UID (SSA_NAME_VAR (arg));
	  /* Need to nullify loop safelen field since it's value is not
	     valid after transformation.  */
	  if (bb->loop_father && bb->loop_father->safelen > 0)
	    bb->loop_father->safelen = 0;
	  if (htab)
	    {
	      p = htab->find (&data);
	      if (p)
		vf = p->vf;
	    }
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
	  tree lhs = gimple_call_lhs (stmt);
	  if (lhs)
	    replace_uses_by (lhs, t);
	  release_defs (stmt);
	  gsi_remove (&i, true);
	}
    }
}

/* Helper structure for note_simd_array_uses.  */

struct note_simd_array_uses_struct
{
  hash_table<simd_array_to_simduid> **htab;
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
      if (!*ns->htab)
	*ns->htab = new hash_table<simd_array_to_simduid> (15);
      data.decl = *tp;
      data.simduid = ns->simduid;
      simd_array_to_simduid **slot = (*ns->htab)->find_slot (&data, INSERT);
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
note_simd_array_uses (hash_table<simd_array_to_simduid> **htab)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  struct walk_stmt_info wi;
  struct note_simd_array_uses_struct ns;

  memset (&wi, 0, sizeof (wi));
  wi.info = &ns;
  ns.htab = htab;

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
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
	gimple *use_stmt;
	ns.simduid = DECL_UID (SSA_NAME_VAR (gimple_call_arg (stmt, 0)));
	FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, lhs)
	  if (!is_gimple_debug (use_stmt))
	    walk_gimple_op (use_stmt, note_simd_array_uses_cb, &wi);
      }
}

/* Shrink arrays with "omp simd array" attribute to the corresponding
   vectorization factor.  */

static void
shrink_simd_arrays
  (hash_table<simd_array_to_simduid> *simd_array_to_simduid_htab,
   hash_table<simduid_to_vf> *simduid_to_vf_htab)
{
  for (hash_table<simd_array_to_simduid>::iterator iter
	 = simd_array_to_simduid_htab->begin ();
       iter != simd_array_to_simduid_htab->end (); ++iter)
    if ((*iter)->simduid != -1U)
      {
	tree decl = (*iter)->decl;
	poly_uint64 vf = 1;
	if (simduid_to_vf_htab)
	  {
	    simduid_to_vf *p = NULL, data;
	    data.simduid = (*iter)->simduid;
	    p = simduid_to_vf_htab->find (&data);
	    if (p)
	      vf = p->vf;
	  }
	tree atype
	  = build_array_type_nelts (TREE_TYPE (TREE_TYPE (decl)), vf);
	TREE_TYPE (decl) = atype;
	relayout_decl (decl);
      }

  delete simd_array_to_simduid_htab;
}

/* Initialize the vec_info with kind KIND_IN and target cost data
   TARGET_COST_DATA_IN.  */

vec_info::vec_info (vec_info::vec_kind kind_in, void *target_cost_data_in,
		    vec_info_shared *shared_)
  : kind (kind_in),
    shared (shared_),
    target_cost_data (target_cost_data_in)
{
  stmt_vec_infos.create (50);
  set_stmt_vec_info_vec (&stmt_vec_infos);
}

vec_info::~vec_info ()
{
  slp_instance instance;
  unsigned int i;

  FOR_EACH_VEC_ELT (slp_instances, i, instance)
    vect_free_slp_instance (instance);

  destroy_cost_data (target_cost_data);
  free_stmt_vec_infos (&stmt_vec_infos);
}

vec_info_shared::vec_info_shared ()
  : datarefs (vNULL),
    datarefs_copy (vNULL),
    ddrs (vNULL)
{
}

vec_info_shared::~vec_info_shared ()
{
  free_data_refs (datarefs);
  free_dependence_relations (ddrs);
  datarefs_copy.release ();
}

void
vec_info_shared::save_datarefs ()
{
  if (!flag_checking)
    return;
  datarefs_copy.reserve_exact (datarefs.length ());
  for (unsigned i = 0; i < datarefs.length (); ++i)
    datarefs_copy.quick_push (*datarefs[i]);
}

void
vec_info_shared::check_datarefs ()
{
  if (!flag_checking)
    return;
  gcc_assert (datarefs.length () == datarefs_copy.length ());
  for (unsigned i = 0; i < datarefs.length (); ++i)
    if (memcmp (&datarefs_copy[i], datarefs[i], sizeof (data_reference)) != 0)
      gcc_unreachable ();
}

/* A helper function to free scev and LOOP niter information, as well as
   clear loop constraint LOOP_C_FINITE.  */

void
vect_free_loop_info_assumptions (struct loop *loop)
{
  scev_reset_htab ();
  /* We need to explicitly reset upper bound information since they are
     used even after free_numbers_of_iterations_estimates.  */
  loop->any_upper_bound = false;
  loop->any_likely_upper_bound = false;
  free_numbers_of_iterations_estimates (loop);
  loop_constraint_clear (loop, LOOP_C_FINITE);
}

/* Return whether STMT is inside the region we try to vectorize.  */

bool
vect_stmt_in_region_p (vec_info *vinfo, gimple *stmt)
{
  if (!gimple_bb (stmt))
    return false;

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    {
      struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
      if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
	return false;
    }
  else
    {
      bb_vec_info bb_vinfo = as_a <bb_vec_info> (vinfo);
      if (gimple_bb (stmt) != BB_VINFO_BB (bb_vinfo)
	  || gimple_uid (stmt) == -1U
	  || gimple_code (stmt) == GIMPLE_PHI)
	return false;
    }

  return true;
}


/* If LOOP has been versioned during ifcvt, return the internal call
   guarding it.  */

static gimple *
vect_loop_vectorized_call (struct loop *loop)
{
  basic_block bb = loop_preheader_edge (loop)->src;
  gimple *g;
  do
    {
      g = last_stmt (bb);
      if (g)
	break;
      if (!single_pred_p (bb))
	break;
      bb = single_pred (bb);
    }
  while (1);
  if (g && gimple_code (g) == GIMPLE_COND)
    {
      gimple_stmt_iterator gsi = gsi_for_stmt (g);
      gsi_prev (&gsi);
      if (!gsi_end_p (gsi))
	{
	  g = gsi_stmt (gsi);
	  if (gimple_call_internal_p (g, IFN_LOOP_VECTORIZED)
	      && (tree_to_shwi (gimple_call_arg (g, 0)) == loop->num
		  || tree_to_shwi (gimple_call_arg (g, 1)) == loop->num))
	    return g;
	}
    }
  return NULL;
}

/* If LOOP has been versioned during loop distribution, return the gurading
   internal call.  */

static gimple *
vect_loop_dist_alias_call (struct loop *loop)
{
  basic_block bb;
  basic_block entry;
  struct loop *outer, *orig;
  gimple_stmt_iterator gsi;
  gimple *g;

  if (loop->orig_loop_num == 0)
    return NULL;

  orig = get_loop (cfun, loop->orig_loop_num);
  if (orig == NULL)
    {
      /* The original loop is somehow destroyed.  Clear the information.  */
      loop->orig_loop_num = 0;
      return NULL;
    }

  if (loop != orig)
    bb = nearest_common_dominator (CDI_DOMINATORS, loop->header, orig->header);
  else
    bb = loop_preheader_edge (loop)->src;

  outer = bb->loop_father;
  entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);

  /* Look upward in dominance tree.  */
  for (; bb != entry && flow_bb_inside_loop_p (outer, bb);
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      g = last_stmt (bb);
      if (g == NULL || gimple_code (g) != GIMPLE_COND)
	continue;

      gsi = gsi_for_stmt (g);
      gsi_prev (&gsi);
      if (gsi_end_p (gsi))
	continue;

      g = gsi_stmt (gsi);
      /* The guarding internal function call must have the same distribution
	 alias id.  */
      if (gimple_call_internal_p (g, IFN_LOOP_DIST_ALIAS)
	  && (tree_to_shwi (gimple_call_arg (g, 0)) == loop->orig_loop_num))
	return g;
    }
  return NULL;
}

/* Set the uids of all the statements in basic blocks inside loop
   represented by LOOP_VINFO. LOOP_VECTORIZED_CALL is the internal
   call guarding the loop which has been if converted.  */
static void
set_uid_loop_bbs (loop_vec_info loop_vinfo, gimple *loop_vectorized_call)
{
  tree arg = gimple_call_arg (loop_vectorized_call, 1);
  basic_block *bbs;
  unsigned int i;
  struct loop *scalar_loop = get_loop (cfun, tree_to_shwi (arg));

  LOOP_VINFO_SCALAR_LOOP (loop_vinfo) = scalar_loop;
  gcc_checking_assert (vect_loop_vectorized_call (scalar_loop)
		       == loop_vectorized_call);
  /* If we are going to vectorize outer loop, prevent vectorization
     of the inner loop in the scalar loop - either the scalar loop is
     thrown away, so it is a wasted work, or is used only for
     a few iterations.  */
  if (scalar_loop->inner)
    {
      gimple *g = vect_loop_vectorized_call (scalar_loop->inner);
      if (g)
	{
	  arg = gimple_call_arg (g, 0);
	  get_loop (cfun, tree_to_shwi (arg))->dont_vectorize = true;
	  fold_loop_internal_call (g, boolean_false_node);
	}
    }
  bbs = get_loop_body (scalar_loop);
  for (i = 0; i < scalar_loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *phi = gsi_stmt (gsi);
	  gimple_set_uid (phi, 0);
	}
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, 0);
	}
    }
  free (bbs);
}

/* Try to vectorize LOOP.  */

static unsigned
try_vectorize_loop_1 (hash_table<simduid_to_vf> *&simduid_to_vf_htab,
		      unsigned *num_vectorized_loops,
		      loop_p loop, loop_vec_info orig_loop_vinfo,
		      gimple *loop_vectorized_call,
		      gimple *loop_dist_alias_call)
{
  unsigned ret = 0;
  vec_info_shared shared;
  vect_location = find_loop_location (loop);
  if (LOCATION_LOCUS (vect_location.get_location_t ()) != UNKNOWN_LOCATION
      && dump_enabled_p ())
    dump_printf (MSG_NOTE, "\nAnalyzing loop at %s:%d\n",
		 LOCATION_FILE (vect_location.get_location_t ()),
		 LOCATION_LINE (vect_location.get_location_t ()));

  loop_vec_info loop_vinfo = vect_analyze_loop (loop, orig_loop_vinfo, &shared);
  loop->aux = loop_vinfo;

  if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
    {
      /* Free existing information if loop is analyzed with some
	 assumptions.  */
      if (loop_constraint_set_p (loop, LOOP_C_FINITE))
	vect_free_loop_info_assumptions (loop);

      /* If we applied if-conversion then try to vectorize the
	 BB of innermost loops.
	 ???  Ideally BB vectorization would learn to vectorize
	 control flow by applying if-conversion on-the-fly, the
	 following retains the if-converted loop body even when
	 only non-if-converted parts took part in BB vectorization.  */
      if (flag_tree_slp_vectorize != 0
	  && loop_vectorized_call
	  && ! loop->inner)
	{
	  basic_block bb = loop->header;
	  bool has_mask_load_store = false;
	  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	       !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (is_gimple_call (stmt)
		  && gimple_call_internal_p (stmt)
		  && (gimple_call_internal_fn (stmt) == IFN_MASK_LOAD
		      || gimple_call_internal_fn (stmt) == IFN_MASK_STORE))
		{
		  has_mask_load_store = true;
		  break;
		}
	      gimple_set_uid (stmt, -1);
	      gimple_set_visited (stmt, false);
	    }
	  if (! has_mask_load_store && vect_slp_bb (bb))
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "basic block vectorized\n");
	      fold_loop_internal_call (loop_vectorized_call,
				       boolean_true_node);
	      loop_vectorized_call = NULL;
	      ret |= TODO_cleanup_cfg;
	    }
	}
      /* If outer loop vectorization fails for LOOP_VECTORIZED guarded
	 loop, don't vectorize its inner loop; we'll attempt to
	 vectorize LOOP_VECTORIZED guarded inner loop of the scalar
	 loop version.  */
      if (loop_vectorized_call && loop->inner)
	loop->inner->dont_vectorize = true;
      return ret;
    }

  if (!dbg_cnt (vect_loop))
    {
      /* Free existing information if loop is analyzed with some
	 assumptions.  */
      if (loop_constraint_set_p (loop, LOOP_C_FINITE))
	vect_free_loop_info_assumptions (loop);
      return ret;
    }

  if (loop_vectorized_call)
    set_uid_loop_bbs (loop_vinfo, loop_vectorized_call);

  unsigned HOST_WIDE_INT bytes;
  if (current_vector_size.is_constant (&bytes))
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
		     "loop vectorized vectorized using "
		     HOST_WIDE_INT_PRINT_UNSIGNED " byte "
		     "vectors\n", bytes);
  else
    dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
		     "loop vectorized using variable length vectors\n");

  loop_p new_loop = vect_transform_loop (loop_vinfo);
  (*num_vectorized_loops)++;
  /* Now that the loop has been vectorized, allow it to be unrolled
     etc.  */
  loop->force_vectorize = false;

  if (loop->simduid)
    {
      simduid_to_vf *simduid_to_vf_data = XNEW (simduid_to_vf);
      if (!simduid_to_vf_htab)
	simduid_to_vf_htab = new hash_table<simduid_to_vf> (15);
      simduid_to_vf_data->simduid = DECL_UID (loop->simduid);
      simduid_to_vf_data->vf = loop_vinfo->vectorization_factor;
      *simduid_to_vf_htab->find_slot (simduid_to_vf_data, INSERT)
	  = simduid_to_vf_data;
    }

  if (loop_vectorized_call)
    {
      fold_loop_internal_call (loop_vectorized_call, boolean_true_node);
      loop_vectorized_call = NULL;
      ret |= TODO_cleanup_cfg;
    }
  if (loop_dist_alias_call)
    {
      tree value = gimple_call_arg (loop_dist_alias_call, 1);
      fold_loop_internal_call (loop_dist_alias_call, value);
      loop_dist_alias_call = NULL;
      ret |= TODO_cleanup_cfg;
    }

  /* Epilogue of vectorized loop must be vectorized too.  */
  if (new_loop)
    ret |= try_vectorize_loop_1 (simduid_to_vf_htab, num_vectorized_loops,
				 new_loop, loop_vinfo, NULL, NULL);

  return ret;
}

/* Try to vectorize LOOP.  */

static unsigned
try_vectorize_loop (hash_table<simduid_to_vf> *&simduid_to_vf_htab,
		    unsigned *num_vectorized_loops, loop_p loop)
{
  if (!((flag_tree_loop_vectorize
	 && optimize_loop_nest_for_speed_p (loop))
	|| loop->force_vectorize))
    return 0;

  return try_vectorize_loop_1 (simduid_to_vf_htab, num_vectorized_loops,
			       loop, NULL,
			       vect_loop_vectorized_call (loop),
			       vect_loop_dist_alias_call (loop));
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
  hash_table<simduid_to_vf> *simduid_to_vf_htab = NULL;
  hash_table<simd_array_to_simduid> *simd_array_to_simduid_htab = NULL;
  bool any_ifcvt_loops = false;
  unsigned ret = 0;

  vect_loops_num = number_of_loops (cfun);

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    return 0;

  if (cfun->has_simduid_loops)
    note_simd_array_uses (&simd_array_to_simduid_htab);

  set_stmt_vec_info_vec (NULL);

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number
     than all previously defined loops.  This fact allows us to run
     only over initial loops skipping newly generated ones.  */
  FOR_EACH_LOOP (loop, 0)
    if (loop->dont_vectorize)
      {
	any_ifcvt_loops = true;
	/* If-conversion sometimes versions both the outer loop
	   (for the case when outer loop vectorization might be
	   desirable) as well as the inner loop in the scalar version
	   of the loop.  So we have:
	    if (LOOP_VECTORIZED (1, 3))
	      {
		loop1
		  loop2
	      }
	    else
	      loop3 (copy of loop1)
		if (LOOP_VECTORIZED (4, 5))
		  loop4 (copy of loop2)
		else
		  loop5 (copy of loop4)
	   If FOR_EACH_LOOP gives us loop3 first (which has
	   dont_vectorize set), make sure to process loop1 before loop4;
	   so that we can prevent vectorization of loop4 if loop1
	   is successfully vectorized.  */
	if (loop->inner)
	  {
	    gimple *loop_vectorized_call
	      = vect_loop_vectorized_call (loop);
	    if (loop_vectorized_call
		&& vect_loop_vectorized_call (loop->inner))
	      {
		tree arg = gimple_call_arg (loop_vectorized_call, 0);
		struct loop *vector_loop
		  = get_loop (cfun, tree_to_shwi (arg));
		if (vector_loop && vector_loop != loop)
		  {
		    /* Make sure we don't vectorize it twice.  */
		    vector_loop->dont_vectorize = true;
		    ret |= try_vectorize_loop (simduid_to_vf_htab,
					       &num_vectorized_loops,
					       vector_loop);
		  }
	      }
	  }
      }
    else
      ret |= try_vectorize_loop (simduid_to_vf_htab, &num_vectorized_loops,
				 loop);

  vect_location = dump_user_location_t ();

  statistics_counter_event (cfun, "Vectorized loops", num_vectorized_loops);
  if (dump_enabled_p ()
      || (num_vectorized_loops > 0 && dump_enabled_p ()))
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vectorized %u loops in function.\n",
                     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  if (any_ifcvt_loops)
    for (i = 1; i < number_of_loops (cfun); i++)
      {
	loop = get_loop (cfun, i);
	if (loop && loop->dont_vectorize)
	  {
	    gimple *g = vect_loop_vectorized_call (loop);
	    if (g)
	      {
		fold_loop_internal_call (g, boolean_false_node);
		ret |= TODO_cleanup_cfg;
		g = NULL;
	      }
	    else
	      g = vect_loop_dist_alias_call (loop);

	    if (g)
	      {
		fold_loop_internal_call (g, boolean_false_node);
		ret |= TODO_cleanup_cfg;
	      }
	  }
      }

  for (i = 1; i < number_of_loops (cfun); i++)
    {
      loop_vec_info loop_vinfo;
      bool has_mask_store;

      loop = get_loop (cfun, i);
      if (!loop || !loop->aux)
	continue;
      loop_vinfo = (loop_vec_info) loop->aux;
      has_mask_store = LOOP_VINFO_HAS_MASK_STORE (loop_vinfo);
      delete loop_vinfo;
      if (has_mask_store
	  && targetm.vectorize.empty_mask_is_expensive (IFN_MASK_STORE))
	optimize_mask_stores (loop);
      loop->aux = NULL;
    }

  /* Fold IFN_GOMP_SIMD_{VF,LANE,LAST_LANE,ORDERED_{START,END}} builtins.  */
  if (cfun->has_simduid_loops)
    adjust_simduid_builtins (simduid_to_vf_htab);

  /* Shrink any "omp array simd" temporary arrays to the
     actual vectorization factors.  */
  if (simd_array_to_simduid_htab)
    shrink_simd_arrays (simd_array_to_simduid_htab, simduid_to_vf_htab);
  delete simduid_to_vf_htab;
  cfun->has_simduid_loops = false;

  if (num_vectorized_loops > 0)
    {
      /* If we vectorized any loop only virtual SSA form needs to be updated.
	 ???  Also while we try hard to update loop-closed SSA form we fail
	 to properly do this in some corner-cases (see PR56286).  */
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa_only_virtuals);
      return TODO_cleanup_cfg;
    }

  return ret;
}


/* Entry point to the simduid cleanup pass.  */

namespace {

const pass_data pass_data_simduid_cleanup =
{
  GIMPLE_PASS, /* type */
  "simduid", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_simduid_cleanup : public gimple_opt_pass
{
public:
  pass_simduid_cleanup (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_simduid_cleanup, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_simduid_cleanup (m_ctxt); }
  virtual bool gate (function *fun) { return fun->has_simduid_loops; }
  virtual unsigned int execute (function *);

}; // class pass_simduid_cleanup

unsigned int
pass_simduid_cleanup::execute (function *fun)
{
  hash_table<simd_array_to_simduid> *simd_array_to_simduid_htab = NULL;

  note_simd_array_uses (&simd_array_to_simduid_htab);

  /* Fold IFN_GOMP_SIMD_{VF,LANE,LAST_LANE,ORDERED_{START,END}} builtins.  */
  adjust_simduid_builtins (NULL);

  /* Shrink any "omp array simd" temporary arrays to the
     actual vectorization factors.  */
  if (simd_array_to_simduid_htab)
    shrink_simd_arrays (simd_array_to_simduid_htab, NULL);
  fun->has_simduid_loops = false;
  return 0;
}

}  // anon namespace

gimple_opt_pass *
make_pass_simduid_cleanup (gcc::context *ctxt)
{
  return new pass_simduid_cleanup (ctxt);
}


/*  Entry point to basic block SLP phase.  */

namespace {

const pass_data pass_data_slp_vectorize =
{
  GIMPLE_PASS, /* type */
  "slp", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
  TV_TREE_SLP_VECTORIZATION, /* tv_id */
  ( PROP_ssa | PROP_cfg ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_slp_vectorize : public gimple_opt_pass
{
public:
  pass_slp_vectorize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_slp_vectorize, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_slp_vectorize (m_ctxt); }
  virtual bool gate (function *) { return flag_tree_slp_vectorize != 0; }
  virtual unsigned int execute (function *);

}; // class pass_slp_vectorize

unsigned int
pass_slp_vectorize::execute (function *fun)
{
  basic_block bb;

  bool in_loop_pipeline = scev_initialized_p ();
  if (!in_loop_pipeline)
    {
      loop_optimizer_init (LOOPS_NORMAL);
      scev_initialize ();
    }

  /* Mark all stmts as not belonging to the current region and unvisited.  */
  FOR_EACH_BB_FN (bb, fun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, -1);
	  gimple_set_visited (stmt, false);
	}
    }

  FOR_EACH_BB_FN (bb, fun)
    {
      if (vect_slp_bb (bb))
	dump_printf_loc (MSG_NOTE, vect_location, "basic block vectorized\n");
    }

  if (!in_loop_pipeline)
    {
      scev_finalize ();
      loop_optimizer_finalize ();
    }

  return 0;
}

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

static unsigned get_vec_alignment_for_type (tree);
static hash_map<tree, unsigned> *type_align_map;

/* Return alignment of array's vector type corresponding to scalar type.
   0 if no vector type exists.  */
static unsigned
get_vec_alignment_for_array_type (tree type) 
{
  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
  poly_uint64 array_size, vector_size;

  tree vectype = get_vectype_for_scalar_type (strip_array_types (type));
  if (!vectype
      || !poly_int_tree_p (TYPE_SIZE (type), &array_size)
      || !poly_int_tree_p (TYPE_SIZE (vectype), &vector_size)
      || maybe_lt (array_size, vector_size))
    return 0;

  return TYPE_ALIGN (vectype);
}

/* Return alignment of field having maximum alignment of vector type
   corresponding to it's scalar type. For now, we only consider fields whose
   offset is a multiple of it's vector alignment.
   0 if no suitable field is found.  */
static unsigned
get_vec_alignment_for_record_type (tree type) 
{
  gcc_assert (TREE_CODE (type) == RECORD_TYPE);

  unsigned max_align = 0, alignment;
  HOST_WIDE_INT offset;
  tree offset_tree;

  if (TYPE_PACKED (type))
    return 0;

  unsigned *slot = type_align_map->get (type);
  if (slot)
    return *slot;

  for (tree field = first_field (type);
       field != NULL_TREE;
       field = DECL_CHAIN (field))
    {
      /* Skip if not FIELD_DECL or if alignment is set by user.  */ 
      if (TREE_CODE (field) != FIELD_DECL
	  || DECL_USER_ALIGN (field)
	  || DECL_ARTIFICIAL (field))
	continue;

      /* We don't need to process the type further if offset is variable,
	 since the offsets of remaining members will also be variable.  */
      if (TREE_CODE (DECL_FIELD_OFFSET (field)) != INTEGER_CST
	  || TREE_CODE (DECL_FIELD_BIT_OFFSET (field)) != INTEGER_CST)
	break;

      /* Similarly stop processing the type if offset_tree
	 does not fit in unsigned HOST_WIDE_INT.  */
      offset_tree = bit_position (field);
      if (!tree_fits_uhwi_p (offset_tree))
	break;

      offset = tree_to_uhwi (offset_tree); 
      alignment = get_vec_alignment_for_type (TREE_TYPE (field));

      /* Get maximum alignment of vectorized field/array among those members
	 whose offset is multiple of the vector alignment.  */ 
      if (alignment
	  && (offset % alignment == 0)
	  && (alignment > max_align))
	max_align = alignment;
    }

  type_align_map->put (type, max_align);
  return max_align;
}

/* Return alignment of vector type corresponding to decl's scalar type
   or 0 if it doesn't exist or the vector alignment is lesser than
   decl's alignment.  */
static unsigned
get_vec_alignment_for_type (tree type)
{
  if (type == NULL_TREE)
    return 0;

  gcc_assert (TYPE_P (type));

  static unsigned alignment = 0;
  switch (TREE_CODE (type))
    {
      case ARRAY_TYPE:
	alignment = get_vec_alignment_for_array_type (type);
	break;
      case RECORD_TYPE:
	alignment = get_vec_alignment_for_record_type (type);
	break;
      default:
	alignment = 0;
	break;
    }

  return (alignment > TYPE_ALIGN (type)) ? alignment : 0;
}

/* Entry point to increase_alignment pass.  */
static unsigned int
increase_alignment (void)
{
  varpool_node *vnode;

  vect_location = dump_user_location_t ();
  type_align_map = new hash_map<tree, unsigned>;

  /* Increase the alignment of all global arrays for vectorization.  */
  FOR_EACH_DEFINED_VARIABLE (vnode)
    {
      tree decl = vnode->decl;
      unsigned int alignment;

      if ((decl_in_symtab_p (decl)
	  && !symtab_node::get (decl)->can_increase_alignment_p ())
	  || DECL_USER_ALIGN (decl) || DECL_ARTIFICIAL (decl))
	continue;

      alignment = get_vec_alignment_for_type (TREE_TYPE (decl));
      if (alignment && vect_can_force_dr_alignment_p (decl, alignment))
        {
	  vnode->increase_alignment (alignment);
          dump_printf (MSG_NOTE, "Increasing alignment of decl: ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM, decl);
          dump_printf (MSG_NOTE, "\n");
        }
    }

  delete type_align_map;
  return 0;
}


namespace {

const pass_data pass_data_ipa_increase_alignment =
{
  SIMPLE_IPA_PASS, /* type */
  "increase_alignment", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
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
  virtual bool gate (function *)
    {
      return flag_section_anchors && flag_tree_loop_vectorize;
    }

  virtual unsigned int execute (function *) { return increase_alignment (); }

}; // class pass_ipa_increase_alignment

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_increase_alignment (gcc::context *ctxt)
{
  return new pass_ipa_increase_alignment (ctxt);
}
