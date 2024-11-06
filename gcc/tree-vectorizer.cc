/* Vectorizer
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
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
  - tree-vect-loop.cc - loop specific parts such as reductions, etc. These are
    used by drivers (1) and (2).
  - tree-vect-loop-manip.cc - vectorizer's loop control-flow utilities, used by
    drivers (1) and (2).
  - tree-vect-slp.cc - BB vectorization specific analysis and transformation,
    used by drivers (2) and (3).
  - tree-vect-stmts.cc - statements analysis and transformation (used by all).
  - tree-vect-data-refs.cc - vectorizer specific data-refs analysis and
    manipulations (used by all).
  - tree-vect-patterns.cc - vectorizable code patterns detector (used by all)

  Here's a poor attempt at illustrating that:

     tree-vectorizer.cc:
     loop_vect()  loop_aware_slp()  slp_vect()
          |        /           \          /
          |       /             \        /
          tree-vect-loop.cc  tree-vect-slp.cc
                | \      \  /      /   |
                |  \      \/      /    |
                |   \     /\     /     |
                |    \   /  \   /      |
         tree-vect-stmts.cc  tree-vect-data-refs.cc
                       \      /
                    tree-vect-patterns.cc
*/

#include "config.h"
#define INCLUDE_MEMORY
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
#include "opt-problem.h"
#include "internal-fn.h"
#include "tree-ssa-sccvn.h"
#include "tree-into-ssa.h"

/* Loop or bb location, with hotness information.  */
dump_user_location_t vect_location;

/* auto_purge_vect_location's dtor: reset the vect_location
   global, to avoid stale location_t values that could reference
   GC-ed blocks.  */

auto_purge_vect_location::~auto_purge_vect_location ()
{
  vect_location = dump_user_location_t ();
}

/* Dump a cost entry according to args to F.  */

void
dump_stmt_cost (FILE *f, int count, enum vect_cost_for_stmt kind,
		stmt_vec_info stmt_info, slp_tree node, tree,
		int misalign, unsigned cost,
		enum vect_cost_model_location where)
{
  if (stmt_info)
    {
      print_gimple_expr (f, STMT_VINFO_STMT (stmt_info), 0, TDF_SLIM);
      fprintf (f, " ");
    }
  else if (node)
    fprintf (f, "node %p ", (void *)node);
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
      ks = "vector_store";
      break;
    case vector_scatter_store:
      ks = "vector_scatter_store";
      break;
    case vec_to_scalar:
      ks = "vec_to_scalar";
      break;
    case scalar_to_vec:
      ks = "scalar_to_vec";
      break;
    case cond_branch_not_taken:
      ks = "cond_branch_not_taken";
      break;
    case cond_branch_taken:
      ks = "cond_branch_taken";
      break;
    case vec_perm:
      ks = "vec_perm";
      break;
    case vec_promote_demote:
      ks = "vec_promote_demote";
      break;
    case vec_construct:
      ks = "vec_construct";
      break;
    }
  fprintf (f, "%s ", ks);
  if (kind == unaligned_load || kind == unaligned_store)
    fprintf (f, "(misalign %d) ", misalign);
  fprintf (f, "costs %u ", cost);
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

class simduid_to_vf : public free_ptr_hash<simduid_to_vf>
{
public:
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
adjust_simduid_builtins (hash_table<simduid_to_vf> *htab, function *fun)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, fun)
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
		  gimple_move_vops (g, stmt);
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
note_simd_array_uses (hash_table<simd_array_to_simduid> **htab, function *fun)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  struct walk_stmt_info wi;
  struct note_simd_array_uses_struct ns;

  memset (&wi, 0, sizeof (wi));
  wi.info = &ns;
  ns.htab = htab;

  FOR_EACH_BB_FN (bb, fun)
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

vec_info::vec_info (vec_info::vec_kind kind_in, vec_info_shared *shared_)
  : kind (kind_in),
    shared (shared_),
    stmt_vec_info_ro (false),
    bbs (NULL),
    nbbs (0),
    inv_pattern_def_seq (NULL)
{
  stmt_vec_infos.create (50);
}

vec_info::~vec_info ()
{
  for (slp_instance &instance : slp_instances)
    vect_free_slp_instance (instance);

  free_stmt_vec_infos ();
}

vec_info_shared::vec_info_shared ()
  : n_stmts (0),
    datarefs (vNULL),
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
    if (memcmp (&datarefs_copy[i], datarefs[i],
		offsetof (data_reference, alt_indices)) != 0)
      gcc_unreachable ();
}

/* Record that STMT belongs to the vectorizable region.  Create and return
   an associated stmt_vec_info.  */

stmt_vec_info
vec_info::add_stmt (gimple *stmt)
{
  stmt_vec_info res = new_stmt_vec_info (stmt);
  set_vinfo_for_stmt (stmt, res);
  return res;
}

/* Record that STMT belongs to the vectorizable region.  Create a new
   stmt_vec_info and mark VECINFO as being related and return the new
   stmt_vec_info.  */

stmt_vec_info
vec_info::add_pattern_stmt (gimple *stmt, stmt_vec_info stmt_info)
{
  stmt_vec_info res = new_stmt_vec_info (stmt);
  res->pattern_stmt_p = true;
  set_vinfo_for_stmt (stmt, res, false);
  STMT_VINFO_RELATED_STMT (res) = stmt_info;
  return res;
}

/* If STMT has an associated stmt_vec_info, return that vec_info, otherwise
   return null.  It is safe to call this function on any statement, even if
   it might not be part of the vectorizable region.  */

stmt_vec_info
vec_info::lookup_stmt (gimple *stmt)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid > 0 && uid - 1 < stmt_vec_infos.length ())
    {
      stmt_vec_info res = stmt_vec_infos[uid - 1];
      if (res && res->stmt == stmt)
	return res;
    }
  return NULL;
}

/* If NAME is an SSA_NAME and its definition has an associated stmt_vec_info,
   return that stmt_vec_info, otherwise return null.  It is safe to call
   this on arbitrary operands.  */

stmt_vec_info
vec_info::lookup_def (tree name)
{
  if (TREE_CODE (name) == SSA_NAME
      && !SSA_NAME_IS_DEFAULT_DEF (name))
    return lookup_stmt (SSA_NAME_DEF_STMT (name));
  return NULL;
}

/* See whether there is a single non-debug statement that uses LHS and
   whether that statement has an associated stmt_vec_info.  Return the
   stmt_vec_info if so, otherwise return null.  */

stmt_vec_info
vec_info::lookup_single_use (tree lhs)
{
  use_operand_p dummy;
  gimple *use_stmt;
  if (single_imm_use (lhs, &dummy, &use_stmt))
    return lookup_stmt (use_stmt);
  return NULL;
}

/* Return vectorization information about DR.  */

dr_vec_info *
vec_info::lookup_dr (data_reference *dr)
{
  stmt_vec_info stmt_info = lookup_stmt (DR_STMT (dr));
  /* DR_STMT should never refer to a stmt in a pattern replacement.  */
  gcc_checking_assert (!is_pattern_stmt_p (stmt_info));
  return STMT_VINFO_DR_INFO (stmt_info->dr_aux.stmt);
}

/* Record that NEW_STMT_INFO now implements the same data reference
   as OLD_STMT_INFO.  */

void
vec_info::move_dr (stmt_vec_info new_stmt_info, stmt_vec_info old_stmt_info)
{
  gcc_assert (!is_pattern_stmt_p (old_stmt_info));
  STMT_VINFO_DR_INFO (old_stmt_info)->stmt = new_stmt_info;
  new_stmt_info->dr_aux = old_stmt_info->dr_aux;
  STMT_VINFO_DR_WRT_VEC_LOOP (new_stmt_info)
    = STMT_VINFO_DR_WRT_VEC_LOOP (old_stmt_info);
  STMT_VINFO_GATHER_SCATTER_P (new_stmt_info)
    = STMT_VINFO_GATHER_SCATTER_P (old_stmt_info);
  STMT_VINFO_STRIDED_P (new_stmt_info)
    = STMT_VINFO_STRIDED_P (old_stmt_info);
}

/* Permanently remove the statement described by STMT_INFO from the
   function.  */

void
vec_info::remove_stmt (stmt_vec_info stmt_info)
{
  gcc_assert (!stmt_info->pattern_stmt_p);
  set_vinfo_for_stmt (stmt_info->stmt, NULL);
  unlink_stmt_vdef (stmt_info->stmt);
  gimple_stmt_iterator si = gsi_for_stmt (stmt_info->stmt);
  gsi_remove (&si, true);
  release_defs (stmt_info->stmt);
  free_stmt_vec_info (stmt_info);
}

/* Replace the statement at GSI by NEW_STMT, both the vectorization
   information and the function itself.  STMT_INFO describes the statement
   at GSI.  */

void
vec_info::replace_stmt (gimple_stmt_iterator *gsi, stmt_vec_info stmt_info,
			gimple *new_stmt)
{
  gimple *old_stmt = stmt_info->stmt;
  gcc_assert (!stmt_info->pattern_stmt_p && old_stmt == gsi_stmt (*gsi));
  gimple_set_uid (new_stmt, gimple_uid (old_stmt));
  stmt_info->stmt = new_stmt;
  gsi_replace (gsi, new_stmt, true);
}

/* Insert stmts in SEQ on the VEC_INFO region entry.  If CONTEXT is
   not NULL it specifies whether to use the sub-region entry
   determined by it, currently used for loop vectorization to insert
   on the inner loop entry vs. the outer loop entry.  */

void
vec_info::insert_seq_on_entry (stmt_vec_info context, gimple_seq seq)
{
  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (this))
    {
      class loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
      basic_block new_bb;
      edge pe;

      if (context && nested_in_vect_loop_p (loop, context))
	loop = loop->inner;

      pe = loop_preheader_edge (loop);
      new_bb = gsi_insert_seq_on_edge_immediate (pe, seq);
      gcc_assert (!new_bb);
    }
  else
    {
      gimple_stmt_iterator gsi_region_begin
	= gsi_after_labels (bbs[0]);
      gsi_insert_seq_before (&gsi_region_begin, seq, GSI_SAME_STMT);
    }
}

/* Like insert_seq_on_entry but just inserts the single stmt NEW_STMT.  */

void
vec_info::insert_on_entry (stmt_vec_info context, gimple *new_stmt)
{
  gimple_seq seq = NULL;
  gimple_stmt_iterator gsi = gsi_start (seq);
  gsi_insert_before_without_update (&gsi, new_stmt, GSI_SAME_STMT);
  insert_seq_on_entry (context, seq);
}

/* Create and initialize a new stmt_vec_info struct for STMT.  */

stmt_vec_info
vec_info::new_stmt_vec_info (gimple *stmt)
{
  stmt_vec_info res = XCNEW (class _stmt_vec_info);
  res->stmt = stmt;

  STMT_VINFO_TYPE (res) = undef_vec_info_type;
  STMT_VINFO_RELEVANT (res) = vect_unused_in_scope;
  STMT_VINFO_VECTORIZABLE (res) = true;
  STMT_VINFO_REDUC_TYPE (res) = TREE_CODE_REDUCTION;
  STMT_VINFO_REDUC_CODE (res) = ERROR_MARK;
  STMT_VINFO_REDUC_FN (res) = IFN_LAST;
  STMT_VINFO_REDUC_IDX (res) = -1;
  STMT_VINFO_SLP_VECT_ONLY (res) = false;
  STMT_VINFO_SLP_VECT_ONLY_PATTERN (res) = false;
  STMT_VINFO_VEC_STMTS (res) = vNULL;
  res->reduc_initial_values = vNULL;
  res->reduc_scalar_results = vNULL;

  if (is_a <loop_vec_info> (this)
      && gimple_code (stmt) == GIMPLE_PHI
      && is_loop_header_bb_p (gimple_bb (stmt)))
    STMT_VINFO_DEF_TYPE (res) = vect_unknown_def_type;
  else
    STMT_VINFO_DEF_TYPE (res) = vect_internal_def;

  STMT_SLP_TYPE (res) = loop_vect;

  /* This is really "uninitialized" until vect_compute_data_ref_alignment.  */
  res->dr_aux.misalignment = DR_MISALIGNMENT_UNINITIALIZED;

  return res;
}

/* Associate STMT with INFO.  */

void
vec_info::set_vinfo_for_stmt (gimple *stmt, stmt_vec_info info, bool check_ro)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    {
      gcc_assert (!check_ro || !stmt_vec_info_ro);
      gcc_checking_assert (info);
      uid = stmt_vec_infos.length () + 1;
      gimple_set_uid (stmt, uid);
      stmt_vec_infos.safe_push (info);
    }
  else
    {
      gcc_checking_assert (info == NULL);
      stmt_vec_infos[uid - 1] = info;
    }
}

/* Free the contents of stmt_vec_infos.  */

void
vec_info::free_stmt_vec_infos (void)
{
  for (stmt_vec_info &info : stmt_vec_infos)
    if (info != NULL)
      free_stmt_vec_info (info);
  stmt_vec_infos.release ();
}

/* Free STMT_INFO.  */

void
vec_info::free_stmt_vec_info (stmt_vec_info stmt_info)
{
  if (stmt_info->pattern_stmt_p)
    {
      gimple_set_bb (stmt_info->stmt, NULL);
      tree lhs = gimple_get_lhs (stmt_info->stmt);
      if (lhs && TREE_CODE (lhs) == SSA_NAME)
	release_ssa_name (lhs);
    }

  stmt_info->reduc_initial_values.release ();
  stmt_info->reduc_scalar_results.release ();
  STMT_VINFO_SIMD_CLONE_INFO (stmt_info).release ();
  STMT_VINFO_VEC_STMTS (stmt_info).release ();
  free (stmt_info);
}

/* Returns true if S1 dominates S2.  */

bool
vect_stmt_dominates_stmt_p (gimple *s1, gimple *s2)
{
  basic_block bb1 = gimple_bb (s1), bb2 = gimple_bb (s2);

  /* If bb1 is NULL, it should be a GIMPLE_NOP def stmt of an (D)
     SSA_NAME.  Assume it lives at the beginning of function and
     thus dominates everything.  */
  if (!bb1 || s1 == s2)
    return true;

  /* If bb2 is NULL, it doesn't dominate any stmt with a bb.  */
  if (!bb2)
    return false;

  if (bb1 != bb2)
    return dominated_by_p (CDI_DOMINATORS, bb2, bb1);

  /* PHIs in the same basic block are assumed to be
     executed all in parallel, if only one stmt is a PHI,
     it dominates the other stmt in the same basic block.  */
  if (gimple_code (s1) == GIMPLE_PHI)
    return true;

  if (gimple_code (s2) == GIMPLE_PHI)
    return false;

  /* Inserted vectorized stmts all have UID 0 while the original stmts
     in the IL have UID increasing within a BB.  Walk from both sides
     until we find the other stmt or a stmt with UID != 0.  */
  gimple_stmt_iterator gsi1 = gsi_for_stmt (s1);
  while (gimple_uid (gsi_stmt (gsi1)) == 0)
    {
      gsi_next (&gsi1);
      if (gsi_end_p (gsi1))
	return false;
      if (gsi_stmt (gsi1) == s2)
	return true;
    }
  if (gimple_uid (gsi_stmt (gsi1)) == -1u)
    return false;

  gimple_stmt_iterator gsi2 = gsi_for_stmt (s2);
  while (gimple_uid (gsi_stmt (gsi2)) == 0)
    {
      gsi_prev (&gsi2);
      if (gsi_end_p (gsi2))
	return false;
      if (gsi_stmt (gsi2) == s1)
	return true;
    }
  if (gimple_uid (gsi_stmt (gsi2)) == -1u)
    return false;

  if (gimple_uid (gsi_stmt (gsi1)) <= gimple_uid (gsi_stmt (gsi2)))
    return true;
  return false;
}

/* A helper function to free scev and LOOP niter information, as well as
   clear loop constraint LOOP_C_FINITE.  */

void
vect_free_loop_info_assumptions (class loop *loop)
{
  scev_reset_htab ();
  /* We need to explicitly reset upper bound information since they are
     used even after free_numbers_of_iterations_estimates.  */
  loop->any_upper_bound = false;
  loop->any_likely_upper_bound = false;
  free_numbers_of_iterations_estimates (loop);
  loop_constraint_clear (loop, LOOP_C_FINITE);
}

/* If LOOP has been versioned during ifcvt, return the internal call
   guarding it.  */

gimple *
vect_loop_vectorized_call (class loop *loop, gcond **cond)
{
  basic_block bb = loop_preheader_edge (loop)->src;
  gimple *g;
  do
    {
      g = *gsi_last_bb (bb);
      if ((g && gimple_code (g) == GIMPLE_COND)
	  || !single_succ_p (bb))
	break;
      if (!single_pred_p (bb))
	break;
      bb = single_pred (bb);
    }
  while (1);
  if (g && gimple_code (g) == GIMPLE_COND)
    {
      if (cond)
	*cond = as_a <gcond *> (g);
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
vect_loop_dist_alias_call (class loop *loop, function *fun)
{
  basic_block bb;
  basic_block entry;
  class loop *outer, *orig;

  if (loop->orig_loop_num == 0)
    return NULL;

  orig = get_loop (fun, loop->orig_loop_num);
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
  entry = ENTRY_BLOCK_PTR_FOR_FN (fun);

  /* Look upward in dominance tree.  */
  for (; bb != entry && flow_bb_inside_loop_p (outer, bb);
       bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      gimple_stmt_iterator gsi = gsi_last_bb (bb);
      if (!safe_is_a <gcond *> (*gsi))
	continue;

      gsi_prev (&gsi);
      if (gsi_end_p (gsi))
	continue;

      gimple *g = gsi_stmt (gsi);
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
set_uid_loop_bbs (loop_vec_info loop_vinfo, gimple *loop_vectorized_call,
		  function *fun)
{
  tree arg = gimple_call_arg (loop_vectorized_call, 1);
  basic_block *bbs;
  unsigned int i;
  class loop *scalar_loop = get_loop (fun, tree_to_shwi (arg));

  LOOP_VINFO_SCALAR_LOOP (loop_vinfo) = scalar_loop;
  LOOP_VINFO_SCALAR_IV_EXIT (loop_vinfo)
    = vec_init_loop_exit_info (scalar_loop);
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
	  get_loop (fun, tree_to_shwi (arg))->dont_vectorize = true;
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

/* Generate vectorized code for LOOP and its epilogues.  */

static unsigned
vect_transform_loops (hash_table<simduid_to_vf> *&simduid_to_vf_htab,
		      loop_p loop, gimple *loop_vectorized_call,
		      function *fun)
{
  loop_vec_info loop_vinfo = loop_vec_info_for_loop (loop);

  if (loop_vectorized_call)
    set_uid_loop_bbs (loop_vinfo, loop_vectorized_call, fun);

  unsigned HOST_WIDE_INT bytes;
  if (dump_enabled_p ())
    {
      if (GET_MODE_SIZE (loop_vinfo->vector_mode).is_constant (&bytes))
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
			 "loop vectorized using %wu byte vectors\n", bytes);
      else
	dump_printf_loc (MSG_OPTIMIZED_LOCATIONS, vect_location,
			 "loop vectorized using variable length vectors\n");
    }

  loop_p new_loop = vect_transform_loop (loop_vinfo,
					 loop_vectorized_call);
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

  /* We should not have to update virtual SSA form here but some
     transforms involve creating new virtual definitions which makes
     updating difficult.
     We delay the actual update to the end of the pass but avoid
     confusing ourselves by forcing need_ssa_update_p () to false.  */
  unsigned todo = 0;
  if (need_ssa_update_p (cfun))
    {
      gcc_assert (loop_vinfo->any_known_not_updated_vssa);
      fun->gimple_df->ssa_renaming_needed = false;
      todo |= TODO_update_ssa_only_virtuals;
    }
  gcc_assert (!need_ssa_update_p (cfun));

  /* Epilogue of vectorized loop must be vectorized too.  */
  if (new_loop)
    todo |= vect_transform_loops (simduid_to_vf_htab, new_loop, NULL, fun);

  return todo;
}

/* Try to vectorize LOOP.  */

static unsigned
try_vectorize_loop_1 (hash_table<simduid_to_vf> *&simduid_to_vf_htab,
		      unsigned *num_vectorized_loops, loop_p loop,
		      gimple *loop_vectorized_call,
		      gimple *loop_dist_alias_call,
		      function *fun)
{
  unsigned ret = 0;
  vec_info_shared shared;
  auto_purge_vect_location sentinel;
  vect_location = find_loop_location (loop);

  if (LOCATION_LOCUS (vect_location.get_location_t ()) != UNKNOWN_LOCATION
      && dump_enabled_p ())
    dump_printf (MSG_NOTE | MSG_PRIORITY_INTERNALS,
		 "\nAnalyzing loop at %s:%d\n",
		 LOCATION_FILE (vect_location.get_location_t ()),
		 LOCATION_LINE (vect_location.get_location_t ()));

  /* Try to analyze the loop, retaining an opt_problem if dump_enabled_p.  */
  opt_loop_vec_info loop_vinfo = vect_analyze_loop (loop, loop_vectorized_call,
						    &shared);
  loop->aux = loop_vinfo;

  if (!loop_vinfo)
    if (dump_enabled_p ())
      if (opt_problem *problem = loop_vinfo.get_problem ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "couldn't vectorize loop\n");
	  problem->emit_and_clear ();
	}

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
	  bool require_loop_vectorize = false;
	  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	       !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      gcall *call = dyn_cast <gcall *> (stmt);
	      if (call && gimple_call_internal_p (call))
		{
		  internal_fn ifn = gimple_call_internal_fn (call);
		  if (ifn == IFN_MASK_LOAD || ifn == IFN_MASK_STORE
		      /* Don't keep the if-converted parts when the ifn with
			 specifc type is not supported by the backend.  */
		      || (direct_internal_fn_p (ifn)
			  && !direct_internal_fn_supported_p
			  (call, OPTIMIZE_FOR_SPEED)))
		    {
		      require_loop_vectorize = true;
		      break;
		    }
		}
	      gimple_set_uid (stmt, -1);
	      gimple_set_visited (stmt, false);
	    }
	  if (!require_loop_vectorize)
	    {
	      tree arg = gimple_call_arg (loop_vectorized_call, 1);
	      class loop *scalar_loop = get_loop (fun, tree_to_shwi (arg));
	      if (vect_slp_if_converted_bb (bb, scalar_loop))
		{
		  fold_loop_internal_call (loop_vectorized_call,
					   boolean_true_node);
		  loop_vectorized_call = NULL;
		  ret |= TODO_cleanup_cfg | TODO_update_ssa_only_virtuals;
		}
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

  (*num_vectorized_loops)++;
  /* Transform LOOP and its epilogues.  */
  ret |= vect_transform_loops (simduid_to_vf_htab, loop,
			       loop_vectorized_call, fun);

  if (loop_vectorized_call)
    {
      fold_loop_internal_call (loop_vectorized_call, boolean_true_node);
      ret |= TODO_cleanup_cfg;
    }
  if (loop_dist_alias_call)
    {
      tree value = gimple_call_arg (loop_dist_alias_call, 1);
      fold_loop_internal_call (loop_dist_alias_call, value);
      ret |= TODO_cleanup_cfg;
    }

  return ret;
}

/* Try to vectorize LOOP.  */

static unsigned
try_vectorize_loop (hash_table<simduid_to_vf> *&simduid_to_vf_htab,
		    unsigned *num_vectorized_loops, loop_p loop,
		    function *fun)
{
  if (!((flag_tree_loop_vectorize
	 && optimize_loop_nest_for_speed_p (loop))
	|| loop->force_vectorize))
    return 0;

  return try_vectorize_loop_1 (simduid_to_vf_htab, num_vectorized_loops, loop,
			       vect_loop_vectorized_call (loop),
			       vect_loop_dist_alias_call (loop, fun), fun);
}


/* Loop autovectorization.  */

namespace {

const pass_data pass_data_vectorize =
{
  GIMPLE_PASS, /* type */
  "vect", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
  TV_TREE_VECTORIZATION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_vectorize : public gimple_opt_pass
{
public:
  pass_vectorize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_vectorize, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *fun) final override
    {
      return flag_tree_loop_vectorize || fun->has_force_vectorize_loops;
    }

  unsigned int execute (function *) final override;

}; // class pass_vectorize

/* Function vectorize_loops.

   Entry point to loop vectorization phase.  */

unsigned
pass_vectorize::execute (function *fun)
{
  unsigned int i;
  unsigned int num_vectorized_loops = 0;
  unsigned int vect_loops_num;
  hash_table<simduid_to_vf> *simduid_to_vf_htab = NULL;
  hash_table<simd_array_to_simduid> *simd_array_to_simduid_htab = NULL;
  bool any_ifcvt_loops = false;
  unsigned ret = 0;

  vect_loops_num = number_of_loops (fun);

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    return 0;

  vect_slp_init ();

  if (fun->has_simduid_loops)
    note_simd_array_uses (&simd_array_to_simduid_htab, fun);

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number
     than all previously defined loops.  This fact allows us to run
     only over initial loops skipping newly generated ones.  */
  for (auto loop : loops_list (fun, 0))
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
	   If loops' iteration gives us loop3 first (which has
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
		class loop *vector_loop
		  = get_loop (fun, tree_to_shwi (arg));
		if (vector_loop && vector_loop != loop)
		  {
		    /* Make sure we don't vectorize it twice.  */
		    vector_loop->dont_vectorize = true;
		    ret |= try_vectorize_loop (simduid_to_vf_htab,
					       &num_vectorized_loops,
					       vector_loop, fun);
		  }
	      }
	  }
      }
    else
      ret |= try_vectorize_loop (simduid_to_vf_htab, &num_vectorized_loops,
				 loop, fun);

  vect_location = dump_user_location_t ();

  statistics_counter_event (fun, "Vectorized loops", num_vectorized_loops);
  if (dump_enabled_p ()
      || (num_vectorized_loops > 0 && dump_enabled_p ()))
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vectorized %u loops in function.\n",
                     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  if (any_ifcvt_loops)
    for (i = 1; i < number_of_loops (fun); i++)
      {
	class loop *loop = get_loop (fun, i);
	if (loop && loop->dont_vectorize)
	  {
	    gimple *g = vect_loop_vectorized_call (loop);
	    if (g)
	      {
		fold_loop_internal_call (g, boolean_false_node);
		loop->dont_vectorize = false;
		ret |= TODO_cleanup_cfg;
		g = NULL;
	      }
	    else
	      g = vect_loop_dist_alias_call (loop, fun);

	    if (g)
	      {
		fold_loop_internal_call (g, boolean_false_node);
		loop->dont_vectorize = false;
		ret |= TODO_cleanup_cfg;
	      }
	  }
      }

  /* Fold IFN_GOMP_SIMD_{VF,LANE,LAST_LANE,ORDERED_{START,END}} builtins.  */
  if (fun->has_simduid_loops)
    {
      adjust_simduid_builtins (simduid_to_vf_htab, fun);
      /* Avoid stale SCEV cache entries for the SIMD_LANE defs.  */
      scev_reset ();
    }
  /* Shrink any "omp array simd" temporary arrays to the
     actual vectorization factors.  */
  if (simd_array_to_simduid_htab)
    shrink_simd_arrays (simd_array_to_simduid_htab, simduid_to_vf_htab);
  delete simduid_to_vf_htab;
  fun->has_simduid_loops = false;

  if (num_vectorized_loops > 0)
    {
      /* We are collecting some corner cases where we need to update
	 virtual SSA form via the TODO but delete the queued update-SSA
	 state.  Force renaming if we think that might be necessary.  */
      if (ret & TODO_update_ssa_only_virtuals)
	mark_virtual_operands_for_renaming (cfun);
      /* If we vectorized any loop only virtual SSA form needs to be updated.
	 ???  Also while we try hard to update loop-closed SSA form we fail
	 to properly do this in some corner-cases (see PR56286).  */
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa_only_virtuals);
      ret |= TODO_cleanup_cfg;
    }

  for (i = 1; i < number_of_loops (fun); i++)
    {
      loop_vec_info loop_vinfo;
      bool has_mask_store;

      class loop *loop = get_loop (fun, i);
      if (!loop || !loop->aux)
	continue;
      loop_vinfo = (loop_vec_info) loop->aux;
      has_mask_store = LOOP_VINFO_HAS_MASK_STORE (loop_vinfo);
      delete loop_vinfo;
      if (has_mask_store
	  && targetm.vectorize.empty_mask_is_expensive (IFN_MASK_STORE))
	optimize_mask_stores (loop);

      auto_bitmap exit_bbs;
      /* Perform local CSE, this esp. helps because we emit code for
	 predicates that need to be shared for optimal predicate usage.
	 However reassoc will re-order them and prevent CSE from working
	 as it should.  CSE only the loop body, not the entry.  */
      auto_vec<edge> exits = get_loop_exit_edges (loop);
      for (edge exit : exits)
	bitmap_set_bit (exit_bbs, exit->dest->index);

      edge entry = EDGE_PRED (loop_preheader_edge (loop)->src, 0);
      do_rpo_vn (fun, entry, exit_bbs);

      loop->aux = NULL;
    }

  vect_slp_fini ();

  return ret;
}

} // anon namespace

gimple_opt_pass *
make_pass_vectorize (gcc::context *ctxt)
{
  return new pass_vectorize (ctxt);
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
  opt_pass * clone () final override
  {
    return new pass_simduid_cleanup (m_ctxt);
  }
  bool gate (function *fun) final override { return fun->has_simduid_loops; }
  unsigned int execute (function *) final override;

}; // class pass_simduid_cleanup

unsigned int
pass_simduid_cleanup::execute (function *fun)
{
  hash_table<simd_array_to_simduid> *simd_array_to_simduid_htab = NULL;

  note_simd_array_uses (&simd_array_to_simduid_htab, fun);

  /* Fold IFN_GOMP_SIMD_{VF,LANE,LAST_LANE,ORDERED_{START,END}} builtins.  */
  adjust_simduid_builtins (NULL, fun);

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
  opt_pass * clone () final override { return new pass_slp_vectorize (m_ctxt); }
  bool gate (function *) final override { return flag_tree_slp_vectorize != 0; }
  unsigned int execute (function *) final override;

}; // class pass_slp_vectorize

unsigned int
pass_slp_vectorize::execute (function *fun)
{
  auto_purge_vect_location sentinel;
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
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *stmt = gsi.phi ();
	  gimple_set_uid (stmt, -1);
	  gimple_set_visited (stmt, false);
	}
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, -1);
	  gimple_set_visited (stmt, false);
	}
    }

  vect_slp_init ();

  vect_slp_function (fun);

  vect_slp_fini ();

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

  tree scalar_type = strip_array_types (type);
  tree vectype = get_related_vectype_for_scalar_type (VOIDmode, scalar_type);
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
	  if (dump_enabled_p ())
	    dump_printf (MSG_NOTE, "Increasing alignment of decl: %T\n", decl);
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
  bool gate (function *) final override
    {
      return flag_section_anchors && flag_tree_loop_vectorize;
    }

  unsigned int execute (function *) final override
  {
    return increase_alignment ();
  }

}; // class pass_ipa_increase_alignment

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_increase_alignment (gcc::context *ctxt)
{
  return new pass_ipa_increase_alignment (ctxt);
}

/* If the condition represented by T is a comparison or the SSA name
   result of a comparison, extract the comparison's operands.  Represent
   T as NE_EXPR <T, 0> otherwise.  */

void
scalar_cond_masked_key::get_cond_ops_from_tree (tree t)
{
  if (TREE_CODE_CLASS (TREE_CODE (t)) == tcc_comparison)
    {
      this->code = TREE_CODE (t);
      this->op0 = TREE_OPERAND (t, 0);
      this->op1 = TREE_OPERAND (t, 1);
      this->inverted_p = false;
      return;
    }

  if (TREE_CODE (t) == SSA_NAME)
    if (gassign *stmt = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (t)))
      {
	tree_code code = gimple_assign_rhs_code (stmt);
	if (TREE_CODE_CLASS (code) == tcc_comparison)
	  {
	    this->code = code;
	    this->op0 = gimple_assign_rhs1 (stmt);
	    this->op1 = gimple_assign_rhs2 (stmt);
	    this->inverted_p = false;
	    return;
	  }
	else if (code == BIT_NOT_EXPR)
	  {
	    tree n_op = gimple_assign_rhs1 (stmt);
	    if ((stmt = dyn_cast<gassign *> (SSA_NAME_DEF_STMT (n_op))))
	      {
		code = gimple_assign_rhs_code (stmt);
		if (TREE_CODE_CLASS (code) == tcc_comparison)
		  {
		    this->code = code;
		    this->op0 = gimple_assign_rhs1 (stmt);
		    this->op1 = gimple_assign_rhs2 (stmt);
		    this->inverted_p = true;
		    return;
		  }
	      }
	  }
      }

  this->code = NE_EXPR;
  this->op0 = t;
  this->op1 = build_zero_cst (TREE_TYPE (t));
  this->inverted_p = false;
}

/* See the comment above the declaration for details.  */

unsigned int
vector_costs::add_stmt_cost (int count, vect_cost_for_stmt kind,
			     stmt_vec_info stmt_info, slp_tree,
			     tree vectype, int misalign,
			     vect_cost_model_location where)
{
  unsigned int cost
    = builtin_vectorization_cost (kind, vectype, misalign) * count;
  return record_stmt_cost (stmt_info, where, cost);
}

/* See the comment above the declaration for details.  */

void
vector_costs::finish_cost (const vector_costs *)
{
  gcc_assert (!m_finished);
  m_finished = true;
}

/* Record a base cost of COST units against WHERE.  If STMT_INFO is
   nonnull, use it to adjust the cost based on execution frequency
   (where appropriate).  */

unsigned int
vector_costs::record_stmt_cost (stmt_vec_info stmt_info,
				vect_cost_model_location where,
				unsigned int cost)
{
  cost = adjust_cost_for_freq (stmt_info, where, cost);
  m_costs[where] += cost;
  return cost;
}

/* COST is the base cost we have calculated for an operation in location WHERE.
   If STMT_INFO is nonnull, use it to adjust the cost based on execution
   frequency (where appropriate).  Return the adjusted cost.  */

unsigned int
vector_costs::adjust_cost_for_freq (stmt_vec_info stmt_info,
				    vect_cost_model_location where,
				    unsigned int cost)
{
  /* Statements in an inner loop relative to the loop being
     vectorized are weighted more heavily.  The value here is
     arbitrary and could potentially be improved with analysis.  */
  if (where == vect_body
      && stmt_info
      && stmt_in_inner_loop_p (m_vinfo, stmt_info))
    {
      loop_vec_info loop_vinfo = as_a<loop_vec_info> (m_vinfo);
      cost *= LOOP_VINFO_INNER_LOOP_COST_FACTOR (loop_vinfo);
    }
  return cost;
}

/* See the comment above the declaration for details.  */

bool
vector_costs::better_main_loop_than_p (const vector_costs *other) const
{
  int diff = compare_inside_loop_cost (other);
  if (diff != 0)
    return diff < 0;

  /* If there's nothing to choose between the loop bodies, see whether
     there's a difference in the prologue and epilogue costs.  */
  diff = compare_outside_loop_cost (other);
  if (diff != 0)
    return diff < 0;

  return false;
}


/* See the comment above the declaration for details.  */

bool
vector_costs::better_epilogue_loop_than_p (const vector_costs *other,
					   loop_vec_info main_loop) const
{
  loop_vec_info this_loop_vinfo = as_a<loop_vec_info> (this->m_vinfo);
  loop_vec_info other_loop_vinfo = as_a<loop_vec_info> (other->m_vinfo);

  poly_int64 this_vf = LOOP_VINFO_VECT_FACTOR (this_loop_vinfo);
  poly_int64 other_vf = LOOP_VINFO_VECT_FACTOR (other_loop_vinfo);

  poly_uint64 main_poly_vf = LOOP_VINFO_VECT_FACTOR (main_loop);
  unsigned HOST_WIDE_INT main_vf;
  unsigned HOST_WIDE_INT other_factor, this_factor, other_cost, this_cost;
  /* If we can determine how many iterations are left for the epilogue
     loop, that is if both the main loop's vectorization factor and number
     of iterations are constant, then we use them to calculate the cost of
     the epilogue loop together with a 'likely value' for the epilogues
     vectorization factor.  Otherwise we use the main loop's vectorization
     factor and the maximum poly value for the epilogue's.  If the target
     has not provided with a sensible upper bound poly vectorization
     factors are likely to be favored over constant ones.  */
  if (main_poly_vf.is_constant (&main_vf)
      && LOOP_VINFO_NITERS_KNOWN_P (main_loop))
    {
      unsigned HOST_WIDE_INT niters
	= LOOP_VINFO_INT_NITERS (main_loop) % main_vf;
      HOST_WIDE_INT other_likely_vf
	= estimated_poly_value (other_vf, POLY_VALUE_LIKELY);
      HOST_WIDE_INT this_likely_vf
	= estimated_poly_value (this_vf, POLY_VALUE_LIKELY);

      /* If the epilogue is using partial vectors we account for the
	 partial iteration here too.  */
      other_factor = niters / other_likely_vf;
      if (LOOP_VINFO_USING_PARTIAL_VECTORS_P (other_loop_vinfo)
	  && niters % other_likely_vf != 0)
	other_factor++;

      this_factor = niters / this_likely_vf;
      if (LOOP_VINFO_USING_PARTIAL_VECTORS_P (this_loop_vinfo)
	  && niters % this_likely_vf != 0)
	this_factor++;
    }
  else
    {
      unsigned HOST_WIDE_INT main_vf_max
	= estimated_poly_value (main_poly_vf, POLY_VALUE_MAX);
      unsigned HOST_WIDE_INT other_vf_max
	= estimated_poly_value (other_vf, POLY_VALUE_MAX);
      unsigned HOST_WIDE_INT this_vf_max
	= estimated_poly_value (this_vf, POLY_VALUE_MAX);

      other_factor = CEIL (main_vf_max, other_vf_max);
      this_factor = CEIL (main_vf_max, this_vf_max);

      /* If the loop is not using partial vectors then it will iterate one
	 time less than one that does.  It is safe to subtract one here,
	 because the main loop's vf is always at least 2x bigger than that
	 of an epilogue.  */
      if (!LOOP_VINFO_USING_PARTIAL_VECTORS_P (other_loop_vinfo))
	other_factor -= 1;
      if (!LOOP_VINFO_USING_PARTIAL_VECTORS_P (this_loop_vinfo))
	this_factor -= 1;
    }

  /* Compute the costs by multiplying the inside costs with the factor and
     add the outside costs for a more complete picture.  The factor is the
     amount of times we are expecting to iterate this epilogue.  */
  other_cost = other->body_cost () * other_factor;
  this_cost = this->body_cost () * this_factor;
  other_cost += other->outside_cost ();
  this_cost += this->outside_cost ();
  return this_cost < other_cost;
}

/* A <=>-style subroutine of better_main_loop_than_p.  Check whether we can
   determine the return value of better_main_loop_than_p by comparing the
   inside (loop body) costs of THIS and OTHER.  Return:

   * -1 if better_main_loop_than_p should return true.
   * 1 if better_main_loop_than_p should return false.
   * 0 if we can't decide.  */

int
vector_costs::compare_inside_loop_cost (const vector_costs *other) const
{
  loop_vec_info this_loop_vinfo = as_a<loop_vec_info> (this->m_vinfo);
  loop_vec_info other_loop_vinfo = as_a<loop_vec_info> (other->m_vinfo);

  struct loop *loop = LOOP_VINFO_LOOP (this_loop_vinfo);
  gcc_assert (LOOP_VINFO_LOOP (other_loop_vinfo) == loop);

  poly_int64 this_vf = LOOP_VINFO_VECT_FACTOR (this_loop_vinfo);
  poly_int64 other_vf = LOOP_VINFO_VECT_FACTOR (other_loop_vinfo);

  /* Limit the VFs to what is likely to be the maximum number of iterations,
     to handle cases in which at least one loop_vinfo is fully-masked.  */
  HOST_WIDE_INT estimated_max_niter = likely_max_stmt_executions_int (loop);
  if (estimated_max_niter != -1)
    {
      if (estimated_poly_value (this_vf, POLY_VALUE_MIN)
	  >= estimated_max_niter)
	this_vf = estimated_max_niter;
      if (estimated_poly_value (other_vf, POLY_VALUE_MIN)
	  >= estimated_max_niter)
	other_vf = estimated_max_niter;
    }

  /* Check whether the (fractional) cost per scalar iteration is lower or
     higher: this_inside_cost / this_vf vs. other_inside_cost / other_vf.  */
  poly_int64 rel_this = this_loop_vinfo->vector_costs->body_cost () * other_vf;
  poly_int64 rel_other
    = other_loop_vinfo->vector_costs->body_cost () * this_vf;

  HOST_WIDE_INT est_rel_this_min
    = estimated_poly_value (rel_this, POLY_VALUE_MIN);
  HOST_WIDE_INT est_rel_this_max
    = estimated_poly_value (rel_this, POLY_VALUE_MAX);

  HOST_WIDE_INT est_rel_other_min
    = estimated_poly_value (rel_other, POLY_VALUE_MIN);
  HOST_WIDE_INT est_rel_other_max
    = estimated_poly_value (rel_other, POLY_VALUE_MAX);

  /* Check first if we can make out an unambigous total order from the minimum
     and maximum estimates.  */
  if (est_rel_this_min < est_rel_other_min
      && est_rel_this_max < est_rel_other_max)
    return -1;

  if (est_rel_other_min < est_rel_this_min
      && est_rel_other_max < est_rel_this_max)
    return 1;

  /* When other_loop_vinfo uses a variable vectorization factor,
     we know that it has a lower cost for at least one runtime VF.
     However, we don't know how likely that VF is.

     One option would be to compare the costs for the estimated VFs.
     The problem is that that can put too much pressure on the cost
     model.  E.g. if the estimated VF is also the lowest possible VF,
     and if other_loop_vinfo is 1 unit worse than this_loop_vinfo
     for the estimated VF, we'd then choose this_loop_vinfo even
     though (a) this_loop_vinfo might not actually be better than
     other_loop_vinfo for that VF and (b) it would be significantly
     worse at larger VFs.

     Here we go for a hacky compromise: pick this_loop_vinfo if it is
     no more expensive than other_loop_vinfo even after doubling the
     estimated other_loop_vinfo VF.  For all but trivial loops, this
     ensures that we only pick this_loop_vinfo if it is significantly
     better than other_loop_vinfo at the estimated VF.  */
  if (est_rel_other_min != est_rel_this_min
      || est_rel_other_max != est_rel_this_max)
    {
      HOST_WIDE_INT est_rel_this_likely
	= estimated_poly_value (rel_this, POLY_VALUE_LIKELY);
      HOST_WIDE_INT est_rel_other_likely
	= estimated_poly_value (rel_other, POLY_VALUE_LIKELY);

      return est_rel_this_likely * 2 <= est_rel_other_likely ? -1 : 1;
    }

  return 0;
}

/* A <=>-style subroutine of better_main_loop_than_p, used when there is
   nothing to choose between the inside (loop body) costs of THIS and OTHER.
   Check whether we can determine the return value of better_main_loop_than_p
   by comparing the outside (prologue and epilogue) costs of THIS and OTHER.
   Return:

   * -1 if better_main_loop_than_p should return true.
   * 1 if better_main_loop_than_p should return false.
   * 0 if we can't decide.  */

int
vector_costs::compare_outside_loop_cost (const vector_costs *other) const
{
  auto this_outside_cost = this->outside_cost ();
  auto other_outside_cost = other->outside_cost ();
  if (this_outside_cost != other_outside_cost)
    return this_outside_cost < other_outside_cost ? -1 : 1;

  return 0;
}
