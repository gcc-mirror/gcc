/* Header file for SSA jump threading.
   Copyright (C) 2013-2021 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSA_THREADEDGE_H
#define GCC_TREE_SSA_THREADEDGE_H

// Class used to maintain path state in the jump threader and pass it
// to the jump threader simplifier.

class jt_state
{
public:
  jt_state (class const_and_copies *,
	    class avail_exprs_stack *,
	    class evrp_range_analyzer *);
  void push (edge);
  void pop ();
  void register_equiv (tree dest, tree src, bool update_range = false);
  void register_equivs_on_edge (edge e);
  void record_ranges_from_stmt (gimple *stmt, bool temporary);

private:
  const_and_copies *m_copies;
  avail_exprs_stack *m_exprs;
  evrp_range_analyzer *m_evrp;
};

// This is the high level threader.  The entry point is
// thread_outgoing_edges(), which calculates and registers paths to be
// threaded.  When all candidates have been registered,
// thread_through_all_blocks() is called to actually change the CFG.

class jump_threader
{
public:
  jump_threader (class jump_threader_simplifier *, class jt_state *);
  ~jump_threader ();
  void thread_outgoing_edges (basic_block);
  void remove_jump_threads_including (edge_def *);
  bool thread_through_all_blocks (bool may_peel_loop_headers);

private:
  tree simplify_control_stmt_condition (edge, gimple *);
  tree simplify_control_stmt_condition_1 (edge,
					  gimple *,
					  tree op0,
					  tree_code cond_code,
					  tree op1,
					  unsigned limit);

  bool thread_around_empty_blocks (vec<class jump_thread_edge *> *path,
				   edge, bitmap visited);
  int thread_through_normal_block (vec<jump_thread_edge *> *path,
				   edge, bitmap visited);
  void thread_across_edge (edge);
  bool record_temporary_equivalences_from_phis (edge);
  gimple *record_temporary_equivalences_from_stmts_at_dest (edge);

  // Dummy condition to avoid creating lots of throw away statements.
  gcond *dummy_cond;

  class jump_thread_path_registry *m_registry;
  jump_threader_simplifier *m_simplifier;
  jt_state *m_state;
};

// Statement simplifier callback for the jump threader.

class jump_threader_simplifier
{
public:
  jump_threader_simplifier (class vr_values *v);
  virtual ~jump_threader_simplifier () { }
  virtual tree simplify (gimple *, gimple *, basic_block, jt_state *);

protected:
  vr_values *m_vr_values;
};

extern void propagate_threaded_block_debug_into (basic_block, basic_block);
extern bool single_succ_to_potentially_threadable_block (basic_block);

// ?? All this ssa_name_values stuff is the store of values for
// avail_exprs_stack and const_and_copies, so it really belongs in the
// jump_threader class.  However, it's probably not worth touching
// this, since all this windable state is slated to go with the
// ranger.
extern vec<tree> ssa_name_values;
#define SSA_NAME_VALUE(x) \
    (SSA_NAME_VERSION (x) < ssa_name_values.length () \
     ? ssa_name_values[SSA_NAME_VERSION (x)] \
     : NULL_TREE)
extern void set_ssa_name_value (tree, tree);

#endif /* GCC_TREE_SSA_THREADEDGE_H */
