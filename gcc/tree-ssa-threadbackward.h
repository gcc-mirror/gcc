/* Header file for the SSA backward jump threader.
   Copyright (C) 2026 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldy@quesejoda.com>.

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

#ifndef GCC_TREE_SSA_THREADBACKWARD_H
#define GCC_TREE_SSA_THREADBACKWARD_H

class gimple_ranger;
class path_range_query;
class back_threader_profitability;

// Path registry for the backwards threader.  After all paths have been
// registered with register_path(), thread_through_all_blocks() is called
// to modify the CFG.

class back_threader_registry : public back_jt_path_registry
{
public:
  bool register_path (const vec<basic_block> &, edge taken);
};

// Back threader flags.
#define BT_NONE 0
// Generate fast code at the expense of code size.
#define BT_SPEED 1
// Resolve unknown SSAs on entry to a threading path.  If set, use the
// ranger.  If not, assume all ranges on entry to a path are VARYING.
#define BT_RESOLVE 2

class back_threader
{
public:
  back_threader (function *fun, unsigned flags, bool first);
  virtual ~back_threader ();
  unsigned thread_blocks ();
  void search ();
  unsigned apply ();
private:
  void maybe_thread_block (basic_block bb);
  bool debug_counter ();
  edge maybe_register_path (back_threader_profitability &);
  void maybe_register_path_dump (edge taken_edge);
  void find_paths_to_names (basic_block bb, bitmap imports, unsigned,
			    back_threader_profitability &);
  edge find_taken_edge (const vec<basic_block> &path);
  edge find_taken_edge_cond (const vec<basic_block> &path, gcond *);
  edge find_taken_edge_switch (const vec<basic_block> &path, gswitch *);
  edge find_taken_edge_goto (const vec<basic_block> &path, ggoto *);
  virtual void debug ();
  virtual void dump (FILE *out);

  back_threader_registry m_registry;

  // Current path being analyzed.
  auto_vec<basic_block> m_path;
  // Flag to mark visited BBs while analyzing a path.
  auto_bb_flag m_visited_flag;
  // The set of SSA names, any of which could potentially change the
  // value of the final conditional in a path.
  auto_bitmap m_imports;
  // The last statement in the path.
  gimple *m_last_stmt;
  // Marker to differentiate unreachable edges.
  static const edge UNREACHABLE_EDGE;
  // Set to TRUE if unknown SSA names along a path should be resolved
  // with the ranger.  Otherwise, unknown SSA names are assumed to be
  // VARYING.  Setting to true is more precise but slower.
  function *m_fun;
  // Ranger for the path solver.
  gimple_ranger *m_ranger;
  // Path solver, reused across all candidate paths.
  path_range_query *m_solver;
  unsigned m_flags;
  // Set to TRUE for the first of each thread[12] pass or the first of
  // each threadfull[12] pass.  This is used to differentiate between
  // the different threading passes so we can set up debug counters.
  bool m_first;
};

#endif // GCC_TREE_SSA_THREADBACKWARD_H
