/* Header file for SSA dominator optimizations.
   Copyright (C) 2013-2015 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSA_SCOPED_TABLES_H
#define GCC_TREE_SSA_SCOPED_TABLES_H

class const_and_copies
{
 public:
  const_and_copies (void) { stack.create (20); };
  ~const_and_copies (void) { stack.release (); }

  /* Push the unwinding marker onto the stack.  */
  void push_marker (void) { stack.safe_push (NULL_TREE); }

  /* Restore the const/copies table to its state when the last marker
     was pushed.  */
  void pop_to_marker (void);

  /* Record a single const/copy pair that can be unwound.  */
  void record_const_or_copy (tree, tree);

  /* Special entry point when we want to provide an explicit previous
     value for the first argument.  Try to get rid of this in the future.  */
  void record_const_or_copy (tree, tree, tree);

  /* When threading we need to invalidate certain equivalences after
     following a loop backedge.  The entries we need to invalidate will
     always be in this unwindable stack.  This entry point handles
     finding and invalidating those entries.  */
  void invalidate (tree);

 private:
  vec<tree> stack;
};

#endif /* GCC_TREE_SSA_SCOPED_TABLES_H */
