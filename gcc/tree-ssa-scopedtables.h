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
  const_and_copies (FILE *, int);
  ~const_and_copies (void) { stack.release (); }
  void push_marker (void) { stack.safe_push (NULL_TREE); }
  void pop_to_marker (void);
  void record_const_or_copy (tree, tree);
  void record_const_or_copy (tree, tree, tree);
  void invalidate (tree);

 private:
  vec<tree> stack;
  FILE *dump_file;
  int dump_flags;
};

#endif /* GCC_TREE_SSA_SCOPED_TABLES_H */
