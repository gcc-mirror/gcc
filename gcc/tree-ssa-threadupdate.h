/* Communication between registering jump thread requests and
   updating the SSA/CFG for jump threading.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef _TREE_SSA_THREADUPDATE_H
#define _TREE_SSA_THREADUPDATE_H 1

/* In tree-ssa-threadupdate.c.  */
extern bool thread_through_all_blocks (bool);
enum jump_thread_edge_type
{
  EDGE_START_JUMP_THREAD,
  EDGE_COPY_SRC_BLOCK,
  EDGE_COPY_SRC_JOINER_BLOCK,
  EDGE_NO_COPY_SRC_BLOCK
};

class jump_thread_edge
{
public:
  jump_thread_edge (edge e, enum jump_thread_edge_type type)
    : e (e), type (type) {}

  edge e;
  enum jump_thread_edge_type type;
};

extern void register_jump_thread (vec <class jump_thread_edge *> *);
extern void delete_jump_thread_path (vec <class jump_thread_edge *> *);
#endif
