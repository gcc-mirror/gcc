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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "tree-pretty-print.h"
#include "tree-pass.h"
#include "tree-ssa-scopedtables.h"
#include "tree-ssa-threadedge.h"

const_and_copies::const_and_copies (FILE *file, int flags)
{
  stack.create (20);
  dump_file = file;
  dump_flags = flags;
}

/* Pop entries off the stack until we hit the NULL marker.
   For each entry popped, use the SRC/DEST pair to restore
   SRC to its prior value.  */

void
const_and_copies::pop_to_marker (void)
{
  while (stack.length () > 0)
    {
      tree prev_value, dest;

      dest = stack.pop ();

      /* A NULL value indicates we should stop unwinding, otherwise
	 pop off the next entry as they're recorded in pairs.  */
      if (dest == NULL)
	break;

      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "<<<< COPY ");
	  print_generic_expr (dump_file, dest, 0);
	  fprintf (dump_file, " = ");
	  print_generic_expr (dump_file, SSA_NAME_VALUE (dest), 0);
	  fprintf (dump_file, "\n");
	}

      prev_value = stack.pop ();
      set_ssa_name_value (dest, prev_value);
    }
}

/* Record that X has the value Y.  */

void
const_and_copies::record_const_or_copy (tree x, tree y)
{
  record_const_or_copy (x, y, SSA_NAME_VALUE (x));
}

/* Record that X has the value Y and that X's previous value is PREV_X.  */

void
const_and_copies::record_const_or_copy (tree x, tree y, tree prev_x)
{
  /* Y may be NULL if we are invalidating entries in the table.  */
  if (y && TREE_CODE (y) == SSA_NAME)
    {
      tree tmp = SSA_NAME_VALUE (y);
      y = tmp ? tmp : y;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "0>>> COPY ");
      print_generic_expr (dump_file, x, 0);
      fprintf (dump_file, " = ");
      print_generic_expr (dump_file, y, 0);
      fprintf (dump_file, "\n");
    }

  set_ssa_name_value (x, y);
  stack.reserve (2);
  stack.quick_push (prev_x);
  stack.quick_push (x);
}

/* A new value has been assigned to LHS.  If necessary, invalidate any
   equivalences that are no longer valid.   This includes invaliding
   LHS and any objects that are currently equivalent to LHS.

   Finding the objects that are currently marked as equivalent to LHS
   is a bit tricky.  We could walk the ssa names and see if any have
   SSA_NAME_VALUE that is the same as LHS.  That's expensive.

   However, it's far more efficient to look at the unwinding stack as
   that will have all context sensitive equivalences which are the only
   ones that we really have to worry about here.   */
void
const_and_copies::invalidate (tree lhs)
{

  /* The stack is an unwinding stack.  If the current element is NULL
     then it's a "stop unwinding" marker.  Else the current marker is
     the SSA_NAME with an equivalence and the prior entry in the stack
     is what the current element is equivalent to.  */
  for (int i = stack.length() - 1; i >= 0; i--)
    {
      /* Ignore the stop unwinding markers.  */
      if ((stack)[i] == NULL)
	continue;

      /* We want to check the current value of stack[i] to see if
	 it matches LHS.  If so, then invalidate.  */
      if (SSA_NAME_VALUE ((stack)[i]) == lhs)
	record_const_or_copy ((stack)[i], NULL_TREE);

      /* Remember, we're dealing with two elements in this case.  */
      i--;
    }

  /* And invalidate any known value for LHS itself.  */
  if (SSA_NAME_VALUE (lhs))
    record_const_or_copy (lhs, NULL_TREE);
}
