/* Copyright (C) 2010, 2011 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libitm_i.h"

namespace GTM HIDDEN {

// Filter out any updates that overlap the libitm stack, as defined by
// TOP (entry point to library) and BOT (below current function).  This
// definition should be fine for all stack-grows-down architectures.

gtm_cacheline_mask __attribute__((noinline))
gtm_mask_stack(gtm_cacheline *line, gtm_cacheline_mask mask)
{
  void *top = gtm_thr()->jb.cfa;
  void *bot = __builtin_dwarf_cfa();

  // We must have come through an entry point that set TOP.
  assert (top != NULL);

  if (line + 1 < bot)
    {
      // Since we don't have the REAL stack boundaries for this thread,
      // we cannot know if this is a dead write to a stack address below
      // the current function or if it is write to another VMA.  In either
      // case allowing the write should not affect correctness.
    }
  else if (line >= top)
    {
      // A valid write to an address in an outer stack frame, or a write
      // to another VMA.
    }
  else
    {
      uintptr_t diff = (uintptr_t)top - (uintptr_t)line;
      if (diff >= CACHELINE_SIZE)
	{
	  // The write is either fully within the proscribed area, or the tail
	  // of the cacheline overlaps the proscribed area.  Assume that all
	  // stacks are at least cacheline aligned and declare the head of the
	  // cacheline dead.
	  mask = 0;
	}
      else
	{
	  // The head of the cacheline is within the proscribed area, but the
	  // tail of the cacheline is live.  Eliminate the dead writes.
	  mask &= (gtm_cacheline_mask)-1 << diff;
	}
    }

  return mask;
}

} // namespace GTM
