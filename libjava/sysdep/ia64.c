/* Copyright (C) 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with other files,
   some of which are compiled with GCC, to produce an executable,
   this library does not by itself cause the resulting executable
   to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include <stddef.h>
#include <memory.h>

#define IA64_UNWIND_INFO
#include "ia64-frame.h"

static int 
ia64_backtrace_helper (void **array, void *throw_pc, 
		       ia64_frame_state *throw_frame,
		       ia64_frame_state *frame, void *bsp, int size)
{
  void *pc = NULL;
  int frame_count = 0;
  unwind_info_ptr *info;

  asm volatile ("flushrs");  /*  Make the local register stacks available.  */
    
  /* Start at our stack frame, get our state.  */
  info = build_ia64_frame_state (throw_pc, throw_frame, bsp, NULL);

  memcpy (frame, throw_frame, sizeof (*frame));

  while (info && frame_count < size)
    {
      pc = array[frame_count++] = get_real_reg_value (&frame->rp);
      --pc;
      bsp = calc_caller_bsp 
	((long)get_real_reg_value (&frame->pfs), frame->my_bsp);
      info = build_ia64_frame_state (pc, frame, bsp, NULL);
      if (frame->rp.loc_type == IA64_UNW_LOC_TYPE_NONE) /* We've finished. */
	break;
    }

  return frame_count;
}
  
int
_Jv_ia64_backtrace (void **array, int size)
{
  ia64_frame_state my_frame;
  ia64_frame_state originator;	/* For the context handler is in.  */
  void *bsp;
 
  /* Do any necessary initialization to access arbitrary stack frames.
     This forces gcc to save memory in our stack frame for saved
     registers. */
  __builtin_unwind_init ();

label_ia64:
  bsp = __builtin_ia64_bsp ();
  
  return ia64_backtrace_helper (array, &&label_ia64, &my_frame, 
				&originator, bsp, size);
}
