/* misc.c -- Builtins for HSAIL misc instructions.

   Copyright (C) 2015-2017 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <stdint.h>
#include <time.h>

#include "workitems.h"

/* Return the monotonic clock as nanoseconds.  */

uint64_t
__hsail_clock ()
{
  struct timespec t;
  clock_gettime (CLOCK_MONOTONIC, &t);
  return (uint64_t) t.tv_sec * 1000000000 + (uint64_t) t.tv_nsec;
}

uint32_t
__hsail_cuid (PHSAWorkItem *wi)
{
  /* All WIs are executed with a single compute unit (core/thread)
     for now.  */
  return 0;
}

uint32_t
__hsail_maxcuid (PHSAWorkItem *wi)
{
  /* All WIs are executed with a single compute unit (core/thread)
     for now.  */
  return 0;
}

void
__hsail_debugtrap (uint32_t src, PHSAWorkItem *wi)
{
  /* Could we produce a SIGTRAP signal here to drop to gdb
     console, or similar?  In any case, the execution of the
     kernel should halt.
  */
  return;
}

uint32_t
__hsail_groupbaseptr (PHSAWorkItem *wi)
{
  return (uint32_t) (uintptr_t) (wi->wg->group_base_ptr
				 - wi->launch_data->group_segment_start_addr);
}

uint64_t
__hsail_kernargbaseptr_u64 (PHSAWorkItem *wi)
{
  /* For now assume only a single kernarg allocation at a time.
     Proper kernarg memory management to do.  */
  return (uint64_t) (uintptr_t) wi->launch_data->kernarg_addr;
}

uint32_t
__hsail_kernargbaseptr_u32 (PHSAWorkItem *wi)
{
  /* For now assume only a single kernarg allocation at a time.
     Proper kernarg memory management to do.  */
  return 0;
}
