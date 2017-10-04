/* workitems.h -- Types for context data passed as hidden parameters to special
   built-ins.

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

#ifndef PHSA_RT_WORKITEMS_H
#define PHSA_RT_WORKITEMS_H

/* As the simple fibers implementation relies only on ucontext, we can
   assume is found by default as it is part of glibc.  However, for partial
   HSAIL support on platforms without having it available, the following define
   can be undefined.  */
#define HAVE_FIBERS

#ifdef HAVE_FIBERS
#include "fibers.h"
#endif

#include <stdint.h>
#include "phsa-rt.h"

/* Data identifying a single work-group instance.  */

typedef struct
{
  /* The group id of the currently executed WG.  */
  size_t x;
  size_t y;
  size_t z;

  /* This is 1 in case there are more work groups to execute.
     If 0, the work-item threads should finish themselves.  */
  int more_wgs;

  /* If the local size does not evenly divide the grid size, will have
     leftover WIs in the last execution.  */
  int leftover_wg;
  int last_wg;

  /* (Flat) pointer to the beginning of the group segment allocated
     to the work-group.  */
  void *group_base_ptr;

  /* The offset in the group memory for the kernel local group variables.
     To support module scope group variables, there might be need to preseve
     room for them in the beginning of the group segment.  */
  uint32_t initial_group_offset;

  /* Similarly to the private segment that gets space allocated for all
     WIs in the work-group.  */
  void *private_base_ptr;
  uint32_t private_segment_total_size;

  /* The first flat address of the group segment allocated for
     the given work group.  */
  uint64_t group_segment_base_addr;

  /* Offset from the beginning of the private segment to the start of
     the previously allocated chunk of dynamic work-item memory (alloca)
     by any WI in the WG.

     Initially set to private_segment_total_size to denote no dynamic
     allocations have been made.  The dynamic allocations are done downwards
     from the private segment end.  */
  uint32_t alloca_stack_p;
  /* The position of the first word in the current function's alloca
     stack frame.  Initialized to point outside the private segment.  */
  uint32_t alloca_frame_p;

} PHSAWorkGroup;

/* Data identifying a single work-item, passed to the work-item thread in case
   of a fiber based work-group execution.  */

typedef struct
{
  PHSAKernelLaunchData *launch_data;
  /* Identifies and keeps book of the currently executed WG of the WI swarm.  */
  volatile PHSAWorkGroup *wg;
  /* The local id of the current WI.  */
  size_t x;
  size_t y;
  size_t z;
#ifdef HAVE_FIBERS
  fiber_t fiber;
#endif
} PHSAWorkItem;


#endif
