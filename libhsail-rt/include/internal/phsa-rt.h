/* phsa-rt.h -- Data structures and functions of the PHSA device side runtime
   scheduler, and HSAIL built-ins.

   Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

#ifndef PHSA_RT_H
#define PHSA_RT_H

#include <stdbool.h>
#include <stdint.h>
#include "hsa.h"

#define PHSA_MAX_WG_SIZE 1024 * 10

/* Pointer type for the public facing kernel launcher function generated
   by gccbrig.  This launches the actual kernel for all work groups and
   work items in the grid.  */
typedef void (*gccbrigKernelLauncherFunc) (void *context, void *);

/* Pointer type for kernel functions produced by gccbrig from the HSAIL.
   This is private from outside the device binary and only called by
   the launcher.  */
typedef void (*gccbrigKernelFunc) (unsigned char *, void *, void *, uint32_t,
				   void *);

/* Context data that is passed to the kernel function, initialized
   by the runtime to the current launch information.  The data is
   used by different id functions etc.

   The struct is used by both the launcher and the targeted device,
   thus the fields must have the same alignment/padding in both sides.
*/
typedef struct
{
  /* Data set by the HSA Runtime's kernel launcher.  */
  hsa_kernel_dispatch_packet_t *dp;

  size_t packet_id;

  /* Data set by the device-side launcher.  */
  gccbrigKernelFunc kernel;

  /* The range of a work groups this dispatch should execute.  */
  size_t wg_min_x;
  size_t wg_min_y;
  size_t wg_min_z;

  size_t wg_max_x;
  size_t wg_max_y;
  size_t wg_max_z;

  /* The barrier used to synch the work-items before executing a new WG.  */
  void *wg_start_barrier;

  /* The barrier to wait at after executing a work-group.  */
  void *wg_completion_barrier;

  /* The barrier used to synchronize WIs in case of the 'barrier' HSAIL
     instruction.  */
  void *wg_sync_barrier;

  /* This should be set to the flat address of the beginning of the group
     segment.  */
  size_t group_segment_start_addr;

  /* This must be set to the correct aligned flat address space location from
     where the kernel can actually read its arguments.  Might point to the
     original global kernarg space.  */
  void *kernarg_addr;
} PHSAKernelLaunchData;

#endif
