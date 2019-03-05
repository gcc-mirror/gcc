/* fbarrier.c -- HSAIL fbarrier built-ins.

   Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

#include <stdlib.h>
#include <signal.h>

#include "workitems.h"
#include "phsa-rt.h"

#ifdef HAVE_FIBERS
#include "fibers.h"

typedef fiber_barrier_t fbarrier;

void
__hsail_initfbar (uint32_t addr, PHSAWorkItem *wi)
{
  fbarrier *fbar = (fbarrier *) (wi->wg->group_base_ptr + addr);
  fbar->threshold = 0;
  fbar->reached = 0;
  fbar->waiting_count = 0;
}

void
__hsail_releasefbar (uint32_t addr, PHSAWorkItem *wi)
{
  fbarrier *fbar = (fbarrier *) (wi->wg->group_base_ptr + addr);
  fbar->threshold = 0;
  fbar->reached = 0;
  fbar->waiting_count = 0;
}

void
__hsail_joinfbar (uint32_t addr, PHSAWorkItem *wi)
{
  fbarrier *fbar = (fbarrier *) (wi->wg->group_base_ptr + addr);
  ++fbar->threshold;
}

void
__hsail_leavefbar (uint32_t addr, PHSAWorkItem *wi)
{
  fbarrier *fbar = (fbarrier *) (wi->wg->group_base_ptr + addr);
  --fbar->threshold;
}

void
__hsail_waitfbar (uint32_t addr, PHSAWorkItem *wi)
{
  fbarrier *fbar = (fbarrier *) (wi->wg->group_base_ptr + addr);
  fiber_barrier_reach (fbar);
}

void
__hsail_arrivefbar (uint32_t addr, PHSAWorkItem *wi)
{
  fbarrier *fbar = (fbarrier *) (wi->wg->group_base_ptr + addr);
  ++fbar->reached;
  if (fbar->reached == fbar->threshold)
    fbar->reached = 0;
}

#endif

