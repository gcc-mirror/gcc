/* segment.c -- Builtins for HSAIL segment related instructions.

   Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#include "workitems.h"

uint32_t
__hsail_segmentp_private (uint64_t flat_addr, PHSAWorkItem *wi)
{
  if (flat_addr == 0)
    return 1;
  else
    return ((void *) (uintptr_t) flat_addr >= wi->wg->private_base_ptr
	    && ((void *) (uintptr_t) flat_addr
		< (wi->wg->private_base_ptr
		   + wi->wg->private_segment_total_size)));
}

uint32_t
__hsail_segmentp_group (uint64_t flat_addr, PHSAWorkItem *wi)
{
  if (flat_addr == 0)
    return 1;
  else
    return ((void *) (uintptr_t) flat_addr >= wi->wg->group_base_ptr
	    && ((void *) (uintptr_t) flat_addr
		< (wi->wg->group_base_ptr
		   + wi->launch_data->dp->group_segment_size)));
}

uint32_t
__hsail_segmentp_global (uint64_t flat_addr, PHSAWorkItem *wi)
{
  return (flat_addr == 0
	  || (!__hsail_segmentp_private (flat_addr, wi)
	      && !__hsail_segmentp_group (flat_addr, wi)));
}
