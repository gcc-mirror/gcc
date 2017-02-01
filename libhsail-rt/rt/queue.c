/* queue.c -- Builtins for HSAIL queue related instructions.

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

#include "phsa-queue-interface.h"

uint64_t
__hsail_ldqueuereadindex (uint64_t queue_addr)
{
  phsa_queue_t *queue = (phsa_queue_t *) (uintptr_t) queue_addr;
  return queue->read_index;
}

uint64_t
__hsail_ldqueuewriteindex (uint64_t queue_addr)
{
  phsa_queue_t *queue = (phsa_queue_t *) (uintptr_t) queue_addr;
  return queue->write_index;
}

uint64_t
__hsail_addqueuewriteindex (uint64_t queue_addr, uint64_t value)
{
  phsa_queue_t *queue = (phsa_queue_t *) (uintptr_t) queue_addr;
  return __sync_fetch_and_add (&queue->write_index, value);
}

uint64_t
__hsail_casqueuewriteindex (uint64_t queue_addr, uint64_t cmp_value,
				   uint64_t new_value)
{
  phsa_queue_t *queue = (phsa_queue_t *) (uintptr_t) queue_addr;
  return __sync_val_compare_and_swap (&queue->write_index, cmp_value,
				      new_value);
}

void
__hsail_stqueuereadindex (uint64_t queue_addr, uint64_t value)
{
  phsa_queue_t *queue = (phsa_queue_t *) (uintptr_t) queue_addr;
  queue->read_index = value;
}

void
__hsail_stqueuewriteindex (uint64_t queue_addr, uint64_t value)
{
  phsa_queue_t *queue = (phsa_queue_t *) (uintptr_t) queue_addr;
  queue->write_index = value;
}
