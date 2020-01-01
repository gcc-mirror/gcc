/* atomic.c -- Builtins for HSAIL atomic instructions for which
   there is no feasible direct gcc GENERIC expression.

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

#include <stdint.h>
#include <stdio.h>

#define DO_ATOMICALLY(T, OPERATION)					\
  int done = 0;								\
  T old_value;								\
  T new_value;								\
  while (!done)								\
    {									\
      old_value = *ptr;							\
      new_value = OPERATION;						\
      done = __sync_bool_compare_and_swap (ptr, old_value, new_value);	\
    }									\
  return old_value

int32_t
__hsail_atomic_min_s32 (int32_t *ptr, int32_t a)
{
  DO_ATOMICALLY (int32_t, (old_value < a) ? old_value : a);
}

int64_t
__hsail_atomic_min_s64 (int64_t *ptr, int64_t a)
{
  DO_ATOMICALLY (int64_t, (old_value < a) ? old_value : a);
}

uint32_t
__hsail_atomic_min_u32 (uint32_t *ptr, uint32_t a)
{
  DO_ATOMICALLY (uint32_t, (old_value < a) ? old_value : a);
}

uint64_t
__hsail_atomic_min_u64 (uint64_t *ptr, uint64_t a)
{
  DO_ATOMICALLY (uint64_t, (old_value < a) ? old_value : a);
}

uint32_t
__hsail_atomic_max_u32 (uint32_t *ptr, uint32_t a)
{
  DO_ATOMICALLY (uint32_t, (old_value > a) ? old_value : a);
}

int32_t
__hsail_atomic_max_s32 (int32_t *ptr, int32_t a)
{
  DO_ATOMICALLY (int32_t, (old_value > a) ? old_value : a);
}

uint64_t
__hsail_atomic_max_u64 (uint64_t *ptr, uint64_t a)
{
  DO_ATOMICALLY (uint64_t, (old_value > a) ? old_value : a);
}

int64_t
__hsail_atomic_max_s64 (int64_t *ptr, int64_t a)
{
  DO_ATOMICALLY (int64_t, (old_value > a) ? old_value : a);
}

uint32_t
__hsail_atomic_wrapinc_u32 (uint32_t *ptr, uint32_t a)
{
  DO_ATOMICALLY (uint32_t, (old_value >= a) ? 0 : (old_value + 1));
}

uint64_t
__hsail_atomic_wrapinc_u64 (uint64_t *ptr, uint64_t a)
{
  DO_ATOMICALLY (uint64_t, (old_value >= a) ? 0 : (old_value + 1));
}

uint32_t
__hsail_atomic_wrapdec_u32 (uint32_t *ptr, uint32_t a)
{
  DO_ATOMICALLY (uint32_t,
		 ((old_value == 0) || (old_value > a)) ? a : (old_value - 1));
}

uint64_t
__hsail_atomic_wrapdec_u64 (uint64_t *ptr, uint64_t a)
{
  DO_ATOMICALLY (uint64_t,
		 ((old_value == 0) || (old_value > a)) ? a : (old_value - 1));
}
