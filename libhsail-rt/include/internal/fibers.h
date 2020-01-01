/* fibers.h -- an extremely simple lightweight thread (fiber) implementation
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

#ifndef PHSA_RT_FIBERS_H
#define PHSA_RT_FIBERS_H

#include <ucontext.h>

typedef enum
{
  /* Ready to run.  */
  FIBER_STATUS_READY,
  /* Exited by calling fiber_thread_exit.  */
  FIBER_STATUS_EXITED,
  /* Joined by the main thread.  */
  FIBER_STATUS_JOINED
} fiber_status_t;

/* A light weight thread (fiber).  */
struct fiber_s
{
  ucontext_t context;
  volatile fiber_status_t status;
  struct fiber_s *next;
  struct fiber_s *prev;
};

typedef struct fiber_s fiber_t;

typedef void (*fiber_function_t)(int, int);

/* Initializes the fiber with the start function given as the first
   argument, and the argument to pass to the start function,
   as the second.  The allocated stack size is given as the last argument.  */
void
fiber_init (fiber_t *fiber, fiber_function_t start_function, void *arg,
	    size_t stack_size, size_t stack_align);

/* Terminates the fiber execution from within the fiber itself.  */
void
fiber_exit ();

/* Blocks until the given fiber returns.  Frees the resources allocated
   for the fiber.  After join returns, the fiber itself can be deleted.  */
void
fiber_join (fiber_t *fiber);

/* Co-operatively transfer execution turn to other fibers.  */
void
fiber_yield ();

/* A multi-entry barrier.  After the last fiber has reached the
   barrier, it is automatically re-initialized to the threshold.  */
typedef struct
{
  /* The barrier participant count.  */
  volatile size_t threshold;
  /* Number of fibers that have reached the barrier.  */
  volatile size_t reached;
  /* Number of fibers that are waiting at the barrier.  */
  volatile size_t waiting_count;
} fiber_barrier_t;

/* Reach the given barrier.  Blocks (co-operatively switches the execution
   fibers) until all other parties have reached it.  Returns 0 only in case
   the calling fiber was the first one to return from the barrier.  */
size_t
fiber_barrier_reach (fiber_barrier_t *barrier);

/* Initializes the given barrier.  */
void
fiber_barrier_init (fiber_barrier_t *barrier, size_t threshold);

void *
fiber_int_args_to_ptr (int arg0, int arg1);

#endif
