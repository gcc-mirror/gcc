/* Copyright (C) 2012-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/**
 * @file gupcr_main.c
 * GUPC Portals4 runtime main program.
 */
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_sup.h"
#include "gupcr_lock_sup.h"
#include "gupcr_alloc.h"
#include "gupcr_broadcast.h"
#include "gupcr_barrier.h"
#include "gupcr_shutdown.h"
#include "gupcr_utils.h"
#include "gupcr_portals.h"
#include "gupcr_runtime.h"
#include "gupcr_gmem.h"
#include "gupcr_node.h"
#include "gupcr_coll_sup.h"
#include "gupcr_atomic_sup.h"

/** User's main program */
extern int GUPCR_MAIN (int argc, char *argv[]);

/** The number of THREADS, as specified on the command line */
int THREADS = -1;

/** The current thread number (range: 0..THREADS-1) */
int MYTHREAD = -1;

/** OK to call finalize routines */
int gupcr_finalize_ok = 0;

/** Depth count used to implement the semantics of
   nested upc_forall statements.  */
int __upc_forall_depth;

/** The filename of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
const char *gupcr_err_filename;

/** The line number of the location where a runtime
   error was detected.  This is set by the various
   debug-enabled ('g') UPC runtime library routines.  */
unsigned int gupcr_err_linenum;

/**
 * @addtogroup INIT GUPCR Initialization
 * @{
 */

/**
 * Initialize UPC runtime.
 *
 * All hardware/software components of the Portals4 interface
 * are initialized in this routine.
 */
static void
gupcr_init (void)
{
  int run_threads_count;
  upc_shared_ptr_t heap_region_base;
  size_t heap_region_size;

  /* Initialize Runtime.  */
  if (gupcr_runtime_init ())
    {
      /* Report an error to stderr as the GUPC error reporting
	 is not initialized yet.  Note: all threads report
	 this error.  */
      fprintf (stderr, "Unable to initialize runtime.\n");
      abort ();
    }

  /* Get the thread number.  */
  MYTHREAD = gupcr_runtime_get_rank ();

  /* Set up debugging, tracing, statistics, and timing support.  */
  gupcr_utils_init ();

  /* Initialize Portals.  */
  gupcr_portals_init ();

  /* Validate program info.  */
  gupcr_validate_pgm_info ();

  run_threads_count = gupcr_get_threads_count ();

  /* THREADS == -1, dynamic number of threads
     THREADS != -1, static number of threads and
     number of running and compiled threads must match.  */
  if (THREADS == -1)
    THREADS = run_threads_count;
  else if (THREADS != run_threads_count)
    gupcr_abort_with_msg ("number of running threads (%d) is "
			  "not equal to compiled threads (%d)",
			  run_threads_count, THREADS);
  gupcr_assert (THREADS >= 1);

  /* Initialize the Portals Network Interface.  */
  gupcr_portals_ni_init ();

  /* Initialize this thread's multi-node tree position.  */
  gupcr_nodetree_setup ();

  /* Initialize various runtime components.  */
  gupcr_node_init ();
  gupcr_gmem_init ();
  gupcr_lock_init ();
  gupcr_barrier_init ();
  gupcr_broadcast_init ();
  gupcr_coll_init ();
  gupcr_atomic_init ();
  gupcr_shutdown_init ();

  GUPCR_PTS_SET_NULL_SHARED (heap_region_base);
  GUPCR_PTS_SET_THREAD (heap_region_base, MYTHREAD);
  GUPCR_PTS_SET_VADDR (heap_region_base, gupcr_gmem_heap_base_offset);
  heap_region_size = gupcr_gmem_heap_size;
  gupcr_alloc_init (heap_region_base, heap_region_size);

  /* Indicate that runtime initialization is complete.  */
  gupcr_init_complete ();

  /* It is ok to call the finalization routines.  */
  gupcr_finalize_ok = 1;
}

/**
 * UPC runtime finalization.
 *
 * All previously allocated Portals4 resources are released.
 */
void
gupcr_fini (void)
{
  gupcr_shutdown_fini ();
  gupcr_atomic_fini ();
  gupcr_broadcast_fini ();
  gupcr_barrier_fini ();
  gupcr_lock_fini ();
  gupcr_gmem_fini ();
  gupcr_node_fini ();
  gupcr_coll_fini ();
  gupcr_portals_ni_fini ();
  gupcr_portals_fini ();
  gupcr_runtime_fini ();
  gupcr_utils_fini ();
}

/**
 * Per thread initialization.
 *
 * The following pre-requisites must be met, before calling this routine:
 * - the runtime system has been initialized.
 * - the UPC heap manager has been initialized.
 *
 * The barrier that is executed subsequent to calling this
 * per thread initialization procedure and prior to
 * calling the main program ensures that the initialization
 * completes before the main program runs.
 */

static void
gupcr_per_thread_init (void)
{
  typedef void (*func_ptr_t) (void);
  extern func_ptr_t GUPCR_INIT_ARRAY_START[];
  extern func_ptr_t GUPCR_INIT_ARRAY_END[];
  const int n_init = (int) (GUPCR_INIT_ARRAY_END - GUPCR_INIT_ARRAY_START);
  int i;
  for (i = 0; i < n_init; ++i)
    {
      func_ptr_t init_func = GUPCR_INIT_ARRAY_START[i];
      /* Skip zero words, possibly introduced by a section marker,
         or by the linker.  */
      if (init_func)
	(*init_func) ();
    }
}

/** @} */

/**
 * UPC program exit.
 *
 * Calls to exit() are rewritten into calls to __upc_exit()
 * by a "#define" in <gcc-upc.h>. Simply perform a upc_barrier and
 * then exit the process.
 * @param [in] status Status code of exit
 * @ingroup GUPC-API GUPC Program Interface
 */
void
__upc_exit (int status)
{
  /* Mask off the top bit; it is used to indicate a global exit.  */
  const int exit_status = status & 0x7f;
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);
  exit (exit_status);
}

/**
 * Exit program with given status and terminate all other threads.
 *
 * A special "shutdown" PTE is used to send a signal to
 * other threads that they should exit.
 * @param [in] status Status code for return
 * @ingroup UPC-LIBRARY UPC Library Interface
 */
void
upc_global_exit (int status)
{
  /* Send exit signal to all other threads.  */
  gupcr_signal_exit (status);
  /* It is NOT ok to call the finalization routines as there might
     be outstanding Portals transactions.  */
  gupcr_finalize_ok = 0;
  exit (status);
}

/**
 * GUPC runtime start up.
 *
 * @param [in] argc Number of command line arguments
 * @param [in] argv Command line arguments
 * @retval Return (exit) value from the user's main program
 * @ingroup INIT GUPCR Initialization
 */
/** @}*/
int
GUPCR_START (int argc, char *argv[])
{
  int status;

  /* Install exit handler.  */
  atexit (gupcr_exit);

  /* Set program name for debug/trace diagnostics.  */
  gupcr_set_pgm_name (argv[0]);

  /* Initialize all runtime components.  */
  gupcr_init ();

  /* Initialize language specific variables.  */
  __upc_forall_depth = 0;

  /* Wait for all threads to finish initialization.  */
  gupcr_startup_barrier ();

  /* Perform per thread initialization.  */
  gupcr_per_thread_init ();

  /* Wait for all threads to complete per thread initialization.  */
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);

  /* Call user main program.  */
  status = GUPCR_MAIN (argc, argv);

  /* Wait for all threads to complete.  */
  __upc_barrier (GUPCR_RUNTIME_BARRIER_ID);

  return status;
}
