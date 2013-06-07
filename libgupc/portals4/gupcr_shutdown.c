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
 * @file gupcr_shutdown.c
 * GUPC Portals4 shutdown support.
 *
 * Support for upc_global_exit ().
 *
 * Each UPC thread (process) creates a helper (shutdown) pthread
 * with the sole purpose of waiting for receipt of a remote request
 * to shutdown, as a result of the other thread issuing a call
 * to upc_global_exit.
 *
 * This pthread uses a special PTE/LE (GUPCR_PTL_PTE_SHUTDOWN) to receive a
 * global exit code from another UPC thread.  A simple PtlPut of the
 * exit code issued to the shutdown PTE on some other UPC thread
 * triggers exit of the receiving thread.
 *
 * The following steps are taken to initialize, wait, and signal
 * the UPC global exit:
 *
 * - Each thread initializes a PTE/LE to receive an exit code that
 *     was passed in as the argument to upc_global_exit().
 * - Each thread creates a helper pthread - gupcr_shutdown_pthread()
 *     that waits on the shutdown LE's counting event (one count only).
 * - The main UPC thread installs a signal handler for SHUTDWON_SIGNAL
 *     that is used by the shutdown pthread to signal a need for
 *     global exit.
 * - Remote shutdown takes the following steps:
 *     -# A UPC thread executing a call to upc_global_exit() sends the
 *        exit code to all other UPC threads by using the shutdown PTE.
 *     -# The pthread associated with each UPC thread receives the
 *        exit code and returns from the counting event Portals
 *        wait call.
 *     -# The receiving pthread sends the SHUTDOWN_SIGNAL to the main
 *     	  UPC thread and calls pthread_exit().
 *     -# The main UPC thread receives the signal, which invokes
 *        the signal handler.
 *     -# The signal handler waits for the shutdown pthread to exit,
 *        and then calls exit() with the code received from
 *        the thread that sent the shutdown request.  The invoking thread
 *        also waits for ACKs from the first step with the configured timeout,
 *
 *  @note
 *  -# The gupcr_exit() function is registered with atexit()
 *     and will be executed when exit() is called.
 *  -# Upon regular exit, the main UPC thread disables the
 *     SHUTDOWN_SIGNAL signal, and terminates the shutdown pthread
 *     by writing a dummy value using its own shutdown PTE.
 *
 * @addtogroup SHUTDOWN GUPCR Shutdown Functions
 * @{
 */

#include <pthread.h>
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_portals.h"
#include "gupcr_shutdown.h"

/** Shutdown signal to main thread */
#define SHUTDOWN_SIGNAL SIGUSR2
/** Shutdown check interval (100 miliseconds) */
#define SHUTDOWN_MICROSEC_WAIT 100000L

/** Shutdown LE buffer */
static int gupcr_shutdown_status;
/** Shutdown LE handle */
static ptl_handle_le_t gupcr_shutdown_le;
/** Shutdown LE event counter */
static ptl_handle_ct_t gupcr_shutdown_le_ct;

/** Shutdown memory buffer for sending data */
static int gupcr_shutdown_send_status;
/** Shutdown memory MD handle */
static ptl_handle_md_t gupcr_shutdown_md;
/** Shutdown memory MD event counter */
static ptl_handle_ct_t gupcr_shutdown_md_ct;
/** Shutdown memory MD event queue */
static ptl_handle_ct_t gupcr_shutdown_md_eq;

/** Shutdown pthread ID */
static pthread_t gupcr_shutdown_pthread_id;
/** Shutdown pthread declaration */
static void *gupcr_shutdown_pthread (void *arg) __attribute__ ((noreturn));

/**
 * Send a remote shutdown request to all threads.
 *
 * Wait for the thread's pthread and ACks from sending
 * messages to other threads (with timeout).
 *
 * @param [in] status exit code passed to other threads
 */
void
gupcr_signal_exit (int status)
{
  int thread;
  int wait_cnt = GUPCR_GLOBAL_EXIT_TIMEOUT *
    (1000000L / SHUTDOWN_MICROSEC_WAIT);
  int done = 0;
  ptl_process_t pid;

  /* Protect local global exit from remote shutdown request.  */
  gupcr_signal_disable (SHUTDOWN_SIGNAL);

  gupcr_shutdown_send_status = status;
  /* Send global exit code to all threads.  */
  for (thread = 0; thread < THREADS; thread++)
    {
      pid.rank = thread;
      gupcr_portals_call (PtlPut, (gupcr_shutdown_md, 0,
				   sizeof (gupcr_shutdown_send_status),
				   PTL_CT_ACK_REQ, pid,
				   GUPCR_PTL_PTE_SHUTDOWN, PTL_NO_MATCH_BITS,
				   0, PTL_NULL_USER_PTR, PTL_NULL_HDR_DATA));
    }
  /* It is NOT ok to call finalize routines as there might
     be outstanding transactions.  */
  gupcr_finalize_ok = 0;
  /* Wait for our own shutdown pthread to complete.  */
  pthread_join (gupcr_shutdown_pthread_id, NULL);
  /* Wait for ACKs from all threads.  It should happen quickly
     if everything is ok, otherwise timeout after configured
     number of seconds.  */
  do
    {
      ptl_ct_event_t ct = { 0, 0 };
      gupcr_portals_call (PtlCTGet, (gupcr_shutdown_md_ct, &ct));
      if ((ct.success + ct.failure) == (ptl_size_t) THREADS)
	done = 1;
      else
	gupcr_cpu_delay (SHUTDOWN_MICROSEC_WAIT);
    }
  while (!done && wait_cnt--);
}

/**
 * Terminate shutdown pthread
 *
 * To terminate the local shutdown pthread a dummy value must
 * be sent by this thread to its own shutdown PTE.  The main thread
 * then waits for pthread to exit.
 */
static void
gupcr_shutdown_terminate_pthread (void)
{
  ptl_process_t pid;
  pid.rank = MYTHREAD;

  /* Disable interrupts before sending a signal to
     shutdown pthread.  */
  gupcr_signal_disable (SHUTDOWN_SIGNAL);

  gupcr_shutdown_send_status = 0;
  gupcr_portals_call (PtlPut, (gupcr_shutdown_md, 0,
			       sizeof (gupcr_shutdown_send_status),
			       PTL_NO_ACK_REQ, pid, GUPCR_PTL_PTE_SHUTDOWN,
			       PTL_NO_MATCH_BITS, 0, PTL_NULL_USER_PTR,
			       PTL_NULL_HDR_DATA));
  pthread_join (gupcr_shutdown_pthread_id, NULL);
}

/**
 * Shutdown pthread that waits for remote shutdown requests.
 *
 * This pthread waits on a shutdown PTE to receive a shutdown
 * request from any other thread that executed upc_global_exit().
 * Then, it uses signal (SHUTDOWN_SIGNAL) to inform the main thread
 * of a need to shutdown this UPC thread.
 * @param [in] arg Pthread arguments (not used in this case)
 * @retval Pthread's exit value
 */
static void *
gupcr_shutdown_pthread (void *arg __attribute ((unused)))
{
  ptl_ct_event_t ct = { 0, 0 };
  int pstatus;

  gupcr_log (FC_MISC, "Shutdown pthread started");
  /* Wait for the shutdown request.  Yield control of the
     CPU frequently as this is a low priority activity
     that should minimize competition for resources with the
     main thread.  */
  do
    {
      gupcr_cpu_delay (SHUTDOWN_MICROSEC_WAIT);
      gupcr_portals_call_with_status (PtlCTGet, pstatus,
				      (gupcr_shutdown_le_ct, &ct));
    }
  while (!(ct.success + ct.failure));
  gupcr_debug (FC_MISC, "Shutdown pthread received exit %d",
	       gupcr_shutdown_status);

  /* Signal the main thread to exit.  */
  kill (getpid (), SHUTDOWN_SIGNAL);
  /* No need for this helper pthread any more.  */
  pthread_exit (NULL);
}

/**
 * Signal handler that performs shutdown.
 *
 * This signal handler will terminate the pthread
 * that listens for shutdown requests and then
 * exit the current process (which is associated with
 * a UPC thread).  The UPC will exit with the
 * the status code received from the remote thread.
 * @param [in] signum Signal number (unused)
 */
void
gupcr_shutdown_signal_handler (int signum __attribute__ ((unused)))
{
  gupcr_debug (FC_MISC, "Shutdown signal handler for signal %d", signum);
  /* Wait for shutdown pthread to exit.  */
  pthread_join (gupcr_shutdown_pthread_id, NULL);
  /* It is NOT ok to call finalize routines as there might
     be outstanding transactions.  */
  gupcr_finalize_ok = 0;
  /* Exit with global exit code.  */
  exit (gupcr_shutdown_status);
}

/**
 * Initialize remote shutdown resources.
 * @ingroup INIT
 */
void
gupcr_shutdown_init (void)
{
  ptl_md_t md;
  ptl_pt_index_t pte;
  ptl_le_t le;

  gupcr_log (FC_MISC, "shutdown init called");

  /* Create the PTE used to communicate shutdown requests.  */
  gupcr_portals_call (PtlPTAlloc, (gupcr_ptl_ni, 0,
				   PTL_EQ_NONE, GUPCR_PTL_PTE_SHUTDOWN,
				   &pte));
  gupcr_assert (pte != GUPCR_PTL_PTE_LOCK);

  gupcr_debug (FC_MISC, "Shutdown PTE allocated: %d", GUPCR_PTL_PTE_SHUTDOWN);

  /* Allocate the LE used for shutdown requests.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_shutdown_le_ct));
  le.start = &gupcr_shutdown_status;
  le.length = sizeof (gupcr_shutdown_status);
  le.ct_handle = gupcr_shutdown_le_ct;
  le.uid = PTL_UID_ANY;
  le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | PTL_LE_EVENT_CT_COMM;
  gupcr_portals_call (PtlLEAppend,
		      (gupcr_ptl_ni, GUPCR_PTL_PTE_SHUTDOWN, &le,
		       PTL_PRIORITY_LIST, NULL, &gupcr_shutdown_le));

  gupcr_debug (FC_MISC, "Service LE created: %lx (%lx)",
	       (long unsigned) &gupcr_shutdown_status,
	       (long unsigned) sizeof (gupcr_shutdown_status));

  /* Setup the MD used to send a shutdown request to other threads.  */
  gupcr_portals_call (PtlCTAlloc, (gupcr_ptl_ni, &gupcr_shutdown_md_ct));
  gupcr_portals_call (PtlEQAlloc, (gupcr_ptl_ni, 1, &gupcr_shutdown_md_eq));

  md.start = &gupcr_shutdown_send_status;
  md.length = sizeof (gupcr_shutdown_send_status);
  md.options = PTL_MD_EVENT_CT_ACK | PTL_MD_EVENT_SUCCESS_DISABLE;
  md.eq_handle = gupcr_shutdown_md_eq;
  md.ct_handle = gupcr_shutdown_md_ct;
  gupcr_portals_call (PtlMDBind, (gupcr_ptl_ni, &md, &gupcr_shutdown_md));

  /* Start a pthread that listens for remote shutdown requests.  */
  gupcr_syscall (pthread_create,
		 (&gupcr_shutdown_pthread_id, (pthread_attr_t *) NULL,
		  &gupcr_shutdown_pthread, NULL));

  /* Install a signal handler that processes remote
     shutdown global exit requests.  */
  gupcr_signal_enable (SHUTDOWN_SIGNAL, gupcr_shutdown_signal_handler);
}

/**
 * Release remote shutdown resources.
 * @ingroup INIT
 */
void
gupcr_shutdown_fini (void)
{
  gupcr_log (FC_MISC, "shutdown fini called");

  /* Terminate the shutdown pthread.  */
  gupcr_shutdown_terminate_pthread ();

  /* Release the shutdown MD.  */
  gupcr_portals_call (PtlMDRelease, (gupcr_shutdown_md));
  gupcr_portals_call (PtlCTFree, (gupcr_shutdown_md_ct));

  /* Release the shutdown LE and PTE.  */
  gupcr_portals_call (PtlLEUnlink, (gupcr_shutdown_le));
  gupcr_portals_call (PtlCTFree, (gupcr_shutdown_le_ct));
  gupcr_portals_call (PtlPTFree, (gupcr_ptl_ni, GUPCR_PTL_PTE_SHUTDOWN));
}

/** @} */
