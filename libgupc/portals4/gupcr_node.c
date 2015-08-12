/* Copyright (C) 2012-2014 Free Software Foundation, Inc.
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
 * @file gupcr_node.c
 * GUPC Node Local Memory.
 */

/**
 * @addtogroup NODE GUPCR Node Local Memory
 * @{
 */

#include <stdio.h>
#include <sys/time.h>
#include "gupcr_config.h"
#include "gupcr_defs.h"
#include "gupcr_utils.h"
#include "gupcr_portals.h"
#include "gupcr_runtime.h"
#include "gupcr_gmem.h"
#include "gupcr_node.h"

/**
 * GUPC Node Local Memory Support
 *
 * Multiple UPC threads can run on a single node and access
 * each other's data via shared memory instead of
 * the Portals library API.  This is true for all
 * ordinary shared accesses (get/put), but not for atomic or
 * some specialized Portals operations (e.g. triggered
 * operations).  Future runtime improvements might remove
 * this restriction in some cases.
 *
 * DISABLED
 *   Each thread allocates its own shared memory with
 *   a private mapping (MAP_ANONYMOUS).
 *
 *   This functionality is useful for testing purposes.
 *
 * ENABLED
 *   Shared memory can be configured to use
 *   an mmap backed file or POSIX shared memory.
 *
 *   Shared memory allocation and mapping:
 *
 *   (1) Each thread creates a shared object or file name in the
 *       form of "upc-mem-MYTHREAD-MYPID" where MYTHREAD and MYPID
 *       are 6 digit numbers with leading zeros.
 *       This name refers to either a file name (mapped via mmap())
 *       or a POSIX shared memory object.
 *   (2) Each thread maps its own shared space and verifies that
 *       it can be accessed (by writing to the first and last page).
 *   (3) A runtime barrier is used to allow for every thread in
 *       the system to complete its own setup.
 *   (4) By using Portals pid-to-nid mappings, each thread searches for
 *       other threads that reside on the same node (same nid).
 *   (5) For each found thread, a POSIX shared object or a file
 *       is opened (in the same manner as under the step 1), mapped,
 *       and verified.
 *
 * UPC ACCESS
 *   Each thread keeps a private array of addresses to other
 *   threads' shared spaces.  A of NULL value indicates that
 *   specified thread is NOT on the same node and Portals
 *   functions should be used.  Otherwise, the mapped address
 *   is the base address of the target thread's shared address space.
 */

/** Node local memory map (indexed by thread id).  */
char **gupcr_node_map;
/** Current thread for mapping verification.  */
int gupcr_check_thread;

/**
 * SIGBUS handler for shared memory check.
 *
 * SIGBUS happens if the current thread is unable to
 * access shared memory of some other thread.
 */
static void
gupcr_mem_sigbus (int sig __attribute__ ((unused)))
{
#if GUPCR_NODE_LOCAL_MEM
  gupcr_mem_local_unlink ();
#endif
  gupcr_fatal_error ("cannot access shared memory of thread %d",
		     gupcr_check_thread);
}

/**
 * Shared memory check.
 *
 * Check that memory at the specified address and with the
 * specified size can be accessed.  As memory might not
 * accessible, this procedure installs its own SIGBUS handler
 * in order to provide the user with better diagnostic.
 */
static void
gupcr_mem_check (char *mem, size_t size, int thread)
{
  char temp;
  struct sigaction action;
  struct sigaction old_action;

  /* Set file name for better diagnostic.  */
  gupcr_check_thread = thread;

  /* Install new SIGBUS handler to catch memory faults.  */
  action.sa_handler = gupcr_mem_sigbus;
  sigemptyset (&action.sa_mask);
  action.sa_flags = 0;
  sigaction (SIGBUS, &action, &old_action);

  if (gupcr_is_forcetouch_enabled ())
    {
      volatile char *memp = (volatile char *) mem;
      while (memp < mem + size)
	{
	  temp = *memp;
	  *memp = temp;
	  memp += GUPCR_MEMORY_PAGE_SIZE;
	}
    }
  else
    {
      /* Only check the first and the last page.  */
      temp = *(volatile char *) mem;
      *(volatile char *) mem = temp;
      temp = *(volatile char *) (mem + size - 16);
      *(volatile char *) (mem + size - 16) = temp;
    }

  /* Restore SIGBUS handler.  */
  sigaction (SIGBUS, &old_action, NULL);
}

/**
 * Allocate memory for a thread's shared space.
 *
 * This memory is never shared with other threads
 * on the same node and is visible through Portals
 * functions only.
 *
 * @retval Memory address of mapping
 */
char *
gupcr_mem_private (size_t size)
{
  char *memaddr;
  memaddr = mmap (NULL, size, PROT_READ | PROT_WRITE,
		  MAP_PRIVATE | MAP_ANONYMOUS, -1, OFFSET_ZERO);
  if (!memaddr || memaddr == MAP_ERROR)
    gupcr_fatal_error
      ("cannot mmap 0x%lx bytes of node's private shared memory (%s)",
       (long unsigned) size, strerror (errno));
  gupcr_log (FC_MEM, "using private mapping for shared space");
  gupcr_mem_check (memaddr, size, MYTHREAD);
  return memaddr;
}

/**
 * Shared space allocation and mapping.
 *
 * Allocate and map each thread's node local shared space.
 * Learn and map the shared space of other threads that reside on
 * the same node.
 *
 * @in size Size of the shared space.
 * @retval Address of the shared space.
 */
char *
gupcr_node_local_alloc (size_t size)
{
  /* Allocate zero initialized space for node local memory map.  */
  gupcr_node_map = calloc (THREADS, sizeof (char *));
  if (!gupcr_node_map)
    gupcr_fatal_error ("cannot allocate space for node local memory map");

#if GUPCR_NODE_LOCAL_MEM
  /* Node Local Memory can be disabled by env variable.  */
  if (gupcr_is_node_local_memory_enabled ())
    {
      int i;
      /* Create mapping for this thread.  */
      gupcr_node_map[MYTHREAD] = gupcr_mem_local_map (MYTHREAD, size);
      /* Verify that we can access memory.  */
      gupcr_mem_check (gupcr_node_map[MYTHREAD], size, MYTHREAD);

      /* Wait for all other threads to complete the same.  */
      gupcr_runtime_barrier ();

      /* Map shared memory of other threads on the same node.  */
      {
	int nid = gupcr_get_rank_nid (MYTHREAD);
	for (i = 0; i < THREADS; i++)
	  {
	    if (i != MYTHREAD && nid == gupcr_get_rank_nid (i))
	      {
		gupcr_node_map[i] = gupcr_mem_local_map (i, size);
	      }
	  }
      }
      /* Make sure everybody completed their mappings.  */
      gupcr_runtime_barrier ();
      /* At this point it is safe to cleanup.  */
      gupcr_mem_local_unlink ();
    }
  else
    {
      /* The node local shared memory optimization has been
         disabled (via an environment variable).
	 Create and map only the shared memory contribution of this node.  */
      gupcr_node_map[MYTHREAD] = gupcr_mem_private (size);
    }
#else
  /* The node local shared memory optimization has been
     disabled (via a configuration option).
     Create and map only the shared memory contribution of this node.  */
  gupcr_node_map[MYTHREAD] = gupcr_mem_private (size);
#endif /* GUPCR_NODE_LOCAL_MEM */
  return gupcr_node_map[MYTHREAD];
}

/**
 * Initialize node specific data.
 */
void
gupcr_node_init (void)
{
#if GUPCR_NODE_LOCAL_MEM
  if (gupcr_is_node_local_memory_enabled ())
    gupcr_mem_local_init ();
#endif
}

/**
 * Finalize node specific data.
 */
void
gupcr_node_fini (void)
{
}

/** @} */
