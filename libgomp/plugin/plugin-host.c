/* OpenACC Runtime Library: acc_device_host, acc_device_host_nonshm.

   Copyright (C) 2013-2015 Free Software Foundation, Inc.

   Contributed by Mentor Embedded.

   This file is part of the GNU Offloading and Multi Processing Library
   (libgomp).

   Libgomp is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   Libgomp is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Simple implementation of support routines for a shared-memory
   acc_device_host, and a non-shared memory acc_device_host_nonshm, with the
   latter built as a plugin.  */

#include "openacc.h"
#include "config.h"
#ifdef HOST_NONSHM_PLUGIN
#include "libgomp-plugin.h"
#include "oacc-plugin.h"
#else
#include "libgomp.h"
#include "oacc-int.h"
#endif

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#ifdef HOST_NONSHM_PLUGIN
#define STATIC
#define GOMP(X) GOMP_PLUGIN_##X
#define SELF "host_nonshm plugin: "
#else
#define STATIC static
#define GOMP(X) gomp_##X
#define SELF "host: "
#endif

#ifdef HOST_NONSHM_PLUGIN
#include "plugin-host.h"
#endif

STATIC const char *
GOMP_OFFLOAD_get_name (void)
{
#ifdef HOST_NONSHM_PLUGIN
  return "host_nonshm";
#else
  return "host";
#endif
}

STATIC unsigned int
GOMP_OFFLOAD_get_caps (void)
{
  unsigned int caps = (GOMP_OFFLOAD_CAP_OPENACC_200
		       | GOMP_OFFLOAD_CAP_NATIVE_EXEC);

#ifndef HOST_NONSHM_PLUGIN
  caps |= GOMP_OFFLOAD_CAP_SHARED_MEM;
#endif

  return caps;
}

STATIC int
GOMP_OFFLOAD_get_type (void)
{
#ifdef HOST_NONSHM_PLUGIN
  return OFFLOAD_TARGET_TYPE_HOST_NONSHM;
#else
  return OFFLOAD_TARGET_TYPE_HOST;
#endif
}

STATIC int
GOMP_OFFLOAD_get_num_devices (void)
{
  return 1;
}

STATIC void
GOMP_OFFLOAD_init_device (int n __attribute__ ((unused)))
{
}

STATIC void
GOMP_OFFLOAD_fini_device (int n __attribute__ ((unused)))
{
}

STATIC int
GOMP_OFFLOAD_load_image (int n __attribute__ ((unused)),
			 void *i __attribute__ ((unused)),
			 struct addr_pair **r __attribute__ ((unused)))
{
  return 0;
}

STATIC void
GOMP_OFFLOAD_unload_image (int n __attribute__ ((unused)),
			   void *i __attribute__ ((unused)))
{
}

STATIC void *
GOMP_OFFLOAD_alloc (int n __attribute__ ((unused)), size_t s)
{
  return GOMP (malloc) (s);
}

STATIC void
GOMP_OFFLOAD_free (int n __attribute__ ((unused)), void *p)
{
  free (p);
}

STATIC void *
GOMP_OFFLOAD_host2dev (int n __attribute__ ((unused)), void *d, const void *h,
		       size_t s)
{
#ifdef HOST_NONSHM_PLUGIN
  memcpy (d, h, s);
#endif

  return 0;
}

STATIC void *
GOMP_OFFLOAD_dev2host (int n __attribute__ ((unused)), void *h, const void *d,
		       size_t s)
{
#ifdef HOST_NONSHM_PLUGIN
  memcpy (h, d, s);
#endif

  return 0;
}

STATIC void
GOMP_OFFLOAD_run (int n __attribute__ ((unused)), void *fn_ptr, void *vars)
{
  void (*fn)(void *) = (void (*)(void *)) fn_ptr;

  fn (vars);
}

STATIC void
GOMP_OFFLOAD_openacc_parallel (void (*fn) (void *),
			       size_t mapnum __attribute__ ((unused)),
			       void **hostaddrs __attribute__ ((unused)),
			       void **devaddrs __attribute__ ((unused)),
			       size_t *sizes __attribute__ ((unused)),
			       unsigned short *kinds __attribute__ ((unused)),
			       int num_gangs __attribute__ ((unused)),
			       int num_workers __attribute__ ((unused)),
			       int vector_length __attribute__ ((unused)),
			       int async __attribute__ ((unused)),
			       void *targ_mem_desc __attribute__ ((unused)))
{
#ifdef HOST_NONSHM_PLUGIN
  struct nonshm_thread *thd = GOMP_PLUGIN_acc_thread ();
  thd->nonshm_exec = true;
  fn (devaddrs);
  thd->nonshm_exec = false;
#else
  fn (hostaddrs);
#endif
}

STATIC void
GOMP_OFFLOAD_openacc_register_async_cleanup (void *targ_mem_desc)
{
#ifdef HOST_NONSHM_PLUGIN
  /* "Asynchronous" launches are executed synchronously on the (non-SHM) host,
     so there's no point in delaying host-side cleanup -- just do it now.  */
  GOMP_PLUGIN_async_unmap_vars (targ_mem_desc);
#endif
}

STATIC void
GOMP_OFFLOAD_openacc_async_set_async (int async __attribute__ ((unused)))
{
}

STATIC int
GOMP_OFFLOAD_openacc_async_test (int async __attribute__ ((unused)))
{
  return 1;
}

STATIC int
GOMP_OFFLOAD_openacc_async_test_all (void)
{
  return 1;
}

STATIC void
GOMP_OFFLOAD_openacc_async_wait (int async __attribute__ ((unused)))
{
}

STATIC void
GOMP_OFFLOAD_openacc_async_wait_all (void)
{
}

STATIC void
GOMP_OFFLOAD_openacc_async_wait_async (int async1 __attribute__ ((unused)),
				       int async2 __attribute__ ((unused)))
{
}

STATIC void
GOMP_OFFLOAD_openacc_async_wait_all_async (int async __attribute__ ((unused)))
{
}

STATIC void *
GOMP_OFFLOAD_openacc_create_thread_data (int ord
					 __attribute__ ((unused)))
{
#ifdef HOST_NONSHM_PLUGIN
  struct nonshm_thread *thd
    = GOMP_PLUGIN_malloc (sizeof (struct nonshm_thread));
  thd->nonshm_exec = false;
  return thd;
#else
  return NULL;
#endif
}

STATIC void
GOMP_OFFLOAD_openacc_destroy_thread_data (void *tls_data)
{
#ifdef HOST_NONSHM_PLUGIN
  free (tls_data);
#endif
}
