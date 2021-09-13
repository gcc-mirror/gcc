/* OpenACC Runtime Library: acc_device_host.

   Copyright (C) 2013-2021 Free Software Foundation, Inc.

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

#include "libgomp.h"
#include "oacc-int.h"
#include "gomp-constants.h"

#include <stdbool.h>
#include <stddef.h>

static struct gomp_device_descr host_dispatch;

static const char *
host_get_name (void)
{
  return host_dispatch.name;
}

static unsigned int
host_get_caps (void)
{
  return host_dispatch.capabilities;
}

static int
host_get_type (void)
{
  return host_dispatch.type;
}

static int
host_get_num_devices (void)
{
  return 1;
}

static bool
host_init_device (int n __attribute__ ((unused)))
{
  return true;
}

static bool
host_fini_device (int n __attribute__ ((unused)))
{
  return true;
}

static unsigned
host_version (void)
{
  return GOMP_VERSION;
}

static int
host_load_image (int n __attribute__ ((unused)),
		 unsigned v __attribute__ ((unused)),
		 const void *t __attribute__ ((unused)),
		 struct addr_pair **r __attribute__ ((unused)))
{
  return 0;
}

static bool
host_unload_image (int n __attribute__ ((unused)),
		   unsigned v __attribute__ ((unused)),
		   const void *t __attribute__ ((unused)))
{
  return true;
}

static void *
host_alloc (int n __attribute__ ((unused)), size_t s)
{
  return gomp_malloc (s);
}

static bool
host_free (int n __attribute__ ((unused)), void *p)
{
  free (p);
  return true;
}

static bool
host_dev2host (int n __attribute__ ((unused)),
	       void *h __attribute__ ((unused)),
	       const void *d __attribute__ ((unused)),
	       size_t s __attribute__ ((unused)))
{
  return true;
}

static bool
host_host2dev (int n __attribute__ ((unused)),
	       void *d __attribute__ ((unused)),
	       const void *h __attribute__ ((unused)),
	       size_t s __attribute__ ((unused)))
{
  return true;
}

static void
host_run (int n __attribute__ ((unused)), void *fn_ptr, void *vars,
	  void **args __attribute__((unused)))
{
  void (*fn)(void *) = (void (*)(void *)) fn_ptr;

  fn (vars);
}

static void
host_openacc_exec (void (*fn) (void *),
		   size_t mapnum __attribute__ ((unused)),
		   void **hostaddrs,
		   void **devaddrs __attribute__ ((unused)),
		   unsigned *dims __attribute__ ((unused)),
		   void *targ_mem_desc __attribute__ ((unused)))
{
  fn (hostaddrs);
}

static void
host_openacc_async_exec (void (*fn) (void *),
			 size_t mapnum __attribute__ ((unused)),
			 void **hostaddrs,
			 void **devaddrs __attribute__ ((unused)),
			 unsigned *dims __attribute__ ((unused)),
			 void *targ_mem_desc __attribute__ ((unused)),
			 struct goacc_asyncqueue *aq __attribute__ ((unused)))
{
  fn (hostaddrs);
}

static int
host_openacc_async_test (struct goacc_asyncqueue *aq __attribute__ ((unused)))
{
  return 1;
}

static bool
host_openacc_async_synchronize (struct goacc_asyncqueue *aq
				__attribute__ ((unused)))
{
  return true;
}

static bool
host_openacc_async_serialize (struct goacc_asyncqueue *aq1
			      __attribute__ ((unused)),
			      struct goacc_asyncqueue *aq2
			      __attribute__ ((unused)))
{
  return true;
}

static bool
host_openacc_async_host2dev (int ord __attribute__ ((unused)),
			     void *dst __attribute__ ((unused)),
			     const void *src __attribute__ ((unused)),
			     size_t n __attribute__ ((unused)),
			     struct goacc_asyncqueue *aq
			     __attribute__ ((unused)))
{
  return true;
}

static bool
host_openacc_async_dev2host (int ord __attribute__ ((unused)),
			     void *dst __attribute__ ((unused)),
			     const void *src __attribute__ ((unused)),
			     size_t n __attribute__ ((unused)),
			     struct goacc_asyncqueue *aq
			     __attribute__ ((unused)))
{
  return true;
}

static void
host_openacc_async_queue_callback (struct goacc_asyncqueue *aq
				   __attribute__ ((unused)),
				   void (*callback_fn)(void *)
				   __attribute__ ((unused)),
				   void *userptr __attribute__ ((unused)))
{
}

static struct goacc_asyncqueue *
host_openacc_async_construct (int device __attribute__((unused)))
{
  /* Non-NULL 0xffff... value as opaque dummy.  */
  return (struct goacc_asyncqueue *) -1;
}

static bool
host_openacc_async_destruct (struct goacc_asyncqueue *aq
			     __attribute__ ((unused)))
{
  return true;
}

static union goacc_property_value
host_openacc_get_property (int n, enum goacc_property prop)
{
  union goacc_property_value nullval = { .val = 0 };

  if (n >= host_get_num_devices ())
    return nullval;

  switch (prop)
    {
    case GOACC_PROPERTY_NAME:
      return (union goacc_property_value) { .ptr = "GOMP" };
    case GOACC_PROPERTY_VENDOR:
      return (union goacc_property_value) { .ptr = "GNU" };
    case GOACC_PROPERTY_DRIVER:
      return (union goacc_property_value) { .ptr = VERSION };
    case GOACC_PROPERTY_MEMORY:
    case GOACC_PROPERTY_FREE_MEMORY:
    default:
      return nullval;
    }
}

static void *
host_openacc_create_thread_data (int ord __attribute__ ((unused)))
{
  return NULL;
}

static void
host_openacc_destroy_thread_data (void *tls_data __attribute__ ((unused)))
{
}

static struct gomp_device_descr host_dispatch =
  {
    .name = "host",
    .capabilities = (GOMP_OFFLOAD_CAP_SHARED_MEM
		     | GOMP_OFFLOAD_CAP_NATIVE_EXEC
		     | GOMP_OFFLOAD_CAP_OPENACC_200),
    .target_id = 0,
    .type = OFFLOAD_TARGET_TYPE_HOST,

    .get_name_func = host_get_name,
    .get_caps_func = host_get_caps,
    .get_type_func = host_get_type,
    .get_num_devices_func = host_get_num_devices,
    .init_device_func = host_init_device,
    .fini_device_func = host_fini_device,
    .version_func = host_version,
    .load_image_func = host_load_image,
    .unload_image_func = host_unload_image,
    .alloc_func = host_alloc,
    .free_func = host_free,
    .dev2host_func = host_dev2host,
    .host2dev_func = host_host2dev,
    .run_func = host_run,

    .mem_map = { NULL },
    /* .lock initialized in goacc_host_init.  */
    .state = GOMP_DEVICE_UNINITIALIZED,

    .openacc = {
      .exec_func = host_openacc_exec,

      .create_thread_data_func = host_openacc_create_thread_data,
      .destroy_thread_data_func = host_openacc_destroy_thread_data,

      .async = {
	.construct_func = host_openacc_async_construct,
	.destruct_func = host_openacc_async_destruct,
	.test_func = host_openacc_async_test,
	.synchronize_func = host_openacc_async_synchronize,
	.serialize_func = host_openacc_async_serialize,
	.queue_callback_func = host_openacc_async_queue_callback,
	.exec_func = host_openacc_async_exec,
	.dev2host_func = host_openacc_async_dev2host,
	.host2dev_func = host_openacc_async_host2dev,
      },

      .get_property_func = host_openacc_get_property,

      .cuda = {
	.get_current_device_func = NULL,
	.get_current_context_func = NULL,
	.get_stream_func = NULL,
	.set_stream_func = NULL,
      }
    }
  };

/* Initialize and register this device type.  */
void
goacc_host_init (void)
{
  gomp_mutex_init (&host_dispatch.lock);
  goacc_register (&host_dispatch);
}
