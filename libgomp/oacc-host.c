/* OpenACC Runtime Library: acc_device_host.

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

/* This shares much of the implementation of the plugin-host.c "host_nonshm"
   plugin.  */
#include "plugin/plugin-host.c"

static struct gomp_device_descr host_dispatch =
  {
    .name = "host",
    .capabilities = (GOMP_OFFLOAD_CAP_OPENACC_200
		     | GOMP_OFFLOAD_CAP_NATIVE_EXEC
		     | GOMP_OFFLOAD_CAP_SHARED_MEM),
    .target_id = 0,
    .type = OFFLOAD_TARGET_TYPE_HOST,

    .get_name_func = GOMP_OFFLOAD_get_name,
    .get_caps_func = GOMP_OFFLOAD_get_caps,
    .get_type_func = GOMP_OFFLOAD_get_type,
    .get_num_devices_func = GOMP_OFFLOAD_get_num_devices,
    .init_device_func = GOMP_OFFLOAD_init_device,
    .fini_device_func = GOMP_OFFLOAD_fini_device,
    .load_image_func = GOMP_OFFLOAD_load_image,
    .unload_image_func = GOMP_OFFLOAD_unload_image,
    .alloc_func = GOMP_OFFLOAD_alloc,
    .free_func = GOMP_OFFLOAD_free,
    .dev2host_func = GOMP_OFFLOAD_dev2host,
    .host2dev_func = GOMP_OFFLOAD_host2dev,
    .run_func = GOMP_OFFLOAD_run,

    .is_initialized = false,

    .openacc = {
      .exec_func = GOMP_OFFLOAD_openacc_parallel,

      .register_async_cleanup_func
        = GOMP_OFFLOAD_openacc_register_async_cleanup,

      .async_set_async_func = GOMP_OFFLOAD_openacc_async_set_async,
      .async_test_func = GOMP_OFFLOAD_openacc_async_test,
      .async_test_all_func = GOMP_OFFLOAD_openacc_async_test_all,
      .async_wait_func = GOMP_OFFLOAD_openacc_async_wait,
      .async_wait_async_func = GOMP_OFFLOAD_openacc_async_wait_async,
      .async_wait_all_func = GOMP_OFFLOAD_openacc_async_wait_all,
      .async_wait_all_async_func = GOMP_OFFLOAD_openacc_async_wait_all_async,

      .create_thread_data_func = GOMP_OFFLOAD_openacc_create_thread_data,
      .destroy_thread_data_func = GOMP_OFFLOAD_openacc_destroy_thread_data,

      .cuda = {
	.get_current_device_func = NULL,
	.get_current_context_func = NULL,
	.get_stream_func = NULL,
	.set_stream_func = NULL,
      }
    }
  };

/* Register this device type.  */
void
goacc_host_init (void)
{
  gomp_mutex_init (&host_dispatch.lock);
  goacc_register (&host_dispatch);
}
