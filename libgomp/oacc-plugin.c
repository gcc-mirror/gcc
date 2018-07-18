/* Copyright (C) 2014-2018 Free Software Foundation, Inc.

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

/* Initialize and register OpenACC dispatch table from libgomp plugin.  */

#include "libgomp.h"
#include "oacc-plugin.h"
#include "oacc-int.h"

void
GOMP_PLUGIN_async_unmap_vars (void *ptr, int async)
{
  struct target_mem_desc *tgt = ptr;
  struct gomp_device_descr *devicep = tgt->device_descr;

  devicep->openacc.async_set_async_func (async);
  gomp_unmap_vars (tgt, true);
  devicep->openacc.async_set_async_func (acc_async_sync);
}

/* Return the target-specific part of the TLS data for the current thread.  */

void *
GOMP_PLUGIN_acc_thread (void)
{
  struct goacc_thread *thr = goacc_thread ();
  return thr ? thr->target_tls : NULL;
}

int
GOMP_PLUGIN_acc_default_dim (unsigned int i)
{
  if (i >= GOMP_DIM_MAX)
    {
      gomp_fatal ("invalid dimension argument: %d", i);
      return -1;
    }
  return goacc_default_dims[i];
}
