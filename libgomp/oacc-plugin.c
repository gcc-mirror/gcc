/* Copyright (C) 2014-2025 Free Software Foundation, Inc.

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
#include "acc_prof.h"

/* This plugin function is now obsolete.  */
void
GOMP_PLUGIN_async_unmap_vars (void *ptr __attribute__((unused)),
			      int async __attribute__((unused)))
{
  gomp_fatal ("invalid plugin function");
}

/* Return the TLS data for the current thread.  */

struct goacc_thread *
GOMP_PLUGIN_goacc_thread (void)
{
  return goacc_thread ();
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
    }
  return goacc_default_dims[i];
}

void
GOMP_PLUGIN_goacc_profiling_dispatch (acc_prof_info *prof_info,
				      acc_event_info *event_info,
				      acc_api_info *api_info)
{
  goacc_profiling_dispatch (prof_info, event_info, api_info);
}
