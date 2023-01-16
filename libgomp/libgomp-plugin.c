/* Copyright (C) 2014-2023 Free Software Foundation, Inc.

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

/* Exported (non-hidden) functions exposing libgomp interface for plugins.  */

#include <stdlib.h>

#include "libgomp.h"
#include "libgomp-plugin.h"

void *
GOMP_PLUGIN_malloc (size_t size)
{
  return gomp_malloc (size);
}

void *
GOMP_PLUGIN_malloc_cleared (size_t size)
{
  return gomp_malloc_cleared (size);
}

void *
GOMP_PLUGIN_realloc (void *ptr, size_t size)
{
  return gomp_realloc (ptr, size);
}

void
GOMP_PLUGIN_debug (int kind, const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  gomp_vdebug (kind, msg, ap);
  va_end (ap);
}

void
GOMP_PLUGIN_error (const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  gomp_verror (msg, ap);
  va_end (ap);
}

void
GOMP_PLUGIN_fatal (const char *msg, ...)
{
  va_list ap;

  va_start (ap, msg);
  gomp_vfatal (msg, ap);
  va_end (ap);
}

void
GOMP_PLUGIN_target_rev (uint64_t fn_ptr, uint64_t mapnum, uint64_t devaddrs_ptr,
			uint64_t sizes_ptr, uint64_t kinds_ptr, int dev_num,
			void (*dev_to_host_cpy) (void *, const void *, size_t,
						 void *),
			void (*host_to_dev_cpy) (void *, const void *, size_t,
						 void *), void *token)
{
  gomp_target_rev (fn_ptr, mapnum, devaddrs_ptr, sizes_ptr, kinds_ptr, dev_num,
		   dev_to_host_cpy, host_to_dev_cpy, token);
}
