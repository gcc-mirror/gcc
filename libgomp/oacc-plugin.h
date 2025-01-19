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

#ifndef OACC_PLUGIN_H
#define OACC_PLUGIN_H 1

#include "oacc-int.h"
#include "acc_prof.h"

extern void GOMP_PLUGIN_async_unmap_vars (void *, int);
extern struct goacc_thread *GOMP_PLUGIN_goacc_thread (void);
extern void *GOMP_PLUGIN_acc_thread (void);
extern int GOMP_PLUGIN_acc_default_dim (unsigned int);
extern void GOMP_PLUGIN_goacc_profiling_dispatch (acc_prof_info *,
						  acc_event_info *,
						  acc_api_info *);

#endif
