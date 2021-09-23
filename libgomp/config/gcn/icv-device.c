/* Copyright (C) 2015-2021 Free Software Foundation, Inc.
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

/* This file defines OpenMP API entry points that accelerator targets are
   expected to replace.  */

#include "libgomp.h"

void
omp_set_default_device (int device_num __attribute__((unused)))
{
}

int
omp_get_default_device (void)
{
  return 0;
}

int
omp_get_initial_device (void)
{
  return GOMP_DEVICE_HOST_FALLBACK;
}

int
omp_get_num_devices (void)
{
  return 0;
}

int
omp_get_num_teams (void)
{
  return gomp_num_teams_var + 1;
}

int __attribute__ ((__optimize__ ("O2")))
omp_get_team_num (void)
{
  return __builtin_gcn_dim_pos (0);
}

int
omp_is_initial_device (void)
{
  /* AMD GCN is an accelerator-only target.  */
  return 0;
}

/* This is set to the device number of current GPU during device initialization,
   when the offload image containing this libgomp portion is loaded.  */
static volatile int GOMP_DEVICE_NUM_VAR;

int
omp_get_device_num (void)
{
  return GOMP_DEVICE_NUM_VAR;
}

ialias (omp_set_default_device)
ialias (omp_get_default_device)
ialias (omp_get_initial_device)
ialias (omp_get_num_devices)
ialias (omp_get_num_teams)
ialias (omp_get_team_num)
ialias (omp_is_initial_device)
ialias (omp_get_device_num)
