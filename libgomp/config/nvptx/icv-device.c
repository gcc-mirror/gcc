/* Copyright (C) 2015-2023 Free Software Foundation, Inc.
   Contributed by Alexander Monakov <amonakov@ispras.ru>

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

/* This is set to the ICV values of current GPU during device initialization,
   when the offload image containing this libgomp portion is loaded.  */
volatile struct gomp_offload_icvs GOMP_ADDITIONAL_ICVS;

void
omp_set_default_device (int device_num __attribute__((unused)))
{
}

int
omp_get_default_device (void)
{
  return GOMP_ADDITIONAL_ICVS.default_device;
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
omp_is_initial_device (void)
{
  /* NVPTX is an accelerator-only target.  */
  return 0;
}

int
omp_get_device_num (void)
{
  return GOMP_ADDITIONAL_ICVS.device_num;
}

int
omp_get_max_teams (void)
{
  return GOMP_ADDITIONAL_ICVS.nteams;
}

void
omp_set_num_teams (int num_teams)
{
  if (num_teams >= 0)
    GOMP_ADDITIONAL_ICVS.nteams = num_teams;
}

int
omp_get_teams_thread_limit (void)
{
  return GOMP_ADDITIONAL_ICVS.teams_thread_limit;
}

void
omp_set_teams_thread_limit (int thread_limit)
{
  if (thread_limit >= 0)
    GOMP_ADDITIONAL_ICVS.teams_thread_limit = thread_limit;
}

ialias (omp_set_default_device)
ialias (omp_get_default_device)
ialias (omp_get_initial_device)
ialias (omp_get_num_devices)
ialias (omp_is_initial_device)
ialias (omp_get_device_num)
ialias (omp_get_max_teams)
ialias (omp_set_num_teams)
ialias (omp_get_teams_thread_limit)
ialias (omp_set_teams_thread_limit)
