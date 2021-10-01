/* Copyright (C) 2005-2021 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

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
omp_set_default_device (int device_num)
{
  struct gomp_task_icv *icv = gomp_icv (true);
  icv->default_device_var = device_num >= 0 ? device_num : 0;
}

ialias (omp_set_default_device)

int
omp_get_default_device (void)
{
  struct gomp_task_icv *icv = gomp_icv (false);
  return icv->default_device_var;
}

ialias (omp_get_default_device)

int
omp_get_initial_device (void)
{
  return gomp_get_num_devices ();
}

ialias (omp_get_initial_device)

int
omp_get_num_devices (void)
{
  return gomp_get_num_devices ();
}

ialias (omp_get_num_devices)

int
omp_is_initial_device (void)
{
  /* Hardcoded to 1 on host, should be 0 on MIC, HSAIL, PTX.  */
  return 1;
}

ialias (omp_is_initial_device)

int
omp_get_device_num (void)
{
  /* By specification, this is equivalent to omp_get_initial_device
     on the host.  */
  return ialias_call (omp_get_initial_device) ();
}

ialias (omp_get_device_num)
