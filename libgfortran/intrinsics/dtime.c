/* Implementation of the dtime intrinsic.
   Copyright (C) 2004, 2005, 2006, 2007, 2009 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"
#include "time_1.h"
#include <gthr.h>

#ifdef __GTHREAD_MUTEX_INIT
static __gthread_mutex_t dtime_update_lock = __GTHREAD_MUTEX_INIT;
#else
static __gthread_mutex_t dtime_update_lock;
#endif

extern void dtime_sub (gfc_array_r4 *t, GFC_REAL_4 *result);
iexport_proto(dtime_sub);

void
dtime_sub (gfc_array_r4 *t, GFC_REAL_4 *result)
{
  static GFC_REAL_4 tu = 0.0, ts = 0.0, tt = 0.0;
  GFC_REAL_4 *tp;
  long user_sec, user_usec, system_sec, system_usec;

  if (((GFC_DESCRIPTOR_EXTENT(t,0))) < 2)
    runtime_error ("Insufficient number of elements in TARRAY.");

  __gthread_mutex_lock (&dtime_update_lock);
  if (__time_1 (&user_sec, &user_usec, &system_sec, &system_usec) == 0)
    {
      tu = (GFC_REAL_4)(user_sec + 1.e-6 * user_usec) - tu;
      ts = (GFC_REAL_4)(system_sec + 1.e-6 * system_usec) - ts;
      tt = tu + ts;
    }
  else
    {
      tu = (GFC_REAL_4)-1.0;
      ts = (GFC_REAL_4)-1.0;
      tt = (GFC_REAL_4)-1.0;
    }

  tp = t->data;

  *tp = tu;
  tp += GFC_DESCRIPTOR_STRIDE(t,0);
  *tp = ts;
  *result = tt;
  __gthread_mutex_unlock (&dtime_update_lock);
}
iexport(dtime_sub);

extern GFC_REAL_4 dtime (gfc_array_r4 *t);
export_proto(dtime);

GFC_REAL_4
dtime (gfc_array_r4 *t)
{
  GFC_REAL_4 val;
  dtime_sub (t, &val);
  return val;
}
