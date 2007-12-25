/* Implementation of the dtime intrinsic.
   Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

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

  if (((t->dim[0].ubound + 1 - t->dim[0].lbound)) < 2)
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
  tp += t->dim[0].stride;
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
