/* Implementation of the ETIME intrinsic.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

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
write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include <sys/types.h>
#include "libgfortran.h"

#include <stdio.h>

#if defined (HAVE_SYS_TIME_H) && defined (HAVE_SYS_RESOURCE_H)
#include <sys/time.h>
#include <sys/resource.h>
#endif

extern void etime_sub (gfc_array_r4 *t, GFC_REAL_4 *result);
iexport_proto(etime_sub);

void
etime_sub (gfc_array_r4 *t, GFC_REAL_4 *result)
{
  GFC_REAL_4 tu, ts, tt, *tp;

#if defined(HAVE_SYS_TIME_H) && defined(HAVE_SYS_RESOURCE_H)
  struct rusage rt;

  if (getrusage(RUSAGE_SELF, &rt) == 0)
    {
      tu = (GFC_REAL_4)(rt.ru_utime.tv_sec + 1.e-6 * rt.ru_utime.tv_usec);
      ts = (GFC_REAL_4)(rt.ru_stime.tv_sec + 1.e-6 * rt.ru_stime.tv_usec);
      tt = tu + ts;
    }
  else
    {
      tu = -1.;
      ts = -1.;
      tt = -1.;
    }
#else
  tu = -1.;
  ts = -1.;
  tt = -1.;
#endif

  if (((t->dim[0].ubound + 1 - t->dim[0].lbound)) < 2)
    runtime_error ("Insufficient number of elements in TARRAY.");

  if (t->dim[0].stride == 0)
    t->dim[0].stride = 1;

  tp = t->data;

  *tp = tu;
  tp += t->dim[0].stride;
  *tp = ts;
  *result = tt;
}
iexport(etime_sub);

extern GFC_REAL_4 etime (gfc_array_r4 *t);
export_proto(etime);

GFC_REAL_4
etime (gfc_array_r4 *t)
{
  GFC_REAL_4 val;
  etime_sub (t, &val);
  return val;
}

/* LAPACK's test programs declares ETIME external, therefore we
   need this.  */

extern GFC_REAL_4 etime_ (GFC_REAL_4 *t);
export_proto_np(etime_);

GFC_REAL_4
etime_ (GFC_REAL_4 *t)
{
  gfc_array_r4 desc;
  GFC_REAL_4 val;

  /* We only fill in the fields that are used in etime_sub.  */
  desc.dim[0].lbound = 0;
  desc.dim[0].ubound = 1;
  desc.dim[0].stride = 1;
  desc.data = t;

  etime_sub (&desc, &val);
  return val;
}
