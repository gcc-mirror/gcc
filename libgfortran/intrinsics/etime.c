/* Implementation of the ETIME intrinsic.
   Copyright (C) 2004-2014 Free Software Foundation, Inc.
   Contributed by Steven G. Kargl <kargls@comcast.net>.

This file is part of the GNU Fortran runtime library (libgfortran).

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

extern void etime_sub (gfc_array_r4 *t, GFC_REAL_4 *result);
iexport_proto(etime_sub);

void
etime_sub (gfc_array_r4 *t, GFC_REAL_4 *result)
{
  GFC_REAL_4 tu, ts, tt, *tp;
  long user_sec, user_usec, system_sec, system_usec;

  if (((GFC_DESCRIPTOR_EXTENT(t,0))) < 2)
    runtime_error ("Insufficient number of elements in TARRAY.");

  if (gf_cputime (&user_sec, &user_usec, &system_sec, &system_usec) == 0)
    {
      tu = (GFC_REAL_4)(user_sec + 1.e-6 * user_usec);
      ts = (GFC_REAL_4)(system_sec + 1.e-6 * system_usec);
      tt = tu + ts;
    }
  else
    {
      tu = (GFC_REAL_4)-1.0;
      ts = (GFC_REAL_4)-1.0;
      tt = (GFC_REAL_4)-1.0;
    }

  tp = t->base_addr;

  *tp = tu;
  tp += GFC_DESCRIPTOR_STRIDE(t,0);
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
