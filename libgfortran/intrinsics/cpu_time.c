/* Implementation of the CPU_TIME intrinsic.
   Copyright (C) 2003-2021 Free Software Foundation, Inc.

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


static void
__cpu_time_1 (long *sec, long *usec)
{
  long user_sec, user_usec, system_sec, system_usec;
  if (gf_cputime (&user_sec, &user_usec, &system_sec, &system_usec) == 0)
    {
      *sec = user_sec + system_sec;
      *usec = user_usec + system_usec;
    }
  else
    {
      *sec = -1;
      *usec = 0;
    }
}


extern void cpu_time_4 (GFC_REAL_4 *);
iexport_proto(cpu_time_4);

void cpu_time_4 (GFC_REAL_4 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * GFC_REAL_4_LITERAL(1.e-6);
}
iexport(cpu_time_4);

extern void cpu_time_8 (GFC_REAL_8 *);
export_proto(cpu_time_8);

void cpu_time_8 (GFC_REAL_8 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * GFC_REAL_8_LITERAL(1.e-6);
}

#ifdef HAVE_GFC_REAL_10
extern void cpu_time_10 (GFC_REAL_10 *);
export_proto(cpu_time_10);

void cpu_time_10 (GFC_REAL_10 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * GFC_REAL_10_LITERAL(1.e-6);
}
#endif

#ifdef HAVE_GFC_REAL_16
extern void cpu_time_16 (GFC_REAL_16 *);
export_proto(cpu_time_16);

void cpu_time_16 (GFC_REAL_16 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * GFC_REAL_16_LITERAL(1.e-6);
}
#endif

extern void second_sub (GFC_REAL_4 *);
export_proto(second_sub);

void
second_sub (GFC_REAL_4 *s)
{
  cpu_time_4 (s);
}

extern GFC_REAL_4 second (void);
export_proto(second);

GFC_REAL_4
second (void)
{
  GFC_REAL_4 s;
  cpu_time_4 (&s);
  return s;
}
