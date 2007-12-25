/* Implementation of the CPU_TIME intrinsic.
   Copyright (C) 2003, 2007 Free Software Foundation, Inc.

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

/* The most accurate way to get the CPU time is getrusage ().
   If we have times(), that's good enough, too.  */
#if !defined (HAVE_GETRUSAGE) || !defined (HAVE_SYS_RESOURCE_H)
/* For times(), we _must_ know the number of clock ticks per second.  */
#  if defined (HAVE_TIMES) && (defined (HZ) || defined (_SC_CLK_TCK) || defined (CLK_TCK))
#    ifdef HAVE_SYS_PARAM_H
#      include <sys/param.h>
#    endif
#    if defined (HAVE_SYS_TIMES_H)
#      include <sys/times.h>
#    endif
#    ifndef HZ
#      if defined _SC_CLK_TCK
#        define HZ  sysconf(_SC_CLK_TCK)
#      else
#        define HZ  CLK_TCK
#      endif
#    endif
#  endif  /* HAVE_TIMES etc.  */
#endif  /* !HAVE_GETRUSAGE || !HAVE_SYS_RESOURCE_H  */

static inline void __cpu_time_1 (long *, long *) ATTRIBUTE_ALWAYS_INLINE;

static inline void
__cpu_time_1 (long *sec, long *usec)
{
#if defined(__MINGW32__) || defined (HAVE_GETRUSAGE) && defined (HAVE_SYS_RESOURCE_H)
  long user_sec, user_usec, system_sec, system_usec;
  __time_1 (&user_sec, &user_usec, &system_sec, &system_usec);
  *sec = user_sec + system_sec;
  *usec = user_usec + system_usec;
#else /* ! HAVE_GETRUSAGE || ! HAVE_SYS_RESOURCE_H  */
#ifdef HAVE_TIMES
  struct tms buf;
  times (&buf);
  *sec = 0;
  *usec = (buf.tms_utime + buf.tms_stime) * (1000000 / HZ);
#else /* ! HAVE_TIMES */
  /* We have nothing to go on.  Return -1.  */
  *sec = -1;
  *usec = 0;
#endif  /* HAVE_TIMES */
#endif  /* __MINGW32__ || HAVE_GETRUSAGE */
}


extern void cpu_time_4 (GFC_REAL_4 *);
iexport_proto(cpu_time_4);

void cpu_time_4 (GFC_REAL_4 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * (GFC_REAL_4)1.e-6;
}
iexport(cpu_time_4);

extern void cpu_time_8 (GFC_REAL_8 *);
export_proto(cpu_time_8);

void cpu_time_8 (GFC_REAL_8 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * (GFC_REAL_8)1.e-6;
}

#ifdef HAVE_GFC_REAL_10
extern void cpu_time_10 (GFC_REAL_10 *);
export_proto(cpu_time_10);

void cpu_time_10 (GFC_REAL_10 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * (GFC_REAL_10)1.e-6;
}
#endif

#ifdef HAVE_GFC_REAL_16
extern void cpu_time_16 (GFC_REAL_16 *);
export_proto(cpu_time_16);

void cpu_time_16 (GFC_REAL_16 *time)
{
  long sec, usec;
  __cpu_time_1 (&sec, &usec);
  *time = sec + usec * (GFC_REAL_16)1.e-6;
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
