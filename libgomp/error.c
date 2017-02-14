/* Copyright (C) 2005-2017 Free Software Foundation, Inc.
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

/* This file contains routines used to signal errors.  Most places in the
   OpenMP API do not make any provision for failure, so we can't just
   defer the decision on reporting the problem to the user; we must do it
   ourselves or not at all.  */
/* ??? Is this about what other implementations do?  Assume stderr hasn't
   been pointed somewhere unsafe?  */

#include "libgomp.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>


#undef gomp_vdebug
void
gomp_vdebug (int kind __attribute__ ((unused)), const char *msg, va_list list)
{
  if (gomp_debug_var)
    vfprintf (stderr, msg, list);
}

#undef gomp_debug
void
gomp_debug (int kind, const char *msg, ...)
{
  va_list list;

  va_start (list, msg);
  gomp_vdebug (kind, msg, list);
  va_end (list);
}

void
gomp_verror (const char *fmt, va_list list)
{
  fputs ("\nlibgomp: ", stderr);
  vfprintf (stderr, fmt, list);
  fputc ('\n', stderr);
}

void
gomp_error (const char *fmt, ...)
{
  va_list list;

  va_start (list, fmt);
  gomp_verror (fmt, list);
  va_end (list);
}

void
gomp_vfatal (const char *fmt, va_list list)
{
  gomp_verror (fmt, list);
  exit (EXIT_FAILURE);
}

void
gomp_fatal (const char *fmt, ...)
{
  va_list list;

  va_start (list, fmt);
  gomp_vfatal (fmt, list);
  va_end (list);
}
