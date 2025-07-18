/* Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Thomas Koenig, Nicolas Koenig, Andre Vehreschild

This file is part of the GNU Fortran Shmem Coarray Library (caf_shmem).

Caf_shmem is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

Caf_shmem is distributed in the hope that it will be useful,
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

#include "caf_error.h"

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void
internal_caf_runtime_error (const char *format, va_list args)
{
  fprintf (stderr, "Fortran runtime error: ");
  vfprintf (stderr, format, args);
  fprintf (stderr, "\n");

  exit (EXIT_FAILURE);
}

void
caf_runtime_error (const char *format, ...)
{
  va_list ap;
  va_start (ap, format);
  internal_caf_runtime_error (format, ap);
}

void
caf_internal_error (const char *format, int *stat, char *errmsg,
		    size_t errmsg_len, ...)
{
  va_list args;
  va_start (args, errmsg_len);
  if (stat)
    {
      *stat = 1;
      if (errmsg_len > 0)
	{
	  int len = vsnprintf (errmsg, errmsg_len, format, args);
	  if (len >= 0 && errmsg_len > (size_t) len)
	    memset (&errmsg[len], ' ', errmsg_len - len);
	}
      va_end (args);
      return;
    }
  else
    internal_caf_runtime_error (format, args);
  va_end (args);
}
