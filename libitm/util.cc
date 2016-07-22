/* Copyright (C) 2009-2016 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
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

#include "libitm_i.h"
#include <stdarg.h>
#include <stdio.h>

namespace GTM HIDDEN {

static void
gtm_verror (const char *fmt, va_list list)
{
  fputs ("\nlibitm: ", stderr);
  vfprintf (stderr, fmt, list);
  fputc ('\n', stderr);
}

void
GTM_error (const char *fmt, ...)
{
  va_list list;

  va_start (list, fmt);
  gtm_verror (fmt, list);
  va_end (list);
}

void
GTM_fatal (const char *fmt, ...)
{
  va_list list;

  va_start (list, fmt);
  gtm_verror (fmt, list);
  va_end (list);

  exit (EXIT_FAILURE);
}

void *
xmalloc (size_t size, bool separate_cl)
{
  void *r;
#ifdef HAVE_POSIX_MEMALIGN
  if (separate_cl)
    {
      if (posix_memalign (&r, HW_CACHELINE_SIZE, size))
	GTM_fatal ("Out of memory allocating %lu bytes aligned on cache line",
		   (unsigned long) size);
    }
  else
#endif
    {
      r = malloc (size);
      if (r == 0)
	GTM_fatal ("Out of memory allocating %lu bytes",
		   (unsigned long) size);
    }
  return r;
}

void *
xcalloc (size_t size, bool separate_cl)
{
  // TODO Use posix_memalign if separate_cl is true, or some other allocation
  // method that will avoid sharing cache lines with data used by other
  // threads.
  void *r = calloc (1, size);
  if (r == 0)
    GTM_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return r;
}

void *
xrealloc (void *old, size_t size, bool separate_cl)
{
  // TODO Use posix_memalign if separate_cl is true, or some other allocation
  // method that will avoid sharing cache lines with data used by other
  // threads.
  void *r = realloc (old, size);
  if (r == 0)
    GTM_fatal ("Out of memory allocating %lu bytes", (unsigned long) size);
  return r;
}

} // namespace GTM
