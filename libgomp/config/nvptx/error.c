/* Copyright (C) 2015-2023 Free Software Foundation, Inc.
   Contributed by Alexander Monakov <amonakov@ispras.ru>

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

/* This file contains routines used to signal errors.  On NVPTX, we have
   one default output stream (stdout), so redirect everything there.  */

#include "libgomp.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>


/* No 'FILE *stream's, just basic 'vprintf' etc.  */

#undef vfprintf
#define vfprintf(stream, fmt, list) vprintf (fmt, list)

#undef fputs
#define fputs(s, stream) printf ("%s", s)

#undef fputc
#define fputc(c, stream) printf ("%c", c)

#undef fwrite
#if 0
# define fwrite(ptr, size, nmemb, stream) \
  printf ("%.*s", (int) (size * nmemb), (int) (size * nmemb), ptr)
/* ... prints literal '%.*s'.  */
#else
# define fwrite(ptr, size, nmemb, stream) \
  do { \
    /* Yuck!  */ \
    for (size_t i = 0; i < size * nmemb; ++i) \
      printf ("%c", ptr[i]); \
  } while (0)
#endif


/* The 'exit (EXIT_FAILURE);' of an Fortran (only, huh?) OpenMP 'error'
   directive with 'severity (fatal)' causes a hang, so 'abort' instead of
   'exit'.  */
#undef exit
#define exit(status) abort ()


#include "../../error.c"
