/* Like sprintf but provides a pointer to malloc'd storage, which must
   be freed by the caller.
   Copyright (C) 1997 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "ansidecl.h"
#include "libiberty.h"

#if defined (ANSI_PROTOTYPES) || defined (ALMOST_STDC)
#define USE_STDARG
#endif

#ifdef USE_STDARG
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* VARARGS */
#ifdef USE_STDARG
int
asprintf (char **buf, const char *fmt, ...)
#else
int
asprintf (buf, fmt, va_alist)
     char **buf;
     const char *fmt;
     va_dcl
#endif
{
  int status;
  va_list ap;
#ifdef USE_STDARG
  va_start (ap, fmt);
#else
  va_start (ap);
#endif
  status = vasprintf (buf, fmt, ap);
  va_end (ap);
  return status;
}
