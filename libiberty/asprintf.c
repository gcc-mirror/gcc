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

#ifdef ANSI_PROTOTYPES
#include <stdarg.h>
#else
#include <varargs.h>
#endif

int
asprintf VPARAMS ((char **buf, const char *fmt, ...))
{
  int status;
  VA_OPEN (ap, fmt);
  VA_FIXEDARG (ap, char **, buf);
  VA_FIXEDARG (ap, const char *, fmt);
  status = vasprintf (buf, fmt, ap);
  VA_CLOSE (ap);
  return status;
}
