/* Checking gets.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include <ssp/ssp.h>
#include <stdarg.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#endif

#if !HAVE_DECL_GETS
extern char *gets (char *);
#endif

extern void __chk_fail (void) __attribute__((__noreturn__));

char *
__gets_chk (char *s, size_t slen)
{
  char *ret, *buf;

  if (slen >= (size_t) INT_MAX)
    return gets (s);

  if (slen <= 8192)
    buf = alloca (slen + 1);
  else
    buf = malloc (slen + 1);
  if (buf == NULL)
    return gets (s);

  ret = fgets (buf, (int) (slen + 1), stdin);
  if (ret != NULL)
    {
      size_t len = strlen (buf);
      if (len > 0 && buf[len - 1] == '\n')
        --len;
      if (len == slen)
        __chk_fail ();
      memcpy (s, buf, len);
      s[len] = '\0';
      ret = s;
    }

  if (slen > 8192)
    free (buf);
  return ret;
}
