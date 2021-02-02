/* Checking vsprintf.
   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
#ifdef HAVE_STDIO_H
# include <stdio.h>
#endif

extern void __chk_fail (void) __attribute__((__noreturn__));

#ifdef HAVE_USABLE_VSNPRINTF
int
__vsprintf_chk (char *s, int flags __attribute__((unused)),
		size_t slen, const char *format, va_list arg)
{
  int done;

  if (slen > (size_t) INT_MAX)
    done = vsprintf (s, format, arg);
  else
    {
      done = vsnprintf (s, slen, format, arg);
      if (done >= 0 && (size_t) done >= slen)
	__chk_fail ();
    }
  return done;
}
#endif
