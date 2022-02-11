/* Implement the xasprintf function.
   Copyright (C) 2014-2022 Free Software Foundation, Inc.
   Contributed by Manuel Lopez-Ibanez.

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
License along with libiberty; see the file COPYING.LIB.  If not, write
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth
Floor, Boston, MA 02110-1301, USA.  */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "ansidecl.h"
#include "libiberty.h"

#include <stdarg.h>

/*

@deftypefn Replacement char* xasprintf (const char *@var{format}, ...)

Print to allocated string without fail.  If @code{xasprintf} fails,
this will print a message to @code{stderr} (using the name set by
@code{xmalloc_set_program_name}, if any) and then call @code{xexit}.

@end deftypefn

*/

char *
xasprintf (const char *fmt, ...)
{
  char *buf;
  va_list ap;
  va_start (ap, fmt);
  buf = xvasprintf (fmt, ap);
  va_end (ap);
  return buf;
}
