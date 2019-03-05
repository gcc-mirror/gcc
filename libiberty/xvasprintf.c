/* Implement the xvasprintf function.
   Copyright (C) 2014-2019 Free Software Foundation, Inc.
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
#include <ansidecl.h>
#include <stdarg.h>
#if !defined (va_copy) && defined (__va_copy)
# define va_copy(d,s)  __va_copy((d),(s))
#endif
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include "libiberty.h"
#include "vprintf-support.h"

/*

@deftypefn Replacement char* xvasprintf (const char *@var{format}, va_list @var{args})

Print to allocated string without fail.  If @code{xvasprintf} fails,
this will print a message to @code{stderr} (using the name set by
@code{xmalloc_set_program_name}, if any) and then call @code{xexit}.

@end deftypefn

*/

char *
xvasprintf (const char *format,
#if defined (_BSD_VA_LIST_) && defined (__FreeBSD__)
           _BSD_VA_LIST_ args)
#else
           va_list args)
#endif
{
  char *result;
  int total_width = libiberty_vprintf_buffer_size (format, args);
  result = (char *) xmalloc (total_width);
  vsprintf (result, format, args);
  return result;
}
