/* Implement the stpncpy function.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Written by Kaveh R. Ghazi <ghazi@caip.rutgers.edu>.

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

/*

@deftypefn Supplemental char* stpncpy (char *@var{dst}, const char *@var{src}, size_t @var{len})

Copies the string @var{src} into @var{dst}, copying exactly @var{len}
and padding with zeros if necessary.  If @var{len} < strlen(@var{src})
then return @var{dst} + @var{len}, otherwise returns @var{dst} +
strlen(@var{src}).

@end deftypefn

*/

#include <ansidecl.h>
#ifdef ANSI_PROTOTYPES
#include <stddef.h>
#else
#define size_t unsigned long
#endif

extern size_t strlen PARAMS ((const char *));
extern char *strncpy PARAMS ((char *, const char *, size_t));

char *
stpncpy (dst, src, len)
     char *dst;
     const char *src;
     size_t len;
{
  size_t n = strlen (src);
  if (n > len)
    n = len;
  return strncpy (dst, src, len) + n;
}
