/* Allocate memory region filled with spaces.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

@deftypefn Extension char* spaces (int @var{count})

Returns a pointer to a memory region filled with the specified
number of spaces and null terminated.  The returned pointer is
valid until at least the next call.

@end deftypefn

*/

#include "ansidecl.h"
#include "libiberty.h"

#if VMS
#include <stdlib.h>
#include <unixlib.h>
#else
/* For systems with larger pointers than ints, these must be declared.  */
extern PTR malloc PARAMS ((size_t));
extern void free PARAMS ((PTR));
#endif

const char *
spaces (count)
  int count;
{
  register char *t;
  static char *buf;
  static int maxsize;

  if (count > maxsize)
    {
      if (buf)
	{
	  free (buf);
	}
      buf = malloc (count + 1);
      if (buf == (char *) 0)
	return 0;
      for (t = buf + count ; t != buf ; )
	{
	  *--t = ' ';
	}
      maxsize = count;
      buf[count] = '\0';
    }
  return (const char *) (buf + maxsize - count);
}

