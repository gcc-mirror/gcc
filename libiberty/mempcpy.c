/* Implement the mempcpy function.
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

@deftypefn Supplemental void* mempcpy (void *@var{out}, const void *@var{in}, size_t @var{length})

Copies @var{length} bytes from memory region @var{in} to region
@var{out}.  Returns a pointer to @var{out} + @var{length}.

@end deftypefn

*/

#include <ansidecl.h>
#ifdef ANSI_PROTOTYPES
#include <stddef.h>
#else
#define size_t unsigned long
#endif

extern PTR memcpy PARAMS ((PTR, const PTR, size_t));

PTR
mempcpy (dst, src, len)
     PTR dst;
     const PTR src;
     size_t len;
{
  return (char *) memcpy (dst, src, len) + len;
}
