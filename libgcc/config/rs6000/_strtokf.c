/* Copyright (C) 1989-2021 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

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

/* Conversion to IEEE 128-bit floating point from string.  */

#include <stddef.h>
#include <stdlib.h>
#include <soft-fp.h>
#include <quad-float128.h>

/* This function must be built with IBM 128-bit as long double, so that we can
   access the strtold function if do not have an IEEE 128-bit version.  */
#if !defined(__LONG_DOUBLE_128__) || !defined(__LONG_DOUBLE_IBM128__)
#error "Long double is not IBM 128-bit"
#endif

/* If the user is using GLIBC 2.32, we can use the __strtoieee128 function.

   If we are linked against an earlier library, we will have fake it by
   converting the string to IBM 128-bit long double, and then converting that to
   __float128.  This isn't ideal, as IEEE 128-bit has more exponent range than
   IBM 128-bit.  */

extern _Float128 __strtoieee128 (const char *, char **) __attribute__ ((__weak__));

_Float128
__strtokf (const char *string, char **endptr)
{
  if (__strtoieee128)
    return __strtoieee128 (string, endptr);

  return strtold (string, endptr);
}
