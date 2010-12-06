/* GCC Quad-Precision Math Library
   Copyright (C) 2010 Free Software Foundation, Inc.
   Written by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>

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
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "quadmath.h"
#include <stdio.h>
#include <string.h>

#define ABS(x) ((x) >= 0 ? (x) : -(x))



static void
format (char * res, const __float128 x, size_t n)
{
  char buffer[1024];
  char *p;

  memset (buffer, 0, sizeof(buffer));

  g_Qfmt (buffer, &x, n + 1, sizeof(buffer) - 3);
  p = buffer + (*buffer == '-' ? 1 : 0);

  // The sign is the easiest part
  res[0] = (signbitq (x) ? '-' : '+');

  if (*p == '.')
  {
    // We have a number smaller than 1, without exponent
    int exp = 0;
    char *c;

    for (c = p+1; *c == '0'; c++)
      exp++;

    // We move the string "exp" characters left
    size_t l = strlen (p+1+exp);
    memcpy (res + 2, p + 1 + exp, l);
    memset (res + 2 + l, '0', n - l + 1);
    sprintf (res + n + 3, "e-%02d", exp + 1);

    res[1] = res[2];
    res[2] = '.';

    return;
  }

  // Now, do we already have an exponent
  char *c;
  for (c = p; *c && *c != 'e'; c++)
    ;
  if (*c)
  {
    int exp = strtol (c + 1, NULL, 10);

    size_t l = c - p;

    memcpy (res + 1, p, l);
    if (l <= n + 1)
      memset (res + 1 + l, '0', (int) n - l + 2);

    sprintf (res + n + 3, "e%c%02d", exp >= 0 ? '+' : '-', ABS(exp));

    return;
  }
  else
  {
    // If we have no exponent, normalize and add the exponent
    for (c = p; *c && *c != '.'; c++)
      ;

    res[1] = *p;
    res[2] = '.';

    size_t l = c - p;
    memcpy (res + 3, p + 1, l);
    size_t l2 = strlen (c + 1);
    memcpy (res + 2 + l, c + 1, l2);
    memset (res + 2 + l + l2, '0', n - (l + l2) + 1);
    sprintf (res + n + 3, "e+%02d", l - 1);

    return;
  }
}


void
quadmath_dtoaq (char *s, size_t size, size_t n, __float128 x)
{
  char buffer[1024];
  memset (buffer, 0, sizeof(buffer));
  format (buffer, x, n);
  memcpy (s, buffer, size);
}
