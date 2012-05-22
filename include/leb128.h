/* Utilities for reading leb128 values.
   Copyright (C) 2012 Free Software Foundation, Inc.

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
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */

/* The functions defined here can be speed critical.
   Since they are all pretty small we keep things simple and just define
   them all as "static inline".  */

#ifndef LEB128_H
#define LEB128_H

/* Get a definition for inline.  */
#include "ansidecl.h"

/* Get a definition for NULL, size_t.  */
#include <stddef.h>

/* Decode the unsigned LEB128 constant at BUF into the variable pointed to
   by R, and return the number of bytes read.
   If we read off the end of the buffer, zero is returned,
   and nothing is stored in R.

   Note: The result is an int instead of a pointer to the next byte to be
   read to avoid const-vs-non-const problems.  */

static inline size_t
read_uleb128_to_ull (const unsigned char *buf, const unsigned char *buf_end,
		     unsigned long long *r)
{
  const unsigned char *p = buf;
  unsigned int shift = 0;
  unsigned long long result = 0;
  unsigned char byte;

  while (1)
    {
      if (p >= buf_end)
	return 0;

      byte = *p++;
      result |= ((unsigned long long) (byte & 0x7f)) << shift;
      if ((byte & 0x80) == 0)
	break;
      shift += 7;
    }

  *r = result;
  return p - buf;
}

/* Decode the signed LEB128 constant at BUF into the variable pointed to
   by R, and return the number of bytes read.
   If we read off the end of the buffer, zero is returned,
   and nothing is stored in R.

   Note: The result is an int instead of a pointer to the next byte to be
   read to avoid const-vs-non-const problems.  */

static inline size_t
read_sleb128_to_ll (const unsigned char *buf, const unsigned char *buf_end,
		    long long *r)
{
  const unsigned char *p = buf;
  unsigned int shift = 0;
  long long result = 0;
  unsigned char byte;

  while (1)
    {
      if (p >= buf_end)
	return 0;

      byte = *p++;
      result |= ((unsigned long long) (byte & 0x7f)) << shift;
      shift += 7;
      if ((byte & 0x80) == 0)
	break;
    }
  if (shift < (sizeof (*r) * 8) && (byte & 0x40) != 0)
    result |= -(((unsigned long long) 1) << shift);

  *r = result;
  return p - buf;
}

/* Return the number of bytes to read to skip past an LEB128 number in BUF.
   If the end isn't found before reaching BUF_END, return zero.

   Note: The result is an int instead of a pointer to the next byte to be
   read to avoid const-vs-non-const problems.  */

static inline size_t
skip_leb128 (const unsigned char *buf, const unsigned char *buf_end)
{
  const unsigned char *p = buf;
  unsigned char byte;

  while (1)
    {
      if (p == buf_end)
	return 0;

      byte = *p++;
      if ((byte & 0x80) == 0)
	return p - buf;
    }
}

#endif /* LEB128_H */
