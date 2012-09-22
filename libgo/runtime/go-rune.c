/* go-rune.c -- rune functions for Go.

   Copyright 2009, 2010 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stddef.h>

#include "go-string.h"

/* Get a character from the UTF-8 string STR, of length LEN.  Store
   the Unicode character, if any, in *RUNE.  Return the number of
   characters used from STR.  */

int
__go_get_rune (const unsigned char *str, size_t len, int *rune)
{
  int c, c1, c2, c3;

  /* Default to the "replacement character".  */
  *rune = 0xfffd;

  if (len <= 0)
    return 1;

  c = *str;
  if (c <= 0x7f)
    {
      *rune = c;
      return 1;
    }

  if (len <= 1)
    return 1;

  c1 = str[1];
  if ((c & 0xe0) == 0xc0
      && (c1 & 0xc0) == 0x80)
    {
      *rune = (((c & 0x1f) << 6)
	       + (c1 & 0x3f));
      return 2;
    }

  if (len <= 2)
    return 1;

  c2 = str[2];
  if ((c & 0xf0) == 0xe0
      && (c1 & 0xc0) == 0x80
      && (c2 & 0xc0) == 0x80)
    {
      *rune = (((c & 0xf) << 12)
	       + ((c1 & 0x3f) << 6)
	       + (c2 & 0x3f));

      if (*rune >= 0xd800 && *rune < 0xe000)
	{
	  /* Invalid surrogate half; return replace character.  */
	  *rune = 0xfffd;
	  return 1;
	}

      return 3;
    }

  if (len <= 3)
    return 1;

  c3 = str[3];
  if ((c & 0xf8) == 0xf0
      && (c1 & 0xc0) == 0x80
      && (c2 & 0xc0) == 0x80
      && (c3 & 0xc0) == 0x80)
    {
      *rune = (((c & 0x7) << 18)
	       + ((c1 & 0x3f) << 12)
	       + ((c2 & 0x3f) << 6)
	       + (c3 & 0x3f));
      return 4;
    }

  /* Invalid encoding.  Return 1 so that we advance.  */
  return 1;
}
