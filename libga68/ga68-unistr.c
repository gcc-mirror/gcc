/* libga68 unicode support routines.
   Copyright (C) 2009-2026 Free Software Foundation, Inc.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the Free Software
   Foundation; either version 3, or (at your option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   Under Section 7 of GPL version 3, you are granted additional permissions
   described in the GCC Runtime Library Exception, version 3.1, as published by
   the Free Software Foundation.

   You should have received a copy of the GNU General Public License and a copy
   of the GCC Runtime Library Exception along with this program; see the files
   COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* The code in this file has been copied from the unistr gnulib module, written
   by Bruno Haible, and adapted to support strides.  */

#include "ga68.h"

#include <stddef.h> /* For ptrdiff_t */
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>


/* CMP (n1, n2) performs a three-valued comparison on n1 vs. n2, where
   n1 and n2 are expressions without side effects, that evaluate to real
   numbers (excluding NaN).
   It returns
     1  if n1 > n2
     0  if n1 == n2
     -1 if n1 < n2
   The naïve code   (n1 > n2 ? 1 : n1 < n2 ? -1 : 0)  produces a conditional
   jump with nearly all GCC versions up to GCC 10.
   This variant     (n1 < n2 ? -1 : n1 > n2)  produces a conditional with many
   GCC versions up to GCC 9.
   The better code  (n1 > n2) - (n1 < n2)  from Hacker's Delight § 2-9
   avoids conditional jumps in all GCC versions >= 3.4.  */

#define CMP(n1, n2) (((n1) > (n2)) - ((n1) < (n2)))

/* MIN(a,b) returns the minimum of A and B.  */

#ifndef MIN
# define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

/* Compare two UCS-4 strings of same lenght, lexicographically.
   Return -1, 0 or 1.  */

static int
_libga68_u32_cmp (const uint32_t *s1, size_t stride1,
		  const uint32_t *s2, size_t stride2,
		  size_t n)
{
  stride1 = stride1 / sizeof (uint32_t);
  stride2 = stride2 / sizeof (uint32_t);
  
  for (; n > 0;)
    {
      uint32_t uc1 = *s1;
      s1 += stride1;
      uint32_t uc2 = *s2;
      s2 += stride2;
      if (uc1 == uc2)
        {
          n--;
          continue;
        }
      /* Note that uc1 and uc2 each have at most 31 bits. */
      return (int)uc1 - (int)uc2;
      /* > 0 if uc1 > uc2, < 0 if uc1 < uc2. */
    }
  return 0;
}

/* Compare two UCS-4 strings of perhaps different lenghts, lexicographically.
   Return -1, 0 or 1.  */

int
_libga68_u32_cmp2 (const uint32_t *s1, size_t n1, size_t stride1,
		   const uint32_t *s2, size_t n2, size_t stride2)
{
  int cmp = _libga68_u32_cmp (s1, stride1, s2, stride2, MIN (n1, n2));

  if (cmp == 0)
    cmp = CMP (n1, n2);

  return cmp;
}

/* Get the UCS code for the first character of a given UTF-8 string.  */

int
_libga68_u8_mbtouc (uint32_t *puc, const uint8_t *s, size_t n)
{
  uint8_t c = *s;

  if (c < 0x80)
    {
      *puc = c;
      return 1;
    }
  else if (c >= 0xc2)
    {
      if (c < 0xe0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  *puc = ((unsigned int) (c & 0x1f) << 6)
                         | (unsigned int) (s[1] ^ 0x80);
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return 1;
            }
        }
      else if (c < 0xf0)
        {
          if (n >= 3)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xe1 || s[1] >= 0xa0)
                  && (c != 0xed || s[1] < 0xa0))
                {
                  if ((s[2] ^ 0x80) < 0x40)
                    {
                      *puc = ((unsigned int) (c & 0x0f) << 12)
                             | ((unsigned int) (s[1] ^ 0x80) << 6)
                             | (unsigned int) (s[2] ^ 0x80);
                      return 3;
                    }
                  /* invalid multibyte character */
                  *puc = 0xfffd;
                  return 2;
                }
              /* invalid multibyte character */
              *puc = 0xfffd;
              return 1;
            }
          else
            {
              *puc = 0xfffd;
              if (n == 1)
                {
                  /* incomplete multibyte character */
                  return 1;
                }
              else
                {
                  if ((s[1] ^ 0x80) < 0x40
                      && (c >= 0xe1 || s[1] >= 0xa0)
                      && (c != 0xed || s[1] < 0xa0))
                    {
                      /* incomplete multibyte character */
                      return 2;
                    }
                  else
                    {
                      /* invalid multibyte character */
                      return 1;
                    }
                }
            }
        }
      else if (c <= 0xf4)
        {
          if (n >= 4)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xf1 || s[1] >= 0x90)
                  && (c < 0xf4 || (/* c == 0xf4 && */ s[1] < 0x90)))
                {
                  if ((s[2] ^ 0x80) < 0x40)
                    {
                      if ((s[3] ^ 0x80) < 0x40)
                        {
                          *puc = ((unsigned int) (c & 0x07) << 18)
                                 | ((unsigned int) (s[1] ^ 0x80) << 12)
                                 | ((unsigned int) (s[2] ^ 0x80) << 6)
                                 | (unsigned int) (s[3] ^ 0x80);
                          return 4;
                        }
                      /* invalid multibyte character */
                      *puc = 0xfffd;
                      return 3;
                    }
                  /* invalid multibyte character */
                  *puc = 0xfffd;
                  return 2;
                }
              /* invalid multibyte character */
              *puc = 0xfffd;
              return 1;
            }
          else
            {
              *puc = 0xfffd;
              if (n == 1)
                {
                  /* incomplete multibyte character */
                  return 1;
                }
              else
                {
                  if ((s[1] ^ 0x80) < 0x40
                      && (c >= 0xf1 || s[1] >= 0x90)
                      && (c < 0xf4 || (/* c == 0xf4 && */ s[1] < 0x90)))
                    {
                      if (n == 2)
                        {
                          /* incomplete multibyte character */
                          return 2;
                        }
                      else
                        {
                          if ((s[2] ^ 0x80) < 0x40)
                            {
                              /* incomplete multibyte character */
                              return 3;
                            }
                          else
                            {
                              /* invalid multibyte character */
                              return 2;
                            }
                        }
                    }
                  else
                    {
                      /* invalid multibyte character */
                      return 1;
                    }
                }
            }
        }
    }
  /* invalid multibyte character */
  *puc = 0xfffd;
  return 1;
}

/* Encode a given UCS code in UTF-8.  */

int
_libga68_u8_uctomb (uint8_t *s, uint32_t uc, ptrdiff_t n)
{
  if (uc < 0x80)
    {
      if (n > 0)
        {
          s[0] = uc;
          return 1;
        }
      /* else return -2, below.  */
    }
  else
    {
      int count;

      if (uc < 0x800)
        count = 2;
      else if (uc < 0x10000)
        {
          if (uc < 0xd800 || uc >= 0xe000)
            count = 3;
          else
            return -1;
        }
      else if (uc < 0x110000)
        count = 4;
      else
        return -1;

      if (n >= count)
        {
          switch (count) /* note: code falls through cases! */
            {
            case 4: s[3] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x10000;
              /* Fallthrough.  */
            case 3: s[2] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0x800;
	      /* Fallthrough.  */
            case 2: s[1] = 0x80 | (uc & 0x3f); uc = uc >> 6; uc |= 0xc0;
          /*case 1:*/ s[0] = uc;
            }
          return count;
        }
    }
  return -2;
}

/* Convert UCS-4 to UTF-8  */

uint8_t *
_libga68_u32_to_u8 (const uint32_t *s, size_t n, size_t stride,
		    uint8_t *resultbuf, size_t *lengthp)
{
  const uint32_t *s_end;
  /* Output string accumulator.  */
  uint8_t *result;
  size_t allocated;
  size_t length;

  stride = stride / sizeof (uint32_t);
  s_end = s + (n * stride);
  
  if (resultbuf != NULL)
    {
      result = resultbuf;
      allocated = *lengthp;
    }
  else
    {
      result = NULL;
      allocated = 0;
    }
  length = 0;
  /* Invariants:
     result is either == resultbuf or == NULL or malloc-allocated.
     If length > 0, then result != NULL.  */

  while (s < s_end)
    {
      uint32_t uc;
      int count;

      /* Fetch a Unicode character from the input string.  */
      uc = *s;
      s += stride;
      /* No need to call the safe variant u32_mbtouc, because
         u8_uctomb will verify uc anyway.  */

      /* Store it in the output string.  */
      count = _libga68_u8_uctomb (result + length, uc, allocated - length);
      if (count == -1)
        {
          if (!(result == resultbuf || result == NULL))
            free (result);
          errno = EILSEQ;
          return NULL;
        }
      if (count == -2)
        {
          uint8_t *memory;

          allocated = (allocated > 0 ? 2 * allocated : 12);
          if (length + 6 > allocated)
            allocated = length + 6;
          if (result == resultbuf || result == NULL)
	    memory = (uint8_t *) _libga68_malloc (allocated * sizeof (uint8_t));
          else
	    memory =
	      (uint8_t *) _libga68_realloc (result, allocated * sizeof (uint8_t));

          if (result == resultbuf && length > 0)
            memcpy ((char *) memory, (char *) result,
                    length * sizeof (uint8_t));
          result = memory;
          count = _libga68_u8_uctomb (result + length, uc, allocated - length);
          if (count < 0)
            abort ();
        }
      length += count;
    }

  if (length == 0)
    {
      if (result == NULL)
        {
          /* Return a non-NULL value.  NULL means error.  */
          result = (uint8_t *) _libga68_malloc (1);
          if (result == NULL)
            {
              errno = ENOMEM;
              return NULL;
            }
        }
    }
  else if (result != resultbuf && length < allocated)
    {
      /* Shrink the allocated memory if possible.  */
      uint8_t *memory;

      memory = (uint8_t *) _libga68_realloc_unchecked (result, length * sizeof (uint8_t));
      if (memory != NULL)
        result = memory;
    }

  *lengthp = length;
  return result;
}

/* Used by ga68_u8_to_u32 below.  */

static int
_libga68_u8_mbtoucr (uint32_t *puc, const uint8_t *s, size_t n)
{
  uint8_t c = *s;

  if (c < 0x80)
    {
      *puc = c;
      return 1;
    }
  else if (c >= 0xc2)
    {
      if (c < 0xe0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40)
                {
                  *puc = ((unsigned int) (c & 0x1f) << 6)
                         | (unsigned int) (s[1] ^ 0x80);
                  return 2;
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return -2;
            }
        }
      else if (c < 0xf0)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xe1 || s[1] >= 0xa0)
                  && (c != 0xed || s[1] < 0xa0))
                {
                  if (n >= 3)
                    {
                      if ((s[2] ^ 0x80) < 0x40)
                        {
                          *puc = ((unsigned int) (c & 0x0f) << 12)
                                 | ((unsigned int) (s[1] ^ 0x80) << 6)
                                 | (unsigned int) (s[2] ^ 0x80);
                          return 3;
                        }
                      /* invalid multibyte character */
                    }
                  else
                    {
                      /* incomplete multibyte character */
                      *puc = 0xfffd;
                      return -2;
                    }
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return -2;
            }
        }
      else if (c <= 0xf4)
        {
          if (n >= 2)
            {
              if ((s[1] ^ 0x80) < 0x40
                  && (c >= 0xf1 || s[1] >= 0x90)
                  && (c < 0xf4 || (/* c == 0xf4 && */ s[1] < 0x90)))
                {
                  if (n >= 3)
                    {
                      if ((s[2] ^ 0x80) < 0x40)
                        {
                          if (n >= 4)
                            {
                              if ((s[3] ^ 0x80) < 0x40)
                                {
                                  *puc = ((unsigned int) (c & 0x07) << 18)
                                         | ((unsigned int) (s[1] ^ 0x80) << 12)
                                         | ((unsigned int) (s[2] ^ 0x80) << 6)
                                         | (unsigned int) (s[3] ^ 0x80);
                                  return 4;
                                }
                              /* invalid multibyte character */
                            }
                          else
                            {
                              /* incomplete multibyte character */
                              *puc = 0xfffd;
                              return -2;
                            }
                        }
                      /* invalid multibyte character */
                    }
                  else
                    {
                      /* incomplete multibyte character */
                      *puc = 0xfffd;
                      return -2;
                    }
                }
              /* invalid multibyte character */
            }
          else
            {
              /* incomplete multibyte character */
              *puc = 0xfffd;
              return -2;
            }
        }
    }
  /* invalid multibyte character */
  *puc = 0xfffd;
  return -1;
}

/* Convert UTF-8 to UTF-32/UCS-4  */

uint32_t *
_libga68_u8_to_u32 (const uint8_t *s, size_t n, uint32_t *resultbuf, size_t *lengthp)
{
  const uint8_t *s_end = s + n;
  /* Output string accumulator.  */
  uint32_t *result;
  size_t allocated;
  size_t length;

  if (resultbuf != NULL)
    {
      result = resultbuf;
      allocated = *lengthp;
    }
  else
    {
      result = NULL;
      allocated = 0;
    }
  length = 0;
  /* Invariants:
     result is either == resultbuf or == NULL or malloc-allocated.
     If length > 0, then result != NULL.  */

  while (s < s_end)
    {
      uint32_t uc;
      int count;

      /* Fetch a Unicode character from the input string.  */
      count = _libga68_u8_mbtoucr (&uc, s, s_end - s);
      if (count < 0)
        {
          if (!(result == resultbuf || result == NULL))
            free (result);
          errno = EILSEQ;
          return NULL;
        }
      s += count;

      /* Store it in the output string.  */
      if (length + 1 > allocated)
        {
          uint32_t *memory;

          allocated = (allocated > 0 ? 2 * allocated : 12);
          if (length + 1 > allocated)
            allocated = length + 1;
          if (result == resultbuf || result == NULL)
	    memory = (uint32_t *) _libga68_malloc (allocated * sizeof (uint32_t));
          else
	    memory =
	      (uint32_t *) _libga68_realloc (result, allocated * sizeof (uint32_t));

          if (result == resultbuf && length > 0)
            memcpy ((char *) memory, (char *) result,
                    length * sizeof (uint32_t));
          result = memory;
        }
      result[length++] = uc;
    }

  if (length == 0)
    {
      if (result == NULL)
        {
          /* Return a non-NULL value.  NULL means error.  */
          result = (uint32_t *) _libga68_malloc (1);
        }
    }
  else if (result != resultbuf && length < allocated)
    {
      /* Shrink the allocated memory if possible.  */
      uint32_t *memory;

      memory = (uint32_t *) _libga68_realloc_unchecked (result, length * sizeof (uint32_t));
      if (memory != NULL)
        result = memory;
    }

  *lengthp = length;
  return result;
}
