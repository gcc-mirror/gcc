/* Internal function for converting integers to ASCII.
   Copyright (C) 1994,95,96,97,98,99,2002,2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#ifndef _ITOWA_H
#define _ITOWA_H	1

/* Convert VALUE into ASCII in base BASE (2..16).
   Write backwards starting the character just before BUFLIM.
   Return the address of the first (left-to-right) character in the number.
   Use upper case letters iff UPPER_CASE is nonzero.  */

static const wchar_t _itowa_lower_digits[16] = L_("0123456789abcdef");
static const wchar_t _itowa_upper_digits[16] = L_("0123456789ABCDEF");

static inline wchar_t *
__attribute__ ((unused, always_inline))
_itowa_word (unsigned long value, wchar_t *buflim,
	     unsigned int base, int upper_case)
{
  const wchar_t *digits = (upper_case
			   ? _itowa_upper_digits : _itowa_lower_digits);
  wchar_t *bp = buflim;

  switch (base)
    {
#define SPECIAL(Base)							      \
    case Base:								      \
      do								      \
	*--bp = digits[value % Base];					      \
      while ((value /= Base) != 0);					      \
      break

      SPECIAL (10);
      SPECIAL (16);
      SPECIAL (8);
    default:
      do
	*--bp = digits[value % base];
      while ((value /= base) != 0);
    }
  return bp;
}

static inline wchar_t *
__attribute__ ((unused, always_inline))
_itowa (uint64_t value, wchar_t *buflim,
	unsigned int base, int upper_case)
{
  const wchar_t *digits = (upper_case
			   ? _itowa_upper_digits : _itowa_lower_digits);
  wchar_t *bp = buflim;

  switch (base)
    {
      SPECIAL (10);
      SPECIAL (16);
      SPECIAL (8);
    default:
      do
	*--bp = digits[value % base];
      while ((value /= base) != 0);
    }
  return bp;
}
#undef SPECIAL

#endif	/* itowa.h */
