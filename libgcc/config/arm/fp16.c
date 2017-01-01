/* Half-float conversion routines.

   Copyright (C) 2008-2017 Free Software Foundation, Inc.
   Contributed by CodeSourcery.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

struct format
{
  /* Number of bits.  */
  unsigned long long size;
  /* Exponent bias.  */
  unsigned long long bias;
  /* Exponent width in bits.  */
  unsigned long long exponent;
  /* Significand precision in explicitly stored bits.  */
  unsigned long long significand;
};

static const struct format
binary32 =
{
  32,   /* size.  */
  127,  /* bias.  */
  8,    /* exponent.  */
  23    /* significand.  */
};

static const struct format
binary64 =
{
  64,    /* size.  */
  1023,  /* bias.  */
  11,    /* exponent.  */
  52     /* significand.  */
};

static inline unsigned short
__gnu_float2h_internal (const struct format* fmt,
			unsigned long long a, int ieee)
{
  unsigned long long point = 1ULL << fmt->significand;
  unsigned short sign = (a >> (fmt->size - 16)) & 0x8000;
  int aexp;
  unsigned long long mantissa;
  unsigned long long mask;
  unsigned long long increment;

  /* Get the exponent and mantissa encodings.  */
  mantissa = a & (point - 1);

  mask = (1 << fmt->exponent) - 1;
  aexp = (a >> fmt->significand) & mask;

  /* Infinity, NaN and alternative format special case.  */
  if (((unsigned int) aexp) == mask)
    {
      if (!ieee)
	return sign;
      if (mantissa == 0)
	return sign | 0x7c00;	/* Infinity.  */
      /* Remaining cases are NaNs.  Convert SNaN to QNaN.  */
      return sign | 0x7e00 | (mantissa >> (fmt->significand - 10));
    }

  /* Zero.  */
  if (aexp == 0 && mantissa == 0)
    return sign;

  /* Construct the exponent and mantissa.  */
  aexp -= fmt->bias;

  /* Decimal point is immediately after the significand.  */
  mantissa |= point;

  if (aexp < -14)
    {
      mask = point | (point - 1);
      /* Minimum exponent for half-precision is 2^-24.  */
      if (aexp >= -25)
	mask >>= 25 + aexp;
    }
  else
    mask = (point - 1) >> 10;

  /* Round.  */
  if (mantissa & mask)
    {
      increment = (mask + 1) >> 1;
      if ((mantissa & mask) == increment)
	increment = mantissa & (increment << 1);
      mantissa += increment;
      if (mantissa >= (point << 1))
	{
	  mantissa >>= 1;
	  aexp++;
	}
    }

  if (ieee)
    {
      if (aexp > 15)
	return sign | 0x7c00;
    }
  else
    {
      if (aexp > 16)
	return sign | 0x7fff;
    }

  if (aexp < -24)
    return sign;

  if (aexp < -14)
    {
      mantissa >>= -14 - aexp;
      aexp = -14;
    }

  /* Encode the final 16-bit floating-point value.

     This is formed of the sign bit, the bias-adjusted exponent, and the
     calculated mantissa, with the following caveats:

     1.  The mantissa calculated after rounding could have a leading 1.
	 To compensate for this, subtract one from the exponent bias (15)
	 before adding it to the calculated exponent.
     2.  When we were calculating rounding, we left the mantissa with the
	 number of bits of the source operand, it needs reduced to ten
	 bits (+1 for the afforementioned leading 1) by shifting right by
	 the number of bits in the source mantissa - 10.
     3.  To ensure the leading 1 in the mantissa is applied to the exponent
	 we need to add the mantissa rather than apply an arithmetic "or"
	 to it.  */

  return sign | (((aexp + 14) << 10) + (mantissa >> (fmt->significand - 10)));
}

static inline unsigned short
__gnu_f2h_internal (unsigned int a, int ieee)
{
  return __gnu_float2h_internal (&binary32, (unsigned long long) a, ieee);
}

static inline unsigned short
__gnu_d2h_internal (unsigned long long a, int ieee)
{
  return __gnu_float2h_internal (&binary64, a, ieee);
}

unsigned int
__gnu_h2f_internal(unsigned short a, int ieee)
{
  unsigned int sign = (unsigned int)(a & 0x8000) << 16;
  int aexp = (a >> 10) & 0x1f;
  unsigned int mantissa = a & 0x3ff;

  if (aexp == 0x1f && ieee)
    return sign | 0x7f800000 | (mantissa << 13);

  if (aexp == 0)
    {
      int shift;

      if (mantissa == 0)
	return sign;

      shift = __builtin_clz(mantissa) - 21;
      mantissa <<= shift;
      aexp = -shift;
    }

  return sign | (((aexp + 0x70) << 23) + (mantissa << 13));
}

unsigned short
__gnu_f2h_ieee(unsigned int a)
{
  return __gnu_f2h_internal(a, 1);
}

unsigned int
__gnu_h2f_ieee(unsigned short a)
{
  return __gnu_h2f_internal(a, 1);
}

unsigned short
__gnu_f2h_alternative(unsigned int x)
{
  return __gnu_f2h_internal(x, 0);
}

unsigned int
__gnu_h2f_alternative(unsigned short a)
{
  return __gnu_h2f_internal(a, 0);
}

unsigned short
__gnu_d2h_ieee (unsigned long long a)
{
  return __gnu_d2h_internal (a, 1);
}

unsigned short
__gnu_d2h_alternative (unsigned long long x)
{
  return __gnu_d2h_internal (x, 0);
}
