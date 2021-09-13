/* Definitions of target machine for GNU compiler, for IBM S/390
   Copyright (C) 1999-2021 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com).

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

#ifndef __s390x__

#define EXPONENT_BIAS   127
#define MANTISSA_BITS   23
#define EXP(fp)         (((fp.l) >> MANTISSA_BITS) & 0xFF)
#define PRECISION       (MANTISSA_BITS + 1)
#define SIGNBIT         0x80000000
#define SIGN(fp)        ((fp.l) & SIGNBIT)
#define HIDDEN          (1 << MANTISSA_BITS)
#define MANT(fp)        (((fp.l) & 0x7FFFFF) | HIDDEN)
#define FRAC(fp)        ((fp.l) & 0x7FFFFF)
#define LLONG_MAX       9223372036854775807LL
#define LLONG_MIN       (-LLONG_MAX - 1LL)

typedef int DItype_x __attribute__ ((mode (DI)));
typedef unsigned int UDItype_x __attribute__ ((mode (DI)));
typedef int SItype_x __attribute__ ((mode (SI)));
typedef unsigned int USItype_x __attribute__ ((mode (SI)));

union float_long
  {
    float f;
    USItype_x l;
  };

static __inline__ void
fexceptdiv (float d, float e)
{
  __asm__ __volatile__ ("debr %0,%1" : : "f" (d), "f" (e) );
}

DItype_x __fixsfdi (float a1);

/* convert double to int */
DItype_x
__fixsfdi (float a1)
{
    register union float_long fl1;
    register int exp;
    register DItype_x l;

    fl1.f = a1;

    /* +/- 0, denormalized */
    if (!EXP (fl1))
      return 0;

    exp = EXP (fl1) - EXPONENT_BIAS - MANTISSA_BITS;

    /* number < 1 */
    if (exp <= -PRECISION)
      return 0;

    /* NaN */

    if ((EXP (fl1) == 0xff) && (FRAC (fl1) != 0)) /* NaN */
      {
	/* C99 Annex F.4 requires an "invalid" exception to be thrown.  */
	fexceptdiv (0.0, 0.0);
	return 0x8000000000000000ULL;
      }

    /* Number big number & +/- inf */
    if (exp >= 40) {      
      /* Don't throw an exception for -1p+63  */
      if (!SIGN (fl1) || exp > 40 || FRAC (fl1) != 0)
	/* C99 Annex F.4 requires an "invalid" exception to be thrown.  */
	fexceptdiv (0.0, 0.0);
      return SIGN (fl1) ? LLONG_MIN : LLONG_MAX;
    }

    l = MANT (fl1);

    if (exp > 0)
      l <<= exp;
    else
      l >>= -exp;

    return (SIGN (fl1) ? -l : l);
}
#endif /* !__s390x__ */
