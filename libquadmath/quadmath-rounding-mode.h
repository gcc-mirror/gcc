/* GCC Quad-Precision Math Library
   Copyright (C) 2012 Free Software Foundation, Inc.

This file is part of the libquadmath library.
Libquadmath is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libquadmath is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libquadmath; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1301, USA.  */


#ifndef QUADMATH_ROUNDING_MODE_H
#define QUADMATH_ROUNDING_MODE_H

#include <stdbool.h>


#if defined(HAVE_FENV_H)
#  include <fenv.h>
#endif /* HAVE_FENV_H */

static inline int
get_rounding_mode (void)
{
#if defined(HAVE_FENV_H) && (defined(FE_DOWNWARD) || defined(FE_TONEAREST) \
			     || defined(FE_TOWARDZERO) || defined(FE_UPWARD))
  return fegetround ();
#else
  return 0;
#endif
}

static inline bool
round_away (bool negative, bool last_digit_odd, bool half_bit, bool more_bits,
            int mode)
{
  switch (mode)
    {
#ifdef FE_DOWNWARD
    case FE_DOWNWARD:
      return negative && (half_bit || more_bits);
#endif

#ifdef FE_DOWNWARD
    case FE_TONEAREST:
      return half_bit && (last_digit_odd || more_bits);
#endif

#ifdef FE_TOWARDZERO
    case FE_TOWARDZERO:
      return false;
#endif


#ifdef FE_UPWARD
    case FE_UPWARD:
      return !negative && (half_bit || more_bits);
#endif

    default:
      return false;
    }
}

#endif /* QUADMATH_ROUNDING_MODE_H  */
