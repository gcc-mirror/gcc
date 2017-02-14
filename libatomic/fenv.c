/* Copyright (C) 2012-2017 Free Software Foundation, Inc.

   This file is part of the GNU Atomic Library (libatomic).

   Libatomic is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libatomic is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "libatomic_i.h"

#ifdef HAVE_FENV_H
# include <fenv.h>
#endif

/* Raise the supported floating-point exceptions from EXCEPTS.  Other
   bits in EXCEPTS are ignored.  */

void
__atomic_feraiseexcept (int excepts __attribute__ ((unused)))
{
  volatile float r __attribute__ ((unused));
#ifdef FE_INVALID
  if (excepts & FE_INVALID)
  {
    volatile float zero = 0.0f;
    r = zero / zero;
  }
#endif
#ifdef FE_DIVBYZERO
  if (excepts & FE_DIVBYZERO)
    {
      volatile float zero = 0.0f;
      r = 1.0f / zero;
    }
#endif
#ifdef FE_OVERFLOW
  if (excepts & FE_OVERFLOW)
    {
      volatile float max = __FLT_MAX__;
      r = max * max;
    }
#endif
#ifdef FE_UNDERFLOW
  if (excepts & FE_UNDERFLOW)
    {
      volatile float min = __FLT_MIN__;
      r = min * min;
    }
#endif
#ifdef FE_INEXACT
  if (excepts & FE_INEXACT)
    {
      volatile float three = 3.0f;
      r = 1.0f / three;
    }
#endif
}
