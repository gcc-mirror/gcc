/* Internal testing support for rounding for decimal float.

   Copyright (C) 2005, 2006, 2009 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "dconfig.h"
#include "decContext.h"
#include "decRound.h"

/* Internal, non-documented functions for testing libgcc functions.
   This support is not sufficient for application use.  */

#define FE_DEC_DOWNWARD 0
#define FE_DEC_TONEAREST 1
#define FE_DEC_TONEARESTFROMZERO 2
#define FE_DEC_TOWARDZERO 3
#define FE_DEC_UPWARD 4
#define FE_DEC_MAX 5

static enum rounding __dfp_rounding_mode = DEC_ROUND_HALF_EVEN;

/* Set the decNumber rounding mode from the FE_DEC_* value in MODE.  */ 

void
__dfp_set_round (int mode)
{
  switch (mode)
    {
    case FE_DEC_DOWNWARD:
      __dfp_rounding_mode = DEC_ROUND_FLOOR; break;
    case FE_DEC_TONEAREST:
      __dfp_rounding_mode = DEC_ROUND_HALF_EVEN; break;
    case FE_DEC_TONEARESTFROMZERO:
      __dfp_rounding_mode = DEC_ROUND_HALF_UP; break;
    case FE_DEC_TOWARDZERO:
      __dfp_rounding_mode = DEC_ROUND_DOWN; break;
    case FE_DEC_UPWARD:
      __dfp_rounding_mode = DEC_ROUND_CEILING; break;
    default:
     /* We can't use assert in libgcc, so just return the default mode.  */
      __dfp_rounding_mode = DEC_ROUND_HALF_EVEN; break;
    }
}

/* Return the decNumber rounding mode as an FE_DEC_* value.  */

int
__dfp_get_round (void)
{
  int mode;

  switch (__dfp_rounding_mode)
    {
    case DEC_ROUND_FLOOR:
      mode = FE_DEC_DOWNWARD; break;
    case DEC_ROUND_HALF_EVEN:
      mode = FE_DEC_TONEAREST; break;
    case DEC_ROUND_HALF_UP:
      mode = FE_DEC_TONEARESTFROMZERO; break;
    case DEC_ROUND_DOWN:
      mode = FE_DEC_TOWARDZERO; break;
    case DEC_ROUND_CEILING:
      mode = FE_DEC_UPWARD; break;
    default:
      /* We shouldn't get here, but can't use assert in libgcc.  */
      mode = -1;
    }
  return mode;
}

/* Return the decNumber version of the current rounding mode.  */

enum rounding
__decGetRound (void)
{
  return __dfp_rounding_mode;
}
