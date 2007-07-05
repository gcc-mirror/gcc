/* Copyright (C) 2007  Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "bid_conf.h"
#include "bid_functions.h"

#if DECIMAL_GLOBAL_ROUNDING
BID_THREAD _IDEC_round __bid_IDEC_glbround = ROUNDING_TO_NEAREST;

void
__dfp_set_round (int mode)
{
  __bid_IDEC_glbround = mode;
}

int
__dfp_get_round (void)
{
  return __bid_IDEC_glbround;
}
#endif

#if DECIMAL_GLOBAL_EXCEPTION_FLAGS
BID_THREAD _IDEC_flags __bid_IDEC_glbflags = EXACT_STATUS;

#include <fenv.h>

void
__dfp_clear_except (void)
{
  __bid_IDEC_glbflags &= ~FLAG_MASK;
}

int
__dfp_test_except (int mask)
{
  int flags = 0;

  if ((__bid_IDEC_glbflags & INEXACT_EXCEPTION) != 0)
    flags |= mask & FE_INEXACT;
  if ((__bid_IDEC_glbflags & UNDERFLOW_EXCEPTION) != 0)
    flags |= mask & FE_UNDERFLOW;
  if ((__bid_IDEC_glbflags & OVERFLOW_EXCEPTION) != 0)
    flags |= mask & FE_OVERFLOW;
  if ((__bid_IDEC_glbflags & ZERO_DIVIDE_EXCEPTION) != 0)
    flags |= mask & FE_DIVBYZERO;
  if ((__bid_IDEC_glbflags & INVALID_EXCEPTION) != 0)
    flags |= mask & FE_INVALID;

  return flags;
}

void
__dfp_raise_except (int mask)
{
  _IDEC_flags flags = 0;

  if ((mask & FE_INEXACT) != 0)
    flags |= INEXACT_EXCEPTION;
  if ((mask & FE_UNDERFLOW) != 0)
    flags |= UNDERFLOW_EXCEPTION;
  if ((mask & FE_OVERFLOW) != 0)
    flags |= OVERFLOW_EXCEPTION;
  if ((mask & FE_DIVBYZERO) != 0)
    flags |= ZERO_DIVIDE_EXCEPTION;
  if ((mask & FE_INVALID) != 0)
    flags |= INVALID_EXCEPTION;

  __bid_IDEC_glbflags |= flags;
}

#endif

#if DECIMAL_ALTERNATE_EXCEPTION_HANDLING
  #if DECIMAL_GLOBAL_EXCEPTION_MASKS
    _IDEC_exceptionmasks _IDEC_glbexceptionmasks = _IDEC_allexcmasksset;
  #endif
  #if DECIMAL_GLOBAL_EXCEPTION_INFO
    _IDEC_excepthandling _IDEC_glbexcepthandling;
  #endif
#endif
