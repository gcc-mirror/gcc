/* Helper functions in C for IEEE modules
   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgfortran.h"

/* Prototypes.  */

extern int ieee_class_helper_4 (GFC_REAL_4 *);
internal_proto(ieee_class_helper_4);

extern int ieee_class_helper_8 (GFC_REAL_8 *);
internal_proto(ieee_class_helper_8);

extern int ieee_is_finite_4_ (GFC_REAL_4 *);
export_proto(ieee_is_finite_4_);

extern int ieee_is_finite_8_ (GFC_REAL_8 *);
export_proto(ieee_is_finite_8_);

extern int ieee_is_nan_4_ (GFC_REAL_4 *);
export_proto(ieee_is_nan_4_);

extern int ieee_is_nan_8_ (GFC_REAL_8 *);
export_proto(ieee_is_nan_8_);

extern int ieee_is_negative_4_ (GFC_REAL_4 *);
export_proto(ieee_is_negative_4_);

extern int ieee_is_negative_8_ (GFC_REAL_8 *);
export_proto(ieee_is_negative_8_);

extern int ieee_is_normal_4_ (GFC_REAL_4 *);
export_proto(ieee_is_normal_4_);

extern int ieee_is_normal_8_ (GFC_REAL_8 *);
export_proto(ieee_is_normal_8_);


/* Enumeration of the possible floating-point types. These values
   correspond to the hidden arguments of the IEEE_CLASS_TYPE
   derived-type of IEEE_ARITHMETIC.  */

enum { IEEE_OTHER_VALUE = 0, IEEE_SIGNALING_NAN, IEEE_QUIET_NAN,
  IEEE_NEGATIVE_INF, IEEE_NEGATIVE_NORMAL, IEEE_NEGATIVE_DENORMAL,
  IEEE_NEGATIVE_ZERO, IEEE_POSITIVE_ZERO, IEEE_POSITIVE_DENORMAL,
  IEEE_POSITIVE_NORMAL, IEEE_POSITIVE_INF };

#define CLASSMACRO(TYPE) \
  int ieee_class_helper_ ## TYPE (GFC_REAL_ ## TYPE *value) \
  { \
    int res = __builtin_fpclassify (IEEE_QUIET_NAN, IEEE_POSITIVE_INF, \
				    IEEE_POSITIVE_NORMAL, \
				    IEEE_POSITIVE_DENORMAL, \
				    IEEE_POSITIVE_ZERO, *value); \
 \
    if (__builtin_signbit (*value)) \
    { \
      if (res == IEEE_POSITIVE_NORMAL) \
	return IEEE_NEGATIVE_NORMAL; \
      else if (res == IEEE_POSITIVE_DENORMAL) \
	return IEEE_NEGATIVE_DENORMAL; \
      else if (res == IEEE_POSITIVE_ZERO) \
	return IEEE_NEGATIVE_ZERO; \
      else if (res == IEEE_POSITIVE_INF) \
	return IEEE_NEGATIVE_INF; \
    } \
 \
    if (res == IEEE_QUIET_NAN) \
    { \
      /* TODO: Handle signaling NaNs  */ \
      return res; \
    } \
 \
    return res; \
  }

CLASSMACRO(4)
CLASSMACRO(8)


/* Testing functions.  */

int ieee_is_finite_4_ (GFC_REAL_4 *val)
{
  return __builtin_isfinite(*val) ? 1 : 0;
}

int ieee_is_finite_8_ (GFC_REAL_8 *val)
{
  return __builtin_isfinite(*val) ? 1 : 0;
}

int ieee_is_nan_4_ (GFC_REAL_4 *val)
{
  return __builtin_isnan(*val) ? 1 : 0;
}

int ieee_is_nan_8_ (GFC_REAL_8 *val)
{
  return __builtin_isnan(*val) ? 1 : 0;
}

int ieee_is_negative_4_ (GFC_REAL_4 *val)
{
  return (__builtin_signbit(*val) && !__builtin_isnan(*val)) ? 1 : 0;
}

int ieee_is_negative_8_ (GFC_REAL_8 *val)
{
  return (__builtin_signbit(*val) && !__builtin_isnan(*val)) ? 1 : 0;
}

int ieee_is_normal_4_ (GFC_REAL_4 *val)
{
  return (__builtin_isnormal(*val) || *val == 0) ? 1 : 0;
}

int ieee_is_normal_8_ (GFC_REAL_8 *val)
{
  return (__builtin_isnormal(*val) || *val == 0) ? 1 : 0;
}

GFC_REAL_4 ieee_copy_sign_4_4_ (GFC_REAL_4 *, GFC_REAL_4 *);
export_proto(ieee_copy_sign_4_4_);
GFC_REAL_4 ieee_copy_sign_4_4_ (GFC_REAL_4 *x, GFC_REAL_4 *y)
{
  GFC_REAL_4 s = __builtin_signbit(*y) ? -1 : 1;
  return __builtin_copysign(*x, s);
}

GFC_REAL_4 ieee_copy_sign_4_8_ (GFC_REAL_4 *, GFC_REAL_8 *);
export_proto(ieee_copy_sign_4_8_);
GFC_REAL_4 ieee_copy_sign_4_8_ (GFC_REAL_4 *x, GFC_REAL_8 *y)
{
  GFC_REAL_4 s = __builtin_signbit(*y) ? -1 : 1;
  return __builtin_copysign(*x, s);
}

GFC_REAL_8 ieee_copy_sign_8_4_ (GFC_REAL_8 *, GFC_REAL_4 *);
export_proto(ieee_copy_sign_8_4_);
GFC_REAL_8 ieee_copy_sign_8_4_ (GFC_REAL_8 *x, GFC_REAL_4 *y)
{
  GFC_REAL_8 s = __builtin_signbit(*y) ? -1 : 1;
  return __builtin_copysign(*x, s);
}

GFC_REAL_8 ieee_copy_sign_8_8_ (GFC_REAL_8 *, GFC_REAL_8 *);
export_proto(ieee_copy_sign_8_8_);
GFC_REAL_8 ieee_copy_sign_8_8_ (GFC_REAL_8 *x, GFC_REAL_8 *y)
{
  GFC_REAL_8 s = __builtin_signbit(*y) ? -1 : 1;
  return __builtin_copysign(*x, s);
}

int ieee_unordered_4_4_ (GFC_REAL_4 *, GFC_REAL_4 *);
export_proto(ieee_unordered_4_4_);
int ieee_unordered_4_4_ (GFC_REAL_4 *x, GFC_REAL_4 *y)
{
  return __builtin_isunordered(*x, *y);
}

int ieee_unordered_4_8_ (GFC_REAL_4 *, GFC_REAL_8 *);
export_proto(ieee_unordered_4_8_);
int ieee_unordered_4_8_ (GFC_REAL_4 *x, GFC_REAL_8 *y)
{
  return __builtin_isunordered(*x, *y);
}

int ieee_unordered_8_4_ (GFC_REAL_8 *, GFC_REAL_4 *);
export_proto(ieee_unordered_8_4_);
int ieee_unordered_8_4_ (GFC_REAL_8 *x, GFC_REAL_4 *y)
{
  return __builtin_isunordered(*x, *y);
}

int ieee_unordered_8_8_ (GFC_REAL_8 *, GFC_REAL_8 *);
export_proto(ieee_unordered_8_8_);
int ieee_unordered_8_8_ (GFC_REAL_8 *x, GFC_REAL_8 *y)
{
  return __builtin_isunordered(*x, *y);
}


/* Arithmetic functions (LOGB, NEXT_AFTER, REM, RINT, SCALB).  */

GFC_REAL_4 ieee_logb_4_ (GFC_REAL_4 *);
export_proto(ieee_logb_4_);

GFC_REAL_4 ieee_logb_4_ (GFC_REAL_4 *x)
{
  GFC_REAL_4 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_logb (*x);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_8 ieee_logb_8_ (GFC_REAL_8 *);
export_proto(ieee_logb_8_);

GFC_REAL_8 ieee_logb_8_ (GFC_REAL_8 *x)
{
  GFC_REAL_8 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_logb (*x);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_4 ieee_next_after_4_4_ (GFC_REAL_4 *, GFC_REAL_4 *);
export_proto(ieee_next_after_4_4_);

GFC_REAL_4 ieee_next_after_4_4_ (GFC_REAL_4 *x, GFC_REAL_4 *y)
{
  return __builtin_nextafterf (*x, *y);
}

GFC_REAL_4 ieee_next_after_4_8_ (GFC_REAL_4 *, GFC_REAL_8 *);
export_proto(ieee_next_after_4_8_);

GFC_REAL_4 ieee_next_after_4_8_ (GFC_REAL_4 *x, GFC_REAL_8 *y)
{
  return __builtin_nextafterf (*x, *y);
}

GFC_REAL_8 ieee_next_after_8_4_ (GFC_REAL_8 *, GFC_REAL_4 *);
export_proto(ieee_next_after_8_4_);

GFC_REAL_8 ieee_next_after_8_4_ (GFC_REAL_8 *x, GFC_REAL_4 *y)
{
  return __builtin_nextafter (*x, *y);
}

GFC_REAL_8 ieee_next_after_8_8_ (GFC_REAL_8 *, GFC_REAL_8 *);
export_proto(ieee_next_after_8_8_);

GFC_REAL_8 ieee_next_after_8_8_ (GFC_REAL_8 *x, GFC_REAL_8 *y)
{
  return __builtin_nextafter (*x, *y);
}

GFC_REAL_4 ieee_rem_4_4_ (GFC_REAL_4 *, GFC_REAL_4 *);
export_proto(ieee_rem_4_4_);

GFC_REAL_4 ieee_rem_4_4_ (GFC_REAL_4 *x, GFC_REAL_4 *y)
{
  GFC_REAL_4 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_remainderf (*x, *y);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_8 ieee_rem_4_8_ (GFC_REAL_4 *, GFC_REAL_8 *);
export_proto(ieee_rem_4_8_);

GFC_REAL_8 ieee_rem_4_8_ (GFC_REAL_4 *x, GFC_REAL_8 *y)
{
  GFC_REAL_8 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_remainder (*x, *y);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_8 ieee_rem_8_4_ (GFC_REAL_8 *, GFC_REAL_4 *);
export_proto(ieee_rem_8_4_);

GFC_REAL_8 ieee_rem_8_4_ (GFC_REAL_8 *x, GFC_REAL_4 *y)
{
  GFC_REAL_8 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_remainder (*x, *y);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_8 ieee_rem_8_8_ (GFC_REAL_8 *, GFC_REAL_8 *);
export_proto(ieee_rem_8_8_);

GFC_REAL_8 ieee_rem_8_8_ (GFC_REAL_8 *x, GFC_REAL_8 *y)
{
  GFC_REAL_8 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_remainder (*x, *y);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_4 ieee_rint_4_ (GFC_REAL_4 *);
export_proto(ieee_rint_4_);

GFC_REAL_4 ieee_rint_4_ (GFC_REAL_4 *x)
{
  GFC_REAL_4 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_rint (*x);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_8 ieee_rint_8_ (GFC_REAL_8 *);
export_proto(ieee_rint_8_);

GFC_REAL_8 ieee_rint_8_ (GFC_REAL_8 *x)
{
  GFC_REAL_8 res;
  char buffer[GFC_FPE_STATE_BUFFER_SIZE];

  get_fpu_state (buffer);
  res = __builtin_rint (*x);
  set_fpu_state (buffer);
  return res;
}

GFC_REAL_4 ieee_scalb_4_ (GFC_REAL_4 *, int *);
export_proto(ieee_scalb_4_);

GFC_REAL_4 ieee_scalb_4_ (GFC_REAL_4 *x, int *i)
{
  return __builtin_scalbnf (*x, *i);
}

GFC_REAL_8 ieee_scalb_8_ (GFC_REAL_8 *, int *);
export_proto(ieee_scalb_8_);

GFC_REAL_8 ieee_scalb_8_ (GFC_REAL_8 *x, int *i)
{
  return __builtin_scalbn (*x, *i);
}


#define GFC_FPE_ALL (GFC_FPE_INVALID | GFC_FPE_DENORMAL | \
		     GFC_FPE_ZERO | GFC_FPE_OVERFLOW | \
		     GFC_FPE_UNDERFLOW | GFC_FPE_INEXACT)

/* Functions to save and restore floating-point state, clear and restore
   exceptions on procedure entry/exit.  The rules we follow are set
   in Fortran 2008's 14.3 paragraph 3, note 14.4, 14.4 paragraph 4,
   14.5 paragraph 2, and 14.6 paragraph 1.  */

void ieee_procedure_entry (void *);
export_proto(ieee_procedure_entry);

void
ieee_procedure_entry (void *state)
{
  /* Save the floating-point state in the space provided by the caller.  */
  get_fpu_state (state);

  /* Clear the floating-point exceptions.  */
  set_fpu_except_flags (0, GFC_FPE_ALL);
}


void ieee_procedure_exit (void *);
export_proto(ieee_procedure_exit);

void
ieee_procedure_exit (void *state)
{
  /* Get the flags currently signaling.  */
  int flags = get_fpu_except_flags ();

  /* Restore the floating-point state we had on entry.  */
  set_fpu_state (state);

  /* And re-raised the flags that were raised since entry.  */
  set_fpu_except_flags (flags, 0);
}

