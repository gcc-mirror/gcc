/* Helper functions in C for IEEE modules
   Copyright (C) 2013-2021 Free Software Foundation, Inc.
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

#ifdef HAVE_GFC_REAL_10
extern int ieee_class_helper_10 (GFC_REAL_10 *);
internal_proto(ieee_class_helper_10);
#endif

#ifdef HAVE_GFC_REAL_16
extern int ieee_class_helper_16 (GFC_REAL_16 *);
internal_proto(ieee_class_helper_16);
#endif

/* Enumeration of the possible floating-point types. These values
   correspond to the hidden arguments of the IEEE_CLASS_TYPE
   derived-type of IEEE_ARITHMETIC.  */

enum { IEEE_OTHER_VALUE = 0, IEEE_SIGNALING_NAN, IEEE_QUIET_NAN,
  IEEE_NEGATIVE_INF, IEEE_NEGATIVE_NORMAL, IEEE_NEGATIVE_DENORMAL,
  IEEE_NEGATIVE_ZERO, IEEE_POSITIVE_ZERO, IEEE_POSITIVE_DENORMAL,
  IEEE_POSITIVE_NORMAL, IEEE_POSITIVE_INF, IEEE_SUBNORMAL,
  IEEE_NEGATIVE_SUBNORMAL, IEEE_POSITIVE_SUBNORMAL };

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

#ifdef HAVE_GFC_REAL_10
CLASSMACRO(10)
#endif

#ifdef HAVE_GFC_REAL_16
CLASSMACRO(16)
#endif


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

