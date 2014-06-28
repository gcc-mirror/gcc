/* FPU-related code for systems with GNU libc.
   Copyright (C) 2005-2014 Free Software Foundation, Inc.
   Contributed by Francois-Xavier Coudert <coudert@clipper.ens.fr>

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

/* FPU-related code for systems with the GNU libc, providing the
   feenableexcept function in fenv.h to set individual exceptions
   (there's nothing to do that in C99).  */

#include <assert.h>

#ifdef HAVE_FENV_H
#include <fenv.h>
#endif


void set_fpu_trap_exceptions (int trap, int notrap)
{
#ifdef FE_INVALID
  if (trap & GFC_FPE_INVALID)
    feenableexcept (FE_INVALID);
  if (notrap & GFC_FPE_INVALID)
    fedisableexcept (FE_INVALID);
#endif

/* glibc does never have a FE_DENORMAL.  */
#ifdef FE_DENORMAL
  if (trap & GFC_FPE_DENORMAL)
    feenableexcept (FE_DENORMAL);
  if (notrap & GFC_FPE_DENORMAL)
    fedisableexcept (FE_DENORMAL);
#endif

#ifdef FE_DIVBYZERO
  if (trap & GFC_FPE_ZERO)
    feenableexcept (FE_DIVBYZERO);
  if (notrap & GFC_FPE_ZERO)
    fedisableexcept (FE_DIVBYZERO);
#endif

#ifdef FE_OVERFLOW
  if (trap & GFC_FPE_OVERFLOW)
    feenableexcept (FE_OVERFLOW);
  if (notrap & GFC_FPE_OVERFLOW)
    fedisableexcept (FE_OVERFLOW);
#endif

#ifdef FE_UNDERFLOW
  if (trap & GFC_FPE_UNDERFLOW)
    feenableexcept (FE_UNDERFLOW);
  if (notrap & GFC_FPE_UNDERFLOW)
    fedisableexcept (FE_UNDERFLOW);
#endif

#ifdef FE_INEXACT
  if (trap & GFC_FPE_INEXACT)
    feenableexcept (FE_INEXACT);
  if (notrap & GFC_FPE_INEXACT)
    fedisableexcept (FE_INEXACT);
#endif
}


int
get_fpu_trap_exceptions (void)
{
  int exceptions = fegetexcept ();
  int res = 0;

#ifdef FE_INVALID
  if (exceptions & FE_INVALID) res |= GFC_FPE_INVALID;
#endif

#ifdef FE_DENORMAL
  if (exceptions & FE_DENORMAL) res |= GFC_FPE_DENORMAL;
#endif

#ifdef FE_DIVBYZERO
  if (exceptions & FE_DIVBYZERO) res |= GFC_FPE_ZERO;
#endif

#ifdef FE_OVERFLOW
  if (exceptions & FE_OVERFLOW) res |= GFC_FPE_OVERFLOW;
#endif

#ifdef FE_UNDERFLOW
  if (exceptions & FE_UNDERFLOW) res |= GFC_FPE_UNDERFLOW;
#endif

#ifdef FE_INEXACT
  if (exceptions & FE_INEXACT) res |= GFC_FPE_INEXACT;
#endif

  return res;
}


int
support_fpu_trap (int flag)
{
  return support_fpu_flag (flag);
}


void set_fpu (void)
{
#ifndef FE_INVALID
  if (options.fpe & GFC_FPE_INVALID)
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

/* glibc does never have a FE_DENORMAL.  */
#ifndef FE_DENORMAL
  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");
#endif

#ifndef FE_DIVBYZERO
  if (options.fpe & GFC_FPE_ZERO)
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

#ifndef FE_OVERFLOW
  if (options.fpe & GFC_FPE_OVERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

#ifndef FE_UNDERFLOW
  if (options.fpe & GFC_FPE_UNDERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

#ifndef FE_INEXACT
  if (options.fpe & GFC_FPE_INEXACT)
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
#endif

  set_fpu_trap_exceptions (options.fpe, 0);
}


int
get_fpu_except_flags (void)
{
  int result, set_excepts;

  result = 0;
  set_excepts = fetestexcept (FE_ALL_EXCEPT);

#ifdef FE_INVALID
  if (set_excepts & FE_INVALID)
    result |= GFC_FPE_INVALID;
#endif

#ifdef FE_DIVBYZERO
  if (set_excepts & FE_DIVBYZERO)
    result |= GFC_FPE_ZERO;
#endif

#ifdef FE_OVERFLOW
  if (set_excepts & FE_OVERFLOW)
    result |= GFC_FPE_OVERFLOW;
#endif

#ifdef FE_UNDERFLOW
  if (set_excepts & FE_UNDERFLOW)
    result |= GFC_FPE_UNDERFLOW;
#endif

#ifdef FE_DENORMAL
  if (set_excepts & FE_DENORMAL)
    result |= GFC_FPE_DENORMAL;
#endif

#ifdef FE_INEXACT
  if (set_excepts & FE_INEXACT)
    result |= GFC_FPE_INEXACT;
#endif

  return result;
}


void
set_fpu_except_flags (int set, int clear)
{
  int exc_set = 0, exc_clr = 0;

#ifdef FE_INVALID
  if (set & GFC_FPE_INVALID)
    exc_set |= FE_INVALID;
  else if (clear & GFC_FPE_INVALID)
    exc_clr |= FE_INVALID;
#endif

#ifdef FE_DIVBYZERO
  if (set & GFC_FPE_ZERO)
    exc_set |= FE_DIVBYZERO;
  else if (clear & GFC_FPE_ZERO)
    exc_clr |= FE_DIVBYZERO;
#endif

#ifdef FE_OVERFLOW
  if (set & GFC_FPE_OVERFLOW)
    exc_set |= FE_OVERFLOW;
  else if (clear & GFC_FPE_OVERFLOW)
    exc_clr |= FE_OVERFLOW;
#endif

#ifdef FE_UNDERFLOW
  if (set & GFC_FPE_UNDERFLOW)
    exc_set |= FE_UNDERFLOW;
  else if (clear & GFC_FPE_UNDERFLOW)
    exc_clr |= FE_UNDERFLOW;
#endif

#ifdef FE_DENORMAL
  if (set & GFC_FPE_DENORMAL)
    exc_set |= FE_DENORMAL;
  else if (clear & GFC_FPE_DENORMAL)
    exc_clr |= FE_DENORMAL;
#endif

#ifdef FE_INEXACT
  if (set & GFC_FPE_INEXACT)
    exc_set |= FE_INEXACT;
  else if (clear & GFC_FPE_INEXACT)
    exc_clr |= FE_INEXACT;
#endif

  feclearexcept (exc_clr);
  feraiseexcept (exc_set);
}


int
support_fpu_flag (int flag)
{
  if (flag & GFC_FPE_INVALID)
  {
#ifndef FE_INVALID
    return 0;
#endif
  }
  else if (flag & GFC_FPE_ZERO)
  {
#ifndef FE_DIVBYZERO
    return 0;
#endif
  }
  else if (flag & GFC_FPE_OVERFLOW)
  {
#ifndef FE_OVERFLOW
    return 0;
#endif
  }
  else if (flag & GFC_FPE_UNDERFLOW)
  {
#ifndef FE_UNDERFLOW
    return 0;
#endif
  }
  else if (flag & GFC_FPE_DENORMAL)
  {
#ifndef FE_DENORMAL
    return 0;
#endif
  }
  else if (flag & GFC_FPE_INEXACT)
  {
#ifndef FE_INEXACT
    return 0;
#endif
  }

  return 1;
}


int
get_fpu_rounding_mode (void)
{
  int rnd_mode;

  rnd_mode = fegetround ();

  switch (rnd_mode)
    {
#ifdef FE_TONEAREST
      case FE_TONEAREST:
	return GFC_FPE_TONEAREST;
#endif

#ifdef FE_UPWARD
      case FE_UPWARD:
	return GFC_FPE_UPWARD;
#endif

#ifdef FE_DOWNWARD
      case FE_DOWNWARD:
	return GFC_FPE_DOWNWARD;
#endif

#ifdef FE_TOWARDZERO
      case FE_TOWARDZERO:
	return GFC_FPE_TOWARDZERO;
#endif
      default:
	return GFC_FPE_INVALID;
    }
}


void
set_fpu_rounding_mode (int mode)
{
  int rnd_mode;

  switch (mode)
    {
#ifdef FE_TONEAREST
      case GFC_FPE_TONEAREST:
	rnd_mode = FE_TONEAREST;
	break;
#endif

#ifdef FE_UPWARD
      case GFC_FPE_UPWARD:
	rnd_mode = FE_UPWARD;
	break;
#endif

#ifdef FE_DOWNWARD
      case GFC_FPE_DOWNWARD:
	rnd_mode = FE_DOWNWARD;
	break;
#endif

#ifdef FE_TOWARDZERO
      case GFC_FPE_TOWARDZERO:
	rnd_mode = FE_TOWARDZERO;
	break;
#endif
      default:
	return;
    }

  fesetround (rnd_mode);
}


int
support_fpu_rounding_mode (int mode)
{
  switch (mode)
    {
      case GFC_FPE_TONEAREST:
#ifdef FE_TONEAREST
	return 1;
#else
	return 0;
#endif

#ifdef FE_UPWARD
	return 1;
#else
	return 0;
#endif

#ifdef FE_DOWNWARD
	return 1;
#else
	return 0;
#endif

#ifdef FE_TOWARDZERO
	return 1;
#else
	return 0;
#endif

      default:
	return 0;
    }
}


void
get_fpu_state (void *state)
{
  /* Check we can actually store the FPU state in the allocated size.  */
  assert (sizeof(fenv_t) <= GFC_FPE_STATE_BUFFER_SIZE);

  fegetenv (state);
}


void
set_fpu_state (void *state)
{
  /* Check we can actually store the FPU state in the allocated size.  */
  assert (sizeof(fenv_t) <= GFC_FPE_STATE_BUFFER_SIZE);

  fesetenv (state);
}

