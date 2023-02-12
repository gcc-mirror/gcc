/* AIX FPU-related code.
   Copyright (C) 2005-2023 Free Software Foundation, Inc.
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


/* FPU-related code for AIX.  */
#ifdef HAVE_FPTRAP_H
#include <fptrap.h>
#endif

#ifdef HAVE_FPXCP_H
#include <fpxcp.h>
#endif

#ifdef HAVE_FENV_H
#include <fenv.h>
#endif


/* Check we can actually store the FPU state in the allocated size.  */
_Static_assert (sizeof(fenv_t) <= (size_t) GFC_FPE_STATE_BUFFER_SIZE,
		"GFC_FPE_STATE_BUFFER_SIZE is too small");


void
set_fpu_trap_exceptions (int trap, int notrap)
{
  fptrap_t mode_set = 0, mode_clr = 0;

#ifdef TRP_INVALID
  if (trap & GFC_FPE_INVALID)
    mode_set |= TRP_INVALID;
  if (notrap & GFC_FPE_INVALID)
    mode_clr |= TRP_INVALID;
#endif

#ifdef TRP_DIV_BY_ZERO
  if (trap & GFC_FPE_ZERO)
    mode_set |= TRP_DIV_BY_ZERO;
  if (notrap & GFC_FPE_ZERO)
    mode_clr |= TRP_DIV_BY_ZERO;
#endif

#ifdef TRP_OVERFLOW
  if (trap & GFC_FPE_OVERFLOW)
    mode_set |= TRP_OVERFLOW;
  if (notrap & GFC_FPE_OVERFLOW)
    mode_clr |= TRP_OVERFLOW;
#endif

#ifdef TRP_UNDERFLOW
  if (trap & GFC_FPE_UNDERFLOW)
    mode_set |= TRP_UNDERFLOW;
  if (notrap & GFC_FPE_UNDERFLOW)
    mode_clr |= TRP_UNDERFLOW;
#endif

#ifdef TRP_INEXACT
  if (trap & GFC_FPE_INEXACT)
    mode_set |= TRP_INEXACT;
  if (notrap & GFC_FPE_INEXACT)
    mode_clr |= TRP_INEXACT;
#endif

  fp_trap (FP_TRAP_SYNC);
  fp_enable (mode_set);
  fp_disable (mode_clr);
}


int
get_fpu_trap_exceptions (void)
{
  int res = 0;

#ifdef TRP_INVALID
  if (fp_is_enabled (TRP_INVALID))
    res |= GFC_FPE_INVALID;
#endif

#ifdef TRP_DIV_BY_ZERO
  if (fp_is_enabled (TRP_DIV_BY_ZERO))
    res |= GFC_FPE_ZERO;
#endif

#ifdef TRP_OVERFLOW
  if (fp_is_enabled (TRP_OVERFLOW))
    res |= GFC_FPE_OVERFLOW;
#endif

#ifdef TRP_UNDERFLOW
  if (fp_is_enabled (TRP_UNDERFLOW))
    res |= GFC_FPE_UNDERFLOW;
#endif

#ifdef TRP_INEXACT
  if (fp_is_enabled (TRP_INEXACT))
    res |= GFC_FPE_INEXACT;
#endif

  return res;
}


int
support_fpu_trap (int flag)
{
  return support_fpu_flag (flag);
}


void
set_fpu (void)
{
#ifndef TRP_INVALID
  if (options.fpe & GFC_FPE_INVALID)
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");

#ifndef TRP_DIV_BY_ZERO
  if (options.fpe & GFC_FPE_ZERO)
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

#ifndef TRP_OVERFLOW
  if (options.fpe & GFC_FPE_OVERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

#ifndef TRP_UNDERFLOW
  if (options.fpe & GFC_FPE_UNDERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

#ifndef TRP_INEXACT
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

#ifdef HAVE_FPXCP_H
  if (!fp_any_xcp ())
    return 0;

  if (fp_invalid_op ())
    result |= GFC_FPE_INVALID;

  if (fp_divbyzero ())
    result |= GFC_FPE_ZERO;

  if (fp_overflow ())
    result |= GFC_FPE_OVERFLOW;

  if (fp_underflow ())
    result |= GFC_FPE_UNDERFLOW;

  if (fp_inexact ())
    result |= GFC_FPE_INEXACT;
#endif

  return result;
}


void
set_fpu_except_flags (int set, int clear)
{
  int exc_set = 0, exc_clr = 0;

#ifdef FP_INVALID
  if (set & GFC_FPE_INVALID)
    exc_set |= FP_INVALID;
  else if (clear & GFC_FPE_INVALID)
    exc_clr |= FP_INVALID;
#endif

#ifdef FP_DIV_BY_ZERO
  if (set & GFC_FPE_ZERO)
    exc_set |= FP_DIV_BY_ZERO;
  else if (clear & GFC_FPE_ZERO)
    exc_clr |= FP_DIV_BY_ZERO;
#endif

#ifdef FP_OVERFLOW
  if (set & GFC_FPE_OVERFLOW)
    exc_set |= FP_OVERFLOW;
  else if (clear & GFC_FPE_OVERFLOW)
    exc_clr |= FP_OVERFLOW;
#endif

#ifdef FP_UNDERFLOW
  if (set & GFC_FPE_UNDERFLOW)
    exc_set |= FP_UNDERFLOW;
  else if (clear & GFC_FPE_UNDERFLOW)
    exc_clr |= FP_UNDERFLOW;
#endif

/* AIX does not have FP_DENORMAL.  */

#ifdef FP_INEXACT
  if (set & GFC_FPE_INEXACT)
    exc_set |= FP_INEXACT;
  else if (clear & GFC_FPE_INEXACT)
    exc_clr |= FP_INEXACT;
#endif

  fp_clr_flag (exc_clr);
  fp_set_flag (exc_set);
}


int
support_fpu_flag (int flag)
{
  if (flag & GFC_FPE_INVALID)
  {
#ifndef FP_INVALID
    return 0;
#endif
  }
  else if (flag & GFC_FPE_ZERO)
  {
#ifndef FP_DIV_BY_ZERO
    return 0;
#endif
  }
  else if (flag & GFC_FPE_OVERFLOW)
  {
#ifndef FP_OVERFLOW
    return 0;
#endif
  }
  else if (flag & GFC_FPE_UNDERFLOW)
  {
#ifndef FP_UNDERFLOW
    return 0;
#endif
  }
  else if (flag & GFC_FPE_DENORMAL)
  {
    /* AIX does not support denormal flag.  */
    return 0;
  }
  else if (flag & GFC_FPE_INEXACT)
  {
#ifndef FP_INEXACT
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

#ifdef FE_TONEARESTFROMZERO
      case FE_TONEARESTFROMZERO:
	return GFC_FPE_AWAY;
#endif

      default:
	return 0; /* Should be unreachable.  */
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

#ifdef FE_TONEARESTFROMZERO
      case GFC_FPE_AWAY:
	rnd_mode = FE_TONEARESTFROMZERO;
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

      case GFC_FPE_UPWARD:
#ifdef FE_UPWARD
	return 1;
#else
	return 0;
#endif

      case GFC_FPE_DOWNWARD:
#ifdef FE_DOWNWARD
	return 1;
#else
	return 0;
#endif

      case GFC_FPE_TOWARDZERO:
#ifdef FE_TOWARDZERO
	return 1;
#else
	return 0;
#endif

      case GFC_FPE_AWAY:
#ifdef FE_TONEARESTFROMZERO
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
  fegetenv (state);
}

void
set_fpu_state (void *state)
{
  fesetenv (state);
}


int
support_fpu_underflow_control (int kind __attribute__((unused)))
{
  return 0;
}


int
get_fpu_underflow_mode (void)
{
  return 0;
}


void
set_fpu_underflow_mode (int gradual __attribute__((unused)))
{
}

