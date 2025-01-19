/* SysV FPU-related code (for systems not otherwise supported).
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

/* FPU-related code for SysV platforms with fpsetmask().  */

/* BSD and Solaris systems have slightly different types and functions
   naming.  We deal with these here, to simplify the code below.  */

#if HAVE_FP_EXCEPT
# define FP_EXCEPT_TYPE fp_except
#elif HAVE_FP_EXCEPT_T
# define FP_EXCEPT_TYPE fp_except_t
#else
  choke me
#endif

#if HAVE_FP_RND
# define FP_RND_TYPE fp_rnd
#elif HAVE_FP_RND_T
# define FP_RND_TYPE fp_rnd_t
#else
  choke me
#endif

#if HAVE_FPSETSTICKY
# define FPSETSTICKY fpsetsticky
#elif HAVE_FPRESETSTICKY
# define FPSETSTICKY fpresetsticky
#else
  choke me
#endif


void
set_fpu_trap_exceptions (int trap, int notrap)
{
  FP_EXCEPT_TYPE cw = fpgetmask();

#ifdef FP_X_INV
  if (trap & GFC_FPE_INVALID)
    cw |= FP_X_INV;
  if (notrap & GFC_FPE_INVALID)
    cw &= ~FP_X_INV;
#endif

#ifdef FP_X_DNML
  if (trap & GFC_FPE_DENORMAL)
    cw |= FP_X_DNML;
  if (notrap & GFC_FPE_DENORMAL)
    cw &= ~FP_X_DNML;
#endif

#ifdef FP_X_DZ
  if (trap & GFC_FPE_ZERO)
    cw |= FP_X_DZ;
  if (notrap & GFC_FPE_ZERO)
    cw &= ~FP_X_DZ;
#endif

#ifdef FP_X_OFL
  if (trap & GFC_FPE_OVERFLOW)
    cw |= FP_X_OFL;
  if (notrap & GFC_FPE_OVERFLOW)
    cw &= ~FP_X_OFL;
#endif

#ifdef FP_X_UFL
  if (trap & GFC_FPE_UNDERFLOW)
    cw |= FP_X_UFL;
  if (notrap & GFC_FPE_UNDERFLOW)
    cw &= ~FP_X_UFL;
#endif

#ifdef FP_X_IMP
  if (trap & GFC_FPE_INEXACT)
    cw |= FP_X_IMP;
  if (notrap & GFC_FPE_INEXACT)
    cw &= ~FP_X_IMP;
#endif

  fpsetmask(cw);
}


int
get_fpu_trap_exceptions (void)
{
  int res = 0;
  FP_EXCEPT_TYPE cw = fpgetmask();

#ifdef FP_X_INV
  if (cw & FP_X_INV) res |= GFC_FPE_INVALID;
#endif

#ifdef FP_X_DNML
  if (cw & FP_X_DNML) res |= GFC_FPE_DENORMAL;
#endif

#ifdef FP_X_DZ
  if (cw & FP_X_DZ) res |= GFC_FPE_ZERO;
#endif

#ifdef FP_X_OFL
  if (cw & FP_X_OFL) res |= GFC_FPE_OVERFLOW;
#endif

#ifdef FP_X_UFL
  if (cw & FP_X_UFL) res |= GFC_FPE_UNDERFLOW;
#endif

#ifdef FP_X_IMP
  if (cw & FP_X_IMP) res |= GFC_FPE_INEXACT;
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
#ifndef FP_X_INV
  if (options.fpe & GFC_FPE_INVALID)
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

#ifndef FP_X_DNML
  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");
#endif

#ifndef FP_X_DZ
  if (options.fpe & GFC_FPE_ZERO)
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

#ifndef FP_X_OFL
  if (options.fpe & GFC_FPE_OVERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

#ifndef FP_X_UFL
  if (options.fpe & GFC_FPE_UNDERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

#ifndef FP_X_IMP
  if (options.fpe & GFC_FPE_INEXACT)
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
#endif

  set_fpu_trap_exceptions (options.fpe, 0);
}


int
get_fpu_except_flags (void)
{
  int result;
  FP_EXCEPT_TYPE set_excepts;

  result = 0;
  set_excepts = fpgetsticky ();

#ifdef FP_X_INV
  if (set_excepts & FP_X_INV)
    result |= GFC_FPE_INVALID;
#endif

#ifdef FP_X_DZ
  if (set_excepts & FP_X_DZ)
    result |= GFC_FPE_ZERO;
#endif

#ifdef FP_X_OFL
  if (set_excepts & FP_X_OFL)
    result |= GFC_FPE_OVERFLOW;
#endif

#ifdef FP_X_UFL
  if (set_excepts & FP_X_UFL)
    result |= GFC_FPE_UNDERFLOW;
#endif

#ifdef FP_X_DNML
  if (set_excepts & FP_X_DNML)
    result |= GFC_FPE_DENORMAL;
#endif

#ifdef FP_X_IMP
  if (set_excepts & FP_X_IMP)
    result |= GFC_FPE_INEXACT;
#endif

  return result;
}


void
set_fpu_except_flags (int set, int clear)
{
  FP_EXCEPT_TYPE flags;

  flags = fpgetsticky ();

#ifdef FP_X_INV
  if (set & GFC_FPE_INVALID)
    flags |= FP_X_INV;
  if (clear & GFC_FPE_INVALID)
    flags &= ~FP_X_INV;
#endif

#ifdef FP_X_DZ
  if (set & GFC_FPE_ZERO)
    flags |= FP_X_DZ;
  if (clear & GFC_FPE_ZERO)
    flags &= ~FP_X_DZ;
#endif

#ifdef FP_X_OFL
  if (set & GFC_FPE_OVERFLOW)
    flags |= FP_X_OFL;
  if (clear & GFC_FPE_OVERFLOW)
    flags &= ~FP_X_OFL;
#endif

#ifdef FP_X_UFL
  if (set & GFC_FPE_UNDERFLOW)
    flags |= FP_X_UFL;
  if (clear & GFC_FPE_UNDERFLOW)
    flags &= ~FP_X_UFL;
#endif

#ifdef FP_X_DNML
  if (set & GFC_FPE_DENORMAL)
    flags |= FP_X_DNML;
  if (clear & GFC_FPE_DENORMAL)
    flags &= ~FP_X_DNML;
#endif

#ifdef FP_X_IMP
  if (set & GFC_FPE_INEXACT)
    flags |= FP_X_IMP;
  if (clear & GFC_FPE_INEXACT)
    flags &= ~FP_X_IMP;
#endif

  FPSETSTICKY (flags);
}


int
support_fpu_flag (int flag)
{
  if (flag & GFC_FPE_INVALID)
  {
#ifndef FP_X_INV
    return 0;
#endif
  }
  else if (flag & GFC_FPE_ZERO)
  {
#ifndef FP_X_DZ
    return 0;
#endif
  }
  else if (flag & GFC_FPE_OVERFLOW)
  {
#ifndef FP_X_OFL
    return 0;
#endif
  }
  else if (flag & GFC_FPE_UNDERFLOW)
  {
#ifndef FP_X_UFL
    return 0;
#endif
  }
  else if (flag & GFC_FPE_DENORMAL)
  {
#ifndef FP_X_DNML
    return 0;
#endif
  }
  else if (flag & GFC_FPE_INEXACT)
  {
#ifndef FP_X_IMP
    return 0;
#endif
  }

  return 1;
}


int
get_fpu_rounding_mode (void)
{
  switch (fpgetround ())
    {
      case FP_RN:
	return GFC_FPE_TONEAREST;
      case FP_RP:
	return GFC_FPE_UPWARD;
      case FP_RM:
	return GFC_FPE_DOWNWARD;
      case FP_RZ:
	return GFC_FPE_TOWARDZERO;
      default:
	return 0; /* Should be unreachable.  */
    }
}


void
set_fpu_rounding_mode (int mode)
{
  FP_RND_TYPE rnd_mode;

  switch (mode)
    {
      case GFC_FPE_TONEAREST:
	rnd_mode = FP_RN;
        break;
      case GFC_FPE_UPWARD:
	rnd_mode = FP_RP;
        break;
      case GFC_FPE_DOWNWARD:
	rnd_mode = FP_RM;
        break;
      case GFC_FPE_TOWARDZERO:
	rnd_mode = FP_RZ;
        break;
      default:
	return; /* Should be unreachable.  */
    }
  fpsetround (rnd_mode);
}


int
support_fpu_rounding_mode (int mode)
{
  if (mode == GFC_FPE_AWAY)
    return 0;
  else
    return 1;
}


typedef struct
{
  FP_EXCEPT_TYPE mask;
  FP_EXCEPT_TYPE sticky;
  FP_RND_TYPE round;
} fpu_state_t;


/* Check we can actually store the FPU state in the allocated size.  */
_Static_assert (sizeof(fpu_state_t) <= (size_t) GFC_FPE_STATE_BUFFER_SIZE,
		"GFC_FPE_STATE_BUFFER_SIZE is too small");


void
get_fpu_state (void *s)
{
  fpu_state_t *state = s;

  state->mask = fpgetmask ();
  state->sticky = fpgetsticky ();
  state->round = fpgetround ();
}

void
set_fpu_state (void *s)
{
  fpu_state_t *state = s;

  fpsetmask (state->mask);
  FPSETSTICKY (state->sticky);
  fpsetround (state->round);
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

