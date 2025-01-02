/* FPU-related code for aarch64.
   Copyright (C) 2020-2025 Free Software Foundation, Inc.
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


/* Rounding mask and modes */

#define FPCR_RM_MASK  0x0c00000
#define FE_TONEAREST  0x0000000
#define FE_UPWARD     0x0400000
#define FE_DOWNWARD   0x0800000
#define FE_TOWARDZERO 0x0c00000
#define FE_MAP_FZ     0x1000000

/* Exceptions */

#define FE_INVALID	1
#define FE_DIVBYZERO	2
#define FE_OVERFLOW	4
#define FE_UNDERFLOW	8
#define FE_INEXACT	16

#define FE_ALL_EXCEPT (FE_INVALID | FE_DIVBYZERO | FE_OVERFLOW | FE_UNDERFLOW | FE_INEXACT)
#define FE_EXCEPT_SHIFT	8



/* This structure corresponds to the layout of the block
   written by FSTENV.  */
struct fenv
{
  unsigned int __fpcr;
  unsigned int __fpsr;
};

/* Check we can actually store the FPU state in the allocated size.  */
_Static_assert (sizeof(struct fenv) <= (size_t) GFC_FPE_STATE_BUFFER_SIZE,
		"GFC_FPE_STATE_BUFFER_SIZE is too small");



void
set_fpu (void)
{
  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");

  set_fpu_trap_exceptions (options.fpe, 0);
}


int
get_fpu_trap_exceptions (void)
{
  unsigned int fpcr, exceptions;
  int res = 0;

  fpcr = __builtin_aarch64_get_fpcr();
  exceptions = (fpcr >> FE_EXCEPT_SHIFT) & FE_ALL_EXCEPT;

  if (exceptions & FE_INVALID) res |= GFC_FPE_INVALID;
  if (exceptions & FE_DIVBYZERO) res |= GFC_FPE_ZERO;
  if (exceptions & FE_OVERFLOW) res |= GFC_FPE_OVERFLOW;
  if (exceptions & FE_UNDERFLOW) res |= GFC_FPE_UNDERFLOW;
  if (exceptions & FE_INEXACT) res |= GFC_FPE_INEXACT;

  return res;
}


void set_fpu_trap_exceptions (int trap, int notrap)
{
  unsigned int mode_set = 0, mode_clr = 0;
  unsigned int fpsr, fpsr_new;
  unsigned int fpcr, fpcr_new;

  if (trap & GFC_FPE_INVALID)
    mode_set |= FE_INVALID;
  if (notrap & GFC_FPE_INVALID)
    mode_clr |= FE_INVALID;

  if (trap & GFC_FPE_ZERO)
    mode_set |= FE_DIVBYZERO;
  if (notrap & GFC_FPE_ZERO)
    mode_clr |= FE_DIVBYZERO;

  if (trap & GFC_FPE_OVERFLOW)
    mode_set |= FE_OVERFLOW;
  if (notrap & GFC_FPE_OVERFLOW)
    mode_clr |= FE_OVERFLOW;

  if (trap & GFC_FPE_UNDERFLOW)
    mode_set |= FE_UNDERFLOW;
  if (notrap & GFC_FPE_UNDERFLOW)
    mode_clr |= FE_UNDERFLOW;

  if (trap & GFC_FPE_INEXACT)
    mode_set |= FE_INEXACT;
  if (notrap & GFC_FPE_INEXACT)
    mode_clr |= FE_INEXACT;

  /* Clear stalled exception flags.  */
  fpsr = __builtin_aarch64_get_fpsr();
  fpsr_new = fpsr & ~FE_ALL_EXCEPT;
  if (fpsr_new != fpsr)
    __builtin_aarch64_set_fpsr(fpsr_new);

  fpcr_new = fpcr = __builtin_aarch64_get_fpcr();
  fpcr_new |= (mode_set << FE_EXCEPT_SHIFT);
  fpcr_new &= ~(mode_clr << FE_EXCEPT_SHIFT);

  if (fpcr_new != fpcr)
    __builtin_aarch64_set_fpcr(fpcr_new);
}


int
support_fpu_flag (int flag)
{
  if (flag & GFC_FPE_DENORMAL)
    return 0;

  return 1;
}


int
support_fpu_trap (int flag)
{
  if (flag & GFC_FPE_DENORMAL)
    return 0;

  return 1;
}


int
get_fpu_except_flags (void)
{
  int result;
  unsigned int fpsr;

  result = 0;
  fpsr = __builtin_aarch64_get_fpsr() & FE_ALL_EXCEPT;

  if (fpsr & FE_INVALID)
    result |= GFC_FPE_INVALID;
  if (fpsr & FE_DIVBYZERO)
    result |= GFC_FPE_ZERO;
  if (fpsr & FE_OVERFLOW)
    result |= GFC_FPE_OVERFLOW;
  if (fpsr & FE_UNDERFLOW)
    result |= GFC_FPE_UNDERFLOW;
  if (fpsr & FE_INEXACT)
    result |= GFC_FPE_INEXACT;

  return result;
}


void
set_fpu_except_flags (int set, int clear)
{
  unsigned int exc_set = 0, exc_clr = 0;
  unsigned int fpsr, fpsr_new;

  if (set & GFC_FPE_INVALID)
    exc_set |= FE_INVALID;
  else if (clear & GFC_FPE_INVALID)
    exc_clr |= FE_INVALID;

  if (set & GFC_FPE_ZERO)
    exc_set |= FE_DIVBYZERO;
  else if (clear & GFC_FPE_ZERO)
    exc_clr |= FE_DIVBYZERO;

  if (set & GFC_FPE_OVERFLOW)
    exc_set |= FE_OVERFLOW;
  else if (clear & GFC_FPE_OVERFLOW)
    exc_clr |= FE_OVERFLOW;

  if (set & GFC_FPE_UNDERFLOW)
    exc_set |= FE_UNDERFLOW;
  else if (clear & GFC_FPE_UNDERFLOW)
    exc_clr |= FE_UNDERFLOW;

  if (set & GFC_FPE_INEXACT)
    exc_set |= FE_INEXACT;
  else if (clear & GFC_FPE_INEXACT)
    exc_clr |= FE_INEXACT;

  fpsr_new = fpsr = __builtin_aarch64_get_fpsr();
  fpsr_new &= ~exc_clr;
  fpsr_new |= exc_set;

  if (fpsr_new != fpsr)
    __builtin_aarch64_set_fpsr(fpsr_new);
}


void
get_fpu_state (void *state)
{
  struct fenv *envp = state;
  envp->__fpcr = __builtin_aarch64_get_fpcr();
  envp->__fpsr = __builtin_aarch64_get_fpsr();
}


void
set_fpu_state (void *state)
{
  struct fenv *envp = state;
  __builtin_aarch64_set_fpcr(envp->__fpcr);
  __builtin_aarch64_set_fpsr(envp->__fpsr);
}


int
get_fpu_rounding_mode (void)
{
  unsigned int fpcr = __builtin_aarch64_get_fpcr();
  fpcr &= FPCR_RM_MASK;

  switch (fpcr)
    {
      case FE_TONEAREST:
        return GFC_FPE_TONEAREST;
      case FE_UPWARD:
        return GFC_FPE_UPWARD;
      case FE_DOWNWARD:
        return GFC_FPE_DOWNWARD;
      case FE_TOWARDZERO:
        return GFC_FPE_TOWARDZERO;
      default:
        return 0; /* Should be unreachable.  */
    }
}


void
set_fpu_rounding_mode (int round)
{
  unsigned int fpcr, round_mode;

  switch (round)
    {
    case GFC_FPE_TONEAREST:
      round_mode = FE_TONEAREST;
      break;
    case GFC_FPE_UPWARD:
      round_mode = FE_UPWARD;
      break;
    case GFC_FPE_DOWNWARD:
      round_mode = FE_DOWNWARD;
      break;
    case GFC_FPE_TOWARDZERO:
      round_mode = FE_TOWARDZERO;
      break;
    default:
      return; /* Should be unreachable.  */
    }

  fpcr = __builtin_aarch64_get_fpcr();

  /* Only set FPCR if requested mode is different from current.  */
  round_mode = (fpcr ^ round_mode) & FPCR_RM_MASK;
  if (round_mode != 0)
    __builtin_aarch64_set_fpcr(fpcr ^ round_mode);
}


int
support_fpu_rounding_mode (int mode)
{
  if (mode == GFC_FPE_AWAY)
    return 0;
  else
    return 1;
}


int
support_fpu_underflow_control (int kind __attribute__((unused)))
{
  /* Not supported for binary128.  */
  return (kind == 4 || kind == 8) ? 1 : 0;
}


int
get_fpu_underflow_mode (void)
{
  unsigned int fpcr = __builtin_aarch64_get_fpcr();

  /* Return 0 for abrupt underflow (flush to zero), 1 for gradual underflow.  */
  return (fpcr & FE_MAP_FZ) ? 0 : 1;
}


void
set_fpu_underflow_mode (int gradual __attribute__((unused)))
{
  unsigned int fpcr = __builtin_aarch64_get_fpcr();

  if (gradual)
    fpcr &= ~FE_MAP_FZ;
  else
    fpcr |= FE_MAP_FZ;

  __builtin_aarch64_set_fpcr(fpcr);
}
