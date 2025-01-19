/* FPU-related code for PowerPC.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
   Contributed by Sergey Fedorov <vital.had@gmail.com>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 3 of the License, or (at your option) any later version.

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
the GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version 3.1,
as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively. If not, see
<http://www.gnu.org/licenses/>. */

/* While some of the definitions and functions used here are available
   with Apple libm, we on purpose avoid pulling it in: to avoid potential
   conflicts and not self-impose unnecessary constraints. */

/* FP exception flags */
#define FE_INEXACT      0x02000000
#define FE_DIVBYZERO    0x04000000
#define FE_UNDERFLOW    0x08000000
#define FE_OVERFLOW     0x10000000
#define FE_INVALID      0x20000000

#define FE_ALL_EXCEPT   0x3E000000
#define FE_NO_EXCEPT    0xC1FFFFFF

/* Extra invalid flags */
#define FE_INVALID_SNAN 0x01000000
#define FE_INVALID_ISI  0x00800000
#define FE_INVALID_IDI  0x00400000
#define FE_INVALID_ZDZ  0x00200000
#define FE_INVALID_IMZ  0x00100000
#define FE_INVALID_XVC  0x00080000
#define FE_INVALID_SOFT 0x00000400
#define FE_INVALID_SQRT 0x00000200
#define FE_INVALID_CVI  0x00000100

/* Rounding modes */
#define FE_TONEAREST    0x00000000
#define FE_TOWARDZERO   0x00000001
#define FE_UPWARD       0x00000002
#define FE_DOWNWARD     0x00000003

/* There is no consistency re what is to be included in all_invalid.
   Apple libm has 0x01f80300, Openlibm/FreeBSD/NetBSD has 0x21f80700
   and OpenBSD has 0x01f80700. In particular, FE_INVALID_SOFT and
   FE_INVALID are contested. */
#define FE_ALL_INVALID  0x01F80300
#define FE_NO_INVALID   0xFE07FCFF

#define FE_ALL_FLAGS    0xFFF80300
#define FE_NO_FLAGS     0x0007FCFF

#define FE_ALL_RND      0x00000003
#define FE_NO_RND       0xFFFFFFFC

/* Floating-point exception summary (FX) bit. */
#define FE_SET_FX       0x80000000
#define FE_CLR_FX       0x7FFFFFFF

/* Some implementations use FE_INVALID_SOFT here. */
#define SET_INVALID     0x01000000

#define FE_EXCEPT_SHIFT 22
#define EXCEPT_MASK     FE_ALL_EXCEPT >> FE_EXCEPT_SHIFT

typedef unsigned int    fenv_t;
typedef unsigned int    fexcept_t;

/* default environment object */
extern const fenv_t     _FE_DFL_ENV;
/* pointer to default environment */
#define FE_DFL_ENV      &_FE_DFL_ENV

typedef union {
    struct {
        unsigned int hi;
        fenv_t       lo;
    } i;
    double           d;
} hexdouble;

#define HEXDOUBLE(hi, lo) {{ hi, lo }}


/* Check we can actually store the FPU state in the allocated size. */
_Static_assert (sizeof(fenv_t) <= (size_t) GFC_FPE_STATE_BUFFER_SIZE,
   "GFC_FPE_STATE_BUFFER_SIZE is too small");


static inline int fegetexcept(void)
{
    hexdouble fe;

    fe.d = __builtin_mffs();
    return ((fe.i.lo & EXCEPT_MASK) << FE_EXCEPT_SHIFT);
}

static inline int feclearexcept(int excepts)
{
    hexdouble fe;

    if (excepts & FE_INVALID)
        excepts |= FE_ALL_INVALID;
    fe.d = __builtin_mffs();
    fe.i.lo &= ~excepts;
    if ((fe.i.lo & FE_ALL_EXCEPT) == 0)
        fe.i.lo &= FE_CLR_FX;
    __builtin_mtfsf(0xFF, fe.d);
    return 0;
}

/* It is not possible to set VX bit directly. */
static inline int feraiseexcept(int excepts)
{
    hexdouble fe;

    if (excepts & FE_INVALID)
        excepts |= SET_INVALID;
    fe.d = __builtin_mffs();
    fe.i.lo |= excepts;
    __builtin_mtfsf(0xFF, fe.d);
    return 0;
}

static inline int fetestexcept(int excepts)
{
    hexdouble fe;

    excepts &= FE_ALL_EXCEPT;
    fe.d = __builtin_mffs();
    return (fe.i.lo & (excepts & FE_ALL_EXCEPT));
}

static inline int feenableexcept(int mask)
{
    hexdouble fe;
    fenv_t oldmask;

    fe.d = __builtin_mffs();
    oldmask = fe.i.lo;
    fe.i.lo |= (mask & FE_ALL_EXCEPT) >> FE_EXCEPT_SHIFT;
    __builtin_mtfsf(0xFF, fe.d);
    return ((oldmask & EXCEPT_MASK) << FE_EXCEPT_SHIFT);
}

static inline int fedisableexcept(int mask)
{
    hexdouble fe;
    fenv_t oldmask;

    fe.d = __builtin_mffs();
    oldmask = fe.i.lo;
    fe.i.lo &= ~((mask & FE_ALL_EXCEPT) >> FE_EXCEPT_SHIFT);
    __builtin_mtfsf(0xFF, fe.d);
    return ((oldmask & EXCEPT_MASK) << FE_EXCEPT_SHIFT);
}

static inline int fegetround(void)
{
    hexdouble fe;

    fe.d = __builtin_mffs();
    return (fe.i.lo & FE_ALL_RND);
}

static inline int fesetround(int round)
{
    hexdouble fe;

    if (round & FE_NO_RND)
        return (-1);
    fe.d = __builtin_mffs();
    fe.i.lo = (fe.i.lo & FE_NO_RND) | round;
    __builtin_mtfsf(0xFF, fe.d);
    return 0;
}

static inline int fegetenv(fenv_t *envp)
{
    hexdouble fe;

    fe.d = __builtin_mffs();
    *envp = fe.i.lo;
    return 0;
}

static inline int fesetenv(const fenv_t *envp)
{
    hexdouble fe;

    fe.i.lo = *envp;
    __builtin_mtfsf(0xFF, fe.d);
    return 0;
}


int get_fpu_trap_exceptions (void)
{
  int exceptions = fegetexcept();
  int res = 0;

  if (exceptions & FE_INVALID) res |= GFC_FPE_INVALID;
  if (exceptions & FE_DIVBYZERO) res |= GFC_FPE_ZERO;
  if (exceptions & FE_OVERFLOW) res |= GFC_FPE_OVERFLOW;
  if (exceptions & FE_UNDERFLOW) res |= GFC_FPE_UNDERFLOW;
  if (exceptions & FE_INEXACT) res |= GFC_FPE_INEXACT;

  return res;
}

void set_fpu (void)
{
  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
            "exception not supported.\n");

  set_fpu_trap_exceptions (options.fpe, 0);
}

void set_fpu_trap_exceptions (int trap, int notrap)
{
  unsigned int mode_set = 0, mode_clr = 0;

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

  /* Clear stalled exception flags. */
  feclearexcept (FE_ALL_EXCEPT);

  feenableexcept (mode_set);
  fedisableexcept (mode_clr);
}

int get_fpu_except_flags (void)
{
  int result, set_excepts;
  result = 0;
  set_excepts = fetestexcept (FE_ALL_EXCEPT);

  if (set_excepts & FE_INVALID)
    result |= GFC_FPE_INVALID;
  if (set_excepts & FE_DIVBYZERO)
    result |= GFC_FPE_ZERO;
  if (set_excepts & FE_OVERFLOW)
    result |= GFC_FPE_OVERFLOW;
  if (set_excepts & FE_UNDERFLOW)
    result |= GFC_FPE_UNDERFLOW;
  if (set_excepts & FE_INEXACT)
    result |= GFC_FPE_INEXACT;

  return result;
}

void set_fpu_except_flags (int set, int clear)
{
  unsigned int exc_set = 0, exc_clr = 0;

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

  feclearexcept (exc_clr);
  feraiseexcept (exc_set);
}

void get_fpu_state (void *state)
{
  fegetenv (state);
}

void set_fpu_state (void *state)
{
  fesetenv (state);
}

int get_fpu_rounding_mode (void)
{
  int rnd_mode;
  rnd_mode = fegetround();

  switch (rnd_mode)
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
        return 0; /* Should be unreachable. */
    }
}

void set_fpu_rounding_mode (int round)
{
  int rnd_mode;

  switch (round)
    {
    case GFC_FPE_TONEAREST:
      rnd_mode = FE_TONEAREST;
      break;
    case GFC_FPE_UPWARD:
      rnd_mode = FE_UPWARD;
      break;
    case GFC_FPE_DOWNWARD:
      rnd_mode = FE_DOWNWARD;
      break;
    case GFC_FPE_TOWARDZERO:
      rnd_mode = FE_TOWARDZERO;
      break;
    default:
      return; /* Should be unreachable. */
    }

  fesetround (rnd_mode);
}

int support_fpu_flag (int flag)
{
  if (flag & GFC_FPE_DENORMAL)
    return 0;

  return 1;
}

int support_fpu_trap (int flag)
{
  if (flag & GFC_FPE_DENORMAL)
    return 0;

  return 1;
}

int support_fpu_rounding_mode(int mode __attribute__((unused)))
{
  return 1;
}

/* The following are not supported. */

int support_fpu_underflow_control(int kind __attribute__((unused)))
{
  return 0;
}

int get_fpu_underflow_mode(void)
{
  return 0;
}

void set_fpu_underflow_mode(int gradual __attribute__((unused)))
{
}
