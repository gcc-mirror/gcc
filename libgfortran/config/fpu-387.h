/* FPU-related code for x86 and x86_64 processors.
   Copyright (C) 2005-2017 Free Software Foundation, Inc.
   Contributed by Francois-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

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

#ifndef __SSE_MATH__
#include "cpuid.h"
#endif

static int
has_sse (void)
{
#ifndef __SSE_MATH__
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  return edx & bit_SSE;
#else
  return 1;
#endif
}

/* i387 exceptions -- see linux <fpu_control.h> header file for details.  */
#define _FPU_MASK_IM  0x01
#define _FPU_MASK_DM  0x02
#define _FPU_MASK_ZM  0x04
#define _FPU_MASK_OM  0x08
#define _FPU_MASK_UM  0x10
#define _FPU_MASK_PM  0x20
#define _FPU_MASK_ALL 0x3f

#define _FPU_EX_ALL   0x3f

/* i387 rounding modes.  */

#define _FPU_RC_NEAREST 0x0
#define _FPU_RC_DOWN    0x1
#define _FPU_RC_UP      0x2
#define _FPU_RC_ZERO    0x3

#define _FPU_RC_MASK    0x3

/* Enable flush to zero mode.  */

#define MXCSR_FTZ (1 << 15)


/* This structure corresponds to the layout of the block
   written by FSTENV.  */
typedef struct
{
  unsigned short int __control_word;
  unsigned short int __unused1;
  unsigned short int __status_word;
  unsigned short int __unused2;
  unsigned short int __tags;
  unsigned short int __unused3;
  unsigned int __eip;
  unsigned short int __cs_selector;
  unsigned short int __opcode;
  unsigned int __data_offset;
  unsigned short int __data_selector;
  unsigned short int __unused5;
  unsigned int __mxcsr;
}
my_fenv_t;

/* Check we can actually store the FPU state in the allocated size.  */
_Static_assert (sizeof(my_fenv_t) <= (size_t) GFC_FPE_STATE_BUFFER_SIZE,
		"GFC_FPE_STATE_BUFFER_SIZE is too small");


/* Raise the supported floating-point exceptions from EXCEPTS.  Other
   bits in EXCEPTS are ignored.  Code originally borrowed from
   libatomic/config/x86/fenv.c.  */

static void
local_feraiseexcept (int excepts)
{
  if (excepts & _FPU_MASK_IM)
    {
      float f = 0.0f;
#ifdef __SSE_MATH__
      volatile float r __attribute__ ((unused));
      __asm__ __volatile__ ("%vdivss\t{%0, %d0|%d0, %0}" : "+x" (f));
      r = f; /* Needed to trigger exception.   */
#else
      __asm__ __volatile__ ("fdiv\t{%y0, %0|%0, %y0}" : "+t" (f));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
  if (excepts & _FPU_MASK_DM)
    {
      my_fenv_t temp;
      __asm__ __volatile__ ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= _FPU_MASK_DM;
      __asm__ __volatile__ ("fldenv\t%0" : : "m" (temp));
      __asm__ __volatile__ ("fwait");
    }
  if (excepts & _FPU_MASK_ZM)
    {
      float f = 1.0f, g = 0.0f;
#ifdef __SSE_MATH__
      volatile float r __attribute__ ((unused));
      __asm__ __volatile__ ("%vdivss\t{%1, %d0|%d0, %1}" : "+x" (f) : "xm" (g));
      r = f; /* Needed to trigger exception.   */
#else
      __asm__ __volatile__ ("fdivs\t%1" : "+t" (f) : "m" (g));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
  if (excepts & _FPU_MASK_OM)
    {
      my_fenv_t temp;
      __asm__ __volatile__ ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= _FPU_MASK_OM;
      __asm__ __volatile__ ("fldenv\t%0" : : "m" (temp));
      __asm__ __volatile__ ("fwait");
    }
  if (excepts & _FPU_MASK_UM)
    {
      my_fenv_t temp;
      __asm__ __volatile__ ("fnstenv\t%0" : "=m" (temp));
      temp.__status_word |= _FPU_MASK_UM;
      __asm__ __volatile__ ("fldenv\t%0" : : "m" (temp));
      __asm__ __volatile__ ("fwait");
    }
  if (excepts & _FPU_MASK_PM)
    {
      float f = 1.0f, g = 3.0f;
#ifdef __SSE_MATH__
      volatile float r __attribute__ ((unused));
      __asm__ __volatile__ ("%vdivss\t{%1, %d0|%d0, %1}" : "+x" (f) : "xm" (g));
      r = f; /* Needed to trigger exception.   */
#else
      __asm__ __volatile__ ("fdivs\t%1" : "+t" (f) : "m" (g));
      /* No need for fwait, exception is triggered by emitted fstp.  */
#endif
    }
}


void
set_fpu_trap_exceptions (int trap, int notrap)
{
  int exc_set = 0, exc_clr = 0;
  unsigned short cw;

  if (trap & GFC_FPE_INVALID) exc_set |= _FPU_MASK_IM;
  if (trap & GFC_FPE_DENORMAL) exc_set |= _FPU_MASK_DM;
  if (trap & GFC_FPE_ZERO) exc_set |= _FPU_MASK_ZM;
  if (trap & GFC_FPE_OVERFLOW) exc_set |= _FPU_MASK_OM;
  if (trap & GFC_FPE_UNDERFLOW) exc_set |= _FPU_MASK_UM;
  if (trap & GFC_FPE_INEXACT) exc_set |= _FPU_MASK_PM;

  if (notrap & GFC_FPE_INVALID) exc_clr |= _FPU_MASK_IM;
  if (notrap & GFC_FPE_DENORMAL) exc_clr |= _FPU_MASK_DM;
  if (notrap & GFC_FPE_ZERO) exc_clr |= _FPU_MASK_ZM;
  if (notrap & GFC_FPE_OVERFLOW) exc_clr |= _FPU_MASK_OM;
  if (notrap & GFC_FPE_UNDERFLOW) exc_clr |= _FPU_MASK_UM;
  if (notrap & GFC_FPE_INEXACT) exc_clr |= _FPU_MASK_PM;

  __asm__ __volatile__ ("fstcw\t%0" : "=m" (cw));

  cw |= exc_clr;
  cw &= ~exc_set;

  __asm__ __volatile__ ("fnclex\n\tfldcw\t%0" : : "m" (cw));

  if (has_sse())
    {
      unsigned int cw_sse;

      __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));

      /* The SSE exception masks are shifted by 7 bits.  */
      cw_sse |= (exc_clr << 7);
      cw_sse &= ~(exc_set << 7);

      /* Clear stalled exception flags.  */
      cw_sse &= ~_FPU_EX_ALL;

      __asm__ __volatile__ ("%vldmxcsr\t%0" : : "m" (cw_sse));
    }
}

void
set_fpu (void)
{
  set_fpu_trap_exceptions (options.fpe, 0);
}

int
get_fpu_trap_exceptions (void)
{
  unsigned short cw;
  int mask;
  int res = 0;

  __asm__ __volatile__ ("fstcw\t%0" : "=m" (cw));
  mask = cw;

  if (has_sse())
    {
      unsigned int cw_sse;

      __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));

      /* The SSE exception masks are shifted by 7 bits.  */
      mask |= (cw_sse >> 7);
    }

  mask = ~mask & _FPU_MASK_ALL;

  if (mask & _FPU_MASK_IM) res |= GFC_FPE_INVALID;
  if (mask & _FPU_MASK_DM) res |= GFC_FPE_DENORMAL;
  if (mask & _FPU_MASK_ZM) res |= GFC_FPE_ZERO;
  if (mask & _FPU_MASK_OM) res |= GFC_FPE_OVERFLOW;
  if (mask & _FPU_MASK_UM) res |= GFC_FPE_UNDERFLOW;
  if (mask & _FPU_MASK_PM) res |= GFC_FPE_INEXACT;

  return res;
}

int
support_fpu_trap (int flag __attribute__((unused)))
{
  return 1;
}

int
get_fpu_except_flags (void)
{
  unsigned short cw;
  int excepts;
  int res = 0;

  __asm__ __volatile__ ("fnstsw\t%0" : "=am" (cw));
  excepts = cw;

  if (has_sse())
    {
      unsigned int cw_sse;

      __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));
      excepts |= cw_sse;
    }

  excepts &= _FPU_EX_ALL;

  if (excepts & _FPU_MASK_IM) res |= GFC_FPE_INVALID;
  if (excepts & _FPU_MASK_DM) res |= GFC_FPE_DENORMAL;
  if (excepts & _FPU_MASK_ZM) res |= GFC_FPE_ZERO;
  if (excepts & _FPU_MASK_OM) res |= GFC_FPE_OVERFLOW;
  if (excepts & _FPU_MASK_UM) res |= GFC_FPE_UNDERFLOW;
  if (excepts & _FPU_MASK_PM) res |= GFC_FPE_INEXACT;

  return res;
}

void
set_fpu_except_flags (int set, int clear)
{
  my_fenv_t temp;
  int exc_set = 0, exc_clr = 0;

  /* Translate from GFC_PE_* values to _FPU_MASK_* values.  */
  if (set & GFC_FPE_INVALID)
    exc_set |= _FPU_MASK_IM;
  if (clear & GFC_FPE_INVALID)
    exc_clr |= _FPU_MASK_IM;

  if (set & GFC_FPE_DENORMAL)
    exc_set |= _FPU_MASK_DM;
  if (clear & GFC_FPE_DENORMAL)
    exc_clr |= _FPU_MASK_DM;

  if (set & GFC_FPE_ZERO)
    exc_set |= _FPU_MASK_ZM;
  if (clear & GFC_FPE_ZERO)
    exc_clr |= _FPU_MASK_ZM;

  if (set & GFC_FPE_OVERFLOW)
    exc_set |= _FPU_MASK_OM;
  if (clear & GFC_FPE_OVERFLOW)
    exc_clr |= _FPU_MASK_OM;

  if (set & GFC_FPE_UNDERFLOW)
    exc_set |= _FPU_MASK_UM;
  if (clear & GFC_FPE_UNDERFLOW)
    exc_clr |= _FPU_MASK_UM;

  if (set & GFC_FPE_INEXACT)
    exc_set |= _FPU_MASK_PM;
  if (clear & GFC_FPE_INEXACT)
    exc_clr |= _FPU_MASK_PM;


  /* Change the flags. This is tricky on 387 (unlike SSE), because we have
     FNSTSW but no FLDSW instruction.  */
  __asm__ __volatile__ ("fnstenv\t%0" : "=m" (temp));
  temp.__status_word &= ~exc_clr;
  __asm__ __volatile__ ("fldenv\t%0" : : "m" (temp));

  /* Change the flags on SSE.  */

  if (has_sse())
  {
    unsigned int cw_sse;

    __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));
    cw_sse &= ~exc_clr;
    __asm__ __volatile__ ("%vldmxcsr\t%0" : : "m" (cw_sse));
  }

  local_feraiseexcept (exc_set);
}

int
support_fpu_flag (int flag __attribute__((unused)))
{
  return 1;
}

void
set_fpu_rounding_mode (int round)
{
  int round_mode;
  unsigned short cw;

  switch (round)
    {
    case GFC_FPE_TONEAREST:
      round_mode = _FPU_RC_NEAREST;
      break;
    case GFC_FPE_UPWARD:
      round_mode = _FPU_RC_UP;
      break;
    case GFC_FPE_DOWNWARD:
      round_mode = _FPU_RC_DOWN;
      break;
    case GFC_FPE_TOWARDZERO:
      round_mode = _FPU_RC_ZERO;
      break;
    default:
      return; /* Should be unreachable.  */
    }

  __asm__ __volatile__ ("fnstcw\t%0" : "=m" (cw));

  /* The x87 round control bits are shifted by 10 bits.  */
  cw &= ~(_FPU_RC_MASK << 10);
  cw |= round_mode << 10;

  __asm__ __volatile__ ("fldcw\t%0" : : "m" (cw));

  if (has_sse())
    {
      unsigned int cw_sse;

      __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));

      /* The SSE round control bits are shifted by 13 bits.  */
      cw_sse &= ~(_FPU_RC_MASK << 13);
      cw_sse |= round_mode << 13;

      __asm__ __volatile__ ("%vldmxcsr\t%0" : : "m" (cw_sse));
    }
}

int
get_fpu_rounding_mode (void)
{
  int round_mode;

#ifdef __SSE_MATH__
  unsigned int cw;

  __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw));

  /* The SSE round control bits are shifted by 13 bits.  */
  round_mode = cw >> 13;
#else
  unsigned short cw;

  __asm__ __volatile__ ("fnstcw\t%0" : "=m" (cw));

  /* The x87 round control bits are shifted by 10 bits.  */
  round_mode = cw >> 10;
#endif

  round_mode &= _FPU_RC_MASK;

  switch (round_mode)
    {
    case _FPU_RC_NEAREST:
      return GFC_FPE_TONEAREST;
    case _FPU_RC_UP:
      return GFC_FPE_UPWARD;
    case _FPU_RC_DOWN:
      return GFC_FPE_DOWNWARD;
    case _FPU_RC_ZERO:
      return GFC_FPE_TOWARDZERO;
    default:
      return 0; /* Should be unreachable.  */
    }
}

int
support_fpu_rounding_mode (int mode __attribute__((unused)))
{
  return 1;
}

void
get_fpu_state (void *state)
{
  my_fenv_t *envp = state;

  __asm__ __volatile__ ("fnstenv\t%0" : "=m" (*envp));

  /* fnstenv has the side effect of masking all exceptions, so we need
     to restore the control word after that.  */
  __asm__ __volatile__ ("fldcw\t%0" : : "m" (envp->__control_word));

  if (has_sse())
    __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (envp->__mxcsr));
}

void
set_fpu_state (void *state)
{
  my_fenv_t *envp = state;

  /* glibc sources (sysdeps/x86_64/fpu/fesetenv.c) do something more
     complex than this, but I think it suffices in our case.  */
  __asm__ __volatile__ ("fldenv\t%0" : : "m" (*envp));

  if (has_sse())
    __asm__ __volatile__ ("%vldmxcsr\t%0" : : "m" (envp->__mxcsr));
}


int
support_fpu_underflow_control (int kind)
{
  if (!has_sse())
    return 0;

  return (kind == 4 || kind == 8) ? 1 : 0;
}


int
get_fpu_underflow_mode (void)
{
  unsigned int cw_sse;

  if (!has_sse())
    return 1;

  __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));

  /* Return 0 for abrupt underflow (flush to zero), 1 for gradual underflow.  */
  return (cw_sse & MXCSR_FTZ) ? 0 : 1;
}


void
set_fpu_underflow_mode (int gradual)
{
  unsigned int cw_sse;

  if (!has_sse())
    return;

  __asm__ __volatile__ ("%vstmxcsr\t%0" : "=m" (cw_sse));

  if (gradual)
    cw_sse &= ~MXCSR_FTZ;
  else
    cw_sse |= MXCSR_FTZ;

  __asm__ __volatile__ ("%vldmxcsr\t%0" : : "m" (cw_sse));
}

