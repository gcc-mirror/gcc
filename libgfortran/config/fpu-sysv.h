/* SysV FPU-related code (for systems not otherwise supported).
   Copyright (C) 2005-2013 Free Software Foundation, Inc.
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

void
set_fpu (void)
{
  int cw = 0;

  if (options.fpe & GFC_FPE_INVALID)
#ifdef FP_X_INV
    cw |= FP_X_INV;
#else
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_DENORMAL)
#ifdef FP_X_DNML
    cw |= FP_X_DNML;
#else
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_ZERO)
#ifdef FP_X_DZ
    cw |= FP_X_DZ;
#else
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef FP_X_OFL
    cw |= FP_X_OFL;
#else
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef FP_X_UFL
    cw |= FP_X_UFL;
#else
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_INEXACT)
#ifdef FP_X_IMP
    cw |= FP_X_IMP;
#else
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
#endif

  fpsetmask(cw);
}

int
get_fpu_except_flags (void)
{
  int result;
#if HAVE_FP_EXCEPT
  fp_except set_excepts;
#elif HAVE_FP_EXCEPT_T
  fp_except_t set_excepts;
#else
  choke me
#endif

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
