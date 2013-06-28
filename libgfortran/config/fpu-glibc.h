/* FPU-related code for systems with GNU libc.
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

/* FPU-related code for systems with the GNU libc, providing the
   feenableexcept function in fenv.h to set individual exceptions
   (there's nothing to do that in C99).  */

#ifdef HAVE_FENV_H
#include <fenv.h>
#endif

void set_fpu (void)
{
  if (FE_ALL_EXCEPT != 0)
    fedisableexcept (FE_ALL_EXCEPT);

  if (options.fpe & GFC_FPE_INVALID)
#ifdef FE_INVALID
    feenableexcept (FE_INVALID);
#else
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

/* glibc does never have a FE_DENORMAL.  */
  if (options.fpe & GFC_FPE_DENORMAL)
#ifdef FE_DENORMAL
    feenableexcept (FE_DENORMAL);
#else
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_ZERO)
#ifdef FE_DIVBYZERO
    feenableexcept (FE_DIVBYZERO);
#else
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef FE_OVERFLOW
    feenableexcept (FE_OVERFLOW);
#else
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef FE_UNDERFLOW
    feenableexcept (FE_UNDERFLOW);
#else
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_INEXACT)
#ifdef FE_INEXACT
    feenableexcept (FE_INEXACT);
#else
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
#endif
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
