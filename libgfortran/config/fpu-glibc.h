/* FPU-related code for systems with GNU libc.
   Copyright 2005, 2007, 2009 Free Software Foundation, Inc.
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
    st_printf ("Fortran runtime warning: IEEE 'invalid operation' "
	       "exception not supported.\n");
#endif

/* glibc does never have a FE_DENORMAL.  */
  if (options.fpe & GFC_FPE_DENORMAL)
#ifdef FE_DENORMAL
    feenableexcept (FE_DENORMAL);
#else
    st_printf ("Fortran runtime warning: IEEE 'denormal number' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_ZERO)
#ifdef FE_DIVBYZERO
    feenableexcept (FE_DIVBYZERO);
#else
    st_printf ("Fortran runtime warning: IEEE 'division by zero' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef FE_OVERFLOW
    feenableexcept (FE_OVERFLOW);
#else
    st_printf ("Fortran runtime warning: IEEE 'overflow' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef FE_UNDERFLOW
    feenableexcept (FE_UNDERFLOW);
#else
    st_printf ("Fortran runtime warning: IEEE 'underflow' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_PRECISION)
#ifdef FE_INEXACT
    feenableexcept (FE_INEXACT);
#else
    st_printf ("Fortran runtime warning: IEEE 'loss of precision' "
	       "exception not supported.\n");
#endif
}
