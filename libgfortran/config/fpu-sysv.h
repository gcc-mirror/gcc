/* SysV FPU-related code (for systems not otherwise supported).
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

/* FPU-related code for SysV platforms with fpsetmask().  */

void
set_fpu (void)
{
  int cw = 0;

  if (options.fpe & GFC_FPE_INVALID)
#ifdef FP_X_INV
    cw |= FP_X_INV;
#else
    st_printf ("Fortran runtime warning: IEEE 'invalid operation' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_DENORMAL)
#ifdef FP_X_DNML
    cw |= FP_X_DNML;
#else
    st_printf ("Fortran runtime warning: IEEE 'denormal number' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_ZERO)
#ifdef FP_X_DZ
    cw |= FP_X_DZ;
#else
    st_printf ("Fortran runtime warning: IEEE 'division by zero' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef FP_X_OFL
    cw |= FP_X_OFL;
#else
    st_printf ("Fortran runtime warning: IEEE 'overflow' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef FP_X_UFL
    cw |= FP_X_UFL;
#else
    st_printf ("Fortran runtime warning: IEEE 'underflow' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_PRECISION)
#ifdef FP_X_IMP
    cw |= FP_X_IMP;
#else
    st_printf ("Fortran runtime warning: IEEE 'loss of precision' "
	       "exception not supported.\n");
#endif

  fpsetmask(cw);
}
