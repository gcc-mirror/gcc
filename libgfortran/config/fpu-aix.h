/* AIX FPU-related code.
   Copyright 2005, 2007 Free Software Foundation, Inc.
   Contributed by Francois-Xavier Coudert <coudert@clipper.ens.fr>

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

Libgfortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public
License along with libgfortran; see the file COPYING.  If not,
write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


/* FPU-related code for AIX.  */
#ifdef HAVE_FPTRAP_H
#include <fptrap.h>
#endif

void
set_fpu (void)
{
  fptrap_t mode = 0;

  if (options.fpe & GFC_FPE_INVALID)
#ifdef TRP_INVALID
    mode |= TRP_INVALID;
#else
    st_printf ("Fortran runtime warning: IEEE 'invalid operation' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_DENORMAL)
    st_printf ("Fortran runtime warning: IEEE 'denormal number' "
	       "exception not supported.\n");

  if (options.fpe & GFC_FPE_ZERO)
#ifdef TRP_DIV_BY_ZERO
    mode |= TRP_DIV_BY_ZERO;
#else
    st_printf ("Fortran runtime warning: IEEE 'division by zero' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef TRP_OVERFLOW
    mode |= TRP_OVERFLOW;
#else
    st_printf ("Fortran runtime warning: IEEE 'overflow' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef TRP_UNDERFLOW
    mode |= TRP_UNDERFLOW;
#else
    st_printf ("Fortran runtime warning: IEEE 'underflow' "
	       "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_PRECISION)
#ifdef TRP_UNDERFLOW
    mode |= TRP_UNDERFLOW;
#else
    st_printf ("Fortran runtime warning: IEEE 'loss of precision' "
	       "exception not supported.\n");
#endif

  fp_trap(FP_TRAP_SYNC);
  fp_enable(mode);
}
