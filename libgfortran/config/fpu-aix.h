/* AIX FPU-related code.
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


/* FPU-related code for AIX.  */
#ifdef HAVE_FPTRAP_H
#include <fptrap.h>
#endif

#ifdef HAVE_FPXCP_H
#include <fpxcp.h>
#endif

void
set_fpu (void)
{
  fptrap_t mode = 0;

  if (options.fpe & GFC_FPE_INVALID)
#ifdef TRP_INVALID
    mode |= TRP_INVALID;
#else
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");

  if (options.fpe & GFC_FPE_ZERO)
#ifdef TRP_DIV_BY_ZERO
    mode |= TRP_DIV_BY_ZERO;
#else
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_OVERFLOW)
#ifdef TRP_OVERFLOW
    mode |= TRP_OVERFLOW;
#else
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_UNDERFLOW)
#ifdef TRP_UNDERFLOW
    mode |= TRP_UNDERFLOW;
#else
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
#endif

  if (options.fpe & GFC_FPE_INEXACT)
#ifdef TRP_INEXACT
    mode |= TRP_INEXACT;
#else
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
#endif

  fp_trap(FP_TRAP_SYNC);
  fp_enable(mode);
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
