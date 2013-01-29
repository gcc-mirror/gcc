/* Fallback FPU-related code (for systems not otherwise supported).
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


/* Fallback FPU-related code for systems not otherwise supported. This
   is mainly telling the user that we will not be able to do what he
   requested.  */

void
set_fpu (void)
{
  if (options.fpe & GFC_FPE_INVALID)
    estr_write ("Fortran runtime warning: IEEE 'invalid operation' "
	        "exception not supported.\n");
  if (options.fpe & GFC_FPE_DENORMAL)
    estr_write ("Fortran runtime warning: Floating point 'denormal operand' "
	        "exception not supported.\n");
  if (options.fpe & GFC_FPE_ZERO)
    estr_write ("Fortran runtime warning: IEEE 'division by zero' "
	        "exception not supported.\n");
  if (options.fpe & GFC_FPE_OVERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'overflow' "
	        "exception not supported.\n");
  if (options.fpe & GFC_FPE_UNDERFLOW)
    estr_write ("Fortran runtime warning: IEEE 'underflow' "
	        "exception not supported.\n");
  if (options.fpe & GFC_FPE_INEXACT)
    estr_write ("Fortran runtime warning: IEEE 'inexact' "
	        "exception not supported.\n");
}
