/* Implementation of the STOP statement.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>

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

#include "libgfortran.h"
#include <stdlib.h>
#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


/* Fortran 2008 demands: If any exception (14) is signaling on that image, the
   processor shall issue a warning indicating which exceptions are signaling;
   this warning shall be on the unit identified by the named constant
   ERROR_UNIT (13.8.2.8).  In line with other compilers, we do not report
   inexact - and we optionally ignore underflow, cf. thread starting at
   http://mailman.j3-fortran.org/pipermail/j3/2013-June/006452.html.  */

static void
report_exception (void)
{
  int set_excepts;

  if (!compile_options.fpe_summary)
    return;

  set_excepts = get_fpu_except_flags ();
  if ((set_excepts & compile_options.fpe_summary) == 0)
    return;

  estr_write ("Note: The following floating-point exceptions are signalling:");

  if ((compile_options.fpe_summary & GFC_FPE_INVALID)
      && (set_excepts & GFC_FPE_INVALID))
    estr_write (" IEEE_INVALID_FLAG");

  if ((compile_options.fpe_summary & GFC_FPE_ZERO)
      && (set_excepts & GFC_FPE_ZERO))
    estr_write (" IEEE_DIVIDE_BY_ZERO");

  if ((compile_options.fpe_summary & GFC_FPE_OVERFLOW)
      && (set_excepts & GFC_FPE_OVERFLOW))
    estr_write (" IEEE_OVERFLOW_FLAG");

  if ((compile_options.fpe_summary & GFC_FPE_UNDERFLOW)
      && (set_excepts & GFC_FPE_UNDERFLOW))
    estr_write (" IEEE_UNDERFLOW_FLAG");

  if ((compile_options.fpe_summary & GFC_FPE_DENORMAL)
      && (set_excepts & GFC_FPE_DENORMAL))
    estr_write (" IEEE_DENORMAL");

  if ((compile_options.fpe_summary & GFC_FPE_INEXACT)
      && (set_excepts & GFC_FPE_INEXACT))
    estr_write (" IEEE_INEXACT_FLAG");

  estr_write ("\n");
}


/* A numeric STOP statement.  */

extern void stop_numeric (GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(stop_numeric);

void
stop_numeric (GFC_INTEGER_4 code)
{
  report_exception ();
  if (code == -1)
    code = 0;
  else
    st_printf ("STOP %d\n", (int)code);

  exit (code);
}


/* A Fortran 2008 numeric STOP statement.  */

extern void stop_numeric_f08 (GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(stop_numeric_f08);

void
stop_numeric_f08 (GFC_INTEGER_4 code)
{
  report_exception ();
  st_printf ("STOP %d\n", (int)code);
  exit (code);
}


/* A character string or blank STOP statement.  */

void
stop_string (const char *string, GFC_INTEGER_4 len)
{
  report_exception ();
  if (string)
    {
      estr_write ("STOP ");
      (void) write (STDERR_FILENO, string, len);
      estr_write ("\n");
    }
  exit (0);
}


/* Per Fortran 2008, section 8.4:  "Execution of a STOP statement initiates
   normal termination of execution. Execution of an ERROR STOP statement
   initiates error termination of execution."  Thus, error_stop_string returns
   a nonzero exit status code.  */

extern void error_stop_string (const char *, GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(error_stop_string);

void
error_stop_string (const char *string, GFC_INTEGER_4 len)
{
  report_exception ();
  estr_write ("ERROR STOP ");
  (void) write (STDERR_FILENO, string, len);
  estr_write ("\n");

  exit (1);
}


/* A numeric ERROR STOP statement.  */

extern void error_stop_numeric (GFC_INTEGER_4)
  __attribute__ ((noreturn));
export_proto(error_stop_numeric);

void
error_stop_numeric (GFC_INTEGER_4 code)
{
  report_exception ();
  st_printf ("ERROR STOP %d\n", (int) code);
  exit (code);
}
