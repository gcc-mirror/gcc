/* Implementation of the STOP statement.
   Copyright (C) 2002-2023 Free Software Foundation, Inc.
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

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <string.h>

/* Fortran 2008 demands: If any exception (14) is signaling on that image, the
   processor shall issue a warning indicating which exceptions are signaling;
   this warning shall be on the unit identified by the named constant
   ERROR_UNIT (13.8.2.8).  In line with other compilers, we do not report
   inexact - and we optionally ignore underflow, cf. thread starting at
   http://mailman.j3-fortran.org/pipermail/j3/2013-June/006452.html.  */

static void
report_exception (void)
{
  struct iovec iov[8];
  int set_excepts, iovcnt = 1;

  if (!compile_options.fpe_summary)
    return;

  set_excepts = get_fpu_except_flags ();
  if ((set_excepts & compile_options.fpe_summary) == 0)
    return;

  iov[0].iov_base = (char*) "Note: The following floating-point exceptions are signalling:";
  iov[0].iov_len = strlen (iov[0].iov_base);

  if ((compile_options.fpe_summary & GFC_FPE_INVALID)
      && (set_excepts & GFC_FPE_INVALID))
    {
      iov[iovcnt].iov_base = (char*) " IEEE_INVALID_FLAG";
      iov[iovcnt].iov_len = strlen (iov[iovcnt].iov_base);
      iovcnt++;
    }

  if ((compile_options.fpe_summary & GFC_FPE_ZERO)
      && (set_excepts & GFC_FPE_ZERO))
    {
      iov[iovcnt].iov_base = (char*) " IEEE_DIVIDE_BY_ZERO";
      iov[iovcnt].iov_len = strlen (iov[iovcnt].iov_base);
      iovcnt++;
    }

  if ((compile_options.fpe_summary & GFC_FPE_OVERFLOW)
      && (set_excepts & GFC_FPE_OVERFLOW))
    {
      iov[iovcnt].iov_base = (char*) " IEEE_OVERFLOW_FLAG";
      iov[iovcnt].iov_len = strlen (iov[iovcnt].iov_base);
      iovcnt++;
    }

  if ((compile_options.fpe_summary & GFC_FPE_UNDERFLOW)
      && (set_excepts & GFC_FPE_UNDERFLOW))
    {
      iov[iovcnt].iov_base = (char*) " IEEE_UNDERFLOW_FLAG";
      iov[iovcnt].iov_len = strlen (iov[iovcnt].iov_base);
      iovcnt++;
    }

  if ((compile_options.fpe_summary & GFC_FPE_DENORMAL)
      && (set_excepts & GFC_FPE_DENORMAL))
    {
      iov[iovcnt].iov_base = (char*) " IEEE_DENORMAL";
      iov[iovcnt].iov_len = strlen (iov[iovcnt].iov_base);
      iovcnt++;
    }

  if ((compile_options.fpe_summary & GFC_FPE_INEXACT)
      && (set_excepts & GFC_FPE_INEXACT))
    {
      iov[iovcnt].iov_base = (char*) " IEEE_INEXACT_FLAG";
      iov[iovcnt].iov_len = strlen (iov[iovcnt].iov_base);
      iovcnt++;
    }

  iov[iovcnt].iov_base = (char*) "\n";
  iov[iovcnt].iov_len = 1;
  iovcnt++;

  estr_writev (iov, iovcnt);
}


/* A numeric STOP statement.  */

extern _Noreturn void stop_numeric (int, bool);
export_proto(stop_numeric);

void
stop_numeric (int code, bool quiet)
{
  if (!quiet)
    {
      report_exception ();
      st_printf ("STOP %d\n", code);
    }
  exit (code);
}


/* A character string or blank STOP statement.  */

void
stop_string (const char *string, size_t len, bool quiet)
{
  if (!quiet)
    {
      report_exception ();
      if (string)
	{
	  struct iovec iov[3];
	  iov[0].iov_base = (char*) "STOP ";
	  iov[0].iov_len = strlen (iov[0].iov_base);
	  iov[1].iov_base = (char*) string;
	  iov[1].iov_len = len;
	  iov[2].iov_base = (char*) "\n";
	  iov[2].iov_len = 1;
	  estr_writev (iov, 3);
	}
    }
  exit (0);
}


/* Per Fortran 2008, section 8.4:  "Execution of a STOP statement initiates
   normal termination of execution. Execution of an ERROR STOP statement
   initiates error termination of execution."  Thus, error_stop_string returns
   a nonzero exit status code.  */

extern _Noreturn void error_stop_string (const char *, size_t, bool);
export_proto(error_stop_string);

void
error_stop_string (const char *string, size_t len, bool quiet)
{
  if (!quiet)
    {
      struct iovec iov[3];
      report_exception ();
      iov[0].iov_base = (char*) "ERROR STOP ";
      iov[0].iov_len = strlen (iov[0].iov_base);
      iov[1].iov_base = (char*) string;
      iov[1].iov_len = len;
      iov[2].iov_base = (char*) "\n";
      iov[2].iov_len = 1;
      estr_writev (iov, 3);
    }
  exit_error (1);
}


/* A numeric ERROR STOP statement.  */

extern _Noreturn void error_stop_numeric (int, bool);
export_proto(error_stop_numeric);

void
error_stop_numeric (int code, bool quiet)
{
  if (!quiet)
    {
      report_exception ();
      st_printf ("ERROR STOP %d\n", code);
    }
  exit_error (code);
}
