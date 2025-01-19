/* Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Andy Vaught and Paul Brook <paul@nowt.org>

This file is part of the GNU Fortran runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

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

#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#if __nvptx__
/* 'printf' is all we have.  */
# undef estr_vprintf
# define estr_vprintf vprintf
#else
# error TODO
#endif


/* runtime/environ.c */

options_t options;


/* runtime/main.c */

/* Stupid function to be sure the constructor is always linked in, even
   in the case of static linking.  See PR libfortran/22298 for details.  */
void
stupid_function_name_for_static_linking (void)
{
  return;
}


static int argc_save;
static char **argv_save;


/* Set the saved values of the command line arguments.  */

void
set_args (int argc, char **argv)
{
  argc_save = argc;
  argv_save = argv;
}
iexport(set_args);


/* Retrieve the saved values of the command line arguments.  */

void
get_args (int *argc, char ***argv)
{
  *argc = argc_save;
  *argv = argv_save;
}


/* runtime/error.c */

/* Write a null-terminated C string to standard error. This function
   is async-signal-safe.  */

ssize_t
estr_write (const char *str)
{
  return write (STDERR_FILENO, str, strlen (str));
}


/* printf() like function for for printing to stderr.  Uses a stack
   allocated buffer and doesn't lock stderr, so it should be safe to
   use from within a signal handler.  */

int
st_printf (const char * format, ...)
{
  int written;
  va_list ap;
  va_start (ap, format);
  written = estr_vprintf (format, ap);
  va_end (ap);
  return written;
}


/* sys_abort()-- Terminate the program showing backtrace and dumping
   core.  */

void
sys_abort (void)
{
  /* If backtracing is enabled, print backtrace and disable signal
     handler for ABRT.  */
  if (options.backtrace == 1
      || (options.backtrace == -1 && compile_options.backtrace == 1))
    {
      estr_write ("\nProgram aborted.\n");
    }

  abort();
}


/* Exit in case of error termination. If backtracing is enabled, print
   backtrace, then exit.  */

void
exit_error (int status)
{
  if (options.backtrace == 1
      || (options.backtrace == -1 && compile_options.backtrace == 1))
    {
      estr_write ("\nError termination.\n");
    }
  exit (status);
}


/* show_locus()-- Print a line number and filename describing where
 * something went wrong */

void
show_locus (st_parameter_common *cmp)
{
  char *filename;

  if (!options.locus || cmp == NULL || cmp->filename == NULL)
    return;
  
  if (cmp->unit > 0)
    {
      filename = /* TODO filename_from_unit (cmp->unit) */ NULL;

      if (filename != NULL)
	{
	  st_printf ("At line %d of file %s (unit = %d, file = '%s')\n",
		   (int) cmp->line, cmp->filename, (int) cmp->unit, filename);
	  free (filename);
	}
      else
	{
	  st_printf ("At line %d of file %s (unit = %d)\n",
		   (int) cmp->line, cmp->filename, (int) cmp->unit);
	}
      return;
    }

  st_printf ("At line %d of file %s\n", (int) cmp->line, cmp->filename);
}


/* recursion_check()-- It's possible for additional errors to occur
 * during fatal error processing.  We detect this condition here and
 * exit with code 4 immediately. */

#define MAGIC 0x20DE8101

static void
recursion_check (void)
{
  static int magic = 0;

  /* Don't even try to print something at this point */
  if (magic == MAGIC)
    sys_abort ();

  magic = MAGIC;
}


/* os_error()-- Operating system error.  We get a message from the
 * operating system, show it and leave.  Some operating system errors
 * are caught and processed by the library.  If not, we come here. */

void
os_error (const char *message)
{
  recursion_check ();
  estr_write ("Operating system error: ");
  estr_write (message);
  estr_write ("\n");
  exit_error (1);
}
iexport(os_error); /* TODO, DEPRECATED, ABI: Should not be exported
		      anymore when bumping so version.  */


/* Improved version of os_error with a printf style format string and
   a locus.  */

void
os_error_at (const char *where, const char *message, ...)
{
  va_list ap;

  recursion_check ();
  estr_write (where);
  estr_write (": ");
  va_start (ap, message);
  estr_vprintf (message, ap);
  va_end (ap);
  estr_write ("\n");
  exit_error (1);
}
iexport(os_error_at);


/* void runtime_error()-- These are errors associated with an
 * invalid fortran program. */

void
runtime_error (const char *message, ...)
{
  va_list ap;

  recursion_check ();
  estr_write ("Fortran runtime error: ");
  va_start (ap, message);
  estr_vprintf (message, ap);
  va_end (ap);
  estr_write ("\n");
  exit_error (2);
}
iexport(runtime_error);

/* void runtime_error_at()-- These are errors associated with a
 * run time error generated by the front end compiler.  */

void
runtime_error_at (const char *where, const char *message, ...)
{
  va_list ap;

  recursion_check ();
  estr_write (where);
  estr_write ("\nFortran runtime error: ");
  va_start (ap, message);
  estr_vprintf (message, ap);
  va_end (ap);
  estr_write ("\n");
  exit_error (2);
}
iexport(runtime_error_at);


void
runtime_warning_at (const char *where, const char *message, ...)
{
  va_list ap;

  estr_write (where);
  estr_write ("\nFortran runtime warning: ");
  va_start (ap, message);
  estr_vprintf (message, ap);
  va_end (ap);
  estr_write ("\n");
}
iexport(runtime_warning_at);


/* void internal_error()-- These are this-can't-happen errors
 * that indicate something deeply wrong. */

void
internal_error (st_parameter_common *cmp, const char *message)
{
  recursion_check ();
  show_locus (cmp);
  estr_write ("Internal Error: ");
  estr_write (message);
  estr_write ("\n");

  /* This function call is here to get the main.o object file included
     when linking statically. This works because error.o is supposed to
     be always linked in (and the function call is in internal_error
     because hopefully it doesn't happen too often).  */
  stupid_function_name_for_static_linking();

  exit_error (3);
}


/* runtime/stop.c */

#undef report_exception
#define report_exception() do {} while (0)


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
	  estr_write ("STOP ");
	  (void) write (STDERR_FILENO, string, len);
	  estr_write ("\n");
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
      report_exception ();
      estr_write ("ERROR STOP ");
      (void) write (STDERR_FILENO, string, len);
      estr_write ("\n");
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
