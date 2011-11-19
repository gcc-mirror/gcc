/* Copyright (C) 2002, 2003, 2005, 2006, 2007, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

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
#include <assert.h>
#include <string.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <stdlib.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* <sys/time.h> has to be included before <sys/resource.h> to work
   around PR 30518; otherwise, MacOS 10.3.9 headers are just broken.  */
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif


#ifdef __MINGW32__
#define HAVE_GETPID 1
#include <process.h>
#endif


/* Termination of a program: F2008 2.3.5 talks about "normal
   termination" and "error termination". Normal termination occurs as
   a result of e.g. executing the end program statement, and executing
   the STOP statement. It includes the effect of the C exit()
   function. 

   Error termination is initiated when the ERROR STOP statement is
   executed, when ALLOCATE/DEALLOCATE fails without STAT= being
   specified, when some of the co-array synchronization statements
   fail without STAT= being specified, and some I/O errors if
   ERR/IOSTAT/END/EOR is not present, and finally EXECUTE_COMMAND_LINE
   failure without CMDSTAT=.

   2.3.5 also explains how co-images synchronize during termination.

   In libgfortran we have two ways of ending a program. exit(code) is
   a normal exit; calling exit() also causes open units to be
   closed. No backtrace or core dump is needed here. When something
   goes wrong, we have sys_abort() which tries to print the backtrace
   if -fbacktrace is enabled, and then dumps core; whether a core file
   is generated is system dependent. When aborting, we don't flush and
   close open units, as program memory might be corrupted and we'd
   rather risk losing dirty data in the buffers rather than corrupting
   files on disk.

*/

/* Error conditions.  The tricky part here is printing a message when
 * it is the I/O subsystem that is severely wounded.  Our goal is to
 * try and print something making the fewest assumptions possible,
 * then try to clean up before actually exiting.
 *
 * The following exit conditions are defined:
 * 0    Normal program exit.
 * 1    Terminated because of operating system error.
 * 2    Error in the runtime library
 * 3    Internal error in runtime library
 *
 * Other error returns are reserved for the STOP statement with a numeric code.
 */


/* Write a null-terminated C string to standard error. This function
   is async-signal-safe.  */

ssize_t
estr_write (const char *str)
{
  return write (STDERR_FILENO, str, strlen (str));
}


/* st_vprintf()-- vsnprintf-like function for error output.  We use a
   stack allocated buffer for formatting; since this function might be
   called from within a signal handler, printing directly to stderr
   with vfprintf is not safe since the stderr locking might lead to a
   deadlock.  */

#define ST_VPRINTF_SIZE 512

int
st_vprintf (const char *format, va_list ap)
{
  int written;
  char buffer[ST_VPRINTF_SIZE];

#ifdef HAVE_VSNPRINTF
  written = vsnprintf(buffer, ST_VPRINTF_SIZE, format, ap);
#else
  written = vsprintf(buffer, format, ap);

  if (written >= ST_VPRINTF_SIZE - 1)
    {
      /* The error message was longer than our buffer.  Ouch.  Because
	 we may have messed up things badly, report the error and
	 quit.  */
#define ERROR_MESSAGE "Internal error: buffer overrun in st_vprintf()\n"
      write (STDERR_FILENO, buffer, ST_VPRINTF_SIZE - 1);
      write (STDERR_FILENO, ERROR_MESSAGE, strlen(ERROR_MESSAGE));
      sys_abort ();
#undef ERROR_MESSAGE

    }
#endif

  written = write (STDERR_FILENO, buffer, written);
  return written;
}


int
st_printf (const char * format, ...)
{
  int written;
  va_list ap;
  va_start (ap, format);
  written = st_vprintf (format, ap);
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
      show_backtrace ();
      signal (SIGABRT, SIG_DFL);
    }

  abort();
}


/* gfc_xtoa()-- Integer to hexadecimal conversion.  */

const char *
gfc_xtoa (GFC_UINTEGER_LARGEST n, char *buffer, size_t len)
{
  int digit;
  char *p;

  assert (len >= GFC_XTOA_BUF_SIZE);

  if (n == 0)
    return "0";

  p = buffer + GFC_XTOA_BUF_SIZE - 1;
  *p = '\0';

  while (n != 0)
    {
      digit = n & 0xF;
      if (digit > 9)
	digit += 'A' - '0' - 10;

      *--p = '0' + digit;
      n >>= 4;
    }

  return p;
}


/* Hopefully thread-safe wrapper for a strerror_r() style function.  */

char *
gf_strerror (int errnum, 
             char * buf __attribute__((unused)), 
	     size_t buflen __attribute__((unused)))
{
#ifdef HAVE_STRERROR_R
  return
    __builtin_choose_expr (__builtin_classify_type (strerror_r (0, buf, 0))
			   == 5,
			   /* GNU strerror_r()  */
			   strerror_r (errnum, buf, buflen),
			   /* POSIX strerror_r ()  */
			   (strerror_r (errnum, buf, buflen), buf));
#else
  /* strerror () is not necessarily thread-safe, but should at least
     be available everywhere.  */
  return strerror (errnum);
#endif
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
      filename = filename_from_unit (cmp->unit);

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


#define STRERR_MAXSZ 256

/* os_error()-- Operating system error.  We get a message from the
 * operating system, show it and leave.  Some operating system errors
 * are caught and processed by the library.  If not, we come here. */

void
os_error (const char *message)
{
  char errmsg[STRERR_MAXSZ];
  recursion_check ();
  estr_write ("Operating system error: ");
  estr_write (gf_strerror (errno, errmsg, STRERR_MAXSZ));
  estr_write ("\n");
  estr_write (message);
  estr_write ("\n");
  exit (1);
}
iexport(os_error);


/* void runtime_error()-- These are errors associated with an
 * invalid fortran program. */

void
runtime_error (const char *message, ...)
{
  va_list ap;

  recursion_check ();
  estr_write ("Fortran runtime error: ");
  va_start (ap, message);
  st_vprintf (message, ap);
  va_end (ap);
  estr_write ("\n");
  exit (2);
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
  st_vprintf (message, ap);
  va_end (ap);
  estr_write ("\n");
  exit (2);
}
iexport(runtime_error_at);


void
runtime_warning_at (const char *where, const char *message, ...)
{
  va_list ap;

  estr_write (where);
  estr_write ("\nFortran runtime warning: ");
  va_start (ap, message);
  st_vprintf (message, ap);
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

  exit (3);
}


/* translate_error()-- Given an integer error code, return a string
 * describing the error. */

const char *
translate_error (int code)
{
  const char *p;

  switch (code)
    {
    case LIBERROR_EOR:
      p = "End of record";
      break;

    case LIBERROR_END:
      p = "End of file";
      break;

    case LIBERROR_OK:
      p = "Successful return";
      break;

    case LIBERROR_OS:
      p = "Operating system error";
      break;

    case LIBERROR_BAD_OPTION:
      p = "Bad statement option";
      break;

    case LIBERROR_MISSING_OPTION:
      p = "Missing statement option";
      break;

    case LIBERROR_OPTION_CONFLICT:
      p = "Conflicting statement options";
      break;

    case LIBERROR_ALREADY_OPEN:
      p = "File already opened in another unit";
      break;

    case LIBERROR_BAD_UNIT:
      p = "Unattached unit";
      break;

    case LIBERROR_FORMAT:
      p = "FORMAT error";
      break;

    case LIBERROR_BAD_ACTION:
      p = "Incorrect ACTION specified";
      break;

    case LIBERROR_ENDFILE:
      p = "Read past ENDFILE record";
      break;

    case LIBERROR_BAD_US:
      p = "Corrupt unformatted sequential file";
      break;

    case LIBERROR_READ_VALUE:
      p = "Bad value during read";
      break;

    case LIBERROR_READ_OVERFLOW:
      p = "Numeric overflow on read";
      break;

    case LIBERROR_INTERNAL:
      p = "Internal error in run-time library";
      break;

    case LIBERROR_INTERNAL_UNIT:
      p = "Internal unit I/O error";
      break;

    case LIBERROR_DIRECT_EOR:
      p = "Write exceeds length of DIRECT access record";
      break;

    case LIBERROR_SHORT_RECORD:
      p = "I/O past end of record on unformatted file";
      break;

    case LIBERROR_CORRUPT_FILE:
      p = "Unformatted file structure has been corrupted";
      break;

    default:
      p = "Unknown error code";
      break;
    }

  return p;
}


/* generate_error()-- Come here when an error happens.  This
 * subroutine is called if it is possible to continue on after the error.
 * If an IOSTAT or IOMSG variable exists, we set it.  If IOSTAT or
 * ERR labels are present, we return, otherwise we terminate the program
 * after printing a message.  The error code is always required but the
 * message parameter can be NULL, in which case a string describing
 * the most recent operating system error is used. */

void
generate_error (st_parameter_common *cmp, int family, const char *message)
{
  char errmsg[STRERR_MAXSZ];

  /* If there was a previous error, don't mask it with another
     error message, EOF or EOR condition.  */

  if ((cmp->flags & IOPARM_LIBRETURN_MASK) == IOPARM_LIBRETURN_ERROR)
    return;

  /* Set the error status.  */
  if ((cmp->flags & IOPARM_HAS_IOSTAT))
    *cmp->iostat = (family == LIBERROR_OS) ? errno : family;

  if (message == NULL)
    message =
      (family == LIBERROR_OS) ? gf_strerror (errno, errmsg, STRERR_MAXSZ) :
      translate_error (family);

  if (cmp->flags & IOPARM_HAS_IOMSG)
    cf_strcpy (cmp->iomsg, cmp->iomsg_len, message);

  /* Report status back to the compiler.  */
  cmp->flags &= ~IOPARM_LIBRETURN_MASK;
  switch (family)
    {
    case LIBERROR_EOR:
      cmp->flags |= IOPARM_LIBRETURN_EOR;
      if ((cmp->flags & IOPARM_EOR))
	return;
      break;

    case LIBERROR_END:
      cmp->flags |= IOPARM_LIBRETURN_END;
      if ((cmp->flags & IOPARM_END))
	return;
      break;

    default:
      cmp->flags |= IOPARM_LIBRETURN_ERROR;
      if ((cmp->flags & IOPARM_ERR))
	return;
      break;
    }

  /* Return if the user supplied an iostat variable.  */
  if ((cmp->flags & IOPARM_HAS_IOSTAT))
    return;

  /* Terminate the program */

  recursion_check ();
  show_locus (cmp);
  estr_write ("Fortran runtime error: ");
  estr_write (message);
  estr_write ("\n");
  exit (2);
}
iexport(generate_error);


/* generate_warning()-- Similar to generate_error but just give a warning.  */

void
generate_warning (st_parameter_common *cmp, const char *message)
{
  if (message == NULL)
    message = " ";

  show_locus (cmp);
  estr_write ("Fortran runtime warning: ");
  estr_write (message);
  estr_write ("\n");
}


/* Whether, for a feature included in a given standard set (GFC_STD_*),
   we should issue an error or a warning, or be quiet.  */

notification
notification_std (int std)
{
  int warning;

  if (!compile_options.pedantic)
    return NOTIFICATION_SILENT;

  warning = compile_options.warn_std & std;
  if ((compile_options.allow_std & std) != 0 && !warning)
    return NOTIFICATION_SILENT;

  return warning ? NOTIFICATION_WARNING : NOTIFICATION_ERROR;
}


/* Possibly issue a warning/error about use of a nonstandard (or deleted)
   feature.  An error/warning will be issued if the currently selected
   standard does not contain the requested bits.  */

try
notify_std (st_parameter_common *cmp, int std, const char * message)
{
  int warning;

  if (!compile_options.pedantic)
    return SUCCESS;

  warning = compile_options.warn_std & std;
  if ((compile_options.allow_std & std) != 0 && !warning)
    return SUCCESS;

  if (!warning)
    {
      recursion_check ();
      show_locus (cmp);
      estr_write ("Fortran runtime error: ");
      estr_write (message);
      estr_write ("\n");
      exit (2);
    }
  else
    {
      show_locus (cmp);
      estr_write ("Fortran runtime warning: ");
      estr_write (message);
      estr_write ("\n");
    }
  return FAILURE;
}
