/* Copyright (C) 2002-2019 Free Software Foundation, Inc.
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
#include "io.h"
#include "async.h"

#include <assert.h>
#include <string.h>
#include <errno.h>
#include <signal.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

/* <sys/time.h> has to be included before <sys/resource.h> to work
   around PR 30518; otherwise, MacOS 10.3.9 headers are just broken.  */
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif


#include <locale.h>

#ifdef HAVE_XLOCALE_H
#include <xlocale.h>
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

   In libgfortran we have three ways of ending a program. exit(code)
   is a normal exit; calling exit() also causes open units to be
   closed. No backtrace or core dump is needed here.  For error
   termination, we have exit_error(status), which prints a backtrace
   if backtracing is enabled, then exits.  Finally, when something
   goes terribly wrong, we have sys_abort() which tries to print the
   backtrace if -fbacktrace is enabled, and then dumps core; whether a
   core file is generated is system dependent. When aborting, we don't
   flush and close open units, as program memory might be corrupted
   and we'd rather risk losing dirty data in the buffers rather than
   corrupting files on disk.

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


/* Write a vector of strings to standard error.  This function is
   async-signal-safe.  */

ssize_t
estr_writev (const struct iovec *iov, int iovcnt)
{
#ifdef HAVE_WRITEV
  return writev (STDERR_FILENO, iov, iovcnt);
#else
  ssize_t w = 0;
  for (int i = 0; i < iovcnt; i++)
    {
      ssize_t r = write (STDERR_FILENO, iov[i].iov_base, iov[i].iov_len);
      if (r == -1)
	return r;
      w += r;
    }
  return w;
#endif
}


#ifndef HAVE_VSNPRINTF
static int
gf_vsnprintf (char *str, size_t size, const char *format, va_list ap)
{
  int written;

  written = vsprintf(buffer, format, ap);

  if (written >= size - 1)
    {
      /* The error message was longer than our buffer.  Ouch.  Because
	 we may have messed up things badly, report the error and
	 quit.  */
#define ERROR_MESSAGE "Internal error: buffer overrun in gf_vsnprintf()\n"
      write (STDERR_FILENO, buffer, size - 1);
      write (STDERR_FILENO, ERROR_MESSAGE, strlen (ERROR_MESSAGE));
      sys_abort ();
#undef ERROR_MESSAGE

    }
  return written;
}

#define vsnprintf gf_vsnprintf
#endif


/* printf() like function for for printing to stderr.  Uses a stack
   allocated buffer and doesn't lock stderr, so it should be safe to
   use from within a signal handler.  */

#define ST_ERRBUF_SIZE 512

int
st_printf (const char * format, ...)
{
  char buffer[ST_ERRBUF_SIZE];
  int written;
  va_list ap;
  va_start (ap, format);
  written = vsnprintf (buffer, ST_ERRBUF_SIZE, format, ap);
  va_end (ap);
  written = write (STDERR_FILENO, buffer, written);
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
      estr_write ("\nProgram aborted. Backtrace:\n");
      show_backtrace (false);
      signal (SIGABRT, SIG_DFL);
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
      estr_write ("\nError termination. Backtrace:\n");
      show_backtrace (false);
    }
  exit (status);
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


/* Hopefully thread-safe wrapper for a strerror() style function.  */

char *
gf_strerror (int errnum, 
             char * buf __attribute__((unused)), 
	     size_t buflen __attribute__((unused)))
{
#ifdef HAVE_STRERROR_L
  locale_t myloc = newlocale (LC_CTYPE_MASK | LC_MESSAGES_MASK, "",
			      (locale_t) 0);
  char *p;
  if (myloc)
    {
      p = strerror_l (errnum, myloc);
      freelocale (myloc);
    }
  else
    /* newlocale might fail e.g. due to running out of memory, fall
       back to the simpler strerror.  */
    p = strerror (errnum);
  return p;
#elif defined(HAVE_STRERROR_R)
#ifdef HAVE_USELOCALE
  /* Some targets (Darwin at least) have the POSIX 2008 extended
     locale functions, but not strerror_l.  So reset the per-thread
     locale here.  */
  uselocale (LC_GLOBAL_LOCALE);
#endif
  /* POSIX returns an "int", GNU a "char*".  */
  return
    __builtin_choose_expr (__builtin_classify_type (strerror_r (0, buf, 0))
			   == 5,
			   /* GNU strerror_r()  */
			   strerror_r (errnum, buf, buflen),
			   /* POSIX strerror_r ()  */
			   (strerror_r (errnum, buf, buflen), buf));
#elif defined(HAVE_STRERROR_R_2ARGS)
  strerror_r (errnum, buf);
  return buf;
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
 * abort immediately. */

static __gthread_key_t recursion_key;

static void
recursion_check (void)
{
  if (__gthread_active_p ())
    {
      bool* p = __gthread_getspecific (recursion_key);
      if (!p)
        {
          p = xcalloc (1, sizeof (bool));
          __gthread_setspecific (recursion_key, p);
        }
      if (*p)
	sys_abort ();
      *p = true;
    }
  else
    {
      static bool recur;
      if (recur)
	sys_abort ();
      recur = true;
    }
}

#ifdef __GTHREADS
static void __attribute__((constructor))
constructor_recursion_check (void)
{
  if (__gthread_active_p ())
    __gthread_key_create (&recursion_key, &free);
}

static void __attribute__((destructor))
destructor_recursion_check (void)
{
  if (__gthread_active_p ())
    __gthread_key_delete (recursion_key);
}
#endif



#define STRERR_MAXSZ 256

/* os_error()-- Operating system error.  We get a message from the
 * operating system, show it and leave.  Some operating system errors
 * are caught and processed by the library.  If not, we come here. */

void
os_error (const char *message)
{
  char errmsg[STRERR_MAXSZ];
  struct iovec iov[5];
  recursion_check ();
  iov[0].iov_base = (char*) "Operating system error: ";
  iov[0].iov_len = strlen (iov[0].iov_base);
  iov[1].iov_base = gf_strerror (errno, errmsg, STRERR_MAXSZ);
  iov[1].iov_len = strlen (iov[1].iov_base);
  iov[2].iov_base = (char*) "\n";
  iov[2].iov_len = 1;
  iov[3].iov_base = (char*) message;
  iov[3].iov_len = strlen (message);
  iov[4].iov_base = (char*) "\n";
  iov[4].iov_len = 1;
  estr_writev (iov, 5);
  exit_error (1);
}
iexport(os_error); /* TODO, DEPRECATED, ABI: Should not be exported
		      anymore when bumping so version.  */


/* Improved version of os_error with a printf style format string and
   a locus.  */

void
os_error_at (const char *where, const char *message, ...)
{
  char errmsg[STRERR_MAXSZ];
  char buffer[STRERR_MAXSZ];
  struct iovec iov[6];
  va_list ap;
  recursion_check ();
  int written;

  iov[0].iov_base = (char*) where;
  iov[0].iov_len = strlen (where);

  iov[1].iov_base = (char*) ": ";
  iov[1].iov_len = strlen (iov[1].iov_base);

  va_start (ap, message);
  written = vsnprintf (buffer, STRERR_MAXSZ, message, ap);
  va_end (ap);
  iov[2].iov_base = buffer;
  if (written >= 0)
    iov[2].iov_len = written;
  else
    iov[2].iov_len = 0;

  iov[3].iov_base = (char*) ": ";
  iov[3].iov_len = strlen (iov[3].iov_base);

  iov[4].iov_base = gf_strerror (errno, errmsg, STRERR_MAXSZ);
  iov[4].iov_len = strlen (iov[4].iov_base);

  iov[5].iov_base = (char*) "\n";
  iov[5].iov_len = 1;

  estr_writev (iov, 6);
  exit_error (1);
}
iexport(os_error_at);


/* void runtime_error()-- These are errors associated with an
 * invalid fortran program. */

void
runtime_error (const char *message, ...)
{
  char buffer[ST_ERRBUF_SIZE];
  struct iovec iov[3];
  va_list ap;
  int written;

  recursion_check ();
  iov[0].iov_base = (char*) "Fortran runtime error: ";
  iov[0].iov_len = strlen (iov[0].iov_base);
  va_start (ap, message);
  written = vsnprintf (buffer, ST_ERRBUF_SIZE, message, ap);
  va_end (ap);
  if (written >= 0)
    {
      iov[1].iov_base = buffer;
      iov[1].iov_len = written;
      iov[2].iov_base = (char*) "\n";
      iov[2].iov_len = 1;
      estr_writev (iov, 3);
    }
  exit_error (2);
}
iexport(runtime_error);

/* void runtime_error_at()-- These are errors associated with a
 * run time error generated by the front end compiler.  */

void
runtime_error_at (const char *where, const char *message, ...)
{
  char buffer[ST_ERRBUF_SIZE];
  va_list ap;
  struct iovec iov[4];
  int written;

  recursion_check ();
  iov[0].iov_base = (char*) where;
  iov[0].iov_len = strlen (where);
  iov[1].iov_base = (char*) "\nFortran runtime error: ";
  iov[1].iov_len = strlen (iov[1].iov_base);
  va_start (ap, message);
  written = vsnprintf (buffer, ST_ERRBUF_SIZE, message, ap);
  va_end (ap);
  if (written >= 0)
    {
      iov[2].iov_base = buffer;
      iov[2].iov_len = written;
      iov[3].iov_base = (char*) "\n";
      iov[3].iov_len = 1;
      estr_writev (iov, 4);
    }
  exit_error (2);
}
iexport(runtime_error_at);


void
runtime_warning_at (const char *where, const char *message, ...)
{
  char buffer[ST_ERRBUF_SIZE];
  va_list ap;
  struct iovec iov[4];
  int written;

  iov[0].iov_base = (char*) where;
  iov[0].iov_len = strlen (where);
  iov[1].iov_base = (char*) "\nFortran runtime warning: ";
  iov[1].iov_len = strlen (iov[1].iov_base);
  va_start (ap, message);
  written = vsnprintf (buffer, ST_ERRBUF_SIZE, message, ap);
  va_end (ap);
  if (written >= 0)
    {
      iov[2].iov_base = buffer;
      iov[2].iov_len = written;
      iov[3].iov_base = (char*) "\n";
      iov[3].iov_len = 1;
      estr_writev (iov, 4);
    }
}
iexport(runtime_warning_at);


/* void internal_error()-- These are this-can't-happen errors
 * that indicate something deeply wrong. */

void
internal_error (st_parameter_common *cmp, const char *message)
{
  struct iovec iov[3];

  recursion_check ();
  show_locus (cmp);
  iov[0].iov_base = (char*) "Internal Error: ";
  iov[0].iov_len = strlen (iov[0].iov_base);
  iov[1].iov_base = (char*) message;
  iov[1].iov_len = strlen (message);
  iov[2].iov_base = (char*) "\n";
  iov[2].iov_len = 1;
  estr_writev (iov, 3);

  /* This function call is here to get the main.o object file included
     when linking statically. This works because error.o is supposed to
     be always linked in (and the function call is in internal_error
     because hopefully it doesn't happen too often).  */
  stupid_function_name_for_static_linking();

 exit_error (3);
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

    case LIBERROR_INQUIRE_INTERNAL_UNIT:
      p = "Inquire statement identifies an internal file";
      break;

    default:
      p = "Unknown error code";
      break;
    }

  return p;
}


/* Worker function for generate_error and generate_error_async.  Return true
   if a straight return is to be done, zero if the program should abort. */

bool
generate_error_common (st_parameter_common *cmp, int family, const char *message)
{
  char errmsg[STRERR_MAXSZ];

#if ASYNC_IO
  gfc_unit *u;

  NOTE ("Entering generate_error_common");

  u = thread_unit;
  if (u && u->au)
    {
      if (u->au->error.has_error)
	return true;

      if (__gthread_equal (u->au->thread, __gthread_self ()))
	{
	  u->au->error.has_error = 1;
	  u->au->error.cmp = cmp;
	  u->au->error.family = family;
	  u->au->error.message = message;
	  return true;
	}
    }
#endif

  /* If there was a previous error, don't mask it with another
     error message, EOF or EOR condition.  */

  if ((cmp->flags & IOPARM_LIBRETURN_MASK) == IOPARM_LIBRETURN_ERROR)
    return true;

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
      cmp->flags |= IOPARM_LIBRETURN_EOR;  NOTE("EOR");
      if ((cmp->flags & IOPARM_EOR))
	return true;
      break;

    case LIBERROR_END:
      cmp->flags |= IOPARM_LIBRETURN_END; NOTE("END");
      if ((cmp->flags & IOPARM_END))
	return true;
      break;

    default:
      cmp->flags |= IOPARM_LIBRETURN_ERROR; NOTE("ERROR");
      if ((cmp->flags & IOPARM_ERR))
	return true;
      break;
    }

  /* Return if the user supplied an iostat variable.  */
  if ((cmp->flags & IOPARM_HAS_IOSTAT))
    return true;

  /* Return code, caller is responsible for terminating
   the program if necessary.  */

  recursion_check ();
  show_locus (cmp);
  struct iovec iov[3];
  iov[0].iov_base = (char*) "Fortran runtime error: ";
  iov[0].iov_len = strlen (iov[0].iov_base);
  iov[1].iov_base = (char*) message;
  iov[1].iov_len = strlen (message);
  iov[2].iov_base = (char*) "\n";
  iov[2].iov_len = 1;
  estr_writev (iov, 3);
  return false;
}

/* generate_error()-- Come here when an error happens.  This
 * subroutine is called if it is possible to continue on after the error.
 * If an IOSTAT or IOMSG variable exists, we set it.  If IOSTAT or
 * ERR labels are present, we return, otherwise we terminate the program
 * after printing a message.  The error code is always required but the
 * message parameter can be NULL, in which case a string describing
 * the most recent operating system error is used.
 * If the error is for an asynchronous unit and if the program is currently
 * executing the asynchronous thread, just mark the error and return.  */

void
generate_error (st_parameter_common *cmp, int family, const char *message)
{
  if (generate_error_common (cmp, family, message))
    return;

  exit_error(2);
}
iexport(generate_error);


/* generate_warning()-- Similar to generate_error but just give a warning.  */

void
generate_warning (st_parameter_common *cmp, const char *message)
{
  if (message == NULL)
    message = " ";

  show_locus (cmp);
  struct iovec iov[3];
  iov[0].iov_base = (char*) "Fortran runtime warning: ";
  iov[0].iov_len = strlen (iov[0].iov_base);
  iov[1].iov_base = (char*) message;
  iov[1].iov_len = strlen (message);
  iov[2].iov_base = (char*) "\n";
  iov[2].iov_len = 1;
  estr_writev (iov, 3);
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

bool
notify_std (st_parameter_common *cmp, int std, const char * message)
{
  int warning;
  struct iovec iov[3];

  if (!compile_options.pedantic)
    return true;

  warning = compile_options.warn_std & std;
  if ((compile_options.allow_std & std) != 0 && !warning)
    return true;

  if (!warning)
    {
      recursion_check ();
      show_locus (cmp);
      iov[0].iov_base = (char*) "Fortran runtime error: ";
      iov[0].iov_len = strlen (iov[0].iov_base);
      iov[1].iov_base = (char*) message;
      iov[1].iov_len = strlen (message);
      iov[2].iov_base = (char*) "\n";
      iov[2].iov_len = 1;
      estr_writev (iov, 3);
      exit_error (2);
    }
  else
    {
      show_locus (cmp);
      iov[0].iov_base = (char*) "Fortran runtime warning: ";
      iov[0].iov_len = strlen (iov[0].iov_base);
      iov[1].iov_base = (char*) message;
      iov[1].iov_len = strlen (message);
      iov[2].iov_base = (char*) "\n";
      iov[2].iov_len = 1;
      estr_writev (iov, 3);
    }
  return false;
}
