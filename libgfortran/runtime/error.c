/* Copyright (C) 2002, 2003, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of the GNU Fortran 95 runtime library (libgfortran).

Libgfortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

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

You should have received a copy of the GNU General Public License
along with libgfortran; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */


#include "config.h"
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <float.h>

#include "libgfortran.h"
#include "../io/io.h"

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
 * 4    Error during error processing (very bad)
 *
 * Other error returns are reserved for the STOP statement with a numeric code.
 */

/* locus variables.  These are optionally set by a caller before a
 * library subroutine is called.  They are always cleared on exit so
 * that files that report loci and those that do not can be linked
 * together without reporting an erroneous position. */

char *filename = 0;
iexport_data(filename);

unsigned line = 0;
iexport_data(line);

/* buffer for integer/ascii conversions.  */
static char buffer[sizeof (GFC_UINTEGER_LARGEST) * 8 + 1];


/* Returns a pointer to a static buffer. */

char *
gfc_itoa (GFC_INTEGER_LARGEST n)
{
  int negative;
  char *p;
  GFC_UINTEGER_LARGEST t;

  if (n == 0)
    {
      buffer[0] = '0';
      buffer[1] = '\0';
      return buffer;
    }

  negative = 0;
  t = n;
  if (n < 0)
    {
      negative = 1;
      t = -n; /*must use unsigned to protect from overflow*/
    }

  p = buffer + sizeof (buffer) - 1;
  *p-- = '\0';

  while (t != 0)
    {
      *p-- = '0' + (t % 10);
      t /= 10;
    }

  if (negative)
    *p-- = '-';
  return ++p;
}


/* xtoa()-- Integer to hexadecimal conversion.  Returns a pointer to a
 * static buffer. */

char *
xtoa (GFC_UINTEGER_LARGEST n)
{
  int digit;
  char *p;

  if (n == 0)
    {
      buffer[0] = '0';
      buffer[1] = '\0';
      return buffer;
    }

  p = buffer + sizeof (buffer) - 1;
  *p-- = '\0';

  while (n != 0)
    {
      digit = n & 0xF;
      if (digit > 9)
	digit += 'A' - '0' - 10;

      *p-- = '0' + digit;
      n >>= 4;
    }

  return ++p;
}


/* st_printf()-- simple printf() function for streams that handles the
 * formats %d, %s and %c.  This function handles printing of error
 * messages that originate within the library itself, not from a user
 * program. */

int
st_printf (const char *format, ...)
{
  int count, total;
  va_list arg;
  char *p, *q;
  stream *s;

  total = 0;
  s = init_error_stream ();
  va_start (arg, format);

  for (;;)
    {
      count = 0;

      while (format[count] != '%' && format[count] != '\0')
	count++;

      if (count != 0)
	{
	  p = salloc_w (s, &count);
	  memmove (p, format, count);
	  sfree (s);
	}

      total += count;
      format += count;
      if (*format++ == '\0')
	break;

      switch (*format)
	{
	case 'c':
	  count = 1;

	  p = salloc_w (s, &count);
	  *p = (char) va_arg (arg, int);

	  sfree (s);
	  break;

	case 'd':
	  q = gfc_itoa (va_arg (arg, int));
	  count = strlen (q);

	  p = salloc_w (s, &count);
	  memmove (p, q, count);
	  sfree (s);
	  break;

	case 'x':
	  q = xtoa (va_arg (arg, unsigned));
	  count = strlen (q);

	  p = salloc_w (s, &count);
	  memmove (p, q, count);
	  sfree (s);
	  break;

	case 's':
	  q = va_arg (arg, char *);
	  count = strlen (q);

	  p = salloc_w (s, &count);
	  memmove (p, q, count);
	  sfree (s);
	  break;

	case '\0':
	  return total;

	default:
	  count = 2;
	  p = salloc_w (s, &count);
	  p[0] = format[-1];
	  p[1] = format[0];
	  sfree (s);
	  break;
	}

      total += count;
      format++;
    }

  va_end (arg);
  return total;
}


/* st_sprintf()-- Simple sprintf() for formatting memory buffers. */

void
st_sprintf (char *buffer, const char *format, ...)
{
  va_list arg;
  char c, *p;
  int count;

  va_start (arg, format);

  for (;;)
    {
      c = *format++;
      if (c != '%')
	{
	  *buffer++ = c;
	  if (c == '\0')
	    break;
	  continue;
	}

      c = *format++;
      switch (c)
	{
	case 'c':
	  *buffer++ = (char) va_arg (arg, int);
	  break;

	case 'd':
	  p = gfc_itoa (va_arg (arg, int));
	  count = strlen (p);

	  memcpy (buffer, p, count);
	  buffer += count;
	  break;

	case 's':
	  p = va_arg (arg, char *);
	  count = strlen (p);

	  memcpy (buffer, p, count);
	  buffer += count;
	  break;

	default:
	  *buffer++ = c;
	}
    }

  va_end (arg);
}


/* show_locus()-- Print a line number and filename describing where
 * something went wrong */

void
show_locus (void)
{
  if (!options.locus || filename == NULL)
    return;

  st_printf ("At line %d of file %s\n", line, filename);
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
    sys_exit (4);

  magic = MAGIC;
}


/* os_error()-- Operating system error.  We get a message from the
 * operating system, show it and leave.  Some operating system errors
 * are caught and processed by the library.  If not, we come here. */

void
os_error (const char *message)
{
  recursion_check ();
  show_locus ();
  st_printf ("Operating system error: %s\n%s\n", get_oserror (), message);
  sys_exit (1);
}


/* void runtime_error()-- These are errors associated with an
 * invalid fortran program. */

void
runtime_error (const char *message)
{
  recursion_check ();
  show_locus ();
  st_printf ("Fortran runtime error: %s\n", message);
  sys_exit (2);
}
iexport(runtime_error);


/* void internal_error()-- These are this-can't-happen errors
 * that indicate something deeply wrong. */

void
internal_error (const char *message)
{
  recursion_check ();
  show_locus ();
  st_printf ("Internal Error: %s\n", message);
  sys_exit (3);
}


/* translate_error()-- Given an integer error code, return a string
 * describing the error. */

const char *
translate_error (int code)
{
  const char *p;

  switch (code)
    {
    case ERROR_EOR:
      p = "End of record";
      break;

    case ERROR_END:
      p = "End of file";
      break;

    case ERROR_OK:
      p = "Successful return";
      break;

    case ERROR_OS:
      p = "Operating system error";
      break;

    case ERROR_BAD_OPTION:
      p = "Bad statement option";
      break;

    case ERROR_MISSING_OPTION:
      p = "Missing statement option";
      break;

    case ERROR_OPTION_CONFLICT:
      p = "Conflicting statement options";
      break;

    case ERROR_ALREADY_OPEN:
      p = "File already opened in another unit";
      break;

    case ERROR_BAD_UNIT:
      p = "Unattached unit";
      break;

    case ERROR_FORMAT:
      p = "FORMAT error";
      break;

    case ERROR_BAD_ACTION:
      p = "Incorrect ACTION specified";
      break;

    case ERROR_ENDFILE:
      p = "Read past ENDFILE record";
      break;

    case ERROR_BAD_US:
      p = "Corrupt unformatted sequential file";
      break;

    case ERROR_READ_VALUE:
      p = "Bad value during read";
      break;

    case ERROR_READ_OVERFLOW:
      p = "Numeric overflow on read";
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
generate_error (int family, const char *message)
{
  /* Set the error status.  */
  if (ioparm.iostat != NULL)
    *ioparm.iostat = family;

  if (message == NULL)
    message =
      (family == ERROR_OS) ? get_oserror () : translate_error (family);

  if (ioparm.iomsg)
    cf_strcpy (ioparm.iomsg, ioparm.iomsg_len, message);

  /* Report status back to the compiler.  */
  switch (family)
    {
    case ERROR_EOR:
      ioparm.library_return = LIBRARY_EOR;
      if (ioparm.eor != 0)
	return;
      break;

    case ERROR_END:
      ioparm.library_return = LIBRARY_END;
      if (ioparm.end != 0)
	return;
      break;

    default:
      ioparm.library_return = LIBRARY_ERROR;
      if (ioparm.err != 0)
	return;
      break;
    }

  /* Return if the user supplied an iostat variable.  */
  if (ioparm.iostat != NULL)
    return;

  /* Terminate the program */

  runtime_error (message);
}



/* Possibly issue a warning/error about use of a nonstandard (or deleted)
   feature.  An error/warning will be issued if the currently selected
   standard does not contain the requested bits.  */

try
notify_std (int std, const char * message)
{
  int warning;

  warning = compile_options.warn_std & std;
  if ((compile_options.allow_std & std) != 0 && !warning)
    return SUCCESS;

  show_locus ();
  if (!warning)
    {
      st_printf ("Fortran runtime error: %s\n", message);
      sys_exit (2);
    }
  else
    st_printf ("Fortran runtime warning: %s\n", message);
  return FAILURE;
}
