/* Copyright (C) 2012-2017 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
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

/* This file is part of the vtable verication runtime library (see
   comments in vtv_rts.cc for more information about vtable
   verification).  This file contains log file utilities.  */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if defined (__CYGWIN__) || defined (__MINGW32__)
#include <windows.h>
#else
#include <execinfo.h>
#endif

#include <unistd.h>
#include <errno.h>

#include "vtv_utils.h"

#ifndef HAVE_SECURE_GETENV
#  ifdef HAVE___SECURE_GETENV
#    define secure_getenv __secure_getenv
#  else
#    define secure_getenv getenv
#  endif
#endif

static int vtv_failures_log_fd = -1;

/* This function takes the NAME of a log file to open, attempts to
   open it in the logs_dir directory, and returns the resulting file
   descriptor.

   This function first checks to see if the user has specifed (via
   the environment variable VTV_LOGS_DIR) a directory to use for the
   vtable verification logs.  If that fails, the function will open
   the logs in the current directory.
*/

int
__vtv_open_log (const char *name)
{
  char log_name[1024];
  char log_dir[512];
#if defined (__CYGWIN__) || defined (__MINGW32__)
  pid_t process_id = GetCurrentProcessId ();
#else
  uid_t user_id = getuid ();
  pid_t process_id = getpid ();
#endif
  char *logs_prefix;
  bool logs_dir_specified = false;
  int fd = -1;

  logs_prefix = secure_getenv ("VTV_LOGS_DIR");
  if (logs_prefix && strlen (logs_prefix) > 0)
    {
      logs_dir_specified = true;
#ifdef __MINGW32__
      mkdir (logs_prefix);
#else
      mkdir (logs_prefix, S_IRWXU);
#endif

      snprintf (log_dir, sizeof (log_dir), "%s/vtv_logs", logs_prefix);

#ifdef __MINGW32__
      mkdir (log_dir);
#else
      mkdir (log_dir, S_IRWXU);
#endif
#if defined (__CYGWIN__) || defined (__MINGW32__)
      snprintf (log_name, sizeof (log_name), "%s_%d_%s", log_dir,
		(unsigned) process_id, name);
      fd = open (log_name, O_WRONLY | O_APPEND | O_CREAT, S_IRWXU);
#else
      snprintf (log_name, sizeof (log_name), "%s/%d_%d_%s", log_dir,
		(unsigned) user_id, (unsigned) process_id, name);
      fd = open (log_name, O_WRONLY | O_APPEND | O_CREAT | O_NOFOLLOW,
		 S_IRWXU);
#endif
    }
  else
    fd = dup (2);

  if (fd == -1)
    __vtv_add_to_log (2, "Cannot open log file %s %s\n", name,
                    strerror (errno));
  return fd;
}

/* This function takes a file descriptor (FD) and a string (STR) and
   tries to write the string to the file.  */

static int
vtv_log_write (int fd, const char *str)
{
  if (write (fd, str, strlen (str)) != -1)
    return 0;

  if (fd != 2) /* Make sure we dont get in a loop.  */
    __vtv_add_to_log (2, "Error writing to log: %s\n", strerror (errno));
  return -1;
}


/* This function takes a file decriptor (LOG_FILE) and an output
 format string (FORMAT), followed by zero or more print format
 arguments (the same as fprintf, for example).  It gets the current
 process ID and PPID, pre-pends them to the formatted message, and
 writes write it out to the log file referenced by LOG_FILE via calles
 to vtv_log_write.  */

int
__vtv_add_to_log (int log_file, const char * format, ...)
{
  /* We dont want to dynamically allocate this buffer. This should be
     more than enough in most cases. It if isn't we are careful not to
     do a buffer overflow.  */
  char output[1024];

  va_list ap;
  va_start (ap, format);

#if defined (__CYGWIN__) || defined (__MINGW32__)
  snprintf (output, sizeof (output), "VTV: PID=%ld ", GetCurrentProcessId ());
#else
  snprintf (output, sizeof (output), "VTV: PID=%d PPID=%d ", getpid (),
            getppid ());
#endif
  vtv_log_write (log_file, output);
  vsnprintf (output, sizeof (output), format, ap);
  vtv_log_write (log_file, output);
  va_end (ap);

  return 0;
}

/* Open error logging file, if not already open, and write vtable
   verification failure messages (LOG_MSG) to the log file.  Also
   generate a backtrace in the log file, if GENERATE_BACKTRACE is
   set.  */

void
__vtv_log_verification_failure (const char *log_msg, bool generate_backtrace)
{
  if (vtv_failures_log_fd == -1)
    vtv_failures_log_fd = __vtv_open_log ("vtable_verification_failures.log");

  if (vtv_failures_log_fd == -1)
    return;

  __vtv_add_to_log (vtv_failures_log_fd, "%s", log_msg);

#if !defined (__CYGWIN__) && !defined (__MINGW32__)
  if (generate_backtrace)
    {
#define STACK_DEPTH 20
      void *callers[STACK_DEPTH];
      int actual_depth = backtrace (callers, STACK_DEPTH);
      backtrace_symbols_fd (callers, actual_depth, vtv_failures_log_fd);
    }
#endif
}
