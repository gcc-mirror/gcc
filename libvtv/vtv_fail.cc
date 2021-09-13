/* Copyright (C) 2012-2021 Free Software Foundation, Inc.

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

/* This file is part of the vtable security feature implementation.
   The vtable security feature is designed to detect when a virtual
   call is about to be made through an invalid vtable pointer
   (possibly due to data corruption or malicious attacks).

   This file also contains the failure functions that get called when
   a vtable pointer is not found in the data set.  Two particularly
   important functions are __vtv_verify_fail and __vtv_really_fail.
   They are both externally visible.  __vtv_verify_fail is defined in
   such a way that it can be replaced by a programmer, if desired.  It
   is the function that __VLTVerifyVtablePointer calls if it can't
   find the pointer in the data set.  Allowing the programmer to
   overwrite this function means that he/she can do some alternate
   verification, including NOT failing in certain specific cases, if
   desired.  This may be the case if the programmer has to deal wtih
   unverified third party software, for example.  __vtv_really_fail is
   available for the programmer to call from his version of
   __vtv_verify_fail, if he decides the failure is real.

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if !defined (__CYGWIN__) && !defined (__MINGW32__)
#include <execinfo.h>
#endif

#include <unistd.h>

#include "vtv_utils.h"
#include "vtv_fail.h"

/* This is used to disable aborts for debugging purposes.  */
bool vtv_no_abort = false;


extern "C" {

  /* __fortify_fail is a function in glibc that calls __libc_message,
     causing it to print out a program termination error message
     (including the name of the binary being terminated), a stack
     trace where the error occurred, and a memory map dump.  Ideally
     we would have called __libc_message directly, but that function
     does not appear to be accessible to functions outside glibc,
     whereas __fortify_fail is.  We call __fortify_fail from
     __vtv_really_fail.  We looked at calling __libc_fatal, which is
     externally accessible, but it does not do the back trace and
     memory dump.  */

  extern void __fortify_fail (const char *) __attribute__((noreturn));

} /* extern "C" */

const unsigned long SET_HANDLE_HANDLE_BIT = 0x2;

/* Instantiate the template classes (in vtv_set.h) for our particular
   hash table needs.  */
typedef void * vtv_set_handle;
typedef vtv_set_handle * vtv_set_handle_handle; 

static int vtv_failures_log_fd = -1;

/* Open error logging file, if not already open, and write vtable
   verification failure messages (LOG_MSG) to the log file.  Also
   generate a backtrace in the log file, if GENERATE_BACKTRACE is
   set.  */

static void
log_error_message (const char *log_msg, bool generate_backtrace)
{
  if (vtv_failures_log_fd == -1)
    vtv_failures_log_fd = vtv_open_log ("vtable_verification_failures.log");

  if (vtv_failures_log_fd == -1)
    return;

  vtv_add_to_log (vtv_failures_log_fd, "%s", log_msg);

  if (generate_backtrace)
    {
#define STACK_DEPTH 20
      void *callers[STACK_DEPTH];
#if !defined (__CYGWIN__) && !defined (__MINGW32__)
      int actual_depth = backtrace (callers, STACK_DEPTH);
      backtrace_symbols_fd (callers, actual_depth, vtv_failures_log_fd);
#endif
    }
}

/* In the case where a vtable map variable is the only instance of the
   variable we have seen, it points directly to the set of valid
   vtable pointers.  All subsequent instances of the 'same' vtable map
   variable point to the first vtable map variable.  This function,
   given a vtable map variable PTR, checks a bit to see whether it's
   pointing directly to the data set or to the first vtable map
   variable.  */

static inline bool
is_set_handle_handle (void * ptr)
{
  return ((unsigned long) ptr & SET_HANDLE_HANDLE_BIT)
                                                      == SET_HANDLE_HANDLE_BIT;
}

/* Returns the actual pointer value of a vtable map variable, PTR (see
   comments for is_set_handle_handle for more details).  */

static inline vtv_set_handle * 
ptr_from_set_handle_handle (void * ptr)
{
  return (vtv_set_handle *) ((unsigned long) ptr & ~SET_HANDLE_HANDLE_BIT);
}

/* Given a vtable map variable, PTR, this function sets the bit that
   says this is the second (or later) instance of a vtable map
   variable.  */

static inline vtv_set_handle_handle
set_handle_handle (vtv_set_handle * ptr)
{
  return (vtv_set_handle_handle) ((unsigned long) ptr | SET_HANDLE_HANDLE_BIT);
}

/* This function is called from __VLTVerifyVtablePointerDebug; it
   sends as much debugging information as it can to the error log
   file, then calls __vtv_verify_fail.  SET_HANDLE_PTR is the pointer
   to the set of valid vtable pointers, VTBL_PTR is the pointer that
   was not found in the set, and DEBUG_MSG is the message to be
   written to the log file before failing. n */

void
__vtv_verify_fail_debug (void **set_handle_ptr, const void *vtbl_ptr, 
                         const char *debug_msg)
{
  log_error_message (debug_msg, false);

  /* Call the public interface in case it has been overwritten by
     user.  */
  __vtv_verify_fail (set_handle_ptr, vtbl_ptr);

  log_error_message ("Returned from __vtv_verify_fail."
                     " Secondary verification succeeded.\n", false);
}

/* This function calls __fortify_fail with a FAILURE_MSG and then
   calls abort.  */

void
__vtv_really_fail (const char *failure_msg)
{
  __fortify_fail (failure_msg);

  /* We should never get this far; __fortify_fail calls __libc_message
     which prints out a back trace and a memory dump and then is
     supposed to call abort, but let's play it safe anyway and call abort
     ourselves.  */
  abort ();
}

/* This function takes an error MSG, a vtable map variable
   (DATA_SET_PTR) and a vtable pointer (VTBL_PTR).  It is called when
   an attempt to verify VTBL_PTR with the set pointed to by
   DATA_SET_PTR failed.  It outputs a failure message with the
   addresses involved, and calls __vtv_really_fail.  */

static void
vtv_fail (const char *msg, void **data_set_ptr, const void *vtbl_ptr)
{
  char buffer[128];
  int buf_len;
  const char *format_str =
                 "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";

  snprintf (buffer, sizeof (buffer), format_str, vtbl_ptr,
            is_set_handle_handle(*data_set_ptr) ?
              ptr_from_set_handle_handle (*data_set_ptr) :
	      *data_set_ptr);
  buf_len = strlen (buffer);
  /*  Send this to to stderr.  */
  write (2, buffer, buf_len);

  if (!vtv_no_abort)
    __vtv_really_fail (msg);
}

/* Send information about what we were trying to do when verification
   failed to the error log, then call vtv_fail.  This function can be
   overwritten/replaced by the user, to implement a secondary
   verification function instead.  DATA_SET_PTR is the vtable map
   variable used for the failed verification, and VTBL_PTR is the
   vtable pointer that was not found in the set.  */

void
__vtv_verify_fail (void **data_set_ptr, const void *vtbl_ptr)
{
  char log_msg[256];
  snprintf (log_msg, sizeof (log_msg), "Looking for vtable %p in set %p.\n",
            vtbl_ptr,
            is_set_handle_handle (*data_set_ptr) ?
              ptr_from_set_handle_handle (*data_set_ptr) :
              *data_set_ptr);
  log_error_message (log_msg, false);

  const char *format_str =
            "*** Unable to verify vtable pointer (%p) in set (%p) *** \n";
  snprintf (log_msg, sizeof (log_msg), format_str, vtbl_ptr, *data_set_ptr);
  log_error_message (log_msg, false);
  log_error_message ("  Backtrace: \n", true);

  const char *fail_msg = "Potential vtable pointer corruption detected!!\n";
  vtv_fail (fail_msg, data_set_ptr, vtbl_ptr);
}

