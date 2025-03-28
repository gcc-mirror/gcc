/* Stub unwinding implementation.

   Copyright (C) 2019-2025 Free Software Foundation, Inc.
   Contributed by Mentor Graphics

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "unwind.h"

_Unwind_Reason_Code
_Unwind_RaiseException(struct _Unwind_Exception *exc __attribute__ ((__unused__)))
{
  __builtin_abort ();
  return 0;
}

void
_Unwind_DeleteException (struct _Unwind_Exception *exc)
{
  if (exc->exception_cleanup)
    (*exc->exception_cleanup) (_URC_FOREIGN_EXCEPTION_CAUGHT, exc);
}

void
_Unwind_Resume (struct _Unwind_Exception *exc __attribute__ ((__unused__)))
{
  __builtin_abort ();
}

_Unwind_Reason_Code
_Unwind_Resume_or_Rethrow (struct _Unwind_Exception *exc __attribute__ ((__unused__)))
{
  __builtin_abort ();
  return 0;
}

_Unwind_Reason_Code
_Unwind_Backtrace(_Unwind_Trace_Fn trace, void * trace_argument)
{
  return 0;
}

_Unwind_Ptr
_Unwind_GetIPInfo (struct _Unwind_Context *c, int *ip_before_insn)
{
  return 0;
}
