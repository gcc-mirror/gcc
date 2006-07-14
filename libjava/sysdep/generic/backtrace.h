// backtrace.h - Fallback backtrace implementation. default implementation.

/* Copyright (C) 2005, 2006  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_BACKTRACE_H__
#define __SYSDEP_BACKTRACE_H__

#include <java-stack.h>

/* Unwind through the call stack calling TRACE_FN with STATE for every stack
   frame.  Returns the reason why the unwinding was stopped.  */
_Unwind_Reason_Code
fallback_backtrace (_Unwind_Trace_Fn, _Jv_UnwindState *)
{
  return _URC_NO_REASON;
}
#endif
