// backtrace.h - Fallback backtrace implementation. default implementation.

/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __SYSDEP_BACKTRACE_H__
#define __SYSDEP_BACKTRACE_H__

#include <java-stack.h>

/* Store return addresses of the current program stack in
   STATE and return the exact number of values stored.  */
void
fallback_backtrace (_Jv_UnwindState *)
{
}
#endif
