// -*- c++ -*-
// no-gc.h - Defines for no garbage collector.

/* Copyright (C) 1998, 1999, 2006, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JV_NO_GC__
#define __JV_NO_GC__

// Suspend the given thread. This includes suspending the calling thread.
extern "C" void _Jv_SuspendThread (_Jv_Thread_t *);

// Resume a suspended thread.
extern "C" void _Jv_ResumeThread (_Jv_Thread_t *);

// Is the given thread suspended?
extern "C" int _Jv_IsThreadSuspended (_Jv_Thread_t *);
#endif /* __JV_NO_GC__ */
