// no-thread.cc - Implementation of `no threads' threads.

/* Copyright (C) 1998, 1999, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Thread.h>
#include <java/lang/InternalError.h>

java::lang::Thread *_Jv_OnlyThread = NULL;

_Jv_Thread_t *
_Jv_ThreadInitData (java::lang::Thread *thread)
{
  // It is ok to create a new Thread object, as long as it isn't started.
  if (_Jv_OnlyThread == NULL)
    _Jv_OnlyThread = thread;
  return NULL;
}

void
_Jv_ThreadStart (java::lang::Thread *, _Jv_Thread_t *, _Jv_ThreadStartFunc *)
{
  throw new java::lang::InternalError (JvNewStringLatin1 ("Thread.start called but threads not available"));
}
