// no-thread.cc - Implementation of `no threads' threads.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <cni.h>
#include <jvm.h>
#include <java/lang/Thread.h>

java::lang::Thread *_Jv_OnlyThread = NULL;

void
_Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *,
		 _Jv_ThreadStartFunc *meth)
{
  JvAssert (! _Jv_OnlyThread);
  _Jv_OnlyThread = thread;
  (*meth) (thread);
}
