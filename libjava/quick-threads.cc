// quick-threads.cc - interface between libjava and QuickThreads.

/* Copyright (C) 1998, 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/Thread.h>
#include <java/lang/System.h>


// Identifier for our piece of thread-local data.  Visible so that it
// can be used in quick-threads.h.
int _Jv_ThreadKey;

void
_Jv_ThreadStart (java::lang::Thread *thread, _Jv_Thread_t *data,
		 _Jv_ThreadStartFunc *meth)
{
  *data = coop_create ((coop_userf_t *) meth, (void *) thread,
		       thread->isDaemon());
  coop_setspecific (*data, _Jv_ThreadKey, thread);
}

static void
qthrow (void *except)
{
  _Jv_Throw ((java::lang::Throwable *) except);
}

// Function to destroy thread-specific data item.  We just don't care.
static void
destroy_data (void *)
{
}

void
_Jv_InitThreads (void)
{
  coop_init ();
  _Jv_ThreadKey = coop_key_create (destroy_data);
  coop_set_throw_function (qthrow);
}
