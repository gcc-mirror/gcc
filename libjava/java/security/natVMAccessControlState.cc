// natVMAccessControlState.cc -- Native part of the VMAccessControlState class.

/* Copyright (C) 2006 Free Software Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <java/lang/Thread.h>
#include <java/security/VMAccessControlState.h>

java::security::VMAccessControlState *
java::security::VMAccessControlState::getThreadState ()
{
  java::lang::Thread *thread = java::lang::Thread::currentThread ();
  if (thread == NULL)
    return NULL;

  VMAccessControlState *state =
    reinterpret_cast<VMAccessControlState *> (thread->accessControlState);
  if (state == NULL)
    thread->accessControlState = state = new VMAccessControlState ();

  return state;
}
