// natFinalizerThread.cc - Implementation of FinalizerThread native methods.

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/gcj/runtime/FinalizerThread.h>

void
gnu::gcj::runtime::FinalizerThread::runFinalizers ()
{
  _Jv_RunFinalizers ();
}
