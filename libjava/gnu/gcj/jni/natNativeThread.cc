// natNativeThread.cc - Native side of attached threads.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

// Written by Tom Tromey <tromey@cygnus.com>

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <gnu/gcj/jni/NativeThread.h>
#include <java/lang/Thread.h>

void
gnu::gcj::jni::NativeThread::finish ()
{
  finish_ ();
}
