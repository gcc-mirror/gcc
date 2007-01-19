// natFrame.cc -- native support for VMFrame.java

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/classpath/jdwp/VMFrame.h>

using namespace java::lang;

Object*
gnu::classpath::jdwp::VMFrame::getValue (MAYBE_UNUSED jint slot)
{
  return 0;
}

void
gnu::classpath::jdwp::VMFrame::setValue (MAYBE_UNUSED jint slot,
					 MAYBE_UNUSED Object* value)
{
}
