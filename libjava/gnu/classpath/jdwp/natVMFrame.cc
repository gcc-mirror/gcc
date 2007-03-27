// natFrame.cc -- native support for VMFrame.java

/* Copyright (C) 2006, 2007 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License. Please consult the file "LIBGCJ_LICENSE" for
details. */

#include <gcj/cni.h>
#include <jvm.h>

#include <gnu/classpath/jdwp/VMFrame.h>
#include <gnu/classpath/jdwp/value/Value.h>

using namespace java::lang;

gnu::classpath::jdwp::value::Value *
gnu::classpath::jdwp::VMFrame::getValue (MAYBE_UNUSED jint slot,
					 MAYBE_UNUSED jbyte tag)
{
  return 0;
}

void
gnu::classpath::jdwp::VMFrame::setValue (MAYBE_UNUSED jint slot,
					 MAYBE_UNUSED gnu::classpath::jdwp::value::Value *value)
{
}
