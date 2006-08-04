// natVMMethod.cc -- native support for VMMethod

/* Copyright (C) 2006 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>

#include <gnu/classpath/jdwp/VMMethod.h>
#include <gnu/classpath/jdwp/util/LineTable.h>
#include <gnu/classpath/jdwp/util/VariableTable.h>

java::lang::String*
gnu::classpath::jdwp::VMMethod::getName ()
{
  return NULL;
}

java::lang::String*
gnu::classpath::jdwp::VMMethod::getSignature ()
{
  return NULL;
}

jint
gnu::classpath::jdwp::VMMethod::getModifiers ()
{
  return 0;
}

gnu::classpath::jdwp::util::LineTable*
gnu::classpath::jdwp::VMMethod::getLineTable ()
{
  return NULL;
}


gnu::classpath::jdwp::util::VariableTable*
gnu::classpath::jdwp::VMMethod::getVariableTable ()
{
  return NULL;
}
