// natObjectOutputStream.cc - Native part of ObjectOutputStream class.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This ObjectOutputStream is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the ObjectOutputStream "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java/io/ObjectOutputStream$PutField.h>
#include <java/io/ObjectOutputStream.h>
#include <java/io/IOException.h>
#include <java/lang/Class.h>


java::lang::reflect::Field *
java::io::ObjectOutputStream::getField (jclass klass, jstring name)
{
  return klass->getPrivateField (name);
}

java::lang::reflect::Method *
java::io::ObjectOutputStream::getMethod (jclass klass, jstring name, 
					 JArray<jclass> *arg_types)
{
  return klass->getPrivateMethod (name, arg_types);
}

