// method.h - Header file for methodID instances.  -*- c++ -*-

/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_METHOD_H__
#define __GCJ_METHOD_H__

#include <java/lang/Class.h>

extern inline jmethodID
_Jv_FromReflectedMethod (java::lang::reflect::Method *method)
{
  return (jmethodID)
    ((char *) method->declaringClass->methods + method->offset);
}

extern inline jmethodID
_Jv_FromReflectedConstructor (java::lang::reflect::Constructor *constructor)
{
  return (jmethodID)
    ((char *) constructor->declaringClass->methods + constructor->offset);
}

extern inline jint
JvNumMethods (jclass klass)
{
  return klass->method_count;
}

extern inline jmethodID
JvGetFirstMethod (jclass klass)
{
  return &klass->methods[0];
}

#endif /* __GCJ_METHOD_H__ */
