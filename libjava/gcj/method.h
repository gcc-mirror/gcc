// java-method.h - Header file for methodID instances.  -*- c++ -*-

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_METHOD_H__
#define __GCJ_METHOD_H__

extern inline jmethodID
_Jv_FromReflectedMethod(java::lang::reflect::Method *method)
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

#endif /* __GCJ_METHOD_H__ */
