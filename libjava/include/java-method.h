// java-method.h - Header file for methodID instances.  -*- c++ -*-

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

extern inline jmethodID
_Jv_FromReflectedMethod(java::lang::reflect::Method *method)
{
  return (jmethodID)
    ((char *) method->declaringClass->methods + method->offset);
}
