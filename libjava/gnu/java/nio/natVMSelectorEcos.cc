// natVMSelectorImplEcos.cc

/* Copyright (C) 2003, 2004, 2007  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <platform.h>

#include <errno.h>
#include <string.h>

#include <gnu/java/nio/VMSelector.h>
#include <java/io/IOException.h>

jint
gnu::java::nio::VMSelector::select (jintArray read, jintArray write,
                                    jintArray except, jlong timeout)
{
  throw new ::java::io::IOException (JvNewStringUTF ("implSelect() not implemented"));
}
