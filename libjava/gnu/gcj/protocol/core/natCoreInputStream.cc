// natCoreInputStream.cc -- C++ side of CoreInputStream

/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Anthony Green <green@redhat.com>.  */

#include <config.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <string.h>

#include <java/lang/NullPointerException.h>
#include <java/lang/ArrayIndexOutOfBoundsException.h>
#include <gnu/gcj/protocol/core/CoreInputStream.h>

jint
gnu::gcj::protocol::core::CoreInputStream::unsafeGetByte (jlong offset)
{
  return ((char *)ptr)[offset];
}

jint
gnu::gcj::protocol::core::CoreInputStream::copyIntoByteArray (jbyteArray dest,
							      jint offset,
							      jint numBytes)
{
  if (! dest)
    throw new java::lang::NullPointerException; 
  jsize destSize = JvGetArrayLength (dest);
  if (offset < 0 || numBytes < 0 || offset + numBytes < 0
      || offset + numBytes > destSize
      || pos + numBytes > count)
    throw new java::lang::ArrayIndexOutOfBoundsException;

  void *pcore = (void *) &((char*)ptr)[pos];
  void *pdest = (void *) (elements (dest) + offset);

  memcpy (pdest, pcore, numBytes);

  return 0;
}

