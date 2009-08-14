// natVMSecureRandomWin32.cc - Native part of VMSecureRandom class for Win32.

/* Copyright (C) 2009 Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include <gcj/cni.h>
#include <java/lang/InternalError.h>
#include <java/lang/UnsupportedOperationException.h>
#include <gnu/java/security/jce/prng/VMSecureRandom.h>

jint
gnu::java::security::jce::prng::VMSecureRandom::natGenerateSeed(jbyteArray byte_array, jint offset, jint length)
{
  if (length != 0)
    throw new UnsupportedOperationException (
      JvNewStringLatin1 ("natGenerateSeed is not available for Win32 target."));
  return 0;
}

