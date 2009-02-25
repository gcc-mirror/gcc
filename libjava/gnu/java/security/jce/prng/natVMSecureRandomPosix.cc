// natVMSecureRandomPosix.cc - Native part of VMSecureRandom class for POSIX.

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
#include <gnu/java/security/jce/prng/VMSecureRandom.h>

jint
gnu::java::security::jce::prng::VMSecureRandom::natGenerateSeed(jbyteArray byte_array, jint offset, jint length)
{
  int a, fd;
  jbyte *bytes = elements (byte_array);
  ssize_t count;

  for (a = 0; a < offset; ++a)
    bytes++;
  fd = open ("/dev/random", O_RDONLY);
  
  if (fd == -1)
    {
      jstring oserr = JvNewStringLatin1 (strerror (errno));
      throw new ::java::lang::InternalError 
	(JvNewStringLatin1 ("Error opening /dev/random: ")->concat(oserr));
    }

  count = read (fd, bytes, length);
  close (fd);

  if (count == -1)
    {
      jstring oserr = JvNewStringLatin1 (strerror (errno));
      throw new ::java::lang::InternalError 
	(JvNewStringLatin1 ("Error reading /dev/random: ")->concat(oserr));
    }

  return count;
}

