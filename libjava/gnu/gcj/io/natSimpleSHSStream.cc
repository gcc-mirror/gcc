// natSimpleSHSStream.cc

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <string.h>
#include <stdlib.h>

#include <gnu/gcj/io/SimpleSHSStream.h>

#include <gcj/cni.h>
#include <jvm.h>

#define PROTO
#include "shs.h"


jbyteArray 
gnu::gcj::io::SimpleSHSStream::shsFinal (jbyteArray shs_info)
{
  SHS_INFO *info = (SHS_INFO *)elements(shs_info);
  ::shsFinal (info);

  jbyteArray buffer = JvNewByteArray (SHS_DIGESTSIZE);
  memcpy (elements (buffer), (jbyte *)&info->digest, SHS_DIGESTSIZE);
  return buffer;
}
    
void 
gnu::gcj::io::SimpleSHSStream::shsUpdate (jbyteArray shs_info, jbyteArray buf, jint count)
{
  SHS_INFO *info = (SHS_INFO *)elements(shs_info);
  uint8_t *buffer = (uint8_t *)elements(buf);
  
  ::shsUpdate (info, buffer, count);
}

jbyteArray 
gnu::gcj::io::SimpleSHSStream::shsInit ()
{
  jbyteArray result = JvNewByteArray (sizeof (SHS_INFO));
  SHS_INFO *info = (SHS_INFO *)elements(result);

  ::shsInit (info);
  return result;
}


