// Native methods for StringBuilder.

/* Copyright (C) 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>
#include <gcj/cni.h>
#include <java/lang/StringBuilder.h>
#include <java/lang/String.h>

jboolean
java::lang::StringBuilder::regionMatches(jint offset, jstring other)
{
  int len = other->count;
  int index = 0;
  jchar *sc = elements (value);
  jchar *oc = _Jv_GetStringChars (other);
  while (--len >= 0)
    {
      if (sc[offset++] != oc[index++])
	return false;
    }
  return true;
}
