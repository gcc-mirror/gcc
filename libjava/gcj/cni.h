// gcj/cni.h -*- c++ -*-
// This file describes the Cygnus Native Interface, CNI.
// It provides a nicer interface to many of the things in gcj/javaprims.h.

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_CNI_H__
#define __GCJ_CNI_H__

#include <java/lang/Object.h>
#include <java/lang/Class.h>

#include <gcj/array.h>

extern inline jobject
JvAllocObject (jclass cls)
{
  return _Jv_AllocObject (cls, cls->size());
}

extern inline jobject
JvAllocObject (jclass cls, jsize sz)
{
  return _Jv_AllocObject (cls, sz);
}

extern "C" jstring _Jv_NewStringUTF (const char *bytes);
extern "C" void _Jv_InitClass (jclass);

extern inline void
JvInitClass (jclass cls)
{
  return _Jv_InitClass (cls);
}

extern inline jstring
JvAllocString (jsize sz)
{
  return _Jv_AllocString (sz);
}

extern inline jstring
JvNewString (const jchar *chars, jsize len)
{
  return _Jv_NewString (chars, len);
}

extern inline jstring
JvNewStringLatin1 (const char *bytes, jsize len)
{
  return _Jv_NewStringLatin1 (bytes, len);
}

extern inline jstring
JvNewStringLatin1 (const char *bytes)
{
  return _Jv_NewStringLatin1 (bytes, strlen (bytes));
}

extern inline jchar *
_Jv_GetStringChars (jstring str)
{
  return (jchar*)((char*) str->data + str->boffset);
}

extern inline jchar*
JvGetStringChars (jstring str)
{
  return _Jv_GetStringChars (str);
}

extern inline jsize
JvGetStringUTFLength (jstring string)
{
  return _Jv_GetStringUTFLength (string);
}

extern inline jsize
JvGetStringUTFRegion (jstring str, jsize start, jsize len, char *buf) 
{ 
  return _Jv_GetStringUTFRegion (str, start, len, buf); 
} 

extern inline jstring
JvNewStringUTF (const char *bytes)
{
  return _Jv_NewStringUTF (bytes);
}

class JvSynchronize
{
private:
  jobject obj;
public:
  JvSynchronize (const jobject &o) : obj (o)
    { _Jv_MonitorEnter (obj); }
  ~JvSynchronize ()
    { _Jv_MonitorExit (obj); }
};

#ifdef SJLJ_EXCEPTIONS
#define _Jv_Throw _Jv_Sjlj_Throw
#endif

// Throw some exception.
extern void JvThrow (jobject obj) __attribute__ ((__noreturn__));
extern inline void
JvThrow (jobject obj)
{
  _Jv_Throw ((void *) obj);
}

/* Call malloc, but throw exception if insufficient memory. */
extern inline void *
JvMalloc (jsize size)
{
  return _Jv_Malloc (size);
}

extern inline void
JvFree (void *ptr)
{
  return _Jv_Free (ptr);
}
#endif /* __GCJ_CNI_H__ */
