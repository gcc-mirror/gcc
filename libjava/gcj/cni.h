// gcj/cni.h -*- c++ -*-
// This file describes the Compiled Native Interface, CNI.
// It provides a nicer interface to many of the things in gcj/javaprims.h.

/* Copyright (C) 1998, 1999, 2002  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __GCJ_CNI_H__
#define __GCJ_CNI_H__

#include <java/lang/Object.h>
#include <java/lang/Class.h>

#include <gcj/array.h>
#include <gcj/javaprims.h>

#include <string.h>

extern "C" void _Jv_InitClass (jclass);
extern "C" void *_Jv_AllocBytes (jsize size) __attribute__((__malloc__));

extern inline void
JvInitClass (jclass cls)
{
  return _Jv_InitClass (cls);
}

extern inline void *
JvAllocBytes (jsize sz)
{
  return _Jv_AllocBytes (sz);
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

/* Call malloc, but throw exception if insufficient memory. */
extern inline void *
JvMalloc (jsize size)
{
  return _Jv_Malloc (size);
}

extern inline void *
JvRealloc (void *ptr, jsize size)
{
  return _Jv_Realloc (ptr, size);
}

extern inline void
JvFree (void *ptr)
{
  return _Jv_Free (ptr);
}

typedef struct _Jv_VMOption JvVMOption;
typedef struct _Jv_VMInitArgs JvVMInitArgs;

extern inline jint
JvCreateJavaVM (JvVMInitArgs* vm_args)
{
  return _Jv_CreateJavaVM (vm_args);
}

extern inline java::lang::Thread*
JvAttachCurrentThread (jstring name, java::lang::ThreadGroup* group)
{
  return _Jv_AttachCurrentThread (name, group);
}

extern inline java::lang::Thread*
JvAttachCurrentThreadAsDaemon (jstring name, java::lang::ThreadGroup* group)
{
  return _Jv_AttachCurrentThreadAsDaemon (name, group);
}

extern inline jint
JvDetachCurrentThread (void)
{
  return _Jv_DetachCurrentThread ();
}
#endif /* __GCJ_CNI_H__ */
