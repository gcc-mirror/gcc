// Object.h - Header file for java.lang.Object.  -*- c++ -*-

/* Copyright (C) 1998, 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_LANG_OBJECT_H__
#define __JAVA_LANG_OBJECT_H__

#pragma interface

#include <gcj/javaprims.h>

// This class is mainly here as a kludge to get G++ to allocate
// vtable pointer as the *first* word of each Object, instead of
// the second word (following sync_info).  Note that various pieces of
// code know that finalize() is the first method.  For instance,
// Object.java knows this, as does _Jv_AllocObject.

struct _JvObjectPrefix
{
protected:
  // This is disguised as the C++ vtbl.
  // _Jv_VTable*  vtable;

  virtual void finalize () = 0;
};

class java::lang::Object : public _JvObjectPrefix
{
public:
  // Order must match order in Object.java.
  jclass getClass (void);
  virtual jint hashCode (void);
  void notify (void);
  void notifyAll (void);
  void wait (jlong timeout, jint nanos);
  virtual jboolean equals (jobject obj);
  Object (void);
  virtual jstring toString (void);
  void wait (void);
  void wait (jlong timeout);

  friend jint _Jv_MonitorEnter (jobject obj);
  friend jint _Jv_MonitorExit (jobject obj);
  friend void _Jv_InitializeSyncMutex (void);
  friend void _Jv_FinalizeObject (jobject obj);

#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif
#ifdef JV_MARKARRAY_DECL
  friend JV_MARKARRAY_DECL;
#endif

protected:
  virtual jobject clone (void);
  virtual void finalize (void);

private:
  // This does not actually refer to a Java object.  Instead it is a
  // placeholder for a piece of internal data (the synchronization
  // information).
  jobject sync_info;

  // Initialize the sync_info field.
  void sync_init (void);

  static void hack12_6 (jobject f);
};

#endif /* __JAVA_LANG_OBJECT_H__ */
