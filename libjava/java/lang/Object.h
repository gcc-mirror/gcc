// Object.h - Header file for java.lang.Object.  -*- c++ -*-

/* Copyright (C) 1998, 1999, 2000, 2001, 2004, 2005  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#ifndef __JAVA_LANG_OBJECT_H__
#define __JAVA_LANG_OBJECT_H__

#pragma interface

#include <gcj/javaprims.h>

extern "Java"
{
// This class is mainly here as a kludge to get G++ to allocate two
// extra entries in each vtable.
struct _JvObjectPrefix
{
protected:
  // New ABI Compatibility Dummy, #1 and 2.
  virtual void nacd_1 (void) {}; // This slot really contains the Class pointer.
  // For IA64, the GC descriptor goes into the second word of the nacd1 descr.
# ifndef __ia64__
    virtual void nacd_2 (void) {}; // Actually the GC bitmap marking descriptor.
# endif
};
}

// Forward declarations for friends of java::lang::Object
void _Jv_MonitorEnter (jobject obj);
void _Jv_MonitorExit (jobject obj);
void _Jv_InitializeSyncMutex (void);
void _Jv_FinalizeObject (jobject obj);
bool _Jv_ObjectCheckMonitor (jobject obj);

class java::lang::Object : public _JvObjectPrefix
{
protected:
  virtual void finalize (void);
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

  friend void ::_Jv_MonitorEnter (jobject obj);
  friend void ::_Jv_MonitorExit (jobject obj);
  friend void ::_Jv_InitializeSyncMutex (void);
  friend void ::_Jv_FinalizeObject (jobject obj);
  friend bool ::_Jv_ObjectCheckMonitor (jobject obj);

#ifdef JV_MARKOBJ_DECL
  friend JV_MARKOBJ_DECL;
#endif
#ifdef JV_MARKARRAY_DECL
  friend JV_MARKARRAY_DECL;
#endif

  static java::lang::Class class$;

protected:
  virtual jobject clone (void);

private:
  // This does not actually refer to a Java object.  Instead it is a
  // placeholder for a piece of internal data (the synchronization
  // information).
# ifndef JV_HASH_SYNCHRONIZATION
    jobject sync_info;
# endif

    // Initialize the sync_info field.  Not called with JV_HASH_SYNCHRONIZATION.
    void sync_init (void);

public:
  virtual void throwNoSuchMethodError (void);
};

#endif /* __JAVA_LANG_OBJECT_H__ */
