// natObject.cc - Implementation of the Object class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <string.h>

#pragma implementation "Object.h"

#include <gcj/cni.h>
#include <jvm.h>
#include <java/lang/Object.h>
#include <java-threads.h>
#include <java-signal.h>
#include <java/lang/CloneNotSupportedException.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/IllegalMonitorStateException.h>
#include <java/lang/InterruptedException.h>
#include <java/lang/NullPointerException.h>
#include <java/lang/Class.h>
#include <java/lang/Cloneable.h>
#include <java/lang/Thread.h>

#define CloneableClass _CL_Q34java4lang9Cloneable
extern java::lang::Class CloneableClass;



// This is used to represent synchronization information.
struct _Jv_SyncInfo
{
#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
  // We only need to keep track of initialization state if we can
  // possibly finalize this object.
  bool init;
#endif
  _Jv_ConditionVariable_t condition;
  _Jv_Mutex_t mutex;
};



jclass
java::lang::Object::getClass (void)
{
  _Jv_VTable **dt = (_Jv_VTable **) this;
  return (*dt)->clas;
}

jint
java::lang::Object::hashCode (void)
{
  return _Jv_HashCode (this);
}

jobject
java::lang::Object::clone (void)
{
  jclass klass = getClass ();
  jobject r;
  jint size;

  // We also clone arrays here.  If we put the array code into
  // __JArray, then we'd have to figure out a way to find the array
  // vtbl when creating a new array class.  This is easier, if uglier.
  if (klass->isArray())
    {
      __JArray *array = (__JArray *) this;
      jclass comp = getClass()->getComponentType();
      jint eltsize;
      if (comp->isPrimitive())
	{
	  r = _Jv_NewPrimArray (comp, array->length);
	  eltsize = comp->size();
	}
      else
	{
	  r = _Jv_NewObjectArray (array->length, comp, NULL);
	  eltsize = sizeof (jobject);
	}
      // We can't use sizeof on __JArray because we must account for
      // alignment of the element type.
      size = (_Jv_GetArrayElementFromElementType (array, comp) - (char *) array
	      + array->length * eltsize);
    }
  else
    {
      if (! CloneableClass.isAssignableFrom(klass))
	JvThrow (new CloneNotSupportedException);

      size = klass->size();
      r = JvAllocObject (klass, size);
    }

  memcpy ((void *) r, (void *) this, size);
  return r;
}


//
// Synchronization code.
//

// This global is used to make sure that only one thread sets an
// object's `sync_info' field.
static _Jv_Mutex_t sync_mutex;

// This macro is used to see if synchronization initialization is
// needed.
#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
#  define INIT_NEEDED(Obj) (! (Obj)->sync_info \
			    || ! ((_Jv_SyncInfo *) ((Obj)->sync_info))->init)
#else
#  define INIT_NEEDED(Obj) (! (Obj)->sync_info)
#endif

#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
// If we have to run a destructor for a sync_info member, then this
// function is registered as a finalizer for the sync_info.
static void
finalize_sync_info (jobject obj)
{
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) obj;
#if defined (_Jv_HaveCondDestroy)
  _Jv_CondDestroy (&si->condition);
#endif
#if defined (_Jv_HaveMutexDestroy)
  _Jv_MutexDestroy (&si->mutex);
#endif
  si->init = false;
}
#endif

// This is called to initialize the sync_info element of an object.
void
java::lang::Object::sync_init (void)
{
  _Jv_MutexLock (&sync_mutex);
  // Check again to see if initialization is needed now that we have
  // the lock.
  if (INIT_NEEDED (this))
    {
      // We assume there are no pointers in the sync_info
      // representation.
      _Jv_SyncInfo *si;
      // We always create a new sync_info, even if there is already
      // one available.  Any given object can only be finalized once.
      // If we get here and sync_info is not null, then it has already
      // been finalized.  So if we just reinitialize the old one,
      // we'll never be able to (re-)destroy the mutex and/or
      // condition variable.
      si = (_Jv_SyncInfo *) _Jv_AllocBytesChecked (sizeof (_Jv_SyncInfo));
      _Jv_MutexInit (&si->mutex);
      _Jv_CondInit (&si->condition);
#if defined (_Jv_HaveCondDestroy) || defined (_Jv_HaveMutexDestroy)
      // Register a finalizer.
      si->init = true;
      _Jv_RegisterFinalizer (si, finalize_sync_info);
#endif
      sync_info = (jobject) si;
    }
  _Jv_MutexUnlock (&sync_mutex);
}

void
java::lang::Object::notify (void)
{
  if (__builtin_expect (INIT_NEEDED (this), 0))
    sync_init ();
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) sync_info;
  if (__builtin_expect (_Jv_CondNotify (&si->condition, &si->mutex), 0))
    JvThrow (new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("current thread not owner")));
}

void
java::lang::Object::notifyAll (void)
{
  if (__builtin_expect (INIT_NEEDED (this), 0))
    sync_init ();
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) sync_info;
  if (__builtin_expect (_Jv_CondNotifyAll (&si->condition, &si->mutex), 0))
    JvThrow (new IllegalMonitorStateException(JvNewStringLatin1 
                                              ("current thread not owner")));
}

void
java::lang::Object::wait (jlong timeout, jint nanos)
{
  if (__builtin_expect (INIT_NEEDED (this), 0))
    sync_init ();
  if (__builtin_expect (timeout < 0 || nanos < 0 || nanos > 999999, 0))
    JvThrow (new IllegalArgumentException);
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) sync_info;
  switch (_Jv_CondWait (&si->condition, &si->mutex, timeout, nanos))
    {
      case _JV_NOT_OWNER:
	JvThrow (new IllegalMonitorStateException (JvNewStringLatin1 
                          ("current thread not owner")));        
      case _JV_INTERRUPTED:
	if (Thread::interrupted ())
	  JvThrow (new InterruptedException);        
    }
}

//
// Some runtime code.
//

// This function is called at system startup to initialize the
// `sync_mutex'.
void
_Jv_InitializeSyncMutex (void)
{
  _Jv_MutexInit (&sync_mutex);
}

jint
_Jv_MonitorEnter (jobject obj)
{
#ifndef HANDLE_SEGV
  if (__builtin_expect (! obj, 0))
    JvThrow (new java::lang::NullPointerException);
#endif
  if (__builtin_expect (INIT_NEEDED (obj), 0))
    obj->sync_init ();
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) obj->sync_info;
  return _Jv_MutexLock (&si->mutex);
}

jint
_Jv_MonitorExit (jobject obj)
{
  JvAssert (obj);
  JvAssert (! INIT_NEEDED (obj));
  _Jv_SyncInfo *si = (_Jv_SyncInfo *) obj->sync_info;
  if (__builtin_expect (_Jv_MutexUnlock (&si->mutex), 0))
    JvThrow (new java::lang::IllegalMonitorStateException);
  return 0;
}

void
_Jv_FinalizeObject (jobject obj)
{
  // Ignore exceptions.  From section 12.6 of the Java Language Spec.
  try
    {
      obj->finalize ();
    }
  catch (java::lang::Throwable *t)
    {
      // Ignore.
    }
}
