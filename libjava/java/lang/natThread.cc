// natThread.cc - Native part of Thread class.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <java-threads.h>

#include <java/lang/Thread.h>
#include <java/lang/ThreadGroup.h>
#include <java/lang/IllegalArgumentException.h>
#include <java/lang/UnsupportedOperationException.h>
#include <java/lang/IllegalThreadStateException.h>
#include <java/lang/InterruptedException.h>
#include <java/lang/NullPointerException.h>
#include <gnu/gcj/RawData.h>

#include <jni.h>



// This structure is used to represent all the data the native side
// needs.  An object of this type is assigned to the `data' member of
// the Thread class.
struct natThread
{
  // These are used to interrupt sleep and join calls.  We can share a
  // condition variable here since it only ever gets notified when the thread
  // exits.
  _Jv_Mutex_t join_mutex;
  _Jv_ConditionVariable_t join_cond;

  // This is private data for the thread system layer.
  _Jv_Thread_t *thread;

  // Each thread has its own JNI object.
  JNIEnv *jni_env;
};

// This is called from the constructor to initialize the native side
// of the Thread.
void
java::lang::Thread::initialize_native (void)
{
  // FIXME: this must interact with the GC in some logical way.  At
  // the very least we must register a finalizer to clean up.  This
  // isn't easy to do.  If the Thread object resurrects itself in its
  // own finalizer then we will need to reinitialize this structure at
  // any "interesting" point.
  natThread *nt = (natThread *) _Jv_AllocBytes (sizeof (natThread));
  data = reinterpret_cast<gnu::gcj::RawData *> (nt);
  _Jv_MutexInit (&nt->join_mutex);
  _Jv_CondInit (&nt->join_cond);
  _Jv_ThreadInitData (&nt->thread, this);
  // FIXME: if JNI_ENV is set we will want to free it.  It is
  // malloc()d.
  nt->jni_env = NULL;
}

jint
java::lang::Thread::countStackFrames (void)
{
  // NOTE: This is deprecated in JDK 1.2.
  JvFail ("java::lang::Thread::countStackFrames unimplemented");
  return 0;
}

java::lang::Thread *
java::lang::Thread::currentThread (void)
{
  return _Jv_ThreadCurrent ();
}

void
java::lang::Thread::destroy (void)
{
  // NOTE: This is marked as unimplemented in the JDK 1.2
  // documentation.
  JvFail ("java::lang::Thread::destroy unimplemented");
}

void
java::lang::Thread::interrupt (void)
{
  natThread *nt = (natThread *) data;
  _Jv_ThreadInterrupt (nt->thread);
}

void
java::lang::Thread::join (jlong millis, jint nanos)
{
  if (millis < 0 || nanos < 0 || nanos > 999999)
    _Jv_Throw (new IllegalArgumentException);

  Thread *current = currentThread ();

  // Here `NT' is the native structure for the thread we are trying to join.
  natThread *nt = (natThread *) data;

  // Now wait for: (1) an interrupt, (2) the thread to exit, or (3)
  // the timeout to occur. 
  _Jv_MutexLock (&nt->join_mutex);
  if (! isAlive ())
    {
      _Jv_MutexUnlock (&nt->join_mutex);
      return;
    }
  _Jv_CondWait (&nt->join_cond, &nt->join_mutex, millis, nanos);
  _Jv_MutexUnlock (&nt->join_mutex);

  if (current->isInterrupted (true))
    _Jv_Throw (new InterruptedException);
}

void
java::lang::Thread::resume (void)
{
  checkAccess ();
  JvFail ("java::lang::Thread::resume unimplemented");
}

void
java::lang::Thread::setPriority (jint newPriority)
{
  checkAccess ();
  if (newPriority < MIN_PRIORITY || newPriority > MAX_PRIORITY)
    _Jv_Throw (new IllegalArgumentException);

  jint gmax = group->getMaxPriority();
  if (newPriority > gmax)
    newPriority = gmax;

  priority = newPriority;
  natThread *nt = (natThread *) data;
  _Jv_ThreadSetPriority (nt->thread, priority);
}

void
java::lang::Thread::sleep (jlong millis, jint nanos)
{
  if (millis < 0 || nanos < 0 || nanos > 999999)
    _Jv_Throw (new IllegalArgumentException);

  if (millis == 0 && nanos == 0)
    ++nanos;

  Thread *current = currentThread ();

  // We use a condition variable to implement sleeping so that an
  // interrupt can wake us up. 
  natThread *nt = (natThread *) current->data;
  _Jv_MutexLock (&nt->join_mutex);
  _Jv_CondWait (&nt->join_cond, &nt->join_mutex, millis, nanos);
  _Jv_MutexUnlock (&nt->join_mutex);

  if (current->isInterrupted (true))
    _Jv_Throw (new InterruptedException);
}

void
java::lang::Thread::finish_ ()
{
  natThread *nt = (natThread *) data;
  
  group->remove (this);
  group = NULL;
  
  // Signal any threads that are waiting to join() us.
  _Jv_MutexLock (&nt->join_mutex);
  alive_flag = false;
  _Jv_CondNotifyAll (&nt->join_cond, &nt->join_mutex);
  _Jv_MutexUnlock (&nt->join_mutex);  
}

void
java::lang::Thread::run_ (jobject obj)
{
  java::lang::Thread *thread = (java::lang::Thread *) obj;
  try
    {
      thread->run ();
    }
  catch (java::lang::Throwable *t)
    {
      // Uncaught exceptions are forwarded to the ThreadGroup.  If
      // this results in an uncaught exception, that is ignored.
      try
	{
	  thread->group->uncaughtException (thread, t);
	}
      catch (java::lang::Throwable *f)
	{
	  // Nothing.
	}
    }

  thread->finish_ ();
}

void
java::lang::Thread::start (void)
{
  JvSynchronize sync (this);

  // Its illegal to re-start() a thread, even if its dead.
  if (!startable_flag)
    _Jv_Throw (new IllegalThreadStateException);

  alive_flag = true;
  startable_flag = false;
  natThread *nt = (natThread *) data;
  _Jv_ThreadStart (this, nt->thread, (_Jv_ThreadStartFunc *) &run_);
}

void
java::lang::Thread::stop (java::lang::Throwable *)
{
  _Jv_Throw (new UnsupportedOperationException
	     (JvNewStringLatin1 ("java::lang::Thread::stop unimplemented")));
}

void
java::lang::Thread::suspend (void)
{
  checkAccess ();
  _Jv_Throw (new UnsupportedOperationException 
	     (JvNewStringLatin1 ("java::lang::Thread::suspend unimplemented")));
}

void
java::lang::Thread::yield (void)
{
  _Jv_ThreadYield ();
}

JNIEnv *
_Jv_GetCurrentJNIEnv ()
{
  java::lang::Thread *t = _Jv_ThreadCurrent ();
  if (t == NULL)
    return NULL;
  return ((natThread *) t->data)->jni_env;
}

void
_Jv_SetCurrentJNIEnv (JNIEnv *env)
{
  java::lang::Thread *t = _Jv_ThreadCurrent ();
  JvAssert (t != NULL);
  ((natThread *) t->data)->jni_env = env;
}
