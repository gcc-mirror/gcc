/* gthread-jni.c -- JNI threading routines for GLIB
   Copyright (C) 1998 Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */

/************************************************************************/
/* Header				     				*/
/************************************************************************/

/*
 * Julian Dolby (dolby@us.ibm.com)
 * February 7, 2003
 *
 *  This code implements the GThreadFunctions interface for GLIB using 
 * Java threading primitives.  All of the locking and conditional variable
 * functionality required by GThreadFunctions is implemented using the
 * monitor and wait/notify functionality of Java objects.  The thread-
 * local fucntionality uses the java.lang.ThreadLocal class. 
 *
 *  This code is designed to be portable in that it makes no assumptions
 * about the underlying VM beyond that it implements the JNI functionality
 * that this code uses.
 *
 *  The one piece that does not really work is trylock for mutexes.  The
 * Java locking model does not include such functionality, and I do not
 * see how to implement it without knowing something about how the VM
 * implements locking.  
 *
 * NOTES:
 *
 *  I have tested it only on JikesRVM---the CVS head as of early February
 * 2003.
 *
 *  Currently, use of this code is governed by the configuration option
 * --enable-portable-native-sync
 *
 */


/************************************************************************/
/* Global data				     				*/
/************************************************************************/

#include "gthread-jni.h"

/*  The VM handle.  This is set in GtkToolkitMain.gtkInit */
JavaVM *gdk_vm;


/************************************************************************/
/* Utilities to reflect exceptions back to the VM			*/
/************************************************************************/

/*  This function checks for a pending exception, and rethrows it with
 * a wrapper RuntimeException to deal with possible type problems (in
 * case some calling piece of code does not expect the exception being
 * thrown) and to include the given extra message.
 */
static void maybe_rethrow(JNIEnv *gdk_env, char *message, char *file, int line) {
  jthrowable cause;

  /* rethrow if an exception happened */
  if ((cause = (*gdk_env)->ExceptionOccurred(gdk_env)) != NULL) {
    jstring jmessage;
  jclass obj_class;
    jobject obj;
    jmethodID ctor;

    /* allocate local message in Java */
    int len = strlen(message) + strlen(file) + 25;
    char buf[ len ];
    bzero(buf, len);
    sprintf(buf, "%s (at %s:%d)", message, file, line);
    jmessage = (*gdk_env)->NewStringUTF(gdk_env, buf);
    
    /* create RuntimeException wrapper object */
    obj_class = (*gdk_env)->FindClass (gdk_env, "java/lang/RuntimeException");
    ctor = (*gdk_env)->GetMethodID(gdk_env, obj_class, "<init>", "(Ljava/langString;Ljava/lang/Throwable)V");
    obj = (*gdk_env)->NewObject (gdk_env, obj_class, ctor, jmessage, cause);

    /* throw it */
    (*gdk_env)->Throw(gdk_env, (jthrowable)obj);
    }
}

/* This macro is used to include a source location in the exception message */
#define MAYBE_RETHROW(_class, _message) \
maybe_rethrow(_class, _message, __FILE__, __LINE__)


/************************************************************************/
/* Utilities to allocate and free java.lang.Objects			*/
/************************************************************************/

/*  Both the mutexes and the condition variables are java.lang.Object objects,
 * which this method allocates and returns a global ref.  Note that global
 * refs must be explicitly freed (isn't C fun?).
 */
static jobject *allocatePlainObject() {
  jclass obj_class;
  jobject *obj;
  JNIEnv *gdk_env;
  jmethodID ctor;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  obj_class = (*gdk_env)->FindClass (gdk_env, "java/lang/Object");
  MAYBE_RETHROW(gdk_env, "cannot find Object");

  ctor = (*gdk_env)->GetMethodID(gdk_env, obj_class, "<init>", "()V");
  MAYBE_RETHROW(gdk_env, "cannot find constructor");

  obj = (jobject *) g_malloc (sizeof (jobject));
  *obj = (*gdk_env)->NewObject (gdk_env, obj_class, ctor);
  MAYBE_RETHROW(gdk_env, "cannot allocate object");
  
  *obj = (*gdk_env)->NewGlobalRef (gdk_env, *obj);
  MAYBE_RETHROW(gdk_env, "cannot make global ref");

  return obj;
}

/*  Frees a Java object given a global ref (isn't C fun?) */
static void freePlainObject(jobject *obj) {
  JNIEnv *gdk_env;

  if (obj) {
    (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

    (*gdk_env)->DeleteGlobalRef (gdk_env, *obj);
    MAYBE_RETHROW(gdk_env, "cannot delete global ref");
  
    g_free (obj);
  }
}


/************************************************************************/
/* Locking code				     				*/
/************************************************************************/

/* Lock a Java object */
static void takeLock(JNIEnv *gdk_env, void *mutex) {
  (*gdk_env)->MonitorEnter (gdk_env, *((jobject *)mutex));
  MAYBE_RETHROW(gdk_env, "cannot get lock");
}

/* Unlock a Java object */
static void releaseLock(JNIEnv *gdk_env, void *mutex) {
    (*gdk_env)->MonitorExit (gdk_env, *((jobject *)mutex));
  MAYBE_RETHROW(gdk_env, "cannot release lock");
}

/* Create a mutex, which is a java.lang.Object for us */
static GMutex *g_mutex_new_jni_impl (void) {
  return (GMutex*) allocatePlainObject();
}

/* Lock a mutex. */
static void g_mutex_lock_jni_impl (GMutex *mutex __attribute__((unused))) {
  JNIEnv *gdk_env;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  takeLock(gdk_env, mutex);
}

/*  Try to lock a mutex.  Actually, do not try because Java objects
 * do not provide such an interface.  To be at least minimally correct,
 * pretend we tried and failed.
 */
static gboolean g_mutex_trylock_jni_impl
  (GMutex *mutex __attribute__((unused)))
{
  // Shall we implement this in a JikesRVM-specific way under a flag?
  return FALSE;
}

/* Unlock a mutex. */
static void g_mutex_unlock_jni_impl (GMutex *mutex) {
  JNIEnv *gdk_env;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  releaseLock(gdk_env, mutex);
}

/* Free a mutex (isn't C fun?) */
static void g_mutex_free_jni_impl (GMutex *mutex)
{
  freePlainObject( (jobject*)mutex );
}


/************************************************************************/
/* Condition variable code		     				*/
/************************************************************************/

/* Create a new condition variable.  This is a java.lang.Object for us. */
static GCond *g_cond_new_jni_impl () {
  return (GCond*)allocatePlainObject();
}

/*  Signal on a condition variable.  This is simply calling Object.notify
 * for us.
 */
static void g_cond_signal_jni_impl (GCond *cond) {
  jclass lcl_class;
  jmethodID signal_mth;
  JNIEnv *gdk_env;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.Object");
  MAYBE_RETHROW(gdk_env, "cannot find Object");
  
  signal_mth = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "notify", "()V");
  MAYBE_RETHROW(gdk_env, "cannot find Object.<notify>");

  /* Must have locked an object to call notify */
  takeLock(gdk_env, cond);

  (*gdk_env)->CallVoidMethod(gdk_env, *(jobject*)cond, signal_mth);
  MAYBE_RETHROW(gdk_env, "cannot signal mutex");

  releaseLock(gdk_env, cond);
}

/*  Broadcast to all waiting on a condition variable.  This is simply 
 * calling Object.notifyAll for us.
 */
static void g_cond_broadcast_jni_impl (GCond *cond) {
  jclass lcl_class;
  jmethodID bcast_mth;
  JNIEnv *gdk_env;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.Object");
  MAYBE_RETHROW(gdk_env, "cannot find Object");
  
  bcast_mth = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "notifyAll", "()V");
  MAYBE_RETHROW(gdk_env, "cannot find Object.<notifyAll>");

  /* Must have locked an object to call notifyAll */
  takeLock(gdk_env, cond);

  (*gdk_env)->CallVoidMethod(gdk_env, *(jobject*)cond, bcast_mth);
  MAYBE_RETHROW(gdk_env, "cannot broadcast to mutex");

  releaseLock(gdk_env, cond);
}


/*  Wait on a condition variable.  For us, this simply means call
 * Object.wait.
 */
static void g_cond_wait_jni_impl
  (GCond *cond, GMutex *mutex __attribute__((unused)))
{
  jclass lcl_class;
  jmethodID wait_mth;
  JNIEnv *gdk_env;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.Object");
  MAYBE_RETHROW(gdk_env, "cannot find Object");
  
  wait_mth = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "wait", "()V");
  MAYBE_RETHROW(gdk_env, "cannot find Object.<wait>");

  /* Must have locked an object to call wait */
  takeLock(gdk_env, cond);

  (*gdk_env)->CallVoidMethod(gdk_env, *(jobject*)cond, wait_mth);
  MAYBE_RETHROW(gdk_env, "cannot wait on mutex");

  releaseLock(gdk_env, cond);
}

/*  Wait on a condition vairable until a timeout.  This is a little tricky
 * for us.  We first call Object.wait(J) giving it the appropriate timeout
 * value.  On return, we check whether an InterruptedException happened.  If
 * so, that is Java-speak for wait timing out.
 */
static gboolean
g_cond_timed_wait_jni_impl
  (GCond *cond, GMutex *mutex __attribute__((unused)),
   GTimeVal *end_time)
{
  jclass lcl_class;
  jmethodID wait_mth;
  JNIEnv *gdk_env;
  jlong time;
  jthrowable cause;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.Object");
  MAYBE_RETHROW(gdk_env, "cannot find Object");
  
  wait_mth = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "wait", "(J)V");
  MAYBE_RETHROW(gdk_env, "cannot find Object.<wait(J)>");
  
  time = end_time->tv_sec*1000;
  time += end_time->tv_usec/1000;

  /* Must have locked an object to call wait */
  takeLock(gdk_env, cond);

  (*gdk_env)->CallVoidMethod(gdk_env, *(jobject*)cond, wait_mth, time);

  if ((cause = (*gdk_env)->ExceptionOccurred(gdk_env)) != NULL) {
    jclass intr = (*gdk_env)->FindClass (gdk_env, "java.lang.InterruptedException");
    if ( (*gdk_env)->IsInstanceOf(gdk_env, cause, intr) ) {
      releaseLock(gdk_env, cond);
  return FALSE;
    } else {
      MAYBE_RETHROW(gdk_env, "error in timed wait");
    }
  }

  releaseLock(gdk_env, cond);

  return TRUE;
}

/* Free a condition variable.  (isn't C fun?) */
static void g_cond_free_jni_impl (GCond *cond) {
  freePlainObject( (jobject*)cond );
}


/************************************************************************/
/* Thread-local data code		     				*/
/************************************************************************/

/*  Create a new thread-local key.  We use java.lang.ThreadLocal objects
 * for this.
 */
static GPrivate *g_private_new_jni_impl
  (GDestroyNotify notify __attribute__((unused)))
{
  jclass lcl_class;
  jobject *local;
  JNIEnv *gdk_env;
  jmethodID ctor;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.ThreadLocal");
  MAYBE_RETHROW(gdk_env, "cannot find ThreadLocal");

  ctor = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "<init>", "()V");
  MAYBE_RETHROW(gdk_env, "cannot find ThreadLocal.<init>");

  local = (jobject *) g_malloc (sizeof (jobject));
  *local = (*gdk_env)->NewObject(gdk_env, lcl_class, ctor);
  MAYBE_RETHROW(gdk_env, "cannot allocate a ThreadLocal");
  
  *local = ((*gdk_env)->NewGlobalRef (gdk_env, *local));
  MAYBE_RETHROW(gdk_env, "cannot create a GlobalRef");

  return (GPrivate*) local;
}

/*  Get this thread's value for a thread-local key.  This is simply
 * ThreadLocal.get for us.
 */
static gpointer g_private_get_jni_impl (GPrivate *private) {
  jclass lcl_class;
  jobject lcl_obj;
  JNIEnv *gdk_env;
  jmethodID get_mth;
  jclass int_class;
  jmethodID val_mth;
  jint int_val;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.ThreadLocal");
  MAYBE_RETHROW(gdk_env, "cannot find ThreadLocal");

  get_mth = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "get", "()Ljava/lang/Object;");
  MAYBE_RETHROW(gdk_env, "cannot find ThreadLocal.<get>");

  lcl_obj = (*gdk_env)->CallObjectMethod(gdk_env, *(jobject*)private, get_mth);
  MAYBE_RETHROW(gdk_env, "cannot find thread-local object");

  int_class = (*gdk_env)->FindClass (gdk_env, "java.lang.Integer");
  MAYBE_RETHROW(gdk_env, "cannot find Integer");

  val_mth = (*gdk_env)->GetMethodID(gdk_env, int_class, "intValue", "()I");
  MAYBE_RETHROW(gdk_env, "cannot find Integer.<intValue>");

  int_val = (*gdk_env)->CallIntMethod(gdk_env, lcl_obj, val_mth);
  MAYBE_RETHROW(gdk_env, "cannot get thread local value");

  return (gpointer) int_val;
}

/*  Set this thread's value for a thread-local key.  This is simply
 * ThreadLocal.set for us.
 */
static void g_private_set_jni_impl (GPrivate *private, gpointer data) {
  jclass lcl_class, int_class;
  jobject lcl_obj;
  JNIEnv *gdk_env;
  jmethodID new_int, set_mth;

  (*gdk_vm)->GetEnv(gdk_vm, (void **)&gdk_env, JNI_VERSION_1_1);

  int_class = (*gdk_env)->FindClass (gdk_env, "java.lang.Integer");
  MAYBE_RETHROW(gdk_env, "cannot find Integer");

  new_int = (*gdk_env)->GetMethodID(gdk_env, int_class, "<init>", "(I)V");
  MAYBE_RETHROW(gdk_env, "cannot find Integer.<init>");

  lcl_obj = (*gdk_env)->NewObject(gdk_env, int_class, new_int, (jint)data);
  MAYBE_RETHROW(gdk_env, "cannot create an Integer");

  lcl_class = (*gdk_env)->FindClass (gdk_env, "java.lang.ThreadLocal");
  MAYBE_RETHROW(gdk_env, "cannot find ThreadLocal");

  set_mth = (*gdk_env)->GetMethodID(gdk_env, lcl_class, "set", "(Ljava/lang/Object;)V");
  MAYBE_RETHROW(gdk_env, "cannot find ThreadLocal.<set>");

  (*gdk_env)->CallVoidMethod(gdk_env, *(jobject*)private, set_mth, lcl_obj);
  MAYBE_RETHROW(gdk_env, "cannot set thread local value");
}


/************************************************************************/
/* GLIB interface			     				*/
/************************************************************************/

/* set of function pointers to give to glib. */
GThreadFunctions g_thread_jni_functions =
{
  g_mutex_new_jni_impl,	      /* mutex_new */
  g_mutex_lock_jni_impl,      /* mutex_lock */
  g_mutex_trylock_jni_impl,   /* mutex_try_lock */
  g_mutex_unlock_jni_impl,    /* mutex_unlock */
  g_mutex_free_jni_impl,      /* mutex_free */
  g_cond_new_jni_impl,        /* cond_new */
  g_cond_signal_jni_impl,     /* cond_signal */
  g_cond_broadcast_jni_impl,  /* cond_broadcast */
  g_cond_wait_jni_impl,       /* cond_wait */
  g_cond_timed_wait_jni_impl, /* cond_timed_wait */
  g_cond_free_jni_impl,       /* cond_free */
  g_private_new_jni_impl,     /* private_new */
  g_private_get_jni_impl,     /* private_get */
  g_private_set_jni_impl,     /* private_set */
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL
};

/* ??? */
void gdk_threads_wake () {

}
