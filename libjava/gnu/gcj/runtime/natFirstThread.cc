// natFirstThread.cc - Implementation of FirstThread native methods.

/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

#include <config.h>

#include <stdlib.h>

#include <gcj/cni.h>
#include <jvm.h>
#include <jni.h>

#include <gnu/gcj/runtime/FirstThread.h>
#include <java/lang/Class.h>
#include <java/lang/String.h>
#include <java/lang/System.h>
#include <java/lang/reflect/Modifier.h>
#include <java/io/PrintStream.h>

#ifdef ENABLE_JVMPI
#include <jvmpi.h>
#include <java/lang/ThreadGroup.h>
#include <java/lang/UnsatisfiedLinkError.h>
#endif

#define DIE(Message)  die (JvNewStringLatin1 (Message))

typedef void main_func (jobject);

#ifdef WITH_JVMPI
extern void (*_Jv_JVMPI_Notify_THREAD_START) (JVMPI_Event *event);
#endif

/* This will be non-NULL if the user has preloaded a JNI library, or
   linked one into the executable.  */
extern "C" 
{
#pragma weak JNI_OnLoad
  extern jint JNI_OnLoad (JavaVM *, void *) __attribute__((weak));
}

void
gnu::gcj::runtime::FirstThread::run (void)
{

  Utf8Const* main_signature = _Jv_makeUtf8Const ("([Ljava.lang.String;)V", 22);
  Utf8Const* main_name = _Jv_makeUtf8Const ("main", 4);

  /* Some systems let you preload shared libraries before running a
     program.  Under Linux, this is done by setting the LD_PRELOAD
     environment variable.  We take advatage of this here to allow for
     dynamically loading a JNI library into a fully linked executable.  */

  if (JNI_OnLoad != NULL)
    {
      JavaVM *vm = _Jv_GetJavaVM ();
      if (vm == NULL)
	{
	  // FIXME: what?
	  return;
	}
      jint vers = JNI_OnLoad (vm, NULL);
      if (vers != JNI_VERSION_1_1 && vers != JNI_VERSION_1_2)
	{
	  // FIXME: unload the library.
	  _Jv_Throw (new java::lang::UnsatisfiedLinkError (JvNewStringLatin1 ("unrecognized version from preloaded JNI_OnLoad")));
	}
    }

  if (klass == NULL)
    klass = java::lang::Class::forName (klass_name);
  if (klass != NULL)
    _Jv_InitClass (klass);

  _Jv_Method *meth = _Jv_GetMethodLocal (klass, main_name, main_signature);

  // Some checks from Java Spec section 12.1.4.
  if (meth == NULL)
    DIE ("no suitable method `main' in class");
  if (! java::lang::reflect::Modifier::isStatic(meth->accflags))
    DIE ("`main' must be static");
  if (! java::lang::reflect::Modifier::isPublic(meth->accflags))
    DIE ("`main' must be public");

#ifdef WITH_JVMPI
  if (_Jv_JVMPI_Notify_THREAD_START)
    {
      JVMPI_Event event;

      jstring thread_name = getName ();
      jstring group_name = NULL, parent_name = NULL;
      java::lang::ThreadGroup *group = getThreadGroup ();

      if (group)
	{
	  group_name = group->getName ();
	  group = group->getParent ();

	  if (group)
	    parent_name = group->getName ();
	}

      int thread_len = thread_name ? JvGetStringUTFLength (thread_name) : 0;
      int group_len = group_name ? JvGetStringUTFLength (group_name) : 0;
      int parent_len = parent_name ? JvGetStringUTFLength (parent_name) : 0;

      char thread_chars[thread_len + 1];
      char group_chars[group_len + 1];
      char parent_chars[parent_len + 1];

      if (thread_name)
	JvGetStringUTFRegion (thread_name, 0, 
			      thread_name->length(), thread_chars);
      if (group_name)
	JvGetStringUTFRegion (group_name, 0, 
			      group_name->length(), group_chars);
      if (parent_name)
	JvGetStringUTFRegion (parent_name, 0, 
			      parent_name->length(), parent_chars);

      thread_chars[thread_len] = '\0';
      group_chars[group_len] = '\0';
      parent_chars[parent_len] = '\0';

      event.event_type = JVMPI_EVENT_THREAD_START;
      event.env_id = NULL;
      event.u.thread_start.thread_name = thread_chars;
      event.u.thread_start.group_name = group_chars;
      event.u.thread_start.parent_name = parent_chars;
      event.u.thread_start.thread_id = (jobjectID) this;
      event.u.thread_start.thread_env_id = _Jv_GetCurrentJNIEnv ();

      _Jv_DisableGC ();
      (*_Jv_JVMPI_Notify_THREAD_START) (&event);
      _Jv_EnableGC ();
    }
#endif

  main_func *real_main = (main_func *) meth->ncode;
  (*real_main) (args);
}
