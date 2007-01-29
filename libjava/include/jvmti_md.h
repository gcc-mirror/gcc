/* jvmti_md.h
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

#ifndef __GCJ_JVMTI_MD_H__
#define __GCJ_JVMTI_MD_H__

#ifdef __GCJ_JNI_IMPL__

/* If __GCJ_JNI_IMPL__ is defined, then we assume that we're building
   libgcj itself, and we include functions which should not be exposed
   to JVMTI users. */

/* The number of event slots needed to keep track of event reporting
   constraints for an environment. This will only work if the order of
   events listed in jvmtiEvent and jvmtiEventCallbacks is kept the same
   (which should not be a problem). */
#define EVENT_SLOTS \
  (int)(JVMTI_EVENT_VM_OBJECT_ALLOC - JVMTI_EVENT_VM_INIT + 1)

/* Contents of the jvmtiEnv; but only inside the implementation. */
#define _CLASSPATH_JVMTIENV_CONTENTS					\
  /* Event handlers registered via SetEventCallbacks */			\
  jvmtiEventCallbacks callbacks;					\
									\
  /* Array of event thread for which to report event. */		\
  /* NULL means all threads. One for each callback.   */		\
  jthread thread[EVENT_SLOTS];						\
  									\
  /* Array of notification modes for callbacks. */			\
  /* One for each callback.                     */			\
  bool enabled[EVENT_SLOTS];

/* Redefine the standard JVMTI types to something a little more
   precise than "jobject". */
#define _CLASSPATH_VM_JVMTI_TYPES_DEFINED
typedef java::lang::Thread *jthread;
typedef java::lang::ThreadGroup *jthreadGroup;
typedef jlong jlocation;
typedef struct _Jv_rawMonitorID *jrawMonitorID;

#endif /* __GCJ_JNI_IMPL__ */

#endif /* __GCJ_JVMTI_MD_H__ */
