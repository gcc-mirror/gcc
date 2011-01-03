/* jni_md.h
   Copyright (C) 2001, 2005, 2007, 2010 Free Software Foundation, Inc.

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

#ifndef __GCJ_JNI_MD_H__
#define __GCJ_JNI_MD_H__

#include <gcj/libgcj-config.h>

#ifdef __GCJ_JNI_IMPL__

/* If __GCJ_JNI_IMPL__ is defined, then we assume that we're building
   libgcj itself, and we include headers which taint the namespace
   more than is acceptable for the ordinary JNI user.  */
#include <gcj/javaprims.h>
#include <gcj/array.h>
#include <gnu/gcj/runtime/JNIWeakRef.h>

typedef gnu::gcj::runtime::JNIWeakRef *jweak;

typedef struct _Jv_JNIEnv JNIEnv;
typedef struct _Jv_JavaVM JavaVM;

#define JNI_TRUE true
#define JNI_FALSE false

/* We defined jobject and friends, so don't redefine them in jni.h.  */
#define _CLASSPATH_VM_JNI_TYPES_DEFINED

/* We defined jmethodID and and jfieldID, so don't redefine them in
   jni.h.  */
#define _CLASSPATH_VM_INTERNAL_TYPES_DEFINED

/* Contents of the JNIEnv; but only inside the implementation.  */
#define _CLASSPATH_JNIENV_CONTENTS					\
  /* The current exception.  */						\
  jthrowable ex;							\
									\
  /* The chain of local frames.  */					\
  struct _Jv_JNI_LocalFrame *locals;					\
									\
  /* The bottom-most element of the chain, initialized with the env and	\
     reused between non-nesting JNI calls.  */				\
  struct _Jv_JNI_LocalFrame *bottom_locals;


#else /* __GCJ_JNI_IMPL__ */

# ifdef __GNUC__

/* If we're using gcc, we can use a platform-independent scheme to get
   the right integer types.  FIXME: this is not always correct, for
   instance on the c4x it will be wrong -- it depends on whether
   QImode is 8 bits.  */
typedef int    jbyte  __attribute__((__mode__(__QI__)));
typedef int    jshort __attribute__((__mode__(__HI__)));
typedef int    jint   __attribute__((__mode__(__SI__)));
typedef int    jlong  __attribute__((__mode__(__DI__)));
typedef unsigned int   jboolean __attribute__((__mode__(__QI__)));
typedef unsigned short jchar __attribute__((__mode__(__HI__)));
typedef float  jfloat;
typedef double jdouble;
typedef jint jsize;

# else /* __GNUC__ */

#  ifdef JV_HAVE_INTTYPES_H

/* If <inttypes.h> is available, we use it.  */

#   include <inttypes.h>

typedef int8_t jbyte;
typedef int16_t jshort;
typedef int32_t jint;
typedef int64_t jlong;
typedef float jfloat;
typedef double jdouble;
typedef jint jsize;
typedef uint8_t jboolean;
typedef uint16_t jchar;

#  else /* JV_HAVE_INTTYPES_H */

/* For now, we require either gcc or <inttypes.h>.  If we did more
   work at configure time we could get around this, but right now it
   doesn't seem worth it.  */
#   error jni.h not ported to this platform

#  endif /* JV_HAVE_INTTYPES_H */

# endif /* __GNUC__ */

#endif /* __GCJ_JNI_IMPL__ */


/* Linkage and calling conventions. */
#if defined (_WIN32) || defined (__WIN32__) || defined (WIN32)

#define JNIIMPORT        __declspec(dllimport)
#define JNIEXPORT        __declspec(dllexport)

#define JNICALL          __stdcall

#else /* !( _WIN32 || __WIN32__ || WIN32) */

#define JNIIMPORT
#if defined(__GNUC__) && __GNUC__ > 3
#define JNIEXPORT __attribute__ ((visibility("default")))
#else
#define JNIEXPORT
#endif

#define JNICALL

#endif /* !( _WIN32 || __WIN32__ || WIN32) */

/* These defines apply to symbols in libgcj */
#ifdef __GCJ_DLL__
# ifdef __GCJ_JNI_IMPL__
#  define _CLASSPATH_JNIIMPEXP JNIEXPORT
# else
#  define _CLASSPATH_JNIIMPEXP JNIIMPORT
# endif /* ! __GCJ_JNI_IMPL__ */
#else /* ! __GCJ_DLL__ */
# define _CLASSPATH_JNIIMPEXP
#endif /*  __GCJ_DLL__ */

#endif /* __GCJ_JNI_MD_H__ */
