/* Japhar implementation of VMI.
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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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

#ifndef __VMI_H__
#define __VMI_H__

#include <jni.h>

typedef void * jframeID;
typedef void * jthread;

typedef enum {
  VMI_ERROR_NONE,
  VMI_ERROR_NULL_POINTER,
  VMI_ERROR_OUT_OF_MEMORY,
  VMI_ERROR_INVALID_METHODID,
  VMI_ERROR_INVALID_CLASS,
  VMI_ERROR_INVALID_BCI,
  VMI_ERROR_NO_SUCH_BREAKPOINT,
  VMI_ERROR_VM_DEAD,
  VMI_ERROR_INVALID_FRAMEID,
  VMI_ERROR_INVALID_SLOT,
  VMI_ERROR_TYPE_MISMATCH,
  VMI_ERROR_NATIVE_FRAME,
  VMI_ERROR_NO_MORE_FRAMES,
  VMI_ERROR_INVALID_THREAD,
  VMI_ERROR_THREAD_NOT_SUSPENDED
} vmiError;


#define VMI_MOD_PUBLIC       0x0001
#define VMI_MOD_PRIVATE      0x0002
#define VMI_MOD_PROTECTED    0x0004
#define VMI_MOD_STATIC       0x0008
#define VMI_MOD_FINAL        0x0010
#define VMI_MOD_SYNCHRONIZED 0x0020
#define VMI_MOD_VOLATILE     0x0040
#define VMI_MOD_TRANSIENT    0x0080
#define VMI_MOD_NATIVE       0x0100
#define VMI_MOD_INTERFACE    0x0200
#define VMI_MOD_ABSTRACT     0x0400

JNIEXPORT vmiError JNICALL
VMI_GetFrameClass(JNIEnv *env, jframeID frame, jobject *obj);

JNIEXPORT vmiError JNICALL
VMI_GetFrameObject(JNIEnv *env, jframeID frame, jobject *obj);

JNIEXPORT vmiError JNICALL
VMI_GetThisFrame(JNIEnv *env, jframeID *frame);

JNIEXPORT vmiError JNICALL
VMI_GetThisThreadObject(JNIEnv *env, jthread *thread);

JNIEXPORT void JNICALL
VMI_ThrowAppropriateException(JNIEnv *env, vmiError err);

#endif
