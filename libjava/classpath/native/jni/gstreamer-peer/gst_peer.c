/*gst_peer.c - Common utility functions for the native peer.
 Copyright (C) 2007 Free Software Foundation, Inc.

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

#include <glib.h>

#include <jni.h>
#include "jcl.h"

#include "gst_peer.h"

JNIEnv *gst_get_jenv (JavaVM *vm)
{
  void *env = NULL;
  
  if ((*vm)->GetEnv(vm, &env, JNI_VERSION_1_2) != JNI_OK)
    {
      if ((*vm)->AttachCurrentThreadAsDaemon(vm, &env, NULL) < 0)
        {
          g_warning ("GstNativePipeline:- env not attached");
          return NULL;
        }
    }
  
  return (JNIEnv *) env;
}

void *
get_object_from_pointer (JNIEnv *env, jobject pointer, jfieldID pointerDataFID)
{
  void *_object = NULL;
  
  if (env == NULL)
    return NULL;
    
  if ((*env)->IsSameObject(env, pointer, NULL) == JNI_TRUE)
    return NULL;
  
#if SIZEOF_VOID_P == 8
  _object = (void *) (*env)->GetLongField(env, pointer, pointerDataFID);
#else
# if SIZEOF_VOID_P == 4
  _object = (void *) (*env)->GetIntField(env, pointer, pointerDataFID);
# else
#   error "Pointer size is not supported."
# endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */

  return _object;
}
