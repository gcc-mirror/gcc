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


#include "gthread-jni.h"

/*
 * This code has been written specifically to be used with GTK+ 1.2.
 * `Real' GLIB threading is not supported.  We fake things where necessary.
 * Once we know we're running on a 1.2 VM, we can write a real implementation.
 */

static GMutex *
g_mutex_new_jni_impl (void)
{
  jclass obj_class;
  jobject *mutex;

  obj_class = (*gdk_env)->FindClass (gdk_env, "java/lang/Object");
  if (obj_class == NULL)
    return NULL;

  mutex = (jobject *) g_malloc (sizeof (jobject));
  *mutex = (*gdk_env)->AllocObject (gdk_env, obj_class);
  if (*mutex == NULL)
    {
      g_free (mutex);
      return NULL;
    }
  *mutex = (*gdk_env)->NewGlobalRef (gdk_env, *mutex);

  return (GMutex *) mutex;
}

static void
g_mutex_lock_jni_impl (GMutex *mutex)
{
  if (mutex && mutex == gdk_threads_mutex)
    (*gdk_env)->MonitorEnter (gdk_env, *((jobject *)mutex));
}

static gboolean
g_mutex_trylock_jni_impl (GMutex *mutex)
{
  return FALSE;
}

static void
g_mutex_unlock_jni_impl (GMutex *mutex)
{
  if (mutex && mutex == gdk_threads_mutex)
    (*gdk_env)->MonitorExit (gdk_env, *((jobject *)mutex));
}

static void
g_mutex_free_jni_impl (GMutex *mutex)
{
  if (mutex && mutex == gdk_threads_mutex)
    {
      (*gdk_env)->DeleteGlobalRef (gdk_env, *((jobject *)mutex));
      g_free (mutex);
    }
}

static GPrivate *
g_private_new_jni_impl (GDestroyNotify notify)
{
  return NULL;
}

static gpointer
g_private_get_jni_impl (GPrivate *private)
{
  return NULL;
}

static void
g_private_set_jni_impl (GPrivate *private, gpointer data)
{
}

static GCond *
g_cond_new_jni_impl ()
{
  return NULL;
}

static void
g_cond_signal_jni_impl (GCond *cond)
{
}

static void
g_cond_broadcast_jni_impl (GCond *cond)
{
}

static void
g_cond_wait_jni_impl (GCond *cond, GMutex *mutex)
{
}

static gboolean
g_cond_timed_wait_jni_impl (GCond *cond, GMutex *mutex)
{
  return FALSE;
}

static void
g_cond_free_jni_impl (GCond *cond)
{
}

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
  g_private_set_jni_impl      /* private_set */
};

void
gdk_threads_wake ()
{
}
