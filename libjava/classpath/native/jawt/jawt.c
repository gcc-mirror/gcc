/* jawt.c -- X11 implementation of the AWT Native Interface
   Copyright (C) 2005 Free Software Foundation, Inc.

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

#include <stdlib.h>
#include <jni.h>
#include <jawt.h>
#include <jawt_md.h>
#include "classpath_jawt.h"

#ifndef __GNUC__
#define __attribute__(x) /* nothing */
#endif

static jint (JNICALL _Jv_Lock) (JAWT_DrawingSurface* surface);
static void (JNICALL _Jv_Unlock) (JAWT_DrawingSurface* surface);
static JAWT_DrawingSurfaceInfo* (JNICALL _Jv_GetDrawingSurfaceInfo)
     (JAWT_DrawingSurface* surface);
static void (JNICALL _Jv_FreeDrawingSurfaceInfo)
     (JAWT_DrawingSurfaceInfo* surface_info);
static JAWT_DrawingSurface* (JNICALL _Jv_GetDrawingSurface) (JNIEnv* env,
							     jobject canvas);
static void (JNICALL _Jv_FreeDrawingSurface) (JAWT_DrawingSurface* surface);
static void (JNICALL _Jv_AWTLock) (JNIEnv*);
static void (JNICALL _Jv_AWTUnlock) (JNIEnv*);

JNIEXPORT jboolean JNICALL
JAWT_GetAWT (JNIEnv* env __attribute__((unused)), JAWT* awt)
{
  jint retrieved_version;

  retrieved_version = classpath_jawt_get_awt_version ();

  if (awt->version > retrieved_version)
    return JNI_FALSE;

  awt->GetDrawingSurface = _Jv_GetDrawingSurface;
  awt->FreeDrawingSurface = _Jv_FreeDrawingSurface;
  awt->Lock = _Jv_AWTLock;
  awt->Unlock = _Jv_AWTUnlock;

  return JNI_TRUE;
}

/* JAWT_DrawingSurface functions */

static jint
(JNICALL _Jv_Lock) (JAWT_DrawingSurface* surface __attribute__((unused)))
{
  return classpath_jawt_lock ();
}

static void
(JNICALL _Jv_Unlock) (JAWT_DrawingSurface* surface __attribute__((unused)))
{
  classpath_jawt_unlock ();
}

static JAWT_DrawingSurfaceInfo*
(JNICALL _Jv_GetDrawingSurfaceInfo) (JAWT_DrawingSurface* surface)
{
  JAWT_DrawingSurfaceInfo* surface_info;
  JAWT_X11DrawingSurfaceInfo* surface_info_x11;

  if (surface == NULL || surface->target == NULL)
    return NULL;

  surface_info = (JAWT_DrawingSurfaceInfo*) malloc (sizeof (JAWT_DrawingSurfaceInfo));

  if (surface_info == NULL)
    return NULL;

  surface_info->platformInfo = malloc (sizeof (JAWT_X11DrawingSurfaceInfo));

  if (surface_info->platformInfo == NULL)
    return NULL;

  surface_info_x11 = (JAWT_X11DrawingSurfaceInfo*) surface_info->platformInfo;

  surface_info_x11->display = classpath_jawt_get_default_display (surface->env,
                                                                  surface->target);
  surface_info_x11->drawable = classpath_jawt_get_drawable (surface->env,
                                                            surface->target);
  surface_info_x11->visualID = classpath_jawt_get_visualID (surface->env,
                                                            surface->target);

  /* FIXME: also include bounding rectangle of drawing surface */
  /* FIXME: also include current clipping region */

  return surface_info;
}

static void
(JNICALL _Jv_FreeDrawingSurfaceInfo) (JAWT_DrawingSurfaceInfo* surface_info)
{
  JAWT_X11DrawingSurfaceInfo* surface_info_x11;

  if (surface_info == NULL)
    return;

  surface_info_x11 = (JAWT_X11DrawingSurfaceInfo*) surface_info->platformInfo;

  surface_info_x11->display = NULL;
  surface_info_x11->drawable = 0;
  surface_info_x11->visualID = 0;

  free (surface_info->platformInfo);
  free (surface_info);
  surface_info = NULL;
}

/* JAWT functions */

static JAWT_DrawingSurface*
(JNICALL _Jv_GetDrawingSurface) (JNIEnv* env, jobject canvas)
{
  JAWT_DrawingSurface* surface;

  surface = (JAWT_DrawingSurface*) malloc (sizeof (JAWT_DrawingSurface));

  if (surface == NULL)
    return NULL;

  surface->env = env;
  surface->target = canvas;

  /* initialize function pointers */
  surface->GetDrawingSurfaceInfo = _Jv_GetDrawingSurfaceInfo;
  surface->FreeDrawingSurfaceInfo = _Jv_FreeDrawingSurfaceInfo;

  surface->Lock = _Jv_Lock;
  surface->Unlock = _Jv_Unlock;

  return surface;
}

static void
(JNICALL _Jv_FreeDrawingSurface) (JAWT_DrawingSurface* surface)
{
  free (surface);
}

static void
(JNICALL _Jv_AWTLock) (JNIEnv* env __attribute__((unused)))
{
  /* FIXME: what is this supposed to do? */
}

static void
(JNICALL _Jv_AWTUnlock) (JNIEnv* env __attribute__((unused)))
{
  /* FIXME: what is this supposed to do? */
}
