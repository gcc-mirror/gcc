/* jawt.h -- the machine-independent parts of the AWT Native Interface
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


/*
 * The AWT Native Interface allows direct access to native screen
 * resources from within a Canvas's paint method.
 */

#ifndef __jawt_h__
#define __jawt_h__

#ifdef __cplusplus
extern "C"
{
#endif

#define JAWT_VERSION_1_3 0x10003
#define JAWT_VERSION_1_4 0x10004

#define JAWT_LOCK_ERROR 0x1
#define JAWT_LOCK_CLIP_CHANGED 0x2
#define JAWT_LOCK_BOUNDS_CHANGED 0x4
#define JAWT_LOCK_SURFACE_CHANGED 0x8

struct _JAWT_Rectangle
{
  jint x;
  jint y;
  jint width;
  jint height;
};

struct _JAWT_DrawingSurfaceInfo
{
  void* platformInfo;
  struct _JAWT_DrawingSurface *ds;
  struct _JAWT_Rectangle bounds;
  jint clipSize;
  struct _JAWT_Rectangle *clip;
};

struct _JAWT_DrawingSurface
{
  JNIEnv* env;
  jobject target;
  jint (JNICALL* Lock) (struct _JAWT_DrawingSurface*);
  struct _JAWT_DrawingSurfaceInfo* (JNICALL* GetDrawingSurfaceInfo) (struct _JAWT_DrawingSurface*);
  void (JNICALL* FreeDrawingSurfaceInfo) (struct _JAWT_DrawingSurfaceInfo*);
  void (JNICALL* Unlock) (struct _JAWT_DrawingSurface*);
};

struct _JAWT
{
  jint version;
  struct _JAWT_DrawingSurface* (JNICALL* GetDrawingSurface) (JNIEnv*, jobject);
  void (JNICALL* FreeDrawingSurface) (struct _JAWT_DrawingSurface*);
  void (JNICALL *Lock) (JNIEnv*);
  void (JNICALL *Unlock) (JNIEnv*);
  jobject (JNICALL *GetComponent)(JNIEnv*, void*);
};

typedef struct _JAWT_Rectangle JAWT_Rectangle;
typedef struct _JAWT_DrawingSurfaceInfo JAWT_DrawingSurfaceInfo;
typedef struct _JAWT_DrawingSurface JAWT_DrawingSurface;
typedef struct _JAWT JAWT;

JNIEXPORT jboolean JNICALL JAWT_GetAWT (JNIEnv* env, struct _JAWT* awt);

#ifdef __cplusplus
}
#endif

#endif /* __jawt_h__ */
