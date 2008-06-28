/* gtkpeer.c -- Some GTK peer specific helper functions
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

#include "gtkpeer.h"
#include "jni.h"

/**
 * The Pointer class.
 */
static jclass pointerClass;

/**
 * The Pointer constructor.
 */
static jmethodID pointerConstructorMID;

/**
 * The field ID of the data field in the Pointer class.
 */
static jfieldID pointerDataFID;

/**
 * The field ID of the widget field in the GtkGenericPeer class.
 */
static jfieldID widgetFID;

/**
 * The field ID of the globalRef field in the GtkGenericPeer class.
 */
static jfieldID globalRefFID;

/**
 * The field ID of the display field in the GdkGraphicsEnvironment class.
 */
static jfieldID displayFID;

/**
 * The field ID of the screen field in the GdkScreenGraphicsDevice class.
 */
static jfieldID screenFID;

/**
 * The field ID of the nativeFont field in GdkFontPeer.
 */
static jfieldID fontFID;

/**
 * The field ID of the nativeDecoder field in GdkPixbufDecoder.
 */
static jfieldID pixbufLoaderFID;

/**
 * Initializes the IDs of the Pointer* classes.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_pointer_IDs(JNIEnv* env)
{
#if SIZEOF_VOID_P == 8
  pointerClass = (*env)->FindClass (env, "gnu/classpath/Pointer64");
  if (pointerClass != NULL)
    {
      pointerClass = (*env)->NewGlobalRef (env, pointerClass);
      pointerDataFID = (*env)->GetFieldID (env, pointerClass, "data", "J");
      pointerConstructorMID = (*env)->GetMethodID (env, pointerClass, "<init>",
						   "(J)V");
    }
#else
#if SIZEOF_VOID_P == 4
  pointerClass = (*env)->FindClass(env, "gnu/classpath/Pointer32");
  if (pointerClass != NULL)
    {
      pointerClass = (*env)->NewGlobalRef (env, pointerClass);
      pointerDataFID = (*env)->GetFieldID (env, pointerClass, "data", "I");
      pointerConstructorMID = (*env)->GetMethodID (env, pointerClass, "<init>",
                                                   "(I)V");
    }
#else
#error "Pointer size is not supported."
#endif /* SIZEOF_VOID_P == 4 */
#endif /* SIZEOF_VOID_P == 8 */
}

/**
 * Initializes the field IDs for the widget reference.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_widget_IDs(JNIEnv *env)
{
  jclass cls;

  /* Find the widget field ID in GtkGenericPeer. */
  cls = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GtkGenericPeer");
  widgetFID = (*env)->GetFieldID(env, cls, "widget",
                                 "Lgnu/classpath/Pointer;");

  /* Find the globalRef field in GtkGenericPeer. */
  globalRefFID = (*env)->GetFieldID(env, cls, "globalRef",
                                 "Lgnu/classpath/Pointer;");
}

/**
 * Stores the GTK widget reference in the GtkGenericPeer object.
 *
 * @param env the JNI environment
 * @param peer the actual peer object
 * @param widget the widget reference to store
 */
void gtkpeer_set_widget(JNIEnv *env, jobject peer, void *widget)
{
  jobject obj;

  /* Fetch the widget field object. */
  obj = (*env)->GetObjectField(env, peer, widgetFID);
  if (obj == NULL)
    {
      /* Create if necessary. */
#if SIZEOF_VOID_P == 8
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jlong) widget);
#else
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jint) widget);
#endif
      (*env)->SetObjectField(env, peer, widgetFID, obj);
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, obj, pointerDataFID, (jlong) widget);
#else
      (*env)->SetIntField(env, obj, pointerDataFID, (jint) widget);
#endif
    }
}

/**
 * Retrieves the GTK widget reference from a GtkGenericPeer object.
 *
 * @param env the JNI environment
 * @param peer the actual peer object
 *
 * @return the widget reference
 */
void* gtkpeer_get_widget(JNIEnv *env, jobject peer)
{
  jobject obj;
  void *widget;

  /* Fetch the widget field from the peer object. */
  obj = (*env)->GetObjectField(env, peer, widgetFID);

  /* Fetch actual widget pointer. */
#if SIZEOF_VOID_P == 8
  widget = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  widget = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  return widget;
}


/**
 * Stores the global JNI reference of a peer inside the peer.
 *
 * @param env the JNI environment
 * @param peer the peer object
 */
void gtkpeer_set_global_ref(JNIEnv *env, jobject peer)
{
  jobject obj;
  void* globalRef;

  /* Create global reference. */
  globalRef = (*env)->NewGlobalRef(env, peer);

  /* Fetch the globalRef field object. */
  obj = (*env)->GetObjectField(env, peer, globalRefFID);
  if (obj == NULL)
    {
      /* Create if necessary. */
#if SIZEOF_VOID_P == 8
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jlong) globalRef);
#else
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jint) globalRef);
#endif
      (*env)->SetObjectField(env, peer, globalRefFID, obj);
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, obj, pointerDataFID, (jlong) globalRef);
#else
      (*env)->SetIntField(env, obj, pointerDataFID, (jint) globalRef);
#endif
    }
}

/**
 * Retrieves the global reference from a peer.
 *
 * @param env the JNI environment
 * @param peer the peer object
 *
 * @return the global reference
 */
void* gtkpeer_get_global_ref(JNIEnv *env, jobject peer)
{
  jobject obj;
  void *globalRef;

  /* Fetch the globalRef field from the peer object. */
  obj = (*env)->GetObjectField(env, peer, globalRefFID);

  /* Fetch actual globalRef pointer. */
#if SIZEOF_VOID_P == 8
  globalRef = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  globalRef = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  return globalRef;
}

/**
 * Deletes the global reference of a peer. This is necessary in order to
 * allow the peer to be garbage collected.
 *
 * @param env the JNI environment
 * @param peer the peer object.
 */
void gtkpeer_del_global_ref(JNIEnv* env, jobject peer)
{
  jobject obj;
  void *globalRef;

  /* Fetch the globalRef field from the peer object. */
  obj = (*env)->GetObjectField(env, peer, globalRefFID);

  /* Fetch actual globalRef pointer. */
#if SIZEOF_VOID_P == 8
  globalRef = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  globalRef = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  (*env)->DeleteGlobalRef(env, globalRef);
}

/**
 * Initializes the fieldIDs for the display and screen fields.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_display_IDs(JNIEnv* env)
{
  jclass cls;

  /* Find the display field ID in GdkGraphicsEnvironment. */
  cls = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GdkGraphicsEnvironment");
  displayFID = (*env)->GetFieldID(env, cls, "display",
                                  "Lgnu/classpath/Pointer;");
}

/**
 * Sets the native display pointer in the GdkGraphicsEnvironment object.
 *
 * @param env the JNI environment
 * @param graphicsenv the GdkGraphicsEnvironment object
 * @param display the native display pointer
 */
void gtkpeer_set_display(JNIEnv* env, jobject graphicsenv, void* display)
{
  jobject obj;

  /* Fetch the display field object. */
  obj = (*env)->GetObjectField(env, graphicsenv, displayFID);
  if (obj == NULL)
    {
      /* Create if necessary. */
#if SIZEOF_VOID_P == 8
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jlong) display);
#else
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jint) display);
#endif
      (*env)->SetObjectField(env, graphicsenv, displayFID, obj);
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, obj, pointerDataFID, (jlong) display);
#else
      (*env)->SetIntField(env, obj, pointerDataFID, (jint) display);
#endif
    }
}

/**
 * Fetches the native display pointer from the GdkGraphicsEnvironment object.
 *
 * @param env the JNI environment
 * @param graphicsenv the GdkGraphicsEnvironment object
 *
 * @return the native display pointer
 */
void* gtkpeer_get_display(JNIEnv* env, jobject graphicsenv)
{
  jobject obj;
  void *display;

  /* Fetch the display field from the peer object. */
  obj = (*env)->GetObjectField(env, graphicsenv, displayFID);

  /* Fetch actual display pointer. */
#if SIZEOF_VOID_P == 8
  display = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  display = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  return display;
}

/**
 * Initializes the fieldIDs for the screen field.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_screen_IDs(JNIEnv* env)
{
  jclass cls;

  /* Find the display field ID in GdkScreenGraphicsDevice. */
  cls = (*env)->FindClass(env,
                          "gnu/java/awt/peer/gtk/GdkScreenGraphicsDevice");
  screenFID = (*env)->GetFieldID(env, cls, "screen",
                                 "Lgnu/classpath/Pointer;");
}

/**
 * Sets the native screen in the GdkScreenGraphicsDevice object.
 *
 * @param env the JNI environment
 * @param screen_graphics_device the GdkScreenGraphicsDevice object
 * @param ptr the native screen pointer
 */
void gtkpeer_set_screen(JNIEnv* env, jobject screen_graphics_device,
                        void* ptr)
{
  jobject obj;

  /* Fetch the screen field object. */
  obj = (*env)->GetObjectField(env, screen_graphics_device, screenFID);
  if (obj == NULL)
    {
      /* Create if necessary. */
#if SIZEOF_VOID_P == 8
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jlong) ptr);
#else
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jint) ptr);
#endif
      (*env)->SetObjectField(env, screen_graphics_device, screenFID, obj);
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, obj, pointerDataFID, (jlong) ptr);
#else
      (*env)->SetIntField(env, obj, pointerDataFID, (jint) ptr);
#endif
    }
}

/**
 * Fetches the native screen pointer from the GdkScreenGraphicsDevice object.
 *
 * @param env the JNI environment
 * @param screen_graphics_device the GdkScreenGraphicsDevice object
 *
 * @return the native screen pointer
 */
void* gtkpeer_get_screen(JNIEnv* env, jobject screen_graphics_device)
{
  jobject obj;
  void *screen;

  /* Fetch the display field from the peer object. */
  obj = (*env)->GetObjectField(env, screen_graphics_device, screenFID);

  /* Fetch actual display pointer. */
#if SIZEOF_VOID_P == 8
  screen = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  screen = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  return screen;
}

/**
 * Initializes the field IDs for fonts.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_font_IDs(JNIEnv* env)
{
  jclass cls;

  /* Find the nativeFont field ID in GdkFontPeer. */
  cls = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GdkFontPeer");
  fontFID = (*env)->GetFieldID(env, cls, "nativeFont",
                               "Lgnu/classpath/Pointer;");
}

/**
 * Sets the native font in the nativeFont field in GdkFontPeer.
 *
 * @param env the JNI environment
 * @param font_peer the font peer object
 * @param font the actual native font reference
 */
void gtkpeer_set_font(JNIEnv* env, jobject font_peer, void* font)
{
  jobject obj;

  /* Fetch the nativeFont field object. */
  obj = (*env)->GetObjectField(env, font_peer, fontFID);
  if (obj == NULL)
    {
      /* Create if necessary. */
#if SIZEOF_VOID_P == 8
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jlong) font);
#else
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jint) font);
#endif
      (*env)->SetObjectField(env, font_peer, fontFID, obj);
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, obj, pointerDataFID, (jlong) font);
#else
      (*env)->SetIntField(env, obj, pointerDataFID, (jint) font);
#endif
    }
}

/**
 * Fetches the native font reference from the GdkFontPeer object.
 *
 * @param env the JNI environment
 * @param font_peer the font peer object
 *
 * @return the native font structure
 */
void* gtkpeer_get_font(JNIEnv* env, jobject font_peer)
{
  jobject obj;
  void *font;

  /* Fetch the nativeFont field from the peer object. */
  obj = (*env)->GetObjectField(env, font_peer, fontFID);

  /* Fetch actual font pointer. */
#if SIZEOF_VOID_P == 8
  font = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  font = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  return font;
}

/**
 * Initializes the field IDs for pixbuf decoder.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_pixbuf_IDs(JNIEnv* env)
{
  jclass cls;

  /* Find the nativeFont field ID in GdkFontPeer. */
  cls = (*env)->FindClass(env, "gnu/java/awt/peer/gtk/GdkPixbufDecoder");
  pixbufLoaderFID = (*env)->GetFieldID(env, cls, "nativeDecoder",
                                       "Lgnu/classpath/Pointer;");
}

/**
 * Sets the native font in the nativeFont field in GdkFontPeer.
 *
 * @param env the JNI environment
 * @param pixbuf_dec the pixbuf decoder object
 * @param pixbuf_loader the native pixbuf loader
 */
void gtkpeer_set_pixbuf_loader(JNIEnv* env, jobject pixbuf_dec,
                               void* pixbuf_loader)
{
  jobject obj;

  /* Fetch the nativeDecoder field object. */
  obj = (*env)->GetObjectField(env, pixbuf_dec, pixbufLoaderFID);
  if (obj == NULL)
    {
      /* Create if necessary. */
#if SIZEOF_VOID_P == 8
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jlong) pixbuf_loader);
#else
      obj = (*env)->NewObject(env, pointerClass, pointerConstructorMID,
                              (jint) pixbuf_loader);
#endif
      (*env)->SetObjectField(env, pixbuf_dec, pixbufLoaderFID, obj);
    }
  else
    {
#if SIZEOF_VOID_P == 8
      (*env)->SetLongField(env, obj, pointerDataFID, (jlong) pixbuf_loader);
#else
      (*env)->SetIntField(env, obj, pointerDataFID, (jint) pixbuf_loader);
#endif
    }
}

/**
 * Fetches the native pixbuf loader reference from the GdkPixbufDecoder object.
 *
 * @param env the JNI environment
 * @param pixbuf_dec the pixbuf decoder object
 *
 * @return the native pixbuf loader
 */
void* gtkpeer_get_pixbuf_loader(JNIEnv* env, jobject pixbuf_dec)
{
  jobject obj;
  void *loader;

  /* Fetch the nativeFont field from the peer object. */
  obj = (*env)->GetObjectField(env, pixbuf_dec, pixbufLoaderFID);

  /* Fetch actual font pointer. */
#if SIZEOF_VOID_P == 8
  loader = (void*) (*env)->GetLongField(env, obj, pointerDataFID);
#else
  loader = (void*) (*env)->GetIntField(env, obj, pointerDataFID);
#endif
  return loader;
}
