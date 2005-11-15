/* gtkpeer.h -- Some global variables and #defines
   Copyright (C) 1998, 1999, 2004, 2005 Free Software Foundation, Inc.

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


#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <config.h>
#include "native_state.h"

#include <jni.h>

#ifndef __GTKPEER_H__
#define __GTKPEER_H__

#ifndef __GNUC__
#define __attribute__(x) /* nothing */
#endif

extern struct state_table *cp_gtk_native_state_table;
extern struct state_table *cp_gtk_native_global_ref_table;

#define NSA_INIT(env, clazz) \
   do {cp_gtk_native_state_table = cp_gtk_init_state_table (env, clazz); \
   cp_gtk_native_global_ref_table = cp_gtk_init_state_table (env, clazz);} while (0)

#define NSA_GET_PTR(env, obj) \
  cp_gtk_get_state (env, obj, cp_gtk_native_state_table)

#define NSA_SET_PTR(env, obj, ptr) \
  cp_gtk_set_state (env, obj, cp_gtk_native_state_table, (void *)ptr)

#define NSA_DEL_PTR(env, obj) \
  cp_gtk_remove_state_slot (env, obj, cp_gtk_native_state_table)

#define NSA_GET_GLOBAL_REF(env, obj) \
  cp_gtk_get_state (env, obj, cp_gtk_native_global_ref_table)

#define NSA_SET_GLOBAL_REF(env, obj) \
  do {jobject *globRefPtr; \
    globRefPtr = (jobject *) malloc (sizeof (jobject)); \
    *globRefPtr = (*env)->NewGlobalRef (env, obj); \
    cp_gtk_set_state (env, obj, cp_gtk_native_global_ref_table, (void *)globRefPtr);} while (0)

#define NSA_DEL_GLOBAL_REF(env, obj) \
  do {jobject *globRefPtr = cp_gtk_get_state (env, obj, cp_gtk_native_global_ref_table); \
    cp_gtk_remove_state_slot (env, obj, cp_gtk_native_global_ref_table); \
    (*env)->DeleteGlobalRef (env, *globRefPtr); \
    free (globRefPtr);} while (0)

extern struct state_table *cp_gtk_native_graphics_state_table;

#define NSA_G_INIT(env, clazz) \
  cp_gtk_native_graphics_state_table = cp_gtk_init_state_table (env, clazz)

#define NSA_GET_G_PTR(env, obj) \
  cp_gtk_get_state (env, obj, cp_gtk_native_graphics_state_table)

#define NSA_SET_G_PTR(env, obj, ptr) \
  cp_gtk_set_state (env, obj, cp_gtk_native_graphics_state_table, (void *)ptr)

#define NSA_DEL_G_PTR(env, obj) \
  cp_gtk_remove_state_slot (env, obj, cp_gtk_native_graphics_state_table)

#define SWAPU32(w)							\
  (((w) << 24) | (((w) & 0xff00) << 8) | (((w) >> 8) & 0xff00) | ((w) >> 24))

struct graphics
{
  GdkDrawable *drawable;
  GdkGC *gc;
  GdkColormap *cm;
  PangoFontDescription *pango_font;
  PangoContext *pango_context;
  PangoLayout *pango_layout;
  jint x_offset, y_offset;
};

/* New-style event masks. */
#define AWT_BUTTON1_DOWN_MASK (1 << 10)
#define AWT_BUTTON2_DOWN_MASK (1 << 11)
#define AWT_BUTTON3_DOWN_MASK (1 << 12)

#define AWT_SHIFT_DOWN_MASK   (1 << 6)
#define AWT_CTRL_DOWN_MASK    (1 << 7)
#define AWT_META_DOWN_MASK    (1 << 8)
#define AWT_ALT_DOWN_MASK     (1 << 9)

/* Old-style event masks. */
#define AWT_BUTTON1_MASK (1 << 4)
#define AWT_BUTTON2_MASK (1 << 3)
#define AWT_BUTTON3_MASK (1 << 2)

#define AWT_SHIFT_MASK   (1 << 0)
#define AWT_CTRL_MASK    (1 << 1)
#define AWT_META_MASK    (1 << 2)
#define AWT_ALT_MASK     (1 << 3)

#define AWT_ITEM_SELECTED 1
#define AWT_ITEM_DESELECTED 2
     
#define AWT_KEY_TYPED    400
#define AWT_KEY_PRESSED  401
#define AWT_KEY_RELEASED 402

#define AWT_KEY_LOCATION_UNKNOWN 0
#define AWT_KEY_LOCATION_STANDARD 1
#define AWT_KEY_LOCATION_LEFT 2
#define AWT_KEY_LOCATION_RIGHT 3
#define AWT_KEY_LOCATION_NUMPAD 4

#define AWT_STYLE_PLAIN  0
#define AWT_STYLE_BOLD   1
#define AWT_STYLE_ITALIC 2

/* Used in GtkComponentPeer and GtkWindowPeer */
#define VK_NUMPAD0 96
#define VK_NUMPAD1 97
#define VK_NUMPAD2 98
#define VK_NUMPAD3 99
#define VK_NUMPAD4 100
#define VK_NUMPAD5 101
#define VK_NUMPAD6 102
#define VK_NUMPAD7 103
#define VK_NUMPAD8 104
#define VK_NUMPAD9 105
#define VK_DECIMAL 110

JNIEnv *cp_gtk_gdk_env(void);

/* Global variables */
extern double cp_gtk_dpi_conversion_factor;
extern GtkWindowGroup *cp_gtk_global_window_group;

/* Shared global clipboard for GtkClipboard and GtkSelection. */
extern GtkClipboard *cp_gtk_clipboard;

/* Standard target (strings) for GtkClipboard and GtkSelection. */
extern jstring cp_gtk_stringTarget;
extern jstring cp_gtk_imageTarget;
extern jstring cp_gtk_filesTarget;

/* Union used for type punning. */
union widget_union
{
  void **void_widget;
  GtkWidget **widget;
};

/* Constant conversion helpers */
guint cp_gtk_awt_keycode_to_keysym (jint keyCode, jint keyLocation);
jint cp_gtk_state_to_awt_mods (guint state);

/* Image helpers */
GdkPixbuf *cp_gtk_image_get_pixbuf (JNIEnv *env, jobject obj);
GdkPixmap *cp_gtk_image_get_pixmap (JNIEnv *env, jobject obj);
jboolean cp_gtk_image_is_offscreen (JNIEnv *env, jobject obj);

/* JNI initialization functions */
#if GTK_CAIRO
void cp_gtk_graphics2d_init_jni (void);
#endif
void cp_gtk_graphics_init_jni (void);
void cp_gtk_button_init_jni (void);
void cp_gtk_checkbox_init_jni (void);
void cp_gtk_choice_init_jni (void);
void cp_gtk_component_init_jni (void);
void cp_gtk_filedialog_init_jni (void);
void cp_gtk_list_init_jni (void);
void cp_gtk_menuitem_init_jni (void);
void cp_gtk_scrollbar_init_jni (void);
void cp_gtk_textcomponent_init_jni (void);
void cp_gtk_window_init_jni (void);

/* Signal connection convience functions */
void cp_gtk_component_connect_expose_signals (GObject *ptr, jobject *gref);
void cp_gtk_component_connect_focus_signals (GObject *ptr, jobject *gref);
void cp_gtk_component_connect_mouse_signals (GObject *ptr, jobject *gref);
void cp_gtk_component_connect_signals (GObject *ptr, jobject *gref);
void cp_gtk_textcomponent_connect_signals (GObject *ptr, jobject *gref);

/* Debugging */
void cp_gtk_print_current_thread (void);

#define SYNCHRONIZE_GDK 0

#define DEBUG_LOCKING 0

#if DEBUG_LOCKING
#define gdk_threads_enter()                          \
{                                                    \
  g_print ("locking: %s, %d\n", __FILE__, __LINE__); \
  cp_gtk_print_current_thread ();                    \
  gdk_threads_enter ();                              \
  g_print ("locked: %s, %d\n", __FILE__, __LINE__); \
  cp_gtk_print_current_thread ();                    \
}
#define gdk_threads_leave()                            \
{                                                      \
  g_print ("unlocking: %s, %d\n", __FILE__, __LINE__); \
  cp_gtk_print_current_thread ();                      \
  gdk_threads_leave ();                                \
  g_print ("unlocked: %s, %d\n", __FILE__, __LINE__);  \
  cp_gtk_print_current_thread ();                      \
}
#endif

#endif /* __GTKPEER_H */
