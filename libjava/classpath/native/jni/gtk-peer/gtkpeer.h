/* gtkpeer.h -- Some global variables and #defines
   Copyright (C) 1998, 1999, 2004, 2005, 2006 Free Software Foundation, Inc.

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


#include <cairo.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <config.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <jni.h>

#ifndef __GTKPEER_H__
#define __GTKPEER_H__

#ifndef __GNUC__
#define __attribute__(x) /* nothing */
#endif

/**
 * Initializes the IDs of the Pointer* classes.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_pointer_IDs(JNIEnv* env);

/**
 * Initializes the field IDs for the widget reference.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_widget_IDs(JNIEnv *env);

/**
 * Stores the GTK widget reference in the GtkComponentPeer object.
 *
 * @param env the JNI environment
 * @param peer the actual peer object
 * @param widget the widget reference to store
 */
void gtkpeer_set_widget(JNIEnv *env, jobject peer, void *widget);

/**
 * Retrieves the GTK widget reference from a GtkComponentPeer object.
 *
 * @param env the JNI environment
 * @param peer the actual peer object
 *
 * @return the widget reference
 */
void* gtkpeer_get_widget(JNIEnv *env, jobject peer);

/**
 * Stores the global JNI reference of a peer inside the peer.
 *
 * @param env the JNI environment
 * @param peer the peer object
 */
void gtkpeer_set_global_ref(JNIEnv *env, jobject peer);

/**
 * Retrieves the global reference from a peer.
 *
 * @param env the JNI environment
 * @param peer the peer object
 *
 * @return the global reference
 */
void* gtkpeer_get_global_ref(JNIEnv *env, jobject peer);

/**
 * Deletes the global reference of a peer. This is necessary in order to
 * allow the peer to be garbage collected.
 *
 * @param env the JNI environment
 * @param peer the peer object.
 */
void gtkpeer_del_global_ref(JNIEnv* env, jobject peer);


/**
 * Initializes the fieldIDs for the display and screen fields.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_display_IDs(JNIEnv* env);

/**
 * Sets the native display pointer in the GdkGraphicsEnvironment object.
 *
 * @param env the JNI environment
 * @param graphicsenv the GdkGraphicsEnvironment object
 * @param display the native display pointer
 */
void gtkpeer_set_display(JNIEnv* env, jobject graphicsenv, void* display);

/**
 * Fetches the native display pointer from the GdkGraphicsEnvironment object.
 *
 * @param env the JNI environment
 * @param graphicsenv the GdkGraphicsEnvironment object
 *
 * @return the native display pointer
 */
void* gtkpeer_get_display(JNIEnv* env, jobject graphicsenv);

/**
 * Initializes the fieldIDs for the screen field.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_screen_IDs(JNIEnv* env);

/**
 * Sets the native screen in the GdkScreenGraphicsDevice object.
 *
 * @param env the JNI environment
 * @param screen_graphics_device the GdkScreenGraphicsDevice object
 * @param ptr the native screen pointer
 */
void gtkpeer_set_screen(JNIEnv* env, jobject screen_graphics_device,
                        void* ptr);

/**
 * Fetches the native screen pointer from the GdkScreenGraphicsDevice object.
 *
 * @param env the JNI environment
 * @param screen_graphics_device the GdkScreenGraphicsDevice object
 *
 * @return the native screen pointer
 */
void* gtkpeer_get_screen(JNIEnv* env, jobject screen_graphics_device);

/**
 * Initializes the field IDs for fonts.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_font_IDs(JNIEnv* env);

/**
 * Sets the native font in the nativeFont field in GdkFontPeer.
 *
 * @param env the JNI environment
 * @param font_peer the font peer object
 * @param font the actual native font reference
 */
void gtkpeer_set_font(JNIEnv* env, jobject font_peer, void* font);

/**
 * Fetches the native font reference from the GdkFontPeer object.
 *
 * @param env the JNI environment
 * @param font_peer the font peer object
 *
 * @return the native font structure
 */
void* gtkpeer_get_font(JNIEnv* env, jobject font_peer);

/**
 * Initializes the field IDs for pixbuf decoder.
 *
 * @param env the JNI environment
 */
void gtkpeer_init_pixbuf_IDs(JNIEnv* env);

/**
 * Sets the native font in the nativeFont field in GdkFontPeer.
 *
 * @param env the JNI environment
 * @param pixbuf_dec the pixbuf decoder object
 * @param pixbuf_loader the native pixbuf loader
 */
void gtkpeer_set_pixbuf_loader(JNIEnv* env, jobject pixbuf_dec,
                               void* pixbuf_loader);

/**
 * Fetches the native pixbuf loader reference from the GdkPixbufDecoder object.
 *
 * @param env the JNI environment
 * @param pixbuf_dec the pixbuf decoder object
 *
 * @return the native pixbuf loader
 */
void* gtkpeer_get_pixbuf_loader(JNIEnv* env, jobject pixbuf_dec);


#define SWAPU32(w)							\
  (((w) << 24) | (((w) & 0xff00) << 8) | (((w) >> 8) & 0xff00) | ((w) >> 24))

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

/* Shared global clipboard and selection for GtkClipboard and GtkSelection. */
extern GtkClipboard *cp_gtk_clipboard;
extern GtkClipboard *cp_gtk_selection;

extern jobject cp_gtk_clipboard_instance;
extern jobject cp_gtk_selection_instance;

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

/* Component Graphics helpers */
void cp_gtk_grab_current_drawable(GtkWidget *widget, GdkDrawable **draw,
				  GdkWindow **win);

/* JNI initialization functions */
void cp_gtk_button_init_jni (JNIEnv*);
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
void cp_gtk_component_connect_expose_signals (GObject *ptr, jobject gref);
void cp_gtk_component_connect_focus_signals (GObject *ptr, jobject gref);
void cp_gtk_component_connect_mouse_signals (GObject *ptr, jobject gref);
void cp_gtk_component_connect_signals (GObject *ptr, jobject gref);
void cp_gtk_textcomponent_connect_signals (GObject *ptr, jobject gref);

/* Debugging */
void cp_gtk_print_current_thread (void);

GdkPixmap *cp_gtk_get_pixmap( JNIEnv *env, jobject obj);

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
