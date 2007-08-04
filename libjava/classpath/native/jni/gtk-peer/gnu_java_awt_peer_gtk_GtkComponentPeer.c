/* gtkcomponentpeer.c -- Native implementation of GtkComponentPeer
   Copyright (C) 1998, 1999, 2002, 2004, 2006 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"

#include <gtk/gtkprivate.h>

#define AWT_DEFAULT_CURSOR 0
#define AWT_CROSSHAIR_CURSOR 1
#define AWT_TEXT_CURSOR 2
#define AWT_WAIT_CURSOR 3
#define AWT_SW_RESIZE_CURSOR 4
#define AWT_SE_RESIZE_CURSOR 5
#define AWT_NW_RESIZE_CURSOR 6
#define AWT_NE_RESIZE_CURSOR 7
#define AWT_N_RESIZE_CURSOR 8
#define AWT_S_RESIZE_CURSOR 9
#define AWT_W_RESIZE_CURSOR 10
#define AWT_E_RESIZE_CURSOR 11
#define AWT_HAND_CURSOR 12
#define AWT_MOVE_CURSOR 13

/* FIXME: use gtk-double-click-time, gtk-double-click-distance */
#define MULTI_CLICK_TIME   250
/* as opposed to a MULTI_PASS_TIME :) */

#define AWT_MOUSE_CLICKED  500
#define AWT_MOUSE_PRESSED  501
#define AWT_MOUSE_RELEASED 502
#define AWT_MOUSE_MOVED    503
#define AWT_MOUSE_ENTERED  504
#define AWT_MOUSE_EXITED   505
#define AWT_MOUSE_DRAGGED  506
#define AWT_MOUSE_WHEEL    507

#define AWT_WHEEL_UNIT_SCROLL 0

#define AWT_FOCUS_GAINED 1004
#define AWT_FOCUS_LOST 1005

static GtkWidget *find_fg_color_widget (GtkWidget *widget);
static GtkWidget *find_bg_color_widget (GtkWidget *widget);
static GtkWidget *get_widget (GtkWidget *widget);

static jmethodID postMouseEventID;
static jmethodID postMouseWheelEventID;
static jmethodID postExposeEventID;
static jmethodID postFocusEventID;

void
cp_gtk_component_init_jni (void)
 {
  jclass gtkcomponentpeer;

  gtkcomponentpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                     "gnu/java/awt/peer/gtk/GtkComponentPeer");

  postMouseEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkcomponentpeer,
                                               "postMouseEvent", "(IJIIIIZ)V");

  postMouseWheelEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
						        gtkcomponentpeer,
						        "postMouseWheelEvent",
							"(IJIIIIZIII)V");

  postExposeEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkcomponentpeer,
                                                 "postExposeEvent", "(IIII)V");

  postFocusEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkcomponentpeer,
                                                "postFocusEvent", "(IZ)V");
}

static gboolean component_button_press_cb (GtkWidget *widget,
                                           GdkEventButton *event,
                                           jobject peer);
static gboolean component_button_release_cb (GtkWidget *widget,
                                             GdkEventButton *event,
                                             jobject peer);
static gboolean component_motion_notify_cb (GtkWidget *widget,
                                            GdkEventMotion *event,
                                            jobject peer);
static gboolean component_scroll_cb (GtkWidget *widget,
				     GdkEventScroll *event,
				     jobject peer);
static gboolean component_enter_notify_cb (GtkWidget *widget,
                                           GdkEventCrossing *event,
                                           jobject peer);
static gboolean component_leave_notify_cb (GtkWidget *widget,
                                           GdkEventCrossing *event,
                                           jobject peer);
static gboolean component_expose_cb (GtkWidget *widget,
                                     GdkEventExpose *event,
                                     jobject peer);
static gboolean component_focus_in_cb (GtkWidget *widget,
                                       GdkEventFocus *event,
                                       jobject peer);
static gboolean component_focus_out_cb (GtkWidget *widget,
                                        GdkEventFocus *event,
                                        jobject peer);

static jint
button_to_awt_mods (int button)
{
  switch (button)
    {
    case 1:
      return AWT_BUTTON1_DOWN_MASK | AWT_BUTTON1_MASK;
    case 2:
      return AWT_BUTTON2_DOWN_MASK | AWT_BUTTON2_MASK;
    case 3:
      return AWT_BUTTON3_DOWN_MASK | AWT_BUTTON3_MASK;
    }

  return 0;
}

jint
cp_gtk_state_to_awt_mods (guint state)
{
  jint result = 0;

  if (state & GDK_SHIFT_MASK)
    result |= (AWT_SHIFT_DOWN_MASK | AWT_SHIFT_MASK);
  if (state & GDK_CONTROL_MASK)
    result |= (AWT_CTRL_DOWN_MASK | AWT_CTRL_MASK);
  if (state & GDK_MOD1_MASK)
    result |= (AWT_ALT_DOWN_MASK | AWT_ALT_MASK);

  return result;
}

static jint
state_to_awt_mods_with_button_states (guint state)
{
  jint result = 0;

  if (state & GDK_SHIFT_MASK)
    result |= AWT_SHIFT_DOWN_MASK | AWT_SHIFT_MASK;
  if (state & GDK_CONTROL_MASK)
    result |= AWT_CTRL_DOWN_MASK | AWT_CTRL_MASK;
  if (state & GDK_MOD1_MASK)
    result |= AWT_ALT_DOWN_MASK | AWT_ALT_MASK;
  if (state & GDK_BUTTON1_MASK)
    result |= AWT_BUTTON1_DOWN_MASK | AWT_BUTTON1_MASK;
  if (state & GDK_BUTTON2_MASK)
    result |= AWT_BUTTON2_DOWN_MASK;
  if (state & GDK_BUTTON3_MASK)
    result |= AWT_BUTTON3_DOWN_MASK;

  return result;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetCursor 
  (JNIEnv *env, jobject obj, jint type, jobject image, jint x, jint y) 
{
  gdk_threads_enter ();

  Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetCursorUnlocked
    (env, obj, type, image, x, y);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetCursorUnlocked
  (JNIEnv *env, jobject obj, jint type, jobject image, jint x, jint y) 
{
  void *ptr;
  GtkWidget *widget;
  GdkWindow *win;
  GdkCursorType gdk_cursor_type;
  GdkCursor *gdk_cursor;

  ptr = gtkpeer_get_widget (env, obj);

  switch (type)
    {
    case AWT_CROSSHAIR_CURSOR:
      gdk_cursor_type = GDK_CROSSHAIR;
      break;
    case AWT_TEXT_CURSOR:
      gdk_cursor_type = GDK_XTERM;
      break;
    case AWT_WAIT_CURSOR:
      gdk_cursor_type = GDK_WATCH;
      break;
    case AWT_SW_RESIZE_CURSOR:
      gdk_cursor_type = GDK_BOTTOM_LEFT_CORNER;
      break;
    case AWT_SE_RESIZE_CURSOR:
      gdk_cursor_type = GDK_BOTTOM_RIGHT_CORNER;
      break;
    case AWT_NW_RESIZE_CURSOR:
      gdk_cursor_type = GDK_TOP_LEFT_CORNER;
      break;
    case AWT_NE_RESIZE_CURSOR:
      gdk_cursor_type = GDK_TOP_RIGHT_CORNER;
      break;
    case AWT_N_RESIZE_CURSOR:
      gdk_cursor_type = GDK_TOP_SIDE;
      break;
    case AWT_S_RESIZE_CURSOR:
      gdk_cursor_type = GDK_BOTTOM_SIDE;
      break;
    case AWT_W_RESIZE_CURSOR:
      gdk_cursor_type = GDK_LEFT_SIDE;
      break;
    case AWT_E_RESIZE_CURSOR:
      gdk_cursor_type = GDK_RIGHT_SIDE;
      break;
    case AWT_HAND_CURSOR:
      gdk_cursor_type = GDK_HAND2;
      break;
    case AWT_MOVE_CURSOR:
      gdk_cursor_type = GDK_FLEUR;
      break;
    default:
      gdk_cursor_type = GDK_LEFT_PTR;
    }
      
  widget = get_widget(GTK_WIDGET(ptr));
  
  win = widget->window;
  if ((widget->window) == NULL)
    win = GTK_WIDGET(ptr)->window;
    
  if (image == NULL)
    gdk_cursor = gdk_cursor_new (gdk_cursor_type);
  else
    gdk_cursor
      = gdk_cursor_new_from_pixbuf (gdk_drawable_get_display (win),
				    cp_gtk_image_get_pixbuf (env, image),
				    x, y);

  gdk_window_set_cursor (win, gdk_cursor);
  gdk_cursor_unref (gdk_cursor);

  /* Make sure the cursor is replaced on screen. */
  gdk_flush();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetParent
  (JNIEnv *env, jobject obj, jobject parent)
{
  void *ptr;
  void *parent_ptr;
  GtkWidget *widget;
  GtkWidget *parent_widget;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  parent_ptr = gtkpeer_get_widget (env, parent);
  
  widget = GTK_WIDGET (ptr);
  parent_widget = get_widget(GTK_WIDGET (parent_ptr));

  if (widget->parent == NULL)
    {
      if (GTK_IS_WINDOW (parent_widget))
	{
	  GList *children = gtk_container_get_children
	    (GTK_CONTAINER (parent_widget));

          if (GTK_IS_MENU_BAR (children->data))
            gtk_fixed_put (GTK_FIXED (children->next->data), widget, 0, 0);
          else
            gtk_fixed_put (GTK_FIXED (children->data), widget, 0, 0);
        }
      else
        if (GTK_IS_SCROLLED_WINDOW (parent_widget))
          {
            gtk_scrolled_window_add_with_viewport 
              (GTK_SCROLLED_WINDOW (parent_widget), widget);
            gtk_viewport_set_shadow_type (GTK_VIEWPORT (widget->parent), 
                                          GTK_SHADOW_NONE);

          }
        else
          {
            if (widget->parent == NULL)
              gtk_fixed_put (GTK_FIXED (parent_widget), widget, 0, 0);
          }
    }

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetSensitive
  (JNIEnv *env, jobject obj, jboolean sensitive)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  gtk_widget_set_sensitive (get_widget(GTK_WIDGET (ptr)), sensitive);

  gdk_threads_leave ();
}

JNIEXPORT jboolean JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetHasFocus
(JNIEnv *env, jobject obj)
{
  void *ptr;
  jboolean retval;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  
  retval = GTK_WIDGET_HAS_FOCUS((GTK_WIDGET (ptr)));

  gdk_threads_leave ();

  return retval;
}

JNIEXPORT jboolean JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetCanFocus
(JNIEnv *env, jobject obj)
{
  void *ptr;
  jboolean retval;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  
  retval = GTK_WIDGET_CAN_FOCUS((GTK_WIDGET (ptr)));

  gdk_threads_leave ();

  return retval;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetRequestFocus
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  
  gtk_widget_grab_focus (get_widget(GTK_WIDGET (ptr)));

  gdk_threads_leave ();
}

/*
 * Translate a Java KeyEvent object into a GdkEventKey event, then
 * pass it to the GTK main loop for processing.
 */
JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetDispatchKeyEvent
  (JNIEnv *env, jobject obj, jint id, jlong when, jint mods,
   jint keyCode, jint keyLocation)
{
  void *ptr;
  GdkEvent *event = NULL;
  GdkKeymapKey *keymap_keys = NULL;
  gint n_keys = 0;
  guint lookup_keyval = 0;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  if (id == AWT_KEY_PRESSED)
    event = gdk_event_new (GDK_KEY_PRESS);
  else if (id == AWT_KEY_RELEASED)
    event = gdk_event_new (GDK_KEY_RELEASE);
  else
    {
      gdk_threads_leave ();
      /* Don't send AWT KEY_TYPED events to GTK. */
      return;
    }

  if (GTK_IS_BUTTON (ptr))
    event->key.window = GTK_BUTTON (get_widget(GTK_WIDGET (ptr)))->event_window;
  else if (GTK_IS_SCROLLED_WINDOW (get_widget(GTK_WIDGET (ptr))))
    event->key.window = GTK_WIDGET (GTK_SCROLLED_WINDOW (get_widget(GTK_WIDGET (ptr)))->container.child)->window;
  else
    event->key.window = get_widget(GTK_WIDGET (ptr))->window;

  event->key.send_event = 0;
  event->key.time = (guint32) when;

  if (mods & AWT_SHIFT_DOWN_MASK)
    event->key.state |= GDK_SHIFT_MASK;
  if (mods & AWT_CTRL_DOWN_MASK)
    event->key.state |= GDK_CONTROL_MASK;
  if (mods & AWT_ALT_DOWN_MASK)
    event->key.state |= GDK_MOD1_MASK;

  /* This hack is needed because the AWT has no notion of num lock.
     It infers numlock state from the only Java virtual keys that are
     affected by it. */
  if (keyCode == VK_NUMPAD9
      || keyCode == VK_NUMPAD8
      || keyCode == VK_NUMPAD7
      || keyCode == VK_NUMPAD6
      || keyCode == VK_NUMPAD5
      || keyCode == VK_NUMPAD4
      || keyCode == VK_NUMPAD3
      || keyCode == VK_NUMPAD2
      || keyCode == VK_NUMPAD1
      || keyCode == VK_NUMPAD0
      || keyCode == VK_DECIMAL)
    event->key.state |= GDK_MOD2_MASK;

  /* These values don't need to be filled in since GTK doesn't use
     them. */
  event->key.length = 0;
  event->key.string = NULL;

  lookup_keyval = cp_gtk_awt_keycode_to_keysym (keyCode, keyLocation);

  if (!gdk_keymap_get_entries_for_keyval (gdk_keymap_get_default (),
                                          lookup_keyval,
                                          &keymap_keys,
                                          &n_keys))
    {
      /* No matching keymap entry was found. */
      g_printerr ("No matching keymap entries were found\n");
      gdk_threads_leave ();
      return;
    }

  /* Note: if n_keys > 1 then there are multiple hardware keycodes
     that translate to lookup_keyval.  We arbitrarily choose the first
     hardware keycode from the list returned by
     gdk_keymap_get_entries_for_keyval. */

  event->key.hardware_keycode = keymap_keys[0].keycode;
  event->key.group =  keymap_keys[0].group;

  g_free (keymap_keys);

  if (!gdk_keymap_translate_keyboard_state (gdk_keymap_get_default (),
                                            event->key.hardware_keycode,
                                            event->key.state,
                                            event->key.group,
                                            &event->key.keyval,
                                            NULL, NULL, NULL))
    {
      /* No matching keyval was found. */
      g_printerr ("No matching keyval was found\n");
      gdk_threads_leave ();
      return;
    }

  /*  keyevent = (GdkEventKey *) event; */
  /*  g_printerr ("generated event: sent: %d  time: %d  state: %d  keyval: %d  length: %d  string: %s  hardware_keycode: %d  group: %d\n", keyevent->send_event, keyevent->time, keyevent->state, keyevent->keyval, keyevent->length, keyevent->string, keyevent->hardware_keycode, keyevent->group); */

  /* We already received the original key event on the window itself,
     so we don't want to resend it. */
  if (!GTK_IS_WINDOW (ptr))
    {
      if (GTK_IS_SCROLLED_WINDOW (get_widget(GTK_WIDGET (ptr))))
        gtk_widget_event (GTK_WIDGET (GTK_SCROLLED_WINDOW (get_widget(GTK_WIDGET (ptr)))->container.child), event);
      else
        gtk_widget_event (get_widget(GTK_WIDGET (ptr)), event);
    }

  gdk_threads_leave ();
}

/*
 * Find the origin of a widget's window.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWindowGetLocationOnScreen
  (JNIEnv * env, jobject obj, jintArray jpoint)
{
  void *ptr;
  jint *point;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  point = (*env)->GetIntArrayElements (env, jpoint, 0);

  gdk_window_get_root_origin (get_widget(GTK_WIDGET (ptr))->window, point, point+1);

  (*env)->ReleaseIntArrayElements(env, jpoint, point, 0);

  gdk_threads_leave ();
}

/*
 * Find the origin of a widget
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetGetLocationOnScreen
  (JNIEnv * env, jobject obj, jintArray jpoint)
{
  void *ptr;
  jint *point;
  GtkWidget *widget;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  point = (*env)->GetIntArrayElements (env, jpoint, 0);

  widget = get_widget(GTK_WIDGET (ptr));
  while(gtk_widget_get_parent(widget) != NULL)
    widget = gtk_widget_get_parent(widget);
  gdk_window_get_position (GTK_WIDGET(widget)->window, point, point+1);

  *point += GTK_WIDGET(ptr)->allocation.x;
  *(point+1) += GTK_WIDGET(ptr)->allocation.y;

  (*env)->ReleaseIntArrayElements(env, jpoint, point, 0);

  gdk_threads_leave ();
}

/*
 * Find this widget's current size.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetGetDimensions
  (JNIEnv *env, jobject obj, jintArray jdims)
{
  void *ptr;
  jint *dims;
  GtkRequisition requisition;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  dims = (*env)->GetIntArrayElements (env, jdims, 0);  
  dims[0] = dims[1] = 0;

  gtk_widget_size_request (get_widget(GTK_WIDGET (ptr)), &requisition);

  dims[0] = requisition.width;
  dims[1] = requisition.height;

  (*env)->ReleaseIntArrayElements (env, jdims, dims, 0);

  gdk_threads_leave ();
}

/*
 * Find this widget's preferred size.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetGetPreferredDimensions
  (JNIEnv *env, jobject obj, jintArray jdims)
{
  void *ptr;
  jint *dims;
  GtkRequisition current_req;
  GtkRequisition natural_req;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  dims = (*env)->GetIntArrayElements (env, jdims, 0);  
  dims[0] = dims[1] = 0;

  /* Widgets that extend GtkWindow such as GtkFileChooserDialog may have
     a default size.  These values seem more useful then the natural
     requisition values, particularly for GtkFileChooserDialog. */
  if (GTK_IS_WINDOW (get_widget(GTK_WIDGET (ptr))))
    {
      gint width, height;
      gtk_window_get_default_size (GTK_WINDOW (get_widget(GTK_WIDGET (ptr))), &width, &height);

      dims[0] = width;
      dims[1] = height;
    }
  else
    {
      /* Save the widget's current size request. */
      gtk_widget_size_request (get_widget(GTK_WIDGET (ptr)), &current_req);

      /* Get the widget's "natural" size request. */
      gtk_widget_set_size_request (get_widget(GTK_WIDGET (ptr)), -1, -1);
      gtk_widget_size_request (get_widget(GTK_WIDGET (ptr)), &natural_req);

      /* Reset the widget's size request. */
      gtk_widget_set_size_request (get_widget(GTK_WIDGET (ptr)),
			           current_req.width, current_req.height);

      dims[0] = natural_req.width;
      dims[1] = natural_req.height;
    }

  (*env)->ReleaseIntArrayElements (env, jdims, dims, 0);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_setNativeBounds
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  GtkWidget *widget;
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  widget = GTK_WIDGET (ptr);

  /* We assume that -1 is a width or height and not a request for the
     widget's natural size. */
  width = width < 0 ? 0 : width;
  height = height < 0 ? 0 : height;

  if (!(width == 0 && height == 0))
    {
      gtk_widget_set_size_request (widget, width, height);
      /* The GTK_IS_FIXED check here prevents gtk_fixed_move being
         called when our parent is a GtkScrolledWindow.  In that
         case though, moving the child widget is invalid since a
         ScrollPane only has one child and that child is always
         located at (0, 0) in viewport coordinates. */
      if (widget->parent != NULL && GTK_IS_FIXED (widget->parent))
        gtk_fixed_move (GTK_FIXED (widget->parent), widget, x, y);
    }

  gdk_threads_leave ();
}

JNIEXPORT jintArray JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetGetBackground
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jintArray array;
  int *rgb;
  GdkColor bg;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  bg = GTK_WIDGET (ptr)->style->bg[GTK_STATE_NORMAL];

  array = (*env)->NewIntArray (env, 3);

  rgb = (*env)->GetIntArrayElements (env, array, NULL);
  /* convert color data from 16 bit values down to 8 bit values */
  rgb[0] = bg.red   >> 8;
  rgb[1] = bg.green >> 8;
  rgb[2] = bg.blue  >> 8;
  (*env)->ReleaseIntArrayElements (env, array, rgb, 0);

  gdk_threads_leave ();

  return array;
}

JNIEXPORT jintArray JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetGetForeground
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jintArray array;
  jint *rgb;
  GdkColor fg;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  fg = get_widget(GTK_WIDGET (ptr))->style->fg[GTK_STATE_NORMAL];

  array = (*env)->NewIntArray (env, 3);

  rgb = (*env)->GetIntArrayElements (env, array, NULL);
  /* convert color data from 16 bit values down to 8 bit values */
  rgb[0] = fg.red   >> 8;
  rgb[1] = fg.green >> 8;
  rgb[2] = fg.blue  >> 8;
  (*env)->ReleaseIntArrayElements (env, array, rgb, 0);

  gdk_threads_leave ();

  return array;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetBackground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor normal_color;
  GdkColor active_color;
  GtkWidget *widget;
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  normal_color.red = (red / 255.0) * 65535;
  normal_color.green = (green / 255.0) * 65535;
  normal_color.blue = (blue / 255.0) * 65535;

  /* This calculation only approximates the active colors produced by
     Sun's AWT. */
  active_color.red = 0.85 * (red / 255.0) * 65535;
  active_color.green = 0.85 * (green / 255.0) * 65535;
  active_color.blue = 0.85 * (blue / 255.0) * 65535;

  widget = find_bg_color_widget (GTK_WIDGET (ptr));

  gtk_widget_modify_bg (widget, GTK_STATE_NORMAL, &normal_color);
  gtk_widget_modify_bg (widget, GTK_STATE_ACTIVE, &active_color);
  gtk_widget_modify_bg (widget, GTK_STATE_PRELIGHT, &normal_color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetForeground
  (JNIEnv *env, jobject obj, jint red, jint green, jint blue)
{
  GdkColor color;
  GtkWidget *widget;
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;

  widget = find_fg_color_widget (GTK_WIDGET (ptr));

  gtk_widget_modify_fg (widget, GTK_STATE_NORMAL, &color);
  gtk_widget_modify_fg (widget, GTK_STATE_ACTIVE, &color);
  gtk_widget_modify_fg (widget, GTK_STATE_PRELIGHT, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_realize (JNIEnv *env, jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  gtk_widget_realize (GTK_WIDGET (ptr));

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_setVisibleNative
  (JNIEnv *env, jobject obj, jboolean visible)
{
  gdk_threads_enter();

  Java_gnu_java_awt_peer_gtk_GtkComponentPeer_setVisibleNativeUnlocked
    (env, obj, visible);

  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_setVisibleNativeUnlocked
  (JNIEnv *env, jobject obj, jboolean visible)
{
  void *ptr;

  ptr = gtkpeer_get_widget (env, obj);

  if (visible)
    gtk_widget_show (GTK_WIDGET (ptr));
  else
    gtk_widget_hide (GTK_WIDGET (ptr));
}

JNIEXPORT jboolean JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_isEnabled 
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jboolean ret_val;
  
  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  ret_val = GTK_WIDGET_IS_SENSITIVE (get_widget(GTK_WIDGET (ptr)));

  gdk_threads_leave ();

  return ret_val;
}

JNIEXPORT jboolean JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_modalHasGrab
  (JNIEnv *env __attribute__((unused)), jclass clazz __attribute__((unused)))
{
  GtkWidget *widget;
  jboolean retval;

  gdk_threads_enter ();

  widget = gtk_grab_get_current ();
  retval = (widget && GTK_IS_WINDOW (widget) && GTK_WINDOW (widget)->modal);

  gdk_threads_leave ();

  return retval;
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject gref;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  gref = gtkpeer_get_global_ref (env, obj);

  cp_gtk_component_connect_signals (ptr, gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_setNativeEventMask
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  gtk_widget_add_events (get_widget(GTK_WIDGET (ptr)),
                         GDK_POINTER_MOTION_MASK
			 | GDK_BUTTON_MOTION_MASK
			 | GDK_BUTTON_PRESS_MASK
			 | GDK_BUTTON_RELEASE_MASK
			 | GDK_KEY_PRESS_MASK
			 | GDK_KEY_RELEASE_MASK
			 | GDK_ENTER_NOTIFY_MASK
			 | GDK_LEAVE_NOTIFY_MASK
			 | GDK_STRUCTURE_MASK
			 | GDK_KEY_PRESS_MASK
                         | GDK_FOCUS_CHANGE_MASK);

  gdk_threads_leave ();
}

static GtkWidget *
get_widget (GtkWidget *widget)
{
  GtkWidget *w;

  if (GTK_IS_EVENT_BOX (widget))
    w = gtk_bin_get_child (GTK_BIN(widget));
  else
    w = widget;

  return w;
}

/* FIXME: these functions should be implemented by overridding the
   appropriate GtkComponentPeer methods. */
static GtkWidget *
find_fg_color_widget (GtkWidget *widget)
{
  GtkWidget *fg_color_widget;

  if (GTK_IS_EVENT_BOX (widget)
      || (GTK_IS_BUTTON (widget)
	  && !GTK_IS_COMBO_BOX (widget)))
    fg_color_widget = gtk_bin_get_child (GTK_BIN(widget));
  else
    fg_color_widget = widget;

  return fg_color_widget;
}

static GtkWidget *
find_bg_color_widget (GtkWidget *widget)
{
  GtkWidget *bg_color_widget;

  bg_color_widget = widget;

  return bg_color_widget;
}

void
cp_gtk_component_connect_expose_signals (GObject *ptr, jobject gref)
{
  g_signal_connect (G_OBJECT (ptr), "expose-event",
                    G_CALLBACK (component_expose_cb), gref);
}

void
cp_gtk_component_connect_focus_signals (GObject *ptr, jobject gref)
{
  g_signal_connect (G_OBJECT (ptr), "focus-in-event",
                    G_CALLBACK (component_focus_in_cb), gref);
 
  g_signal_connect (G_OBJECT (ptr), "focus-out-event",
                    G_CALLBACK (component_focus_out_cb), gref);
}

void
cp_gtk_component_connect_mouse_signals (GObject *ptr, jobject gref)
{
  g_signal_connect (G_OBJECT (ptr), "button-press-event",
                    G_CALLBACK (component_button_press_cb), gref);
 
  g_signal_connect (G_OBJECT (ptr), "button-release-event",
                    G_CALLBACK (component_button_release_cb), gref);
 
  g_signal_connect (G_OBJECT (ptr), "enter-notify-event",
                    G_CALLBACK (component_enter_notify_cb), gref);
 
  g_signal_connect (G_OBJECT (ptr), "leave-notify-event",
                    G_CALLBACK (component_leave_notify_cb), gref);

  g_signal_connect (G_OBJECT (ptr), "motion-notify-event",
                    G_CALLBACK (component_motion_notify_cb), gref);

  g_signal_connect (G_OBJECT (ptr), "scroll-event",
                    G_CALLBACK (component_scroll_cb), gref);
}

void
cp_gtk_component_connect_signals (GObject *ptr, jobject gref)
{
  cp_gtk_component_connect_expose_signals (ptr, gref);
  cp_gtk_component_connect_focus_signals (ptr, gref);
  cp_gtk_component_connect_mouse_signals (ptr, gref);
}

/* These variables are used to keep track of click counts.  The AWT
   allows more than a triple click to occur but GTK doesn't report
   more-than-triple clicks.  Also used for keeping track of scroll events.*/
static jint click_count = 1;
static guint32 button_click_time = 0;
static GdkWindow *button_window = NULL;
static guint button_number_direction = -1;
static int hasBeenDragged;

static gboolean
component_button_press_cb (GtkWidget *widget __attribute__((unused)),
                           GdkEventButton *event,
                           jobject peer)
{
  /* Ignore double and triple click events. */
  if (event->type == GDK_2BUTTON_PRESS
      || event->type == GDK_3BUTTON_PRESS)
    return FALSE;

  if ((event->time < (button_click_time + MULTI_CLICK_TIME))
      && (event->window == button_window)
      && (event->button == button_number_direction))
    click_count++;
  else
    click_count = 1;
      
  button_click_time = event->time;
  button_window = event->window;
  button_number_direction = event->button;

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postMouseEventID,
                                AWT_MOUSE_PRESSED, 
                                (jlong)event->time,
                                cp_gtk_state_to_awt_mods (event->state)
                                | button_to_awt_mods (event->button),
                                (jint)event->x,
                                (jint)event->y, 
                                click_count, 
                                (event->button == 3) ? JNI_TRUE :
                                JNI_FALSE);

  hasBeenDragged = FALSE;

  return FALSE;
}

static gboolean
component_button_release_cb (GtkWidget *widget __attribute__((unused)),
                             GdkEventButton *event,
                             jobject peer)
{
  int width, height;

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postMouseEventID,
                                AWT_MOUSE_RELEASED, 
                                (jlong)event->time,
                                cp_gtk_state_to_awt_mods (event->state)
                                | button_to_awt_mods (event->button),
                                (jint)event->x,
                                (jint)event->y, 
                                click_count,
                                JNI_FALSE);

  /* Generate an AWT click event only if the release occured in the
     window it was pressed in, and the mouse has not been dragged since
     the last time it was pressed. */
  gdk_drawable_get_size (event->window, &width, &height);
  if (! hasBeenDragged
      && event->x >= 0
      && event->y >= 0
      && event->x <= width 
      && event->y <= height)
    {
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                    postMouseEventID,
                                    AWT_MOUSE_CLICKED, 
                                    (jlong)event->time,
                                    cp_gtk_state_to_awt_mods (event->state)
                                    | button_to_awt_mods (event->button),
                                    (jint)event->x,
                                    (jint)event->y, 
                                    click_count,
                                    JNI_FALSE);
    }
  return FALSE;
}
 
static gboolean
component_motion_notify_cb (GtkWidget *widget __attribute__((unused)),
                            GdkEventMotion *event,
                            jobject peer)
{
  if (event->state & (GDK_BUTTON1_MASK
                      | GDK_BUTTON2_MASK
                      | GDK_BUTTON3_MASK
                      | GDK_BUTTON4_MASK
                      | GDK_BUTTON5_MASK))
    {
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                    postMouseEventID,
                                    AWT_MOUSE_DRAGGED,
                                    (jlong)event->time,
                                    state_to_awt_mods_with_button_states (event->state),
                                    (jint)event->x,
                                    (jint)event->y,
                                    0,
                                    JNI_FALSE);

      hasBeenDragged = TRUE;
    }
  else
    {
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer, postMouseEventID,
                                    AWT_MOUSE_MOVED,
                                    (jlong)event->time,
                                    cp_gtk_state_to_awt_mods (event->state),
                                    (jint)event->x,
                                    (jint)event->y,
                                    0,
                                    JNI_FALSE);
    }
  return FALSE;
}

static gboolean
component_scroll_cb (GtkWidget *widget __attribute__((unused)),
		     GdkEventScroll *event,
		     jobject peer)
{
  int rotation;
  /** Record click count for specific direction. */
  if ((event->time < (button_click_time + MULTI_CLICK_TIME))
      && (event->window == button_window)
      && (event->direction == button_number_direction))
    click_count++;
  else
    click_count = 1;
      
  button_click_time = event->time;
  button_window = event->window;
  button_number_direction = event->direction;

  if (event->direction == GDK_SCROLL_UP
      || event->direction == GDK_SCROLL_LEFT)
    rotation = -1;
  else
    rotation = 1;

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
				       postMouseWheelEventID,
				       AWT_MOUSE_WHEEL, 
				       (jlong)event->time,
				       cp_gtk_state_to_awt_mods (event->state),
				       (jint)event->x,
				       (jint)event->y, 
				       click_count, 
				       JNI_FALSE,
				       AWT_WHEEL_UNIT_SCROLL,
				       1 /* amount */,
				       rotation);
  return FALSE;
}

static gboolean
component_enter_notify_cb (GtkWidget *widget __attribute__((unused)),
                           GdkEventCrossing *event,
                           jobject peer)
{
  /* We are not interested in enter events that are due to
     grab/ungrab and not to actually crossing boundaries */
  if (event->mode == GDK_CROSSING_NORMAL)
    {
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer, postMouseEventID,
                                    AWT_MOUSE_ENTERED, 
                                    (jlong)event->time,
                                    state_to_awt_mods_with_button_states (event->state), 
                                    (jint)event->x,
                                    (jint)event->y, 
                                    0,
                                    JNI_FALSE);
    }
  return FALSE;
}

static gboolean
component_leave_notify_cb (GtkWidget *widget __attribute__((unused)),
                           GdkEventCrossing *event,
                           jobject peer)
{
  /* We are not interested in leave events that are due to
     grab/ungrab and not to actually crossing boundaries */
  if (event->mode == GDK_CROSSING_NORMAL)
    {
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                    postMouseEventID,
                                    AWT_MOUSE_EXITED, 
                                    (jlong)event->time,
                                    state_to_awt_mods_with_button_states (event->state),
                                    (jint)event->x,
                                    (jint)event->y, 
                                    0,
                                    JNI_FALSE);
    }
  return FALSE;
}

static gboolean
component_expose_cb (GtkWidget *widget __attribute__((unused)),
                     GdkEventExpose *event,
                     jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postExposeEventID,
                                (jint)event->area.x,
                                (jint)event->area.y,
                                (jint)event->area.width,
                                (jint)event->area.height);

  return FALSE;
}

static gboolean
component_focus_in_cb (GtkWidget *widget __attribute((unused)),
                       GdkEventFocus *event __attribute((unused)),
                       jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postFocusEventID,
                                AWT_FOCUS_GAINED,
                                JNI_FALSE);

  return FALSE;
}

static gboolean
component_focus_out_cb (GtkWidget *widget __attribute((unused)),
                        GdkEventFocus *event __attribute((unused)),
                        jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postFocusEventID,
                                AWT_FOCUS_LOST,
                                JNI_FALSE);

  return FALSE;
}
