/* gtkcomponentpeer.c -- Native implementation of GtkComponentPeer
   Copyright (C) 1998, 1999, 2002, 2004 Free Software Foundation, Inc.

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


#include "gtkpeer.h"
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include <gtk/gtkprivate.h>

static GtkWidget *find_fg_color_widget (GtkWidget *widget);
static GtkWidget *find_bg_color_widget (GtkWidget *widget);

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetCursor 
  (JNIEnv *env, jobject obj, jint type) 
{
  void *ptr;
  GtkWidget *widget;
  GdkCursorType gdk_cursor_type;
  GdkCursor *gdk_cursor;

  ptr = NSA_GET_PTR (env, obj);

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
      
  gdk_threads_enter ();

  widget = GTK_WIDGET(ptr);

  gdk_cursor = gdk_cursor_new (gdk_cursor_type);
  gdk_window_set_cursor (widget->window, gdk_cursor);
  gdk_cursor_destroy (gdk_cursor);

  gdk_threads_leave ();
}

/*
 * Find the origin of a widget's window.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetGetLocationOnScreen
  (JNIEnv * env, jobject obj, jintArray jpoint)
{
  void *ptr;
  jint *point;

  ptr = NSA_GET_PTR (env, obj);
  point = (*env)->GetIntArrayElements (env, jpoint, 0);

  gdk_threads_enter ();

  gdk_window_get_origin (GTK_WIDGET (ptr)->window, point, point+1);

  if (!GTK_IS_CONTAINER (ptr))
    {
      *point += GTK_WIDGET(ptr)->allocation.x;
      *(point+1) += GTK_WIDGET(ptr)->allocation.y;
    }

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements(env, jpoint, point, 0);
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

  ptr = NSA_GET_PTR (env, obj);

  dims = (*env)->GetIntArrayElements (env, jdims, 0);  
  dims[0] = dims[1] = 0;

  gdk_threads_enter ();

  gtk_widget_size_request (GTK_WIDGET (ptr), &requisition);

  dims[0] = requisition.width;
  dims[1] = requisition.height;

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements (env, jdims, dims, 0);
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

  ptr = NSA_GET_PTR (env, obj);

  dims = (*env)->GetIntArrayElements (env, jdims, 0);  
  dims[0] = dims[1] = 0;

  gdk_threads_enter ();

  /* Save the widget's current size request. */
  gtk_widget_size_request (GTK_WIDGET (ptr), &current_req);

  /* Get the widget's "natural" size request. */
  gtk_widget_set_size_request (GTK_WIDGET (ptr), -1, -1);
  gtk_widget_size_request (GTK_WIDGET (ptr), &natural_req);

  /* Reset the widget's size request. */
  gtk_widget_set_size_request (GTK_WIDGET (ptr),
			       current_req.width, current_req.height);

  dims[0] = natural_req.width;
  dims[1] = natural_req.height;

  gdk_threads_leave ();

  (*env)->ReleaseIntArrayElements (env, jdims, dims, 0);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_setNativeBounds
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  GtkWidget *widget;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  widget = GTK_WIDGET (ptr);
  if (GTK_IS_VIEWPORT (widget->parent))
    {
      gtk_widget_set_size_request (widget, width, height);
    }
  else
    {
      gtk_widget_set_size_request (widget, width, height);
      gtk_layout_move (GTK_LAYOUT (widget->parent), widget, x, y);
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

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  bg = GTK_WIDGET (ptr)->style->bg[GTK_STATE_NORMAL];
  gdk_threads_leave ();

  array = (*env)->NewIntArray (env, 3);
  rgb = (*env)->GetIntArrayElements (env, array, NULL);
  /* convert color data from 16 bit values down to 8 bit values */
  rgb[0] = bg.red   >> 8;
  rgb[1] = bg.green >> 8;
  rgb[2] = bg.blue  >> 8;
  (*env)->ReleaseIntArrayElements (env, array, rgb, 0);

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

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  fg = GTK_WIDGET (ptr)->style->fg[GTK_STATE_NORMAL];
  gdk_threads_leave ();

  array = (*env)->NewIntArray (env, 3);
  rgb = (*env)->GetIntArrayElements (env, array, NULL);
  /* convert color data from 16 bit values down to 8 bit values */
  rgb[0] = fg.red   >> 8;
  rgb[1] = fg.green >> 8;
  rgb[2] = fg.blue  >> 8;
  (*env)->ReleaseIntArrayElements (env, array, rgb, 0);

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

  ptr = NSA_GET_PTR (env, obj);

  normal_color.red = (red / 255.0) * 65535;
  normal_color.green = (green / 255.0) * 65535;
  normal_color.blue = (blue / 255.0) * 65535;

  /* This calculation only approximates the active colors produced by
     Sun's AWT. */
  active_color.red = 0.85 * (red / 255.0) * 65535;
  active_color.green = 0.85 * (green / 255.0) * 65535;
  active_color.blue = 0.85 * (blue / 255.0) * 65535;

  gdk_threads_enter ();

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

  ptr = NSA_GET_PTR (env, obj);

  color.red = (red / 255.0) * 65535;
  color.green = (green / 255.0) * 65535;
  color.blue = (blue / 255.0) * 65535;

  gdk_threads_enter ();

  widget = find_fg_color_widget (GTK_WIDGET (ptr));

  gtk_widget_modify_fg (widget, GTK_STATE_NORMAL, &color);
  gtk_widget_modify_fg (widget, GTK_STATE_ACTIVE, &color);
  gtk_widget_modify_fg (widget, GTK_STATE_PRELIGHT, &color);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkSetFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  PangoFontDescription *font_desc;

  ptr = NSA_GET_PTR (env, obj);

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  gdk_threads_enter();

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc, size * PANGO_SCALE);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET(ptr), font_desc);

  pango_font_description_free (font_desc);

  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, name, font_name);
}

void
set_visible (GtkWidget *widget, jboolean visible)
{
  if (visible)
    gtk_widget_show (widget);
  else
    gtk_widget_hide (widget);
}

GtkLayout *
find_gtk_layout (GtkWidget *parent)
{
  if (GTK_IS_WINDOW (parent))
    {
      GList *children = gtk_container_children 
	                  (GTK_CONTAINER (GTK_BIN (parent)->child));

      if (GTK_IS_MENU_BAR (children->data))
	return GTK_LAYOUT (children->next->data);
      else /* GTK_IS_LAYOUT (children->data) */
	return GTK_LAYOUT (children->data);
    }

  return NULL;
}

#define WIDGET_CLASS(w)  GTK_WIDGET_CLASS (GTK_OBJECT (w)->klass)

void
set_parent (GtkWidget *widget, GtkContainer *parent)
{
  if (GTK_IS_WINDOW (parent))
    {
      GList *children = gtk_container_children 
	                  (GTK_CONTAINER (GTK_BIN (parent)->child));

      if (GTK_IS_MENU_BAR (children->data))
	gtk_layout_put (GTK_LAYOUT (children->next->data), widget, 0, 0);
      else /* GTK_IS_LAYOUT (children->data) */
	gtk_layout_put (GTK_LAYOUT (children->data), widget, 0, 0);
    }
  else
    if (GTK_IS_SCROLLED_WINDOW (parent))
      {
/*  	if (WIDGET_CLASS (widget)->set_scroll_adjustments_signal) */
/*  	  gtk_container_add (GTK_CONTAINER (parent), widget); */
/*  	else */
/*  	  { */
	    gtk_scrolled_window_add_with_viewport 
	      (GTK_SCROLLED_WINDOW (parent), widget);
	    gtk_viewport_set_shadow_type (GTK_VIEWPORT (widget->parent), 
					  GTK_SHADOW_NONE);
/*  	  } */

      }
/*        gtk_layout_put  */
/*  	(GTK_LAYOUT (GTK_BIN (parent)->child), widget, 0, 0); */

/*      if (GTK_IS_SCROLLED_WINDOW (parent)) */
/*        gtk_layout_put  */
/*  	(GTK_LAYOUT (GTK_BIN (GTK_BIN (parent)->child)->child), widget, 0, 0); */
    else
      gtk_layout_put (GTK_LAYOUT (parent), widget, 0, 0);
}

JNIEXPORT jboolean JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_isEnabled 
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jboolean ret_val;
  
  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  ret_val = GTK_WIDGET_IS_SENSITIVE (GTK_WIDGET (ptr));
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
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_set__Ljava_lang_String_2Ljava_lang_String_2
  (JNIEnv *env, jobject obj, jstring jname, jstring jvalue)
{
  const char *name;
  const char *value;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);
  name = (*env)->GetStringUTFChars (env, jname, NULL);
  value = (*env)->GetStringUTFChars (env, jvalue, NULL);

  gdk_threads_enter();
  g_object_set(ptr, name, value, NULL);
  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, jname, name);
  (*env)->ReleaseStringUTFChars (env, jvalue, value);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_set__Ljava_lang_String_2Z
  (JNIEnv *env, jobject obj, jstring jname, jboolean value)
{
  const char *name;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  name = (*env)->GetStringUTFChars (env, jname, NULL);

  gdk_threads_enter();
  g_object_set(ptr, name, value, NULL);
  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, jname, name);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_set__Ljava_lang_String_2I
  (JNIEnv *env, jobject obj, jstring jname, jint value)
{
  const char *name;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);
  name = (*env)->GetStringUTFChars (env, jname, NULL);

  gdk_threads_enter();                          
  g_object_set(ptr, name, value, NULL);
  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, jname, name);
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_set__Ljava_lang_String_2F
  (JNIEnv *env, jobject obj, jstring jname, jfloat value)
{
  const char *name;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);
  name = (*env)->GetStringUTFChars (env, jname, NULL);

  gdk_threads_enter();                          
  g_object_set(ptr, name, value, NULL);
  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, jname, name);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_set__Ljava_lang_String_2Ljava_lang_Object_2
  (JNIEnv *env, jobject obj1, jstring jname, jobject obj2)
{
  const char *name;
  void *ptr1, *ptr2;

  ptr1 = NSA_GET_PTR (env, obj1);
  ptr2 = NSA_GET_PTR (env, obj2);
  
  name = (*env)->GetStringUTFChars (env, jname, NULL);

  /* special case to catch where we need to set the parent */
  if (!strcmp (name, "parent"))
    {
      gdk_threads_enter ();
      set_parent (GTK_WIDGET (ptr1), GTK_CONTAINER (ptr2));
      gdk_threads_leave ();

      (*env)->ReleaseStringUTFChars (env, jname, name);
      return;
    }

  gdk_threads_enter();                          
  g_object_set(ptr1, name, ptr2, NULL);
  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, jname, name);
}

gboolean
filter_expose_event_handler (GtkWidget *widget, GdkEvent *event, jobject peer)
{
  /*
   * Prevent the default event handler from getting this signal if applicable
   * FIXME: I came up with these filters by looking for patterns in the unwanted
   *        expose events that are fed back to us from gtk/X. Perhaps there is
   *        a way to prevent them from occuring in the first place.
   */
  if (event->type == GDK_EXPOSE && (!GTK_IS_LAYOUT(widget)
                                    || event->any.window != widget->window))
    {
      g_signal_stop_emission_by_name(GTK_OBJECT(widget), "event");
      return FALSE;
    }
  else
    {
      /* There may be non-expose events that are triggered while we're
        painting a heavyweight peer. */
      return pre_event_handler(widget, event, peer);
    }
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_addExposeFilter
  (JNIEnv *env, jobject obj)
{
  GtkObject *filterobj;
  GtkWidget *vbox, *layout;
  GList *children;
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);

  g_assert (gref);

  gdk_threads_enter ();

  /* GtkFramePeer is built as a GtkLayout inside a GtkVBox inside a GtkWindow.
     Events go to the GtkLayout layer, so we filter them there. */
  if (GTK_IS_WINDOW(ptr))
    {
      children = gtk_container_get_children(GTK_CONTAINER(ptr));
      vbox = children->data;
      g_assert (GTK_IS_VBOX(vbox));

      children = gtk_container_get_children(GTK_CONTAINER(vbox));
      do
      {
        layout = children->data;
        children = children->next;
      }
      while (!GTK_IS_LAYOUT (layout) && children != NULL);
      g_assert (GTK_IS_LAYOUT(layout));

      filterobj = GTK_OBJECT(layout);
    }
  else
    {
      filterobj = GTK_OBJECT(ptr);
    }

  g_signal_handlers_block_by_func (filterobj, *pre_event_handler, *gref);
  g_signal_connect( filterobj, "event",
                    G_CALLBACK(filter_expose_event_handler), *gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_removeExposeFilter
  (JNIEnv *env, jobject obj)
{
  GtkObject *filterobj;
  GtkWidget *vbox, *layout;
  GList *children;
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);

  g_assert (gref);

  gdk_threads_enter ();

  /* GtkFramePeer is built as a GtkLayout inside a GtkVBox inside a GtkWindow.
     Events go to the GtkLayout layer, so we filter them there. */
  if (GTK_IS_WINDOW(ptr))
    {
      children = gtk_container_get_children(GTK_CONTAINER(ptr));
      vbox = children->data;
      g_assert (GTK_IS_VBOX(vbox));

      children = gtk_container_get_children(GTK_CONTAINER(vbox));
      do
      {
        layout = children->data;
        children = children->next;
      }
      while (!GTK_IS_LAYOUT (layout) && children != NULL);
      g_assert (GTK_IS_LAYOUT(layout));

      filterobj = GTK_OBJECT(layout);
    }
  else
    {
      filterobj = GTK_OBJECT(ptr);
    }

  g_signal_handlers_disconnect_by_func (filterobj,
                                        *filter_expose_event_handler, *gref);
  g_signal_handlers_unblock_by_func (filterobj, *pre_event_handler, *gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetQueueDrawArea
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  GdkRectangle rect;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  rect.x = x + GTK_WIDGET(ptr)->allocation.x;
  rect.y = y + GTK_WIDGET(ptr)->allocation.y;
  rect.width = width;
  rect.height = height;

  gdk_threads_enter ();

  gdk_window_invalidate_rect (GTK_WIDGET (ptr)->window, &rect, 0);
  gdk_window_process_all_updates();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectJObject
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));

  connect_awt_hook (env, obj, 1, GTK_WIDGET (ptr)->window);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);
  g_assert (gref);

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));
  
  /* FIXME: We could check here if this is a scrolled window with a
     single child that does not have an associated jobject.  This
     means that it is one of our wrapped widgets like List or TextArea
     and thus we could connect the signal to the child without having
     to specialize this method. */

  /* Connect EVENT signal, which happens _before_ any specific signal. */

  g_signal_connect (GTK_OBJECT (ptr), "event", 
                    G_CALLBACK (pre_event_handler), *gref);

  gdk_threads_leave ();
}

static GtkWidget *
find_fg_color_widget (GtkWidget *widget)
{
  GtkWidget *fg_color_widget;

  if (GTK_IS_EVENT_BOX (widget) || GTK_IS_BUTTON (widget))
    fg_color_widget = gtk_bin_get_child (GTK_BIN(widget));
  else
    fg_color_widget = widget;

  return fg_color_widget;
}

static GtkWidget *
find_bg_color_widget (GtkWidget *widget)
{
  GtkWidget *bg_color_widget;

  if (GTK_IS_WINDOW (widget))
    {
      GtkWidget *vbox;
      GList* children;

      children = gtk_container_get_children(GTK_CONTAINER(widget));
      vbox = children->data;

      children = gtk_container_get_children(GTK_CONTAINER(vbox));
      bg_color_widget = children->data;
    }
  else
    bg_color_widget = widget;

  return bg_color_widget;
}

