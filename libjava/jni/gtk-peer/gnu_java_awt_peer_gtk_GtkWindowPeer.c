/* gtkwindowpeer.c -- Native implementation of GtkWindowPeer
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
#include "gnu_java_awt_peer_gtk_GtkWindowPeer.h"
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>
#include <X11/Xatom.h>

/* FIXME: we're currently seeing the double-activation that occurs
   with metacity and GTK.  See
   http://bugzilla.gnome.org/show_bug.cgi?id=140977 for details. */

static void window_get_frame_extents (GtkWidget *window,
                                      int *top, int *left,
                                      int *bottom, int *right);

static void request_frame_extents (GtkWidget *window);

static Bool property_notify_predicate (Display *display,
                                       XEvent  *xevent,
                                       XPointer arg);

static void window_delete_cb (GtkWidget *widget, GdkEvent *event,
			      jobject peer);
static void window_destroy_cb (GtkWidget *widget, GdkEvent *event,
			       jobject peer);
static void window_show_cb (GtkWidget *widget, jobject peer);
static void window_active_state_change_cb (GtkWidget *widget,
                                           GParamSpec *pspec,
                                           jobject peer);
static void window_focus_state_change_cb (GtkWidget *widget,
                                          GParamSpec *pspec,
                                          jobject peer);
static gboolean window_focus_in_cb (GtkWidget * widget,
                                    GdkEventFocus *event,
                                    jobject peer);
static gboolean window_focus_out_cb (GtkWidget * widget,
                                     GdkEventFocus *event,
                                     jobject peer);
static gboolean window_window_state_cb (GtkWidget *widget,
					GdkEvent *event,
					jobject peer);
static jint window_get_new_state (GtkWidget *widget);
static gboolean window_property_changed_cb (GtkWidget *widget,
					    GdkEventProperty *event,
					    jobject peer);
static void realize_cb (GtkWidget *widget, jobject peer);

/* Union used for type punning. */
union extents_union
{
  guchar **gu_extents;
  unsigned long **extents;
};

union atom_list_union
{
  guchar **gu_extents;
  Atom **atom_list;
};

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_create
  (JNIEnv *env, jobject obj, jint type, jboolean decorated, jobject parent)
{
  GtkWidget *window_widget;
  GtkWindow *window;
  void *window_parent;
  GtkWidget *fixed;

  NSA_SET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();
  
  window_widget = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  window = GTK_WINDOW (window_widget);

  /* Keep this window in front of its parent, if it has one. */
  if (parent)
    {
      window_parent = NSA_GET_PTR (env, parent);
      gtk_window_set_transient_for (window, GTK_WINDOW(window_parent));
    }

  gtk_window_set_decorated (window, decorated);

  gtk_window_set_type_hint (window, type);

  gtk_window_group_add_window (global_gtk_window_group, window);

  fixed = gtk_fixed_new ();
  gtk_container_add (GTK_CONTAINER (window_widget), fixed);

  gtk_widget_show (fixed);

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, window_widget);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_gtkWindowSetTitle
  (JNIEnv *env, jobject obj, jstring title)
{
  const char *c_title;
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  c_title = (*env)->GetStringUTFChars (env, title, NULL);

  gdk_threads_enter ();

  gtk_window_set_title (GTK_WINDOW (ptr), c_title);

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, title, c_title);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_gtkWindowSetResizable
  (JNIEnv *env, jobject obj, jboolean resizable)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_window_set_policy (GTK_WINDOW (ptr), resizable, resizable, FALSE);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_gtkWindowSetModal
  (JNIEnv *env, jobject obj, jboolean modal)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_window_set_modal (GTK_WINDOW (ptr), modal);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetVisible
  (JNIEnv *env, jobject obj, jboolean visible)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  if (visible)
    gtk_widget_show (GTK_WIDGET (ptr));
  else
    gtk_widget_hide (GTK_WIDGET (ptr));

  XFlush (GDK_DISPLAY ());

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject *gref;

  ptr = NSA_GET_PTR (env, obj);
  gref = NSA_GET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();

  g_signal_connect (G_OBJECT (ptr), "event",
                    G_CALLBACK (pre_event_handler), *gref);

  g_signal_connect (G_OBJECT (ptr), "delete-event",
		    G_CALLBACK (window_delete_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "destroy-event",
		    G_CALLBACK (window_destroy_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "show",
		    G_CALLBACK (window_show_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "notify::is-active",
  		    G_CALLBACK (window_active_state_change_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "notify::has-toplevel-focus",
  		    G_CALLBACK (window_focus_state_change_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "focus-in-event",
                    G_CALLBACK (window_focus_in_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "focus-out-event",
                    G_CALLBACK (window_focus_out_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "window-state-event",
		    G_CALLBACK (window_window_state_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "property-notify-event",
		    G_CALLBACK (window_property_changed_cb), *gref);

  g_signal_connect_after (G_OBJECT (ptr), "realize",
                          G_CALLBACK (realize_cb), *gref);

  g_signal_connect_after (G_OBJECT (ptr), "realize",
                          G_CALLBACK (connect_awt_hook_cb), *gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_toBack (JNIEnv *env, 
    jobject obj)
{
  void *ptr;
  ptr = NSA_GET_PTR (env, obj);
    
  gdk_threads_enter ();

  gdk_window_lower (GTK_WIDGET (ptr)->window);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_toFront (JNIEnv *env, 
    jobject obj)
{
  void *ptr;
  ptr = NSA_GET_PTR (env, obj);
    
  gdk_threads_enter ();

  gdk_window_raise (GTK_WIDGET (ptr)->window);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setBoundsCallback
  (JNIEnv *env __attribute__((unused)), jobject obj __attribute__((unused)),
   jobject window, jint x, jint y, jint width, jint height)
{
  /* Circumvent package-private access to call Window's
     setBoundsCallback method. */
  (*gdk_env())->CallVoidMethod (gdk_env(), window, setBoundsCallbackID,
			      x, y, width, height);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setSize
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  void *ptr = NSA_GET_PTR (env, obj);

  /* Avoid GTK runtime assertion failures. */
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

  gdk_threads_enter ();

  gtk_widget_set_size_request (GTK_WIDGET(ptr), width, height);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetBounds
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  void *ptr = NSA_GET_PTR (env, obj);

  /* Avoid GTK runtime assertion failures. */
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

  gdk_threads_enter ();

  gtk_window_move (GTK_WINDOW(ptr), x, y);
  /* The call to gdk_window_move is needed in addition to the call to
     gtk_window_move.  If gdk_window_move isn't called, then the
     following set of operations doesn't give the expected results:

     1. show a window
     2. manually move it to another position on the screen
     3. hide the window
     4. reposition the window with Component.setLocation
     5. show the window

     Instead of being at the position set by setLocation, the window
     is reshown at the position to which it was moved manually. */
  if (GTK_WIDGET (ptr)->window != NULL)
    gdk_window_move (GTK_WIDGET (ptr)->window, x, y);

  /* Need to change the widget's request size. */
  gtk_widget_set_size_request (GTK_WIDGET(ptr), width, height);
  /* Also need to call gtk_window_resize.  If the resize is requested
     by the program and the window's "resizable" property is true then
     the size request will not be honoured. */
  gtk_window_resize (GTK_WINDOW (ptr), width, height);
  gdk_threads_leave ();
}

static void
window_get_frame_extents (GtkWidget *window,
                          int *top, int *left, int *bottom, int *right)
{
  unsigned long *extents = NULL;
  union extents_union gu_ex;

  /* Guess frame extents in case _NET_FRAME_EXTENTS is not
     supported. */
  *top = 23;
  *left = 6;
  *bottom = 6;
  *right = 6;

  /* Request that the window manager set window's
     _NET_FRAME_EXTENTS property. */
  request_frame_extents (window);

  /* Attempt to retrieve window's frame extents. */
  gu_ex.extents = &extents;
  if (gdk_property_get (window->window,
                        gdk_atom_intern ("_NET_FRAME_EXTENTS", FALSE),
                        gdk_atom_intern ("CARDINAL", FALSE),
                        0,
                        sizeof (unsigned long) * 4,
                        FALSE,
                        NULL,
                        NULL,
                        NULL,
                        gu_ex.gu_extents))
    {
      *left = extents [0];
      *right = extents [1];
      *top = extents [2];
      *bottom = extents [3];
    }
}

static Atom extents_atom = 0;

/* Requests that the window manager set window's
   _NET_FRAME_EXTENTS property. */
static void
request_frame_extents (GtkWidget *window)
{
  const char *request_str = "_NET_REQUEST_FRAME_EXTENTS";
  GdkAtom request_extents = gdk_atom_intern (request_str, FALSE);

  /* Check if the current window manager supports
     _NET_REQUEST_FRAME_EXTENTS. */
  if (gdk_net_wm_supports (request_extents))
    {
      GdkDisplay *display = gtk_widget_get_display (window);
      Display *xdisplay = GDK_DISPLAY_XDISPLAY (display);

      GdkWindow *root_window = gdk_get_default_root_window ();
      Window xroot_window = GDK_WINDOW_XID (root_window);

      Atom extents_request_atom =
	gdk_x11_get_xatom_by_name_for_display (display, request_str);

      XEvent xevent;
      XEvent notify_xevent;

      unsigned long window_id = GDK_WINDOW_XID (GDK_DRAWABLE(window->window));

      if (!extents_atom)
	{
	  const char *extents_str = "_NET_FRAME_EXTENTS";
	  extents_atom =
	    gdk_x11_get_xatom_by_name_for_display (display, extents_str);
	}

      xevent.xclient.type = ClientMessage;
      xevent.xclient.message_type = extents_request_atom;
      xevent.xclient.display = xdisplay;
      xevent.xclient.window = window_id;
      xevent.xclient.format = 32;
      xevent.xclient.data.l[0] = 0;
      xevent.xclient.data.l[1] = 0;
      xevent.xclient.data.l[2] = 0;
      xevent.xclient.data.l[3] = 0;
      xevent.xclient.data.l[4] = 0;

      XSendEvent (xdisplay, xroot_window, False,
		  (SubstructureRedirectMask | SubstructureNotifyMask),
                  &xevent);

      XIfEvent(xdisplay, &notify_xevent,
	       property_notify_predicate, (XPointer) &window_id);
    }
}

static Bool
property_notify_predicate (Display *xdisplay __attribute__((unused)),
                           XEvent  *event,
                           XPointer window_id)
{
  unsigned long *window = (unsigned long *) window_id;

  if (event->xany.type == PropertyNotify
      && event->xany.window == *window
      && event->xproperty.atom == extents_atom)
    return True;
  else
    return False;
}

static void
window_delete_cb (GtkWidget *widget __attribute__((unused)),
		  GdkEvent *event __attribute__((unused)),
		  jobject peer)
{
  gdk_threads_leave ();
  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_CLOSING,
			      (jobject) NULL, (jint) 0);
  gdk_threads_enter ();
}

static void
window_destroy_cb (GtkWidget *widget __attribute__((unused)),
		   GdkEvent *event __attribute__((unused)),
		   jobject peer)
{
  gdk_threads_leave ();
  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_CLOSED,
			      (jobject) NULL, (jint) 0);
  gdk_threads_enter ();
}

static void
window_show_cb (GtkWidget *widget __attribute__((unused)),
		jobject peer)
{
  gdk_threads_leave ();
  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_OPENED,
			      (jobject) NULL, (jint) 0);
  gdk_threads_enter ();
}

static void
window_active_state_change_cb (GtkWidget *widget __attribute__((unused)),
			       GParamSpec *pspec __attribute__((unused)),
			       jobject peer __attribute__((unused)))
{
  /* FIXME: not sure if this is needed or not. */
  /* Remove the unused attributes if you fix the below.  */
#if 0
  gdk_threads_leave ();
  if (GTK_WINDOW (widget)->is_active)
    (*gdk_env())->CallVoidMethod (gdk_env(), peer,
                                postWindowEventID,
                                (jint) AWT_WINDOW_GAINED_FOCUS,
                                (jobject) NULL, (jint) 0);
  else
    (*gdk_env())->CallVoidMethod (gdk_env(), peer,
                                postWindowEventID,
                                (jint) AWT_WINDOW_DEACTIVATED,
                                (jobject) NULL, (jint) 0);
  gdk_threads_enter ();
#endif
}

static void
window_focus_state_change_cb (GtkWidget *widget,
			      GParamSpec *pspec __attribute__((unused)),
			      jobject peer)
{
  gdk_threads_leave ();
  if (GTK_WINDOW (widget)->has_toplevel_focus)
    (*gdk_env())->CallVoidMethod (gdk_env(), peer,
                                postWindowEventID,
                                (jint) AWT_WINDOW_ACTIVATED,
                                (jobject) NULL, (jint) 0);
  else
    (*gdk_env())->CallVoidMethod (gdk_env(), peer,
                                postWindowEventID,
                                (jint) AWT_WINDOW_DEACTIVATED,
                                (jobject) NULL, (jint) 0);
  gdk_threads_enter ();
}

static gboolean
window_focus_in_cb (GtkWidget * widget  __attribute__((unused)),
		    GdkEventFocus *event  __attribute__((unused)),
		    jobject peer)
{
  gdk_threads_leave ();
  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
                              postWindowEventID,
                              (jint) AWT_WINDOW_GAINED_FOCUS,
                              (jobject) NULL, (jint) 0);
  /* FIXME: somewhere after this is handled, the child window is
     getting an expose event. */
  gdk_threads_enter ();
  return FALSE;
}

static gboolean
window_focus_out_cb (GtkWidget * widget __attribute__((unused)),
		     GdkEventFocus *event __attribute__((unused)),
		     jobject peer)
{
  gdk_threads_leave ();
  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
                              postWindowEventID,
                              (jint) AWT_WINDOW_LOST_FOCUS,
                              (jobject) NULL, (jint) 0);
  /* FIXME: somewhere after this is handled, the child window is
     getting an expose event. */
  gdk_threads_enter ();
  return FALSE;
}

static gboolean
window_window_state_cb (GtkWidget *widget,
			GdkEvent *event,
			jobject peer)
{
  jint new_state;

  /* Handle WINDOW_ICONIFIED and WINDOW_DEICONIFIED events. */
  if (event->window_state.changed_mask & GDK_WINDOW_STATE_ICONIFIED)
    {
      /* We've either been iconified or deiconified. */
      if (event->window_state.new_window_state & GDK_WINDOW_STATE_ICONIFIED)
	{
	  /* We've been iconified. */
	  gdk_threads_leave ();
	  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
				      postWindowEventID,
				      (jint) AWT_WINDOW_ICONIFIED,
				      (jobject) NULL, (jint) 0);
	  gdk_threads_enter ();
	}
      else
	{
	  /* We've been deiconified. */
	  gdk_threads_leave ();
	  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
				      postWindowEventID,
				      (jint) AWT_WINDOW_DEICONIFIED,
				      (jobject) NULL, (jint) 0);
	  gdk_threads_enter ();
	}
    }

  /* Post a WINDOW_STATE_CHANGED event, passing the new frame state to
     GtkWindowPeer. */
  new_state = AWT_FRAME_STATE_NORMAL;

  if (event->window_state.new_window_state & GDK_WINDOW_STATE_ICONIFIED)
    new_state |= AWT_FRAME_STATE_ICONIFIED;

  new_state |= window_get_new_state (widget);

  gdk_threads_leave ();
  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_STATE_CHANGED,
			      (jobject) NULL, new_state);
  gdk_threads_enter ();
  return TRUE;
}

static jint
window_get_new_state (GtkWidget *widget)
{
  GdkDisplay *display = gtk_widget_get_display(widget);
  jint new_state = AWT_FRAME_STATE_NORMAL;
  Atom type;
  gint format;
  gulong atom_count;
  gulong bytes_after;
  Atom *atom_list = NULL;
  union atom_list_union alu;
  gulong i;

  alu.atom_list = &atom_list;
  XGetWindowProperty (GDK_DISPLAY_XDISPLAY (display), 
		      GDK_WINDOW_XID (widget->window),
		      gdk_x11_get_xatom_by_name_for_display (display, "_NET_WM_STATE"),
		      0, G_MAXLONG, False, XA_ATOM, &type, &format, &atom_count,
		      &bytes_after, alu.gu_extents);

  if (type != None)
    {
      Atom maxvert = gdk_x11_get_xatom_by_name_for_display (display, "_NET_WM_STATE_MAXIMIZED_VERT");
      Atom maxhorz	= gdk_x11_get_xatom_by_name_for_display (display, "_NET_WM_STATE_MAXIMIZED_HORZ");

      i = 0;
      while (i < atom_count)
        {
	  if (atom_list[i] == maxhorz)
	    new_state |= AWT_FRAME_STATE_MAXIMIZED_HORIZ;
          else if (atom_list[i] == maxvert)
	    new_state |= AWT_FRAME_STATE_MAXIMIZED_VERT;

          ++i;
        }

      XFree (atom_list);
    }
  return new_state;
}

static gboolean
window_property_changed_cb (GtkWidget *widget __attribute__((unused)),
                            GdkEventProperty *event,
                            jobject peer)
{
  unsigned long *extents;
  union extents_union gu_ex;

  gu_ex.extents = &extents;
  if (gdk_atom_intern ("_NET_FRAME_EXTENTS", FALSE) == event->atom
      && gdk_property_get (event->window,
                           gdk_atom_intern ("_NET_FRAME_EXTENTS", FALSE),
                           gdk_atom_intern ("CARDINAL", FALSE),
                           0,
                           sizeof (unsigned long) * 4,
                           FALSE,
                           NULL,
                           NULL,
                           NULL,
                           gu_ex.gu_extents))
    {
      gdk_threads_leave ();
      (*gdk_env())->CallVoidMethod (gdk_env(), peer,
				    postInsetsChangedEventID,
				    (jint) extents[2],  /* top */
				    (jint) extents[0],  /* left */
				    (jint) extents[3],  /* bottom */
				    (jint) extents[1]); /* right */
      gdk_threads_enter ();
    }
  

  return FALSE;
}

static void
realize_cb (GtkWidget *widget, jobject peer)
{
  jint top = 0;
  jint left = 0;
  jint bottom = 0;
  jint right = 0;
  jint width = 0;
  jint height = 0;

  width = (*gdk_env())->CallIntMethod (gdk_env(), peer, windowGetWidthID);
  height = (*gdk_env())->CallIntMethod (gdk_env(), peer, windowGetHeightID);

  window_get_frame_extents (widget, &top, &left, &bottom, &right);

  (*gdk_env())->CallVoidMethod (gdk_env(), peer,
				postInsetsChangedEventID,
				top, left, bottom, right);

  gtk_window_set_default_size (GTK_WINDOW (widget),
			       MAX (1, width - left - right),
			       MAX (1, height - top - bottom));

  /* set the size like we do in nativeSetBounds */
  gtk_widget_set_size_request (widget,
			       MAX (1, width - left - right),
			       MAX (1, height - top - bottom));

  gtk_window_resize (GTK_WINDOW (widget),
		     MAX (1, width - left - right),
		     MAX (1, height - top - bottom));
}
