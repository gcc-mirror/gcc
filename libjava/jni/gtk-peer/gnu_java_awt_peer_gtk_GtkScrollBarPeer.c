/* gtkscrollbarpeer.c -- Native implementation of GtkScrollbarPeer
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkScrollbarPeer.h"

struct range_scrollbar
{
  GtkRange *range;
  jobject *scrollbar;
};

static void 
post_change_event (GtkRange *range,
		   struct range_scrollbar *rs)
{
  GtkAdjustment *adj;
  adj = gtk_range_get_adjustment (range);
  (*gdk_env)->CallVoidMethod (gdk_env, *(rs->scrollbar), postAdjustmentEventID,
			      AWT_ADJUSTMENT_TRACK, (jint) adj->value);
  
}

static void 
post_adjustment_event (GtkRange *range, GtkScrollType scroll, 
		       struct range_scrollbar *rs)
{
  jint type;
  GtkAdjustment *adj;

  adj = gtk_range_get_adjustment (range);

  switch (scroll)
    {
    case GTK_SCROLL_STEP_UP:
    case GTK_SCROLL_STEP_RIGHT:
    case GTK_SCROLL_STEP_FORWARD:
      type = AWT_ADJUSTMENT_UNIT_INCREMENT;
      break;
    case GTK_SCROLL_STEP_DOWN:
    case GTK_SCROLL_STEP_LEFT:
    case GTK_SCROLL_STEP_BACKWARD:
      type = AWT_ADJUSTMENT_UNIT_DECREMENT;
      break;
    case GTK_SCROLL_PAGE_UP:
    case GTK_SCROLL_PAGE_RIGHT:
    case GTK_SCROLL_PAGE_FORWARD:
      type = AWT_ADJUSTMENT_BLOCK_INCREMENT;
      break;
    case GTK_SCROLL_PAGE_DOWN:
    case GTK_SCROLL_PAGE_LEFT:
    case GTK_SCROLL_PAGE_BACKWARD:
      type = AWT_ADJUSTMENT_BLOCK_DECREMENT;
      break;
    case GTK_SCROLL_JUMP:
    case GTK_SCROLL_NONE:  /* Apparently generated when slider is dragged. */
      type = AWT_ADJUSTMENT_TRACK;
      break;
    default: /* Can this happen?  If so, is this right? */
      return;
    }
  
  (*gdk_env)->CallVoidMethod (gdk_env, *(rs->scrollbar), postAdjustmentEventID,
			      type, (jint) adj->value);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_create
(JNIEnv *env, jobject obj, jint orientation, jint value, 
 jint min, jint max, jint step_incr, jint page_incr, jint visible_amount)
{
  GtkWidget *sb;
  GtkObject *adj;

  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  gdk_threads_enter ();
  
  adj = gtk_adjustment_new (value, min, max, 
			    step_incr, page_incr, 
			    visible_amount);

  sb = (orientation) ? gtk_vscrollbar_new (GTK_ADJUSTMENT (adj)) :
                       gtk_hscrollbar_new (GTK_ADJUSTMENT (adj));

  gdk_threads_leave ();

  NSA_SET_PTR (env, obj, sb);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_connectJObject
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));

  connect_awt_hook (env, obj, 1, GTK_SCROLLBAR (ptr)->range);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  struct range_scrollbar *rs;
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);
  g_assert (gref);

  rs = (struct range_scrollbar *) malloc (sizeof (struct range_scrollbar));

  gdk_threads_enter ();

  gtk_widget_realize (GTK_WIDGET (ptr));

  rs->range = GTK_RANGE (ptr);
  rs->scrollbar = gref;

  g_signal_connect (G_OBJECT (GTK_RANGE (ptr)), 
		      "move-slider", 
		      GTK_SIGNAL_FUNC (post_adjustment_event), rs);

  g_signal_connect (G_OBJECT (GTK_RANGE (ptr)), 
		      "value-changed", 
		      GTK_SIGNAL_FUNC (post_change_event), rs);

  gdk_threads_leave ();

  /* Connect the superclass signals.  */
  Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals (env, obj);
}


JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_setLineIncrement
    (JNIEnv *env, jobject obj, jint amount)
{
  void *ptr;
  GtkAdjustment *adj;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  adj = GTK_RANGE (ptr)->adjustment;
  adj->step_increment = amount;
  gtk_adjustment_changed (adj);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_setPageIncrement
    (JNIEnv *env, jobject obj, jint amount)
{
  void *ptr;
  GtkAdjustment *adj;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  adj = GTK_RANGE (ptr)->adjustment;
  adj->page_increment = amount;
  gtk_adjustment_changed (adj);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_setValues
    (JNIEnv *env, jobject obj, jint value, jint visible, jint min, jint max)
{
  void *ptr;
  GtkAdjustment *adj;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  adj = GTK_RANGE (ptr)->adjustment;
  adj->value = value;
  adj->page_size = visible;
  adj->lower = min;
  adj->upper = max;
  gtk_adjustment_changed (adj);

  gdk_threads_leave ();
}
