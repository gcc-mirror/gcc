/* gtkscrollbarpeer.c -- Native implementation of GtkScrollbarPeer
   Copyright (C) 1998, 1999, 2006 Free Software Foundation, Inc.

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


#include <math.h>
#include "gtkpeer.h"
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include "gnu_java_awt_peer_gtk_GtkScrollbarPeer.h"

#define AWT_ADJUSTMENT_UNIT_INCREMENT 1
#define AWT_ADJUSTMENT_UNIT_DECREMENT 2
#define AWT_ADJUSTMENT_BLOCK_DECREMENT 3
#define AWT_ADJUSTMENT_BLOCK_INCREMENT 4
#define AWT_ADJUSTMENT_TRACK 5

static jmethodID postAdjustmentEventID;
static GtkWidget *scrollbar_get_widget (GtkWidget *widget);

void
cp_gtk_scrollbar_init_jni (void)
{
  jclass gtkscrollbarpeer;

  gtkscrollbarpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                     "gnu/java/awt/peer/gtk/GtkScrollbarPeer");

  postAdjustmentEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                                     gtkscrollbarpeer,
                                                     "postAdjustmentEvent",
                                                     "(II)V");
}

#if GTK_MINOR_VERSION > 4
static gboolean slider_moved_cb (GtkRange *range,
                                 GtkScrollType scroll,
                                 gdouble value,
                                 jobject obj);
#else
static void post_change_event_cb (GtkRange *range,
                                  jobject peer);
#endif

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_create
  (JNIEnv *env, jobject obj, jint orientation, jint value,
   jint min, jint max, jint step_incr, jint page_incr, jint visible_amount)
{
  GtkWidget *scrollbar;
  GtkWidget *eventbox;
  GtkObject *adj;

  /* Create global reference and save it for future use */
  gtkpeer_set_global_ref (env, obj);

  gdk_threads_enter ();

  /* A little hack because gtk_range_set_range() doesn't allow min == max. */
  if (min == max)
    {
      if (visible_amount == 0)
	visible_amount = 1;
      max++;
    }

  adj = gtk_adjustment_new ((gdouble) value,
                            (gdouble) min,
                            (gdouble) max,
			    (gdouble) step_incr,
                            (gdouble) page_incr,
			    (gdouble) visible_amount);

  scrollbar = orientation
    ? gtk_vscrollbar_new (GTK_ADJUSTMENT (adj))
    : gtk_hscrollbar_new (GTK_ADJUSTMENT (adj));
  eventbox = gtk_event_box_new ();
  gtk_container_add (GTK_CONTAINER (eventbox), scrollbar);
  gtk_widget_show (scrollbar);
  
  GTK_RANGE (scrollbar)->round_digits = 0;
  /* These calls seem redundant but they are not.  They clamp values
     so that the slider's entirety is always between the two
     steppers. */
  gtk_range_set_range (GTK_RANGE (scrollbar), (gdouble) min, (gdouble) max);
  gtk_range_set_value (GTK_RANGE (scrollbar), (gdouble) value);

  gdk_threads_leave ();

  gtkpeer_set_widget (env, obj, eventbox);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr = gtkpeer_get_widget (env, obj);
  GtkWidget *wid = scrollbar_get_widget (GTK_WIDGET (ptr));
  jobject gref = gtkpeer_get_global_ref (env, obj);
  g_assert (gref);

  gdk_threads_enter ();

  /* Scrollbar signals */
#if GTK_MINOR_VERSION > 4
  g_signal_connect (G_OBJECT (wid), "change-value",
                    G_CALLBACK (slider_moved_cb), gref);
#else
  g_signal_connect (G_OBJECT (wid), "value-changed",
                    G_CALLBACK (post_change_event_cb), gref);
#endif

  /* Component signals */
  cp_gtk_component_connect_signals (G_OBJECT (wid), gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_setLineIncrement
  (JNIEnv *env, jobject obj, jint amount)
{
  void *ptr;
  GtkAdjustment *adj;
  GtkWidget *wid;

  ptr = gtkpeer_get_widget (env, obj);
  wid = scrollbar_get_widget (GTK_WIDGET (ptr));
  
  gdk_threads_enter ();

  adj = gtk_range_get_adjustment (GTK_RANGE (wid));
  adj->step_increment = (gdouble) amount;
  gtk_adjustment_changed (adj);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_setPageIncrement
  (JNIEnv *env, jobject obj, jint amount)
{
  void *ptr;
  GtkAdjustment *adj;
  GtkWidget *wid;
  
  ptr = gtkpeer_get_widget (env, obj);
  wid = scrollbar_get_widget (GTK_WIDGET (ptr));
  
  gdk_threads_enter ();

  adj = gtk_range_get_adjustment (GTK_RANGE (wid));
  adj->page_increment = (gdouble) amount;
  gtk_adjustment_changed (adj);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkScrollbarPeer_setBarValues
  (JNIEnv *env, jobject obj, jint value, jint visible, jint min, jint max)
{
  void *ptr;
  GtkAdjustment *adj;
  GtkWidget *wid;
  
  ptr = gtkpeer_get_widget (env, obj);
  wid = scrollbar_get_widget (GTK_WIDGET (ptr));
  
  gdk_threads_enter ();

  /* A little hack because gtk_range_set_range() doesn't allow min == max. */
  if (min == max)
    {
      if (visible == 0)
	visible = 1;
      max++;
    }

  adj = gtk_range_get_adjustment (GTK_RANGE (wid));
  adj->page_size = (gdouble) visible;

  gtk_range_set_range (GTK_RANGE (wid), (gdouble) min, (gdouble) max);
  gtk_range_set_value (GTK_RANGE (wid), (gdouble) value);

  gdk_threads_leave ();
}

#if GTK_MINOR_VERSION > 4
static gboolean
slider_moved_cb (GtkRange *range,
                 GtkScrollType scroll,
                 gdouble value,
                 jobject obj)
{
  GtkAdjustment *adj = gtk_range_get_adjustment (GTK_RANGE (range));
  
  value = CLAMP (value, adj->lower,
                 (adj->upper - adj->page_size));

  if (range->round_digits >= 0)
    {
      gdouble power;
      gint i;

      i = range->round_digits;
      power = 1;
      while (i--)
        power *= 10;
      
      value = floor ((value * power) + 0.5) / power;
    }
  
  switch (scroll)
    {
    case GTK_SCROLL_STEP_BACKWARD:
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj, postAdjustmentEventID,
                                    AWT_ADJUSTMENT_UNIT_DECREMENT,
                                    (jint) value);
      break;
    case GTK_SCROLL_STEP_FORWARD:
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj, postAdjustmentEventID,
                                    AWT_ADJUSTMENT_UNIT_INCREMENT,
                                    (jint) value);
      break;
    case GTK_SCROLL_PAGE_BACKWARD:
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj, postAdjustmentEventID,
                                    AWT_ADJUSTMENT_BLOCK_DECREMENT,
                                    (jint) value);
      break;
    case GTK_SCROLL_PAGE_FORWARD:
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj, postAdjustmentEventID,
                                    AWT_ADJUSTMENT_BLOCK_INCREMENT,
                                    (jint) value);
      break;
    default:
      /* GTK_SCROLL_JUMP: */
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj, postAdjustmentEventID,
                                    AWT_ADJUSTMENT_TRACK,
                                    (jint) value);
      break;
    }
  return FALSE;
}
#else
static void
post_change_event_cb (GtkRange *range, jobject peer)
{
  GtkAdjustment *adj;
  adj = gtk_range_get_adjustment (range);
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer, postAdjustmentEventID,
				AWT_ADJUSTMENT_TRACK, (jint) adj->value);
}
#endif

static GtkWidget *
scrollbar_get_widget (GtkWidget *widget)
{
  GtkWidget *wid;
  g_assert (GTK_IS_EVENT_BOX (widget));
  wid = gtk_bin_get_child (GTK_BIN(widget));

  return wid;
}
