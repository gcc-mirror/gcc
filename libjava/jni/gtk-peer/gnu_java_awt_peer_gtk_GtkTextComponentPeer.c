/* gtktextcomponentpeer.c -- Native implementation of GtkTextComponentPeer
   Copyright (C) 1998, 1999, 2004 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkTextComponentPeer.h"

static void textcomponent_changed_cb (GtkEditable *editable,
                                  jobject peer);

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  GtkTextView *text = NULL;
  GtkTextBuffer *buf;
  void *ptr = NSA_GET_PTR (env, obj);
  jobject *gref = NSA_GET_GLOBAL_REF (env, obj);
  g_assert (gref);

  gdk_threads_enter ();

  if (GTK_IS_ENTRY(ptr))
    {
      g_signal_connect (GTK_EDITABLE (ptr), "changed",
                        G_CALLBACK (textcomponent_changed_cb), *gref);

      gdk_threads_leave ();

      /* Connect the superclass signals.  */
      Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals (env, *gref);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
          text = GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child);
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_TEXT_VIEW (ptr);
	}

      if (text)
	{
          buf = gtk_text_view_get_buffer (text);
          if (buf)
            g_signal_connect (buf, "changed",
                              G_CALLBACK (textcomponent_changed_cb), *gref);

          /* Connect the superclass signals.  */
          /* FIXME: Cannot do that here or it will get the sw and not the list.
             We must a generic way of doing this. */
          /* Java_gnu_java_awt_peer_gtk_GtkComponentPeer_connectSignals (env,
	                                                                 obj); */
          g_signal_connect (GTK_OBJECT (text), "event", 
                    G_CALLBACK (pre_event_handler), *gref);

          gdk_threads_leave ();
	}
    }
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_getCaretPosition
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  GtkEditable *editable;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextMark *mark;
  GtkTextIter iter;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      pos = gtk_editable_get_position (editable);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
	  mark = gtk_text_buffer_get_insert (buf);
	  gtk_text_buffer_get_iter_at_mark (buf, &iter, mark);
	  pos = gtk_text_iter_get_offset (&iter);
	}
    }

  gdk_threads_leave ();
  
  return pos;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_setCaretPosition
  (JNIEnv *env, jobject obj, jint pos)
{
  void *ptr;
  GtkEditable *editable;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter iter;
  GtkTextMark *oldmark;
  GtkTextIter olditer;
  int oldpos;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();
  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      gtk_editable_set_position (editable, pos);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));

	  /* Save old position. */
	  oldmark = gtk_text_buffer_get_insert (buf);
	  gtk_text_buffer_get_iter_at_mark (buf, &olditer, oldmark);
	  oldpos = gtk_text_iter_get_offset (&olditer);

	  /* Move to new position. */
	  gtk_text_buffer_get_iter_at_offset (buf, &iter, pos);
	  gtk_text_buffer_place_cursor (buf, &iter);

	  /* Scroll to new position. Alignment is determined
	     comparing the new position to the old position. */
	  if (oldpos > pos)
	    gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (text),
	                                  &iter, 0, TRUE, 0, 0);
	  else if (oldpos < pos)
	    gtk_text_view_scroll_to_iter (GTK_TEXT_VIEW (text),
	                                  &iter, 0, TRUE, 1, 1);
	}
    }

  gdk_threads_leave ();
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_getSelectionStart
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  GtkEditable *editable;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter start;
  GtkTextIter end;
  int starti, endi;
  GtkTextMark *mark;
  GtkTextIter iter;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      if (gtk_editable_get_selection_bounds (editable, &starti, &endi))
	pos = starti;
      else
        pos = gtk_editable_get_position (editable);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
	  if (gtk_text_buffer_get_selection_bounds(buf, &start, &end))
	    pos = gtk_text_iter_get_offset (&start);
	  else 
           {
            mark = gtk_text_buffer_get_insert (buf);
            gtk_text_buffer_get_iter_at_mark (buf, &iter, mark);
            pos = gtk_text_iter_get_offset (&iter);
           }  
	}
    }

  gdk_threads_leave ();

  return pos;
}

JNIEXPORT jint JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_getSelectionEnd
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  int pos = 0;
  GtkEditable *editable;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter start;
  GtkTextIter end;
  int starti, endi;
  GtkTextMark *mark;
  GtkTextIter iter;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      if (gtk_editable_get_selection_bounds (editable, &starti, &endi))
	pos = endi;
      else
        pos = gtk_editable_get_position (editable);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
	  if (gtk_text_buffer_get_selection_bounds(buf, &start, &end))
	    pos = gtk_text_iter_get_offset (&end);
	  else 
           {
            mark = gtk_text_buffer_get_insert (buf);
            gtk_text_buffer_get_iter_at_mark (buf, &iter, mark);
            pos = gtk_text_iter_get_offset (&iter);
           }    
	}
    }

  gdk_threads_leave ();

  return pos;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_select
  (JNIEnv *env, jobject obj, jint start, jint end)
{
  void *ptr;
  GtkEditable *editable;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter iter;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();


  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      gtk_editable_select_region (editable, start, end);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
	  gtk_text_buffer_get_iter_at_offset (buf, &iter, start);
	  /* quickly move both 'insert' and 'selection_bound' to the 
	     same position */
	  gtk_text_buffer_place_cursor (buf, &iter);  
	  gtk_text_buffer_get_iter_at_offset (buf, &iter, end);
	  gtk_text_buffer_move_mark_by_name (buf, "selection_bound", &iter);
	}
    }

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_setEditable
  (JNIEnv *env, jobject obj, jboolean state)
{
  void *ptr;
  GtkEditable *editable;
  GtkWidget *text = NULL;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      gtk_editable_set_editable (editable, state);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  gtk_text_view_set_editable (GTK_TEXT_VIEW (text), state);
	}
    }

  gdk_threads_leave ();
}

JNIEXPORT jstring JNICALL
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_getText
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  char *contents = NULL;
  jstring jcontents;
  GtkEditable *editable;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;
  GtkTextIter start, end;

  ptr = NSA_GET_PTR (env, obj);
  
  gdk_threads_enter ();

  if (GTK_IS_EDITABLE (ptr))
    {
      editable = GTK_EDITABLE (ptr);
      contents = gtk_editable_get_chars (editable, 0, -1);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
	  gtk_text_buffer_get_start_iter (buf, &start);
	  gtk_text_buffer_get_end_iter (buf, &end);
	  contents = gtk_text_buffer_get_text (buf, &start, &end, FALSE);
	}
    }

  gdk_threads_leave ();

  jcontents = (*env)->NewStringUTF (env, contents);
  g_free (contents);

  return jcontents;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkTextComponentPeer_setText
  (JNIEnv *env, jobject obj, jstring contents)
{
  void *ptr;
  const char *str;
  GtkWidget *text = NULL;
  GtkTextBuffer *buf;

  ptr = NSA_GET_PTR (env, obj);
  str = (*env)->GetStringUTFChars (env, contents, NULL);
  
  gdk_threads_enter ();

  if (GTK_IS_EDITABLE (ptr))
    {
      gtk_entry_set_text (GTK_ENTRY (ptr), str);
    }
  else
    {
      if (GTK_IS_SCROLLED_WINDOW (ptr))
	{
	  text = GTK_WIDGET (GTK_TEXT_VIEW (GTK_SCROLLED_WINDOW (ptr)->container.child));
	}
      else if (GTK_IS_TEXT_VIEW (ptr))
	{
	  text = GTK_WIDGET (ptr);
	}

      if (text)
	{
	  buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text));
	  gtk_text_buffer_set_text (buf, str, strlen (str));
	}
    }

  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, contents, str);
}

static void
textcomponent_changed_cb (GtkEditable *editable __attribute__((unused)),
			  jobject peer)
{
  (*gdk_env)->CallVoidMethod (gdk_env, peer, postTextEventID);
}
