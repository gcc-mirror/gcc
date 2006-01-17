/* gtkclipboard.c
   Copyright (C) 1998, 1999, 2005 Free Software Foundation, Inc.

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


#include "jcl.h"
#include "gtkpeer.h"
#include "gnu_java_awt_peer_gtk_GtkClipboard.h"

#define OBJECT_TARGET 1
#define TEXT_TARGET   2
#define IMAGE_TARGET  3
#define URI_TARGET    4

/* The clipboard and standard (string targets) shared with GtkSelection. */
GtkClipboard *cp_gtk_clipboard;

jstring cp_gtk_stringTarget;
jstring cp_gtk_imageTarget;
jstring cp_gtk_filesTarget;

/* Simple id to keep track of the selection we are currently managing. */
static gint current_selection = 0;

/* Whether we "own" the clipboard. And may clear it. */
static int owner = 0;

static jclass gtk_clipboard_class;
static jmethodID setSystemContentsID;

static jobject gtk_clipboard_instance = NULL;
static jmethodID provideContentID;
static jmethodID provideTextID;
static jmethodID provideImageID;
static jmethodID provideURIsID;

/* Called when clipboard owner changes. Used to update available targets. */
#if GTK_MINOR_VERSION > 4
static void
clipboard_owner_change_cb (GtkClipboard *clipboard __attribute__((unused)),
			   GdkEvent *event __attribute__((unused)),
			   gpointer user_data __attribute__((unused)))
{
  /* These are only interesting when we are not the owner. Otherwise
     we will have the set and clear functions doing the updating. */
  JNIEnv *env = cp_gtk_gdk_env ();
  if (!owner)
    (*env)->CallStaticVoidMethod (env, gtk_clipboard_class,
				  setSystemContentsID);
}
#endif

JNIEXPORT jboolean JNICALL 
Java_gnu_java_awt_peer_gtk_GtkClipboard_initNativeState (JNIEnv *env,
							 jclass gtkclipboard,
							 jstring string,
							 jstring image,
							 jstring files)
{
  GdkDisplay* display;
  jboolean can_cache;

  gtk_clipboard_class = gtkclipboard;
  setSystemContentsID = (*env)->GetStaticMethodID (env, gtk_clipboard_class,
						   "setSystemContents",
						   "()V");
  if (setSystemContentsID == NULL)
    return JNI_FALSE;

  cp_gtk_stringTarget = (*env)->NewGlobalRef(env, string);
  cp_gtk_imageTarget = (*env)->NewGlobalRef(env, image);
  cp_gtk_filesTarget = (*env)->NewGlobalRef(env, files);

  gdk_threads_enter ();
  cp_gtk_clipboard = gtk_clipboard_get (GDK_SELECTION_CLIPBOARD);

  display = gtk_clipboard_get_display (cp_gtk_clipboard);
  /* Check for support for clipboard owner changes. */
#if GTK_MINOR_VERSION > 4
  if (gdk_display_supports_selection_notification (display))
    {
      g_signal_connect (cp_gtk_clipboard, "owner-change",
			G_CALLBACK (clipboard_owner_change_cb), NULL);
      gdk_display_request_selection_notification (display,
						  GDK_SELECTION_CLIPBOARD);
      can_cache = JNI_TRUE;
    }
  else
#endif
    can_cache = JNI_FALSE;
  gdk_threads_leave ();

  return can_cache;
}

static void
clipboard_get_func (GtkClipboard *clipboard __attribute__((unused)),
		    GtkSelectionData *selection,
		    guint info,
		    gpointer user_data __attribute__((unused)))
{
  JNIEnv *env = cp_gtk_gdk_env ();
  
  if (info == OBJECT_TARGET)
    {
      const gchar *target_name;
      jstring target_string;
      jbyteArray bytes;
      jint len;
      jbyte *barray;

      target_name = gdk_atom_name (selection->target);
      if (target_name == NULL)
	return;
      target_string = (*env)->NewStringUTF (env, target_name);
      if (target_string == NULL)
	return;
      bytes = (*env)->CallObjectMethod(env,
				       gtk_clipboard_instance,
				       provideContentID,
				       target_string);
      if (bytes == NULL)
	return;
      len = (*env)->GetArrayLength(env, bytes);
      if (len <= 0)
	return;
      barray = (*env)->GetByteArrayElements(env, bytes, NULL);
      if (barray == NULL)
	return;
      gtk_selection_data_set (selection, selection->target, 8,
			      (guchar *) barray, len);

      (*env)->ReleaseByteArrayElements(env, bytes, barray, 0);

    }
  else if (info == TEXT_TARGET)
    {
      jstring string;
      const gchar *text;
      int len;
      string = (*env)->CallObjectMethod(env,
					gtk_clipboard_instance,
					provideTextID);
      if (string == NULL)
	return;
      len = (*env)->GetStringUTFLength (env, string);
      if (len == -1)
	return;
      text = (*env)->GetStringUTFChars (env, string, NULL);
      if (text == NULL)
	return;

      gtk_selection_data_set_text (selection, text, len);
      (*env)->ReleaseStringUTFChars (env, string, text);
    }
  /* Images and URIs/Files support only available with gtk+2.6 or higher. */
#if GTK_MINOR_VERSION > 4
  else if (info == IMAGE_TARGET)
    {
      jobject gtkimage;
      GdkPixbuf *pixbuf = NULL;
      
      gtkimage = (*env)->CallObjectMethod(env,
					  gtk_clipboard_instance,
					  provideImageID);
      if (gtkimage == NULL)
	return;
      
      pixbuf = cp_gtk_image_get_pixbuf (env, gtkimage);
      if (pixbuf != NULL)
	{
	  gtk_selection_data_set_pixbuf (selection, pixbuf);

	  /* if the GtkImage is offscreen, this is a temporary pixbuf
	     which should be thrown out. */
	  if(cp_gtk_image_is_offscreen (env, gtkimage) == JNI_TRUE)
	    gdk_pixbuf_unref (pixbuf);
	}
    }
  else if (info == URI_TARGET)
    {
      jobjectArray uris;
      jint count;
      int i;
      gchar **list;

      uris = (*env)->CallObjectMethod(env,
				      gtk_clipboard_instance,
				      provideURIsID);
      if (uris == NULL)
	return;
      count = (*env)->GetArrayLength (env, uris);
      if (count <= 0)
	return;

      list = (gchar **) JCL_malloc (env, (count + 1) * sizeof (gchar *));
      for (i = 0; i < count; i++)
	{
	  const char *text;
	  jstring uri;
	  
	  /* Mark NULL in so case of some error we can find the end. */
	  list[i] = NULL;
	  uri = (*env)->GetObjectArrayElement (env, uris, i);
	  if (uri == NULL)
	    break;
	  text = (*env)->GetStringUTFChars (env, uri, NULL);
	  if (text == NULL)
	    break;
	  list[i] = strdup (text);
	  (*env)->ReleaseStringUTFChars (env, uri, text);
	}

      if (i == count)
	{
	  list[count] = NULL;
	  gtk_selection_data_set_uris (selection, list);
	}

      for (i = 0; list[i] != NULL; i++)
	free (list[i]);
      JCL_free (env, list);
    }
#endif
}

static void
clipboard_clear_func (GtkClipboard *clipboard __attribute__((unused)),
		      gpointer user_data)
{
  if (owner && GPOINTER_TO_INT(user_data) == current_selection)
    {
      JNIEnv *env = cp_gtk_gdk_env();
      owner = 0;
      (*env)->CallStaticVoidMethod (env, gtk_clipboard_class,
				    setSystemContentsID);
    }
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkClipboard_advertiseContent
(JNIEnv *env,
 jobject instance,
 jobjectArray mime_array,
#if GTK_MINOR_VERSION > 4
 jboolean add_text, jboolean add_images, jboolean add_uris)
#else
 jboolean add_text __attribute__((unused)),
 jboolean add_images __attribute__((unused)),
 jboolean add_uris __attribute__((unused)))
#endif
{
  GtkTargetList *target_list;
  GList *list;
  GtkTargetEntry *targets;
  gint n, i;

  gdk_threads_enter ();
  target_list = gtk_target_list_new (NULL, 0);

  if (mime_array != NULL)
    {
      n = (*env)->GetArrayLength (env, mime_array);
      for (i = 0; i < n; i++)
	{
	  const char *text;
	  jstring target;
	  GdkAtom atom;

	  target = (*env)->GetObjectArrayElement (env, mime_array, i);
	  if (target == NULL)
	    break;
	  text = (*env)->GetStringUTFChars (env, target, NULL);
	  if (text == NULL)
	    break;

	  atom = gdk_atom_intern (text, FALSE);
	  gtk_target_list_add (target_list, atom, 0, OBJECT_TARGET);

	  (*env)->ReleaseStringUTFChars (env, target, text);
	}
    }

  /* Add extra targets that gtk+ can provide/translate for us. */
#if GTK_MINOR_VERSION > 4
  if (add_text)
    gtk_target_list_add_text_targets (target_list, TEXT_TARGET);
  if (add_images)
    gtk_target_list_add_image_targets (target_list, IMAGE_TARGET, TRUE);
  if (add_uris)
    gtk_target_list_add_uri_targets (target_list, URI_TARGET);
#else
  if (add_text)
    gtk_target_list_add (target_list,
	                 gdk_atom_intern ("STRING", FALSE),
	                 0, TEXT_TARGET);
#endif


  /* Turn list into a target table. */
  n = g_list_length (target_list->list);
  if (n > 0)
    {
      targets = g_new (GtkTargetEntry, n);
      for (list = target_list->list, i = 0;
	   list != NULL;
	   list = list->next, i++)
	{
	  GtkTargetPair *pair = (GtkTargetPair *) list->data;
	  targets[i].target = gdk_atom_name (pair->target);
	  targets[i].flags = pair->flags;
	  targets[i].info = pair->info;
	}

      /* Set the targets plus callback functions and ask for the clipboard
	 to be stored when the application exists if supported. */
      current_selection++;
      if (gtk_clipboard_set_with_data (cp_gtk_clipboard, targets, n,
				       clipboard_get_func,
				       clipboard_clear_func,
				       GINT_TO_POINTER(current_selection)))
	{
	  owner = 1;
	  if (gtk_clipboard_instance == NULL)
	    {
	      JNIEnv *env = cp_gtk_gdk_env ();
	      gtk_clipboard_instance =  (*env)->NewGlobalRef(env, instance);

	      provideContentID
		= (*env)->GetMethodID (env, gtk_clipboard_class,
				       "provideContent",
				       "(Ljava/lang/String;)[B");
	      if (provideContentID == NULL)
		return;

	      provideTextID
		= (*env)->GetMethodID (env, gtk_clipboard_class,
				       "provideText", "()Ljava/lang/String;");
	      if (provideTextID == NULL)
		return;

	      provideImageID
		= (*env)->GetMethodID (env, gtk_clipboard_class,
				       "provideImage",
				       "()Lgnu/java/awt/peer/gtk/GtkImage;");
	      if (provideImageID == NULL)
		return;

	      provideURIsID
		= (*env)->GetMethodID (env, gtk_clipboard_class,
				       "provideURIs",
				       "()[Ljava/lang/String;");
	      if (provideURIsID == NULL)
		return;
	    }
#if GTK_MINOR_VERSION > 4
	  gtk_clipboard_set_can_store (cp_gtk_clipboard, NULL, 0);
#endif
	}
      else
	{
	  owner = 0;
	  (*env)->CallStaticVoidMethod (env, gtk_clipboard_class,
					setSystemContentsID);
	}

      for (i = 0; i < n; i++)
	g_free (targets[i].target);
      g_free (targets);
    }
  else if (owner)
    {
      gtk_clipboard_clear (cp_gtk_clipboard);
      owner = 0;
    }

  gtk_target_list_unref (target_list);
  gdk_threads_leave ();
}
