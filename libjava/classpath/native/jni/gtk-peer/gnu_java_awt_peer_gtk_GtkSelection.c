/* gtkselection.c -- Native C functions for GtkSelection class using gtk+.
   Copyright (C) 2005 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkSelection.h"

static jmethodID mimeTypesAvailableID;

/* Note this is actually just a GtkClipboardReceivedFunc, not a real
   GtkClipboardTargetsReceivedFunc, see requestMimeTypes. */
static void
clipboard_targets_received (GtkClipboard *clipboard
			    __attribute__((unused)),
			    GtkSelectionData *target_data,
			    gpointer selection)
{
  GdkAtom *targets = NULL;
  gint targets_len = 0;
  gchar **target_strings = NULL;
  jobjectArray strings = NULL;
  int strings_len = 0;
  gboolean include_text = FALSE;
  gboolean include_image = FALSE;
  gboolean include_uris = FALSE;
  jobject selection_obj = (jobject) selection;
  JNIEnv *env = cp_gtk_gdk_env ();

  if (target_data != NULL && target_data->length > 0)
    {
      include_text = gtk_selection_data_targets_include_text (target_data);

#if GTK_MINOR_VERSION > 4
      include_image = gtk_selection_data_targets_include_image (target_data,
								TRUE);
#endif
      if (gtk_selection_data_get_targets (target_data, &targets, &targets_len))
	{
	  int i;
	  GdkAtom uri_list_atom = gdk_atom_intern ("text/uri-list", FALSE);
	  target_strings = g_new (gchar*, targets_len);
	  if (target_strings != NULL)
	    for (i = 0; i < targets_len; i++)
	      {
		gchar *name =  gdk_atom_name (targets[i]);
		if (strchr (name, '/') != NULL)
		  {
		    target_strings[i] = name;
		    strings_len++;
		    if (! include_uris && targets[i] == uri_list_atom)
		      include_uris = TRUE;
		  }
		else
		  target_strings[i] = NULL;
	      }
	}

      if (target_strings != NULL)
	{
	  int i = 0, j = 0;
	  jclass stringClass;
	  
	  if (include_text)
	    strings_len++;
	  if (include_image)
	    strings_len++;
	  if (include_uris)
	    strings_len++;
	  
	  stringClass = (*env)->FindClass (env, "java/lang/String");
	  strings = (*env)->NewObjectArray (env, strings_len, stringClass,
					    NULL);
	  if (strings != NULL)
	    {
	      if (include_text)
		(*env)->SetObjectArrayElement (env, strings, i++,
					       cp_gtk_stringTarget);
	      if (include_image)
		(*env)->SetObjectArrayElement (env, strings, i++,
					       cp_gtk_imageTarget);
	      if (include_uris)
		(*env)->SetObjectArrayElement (env, strings, i++,
					       cp_gtk_filesTarget);
	      
	      while(i < strings_len)
		{
		  if (target_strings[j] == NULL)
		    j++;
		  else
		    {
		      jstring string;
		      string = (*env)->NewStringUTF (env,
						     target_strings[j++]);
		      if (string == NULL)
			break;
		      (*env)->SetObjectArrayElement (env, strings, i++,
						     string);
		      (*env)->DeleteLocalRef (env, string);
		    }
		}
	    }

	  for (i = 0; i < targets_len; i++)
	    g_free (target_strings[i]);
	  g_free (target_strings);
	}
    }

  (*env)->CallVoidMethod (env, selection_obj,
			  mimeTypesAvailableID,
			  strings);
  (*env)->DeleteGlobalRef (env, selection_obj);
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkSelection_requestMimeTypes
(JNIEnv *env, jobject selection)
{
  jobject selection_obj;
  selection_obj = (*env)->NewGlobalRef(env, selection);
  if (selection_obj == NULL)
    return;

  if (mimeTypesAvailableID == NULL)
    {
      jclass gtk_selection_class;
      gtk_selection_class = (*env)->GetObjectClass (env, selection_obj);
      mimeTypesAvailableID = (*env)->GetMethodID (env, gtk_selection_class,
						"mimeTypesAvailable",
						"([Ljava/lang/String;)V");
      if (mimeTypesAvailableID == NULL)
	return;
    }

  /* We would have liked to call gtk_clipboard_request_targets ()
     since that is more general. But the result of that, an array of
     GdkAtoms, cannot be used with the
     gtk_selection_data_targets_include_<x> functions (despite what
     the name suggests). */
  gdk_threads_enter ();
  gtk_clipboard_request_contents (cp_gtk_clipboard,
				  gdk_atom_intern ("TARGETS", FALSE),
				  clipboard_targets_received,
				  (gpointer) selection_obj);
  gdk_threads_leave ();
}


static jmethodID textAvailableID;

static void
clipboard_text_received (GtkClipboard *clipboard
			 __attribute__((unused)),
			 const gchar *text,
			 gpointer selection)
{
  jstring string;
  jobject selection_obj = (jobject) selection;

  JNIEnv *env = cp_gtk_gdk_env ();
  if (text != NULL)
    string = (*env)->NewStringUTF (env, text);
  else
    string = NULL;

  (*env)->CallVoidMethod (env, selection_obj,
                          textAvailableID,
                          string);
  (*env)->DeleteGlobalRef (env, selection_obj);

  if (string != NULL)
    (*env)->DeleteLocalRef (env, string);

}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkSelection_requestText
(JNIEnv *env, jobject selection)
{
  jobject selection_obj;
  selection_obj = (*env)->NewGlobalRef(env, selection);
  if (selection_obj == NULL)
    return;

  if (textAvailableID == NULL)
    {
      jclass gtk_selection_class;
      gtk_selection_class = (*env)->GetObjectClass (env, selection_obj);
      textAvailableID = (*env)->GetMethodID (env, gtk_selection_class,
					     "textAvailable",
					     "(Ljava/lang/String;)V");
      if (textAvailableID == NULL)
        return;
    }

  gdk_threads_enter ();
  gtk_clipboard_request_text (cp_gtk_clipboard,
			      clipboard_text_received,
			      (gpointer) selection_obj);
  gdk_threads_leave ();
}

static jmethodID imageAvailableID;

static void
clipboard_image_received (GtkClipboard *clipboard
			  __attribute__((unused)),
			  GdkPixbuf *pixbuf,
			  gpointer selection)
{
  jobject pointer = NULL;
  jobject selection_obj = (jobject) selection;
  JNIEnv *env = cp_gtk_gdk_env ();

  if (pixbuf != NULL)
    {
      g_object_ref (pixbuf);
      pointer = JCL_NewRawDataObject (env, (void *) pixbuf);
    }

  (*env)->CallVoidMethod (env, selection_obj,
			  imageAvailableID,
                          pointer);
  (*env)->DeleteGlobalRef (env, selection_obj);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkSelection_requestImage (JNIEnv *env, jobject obj)
{
  jobject selection_obj;
  selection_obj = (*env)->NewGlobalRef(env, obj);
  if (selection_obj == NULL)
    return;

  if (imageAvailableID == NULL)
    {
      jclass gtk_selection_class;
      gtk_selection_class = (*env)->GetObjectClass (env, selection_obj);
      imageAvailableID = (*env)->GetMethodID (env, gtk_selection_class,
					     "imageAvailable",
					     "(Lgnu/classpath/Pointer;)V");
      if (imageAvailableID == NULL)
        return;
    }

#if GTK_MINOR_VERSION > 4
  gdk_threads_enter ();
  gtk_clipboard_request_image (cp_gtk_clipboard,
			       clipboard_image_received,
			       (gpointer) selection_obj);
  gdk_threads_leave ();
#else
  clipboard_image_received (cp_gtk_clipboard, NULL, (gpointer) selection_obj);
#endif
}

static jmethodID urisAvailableID;

static void
clipboard_uris_received (GtkClipboard *clipboard
			 __attribute__((unused)),
			 GtkSelectionData *uri_data,
			 gpointer selection)
{
  gchar **uris = NULL;
  jobjectArray strings = NULL;
  jobject selection_obj = (jobject) selection;
  JNIEnv *env = cp_gtk_gdk_env ();

#if GTK_MINOR_VERSION > 4
  if (uri_data != NULL)
    uris = gtk_selection_data_get_uris (uri_data);
#else
  if (uri_data != NULL)
    uris = NULL;
#endif

  if (uris != NULL)
    {
      int len, i;
      gchar **count = uris;
      jclass stringClass = (*env)->FindClass (env, "java/lang/String");

      len = 0;
      while (count[len])
	len++;

      strings = (*env)->NewObjectArray (env, len, stringClass, NULL);
      if (strings != NULL)
	{
	  for (i = 0; i < len; i++)
	    {
	      jstring string = (*env)->NewStringUTF (env, uris[i]);
	      if (string == NULL)
		break;
	      (*env)->SetObjectArrayElement (env, strings, i, string);
	      (*env)->DeleteLocalRef (env, string);
	    }
	}
      g_strfreev (uris);
    }

  (*env)->CallVoidMethod (env, selection_obj,
                          urisAvailableID,
                          strings);
  (*env)->DeleteGlobalRef (env, selection_obj);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkSelection_requestURIs (JNIEnv *env, jobject obj)
{
#if GTK_MINOR_VERSION > 4
  GdkAtom uri_atom;
#endif
  jobject selection_obj;
  selection_obj = (*env)->NewGlobalRef(env, obj);
  if (selection_obj == NULL)
    return;

  if (urisAvailableID == NULL)
    {
      jclass gtk_selection_class;
      gtk_selection_class = (*env)->GetObjectClass (env, selection_obj);
      urisAvailableID = (*env)->GetMethodID (env, gtk_selection_class,
					     "urisAvailable",
                                             "([Ljava/lang/String;)V");
      if (urisAvailableID == NULL)
        return;
    }

#if GTK_MINOR_VERSION > 4
  /* There is no real request_uris so we have to make one ourselves. */
  gdk_threads_enter ();
  uri_atom = gdk_atom_intern ("text/uri-list", FALSE);
  gtk_clipboard_request_contents (cp_gtk_clipboard,
				  uri_atom,
				  clipboard_uris_received,
				  (gpointer) selection_obj);
  gdk_threads_leave ();
#else
  clipboard_uris_received (cp_gtk_clipboard, NULL, (gpointer) selection_obj);
#endif
}

static jmethodID bytesAvailableID;

static void
clipboard_bytes_received (GtkClipboard *clipboard
			  __attribute__((unused)),
			  GtkSelectionData *selection_data,
			  gpointer selection)
{
  jbyteArray bytes = NULL;
  jobject selection_obj = (jobject) selection;
  JNIEnv *env = cp_gtk_gdk_env ();

   if (selection_data != NULL && selection_data->length > 0)
    {
      bytes = (*env)->NewByteArray (env, selection_data->length);
      if (bytes != NULL)
	(*env)->SetByteArrayRegion(env, bytes, 0, selection_data->length,
				   (jbyte *) selection_data->data);
    }

  (*env)->CallVoidMethod (env, selection_obj,
                          bytesAvailableID,
                          bytes);
  (*env)->DeleteGlobalRef (env, selection_obj);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkSelection_requestBytes (JNIEnv *env,
						      jobject obj,
						      jstring target_string)
{
  int len;
  const gchar *target_text;
  GdkAtom target_atom;
  jobject selection_obj;
  selection_obj = (*env)->NewGlobalRef(env, obj);
  if (selection_obj == NULL)
    return;

  if (bytesAvailableID == NULL)
    {
      jclass gtk_selection_class;
      gtk_selection_class = (*env)->GetObjectClass (env, selection_obj);
      bytesAvailableID = (*env)->GetMethodID (env, gtk_selection_class,
					      "bytesAvailable",
					      "([B)V");
      if (bytesAvailableID == NULL)
        return;
    }

  len = (*env)->GetStringUTFLength (env, target_string);
  if (len == -1)
    return;
  target_text = (*env)->GetStringUTFChars (env, target_string, NULL);
  if (target_text == NULL)
    return;

  gdk_threads_enter ();
  target_atom = gdk_atom_intern (target_text, FALSE);
  gtk_clipboard_request_contents (cp_gtk_clipboard,
                                  target_atom,
                                  clipboard_bytes_received,
                                  (gpointer) selection_obj);
  gdk_threads_leave ();

  (*env)->ReleaseStringUTFChars (env, target_string, target_text);
}
