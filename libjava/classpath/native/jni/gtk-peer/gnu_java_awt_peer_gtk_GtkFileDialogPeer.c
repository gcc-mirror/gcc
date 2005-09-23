/* gtkfiledialogpeer.c -- Native implementation of GtkFileDialogPeer
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
#include "gnu_java_awt_peer_gtk_GtkFileDialogPeer.h"

#define AWT_FILEDIALOG_LOAD 0
#define AWT_FILEDIALOG_SAVE 1

static void handle_response_cb (GtkDialog *dialog,
                                gint responseId,
                                jobject peer_obj);

static jmethodID gtkSetFilenameID;
static jmethodID gtkHideFileDialogID;
static jmethodID gtkDisposeFileDialogID;
static jmethodID filenameFilterCallbackID;

void
cp_gtk_filedialog_init_jni (void)
{
  jclass gtkfiledialogpeer;

  gtkfiledialogpeer =
    (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                    "gnu/java/awt/peer/gtk/GtkFileDialogPeer");

  gtkDisposeFileDialogID =
    (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                      gtkfiledialogpeer,
                                      "gtkDisposeFileDialog", "()V");

  gtkHideFileDialogID =
    (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                      gtkfiledialogpeer,
                                      "gtkHideFileDialog", "()V");

  gtkSetFilenameID =
    (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                      gtkfiledialogpeer,
                                      "gtkSetFilename",
                                      "(Ljava/lang/String;)V");

  filenameFilterCallbackID =
    (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(),
                                      gtkfiledialogpeer,
                                      "filenameFilterCallback",
                                      "(Ljava/lang/String;)Z");
}

/*
 * Make a new file selection dialog
 */

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkFileDialogPeer_create 
  (JNIEnv *env, jobject obj, jobject parent, int mode)
{
  void *parentp;
  gpointer widget;

  gdk_threads_enter ();
  
  /* Create global reference and save it for future use */
  NSA_SET_GLOBAL_REF (env, obj);

  parentp = NSA_GET_PTR(env, parent);

  if (mode == AWT_FILEDIALOG_LOAD)
    widget = gtk_file_chooser_dialog_new
      ("Open File",
       GTK_WINDOW(parentp),
       GTK_FILE_CHOOSER_ACTION_OPEN,
       GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
       GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
       NULL);
  else
    {
      widget = gtk_file_chooser_dialog_new
        ("Save File",
         GTK_WINDOW(parentp),
         GTK_FILE_CHOOSER_ACTION_SAVE,
         GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
         GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT,
         NULL);
#if GTK_MINOR_VERSION >= 8
      gtk_file_chooser_set_do_overwrite_confirmation
        (GTK_FILE_CHOOSER (widget), TRUE);
#endif
    }


  /* GtkFileChooserDialog is not modal by default */
  gtk_window_set_modal (GTK_WINDOW (widget), TRUE);

  /* We must add this window to the group so input in the others are
     disable while it is being shown */
  gtk_window_group_add_window (cp_gtk_global_window_group,
                               GTK_WINDOW (widget));

  NSA_SET_PTR (env, obj, widget);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkFileDialogPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr = NULL;
  jobject *gref = NULL;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  gref = NSA_GET_GLOBAL_REF (env, obj);

  /* FileDialog signals */
  g_signal_connect (G_OBJECT (ptr), "response",
		    G_CALLBACK (handle_response_cb), *gref);

  /* Component signals */
  cp_gtk_component_connect_signals (G_OBJECT (ptr), gref);

  gdk_threads_leave ();
}

JNIEXPORT jstring JNICALL 
Java_gnu_java_awt_peer_gtk_GtkFileDialogPeer_nativeGetDirectory
    (JNIEnv *env, jobject obj)
{
  void *ptr;
  const char *str;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  str = gtk_file_chooser_get_current_folder (GTK_FILE_CHOOSER(ptr));

  gdk_threads_leave ();

  return (*env)->NewStringUTF(env, str);
}


/* This function interfaces with the Java callback method of the same name.
   This function extracts the filename from the GtkFileFilterInfo object,
   and passes it to the Java method.  The Java method will call the filter's
   accept() method and will give back the return value. */
static gboolean filename_filter_cb (const GtkFileFilterInfo *filter_info,
					gpointer obj)
{
  jstring *filename;
  gboolean accepted;

  filename = (*cp_gtk_gdk_env())->NewStringUTF(cp_gtk_gdk_env(), filter_info->filename);

  accepted = (*cp_gtk_gdk_env())->CallBooleanMethod(cp_gtk_gdk_env(), obj,
                                                    filenameFilterCallbackID,
                                                    filename);

  return accepted;
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkFileDialogPeer_nativeSetFilenameFilter
    (JNIEnv *env, jobject obj, jobject filter_obj __attribute__((unused)))
{
  void *ptr;
  GtkFileFilter *filter;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  filter = gtk_file_filter_new();
  gtk_file_filter_add_custom(filter, GTK_FILE_FILTER_FILENAME,
			     filename_filter_cb, obj, NULL);

  gtk_file_chooser_set_filter(GTK_FILE_CHOOSER(ptr), filter);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkFileDialogPeer_nativeSetDirectory
    (JNIEnv *env, jobject obj, jstring directory)
{
  void *ptr;
  const char *str;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  str = (*env)->GetStringUTFChars (env, directory, 0);

  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER(ptr), str);

  (*env)->ReleaseStringUTFChars (env, directory, str);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkFileDialogPeer_nativeSetFile 
    (JNIEnv *env, jobject obj, jstring filename)
{
  void *ptr;
  const char *str;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
    
  str = (*env)->GetStringUTFChars (env, filename, 0);
     
  gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (ptr), str);

  (*env)->ReleaseStringUTFChars (env, filename, str);

  gdk_threads_leave ();
}

static void
handle_response_cb (GtkDialog *dialog __attribute__((unused)),
                    gint responseId,
                    jobject peer_obj)
{
  void *ptr;
  G_CONST_RETURN gchar *fileName;
  jstring str_fileName = NULL;

  /* We only need this for the case when the user closed the window,
     or clicked ok or cancel. */
  if (responseId != GTK_RESPONSE_DELETE_EVENT
      && responseId != GTK_RESPONSE_ACCEPT
      && responseId != GTK_RESPONSE_CANCEL)
    return;

  ptr = NSA_GET_PTR (cp_gtk_gdk_env(), peer_obj);

  if (responseId == GTK_RESPONSE_DELETE_EVENT)
  {
    /* We can dispose of the dialog now (and unblock show) */
    (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer_obj,
                                         gtkDisposeFileDialogID);

    return;
  }

  if (responseId == GTK_RESPONSE_ACCEPT)
    {
      fileName = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (GTK_WIDGET (ptr)));
      str_fileName = (*cp_gtk_gdk_env())->NewStringUTF (cp_gtk_gdk_env(), fileName);
    }

  /* Set the Java object field 'file' with this value. */
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer_obj,
                                       gtkSetFilenameID, str_fileName);

  /* We can hide the dialog now (and unblock show) */
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer_obj,
                                       gtkHideFileDialogID);
}
