/* gtkcheckboxpeer.c -- Native implementation of GtkCheckboxPeer
   Copyright (C) 1998, 1999, 2002, 2003, 2004, 2006
   Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkCheckboxPeer.h"
#include "gnu_java_awt_peer_gtk_GtkComponentPeer.h"
#include "jcl.h"

static jmethodID postItemEventID;
static jmethodID addToGroupMapID;
static GtkWidget *checkbox_get_widget (GtkWidget *widget);
static void item_toggled_cb (GtkToggleButton *item, jobject peer);

void
cp_gtk_checkbox_init_jni (void)
{
  jclass gtkcheckboxpeer;

  gtkcheckboxpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                             "gnu/java/awt/peer/gtk/GtkCheckboxPeer");

  postItemEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkcheckboxpeer,
                                               "postItemEvent", 
                                               "(Ljava/lang/Object;Z)V");
  
  addToGroupMapID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkcheckboxpeer,
                                               "addToGroupMap", 
                                               "(J)V");
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject gref;
  GtkWidget *bin;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  gref = gtkpeer_get_global_ref (env, obj);
  bin = checkbox_get_widget (GTK_WIDGET (ptr));

  /* Checkbox signals */
  g_signal_connect (G_OBJECT (bin), "toggled",
                    G_CALLBACK (item_toggled_cb), gref);

  /* Component signals */
  cp_gtk_component_connect_signals (G_OBJECT (bin), gref);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_gtkToggleButtonSetActive
  (JNIEnv *env, jobject obj, jboolean is_active)
{
  void *ptr;
  GtkWidget *bin;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  bin = checkbox_get_widget (GTK_WIDGET (ptr));
  
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (bin), is_active);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_gtkWidgetModifyFont
  (JNIEnv *env, jobject obj, jstring name, jint style, jint size)
{
  const char *font_name;
  void *ptr;
  GtkWidget *button;
  GtkWidget *label;
  PangoFontDescription *font_desc;

  gdk_threads_enter();

  ptr = gtkpeer_get_widget (env, obj);

  button = checkbox_get_widget (GTK_WIDGET (ptr));
  label = gtk_bin_get_child (GTK_BIN(button));

  if (!label)
    return;

  font_name = (*env)->GetStringUTFChars (env, name, NULL);

  font_desc = pango_font_description_from_string (font_name);
  pango_font_description_set_size (font_desc,
                                   size * cp_gtk_dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);
  
  gtk_widget_modify_font (GTK_WIDGET(label), font_desc);
  
  pango_font_description_free (font_desc);
  
  (*env)->ReleaseStringUTFChars (env, name, font_name);
  
  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_gtkButtonSetLabel
  (JNIEnv *env, jobject obj, jstring label)
{
  const char *c_label;
  GtkWidget *label_widget;
  void *ptr;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);

  c_label = (*env)->GetStringUTFChars (env, label, NULL);

  label_widget = gtk_bin_get_child (GTK_BIN (checkbox_get_widget (GTK_WIDGET (ptr))));
  gtk_label_set_text (GTK_LABEL (label_widget), c_label);

  (*env)->ReleaseStringUTFChars (env, label, c_label);

  gdk_threads_leave ();
}

/* A check button is created if we are not part of 
   a group. 
   This function is called when initially creating the
   button, so an eventbox is created.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_createCheckButton 
  (JNIEnv *env, jobject obj)
{
  GtkWidget *button;
  GtkWidget *eventbox;

  gdk_threads_enter ();

  gtkpeer_set_global_ref (env, obj);
  eventbox = gtk_event_box_new ();

  button = gtk_check_button_new_with_label ("");
  gtk_container_add (GTK_CONTAINER (eventbox), button);
  gtk_widget_show (button); 

  gtkpeer_set_widget (env, obj, eventbox);

  gdk_threads_leave ();
}

/* A radio button is created if we are part of a group. 
   groupPointer points to the corresponding group. If 0,
   a new group is created.
   This function is called when initially creating the
   button, so an eventbox is created.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_createRadioButton 
  (JNIEnv *env, jobject obj, jlong groupPointer)
{
  GtkWidget *button;
  GtkWidget *eventbox;
  GSList *native_group = NULL;
  
  gdk_threads_enter ();

  gtkpeer_set_global_ref (env, obj);
  eventbox = gtk_event_box_new ();

  if (groupPointer != 0)
  {
    native_group = JLONG_TO_PTR (GSList, groupPointer);
    g_assert (GTK_IS_RADIO_BUTTON (native_group->data));
  }
  button = gtk_radio_button_new_with_label (native_group, "");
  
  if (native_group == NULL)
    native_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (button));
  if (g_slist_index (native_group, GTK_RADIO_BUTTON (button)) == -1)
  {
    native_group = g_slist_prepend (native_group, GTK_RADIO_BUTTON (button));
    GTK_RADIO_BUTTON(button)->group = native_group;  
  }
  
  gtk_container_add (GTK_CONTAINER (eventbox), button);
  gtk_widget_show (button);
  
  gtkpeer_set_widget (env, obj, eventbox);
  
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj,
                                addToGroupMapID,
                                PTR_TO_JLONG (native_group));
  
  gdk_threads_leave ();
}

/* Add the object to the group pointed to by groupPointer.
   If groupPointer is 0, create a new group and create
   a radio button. Otherwise, creating a radio button in an
   existing group.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_addToGroup 
  (JNIEnv *env, jobject obj, jlong groupPointer)
{
  void *ptr;
  GtkWidget *container;
  GtkWidget *check_button;
  GtkWidget *radio_button;
  const gchar *label;
  GSList *native_group = NULL;

  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  container = GTK_WIDGET (ptr);
  check_button = checkbox_get_widget (container);
  label = gtk_label_get_text (GTK_LABEL (gtk_bin_get_child 
                                        (GTK_BIN (check_button))));
                                        
  /* Need to remove the check_button, and replace it with 
     a radio button in a group.
   */
  if (groupPointer != 0)
    {
      native_group = JLONG_TO_PTR (GSList, groupPointer);
      g_assert (GTK_IS_RADIO_BUTTON (native_group->data));
    }
      
  radio_button = gtk_radio_button_new_with_label (native_group, label);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radio_button), 
             gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (check_button)));
  
  if (native_group == NULL)
    native_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radio_button));
  if (g_slist_index (native_group, GTK_RADIO_BUTTON (radio_button)) == -1)
  {
    native_group = g_slist_prepend (native_group, GTK_RADIO_BUTTON (radio_button));
    GTK_RADIO_BUTTON(radio_button)->group = native_group;
  }
             
  gtk_container_remove (GTK_CONTAINER (container), check_button);
  gtk_container_add (GTK_CONTAINER (container), radio_button);
  gtk_widget_show (radio_button);
  
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj,
                                addToGroupMapID,
                                PTR_TO_JLONG (native_group));
  
  gdk_threads_leave ();
}

/* Remove the object from the group pointed to by groupPointer.
   We are removing the radio button and creating a check button. 
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_removeFromGroup 
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  GtkWidget *container;
  GtkWidget *check_button;
  GtkWidget *radio_button;
  GSList *native_group;
  const gchar *label;
    
  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  container = GTK_WIDGET (ptr);
  radio_button = checkbox_get_widget (container);
  label = gtk_label_get_text (GTK_LABEL (gtk_bin_get_child 
                                        (GTK_BIN (radio_button))));
                                        
  /* Need to remove the radio_button, and replace it with 
     a check button.
   */   
  check_button = gtk_check_button_new_with_label (label);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (check_button), 
             gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radio_button))); 
             
  native_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radio_button));
  native_group = g_slist_remove (native_group, GTK_RADIO_BUTTON (radio_button));
  
  if (native_group && ! GTK_IS_RADIO_BUTTON (native_group->data))
    native_group = NULL;
  
  GTK_RADIO_BUTTON(radio_button)->group = NULL;
  
  gtk_container_remove (GTK_CONTAINER (container), radio_button);  
  gtk_container_add (GTK_CONTAINER (container), check_button);
  gtk_widget_show (check_button);
  
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj,
                                addToGroupMapID,
                                PTR_TO_JLONG (native_group));
  
  gdk_threads_leave ();
}

/* Move the radio button to a new group. If groupPointer is
   0, create a new group.
 */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkCheckboxPeer_switchToGroup 
  (JNIEnv *env, jobject obj, jlong groupPointer)
{
  void *ptr;
  GtkWidget *radio_button;
  GSList *native_group = NULL;
  
  gdk_threads_enter ();

  ptr = gtkpeer_get_widget (env, obj);
  radio_button = checkbox_get_widget (GTK_WIDGET (ptr));
  
  native_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radio_button));
  native_group = g_slist_remove (native_group, GTK_RADIO_BUTTON (radio_button));
  GTK_RADIO_BUTTON(radio_button)->group = NULL;
  
  if (groupPointer != 0)
  {
    native_group = JLONG_TO_PTR (GSList, groupPointer);
    g_assert (GTK_IS_RADIO_BUTTON (native_group->data));
  }
  gtk_radio_button_set_group (GTK_RADIO_BUTTON (radio_button), native_group);
  
  native_group = gtk_radio_button_get_group (GTK_RADIO_BUTTON (radio_button));
  if (g_slist_index (native_group, GTK_RADIO_BUTTON (radio_button)) == -1)
  {
    native_group = g_slist_prepend (native_group, GTK_RADIO_BUTTON (radio_button));
    GTK_RADIO_BUTTON(radio_button)->group = native_group;
  }
   
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), obj,
                                addToGroupMapID,
                                PTR_TO_JLONG (native_group));
  
  gdk_threads_leave ();
}

static void
item_toggled_cb (GtkToggleButton *item, jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postItemEventID,
                                peer,
                                item->active);
}

static GtkWidget *
checkbox_get_widget (GtkWidget *widget)
{
  GtkWidget *wid;

  g_assert (GTK_IS_EVENT_BOX (widget));
  wid = gtk_bin_get_child (GTK_BIN(widget));

  return wid;
}

