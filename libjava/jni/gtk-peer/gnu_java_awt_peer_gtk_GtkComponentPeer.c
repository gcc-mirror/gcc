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
#include <gdk/gdkkeysyms.h>

static GtkWidget *find_fg_color_widget (GtkWidget *widget);
static GtkWidget *find_bg_color_widget (GtkWidget *widget);
static gboolean focus_in_cb (GtkWidget *widget,
                             GdkEventFocus *event,
                             jobject peer);
static gboolean focus_out_cb (GtkWidget *widget,
                              GdkEventFocus *event,
                              jobject peer);
/*
 * This method returns a GDK keyval that corresponds to one of the
 * keysyms in the X keymap table.  The return value is only used to
 * determine the keyval's corresponding hardware keycode, and doesn't
 * reflect an accurate translation of a Java virtual key value to a
 * GDK keyval.
 */
#ifdef __GNUC__
__inline
#endif
static guint
awt_keycode_to_keysym (jint keyCode, jint keyLocation)
{
  /* GDK_A through GDK_Z */
  if (keyCode >= VK_A && keyCode <= VK_Z)
    return gdk_keyval_to_lower (keyCode);

  /* GDK_0 through GDK_9 */
  if (keyCode >= VK_0 && keyCode <= VK_9)
    return keyCode;

  switch (keyCode)
    {
    case VK_ENTER:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_Enter : GDK_Return;
    case VK_BACK_SPACE:
      return GDK_BackSpace;
    case VK_TAB:
      return GDK_Tab;
    case VK_CANCEL:
      return GDK_Cancel;
    case VK_CLEAR:
      return GDK_Clear;
    case VK_SHIFT:
      return keyLocation == AWT_KEY_LOCATION_LEFT ? GDK_Shift_L : GDK_Shift_R;
    case VK_CONTROL:
      return keyLocation == AWT_KEY_LOCATION_LEFT ? GDK_Control_L : GDK_Control_R;
    case VK_ALT:
      return keyLocation == AWT_KEY_LOCATION_LEFT ? GDK_Alt_L : GDK_Alt_R;
    case VK_PAUSE:
      return GDK_Pause;
    case VK_CAPS_LOCK:
      return GDK_Caps_Lock;
    case VK_ESCAPE:
      return GDK_Escape;
    case VK_SPACE:
      return GDK_space;
    case VK_PAGE_UP:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_Page_Up : GDK_Page_Up;
    case VK_PAGE_DOWN:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_Page_Down : GDK_Page_Down;
    case VK_END:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_End : GDK_End;
    case VK_HOME:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_Home : GDK_Home;
    case VK_LEFT:
      return GDK_Left;
    case VK_UP:
      return GDK_Up;
    case VK_RIGHT:
      return GDK_Right;
    case VK_DOWN:
      return GDK_Down;
    case VK_COMMA:
      return GDK_comma;
    case VK_MINUS:
      return GDK_minus;
    case VK_PERIOD:
      return GDK_period;
    case VK_SLASH:
      return GDK_slash;
      /*
    case VK_0:
    case VK_1:
    case VK_2:
    case VK_3:
    case VK_4:
    case VK_5:
    case VK_6:
    case VK_7:
    case VK_8:
    case VK_9:
      */
    case VK_SEMICOLON:
      return GDK_semicolon;
    case VK_EQUALS:
      return GDK_equal;
      /*
    case VK_A:
    case VK_B:
    case VK_C:
    case VK_D:
    case VK_E:
    case VK_F:
    case VK_G:
    case VK_H:
    case VK_I:
    case VK_J:
    case VK_K:
    case VK_L:
    case VK_M:
    case VK_N:
    case VK_O:
    case VK_P:
    case VK_Q:
    case VK_R:
    case VK_S:
    case VK_T:
    case VK_U:
    case VK_V:
    case VK_W:
    case VK_X:
    case VK_Y:
    case VK_Z:
      */
    case VK_OPEN_BRACKET:
      return GDK_bracketleft;
    case VK_BACK_SLASH:
      return GDK_backslash;
    case VK_CLOSE_BRACKET:
      return GDK_bracketright;
    case VK_NUMPAD0:
      return GDK_KP_0;
    case VK_NUMPAD1:
      return GDK_KP_1;
    case VK_NUMPAD2:
      return GDK_KP_2;
    case VK_NUMPAD3:
      return GDK_KP_3;
    case VK_NUMPAD4:
      return GDK_KP_4;
    case VK_NUMPAD5:
      return GDK_KP_5;
    case VK_NUMPAD6:
      return GDK_KP_6;
    case VK_NUMPAD7:
      return GDK_KP_7;
    case VK_NUMPAD8:
      return GDK_KP_8;
    case VK_NUMPAD9:
      return GDK_KP_9;
    case VK_MULTIPLY:
      return GDK_KP_Multiply;
    case VK_ADD:
      return GDK_KP_Add;
      /*
    case VK_SEPARATER:
      */
    case VK_SEPARATOR:
      return GDK_KP_Separator;
    case VK_SUBTRACT:
      return GDK_KP_Subtract;
    case VK_DECIMAL:
      return GDK_KP_Decimal;
    case VK_DIVIDE:
      return GDK_KP_Divide;
    case VK_DELETE:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_Delete : GDK_Delete;
    case VK_NUM_LOCK:
      return GDK_Num_Lock;
    case VK_SCROLL_LOCK:
      return GDK_Scroll_Lock;
    case VK_F1:
      return GDK_F1;
    case VK_F2:
      return GDK_F2;
    case VK_F3:
      return GDK_F3;
    case VK_F4:
      return GDK_F4;
    case VK_F5:
      return GDK_F5;
    case VK_F6:
      return GDK_F6;
    case VK_F7:
      return GDK_F7;
    case VK_F8:
      return GDK_F8;
    case VK_F9:
      return GDK_F9;
    case VK_F10:
      return GDK_F10;
    case VK_F11:
      return GDK_F11;
    case VK_F12:
      return GDK_F12;
    case VK_F13:
      return GDK_F13;
    case VK_F14:
      return GDK_F14;
    case VK_F15:
      return GDK_F15;
    case VK_F16:
      return GDK_F16;
    case VK_F17:
      return GDK_F17;
    case VK_F18:
      return GDK_F18;
    case VK_F19:
      return GDK_F19;
    case VK_F20:
      return GDK_F20;
    case VK_F21:
      return GDK_F21;
    case VK_F22:
      return GDK_F22;
    case VK_F23:
      return GDK_F23;
    case VK_F24:
      return GDK_F24;
    case VK_PRINTSCREEN:
      return GDK_Print;
    case VK_INSERT:
      return keyLocation == AWT_KEY_LOCATION_NUMPAD ? GDK_KP_Insert : GDK_Insert;
    case VK_HELP:
      return GDK_Help;
    case VK_META:
      return keyLocation == AWT_KEY_LOCATION_LEFT ? GDK_Meta_L : GDK_Meta_R;
    case VK_BACK_QUOTE:
      return GDK_grave;
    case VK_QUOTE:
      return GDK_apostrophe;
    case VK_KP_UP:
      return GDK_KP_Up;
    case VK_KP_DOWN:
      return GDK_KP_Down;
    case VK_KP_LEFT:
      return GDK_KP_Left;
    case VK_KP_RIGHT:
      return GDK_KP_Right;
    case VK_DEAD_GRAVE:
      return GDK_dead_grave;
    case VK_DEAD_ACUTE:
      return GDK_dead_acute;
    case VK_DEAD_CIRCUMFLEX:
      return GDK_dead_circumflex;
    case VK_DEAD_TILDE:
      return GDK_dead_tilde;
    case VK_DEAD_MACRON:
      return GDK_dead_macron;
    case VK_DEAD_BREVE:
      return GDK_dead_breve;
    case VK_DEAD_ABOVEDOT:
      return GDK_dead_abovedot;
    case VK_DEAD_DIAERESIS:
      return GDK_dead_diaeresis;
    case VK_DEAD_ABOVERING:
      return GDK_dead_abovering;
    case VK_DEAD_DOUBLEACUTE:
      return GDK_dead_doubleacute;
    case VK_DEAD_CARON:
      return GDK_dead_caron;
    case VK_DEAD_CEDILLA:
      return GDK_dead_cedilla;
    case VK_DEAD_OGONEK:
      return GDK_dead_ogonek;
    case VK_DEAD_IOTA:
      return GDK_dead_iota;
    case VK_DEAD_VOICED_SOUND:
      return GDK_dead_voiced_sound;
    case VK_DEAD_SEMIVOICED_SOUND:
      return GDK_dead_semivoiced_sound;
    case VK_AMPERSAND:
      return GDK_ampersand;
    case VK_ASTERISK:
      return GDK_asterisk;
    case VK_QUOTEDBL:
      return GDK_quotedbl;
    case VK_LESS:
      return GDK_less;
    case VK_GREATER:
      return GDK_greater;
    case VK_BRACELEFT:
      return GDK_braceleft;
    case VK_BRACERIGHT:
      return GDK_braceright;
    case VK_AT:
      return GDK_at;
    case VK_COLON:
      return GDK_colon;
    case VK_CIRCUMFLEX:
      return GDK_asciicircum;
    case VK_DOLLAR:
      return GDK_dollar;
    case VK_EURO_SIGN:
      return GDK_EuroSign;
    case VK_EXCLAMATION_MARK:
      return GDK_exclam;
    case VK_INVERTED_EXCLAMATION_MARK:
      return GDK_exclamdown;
    case VK_LEFT_PARENTHESIS:
      return GDK_parenleft;
    case VK_NUMBER_SIGN:
      return GDK_numbersign;
    case VK_PLUS:
      return GDK_plus;
    case VK_RIGHT_PARENTHESIS:
      return GDK_parenright;
    case VK_UNDERSCORE:
      return GDK_underscore;
      /*
    case VK_FINAL:
    case VK_CONVERT:
    case VK_NONCONVERT:
    case VK_ACCEPT:
      */
    case VK_MODECHANGE:
      return GDK_Mode_switch;
      /*
    case VK_KANA:
      */
    case VK_KANJI:
      return GDK_Kanji;
      /*
    case VK_ALPHANUMERIC:
      */
    case VK_KATAKANA:
      return GDK_Katakana;
    case VK_HIRAGANA:
      return GDK_Hiragana;
      /*
    case VK_FULL_WIDTH:
    case VK_HALF_WIDTH:
    case VK_ROMAN_CHARACTERS:
    case VK_ALL_CANDIDATES:
      */
    case VK_PREVIOUS_CANDIDATE:
      return GDK_PreviousCandidate;
    case VK_CODE_INPUT:
      return GDK_Codeinput;
      /*
    case VK_JAPANESE_KATAKANA:
    case VK_JAPANESE_HIRAGANA:
    case VK_JAPANESE_ROMAN:
      */
    case VK_KANA_LOCK:
      return GDK_Kana_Lock;
      /*
    case VK_INPUT_METHOD_ON_OFF:
    case VK_CUT:
    case VK_COPY:
    case VK_PASTE:
    case VK_UNDO:
    case VK_AGAIN:
    case VK_FIND:
    case VK_PROPS:
    case VK_STOP:
    case VK_COMPOSE:
    case VK_ALT_GRAPH:
      */
    default:
      return GDK_VoidSymbol;
    }
}


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

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetParent
  (JNIEnv *env, jobject obj, jobject parent)
{
  void *ptr;
  void *parent_ptr;
  GtkWidget *widget;
  GtkWidget *parent_widget;

  ptr = NSA_GET_PTR (env, obj);
  parent_ptr = NSA_GET_PTR (env, parent);

  gdk_threads_enter ();

  widget = GTK_WIDGET (ptr);
  parent_widget = GTK_WIDGET (parent_ptr);

  if (GTK_IS_WINDOW (parent_widget))
    {
      GList *children = gtk_container_children 
        (GTK_CONTAINER (GTK_BIN (parent_widget)->child));

      if (GTK_IS_MENU_BAR (children->data))
	gtk_layout_put (GTK_LAYOUT (children->next->data), widget, 0, 0);
      else
	gtk_layout_put (GTK_LAYOUT (children->data), widget, 0, 0);
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
      gtk_layout_put (GTK_LAYOUT (parent_widget), widget, 0, 0);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetSetSensitive
  (JNIEnv *env, jobject obj, jboolean sensitive)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

  gtk_widget_set_sensitive (GTK_WIDGET (ptr), sensitive);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_gtkWidgetRequestFocus
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);
  
  gdk_threads_enter ();
  gtk_widget_grab_focus (GTK_WIDGET (ptr));
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

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter ();

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
    event->key.window = GTK_BUTTON (ptr)->event_window;
  else if (GTK_IS_SCROLLED_WINDOW (ptr))
    event->key.window = GTK_WIDGET (GTK_SCROLLED_WINDOW (ptr)->container.child)->window;
  else
    event->key.window = GTK_WIDGET (ptr)->window;

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

  lookup_keyval = awt_keycode_to_keysym (keyCode, keyLocation);

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
      if (GTK_IS_SCROLLED_WINDOW (ptr))
        gtk_widget_event (GTK_WIDGET (GTK_SCROLLED_WINDOW (ptr)->container.child), event);
      else
        gtk_widget_event (GTK_WIDGET (ptr), event);
    }

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

  /* Widgets that extend GtkWindow such as GtkFileChooserDialog may have
     a default size.  These values seem more useful then the natural
     requisition values, particularly for GtkFileChooserDialog. */
  if (GTK_IS_WINDOW (ptr))
    {
      gint width, height;
      gtk_window_get_default_size (GTK_WINDOW (ptr), &width, &height);

      dims[0] = width;
      dims[1] = height;
    }
  else
    {
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
    }

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

  /* We assume that -1 is a width or height and not a request for the
     widget's natural size. */
  width = width < 0 ? 0 : width;
  height = height < 0 ? 0 : height;

  if (GTK_IS_VIEWPORT (widget->parent))
    gtk_widget_set_size_request (widget, width, height);
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
  pango_font_description_set_size (font_desc, size * dpi_conversion_factor);

  if (style & AWT_STYLE_BOLD)
    pango_font_description_set_weight (font_desc, PANGO_WEIGHT_BOLD);

  if (style & AWT_STYLE_ITALIC)
    pango_font_description_set_style (font_desc, PANGO_STYLE_OBLIQUE);

  gtk_widget_modify_font (GTK_WIDGET(ptr), font_desc);

  pango_font_description_free (font_desc);

  gdk_threads_leave();

  (*env)->ReleaseStringUTFChars (env, name, font_name);
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_show
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter();
  gtk_widget_show (GTK_WIDGET (ptr));
  gdk_threads_leave();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkComponentPeer_hide
  (JNIEnv *env, jobject obj)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  gdk_threads_enter();
  gtk_widget_hide (GTK_WIDGET (ptr));
  gdk_threads_leave();
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

static gboolean
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
  gulong hid;

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
  else if (GTK_IS_SCROLLED_WINDOW(ptr))
    {
      /* The event will go to the parent GtkLayout. */
      filterobj = GTK_OBJECT(GTK_WIDGET(ptr)->parent);
    }
  else
    {
      filterobj = GTK_OBJECT(ptr);
    }
  hid = g_signal_handler_find(filterobj,
                              G_SIGNAL_MATCH_FUNC,
                              0, 0, NULL, *pre_event_handler, NULL);
  if (hid > 0)
  {
    g_signal_handler_block(filterobj, hid);
  }
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
  gulong hid;

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
  else if (GTK_IS_SCROLLED_WINDOW(ptr))
    {
      /* The event will go to the parent GtkLayout. */
      filterobj = GTK_OBJECT(GTK_WIDGET(ptr)->parent);
    }
  else
    {
      filterobj = GTK_OBJECT(ptr);
    }

  g_signal_handlers_disconnect_by_func (filterobj,
                                        *filter_expose_event_handler, *gref);
  hid = g_signal_handler_find(filterobj,
                              G_SIGNAL_MATCH_FUNC,
                              0, 0, NULL, *pre_event_handler, NULL);
  if (hid > 0)
  {
    g_signal_handler_unblock(filterobj, hid);
  }

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

  g_signal_connect (G_OBJECT (ptr), "focus-in-event",
                    G_CALLBACK (focus_in_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "focus-out-event",
                    G_CALLBACK (focus_out_cb), *gref);

  gdk_threads_leave ();
}

static GtkWidget *
find_fg_color_widget (GtkWidget *widget)
{
  GtkWidget *fg_color_widget;

  if (GTK_IS_EVENT_BOX (widget)
      || (GTK_IS_BUTTON (widget)
          && !GTK_IS_OPTION_MENU (widget)))
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

static gboolean
focus_in_cb (GtkWidget *widget __attribute((unused)),
             GdkEventFocus *event __attribute((unused)),
             jobject peer)
{
  (*gdk_env)->CallVoidMethod (gdk_env, peer,
                              postFocusEventID,
                              AWT_FOCUS_GAINED,
                              JNI_FALSE);
  return FALSE;
}

static gboolean
focus_out_cb (GtkWidget *widget __attribute((unused)),
              GdkEventFocus *event __attribute((unused)),
              jobject peer)
{
  (*gdk_env)->CallVoidMethod (gdk_env, peer,
                              postFocusEventID,
                              AWT_FOCUS_LOST,
                              JNI_FALSE);
  return FALSE;
}
