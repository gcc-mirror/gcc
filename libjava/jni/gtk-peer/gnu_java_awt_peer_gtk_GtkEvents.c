/* gtkevents.c -- GDK/GTK event handlers
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
#include <X11/Xlib.h>
#include <gdk/gdkkeysyms.h>
#include <stdarg.h>
#include <assert.h>

/* A widget can be composed of multipled windows, so we need to hook
   events on all of them. */
struct event_hook_info
{
  jobject *peer_obj;
  int nwindows;
  /* array of pointers to (GdkWindow *) */
  GdkWindow ***windows;
};

static jint
button_to_awt_mods (int button)
{
  switch (button)
    {
    case 1:
      return AWT_BUTTON1_DOWN_MASK;
    case 2:
      return AWT_BUTTON2_DOWN_MASK;
    case 3:
      return AWT_BUTTON3_DOWN_MASK;
    }

  return 0;
}

static jint
state_to_awt_mods (guint state)
{
  jint result = 0;

  if (state & GDK_SHIFT_MASK)
    result |= AWT_SHIFT_DOWN_MASK;
  if (state & GDK_CONTROL_MASK)
    result |= AWT_CTRL_DOWN_MASK;
  if (state & GDK_MOD1_MASK)
    result |= AWT_ALT_DOWN_MASK;

  return result;
}

static jint
state_to_awt_mods_with_button_states (guint state)
{
  jint result = 0;

  if (state & GDK_SHIFT_MASK)
    result |= AWT_SHIFT_DOWN_MASK;
  if (state & GDK_CONTROL_MASK)
    result |= AWT_CTRL_DOWN_MASK;
  if (state & GDK_MOD1_MASK)
    result |= AWT_ALT_DOWN_MASK;
  if (state & GDK_BUTTON1_MASK)
    result |= AWT_BUTTON1_DOWN_MASK;
  if (state & GDK_BUTTON2_MASK)
    result |= AWT_BUTTON2_DOWN_MASK;
  if (state & GDK_BUTTON3_MASK)
    result |= AWT_BUTTON3_DOWN_MASK;

  return result;
}

/* Modifier key events need special treatment.  In Sun's peer
   implementation, when a modifier key is pressed, the KEY_PRESSED
   event has that modifier in its modifiers list.  The corresponding
   KEY_RELEASED event's modifier list does not contain the modifier.
   For example, pressing and releasing the shift key will produce a
   key press event with modifiers=Shift, and a key release event with
   no modifiers.  GDK's key events behave in the exact opposite way,
   so this translation code is needed. */
jint
keyevent_state_to_awt_mods (GdkEvent *event)
{
  jint result = 0;
  guint state;

  if (event->type == GDK_KEY_PRESS)
    {
      state = event->key.state;

      if (event->key.keyval == GDK_Shift_L
          || event->key.keyval == GDK_Shift_R)
        result |= AWT_SHIFT_DOWN_MASK;
      else
        {
          if (state & GDK_SHIFT_MASK)
            result |= AWT_SHIFT_DOWN_MASK;
        }

      if (event->key.keyval == GDK_Control_L
          || event->key.keyval == GDK_Control_R)
        result |= AWT_CTRL_DOWN_MASK;
      else
        {
          if (state & GDK_CONTROL_MASK)
            result |= AWT_CTRL_DOWN_MASK;
        }

      if (event->key.keyval == GDK_Alt_L
          || event->key.keyval == GDK_Alt_R)
        result |= AWT_ALT_DOWN_MASK;
      else
        {
          if (state & GDK_MOD1_MASK)
            result |= AWT_ALT_DOWN_MASK;
        }
    }
  else if (event->type == GDK_KEY_RELEASE)
    {
      state = event->key.state;

      if (event->key.keyval != GDK_Shift_L
          && event->key.keyval != GDK_Shift_R)
        {
          if (state & GDK_SHIFT_MASK)
            result |= AWT_SHIFT_DOWN_MASK;
        }
      if (event->key.keyval != GDK_Control_L
          && event->key.keyval != GDK_Control_R)
        {
          if (state & GDK_CONTROL_MASK)
            result |= AWT_CTRL_DOWN_MASK;
        }

      if (event->key.keyval != GDK_Alt_L
          && event->key.keyval != GDK_Alt_R)
        {
          if (state & GDK_MOD1_MASK)
            result |= AWT_ALT_DOWN_MASK;
        }
    }

  return result;
}

/* Get the first keyval in the keymap for this event's keycode.  The
   first keyval corresponds roughly to Java's notion of a virtual
   key.  Returns the uppercase version of the first keyval. */
static guint
get_first_keyval_from_keymap (GdkEvent *event)
{
  guint keyval;
  guint *keyvals;
  gint n_entries;

  if (!gdk_keymap_get_entries_for_keycode (NULL,
                                           event->key.hardware_keycode,
                                           NULL,
                                           &keyvals,
                                           &n_entries))
    {
      g_warning ("No keyval found for hardware keycode %d\n",
                 event->key.hardware_keycode);
      /* Try to recover by using the keyval in the event structure. */
      keyvals = &(event->key.keyval);
    }
  keyval = keyvals[0];
  g_free (keyvals);

  return gdk_keyval_to_upper (keyval);
}

#ifdef __GNUC__
__inline
#endif
static jint
keysym_to_awt_keycode (GdkEvent *event)
{
  guint ukeyval;
  guint state;

  ukeyval = get_first_keyval_from_keymap (event);
  state = event->key.state;

  /* VK_A through VK_Z */
  if (ukeyval >= GDK_A && ukeyval <= GDK_Z)
    return ukeyval;

  /* VK_0 through VK_9 */
  if (ukeyval >= GDK_0 && ukeyval <= GDK_9)
    return ukeyval;

  switch (ukeyval)
    {
    case GDK_Return:
    case GDK_KP_Enter:
      return VK_ENTER;
    case GDK_BackSpace:
      return VK_BACK_SPACE;
    case GDK_Tab:
      return VK_TAB;
    case GDK_Cancel:
      return VK_CANCEL;
    case GDK_Clear:
      return VK_CLEAR;
    case GDK_Shift_L:
    case GDK_Shift_R:
      return VK_SHIFT;
    case GDK_Control_L:
    case GDK_Control_R:
      return VK_CONTROL;
    case GDK_Alt_L:
    case GDK_Alt_R:
      return VK_ALT;
    case GDK_Pause:
      return VK_PAUSE;
    case GDK_Caps_Lock:
      return VK_CAPS_LOCK;
    case GDK_Escape:
      return VK_ESCAPE;
    case GDK_space:
      return VK_SPACE;
    case GDK_KP_Page_Up:
      /* For keys on the numeric keypad, the JVM produces one of two
         virtual keys, depending on the num lock state. */
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD9;
      else
        return VK_PAGE_UP;
    case GDK_Page_Up:
      return VK_PAGE_UP;
    case GDK_KP_Page_Down:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD3;
      else
        return VK_PAGE_DOWN;
    case GDK_Page_Down:
      return VK_PAGE_DOWN;
    case GDK_KP_End:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD1;
      else
        return VK_END;
    case GDK_End:
      return VK_END;
    case GDK_KP_Home:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD7;
      else
        return VK_HOME;
    case GDK_Home:
      return VK_HOME;
    case GDK_KP_Begin:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD5;
      else
        return VK_UNDEFINED;
    case GDK_Left:
      return VK_LEFT;
    case GDK_Up:
      return VK_UP;
    case GDK_Right:
      return VK_RIGHT;
    case GDK_Down:
      return VK_DOWN;
    case GDK_comma:
      return VK_COMMA;
    case GDK_minus:
      return VK_MINUS;
    case GDK_period:
      return VK_PERIOD;
    case GDK_slash:
      return VK_SLASH;
      /*
      return VK_0;
      return VK_1;
      return VK_2;
      return VK_3;
      return VK_4;
      return VK_5;
      return VK_6;
      return VK_7;
      return VK_8;
      return VK_9;
      */
    case GDK_semicolon:
      return VK_SEMICOLON;
    case GDK_equal:
      return VK_EQUALS;
      /*
      return VK_A;
      return VK_B;
      return VK_C;
      return VK_D;
      return VK_E;
      return VK_F;
      return VK_G;
      return VK_H;
      return VK_I;
      return VK_J;
      return VK_K;
      return VK_L;
      return VK_M;
      return VK_N;
      return VK_O;
      return VK_P;
      return VK_Q;
      return VK_R;
      return VK_S;
      return VK_T;
      return VK_U;
      return VK_V;
      return VK_W;
      return VK_X;
      return VK_Y;
      return VK_Z;
      */
    case GDK_bracketleft:
      return VK_OPEN_BRACKET;
    case GDK_backslash:
      return VK_BACK_SLASH;
    case GDK_bracketright:
      return VK_CLOSE_BRACKET;
    case GDK_KP_0:
      return VK_NUMPAD0;
    case GDK_KP_1:
      return VK_NUMPAD1;
    case GDK_KP_2:
      return VK_NUMPAD2;
    case GDK_KP_3:
      return VK_NUMPAD3;
    case GDK_KP_4:
      return VK_NUMPAD4;
    case GDK_KP_5:
      return VK_NUMPAD5;
    case GDK_KP_6:
      return VK_NUMPAD6;
    case GDK_KP_7:
      return VK_NUMPAD7;
    case GDK_KP_8:
      return VK_NUMPAD8;
    case GDK_KP_9:
      return VK_NUMPAD9;
    case GDK_KP_Multiply:
      return VK_MULTIPLY;
    case GDK_KP_Add:
      return VK_ADD;
      /*
      return VK_SEPARATER;
      */
    case GDK_KP_Separator:
      return VK_SEPARATOR;
    case GDK_KP_Subtract:
      return VK_SUBTRACT;
    case GDK_KP_Decimal:
      return VK_DECIMAL;
    case GDK_KP_Divide:
      return VK_DIVIDE;
    case GDK_KP_Delete:
      if (state & GDK_MOD2_MASK)
        return VK_DECIMAL;
      else
        return VK_DELETE;
    case GDK_Delete:
      return VK_DELETE;
    case GDK_Num_Lock:
      return VK_NUM_LOCK;
    case GDK_Scroll_Lock:
      return VK_SCROLL_LOCK;
    case GDK_F1:
      return VK_F1;
    case GDK_F2:
      return VK_F2;
    case GDK_F3:
      return VK_F3;
    case GDK_F4:
      return VK_F4;
    case GDK_F5:
      return VK_F5;
    case GDK_F6:
      return VK_F6;
    case GDK_F7:
      return VK_F7;
    case GDK_F8:
      return VK_F8;
    case GDK_F9:
      return VK_F9;
    case GDK_F10:
      return VK_F10;
    case GDK_F11:
      return VK_F11;
    case GDK_F12:
      return VK_F12;
    case GDK_F13:
      return VK_F13;
    case GDK_F14:
      return VK_F14;
    case GDK_F15:
      return VK_F15;
    case GDK_F16:
      return VK_F16;
    case GDK_F17:
      return VK_F17;
    case GDK_F18:
      return VK_F18;
    case GDK_F19:
      return VK_F19;
    case GDK_F20:
      return VK_F20;
    case GDK_F21:
      return VK_F21;
    case GDK_F22:
      return VK_F22;
    case GDK_F23:
      return VK_F23;
    case GDK_F24:
      return VK_F24;
    case GDK_Print:
      return VK_PRINTSCREEN;
    case GDK_KP_Insert:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD0;
      else
        return VK_INSERT;
    case GDK_Insert:
      return VK_INSERT;
    case GDK_Help:
      return VK_HELP;
    case GDK_Meta_L:
    case GDK_Meta_R:
      return VK_META;
    case GDK_grave:
      return VK_BACK_QUOTE;
    case GDK_apostrophe:
      return VK_QUOTE;
    case GDK_KP_Up:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD8;
      else
        return VK_KP_UP;
    case GDK_KP_Down:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD2;
      else
        return VK_KP_DOWN;
    case GDK_KP_Left:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD4;
      else
        return VK_KP_LEFT;
    case GDK_KP_Right:
      if (state & GDK_MOD2_MASK)
        return VK_NUMPAD6;
      else
        return VK_KP_RIGHT;
    case GDK_dead_grave:
      return VK_DEAD_GRAVE;
    case GDK_dead_acute:
      return VK_DEAD_ACUTE;
    case GDK_dead_circumflex:
      return VK_DEAD_CIRCUMFLEX;
    case GDK_dead_tilde:
      return VK_DEAD_TILDE;
    case GDK_dead_macron:
      return VK_DEAD_MACRON;
    case GDK_dead_breve:
      return VK_DEAD_BREVE;
    case GDK_dead_abovedot:
      return VK_DEAD_ABOVEDOT;
    case GDK_dead_diaeresis:
      return VK_DEAD_DIAERESIS;
    case GDK_dead_abovering:
      return VK_DEAD_ABOVERING;
    case GDK_dead_doubleacute:
      return VK_DEAD_DOUBLEACUTE;
    case GDK_dead_caron:
      return VK_DEAD_CARON;
    case GDK_dead_cedilla:
      return VK_DEAD_CEDILLA;
    case GDK_dead_ogonek:
      return VK_DEAD_OGONEK;
    case GDK_dead_iota:
      return VK_DEAD_IOTA;
    case GDK_dead_voiced_sound:
      return VK_DEAD_VOICED_SOUND;
    case GDK_dead_semivoiced_sound:
      return VK_DEAD_SEMIVOICED_SOUND;
    case GDK_ampersand:
      return VK_AMPERSAND;
    case GDK_asterisk:
      return VK_ASTERISK;
    case GDK_quotedbl:
      return VK_QUOTEDBL;
    case GDK_less:
      return VK_LESS;
    case GDK_greater:
      return VK_GREATER;
    case GDK_braceleft:
      return VK_BRACELEFT;
    case GDK_braceright:
      return VK_BRACERIGHT;
    case GDK_at:
      return VK_AT;
    case GDK_colon:
      return VK_COLON;
    case GDK_asciicircum:
      return VK_CIRCUMFLEX;
    case GDK_dollar:
      return VK_DOLLAR;
    case GDK_EuroSign:
      return VK_EURO_SIGN;
    case GDK_exclam:
      return VK_EXCLAMATION_MARK;
    case GDK_exclamdown:
      return VK_INVERTED_EXCLAMATION_MARK;
    case GDK_parenleft:
      return VK_LEFT_PARENTHESIS;
    case GDK_numbersign:
      return VK_NUMBER_SIGN;
    case GDK_plus:
      return VK_PLUS;
    case GDK_parenright:
      return VK_RIGHT_PARENTHESIS;
    case GDK_underscore:
      return VK_UNDERSCORE;
      /*
      return VK_FINAL;
      return VK_CONVERT;
      return VK_NONCONVERT;
      return VK_ACCEPT;
      */
    case GDK_Mode_switch:
      return VK_MODECHANGE;
      /*
      return VK_KANA;
      */
    case GDK_Kanji:
      return VK_KANJI;
      /*
      return VK_ALPHANUMERIC;
      */
    case GDK_Katakana:
      return VK_KATAKANA;
    case GDK_Hiragana:
      return VK_HIRAGANA;
      /*
      return VK_FULL_WIDTH;
      return VK_HALF_WIDTH;
      return VK_ROMAN_CHARACTERS;
      return VK_ALL_CANDIDATES;
      */
    case GDK_PreviousCandidate:
      return VK_PREVIOUS_CANDIDATE;
    case GDK_Codeinput:
      return VK_CODE_INPUT;
      /*
      return VK_JAPANESE_KATAKANA;
      return VK_JAPANESE_HIRAGANA;
      return VK_JAPANESE_ROMAN;
      */
    case GDK_Kana_Lock:
      return VK_KANA_LOCK;
      /*
      return VK_INPUT_METHOD_ON_OFF;
      return VK_CUT;
      return VK_COPY;
      return VK_PASTE;
      return VK_UNDO;
      return VK_AGAIN;
      return VK_FIND;
      return VK_PROPS;
      return VK_STOP;
      return VK_COMPOSE;
      return VK_ALT_GRAPH;
      */
    default:
      return VK_UNDEFINED;
    }
}

static jint
keysym_to_awt_keylocation (GdkEvent *event)
{
  guint ukeyval;

  ukeyval = get_first_keyval_from_keymap (event);

  /* VK_A through VK_Z */
  if (ukeyval >= GDK_A && ukeyval <= GDK_Z)
    return AWT_KEY_LOCATION_STANDARD;

  /* VK_0 through VK_9 */
  if (ukeyval >= GDK_0 && ukeyval <= GDK_9)
    return AWT_KEY_LOCATION_STANDARD;

  switch (ukeyval)
    {
    case GDK_Shift_L:
    case GDK_Control_L:
    case GDK_Alt_L:
    case GDK_Meta_L:
      return AWT_KEY_LOCATION_LEFT;

    case GDK_Shift_R:
    case GDK_Control_R:
    case GDK_Alt_R:
    case GDK_Meta_R:
      return AWT_KEY_LOCATION_RIGHT;

    case GDK_Return:
    case GDK_BackSpace:
    case GDK_Tab:
    case GDK_Cancel:
    case GDK_Clear:
    case GDK_Pause:
    case GDK_Caps_Lock:
    case GDK_Escape:
    case GDK_space:
    case GDK_Page_Up:
    case GDK_Page_Down:
    case GDK_End:
    case GDK_Home:
    case GDK_Left:
    case GDK_Up:
    case GDK_Right:
    case GDK_Down:
    case GDK_comma:
    case GDK_minus:
    case GDK_period:
    case GDK_slash:
    case GDK_semicolon:
    case GDK_equal:
    case GDK_bracketleft:
    case GDK_backslash:
    case GDK_bracketright:
    case GDK_Delete:
    case GDK_Scroll_Lock:
    case GDK_F1:
    case GDK_F2:
    case GDK_F3:
    case GDK_F4:
    case GDK_F5:
    case GDK_F6:
    case GDK_F7:
    case GDK_F8:
    case GDK_F9:
    case GDK_F10:
    case GDK_F11:
    case GDK_F12:
    case GDK_F13:
    case GDK_F14:
    case GDK_F15:
    case GDK_F16:
    case GDK_F17:
    case GDK_F18:
    case GDK_F19:
    case GDK_F20:
    case GDK_F21:
    case GDK_F22:
    case GDK_F23:
    case GDK_F24:
    case GDK_Print:
    case GDK_Insert:
    case GDK_Help:
    case GDK_grave:
    case GDK_apostrophe:
    case GDK_dead_grave:
    case GDK_dead_acute:
    case GDK_dead_circumflex:
    case GDK_dead_tilde:
    case GDK_dead_macron:
    case GDK_dead_breve:
    case GDK_dead_abovedot:
    case GDK_dead_diaeresis:
    case GDK_dead_abovering:
    case GDK_dead_doubleacute:
    case GDK_dead_caron:
    case GDK_dead_cedilla:
    case GDK_dead_ogonek:
    case GDK_dead_iota:
    case GDK_dead_voiced_sound:
    case GDK_dead_semivoiced_sound:
    case GDK_ampersand:
    case GDK_asterisk:
    case GDK_quotedbl:
    case GDK_less:
    case GDK_greater:
    case GDK_braceleft:
    case GDK_braceright:
    case GDK_at:
    case GDK_colon:
    case GDK_asciicircum:
    case GDK_dollar:
    case GDK_EuroSign:
    case GDK_exclam:
    case GDK_exclamdown:
    case GDK_parenleft:
    case GDK_numbersign:
    case GDK_plus:
    case GDK_parenright:
    case GDK_underscore:
    case GDK_Mode_switch:
    case GDK_Kanji:
    case GDK_Katakana:
    case GDK_Hiragana:
    case GDK_PreviousCandidate:
    case GDK_Codeinput:
    case GDK_Kana_Lock:
      return AWT_KEY_LOCATION_STANDARD;

    case GDK_KP_Enter:
    case GDK_KP_Page_Up:
    case GDK_KP_Page_Down:
    case GDK_KP_End:
    case GDK_KP_Home:
    case GDK_KP_Begin:
    case GDK_KP_0:
    case GDK_KP_1:
    case GDK_KP_2:
    case GDK_KP_3:
    case GDK_KP_4:
    case GDK_KP_5:
    case GDK_KP_6:
    case GDK_KP_7:
    case GDK_KP_8:
    case GDK_KP_9:
    case GDK_KP_Multiply:
    case GDK_KP_Add:
    case GDK_KP_Separator:
    case GDK_KP_Subtract:
    case GDK_KP_Decimal:
    case GDK_KP_Divide:
    case GDK_KP_Delete:
    case GDK_Num_Lock:
    case GDK_KP_Insert:
    case GDK_KP_Up:
    case GDK_KP_Down:
    case GDK_KP_Left:
    case GDK_KP_Right:
      return AWT_KEY_LOCATION_NUMPAD;

    default:
      return AWT_KEY_LOCATION_UNKNOWN;
    }
}

static jchar
keyevent_to_awt_keychar (GdkEvent *event)
{
  if (event->key.length > 0)
    {
      /* Translate GDK carriage return to Java linefeed. */
      if (event->key.string[0] == 13)
        return VK_ENTER;
      else
        return event->key.string[0];
    }
  else
    {
      switch (event->key.keyval)
        {
        case GDK_BackSpace:
          return VK_BACK_SPACE;
        case GDK_Tab:
          return VK_TAB;
        case GDK_Delete:
        case GDK_KP_Delete:
          return VK_DELETE;
        default:
          return AWT_KEY_CHAR_UNDEFINED;
        }
    }
}

void
awt_event_handler (GdkEvent *event)
{
  /* keep synthetic AWT events from being processed recursively */
  if (event->type & SYNTHETIC_EVENT_MASK && event->type != GDK_NOTHING)
    {
      event->type ^= SYNTHETIC_EVENT_MASK;
    }

  gtk_main_do_event (event);
}

gboolean
pre_event_handler (GtkWidget *widget, GdkEvent *event, jobject peer)
{
  GtkWidget *event_widget;
  static guint32 button_click_time = 0;
  static GdkWindow *button_window = NULL;
  static guint button_number = -1;
  static jint click_count = 1;
  static int hasBeenDragged;

  /* If it is not a focus change event, the widget must be realized already.
     If not, ignore the event (Gtk+ will do the same). */
  if (!(event->type == GDK_FOCUS_CHANGE || GTK_WIDGET_REALIZED(widget)))
    return FALSE;
    
  /* Do not handle propagated events.  AWT has its own propagation rules */
  gdk_window_get_user_data (event->any.window, (void **) &event_widget);
  if (event_widget != widget)
    return FALSE;

  /* We only care about input events */    
  if (!(event->type == GDK_BUTTON_PRESS
       || event->type == GDK_BUTTON_RELEASE
       || event->type == GDK_ENTER_NOTIFY
       || event->type == GDK_LEAVE_NOTIFY
       || event->type == GDK_CONFIGURE
       || event->type == GDK_EXPOSE
       || event->type == GDK_KEY_PRESS
       || event->type == GDK_KEY_RELEASE
       || event->type == GDK_FOCUS_CHANGE
       || event->type == GDK_MOTION_NOTIFY))
    {
      return FALSE;
    }
  /* g_print("event %u widget %s peer %p\n",
            event->type, gtk_widget_get_name (widget), peer); */

  /* If it has no jobject associated we can send no AWT event */
  if (!peer)
    return FALSE;

  /* for all input events, which have a window with a jobject attached,
     send the AWT input event corresponding to the Gtk event off to Java  */

  /* keep track of clickCount ourselves, since the AWT allows more
     than a triple click to occur */
  if (event->type == GDK_BUTTON_PRESS)
    {
      if ((event->button.time < (button_click_time + MULTI_CLICK_TIME))
	  && (event->button.window == button_window)
	  && (event->button.button == button_number))
	click_count++;
      else
	click_count = 1;
      
      button_click_time = event->button.time;
      button_window = event->button.window;
      button_number = event->button.button;
    }

  switch (event->type)
    {
    case GDK_BUTTON_PRESS:
      (*gdk_env)->CallVoidMethod (gdk_env, peer,
                                  postMouseEventID,
				  AWT_MOUSE_PRESSED, 
				  (jlong)event->button.time,
                                  state_to_awt_mods (event->button.state)
                                  | button_to_awt_mods (event->button.button),
				  (jint)event->button.x,
				  (jint)event->button.y, 
				  click_count, 
				  (event->button.button == 3) ? JNI_TRUE :
				                                JNI_FALSE);
      hasBeenDragged = FALSE;
      break;
    case GDK_BUTTON_RELEASE:
      {
	int width, height;

	(*gdk_env)->CallVoidMethod (gdk_env, peer,
				    postMouseEventID,
				    AWT_MOUSE_RELEASED, 
				    (jlong)event->button.time,
                                    state_to_awt_mods (event->button.state)
                                    | button_to_awt_mods (event->button.button),
				    (jint)event->button.x,
				    (jint)event->button.y, 
				    click_count,
				    JNI_FALSE);

	/* Generate an AWT click event only if the release occured in the
	   window it was pressed in, and the mouse has not been dragged since
	   the last time it was pressed. */
	gdk_window_get_size (event->any.window, &width, &height);
	if (! hasBeenDragged
	    && event->button.x >= 0
            && event->button.y >= 0
	    && event->button.x <= width 
	    && event->button.y <= height)
          {
	    (*gdk_env)->CallVoidMethod (gdk_env, peer,
				        postMouseEventID,
				        AWT_MOUSE_CLICKED, 
				        (jlong)event->button.time,
				        state_to_awt_mods (event->button.state)
                                        | button_to_awt_mods (event->button.button),
				        (jint)event->button.x,
				        (jint)event->button.y, 
				        click_count,
				        JNI_FALSE);
          }
      }
      break;
    case GDK_MOTION_NOTIFY:
      if (event->motion.state & (GDK_BUTTON1_MASK
				 | GDK_BUTTON2_MASK
				 | GDK_BUTTON3_MASK
				 | GDK_BUTTON4_MASK
				 | GDK_BUTTON5_MASK))
	{
	  (*gdk_env)->CallVoidMethod (gdk_env, peer,
			              postMouseEventID,
				      AWT_MOUSE_DRAGGED,
				      (jlong)event->motion.time,
				      state_to_awt_mods_with_button_states (event->motion.state),
				      (jint)event->motion.x,
				      (jint)event->motion.y,
				      0,
				      JNI_FALSE);
	  hasBeenDragged = TRUE;
	}
      else
        (*gdk_env)->CallVoidMethod (gdk_env, peer, postMouseEventID,
				    AWT_MOUSE_MOVED,
				    (jlong)event->motion.time,
				    state_to_awt_mods (event->motion.state),
				    (jint)event->motion.x,
				    (jint)event->motion.y,
				    0,
				    JNI_FALSE);
      break;
    case GDK_ENTER_NOTIFY:
      /* We are not interested in enter events that are due to
         grab/ungrab and not to actually crossing boundaries */
      if (event->crossing.mode == GDK_CROSSING_NORMAL)
        (*gdk_env)->CallVoidMethod (gdk_env, peer, postMouseEventID,
				    AWT_MOUSE_ENTERED, 
				    (jlong)event->crossing.time,
				    state_to_awt_mods_with_button_states (event->crossing.state), 
				    (jint)event->crossing.x,
				    (jint)event->crossing.y, 
				    0,
				    JNI_FALSE);
      break;
    case GDK_LEAVE_NOTIFY:
      /* We are not interested in leave events that are due to
         grab/ungrab and not to actually crossing boundaries */
      if (event->crossing.mode == GDK_CROSSING_NORMAL)
	(*gdk_env)->CallVoidMethod (gdk_env, peer,
				    postMouseEventID,
				    AWT_MOUSE_EXITED, 
				    (jlong)event->crossing.time,
				    state_to_awt_mods_with_button_states (event->crossing.state),
				    (jint)event->crossing.x,
				    (jint)event->crossing.y, 
				    0,
				    JNI_FALSE);
      break;
    case GDK_CONFIGURE:
      {
        /* Only send configure events to visible top-level windows. */
	if (widget && GTK_WIDGET_TOPLEVEL (widget)
            && GTK_WIDGET_VISIBLE (widget))
	  {
	    /* Configure events are not posted to the AWT event
	       queue, and as such, the gdk/gtk peer functions will
	       be called back before postConfigureEvent
	       returns. */
	    gdk_threads_leave ();

 	    (*gdk_env)->CallVoidMethod (gdk_env, peer,
					postConfigureEventID,
					(jint) event->configure.x,
					(jint) event->configure.y,
					(jint) event->configure.width,
					(jint) event->configure.height);
	    gdk_threads_enter ();
	  }
      }
      break;
    case GDK_EXPOSE:
      {
        /* This filters out unwanted feedback expose events from gtk/X
           when we explictly invalidate and update heavyweight components,
           thus avoiding an infinite loop.
           FIXME: I'm not quite sure why we're getting these expose events. 
                  Maybe there is a way to avoid them? */
        if((event->any.window == widget->window && event->any.send_event)
           || GTK_IS_LAYOUT(widget))
          {
	    (*gdk_env)->CallVoidMethod (gdk_env, peer,
				        postExposeEventID,
				        (jint)event->expose.area.x,
				        (jint)event->expose.area.y,
				        (jint)event->expose.area.width,
				        (jint)event->expose.area.height);
          }
      }
      break;

    case GDK_FOCUS_CHANGE:
      (*gdk_env)->CallVoidMethod (gdk_env, peer,
				  postFocusEventID,
				  (jint) (event->focus_change.in) ?
				  AWT_FOCUS_GAINED : AWT_FOCUS_LOST,
				  JNI_FALSE);
      break;
    case GDK_KEY_PRESS:
        if (GTK_IS_WINDOW (widget))
          {
            /*            GdkEventKey *keyevent = (GdkEventKey *) event; */
            /*            g_printerr ("key press event: sent: %d  time: %d  state: %d  keyval: %d  length: %d  string: %s  hardware_keycode: %d  group: %d\n", keyevent->send_event, keyevent->time, keyevent->state, keyevent->keyval, keyevent->length, keyevent->string, keyevent->hardware_keycode, keyevent->group); */

            (*gdk_env)->CallVoidMethod (gdk_env, peer,
                                        postKeyEventID,
                                        (jint) AWT_KEY_PRESSED,
                                        (jlong) event->key.time,
                                        keyevent_state_to_awt_mods (event),
                                        keysym_to_awt_keycode (event),
                                        keyevent_to_awt_keychar (event),
                                        keysym_to_awt_keylocation (event));
            /* FIXME: generation of key typed events needs to be moved
               to GtkComponentPeer.postKeyEvent.  If the key in a key
               press event is not an "action" key
               (KeyEvent.isActionKey) and is not a modifier key, then
               it should generate a key typed event. */
            return TRUE;
          }
        else
          return FALSE;
        break;
    case GDK_KEY_RELEASE:
      if (GTK_IS_WINDOW (widget))
        {
            (*gdk_env)->CallVoidMethod (gdk_env, peer,
                                        postKeyEventID,
                                        (jint) AWT_KEY_RELEASED,
                                        (jlong) event->key.time,
                                        keyevent_state_to_awt_mods (event),
                                        keysym_to_awt_keycode (event),
                                        keyevent_to_awt_keychar (event),
                                        keysym_to_awt_keylocation (event));
            return TRUE;
        }
      else
        return FALSE;
      break;
    default:
      break;
    }
  
  return FALSE;
}

static void
attach_jobject (GdkWindow *window, jobject *obj)
{
  GdkAtom addr_atom = gdk_atom_intern ("_GNU_GTKAWT_ADDR", FALSE);
  GdkAtom type_atom = gdk_atom_intern ("CARDINAL", FALSE);

  gdk_window_set_events (window, 
			 gdk_window_get_events (window)
			 | GDK_POINTER_MOTION_MASK
			 | GDK_BUTTON_MOTION_MASK
			 | GDK_BUTTON_PRESS_MASK
			 | GDK_BUTTON_RELEASE_MASK
			 | GDK_KEY_PRESS_MASK
			 | GDK_KEY_RELEASE_MASK
			 | GDK_ENTER_NOTIFY_MASK
			 | GDK_LEAVE_NOTIFY_MASK
			 | GDK_STRUCTURE_MASK
			 | GDK_KEY_PRESS_MASK
			 | GDK_FOCUS_CHANGE_MASK);

  gdk_property_change (window,
		       addr_atom,
		       type_atom,
		       8,
		       GDK_PROP_MODE_REPLACE,
		       (guchar *)obj,
		       sizeof (jobject));
}

void
connect_awt_hook (JNIEnv *env, jobject peer_obj, int nwindows, ...)
{
  va_list ap;
  jobject *obj;

  obj = NSA_GET_GLOBAL_REF (env, peer_obj);
  g_assert (obj);

  va_start (ap, nwindows);
  {
  int i;
  for (i = 0; i < nwindows; i++)
    {
      GdkWindow* attach = (va_arg (ap, GdkWindow *));
      attach_jobject(attach, obj);
    }
  }
  va_end (ap);
}

