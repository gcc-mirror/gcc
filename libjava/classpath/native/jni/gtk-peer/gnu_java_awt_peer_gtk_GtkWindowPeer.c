/* gtkwindowpeer.c -- Native implementation of GtkWindowPeer
   Copyright (C) 1998, 1999, 2002, 2004, 2005 Free Software Foundation, Inc.

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
#include "gnu_java_awt_peer_gtk_GtkWindowPeer.h"
#include <gdk/gdkprivate.h>
#include <gdk/gdkx.h>
#include <X11/Xatom.h>
#include <gdk/gdkkeysyms.h>

#define AWT_WINDOW_OPENED 200
#define AWT_WINDOW_CLOSING 201
#define AWT_WINDOW_CLOSED 202
#define AWT_WINDOW_ICONIFIED 203
#define AWT_WINDOW_DEICONIFIED 204
#define AWT_WINDOW_ACTIVATED 205
#define AWT_WINDOW_DEACTIVATED 206
#define AWT_WINDOW_GAINED_FOCUS 207
#define AWT_WINDOW_LOST_FOCUS 208
#define AWT_WINDOW_STATE_CHANGED 209

/* Virtual Keys */
/* This list should be kept in the same order as the VK_ field
   declarations in KeyEvent.java. */
#define VK_ENTER '\n'
#define VK_BACK_SPACE '\b'
#define VK_TAB '\t'
#define VK_CANCEL 3
#define VK_CLEAR 12
#define VK_SHIFT 16
#define VK_CONTROL 17
#define VK_ALT 18
#define VK_PAUSE 19
#define VK_CAPS_LOCK 20
#define VK_ESCAPE 27
#define VK_SPACE ' '
#define VK_PAGE_UP 33
#define VK_PAGE_DOWN 34
#define VK_END 35
#define VK_HOME 36
#define VK_LEFT 37
#define VK_UP 38
#define VK_RIGHT 39
#define VK_DOWN 40
#define VK_COMMA ','
#define VK_MINUS '-'
#define VK_PERIOD '.'
#define VK_SLASH '/'
#define VK_0 '0'
#define VK_1 '1'
#define VK_2 '2'
#define VK_3 '3'
#define VK_4 '4'
#define VK_5 '5'
#define VK_6 '6'
#define VK_7 '7'
#define VK_8 '8'
#define VK_9 '9'
#define VK_SEMICOLON ';'
#define VK_EQUALS '='
#define VK_A 'A'
#define VK_B 'B'
#define VK_C 'C'
#define VK_D 'D'
#define VK_E 'E'
#define VK_F 'F'
#define VK_G 'G'
#define VK_H 'H'
#define VK_I 'I'
#define VK_J 'J'
#define VK_K 'K'
#define VK_L 'L'
#define VK_M 'M'
#define VK_N 'N'
#define VK_O 'O'
#define VK_P 'P'
#define VK_Q 'Q'
#define VK_R 'R'
#define VK_S 'S'
#define VK_T 'T'
#define VK_U 'U'
#define VK_V 'V'
#define VK_W 'W'
#define VK_X 'X'
#define VK_Y 'Y'
#define VK_Z 'Z'
#define VK_OPEN_BRACKET '['
#define VK_BACK_SLASH '\\'
#define VK_CLOSE_BRACKET ']'
/* See gtkpeer.h */
/* #define VK_NUMPAD0 96 */
/* #define VK_NUMPAD1 97 */
/* #define VK_NUMPAD2 98 */
/* #define VK_NUMPAD3 99 */
/* #define VK_NUMPAD4 100 */
/* #define VK_NUMPAD5 101 */
/* #define VK_NUMPAD6 102 */
/* #define VK_NUMPAD7 103 */
/* #define VK_NUMPAD8 104 */
/* #define VK_NUMPAD9 105 */
#define VK_MULTIPLY 106
#define VK_ADD 107
#define VK_SEPARATER 108
#define VK_SEPARATOR 108
#define VK_SUBTRACT 109
/* See gtkpeer.h */
/* #define VK_DECIMAL 110 */
#define VK_DIVIDE 111
#define VK_DELETE 127
#define VK_NUM_LOCK 144
#define VK_SCROLL_LOCK 145
#define VK_F1 112
#define VK_F2 113
#define VK_F3 114
#define VK_F4 115
#define VK_F5 116
#define VK_F6 117
#define VK_F7 118
#define VK_F8 119
#define VK_F9 120
#define VK_F10 121
#define VK_F11 122
#define VK_F12 123
#define VK_F13 61440
#define VK_F14 61441
#define VK_F15 61442
#define VK_F16 61443
#define VK_F17 61444
#define VK_F18 61445
#define VK_F19 61446
#define VK_F20 61447
#define VK_F21 61448
#define VK_F22 61449
#define VK_F23 61450
#define VK_F24 61451
#define VK_PRINTSCREEN 154
#define VK_INSERT 155
#define VK_HELP 156
#define VK_META 157
#define VK_BACK_QUOTE 192
#define VK_QUOTE 222
#define VK_KP_UP 224
#define VK_KP_DOWN 225
#define VK_KP_LEFT 226
#define VK_KP_RIGHT 227
#define VK_DEAD_GRAVE 128
#define VK_DEAD_ACUTE 129
#define VK_DEAD_CIRCUMFLEX 130
#define VK_DEAD_TILDE 131
#define VK_DEAD_MACRON 132
#define VK_DEAD_BREVE 133
#define VK_DEAD_ABOVEDOT 134
#define VK_DEAD_DIAERESIS 135
#define VK_DEAD_ABOVERING 136
#define VK_DEAD_DOUBLEACUTE 137
#define VK_DEAD_CARON 138
#define VK_DEAD_CEDILLA 139
#define VK_DEAD_OGONEK 140
#define VK_DEAD_IOTA 141
#define VK_DEAD_VOICED_SOUND 142
#define VK_DEAD_SEMIVOICED_SOUND 143
#define VK_AMPERSAND 150
#define VK_ASTERISK 151
#define VK_QUOTEDBL 152
#define VK_LESS 153
#define VK_GREATER 160
#define VK_BRACELEFT 161
#define VK_BRACERIGHT 162
#define VK_AT 512
#define VK_COLON 513
#define VK_CIRCUMFLEX 514
#define VK_DOLLAR 515
#define VK_EURO_SIGN 516
#define VK_EXCLAMATION_MARK 517
#define VK_INVERTED_EXCLAMATION_MARK 518
#define VK_LEFT_PARENTHESIS 519
#define VK_NUMBER_SIGN 520
#define VK_PLUS 521
#define VK_RIGHT_PARENTHESIS 522
#define VK_UNDERSCORE 523
#define VK_FINAL 24
#define VK_CONVERT 28
#define VK_NONCONVERT 29
#define VK_ACCEPT 30
#define VK_MODECHANGE 31
#define VK_KANA 21
#define VK_KANJI 25
#define VK_ALPHANUMERIC 240
#define VK_KATAKANA 241
#define VK_HIRAGANA 242
#define VK_FULL_WIDTH 243
#define VK_HALF_WIDTH 244
#define VK_ROMAN_CHARACTERS 245
#define VK_ALL_CANDIDATES 256
#define VK_PREVIOUS_CANDIDATE 257
#define VK_CODE_INPUT 258
#define VK_JAPANESE_KATAKANA 259
#define VK_JAPANESE_HIRAGANA 260
#define VK_JAPANESE_ROMAN 261
#define VK_KANA_LOCK 262
#define VK_INPUT_METHOD_ON_OFF 263
#define VK_CUT 65489
#define VK_COPY 65485
#define VK_PASTE 65487
#define VK_UNDO 65483
#define VK_AGAIN 65481
#define VK_FIND 65488
#define VK_PROPS 65482
#define VK_STOP 65480
#define VK_COMPOSE 65312
#define VK_ALT_GRAPH 65406
#define VK_UNDEFINED 0

#define AWT_KEY_CHAR_UNDEFINED 0

#define AWT_FRAME_STATE_NORMAL 0
#define AWT_FRAME_STATE_ICONIFIED 1
#define AWT_FRAME_STATE_MAXIMIZED_HORIZ 2
#define AWT_FRAME_STATE_MAXIMIZED_VERT 4
#define AWT_FRAME_STATE_MAXIMIZED_BOTH 6

static jmethodID postKeyEventID;
static jmethodID postWindowEventID;
static jmethodID postConfigureEventID;
static jmethodID postInsetsChangedEventID;
static jmethodID windowGetWidthID;
static jmethodID windowGetHeightID;

void
cp_gtk_window_init_jni (void)
{
  jclass gtkwindowpeer;

  gtkwindowpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                           "gnu/java/awt/peer/gtk/GtkWindowPeer");

  postKeyEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkwindowpeer,
                                              "postKeyEvent", "(IJIICI)V");

  postWindowEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkwindowpeer,
                                                 "postWindowEvent",
                                                 "(ILjava/awt/Window;I)V");

  postConfigureEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkwindowpeer, 
                                                    "postConfigureEvent", "(IIII)V");

  postInsetsChangedEventID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkwindowpeer,
                                                        "postInsetsChangedEvent",
                                                        "(IIII)V");

  windowGetWidthID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkwindowpeer,
                                                "getWidth", "()I");

  windowGetHeightID = (*cp_gtk_gdk_env())->GetMethodID (cp_gtk_gdk_env(), gtkwindowpeer,
                                                 "getHeight", "()I");

  gtkwindowpeer = (*cp_gtk_gdk_env())->FindClass (cp_gtk_gdk_env(),
                                           "gnu/java/awt/peer/gtk/GtkWindowPeer");
}

/* Get the first keyval in the keymap for this event's keycode.  The
   first keyval corresponds roughly to Java's notion of a virtual key.
   Returns the uppercase version of the first keyval or -1 if no
   keyval was found for the given hardware keycode. */
static gint
get_first_keyval_from_keymap (GdkEventKey *event)
{
  guint keyval;
  guint *keyvals;
  gint n_entries;

  if (!gdk_keymap_get_entries_for_keycode (NULL,
                                           event->hardware_keycode,
                                           NULL,
                                           &keyvals,
                                           &n_entries))
    {
      /* No keyval found for hardware keycode */
      return -1;
    }
  keyval = keyvals[0];
  g_free (keyvals);

  return gdk_keyval_to_upper (keyval);
}

/* Return the AWT key code for the given keysym or -1 if no keyval was
   found for the given hardware keycode. */
#ifdef __GNUC__
__inline
#endif
static jint
keysym_to_awt_keycode (GdkEventKey *event)
{
  gint ukeyval;
  guint state;

  ukeyval = get_first_keyval_from_keymap (event);

  if (ukeyval < 0)
    return -1;

  state = event->state;

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

/* Return the AWT key location code for the given keysym or -1 if no
   keyval was found for the given hardware keycode. */
static jint
keysym_to_awt_keylocation (GdkEventKey *event)
{
  gint ukeyval;

  ukeyval = get_first_keyval_from_keymap (event);

  if (ukeyval < 0)
    return -1;

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
keyevent_to_awt_keychar (GdkEventKey *event)
{
  if (event->length > 0)
    {
      /* Translate GDK carriage return to Java linefeed. */
      if (event->string[0] == 13)
        return VK_ENTER;
      else
        return event->string[0];
    }
  else
    {
      switch (event->keyval)
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

/* Modifier key events need special treatment.  In Sun's peer
   implementation, when a modifier key is pressed, the KEY_PRESSED
   event has that modifier in its modifiers list.  The corresponding
   KEY_RELEASED event's modifier list does not contain the modifier.
   For example, pressing and releasing the shift key will produce a
   key press event with modifiers=Shift, and a key release event with
   no modifiers.  GDK's key events behave in the exact opposite way,
   so this translation code is needed. */
static jint
keyevent_state_to_awt_mods (GdkEventKey *event)
{
  jint result = 0;
  guint state;

  if (event->type == GDK_KEY_PRESS)
    {
      state = event->state;

      if (event->keyval == GDK_Shift_L
          || event->keyval == GDK_Shift_R)
        result |= AWT_SHIFT_DOWN_MASK | AWT_SHIFT_MASK;
      else
        {
          if (state & GDK_SHIFT_MASK)
            result |= AWT_SHIFT_DOWN_MASK | AWT_SHIFT_MASK;
        }

      if (event->keyval == GDK_Control_L
          || event->keyval == GDK_Control_R)
        result |= AWT_CTRL_DOWN_MASK | AWT_CTRL_MASK;
      else
        {
          if (state & GDK_CONTROL_MASK)
            result |= AWT_CTRL_DOWN_MASK | AWT_CTRL_MASK;
        }

      if (event->keyval == GDK_Alt_L
          || event->keyval == GDK_Alt_R)
        result |= AWT_ALT_DOWN_MASK | AWT_ALT_MASK;
      else
        {
          if (state & GDK_MOD1_MASK)
            result |= AWT_ALT_DOWN_MASK | AWT_ALT_MASK;
        }
    }
  else if (event->type == GDK_KEY_RELEASE)
    {
      state = event->state;

      if (event->keyval != GDK_Shift_L
          && event->keyval != GDK_Shift_R)
        {
          if (state & GDK_SHIFT_MASK)
            result |= AWT_SHIFT_DOWN_MASK | AWT_SHIFT_MASK;
        }
      if (event->keyval != GDK_Control_L
          && event->keyval != GDK_Control_R)
        {
          if (state & GDK_CONTROL_MASK)
            result |= AWT_CTRL_DOWN_MASK | AWT_CTRL_MASK;
        }

      if (event->keyval != GDK_Alt_L
          && event->keyval != GDK_Alt_R)
        {
          if (state & GDK_MOD1_MASK)
            result |= AWT_ALT_DOWN_MASK | AWT_ALT_MASK;
        }
    }

  return result;
}

static gboolean window_configure_cb (GtkWidget *widget,
                                     GdkEventConfigure *event,
                                     jobject peer);

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

static gboolean window_delete_cb (GtkWidget *widget, GdkEvent *event,
			      jobject peer);
static void window_destroy_cb (GtkWidget *widget, GdkEvent *event,
			       jobject peer);
static void window_show_cb (GtkWidget *widget, jobject peer);
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
static gboolean window_property_changed_cb (GtkWidget *widget,
					    GdkEventProperty *event,
					    jobject peer);
static void realize_cb (GtkWidget *widget, jobject peer);

static gboolean
window_configure_cb (GtkWidget *widget __attribute__((unused)),
                     GdkEventConfigure *event,
                     jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postConfigureEventID,
                                (jint) event->x,
                                (jint) event->y,
                                (jint) event->width,
                                (jint) event->height);

  return FALSE;
}

static gboolean
key_press_cb (GtkWidget *widget __attribute__((unused)),
              GdkEventKey *event,
              jobject peer)
{
  jint keycode;
  jint keylocation;

  keycode = keysym_to_awt_keycode (event);
  keylocation = keysym_to_awt_keylocation (event);

  /* Return immediately if an error occurs translating a hardware
     keycode to a keyval. */
  if (keycode < 0 || keylocation < 0)
    return TRUE;

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postKeyEventID,
                                (jint) AWT_KEY_PRESSED,
                                (jlong) event->time,
                                keyevent_state_to_awt_mods (event),
                                keycode,
                                keyevent_to_awt_keychar (event),
                                keylocation);

  /* FIXME: generation of key typed events needs to be moved
     to GtkComponentPeer.postKeyEvent.  If the key in a key
     press event is not an "action" key
     (KeyEvent.isActionKey) and is not a modifier key, then
     it should generate a key typed event. */
  return TRUE;
}


static gboolean
key_release_cb (GtkWidget *widget __attribute__((unused)),
                GdkEventKey *event,
                jobject peer)
{
  jint keycode;
  jint keylocation;

  keycode = keysym_to_awt_keycode (event);
  keylocation = keysym_to_awt_keylocation (event);

  /* Return immediately if an error occurs translating a hardware
     keycode to a keyval. */
  if (keycode < 0 || keylocation < 0)
    return TRUE;

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postKeyEventID,
                                (jint) AWT_KEY_RELEASED,
                                (jlong) event->time,
                                keyevent_state_to_awt_mods (event),
                                keycode,
                                keyevent_to_awt_keychar (event),
                                keylocation);

  return TRUE;
}

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

  gdk_threads_enter ();
  
  NSA_SET_GLOBAL_REF (env, obj);

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

  gtk_window_group_add_window (cp_gtk_global_window_group, window);

  fixed = gtk_fixed_new ();

  gtk_container_add (GTK_CONTAINER (window_widget), fixed);

  gtk_widget_show (fixed);

  NSA_SET_PTR (env, obj, window_widget);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_gtkWindowSetTitle
  (JNIEnv *env, jobject obj, jstring title)
{
  const char *c_title;
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  c_title = (*env)->GetStringUTFChars (env, title, NULL);

  gtk_window_set_title (GTK_WINDOW (ptr), c_title);

  (*env)->ReleaseStringUTFChars (env, title, c_title);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_gtkWindowSetResizable
  (JNIEnv *env, jobject obj, jboolean resizable)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  gtk_window_set_resizable (GTK_WINDOW (ptr), resizable);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_gtkWindowSetModal
  (JNIEnv *env, jobject obj, jboolean modal)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  gtk_window_set_modal (GTK_WINDOW (ptr), modal);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setVisibleNative
  (JNIEnv *env, jobject obj, jboolean visible)
{
  gdk_threads_enter ();

  Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setVisibleNativeUnlocked
    (env, obj, visible);

  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setVisibleNativeUnlocked
  (JNIEnv *env, jobject obj, jboolean visible)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  if (visible)
    gtk_widget_show (GTK_WIDGET (ptr));
  else
    gtk_widget_hide (GTK_WIDGET (ptr));
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_connectSignals
  (JNIEnv *env, jobject obj)
{
  void *ptr;
  jobject *gref;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
  gref = NSA_GET_GLOBAL_REF (env, obj);

  g_signal_connect (G_OBJECT (ptr), "delete-event",
		    G_CALLBACK (window_delete_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "destroy-event",
		    G_CALLBACK (window_destroy_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "show",
		    G_CALLBACK (window_show_cb), *gref);

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

  g_signal_connect (G_OBJECT (ptr), "key-press-event",
                    G_CALLBACK (key_press_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "key-release-event",
                    G_CALLBACK (key_release_cb), *gref);

  g_signal_connect_after (G_OBJECT (ptr), "window-state-event",
                          G_CALLBACK (window_window_state_cb), *gref);

  g_signal_connect (G_OBJECT (ptr), "configure-event",
                    G_CALLBACK (window_configure_cb), *gref);

  cp_gtk_component_connect_expose_signals (ptr, gref);
  cp_gtk_component_connect_mouse_signals (ptr, gref);

  /* FIXME: override focus signals here to prevent child fixed repaint? */

  gdk_threads_leave ();
}

/* Realize the window here so that its frame extents are known now.
   That way Window.pack can operate with the accurate insets returned
   by the window manager rather than the default estimates. */
JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_realize (JNIEnv *env, jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  gtk_widget_realize (GTK_WIDGET (ptr));

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_toBack (JNIEnv *env, 
    jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
    
  gdk_window_lower (GTK_WIDGET (ptr)->window);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL 
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_toFront (JNIEnv *env, 
    jobject obj)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);
    
  gdk_window_raise (GTK_WIDGET (ptr)->window);
  gdk_flush ();

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_setSize
  (JNIEnv *env, jobject obj, jint width, jint height)
{
  void *ptr;

  gdk_threads_enter ();

  ptr = NSA_GET_PTR (env, obj);

  /* Avoid GTK runtime assertion failures. */
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

  gtk_widget_set_size_request (GTK_WIDGET(ptr), width, height);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetBounds
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  gdk_threads_enter ();

  Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetBoundsUnlocked
    (env, obj, x, y, width, height);

  gdk_threads_leave ();
}

JNIEXPORT void JNICALL
Java_gnu_java_awt_peer_gtk_GtkWindowPeer_nativeSetBoundsUnlocked
  (JNIEnv *env, jobject obj, jint x, jint y, jint width, jint height)
{
  void *ptr;

  ptr = NSA_GET_PTR (env, obj);

  /* Avoid GTK runtime assertion failures. */
  width = (width < 1) ? 1 : width;
  height = (height < 1) ? 1 : height;

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
}

static void
window_get_frame_extents (GtkWidget *window,
                          int *top, int *left, int *bottom, int *right)
{
  unsigned long *extents = NULL;
  union extents_union gu_ex;

  /* Guess frame extents in case _NET_FRAME_EXTENTS is not
     supported. */
  if (gtk_window_get_decorated (GTK_WINDOW (window)))
    {
      *top = 23;
      *left = 6;
      *bottom = 6;
      *right = 6;
    }
  else
    {
      *top = 0;
      *left = 0;
      *bottom = 0;
      *right = 0;
    }

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

static gboolean
window_delete_cb (GtkWidget *widget __attribute__((unused)),
		  GdkEvent *event __attribute__((unused)),
		  jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_CLOSING,
			      (jobject) NULL, (jint) 0);

  /* Prevents that the Window dissappears ("destroy"
     not being signalled). This is necessary because it
     should be up to a WindowListener implementation
     how the AWT Frame responds to close requests. */
  return TRUE;
}

static void
window_destroy_cb (GtkWidget *widget __attribute__((unused)),
		   GdkEvent *event __attribute__((unused)),
		   jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_CLOSED,
			      (jobject) NULL, (jint) 0);
}

static void
window_show_cb (GtkWidget *widget __attribute__((unused)),
		jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_OPENED,
			      (jobject) NULL, (jint) 0);
}

static void
window_focus_state_change_cb (GtkWidget *widget,
			      GParamSpec *pspec __attribute__((unused)),
			      jobject peer)
{
  if (GTK_WINDOW (widget)->has_toplevel_focus)
    (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postWindowEventID,
                                (jint) AWT_WINDOW_ACTIVATED,
                                (jobject) NULL, (jint) 0);
  else
    (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                                postWindowEventID,
                                (jint) AWT_WINDOW_DEACTIVATED,
                                (jobject) NULL, (jint) 0);
}

static gboolean
window_focus_in_cb (GtkWidget * widget  __attribute__((unused)),
		    GdkEventFocus *event  __attribute__((unused)),
		    jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                              postWindowEventID,
                              (jint) AWT_WINDOW_GAINED_FOCUS,
                              (jobject) NULL, (jint) 0);

  return FALSE;
}

static gboolean
window_focus_out_cb (GtkWidget * widget __attribute__((unused)),
		     GdkEventFocus *event __attribute__((unused)),
		     jobject peer)
{
  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
                              postWindowEventID,
                              (jint) AWT_WINDOW_LOST_FOCUS,
                              (jobject) NULL, (jint) 0);

  return FALSE;
}

static gboolean
window_window_state_cb (GtkWidget *widget __attribute__((unused)),
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
	  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
				      postWindowEventID,
				      (jint) AWT_WINDOW_ICONIFIED,
				      (jobject) NULL, (jint) 0);
	}
      else
	{
	  /* We've been deiconified. */
	  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
				      postWindowEventID,
				      (jint) AWT_WINDOW_DEICONIFIED,
				      (jobject) NULL, (jint) 0);
	}
    }

  /* Post a WINDOW_STATE_CHANGED event, passing the new frame state to
     GtkWindowPeer. */
  new_state = AWT_FRAME_STATE_NORMAL;

  if (event->window_state.new_window_state & GDK_WINDOW_STATE_ICONIFIED)
    new_state |= AWT_FRAME_STATE_ICONIFIED;

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
			      postWindowEventID,
			      (jint) AWT_WINDOW_STATE_CHANGED,
			      (jobject) NULL, new_state);

  return TRUE;
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
      (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
				    postInsetsChangedEventID,
				    (jint) extents[2],  /* top */
				    (jint) extents[0],  /* left */
				    (jint) extents[3],  /* bottom */
				    (jint) extents[1]); /* right */
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

  width = (*cp_gtk_gdk_env())->CallIntMethod (cp_gtk_gdk_env(), peer, windowGetWidthID);
  height = (*cp_gtk_gdk_env())->CallIntMethod (cp_gtk_gdk_env(), peer, windowGetHeightID);

  window_get_frame_extents (widget, &top, &left, &bottom, &right);

  (*cp_gtk_gdk_env())->CallVoidMethod (cp_gtk_gdk_env(), peer,
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
guint
cp_gtk_awt_keycode_to_keysym (jint keyCode, jint keyLocation)
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
