/* gtkpeer.h -- Some global variables and #defines
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


#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <config.h>
#include "native_state.h"

#include <jni.h>

#define RC_FILE      ".classpath-gtkrc"
#define JVM_SUN
/*
  #define JVM_JAPHAR
*/

#ifndef __GTKPEER_H__
#define __GTKPEER_H__

#ifndef __GNUC__
#define __attribute__(x) /* nothing */
#endif

#ifdef JVM_SUN

extern struct state_table *native_state_table;
extern struct state_table *native_global_ref_table;

#define NSA_INIT(env, clazz) \
   do {native_state_table = init_state_table (env, clazz); \
   native_global_ref_table = init_state_table (env, clazz);} while (0)

#define NSA_GET_PTR(env, obj) \
  get_state (env, obj, native_state_table)

#define NSA_SET_PTR(env, obj, ptr) \
  set_state (env, obj, native_state_table, (void *)ptr)

#define NSA_DEL_PTR(env, obj) \
  remove_state_slot (env, obj, native_state_table)

#define NSA_GET_GLOBAL_REF(env, obj) \
  get_state (env, obj, native_global_ref_table)

#define NSA_SET_GLOBAL_REF(env, obj) \
  do {jobject *globRefPtr; \
    globRefPtr = (jobject *) malloc (sizeof (jobject)); \
    *globRefPtr = (*env)->NewGlobalRef (env, obj); \
    set_state (env, obj, native_global_ref_table, (void *)globRefPtr);} while (0)

#define NSA_DEL_GLOBAL_REF(env, obj) \
  do {jobject *globRefPtr = get_state (env, obj, native_global_ref_table); \
    remove_state_slot (env, obj, native_global_ref_table); \
    (*env)->DeleteGlobalRef (env, *globRefPtr); \
    free (globRefPtr);} while (0)

#endif /* JVM_SUN */

struct graphics
{
  GdkDrawable *drawable;
  GdkGC *gc;
  GdkColormap *cm;
  jint x_offset, y_offset;
};

#define AWT_DEFAULT_CURSOR 0
#define AWT_CROSSHAIR_CURSOR 1
#define AWT_TEXT_CURSOR 2
#define AWT_WAIT_CURSOR 3
#define AWT_SW_RESIZE_CURSOR 4
#define AWT_SE_RESIZE_CURSOR 5
#define AWT_NW_RESIZE_CURSOR 6
#define AWT_NE_RESIZE_CURSOR 7
#define AWT_N_RESIZE_CURSOR 8
#define AWT_S_RESIZE_CURSOR 9
#define AWT_W_RESIZE_CURSOR 10
#define AWT_E_RESIZE_CURSOR 11
#define AWT_HAND_CURSOR 12
#define AWT_MOVE_CURSOR 13

#define SYNTHETIC_EVENT_MASK (1 << 10)

#define AWT_SHIFT_MASK   (1 << 0)
#define AWT_CTRL_MASK    (1 << 1)
#define AWT_META_MASK    (1 << 2)
#define AWT_ALT_MASK     (1 << 3)

#define AWT_BUTTON1_MASK (1 << 4)
#define AWT_BUTTON2_MASK AWT_ALT_MASK
#define AWT_BUTTON3_MASK AWT_META_MASK

#define MULTI_CLICK_TIME   250
/* as opposed to a MULTI_PASS_TIME :) */

#define AWT_MOUSE_CLICKED  500
#define AWT_MOUSE_PRESSED  501
#define AWT_MOUSE_RELEASED 502
#define AWT_MOUSE_MOVED    503
#define AWT_MOUSE_ENTERED  504
#define AWT_MOUSE_EXITED   505
#define AWT_MOUSE_DRAGGED  506

#define AWT_ADJUSTMENT_UNIT_INCREMENT 1
#define AWT_ADJUSTMENT_UNIT_DECREMENT 2
#define AWT_ADJUSTMENT_BLOCK_DECREMENT 3
#define AWT_ADJUSTMENT_BLOCK_INCREMENT 4
#define AWT_ADJUSTMENT_TRACK 5

#define AWT_SCROLLPANE_SCROLLBARS_AS_NEEDED 0
#define AWT_SCROLLPANE_SCROLLBARS_ALWAYS 1
#define AWT_SCROLLPANE_SCROLLBARS_NEVER 2

#define AWT_LABEL_LEFT 0
#define AWT_LABEL_CENTER 1
#define AWT_LABEL_RIGHT 2

#define AWT_TEXTAREA_SCROLLBARS_BOTH 0
#define AWT_TEXTAREA_SCROLLBARS_VERTICAL_ONLY 1
#define AWT_TEXTAREA_SCROLLBARS_HORIZONTAL_ONLY 2

#define AWT_ITEM_SELECTED 1
#define AWT_ITEM_DESELECTED 2
     
#define AWT_KEY_TYPED    400
#define AWT_KEY_PRESSED  401
#define AWT_KEY_RELEASED 402

#define AWT_KEY_CHAR_UNDEFINED 0

#define AWT_KEY_LOCATION_UNKNOWN 0
#define AWT_KEY_LOCATION_STANDARD 1
#define AWT_KEY_LOCATION_LEFT 2
#define AWT_KEY_LOCATION_RIGHT 3
#define AWT_KEY_LOCATION_NUMPAD 4

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
#define VK_NUMPAD0 96
#define VK_NUMPAD1 97
#define VK_NUMPAD2 98
#define VK_NUMPAD3 99
#define VK_NUMPAD4 100
#define VK_NUMPAD5 101
#define VK_NUMPAD6 102
#define VK_NUMPAD7 103
#define VK_NUMPAD8 104
#define VK_NUMPAD9 105
#define VK_MULTIPLY 106
#define VK_ADD 107
#define VK_SEPARATER 108
#define VK_SEPARATOR 108
#define VK_SUBTRACT 109
#define VK_DECIMAL 110
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

#define AWT_FOCUS_LOST 1004
#define AWT_FOCUS_GAINED 1005

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

#define AWT_FRAME_STATE_NORMAL 0
#define AWT_FRAME_STATE_ICONIFIED 1
#define AWT_FRAME_STATE_MAXIMIZED_HORIZ 2
#define AWT_FRAME_STATE_MAXIMIZED_VERT 4
#define AWT_FRAME_STATE_MAXIMIZED_BOTH 6

#define AWT_STYLE_PLAIN  0
#define AWT_STYLE_BOLD   1
#define AWT_STYLE_ITALIC 2

extern jmethodID setBoundsCallbackID;

extern jmethodID postActionEventID;
extern jmethodID postMenuActionEventID;
extern jmethodID postMouseEventID;
extern jmethodID postConfigureEventID;
extern jmethodID postExposeEventID;
extern jmethodID postKeyEventID;
extern jmethodID postFocusEventID;
extern jmethodID postAdjustmentEventID;
extern jmethodID choicePostItemEventID;
extern jmethodID postItemEventID;
extern jmethodID postListItemEventID;
extern jmethodID postTextEventID;
extern jmethodID postWindowEventID;

extern jmethodID syncAttrsID;
extern jclass gdkColor;
extern jmethodID gdkColorID;
extern JNIEnv *gdk_env;

extern GtkWindowGroup *global_gtk_window_group;

void awt_event_handler (GdkEvent *event);

gboolean pre_event_handler (GtkWidget *widget,
                               GdkEvent *event,
			       jobject peer);

void connect_awt_hook (JNIEnv *env, jobject peer_obj, int nwindows, ...);

void set_visible (GtkWidget *widget, jboolean visible);
void set_parent (GtkWidget *widget, GtkContainer *parent);
GtkLayout *find_gtk_layout (GtkWidget *parent);

jint keyevent_state_to_awt_mods (GdkEvent *event);

struct item_event_hook_info
{
  jobject peer_obj;
  const char *label;
};

#endif /* __GTKPEER_H */
