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

#ifdef JVM_SUN

extern struct state_table *native_state_table;

#define NSA_INIT(env, clazz) \
  native_state_table = init_state_table (env, clazz)

#define NSA_GET_PTR(env, obj) \
  get_state (env, obj, native_state_table)

#define NSA_SET_PTR(env, obj, ptr) \
  set_state (env, obj, native_state_table, (void *)ptr)

#define NSA_DEL_PTR(env, obj) \
  remove_state_slot (env, obj, native_state_table)

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

#define VK_UNDEFINED   0
#define AWT_KEY_CHAR_UNDEFINED 0

#define VK_0 48
#define VK_1 49
#define VK_2 50
#define VK_3 51
#define VK_4 52
#define VK_5 53
#define VK_6 54
#define VK_7 55
#define VK_8 56
#define VK_9 57
#define VK_A 65
#define VK_ACCEPT 30
#define VK_ADD 107
#define VK_ALT 18
#define VK_B 66
#define VK_BACK_QUOTE 192
#define VK_BACK_SLASH 92
#define VK_BACK_SPACE 8
#define VK_C 67
#define VK_CANCEL 3
#define VK_CAPS_LOCK 20
#define VK_CLEAR 12 
#define VK_CLOSE_BRACKET 93
#define VK_COMMA 44
#define VK_CONTROL 17
#define VK_CONVERT 28
#define VK_D 68
#define VK_DECIMAL 110
#define VK_DELETE 127
#define VK_DIVIDE 111
#define VK_DOWN 40
#define VK_E 69
#define VK_END 35
#define VK_ENTER 10
#define VK_ESCAPE 27
#define VK_F 70
#define VK_F1 112
#define VK_F10 121
#define VK_F11 122
#define VK_F12 123
#define VK_F2 113
#define VK_F3 114
#define VK_F4 115
#define VK_F5 116
#define VK_F6 117
#define VK_F7 118
#define VK_F8 119
#define VK_F9 120
#define VK_FINAL 24
#define VK_G 71
#define VK_H 72
#define VK_HELP 156
#define VK_HOME 36
#define VK_I 73
#define VK_INSERT 155
#define VK_J 74
#define VK_K 75
#define VK_KANA 21
#define VK_KANJI 25
#define VK_L 76
#define VK_LEFT 37
#define VK_M 77
#define VK_META 157
#define VK_MODECHANGE 31
#define VK_MULTIPLY 106
#define VK_N 78
#define VK_NONCONVERT 29
#define VK_NUM_LOCK 144
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
#define VK_O 79
#define VK_OPEN_BRACKET 91
#define VK_P 80
#define VK_PAGE_DOWN 34
#define VK_PAGE_UP 33
#define VK_PAUSE 19
#define VK_PERIOD 46
#define VK_PRINTSCREEN 154
#define VK_Q 81
#define VK_QUOTE 222
#define VK_R 82
#define VK_RIGHT 39
#define VK_S 83
#define VK_SCROLL_LOCK 145
#define VK_SEMICOLON 59
#define VK_SEPARATOR 108
#define VK_SHIFT 16
#define VK_SLASH 47
#define VK_SPACE 32
#define VK_SUBTRACT 109
#define VK_T 84
#define VK_TAB 9
#define VK_U 85
#define VK_UP 38
#define VK_V 86
#define VK_W 87
#define VK_X 88
#define VK_Y 89
#define VK_Z 90

#define AWT_FOCUS_LOST 1004
#define AWT_FOCUS_GAINED 1005

extern jmethodID postActionEventID;
extern jmethodID postMenuActionEventID;
extern jmethodID postMouseEventID;
extern jmethodID postConfigureEventID;
extern jmethodID postExposeEventID;
extern jmethodID postKeyEventID;
extern jmethodID postFocusEventID;
extern jmethodID postAdjustmentEventID;
extern jmethodID postItemEventID;
extern jmethodID postListItemEventID;
extern jmethodID syncAttrsID;
extern jclass gdkColor;
extern jmethodID gdkColorID;
extern JNIEnv *gdk_env;

void
gdk_window_get_root_geometry (GdkWindow *window,
			      gint      *x,
			      gint      *y,
			      gint      *width,
			      gint      *height,
			      gint      *border,
			      gint      *depth);

void awt_event_handler (GdkEvent *event);

void connect_awt_hook (JNIEnv *env, jobject peer_obj, int nwindows, ...);

void set_visible (GtkWidget *widget, jboolean visible);
void set_parent (GtkWidget *widget, GtkContainer *parent);
GtkLayout *find_gtk_layout (GtkWidget *parent);
void setup_window (JNIEnv *env, jobject obj, GtkWidget *window, jint width, 
		   jint height, jboolean visible);

struct item_event_hook_info
{
  jobject peer_obj;
  jobject item_obj;
};

#endif /* __GTKPEER_H */
