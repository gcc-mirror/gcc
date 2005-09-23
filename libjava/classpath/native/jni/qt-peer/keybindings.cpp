/* keybindings.cpp --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

#include "keybindings.h"

/* InputEvent key modifiers */
#define SHIFT_MASK 1
#define CTRL_MASK 2
#define META_MASK 4
#define ALT_MASK 8
#define ALT_GRAPH_MASK 0x20
#define BUTTON1_MASK 0x10
#define BUTTON2_MASK 8
#define BUTTON3_MASK 4

#define SHIFT_DOWN_MASK 0x0040
#define CTRL_DOWN_MASK 0x0080
#define META_DOWN_MASK 0x0100
#define ALT_DOWN_MASK 0x0200
#define BUTTON1_DOWN_MASK  0x0400
#define BUTTON2_DOWN_MASK  0x0800
#define BUTTON3_DOWN_MASK  0x1000
#define ALT_GRAPH_DOWN_MASK  0x2000

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


int mapKeyCode(QKeyEvent *key)
{
  switch(key->key())
    {
    case Qt::Key_Escape:
      return VK_ESCAPE;
    case Qt::Key_Tab:
      return VK_TAB;
    case Qt::Key_Backspace:
      return VK_BACK_SPACE;
    case Qt::Key_Return:
    case Qt::Key_Enter:
      return VK_ENTER;
    case Qt::Key_Insert:
      return VK_INSERT;
    case Qt::Key_Delete:
      return VK_DELETE;
    case Qt::Key_Pause:
      return VK_PAUSE;
    case Qt::Key_Print:
    case Qt::Key_SysReq:
      return VK_PRINTSCREEN;
    case Qt::Key_Home:
      return VK_HOME;
    case Qt::Key_End:
      return VK_END;
    case Qt::Key_Left:
      return VK_LEFT;
    case Qt::Key_Up:
      return VK_UP;
    case Qt::Key_Right:
      return VK_RIGHT;
    case Qt::Key_Down:
      return VK_DOWN;
    case Qt::Key_PageUp:
      return VK_PAGE_UP;
    case Qt::Key_PageDown:
      return VK_PAGE_DOWN;
    case Qt::Key_Shift:
      return VK_SHIFT;
    case Qt::Key_Control:
      return VK_CONTROL;
    case Qt::Key_Meta:
      return VK_META;
    case Qt::Key_Alt:
      return VK_ALT;
    case Qt::Key_CapsLock:
      return VK_CAPS_LOCK;
    case Qt::Key_NumLock:
      return VK_NUM_LOCK;
    case Qt::Key_ScrollLock:
      return VK_SCROLL_LOCK;
    case Qt::Key_Clear:
      return VK_CLEAR;
    case Qt::Key_F1:
      return VK_F1;
    case Qt::Key_F2:
      return VK_F2;
    case Qt::Key_F3:
      return VK_F3;
    case Qt::Key_F4:
      return VK_F4;
    case Qt::Key_F5:
      return VK_F5;
    case Qt::Key_F6:
      return VK_F6;
    case Qt::Key_F7:
      return VK_F7;
    case Qt::Key_F8:
      return VK_F8;
    case Qt::Key_F9:
      return VK_F9;
    case Qt::Key_F10:
      return VK_F10;
    case Qt::Key_F11:
      return VK_F11;
    case Qt::Key_F12:
      return VK_F12;
    case Qt::Key_F13:
      return VK_F13;
    case Qt::Key_F14:
      return VK_F14;
    case Qt::Key_F15:
      return VK_F15;
    case Qt::Key_F16:
      return VK_F16;
    case Qt::Key_F17:
      return VK_F17;
    case Qt::Key_F18:
      return VK_F18;
    case Qt::Key_F19:
      return VK_F19;
    case Qt::Key_F20:
      return VK_F20;
    case Qt::Key_F21:
      return VK_F21;
    case Qt::Key_F22:
      return VK_F22;
    case Qt::Key_F23:
      return VK_F23;
    case Qt::Key_F24:
      return VK_F24;
    case Qt::Key_Help:
      return VK_HELP;

    case Qt::Key_Space:
      return VK_SPACE;

    case Qt::Key_Exclam:
      return VK_EXCLAMATION_MARK;
    case Qt::Key_QuoteDbl:
      return VK_QUOTEDBL;
    case Qt::Key_NumberSign:
      return VK_NUMBER_SIGN;

    case Qt::Key_Dollar:
      return VK_DOLLAR;


    case Qt::Key_Ampersand:
      return VK_AMPERSAND;

    case Qt::Key_ParenLeft:
      return VK_LEFT_PARENTHESIS;
    case Qt::Key_ParenRight:
      return VK_RIGHT_PARENTHESIS;
    case Qt::Key_Asterisk:
      return VK_ASTERISK;
    case Qt::Key_Plus:
      return VK_PLUS;
    case Qt::Key_Comma:
      return VK_COMMA;
    case Qt::Key_Minus:
      return VK_MINUS;
    case Qt::Key_Period:
      return VK_PERIOD;
    case Qt::Key_Slash:
      return VK_SLASH;

    case Qt::Key_0:
      return VK_0;
    case Qt::Key_1:
      return VK_1;
    case Qt::Key_2:
      return VK_2;
    case Qt::Key_3:
      return VK_3;
    case Qt::Key_4:
      return VK_4;
    case Qt::Key_5:
      return VK_5  ;
    case Qt::Key_6:
      return VK_6;
    case Qt::Key_7:
      return VK_7;
    case Qt::Key_8:
      return VK_8;
    case Qt::Key_9:
      return VK_9;

    case Qt::Key_Colon:
      return VK_COLON;
    case Qt::Key_Semicolon:
      return VK_SEMICOLON;
    case Qt::Key_Less:
      return VK_LESS;
    case Qt::Key_Equal:
      return VK_EQUALS;
    case Qt::Key_Greater:
      return VK_GREATER;
    case Qt::Key_Question:
    case Qt::Key_At:

    case Qt::Key_A:
      return VK_A;
    case Qt::Key_B:
      return VK_B;
    case Qt::Key_C:
      return VK_C;
    case Qt::Key_D:
      return VK_D;
    case Qt::Key_E:
      return VK_E;
    case Qt::Key_F:
      return VK_F;
    case Qt::Key_G:
      return VK_G;
    case Qt::Key_H:
      return VK_H;
    case Qt::Key_I:
      return VK_I;
    case Qt::Key_J:
      return VK_J;
    case Qt::Key_K:
      return VK_K;
    case Qt::Key_L:
      return VK_L;
    case Qt::Key_M:
      return VK_M;
    case Qt::Key_N:
      return VK_N;
    case Qt::Key_O:
      return VK_O;
    case Qt::Key_P:
      return VK_P;
    case Qt::Key_Q:
      return VK_Q;
    case Qt::Key_R:
      return VK_R;
    case Qt::Key_S:
      return VK_S;
    case Qt::Key_T:
      return VK_T;
    case Qt::Key_U:
      return VK_U;
    case Qt::Key_V:
      return VK_V;
    case Qt::Key_W:
      return VK_W;
    case Qt::Key_X:
      return VK_X;
    case Qt::Key_Y:
      return VK_Y;
    case Qt::Key_Z:
      return VK_Z;
    case Qt::Key_division:
      return VK_DIVIDE;
    case Qt::Key_BracketLeft:
      return VK_OPEN_BRACKET;
    case Qt::Key_Backslash:
      return VK_BACK_SLASH;
    case Qt::Key_BracketRight:
      return VK_CLOSE_BRACKET;
    case Qt::Key_BraceLeft:
      return VK_BRACELEFT;
    case Qt::Key_BraceRight:
      return VK_BRACERIGHT;
    case Qt::Key_brokenbar:
      return VK_SEPARATOR; // correct?

    default:
      return VK_UNDEFINED;
    }
}

int getUnicode(QKeyEvent *key)
{
  QString s = key->text();
  if(s.isEmpty())
    return 0; // CHAR_UNDEFINED
  QChar c = s.at(0);
  return (int)c.unicode();
}

/**
 * Returns the key modifiers in KeyEvent format 
 */
int getKeyModifiers(Qt::KeyboardModifiers state)
{
  int modifier = 0;
  if( state & Qt::ShiftModifier )
    modifier |= SHIFT_DOWN_MASK;
  if( state & Qt::ControlModifier )
    modifier |= CTRL_DOWN_MASK;
  if( state & Qt::AltModifier )
    modifier |= ALT_DOWN_MASK;
  if( state & Qt::MetaModifier )
    modifier |= META_DOWN_MASK;

  return modifier;
}

/**
 * Returns the key modifiers in ActionEvent format 
 */
int getAEKeyModifiers(Qt::KeyboardModifiers state)
{
  int modifier = 0;
  if( state & Qt::ShiftModifier )
    modifier |= SHIFT_MASK;
  if( state & Qt::ControlModifier )
    modifier |= CTRL_MASK;
  if( state & Qt::AltModifier )
    modifier |= ALT_MASK;
  if( state & Qt::MetaModifier )
    modifier |= META_MASK;

  return modifier;
}

/**
 * Returns the mouse modifiers in InputEvent format 
 */
int getMouseModifiers(QMouseEvent *e)
{
  int modifier = 0;
  int buttons = e->buttons();
  int state = e->modifiers();

  if( buttons & Qt::LeftButton )
    modifier |= BUTTON1_DOWN_MASK;
  if( buttons & Qt::MidButton )
    modifier |= BUTTON2_DOWN_MASK;
  if( buttons & Qt::RightButton )
    modifier |= BUTTON3_DOWN_MASK;

  if( state & Qt::ShiftModifier )
    modifier |= SHIFT_DOWN_MASK;
  if( state & Qt::ControlModifier )
    modifier |= CTRL_DOWN_MASK;
  if( state & Qt::AltModifier )
    modifier |= ALT_DOWN_MASK;
  if( state & Qt::MetaModifier )
    modifier |= META_DOWN_MASK;

  // FIXME: Alt Gr?
  return modifier;
}

/**
 * Returns the mouse modifiers in InputEvent format 
 * We need a different method here because e->buttons() doesn't work for,
 * mouseReleased events. (But strangely enough it does for pressed ones)
 */
int getReleaseModifiers(QMouseEvent *e)
{
  int modifier = 0;
  int button = e->button();
  int state = e->modifiers();

  if( button & Qt::LeftButton )
    modifier |= BUTTON1_DOWN_MASK;
  if( button & Qt::MidButton )
    modifier |= BUTTON2_DOWN_MASK;
  if( button & Qt::RightButton )
    modifier |= BUTTON3_DOWN_MASK;

  if( state & Qt::ShiftModifier )
    modifier |= SHIFT_DOWN_MASK;
  if( state & Qt::ControlModifier )
    modifier |= CTRL_DOWN_MASK;
  if( state & Qt::AltModifier )
    modifier |= ALT_DOWN_MASK;
  if( state & Qt::MetaModifier )
    modifier |= META_DOWN_MASK;

  // FIXME: Alt Gr?
  return modifier;
}


