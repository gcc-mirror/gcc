/* KeyboardMapping.java -- Maps X keysyms to Java keyCode and keyChar
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package gnu.java.awt.peer.x;

import gnu.x11.Input;
import gnu.x11.keysym.Latin1;
import gnu.x11.keysym.Misc;

import java.awt.event.KeyEvent;

/**
 * Defines the keyboard mapping from X keysyms to Java
 * keycodes and keychars.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
final class KeyboardMapping
{

  /**
   * Maps X keycodes to AWT keycodes.
   * 
   * @param xInput the X input event
   * @param xKeyCode the X keycode to map
   * @param xMods the X modifiers
   *
   * @return the AWT keycode and keychar
   */
  static int mapToKeyCode(gnu.x11.Input xInput, int xKeyCode, int xMods)
  {
    int mapped = KeyEvent.VK_UNDEFINED;
    int keysym = xInput.keycode_to_keysym(xKeyCode, xMods, true);

    // Special keys.
    if (keysym >= 255 << 8)
      {
        // FIXME: Add missing mappings.
        switch (keysym)
        {
          case Misc.BACKSPACE:
            mapped = KeyEvent.VK_BACK_SPACE;
            break;
          case Misc.TAB:
            mapped = KeyEvent.VK_TAB;
            break;
          case Misc.CLEAR:
            mapped = KeyEvent.VK_CLEAR;
            break;
          case Misc.RETURN:
            mapped = KeyEvent.VK_ENTER;
            break;
          case Misc.PAUSE:
            mapped = KeyEvent.VK_PAUSE;
            break;
          case Misc.SCROLL_LOCK:
            mapped = KeyEvent.VK_SCROLL_LOCK;
            break;
          case Misc.ESCAPE:
            mapped = KeyEvent.VK_ESCAPE;
            break;
          case Misc.HOME:
            mapped = KeyEvent.VK_HOME;
            break;
          case Misc.LEFT:
            mapped = KeyEvent.VK_LEFT;
            break;
          case Misc.UP:
            mapped = KeyEvent.VK_UP;
            break;
          case Misc.RIGHT:
            mapped = KeyEvent.VK_RIGHT;
            break;
          case Misc.DOWN:
            mapped = KeyEvent.VK_DOWN;
            break;
          case Misc.PAGE_UP:
            mapped = KeyEvent.VK_PAGE_UP;
            break;
          case Misc.PAGE_DOWN:
            mapped = KeyEvent.VK_PAGE_DOWN;
            break;
          case Misc.END:
            mapped = KeyEvent.VK_END;
            break;
          case Misc.BEGIN:
            mapped = KeyEvent.VK_BEGIN;
            break;
          case Misc.INSERT:
            mapped = KeyEvent.VK_INSERT;
            break;
          case Misc.UNDO:
            mapped = KeyEvent.VK_UNDO;
            break;
          case Misc.FIND:
            mapped = KeyEvent.VK_FIND;
            break;
          case Misc.CANCEL:
            mapped = KeyEvent.VK_CANCEL;
            break;
          case Misc.HELP:
            mapped = KeyEvent.VK_HELP;
            break;
          case Misc.MODE_SWITCH:
            mapped = KeyEvent.VK_MODECHANGE;
            break;
          case Misc.NUM_LOCK:
            mapped = KeyEvent.VK_NUM_LOCK;
            break;
          case Misc.KP_LEFT:
            mapped = KeyEvent.VK_KP_LEFT;
            break;
          case Misc.KP_UP:
            mapped = KeyEvent.VK_KP_UP;
            break;
          case Misc.KP_RIGHT:
            mapped = KeyEvent.VK_KP_RIGHT;
            break;
          case Misc.KP_DOWN:
            mapped = KeyEvent.VK_KP_DOWN;
            break;
          case Misc.F1:
            mapped = KeyEvent.VK_F1;
            break;
          case Misc.F2:
            mapped = KeyEvent.VK_F2;
            break;
          case Misc.F3:
            mapped = KeyEvent.VK_F3;
            break;
          case Misc.F4:
            mapped = KeyEvent.VK_F4;
            break;
          case Misc.F5:
            mapped = KeyEvent.VK_F5;
            break;
          case Misc.F6:
            mapped = KeyEvent.VK_F6;
            break;
          case Misc.F7:
            mapped = KeyEvent.VK_F7;
            break;
          case Misc.F8:
            mapped = KeyEvent.VK_F8;
            break;
          case Misc.F9:
            mapped = KeyEvent.VK_F9;
            break;
          case Misc.F10:
            mapped = KeyEvent.VK_F10;
            break;
          case Misc.F11:
            mapped = KeyEvent.VK_F11;
            break;
          case Misc.F12:
            mapped = KeyEvent.VK_F12;
            break;
          case Misc.F13:
            mapped = KeyEvent.VK_F13;
            break;
          case Misc.F14:
            mapped = KeyEvent.VK_F14;
            break;
          case Misc.F15:
            mapped = KeyEvent.VK_F15;
            break;
          case Misc.F16:
            mapped = KeyEvent.VK_F16;
            break;
          case Misc.F17:
            mapped = KeyEvent.VK_F17;
            break;
          case Misc.F18:
            mapped = KeyEvent.VK_F18;
            break;
          case Misc.F19:
            mapped = KeyEvent.VK_F19;
            break;
          case Misc.F20:
            mapped = KeyEvent.VK_F20;
            break;
          case Misc.F21:
            mapped = KeyEvent.VK_F21;
            break;
          case Misc.F22:
            mapped = KeyEvent.VK_F22;
            break;
          case Misc.F23:
            mapped = KeyEvent.VK_F23;
            break;
          case Misc.F24:
            mapped = KeyEvent.VK_F24;
            break;
          case Misc.SHIFT_L:
          case Misc.SHIFT_R:
            mapped = KeyEvent.VK_SHIFT;
            break;
          case Misc.CONTROL_L:
          case Misc.CONTROL_R:
            mapped = KeyEvent.VK_CONTROL;
            break;
          case Misc.CAPS_LOCK:
          case Misc.SHIFT_LOCK:
            mapped = KeyEvent.VK_CAPS_LOCK;
            break;
          case Misc.META_L:
          case Misc.META_R:
            mapped = KeyEvent.VK_META;
            break;
          case Misc.ALT_L:
          case Misc.ALT_R:
            mapped = KeyEvent.VK_ALT;
            break;
          case Misc.DELETE:
            mapped = KeyEvent.VK_DELETE;
            break;
          default:
            mapped = KeyEvent.VK_UNDEFINED;
        }
      }
    // Map Latin1 characters.
    else if (keysym < 256)
      {
        // TODO: Add missing mappings, if any.
        // Lowercase characters are mapped to
        // their corresponding upper case pendants.
        if (keysym >= Latin1.A_SMALL && keysym <= Latin1.Z_SMALL)
          mapped = keysym - 0x20;
        // Uppercase characters are mapped 1:1.
        else if (keysym >= Latin1.A && keysym <= Latin1.Z)
          mapped = keysym;
        // Digits are mapped 1:1.
        else if (keysym >= Latin1.NUM_0 && keysym <= Latin1.NUM_9)
          mapped = keysym;
        else
          {
            switch (keysym)
            {
              case Latin1.SPACE:
                mapped = KeyEvent.VK_SPACE;
                break;
              case Latin1.EXCLAM:
                mapped = KeyEvent.VK_EXCLAMATION_MARK;
                break;
              case Latin1.QUOTE_DBL:
                mapped = KeyEvent.VK_QUOTEDBL;
                break;
              case Latin1.NUMBER_SIGN:
                mapped = KeyEvent.VK_NUMBER_SIGN;
                break;
              case Latin1.DOLLAR:
                mapped = KeyEvent.VK_DOLLAR;
                break;
              case Latin1.AMPERSAND:
                mapped = KeyEvent.VK_AMPERSAND;
                break;
              case Latin1.APOSTROPHE:
                mapped = KeyEvent.VK_QUOTE;
                break;
              case Latin1.PAREN_LEFT:
                mapped = KeyEvent.VK_LEFT_PARENTHESIS;
                break;
              case Latin1.PAREN_RIGHT:
                mapped = KeyEvent.VK_RIGHT_PARENTHESIS;
                break;
              case Latin1.ASTERISK:
                mapped = KeyEvent.VK_ASTERISK;
                break;
              case Latin1.PLUS:
                mapped = KeyEvent.VK_PLUS;
                break;
              case Latin1.COMMA:
                mapped = KeyEvent.VK_COMMA;
                break;
              case Latin1.MINUS:
                mapped = KeyEvent.VK_MINUS;
                break;
              case Latin1.PERIOD:
                mapped = KeyEvent.VK_PERIOD;
                break;
              case Latin1.SLASH:
                mapped = KeyEvent.VK_SLASH;
                break;
              case Latin1.COLON:
                mapped = KeyEvent.VK_COLON;
                break;
              case Latin1.SEMICOLON:
                mapped = KeyEvent.VK_SEMICOLON;
                break;
              case Latin1.LESS:
                mapped = KeyEvent.VK_LESS;
                break;
              case Latin1.EQUAL:
                mapped = KeyEvent.VK_EQUALS;
                break;
              case Latin1.GREATER:
                mapped = KeyEvent.VK_GREATER;
                break;
              case Latin1.AT:
                mapped = KeyEvent.VK_AT;
                break;
              case Latin1.BRACKET_LEFT:
                mapped = KeyEvent.VK_OPEN_BRACKET;
                break;
              case Latin1.BACKSLASH:
                mapped = KeyEvent.VK_BACK_SLASH;
                break;
              case Latin1.BRACKET_RIGHT:
                mapped = KeyEvent.VK_CLOSE_BRACKET;
                break;
              case Latin1.ASCII_CIRCUM:
                mapped = KeyEvent.VK_CIRCUMFLEX;
                break;
              case Latin1.UNDERSCORE:
                mapped = KeyEvent.VK_UNDERSCORE;
                break;
              case Latin1.GRAVE:
                mapped = KeyEvent.VK_DEAD_GRAVE;
                break;
              case Latin1.BRACE_LEFT:
                mapped = KeyEvent.VK_BRACELEFT;
                break;
              case Latin1.BRACE_RIGHT:
                mapped = KeyEvent.VK_BRACERIGHT;
                break;
              case Latin1.ASCII_TILDE:
                mapped = KeyEvent.VK_DEAD_TILDE;
                break;
              case Latin1.EXCLAM_DOWN:
                mapped = KeyEvent.VK_INVERTED_EXCLAMATION_MARK;
                break;
              default:
                mapped = KeyEvent.VK_UNDEFINED;
            }
          }
      }
    return mapped;
  }

  /**
   * Maps X keycodes+modifiers to Java keychars.
   *
   * @param xInput The X Input to use for mapping
   * @param xKeyCode the X keycode
   * @param xMods the X key modifiers
   *
   * @return the Java keychar
   */
  static char mapToKeyChar(gnu.x11.Input xInput, int xKeyCode, int xMods)
  {
    char mapped = KeyEvent.CHAR_UNDEFINED;
    char keysym = (char) xInput.keycode_to_keysym(xKeyCode, xMods, false);
    // FIXME: Map other encodings properly.
    if (keysym < 256) // Latin1.
      {
        mapped = keysym;
      }
    return mapped;
  }

  /**
   * Maps X modifier masks to AWT modifier masks.
   *
   * @param xMods the X modifiers
   *
   * @return the AWT modifiers
   */
  static int mapModifiers(int xMods)
  {
    int mods = 0;

    if ((xMods & Input.SHIFT_MASK) != 0)
      mods |= KeyEvent.SHIFT_MASK | KeyEvent.SHIFT_DOWN_MASK;
    if ((xMods & Input.META_MASK) != 0)
      mods |= KeyEvent.META_MASK | KeyEvent.META_DOWN_MASK;
    if ((xMods & Input.ALT_MASK) != 0)
      mods |= KeyEvent.ALT_MASK | KeyEvent.ALT_DOWN_MASK;
    if ((xMods & Input.MOD5_MASK) != 0)
      mods |= KeyEvent.ALT_GRAPH_MASK | KeyEvent.ALT_GRAPH_DOWN_MASK;
    if ((xMods & Input.CONTROL_MASK) != 0)
      mods |= KeyEvent.CTRL_MASK | KeyEvent.CTRL_DOWN_MASK;

    return mods;
  }
}
