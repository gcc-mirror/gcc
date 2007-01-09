/* MenuShortcut.java -- A class for menu accelerators
   Copyright (C) 1999, 2002 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.event.KeyEvent;

/**
  * This class implements a keyboard accelerator for a menu item.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class MenuShortcut implements java.io.Serializable
{

/*
 * Static Variables
 */

// Serialization Constant
private static final long serialVersionUID = 143448358473180225L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The virtual keycode for the shortcut.
  */
private int key;

/**
  * @serial <code>true</code> if the shift key was used with this shortcut,
  * or <code>false</code> otherwise.
  */
private boolean usesShift;

private String keyName;

/*************************************************************************/

/**
  * Initializes a new instance of <code>MenuShortcut</code> with the
  * specified virtual key value.
  *
  * @param key The virtual keycode for the shortcut.
  */
public
MenuShortcut(int key)
{
  this(key, false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>MenuShortcut</code> with the
  * specified virtual key value and shift setting.
  *
  * @param key The virtual keycode for the shortcut.
  * @param usesShift <code>true</code> if the shift key was pressed,
  * <code>false</code> otherwise.
  */
public
MenuShortcut(int key, boolean usesShift)
{
  this.key = key;
  this.usesShift = usesShift;
  setKeyName(key);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the virtual keycode for this shortcut.
  *
  * @return The virtual keycode for this shortcut.
  */
public int
getKey()
{
  return(key);
}

/*************************************************************************/

/**
  * Returns the shift setting for this shortcut.
  *
  * @return <code>true</code> if the shift key was pressed, <code>false</code>
  * otherwise.
  */
public boolean
usesShiftModifier()
{
  return(usesShift);
}

/*************************************************************************/

/**
  * Tests this object for equality against the specified object.  The two
  * objects will be considered equal if and only if the specified object
  * is an instance of <code>MenuShortcut</code> and has the same key value
  * and shift setting as this object.
  *
  * @param obj The object to test for equality against.
  *
  * @return <code>true</code> if the two objects are equal, <code>false</code>
  * otherwise.
  */
public boolean
equals(MenuShortcut obj)
{
  if (obj == null)
    return(false);

  if (obj.key != this.key)
    return(false);

  if (obj.usesShift != this.usesShift)
    return(false);

  return(true);
}

public boolean
equals(Object obj)
{
  if (obj instanceof MenuShortcut)
    {
      MenuShortcut ms = (MenuShortcut) obj;
      return (ms.key == key && ms.usesShift == usesShift);
    }      
  return false;
}

/*************************************************************************/

/**
  * Returns a string representation of this shortcut.
  *
  * @return A string representation of this shortcut.
  */
public String
toString()
{
  String temp = "Ctrl+";
  if (usesShift)
    temp = temp + "Shift+";
  temp = temp + keyName;
  return temp;
}

public int
hashCode()
{
  // Arbitrary.
  return key + (usesShift ? 23 : 57);
}

/*************************************************************************/

/**
  * Returns a debugging string for this object.
  *
  * @return A debugging string for this object.
  */
protected String
paramString()
{
  return "key=" + key + ",usesShift=" + usesShift;
}

private void 
setKeyName(int key)
{
  if (key == '\n')
    keyName = "Enter";
  else if (key == '\b')
    keyName = "Backspace";
  else if (key == '\t')
    keyName = "Tab";
  else if (key == ' ')
    keyName = "Space";
  else if (key == ',')
    keyName = "Comma";
  else if (key == '.')
    keyName = "Period";
  else if (key == '/')
    keyName = "Slash";
  else if (key == '\\')
    keyName = "Back Slash";
  else if (key == ';')
    keyName = "Semicolon";
  else if (key == '=')
    keyName = "Equals";
  else if (key == '[')
    keyName = "Open Bracket";
  else if (key == ']')
    keyName = "Close Bracket";
  else if (key == '0')
    keyName = "0";
  else if (key == '1')
    keyName = "1";
  else if (key == '2')
    keyName = "2";
  else if (key == '3')
    keyName = "3";
  else if (key == '4')
    keyName = "4";
  else if (key == '5')
    keyName = "5";
  else if (key == '6')
    keyName = "6";
  else if (key == '7')
    keyName = "7";
  else if (key == '8')
    keyName = "8";
  else if (key == '9')
    keyName = "9";
  else if (key == 'A')
    keyName = "A";
  else if (key == 'B')
    keyName = "B";
  else if (key == 'C')
    keyName = "C";
  else if (key == 'D')
    keyName = "D";
  else if (key == 'E')
    keyName = "E";
  else if (key == 'F')
    keyName = "F";
  else if (key == 'G')
    keyName = "G";
  else if (key == 'H')
    keyName = "H";
  else if (key == 'I')
    keyName = "I";
  else if (key == 'J')
    keyName = "J";
  else if (key == 'K')
    keyName = "K";
  else if (key == 'L')
    keyName = "L";
  else if (key == 'M')
    keyName = "M";
  else if (key == 'N')
    keyName = "N";
  else if (key == 'O')
    keyName = "O";
  else if (key == 'P')
    keyName = "P";
  else if (key == 'Q')
    keyName = "Q";
  else if (key == 'R')
    keyName = "R";
  else if (key == 'S')
    keyName = "S";
  else if (key == 'T')
    keyName = "T";
  else if (key == 'U')
    keyName = "U";
  else if (key == 'V')
    keyName = "V";
  else if (key == 'W')
    keyName = "W";
  else if (key == 'X')
    keyName = "X";
  else if (key == 'Y')
    keyName = "Y";
  else if (key == 'Z')
    keyName = "Z";
  else if (key == 3)
    keyName = "Cancel";
  else if (key == 12)
    keyName = "Clear";
  else if (key == 16)
    keyName = "Shift";
  else if (key == 17)
    keyName = "Ctrl";
  else if (key == 18)
    keyName = "Alt";
  else if (key == 19)
    keyName = "Pause";
  else if (key == 20)
    keyName = "Caps Lock";
  else if (key == 21)
    keyName = "Kana";
  else if (key == 24)
    keyName = "Final";
  else if (key == 25)
    keyName = "Kanji";
  else if (key == 27)
    keyName = "Escape";
  else if (key == 28)
    keyName = "Convert";
  else if (key == 29)
    keyName = "No Convert";
  else if (key == 30)
    keyName = "Accept";
  else if (key == 31)
    keyName = "Mode Change";
  else if (key == 33)
    keyName = "Page Up";
  else if (key == 34)
    keyName = "Page Down";
  else if (key == 35)
    keyName = "End";
  else if (key == 36)
    keyName = "Home";
  else if (key == 37)
    keyName = "Left";
  else if (key == 38)
    keyName = "Up";
  else if (key == 39)
    keyName = "Right";
  else if (key == 40)
    keyName = "Down";
  else if (key == 96)
    keyName = "NumPad-0";
  else if (key == 97)
    keyName = "NumPad-1";
  else if (key == 98)
    keyName = "NumPad-2";
  else if (key == 99)
    keyName = "NumPad-3";
  else if (key == 100)
    keyName = "NumPad-4";
  else if (key == 101)
    keyName = "NumPad-5";
  else if (key == 102)
    keyName = "NumPad-6";
  else if (key == 103)
    keyName = "NumPad-7";
  else if (key == 104)
    keyName = "NumPad-8";
  else if (key == 105)
    keyName = "NumPad-9";
  else if (key == 106)
    keyName = "NumPad *";
  else if (key == 107)
    keyName = "NumPad +";
  else if (key == 108)
    keyName = "NumPad ,";
  else if (key == 109)
    keyName = "NumPad -";
  else if (key == 110)
    keyName = "NumPad .";
  else if (key == 111)
    keyName = "NumPad /";
  else if (key == 112)
    keyName = "F1";
  else if (key == 113)
    keyName = "F2";
  else if (key == 114)
    keyName = "F3";
  else if (key == 115)
    keyName = "F4";
  else if (key == 116)
    keyName = "F5";
  else if (key == 117)
    keyName = "F6";
  else if (key == 118)
    keyName = "F7";
  else if (key == 119)
    keyName = "F8";
  else if (key == 120)
    keyName = "F9";
  else if (key == 121)
    keyName = "F10";
  else if (key == 122)
    keyName = "F11";
  else if (key == 123)
    keyName = "F12";
  else if (key == 127)
    keyName = "Delete";
  else if (key == 144)
    keyName = "Num Lock";
  else if (key == 145)
    keyName = "Scroll Lock";
  else if (key == 154)
    keyName = "Print Screen";
  else if (key == 155)
    keyName = "Insert";
  else if (key == 156)
    keyName = "Help";
  else if (key == 157)
    keyName = "Meta";
  else if (key == 192)
    keyName = "Back Quote";
  else if (key == 222)
    keyName = "Quote";
}
} // class MenuShortcut
