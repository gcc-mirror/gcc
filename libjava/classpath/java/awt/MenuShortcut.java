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
  return(getClass().getName() + "[" + paramString () + "]");
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

} // class MenuShortcut 
