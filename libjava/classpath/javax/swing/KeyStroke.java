/* KeyStroke.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.AWTKeyStroke;
import java.awt.event.KeyEvent;
import java.io.Serializable;

public class KeyStroke 
  extends AWTKeyStroke
  implements Serializable
{
  private static final long serialVersionUID = -9060180771037902530L;
  
  // Called by java.awt.AWTKeyStroke.registerSubclass via reflection.
  private KeyStroke()
  {
    // Nothing to do here.
  }
  
  private KeyStroke(char keyChar, int keyCode, int modifiers,
                    boolean onKeyRelease)
  {
    super(keyChar, keyCode, modifiers, onKeyRelease);
  }

  static
  {
    AWTKeyStroke.registerSubclass(KeyStroke.class);
  }

  public static KeyStroke getKeyStroke(char keyChar) 
  {
    return (KeyStroke) getAWTKeyStroke(keyChar);
  }

  /** 
   * @deprecated Use {@link #getKeyStroke(char)}
   *
   * This method, unlike all the other factory methods on this object,
   * returns a non-cached, non-shared object. New code should not use it.
   */
  public static KeyStroke getKeyStroke(char keyChar, boolean onKeyRelease) 
  {
    return new KeyStroke(keyChar, KeyEvent.VK_UNDEFINED, 0, onKeyRelease);
  }

  public static KeyStroke getKeyStroke(Character keyChar, int modifiers) 
  {
    return (KeyStroke) getAWTKeyStroke(keyChar, modifiers);
  }

  public static KeyStroke getKeyStroke(int keyCode, int modifiers, 
                                       boolean onKeyRelease) 
  {
    return (KeyStroke) getAWTKeyStroke(keyCode, modifiers, onKeyRelease);
  }

  public static KeyStroke getKeyStroke(int keyCode, int modifiers) 
  {
    return (KeyStroke) getAWTKeyStroke(keyCode, modifiers);
  }

  /**
   * Returns the KeyStroke according to <code>getAWTKeyStroke()</code>.
   * But it returns null instead of throwing
   * <code>IllegalArugmentException</code> when
   * the keystoke sequence cannot be parsed from the given string.
   */
  public static KeyStroke getKeyStroke(String str) 
  {
    try
      {
	return (KeyStroke) getAWTKeyStroke(str);
      }
    catch (IllegalArgumentException iae)
      {
	return null;
      }
  }

  public static KeyStroke getKeyStrokeForEvent(KeyEvent event) 
  {
    return (KeyStroke) getAWTKeyStrokeForEvent(event);
  }

}
