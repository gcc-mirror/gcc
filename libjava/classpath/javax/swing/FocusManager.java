/* FocusManager.java --
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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

import java.awt.Component;
import java.awt.DefaultKeyboardFocusManager;
import java.awt.KeyboardFocusManager;
import java.awt.event.KeyEvent;

/**
 * This class has been obsoleted by the new
 * {@link java.awt.KeyboardFocusManager} and
 * {@link java.awt.DefaultKeyboardFocusManager} API.
 *
 * @author Andrew Selkirk
 */
public abstract class FocusManager
  extends DefaultKeyboardFocusManager
{
  /**
   * DisabledFocusManager
   */
  static class DisabledFocusManager
    extends FocusManager
  {

    /**
     * Constructor DisabledFocusManager
     */
    DisabledFocusManager()
    {
      // TODO
    }

    /**
     * processKeyEvent
     * @param component TODO
     * @param event TODO
     */
    public void processKeyEvent(Component component, KeyEvent event)
    {
      // TODO
    }

    /**
     * focusNextComponent
     * @param component TODO
     */
    public void focusNextComponent(Component component)
    {
      // TODO
    }

    /**
     * focusPreviousComponent
     * @param value0 TODO
     */
    public void focusPreviousComponent(Component value0)
    {
      // TODO
    }
  }

  /**
   * FOCUS_MANAGER_CLASS_PROPERTY
   */
  public static final String FOCUS_MANAGER_CLASS_PROPERTY =
    "FocusManagerClassName";

  /**
   * Constructor FocusManager
   */
  public FocusManager()
  {
    super();
  }

  /**
   * getCurrentManager
   * @returns FocusManager
   */
  public static FocusManager getCurrentManager()
  {
    KeyboardFocusManager fm =
      KeyboardFocusManager.getCurrentKeyboardFocusManager();
    if (fm instanceof FocusManager)
      return (FocusManager) fm;
    else
      {
        System.err.println("The Swing FocusManager API has been obsoleted by");
        System.err.println("the new KeyboardFocusManager system.");
        System.err.println("You should either not use the Swing FocusManager");
        System.err.println("API or set the system property");
        System.err.println
          ("gnu.java.awt.FocusManager=javax.swing.FocusManager");
      }
      return null;
  }

  /**
   * setCurrentManager
   * @param manager TODO
   */
  public static void setCurrentManager(FocusManager manager)
  {
    KeyboardFocusManager.setCurrentKeyboardFocusManager(manager);
  }

  /**
   * disableSwingFocusManager
   * @deprecated 1.4
   */
  public static void disableSwingFocusManager()
  {
    // TODO
  }

  /**
   * isFocusManagerEnabled
   * @return boolean
   * @deprecated 1.4
   */
  public static boolean isFocusManagerEnabled()
  {
    return false; // TODO
  }
}
