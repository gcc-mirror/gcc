/* DefaultFocusManager.java --
   Copyright (C) 2002, 2004, 2006, Free Software Foundation, Inc.

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
import java.awt.Container;
import java.awt.event.KeyEvent;
import java.util.Stack;

/**
 * This class has been obsoleted by the new
 * {@link java.awt.KeyboardFocusManager} and
 * {@link java.awt.DefaultKeyboardFocusManager} API.
 *
 * @author Andrew Selkirk
 */
public class DefaultFocusManager extends FocusManager
{

  /**
   * historyStack
   */
  private Stack historyStack;

  /**
   * Constructor DefaultFocusManager
   */
  public DefaultFocusManager()
  {
    // TODO
  } // DefaultFocusManager()

        /**
   * processKeyEvent
   *
   * @param component
   *          TODO
   * @param event
   *          TODO
   */
  public void processKeyEvent(Component component, KeyEvent event)
  {
    // TODO
  } // processKeyEvent()

  /**
   * focusNextComponent
   *
   * @param component
   *          TODO
   */
  public void focusNextComponent(Component component)
  {
    // TODO
  } // focusNextComponent()

  /**
   * focusPreviousComponent
   *
   * @param component
   *          TODO
   */
  public void focusPreviousComponent(Component component)
  {
    // TODO
  } // focusPreviousComponent()

  /**
   * getFirstComponent
   *
   * @param container
   *          TODO
   * @return Component
   */
  public Component getFirstComponent(Container container)
  {
    return null; // TODO
  } // getFirstComponent()

  /**
   * getLastComponent
   *
   * @param container
   *          TODO
   * @return Component
   */
  public Component getLastComponent(Container container)
  {
    return null; // TODO
  } // getLastComponent()

  /**
   * getComponentBefore
   *
   * @param container
   *          TODO
   * @param component
   *          TODO
   * @return Component
   */
  public Component getComponentBefore(Container container, Component component)
  {
    return null; // TODO
  } // getComponentBefore()

  /**
   * getComponentAfter
   *
   * @param container
   *          TODO
   * @param component
   *          TODO
   * @return Component
   */
  public Component getComponentAfter(Container container, Component component)
  {
    return null; // TODO
  } // getComponentAfter()

  /**
   * compareTabOrder
   *
   * @param component1
   *          TODO
   * @param component2
   *          TODO
   * @return boolean
   */
  public boolean compareTabOrder(Component component1, Component component2)
  {
    return false; // TODO
  } // compareTabOrder()

} // DefaultFocusManager
