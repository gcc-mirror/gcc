/* WindowListener.java -- listens for window events
   Copyright (C) 1999, 2002, 2005  Free Software Foundation, Inc.

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


package java.awt.event;

import java.awt.Frame;
import java.awt.Image;
import java.util.EventListener;

/**
 * This interface is for classes that wish to monitor events for window
 * changes. To watch a subset of these events, use a WindowAdapter.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see WindowAdapter
 * @see WindowEvent
 * @since 1.1
 * @status updated to 1.4
 */
public interface WindowListener extends EventListener
{
  /**
   * This method is called when the window is made visible.
   *
   * @param event the <code>WindowEvent</code> indicating the change
   */
  void windowOpened(WindowEvent event);

  /**
   * This method is called when the user calls the system menu close
   * function, giving the program a chance to cancel the close.
   *
   * @param event the <code>WindowEvent</code> indicating the close attempt
   */
  void windowClosing(WindowEvent event);

  /**
   * This method is called when the window is closed.
   *
   * @param event the <code>WindowEvent</code> indicating the dispose
   */
  void windowClosed(WindowEvent event);

  /**
   * This method is called when the window is iconified.
   *
   * @param event the <code>WindowEvent</code> indicating the iconification
   * @see Frame#setIconImage(Image)
   */
  void windowIconified(WindowEvent event);

  /**
   * This method is called when the window is deiconified.
   *
   * @param event the <code>WindowEvent</code> indicating the deiconification
   */
  void windowDeiconified(WindowEvent event);

  /**
   * This method is called when a window is activated. Only Frames and Dialogs
   * can be active, and the active window always contains the component with
   * focus.
   *
   * @param event the <code>WindowEvent</code> indicating the activation
   */
  void windowActivated(WindowEvent event);

  /**
   * This method is called when the window is deactivated.
   *
   * @param event the <code>WindowEvent</code> indicating the deactivation
   */
  void windowDeactivated(WindowEvent event);
} // interface WindowListener
