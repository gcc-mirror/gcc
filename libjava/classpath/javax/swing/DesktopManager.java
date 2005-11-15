/* DesktopManager.java --
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

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

/**
 * DesktopManagers are responsible for implementing the behaviours for the
 * JInternalFrames that belong to JDesktopPanes. Actions such as maximizing,
 * minimizing, iconifying, etc will be delegated to the DesktopManager.
 */
public interface DesktopManager
{
  /**
   * This method will cause the JInternalFrame to be displayed in the set
   * location. This usually is not needed since the user will add the
   * JInternalFrame to a Container separately.
   *
   * @param frame The JInternalFrame to open.
   */
  void openFrame(JInternalFrame frame);

  /**
   * This method should remove the JInternalFrame from its parent.
   *
   * @param frame The JInternalFrame to close.
   */
  void closeFrame(JInternalFrame frame);

  /**
   * This method should maximize the JInternalFrame to match its parent's
   * bounds.
   *
   * @param frame The JInternalFrame to maximize.
   */
  void maximizeFrame(JInternalFrame frame);

  /**
   * This method should restore the JInternalFrame to its normal bounds.
   *
   * @param frame The JInternalFrame to minimize.
   */
  void minimizeFrame(JInternalFrame frame);

  /**
   * This method should remove the JInternalFrame from its parent and replace
   * it with a JDesktopIcon.
   *
   * @param frame The JInternalFrame to iconify.
   */
  void iconifyFrame(JInternalFrame frame);

  /**
   * This method should remove the JDesktopIcon from its parent and replace it
   * with the JInternalFrame that the JDesktopIcon represents.
   *
   * @param frame The JInternalFrame to deiconify.
   */
  void deiconifyFrame(JInternalFrame frame);

  /**
   * This method should give focus to the JInternalFrame and its default focus
   * owner.
   *
   * @param vframe The JInternalFrame to activate.
   */
  void activateFrame(JInternalFrame vframe);

  /**
   * This method should be called when the JInternalFrame gets deselected and
   * subsequently loses focus.
   *
   * @param frame The JInternalFrame to deactivate.
   */
  void deactivateFrame(JInternalFrame frame);

  /**
   * This method should be called in preparation for dragging. This needs to
   * be called prior to dragFrame calls so that the DesktopManager can
   * prepare any state information.
   *
   * @param frame The JInternalFrame to prepare for dragging.
   */
  void beginDraggingFrame(JComponent frame);

  /**
   * This method drags the given JInternalFrame to the given x and y
   * coordinates.
   *
   * @param frame The JInternalFrame to drag.
   * @param x The new x coordinate.
   * @param y The new y coordinate.
   */
  void dragFrame(JComponent frame, int x, int y);

  /**
   * This method should be called after dragFrame calls. Any information used
   * by the DesktopManager for dragging the JInternalFrame can be cleared.
   *
   * @param frame The JInternalFrame that finished dragging.
   */
  void endDraggingFrame(JComponent frame);

  /**
   * This method should be called prior to any resizeFrame calls. Any state
   * information needed by the DesktopManager to resize the JInternalFrame
   * will be prepared here.
   *
   * @param frame The JInternalFrame to resize.
   * @param direction One of eight directions specified by SwingConstants.
   */
  void beginResizingFrame(JComponent frame, int direction);

  /**
   * This method is called to resize the given JInternalFrame to the given
   * bounds.
   *
   * @param frame The JInternalFrame to resize.
   * @param x The new x coordinate.
   * @param y The new y coordinate.
   * @param width The new width.
   * @param height The new height.
   */
  void resizeFrame(JComponent frame, int x, int y, int width, int height);

  /**
   * This method is called to signify that the resize is finished. Any
   * information used to resize the JInternalFrame can now be cleared.
   *
   * @param frame The JInternalFrame that just finished dragging.
   */
  void endResizingFrame(JComponent frame);

  /**
   * This method does the actual work for reshaping the JInternalFrame.
   *
   * @param frame The JInternalFrame to resize.
   * @param x The new x coordinate.
   * @param y The new y coordinate.
   * @param width The new width.
   * @param height The new height.
   */
  void setBoundsForFrame(JComponent frame, int x, int y, int width, int height);
} // DesktopManager
