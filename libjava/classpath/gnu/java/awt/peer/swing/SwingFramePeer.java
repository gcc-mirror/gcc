/* SwingFramePeer.java -- An abstract Swing based peer for AWT frames
   Copyright (C)  2006  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.swing;

import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.MenuBar;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.peer.ComponentPeer;
import java.awt.peer.FramePeer;

/**
 * An abstract base class for FramePeer implementations based on Swing.
 * This class provides the ability to display and handle AWT MenuBars that
 * are based on Swing.
 *
 * As a minimum, a subclass must implement all the remaining abstract methods
 * as well as the following methods:
 * <ul>
 * <li>{@link ComponentPeer#getLocationOnScreen()}</li>
 * <li>{@link ComponentPeer#getGraphics()}</li>
 * <li>{@link ComponentPeer#createImage(int, int)}</li>
 * </ul>
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public abstract class SwingFramePeer
  extends SwingWindowPeer
  implements FramePeer
{
  /**
   * The menu bar to display.
   */
  SwingMenuBarPeer menuBar = null;

  /**
   * Creates a new SwingFramePeer.
   *
   * @param frame the frame
   */
  public SwingFramePeer(Frame frame)
  {
    super(frame);
  }

  /**
   * Sets the menu bar to display in this frame.
   *
   * @param mb the menu bar to set
   */
  public void setMenuBar(MenuBar mb)
  {
    menuBar = (SwingMenuBarPeer) mb.getPeer();
    menuBar.setFramePeer(this);
    menuBar.setWidth(awtComponent.getWidth());
  }

  /**
   * Triggers 'heavyweight' painting of the frame. This will paint a menu bar
   * if present as well as the child components of this frame.
   *
   * @param g the graphics context to use for painting
   */
  protected void peerPaintComponent(Graphics g)
  {
    super.peerPaintComponent(g);
    if (menuBar != null)
      menuBar.peerPaint(g);
  }

  /**
   * Sets the size and location of this frame. This resizes the menubar to fit
   * within the frame.
   *
   * @param x the X coordinate of the screen location
   * @param y the Y coordinate of the screen location
   * @param w the width of the frame
   * @param h the height of the frame
   */
  public void setBounds(int x, int y, int w, int h)
  {
    super.setBounds(x, y, w, h);
    if (menuBar != null)
      menuBar.setWidth(w);
  }

  /**
   * Calculates the insets of this frame peer. This fetches the insets
   * from the superclass and adds the insets of the menubar if one is present.
   *
   * @return the insets of the frame
   */
  public Insets getInsets()
  {
    Insets insets = super.getInsets();
    if (menuBar != null)
      insets.top += menuBar.getHeight();
    return insets;
  }

  /**
   * Returns the location of the menu on the screen. This is needed internally
   * by the {@link SwingMenuBarPeer} in order to determine its screen location.
   *
   * @return the location of the menu on the screen
   */
  public Point getMenuLocationOnScreen()
  {
    Insets i = super.getInsets();
    return new Point(i.top, i.left);
  }

  /**
   * Overridden to provide the ability to handle menus.
   *
   * @param ev the mouse event
   */
  protected void handleMouseEvent(MouseEvent ev)
  {
    Point p = ev.getPoint();
    Insets i = super.getInsets();
    if (menuBar != null)
      {
        int menuHeight = menuBar.getHeight();
        if (p.y >= i.top && p.y <= i.top + menuHeight)
          menuBar.handleMouseEvent(ev);
        else
          {
            ev.translatePoint(0, -menuHeight);
            super.handleMouseMotionEvent(ev);
          }
      }

    super.handleMouseEvent(ev);
  }

  /**
   * Overridden to provide the ability to handle menus.
   *
   * @param ev the mouse event
   */
  protected void handleMouseMotionEvent(MouseEvent ev)
  {
    Point p = ev.getPoint();
    Insets i = super.getInsets();
    if (menuBar != null)
      {
        int menuHeight = menuBar.getHeight();
        if (p.y >= i.top && p.y <= i.top + menuHeight)
          menuBar.handleMouseMotionEvent(ev);
        else
          {
            ev.translatePoint(0, -menuHeight);
            super.handleMouseMotionEvent(ev);
          }
      }

    super.handleMouseMotionEvent(ev);
  }
}
