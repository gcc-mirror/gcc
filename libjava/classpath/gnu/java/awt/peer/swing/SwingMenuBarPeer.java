/* SwingMenuBarPeer.java -- A Swing based peer for AWT menu bars
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

import java.awt.Container;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.peer.MenuBarPeer;

import javax.swing.JMenuBar;

/**
 * A Swing based peer for the AWT menu bar. This is a little bit different from
 * the other peers, since the AWT MenuBar is not derived from the AWT
 * component.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingMenuBarPeer
  implements MenuBarPeer
{

  /**
   * The AWT menu bar.
   */
  MenuBar awtMenuBar;

  /**
   * The Swing menu bar.
   */
  SwingMenuBar menuBar;

  /**
   * The peer of the frame that contains this menu bar.
   */
  SwingFramePeer framePeer;

  /**
   * A specialized JMenuBar that can be used as 'backend' for AWT MenuBars.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingMenuBar
    extends JMenuBar
  {
    /**
     * Overridden in order to provide a parent frame for this menu bar. The
     * menu bar still is not inside the component hierarchy, we are faking
     * here.
     */
    public Container getParent()
    {
      Container result = null;
      if (framePeer != null)
        result = (Container) framePeer.awtComponent;
      return result;
    }

    /**
     * Unconditionally returns <code>true</code>, since we assume that when the
     * menubar has a peer, it must be showing.
     *
     * @return <code>true</code>
     */
    public boolean isShowing()
    {
      // FIXME: This might be wrong. Maybe find a better way to do that.
      return true;
    }

    /**
     * Handles mouse events by forwarding it to
     * <code>processMouseEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleMouseEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseEvent(ev);
    }

    /**
     * Determines the menubar's screen location by asking the SwingFramePeer
     * for it.
     *
     * @return the screen location of the menu bar
     */
    public Point getLocationOnScreen()
    {
      return framePeer.getMenuLocationOnScreen();
    }
  }

  /**
   * Creates a new <code>SwingMenuBarPeer</code> instance.
   *
   * @param awtMenuBar the AWT menu bar
   */
  public SwingMenuBarPeer(MenuBar awtMenuBar)
  {
    this.awtMenuBar = awtMenuBar;
    menuBar = new SwingMenuBar();
    menuBar.setDoubleBuffered(false);
    // Add all the menus that are already in the MenuBar.
    for (int i = 0; i < awtMenuBar.getMenuCount(); i++)
      {
        Menu menu = awtMenuBar.getMenu(i);
        menu.addNotify();
        addMenu(awtMenuBar.getMenu(i));
      }
  }

  /**
   * Sets the <code>SwingFramePeer</code> of the frame that holds this menu.
   *
   * @param peer the <code>SwingFramePeer</code> to set
   */
  public void setFramePeer(SwingFramePeer peer)
  {
    framePeer = peer;
  }

  /**
   * Adds a menu to the menu bar.
   *
   * @param m the menu to add
   */
  public void addMenu(Menu m)
  {
    SwingMenuPeer menuPeer = (SwingMenuPeer) m.getPeer();
    menuBar.add(menuPeer.menu);
  }

  /**
   * Adds a help menu to the menu bar.
   *
   * @param menu the menu to add
   */
  public void addHelpMenu(Menu menu)
  {
    // FIXME: We should manage the help menu differently, so that it always
    // appears at the rightmost position.
    SwingMenuPeer menuPeer = (SwingMenuPeer) menu.getPeer();
    menuBar.add(menuPeer.menu);
  }

  /**
   * Removes the menu with the specified index.
   *
   * @param index the index of the menu to remove
   */
  public void delMenu(int index)
  {
    menuBar.remove(index);
  }

  /**
   * Disposes this peer. This releases any reference to the AWT and Swing
   * components.
   */
  public void dispose()
  {
    menuBar = null;
    awtMenuBar = null;
  }

  /**
   * Sets a font for the menu bar.
   *
   * @param font the font to set
   */
  public void setFont(Font font)
  {
    menuBar.setFont(font);
  }

  /**
   * Sets the width of the menu bar. This is called from the top level
   * component peers to adjust the width of the menubar when their sizes
   * change.
   *
   * @param w the width to set
   */
  public void setWidth(int w)
  {
    menuBar.setSize(w, menuBar.getPreferredSize().height);
    menuBar.doLayout();
  }

  /**
   * Paints the menu bar.
   *
   * @param g the graphics context to use for painting
   */
  public void peerPaint(Graphics g)
  {
    menuBar.paint(g);
  }

  /**
   * Determines the height of the menubar.
   *
   * @return the height of the menu bar
   */
  public int getHeight()
  {
    return menuBar.getPreferredSize().height;
  }

  /**
   * Handles mouse events.
   *
   * @param ev the mouse event
   */
  public void handleMouseEvent(MouseEvent ev)
  {
    Point point = ev.getPoint();
    for (int i = 0; i < awtMenuBar.getMenuCount(); i++)
      {
        Menu menu = awtMenuBar.getMenu(i);
        SwingMenuPeer peer = (SwingMenuPeer) menu.getPeer();
        int x1 = peer.getX();
        int x2 = x1 + peer.getWidth();
        if (point.x >= x1 && point.x <= x2)
          {
            ev.translatePoint(peer.getX(), peer.getY());
            peer.handleMouseEvent(ev);
            break;
          }
      }
  }

  /**
   * Handles mouse motion events.
   *
   * @param ev the mouse motion event
   */
  public void handleMouseMotionEvent(MouseEvent ev)
  {
    Point point = ev.getPoint();
    for (int i = 0; i < awtMenuBar.getMenuCount(); i++)
      {
        Menu menu = awtMenuBar.getMenu(i);
        SwingMenuPeer peer = (SwingMenuPeer) menu.getPeer();
        int x1 = peer.getX();
        int x2 = x1 + peer.getWidth();
        if (point.x >= x1 && point.x <= x2)
          {
            ev.translatePoint(peer.getX(), peer.getY());
            peer.handleMouseMotionEvent(ev);
            break;
          }
      }
  }
}
