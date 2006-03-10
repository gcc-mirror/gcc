/* SwingMenuPeer.java -- A Swing based peer for AWT menus
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

import java.awt.Font;
import java.awt.Menu;
import java.awt.MenuItem;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.peer.MenuPeer;

import javax.swing.JMenu;

/**
 * A Swing based peer for the AWT menu.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingMenuPeer
  implements MenuPeer
{

  /**
   * The AWT menu.
   */
  Menu awtMenu;

  /**
   * The Swing menu.
   */
  SwingMenu menu;

  /**
   * A specialized JMenu that can be used as 'backend' for an AWT menu.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingMenu
    extends JMenu
  {

    /**
     * Unconditionally returns <code>true</code>, since we assume that when the
     * menu has a peer, it must be showing.
     *
     * @return <code>true</code>
     */
    public boolean isShowing()
    {
      // FIXME: This might be wrong. Maybe find a better way to do that.
      return true;
    }

    /**
     * Overridden so that we can provide a location even without a real peer
     * attached.
     *
     * @return the screen location of this menu
     */
    public Point getLocationOnScreen()
    {
      Point parentLoc = getParent().getLocationOnScreen();
      parentLoc.x += getX();
      parentLoc.y += getY();
      return parentLoc;
    }

    /**
     * Handles mouse events by forwarding them to
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
     * Handles mouse events by forwarding them to
     * <code>processMouseMotionEvent()</code>.
     *
     * @param ev the mouse event
     */
    public void handleMouseMotionEvent(MouseEvent ev)
    {
      ev.setSource(this);
      processMouseMotionEvent(ev);
    }
  }

  /**
   * Creates a new <code>SwingMenuPeer</code> instance.
   *
   * @param awtMenu the AWT menu
   */
  public SwingMenuPeer(Menu awtMenu)
  {
    this.awtMenu = awtMenu;
    menu = new SwingMenu();
    menu.setDoubleBuffered(false);
    menu.setText(awtMenu.getLabel());
    for (int i = 0; i < awtMenu.getItemCount(); i++)
      {
        MenuItem item = awtMenu.getItem(i);
        item.addNotify();
        SwingMenuItemPeer peer = (SwingMenuItemPeer) item.getPeer(); 
        menu.add(peer.menuItem);
      }
  }

  /**
   * Adds a menu item to this menu.
   *
   * @param item the menu item to add
   */
  public void addItem(MenuItem item)
  {
    SwingMenuItemPeer menuItemPeer = (SwingMenuItemPeer) item.getPeer();
    menu.add(menuItemPeer.menuItem);
  }

  /**
   * Adds a separator to the menu.
   */
  public void addSeparator()
  {
    menu.addSeparator();
  }

  /**
   * Removes a menu item from the menu.
   *
   * @param index the index of the menu item to remove
   */
  public void delItem(int index)
  {
    menu.remove(index);
  }

  /**
   * Disables the menu.
   */
  public void disable()
  {
    menu.setEnabled(false);
  }

  /**
   * Enables the menu.
   */
  public void enable()
  {
    menu.setEnabled(true);
  }

  /**
   * Sets the enabled state of the menu to <code>enabled</code>.
   *
   * @param enabled if the menu should be enabled or not
   */
  public void setEnabled(boolean enabled)
  {
    menu.setEnabled(enabled);
  }

  /**
   * Sets the label of the menu.
   *
   * @param text the label to set
   */
  public void setLabel(String text)
  {
    menu.setText(text);
  }

  /**
   * Releases any reference to the AWT and Swing menu instances.
   */
  public void dispose()
  {
    menu = null;
    awtMenu = null;
  }

  /**
   * Sets the font for the menu.
   *
   * @param font the font to set
   */
  public void setFont(Font font)
  {
    menu.setFont(font);
  }

  /**
   * Handles mouse events by forwarding them to the Swing menu.
   *
   * @param ev the mouse event
   */
  public void handleMouseEvent(MouseEvent ev)
  {
    menu.handleMouseEvent(ev);
  }

  /**
   * Handles mouse motion events by forwarding them to the Swing menu.
   *
   * @param ev the mouse event
   */
  public void handleMouseMotionEvent(MouseEvent ev)
  {
    menu.handleMouseMotionEvent(ev);
  }

  /**
   * Returns the X coordinate of the upper left corner of the menu. This is
   * used internally by the SwingMenuBarPeer.
   *
   * @return the X coordinate of the upper left corner of the menu
   */
  int getX()
  {
    return menu.getX();
  }

  /**
   * Returns the width of the menu. This is used internally by the
   * SwingMenuBarPeer.
   *
   * @return the X coordinate of the upper left corner of the menu
   */
  int getWidth()
  {
    return menu.getWidth();
  }

  /**
   * Returns the Y coordinate of the upper left corner of the menu. This is
   * used internally by the SwingMenuBarPeer.
   *
   * @return the X coordinate of the upper left corner of the menu
   */
  public int getY()
  {
    return menu.getY();
  }
}
