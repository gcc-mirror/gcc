/* SwingMenuItemPeer.java -- A Swing based peer for AWT menu items
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
import java.awt.MenuItem;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.peer.MenuItemPeer;

import javax.swing.JMenuItem;

/**
 * A Swing based peer for the AWT MenuItem.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingMenuItemPeer
  implements MenuItemPeer
{
  /**
   * The AWT menu item.
   */
  MenuItem awtMenuItem;

  /**
   * The Swing menu item.
   */
  JMenuItem menuItem;

  /**
   * Receives ActionEvents from the Swing menu item and forwards them
   * to the ActionListeners of the AWT MenuItem.
   *
   * @author Roman Kennke (kennke@aicas.com)
   */
  private class SwingMenuItemListener implements ActionListener
  {

    /**
     * Receives notification when the action has been performed.
     *
     * @param event the action event
     */
    public void actionPerformed(ActionEvent event)
    {
      event.setSource(awtMenuItem);
      Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(event);
    }
    
  }

  /**
   * Creates a new instance of <code>SwingMenuItemPeer</code>.
   *
   * @param awtMenuItem the AWT menu item
   */
  public SwingMenuItemPeer(MenuItem awtMenuItem)
  {
    this.awtMenuItem = awtMenuItem;
    menuItem = new JMenuItem(awtMenuItem.getLabel());
    menuItem.addActionListener(new SwingMenuItemListener());
  }

  /**
   * Disables the menu item.
   */
  public void disable()
  {
    menuItem.setEnabled(false);
  }

  /**
   * Enables the menu item.
   */
  public void enable()
  {
    menuItem.setEnabled(true);
  }

  /**
   * Sets the enabled state to <code>enabled</code>.
   *
   * @param enabled if the menu item should be enabled or not
   */
  public void setEnabled(boolean enabled)
  {
    menuItem.setEnabled(enabled);
  }

  /**
   * Sets the label for the menu item.
   *
   * @param text the label to set
   */
  public void setLabel(String text)
  {
    menuItem.setText(text);
  }

  /**
   * Disposes the menu item. This releases any reference to the Swing and AWT
   * menu item.
   */
  public void dispose()
  {
    menuItem = null;
    awtMenuItem = null;
  }

  /**
   * Sets the font for this menu item.
   *
   * @param font the font to set
   */
  public void setFont(Font font)
  {
    menuItem.setFont(font);
  }

}
