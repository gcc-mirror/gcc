/* GtkMenuBarPeer.java -- Implements MenuBarPeer with GTK+
   Copyright (C) 1999, 2005, 2006  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.peer.MenuBarPeer;

public class GtkMenuBarPeer extends GtkMenuComponentPeer
  implements MenuBarPeer
{
  /** Whether we already have an help menu set on this peer. */
  private boolean hasHelpMenu;

  /**
   * Creates the gtk+ widget for this peer and puts it in the nsa
   * table. Called from the (super class) constructor.
   */
  protected native void create();

  /**
   * Adds a new GtkMenuPeer to the end of the GtkMenuBarPeer.
   */
  private native void addMenu(GtkMenuPeer menu);

  /**
   * Creates a new GtkMenuBarPeer associated with the given MenuBar.
   */
  public GtkMenuBarPeer(MenuBar menubar)
  {
    super(menubar);
  }

  /**
   * Adds a help menu to this MenuBar. Gnome styleguides say the help
   * menu is just the last item in the menubar (they are NOT right
   * justified).
   */
  public void addHelpMenu (Menu menu)
  {
    if (hasHelpMenu)
      {
        // Remove the (help) menu, which is after all the other items.
        delMenu(((MenuBar) awtWidget).getMenuCount());
        hasHelpMenu = false;
      }

    if (menu != null)
      {
        addMenu(menu);
        hasHelpMenu = true;
      }
  }

  /**
   * Deletes the menu at (zero-based) index from this GtkMenuBar.
   */
  public native void delMenu(int index);

  /**
   * Adds the GtkMenuPeer associated with the Menu to this
   * GtkMenuBarPeer. Makes sure that any help menus keep the last menu
   * on the bar.
   */
  public void addMenu(Menu m)
  {
    // Make sure the help menu is the last one.
    if (hasHelpMenu)
      {
        addHelpMenu(null);
        addMenu((GtkMenuPeer) m.getPeer());
        addHelpMenu(((MenuBar) awtWidget).getHelpMenu());
      }
    else
      addMenu((GtkMenuPeer) m.getPeer());
  }
}
