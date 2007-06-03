/* GtkMenuPeer.java -- Implements MenuPeer with GTK+
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

import java.awt.Component;
import java.awt.Menu;
import java.awt.MenuContainer;
import java.awt.MenuItem;
import java.awt.MenuShortcut;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.MenuPeer;

public class GtkMenuPeer extends GtkMenuItemPeer
  implements MenuPeer
{
  /**
   * Creates the associated gtk+ widget and stores it in the nsa table
   * for this peer. Called by the create() method with the label name
   * of the associated MenuItem. Overridden to greate a Menu widget.
   */
  protected native void create (String label);

  private native void addItem(MenuItemPeer item, int key,
			      boolean shiftModifier);

  /** XXX - Document this and the override in GtkPopupMenuPeer. */
  native void setupAccelGroup (GtkGenericPeer container);

  private native void addTearOff ();

  /**
   * Overridden to not connect any signals.
   */
  protected void connectSignals()
  {
    // No signals to connect.
  }

  public GtkMenuPeer (Menu menu)
  {
    super (menu);
    
    if (menu.isTearOff())
      addTearOff();

    MenuContainer parent = menu.getParent ();
    if (parent instanceof Menu)
      setupAccelGroup ((GtkMenuPeer)((Menu)parent).getPeer ());
    else if (parent instanceof Component)
      setupAccelGroup ((GtkComponentPeer)((Component)parent).getPeer ());
    else
      setupAccelGroup (null); // XXX, should we warn about unknown parent?
  }

  public void addItem (MenuItem item)
  {
    int key = 0;
    boolean shiftModifier = false;

    MenuShortcut ms = item.getShortcut ();
    if (ms != null)
      {
        key = ms.getKey ();
        shiftModifier = ms.usesShiftModifier ();
      }

    addItem ((MenuItemPeer) item.getPeer (), key, shiftModifier);
  }

  public void addItem (MenuItemPeer item, MenuShortcut ms)
  {
    int key = 0;
    boolean shiftModifier = false;

    if (ms != null)
      {
        key = ms.getKey ();
        shiftModifier = ms.usesShiftModifier ();
      }

    addItem (item, key, shiftModifier);
  }

  public native void delItem(int index);

  public void addSeparator()
  {
    // FIXME: implement
  }
}
