/* GtkMenuItemPeer.java -- Implements MenuItemPeer with GTK+
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

import java.awt.MenuItem;
import java.awt.peer.MenuItemPeer;

public class GtkMenuItemPeer extends GtkMenuComponentPeer
  implements MenuItemPeer
{
  /**
   * Creates the associated gtk+ widget and stores it in the nsa table
   * for this peer. Called by the create() method with the label name
   * of the associated MenuItem. Needs to be overridden my subclasses
   * that want to create a different gtk+ widget.
   */
  protected native void create (String label);

  /**
   * Called from constructor to enable signals from an item. If a
   * subclass needs different (or no) signals connected this method
   * should be overridden.
   */
  protected native void connectSignals ();

  /**
   * Overridden to set font on menu item label.
   */
  protected native void gtkWidgetModifyFont(String name, int style, int size);

  /**
   * Creates the associated gtk+ widget and stores it in the nsa table
   * for this peer. Called by the (super class) constructor.
   * Overridden to get the label if the assiociated MenuItem and to
   * call create(String).
   */
  protected void create()
  {
    create (((MenuItem) awtWidget).getLabel());
  }

  /**
   * Creates a new GtkMenuItemPeer associated with the given MenuItem.
   * It will call create(), setFont(), setEnabled() and
   * connectSignals() in that order.
   */
  public GtkMenuItemPeer(MenuItem item)
  {
    super(item);
    setEnabled (item.isEnabled());
    connectSignals();
  }

  /**
   * Calls setEnabled(false).
   */
  public void disable()
  {
    setEnabled(false);
  }

  /**
   * Calls setEnabled(true).
   */
  public void enable()
  {
    setEnabled(true);
  }

  public native void setEnabled(boolean b);
  public native void setLabel(String label);

  /**
   * Callback setup through connectSignals().
   */
  protected void postMenuActionEvent ()
  {
    postActionEvent (((MenuItem)awtWidget).getActionCommand (), 0);
  }
}
