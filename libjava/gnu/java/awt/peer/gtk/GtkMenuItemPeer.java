/* GtkMenuItemPeer.java -- Implements MenuItemPeer with GTK+
   Copyright (C) 1999, 2005  Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import java.awt.Font;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuComponent;
import java.awt.MenuItem;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.MenuPeer;

public class GtkMenuItemPeer extends GtkMenuComponentPeer
  implements MenuItemPeer
{
  native void create (String label);
  native void connectSignals ();
  native void gtkWidgetModifyFont (String name, int style, int size);

  void create ()
  {
    create (((MenuItem) awtWidget).getLabel());
  }

  public GtkMenuItemPeer (MenuItem item)
  {
    super (item);
    setEnabled (item.isEnabled ());
    setParent (item);

    if (item.getParent() instanceof Menu && ! (item instanceof Menu))
      connectSignals();
  }

  void setFont ()
  {
    MenuComponent mc = ((MenuComponent) awtWidget);
    Font f = mc.getFont ();

    if (f == null)
      {
        MenuComponent parent = (MenuComponent) mc.getParent ();
        Font pf = parent.getFont ();
        gtkWidgetModifyFont (pf.getName (), pf.getStyle (), pf.getSize ());
      }
    else
      gtkWidgetModifyFont(f.getName(), f.getStyle(), f.getSize());
  }

  void setParent (MenuItem item)
  {
    // add ourself differently, based on what type of parent we have
    // yes, the typecasting here is nasty.
    Object parent = item.getParent ();
    if (parent instanceof MenuBar)
      {
	((GtkMenuBarPeer)((MenuBar)parent).getPeer ()).addMenu ((MenuPeer) this);
      }
    else // parent instanceof Menu
      {
	((GtkMenuPeer)((Menu)parent).getPeer ()).addItem (this, 
							  item.getShortcut ());
      }
  }

  public void disable ()
  {
    setEnabled (false);
  }

  public void enable ()
  {
    setEnabled (true);
  }

  public native void setEnabled(boolean b);

  public native void setLabel(String label);

  protected void postMenuActionEvent ()
  {
    postActionEvent (((MenuItem)awtWidget).getActionCommand (), 0);
  }
}
