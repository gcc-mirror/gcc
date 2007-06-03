/* GtkMenuComponentPeer.java -- Implements MenuComponentPeer with GTK+
   Copyright (C) 1999, 2006 Free Software Foundation, Inc.

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

import java.awt.Font;
import java.awt.MenuComponent;
import java.awt.MenuContainer;
import java.awt.peer.MenuComponentPeer;

public abstract class GtkMenuComponentPeer extends GtkGenericPeer
  implements MenuComponentPeer
{
  /**
   * Creates the associated gtk+ widget and stores it in the nsa table
   * for this peer. Called by the constructor.
   */
  protected abstract void create ();

  /**
   * Sets font based on MenuComponent font, or containing menu(bar)
   * parent font.
   */
  private void setFont()
  {
    MenuComponent mc = ((MenuComponent) awtWidget);
    Font f = mc.getFont();
    
    if (f == null)
      {
        MenuContainer parent = mc.getParent ();
        // Submenus inherit the font of their containing Menu(Bar).
        if (parent instanceof MenuComponent)
          f = parent.getFont ();
      }

    setFont(f);
  }

  /**
   * Will call the abstract <code>create()</code> that needs to be
   * overridden by subclasses, to create the MenuComponent. It will
   * then correctly setup the font for the component based on the
   * component and/or its containing parent component.
   */
  public GtkMenuComponentPeer(MenuComponent component)
  {
    super(component);
    create();
    setFont();
  }

  /**
   * Removes the awtWidget components from the native state tables.
   * Subclasses should call <code>super.dispose()</code> if they don't
   * remove these themselves.
   */
  public native void dispose();

  /**
   * Sets the font for this particular MenuComponent only (not any
   * containing items, if any).
   */
  public void setFont(Font font)
  {
    if (font != null)
      gtkWidgetModifyFont(font);
  }
}
