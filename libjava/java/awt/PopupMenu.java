/* PopupMenu.java -- An AWT popup menu
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.awt;

import java.awt.peer.PopupMenuPeer;
import java.awt.peer.MenuPeer;
import java.awt.peer.MenuItemPeer;
import java.awt.peer.MenuComponentPeer;
/**
  * This class implement an AWT popup menu widget
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PopupMenu extends Menu implements java.io.Serializable
{

/*
 * Static Variables
 */

// Serialization Constant
private static final long serialVersionUID = -4620452533522760060L;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>PopupMenu</code>.
  */
public
PopupMenu()
{
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>PopupMenu</code> with the specified
  * label.
  *
  * @param label The label for this popup menu.
  */
public
PopupMenu(String label)
{
  super(label);
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Creates this object's native peer.
  */
public void
addNotify()
{
  if (peer != null)
    peer = getToolkit ().createPopupMenu (this);
  super.addNotify ();
}

/*************************************************************************/

/**
  * Displays this popup menu at the specified coordinates relative to
  * the specified component.
  *
  * @param component The component to which the display coordinates are relative.
  * @param x The X coordinate of the menu.
  * @param y The Y coordinate of the menu.
  */
public void
show(Component component, int x, int y)
{
  PopupMenuPeer pmp = (PopupMenuPeer)getPeer();
  if (pmp != null)
    {
      /* XXX
      Event e = new Event (component, Event.ACTION_EVENT, component);
      e.x = x;
      e.y = y;*/
      pmp.show (component, x, y);
    }
}

} // class PopupMenu

