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


package java.awt;

import java.awt.peer.MenuPeer;
import java.awt.peer.PopupMenuPeer;

/**
  * This class implement an AWT popup menu widget
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class PopupMenu extends Menu
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
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true.
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
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless()
  * returns true.
  */
public
PopupMenu(String label)
{
  super(label);

  if (GraphicsEnvironment.isHeadless())
    throw new HeadlessException ();
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
  if (peer == null)
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
  if (getPeer() == null)
    this.addNotify();
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

