/* Menu.java -- A Java AWT Menu
   Copyright (C) 1999, 2002, 2004 Free Software Foundation, Inc.

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
import java.io.Serializable;
import java.util.Vector;
import java.util.Enumeration;

/**
  * This class represents a pull down or tear off menu in Java's AWT.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Menu extends MenuItem implements MenuContainer, Serializable
{

/*
 * Static Variables
 */

// Serialization Constant
private static final long serialVersionUID = -8809584163345499784L;

/*************************************************************************/

/*
 * Instance Variables
 */

/**
  * @serial The actual items in the menu
  */
private Vector items = new Vector();

/**
  * @serial Flag indicating whether or not this menu is a tear off
  */
private boolean isTearOff;

/**
  * @serial Indicates whether or not this is a help menu.
  */
private boolean isHelpMenu;

// From the serialization spec.  FIXME: what should it be?
private int menuSerializedDataVersion;

static final String separatorLabel = "-";

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Initializes a new instance of <code>Menu</code> with no label and that
  * is not a tearoff;
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
Menu()
{
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Menu</code> that is not a tearoff and
  * that has the specified label.
  *
  * @param label The menu label.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
Menu(String label)
{
  this(label, false);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>Menu</code> with the specified
  * label and tearoff status.
  *
  * @param label The label for this menu
  * @param isTearOff <code>true</code> if this menu is a tear off menu,
  * <code>false</code> otherwise.
  *
  * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
  */
public
Menu(String label, boolean isTearOff)
{
  super(label);

  this.isTearOff = isTearOff;

  if (label.equals("Help"))
    isHelpMenu = true;

  if (GraphicsEnvironment.isHeadless())
    throw new HeadlessException ();
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Tests whether or not this menu is a tearoff.
  *
  * @return <code>true</code> if this menu is a tearoff, <code>false</code>
  * otherwise.
  */
public boolean
isTearOff()
{
  return(isTearOff);
}

/*************************************************************************/

/**
  * Returns the number of items in this menu.
  *
  * @return The number of items in this menu.
  */
public int
getItemCount()
{
  return countItems ();
}

/**
 * Returns the number of items in this menu.
 *
 * @return The number of items in this menu.
 *
 * @deprecated As of JDK 1.1, replaced by getItemCount().
 */
public int countItems ()
{
  return items.size ();
}
 
/*************************************************************************/

/**
  * Returns the item at the specified index.
  *
  * @return The item at the specified index.
  *
  * @exception ArrayIndexOutOfBoundsException If the index value is not valid.
  */
public MenuItem
getItem(int index)
{
  return((MenuItem)items.elementAt(index));
}

/*************************************************************************/

/**
  * Adds the specified item to this menu.  If it was previously part of
  * another menu, it is first removed from that menu.
  *
  * @param item The new item to add.
  *
  * @return The item that was added.
  */
public MenuItem
add(MenuItem item)
{
  items.addElement(item);
  if (item.parent != null)
    {
      item.parent.remove(item);
    }
  item.parent = this;

  if (peer != null)
    {
      MenuPeer mp = (MenuPeer) peer;
      mp.addItem(item);
    }

  return item;
}

/*************************************************************************/

/**
  * Add an item with the specified label to this menu.
  *
  * @param label The label of the menu item to add.
  */
public void
add(String label)
{
  add(new MenuItem(label));
}

/*************************************************************************/

/**
  * Inserts the specified menu item into this menu at the specified index.
  *
  * @param item The menu item to add.
  * @param index The index of the menu item.
  *
  * XXX: FIXME
  *
  * @exception IllegalArgumentException If the index is less than zero.
  * @exception ArrayIndexOutOfBoundsException If the index is otherwise invalid.
  */
public void
insert(MenuItem item, int index)
{
  if (index < 0)
    throw new IllegalArgumentException("Index is less than zero");

  items.insertElementAt(item, index);

  MenuPeer mp = (MenuPeer)getPeer();
  // FIXME: Need to add a peer method here.
//    if (mp != null)
//      mp.insertItem(item, index);
}

/*************************************************************************/

/**
  * Inserts an item with the specified label into this menu at the specified index.
  *
  * @param label The label of the item to add.
  * @param index The index of the menu item.
  *
  * @exception IllegalArgumentException If the index is less than zero.
  * @exception ArrayIndexOutOfBoundsException If the index is otherwise invalid.
  */
public void
insert(String label, int index)
{
  insert(new MenuItem(label), index);
}

/*************************************************************************/

/**
  * Adds a separator bar at the current menu location.
  */
public void
addSeparator()
{
  add(new MenuItem(separatorLabel));
}

/*************************************************************************/

/**
  * Inserts a separator bar at the specified index value.
  *
  * @param index The index at which to insert a separator bar.
  *
  * XXX: FIXME
  *
  * @exception IllegalArgumentException If the index is less than zero.
  * @exception ArrayIndexOutOfBoundsException If the index is otherwise invalid.
  */
public void
insertSeparator(int index)
{
  insert(new MenuItem(separatorLabel), index);
}

/*************************************************************************/

/**
  * Deletes the item at the specified index from this menu.
  *
  * @param index The index of the item to remove.
  * 
  * @exception ArrayIndexOutOfBoundsException If the index is otherwise invalid.
  */
public synchronized void
remove(int index)
{
  items.removeElementAt(index);

  MenuPeer mp = (MenuPeer)getPeer();
  if (mp != null)
    mp.delItem(index);
}

/*************************************************************************/

/**
  * Removes the specifed item from the menu.  If the specified component
  * does not exist, this method does nothing. // FIXME: Right?
  *
  * @param item The component to remove.
  */
public void
remove(MenuComponent item)
{
  int index = items.indexOf(item);
  if (index == -1)
    return;

  remove(index);
}

/*************************************************************************/

/**
  * Removes all the elements from this menu.
  */
public synchronized void
removeAll()
{
  int count = getItemCount();
  for(int i = 0; i < count; i++)
    {
      // We must always remove item 0.
      remove(0);
    }
}

/*************************************************************************/

/**
  * Creates the native peer for this object.
  */
public void
addNotify()
{
  if (peer == null)
    peer = getToolkit().createMenu(this);
  Enumeration e = items.elements();
  while (e.hasMoreElements())
  {
    MenuItem mi = (MenuItem)e.nextElement();
    mi.addNotify();
  }    
  super.addNotify ();
}

/*************************************************************************/

/**
  * Destroys the native peer for this object.
  */
public void
removeNotify()
{
  Enumeration e = items.elements();
  while (e.hasMoreElements())
  {
    MenuItem mi = (MenuItem) e.nextElement();
    mi.removeNotify();
  }
  super.removeNotify();
}

/*************************************************************************/

/**
  * Returns a debugging string for this menu.
  *
  * @return A debugging string for this menu.
  */
public String
paramString()
{
  return (",isTearOff=" + isTearOff + ",isHelpMenu=" + isHelpMenu
	  + super.paramString());
}

// Accessibility API not yet implemented.
// public AccessibleContext getAccessibleContext()

} // class Menu
