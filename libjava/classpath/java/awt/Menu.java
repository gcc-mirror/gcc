/* Menu.java -- A Java AWT Menu
   Copyright (C) 1999, 2002, 2004, 2006 Free Software Foundation, Inc.

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


package java.awt;

import java.awt.peer.MenuPeer;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;

import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
  * This class represents a pull down or tear off menu in Java's AWT.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class Menu extends MenuItem implements MenuContainer, Serializable
{

  /**
   * The number used to generate the name returned by getName.
   */
  private static transient long next_menu_number;

  // Serialization Constant
  private static final long serialVersionUID = -8809584163345499784L;

  /**
    * @serial The actual items in the menu
    */
  private Vector items = new Vector();

  /**
   * @serial Flag indicating whether or not this menu is a tear off
   */
  private boolean tearOff;

  /**
   * @serial Indicates whether or not this is a help menu.
   */
  private boolean isHelpMenu;

  /*
   * @serial Unused in this implementation, but present in Sun's
   * serialization spec.  Value obtained via reflection.
   */
  private int menuSerializedDataVersion = 1;

  static final transient String separatorLabel = "-";

  /**
   * Initializes a new instance of <code>Menu</code> with no label and that
   * is not a tearoff;
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public Menu()
  {
  }

  /**
   * Initializes a new instance of <code>Menu</code> that is not a tearoff and
   * that has the specified label.
   *
   * @param label The menu label.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public Menu(String label)
  {
    this(label, false);
  }

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
  public Menu(String label, boolean isTearOff)
  {
    super(label);

    tearOff = isTearOff;

    if (label.equals("Help"))
      isHelpMenu = true;

    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();
  }

  /**
   * Tests whether or not this menu is a tearoff.
   *
   * @return <code>true</code> if this menu is a tearoff, <code>false</code>
   * otherwise.
   */
  public boolean isTearOff()
  {
    return(tearOff);
  }

  /**
   * Returns the number of items in this menu.
   *
   * @return The number of items in this menu.
   */
  public int getItemCount()
  {
    return countItems();
  }

  /**
   * Returns the number of items in this menu.
   *
   * @return The number of items in this menu.
   *
   * @deprecated As of JDK 1.1, replaced by getItemCount().
   */
  public int countItems()
  {
    return items.size();
  }

  /**
   * Returns the item at the specified index.
   *
   * @param index  the item index.
   *
   * @return The item at the specified index.
   *
   * @exception ArrayIndexOutOfBoundsException If the index value is not valid.
   */
  public MenuItem getItem(int index)
  {
    return((MenuItem) items.elementAt(index));
  }

  /**
   * Adds the specified item to this menu.  If it was previously part of
   * another menu, it is first removed from that menu.
   *
   * @param item The new item to add.
   *
   * @return The item that was added.
   */
  public MenuItem add(MenuItem item)
  {
    MenuContainer parent = item.getParent();
    if (parent != null)
      parent.remove(item);

    items.addElement(item);
    item.setParent(this);

    if (peer != null)
      {
        item.addNotify();
        MenuPeer mp = (MenuPeer) peer;
        mp.addItem(item);
      }

    return item;
  }

  /**
   * Add an item with the specified label to this menu.
   *
   * @param label The label of the menu item to add.
   */
  public void add(String label)
  {
    add(new MenuItem(label));
  }

  /**
   * Inserts the specified menu item into this menu at the specified index.  If
   * the index is greater than or equal to the number of items already in the
   * menu, the new item is added as the last item in the menu.
   *
   * @param item The menu item to add (<code>null</code> not permitted).
   * @param index The index of the menu item (>= 0).
   *
   * @throws IllegalArgumentException if the index is less than zero.
   * @throws NullPointerException if <code>item</code> is <code>null</code>.
   */
  public void insert(MenuItem item, int index)
  {
    if (index < 0)
      throw new IllegalArgumentException("Index is less than zero");

    int count = getItemCount();

    if (index >= count)
      add(item);
    else
      {
        MenuContainer parent = item.getParent();
        if (parent != null)
          parent.remove(item);

        items.insertElementAt(item, index);
        item.setParent(this);

        MenuPeer peer = (MenuPeer) getPeer();
        if (peer == null)
          return;

        for (int i = count - 1; i >= index; i--)
          peer.delItem(i);

        item.addNotify();
        peer.addItem(item);

        // bear in mind that count is the number of items *before* the new
        // item was added
        for (int i = index + 1; i <= count; i++)
          peer.addItem((MenuItem) items.elementAt(i));
      }

  }

  /**
   * Inserts an item with the specified label into this menu at the specified
   * index.  If the index is greater than or equal to the number of items
   * already in the menu, the new item is added as the last item in the menu.
   *
   * @param label The label of the item to add.
   * @param index The index of the menu item (>= 0).
   *
   * @throws IllegalArgumentException If the index is less than zero.
   */
  public void insert(String label, int index)
  {
    insert(new MenuItem(label), index);
  }

  /**
   * Adds a separator bar at the current menu location.
   */
  public void addSeparator()
  {
    add(new MenuItem(separatorLabel));
  }

  /**
   * Inserts a separator bar at the specified index value.
   *
   * @param index The index at which to insert a separator bar.
   *
   * @exception IllegalArgumentException If the index is less than zero.
   * @exception ArrayIndexOutOfBoundsException If the index is otherwise invalid.
   */
  public void insertSeparator(int index)
  {
    insert(new MenuItem(separatorLabel), index);
  }

  /**
   * Deletes the item at the specified index from this menu.
   *
   * @param index The index of the item to remove.
   *
   * @exception ArrayIndexOutOfBoundsException If the index is otherwise invalid.
   */
  public synchronized void remove(int index)
  {
    MenuItem item = (MenuItem) items.remove(index);

    MenuPeer mp = (MenuPeer) getPeer();
    if (mp != null)
      {
        mp.delItem(index);
        item.removeNotify();
      }
    item.setParent(null);
  }

  /**
   * Removes the specifed item from the menu.  If the specified component
   * does not exist, this method does nothing.
   *
   * @param item The component to remove.
   */
  public void remove(MenuComponent item)
  {
    int index = items.indexOf(item);
    if (index == -1)
      return;

    remove(index);
  }

  /**
   * Removes all the elements from this menu.
   */
  public synchronized void removeAll()
  {
    int count = getItemCount();
    for(int i = 0; i < count; i++)
      {
        // We must always remove item 0.
        remove(0);
      }
  }

  /**
   * Creates the native peer for this object.
   */
  public void addNotify()
  {
    MenuPeer peer = (MenuPeer) getPeer();
    if (peer == null)
      {
        peer = getToolkit().createMenu(this);
        setPeer(peer);
      }

    Enumeration e = items.elements();
    while (e.hasMoreElements())
    {
      MenuItem mi = (MenuItem)e.nextElement();
      mi.addNotify();
      peer.addItem(mi);
    }

    super.addNotify();
  }

  /**
   * Destroys the native peer for this object.
   */
  public void removeNotify()
  {
    Enumeration e = items.elements();
    while (e.hasMoreElements())
    {
      MenuItem mi = (MenuItem) e.nextElement();
      mi.removeNotify();
    }
    super.removeNotify();
  }

  /**
   * Returns a debugging string for this menu.
   *
   * @return A debugging string for this menu.
   */
  public String paramString()
  {
    return (",tearOff=" + tearOff + ",isHelpMenu=" + isHelpMenu
            + super.paramString());
  }

  /**
   * Basic Accessibility class for Menu.  Details get provided in derived
   * classes.
   */
  protected class AccessibleAWTMenu extends AccessibleAWTMenuItem
  {
    private static final long serialVersionUID = 5228160894980069094L;

    protected AccessibleAWTMenu()
    {
    }

    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.MENU;
    }
  }

  /**
   * Gets the AccessibleContext associated with this <code>Menu</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTMenu();
    return accessibleContext;
  }

  /**
   * Generate a unique name for this <code>Menu</code>.
   *
   * @return A unique name for this <code>Menu</code>.
   */
  String generateName()
  {
    return "menu" + getUniqueLong();
  }

  private static synchronized long getUniqueLong()
  {
    return next_menu_number++;
  }

} // class Menu
