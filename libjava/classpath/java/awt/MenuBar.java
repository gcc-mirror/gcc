/* MenuBar.java -- An AWT menu bar class
   Copyright (C) 1999, 2000, 2001, 2002, 2004, 2005, 2006
   Free Software Foundation, Inc.

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

import java.awt.peer.MenuBarPeer;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Vector;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
  * This class implements a menu bar in the AWT system.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @author Tom Tromey (tromey@redhat.com)
  * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
  */
public class MenuBar extends MenuComponent
  implements MenuContainer, Serializable, Accessible
{

  // Serialization Constant
  private static final long serialVersionUID = -4930327919388951260L;

  /**
   * The number used to generate the name returned by getName.
   */
  private static transient long next_menubar_number;

  /**
   * @serial The menu used for providing help information
   */
  private Menu helpMenu;

  /**
   * @serial The menus contained in this menu bar.
   */
  private Vector menus = new Vector();

  /**
   * Initializes a new instance of <code>MenuBar</code>.
   *
   * @throws HeadlessException if GraphicsEnvironment.isHeadless() is true
   */
  public MenuBar()
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException();
  }

  /**
   * Returns the help menu for this menu bar.  This may be <code>null</code>.
   *
   * @return the help menu for this menu bar
   */
  public Menu getHelpMenu()
  {
    return helpMenu;
  }

  /**
   * Sets the help menu for this menu bar.
   *
   * @param menu the new help menu for this menu bar
   */
  public synchronized void setHelpMenu(Menu menu)
  {
    MenuBarPeer myPeer = (MenuBarPeer) getPeer ();

    if (helpMenu != null)
      {
        if (myPeer != null)
          helpMenu.removeNotify();
        helpMenu.setParent(null);
      }
    helpMenu = menu;

    MenuContainer parent = menu.getParent();
    if (parent != null)
      parent.remove(menu);
    menu.setParent(this);

    if (myPeer != null)
      {
        menu.addNotify();
        myPeer.addHelpMenu(menu);
      }
  }

  /**
   * Add a menu to this MenuBar.  If the menu has already has a
   * parent, it is first removed from its old parent before being
   * added.
   *
   * @param menu the menu to add
   *
   * @return the menu that was added
   */
  public synchronized Menu add(Menu menu)
  {
    MenuBarPeer myPeer = (MenuBarPeer) getPeer ();

    MenuContainer parent = menu.getParent();
    if (parent != null)
      parent.remove(menu);

    menus.addElement(menu);
    menu.setParent(this);

    if (myPeer != null)
      {
        menu.addNotify();
        myPeer.addMenu(menu);
      }
    return menu;
  }

  /**
   * Removes the menu at the specified index.
   *
   * @param index the index of the menu to remove from the menu bar
   */
  public synchronized void remove(int index)
  {
    Menu m = (Menu) menus.remove(index);
    MenuBarPeer mp = (MenuBarPeer) getPeer();

    if (mp != null)
      m.removeNotify();

    m.setParent(null);

    if (mp != null)
      mp.delMenu(index);
  }

  /**
   * Removes the specified menu from the menu bar.
   *
   * @param menu the menu to remove from the menu bar
   */
  public void remove(MenuComponent menu)
  {
    int index = menus.indexOf(menu);
    if (index == -1)
      return;

    remove(index);
  }

  /**
   * Returns the number of elements in this menu bar.
   *
   * @return the number of elements in the menu bar
   */
  public int getMenuCount()
  {
    return countMenus();
  }

  /**
   * Returns the number of elements in this menu bar.
   *
   * @return the number of elements in the menu bar
   *
   * @deprecated This method is deprecated in favor of
   *             <code>getMenuCount()</code>.
   */
  public int countMenus()
  {
    return menus.size() + (getHelpMenu() == null ? 0 : 1);
  }

  /**
   * Returns the menu at the specified index.
   *
   * @param index the index of the menu
   *
   * @return the requested menu
   *
   * @throws ArrayIndexOutOfBoundsException if the index is not valid
   */
  public Menu getMenu(int index)
  {
    return (Menu) menus.elementAt(index);
  }

  /**
   * Creates this object's native peer.
   */
  public void addNotify()
  {
    MenuBarPeer peer = (MenuBarPeer) getPeer();
    if (peer == null)
      {
        peer = getToolkit().createMenuBar(this);
        setPeer(peer);
      }

    Enumeration e = menus.elements();
    while (e.hasMoreElements())
      {
        Menu mi = (Menu)e.nextElement();
        mi.addNotify();
        peer.addMenu(mi);
      }

    if (helpMenu != null)
      {
        helpMenu.addNotify();
        peer.addHelpMenu(helpMenu);
      }
  }

  /**
   * Destroys this object's native peer.
   */
  public void removeNotify()
  {
    Enumeration e = menus.elements();
    while (e.hasMoreElements())
      {
        Menu mi = (Menu) e.nextElement();
        mi.removeNotify();
      }
    super.removeNotify();
  }

  /**
   * Returns a list of all shortcuts for the menus in this menu bar.
   *
   * @return a list of all shortcuts for the menus in this menu bar
   */
  public synchronized Enumeration<MenuShortcut> shortcuts()
  {
    Vector shortcuts = new Vector();
    Enumeration e = menus.elements();

    while (e.hasMoreElements())
      {
        Menu menu = (Menu)e.nextElement();
        if (menu.getShortcut() != null)
          shortcuts.addElement(menu.getShortcut());
      }

    return shortcuts.elements();
  }

  /**
   * Returns the menu item for the specified shortcut, or <code>null</code>
   * if no such item exists.
   *
   * @param shortcut the shortcut to return the menu item for
   *
   * @return the menu item for the specified shortcut
   */
  public MenuItem getShortcutMenuItem(MenuShortcut shortcut)
  {
    Enumeration e = menus.elements();

    while (e.hasMoreElements())
      {
        Menu menu = (Menu) e.nextElement();
        MenuShortcut s = menu.getShortcut();
        if ((s != null) && s.equals(shortcut))
          return menu;
      }

    return null;
  }

  /**
   * Deletes the specified menu shortcut.
   *
   * @param shortcut the shortcut to delete
   */
  public void deleteShortcut(MenuShortcut shortcut)
  {
    MenuItem it;
    // This is a slow implementation, but it probably doesn't matter.
    while ((it = getShortcutMenuItem (shortcut)) != null)
      it.deleteShortcut();
  }

  /**
   * Gets the AccessibleContext associated with this <code>MenuBar</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    // Create the context if this is the first request.
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTMenuBar();
    return accessibleContext;
  }

  /**
   * Generate a unique name for this <code>MenuBar</code>.
   *
   * @return A unique name for this <code>MenuBar</code>.
   */
  String generateName()
  {
    return "menubar" + getUniqueLong();
  }

  private static synchronized long getUniqueLong()
  {
    return next_menubar_number++;
  }

  /**
   * This class provides accessibility support for AWT menu bars.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  protected class AccessibleAWTMenuBar
    extends AccessibleAWTMenuComponent
  {

    /**
     * Compatible with JDK 1.4.2 revision 5
     */
    private static final long serialVersionUID = -8577604491830083815L;

    /**
     * This is the default constructor, which simply calls the default
     * constructor of the superclass.
     */
    protected AccessibleAWTMenuBar()
    {
      super();
    }

    /**
     * Returns the accessible role relating to the menu bar.
     *
     * @return <code>AccessibleRole.MENU_BAR</code>
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.MENU_BAR;
    }

  }

}
