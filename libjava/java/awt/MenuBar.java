/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.MenuBarPeer;
import java.util.Vector;
import java.util.Enumeration;
import java.util.NoSuchElementException;

/** This class implements a MenuBar, such as might appear across the
 * top of a window.
 * @author Tom Tromey <tromey@redhat.com>
 * @date December 25, 2000
 */
public class MenuBar extends MenuComponent implements MenuContainer
{
  /** Create a new MenuBar.  */
  public MenuBar ()
  {
    menus = new Vector ();
  }

  /** Add a menu to this MenuBar.  If the menu has already has a
   * parent, it is first removed from its old parent before being
   * added.
   * @param menu The menu to add.
   * @returns menu
   */
  public synchronized Menu add (Menu menu)
  {
    if (menu.parent != null)
      menu.parent.remove (menu);

    menu.parent = this;
    menus.add (menu);

    if (peer != null)
      {
	MenuBarPeer mp = (MenuBarPeer) peer;
	mp.add (menu);
      }

    return menu;
  }

  /** This creates the component's peer.  */
  public void addNotify ()
  {
    if (peer != null)
      {
	// This choice of toolkit seems unsatisfying, but I'm not sure
	// what else to do.
	peer = Toolkit.getDefaultToolkit ().createMenuBar (this);
      }
  }

  /** @deprecated  Use getMenuCount() instead.  */
  public int countMenus ()
  {
    return getMenuCount ();
  }

  /** Delete a keyboard shortcut.
   * @param shortcut The short cut which should be deleted from all
   *                 menus on this MenuBar.
   */
  public void deleteShortcut (MenuShortcut shortcut)
  {
    MenuItem it;
    // This is a slow implementation, but it probably doesn't matter.
    while ((it = getShortcutMenuItem (shortcut)) != null)
      it.deleteShortcut ();
  }

  /** Returns the current Help menu.  */
  public Menu getHelpMenu ()
  {
    return help_menu;
  }

  /** Returns a menu from this object.
   * @param index Index of menu to return.
   */
  public Menu getMenu (int index)
  {
    return (Menu) menus.get (index);
  }

  /** Returns the number of menus on this MenuBar.  */
  public int getMenuCount ()
  {
    return menus.size ();
  }

  /** Returns the menu item on this MenuBar with the specified
   * shortcut.
   * @param shortcut Shortcut to look for
   */
  public MenuItem getShortcutMenuItem (MenuShortcut shortcut)
  {
    Enumeration m = new MenuEnumeration (this);
    while (m.hasMoreElements ())
      {
	MenuItem item = (MenuItem) m.nextElement ();
	if (item.getShortcut () == shortcut)
	  return item;
      }
    return null;
  }

  /** Remove a menu from the menu bar.  If the menu is specified by
   * component (and not index), and does not exist on the menu, then
   * the method does nothing.  If the removed menu has a peer, it is
   * destroyed.
   * @param menu The menu to remove
   * @param index The index of the menu to remove
   */
  public synchronized void remove (MenuComponent menu)
  {
    int s = menus.size ();
    for (int i = 0; i < s; ++i)
      {
	if (menus.get (i) == menu)
	  {
	    remove (i);
	    break;
	  }
      }
  }

  public synchronized void remove (int index)
  {
    Menu m = (Menu) menus.get (index);
    menus.remove (index);
    m.removeNotify ();
    m.parent = null;

    if (peer != null)
      {
	MenuBarPeer mp = (MenuBarPeer) peer;
	mp.remove (index);
      }
  }

  /** Set the Help menu for this MenuBar.  If a Help menu already
   * exists, it is first removed.
   * @param menu The new Help menu.
   */
  public synchronized void setHelpMenu (Menu menu)
  {
    if (help_menu != null)
      {
	help_menu.removeNotify ();
	help_menu.parent = null;
      }

    if (menu.parent != null)
      menu.parent.remove (menu);
    if (menu.parent != null)
      menu.parent.remove (menu);
    menu.parent = this;

    if (peer != null)
      {
	MenuBarPeer mp = (MenuBarPeer) peer;
	mp.addHelpMenu (menu);
      }
  }

  /** Returns an Enumeration which lists the keyboard shortcuts
   * associated with menu items on this MenuBar.
   */
  public synchronized Enumeration shortcuts ()
  {
    return new ShortcutEnumeration (new MenuEnumeration (this));
  }

  // Iterate over the items of a menu.
  private static class MenuEnumeration implements Enumeration
  {
    // Enumerate over the menu's items.
    Enumeration main;
    // Enumerate over a submenu.
    Enumeration sub;
    // Menubar so we can keep track of help menu too.
    MenuBar menubar;

    MenuEnumeration (Menu m)
    {
      sub = null;
      menubar = null;
      main = m.items.elements ();
    }

    MenuEnumeration (MenuBar mb)
    {
      sub = null;
      menubar = mb;
      main = mb.menus.elements ();
    }

    public boolean hasMoreElements ()
    {
      boolean r = false;
      if (sub != null)
	r = sub.hasMoreElements ();
      if (! r)
	r = main.hasMoreElements ();
      if (! r && menubar != null)
	{
	  if (menubar.help_menu != null)
	    {
	      main = new MenuEnumeration (menubar.help_menu);
	      r = main.hasMoreElements ();
	    }
	  menubar = null;
	}
      return r;
    }

    public Object nextElement () throws NoSuchElementException
    {
      while (true)
	{
	  if (! sub.hasMoreElements ())
	    sub = null;
	  else
	    return sub.nextElement ();

	  if (! main.hasMoreElements () && menubar != null
	      && menubar.help_menu != null)
	    {
	      main = new MenuEnumeration (menubar.help_menu);
	      menubar = null;
	    }

	  Object r = main.nextElement ();
	  if (r instanceof Menu)
	    {
	      sub = new MenuEnumeration ((Menu) r);
	      continue;
	    }

	  return r;
	}
    }
  }

  // This is an enumeration that shadows another enumeration and
  // returns the shortcut for each item returned.  I wonder if we're
  // only supposed to return unique shortcuts?  If so then we could
  // keep a hash table here and remove duplicates.
  private static class ShortcutEnumeration implements Enumeration
  {
    Enumeration back;

    ShortcutEnumeration (Enumeration back)
    {
      this.back = back;
    }

    public boolean hasMoreElements ()
    {
      return back.hasMoreElements ();
    }

    public Object nextElement () throws NoSuchElementException
    {
      while (true)
	{
	  MenuItem item = (MenuItem) back.nextElement ();
	  if (item.getShortcut () != null)
	    return item.getShortcut ();
	}
    }
  }

  // We use Vector because it makes enumerating easier than ArrayList
  // in this case.
  Vector menus;
  Menu help_menu;
}
