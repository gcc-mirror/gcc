/* Copyright (C) 1999, 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public class MenuBar extends MenuComponent  implements MenuContainer
{
  Menu[] menus;
  int count;

  public synchronized Menu add (Menu m)
  {
    if (menus == null)
      menus = new Menu[1];
    else if (count == menus.length)
      {
	Menu[] newMenus = new Menu[2 * count];
	System.arraycopy(menus, 0, newMenus, 0, count);
      }
    menus[count++] = m;
    return m;
  }

  public void remove (MenuComponent comp)
  {
    for (int i = count; --i >= 0; )
      {
	if (menus[i] == comp)
	  {
	    System.arraycopy(menus, i, menus, i+1, count-i-1);
	    count--;
	    // FIXME:  destroy peer
	    return;
	  }
      }
  }

  public Font getFont() { return null; } // FIXME
  public boolean postEvent(Event evt) { return false; } // FIXME
}
