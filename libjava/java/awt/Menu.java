/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.util.Vector;

/* Status: Incomplete. */

public class Menu extends MenuItem implements MenuContainer
{
  // Fields from the serialization spec. Decalare others "transient".
  Vector items = new Vector();
  boolean tearOff;
  boolean isHelpMenu;
  int menuSerializedDataVersion;
  
  static final MenuItem separator = new MenuItem("-");

  public Menu()
  {
    this(null, false);
  }
  
  public Menu(String label)
  {
    this(label, false);
  }
  
  public Menu(String label, boolean tearOff)
  {
    super(label);
    this.tearOff = tearOff;
  }

  public void addNotify()
  {
    if (peer != null)
      {
	// This choice of toolkit seems unsatisfying, but I'm not sure
	// what else to do.
	peer = Toolkit.getDefaultToolkit ().createMenu (this);
      }
    super.addNotify ();
  }

  public void removeNotify()
  {
    // FIXME
  }

  public boolean isTearOff()
  {
    return tearOff;
  }

  public int getItemCount()
  {
    return items.size();
  }

  /** @deprecated Use getItemCount() instead. */
  public int countItems()
  {
    return getItemCount();
  }

  public MenuItem getItem(int index)
  {
    return (MenuItem) items.elementAt(index);
  }

  public synchronized MenuItem add(MenuItem mi)
  {
    items.addElement(mi);
    if (mi.parent != null)
      {
	mi.parent.remove(mi);
      }
    mi.parent = this;
    return mi;
  }

  public void add(String label)
  {
    MenuItem mi = new MenuItem(label);
    this.add(mi);
  }

  public synchronized void insert(MenuItem menuitem, int index)
  {
    if (index < 0)
      throw new IllegalArgumentException();
    items.insertElementAt(menuitem, index);
  }

  public void insert(String label, int index)
  {
    MenuItem mi = new MenuItem(label);
    this.insert(mi, index);
  }

  public void addSeparator()
  {
    this.add(separator);
  }

  public void insertSeparator(int index)
  {
    this.insert(separator, index);    
  }

  public synchronized void remove(int index)
  {
    items.removeElementAt(index);
  }

  public synchronized void remove(MenuComponent item)
  {
    items.removeElement(item);
  }

  public synchronized void removeAll()
  {
    items.removeAllElements();
  }

  public String paramString()
  {
    return getName() + ",label" + label + ",tearOff=" + tearOff + 
           ",isHelpMenu=" + isHelpMenu;
  }
  
  // Accessibility API not yet implemented.
  // public AccessibleContext getAccessibleContext()
}
