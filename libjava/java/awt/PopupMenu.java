/* Copyright (C) 2000  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* Status: Incomplete. */

public class PopupMenu extends Menu
{
  public PopupMenu()
  {
    super();
  }

  public PopupMenu(String label)
  {
    super(label);
  }

  public void addNotify()
  {
    // FIXME
  }

  public void show(Component origin, int x, int y)
  {
    // FIXME
  }
  
  // Accessibility API not yet implemented.
  // public AccessibleContext getAccessibleContext()
}
