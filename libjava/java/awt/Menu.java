/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

/* A very incomplete placeholder. */

public class Menu extends MenuItem implements MenuContainer
{
  public Menu (String label)
  {
    super(label);  // ???
    throw new Error ("java.awt.Menu: not implemented");
  }

  public void add (String label)
  { /* FIXME */ }

  public synchronized MenuItem add (MenuItem item)
  {
    /* FIXME */
    return item;
  }

  public Font getFont() { return null; } // FIXME
  //public boolean postEvent(Event evt);
  public void remove(MenuComponent comp) { } // FIXME
}
