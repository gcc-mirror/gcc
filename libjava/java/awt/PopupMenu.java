/* Copyright (C) 2000, 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.awt.peer.PopupMenuPeer;

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
    if (peer != null)
      {
	// This choice of toolkit seems unsatisfying, but I'm not sure
	// what else to do.
	peer = Toolkit.getDefaultToolkit ().createPopupMenu (this);
      }
    super.addNotify ();
  }

  public void show(Component origin, int x, int y)
  {
    if (! origin.isShowing ()
	// FIXME: or ! parent is showing -- but how?
	)
      {
	// This is an invalid call which we choose to ignore.
	return;
      }
	
    addNotify ();		// FIXME?
    Event e = new Event (origin, 0, 0, x, y, 0, 0);
    PopupMenuPeer p = (PopupMenuPeer) peer;
    p.show (e);
  }

  // Accessibility API not yet implemented.
  // public AccessibleContext getAccessibleContext()
}
