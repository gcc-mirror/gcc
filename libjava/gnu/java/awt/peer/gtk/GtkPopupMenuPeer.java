/* GtkPopupMenuPeer.java -- Implements PopupMenuPeer with GTK+
   Copyright (C) 1999 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Component;
import java.awt.Event;
import java.awt.MenuItem;
import java.awt.Point;
import java.awt.PopupMenu;
import java.awt.peer.PopupMenuPeer;

public class GtkPopupMenuPeer extends GtkMenuPeer
  implements PopupMenuPeer
{
  public GtkPopupMenuPeer (PopupMenu menu)
  {
    super (menu);
  }

  native void setupAccelGroup (GtkGenericPeer container);

  void setParent (MenuItem item)
  {
    // we don't need to "add" ourselves to our parent
  }

  native void show (int x, int y, long time);
  public void show (Component origin, int x, int y)
  {
    Point abs = origin.getLocationOnScreen ();
    show (abs.x + x, abs.y + y, 0);
  }
  
  public void show (Event e)
  {

  }
}
