/* GtkGenericPeer.java - Has a hashcode.  Yuck.
   Copyright (C) 1998, 1999, 2002 Free Software Foundation, Inc.

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

import java.awt.EventQueue;
import java.awt.event.ActionEvent;

public class GtkGenericPeer
{
  final int native_state = getUniqueInteger ();

  // Next native state value we will assign.
  private static int next_native_state = 0;

  // The widget or other java-side object we wrap.
  protected Object awtWidget;

  // Global event queue.
  protected static EventQueue q = null;

  // Dispose of our native state.
  public native void dispose ();

  protected GtkGenericPeer (Object awtWidget)
  {
    this.awtWidget = awtWidget;
  }

  public static void enableQueue (EventQueue sq) 
  {
    if (q == null)
      q = sq;
  }

  protected void postActionEvent (String command, int mods) 
  {
    q.postEvent (new ActionEvent (awtWidget, ActionEvent.ACTION_PERFORMED, 
				  command, mods));
  }

  // Return a unique integer for use in the native state mapping
  // code.  We can't use a hash code since that is not guaranteed to
  // be unique.
  static synchronized int getUniqueInteger ()
  {
    // Let's assume this will never wrap.
    return next_native_state++;
  }
}
