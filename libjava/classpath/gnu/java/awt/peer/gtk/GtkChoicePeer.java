/* GtkChoicePeer.java -- Implements ChoicePeer with GTK
   Copyright (C) 1998, 1999, 2005  Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Choice;
import java.awt.AWTEvent;
import java.awt.event.ItemEvent;
import java.awt.peer.ChoicePeer;

public class GtkChoicePeer extends GtkComponentPeer
  implements ChoicePeer
{
  private int selected;
  
  public GtkChoicePeer (Choice c)
  {
    super (c);

    int count = c.getItemCount ();
    if (count > 0)
      {
        for (int i = 0; i < count; i++)
          add(c.getItem(i), i);

        selected = c.getSelectedIndex();
        if (selected >= 0)
          select( selected );
      }
    else
      selected = -1;
  }

  native void create ();

  native int nativeGetSelected ();

  native void connectSignals ();

  native void selectNative (int position);

  native void selectNativeUnlocked (int position);

  public native void add (String item, int index);

  native void nativeRemove(int index);

  native void nativeRemoveAll();

  public void select (int position)
  {
    if (Thread.currentThread() == GtkMainThread.mainThread)
      selectNativeUnlocked (position);
    else
      selectNative (position);
  }

  public void remove( int index )
  {
    // Ensure the triggering of an event when removing item zero if zero is the
    // selected item, even though the selected index doesn't change.
    if( index == 0 && selected == 0 )
      selected = -1; 
    nativeRemove( index );
  }

  public void removeAll()
  {
    selected = -1; // we do not want to trigger a select event here.
    nativeRemoveAll();
  }
  
  public void addItem (String item, int position)
  {
    add (item, position);
  }

  /**
   * Callback from the native side on an item-select event, 
   * which posts an event. The event is only posted if it represents an actual
   * change. Selected is set to the peer's state initially, so that the
   * first call to select(int) from the constructor will not trigger an event.
   * (it should not)
   */
  protected void postChoiceItemEvent ( int index )
  {
    if( selected != index )
      {
        selected = index;
        postItemEvent (((Choice) awtComponent).getItem( selected ), 
                       ItemEvent.SELECTED);
      }
  }

  /**
   * Catches the event and calls Choice.select() if the component state
   * needs updating.
   */
  public void handleEvent (AWTEvent event)
  {
    super.handleEvent (event);
    if (event instanceof ItemEvent)
      if (((ItemEvent)event).getItemSelectable() == awtComponent
           && ((ItemEvent)event).getStateChange() == ItemEvent.SELECTED)
        ((Choice)awtComponent).select( selected );
  }
}

