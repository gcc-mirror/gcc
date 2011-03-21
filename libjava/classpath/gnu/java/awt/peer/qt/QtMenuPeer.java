/* QtMenuPeer.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Menu;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.event.ActionEvent;
import java.awt.peer.MenuPeer;
import java.util.Vector;

public class QtMenuPeer extends QtMenuComponentPeer implements MenuPeer
{
  Vector items;
  boolean itemsAdded;

  public QtMenuPeer( QtToolkit kit, Menu owner )
  {
    super( kit, owner );
    itemsAdded = false;
  }

  protected native void init();

  protected void setup()
  {
    items = new Vector();
    setLabel( ((Menu)owner).getLabel() );
    if( ((Menu)owner).isTearOff() )
      allowTearOff();
  }

  // Recurse the menu tree adding items,
  // called from the MenuBar addMenus() method, called from the Frame peer.
  void addItems()
  {
    if(!itemsAdded)
      {
        Menu o = (Menu)owner;
        for( int i=0; i < o.getItemCount(); i++ )
          {
            MenuItem ci = o.getItem(i);
            if (ci instanceof Menu && ci.getPeer() != null)
              ((QtMenuPeer)ci.getPeer()).addItems();
            addItem( ci );
          }
        itemsAdded = true;
      }
  }

  private void fireClick()
  {
    ActionEvent e = new ActionEvent(owner,
                                    ActionEvent.ACTION_PERFORMED,
                                    ((Menu)owner).getActionCommand());
    QtToolkit.eventQueue.postEvent(e);
  }

  private native void allowTearOff();

  private native void insertSeperator();

  private native void insertItem(QtMenuItemPeer p);

  private native void insertMenu(QtMenuPeer menu);

  private native void delItem(long ptr);

  private void add(long ptr)
  {
    items.add(new Long(ptr));
  }

  // ************ Public methods *********************

  public void addItem( MenuItem item )
  {
    if( item instanceof Menu || item instanceof PopupMenu)
      insertMenu((QtMenuPeer)item.getPeer());
    else
      {
        QtMenuItemPeer p = (QtMenuItemPeer)item.getPeer();
        insertItem(p);
      }
  }

  public void addSeparator()
  {
    insertSeperator();
  }

  public void delItem( int index )
  {
    long ptr = ((Long)items.elementAt(index)).longValue();
    delItem(ptr);
    items.removeElementAt(index);
  }

  // Inherited methods..

  public void disable()
  {
    setEnabled(false);
  }

  public void enable()
  {
    setEnabled(true);
  }

  public native void setEnabled(boolean enabled);

  public native void setLabel(String text);
}
