/* GtkListPeer.java -- Implements ListPeer with GTK
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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
import java.awt.*;
import java.awt.peer.*;

public class GtkListPeer extends GtkComponentPeer
  implements ListPeer
{
//    native void create (ComponentPeer parent, String [] items, boolean mode);

  native void create ();
  native void connectHooks ();

  native void getSize (int rows, int dims[]);

  public GtkListPeer (List list)
  {
    super (list);
    
    setMultipleMode (list.isMultipleMode ());

    if (list.getItemCount () > 0)
      append (list.getItems ());
  }

  native void append (String items[]);

  public native void add (String item, int index);
  
  public void addItem (String item, int index)
  {
    add (item, index);
  }
  
  public void clear ()
  {
    removeAll ();
  }
  
  public native void delItems (int start, int end);
  public native void deselect (int index);
  
  public Dimension getMinimumSize (int rows)
  {
    int dims[] = new int[2];

    getSize (rows, dims);
    return (new Dimension (dims[0], dims[1]));
  }

  public Dimension getPreferredSize (int rows)
  {
    int dims[] = new int[2];

    getSize (rows, dims);
    return (new Dimension (dims[0], dims[1]));
  }
  
  public native int[] getSelectedIndexes ();
  public native void makeVisible (int index);

  public Dimension minimumSize (int rows)
  {
    return (getMinimumSize (rows));
  }

  public Dimension preferredSize (int rows)
  {
    return (getPreferredSize (rows));
  }

  public void removeAll ()
  {
    delItems (0, -1);
  }

  public native void select (int index);
  public native void setMultipleMode (boolean b);

  public void setMultipleSelections (boolean b)
  {
    setMultipleMode (b);
  }

  protected void postItemEvent (int item, int stateChange)
  {
    postItemEvent (new Integer (item), stateChange);
  }
}
