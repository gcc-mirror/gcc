/* GtkListPeer.java -- Implements ListPeer with GTK
   Copyright (C) 1998, 1999, 2006 Free Software Foundation, Inc.

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

import java.awt.AWTEvent;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.List;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.peer.ListPeer;

public class GtkListPeer extends GtkComponentPeer
  implements ListPeer
{
  void create ()
  {
    List list = (List) awtComponent;

    create (list.getRows ());

    setMultipleMode (list.isMultipleMode ());
  }

  native void create (int rows);
  native void connectSignals ();

  /**
   * Overridden to set the Font of the text insode the gtk_scrolled_window.
   */
  protected native void gtkWidgetModifyFont (String name, int style, int size);

  native void gtkWidgetRequestFocus ();

  native void getSize (int rows, int visibleRows, int dims[]);

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
    return minimumSize (rows);
  }

  public Dimension getPreferredSize (int rows)
  {
    return preferredSize (rows);
  }
  
  public native int[] getSelectedIndexes ();
  public native void makeVisible (int index);

  public Dimension minimumSize (int rows)
  {
    int dims[] = new int[2];

    int visibleRows = ((List) awtComponent).getRows();
    getSize (rows, visibleRows, dims);
    return new Dimension (dims[0], dims[1]);
  }

  public Dimension preferredSize (int rows)
  {
    // getSize returns the minimum size of the list.
    // The width is too narrow for a typical list.
    List l = (List) awtComponent;
    Dimension d = getMinimumSize();
    FontMetrics fm = l.getFontMetrics(l.getFont());
    return new Dimension(d.width + fm.stringWidth("1234567890"), d.height);
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

  public void handleEvent (AWTEvent e)
  {
    if (e.getID () == MouseEvent.MOUSE_CLICKED && isEnabled ())
      {
        // Only generate the ActionEvent on the second click of a
        // multiple click.
        MouseEvent me = (MouseEvent) e;
        if (!me.isConsumed ()
            && (me.getModifiersEx () & MouseEvent.BUTTON1_DOWN_MASK) != 0
            && me.getClickCount() == 2)
          {
            String selectedItem = ((List) awtComponent).getSelectedItem ();

            // Double-click only generates an Action event if
            // something is selected.
            if (selectedItem != null)
              postActionEvent (((List) awtComponent).getSelectedItem (), 
                               me.getModifiersEx ());
          }
      }

    if (e.getID () == KeyEvent.KEY_PRESSED)
      {
        KeyEvent ke = (KeyEvent) e;
        if (!ke.isConsumed () && ke.getKeyCode () == KeyEvent.VK_ENTER)
          {
            String selectedItem = ((List) awtComponent).getSelectedItem ();

            // Enter only generates an Action event if something is
            // selected.
            if (selectedItem != null)
              postActionEvent (selectedItem, ke.getModifiersEx ());
          }
      }

    super.handleEvent (e);
  }

  protected void postItemEvent (int item, int stateChange)
  {
    postItemEvent (new Integer (item), stateChange);
  }
}
