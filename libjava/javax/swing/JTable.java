/* JTable.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Hashtable;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.swing.event.ChangeEvent;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumnModel;

public class JTable extends JComponent
  implements TableModelListener, Scrollable, TableColumnModelListener,
             ListSelectionListener, CellEditorListener, Accessible
{
  public static final int AUTO_RESIZE_ALL_COLUMNS = 4;
  public static final int AUTO_RESIZE_LAST_COLUMN = 3;
  public static final int AUTO_RESIZE_NEXT_COLUMN = 1;
  public static final int AUTO_RESIZE_OFF = 0;
  public static final int AUTO_RESIZE_SUBSEQUENT_COLUMNS = 2;
  
  public JTable ()
  {
    throw new Error ("Not implemented");
  }

  public JTable (int numRows, int numColumns)
  {
    throw new Error ("Not implemented");
  }

  public JTable (Object[][] rowData, Object[] columnNames)
  {
    throw new Error ("Not implemented");
  }

  public JTable (TableModel dm)
  {
    throw new Error ("Not implemented");
  }

  public JTable (TableModel dm, TableColumnModel cm)
  {
    throw new Error ("Not implemented");
  }

  public JTable (TableModel dm, TableColumnModel cm, ListSelectionModel sm)
  {
    throw new Error ("Not implemented");
  }

  public JTable (Vector rowData, Vector columnNames)
  {
    throw new Error ("Not implemented");
  }

  public void columnAdded (TableColumnModelEvent event)
  {
    throw new Error ("Not implemented");
  }

  public void columnMarginChanged (ChangeEvent event)
  {
    throw new Error ("Not implemented");
  }
  
  public void columnMoved (TableColumnModelEvent event)
  {
    throw new Error ("Not implemented");
  }
  
  public void columnRemoved (TableColumnModelEvent event)
  {
    throw new Error ("Not implemented");
  }
  
  public void columnSelectionChanged (ListSelectionEvent event)
  {
    throw new Error ("Not implemented");
  }
 
  public void editingCanceled (ChangeEvent event)
  {
    throw new Error ("Not implemented");
  }

  public void editingStopped (ChangeEvent event)
  {
    throw new Error ("Not implemented");
  }

  public TableColumnModel getColumnModel ()
  {
    throw new Error ("Not implemented");
  }
  
  public Dimension getPreferredScrollableViewportSize ()
  {
    throw new Error ("Not implemented");
  }

  public int getScrollableBlockIncrement (Rectangle visibleRect, int orientation, int direction)
  {
    throw new Error ("Not implemented");
  }

  public boolean getScrollableTracksViewportHeight ()
  {
    throw new Error ("Not implemented");
  }
  
  public boolean getScrollableTracksViewportWidth ()
  {
    throw new Error ("Not implemented");
  }

  public int getScrollableUnitIncrement (Rectangle visibleRect, int orientation, int direction)
  {
    throw new Error ("Not implemented");
  }

  public int getSelectedRow ()
  {
    throw new Error ("Not implemented");
  }
  
  public ListSelectionModel getSelectionModel ()
  {
    throw new Error ("Not implemented");
  }

  public void tableChanged (TableModelEvent event)
  {
    throw new Error ("Not implemented");
  }

  public void setModel (TableModel model)
  {
    throw new Error ("Not implemented");
  }

  public void setSelectionMode (int selectionMode)
  {
    throw new Error ("Not implemented");
  }

  public void setSelectionModel (ListSelectionModel model)
  {
    throw new Error ("Not implemented");
  }

  public void setShowGrid (boolean showGrid)
  {
    throw new Error ("Not implemented");
  }

  public void valueChanged (ListSelectionEvent event)
  {
    throw new Error ("Not implemented");
  }
} // class JTable
