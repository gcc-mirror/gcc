/* JTable.java -- 
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Vector;
import java.util.Hashtable;

import javax.accessibility.Accessible;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

public class JTable extends JComponent
  implements TableModelListener, Scrollable, TableColumnModelListener,
             ListSelectionListener, CellEditorListener, Accessible
{
  private static final long serialVersionUID = 3876025080382781659L;
  
  public static final int AUTO_RESIZE_ALL_COLUMNS = 4;
  public static final int AUTO_RESIZE_LAST_COLUMN = 3;
  public static final int AUTO_RESIZE_NEXT_COLUMN = 1;
  public static final int AUTO_RESIZE_OFF = 0;
  public static final int AUTO_RESIZE_SUBSEQUENT_COLUMNS = 2;
  
  protected boolean autoCreateColumnsFromModel;
  protected int autoResizeMode;
  protected TableCellEditor cellEditor;
  protected boolean cellSelectionEnabled;
  protected TableColumnModel columnModel;
  protected TableModel dataModel;
  protected Hashtable defaultEditorsByColumnClass;
  protected Hashtable defaultRenderersByColumnClass;
  protected int editingColumn;
  protected int editingRow;
  protected Color gridColor;
  protected Dimension preferredViewportSize;
  protected int rowHeight;
  protected int rowMargin;
  protected boolean rowSelectionAllowed;
  protected Color selectionBackground;
  protected Color selectionForeground;
  protected ListSelectionModel selectionModel;
  protected boolean showHorizontalLines;
  protected boolean showVerticalLines;
  protected JTableHeader tableHeader;
  
  /**
   * Creates a new <code>JTable</code> instance.
   */
  public JTable ()
  {
    this(null, null, null);
  }

  /**
   * Creates a new <code>JTable</code> instance.
   *
   * @param numRows an <code>int</code> value
   * @param numColumns an <code>int</code> value
   */
  public JTable (int numRows, int numColumns)
  {
    this(new DefaultTableModel(numRows, numColumns));
  }

  /**
   * Creates a new <code>JTable</code> instance.
   *
   * @param data an <code>Object[][]</code> value
   * @param columnNames an <code>Object[]</code> value
   */
  public JTable(Object[][] data, Object[] columnNames)
  {
    this(new DefaultTableModel(data, columnNames));
  }

  /**
   * Creates a new <code>JTable</code> instance.
   *
   * @param dm a <code>TableModel</code> value
   */
  public JTable (TableModel dm)
  {
    this(dm, null, null);
  }

  /**
   * Creates a new <code>JTable</code> instance.
   *
   * @param dm a <code>TableModel</code> value
   * @param cm a <code>TableColumnModel</code> value
   */
  public JTable (TableModel dm, TableColumnModel cm)
  {
    this(dm, cm, null);
  }

  /**
   * Creates a new <code>JTable</code> instance.
   *
   * @param dm a <code>TableModel</code> value
   * @param cm a <code>TableColumnModel</code> value
   * @param sm a <code>ListSelectionModel</code> value
   */
  public JTable (TableModel dm, TableColumnModel cm, ListSelectionModel sm)
  {
    this.dataModel = dm == null ? createDefaultDataModel() : dm;
    this.columnModel = cm == null ? createDefaultColumnModel() : cm;
    this.selectionModel = sm == null ? createDefaultListSelectionModel() : sm;
  }

  /**
   * Creates a new <code>JTable</code> instance.
   *
   * @param data a <code>Vector</code> value
   * @param columnNames a <code>Vector</code> value
   */
  public JTable(Vector data, Vector columnNames)
  {
    this(new DefaultTableModel(data, columnNames));
  }

  /**
   * @deprecated 1.0.2, replaced by <code>new JScrollPane(JTable)</code>
   */
  public static JScrollPane createScrollPaneForTable(JTable table)
  {
    return new JScrollPane(table);
  }

  public void clearSelection()
  {
    selectionModel.clearSelection();
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
 
  protected TableColumnModel createDefaultColumnModel()
  {
    return new DefaultTableColumnModel();
  }

  protected TableModel createDefaultDataModel()
  {
    return new DefaultTableModel();
  }

  protected ListSelectionModel createDefaultListSelectionModel()
  {
    return new DefaultListSelectionModel();
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
    return columnModel;
  }
  
  public TableModel getModel()
  {
    return dataModel;
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
    return selectionModel.getMinSelectionIndex();
  }
  
  public ListSelectionModel getSelectionModel ()
  {
    if (! rowSelectionAllowed)
      return null;

    return selectionModel;
  }

  public void tableChanged (TableModelEvent event)
  {
    throw new Error ("Not implemented");
  }

  public void setModel (TableModel model)
  {
    if (model == null)
      throw new IllegalArgumentException();

    // FIXME: Should we deregister from old model ?
    
    dataModel = model;
    dataModel.addTableModelListener(this);
  }

  public void setSelectionMode (int selectionMode)
  {
    throw new Error ("Not implemented");
  }

  public void setSelectionModel (ListSelectionModel model)
  {
    if (model == null)
      throw new IllegalArgumentException();

    // FIXME: Should we deregister from old model ?
    
    selectionModel = model;
    selectionModel.addListSelectionListener(this);
  }

  public void setShowGrid (boolean showGrid)
  {
    throw new Error ("Not implemented");
  }

  public void valueChanged (ListSelectionEvent event)
  {
    throw new Error ("Not implemented");
  }

  public JTableHeader getTableHeader()
  {
    return tableHeader;
  }

  public void setTableHeader(JTableHeader newHeader)
  {
    tableHeader = newHeader;
  }

  public boolean getColumnSelectionAllowed()
  {
    return columnModel.getColumnSelectionAllowed();
  }
  
  public void setColumnSelectionAllowed(boolean flag)
  {
    columnModel.setColumnSelectionAllowed(flag);
  }

  public boolean getRowSelectionAllowed()
  {
    return rowSelectionAllowed;
  }
  
  public void setRowSelectionAllowed(boolean flag)
  {
    rowSelectionAllowed = flag;
  }

  public int getAutoResizeMode()
  {
    return autoResizeMode;
  }

  public void setAutoResizeMode(int mode)
  {
    autoResizeMode = mode;
  }

  public int getColumnCount()
  {
    return dataModel.getColumnCount();
  }

  public int getRowCount()
  {
    return dataModel.getRowCount();
  }

  public TableCellRenderer getCellRenderer(int row, int column)
  {
    TableCellRenderer renderer =
      columnModel.getColumn(column).getCellRenderer();
    
    if (renderer == null)
      renderer = getDefaultRenderer(dataModel.getColumnClass(column));
    
    return renderer;
  }

  public TableCellRenderer getDefaultRenderer(Class columnClass)
  {
    // FIXME:
    return null;
  }
}
