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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Vector;
import java.util.Hashtable;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.plaf.TableUI;
import javax.swing.table.DefaultTableColumnModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;

public class JTable extends JComponent
  implements TableModelListener, Scrollable, TableColumnModelListener,
             ListSelectionListener, CellEditorListener, Accessible
{
  private static final long serialVersionUID = 3876025080382781659L;


  /**
   * When resizing columns, do not automatically change any columns. In this
   * case the table should be enclosed in a {@link JScrollPane} in order to
   * accomodate cases in which the table size exceeds its visible area.
   */
  public static final int AUTO_RESIZE_OFF = 0;

  /**
   * When resizing column <code>i</code>, automatically change only the
   * single column <code>i+1</code> to provide or absorb excess space
   * requirements.
   */
  public static final int AUTO_RESIZE_NEXT_COLUMN = 1;

  /**
   * When resizing column <code>i</code> in a table of <code>n</code>
   * columns, automatically change all columns in the range <code>[i+1,
   * n)</code>, uniformly, to provide or absorb excess space requirements.
   */
  public static final int AUTO_RESIZE_SUBSEQUENT_COLUMNS = 2;
  
  /**
   * When resizing column <code>i</code> in a table of <code>n</code>
   * columns, automatically change all columns in the range <code>[0,
   * n)</code> (with the exception of column i) uniformly, to provide or
   * absorb excess space requirements.
   */
  public static final int AUTO_RESIZE_ALL_COLUMNS = 4;

  /**
   * When resizing column <code>i</code> in a table of <code>n</code>
   * columns, automatically change column <code>n-1</code> (the last column
   * in the table) to provide or absorb excess space requirements.
   */
  public static final int AUTO_RESIZE_LAST_COLUMN = 3;


  /**
   * A table mapping {@link java.lang.Class} objects to 
   * {@link TableCellEditor} objects. This table is consulted by the 
   * 
   */
  protected Hashtable defaultEditorsByColumnClass;
  protected Hashtable defaultRenderersByColumnClass;
  protected int editingColumn;
  protected int editingRow;

  /**
   * Whether or not the table should automatically compute a matching
   * {@link TableColumnModel} and assign it to the {@link #columnModel}
   * property when the {@link #dataModel} property is changed. 
   *
   * @see #setModel()
   * @see #createColumnsFromModel()
   * @see #setColumnModel()
   * @see #setAutoCreateColumnsFromModel()
   * @see #getAutoCreateColumnsFromModel()
   */
  protected boolean autoCreateColumnsFromModel;

  /**
   * A numeric code specifying the resizing behavior of the table. Must be
   * one of {@link #AUTO_RESIZE_ALL_COLUMNS} (the default), {@link
   * #AUTO_RESIZE_LAST_COLUMN}, {@link #AUTO_RESIZE_NEXT_COLUMN}, {@link
   * #AUTO_RESIZE_SUBSEQUENT_COLUMNS}, or {@link #AUTO_RESIZE_OFF}.
   * 
   * @see #doLayout()
   * @see #setAutoResizeMode()
   * @see #getAutoResizeMode()
   */
  protected int autoResizeMode;

  /**
   * The height in pixels of any row of the table. All rows in a table are
   * of uniform height. This differs from column width, which varies on a
   * per-column basis, and is stored in the individual columns of the
   * {@link #columnModel}.
   * 
   * @see #getRowHeight()
   * @see #setRowHeight()
   * @see TableColumn#getWidth()
   * @see TableColumn#setWidth()
   */
  protected int rowHeight;

  /**
   * The height in pixels of the gap left between any two rows of the table. 
   * 
   * @see #setRowMargin()
   * @see #getRowHeight()
   * @see #getInterCellSpacing()
   * @see #setInterCellSpacing()
   * @see TableColumnModel#getColumnMargin()
   * @see TableColumnModel#setColumnMargin()
   */
  protected int rowMargin;

  /**
   * Whether or not the table should allow row selection. If the table
   * allows both row <em>and</em> column selection, it is said to allow
   * "cell selection". Previous versions of the JDK supported cell
   * selection as an independent concept, but it is now represented solely
   * in terms of simultaneous row and column selection.
   *
   * @see TableColumnModel#columnSelectionAllowed()
   * @see #setRowSelectionAllowed()
   * @see #getRowSelectionAllowed()
   * @see #getCellSelectionEnabled()
   * @see #setCellSelectionEnabled()
   */
  protected boolean rowSelectionAllowed;

  /**
   * @deprecated Use {@link #rowSelectionAllowed}, {@link
   * #columnSelectionAllowed}, or the combined methods {@link
   * getCellSelectionEnabled} and {@link setCellSelectionEnabled}.
   */
  protected boolean cellSelectionEnabled;
  
  /**
   * The model for data stored in the table. Confusingly, the published API
   * requires that this field be called <code>dataModel</code>, despite its
   * property name. The table listens to its model as a {@link
   * TableModelListener}.
   *
   * @see #tableChanged()
   * @see TableModel#addTableModelListener()
   */
  protected TableModel dataModel;

  /**
   * <p>A model of various aspects of the columns of the table, <em>not
   * including</em> the data stored in them. The {@link TableColumnModel}
   * is principally concerned with holding a set of {@link TableColumn}
   * objects, each of which describes the display parameters of a column
   * and the numeric index of the column from the data model which the
   * column is presenting.</p>
   *
   * <p>The TableColumnModel also contains a {@link ListSelectionModel} which
   * indicates which columns are currently selected. This selection model
   * works in combination with the {@link selectionModel} of the table
   * itself to specify a <em>table selection</em>: a combination of row and
   * column selections.</p>
   *
   * <p>Most application programmers do not need to work with this property
   * at all: setting {@link #autoCreateColumnsFromModel} will construct the
   * columnModel automatically, and the table acts as a facade for most of
   * the interesting properties of the columnModel anyways.</p>
   * 
   * @see #setColumnModel()
   * @see #getColumnModel()
   */
  protected TableColumnModel columnModel;

  /**
   * A model of the rows of this table which are currently selected. This
   * model is used in combination with the column selection model held as a
   * member of the {@link columnModel} property, to represent the rows and
   * columns (or both: cells) of the table which are currently selected.
   *
   * @see #rowSelectionAllowed
   * @see #setSelectionModel()
   * @see #getSelectionModel()
   * @see TableColumnModel#getSelectionModel()
   * @see ListSelectionModel#addListSelectionListener()   
   */
  protected ListSelectionModel selectionModel;

  /**
   * The accessibleContext property.
   */
  protected AccessibleContext accessibleContext;

  /**
   * The current cell editor. 
   */
  protected TableCellEditor cellEditor;

  /**
   * Whether or not drag-and-drop is enabled on this table.
   *
   * @see #setDragEnabled()
   * @see #getDragEnabled()
   */
  protected boolean dragEnabled;

  /**
   * The color to paint the grid lines of the table, when either {@link
   * #showHorizontalLines} or {@link #showVerticalLines} is set.
   *
   * @see #setGridColor()
   * @see #getGridColor()
   */
  protected Color gridColor;

  /**
   * The size this table would prefer its viewport assume, if it is
   * contained in a {@link JScrollPane}.
   *
   * @see #setPreferredScrollableViewportSize()
   * @see #getPreferredScrollableViewportSize()
   */
  protected Dimension preferredScrollableViewportSize;

  /**
   * The color to paint the background of selected cells. Fires a property
   * change event with name {@link #SELECTION_BACKGROUND_CHANGED_PROPERTY}
   * when its value changes.
   *
   * @see #setSelectionBackground()
   * @see #getSelectionBackground()
   */
  Color selectionBackground;

  /**
   * The name carried in property change events when the {@link
   * #selectionBackground} property changes.
   */
  private static final String SELECTION_BACKGROUND_CHANGED_PROPERTY = "selectionBackground";

  /**
   * The color to paint the foreground of selected cells. Fires a property
   * change event with name {@link #SELECTION_FOREGROUND_CHANGED_PROPERTY}
   * when its value changes.
   *
   * @see #setSelectionForeground()
   * @see #getSelectionForeground()
   */
  Color selectionForeground;

  /**
   * The name carried in property change events when the
   * {@link #selectionForeground} property changes.
   */
  private static final String SELECTION_FOREGROUND_CHANGED_PROPERTY = "selectionForeground";

  /**
   * The showHorizontalLines property.
   */
  protected boolean showHorizontalLines;

  /**
   * The showVerticalLines property.
   */
  protected boolean showVerticalLines;

  /**
   * The tableHeader property.
   */
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
    setSelectionModel(sm == null ? createDefaultListSelectionModel() : sm);

    this.columnModel = cm;
    this.autoCreateColumnsFromModel = false;
    if (cm == null)
      {
        this.autoCreateColumnsFromModel = true;
        createColumnsFromModel();
      }
    this.columnModel.addColumnModelListener(this);
    
    this.defaultRenderersByColumnClass = new Hashtable();
    this.defaultEditorsByColumnClass = new Hashtable();

    this.autoResizeMode = AUTO_RESIZE_ALL_COLUMNS;
    this.rowHeight = 16;
    this.rowMargin = 1;
    this.rowSelectionAllowed = true;
    // this.accessibleContext = new AccessibleJTable();
    this.cellEditor = null;
    this.dragEnabled = false;
    this.preferredScrollableViewportSize = new Dimension(450,400);
    this.showHorizontalLines = true;
    this.showVerticalLines = true;
    setInterCellSpacing(new Dimension(1,1));
    setTableHeader(new JTableHeader(columnModel));
    updateUI();
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

  private void createColumnsFromModel()
  {
    if (dataModel == null)
      return;

    TableColumnModel cm = createDefaultColumnModel();

    for (int i = 0; i < dataModel.getColumnCount(); ++i)
      {
        cm.addColumn(new TableColumn(i));
      }
    this.setColumnModel(cm);
  }

  // listener support 

  public void columnAdded (TableColumnModelEvent event)
  {
    revalidate();
    repaint();
  }

  public void columnMarginChanged (ChangeEvent event)
  {
    revalidate();
    repaint();
  }

  public void columnMoved (TableColumnModelEvent event)
  {
    revalidate();
    repaint();
  }

  public void columnRemoved (TableColumnModelEvent event)
  {
    revalidate();
    repaint();
  }
  
  public void columnSelectionChanged (ListSelectionEvent event)
  {
    repaint();
  }

  public void editingCanceled (ChangeEvent event)
  {
    repaint();
  }

  public void editingStopped (ChangeEvent event)
  {
    repaint();
  }

  public void tableChanged (TableModelEvent event)
  {
    repaint();
  }

  public void valueChanged (ListSelectionEvent event)
  {
    repaint();
  }


  /** 
   * Calculate the visible rectangle for a particular row and column. The
   * row and column are specified in visual terms; the column may not match
   * the {@link #dataModel} column.
   *
   * @param row the visible row to get the cell rectangle of
   *
   * @param column the visible column to get the cell rectangle of, which may
   * differ from the {@link #dataModel} column
   *
   * @param includeSpacing whether or not to include the cell margins in the
   * resulting cell. If <code>false</code>, the result will only contain the
   * inner area of the target cell, not including its margins.
   *
   * @return a rectangle enclosing the specified cell
   */
  public Rectangle getCellRect(int row,
                               int column,
                               boolean includeSpacing)
  {
    int height = getHeight();
    int width = columnModel.getColumn(column).getWidth();
    int x_gap = columnModel.getColumnMargin();
    int y_gap = rowMargin;

    column = Math.max(0, Math.min(column, getColumnCount() - 1));
    row = Math.max(0, Math.min(row, getRowCount() - 1));

    int x = 0;
    int y = (height + y_gap) * row;

    for (int i = 0; i < column; ++i)
      {        
        x += columnModel.getColumn(i).getWidth();
        x += x_gap;
      }

    if (includeSpacing)
      return new Rectangle(x, y, width, height);
    else
      return new Rectangle(x, y, width - x_gap, height - y_gap);
  }

  public void clearSelection()
  {
    selectionModel.clearSelection();
  }

  /**
   * Get the value of the {@link #selectedRow} property by delegation to
   * the {@link ListSelectionModel#getMinSelectionIndex} method of the
   * {@link #selectionModel} field.
   *
   * @return The current value of the selectedRow property
   */
  public int getSelectedRow ()
  {
    return selectionModel.getMinSelectionIndex();
  }
  
  /**
   * Get the value of the {@link #selectionModel} property.
   *
   * @return The current value of the property
   */
  public ListSelectionModel getSelectionModel()
  {
    if (! rowSelectionAllowed)
      return null;

    return selectionModel;
  }
  
  public int getScrollableBlockIncrement(Rectangle visibleRect, int orientation, int direction)
  {
    if (orientation == SwingConstants.VERTICAL)
      return visibleRect.height * direction;
    else
      return visibleRect.width * direction;
  }

  /**
   * Get the value of the {@link #scrollableTracksViewportHeight} property.
   *
   * @return The constant value <code>false</code>
   */

  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }
  
  /**
   * Get the value of the {@link #scrollableTracksViewportWidth} property.
   *
   * @return <code>true</code> unless the {@link autoResizeMode} prperty is
   * <code>AUTO_RESIZE_OFF</code>
   */

  public boolean getScrollableTracksViewportWidth()
  {
    if (autoResizeMode == AUTO_RESIZE_OFF)
      return false;
    else
      return true;
  }

  public int getScrollableUnitIncrement(Rectangle visibleRect, int orientation, int direction)
  {
    // FIXME: I don't exactly know what sun does here. in both cases they
    // pick values which do *not* simply expose the next cell in a given
    // scroll direction.

    if (orientation == SwingConstants.VERTICAL)
      return rowHeight;
    else
      {
        int sum = 0;
        for (int i = 0; i < getColumnCount(); ++i)
          sum += columnModel.getColumn(0).getWidth();
        return getColumnCount() == 0 ? 10 : sum / getColumnCount();
      }
  }


  public TableCellEditor getCellEditor(int row, int column)
  {
    TableCellEditor editor = columnModel.getColumn(column).getCellEditor();

    if (editor == null)
      editor = getDefaultEditor(dataModel.getColumnClass(column));

    return editor;
  }

  public TableCellEditor getDefaultEditor(Class columnClass)
  {
    if (defaultEditorsByColumnClass.containsKey(columnClass))
      return (TableCellEditor) defaultEditorsByColumnClass.get(columnClass);
    else
      {
        TableCellEditor r = new DefaultCellEditor(new JTextField());
        defaultEditorsByColumnClass.put(columnClass, r);
        return r;
      }
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
    if (defaultRenderersByColumnClass.containsKey(columnClass))
      return (TableCellRenderer) defaultRenderersByColumnClass.get(columnClass);
    else
      {
        TableCellRenderer r = new DefaultTableCellRenderer();
        defaultRenderersByColumnClass.put(columnClass, r);
        return r;
      }
  }

  public int convertColumnIndexToModel(int vc)
  {
    if (vc < 0)
      return vc;
    else if (vc > getColumnCount())
      return -1;
    else
      return columnModel.getColumn(vc).getModelIndex();
  }

  public int convertColumnIndexToView(int mc)
  {
    if (mc < 0)
      return mc;
    int ncols = getColumnCount();
    for (int vc = 0; vc < ncols; ++vc)
      {
        if (columnModel.getColumn(vc).getModelIndex() == mc)
          return vc;
      }
    return -1;
  }

  public Component prepareRenderer(TableCellRenderer renderer,
                                   int row,
                                   int column)
  {
    boolean rsa = getRowSelectionAllowed();
    boolean csa = getColumnSelectionAllowed();
    boolean rs = rsa ? getSelectionModel().isSelectedIndex(row) : false;
    boolean cs = csa ? columnModel.getSelectionModel().isSelectedIndex(column) : false;
    boolean isSelected = ((rsa && csa && rs && cs) 
                          || (rsa && !csa && rs) 
                          || (!rsa && csa && cs));
    
    return renderer.getTableCellRendererComponent(this,
                                                  dataModel.getValueAt(row, 
                                                                       convertColumnIndexToView(column)),
                                                  isSelected,
                                                  false, // hasFocus
                                                  row, column);
  }


  /**
   * Get the value of the {@link #autoCreateColumnsFromModel} property.
   *
   * @return The current value of the property
   */
  public boolean getAutoCreateColumnsFromModel()
  {
    return autoCreateColumnsFromModel;
  }

  /**
   * Get the value of the {@link #autoResizeMode} property.
   *
   * @return The current value of the property
   */
  public int getAutoResizeMode()
  {
    return autoResizeMode;
  }

  /**
   * Get the value of the {@link #rowHeight} property.
   *
   * @return The current value of the property
   */
  public int getRowHeight()
  {
    return rowHeight;
  }

  /**
   * Get the value of the {@link #rowMargin} property.
   *
   * @return The current value of the property
   */
  public int getRowMargin()
  {
    return rowMargin;
  }

  /**
   * Get the value of the {@link #rowSelectionAllowed} property.
   *
   * @return The current value of the property
   */
  public boolean getRowSelectionAllowed()
  {
    return rowSelectionAllowed;
  }

  /**
   * Get the value of the {@link #cellSelectionEnabled} property.
   *
   * @return The current value of the property
   */
  public boolean getCellSelectionEnabled()
  {
    return getColumnSelectionAllowed() && getRowSelectionAllowed();
  }

  /**
   * Get the value of the {@link #dataModel} property.
   *
   * @return The current value of the property
   */
  public TableModel getModel()
  {
    return dataModel;
  }

  /**
   * Get the value of the {@link #columnCount} property by
   * delegation to the @{link #dataModel} field.
   *
   * @return The current value of the columnCount property
   */
  public int getColumnCount()
  {
    return dataModel.getColumnCount();
  }

  /**
   * Get the value of the {@link #rowCount} property by
   * delegation to the @{link #dataModel} field.
   *
   * @return The current value of the rowCount property
   */
  public int getRowCount()
  {
    return dataModel.getRowCount();
  }

  /**
   * Get the value of the {@link #columnModel} property.
   *
   * @return The current value of the property
   */
  public TableColumnModel getColumnModel()
  {
    return columnModel;
  }

  /**
   * Get the value of the {@link #selectedColumn} property by
   * delegation to the @{link #columnModel} field.
   *
   * @return The current value of the selectedColumn property
   */
  public int getSelectedColumn()
  {
    return columnModel.getSelectionModel().getMinSelectionIndex();
  }

  private static int countSelections(ListSelectionModel lsm)
  {
    int lo = lsm.getMinSelectionIndex();
    int hi = lsm.getMaxSelectionIndex();
    int sum = 0;
    if (lo != -1 && hi != -1)
      {
        switch (lsm.getSelectionMode())
          {
          case ListSelectionModel.SINGLE_SELECTION:
            sum = 1;
            break;
            
          case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
            sum = hi - lo;
            break;
            
          case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:        
            for (int i = lo; i < hi; ++i)
              if (lsm.isSelectedIndex(i))        
                ++sum;
            break;
          }
      }
    return sum;
  }

  private static int[] getSelections(ListSelectionModel lsm)
  {
    int sz = countSelections(lsm);
    int [] ret = new int[sz];

    int lo = lsm.getMinSelectionIndex();
    int hi = lsm.getMaxSelectionIndex();
    int j = 0;
    java.util.ArrayList ls = new java.util.ArrayList();
    if (lo != -1 && hi != -1)
      {
        switch (lsm.getSelectionMode())
          {
          case ListSelectionModel.SINGLE_SELECTION:
            ret[0] = lo;
            break;      
      
          case ListSelectionModel.SINGLE_INTERVAL_SELECTION:            
            for (int i = lo; i < hi; ++i)
              ret[j++] = i;
            break;
            
          case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:        
            for (int i = lo; i < hi; ++i)
              if (lsm.isSelectedIndex(i))        
                ret[j++] = i;
            break;
          }
      }
    return ret;
  }

  /**
   * Get the value of the {@link #selectedColumnCount} property by
   * delegation to the @{link #columnModel} field.
   *
   * @return The current value of the selectedColumnCount property
   */  
  public int getSelectedColumnCount()
  {
    return countSelections(columnModel.getSelectionModel());
  }

  /**
   * Get the value of the {@link #selectedColumns} property by
   * delegation to the @{link #columnModel} field.
   *
   * @return The current value of the selectedColumns property
   */
  public int[] getSelectedColumns()
  {
    return getSelections(columnModel.getSelectionModel());
  }

  /**
   * Get the value of the {@link #columnSelectionAllowed} property.
   *
   * @return The current value of the columnSelectionAllowed property
   */
  public boolean getColumnSelectionAllowed()
  {
    return getColumnModel().getColumnSelectionAllowed();
  }

  /**
   * Get the value of the {@link #selectedRowCount} property by
   * delegation to the @{link #selectionModel} field.
   *
   * @return The current value of the selectedRowCount property
   */
  public int getSelectedRowCount()
  {
    return countSelections(selectionModel);
  }

  /**
   * Get the value of the {@link #selectedRows} property by
   * delegation to the @{link #selectionModel} field.
   *
   * @return The current value of the selectedRows property
   */
  public int[] getSelectedRows()
  {
    return getSelections(selectionModel);
  }

  /**
   * Get the value of the {@link #accessibleContext} property.
   *
   * @return The current value of the property
   */
  public AccessibleContext getAccessibleContext()
  {
    return accessibleContext;
  }

  /**
   * Get the value of the {@link #cellEditor} property.
   *
   * @return The current value of the property
   */
  public TableCellEditor getCellEditor()
  {
    return cellEditor;
  }

  /**
   * Get the value of the {@link #dragEnabled} property.
   *
   * @return The current value of the property
   */
  public boolean getDragEnabled()
  {
    return dragEnabled;
  }

  /**
   * Get the value of the {@link #gridColor} property.
   *
   * @return The current value of the property
   */
  public Color getGridColor()
  {
    return gridColor;
  }

  /**
   * Get the value of the {@link #interCellSpacing} property.
   *
   * @return The current value of the property
   */
  public Dimension getInterCellSpacing()
  {
    return new Dimension(columnModel.getColumnMargin(), rowMargin);
  }

  /**
   * Get the value of the {@link #preferredScrollableViewportSize} property.
   *
   * @return The current value of the property
   */
  public Dimension getPreferredScrollableViewportSize()
  {
    return preferredScrollableViewportSize;
  }

  /**
   * Get the value of the {@link #selectionBackground} property.
   *
   * @return The current value of the property
   */
  public Color getSelectionBackground()
  {
    return selectionBackground;
  }

  /**
   * Get the value of the {@link #selectionForeground} property.
   *
   * @return The current value of the property
   */
  public Color getSelectionForeground()
  {
    return selectionForeground;
  }

  /**
   * Get the value of the {@link #showHorizontalLines} property.
   *
   * @return The current value of the property
   */
  public boolean getShowHorizontalLines()
  {
    return showHorizontalLines;
  }

  /**
   * Get the value of the {@link #showVerticalLines} property.
   *
   * @return The current value of the property
   */
  public boolean getShowVerticalLines()
  {
    return showVerticalLines;
  }

  /**
   * Get the value of the {@link #tableHeader} property.
   *
   * @return The current value of the property
   */
  public JTableHeader getTableHeader()
  {
    return tableHeader;
  }

  /**
   * Set the value of the {@link #autoCreateColumnsFromModel} property.
   *
   * @param a The new value of the autoCreateColumnsFromModel property
   */ 
  public void setAutoCreateColumnsFromModel(boolean a)
  {
    autoCreateColumnsFromModel = a;
  }

  /**
   * Set the value of the {@link #autoResizeMode} property.
   *
   * @param a The new value of the autoResizeMode property
   */ 
  public void setAutoResizeMode(int a)
  {
    autoResizeMode = a;
    revalidate();
    repaint();
  }

  /**
   * Set the value of the {@link #rowHeight} property.
   *
   * @param r The new value of the rowHeight property
   */ 
  public void setRowHeight(int r)
  {
    rowHeight = r;
    revalidate();
    repaint();
  }

  /**
   * Set the value of the {@link #rowMargin} property.
   *
   * @param r The new value of the rowMargin property
   */ 
  public void setRowMargin(int r)
  {
    rowMargin = r;
    revalidate();
    repaint();
  }

  /**
   * Set the value of the {@link #rowSelectionAllowed} property.
   *
   * @param r The new value of the rowSelectionAllowed property
   */ 
  public void setRowSelectionAllowed(boolean r)
  {
    rowSelectionAllowed = r;
    repaint();
  }

  /**
   * Set the value of the {@link #cellSelectionEnabled} property.
   *
   * @param c The new value of the cellSelectionEnabled property
   */ 
  public void setCellSelectionEnabled(boolean c)
  {
    setColumnSelectionAllowed(c);
    setRowSelectionAllowed(c);
    // for backward-compatibility sake:
    cellSelectionEnabled = true;
  }

  /**
   * <p>Set the value of the {@link #dataModel} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link TableModelListener} from
   * previous {@link #dataModel} and register it with new parameter
   * <code>m</code>.</p>
   *
   * @param m The new value of the model property
   */ 
  public void setModel(TableModel m)
  {
    if (m == null)
      throw new IllegalArgumentException();
    TableModel tmp = dataModel;
    if (autoCreateColumnsFromModel)
      createColumnsFromModel();
    if (tmp != null)
      tmp.removeTableModelListener(this);
    if (m != null)
      m.addTableModelListener(this);
    dataModel = m;
    revalidate();
    repaint();
  }

  /**
   * <p>Set the value of the {@link #columnModel} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link TableColumnModelListener}
   * from previous {@link #columnModel} and register it with new parameter
   * <code>c</code>.</p>
   *
   * @param c The new value of the columnModel property
   */ 
  public void setColumnModel(TableColumnModel c)
  {
    if (c == null)
      throw new IllegalArgumentException();
    TableColumnModel tmp = columnModel;
    if (tmp != null)
      tmp.removeColumnModelListener(this);
    if (c != null)
      c.addColumnModelListener(this);
    columnModel = c;
    if (dataModel != null && columnModel != null)
      {
        int ncols = getColumnCount();
        for (int i = 0; i < ncols; ++i)
          columnModel.getColumn(i).setHeaderValue(dataModel.getColumnName(i));
      }
    revalidate();
    repaint();
  }

  /**
   * Set the value of the {@link #columnSelectionAllowed} property.
   *
   * @param c The new value of the property
   */ 
  public void setColumnSelectionAllowed(boolean c)
  {
    getColumnModel().setColumnSelectionAllowed(c);
    repaint();
  }

  /**
   * <p>Set the value of the {@link #selectionModel} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link ListSelectionListener}
   * from previous {@link #selectionModel} and register it with new
   * parameter <code>s</code>.</p>
   *
   * @param s The new value of the selectionModel property
   */ 
  public void setSelectionModel(ListSelectionModel s)
  {
    if (s == null)
      throw new IllegalArgumentException();
    ListSelectionModel tmp = selectionModel;
    if (tmp != null)
      tmp.removeListSelectionListener(this);
    if (s != null)
      s.addListSelectionListener(this);
    selectionModel = s;
  }

  /**
   * Set the value of the {@link #selectionMode} property by
   * delegation to the {@link #selectionModel} field.
   *
   * @param s The new value of the property
   */ 
  public void setSelectionMode(int s)
  {
    selectionModel.setSelectionMode(s);
    repaint();
  }

  /**
   * <p>Set the value of the {@link #cellEditor} property.</p>
   *
   * <p>Unregister <code>this</code> as a {@link CellEditorListener} from
   * previous {@link #cellEditor} and register it with new parameter
   * <code>c</code>.</p>
   *
   * @param c The new value of the cellEditor property
   */ 
  public void setCellEditor(TableCellEditor c)
  {
    TableCellEditor tmp = cellEditor;
    if (tmp != null)
      tmp.removeCellEditorListener(this);
    if (c != null)
      c.addCellEditorListener(this);
    cellEditor = c;
  }

  /**
   * Set the value of the {@link #dragEnabled} property.
   *
   * @param d The new value of the dragEnabled property
   */ 
  public void setDragEnabled(boolean d)
  {
    dragEnabled = d;
  }

  /**
   * Set the value of the {@link #gridColor} property.
   *
   * @param g The new value of the gridColor property
   */ 
  public void setGridColor(Color g)
  {
    gridColor = g;
    repaint();
  }

  /**
   * Set the value of the {@link #interCellSpacing} property.
   *
   * @param i The new value of the interCellSpacing property
   */ 
  public void setInterCellSpacing(Dimension i)
  {
    rowMargin = i.height;
    columnModel.setColumnMargin(i.width);
    repaint();
  }

  /**
   * Set the value of the {@link #preferredScrollableViewportSize} property.
   *
   * @param p The new value of the preferredScrollableViewportSize property
   */ 
  public void setPreferredScrollableViewportSize(Dimension p)
  {
    preferredScrollableViewportSize = p;
    revalidate();
    repaint();
  }

  /**
   * <p>Set the value of the {@link #selectionBackground} property.</p>
   *
   * <p>Fire a PropertyChangeEvent with name {@link
   * #SELECTION_BACKGROUND_CHANGED_PROPERTY} to registered listeners, if
   * selectionBackground changed.</p>
   *
   * @param s The new value of the selectionBackground property
   */ 
  public void setSelectionBackground(Color s)
  {
    Color tmp = selectionBackground;
    selectionBackground = s;
    if (((tmp == null && s != null)
         || (s == null && tmp != null)
         || (tmp != null && s != null && !tmp.equals(s))))
      firePropertyChange(SELECTION_BACKGROUND_CHANGED_PROPERTY, tmp, s);
    repaint();
  }

  /**
   * <p>Set the value of the {@link #selectionForeground} property.</p>
   *
   * <p>Fire a PropertyChangeEvent with name {@link
   * SELECTION_FOREGROUND_CHANGED_PROPERTY} to registered listeners, if
   * selectionForeground changed.</p>
   *
   * @param s The new value of the selectionForeground property
   */ 
  public void setSelectionForeground(Color s)
  {
    Color tmp = selectionForeground;
    selectionForeground = s;
    if (((tmp == null && s != null)
         || (s == null && tmp != null)
         || (tmp != null && s != null && !tmp.equals(s))))
      firePropertyChange(SELECTION_FOREGROUND_CHANGED_PROPERTY, tmp, s);
    repaint();
  }

  /**
   * Set the value of the {@link #showGrid} property.
   *
   * @param s The new value of the showGrid property
   */ 
  public void setShowGrid(boolean s)
  {
    setShowVerticalLines(s);
    setShowHorizontalLines(s);
  }

  /**
   * Set the value of the {@link #showHorizontalLines} property.
   *
   * @param s The new value of the showHorizontalLines property
   */ 
  public void setShowHorizontalLines(boolean s)
  {
    showHorizontalLines = s;
    repaint();
  }

  /**
   * Set the value of the {@link #showVerticalLines} property.
   *
   * @param s The new value of the showVerticalLines property
   */ 
  public void setShowVerticalLines(boolean s)
  {
    showVerticalLines = s;
    repaint();
  }

  /**
   * Set the value of the {@link #tableHeader} property.
   *
   * @param t The new value of the tableHeader property
   */ 
  public void setTableHeader(JTableHeader t)
  {
    if (tableHeader != null)
      tableHeader.setTable(null);
    tableHeader = t;
    if (tableHeader != null)
      tableHeader.setTable(this);
    revalidate();
    repaint();
  }

  protected void configureEnclosingScrollPane()
  {
    JScrollPane jsp = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, this);
    if (jsp != null && tableHeader != null)
      {
        jsp.setColumnHeaderView(tableHeader);
      }
  }

  protected void unconfigureEnclosingScrollPane()
  {
    JScrollPane jsp = (JScrollPane) SwingUtilities.getAncestorOfClass(JScrollPane.class, this);
    if (jsp != null)
      {
        jsp.setColumnHeaderView(null);
      }    
  }


  public void addNotify()
  {
    super.addNotify();
    configureEnclosingScrollPane();
  }

  public void removeNotify()
  {
    super.addNotify();
    unconfigureEnclosingScrollPane();
  }


  /**
   * Sun javadocs describe an unusual implementation of
   * <code>doLayout</code> which involves some private interfaces. We try
   * to implement the same algorithm as is documented, but using the
   * columnModel directly. We still use a private helper method, but it has
   * a simpler signature.
   */

  private void distributeSpill(TableColumn[] cols, int spill)
  {
    int MIN = 0;
    int MAX = 0;
    int PREF = 0;

    int[] min = new int[cols.length];
    int[] max = new int[cols.length];
    int[] pref = new int[cols.length];

    for (int i = 0; i < cols.length; ++i)
      {
        pref[i] = cols[i].getPreferredWidth();
        min[i] = cols[i].getMinWidth();
        max[i] = cols[i].getMaxWidth();
        PREF += pref[i];
        MIN += min[i];
        MAX += max[i];
      }

    for (int i = 0; i < cols.length; ++i)
      {
        int adj = 0;
        if (spill > 0)          
          adj = (spill * (pref[i] - min[i])) / (PREF - MIN);
        else
          adj = (spill * (max[i] - pref[i])) / (MAX - PREF);
        cols[i].setWidth(pref[i] + adj);        
      }    
  }
  
  public void doLayout()
  {
    TableColumn resizingColumn = null;

    int ncols = getColumnCount();
    if (ncols < 1)
      return;

    int[] pref = new int[ncols];
    int prefSum = 0;
    int rCol = -1;

    if (tableHeader != null)
      resizingColumn = tableHeader.getResizingColumn();

    for (int i = 0; i < ncols; ++i)
      {
        TableColumn col = columnModel.getColumn(i);
        int p = col.getWidth();
        pref[i] = p;
        prefSum += p;
        if (resizingColumn == col)
          rCol = i;
      }

    int spill = prefSum - getWidth();

    if (resizingColumn != null)
      {
        TableColumn col;
        TableColumn [] cols;

        switch (getAutoResizeMode())
          {
          case AUTO_RESIZE_LAST_COLUMN:
            col = columnModel.getColumn(ncols-1);
            col.setWidth(col.getPreferredWidth() + spill);
            break;
            
          case AUTO_RESIZE_NEXT_COLUMN:
            col = columnModel.getColumn(ncols-1);
            col.setWidth(col.getPreferredWidth() + spill);
            break;

          case AUTO_RESIZE_ALL_COLUMNS:
            cols = new TableColumn[ncols];
            for (int i = 0; i < ncols; ++i)
              cols[i] = columnModel.getColumn(i);
            distributeSpill(cols, spill);
            break;

          case AUTO_RESIZE_SUBSEQUENT_COLUMNS:
            cols = new TableColumn[ncols];
            for (int i = rCol; i < ncols; ++i)
              cols[i] = columnModel.getColumn(i);
            distributeSpill(cols, spill);
            break;

          case AUTO_RESIZE_OFF:
          default:
          }
      }
    else
      {
        TableColumn [] cols = new TableColumn[ncols];
        for (int i = 0; i < ncols; ++i)
          cols[i] = columnModel.getColumn(i);
        distributeSpill(cols, spill);        
      }
  }
  
  public void sizeColumnsToFit(boolean lastColumnOnly)
  {
    doLayout();
  }

  public void sizeColumnsToFit(int resizingColumn)
  {
    doLayout();
  }


  public String getUIClassID()
  {
    return "TableUI";
  }

  public TableUI getUI()
  {
    return (TableUI) ui;
  }

  public void updateUI()
  {
    setUI((TableUI) UIManager.getUI(this));
    revalidate();
    repaint();
  }

}
