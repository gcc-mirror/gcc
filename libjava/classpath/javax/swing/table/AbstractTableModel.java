/* AbstractTableModel.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing.table;

import java.io.Serializable;
import java.util.EventListener;

import javax.swing.event.EventListenerList;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

/**
 * A base class that can be used to create implementations of the 
 * {@link TableModel} interface.
 * 
 * @author Andrew Selkirk
 */
public abstract class AbstractTableModel implements TableModel, Serializable
{
  static final long serialVersionUID = -5798593159423650347L;

  /**
   * Storage for the listeners registered with this model.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * Creates a default instance.
   */
  public AbstractTableModel()
  {
    // no setup required here
  }

  /**
   * Returns the name of the specified column.  This method generates default 
   * names in a sequence (starting with column 0):  A, B, C, ..., Z, AA, AB, 
   * AC, ..., AZ, BA, BB, BC, and so on.  Subclasses may override this method
   * to allow column names to be specified on some other basis. 
   *
   * @param columnIndex  the column index.
   *
   * @return The name of the column.
   */
  public String getColumnName(int columnIndex)
  {
    StringBuffer buffer = new StringBuffer();
    while (columnIndex >= 0)
      {
        buffer.insert (0, (char) ('A' + columnIndex % 26));
        columnIndex = columnIndex / 26 - 1;
      }
    return buffer.toString();
  }

  /**
   * Return the index of the specified column, or <code>-1</code> if there is
   * no column with the specified name.
   *
   * @param columnName  the name of the column (<code>null</code> not permitted).
   *
   * @return The index of the column, -1 if not found.
   * 
   * @see #getColumnName(int)
   * @throws NullPointerException if <code>columnName</code> is 
   *         <code>null</code>.
   */
  public int findColumn(String columnName)
  {
    int count = getColumnCount();
    
    for (int index = 0; index < count; index++)
      {
        String name = getColumnName(index);
        
        if (columnName.equals(name))
          return index;
    }

    // Unable to locate.
    return -1;
  }

  /**
   * Returns the <code>Class</code> for all <code>Object</code> instances
   * in the specified column.  
   * 
   * @param columnIndex the column index.
   * 
   * @return The class.
   */
  public Class getColumnClass(int columnIndex)
  {
    return Object.class;
  }

  /**
   * Returns <code>true</code> if the specified cell is editable, and 
   * <code>false</code> if it is not.  This implementation returns 
   * <code>false</code> for all arguments, subclasses should override the 
   * method if necessary.
   *
   * @param rowIndex  the row index of the cell.
   * @param columnIndex  the column index of the cell.
   *
   * @return <code>false</code>.
   */
  public boolean isCellEditable(int rowIndex, int columnIndex)
  {
    return false;
  }

  /**
   * Sets the value of the given cell.  This implementation ignores all 
   * arguments and does nothing, subclasses should override the 
   * method if necessary.
   *
   * @param value  the new value (<code>null</code> permitted).
   * @param rowIndex  the row index of the cell.
   * @param columnIndex  the column index of the cell.
   */
  public void setValueAt(Object value, int rowIndex, int columnIndex)
  {
    // Do nothing...
  }

  /**
   * Adds a listener to the table model.  The listener will receive notification
   * of all changes to the table model.
   *
   * @param listener  the listener.
   */
  public void addTableModelListener(TableModelListener listener)
  {
    listenerList.add(TableModelListener.class, listener);
  }

  /**
   * Removes a listener from the table model so that it will no longer receive
   * notification of changes to the table model.
   *
   * @param listener  the listener to remove.
   */
  public void removeTableModelListener(TableModelListener listener)
  {
    listenerList.remove(TableModelListener.class, listener);
  }

  /**
   * Returns an array containing the listeners that have been added to the
   * table model.
   *
   * @return Array of {@link TableModelListener} objects.
   *
   * @since 1.4
   */
  public TableModelListener[] getTableModelListeners()
  {
    return (TableModelListener[])
      listenerList.getListeners(TableModelListener.class);
  }

  /**
   * Sends a {@link TableModelEvent} to all registered listeners to inform
   * them that the table data has changed.
   */
  public void fireTableDataChanged()
  {
    fireTableChanged(new TableModelEvent(this, 0, Integer.MAX_VALUE));
  }

  /**
   * Sends a {@link TableModelEvent} to all registered listeners to inform
   * them that the table structure has changed.
   */
  public void fireTableStructureChanged()
  {
    fireTableChanged(new TableModelEvent(this, TableModelEvent.HEADER_ROW));
  }

  /**
   * Sends a {@link TableModelEvent} to all registered listeners to inform
   * them that some rows have been inserted into the model.
   * 
   * @param firstRow  the index of the first row.
   * @param lastRow  the index of the last row.
   */
  public void fireTableRowsInserted (int firstRow, int lastRow)
  {
    fireTableChanged(new TableModelEvent(this, firstRow, lastRow,
                                         TableModelEvent.ALL_COLUMNS,
                                         TableModelEvent.INSERT));
  }

  /**
   * Sends a {@link TableModelEvent} to all registered listeners to inform
   * them that some rows have been updated.
   * 
   * @param firstRow  the index of the first row.
   * @param lastRow  the index of the last row.
   */
  public void fireTableRowsUpdated (int firstRow, int lastRow)
  {
    fireTableChanged(new TableModelEvent(this, firstRow, lastRow,
                                         TableModelEvent.ALL_COLUMNS,
                                         TableModelEvent.UPDATE));
  }

  /**
   * Sends a {@link TableModelEvent} to all registered listeners to inform
   * them that some rows have been deleted from the model.
   * 
   * @param firstRow  the index of the first row.
   * @param lastRow  the index of the last row.
   */
  public void fireTableRowsDeleted(int firstRow, int lastRow)
  {
    fireTableChanged(new TableModelEvent(this, firstRow, lastRow,
                                         TableModelEvent.ALL_COLUMNS,
                                         TableModelEvent.DELETE));
  }

  /**
   * Sends a {@link TableModelEvent} to all registered listeners to inform
   * them that a single cell has been updated.
   * 
   * @param row  the row index.
   * @param column  the column index.
   */
  public void fireTableCellUpdated (int row, int column)
  {
    fireTableChanged(new TableModelEvent(this, row, row, column));
  }

  /**
   * Sends the specified event to all registered listeners.
   * 
   * @param event  the event to send.
   */
  public void fireTableChanged(TableModelEvent event)
  {
    int	index;
    TableModelListener listener;
    Object[] list = listenerList.getListenerList();
 
    for (index = 0; index < list.length; index += 2)
      {
        listener = (TableModelListener) list [index + 1];
        listener.tableChanged (event);
      }
  }

  /**
   * Returns an array of listeners of the given type that are registered with
   * this model.
   * 
   * @param listenerType  the listener class.
   * 
   * @return An array of listeners (possibly empty).
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }
}
