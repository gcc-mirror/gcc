/* AbstractTableModel.java --
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

package javax.swing.table;

import java.io.Serializable;
import java.util.EventListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;

/**
 * AbstractTableModel
 * 
 * @author Andrew Selkirk
 */
public abstract class AbstractTableModel implements TableModel, Serializable
{
  static final long serialVersionUID = -5798593159423650347L;

  /**
   * listenerList
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * Constructor AbstractTableModel
   */
  public AbstractTableModel()
  {
    // TODO
  }

  /**
   * Get the name of the column for this index. If you do not override
   * this methode, you'll get something like: 0, A; 1, B; ...; AA; AB;
   * ...
   *
   * @param columnIndex The index of the column.
   *
   * @return The name of the column.
   */
  public String getColumnName (int columnIndex)
  {
    int index = columnIndex + 1;
    StringBuffer buffer = new StringBuffer();

    while (index > 0)
      {
	buffer.insert (0, (char) ('A' + ((index - 1) % 26)));
	index = (index - 1) / 26;
      }
    
    // Return column name.
    return buffer.toString();
  }

  /**
   * Return the index of the given name.
   *
   * @param columnName The name of the column.
   *
   * @return The index of the column, -1 if not found.
   */
  public int findColumn (String columnName)
  {
    int count = getColumnCount();
    
    for (int index = 0; index < count; index++)
      {
        String name = getColumnName (index);
        
        if (name.equals (columnName))
          return index;
    }

    // Unable to locate.
    return -1;
  }

  /**
   * Returns the class of a comlumn.
   *
   * @param columnIndex The index of the column.
   *
   * @return The class type of the column.
   */
  public Class getColumnClass (int columnIndex)
  {
    return Object.class;
  }

  /**
   * Tells whether a cell is editable.
   *
   * @param rowIndex The row of the cell.
   * @param columnIndex The index of the cell.
   *
   * @return True if cell is editable.
   */
  public boolean isCellEditable (int rowIndex, int columnIndex)
  {
    return false;
  }

  /**
   * Sets a cell to a value.
   *
   * @param value New value of cell.
   * @param rowIndex The row of the cell.
   * @param columnIndex The column of the cell.
   */
  public void setValueAt (Object value, int rowIndex, int columnIndex)
  {
    // Do nothing...
  }

  /**
   * Add a TableModelListener.
   *
   * @param listener The listener to add.
   */
  public void addTableModelListener (TableModelListener listener)
  {
    listenerList.add (TableModelListener.class, listener);
  }

  /**
   * Removes a TableModelListener.
   *
   * @param listener The listener to remove.
   */
  public void removeTableModelListener (TableModelListener listener)
  {
    listenerList.remove (TableModelListener.class, listener);
  }

  /**
   * Return all registered TableModelListener objects.
   *
   * @return Array of TableModelListener objects.
   *
   * @since 1.4
   */
  public TableModelListener[] getTableModelListeners()
  {
    return (TableModelListener[])
      listenerList.getListeners (TableModelListener.class);
  }

  /**
   * fireTableDataChanged
   */
  public void fireTableDataChanged()
  {
    fireTableChanged (new TableModelEvent (this));
  }

  /**
   * fireTableStructureChanged
   */
  public void fireTableStructureChanged()
  {
    fireTableChanged (new TableModelEvent (this, TableModelEvent.HEADER_ROW));
  }

  /**
   * fireTableRowsInserted
   * @param value0 TODO
   * @param value1 TODO
   */
  public void fireTableRowsInserted (int firstRow, int lastRow)
  {
    fireTableChanged (new TableModelEvent (this, firstRow, lastRow,
                                           TableModelEvent.ALL_COLUMNS,
                                           TableModelEvent.INSERT));
  }

  /**
   * fireTableRowsUpdated
   * @param value0 TODO
   * @param value1 TODO
   */
  public void fireTableRowsUpdated (int firstRow, int lastRow)
  {
    fireTableChanged (new TableModelEvent (this, firstRow, lastRow,
                                           TableModelEvent.ALL_COLUMNS,
                                           TableModelEvent.UPDATE));
  }

  /**
   * fireTableRowsDeleted
   * @param value0 TODO
   * @param value1 TODO
   */
  public void fireTableRowsDeleted(int firstRow, int lastRow)
  {
    fireTableChanged (new TableModelEvent (this, firstRow, lastRow,
                                           TableModelEvent.ALL_COLUMNS,
                                           TableModelEvent.DELETE));
  }

  /**
   * fireTableCellUpdated
   * @param value0 TODO
   * @param value1 TODO
   */
  public void fireTableCellUpdated (int row, int column)
  {
    fireTableChanged (new TableModelEvent (this, row, row, column));
  }

  /**
   * fireTableChanged
   * @param value0 TODO
   */
  public void fireTableChanged (TableModelEvent event)
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
   * getListeners
   * @param value0 TODO
   * @return EventListener[]
   */
  public EventListener[] getListeners (Class listenerType)
  {
    return listenerList.getListeners (listenerType);
  }
}
