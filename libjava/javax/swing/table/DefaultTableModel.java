/* DefaultTableModel.java --
   Copyright (C) 2002, 2004, 2005,  Free Software Foundation, Inc.

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
import java.util.Vector;

import javax.swing.event.TableModelEvent;

/**
 * A two dimensional data structure used to store <code>Object</code> 
 * instances, usually for display in a <code>JTable</code> component.
 * 
 * @author	Andrew Selkirk
 */
public class DefaultTableModel extends AbstractTableModel
  implements Serializable
{
  static final long serialVersionUID = 6680042567037222321L;

  /**
   * Storage for the rows in the table (each row is itself 
   * a <code>Vector</code>).
   */
  protected Vector dataVector;

  /**
   * columnIdentifiers
   */
  protected Vector columnIdentifiers;

  /**
   * Creates an empty table with zero rows and zero columns.
   */
  public DefaultTableModel() 
  {
    this(0, 0);
  }
  
  /**
   * Creates a new table with the specified number of rows and columns.
   * All cells in the table are initially empty (set to <code>null</code>).
   * 
   * @param numRows  the number of rows.
   * @param numColumns  the number of columns.
   */
  public DefaultTableModel(int numRows, int numColumns) 
  {
    Vector defaultNames = new Vector(numColumns);
    Vector data = new Vector(numRows);
    for (int i = 0; i < numColumns; i++) 
      {
        defaultNames.add(super.getColumnName(i));
      }          
    for (int r = 0; r < numRows; r++) 
      {
        Vector tmp = new Vector(numColumns);
        tmp.setSize(numColumns);
        data.add(tmp);
      }
    setDataVector(data, defaultNames);
  }
  
  /**
   * Creates a new table with the specified column names and number of
   * rows.  The number of columns is determined by the number of column
   * names supplied.
   *   
   * @param columnNames the column names.
   * @param numRows the number of rows.
   */
  public DefaultTableModel(Vector columnNames, int numRows) 
  {
    if (numRows < 0)
      throw new IllegalArgumentException("numRows < 0");
    Vector data = new Vector();
    int numColumns = 0;

    if (columnNames != null)
      numColumns = columnNames.size();
    
    while (0 < numRows--) 
      {
        Vector rowData = new Vector();
        rowData.setSize(numColumns);
        data.add(rowData);
      }
    setDataVector(data, columnNames);
  }

  /**
   * Creates a new table with the specified column names and row count.
   * 
   * @param columnNames the column names.
   * @param numRows the number of rows.
   */
  public DefaultTableModel(Object[] columnNames, int numRows) 
  {
    this(convertToVector(columnNames), numRows);
  }
  
  /**
   * Creates a new table with the specified data values and column names.
   * 
   * @param data the data values.
   * @param columnNames the column names.
   */
  public DefaultTableModel(Vector data, Vector columnNames) 
  {
    setDataVector(data, columnNames);
  }

  /**
   * Creates a new table with the specified data values and column names.
   * 
   * @param data the data values.
   * @param columnNames the column names.
   */
  public DefaultTableModel(Object[][] data, Object[] columnNames) 
  {
    this(convertToVector(data), convertToVector(columnNames));
  }

  /**
   * Returns the vector containing the row data for the table.
   * 
   * @returns The data vector.
   */
  public Vector getDataVector() 
  {
    return dataVector;
  }

  /**
   * Sets the data and column identifiers for the table.  The data vector
   * contains a <code>Vector</code> for each row in the table - if the
   * number of objects in each row does not match the number of column
   * names specified, the row data is truncated or expanded (by adding
   * <code>null</code> values) as required.
   * 
   * @param data the data for the table (a vector of row vectors).
   * @param columnNames the column names.
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   */
  public void setDataVector(Vector data, Vector columnNames) 
  {
    dataVector = data;
    columnIdentifiers = columnNames;
    for (int r = 0; r < data.size(); r++) {
      ((Vector) dataVector.get(r)).setSize(columnNames.size());
    }          
  }

  /**
   * Sets the data and column identifiers for the table.
   * 
   * @param data the data for the table.
   * @param columnNames the column names.
   * 
   * @throws NullPointerException if either argument is <code>null</code>.
   */
  public void setDataVector(Object[][] data, Object[] columnNames) 
  {
    setDataVector(convertToVector(data), 
                  convertToVector(columnNames));
  }
  
  /**
   * Sends the specified <code>event</code> to all registered listeners.
   * This method is equivalent to 
   * {@link AbstractTableModel#fireTableChanged(TableModelEvent)}.
   * 
   * @param event the event.
   */
  public void newDataAvailable(TableModelEvent event) 
  {
    fireTableChanged(event);
  }

  /**
   * Sends the specified <code>event</code> to all registered listeners.
   * This method is equivalent to 
   * {@link AbstractTableModel#fireTableChanged(TableModelEvent)}.
   * 
   * @param event the event.
   */
  public void newRowsAdded(TableModelEvent event) 
  {
    fireTableChanged(event);
  }

  /**
   * Sends the specified <code>event</code> to all registered listeners.
   * This method is equivalent to 
   * {@link AbstractTableModel#fireTableChanged(TableModelEvent)}.
   * 
   * @param event the event.
   */
  public void rowsRemoved(TableModelEvent event) 
  {
    fireTableChanged(event);
  }

  /**
   * Sets the column identifiers, updates the data rows (truncating
   * or padding each row with <code>null</code> values) to match the 
   * number of columns, and sends a {@link TableModelEvent} to all
   * registered listeners.
   * 
   * @param columnIdentifiers the column identifiers.
   */
  public void setColumnIdentifiers(Vector columnIdentifiers) 
  {
    this.columnIdentifiers = columnIdentifiers;
    setColumnCount((columnIdentifiers == null ? 0 : columnIdentifiers.size()));
  }
  
  /**
   * Sets the column identifiers, updates the data rows (truncating
   * or padding each row with <code>null</code> values) to match the 
   * number of columns, and sends a {@link TableModelEvent} to all
   * registered listeners.
   * 
   * @param columnIdentifiers the column identifiers.
   */
  public void setColumnIdentifiers(Object[] columnIdentifiers) 
  {
    setColumnIdentifiers(convertToVector(columnIdentifiers));
  }

  /**
   * This method is obsolete, use {@link #setRowCount(int)} instead.
   * 
   * @param numRows the number of rows.
   */
  public void setNumRows(int numRows) 
  {
    setRowCount(numRows);
  }

  /**
   * Sets the number of rows in the table.  If <code>rowCount</code> is less
   * than the current number of rows in the table, rows are discarded.
   * If <code>rowCount</code> is greater than the current number of rows in
   * the table, new (empty) rows are added.
   * 
   * @param the row count.
   */
  public void setRowCount(int rowCount) 
  {
    int existingRowCount = dataVector.size();
    if (rowCount < existingRowCount) 
    {
      dataVector.setSize(rowCount);
      fireTableRowsDeleted(rowCount,existingRowCount-1);      
    }
    else 
    {
      int rowsToAdd = rowCount - existingRowCount;
      for (int i = 0; i < rowsToAdd; i++) 
        {
          Vector tmp = new Vector();
          tmp.setSize(columnIdentifiers.size());
          dataVector.add(tmp);
        } 
      fireTableRowsInserted(existingRowCount,rowCount-1);
    }
  }

  /**
   * Sets the number of columns in the table.  Existing rows are truncated
   * or padded with <code>null</code> values to match the new column count.
   * A {@link TableModelEvent} is sent to all registered listeners.
   * 
   * @param columnCount the column count.
   */
  public void setColumnCount(int columnCount) 
  {
    for (int i = 0; i < dataVector.size(); ++i)
      {
        ((Vector) dataVector.get(i)).setSize(columnCount);
      }
    if (columnIdentifiers != null)  
      columnIdentifiers.setSize(columnCount);
    fireTableDataChanged();
  }

  /**
   * Adds a column with the specified name to the table.  All cell values
   * for the column are initially set to <code>null</code>.
   * 
   * @param columnName the column name (<code>null</code> permitted).
   */
  public void addColumn(Object columnName) 
  {
    addColumn(columnName, (Object[]) null);
  }

  /**
   * Adds a column with the specified name and data values to the table.  
   * 
   * @param columnName the column name (<code>null</code> permitted).
   * @param columnData the column data.
   */
  public void addColumn(Object columnName, Vector columnData) 
  {
    Object[] dataArray = null;
    if (columnData != null) 
    {
      int rowCount = dataVector.size();
      if (columnData.size() < rowCount)
        columnData.setSize(rowCount);
      dataArray = columnData.toArray();
    }
    addColumn(columnName, dataArray);
  }

  /**
   * Adds a column with the specified name and data values to the table.
   * 
   * @param columnName the column name (<code>null</code> permitted).
   * @param columnData the column data.
   */
  public void addColumn(Object columnName, Object[] columnData) {
    if (columnData != null)
    {
      // check columnData array for cases where the number of items
      // doesn't match the number of rows in the existing table
      if (columnData.length > dataVector.size()) 
      {
        int rowsToAdd = columnData.length - dataVector.size();
        for (int i = 0; i < rowsToAdd; i++) 
        {
          Vector tmp = new Vector();
          tmp.setSize(columnIdentifiers.size());
          dataVector.add(tmp);
        }
      }
      else if (columnData.length < dataVector.size())
      {
        Object[] tmp = new Object[dataVector.size()];
        System.arraycopy(columnData, 0, tmp, 0, columnData.length);
        columnData = tmp;
      }
    }
    for (int i = 0; i < dataVector.size(); ++i)
      {
        ((Vector) dataVector.get(i)).add(columnData == null ? null : columnData[i]);
      }
    columnIdentifiers.add(columnName);
    fireTableDataChanged();
  }

  /**
   * Adds a new row containing the specified data to the table and sends a
   * {@link TableModelEvent} to all registered listeners.
   * 
   * @param rowData the row data (<code>null</code> permitted).
   */
  public void addRow(Vector rowData) {
    dataVector.add(rowData);
    newRowsAdded(new TableModelEvent(
      this, dataVector.size(), dataVector.size(), -1, TableModelEvent.INSERT)
    );
  }

  /**
   * Adds a new row containing the specified data to the table and sends a
   * {@link TableModelEvent} to all registered listeners.
   * 
   * @param rowData the row data (<code>null</code> permitted).
   */
  public void addRow(Object[] rowData) {
    addRow(convertToVector(rowData));
  }

  /**
   * Inserts a new row into the table.
   * 
   * @param row the row index.
   * @param rowData the row data.
   */
  public void insertRow(int row, Vector rowData) {
    dataVector.add(row, rowData);
    fireTableRowsInserted(row,row);
  }

  /**
   * Inserts a new row into the table.
   * 
   * @param row the row index.
   * @param rowData the row data.
   */
  public void insertRow(int row, Object[] rowData) {
    insertRow(row, convertToVector(rowData));
  }

  /**
   * Moves the rows from <code>startIndex</code> to <code>endIndex</code>
   * (inclusive) to the specified row.
   * 
   * @param startIndex the start row.
   * @param endIndex the end row.
   * @param toIndex the row to move to.
   */
  public void moveRow(int startIndex, int endIndex, int toIndex) {
    Vector removed = new Vector();
    for (int i = endIndex; i >= startIndex; i--)
    {
      removed.add(this.dataVector.remove(i));
    }
    for (int i = 0; i <= endIndex - startIndex; i++) 
    {
      dataVector.insertElementAt(removed.get(i), toIndex);  
    }
    fireTableDataChanged();
  }

  /**
   * Removes a row from the table and sends a {@link TableModelEvent} to
   * all registered listeners.
   * 
   * @param row the row index.
   */
  public void removeRow(int row) {
    dataVector.remove(row);
    fireTableRowsDeleted(row,row);
  }

  /**
   * getRowCount
   * @returns int
   */
  public int getRowCount() {
    return dataVector.size();
  }

  /**
   * Returns the number of columns in the model.
   * 
   * @return The column count.
   */
  public int getColumnCount() {
    return (columnIdentifiers == null ? 0 : columnIdentifiers.size());
  }

  /**
   * Returns the name of the specified column.
   * 
   * @param column the column index.
   * 
   * @returns The column name.
   */
  public String getColumnName(int column) {
    String result = "";
    if (columnIdentifiers == null) 
      result = super.getColumnName(column);
    else 
    {
      if (column < getColumnCount())
      {  
        Object id = columnIdentifiers.get(column);
        if (id != null) 
          result = id.toString();
        else
          result = super.getColumnName(column);
      }
    }
    return result;
  }

  /**
   * Returns <code>true</code> if the specified cell can be modified, and
   * <code>false</code> otherwise.  For this implementation, the method
   * always returns <code>true</code>.
   * 
   * @param row the row index.
   * @param column the column index.
   * 
   * @returns <code>true</code> in all cases.
   */
  public boolean isCellEditable(int row, int column) {
    return true;
  }

  /**
   * Returns the value at the specified cell in the table.
   * 
   * @param row the row index.
   * @param column the column index.
   * 
   * @returns The value (<code>Object</code>, possibly <code>null</code>) at 
   *          the specified cell in the table.
   */
  public Object getValueAt(int row, int column) {
    return ((Vector) dataVector.get(row)).get(column);
  }

  /**
   * Sets the value for the specified cell in the table and sends a 
   * {@link TableModelEvent} to all registered listeners.
   * 
   * @param value the value (<code>Object</code>, <code>null</code> permitted).
   * @param row the row index.
   * @param column the column index.
   */
  public void setValueAt(Object value, int row, int column) {
    ((Vector) dataVector.get(row)).set(column, value);
    fireTableCellUpdated(row,column);
  }

  /**
   * Converts the data array to a <code>Vector</code>.
   * 
   * @param data the data array (<code>null</code> permitted).
   * 
   * @returns A vector (or <code>null</code> if the data array 
   *          is <code>null</code>).
   */
  protected static Vector convertToVector(Object[] data) {
    if (data == null)
      return null;
    Vector vector = new Vector(data.length);
    for (int i = 0; i < data.length; i++) 
      vector.add(data[i]);
    return vector;          
  }
  
  /**
   * Converts the data array to a <code>Vector</code> of rows.
   * 
   * @param the data array (<code>null</code> permitted).
   * 
   * @returns A vector (or <code>null</code> if the data array 
   *          is <code>null</code>.
   */
  protected static Vector convertToVector(Object[][] data) {
    if (data == null)
      return null;
    Vector vector = new Vector(data.length);
    for (int i = 0; i < data.length; i++)
      vector.add(convertToVector(data[i]));
    return vector;
  }
}
