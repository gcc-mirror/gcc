/* DefaultTableModel.java --
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


package javax.swing.table;

import java.io.Serializable;
import java.util.Vector;

import javax.swing.event.TableModelEvent;

/**
 * DefaultTableModel
 * @author	Andrew Selkirk
 */
public class DefaultTableModel extends AbstractTableModel
  implements Serializable
{
  static final long serialVersionUID = 6680042567037222321L;
  /**
   * dataVector
   */
  protected Vector dataVector;

  /**
   * columnIdentifiers
   */
  protected Vector columnIdentifiers;

  /**
   * Constructor DefaultTableModel
   */
  public DefaultTableModel() 
  {
    this(0, 0);
  }
  
  /**
   * Constructor DefaultTableModel
   * @param value0 TODO
   * @param value1 TODO
   */
  public DefaultTableModel(int numRows, int numColumns) 
  {
    Vector defaultNames = new Vector(numColumns);
    Vector data = new Vector(numRows);
    for (int i = 0; i < numColumns; i++) 
      {
        defaultNames.add(super.getColumnName(i));
        Vector tmp = new Vector(numColumns);
        tmp.setSize(numColumns);
        data.add(tmp);
      }          
    setDataVector(defaultNames, data);
  }
  
  /**
   * Constructor DefaultTableModel
   * @param value0 TODO
   * @param value1 TODO
   */
  public DefaultTableModel(Vector columnNames, int numRows) 
  {
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
   * Constructor DefaultTableModel
   * @param value0 TODO
   * @param value1 TODO
   */
  public DefaultTableModel(Object[] columnNames, int numRows) 
  {
    this(convertToVector(columnNames), numRows);
  }
  
  /**
   * Constructor DefaultTableModel
   * @param value0 TODO
   * @param value1 TODO
   */
  public DefaultTableModel(Vector data, Vector columnNames) 
  {
    setDataVector(data, columnNames);
  }

  /**
   * Constructor DefaultTableModel
   * @param value0 TODO
   * @param value1 TODO
   */
  public DefaultTableModel(Object[][] data, Object[] columnNames) 
  {
    this(convertToVector(data), convertToVector(columnNames));
  }

  /**
   * getDataVector
   * @returns Vector
   */
  public Vector getDataVector() 
  {
    return dataVector;
  }

  /**
   * setDataVector
   * @param value0 TODO
   * @param value1 TODO
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
   * setDataVector
   * @param value0 TODO
   * @param value1 TODO
   */
  public void setDataVector(Object[][] data, Object[] columnNames) 
  {
    setDataVector(convertToVector(data), 
                  convertToVector(columnNames));
  }
  
  /**
   * newDataAvailable
   * @param value0 TODO
   */
  public void newDataAvailable(TableModelEvent event) 
  {
    fireTableChanged(event);
  }

  /**
   * newRowsAdded
   * @param value0 TODO
   */
  public void newRowsAdded(TableModelEvent event) 
  {
    fireTableChanged(event);
  }

  /**
   * rowsRemoved
   * @param value0 TODO
   */
  public void rowsRemoved(TableModelEvent event) 
  {
    fireTableChanged(event);
  }

  /**
   * setColumnIdentifiers
   * @param value0 TODO
   */
  public void setColumnIdentifiers(Vector columnIdentifiers) 
  {
    this.columnIdentifiers = columnIdentifiers;
    setColumnCount(columnIdentifiers.size());
  }
  
  /**
   * setColumnIdentifiers
   * @param value0 TODO
   */
  public void setColumnIdentifiers(Object[] columnIdentifiers) 
  {
    setColumnIdentifiers(convertToVector(columnIdentifiers));
  }

  /**
   * setNumRows
   * @param value0 TODO
   */
  public void setNumRows(int numRows) 
  {
    setRowCount(numRows);
  }

  /**
   * setRowCount
   * @param value0 TODO
   */
  public void setRowCount(int rowCount) 
  {
    dataVector.setSize(rowCount);
    fireTableDataChanged();
  }

  /**
   * setColumnCount
   * @param value0 TODO
   */
  public void setColumnCount(int columnCount) 
  {
    for (int i = 0; i < dataVector.size(); ++i)
      {
        ((Vector) dataVector.get(i)).setSize(columnCount);
      }
    columnIdentifiers.setSize(columnCount);
    fireTableDataChanged();
  }

  /**
   * addColumn
   * @param value0 TODO
   */
  public void addColumn(Object columnName) 
  {
    addColumn(columnName, (Object[]) null);
  }

  /**
   * addColumn
   * @param value0 TODO
   * @param value1 TODO
   */
  public void addColumn(Object columnName, Vector columnData) 
  {
    addColumn(columnName, columnData == null ? null : columnData.toArray());
  }

  /**
   * addColumn
   * @param value0 TODO
   * @param value1 TODO
   */
  public void addColumn(Object columnName, Object[] columnData) {
    for (int i = 0; i < dataVector.size(); ++i)
      {
        ((Vector) dataVector.get(i)).add(columnData == null ? null : columnData[i]);
      }
    columnIdentifiers.add(columnName);
    fireTableDataChanged();
  }

  /**
   * addRow
   * @param value0 TODO
   */
  public void addRow(Vector rowData) {
    dataVector.add(rowData);
    fireTableDataChanged();
  }

  /**
   * addRow
   * @param value0 TODO
   */
  public void addRow(Object[] rowData) {
    addRow(convertToVector(rowData));
  }

  /**
   * insertRow
   * @param value0 TODO
   * @param value1 TODO
   */
  public void insertRow(int row, Vector rowData) {
    dataVector.add(row, rowData);
    fireTableDataChanged();
  }

  /**
   * insertRow
   * @param value0 TODO
   * @param value1 TODO
   */
  public void insertRow(int row, Object[] rowData) {
    insertRow(row, convertToVector(rowData));
  }

  /**
   * moveRow
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   */
  public void moveRow(int startIndex, int endIndex, int toIndex) {
    for (int index = 0; index < (endIndex - startIndex); index++) {
      Vector vector = (Vector) dataVector.remove(startIndex);
      dataVector.add(toIndex, vector);
    }
    fireTableDataChanged();
  }

  /**
   * removeRow
   * @param value0 TODO
   */
  public void removeRow(int row) {
    dataVector.remove(row);
    fireTableDataChanged();
  }

  /**
   * getRowCount
   * @returns int
   */
  public int getRowCount() {
    return dataVector.size();
  }

  /**
   * getColumnCount
   * @returns int
   */
  public int getColumnCount() {
    return columnIdentifiers.size();
  }

  /**
   * getColumnName
   * @param value0 TODO
   * @returns String
   */
  public String getColumnName(int column) {
    // Check for Column
    if (columnIdentifiers == null || column >= getColumnCount()) {
      return super.getColumnName(column);
    }
          
    // Return Column name
    return (String) columnIdentifiers.get(column);          
  }

  /**
   * isCellEditable
   * @param value0 TODO
   * @param value1 TODO
   * @returns boolean
   */
  public boolean isCellEditable(int row, int column) {
    return true;
  }

  /**
   * getValueAt
   * @param value0 TODO
   * @param value1 TODO
   * @returns Object
   */
  public Object getValueAt(int row, int column) {
    return ((Vector) dataVector.get(row)).get(column);
  }

  /**
   * setValueAt
   * @param value0 TODO
   * @param value1 TODO
   * @param value2 TODO
   */
  public void setValueAt(Object value, int row, int column) {
    ((Vector) dataVector.get(row)).set(column, value);
    fireTableDataChanged();
  }

  /**
   * convertToVector
   * @param value0 TODO
   * @returns Vector
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
   * convertToVector
   * @param value0 TODO
   * @returns Vector
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
