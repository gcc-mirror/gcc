/* DefaultTableModel.java --
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

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * dataVector
	 */
	protected Vector dataVector;

	/**
	 * columnIdentifiers
	 */
	protected Vector columnIdentifiers;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultTableModel
	 */
	public DefaultTableModel() {
		this(0, 0);
	} // DefaultTableModel()

	/**
	 * Constructor DefaultTableModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTableModel(int numRows, int numColumns) {

		// Variables
		int		columnIndex;
		Vector		defaultNames;

		// Create Column Names
		defaultNames = new Vector();
		for (columnIndex = 0; columnIndex < numColumns; columnIndex++) {
			defaultNames.addElement(super.getColumnName(columnIndex));
		} // for

		// Setup Data
//		setDataVector(defaultNames, numRows);

	} // DefaultTableModel()

	/**
	 * Constructor DefaultTableModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTableModel(Vector columnNames, int numRows) {

		// Variables
		Vector		data;
		Vector		rowData;
		int		rowIndex;
		int		numColumns;

		// Create Data
		data = new Vector();
		if (columnNames == null) {
			numColumns = 0;
		} else {
			numColumns = columnNames.size();
		} // if
		for (rowIndex = 0; rowIndex < numRows; rowIndex++) {
			rowData = new Vector();
			rowData.setSize(numColumns);
			data.addElement(rowData);
		} // for

		// Setup Data
		setDataVector(data, columnNames);

	} // DefaultTableModel()

	/**
	 * Constructor DefaultTableModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTableModel(Object[] columnNames, int numRows) {
		this(convertToVector(columnNames), numRows);
	} // DefaultTableModel()

	/**
	 * Constructor DefaultTableModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTableModel(Vector data, Vector columnNames) {
		setDataVector(data, columnNames);
	} // DefaultTableModel()

	/**
	 * Constructor DefaultTableModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTableModel(Object[][] data, Object[] columnNames) {
		this(convertToVector(data), convertToVector(columnNames));
	} // DefaultTableModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getDataVector
	 * @returns Vector
	 */
	public Vector getDataVector() {
		return dataVector;
	} // getDataVector()

	/**
	 * setDataVector
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void setDataVector(Vector data, Vector columnNames) {

		// Variables
		int	rowIndex;
		int	numRows;
		int	numColumns;
		Vector	columnVector;

		// Set Data
		dataVector = data;
		columnIdentifiers = columnNames;

		// Check Data
		numRows = data.size();
		numColumns = columnNames.size();
		for (rowIndex = 0; rowIndex < numRows; rowIndex++) {
			columnVector = (Vector) dataVector.get(rowIndex);
			columnVector.setSize(numColumns);
		} // for

	} // setDataVector()

	/**
	 * setDataVector
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void setDataVector(Object[][] data, Object[] columnNames) {
		setDataVector(convertToVector(data), convertToVector(columnNames));
	} // setDataVector()

	/**
	 * newDataAvailable
	 * @param value0 TODO
	 */
	public void newDataAvailable(TableModelEvent event) {
		fireTableChanged(event);
	} // newDataAvailable()

	/**
	 * newRowsAdded
	 * @param value0 TODO
	 */
	public void newRowsAdded(TableModelEvent event) {
		// TODO
	} // newRowsAdded()

	/**
	 * rowsRemoved
	 * @param value0 TODO
	 */
	public void rowsRemoved(TableModelEvent event) {
		fireTableChanged(event);
	} // rowsRemoved()

	/**
	 * setColumnIdentifiers
	 * @param value0 TODO
	 */
	public void setColumnIdentifiers(Vector columnIdentifiers) {
		this.columnIdentifiers = columnIdentifiers;
		setColumnCount(columnIdentifiers.size());
	} // setColumnIdentifiers()

	/**
	 * setColumnIdentifiers
	 * @param value0 TODO
	 */
	public void setColumnIdentifiers(Object[] columnIdentifiers) {
		setColumnIdentifiers(convertToVector(columnIdentifiers));
	} // setColumnIdentifiers()

	/**
	 * setNumRows
	 * @param value0 TODO
	 */
	public void setNumRows(int numRows) {
		setRowCount(numRows);
	} // setNumRows()

	/**
	 * setRowCount
	 * @param value0 TODO
	 */
	public void setRowCount(int rowCount) {
		// TODO
	} // setRowCount()

	/**
	 * setColumnCount
	 * @param value0 TODO
	 */
	public void setColumnCount(int columnCount) {
		// TODO
	} // setColumnCount()

	/**
	 * addColumn
	 * @param value0 TODO
	 */
	public void addColumn(Object columnName) {
		addColumn(columnName, new Vector(dataVector.size()));
	} // addColumn()

	/**
	 * addColumn
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void addColumn(Object columnName, Vector columnData) {
		// TODO
	} // addColumn()

	/**
	 * addColumn
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void addColumn(Object columnName, Object[] columnData) {
		// TODO
	} // addColumn()

	/**
	 * addRow
	 * @param value0 TODO
	 */
	public void addRow(Vector rowData) {
		// TODO
	} // addRow()

	/**
	 * addRow
	 * @param value0 TODO
	 */
	public void addRow(Object[] rowData) {
		addRow(convertToVector(rowData));
	} // addRow()

	/**
	 * insertRow
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void insertRow(int row, Vector rowData) {
		dataVector.add(row, rowData);
	} // insertRow()

	/**
	 * insertRow
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void insertRow(int row, Object[] rowData) {
		insertRow(row, convertToVector(rowData));
	} // insertRow()

	/**
	 * moveRow
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void moveRow(int startIndex, int endIndex, int toIndex) {

		// Variables
		int		index;
		Vector	vector;

		// Move Rows
		for (index = 0; index < (endIndex - startIndex); index++) {
			vector = (Vector) dataVector.remove(startIndex);
			dataVector.add(toIndex, vector);
		} // for

	} // moveRow()

	/**
	 * removeRow
	 * @param value0 TODO
	 */
	public void removeRow(int row) {
		dataVector.remove(row);
	} // removeRow()

	/**
	 * getRowCount
	 * @returns int
	 */
	public int getRowCount() {
		return dataVector.size();
	} // getRowCount()

	/**
	 * getColumnCount
	 * @returns int
	 */
	public int getColumnCount() {
		return columnIdentifiers.size();
	} // getColumnCount()

	/**
	 * getColumnName
	 * @param value0 TODO
	 * @returns String
	 */
	public String getColumnName(int column) {

		// Check for Column
		if (columnIdentifiers == null || column >= getColumnCount()) {
			return super.getColumnName(column);
		} // if

		// Return Column name
		return (String) columnIdentifiers.get(column);

	} // getColumnName()

	/**
	 * isCellEditable
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns boolean
	 */
	public boolean isCellEditable(int row, int column) {
		return true;
	} // isCellEditable()

	/**
	 * getValueAt
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns Object
	 */
	public Object getValueAt(int row, int column) {

		// Variables
		Vector	rowVector;

		// Get Row Vector
		rowVector = (Vector) dataVector.get(row);

		// Get Data
		return rowVector.get(column);

	} // getValueAt()

	/**
	 * setValueAt
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void setValueAt(Object value, int row, int column) {

		// Variables
		Vector	rowVector;

		// Get Row Vector
		rowVector = (Vector) dataVector.get(row);

		// Set Data
		rowVector.remove(column);
		rowVector.add(column, value);

	} // setValueAt()

	/**
	 * convertToVector
	 * @param value0 TODO
	 * @returns Vector
	 */
	protected static Vector convertToVector(Object[] data) {

		// Variables
		int	index;
		Vector	vector;

		// Check for null
		if (data == null) {
			return null;
		} // if

		// Process
		vector = new Vector();
		for (index = 0; index < data.length; index++) {
			vector.add(data[index]);
		} // for: index

		// Return new Vector
		return vector;

	} // convertToVector()

	/**
	 * convertToVector
	 * @param value0 TODO
	 * @returns Vector
	 */
	protected static Vector convertToVector(Object[][] data) {

		// Variables
		int	index;
		Vector	vector;

		// Process
		vector = new Vector();
		for (index = 0; index < data.length; index++) {
			vector.add(convertToVector(data[index]));
		} // for: index

		// Return new Vector
		return vector;

	} // convertToVector()


} // DefaultTableModel
