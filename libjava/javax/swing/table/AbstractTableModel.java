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

// Imports
import java.io.*;
import java.util.*;
import javax.swing.event.*;

/**
 * AbstractTableModel
 * @author Andrew Selkirk
 */
public abstract class AbstractTableModel implements TableModel, Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * listenerList
	 */
	protected EventListenerList listenerList = new EventListenerList();


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor AbstractTableModel
	 */
	public AbstractTableModel() {
		// TODO
	} // AbstractTableModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * getColumnName
	 * @param value0 TODO
	 * @returns String
	 */
	public String getColumnName(int columnIndex) {

		// Variables
		int		index;
		int		left;
		int		base;
		int		multiplier;
		StringBuffer	buffer;
		boolean		foundFirst;

		// Ok, this is not the best solution in the world
		// and it does produce wrong answers starting 1378
		// but it's a start.  I sure hope there is a more
		// simple algorithm.  I started with a base 10 to
		// base 26 converter and later found that there
		// were so many are exceptions that it has morphed
		// into a pile of goop.
		
		// NOTE2: I have a working algorithm which is much
		// much simplier and works for all values...I'll
		// be adding it soon...

		// Process Exponent levels
		buffer = new StringBuffer();
		left = columnIndex;
		foundFirst = false;
		for (index = 6; index >= 0; index--) {
			base = (int) (Math.pow(26, index));
			if (index > 1) {
				base = base + (int) (Math.pow(26, index - 1));
			}
			if (base <= left) {
				multiplier = left / base;
				if (foundFirst == false && index > 0) {
					buffer.append((char) (multiplier + 64)); 
				} else {
					buffer.append((char) (multiplier + 65));
				}
				left = left - (base * multiplier);
				foundFirst = true;
			} else if (foundFirst == true || index == 0) {
				buffer.append('A');
			}
		} // for

		// Return Column Name
		return buffer.toString();

	} // getColumnName()

	/**
	 * findColumn
	 * @param value0 TODO
	 * @returns int
	 */
	public int findColumn(String columnName) {

		// Variables
		int		index;
		String		name;
		int		count;

		// Process Columns
		count = getColumnCount();
		for (index = 0; index < count; index++) {
			name = getColumnName(index);
			if (columnName.equals(name) == true) {
				return index;
			} // if
		} // for

		// Unable to Locate
		return -1;

	} // findColumn()

	/**
	 * getColumnClass
	 * @param value0 TODO
	 * @returns Class
	 */
	public Class getColumnClass(int columnIndex) {
		return Object.class;
	} // getColumnClass()

	/**
	 * isCellEditable
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns boolean
	 */
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return false;
	} // isCellEditable()

	/**
	 * setValueAt
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void setValueAt(Object value, int rowIndex, int columnIndex) {
		// Do nothing...
	} // setValueAt()

	/**
	 * addTableModelListener
	 * @param value0 TODO
	 */
	public void addTableModelListener(TableModelListener listener) {
		listenerList.add(TableModelListener.class, listener);
	} // addTableModelListener()

	/**
	 * removeTableModelListener
	 * @param value0 TODO
	 */
	public void removeTableModelListener(TableModelListener listener) {
		listenerList.remove(TableModelListener.class, listener);
	} // removeTableModelListener()

	/**
	 * fireTableDataChanged
	 */
	public void fireTableDataChanged() {
		fireTableChanged(new TableModelEvent(this));
	} // fireTableDataChanged()

	/**
	 * fireTableStructureChanged
	 */
	public void fireTableStructureChanged() {
		fireTableChanged(new TableModelEvent(this,
			TableModelEvent.HEADER_ROW));
	} // fireTableStructureChanged()

	/**
	 * fireTableRowsInserted
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void fireTableRowsInserted(int firstRow, int lastRow) {
		fireTableChanged(new TableModelEvent(this, firstRow, lastRow,
			TableModelEvent.ALL_COLUMNS, TableModelEvent.INSERT));
	} // fireTableRowsInserted()

	/**
	 * fireTableRowsUpdated
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void fireTableRowsUpdated(int firstRow, int lastRow) {
		fireTableChanged(new TableModelEvent(this, firstRow, lastRow,
			TableModelEvent.ALL_COLUMNS, TableModelEvent.UPDATE));
	} // fireTableRowsUpdated()

	/**
	 * fireTableRowsDeleted
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void fireTableRowsDeleted(int firstRow, int lastRow) {
		fireTableChanged(new TableModelEvent(this, firstRow, lastRow,
			TableModelEvent.ALL_COLUMNS, TableModelEvent.DELETE));
	} // fireTableRowsDeleted()

	/**
	 * fireTableCellUpdated
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void fireTableCellUpdated(int row, int column) {
		fireTableChanged(new TableModelEvent(this, row, row, column));
	} // fireTableCellUpdated()

	/**
	 * fireTableChanged
	 * @param value0 TODO
	 */
	public void fireTableChanged(TableModelEvent event) {

		// Variables
		Object[]		list;
		int			index;
		TableModelListener	listener;
 
		// Get Listener List
		list = listenerList.getListenerList();
 
		for (index = 0; index < list.length; index += 2) {
 
			// Get Listener
			listener = (TableModelListener) list[index + 1];
 
			// Notify Listener
			listener.tableChanged(event);
 
		} // for: index                                                 

	} // fireTableChanged()

	/**
	 * getListeners
	 * @param value0 TODO
	 * @returns EventListener[]
	 */
	public EventListener[] getListeners(Class listenerType) {
		return listenerList.getListeners(listenerType);
	} // getListeners()

	/**
	 * getValueAt
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns Object
	 */
	public abstract Object getValueAt(int row, int column);

	/**
	 * getColumnCount
	 * @returns int
	 */
	public abstract int getColumnCount();

	/**
	 * getRowCount
	 * @returns int
	 */
	public abstract int getRowCount();


} // AbstractTableModel

