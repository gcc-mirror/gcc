/* TableColumnModel.java --
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
import java.util.Enumeration;
import javax.swing.ListSelectionModel;
import javax.swing.event.TableColumnModelListener;

/**
 * TableColumnModel public interface
 * @author Andrew Selkirk
 */
public interface TableColumnModel {

	/**
	 * addColumn
	 * @param column TableColumn
	 */
	void addColumn(TableColumn column);

	/**
	 * removeColumn
	 * @param column TableColumn
	 */
	void removeColumn(TableColumn column);

	/**
	 * moveColumn
	 * @param columnIndex Index of column to move
	 * @param newIndex New index of column
	 */
	void moveColumn(int columnIndex, int newIndex);

	/**
	 * setColumnMargin
	 * @param margin Margin of column
	 */
	void setColumnMargin(int margin);

	/**
	 * getColumnCount
	 * @returns Column count
	 */
	int getColumnCount();

	/**
	 * getColumns
	 * @returns Enumeration of columns
	 */
	Enumeration getColumns();

	/**
	 * getColumnIndex
	 * @param columnIdentifier Column id
	 */
	int getColumnIndex(Object columnIdentifier);

	/**
	 * getColumn
	 * @param columnIndex Index of column
	 */
	TableColumn getColumn(int columnIndex);

	/**
	 * getColumnMargin
	 * @returns Column margin
	 */
	int getColumnMargin();

	/**
	 * getColumnIndexAtX
	 * @returns Column index as position x
	 */
	int getColumnIndexAtX(int xPosition);

	/**
	 * getTotalColumnWidth
	 * @returns Total column width
	 */
	int getTotalColumnWidth();

	/**
	 * setColumnSelectionAllowed
	 * @param value Set column selection
	 */
	void setColumnSelectionAllowed(boolean value);

	/**
	 * getColumnSelectionAllowed
	 * @returns true if column selection allowed, false otherwise
	 */
	boolean getColumnSelectionAllowed();

	/**
	 * getSelectedColumns
	 * @returns Selected columns
	 */
	int[] getSelectedColumns();

	/**
	 * getSelectedColumnCount
	 * @returns Count of selected columns
	 */
	int getSelectedColumnCount();

	/**
	 * setSelectionModel
	 * @param model ListSelectionModel
	 */
	void setSelectionModel(ListSelectionModel model);

	/**
	 * getSelectionModel
	 * @param column TableColumn
	 */
	ListSelectionModel getSelectionModel();

	/**
	 * addColumnModelListener
	 * @param listener TableColumnModelListener
	 */
	void addColumnModelListener(TableColumnModelListener listener);

	/**
	 * removeColumnModelListener
	 * @param listener TableColumnModelListener
	 */
	void removeColumnModelListener(TableColumnModelListener listener);


} // TableColumnModel
