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
 * TableColumnModel interface
 * @author Andrew Selkirk
 */
public interface TableColumnModel {

	/**
	 * addColumn
	 * @param column TableColumn
	 */
	public void addColumn(TableColumn column);

	/**
	 * removeColumn
	 * @param column TableColumn
	 */
	public void removeColumn(TableColumn column);

	/**
	 * moveColumn
	 * @param columnIndex Index of column to move
	 * @param newIndex New index of column
	 */
	public void moveColumn(int columnIndex, int newIndex);

	/**
	 * setColumnMargin
	 * @param margin Margin of column
	 */
	public void setColumnMargin(int margin);

	/**
	 * getColumnCount
	 * @returns Column count
	 */
	public int getColumnCount();

	/**
	 * getColumns
	 * @returns Enumeration of columns
	 */
	public Enumeration getColumns();

	/**
	 * getColumnIndex
	 * @param columnIdentifier Column id
	 */
	public int getColumnIndex(Object columnIdentifier);

	/**
	 * getColumn
	 * @param columnIndex Index of column
	 */
	public TableColumn getColumn(int columnIndex);

	/**
	 * getColumnMargin
	 * @returns Column margin
	 */
	public int getColumnMargin();

	/**
	 * getColumnIndexAtX
	 * @returns Column index as position x
	 */
	public int getColumnIndexAtX(int xPosition);

	/**
	 * getTotalColumnWidth
	 * @returns Total column width
	 */
	public int getTotalColumnWidth();

	/**
	 * setColumnSelectionAllowed
	 * @param value Set column selection
	 */
	public void setColumnSelectionAllowed(boolean value);

	/**
	 * getColumnSelectionAllowed
	 * @returns true if column selection allowed, false otherwise
	 */
	public boolean getColumnSelectionAllowed();

	/**
	 * getSelectedColumns
	 * @returns Selected columns
	 */
	public int[] getSelectedColumns();

	/**
	 * getSelectedColumnCount
	 * @returns Count of selected columns
	 */
	public int getSelectedColumnCount();

	/**
	 * setSelectionModel
	 * @param model ListSelectionModel
	 */
	public void setSelectionModel(ListSelectionModel model);

	/**
	 * getSelectionModel
	 * @param column TableColumn
	 */
	public ListSelectionModel getSelectionModel();

	/**
	 * addColumnModelListener
	 * @param listener TableColumnModelListener
	 */
	public void addColumnModelListener(TableColumnModelListener listener);

	/**
	 * removeColumnModelListener
	 * @param listener TableColumnModelListener
	 */
	public void removeColumnModelListener(TableColumnModelListener listener);


} // TableColumnModel
