/* TableColumnModel.java --
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

import java.util.Enumeration;

import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.TableColumnModelListener;

/**
 * The interface used by {@link JTable} to access the columns in the table
 * view.
 * 
 * @author Andrew Selkirk
 */
// FIXME: The API documentation in this class is incomplete.
public interface TableColumnModel
{
  /**
   * Adds a column to the model.
   * 
   * @param column  the new column (<code>null</code> not permitted).
   * 
   * @throws IllegalArgumentException if <code>column</code> is 
   *         <code>null</code>.
   */
  void addColumn(TableColumn column);

  /**
   * Removes a column from the model.  If <code>column</code> is not defined
   * in the model, this method does nothing.
   * 
   * @param column TableColumn
   */
  void removeColumn(TableColumn column);

  /**
   * Moves a column.
   * 
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
   * Returns the number of columns in the model.
   * 
   * @return The column count
   */
  int getColumnCount();

  /**
   * getColumns
   * @return Enumeration of columns
   */
  Enumeration getColumns();

  /**
   * Returns the index of the {@link TableColumn} with the given identifier.
   *
   * @param identifier  the identifier (<code>null</code> not permitted).
   * 
   * @return The index of the {@link TableColumn} with the given identifier.
   * 
   * @throws IllegalArgumentException if <code>identifier</code> is 
   *         <code>null</code> or there is no column with that identifier.
   */
  int getColumnIndex(Object identifier);

  /**
   * Returns the <code>TableColumn</code> at the specified index.
   * 
   * @param columnIndex  the column index.
   * 
   * @return The table column.
   */
  TableColumn getColumn(int columnIndex);

  /**
   * Returns the column margin.
   * 
   * @return The column margin.
   */
  int getColumnMargin();

  /**
   * getColumnIndexAtX
   * @return Column index as position x
   */
  int getColumnIndexAtX(int xPosition);

  /**
   * getTotalColumnWidth
   * @return Total column width
   */
  int getTotalColumnWidth();

  /**
   * setColumnSelectionAllowed
   * @param value Set column selection
   */
  void setColumnSelectionAllowed(boolean value);

  /**
   * getColumnSelectionAllowed
   * @return true if column selection allowed, false otherwise
   */
  boolean getColumnSelectionAllowed();

  /**
   * getSelectedColumns
   * @return Selected columns
   */
  int[] getSelectedColumns();

  /**
   * getSelectedColumnCount
   * @return Count of selected columns
   */
  int getSelectedColumnCount();

  /**
   * setSelectionModel
   * @param model ListSelectionModel
   */
  void setSelectionModel(ListSelectionModel model);

  /**
   * getSelectionModel
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
}
