/* TableColumnModel.java --
   Copyright (C) 2002, 2004, 2005, 2006,  Free Software Foundation, Inc.

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
import javax.swing.event.ChangeEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;

/**
 * The interface used by {@link JTable} to access the columns in the table
 * view.
 * 
 * @author Andrew Selkirk
 */
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
   * Sets the column margin and sends a {@link ChangeEvent} to all registered
   * {@link TableColumnModelListener}s registered with the model.
   * 
   * @param margin  the column margin.
   * 
   * @see #getColumnMargin()
   */
  void setColumnMargin(int margin);

  /**
   * Returns the number of columns in the model.
   * 
   * @return The column count.
   */
  int getColumnCount();

  /**
   * Returns an enumeration of the columns in the model.
   * 
   * @return An enumeration of the columns in the model.
   */
  Enumeration<TableColumn> getColumns();

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
   * 
   * @see #setColumnMargin(int)
   */
  int getColumnMargin();

  /**
   * Returns the index of the column that contains the specified x-coordinate,
   * assuming that:
   * <ul>
   * <li>column zero begins at position zero;</li>
   * <li>all columns appear in order;</li>
   * <li>individual column widths are taken into account, but the column margin
   *     is ignored.</li>
   * </ul>
   * If no column contains the specified position, this method returns 
   * <code>-1</code>.
   * 
   * @param xPosition  the x-position.
   * 
   * @return The column index, or <code>-1</code>.
   */
  int getColumnIndexAtX(int xPosition);

  /**
   * Returns total width of all the columns in the model, ignoring the
   * column margin (see {@link #getColumnMargin()}).
   *
   * @return The total width of all the columns.
   */
  int getTotalColumnWidth();

  /**
   * Sets the flag that indicates whether or not column selection is allowed.
   *
   * @param allowed  the new flag value.
   * 
   * @see #getColumnSelectionAllowed()
   */
  void setColumnSelectionAllowed(boolean allowed);

  /**
   * Returns <code>true</code> if column selection is allowed, and 
   * <code>false</code> if column selection is not allowed.
   *
   * @return A boolean.
   * 
   * @see #setColumnSelectionAllowed(boolean)
   */
  boolean getColumnSelectionAllowed();

  /**
   * getSelectedColumns
   * @return Selected columns
   */
  int[] getSelectedColumns();

  /**
   * Returns the number of selected columns in the model.
   * 
   * @return The selected column count.
   * 
   * @see #getSelectionModel()
   */
  int getSelectedColumnCount();

  /**
   * Sets the selection model that will be used to keep track of the selected 
   * columns.
   *
   * @param model  the selection model (<code>null</code> not permitted).
   * 
   * @throws IllegalArgumentException if <code>model</code> is 
   *     <code>null</code>.
   */
  void setSelectionModel(ListSelectionModel model);

  /**
   * Returns the selection model used to track table column selections.
   * 
   * @return The selection model.
   * 
   * @see #setSelectionModel(ListSelectionModel)
   */
  ListSelectionModel getSelectionModel();

  /**
   * Registers a listener with the model, so that it will receive
   * {@link TableColumnModelEvent} notifications.
   *
   * @param listener the listener (<code>null</code> ignored).
   */
  void addColumnModelListener(TableColumnModelListener listener);

  /**
   * Deregisters a listener, so that it will no longer receive 
   * {@link TableColumnModelEvent} notifications.
   * 
   * @param listener  the listener.
   */
  void removeColumnModelListener(TableColumnModelListener listener);
}
