/* TableModel.java --
   Copyright (C) 2002, 2005, Free Software Foundation, Inc.

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

import javax.swing.event.TableModelListener;

/**
 * A <code>TableModel</code> is a two dimensional data structure that
 * can store arbitrary <code>Object</code> instances, usually for the
 * purpose of display in a {@link javax.swing.JTable} component.  Individual
 * objects can be accessed by specifying the row index and column index for
 * the object.  Each column in the model has a name associated with it.
 * <p>
 * The {@link DefaultTableModel} class provides one implementation of
 * this interface.
 *
 * @author Andrew Selkirk
 */
public interface TableModel
{
  /**
   * Returns the number of rows in the model.
   *
   * @return The row count.
   */
  int getRowCount();

  /**
   * Returns the number of columns in the model.
   *
   * @return The column count
   */
  int getColumnCount();

  /**
   * Returns the name of a column in the model.
   *
   * @param columnIndex the column index.
   *
   * @return The column name.
   */
  String getColumnName(int columnIndex);

  /**
   * Returns the <code>Class</code> for all <code>Object</code> instances
   * in the specified column.
   *
   * @param columnIndex the column index.
   *
   * @return The class.
   */
  Class<?> getColumnClass(int columnIndex);

  /**
   * Returns <code>true</code> if the cell is editable, and <code>false</code>
   * otherwise.
   *
   * @param rowIndex the row index.
   * @param columnIndex the column index.
   *
   * @return <code>true</code> if editable, <code>false</code> otherwise.
   */
  boolean isCellEditable(int rowIndex, int columnIndex);

  /**
   * Returns the value (<code>Object</code>) at a particular cell in the
   * table.
   *
   * @param rowIndex the row index.
   * @param columnIndex the column index.
   *
   * @return The value at the specified cell.
   */
  Object getValueAt(int rowIndex, int columnIndex);

  /**
   * Sets the value at a particular cell in the table.
   *
   * @param aValue the value (<code>null</code> permitted).
   * @param rowIndex the row index.
   * @param columnIndex the column index.
   */
  void setValueAt(Object aValue, int rowIndex, int columnIndex);

  /**
   * Adds a listener to the model.  The listener will receive notification
   * of updates to the model.
   *
   * @param listener the listener.
   */
  void addTableModelListener(TableModelListener listener);

  /**
   * Removes a listener from the model.
   *
   * @param listener the listener.
   */
  void removeTableModelListener(TableModelListener listener);
}
