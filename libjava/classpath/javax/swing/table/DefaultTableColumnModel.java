/* DefaultTableColumnModel.java --
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.Vector;

import javax.swing.DefaultListSelectionModel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.EventListenerList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;

/**
 * A model that stores information about the columns used in a {@link JTable}.
 *
 * @see JTable#setColumnModel(TableColumnModel)
 *
 * @author      Andrew Selkirk
 */
public class DefaultTableColumnModel
  implements TableColumnModel, PropertyChangeListener, ListSelectionListener,
             Serializable
{
  private static final long serialVersionUID = 6580012493508960512L;

  /**
   * Storage for the table columns.
   */
  protected Vector<TableColumn> tableColumns;

  /**
   * A selection model that keeps track of column selections.
   */
  protected ListSelectionModel selectionModel;

  /**
   * The space between the columns (the default value is <code>1</code>).
   */
  protected int columnMargin;

  /**
   * Storage for the listeners registered with the model.
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * A change event used when notifying listeners of a change to the
   * <code>columnMargin</code> field.  This single event is reused for all
   * notifications (it is lazily instantiated within the
   * {@link #fireColumnMarginChanged()} method).
   */
  protected transient ChangeEvent changeEvent;

  /**
   * A flag that indicates whether or not columns can be selected.
   */
  protected boolean columnSelectionAllowed;

  /**
   * The total width of all the columns in this model.
   */
  protected int totalColumnWidth;

  /**
   * Creates a new table column model with zero columns.  A default column
   * selection model is created by calling {@link #createSelectionModel()}.
   * The default value for <code>columnMargin</code> is <code>1</code> and
   * the default value for <code>columnSelectionAllowed</code> is
   * <code>false</code>.
   */
  public DefaultTableColumnModel()
  {
    tableColumns = new Vector();
    selectionModel = createSelectionModel();
    selectionModel.addListSelectionListener(this);
    columnMargin = 1;
    columnSelectionAllowed = false;
  }

  /**
   * Adds a column to the model then calls
   * {@link #fireColumnAdded(TableColumnModelEvent)} to notify the registered
   * listeners.  The model registers itself with the column as a
   * {@link PropertyChangeListener} so that changes to the column width will
   * invalidate the cached {@link #totalColumnWidth} value.
   *
   * @param column  the column (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if <code>column</code> is
   *     <code>null</code>.
   *
   * @see #removeColumn(TableColumn)
   */
  public void addColumn(TableColumn column)
  {
    if (column == null)
      throw new IllegalArgumentException("Null 'col' argument.");
    tableColumns.add(column);
    column.addPropertyChangeListener(this);
    invalidateWidthCache();
    fireColumnAdded(new TableColumnModelEvent(this, 0,
                                              tableColumns.size() - 1));
  }

  /**
   * Removes a column from the model then calls
   * {@link #fireColumnRemoved(TableColumnModelEvent)} to notify the registered
   * listeners.  If the specified column does not belong to the model, or is
   * <code>null</code>, this method does nothing.
   *
   * @param column the column to be removed (<code>null</code> permitted).
   *
   * @see #addColumn(TableColumn)
   */
  public void removeColumn(TableColumn column)
  {
    int index = this.tableColumns.indexOf(column);
    if (index < 0)
      return;
    tableColumns.remove(column);
    fireColumnRemoved(new TableColumnModelEvent(this, index, 0));
    column.removePropertyChangeListener(this);
    invalidateWidthCache();
  }

  /**
   * Moves the column at index i to the position specified by index j, then
   * calls {@link #fireColumnMoved(TableColumnModelEvent)} to notify registered
   * listeners.
   *
   * @param i index of the column that will be moved.
   * @param j index of the column's new location.
   *
   * @throws IllegalArgumentException if <code>i</code> or <code>j</code> are
   *     outside the range <code>0</code> to <code>N-1</code>, where
   *     <code>N</code> is the column count.
   */
  public void moveColumn(int i, int j)
  {
    int columnCount = getColumnCount();
    if (i < 0 || i >= columnCount)
      throw new IllegalArgumentException("Index 'i' out of range.");
    if (j < 0 || j >= columnCount)
      throw new IllegalArgumentException("Index 'j' out of range.");
    TableColumn column = tableColumns.remove(i);
    tableColumns.add(j, column);
    fireColumnMoved(new TableColumnModelEvent(this, i, j));
  }

  /**
   * Sets the column margin then calls {@link #fireColumnMarginChanged()} to
   * notify the registered listeners.
   *
   * @param margin  the column margin.
   *
   * @see #getColumnMargin()
   */
  public void setColumnMargin(int margin)
  {
    columnMargin = margin;
    fireColumnMarginChanged();
  }

  /**
   * Returns the number of columns in the model.
   *
   * @return The column count.
   */
  public int getColumnCount()
  {
    return tableColumns.size();
  }

  /**
   * Returns an enumeration of the columns in the model.
   *
   * @return An enumeration of the columns in the model.
   */
  public Enumeration<TableColumn> getColumns()
  {
    return tableColumns.elements();
  }

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
  public int getColumnIndex(Object identifier)
  {
    if (identifier == null)
      throw new IllegalArgumentException("Null identifier.");
    int columnCount = tableColumns.size();
    for (int i = 0; i < columnCount; i++)
    {
      TableColumn tc = tableColumns.get(i);
      if (identifier.equals(tc.getIdentifier()))
        return i;
    }
    throw new IllegalArgumentException("No TableColumn with that identifier.");
  }

  /**
   * Returns the column at the specified index.
   *
   * @param columnIndex  the column index (in the range from <code>0</code> to
   *     <code>N-1</code>, where <code>N</code> is the number of columns in
   *     the model).
   *
   * @return The column at the specified index.
   *
   * @throws ArrayIndexOutOfBoundsException if <code>i</code> is not within
   *     the specified range.
   */
  public TableColumn getColumn(int columnIndex)
  {
    return tableColumns.get(columnIndex);
  }

  /**
   * Returns the column margin.
   *
   * @return The column margin.
   *
   * @see #setColumnMargin(int)
   */
  public int getColumnMargin()
  {
    return columnMargin;
  }

  /**
   * Returns the index of the column that contains the specified x-coordinate.
   * This method assumes that:
   * <ul>
   * <li>column zero begins at position zero;</li>
   * <li>all columns appear in order;</li>
   * <li>individual column widths are taken into account, but the column margin
   *     is ignored.</li>
   * </ul>
   * If no column contains the specified position, this method returns
   * <code>-1</code>.
   *
   * @param x  the x-position.
   *
   * @return The column index, or <code>-1</code>.
   */
  public int getColumnIndexAtX(int x)
  {
    for (int i = 0; i < tableColumns.size(); ++i)
      {
        int w = (tableColumns.get(i)).getWidth();
        if (0 <= x && x < w)
          return i;
        else
          x -= w;
      }
    return -1;
  }

  /**
   * Returns total width of all the columns in the model, ignoring the
   * {@link #columnMargin}.
   *
   * @return The total width of all the columns.
   */
  public int getTotalColumnWidth()
  {
    if (totalColumnWidth == -1)
      recalcWidthCache();
    return totalColumnWidth;
  }

  /**
   * Sets the selection model that will be used to keep track of the selected
   * columns.
   *
   * @param model  the selection model (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if <code>model</code> is
   *     <code>null</code>.
   *
   * @see #getSelectionModel()
   */
  public void setSelectionModel(ListSelectionModel model)
  {
    if (model == null)
      throw new IllegalArgumentException();

    selectionModel.removeListSelectionListener(this);
    selectionModel = model;
    selectionModel.addListSelectionListener(this);
  }

  /**
   * Returns the selection model used to track table column selections.
   *
   * @return The selection model.
   *
   * @see #setSelectionModel(ListSelectionModel)
   */
  public ListSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  /**
   * Sets the flag that indicates whether or not column selection is allowed.
   *
   * @param flag  the new flag value.
   *
   * @see #getColumnSelectionAllowed()
   */
  public void setColumnSelectionAllowed(boolean flag)
  {
    columnSelectionAllowed = flag;
  }

  /**
   * Returns <code>true</code> if column selection is allowed, and
   * <code>false</code> if column selection is not allowed.
   *
   * @return A boolean.
   *
   * @see #setColumnSelectionAllowed(boolean)
   */
  public boolean getColumnSelectionAllowed()
  {
    return columnSelectionAllowed;
  }

  /**
   * Returns an array containing the indices of the selected columns.
   *
   * @return An array containing the indices of the selected columns.
   */
  public int[] getSelectedColumns()
  {
    // FIXME: Implementation of this method was taken from private method
    // JTable.getSelections(), which is used in various places in JTable
    // including selected row calculations and cannot be simply removed.
    // This design should be improved to illuminate duplication of code.

    ListSelectionModel lsm = this.selectionModel;
    int sz = getSelectedColumnCount();
    int [] ret = new int[sz];

    int lo = lsm.getMinSelectionIndex();
    int hi = lsm.getMaxSelectionIndex();
    int j = 0;
    java.util.ArrayList ls = new java.util.ArrayList();
    if (lo != -1 && hi != -1)
      {
        switch (lsm.getSelectionMode())
          {
          case ListSelectionModel.SINGLE_SELECTION:
            ret[0] = lo;
            break;

          case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
            for (int i = lo; i <= hi; ++i)
              ret[j++] = i;
            break;

          case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:
            for (int i = lo; i <= hi; ++i)
              if (lsm.isSelectedIndex(i))
                ret[j++] = i;
            break;
          }
      }
    return ret;
  }

  /**
   * Returns the number of selected columns in the model.
   *
   * @return The selected column count.
   *
   * @see #getSelectionModel()
   */
  public int getSelectedColumnCount()
  {
    // FIXME: Implementation of this method was taken from private method
    // JTable.countSelections(), which is used in various places in JTable
    // including selected row calculations and cannot be simply removed.
    // This design should be improved to illuminate duplication of code.

    ListSelectionModel lsm = this.selectionModel;
    int lo = lsm.getMinSelectionIndex();
    int hi = lsm.getMaxSelectionIndex();
    int sum = 0;

    if (lo != -1 && hi != -1)
      {
        switch (lsm.getSelectionMode())
          {
          case ListSelectionModel.SINGLE_SELECTION:
            sum = 1;
            break;

          case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
            sum = hi - lo + 1;
            break;

          case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:
            for (int i = lo; i <= hi; ++i)
              if (lsm.isSelectedIndex(i))
                ++sum;
            break;
          }
      }

     return sum;
  }

  /**
   * Registers a listener with the model, so that it will receive
   * {@link TableColumnModelEvent} notifications.
   *
   * @param listener the listener (<code>null</code> ignored).
   */
  public void addColumnModelListener(TableColumnModelListener listener)
  {
    listenerList.add(TableColumnModelListener.class, listener);
  }

  /**
   * Deregisters a listener so that it no longer receives notification of
   * changes to this model.
   *
   * @param listener  the listener to remove
   */
  public void removeColumnModelListener(TableColumnModelListener listener)
  {
    listenerList.remove(TableColumnModelListener.class, listener);
  }

  /**
   * Returns an array containing the listeners that are registered with the
   * model.  If there are no listeners, an empty array is returned.
   *
   * @return An array containing the listeners that are registered with the
   *     model.
   *
   * @see #addColumnModelListener(TableColumnModelListener)
   * @since 1.4
   */
  public TableColumnModelListener[] getColumnModelListeners()
  {
    return (TableColumnModelListener[])
      listenerList.getListeners(TableColumnModelListener.class);
  }

  /**
   * Sends the specified {@link TableColumnModelEvent} to all registered
   * listeners, to indicate that a column has been added to the model.  The
   * event's <code>toIndex</code> attribute should contain the index of the
   * added column.
   *
   * @param e  the event.
   *
   * @see #addColumn(TableColumn)
   */
  protected void fireColumnAdded(TableColumnModelEvent e)
  {
    TableColumnModelListener[] listeners = getColumnModelListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].columnAdded(e);
  }

  /**
   * Sends the specified {@link TableColumnModelEvent} to all registered
   * listeners, to indicate that a column has been removed from the model.  The
   * event's <code>fromIndex</code> attribute should contain the index of the
   * removed column.
   *
   * @param e  the event.
   *
   * @see #removeColumn(TableColumn)
   */
  protected void fireColumnRemoved(TableColumnModelEvent e)
  {
    TableColumnModelListener[] listeners = getColumnModelListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].columnRemoved(e);
  }

  /**
   * Sends the specified {@link TableColumnModelEvent} to all registered
   * listeners, to indicate that a column in the model has been moved.  The
   * event's <code>fromIndex</code> attribute should contain the old column
   * index, and the <code>toIndex</code> attribute should contain the new
   * column index.
   *
   * @param e  the event.
   *
   * @see #moveColumn(int, int)
   */
  protected void fireColumnMoved(TableColumnModelEvent e)
  {
    TableColumnModelListener[] listeners = getColumnModelListeners();

    for (int i = 0; i < listeners.length; i++)
      listeners[i].columnMoved(e);
  }

  /**
   * Sends the specified {@link ListSelectionEvent} to all registered listeners,
   * to indicate that the column selections have changed.
   *
   * @param e  the event.
   *
   * @see #valueChanged(ListSelectionEvent)
   */
  protected void fireColumnSelectionChanged(ListSelectionEvent e)
  {
    EventListener [] listeners = getListeners(TableColumnModelListener.class);
    for (int i = 0; i < listeners.length; ++i)
      ((TableColumnModelListener) listeners[i]).columnSelectionChanged(e);
  }

  /**
   * Sends a {@link ChangeEvent} to the model's registered listeners to
   * indicate that the column margin was changed.
   *
   * @see #setColumnMargin(int)
   */
  protected void fireColumnMarginChanged()
  {
    EventListener[] listeners = getListeners(TableColumnModelListener.class);
    if (changeEvent == null && listeners.length > 0)
      changeEvent = new ChangeEvent(this);
    for (int i = 0; i < listeners.length; ++i)
      ((TableColumnModelListener) listeners[i]).columnMarginChanged(changeEvent);
  }

  /**
   * Returns an array containing the listeners (of the specified type) that
   * are registered with this model.
   *
   * @param listenerType  the listener type (must indicate a subclass of
   *     {@link EventListener}, <code>null</code> not permitted).
   *
   * @return An array containing the listeners (of the specified type) that
   *     are registered with this model.
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * Receives notification of property changes for the columns in the model.
   * If the <code>width</code> property for any column changes, we invalidate
   * the {@link #totalColumnWidth} value here.
   *
   * @param event  the event.
   */
  public void propertyChange(PropertyChangeEvent event)
  {
    if (event.getPropertyName().equals("width"))
          invalidateWidthCache();
  }

  /**
   * Receives notification of the change to the list selection model, and
   * responds by calling
   * {@link #fireColumnSelectionChanged(ListSelectionEvent)}.
   *
   * @param e  the list selection event.
   *
   * @see #getSelectionModel()
   */
  public void valueChanged(ListSelectionEvent e)
  {
    fireColumnSelectionChanged(e);
  }

  /**
   * Creates a default selection model to track the currently selected
   * column(s).  This method is called by the constructor and returns a new
   * instance of {@link DefaultListSelectionModel}.
   *
   * @return A new default column selection model.
   */
  protected ListSelectionModel createSelectionModel()
  {
    return new DefaultListSelectionModel();
  }

  /**
   * Recalculates the total width of the columns, if the cached value is
   * <code>-1</code>.  Otherwise this method does nothing.
   *
   * @see #getTotalColumnWidth()
   */
  protected void recalcWidthCache()
  {
    if (totalColumnWidth == -1)
      {
        totalColumnWidth = 0;
        for (int i = 0; i < tableColumns.size(); ++i)
          {
            totalColumnWidth += tableColumns.get(i).getWidth();
          }
      }
  }

  /**
   * Sets the {@link #totalColumnWidth} field to <code>-1</code>.
   *
   * @see #recalcWidthCache()
   */
  private void invalidateWidthCache()
  {
    totalColumnWidth = -1;
  }
}
