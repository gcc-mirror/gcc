/* DefaultTableColumnModel.java --
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.Enumeration;
import java.util.EventListener;
import java.util.Vector;

import javax.swing.DefaultListSelectionModel;
import javax.swing.ListSelectionModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.EventListenerList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;

/**
 * DefaultTableColumnModel
 * @author	Andrew Selkirk
 * @version	1.0
 */
public class DefaultTableColumnModel
  implements TableColumnModel, PropertyChangeListener, ListSelectionListener,
             Serializable
{
  private static final long serialVersionUID = 6580012493508960512L;

  /**
   * Columns that this model keeps track of.
   */
  protected Vector tableColumns;

  /**
   * Selection Model that keeps track of columns selection
   */
  protected ListSelectionModel selectionModel;

  /**
   * Space between two columns. By default it is set to 1
   */
  protected int columnMargin;

  /**
   * listenerList keeps track of all listeners registered with this model
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * changeEvent is fired when change occurs in one of the columns properties
   */
  protected transient ChangeEvent changeEvent = new ChangeEvent(this);

  /**
   * Indicates whether columns can be selected 
   */
  protected boolean columnSelectionAllowed;

  /**
   * Total width of all the columns in this model
   */
  protected int totalColumnWidth;

  /**
   * Constructor DefaultTableColumnModel
   */
  public DefaultTableColumnModel()
  {
    tableColumns = new Vector();
    setSelectionModel(createSelectionModel());
    columnMargin = 1;
    columnSelectionAllowed = false;
  }

  /**
   * addColumn adds column to the model. This method fires ColumnAdded 
   * event to model's registered TableColumnModelListeners.
   *
   * @param col column to add
   */
  public void addColumn(TableColumn col)
  {
    tableColumns.add(col);
    invalidateWidthCache();
    fireColumnAdded(new TableColumnModelEvent(this,0,tableColumns.size()));
  }

  /**
   * removeColumn removes table column from the model. This method fires 
   * ColumnRemoved event to model's registered TableColumnModelListeners.
   *
   * @param col column to be removed
   */
  public void removeColumn(TableColumn col)
  {
    int index = getColumnIndex(col);
    fireColumnRemoved(new TableColumnModelEvent(this,index,0));    
    tableColumns.remove(col);
    invalidateWidthCache();
  }

  /**
   * moveColumn moves column at index i to index j. This method fires
   * ColumnMoved event to model's registered TableColumnModelListeners.
   *
   * @param i index of the column that will be moved
   * @param j index of column's new location
   */
  public void moveColumn(int i, int j)
  {
    Object tmp = tableColumns.get(i);
    tableColumns.set(i, tableColumns.get(j));
    tableColumns.set(j, tmp);
    fireColumnAdded(new TableColumnModelEvent(this,i,j));
  }

  /**
   * setColumnMargin sets margin of the columns.
   * @param m new column margin
   */
  public void setColumnMargin(int m)
  {
    columnMargin = m;
    fireColumnMarginChanged();
  }

  /**
   * getColumnCount returns number of columns in the model
   * @return int number of columns in the model
   */
  public int getColumnCount()
  {
    return tableColumns.size();
  }

  /**
   * getColumns
   * @return Enumeration
   */
  public Enumeration getColumns()
  {
    return tableColumns.elements();
  }

  /**
   * getColumnIndex returns index of the specified column
   *
   * @param identifier identifier of the column
   * @return int index of the given column
   */
  public int getColumnIndex(Object identifier)
  {
    return tableColumns.indexOf(identifier, 0);
  }

  /**
   * getColumn returns column at the specified index
   * @param i index of the column 
   * @return TableColumn column at the specified index
   */
  public TableColumn getColumn(int i)
  {
    return (TableColumn) tableColumns.get(i);
  }

  /**
   * getColumnMargin returns column margin
   * @return int column margin
   */
  public int getColumnMargin()
  {
    return columnMargin;
  }

  /**
   * getColumnIndexAtX returns column that contains specified x-coordinate.
   * @param x x-coordinate that column should contain
   * @return int index of the column that contains specified x-coordinate relative
   * to this column model
   */
  public int getColumnIndexAtX(int x)
  {    
    for (int i = 0; i < tableColumns.size(); ++i)
      {
        int w = ((TableColumn)tableColumns.get(i)).getWidth();
        if (0 <= x && x < w)
          return i;
        else
          x -= w;
      }
    return -1;
  }

  /**
   * getTotalColumnWidth returns total width of all the columns including
   * column's margins.
   *
   * @return total width of all the columns
   */
  public int getTotalColumnWidth()
  {
    if (totalColumnWidth == -1)
      recalcWidthCache();
    return totalColumnWidth;
  }

  /**
   * setSelectionModel sets selection model that will be used by this ColumnTableModel
   * to keep track of currently selected columns
   *
   * @param model new selection model
   * @exception IllegalArgumentException if model is null
   */
  public void setSelectionModel(ListSelectionModel model)
  {
    if (model == null)
      throw new IllegalArgumentException();
    
    selectionModel = model;
    selectionModel.addListSelectionListener(this);
  }

  /**
   * getSelectionModel returns selection model
   * @return ListSelectionModel selection model
   */
  public ListSelectionModel getSelectionModel()
  {
    return selectionModel;
  }

  /**
   * setColumnSelectionAllowed sets whether column selection is allowed
   * or not.
   *
   * @param flag true if column selection is allowed and false otherwise
   */
  public void setColumnSelectionAllowed(boolean flag)
  {
    columnSelectionAllowed = flag;
  }

  /**
   * getColumnSelectionAllowed indicates whether column selection is 
   * allowed or not.
   *
   * @return boolean true if column selection is allowed and false otherwise.
   */
  public boolean getColumnSelectionAllowed()
  {
    return columnSelectionAllowed;
  }

  /**
   * getSelectedColumns returns array containing indexes of currently 
   * selected columns
   *
   * @return int[] array containing indexes of currently selected columns
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
   * getSelectedColumnCount returns number of currently selected columns
   * @return int number of currently selected columns
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
   * addColumnModelListener adds specified listener to the model's
   * listener list
   *
   * @param listener the listener to add
   */
  public void addColumnModelListener(TableColumnModelListener listener)
  {
    listenerList.add(TableColumnModelListener.class, listener);
  }

  /**
   * removeColumnModelListener removes specified listener from the model's 
   * listener list.
   *
   * @param listener the listener to remove
   */
  public void removeColumnModelListener(TableColumnModelListener listener)
  {
    listenerList.remove(TableColumnModelListener.class, listener);
  }

  /**
   * @since 1.4
   */
  public TableColumnModelListener[] getColumnModelListeners()
  {
    return (TableColumnModelListener[])
      listenerList.getListeners(TableColumnModelListener.class);
  }	  

  /**
   * fireColumnAdded fires TableColumnModelEvent to registered 
   * TableColumnModelListeners to indicate that column was added
   *
   * @param e TableColumnModelEvent
   */
  protected void fireColumnAdded(TableColumnModelEvent e)
  {    
    TableColumnModelListener[] listeners = getColumnModelListeners();

    for (int i=0; i< listeners.length; i++)
      listeners[i].columnAdded(e);        
  }

  /**
   * fireColumnAdded fires TableColumnModelEvent to registered 
   * TableColumnModelListeners to indicate that column was removed
   *
   * @param e TableColumnModelEvent
   */
  protected void fireColumnRemoved(TableColumnModelEvent e)
  {
    TableColumnModelListener[] listeners = getColumnModelListeners();

    for (int i=0; i< listeners.length; i++)
      listeners[i].columnRemoved(e);        
  }

  /**
   * fireColumnAdded fires TableColumnModelEvent to registered 
   * TableColumnModelListeners to indicate that column was moved
   *
   * @param e TableColumnModelEvent
   */
  protected void fireColumnMoved(TableColumnModelEvent e)
  {
    TableColumnModelListener[] listeners = getColumnModelListeners();

    for (int i=0; i< listeners.length; i++)
      listeners[i].columnMoved(e);        
  }

  /**
   * fireColumnSelectionChanged fires TableColumnModelEvent to model's
   * registered TableColumnModelListeners to indicate that different column 
   * was selected.
   *
   * @param evt ListSelectionEvent
   */
  protected void fireColumnSelectionChanged(ListSelectionEvent evt)
  {
    EventListener [] listeners = getListeners(TableColumnModelListener.class);
    for (int i = 0; i < listeners.length; ++i)
      ((TableColumnModelListener)listeners[i]).columnSelectionChanged(evt);
  }

  /**
   * fireColumnMarginChanged fires TableColumnModelEvent to model's
   * registered TableColumnModelListeners to indicate that column margin
   * was changed.
   */
  protected void fireColumnMarginChanged()
  {
    EventListener [] listeners = getListeners(TableColumnModelListener.class);
    for (int i = 0; i < listeners.length; ++i)
      ((TableColumnModelListener)listeners[i]).columnMarginChanged(changeEvent);
  }

  /**
   * getListeners returns currently registered listeners with this model.
   * @param listenerType type of listeners to return
   *
   * @return EventListener[] array of model's listeners of the specified type
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * propertyChange handles changes occuring in the properties of the
   * model's columns. 
   *
   * @param evt PropertyChangeEvent
   */
  public void propertyChange(PropertyChangeEvent evt)
  {
    if (evt.getPropertyName().equals(TableColumn.COLUMN_WIDTH_PROPERTY))
	invalidateWidthCache(); 
  }

  /**
   * valueChanged handles changes in the selectionModel.
   * @param e ListSelectionEvent
   */
  public void valueChanged(ListSelectionEvent e)
  {
    fireColumnSelectionChanged(e);
  }

  /**
   * createSelectionModel creates selection model that will keep track
   * of currently selected column(s)
   *
   * @return ListSelectionModel selection model of the columns
   */
  protected ListSelectionModel createSelectionModel()
  {    
    return new DefaultListSelectionModel();
  }

  /**
   * recalcWidthCache calculates total width of the columns.
   * If the current cache of the total width is in invalidated state, 
   * then width is recalculated. Otherwise nothing is done.
   */
  protected void recalcWidthCache()
  {
    if (totalColumnWidth == -1)
      {
        totalColumnWidth = 0;
        for (int i = 0; i < tableColumns.size(); ++i)
          {
            totalColumnWidth += ((TableColumn)tableColumns.get(i)).getWidth();
          }
      }
  }

  /**
   * invalidateWidthCache
   */
  private void invalidateWidthCache()
  {
    totalColumnWidth = -1;
  }
}
