/* DefaultTreeSelectionModel.java --
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


package javax.swing.tree;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.EventListener;
import java.util.Vector;

import javax.swing.DefaultListSelectionModel;
import javax.swing.event.EventListenerList;
import javax.swing.event.SwingPropertyChangeSupport;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

/**
 * DefaultTreeSelectionModel
 * @author Andrew Selkirk
 */
public class DefaultTreeSelectionModel
  implements Cloneable, Serializable, TreeSelectionModel
{
  static final long serialVersionUID = 3288129636638950196L;

  /**
   * SELECTION_MODE_PROPERTY
   */
  public static final String SELECTION_MODE_PROPERTY = "selectionMode";

  /**
   * changeSupport
   */
  protected SwingPropertyChangeSupport changeSupport;

  /**
   * selection
   */
  protected TreePath[] selection;

  /**
   * listenerList
   */
  protected EventListenerList listenerList;

  /**
   * rowMapper
   */
  protected transient RowMapper rowMapper;

  /**
   * listSelectionModel
   */
  protected DefaultListSelectionModel listSelectionModel;

  /**
   * selectionMode
   */
  protected int selectionMode;

  /**
   * leadPath
   */
  protected TreePath leadPath;

  /**
   * leadIndex
   */
  protected int leadIndex;

  /**
   * leadRow
   */
  protected int leadRow;

  /**
   * Constructor DefaultTreeSelectionModel
   */
  public DefaultTreeSelectionModel()
  {
    // TODO
  }

  /**
   * clone
   * @exception CloneNotSupportedException TODO
   * @return Object
   */
  public Object clone() throws CloneNotSupportedException
  {
    return null; // TODO
  }

  /**
   * toString
   * @return String
   */
  public String toString()
  {
    return null; // TODO
  }

  /**
   * writeObject
   * @param value0 TODO
   * @exception IOException TODO
   */
  private void writeObject(ObjectOutputStream value0) throws IOException
  {
    // TODO
  }

  /**
   * readObject
   * @param value0 TODO
   * @exception IOException TODO
   * @exception ClassNotFoundException TODO
   */
  private void readObject(ObjectInputStream value0)
    throws IOException, ClassNotFoundException
  {
    // TODO
  }

  /**
   * setRowMapper
   * @param value0 TODO
   */
  public void setRowMapper(RowMapper value0)
  {
    // TODO
  }

  /**
   * getRowMapper
   * @return RowMapper
   */
  public RowMapper getRowMapper()
  {
    return null; // TODO
  }

  /**
   * setSelectionMode
   * @param value0 TODO
   */
  public void setSelectionMode(int value0)
  {
    // TODO
  }

  /**
   * getSelectionMode
   * @return int
   */
  public int getSelectionMode()
  {
    return 0; // TODO
  }

  /**
   * setSelectionPath
   * @param value0 TODO
   */
  public void setSelectionPath(TreePath value0)
  {
    // TODO
  }

  /**
   * setSelectionPaths
   * @param value0 TODO
   */
  public void setSelectionPaths(TreePath[] value0)
  {
    // TODO
  }

  /**
   * addSelectionPath
   * @param value0 TODO
   */
  public void addSelectionPath(TreePath value0)
  {
    // TODO
  }

  /**
   * addSelectionPaths
   * @param value0 TODO
   */
  public void addSelectionPaths(TreePath[] value0)
  {
    // TODO
  }

  /**
   * removeSelectionPath
   * @param value0 TODO
   */
  public void removeSelectionPath(TreePath value0)
  {
    // TODO
  }

  /**
   * removeSelectionPaths
   * @param value0 TODO
   */
  public void removeSelectionPaths(TreePath[] value0)
  {
    // TODO
  }

  /**
   * getSelectionPath
   * @return TreePath
   */
  public TreePath getSelectionPath()
  {
    return null; // TODO
  }

  /**
   * getSelectionPaths
   * @return TreePath[]
   */
  public TreePath[] getSelectionPaths()
  {
    return null; // TODO
  }

  /**
   * getSelectionCount
   * @return int
   */
  public int getSelectionCount()
  {
    return 0; // TODO
  }

  /**
   * isPathSelected
   * @param value0 TODO
   * @return boolean
   */
  public boolean isPathSelected(TreePath value0)
  {
    return false; // TODO
  }

  /**
   * isSelectionEmpty
   * @return boolean
   */
  public boolean isSelectionEmpty()
  {
    return false; // TODO
  }

  /**
   * clearSelection
   */
  public void clearSelection()
  {
    // TODO
  }

  /**
   * Adds a <code>TreeSelectionListener</code> object to this model.
   *
   * @param listener the listener to add
   */
  public void addTreeSelectionListener(TreeSelectionListener listener)
  {
    listenerList.add(TreeSelectionListener.class, listener);
  }

  /**
   * Removes a <code>TreeSelectionListener</code> object from this model.
   *
   * @param listener the listener to remove
   */
  public void removeTreeSelectionListener(TreeSelectionListener listener)
  {
    listenerList.remove(TreeSelectionListener.class, listener);
  }

  /**
   * Returns all <code>TreeSelectionListener</code> added to this model.
   *
   * @return an array of listeners
   *
   * @since 1.4
   */
  public TreeSelectionListener[] getTreeSelectionListeners()
  {
    return (TreeSelectionListener[]) listenerList.getListeners(TreeSelectionListener.class);
  }

  /**
   * fireValueChanged
   *
   * @param event the event to fire.
   */
  protected void fireValueChanged(TreeSelectionEvent event)
  {
    TreeSelectionListener[] listeners = getTreeSelectionListeners();

    for (int i = listeners.length - 1; i >= 0; --i)
      listeners[i].valueChanged(event);
  }

  /**
   * Returns all added listeners of a special type.
   *
   * @param listenerType the listener type
   *
   * @return an array of listeners
   *
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }

  /**
   * getSelectionRows
   * @return int[]
   */
  public int[] getSelectionRows()
  {
    return null; // TODO
  }

  /**
   * getMinSelectionRow
   * @return int
   */
  public int getMinSelectionRow()
  {
    return 0; // TODO
  }

  /**
   * getMaxSelectionRow
   * @return int
   */
  public int getMaxSelectionRow()
  {
    return 0; // TODO
  }

  /**
   * isRowSelected
   * @param value0 TODO
   * @return boolean
   */
  public boolean isRowSelected(int value0)
  {
    return false; // TODO
  }

  /**
   * resetRowSelection
   */
  public void resetRowSelection()
  {
    // TODO
  }

  /**
   * getLeadSelectionRow
   * @return int
   */
  public int getLeadSelectionRow()
  {
    return 0; // TODO
  }

  /**
   * getLeadSelectionPath
   * @return TreePath
   */
  public TreePath getLeadSelectionPath()
  {
    return null; // TODO
  }

  /**
   * Adds a <code>PropertyChangeListener</code> object to this model.
   *
   * @param listener the listener to add.
   */
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Removes a <code>PropertyChangeListener</code> object from this model.
   *
   * @param listener the listener to remove.
   */
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns all added <code>PropertyChangeListener</code> objects.
   *
   * @return an array of listeners.
   *
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return changeSupport.getPropertyChangeListeners();
  }

  /**
   * insureRowContinuity
   */
  protected void insureRowContinuity()
  {
    // TODO
  }

  /**
   * arePathsContiguous
   * @param value0 TODO
   * @return boolean
   */
  protected boolean arePathsContiguous(TreePath[] value0)
  {
    return false; // TODO
  }

  /**
   * canPathsBeAdded
   * @param value0 TODO
   * @return boolean
   */
  protected boolean canPathsBeAdded(TreePath[] value0)
  {
    return false; // TODO
  }

  /**
   * canPathsBeRemoved
   * @param value0 TODO
   * @return boolean
   */
  protected boolean canPathsBeRemoved(TreePath[] value0)
  {
    return false; // TODO
  }

  /**
   * notifyPathChange
   * @param value0 TODO
   * @param value1 TODO
   */
  protected void notifyPathChange(Vector value0, TreePath value1)
  {
    // TODO
  }

  /**
   * updateLeadIndex
   */
  protected void updateLeadIndex()
  {
    // TODO
  }

  /**
   * insureUniqueness
   */
  protected void insureUniqueness()
  {
    // TODO
  }
}
