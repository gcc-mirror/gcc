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
   * Our Swing property change support.
   */
  protected SwingPropertyChangeSupport changeSupport;

  /**
   * The current selection.
   */
  protected TreePath[] selection;

  /**
   * Our TreeSelectionListeners.
   */
  protected EventListenerList listenerList;

  /**
   * The current RowMapper.
   */
  protected transient RowMapper rowMapper;

  /**
   * The current listSelectionModel.
   */
  protected DefaultListSelectionModel listSelectionModel;

  /**
   * The current selection mode.
   */
  protected int selectionMode;

  /**
   * The path that has been added last.
   */
  protected TreePath leadPath;

  /**
   * The index of the last added path.
   */
  protected int leadIndex;

  /**
   * The row of the last added path according to the RowMapper.
   */
  protected int leadRow;

  /**
   * Constructs a new DefaultTreeSelectionModel.
   */
  public DefaultTreeSelectionModel()
  {
    // TODO
  }

  /**
   * Creates a clone of this DefaultTreeSelectionModel with the same
   * selection.
   *
   * @exception CloneNotSupportedException should not be thrown here
   *
   * @return a clone of this DefaultTreeSelectionModel
   */
  public Object clone() throws CloneNotSupportedException
  {
    return null; // TODO
  }

  /**
   * Returns a string that shows this object's properties.
   *
   * @return a string that shows this object's properties
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
   * Sets the RowMapper that should be used to map between paths and their
   * rows.
   *
   * @param rowMapper the RowMapper to set
   *
   * @see {@link RowMapper
   */
  public void setRowMapper(RowMapper value0)
  {
    // TODO
  }

  /**
   * Returns the RowMapper that is currently used to map between paths and
   * their rows.
   *
   * @return the current RowMapper
   *
   * @see {@link RowMapper
   */
  public RowMapper getRowMapper()
  {
    return null; // TODO
  }

  /**
   * Sets the current selection mode. Possible values are
   * {@link #SINGLE_TREE_SELECTION}, {@link CONTIGUOUS_TREE_SELECTION}
   * and {@link #DISCONTIGUOUS_TREE_SELECTION}.
   *
   * @param mode the selection mode to be set
   *
   * @see {@link #getSelectionMode}
   * @see {@link #SINGLE_TREE_SELECTION}
   * @see {@link #CONTIGUOUS_TREE_SELECTION}
   * @see {@link #DISCONTIGUOUS_TREE_SELECTION}
   */
  public void setSelectionMode(int value0)
  {
    // TODO
  }

  /**
   * Returns the current selection mode.
   *
   * @return the current selection mode
   *
   * @see {@link #setSelectionMode}
   * @see {@link #SINGLE_TREE_SELECTION}
   * @see {@link #CONTIGUOUS_TREE_SELECTION}
   * @see {@link #DISCONTIGUOUS_TREE_SELECTION}
   */
  public int getSelectionMode()
  {
    return 0; // TODO
  }

  /**
   * Sets this path as the only selection.
   *
   * If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param path the path to set as selection
   */
  public void setSelectionPath(TreePath value0)
  {
    // TODO
  }

  /**
   * Sets the paths as selection. This method checks for duplicates and
   * removes them.
   *
   * If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param paths the paths to set as selection
   */
  public void setSelectionPaths(TreePath[] value0)
  {
    // TODO
  }

  /**
   * Adds a path to the list of selected paths. This method checks if the
   * path is already selected and doesn't add the same path twice.
   *
   * If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param path the path to add to the selection
   */
  public void addSelectionPath(TreePath value0)
  {
    // TODO
  }

  /**
   * Adds the paths to the list of selected paths. This method checks if the
   * paths are already selected and doesn't add the same path twice.
   *
   * If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param paths the paths to add to the selection
   */
  public void addSelectionPaths(TreePath[] value0)
  {
    // TODO
  }

  /**
   * Removes the path from the selection.
   *
   * If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param path the path to remove
   */
  public void removeSelectionPath(TreePath value0)
  {
    // TODO
  }

  /**
   * Removes the paths from the selection.
   *
   * If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param paths the path to remove
   */
  public void removeSelectionPaths(TreePath[] value0)
  {
    // TODO
  }

  /**
   * Returns the first path in the selection. This is especially useful
   * when the selectionMode is {@link #SINGLE_TREE_SELECTION}.
   *
   * @return the first path in the selection
   */
  public TreePath getSelectionPath()
  {
    return null; // TODO
  }

  /**
   * Returns the complete selection.
   *
   * @return the complete selection
   */
  public TreePath[] getSelectionPaths()
  {
    return null; // TODO
  }

  /**
   * Returns the number of paths in the selection.
   *
   * @return the number of paths in the selection
   */
  public int getSelectionCount()
  {
    return 0; // TODO
  }

  /**
   * Checks if a given path is in the selection.
   *
   * @param path the path to check
   *
   * @return <code>true</code> if the path is in the selection,
   *         <code>false</code> otherwise
   */
  public boolean isPathSelected(TreePath value0)
  {
    return false; // TODO
  }

  /**
   * Checks if the selection is empty.
   *
   * @return <code>true</code> if the selection is empty,
   *         <code>false</code> otherwise
   */
  public boolean isSelectionEmpty()
  {
    return false; // TODO
  }

  /**
   * Removes all paths from the selection.
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
   * Returns the currently selected rows.
   *
   * @return the currently selected rows
   */
  public int[] getSelectionRows()
  {
    return null; // TODO
  }

  /**
   * Returns the smallest row index from the selection.
   *
   * @return the smallest row index from the selection
   */
  public int getMinSelectionRow()
  {
    return 0; // TODO
  }

  /**
   * Returns the largest row index from the selection.
   *
   * @return the largest row index from the selection
   */
  public int getMaxSelectionRow()
  {
    return 0; // TODO
  }

  /**
   * Checks if a particular row is selected.
   *
   * @param row the index of the row to check
   *
   * @return <code>true</code> if the row is in this selection,
   *         <code>false</code> otherwise
   */
  public boolean isRowSelected(int value0)
  {
    return false; // TODO
  }

  /**
   * Updates the mappings from TreePaths to row indices.
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
   * Makes sure the currently selected paths are valid according to the
   * current selectionMode.
   *
   * If the selectionMode is set to {@link CONTIGUOUS_TREE_SELECTION}
   * and the selection isn't contiguous then the selection is reset to
   * the first set of contguous paths.
   *
   * If the selectionMode is set to {@link SINGLE_TREE_SELECTION}
   * and the selection has more than one path, the selection is reset to
   * the contain only the first path.
   */
  protected void insureRowContinuity()
  {
    // TODO
  }

  /**
   * Returns <code>true</code> if the paths are contiguous or we
   * have no RowMapper assigned.
   *
   * @param paths the paths to check for continuity
   * @return <code>true</code> if the paths are contiguous or we
   *         have no RowMapper assigned
   */
  protected boolean arePathsContiguous(TreePath[] value0)
  {
    return false; // TODO
  }

  /**
   * Checks if the paths can be added. This returns <code>true</code> if:
   * <ul>
   * <li><code>paths</code> is <code>null</code> or empty</li>
   * <li>we have no RowMapper assigned</li>
   * <li>nothing is currently selected</li>
   * <li>selectionMode is {@link DISCONTIGUOUS_TREE_SELECTION</li>
   * <li>adding the paths to the selection still results in a contiguous set
   *   of paths</li>
   *
   * @param paths the paths to check
   *
   * @return <code>true</code> if the paths can be added with respect to the
   *         selectionMode
   */
  protected boolean canPathsBeAdded(TreePath[] value0)
  {
    return false; // TODO
  }

  /**
   * Checks if the paths can be removed without breaking the continuity of
   * the selection according to selectionMode.
   *
   * @param paths the paths to check
   * @return  <code>true</code> if the paths can be removed with respect to the
   *         selectionMode
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
   * Updates the lead index instance field.
   */
  protected void updateLeadIndex()
  {
    // TODO
  }

  /**
   * Deprecated and not used.
   */
  protected void insureUniqueness()
  {
    // TODO
  }
}
