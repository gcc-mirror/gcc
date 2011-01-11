/* DefaultTreeSelectionModel.java
   Copyright (C) 2002, 2004, 2005, 2006 Free Software Foundation, Inc.

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


package javax.swing.tree;

import gnu.java.lang.CPStringBuilder;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Arrays;
import java.util.BitSet;
import java.util.EventListener;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.DefaultListSelectionModel;
import javax.swing.event.EventListenerList;
import javax.swing.event.SwingPropertyChangeSupport;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;

/**
 * The implementation of the default tree selection model. The installed
 * listeners are notified about the path and not the row changes. If you
 * specifically need to track the row changes, register the listener for the
 * expansion events.
 *
 * @author Andrew Selkirk
 * @author Audrius Meskauskas
 */
public class DefaultTreeSelectionModel
    implements Cloneable, Serializable, TreeSelectionModel
{

  /**
   * According to the API docs, the method
   * {@link DefaultTreeSelectionModel#notifyPathChange} should
   * expect instances of a class PathPlaceHolder in the Vector parameter.
   * This seems to be a non-public class, so I can only make guesses about the
   * use of it.
   */
  private static class PathPlaceHolder
  {
    /**
     * The path that we wrap.
     */
    TreePath path;

    /**
     * Indicates if the path is new or already in the selection.
     */
    boolean isNew;

    /**
     * Creates a new instance.
     *
     * @param p the path to wrap
     * @param n if the path is new or already in the selection
     */
    PathPlaceHolder(TreePath p, boolean n)
    {
      path = p;
      isNew = n;
    }
  }

  /**
   * Use serialVersionUID for interoperability.
   */
  static final long serialVersionUID = 3288129636638950196L;

  /**
   * The name of the selection mode property.
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
  protected int leadRow = -1;

  /**
   * A supporting datastructure that is used in addSelectionPaths() and
   * removeSelectionPaths(). It contains currently selected paths.
   *
   * @see #addSelectionPaths(TreePath[])
   * @see #removeSelectionPaths(TreePath[])
   * @see #setSelectionPaths(TreePath[])
   */
  private transient HashSet<TreePath> selectedPaths;

  /**
   * A supporting datastructure that is used in addSelectionPaths() and
   * removeSelectionPaths(). It contains the paths that are added or removed.
   *
   * @see #addSelectionPaths(TreePath[])
   * @see #removeSelectionPaths(TreePath[])
   * @see #setSelectionPaths(TreePath[])
   */
  private transient HashSet<TreePath> tmpPaths;

  /**
   * Constructs a new DefaultTreeSelectionModel.
   */
  public DefaultTreeSelectionModel()
  {
    setSelectionMode(DISCONTIGUOUS_TREE_SELECTION);
    listSelectionModel = new DefaultListSelectionModel();
    listenerList = new EventListenerList();
    leadIndex = -1;
    tmpPaths = new HashSet<TreePath>();
    selectedPaths = new HashSet<TreePath>();
  }

  /**
   * Creates a clone of this DefaultTreeSelectionModel with the same selection.
   * The cloned instance will have the same registered listeners, the listeners
   * themselves will not be cloned. The selection will be cloned.
   *
   * @exception CloneNotSupportedException should not be thrown here
   * @return a copy of this DefaultTreeSelectionModel
   */
  public Object clone() throws CloneNotSupportedException
  {
    DefaultTreeSelectionModel cloned =
      (DefaultTreeSelectionModel) super.clone();
    cloned.changeSupport = null;
    cloned.selection = (TreePath[]) selection.clone();
    cloned.listenerList = new EventListenerList();
    cloned.listSelectionModel =
      (DefaultListSelectionModel) listSelectionModel.clone();
    cloned.selectedPaths = new HashSet<TreePath>();
    cloned.tmpPaths = new HashSet<TreePath>();

    return cloned;
  }

  /**
   * Returns a string that shows this object's properties.
   * The returned string lists the selected tree rows, if any.
   *
   * @return a string that shows this object's properties
   */
  public String toString()
  {
    if (isSelectionEmpty())
      return "[selection empty]";
    else
      {
        CPStringBuilder b = new CPStringBuilder("selected rows: [");
        for (int i = 0; i < selection.length; i++)
          {
            b.append(getRow(selection[i]));
            b.append(' ');
          }
        b.append(", lead " + getLeadSelectionRow());
        return b.toString();
      }
  }

  /**
   * writeObject
   *
   * @param value0 TODO
   * @exception IOException TODO
   */
  private void writeObject(ObjectOutputStream value0) throws IOException
  {
    // TODO
  }

  /**
   * readObject
   *
   * @param value0 TODO
   * @exception IOException TODO
   * @exception ClassNotFoundException TODO
   */
  private void readObject(ObjectInputStream value0) throws IOException,
      ClassNotFoundException
  {
    // TODO
  }

  /**
   * Sets the RowMapper that should be used to map between paths and their rows.
   *
   * @param mapper the RowMapper to set
   * @see RowMapper
   */
  public void setRowMapper(RowMapper mapper)
  {
    rowMapper = mapper;
    resetRowSelection();
  }

  /**
   * Returns the RowMapper that is currently used to map between paths and their
   * rows.
   *
   * @return the current RowMapper
   * @see RowMapper
   */
  public RowMapper getRowMapper()
  {
    return rowMapper;
  }

  /**
   * Sets the current selection mode. Possible values are
   * {@link #SINGLE_TREE_SELECTION}, {@link #CONTIGUOUS_TREE_SELECTION} and
   * {@link #DISCONTIGUOUS_TREE_SELECTION}.
   *
   * @param mode the selection mode to be set
   * @see #getSelectionMode
   * @see #SINGLE_TREE_SELECTION
   * @see #CONTIGUOUS_TREE_SELECTION
   * @see #DISCONTIGUOUS_TREE_SELECTION
   */
  public void setSelectionMode(int mode)
  {
    int oldMode = selectionMode;
    selectionMode = mode;
    // Make sure we have a valid selection mode.
    if (selectionMode != SINGLE_TREE_SELECTION
        && selectionMode != CONTIGUOUS_TREE_SELECTION
        && selectionMode != DISCONTIGUOUS_TREE_SELECTION)
      selectionMode = DISCONTIGUOUS_TREE_SELECTION;

    // Fire property change event.
    if (oldMode != selectionMode && changeSupport != null)
      changeSupport.firePropertyChange(SELECTION_MODE_PROPERTY, oldMode,
                                       selectionMode);
  }

  /**
   * Returns the current selection mode.
   *
   * @return the current selection mode
   * @see #setSelectionMode
   * @see #SINGLE_TREE_SELECTION
   * @see #CONTIGUOUS_TREE_SELECTION
   * @see #DISCONTIGUOUS_TREE_SELECTION
   */
  public int getSelectionMode()
  {
    return selectionMode;
  }

  /**
   * Sets this path as the only selection. If this changes the selection the
   * registered TreeSelectionListeners are notified.
   *
   * @param path the path to set as selection
   */
  public void setSelectionPath(TreePath path)
  {
    TreePath[] paths = null;
    if (path != null)
      paths = new TreePath[]{ path };
    setSelectionPaths(paths);
  }

  /**
   * Get the number of the tree row for the given path.
   *
   * @param path the tree path
   * @return the tree row for this path or -1 if the path is not visible.
   */
  int getRow(TreePath path)
  {
    RowMapper mapper = getRowMapper();

    if (mapper instanceof AbstractLayoutCache)
      {
        // The absolute majority of cases, unless the TreeUI is very
        // seriously rewritten
        AbstractLayoutCache ama = (AbstractLayoutCache) mapper;
        return ama.getRowForPath(path);
      }
    else if (mapper != null)
      {
        // Generic non optimized implementation.
        int[] rows = mapper.getRowsForPaths(new TreePath[] { path });
        if (rows.length == 0)
          return - 1;
        else
          return rows[0];
      }
    return -1;
  }

  /**
   * Sets the paths as selection. This method checks for duplicates and removes
   * them. If this changes the selection the registered TreeSelectionListeners
   * are notified.
   *
   * @param paths the paths to set as selection
   */
  public void setSelectionPaths(TreePath[] paths)
  {
    int oldLength = 0;
    if (selection != null)
      oldLength = selection.length;
    int newLength = 0;
    if (paths != null)
      newLength = paths.length;
    if (newLength > 0 || oldLength > 0)
      {
        // For SINGLE_TREE_SELECTION and for CONTIGUOUS_TREE_SELECTION with
        // a non-contiguous path, we only allow the first path element.
        if ((selectionMode == SINGLE_TREE_SELECTION && newLength > 1)
            || (selectionMode == CONTIGUOUS_TREE_SELECTION && newLength > 0
                && ! arePathsContiguous(paths)))
          {
            paths = new TreePath[] { paths[0] };
            newLength = 1;
          }
        // Find new paths.
        Vector<PathPlaceHolder> changedPaths = null;
        tmpPaths.clear();
        int validPaths = 0;
        TreePath oldLeadPath = leadPath;
        for (int i = 0; i < newLength; i++)
          {
            if (paths[i] != null && ! tmpPaths.contains(paths[i]))
              {
                validPaths++;
                tmpPaths.add(paths[i]);
                if (! selectedPaths.contains(paths[i]))
                  {
                    if (changedPaths == null)
                      changedPaths = new Vector<PathPlaceHolder>();
                    changedPaths.add(new PathPlaceHolder(paths[i], true));
                  }
                leadPath = paths[i];
              }
          }
        // Put together the new selection.
        TreePath[] newSelection = null;
        if (validPaths != 0)
          {
            if (validPaths != newLength)
              {
                // Some of the paths are already selected, put together
                // the new selection carefully.
                newSelection = new TreePath[validPaths];
                Iterator<TreePath> newPaths = tmpPaths.iterator();
                validPaths = 0;
                for (int i = 0; newPaths.hasNext(); i++)
                  newSelection[i] = newPaths.next();
              }
            else
              {
                newSelection = new TreePath[paths.length];
                System.arraycopy(paths, 0, newSelection, 0, paths.length);
              }
          }

        // Find paths that have been selected, but are no more.
        for (int i = 0; i < oldLength; i++)
          {
            if (selection[i] != null && ! tmpPaths.contains(selection[i]))
              {
                if (changedPaths == null)
                  changedPaths = new Vector<PathPlaceHolder>();
                changedPaths.add(new PathPlaceHolder(selection[i], false));
              }
          }

        // Perform changes and notification.
        selection = newSelection;
        HashSet<TreePath> tmp = selectedPaths;
        selectedPaths = tmpPaths;
        tmpPaths = tmp;
        tmpPaths.clear();

        // Not necessary, but required according to the specs and to tests.
        if (selection != null)
          insureUniqueness();
        updateLeadIndex();
        resetRowSelection();
        if (changedPaths != null && changedPaths.size() > 0)
          notifyPathChange(changedPaths, oldLeadPath);
      }
  }

  /**
   * Adds a path to the list of selected paths. This method checks if the path
   * is already selected and doesn't add the same path twice. If this changes
   * the selection the registered TreeSelectionListeners are notified.
   *
   * The lead path is changed to the added path. This also happen if the
   * passed path was already selected before.
   *
   * @param path the path to add to the selection
   */
  public void addSelectionPath(TreePath path)
  {
    if (path != null)
      {
        TreePath[] add = new TreePath[]{ path };
        addSelectionPaths(add);
      }
  }

  /**
   * Adds the paths to the list of selected paths. This method checks if the
   * paths are already selected and doesn't add the same path twice. If this
   * changes the selection the registered TreeSelectionListeners are notified.
   *
   * @param paths the paths to add to the selection
   */
  public void addSelectionPaths(TreePath[] paths)
  {
    int length = paths != null ? paths.length : 0;
    if (length > 0)
      {
        if (selectionMode == SINGLE_TREE_SELECTION)
          setSelectionPaths(paths);
        else if (selectionMode == CONTIGUOUS_TREE_SELECTION
                 &&  ! canPathsBeAdded(paths))
          {
            if (arePathsContiguous(paths))
              setSelectionPaths(paths);
            else
              setSelectionPaths(new TreePath[] { paths[0] });
          }
        else
          {
            Vector<PathPlaceHolder> changedPaths = null;
            tmpPaths.clear();
            int validPaths = 0;
            TreePath oldLeadPath = leadPath;
            int oldPaths = 0;
            if (selection != null)
              oldPaths = selection.length;
            int i;
            for (i = 0; i < length; i++)
              {
                if (paths[i] != null)
                  {
                    if (! selectedPaths.contains(paths[i]))
                      {
                        validPaths++;
                        if (changedPaths == null)
                          changedPaths = new Vector<PathPlaceHolder>();
                        changedPaths.add(new PathPlaceHolder(paths[i], true));
                        selectedPaths.add(paths[i]);
                        tmpPaths.add(paths[i]);
                      }
                    leadPath = paths[i];
                  }
              }
            if (validPaths > 0)
              {
                TreePath[] newSelection = new TreePath[oldPaths + validPaths];
                if (oldPaths > 0)
                  System.arraycopy(selection, 0, newSelection, 0, oldPaths);
                if (validPaths != paths.length)
                  {
                    // Some of the paths are already selected, put together
                    // the new selection carefully.
                    Iterator<TreePath> newPaths = tmpPaths.iterator();
                    i = oldPaths;
                    while (newPaths.hasNext())
                      {
                        newSelection[i] = newPaths.next();
                        i++;
                      }
                  }
                else
                  System.arraycopy(paths, 0, newSelection, oldPaths,
                                   validPaths);
                selection = newSelection;
                insureUniqueness();
                updateLeadIndex();
                resetRowSelection();
                if (changedPaths != null && changedPaths.size() > 0)
                  notifyPathChange(changedPaths, oldLeadPath);
              }
            else
              leadPath = oldLeadPath;
            tmpPaths.clear();
          }
      }
  }

  /**
   * Removes the path from the selection. If this changes the selection the
   * registered TreeSelectionListeners are notified.
   *
   * @param path the path to remove
   */
  public void removeSelectionPath(TreePath path)
  {
    if (path != null)
      removeSelectionPaths(new TreePath[]{ path });
  }

  /**
   * Removes the paths from the selection. If this changes the selection the
   * registered TreeSelectionListeners are notified.
   *
   * @param paths the paths to remove
   */
  public void removeSelectionPaths(TreePath[] paths)
  {
    if (paths != null && selection != null && paths.length > 0)
      {
        if (! canPathsBeRemoved(paths))
          clearSelection();
        else
          {
            Vector<PathPlaceHolder> pathsToRemove = null;
            for (int i = paths.length - 1; i >= 0; i--)
              {
                if (paths[i] != null && selectedPaths.contains(paths[i]))
                  {
                    if (pathsToRemove == null)
                      pathsToRemove = new Vector<PathPlaceHolder>();
                    selectedPaths.remove(paths[i]);
                    pathsToRemove.add(new PathPlaceHolder(paths[i],
                                                          false));
                  }
              }
            if (pathsToRemove != null)
              {
                int numRemove = pathsToRemove.size();
                TreePath oldLead = leadPath;
                if (numRemove == selection.length)
                  selection = null;
                else
                  {
                    selection = new TreePath[selection.length - numRemove];
                    Iterator<TreePath> keep = selectedPaths.iterator();
                    for (int valid = 0; keep.hasNext(); valid++)
                      selection[valid] = keep.next();
                  }
                // Update lead path.
                if (leadPath != null && ! selectedPaths.contains(leadPath))
                  {
                    if (selection != null)
                      leadPath = selection[selection.length - 1];
                    else
                      leadPath = null;
                  }
                else if (selection != null)
                  leadPath = selection[selection.length - 1];
                else
                  leadPath = null;
                updateLeadIndex();
                resetRowSelection();
                notifyPathChange(pathsToRemove, oldLead);
              }
          }
      }
  }

  /**
   * Returns the first path in the selection. This is especially useful when the
   * selectionMode is {@link #SINGLE_TREE_SELECTION}.
   *
   * @return the first path in the selection
   */
  public TreePath getSelectionPath()
  {
    if ((selection == null) || (selection.length == 0))
      return null;
    else
      return selection[0];
  }

  /**
   * Returns the complete selection.
   *
   * @return the complete selection
   */
  public TreePath[] getSelectionPaths()
  {
    return selection;
  }

  /**
   * Returns the number of paths in the selection.
   *
   * @return the number of paths in the selection
   */
  public int getSelectionCount()
  {
    if (selection == null)
      return 0;
    else
      return selection.length;
  }

  /**
   * Checks if a given path is in the selection.
   *
   * @param path the path to check
   * @return <code>true</code> if the path is in the selection,
   *         <code>false</code> otherwise
   */
  public boolean isPathSelected(TreePath path)
  {
    if (selection == null)
      return false;

    for (int i = 0; i < selection.length; i++)
      {
        if (selection[i].equals(path))
          return true;
      }
    return false;
  }

  /**
   * Checks if the selection is empty.
   *
   * @return <code>true</code> if the selection is empty, <code>false</code>
   *         otherwise
   */
  public boolean isSelectionEmpty()
  {
    return (selection == null) || (selection.length == 0);
  }

  /**
   * Removes all paths from the selection. Fire the unselection event.
   */
  public void clearSelection()
  {
    if (selection != null)
      {
        int selectionLength = selection.length;
        boolean[] news = new boolean[selectionLength];
        Arrays.fill(news, false);
        TreeSelectionEvent event = new TreeSelectionEvent(this, selection,
                                                          news, leadPath,
                                                          null);
        leadPath = null;
        leadIndex = 0;
        leadRow = 0;
        selectedPaths.clear();
        selection = null;
        resetRowSelection();
        fireValueChanged(event);
      }
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
   * @since 1.4
   */
  public TreeSelectionListener[] getTreeSelectionListeners()
  {
    return (TreeSelectionListener[]) getListeners(TreeSelectionListener.class);
  }

  /**
   * fireValueChanged
   *
   * @param event the event to fire.
   */
  protected void fireValueChanged(TreeSelectionEvent event)
  {
    TreeSelectionListener[] listeners = getTreeSelectionListeners();

    for (int i = 0; i < listeners.length; ++i)
      listeners[i].valueChanged(event);
  }

  /**
   * Returns all added listeners of a special type.
   *
   * @param listenerType the listener type
   * @return an array of listeners
   * @since 1.3
   */
  public <T extends EventListener> T[] getListeners(Class<T> listenerType)
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
    int[] rows = null;
    if (rowMapper != null && selection != null)
      {
        rows = rowMapper.getRowsForPaths(selection);
        if (rows != null)
          {
            // Find invisible rows.
            int invisible = 0;
            for (int i = rows.length - 1; i >= 0; i--)
              {
                if (rows[i] == -1)
                  invisible++;

              }
            // Clean up invisible rows.
            if (invisible > 0)
              {
                if (invisible == rows.length)
                  rows = null;
                else
                  {
                    int[] newRows = new int[rows.length - invisible];
                    int visCount = 0;
                    for (int i = rows.length - 1; i >= 0; i--)
                      {
                        if (rows[i] != -1)
                          {
                            newRows[visCount] = rows[i];
                            visCount++;
                          }
                      }
                    rows = newRows;
                  }
              }
          }
      }
    return rows;
  }

  /**
   * Returns the smallest row index from the selection.
   *
   * @return the smallest row index from the selection
   */
  public int getMinSelectionRow()
  {
    return listSelectionModel.getMinSelectionIndex();
  }

  /**
   * Returns the largest row index from the selection.
   *
   * @return the largest row index from the selection
   */
  public int getMaxSelectionRow()
  {
    return listSelectionModel.getMaxSelectionIndex();
  }

  /**
   * Checks if a particular row is selected.
   *
   * @param row the index of the row to check
   * @return <code>true</code> if the row is in this selection,
   *         <code>false</code> otherwise
   * @throws NullPointerException if the row mapper is not set (can only happen
   *           if the user has plugged in the custom incorrect TreeUI
   *           implementation.
   */
  public boolean isRowSelected(int row)
  {
    return listSelectionModel.isSelectedIndex(row);
  }

  /**
   * Updates the mappings from TreePaths to row indices.
   */
  public void resetRowSelection()
  {
    listSelectionModel.clearSelection();
    if (selection != null && rowMapper != null)
      {
        int[] rows = rowMapper.getRowsForPaths(selection);
        // Update list selection model.
        for (int i = 0; i < rows.length; i++)
          {
            int row = rows[i];
            if (row != -1)
              listSelectionModel.addSelectionInterval(row, row);
          }
        // Update lead selection.
        if (leadIndex != -1 && rows != null)
          leadRow = rows[leadIndex];
        else if (leadPath != null)
          {
            TreePath[] tmp = new TreePath[]{ leadPath };
            rows = rowMapper.getRowsForPaths(tmp);
            leadRow = rows != null ? rows[0] : -1;
          }
        else
          leadRow = -1;
        insureRowContinuity();
      }
    else
      leadRow = -1;
  }

  /**
   * getLeadSelectionRow
   *
   * @return int
   */
  public int getLeadSelectionRow()
  {
    return leadRow;
  }

  /**
   * getLeadSelectionPath
   *
   * @return TreePath
   */
  public TreePath getLeadSelectionPath()
  {
    return leadPath;
  }

  /**
   * Adds a <code>PropertyChangeListener</code> object to this model.
   *
   * @param listener the listener to add.
   */
  public void addPropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport == null)
      changeSupport = new SwingPropertyChangeSupport(this);
    changeSupport.addPropertyChangeListener(listener);
  }

  /**
   * Removes a <code>PropertyChangeListener</code> object from this model.
   *
   * @param listener the listener to remove.
   */
  public void removePropertyChangeListener(PropertyChangeListener listener)
  {
    if (changeSupport != null)
      changeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Returns all added <code>PropertyChangeListener</code> objects.
   *
   * @return an array of listeners.
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    PropertyChangeListener[] listeners = null;
    if (changeSupport != null)
      listeners = changeSupport.getPropertyChangeListeners();
    else
      listeners = new PropertyChangeListener[0];
    return listeners;
  }

  /**
   * Makes sure the currently selected paths are valid according to the current
   * selectionMode. If the selectionMode is set to
   * {@link #CONTIGUOUS_TREE_SELECTION} and the selection isn't contiguous then
   * the selection is reset to the first set of contguous paths. If the
   * selectionMode is set to {@link #SINGLE_TREE_SELECTION} and the selection
   * has more than one path, the selection is reset to the contain only the
   * first path.
   */
  protected void insureRowContinuity()
  {
    if (selectionMode == CONTIGUOUS_TREE_SELECTION && selection != null
        && rowMapper != null)
      {
        int min = listSelectionModel.getMinSelectionIndex();
        if (min != -1)
          {
            int max = listSelectionModel.getMaxSelectionIndex();
            for (int i = min; i <= max; i++)
              {
                if (! listSelectionModel.isSelectedIndex(i))
                  {
                    if (i == min)
                      clearSelection();
                    else
                      {
                        TreePath[] newSelection = new TreePath[i - min];
                        int[] rows = rowMapper.getRowsForPaths(selection);
                        for (int j = 0; j < rows.length; j++)
                          {
                            if (rows[j] < i)
                              newSelection[rows[j] - min] = selection[j];
                          }
                        setSelectionPaths(newSelection);
                        break;
                      }
                  }
              }
          }
      }
    else if (selectionMode == SINGLE_TREE_SELECTION && selection != null
        && selection.length > 1)
      setSelectionPath(selection[0]);
  }

  /**
   * Returns <code>true</code> if the paths are contiguous (take subsequent
   * rows in the diplayed tree view. The method returns <code>true</code> if
   * we have no RowMapper assigned.
   *
   * @param paths the paths to check for continuity
   * @return <code>true</code> if the paths are contiguous or we have no
   *         RowMapper assigned
   */
  protected boolean arePathsContiguous(TreePath[] paths)
  {
    if (rowMapper == null || paths.length < 2)
      return true;

    int length = paths.length;
    TreePath[] tmp = new TreePath[1];
    tmp[0] = paths[0];
    int min = rowMapper.getRowsForPaths(tmp)[0];
    BitSet selected = new BitSet();
    int valid = 0;
    for (int i = 0; i < length; i++)
      {
        if (paths[i] != null)
          {
            tmp[0] = paths[i];
            int[] rows = rowMapper.getRowsForPaths(tmp);
            if (rows == null)
              return false; // No row mapping yet, can't be selected.
            int row = rows[0];
            if (row == -1 || row < (min - length) || row > (min + length))
              return false; // Not contiguous.
            min = Math.min(min, row);
            if (! selected.get(row))
              {
                selected.set(row);
                valid++;
              }

          }
      }
    int max = valid + min;
    for (int i = min; i < max; i++)
      if (! selected.get(i))
        return false; // Not contiguous.
    return true;
  }

  /**
   * Checks if the paths can be added. This returns <code>true</code> if:
   * <ul>
   * <li><code>paths</code> is <code>null</code> or empty</li>
   * <li>we have no RowMapper assigned</li>
   * <li>nothing is currently selected</li>
   * <li>selectionMode is {@link #DISCONTIGUOUS_TREE_SELECTION}</li>
   * <li>adding the paths to the selection still results in a contiguous set of
   * paths</li>
   *
   * @param paths the paths to check
   * @return <code>true</code> if the paths can be added with respect to the
   *         selectionMode
   */
  protected boolean canPathsBeAdded(TreePath[] paths)
  {
    if (paths == null || paths.length == 0 || rowMapper == null
        || selection == null || selectionMode == DISCONTIGUOUS_TREE_SELECTION)
      return true;

    BitSet selected = new BitSet();
    int min = listSelectionModel.getMinSelectionIndex();
    int max = listSelectionModel.getMaxSelectionIndex();
    TreePath[] tmp = new TreePath[1];
    if (min != -1)
      {
        // Set the bitmask of selected elements.
        for (int i = min; i <= max; i++)
          selected.set(i);
      }
    else
      {
        tmp[0] = paths[0];
        min = rowMapper.getRowsForPaths(tmp)[0];
        max = min;
      }
    // Mark new paths as selected.
    for (int i = paths.length - 1; i >= 0; i--)
      {
        if (paths[i] != null)
          {
            tmp[0] = paths[i];
            int[] rows = rowMapper.getRowsForPaths(tmp);
            if (rows == null)
              return false; // Now row mapping yet, can't be selected.
            int row = rows[0];
            if (row == -1)
              return false; // Now row mapping yet, can't be selected.
            min = Math.min(min, row);
            max = Math.max(max, row);
            selected.set(row);
          }
      }
    // Now look if the new selection would be contiguous.
    for (int i = min; i <= max; i++)
      if (! selected.get(i))
        return false;
    return true;
  }

  /**
   * Checks if the paths can be removed without breaking the continuity of the
   * selection according to selectionMode.
   *
   * @param paths the paths to check
   * @return <code>true</code> if the paths can be removed with respect to the
   *         selectionMode
   */
  protected boolean canPathsBeRemoved(TreePath[] paths)
  {
    if (rowMapper == null || isSelectionEmpty()
        || selectionMode == DISCONTIGUOUS_TREE_SELECTION)
      return true;

    HashSet<TreePath> set = new HashSet<TreePath>();
    for (int i = 0; i < selection.length; i++)
      set.add(selection[i]);

    for (int i = 0; i < paths.length; i++)
      set.remove(paths[i]);

    TreePath[] remaining = new TreePath[set.size()];
    Iterator<TreePath> iter = set.iterator();

    for (int i = 0; i < remaining.length; i++)
      remaining[i] = iter.next();

    return arePathsContiguous(remaining);
  }

  /**
   * Notify the installed listeners that the given patches have changed. This
   * method will call listeners if invoked, but it is not called from the
   * implementation of this class.
   *
   * @param vPaths the vector of the changed patches
   * @param oldLeadSelection the old selection index
   */
  protected void notifyPathChange(Vector<PathPlaceHolder> vPaths,
                                  TreePath oldLeadSelection)
  {

    int numChangedPaths = vPaths.size();
    boolean[] news = new boolean[numChangedPaths];
    TreePath[] paths = new TreePath[numChangedPaths];
    for (int i = 0; i < numChangedPaths; i++)
      {
        PathPlaceHolder p = vPaths.get(i);
        news[i] = p.isNew;
        paths[i] = p.path;
      }

    TreeSelectionEvent event = new TreeSelectionEvent(this, paths, news,
                                                      oldLeadSelection,
                                                      leadPath);
    fireValueChanged(event);
  }

  /**
   * Updates the lead selection row number after changing the lead selection
   * path.
   */
  protected void updateLeadIndex()
  {
    leadIndex = -1;
    if (leadPath != null)
      {
        leadRow = -1;
        if (selection == null)
          leadPath = null;
        else
          {
            for (int i = selection.length - 1; i >= 0 && leadIndex == -1; i--)
              {
                if (selection[i] == leadPath)
                  leadIndex = i;
              }
          }
      }
  }

  /**
   * This method exists due historical reasons and returns without action
   * (unless overridden). For compatibility with the applications that override
   * it, it is still called from the {@link #setSelectionPaths(TreePath[])} and
   * {@link #addSelectionPaths(TreePath[])}.
   */
  protected void insureUniqueness()
  {
    // Following the API 1.4, the method should return without action.
  }
}
