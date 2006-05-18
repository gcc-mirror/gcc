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

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.Arrays;
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
   * Constructs a new DefaultTreeSelectionModel.
   */
  public DefaultTreeSelectionModel()
  {
    setSelectionMode(DISCONTIGUOUS_TREE_SELECTION);
    listenerList = new EventListenerList();
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
    
    // Clone the selection and the list selection model.
    cloned.selection = (TreePath[]) selection.clone();
    if (listSelectionModel!=null)
      cloned.listSelectionModel = 
        (DefaultListSelectionModel) listSelectionModel.clone();
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
        StringBuffer b = new StringBuffer("selected rows: [");
        for (int i = 0; i < selection.length; i++)
          {
            b.append(getRow(selection[i]));
            b.append(' ');
          }
        b.append(", lead "+getLeadSelectionRow());
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
    selectionMode = mode;
    insureRowContinuity();
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
    // The most frequently only one cell in the tree is selected.
    TreePath[] ose = selection;
    selection = new TreePath[] { path };
    TreePath oldLead = leadPath;
    leadIndex = 0;
    leadRow = getRow(path);
    leadPath = path;

    TreeSelectionEvent event;

    if (ose != null && ose.length > 0)
      {
        // The first item in the path list is the selected path.
        // The remaining items are unselected pathes.
        TreePath[] changed = new TreePath[ose.length + 1];
        boolean[] news = new boolean[changed.length];
        news[0] = true;
        changed[0] = path;
        System.arraycopy(ose, 0, changed, 1, ose.length);
        event = new TreeSelectionEvent(this, changed, news, oldLead, path);
      }
    else
      {
        event = new TreeSelectionEvent(this, path, true, oldLead, path);
      }
    fireValueChanged(event);
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
    else
      {
        // Generic non optimized implementation.
        int[] rows = mapper.getRowsForPaths(new TreePath[] { path });
        if (rows.length == 0)
          return - 1;
        else
          return rows[0];
      }
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
    // Must be called, as defined in JDK API 1.4.
    insureUniqueness();
    clearSelection();
    addSelectionPaths(paths);
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
    if (! isPathSelected(path))
      {
        if (selectionMode == SINGLE_TREE_SELECTION || isSelectionEmpty()
            || ! canPathBeAdded(path))
          setSelectionPath(path);
        else
          {
            TreePath[] temp = new TreePath[selection.length + 1];
            System.arraycopy(selection, 0, temp, 0, selection.length);
            temp[temp.length - 1] = path;
            selection = new TreePath[temp.length];
            System.arraycopy(temp, 0, selection, 0, temp.length);
          }
      }
    
     if (path!=leadPath)
       {
        TreePath oldLead = leadPath;
        leadPath = path;
        leadRow = getRow(path);
        leadIndex = selection.length - 1;
        fireValueChanged(new TreeSelectionEvent(this, path, true, oldLead,
                                                leadPath));
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
    // Must be called, as defined in JDK API 1.4.
    insureUniqueness();

    if (paths != null)
      {
        TreePath v0 = null;
        for (int i = 0; i < paths.length; i++)
          {
            v0 = paths[i];
            if (! isPathSelected(v0))
              {
                if (isSelectionEmpty())
                  setSelectionPath(v0);
                else
                  {
                    TreePath[] temp = new TreePath[selection.length + 1];
                    System.arraycopy(selection, 0, temp, 0, selection.length);
                    temp[temp.length - 1] = v0;
                    selection = new TreePath[temp.length];
                    System.arraycopy(temp, 0, selection, 0, temp.length);
                  }
               TreePath oldLead = leadPath;                
                leadPath = paths[paths.length - 1];
                leadRow = getRow(leadPath);
                leadIndex = selection.length - 1;

                fireValueChanged(new TreeSelectionEvent(this, v0, true,
                                                        oldLead, leadPath));
              }
          }
        insureRowContinuity();
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
    if (isSelectionEmpty())
      return;
    
    int index = - 1;
    if (isPathSelected(path))
      {
        for (int i = 0; i < selection.length; i++)
          {
            if (selection[i].equals(path))
              {
                index = i;
                break;
              }
          }
        TreePath[] temp = new TreePath[selection.length - 1];
        System.arraycopy(selection, 0, temp, 0, index);
        System.arraycopy(selection, index + 1, temp, index, selection.length
                                                            - index - 1);
        selection = new TreePath[temp.length];
        System.arraycopy(temp, 0, selection, 0, temp.length);
        
        // If the removed path was the lead path, set the lead path to null.
        TreePath oldLead = leadPath;
        if (path!=null && leadPath!=null && path.equals(leadPath))
          leadPath = null;

        fireValueChanged(new TreeSelectionEvent(this, path, false, oldLead,
                                                leadPath));
        insureRowContinuity();
      }
  }

  /**
   * Removes the paths from the selection. If this changes the selection the
   * registered TreeSelectionListeners are notified.
   * 
   * @param paths the paths to remove
   */
  public void removeSelectionPaths(TreePath[] paths)
  {
    if (isSelectionEmpty())
      return;
    if (paths != null)
      {
        int index = - 1;
        TreePath v0 = null;
        TreePath oldLead = leadPath;
        for (int i = 0; i < paths.length; i++)
          {
            v0 = paths[i];
            if (isPathSelected(v0))
              {
                for (int x = 0; x < selection.length; x++)
                  {
                    if (selection[i].equals(v0))
                      {
                        index = x;
                        break;
                      }
                    if (leadPath != null && leadPath.equals(v0))
                      leadPath = null;
                  }
                TreePath[] temp = new TreePath[selection.length - 1];
                System.arraycopy(selection, 0, temp, 0, index);
                System.arraycopy(selection, index + 1, temp, index,
                                 selection.length - index - 1);
                selection = new TreePath[temp.length];
                System.arraycopy(temp, 0, selection, 0, temp.length);

                fireValueChanged(new TreeSelectionEvent(this, v0, false,
                                                        oldLead, leadPath));
              }
          }
        insureRowContinuity();
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
    return ((selection == null) || (selection.length == 0));
  }

  /**
   * Removes all paths from the selection. Fire the unselection event.
   */
  public void clearSelection()
  {
    if (! isSelectionEmpty())
      {
        TreeSelectionEvent event = new TreeSelectionEvent(
          this, selection, new boolean[selection.length], leadPath, null);
        leadPath = null;
        selection = null;
        fireValueChanged(event);
      }
    else
      {
        leadPath = null;
        selection = null;
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
    if (rowMapper == null)
      return null;
    else
      return rowMapper.getRowsForPaths(selection);
  }

  /**
   * Returns the smallest row index from the selection.
   * 
   * @return the smallest row index from the selection
   */
  public int getMinSelectionRow()
  {
    if ((rowMapper == null) || (selection == null) || (selection.length == 0))
      return - 1;
    else
      {
        int[] rows = rowMapper.getRowsForPaths(selection);
        int minRow = Integer.MAX_VALUE;
        for (int index = 0; index < rows.length; index++)
          minRow = Math.min(minRow, rows[index]);
        return minRow;
      }
  }

  /**
   * Returns the largest row index from the selection.
   * 
   * @return the largest row index from the selection
   */
  public int getMaxSelectionRow()
  {
    if ((rowMapper == null) || (selection == null) || (selection.length == 0))
      return - 1;
    else
      {
        int[] rows = rowMapper.getRowsForPaths(selection);
        int maxRow = - 1;
        for (int index = 0; index < rows.length; index++)
          maxRow = Math.max(maxRow, rows[index]);
        return maxRow;
      }
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
    // Return false if nothing is selected.
    if (isSelectionEmpty())
      return false;

    RowMapper mapper = getRowMapper();

    if (mapper instanceof AbstractLayoutCache)
      {
        // The absolute majority of cases, unless the TreeUI is very
        // seriously rewritten
        AbstractLayoutCache ama = (AbstractLayoutCache) mapper;
        TreePath path = ama.getPathForRow(row);
        return isPathSelected(path);
      }
    else
      {
        // Generic non optimized implementation.
        int[] rows = mapper.getRowsForPaths(selection);
        for (int i = 0; i < rows.length; i++)
          if (rows[i] == row)
            return true;
        return false;
      }
  }

  /**
   * Updates the mappings from TreePaths to row indices.
   */
  public void resetRowSelection()
  {
    // Nothing to do here.
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
   * @since 1.4
   */
  public PropertyChangeListener[] getPropertyChangeListeners()
  {
    return changeSupport.getPropertyChangeListeners();
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
    if (selection == null || selection.length < 2)
      return;
    else if (selectionMode == CONTIGUOUS_TREE_SELECTION)
      {
        if (rowMapper == null)
          // This is the best we can do without the row mapper:
          selectOne();
        else
          {
            int[] rows = rowMapper.getRowsForPaths(selection);
            Arrays.sort(rows);
            int i;
            for (i = 1; i < rows.length; i++)
              {
                if (rows[i - 1] != rows[i] - 1)
                  // Break if no longer continuous.
                  break;
              }

            if (i < rows.length)
              {
                TreePath[] ns = new TreePath[i];
                for (int j = 0; j < ns.length; j++)
                  ns[i] = getPath(j);
                setSelectionPaths(ns);
              }
          }
      }
    else if (selectionMode == SINGLE_TREE_SELECTION)
      selectOne();
  }
  
  /**
   * Keep only one (normally last or leading) path in the selection.
   */
  private void selectOne()
  {
    if (leadIndex > 0 && leadIndex < selection.length)
      setSelectionPath(selection[leadIndex]);
    else
      setSelectionPath(selection[selection.length -1]);
  }
  
  /**
   * Get path for the given row that must be in the current selection.
   */
  private TreePath getPath(int row)
  {
    if (rowMapper instanceof AbstractLayoutCache)
      return ((AbstractLayoutCache) rowMapper).getPathForRow(row);
    else
      {
        int[] rows = rowMapper.getRowsForPaths(selection);
        for (int i = 0; i < rows.length; i++)
          if (rows[i] == row)
            return selection[i];
      }
    throw new InternalError(row + " not in selection");
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

    int[] rows = rowMapper.getRowsForPaths(paths);
    
    // The patches may not be sorted.
    Arrays.sort(rows);

    for (int i = 1; i < rows.length; i++)
      {
        if (rows[i-1] != rows[i] - 1)
          return false;
      }
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
    if (rowMapper == null || isSelectionEmpty()
        || selectionMode == DISCONTIGUOUS_TREE_SELECTION)
      return true;
   
    TreePath [] all = new TreePath[paths.length + selection.length];
    System.arraycopy(paths, 0, all, 0, paths.length);
    System.arraycopy(selection, 0, all, paths.length, selection.length);

    return arePathsContiguous(all);
  }
  
  /**
   * Checks if the single path can be added to selection.
   */
  private boolean canPathBeAdded(TreePath path)
  {
    if (rowMapper == null || isSelectionEmpty()
        || selectionMode == DISCONTIGUOUS_TREE_SELECTION)
      return true;

    TreePath[] all = new TreePath[selection.length + 1];
    System.arraycopy(selection, 0, all, 0, selection.length);
    all[all.length - 1] = path;

    return arePathsContiguous(all);
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
    
    HashSet set = new HashSet();
    for (int i = 0; i < selection.length; i++)
      set.add(selection[i]);
    
    for (int i = 0; i < paths.length; i++)
      set.remove(paths[i]);
    
    TreePath[] remaining = new TreePath[set.size()];
    Iterator iter = set.iterator();
    
    for (int i = 0; i < remaining.length; i++)
      remaining[i] = (TreePath) iter.next();
    
    return arePathsContiguous(remaining);
  }

  /**
   * Notify the installed listeners that the given patches have changed. This
   * method will call listeners if invoked, but it is not called from the
   * implementation of this class.
   * 
   * @param vPathes the vector of the changed patches
   * @param oldLeadSelection the old selection index
   */
  protected void notifyPathChange(Vector vPathes, TreePath oldLeadSelection)
  {
    TreePath[] pathes = new TreePath[vPathes.size()];
    for (int i = 0; i < pathes.length; i++)
      pathes[i] = (TreePath) vPathes.get(i);

    boolean[] news = new boolean[pathes.length];
    for (int i = 0; i < news.length; i++)
      news[i] = isPathSelected(pathes[i]);

    TreeSelectionEvent event = new TreeSelectionEvent(this, pathes, news,
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
    if (isSelectionEmpty())
      {
        leadRow = leadIndex = - 1;
      }
    else
      {
        leadRow = getRow(leadPath);
        for (int i = 0; i < selection.length; i++)
          {
            if (selection[i].equals(leadPath))
              {
                leadIndex = i;
                break;
              }
          }
        leadIndex = leadRow;
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
