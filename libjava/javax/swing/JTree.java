/* JTree.java --
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

package javax.swing;

import java.awt.Dimension;
import java.awt.Rectangle;
import java.util.Hashtable;
import java.util.Vector;
import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.plaf.TreeUI;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;


public class JTree extends JComponent
  implements Scrollable, Accessible
{
  private static final long serialVersionUID = 7559816092864483649L;

  protected TreeModel treeModel;

  /**
   * Creates a new <code>JTree</code> object.
   */
  public JTree()
  {
    treeModel = createTreeModel(null);
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param value the initial nodes in the tree
   */
  public JTree(Hashtable value)
  {
    treeModel = createTreeModel(value);
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param value the initial nodes in the tree
   */
  public JTree(Object[] value)
  {
    treeModel = createTreeModel(value);
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param model the model to use
   */
  public JTree(TreeModel model)
  {
    treeModel = model;
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param root the root node
   */
  public JTree(TreeNode root)
  {
    this(root, false);
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param root the root node
   * @param asksAllowChildren if false, all nodes without children are leaf nodes.
   * If true, only nodes that do not allow children are leaf nodes.
   */
  public JTree(TreeNode root, boolean asksAllowChildren)
  {
  }

  /**
   * Creates a new <code>JTree</code> object.
   *
   * @param value the initial nodes in the tree
   */
  public JTree(Vector value)
  {
    treeModel = createTreeModel(value);
  }

  /**
   * Creates a new <code>TreeModel</code> object.
   *
   * @param value the values stored in the model
   */
  protected static TreeModel createTreeModel(Object value)
  {
    // FIXME: Implement this.
    return null;
  }

  public TreeUI getUI()
  {
    return (TreeUI) ui;
  }

  public void setUI(TreeUI ui)
  {
    super.setUI(ui);
  }

  public void updateUI()
  {
    setUI((TreeUI) UIManager.getUI(this));
  }

  public String getUIClassID()
  {
    return "TreeUI";
  }

  public AccessibleContext getAccessibleContext()
  {
    return null;
  }

  public Dimension getPreferredScrollableViewportSize()
  {
    return null;
  }

  public int getScrollableUnitIncrement(Rectangle visibleRect,
                                        int orientation, int direction)
  {
    return 1;
  }

  public int getScrollableBlockIncrement(Rectangle visibleRect,
                                         int orientation, int direction)
  {
    return 1;
  }

  public boolean getScrollableTracksViewportWidth()
  {
    return false;
  }

  public boolean getScrollableTracksViewportHeight()
  {
    return false;
  }

  /**
   * Adds a <code>TreeExpansionListener</code> object to the tree.
   *
   * @param listener the listener to add
   */
  public void addTreeExpansionListener(TreeExpansionListener listener)
  {
    listenerList.add(TreeExpansionListener.class, listener);
  }

  /**
   * Removes a <code>TreeExpansionListener</code> object from the tree.
   *
   * @param listener the listener to remove
   */
  public void removeTreeExpansionListener(TreeExpansionListener listener)
  {
    listenerList.remove(TreeExpansionListener.class, listener);
  }

  /**
   * Returns all added <code>TreeExpansionListener</code> objects.
   *
   * @return an array of listeners
   */
  public TreeExpansionListener[] getTreeExpansionListeners()
  {
    return (TreeExpansionListener[]) getListeners(TreeExpansionListener.class);
  }

  /**
   * Notifies all listeners that the tree was collapsed.
   *
   * @param path the path to the node that was collapsed
   */
  public void fireTreeCollapsed(TreePath path)
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeExpansionListener[] listeners = getTreeExpansionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeCollapsed(event);
  }
  
  /**
   * Notifies all listeners that the tree was expanded.
   *
   * @param path the path to the node that was expanded
   */
  public void fireTreeExpanded(TreePath path)
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeExpansionListener[] listeners = getTreeExpansionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeExpanded(event);
  }

  /**
   * Adds a <code>TreeSelctionListener</code> object to the tree.
   *
   * @param listener the listener to add
   */
  public void addTreeSelectionListener(TreeSelectionListener listener)
  {
    listenerList.add(TreeSelectionListener.class, listener);
  }

  /**
   * Removes a <code>TreeSelectionListener</code> object from the tree.
   *
   * @param listener the listener to remove
   */
  public void removeTreeSelectionListener(TreeSelectionListener listener)
  {
    listenerList.remove(TreeSelectionListener.class, listener);
  }

  /**
   * Returns all added <code>TreeSelectionListener</code> objects.
   *
   * @return an array of listeners
   */
  public TreeSelectionListener[] getTreeSelectionListeners()
  {
    return (TreeSelectionListener[]) getListeners(TreeSelectionListener.class);
  }

  /**
   * Notifies all listeners when the selection of the tree changed.
   *
   * @param event the event to send
   */
  protected void fireValueChanged(TreeSelectionEvent event)
  {
    TreeSelectionListener[] listeners = getTreeSelectionListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].valueChanged(event);
  }

  /**
   * Adds a <code>TreeWillExpandListener</code> object to the tree.
   *
   * @param listener the listener to add
   */
  public void addTreeWillExpandListener(TreeWillExpandListener listener)
  {
    listenerList.add(TreeWillExpandListener.class, listener);
  }

  /**
   * Removes a <code>TreeWillExpandListener</code> object from the tree.
   *
   * @param listener the listener to remove
   */
  public void removeTreeWillExpandListener(TreeWillExpandListener listener)
  {
    listenerList.remove(TreeWillExpandListener.class, listener);
  }

  /**
   * Returns all added <code>TreeWillExpandListener</code> objects.
   *
   * @return an array of listeners
   */
  public TreeWillExpandListener[] getTreeWillExpandListeners()
  {
    return (TreeWillExpandListener[]) getListeners(TreeWillExpandListener.class);
  }

  /**
   * Notifies all listeners that the tree will collapse.
   *
   * @param path the path to the node that will collapse
   */
  public void fireTreeWillCollapse(TreePath path)
    throws ExpandVetoException
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeWillExpandListener[] listeners = getTreeWillExpandListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeWillCollapse(event);
  }

  /**
   * Notifies all listeners that the tree will expand.
   *
   * @param path the path to the node that will expand
   */
  public void fireTreeWillExpand(TreePath path)
    throws ExpandVetoException
  {
    TreeExpansionEvent event = new TreeExpansionEvent(this, path);
    TreeWillExpandListener[] listeners = getTreeWillExpandListeners();

    for (int index = 0; index < listeners.length; ++index)
      listeners[index].treeWillExpand(event);
  }
}
