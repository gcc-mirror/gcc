/* DefaultTreeModel.java -- 
   Copyright (C) 2002, 2004, 2005 Free Software Foundation, Inc.
 
This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING. If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library. Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module. An independent module is a module which is not derived from
or based on this library. If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so. If you do not wish to do so, delete this
exception statement from your version. */

package javax.swing.tree;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.EventListener;

import javax.swing.event.EventListenerList;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.tree.DefaultMutableTreeNode;

/**
 * DefaultTreeModel
 * 
 * @author Andrew Selkirk
 */
public class DefaultTreeModel
    implements Serializable, TreeModel
{
  static final long serialVersionUID = -2621068368932566998L;

  /**
   * root
   */
  protected TreeNode root = null;

  /**
   * listenerList
   */
  protected EventListenerList listenerList = new EventListenerList();

  /**
   * asksAllowsChildren
   */
  protected boolean asksAllowsChildren;

  /**
   * Constructor DefaultTreeModel
   * 
   * @param root the tree root.
   */
  public DefaultTreeModel(TreeNode root)
  {
    if (root == null)
      root = new DefaultMutableTreeNode();
    setRoot(root);
  }

  /**
   * Constructor DefaultTreeModel
   * 
   * @param root the tree root.
   * @param asksAllowsChildren TODO
   */
  public DefaultTreeModel(TreeNode root, boolean asksAllowsChildren)
  {
    setRoot(root);
    this.asksAllowsChildren = asksAllowsChildren;
  }

  /**
   * writeObject
   * 
   * @param obj the object.
   * @exception IOException TODO
   */
  private void writeObject(ObjectOutputStream obj) throws IOException
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
   * asksAllowsChildren
   * 
   * @return boolean
   */
  public boolean asksAllowsChildren()
  {
    return asksAllowsChildren;
  }

  /**
   * setAsksAllowsChildren
   * 
   * @param value TODO
   */
  public void setAsksAllowsChildren(boolean value)
  {
    asksAllowsChildren = value;
  }

  /**
   * setRoot
   * 
   * @param root the root node.
   */
  public void setRoot(TreeNode root)
  {
    // Sanity Check
    if (root == null)
      {
        throw new IllegalArgumentException("null root");
      }
    // Set new root
    this.root = root;
  }

  /**
   * getRoot
   * 
   * @return Object
   */
  public Object getRoot()
  {
    return root;
  }

  /**
   * getIndexOfChild
   * 
   * @param parent TODO
   * @param child TODO
   * @return int
   */
  public int getIndexOfChild(Object parent, Object child)
  {
    for (int i = 0; i < getChildCount(parent); i++)
      {
        if (getChild(parent, i).equals(child))
          return i;
      }
    return -1;
  }

  /**
   * getChild
   * 
   * @param node TODO
   * @param idx TODO
   * @return Object
   */
  public Object getChild(Object node, int idx)
  {
    if (node instanceof TreeNode)
      return ((TreeNode) node).getChildAt(idx);
    else
      return null;
  }

  /**
   * getChildCount
   * 
   * @param node TODO
   * @return int
   */
  public int getChildCount(Object node)
  {
    if (node instanceof TreeNode)
      return ((TreeNode) node).getChildCount();
    else
      return 0;
  }

  /**
   * isLeaf
   * 
   * @param node TODO
   * @return boolean
   */
  public boolean isLeaf(Object node)
  {
    if (node instanceof TreeNode)
      return ((TreeNode) node).isLeaf();
    else
      return true;
  }

  /**
   * Invoke this method if you've modified the TreeNodes upon 
   * which this model depends. The model will notify all of its 
   * listeners that the model has changed.
   */
  public void reload()
  {
    // TODO
  }

  /**
   * Invoke this method if you've modified the TreeNodes upon 
   * which this model depends. The model will notify all of its 
   * listeners that the model has changed.
   * 
   * @param node - TODO
   */
  public void reload(TreeNode node)
  {
    // TODO
  }

  /**
   * Messaged when the user has altered the value for the item 
   * identified by path to newValue. If newValue signifies a truly new 
   * value the model should post a treeNodesChanged event.
   * This sets the user object of the TreeNode identified by 
   * path and posts a node changed. If you use custom user objects 
   * in the TreeModel you're going to need to subclass this and set 
   * the user object of the changed node to something meaningful.
   * 
   * @param path - path to the node that the user has altered
   * @param newValue - the new value from the TreeCellEditor
   */
  public void valueForPathChanged(TreePath path, Object newValue)
  {
    Object node = path.getLastPathComponent();
    if (node instanceof MutableTreeNode)
      {
        ((MutableTreeNode) node).setUserObject(newValue);
        int[] ci = null;
        Object[] c = null; 
        Object[] parentPath = path.getPath();
        if (path.getPathCount() > 1)
          {
            Object parent = ((TreeNode) node).getParent();
            ci = new int[1];
            ci[0] = getIndexOfChild(parent, node);
            node = newValue;
            path = path.getParentPath().pathByAddingChild(node);
            c = new Object[1];
            c[0] = node;
            parentPath = path.getParentPath().getPath();
          }
        
        fireTreeNodesChanged(this, parentPath, ci, c);
      }
    }

  /**
   * Invoked this to insert newChild at location index in parents children.
   * This will then message nodesWereInserted to create the appropriate event. 
   * This is the preferred way to add children as it will create the 
   * appropriate event.
   * 
   * @param newChild is the node to add to the parent's children
   * @param parent is the parent of the newChild
   * @param index is the index of the newChild
   */
  public void insertNodeInto(MutableTreeNode newChild, MutableTreeNode parent,
                             int index)
  {
    parent.insert(newChild, index);
    int[] childIndices = new int[1];
    childIndices[0] = index;
    nodesWereInserted(parent, childIndices);
  }

  /**
   * Message this to remove node from its parent. This will message 
   * nodesWereRemoved to create the appropriate event. This is the preferred 
   * way to remove a node as it handles the event creation for you.
   * 
   * @param node to be removed
   */
  public void removeNodeFromParent(MutableTreeNode node)
  {
    TreeNode parent = node.getParent();
    Object[] children = new Object[1];
    children[0] = node;
    int[] childIndices = new int[1];
    childIndices[0] = getIndexOfChild(parent, node);
    node.removeFromParent();
    nodesWereRemoved(parent, childIndices, children);
  }

  /**
   * Invoke this method after you've changed how node is to be represented
   * in the tree.
   * 
   * @param node that was changed
   */
  public void nodeChanged(TreeNode node)
  {
    TreeNode parent = node.getParent();
    int[] childIndices = new int[1];
    childIndices[0] = getIndexOfChild(parent, node);
    Object[] children = new Object[1];
    children[0] = node;
    fireTreeNodesChanged(this, getPathToRoot(node), childIndices, children);
  }

  /**
   * Invoke this method after you've inserted some TreeNodes 
   * into node. childIndices should be the index of the new elements and must 
   * be sorted in ascending order.
   * 
   * @param parent that had a child added to
   * @param childIndices of the children added
   */
  public void nodesWereInserted(TreeNode parent, int[] childIndices)
  {
    Object[] children = new Object[childIndices.length];
    for (int i = 0; i < children.length; i++)
      children[i] = getChild(parent, childIndices[i]);
    fireTreeNodesInserted(this, getPathToRoot(parent), childIndices, children);
  }

  /**
   * Invoke this method after you've removed some TreeNodes from node. 
   * childIndices should be the index of the removed elements and 
   * must be sorted in ascending order. And removedChildren should be the 
   * array of the children objects that were removed.
   * 
   * @param parent that had a child added to
   * @param childIndices of the children added
   * @param removedChildren are all the children removed from parent.
   */
  public void nodesWereRemoved(TreeNode parent, int[] childIndices, 
                               Object[] removedChildren)
  {
    fireTreeNodesRemoved(this, getPathToRoot(parent), childIndices, 
                         removedChildren);
  }

  /**
   * Invoke this method after you've changed how the children identified by 
   * childIndices are to be represented in the tree.
   * 
   * @param node that is the parent of the children that changed in a tree.
   * @param childIndices are the child nodes that changed.
   */
  public void nodesChanged(TreeNode node, int[] childIndices)
  {
    Object[] children = new Object[childIndices.length];
    for (int i = 0; i < children.length; i++)
      children[i] = getChild(node, childIndices[i]);
    fireTreeNodesChanged(this, getPathToRoot(node), childIndices, children);
  }

  /**
   * Invoke this method if you've totally changed the children of node and 
   * its childrens children. This will post a treeStructureChanged event.
   * 
   * @param node that had its children and grandchildren changed.
   */
  public void nodeStructureChanged(TreeNode node)
  {
    // TODO
  }

  /**
   * Builds the parents of node up to and including the root node, where 
   * the original node is the last element in the returned array. The 
   * length of the returned array gives the node's depth in the tree.
   * 
   * @param node - the TreeNode to get the path for
   * @return TreeNode[] - the path from node to the root
   */
  public TreeNode[] getPathToRoot(TreeNode node)
  {
    return getPathToRoot(node, 0);
  }

  /**
   * Builds the parents of node up to and including the root node, where 
   * the original node is the last element in the returned array. The 
   * length of the returned array gives the node's depth in the tree.
   * 
   * @param node - the TreeNode to get the path for
   * @param depth - an int giving the number of steps already taken 
   * towards the root (on recursive calls), used to size the returned array
   * @return an array of TreeNodes giving the path from the root to the 
   * specified node
   */
  protected TreeNode[] getPathToRoot(TreeNode node, int depth)
  {
    if (node == null)
      {
        if (depth == 0)
          return null;
        
        return new TreeNode[depth];
      }

    TreeNode[] path = getPathToRoot(node.getParent(), depth + 1);
    path[path.length - depth - 1] = node;
    return path;
  }

  /**
   * Registers a listere to the model.
   * 
   * @param listener the listener to add
   */
  public void addTreeModelListener(TreeModelListener listener)
  {
    listenerList.add(TreeModelListener.class, listener);
  }

  /**
   * Removes a listener from the model.
   * 
   * @param listener the listener to remove
   */
  public void removeTreeModelListener(TreeModelListener listener)
  {
    listenerList.remove(TreeModelListener.class, listener);
  }

  /**
   * Returns all registered <code>TreeModelListener</code> listeners.
   * 
   * @return an array of listeners.
   * 
   * @since 1.4
   */
  public TreeModelListener[] getTreeModelListeners()
  {
    return (TreeModelListener[]) listenerList
        .getListeners(TreeModelListener.class);
  }

  /**
   * Notifies all listeners that have registered interest for notification 
   * on this event type. The event instance is lazily created using the parameters 
   * passed into the fire method.
   * 
   * @param source the node being changed
   * @param path the path to the root node
   * @param childIndices the indices of the changed elements
   * @param children the changed elements
   */
  protected void fireTreeNodesChanged(Object source, Object[] path,
      int[] childIndices, Object[] children)
  {
    TreeModelEvent event = new TreeModelEvent(source, path, childIndices,
        children);

    TreeModelListener[] listeners = getTreeModelListeners();

    for (int i = listeners.length - 1; i >= 0; --i)
      listeners[i].treeNodesChanged(event);
  }

  /**
   * fireTreeNodesInserted
   * 
   * @param source the node where new nodes got inserted
   * @param path the path to the root node
   * @param childIndices the indices of the new elements
   * @param children the new elements
   */
  protected void fireTreeNodesInserted(Object source, Object[] path,
      int[] childIndices, Object[] children)
  {
    TreeModelEvent event = new TreeModelEvent(source, path, childIndices,
        children);
    TreeModelListener[] listeners = getTreeModelListeners();

    for (int i = listeners.length - 1; i >= 0; --i)
      listeners[i].treeNodesInserted(event);
  }

  /**
   * fireTreeNodesRemoved
   * 
   * @param source the node where nodes got removed-
   * @param path the path to the root node
   * @param childIndices the indices of the removed elements
   * @param children the removed elements
   */
  protected void fireTreeNodesRemoved(Object source, Object[] path,
      int[] childIndices, Object[] children)
  {
    TreeModelEvent event = new TreeModelEvent(source, path, childIndices,
        children);
    TreeModelListener[] listeners = getTreeModelListeners();

    for (int i = listeners.length - 1; i >= 0; --i)
      listeners[i].treeNodesRemoved(event);
  }

  /**
   * fireTreeStructureChanged
   * 
   * @param source the node where the model has changed
   * @param path the path to the root node
   * @param childIndices the indices of the affected elements
   * @param children the affected elements
   */
  protected void fireTreeStructureChanged(Object source, Object[] path,
      int[] childIndices, Object[] children)
  {
    TreeModelEvent event = new TreeModelEvent(source, path, childIndices,
        children);
    TreeModelListener[] listeners = getTreeModelListeners();

    for (int i = listeners.length - 1; i >= 0; --i)
      listeners[i].treeStructureChanged(event);
  }

  /**
   * Returns the registered listeners of a given type.
   *
   * @param listenerType the listener type to return
   *
   * @return an array of listeners
   *
   * @since 1.3
   */
  public EventListener[] getListeners(Class listenerType)
  {
    return listenerList.getListeners(listenerType);
  }
}
