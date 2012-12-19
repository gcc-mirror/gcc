/* DefaultMutableTreeNode.java --
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


package javax.swing.tree;

import gnu.java.util.EmptyEnumeration;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.Vector;


/**
 * A default implementation of the {@link MutableTreeNode} interface.
 *
 * @author Andrew Selkirk
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class DefaultMutableTreeNode
  implements Cloneable, MutableTreeNode, Serializable
{
  private static final long serialVersionUID = -4298474751201349152L;

  /**
   * An empty enumeration, returned by {@link #children()} if a node has no
   * children.
   */
  public static final Enumeration<TreeNode> EMPTY_ENUMERATION =
    new EmptyEnumeration<TreeNode>();

  /**
   * The parent of this node (possibly <code>null</code>).
   */
  protected MutableTreeNode parent;

  /**
   * The child nodes for this node (may be empty).
   */
  protected Vector<MutableTreeNode> children = new Vector<MutableTreeNode>();

  /**
   * userObject
   */
  protected transient Object userObject;

  /**
   * allowsChildren
   */
  protected boolean allowsChildren;

  /**
   * Creates a <code>DefaultMutableTreeNode</code> object.
   * This is equivalent to <code>DefaultMutableTreeNode(null, true)</code>.
   */
  public DefaultMutableTreeNode()
  {
    this(null, true);
  }

  /**
   * Creates a <code>DefaultMutableTreeNode</code> object with the given
   * user object attached to it. This is equivalent to
   * <code>DefaultMutableTreeNode(userObject, true)</code>.
   *
   * @param userObject the user object (<code>null</code> permitted).
   */
  public DefaultMutableTreeNode(Object userObject)
  {
    this(userObject, true);
  }

  /**
   * Creates a <code>DefaultMutableTreeNode</code> object with the given
   * user object attached to it.
   *
   * @param userObject the user object (<code>null</code> permitted).
   * @param allowsChildren <code>true</code> if the code allows to add child
   * nodes, <code>false</code> otherwise
   */
  public DefaultMutableTreeNode(Object userObject, boolean allowsChildren)
  {
    this.userObject = userObject;
    this.allowsChildren = allowsChildren;
  }

  /**
   * Returns a clone of the node.  The clone contains a shallow copy of the
   * user object, and does not copy the parent node or the child nodes.
   *
   * @return A clone of the node.
   */
  public Object clone()
  {
    return new DefaultMutableTreeNode(this.userObject, this.allowsChildren);
  }

  /**
   * Returns a string representation of the node.  This implementation returns
   * <code>getUserObject().toString()</code>, or <code>null</code> if there
   * is no user object.
   *
   * @return A string representation of the node (possibly <code>null</code>).
   */
  public String toString()
  {
    if (userObject == null)
      return null;

    return userObject.toString();
  }

  /**
   * Adds a new child node to this node and sets this node as the parent of
   * the child node.  The child node must not be an ancestor of this node.
   * If the tree uses the {@link DefaultTreeModel}, you must subsequently
   * call {@link DefaultTreeModel#reload(TreeNode)}.
   *
   * @param child the child node (<code>null</code> not permitted).
   *
   * @throws IllegalStateException if {@link #getAllowsChildren()} returns
   *     <code>false</code>.
   * @throws IllegalArgumentException if {@link #isNodeAncestor} returns
   *     <code>true</code>.
   * @throws IllegalArgumentException if <code>child</code> is
   *     <code>null</code>.
   */
  public void add(MutableTreeNode child)
  {
    if (! allowsChildren)
      throw new IllegalStateException();

    if (child == null)
      throw new IllegalArgumentException();

    if (isNodeAncestor(child))
      throw new IllegalArgumentException("Cannot add ancestor node.");

    children.add(child);
    child.setParent(this);
  }

  /**
   * Returns the parent node of this node.
   *
   * @return The parent node (possibly <code>null</code>).
   */
  public TreeNode getParent()
  {
    return parent;
  }

  /**
   * Removes the child with the given index from this node.
   *
   * @param index the index (in the range <code>0</code> to
   *     <code>getChildCount() - 1</code>).
   *
   * @throws ArrayIndexOutOfBoundsException if <code>index</code> is outside
   *         the valid range.
   */
  public void remove(int index)
  {
    MutableTreeNode child = children.remove(index);
    child.setParent(null);
  }

  /**
   * Removes the given child from this node and sets its parent to
   * <code>null</code>.
   *
   * @param node the child node (<code>null</code> not permitted).
   *
   * @throws IllegalArgumentException if <code>node</code> is not a child of
   *     this node.
   * @throws IllegalArgumentException if <code>node</code> is null.
   */
  public void remove(MutableTreeNode node)
  {
    if (node == null)
      throw new IllegalArgumentException("Null 'node' argument.");
    if (node.getParent() != this)
      throw new IllegalArgumentException(
          "The given 'node' is not a child of this node.");
    children.remove(node);
    node.setParent(null);
  }

  /**
   * writeObject
   *
   * @param stream the output stream
   *
   * @exception IOException If an error occurs
   */
  private void writeObject(ObjectOutputStream stream)
    throws IOException
  {
    // TODO: Implement me.
  }

  /**
   * readObject
   *
   * @param stream the input stream
   *
   * @exception IOException If an error occurs
   * @exception ClassNotFoundException TODO
   */
  private void readObject(ObjectInputStream stream)
    throws IOException, ClassNotFoundException
  {
    // TODO: Implement me.
  }

  /**
   * Inserts given child node at the given index.
   *
   * @param node the child node (<code>null</code> not permitted).
   * @param index the index.
   *
   * @throws IllegalArgumentException if <code>node</code> is
   *     </code>null</code>.
   */
  public void insert(MutableTreeNode node, int index)
  {
    if (! allowsChildren)
      throw new IllegalStateException();

    if (node == null)
      throw new IllegalArgumentException("Null 'node' argument.");

    if (isNodeAncestor(node))
      throw new IllegalArgumentException("Cannot insert ancestor node.");

    children.insertElementAt(node, index);
  }

  /**
   * Returns a path to this node from the root.
   *
   * @return an array of tree nodes
   */
  public TreeNode[] getPath()
  {
    return getPathToRoot(this, 0);
  }

  /**
   * Returns an enumeration containing all children of this node.
   * <code>EMPTY_ENUMERATION</code> is returned if this node has no children.
   *
   * @return an enumeration of tree nodes
   */
  @SuppressWarnings("rawtypes") // Required for API compatibility
  public Enumeration children()
  {
    if (children.size() == 0)
      return EMPTY_ENUMERATION;

    return children.elements();
  }

  /**
   * Set the parent node for this node.
   *
   * @param node the parent node
   */
  public void setParent(MutableTreeNode node)
  {
    parent = node;
  }

  /**
   * Returns the child node at a given index.
   *
   * @param index the index
   *
   * @return the child node
   */
  public TreeNode getChildAt(int index)
  {
    return children.elementAt(index);
  }

  /**
   * Returns the number of children of this node.
   *
   * @return the number of children
   */
  public int getChildCount()
  {
    return children.size();
  }

  /**
   * Returns the index of the specified child node, or -1 if the node is not
   * in fact a child of this node.
   *
   * @param node  the node (<code>null</code> not permitted).
   *
   * @return The index of the specified child node, or -1.
   *
   * @throws IllegalArgumentException if <code>node</code> is <code>null</code>.
   */
  public int getIndex(TreeNode node)
  {
    if (node == null)
      throw new IllegalArgumentException("Null 'node' argument.");
    return children.indexOf(node);
  }

  /**
   * Sets the flag that controls whether or not this node allows the addition /
   * insertion of child nodes.  If the flag is set to <code>false</code>, any
   * existing children are removed.
   *
   * @param allowsChildren  the flag.
   */
  public void setAllowsChildren(boolean allowsChildren)
  {
    if (!allowsChildren)
      removeAllChildren();
    this.allowsChildren = allowsChildren;
  }

  /**
   * getAllowsChildren
   *
   * @return boolean
   */
  public boolean getAllowsChildren()
  {
    return allowsChildren;
  }

  /**
   * Sets the user object for this node
   *
   * @param userObject the user object
   */
  public void setUserObject(Object userObject)
  {
    this.userObject = userObject;
  }

  /**
   * Returns the user object attached to this node. <code>null</code> is
   * returned when no user object is set.
   *
   * @return the user object
   */
  public Object getUserObject()
  {
    return userObject;
  }

  /**
   * Removes this node from its parent.
   */
  public void removeFromParent()
  {
    parent.remove(this);
    parent = null;
  }

  /**
   * Removes all child nodes from this node.
   */
  public void removeAllChildren()
  {
    for (int i = getChildCount() - 1; i >= 0; i--)
      remove(i);
  }

  /**
   * Returns <code>true</code> if <code>node</code> is an ancestor of this
   * tree node, and <code>false</code> otherwise.  An ancestor node is any of:
   * <ul>
   * <li>this tree node;</li>
   * <li>the parent node (if there is one);</li>
   * <li>any ancestor of the parent node;</li>
   * </ul>
   * If <code>node</code> is <code>null</code>, this method returns
   * <code>false</code>.
   *
   * @param node  the node (<code>null</code> permitted).
   *
   * @return A boolean.
   */
  public boolean isNodeAncestor(TreeNode node)
  {
    if (node == null)
      return false;

    TreeNode current = this;

    while (current != null && current != node)
      current = current.getParent();

    return current == node;
  }

  /**
   * Returns <code>true</code> if <code>node</code> is a descendant of this
   * tree node, and <code>false</code> otherwise.  A descendant node is any of:
   * <ul>
   * <li>this tree node;</li>
   * <li>the child nodes belonging to this tree node, if there are any;</li>
   * <li>any descendants of the child nodes;</li>
   * </ul>
   * If <code>node</code> is <code>null</code>, this method returns
   * <code>false</code>.
   *
   * @param node  the node (<code>null</code> permitted).
   *
   * @return A boolean.
   */
  public boolean isNodeDescendant(DefaultMutableTreeNode node)
  {
    if (node == null)
      return false;

    TreeNode current = node;

    while (current != null
           && current != this)
      current = current.getParent();

    return current == this;
  }

  /**
   * getSharedAncestor
   *
   * @param node TODO
   *
   * @return TreeNode
   */
  public TreeNode getSharedAncestor(DefaultMutableTreeNode node)
  {
    TreeNode current = this;
    ArrayList<TreeNode> list = new ArrayList<TreeNode>();

    while (current != null)
      {
        list.add(current);
        current = current.getParent();
      }

    current = node;

    while (current != null)
      {
        if (list.contains(current))
          return current;

        current = current.getParent();
      }

    return null;
  }

  /**
   * isNodeRelated
   *
   * @param node TODO
   *
   * @return boolean
   */
  public boolean isNodeRelated(DefaultMutableTreeNode node)
  {
    if (node == null)
      return false;

    return node.getRoot() == getRoot();
  }

  /**
   * getDepth
   *
   * @return int
   */
  public int getDepth()
  {
    if ((! allowsChildren)
        || children.size() == 0)
      return 0;

    Stack<Integer> stack = new Stack<Integer>();
    stack.push(new Integer(0));
    TreeNode node = getChildAt(0);
    int depth = 0;
    int current = 1;

    while (! stack.empty())
      {
        if (node.getChildCount() != 0)
          {
            node = node.getChildAt(0);
            stack.push(new Integer(0));
            current++;
          }
        else
          {
            if (current > depth)
              depth = current;

            int size;
            int index;

            do
              {
                node = node.getParent();
                size = node.getChildCount();
                index = stack.pop().intValue() + 1;
                current--;
              }
            while (index >= size
                   && node != this);

            if (index < size)
              {
                node = node.getChildAt(index);
                stack.push(new Integer(index));
                current++;
              }
          }
      }

    return depth;
  }

  /**
   * getLevel
   *
   * @return int
   */
  public int getLevel()
  {
    int count = -1;
    TreeNode current = this;

    do
      {
        current = current.getParent();
        count++;
      }
    while (current != null);

    return count;
  }

  /**
   * getPathToRoot
   *
   * @param node TODO
   * @param depth TODO
   *
   * @return TreeNode[]
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
   * getUserObjectPath
   *
   * @return Object[]
   */
  public Object[] getUserObjectPath()
  {
    TreeNode[] path = getPathToRoot(this, 0);
    Object[] object = new Object[path.length];

    for (int index = 0; index < path.length; ++index)
      object[index] = ((DefaultMutableTreeNode) path[index]).getUserObject();

    return object;
  }

  /**
   * Returns the root node by iterating the parents of this node.
   *
   * @return the root node
   */
  public TreeNode getRoot()
  {
    TreeNode current = this;
    TreeNode check = current.getParent();

    while (check != null)
      {
        current = check;
        check = current.getParent();
      }

    return current;
  }

  /**
   * Tells whether this node is the root node or not.
   *
   * @return <code>true</code> if this is the root node,
   * <code>false</code>otherwise
   */
  public boolean isRoot()
  {
    return parent == null;
  }

  /**
   * getNextNode
   *
   * @return DefaultMutableTreeNode
   */
  public DefaultMutableTreeNode getNextNode()
  {
    // Return first child.
    if (getChildCount() != 0)
      return (DefaultMutableTreeNode) getChildAt(0);

    // Return next sibling (if needed the sibling of some parent).
    DefaultMutableTreeNode node = this;
    DefaultMutableTreeNode sibling;

    do
      {
        sibling = node.getNextSibling();
        node = (DefaultMutableTreeNode) node.getParent();
      }
    while (sibling == null &&
           node != null);

    // Return sibling.
    return sibling;
  }

  /**
   * getPreviousNode
   *
   * @return DefaultMutableTreeNode
   */
  public DefaultMutableTreeNode getPreviousNode()
  {
    // Return null if no parent.
    if (parent == null)
      return null;

    DefaultMutableTreeNode sibling = getPreviousSibling();

    // Return parent if no sibling.
    if (sibling == null)
      return (DefaultMutableTreeNode) parent;

    // Return last leaf of sibling.
    if (sibling.getChildCount() != 0)
      return sibling.getLastLeaf();

    // Return sibling.
    return sibling;
  }

  /**
   * preorderEnumeration
   *
   * @return Enumeration
   */
  @SuppressWarnings("rawtypes") // Required for API compatibility
  public Enumeration preorderEnumeration()
  {
    return new PreorderEnumeration(this);
  }

  /**
   * postorderEnumeration
   *
   * @return Enumeration
   */
  @SuppressWarnings("rawtypes") // Required for API compatibility
  public Enumeration postorderEnumeration()
  {
    return new PostorderEnumeration(this);
  }

  /**
   * breadthFirstEnumeration
   *
   * @return Enumeration
   */
  @SuppressWarnings("rawtypes") // Required for API compatibility
  public Enumeration breadthFirstEnumeration()
  {
    return new BreadthFirstEnumeration(this);
  }

  /**
   * depthFirstEnumeration
   *
   * @return Enumeration
   */
  @SuppressWarnings("rawtypes") // Required for API compatibility
  public Enumeration depthFirstEnumeration()
  {
    return postorderEnumeration();
  }

  /**
   * pathFromAncestorEnumeration
   *
   * @param node TODO
   *
   * @return Enumeration
   */
  @SuppressWarnings("rawtypes") // Required for API compatibility
  public Enumeration pathFromAncestorEnumeration(TreeNode node)
  {
    if (node == null)
      throw new IllegalArgumentException();

    TreeNode parent = this;
    Vector<TreeNode> nodes = new Vector<TreeNode>();
    nodes.add(this);

    while (parent != node && parent != null)
      {
        parent = parent.getParent();
        nodes.add(0, parent);
      }

    if (parent != node)
      throw new IllegalArgumentException();

    return nodes.elements();
  }

  /**
   * Returns <code>true</code> if <code>node</code> is a child of this tree
   * node, and <code>false</code> otherwise.  If <code>node</code> is
   * <code>null</code>, this method returns <code>false</code>.
   *
   * @param node  the node (<code>null</code> permitted).
   *
   * @return A boolean.
   */
  public boolean isNodeChild(TreeNode node)
  {
    if (node == null)
      return false;

    return node.getParent() == this;
  }

  /**
   * Returns the first child node belonging to this tree node.
   *
   * @return The first child node.
   *
   * @throws NoSuchElementException if this tree node has no children.
   */
  public TreeNode getFirstChild()
  {
    return children.firstElement();
  }

  /**
   * Returns the last child node belonging to this tree node.
   *
   * @return The last child node.
   *
   * @throws NoSuchElementException if this tree node has no children.
   */
  public TreeNode getLastChild()
  {
    return children.lastElement();
  }

  /**
   * Returns the next child after the specified <code>node</code>, or
   * <code>null</code> if there is no child after the specified
   * <code>node</code>.
   *
   * @param node  a child of this node (<code>null</code> not permitted).
   *
   * @return The next child, or <code>null</code>.
   *
   * @throws IllegalArgumentException if <code>node</code> is not a child of
   *     this node, or is <code>null</code>.
   */
  public TreeNode getChildAfter(TreeNode node)
  {
    if (node == null || node.getParent() != this)
      throw new IllegalArgumentException();

    int index = getIndex(node) + 1;

    if (index == getChildCount())
      return null;

    return getChildAt(index);
  }

  /**
   * Returns the previous child before the specified <code>node</code>, or
   * <code>null</code> if there is no child before the specified
   * <code>node</code>.
   *
   * @param node  a child of this node (<code>null</code> not permitted).
   *
   * @return The previous child, or <code>null</code>.
   *
   * @throws IllegalArgumentException if <code>node</code> is not a child of
   *     this node, or is <code>null</code>.
   */
  public TreeNode getChildBefore(TreeNode node)
  {
    if (node == null || node.getParent() != this)
      throw new IllegalArgumentException();

    int index = getIndex(node) - 1;

    if (index < 0)
      return null;

    return getChildAt(index);
  }

  /**
   * Returns <code>true</code> if this tree node and <code>node</code> share
   * the same parent.  If <code>node</code> is this tree node, the method
   * returns <code>true</code> and if <code>node</code> is <code>null</code>
   * this method returns <code>false</code>.
   *
   * @param node  the node (<code>null</code> permitted).
   *
   * @return A boolean.
   */
  public boolean isNodeSibling(TreeNode node)
  {
    if (node == null)
      return false;
    if (node == this)
      return true;
    return node.getParent() == getParent() && getParent() != null;
  }

  /**
   * Returns the number of siblings for this tree node.  If the tree node has
   * a parent, this method returns the child count for the parent, otherwise
   * it returns <code>1</code>.
   *
   * @return The sibling count.
   */
  public int getSiblingCount()
  {
    if (parent == null)
      return 1;

    return parent.getChildCount();
  }

  /**
   * Returns the next sibling for this tree node.  If this node has no parent,
   * or this node is the last child of its parent, this method returns
   * <code>null</code>.
   *
   * @return The next sibling, or <code>null</code>.
   */
  public DefaultMutableTreeNode getNextSibling()
  {
    if (parent == null)
      return null;

    int index = parent.getIndex(this) + 1;

    if (index == parent.getChildCount())
      return null;

    return (DefaultMutableTreeNode) parent.getChildAt(index);
  }

  /**
   * Returns the previous sibling for this tree node.  If this node has no
   * parent, or this node is the first child of its parent, this method returns
   * <code>null</code>.
   *
   * @return The previous sibling, or <code>null</code>.
   */
  public DefaultMutableTreeNode getPreviousSibling()
  {
    if (parent == null)
      return null;

    int index = parent.getIndex(this) - 1;

    if (index < 0)
      return null;

    return (DefaultMutableTreeNode) parent.getChildAt(index);
  }

  /**
   * Returns <code>true</code> if this tree node is a lead node (that is, it
   * has no children), and <code>false</otherwise>.
   *
   * @return A boolean.
   */
  public boolean isLeaf()
  {
    return children.size() == 0;
  }

  /**
   * Returns the first leaf node that is a descendant of this node.  Recall
   * that a node is its own descendant, so if this node has no children then
   * it is returned as the first leaf.
   *
   * @return The first leaf node.
   */
  public DefaultMutableTreeNode getFirstLeaf()
  {
    TreeNode current = this;

    while (current.getChildCount() > 0)
      current = current.getChildAt(0);

    return (DefaultMutableTreeNode) current;
  }

  /**
   * Returns the last leaf node that is a descendant of this node.  Recall
   * that a node is its own descendant, so if this node has no children then
   * it is returned as the last leaf.
   *
   * @return The first leaf node.
   */
  public DefaultMutableTreeNode getLastLeaf()
  {
    TreeNode current = this;
    int size = current.getChildCount();

    while (size > 0)
      {
        current = current.getChildAt(size - 1);
        size = current.getChildCount();
      }

    return (DefaultMutableTreeNode) current;
  }

  /**
   * Returns the next leaf node after this tree node.
   *
   * @return The next leaf node, or <code>null</code>.
   */
  public DefaultMutableTreeNode getNextLeaf()
  {
    // if there is a next sibling, return its first leaf
    DefaultMutableTreeNode sibling = getNextSibling();
    if (sibling != null)
      return sibling.getFirstLeaf();
    // otherwise move up one level and try again...
    if (parent != null)
      return ((DefaultMutableTreeNode) parent).getNextLeaf();
    return null;
  }

  /**
   * Returns the previous leaf node before this tree node.
   *
   * @return The previous leaf node, or <code>null</code>.
   */
  public DefaultMutableTreeNode getPreviousLeaf()
  {
    // if there is a previous sibling, return its last leaf
    DefaultMutableTreeNode sibling = getPreviousSibling();
    if (sibling != null)
      return sibling.getLastLeaf();
    // otherwise move up one level and try again...
    if (parent != null)
      return ((DefaultMutableTreeNode) parent).getPreviousLeaf();
    return null;
  }

  /**
   * getLeafCount
   *
   * @return int
   */
  public int getLeafCount()
  {
    int count = 0;
    Enumeration<?> e = depthFirstEnumeration();

    while (e.hasMoreElements())
      {
        TreeNode current = (TreeNode) e.nextElement();

        if (current.isLeaf())
          count++;
      }

    return count;
  }

  /** Provides an enumeration of a tree in breadth-first traversal
   * order.
   */
  static class BreadthFirstEnumeration implements Enumeration<TreeNode>
  {

      LinkedList<TreeNode> queue = new LinkedList<TreeNode>();

      BreadthFirstEnumeration(TreeNode node)
      {
          queue.add(node);
      }

      public boolean hasMoreElements()
      {
          return !queue.isEmpty();
      }

      public TreeNode nextElement()
      {
          if (queue.isEmpty())
              throw new NoSuchElementException("No more elements left.");

          TreeNode node = queue.removeFirst();

	  @SuppressWarnings("unchecked")
          Enumeration<TreeNode> children =
            (Enumeration<TreeNode>) node.children();
          while (children.hasMoreElements())
              queue.add(children.nextElement());

          return node;
      }
  }

  /** Provides an enumeration of a tree traversing it
   * preordered.
   */
  static class PreorderEnumeration implements Enumeration<TreeNode>
  {
          TreeNode next;

      Stack<Enumeration<TreeNode>> childrenEnums =
        new Stack<Enumeration<TreeNode>>();

      PreorderEnumeration(TreeNode node)
      {
          next = node;
	  @SuppressWarnings("unchecked")
	      Enumeration<TreeNode> children =
	      (Enumeration<TreeNode>) node.children();
          childrenEnums.push(children);
      }

      public boolean hasMoreElements()
      {
          return next != null;
      }

      public TreeNode nextElement()
      {
          if (next == null)
              throw new NoSuchElementException("No more elements left.");

          TreeNode current = next;

          Enumeration<TreeNode> children = childrenEnums.peek();

          // Retrieves the next element.
          next = traverse(children);

          return current;
      }

      private TreeNode traverse(Enumeration<TreeNode> children)
      {
          // If more children are available step down.
          if (children.hasMoreElements())
          {
              TreeNode child = children.nextElement();
	      @SuppressWarnings("unchecked")
		  Enumeration<TreeNode> grandchildren =
		  (Enumeration<TreeNode>) child.children();
              childrenEnums.push(grandchildren);

              return child;
          }

          // If no children are left, we return to a higher level.
          childrenEnums.pop();

          // If there are no more levels left, there is no next
          // element to return.
          if (childrenEnums.isEmpty())
              return null;
          else
          {
              return traverse(childrenEnums.peek());
          }
      }
   }

  /** Provides an enumeration of a tree traversing it
   * postordered (= depth-first).
   */
   static class PostorderEnumeration implements Enumeration<TreeNode>
   {

       Stack<TreeNode> nodes = new Stack<TreeNode>();
       Stack<Enumeration<TreeNode>> childrenEnums =
         new Stack<Enumeration<TreeNode>>();

       PostorderEnumeration(TreeNode node)
       {
           nodes.push(node);
	   @SuppressWarnings("unchecked")
	       Enumeration<TreeNode> children =
	       (Enumeration<TreeNode>) node.children();
           childrenEnums.push(children);
       }

       public boolean hasMoreElements()
       {
           return !nodes.isEmpty();
       }

       public TreeNode nextElement()
       {
           if (nodes.isEmpty())
               throw new NoSuchElementException("No more elements left!");

           Enumeration<TreeNode> children = childrenEnums.peek();

           return traverse(children);
       }

       private TreeNode traverse(Enumeration<TreeNode> children)
       {
           if (children.hasMoreElements())
           {
               TreeNode node = children.nextElement();
               nodes.push(node);

	       @SuppressWarnings("unchecked")
		   Enumeration<TreeNode> newChildren =
		   (Enumeration<TreeNode>) node.children();
               childrenEnums.push(newChildren);

               return traverse(newChildren);
           }
           else
           {
               childrenEnums.pop();

               // Returns the node whose children
               // have all been visited. (= postorder)
               TreeNode next = nodes.peek();
               nodes.pop();

               return next;
           }
       }

    }

}
