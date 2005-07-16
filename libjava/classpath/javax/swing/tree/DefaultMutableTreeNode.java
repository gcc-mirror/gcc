/* DefaultMutableTreeNode.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.Stack;
import java.util.Vector;


/**
 * DefaultMutableTreeNode
 *
 * @author Andrew Selkirk
 * @author Robert Schuster (robertschuster@fsfe.org)
 */
public class DefaultMutableTreeNode
  implements Cloneable, MutableTreeNode, Serializable
{
  private static final long serialVersionUID = -4298474751201349152L;

  /**
   * EMPTY_ENUMERATION
   */
  public static final Enumeration EMPTY_ENUMERATION =
    EmptyEnumeration.getInstance();

  /**
   * parent
   */
  protected MutableTreeNode parent;

  /**
   * children
   */
  protected Vector children = new Vector();

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
   * This node allows to add child nodes.
   */
  public DefaultMutableTreeNode()
  {
    this(null, true);
  }

  /**
   * Creates a <code>DefaultMutableTreeNode</code> object with the given
   * user object attached to it. This node allows to add child nodes.
   *
   * @param userObject the user object
   */
  public DefaultMutableTreeNode(Object userObject)
  {
    this(userObject, true);
  }

  /**
   * Creates a <code>DefaultMutableTreeNode</code> object with the given
   * user object attached to it.
   *
   * @param userObject the user object
   * @param allowsChildren <code>true</code> if the code allows to add child
   * nodes, <code>false</code> otherwise
   */
  public DefaultMutableTreeNode(Object userObject, boolean allowsChildren)
  {
    this.userObject = userObject;
    this.allowsChildren = allowsChildren;
  }

  /**
   * clone
   *
   * @return Object
   */
  public Object clone()
  {
    try
      {
        return super.clone();
        // TODO: Do we need to do more here ?
      }
    catch (CloneNotSupportedException e)
      {
        // This never happens.
        return null;
      }
  }

  /**
   * Returns a string representation of this node
   *
   * @return a human-readable String representing this node
   */
  public String toString()
  {
    if (userObject == null)
      return null;

    return userObject.toString();
  }

  /**
   * Adds a new child node to this node.
   *
   * @param child the child node
   *
   * @throws IllegalArgumentException if <code>child</code> is null
   * @throws IllegalStateException if the node does not allow children
   */
  public void add(MutableTreeNode child)
  {
    if (child == null)
      throw new IllegalArgumentException();

    if (! allowsChildren)
      throw new IllegalStateException();
    
    children.add(child);
    child.setParent(this);
  }

  /**
   * Returns the parent node of this node.
   *
   * @return the parent node
   */
  public TreeNode getParent()
  {
    return parent;
  }

  /**
   * Removes the child with the given index from this node
   *
   * @param index the index
   */
  public void remove(int index)
  {
    children.remove(index);
  }

  /**
   * Removes the given child from this node.
   *
   * @param node the child node
   */
  public void remove(MutableTreeNode node)
  {
    children.remove(node);
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
   * @param node the child node
   * @param index the index.
   */
  public void insert(MutableTreeNode node, int index)
  {
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
    return (TreeNode) children.elementAt(index);
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
   * Returns the child index for a given node.
   *
   * @param node this node
   *
   * @return the index
   */
  public int getIndex(TreeNode node)
  {
    return children.indexOf(node);
  }

  /**
   * setAllowsChildren
   *
   * @param allowsChildren TODO
   */
  public void setAllowsChildren(boolean allowsChildren)
  {
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
    children.removeAllElements();
  }

  /**
   * isNodeAncestor
   *
   * @param node TODO
   *
   * @return boolean
   */
  public boolean isNodeAncestor(TreeNode node)
  {
    if (node == null)
      return false;

    TreeNode current = this;

    while (current != null
           && current != node)
      current = current.getParent();

    return current == node;
  }

  /**
   * isNodeDescendant
   *
   * @param node TODO
   *
   * @return boolean
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
    ArrayList list = new ArrayList();

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

    Stack stack = new Stack();
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
                index = ((Integer) stack.pop()).intValue() + 1;
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
  public Enumeration preorderEnumeration()
  {
    return new PreorderEnumeration(this);
  }

  /**
   * postorderEnumeration
   *
   * @return Enumeration
   */
  public Enumeration postorderEnumeration()
  {
    return new PostorderEnumeration(this);
  }

  /**
   * breadthFirstEnumeration
   *
   * @return Enumeration
   */
  public Enumeration breadthFirstEnumeration()
  {
    return new BreadthFirstEnumeration(this);
  }

  /**
   * depthFirstEnumeration
   *
   * @return Enumeration
   */
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
  public Enumeration pathFromAncestorEnumeration(TreeNode node)
  {
    if (node == null)
      throw new IllegalArgumentException();
    
    TreeNode parent = this;
    Vector nodes = new Vector();
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
   * isNodeChild
   *
   * @param node TODO
   *
   * @return boolean
   */
  public boolean isNodeChild(TreeNode node)
  {
    if (node == null)
      return false;

    return node.getParent() == this;
  }

  /**
   * getFirstChild
   *
   * @return TreeNode
   */
  public TreeNode getFirstChild()
  {
    return (TreeNode) children.firstElement();
  }

  /**
   * getLastChild
   *
   * @return TreeNode
   */
  public TreeNode getLastChild()
  {
    return (TreeNode) children.lastElement();
  }

  /**
   * getChildAfter
   *
   * @param node TODO
   *
   * @return TreeNode
   */
  public TreeNode getChildAfter(TreeNode node)
  {
    if (node == null
        || node.getParent() != this)
      throw new IllegalArgumentException();

    int index = getIndex(node) + 1;

    if (index == getChildCount())
      return null;

    return getChildAt(index);
  }

  /**
   * getChildBefore
   *
   * @param node TODO
   *
   * @return TreeNode
   */
  public TreeNode getChildBefore(TreeNode node)
  {
    if (node == null
        || node.getParent() != this)
      throw new IllegalArgumentException();

    int index = getIndex(node) - 1;

    if (index < 0)
      return null;

    return getChildAt(index);
  }

  /**
   * isNodeSibling
   *
   * @param node TODO
   *
   * @return boolean
   */
  public boolean isNodeSibling(TreeNode node)
  {
    if (node == null)
      return false;

    return (node.getParent() == getParent()
            && getParent() != null);
  }

  /**
   * getSiblingCount
   *
   * @return int
   */
  public int getSiblingCount()
  {
    if (parent == null)
      return 1;

    return parent.getChildCount();
  }

  /**
   * getNextSibling
   *
   * @return DefaultMutableTreeNode
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
   * getPreviousSibling
   *
   * @return DefaultMutableTreeNode
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
   * isLeaf
   *
   * @return boolean
   */
  public boolean isLeaf()
  {
    return children.size() == 0;
  }

  /**
   * getFirstLeaf
   *
   * @return DefaultMutableTreeNode
   */
  public DefaultMutableTreeNode getFirstLeaf()
  {
    TreeNode current = this;
    
    while (current.getChildCount() > 0)
      current = current.getChildAt(0);

    return (DefaultMutableTreeNode) current;
  }

  /**
   * getLastLeaf
   *
   * @return DefaultMutableTreeNode
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
   * getNextLeaf
   *
   * @return DefaultMutableTreeNode
   */
  public DefaultMutableTreeNode getNextLeaf()
  {
    if (parent == null)
      return null;

    // TODO: Fix implementation.
    return null;
    //return parent.getChildAfter(this);
  }

  /**
   * getPreviousLeaf
   *
   * @return DefaultMutableTreeNode
   */
  public DefaultMutableTreeNode getPreviousLeaf()
  {
    if (parent == null)
      return null;

    // TODO: Fix implementation.
    return null;
    //return parent.getChildBefore(this);
  }

  /**
   * getLeafCount
   *
   * @return int
   */
  public int getLeafCount()
  {
    int count = 0;
    Enumeration e = depthFirstEnumeration();

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
  static class BreadthFirstEnumeration implements Enumeration
  {

      LinkedList queue = new LinkedList();

      BreadthFirstEnumeration(TreeNode node)
      {
          queue.add(node);
      }

      public boolean hasMoreElements()
      {
          return !queue.isEmpty();
      }

      public Object nextElement()
      {
          if(queue.isEmpty())
              throw new NoSuchElementException("No more elements left.");

          TreeNode node = (TreeNode) queue.removeFirst();

          Enumeration children = node.children();
          while (children.hasMoreElements())
              queue.add(children.nextElement());

          return node;
      }
  }

  /** Provides an enumeration of a tree traversing it
   * preordered.
   */
  static class PreorderEnumeration implements Enumeration
  {
	  TreeNode next;

      Stack childrenEnums = new Stack();

      PreorderEnumeration(TreeNode node)
      {
          next = node;
          childrenEnums.push(node.children());
      }

      public boolean hasMoreElements()
      {
          return next != null;
      }

      public Object nextElement()
      {
          if( next == null )
              throw new NoSuchElementException("No more elements left.");

          Object current = next;

          Enumeration children = (Enumeration) childrenEnums.peek();

          // Retrieves the next element.
          next = traverse(children);

          return current;
      }

      private TreeNode traverse(Enumeration children)
      {
          // If more children are available step down.
          if( children.hasMoreElements() )
          {
              TreeNode child = (TreeNode) children.nextElement();
              childrenEnums.push(child.children());

              return child;
          }
          
          // If no children are left, we return to a higher level.
          childrenEnums.pop();

          // If there are no more levels left, there is no next
          // element to return.
          if ( childrenEnums.isEmpty() )
              return null;
          else
          {
              return traverse((Enumeration) childrenEnums.peek());
          }
      }
   }

  /** Provides an enumeration of a tree traversing it
   * postordered (= depth-first).
   */
   static class PostorderEnumeration implements Enumeration
   {

       Stack nodes = new Stack();
       Stack childrenEnums = new Stack();

       PostorderEnumeration(TreeNode node)
       {
           nodes.push(node);
           childrenEnums.push(node.children());
       }

       public boolean hasMoreElements()
       {
           return !nodes.isEmpty();
       }

       public Object nextElement()
       {
           if( nodes.isEmpty() )
               throw new NoSuchElementException("No more elements left!");

           Enumeration children = (Enumeration) childrenEnums.peek();

           return traverse(children);
       }

       private Object traverse(Enumeration children)
       {
           if ( children.hasMoreElements() )
           {
               TreeNode node = (TreeNode) children.nextElement();
               nodes.push(node);

               Enumeration newChildren = node.children();
               childrenEnums.push(newChildren);

               return traverse(newChildren);
           }
           else
           {
               childrenEnums.pop();

               // Returns the node whose children
               // have all been visited. (= postorder)
               Object next = nodes.peek();
               nodes.pop();

               return next;
           }
       }

    }

}
