/* DefaultTreeModel.java --
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
	 * @param value0 TODO
	 */
	public DefaultTreeModel(TreeNode root)
	{
		if (root == null)
			root = new DefaultMutableTreeNode();
		setRoot(root);
	}

	/**
	 * Constructor DefaultTreeModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTreeModel(TreeNode root, boolean asksAllowsChildren)
	{
		setRoot(root);
		this.asksAllowsChildren = asksAllowsChildren;
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
	private void readObject(ObjectInputStream value0) throws IOException,
			ClassNotFoundException
	{
		// TODO
	}

	/**
	 * asksAllowsChildren
	 * @return boolean
	 */
	public boolean asksAllowsChildren()
	{
		return asksAllowsChildren;
	}

	/**
	 * setAsksAllowsChildren
	 * @param value0 TODO
	 */
	public void setAsksAllowsChildren(boolean value)
	{
		asksAllowsChildren = value; // TODO
	}

	/**
	 * setRoot
	 * @param value0 TODO
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

		// TODO
	}

	/**
	 * getRoot
	 * @return Object
	 */
	public Object getRoot()
	{
		return root;
	}

	/**
	 * getIndexOfChild
	 * @param value0 TODO
	 * @param value1 TODO
	 * @return int
	 */
	public int getIndexOfChild(Object parent, Object child)
	{
		return 0; // TODO
	}

	/**
	 * getChild
	 * @param value0 TODO
	 * @param value1 TODO
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
	 * @param value0 TODO
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
	 * @param value0 TODO
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
	 * reload
	 */
	public void reload()
	{
		// TODO
	}

	/**
	 * reload
	 * @param value0 TODO
	 */
	public void reload(TreeNode value0)
	{
		// TODO
	}

	/**
	 * valueForPathChanged
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void valueForPathChanged(TreePath value0, Object value1)
	{
		// TODO
	}

	/**
	 * insertNodeInto
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void insertNodeInto(MutableTreeNode value0, MutableTreeNode value1,
			int value2)
	{
		// TODO
	}

	/**
	 * removeNodeFromParent
	 * @param value0 TODO
	 */
	public void removeNodeFromParent(MutableTreeNode value0)
	{
		// TODO
	}

	/**
	 * nodeChanged
	 * @param value0 TODO
	 */
	public void nodeChanged(TreeNode value0)
	{
		// TODO
	}

	/**
	 * nodesWereInserted
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void nodesWereInserted(TreeNode value0, int[] value1)
	{
		// TODO
	}

	/**
	 * nodesWereRemoved
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void nodesWereRemoved(TreeNode value0, int[] value1, Object[] value2)
	{
		// TODO
	}

	/**
	 * nodesChanged
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void nodesChanged(TreeNode value0, int[] value1)
	{
		// TODO
	}

	/**
	 * nodeStructureChanged
	 * @param value0 TODO
	 */
	public void nodeStructureChanged(TreeNode value0)
	{
		// TODO
	}

	/**
	 * getPathToRoot
	 * @param value0 TODO
	 * @return TreeNode[]
	 */
	public TreeNode[] getPathToRoot(TreeNode value0)
	{
		return null; // TODO
	}

	/**
	 * getPathToRoot
	 * @param value0 TODO
	 * @param value1 TODO
	 * @return TreeNode[]
	 */
	protected TreeNode[] getPathToRoot(TreeNode value0, int value1)
	{
		return null; // TODO
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
	 * fireTreeNodesChanged
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
