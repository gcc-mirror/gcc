/* DefaultTreeModel.java --
   Copyright (C) 2002 Free Software Foundation, Inc.

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

// Imports
import java.io.*;
import java.util.*;
import javax.swing.event.*;

/**
 * DefaultTreeModel
 * @author Andrew Selkirk
 */
public class DefaultTreeModel implements Serializable, TreeModel {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

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


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultTreeModel
	 * @param value0 TODO
	 */
	public DefaultTreeModel(TreeNode root) {
		setRoot(root);
	} // DefaultTreeModel()

	/**
	 * Constructor DefaultTreeModel
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultTreeModel(TreeNode root, boolean asksAllowsChildren) {
		setRoot(root);
		this.asksAllowsChildren = asksAllowsChildren;
	} // DefaultTreeModel()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * writeObject
	 * @param value0 TODO
	 * @exception IOException TODO
	 */
	private void writeObject(ObjectOutputStream value0) throws IOException {
		// TODO
	} // writeObject()

	/**
	 * readObject
	 * @param value0 TODO
	 * @exception IOException TODO
	 * @exception ClassNotFoundException TODO
	 */
	private void readObject(ObjectInputStream value0) throws IOException, ClassNotFoundException {
		// TODO
	} // readObject()

	/**
	 * asksAllowsChildren
	 * @returns boolean
	 */
	public boolean asksAllowsChildren() {
		return asksAllowsChildren;
	} // asksAllowsChildren()

	/**
	 * setAsksAllowsChildren
	 * @param value0 TODO
	 */
	public void setAsksAllowsChildren(boolean value) {
		asksAllowsChildren = value; // TODO
	} // setAsksAllowsChildren()

	/**
	 * setRoot
	 * @param value0 TODO
	 */
	public void setRoot(TreeNode root) {

		// Sanity Check
		if (root == null) {
			throw new IllegalArgumentException("null root");
		} // if

		// Set new root
		this.root = root;

		// TODO
	} // setRoot()

	/**
	 * getRoot
	 * @returns Object
	 */
	public Object getRoot() {
		return root;
	} // getRoot()

	/**
	 * getIndexOfChild
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns int
	 */
	public int getIndexOfChild(Object parent, Object child) {
		return 0; // TODO
	} // getIndexOfChild()

	/**
	 * getChild
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns Object
	 */
	public Object getChild(Object value0, int value1) {
		return null; // TODO
	} // getChild()

	/**
	 * getChildCount
	 * @param value0 TODO
	 * @returns int
	 */
	public int getChildCount(Object value0) {
		return 0; // TODO
	} // getChildCount()

	/**
	 * isLeaf
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isLeaf(Object value0) {
		return false; // TODO
	} // isLeaf()

	/**
	 * reload
	 */
	public void reload() {
		// TODO
	} // reload()

	/**
	 * reload
	 * @param value0 TODO
	 */
	public void reload(TreeNode value0) {
		// TODO
	} // reload()

	/**
	 * valueForPathChanged
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void valueForPathChanged(TreePath value0, Object value1) {
		// TODO
	} // valueForPathChanged()

	/**
	 * insertNodeInto
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void insertNodeInto(MutableTreeNode value0, MutableTreeNode value1, int value2) {
		// TODO
	} // insertNodeInto()

	/**
	 * removeNodeFromParent
	 * @param value0 TODO
	 */
	public void removeNodeFromParent(MutableTreeNode value0) {
		// TODO
	} // removeNodeFromParent()

	/**
	 * nodeChanged
	 * @param value0 TODO
	 */
	public void nodeChanged(TreeNode value0) {
		// TODO
	} // nodeChanged()

	/**
	 * nodesWereInserted
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void nodesWereInserted(TreeNode value0, int[] value1) {
		// TODO
	} // nodesWereInserted()

	/**
	 * nodesWereRemoved
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 */
	public void nodesWereRemoved(TreeNode value0, int[] value1, Object[] value2) {
		// TODO
	} // nodesWereRemoved()

	/**
	 * nodesChanged
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void nodesChanged(TreeNode value0, int[] value1) {
		// TODO
	} // nodesChanged()

	/**
	 * nodeStructureChanged
	 * @param value0 TODO
	 */
	public void nodeStructureChanged(TreeNode value0) {
		// TODO
	} // nodeStructureChanged()

	/**
	 * getPathToRoot
	 * @param value0 TODO
	 * @returns TreeNode[]
	 */
	public TreeNode[] getPathToRoot(TreeNode value0) {
		return null; // TODO
	} // getPathToRoot()

	/**
	 * getPathToRoot
	 * @param value0 TODO
	 * @param value1 TODO
	 * @returns TreeNode[]
	 */
	protected TreeNode[] getPathToRoot(TreeNode value0, int value1) {
		return null; // TODO
	} // getPathToRoot()

	/**
	 * addTreeModelListener
	 * @param value0 TODO
	 */
	public void addTreeModelListener(TreeModelListener listener) {
		listenerList.add(TreeModelListener.class, listener);
	} // addTreeModelListener()

	/**
	 * removeTreeModelListener
	 * @param value0 TODO
	 */
	public void removeTreeModelListener(TreeModelListener listener) {
		listenerList.remove(TreeModelListener.class, listener);
	} // removeTreeModelListener()

	/**
	 * fireTreeNodesChanged
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 */
	protected void fireTreeNodesChanged(Object value0, Object[] value1, int[] value2, Object[] value3) {
		// TODO
	} // fireTreeNodesChanged()

	/**
	 * fireTreeNodesInserted
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 */
	protected void fireTreeNodesInserted(Object value0, Object[] value1, int[] value2, Object[] value3) {
		// TODO
	} // fireTreeNodesInserted()

	/**
	 * fireTreeNodesRemoved
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 */
	protected void fireTreeNodesRemoved(Object value0, Object[] value1, int[] value2, Object[] value3) {
		// TODO
	} // fireTreeNodesRemoved()

	/**
	 * fireTreeStructureChanged
	 * @param value0 TODO
	 * @param value1 TODO
	 * @param value2 TODO
	 * @param value3 TODO
	 */
	protected void fireTreeStructureChanged(Object value0, Object[] value1, int[] value2, Object[] value3) {
		// TODO
	} // fireTreeStructureChanged()

	/**
	 * getListeners
	 * @param value0 TODO
	 * @returns EventListener[]
	 */
	public EventListener[] getListeners(Class classType) {
		return listenerList.getListeners(classType);
	} // getListeners()


} // DefaultTreeModel
