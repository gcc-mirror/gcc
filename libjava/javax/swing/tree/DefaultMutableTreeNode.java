/* DefaultMutableTreeNode.java --
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

/**
 * DefaultMutableTreeNode
 * @author Andrew Selkirk
 */
public class DefaultMutableTreeNode implements Cloneable, MutableTreeNode, Serializable {

	//-------------------------------------------------------------
	// Variables --------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * EMPTY_ENUMERATION
	 */
	public static final Enumeration EMPTY_ENUMERATION = null; // TODO

	/**
	 * parent
	 */
	protected MutableTreeNode parent = null;

	/**
	 * children
	 */
	protected Vector children = new Vector();

	/**
	 * userObject
	 */
	protected transient Object userObject = "";

	/**
	 * allowsChildren
	 */
	protected boolean allowsChildren = true;


	//-------------------------------------------------------------
	// Initialization ---------------------------------------------
	//-------------------------------------------------------------

	/**
	 * Constructor DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode() {
		// TODO
	} // DefaultMutableTreeNode()

	/**
	 * Constructor DefaultMutableTreeNode
	 * @param value0 TODO
	 */
	public DefaultMutableTreeNode(Object userObject) {
		this.userObject = userObject;
	} // DefaultMutableTreeNode()

	/**
	 * Constructor DefaultMutableTreeNode
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public DefaultMutableTreeNode(Object userObject, boolean allowsChildren) {
		this.userObject = userObject;
		this.allowsChildren = allowsChildren;
	} // DefaultMutableTreeNode()


	//-------------------------------------------------------------
	// Methods ----------------------------------------------------
	//-------------------------------------------------------------

	/**
	 * clone
	 * @returns Object
	 */
	public Object clone() {
		return null; // TODO
	} // clone()

	/**
	 * toString
	 * @returns String
	 */
	public String toString() {
		if (userObject == null) {
			return null;
		} // if
		return userObject.toString();
	} // toString()

	/**
	 * add
	 * @param value0 TODO
	 */
	public void add(MutableTreeNode child) {
		children.add(child);
		child.setParent(this);
	} // add()

	/**
	 * getParent
	 * @returns TreeNode
	 */
	public TreeNode getParent() {
		return parent;
	} // getParent()

	/**
	 * remove
	 * @param value0 TODO
	 */
	public void remove(int index) {
		children.remove(index);
	} // remove()

	/**
	 * remove
	 * @param value0 TODO
	 */
	public void remove(MutableTreeNode node) {
		children.remove(node);
	} // remove()

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
	 * insert
	 * @param value0 TODO
	 * @param value1 TODO
	 */
	public void insert(MutableTreeNode node, int index) {
		children.insertElementAt(node, index);
	} // insert()

	/**
	 * getPath
	 * @returns TreeNode[]
	 */
	public TreeNode[] getPath() {

		// Variables
		TreeNode[]	path;
		int			size;
		int			index;
		TreeNode	current;

		// Determine length of Path
		size = getLevel() + 1;

		// Create Path
		path = new TreeNode[size];
		current = this;
		for (index = size - 1; index >= 0; index--) {
			path[index] = current;
			current = current.getParent();
		} // for

		// Return Path
		return path;

	} // getPath()

	/**
	 * children
	 * @returns Enumeration
	 */
	public Enumeration children() {
		return children.elements();
	} // children()

	/**
	 * setParent
	 * @param value0 TODO
	 */
	public void setParent(MutableTreeNode node) {
		parent = node;
	} // setParent()

	/**
	 * getChildAt
	 * @param value0 TODO
	 * @returns TreeNode
	 */
	public TreeNode getChildAt(int index) {
		return (TreeNode) children.elementAt(index);
	} // getChildAt()

	/**
	 * getChildCount
	 * @returns int
	 */
	public int getChildCount() {
		return children.size();
	} // getChildCount()

	/**
	 * getIndex
	 * @param value0 TODO
	 * @returns int
	 */
	public int getIndex(TreeNode node) {
		return children.indexOf(node);
	} // getIndex()

	/**
	 * setAllowsChildren
	 * @param value0 TODO
	 */
	public void setAllowsChildren(boolean allowsChildren) {
		this.allowsChildren = allowsChildren;
	} // setAllowsChildren()

	/**
	 * getAllowsChildren
	 * @returns boolean
	 */
	public boolean getAllowsChildren() {
		return allowsChildren;
	} // getAllowsChildren()

	/**
	 * setUserObject
	 * @param value0 TODO
	 */
	public void setUserObject(Object userObject) {
		this.userObject = userObject;
	} // setUserObject()

	/**
	 * getUserObject
	 * @returns Object
	 */
	public Object getUserObject() {
		return userObject;
	} // getUserObject()

	/**
	 * removeFromParent
	 */
	public void removeFromParent() {
		parent = null;
		// TODO
	} // removeFromParent()

	/**
	 * removeAllChildren
	 */
	public void removeAllChildren() {
		children.removeAllElements();
	} // removeAllChildren()

	/**
	 * isNodeAncestor
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isNodeAncestor(TreeNode node) {

		// Variables
		TreeNode	current;

		// Sanity Check
		if (node == null) {
			return false;
		} // if

		// Search For Ancestor
		current = this;
		while (current != null && current != node) {
			current = current.getParent();
		} // while

		// Check for Ancestor
		if (current == node) {
			return true;
		} // if

		// Otherwise, no
		return false;

	} // isNodeAncestor()

	/**
	 * isNodeDescendant
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isNodeDescendant(DefaultMutableTreeNode node) {

		// Variables
		TreeNode	current;

		// Sanity Check
		if (node == null) {
			return false;
		} // if

		// Search For Descendant
		current = node;
		while (current != null && current != this) {
			current = current.getParent();
		} // while

		// Check for Descendant
		if (current == this) {
			return true;
		} // if

		// Otherwise, no
		return false;

	} // isNodeDescendant()

	/**
	 * getSharedAncestor
	 * @param value0 TODO
	 * @returns TreeNode
	 */
	public TreeNode getSharedAncestor(DefaultMutableTreeNode node) {

		// Variables
		ArrayList	list;
		TreeNode	current;

		// Get List of Path Elements for this node
		current = this;
		list = new ArrayList();
		while (current != null) {
			list.add(current);
			current = current.getParent();
		} // while

		// Check if any path element of node are in list
		current = node;
		while (current != null) {
			if (list.contains(current) == true) {
				return current;
			} // if
			current = current.getParent();
		} // while

		// Unable to locate shared ancestor
		return null;

	} // getSharedAncestor()

	/**
	 * isNodeRelated
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isNodeRelated(DefaultMutableTreeNode node) {

		// Sanity Check
		if (node == null) {
			return false;
		} // if

		// Check for the same root
		if (node.getRoot() == getRoot()) {
			return true;
		} // if

		// Nodes are not related
		return false;

	} // isNodeRelated()

	/**
	 * getDepth
	 * @returns int
	 */
	public int getDepth() {

		// Variables
		TreeNode		node;
		int				depth;
		int				current;
		int				size;
		Stack			stack;
		int				index;

		// Check for children
		if (allowsChildren == false || children.size() == 0) {
			return 0;
		} // if

		// Process Depths
		stack = new Stack();
		stack.push(new Integer(0));
		node = getChildAt(0);
//System.out.println("  * Descend: 0-0");
		depth = 0;
		current = 1;
		while (stack.empty() == false) {

			// Check if node has children
			if (node.getChildCount() != 0) {
				node = node.getChildAt(0);
				stack.push(new Integer(0));
				current++;
//				System.out.println("  * Descend: 0-" + current);

			// Check for next sibling
			} else {

				// Check Depth
				if (current > depth) {
					depth = current;
				} // if

				do {

					// Traverse to Parent
					node = node.getParent();
					size = node.getChildCount();
					current--;
					index = ((Integer) stack.pop()).intValue();
//					System.out.println("  * Ascend from: " + index + "-" + current);
					index++;

				} while (index >= size && node != this);

				// Check for child
				if (index < size) {
					node = node.getChildAt(index);
					stack.push(new Integer(index));
					current++;
//					System.out.println("  * Descend: " + index + "-" + current);
				} // if

			} // if

		} // while

		return depth;

	} // getDepth()

	static Random	random = new Random(System.currentTimeMillis());

	public static void growTree(DefaultMutableTreeNode root) {

		// Variables
		int						size;
		int						index;
		DefaultMutableTreeNode	node;
		DefaultMutableTreeNode	current;

		current = root;
		index = 0;
//		while (current != root) {
		do {

//			if (random.nextInt(3) < 2) {
			if (random.nextBoolean()) {
				node = new DefaultMutableTreeNode(String.valueOf(index));
				index++;
				current.add(node);
				current = node;
			} else {
				current = (DefaultMutableTreeNode) current.getParent();
			} // if

//		} // while
		} while (current != root && current != null);

		System.out.println("Number of nodes: " + index);

/*
		// Calc # children
		size = random.nextInt(4);

		for (index = 0; index < size; index++) {

			// Create Node
			node = new DefaultMutableTreeNode(String.valueOf(index));
			growTree(node);

			// Add Node to root
			root.add(node);

		} // for
*/
	} // growTree()

	public static void main(String[] argv) {
/*
		DefaultMutableTreeNode	node1 = new DefaultMutableTreeNode("node1");
		DefaultMutableTreeNode	node2 = new DefaultMutableTreeNode("node2");
		DefaultMutableTreeNode	node3 = new DefaultMutableTreeNode("node3");
		DefaultMutableTreeNode	node4 = new DefaultMutableTreeNode("node4");
		DefaultMutableTreeNode	node5 = new DefaultMutableTreeNode("node5");
		DefaultMutableTreeNode	node6 = new DefaultMutableTreeNode("node6");
		DefaultMutableTreeNode	node7 = new DefaultMutableTreeNode("node7");
		DefaultMutableTreeNode	node8 = new DefaultMutableTreeNode("node8");

		node1.add(node2);
		node1.add(node3);
		node2.add(node4);
		node2.add(node5);
		node3.add(node6);
		node3.add(node7);
		node5.add(node8);

		System.out.println("Depth (node1): " + node1.getDepth());
		System.out.println("Depth (node2): " + node2.getDepth());
		System.out.println("Depth (node3): " + node3.getDepth());
*/

		System.out.println("Create tree...");
		DefaultMutableTreeNode	root = new DefaultMutableTreeNode("root");
		growTree(root);
		System.out.println("Find depth...");
		System.out.println("Depth (root): " + root.getDepth());

	} // main

	/**
	 * getLevel
	 * @returns int
	 */
	public int getLevel() {

		// Variables
		TreeNode	current;
		int			count;

		// Lookup Parent
		count = -1;
		current = this;
		do {
			current = current.getParent();
			count++;
		} while (current != null);

		return count;

	} // getLevel()

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
	 * getUserObjectPath
	 * @returns Object[]
	 */
	public Object[] getUserObjectPath() {

		// Variables
		TreeNode[]	path;
		Object[]	object;
		int			size;
		int			index;

		// Get Path for Tree Nodes
		path = getPath();

		// Construct Object Path
		object = new Object[path.length];
		for (index = 0; index < path.length; index++) {
			object[index] = ((DefaultMutableTreeNode) path[index]).getUserObject();
		} // for

		// Return Object Path
		return object;

	} // getUserObjectPath()

	/**
	 * getRoot
	 * @returns TreeNode
	 */
	public TreeNode getRoot() {

		// Variables
		TreeNode	current;
		TreeNode	check;

		// Lookup Parent
		current = this;
		check = current.getParent();
		while (check != null) {
			current = check;
			check = current.getParent();
		} // while

		return current;

	} // getRoot()

	/**
	 * isRoot
	 * @returns boolean
	 */
	public boolean isRoot() {
		return (parent == null);
	} // isRoot()

	/**
	 * getNextNode
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getNextNode() {
		return null; // TODO
	} // getNextNode()

	/**
	 * getPreviousNode
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getPreviousNode() {
		return null; // TODO
	} // getPreviousNode()

	/**
	 * preorderEnumeration
	 * @returns Enumeration
	 */
	public Enumeration preorderEnumeration() {
		return null; // TODO
	} // preorderEnumeration()

	/**
	 * postorderEnumeration
	 * @returns Enumeration
	 */
	public Enumeration postorderEnumeration() {
		return null; // TODO
	} // postorderEnumeration()

	/**
	 * breadthFirstEnumeration
	 * @returns Enumeration
	 */
	public Enumeration breadthFirstEnumeration() {
		return null; // TODO
	} // breadthFirstEnumeration()

	/**
	 * depthFirstEnumeration
	 * @returns Enumeration
	 */
	public Enumeration depthFirstEnumeration() {
		return null; // TODO
	} // depthFirstEnumeration()

	/**
	 * pathFromAncestorEnumeration
	 * @param value0 TODO
	 * @returns Enumeration
	 */
	public Enumeration pathFromAncestorEnumeration(TreeNode value0) {
		return null; // TODO
	} // pathFromAncestorEnumeration()

	/**
	 * isNodeChild
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isNodeChild(TreeNode node) {

		// Variables
		TreeNode	current;
		int			index;

		// Sanity Check
		if (node == null) {
			return false;
		} // if

		// Process Path
		current = node;
		while (current != null) {
			if (current == this) {
				return true;
			} // if
			current = current.getParent();
		} // while

		// Node not located in path, not child
		return false;

	} // isNodeChild()

	/**
	 * getFirstChild
	 * @returns TreeNode
	 */
	public TreeNode getFirstChild() {
		return (TreeNode) children.firstElement();
	} // getFirstChild()

	/**
	 * getLastChild
	 * @returns TreeNode
	 */
	public TreeNode getLastChild() {
		return (TreeNode) children.lastElement();
	} // getLastChild()

	/**
	 * getChildAfter
	 * @param value0 TODO
	 * @returns TreeNode
	 */
	public TreeNode getChildAfter(TreeNode node) {

		// Variables
		int		index;

		// Check node
		if (node == null || node.getParent() != this) {
			throw new IllegalArgumentException();
		} // if

		// Get index of child node
		index = getIndex(node);

		// Check for child after
		index++;
		if (index == getChildCount()) {
			return null;
		} // if

		// Retrieve Child After
		return getChildAt(index);

	} // getChildAfter()

	/**
	 * getChildBefore
	 * @param value0 TODO
	 * @returns TreeNode
	 */
	public TreeNode getChildBefore(TreeNode node) {

		// Variables
		int		index;

		// Check node
		if (node == null || node.getParent() != this) {
			throw new IllegalArgumentException();
		} // if

		// Get index of child node
		index = getIndex(node);

		// Check for child before
		index--;
		if (index < 0) {
			return null;
		} // if

		// Retrieve Child Before
		return getChildAt(index);

	} // getChildBefore()

	/**
	 * isNodeSibling
	 * @param value0 TODO
	 * @returns boolean
	 */
	public boolean isNodeSibling(TreeNode node) {

		// Variables
		int			index;

		// Check for null
		if (node == null) {
			return false;
		} // if

		// Check if nodes share a parent
		if (node.getParent() == getParent() && getParent() != null) {
			return true;
		} // if

		// Nodes are not siblings
		return false;

	} // isNodeSibling()

	/**
	 * getSiblingCount
	 * @returns int
	 */
	public int getSiblingCount() {

		// Variables

		// Check for no parent
		if (parent == null) {
			return 1;
		} // if

		// Calculate sibling count from parent's child count
		return parent.getChildCount();

	} // getSiblingCount()

	/**
	 * getNextSibling
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getNextSibling() {

		// Variables
		int		index;
		int		size;

		// Check for Parent
		if (parent == null) {
			return null;
		} // if

		// Get Index of this node
		index = parent.getIndex(this);

		// Check for Next Sibling
		size = parent.getChildCount();
		index++;
		if (index == size) {
			return null;
		} // if

		return (DefaultMutableTreeNode) parent.getChildAt(index);

	} // getNextSibling()

	/**
	 * getPreviousSibling
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getPreviousSibling() {

		// Variables
		int		index;

		// Check for Parent
		if (parent == null) {
			return null;
		} // if

		// Get Index of this node
		index = parent.getIndex(this);

		// Check for Previous Sibling
		index--;
		if (index < 0) {
			return null;
		} // if

		return (DefaultMutableTreeNode) parent.getChildAt(index);

	} // getPreviousSibling()

	/**
	 * isLeaf
	 * @returns boolean
	 */
	public boolean isLeaf() {
		return (children.size() == 0); // TODO: check allowsChildren??
	} // isLeaf()

	/**
	 * getFirstLeaf
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getFirstLeaf() {

		// Variables
		TreeNode	current;

		current = this;
		while (current.getChildCount() > 0) {
			current = current.getChildAt(0);
		} // while

		return (DefaultMutableTreeNode) current;

	} // getFirstLeaf()

	/**
	 * getLastLeaf
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getLastLeaf() {

		// Variables
		TreeNode	current;
		int			size;

		current = this;
		size = current.getChildCount();
		while (size > 0) {
			current = current.getChildAt(size - 1);
			size = current.getChildCount();
		} // while

		return (DefaultMutableTreeNode) current;

	} // getLastLeaf()

	/**
	 * getNextLeaf
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getNextLeaf() {
		return null; // TODO
	} // getNextLeaf()

	/**
	 * getPreviousLeaf
	 * @returns DefaultMutableTreeNode
	 */
	public DefaultMutableTreeNode getPreviousLeaf() {
		return null; // TODO
	} // getPreviousLeaf()

	/**
	 * getLeafCount
	 * @returns int
	 */
	public int getLeafCount() {

		// Variables
		Enumeration	enum;
		int			count;
		TreeNode	current;

		// Get Enumeration of all descendants
		enum = depthFirstEnumeration();

		// Process Nodes
		count = 0;
		while (enum.hasMoreElements() == true) {
			current = (TreeNode) enum.nextElement();
			if (current.isLeaf() == true) {
				count++;
			} // if
		} // if

		return count;

	} // getLeafCount()


} // DefaultMutableTreeNode
