/* TreeMap.java -- a class providing a basic Red-Black Tree data structure,
   mapping Object --> Object
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;

/**
 * This class provides a red-black tree implementation of the SortedMap
 * interface.  Elements in the Map will be sorted by either a user-provided
 * Comparator object, or by the natural ordering of the keys.
 *
 * The algorithms are adopted from Corman, Leiserson,
 * and Rivest's <i>Introduction to Algorithms.</i>  In other words,
 * I cribbed from the same pseudocode as Sun.  <em>Any similarity
 * between my code and Sun's (if there is any -- I have never looked
 * at Sun's) is a result of this fact.</em>
 *
 * TreeMap guarantees O(log n) insertion and deletion of elements.  That 
 * being said, there is a large enough constant coefficient in front of 
 * that "log n" (overhead involved in keeping the tree 
 * balanced), that TreeMap may not be the best choice for small
 * collections.
 *
 * TreeMap is a part of the JDK1.2 Collections API.  Null keys are allowed
 * only if a Comparator is used which can deal with them.  Null values are 
 * always allowed.
 *
 * @author           Jon Zeppieri
 * @author	     Bryce McKinlay
 */
public class TreeMap extends AbstractMap
  implements SortedMap, Cloneable, Serializable
{
  private static final int RED = -1,
                           BLACK = 1;

  /** Sentinal node, used to avoid null checks for corner cases and make the
      delete rebalance code simpler. Note that this must not be static, due 
      to thread-safety concerns. */
  transient Node nil = new Node(null, null);

  /** The root node of this TreeMap */
  transient Node root = nil;

  /** The size of this TreeMap */
  transient int size = 0;

  /** Number of modifications */
  transient int modCount = 0;

  /** This TreeMap's comparator, if any. */
  Comparator comparator = null;

  static final long serialVersionUID = 919286545866124006L;

  private static class Node extends BasicMapEntry implements Map.Entry
  {
    int color;
    Node left;
    Node right;
    Node parent;

    Node(Object key, Object value)
    {
      super(key, value);
      this.color = BLACK;
    }
  }

  /**
   * Instantiate a new TreeMap with no elements, using the keys'
   * natural ordering to sort.
   *
   * @see java.lang.Comparable
   */
  public TreeMap()
  {
  }

  /**
   * Instantiate a new TreeMap with no elements, using the provided
   * comparator to sort.
   *
   * @param        oComparator        a Comparator object, used to sort 
   *                                  the keys of this SortedMap
   */
  public TreeMap(Comparator c)
  {
    comparator = c;
  }

  /**
   * Instantiate a new TreeMap, initializing it with all of the
   * elements in the provided Map.  The elements will be sorted 
   * using the natural ordering of the keys.
   *
   * @param              map         a Map, whose keys will be put into
   *                                  this TreeMap
   *
   * @throws             ClassCastException     if the keys in the provided
   *                                            Map do not implement 
   *                                            Comparable
   *
   * @see                java.lang.Comparable
   */
  public TreeMap(Map map)
  {
    putAll(map);
  }

  /** 
   * Instantiate a new TreeMap, initializing it with all of the
   * elements in the provided SortedMap.  The elements will be sorted 
   * using the same method as in the provided SortedMap.
   */
  public TreeMap(SortedMap sm)
  {
    this(sm.comparator());

    int sm_size = sm.size();
    Iterator itr = sm.entrySet().iterator();

    fabricateTree(sm_size);
    Node node = firstNode();
    
    for (int i = 0; i < sm_size; i++)
      {
	Map.Entry me = (Map.Entry) itr.next();
	node.key = me.getKey();
	node.value = me.getValue();	
	node = successor(node);
      }
  }

  public int size()
  {
    return size;
  }

  public void clear()
  {
    modCount++;
    root = nil;
    // nil node could have a residual parent reference, clear it for GC.
    nil.parent = null;
    size = 0;
  }

  public Object clone()
  {
    TreeMap copy = null;
    try
      {
        copy = (TreeMap) super.clone();
      }
    catch (CloneNotSupportedException x)
      {
      }
    // Each instance must have a unique sentinal.
    copy.nil = new Node(null, null);
    copy.fabricateTree(size);

    Node node = firstNode();
    Node cnode = copy.firstNode();
    
    while (node != nil)
      {
        cnode.key = node.key;
	cnode.value = node.value;
	node = successor(node);
	cnode = copy.successor(cnode);
      }
    return copy;
  }
  
  public Comparator comparator()
  {
    return comparator;
  }

  public boolean containsKey(Object key)
  {
    return getNode(key) != nil;
  }

  public boolean containsValue(Object value)
  {
    Node node = firstNode();
    Object currentVal;

    while (node != nil)
      {
	currentVal = node.getValue();

        if (value == null ? currentVal == null : value.equals (currentVal))
	  return true;

	node = successor(node);
      }
    return false;
  }

  public Set entrySet()
  {
    // Create an AbstractSet with custom implementations of those methods that 
    // can be overriden easily and efficiently.
    return new AbstractSet()
    {
      public int size()
      {
        return size;
      }
      
      public Iterator iterator()
      {
        return new TreeIterator(TreeIterator.ENTRIES);
      }
            
      public void clear()
      {
        TreeMap.this.clear();
      }

      public boolean contains(Object o)
      {
        if (!(o instanceof Map.Entry))
	  return false;
	Map.Entry me = (Map.Entry) o;
	Node n = getNode(me.getKey());
	return (n != nil && me.getValue().equals(n.value));
      }
      
      public boolean remove(Object o)
      {
        if (!(o instanceof Map.Entry))
	  return false;
	Map.Entry me = (Map.Entry) o;
	Node n = getNode(me.getKey());
	if (n != nil && me.getValue().equals(n.value))
	  {
	    removeNode(n);
	    return true;
	  }
	return false;
      }
    };
  }

  public Object firstKey()
  {
    if (root == nil)
      throw new NoSuchElementException("empty");
    return firstNode().getKey();
  }
  
  private Node firstNode()
  {
    if (root == nil)
      return nil;
    Node node = root;
    while (node.left != nil)
      node = node.left;
    return node;
  }

  public Object lastKey()
  {
    if (root == nil)
      throw new NoSuchElementException("empty");
    return lastNode().getKey();
  }
  
  private Node lastNode()
  {
    if (root == nil)
      return nil;
    Node node = root;
    while (node.right != nil)
      node = node.right;
    return node;  
  }
  
  public Object get(Object key)
  {
    return getNode(key).value;
  }
  
  /** Return the TreeMap.Node associated with KEY, or the nil node if no such
      node exists in the tree. */
  private Node getNode(Object key)
  {
    int comparison;
    Node current = root;

    while (current != nil)
      {
        comparison = compare(key, current.key);
	if (comparison > 0)
	  current = current.right;
	else if (comparison < 0)
	  current = current.left;
	else
	  return current;
      }
    return current; 
  }

  public Set keySet()
  {
    // Create an AbstractSet with custom implementations of those methods that 
    // can be overriden easily and efficiently.
    return new AbstractSet()
    {
      public int size()
      {
        return size;
      }
      
      public Iterator iterator()
      {
        return new TreeIterator(TreeIterator.KEYS);
      }

      public void clear()
      {
        TreeMap.this.clear();
      }

      public boolean contains(Object o)
      {
        return TreeMap.this.containsKey(o);
      }
      
      public boolean remove(Object key)
      {
        Node n = getNode(key);
	if (n == nil)
	  return false;
        TreeMap.this.removeNode(n);
	return true;
      }
    };
  }

  public Object put(Object key, Object value)
  {
    modCount++;
    Node current = root;
    Node parent = nil;
    int comparison = 0;
    
    // Find new node's parent.
    while (current != nil)
      {
	parent = current;
	comparison = compare(key, current.key);
	if (comparison > 0)
	  current = current.right;
	else if (comparison < 0)
	  current = current.left;
	else
	  {
	    // Key already in tree.
	    Object r = current.value;
	    current.value = value;
	    return r;
	  }
      }
    
    // Set up new node.
    Node n = new Node(key, value);
    n.color = RED;
    n.parent = parent;
    n.left = nil;
    n.right = nil;
    
    // Insert node in tree.
    size++;
    if (parent == nil)
      {
        // Special case: inserting into an empty tree.
	root = n;
	n.color = BLACK;
	return null;
      }
    else if (comparison > 0)
      parent.right = n;
    else
      parent.left = n;   
    
    // Rebalance after insert.
    insertFixup(n);
    //verifyTree();
    return null;
  }

  /** Maintain red-black balance after inserting a new node. */
  private void insertFixup(Node n)
  {
    // Only need to rebalance when parent is a RED node, and while at least
    // 2 levels deep into the tree (ie: node has a grandparent).
    while (n != root && n.parent.parent != nil && n.parent.color == RED)
      {
	if (n.parent == n.parent.parent.left)
	  {
            Node uncle = n.parent.parent.right;
            if (uncle != nil && uncle.color == RED) 
	      {
        	n.parent.color = BLACK;
        	uncle.color = BLACK;
        	n.parent.parent.color = RED;
        	n = n.parent.parent;
              }
	    else // Uncle is BLACK.
	      {                
                if (n == n.parent.right)
		  {
                    // Make n a left child.
                    n = n.parent;
                    rotateLeft(n);
                  }

                // Recolor and rotate.
                n.parent.color = BLACK;
                n.parent.parent.color = RED;
                rotateRight(n.parent.parent);
              }
	  }
	else
	  {
	    // Mirror image of above code.
	    Node uncle = n.parent.parent.left;
            if (uncle != nil && uncle.color == RED)
	      {
                n.parent.color = BLACK;
                uncle.color = BLACK;
                n.parent.parent.color = RED;
                n = n.parent.parent;
              }
	    else
	      {
                if (n == n.parent.left)
		  {
                    n = n.parent;
                    rotateRight(n);
                  }
                n.parent.color = BLACK;
                n.parent.parent.color = RED;
                rotateLeft(n.parent.parent);
	      }
	  }
      }
    root.color = BLACK;
  }

  public void putAll(Map m)
  {
    Iterator itr = m.entrySet().iterator();
    int msize = m.size();
    Map.Entry e;

    for (int i = 0; i < msize; i++)
      {
	e = (Map.Entry) itr.next();
	put(e.getKey(), e.getValue());
      }
  }

  public Object remove(Object key)
  {
    Node n = getNode(key);
    if (n != nil)
      {
        removeNode(n);
	return n.value;
      }
    return null;
  }
  
  // Remove node from tree. This will increment modCount and decrement size. 
  // Node must exist in the tree.
  private void removeNode(Node node) // z
  {
    Node splice; // y
    Node child;  // x
    
    modCount++;
    size--;

    // Find splice, the node at the position to actually remove from the tree. 
    if (node.left == nil || node.right == nil)
      {
	// Node to be deleted has 0 or 1 children.
        splice = node;
	if (node.left == nil)
	  child = node.right;
	else
	  child = node.left;
      }
    else
      {
	// Node has 2 children. Splice is node's successor, and will be 
	// swapped with node since we can't remove node directly.
        splice = node.right;
        while (splice.left != nil)
	  splice = splice.left;
	child = splice.right;
      }

    // Unlink splice from the tree.
    Node parent = splice.parent;
    child.parent = parent;
    if (parent != nil)
      {
	if (splice == parent.left)
          parent.left = child;
	else
          parent.right = child;
      }
    else
      root = child;

    // Keep track of splice's color in case it gets changed in the swap.
    int spliceColor = splice.color;

/*
    if (splice != node)
      {
        node.key = splice.key;
	node.value = splice.value;
      }
*/
    if (splice != node)
      {
        // Swap SPLICE for NODE. Some implementations optimize here by simply
	// swapping the values, but we can't do that: if an iterator was
	// referencing a node in its "next" field, and that node got swapped, 
	// things would get confused.
	if (node == root)
	  {
	    root = splice;
	  }
	else
	  {
	    if (node.parent.left == node)
	      node.parent.left = splice;
	    else
	      node.parent.right = splice;
          }
	splice.parent = node.parent;
	splice.left = node.left;
	splice.right = node.right;
	splice.left.parent = splice;
	splice.right.parent = splice;
	splice.color = node.color;
      }

    if (spliceColor == BLACK)
      deleteFixup (child);
    
    //verifyTree();      
  }

  /** Maintain red-black balance after deleting a node. */
  private void deleteFixup (Node node)
  {
    // A black node has been removed, so we need to rebalance to avoid 
    // violating the "same number of black nodes on any path" rule. If
    // node is red, we can simply recolor it black and all is well. 
    while (node != root && node.color == BLACK)
      {
        if (node == node.parent.left)
	  {
	    // Rebalance left side.
	    Node sibling = node.parent.right;
	    if (sibling.color == RED)
	      {
                sibling.color = BLACK;
                node.parent.color = RED;
                rotateLeft(node.parent);
                sibling = node.parent.right;
	      }

	    if (sibling.left.color == BLACK && sibling.right.color == BLACK)
              {
	        // Case 2: Sibling has no red children.
		sibling.color = RED;
		// Black height has been decreased, so move up the tree and 
		// repeat.
		node = node.parent;
              }
	    else
	      {	      
	        if (sibling.right.color == BLACK)
		  {
		    // Case 3: Sibling has red left child.
		    sibling.left.color = BLACK;
		    sibling.color = RED;
                    rotateRight(sibling);
                    sibling = node.parent.right;
		  }		  
		
		// Case 4: Sibling has red right child.
		sibling.color = sibling.parent.color;
		sibling.parent.color = BLACK;
		sibling.right.color = BLACK;
                rotateLeft(node.parent);
                node = root; // Finished.
	      }
	  }
	else
	  {
	    // Symmetric "mirror" of left-side case.
	    Node sibling = node.parent.left;
	    if (sibling.color == RED)
	      {
                sibling.color = BLACK;
                node.parent.color = RED;
                rotateRight(node.parent);
                sibling = node.parent.left;
	      }

	    if (sibling.left.color == BLACK && sibling.right.color == BLACK)
              {
		sibling.color = RED;
		node = node.parent;
              }
	    else
	      {	      
	        if (sibling.left.color == BLACK)
		  {
		    sibling.right.color = BLACK;
		    sibling.color = RED;
                    rotateLeft(sibling);
                    sibling = node.parent.left;
		  }		  
		
		sibling.color = sibling.parent.color;
		sibling.parent.color = BLACK;
		sibling.left.color = BLACK;
                rotateRight(node.parent);
                node = root;
	      }
	  }
      }
    node.color = BLACK;
  }

  public SortedMap subMap(Object fromKey, Object toKey)
  {
    if (compare(fromKey, toKey) <= 0)
      return new SubMap(fromKey, toKey);
    else
      throw new IllegalArgumentException("fromKey > toKey");
  }

  public SortedMap headMap(Object toKey)
  {
    return new SubMap(nil, toKey);
  }

  public SortedMap tailMap(Object fromKey)
  {
    return new SubMap(fromKey, nil);
  }

  /** Returns a "collection view" (or "bag view") of this TreeMap's values. */
  public Collection values()
  {
    // We don't bother overriding many of the optional methods, as doing so
    // wouldn't provide any significant performance advantage.
    return new AbstractCollection()
    {
      public int size()
      {
        return size;
      }
      
      public Iterator iterator()
      {
        return new TreeIterator(TreeIterator.VALUES);
      }
      
      public void clear()
      {
        TreeMap.this.clear();
      }
    };
  }

  // Find the "highest" node which is < key. If key is nil, return last node.
  // Note that highestLessThan is exclusive (it won't return a key which is
  // equal to "key"), while lowestGreaterThan is inclusive, in order to be 
  // consistent with the semantics of subMap().
  private Node highestLessThan(Object key)
  {
    if (key == nil)
      return lastNode();
  
    Node last = nil;
    Node current = root;
    int comparison = 0;

    while (current != nil)
      {
        last = current;
        comparison = compare(key, current.key);
	if (comparison > 0)
	  current = current.right;
	else if (comparison < 0)
	  current = current.left;
	else /* Exact match. */
	  return predecessor(last);
      }
    if (comparison <= 0)
      return predecessor(last);
    else
      return last;
  }

  // Find the "lowest" node which is >= key. If key is nil, return first node.
  private Node lowestGreaterThan(Object key)
  {
    if (key == nil)
      return firstNode();

    Node last = nil;
    Node current = root;
    int comparison = 0;

    while (current != nil)
      {
        last = current;
        comparison = compare(key, current.key);
	if (comparison > 0)
	  current = current.right;
	else if (comparison < 0)
	  current = current.left;
	else
	  return current;
      }
    if (comparison > 0)
      return successor(last);
    else
      return last;
  }  

  private void writeObject(ObjectOutputStream out) throws IOException
  {
    out.defaultWriteObject();

    Node node = firstNode();
    out.writeInt(size);
    
    while (node != nil)
      {
        out.writeObject(node.key);
	out.writeObject(node.value);
	node = successor(node);
      }
  }

  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    in.defaultReadObject();
    int size = in.readInt();
    putFromObjStream(in, size, true);
  }

  private int compare(Object o1, Object o2)
  {
    if (comparator == null)
      return ((Comparable) o1).compareTo(o2);
    else
      return comparator.compare(o1, o2);
  }

  /* Return the node following Node, or nil if there isn't one. */
  private Node successor(Node node)
  {
    if (node.right != nil)
      {
        node = node.right;
	while (node.left != nil)
	  node = node.left;
	return node;
      }

    Node parent = node.parent;
    while (parent != nil && node == parent.right)
      {
	node = parent;
	parent = parent.parent;
      }
    return parent;
  }

  /* Return the node preceeding Node, or nil if there isn't one. */
  private Node predecessor(Node node)
  {
    if (node.left != nil)
      {
        node = node.left;
	while (node.right != nil)
	  node = node.right;
	return node;
      }
      
    Node parent = node.parent;
    while (parent != nil && node == parent.left)
      {
	node = parent;
	parent = parent.parent;
      }
    return parent;
  }

  /** Rotate node n to the left. */
  private void rotateLeft(Node node)
  {
    Node child = node.right;
    
    // Establish node.right link.
    node.right = child.left;
    if (child.left != nil)
      child.left.parent = node;

    // Establish child->parent link.
    child.parent = node.parent;
    if (node.parent != nil)
      {
        if (node == node.parent.left)
	  node.parent.left = child;
	else
	  node.parent.right = child;
      }
    else
      root = child;

    // Link n and child.
    child.left = node;
    if (node != nil)
      node.parent = child;
  }

  /** Rotate node n to the right. */
  private void rotateRight(Node node)
  {
    Node child = node.left;
    
    // Establish node.left link.
    node.left = child.right;
    if (child.right != nil)
      child.right.parent = node;
      
    // Establish child->parent link.
    child.parent = node.parent;
    if (node.parent != nil)
      {
        if (node == node.parent.right)
	  node.parent.right = child;
	else
	  node.parent.left = child;
      }
    else
      root = child;
    
    // Link n and child.
    child.right = node;
    if (node != nil)
      node.parent = child;
  }
  
  /* Construct a tree from sorted keys in linear time. This is used to
     implement TreeSet's SortedSet constructor. */
  void putKeysLinear(Iterator keys, int count)
  {
    fabricateTree(count);    
    Node node = firstNode();
    
    for (int i = 0; i < count; i++)
      {
	node.key = keys.next();
	node.value = Boolean.TRUE;
	node = successor(node);
      }
  }
  
  /* As above, but load keys from an ObjectInputStream. Used by readObject()
     methods. If "readValues" is set, entry values will also be read from the 
     stream. If not, only keys will be read. */
  void putFromObjStream(ObjectInputStream in, int count, boolean readValues) 
    throws IOException, ClassNotFoundException
  {
    fabricateTree(count);    
    Node node = firstNode();
    
    for (int i = 0; i < count; i++)
      {
	node.key = in.readObject();
	if (readValues)
	  node.value = in.readObject();
	else
	  node.value = Boolean.TRUE;	  
	node = successor(node);
      }
  }
     
  /* Construct a perfectly balanced tree consisting of n "blank" nodes. 
     This permits a tree to be generated from pre-sorted input in linear 
     time. */
  private void fabricateTree(int count)
  {
    if (count == 0)
      return;
    // Calculate the (maximum) depth of the perfectly balanced tree.
    double ddepth = (Math.log (count + 1) / Math.log (2));
    int maxdepth = (int) Math.ceil (ddepth);
    
    // The number of nodes which can fit in a perfectly-balanced tree of 
    // height "depth - 1".
    int max = (int) Math.pow (2, maxdepth - 1) - 1;
    
    // Number of nodes which spill over into the deepest row of the tree.
    int overflow = (int) count - max;
    
    size = count;
    // Make the root node.
    root = new Node(null, null);
    root.parent = nil;
    root.left = nil;
    root.right = nil;
    
    Node row = root;
    for (int depth = 2; depth <= maxdepth; depth++)  // each row
      {	
	// Number of nodes at this depth
	int rowcap = (int) Math.pow (2, depth - 1);
	Node parent = row;
	Node last = null;
	
	// Actual number of nodes to create in this row
	int rowsize;
	if (depth == maxdepth)
	  rowsize = overflow;
	else
	  rowsize = rowcap;
	
	// The bottom most row of nodes is coloured red, as is every second row 
	// going up, except the root node (row 1). I'm not sure if this is the 
	// optimal configuration for the tree, but it seems logical enough.
	// We just need to honour the black-height and red-parent rules here.
	boolean colorRowRed = (depth % 2 == maxdepth % 2);
	
	int i;
	for (i = 1; i <= rowsize; i++)  // each node in row
	  {
	    Node node = new Node(null, null);
	    node.parent = parent;
	    if (i % 2 == 1)
	      parent.left = node;
	    else
	      {
		Node nextparent = parent.right;
		parent.right = node;
		parent = nextparent;
	      }

	    // We use the "right" link to maintain a chain of nodes in 
	    // each row until the parent->child links are established.
	    if (last != null)
	      last.right = node;
	    last = node;
	    
	    if (colorRowRed)
	      node.color = RED;
	    
	    if (i == 1)
	      row = node;
	  }

        // Set nil child pointers on leaf nodes.
	if (depth == maxdepth)
	  {
	    // leaf nodes at maxdepth-1.
	    if (parent != null)
	      {
		if (i % 2 == 0)
		  {
	            // Current "parent" has "left" set already.
		    Node next = parent.right;
		    parent.right = nil;
		    parent = next;
		  }		  		  
		while (parent != null)
		  {
		    parent.left = nil;
		    Node next = parent.right;
		    parent.right = nil;
		    parent = next;
		  }
	      }
	    // leaf nodes at maxdepth.
	    Node node = row;
	    Node next;
	    while (node != null)
	      {
	        node.left = nil;
		next = node.right;
		node.right = nil;
		node = next;
	      }
	  }
      }
  }
  
  private class VerifyResult
  {
    int count; // Total number of nodes.
    int black; // Black height/depth.
    int maxdepth; // Maximum depth of branch.
  }

  /* Check that red-black properties are consistent for the tree. */
  private void verifyTree()
  {
    if (root == nil)
      {
        System.err.println ("Verify: empty tree");
	if (size != 0)
	  verifyError (this, "no root node but size=" + size);
	return;
      }
    VerifyResult vr = verifySub (root);
    if (vr.count != size)
      {
	verifyError (this, "Tree size not consistent with actual nodes counted. "
                     + "counted " + vr.count + ", size=" + size);
        System.exit(1);
      }
    System.err.println ("Verify: " + vr.count + " nodes, black height=" + vr.black
                        + ", maxdepth=" + vr.maxdepth);
  }
  
  /* Recursive call to check that rbtree rules hold. Returns total node count
     and black height of the given branch. */
  private VerifyResult verifySub(Node n)
  {
    VerifyResult vr1 = null;
    VerifyResult vr2 = null;
    
    if (n.left == nil && n.right == nil)
      {
        // leaf node
	VerifyResult r = new VerifyResult();
	r.black = (n.color == BLACK ? 1 : 0);
	r.count = 1;
	r.maxdepth = 1;
	return r;
      }
    
    if (n.left != nil)
      {
        if (n.left.parent != n)
	  verifyError(n.left, "Node's parent link does not point to " + n);
	
	if (n.color == RED && n.left.color == RED)
	  verifyError(n, "Red node has red left child");
	
	vr1 = verifySub (n.left);
	if (n.right == nil)
	  {
	    if (n.color == BLACK)
	      vr1.black++;
	    vr1.count++;
	    vr1.maxdepth++;
	    return vr1;
	  }
      }

    if (n.right != nil)
      {
        if (n.right.parent != n)
	  verifyError(n.right, "Node's parent link does not point to " + n);

	if (n.color == RED && n.right.color == RED)
	  verifyError(n, "Red node has red right child");

	vr2 = verifySub (n.right);
	if (n.left == nil)
	  {
	    if (n.color == BLACK)
	      vr2.black++;
	    vr2.count++;
	    vr2.maxdepth++;
	    return vr2;
	  }
      }
    
    if (vr1.black != vr2.black)
      verifyError (n, "Black heights: " + vr1.black + "," + vr2.black + " don't match.");
    vr1.count += vr2.count + 1;
    vr1.maxdepth = Math.max(vr1.maxdepth, vr2.maxdepth) + 1;
    if (n.color == BLACK)
      vr1.black++;
    return vr1;
  }
  
  private void verifyError (Object obj, String msg)
  {
    System.err.print ("Verify error: ");
    try
      {
        System.err.print (obj);
      }
    catch (Exception x)
      {
        System.err.print ("(error printing obj): " + x);
      }
    System.err.println();
    System.err.println (msg);
    Thread.dumpStack();
    System.exit(1);
  }

  /**
   * Iterate over HashMap's entries.
   * This implementation is parameterized to give a sequential view of
   * keys, values, or entries.
   */   
  class TreeIterator implements Iterator
  {
    static final int ENTRIES = 0,
                     KEYS = 1,
                     VALUES = 2;  
  
    // the type of this Iterator: KEYS, VALUES, or ENTRIES.
    int type;
    // the number of modifications to the backing Map that we know about.
    int knownMod = TreeMap.this.modCount;
    // The last Entry returned by a next() call.
    Node last;
    // The next entry that should be returned by next().
    Node next;
    // The last node visible to this iterator. This is used when iterating
    // on a SubMap.
    Node max;

    /* Create Iterator with the supplied type: KEYS, VALUES, or ENTRIES */
    TreeIterator(int type)
    {
      this.type = type;
      this.next = firstNode();
    }
    
    /* Construct an interator for a SubMap. Iteration will begin at node
       "first", and stop when "max" is reached. */    
    TreeIterator(int type, Node first, Node max)
    {
      this.type = type;
      this.next = first;
      this.max = max;
    }

    public boolean hasNext()
    {
      if (knownMod != TreeMap.this.modCount)
	throw new ConcurrentModificationException();
      return (next != nil);
    }

    public Object next()
    {
      if (next == nil)
	throw new NoSuchElementException();
      if (knownMod != TreeMap.this.modCount)
	throw new ConcurrentModificationException();
      Node n = next;

      // Check limit in case we are iterating through a submap.
      if (n != max)
	next = successor(n);
      else
        next = nil;
      
      last = n;
      
      if (type == VALUES)
        return n.value;
      else if (type == KEYS)
        return n.key;
      return n;
    }

    public void remove()
    {
      if (last == null)
	throw new IllegalStateException();
      if (knownMod != TreeMap.this.modCount)
	throw new ConcurrentModificationException();
/*
      Object key = null;
      if (next != nil)
        key = next.key;
*/
      TreeMap.this.removeNode(last);
      knownMod++;
/*
      if (key != null)
        next = getNode(key);
*/	
      last = null;
    }
  }

  class SubMap extends AbstractMap implements SortedMap
  {
    Object minKey;
    Object maxKey;

    /* Create a SubMap representing the elements between minKey and maxKey
       (inclusive). If minKey is nil, SubMap has no lower bound (headMap).
       If maxKey is nil, the SubMap has no upper bound (tailMap). */
    SubMap(Object minKey, Object maxKey)
    {
      this.minKey = minKey;
      this.maxKey = maxKey;
    }

    public void clear()
    {
      Node current;
      Node next = lowestGreaterThan(minKey);
      Node max = highestLessThan(maxKey);
      
      if (compare(next.key, max.key) > 0)
        // Nothing to delete.
	return;
        
      do
        {
	  current = next;
	  next = successor(current);
	  remove(current);
	}
      while (current != max);
    }
    
    /* Check if "key" is in within the range bounds for this SubMap. 
       The lower ("from") SubMap range is inclusive, and the upper (to) bound
       is exclusive. */
    private boolean keyInRange(Object key)
    {
      return ((minKey == nil || compare(key, minKey) >= 0)
	      && (maxKey == nil || compare(key, maxKey) < 0));
    }

    public boolean containsKey(Object key)
    {
      return (keyInRange(key) && TreeMap.this.containsKey(key));
    }

    public boolean containsValue(Object value)
    {
      Node node = lowestGreaterThan(minKey);
      Node max = highestLessThan(maxKey);
      Object currentVal;

      if (node == nil || max == nil || compare(node.key, max.key) > 0)
        // Nothing to search.
	return false;

      while (true)
	{
	  currentVal = node.getValue();
          if (value == null ? currentVal == null : value.equals (currentVal))
	    return true;
	  if (node == max)
	    return false;
	  node = successor(node);
	}
    }

    public Object get(Object key)
    {
      if (keyInRange(key))
	return TreeMap.this.get(key);
      return null;
    }

    public Object put(Object key, Object value)
    {
      if (keyInRange(key))
	return TreeMap.this.put(key, value);
      else
	throw new IllegalArgumentException("Key outside range");
    }

    public Object remove(Object key)
    {
      if (keyInRange(key))
	return TreeMap.this.remove(key);
      else
        return null;
    }

    public int size()
    {
      Node node = lowestGreaterThan(minKey);
      Node max = highestLessThan(maxKey);

      if (node == nil || max == nil || compare(node.key, max.key) > 0)
	return 0;  // Empty.

      int count = 1;
      while (node != max)
        {
	  count++;
	  node = successor(node);
	}

      return count;
    }

    public Set entrySet()
    {
      // Create an AbstractSet with custom implementations of those methods that 
      // can be overriden easily and efficiently.
      return new AbstractSet()
      {
	public int size()
	{
          return SubMap.this.size();
	}

	public Iterator iterator()
	{
	  Node first = lowestGreaterThan(minKey);
	  Node max = highestLessThan(maxKey);
          return new TreeIterator(TreeIterator.ENTRIES, first, max);
	}

	public void clear()
	{
          this.clear();
	}

	public boolean contains(Object o)
	{
          if (!(o instanceof Map.Entry))
	    return false;
	  Map.Entry me = (Map.Entry) o;
	  Object key = me.getKey();
	  if (!keyInRange(key))
	    return false;
	  Node n = getNode(key);
	  return (n != nil && me.getValue().equals(n.value));
	}

	public boolean remove(Object o)
	{
          if (!(o instanceof Map.Entry))
	    return false;
	  Map.Entry me = (Map.Entry) o;
	  Object key = me.getKey();
	  if (!keyInRange(key))
	    return false;
	  Node n = getNode(key);
	  if (n != nil && me.getValue().equals(n.value))
	    {
	      removeNode(n);
	      return true;
	    }
	  return false;
	}
      };    
    }

    public Comparator comparator()
    {
      return comparator;
    }

    public Object firstKey()
    {
      Node node = lowestGreaterThan(minKey);
      if (node == nil || !keyInRange(node.key))
        throw new NoSuchElementException ("empty");
      return node.key;
    }

    public Object lastKey()
    {
      Node node = highestLessThan(maxKey);
      if (node == nil || !keyInRange(node.key))
        throw new NoSuchElementException ("empty");
      return node.key;
    }

    public SortedMap subMap(Object fromKey, Object toKey)
    {
      if (!keyInRange(fromKey) || !keyInRange(toKey))
        throw new IllegalArgumentException("key outside range");

      return TreeMap.this.subMap(fromKey, toKey);
    }

    public SortedMap headMap(Object toKey)
    {
      if (!keyInRange(toKey))
        throw new IllegalArgumentException("key outside range");

      return TreeMap.this.subMap(minKey, toKey);
    }

    public SortedMap tailMap(Object fromKey)
    {
      if (!keyInRange(fromKey))
        throw new IllegalArgumentException("key outside range");

      return TreeMap.this.subMap(fromKey, maxKey);
    }
  }
}
