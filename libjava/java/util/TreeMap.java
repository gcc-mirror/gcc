/* TreeMap.java -- a class providing a basic Red-Black Tree data structure,
   mapping Object --> Object
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005  Free Software Foundation, Inc.

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


package java.util;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

/**
 * This class provides a red-black tree implementation of the SortedMap
 * interface.  Elements in the Map will be sorted by either a user-provided
 * Comparator object, or by the natural ordering of the keys.
 *
 * The algorithms are adopted from Corman, Leiserson, and Rivest's
 * <i>Introduction to Algorithms.</i>  TreeMap guarantees O(log n)
 * insertion and deletion of elements.  That being said, there is a large
 * enough constant coefficient in front of that "log n" (overhead involved
 * in keeping the tree balanced), that TreeMap may not be the best choice
 * for small collections. If something is already sorted, you may want to
 * just use a LinkedHashMap to maintain the order while providing O(1) access.
 *
 * TreeMap is a part of the JDK1.2 Collections API.  Null keys are allowed
 * only if a Comparator is used which can deal with them; natural ordering
 * cannot cope with null.  Null values are always allowed. Note that the
 * ordering must be <i>consistent with equals</i> to correctly implement
 * the Map interface. If this condition is violated, the map is still
 * well-behaved, but you may have suprising results when comparing it to
 * other maps.<p>
 *
 * This implementation is not synchronized. If you need to share this between
 * multiple threads, do something like:<br>
 * <code>SortedMap m
 *       = Collections.synchronizedSortedMap(new TreeMap(...));</code><p>
 *
 * The iterators are <i>fail-fast</i>, meaning that any structural
 * modification, except for <code>remove()</code> called on the iterator
 * itself, cause the iterator to throw a
 * <code>ConcurrentModificationException</code> rather than exhibit
 * non-deterministic behavior.
 *
 * @author Jon Zeppieri
 * @author Bryce McKinlay
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Map
 * @see HashMap
 * @see Hashtable
 * @see LinkedHashMap
 * @see Comparable
 * @see Comparator
 * @see Collection
 * @see Collections#synchronizedSortedMap(SortedMap)
 * @since 1.2
 * @status updated to 1.4
 */
public class TreeMap extends AbstractMap
  implements SortedMap, Cloneable, Serializable
{
  // Implementation note:
  // A red-black tree is a binary search tree with the additional properties
  // that all paths to a leaf node visit the same number of black nodes,
  // and no red node has red children. To avoid some null-pointer checks,
  // we use the special node nil which is always black, has no relatives,
  // and has key and value of null (but is not equal to a mapping of null).

  /**
   * Compatible with JDK 1.2.
   */
  private static final long serialVersionUID = 919286545866124006L;

  /**
   * Color status of a node. Package visible for use by nested classes.
   */
  static final int RED = -1,
                   BLACK = 1;

  /**
   * Sentinal node, used to avoid null checks for corner cases and make the
   * delete rebalance code simpler. The rebalance code must never assign
   * the parent, left, or right of nil, but may safely reassign the color
   * to be black. This object must never be used as a key in a TreeMap, or
   * it will break bounds checking of a SubMap.
   */
  static final Node nil = new Node(null, null, BLACK);
  static
    {
      // Nil is self-referential, so we must initialize it after creation.
      nil.parent = nil;
      nil.left = nil;
      nil.right = nil;
    }

  /**
   * The root node of this TreeMap.
   */
  private transient Node root;

  /**
   * The size of this TreeMap. Package visible for use by nested classes.
   */
  transient int size;

  /**
   * The cache for {@link #entrySet()}.
   */
  private transient Set entries;

  /**
   * Counts the number of modifications this TreeMap has undergone, used
   * by Iterators to know when to throw ConcurrentModificationExceptions.
   * Package visible for use by nested classes.
   */
  transient int modCount;

  /**
   * This TreeMap's comparator, or null for natural ordering.
   * Package visible for use by nested classes.
   * @serial the comparator ordering this tree, or null
   */
  final Comparator comparator;

  /**
   * Class to represent an entry in the tree. Holds a single key-value pair,
   * plus pointers to parent and child nodes.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class Node extends AbstractMap.BasicMapEntry
  {
    // All fields package visible for use by nested classes.
    /** The color of this node. */
    int color;

    /** The left child node. */
    Node left = nil;
    /** The right child node. */
    Node right = nil;
    /** The parent node. */
    Node parent = nil;

    /**
     * Simple constructor.
     * @param key the key
     * @param value the value
     */
    Node(Object key, Object value, int color)
    {
      super(key, value);
      this.color = color;
    }
  }

  /**
   * Instantiate a new TreeMap with no elements, using the keys' natural
   * ordering to sort. All entries in the map must have a key which implements
   * Comparable, and which are <i>mutually comparable</i>, otherwise map
   * operations may throw a {@link ClassCastException}. Attempts to use
   * a null key will throw a {@link NullPointerException}.
   *
   * @see Comparable
   */
  public TreeMap()
  {
    this((Comparator) null);
  }

  /**
   * Instantiate a new TreeMap with no elements, using the provided comparator
   * to sort. All entries in the map must have keys which are mutually
   * comparable by the Comparator, otherwise map operations may throw a
   * {@link ClassCastException}.
   *
   * @param comparator the sort order for the keys of this map, or null
   *        for the natural order
   */
  public TreeMap(Comparator c)
  {
    comparator = c;
    fabricateTree(0);
  }

  /**
   * Instantiate a new TreeMap, initializing it with all of the elements in
   * the provided Map.  The elements will be sorted using the natural
   * ordering of the keys. This algorithm runs in n*log(n) time. All entries
   * in the map must have keys which implement Comparable and are mutually
   * comparable, otherwise map operations may throw a
   * {@link ClassCastException}.
   *
   * @param map a Map, whose entries will be put into this TreeMap
   * @throws ClassCastException if the keys in the provided Map are not
   *         comparable
   * @throws NullPointerException if map is null
   * @see Comparable
   */
  public TreeMap(Map map)
  {
    this((Comparator) null);
    putAll(map);
  }

  /**
   * Instantiate a new TreeMap, initializing it with all of the elements in
   * the provided SortedMap.  The elements will be sorted using the same
   * comparator as in the provided SortedMap. This runs in linear time.
   *
   * @param sm a SortedMap, whose entries will be put into this TreeMap
   * @throws NullPointerException if sm is null
   */
  public TreeMap(SortedMap sm)
  {
    this(sm.comparator());
    int pos = sm.size();
    Iterator itr = sm.entrySet().iterator();

    fabricateTree(pos);
    Node node = firstNode();

    while (--pos >= 0)
      {
        Map.Entry me = (Map.Entry) itr.next();
        node.key = me.getKey();
        node.value = me.getValue();
        node = successor(node);
      }
  }

  /**
   * Clears the Map so it has no keys. This is O(1).
   */
  public void clear()
  {
    if (size > 0)
      {
        modCount++;
        root = nil;
        size = 0;
      }
  }

  /**
   * Returns a shallow clone of this TreeMap. The Map itself is cloned,
   * but its contents are not.
   *
   * @return the clone
   */
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
    copy.entries = null;
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

  /**
   * Return the comparator used to sort this map, or null if it is by
   * natural order.
   *
   * @return the map's comparator
   */
  public Comparator comparator()
  {
    return comparator;
  }

  /**
   * Returns true if the map contains a mapping for the given key.
   *
   * @param key the key to look for
   * @return true if the key has a mapping
   * @throws ClassCastException if key is not comparable to map elements
   * @throws NullPointerException if key is null and the comparator is not
   *         tolerant of nulls
   */
  public boolean containsKey(Object key)
  {
    return getNode(key) != nil;
  }

  /**
   * Returns true if the map contains at least one mapping to the given value.
   * This requires linear time.
   *
   * @param value the value to look for
   * @return true if the value appears in a mapping
   */
  public boolean containsValue(Object value)
  {
    Node node = firstNode();
    while (node != nil)
      {
        if (equals(value, node.value))
          return true;
        node = successor(node);
      }
    return false;
  }

  /**
   * Returns a "set view" of this TreeMap's entries. The set is backed by
   * the TreeMap, so changes in one show up in the other.  The set supports
   * element removal, but not element addition.<p>
   *
   * Note that the iterators for all three views, from keySet(), entrySet(),
   * and values(), traverse the TreeMap in sorted sequence.
   *
   * @return a set view of the entries
   * @see #keySet()
   * @see #values()
   * @see Map.Entry
   */
  public Set entrySet()
  {
    if (entries == null)
      // Create an AbstractSet with custom implementations of those methods
      // that can be overriden easily and efficiently.
      entries = new AbstractSet()
      {
        public int size()
        {
          return size;
        }

        public Iterator iterator()
        {
          return new TreeIterator(ENTRIES);
        }

        public void clear()
        {
          TreeMap.this.clear();
        }

        public boolean contains(Object o)
        {
          if (! (o instanceof Map.Entry))
            return false;
          Map.Entry me = (Map.Entry) o;
          Node n = getNode(me.getKey());
          return n != nil && AbstractSet.equals(me.getValue(), n.value);
      }

        public boolean remove(Object o)
        {
          if (! (o instanceof Map.Entry))
            return false;
          Map.Entry me = (Map.Entry) o;
          Node n = getNode(me.getKey());
          if (n != nil && AbstractSet.equals(me.getValue(), n.value))
            {
              removeNode(n);
              return true;
            }
          return false;
        }
      };
    return entries;
  }

  /**
   * Returns the first (lowest) key in the map.
   *
   * @return the first key
   * @throws NoSuchElementException if the map is empty
   */
  public Object firstKey()
  {
    if (root == nil)
      throw new NoSuchElementException();
    return firstNode().key;
  }

  /**
   * Return the value in this TreeMap associated with the supplied key,
   * or <code>null</code> if the key maps to nothing.  NOTE: Since the value
   * could also be null, you must use containsKey to see if this key
   * actually maps to something.
   *
   * @param key the key for which to fetch an associated value
   * @return what the key maps to, if present
   * @throws ClassCastException if key is not comparable to elements in the map
   * @throws NullPointerException if key is null but the comparator does not
   *         tolerate nulls
   * @see #put(Object, Object)
   * @see #containsKey(Object)
   */
  public Object get(Object key)
  {
    // Exploit fact that nil.value == null.
    return getNode(key).value;
  }

  /**
   * Returns a view of this Map including all entries with keys less than
   * <code>toKey</code>. The returned map is backed by the original, so changes
   * in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned map does not include
   * the endpoint; if you want inclusion, pass the successor element.
   *
   * @param toKey the (exclusive) cutoff point
   * @return a view of the map less than the cutoff
   * @throws ClassCastException if <code>toKey</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if toKey is null, but the comparator does not
   *         tolerate null elements
   */
  public SortedMap headMap(Object toKey)
  {
    return new SubMap(nil, toKey);
  }

  /**
   * Returns a "set view" of this TreeMap's keys. The set is backed by the
   * TreeMap, so changes in one show up in the other.  The set supports
   * element removal, but not element addition.
   *
   * @return a set view of the keys
   * @see #values()
   * @see #entrySet()
   */
  public Set keySet()
  {
    if (keys == null)
      // Create an AbstractSet with custom implementations of those methods
      // that can be overriden easily and efficiently.
      keys = new AbstractSet()
      {
        public int size()
        {
          return size;
        }

        public Iterator iterator()
        {
          return new TreeIterator(KEYS);
        }

        public void clear()
        {
          TreeMap.this.clear();
        }

        public boolean contains(Object o)
        {
          return containsKey(o);
        }

        public boolean remove(Object key)
        {
          Node n = getNode(key);
          if (n == nil)
            return false;
          removeNode(n);
          return true;
        }
      };
    return keys;
  }

  /**
   * Returns the last (highest) key in the map.
   *
   * @return the last key
   * @throws NoSuchElementException if the map is empty
   */
  public Object lastKey()
  {
    if (root == nil)
      throw new NoSuchElementException("empty");
    return lastNode().key;
  }

  /**
   * Puts the supplied value into the Map, mapped by the supplied key.
   * The value may be retrieved by any object which <code>equals()</code>
   * this key. NOTE: Since the prior value could also be null, you must
   * first use containsKey if you want to see if you are replacing the
   * key's mapping.
   *
   * @param key the key used to locate the value
   * @param value the value to be stored in the Map
   * @return the prior mapping of the key, or null if there was none
   * @throws ClassCastException if key is not comparable to current map keys
   * @throws NullPointerException if key is null, but the comparator does
   *         not tolerate nulls
   * @see #get(Object)
   * @see Object#equals(Object)
   */
  public Object put(Object key, Object value)
  {
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
        else // Key already in tree.
          return current.setValue(value);
      }

    // Set up new node.
    Node n = new Node(key, value, RED);
    n.parent = parent;

    // Insert node in tree.
    modCount++;
    size++;
    if (parent == nil)
      {
        // Special case inserting into an empty tree.
        root = n;
        return null;
      }
    if (comparison > 0)
      parent.right = n;
    else
      parent.left = n;

    // Rebalance after insert.
    insertFixup(n);
    return null;
  }

  /**
   * Copies all elements of the given map into this TreeMap.  If this map
   * already has a mapping for a key, the new mapping replaces the current
   * one.
   *
   * @param m the map to be added
   * @throws ClassCastException if a key in m is not comparable with keys
   *         in the map
   * @throws NullPointerException if a key in m is null, and the comparator
   *         does not tolerate nulls
   */
  public void putAll(Map m)
  {
    Iterator itr = m.entrySet().iterator();
    int pos = m.size();
    while (--pos >= 0)
      {
        Map.Entry e = (Map.Entry) itr.next();
        put(e.getKey(), e.getValue());
      }
  }

  /**
   * Removes from the TreeMap and returns the value which is mapped by the
   * supplied key. If the key maps to nothing, then the TreeMap remains
   * unchanged, and <code>null</code> is returned. NOTE: Since the value
   * could also be null, you must use containsKey to see if you are
   * actually removing a mapping.
   *
   * @param key the key used to locate the value to remove
   * @return whatever the key mapped to, if present
   * @throws ClassCastException if key is not comparable to current map keys
   * @throws NullPointerException if key is null, but the comparator does
   *         not tolerate nulls
   */
  public Object remove(Object key)
  {
    Node n = getNode(key);
    if (n == nil)
      return null;
    // Note: removeNode can alter the contents of n, so save value now.
    Object result = n.value;
    removeNode(n);
    return result;
  }

  /**
   * Returns the number of key-value mappings currently in this Map.
   *
   * @return the size
   */
  public int size()
  {
    return size;
  }

  /**
   * Returns a view of this Map including all entries with keys greater or
   * equal to <code>fromKey</code> and less than <code>toKey</code> (a
   * half-open interval). The returned map is backed by the original, so
   * changes in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoffs. The returned map includes the low
   * endpoint but not the high; if you want to reverse this behavior on
   * either end, pass in the successor element.
   *
   * @param fromKey the (inclusive) low cutoff point
   * @param toKey the (exclusive) high cutoff point
   * @return a view of the map between the cutoffs
   * @throws ClassCastException if either cutoff is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if fromKey or toKey is null, but the
   *         comparator does not tolerate null elements
   * @throws IllegalArgumentException if fromKey is greater than toKey
   */
  public SortedMap subMap(Object fromKey, Object toKey)
  {
    return new SubMap(fromKey, toKey);
  }

  /**
   * Returns a view of this Map including all entries with keys greater or
   * equal to <code>fromKey</code>. The returned map is backed by the
   * original, so changes in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned map includes the
   * endpoint; if you want to exclude it, pass in the successor element.
   *
   * @param fromKey the (inclusive) low cutoff point
   * @return a view of the map above the cutoff
   * @throws ClassCastException if <code>fromKey</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if fromKey is null, but the comparator
   *         does not tolerate null elements
   */
  public SortedMap tailMap(Object fromKey)
  {
    return new SubMap(fromKey, nil);
  }

  /**
   * Returns a "collection view" (or "bag view") of this TreeMap's values.
   * The collection is backed by the TreeMap, so changes in one show up
   * in the other.  The collection supports element removal, but not element
   * addition.
   *
   * @return a bag view of the values
   * @see #keySet()
   * @see #entrySet()
   */
  public Collection values()
  {
    if (values == null)
      // We don't bother overriding many of the optional methods, as doing so
      // wouldn't provide any significant performance advantage.
      values = new AbstractCollection()
      {
        public int size()
        {
          return size;
        }

        public Iterator iterator()
        {
          return new TreeIterator(VALUES);
        }

        public void clear()
        {
          TreeMap.this.clear();
        }
      };
    return values;
  }

  /**
   * Compares two elements by the set comparator, or by natural ordering.
   * Package visible for use by nested classes.
   *
   * @param o1 the first object
   * @param o2 the second object
   * @throws ClassCastException if o1 and o2 are not mutually comparable,
   *         or are not Comparable with natural ordering
   * @throws NullPointerException if o1 or o2 is null with natural ordering
   */
  final int compare(Object o1, Object o2)
  {
    return (comparator == null
            ? ((Comparable) o1).compareTo(o2)
            : comparator.compare(o1, o2));
  }

  /**
   * Maintain red-black balance after deleting a node.
   *
   * @param node the child of the node just deleted, possibly nil
   * @param parent the parent of the node just deleted, never nil
   */
  private void deleteFixup(Node node, Node parent)
  {
    // if (parent == nil)
    //   throw new InternalError();
    // If a black node has been removed, we need to rebalance to avoid
    // violating the "same number of black nodes on any path" rule. If
    // node is red, we can simply recolor it black and all is well.
    while (node != root && node.color == BLACK)
      {
        if (node == parent.left)
          {
            // Rebalance left side.
            Node sibling = parent.right;
            // if (sibling == nil)
            //   throw new InternalError();
            if (sibling.color == RED)
              {
                // Case 1: Sibling is red.
                // Recolor sibling and parent, and rotate parent left.
                sibling.color = BLACK;
                parent.color = RED;
                rotateLeft(parent);
                sibling = parent.right;
              }

            if (sibling.left.color == BLACK && sibling.right.color == BLACK)
              {
                // Case 2: Sibling has no red children.
                // Recolor sibling, and move to parent.
                sibling.color = RED;
                node = parent;
                parent = parent.parent;
              }
            else
              {
                if (sibling.right.color == BLACK)
                  {
                    // Case 3: Sibling has red left child.
                    // Recolor sibling and left child, rotate sibling right.
                    sibling.left.color = BLACK;
                    sibling.color = RED;
                    rotateRight(sibling);
                    sibling = parent.right;
                  }
                // Case 4: Sibling has red right child. Recolor sibling,
                // right child, and parent, and rotate parent left.
                sibling.color = parent.color;
                parent.color = BLACK;
                sibling.right.color = BLACK;
                rotateLeft(parent);
                node = root; // Finished.
              }
          }
        else
          {
            // Symmetric "mirror" of left-side case.
            Node sibling = parent.left;
            // if (sibling == nil)
            //   throw new InternalError();
            if (sibling.color == RED)
              {
                // Case 1: Sibling is red.
                // Recolor sibling and parent, and rotate parent right.
                sibling.color = BLACK;
                parent.color = RED;
                rotateRight(parent);
                sibling = parent.left;
              }

            if (sibling.right.color == BLACK && sibling.left.color == BLACK)
              {
                // Case 2: Sibling has no red children.
                // Recolor sibling, and move to parent.
                sibling.color = RED;
                node = parent;
                parent = parent.parent;
              }
            else
              {
                if (sibling.left.color == BLACK)
                  {
                    // Case 3: Sibling has red right child.
                    // Recolor sibling and right child, rotate sibling left.
                    sibling.right.color = BLACK;
                    sibling.color = RED;
                    rotateLeft(sibling);
                    sibling = parent.left;
                  }
                // Case 4: Sibling has red left child. Recolor sibling,
                // left child, and parent, and rotate parent right.
                sibling.color = parent.color;
                parent.color = BLACK;
                sibling.left.color = BLACK;
                rotateRight(parent);
                node = root; // Finished.
              }
          }
      }
    node.color = BLACK;
  }

  /**
   * Construct a perfectly balanced tree consisting of n "blank" nodes. This
   * permits a tree to be generated from pre-sorted input in linear time.
   *
   * @param count the number of blank nodes, non-negative
   */
  private void fabricateTree(final int count)
  {
    if (count == 0)
      {
	root = nil;
	size = 0;
	return;
      }

    // We color every row of nodes black, except for the overflow nodes.
    // I believe that this is the optimal arrangement. We construct the tree
    // in place by temporarily linking each node to the next node in the row,
    // then updating those links to the children when working on the next row.

    // Make the root node.
    root = new Node(null, null, BLACK);
    size = count;
    Node row = root;
    int rowsize;

    // Fill each row that is completely full of nodes.
    for (rowsize = 2; rowsize + rowsize <= count; rowsize <<= 1)
      {
        Node parent = row;
        Node last = null;
        for (int i = 0; i < rowsize; i += 2)
          {
            Node left = new Node(null, null, BLACK);
            Node right = new Node(null, null, BLACK);
            left.parent = parent;
            left.right = right;
            right.parent = parent;
            parent.left = left;
            Node next = parent.right;
            parent.right = right;
            parent = next;
            if (last != null)
              last.right = left;
            last = right;
          }
        row = row.left;
      }

    // Now do the partial final row in red.
    int overflow = count - rowsize;
    Node parent = row;
    int i;
    for (i = 0; i < overflow; i += 2)
      {
        Node left = new Node(null, null, RED);
        Node right = new Node(null, null, RED);
        left.parent = parent;
        right.parent = parent;
        parent.left = left;
        Node next = parent.right;
        parent.right = right;
        parent = next;
      }
    // Add a lone left node if necessary.
    if (i - overflow == 0)
      {
        Node left = new Node(null, null, RED);
        left.parent = parent;
        parent.left = left;
        parent = parent.right;
        left.parent.right = nil;
      }
    // Unlink the remaining nodes of the previous row.
    while (parent != nil)
      {
        Node next = parent.right;
        parent.right = nil;
        parent = next;
      }
  }

  /**
   * Returns the first sorted node in the map, or nil if empty. Package
   * visible for use by nested classes.
   *
   * @return the first node
   */
  final Node firstNode()
  {
    // Exploit fact that nil.left == nil.
    Node node = root;
    while (node.left != nil)
      node = node.left;
    return node;
  }

  /**
   * Return the TreeMap.Node associated with key, or the nil node if no such
   * node exists in the tree. Package visible for use by nested classes.
   *
   * @param key the key to search for
   * @return the node where the key is found, or nil
   */
  final Node getNode(Object key)
  {
    Node current = root;
    while (current != nil)
      {
        int comparison = compare(key, current.key);
        if (comparison > 0)
          current = current.right;
        else if (comparison < 0)
          current = current.left;
        else
          return current;
      }
    return current;
  }

  /**
   * Find the "highest" node which is &lt; key. If key is nil, return last
   * node. Package visible for use by nested classes.
   *
   * @param key the upper bound, exclusive
   * @return the previous node
   */
  final Node highestLessThan(Object key)
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
        else // Exact match.
          return predecessor(last);
      }
    return comparison <= 0 ? predecessor(last) : last;
  }

  /**
   * Maintain red-black balance after inserting a new node.
   *
   * @param n the newly inserted node
   */
  private void insertFixup(Node n)
  {
    // Only need to rebalance when parent is a RED node, and while at least
    // 2 levels deep into the tree (ie: node has a grandparent). Remember
    // that nil.color == BLACK.
    while (n.parent.color == RED && n.parent.parent != nil)
      {
        if (n.parent == n.parent.parent.left)
          {
            Node uncle = n.parent.parent.right;
            // Uncle may be nil, in which case it is BLACK.
            if (uncle.color == RED)
              {
                // Case 1. Uncle is RED: Change colors of parent, uncle,
                // and grandparent, and move n to grandparent.
                n.parent.color = BLACK;
                uncle.color = BLACK;
                uncle.parent.color = RED;
                n = uncle.parent;
              }
            else
              {
                if (n == n.parent.right)
                  {
                    // Case 2. Uncle is BLACK and x is right child.
                    // Move n to parent, and rotate n left.
                    n = n.parent;
                    rotateLeft(n);
                  }
                // Case 3. Uncle is BLACK and x is left child.
                // Recolor parent, grandparent, and rotate grandparent right.
                n.parent.color = BLACK;
                n.parent.parent.color = RED;
                rotateRight(n.parent.parent);
              }
          }
        else
          {
            // Mirror image of above code.
            Node uncle = n.parent.parent.left;
            // Uncle may be nil, in which case it is BLACK.
            if (uncle.color == RED)
              {
                // Case 1. Uncle is RED: Change colors of parent, uncle,
                // and grandparent, and move n to grandparent.
                n.parent.color = BLACK;
                uncle.color = BLACK;
                uncle.parent.color = RED;
                n = uncle.parent;
              }
            else
              {
                if (n == n.parent.left)
                {
                    // Case 2. Uncle is BLACK and x is left child.
                    // Move n to parent, and rotate n right.
                    n = n.parent;
                    rotateRight(n);
                  }
                // Case 3. Uncle is BLACK and x is right child.
                // Recolor parent, grandparent, and rotate grandparent left.
                n.parent.color = BLACK;
                n.parent.parent.color = RED;
                rotateLeft(n.parent.parent);
              }
          }
      }
    root.color = BLACK;
  }

  /**
   * Returns the last sorted node in the map, or nil if empty.
   *
   * @return the last node
   */
  private Node lastNode()
  {
    // Exploit fact that nil.right == nil.
    Node node = root;
    while (node.right != nil)
      node = node.right;
    return node;
  }

  /**
   * Find the "lowest" node which is &gt;= key. If key is nil, return either
   * nil or the first node, depending on the parameter first.
   * Package visible for use by nested classes.
   *
   * @param key the lower bound, inclusive
   * @param first true to return the first element instead of nil for nil key
   * @return the next node
   */
  final Node lowestGreaterThan(Object key, boolean first)
  {
    if (key == nil)
      return first ? firstNode() : nil;

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
    return comparison > 0 ? successor(last) : last;
  }

  /**
   * Return the node preceding the given one, or nil if there isn't one.
   *
   * @param node the current node, not nil
   * @return the prior node in sorted order
   */
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
    // Exploit fact that nil.left == nil and node is non-nil.
    while (node == parent.left)
      {
        node = parent;
        parent = node.parent;
      }
    return parent;
  }

  /**
   * Construct a tree from sorted keys in linear time. Package visible for
   * use by TreeSet.
   *
   * @param s the stream to read from
   * @param count the number of keys to read
   * @param readValue true to read values, false to insert "" as the value
   * @throws ClassNotFoundException if the underlying stream fails
   * @throws IOException if the underlying stream fails
   * @see #readObject(ObjectInputStream)
   * @see TreeSet#readObject(ObjectInputStream)
   */
  final void putFromObjStream(ObjectInputStream s, int count,
                              boolean readValues)
    throws IOException, ClassNotFoundException
  {
    fabricateTree(count);
    Node node = firstNode();

    while (--count >= 0)
      {
        node.key = s.readObject();
        node.value = readValues ? s.readObject() : "";
        node = successor(node);
      }
  }

  /**
   * Construct a tree from sorted keys in linear time, with values of "".
   * Package visible for use by TreeSet.
   *
   * @param keys the iterator over the sorted keys
   * @param count the number of nodes to insert
   * @see TreeSet#TreeSet(SortedSet)
   */
  final void putKeysLinear(Iterator keys, int count)
  {
    fabricateTree(count);
    Node node = firstNode();

    while (--count >= 0)
      {
        node.key = keys.next();
        node.value = "";
        node = successor(node);
      }
  }

  /**
   * Deserializes this object from the given stream.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if the underlying stream fails
   * @throws IOException if the underlying stream fails
   * @serialData the <i>size</i> (int), followed by key (Object) and value
   *             (Object) pairs in sorted order
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    int size = s.readInt();
    putFromObjStream(s, size, true);
  }

  /**
   * Remove node from tree. This will increment modCount and decrement size.
   * Node must exist in the tree. Package visible for use by nested classes.
   *
   * @param node the node to remove
   */
  final void removeNode(Node node)
  {
    Node splice;
    Node child;

    modCount++;
    size--;

    // Find splice, the node at the position to actually remove from the tree.
    if (node.left == nil)
      {
        // Node to be deleted has 0 or 1 children.
        splice = node;
        child = node.right;
      }
    else if (node.right == nil)
      {
        // Node to be deleted has 1 child.
        splice = node;
        child = node.left;
      }
    else
      {
        // Node has 2 children. Splice is node's predecessor, and we swap
        // its contents into node.
        splice = node.left;
        while (splice.right != nil)
          splice = splice.right;
        child = splice.left;
        node.key = splice.key;
        node.value = splice.value;
      }

    // Unlink splice from the tree.
    Node parent = splice.parent;
    if (child != nil)
      child.parent = parent;
    if (parent == nil)
      {
        // Special case for 0 or 1 node remaining.
        root = child;
        return;
      }
    if (splice == parent.left)
      parent.left = child;
    else
      parent.right = child;

    if (splice.color == BLACK)
      deleteFixup(child, parent);
  }

  /**
   * Rotate node n to the left.
   *
   * @param node the node to rotate
   */
  private void rotateLeft(Node node)
  {
    Node child = node.right;
    // if (node == nil || child == nil)
    //   throw new InternalError();

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
    node.parent = child;
  }

  /**
   * Rotate node n to the right.
   *
   * @param node the node to rotate
   */
  private void rotateRight(Node node)
  {
    Node child = node.left;
    // if (node == nil || child == nil)
    //   throw new InternalError();

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
    node.parent = child;
  }

  /**
   * Return the node following the given one, or nil if there isn't one.
   * Package visible for use by nested classes.
   *
   * @param node the current node, not nil
   * @return the next node in sorted order
   */
  final Node successor(Node node)
  {
    if (node.right != nil)
      {
        node = node.right;
        while (node.left != nil)
          node = node.left;
        return node;
      }

    Node parent = node.parent;
    // Exploit fact that nil.right == nil and node is non-nil.
    while (node == parent.right)
      {
        node = parent;
        parent = parent.parent;
      }
    return parent;
  }

  /**
   * Serializes this object to the given stream.
   *
   * @param s the stream to write to
   * @throws IOException if the underlying stream fails
   * @serialData the <i>size</i> (int), followed by key (Object) and value
   *             (Object) pairs in sorted order
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();

    Node node = firstNode();
    s.writeInt(size);
    while (node != nil)
      {
        s.writeObject(node.key);
        s.writeObject(node.value);
        node = successor(node);
      }
  }

  /**
   * Iterate over TreeMap's entries. This implementation is parameterized
   * to give a sequential view of keys, values, or entries.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private final class TreeIterator implements Iterator
  {
    /**
     * The type of this Iterator: {@link #KEYS}, {@link #VALUES},
     * or {@link #ENTRIES}.
     */
    private final int type;
    /** The number of modifications to the backing Map that we know about. */
    private int knownMod = modCount;
    /** The last Entry returned by a next() call. */
    private Node last;
    /** The next entry that should be returned by next(). */
    private Node next;
    /**
     * The last node visible to this iterator. This is used when iterating
     * on a SubMap.
     */
    private final Node max;

    /**
     * Construct a new TreeIterator with the supplied type.
     * @param type {@link #KEYS}, {@link #VALUES}, or {@link #ENTRIES}
     */
    TreeIterator(int type)
    {
      // FIXME gcj cannot handle this. Bug java/4695
      // this(type, firstNode(), nil);
      this.type = type;
      this.next = firstNode();
      this.max = nil;
    }

    /**
     * Construct a new TreeIterator with the supplied type. Iteration will
     * be from "first" (inclusive) to "max" (exclusive).
     *
     * @param type {@link #KEYS}, {@link #VALUES}, or {@link #ENTRIES}
     * @param first where to start iteration, nil for empty iterator
     * @param max the cutoff for iteration, nil for all remaining nodes
     */
    TreeIterator(int type, Node first, Node max)
    {
      this.type = type;
      this.next = first;
      this.max = max;
    }

    /**
     * Returns true if the Iterator has more elements.
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the TreeMap was modified
     */
    public boolean hasNext()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
      return next != max;
    }

    /**
     * Returns the next element in the Iterator's sequential view.
     * @return the next element
     * @throws ConcurrentModificationException if the TreeMap was modified
     * @throws NoSuchElementException if there is none
     */
    public Object next()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
      if (next == max)
        throw new NoSuchElementException();
      last = next;
      next = successor(last);

      if (type == VALUES)
        return last.value;
      else if (type == KEYS)
        return last.key;
      return last;
    }

    /**
     * Removes from the backing TreeMap the last element which was fetched
     * with the <code>next()</code> method.
     * @throws ConcurrentModificationException if the TreeMap was modified
     * @throws IllegalStateException if called when there is no last element
     */
    public void remove()
    {
      if (last == null)
        throw new IllegalStateException();
      if (knownMod != modCount)
        throw new ConcurrentModificationException();

      removeNode(last);
      last = null;
      knownMod++;
    }
  } // class TreeIterator

  /**
   * Implementation of {@link #subMap(Object, Object)} and other map
   * ranges. This class provides a view of a portion of the original backing
   * map, and throws {@link IllegalArgumentException} for attempts to
   * access beyond that range.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private final class SubMap extends AbstractMap implements SortedMap
  {
    /**
     * The lower range of this view, inclusive, or nil for unbounded.
     * Package visible for use by nested classes.
     */
    final Object minKey;

    /**
     * The upper range of this view, exclusive, or nil for unbounded.
     * Package visible for use by nested classes.
     */
    final Object maxKey;

    /**
     * The cache for {@link #entrySet()}.
     */
    private Set entries;

    /**
     * Create a SubMap representing the elements between minKey (inclusive)
     * and maxKey (exclusive). If minKey is nil, SubMap has no lower bound
     * (headMap). If maxKey is nil, the SubMap has no upper bound (tailMap).
     *
     * @param minKey the lower bound
     * @param maxKey the upper bound
     * @throws IllegalArgumentException if minKey &gt; maxKey
     */
    SubMap(Object minKey, Object maxKey)
    {
      if (minKey != nil && maxKey != nil && compare(minKey, maxKey) > 0)
        throw new IllegalArgumentException("fromKey > toKey");
      this.minKey = minKey;
      this.maxKey = maxKey;
    }

    /**
     * Check if "key" is in within the range bounds for this SubMap. The
     * lower ("from") SubMap range is inclusive, and the upper ("to") bound
     * is exclusive. Package visible for use by nested classes.
     *
     * @param key the key to check
     * @return true if the key is in range
     */
    boolean keyInRange(Object key)
    {
      return ((minKey == nil || compare(key, minKey) >= 0)
              && (maxKey == nil || compare(key, maxKey) < 0));
    }

    public void clear()
    {
      Node next = lowestGreaterThan(minKey, true);
      Node max = lowestGreaterThan(maxKey, false);
      while (next != max)
        {
          Node current = next;
          next = successor(current);
          removeNode(current);
        }
    }

    public Comparator comparator()
    {
      return comparator;
    }

    public boolean containsKey(Object key)
    {
      return keyInRange(key) && TreeMap.this.containsKey(key);
    }

    public boolean containsValue(Object value)
    {
      Node node = lowestGreaterThan(minKey, true);
      Node max = lowestGreaterThan(maxKey, false);
      while (node != max)
        {
          if (equals(value, node.getValue()))
            return true;
          node = successor(node);
        }
      return false;
    }

    public Set entrySet()
    {
      if (entries == null)
        // Create an AbstractSet with custom implementations of those methods
        // that can be overriden easily and efficiently.
        entries = new AbstractSet()
        {
          public int size()
          {
            return SubMap.this.size();
          }

          public Iterator iterator()
          {
            Node first = lowestGreaterThan(minKey, true);
            Node max = lowestGreaterThan(maxKey, false);
            return new TreeIterator(ENTRIES, first, max);
          }

          public void clear()
          {
            SubMap.this.clear();
          }

          public boolean contains(Object o)
          {
            if (! (o instanceof Map.Entry))
              return false;
            Map.Entry me = (Map.Entry) o;
            Object key = me.getKey();
            if (! keyInRange(key))
              return false;
            Node n = getNode(key);
            return n != nil && AbstractSet.equals(me.getValue(), n.value);
          }

          public boolean remove(Object o)
          {
            if (! (o instanceof Map.Entry))
              return false;
            Map.Entry me = (Map.Entry) o;
            Object key = me.getKey();
            if (! keyInRange(key))
              return false;
            Node n = getNode(key);
            if (n != nil && AbstractSet.equals(me.getValue(), n.value))
              {
                removeNode(n);
                return true;
              }
            return false;
          }
        };
      return entries;
    }

    public Object firstKey()
    {
      Node node = lowestGreaterThan(minKey, true);
      if (node == nil || ! keyInRange(node.key))
        throw new NoSuchElementException();
      return node.key;
    }

    public Object get(Object key)
    {
      if (keyInRange(key))
        return TreeMap.this.get(key);
      return null;
    }

    public SortedMap headMap(Object toKey)
    {
      if (! keyInRange(toKey))
        throw new IllegalArgumentException("key outside range");
      return new SubMap(minKey, toKey);
    }

    public Set keySet()
    {
      if (this.keys == null)
        // Create an AbstractSet with custom implementations of those methods
        // that can be overriden easily and efficiently.
        this.keys = new AbstractSet()
        {
          public int size()
          {
            return SubMap.this.size();
          }

          public Iterator iterator()
          {
            Node first = lowestGreaterThan(minKey, true);
            Node max = lowestGreaterThan(maxKey, false);
            return new TreeIterator(KEYS, first, max);
          }

          public void clear()
          {
            SubMap.this.clear();
          }

          public boolean contains(Object o)
          {
            if (! keyInRange(o))
              return false;
            return getNode(o) != nil;
          }

          public boolean remove(Object o)
          {
            if (! keyInRange(o))
              return false;
            Node n = getNode(o);
            if (n != nil)
              {
                removeNode(n);
                return true;
              }
            return false;
          }
        };
      return this.keys;
    }

    public Object lastKey()
    {
      Node node = highestLessThan(maxKey);
      if (node == nil || ! keyInRange(node.key))
        throw new NoSuchElementException();
      return node.key;
    }

    public Object put(Object key, Object value)
    {
      if (! keyInRange(key))
        throw new IllegalArgumentException("Key outside range");
      return TreeMap.this.put(key, value);
    }

    public Object remove(Object key)
    {
      if (keyInRange(key))
        return TreeMap.this.remove(key);
      return null;
    }

    public int size()
    {
      Node node = lowestGreaterThan(minKey, true);
      Node max = lowestGreaterThan(maxKey, false);
      int count = 0;
      while (node != max)
        {
          count++;
          node = successor(node);
        }
      return count;
    }

    public SortedMap subMap(Object fromKey, Object toKey)
    {
      if (! keyInRange(fromKey) || ! keyInRange(toKey))
        throw new IllegalArgumentException("key outside range");
      return new SubMap(fromKey, toKey);
    }

    public SortedMap tailMap(Object fromKey)
    {
      if (! keyInRange(fromKey))
        throw new IllegalArgumentException("key outside range");
      return new SubMap(fromKey, maxKey);
    }

    public Collection values()
    {
      if (this.values == null)
        // Create an AbstractCollection with custom implementations of those
        // methods that can be overriden easily and efficiently.
        this.values = new AbstractCollection()
        {
          public int size()
          {
            return SubMap.this.size();
          }

          public Iterator iterator()
          {
            Node first = lowestGreaterThan(minKey, true);
            Node max = lowestGreaterThan(maxKey, false);
            return new TreeIterator(VALUES, first, max);
          }

          public void clear()
          {
            SubMap.this.clear();
          }
        };
      return this.values;
    }
  } // class SubMap  
} // class TreeMap
