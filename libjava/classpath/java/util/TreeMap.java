/* TreeMap.java -- a class providing a basic Red-Black Tree data structure,
   mapping Object --> Object
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006  Free Software Foundation, Inc.

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


package java.util;

import gnu.java.lang.CPStringBuilder;

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
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see Map
 * @see HashMap
 * @see Hashtable
 * @see LinkedHashMap
 * @see Comparable
 * @see Comparator
 * @see Collection
 * @see Collections#synchronizedSortedMap(SortedMap)
 * @since 1.2
 * @status updated to 1.6
 */
public class TreeMap<K, V> extends AbstractMap<K, V>
  implements NavigableMap<K, V>, Cloneable, Serializable
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
  private transient Set<Map.Entry<K,V>> entries;

  /**
   * The cache for {@link #descendingMap()}.
   */
  private transient NavigableMap<K,V> descendingMap;

  /**
   * The cache for {@link #navigableKeySet()}.
   */
  private transient NavigableSet<K> nKeys;

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
  final Comparator<? super K> comparator;

  /**
   * Class to represent an entry in the tree. Holds a single key-value pair,
   * plus pointers to parent and child nodes.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class Node<K, V> extends AbstractMap.SimpleEntry<K, V>
  {
    // All fields package visible for use by nested classes.
    /** The color of this node. */
    int color;

    /** The left child node. */
    Node<K, V> left = nil;
    /** The right child node. */
    Node<K, V> right = nil;
    /** The parent node. */
    Node<K, V> parent = nil;

    /**
     * Simple constructor.
     * @param key the key
     * @param value the value
     */
    Node(K key, V value, int color)
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
   * @param c the sort order for the keys of this map, or null
   *        for the natural order
   */
  public TreeMap(Comparator<? super K> c)
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
  public TreeMap(Map<? extends K, ? extends V> map)
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
  public TreeMap(SortedMap<K, ? extends V> sm)
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
  public Comparator<? super K> comparator()
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
    return getNode((K) key) != nil;
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
  public Set<Map.Entry<K,V>> entrySet()
  {
    if (entries == null)
      // Create an AbstractSet with custom implementations of those methods
      // that can be overriden easily and efficiently.
      entries = new NavigableEntrySet();
    return entries;
  }

  /**
   * Returns the first (lowest) key in the map.
   *
   * @return the first key
   * @throws NoSuchElementException if the map is empty
   */
  public K firstKey()
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
  public V get(Object key)
  {
    // Exploit fact that nil.value == null.
    return getNode((K) key).value;
  }

  /**
   * Returns a view of this Map including all entries with keys less than
   * <code>toKey</code>. The returned map is backed by the original, so changes
   * in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned map does not include
   * the endpoint; if you want inclusion, pass the successor element
   * or call <code>headMap(toKey, true)</code>.  This is equivalent to
   * calling <code>headMap(toKey, false)</code>.
   *
   * @param toKey the (exclusive) cutoff point
   * @return a view of the map less than the cutoff
   * @throws ClassCastException if <code>toKey</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if toKey is null, but the comparator does not
   *         tolerate null elements
   */
  public SortedMap<K, V> headMap(K toKey)
  {
    return headMap(toKey, false);
  }

  /**
   * Returns a view of this Map including all entries with keys less than
   * (or equal to, if <code>inclusive</code> is true) <code>toKey</code>.
   * The returned map is backed by the original, so changes in one appear
   * in the other. The submap will throw an {@link IllegalArgumentException}
   * for any attempt to access or add an element beyond the specified cutoff. 
   *
   * @param toKey the cutoff point
   * @param inclusive true if the cutoff point should be included.
   * @return a view of the map less than (or equal to, if <code>inclusive</code>
   *         is true) the cutoff.
   * @throws ClassCastException if <code>toKey</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if toKey is null, but the comparator does not
   *         tolerate null elements
   */
  public NavigableMap<K, V> headMap(K toKey, boolean inclusive)
  {
    return new SubMap((K)(Object)nil, inclusive 
		      ? successor(getNode(toKey)).key : toKey);
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
  public Set<K> keySet()
  {
    if (keys == null)
      // Create an AbstractSet with custom implementations of those methods
      // that can be overriden easily and efficiently.
      keys = new KeySet();
    return keys;
  }

  /**
   * Returns the last (highest) key in the map.
   *
   * @return the last key
   * @throws NoSuchElementException if the map is empty
   */
  public K lastKey()
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
  public V put(K key, V value)
  {
    Node<K,V> current = root;
    Node<K,V> parent = nil;
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
  public void putAll(Map<? extends K, ? extends V> m)
  {
    Iterator itr = m.entrySet().iterator();
    int pos = m.size();
    while (--pos >= 0)
      {
        Map.Entry<K,V> e = (Map.Entry<K,V>) itr.next();
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
  public V remove(Object key)
  {
    Node<K, V> n = getNode((K)key);
    if (n == nil)
      return null;
    // Note: removeNode can alter the contents of n, so save value now.
    V result = n.value;
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
   * either end, pass in the successor element or call
   * {@link #subMap(K,boolean,K,boolean)}.  This call is equivalent to
   * <code>subMap(fromKey, true, toKey, false)</code>.
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
  public SortedMap<K, V> subMap(K fromKey, K toKey)
  {
    return subMap(fromKey, true, toKey, false);
  }

  /**
   * Returns a view of this Map including all entries with keys greater (or
   * equal to, if <code>fromInclusive</code> is true) <code>fromKey</code> and
   * less than (or equal to, if <code>toInclusive</code> is true)
   * <code>toKey</code>. The returned map is backed by the original, so
   * changes in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoffs. 
   *
   * @param fromKey the low cutoff point
   * @param fromInclusive true if the low cutoff point should be included.
   * @param toKey the high cutoff point
   * @param toInclusive true if the high cutoff point should be included.
   * @return a view of the map for the specified range.
   * @throws ClassCastException if either cutoff is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if fromKey or toKey is null, but the
   *         comparator does not tolerate null elements
   * @throws IllegalArgumentException if fromKey is greater than toKey
   */
  public NavigableMap<K, V> subMap(K fromKey, boolean fromInclusive,
				   K toKey, boolean toInclusive)
  {
    return new SubMap(fromInclusive ? fromKey : successor(getNode(fromKey)).key,
		      toInclusive ? successor(getNode(toKey)).key : toKey);
  }

  /**
   * Returns a view of this Map including all entries with keys greater or
   * equal to <code>fromKey</code>. The returned map is backed by the
   * original, so changes in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned map includes the
   * endpoint; if you want to exclude it, pass in the successor element.
   * This is equivalent to calling <code>tailMap(fromKey, true)</code>.
   *
   * @param fromKey the (inclusive) low cutoff point
   * @return a view of the map above the cutoff
   * @throws ClassCastException if <code>fromKey</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if fromKey is null, but the comparator
   *         does not tolerate null elements
   */
  public SortedMap<K, V> tailMap(K fromKey)
  {
    return tailMap(fromKey, true);
  }

  /**
   * Returns a view of this Map including all entries with keys greater or
   * equal to <code>fromKey</code>. The returned map is backed by the
   * original, so changes in one appear in the other. The submap will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned map includes the
   * endpoint; if you want to exclude it, pass in the successor element.
   *
   * @param fromKey the low cutoff point
   * @param inclusive true if the cutoff point should be included.
   * @return a view of the map above the cutoff
   * @throws ClassCastException if <code>fromKey</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if fromKey is null, but the comparator
   *         does not tolerate null elements
   */
  public NavigableMap<K, V> tailMap(K fromKey, boolean inclusive)
  {
    return new SubMap(inclusive ? fromKey : successor(getNode(fromKey)).key,
		      (K)(Object)nil);
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
  public Collection<V> values()
  {
    if (values == null)
      // We don't bother overriding many of the optional methods, as doing so
      // wouldn't provide any significant performance advantage.
      values = new AbstractCollection<V>()
      {
        public int size()
        {
          return size;
        }

        public Iterator<V> iterator()
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
  final int compare(K o1, K o2)
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
  private void deleteFixup(Node<K,V> node, Node<K,V> parent)
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
            Node<K,V> sibling = parent.right;
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
            Node<K,V> sibling = parent.left;
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
  final Node<K, V> firstNode()
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
  final Node<K, V> getNode(K key)
  {
    Node<K,V> current = root;
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
  final Node<K,V> highestLessThan(K key)
  {
    return highestLessThan(key, false);
  }

  /**
   * Find the "highest" node which is &lt; (or equal to,
   * if <code>equal</code> is true) key. If key is nil,
   * return last node. Package visible for use by nested
   * classes.
   *
   * @param key the upper bound, exclusive
   * @param equal true if the key should be returned if found.
   * @return the previous node
   */
  final Node<K,V> highestLessThan(K key, boolean equal)
  {
    if (key == nil)
      return lastNode();

    Node<K,V> last = nil;
    Node<K,V> current = root;
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
          return (equal ? last : predecessor(last));
      }
    return comparison < 0 ? predecessor(last) : last;
  }

  /**
   * Maintain red-black balance after inserting a new node.
   *
   * @param n the newly inserted node
   */
  private void insertFixup(Node<K,V> n)
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
  private Node<K,V> lastNode()
  {
    // Exploit fact that nil.right == nil.
    Node node = root;
    while (node.right != nil)
      node = node.right;
    return node;
  }

  /**
   * Find the "lowest" node which is &gt;= key. If key is nil, return either
   * nil or the first node, depending on the parameter first.  Package visible
   * for use by nested classes.
   *
   * @param key the lower bound, inclusive
   * @param first true to return the first element instead of nil for nil key
   * @return the next node
   */
  final Node<K,V> lowestGreaterThan(K key, boolean first)
  {
    return lowestGreaterThan(key, first, true);
  }

  /**
   * Find the "lowest" node which is &gt; (or equal to, if <code>equal</code>
   * is true) key. If key is nil, return either nil or the first node, depending
   * on the parameter first.  Package visible for use by nested classes.
   *
   * @param key the lower bound, inclusive
   * @param first true to return the first element instead of nil for nil key
   * @param equal true if the key should be returned if found.
   * @return the next node
   */
  final Node<K,V> lowestGreaterThan(K key, boolean first, boolean equal)
  {
    if (key == nil)
      return first ? firstNode() : nil;

    Node<K,V> last = nil;
    Node<K,V> current = root;
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
          return (equal ? current : successor(current));
      }
    return comparison > 0 ? successor(last) : last;
  }

  /**
   * Return the node preceding the given one, or nil if there isn't one.
   *
   * @param node the current node, not nil
   * @return the prior node in sorted order
   */
  private Node<K,V> predecessor(Node<K,V> node)
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
   * @param readValues true to read values, false to insert "" as the value
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
   * Package visible for use by TreeSet, which uses a value type of String.
   *
   * @param keys the iterator over the sorted keys
   * @param count the number of nodes to insert
   * @see TreeSet#TreeSet(SortedSet)
   */
  final void putKeysLinear(Iterator<K> keys, int count)
  {
    fabricateTree(count);
    Node<K,V> node = firstNode();

    while (--count >= 0)
      {
        node.key = keys.next();
        node.value = (V) "";
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
  final void removeNode(Node<K,V> node)
  {
    Node<K,V> splice;
    Node<K,V> child;

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
  private void rotateLeft(Node<K,V> node)
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
  private void rotateRight(Node<K,V> node)
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
  final Node<K,V> successor(Node<K,V> node)
  {
    if (node.right != nil)
      {
        node = node.right;
        while (node.left != nil)
          node = node.left;
        return node;
      }

    Node<K,V> parent = node.parent;
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
      this(type, firstNode(), nil);
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
     */
    public boolean hasNext()
    {
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
  private final class SubMap
    extends AbstractMap<K,V>
    implements NavigableMap<K,V>
  {
    /**
     * The lower range of this view, inclusive, or nil for unbounded.
     * Package visible for use by nested classes.
     */
    final K minKey;

    /**
     * The upper range of this view, exclusive, or nil for unbounded.
     * Package visible for use by nested classes.
     */
    final K maxKey;

    /**
     * The cache for {@link #entrySet()}.
     */
    private Set<Map.Entry<K,V>> entries;

    /**
     * The cache for {@link #descendingMap()}.
     */
    private NavigableMap<K,V> descendingMap;

    /**
     * The cache for {@link #navigableKeySet()}.
     */
    private NavigableSet<K> nKeys;

    /**
     * Create a SubMap representing the elements between minKey (inclusive)
     * and maxKey (exclusive). If minKey is nil, SubMap has no lower bound
     * (headMap). If maxKey is nil, the SubMap has no upper bound (tailMap).
     *
     * @param minKey the lower bound
     * @param maxKey the upper bound
     * @throws IllegalArgumentException if minKey &gt; maxKey
     */
    SubMap(K minKey, K maxKey)
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
    boolean keyInRange(K key)
    {
      return ((minKey == nil || compare(key, minKey) >= 0)
              && (maxKey == nil || compare(key, maxKey) < 0));
    }

    public Entry<K,V> ceilingEntry(K key)
    {
      Entry<K,V> n = TreeMap.this.ceilingEntry(key);
      if (n != null && keyInRange(n.getKey()))
	return n;
      return null;
    }

    public K ceilingKey(K key)
    {
      K found = TreeMap.this.ceilingKey(key);
      if (keyInRange(found))
	return found;
      else
	return null;
    }

    public NavigableSet<K> descendingKeySet()
    {
      return descendingMap().navigableKeySet();
    }

    public NavigableMap<K,V> descendingMap()
    {
      if (descendingMap == null)
	descendingMap = new DescendingMap(this);
      return descendingMap;
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

    public Comparator<? super K> comparator()
    {
      return comparator;
    }

    public boolean containsKey(Object key)
    {
      return keyInRange((K) key) && TreeMap.this.containsKey(key);
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

    public Set<Map.Entry<K,V>> entrySet()
    {
      if (entries == null)
        // Create an AbstractSet with custom implementations of those methods
        // that can be overriden easily and efficiently.
        entries = new SubMap.NavigableEntrySet();
      return entries;
    }

    public Entry<K,V> firstEntry()
    {
      Node<K,V> node = lowestGreaterThan(minKey, true);
      if (node == nil || ! keyInRange(node.key))
	return null;
      return node;
    }

    public K firstKey()
    {
      Entry<K,V> e = firstEntry();
      if (e == null)
        throw new NoSuchElementException();
      return e.getKey();
    }

    public Entry<K,V> floorEntry(K key)
    {
      Entry<K,V> n = TreeMap.this.floorEntry(key);
      if (n != null && keyInRange(n.getKey()))
	return n;
      return null;
    }

    public K floorKey(K key)
    {
      K found = TreeMap.this.floorKey(key);
      if (keyInRange(found))
	return found;
      else
	return null;
    }

    public V get(Object key)
    {
      if (keyInRange((K) key))
        return TreeMap.this.get(key);
      return null;
    }

    public SortedMap<K,V> headMap(K toKey)
    {
      return headMap(toKey, false);
    }

    public NavigableMap<K,V> headMap(K toKey, boolean inclusive)
    {
      if (!keyInRange(toKey))
        throw new IllegalArgumentException("Key outside submap range");
      return new SubMap(minKey, (inclusive ? 
				 successor(getNode(toKey)).key : toKey));
    }

    public Set<K> keySet()
    {
      if (this.keys == null)
        // Create an AbstractSet with custom implementations of those methods
        // that can be overriden easily and efficiently.
        this.keys = new SubMap.KeySet();
      return this.keys;
    }

    public Entry<K,V> higherEntry(K key)
    {
      Entry<K,V> n = TreeMap.this.higherEntry(key);
      if (n != null && keyInRange(n.getKey()))
	return n;
      return null;
    }

    public K higherKey(K key)
    {
      K found = TreeMap.this.higherKey(key);
      if (keyInRange(found))
	return found;
      else
	return null;
    }

    public Entry<K,V> lastEntry()
    {
      return lowerEntry(maxKey);
    }

    public K lastKey()
    {
      Entry<K,V> e = lastEntry();
      if (e == null)
        throw new NoSuchElementException();
      return e.getKey();
    }

    public Entry<K,V> lowerEntry(K key)
    {
      Entry<K,V> n = TreeMap.this.lowerEntry(key);
      if (n != null && keyInRange(n.getKey()))
	return n;
      return null;
    }

    public K lowerKey(K key)
    {
      K found = TreeMap.this.lowerKey(key);
      if (keyInRange(found))
	return found;
      else
	return null;
    }

    public NavigableSet<K> navigableKeySet()
    {
      if (this.nKeys == null)
        // Create an AbstractSet with custom implementations of those methods
        // that can be overriden easily and efficiently.
        this.nKeys = new SubMap.NavigableKeySet();
      return this.nKeys;    
    }

    public Entry<K,V> pollFirstEntry()
    {
      Entry<K,V> e = firstEntry();
      if (e != null)
	removeNode((Node<K,V>) e);
      return e;
    }

    public Entry<K,V> pollLastEntry()
    {
      Entry<K,V> e = lastEntry();
      if (e != null)
	removeNode((Node<K,V>) e);
      return e;
    }

    public V put(K key, V value)
    {
      if (! keyInRange(key))
        throw new IllegalArgumentException("Key outside range");
      return TreeMap.this.put(key, value);
    }

    public V remove(Object key)
    {
      if (keyInRange((K)key))
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

    public SortedMap<K,V> subMap(K fromKey, K toKey)
    {
      return subMap(fromKey, true, toKey, false);
    }

    public NavigableMap<K,V> subMap(K fromKey, boolean fromInclusive,
				    K toKey, boolean toInclusive)
    {
      if (! keyInRange(fromKey) || ! keyInRange(toKey))
        throw new IllegalArgumentException("key outside range");
      return new SubMap(fromInclusive ? fromKey : successor(getNode(fromKey)).key, 
			toInclusive ? successor(getNode(toKey)).key : toKey);
    }

    public SortedMap<K, V> tailMap(K fromKey)
    {
      return tailMap(fromKey, true);
    }
    
    public NavigableMap<K,V> tailMap(K fromKey, boolean inclusive)
    {
      if (! keyInRange(fromKey))
        throw new IllegalArgumentException("key outside range");
      return new SubMap(inclusive ? fromKey : successor(getNode(fromKey)).key,
			maxKey);
    }

    public Collection<V> values()
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

          public Iterator<V> iterator()
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
    
    private class KeySet
      extends AbstractSet<K>
    {
      public int size()
      {
	return SubMap.this.size();
      }
      
      public Iterator<K> iterator()
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
	if (! keyInRange((K) o))
	  return false;
	return getNode((K) o) != nil;
      }
      
      public boolean remove(Object o)
      {
	if (! keyInRange((K) o))
	  return false;
	Node n = getNode((K) o);
	if (n != nil)
	  {
	    removeNode(n);
	    return true;
	  }
	return false;
      }
      
    } // class SubMap.KeySet

    private final class NavigableKeySet
      extends KeySet
      implements NavigableSet<K>
    {

      public K ceiling(K k)
      {
	return SubMap.this.ceilingKey(k);
      }
      
      public Comparator<? super K> comparator()
      {
	return comparator;
      }
      
      public Iterator<K> descendingIterator()
      {
	return descendingSet().iterator();
      }
      
      public NavigableSet<K> descendingSet()
      {
	return new DescendingSet(this);
      }
      
      public K first()
      {
	return SubMap.this.firstKey();
      }
      
      public K floor(K k)
      {
	return SubMap.this.floorKey(k);
      }
      
      public SortedSet<K> headSet(K to)
      {
	return headSet(to, false);
      }

      public NavigableSet<K> headSet(K to, boolean inclusive)
      {
	return SubMap.this.headMap(to, inclusive).navigableKeySet();
      }

      public K higher(K k)
      {
	return SubMap.this.higherKey(k);
      }

      public K last()
      {
	return SubMap.this.lastKey();
      }

      public K lower(K k)
      {
	return SubMap.this.lowerKey(k);
      }

      public K pollFirst()
      {
	return SubMap.this.pollFirstEntry().getKey();
      }

      public K pollLast()
      {
	return SubMap.this.pollLastEntry().getKey();
      }

      public SortedSet<K> subSet(K from, K to)
      {
	return subSet(from, true, to, false);
      }
      
      public NavigableSet<K> subSet(K from, boolean fromInclusive,
				    K to, boolean toInclusive)
      {
	return SubMap.this.subMap(from, fromInclusive,
				  to, toInclusive).navigableKeySet();
      }

      public SortedSet<K> tailSet(K from)
      {
	return tailSet(from, true);
      }
      
      public NavigableSet<K> tailSet(K from, boolean inclusive)
      {
	return SubMap.this.tailMap(from, inclusive).navigableKeySet();
      }
      
  } // class SubMap.NavigableKeySet

  /**
   * Implementation of {@link #entrySet()}.
   */
  private class EntrySet
    extends AbstractSet<Entry<K,V>>
  {
    
    public int size()
    {
      return SubMap.this.size();
    }
    
    public Iterator<Map.Entry<K,V>> iterator()
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
      Map.Entry<K,V> me = (Map.Entry<K,V>) o;
      K key = me.getKey();
      if (! keyInRange(key))
	return false;
      Node<K,V> n = getNode(key);
      return n != nil && AbstractSet.equals(me.getValue(), n.value);
    }
    
    public boolean remove(Object o)
    {
      if (! (o instanceof Map.Entry))
	return false;
      Map.Entry<K,V> me = (Map.Entry<K,V>) o;
      K key = me.getKey();
      if (! keyInRange(key))
	return false;
      Node<K,V> n = getNode(key);
      if (n != nil && AbstractSet.equals(me.getValue(), n.value))
	{
	  removeNode(n);
	  return true;
	}
      return false;
    }
  } // class SubMap.EntrySet
    
    private final class NavigableEntrySet
      extends EntrySet
      implements NavigableSet<Entry<K,V>>
    {

      public Entry<K,V> ceiling(Entry<K,V> e)
      {
	return SubMap.this.ceilingEntry(e.getKey());
      }
      
      public Comparator<? super Entry<K,V>> comparator()
      {
	return new Comparator<Entry<K,V>>()
	  {
	    public int compare(Entry<K,V> t1, Entry<K,V> t2)
	      {
		return comparator.compare(t1.getKey(), t2.getKey());
	      }
	  };
      }
      
      public Iterator<Entry<K,V>> descendingIterator()
      {
	return descendingSet().iterator();
      }
      
      public NavigableSet<Entry<K,V>> descendingSet()
      {
	return new DescendingSet(this);
      }
      
      public Entry<K,V> first()
      {
	return SubMap.this.firstEntry();
      }
      
      public Entry<K,V> floor(Entry<K,V> e)
      {
	return SubMap.this.floorEntry(e.getKey());
      }
      
      public SortedSet<Entry<K,V>> headSet(Entry<K,V> to)
      {
	return headSet(to, false);
      }

      public NavigableSet<Entry<K,V>> headSet(Entry<K,V> to, boolean inclusive)
      {
	return (NavigableSet<Entry<K,V>>)
	  SubMap.this.headMap(to.getKey(), inclusive).entrySet();
      }

      public Entry<K,V> higher(Entry<K,V> e)
      {
	return SubMap.this.higherEntry(e.getKey());
      }

      public Entry<K,V> last()
      {
	return SubMap.this.lastEntry();
      }

      public Entry<K,V> lower(Entry<K,V> e)
      {
	return SubMap.this.lowerEntry(e.getKey());
      }

      public Entry<K,V> pollFirst()
      {
	return SubMap.this.pollFirstEntry();
      }

      public Entry<K,V> pollLast()
      {
	return SubMap.this.pollLastEntry();
      }

      public SortedSet<Entry<K,V>> subSet(Entry<K,V> from, Entry<K,V> to)
      {
	return subSet(from, true, to, false);
      }
      
      public NavigableSet<Entry<K,V>> subSet(Entry<K,V> from, boolean fromInclusive,
					     Entry<K,V> to, boolean toInclusive)
      {
	return (NavigableSet<Entry<K,V>>)
	  SubMap.this.subMap(from.getKey(), fromInclusive,
			     to.getKey(), toInclusive).entrySet();
      }

      public SortedSet<Entry<K,V>> tailSet(Entry<K,V> from)
      {
	return tailSet(from, true);
      }
      
      public NavigableSet<Entry<K,V>> tailSet(Entry<K,V> from, boolean inclusive)
      {
	return (NavigableSet<Entry<K,V>>)
	  SubMap.this.tailMap(from.getKey(), inclusive).navigableKeySet();
      }
      
  } // class SubMap.NavigableEntrySet

} // class SubMap  

  /**
   * Returns the entry associated with the least or lowest key
   * that is greater than or equal to the specified key, or
   * <code>null</code> if there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the entry with the least key greater than or equal
   *         to the given key, or <code>null</code> if there is
   *         no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public Entry<K,V> ceilingEntry(K key)
  {
    Node<K,V> n = lowestGreaterThan(key, false);
    return (n == nil) ? null : n;
  }

  /**
   * Returns the the least or lowest key that is greater than
   * or equal to the specified key, or <code>null</code> if
   * there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the least key greater than or equal to the given key,
   *         or <code>null</code> if there is no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public K ceilingKey(K key)
  {
    Entry<K,V> e = ceilingEntry(key);
    return (e == null) ? null : e.getKey();
  }

  /**
   * Returns a reverse ordered {@link NavigableSet} view of this
   * map's keys. The set is backed by the {@link TreeMap}, so changes
   * in one show up in the other.  The set supports element removal,
   * but not element addition.
   *
   * @return a reverse ordered set view of the keys.
   * @since 1.6
   * @see #descendingMap()
   */
  public NavigableSet<K> descendingKeySet()
  {
    return descendingMap().navigableKeySet();
  }

  /**
   * Returns a view of the map in reverse order.  The descending map
   * is backed by the original map, so that changes affect both maps.
   * Any changes occurring to either map while an iteration is taking
   * place (with the exception of a {@link Iterator#remove()} operation)
   * result in undefined behaviour from the iteration.  The ordering
   * of the descending map is the same as for a map with a
   * {@link Comparator} given by {@link Collections#reverseOrder()},
   * and calling {@link #descendingMap()} on the descending map itself
   * results in a view equivalent to the original map.
   *
   * @return a reverse order view of the map.
   * @since 1.6
   */
  public NavigableMap<K,V> descendingMap()
  {
    if (descendingMap == null)
      descendingMap = new DescendingMap<K,V>(this);
    return descendingMap;
  }

  /**
   * Returns the entry associated with the least or lowest key
   * in the map, or <code>null</code> if the map is empty.
   *
   * @return the lowest entry, or <code>null</code> if the map
   *         is empty.
   * @since 1.6
   */
  public Entry<K,V> firstEntry()
  {
    Node<K,V> n = firstNode();
    return (n == nil) ? null : n;
  }

  /**
   * Returns the entry associated with the greatest or highest key
   * that is less than or equal to the specified key, or
   * <code>null</code> if there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the entry with the greatest key less than or equal
   *         to the given key, or <code>null</code> if there is
   *         no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public Entry<K,V> floorEntry(K key)
  {
    Node<K,V> n = highestLessThan(key, true);
    return (n == nil) ? null : n;
  }

  /**
   * Returns the the greatest or highest key that is less than
   * or equal to the specified key, or <code>null</code> if
   * there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the greatest key less than or equal to the given key,
   *         or <code>null</code> if there is no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public K floorKey(K key)
  {
    Entry<K,V> e = floorEntry(key);
    return (e == null) ? null : e.getKey();
  }

  /**
   * Returns the entry associated with the least or lowest key
   * that is strictly greater than the specified key, or
   * <code>null</code> if there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the entry with the least key greater than 
   *         the given key, or <code>null</code> if there is
   *         no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public Entry<K,V> higherEntry(K key)
  {
    Node<K,V> n = lowestGreaterThan(key, false, false);
    return (n == nil) ? null : n;
  }

  /**
   * Returns the the least or lowest key that is strictly
   * greater than the specified key, or <code>null</code> if
   * there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the least key greater than the given key,
   *         or <code>null</code> if there is no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public K higherKey(K key)
  {
    Entry<K,V> e = higherEntry(key);
    return (e == null) ? null : e.getKey();
  }

  /**
   * Returns the entry associated with the greatest or highest key
   * in the map, or <code>null</code> if the map is empty.
   *
   * @return the highest entry, or <code>null</code> if the map
   *         is empty.
   * @since 1.6
   */
  public Entry<K,V> lastEntry()
  {
    Node<K,V> n = lastNode();
    return (n == nil) ? null : n;
  }

  /**
   * Returns the entry associated with the greatest or highest key
   * that is strictly less than the specified key, or
   * <code>null</code> if there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the entry with the greatest key less than 
   *         the given key, or <code>null</code> if there is
   *         no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public Entry<K,V> lowerEntry(K key)
  {
    Node<K,V> n = highestLessThan(key);
    return (n == nil) ? null : n;
  }

  /**
   * Returns the the greatest or highest key that is strictly
   * less than the specified key, or <code>null</code> if
   * there is no such key.
   *
   * @param key the key relative to the returned entry.
   * @return the greatest key less than the given key,
   *         or <code>null</code> if there is no such key.
   * @throws ClassCastException if the specified key can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the key is <code>null</code>
   *                              and this map either uses natural
   *                              ordering or a comparator that does
   *                              not permit null keys.
   * @since 1.6
   */
  public K lowerKey(K key)
  {
    Entry<K,V> e = lowerEntry(key);
    return (e == null) ? null : e.getKey();
  }

  /**
   * Returns a {@link NavigableSet} view of this map's keys. The set is
   * backed by the {@link TreeMap}, so changes in one show up in the other.
   * Any changes occurring to either while an iteration is taking
   * place (with the exception of a {@link Iterator#remove()} operation)
   * result in undefined behaviour from the iteration.  The ordering
   * The set supports element removal, but not element addition.
   *
   * @return a {@link NavigableSet} view of the keys.
   * @since 1.6
   */
  public NavigableSet<K> navigableKeySet()
  {
    if (nKeys == null)
      nKeys = new NavigableKeySet();
    return nKeys;
  }

  /**
   * Removes and returns the entry associated with the least
   * or lowest key in the map, or <code>null</code> if the map
   * is empty.
   *
   * @return the removed first entry, or <code>null</code> if the
   *         map is empty.
   * @since 1.6
   */
  public Entry<K,V> pollFirstEntry()
  {
    Entry<K,V> e = firstEntry();
    if (e != null)
      removeNode((Node<K,V>)e);
    return e;
  }

  /**
   * Removes and returns the entry associated with the greatest
   * or highest key in the map, or <code>null</code> if the map
   * is empty.
   *
   * @return the removed last entry, or <code>null</code> if the
   *         map is empty.
   * @since 1.6
   */
  public Entry<K,V> pollLastEntry()
  {
    Entry<K,V> e = lastEntry();
    if (e != null)
      removeNode((Node<K,V>)e);
    return e;    
  }

  /**
   * Implementation of {@link #descendingMap()} and associated
   * derivatives. This class provides a view of the
   * original backing map in reverse order, and throws
   * {@link IllegalArgumentException} for attempts to
   * access beyond that range.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static final class DescendingMap<DK,DV>
    implements NavigableMap<DK,DV>
  {

    /**
     * The cache for {@link #entrySet()}.
     */
    private Set<Map.Entry<DK,DV>> entries;

    /**
     * The cache for {@link #keySet()}.
     */
    private Set<DK> keys;

    /**
     * The cache for {@link #navigableKeySet()}.
     */
    private NavigableSet<DK> nKeys;

    /**
     * The cache for {@link #values()}.
     */
    private Collection<DV> values;

    /**
     * The backing {@link NavigableMap}.
     */
    private NavigableMap<DK,DV> map;

    /**
     * Create a {@link DescendingMap} around the specified
     * map.
     *
     * @param map the map to wrap.
     */
    public DescendingMap(NavigableMap<DK,DV> map)
    {
      this.map = map;
    }
      
    public Map.Entry<DK,DV> ceilingEntry(DK key)
    {
      return map.floorEntry(key);
    }

    public DK ceilingKey(DK key)
    {
      return map.floorKey(key);
    }

    public void clear()
    {
      map.clear();
    }

    public Comparator<? super DK> comparator()
    {
      return Collections.reverseOrder(map.comparator());
    }

    public boolean containsKey(Object o)
    {
      return map.containsKey(o);
    }
    
    public boolean containsValue(Object o)
    {
      return map.containsValue(o);
    }

    public NavigableSet<DK> descendingKeySet()
    {
      return descendingMap().navigableKeySet();
    }

    public NavigableMap<DK,DV> descendingMap()
    {
      return map;
    }

    public Set<Entry<DK,DV>> entrySet()
    {
      if (entries == null)
	entries =
	  new DescendingSet<Entry<DK,DV>>((NavigableSet<Entry<DK,DV>>)
					  map.entrySet());
      return entries;
    }

    public boolean equals(Object o)
    {
      return map.equals(o);
    }

    public Entry<DK,DV> firstEntry()
    {
      return map.lastEntry();
    }

    public DK firstKey()
    {
      return map.lastKey();
    }

    public Entry<DK,DV> floorEntry(DK key)
    {
      return map.ceilingEntry(key);
    }

    public DK floorKey(DK key)
    {
      return map.ceilingKey(key);
    }

    public DV get(Object key)
    {
      return map.get(key);
    }

    public int hashCode()
    {
      return map.hashCode();
    }

    public SortedMap<DK,DV> headMap(DK toKey)
    {
      return headMap(toKey, false);
    }

    public NavigableMap<DK,DV> headMap(DK toKey, boolean inclusive)
    {
      return new DescendingMap(map.tailMap(toKey, inclusive));
    }

    public Entry<DK,DV> higherEntry(DK key)
    {
      return map.lowerEntry(key);
    }

    public DK higherKey(DK key)
    {
      return map.lowerKey(key);
    }

    public Set<DK> keySet()
    {
      if (keys == null)
	keys = new DescendingSet<DK>(map.navigableKeySet());
      return keys;
    }

    public boolean isEmpty()
    {
      return map.isEmpty();
    }

    public Entry<DK,DV> lastEntry()
    {
      return map.firstEntry();
    }

    public DK lastKey()
    {
      return map.firstKey();
    }

    public Entry<DK,DV> lowerEntry(DK key)
    {
      return map.higherEntry(key);
    }

    public DK lowerKey(DK key)
    {
      return map.higherKey(key);
    }

    public NavigableSet<DK> navigableKeySet()
    {
      if (nKeys == null)
	nKeys = new DescendingSet<DK>(map.navigableKeySet());
      return nKeys;
    }

    public Entry<DK,DV> pollFirstEntry()
    {
      return pollLastEntry();
    }

    public Entry<DK,DV> pollLastEntry()
    {
      return pollFirstEntry();
    }

    public DV put(DK key, DV value)
    {
      return map.put(key, value);
    }

    public void putAll(Map<? extends DK, ? extends DV> m)
    {
      map.putAll(m);
    }

    public DV remove(Object key)
    {
      return map.remove(key);
    }

    public int size()
    {
      return map.size();
    }

    public SortedMap<DK,DV> subMap(DK fromKey, DK toKey)
    {
      return subMap(fromKey, true, toKey, false);
    }

    public NavigableMap<DK,DV> subMap(DK fromKey, boolean fromInclusive,
				      DK toKey, boolean toInclusive)
    {
      return new DescendingMap(map.subMap(fromKey, fromInclusive,
					  toKey, toInclusive));
    }

    public SortedMap<DK,DV> tailMap(DK fromKey)
    {
      return tailMap(fromKey, true);
    }

    public NavigableMap<DK,DV> tailMap(DK fromKey, boolean inclusive)
    {
      return new DescendingMap(map.headMap(fromKey, inclusive));
    }

    public String toString()
    {
      CPStringBuilder r = new CPStringBuilder("{");
      final Iterator<Entry<DK,DV>> it = entrySet().iterator();
      while (it.hasNext())
      {
	final Entry<DK,DV> e = it.next();
        r.append(e.getKey());
        r.append('=');
        r.append(e.getValue());
	r.append(", ");
      }
      r.replace(r.length() - 2, r.length(), "}");
      return r.toString();
    }

    public Collection<DV> values()
    {
      if (values == null)
        // Create an AbstractCollection with custom implementations of those
        // methods that can be overriden easily and efficiently.
        values = new AbstractCollection()
	  {
	    public int size()
	    {
	      return size();
	    }
	    
	    public Iterator<DV> iterator()
	    {
	      return new Iterator<DV>()
		{	  
		  /** The last Entry returned by a next() call. */
		  private Entry<DK,DV> last;
		  
		  /** The next entry that should be returned by next(). */
		  private Entry<DK,DV> next = firstEntry();
		  
		  public boolean hasNext()
		  {
		    return next != null;
		  }

		  public DV next()
		  {
		    if (next == null)
		      throw new NoSuchElementException();
		    last = next;
		    next = higherEntry(last.getKey());
		    
		    return last.getValue();
		  }

		  public void remove()
		  {
		    if (last == null)
		      throw new IllegalStateException();
		    
		    DescendingMap.this.remove(last.getKey());
		    last = null;
		  }
		};
	    }
	    
	    public void clear()
	    {
	      clear();
	    }
	  };
      return values;
    }

  } // class DescendingMap

  /**
   * Implementation of {@link #keySet()}.
   */
  private class KeySet
    extends AbstractSet<K>
  {

    public int size()
    {
      return size;
    }

    public Iterator<K> iterator()
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
      Node<K,V> n = getNode((K) key);
      if (n == nil)
	return false;
      removeNode(n);
      return true;
    }
  } // class KeySet

  /**
   * Implementation of {@link #navigableKeySet()}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private final class NavigableKeySet
    extends KeySet
    implements NavigableSet<K>
  {

    public K ceiling(K k)
    {
      return ceilingKey(k);
    }

    public Comparator<? super K> comparator()
    {
      return comparator;
    }

    public Iterator<K> descendingIterator()
    {
      return descendingSet().iterator();
    }

    public NavigableSet<K> descendingSet()
    {
      return new DescendingSet<K>(this);
    }

    public K first()
    {
      return firstKey();
    }

    public K floor(K k)
    {
      return floorKey(k);
    }

    public SortedSet<K> headSet(K to)
    {
      return headSet(to, false);
    }

    public NavigableSet<K> headSet(K to, boolean inclusive)
    {
      return headMap(to, inclusive).navigableKeySet();
    }

    public K higher(K k)
    {
      return higherKey(k);
    }

    public K last()
    {
      return lastKey();
    }

    public K lower(K k)
    {
      return lowerKey(k);
    }

    public K pollFirst()
    {
      return pollFirstEntry().getKey();
    }

    public K pollLast()
    {
      return pollLastEntry().getKey();
    }

    public SortedSet<K> subSet(K from, K to)
    {
      return subSet(from, true, to, false);
    }

    public NavigableSet<K> subSet(K from, boolean fromInclusive,
				  K to, boolean toInclusive)
    {
      return subMap(from, fromInclusive,
		    to, toInclusive).navigableKeySet();
    }

    public SortedSet<K> tailSet(K from)
    {
      return tailSet(from, true);
    }

    public NavigableSet<K> tailSet(K from, boolean inclusive)
    {
      return tailMap(from, inclusive).navigableKeySet();
    }


  } // class NavigableKeySet

  /**
   * Implementation of {@link #descendingSet()} and associated
   * derivatives. This class provides a view of the
   * original backing set in reverse order, and throws
   * {@link IllegalArgumentException} for attempts to
   * access beyond that range.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static final class DescendingSet<D>
    implements NavigableSet<D>
  {

    /**
     * The backing {@link NavigableSet}.
     */
    private NavigableSet<D> set;

    /**
     * Create a {@link DescendingSet} around the specified
     * set.
     *
     * @param map the set to wrap.
     */
    public DescendingSet(NavigableSet<D> set)
    {
      this.set = set;
    }
    
    public boolean add(D e)
    {
      return set.add(e);
    }

    public boolean addAll(Collection<? extends D> c)
    {
      return set.addAll(c);
    }

    public D ceiling(D e)
    {
      return set.floor(e);
    }

    public void clear()
    {
      set.clear();
    }

    public Comparator<? super D> comparator()
    {
      return Collections.reverseOrder(set.comparator());
    }

    public boolean contains(Object o)
    {
      return set.contains(o);
    }

    public boolean containsAll(Collection<?> c)
    {
      return set.containsAll(c);
    }

    public Iterator<D> descendingIterator()
    {
      return descendingSet().iterator();
    }

    public NavigableSet<D> descendingSet()
    {
      return set;
    }

    public boolean equals(Object o)
    {
      return set.equals(o);
    }

    public D first()
    {
      return set.last();
    }

    public D floor(D e)
    {
      return set.ceiling(e);
    }

    public int hashCode()
    {
      return set.hashCode();
    }

    public SortedSet<D> headSet(D to)
    {
      return headSet(to, false);
    }

    public NavigableSet<D> headSet(D to, boolean inclusive)
    {
      return new DescendingSet(set.tailSet(to, inclusive));
    }

    public D higher(D e)
    {
      return set.lower(e);
    }

    public boolean isEmpty()
    {
      return set.isEmpty();
    }

    public Iterator<D> iterator()
    {
      return new Iterator<D>()
	{
	  	  
	  /** The last element returned by a next() call. */
	  private D last;
		  
	  /** The next element that should be returned by next(). */
	  private D next = first();
		  
	  public boolean hasNext()
	  {
	    return next != null;
	  }

	  public D next()
	  {
	    if (next == null)
	      throw new NoSuchElementException();
	    last = next;
	    next = higher(last);
		    
	    return last;
	  }

	  public void remove()
	  {
	    if (last == null)
	      throw new IllegalStateException();
	    
	    DescendingSet.this.remove(last);
	    last = null;
	  }
	};
    }

    public D last()
    {
      return set.first();
    }

    public D lower(D e)
    {
      return set.higher(e);
    }

    public D pollFirst()
    {
      return set.pollLast();
    }

    public D pollLast()
    {
      return set.pollFirst();
    }

    public boolean remove(Object o)
    {
      return set.remove(o);
    }

    public boolean removeAll(Collection<?> c)
    {
      return set.removeAll(c);
    }

    public boolean retainAll(Collection<?> c)
    {
      return set.retainAll(c);
    }

    public int size()
    {
      return set.size();
    }

    public SortedSet<D> subSet(D from, D to)
    {
      return subSet(from, true, to, false);
    }

    public NavigableSet<D> subSet(D from, boolean fromInclusive,
				  D to, boolean toInclusive)
    {
      return new DescendingSet(set.subSet(from, fromInclusive,
					  to, toInclusive));
    }

    public SortedSet<D> tailSet(D from)
    {
      return tailSet(from, true);
    }

    public NavigableSet<D> tailSet(D from, boolean inclusive)
    {
      return new DescendingSet(set.headSet(from, inclusive));
    }

    public Object[] toArray()
    {
      D[] array = (D[]) set.toArray();
      Arrays.sort(array, comparator());
      return array;
    }

    public <T> T[] toArray(T[] a)
    {
      T[] array = set.toArray(a);
      Arrays.sort(array, (Comparator<? super T>) comparator());
      return array;
    }

    public String toString()
    {
      CPStringBuilder r = new CPStringBuilder("[");
      final Iterator<D> it = iterator();
      while (it.hasNext())
      {
	final D o = it.next();
	if (o == this)
	  r.append("<this>");
	else
	  r.append(o);
	r.append(", ");
      }
      r.replace(r.length() - 2, r.length(), "]");
      return r.toString();
    }

  } // class DescendingSet

  private class EntrySet
    extends AbstractSet<Entry<K,V>>
  {
    public int size()
    {
      return size;
    }
    
    public Iterator<Map.Entry<K,V>> iterator()
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
      Map.Entry<K,V> me = (Map.Entry<K,V>) o;
      Node<K,V> n = getNode(me.getKey());
      return n != nil && AbstractSet.equals(me.getValue(), n.value);
    }
    
    public boolean remove(Object o)
    {
      if (! (o instanceof Map.Entry))
	return false;
      Map.Entry<K,V> me = (Map.Entry<K,V>) o;
      Node<K,V> n = getNode(me.getKey());
      if (n != nil && AbstractSet.equals(me.getValue(), n.value))
	{
	  removeNode(n);
	  return true;
	}
      return false;
    }
  }
  
  private final class NavigableEntrySet
    extends EntrySet
    implements NavigableSet<Entry<K,V>>
  {
    
    public Entry<K,V> ceiling(Entry<K,V> e)
    {
      return ceilingEntry(e.getKey());
    }
      
    public Comparator<? super Entry<K,V>> comparator()
    {
      return new Comparator<Entry<K,V>>()
	{
	  public int compare(Entry<K,V> t1, Entry<K,V> t2)
	  {
	    return comparator.compare(t1.getKey(), t2.getKey());
	  }
	};
    }
    
    public Iterator<Entry<K,V>> descendingIterator()
    {
      return descendingSet().iterator();
    }
    
    public NavigableSet<Entry<K,V>> descendingSet()
    {
      return new DescendingSet(this);
    }
    
    public Entry<K,V> first()
    {
      return firstEntry();
    }
      
    public Entry<K,V> floor(Entry<K,V> e)
    {
      return floorEntry(e.getKey());
    }
      
    public SortedSet<Entry<K,V>> headSet(Entry<K,V> to)
    {
      return headSet(to, false);
    }

    public NavigableSet<Entry<K,V>> headSet(Entry<K,V> to, boolean inclusive)
    {
      return (NavigableSet<Entry<K,V>>) headMap(to.getKey(), inclusive).entrySet();
    }
    
    public Entry<K,V> higher(Entry<K,V> e)
    {
      return higherEntry(e.getKey());
    }

    public Entry<K,V> last()
    {
      return lastEntry();
    }

    public Entry<K,V> lower(Entry<K,V> e)
    {
      return lowerEntry(e.getKey());
    }

    public Entry<K,V> pollFirst()
    {
      return pollFirstEntry();
    }

    public Entry<K,V> pollLast()
    {
      return pollLastEntry();
    }

    public SortedSet<Entry<K,V>> subSet(Entry<K,V> from, Entry<K,V> to)
    {
      return subSet(from, true, to, false);
    }
    
    public NavigableSet<Entry<K,V>> subSet(Entry<K,V> from, boolean fromInclusive,
					   Entry<K,V> to, boolean toInclusive)
    {
      return (NavigableSet<Entry<K,V>>) subMap(from.getKey(), fromInclusive,
					       to.getKey(), toInclusive).entrySet();
    }

    public SortedSet<Entry<K,V>> tailSet(Entry<K,V> from)
    {
      return tailSet(from, true);
    }
    
    public NavigableSet<Entry<K,V>> tailSet(Entry<K,V> from, boolean inclusive)
    {
      return (NavigableSet<Entry<K,V>>) tailMap(from.getKey(), inclusive).navigableKeySet();
    }
    
  } // class NavigableEntrySet

} // class TreeMap
