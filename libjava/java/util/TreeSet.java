/* TreeSet.java -- a class providing a TreeMap-backed SortedSet
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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
import java.io.Serializable;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * This class provides a TreeMap-backed implementation of the SortedSet
 * interface. The elements will be sorted according to their <i>natural
 * order</i>, or according to the provided <code>Comparator</code>.<p>
 *
 * Most operations are O(log n), but there is so much overhead that this
 * makes small sets expensive. Note that the ordering must be <i>consistent
 * with equals</i> to correctly implement the Set interface. If this
 * condition is violated, the set is still well-behaved, but you may have
 * suprising results when comparing it to other sets.<p>
 *
 * This implementation is not synchronized. If you need to share this between
 * multiple threads, do something like:<br>
 * <code>SortedSet s
 *       = Collections.synchronizedSortedSet(new TreeSet(...));</code><p>
 *
 * The iterators are <i>fail-fast</i>, meaning that any structural
 * modification, except for <code>remove()</code> called on the iterator
 * itself, cause the iterator to throw a
 * <code>ConcurrentModificationException</code> rather than exhibit
 * non-deterministic behavior.
 *
 * @author Jon Zeppieri
 * @author Bryce McKinlay
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Collection
 * @see Set
 * @see HashSet
 * @see LinkedHashSet
 * @see Comparable
 * @see Comparator
 * @see Collections#synchronizedSortedSet(SortedSet)
 * @see TreeMap
 * @since 1.2
 * @status updated to 1.4
 */
public class TreeSet extends AbstractSet
  implements SortedSet, Cloneable, Serializable
{
  /**
   * Compatible with JDK 1.2.
   */
  private static final long serialVersionUID = -2479143000061671589L;

  /**
   * The SortedMap which backs this Set.
   */
  // Not final because of readObject. This will always be one of TreeMap or
  // TreeMap.SubMap, which both extend AbstractMap.
  private transient SortedMap map;

  /**
   * Construct a new TreeSet whose backing TreeMap using the "natural"
   * ordering of keys. Elements that are not mutually comparable will cause
   * ClassCastExceptions down the road.
   *
   * @see Comparable
   */
  public TreeSet()
  {
    map = new TreeMap();
  }

  /**
   * Construct a new TreeSet whose backing TreeMap uses the supplied
   * Comparator. Elements that are not mutually comparable will cause
   * ClassCastExceptions down the road.
   *
   * @param comparator the Comparator this Set will use
   */
  public TreeSet(Comparator comparator)
  {
    map = new TreeMap(comparator);
  }

  /**
   * Construct a new TreeSet whose backing TreeMap uses the "natural"
   * orering of the keys and which contains all of the elements in the
   * supplied Collection. This runs in n*log(n) time.
   *
   * @param collection the new Set will be initialized with all
   *        of the elements in this Collection
   * @throws ClassCastException if the elements of the collection are not
   *         comparable
   * @throws NullPointerException if the collection is null
   * @see Comparable
   */
  public TreeSet(Collection collection)
  {
    map = new TreeMap();
    addAll(collection);
  }

  /**
   * Construct a new TreeSet, using the same key ordering as the supplied
   * SortedSet and containing all of the elements in the supplied SortedSet.
   * This constructor runs in linear time.
   *
   * @param sortedSet the new TreeSet will use this SortedSet's comparator
   *        and will initialize itself with all its elements
   * @throws NullPointerException if sortedSet is null
   */
  public TreeSet(SortedSet sortedSet)
  {
    map = new TreeMap(sortedSet.comparator());
    Iterator itr = sortedSet.iterator();
    ((TreeMap) map).putKeysLinear(itr, sortedSet.size());
  }

  /**
   * This private constructor is used to implement the subSet() calls around
   * a backing TreeMap.SubMap.
   *
   * @param backingMap the submap
   */
  private TreeSet(SortedMap backingMap)
  {
    map = backingMap;
  }

  /**
   * Adds the spplied Object to the Set if it is not already in the Set;
   * returns true if the element is added, false otherwise.
   *
   * @param obj the Object to be added to this Set
   * @throws ClassCastException if the element cannot be compared with objects
   *         already in the set
   */
  public boolean add(Object obj)
  {
    return map.put(obj, "") == null;
  }

  /**
   * Adds all of the elements in the supplied Collection to this TreeSet.
   *
   * @param c The collection to add
   * @return true if the Set is altered, false otherwise
   * @throws NullPointerException if c is null
   * @throws ClassCastException if an element in c cannot be compared with
   *         objects already in the set
   */
  public boolean addAll(Collection c)
  {
    boolean result = false;
    int pos = c.size();
    Iterator itr = c.iterator();
    while (--pos >= 0)
      result |= (map.put(itr.next(), "") == null);
    return result;
  }

  /**
   * Removes all elements in this Set.
   */
  public void clear()
  {
    map.clear();
  }

  /**
   * Returns a shallow copy of this Set. The elements are not cloned.
   *
   * @return the cloned set
   */
  public Object clone()
  {
    TreeSet copy = null;
    try
      {
        copy = (TreeSet) super.clone();
        // Map may be either TreeMap or TreeMap.SubMap, hence the ugly casts.
        copy.map = (SortedMap) ((AbstractMap) map).clone();
      }
    catch (CloneNotSupportedException x)
      {
        // Impossible result.
      }
    return copy;
  }

  /**
   * Returns this Set's comparator.
   *
   * @return the comparator, or null if the set uses natural ordering
   */
  public Comparator comparator()
  {
    return map.comparator();
  }

  /**
   * Returns true if this Set contains the supplied Object, false otherwise.
   *
   * @param obj the Object to check for
   * @return true if it is in the set
   * @throws ClassCastException if obj cannot be compared with objects
   *         already in the set
   */
  public boolean contains(Object obj)
  {
    return map.containsKey(obj);
  }

  /**
   * Returns the first (by order) element in this Set.
   *
   * @return the first element
   * @throws NoSuchElementException if the set is empty
   */
  public Object first()
  {
    return map.firstKey();
  }

  /**
   * Returns a view of this Set including all elements less than
   * <code>to</code>. The returned set is backed by the original, so changes
   * in one appear in the other. The subset will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned set does not include
   * the endpoint; if you want inclusion, pass the successor element.
   *
   * @param to the (exclusive) cutoff point
   * @return a view of the set less than the cutoff
   * @throws ClassCastException if <code>to</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if to is null, but the comparator does not
   *         tolerate null elements
   */
  public SortedSet headSet(Object to)
  {
    return new TreeSet(map.headMap(to));
  }

  /**
   * Returns true if this Set has size 0, false otherwise.
   *
   * @return true if the set is empty
   */
  public boolean isEmpty()
  {
    return map.isEmpty();
  }

  /**
   * Returns in Iterator over the elements in this TreeSet, which traverses
   * in ascending order.
   *
   * @return an iterator
   */
  public Iterator iterator()
  {
    return map.keySet().iterator();
  }

  /**
   * Returns the last (by order) element in this Set.
   *
   * @return the last element
   * @throws NoSuchElementException if the set is empty
   */
  public Object last()
  {
    return map.lastKey();
  }

  /**
   * If the supplied Object is in this Set, it is removed, and true is
   * returned; otherwise, false is returned.
   *
   * @param obj the Object to remove from this Set
   * @return true if the set was modified
   * @throws ClassCastException if obj cannot be compared to set elements
   */
  public boolean remove(Object obj)
  {
    return map.remove(obj) != null;
  }

  /**
   * Returns the number of elements in this Set
   *
   * @return the set size
   */
  public int size()
  {
    return map.size();
  }

  /**
   * Returns a view of this Set including all elements greater or equal to
   * <code>from</code> and less than <code>to</code> (a half-open interval).
   * The returned set is backed by the original, so changes in one appear in
   * the other. The subset will throw an {@link IllegalArgumentException}
   * for any attempt to access or add an element beyond the specified cutoffs.
   * The returned set includes the low endpoint but not the high; if you want
   * to reverse this behavior on either end, pass in the successor element.
   *
   * @param from the (inclusive) low cutoff point
   * @param to the (exclusive) high cutoff point
   * @return a view of the set between the cutoffs
   * @throws ClassCastException if either cutoff is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if from or to is null, but the comparator
   *         does not tolerate null elements
   * @throws IllegalArgumentException if from is greater than to
   */
  public SortedSet subSet(Object from, Object to)
  {
    return new TreeSet(map.subMap(from, to));
  }

  /**
   * Returns a view of this Set including all elements greater or equal to
   * <code>from</code>. The returned set is backed by the original, so
   * changes in one appear in the other. The subset will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned set includes the
   * endpoint; if you want to exclude it, pass in the successor element.
   *
   * @param from the (inclusive) low cutoff point
   * @return a view of the set above the cutoff
   * @throws ClassCastException if <code>from</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if from is null, but the comparator
   *         does not tolerate null elements
   */
  public SortedSet tailSet(Object from)
  {
    return new TreeSet(map.tailMap(from));
  }

  /**
   * Serializes this object to the given stream.
   *
   * @param s the stream to write to
   * @throws IOException if the underlying stream fails
   * @serialData the <i>comparator</i> (Object), followed by the set size
   *             (int), the the elements in sorted order (Object)
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();
    Iterator itr = map.keySet().iterator();
    int pos = map.size();
    s.writeObject(map.comparator());
    s.writeInt(pos);
    while (--pos >= 0)
      s.writeObject(itr.next());
  }

  /**
   * Deserializes this object from the given stream.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if the underlying stream fails
   * @throws IOException if the underlying stream fails
   * @serialData the <i>comparator</i> (Object), followed by the set size
   *             (int), the the elements in sorted order (Object)
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    Comparator comparator = (Comparator) s.readObject();
    int size = s.readInt();
    map = new TreeMap(comparator);
    ((TreeMap) map).putFromObjStream(s, size, false);
  }
}
