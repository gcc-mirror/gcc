/* TreeSet.java -- a class providing a TreeMap-backed SortedSet
   Copyright (C) 1999, 2000, 2001, 2004, 2005 Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

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
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see Collection
 * @see Set
 * @see HashSet
 * @see LinkedHashSet
 * @see Comparable
 * @see Comparator
 * @see Collections#synchronizedSortedSet(SortedSet)
 * @see TreeMap
 * @since 1.2
 * @status updated to 1.6
 */
public class TreeSet<T> extends AbstractSet<T>
  implements NavigableSet<T>, Cloneable, Serializable
{
  /**
   * Compatible with JDK 1.2.
   */
  private static final long serialVersionUID = -2479143000061671589L;

  /**
   * The NavigableMap which backs this Set.
   */
  // Not final because of readObject. This will always be one of TreeMap or
  // TreeMap.SubMap, which both extend AbstractMap.
  private transient NavigableMap<T, String> map;

  /**
   * Construct a new TreeSet whose backing TreeMap using the "natural"
   * ordering of keys. Elements that are not mutually comparable will cause
   * ClassCastExceptions down the road.
   *
   * @see Comparable
   */
  public TreeSet()
  {
    map = new TreeMap<T, String>();
  }

  /**
   * Construct a new TreeSet whose backing TreeMap uses the supplied
   * Comparator. Elements that are not mutually comparable will cause
   * ClassCastExceptions down the road.
   *
   * @param comparator the Comparator this Set will use
   */
  public TreeSet(Comparator<? super T> comparator)
  {
    map = new TreeMap<T, String>(comparator);
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
  public TreeSet(Collection<? extends T> collection)
  {
    map = new TreeMap<T, String>();
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
  public TreeSet(SortedSet<T> sortedSet)
  {
    Iterator<T> itr;

    map = new TreeMap<T, String>
      ((Comparator<? super T>)sortedSet.comparator());
    itr = ((SortedSet<T>) sortedSet).iterator();
    ((TreeMap<T, String>) map).putKeysLinear(itr, sortedSet.size());
  }

  /**
   * This private constructor is used to implement the subSet() calls around
   * a backing TreeMap.SubMap.
   *
   * @param backingMap the submap
   */
  private TreeSet(NavigableMap<T,String> backingMap)
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
  public boolean add(T obj)
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
  public boolean addAll(Collection<? extends T> c)
  {
    boolean result = false;
    int pos = c.size();
    Iterator<? extends T> itr = c.iterator();
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
    TreeSet<T> copy = null;
    try
      {
        copy = (TreeSet<T>) super.clone();
        // Map may be either TreeMap or TreeMap.SubMap, hence the ugly casts.
        copy.map = (NavigableMap<T, String>) ((AbstractMap<T, String>) map).clone();
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
  public Comparator<? super T> comparator()
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
  public T first()
  {
    return map.firstKey();
  }

  /**
   * Returns a view of this Set including all elements less than
   * <code>to</code>. The returned set is backed by the original, so changes
   * in one appear in the other. The subset will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned set does not include
   * the endpoint; if you want inclusion, pass the successor element or
   * call {@link #headSet(T,boolean)}.  This call is equivalent to
   * <code>headSet(to, false)</code>.
   *
   * @param to the (exclusive) cutoff point
   * @return a view of the set less than the cutoff
   * @throws ClassCastException if <code>to</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if to is null, but the comparator does not
   *         tolerate null elements
   */
  public SortedSet<T> headSet(T to)
  {
    return headSet(to, false);
  }

  /**
   * Returns a view of this Set including all elements less than
   * (or equal to, if <code>inclusive</code> is true) <code>to</code>.
   * The returned set is backed by the original, so changes
   * in one appear in the other. The subset will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. 
   *
   * @param to the cutoff point
   * @param inclusive true if <code>to</code> should be included.
   * @return a view of the set for the specified range.
   * @throws ClassCastException if <code>to</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if to is null, but the comparator does not
   *         tolerate null elements
   */
  public NavigableSet<T> headSet(T to, boolean inclusive)
  {
    return new TreeSet<T>(map.headMap(to, inclusive));
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
  public Iterator<T> iterator()
  {
    return map.keySet().iterator();
  }

  /**
   * Returns the last (by order) element in this Set.
   *
   * @return the last element
   * @throws NoSuchElementException if the set is empty
   */
  public T last()
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
   * to reverse this behavior on either end, pass in the successor element
   * or call {@link #subSet(T,boolean,T,boolean)}.  This is equivalent to
   * calling <code>subSet(from,true,to,false)</code>.
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
  public SortedSet<T> subSet(T from, T to)
  {
    return subSet(from, true, to, false);
  }

  /**
   * Returns a view of this Set including all elements greater than (or equal to,
   * if <code>fromInclusive</code> is true</code> <code>from</code> and less than
   * (or equal to, if <code>toInclusive</code> is true) <code>to</code>.
   * The returned set is backed by the original, so changes in one appear in
   * the other. The subset will throw an {@link IllegalArgumentException}
   * for any attempt to access or add an element beyond the specified cutoffs.
   *
   * @param from the low cutoff point
   * @param fromInclusive true if <code>from</code> should be included.
   * @param to the high cutoff point
   * @param toInclusive true if <code>to</code> should be included.
   * @return a view of the set for the specified range.
   * @throws ClassCastException if either cutoff is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if from or to is null, but the comparator
   *         does not tolerate null elements
   * @throws IllegalArgumentException if from is greater than to
   */
  public NavigableSet<T> subSet(T from, boolean fromInclusive,
				T to, boolean toInclusive)
  {
    return new TreeSet<T>(map.subMap(from, fromInclusive,
				     to, toInclusive));
  }

  /**
   * Returns a view of this Set including all elements greater or equal to
   * <code>from</code>. The returned set is backed by the original, so
   * changes in one appear in the other. The subset will throw an
   * {@link IllegalArgumentException} for any attempt to access or add an
   * element beyond the specified cutoff. The returned set includes the
   * endpoint; if you want to exclude it, pass in the successor element
   * or call {@link #tailSet(T,boolean)}.  This is equivalent to calling
   * <code>tailSet(from, true)</code>.
   *
   * @param from the (inclusive) low cutoff point
   * @return a view of the set above the cutoff
   * @throws ClassCastException if <code>from</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if from is null, but the comparator
   *         does not tolerate null elements
   */
  public SortedSet<T> tailSet(T from)
  {
    return tailSet(from, true);
  }

  /**
   * Returns a view of this Set including all elements greater (or equal to,
   * if <code>inclusive</code> is true) <code>from</code>. The returned set
   * is backed by the original, so changes in one appear in the other. The
   * subset will throw an {@link IllegalArgumentException} for any attempt
   * to access or add an element beyond the specified cutoff.
   *
   * @param from the low cutoff point.
   * @param inclusive true if <code>from</code> should be included.
   * @return a view of the set for the specified range.
   * @throws ClassCastException if <code>from</code> is not compatible with
   *         the comparator (or is not Comparable, for natural ordering)
   * @throws NullPointerException if from is null, but the comparator
   *         does not tolerate null elements
   */
  public NavigableSet<T> tailSet(T from, boolean inclusive)
  {
    return new TreeSet<T>(map.tailMap(from, inclusive));
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
    Iterator<T> itr = map.keySet().iterator();
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
    Comparator<? super T> comparator = (Comparator<? super T>) s.readObject();
    int size = s.readInt();
    map = new TreeMap<T, String>(comparator);
    ((TreeMap<T, String>) map).putFromObjStream(s, size, false);
  }

  /**
   * Returns the least or lowest element in the set greater than or
   * equal to the given element, or <code>null</code> if there is
   * no such element.
   *
   * @param e the element relative to the returned element.
   * @return the least element greater than or equal
   *         to the given element, or <code>null</code> if there is
   *         no such element.
   * @throws ClassCastException if the specified element can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the element is <code>null</code>
   *                              and this set either uses natural
   *                              ordering or a comparator that does
   *                              not permit null elements.
   * @since 1.6
   */
  public T ceiling(T e)
  {
    return map.ceilingKey(e);
  }

  /**
   * Returns an iterator over the elements of this set in descending
   * order.  This is equivalent to calling
   * <code>descendingSet().iterator()</code>.
   *
   * @return an iterator over the elements in descending order.
   * @since 1.6
   */
  public Iterator<T> descendingIterator()
  {
    return descendingSet().iterator();
  }

  /**
   * Returns a view of the set in reverse order.  The descending set
   * is backed by the original set, so that changes affect both sets.
   * Any changes occurring to either set while an iteration is taking
   * place (with the exception of a {@link Iterator#remove()} operation)
   * result in undefined behaviour from the iteration.  The ordering
   * of the descending set is the same as for a set with a
   * {@link Comparator} given by {@link Collections#reverseOrder()},
   * and calling {@link #descendingSet()} on the descending set itself
   * results in a view equivalent to the original set.
   *
   * @return a reverse order view of the set.
   * @since 1.6
   */
  public NavigableSet<T> descendingSet()
  {
    return map.descendingKeySet();
  }

  /**
   * Returns the greatest or highest element in the set less than or
   * equal to the given element, or <code>null</code> if there is
   * no such element.
   *
   * @param e the element relative to the returned element.
   * @return the greatest element less than or equal
   *         to the given element, or <code>null</code> if there is
   *         no such element.
   * @throws ClassCastException if the specified element can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the element is <code>null</code>
   *                              and this set either uses natural
   *                              ordering or a comparator that does
   *                              not permit null elements.
   * @since 1.6
   */
  public T floor(T e)
  {
    return map.floorKey(e);
  }

  /**
   * Returns the least or lowest element in the set strictly greater
   * than the given element, or <code>null</code> if there is
   * no such element.
   *
   * @param e the element relative to the returned element.
   * @return the least element greater than 
   *         the given element, or <code>null</code> if there is
   *         no such element.
   * @throws ClassCastException if the specified element can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the element is <code>null</code>
   *                              and this set either uses natural
   *                              ordering or a comparator that does
   *                              not permit null elements.
   * @since 1.6
   */
  public T higher(T e)
  {
    return map.higherKey(e);
  }

  /**
   * Returns the greatest or highest element in the set strictly less
   * than the given element, or <code>null</code> if there is
   * no such element.
   *
   * @param e the element relative to the returned element.
   * @return the greatest element less than 
   *         the given element, or <code>null</code> if there is
   *         no such element.
   * @throws ClassCastException if the specified element can not
   *                            be compared with those in the map.
   * @throws NullPointerException if the element is <code>null</code>
   *                              and this set either uses natural
   *                              ordering or a comparator that does
   *                              not permit null elements.
   * @since 1.6
   */
  public T lower(T e)
  {
    return map.lowerKey(e);
  }

  /**
   * Removes and returns the least or lowest element in the set,
   * or <code>null</code> if the map is empty.
   *
   * @return the removed first element, or <code>null</code> if the
   *         map is empty.
   * @since 1.6
   */
  public T pollFirst()
  {
    return map.pollFirstEntry().getKey();
  }

  /**
   * Removes and returns the greatest or highest element in the set,
   * or <code>null</code> if the map is empty.
   *
   * @return the removed last element, or <code>null</code> if the
   *         map is empty.
   * @since 1.6
   */
  public T pollLast()
  {
    return map.pollLastEntry().getKey();
  }

}
