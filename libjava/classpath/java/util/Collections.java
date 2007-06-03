/* Collections.java -- Utility class with methods to operate on collections
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006
   Free Software Foundation, Inc.

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

import java.io.Serializable;

/**
 * Utility class consisting of static methods that operate on, or return
 * Collections. Contains methods to sort, search, reverse, fill and shuffle
 * Collections, methods to facilitate interoperability with legacy APIs that
 * are unaware of collections, a method to return a list which consists of
 * multiple copies of one element, and methods which "wrap" collections to give
 * them extra properties, such as thread-safety and unmodifiability.
 * <p>
 *
 * All methods which take a collection throw a {@link NullPointerException} if
 * that collection is null. Algorithms which can change a collection may, but
 * are not required, to throw the {@link UnsupportedOperationException} that
 * the underlying collection would throw during an attempt at modification.
 * For example,
 * <code>Collections.singleton("").addAll(Collections.EMPTY_SET)</code>
 * does not throw a exception, even though addAll is an unsupported operation
 * on a singleton; the reason for this is that addAll did not attempt to
 * modify the set.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @see Collection
 * @see Set
 * @see List
 * @see Map
 * @see Arrays
 * @since 1.2
 * @status updated to 1.5
 */
public class Collections
{
  /**
   * Constant used to decide cutoff for when a non-RandomAccess list should
   * be treated as sequential-access. Basically, quadratic behavior is
   * acceptable for small lists when the overhead is so small in the first
   * place. I arbitrarily set it to 16, so it may need some tuning.
   */
  private static final int LARGE_LIST_SIZE = 16;

  /**
   * Determines if a list should be treated as a sequential-access one.
   * Rather than the old method of JDK 1.3 of assuming only instanceof
   * AbstractSequentialList should be sequential, this uses the new method
   * of JDK 1.4 of assuming anything that does NOT implement RandomAccess
   * and exceeds a large (unspecified) size should be sequential.
   *
   * @param l the list to check
   * @return <code>true</code> if it should be treated as sequential-access
   */
  private static boolean isSequential(List<?> l)
  {
    return ! (l instanceof RandomAccess) && l.size() > LARGE_LIST_SIZE;
  }

  /**
   * This class is non-instantiable.
   */
  private Collections()
  {
  }

  /**
   * An immutable, serializable, empty Set.
   * @see Serializable
   */
  public static final Set EMPTY_SET = new EmptySet();

  /**
   * Returns an immutable, serializable parameterized empty set.
   * Unlike the constant <code>EMPTY_SET</code>, the set returned by
   * this method is type-safe.
   *
   * @return an empty parameterized set.
   * @since 1.5
   */
  public static final <T> Set<T> emptySet()
  {
    /* FIXME: Could this be optimized? */
    return new EmptySet<T>();
  }

  /**
   * The implementation of {@link #EMPTY_SET}. This class name is required
   * for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class EmptySet<T> extends AbstractSet<T>
    implements Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1582296315990362920L;

    /**
     * A private constructor adds overhead.
     */
    EmptySet()
    {
    }

    /**
     * The size: always 0!
     * @return 0.
     */
    public int size()
    {
      return 0;
    }

    /**
     * Returns an iterator that does not iterate.
     * @return A non-iterating iterator.
     */
    // This is really cheating! I think it's perfectly valid, though.
    public Iterator<T> iterator()
    {
      return (Iterator<T>) EMPTY_LIST.iterator();
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractSet.
    /**
     * The empty set never contains anything.
     * @param o The object to search for.
     * @return <code>false</code>.
     */
    public boolean contains(Object o)
    {
      return false;
    }

    /**
     * This is true only if the given collection is also empty.
     * @param c The collection of objects which are to be compared
     *          against the members of this set.
     * @return <code>true</code> if c is empty.
     */
    public boolean containsAll(Collection<?> c)
    {
      return c.isEmpty();
    }

    /**
     * Equal only if the other set is empty.
     * @param o The object to compare with this set.
     * @return <code>true</code> if o is an empty instance of <code>Set</code>.
     */
    public boolean equals(Object o)
    {
      return o instanceof Set && ((Set) o).isEmpty();
    }

    /**
     * The hashcode is always 0.
     * @return 0.
     */
    public int hashCode()
    {
      return 0;
    }

    /**
     * Always succeeds with a <code>false</code> result.
     * @param o The object to remove.
     * @return <code>false</code>.
     */
    public boolean remove(Object o)
    {
      return false;
    }

    /**
     * Always succeeds with a <code>false</code> result.
     * @param c The collection of objects which should
     *          all be removed from this set.
     * @return <code>false</code>.
     */
    public boolean removeAll(Collection<?> c)
    {
      return false;
    }

    /**
     * Always succeeds with a <code>false</code> result.
     * @param c The collection of objects which should
     *          all be retained within this set.
     * @return <code>false</code>.
     */
    public boolean retainAll(Collection<?> c)
    {
      return false;
    }

    /**
     * The array is always empty.
     * @return A new array with a size of 0.
     */
    public Object[] toArray()
    {
      return new Object[0];
    }

    /**
     * We don't even need to use reflection!
     * @param a An existing array, which can be empty.
     * @return The original array with any existing
     *         initial element set to null.
     */
    public <E> E[] toArray(E[] a)
    {
      if (a.length > 0)
        a[0] = null;
      return a;
    }

    /**
     * The string never changes.
     *
     * @return the string "[]".
     */
    public String toString()
    {
      return "[]";
    }
  } // class EmptySet

  /**
   * An immutable, serializable, empty List, which implements RandomAccess.
   * @see Serializable
   * @see RandomAccess
   */
  public static final List EMPTY_LIST = new EmptyList();

  /**
   * Returns an immutable, serializable parameterized empty list.
   * Unlike the constant <code>EMPTY_LIST</code>, the list returned by
   * this method is type-safe.
   *
   * @return an empty parameterized list.
   * @since 1.5
   */
  public static final <T> List<T> emptyList()
  {
    /* FIXME: Could this be optimized? */
    return new EmptyList<T>();
  }

  /**
   * The implementation of {@link #EMPTY_LIST}. This class name is required
   * for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class EmptyList<T> extends AbstractList<T>
    implements Serializable, RandomAccess
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 8842843931221139166L;

    /**
     * A private constructor adds overhead.
     */
    EmptyList()
    {
    }

    /**
     * The size is always 0.
     * @return 0.
     */
    public int size()
    {
      return 0;
    }

    /**
     * No matter the index, it is out of bounds.  This
     * method never returns, throwing an exception instead.
     *
     * @param index The index of the element to retrieve.
     * @return the object at the specified index.
     * @throws IndexOutOfBoundsException as any given index
     *         is outside the bounds of an empty array.
     */
    public T get(int index)
    {
      throw new IndexOutOfBoundsException();
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractList.
    /**
     * Never contains anything.
     * @param o The object to search for.
     * @return <code>false</code>.
     */
    public boolean contains(Object o)
    {
      return false;
    }

    /**
     * This is true only if the given collection is also empty.
     * @param c The collection of objects, which should be compared
     *          against the members of this list.
     * @return <code>true</code> if c is also empty. 
     */
    public boolean containsAll(Collection<?> c)
    {
      return c.isEmpty();
    }

    /**
     * Equal only if the other list is empty.
     * @param o The object to compare against this list.
     * @return <code>true</code> if o is also an empty instance of
     *         <code>List</code>.
     */
    public boolean equals(Object o)
    {
      return o instanceof List && ((List) o).isEmpty();
    }

    /**
     * The hashcode is always 1.
     * @return 1.
     */
    public int hashCode()
    {
      return 1;
    }

    /**
     * Returns -1.
     * @param o The object to search for.
     * @return -1.
     */
    public int indexOf(Object o)
    {
      return -1;
    }

    /**
     * Returns -1.
     * @param o The object to search for.
     * @return -1.
     */
    public int lastIndexOf(Object o)
    {
      return -1;
    }

    /**
     * Always succeeds with <code>false</code> result.
     * @param o The object to remove.
     * @return -1.
     */
    public boolean remove(Object o)
    {
      return false;
    }

    /**
     * Always succeeds with <code>false</code> result.
     * @param c The collection of objects which should
     *          all be removed from this list.
     * @return <code>false</code>.
     */
    public boolean removeAll(Collection<?> c)
    {
      return false;
    }

    /**
     * Always succeeds with <code>false</code> result.
     * @param c The collection of objects which should
     *          all be retained within this list.
     * @return <code>false</code>.
     */
    public boolean retainAll(Collection<?> c)
    {
      return false;
    }

    /**
     * The array is always empty.
     * @return A new array with a size of 0.
     */
    public Object[] toArray()
    {
      return new Object[0];
    }

    /**
     * We don't even need to use reflection!
     * @param a An existing array, which can be empty.
     * @return The original array with any existing
     *         initial element set to null.
     */
    public <E> E[] toArray(E[] a)
    {
      if (a.length > 0)
        a[0] = null;
      return a;
    }

    /**
     * The string never changes.
     *
     * @return the string "[]".
     */
    public String toString()
    {
      return "[]";
    }
  } // class EmptyList

  /**
   * An immutable, serializable, empty Map.
   * @see Serializable
   */
  public static final Map EMPTY_MAP = new EmptyMap();

  /**
   * Returns an immutable, serializable parameterized empty map.
   * Unlike the constant <code>EMPTY_MAP</code>, the map returned by
   * this method is type-safe.
   *
   * @return an empty parameterized map.
   * @since 1.5
   */
  public static final <K,V> Map<K,V> emptyMap()
  {
    /* FIXME: Could this be optimized? */
    return new EmptyMap<K,V>();
  }

  /**
   * The implementation of {@link #EMPTY_MAP}. This class name is required
   * for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class EmptyMap<K, V> extends AbstractMap<K, V>
    implements Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 6428348081105594320L;

    /**
     * A private constructor adds overhead.
     */
    EmptyMap()
    {
    }

    /**
     * There are no entries.
     * @return The empty set.
     */
    public Set<Map.Entry<K, V>> entrySet()
    {
      return EMPTY_SET;
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractMap.
    /**
     * No entries!
     * @param key The key to search for.
     * @return <code>false</code>.
     */
    public boolean containsKey(Object key)
    {
      return false;
    }

    /**
     * No entries!
     * @param value The value to search for.
     * @return <code>false</code>.
     */
    public boolean containsValue(Object value)
    {
      return false;
    }

    /**
     * Equal to all empty maps.
     * @param o The object o to compare against this map.
     * @return <code>true</code> if o is also an empty instance of
     *         <code>Map</code>.
     */
    public boolean equals(Object o)
    {
      return o instanceof Map && ((Map) o).isEmpty();
    }

    /**
     * No mappings, so this returns null.
     * @param o The key of the object to retrieve.
     * @return null. 
     */
    public V get(Object o)
    {
      return null;
    }

    /**
     * The hashcode is always 0.
     * @return 0.
     */
    public int hashCode()
    {
      return 0;
    }

    /**
     * No entries.
     * @return The empty set.
     */
    public Set<K> keySet()
    {
      return EMPTY_SET;
    }

    /**
     * Remove always succeeds, with null result.
     * @param o The key of the mapping to remove.
     * @return null, as there is never a mapping for o.
     */
    public V remove(Object o)
    {
      return null;
    }

    /**
     * Size is always 0.
     * @return 0.
     */
    public int size()
    {
      return 0;
    }

    /**
     * No entries. Technically, EMPTY_SET, while more specific than a general
     * Collection, will work. Besides, that's what the JDK uses!
     * @return The empty set.
     */
    public Collection<V> values()
    {
      return EMPTY_SET;
    }

    /**
     * The string never changes.
     *
     * @return the string "[]".
     */
    public String toString()
    {
      return "[]";
    }
  } // class EmptyMap


  /**
   * Compare two objects with or without a Comparator. If c is null, uses the
   * natural ordering. Slightly slower than doing it inline if the JVM isn't
   * clever, but worth it for removing a duplicate of the search code.
   * Note: This code is also used in Arrays (for sort as well as search).
   */
  static final <T> int compare(T o1, T o2, Comparator<? super T> c)
  {
    return c == null ? ((Comparable) o1).compareTo(o2) : c.compare(o1, o2);
  }

  /**
   * Perform a binary search of a List for a key, using the natural ordering of
   * the elements. The list must be sorted (as by the sort() method) - if it is
   * not, the behavior of this method is undefined, and may be an infinite
   * loop. Further, the key must be comparable with every item in the list. If
   * the list contains the key more than once, any one of them may be found.
   * <p>
   *
   * This algorithm behaves in log(n) time for {@link RandomAccess} lists,
   * and uses a linear search with O(n) link traversals and log(n) comparisons
   * with {@link AbstractSequentialList} lists. Note: although the
   * specification allows for an infinite loop if the list is unsorted, it will
   * not happen in this (Classpath) implementation.
   *
   * @param l the list to search (must be sorted)
   * @param key the value to search for
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value
   * @throws ClassCastException if key could not be compared with one of the
   *         elements of l
   * @throws NullPointerException if a null element has compareTo called
   * @see #sort(List)
   */
  public static <T> int binarySearch(List<? extends Comparable<? super T>> l, 
				     T key)
  {
    return binarySearch(l, key, null);
  }

  /**
   * Perform a binary search of a List for a key, using a supplied Comparator.
   * The list must be sorted (as by the sort() method with the same Comparator)
   * - if it is not, the behavior of this method is undefined, and may be an
   * infinite loop. Further, the key must be comparable with every item in the
   * list. If the list contains the key more than once, any one of them may be
   * found. If the comparator is null, the elements' natural ordering is used.
   * <p>
   *
   * This algorithm behaves in log(n) time for {@link RandomAccess} lists,
   * and uses a linear search with O(n) link traversals and log(n) comparisons
   * with {@link AbstractSequentialList} lists. Note: although the
   * specification allows for an infinite loop if the list is unsorted, it will
   * not happen in this (Classpath) implementation.
   *
   * @param l the list to search (must be sorted)
   * @param key the value to search for
   * @param c the comparator by which the list is sorted
   * @return the index at which the key was found, or -n-1 if it was not
   *         found, where n is the index of the first value higher than key or
   *         a.length if there is no such value
   * @throws ClassCastException if key could not be compared with one of the
   *         elements of l
   * @throws NullPointerException if a null element is compared with natural
   *         ordering (only possible when c is null)
   * @see #sort(List, Comparator)
   */
  public static <T> int binarySearch(List<? extends T> l, T key,
				     Comparator<? super T> c)
  {
    int pos = 0;
    int low = 0;
    int hi = l.size() - 1;

    // We use a linear search with log(n) comparisons using an iterator
    // if the list is sequential-access.
    if (isSequential(l))
      {
	ListIterator<T> itr = ((List<T>) l).listIterator();
        int i = 0;
	T o = itr.next(); // Assumes list is not empty (see isSequential)
	boolean forward = true;
        while (low <= hi)
          {
            pos = (low + hi) >>> 1;
            if (i < pos)
	      {
		if (!forward)
		  itr.next(); // Changing direction first.
		for ( ; i != pos; i++, o = itr.next())
                  ;
		forward = true;
	      }
            else
	      {
		if (forward)
		  itr.previous(); // Changing direction first.
		for ( ; i != pos; i--, o = itr.previous())
                  ;
		forward = false;
	      }
	    final int d = compare(o, key, c);
	    if (d == 0)
              return pos;
	    else if (d > 0)
              hi = pos - 1;
	    else
              // This gets the insertion point right on the last loop
              low = ++pos;
          }
      }
    else
      {
	while (low <= hi)
	  {
	    pos = (low + hi) >>> 1;
	    final int d = compare(((List<T>) l).get(pos), key, c);
	    if (d == 0)
              return pos;
	    else if (d > 0)
              hi = pos - 1;
	    else
              // This gets the insertion point right on the last loop
              low = ++pos;
	  }
      }

    // If we failed to find it, we do the same whichever search we did.
    return -pos - 1;
  }

  /**
   * Copy one list to another. If the destination list is longer than the
   * source list, the remaining elements are unaffected. This method runs in
   * linear time.
   *
   * @param dest the destination list
   * @param source the source list
   * @throws IndexOutOfBoundsException if the destination list is shorter
   *         than the source list (the destination will be unmodified)
   * @throws UnsupportedOperationException if dest.listIterator() does not
   *         support the set operation
   */
  public static <T> void copy(List<? super T> dest, List<? extends T> source)
  {
    int pos = source.size();
    if (dest.size() < pos)
      throw new IndexOutOfBoundsException("Source does not fit in dest");

    Iterator<? extends T> i1 = source.iterator();
    ListIterator<? super T> i2 = dest.listIterator();

    while (--pos >= 0)
      {
        i2.next();
        i2.set(i1.next());
      }
  }

  /**
   * Returns an Enumeration over a collection. This allows interoperability
   * with legacy APIs that require an Enumeration as input.
   *
   * @param c the Collection to iterate over
   * @return an Enumeration backed by an Iterator over c
   */
  public static <T> Enumeration<T> enumeration(Collection<T> c)
  {
    final Iterator<T> i = c.iterator();
    return new Enumeration<T>()
    {
      /**
       * Returns <code>true</code> if there are more elements to
       * be enumerated.
       *
       * @return The result of <code>hasNext()</code>
       *         called on the underlying iterator.
       */
      public final boolean hasMoreElements()
      {
	return i.hasNext();
      }

      /**
       * Returns the next element to be enumerated.
       *
       * @return The result of <code>next()</code>
       *         called on the underlying iterator.
       */
      public final T nextElement()
      {
	return i.next();
      }
    };
  }

  /**
   * Replace every element of a list with a given value. This method runs in
   * linear time.
   *
   * @param l the list to fill.
   * @param val the object to vill the list with.
   * @throws UnsupportedOperationException if l.listIterator() does not
   *         support the set operation.
   */
  public static <T> void fill(List<? super T> l, T val)
  {
    ListIterator<? super T> itr = l.listIterator();
    for (int i = l.size() - 1; i >= 0; --i)
      {
	itr.next();
	itr.set(val);
      }
  }

  /**
   * Returns the starting index where the specified sublist first occurs
   * in a larger list, or -1 if there is no matching position. If
   * <code>target.size() &gt; source.size()</code>, this returns -1,
   * otherwise this implementation uses brute force, checking for
   * <code>source.sublist(i, i + target.size()).equals(target)</code>
   * for all possible i.
   *
   * @param source the list to search
   * @param target the sublist to search for
   * @return the index where found, or -1
   * @since 1.4
   */
  public static int indexOfSubList(List<?> source, List<?> target)
  {
    int ssize = source.size();
    for (int i = 0, j = target.size(); j <= ssize; i++, j++)
      if (source.subList(i, j).equals(target))
        return i;
    return -1;
  }

  /**
   * Returns the starting index where the specified sublist last occurs
   * in a larger list, or -1 if there is no matching position. If
   * <code>target.size() &gt; source.size()</code>, this returns -1,
   * otherwise this implementation uses brute force, checking for
   * <code>source.sublist(i, i + target.size()).equals(target)</code>
   * for all possible i.
   *
   * @param source the list to search
   * @param target the sublist to search for
   * @return the index where found, or -1
   * @since 1.4
   */
  public static int lastIndexOfSubList(List<?> source, List<?> target)
  {
    int ssize = source.size();
    for (int i = ssize - target.size(), j = ssize; i >= 0; i--, j--)
      if (source.subList(i, j).equals(target))
        return i;
    return -1;
  }

  /**
   * Returns an ArrayList holding the elements visited by a given
   * Enumeration. This method exists for interoperability between legacy
   * APIs and the new Collection API.
   *
   * @param e the enumeration to put in a list
   * @return a list containing the enumeration elements
   * @see ArrayList
   * @since 1.4
   */
  public static <T> ArrayList<T> list(Enumeration<T> e)
  {
    ArrayList<T> l = new ArrayList<T>();
    while (e.hasMoreElements())
      l.add(e.nextElement());
    return l;
  }

  /**
   * Find the maximum element in a Collection, according to the natural
   * ordering of the elements. This implementation iterates over the
   * Collection, so it works in linear time.
   *
   * @param c the Collection to find the maximum element of
   * @return the maximum element of c
   * @exception NoSuchElementException if c is empty
   * @exception ClassCastException if elements in c are not mutually comparable
   * @exception NullPointerException if null.compareTo is called
   */
  public static <T extends Object & Comparable<? super T>>
  T max(Collection<? extends T> c)
  {
    return max(c, null);
  }

  /**
   * Find the maximum element in a Collection, according to a specified
   * Comparator. This implementation iterates over the Collection, so it
   * works in linear time.
   *
   * @param c the Collection to find the maximum element of
   * @param order the Comparator to order the elements by, or null for natural
   *        ordering
   * @return the maximum element of c
   * @throws NoSuchElementException if c is empty
   * @throws ClassCastException if elements in c are not mutually comparable
   * @throws NullPointerException if null is compared by natural ordering
   *        (only possible when order is null)
   */
  public static <T> T max(Collection<? extends T> c,
			  Comparator<? super T> order)
  {
    Iterator<? extends T> itr = c.iterator();
    T max = itr.next(); // throws NoSuchElementException
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	T o = itr.next();
	if (compare(max, o, order) < 0)
	  max = o;
      }
    return max;
  }

  /**
   * Find the minimum element in a Collection, according to the natural
   * ordering of the elements. This implementation iterates over the
   * Collection, so it works in linear time.
   *
   * @param c the Collection to find the minimum element of
   * @return the minimum element of c
   * @throws NoSuchElementException if c is empty
   * @throws ClassCastException if elements in c are not mutually comparable
   * @throws NullPointerException if null.compareTo is called
   */
  public static <T extends Object & Comparable<? super T>>
  T min(Collection<? extends T> c)
  {
    return min(c, null);
  }

  /**
   * Find the minimum element in a Collection, according to a specified
   * Comparator. This implementation iterates over the Collection, so it
   * works in linear time.
   *
   * @param c the Collection to find the minimum element of
   * @param order the Comparator to order the elements by, or null for natural
   *        ordering
   * @return the minimum element of c
   * @throws NoSuchElementException if c is empty
   * @throws ClassCastException if elements in c are not mutually comparable
   * @throws NullPointerException if null is compared by natural ordering
   *        (only possible when order is null)
   */
  public static <T> T min(Collection<? extends T> c,
			  Comparator<? super T> order)
  {
    Iterator<? extends T> itr = c.iterator();
    T min = itr.next();	// throws NoSuchElementExcception
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	T o = itr.next();
	if (compare(min, o, order) > 0)
	  min = o;
      }
    return min;
  }

  /**
   * Creates an immutable list consisting of the same object repeated n times.
   * The returned object is tiny, consisting of only a single reference to the
   * object and a count of the number of elements. It is Serializable, and
   * implements RandomAccess. You can use it in tandem with List.addAll for
   * fast list construction.
   *
   * @param n the number of times to repeat the object
   * @param o the object to repeat
   * @return a List consisting of n copies of o
   * @throws IllegalArgumentException if n &lt; 0
   * @see List#addAll(Collection)
   * @see Serializable
   * @see RandomAccess
   */
  public static <T> List<T> nCopies(final int n, final T o)
  {
    return new CopiesList<T>(n, o);
  }

  /**
   * The implementation of {@link #nCopies(int, Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class CopiesList<T> extends AbstractList<T>
    implements Serializable, RandomAccess
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 2739099268398711800L;

    /**
     * The count of elements in this list.
     * @serial the list size
     */
    private final int n;

    /**
     * The repeated list element.
     * @serial the list contents
     */
    private final T element;

    /**
     * Constructs the list.
     *
     * @param n the count
     * @param o the object
     * @throws IllegalArgumentException if n &lt; 0
     */
    CopiesList(int n, T o)
    {
      if (n < 0)
	throw new IllegalArgumentException();
      this.n = n;
      element = o;
    }

    /**
     * The size is fixed.
     * @return The size of the list.
     */
    public int size()
    {
      return n;
    }

    /**
     * The same element is returned.
     * @param index The index of the element to be returned (irrelevant
     *        as the list contains only copies of <code>element</code>).
     * @return The element used by this list.
     */
    public T get(int index)
    {
      if (index < 0 || index >= n)
        throw new IndexOutOfBoundsException();
      return element;
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractList.
    /**
     * This list only contains one element.
     * @param o The object to search for.
     * @return <code>true</code> if o is the element used by this list.
     */
    public boolean contains(Object o)
    {
      return n > 0 && equals(o, element);
    }

    /**
     * The index is either 0 or -1.
     * @param o The object to find the index of.
     * @return 0 if <code>o == element</code>, -1 if not.
     */
    public int indexOf(Object o)
    {
      return (n > 0 && equals(o, element)) ? 0 : -1;
    }

    /**
     * The index is either n-1 or -1.
     * @param o The object to find the last index of.
     * @return The last index in the list if <code>o == element</code>,
     *         -1 if not.
     */
    public int lastIndexOf(Object o)
    {
      return equals(o, element) ? n - 1 : -1;
    }

    /**
     * A subList is just another CopiesList.
     * @param from The starting bound of the sublist.
     * @param to The ending bound of the sublist.
     * @return A list of copies containing <code>from - to</code>
     *         elements, all of which are equal to the element
     *         used by this list.
     */
    public List<T> subList(int from, int to)
    {
      if (from < 0 || to > n)
        throw new IndexOutOfBoundsException();
      return new CopiesList<T>(to - from, element);
    }

    /**
     * The array is easy.
     * @return An array of size n filled with copies of
     *         the element used by this list.
     */
    public Object[] toArray()
    {
      Object[] a = new Object[n];
      Arrays.fill(a, element);
      return a;
    }

    /**
     * The string is easy to generate.
     * @return A string representation of the list.
     */
    public String toString()
    {
      StringBuffer r = new StringBuffer("{");
      for (int i = n - 1; --i > 0; )
        r.append(element).append(", ");
      r.append(element).append("}");
      return r.toString();
    }
  } // class CopiesList

  /**
   * Replace all instances of one object with another in the specified list.
   * The list does not change size. An element e is replaced if
   * <code>oldval == null ? e == null : oldval.equals(e)</code>.
   *
   * @param list the list to iterate over
   * @param oldval the element to replace
   * @param newval the new value for the element
   * @return <code>true</code> if a replacement occurred.
   * @throws UnsupportedOperationException if the list iterator does not allow
   *         for the set operation
   * @throws ClassCastException if newval is of a type which cannot be added
   *         to the list
   * @throws IllegalArgumentException if some other aspect of newval stops
   *         it being added to the list
   * @since 1.4
   */
  public static <T> boolean replaceAll(List<T> list, T oldval, T newval)
  {
    ListIterator<T> itr = list.listIterator();
    boolean replace_occured = false;
    for (int i = list.size(); --i >= 0; )
      if (AbstractCollection.equals(oldval, itr.next()))
        {
          itr.set(newval);
          replace_occured = true;
        }
    return replace_occured;
  }

  /**
   * Reverse a given list. This method works in linear time.
   *
   * @param l the list to reverse
   * @throws UnsupportedOperationException if l.listIterator() does not
   *         support the set operation
   */
  public static void reverse(List<?> l)
  {
    ListIterator i1 = l.listIterator();
    int pos1 = 1;
    int pos2 = l.size();
    ListIterator i2 = l.listIterator(pos2);
    while (pos1 < pos2)
      {
	Object o1 = i1.next();
    Object o2 = i2.previous();
	i1.set(o2);
	i2.set(o1);
	++pos1;
	--pos2;
      }
  }

  /**
   * Get a comparator that implements the reverse of the ordering
   * specified by the given Comparator. If the Comparator is null,
   * this is equivalent to {@link #reverseOrder()}.  The return value
   * of this method is Serializable, if the specified Comparator is
   * either Serializable or null.
   *
   * @param c the comparator to invert
   * @return a comparator that imposes reverse ordering
   * @see Comparable
   * @see Serializable
   *
   * @since 1.5
   */
  public static <T> Comparator<T> reverseOrder(final Comparator<T> c)
  {
    if (c == null)
      return (Comparator<T>) rcInstance;
    return new ReverseComparator<T> ()
    {
      public int compare(T a, T b)
      {
	return - c.compare(a, b);
      }
    };
  }

  /**
   * Get a comparator that implements the reverse of natural ordering. In
   * other words, this sorts Comparable objects opposite of how their
   * compareTo method would sort. This makes it easy to sort into reverse
   * order, by simply passing Collections.reverseOrder() to the sort method.
   * The return value of this method is Serializable.
   *
   * @return a comparator that imposes reverse natural ordering
   * @see Comparable
   * @see Serializable
   */
  public static <T> Comparator<T> reverseOrder()
  {
    return (Comparator<T>) rcInstance;
  }

  /**
   * The object for {@link #reverseOrder()}.
   */
  private static final ReverseComparator rcInstance = new ReverseComparator();

  /**
   * The implementation of {@link #reverseOrder()}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class ReverseComparator<T>
    implements Comparator<T>, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 7207038068494060240L;

    /**
     * A private constructor adds overhead.
     */
    ReverseComparator()
    {
    }

    /**
     * Compare two objects in reverse natural order.
     *
     * @param a the first object
     * @param b the second object
     * @return &lt;, ==, or &gt; 0 according to b.compareTo(a)
     */
    public int compare(T a, T b)
    {
      return ((Comparable) b).compareTo(a);
    }
  }

  /**
   * Rotate the elements in a list by a specified distance. After calling this
   * method, the element now at index <code>i</code> was formerly at index
   * <code>(i - distance) mod list.size()</code>. The list size is unchanged.
   * <p>
   *
   * For example, suppose a list contains <code>[t, a, n, k, s]</code>. After
   * either <code>Collections.rotate(l, 4)</code> or
   * <code>Collections.rotate(l, -1)</code>, the new contents are
   * <code>[s, t, a, n, k]</code>. This can be applied to sublists to rotate
   * just a portion of the list. For example, to move element <code>a</code>
   * forward two positions in the original example, use
   * <code>Collections.rotate(l.subList(1, 3+1), -1)</code>, which will
   * result in <code>[t, n, k, a, s]</code>.
   * <p>
   *
   * If the list is small or implements {@link RandomAccess}, the
   * implementation exchanges the first element to its destination, then the
   * displaced element, and so on until a circuit has been completed. The
   * process is repeated if needed on the second element, and so forth, until
   * all elements have been swapped.  For large non-random lists, the
   * implementation breaks the list into two sublists at index
   * <code>-distance mod size</code>, calls {@link #reverse(List)} on the
   * pieces, then reverses the overall list.
   *
   * @param list the list to rotate
   * @param distance the distance to rotate by; unrestricted in value
   * @throws UnsupportedOperationException if the list does not support set
   * @since 1.4
   */
  public static void rotate(List<?> list, int distance)
  {
    int size = list.size();
    if (size == 0)
      return;
    distance %= size;
    if (distance == 0)
      return;
    if (distance < 0)
      distance += size;

    if (isSequential(list))
      {
        reverse(list);
        reverse(list.subList(0, distance));
        reverse(list.subList(distance, size));
      }
    else
      {
        // Determine the least common multiple of distance and size, as there
        // are (distance / LCM) loops to cycle through.
        int a = size;
        int lcm = distance;
        int b = a % lcm;
        while (b != 0)
          {
            a = lcm;
            lcm = b;
            b = a % lcm;
          }

        // Now, make the swaps. We must take the remainder every time through
        // the inner loop so that we don't overflow i to negative values.
	List<Object> objList = (List<Object>) list;
        while (--lcm >= 0)
          {
            Object o = objList.get(lcm);
            for (int i = lcm + distance; i != lcm; i = (i + distance) % size)
              o = objList.set(i, o);
            objList.set(lcm, o);
          }
      }
  }

  /**
   * Shuffle a list according to a default source of randomness. The algorithm
   * used iterates backwards over the list, swapping each element with an
   * element randomly selected from the elements in positions less than or
   * equal to it (using r.nextInt(int)).
   * <p>
   *
   * This algorithm would result in a perfectly fair shuffle (that is, each
   * element would have an equal chance of ending up in any position) if r were
   * a perfect source of randomness. In practice the results are merely very
   * close to perfect.
   * <p>
   *
   * This method operates in linear time. To do this on large lists which do
   * not implement {@link RandomAccess}, a temporary array is used to acheive
   * this speed, since it would be quadratic access otherwise.
   *
   * @param l the list to shuffle
   * @throws UnsupportedOperationException if l.listIterator() does not
   *         support the set operation
   */
  public static void shuffle(List<?> l)
  {
    if (defaultRandom == null)
      {
        synchronized (Collections.class)
	  {
	    if (defaultRandom == null)
	      defaultRandom = new Random();
	  }
      }
    shuffle(l, defaultRandom);
  }

  /**
   * Cache a single Random object for use by shuffle(List). This improves
   * performance as well as ensuring that sequential calls to shuffle() will
   * not result in the same shuffle order occurring: the resolution of
   * System.currentTimeMillis() is not sufficient to guarantee a unique seed.
   */
  private static Random defaultRandom = null;

  /**
   * Shuffle a list according to a given source of randomness. The algorithm
   * used iterates backwards over the list, swapping each element with an
   * element randomly selected from the elements in positions less than or
   * equal to it (using r.nextInt(int)).
   * <p>
   *
   * This algorithm would result in a perfectly fair shuffle (that is, each
   * element would have an equal chance of ending up in any position) if r were
   * a perfect source of randomness. In practise (eg if r = new Random()) the
   * results are merely very close to perfect.
   * <p>
   *
   * This method operates in linear time. To do this on large lists which do
   * not implement {@link RandomAccess}, a temporary array is used to acheive
   * this speed, since it would be quadratic access otherwise.
   *
   * @param l the list to shuffle
   * @param r the source of randomness to use for the shuffle
   * @throws UnsupportedOperationException if l.listIterator() does not
   *         support the set operation
   */
  public static void shuffle(List<?> l, Random r)
  {
    int lsize = l.size();
    List<Object> list = (List<Object>) l;
    ListIterator<Object> i = list.listIterator(lsize);
    boolean sequential = isSequential(l);
    Object[] a = null; // stores a copy of the list for the sequential case

    if (sequential)
      a = list.toArray();

    for (int pos = lsize - 1; pos > 0; --pos)
      {
	// Obtain a random position to swap with. pos + 1 is used so that the
	// range of the random number includes the current position.
	int swap = r.nextInt(pos + 1);

	// Swap the desired element.
	Object o;
        if (sequential)
          {
            o = a[swap];
            a[swap] = i.previous();
          }
        else
          o = list.set(swap, i.previous());

	i.set(o);
      }
  }

  /**
   * Returns the frequency of the specified object within the supplied
   * collection.  The frequency represents the number of occurrences of
   * elements within the collection which return <code>true</code> when
   * compared with the object using the <code>equals</code> method.
   * 
   * @param c the collection to scan for occurrences of the object.
   * @param o the object to locate occurrances of within the collection.
   * @throws NullPointerException if the collection is <code>null</code>.
   * @since 1.5 
   */
  public static int frequency (Collection<?> c, Object o)
  {
    int result = 0;
    final Iterator<?> it = c.iterator();
    while (it.hasNext())
      {
	Object v = it.next();
	if (AbstractCollection.equals(o, v))
	  ++result;
      }
    return result;
  }

  /**
   * Adds all the specified elements to the given collection, in a similar
   * way to the <code>addAll</code> method of the <code>Collection</code>.
   * However, this is a variable argument method which allows the new elements
   * to be specified individually or in array form, as opposed to the list
   * required by the collection's <code>addAll</code> method.  This has
   * benefits in both simplicity (multiple elements can be added without
   * having to be wrapped inside a grouping structure) and efficiency
   * (as a redundant list doesn't have to be created to add an individual
   * set of elements or an array).
   *
   * @param c the collection to which the elements should be added.
   * @param a the elements to be added to the collection.
   * @return true if the collection changed its contents as a result.
   * @throws UnsupportedOperationException if the collection does not support
   *                                       addition.
   * @throws NullPointerException if one or more elements in a are null,
   *                              and the collection does not allow null
   *                              elements.  This exception is also thrown
   *                              if either <code>c</code> or <code>a</code>
   *                              are null.
   * @throws IllegalArgumentException if the collection won't allow an element
   *                                  to be added for some other reason.
   * @since 1.5
   */
  public static <T> boolean addAll(Collection<? super T> c, T... a)
  {
    boolean overall = false;

    for (T element : a)
      {
	boolean result = c.add(element);
	if (result)
	  overall = true;
      }
    return overall;
  }

  /**
   * Returns true if the two specified collections have no elements in
   * common.  This method may give unusual results if one or both collections
   * use a non-standard equality test.  In the trivial case of comparing
   * a collection with itself, this method returns true if, and only if,
   * the collection is empty.
   *
   * @param c1 the first collection to compare.
   * @param c2 the second collection to compare.
   * @return true if the collections are disjoint.
   * @throws NullPointerException if either collection is null.
   * @since 1.5
   */
  public static boolean disjoint(Collection<?> c1, Collection<?> c2)
  {
    Collection<Object> oc1 = (Collection<Object>) c1;
    final Iterator<Object> it = oc1.iterator();
    while (it.hasNext())
      if (c2.contains(it.next()))
	return false;
    return true;
  }


  /**
   * Obtain an immutable Set consisting of a single element. The return value
   * of this method is Serializable.
   *
   * @param o the single element
   * @return an immutable Set containing only o
   * @see Serializable
   */
  public static <T> Set<T> singleton(T o)
  {
    return new SingletonSet<T>(o);
  }

  /**
   * The implementation of {@link #singleton(Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SingletonSet<T> extends AbstractSet<T>
    implements Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 3193687207550431679L;


    /**
     * The single element; package visible for use in nested class.
     * @serial the singleton
     */
    final T element;

    /**
     * Construct a singleton.
     * @param o the element
     */
    SingletonSet(T o)
    {
      element = o;
    }

    /**
     * The size: always 1!
     * @return 1.
     */
    public int size()
    {
      return 1;
    }

    /**
     * Returns an iterator over the lone element.
     */
    public Iterator<T> iterator()
    {
      return new Iterator<T>()
      {
	/**
	 * Flag to indicate whether or not the element has
	 * been retrieved.
	 */
        private boolean hasNext = true;

	/**
	 * Returns <code>true</code> if elements still remain to be
	 * iterated through.
	 *
	 * @return <code>true</code> if the element has not yet been returned.
	 */
        public boolean hasNext()
        {
          return hasNext;
        }

	/**
	 * Returns the element.
	 *
	 * @return The element used by this singleton.
	 * @throws NoSuchElementException if the object
	 *         has already been retrieved.
	 */ 
        public T next()
        {
          if (hasNext)
          {
            hasNext = false;
            return element;
          }
          else
            throw new NoSuchElementException();
        }

	/**
	 * Removes the element from the singleton.
	 * As this set is immutable, this will always
	 * throw an exception.
	 *
	 * @throws UnsupportedOperationException as the
	 *         singleton set doesn't support
	 *         <code>remove()</code>.
	 */
        public void remove()
        {
          throw new UnsupportedOperationException();
        }
      };
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractSet.
    /**
     * The set only contains one element.
     *
     * @param o The object to search for.
     * @return <code>true</code> if o == the element of the singleton.
     */
    public boolean contains(Object o)
    {
      return equals(o, element);
    }

    /**
     * This is true if the other collection only contains the element.
     *
     * @param c A collection to compare against this singleton.
     * @return <code>true</code> if c only contains either no elements or
     *         elements equal to the element in this singleton.
     */
    public boolean containsAll(Collection<?> c)
    {
      Iterator<?> i = c.iterator();
      int pos = c.size();
      while (--pos >= 0)
        if (! equals(i.next(), element))
          return false;
      return true;
    }

    /**
     * The hash is just that of the element.
     * 
     * @return The hashcode of the element.
     */
    public int hashCode()
    {
      return hashCode(element);
    }

    /**
     * Returning an array is simple.
     *
     * @return An array containing the element.
     */
    public Object[] toArray()
    {
      return new Object[] {element};
    }

    /**
     * Obvious string.
     *
     * @return The string surrounded by enclosing
     *         square brackets.
     */
    public String toString()
    {
      return "[" + element + "]";
    }
  } // class SingletonSet

  /**
   * Obtain an immutable List consisting of a single element. The return value
   * of this method is Serializable, and implements RandomAccess.
   *
   * @param o the single element
   * @return an immutable List containing only o
   * @see Serializable
   * @see RandomAccess
   * @since 1.3
   */
  public static <T> List<T> singletonList(T o)
  {
    return new SingletonList<T>(o);
  }

  /**
   * The implementation of {@link #singletonList(Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SingletonList<T> extends AbstractList<T>
    implements Serializable, RandomAccess
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 3093736618740652951L;

    /**
     * The single element.
     * @serial the singleton
     */
    private final T element;

    /**
     * Construct a singleton.
     * @param o the element
     */
    SingletonList(T o)
    {
      element = o;
    }

    /**
     * The size: always 1!
     * @return 1.
     */
    public int size()
    {
      return 1;
    }

    /**
     * Only index 0 is valid.
     * @param index The index of the element
     *        to retrieve.
     * @return The singleton's element if the
     *         index is 0.
     * @throws IndexOutOfBoundsException if
     *         index is not 0.
     */
    public T get(int index)
    {
      if (index == 0)
        return element;
      throw new IndexOutOfBoundsException();
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractList.
    /**
     * The set only contains one element.
     *
     * @param o The object to search for.
     * @return <code>true</code> if o == the singleton element.
     */
    public boolean contains(Object o)
    {
      return equals(o, element);
    }

    /**
     * This is true if the other collection only contains the element.
     *
     * @param c A collection to compare against this singleton.
     * @return <code>true</code> if c only contains either no elements or
     *         elements equal to the element in this singleton.
     */
    public boolean containsAll(Collection<?> c)
    {
      Iterator<?> i = c.iterator();
      int pos = c.size();
      while (--pos >= 0)
        if (! equals(i.next(), element))
          return false;
      return true;
    }

    /**
     * Speed up the hashcode computation.
     *
     * @return The hashcode of the list, based
     *         on the hashcode of the singleton element.
     */
    public int hashCode()
    {
      return 31 + hashCode(element);
    }

    /**
     * Either the list has it or not.
     *
     * @param o The object to find the first index of.
     * @return 0 if o is the singleton element, -1 if not.
     */
    public int indexOf(Object o)
    {
      return equals(o, element) ? 0 : -1;
    }

    /**
     * Either the list has it or not.
     *
     * @param o The object to find the last index of.
     * @return 0 if o is the singleton element, -1 if not.
     */
    public int lastIndexOf(Object o)
    {
      return equals(o, element) ? 0 : -1;
    }

    /**
     * Sublists are limited in scope.
     * 
     * @param from The starting bound for the sublist.
     * @param to The ending bound for the sublist.
     * @return Either an empty list if both bounds are
     *         0 or 1, or this list if the bounds are 0 and 1.
     * @throws IllegalArgumentException if <code>from > to</code>
     * @throws IndexOutOfBoundsException if either bound is greater
     *         than 1.
     */
    public List<T> subList(int from, int to)
    {
      if (from == to && (to == 0 || to == 1))
        return EMPTY_LIST;
      if (from == 0 && to == 1)
        return this;
      if (from > to)
        throw new IllegalArgumentException();
      throw new IndexOutOfBoundsException();
    }

    /**
     * Returning an array is simple.
     *
     * @return An array containing the element.
     */
    public Object[] toArray()
    {
      return new Object[] {element};
    }

    /**
     * Obvious string.
     *
     * @return The string surrounded by enclosing
     *         square brackets. 
     */
    public String toString()
    {
      return "[" + element + "]";
    }
  } // class SingletonList

  /**
   * Obtain an immutable Map consisting of a single key-value pair.
   * The return value of this method is Serializable.
   *
   * @param key the single key
   * @param value the single value
   * @return an immutable Map containing only the single key-value pair
   * @see Serializable
   * @since 1.3
   */
  public static <K, V> Map<K, V> singletonMap(K key, V value)
  {
    return new SingletonMap<K, V>(key, value);
  }

  /**
   * The implementation of {@link #singletonMap(Object, Object)}. This class
   * name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SingletonMap<K, V> extends AbstractMap<K, V>
    implements Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -6979724477215052911L;

    /**
     * The single key.
     * @serial the singleton key
     */
    private final K k;

    /**
     * The corresponding value.
     * @serial the singleton value
     */
    private final V v;

    /**
     * Cache the entry set.
     */
    private transient Set<Map.Entry<K, V>> entries;

    /**
     * Construct a singleton.
     * @param key the key
     * @param value the value
     */
    SingletonMap(K key, V value)
    {
      k = key;
      v = value;
    }

    /**
     * There is a single immutable entry.
     *
     * @return A singleton containing the map entry.
     */
    public Set<Map.Entry<K, V>> entrySet()
    {
      if (entries == null)
	{
	  Map.Entry<K,V> entry = new AbstractMap.SimpleEntry<K, V>(k, v)
	  {
	    /**
	     * Sets the value of the map entry to the supplied value.
	     * An exception is always thrown, as the map is immutable.
	     *
	     * @param o The new value.
	     * @return The old value.
	     * @throws UnsupportedOperationException as setting the value
	     *         is not supported.
	     */
	    public V setValue(V o)
	    {
	      throw new UnsupportedOperationException();
	    }
	  };
	  entries = singleton(entry);
	}
      return entries;
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractMap.
    /**
     * Single entry.
     *
     * @param key The key to look for.
     * @return <code>true</code> if the key is the same as the one used by
     *         this map.
     */
    public boolean containsKey(Object key)
    {
      return equals(key, k);
    }

    /**
     * Single entry.
     *
     * @param value The value to look for.
     * @return <code>true</code> if the value is the same as the one used by
     *         this map.
     */
    public boolean containsValue(Object value)
    {
      return equals(value, v);
    }

    /**
     * Single entry.
     *
     * @param key The key of the value to be retrieved.
     * @return The singleton value if the key is the same as the
     *         singleton key, null otherwise.
     */
    public V get(Object key)
    {
      return equals(key, k) ? v : null;
    }

    /**
     * Calculate the hashcode directly.
     *
     * @return The hashcode computed from the singleton key
     *         and the singleton value.
     */
    public int hashCode()
    {
      return hashCode(k) ^ hashCode(v);
    }

    /**
     * Return the keyset.
     *
     * @return A singleton containing the key.
     */
    public Set<K> keySet()
    {
      if (keys == null)
        keys = singleton(k);
      return keys;
    }

    /**
     * The size: always 1!
     *
     * @return 1.
     */
    public int size()
    {
      return 1;
    }

    /**
     * Return the values. Technically, a singleton, while more specific than
     * a general Collection, will work. Besides, that's what the JDK uses!
     *
     * @return A singleton containing the value.
     */
    public Collection<V> values()
    {
      if (values == null)
        values = singleton(v);
      return values;
    }

    /**
     * Obvious string.
     *
     * @return A string containing the string representations of the key
     *         and its associated value.
     */
    public String toString()
    {
      return "{" + k + "=" + v + "}";
    }
  } // class SingletonMap

  /**
   * Sort a list according to the natural ordering of its elements. The list
   * must be modifiable, but can be of fixed size. The sort algorithm is
   * precisely that used by Arrays.sort(Object[]), which offers guaranteed
   * nlog(n) performance. This implementation dumps the list into an array,
   * sorts the array, and then iterates over the list setting each element from
   * the array.
   *
   * @param l the List to sort (<code>null</code> not permitted)
   * @throws ClassCastException if some items are not mutually comparable
   * @throws UnsupportedOperationException if the List is not modifiable
   * @throws NullPointerException if the list is <code>null</code>, or contains
   *     some element that is <code>null</code>.
   * @see Arrays#sort(Object[])
   */
  public static <T extends Comparable<? super T>> void sort(List<T> l)
  {
    sort(l, null);
  }

  /**
   * Sort a list according to a specified Comparator. The list must be
   * modifiable, but can be of fixed size. The sort algorithm is precisely that
   * used by Arrays.sort(Object[], Comparator), which offers guaranteed
   * nlog(n) performance. This implementation dumps the list into an array,
   * sorts the array, and then iterates over the list setting each element from
   * the array.
   *
   * @param l the List to sort (<code>null</code> not permitted)
   * @param c the Comparator specifying the ordering for the elements, or
   *        <code>null</code> for natural ordering
   * @throws ClassCastException if c will not compare some pair of items
   * @throws UnsupportedOperationException if the List is not modifiable
   * @throws NullPointerException if the List is <code>null</code> or 
   *         <code>null</code> is compared by natural ordering (only possible 
   *         when c is <code>null</code>)
   *         
   * @see Arrays#sort(Object[], Comparator)
   */
  public static <T> void sort(List<T> l, Comparator<? super T> c)
  {
    T[] a = (T[]) l.toArray();
    Arrays.sort(a, c);
    ListIterator<T> i = l.listIterator();
    for (int pos = 0, alen = a.length;  pos < alen;  pos++)
      {
	i.next();
	i.set(a[pos]);
      }
  }

  /**
   * Swaps the elements at the specified positions within the list. Equal
   * positions have no effect.
   *
   * @param l the list to work on
   * @param i the first index to swap
   * @param j the second index
   * @throws UnsupportedOperationException if list.set is not supported
   * @throws IndexOutOfBoundsException if either i or j is &lt; 0 or &gt;=
   *         list.size()
   * @since 1.4
   */
  public static void swap(List<?> l, int i, int j)
  {
    List<Object> list = (List<Object>) l;
    list.set(i, list.set(j, list.get(i)));
  }


  /**
   * Returns a synchronized (thread-safe) collection wrapper backed by the
   * given collection. Notice that element access through the iterators
   * is thread-safe, but if the collection can be structurally modified
   * (adding or removing elements) then you should synchronize around the
   * iteration to avoid non-deterministic behavior:<br>
   * <pre>
   * Collection c = Collections.synchronizedCollection(new Collection(...));
   * ...
   * synchronized (c)
   *   {
   *     Iterator i = c.iterator();
   *     while (i.hasNext())
   *       foo(i.next());
   *   }
   * </pre><p>
   *
   * Since the collection might be a List or a Set, and those have incompatible
   * equals and hashCode requirements, this relies on Object's implementation
   * rather than passing those calls on to the wrapped collection. The returned
   * Collection implements Serializable, but can only be serialized if
   * the collection it wraps is likewise Serializable.
   *
   * @param c the collection to wrap
   * @return a synchronized view of the collection
   * @see Serializable
   */
  public static <T> Collection<T> synchronizedCollection(Collection<T> c)
  {
    return new SynchronizedCollection<T>(c);
  }

  /**
   * The implementation of {@link #synchronizedCollection(Collection)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   * Package visible, so that collections such as the one for
   * Hashtable.values() can specify which object to synchronize on.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  static class SynchronizedCollection<T>
    implements Collection<T>, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 3053995032091335093L;

    /**
     * The wrapped collection. Package visible for use by subclasses.
     * @serial the real collection
     */
    final Collection<T> c;

    /**
     * The object to synchronize on.  When an instance is created via public
     * methods, it will be this; but other uses like SynchronizedMap.values()
     * must specify another mutex. Package visible for use by subclasses.
     * @serial the lock
     */
    final Object mutex;

    /**
     * Wrap a given collection.
     * @param c the collection to wrap
     * @throws NullPointerException if c is null
     */
    SynchronizedCollection(Collection<T> c)
    {
      this.c = c;
      mutex = this;
      if (c == null)
        throw new NullPointerException();
    }

    /**
     * Called only by trusted code to specify the mutex as well as the
     * collection.
     * @param sync the mutex
     * @param c the collection
     */
    SynchronizedCollection(Object sync, Collection<T> c)
    {
      this.c = c;
      mutex = sync;
    }

    /**
     * Adds the object to the underlying collection, first
     * obtaining a lock on the mutex.
     *
     * @param o The object to add.
     * @return <code>true</code> if the collection was modified as a result
     *         of this action.
     * @throws UnsupportedOperationException if this collection does not
     *         support the add operation.
     * @throws ClassCastException if o cannot be added to this collection due
     *         to its type.
     * @throws NullPointerException if o is null and this collection doesn't
     *         support the addition of null values.
     * @throws IllegalArgumentException if o cannot be added to this
     *         collection for some other reason.
     */
    public boolean add(T o)
    {
      synchronized (mutex)
        {
          return c.add(o);
        }
    }

    /**
     * Adds the objects in col to the underlying collection, first
     * obtaining a lock on the mutex.
     *
     * @param col The collection to take the new objects from.
     * @return <code>true</code> if the collection was modified as a result
     *          of this action.
     * @throws UnsupportedOperationException if this collection does not
     *         support the addAll operation.
     * @throws ClassCastException if some element of col cannot be added to this
     *         collection due to its type.
     * @throws NullPointerException if some element of col is null and this
     *         collection does not support the addition of null values.
     * @throws NullPointerException if col itself is null.
     * @throws IllegalArgumentException if some element of col cannot be added
     *         to this collection for some other reason.
     */
    public boolean addAll(Collection<? extends T> col)
    {
      synchronized (mutex)
        {
          return c.addAll(col);
        }
    }

    /**
     * Removes all objects from the underlying collection,
     * first obtaining a lock on the mutex.
     *
     * @throws UnsupportedOperationException if this collection does not
     *         support the clear operation.
     */
    public void clear()
    {
      synchronized (mutex)
        {
          c.clear();
        }
    }

    /**
     * Checks for the existence of o within the underlying
     * collection, first obtaining a lock on the mutex.
     *
     * @param o the element to look for.
     * @return <code>true</code> if this collection contains at least one
     *         element e such that <code>o == null ? e == null : o.equals(e)</code>.
     * @throws ClassCastException if the type of o is not a valid type for this
     *         collection.
     * @throws NullPointerException if o is null and this collection doesn't
     *         support null values.
     */
    public boolean contains(Object o)
    {
      synchronized (mutex)
        {
          return c.contains(o);
        }
    }

    /**
     * Checks for the existence of each object in cl
     * within the underlying collection, first obtaining
     * a lock on the mutex.
     *
     * @param c1 the collection to test for.
     * @return <code>true</code> if for every element o in c, contains(o)
     *         would return <code>true</code>.
     * @throws ClassCastException if the type of any element in cl is not a valid
     *         type for this collection.
     * @throws NullPointerException if some element of cl is null and this
     *         collection does not support null values.
     * @throws NullPointerException if cl itself is null.
     */
    public boolean containsAll(Collection<?> c1)
    {
      synchronized (mutex)
        {
          return c.containsAll(c1);
        }
    }

    /**
     * Returns <code>true</code> if there are no objects in the underlying
     * collection.  A lock on the mutex is obtained before the
     * check is performed.
     *
     * @return <code>true</code> if this collection contains no elements.
     */
    public boolean isEmpty()
    {
      synchronized (mutex)
        {
          return c.isEmpty();
        }
    }

    /**
     * Returns a synchronized iterator wrapper around the underlying
     * collection's iterator.  A lock on the mutex is obtained before
     * retrieving the collection's iterator.
     *
     * @return An iterator over the elements in the underlying collection,
     *         which returns each element in any order.
     */
    public Iterator<T> iterator()
    {
      synchronized (mutex)
        {
          return new SynchronizedIterator<T>(mutex, c.iterator());
        }
    }

    /**
     * Removes the specified object from the underlying collection,
     * first obtaining a lock on the mutex.
     *
     * @param o The object to remove.
     * @return <code>true</code> if the collection changed as a result of this call, that is,
     *         if the collection contained at least one occurrence of o.
     * @throws UnsupportedOperationException if this collection does not
     *         support the remove operation.
     * @throws ClassCastException if the type of o is not a valid type
     *         for this collection.
     * @throws NullPointerException if o is null and the collection doesn't
     *         support null values.
     */
    public boolean remove(Object o)
    {
      synchronized (mutex)
        {
          return c.remove(o);
        }
    }

    /**
     * Removes all elements, e, of the underlying
     * collection for which <code>col.contains(e)</code>
     * returns <code>true</code>.  A lock on the mutex is obtained
     * before the operation proceeds.
     *
     * @param col The collection of objects to be removed.
     * @return <code>true</code> if this collection was modified as a result of this call.
     * @throws UnsupportedOperationException if this collection does not
     *   support the removeAll operation.
     * @throws ClassCastException if the type of any element in c is not a valid
     *   type for this collection.
     * @throws NullPointerException if some element of c is null and this
     *   collection does not support removing null values.
     * @throws NullPointerException if c itself is null.
     */
    public boolean removeAll(Collection<?> col)
    {
      synchronized (mutex)
        {
          return c.removeAll(col);
        }
    }

    /**
     * Retains all elements, e, of the underlying
     * collection for which <code>col.contains(e)</code>
     * returns <code>true</code>.  That is, every element that doesn't
     * exist in col is removed.  A lock on the mutex is obtained
     * before the operation proceeds.
     *
     * @param col The collection of objects to be removed.
     * @return <code>true</code> if this collection was modified as a result of this call.
     * @throws UnsupportedOperationException if this collection does not
     *   support the removeAll operation.
     * @throws ClassCastException if the type of any element in c is not a valid
     *   type for this collection.
     * @throws NullPointerException if some element of c is null and this
     *   collection does not support removing null values.
     * @throws NullPointerException if c itself is null.
     */
    public boolean retainAll(Collection<?> col)
    {
      synchronized (mutex)
        {
          return c.retainAll(col);
        }
    }

    /**
     * Retrieves the size of the underlying collection.
     * A lock on the mutex is obtained before the collection
     * is accessed.
     *
     * @return The size of the collection.
     */
    public int size()
    {
      synchronized (mutex)
        {
          return c.size();
        }
    }

    /**
     * Returns an array containing each object within the underlying
     * collection.  A lock is obtained on the mutex before the collection
     * is accessed.
     *
     * @return An array of objects, matching the collection in size.  The
     *         elements occur in any order.
     */
    public Object[] toArray()
    {
      synchronized (mutex)
        {
          return c.toArray();
        }
    }

    /**
     * Copies the elements in the underlying collection to the supplied
     * array.  If <code>a.length < size()</code>, a new array of the
     * same run-time type is created, with a size equal to that of
     * the collection.  If <code>a.length > size()</code>, then the
     * elements from 0 to <code>size() - 1</code> contain the elements
     * from this collection.  The following element is set to null
     * to indicate the end of the collection objects.  However, this
     * only makes a difference if null is not a permitted value within
     * the collection.
     * Before the copying takes place, a lock is obtained on the mutex.
     *
     * @param a An array to copy elements to.
     * @return An array containing the elements of the underlying collection.
     * @throws ArrayStoreException if the type of any element of the
     *         collection is not a subtype of the element type of a.
     */
    public <T> T[] toArray(T[] a)
    {
      synchronized (mutex)
        {
          return c.toArray(a);
        }
    }

    /**
     * Returns a string representation of the underlying collection.
     * A lock is obtained on the mutex before the string is created.
     *
     * @return A string representation of the collection.
     */
    public String toString()
    {
      synchronized (mutex)
        {
          return c.toString();
        }
    }
  } // class SynchronizedCollection

  /**
   * The implementation of the various iterator methods in the
   * synchronized classes. These iterators must "sync" on the same object
   * as the collection they iterate over.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class SynchronizedIterator<T> implements Iterator<T>
  {
    /**
     * The object to synchronize on. Package visible for use by subclass.
     */
    final Object mutex;

    /**
     * The wrapped iterator.
     */
    private final Iterator<T> i;

    /**
     * Only trusted code creates a wrapper, with the specified sync.
     * @param sync the mutex
     * @param i the wrapped iterator
     */
    SynchronizedIterator(Object sync, Iterator<T> i)
    {
      this.i = i;
      mutex = sync;
    }

    /**
     * Retrieves the next object in the underlying collection.
     * A lock is obtained on the mutex before the collection is accessed.
     * 
     * @return The next object in the collection.
     * @throws NoSuchElementException if there are no more elements
     */
    public T next()
    {
      synchronized (mutex)
        {
          return i.next();
        }
    }

    /**
     * Returns <code>true</code> if objects can still be retrieved from the iterator
     * using <code>next()</code>.  A lock is obtained on the mutex before
     * the collection is accessed.
     *
     * @return <code>true</code> if at least one element is still to be returned by
     *         <code>next()</code>.
     */
    public boolean hasNext()
    {
      synchronized (mutex)
        {
          return i.hasNext();
        }
    }

    /**
     * Removes the object that was last returned by <code>next()</code>
     * from the underlying collection.  Only one call to this method is
     * allowed per call to the <code>next()</code> method, and it does
     * not affect the value that will be returned by <code>next()</code>.
     * Thus, if element n was retrieved from the collection by
     * <code>next()</code>, it is this element that gets removed.
     * Regardless of whether this takes place or not, element n+1 is
     * still returned on the subsequent <code>next()</code> call.
     *
     * @throws IllegalStateException if next has not yet been called or remove
     *         has already been called since the last call to next.
     * @throws UnsupportedOperationException if this Iterator does not support
     *         the remove operation.
     */
    public void remove()
    {
      synchronized (mutex)
        {
          i.remove();
        }
    }
  } // class SynchronizedIterator

  /**
   * Returns a synchronized (thread-safe) list wrapper backed by the
   * given list. Notice that element access through the iterators
   * is thread-safe, but if the list can be structurally modified
   * (adding or removing elements) then you should synchronize around the
   * iteration to avoid non-deterministic behavior:<br>
   * <pre>
   * List l = Collections.synchronizedList(new List(...));
   * ...
   * synchronized (l)
   *   {
   *     Iterator i = l.iterator();
   *     while (i.hasNext())
   *       foo(i.next());
   *   }
   * </pre><p>
   *
   * The returned List implements Serializable, but can only be serialized if
   * the list it wraps is likewise Serializable. In addition, if the wrapped
   * list implements RandomAccess, this does too.
   *
   * @param l the list to wrap
   * @return a synchronized view of the list
   * @see Serializable
   * @see RandomAccess
   */
  public static <T> List<T> synchronizedList(List<T> l)
  {
    if (l instanceof RandomAccess)
      return new SynchronizedRandomAccessList<T>(l);
    return new SynchronizedList<T>(l);
  }

  /**
   * The implementation of {@link #synchronizedList(List)} for sequential
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability. Package visible, so that lists such as Vector.subList()
   * can specify which object to synchronize on.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  static class SynchronizedList<T> extends SynchronizedCollection<T>
    implements List<T>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -7754090372962971524L;

    /**
     * The wrapped list; stored both here and in the superclass to avoid
     * excessive casting. Package visible for use by subclass.
     * @serial the wrapped list
     */
    final List<T> list;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @throws NullPointerException if l is null
     */
    SynchronizedList(List<T> l)
    {
      super(l);
      list = l;
    }

    /**
     * Called only by trusted code to specify the mutex as well as the list.
     * @param sync the mutex
     * @param l the list
     */
    SynchronizedList(Object sync, List<T> l)
    {
      super(sync, l);
      list = l;
    }

  /**
   * Insert an element into the underlying list at a given position (optional
   * operation).  This shifts all existing elements from that position to the
   * end one index to the right. This version of add has no return, since it is
   * assumed to always succeed if there is no exception.  Before the
   * addition takes place, a lock is obtained on the mutex.
   *
   * @param index the location to insert the item
   * @param o the object to insert
   * @throws UnsupportedOperationException if this list does not support the
   *         add operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and this list doesn't support
   *         the addition of null values.
   */
    public void add(int index, T o)
    {
      synchronized (mutex)
        {
          list.add(index, o);
        }
    }

  /**
   * Add the contents of a collection to the underlying list at the given
   * index (optional operation).  If the list imposes restraints on what 
   * can be inserted, such as no null elements, this should be documented.
   * A lock is obtained on the mutex before any of the elements are added.
   *
   * @param index the index at which to insert
   * @param c the collection to add
   * @return <code>true</code>, as defined by Collection for a modified list
   * @throws UnsupportedOperationException if this list does not support the
   *         add operation
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and this list doesn't support
   *         the addition of null values.
   */
    public boolean addAll(int index, Collection<? extends T> c)
    {
      synchronized (mutex)
        {
          return list.addAll(index, c);
        }
    }

   /**
    * Tests whether the underlying list is equal to the supplied object.
    * The object is deemed to be equal if it is also a <code>List</code>
    * of equal size and with the same elements (i.e. each element, e1,
    * in list, l1, and each element, e2, in l2, must return <code>true</code> for
    * <code>e1 == null ? e2 == null : e1.equals(e2)</code>.  Before the
    * comparison is made, a lock is obtained on the mutex.
    *
    * @param o The object to test for equality with the underlying list.
    * @return <code>true</code> if o is equal to the underlying list under the above
    *         definition.
    */
    public boolean equals(Object o)
    {
      synchronized (mutex)
        {
          return list.equals(o);
        }
    }

    /**
     * Retrieves the object at the specified index.  A lock
     * is obtained on the mutex before the list is accessed.
     *
     * @param index the index of the element to be returned
     * @return the element at index index in this list
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     */
    public T get(int index)
    {
      synchronized (mutex)
        {
          return list.get(index);
        }
    }

    /**
     * Obtains a hashcode for the underlying list, first obtaining
     * a lock on the mutex.  The calculation of the hashcode is
     * detailed in the documentation for the <code>List</code>
     * interface.
     *
     * @return The hashcode of the underlying list.
     * @see List#hashCode()
     */
    public int hashCode()
    {
      synchronized (mutex)
        {
          return list.hashCode();
        }
    }

    /**
     * Obtain the first index at which a given object is to be found in the
     * underlying list.  A lock is obtained on the mutex before the list is
     * accessed.
     *
     * @param o the object to search for
     * @return the least integer n such that <code>o == null ? get(n) == null :
     *         o.equals(get(n))</code>, or -1 if there is no such index.
     * @throws ClassCastException if the type of o is not a valid
     *         type for this list.
     * @throws NullPointerException if o is null and this
     *         list does not support null values.
     */

    public int indexOf(Object o)
    {
      synchronized (mutex)
        {
          return list.indexOf(o);
        }
    }

    /**
     * Obtain the last index at which a given object is to be found in this
     * underlying list.  A lock is obtained on the mutex before the list
     * is accessed.
     *
     * @return the greatest integer n such that <code>o == null ? get(n) == null
     *         : o.equals(get(n))</code>, or -1 if there is no such index.
     * @throws ClassCastException if the type of o is not a valid
     *         type for this list.
     * @throws NullPointerException if o is null and this
     *         list does not support null values.
     */
    public int lastIndexOf(Object o)
    {
      synchronized (mutex)
        {
          return list.lastIndexOf(o);
        }
    }

    /**
     * Retrieves a synchronized wrapper around the underlying list's
     * list iterator.  A lock is obtained on the mutex before the
     * list iterator is retrieved.
     *
     * @return A list iterator over the elements in the underlying list.
     *         The list iterator allows additional list-specific operations
     *         to be performed, in addition to those supplied by the
     *         standard iterator.
     */
    public ListIterator<T> listIterator()
    {
      synchronized (mutex)
        {
          return new SynchronizedListIterator<T>(mutex, list.listIterator());
        }
    }

    /**
     * Retrieves a synchronized wrapper around the underlying list's
     * list iterator.  A lock is obtained on the mutex before the
     * list iterator is retrieved.  The iterator starts at the
     * index supplied, leading to the element at that index being
     * the first one returned by <code>next()</code>.  Calling
     * <code>previous()</code> from this initial position returns
     * index - 1.
     *
     * @param index the position, between 0 and size() inclusive, to begin the
     *        iteration from
     * @return A list iterator over the elements in the underlying list.
     *         The list iterator allows additional list-specific operations
     *         to be performed, in addition to those supplied by the
     *         standard iterator.
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
     */
    public ListIterator<T> listIterator(int index)
    {
      synchronized (mutex)
        {
          return new SynchronizedListIterator<T>(mutex,
						 list.listIterator(index));
        }
    }

    /**
     * Remove the element at a given position in the underlying list (optional
     * operation).  All remaining elements are shifted to the left to fill the gap.
     * A lock on the mutex is obtained before the element is removed.
     *
     * @param index the position within the list of the object to remove
     * @return the object that was removed
     * @throws UnsupportedOperationException if this list does not support the
     *         remove operation
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     */
    public T remove(int index)
    {
      synchronized (mutex)
        {
          return list.remove(index);
        }
    }

  /**
   * Replace an element of the underlying list with another object (optional
   * operation).  A lock is obtained on the mutex before the element is
   * replaced.
   *
   * @param index the position within this list of the element to be replaced
   * @param o the object to replace it with
   * @return the object that was replaced
   * @throws UnsupportedOperationException if this list does not support the
   *         set operation.
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and this
   *         list does not support null values.
   */
    public T set(int index, T o)
    {
      synchronized (mutex)
        {
          return list.set(index, o);
        }
    }

    /**
     * Obtain a List view of a subsection of the underlying list, from fromIndex
     * (inclusive) to toIndex (exclusive). If the two indices are equal, the
     * sublist is empty. The returned list should be modifiable if and only
     * if this list is modifiable. Changes to the returned list should be
     * reflected in this list. If this list is structurally modified in
     * any way other than through the returned list, the result of any subsequent
     * operations on the returned list is undefined.  A lock is obtained
     * on the mutex before the creation of the sublist.  The returned list
     * is also synchronized, using the same mutex.
     *
     * @param fromIndex the index that the returned list should start from
     *        (inclusive)
     * @param toIndex the index that the returned list should go to (exclusive)
     * @return a List backed by a subsection of this list
     * @throws IndexOutOfBoundsException if fromIndex &lt; 0
     *         || toIndex &gt; size() || fromIndex &gt; toIndex
     */
    public List<T> subList(int fromIndex, int toIndex)
    {
      synchronized (mutex)
        {
          return new SynchronizedList<T>(mutex,
					 list.subList(fromIndex, toIndex));
        }
    }
  } // class SynchronizedList

  /**
   * The implementation of {@link #synchronizedList(List)} for random-access
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SynchronizedRandomAccessList<T>
    extends SynchronizedList<T> implements RandomAccess
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1530674583602358482L;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @throws NullPointerException if l is null
     */
    SynchronizedRandomAccessList(List<T> l)
    {
      super(l);
    }

    /**
     * Called only by trusted code to specify the mutex as well as the
     * collection.
     * @param sync the mutex
     * @param l the list
     */
    SynchronizedRandomAccessList(Object sync, List<T> l)
    {
      super(sync, l);
    }

    /**
     * Obtain a List view of a subsection of the underlying list, from fromIndex
     * (inclusive) to toIndex (exclusive). If the two indices are equal, the
     * sublist is empty. The returned list should be modifiable if and only
     * if this list is modifiable. Changes to the returned list should be
     * reflected in this list. If this list is structurally modified in
     * any way other than through the returned list, the result of any subsequent
     * operations on the returned list is undefined.    A lock is obtained
     * on the mutex before the creation of the sublist.  The returned list
     * is also synchronized, using the same mutex.  Random accessibility
     * is also extended to the new list.
     *
     * @param fromIndex the index that the returned list should start from
     *        (inclusive)
     * @param toIndex the index that the returned list should go to (exclusive)
     * @return a List backed by a subsection of this list
     * @throws IndexOutOfBoundsException if fromIndex &lt; 0
     *         || toIndex &gt; size() || fromIndex &gt; toIndex
     */
    public List<T> subList(int fromIndex, int toIndex)
    {
      synchronized (mutex)
        {
          return new SynchronizedRandomAccessList<T>(mutex,
						     list.subList(fromIndex,
								  toIndex));
        }
    }
  } // class SynchronizedRandomAccessList

  /**
   * The implementation of {@link SynchronizedList#listIterator()}. This
   * iterator must "sync" on the same object as the list it iterates over.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SynchronizedListIterator<T>
    extends SynchronizedIterator<T> implements ListIterator<T>
  {
    /**
     * The wrapped iterator, stored both here and in the superclass to
     * avoid excessive casting.
     */
    private final ListIterator<T> li;

    /**
     * Only trusted code creates a wrapper, with the specified sync.
     * @param sync the mutex
     * @param li the wrapped iterator
     */
    SynchronizedListIterator(Object sync, ListIterator<T> li)
    {
      super(sync, li);
      this.li = li;
    }

    /**
     * Insert an element into the underlying list at the current position of
     * the iterator (optional operation). The element is inserted in between
     * the element that would be returned by <code>previous()</code> and the
     * element that would be returned by <code>next()</code>. After the
     * insertion, a subsequent call to next is unaffected, but
     * a call to previous returns the item that was added. The values returned
     * by nextIndex() and previousIndex() are incremented.  A lock is obtained
     * on the mutex before the addition takes place.
     *
     * @param o the object to insert into the list
     * @throws ClassCastException if the object is of a type which cannot be added
     *         to this list.
     * @throws IllegalArgumentException if some other aspect of the object stops
     *         it being added to this list.
     * @throws UnsupportedOperationException if this ListIterator does not
     *         support the add operation.
     */
    public void add(T o)
    {
      synchronized (mutex)
        {
          li.add(o);
        }
    }

    /**
     * Tests whether there are elements remaining in the underlying list
     * in the reverse direction. In other words, <code>previous()</code>
     * will not fail with a NoSuchElementException.  A lock is obtained
     * on the mutex before the check takes place.
     *
     * @return <code>true</code> if the list continues in the reverse direction
     */
    public boolean hasPrevious()
    {
      synchronized (mutex)
        {
          return li.hasPrevious();
        }
    }

    /**
      * Find the index of the element that would be returned by a call to
      * <code>next()</code>.  If hasNext() returns <code>false</code>, this
      * returns the list size.  A lock is obtained on the mutex before the
      * query takes place.
      *
      * @return the index of the element that would be returned by next()
      */
    public int nextIndex()
    {
      synchronized (mutex)
        {
          return li.nextIndex();
        }
    }

    /**
     * Obtain the previous element from the underlying list. Repeated
     * calls to previous may be used to iterate backwards over the entire list,
     * or calls to next and previous may be used together to go forwards and
     * backwards. Alternating calls to next and previous will return the same
     * element.  A lock is obtained on the mutex before the object is retrieved.
     *
     * @return the next element in the list in the reverse direction
     * @throws NoSuchElementException if there are no more elements
     */
    public T previous()
    {
      synchronized (mutex)
        {
          return li.previous();
        }
    }

    /**
     * Find the index of the element that would be returned by a call to
     * previous. If hasPrevious() returns <code>false</code>, this returns -1.
     * A lock is obtained on the mutex before the query takes place.
     *
     * @return the index of the element that would be returned by previous()
     */
    public int previousIndex()
    {
      synchronized (mutex)
        {
          return li.previousIndex();
        }
    }

    /**
     * Replace the element last returned by a call to <code>next()</code> or
     * <code>previous()</code> with a given object (optional operation).  This
     * method may only be called if neither <code>add()</code> nor
     * <code>remove()</code> have been called since the last call to
     * <code>next()</code> or <code>previous</code>.  A lock is obtained
     * on the mutex before the list is modified.
     *
     * @param o the object to replace the element with
     * @throws ClassCastException the object is of a type which cannot be added
     *         to this list
     * @throws IllegalArgumentException some other aspect of the object stops
     *         it being added to this list
     * @throws IllegalStateException if neither next or previous have been
     *         called, or if add or remove has been called since the last call
     *         to next or previous
     * @throws UnsupportedOperationException if this ListIterator does not
     *         support the set operation
     */
    public void set(T o)
    {
      synchronized (mutex)
        {
          li.set(o);
        }
    }
  } // class SynchronizedListIterator

  /**
   * Returns a synchronized (thread-safe) map wrapper backed by the given
   * map. Notice that element access through the collection views and their
   * iterators are thread-safe, but if the map can be structurally modified
   * (adding or removing elements) then you should synchronize around the
   * iteration to avoid non-deterministic behavior:<br>
   * <pre>
   * Map m = Collections.synchronizedMap(new Map(...));
   * ...
   * Set s = m.keySet(); // safe outside a synchronized block
   * synchronized (m) // synch on m, not s
   *   {
   *     Iterator i = s.iterator();
   *     while (i.hasNext())
   *       foo(i.next());
   *   }
   * </pre><p>
   *
   * The returned Map implements Serializable, but can only be serialized if
   * the map it wraps is likewise Serializable.
   *
   * @param m the map to wrap
   * @return a synchronized view of the map
   * @see Serializable
   */
  public static <K, V> Map<K, V> synchronizedMap(Map<K, V> m)
  {
    return new SynchronizedMap<K, V>(m);
  }

  /**
   * The implementation of {@link #synchronizedMap(Map)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class SynchronizedMap<K, V> implements Map<K, V>, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1978198479659022715L;

    /**
     * The wrapped map.
     * @serial the real map
     */
    private final Map<K, V> m;

    /**
     * The object to synchronize on.  When an instance is created via public
     * methods, it will be this; but other uses like
     * SynchronizedSortedMap.subMap() must specify another mutex. Package
     * visible for use by subclass.
     * @serial the lock
     */
    final Object mutex;

    /**
     * Cache the entry set.
     */
    private transient Set<Map.Entry<K, V>> entries;

    /**
     * Cache the key set.
     */
    private transient Set<K> keys;

    /**
     * Cache the value collection.
     */
    private transient Collection<V> values;

    /**
     * Wrap a given map.
     * @param m the map to wrap
     * @throws NullPointerException if m is null
     */
    SynchronizedMap(Map<K, V> m)
    {
      this.m = m;
      mutex = this;
      if (m == null)
        throw new NullPointerException();
    }

    /**
     * Called only by trusted code to specify the mutex as well as the map.
     * @param sync the mutex
     * @param m the map
     */
    SynchronizedMap(Object sync, Map<K, V> m)
    {
      this.m = m;
      mutex = sync;
    }

    /**
     * Clears all the entries from the underlying map.  A lock is obtained
     * on the mutex before the map is cleared.
     *
     * @throws UnsupportedOperationException if clear is not supported
     */
    public void clear()
    {
      synchronized (mutex)
        {
          m.clear();
        }
    }

    /**
     * Returns <code>true</code> if the underlying map contains a entry for the given key.
     * A lock is obtained on the mutex before the map is queried.
     *
     * @param key the key to search for.
     * @return <code>true</code> if the underlying map contains the key.
     * @throws ClassCastException if the key is of an inappropriate type.
     * @throws NullPointerException if key is <code>null</code> but the map
     *         does not permit null keys.
     */
    public boolean containsKey(Object key)
    {
      synchronized (mutex)
        {
          return m.containsKey(key);
        }
    }

  /**
   * Returns <code>true</code> if the underlying map contains at least one entry with the
   * given value.  In other words, returns <code>true</code> if a value v exists where
   * <code>(value == null ? v == null : value.equals(v))</code>. This usually
   * requires linear time.  A lock is obtained on the mutex before the map
   * is queried.
   *
   * @param value the value to search for
   * @return <code>true</code> if the map contains the value
   * @throws ClassCastException if the type of the value is not a valid type
   *         for this map.
   * @throws NullPointerException if the value is null and the map doesn't
   *         support null values.
   */
    public boolean containsValue(Object value)
    {
      synchronized (mutex)
        {
          return m.containsValue(value);
        }
    }

    // This is one of the ickiest cases of nesting I've ever seen. It just
    // means "return a SynchronizedSet, except that the iterator() method
    // returns an SynchronizedIterator whose next() method returns a
    // synchronized wrapper around its normal return value".
    public Set<Map.Entry<K, V>> entrySet()
    {
      // Define this here to spare some nesting.
      class SynchronizedMapEntry<K, V> implements Map.Entry<K, V>
      {
        final Map.Entry<K, V> e;
        SynchronizedMapEntry(Map.Entry<K, V> o)
        {
          e = o;
        }

	/**
	 * Returns <code>true</code> if the object, o, implements <code>Map.Entry</code>
	 * with the same key and value as the underlying entry.  A lock is
	 * obtained on the mutex before the comparison takes place.
	 *
	 * @param o The object to compare with this entry.
	 * @return <code>true</code> if o is equivalent to the underlying map entry.
	 */
        public boolean equals(Object o)
        {
          synchronized (mutex)
            {
              return e.equals(o);
            }
        }

	/**
	 * Returns the key used in the underlying map entry.  A lock is obtained
	 * on the mutex before the key is retrieved.
	 *
	 * @return The key of the underlying map entry.
	 */
        public K getKey()
        {
          synchronized (mutex)
            {
              return e.getKey();
            }
        }

	/**
	 * Returns the value used in the underlying map entry.  A lock is obtained
	 * on the mutex before the value is retrieved.
	 *
	 * @return The value of the underlying map entry.
	 */
        public V getValue()
        {
          synchronized (mutex)
            {
              return e.getValue();
            }
        }

	/**
	 * Computes the hash code for the underlying map entry.
	 * This computation is described in the documentation for the
	 * <code>Map</code> interface.  A lock is obtained on the mutex
	 * before the underlying map is accessed.
	 *
	 * @return The hash code of the underlying map entry.
	 * @see Map#hashCode()
	 */
        public int hashCode()
        {
          synchronized (mutex)
            {
              return e.hashCode();
            }
        }

	/**
	 * Replaces the value in the underlying map entry with the specified
	 * object (optional operation).  A lock is obtained on the mutex
	 * before the map is altered.  The map entry, in turn, will alter
	 * the underlying map object.  The operation is undefined if the
	 * <code>remove()</code> method of the iterator has been called
	 * beforehand.
	 *
	 * @param value the new value to store
	 * @return the old value
	 * @throws UnsupportedOperationException if the operation is not supported.
	 * @throws ClassCastException if the value is of the wrong type.
	 * @throws IllegalArgumentException if something about the value
	 *         prevents it from existing in this map.
	 * @throws NullPointerException if the map forbids null values.
	 */
        public V setValue(V value)
        {
          synchronized (mutex)
            {
              return e.setValue(value);
            }
        }

	/**
	 * Returns a textual representation of the underlying map entry.
	 * A lock is obtained on the mutex before the entry is accessed.
	 *
	 * @return The contents of the map entry in <code>String</code> form.
	 */
        public String toString()
        {
          synchronized (mutex)
            {
              return e.toString();
            }
        }
      } // class SynchronizedMapEntry

      // Now the actual code.
      if (entries == null)
        synchronized (mutex)
          {
            entries = new SynchronizedSet<Map.Entry<K, V>>(mutex, m.entrySet())
            {
	      /**
	       * Returns an iterator over the set.  The iterator has no specific order,
	       * unless further specified.  A lock is obtained on the set's mutex
	       * before the iterator is created.  The created iterator is also
	       * thread-safe.
	       *
	       * @return A synchronized set iterator.
	       */
              public Iterator<Map.Entry<K, V>> iterator()
              {
                synchronized (super.mutex)
                  {
                    return new SynchronizedIterator<Map.Entry<K, V>>(super.mutex,
								     c.iterator())
                    {
		      /**
		       * Retrieves the next map entry from the iterator.
		       * A lock is obtained on the iterator's mutex before
		       * the entry is created.  The new map entry is enclosed in
		       * a thread-safe wrapper.
		       *
		       * @return A synchronized map entry.
		       */
                      public Map.Entry<K, V> next()
                      {
                        synchronized (super.mutex)
                          {
                            return new SynchronizedMapEntry<K, V>(super.next());
                          }
                      }
                    };
                  }
              }
            };
          }
      return entries;
    }

    /**
     * Returns <code>true</code> if the object, o, is also an instance
     * of <code>Map</code> and contains an equivalent
     * entry set to that of the underlying map.  A lock
     * is obtained on the mutex before the objects are
     * compared.
     *
     * @param o The object to compare.
     * @return <code>true</code> if o and the underlying map are equivalent.
     */
    public boolean equals(Object o)
    {
      synchronized (mutex)
        {
          return m.equals(o);
        }
    }

    /**
     * Returns the value associated with the given key, or null
     * if no such mapping exists.  An ambiguity exists with maps
     * that accept null values as a return value of null could
     * be due to a non-existent mapping or simply a null value
     * for that key.  To resolve this, <code>containsKey</code>
     * should be used.  A lock is obtained on the mutex before
     * the value is retrieved from the underlying map.
     *
     * @param key The key of the required mapping.
     * @return The value associated with the given key, or
     *         null if no such mapping exists.
     * @throws ClassCastException if the key is an inappropriate type.
     * @throws NullPointerException if this map does not accept null keys.
     */
    public V get(Object key)
    {
      synchronized (mutex)
        {
          return m.get(key);
        }
    }

    /**
     * Calculates the hash code of the underlying map as the
     * sum of the hash codes of all entries.  A lock is obtained
     * on the mutex before the hash code is computed.
     *
     * @return The hash code of the underlying map.
     */
    public int hashCode()
    {
      synchronized (mutex)
        {
          return m.hashCode();
        }
    }

    /**
     * Returns <code>true</code> if the underlying map contains no entries.
     * A lock is obtained on the mutex before the map is examined.
     *
     * @return <code>true</code> if the map is empty.
     */
    public boolean isEmpty()
    {
      synchronized (mutex)
        {
          return m.isEmpty();
        }
    }

    /**
     * Returns a thread-safe set view of the keys in the underlying map.  The
     * set is backed by the map, so that changes in one show up in the other.
     * Modifications made while an iterator is in progress cause undefined
     * behavior.  If the set supports removal, these methods remove the
     * underlying mapping from the map: <code>Iterator.remove</code>,
     * <code>Set.remove</code>, <code>removeAll</code>, <code>retainAll</code>,
     * and <code>clear</code>.  Element addition, via <code>add</code> or
     * <code>addAll</code>, is not supported via this set.  A lock is obtained
     * on the mutex before the set is created.
     *
     * @return A synchronized set containing the keys of the underlying map.
     */
    public Set<K> keySet()
    {
      if (keys == null)
        synchronized (mutex)
          {
            keys = new SynchronizedSet<K>(mutex, m.keySet());
          }
      return keys;
    }

    /**
     * Associates the given key to the given value (optional operation). If the
     * underlying map already contains the key, its value is replaced. Be aware
     * that in a map that permits <code>null</code> values, a null return does not
     * always imply that the mapping was created.  A lock is obtained on the mutex
     * before the modification is made.
     *
     * @param key the key to map.
     * @param value the value to be mapped.
     * @return the previous value of the key, or null if there was no mapping
     * @throws UnsupportedOperationException if the operation is not supported
     * @throws ClassCastException if the key or value is of the wrong type
     * @throws IllegalArgumentException if something about this key or value
     *         prevents it from existing in this map
     * @throws NullPointerException if either the key or the value is null,
     *         and the map forbids null keys or values
     * @see #containsKey(Object)
     */
    public V put(K key, V value)
    {
      synchronized (mutex)
        {
          return m.put(key, value);
        }
    }

    /**
     * Copies all entries of the given map to the underlying one (optional
     * operation). If the map already contains a key, its value is replaced.
     * A lock is obtained on the mutex before the operation proceeds.
     *
     * @param map the mapping to load into this map
     * @throws UnsupportedOperationException if the operation is not supported
     * @throws ClassCastException if a key or value is of the wrong type
     * @throws IllegalArgumentException if something about a key or value
     *         prevents it from existing in this map
     * @throws NullPointerException if the map forbids null keys or values, or
     *         if <code>m</code> is null.
     * @see #put(Object, Object)
     */
    public void putAll(Map<? extends K, ? extends V> map)
    {
      synchronized (mutex)
        {
          m.putAll(map);
        }
    }

    /**
     * Removes the mapping for the key, o, if present (optional operation). If
     * the key is not present, this returns null. Note that maps which permit
     * null values may also return null if the key was removed.  A prior
     * <code>containsKey()</code> check is required to avoid this ambiguity.
     * Before the mapping is removed, a lock is obtained on the mutex.
     *
     * @param o the key to remove
     * @return the value the key mapped to, or null if not present
     * @throws UnsupportedOperationException if deletion is unsupported
     * @throws NullPointerException if the key is null and this map doesn't
     *         support null keys.
     * @throws ClassCastException if the type of the key is not a valid type
     *         for this map.
     */
    public V remove(Object o)
    {
      synchronized (mutex)
        {
          return m.remove(o);
        }
    }

    /**
     * Retrieves the size of the underlying map.  A lock
     * is obtained on the mutex before access takes place.
     * Maps with a size greater than <code>Integer.MAX_VALUE</code>
     * return <code>Integer.MAX_VALUE</code> instead.
     *
     * @return The size of the underlying map.
     */
    public int size()
    {
      synchronized (mutex)
        {
          return m.size();
        }
    }

    /**
     * Returns a textual representation of the underlying
     * map.  A lock is obtained on the mutex before the map
     * is accessed.
     *
     * @return The map in <code>String</code> form.
     */
    public String toString()
    {
      synchronized (mutex)
        {
          return m.toString();
        }
    }

    /**
     * Returns a synchronized collection view of the values in the underlying
     * map.  The collection is backed by the map, so that changes in one show up in
     * the other.  Modifications made while an iterator is in progress cause
     * undefined behavior.  If the collection supports removal, these methods
     * remove the underlying mapping from the map: <code>Iterator.remove</code>,
     * <code>Collection.remove</code>, <code>removeAll</code>,
     * <code>retainAll</code>, and <code>clear</code>. Element addition, via
     * <code>add</code> or <code>addAll</code>, is not supported via this
     * collection.  A lock is obtained on the mutex before the collection
     * is created.
     * 
     * @return the collection of all values in the underlying map.
     */
    public Collection<V> values()
    {
      if (values == null)
        synchronized (mutex)
          {
            values = new SynchronizedCollection<V>(mutex, m.values());
          }
      return values;
    }
  } // class SynchronizedMap

  /**
   * Returns a synchronized (thread-safe) set wrapper backed by the given
   * set. Notice that element access through the iterator is thread-safe, but
   * if the set can be structurally modified (adding or removing elements)
   * then you should synchronize around the iteration to avoid
   * non-deterministic behavior:<br>
   * <pre>
   * Set s = Collections.synchronizedSet(new Set(...));
   * ...
   * synchronized (s)
   *   {
   *     Iterator i = s.iterator();
   *     while (i.hasNext())
   *       foo(i.next());
   *   }
   * </pre><p>
   *
   * The returned Set implements Serializable, but can only be serialized if
   * the set it wraps is likewise Serializable.
   *
   * @param s the set to wrap
   * @return a synchronized view of the set
   * @see Serializable
   */
  public static <T> Set<T> synchronizedSet(Set<T> s)
  {
    return new SynchronizedSet<T>(s);
  }

  /**
   * The implementation of {@link #synchronizedSet(Set)}. This class
   * name is required for compatibility with Sun's JDK serializability.
   * Package visible, so that sets such as Hashtable.keySet()
   * can specify which object to synchronize on.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  static class SynchronizedSet<T> extends SynchronizedCollection<T>
    implements Set<T>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 487447009682186044L;

    /**
     * Wrap a given set.
     * @param s the set to wrap
     * @throws NullPointerException if s is null
     */
    SynchronizedSet(Set<T> s)
    {
      super(s);
    }

    /**
     * Called only by trusted code to specify the mutex as well as the set.
     * @param sync the mutex
     * @param s the set
     */
    SynchronizedSet(Object sync, Set<T> s)
    {
      super(sync, s);
    }

    /**
     * Returns <code>true</code> if the object, o, is a <code>Set</code>
     * of the same size as the underlying set, and contains
     * each element, e, which occurs in the underlying set.
     * A lock is obtained on the mutex before the comparison
     * takes place.
     *
     * @param o The object to compare against.
     * @return <code>true</code> if o is an equivalent set.
     */
    public boolean equals(Object o)
    {
      synchronized (mutex)
        {
          return c.equals(o);
        }
    }

    /**
     * Computes the hash code for the underlying set as the
     * sum of the hash code of all elements within the set.
     * A lock is obtained on the mutex before the computation
     * occurs.
     *
     * @return The hash code for the underlying set.
     */
    public int hashCode()
    {
      synchronized (mutex)
        {
          return c.hashCode();
        }
    }
  } // class SynchronizedSet

  /**
   * Returns a synchronized (thread-safe) sorted map wrapper backed by the
   * given map. Notice that element access through the collection views,
   * subviews, and their iterators are thread-safe, but if the map can be
   * structurally modified (adding or removing elements) then you should
   * synchronize around the iteration to avoid non-deterministic behavior:<br>
   * <pre>
   * SortedMap m = Collections.synchronizedSortedMap(new SortedMap(...));
   * ...
   * Set s = m.keySet(); // safe outside a synchronized block
   * SortedMap m2 = m.headMap(foo); // safe outside a synchronized block
   * Set s2 = m2.keySet(); // safe outside a synchronized block
   * synchronized (m) // synch on m, not m2, s or s2
   *   {
   *     Iterator i = s.iterator();
   *     while (i.hasNext())
   *       foo(i.next());
   *     i = s2.iterator();
   *     while (i.hasNext())
   *       bar(i.next());
   *   }
   * </pre><p>
   *
   * The returned SortedMap implements Serializable, but can only be
   * serialized if the map it wraps is likewise Serializable.
   *
   * @param m the sorted map to wrap
   * @return a synchronized view of the sorted map
   * @see Serializable
   */
  public static <K, V> SortedMap<K, V> synchronizedSortedMap(SortedMap<K, V> m)
  {
    return new SynchronizedSortedMap<K, V>(m);
  }

  /**
   * The implementation of {@link #synchronizedSortedMap(SortedMap)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SynchronizedSortedMap<K, V>
    extends SynchronizedMap<K, V>
    implements SortedMap<K, V>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -8798146769416483793L;

    /**
     * The wrapped map; stored both here and in the superclass to avoid
     * excessive casting.
     * @serial the wrapped map
     */
    private final SortedMap<K, V> sm;

    /**
     * Wrap a given map.
     * @param sm the map to wrap
     * @throws NullPointerException if sm is null
     */
    SynchronizedSortedMap(SortedMap<K, V> sm)
    {
      super(sm);
      this.sm = sm;
    }

    /**
     * Called only by trusted code to specify the mutex as well as the map.
     * @param sync the mutex
     * @param sm the map
     */
    SynchronizedSortedMap(Object sync, SortedMap<K, V> sm)
    {
      super(sync, sm);
      this.sm = sm;
    }

    /**
     * Returns the comparator used in sorting the underlying map, or null if
     * it is the keys' natural ordering.  A lock is obtained on the mutex
     * before the comparator is retrieved.
     *
     * @return the sorting comparator.
     */
    public Comparator<? super K> comparator()
    {
      synchronized (mutex)
        {
          return sm.comparator();
        }
    }

    /**
     * Returns the first, lowest sorted, key from the underlying map.
     * A lock is obtained on the mutex before the map is accessed.
     *
     * @return the first key.
     * @throws NoSuchElementException if this map is empty.
     */
    public K firstKey()
    {
      synchronized (mutex)
        {
          return sm.firstKey();
        }
    }

    /**
     * Returns a submap containing the keys from the first
     * key (as returned by <code>firstKey()</code>) to
     * the key before that specified.  The submap supports all
     * operations supported by the underlying map and all actions
     * taking place on the submap are also reflected in the underlying
     * map.  A lock is obtained on the mutex prior to submap creation.
     * This operation is equivalent to <code>subMap(firstKey(), toKey)</code>.
     * The submap retains the thread-safe status of this map.
     *
     * @param toKey the exclusive upper range of the submap.
     * @return a submap from <code>firstKey()</code> to the
     *         the key preceding toKey.
     * @throws ClassCastException if toKey is not comparable to the underlying
     *         map's contents.
     * @throws IllegalArgumentException if toKey is outside the map's range.
     * @throws NullPointerException if toKey is null. but the map does not allow
     *         null keys.
     */
    public SortedMap<K, V> headMap(K toKey)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedMap<K, V>(mutex, sm.headMap(toKey));
        }
    }

    /**
     * Returns the last, highest sorted, key from the underlying map.
     * A lock is obtained on the mutex before the map is accessed.
     *
     * @return the last key.
     * @throws NoSuchElementException if this map is empty.
     */
    public K lastKey()
    {
      synchronized (mutex)
        {
          return sm.lastKey();
        }
    }

    /**
     * Returns a submap containing the keys from fromKey to
     * the key before toKey.  The submap supports all
     * operations supported by the underlying map and all actions
     * taking place on the submap are also reflected in the underlying
     * map.  A lock is obtained on the mutex prior to submap creation.
     * The submap retains the thread-safe status of this map.
     *
     * @param fromKey the inclusive lower range of the submap.
     * @param toKey the exclusive upper range of the submap.
     * @return a submap from fromKey to the key preceding toKey.
     * @throws ClassCastException if fromKey or toKey is not comparable
     *         to the underlying map's contents.
     * @throws IllegalArgumentException if fromKey or toKey is outside the map's
     *         range.
     * @throws NullPointerException if fromKey or toKey is null. but the map does
     *         not allow  null keys.
     */
    public SortedMap<K, V> subMap(K fromKey, K toKey)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedMap<K, V>(mutex,
						 sm.subMap(fromKey, toKey));
        }
    }

    /**
     * Returns a submap containing all the keys from fromKey onwards.
     * The submap supports all operations supported by the underlying
     * map and all actions taking place on the submap are also reflected
     * in the underlying map.  A lock is obtained on the mutex prior to
     * submap creation.  The submap retains the thread-safe status of
     * this map.
     *
     * @param fromKey the inclusive lower range of the submap.
     * @return a submap from fromKey to <code>lastKey()</code>.
     * @throws ClassCastException if fromKey is not comparable to the underlying
     *         map's contents.
     * @throws IllegalArgumentException if fromKey is outside the map's range.
     * @throws NullPointerException if fromKey is null. but the map does not allow
     *         null keys.
     */
    public SortedMap<K, V> tailMap(K fromKey)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedMap<K, V>(mutex, sm.tailMap(fromKey));
        }
    }
  } // class SynchronizedSortedMap

  /**
   * Returns a synchronized (thread-safe) sorted set wrapper backed by the
   * given set. Notice that element access through the iterator and through
   * subviews are thread-safe, but if the set can be structurally modified
   * (adding or removing elements) then you should synchronize around the
   * iteration to avoid non-deterministic behavior:<br>
   * <pre>
   * SortedSet s = Collections.synchronizedSortedSet(new SortedSet(...));
   * ...
   * SortedSet s2 = s.headSet(foo); // safe outside a synchronized block
   * synchronized (s) // synch on s, not s2
   *   {
   *     Iterator i = s2.iterator();
   *     while (i.hasNext())
   *       foo(i.next());
   *   }
   * </pre><p>
   *
   * The returned SortedSet implements Serializable, but can only be
   * serialized if the set it wraps is likewise Serializable.
   *
   * @param s the sorted set to wrap
   * @return a synchronized view of the sorted set
   * @see Serializable
   */
  public static <T> SortedSet<T> synchronizedSortedSet(SortedSet<T> s)
  {
    return new SynchronizedSortedSet<T>(s);
  }

  /**
   * The implementation of {@link #synchronizedSortedSet(SortedSet)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class SynchronizedSortedSet<T>
    extends SynchronizedSet<T>
    implements SortedSet<T>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 8695801310862127406L;

    /**
     * The wrapped set; stored both here and in the superclass to avoid
     * excessive casting.
     * @serial the wrapped set
     */
    private final SortedSet<T> ss;

    /**
     * Wrap a given set.
     * @param ss the set to wrap
     * @throws NullPointerException if ss is null
     */
    SynchronizedSortedSet(SortedSet<T> ss)
    {
      super(ss);
      this.ss = ss;
    }

    /**
     * Called only by trusted code to specify the mutex as well as the set.
     * @param sync the mutex
     * @param ss the set
     */
    SynchronizedSortedSet(Object sync, SortedSet<T> ss)
    {
      super(sync, ss);
      this.ss = ss;
    }

    /**
     * Returns the comparator used in sorting the underlying set, or null if
     * it is the elements' natural ordering.  A lock is obtained on the mutex
     * before the comparator is retrieved.
     *
     * @return the sorting comparator.
     */
    public Comparator<? super T> comparator()
    {
      synchronized (mutex)
        {
          return ss.comparator();
        }
    }

    /**
     * Returns the first, lowest sorted, element from the underlying set.
     * A lock is obtained on the mutex before the set is accessed.
     *
     * @return the first element.
     * @throws NoSuchElementException if this set is empty.
     */
    public T first()
    {
      synchronized (mutex)
        {
          return ss.first();
        }
    }

    /**
     * Returns a subset containing the element from the first
     * element (as returned by <code>first()</code>) to
     * the element before that specified.  The subset supports all
     * operations supported by the underlying set and all actions
     * taking place on the subset are also reflected in the underlying
     * set.  A lock is obtained on the mutex prior to subset creation.
     * This operation is equivalent to <code>subSet(first(), toElement)</code>.
     * The subset retains the thread-safe status of this set.
     *
     * @param toElement the exclusive upper range of the subset.
     * @return a subset from <code>first()</code> to the
     *         the element preceding toElement.
     * @throws ClassCastException if toElement is not comparable to the underlying
     *         set's contents.
     * @throws IllegalArgumentException if toElement is outside the set's range.
     * @throws NullPointerException if toElement is null. but the set does not allow
     *         null elements.
     */
    public SortedSet<T> headSet(T toElement)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedSet<T>(mutex, ss.headSet(toElement));
        }
    }

    /**
     * Returns the last, highest sorted, element from the underlying set.
     * A lock is obtained on the mutex before the set is accessed.
     *
     * @return the last element.
     * @throws NoSuchElementException if this set is empty.
     */
    public T last()
    {
      synchronized (mutex)
        {
          return ss.last();
        }
    }

    /**
     * Returns a subset containing the elements from fromElement to
     * the element before toElement.  The subset supports all
     * operations supported by the underlying set and all actions
     * taking place on the subset are also reflected in the underlying
     * set.  A lock is obtained on the mutex prior to subset creation.
     * The subset retains the thread-safe status of this set.
     *
     * @param fromElement the inclusive lower range of the subset.
     * @param toElement the exclusive upper range of the subset.
     * @return a subset from fromElement to the element preceding toElement.
     * @throws ClassCastException if fromElement or toElement is not comparable
     *         to the underlying set's contents.
     * @throws IllegalArgumentException if fromElement or toElement is outside the set's
     *         range.
     * @throws NullPointerException if fromElement or toElement is null. but the set does
     *         not allow null elements.
     */
    public SortedSet<T> subSet(T fromElement, T toElement)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedSet<T>(mutex,
					      ss.subSet(fromElement,
							toElement));
        }
    }

    /**
     * Returns a subset containing all the elements from fromElement onwards.
     * The subset supports all operations supported by the underlying
     * set and all actions taking place on the subset are also reflected
     * in the underlying set.  A lock is obtained on the mutex prior to
     * subset creation.  The subset retains the thread-safe status of
     * this set.
     *
     * @param fromElement the inclusive lower range of the subset.
     * @return a subset from fromElement to <code>last()</code>.
     * @throws ClassCastException if fromElement is not comparable to the underlying
     *         set's contents.
     * @throws IllegalArgumentException if fromElement is outside the set's range.
     * @throws NullPointerException if fromElement is null. but the set does not allow
     *         null elements.
     */
    public SortedSet<T> tailSet(T fromElement)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedSet<T>(mutex, ss.tailSet(fromElement));
        }
    }
  } // class SynchronizedSortedSet


  /**
   * Returns an unmodifiable view of the given collection. This allows
   * "read-only" access, although changes in the backing collection show up
   * in this view. Attempts to modify the collection directly or via iterators
   * will fail with {@link UnsupportedOperationException}.  Although this view
   * prevents changes to the structure of the collection and its elements, the values
   * referenced by the objects in the collection can still be modified.
   * <p>
   *
   * Since the collection might be a List or a Set, and those have incompatible
   * equals and hashCode requirements, this relies on Object's implementation
   * rather than passing those calls on to the wrapped collection. The returned
   * Collection implements Serializable, but can only be serialized if
   * the collection it wraps is likewise Serializable.
   *
   * @param c the collection to wrap
   * @return a read-only view of the collection
   * @see Serializable
   */
  public static <T> Collection<T> unmodifiableCollection(Collection<? extends T> c)
  {
    return new UnmodifiableCollection<T>(c);
  }

  /**
   * The implementation of {@link #unmodifiableCollection(Collection)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableCollection<T>
    implements Collection<T>, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1820017752578914078L;

    /**
     * The wrapped collection. Package visible for use by subclasses.
     * @serial the real collection
     */
    final Collection<? extends T> c;

    /**
     * Wrap a given collection.
     * @param c the collection to wrap
     * @throws NullPointerException if c is null
     */
    UnmodifiableCollection(Collection<? extends T> c)
    {
      this.c = c;
      if (c == null)
        throw new NullPointerException();
    }

    /**
     * Blocks the addition of elements to the underlying collection.
     * This method never returns, throwing an exception instead.
     *
     * @param o the object to add.
     * @return <code>true</code> if the collection was modified as a result of this action.
     * @throws UnsupportedOperationException as an unmodifiable collection does not
     *         support the add operation.
     */
    public boolean add(T o)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the addition of a collection of elements to the underlying
     * collection.  This method never returns, throwing an exception instead.
     *
     * @param c the collection to add.
     * @return <code>true</code> if the collection was modified as a result of this action.
     * @throws UnsupportedOperationException as an unmodifiable collection does not
     *         support the <code>addAll</code> operation.
     */
    public boolean addAll(Collection<? extends T> c)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the clearing of the underlying collection.  This method never
     * returns, throwing an exception instead.
     *
     * @throws UnsupportedOperationException as an unmodifiable collection does
     *         not support the <code>clear()</code> operation.
     */
    public void clear()
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Test whether the underlying collection contains a given object as one of its
     * elements.
     *
     * @param o the element to look for.
     * @return <code>true</code> if the underlying collection contains at least
     *         one element e such that
     *         <code>o == null ? e == null : o.equals(e)</code>.
     * @throws ClassCastException if the type of o is not a valid type for the
     *         underlying collection.
     * @throws NullPointerException if o is null and the underlying collection
     *         doesn't support null values.
     */
    public boolean contains(Object o)
    {
      return c.contains(o);
    }

    /**
     * Test whether the underlying collection contains every element in a given
     * collection.
     *
     * @param c1 the collection to test for.
     * @return <code>true</code> if for every element o in c, contains(o) would
     *         return <code>true</code>.
     * @throws ClassCastException if the type of any element in c is not a valid
     *   type for the underlying collection.
     * @throws NullPointerException if some element of c is null and the underlying
     *   collection does not support null values.
     * @throws NullPointerException if c itself is null.
     */
    public boolean containsAll(Collection<?> c1)
    {
      return c.containsAll(c1);
    }

    /**
     * Tests whether the underlying collection is empty, that is,
     * if size() == 0.
     *
     * @return <code>true</code> if this collection contains no elements.
     */
    public boolean isEmpty()
    {
      return c.isEmpty();
    }

    /**
     * Obtain an Iterator over the underlying collection, which maintains
     * its unmodifiable nature.
     *
     * @return an UnmodifiableIterator over the elements of the underlying
     *         collection, in any order.
     */
    public Iterator<T> iterator()
    {
      return new UnmodifiableIterator<T>(c.iterator());
    }

    /**
     * Blocks the removal of an object from the underlying collection.
     * This method never returns, throwing an exception instead.
     *
     * @param o The object to remove.
     * @return <code>true</code> if the object was removed (i.e. the underlying
     *         collection returned 1 or more instances of o).
     * @throws UnsupportedOperationException as an unmodifiable collection
     *         does not support the <code>remove()</code> operation.
     */
    public boolean remove(Object o)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the removal of a collection of objects from the underlying
     * collection.  This method never returns, throwing an exception
     * instead.
     *
     * @param c The collection of objects to remove.
     * @return <code>true</code> if the collection was modified.
     * @throws UnsupportedOperationException as an unmodifiable collection
     *         does not support the <code>removeAll()</code> operation.
     */
    public boolean removeAll(Collection<?> c)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the removal of all elements from the underlying collection,
     * except those in the supplied collection.  This method never returns,
     * throwing an exception instead.
     *
     * @param c The collection of objects to retain.
     * @return <code>true</code> if the collection was modified.
     * @throws UnsupportedOperationException as an unmodifiable collection
     *         does not support the <code>retainAll()</code> operation.
     */
    public boolean retainAll(Collection<?> c)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Retrieves the number of elements in the underlying collection.
     *
     * @return the number of elements in the collection.
     */
    public int size()
    {
      return c.size();
    }

    /**
     * Copy the current contents of the underlying collection into an array.
     *
     * @return an array of type Object[] with a length equal to the size of the
     *         underlying collection and containing the elements currently in
     *         the underlying collection, in any order.
     */
    public Object[] toArray()
    {
      return c.toArray();
    }

    /**
     * Copy the current contents of the underlying collection into an array.  If
     * the array passed as an argument has length less than the size of the
     * underlying collection, an array of the same run-time type as a, with a length
     * equal to the size of the underlying collection, is allocated using reflection.
     * Otherwise, a itself is used.  The elements of the underlying collection are
     * copied into it, and if there is space in the array, the following element is
     * set to null. The resultant array is returned.
     * Note: The fact that the following element is set to null is only useful
     * if it is known that this collection does not contain any null elements.
     *
     * @param a the array to copy this collection into.
     * @return an array containing the elements currently in the underlying
     *         collection, in any order.
     * @throws ArrayStoreException if the type of any element of the
     *         collection is not a subtype of the element type of a.
     */
    public <S> S[] toArray(S[] a)
    {
      return c.toArray(a);
    }

    /**
     * A textual representation of the unmodifiable collection.
     *
     * @return The unmodifiable collection in the form of a <code>String</code>.
     */
    public String toString()
    {
      return c.toString();
    }
  } // class UnmodifiableCollection

  /**
   * The implementation of the various iterator methods in the
   * unmodifiable classes.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableIterator<T> implements Iterator<T>
  {
    /**
     * The wrapped iterator.
     */
    private final Iterator<? extends T> i;

    /**
     * Only trusted code creates a wrapper.
     * @param i the wrapped iterator
     */
    UnmodifiableIterator(Iterator<? extends T> i)
    {
      this.i = i;
    }

    /**
     * Obtains the next element in the underlying collection.
     *
     * @return the next element in the collection.
     * @throws NoSuchElementException if there are no more elements.
     */
    public T next()
    {
      return i.next();
    }

    /**
     * Tests whether there are still elements to be retrieved from the
     * underlying collection by <code>next()</code>.  When this method
     * returns <code>true</code>, an exception will not be thrown on calling
     * <code>next()</code>.
     *
     * @return <code>true</code> if there is at least one more element in the underlying
     *         collection.
     */
    public boolean hasNext()
    {
      return i.hasNext();
    }

    /**
     * Blocks the removal of elements from the underlying collection by the
     * iterator.
     *
     * @throws UnsupportedOperationException as an unmodifiable collection
     *         does not support the removal of elements by its iterator.
     */
    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  } // class UnmodifiableIterator

  /**
   * Returns an unmodifiable view of the given list. This allows
   * "read-only" access, although changes in the backing list show up
   * in this view. Attempts to modify the list directly, via iterators, or
   * via sublists, will fail with {@link UnsupportedOperationException}.
   * Although this view prevents changes to the structure of the list and
   * its elements, the values referenced by the objects in the list can
   * still be modified.   
   * <p>
   *
   * The returned List implements Serializable, but can only be serialized if
   * the list it wraps is likewise Serializable. In addition, if the wrapped
   * list implements RandomAccess, this does too.
   *
   * @param l the list to wrap
   * @return a read-only view of the list
   * @see Serializable
   * @see RandomAccess
   */
  public static <T> List<T> unmodifiableList(List<? extends T> l)
  {
    if (l instanceof RandomAccess)
      return new UnmodifiableRandomAccessList<T>(l);
    return new UnmodifiableList<T>(l);
  }

  /**
   * The implementation of {@link #unmodifiableList(List)} for sequential
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableList<T> extends UnmodifiableCollection<T>
    implements List<T>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -283967356065247728L;


    /**
     * The wrapped list; stored both here and in the superclass to avoid
     * excessive casting. Package visible for use by subclass.
     * @serial the wrapped list
     */
    final List<T> list;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @throws NullPointerException if l is null
     */
    UnmodifiableList(List<? extends T> l)
    {
      super(l);
      list = (List<T>) l;
    }

    /**
     * Blocks the addition of an element to the underlying
     * list at a specific index.  This method never returns,
     * throwing an exception instead.
     *
     * @param index The index at which to place the new element.
     * @param o the object to add.
     * @throws UnsupportedOperationException as an unmodifiable
     *         list doesn't support the <code>add()</code> operation.
     */
    public void add(int index, T o)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the addition of a collection of elements to the
     * underlying list at a specific index.  This method never
     * returns, throwing an exception instead.
     *
     * @param index The index at which to place the new element.
     * @param c the collections of objects to add.
     * @throws UnsupportedOperationException as an unmodifiable
     *         list doesn't support the <code>addAll()</code> operation.
     */
    public boolean addAll(int index, Collection<? extends T> c)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Returns <code>true</code> if the object, o, is an instance of
     * <code>List</code> with the same size and elements
     * as the underlying list.
     *
     * @param o The object to compare.
     * @return <code>true</code> if o is equivalent to the underlying list.
     */
    public boolean equals(Object o)
    {
      return list.equals(o);
    }

    /**
     * Retrieves the element at a given index in the underlying list.
     *
     * @param index the index of the element to be returned
     * @return the element at index index in this list
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     */
    public T get(int index)
    {
      return list.get(index);
    }

    /**
     * Computes the hash code for the underlying list.
     * The exact computation is described in the documentation
     * of the <code>List</code> interface.
     *
     * @return The hash code of the underlying list.
     * @see List#hashCode()
     */
    public int hashCode()
    {
      return list.hashCode();
    }

    /**
     * Obtain the first index at which a given object is to be found in the
     * underlying list.
     *
     * @param o the object to search for
     * @return the least integer n such that <code>o == null ? get(n) == null :
     *         o.equals(get(n))</code>, or -1 if there is no such index.
     * @throws ClassCastException if the type of o is not a valid
     *         type for the underlying list.
     * @throws NullPointerException if o is null and the underlying
     *         list does not support null values.
     */
    public int indexOf(Object o)
    {
      return list.indexOf(o);
    }

    /**
     * Obtain the last index at which a given object is to be found in the
     * underlying list.
     *
     * @return the greatest integer n such that <code>o == null ? get(n) == null
     *         : o.equals(get(n))</code>, or -1 if there is no such index.
     * @throws ClassCastException if the type of o is not a valid
     *         type for the underlying list.
     * @throws NullPointerException if o is null and the underlying
     *         list does not support null values.
     */
    public int lastIndexOf(Object o)
    {
      return list.lastIndexOf(o);
    }

  /**
   * Obtains a list iterator over the underlying list, starting at the beginning
   * and maintaining the unmodifiable nature of this list.
   *
   * @return a <code>UnmodifiableListIterator</code> over the elements of the
   *         underlying list, in order, starting at the beginning.
   */
    public ListIterator<T> listIterator()
    {
      return new UnmodifiableListIterator<T>(list.listIterator());
    }

  /**
   * Obtains a list iterator over the underlying list, starting at the specified
   * index and maintaining the unmodifiable nature of this list.  An initial call
   * to <code>next()</code> will retrieve the element at the specified index,
   * and an initial call to <code>previous()</code> will retrieve the element
   * at index - 1.
   *
   *
   * @param index the position, between 0 and size() inclusive, to begin the
   *        iteration from.
   * @return a <code>UnmodifiableListIterator</code> over the elements of the
   *         underlying list, in order, starting at the specified index.
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
    public ListIterator<T> listIterator(int index)
    {
      return new UnmodifiableListIterator<T>(list.listIterator(index));
    }

    /**
     * Blocks the removal of the element at the specified index.
     * This method never returns, throwing an exception instead.
     *
     * @param index The index of the element to remove.
     * @return the removed element.
     * @throws UnsupportedOperationException as an unmodifiable
     *         list does not support the <code>remove()</code>
     *         operation.
     */
    public T remove(int index)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the replacement of the element at the specified index.
     * This method never returns, throwing an exception instead.
     *
     * @param index The index of the element to replace.
     * @param o The new object to place at the specified index.
     * @return the replaced element.
     * @throws UnsupportedOperationException as an unmodifiable
     *         list does not support the <code>set()</code>
     *         operation.
     */
    public T set(int index, T o)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Obtain a List view of a subsection of the underlying list, from
     * fromIndex (inclusive) to toIndex (exclusive). If the two indices
     * are equal, the sublist is empty. The returned list will be
     * unmodifiable, like this list.  Changes to the elements of the
     * returned list will be reflected in the underlying list. No structural
     * modifications can take place in either list.
     *
     * @param fromIndex the index that the returned list should start from
     *        (inclusive).
     * @param toIndex the index that the returned list should go to (exclusive).
     * @return a List backed by a subsection of the underlying list.
     * @throws IndexOutOfBoundsException if fromIndex &lt; 0
     *         || toIndex &gt; size() || fromIndex &gt; toIndex.
     */
    public List<T> subList(int fromIndex, int toIndex)
    {
      return unmodifiableList(list.subList(fromIndex, toIndex));
    }
  } // class UnmodifiableList

  /**
   * The implementation of {@link #unmodifiableList(List)} for random-access
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class UnmodifiableRandomAccessList<T>
    extends UnmodifiableList<T> implements RandomAccess
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -2542308836966382001L;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @throws NullPointerException if l is null
     */
    UnmodifiableRandomAccessList(List<? extends T> l)
    {
      super(l);
    }
  } // class UnmodifiableRandomAccessList

  /**
   * The implementation of {@link UnmodifiableList#listIterator()}.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class UnmodifiableListIterator<T>
    extends UnmodifiableIterator<T> implements ListIterator<T>
  {
    /**
     * The wrapped iterator, stored both here and in the superclass to
     * avoid excessive casting.
     */
    private final ListIterator<T> li;

    /**
     * Only trusted code creates a wrapper.
     * @param li the wrapped iterator
     */
    UnmodifiableListIterator(ListIterator<T> li)
    {
      super(li);
      this.li = li;
    }

    /**
     * Blocks the addition of an object to the list underlying this iterator.
     * This method never returns, throwing an exception instead.
     *
     * @param o The object to add.
     * @throws UnsupportedOperationException as the iterator of an unmodifiable
     *         list does not support the <code>add()</code> operation.
     */
    public void add(T o)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Tests whether there are still elements to be retrieved from the
     * underlying collection by <code>previous()</code>.  When this method
     * returns <code>true</code>, an exception will not be thrown on calling
     * <code>previous()</code>.
     *
     * @return <code>true</code> if there is at least one more element prior to the
     *         current position in the underlying list.
     */
    public boolean hasPrevious()
    {
      return li.hasPrevious();
    }

    /**
     * Find the index of the element that would be returned by a call to next.
     * If <code>hasNext()</code> returns <code>false</code>, this returns the list size.
     *
     * @return the index of the element that would be returned by
     *         <code>next()</code>.
     */
    public int nextIndex()
    {
      return li.nextIndex();
    }

    /**
     * Obtains the previous element in the underlying list.
     *
     * @return the previous element in the list.
     * @throws NoSuchElementException if there are no more prior elements.
     */
    public T previous()
    {
      return li.previous();
    }

    /**
     * Find the index of the element that would be returned by a call to
     * previous. If <code>hasPrevious()</code> returns <code>false</code>,
     * this returns -1.
     *
     * @return the index of the element that would be returned by
     *         <code>previous()</code>.
     */
    public int previousIndex()
    {
      return li.previousIndex();
    }

    /**
     * Blocks the replacement of an element in the list underlying this
     * iterator.  This method never returns, throwing an exception instead.
     *
     * @param o The new object to replace the existing one.
     * @throws UnsupportedOperationException as the iterator of an unmodifiable
     *         list does not support the <code>set()</code> operation.
     */
    public void set(T o)
    {
      throw new UnsupportedOperationException();
    }
  } // class UnmodifiableListIterator

  /**
   * Returns an unmodifiable view of the given map. This allows "read-only"
   * access, although changes in the backing map show up in this view.
   * Attempts to modify the map directly, or via collection views or their
   * iterators will fail with {@link UnsupportedOperationException}.
   * Although this view prevents changes to the structure of the map and its
   * entries, the values referenced by the objects in the map can still be
   * modified.   
   * <p>
   *
   * The returned Map implements Serializable, but can only be serialized if
   * the map it wraps is likewise Serializable.
   *
   * @param m the map to wrap
   * @return a read-only view of the map
   * @see Serializable
   */
  public static <K, V> Map<K, V> unmodifiableMap(Map<? extends K,
						 ? extends V> m)
  {
    return new UnmodifiableMap<K, V>(m);
  }

  /**
   * The implementation of {@link #unmodifiableMap(Map)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableMap<K, V> implements Map<K, V>, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -1034234728574286014L;

    /**
     * The wrapped map.
     * @serial the real map
     */
    private final Map<K, V> m;

    /**
     * Cache the entry set.
     */
    private transient Set<Map.Entry<K, V>> entries;

    /**
     * Cache the key set.
     */
    private transient Set<K> keys;

    /**
     * Cache the value collection.
     */
    private transient Collection<V> values;

    /**
     * Wrap a given map.
     * @param m the map to wrap
     * @throws NullPointerException if m is null
     */
    UnmodifiableMap(Map<? extends K, ? extends V> m)
    {
      this.m = (Map<K,V>) m;
      if (m == null)
        throw new NullPointerException();
    }

    /**
     * Blocks the clearing of entries from the underlying map.
     * This method never returns, throwing an exception instead.
     *
     * @throws UnsupportedOperationException as an unmodifiable
     *         map does not support the <code>clear()</code> operation.
     */
    public void clear()
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Returns <code>true</code> if the underlying map contains a mapping for
     * the given key.
     *
     * @param key the key to search for
     * @return <code>true</code> if the map contains the key
     * @throws ClassCastException if the key is of an inappropriate type
     * @throws NullPointerException if key is <code>null</code> but the map
     *         does not permit null keys
     */
    public boolean containsKey(Object key)
    {
      return m.containsKey(key);
    }

    /**
     * Returns <code>true</code> if the underlying map contains at least one mapping with
     * the given value.  In other words, it returns <code>true</code> if a value v exists where
     * <code>(value == null ? v == null : value.equals(v))</code>. This usually
     * requires linear time.
     *
     * @param value the value to search for
     * @return <code>true</code> if the map contains the value
     * @throws ClassCastException if the type of the value is not a valid type
     *         for this map.
     * @throws NullPointerException if the value is null and the map doesn't
     *         support null values.
     */
    public boolean containsValue(Object value)
    {
      return m.containsValue(value);
    }

    /**
     * Returns a unmodifiable set view of the entries in the underlying map.
     * Each element in the set is a unmodifiable variant of <code>Map.Entry</code>.
     * The set is backed by the map, so that changes in one show up in the other.
     * Modifications made while an iterator is in progress cause undefined
     * behavior.  These modifications are again limited to the values of
     * the objects.
     *
     * @return the unmodifiable set view of all mapping entries.
     * @see Map.Entry
     */
    public Set<Map.Entry<K, V>> entrySet()
    {
      if (entries == null)
        entries = new UnmodifiableEntrySet<K,V>(m.entrySet());
      return entries;
    }

    /**
     * The implementation of {@link UnmodifiableMap#entrySet()}. This class
     * name is required for compatibility with Sun's JDK serializability.
     *
     * @author Eric Blake (ebb9@email.byu.edu)
     */
    private static final class UnmodifiableEntrySet<K,V>
      extends UnmodifiableSet<Map.Entry<K,V>>
      implements Serializable
    {
      // Unmodifiable implementation of Map.Entry used as return value for
      // UnmodifiableEntrySet accessors (iterator, toArray, toArray(Object[]))
      private static final class UnmodifiableMapEntry<K,V>
          implements Map.Entry<K,V>
      {
        private final Map.Entry<K,V> e;

        private UnmodifiableMapEntry(Map.Entry<K,V> e)
        {
          super();
          this.e = e;
        }

        /**
         * Returns <code>true</code> if the object, o, is also a map entry
         * with an identical key and value.
         * 
         * @param o the object to compare.
         * @return <code>true</code> if o is an equivalent map entry.
         */
        public boolean equals(Object o)
        {
          return e.equals(o);
        }

        /**
         * Returns the key of this map entry.
         * 
         * @return the key.
         */
        public K getKey()
        {
          return e.getKey();
        }

        /**
         * Returns the value of this map entry.
         * 
         * @return the value.
         */
        public V getValue()
        {
          return e.getValue();
        }

        /**
         * Computes the hash code of this map entry. The computation is
         * described in the <code>Map</code> interface documentation.
         * 
         * @return the hash code of this entry.
         * @see Map#hashCode()
         */
        public int hashCode()
        {
          return e.hashCode();
        }

        /**
         * Blocks the alteration of the value of this map entry. This method
         * never returns, throwing an exception instead.
         * 
         * @param value The new value.
         * @throws UnsupportedOperationException as an unmodifiable map entry
         *           does not support the <code>setValue()</code> operation.
         */
        public V setValue(V value)
        {
          throw new UnsupportedOperationException();
        }

        /**
         * Returns a textual representation of the map entry.
         * 
         * @return The map entry as a <code>String</code>.
         */
        public String toString()
        {
          return e.toString();
        }
      }

      /**
       * Compatible with JDK 1.4.
       */
      private static final long serialVersionUID = 7854390611657943733L;

      /**
       * Wrap a given set.
       * @param s the set to wrap
       */
      UnmodifiableEntrySet(Set<Map.Entry<K,V>> s)
      {
        super(s);
      }

      // The iterator must return unmodifiable map entries.
      public Iterator<Map.Entry<K,V>> iterator()
      {
        return new UnmodifiableIterator<Map.Entry<K,V>>(c.iterator())
	{
	  /**
	   * Obtains the next element from the underlying set of
	   * map entries.
	   *
	   * @return the next element in the collection.
	   * @throws NoSuchElementException if there are no more elements.
	   */
          public Map.Entry<K,V> next()
          {
            final Map.Entry<K,V> e = super.next();
	    return new UnmodifiableMapEntry<K,V>(e);
	  }
	};
      }

      // The array returned is an array of UnmodifiableMapEntry instead of
      // Map.Entry
      public Object[] toArray()
      {
        Object[] mapEntryResult = super.toArray();
        UnmodifiableMapEntry<K,V> result[] = null;
  
        if (mapEntryResult != null)
          {
            result = (UnmodifiableMapEntry<K,V>[])
	      new UnmodifiableMapEntry[mapEntryResult.length];
            for (int i = 0; i < mapEntryResult.length; ++i)
	      result[i] = new UnmodifiableMapEntry<K,V>((Map.Entry<K,V>)mapEntryResult[i]);
	  }
        return result;
      }

      // The array returned is an array of UnmodifiableMapEntry instead of
      // Map.Entry
      public <S> S[] toArray(S[] array)
      {
        S[] result = super.toArray(array);
  
        if (result != null)
	  for (int i = 0; i < result.length; i++)
	    array[i] =
	      (S) new UnmodifiableMapEntry<K,V>((Map.Entry<K,V>) result[i]);
        return array;
      }
      

    } // class UnmodifiableEntrySet

    /**
     * Returns <code>true</code> if the object, o, is also an instance
     * of <code>Map</code> with an equal set of map entries.
     *
     * @param o The object to compare.
     * @return <code>true</code> if o is an equivalent map.
     */
    public boolean equals(Object o)
    {
      return m.equals(o);
    }

    /**
     * Returns the value associated with the supplied key or
     * null if no such mapping exists.  An ambiguity can occur
     * if null values are accepted by the underlying map.
     * In this case, <code>containsKey()</code> can be used
     * to separate the two possible cases of a null result.
     *
     * @param key The key to look up.
     * @return the value associated with the key, or null if key not in map.
     * @throws ClassCastException if the key is an inappropriate type.
     * @throws NullPointerException if this map does not accept null keys.
     * @see #containsKey(Object)
     */
    public V get(Object key)
    {
      return m.get(key);
    }

    /**
     * Blocks the addition of a new entry to the underlying map.
     * This method never returns, throwing an exception instead.
     *
     * @param key The new key.
     * @param value The new value.
     * @return the previous value of the key, or null if there was no mapping.
     * @throws UnsupportedOperationException as an unmodifiable
     *         map does not support the <code>put()</code> operation.
     */
    public V put(K key, V value)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Computes the hash code for the underlying map, as the sum
     * of the hash codes of all entries.
     *
     * @return The hash code of the underlying map.
     * @see Map.Entry#hashCode()
     */
    public int hashCode()
    {
      return m.hashCode();
    }

    /**
     * Returns <code>true</code> if the underlying map contains no entries.
     *
     * @return <code>true</code> if the map is empty.
     */
    public boolean isEmpty()
    {
      return m.isEmpty();
    }

    /**
     * Returns a unmodifiable set view of the keys in the underlying map.
     * The set is backed by the map, so that changes in one show up in the other.
     * Modifications made while an iterator is in progress cause undefined
     * behavior.  These modifications are again limited to the values of
     * the keys.
     *
     * @return the set view of all keys.
     */
    public Set<K> keySet()
    {
      if (keys == null)
        keys = new UnmodifiableSet<K>(m.keySet());
      return keys;
    }

    /**
     * Blocks the addition of the entries in the supplied map.
     * This method never returns, throwing an exception instead.
     *
     * @param m The map, the entries of which should be added
     *          to the underlying map.
     * @throws UnsupportedOperationException as an unmodifiable
     *         map does not support the <code>putAll</code> operation.
     */
    public void putAll(Map<? extends K, ? extends V> m)
    {
      throw new UnsupportedOperationException();
    }

    /**
     * Blocks the removal of an entry from the map.
     * This method never returns, throwing an exception instead.
     *
     * @param o The key of the entry to remove.
     * @return The value the key was associated with, or null
     *         if no such mapping existed.  Null is also returned
     *         if the removed entry had a null key.
     * @throws UnsupportedOperationException as an unmodifiable
     *         map does not support the <code>remove</code> operation.
     */
    public V remove(Object o)
    {
      throw new UnsupportedOperationException();
    }


    /**
     * Returns the number of key-value mappings in the underlying map.
     * If there are more than Integer.MAX_VALUE mappings, Integer.MAX_VALUE
     * is returned.
     *
     * @return the number of mappings.
     */
    public int size()
    {
      return m.size();
    }

    /**
     * Returns a textual representation of the map.
     *
     * @return The map in the form of a <code>String</code>.
     */
    public String toString()
    {
      return m.toString();
    }

    /**
     * Returns a unmodifiable collection view of the values in the underlying map.
     * The collection is backed by the map, so that changes in one show up in the other.
     * Modifications made while an iterator is in progress cause undefined
     * behavior.  These modifications are again limited to the values of
     * the keys.
     *
     * @return the collection view of all values.
     */
    public Collection<V> values()
    {
      if (values == null)
        values = new UnmodifiableCollection<V>(m.values());
      return values;
    }
  } // class UnmodifiableMap

  /**
   * Returns an unmodifiable view of the given set. This allows
   * "read-only" access, although changes in the backing set show up
   * in this view. Attempts to modify the set directly or via iterators
   * will fail with {@link UnsupportedOperationException}.
   * Although this view prevents changes to the structure of the set and its
   * entries, the values referenced by the objects in the set can still be
   * modified.   
   * <p>
   *
   * The returned Set implements Serializable, but can only be serialized if
   * the set it wraps is likewise Serializable.
   *
   * @param s the set to wrap
   * @return a read-only view of the set
   * @see Serializable
   */
  public static <T> Set<T> unmodifiableSet(Set<? extends T> s)
  {
    return new UnmodifiableSet<T>(s);
  }

  /**
   * The implementation of {@link #unmodifiableSet(Set)}. This class
   * name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableSet<T> extends UnmodifiableCollection<T>
    implements Set<T>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -9215047833775013803L;

    /**
     * Wrap a given set.
     * @param s the set to wrap
     * @throws NullPointerException if s is null
     */
    UnmodifiableSet(Set<? extends T> s)
    {
      super(s);
    }

    /**
     * Returns <code>true</code> if the object, o, is also an instance of
     * <code>Set</code> of the same size and with the same entries.
     *
     * @return <code>true</code> if o is an equivalent set.
     */
    public boolean equals(Object o)
    {
      return c.equals(o);
    }

    /**
     * Computes the hash code of this set, as the sum of the
     * hash codes of all elements within the set.
     *
     * @return the hash code of the set.
     */ 
    public int hashCode()
    {
      return c.hashCode();
    }
  } // class UnmodifiableSet

  /**
   * Returns an unmodifiable view of the given sorted map. This allows
   * "read-only" access, although changes in the backing map show up in this
   * view. Attempts to modify the map directly, via subviews, via collection
   * views, or iterators, will fail with {@link UnsupportedOperationException}.
   * Although this view prevents changes to the structure of the map and its
   * entries, the values referenced by the objects in the map can still be
   * modified.   
   * <p>
   *
   * The returned SortedMap implements Serializable, but can only be
   * serialized if the map it wraps is likewise Serializable.
   *
   * @param m the map to wrap
   * @return a read-only view of the map
   * @see Serializable
   */
  public static <K, V> SortedMap<K, V> unmodifiableSortedMap(SortedMap<K,
							     ? extends V> m)
  {
    return new UnmodifiableSortedMap<K, V>(m);
  }

  /**
   * The implementation of {@link #unmodifiableSortedMap(SortedMap)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableSortedMap<K, V>
    extends UnmodifiableMap<K, V>
    implements SortedMap<K, V>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -8806743815996713206L;

    /**
     * The wrapped map; stored both here and in the superclass to avoid
     * excessive casting.
     * @serial the wrapped map
     */
    private final SortedMap<K, V> sm;

    /**
     * Wrap a given map.
     * @param sm the map to wrap
     * @throws NullPointerException if sm is null
     */
    UnmodifiableSortedMap(SortedMap<K, ? extends V> sm)
    {
      super(sm);
      this.sm = (SortedMap<K,V>) sm;
    }

    /**
     * Returns the comparator used in sorting the underlying map,
     * or null if it is the keys' natural ordering.
     *
     * @return the sorting comparator.
     */
    public Comparator<? super K> comparator()
    {
      return sm.comparator();
    }

    /**
     * Returns the first (lowest sorted) key in the map.
     *
     * @return the first key.
     * @throws NoSuchElementException if this map is empty.
     */
    public K firstKey()
    {
      return sm.firstKey();
    }

    /**
     * Returns a unmodifiable view of the portion of the map strictly less
     * than toKey. The view is backed by the underlying map, so changes in
     * one show up in the other.  The submap supports all optional operations
     * of the original.  This operation is equivalent to
     * <code>subMap(firstKey(), toKey)</code>.
     * <p>
     *
     * The returned map throws an IllegalArgumentException any time a key is
     * used which is out of the range of toKey. Note that the endpoint, toKey,
     * is not included; if you want this value to be included, pass its successor
     * object in to toKey.  For example, for Integers, you could request
     * <code>headMap(new Integer(limit.intValue() + 1))</code>.
     *
     * @param toKey the exclusive upper range of the submap.
     * @return the submap.
     * @throws ClassCastException if toKey is not comparable to the map contents.
     * @throws IllegalArgumentException if this is a subMap, and toKey is out
     *         of range.
     * @throws NullPointerException if toKey is null but the map does not allow
     *         null keys.
     */
    public SortedMap<K, V> headMap(K toKey)
    {
      return new UnmodifiableSortedMap<K, V>(sm.headMap(toKey));
    }

    /**
     * Returns the last (highest sorted) key in the map.
     *
     * @return the last key.
     * @throws NoSuchElementException if this map is empty.
     */
    public K lastKey()
    {
      return sm.lastKey();
    }

    /**
     * Returns a unmodifiable view of the portion of the map greater than or
     * equal to fromKey, and strictly less than toKey. The view is backed by
     * the underlying map, so changes in one show up in the other. The submap
     * supports all optional operations of the original.
     * <p>
     *
     * The returned map throws an IllegalArgumentException any time a key is
     * used which is out of the range of fromKey and toKey. Note that the
     * lower endpoint is included, but the upper is not; if you want to
     * change the inclusion or exclusion of an endpoint, pass its successor
     * object in instead.  For example, for Integers, you could request
     * <code>subMap(new Integer(lowlimit.intValue() + 1),
     * new Integer(highlimit.intValue() + 1))</code> to reverse
     * the inclusiveness of both endpoints.
     *
     * @param fromKey the inclusive lower range of the submap.
     * @param toKey the exclusive upper range of the submap.
     * @return the submap.
     * @throws ClassCastException if fromKey or toKey is not comparable to
     *         the map contents.
     * @throws IllegalArgumentException if this is a subMap, and fromKey or
     *         toKey is out of range.
     * @throws NullPointerException if fromKey or toKey is null but the map
     *         does not allow null keys.
     */
    public SortedMap<K, V> subMap(K fromKey, K toKey)
    {
      return new UnmodifiableSortedMap<K, V>(sm.subMap(fromKey, toKey));
    }

    /**
     * Returns a unmodifiable view of the portion of the map greater than or
     * equal to fromKey. The view is backed by the underlying map, so changes
     * in one show up in the other. The submap supports all optional operations
     * of the original.
     * <p>
     *
     * The returned map throws an IllegalArgumentException any time a key is
     * used which is out of the range of fromKey. Note that the endpoint, fromKey, is
     * included; if you do not want this value to be included, pass its successor object in
     * to fromKey.  For example, for Integers, you could request
     * <code>tailMap(new Integer(limit.intValue() + 1))</code>.
     *
     * @param fromKey the inclusive lower range of the submap
     * @return the submap
     * @throws ClassCastException if fromKey is not comparable to the map
     *         contents
     * @throws IllegalArgumentException if this is a subMap, and fromKey is out
     *         of range
     * @throws NullPointerException if fromKey is null but the map does not allow
     *         null keys
     */
    public SortedMap<K, V> tailMap(K fromKey)
    {
      return new UnmodifiableSortedMap<K, V>(sm.tailMap(fromKey));
    }
  } // class UnmodifiableSortedMap

  /**
   * Returns an unmodifiable view of the given sorted set. This allows
   * "read-only" access, although changes in the backing set show up
   * in this view. Attempts to modify the set directly, via subsets, or via
   * iterators, will fail with {@link UnsupportedOperationException}.
   * Although this view prevents changes to the structure of the set and its
   * entries, the values referenced by the objects in the set can still be
   * modified.   
   * <p>
   *
   * The returns SortedSet implements Serializable, but can only be
   * serialized if the set it wraps is likewise Serializable.
   *
   * @param s the set to wrap
   * @return a read-only view of the set
   * @see Serializable
   */
  public static <T> SortedSet<T> unmodifiableSortedSet(SortedSet<T> s)
  {
    return new UnmodifiableSortedSet<T>(s);
  }

  /**
   * The implementation of {@link #synchronizedSortedMap(SortedMap)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class UnmodifiableSortedSet<T> extends UnmodifiableSet<T>
    implements SortedSet<T>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -4929149591599911165L;

    /**
     * The wrapped set; stored both here and in the superclass to avoid
     * excessive casting.
     * @serial the wrapped set
     */
    private SortedSet<T> ss;

    /**
     * Wrap a given set.
     * @param ss the set to wrap
     * @throws NullPointerException if ss is null
     */
    UnmodifiableSortedSet(SortedSet<T> ss)
    {
      super(ss);
      this.ss = ss;
    }

    /**
     * Returns the comparator used in sorting the underlying set,
     * or null if it is the elements' natural ordering.
     *
     * @return the sorting comparator
     */
    public Comparator<? super T> comparator()
    {
      return ss.comparator();
    }

    /**
     * Returns the first (lowest sorted) element in the underlying
     * set.
     *
     * @return the first element.
     * @throws NoSuchElementException if the set is empty.
     */
    public T first()
    {
      return ss.first();
    }

    /**
     * Returns a unmodifiable view of the portion of the set strictly
     * less than toElement. The view is backed by the underlying set,
     * so changes in one show up in the other.  The subset supports
     * all optional operations of the original.  This operation
     * is equivalent to <code>subSet(first(), toElement)</code>.
     * <p>
     *
     * The returned set throws an IllegalArgumentException any time an element is
     * used which is out of the range of toElement. Note that the endpoint, toElement,
     * is not included; if you want this value included, pass its successor object in to
     * toElement.  For example, for Integers, you could request
     * <code>headSet(new Integer(limit.intValue() + 1))</code>.
     *
     * @param toElement the exclusive upper range of the subset
     * @return the subset.
     * @throws ClassCastException if toElement is not comparable to the set
     *         contents.
     * @throws IllegalArgumentException if this is a subSet, and toElement is out
     *         of range.
     * @throws NullPointerException if toElement is null but the set does not
     *         allow null elements.
     */
    public SortedSet<T> headSet(T toElement)
    {
      return new UnmodifiableSortedSet<T>(ss.headSet(toElement));
    }

    /**
     * Returns the last (highest sorted) element in the underlying
     * set.
     *
     * @return the last element.
     * @throws NoSuchElementException if the set is empty.
     */
    public T last()
    {
      return ss.last();
    }

    /**
     * Returns a unmodifiable view of the portion of the set greater than or
     * equal to fromElement, and strictly less than toElement. The view is backed by
     * the underlying set, so changes in one show up in the other. The subset
     * supports all optional operations of the original.
     * <p>
     *
     * The returned set throws an IllegalArgumentException any time an element is
     * used which is out of the range of fromElement and toElement. Note that the
     * lower endpoint is included, but the upper is not; if you want to
     * change the inclusion or exclusion of an endpoint, pass its successor
     * object in instead.  For example, for Integers, you can request
     * <code>subSet(new Integer(lowlimit.intValue() + 1),
     * new Integer(highlimit.intValue() + 1))</code> to reverse
     * the inclusiveness of both endpoints.
     *
     * @param fromElement the inclusive lower range of the subset.
     * @param toElement the exclusive upper range of the subset.
     * @return the subset.
     * @throws ClassCastException if fromElement or toElement is not comparable
     *         to the set contents.
     * @throws IllegalArgumentException if this is a subSet, and fromElement or
     *         toElement is out of range.
     * @throws NullPointerException if fromElement or toElement is null but the
     *         set does not allow null elements.
     */
    public SortedSet<T> subSet(T fromElement, T toElement)
    {
      return new UnmodifiableSortedSet<T>(ss.subSet(fromElement, toElement));
    }

    /**
     * Returns a unmodifiable view of the portion of the set greater than or equal to
     * fromElement. The view is backed by the underlying set, so changes in one show up
     * in the other. The subset supports all optional operations of the original.
     * <p>
     *
     * The returned set throws an IllegalArgumentException any time an element is
     * used which is out of the range of fromElement. Note that the endpoint,
     * fromElement, is included; if you do not want this value to be included, pass its
     * successor object in to fromElement.  For example, for Integers, you could request
     * <code>tailSet(new Integer(limit.intValue() + 1))</code>.
     *
     * @param fromElement the inclusive lower range of the subset
     * @return the subset.
     * @throws ClassCastException if fromElement is not comparable to the set
     *         contents.
     * @throws IllegalArgumentException if this is a subSet, and fromElement is
     *         out of range.
     * @throws NullPointerException if fromElement is null but the set does not
     *         allow null elements.
     */
    public SortedSet<T> tailSet(T fromElement)
    {
      return new UnmodifiableSortedSet<T>(ss.tailSet(fromElement));
    }
  } // class UnmodifiableSortedSet

  /**
   * <p> 
   * Returns a dynamically typesafe view of the given collection,
   * where any modification is first checked to ensure that the type
   * of the new data is appropriate.  Although the addition of
   * generics and parametrically-typed collections prevents an
   * incorrect type of element being added to a collection at
   * compile-time, via static type checking, this can be overridden by
   * casting.  In contrast, wrapping the collection within a
   * dynamically-typesafe wrapper, using this and associated methods,
   * <emph>guarantees</emph> that the collection will only contain
   * elements of an appropriate type (provided it only contains such
   * at the type of wrapping, and all subsequent access is via the
   * wrapper).  This can be useful for debugging the cause of a
   * <code>ClassCastException</code> caused by erroneous casting, or
   * for protecting collections from corruption by external libraries.
   * </p>
   * <p> 
   * Since the collection might be a List or a Set, and those
   * have incompatible equals and hashCode requirements, this relies
   * on Object's implementation rather than passing those calls on to
   * the wrapped collection. The returned Collection implements
   * Serializable, but can only be serialized if the collection it
   * wraps is likewise Serializable.
   * </p>
   * 
   * @param c the collection to wrap in a dynamically typesafe wrapper
   * @param type the type of elements the collection should hold.
   * @return a dynamically typesafe view of the collection.
   * @see Serializable
   * @since 1.5
   */
  public static <E> Collection<E> checkedCollection(Collection<E> c,
						    Class<E> type)
  {
    return new CheckedCollection<E>(c, type);
  }

  /**
   * The implementation of {@link #checkedCollection(Collection,Class)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class CheckedCollection<E>
    implements Collection<E>, Serializable
  {
    /**
     * Compatible with JDK 1.5.
     */
    private static final long serialVersionUID = 1578914078182001775L;
    
    /**
     * The wrapped collection. Package visible for use by subclasses.
     * @serial the real collection
     */
    final Collection<E> c;

    /**
     * The type of the elements of this collection.
     * @serial the element type.
     */
    final Class<E> type;

    /**
     * Wrap a given collection.
     * @param c the collection to wrap
     * @param type the type to wrap
     * @throws NullPointerException if c is null
     */
    CheckedCollection(Collection<E> c, Class<E> type)
    {
      this.c = c;
      this.type = type;
      if (c == null)
        throw new NullPointerException();
    }

    /**
     * Adds the supplied object to the collection, on the condition that
     * it is of the correct type.
     *
     * @param o the object to add.
     * @return <code>true</code> if the collection was modified as a result
     *                           of this action.
     * @throws ClassCastException if the object is not of the correct type.
     */
    public boolean add(E o)
    {
      if (type.isInstance(o))
	return c.add(o);
      else
	throw new ClassCastException("The element is of the incorrect type.");
    }

    /**
     * Adds the elements of the specified collection to the backing collection,
     * provided they are all of the correct type.
     *
     * @param coll the collection to add.
     * @return <code>true</code> if the collection was modified as a result
     *                           of this action.
     * @throws ClassCastException if <code>c</code> contained elements of an
     *                            incorrect type.
     */
    public boolean addAll(Collection<? extends E> coll)
    {
      Collection<E> typedColl = (Collection<E>) c;
      final Iterator<E> it = typedColl.iterator();
      while (it.hasNext())
	{
	  final E element = it.next();
	  if (!type.isInstance(element))
	    throw new ClassCastException("A member of the collection is not of the correct type.");
	}
      return c.addAll(typedColl);
    }

    /**
     * Removes all elements from the underlying collection.
     */
    public void clear()
    {
      c.clear();
    }

    /**
     * Test whether the underlying collection contains a given object as one
     * of its elements.
     *
     * @param o the element to look for.
     * @return <code>true</code> if the underlying collection contains at least
     *         one element e such that
     *         <code>o == null ? e == null : o.equals(e)</code>.
     * @throws ClassCastException if the type of o is not a valid type for the
     *         underlying collection.
     * @throws NullPointerException if o is null and the underlying collection
     *         doesn't support null values.
     */
    public boolean contains(Object o)
    {
      return c.contains(o);
    }

    /**
     * Test whether the underlying collection contains every element in a given
     * collection.
     *
     * @param coll the collection to test for.
     * @return <code>true</code> if for every element o in c, contains(o) would
     *         return <code>true</code>.
     * @throws ClassCastException if the type of any element in c is not a
     *                            valid type for the underlying collection.
     * @throws NullPointerException if some element of c is null and the
     *                              underlying collection does not support
     *                              null values.
     * @throws NullPointerException if c itself is null.
     */
    public boolean containsAll(Collection<?> coll)
    {
      return c.containsAll(coll);
    }

    /**
     * Tests whether the underlying collection is empty, that is,
     * if size() == 0.
     *
     * @return <code>true</code> if this collection contains no elements.
     */
    public boolean isEmpty()
    {
      return c.isEmpty();
    }

    /**
     * Obtain an Iterator over the underlying collection, which maintains
     * its checked nature.
     *
     * @return a Iterator over the elements of the underlying
     *         collection, in any order.
     */
    public Iterator<E> iterator()
    {
      return new CheckedIterator<E>(c.iterator(), type);
    }

    /**
     * Removes the supplied object from the collection, if it exists.
     *
     * @param o The object to remove.
     * @return <code>true</code> if the object was removed (i.e. the underlying
     *         collection returned 1 or more instances of o).
     */
    public boolean remove(Object o)
    {
      return c.remove(o);
    }

    /**
     * Removes all objects in the supplied collection from the backing
     * collection, if they exist within it.
     *
     * @param coll the collection of objects to remove.
     * @return <code>true</code> if the collection was modified.
     */
    public boolean removeAll(Collection<?> coll)
    {
      return c.removeAll(coll);
    }

    /**
     * Retains all objects specified by the supplied collection which exist
     * within the backing collection, and removes all others.
     *
     * @param coll the collection of objects to retain.
     * @return <code>true</code> if the collection was modified.
     */
    public boolean retainAll(Collection<?> coll)
    {
      return c.retainAll(coll);
    }

    /**
     * Retrieves the number of elements in the underlying collection.
     *
     * @return the number of elements in the collection.
     */
    public int size()
    {
      return c.size();
    }

    /**
     * Copy the current contents of the underlying collection into an array.
     *
     * @return an array of type Object[] with a length equal to the size of the
     *         underlying collection and containing the elements currently in
     *         the underlying collection, in any order.
     */
    public Object[] toArray()
    {
      return c.toArray();
    }

    /**
     * <p>
     * Copy the current contents of the underlying collection into an array. If
     * the array passed as an argument has length less than the size of the
     * underlying collection, an array of the same run-time type as a, with a
     * length equal to the size of the underlying collection, is allocated
     * using reflection.
     * </p>
     * <p>
     * Otherwise, a itself is used.  The elements of the underlying collection
     * are copied into it, and if there is space in the array, the following
     * element is set to null. The resultant array is returned.
     * </p>
     * <p>
     * <emph>Note</emph>: The fact that the following element is set to null
     * is only useful if it is known that this collection does not contain
     * any null elements.
     *
     * @param a the array to copy this collection into.
     * @return an array containing the elements currently in the underlying
     *         collection, in any order.
     * @throws ArrayStoreException if the type of any element of the
     *         collection is not a subtype of the element type of a.
     */
    public <S> S[] toArray(S[] a)
    {
      return c.toArray(a);
    }

    /**
     * A textual representation of the unmodifiable collection.
     *
     * @return The checked collection in the form of a <code>String</code>.
     */
    public String toString()
    {
      return c.toString();
    }
  } // class CheckedCollection

  /**
   * The implementation of the various iterator methods in the
   * checked classes.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class CheckedIterator<E>
    implements Iterator<E>
  {
    /**
     * The wrapped iterator.
     */
    private final Iterator<E> i;

    /**
     * The type of the elements of this collection.
     * @serial the element type.
     */
    final Class<E> type;

    /**
     * Only trusted code creates a wrapper.
     * @param i the wrapped iterator
     * @param type the type of the elements within the checked list.
     */
    CheckedIterator(Iterator<E> i, Class<E> type)
    {
      this.i = i;
      this.type = type;
    }

    /**
     * Obtains the next element in the underlying collection.
     *
     * @return the next element in the collection.
     * @throws NoSuchElementException if there are no more elements.
     */
    public E next()
    {
      return i.next();
    }

    /**
     * Tests whether there are still elements to be retrieved from the
     * underlying collection by <code>next()</code>.  When this method
     * returns <code>true</code>, an exception will not be thrown on calling
     * <code>next()</code>.
     *
     * @return <code>true</code> if there is at least one more element in the
     *         underlying collection.
     */
    public boolean hasNext()
    {
      return i.hasNext();
    }

    /**
     * Removes the next element from the collection.
     */
    public void remove()
    {
      i.remove();
    }
  } // class CheckedIterator

  /**
   * <p> 
   * Returns a dynamically typesafe view of the given list,
   * where any modification is first checked to ensure that the type
   * of the new data is appropriate.  Although the addition of
   * generics and parametrically-typed collections prevents an
   * incorrect type of element being added to a collection at
   * compile-time, via static type checking, this can be overridden by
   * casting.  In contrast, wrapping the collection within a
   * dynamically-typesafe wrapper, using this and associated methods,
   * <emph>guarantees</emph> that the collection will only contain
   * elements of an appropriate type (provided it only contains such
   * at the type of wrapping, and all subsequent access is via the
   * wrapper).  This can be useful for debugging the cause of a
   * <code>ClassCastException</code> caused by erroneous casting, or
   * for protecting collections from corruption by external libraries.
   * </p>
   * <p>
   * The returned List implements Serializable, but can only be serialized if
   * the list it wraps is likewise Serializable. In addition, if the wrapped
   * list implements RandomAccess, this does too.
   * </p>
   *
   * @param l the list to wrap
   * @param type the type of the elements within the checked list.
   * @return a dynamically typesafe view of the list
   * @see Serializable
   * @see RandomAccess
   */
  public static <E> List<E> checkedList(List<E> l, Class<E> type)
  {
    if (l instanceof RandomAccess)
      return new CheckedRandomAccessList<E>(l, type);
    return new CheckedList<E>(l, type);
  }

  /**
   * The implementation of {@link #checkedList(List,Class)} for sequential
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class CheckedList<E> 
    extends CheckedCollection<E>
    implements List<E>
  {
    /**
     * Compatible with JDK 1.5.
     */
    private static final long serialVersionUID = 65247728283967356L;

    /**
     * The wrapped list; stored both here and in the superclass to avoid
     * excessive casting. Package visible for use by subclass.
     * @serial the wrapped list
     */
    final List<E> list;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @param type the type of the elements within the checked list.
     * @throws NullPointerException if l is null
     */
    CheckedList(List<E> l, Class<E> type)
    {
      super(l, type);
      list = l;
    }

    /**
     * Adds the supplied element to the underlying list at the specified
     * index, provided it is of the right type.
     *
     * @param index The index at which to place the new element.
     * @param o the object to add.
     * @throws ClassCastException if the type of the object is not a
     *                            valid type for the underlying collection.
     */
    public void add(int index, E o)
    {
      if (type.isInstance(o))
	list.add(index, o);
      else
	throw new ClassCastException("The object is of the wrong type.");
    }

    /**
     * Adds the members of the supplied collection to the underlying
     * collection at the specified index, provided they are all of the
     * correct type.
     *
     * @param index the index at which to place the new element.
     * @param c the collections of objects to add.
     * @throws ClassCastException if the type of any element in c is not a
     *                            valid type for the underlying collection.
     */
    public boolean addAll(int index, Collection<? extends E> coll)
    {
      Collection<E> typedColl = (Collection<E>) coll;
      final Iterator<E> it = typedColl.iterator();
      while (it.hasNext())
	{
	  if (!type.isInstance(it.next()))
	    throw new ClassCastException("A member of the collection is not of the correct type.");
	}
      return list.addAll(index, coll);
    }

    /**
     * Returns <code>true</code> if the object, o, is an instance of
     * <code>List</code> with the same size and elements
     * as the underlying list.
     *
     * @param o The object to compare.
     * @return <code>true</code> if o is equivalent to the underlying list.
     */
    public boolean equals(Object o)
    {
      return list.equals(o);
    }

    /**
     * Retrieves the element at a given index in the underlying list.
     *
     * @param index the index of the element to be returned
     * @return the element at the specified index in the underlying list
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     */
    public E get(int index)
    {
      return list.get(index);
    }

    /**
     * Computes the hash code for the underlying list.
     * The exact computation is described in the documentation
     * of the <code>List</code> interface.
     *
     * @return The hash code of the underlying list.
     * @see List#hashCode()
     */
    public int hashCode()
    {
      return list.hashCode();
    }

    /**
     * Obtain the first index at which a given object is to be found in the
     * underlying list.
     *
     * @param o the object to search for
     * @return the least integer n such that <code>o == null ? get(n) == null :
     *         o.equals(get(n))</code>, or -1 if there is no such index.
     * @throws ClassCastException if the type of o is not a valid
     *         type for the underlying list.
     * @throws NullPointerException if o is null and the underlying
     *         list does not support null values.
     */
    public int indexOf(Object o)
    {
      return list.indexOf(o);
    }

    /**
     * Obtain the last index at which a given object is to be found in the
     * underlying list.
     *
     * @return the greatest integer n such that
     *         <code>o == null ? get(n) == null : o.equals(get(n))</code>,
     *         or -1 if there is no such index.
     * @throws ClassCastException if the type of o is not a valid
     *         type for the underlying list.
     * @throws NullPointerException if o is null and the underlying
     *         list does not support null values.
     */
    public int lastIndexOf(Object o)
    {
      return list.lastIndexOf(o);
    }

    /**
     * Obtains a list iterator over the underlying list, starting at the
     * beginning and maintaining the checked nature of this list.
     *
     * @return a <code>CheckedListIterator</code> over the elements of the
     *         underlying list, in order, starting at the beginning.
     */
    public ListIterator<E> listIterator()
    {
      return new CheckedListIterator<E>(list.listIterator(), type);
    }

  /**
   * Obtains a list iterator over the underlying list, starting at the
   * specified index and maintaining the checked nature of this list.  An
   * initial call to <code>next()</code> will retrieve the element at the
   * specified index, and an initial call to <code>previous()</code> will
   * retrieve the element at index - 1.
   *
   * @param index the position, between 0 and size() inclusive, to begin the
   *        iteration from.
   * @return a <code>CheckedListIterator</code> over the elements of the
   *         underlying list, in order, starting at the specified index.
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
    public ListIterator<E> listIterator(int index)
    {
      return new CheckedListIterator<E>(list.listIterator(index), type);
    }

    /**
     * Removes the element at the specified index.
     *
     * @param index The index of the element to remove.
     * @return the removed element.
     */
    public E remove(int index)
    {
      return list.remove(index);
    }

    /**
     * Replaces the element at the specified index in the underlying list
     * with that supplied.
     *
     * @param index the index of the element to replace.
     * @param o the new object to place at the specified index.
     * @return the replaced element.
     */
    public E set(int index, E o)
    {
      return list.set(index, o);
    }

    /**
     * Obtain a List view of a subsection of the underlying list, from
     * fromIndex (inclusive) to toIndex (exclusive). If the two indices
     * are equal, the sublist is empty. The returned list will be
     * checked, like this list.  Changes to the elements of the
     * returned list will be reflected in the underlying list. The effect
     * of structural modifications is undefined.
     *
     * @param fromIndex the index that the returned list should start from
     *        (inclusive).
     * @param toIndex the index that the returned list should go
     *                to (exclusive).
     * @return a List backed by a subsection of the underlying list.
     * @throws IndexOutOfBoundsException if fromIndex &lt; 0
     *         || toIndex &gt; size() || fromIndex &gt; toIndex.
     */
    public List<E> subList(int fromIndex, int toIndex)
    {
      return checkedList(list.subList(fromIndex, toIndex), type);
    }
  } // class CheckedList

  /**
   * The implementation of {@link #checkedList(List)} for random-access
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class CheckedRandomAccessList<E>
    extends CheckedList<E>
    implements RandomAccess
  {
    /**
     * Compatible with JDK 1.5.
     */
    private static final long serialVersionUID = 1638200125423088369L;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @param type the type of the elements within the checked list.
     * @throws NullPointerException if l is null
     */
    CheckedRandomAccessList(List<E> l, Class<E> type)
    {
      super(l, type);
    }
  } // class CheckedRandomAccessList

  /**
   * The implementation of {@link CheckedList#listIterator()}.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static final class CheckedListIterator<E>
    extends CheckedIterator<E>
    implements ListIterator<E>
  {
    /**
     * The wrapped iterator, stored both here and in the superclass to
     * avoid excessive casting.
     */
    private final ListIterator<E> li;

    /**
     * Only trusted code creates a wrapper.
     * @param li the wrapped iterator
     */
    CheckedListIterator(ListIterator<E> li, Class<E> type)
    {
      super(li, type);
      this.li = li;
    }

    /**
     * Adds the supplied object at the current iterator position, provided
     * it is of the correct type.
     *
     * @param o the object to add.
     * @throws ClassCastException if the type of the object is not a
     *                            valid type for the underlying collection.
     */
    public void add(E o)
    {
      if (type.isInstance(o))
	li.add(o);
      else
	throw new ClassCastException("The object is of the wrong type.");
    }

    /**
     * Tests whether there are still elements to be retrieved from the
     * underlying collection by <code>previous()</code>.  When this method
     * returns <code>true</code>, an exception will not be thrown on calling
     * <code>previous()</code>.
     *
     * @return <code>true</code> if there is at least one more element prior
     *         to the current position in the underlying list.
     */
    public boolean hasPrevious()
    {
      return li.hasPrevious();
    }

    /**
     * Find the index of the element that would be returned by a call to next.
     * If <code>hasNext()</code> returns <code>false</code>, this returns the
     * list size.
     *
     * @return the index of the element that would be returned by
     *         <code>next()</code>.
     */
    public int nextIndex()
    {
      return li.nextIndex();
    }

    /**
     * Obtains the previous element in the underlying list.
     *
     * @return the previous element in the list.
     * @throws NoSuchElementException if there are no more prior elements.
     */
    public E previous()
    {
      return li.previous();
    }

    /**
     * Find the index of the element that would be returned by a call to
     * previous. If <code>hasPrevious()</code> returns <code>false</code>,
     * this returns -1.
     *
     * @return the index of the element that would be returned by
     *         <code>previous()</code>.
     */
    public int previousIndex()
    {
      return li.previousIndex();
    }

    /**
     * Sets the next element to that supplied, provided that it is of the
     * correct type.
     *
     * @param o The new object to replace the existing one.
     * @throws ClassCastException if the type of the object is not a
     *                            valid type for the underlying collection.
     */
    public void set(E o)
    {
      if (type.isInstance(o))
	li.set(o);
      else
	throw new ClassCastException("The object is of the wrong type.");
    }
  } // class CheckedListIterator

  /**
   * <p> 
   * Returns a dynamically typesafe view of the given map,
   * where any modification is first checked to ensure that the type
   * of the new data is appropriate.  Although the addition of
   * generics and parametrically-typed collections prevents an
   * incorrect type of element being added to a collection at
   * compile-time, via static type checking, this can be overridden by
   * casting.  In contrast, wrapping the collection within a
   * dynamically-typesafe wrapper, using this and associated methods,
   * <emph>guarantees</emph> that the collection will only contain
   * elements of an appropriate type (provided it only contains such
   * at the type of wrapping, and all subsequent access is via the
   * wrapper).  This can be useful for debugging the cause of a
   * <code>ClassCastException</code> caused by erroneous casting, or
   * for protecting collections from corruption by external libraries.
   * </p>
   * <p>
   * The returned Map implements Serializable, but can only be serialized if
   * the map it wraps is likewise Serializable.
   * </p>
   *
   * @param m the map to wrap
   * @param keyType the dynamic type of the map's keys.
   * @param valueType the dynamic type of the map's values.
   * @return a dynamically typesafe view of the map
   * @see Serializable
   */
  public static <K, V> Map<K, V> checkedMap(Map<K, V> m, Class<K> keyType,
					    Class<V> valueType)
  {
    return new CheckedMap<K, V>(m, keyType, valueType);
  }

  /**
   * The implementation of {@link #checkedMap(Map)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class CheckedMap<K, V> 
    implements Map<K, V>, Serializable
  {
    /**
     * Compatible with JDK 1.5.
     */
    private static final long serialVersionUID = 5742860141034234728L;

    /**
     * The wrapped map.
     * @serial the real map
     */
    private final Map<K, V> m;

    /**
     * The type of the map's keys.
     * @serial the key type.
     */
    final Class<K> keyType;

    /**
     * The type of the map's values.
     * @serial the value type.
     */
    final Class<V> valueType;

    /**
     * Cache the entry set.
     */
    private transient Set<Map.Entry<K, V>> entries;

    /**
     * Cache the key set.
     */
    private transient Set<K> keys;

    /**
     * Cache the value collection.
     */
    private transient Collection<V> values;

    /**
     * Wrap a given map.
     * @param m the map to wrap
     * @param keyType the dynamic type of the map's keys.
     * @param valueType the dynamic type of the map's values.
     * @throws NullPointerException if m is null
     */
    CheckedMap(Map<K, V> m, Class<K> keyType, Class<V> valueType)
    {
      this.m = m;
      this.keyType = keyType;
      this.valueType = valueType;
      if (m == null)
        throw new NullPointerException();
    }

    /**
     * Clears all pairs from the map.
     */
    public void clear()
    {
      m.clear();
    }

    /**
     * Returns <code>true</code> if the underlying map contains a mapping for
     * the given key.
     *
     * @param key the key to search for
     * @return <code>true</code> if the map contains the key
     * @throws ClassCastException if the key is of an inappropriate type
     * @throws NullPointerException if key is <code>null</code> but the map
     *         does not permit null keys
     */
    public boolean containsKey(Object key)
    {
      return m.containsKey(key);
    }

    /**
     * Returns <code>true</code> if the underlying map contains at least one
     * mapping with the given value.  In other words, it returns
     * <code>true</code> if a value v exists where
     * <code>(value == null ? v == null : value.equals(v))</code>.
     * This usually requires linear time.
     *
     * @param value the value to search for
     * @return <code>true</code> if the map contains the value
     * @throws ClassCastException if the type of the value is not a valid type
     *         for this map.
     * @throws NullPointerException if the value is null and the map doesn't
     *         support null values.
     */
    public boolean containsValue(Object value)
    {
      return m.containsValue(value);
    }

    /**
     * <p>
     * Returns a checked set view of the entries in the underlying map.
     * Each element in the set is a unmodifiable variant of
     * <code>Map.Entry</code>.
     * </p>
     * <p>
     * The set is backed by the map, so that changes in one show up in the
     * other.  Modifications made while an iterator is in progress cause
     * undefined behavior.  
     * </p>
     *
     * @return the checked set view of all mapping entries.
     * @see Map.Entry
     */
    public Set<Map.Entry<K, V>> entrySet()
    {
      if (entries == null)
	{
	  Class<Map.Entry<K,V>> klass =
	    (Class<Map.Entry<K,V>>) (Class) Map.Entry.class;
	  entries = new CheckedEntrySet<Map.Entry<K,V>,K,V>(m.entrySet(),
							    klass,
							    keyType,
							    valueType);
	}
      return entries;
    }

    /**
     * The implementation of {@link CheckedMap#entrySet()}. This class
     * is <emph>not</emph> serializable.
     *
     * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
     * @since 1.5
     */
    private static final class CheckedEntrySet<E,SK,SV>
      extends CheckedSet<E>
    {
      /**
       * The type of the map's keys.
       * @serial the key type.
       */
      private final Class<SK> keyType;
      
      /**
       * The type of the map's values.
       * @serial the value type.
       */
      private final Class<SV> valueType;
      
      /**
       * Wrap a given set of map entries.
       *
       * @param s the set to wrap.
       * @param type the type of the set's entries.
       * @param keyType the type of the map's keys.
       * @param valueType the type of the map's values.
       */
      CheckedEntrySet(Set<E> s, Class<E> type, Class<SK> keyType,
		      Class<SV> valueType)
      {
        super(s, type);
	this.keyType = keyType;
	this.valueType = valueType;
      }

      // The iterator must return checked map entries.
      public Iterator<E> iterator()
      {
        return new CheckedIterator<E>(c.iterator(), type)
	{
	  /**
	   * Obtains the next element from the underlying set of
	   * map entries.
	   *
	   * @return the next element in the collection.
	   * @throws NoSuchElementException if there are no more elements.
	   */
          public E next()
          {
            final Map.Entry e = (Map.Entry) super.next();
            return (E) new Map.Entry()
	    {
	      /**
	       * Returns <code>true</code> if the object, o, is also a map
	       * entry with an identical key and value.
	       *
	       * @param o the object to compare.
	       * @return <code>true</code> if o is an equivalent map entry.
	       */
              public boolean equals(Object o)
              {
                return e.equals(o);
              }
	      
	      /**
	       * Returns the key of this map entry.
	       *
	       * @return the key.
	       */
              public Object getKey()
              {
                return e.getKey();
              }

	      /**
	       * Returns the value of this map entry.
	       *
	       * @return the value.
	       */
              public Object getValue()
              {
                return e.getValue();
              }

	      /**
	       * Computes the hash code of this map entry.
	       * The computation is described in the <code>Map</code>
	       * interface documentation.
	       *
	       * @return the hash code of this entry.
	       * @see Map#hashCode()
	       */ 
	      public int hashCode()
              {
                return e.hashCode();
              }

	      /**
	       * Sets the value of this map entry, provided it is of the
	       * right type.
	       *
	       * @param value The new value.
	       * @throws ClassCastException if the type of the value is not
	       *                            a valid type for the underlying
	       *                             map.
	       */
              public Object setValue(Object value)
              {
		if (valueType.isInstance(value))
		  return e.setValue(value);
		else
		  throw new ClassCastException("The value is of the wrong type.");
              }

	      /**
	       * Returns a textual representation of the map entry.
	       *
	       * @return The map entry as a <code>String</code>.
	       */
              public String toString()
              {
                return e.toString();
              }
	    };
          }
	};
      }
    } // class CheckedEntrySet

    /**
     * Returns <code>true</code> if the object, o, is also an instance
     * of <code>Map</code> with an equal set of map entries.
     *
     * @param o The object to compare.
     * @return <code>true</code> if o is an equivalent map.
     */
    public boolean equals(Object o)
    {
      return m.equals(o);
    }

    /**
     * Returns the value associated with the supplied key or
     * null if no such mapping exists.  An ambiguity can occur
     * if null values are accepted by the underlying map.
     * In this case, <code>containsKey()</code> can be used
     * to separate the two possible cases of a null result.
     *
     * @param key The key to look up.
     * @return the value associated with the key, or null if key not in map.
     * @throws ClassCastException if the key is an inappropriate type.
     * @throws NullPointerException if this map does not accept null keys.
     * @see #containsKey(Object)
     */
    public V get(Object key)
    {
      return m.get(key);
    }

    /**
     * Adds a new pair to the map, provided both the key and the value are
     * of the correct types.
     *
     * @param key The new key.
     * @param value The new value.
     * @return the previous value of the key, or null if there was no mapping.
     * @throws ClassCastException if the type of the key or the value is
     *                            not a valid type for the underlying map.    
     */
    public V put(K key, V value)
    {
      if (keyType.isInstance(key))
	{
	  if (valueType.isInstance(value))
	    return m.put(key,value);
	  else
	    throw new ClassCastException("The value is of the wrong type.");
	}
      throw new ClassCastException("The key is of the wrong type.");
    }

    /**
     * Computes the hash code for the underlying map, as the sum
     * of the hash codes of all entries.
     *
     * @return The hash code of the underlying map.
     * @see Map.Entry#hashCode()
     */
    public int hashCode()
    {
      return m.hashCode();
    }

    /**
     * Returns <code>true</code> if the underlying map contains no entries.
     *
     * @return <code>true</code> if the map is empty.
     */
    public boolean isEmpty()
    {
      return m.isEmpty();
    }

    /**
     * <p>
     * Returns a checked set view of the keys in the underlying map.
     * The set is backed by the map, so that changes in one show up in the
     * other.
     * </p>
     * <p>
     * Modifications made while an iterator is in progress cause undefined
     * behavior.  These modifications are again limited to the values of
     * the keys.
     * </p>
     *
     * @return the set view of all keys.
     */
    public Set<K> keySet()
    {
      if (keys == null)
        keys = new CheckedSet<K>(m.keySet(), keyType);
      return keys;
    }

    /**
     * Adds all pairs within the supplied map to the underlying map,
     * provided they are all have the correct key and value types.
     *
     * @param m the map, the entries of which should be added
     *          to the underlying map.
     * @throws ClassCastException if the type of a key or value is
     *                            not a valid type for the underlying map.    
     */
    public void putAll(Map<? extends K, ? extends V> map)
    {
      Map<K,V> typedMap = (Map<K,V>) map;
      final Iterator<Map.Entry<K,V>> it = typedMap.entrySet().iterator();
      while (it.hasNext())
	{
	  final Map.Entry<K,V> entry = it.next();
	  if (!keyType.isInstance(entry.getKey()))
	    throw new ClassCastException("A key is of the wrong type.");
	  if (!valueType.isInstance(entry.getValue()))
	    throw new ClassCastException("A value is of the wrong type.");
	}
      m.putAll(typedMap);
    }

    /**
     * Removes a pair from the map.
     *
     * @param o The key of the entry to remove.
     * @return The value the key was associated with, or null
     *         if no such mapping existed.  Null is also returned
     *         if the removed entry had a null key.
     * @throws UnsupportedOperationException as an unmodifiable
     *         map does not support the <code>remove</code> operation.
     */
    public V remove(Object o)
    {
      return m.remove(o);
    }


    /**
     * Returns the number of key-value mappings in the underlying map.
     * If there are more than Integer.MAX_VALUE mappings, Integer.MAX_VALUE
     * is returned.
     *
     * @return the number of mappings.
     */
    public int size()
    {
      return m.size();
    }

    /**
     * Returns a textual representation of the map.
     *
     * @return The map in the form of a <code>String</code>.
     */
    public String toString()
    {
      return m.toString();
    }

    /**
     * <p>
     * Returns a unmodifiable collection view of the values in the underlying
     * map.  The collection is backed by the map, so that changes in one show
     * up in the other.
     * </p>
     * <p>
     * Modifications made while an iterator is in progress cause undefined
     * behavior.  These modifications are again limited to the values of
     * the keys.
     * </p>
     * 
     * @return the collection view of all values.
     */
    public Collection<V> values()
    {
      if (values == null)
        values = new CheckedCollection<V>(m.values(), valueType);
      return values;
    }
  } // class CheckedMap

  /**
   * <p> 
   * Returns a dynamically typesafe view of the given set,
   * where any modification is first checked to ensure that the type
   * of the new data is appropriate.  Although the addition of
   * generics and parametrically-typed collections prevents an
   * incorrect type of element being added to a collection at
   * compile-time, via static type checking, this can be overridden by
   * casting.  In contrast, wrapping the collection within a
   * dynamically-typesafe wrapper, using this and associated methods,
   * <emph>guarantees</emph> that the collection will only contain
   * elements of an appropriate type (provided it only contains such
   * at the type of wrapping, and all subsequent access is via the
   * wrapper).  This can be useful for debugging the cause of a
   * <code>ClassCastException</code> caused by erroneous casting, or
   * for protecting collections from corruption by external libraries.
   * </p>
   * <p>
   * The returned Set implements Serializable, but can only be serialized if
   * the set it wraps is likewise Serializable.
   * </p>
   *
   * @param s the set to wrap.
   * @param type the type of the elements within the checked list.
   * @return a dynamically typesafe view of the set
   * @see Serializable
   */
  public static <E> Set<E> checkedSet(Set<E> s, Class<E> type)
  {
    return new CheckedSet<E>(s, type);
  }

  /**
   * The implementation of {@link #checkedSet(Set)}. This class
   * name is required for compatibility with Sun's JDK serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class CheckedSet<E> 
    extends CheckedCollection<E>
    implements Set<E>
  {
    /**
     * Compatible with JDK 1.5.
     */
    private static final long serialVersionUID = 4694047833775013803L;

    /**
     * Wrap a given set.
     *
     * @param s the set to wrap
     * @throws NullPointerException if s is null
     */
    CheckedSet(Set<E> s, Class<E> type)
    {
      super(s, type);
    }

    /**
     * Returns <code>true</code> if the object, o, is also an instance of
     * <code>Set</code> of the same size and with the same entries.
     *
     * @return <code>true</code> if o is an equivalent set.
     */
    public boolean equals(Object o)
    {
      return c.equals(o);
    }

    /**
     * Computes the hash code of this set, as the sum of the
     * hash codes of all elements within the set.
     *
     * @return the hash code of the set.
     */ 
    public int hashCode()
    {
      return c.hashCode();
    }
  } // class CheckedSet

  /**
   * <p> 
   * Returns a dynamically typesafe view of the given sorted map,
   * where any modification is first checked to ensure that the type
   * of the new data is appropriate.  Although the addition of
   * generics and parametrically-typed collections prevents an
   * incorrect type of element being added to a collection at
   * compile-time, via static type checking, this can be overridden by
   * casting.  In contrast, wrapping the collection within a
   * dynamically-typesafe wrapper, using this and associated methods,
   * <emph>guarantees</emph> that the collection will only contain
   * elements of an appropriate type (provided it only contains such
   * at the type of wrapping, and all subsequent access is via the
   * wrapper).  This can be useful for debugging the cause of a
   * <code>ClassCastException</code> caused by erroneous casting, or
   * for protecting collections from corruption by external libraries.
   * </p>
   * <p>
   * The returned SortedMap implements Serializable, but can only be
   * serialized if the map it wraps is likewise Serializable.
   * </p>
   *
   * @param m the map to wrap.
   * @param keyType the dynamic type of the map's keys.
   * @param valueType the dynamic type of the map's values.
   * @return a dynamically typesafe view of the map
   * @see Serializable
   */
  public static <K, V> SortedMap<K, V> checkedSortedMap(SortedMap<K, V> m,
							Class<K> keyType,
							Class<V> valueType)
  {
    return new CheckedSortedMap<K, V>(m, keyType, valueType);
  }

  /**
   * The implementation of {@link #checkedSortedMap(SortedMap,Class,Class)}.
   * This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private static class CheckedSortedMap<K, V>
    extends CheckedMap<K, V>
    implements SortedMap<K, V>
  {
    /**
     * Compatible with JDK 1.5.
     */
    private static final long serialVersionUID = 1599671320688067438L;

    /**
     * The wrapped map; stored both here and in the superclass to avoid
     * excessive casting.
     * @serial the wrapped map
     */
    private final SortedMap<K, V> sm;

    /**
     * Wrap a given map.
     *
     * @param sm the map to wrap
     * @param keyType the dynamic type of the map's keys.
     * @param valueType the dynamic type of the map's values.
     * @throws NullPointerException if sm is null
     */
    CheckedSortedMap(SortedMap<K, V> sm, Class<K> keyType, Class<V> valueType)
    {
      super(sm, keyType, valueType);
      this.sm = sm;
    }

    /**
     * Returns the comparator used in sorting the underlying map,
     * or null if it is the keys' natural ordering.
     *
     * @return the sorting comparator.
     */
    public Comparator<? super K> comparator()
    {
      return sm.comparator();
    }

    /**
     * Returns the first (lowest sorted) key in the map.
     *
     * @return the first key.
     * @throws NoSuchElementException if this map is empty.
     */
    public K firstKey()
    {
      return sm.firstKey();
    }

    /**
     * <p>
     * Returns a checked view of the portion of the map strictly less
     * than toKey. The view is backed by the underlying map, so changes in
     * one show up in the other.  The submap supports all optional operations
     * of the original.  This operation is equivalent to
     * <code>subMap(firstKey(), toKey)</code>.
     * </p>
     * <p>
     * The returned map throws an IllegalArgumentException any time a key is
     * used which is out of the range of toKey. Note that the endpoint, toKey,
     * is not included; if you want this value to be included, pass its
     * successor object in to toKey.  For example, for Integers, you could
     * request <code>headMap(new Integer(limit.intValue() + 1))</code>.
     * </p>
     *
     * @param toKey the exclusive upper range of the submap.
     * @return the submap.
     * @throws ClassCastException if toKey is not comparable to the map
     *                            contents.
     * @throws IllegalArgumentException if this is a subMap, and toKey is out
     *         of range.
     * @throws NullPointerException if toKey is null but the map does not allow
     *         null keys.
     */
    public SortedMap<K, V> headMap(K toKey)
    {
      return new CheckedSortedMap<K, V>(sm.headMap(toKey), keyType, valueType);
    }

    /**
     * Returns the last (highest sorted) key in the map.
     *
     * @return the last key.
     * @throws NoSuchElementException if this map is empty.
     */
    public K lastKey()
    {
      return sm.lastKey();
    }

    /**
     * <p>
     * Returns a checked view of the portion of the map greater than or
     * equal to fromKey, and strictly less than toKey. The view is backed by
     * the underlying map, so changes in one show up in the other. The submap
     * supports all optional operations of the original.
     * </p>
     * <p>
     * The returned map throws an IllegalArgumentException any time a key is
     * used which is out of the range of fromKey and toKey. Note that the
     * lower endpoint is included, but the upper is not; if you want to
     * change the inclusion or exclusion of an endpoint, pass its successor
     * object in instead.  For example, for Integers, you could request
     * <code>subMap(new Integer(lowlimit.intValue() + 1),
     * new Integer(highlimit.intValue() + 1))</code> to reverse
     * the inclusiveness of both endpoints.
     * </p>
     *
     * @param fromKey the inclusive lower range of the submap.
     * @param toKey the exclusive upper range of the submap.
     * @return the submap.
     * @throws ClassCastException if fromKey or toKey is not comparable to
     *         the map contents.
     * @throws IllegalArgumentException if this is a subMap, and fromKey or
     *         toKey is out of range.
     * @throws NullPointerException if fromKey or toKey is null but the map
     *         does not allow null keys.
     */
    public SortedMap<K, V> subMap(K fromKey, K toKey)
    {
      return new CheckedSortedMap<K, V>(sm.subMap(fromKey, toKey), keyType, 
					valueType);
    }

    /**
     * <p>
     * Returns a checked view of the portion of the map greater than or
     * equal to fromKey. The view is backed by the underlying map, so changes
     * in one show up in the other. The submap supports all optional operations
     * of the original.
     * </p>
     * <p>
     * The returned map throws an IllegalArgumentException any time a key is
     * used which is out of the range of fromKey. Note that the endpoint,
     * fromKey, is included; if you do not want this value to be included,
     * pass its successor object in to fromKey.  For example, for Integers,
     * you could request
     * <code>tailMap(new Integer(limit.intValue() + 1))</code>.
     * </p>
     *
     * @param fromKey the inclusive lower range of the submap
     * @return the submap
     * @throws ClassCastException if fromKey is not comparable to the map
     *         contents
     * @throws IllegalArgumentException if this is a subMap, and fromKey is out
     *         of range
     * @throws NullPointerException if fromKey is null but the map does not
     *                              allow null keys
     */
    public SortedMap<K, V> tailMap(K fromKey)
    {
      return new CheckedSortedMap<K, V>(sm.tailMap(fromKey), keyType,
					valueType);
    }
  } // class CheckedSortedMap

  /**
   * <p>
   * Returns a dynamically typesafe view of the given sorted set,
   * where any modification is first checked to ensure that the type
   * of the new data is appropriate.  Although the addition of
   * generics and parametrically-typed collections prevents an
   * incorrect type of element being added to a collection at
   * compile-time, via static type checking, this can be overridden by
   * casting.  In contrast, wrapping the collection within a
   * dynamically-typesafe wrapper, using this and associated methods,
   * <emph>guarantees</emph> that the collection will only contain
   * elements of an appropriate type (provided it only contains such
   * at the type of wrapping, and all subsequent access is via the
   * wrapper).  This can be useful for debugging the cause of a
   * <code>ClassCastException</code> caused by erroneous casting, or
   * for protecting collections from corruption by external libraries.
   * </p>
   * <p>
   * The returned SortedSet implements Serializable, but can only be
   * serialized if the set it wraps is likewise Serializable.
   * </p>
   *
   * @param s the set to wrap.
   * @param type the type of the set's elements.
   * @return a dynamically typesafe view of the set
   * @see Serializable
   */
  public static <E> SortedSet<E> checkedSortedSet(SortedSet<E> s,
						  Class<E> type)
  {
    return new CheckedSortedSet<E>(s, type);
  }

  /**
   * The implementation of {@link #checkedSortedSet(SortedSet,Class)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.5
   */
  private static class CheckedSortedSet<E> 
    extends CheckedSet<E>
    implements SortedSet<E>
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1599911165492914959L;

    /**
     * The wrapped set; stored both here and in the superclass to avoid
     * excessive casting.
     * 
     * @serial the wrapped set
     */
    private SortedSet<E> ss;

    /**
     * Wrap a given set.
     *
     * @param ss the set to wrap.
     * @param type the type of the set's elements.
     * @throws NullPointerException if ss is null
     */
    CheckedSortedSet(SortedSet<E> ss, Class<E> type)
    {
      super(ss, type);
      this.ss = ss;
    }

    /**
     * Returns the comparator used in sorting the underlying set,
     * or null if it is the elements' natural ordering.
     *
     * @return the sorting comparator
     */
    public Comparator<? super E> comparator()
    {
      return ss.comparator();
    }

    /**
     * Returns the first (lowest sorted) element in the underlying
     * set.
     *
     * @return the first element.
     * @throws NoSuchElementException if the set is empty.
     */
    public E first()
    {
      return ss.first();
    }

    /**
     * <p>
     * Returns a checked view of the portion of the set strictly
     * less than toElement. The view is backed by the underlying set,
     * so changes in one show up in the other.  The subset supports
     * all optional operations of the original.  This operation
     * is equivalent to <code>subSet(first(), toElement)</code>.
     * </p>
     * <p>
     * The returned set throws an IllegalArgumentException any time an
     * element is used which is out of the range of toElement. Note that
     * the endpoint, toElement, is not included; if you want this value
     * included, pass its successor object in to toElement.  For example,
     * for Integers, you could request
     * <code>headSet(new Integer(limit.intValue() + 1))</code>.
     * </p>
     *
     * @param toElement the exclusive upper range of the subset
     * @return the subset.
     * @throws ClassCastException if toElement is not comparable to the set
     *         contents.
     * @throws IllegalArgumentException if this is a subSet, and toElement is
     *                                  out of range.
     * @throws NullPointerException if toElement is null but the set does not
     *         allow null elements.
     */
    public SortedSet<E> headSet(E toElement)
    {
      return new CheckedSortedSet<E>(ss.headSet(toElement), type);
    }

    /**
     * Returns the last (highest sorted) element in the underlying
     * set.
     *
     * @return the last element.
     * @throws NoSuchElementException if the set is empty.
     */
    public E last()
    {
      return ss.last();
    }

    /**
     * <p>
     * Returns a checked view of the portion of the set greater than or
     * equal to fromElement, and strictly less than toElement. The view is
     * backed by the underlying set, so changes in one show up in the other.
     * The subset supports all optional operations of the original.
     * </p>
     * <p>
     * The returned set throws an IllegalArgumentException any time an
     * element is used which is out of the range of fromElement and toElement.
     * Note that the lower endpoint is included, but the upper is not; if you
     * want to change the inclusion or exclusion of an endpoint, pass its
     * successor object in instead.  For example, for Integers, you can request
     * <code>subSet(new Integer(lowlimit.intValue() + 1),
     * new Integer(highlimit.intValue() + 1))</code> to reverse
     * the inclusiveness of both endpoints.
     * </p>
     * 
     * @param fromElement the inclusive lower range of the subset.
     * @param toElement the exclusive upper range of the subset.
     * @return the subset.
     * @throws ClassCastException if fromElement or toElement is not comparable
     *         to the set contents.
     * @throws IllegalArgumentException if this is a subSet, and fromElement or
     *         toElement is out of range.
     * @throws NullPointerException if fromElement or toElement is null but the
     *         set does not allow null elements.
     */
    public SortedSet<E> subSet(E fromElement, E toElement)
    {
      return new CheckedSortedSet<E>(ss.subSet(fromElement, toElement), type);
    }

    /**
     * <p>
     * Returns a checked view of the portion of the set greater than or equal
     * to fromElement. The view is backed by the underlying set, so changes in
     * one show up in the other. The subset supports all optional operations
     * of the original.
     * </p>
     * <p>
     * The returned set throws an IllegalArgumentException any time an
     * element is used which is out of the range of fromElement. Note that
     * the endpoint, fromElement, is included; if you do not want this value
     * to be included, pass its successor object in to fromElement.  For
     * example, for Integers, you could request
     * <code>tailSet(new Integer(limit.intValue() + 1))</code>.
     * </p>
     *
     * @param fromElement the inclusive lower range of the subset
     * @return the subset.
     * @throws ClassCastException if fromElement is not comparable to the set
     *         contents.
     * @throws IllegalArgumentException if this is a subSet, and fromElement is
     *         out of range.
     * @throws NullPointerException if fromElement is null but the set does not
     *         allow null elements.
     */
    public SortedSet<E> tailSet(E fromElement)
    {
      return new CheckedSortedSet<E>(ss.tailSet(fromElement), type);
    }
  } // class CheckedSortedSet

  /**
   * Returns a view of a {@link Deque} as a stack or LIFO (Last-In-First-Out)
   * {@link Queue}.  Each call to the LIFO queue corresponds to one
   * equivalent method call to the underlying deque, with the exception
   * of {@link Collection#addAll(Collection)}, which is emulated by a series
   * of {@link Deque#push(E)} calls.
   *
   * @param deque the deque to convert to a LIFO queue.
   * @return a LIFO queue.
   * @since 1.6
   */
  public static <T> Queue<T> asLifoQueue(Deque<T> deque)
  {
    return new LIFOQueue<T>(deque);
  }

  /**
   * Returns a set backed by the supplied map.  The resulting set
   * has the same performance, concurrency and ordering characteristics
   * as the original map.  The supplied map must be empty and should not
   * be used after the set is created.  Each call to the set corresponds
   * to one equivalent method call to the underlying map, with the exception
   * of {@link Set#addAll(Collection)} which is emulated by a series of
   * calls to <code>put</code>.
   *
   * @param map the map to convert to a set.
   * @return a set backed by the supplied map.
   * @throws IllegalArgumentException if the map is not empty.
   * @since 1.6
   */
  public static <E> Set<E> newSetFromMap(Map<E,Boolean> map)
  {
    return new MapSet<E>(map);
  }

  /**
   * The implementation of {@link #asLIFOQueue(Deque)}. 
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.6
   */
  private static class LIFOQueue<T>
    extends AbstractQueue<T>
  {
    
    /**
     * The backing deque.
     */
    private Deque<T> deque;

    /**
     * Constructs a new {@link LIFOQueue} with the specified
     * backing {@link Deque}.
     *
     * @param deque the backing deque.
     */
    public LIFOQueue(Deque<T> deque)
    {
      this.deque = deque;
    }

    public boolean add(T e)
    {
      return deque.offerFirst(e);
    }
    
    public boolean addAll(Collection<? extends T> c)
    {
      boolean result = false;
      final Iterator<? extends T> it = c.iterator();
      while (it.hasNext())
	result |= deque.offerFirst(it.next());
      return result;
    }
    
    public void clear()
    {
      deque.clear();
    }
    
    public boolean isEmpty()
    {
      return deque.isEmpty();
    }
    
    public Iterator<T> iterator()
    {
      return deque.iterator();
    }
    
    public boolean offer(T e)
    {
      return deque.offerFirst(e);
    }
    
    public T peek()
    {
      return deque.peek();
    }

    public T poll()
    {
      return deque.poll();
    }
    
    public int size()
    {
      return deque.size();
    }
  } // class LIFOQueue

  /**
   * The implementation of {@link #newSetFromMap(Map)}. 
   *
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   * @since 1.6
   */
  private static class MapSet<E>
    extends AbstractSet<E>
  {
    
    /**
     * The backing map.
     */
    private Map<E,Boolean> map;

    /**
     * Constructs a new {@link MapSet} using the specified
     * backing {@link Map}.
     *
     * @param map the backing map.
     * @throws IllegalArgumentException if the map is not empty.
     */
    public MapSet(Map<E,Boolean> map)
    {
      if (!map.isEmpty())
	throw new IllegalArgumentException("The map must be empty.");
      this.map = map;
    }

    public boolean add(E e)
    {
      return map.put(e, true) == null;
    }
    
    public boolean addAll(Collection<? extends E> c)
    {
      boolean result = false;
      final Iterator<? extends E> it = c.iterator();
      while (it.hasNext())
	result |= (map.put(it.next(), true) == null);
      return result;
    }
    
    public void clear()
    {
      map.clear();
    }
    
    public boolean contains(Object o)
    {
      return map.containsKey(o);
    }
    
    public boolean isEmpty()
    {
      return map.isEmpty();
    }
    
    public Iterator<E> iterator()
    {
      return map.keySet().iterator();
    }
    
    public boolean remove(Object o)
    {
      return map.remove(o) != null;
    }
    
    public int size()
    {
      return map.size();
    }
  } // class MapSet
  
} // class Collections
