/* Collections.java -- Utility class with methods to operate on collections
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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
 * <code>Collections.singleton("").addAll(Collections.EMPTY_SET)<code>
 * does not throw a exception, even though addAll is an unsupported operation
 * on a singleton; the reason for this is that addAll did not attempt to
 * modify the set.
 *
 * @author Original author unknown
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Collection
 * @see Set
 * @see List
 * @see Map
 * @see Arrays
 * @since 1.2
 * @status updated to 1.4
 */
public class Collections
{
  /**
   * Constant used to decide cutoff for when a non-RandomAccess list should
   * be treated as sequential-access. Basically, quadratic behavior is
   * acceptible for small lists when the overhead is so small in the first
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
   * @return true if it should be treated as sequential-access
   */
  private static boolean isSequential(List l)
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
   * The implementation of {@link #EMPTY_SET}. This class name is required
   * for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class EmptySet extends AbstractSet
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
     */
    public int size()
    {
      return 0;
    }

    /**
     * Returns an iterator that does not iterate.
     */
    // This is really cheating! I think it's perfectly valid, though.
    public Iterator iterator()
    {
      return EMPTY_LIST.iterator();
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractSet.
    /**
     * The empty set never contains anything.
     */
    public boolean contains(Object o)
    {
      return false;
    }

    /**
     * This is true only if the given collection is also empty.
     */
    public boolean containsAll(Collection c)
    {
      return c.isEmpty();
    }

    /**
     * Equal only if the other set is empty.
     */
    public boolean equals(Object o)
    {
      return o instanceof Set && ((Set) o).isEmpty();
    }

    /**
     * The hashcode is always 0.
     */
    public int hashCode()
    {
      return 0;
    }

    /**
     * Always succeeds with false result.
     */
    public boolean remove(Object o)
    {
      return false;
    }

    /**
     * Always succeeds with false result.
     */
    public boolean removeAll(Collection c)
    {
      return false;
    }

    /**
     * Always succeeds with false result.
     */
    public boolean retainAll(Collection c)
    {
      return false;
    }

    /**
     * The array is always empty.
     */
    public Object[] toArray()
    {
      return new Object[0];
    }

    /**
     * We don't even need to use reflection!
     */
    public Object[] toArray(Object[] a)
    {
      if (a.length > 0)
        a[0] = null;
      return a;
    }

    /**
     * The string never changes.
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
   * The implementation of {@link #EMPTY_LIST}. This class name is required
   * for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class EmptyList extends AbstractList
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
     */
    public int size()
    {
      return 0;
    }

    /**
     * No matter the index, it is out of bounds.
     */
    public Object get(int index)
    {
      throw new IndexOutOfBoundsException();
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractList.
    /**
     * Never contains anything.
     */
    public boolean contains(Object o)
    {
      return false;
    }

    /**
     * This is true only if the given collection is also empty.
     */
    public boolean containsAll(Collection c)
    {
      return c.isEmpty();
    }

    /**
     * Equal only if the other set is empty.
     */
    public boolean equals(Object o)
    {
      return o instanceof List && ((List) o).isEmpty();
    }

    /**
     * The hashcode is always 1.
     */
    public int hashCode()
    {
      return 1;
    }

    /**
     * Returns -1.
     */
    public int indexOf(Object o)
    {
      return -1;
    }

    /**
     * Returns -1.
     */
    public int lastIndexOf(Object o)
    {
      return -1;
    }

    /**
     * Always succeeds with false result.
     */
    public boolean remove(Object o)
    {
      return false;
    }

    /**
     * Always succeeds with false result.
     */
    public boolean removeAll(Collection c)
    {
      return false;
    }

    /**
     * Always succeeds with false result.
     */
    public boolean retainAll(Collection c)
    {
      return false;
    }

    /**
     * The array is always empty.
     */
    public Object[] toArray()
    {
      return new Object[0];
    }

    /**
     * We don't even need to use reflection!
     */
    public Object[] toArray(Object[] a)
    {
      if (a.length > 0)
        a[0] = null;
      return a;
    }

    /**
     * The string never changes.
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
   * The implementation of {@link #EMPTY_MAP}. This class name is required
   * for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class EmptyMap extends AbstractMap
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
     */
    public Set entrySet()
    {
      return EMPTY_SET;
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractMap.
    /**
     * No entries!
     */
    public boolean containsKey(Object key)
    {
      return false;
    }

    /**
     * No entries!
     */
    public boolean containsValue(Object value)
    {
      return false;
    }

    /**
     * Equal to all empty maps.
     */
    public boolean equals(Object o)
    {
      return o instanceof Map && ((Map) o).isEmpty();
    }

    /**
     * No mappings, so this returns null.
     */
    public Object get(Object o)
    {
      return null;
    }

    /**
     * The hashcode is always 0.
     */
    public int hashCode()
    {
      return 0;
    }

    /**
     * No entries.
     */
    public Set keySet()
    {
      return EMPTY_SET;
    }

    /**
     * Remove always succeeds, with null result.
     */
    public Object remove(Object o)
    {
      return null;
    }

    /**
     * Size is always 0.
     */
    public int size()
    {
      return 0;
    }

    /**
     * No entries. Technically, EMPTY_SET, while more specific than a general
     * Collection, will work. Besides, that's what the JDK uses!
     */
    public Collection values()
    {
      return EMPTY_SET;
    }

    /**
     * The string never changes.
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
  static final int compare(Object o1, Object o2, Comparator c)
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
  public static int binarySearch(List l, Object key)
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
  public static int binarySearch(List l, Object key, Comparator c)
  {
    int pos = 0;
    int low = 0;
    int hi = l.size() - 1;

    // We use a linear search with log(n) comparisons using an iterator
    // if the list is sequential-access.
    if (isSequential(l))
      {
	ListIterator itr = l.listIterator();
        int i = 0;
        while (low <= hi)
          {
            pos = (low + hi) >> 1;
            if (i < pos)
              for ( ; i != pos; i++, itr.next());
            else
              for ( ; i != pos; i--, itr.previous());
	    final int d = compare(key, itr.next(), c);
	    if (d == 0)
              return pos;
	    else if (d < 0)
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
	    pos = (low + hi) >> 1;
	    final int d = compare(key, l.get(pos), c);
	    if (d == 0)
              return pos;
	    else if (d < 0)
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
  public static void copy(List dest, List source)
  {
    int pos = source.size();
    if (dest.size() < pos)
      throw new IndexOutOfBoundsException("Source does not fit in dest");

    Iterator i1 = source.iterator();
    ListIterator i2 = dest.listIterator();

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
  public static Enumeration enumeration(Collection c)
  {
    final Iterator i = c.iterator();
    return new Enumeration()
    {
      public final boolean hasMoreElements()
      {
	return i.hasNext();
      }
      public final Object nextElement()
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
  public static void fill(List l, Object val)
  {
    ListIterator itr = l.listIterator();
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
  public static int indexOfSubList(List source, List target)
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
  public static int lastIndexOfSubList(List source, List target)
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
  public static ArrayList list(Enumeration e)
  {
    ArrayList l = new ArrayList();
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
  public static Object max(Collection c)
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
  public static Object max(Collection c, Comparator order)
  {
    Iterator itr = c.iterator();
    Object max = itr.next(); // throws NoSuchElementException
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	Object o = itr.next();
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
  public static Object min(Collection c)
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
  public static Object min(Collection c, Comparator order)
  {
    Iterator itr = c.iterator();
    Object min = itr.next();	// throws NoSuchElementExcception
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	Object o = itr.next();
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
  public static List nCopies(final int n, final Object o)
  {
    return new CopiesList(n, o);
  }

  /**
   * The implementation of {@link #nCopies(int, Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class CopiesList extends AbstractList
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
    private final Object element;

    /**
     * Constructs the list.
     *
     * @param n the count
     * @param o the object
     * @throws IllegalArgumentException if n &lt; 0
     */
    CopiesList(int n, Object o)
    {
      if (n < 0)
	throw new IllegalArgumentException();
      this.n = n;
      element = o;
    }

    /**
     * The size is fixed.
     */
    public int size()
    {
      return n;
    }

    /**
     * The same element is returned.
     */
    public Object get(int index)
    {
      if (index < 0 || index >= n)
        throw new IndexOutOfBoundsException();
      return element;
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractList.
    /**
     * This list only contains one element.
     */
    public boolean contains(Object o)
    {
      return n > 0 && equals(o, element);
    }

    /**
     * The index is either 0 or -1.
     */
    public int indexOf(Object o)
    {
      return (n > 0 && equals(o, element)) ? 0 : -1;
    }

    /**
     * The index is either n-1 or -1.
     */
    public int lastIndexOf(Object o)
    {
      return equals(o, element) ? n - 1 : -1;
    }

    /**
     * A subList is just another CopiesList.
     */
    public List subList(int from, int to)
    {
      if (from < 0 || to > n)
        throw new IndexOutOfBoundsException();
      return new CopiesList(to - from, element);
    }

    /**
     * The array is easy.
     */
    public Object[] toArray()
    {
      Object[] a = new Object[n];
      Arrays.fill(a, element);
      return a;
    }

    /**
     * The string is easy to generate.
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
   * @return true if a replacement occurred
   * @throws UnsupportedOperationException if the list iterator does not allow
   *         for the set operation
   * @throws ClassCastException newval is of a type which cannot be added
   *         to the list
   * @throws IllegalArgumentException some other aspect of newval stops
   *         it being added to the list
   * @since 1.4
   */
  public static boolean replaceAll(List list, Object oldval, Object newval)
  {
    ListIterator itr = list.listIterator();
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
  public static void reverse(List l)
  {
    ListIterator i1 = l.listIterator();
    int pos1 = 1;
    int pos2 = l.size();
    ListIterator i2 = l.listIterator(pos2);
    while (pos1 < pos2)
      {
	Object o = i1.next();
	i1.set(i2.previous());
	i2.set(o);
	++pos1;
	--pos2;
      }
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
  public static Comparator reverseOrder()
  {
    return rcInstance;
  }

  /**
   * The object for {@link #reverseOrder()}.
   */
  static private final ReverseComparator rcInstance = new ReverseComparator();

  /**
   * The implementation of {@link #reverseOrder()}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class ReverseComparator
    implements Comparator, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    static private final long serialVersionUID = 7207038068494060240L;

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
    public int compare(Object a, Object b)
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
  public static void rotate(List list, int distance)
  {
    int size = list.size();
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
        while (--lcm >= 0)
          {
            Object o = list.get(lcm);
            for (int i = lcm + distance; i != lcm; i = (i + distance) % size)
              o = list.set(i, o);
            list.set(lcm, o);
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
  public static void shuffle(List l)
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
  public static void shuffle(List l, Random r)
  {
    int lsize = l.size();
    ListIterator i = l.listIterator(lsize);
    boolean sequential = isSequential(l);
    Object[] a = null; // stores a copy of the list for the sequential case

    if (sequential)
      a = l.toArray();

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
          o = l.set(swap, i.previous());

	i.set(o);
      }
  }


  /**
   * Obtain an immutable Set consisting of a single element. The return value
   * of this method is Serializable.
   *
   * @param o the single element
   * @return an immutable Set containing only o
   * @see Serializable
   */
  public static Set singleton(Object o)
  {
    return new SingletonSet(o);
  }

  /**
   * The implementation of {@link #singleton(Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SingletonSet extends AbstractSet
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
    final Object element;

    /**
     * Construct a singleton.
     * @param o the element
     */
    SingletonSet(Object o)
    {
      element = o;
    }

    /**
     * The size: always 1!
     */
    public int size()
    {
      return 1;
    }

    /**
     * Returns an iterator over the lone element.
     */
    public Iterator iterator()
    {
      return new Iterator()
      {
        private boolean hasNext = true;

        public boolean hasNext()
        {
          return hasNext;
        }

        public Object next()
        {
          if (hasNext)
          {
            hasNext = false;
            return element;
          }
          else
            throw new NoSuchElementException();
        }

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
     */
    public boolean contains(Object o)
    {
      return equals(o, element);
    }

    /**
     * This is true if the other collection only contains the element.
     */
    public boolean containsAll(Collection c)
    {
      Iterator i = c.iterator();
      int pos = c.size();
      while (--pos >= 0)
        if (! equals(i.next(), element))
          return false;
      return true;
    }

    /**
     * The hash is just that of the element.
     */
    public int hashCode()
    {
      return hashCode(element);
    }

    /**
     * Returning an array is simple.
     */
    public Object[] toArray()
    {
      return new Object[] {element};
    }

    /**
     * Obvious string.
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
  public static List singletonList(Object o)
  {
    return new SingletonList(o);
  }

  /**
   * The implementation of {@link #singletonList(Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SingletonList extends AbstractList
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
    private final Object element;

    /**
     * Construct a singleton.
     * @param o the element
     */
    SingletonList(Object o)
    {
      element = o;
    }

    /**
     * The size: always 1!
     */
    public int size()
    {
      return 1;
    }

    /**
     * Only index 0 is valid.
     */
    public Object get(int index)
    {
      if (index == 0)
        return element;
      throw new IndexOutOfBoundsException();
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractList.
    /**
     * The set only contains one element.
     */
    public boolean contains(Object o)
    {
      return equals(o, element);
    }

    /**
     * This is true if the other collection only contains the element.
     */
    public boolean containsAll(Collection c)
    {
      Iterator i = c.iterator();
      int pos = c.size();
      while (--pos >= 0)
        if (! equals(i.next(), element))
          return false;
      return true;
    }

    /**
     * Speed up the hashcode computation.
     */
    public int hashCode()
    {
      return 31 + hashCode(element);
    }

    /**
     * Either the list has it or not.
     */
    public int indexOf(Object o)
    {
      return equals(o, element) ? 0 : -1;
    }

    /**
     * Either the list has it or not.
     */
    public int lastIndexOf(Object o)
    {
      return equals(o, element) ? 0 : -1;
    }

    /**
     * Sublists are limited in scope.
     */
    public List subList(int from, int to)
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
     */
    public Object[] toArray()
    {
      return new Object[] {element};
    }

    /**
     * Obvious string.
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
  public static Map singletonMap(Object key, Object value)
  {
    return new SingletonMap(key, value);
  }

  /**
   * The implementation of {@link #singletonMap(Object)}. This class name
   * is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SingletonMap extends AbstractMap
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
    private final Object k;

    /**
     * The corresponding value.
     * @serial the singleton value
     */
    private final Object v;

    /**
     * Cache the entry set.
     */
    private transient Set entries;

    /**
     * Construct a singleton.
     * @param key the key
     * @param value the value
     */
    SingletonMap(Object key, Object value)
    {
      k = key;
      v = value;
    }

    /**
     * There is a single immutable entry.
     */
    public Set entrySet()
    {
      if (entries == null)
        entries = singleton(new AbstractMap.BasicMapEntry(k, v)
        {
          public Object setValue(Object o)
          {
            throw new UnsupportedOperationException();
          }
        });
      return entries;
    }

    // The remaining methods are optional, but provide a performance
    // advantage by not allocating unnecessary iterators in AbstractMap.
    /**
     * Single entry.
     */
    public boolean containsKey(Object key)
    {
      return equals(key, k);
    }

    /**
     * Single entry.
     */
    public boolean containsValue(Object value)
    {
      return equals(value, v);
    }

    /**
     * Single entry.
     */
    public Object get(Object key)
    {
      return equals(key, k) ? v : null;
    }

    /**
     * Calculate the hashcode directly.
     */
    public int hashCode()
    {
      return hashCode(k) ^ hashCode(v);
    }

    /**
     * Return the keyset.
     */
    public Set keySet()
    {
      if (keys == null)
        keys = singleton(k);
      return keys;
    }

    /**
     * The size: always 1!
     */
    public int size()
    {
      return 1;
    }

    /**
     * Return the values. Technically, a singleton, while more specific than
     * a general Collection, will work. Besides, that's what the JDK uses!
     */
    public Collection values()
    {
      if (values == null)
        values = singleton(v);
      return values;
    }

    /**
     * Obvious string.
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
   * @param l the List to sort
   * @throws ClassCastException if some items are not mutually comparable
   * @throws UnsupportedOperationException if the List is not modifiable
   * @throws NullPointerException if some element is null
   * @see Arrays#sort(Object[])
   */
  public static void sort(List l)
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
   * @param l the List to sort
   * @param c the Comparator specifying the ordering for the elements, or
   *        null for natural ordering
   * @throws ClassCastException if c will not compare some pair of items
   * @throws UnsupportedOperationException if the List is not modifiable
   * @throws NullPointerException if null is compared by natural ordering
   *        (only possible when c is null)
   * @see Arrays#sort(Object[], Comparator)
   */
  public static void sort(List l, Comparator c)
  {
    Object[] a = l.toArray();
    Arrays.sort(a, c);
    ListIterator i = l.listIterator(a.length);
    for (int pos = a.length; --pos >= 0; )
      {
	i.previous();
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
  public static void swap(List l, int i, int j)
  {
    l.set(i, l.set(j, l.get(i)));
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
  public static Collection synchronizedCollection(Collection c)
  {
    return new SynchronizedCollection(c);
  }

  /**
   * The implementation of {@link #synchronizedCollection(Collection)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   * Package visible, so that collections such as the one for
   * Hashtable.values() can specify which object to synchronize on.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  static class SynchronizedCollection
    implements Collection, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 3053995032091335093L;

    /**
     * The wrapped collection. Package visible for use by subclasses.
     * @serial the real collection
     */
    final Collection c;

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
    SynchronizedCollection(Collection c)
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
    SynchronizedCollection(Object sync, Collection c)
    {
      this.c = c;
      mutex = sync;
    }

    public boolean add(Object o)
    {
      synchronized (mutex)
        {
          return c.add(o);
        }
    }

    public boolean addAll(Collection col)
    {
      synchronized (mutex)
        {
          return c.addAll(col);
        }
    }

    public void clear()
    {
      synchronized (mutex)
        {
          c.clear();
        }
    }

    public boolean contains(Object o)
    {
      synchronized (mutex)
        {
          return c.contains(o);
        }
    }

    public boolean containsAll(Collection c1)
    {
      synchronized (mutex)
        {
          return c.containsAll(c1);
        }
    }

    public boolean isEmpty()
    {
      synchronized (mutex)
        {
          return c.isEmpty();
        }
    }

    public Iterator iterator()
    {
      synchronized (mutex)
        {
          return new SynchronizedIterator(mutex, c.iterator());
        }
    }

    public boolean remove(Object o)
    {
      synchronized (mutex)
        {
          return c.remove(o);
        }
    }

    public boolean removeAll(Collection col)
    {
      synchronized (mutex)
        {
          return c.removeAll(col);
        }
    }

    public boolean retainAll(Collection col)
    {
      synchronized (mutex)
        {
          return c.retainAll(col);
        }
    }

    public int size()
    {
      synchronized (mutex)
        {
          return c.size();
        }
    }

    public Object[] toArray()
    {
      synchronized (mutex)
        {
          return c.toArray();
        }
    }

    public Object[] toArray(Object[] a)
    {
      synchronized (mutex)
        {
          return c.toArray(a);
        }
    }

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
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class SynchronizedIterator implements Iterator
  {
    /**
     * The object to synchronize on. Package visible for use by subclass.
     */
    final Object mutex;

    /**
     * The wrapped iterator.
     */
    private final Iterator i;

    /**
     * Only trusted code creates a wrapper, with the specified sync.
     * @param sync the mutex
     * @param i the wrapped iterator
     */
    SynchronizedIterator(Object sync, Iterator i)
    {
      this.i = i;
      mutex = sync;
    }

    public Object next()
    {
      synchronized (mutex)
        {
          return i.next();
        }
    }

    public boolean hasNext()
    {
      synchronized (mutex)
        {
          return i.hasNext();
        }
    }

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
  public static List synchronizedList(List l)
  {
    if (l instanceof RandomAccess)
      return new SynchronizedRandomAccessList(l);
    return new SynchronizedList(l);
  }

  /**
   * The implementation of {@link #synchronizedList(List)} for sequential
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability. Package visible, so that lists such as Vector.subList()
   * can specify which object to synchronize on.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  static class SynchronizedList extends SynchronizedCollection
    implements List
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
    final List list;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @throws NullPointerException if l is null
     */
    SynchronizedList(List l)
    {
      super(l);
      list = l;
    }

    /**
     * Called only by trusted code to specify the mutex as well as the list.
     * @param sync the mutex
     * @param l the list
     */
    SynchronizedList(Object sync, List l)
    {
      super(sync, l);
      list = l;
    }

    public void add(int index, Object o)
    {
      synchronized (mutex)
        {
          list.add(index, o);
        }
    }

    public boolean addAll(int index, Collection c)
    {
      synchronized (mutex)
        {
          return list.addAll(index, c);
        }
    }

    public boolean equals(Object o)
    {
      synchronized (mutex)
        {
          return list.equals(o);
        }
    }

    public Object get(int index)
    {
      synchronized (mutex)
        {
          return list.get(index);
        }
    }

    public int hashCode()
    {
      synchronized (mutex)
        {
          return list.hashCode();
        }
    }

    public int indexOf(Object o)
    {
      synchronized (mutex)
        {
          return list.indexOf(o);
        }
    }

    public int lastIndexOf(Object o)
    {
      synchronized (mutex)
        {
          return list.lastIndexOf(o);
        }
    }

    public ListIterator listIterator()
    {
      synchronized (mutex)
        {
          return new SynchronizedListIterator(mutex, list.listIterator());
        }
    }

    public ListIterator listIterator(int index)
    {
      synchronized (mutex)
        {
          return new SynchronizedListIterator(mutex, list.listIterator(index));
        }
    }

    public Object remove(int index)
    {
      synchronized (mutex)
        {
          return list.remove(index);
        }
    }

    public Object set(int index, Object o)
    {
      synchronized (mutex)
        {
          return list.set(index, o);
        }
    }

    public List subList(int fromIndex, int toIndex)
    {
      synchronized (mutex)
        {
          return new SynchronizedList(mutex, list.subList(fromIndex, toIndex));
        }
    }
  } // class SynchronizedList

  /**
   * The implementation of {@link #synchronizedList(List)} for random-access
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SynchronizedRandomAccessList
    extends SynchronizedList implements RandomAccess
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
    SynchronizedRandomAccessList(List l)
    {
      super(l);
    }

    /**
     * Called only by trusted code to specify the mutex as well as the
     * collection.
     * @param sync the mutex
     * @param l the list
     */
    SynchronizedRandomAccessList(Object sync, List l)
    {
      super(sync, l);
    }

    public List subList(int fromIndex, int toIndex)
    {
      synchronized (mutex)
        {
          return new SynchronizedRandomAccessList(mutex,
                                                  list.subList(fromIndex,
                                                               toIndex));
        }
    }
  } // class SynchronizedRandomAccessList

  /**
   * The implementation of {@link SynchronizedList#listIterator()}. This
   * iterator must "sync" on the same object as the list it iterates over.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SynchronizedListIterator
    extends SynchronizedIterator implements ListIterator
  {
    /**
     * The wrapped iterator, stored both here and in the superclass to
     * avoid excessive casting.
     */
    private final ListIterator li;

    /**
     * Only trusted code creates a wrapper, with the specified sync.
     * @param sync the mutex
     * @param li the wrapped iterator
     */
    SynchronizedListIterator(Object sync, ListIterator li)
    {
      super(sync, li);
      this.li = li;
    }

    public void add(Object o)
    {
      synchronized (mutex)
        {
          li.add(o);
        }
    }
    public boolean hasPrevious()
    {
      synchronized (mutex)
        {
          return li.hasPrevious();
        }
    }

    public int nextIndex()
    {
      synchronized (mutex)
        {
          return li.nextIndex();
        }
    }

    public Object previous()
    {
      synchronized (mutex)
        {
          return li.previous();
        }
    }

    public int previousIndex()
    {
      synchronized (mutex)
        {
          return li.previousIndex();
        }
    }

    public void set(Object o)
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
  public static Map synchronizedMap(Map m)
  {
    return new SynchronizedMap(m);
  }

  /**
   * The implementation of {@link #synchronizedMap(Map)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class SynchronizedMap implements Map, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1978198479659022715L;

    /**
     * The wrapped map.
     * @serial the real map
     */
    private final Map m;

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
    private transient Set entries;

    /**
     * Cache the key set.
     */
    private transient Set keys;

    /**
     * Cache the value collection.
     */
    private transient Collection values;

    /**
     * Wrap a given map.
     * @param m the map to wrap
     * @throws NullPointerException if m is null
     */
    SynchronizedMap(Map m)
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
    SynchronizedMap(Object sync, Map m)
    {
      this.m = m;
      mutex = sync;
    }

    public void clear()
    {
      synchronized (mutex)
        {
          m.clear();
        }
    }

    public boolean containsKey(Object key)
    {
      synchronized (mutex)
        {
          return m.containsKey(key);
        }
    }

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
    public Set entrySet()
    {
      // Define this here to spare some nesting.
      class SynchronizedMapEntry implements Map.Entry
      {
        final Map.Entry e;
        SynchronizedMapEntry(Object o)
        {
          e = (Map.Entry) o;
        }
        public boolean equals(Object o)
        {
          synchronized (mutex)
            {
              return e.equals(o);
            }
        }
        public Object getKey()
        {
          synchronized (mutex)
            {
              return e.getKey();
            }
        }
        public Object getValue()
        {
          synchronized (mutex)
            {
              return e.getValue();
            }
        }
        public int hashCode()
        {
          synchronized (mutex)
            {
              return e.hashCode();
            }
        }
        public Object setValue(Object value)
        {
          synchronized (mutex)
            {
              return e.setValue(value);
            }
        }
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
            entries = new SynchronizedSet(mutex, m.entrySet())
            {
              public Iterator iterator()
              {
                synchronized (super.mutex)
                  {
                    return new SynchronizedIterator(super.mutex, c.iterator())
                    {
                      public Object next()
                      {
                        synchronized (super.mutex)
                          {
                            return new SynchronizedMapEntry(super.next());
                          }
                      }
                    };
                  }
              }
            };
          }
      return entries;
    }

    public boolean equals(Object o)
    {
      synchronized (mutex)
        {
          return m.equals(o);
        }
    }

    public Object get(Object key)
    {
      synchronized (mutex)
        {
          return m.get(key);
        }
    }

    public int hashCode()
    {
      synchronized (mutex)
        {
          return m.hashCode();
        }
    }

    public boolean isEmpty()
    {
      synchronized (mutex)
        {
          return m.isEmpty();
        }
    }

    public Set keySet()
    {
      if (keys == null)
        synchronized (mutex)
          {
            keys = new SynchronizedSet(mutex, m.keySet());
          }
      return keys;
    }

    public Object put(Object key, Object value)
    {
      synchronized (mutex)
        {
          return m.put(key, value);
        }
    }

    public void putAll(Map map)
    {
      synchronized (mutex)
        {
          m.putAll(map);
        }
    }

    public Object remove(Object o)
    {
      synchronized (mutex)
        {
          return m.remove(o);
        }
    }

    public int size()
    {
      synchronized (mutex)
        {
          return m.size();
        }
    }

    public String toString()
    {
      synchronized (mutex)
        {
          return m.toString();
        }
    }

    public Collection values()
    {
      if (values == null)
        synchronized (mutex)
          {
            values = new SynchronizedCollection(mutex, m.values());
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
  public static Set synchronizedSet(Set s)
  {
    return new SynchronizedSet(s);
  }

  /**
   * The implementation of {@link #synchronizedSet(Set)}. This class
   * name is required for compatibility with Sun's JDK serializability.
   * Package visible, so that sets such as Hashtable.keySet()
   * can specify which object to synchronize on.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  static class SynchronizedSet extends SynchronizedCollection
    implements Set
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
    SynchronizedSet(Set s)
    {
      super(s);
    }

    /**
     * Called only by trusted code to specify the mutex as well as the set.
     * @param sync the mutex
     * @param s the set
     */
    SynchronizedSet(Object sync, Set s)
    {
      super(sync, s);
    }

    public boolean equals(Object o)
    {
      synchronized (mutex)
        {
          return c.equals(o);
        }
    }

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
  public static SortedMap synchronizedSortedMap(SortedMap m)
  {
    return new SynchronizedSortedMap(m);
  }

  /**
   * The implementation of {@link #synchronizedSortedMap(SortedMap)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SynchronizedSortedMap extends SynchronizedMap
    implements SortedMap
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
    private final SortedMap sm;

    /**
     * Wrap a given map.
     * @param sm the map to wrap
     * @throws NullPointerException if sm is null
     */
    SynchronizedSortedMap(SortedMap sm)
    {
      super(sm);
      this.sm = sm;
    }

    /**
     * Called only by trusted code to specify the mutex as well as the map.
     * @param sync the mutex
     * @param sm the map
     */
    SynchronizedSortedMap(Object sync, SortedMap sm)
    {
      super(sync, sm);
      this.sm = sm;
    }

    public Comparator comparator()
    {
      synchronized (mutex)
        {
          return sm.comparator();
        }
    }

    public Object firstKey()
    {
      synchronized (mutex)
        {
          return sm.firstKey();
        }
    }

    public SortedMap headMap(Object toKey)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedMap(mutex, sm.headMap(toKey));
        }
    }

    public Object lastKey()
    {
      synchronized (mutex)
        {
          return sm.lastKey();
        }
    }

    public SortedMap subMap(Object fromKey, Object toKey)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedMap(mutex, sm.subMap(fromKey, toKey));
        }
    }

    public SortedMap tailMap(Object fromKey)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedMap(mutex, sm.tailMap(fromKey));
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
  public static SortedSet synchronizedSortedSet(SortedSet s)
  {
    return new SynchronizedSortedSet(s);
  }

  /**
   * The implementation of {@link #synchronizedSortedSet(SortedSet)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class SynchronizedSortedSet extends SynchronizedSet
    implements SortedSet
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
    private final SortedSet ss;

    /**
     * Wrap a given set.
     * @param ss the set to wrap
     * @throws NullPointerException if ss is null
     */
    SynchronizedSortedSet(SortedSet ss)
    {
      super(ss);
      this.ss = ss;
    }

    /**
     * Called only by trusted code to specify the mutex as well as the set.
     * @param sync the mutex
     * @param l the list
     */
    SynchronizedSortedSet(Object sync, SortedSet ss)
    {
      super(sync, ss);
      this.ss = ss;
    }

    public Comparator comparator()
    {
      synchronized (mutex)
        {
          return ss.comparator();
        }
    }

    public Object first()
    {
      synchronized (mutex)
        {
          return ss.first();
        }
    }

    public SortedSet headSet(Object toElement)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedSet(mutex, ss.headSet(toElement));
        }
    }

    public Object last()
    {
      synchronized (mutex)
        {
          return ss.last();
        }
    }

    public SortedSet subSet(Object fromElement, Object toElement)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedSet(mutex,
                                           ss.subSet(fromElement, toElement));
        }
    }

    public SortedSet tailSet(Object fromElement)
    {
      synchronized (mutex)
        {
          return new SynchronizedSortedSet(mutex, ss.tailSet(fromElement));
        }
    }
  } // class SynchronizedSortedSet


  /**
   * Returns an unmodifiable view of the given collection. This allows
   * "read-only" access, although changes in the backing collection show up
   * in this view. Attempts to modify the collection directly or via iterators
   * will fail with {@link UnsupportedOperationException}.
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
  public static Collection unmodifiableCollection(Collection c)
  {
    return new UnmodifiableCollection(c);
  }

  /**
   * The implementation of {@link #unmodifiableCollection(Collection)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableCollection
    implements Collection, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = 1820017752578914078L;

    /**
     * The wrapped collection. Package visible for use by subclasses.
     * @serial the real collection
     */
    final Collection c;

    /**
     * Wrap a given collection.
     * @param c the collection to wrap
     * @throws NullPointerException if c is null
     */
    UnmodifiableCollection(Collection c)
    {
      this.c = c;
      if (c == null)
        throw new NullPointerException();
    }

    public boolean add(Object o)
    {
      throw new UnsupportedOperationException();
    }

    public boolean addAll(Collection c)
    {
      throw new UnsupportedOperationException();
    }

    public void clear()
    {
      throw new UnsupportedOperationException();
    }

    public boolean contains(Object o)
    {
      return c.contains(o);
    }

    public boolean containsAll(Collection c1)
    {
      return c.containsAll(c1);
    }

    public boolean isEmpty()
    {
      return c.isEmpty();
    }

    public Iterator iterator()
    {
      return new UnmodifiableIterator(c.iterator());
    }

    public boolean remove(Object o)
    {
      throw new UnsupportedOperationException();
    }

    public boolean removeAll(Collection c)
    {
      throw new UnsupportedOperationException();
    }

    public boolean retainAll(Collection c)
    {
      throw new UnsupportedOperationException();
    }

    public int size()
    {
      return c.size();
    }

    public Object[] toArray()
    {
      return c.toArray();
    }

    public Object[] toArray(Object[] a)
    {
      return c.toArray(a);
    }

    public String toString()
    {
      return c.toString();
    }
  } // class UnmodifiableCollection

  /**
   * The implementation of the various iterator methods in the
   * unmodifiable classes.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableIterator implements Iterator
  {
    /**
     * The wrapped iterator.
     */
    private final Iterator i;

    /**
     * Only trusted code creates a wrapper.
     * @param i the wrapped iterator
     */
    UnmodifiableIterator(Iterator i)
    {
      this.i = i;
    }

    public Object next()
    {
      return i.next();
    }

    public boolean hasNext()
    {
      return i.hasNext();
    }

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
  public static List unmodifiableList(List l)
  {
    if (l instanceof RandomAccess)
      return new UnmodifiableRandomAccessList(l);
    return new UnmodifiableList(l);
  }

  /**
   * The implementation of {@link #unmodifiableList(List)} for sequential
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableList extends UnmodifiableCollection
    implements List
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
    final List list;

    /**
     * Wrap a given list.
     * @param l the list to wrap
     * @throws NullPointerException if l is null
     */
    UnmodifiableList(List l)
    {
      super(l);
      list = l;
    }

    public void add(int index, Object o)
    {
      throw new UnsupportedOperationException();
    }

    public boolean addAll(int index, Collection c)
    {
      throw new UnsupportedOperationException();
    }

    public boolean equals(Object o)
    {
      return list.equals(o);
    }

    public Object get(int index)
    {
      return list.get(index);
    }

    public int hashCode()
    {
      return list.hashCode();
    }

    public int indexOf(Object o)
    {
      return list.indexOf(o);
    }

    public int lastIndexOf(Object o)
    {
      return list.lastIndexOf(o);
    }

    public ListIterator listIterator()
    {
      return new UnmodifiableListIterator(list.listIterator());
    }

    public ListIterator listIterator(int index)
    {
      return new UnmodifiableListIterator(list.listIterator(index));
    }

    public Object remove(int index)
    {
      throw new UnsupportedOperationException();
    }

    public Object set(int index, Object o)
    {
      throw new UnsupportedOperationException();
    }

    public List subList(int fromIndex, int toIndex)
    {
      return unmodifiableList(list.subList(fromIndex, toIndex));
    }
  } // class UnmodifiableList

  /**
   * The implementation of {@link #unmodifiableList(List)} for random-access
   * lists. This class name is required for compatibility with Sun's JDK
   * serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class UnmodifiableRandomAccessList
    extends UnmodifiableList implements RandomAccess
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
    UnmodifiableRandomAccessList(List l)
    {
      super(l);
    }
  } // class UnmodifiableRandomAccessList

  /**
   * The implementation of {@link UnmodifiableList#listIterator()}.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static final class UnmodifiableListIterator
    extends UnmodifiableIterator implements ListIterator
  {
    /**
     * The wrapped iterator, stored both here and in the superclass to
     * avoid excessive casting.
     */
    private final ListIterator li;

    /**
     * Only trusted code creates a wrapper.
     * @param li the wrapped iterator
     */
    UnmodifiableListIterator(ListIterator li)
    {
      super(li);
      this.li = li;
    }

    public void add(Object o)
    {
      throw new UnsupportedOperationException();
    }

    public boolean hasPrevious()
    {
      return li.hasPrevious();
    }

    public int nextIndex()
    {
      return li.nextIndex();
    }

    public Object previous()
    {
      return li.previous();
    }

    public int previousIndex()
    {
      return li.previousIndex();
    }

    public void set(Object o)
    {
      throw new UnsupportedOperationException();
    }
  } // class UnmodifiableListIterator

  /**
   * Returns an unmodifiable view of the given map. This allows "read-only"
   * access, although changes in the backing map show up in this view.
   * Attempts to modify the map directly, or via collection views or their
   * iterators will fail with {@link UnsupportedOperationException}.
   * <p>
   *
   * The returned Map implements Serializable, but can only be serialized if
   * the map it wraps is likewise Serializable.
   *
   * @param m the map to wrap
   * @return a read-only view of the map
   * @see Serializable
   */
  public static Map unmodifiableMap(Map m)
  {
    return new UnmodifiableMap(m);
  }

  /**
   * The implementation of {@link #unmodifiableMap(Map)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableMap implements Map, Serializable
  {
    /**
     * Compatible with JDK 1.4.
     */
    private static final long serialVersionUID = -1034234728574286014L;

    /**
     * The wrapped map.
     * @serial the real map
     */
    private final Map m;

    /**
     * Cache the entry set.
     */
    private transient Set entries;

    /**
     * Cache the key set.
     */
    private transient Set keys;

    /**
     * Cache the value collection.
     */
    private transient Collection values;

    /**
     * Wrap a given map.
     * @param m the map to wrap
     * @throws NullPointerException if m is null
     */
    UnmodifiableMap(Map m)
    {
      this.m = m;
      if (m == null)
        throw new NullPointerException();
    }

    public void clear()
    {
      throw new UnsupportedOperationException();
    }

    public boolean containsKey(Object key)
    {
      return m.containsKey(key);
    }

    public boolean containsValue(Object value)
    {
      return m.containsValue(value);
    }

    public Set entrySet()
    {
      if (entries == null)
        entries = new UnmodifiableEntrySet(m.entrySet());
      return entries;
    }

    /**
     * The implementation of {@link UnmodifiableMap#entrySet()}. This class
     * name is required for compatibility with Sun's JDK serializability.
     *
     * @author Eric Blake <ebb9@email.byu.edu>
     */
    private static final class UnmodifiableEntrySet extends UnmodifiableSet
      implements Serializable
    {
      /**
       * Compatible with JDK 1.4.
       */
      private static final long serialVersionUID = 7854390611657943733L;

      /**
       * Wrap a given set.
       * @param s the set to wrap
       */
      UnmodifiableEntrySet(Set s)
      {
        super(s);
      }

      // The iterator must return unmodifiable map entries.
      public Iterator iterator()
      {
        return new UnmodifiableIterator(c.iterator())
	{
          public Object next()
          {
            final Map.Entry e = (Map.Entry) super.next();
            return new Map.Entry()
	    {
              public boolean equals(Object o)
              {
                return e.equals(o);
              }
              public Object getKey()
              {
                return e.getKey();
              }
              public Object getValue()
              {
                return e.getValue();
              }
              public int hashCode()
              {
                return e.hashCode();
              }
              public Object setValue(Object value)
              {
                throw new UnsupportedOperationException();
              }
              public String toString()
              {
                return e.toString();
              }
	    };
          }
	};
      }
    } // class UnmodifiableEntrySet

    public boolean equals(Object o)
    {
      return m.equals(o);
    }

    public Object get(Object key)
    {
      return m.get(key);
    }

    public Object put(Object key, Object value)
    {
      throw new UnsupportedOperationException();
    }

    public int hashCode()
    {
      return m.hashCode();
    }

    public boolean isEmpty()
    {
      return m.isEmpty();
    }

    public Set keySet()
    {
      if (keys == null)
        keys = new UnmodifiableSet(m.keySet());
      return keys;
    }

    public void putAll(Map m)
    {
      throw new UnsupportedOperationException();
    }

    public Object remove(Object o)
    {
      throw new UnsupportedOperationException();
    }

    public int size()
    {
      return m.size();
    }

    public String toString()
    {
      return m.toString();
    }

    public Collection values()
    {
      if (values == null)
        values = new UnmodifiableCollection(m.values());
      return values;
    }
  } // class UnmodifiableMap

  /**
   * Returns an unmodifiable view of the given set. This allows
   * "read-only" access, although changes in the backing set show up
   * in this view. Attempts to modify the set directly or via iterators
   * will fail with {@link UnsupportedOperationException}.
   * <p>
   *
   * The returned Set implements Serializable, but can only be serialized if
   * the set it wraps is likewise Serializable.
   *
   * @param s the set to wrap
   * @return a read-only view of the set
   * @see Serializable
   */
  public static Set unmodifiableSet(Set s)
  {
    return new UnmodifiableSet(s);
  }

  /**
   * The implementation of {@link #unmodifiableSet(Set)}. This class
   * name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableSet extends UnmodifiableCollection
    implements Set
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
    UnmodifiableSet(Set s)
    {
      super(s);
    }

    public boolean equals(Object o)
    {
      return c.equals(o);
    }

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
   * <p>
   *
   * The returned SortedMap implements Serializable, but can only be
   * serialized if the map it wraps is likewise Serializable.
   *
   * @param m the map to wrap
   * @return a read-only view of the map
   * @see Serializable
   */
  public static SortedMap unmodifiableSortedMap(SortedMap m)
  {
    return new UnmodifiableSortedMap(m);
  }

  /**
   * The implementation of {@link #unmodifiableSortedMap(SortedMap)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableSortedMap extends UnmodifiableMap
    implements SortedMap
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
    private final SortedMap sm;

    /**
     * Wrap a given map.
     * @param sm the map to wrap
     * @throws NullPointerException if sm is null
     */
    UnmodifiableSortedMap(SortedMap sm)
    {
      super(sm);
      this.sm = sm;
    }

    public Comparator comparator()
    {
      return sm.comparator();
    }

    public Object firstKey()
    {
      return sm.firstKey();
    }

    public SortedMap headMap(Object toKey)
    {
      return new UnmodifiableSortedMap(sm.headMap(toKey));
    }

    public Object lastKey()
    {
      return sm.lastKey();
    }

    public SortedMap subMap(Object fromKey, Object toKey)
    {
      return new UnmodifiableSortedMap(sm.subMap(fromKey, toKey));
    }

    public SortedMap tailMap(Object fromKey)
    {
      return new UnmodifiableSortedMap(sm.tailMap(fromKey));
    }
  } // class UnmodifiableSortedMap

  /**
   * Returns an unmodifiable view of the given sorted set. This allows
   * "read-only" access, although changes in the backing set show up
   * in this view. Attempts to modify the set directly, via subsets, or via
   * iterators, will fail with {@link UnsupportedOperationException}.
   * <p>
   *
   * The returns SortedSet implements Serializable, but can only be
   * serialized if the set it wraps is likewise Serializable.
   *
   * @param s the set to wrap
   * @return a read-only view of the set
   * @see Serializable
   */
  public static SortedSet unmodifiableSortedSet(SortedSet s)
  {
    return new UnmodifiableSortedSet(s);
  }

  /**
   * The implementation of {@link #synchronizedSortedMap(SortedMap)}. This
   * class name is required for compatibility with Sun's JDK serializability.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   */
  private static class UnmodifiableSortedSet extends UnmodifiableSet
    implements SortedSet
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
    private SortedSet ss;

    /**
     * Wrap a given set.
     * @param ss the set to wrap
     * @throws NullPointerException if ss is null
     */
    UnmodifiableSortedSet(SortedSet ss)
    {
      super(ss);
      this.ss = ss;
    }

    public Comparator comparator()
    {
      return ss.comparator();
    }

    public Object first()
    {
      return ss.first();
    }

    public SortedSet headSet(Object toElement)
    {
      return new UnmodifiableSortedSet(ss.headSet(toElement));
    }

    public Object last()
    {
      return ss.last();
    }

    public SortedSet subSet(Object fromElement, Object toElement)
    {
      return new UnmodifiableSortedSet(ss.subSet(fromElement, toElement));
    }

    public SortedSet tailSet(Object fromElement)
    {
      return new UnmodifiableSortedSet(ss.tailSet(fromElement));
    }
  } // class UnmodifiableSortedSet
} // class Collections
