/* Collections.java -- Utility class with methods to operate on collections
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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


// TO DO:
// ~ Serialization is very much broken. Blame Sun for not specifying it.
// ~ The synchronized* and unmodifiable* methods don't have doc-comments.

package java.util;

import java.io.Serializable;

/**
 * Utility class consisting of static methods that operate on, or return
 * Collections. Contains methods to sort, search, reverse, fill and shuffle
 * Collections, methods to facilitate interoperability with legacy APIs that
 * are unaware of collections, a method to return a list which consists of
 * multiple copies of one element, and methods which "wrap" collections to give
 * them extra properties, such as thread-safety and unmodifiability.
 */
public class Collections
{
  /**
   * This class is non-instantiable.
   */
  private Collections()
  {
  }

  /**
   * An immutable, empty Set.
   * Note: This implementation isn't Serializable, although it should be by the
   * spec.
   */
  public static final Set EMPTY_SET = new AbstractSet()
  {
    public int size()
    {
      return 0;
    }

    // This is really cheating! I think it's perfectly valid, though - the
    // more conventional code is here, commented out, in case anyone disagrees.
    public Iterator iterator()
    {
      return EMPTY_LIST.iterator();
    }

    // public Iterator iterator() {
    //   return new Iterator() {
    // 
    //     public boolean hasNext() {
    //       return false;
    //     }
    // 
    //     public Object next() {
    //       throw new NoSuchElementException();
    //     }
    // 
    //     public void remove() {
    //       throw new UnsupportedOperationException();
    //     }
    //   };
    // }

  };

  /**
   * An immutable, empty List.
   * Note: This implementation isn't serializable, although it should be by the
   * spec.
   */
  public static final List EMPTY_LIST = new AbstractList()
  {
    public int size()
    {
      return 0;
    }

    public Object get(int index)
    {
      throw new IndexOutOfBoundsException();
    }
  };

  /**
   * An immutable, empty Map.
   * Note: This implementation isn't serializable, although it should be by the
   * spec.
   */
  public static final Map EMPTY_MAP = new AbstractMap()
  {
    public Set entrySet()
    {
      return EMPTY_SET;
    }
  };

  /**
   * Compare two objects with or without a Comparator. If c is null, uses the
   * natural ordering. Slightly slower than doing it inline if the JVM isn't
   * clever, but worth it for removing a duplicate of the search code.
   * Note: This same code is used in Arrays (for sort as well as search)
   */
  private static int compare(Object o1, Object o2, Comparator c)
  {
    if (c == null)
      {
	return ((Comparable) o1).compareTo(o2);
      }
    else
      {
	return c.compare(o1, o2);
      }
  }

  /**
   * The hard work for the search routines. If the Comparator given is null,
   * uses the natural ordering of the elements.
   */
  private static int search(List l, Object key, final Comparator c)
  {
    int pos = 0;

    // We use a linear search using an iterator if we can guess that the list
    // is sequential-access.
    if (l instanceof AbstractSequentialList)
      {
	ListIterator itr = l.listIterator();
    	for (int i = l.size() - 1; i >= 0; --i)
	  {
	    final int d = compare(key, itr.next(), c);
	    if (d == 0)
	      {
		return pos;
	      }
	    else if (d < 0)
	      {
		return -pos - 1;
	      }
	    pos++;
	  }

	// We assume the list is random-access, and use a binary search
      }
    else
      {
	int low = 0;
	int hi = l.size() - 1;
	while (low <= hi)
	  {
	    pos = (low + hi) >> 1;
	    final int d = compare(key, l.get(pos), c);
	    if (d == 0)
	      {
		return pos;
	      }
	    else if (d < 0)
	      {
		hi = pos - 1;
	      }
	    else
	      {
		low = ++pos;	// This gets the insertion point right on the last loop
	      }
	  }
      }

    // If we failed to find it, we do the same whichever search we did.
    return -pos - 1;
  }

  /**
   * Perform a binary search of a List for a key, using the natural ordering of
   * the elements. The list must be sorted (as by the sort() method) - if it is
   * not, the behaviour of this method is undefined, and may be an infinite
   * loop. Further, the key must be comparable with every item in the list. If
   * the list contains the key more than once, any one of them may be found. To
   * avoid pathological behaviour on sequential-access lists, a linear search
   * is used if (l instanceof AbstractSequentialList). Note: although the
   * specification allows for an infinite loop if the list is unsorted, it will
   * not happen in this (Classpath) implementation.
   *
   * @param l the list to search (must be sorted)
   * @param key the value to search for
   * @returns the index at which the key was found, or -n-1 if it was not
   *   found, where n is the index of the first value higher than key or
   *   a.length if there is no such value.
   * @exception ClassCastException if key could not be compared with one of the
   *   elements of l
   * @exception NullPointerException if a null element has compareTo called
   */
  public static int binarySearch(List l, Object key)
  {
    return search(l, key, null);
  }

  /**
   * Perform a binary search of a List for a key, using a supplied Comparator.
   * The list must be sorted (as by the sort() method with the same Comparator)
   * - if it is not, the behaviour of this method is undefined, and may be an
   * infinite loop. Further, the key must be comparable with every item in the
   * list. If the list contains the key more than once, any one of them may be
   * found. To avoid pathological behaviour on sequential-access lists, a
   * linear search is used if (l instanceof AbstractSequentialList). Note:
   * although the specification allows for an infinite loop if the list is
   * unsorted, it will not happen in this (Classpath) implementation.
   *
   * @param l the list to search (must be sorted)
   * @param key the value to search for
   * @param c the comparator by which the list is sorted
   * @returns the index at which the key was found, or -n-1 if it was not
   *   found, where n is the index of the first value higher than key or
   *   a.length if there is no such value.
   * @exception ClassCastException if key could not be compared with one of the
   *   elements of l
   */
  public static int binarySearch(List l, Object key, Comparator c)
  {
    if (c == null)
      {
	throw new NullPointerException();
      }
    return search(l, key, c);
  }

  /**
   * Copy one list to another. If the destination list is longer than the
   * source list, the remaining elements are unaffected. This method runs in
   * linear time.
   *
   * @param dest the destination list.
   * @param source the source list.
   * @exception IndexOutOfBoundsException if the destination list is shorter
   *   than the source list (the elements that can be copied will be, prior to
   *   the exception being thrown).
   * @exception UnsupportedOperationException if dest.listIterator() does not
   *   support the set operation.
   */
  public static void copy(List dest, List source)
  {
    Iterator i1 = source.iterator();
    ListIterator i2 = dest.listIterator();

    try
      {
	for (int i = source.size() - 1; i >= 0; --i)
	  {
	    i2.next();
	    i2.set(i1.next());
	  }
      }
    catch (NoSuchElementException x)
      {
	throw new IndexOutOfBoundsException("Source doesn't fit in dest.");      
      }
  }

  /**
   * Returns an Enumeration over a collection. This allows interoperability
   * with legacy APIs that require an Enumeration as input.
   *
   * @param c the Collection to iterate over
   * @returns an Enumeration backed by an Iterator over c
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
   * @exception UnsupportedOperationException if l.listIterator() does not
   *   support the set operation.
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
   * Find the maximum element in a Collection, according to the natural
   * ordering of the elements. This implementation iterates over the
   * Collection, so it works in linear time.
   *
   * @param c the Collection to find the maximum element of
   * @returns the maximum element of c
   * @exception NoSuchElementException if c is empty
   * @exception ClassCastException if elements in c are not mutually comparable
   * @exception NullPointerException if null.compareTo is called
   */
  public static Object max(Collection c)
  {
    Iterator itr = c.iterator();
    Comparable max = (Comparable) itr.next();	// throws NoSuchElementException
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	Object o = itr.next();
	if (max.compareTo(o) < 0)
	  {
	    max = (Comparable) o;
	  }
      }
    return max;
  }

  /**
   * Find the maximum element in a Collection, according to a specified
   * Comparator. This implementation iterates over the Collection, so it
   * works in linear time.
   *
   * @param c the Collection to find the maximum element of
   * @param order the Comparator to order the elements by
   * @returns the maximum element of c
   * @exception NoSuchElementException if c is empty
   * @exception ClassCastException if elements in c are not mutually comparable
   */
  public static Object max(Collection c, Comparator order)
  {
    Iterator itr = c.iterator();
    Object max = itr.next();	// throws NoSuchElementException
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	Object o = itr.next();
	if (order.compare(max, o) < 0)
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
   * @returns the minimum element of c
   * @exception NoSuchElementException if c is empty
   * @exception ClassCastException if elements in c are not mutually comparable
   * @exception NullPointerException if null.compareTo is called
   */
  public static Object min(Collection c)
  {
    Iterator itr = c.iterator();
    Comparable min = (Comparable) itr.next();	// throws NoSuchElementException
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	Object o = itr.next();
	if (min.compareTo(o) > 0)
	  min = (Comparable) o;
      }
    return min;
  }

  /**
   * Find the minimum element in a Collection, according to a specified
   * Comparator. This implementation iterates over the Collection, so it
   * works in linear time.
   *
   * @param c the Collection to find the minimum element of
   * @param order the Comparator to order the elements by
   * @returns the minimum element of c
   * @exception NoSuchElementException if c is empty
   * @exception ClassCastException if elements in c are not mutually comparable
   */
  public static Object min(Collection c, Comparator order)
  {
    Iterator itr = c.iterator();
    Object min = itr.next();	// throws NoSuchElementExcception
    int csize = c.size();
    for (int i = 1; i < csize; i++)
      {
	Object o = itr.next();
	if (order.compare(min, o) > 0)
	  min = o;
      }
    return min;
  }

  /**
   * Creates an immutable list consisting of the same object repeated n times.
   * The returned object is tiny, consisting of only a single reference to the
   * object and a count of the number of elements. It is Serializable.
   *
   * @param n the number of times to repeat the object
   * @param o the object to repeat
   * @returns a List consisting of n copies of o
   * @throws IllegalArgumentException if n < 0
   */
  // It's not Serializable, because the serialized form is unspecced.
  // Also I'm only assuming that it should be because I don't think it's
  // stated - I just would be amazed if it isn't...
  public static List nCopies(final int n, final Object o)
  {
    // Check for insane arguments
    if (n < 0)
      {
	throw new IllegalArgumentException();
      }

    // Create a minimal implementation of List
    return new AbstractList()
    {
      public int size()
      {
	return n;
      }

      public Object get(int index)
      {
	if (index < 0 || index >= n)
	  {
	    throw new IndexOutOfBoundsException();
	  }
	return o;
      }
    };
  }

  /**
   * Reverse a given list. This method works in linear time.
   *
   * @param l the list to reverse.
   * @exception UnsupportedOperationException if l.listIterator() does not
   *   support the set operation.
   */
  public static void reverse(List l)
  {
    ListIterator i1 = l.listIterator();
    int pos1 = 0;
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

  static class ReverseComparator implements Comparator, Serializable
  {
    public int compare(Object a, Object b)
    {
      return -((Comparable) a).compareTo(b);
    }
  }
  
  static ReverseComparator rcInstance = new ReverseComparator();
  
  /**
   * Get a comparator that implements the reverse of natural ordering. This is
   * intended to make it easy to sort into reverse order, by simply passing
   * Collections.reverseOrder() to the sort method. The return value of this
   * method is Serializable.
   */
  public static Comparator reverseOrder()
  {
    return rcInstance;
  }

  /**
   * Shuffle a list according to a default source of randomness. The algorithm
   * used would result in a perfectly fair shuffle (that is, each element would
   * have an equal chance of ending up in any position) with a perfect source
   * of randomness; in practice the results are merely very close to perfect.
   * <p>
   * This method operates in linear time on a random-access list, but may take
   * quadratic time on a sequential-access list.
   * Note: this (classpath) implementation will never take quadratic time, but
   * it does make a copy of the list. This is in line with the behaviour of the
   * sort methods and seems preferable.
   *
   * @param l the list to shuffle.
   * @exception UnsupportedOperationException if l.listIterator() does not
   *   support the set operation.
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

  /** Cache a single Random object for use by shuffle(List). This improves
    * performance as well as ensuring that sequential calls to shuffle() will
    * not result in the same shuffle order occuring: the resolution of 
    * System.currentTimeMillis() is not sufficient to guarantee a unique seed.
    */
  private static Random defaultRandom = null;

  /**
   * Shuffle a list according to a given source of randomness. The algorithm
   * used iterates backwards over the list, swapping each element with an
   * element randomly selected from the elements in positions less than or
   * equal to it (using r.nextInt(int)).
   * <p>
   * This algorithm would result in a perfectly fair shuffle (that is, each
   * element would have an equal chance of ending up in any position) if r were
   * a perfect source of randomness. In practise (eg if r = new Random()) the
   * results are merely very close to perfect.
   * <p>
   * This method operates in linear time on a random-access list, but may take
   * quadratic time on a sequential-access list.
   * Note: this (classpath) implementation will never take quadratic time, but
   * it does make a copy of the list. This is in line with the behaviour of the
   * sort methods and seems preferable.
   *
   * @param l the list to shuffle.
   * @param r the source of randomness to use for the shuffle.
   * @exception UnsupportedOperationException if l.listIterator() does not
   *   support the set operation.
   */
  public static void shuffle(List l, Random r)
  {
    Object[] a = l.toArray();	// Dump l into an array
    int lsize = l.size();
    ListIterator i = l.listIterator(lsize);

    // Iterate backwards over l
    for (int pos = lsize - 1; pos >= 0; --pos)
      {
	// Obtain a random position to swap with. pos + 1 is used so that the
	// range of the random number includes the current position.
	int swap = r.nextInt(pos + 1);

	// Swap the swapth element of the array with the next element of the
	// list.
	Object o = a[swap];
	a[swap] = a[pos];
	a[pos] = o;

	// Set the element in the original list accordingly.
	i.previous();
	i.set(o);
      }
  }

  /**
   * Obtain an immutable Set consisting of a single element. The return value
   * of this method is Serializable.
   *
   * @param o the single element.
   * @returns an immutable Set containing only o.
   */
  // It's not serializable because the spec is broken.
  public static Set singleton(final Object o)
  {
    return new AbstractSet()
    {
      public int size()
      {
	return 1;
      }

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
		return o;
	      }
	    else
	      {
		throw new NoSuchElementException();
	      }
	  }

	  public void remove()
	  {
	    throw new UnsupportedOperationException();
	  }
	};
      }
    };
  }

  /**
   * Obtain an immutable List consisting of a single element. The return value
   * of this method is Serializable.
   *
   * @param o the single element.
   * @returns an immutable List containing only o.
   */
  // It's not serializable because the spec is broken.
  public static List singletonList(final Object o)
  {
    return new AbstractList()
    {
      public int size()
      {
	return 1;
      }

      public Object get(int index)
      {
	if (index == 0)
	  {
	    throw new IndexOutOfBoundsException();
	  }
	else
	  {
	    return o;
	  }
      }
    };
  }

  /**
   * Obtain an immutable Map consisting of a single key value pair.
   * The return value of this method is Serializable.
   *
   * @param key the single key.
   * @param value the single value.
   * @returns an immutable Map containing only the single key value pair.
   */
  // It's not serializable because the spec is broken.
  public static Map singletonMap(final Object key, final Object value)
  {
    return new AbstractMap()
    {
      public Set entrySet()
      {
	return singleton(new HashMap.Entry(key, value));
      }
    };
  }

  /**
   * Sort a list according to the natural ordering of its elements. The list
   * must be modifiable, but can be of fixed size. The sort algorithm is
   * precisely that used by Arrays.sort(Object[]), which offers guaranteed
   * nlog(n) performance. This implementation dumps the list into an array,
   * sorts the array, and then iterates over the list setting each element from
   * the array.
   *
   * @param l the List to sort
   * @exception ClassCastException if some items are not mutually comparable
   * @exception UnsupportedOperationException if the List is not modifiable
   */
  public static void sort(List l)
  {
    Object[] a = l.toArray();
    Arrays.sort(a);
    ListIterator i = l.listIterator();
    for (int pos = 0; pos < a.length; pos++)
      {
	i.next();
	i.set(a[pos]);
      }
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
   * @param c the Comparator specifying the ordering for the elements
   * @exception ClassCastException if c will not compare some pair of items
   * @exception UnsupportedOperationException if the List is not modifiable
   */
  public static void sort(List l, Comparator c)
  {
    Object[] a = l.toArray();
    Arrays.sort(a, c);
    ListIterator i = l.listIterator();
    for (int pos = 0; pos < a.length; pos++)
      {
	i.next();
	i.set(a[pos]);
      }
  }

  // All the methods from here on in require doc-comments.

  public static Collection synchronizedCollection(Collection c)
  {
    return new SynchronizedCollection(c);
  }
  public static List synchronizedList(List l)
  {
    return new SynchronizedList(l);
  }
  public static Map synchronizedMap(Map m)
  {
    return new SynchronizedMap(m);
  }
  public static Set synchronizedSet(Set s)
  {
    return new SynchronizedSet(s);
  }
  public static SortedMap synchronizedSortedMap(SortedMap m)
  {
    return new SynchronizedSortedMap(m);
  }
  public static SortedSet synchronizedSortedSet(SortedSet s)
  {
    return new SynchronizedSortedSet(s);
  }
  public static Collection unmodifiableCollection(Collection c)
  {
    return new UnmodifiableCollection(c);
  }
  public static List unmodifiableList(List l)
  {
    return new UnmodifiableList(l);
  }
  public static Map unmodifiableMap(Map m)
  {
    return new UnmodifiableMap(m);
  }
  public static Set unmodifiableSet(Set s)
  {
    return new UnmodifiableSet(s);
  }
  public static SortedMap unmodifiableSortedMap(SortedMap m)
  {
    return new UnmodifiableSortedMap(m);
  }
  public static SortedSet unmodifiableSortedSet(SortedSet s)
  {
    return new UnmodifiableSortedSet(s);
  }

  // Sun's spec will need to be checked for the precise names of these
  // classes, for serializability's sake. However, from what I understand,
  // serialization is broken for these classes anyway.

  // Note: although this code is largely uncommented, it is all very
  // mechanical and there's nothing really worth commenting.
  // When serialization of these classes works, we'll need doc-comments on
  // them to document the serialized form.

  private static class UnmodifiableIterator implements Iterator
  {
    private Iterator i;

    public UnmodifiableIterator(Iterator i)
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
  }

  private static class UnmodifiableListIterator extends UnmodifiableIterator
    implements ListIterator
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    private ListIterator li;

    public UnmodifiableListIterator(ListIterator li)
    {
      super(li);
      this.li = li;
    }

    public boolean hasPrevious()
    {
      return li.hasPrevious();
    }
    public Object previous()
    {
      return li.previous();
    }
    public int nextIndex()
    {
      return li.nextIndex();
    }
    public int previousIndex()
    {
      return li.previousIndex();
    }
    public void add(Object o)
    {
      throw new UnsupportedOperationException();
    }
    public void set(Object o)
    {
      throw new UnsupportedOperationException();
    }
  }

  private static class UnmodifiableCollection implements Collection,
    Serializable
  {
    Collection c;

    public UnmodifiableCollection(Collection c)
    {
      this.c = c;
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
    public Object[] toArray(Object[]a)
    {
      return c.toArray(a);
    }
    public String toString()
    {
      return c.toString();
    }
  }

  private static class UnmodifiableList extends UnmodifiableCollection
    implements List
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    List l;

    public UnmodifiableList(List l)
    {
      super(l);
      this.l = l;
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
      return l.equals(o);
    }
    public Object get(int index)
    {
      return l.get(index);
    }
    public int hashCode()
    {
      return l.hashCode();
    }
    public int indexOf(Object o)
    {
      return l.indexOf(o);
    }
    public int lastIndexOf(Object o)
    {
      return l.lastIndexOf(o);
    }
    public ListIterator listIterator()
    {
      return new UnmodifiableListIterator(l.listIterator());
    }
    public ListIterator listIterator(int index)
    {
      return new UnmodifiableListIterator(l.listIterator(index));
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
      return new UnmodifiableList(l.subList(fromIndex, toIndex));
    }
  }

  private static class UnmodifiableSet extends UnmodifiableCollection
    implements Set
  {
    public UnmodifiableSet(Set s)
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
  }

  private static class UnmodifiableSortedSet extends UnmodifiableSet
    implements SortedSet
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    private SortedSet ss;

    public UnmodifiableSortedSet(SortedSet ss)
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
    public Object last()
    {
      return ss.last();
    }
    public SortedSet headSet(Object toElement)
    {
      return new UnmodifiableSortedSet(ss.headSet(toElement));
    }
    public SortedSet tailSet(Object fromElement)
    {
      return new UnmodifiableSortedSet(ss.tailSet(fromElement));
    }
    public SortedSet subSet(Object fromElement, Object toElement)
    {
      return new UnmodifiableSortedSet(ss.subSet(fromElement, toElement));
    }
  }

  private static class UnmodifiableMap implements Map, Serializable
  {
    Map m;

    public UnmodifiableMap(Map m)
    {
      this.m = m;
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

    // This is one of the ickiest cases of nesting I've ever seen. It just
    // means "return an UnmodifiableSet, except that the iterator() method
    // returns an UnmodifiableIterator whos next() method returns an
    // unmodifiable wrapper around its normal return value".
    public Set entrySet()
    {
      return new UnmodifiableSet(m.entrySet())
      {
	public Iterator iterator()
	{
	  return new UnmodifiableIterator(c.iterator())
	  {
	    public Object next()
	    {
	      final Map.Entry e = (Map.Entry) super.next();
	      return new Map.Entry()
	      {
		public Object getKey()
		{
		  return e.getKey();
		}
		public Object getValue()
		{
		  return e.getValue();
		}
		public Object setValue(Object value)
		{
		  throw new UnsupportedOperationException();
		}
		public int hashCode()
		{
		  return e.hashCode();
		}
		public boolean equals(Object o)
		{
		  return e.equals(o);
		}
	      };
	    }
	  };
	}
      };
    }
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
      return new UnmodifiableSet(m.keySet());
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
    public Collection values()
    {
      return new UnmodifiableCollection(m.values());
    }
    public String toString()
    {
      return m.toString();
    }
  }

  private static class UnmodifiableSortedMap extends UnmodifiableMap
    implements SortedMap
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    private SortedMap sm;

    public UnmodifiableSortedMap(SortedMap sm)
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
    public Object lastKey()
    {
      return sm.lastKey();
    }
    public SortedMap headMap(Object toKey)
    {
      return new UnmodifiableSortedMap(sm.headMap(toKey));
    }
    public SortedMap tailMap(Object fromKey)
    {
      return new UnmodifiableSortedMap(sm.tailMap(fromKey));
    }
    public SortedMap subMap(Object fromKey, Object toKey)
    {
      return new UnmodifiableSortedMap(sm.subMap(fromKey, toKey));
    }
  }

  // All the "Synchronized" wrapper objects include a "sync" field which
  // specifies what object to synchronize on. That way, nested wrappers such as
  // UnmodifiableMap.keySet synchronize on the right things.

  private static class SynchronizedIterator implements Iterator
  {
    Object sync;
    private Iterator i;

    public SynchronizedIterator(Object sync, Iterator i)
    {
      this.sync = sync;
      this.i = i;
    }

    public Object next()
    {
      synchronized(sync)
      {
	return i.next();
      }
    }
    public boolean hasNext()
    {
      synchronized(sync)
      {
	return i.hasNext();
      }
    }
    public void remove()
    {
      synchronized(sync)
      {
	i.remove();
      }
    }
  }

  private static class SynchronizedListIterator extends SynchronizedIterator
    implements ListIterator
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    private ListIterator li;

    public SynchronizedListIterator(Object sync, ListIterator li)
    {
      super(sync, li);
      this.li = li;
    }

    public boolean hasPrevious()
    {
      synchronized(sync)
      {
	return li.hasPrevious();
      }
    }
    public Object previous()
    {
      synchronized(sync)
      {
	return li.previous();
      }
    }
    public int nextIndex()
    {
      synchronized(sync)
      {
	return li.nextIndex();
      }
    }
    public int previousIndex()
    {
      synchronized(sync)
      {
	return li.previousIndex();
      }
    }
    public void add(Object o)
    {
      synchronized(sync)
      {
	li.add(o);
      }
    }
    public void set(Object o)
    {
      synchronized(sync)
      {
	li.set(o);
      }
    }
  }

  private static class SynchronizedCollection implements Collection,
    Serializable
  {
    Object sync;
    Collection c;

    public SynchronizedCollection(Collection c)
    {
      this.sync = this;
      this.c = c;
    }
    public SynchronizedCollection(Object sync, Collection c)
    {
      this.c = c;
      this.sync = sync;
    }

    public boolean add(Object o)
    {
      synchronized(sync)
      {
	return c.add(o);
      }
    }
    public boolean addAll(Collection col)
    {
      synchronized(sync)
      {
	return c.addAll(col);
      }
    }
    public void clear()
    {
      synchronized(sync)
      {
	c.clear();
      }
    }
    public boolean contains(Object o)
    {
      synchronized(sync)
      {
	return c.contains(o);
      }
    }
    public boolean containsAll(Collection c1)
    {
      synchronized(sync)
      {
	return c.containsAll(c1);
      }
    }
    public boolean isEmpty()
    {
      synchronized(sync)
      {
	return c.isEmpty();
      }
    }
    public Iterator iterator()
    {
      synchronized(sync)
      {
	return new SynchronizedIterator(sync, c.iterator());
      }
    }
    public boolean remove(Object o)
    {
      synchronized(sync)
      {
	return c.remove(o);
      }
    }
    public boolean removeAll(Collection col)
    {
      synchronized(sync)
      {
	return c.removeAll(col);
      }
    }
    public boolean retainAll(Collection col)
    {
      synchronized(sync)
      {
	return c.retainAll(col);
      }
    }
    public int size()
    {
      synchronized(sync)
      {
	return c.size();
      }
    }
    public Object[] toArray()
    {
      synchronized(sync)
      {
	return c.toArray();
      }
    }
    public Object[] toArray(Object[]a)
    {
      synchronized(sync)
      {
	return c.toArray(a);
      }
    }
    public String toString()
    {
      synchronized(sync)
      {
	return c.toString();
      }
    }
  }

  private static class SynchronizedList extends SynchronizedCollection
    implements List
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    List l;

    public SynchronizedList(Object sync, List l)
    {
      super(sync, l);
      this.l = l;
    }
    public SynchronizedList(List l)
    {
      super(l);
      this.l = l;
    }

    public void add(int index, Object o)
    {
      synchronized(sync)
      {
	l.add(index, o);
      }
    }
    public boolean addAll(int index, Collection c)
    {
      synchronized(sync)
      {
	return l.addAll(index, c);
      }
    }
    public boolean equals(Object o)
    {
      synchronized(sync)
      {
	return l.equals(o);
      }
    }
    public Object get(int index)
    {
      synchronized(sync)
      {
	return l.get(index);
      }
    }
    public int hashCode()
    {
      synchronized(sync)
      {
	return l.hashCode();
      }
    }
    public int indexOf(Object o)
    {
      synchronized(sync)
      {
	return l.indexOf(o);
      }
    }
    public int lastIndexOf(Object o)
    {
      synchronized(sync)
      {
	return l.lastIndexOf(o);
      }
    }
    public ListIterator listIterator()
    {
      synchronized(sync)
      {
	return new SynchronizedListIterator(sync, l.listIterator());
      }
    }
    public ListIterator listIterator(int index)
    {
      synchronized(sync)
      {
	return new SynchronizedListIterator(sync, l.listIterator(index));
      }
    }
    public Object remove(int index)
    {
      synchronized(sync)
      {
	return l.remove(index);
      }
    }
    public boolean remove(Object o)
    {
      synchronized(sync)
      {
	return l.remove(o);
      }
    }
    public Object set(int index, Object o)
    {
      synchronized(sync)
      {
	return l.set(index, o);
      }
    }
    public List subList(int fromIndex, int toIndex)
    {
      synchronized(sync)
      {
	return new SynchronizedList(l.subList(fromIndex, toIndex));
      }
    }
  }

  private static class SynchronizedSet extends SynchronizedCollection
    implements Set
  {
    public SynchronizedSet(Object sync, Set s)
    {
      super(sync, s);
    }
    public SynchronizedSet(Set s)
    {
      super(s);
    }

    public boolean equals(Object o)
    {
      synchronized(sync)
      {
	return c.equals(o);
      }
    }
    public int hashCode()
    {
      synchronized(sync)
      {
	return c.hashCode();
      }
    }
  }

  private static class SynchronizedSortedSet extends SynchronizedSet
    implements SortedSet
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    private SortedSet ss;

    public SynchronizedSortedSet(Object sync, SortedSet ss)
    {
      super(sync, ss);
      this.ss = ss;
    }
    public SynchronizedSortedSet(SortedSet ss)
    {
      super(ss);
      this.ss = ss;
    }

    public Comparator comparator()
    {
      synchronized(sync)
      {
	return ss.comparator();
      }
    }
    public Object first()
    {
      synchronized(sync)
      {
	return ss.first();
      }
    }
    public Object last()
    {
      synchronized(sync)
      {
	return ss.last();
      }
    }
    public SortedSet headSet(Object toElement)
    {
      synchronized(sync)
      {
	return new SynchronizedSortedSet(sync, ss.headSet(toElement));
      }
    }
    public SortedSet tailSet(Object fromElement)
    {
      synchronized(sync)
      {
	return new SynchronizedSortedSet(sync, ss.tailSet(fromElement));
      }
    }
    public SortedSet subSet(Object fromElement, Object toElement)
    {
      synchronized(sync)
      {
	return new SynchronizedSortedSet(sync,
					 ss.subSet(fromElement, toElement));
      }
    }
  }

  private static class SynchronizedMap implements Map, Serializable
  {
    Object sync;
    Map m;

    public SynchronizedMap(Object sync, Map m)
    {
      this.sync = sync;
      this.m = m;
    }
    public SynchronizedMap(Map m)
    {
      this.m = m;
      this.sync = this;
    }

    public void clear()
    {
      synchronized(sync)
      {
	m.clear();
      }
    }
    public boolean containsKey(Object key)
    {
      synchronized(sync)
      {
	return m.containsKey(key);
      }
    }
    public boolean containsValue(Object value)
    {
      synchronized(sync)
      {
	return m.containsValue(value);
      }
    }

    // This is one of the ickiest cases of nesting I've ever seen. It just
    // means "return a SynchronizedSet, except that the iterator() method
    // returns an SynchronizedIterator whos next() method returns a
    // synchronized wrapper around its normal return value".
    public Set entrySet()
    {
      synchronized(sync)
      {
	return new SynchronizedSet(sync, m.entrySet())
	{
	  public Iterator iterator()
	  {
	    synchronized(SynchronizedMap.this.sync)
	    {
	      return new SynchronizedIterator(SynchronizedMap.this.sync,
					      c.iterator())
	      {
		public Object next()
		{
		  synchronized(SynchronizedMap.this.sync)
		  {
		    final Map.Entry e = (Map.Entry) super.next();
		    return new Map.Entry()
		    {
		      public Object getKey()
		      {
			synchronized(SynchronizedMap.this.sync)
			{
			  return e.getKey();
			}
		      }
		      public Object getValue()
		      {
			synchronized(SynchronizedMap.this.sync)
			{
			  return e.getValue();
			}
		      }
		      public Object setValue(Object value)
		      {
			synchronized(SynchronizedMap.this.sync)
			{
			  return e.setValue(value);
			}
		      }
		      public int hashCode()
		      {
			synchronized(SynchronizedMap.this.sync)
			{
			  return e.hashCode();
			}
		      }
		      public boolean equals(Object o)
		      {
			synchronized(SynchronizedMap.this.sync)
			{
			  return e.equals(o);
			}
		      }
		    };
		  }
		}
	      };
	    }
	  }
	};
      }
    }
    public boolean equals(Object o)
    {
      synchronized(sync)
      {
	return m.equals(o);
      }
    }
    public Object get(Object key)
    {
      synchronized(sync)
      {
	return m.get(key);
      }
    }
    public Object put(Object key, Object value)
    {
      synchronized(sync)
      {
	return m.put(key, value);
      }
    }
    public int hashCode()
    {
      synchronized(sync)
      {
	return m.hashCode();
      }
    }
    public boolean isEmpty()
    {
      synchronized(sync)
      {
	return m.isEmpty();
      }
    }
    public Set keySet()
    {
      synchronized(sync)
      {
	return new SynchronizedSet(sync, m.keySet());
      }
    }
    public void putAll(Map map)
    {
      synchronized(sync)
      {
	m.putAll(map);
      }
    }
    public Object remove(Object o)
    {
      synchronized(sync)
      {
	return m.remove(o);
      }
    }

    public int size()
    {
      synchronized(sync)
      {
	return m.size();
      }
    }
    public Collection values()
    {
      synchronized(sync)
      {
	return new SynchronizedCollection(sync, m.values());
      }
    }
    public String toString()
    {
      synchronized(sync)
      {
	return m.toString();
      }
    }
  }

  private static class SynchronizedSortedMap extends SynchronizedMap
    implements SortedMap
  {
    // This is stored both here and in the superclass, to avoid excessive
    // casting.
    private SortedMap sm;

    public SynchronizedSortedMap(Object sync, SortedMap sm)
    {
      super(sync, sm);
      this.sm = sm;
    }
    public SynchronizedSortedMap(SortedMap sm)
    {
      super(sm);
      this.sm = sm;
    }

    public Comparator comparator()
    {
      synchronized(sync)
      {
	return sm.comparator();
      }
    }
    public Object firstKey()
    {
      synchronized(sync)
      {
	return sm.firstKey();
      }
    }
    public Object lastKey()
    {
      synchronized(sync)
      {
	return sm.lastKey();
      }
    }
    public SortedMap headMap(Object toKey)
    {
      return new SynchronizedSortedMap(sync, sm.headMap(toKey));
    }
    public SortedMap tailMap(Object fromKey)
    {
      return new SynchronizedSortedMap(sync, sm.tailMap(fromKey));
    }
    public SortedMap subMap(Object fromKey, Object toKey)
    {
      return new SynchronizedSortedMap(sync, sm.subMap(fromKey, toKey));
    }
  }
}
