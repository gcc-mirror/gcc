/* CopyOnWriteArrayList.java
   Copyright (C) 2006 Free Software Foundation

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

package java.util.concurrent;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import java.lang.reflect.Array;

import java.util.AbstractList;
import java.util.Arrays;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.RandomAccess;

/**
 * A thread-safe implementation of an ArrayList. A CopyOnWriteArrayList is
 * as special ArrayList which performs copies of the underlying storage
 * each time a write (<code>remove</code>, <code>add</code> etc..) operation
 * is performed.<br />
 * <br />
 * The update operation in this class run usually in <code>O(n)</code> or worse,
 * but traversal operations are fast and efficient, especially when running in
 * a multi-thread environment without the need to design complex synchronize
 * mechanisms.<br />
 * <br />
 * <code>Iterator</code>s in this class work on a snapshot of the backing store
 * at the moment the iterator itself was created, hence the iterator will not
 * reflect changes in the underlying storage. Thus, update operation on the
 * <code>Iterator</code>s are not supported, but as interferences from other
 * threads are impossible, no <code>ConcurrentModificationException</code>
 * will be ever thrown from within the <code>Iterator</code>.
 * <br /><br />
 * This class is especially useful when used with event handling, like the
 * following code demonstrates:<br />
 * <code><pre>
 *
 * CopyOnWriteArrayList<EventListener> listeners =
 *   new CopyOnWriteArrayList<EventListener>();
 *
 * [...]
 *
 * for (final EventListener listener : listeners)
 *   {
 *     Runnable dispatcher = new Runnable() {
 *       public void run()
 *       {
 *         listener.preferenceChange(event);
 *       }
 *     };
 *
 *     Executor executor = Executors.newSingleThreadExecutor();
 *     executor.execute(dispatcher);
 *   }
 * </pre></code>
 *
 * @since 1.5
 */
public class CopyOnWriteArrayList<E>
  implements List<E>, RandomAccess, Cloneable, Serializable
{
  /**
   *
   */
  private static final long serialVersionUID = 8673264195747942595L;

  /**
   * Where the data is stored.
   */
  private transient E[] data;

  /**
   * Construct a new ArrayList with the default capacity (16).
   */
  public CopyOnWriteArrayList()
  {
    data = (E[]) new Object[0];
  }

  /**
   * Construct a new ArrayList, and initialize it with the elements in the
   * supplied Collection. The initial capacity is 110% of the Collection's size.
   *
   * @param c
   *          the collection whose elements will initialize this list
   * @throws NullPointerException
   *           if c is null
   */
  public CopyOnWriteArrayList(Collection< ? extends E> c)
  {
    // FIXME ... correct?  use c.toArray()
    data = (E[]) new Object[c.size()];
    int index = 0;
    for (E value : c)
      data[index++] = value;
  }

  /**
   * Construct a new ArrayList, and initialize it with the elements in the
   * supplied array.
   *
   * @param array
   *          the array used to initialize this list
   * @throws NullPointerException
   *           if array is null
   */
  public CopyOnWriteArrayList(E[] array)
  {
    data = (E[]) array.clone();
  }

  /**
   * Returns the number of elements in this list.
   *
   * @return the list size
   */
  public int size()
  {
    return data.length;
  }

  /**
   * Checks if the list is empty.
   *
   * @return true if there are no elements
   */
  public boolean isEmpty()
  {
    return data.length == 0;
  }

  /**
   * Returns true if element is in this ArrayList.
   *
   * @param e
   *          the element whose inclusion in the List is being tested
   * @return true if the list contains e
   */
  public boolean contains(Object e)
  {
    return indexOf(e) != -1;
  }

  /**
   * Tests whether this collection contains all the elements in a given
   * collection. This implementation iterates over the given collection,
   * testing whether each element is contained in this collection. If any one
   * is not, false is returned. Otherwise true is returned.
   *
   * @param c the collection to test against
   * @return true if this collection contains all the elements in the given
   *         collection
   * @throws NullPointerException if the given collection is null
   * @see #contains(Object)
   */
  public boolean containsAll(Collection<?> c)
  {
    Iterator<?> itr = c.iterator();
    int pos = c.size();
    while (--pos >= 0)
      if (!contains(itr.next()))
        return false;
    return true;
  }

  /**
   * Returns the lowest index at which element appears in this List, or -1 if it
   * does not appear.
   *
   * @param e
   *          the element whose inclusion in the List is being tested
   * @return the index where e was found
   */
  public int indexOf(Object e)
  {
    E[] data = this.data;
    for (int i = 0; i < data.length; i++)
      if (equals(e, data[i]))
        return i;
    return -1;
  }

  /**
   * Return the lowest index greater equal <code>index</code> at which
   * <code>e</code> appears in this List, or -1 if it does not
   * appear.
   *
   * @param e the element whose inclusion in the list is being tested
   * @param index the index at which the search begins
   * @return the index where <code>e</code> was found
   */
  public int indexOf(E e, int index)
  {
    E[] data = this.data;

    for (int i = index; i < data.length; i++)
      if (equals(e, data[i]))
        return i;
    return -1;
  }

  /**
   * Returns the highest index at which element appears in this List, or -1 if
   * it does not appear.
   *
   * @param e
   *          the element whose inclusion in the List is being tested
   * @return the index where e was found
   */
  public int lastIndexOf(Object e)
  {
    E[] data = this.data;
    for (int i = data.length - 1; i >= 0; i--)
      if (equals(e, data[i]))
        return i;
    return -1;
  }

  /**
   * Returns the highest index lesser equal <code>index</code> at
   * which <code>e</code> appears in this List, or -1 if it does not
   * appear.
   *
   * @param e the element whose inclusion in the list is being tested
   * @param index the index at which the search begins
   * @return the index where <code>e</code> was found
   */
  public int lastIndexOf(E e, int index)
  {
    E[] data = this.data;

    for (int i = index; i >= 0; i--)
      if (equals(e, data[i]))
        return i;
    return -1;
  }

  /**
   * Creates a shallow copy of this ArrayList (elements are not cloned).
   *
   * @return the cloned object
   */
  public Object clone()
  {
    CopyOnWriteArrayList<E> clone = null;
    try
      {
        clone = (CopyOnWriteArrayList<E>) super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        // Impossible to get here.
      }
    return clone;
  }

  /**
   * Returns an Object array containing all of the elements in this ArrayList.
   * The array is independent of this list.
   *
   * @return an array representation of this list
   */
  public Object[] toArray()
  {
    E[] data = this.data;
    E[] array = (E[]) new Object[data.length];
    System.arraycopy(data, 0, array, 0, data.length);
    return array;
  }

  /**
   * Returns an Array whose component type is the runtime component type of the
   * passed-in Array. The returned Array is populated with all of the elements
   * in this ArrayList. If the passed-in Array is not large enough to store all
   * of the elements in this List, a new Array will be created and returned; if
   * the passed-in Array is <i>larger</i> than the size of this List, then
   * size() index will be set to null.
   *
   * @param a
   *          the passed-in Array
   * @return an array representation of this list
   * @throws ArrayStoreException
   *           if the runtime type of a does not allow an element in this list
   * @throws NullPointerException
   *           if a is null
   */
  public <T> T[] toArray(T[] a)
  {
    E[] data = this.data;
    if (a.length < data.length)
      a = (T[]) Array.newInstance(a.getClass().getComponentType(), data.length);
    else if (a.length > data.length)
      a[data.length] = null;
    System.arraycopy(data, 0, a, 0, data.length);
    return a;
  }

  /**
   * Retrieves the element at the user-supplied index.
   *
   * @param index
   *          the index of the element we are fetching
   * @throws IndexOutOfBoundsException
   *           if index &lt; 0 || index &gt;= size()
   */
  public E get(int index)
  {
    return data[index];
  }

  /**
   * Sets the element at the specified index. The new element, e, can be an
   * object of any type or null.
   *
   * @param index
   *          the index at which the element is being set
   * @param e
   *          the element to be set
   * @return the element previously at the specified index
   * @throws IndexOutOfBoundsException
   *           if index &lt; 0 || index &gt;= 0
   */
  public synchronized E set(int index, E e)
  {
    E result = data[index];
    E[] newData = (E[]) data.clone();
    newData[index] = e;
    data = newData;
    return result;
  }

  /**
   * Appends the supplied element to the end of this list. The element, e, can
   * be an object of any type or null.
   *
   * @param e
   *          the element to be appended to this list
   * @return true, the add will always succeed
   */
  public synchronized boolean add(E e)
  {
    E[] data = this.data;
    E[] newData = (E[]) new Object[data.length + 1];
    System.arraycopy(data, 0, newData, 0, data.length);
    newData[data.length] = e;
    this.data = newData;
    return true;
  }

  /**
   * Adds the supplied element at the specified index, shifting all elements
   * currently at that index or higher one to the right. The element, e, can be
   * an object of any type or null.
   *
   * @param index
   *          the index at which the element is being added
   * @param e
   *          the item being added
   * @throws IndexOutOfBoundsException
   *           if index &lt; 0 || index &gt; size()
   */
  public synchronized void add(int index, E e)
  {
    E[] data = this.data;
    E[] newData = (E[]) new Object[data.length + 1];
    System.arraycopy(data, 0, newData, 0, index);
    newData[index] = e;
    System.arraycopy(data, index, newData, index + 1, data.length - index);
    this.data = newData;
  }

  /**
   * Removes the element at the user-supplied index.
   *
   * @param index
   *          the index of the element to be removed
   * @return the removed Object
   * @throws IndexOutOfBoundsException
   *           if index &lt; 0 || index &gt;= size()
   */
  public synchronized E remove(int index)
  {
    if (index < 0 || index >= this.size())
      throw new IndexOutOfBoundsException("index = " +  index);

    E[] snapshot = this.data;
    E[] newData = (E[]) new Object[snapshot.length - 1];

    E result = snapshot[index];

    if (index > 0)
      System.arraycopy(snapshot, 0, newData, 0, index);

    System.arraycopy(snapshot, index + 1, newData, index,
                     snapshot.length - index - 1);

    this.data = newData;

    return result;
  }

  /**
   * Remove the first occurrence, if any, of the given object from this list,
   * returning <code>true</code> if the object was removed, <code>false</code>
   * otherwise.
   *
   * @param element the object to be removed.
   * @return true if element was removed, false otherwise. false means also that
   * the underlying storage was unchanged after this operation concluded.
   */
  public synchronized boolean remove(Object element)
  {
    E[] snapshot = this.data;
    int len = snapshot.length;

    if (len == 0)
      return false;

    E[] newData = (E[]) new Object[len - 1];

    // search the element to remove while filling the backup array
    // this way we can run this method in O(n)
    int elementIndex = -1;
    for (int i = 0; i < snapshot.length; i++)
      {
        if (equals(element, snapshot[i]))
          {
            elementIndex = i;
            break;
          }

        if (i < newData.length)
          newData[i] = snapshot[i];
      }

    if (elementIndex < 0)
      return false;

    System.arraycopy(snapshot, elementIndex + 1, newData, elementIndex,
                     snapshot.length - elementIndex - 1);
    this.data = newData;

    return true;
  }

  /**
   * Removes all the elements contained in the given collection.
   * This method removes the elements that are contained in both
   * this list and in the given collection.
   *
   * @param c the collection containing the elements to be removed from this
   * list.
   * @return true if at least one element was removed, indicating that
   * the list internal storage changed as a result, false otherwise.
   */
  public synchronized boolean removeAll(Collection<?> c)
  {
    if (c.size() == 0)
      return false;

    E [] snapshot = this.data;
    E [] storage = (E[]) new Object[this.data.length];
    boolean changed = false;

    int length = 0;
    for (E element : snapshot)
      {
        // copy all the elements, including null values
        // if the collection can hold it
        // FIXME: slow operation
        if (c.contains(element))
          changed = true;
        else
          storage[length++] = element;
      }

    if (!changed)
      return false;

    E[] newData = (E[]) new Object[length];
    System.arraycopy(storage, 0, newData, 0, length);

    this.data = newData;

    return true;
  }

  /**
   * Removes all the elements that are not in the passed collection.
   * If the collection is void, this method has the same effect of
   * <code>clear()</code>.
   * Please, note that this method is extremely slow (unless the argument has
   * <code>size == 0</code>) and has bad performance is both space and time
   * usage.
   *
   * @param c the collection containing the elements to be retained by this
   * list.
   * @return true the list internal storage changed as a result of this
   * operation, false otherwise.
   */
  public synchronized boolean retainAll(Collection<?> c)
  {
    // if the given collection does not contain elements
    // we remove all the elements from our storage
    if (c.size() == 0)
      {
        this.clear();
        return true;
      }

    E [] snapshot = this.data;
    E [] storage = (E[]) new Object[this.data.length];

    int length = 0;
    for (E element : snapshot)
      {
        if (c.contains(element))
          storage[length++] = element;
      }

    // means we retained all the elements previously in our storage
    // we are running already slow here, but at least we avoid copying
    // another array and changing the internal storage
    if (length == snapshot.length)
      return false;

    E[] newData = (E[]) new Object[length];
    System.arraycopy(storage, 0, newData, 0, length);

    this.data = newData;

    return true;
  }

  /**
   * Removes all elements from this List
   */
  public synchronized void clear()
  {
    data = (E[]) new Object[0];
  }

  /**
   * Add each element in the supplied Collection to this List. It is undefined
   * what happens if you modify the list while this is taking place; for
   * example, if the collection contains this list. c can contain objects of any
   * type, as well as null values.
   *
   * @param c
   *          a Collection containing elements to be added to this List
   * @return true if the list was modified, in other words c is not empty
   * @throws NullPointerException
   *           if c is null
   */
  public synchronized boolean addAll(Collection< ? extends E> c)
  {
    return addAll(data.length, c);
  }

  /**
   * Add all elements in the supplied collection, inserting them beginning at
   * the specified index. c can contain objects of any type, as well as null
   * values.
   *
   * @param index
   *          the index at which the elements will be inserted
   * @param c
   *          the Collection containing the elements to be inserted
   * @throws IndexOutOfBoundsException
   *           if index &lt; 0 || index &gt; 0
   * @throws NullPointerException
   *           if c is null
   */
  public synchronized boolean addAll(int index, Collection< ? extends E> c)
  {
    if (index < 0 || index > this.size())
      throw new IndexOutOfBoundsException("index = " +  index);

    int csize = c.size();
    if (csize == 0)
      return false;

    E[] data = this.data;
    Iterator<? extends E> itr = c.iterator();

    E[] newData = (E[]) new Object[data.length + csize];

    // avoid this call at all if we were asked to put the elements at the
    // beginning of our storage
    if (index != 0)
      System.arraycopy(data, 0, newData, 0, index);

    int itemsLeft = index;

    for (E value : c)
      newData[index++] = value;

    // now copy the remaining elements
    System.arraycopy(data, itemsLeft, newData, 0, data.length - itemsLeft);

    this.data = newData;

    return true;
  }

  /**
   * Adds an element if the list does not contains it already.
   *
   * @param val the element to add to the list.
   * @return true if the element was added, false otherwise.
   */
  public synchronized boolean addIfAbsent(E val)
  {
    if (contains(val))
      return false;
    add(val);
    return true;
  }

  /**
   * Adds all the element from the given collection that are not already
   * in this list.
   *
   * @param c the Collection containing the elements to be inserted
   * @return true the list internal storage changed as a result of this
   * operation, false otherwise.
   */
  public synchronized int addAllAbsent(Collection<? extends E> c)
  {
    int size = c.size();
    if (size == 0)
      return 0;

    E [] snapshot = this.data;
    E [] storage = (E[]) new Object[size];

    size = 0;
    for (E val : c)
      {
        if (!this.contains(val))
          storage[size++] = val;
      }

    if (size == 0)
      return 0;

    // append storage to data
    E [] newData = (E[]) new Object[snapshot.length + size];

    System.arraycopy(snapshot, 0, newData, 0, snapshot.length);
    System.arraycopy(storage, 0, newData, snapshot.length, size);

    this.data = newData;

    return size;
  }

  public String toString()
  {
    return Arrays.toString(this.data);
  }

  public boolean equals(Object o)
  {
    if (o == null)
      return false;

    if (this == o)
      return true;

    // let's see if 'o' is a list, if so, we need to compare the elements
    // as returned by the iterator
    if (o instanceof List)
      {
        List<?> source = (List<?>) o;

        if (source.size() != this.size())
          return false;

        Iterator<?> sourceIterator = source.iterator();
        for (E element : this)
          {
            if (!element.equals(sourceIterator.next()))
              return false;
          }

        return true;
      }

    return false;
  }

  public int hashCode()
  {
    // see http://java.sun.com/6/docs/api/java/util/List.html#hashcode()
    int hashcode = 1;
    for (E element : this)
      {
        hashcode = 31 * hashcode + (element == null ? 0 : element.hashCode());
      }
    return hashcode;
  }

  /**
   * Return an Iterator containing the elements of this list.
   * The Iterator uses a snapshot of the state of the internal storage
   * at the moment this method is called and does <strong>not</strong> support
   * update operations, so no synchronization is needed to traverse the
   * iterator.
   *
   * @return an Iterator containing the elements of this list in sequence.
   */
  public Iterator<E> iterator()
  {
    return new Iterator<E>()
    {
      E [] iteratorData = CopyOnWriteArrayList.this.data;
      int currentElement = 0;

      public boolean hasNext()
      {
        return (currentElement < iteratorData.length);
      }

      public E next()
      {
        return iteratorData[currentElement++];
      }

      public void remove()
      {
        throw new UnsupportedOperationException("updating of elements in " +
                                                "iterators is not supported " +
                                                "by this class");
      }
    };
  }

  /**
   * Return a ListIterator containing the elements of this list.
   * The Iterator uses a snapshot of the state of the internal storage
   * at the moment this method is called and does <strong>not</strong> support
   * update operations, so no synchronization is needed to traverse the
   * iterator.
   *
   * @return a ListIterator containing the elements of this list in sequence.
   */
  public ListIterator<E> listIterator()
  {
    return listIterator(0);
  }

  /**
   * Return a ListIterator over the elements of this list starting at
   * the specified index.  An initial call to {@code next()} will thus
   * return the element at {@code index}, while an initial call to
   * {@code previous()} will return the element at {@code index-1}.  The
   * Iterator uses a snapshot of the state of the internal storage
   * at the moment this method is called and does <strong>not</strong> support
   * update operations, so no synchronization is needed to traverse the
   * iterator.
   *
   * @param index the index at which to start iterating.
   * @return a ListIterator containing the elements of this list in sequence.
   */
  public ListIterator<E> listIterator(final int index)
  {
    if (index < 0 || index > size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:"
                                          + size());

    return new ListIterator<E>()
    {
      E [] iteratorData = CopyOnWriteArrayList.this.data;
      int currentElement = index;

      public void add(E o)
      {
        throw new UnsupportedOperationException("updating of elements in " +
                                                "iterators is not supported " +
                                                "by this class");
      }

      public boolean hasNext()
      {
        return (currentElement < iteratorData.length);
      }

      public boolean hasPrevious()
      {
        return (currentElement > 0);
      }

      public E next()
      {
        if (hasNext() == false)
          throw new java.util.NoSuchElementException();

        return iteratorData[currentElement++];
      }

      public int nextIndex()
      {
        return (currentElement + 1);
      }

      public E previous()
      {
        if (hasPrevious() == false)
          throw new java.util.NoSuchElementException();

        return iteratorData[--currentElement];
      }

      public int previousIndex()
      {
        return (currentElement - 1);
      }

      public void remove()
      {
        throw new UnsupportedOperationException("updating of elements in " +
                                                "iterators is not supported " +
                                                "by this class");
      }

      public void set(E o)
      {
        throw new UnsupportedOperationException("updating of elements in " +
                                                "iterators is not supported " +
                                                "by this class");
      }

    };
  }

  /**
   * Obtain a List view of a subsection of this list, from fromIndex
   * (inclusive) to toIndex (exclusive). If the two indices are equal, the
   * sublist is empty. The returned list should be modifiable if and only
   * if this list is modifiable. Changes to the returned list should be
   * reflected in this list. If this list is structurally modified in
   * any way other than through the returned list, the result of any subsequent
   * operations on the returned list is undefined.
   * <p>
   *
   * This implementation returns a subclass of AbstractList. It stores, in
   * private fields, the offset and size of the sublist, and the expected
   * modCount of the backing list. If the backing list implements RandomAccess,
   * the sublist will also.
   * <p>
   *
   * The subclass's <code>set(int, Object)</code>, <code>get(int)</code>,
   * <code>add(int, Object)</code>, <code>remove(int)</code>,
   * <code>addAll(int, Collection)</code> and
   * <code>removeRange(int, int)</code> methods all delegate to the
   * corresponding methods on the backing abstract list, after
   * bounds-checking the index and adjusting for the offset. The
   * <code>addAll(Collection c)</code> method merely returns addAll(size, c).
   * The <code>listIterator(int)</code> method returns a "wrapper object"
   * over a list iterator on the backing list, which is created with the
   * corresponding method on the backing list. The <code>iterator()</code>
   * method merely returns listIterator(), and the <code>size()</code> method
   * merely returns the subclass's size field.
   * <p>
   *
   * All methods first check to see if the actual modCount of the backing
   * list is equal to its expected value, and throw a
   * ConcurrentModificationException if it is not.
   *
   * @param fromIndex the index that the returned list should start from
   *        (inclusive)
   * @param toIndex the index that the returned list should go to (exclusive)
   * @return a List backed by a subsection of this list
   * @throws IndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; size()
   * @throws IndexOutOfBoundsException if fromIndex &gt; toIndex
   * @see ConcurrentModificationException
   * @see RandomAccess
   */
  public synchronized List<E> subList(int fromIndex, int toIndex)
  {
    // This follows the specification of AbstractList, but is inconsistent
    // with the one in List. Don't you love Sun's inconsistencies?
    if (fromIndex > toIndex)
      throw new IndexOutOfBoundsException(fromIndex + " > " + toIndex);
    if (fromIndex < 0 || toIndex > size())
      throw new IndexOutOfBoundsException();

    if (this instanceof RandomAccess)
      return new RandomAccessSubList<E>(this, fromIndex, toIndex);
    return new SubList<E>(this, fromIndex, toIndex);
  }

  /**
   * This class follows the implementation requirements set forth in
   * {@link AbstractList#subList(int, int)}. It matches Sun's implementation
   * by using a non-public top-level class in the same package.
   *
   * @author Original author unknown
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static class SubList<E>
    extends AbstractList<E>
  {
    // Package visible, for use by iterator.
    /** The original list. */
    final CopyOnWriteArrayList<E> backingList;
    /** The index of the first element of the sublist. */
    final int offset;
    /** The size of the sublist. */
    int size;
    /** The backing data */
    E[] data;

    /**
     * Construct the sublist.
     *
     * @param backing the list this comes from
     * @param fromIndex the lower bound, inclusive
     * @param toIndex the upper bound, exclusive
     */
    SubList(CopyOnWriteArrayList<E> backing, int fromIndex, int toIndex)
    {
      backingList = backing;
      data = backing.data;
      offset = fromIndex;
      size = toIndex - fromIndex;
    }

    /**
     * This method checks the two modCount fields to ensure that there has
     * not been a concurrent modification, returning if all is okay.
     *
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     */
    // This can be inlined. Package visible, for use by iterator.
    void checkMod()
    {
      if (data != backingList.data)
        throw new ConcurrentModificationException();
    }

    /**
     * This method checks that a value is between 0 and size (inclusive). If
     * it is not, an exception is thrown.
     *
     * @param index the value to check
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
     */
    // This will get inlined, since it is private.
    private void checkBoundsInclusive(int index)
    {
      if (index < 0 || index > size)
        throw new IndexOutOfBoundsException("Index: " + index +
                                            ", Size:" + size);
    }

    /**
     * This method checks that a value is between 0 (inclusive) and size
     * (exclusive). If it is not, an exception is thrown.
     *
     * @param index the value to check
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     */
    // This will get inlined, since it is private.
    private void checkBoundsExclusive(int index)
    {
      if (index < 0 || index >= size)
        throw new IndexOutOfBoundsException("Index: " + index +
                                            ", Size:" + size);
    }

    /**
     * Specified by AbstractList.subList to return the private field size.
     *
     * @return the sublist size
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     */
    public int size()
    {
      synchronized (backingList)
        {
          checkMod();
          return size;
        }
    }

    public void clear()
    {
      synchronized (backingList)
        {
          E[] snapshot = backingList.data;
          E[] newData = (E[]) new Object[snapshot.length - size];

          int toIndex = size + offset;

          System.arraycopy(snapshot, 0, newData, 0, offset);
          System.arraycopy(snapshot, toIndex, newData, offset,
                           snapshot.length - toIndex);

          backingList.data = newData;
          this.data = backingList.data;
          this.size = 0;
        }
    }

    /**
     * Specified by AbstractList.subList to delegate to the backing list.
     *
     * @param index the location to modify
     * @param o the new value
     * @return the old value
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws UnsupportedOperationException if the backing list does not
     *         support the set operation
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     * @throws ClassCastException if o cannot be added to the backing list due
     *         to its type
     * @throws IllegalArgumentException if o cannot be added to the backing list
     *         for some other reason
     */
    public E set(int index, E o)
    {
      synchronized (backingList)
        {
          checkMod();
          checkBoundsExclusive(index);

          E el =  backingList.set(index + offset, o);
          this.data = backingList.data;

          return el;
        }
    }

    /**
     * Specified by AbstractList.subList to delegate to the backing list.
     *
     * @param index the location to get from
     * @return the object at that location
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     */
    public E get(int index)
    {
      synchronized (backingList)
      {
        checkMod();
        checkBoundsExclusive(index);

        return backingList.get(index + offset);
      }
    }

    /**
     * Specified by AbstractList.subList to delegate to the backing list.
     *
     * @param index the index to insert at
     * @param o the object to add
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
     * @throws UnsupportedOperationException if the backing list does not
     *         support the add operation.
     * @throws ClassCastException if o cannot be added to the backing list due
     *         to its type.
     * @throws IllegalArgumentException if o cannot be added to the backing
     *         list for some other reason.
     */
    public void add(int index, E o)
    {
      synchronized (backingList)
      {
        checkMod();
        checkBoundsInclusive(index);

        backingList.add(index + offset, o);

        this.data = backingList.data;
        size++;
      }
    }

    /**
     * Specified by AbstractList.subList to delegate to the backing list.
     *
     * @param index the index to remove
     * @return the removed object
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
     * @throws UnsupportedOperationException if the backing list does not
     *         support the remove operation
     */
    public E remove(int index)
    {
      synchronized (backingList)
      {
        checkMod();
        checkBoundsExclusive(index);
        E o = backingList.remove(index + offset);

        this.data = backingList.data;
        size--;

        return o;
      }
    }

    /**
     * Specified by AbstractList.subList to delegate to the backing list.
     *
     * @param index the location to insert at
     * @param c the collection to insert
     * @return true if this list was modified, in other words, c is non-empty
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
     * @throws UnsupportedOperationException if this list does not support the
     *         addAll operation
     * @throws ClassCastException if some element of c cannot be added to this
     *         list due to its type
     * @throws IllegalArgumentException if some element of c cannot be added
     *         to this list for some other reason
     * @throws NullPointerException if the specified collection is null
     */
    public boolean addAll(int index, Collection<? extends E> c)
    {
      synchronized (backingList)
      {
        checkMod();
        checkBoundsInclusive(index);
        int csize = c.size();
        boolean result = backingList.addAll(offset + index, c);

        this.data = backingList.data;
        size += csize;

        return result;
      }
    }

    /**
     * Specified by AbstractList.subList to return addAll(size, c).
     *
     * @param c the collection to insert
     * @return true if this list was modified, in other words, c is non-empty
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws UnsupportedOperationException if this list does not support the
     *         addAll operation
     * @throws ClassCastException if some element of c cannot be added to this
     *         list due to its type
     * @throws IllegalArgumentException if some element of c cannot be added
     *         to this list for some other reason
     * @throws NullPointerException if the specified collection is null
     */
    public boolean addAll(Collection<? extends E> c)
    {
      synchronized (backingList)
      {
        return addAll(size, c);
      }
    }

    /**
     * Specified by AbstractList.subList to return listIterator().
     *
     * @return an iterator over the sublist
     */
    public Iterator<E> iterator()
    {
      return listIterator();
    }

    /**
     * Specified by AbstractList.subList to return a wrapper around the
     * backing list's iterator.
     *
     * @param index the start location of the iterator
     * @return a list iterator over the sublist
     * @throws ConcurrentModificationException if the backing list has been
     *         modified externally to this sublist
     * @throws IndexOutOfBoundsException if the value is out of range
     */
    public ListIterator<E> listIterator(final int index)
    {
      checkMod();
      checkBoundsInclusive(index);

      return new ListIterator<E>()
      {
        private final ListIterator<E> i =
          backingList.listIterator(index + offset);
        private int position = index;

        /**
         * Tests to see if there are any more objects to
         * return.
         *
         * @return True if the end of the list has not yet been
         *         reached.
         */
        public boolean hasNext()
        {
          return position < size;
        }

        /**
         * Tests to see if there are objects prior to the
         * current position in the list.
         *
         * @return True if objects exist prior to the current
         *         position of the iterator.
         */
        public boolean hasPrevious()
        {
          return position > 0;
        }

        /**
         * Retrieves the next object from the list.
         *
         * @return The next object.
         * @throws NoSuchElementException if there are no
         *         more objects to retrieve.
         * @throws ConcurrentModificationException if the
         *         list has been modified elsewhere.
         */
        public E next()
        {
          if (position == size)
            throw new NoSuchElementException();
          position++;
          return i.next();
        }

        /**
         * Retrieves the previous object from the list.
         *
         * @return The next object.
         * @throws NoSuchElementException if there are no
         *         previous objects to retrieve.
         * @throws ConcurrentModificationException if the
         *         list has been modified elsewhere.
         */
        public E previous()
        {
          if (position == 0)
            throw new NoSuchElementException();
          position--;
          return i.previous();
        }

        /**
         * Returns the index of the next element in the
         * list, which will be retrieved by <code>next()</code>
         *
         * @return The index of the next element.
         */
        public int nextIndex()
        {
          return i.nextIndex() - offset;
        }

        /**
         * Returns the index of the previous element in the
         * list, which will be retrieved by <code>previous()</code>
         *
         * @return The index of the previous element.
         */
        public int previousIndex()
        {
          return i.previousIndex() - offset;
        }

        /**
         * Removes the last object retrieved by <code>next()</code>
         * from the list, if the list supports object removal.
         *
         * @throws IllegalStateException if the iterator is positioned
         *         before the start of the list or the last object has already
         *         been removed.
         * @throws UnsupportedOperationException if the list does
         *         not support removing elements.
         */
        public void remove()
        {
          throw new UnsupportedOperationException("Modification not supported " +
              "on CopyOnWriteArrayList iterators");
        }

        /**
         * Replaces the last object retrieved by <code>next()</code>
         * or <code>previous</code> with o, if the list supports object
         * replacement and an add or remove operation has not already
         * been performed.
         *
         * @throws IllegalStateException if the iterator is positioned
         *         before the start of the list or the last object has already
         *         been removed.
         * @throws UnsupportedOperationException if the list doesn't support
         *         the addition or removal of elements.
         * @throws ClassCastException if the type of o is not a valid type
         *         for this list.
         * @throws IllegalArgumentException if something else related to o
         *         prevents its addition.
         * @throws ConcurrentModificationException if the list
         *         has been modified elsewhere.
         */
        public void set(E o)
        {
          throw new UnsupportedOperationException("Modification not supported " +
              "on CopyOnWriteArrayList iterators");
        }

        /**
         * Adds the supplied object before the element that would be returned
         * by a call to <code>next()</code>, if the list supports addition.
         *
         * @param o The object to add to the list.
         * @throws UnsupportedOperationException if the list doesn't support
         *         the addition of new elements.
         * @throws ClassCastException if the type of o is not a valid type
         *         for this list.
         * @throws IllegalArgumentException if something else related to o
         *         prevents its addition.
         * @throws ConcurrentModificationException if the list
         *         has been modified elsewhere.
         */
        public void add(E o)
        {
          throw new UnsupportedOperationException("Modification not supported " +
              "on CopyOnWriteArrayList iterators");
        }
      };
    }
  } // class SubList

  /**
   * This class is a RandomAccess version of SubList, as required by
   * {@link AbstractList#subList(int, int)}.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private static final class RandomAccessSubList<E> extends SubList<E>
    implements RandomAccess
  {
    /**
     * Construct the sublist.
     *
     * @param backing the list this comes from
     * @param fromIndex the lower bound, inclusive
     * @param toIndex the upper bound, exclusive
     */
    RandomAccessSubList(CopyOnWriteArrayList<E> backing, int fromIndex, int toIndex)
    {
      super(backing, fromIndex, toIndex);
    }
  } // class RandomAccessSubList

  /**
   * Serializes this object to the given stream.
   *
   * @param s
   *          the stream to write to
   * @throws IOException
   *           if the underlying stream fails
   * @serialData the size field (int), the length of the backing array (int),
   *             followed by its elements (Objects) in proper order.
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    // The 'size' field.
    s.defaultWriteObject();
    // We serialize unused list entries to preserve capacity.
    int len = data.length;
    s.writeInt(len);
    // it would be more efficient to just write "size" items,
    // this need readObject read "size" items too.
    for (int i = 0; i < data.length; i++)
      s.writeObject(data[i]);
  }

  /**
   * Deserializes this object from the given stream.
   *
   * @param s
   *          the stream to read from
   * @throws ClassNotFoundException
   *           if the underlying stream fails
   * @throws IOException
   *           if the underlying stream fails
   * @serialData the size field (int), the length of the backing array (int),
   *             followed by its elements (Objects) in proper order.
   */
  private void readObject(ObjectInputStream s) throws IOException,
      ClassNotFoundException
  {
    // the `size' field.
    s.defaultReadObject();
    int capacity = s.readInt();
    data = (E[]) new Object[capacity];
    for (int i = 0; i < capacity; i++)
      data[i] = (E) s.readObject();
  }

  static final boolean equals(Object o1, Object o2)
  {
    return o1 == null ? o2 == null : o1.equals(o2);
  }

  Object[] getArray()
  {
    return data;
  }
}
