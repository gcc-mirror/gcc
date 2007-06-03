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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.RandomAccess;

/** @since 1.5 */
public class CopyOnWriteArrayList<E> extends AbstractList<E> implements
    List<E>, RandomAccess, Cloneable, Serializable
{
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
   * Returns true iff element is in this ArrayList.
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
        clone.data = (E[]) data.clone();
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
    E[] data = this.data;
    E[] newData = (E[]) new Object[data.length - 1];
    if (index > 0)
      System.arraycopy(data, 0, newData, 0, index - 1);
    System.arraycopy(data, index + 1, newData, index,
                     data.length - index - 1);
    E r = data[index];
    this.data = newData;
    return r;
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
    E[] data = this.data;
    Iterator<? extends E> itr = c.iterator();
    int csize = c.size();
    if (csize == 0)
      return false;

    E[] newData = (E[]) new Object[data.length + csize];
    System.arraycopy(data, 0, newData, 0, data.length);
    int end = data.length;
    for (E value : c)
      newData[end++] = value;
    this.data = newData;
    return true;
  }
  
  public synchronized boolean addIfAbsent(E val)
  {
    if (contains(val))
      return false;
    add(val);
    return true;
  }

  public synchronized int addAllAbsent(Collection<? extends E> c)
  {
    int result = 0;
    for (E val : c)
      {
        if (addIfAbsent(val))
          ++result;
      }
    return result;
  }

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
