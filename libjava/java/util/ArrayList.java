/* ArrayList.java -- JDK1.2's answer to Vector; this is an array-backed
   implementation of the List interface
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


package java.util;

import java.lang.reflect.Array;
import java.io.Serializable;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * An array-backed implementation of the List interface.  ArrayList
 * performs well on simple tasks:  random access into a list, appending
 * to or removing from the end of a list, checking the size, &c.
 *
 * @author        Jon A. Zeppieri
 * @see           java.util.AbstractList
 * @see           java.util.List
 */
public class ArrayList extends AbstractList
  implements List, Cloneable, Serializable
{
  /** the default capacity for new ArrayLists */
  private static final int DEFAULT_CAPACITY = 16;

  /** the number of elements in this list */
  int size;

  /** where the data is stored */
  transient Object[] data;

  /** 
   * Construct a new ArrayList with the supplied initial capacity. 
   *
   * @param capacity Initial capacity of this ArrayList
   */
  public ArrayList(int capacity)
  {
    data = new Object[capacity];
  }


  /**
   * Construct a new ArrayList with the default capcity 
   */
  public ArrayList()
  {
    this(DEFAULT_CAPACITY);
  }

  /** 
   * Construct a new ArrayList, and initialize it with the elements
   * in the supplied Collection; Sun specs say that the initial 
   * capacity is 110% of the Collection's size.
   *
   * @param c the collection whose elements will initialize this list
   */
  public ArrayList(Collection c)
  {
    this((int) (c.size() * 1.1));
    addAll(c);
  }

  /**
   * Guarantees that this list will have at least enough capacity to
   * hold minCapacity elements. 
   *
   * @specnote This implementation will grow the list to 
   *   max(current * 2, minCapacity) if (minCapacity > current). The JCL says
   *   explictly that "this method increases its capacity to minCap", while
   *   the JDK 1.3 online docs specify that the list will grow to at least the
   *   size specified.
   * @param minCapacity the minimum guaranteed capacity
   */
  public void ensureCapacity(int minCapacity)
  {
    Object[] newData;
    int current = data.length;

    if (minCapacity > current)
      {
	newData = new Object[Math.max((current * 2), minCapacity)];
	System.arraycopy(data, 0, newData, 0, size);
	data = newData;
      }
  }

  /**
   * Appends the supplied element to the end of this list.
   *
   * @param       e      the element to be appended to this list
   */
  public boolean add(Object e)
  {
    modCount++;
    if (size == data.length)
      ensureCapacity(size + 1);
    data[size++] = e;
    return true;
  }

  /**
   * Retrieves the element at the user-supplied index.
   *
   * @param    index        the index of the element we are fetching
   * @throws   IndexOutOfBoundsException  (iIndex < 0) || (iIndex >= size())
   */
  public Object get(int index)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    return data[index];
  }

  /**
   * Returns the number of elements in this list 
   */
  public int size()
  {
    return size;
  }

  /**
   * Removes the element at the user-supplied index
   *
   * @param     iIndex      the index of the element to be removed
   * @return    the removed Object
   * @throws    IndexOutOfBoundsException  (iIndex < 0) || (iIndex >= size())
   */
  public Object remove(int index)
  {
    modCount++;
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    Object r = data[index];
    if (index != --size)
      System.arraycopy(data, (index + 1), data, index, (size - index));
    data[size] = null;
    return r;
  }

  /**
   * Removes all elements in the half-open interval [iFromIndex, iToIndex).
   *
   * @param     fromIndex   the first index which will be removed
   * @param     toIndex     one greater than the last index which will be 
   *                         removed
   */
  protected void removeRange(int fromIndex, int toIndex)
  {
    modCount++;
    if (fromIndex != toIndex)
      {
	System.arraycopy(data, toIndex, data, fromIndex, size - toIndex);
	size -= (toIndex - fromIndex);
      }
  }

  /**
   * Adds the supplied element at the specified index, shifting all
   * elements currently at that index or higher one to the right.
   *
   * @param     index      the index at which the element is being added
   * @param     e          the item being added
   */
  public void add(int index, Object e)
  {
    modCount++;
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    if (size == data.length)
      ensureCapacity(size + 1);
    if (index != size)
      System.arraycopy(data, index, data, index + 1, size - index);    
    data[index] = e;
    size++;
  }

  /** 
   * Add each element in the supplied Collection to this List.
   *
   * @param        c          a Collection containing elements to be 
   *                          added to this List
   */
  public boolean addAll(Collection c)
  {
    return addAll(size, c);
  }

  /** 
   * Add all elements in the supplied collection, inserting them beginning
   * at the specified index.
   *
   * @param     index       the index at which the elements will be inserted
   * @param     c           the Collection containing the elements to be
   *                        inserted
   */
  public boolean addAll(int index, Collection c)
  {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    modCount++;
    Iterator itr = c.iterator();
    int csize = c.size();

    if (csize + size > data.length)
      ensureCapacity(size + csize);
    int end = index + csize;
    if (size > 0 && index != size)
      System.arraycopy(data, index, data, end, csize);
    size += csize;
    for (; index < end; index++)
      {
        data[index] = itr.next();
      }
    return (csize > 0);
  }

  /**
   * Creates a shallow copy of this ArrayList
   */
  public Object clone()
  {
    ArrayList clone = null;
    try
      {
	clone = (ArrayList) super.clone();
	clone.data = new Object[data.length];
	System.arraycopy(data, 0, clone.data, 0, size);
      }
    catch (CloneNotSupportedException e) {}
    return clone;
  }

  /** 
   * Returns true iff oElement is in this ArrayList.
   *
   * @param     e     the element whose inclusion in the List is being
   *                  tested
   */
  public boolean contains(Object e)
  {
    return (indexOf(e) != -1);
  }

  /**
   * Returns the lowest index at which oElement appears in this List, or 
   * -1 if it does not appear.
   *
   * @param    e       the element whose inclusion in the List is being
   *                   tested
   */
  public int indexOf(Object e)
  {
    for (int i = 0; i < size; i++)
      {
	if (e == null ? data[i] == null : e.equals(data[i]))
	  return i;
      }
    return -1;
  }

  /**
   * Returns the highest index at which oElement appears in this List, or 
   * -1 if it does not appear.
   *
   * @param    e       the element whose inclusion in the List is being
   *                   tested
   */
  public int lastIndexOf(Object e)
  {
    int i;

    for (i = size - 1; i >= 0; i--)
      {
	if (e == null ? data[i] == null : e.equals(data[i]))
	  return i;
      }
    return -1;
  }

  /**
   * Removes all elements from this List
   */
  public void clear()
  {
    modCount++;
    for (int i = 0; i < size; i++)
      {
	data[i] = null;
      }    
    size = 0;
  }

  /**
   * Sets the element at the specified index.
   *
   * @param     index   the index at which the element is being set
   * @param     e       the element to be set
   * @return    the element previously at the specified index, or null if
   *            none was there
   */
  public Object set(int index, Object e)
  {
    Object result;
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    result = data[index];
    // SEH: no structural change, so don't update modCount
    data[index] = e;
    return result;
  }

  /**
   * Returns an Object Array containing all of the elements in this ArrayList
   */
  public Object[] toArray()
  {
    Object[] array = new Object[size];
    System.arraycopy(data, 0, array, 0, size);
    return array;
  }

  /**
   * Returns an Array whose component type is the runtime component type of
   * the passed-in Array.  The returned Array is populated with all of the
   * elements in this ArrayList.  If the passed-in Array is not large enough
   * to store all of the elements in this List, a new Array will be created 
   * and returned; if the passed-in Array is <i>larger</i> than the size
   * of this List, then size() index will be set to null.
   *
   * @param      array      the passed-in Array
   */
  public Object[] toArray(Object[] array)
  {
    if (array.length < size)
      array = (Object[]) Array.newInstance(array.getClass().getComponentType(), 
        				   size);
    else if (array.length > size)
      array[size] = null;
    System.arraycopy(data, 0, array, 0, size);
    return array;
  }

  /**
   * Trims the capacity of this List to be equal to its size; 
   * a memory saver.   
   */
  public void trimToSize()
  {
    // not a structural change from the perspective of iterators on this list, 
    // so don't update modCount
    Object[] newData = new Object[size];
    System.arraycopy(data, 0, newData, 0, size);
    data = newData;
  }

  private void writeObject(ObjectOutputStream out) throws IOException
  {
    int i;

    // The 'size' field.
    out.defaultWriteObject();

    // FIXME: Do we really want to serialize unused list entries??
    out.writeInt(data.length);
    for (i = 0; i < data.length; i++)
      out.writeObject(data[i]);
  }

  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    int i;
    int capacity;

    // the `size' field.
    in.defaultReadObject();

    capacity = in.readInt();
    data = new Object[capacity];

    for (i = 0; i < capacity; i++)
      data[i] = in.readObject();
  }
}
