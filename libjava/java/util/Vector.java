/* Vector.java -- Class that provides growable arrays.
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

/**
 * the <b>Vector</b> classes implements growable arrays of Objects.
 * You can access elements in a Vector with an index, just as you
 * can in a built in array, but Vectors can grow and shrink to accomodate
 * more or fewer objects.  
 *
 * Vectors try to mantain efficiency in growing by having a
 * <b>capacityIncrement</b> that can be specified at instantiation.
 * When a Vector can no longer hold a new Object, it grows by the amount
 * in <b>capacityIncrement</b>.  
 *
 * Vector implements the JDK 1.2 List interface, and is therefor a fully
 * compliant Collection object.
 *
 * @specnote The JCL claims that various methods in this class throw
 * IndexOutOfBoundsException, which would be consistent with other collections
 * classes. ArrayIndexOutOfBoundsException is actually thrown, per the online 
 * docs, even for List method implementations.
 * 
 * @author Scott G. Miller
 */
public class Vector extends AbstractList 
  implements List, Cloneable, Serializable
{
  /**
   * The amount the Vector's internal array should be increased in size when
   * a new element is added that exceeds the current size of the array,
   * or when {@link #ensureCapacity} is called.
   * @serial
   */
  protected int capacityIncrement = 0;

  /**
   * The number of elements currently in the vector, also returned by
   * {@link #size}.
   * @serial
   */
  protected int elementCount = 0;

  /**
   * The internal array used to hold members of a Vector
   * @serial
   */
  protected Object[] elementData;

  private static final long serialVersionUID = -2767605614048989439L;

  /**
   * Constructs an empty vector with an initial size of 10, and
   * a capacity increment of 0
   */
  public Vector()
  {
    this(10);
  }

  /**
   * Constructs a vector containing the contents of Collection, in the
   * order given by the collection
   *
   * @param c A collection of elements to be added to the newly constructed
   * vector
   */
  public Vector(Collection c)
  {
    int csize = c.size();
    elementData = new Object[csize];
    elementCount = csize;
    Iterator itr = c.iterator();
    for (int i = 0; i < csize; i++)
      {
	elementData[i] = itr.next();
      }
  }

  /**
   * Constructs a Vector with the initial capacity and capacity 
   * increment specified
   *
   * @param initialCapacity The initial size of the Vector's internal
   * array
   * @param capacityIncrement The amount the internal array should be
   * increased if necessary
   */
  public Vector(int initialCapacity, int capacityIncrement)
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException();
    elementData = new Object[initialCapacity];
    this.capacityIncrement = capacityIncrement;
  }

  /**
   * Constructs a Vector with the initial capacity specified
   *
   * @param initialCapacity The initial size of the Vector's internal array
   */
  public Vector(int initialCapacity)
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException();
    elementData = new Object[initialCapacity];
  }

  /**
   * Copies the contents of a provided array into the Vector.  If the 
   * array is too large to fit in the Vector, an ArrayIndexOutOfBoundsException
   * is thrown.  Old elements in the Vector are overwritten by the new
   * elements
   *
   * @param anArray An array from which elements will be copied into the Vector
   * 
   * @throws ArrayIndexOutOfBoundsException the array being copied
   * is larger than the Vectors internal data array
   */
  public synchronized void copyInto(Object[] anArray)
  {
    System.arraycopy(elementData, 0, anArray, 0, elementCount);
  }

  /**
   * Trims the Vector down to size.  If the internal data array is larger
   * than the number of Objects its holding, a new array is constructed
   * that precisely holds the elements.  
   */
  public synchronized void trimToSize()
  {
    // Don't bother checking for the case where size() == the capacity of the
    // vector since that is a much less likely case; it's more efficient to
    // not do the check and lose a bit of performance in that infrequent case

    Object[] newArray = new Object[elementCount];
    System.arraycopy(elementData, 0, newArray, 0, elementCount);
    elementData = newArray;
  }

  /**
   * Ensures that <b>minCapacity</b> elements can fit within this Vector.
   * If it cannot hold this many elements, the internal data array is expanded
   * in the following manner.  If the current size plus the capacityIncrement
   * is sufficient, the internal array is expanded by capacityIncrement.  
   * If capacityIncrement is non-positive, the size is doubled.  If 
   * neither is sufficient, the internal array is expanded to size minCapacity
   *
   * @param minCapacity The minimum capacity the internal array should be
   * able to handle after executing this method
   */
  public synchronized void ensureCapacity(int minCapacity)
  {
    modCount++;
    if (elementData.length >= minCapacity)
      return;

    int newCapacity; 
    if (capacityIncrement <= 0)
      newCapacity = elementData.length * 2;
    else
      newCapacity = elementData.length + capacityIncrement;
      
    Object[] newArray = new Object[Math.max(newCapacity, minCapacity)];

    System.arraycopy(elementData, 0, newArray, 0, elementData.length);
    elementData = newArray;
  }

  /**
   * Explicitly sets the size of the internal data array, copying the 
   * old values to the new internal array.  If the new array is smaller
   * than the old one, old values that don't fit are lost. If the new size
   * is larger than the old one, the vector is padded with null entries.
   *
   * @param newSize The new size of the internal array
   */
  public synchronized void setSize(int newSize)
  {
    modCount++;
    Object[] newArray = new Object[newSize];
    System.arraycopy(elementData, 0, newArray, 0, 
                     Math.min(newSize, elementCount));
    elementCount = newSize;
    elementData = newArray;
  }

  /**
   * Returns the size of the internal data array (not the amount of elements
   * contained in the Vector)
   *
   * @returns capacity of the internal data array
   */
  public int capacity()
  {
    return elementData.length;
  }

  /**
   * Returns the number of elements stored in this Vector
   *
   * @returns the number of elements in this Vector
   */
  public int size()
  {
    return elementCount;
  }

  /**
   * Returns true if this Vector is empty, false otherwise
   *
   * @returns true if the Vector is empty, false otherwise
   */
  public boolean isEmpty()
  {
    return elementCount == 0;
  }

  /**
   * Searches the vector starting at <b>index</b> for object <b>elem</b>
   * and returns the index of the first occurence of this Object.  If
   * the object is not found, -1 is returned
   *
   * @param e The Object to search for
   * @param index Start searching at this index
   * @returns The index of the first occurence of <b>elem</b>, or -1
   * if it is not found
   */
  public synchronized int indexOf(Object e, int index)
  {
    for (int i = index; i < elementCount; i++)
      {
	if (e == null ? elementData[i] == null : e.equals(elementData[i]))
	  return i;
      }
    return -1;
  }

  /**
   * Returns the first occurence of <b>elem</b> in the Vector, or -1 if
   * <b>elem</b> is not found.
   *
   * @param elem The object to search for
   * @returns The index of the first occurence of <b>elem</b> or -1 if 
   * not found
   */
  public int indexOf(Object elem)
  {
    return indexOf(elem, 0);
  }

  /**
   * Returns true if <b>elem</b> is contained in this Vector, false otherwise.
   *
   * @param elem The element to check
   * @returns true if the object is contained in this Vector, false otherwise
   */
  public boolean contains(Object elem)
  {
    return indexOf(elem, 0) != -1;
  }

  /**
   * Returns the index of the first occurence of <b>elem</b>, when searching
   * backwards from <b>index</b>.  If the object does not occur in this Vector,
   * -1 is returned.
   *
   * @param eThe object to search for
   * @param index The index to start searching in reverse from
   * @returns The index of the Object if found, -1 otherwise
   */
  public synchronized int lastIndexOf(Object e, int index)
  {
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);

    for (int i = index; i >= 0; i--)
      {
	if (e == null ? elementData[i] == null : e.equals(elementData[i]))
	  return i;
      }
    return -1;
  }

  /**
   * Returns the last index of <b>elem</b> within this Vector, or -1
   * if the object is not within the Vector
   *
   * @param elem The object to search for
   * @returns the last index of the object, or -1 if not found
   */
  public int lastIndexOf(Object elem)
  {
    return lastIndexOf(elem, elementCount - 1);
  }

  /**
   * Returns the Object stored at <b>index</b>.  If index is out of range
   * an ArrayIndexOutOfBoundsException is thrown.
   *
   * @param index the index of the Object to retrieve
   * @returns The object at <b>index</b>
   * @throws ArrayIndexOutOfBoundsException <b>index</b> is
   * larger than the Vector
   */
  public synchronized Object elementAt(int index)
  {
    //Within the bounds of this Vector does not necessarily mean within 
    //the bounds of the internal array
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);

    return elementData[index];
  }

  /**
   * Returns the first element in the Vector.  If there is no first Object 
   * (The vector is empty), a NoSuchElementException is thrown.
   *
   * @returns The first Object in the Vector
   * @throws NoSuchElementException the Vector is empty
   */
  public synchronized Object firstElement()
  {
    if (elementCount == 0)
      throw new NoSuchElementException();

    return elementAt(0);
  }

  /**
   * Returns the last element in the Vector.  If the Vector has no last element
   * (The vector is empty), a NoSuchElementException is thrown.
   *
   * @returns The last Object in the Vector
   * @throws NoSuchElementException the Vector is empty
   */
  public synchronized Object lastElement()
  {
    if (elementCount == 0)
      throw new NoSuchElementException();

    return elementAt(elementCount - 1);
  }

  /**
   * Places <b>obj</b> at <b>index</b> within the Vector.  If <b>index</b>
   * refers to an index outside the Vector, an ArrayIndexOutOfBoundsException
   * is thrown.  
   * 
   * @param obj The object to store
   * @param index The position in the Vector to store the object
   * @throws ArrayIndexOutOfBoundsException the index is out of range
   */
  public synchronized void setElementAt(Object obj, int index)
  {
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);

    elementData[index] = obj;
  }

  /**
   * Puts <b>element</b> into the Vector at position <b>index</b> and returns
   * the Object that previously occupied that position.
   *
   * @param index The index within the Vector to place the Object
   * @param element The Object to store in the Vector
   * @returns The previous object at the specified index
   * @throws ArrayIndexOutOfBoundsException the index is out of range
   *
   */
  public synchronized Object set(int index, Object element)
  {
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);

    Object temp = elementData[index];
    elementData[index] = element;
    return temp;
  }

  /**
   * Removes the element at <b>index</b>, and shifts all elements at
   * positions greater than index to their index - 1.  
   *
   * @param index The index of the element to remove
   */
  public synchronized void removeElementAt(int index)
  {
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);

    modCount++;
    elementCount--;
    if (index < elementCount)
      System.arraycopy(elementData, index + 1, elementData, index,
		       elementCount - index);
    //Delete the last element (which has been copied back one index)
    //so it can be garbage collected;
    elementData[elementCount] = null;
  }

  /**
   * Inserts a new element into the Vector at <b>index</b>.  Any elements
   * at or greater than index are shifted up one position.
   *
   * @param obj The object to insert
   * @param index The index at which the object is inserted
   */
  public void insertElementAt(Object obj, int index)
  {
    if (index > elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " > " + elementCount);

    if (elementCount == elementData.length)
      ensureCapacity(++elementCount);
    modCount++;
    System.arraycopy(elementData, index, elementData, index + 1,
		     elementCount - 1 - index);
    elementData[index] = obj;
  }

  /**
   * Adds an element to the Vector at the end of the Vector.  If the vector
   * cannot hold the element with its present capacity, its size is increased
   * based on the same rules followed if ensureCapacity was called with the
   * argument currentSize+1.
   *
   * @param obj The object to add to the Vector
   */
  public synchronized void addElement(Object obj)
  {
    if (elementCount == elementData.length)
      ensureCapacity(++elementCount);
    modCount++;
    elementData[elementCount++] = obj;
  }

  /**
   * Removes the first occurence of the given object from the Vector.
   * If such a remove was performed (the object was found), true is returned.
   * If there was no such object, false is returned.
   *
   * @param obj The object to remove from the Vector
   * @returns true if the Object was in the Vector, false otherwise
   */
  public synchronized boolean removeElement(Object obj)
  {
    int idx = indexOf(obj);
    if (idx != -1)
      {
	removeElementAt(idx);
	return true;
      }
    return false;
  }

  /**
   * Removes all elements from the Vector.  Note that this does not
   * resize the internal data array.
   */
  public synchronized void removeAllElements()
  {
    modCount++;
    if (elementCount == 0)
      return;

    for (int i = elementCount - 1; i >= 0; --i)
      {
	elementData[i] = null;
      }
    elementCount = 0;
  }

  /**
   * Creates a new Vector with the same contents as this one.
   */
  public synchronized Object clone()
  {
    try
      {
	Vector clone = (Vector) super.clone();
	clone.elementData = (Object[]) elementData.clone();
	return clone;
      }
    catch (CloneNotSupportedException ex)
      {
	throw new InternalError(ex.toString());
      }
  }

  /**
   * Returns an Object array with the contents of this Vector, in the order
   * they are stored within this Vector.  Note that the Object array returned 
   * is not the internal data array, and that it holds only the elements 
   * within the Vector.  This is similar to creating a new Object[] with the 
   * size of this Vector, then calling Vector.copyInto(yourArray).
   *
   * @returns An Object[] containing the contents of this Vector in order
   *
   */
  public synchronized Object[] toArray()
  {
    Object[] newArray = new Object[elementCount];
    copyInto(newArray);
    return newArray;
  }

  /**
   * Returns an array containing the contents of this Vector.  
   * If the provided array is large enough, the contents are copied
   * into that array, and a null is placed in the position size(). 
   * In this manner, you can obtain the size of a Vector by the position
   * of the null element.  If the type of the provided array cannot 
   * hold the elements, an ArrayStoreException is thrown.
   * 
   * If the provided array is not large enough,
   * a new one is created with the contents of the Vector, and no null 
   * element.  The new array is of the same runtime type as the provided
   * array.
   *
   *
   * @param array An array to copy the Vector into if large enough
   * @returns An array with the contents of this Vector in order
   * @throws ArrayStoreException the runtime type of the provided array
   * cannot hold the elements of the Vector
   */
  public synchronized Object[] toArray(Object[] array)
  {
    if (array.length < elementCount)
      array = (Object[]) Array.newInstance(array.getClass().getComponentType(), 
        				   elementCount);
    else if (array.length > elementCount)
      array[elementCount] = null;
    System.arraycopy(elementData, 0, array, 0, elementCount);
    return array;
  }

  /**
   * Returns the element at position <b>index</b>
   *
   * @param index the position from which an element will be retrieved
   * @throws ArrayIndexOutOfBoundsException the index is not within the 
   * range of the Vector
   */
  public synchronized Object get(int index)
  {
    return elementAt(index);
  }

  /**
   * Removes the given Object from the Vector.  If it exists, true
   * is returned, if not, false is returned.
   *
   * @param o The object to remove from the Vector
   * @returns true if the Object existed in the Vector, false otherwise
   */
  public boolean remove(Object o)
  {
    return removeElement(o);
  }

  /**
   * Adds an object to the Vector.
   *
   * @param o The element to add to the Vector
   */
  public synchronized boolean add(Object o)
  {
    addElement(o);
    return true;
  }

  /**
   * Adds an object at the specified index.  Elements at or above
   * index are shifted up one position.
   *
   * @param index The index at which to add the element
   * @param element The element to add to the Vector
   */
  public void add(int index, Object element)
  {
    insertElementAt(element, index);
  }

  /**
   * Removes the element at the specified index, and returns it.
   *
   * @param index The position from which to remove the element
   * @returns The object removed
   * @throws ArrayIndexOutOfBoundsException the index was out of the range
   * of the Vector
   */
  public synchronized Object remove(int index)
  {
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);
  
    Object temp = elementData[index];
    removeElementAt(index);
    return temp;
  }

  /**
   * Clears all elements in the Vector and sets its size to 0
   */
  public void clear()
  {
    removeAllElements();
  }

  public synchronized boolean containsAll(Collection c)
  {
    Iterator itr = c.iterator();
    int size = c.size();
    for (int pos = 0; pos < size; pos++)
      {
	if (!contains(itr.next()))
	  return false;
      }
    return true;
  }

  public synchronized boolean addAll(Collection c)
  {
    return addAll(elementCount, c);
  }
  
  public synchronized boolean removeAll(Collection c)
  {
    return super.removeAll(c);
  }
  
  public synchronized boolean retainAll(Collection c)
  {
    return super.retainAll(c);
  }

  public synchronized boolean addAll(int index, Collection c)
  {
    if (index < 0 || index > elementCount)
      throw new ArrayIndexOutOfBoundsException(index);
    modCount++;
    Iterator itr = c.iterator();
    int csize = c.size();

    ensureCapacity(elementCount + csize);
    int end = index + csize;
    if (elementCount > 0 && index != elementCount)
      System.arraycopy(elementData, index, elementData, end, csize);
    elementCount += csize;
    for (; index < end; index++)
      {
        elementData[index] = itr.next();
      }
    return (csize > 0);  
  }

  public synchronized boolean equals(Object c)
  {
    return super.equals(c);
  }

  public synchronized int hashCode()
  {
    return super.hashCode();
  }

  /**
   * Returns a string representation of this Vector in the form 
   * [element0, element1, ... elementN]
   *
   * @returns the String representation of this Vector
   */
  public synchronized String toString()
  {
    String r = "[";
    for (int i = 0; i < elementCount; i++)
      {
	r += elementData[i];
	if (i < elementCount - 1)
	  r += ", ";
      }
    r += "]";
    return r;
  }

  /**
   * Returns an Enumeration of the elements of this List.
   * The Enumeration returned is compatible behavior-wise with
   * the 1.1 elements() method, in that it does not check for
   * concurrent modification.
   *
   * @returns an Enumeration
   */
  public synchronized Enumeration elements()
  {
    return new Enumeration()
    {
      int i = 0;
      public boolean hasMoreElements()
      {
	return (i < elementCount);
      }
      public Object nextElement()
      {
	if (i >= elementCount)
	  throw new NoSuchElementException();
	return (elementAt(i++));
      }
    };
  }
  
  public List subList(int fromIndex, int toIndex)
  {
    List sub = super.subList(fromIndex, toIndex);
    return Collections.synchronizedList(sub);
  }
  
  /** @specnote This is not specified as synchronized in the JCL, but it seems
    * to me that is should be. If it isn't, a clear() operation on a sublist
    * will not be synchronized w.r.t. the Vector object.
    */
  protected synchronized void removeRange(int fromIndex, int toIndex)
  {
    modCount++;
    if (fromIndex != toIndex)
      {
	System.arraycopy(elementData, toIndex, elementData, fromIndex, 
	                 elementCount - toIndex);
	elementCount -= (toIndex - fromIndex);
      }
  }
}
