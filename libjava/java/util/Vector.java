/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

import java.io.Serializable;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date August 17, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

class VectorEnumeration implements Enumeration
{
  private int enumIndex;
  private Vector enumVec;

  public VectorEnumeration(Vector vec)
  {
    enumVec = vec;
    enumIndex = 0;
  }

  public boolean hasMoreElements()
  {
    return enumIndex < enumVec.size();
  }

  public Object nextElement()
  {
    if (!hasMoreElements())
      throw new NoSuchElementException();

    return enumVec.elementData[enumIndex++];
  }
}

// TODO12:
// public class Vector extends AbstractList
//	implements List, Cloneable, Serializable

public class Vector implements Cloneable, Serializable
{
  /* The size of the increment to use when growing this vector.
     The default of 0 means to double the capacity when growing. */
  protected int capacityIncrement;

  /* The number of elements currently in elementData */
  protected int elementCount;

  /* The buffer in which elements of this vector are stored */
  protected Object[] elementData;

  public Vector()
  {
    this(10, 0);
  }

  public Vector(int initCap)
  {
    this(initCap, 0);
  }

  public Vector(int initCap, int capIncrement)
  {
    if (initCap < 0)
      throw new IllegalArgumentException ();
    elementData = new Object[initCap];
    capacityIncrement = capIncrement;
    elementCount = 0;
  }

  public final synchronized void addElement(Object obj)
  {
    // Make sure there's room for a new element
    if (elementCount == elementData.length)
      ensureCapacity(elementCount+1);

    elementData[elementCount++] = obj;
  }

  public final int capacity()
  {
    return elementData.length;
  }

  public synchronized Object clone()
  {
    // New vector needs to have same size, capacity and capacityIncrement
    Vector newVec = new Vector(elementData.length, capacityIncrement);

    System.arraycopy(elementData, 0, newVec.elementData, 0, elementCount);
    newVec.elementCount = elementCount;
    return newVec;
  }

  public final boolean contains(Object obj)
  {
    for (int i = 0; i < elementCount; i++)
      {
	if (obj == null
	    ? elementData[i] == null
	    : obj.equals(elementData[i]))
	  return true;
      }

    return false;
  }

  public final synchronized void copyInto(Object[] objArray)
  {
    System.arraycopy(elementData, 0, objArray, 0, elementCount);
  }

  public final synchronized Object elementAt(int idx)
  {
    if (idx < 0 || idx >= size())
      throw new ArrayIndexOutOfBoundsException();

    return elementData[idx];
  }

  public final synchronized Enumeration elements()
  {
    return new VectorEnumeration(this);
  }

  public final synchronized void ensureCapacity(int minCap)
  {
    // Increasing the vector could make it much larger than minCap;
    // e.g. if minCap is just larger than the vector, size may double.
    // If someone cares about this possibility they should set capacityIncrement
    if (minCap > elementData.length)
      {
	// Increase the vector; double it if capacityIncrement is zero
	int newSize = elementData.length;
	newSize +=
	  (capacityIncrement > 0) ? capacityIncrement : elementData.length;

	// Make sure newSize is at least minCap
	if (newSize < minCap)
	  newSize = minCap;

	Object[] newArray = new Object[newSize];
	System.arraycopy(elementData, 0, newArray, 0, elementCount);
	elementData = newArray;
      }
  }

  public final synchronized Object firstElement()
  {
    if (elementCount == 0)
      throw new NoSuchElementException();

    return elementData[0];
  }

  public final int indexOf(Object obj)
  {
    return indexOf(obj, 0);
  }

  public final synchronized int indexOf(Object obj, int idx)
  {
    if (idx < 0)
      throw new IllegalArgumentException ();
    for (int i = idx; i < elementCount; i++)
      {
	if (obj == null
	    ? elementData[i] == null
	    : obj.equals(elementData[i]))
	  return i;
      }

    return -1;
  }

  public final synchronized void insertElementAt(Object obj, int idx)
  {
    if (idx < 0 || idx > size())
      throw new ArrayIndexOutOfBoundsException();
    else if (idx == size())		// Spec says just use addElement()
      addElement(obj);
    else
      {
	// Make sure there's room for a new element
	if (elementCount == elementData.length)
	  ensureCapacity(elementCount+1);

	// Shift the existing elements up and increment elementCount
	for (int i = elementCount++; i > idx; --i)
	  elementData[i] = elementData[i-1];

	elementData[idx] = obj;
      }
  }

  public final boolean isEmpty()
  {
    return elementCount == 0;
  }

  public final synchronized Object lastElement()
  {
    if (elementCount == 0)
      throw new NoSuchElementException();

    return elementData[elementCount - 1];
  }

  public final int lastIndexOf(Object obj)
  {
    return lastIndexOf(obj, size()-1);
  }

  public final synchronized int lastIndexOf(Object obj, int idx)
  {
    if (idx < 0)
      throw new IllegalArgumentException ();
    for (int i = idx; i >= 0; --i)
      {
	if (obj == null
	    ? elementData[i] == null
	    : obj.equals(elementData[i]))
	  return i;
      }

    return -1;
  }

  public final synchronized void removeAllElements()
  {
    // Remove elements now to assist the gc in early cleanup
    for (int i = elementCount-1; i >= 0; --i)
	elementData[i] = null;
    elementCount = 0;
  }

  public final synchronized boolean removeElement(Object obj)
  {
    for (int i = 0; i < elementCount; i++)
      {
	if (obj == null
	    ? elementData[i] == null
	    : obj.equals(elementData[i]))
	  {
	    int j;

	    // Decrement count first to ensure we don't walk off end of array
	    --elementCount;

	    for (j = i; j < elementCount; j++)
	      elementData[j] = elementData[j+1];

	    // At this point, j was incrememented and points to old last element
	    // Remove element now to assist the gc in early cleanup
	    elementData[j] = null;
	    return true;
	  }
      }

    return false;
  }

  public final synchronized void removeElementAt(int idx)
  {
    int i;

    if (idx < 0 || idx >= size())
      throw new ArrayIndexOutOfBoundsException();

    // Decrement count first to ensure we don't walk off the end of the array
    --elementCount;

    for (i = idx; i < elementCount; i++)
      elementData[i] = elementData[i+1];

    // At this point, i was incrememented and now points to the old last element
    // Remove element now to assist the gc in early cleanup
    elementData[i] = null;
  }

  public final synchronized void setElementAt(Object obj, int idx)
  {
    if (idx < 0 || idx >= size())
      throw new ArrayIndexOutOfBoundsException();

    elementData[idx] = obj;
  }

  public final synchronized void setSize(int newSize)
  {
    if (newSize < 0)
      throw new ArrayIndexOutOfBoundsException();

    // Java Lang Spec p. 658 says to remove the excess elements and discard
    // when new size is smaller than old size.
    // When truncating, we could alternatively just reset elementCount instead
    // of freeing up the memory if the spec hadn't specified.  The spec makes
    // sense though; if someone cares enough to call a setSize() function
    // they probably are doing so to free memory.
    if (newSize < elementCount)
      {
	elementCount = newSize;
	trimToSize();
      }
    else if (newSize > elementCount)	// Skip == case
      {
	// TBD: ensureCapacity() may create a vector much larger than newSize;
	// do we want to make the vector exactly newSize?  Spec is unclear.
	ensureCapacity(newSize);

	// Make sure to null out new elements of grown vector
	for (int i = elementCount; i < newSize; i++)
	  elementData[i] = null;
	elementCount = newSize;
      }
  }

  public final int size()
  {
    return elementCount;
  }

  public final synchronized String toString()
  {
    // Following the Java Lang Spec p. 656

    // Prepend first element with open bracket
    StringBuffer result = new StringBuffer("[");

    if (elementCount > 0)	// add first element if one exists
      result.append(elementData[0].toString());

    // Prepend subsequent elements with ", "
    for (int i = 1; i < elementCount; i++)
      result.append(", ").append(elementData[i].toString());

    // Append last element with closing bracket
    result.append("]");
    return result.toString();
  }

  public final synchronized void trimToSize()
  {
    // Give up excess storage capacity to save memory
    //
    // Don't bother checking for the case where size() == the capacity of the
    // vector since that is a much less likely case; it's more efficient to
    // not do the check and lose a bit of performance in that infrequent case
    Object[] newArray = new Object[elementCount];
    System.arraycopy(elementData, 0, newArray, 0, elementCount);
    elementData = newArray;
  }

  // TODO12:
  // public Vector(Collection c)
  // {
  // }

  // TODO12:
  // public public boolean add(Object o)
  // {
  // }

  // TODO12:
  // public void add(int index, Object element)
  // {
  // }

  // TODO12:
  // public boolean addAll(Collection c)
  // {
  // }

  // TODO12:
  // public boolean addAll(int index, Collection c)
  // {
  // }

  // TODO12:
  // public void clear()
  // {
  // }

  // TODO12:
  // public boolean containsAll(Collection c)
  // {
  // }

  // TODO12:
  // public boolean equals(Object o)
  // {
  // }

  // TODO12:
  // public int hashCode()
  // {
  // }

  // TODO12:
  // public Object get(int index)
  // {
  // }

  // TODO12:
  // public boolean remove(Object o)
  // {
  // }

  // TODO12:
  // public Object remove(int index)
  // {
  // }

  // TODO12:
  // public boolean removeAll(Collection c)
  // {
  // }

  // TODO12:
  // public boolean retainAll(Collection c)
  // {
  // }

  // TODO12:
  // public Object set(int index, Object element)
  // {
  // }

  // TODO12:
  // public Object[] toArray()
  // {
  // }

  // TODO12:
  // public Object[] toArray(Object[] a)
  // {
  // }
}
