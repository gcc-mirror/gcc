/* DefaultListModel.java --
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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

package javax.swing;

import java.util.Enumeration;
import java.util.Vector;

/**
 * The default implementation of {@link AbstractListModel}, used by
 * {@link javax.swing.JList} and similar objects as the model of a list of
 * values. The implementation is based on an underlying {@link
 * java.util.Vector}.
 *
 * @author Andrew Selkirk
 * @author Graydon Hoare (graydon@redhat.com)
 */

public class DefaultListModel extends AbstractListModel
{
  private static final long serialVersionUID = 2315945659722172272L;

  /**
   * The vector of elements in this list model.
   */
  private Vector elements = new Vector();

  /**
   * Gets an element of the list at the provided index.
   *
   * @param index The index of the element to get
   *
   * @return The object at the given index
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public Object elementAt(int index)
  {
    return elements.elementAt(index);
  }

  /**
   * Convert the list to a string representation.
   *
   * @return A string representation of the list
   */
  public String toString()
  {
    return elements.toString();
  }

  /**
   * Gets the first index of a particular element in the list.
   *
   * @param element The element to search for
   *
   * @return The first index in the list at which an object
   *     <code>obj</code> exists such that <code>obj.equals(element)</code> is
   *     <code>true</code>; if no such object exists, the method returns
   *     <code>-1</code>
   */
  public int indexOf(Object element)
  {
    return elements.indexOf(element);
  }

  /**
   * Gets the first index of a particular element in a list which occurs
   * <em>at or after</em> a particular index.
   *
   * @param element The element to search for
   * @param startIndex The index to begin searching at
   *
   * @return The first index in the list, greater than or equal to
   *     <code>startIndex</code>, at which an object <code>obj</code> exists
   *     such that <code>obj.equals(element)</code> is <code>true</code>; if no
   *     such object exists, the method returns <code>-1</code>
   */
  public int indexOf(Object element, int startIndex)
  {
    return elements.indexOf(element, startIndex);
  }

  /**
   * Gets the last index of a particular element in the list.
   *
   * @param element The element to search for
   *
   * @return The last index in the list at which an object
   *     <code>obj</code> exists such that <code>obj.equals(element)</code> is
   *     <code>true</code>; if no such object exists, the method returns
   *     <code>-1</code>
   */
  public int lastIndexOf(Object element)
  {
    return elements.lastIndexOf(element);
  }

  /**
   * Gets the last index of a particular element in a list which occurs
   * <em>at or before</em> a particular index.
   *
   * @param element The element to search for
   * @param endIndex The index to finish searching at
   *
   * @return The last index in the list, less than to or equal to
   *     <code>endIndexIndex</code>, at which an object <code>obj</code> exists
   *     such that <code>obj.equals(element)</code> is <code>true</code>; if no
   *     such object exists, the method returns <code>-1</code>
   */
  public int lastIndexOf(Object element, int endIndex)
  {
    return elements.lastIndexOf(element, endIndex);
  }

  /**
   * Gets the list element at a particular index.
   *
   * @param index The index to get the list value at
   *
   * @return The list value at the provided index
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public Object get(int index)
  {
    return elements.get(index);
  }

  /**
   * Sets the list element at a particular index.
   *
   * @param index The list index at which to set a value 
   * @param element The value to set at the specified index
   *
   * @return The value previously held at the specified index
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public Object set(int index, Object element)
  {
    Object result;
    result = elements.set(index, element);
    fireContentsChanged(this, index, index);
    return result;
  }

  /**
   * Inserts an element at a particular index in the list. Each element at
   * index <code>i >= index</code> is shifted to position <code>i + 1</code>.
   * If <code>index</code> is equal to <code>size()</code>, this is
   * equivalent to appending an element to the array. Any
   * <code>index</code> greater than <code>size()</code> is illegal.
   *
   * @param index The index to insert the element at
   * @param element The element to insert at the index
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds <code>[0, size()]</code>
   */
  public void add(int index, Object element)
  {
    elements.add(index, element);
    fireIntervalAdded(this, index, index);
  }

  /**
   * Inserts an element at the end of the list. This is equivalent to
   * calling <code>list.add(list.size(), element)</code>.
   *
   * @param element The element to add to the list
   */
  public void addElement(Object element)
  {
    int s = elements.size();
    elements.add(element);
    fireIntervalAdded(this, s, s);
  }

  /**
   * Gets the number of elements in the list.
   *
   * @return The number of elements in the list
   */
  public int size()
  {
    return elements.size();
  }

  /**
   * Gets an array containing the elements of the list.
   *
   * @return An array of the objects in the list, in the order they occur
   *     in the list
   */
  public Object[] toArray()
  {
    return elements.toArray();
  }

  /**
   * Determines whether a particular element is a member of the list.
   *
   * @param element The element to search for
   *
   * @return <code>true</code> if <code>element</code> is a member of the
   *     list, otherwise <code>false</code>
   */
  public boolean contains(Object element)
  {
    return elements.contains(element);
  }

  /**
   * Copies the list into a provided array. The provided array must be at
   * least as large as the list.
   *
   * @param array The array to copy the list into
   * 
   * @throws IndexOutOfBoundsException if the array is too small to hold the
   *     elements of the list
   */
  public void copyInto(Object[] array)
  {
    elements.copyInto(array);
  }

  /**
   * Erases all the elements of the list, setting the list's size to 0.
   */
  public void clear()
  {
    int s = elements.size();
    if (s > 0)
    {
      elements.clear();
      fireIntervalRemoved(this, 0, s - 1);
    }
  }

  /**
   * Removes the element at a particular index from the list.
   *
   * @param index The index of the element to remove
   *
   * @return The value at the index, which has been removed from the list
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public Object remove(int index)
  {
    Object result;
    result = elements.remove(index);
    fireIntervalRemoved(this, index, index);
    return result;
  }

  /**
   * Determines whether the list is empty.
   *
   * @return <code>true</code> if the list is empty, otherwise
   *     <code>false</code>
   */
  public boolean isEmpty()
  {
    return elements.isEmpty();
  }

  /**
   * Returns an {@link java.util.Enumeration} over the elements of the list.
   *
   * @return A new enumeration which iterates over the list
   */
  public Enumeration<?> elements()
  {
    return elements.elements();
  }

  /**
   * Sets the capacity of the list to be equal to its size. The list's capacity
   * is the number of elements it can hold before it needs to be reallocated.
   * The list's size is the number of elements it currently holds. 
   */
  public void trimToSize()
  {
    elements.trimToSize();
  }

  /**
   * Ensures that the list's capacity is at least equal to
   * <code>size</code>. The list's capacity is the number of elements it
   * can hold before it needs to be reallocated.
   *
   * @param size The capacity to ensure the list can hold
   */
  public void ensureCapacity(int size)
  {
    elements.ensureCapacity(size);
  }

  /**
   * Sets the size of the list to a particular value. If the specified size
   * is greater than the current size, the values at the excess list
   * indices are set to <code>null</code>.  If the specified size is less
   * than the current size, the excess elements are removed from the list.
   *
   * @param size The new size to set the list to
   */
  public void setSize(int size)
  {
    int oldSize = elements.size();
    elements.setSize(size);
    if (oldSize < size)
      {
        fireIntervalAdded(this, oldSize, size - 1);
      }
    else if (oldSize > size)
      {
        this.fireIntervalRemoved(this, size, oldSize - 1);
      }
  }

  /**
   * Gets the capacity of the list. The list's capacity is the number of
   * elements it can hold before it needs to be reallocated. 
   *
   * @return The capacity of the list
   */
  public int capacity()
  {
    return elements.capacity();
  }

  /**
   * Gets the first element in the list.
   *
   * @return The first element in the list
   */
  public Object firstElement()
  {
    return elements.firstElement();
  }

  /**
   * Gets the last element in the list.
   *
   * @return The last element in the list
   */
  public Object lastElement()
  {
    return elements.lastElement();
  }

  /**
   * Sets the list element at a particular index.
   *
   * @param element The value to set at the specified index
   * @param index The list index at which to set a value 
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public void setElementAt(Object element, int index)
  {
    elements.setElementAt(element, index);
    fireContentsChanged(this, index, index);
  }

  /**
   * Removes the element at a particular index from the list.
   *
   * @param index The index of the element to remove
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public void removeElementAt(int index)
  {
    elements.remove(index);
    fireIntervalRemoved(this, index, index);
  }

  /**
   * Inserts an element at a particular index in the list. Each element at
   * index <code>i >= index</code> is shifted to position <code>i + 1</code>.
   * If <code>index</code> is equal to <code>size()</code>, this is
   * equivalent to appending an element to the array. Any
   * <code>index</code> greater than <code>size()</code> is illegal.
   *
   * @param element The element to insert at the index
   * @param index The index to insert the element at
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds <code>[0, size()]</code>
   */
  public void insertElementAt(Object element, int index)
  {
    elements.insertElementAt(element, index);
    fireIntervalAdded(this, index, index);
  }

  /**
   * Removes the first occurrence of a particular element in the list. If the
   * element does not exist in the list, nothing happens.
   *
   * @param element The element to remove
   *
   * @return <code>true</code> if the element existed in the list (and was
   *     removed), <code>false</code> otherwise
   */
  public boolean removeElement(Object element)
  {
    int index;
    index = elements.indexOf(element);
    if (index != -1)
      {
        elements.remove(index);
        fireIntervalRemoved(this, index, index);
        return true;
      }
    return false;
  }

  /**
   * Remove all elements in the list.
   */
  public void removeAllElements()
  {
    int size;
    size = size();
    if (size > 0)
      {
        elements.clear();
        fireIntervalRemoved(this, 0, size - 1);
      }
  }

  /**
   * Remove all elements between <code>startIndex</code> and
   * <code>endIndex</code> inclusive.
   *
   * @param startIndex The first index in the range to remove
   * @param endIndex The last index in the range to remove
   *
   * @throws ArrayIndexOutOfBoundsException if either index is outside the
   *     valid range of indices for this list <code>[0, size())</code>
   * @throws IllegalArgumentException if <code>startIndex &gt; endIndex</code>
   */
  public void removeRange(int startIndex, int endIndex)
  {
    int index;
    if (startIndex > endIndex)
      throw new IllegalArgumentException();
    for (index = endIndex; index >= startIndex; index--)
      elements.remove(index);
    fireIntervalRemoved(this, startIndex, endIndex);
  }

  /**
   * Gets the size of the list.
   *
   * @return The number of elements currently in the list
   */
  public int getSize()
  {
    return elements.size();
  }

  /**
   * Gets the list element at a particular index.
   *
   * @param index The index to get the list value at
   *
   * @return The list value at the provided index
   *
   * @throws ArrayIndexOutOfBoundsException If the provided index is
   *     outside the bounds of the list <code>[0, size())</code>
   */
  public Object getElementAt(int index)
  {
    return elements.get(index);
  }
}
