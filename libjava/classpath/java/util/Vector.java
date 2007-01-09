/* Vector.java -- Class that provides growable arrays.
   Copyright (C) 1998, 1999, 2000, 2001, 2004, 2005, 2006,  
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

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Array;

/**
 * The <code>Vector</code> classes implements growable arrays of Objects.
 * You can access elements in a Vector with an index, just as you
 * can in a built in array, but Vectors can grow and shrink to accommodate
 * more or fewer objects.<p>
 *
 * Vectors try to mantain efficiency in growing by having a
 * <code>capacityIncrement</code> that can be specified at instantiation.
 * When a Vector can no longer hold a new Object, it grows by the amount
 * in <code>capacityIncrement</code>. If this value is 0, the vector doubles in
 * size.<p>
 *
 * Vector implements the JDK 1.2 List interface, and is therefore a fully
 * compliant Collection object. The iterators are fail-fast - if external
 * code structurally modifies the vector, any operation on the iterator will
 * then throw a {@link ConcurrentModificationException}. The Vector class is
 * fully synchronized, but the iterators are not. So, when iterating over a
 * vector, be sure to synchronize on the vector itself.  If you don't want the
 * expense of synchronization, use ArrayList instead. On the other hand, the
 * Enumeration of elements() is not thread-safe, nor is it fail-fast; so it
 * can lead to undefined behavior even in a single thread if you modify the
 * vector during iteration.<p>
 *
 * Note: Some methods, especially those specified by List, specify throwing
 * {@link IndexOutOfBoundsException}, but it is easier to implement by
 * throwing the subclass {@link ArrayIndexOutOfBoundsException}. Others
 * directly specify this subclass.
 *
 * @author Scott G. Miller
 * @author Bryce McKinlay
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see List
 * @see ArrayList
 * @see LinkedList
 * @since 1.0
 * @status updated to 1.4
 */
public class Vector<T> extends AbstractList<T>
  implements List<T>, RandomAccess, Cloneable, Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -2767605614048989439L;

  /**
   * The internal array used to hold members of a Vector. The elements are
   * in positions 0 through elementCount - 1, and all remaining slots are null.
   * @serial the elements
   */
  protected T[] elementData;

  /**
   * The number of elements currently in the vector, also returned by
   * {@link #size}.
   * @serial the size
   */
  protected int elementCount;

  /**
   * The amount the Vector's internal array should be increased in size when
   * a new element is added that exceeds the current size of the array,
   * or when {@link #ensureCapacity} is called. If &lt;= 0, the vector just
   * doubles in size.
   * @serial the amount to grow the vector by
   */
  protected int capacityIncrement;

  /**
   * Constructs an empty vector with an initial size of 10, and
   * a capacity increment of 0
   */
  public Vector()
  {
    this(10, 0);
  }

  /**
   * Constructs a vector containing the contents of Collection, in the
   * order given by the collection.
   *
   * @param c collection of elements to add to the new vector
   * @throws NullPointerException if c is null
   * @since 1.2
   */
  public Vector(Collection<? extends T> c)
  {
    elementCount = c.size();
    elementData = c.toArray((T[]) new Object[elementCount]);
  }

  /**
   * Constructs a Vector with the initial capacity and capacity
   * increment specified.
   *
   * @param initialCapacity the initial size of the Vector's internal array
   * @param capacityIncrement the amount the internal array should be
   *        increased by when necessary, 0 to double the size
   * @throws IllegalArgumentException if initialCapacity &lt; 0
   */
  public Vector(int initialCapacity, int capacityIncrement)
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException();
    elementData = (T[]) new Object[initialCapacity];
    this.capacityIncrement = capacityIncrement;
  }

  /**
   * Constructs a Vector with the initial capacity specified, and a capacity
   * increment of 0 (double in size).
   *
   * @param initialCapacity the initial size of the Vector's internal array
   * @throws IllegalArgumentException if initialCapacity &lt; 0
   */
  public Vector(int initialCapacity)
  {
    this(initialCapacity, 0);
  }

  /**
   * Copies the contents of the Vector into the provided array.  If the
   * array is too small to fit all the elements in the Vector, an 
   * {@link IndexOutOfBoundsException} is thrown without modifying the array.  
   * Old elements in the array are overwritten by the new elements.
   *
   * @param a target array for the copy
   * @throws IndexOutOfBoundsException the array is not large enough
   * @throws NullPointerException the array is null
   * @see #toArray(Object[])
   */
  public synchronized void copyInto(Object[] a)
  {
    System.arraycopy(elementData, 0, a, 0, elementCount);
  }

  /**
   * Trims the Vector down to size.  If the internal data array is larger
   * than the number of Objects its holding, a new array is constructed
   * that precisely holds the elements. Otherwise this does nothing.
   */
  public synchronized void trimToSize()
  {
    // Don't bother checking for the case where size() == the capacity of the
    // vector since that is a much less likely case; it's more efficient to
    // not do the check and lose a bit of performance in that infrequent case

    T[] newArray = (T[]) new Object[elementCount];
    System.arraycopy(elementData, 0, newArray, 0, elementCount);
    elementData = newArray;
  }

  /**
   * Ensures that <code>minCapacity</code> elements can fit within this Vector.
   * If <code>elementData</code> is too small, it is expanded as follows:
   * If the <code>elementCount + capacityIncrement</code> is adequate, that
   * is the new size. If <code>capacityIncrement</code> is non-zero, the
   * candidate size is double the current. If that is not enough, the new
   * size is <code>minCapacity</code>.
   *
   * @param minCapacity the desired minimum capacity, negative values ignored
   */
  public synchronized void ensureCapacity(int minCapacity)
  {
    if (elementData.length >= minCapacity)
      return;

    int newCapacity;
    if (capacityIncrement <= 0)
      newCapacity = elementData.length * 2;
    else
      newCapacity = elementData.length + capacityIncrement;

    T[] newArray = (T[]) new Object[Math.max(newCapacity, minCapacity)];

    System.arraycopy(elementData, 0, newArray, 0, elementCount);
    elementData = newArray;
  }

  /**
   * Explicitly sets the size of the vector (but not necessarily the size of
   * the internal data array). If the new size is smaller than the old one,
   * old values that don't fit are lost. If the new size is larger than the
   * old one, the vector is padded with null entries.
   *
   * @param newSize The new size of the internal array
   * @throws ArrayIndexOutOfBoundsException if the new size is negative
   */
  public synchronized void setSize(int newSize)
  {
    // Don't bother checking for the case where size() == the capacity of the
    // vector since that is a much less likely case; it's more efficient to
    // not do the check and lose a bit of performance in that infrequent case
    modCount++;
    ensureCapacity(newSize);
    if (newSize < elementCount)
      Arrays.fill(elementData, newSize, elementCount, null);
    elementCount = newSize;
  }

  /**
   * Returns the size of the internal data array (not the amount of elements
   * contained in the Vector).
   *
   * @return capacity of the internal data array
   */
  public synchronized int capacity()
  {
    return elementData.length;
  }

  /**
   * Returns the number of elements stored in this Vector.
   *
   * @return the number of elements in this Vector
   */
  public synchronized int size()
  {
    return elementCount;
  }

  /**
   * Returns true if this Vector is empty, false otherwise
   *
   * @return true if the Vector is empty, false otherwise
   */
  public synchronized boolean isEmpty()
  {
    return elementCount == 0;
  }

  /**
   * Returns an Enumeration of the elements of this Vector. The enumeration
   * visits the elements in increasing index order, but is NOT thread-safe.
   *
   * @return an Enumeration
   * @see #iterator()
   */
  // No need to synchronize as the Enumeration is not thread-safe!
  public Enumeration<T> elements()
  {
    return new Enumeration<T>()
    {
      private int i = 0;

      public boolean hasMoreElements()
      {
        return i < elementCount;
      }

      public T nextElement()
      {
        if (i >= elementCount)
          throw new NoSuchElementException();
        return elementData[i++];
      }
    };
  }

  /**
   * Returns true when <code>elem</code> is contained in this Vector.
   *
   * @param elem the element to check
   * @return true if the object is contained in this Vector, false otherwise
   */
  public boolean contains(Object elem)
  {
    return indexOf(elem, 0) >= 0;
  }

  /**
   * Returns the first occurrence of <code>elem</code> in the Vector, or -1 if
   * <code>elem</code> is not found.
   *
   * @param elem the object to search for
   * @return the index of the first occurrence, or -1 if not found
   */
  public int indexOf(Object elem)
  {
    return indexOf(elem, 0);
  }

  /**
   * Searches the vector starting at <code>index</code> for object
   * <code>elem</code> and returns the index of the first occurrence of this
   * Object.  If the object is not found, or index is larger than the size
   * of the vector, -1 is returned.
   *
   * @param e the Object to search for
   * @param index start searching at this index
   * @return the index of the next occurrence, or -1 if it is not found
   * @throws IndexOutOfBoundsException if index &lt; 0
   */
  public synchronized int indexOf(Object e, int index)
  {
    for (int i = index; i < elementCount; i++)
      if (equals(e, elementData[i]))
        return i;
    return -1;
  }

  /**
   * Returns the last index of <code>elem</code> within this Vector, or -1
   * if the object is not within the Vector.
   *
   * @param elem the object to search for
   * @return the last index of the object, or -1 if not found
   */
  public int lastIndexOf(Object elem)
  {
    return lastIndexOf(elem, elementCount - 1);
  }

  /**
   * Returns the index of the first occurrence of <code>elem</code>, when
   * searching backwards from <code>index</code>.  If the object does not
   * occur in this Vector, or index is less than 0, -1 is returned.
   *
   * @param e the object to search for
   * @param index the index to start searching in reverse from
   * @return the index of the Object if found, -1 otherwise
   * @throws IndexOutOfBoundsException if index &gt;= size()
   */
  public synchronized int lastIndexOf(Object e, int index)
  {
    checkBoundExclusive(index);
    for (int i = index; i >= 0; i--)
      if (equals(e, elementData[i]))
        return i;
    return -1;
  }

  /**
   * Returns the Object stored at <code>index</code>.
   *
   * @param index the index of the Object to retrieve
   * @return the object at <code>index</code>
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt;= size()
   * @see #get(int)
   */
  public synchronized T elementAt(int index)
  {
    checkBoundExclusive(index);
    return elementData[index];
  }

  /**
   * Returns the first element (index 0) in the Vector.
   *
   * @return the first Object in the Vector
   * @throws NoSuchElementException the Vector is empty
   */
  public synchronized T firstElement()
  {
    if (elementCount == 0)
      throw new NoSuchElementException();

    return elementData[0];
  }

  /**
   * Returns the last element in the Vector.
   *
   * @return the last Object in the Vector
   * @throws NoSuchElementException the Vector is empty
   */
  public synchronized T lastElement()
  {
    if (elementCount == 0)
      throw new NoSuchElementException();

    return elementData[elementCount - 1];
  }

  /**
   * Changes the element at <code>index</code> to be <code>obj</code>
   *
   * @param obj the object to store
   * @param index the position in the Vector to store the object
   * @throws ArrayIndexOutOfBoundsException the index is out of range
   * @see #set(int, Object)
   */
  public void setElementAt(T obj, int index)
  {
    set(index, obj);
  }

  /**
   * Removes the element at <code>index</code>, and shifts all elements at
   * positions greater than index to their index - 1.
   *
   * @param index the index of the element to remove
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt;= size();
   * @see #remove(int)
   */
  public void removeElementAt(int index)
  {
    remove(index);
  }

  /**
   * Inserts a new element into the Vector at <code>index</code>.  Any elements
   * at or greater than index are shifted up one position.
   *
   * @param obj the object to insert
   * @param index the index at which the object is inserted
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt; size()
   * @see #add(int, Object)
   */
  public synchronized void insertElementAt(T obj, int index)
  {
    checkBoundInclusive(index);
    if (elementCount == elementData.length)
      ensureCapacity(elementCount + 1);
    modCount++;
    System.arraycopy(elementData, index, elementData, index + 1,
                     elementCount - index);
    elementCount++;
    elementData[index] = obj;
  }

  /**
   * Adds an element to the Vector at the end of the Vector.  The vector
   * is increased by ensureCapacity(size() + 1) if needed.
   *
   * @param obj the object to add to the Vector
   */
  public synchronized void addElement(T obj)
  {
    if (elementCount == elementData.length)
      ensureCapacity(elementCount + 1);
    modCount++;
    elementData[elementCount++] = obj;
  }

  /**
   * Removes the first (the lowest index) occurrence of the given object from
   * the Vector. If such a remove was performed (the object was found), true
   * is returned. If there was no such object, false is returned.
   *
   * @param obj the object to remove from the Vector
   * @return true if the Object was in the Vector, false otherwise
   * @see #remove(Object)
   */
  public synchronized boolean removeElement(Object obj)
  {
    int idx = indexOf(obj, 0);
    if (idx >= 0)
      {
        remove(idx);
        return true;
      }
    return false;
  }

  /**
   * Removes all elements from the Vector.  Note that this does not
   * resize the internal data array.
   *
   * @see #clear()
   */
  public synchronized void removeAllElements()
  {
    if (elementCount == 0)
      return;

    modCount++;
    Arrays.fill(elementData, 0, elementCount, null);
    elementCount = 0;
  }

  /**
   * Creates a new Vector with the same contents as this one. The clone is
   * shallow; elements are not cloned.
   *
   * @return the clone of this vector
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
        // Impossible to get here.
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
   * @return an Object[] containing the contents of this Vector in order
   * @since 1.2
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
   * of the null element, if you know the vector does not itself contain
   * null entries.  If the array is not large enough, reflection is used
   * to create a bigger one of the same runtime type.
   *
   * @param a an array to copy the Vector into if large enough
   * @return an array with the contents of this Vector in order
   * @throws ArrayStoreException the runtime type of the provided array
   *         cannot hold the elements of the Vector
   * @throws NullPointerException if <code>a</code> is null
   * @since 1.2
   */
  public synchronized <S> S[] toArray(S[] a)
  {
    if (a.length < elementCount)
      a = (S[]) Array.newInstance(a.getClass().getComponentType(),
				  elementCount);
    else if (a.length > elementCount)
      a[elementCount] = null;
    System.arraycopy(elementData, 0, a, 0, elementCount);
    return a;
  }

  /**
   * Returns the element at position <code>index</code>.
   *
   * @param index the position from which an element will be retrieved
   * @return the element at that position
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt;= size()
   * @since 1.2
   */
  public T get(int index)
  {
    return elementAt(index);
  }

  /**
   * Puts <code>element</code> into the Vector at position <code>index</code>
   * and returns the Object that previously occupied that position.
   *
   * @param index the index within the Vector to place the Object
   * @param element the Object to store in the Vector
   * @return the previous object at the specified index
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt;= size()
   * @since 1.2
   */
  public synchronized T set(int index, T element)
  {
    checkBoundExclusive(index);
    T temp = elementData[index];
    elementData[index] = element;
    return temp;
  }

  /**
   * Adds an object to the Vector.
   *
   * @param o the element to add to the Vector
   * @return true, as specified by List
   * @since 1.2
   */
  public boolean add(T o)
  {
    addElement(o);
    return true;
  }

  /**
   * Removes the given Object from the Vector.  If it exists, true
   * is returned, if not, false is returned.
   *
   * @param o the object to remove from the Vector
   * @return true if the Object existed in the Vector, false otherwise
   * @since 1.2
   */
  public boolean remove(Object o)
  {
    return removeElement(o);
  }

  /**
   * Adds an object at the specified index.  Elements at or above
   * index are shifted up one position.
   *
   * @param index the index at which to add the element
   * @param element the element to add to the Vector
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt; size()
   * @since 1.2
   */
  public void add(int index, T element)
  {
    insertElementAt(element, index);
  }

  /**
   * Removes the element at the specified index, and returns it.
   *
   * @param index the position from which to remove the element
   * @return the object removed
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt;= size()
   * @since 1.2
   */
  public synchronized T remove(int index)
  {
    checkBoundExclusive(index);
    T temp = elementData[index];
    modCount++;
    elementCount--;
    if (index < elementCount)
      System.arraycopy(elementData, index + 1, elementData, index,
                       elementCount - index);
    elementData[elementCount] = null;
    return temp;
  }

  /**
   * Clears all elements in the Vector and sets its size to 0.
   */
  public void clear()
  {
    removeAllElements();
  }

  /**
   * Returns true if this Vector contains all the elements in c.
   *
   * @param c the collection to compare to
   * @return true if this vector contains all elements of c
   * @throws NullPointerException if c is null
   * @since 1.2
   */
  public synchronized boolean containsAll(Collection<?> c)
  {
    // Here just for the sychronization.
    return super.containsAll(c);
  }

  /**
   * Appends all elements of the given collection to the end of this Vector.
   * Behavior is undefined if the collection is modified during this operation
   * (for example, if this == c).
   *
   * @param c the collection to append
   * @return true if this vector changed, in other words c was not empty
   * @throws NullPointerException if c is null
   * @since 1.2
   */
  public synchronized boolean addAll(Collection<? extends T> c)
  {
    return addAll(elementCount, c);
  }

  /**
   * Remove from this vector all elements contained in the given collection.
   *
   * @param c the collection to filter out
   * @return true if this vector changed
   * @throws NullPointerException if c is null
   * @since 1.2
   */
  public synchronized boolean removeAll(Collection<?> c)
  {
    // The NullPointerException is thrown implicitly when the Vector
    // is not empty and c is null. The RI allows null arguments when
    // the vector is empty. See Mauve test:
    // gnu/testlet/java/util/Vector/removeAll.java

    int i;
    int j;
    for (i = 0; i < elementCount; i++)
      if (c.contains(elementData[i]))
        break;
    if (i == elementCount)
      return false;

    modCount++;
    for (j = i++; i < elementCount; i++)
      if (! c.contains(elementData[i]))
        elementData[j++] = elementData[i];
    elementCount -= i - j;
    return true;
  }

  /**
   * Retain in this vector only the elements contained in the given collection.
   *
   * @param c the collection to filter by
   * @return true if this vector changed
   * @throws NullPointerException if c is null
   * @since 1.2
   */
  public synchronized boolean retainAll(Collection<?> c)
  {
    // The NullPointerException is thrown implicitly when the Vector
    // is not empty and c is null. The RI allows null arguments when
    // the vector is empty. See Mauve test:
    // gnu/testlet/java/util/Vector/retainAll.java

    int i;
    int j;
    for (i = 0; i < elementCount; i++)
      if (! c.contains(elementData[i]))
        break;
    if (i == elementCount)
      return false;

    modCount++;
    for (j = i++; i < elementCount; i++)
      if (c.contains(elementData[i]))
        elementData[j++] = elementData[i];
    elementCount -= i - j;
    return true;
  }

  /**
   * Inserts all elements of the given collection at the given index of
   * this Vector. Behavior is undefined if the collection is modified during
   * this operation (for example, if this == c).
   *
   * @param c the collection to append
   * @return true if this vector changed, in other words c was not empty
   * @throws NullPointerException if c is null
   * @throws ArrayIndexOutOfBoundsException index &lt; 0 || index &gt; size()
   * @since 1.2
   */
  public synchronized boolean addAll(int index, Collection<? extends T> c)
  {
    checkBoundInclusive(index);
    Iterator<? extends T> itr = c.iterator();
    int csize = c.size();

    modCount++;
    ensureCapacity(elementCount + csize);
    int end = index + csize;
    if (elementCount > 0 && index != elementCount)
      System.arraycopy(elementData, index,
		       elementData, end, elementCount - index);
    elementCount += csize;
    for ( ; index < end; index++)
      elementData[index] = itr.next();
    return (csize > 0);
  }

  /**
   * Compares this to the given object.
   *
   * @param o the object to compare to
   * @return true if the two are equal
   * @since 1.2
   */
  public synchronized boolean equals(Object o)
  {
    // Here just for the sychronization.
    return super.equals(o);
  }

  /**
   * Computes the hashcode of this object.
   *
   * @return the hashcode
   * @since 1.2
   */
  public synchronized int hashCode()
  {
    // Here just for the sychronization.
    return super.hashCode();
  }

  /**
   * Returns a string representation of this Vector in the form
   * "[element0, element1, ... elementN]".
   *
   * @return the String representation of this Vector
   */
  public synchronized String toString()
  {
    // Here just for the sychronization.
    return super.toString();
  }

  /**
   * Obtain a List view of a subsection of this list, from fromIndex
   * (inclusive) to toIndex (exclusive). If the two indices are equal, the
   * sublist is empty. The returned list is modifiable, and changes in one
   * reflect in the other. If this list is structurally modified in
   * any way other than through the returned list, the result of any subsequent
   * operations on the returned list is undefined.
   * <p>
   *
   * @param fromIndex the index that the returned list should start from
   *        (inclusive)
   * @param toIndex the index that the returned list should go to (exclusive)
   * @return a List backed by a subsection of this vector
   * @throws IndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; size()
   * @throws IllegalArgumentException if fromIndex &gt; toIndex
   * @see ConcurrentModificationException
   * @since 1.2
   */
  public synchronized List<T> subList(int fromIndex, int toIndex)
  {
    List<T> sub = super.subList(fromIndex, toIndex);
    // We must specify the correct object to synchronize upon, hence the
    // use of a non-public API
    return new Collections.SynchronizedList<T>(this, sub);
  }

  /**
   * Removes a range of elements from this list.
   * Does nothing when toIndex is equal to fromIndex.
   *
   * @param fromIndex the index to start deleting from (inclusive)
   * @param toIndex the index to delete up to (exclusive)
   * @throws IndexOutOfBoundsException if fromIndex &gt; toIndex
   */
  // This does not need to be synchronized, because it is only called through
  // clear() of a sublist, and clear() had already synchronized.
  protected void removeRange(int fromIndex, int toIndex)
  {
    int change = toIndex - fromIndex;
    if (change > 0)
      {
        modCount++;
        System.arraycopy(elementData, toIndex, elementData, fromIndex,
                         elementCount - toIndex);
        int save = elementCount;
        elementCount -= change;
        Arrays.fill(elementData, elementCount, save, null);
      }
    else if (change < 0)
      throw new IndexOutOfBoundsException();
  }

  /**
   * Checks that the index is in the range of possible elements (inclusive).
   *
   * @param index the index to check
   * @throws ArrayIndexOutOfBoundsException if index &gt; size
   */
  private void checkBoundInclusive(int index)
  {
    // Implementation note: we do not check for negative ranges here, since
    // use of a negative index will cause an ArrayIndexOutOfBoundsException
    // with no effort on our part.
    if (index > elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " > " + elementCount);
  }

  /**
   * Checks that the index is in the range of existing elements (exclusive).
   *
   * @param index the index to check
   * @throws ArrayIndexOutOfBoundsException if index &gt;= size
   */
  private void checkBoundExclusive(int index)
  {
    // Implementation note: we do not check for negative ranges here, since
    // use of a negative index will cause an ArrayIndexOutOfBoundsException
    // with no effort on our part.
    if (index >= elementCount)
      throw new ArrayIndexOutOfBoundsException(index + " >= " + elementCount);
  }

  /**
   * Serializes this object to the given stream.
   *
   * @param s the stream to write to
   * @throws IOException if the underlying stream fails
   * @serialData just calls default write function
   */
  private synchronized void writeObject(ObjectOutputStream s)
    throws IOException
  {
    s.defaultWriteObject();
  }

}
