/* AbstractSequentialList.java -- List implementation for sequential access
   Copyright (C) 1998, 1999, 2000, 2001, 2005  Free Software Foundation, Inc.

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

/**
 * Abstract superclass to make it easier to implement the List interface when
 * backed by a sequential-access store, such as a linked list. For random
 * access data, use AbstractList. This class implements the random access
 * methods (<code>get</code>, <code>set</code>, <code>add</code>, and
 * <code>remove</code>) atop the list iterator, opposite of AbstractList's
 * approach of implementing the iterator atop random access.
 * <p>
 *
 * To implement a list, you need an implementation for <code>size()</code>
 * and <code>listIterator</code>.  With just <code>hasNext</code>,
 * <code>next</code>, <code>hasPrevious</code>, <code>previous</code>,
 * <code>nextIndex</code>, and <code>previousIndex</code>, you have an
 * unmodifiable list. For a modifiable one, add <code>set</code>, and for
 * a variable-size list, add <code>add</code> and <code>remove</code>.
 * <p>
 *
 * The programmer should provide a no-argument constructor, and one that
 * accepts another Collection, as recommended by the Collection interface.
 * Unfortunately, there is no way to enforce this in Java.
 *
 * @author Original author unknown
 * @author Bryce McKinlay
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see List
 * @see AbstractList
 * @see AbstractCollection
 * @see ListIterator
 * @see LinkedList
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class AbstractSequentialList extends AbstractList
{
  /**
   * The main constructor, for use by subclasses.
   */
  protected AbstractSequentialList()
  {
  }

  /**
   * Returns a ListIterator over the list, starting from position index.
   * Subclasses must provide an implementation of this method.
   *
   * @param index the starting position of the list
   * @return the list iterator
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
  public abstract ListIterator listIterator(int index);

  /**
   * Insert an element into the list at a given position (optional operation).
   * This shifts all existing elements from that position to the end one
   * index to the right. This version of add has no return, since it is
   * assumed to always succeed if there is no exception. This iteration
   * uses listIterator(index).add(o).
   *
   * @param index the location to insert the item
   * @param o the object to insert
   * @throws UnsupportedOperationException if this list does not support the
   *         add operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason.
   * @throws NullPointerException if o is null and the list does not permit
   *         the addition of null values.
   */
  public void add(int index, Object o)
  {
    listIterator(index).add(o);
  }

  /**
   * Insert the contents of a collection into the list at a given position
   * (optional operation). Shift all elements at that position to the right
   * by the number of elements inserted. This operation is undefined if
   * this list is modified during the operation (for example, if you try
   * to insert a list into itself).
   * <p>
   *
   * This implementation grabs listIterator(index), then proceeds to use add
   * for each element returned by c's iterator. Sun's online specs are wrong,
   * claiming that this also calls next(): listIterator.add() correctly
   * skips the added element.
   *
   * @param index the location to insert the collection
   * @param c the collection to insert
   * @return true if the list was modified by this action, that is, if c is
   *         non-empty
   * @throws UnsupportedOperationException if this list does not support the
   *         addAll operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   * @throws ClassCastException if some element of c cannot be added to this
   *         list due to its type
   * @throws IllegalArgumentException if some element of c cannot be added
   *         to this list for some other reason
   * @throws NullPointerException if the specified collection is null
   * @throws NullPointerException if an object, o, in c is null and the list
   *         does not permit the addition of null values.
   * @see #add(int, Object)
   */
  public boolean addAll(int index, Collection c)
  {
    Iterator ci = c.iterator();
    int size = c.size();
    ListIterator i = listIterator(index);
    for (int pos = size; pos > 0; pos--)
      i.add(ci.next());
    return size > 0;
  }

  /**
   * Get the element at a given index in this list. This implementation
   * returns listIterator(index).next().
   *
   * @param index the index of the element to be returned
   * @return the element at index index in this list
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   */
  public Object get(int index)
  {
    // This is a legal listIterator position, but an illegal get.
    if (index == size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:"
                                          + size());
    return listIterator(index).next();
  }

  /**
   * Obtain an Iterator over this list, whose sequence is the list order. This
   * implementation returns listIterator().
   *
   * @return an Iterator over the elements of this list, in order
   */
  public Iterator iterator()
  {
    return listIterator();
  }

  /**
   * Remove the element at a given position in this list (optional operation).
   * Shifts all remaining elements to the left to fill the gap. This
   * implementation uses listIterator(index) and ListIterator.remove().
   *
   * @param index the position within the list of the object to remove
   * @return the object that was removed
   * @throws UnsupportedOperationException if this list does not support the
   *         remove operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   */
  public Object remove(int index)
  {
    // This is a legal listIterator position, but an illegal remove.
    if (index == size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:"
                                          + size());
    ListIterator i = listIterator(index);
    Object removed = i.next();
    i.remove();
    return removed;
  }

  /**
   * Replace an element of this list with another object (optional operation).
   * This implementation uses listIterator(index) and ListIterator.set(o).
   *
   * @param index the position within this list of the element to be replaced
   * @param o the object to replace it with
   * @return the object that was replaced
   * @throws UnsupportedOperationException if this list does not support the
   *         set operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and the list does not allow
   *         a value to be set to null.
   */
  public Object set(int index, Object o)
  {
    // This is a legal listIterator position, but an illegal set.
    if (index == size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:"
                                          + size());
    ListIterator i = listIterator(index);
    Object old = i.next();
    i.set(o);
    return old;
  }
}
