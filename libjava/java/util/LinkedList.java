/* LinkedList.java -- Linked list implementation of the List interface
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005  Free Software Foundation, Inc.

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
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.lang.reflect.Array;

/**
 * Linked list implementation of the List interface. In addition to the
 * methods of the List interface, this class provides access to the first
 * and last list elements in O(1) time for easy stack, queue, or double-ended
 * queue (deque) creation. The list is doubly-linked, with traversal to a
 * given index starting from the end closest to the element.<p>
 *
 * LinkedList is not synchronized, so if you need multi-threaded access,
 * consider using:<br>
 * <code>List l = Collections.synchronizedList(new LinkedList(...));</code>
 * <p>
 *
 * The iterators are <i>fail-fast</i>, meaning that any structural
 * modification, except for <code>remove()</code> called on the iterator
 * itself, cause the iterator to throw a
 * {@link ConcurrentModificationException} rather than exhibit
 * non-deterministic behavior.
 *
 * @author Original author unknown
 * @author Bryce McKinlay
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see List
 * @see ArrayList
 * @see Vector
 * @see Collections#synchronizedList(List)
 * @since 1.2
 * @status missing javadoc, but complete to 1.4
 */
public class LinkedList extends AbstractSequentialList
  implements List, Cloneable, Serializable
{
  /**
   * Compatible with JDK 1.2.
   */
  private static final long serialVersionUID = 876323262645176354L;

  /**
   * The first element in the list.
   */
  transient Entry first;

  /**
   * The last element in the list.
   */
  transient Entry last;

  /**
   * The current length of the list.
   */
  transient int size = 0;

  /**
   * Class to represent an entry in the list. Holds a single element.
   */
  private static final class Entry
  {
    /** The element in the list. */
    Object data;

    /** The next list entry, null if this is last. */
    Entry next;

    /** The previous list entry, null if this is first. */
    Entry previous;

    /**
     * Construct an entry.
     * @param data the list element
     */
    Entry(Object data)
    {
      this.data = data;
    }
  } // class Entry

  /**
   * Obtain the Entry at a given position in a list. This method of course
   * takes linear time, but it is intelligent enough to take the shorter of the
   * paths to get to the Entry required. This implies that the first or last
   * entry in the list is obtained in constant time, which is a very desirable
   * property.
   * For speed and flexibility, range checking is not done in this method:
   * Incorrect values will be returned if (n &lt; 0) or (n &gt;= size).
   *
   * @param n the number of the entry to get
   * @return the entry at position n
   */
  // Package visible for use in nested classes.
  Entry getEntry(int n)
  {
    Entry e;
    if (n < size / 2)
      {
        e = first;
        // n less than size/2, iterate from start
        while (n-- > 0)
          e = e.next;
      }
    else
      {
        e = last;
        // n greater than size/2, iterate from end
        while (++n < size)
          e = e.previous;
      }
    return e;
  }

  /**
   * Remove an entry from the list. This will adjust size and deal with
   *  `first' and  `last' appropriatly.
   *
   * @param e the entry to remove
   */
  // Package visible for use in nested classes.
  void removeEntry(Entry e)
  {
    modCount++;
    size--;
    if (size == 0)
      first = last = null;
    else
      {
        if (e == first)
          {
            first = e.next;
            e.next.previous = null;
          }
        else if (e == last)
          {
            last = e.previous;
            e.previous.next = null;
          }
        else
          {
            e.next.previous = e.previous;
            e.previous.next = e.next;
          }
      }
  }

  /**
   * Checks that the index is in the range of possible elements (inclusive).
   *
   * @param index the index to check
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size
   */
  private void checkBoundsInclusive(int index)
  {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:"
                                          + size);
  }

  /**
   * Checks that the index is in the range of existing elements (exclusive).
   *
   * @param index the index to check
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size
   */
  private void checkBoundsExclusive(int index)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:"
                                          + size);
  }

  /**
   * Create an empty linked list.
   */
  public LinkedList()
  {
  }

  /**
   * Create a linked list containing the elements, in order, of a given
   * collection.
   *
   * @param c the collection to populate this list from
   * @throws NullPointerException if c is null
   */
  public LinkedList(Collection c)
  {
    addAll(c);
  }

  /**
   * Returns the first element in the list.
   *
   * @return the first list element
   * @throws NoSuchElementException if the list is empty
   */
  public Object getFirst()
  {
    if (size == 0)
      throw new NoSuchElementException();
    return first.data;
  }

  /**
   * Returns the last element in the list.
   *
   * @return the last list element
   * @throws NoSuchElementException if the list is empty
   */
  public Object getLast()
  {
    if (size == 0)
      throw new NoSuchElementException();
    return last.data;
  }

  /**
   * Remove and return the first element in the list.
   *
   * @return the former first element in the list
   * @throws NoSuchElementException if the list is empty
   */
  public Object removeFirst()
  {
    if (size == 0)
      throw new NoSuchElementException();
    modCount++;
    size--;
    Object r = first.data;

    if (first.next != null)
      first.next.previous = null;
    else
      last = null;

    first = first.next;

    return r;
  }

  /**
   * Remove and return the last element in the list.
   *
   * @return the former last element in the list
   * @throws NoSuchElementException if the list is empty
   */
  public Object removeLast()
  {
    if (size == 0)
      throw new NoSuchElementException();
    modCount++;
    size--;
    Object r = last.data;

    if (last.previous != null)
      last.previous.next = null;
    else
      first = null;

    last = last.previous;

    return r;
  }

  /**
   * Insert an element at the first of the list.
   *
   * @param o the element to insert
   */
  public void addFirst(Object o)
  {
    Entry e = new Entry(o);

    modCount++;
    if (size == 0)
      first = last = e;
    else
      {
        e.next = first;
        first.previous = e;
        first = e;
      }
    size++;
  }

  /**
   * Insert an element at the last of the list.
   *
   * @param o the element to insert
   */
  public void addLast(Object o)
  {
    addLastEntry(new Entry(o));
  }

  /**
   * Inserts an element at the end of the list.
   *
   * @param e the entry to add
   */
  private void addLastEntry(Entry e)
  {
    modCount++;
    if (size == 0)
      first = last = e;
    else
      {
        e.previous = last;
        last.next = e;
        last = e;
      }
    size++;
  }

  /**
   * Returns true if the list contains the given object. Comparison is done by
   * <code>o == null ? e = null : o.equals(e)</code>.
   *
   * @param o the element to look for
   * @return true if it is found
   */
  public boolean contains(Object o)
  {
    Entry e = first;
    while (e != null)
      {
        if (equals(o, e.data))
          return true;
        e = e.next;
      }
    return false;
  }

  /**
   * Returns the size of the list.
   *
   * @return the list size
   */
  public int size()
  {
    return size;
  }

  /**
   * Adds an element to the end of the list.
   *
   * @param e the entry to add
   * @return true, as it always succeeds
   */
  public boolean add(Object o)
  {
    addLastEntry(new Entry(o));
    return true;
  }

  /**
   * Removes the entry at the lowest index in the list that matches the given
   * object, comparing by <code>o == null ? e = null : o.equals(e)</code>.
   *
   * @param o the object to remove
   * @return true if an instance of the object was removed
   */
  public boolean remove(Object o)
  {
    Entry e = first;
    while (e != null)
      {
        if (equals(o, e.data))
          {
            removeEntry(e);
            return true;
          }
        e = e.next;
      }
    return false;
  }

  /**
   * Append the elements of the collection in iteration order to the end of
   * this list. If this list is modified externally (for example, if this
   * list is the collection), behavior is unspecified.
   *
   * @param c the collection to append
   * @return true if the list was modified
   * @throws NullPointerException if c is null
   */
  public boolean addAll(Collection c)
  {
    return addAll(size, c);
  }

  /**
   * Insert the elements of the collection in iteration order at the given
   * index of this list. If this list is modified externally (for example,
   * if this list is the collection), behavior is unspecified.
   *
   * @param c the collection to append
   * @return true if the list was modified
   * @throws NullPointerException if c is null
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
  public boolean addAll(int index, Collection c)
  {
    checkBoundsInclusive(index);
    int csize = c.size();

    if (csize == 0)
      return false;

    Iterator itr = c.iterator();

    // Get the entries just before and after index. If index is at the start
    // of the list, BEFORE is null. If index is at the end of the list, AFTER
    // is null. If the list is empty, both are null.
    Entry after = null;
    Entry before = null;
    if (index != size)
      {
        after = getEntry(index);
        before = after.previous;
      }
    else
      before = last;

    // Create the first new entry. We do not yet set the link from `before'
    // to the first entry, in order to deal with the case where (c == this).
    // [Actually, we don't have to handle this case to fufill the
    // contract for addAll(), but Sun's implementation appears to.]
    Entry e = new Entry(itr.next());
    e.previous = before;
    Entry prev = e;
    Entry firstNew = e;

    // Create and link all the remaining entries.
    for (int pos = 1; pos < csize; pos++)
      {
        e = new Entry(itr.next());
        e.previous = prev;
        prev.next = e;
        prev = e;
      }

    // Link the new chain of entries into the list.
    modCount++;
    size += csize;
    prev.next = after;
    if (after != null)
      after.previous = e;
    else
      last = e;

    if (before != null)
      before.next = firstNew;
    else
      first = firstNew;
    return true;
  }

  /**
   * Remove all elements from this list.
   */
  public void clear()
  {
    if (size > 0)
      {
        modCount++;
        first = null;
        last = null;
        size = 0;
      }
  }

  /**
   * Return the element at index.
   *
   * @param index the place to look
   * @return the element at index
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   */
  public Object get(int index)
  {
    checkBoundsExclusive(index);
    return getEntry(index).data;
  }

  /**
   * Replace the element at the given location in the list.
   *
   * @param index which index to change
   * @param o the new element
   * @return the prior element
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   */
  public Object set(int index, Object o)
  {
    checkBoundsExclusive(index);
    Entry e = getEntry(index);
    Object old = e.data;
    e.data = o;
    return old;
  }

  /**
   * Inserts an element in the given position in the list.
   *
   * @param index where to insert the element
   * @param o the element to insert
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
  public void add(int index, Object o)
  {
    checkBoundsInclusive(index);
    Entry e = new Entry(o);

    if (index < size)
      {
        modCount++;
        Entry after = getEntry(index);
        e.next = after;
        e.previous = after.previous;
        if (after.previous == null)
          first = e;
        else
          after.previous.next = e;
        after.previous = e;
        size++;
      }
    else
      addLastEntry(e);
  }

  /**
   * Removes the element at the given position from the list.
   *
   * @param index the location of the element to remove
   * @return the removed element
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
  public Object remove(int index)
  {
    checkBoundsExclusive(index);
    Entry e = getEntry(index);
    removeEntry(e);
    return e.data;
  }

  /**
   * Returns the first index where the element is located in the list, or -1.
   *
   * @param o the element to look for
   * @return its position, or -1 if not found
   */
  public int indexOf(Object o)
  {
    int index = 0;
    Entry e = first;
    while (e != null)
      {
        if (equals(o, e.data))
          return index;
        index++;
        e = e.next;
      }
    return -1;
  }

  /**
   * Returns the last index where the element is located in the list, or -1.
   *
   * @param o the element to look for
   * @return its position, or -1 if not found
   */
  public int lastIndexOf(Object o)
  {
    int index = size - 1;
    Entry e = last;
    while (e != null)
      {
        if (equals(o, e.data))
          return index;
        index--;
        e = e.previous;
      }
    return -1;
  }

  /**
   * Obtain a ListIterator over this list, starting at a given index. The
   * ListIterator returned by this method supports the add, remove and set
   * methods.
   *
   * @param index the index of the element to be returned by the first call to
   *        next(), or size() to be initially positioned at the end of the list
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
  public ListIterator listIterator(int index)
  {
    checkBoundsInclusive(index);
    return new LinkedListItr(index);
  }

  /**
   * Create a shallow copy of this LinkedList (the elements are not cloned).
   *
   * @return an object of the same class as this object, containing the
   *         same elements in the same order
   */
  public Object clone()
  {
    LinkedList copy = null;
    try
      {
        copy = (LinkedList) super.clone();
      }
    catch (CloneNotSupportedException ex)
      {
      }
    copy.clear();
    copy.addAll(this);
    return copy;
  }

  /**
   * Returns an array which contains the elements of the list in order.
   *
   * @return an array containing the list elements
   */
  public Object[] toArray()
  {
    Object[] array = new Object[size];
    Entry e = first;
    for (int i = 0; i < size; i++)
      {
        array[i] = e.data;
        e = e.next;
      }
    return array;
  }

  /**
   * Returns an Array whose component type is the runtime component type of
   * the passed-in Array.  The returned Array is populated with all of the
   * elements in this LinkedList.  If the passed-in Array is not large enough
   * to store all of the elements in this List, a new Array will be created 
   * and returned; if the passed-in Array is <i>larger</i> than the size
   * of this List, then size() index will be set to null.
   *
   * @param a the passed-in Array
   * @return an array representation of this list
   * @throws ArrayStoreException if the runtime type of a does not allow
   *         an element in this list
   * @throws NullPointerException if a is null
   */
  public Object[] toArray(Object[] a)
  {
    if (a.length < size)
      a = (Object[]) Array.newInstance(a.getClass().getComponentType(), size);
    else if (a.length > size)
      a[size] = null;
    Entry e = first;
    for (int i = 0; i < size; i++)
      {
        a[i] = e.data;
        e = e.next;
      }
    return a;
  }

  /**
   * Serializes this object to the given stream.
   *
   * @param s the stream to write to
   * @throws IOException if the underlying stream fails
   * @serialData the size of the list (int), followed by all the elements
   *             (Object) in proper order
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.defaultWriteObject();
    s.writeInt(size);
    Entry e = first;
    while (e != null)
      {
        s.writeObject(e.data);
        e = e.next;
      }
  }

  /**
   * Deserializes this object from the given stream.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if the underlying stream fails
   * @throws IOException if the underlying stream fails
   * @serialData the size of the list (int), followed by all the elements
   *             (Object) in proper order
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    s.defaultReadObject();
    int i = s.readInt();
    while (--i >= 0)
      addLastEntry(new Entry(s.readObject()));
  }

  /**
   * A ListIterator over the list. This class keeps track of its
   * position in the list and the two list entries it is between.
   *
   * @author Original author unknown
   * @author Eric Blake (ebb9@email.byu.edu)
   */
  private final class LinkedListItr implements ListIterator
  {
    /** Number of modifications we know about. */
    private int knownMod = modCount;

    /** Entry that will be returned by next(). */
    private Entry next;

    /** Entry that will be returned by previous(). */
    private Entry previous;

    /** Entry that will be affected by remove() or set(). */
    private Entry lastReturned;

    /** Index of `next'. */
    private int position;

    /**
     * Initialize the iterator.
     *
     * @param index the initial index
     */
    LinkedListItr(int index)
    {
      if (index == size)
        {
          next = null;
          previous = last;
        }
      else
        {
          next = getEntry(index);
          previous = next.previous;
        }
      position = index;
    }

    /**
     * Checks for iterator consistency.
     *
     * @throws ConcurrentModificationException if the list was modified
     */
    private void checkMod()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
    }

    /**
     * Returns the index of the next element.
     *
     * @return the next index
     * @throws ConcurrentModificationException if the list was modified
     */
    public int nextIndex()
    {
      checkMod();
      return position;
    }

    /**
     * Returns the index of the previous element.
     *
     * @return the previous index
     * @throws ConcurrentModificationException if the list was modified
     */
    public int previousIndex()
    {
      checkMod();
      return position - 1;
    }

    /**
     * Returns true if more elements exist via next.
     *
     * @return true if next will succeed
     * @throws ConcurrentModificationException if the list was modified
     */
    public boolean hasNext()
    {
      checkMod();
      return (next != null);
    }

    /**
     * Returns true if more elements exist via previous.
     *
     * @return true if previous will succeed
     * @throws ConcurrentModificationException if the list was modified
     */
    public boolean hasPrevious()
    {
      checkMod();
      return (previous != null);
    }

    /**
     * Returns the next element.
     *
     * @return the next element
     * @throws ConcurrentModificationException if the list was modified
     * @throws NoSuchElementException if there is no next
     */
    public Object next()
    {
      checkMod();
      if (next == null)
        throw new NoSuchElementException();
      position++;
      lastReturned = previous = next;
      next = lastReturned.next;
      return lastReturned.data;
    }

    /**
     * Returns the previous element.
     *
     * @return the previous element
     * @throws ConcurrentModificationException if the list was modified
     * @throws NoSuchElementException if there is no previous
     */
    public Object previous()
    {
      checkMod();
      if (previous == null)
        throw new NoSuchElementException();
      position--;
      lastReturned = next = previous;
      previous = lastReturned.previous;
      return lastReturned.data;
    }

    /**
     * Remove the most recently returned element from the list.
     *
     * @throws ConcurrentModificationException if the list was modified
     * @throws IllegalStateException if there was no last element
     */
    public void remove()
    {
      checkMod();
      if (lastReturned == null)
        throw new IllegalStateException();

      // Adjust the position to before the removed element, if the element
      // being removed is behind the cursor.
      if (lastReturned == previous)
        position--;

      next = lastReturned.next;
      previous = lastReturned.previous;
      removeEntry(lastReturned);
      knownMod++;

      lastReturned = null;
    }

    /**
     * Adds an element between the previous and next, and advance to the next.
     *
     * @param o the element to add
     * @throws ConcurrentModificationException if the list was modified
     */
    public void add(Object o)
    {
      checkMod();
      modCount++;
      knownMod++;
      size++;
      position++;
      Entry e = new Entry(o);
      e.previous = previous;
      e.next = next;

      if (previous != null)
        previous.next = e;
      else
        first = e;

      if (next != null)
        next.previous = e;
      else
        last = e;

      previous = e;
      lastReturned = null;
    }

    /**
     * Changes the contents of the element most recently returned.
     *
     * @param o the new element
     * @throws ConcurrentModificationException if the list was modified
     * @throws IllegalStateException if there was no last element
     */
    public void set(Object o)
    {
      checkMod();
      if (lastReturned == null)
        throw new IllegalStateException();
      lastReturned.data = o;
    }
  } // class LinkedListItr
}
