/* LinkedList.java -- Linked list implementation of the List interface
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
import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.lang.reflect.Array;

// TO DO:
// ~ Doc comment for the class.
// ~ Doc comments for the non-list methods.
// ~ other general implementation notes.

/**
 * Linked list implementation of the List interface.
 */
public class LinkedList extends AbstractSequentialList
  implements List, Cloneable, Serializable
{
  static final long serialVersionUID = 876323262645176354L;

  /**
   * An Entry containing the head (in the next field) and the tail (in the
   * previous field) of the list. The data field is null. If the list is empty,
   * both the head and the tail point to ends itself.
   */
  transient Entry first;
  transient Entry last;

  /**
   * The current length of the list.
   */
  transient int size = 0;

  /**
   * Class to represent an entry in the list. Holds a single element.
   */
  private static class Entry
  {
    Object data;
    Entry next;
    Entry previous;
    
    Entry(Object data)
    {
      this.data = data;
    }
  }
  
  /**
   * Obtain the Entry at a given position in a list. This method of course
   * takes linear time, but it is intelligent enough to take the shorter of the
   * paths to get to the Entry required. This implies that the first or last
   * entry in the list is obtained in constant time, which is a very desirable
   * property.
   * For speed and flexibility, range checking is not done in this method:
   * Incorrect values will be returned if (n < 0) or (n >= size).
   *
   * @param n the number of the entry to get.
   */
  private Entry getEntry(int n)
  {
    Entry e;
    if (n < size / 2)
      {
        e = first;
	// n less than size/2, iterate from start
	while (n-- > 0)
	  {
	    e = e.next;
	  }
      }
    else
      {
        e = last;      
	// n greater than size/2, iterate from end
	while (++n < size)
	  {
	    e = e.previous;
	  }
      }
    return e;
  }
  
  /** Remove an entry from the list. This will adjust size and deal with
   *  `first' and  `last' appropriatly. It does not effect modCount, that is 
   *  the responsibility of the caller. */
  private void removeEntry(Entry e)
  {
    if (size == 1)
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
    size--;
  }

  /**
   * Create an empty linked list.
   */
  public LinkedList()
  {
    super();
  }

  /**
   * Create a linked list containing the elements, in order, of a given
   * collection.
   *
   * @param c the collection to populate this list from.
   */
  public LinkedList(Collection c)
  {
    super();
    // Note: addAll could be made slightly faster, but not enough so to justify
    // re-implementing it from scratch. It is just a matter of a relatively
    // small constant factor.
    addAll(c);
  }

  public Object getFirst()
  {
    if (size == 0)
      throw new NoSuchElementException();
    return first.data;
  }

  public Object getLast()
  {
    if (size == 0)
      throw new NoSuchElementException();
    return last.data;
  }

  public Object removeFirst()
  {
    if (size == 0)
      throw new NoSuchElementException();
    size--;
    modCount++;
    Object r = first.data;
    
    if (first.next != null)
      first.next.previous = null;
    else
      last = null;

    first = first.next;
    
    return r;
  }

  public Object removeLast()
  {
    if (size == 0)
      throw new NoSuchElementException();
    size--;
    modCount++;
    Object r = last.data;
    
    if (last.previous != null)
      last.previous.next = null;
    else
      first = null;
    
    last = last.previous;
    
    return r;
  }

  public void addFirst(Object o)
  {
    modCount++;
    Entry e = new Entry(o);
    
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

  public void addLast(Object o)
  {
    modCount++;
    addLastEntry(new Entry(o));
  }
  
  private void addLastEntry(Entry e)
  {
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

  public boolean contains(Object o)
  {
    Entry e = first;
    while (e != null)
      {
        if (e.data == null ? o == null : o.equals(e.data))
	  return true;
        e = e.next;
      }
    return false;
  }

  public int size()
  {
    return size;
  }
  
  public boolean add(Object o)
  {
    modCount++;
    addLastEntry(new Entry(o));
    return true;
  }
  
  public boolean remove(Object o)
  {
    modCount++;
    Entry e = first;
    while (e != null)
      {
        if (e.data == null ? o == null : o.equals(e.data))
	  {
	    removeEntry(e);
	    return true;
	  }
        e = e.next;
      }
    return false;
  }

  public boolean addAll(Collection c)
  {
    return addAll(size, c);
  }
  
  public boolean addAll(int index, Collection c)
  {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    modCount++;
    int csize = c.size();

    if (csize == 0)
      return false;

    Iterator itr = c.iterator();
    
    // Get the entries just before and after index. If index is at the start
    // of the list, BEFORE is null. If index is at the end of thelist, AFTER is
    // null. If the list is empty, both are null.
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
    prev.next = after;
    if (after != null)
      after.previous = e;
    else
      last = e;
    
    if (before != null)
      before.next = firstNew;
    else
      first = firstNew;
    
    size += csize;
    return true;
  }

  public void clear()
  {
    modCount++;
    first = null;
    last = null;
    size = 0;
  }

  public Object get(int index)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    Entry e = getEntry(index);
    return e.data;
  }
  
  public Object set(int index, Object o)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    Entry e = getEntry(index);
    Object old = e.data;
    e.data = o;
    return old;
  }

  public void add(int index, Object o)
  {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    modCount++;
    addEntry(index, new Entry(o));    
  }
  
  private void addEntry(int index, Entry e)
  {
    if (index < size)
      {
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
  
  public Object remove(int index)
  {
    if (index < 0 || index >= size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    modCount++;
    Entry e = getEntry(index);
    removeEntry(e);
    return e.data;
  }
  
  public int indexOf(Object o)
  {
    int index = 0;
    Entry e = first;
    while (e != null)
      {
        if (e.data == null ? o == null : o.equals(e.data))
	  return index;
	++index;
        e = e.next;
      }
    return -1;
  }
  
  public int lastIndexOf(Object o)
  {
    int index = size - 1;
    Entry e = last;
    while (e != null)
      {
        if (e.data == null ? o == null : o.equals(e.data))
	  return index;
	--index;
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
   *   next(), or size() to be initially positioned at the end of the list.
   * @exception IndexOutOfBoundsException if index < 0 || index > size().
   */
  public ListIterator listIterator(int index)
  {
    if (index < 0 || index > size)
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size);
    return new LinkedListItr(index);
  }

  /**
   * Create a shallow copy of this LinkedList.
   * @return an object of the same class as this object, containing the
   * same elements in the same order.
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
  
  public Object[] toArray(Object[] array)
  {
    if (array.length < size)
      array = (Object[]) Array.newInstance(array.getClass().getComponentType(), 
        				   size);
    else if (array.length > size)
      array[size] = null;
    Entry e = first;
    for (int i = 0; i < size; i++)
      {
        array[i] = e.data;
        e = e.next;
      }
    return array;  
  }

  /**
   * Serialize an object to a stream.
   * @serialdata the size of the list (int), followed by all the elements
   * (Object) in proper order.
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    s.writeInt(size);
    Iterator itr = iterator();
    for (int i = 0; i < size; i++)
      s.writeObject(itr.next());
  }

  /**
   * Deserialize an object from a stream.
   * @serialdata the size of the list (int), followed by all the elements
   * (Object) in proper order.
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    int serialSize = s.readInt();
    for (int i = 0; i < serialSize; i++)
      addLastEntry(new Entry(s.readObject()));
  }
  
  /** A ListIterator over the list. This class keeps track of its
   * position in the list and the two list entries it is between.
   */
  class LinkedListItr implements ListIterator
  {
    int knownMod;
    Entry next;         // entry that will be returned by next().
    Entry previous;     // entry that will be returned by previous().
    Entry lastReturned; // entry that will be affected by remove() or set().
    int position;       // index of `next'.

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
      knownMod = modCount;
    }

    private void checkMod()
    {
      if (knownMod != modCount)
	throw new ConcurrentModificationException();
    }

    public int nextIndex()
    {
      checkMod();
      return position;
    }

    public int previousIndex()
    {
      checkMod();
      return position - 1;
    }

    public boolean hasNext()
    {
      checkMod();
      return (next != null);
    }

    public boolean hasPrevious()
    {
      checkMod();
      return (previous != null);
    }

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
      modCount++;
      knownMod++;
      removeEntry(lastReturned);
      
      lastReturned = null;
    }

    public void add(Object o)
    {
      checkMod();
      modCount++;
      knownMod++;
      Entry e = new Entry(o);
      e.previous = previous;
      e.next = next;

      if (previous != null)
	previous.next = e;
      else
	first = e;

      if (next != null)
        {
	  next.previous = e;
	  next = next.next;
	}
      else
	last = e;

      previous = e;
      size++;
      position++;
      lastReturned = null;
    }

    public void set(Object o)
    {
      checkMod();
      if (lastReturned == null)
	throw new IllegalStateException();
      lastReturned.data = o;
    }
  }  // class LinkedListItr  
}
