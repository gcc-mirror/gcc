/* AbstractSequentialList.java -- List implementation for sequential access
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


// TO DO:
// ~ Lots of doc comments still missing.
// ~ The class comment should include a description of what should be overridden
//   to provide what features, as should the listIterator comment.

package java.util;

/**
 * Abstract superclass to make it easier to implement the List interface when
 * backed by a sequential-access store, such as a linked list.
 */
public abstract class AbstractSequentialList extends AbstractList
{
  /**
   * Returns a ListIterator over the list, starting from position index.
   * Subclasses must provide an implementation of this method.
   *
   * @exception IndexOutOfBoundsException if index < 0 || index > size()
   */
  public abstract ListIterator listIterator(int index);

  /**
   * Add an element to the list at a given index. This implementation obtains a
   * ListIterator positioned at the specified index, and then adds the element
   * using the ListIterator's add method.
   *
   * @param index the position to add the element
   * @param o the element to insert
   * @exception IndexOutOfBoundsException if index < 0 || index > size()
   * @exception UnsupportedOperationException if the iterator returned by
   *   listIterator(index) does not support the add method.
   */
  public void add(int index, Object o)
  {
    ListIterator i = listIterator(index);
    i.add(o);
  }

  /**
   * @specnote The spec in the JDK1.3 online docs is wrong. The implementation
   *           should not call next() to skip over new elements as they are
   *           added, because iterator.add() should add new elements BEFORE
   *           the cursor.
   */
  public boolean addAll(int index, Collection c)
  {
    boolean modified = false;
    Iterator ci = c.iterator();
    int size = c.size();
    ListIterator i = listIterator(index);
    for (int pos = 0; pos < size; pos++)
      {
	i.add(ci.next());
      }
    return (size > 0);
  }

  public Object get(int index)
  {
    ListIterator i = listIterator(index);
    if (index < 0 || index > size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size());
    return i.next();
  }

  /**
   * Return an Iterator over this List. This implementation returns
   * listIterator().
   *
   * @return an Iterator over this List
   */
  public Iterator iterator()
  {
    return listIterator();
  }

  public Object remove(int index)
  {
    ListIterator i = listIterator(index);
    if (index < 0 || index > size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size());
    Object removed = i.next();
    i.remove();
    return removed;
  }

  public Object set(int index, Object o)
  {
    ListIterator i = listIterator(index);
    if (index < 0 || index > size())
      throw new IndexOutOfBoundsException("Index: " + index + ", Size:" + 
                                          size());
    Object old = i.next();
    i.set(o);
    return old;
  }
}
