/* ListIterator.java -- Extended Iterator for iterating over ordered lists
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

/**
 * An extended version of Iterator to support the extra features of Lists. The
 * elements may be accessed in forward or reverse order, elements may be
 * replaced as well as removed, and new elements may be inserted, during the
 * traversal of the list.
 */
public interface ListIterator extends Iterator
{
  /**
   * Tests whether there are elements remaining in the list in the forward
   * direction.
   *
   * @return true if there is at least one more element in the list in the
   *   forward direction, that is, if the next call to next will not throw
   *   NoSuchElementException.
   */
  boolean hasNext();

  /**
   * Tests whether there are elements remaining in the list in the reverse
   * direction.
   *
   * @return true if there is at least one more element in the list in the
   *   reverse direction, that is, if the next call to previous will not throw
   *   NoSuchElementException.
   */
  boolean hasPrevious();

  /**
   * Obtain the next element in the list in the forward direction. Repeated
   * calls to next may be used to iterate over the entire list, or calls to next
   * and previous may be used together to go forwards and backwards. Alternating
   * calls to next and previous will return the same element.
   *
   * @return the next element in the list in the forward direction
   * @exception NoSuchElementException if there are no more elements
   */
  Object next();

  /**
   * Obtain the next element in the list in the reverse direction. Repeated
   * calls to previous may be used to iterate backwards over the entire list, or
   * calls to next and previous may be used together to go forwards and
   * backwards. Alternating calls to next and previous will return the same
   * element.
   *
   * @return the next element in the list in the reverse direction
   * @exception NoSuchElementException if there are no more elements
   */
  Object previous();

  /**
   * Find the index of the element that would be returned by a call to next.
   *
   * @return the index of the element that would be returned by a call to next,
   *   or list.size() if the iterator is at the end of the list.
   */
  int nextIndex();

  /**
   * Find the index of the element that would be returned by a call to previous.
   *
   * @return the index of the element that would be returned by a call to
   *   previous, or -1 if the iterator is at the beginning of the list.
   */
  int previousIndex();

  /**
   * Insert an element into the list at the current position of the iterator.
   * The element is inserted in between the element that would be returned by
   * previous and the element that would be returned by next. After the
   * insertion, a subsequent call to next is unaffected, but a call to
   * previous returns the item that was added. This operation is optional, it
   * may throw an UnsupportedOperationException.
   *
   * @param o the object to insert into the list
   * @exception ClassCastException the object is of a type which cannot be added
   *   to this list
   * @exception IllegalArgumentException some other aspect of the object stops
   *   it being added to this list
   * @exception UnsupportedOperationException if this ListIterator does not
   *   support the add operation
   */
  void add(Object o);

  /**
   * Remove from the list the element last returned by a call to next or
   * previous. This method may only be called if neither add nor remove have
   * been called since the last call to next or previous. This operation is
   * optional, it may throw an UnsupportedOperationException.
   *
   * @exception IllegalStateException if neither next or previous have been
   *   called, or if add or remove has been called since the last call to next
   *   or previous.
   * @exception UnsupportedOperationException if this ListIterator does not
   *   support the remove operation.
   */
  void remove();

  /**
   * Replace the element last returned by a call to next or previous with a
   * given object. This method may only be called if neither add nor remove have
   * been called since the last call to next or previous. This operation is
   * optional, it may throw an UnsupportedOperationException.
   *
   * @param o the object to replace the element with
   * @exception ClassCastException the object is of a type which cannot be added
   *   to this list
   * @exception IllegalArgumentException some other aspect of the object stops
   *   it being added to this list
   * @exception IllegalStateException if neither next or previous have been
   *   called, or if add or remove has been called since the last call to next
   *   or previous.
   * @exception UnsupportedOperationException if this ListIterator does not
   *   support the set operation.
   */
  void set(Object o);
}
