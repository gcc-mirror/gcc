/* ListIterator.java -- Extended Iterator for iterating over ordered lists
   Copyright (C) 1998, 1999, 2001, 2005  Free Software Foundation, Inc.

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
 * An extended version of Iterator to support the extra features of Lists. The
 * elements may be accessed in forward or reverse order, elements may be
 * replaced as well as removed, and new elements may be inserted, during the
 * traversal of the list.
 * <p>
 *
 * A list with n elements provides n+1 iterator positions (the front, the end,
 * or between two elements). Note that <code>remove</code> and <code>set</code>
 * operate on the last element returned, whether it was by <code>next</code>
 * or <code>previous</code>.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see List
 * @see Iterator
 * @see Enumeration
 * @since 1.2
 * @status updated to 1.4
 */
public interface ListIterator extends Iterator
{
  /**
   * Tests whether there are elements remaining in the list in the forward
   * direction. In other words, next() will not fail with a
   * NoSuchElementException.
   *
   * @return true if the list continues in the forward direction
   */
  boolean hasNext();

  /**
   * Tests whether there are elements remaining in the list in the reverse
   * direction. In other words, previous() will not fail with a
   * NoSuchElementException.
   *
   * @return true if the list continues in the reverse direction
   */
  boolean hasPrevious();

  /**
   * Obtain the next element in the list in the forward direction. Repeated
   * calls to next may be used to iterate over the entire list, or calls to
   * next and previous may be used together to go forwards and backwards.
   * Alternating calls to next and previous will return the same element.
   *
   * @return the next element in the list in the forward direction
   * @throws NoSuchElementException if there are no more elements
   */
  Object next();

  /**
   * Obtain the next element in the list in the reverse direction. Repeated
   * calls to previous may be used to iterate backwards over the entire list,
   * or calls to next and previous may be used together to go forwards and
   * backwards. Alternating calls to next and previous will return the same
   * element.
   *
   * @return the next element in the list in the reverse direction
   * @throws NoSuchElementException if there are no more elements
   */
  Object previous();

  /**
   * Find the index of the element that would be returned by a call to next.
   * If hasNext() returns false, this returns the list size.
   *
   * @return the index of the element that would be returned by next()
   */
  int nextIndex();

  /**
   * Find the index of the element that would be returned by a call to
   * previous. If hasPrevious() returns false, this returns -1.
   *
   * @return the index of the element that would be returned by previous()
   */
  int previousIndex();

  /**
   * Insert an element into the list at the current position of the iterator
   * (optional operation). The element is inserted in between the element that
   * would be returned by previous and the element that would be returned by
   * next. After the insertion, a subsequent call to next is unaffected, but
   * a call to previous returns the item that was added. The values returned
   * by nextIndex() and previousIndex() are incremented.
   *
   * @param o the object to insert into the list
   * @throws ClassCastException if the object is of a type which cannot be added
   *         to this list.
   * @throws IllegalArgumentException if some other aspect of the object stops
   *         it being added to this list.
   * @throws UnsupportedOperationException if this ListIterator does not
   *         support the add operation.
   */
  void add(Object o);

  /**
   * Remove from the list the element last returned by a call to next or
   * previous (optional operation). This method may only be called if neither
   * add nor remove have been called since the last call to next or previous.
   *
   * @throws IllegalStateException if neither next or previous have been
   *         called, or if add or remove has been called since the last call
   *         to next or previous
   * @throws UnsupportedOperationException if this ListIterator does not
   *         support the remove operation
   */
  void remove();

  /**
   * Replace the element last returned by a call to next or previous with a
   * given object (optional operation). This method may only be called if
   * neither add nor remove have been called since the last call to next or
   * previous.
   *
   * @param o the object to replace the element with
   * @throws ClassCastException the object is of a type which cannot be added
   *         to this list
   * @throws IllegalArgumentException some other aspect of the object stops
   *         it being added to this list
   * @throws IllegalStateException if neither next or previous have been
   *         called, or if add or remove has been called since the last call
   *         to next or previous
   * @throws UnsupportedOperationException if this ListIterator does not
   *         support the set operation
   */
  void set(Object o);
}
