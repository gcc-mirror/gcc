/* Iterator.java -- Interface for iterating over collections
   Copyright (C) 1998, 2001, 2005  Free Software Foundation, Inc.

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

/**
 * An object which iterates over a collection. An Iterator is used to return
 * the items once only, in sequence, by successive calls to the next method.
 * It is also possible to remove elements from the underlying collection by
 * using the optional remove method. Iterator is intended as a replacement
 * for the Enumeration interface of previous versions of Java, which did not
 * have the remove method and had less conveniently named methods.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see ListIterator
 * @see Enumeration
 * @since 1.2
 * @status updated to 1.4
 */
public interface Iterator
{
  /**
   * Tests whether there are elements remaining in the collection. In other
   * words, calling <code>next()</code> will not throw an exception.
   *
   * @return true if there is at least one more element in the collection
   */
  boolean hasNext();

  /**
   * Obtain the next element in the collection.
   *
   * @return the next element in the collection
   * @throws NoSuchElementException if there are no more elements
   */
  Object next();

  /**
   * Remove from the underlying collection the last element returned by next
   * (optional operation). This method can be called only once after each
   * call to <code>next()</code>. It does not affect what will be returned
   * by subsequent calls to next.
   *
   * @throws IllegalStateException if next has not yet been called or remove
   *         has already been called since the last call to next.
   * @throws UnsupportedOperationException if this Iterator does not support
   *         the remove operation.
   */
  void remove();
}
