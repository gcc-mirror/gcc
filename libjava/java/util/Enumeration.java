/* Enumeration.java -- Interface for enumerating lists of objects
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1.
 * Status:  Believed complete and correct
 */

/**
 * Interface for lists of objects that can be returned in sequence. Successive
 * objects are obtained by the nextElement method.
 * <p>
 * As of Java 1.2, the Iterator interface provides the same functionality, but
 * with shorter method names and a new optional method to remove items from the
 * list. If writing for 1.2, consider using Iterator instead. Enumerations over
 * the new collections classes, for use with legacy APIs that require them, can
 * be obtained by the enumeration method in class Collections.
 *
 * @author Warren Levy <warrenl@cygnus.com>
 * @date August 25, 1998.
 */
public interface Enumeration
{
  /**
   * Tests whether there are elements remaining in the enumeration.
   *
   * @return true if there is at least one more element in the enumeration,
   *   that is, if the next call to nextElement will not throw a
   *   NoSuchElementException.
   */
  boolean hasMoreElements();

  /**
   * Obtain the next element in the enumeration.
   *
   * @return the next element in the enumeration
   * @exception NoSuchElementException if there are no more elements
   */
  Object nextElement() throws NoSuchElementException;
}
