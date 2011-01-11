/* NamingEnumeration.java -- The JNDI enumeration
   Copyright (C) 2000, 2006 Free Software Foundation, Inc.

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


package javax.naming;

import java.util.Enumeration;

/**
 * <p>The specific type of enumeration that supports throwing various exceptions by
 * the hasMore method. The exceptions are only thrown if the enumeration is
 * scanned using {@link #next()} and {@link #hasMore()}. If the inherited
 * {@link java.util.Enumeration#nextElement()} and
 * {@link Enumeration#hasMoreElements()} are used instead, the exceptions are
 * not throwed, and the enumeration is just iterated over available elements.
 * </p>
 * <p>This enumeration becomes invalid after throwing the exception. If the
 * exception has been thrown, not other method should be called of that
 * enumeration.</p>
 */
public interface NamingEnumeration<T> extends Enumeration<T>
{
  /**
   * Returns the next element in this enumeration. The naming - specific
   * exceptions are only throws after returning all still available elements of
   * the enumeration.
   *
   * @return the next element of this enumeration
   * @throws NamingException
   */
  T next() throws NamingException;

  /**
   * Checks if there are more unvisited elements in the enumeration, throwing
   * exceptions if there are some unvisited, but not available elements.
   *
   * @return true if there are some unvisited elements, false otherwise.
   * @throws PartialResultException if the enumeration, returned by the
   *           {@link Context#list(Name)} or other similar method contains only
   *           partial answer.
   * @throws SizeLimitExceededException if remaining elements are not available
   *           because of the previously specified size limit.
   * @throws NamingException
   */
  boolean hasMore() throws NamingException;

  /**
   * Immediately frees all resources, owned by this enumeration. If invoked, it
   * must be the last method called for that enumeration.
   *
   * @throws NamingException
   */
  void close() throws NamingException;

}
