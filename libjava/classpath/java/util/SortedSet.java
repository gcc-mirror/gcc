/* SortedSet.java -- A set that makes guarantees about the order of its
   elements
   Copyright (C) 1998, 2001, 2004, 2005 Free Software Foundation, Inc.

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
 * A set which guarantees its iteration order. The elements in the set
 * are related by the <i>natural ordering</i> if they are Comparable, or
 * by the provided Comparator.  Additional operations take advantage of
 * the sorted nature of the set.
 * <p>
 *
 * All elements entered in the set must be mutually comparable; in other words,
 * <code>k1.compareTo(k2)</code> or <code>comparator.compare(k1, k2)</code>
 * must not throw a ClassCastException. The ordering must be <i>consistent
 * with equals</i> (see {@link Comparator} for this definition), if the
 * set is to obey the general contract of the Set interface.  If not,
 * the results are well-defined, but probably not what you wanted.
 * <p>
 *
 * It is recommended that all implementing classes provide four constructors:
 * 1) one that takes no arguments and builds an empty set sorted by natural
 * order of the elements; 2) one that takes a Comparator for the sorting order;
 * 3) one that takes a Set and sorts according to the natural order of its
 * elements; and 4) one that takes a SortedSet and sorts by the same
 * comparator. Unfortunately, the Java language does not provide a way to
 * enforce this.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Set
 * @see TreeSet
 * @see SortedMap
 * @see Collection
 * @see Comparable
 * @see Comparator
 * @see ClassCastException
 * @since 1.2
 * @status updated to 1.4
 */
public interface SortedSet<E> extends Set<E>
{
  /**
   * Returns the comparator used in sorting this set, or null if it is
   * the elements' natural ordering.
   *
   * @return the sorting comparator
   */
  Comparator<? super E> comparator();

  /**
   * Returns the first (lowest sorted) element in the set.
   *
   * @return the first element
   * @throws NoSuchElementException if the set is empty.
   */
  E first();

  /**
   * Returns a view of the portion of the set strictly less than toElement. The
   * view is backed by this set, so changes in one show up in the other.
   * The subset supports all optional operations of the original.
   * <p>
   *
   * The returned set throws an IllegalArgumentException any time an element is
   * used which is out of the range of toElement. Note that the endpoint, toElement,
   * is not included; if you want this value included, pass its successor object in to
   * toElement.  For example, for Integers, you could request
   * <code>headSet(new Integer(limit.intValue() + 1))</code>.
   *
   * @param toElement the exclusive upper range of the subset
   * @return the subset
   * @throws ClassCastException if toElement is not comparable to the set
   *         contents
   * @throws IllegalArgumentException if this is a subSet, and toElement is out
   *         of range
   * @throws NullPointerException if toElement is null but the set does not
   *         allow null elements
   */
  SortedSet<E> headSet(E toElement);

  /**
   * Returns the last (highest sorted) element in the set.
   *
   * @return the last element
   * @throws NoSuchElementException if the set is empty.
   */
  E last();

  /**
   * Returns a view of the portion of the set greater than or equal to
   * fromElement, and strictly less than toElement. The view is backed by
   * this set, so changes in one show up in the other. The subset supports all
   * optional operations of the original.
   * <p>
   *
   * The returned set throws an IllegalArgumentException any time an element is
   * used which is out of the range of fromElement and toElement. Note that the
   * lower endpoint is included, but the upper is not; if you want to
   * change the inclusion or exclusion of an endpoint, pass its successor
   * object in instead.  For example, for Integers, you can request
   * <code>subSet(new Integer(lowlimit.intValue() + 1),
   * new Integer(highlimit.intValue() + 1))</code> to reverse
   * the inclusiveness of both endpoints.
   *
   * @param fromElement the inclusive lower range of the subset
   * @param toElement the exclusive upper range of the subset
   * @return the subset
   * @throws ClassCastException if fromElement or toElement is not comparable
   *         to the set contents
   * @throws IllegalArgumentException if this is a subSet, and fromElement or
   *         toElement is out of range
   * @throws NullPointerException if fromElement or toElement is null but the
   *         set does not allow null elements
   */
  SortedSet<E> subSet(E fromElement, E toElement);

  /**
   * Returns a view of the portion of the set greater than or equal to
   * fromElement. The view is backed by this set, so changes in one show up
   * in the other. The subset supports all optional operations of the original.
   * <p>
   *
   * The returned set throws an IllegalArgumentException any time an element is
   * used which is out of the range of fromElement. Note that the endpoint,
   * fromElement, is included; if you do not want this value to be included, pass its
   * successor object in to fromElement.  For example, for Integers, you could request
   * <code>tailSet(new Integer(limit.intValue() + 1))</code>.
   *
   * @param fromElement the inclusive lower range of the subset
   * @return the subset
   * @throws ClassCastException if fromElement is not comparable to the set
   *         contents
   * @throws IllegalArgumentException if this is a subSet, and fromElement is
   *         out of range
   * @throws NullPointerException if fromElement is null but the set does not
   *         allow null elements
   */
  SortedSet<E> tailSet(E fromElement);
}
