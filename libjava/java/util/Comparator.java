/* Comparator.java -- Interface for objects that specify an ordering
   Copyright (C) 1998, 2001 Free Software Foundation, Inc.

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
 * Interface for objects that specify an ordering between objects. The ordering
 * should be <em>total</em>, such that any two objects of the correct type
 * can be compared, and the comparison is reflexive, anti-symmetric, and
 * transitive.  It is also recommended that the comparator be <em>consistent
 * with equals</em>, although this is not a strict requirement. A relation
 * is consistent with equals if these two statements always have the same
 * results (if no exceptions occur):<br>
 * <code>compare((Object) e1, (Object) e2) == 0</code> and
 * <code>e1.equals((Object) e2)</code><br>
 * Comparators that violate consistency with equals may cause strange behavior
 * in sorted lists and sets.  For example, a case-sensitive dictionary order
 * comparison of Strings is consistent with equals, but if it is
 * case-insensitive it is not, because "abc" and "ABC" compare as equal even
 * though "abc".equals("ABC") returns false.
 * <P>
 * In general, Comparators should be Serializable, because when they are passed
 * to Serializable data structures such as SortedMap or SortedSet, the entire
 * data structure will only serialize correctly if the comparator is
 * Serializable.
 *
 * @author Original author unknown
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Comparable
 * @see TreeMap
 * @see TreeSet
 * @see SortedMap
 * @see SortedSet
 * @see Arrays#sort(Object[], Comparator)
 * @see java.io.Serializable
 * @since 1.2
 * @status updated to 1.4
 */
public interface Comparator
{
  /**
   * Return an integer that is negative, zero or positive depending on whether
   * the first argument is less than, equal to or greater than the second
   * according to this ordering. This method should obey the following
   * contract:
   * <ul>
   *   <li>if compare(a, b) &lt; 0 then compare(b, a) &gt; 0</li>
   *   <li>if compare(a, b) throws an exception, so does compare(b, a)</li>
   *   <li>if compare(a, b) &lt; 0 and compare(b, c) &lt; 0 then compare(a, c)
   *       &lt; 0</li>
   *   <li>if compare(a, b) == 0 then compare(a, c) and compare(b, c) must
   *       have the same sign</li
   * </ul>
   * To be consistent with equals, the following additional constraint is
   * in place:
   * <ul>
   *   <li>if a.equals(b) or both a and b are null, then
   *       compare(a, b) == 0.</li>
   * </ul><p>
   *
   * Although it is permissible for a comparator to provide an order
   * inconsistent with equals, that should be documented.
   *
   * @param o1 the first object
   * @param o2 the second object
   * @return the comparison
   * @throws ClassCastException if the elements are not of types that can be
   *         compared by this ordering.
   */
  int compare(Object o1, Object o2);

  /**
   * Return true if the object is equal to this object.  To be
   * considered equal, the argument object must satisfy the constraints
   * of <code>Object.equals()</code>, be a Comparator, and impose the
   * same ordering as this Comparator. The default implementation
   * inherited from Object is usually adequate.
   *
   * @param obj The object
   * @return true if it is a Comparator that imposes the same order
   * @see Object#equals(Object)
   */
  boolean equals(Object obj);
}
