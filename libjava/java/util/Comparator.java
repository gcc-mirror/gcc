/* Comparator.java -- Interface for objects that specify an ordering
   Copyright (C) 1998 Free Software Foundation, Inc.

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
 * Interface for objects that specify an ordering between objects. The ordering
 * can be <EM>total</EM>, such that two objects only compare equal if they are
 * equal by the equals method, or <EM>partial</EM> such that this is not
 * necessarily true. For example, a case-sensitive dictionary order comparison
 * of Strings is total, but if it is case-insensitive it is partial, because
 * "abc" and "ABC" compare as equal even though "abc".equals("ABC") returns
 * false.
 * <P>
 * In general, Comparators should be Serializable, because when they are passed
 * to Serializable data structures such as SortedMap or SortedSet, the entire
 * data structure will only serialize correctly if the comparator is
 * Serializable.
 */
public interface Comparator
{
  /**
   * Return an integer that is negative, zero or positive depending on whether
   * the first argument is less than, equal to or greater than the second
   * according to this ordering. This method should obey the following contract:
   * <UL>
   *   <LI>if compare(a, b) &lt; 0 then compare(b, a) &gt; 0</LI>
   *   <LI>if compare(a, b) throws an exception, so does compare(b, a)</LI>
   *   <LI>if compare(a, b) &lt; 0 and compare(b, c) &lt; 0 then compare(a, c)
   *       &lt; 0</LI>
   *   <LI>if a.equals(b) or both a and b are null, then compare(a, b) == 0.
   *       The converse need not be true, but if it is, this Comparator
   *       specifies a <EM>total</EM> ordering.</LI>
   * </UL>
   *
   * @throws ClassCastException if the elements are not of types that can be
   *   compared by this ordering.
   */
  int compare(Object o1, Object o2);
}
