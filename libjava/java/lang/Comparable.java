/* Comparable.java -- Interface for comparaing objects to obtain an ordering
   Copyright (C) 1998, 1999, 2001, 2002 Free Software Foundation, Inc.

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


package java.lang;

/**
 * Interface for objects that can be ordering among other objects. The
 * ordering can be <em>total</em>, such that two objects only compare equal
 * if they are also equal by the equals method, or <em>partial</em> such
 * that this is not necessarily true. For example, a case-sensitive
 * dictionary order comparison of Strings is total, but if it is
 * case-insensitive it is partial, because "abc" and "ABC" compare as
 * equal even though "abc".equals("ABC") returns false. However, if you use
 * a partial ordering, it is a good idea to document your class as
 * "inconsistent with equals", because the behavior of your class in a
 * SortedMap will be different than in a HashMap.
 *
 * <p>Lists, arrays, and sets of objects that implement this interface can
 * be sorted automatically, without the need for an explicit
 * {@link Comparator}. Note that <code>e1.compareTo(null)</code> should
 * throw an Exception; as should comparison between incompatible classes.
 *
 * @author Geoff Berry
 * @author Warren Levy <warrenl@cygnus.com>
 * @see Comparator
 * @see Collections#sort(List)
 * @see Arrays#sort(Object[])
 * @see SortedSet
 * @see SortedMap
 * @see TreeSet
 * @see TreeMap
 * @since 1.2
 * @status updated to 1.4
 */
public interface Comparable
{
  /**
   * Compares this object with another, and returns a numerical result based
   * on the comparison.  If the result is negative, this object sorts less
   * than the other; if 0, the two are equal, and if positive, this object
   * sorts greater than the other.  To translate this into boolean, simply
   * perform <code>o1.compareTo(o2) <em>&lt;op&gt;</em> 0</code>, where op
   * is one of &lt;, &lt;=, =, !=, &gt;, or &gt;=.
   *
   * <p>You must make sure that the comparison is mutual, ie.
   * <code>sgn(x.compareTo(y)) == -sgn(y.compareTo(x))</code> (where sgn() is
   * defined as -1, 0, or 1 based on the sign).  This includes throwing an
   * exception in either direction if the two are not comparable; hence,
   * <code>compareTo(null)</code> should always throw an Exception.
   *
   * <p>You should also ensure transitivity, in two forms:
   * <code>x.compareTo(y) &gt; 0 && y.compareTo(z) &gt; 0</code> implies
   * <code>x.compareTo(z) &gt; 0</code>; and <code>x.compareTo(y) == 0</code>
   * implies <code>x.compareTo(z) == y.compareTo(z)</code>.
   *
   * @param o the object to be compared
   * @return an integer describing the comparison
   * @throws NullPointerException if o is null
   * @throws ClassCastException if o cannot be compared
   */
  int compareTo(Object o);
}
