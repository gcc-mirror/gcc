/* Comparable.java -- Interface for comparaing objects to obtain an ordering
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

/* Written using online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct.
 */

/** 
 * Interface for objects that can be ordering among other
 * objects. The ordering can be <EM>total</EM>, such that two objects
 * only compare equal if they are equal by the equals method, or
 * <EM>partial</EM> such that this is not necessarily true. For
 * example, a case-sensitive dictionary order comparison of Strings
 * is total, but if it is case-insensitive it is partial, because
 * "abc" and "ABC" compare as equal even though "abc".equals("ABC")
 * returns false.
 *
 * @author Geoff Berry
 * @author Warren Levy <warrenl@cygnus.com>
 *
 * @since JDK1.2
 * @see java.util.Comparator
 */
public interface Comparable
{
  /**
   * @return a negative integer if this object is less than
   * <code>o<code>, zero if this object is equal to <code>o</code>, or
   * a positive integer if this object is greater than <code>o</code>
   */
  int compareTo( Object o );
}
