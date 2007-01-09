/* Annotation.java - Base interface for all annotations
   Copyright (C) 2004 Free Software Foundation

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

package java.lang.annotation;

/**
 * This is the common interface for all annotations.  Note that classes
 * that implement this class manually are not classed as annotations, and
 * that this interface does not define an annotation type in itself.
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface Annotation
{

  /**
   * Returns the type of this annotation.
   *
   * @return the class of which this annotation is an instance.
   */
  Class<? extends Annotation> annotationType();

  /**
   * <p>
   * Returns true if the supplied object is equivalent to this annotation.
   * For this property to hold, the following must be true of <code>o</code>:
   * </p>
   * <ul>
   * <li>The object is also an instance of the same annotation type.</li>
   * <li>The members of the supplied annotation are equal to those of this
   * annotation, according to the following:
   * <ul>
   * <li>If the members are <code>float</code>s, then, for floats
   * <code>x</code> and <code>y</code>, 
   * <code>Float.valueOf(x).equals(Float.valueOf(y)</code> must return
   * true.  This differs from the usual (<code>==</code>) comparison
   * in that <code>NaN</code> is considered equal to itself and positive
   * and negative zero are seen as different.</li>
   * <li>Likewise, if the members are <code>double</code>s, then, for doubles
   * <code>x</code> and <code>y</code>, 
   * <code>Double.valueOf(x).equals(Double.valueOf(y)</code> must return
   * true.  This differs from the usual (<code>==</code>) comparison
   * in that <code>NaN</code> is considered equal to itself and positive
   * and negative zero are seen as different.</li>
   * <li>Strings, classes, enumerations and annotations are considered
   * equal according to the <code>equals()</code> implementation for these
   * types.</li>
   * <li>Arrays are considered equal according to <code>Arrays.equals()</code>
   * </li>
   * <li>Any remaining types are considered equal using <code>==</code>.</li>
   * </li>
   * </ul>
   *
   * @param o the object to compare with this annotation.
   * @return true if the supplied object is an annotation with equivalent
   *         members.
   */
  boolean equals(Object o);

  /**
   * <p>
   * Returns the hash code of the annotation.  This is computed as the
   * sum of the hash codes of the annotation's members.
   * </p>
   * <p>
   * The hash code of a member of the annotation is the result of XORing
   * the hash code of its value with the result of multiplying the hash code
   * of its name by 127.  Formally, if the value is <code>v</code> and the
   * name is <code>n</code>, the hash code of the member is
   * v.hashCode() XOR (127 * String.hashCode(n)).  <code>v.hashCode()</code>
   * is defined as follows:
   * </p>
   * <ul>
   * <li>The hash code of a primitive value (i.e. <code>byte</code>,
   * <code>char</code>, <code>double</code>, <code>float</code>,
   * <code>int</code>, <code>long</code>, <code>short</code> and
   * <code>boolean</code>) is the hash code obtained from its corresponding
   * wrapper class using <code>valueOf(v).hashCode()</code>, where
   * <code>v</code> is the primitive value.</li>
   * <li>The hash code of an enumeration, string, class or other annotation
   * is obtained using <code>v.hashCode()</code>.</li>
   * <li>The hash code of an array is computed using
   * <code>Arrays.hashCode(v)</code>.</li>
   * </ul>
   *
   * @return the hash code of the annotation, computed as the sum of its
   *         member hashcodes.
   */
  int hashCode();

  /**
   * Returns a textual representation of the annotation.  This is
   * implementation-dependent, but is expected to include the classname
   * and the names and values of each member.
   *
   * @return a textual representation of the annotation.
   */
  String toString();
}
