/* WildcardType.java -- A wildcard type expression e.g. ? extends String
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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


package java.lang.reflect;

/**
 * Represents a wildcard type expression, where the type variable
 * is unnamed.  The simplest example of this is <code>?</code>,
 * which represents any unbounded type.  Another example is
 * <code>? extends Number</code>, which specifies any type
 * which is a subclass of <code>Number</code> (<code>Number</code>
 * is the upper bound).
 * </p>
 * <p>
 * <code>? super String</code> gives the type a less common lower bound,
 * which means that the type must be either a <code>String</code> or one
 * of its superclasses. This can be useful in working with collections.
 * You may want a method to add instances of a class to a collection
 * with a more generic type (e.g. adding <code>String</code>s to
 * a list of <code>Object</code>s), but don't want to allow users
 * to pass in a collection with a more specific type.
 * </p>
 *
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface WildcardType extends Type
{

  /**
   * <p>
   * Returns an array of <code>Type</code>s, which specify the
   * lower bounds of this type.  The default lower bound is
   * <code>null</code>, which causes this method to return an
   * empty array.
   * </p>
   * <p>
   * In generating the array of <code>Type</code>s, each
   * <code>ParameterizedType</code> or <code>TypeVariable</code> is
   * created, (see the documentation for these classes for details of this
   * process), if necessary, while all other types are simply
   * resolved.
   * </p>
   *
   * @return an array of <code>Type</code> objects, representing
   *         the wildcard type's lower bounds.
   * @throws TypeNotPresentException if any of the types referred to by
   *         the lower bounds of this type do not actually exist.
   * @throws MalformedParameterizedTypeException if any of the types
   *         refer to a type which can not be instantiated.
   */
  Type[] getLowerBounds();

  /**
   * <p>
   * Returns an array of <code>Type</code>s, which specify the
   * upper bounds of this type.  The default upper bound is
   * <code>Object</code>, which causes this method to return an
   * array, containing just the <code>Type</code> instance for
   * <code>Object</code>.
   * </p>
   * <p>
   * In generating the array of <code>Type</code>s, each
   * <code>ParameterizedType</code> or <code>TypeVariable</code> is
   * created, (see the documentation for these classes for details of this
   * process), if necessary, while all other types are simply
   * resolved.
   * </p>
   *
   * @return an array of <code>Type</code> objects, representing
   *         the wildcard type's upper bounds.
   * @throws TypeNotPresentException if any of the types referred to by
   *         the upper bounds of this type do not actually exist.
   * @throws MalformedParameterizedTypeException if any of the types
   *         refer to a type which can not be instantiated.
   */
  Type[] getUpperBounds();

}
