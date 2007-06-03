/* CompositeData.java -- A composite data structure.
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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

package javax.management.openmbean;

import java.util.Collection;

/**
 * Provides an interface to a composite data structure,
 * in order to aid interoperability.  The composite data
 * structure is represented by mapping field names to
 * values.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface CompositeData
{

  /**
   * Returns true if this {@link CompositeData} instance contains
   * the specified key.  This method always returns false for
   * an input key equal to <code>null</code> or the empty string.
   *
   * @param key the key to find in the structure.
   * @return true if the key exists.
   */
  boolean containsKey(String key);

  /**
   * Returns true if this {@link CompositeData} instance has
   * a value equal to that supplied.
   *
   * @param value the value to look for.
   * @return true if the value exists.
   */
  boolean containsValue(Object value);

  /**
   * Compares the specified object with this object for equality.
   * The object is judged equivalent if it is non-null, and also
   * an instance of {@link CompositeData} with the same name-value
   * mappings and types.  The two compared instances may be
   * equivalent even if they represent different implementations of
   * {@link CompositeData}.
   *
   * @param obj the object to compare for equality.
   * @return true if <code>obj</code> is equal to <code>this</code>.
   */
  boolean equals(Object obj);

  /**
   * Retrieves the value for the specified key.
   *
   * @param key the key whose value should be returned.
   * @return the matching value.
   * @throws IllegalArgumentException if the key is <code>null</code>
   *                                  or the empty string.
   * @throws InvalidKeyException if the key does not exist.
   */
  Object get(String key);

  /**
   * Returns the appropriate value for each key in the given array,
   * using the same ordering.
   *
   * @param keys the keys whose values should be returned.
   * @return the matching values.
   * @throws IllegalArgumentException if one of the keys is
   *                                  <code>null</code> or the
   *                                  empty string.
   * @throws InvalidKeyException if one of the keys does not exist.
   */
  Object[] getAll(String[] keys);

  /**
   * Returns the composite type which corresponds to this instance
   * of {@link CompositeData}.
   *
   * @return the composite type for this instance.
   */
  CompositeType getCompositeType();

  /**
   * Returns the hash code of this instance.  The hash code is
   * computed as the sum of the hash codes of all the values plus
   * the hash code of the composite type.  As equality comparisons
   * take place using this same information, this ensures that
   * the property, <code>e1.equals(e2)</code> implies
   * <code>e1.hashCode() == e2.hashCode(), holds for any pair
   * of instances, <code>e1</code> and <code>e2</code>.
   *
   * @return the hash code of this {@link CompositeData}.
   * @see Object#equals(Object)
   */
  int hashCode();

  /**
   * Returns a textual representation of this instance.  The
   * exact format is left up to the implementation, but it
   * should contain the name of the implementing class,
   * the name of the type and a mapping of the form
   * <code>key=value</code> for each pair of key and value.
   *
   * @return a {@link java.lang.String} representation of the
   *         object.
   */
  String toString();

  /**
   * Returns a read-only collection of the values associated with
   * this instance.  The values are sorted using the lexicographic
   * ordering of the corresponding keys.
   *
   * @return the values of this instance.
   */
  Collection<?> values();

}

