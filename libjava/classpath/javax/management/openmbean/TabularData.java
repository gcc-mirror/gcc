/* TabularData.java -- Tables of composite data structures.
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
import java.util.Set;

/**
 * Provides an interface to a specific type of composite
 * data structure, where keys (the columns) map to the
 * {@link CompositeData} objects that form the rows of
 * the table.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public interface TabularData
{

  /**
   * Calculates the index the specified {@link CompositeData} value
   * would have, if it was to be added to this {@link TabularData}
   * instance.  This method includes a check that the type of the
   * given value is the same as the row type of this instance, but not
   * a check for existing instances of the given value.  The value
   * must also not be <code>null</code>.  Possible indices are
   * returned by the {@link TabularType#getIndexNames()} method of
   * this instance's tabular type.  The returned indices are the
   * values of the fields in the supplied {@link CompositeData}
   * instance that match the names given in the {@link TabularType}.
   * 
   * @param val the {@link CompositeData} value whose index should
   *            be calculated.
   * @return the index the value would take on, if it were to be added.
   * @throws NullPointerException if the value is <code>null</code>.
   * @throws InvalidOpenTypeException if the value does not match the
   *                                  row type of this instance.
   */
  Object[] calculateIndex(CompositeData val);

  /**
   * Removes all {@link CompositeData} values from the table.
   */
  void clear();

  /**
   * Returns true iff this instance of the {@link TabularData} class
   * contains a {@link CompositeData} value at the specified index.
   * In any other circumstance, including if the given key
   * is <code>null</code> or of the incorrect type, according to
   * the {@link TabularType} of this instance, this method returns
   * false.
   *
   * @param key the key to test for.
   * @return true if the key maps to a {@link CompositeData} value.
   */
  boolean containsKey(Object[] key);

  /**
   * Returns true iff this instance of the {@link TabularData} class
   * contains the specified {@link CompositeData} value.
   * In any other circumstance, including if the given value
   * is <code>null</code> or of the incorrect type, according to
   * the {@link TabularType} of this instance, this method returns
   * false.
   *
   * @param val the value to test for.
   * @return true if the value exists.
   */
  boolean containsValue(CompositeData val);

  /**
   * Compares the specified object with this object for equality.
   * The object is judged equivalent if it is non-null, and also
   * an instance of {@link TabularData} with the same row type,
   * and {@link CompositeData} values.  The two compared instances may
   * be equivalent even if they represent different implementations
   * of {@link TabularData}.
   *
   * @param obj the object to compare for equality.
   * @return true if <code>obj</code> is equal to <code>this</code>.
   */
  boolean equals(Object obj);

  /**
   * Retrieves the {@link CompositeData} value for the specified
   * key, or <code>null</code> if no such mapping exists.
   *
   * @param key the key whose value should be returned.
   * @return the matching {@link CompositeData} value, or
   *         <code>null</code> if one does not exist.
   * @throws NullPointerException if the key is <code>null</code>.
   * @throws InvalidKeyException if the key does not match
   *                             the {@link TabularType} of this
   *                             instance.
   */
  CompositeData get(Object[] key);

  /**
   * Returns the tabular type which corresponds to this instance
   * of {@link TabularData}.
   *
   * @return the tabular type for this instance.
   */
  TabularType getTabularType();

  /**
   * Returns the hash code of the composite data type.  This is
   * computed as the sum of the hash codes of each value, together
   * with the hash code of the tabular type.  These are the same
   * elements of the type that are compared as part of the {@link
   * #equals(java.lang.Object)} method, thus ensuring that the
   * hashcode is compatible with the equality test.
   *
   * @return the hash code of this instance.
   */
  int hashCode();

  /**
   * Returns true if this {@link TabularData} instance
   * contains no {@link CompositeData} values.
   * 
   * @return true if the instance is devoid of rows.
   */
  boolean isEmpty();

  /**
   * Returns a {@link java.util.Set} view of the keys or
   * indices of this {@link TabularData} instance. 
   *
   * @return a set containing the keys of this instance.
   */
  Set<?> keySet();

  /**
   * Adds the specified {@link CompositeData} value to the
   * table.  The value must be non-null, of the same type
   * as the row type of this instance, and must not have
   * the same index as an existing value.  The index is
   * calculated using the index names of the
   * {@link TabularType} for this instance.
   * 
   * @param val the {@link CompositeData} value to add.
   * @throws NullPointerException if <code>val</code> is
   *                              <code>null</code>.
   * @throws InvalidOpenTypeException if the type of the
   *                                  given value does not
   *                                  match the row type.
   * @throws KeyAlreadyExistsException if the value has the
   *                                   same calculated index
   *                                   as an existing value.
   */
  void put(CompositeData val);

  /**
   * Adds each of the specified {@link CompositeData} values
   * to the table.  Each element of the array must meet the
   * conditions given for the {@link #put(CompositeData)}
   * method.  In addition, the index of each value in the
   * array must be distinct from the index of the other
   * values in the array, as well as from the existing values
   * in the table.  The operation should be atomic; if one
   * value can not be added, then none of the values should
   * be.  If the array is <code>null</code> or empty, the
   * method simply returns.
   * 
   * @param vals the {@link CompositeData} values to add.
   * @throws NullPointerException if a value from the array is
   *                              <code>null</code>.
   * @throws InvalidOpenTypeException if the type of a
   *                                  given value does not
   *                                  match the row type.
   * @throws KeyAlreadyExistsException if a value has the
   *                                   same calculated index
   *                                   as an existing value or
   *                                   of one of the other
   *                                   specified values.
   */
  void putAll(CompositeData[] vals);

  /**
   * Removes the {@link CompositeData} value located at the
   * specified index.  <code>null</code> is returned if the
   * value does not exist.  Otherwise, the removed value is
   * returned.
   *
   * @param key the key of the value to remove.
   * @return the removed value, or <code>null</code> if
   *         there is no value for the given key.
   * @throws NullPointerException if the key is <code>null</code>.
   * @throws InvalidOpenTypeException if the key does not match
   *                                  the {@link TabularType} of this
   *                                  instance.
   */
  CompositeData remove(Object[] key);

  /**
   * Returns the number of {@link CompositeData} values or rows
   * in the table.
   *
   * @return the number of rows in the table.
   */
  int size();

  /**
   * Returns a textual representation of this instance.  The
   * exact format is left up to the implementation, but it
   * should contain the name of the implementing class and
   * the tabular type.
   *
   * @return a {@link java.lang.String} representation of the
   *         object.
   */
  String toString();
 
  /**
   * Returns the values associated with this instance.
   *
   * @return the values of this instance.
   */
  Collection<?> values();

}

