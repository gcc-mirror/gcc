/* CompositeData.java -- A composite data structure implementation.
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

import java.io.Serializable;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

/**
 * Provides an implementation of the {@link CompositeData}
 * interface.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class CompositeDataSupport
  implements CompositeData, Serializable
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 8003518976613702244L;

  /**
   * Mapping of field names to values.
   *
   * @serial the map of field names to values.
   */
  private SortedMap<String, Object> contents;

  /**
   * The composite type which represents this composite data instance.
   *
   * @serial the type information for this instance.
   */
  private CompositeType compositeType;

  /**
   * Constructs a new {@link CompositeDataSupport} instance with the
   * specified type using field names and values from the supplied map.
   * The keys of the map become the field names, while the values
   * become the values of each respective field.  This constructor simply
   * calls the other constructor, with the two arrays formed using the
   * keys and values of this map, respectively.  Thus, the input parameters
   * given should conform to the same requirements given there (i.e. no
   * null values or empty strings).
   *
   * @param type the composite type of this composite data structure.
   * @param items a mapping of field names to values.  This should match
   *              the mappings given by the type (i.e. for each mapping
   *              in the type, there should be a corresponding field name
   *              with a value of the correct type).
   * @throws IllegalArgumentException if the type, the map or any of the keys
   *                                  or values in the map are <code>null</code>,
   *                                  or if any key from the map is an empty
   *                                  string.
   * @throws OpenDataException if a mismatch occurs between the map and the
   *                           field name/type specification given by the
   *                           {@link CompositeType} instance.  This may be
   *                           due to the two having a different size, a
   *                           mismatch between keys or an incorrectly typed
   *                           value.
   * @throws ArrayStoreException if one of the keys is not a
   *                             {@link java.lang.String} (thus calling a failure
   *                             in converting the keys to an array of strings).
   */
  public CompositeDataSupport(CompositeType type, Map<String, ?> items)
    throws OpenDataException
  {
    this(type, 
	 items.keySet().toArray(new String[items.size()]),
	 items.values().toArray());
  }

  /**
   * Constructs a new {@link CompositeDataSupport} instance with the
   * specified type using the supplied arrays of field names and
   * values.  Neither the type, the two arrays or any elements of the
   * arrays may be <code>null</code>.  The {@link java.lang.String}s
   * within the <code>names</code> array must be non-empty.  The
   * arrays must match in size and order, as each element of the
   * <code>names</code> array is matched against the corresponding
   * value in the <code>values</code> array.  Internally, the two are
   * stored in a map, lexographically ordered using the field names.
   * The data given should also conform to the description of the
   * instance given by the {@link CompositeType} instance supplied.
   *
   * @param type the composite type of this composite data structure.
   * @param names the field names.
   * @param values the corresponding values of the fields.
   * @throws IllegalArgumentException if the type, the arrays or any of the keys
   *                                  or values in the arrays are <code>null</code>,
   *                                  or if any key from <code>names</code> is
   *                                  an empty string.  This also occurs if the
   *                                  arrays differ in length.
   * @throws OpenDataException if a mismatch occurs between the arrays and the
   *                           field name/type specification given by the
   *                           {@link CompositeType} instance.  This may be
   *                           due to a differing number of field names, a
   *                           mismatch between names or an incorrectly typed
   *                           value.
   */
  public CompositeDataSupport(CompositeType type, String[] names, Object[] values)
    throws OpenDataException
  {
    if (type == null)
      throw new IllegalArgumentException("The given composite type is null.");
    compositeType = type;
    if (names == null)
      throw new IllegalArgumentException("The names array is null.");
    if (values == null)
      throw new IllegalArgumentException("The values array is null.");
    if (names.length != values.length)
      throw new IllegalArgumentException("The sizes of the arrays differ.");
    Set<String> typeKeys = type.keySet();
    if (typeKeys.size() != names.length)
      throw new OpenDataException("The number of field names does not match " +
				  "the type description.");
    contents = new TreeMap<String, Object>();
    for (int a = 0; a < names.length; ++a)
      {
	if (names[a] == null)
	  throw new IllegalArgumentException("Element " + a + " of the names " +
					     "array is null.");
	if (names[a].length() == 0)
	  throw new IllegalArgumentException("Element " + a + " of the names " +
					     "array is an empty string.");
	if (values[a] == null)
	  throw new IllegalArgumentException("Element " + a + " of the values " +
					     "array is null.");
	if (!(typeKeys.contains(names[a])))
	  throw new OpenDataException("The name, " + names[a] + ", is not a " +
				      "field in the given type description.");
	if (!(type.getType(names[a]).isValue(values[a])))
	  throw new OpenDataException("The value, " + values[a] + ", is not a " +
				      "valid value for the " + names[a] + " field.");
	contents.put(names[a], values[a]);
      }
  }

  /**
   * Returns true if this {@link CompositeData} instance contains
   * the specified key.  This method always returns false for
   * an input key equal to <code>null</code> or the empty string.
   *
   * @param key the key to find in the structure.
   * @return true if the key exists.
   */
  public boolean containsKey(String key)
  {
    if (key == null || key.length() == 0)
      return false;
    else
      return contents.containsKey(key);
  }

  /**
   * Returns true if this {@link CompositeData} instance has
   * a value equal to that supplied.
   *
   * @param value the value to look for.
   * @return true if the value exists.
   */
  public boolean containsValue(Object value)
  {
    return contents.containsValue(value);
  }


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
  public boolean equals(Object obj)
  {
    if (!(obj instanceof CompositeData))
      return false;
    CompositeData data = (CompositeData) obj;
    if (!(data.getCompositeType().equals(compositeType)))
      return false;
    for (String key : contents.keySet())
      {
	if (!(data.containsKey(key)))
	  return false;
	if (!(data.get(key).equals(contents.get(key))))
	  return false;
      }
    return true;
  }

  /**
   * Retrieves the value for the specified key.
   *
   * @param key the key whose value should be returned.
   * @return the matching value.
   * @throws IllegalArgumentException if the key is <code>null</code>
   *                                  or the empty string.
   * @throws InvalidKeyException if the key does not exist.
   */
  public Object get(String key)
  {
    if (key == null)
      throw new IllegalArgumentException("The supplied key is null.");
    if (key.length() == 0)
      throw new IllegalArgumentException("The supplied key is the empty string.");
    if (!(contents.containsKey(key)))
      throw new InvalidKeyException("The supplied key does not exist.");
    return contents.get(key);
  }

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
  public Object[] getAll(String[] keys)
  {
    Object[] values = new Object[keys.length];
    for (int a = 0; a < keys.length; ++a)
      values[a] = get(keys[a]);
    return values;
  }


  /**
   * Returns the composite type which corresponds to this instance
   * of {@link CompositeData}.
   *
   * @return the composite type for this instance.
   */
  public CompositeType getCompositeType()
  {
    return compositeType;
  }

  /**
   * Returns the hash code of this instance.  The hash code is
   * computed as the sum of the hash codes of all the values plus
   * the hash code of the composite type.  As equality comparisons
   * take place using this same information, this should ensure that
   * the property, <code>e1.equals(e2)</code> implies
   * <code>e1.hashCode() == e2.hashCode(), holds for any pair
   * of instances, <code>e1</code> and <code>e2</code>. However,
   * this relies on the other instance implementing the
   * <code>hashCode</code> method correctly, if it is not an
   * instance of {@link CompositeDataSupport}.
   *
   * @return the hash code of this {@link CompositeData}.
   * @see Object#equals(Object)
   */
  public int hashCode()
  {
    int code = compositeType.hashCode();
    for (Object o : contents.values())
      code += o.hashCode();
    return code;
  }


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
  public String toString()
  {
    return getClass().getName() +
      "[compositeType=" + compositeType +
      ",contents=" + contents +
      "]";
  }

  /**
   * Returns a read-only collection of the values associated with
   * this instance.  The values are sorted using the lexicographic
   * ordering of the corresponding keys.
   *
   * @return the values of this instance.
   */
  public Collection<?> values()
  {
    return Collections.unmodifiableCollection(contents.values());
  }

}

