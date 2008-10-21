/* TabularDataSupport.java -- Tables of composite data structures.
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Provides an implementation of the {@link TabularData}
 * interface using a {@link java.util.HashMap}.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class TabularDataSupport
  implements TabularData, Serializable, Cloneable, Map<Object,Object>
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = 5720150593236309827L;

  /**
   * Mapping of rows to column values.
   *
   * @serial the map of rows to column values.
   */
  private HashMap<Object,Object> dataMap;

  /**
   * The tabular type which represents this tabular data instance.
   *
   * @serial the type information for this instance.
   */
  private TabularType tabularType;

  /**
   * Constructs a new empty {@link TabularDataSupport} with the
   * specified type.  The type may not be null.  This constructor
   * simply calls the other, with the default initial capacity of
   * <code>101</code> and default load factor of <code>0.75</code>.
   *
   * @param type the tabular type of this tabular data instance.
   * @throws IllegalArgumentException if <code>type</code> is
   *                                  <code>null</code>.
   */
  public TabularDataSupport(TabularType type)
  {
    this(type, 101, 0.75f);
  }

  /**
   * Constructs a new empty {@link TabularDataSupport} with the
   * specified type and the supplied initial capacity and load factor
   * being used for the underlying {@link java.util.HashMap}.  The
   * type may not be null and the initial capacity and load factor
   * must be positive.
   *
   * @param type the tabular type of this tabular data instance.
   * @param cap the initial capacity of the underlying map.
   * @param lf the load factor of the underlying map.
   * @throws IllegalArgumentException if <code>type</code> is
   *                                  <code>null</code>, or
   *                                  <code>cap</code> or
   *                                  <code>lf</code> are
   *                                  negative.
   */
  public TabularDataSupport(TabularType type, int cap, float lf)
  {
    if (type == null)
      throw new IllegalArgumentException("The type may not be null.");
    tabularType = type;
    dataMap = new HashMap<Object,Object>(cap, lf);
  }
   
  /**
   * Calculates the index the specified {@link CompositeData} value
   * would have, if it was to be added to this {@link TabularData}
   * instance.  This method includes a check that the type of the
   * given value is the same as the row type of this instance, but not
   * a check for existing instances of the given value.  The value
   * must also not be <code>null</code>.  Possible indices are
   * selected by the {@link TabularType#getIndexNames()} method of
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
  public Object[] calculateIndex(CompositeData val)
  {
    if (!(val.getCompositeType().equals(tabularType.getRowType())))
      throw new InvalidOpenTypeException("The type of the given value " +
					 "does not match the row type " +
					 "of this instance.");
    List<String> indexNames = tabularType.getIndexNames();
    List<String> matchingIndicies = new ArrayList<String>(indexNames.size());
    for (String name : indexNames)
      matchingIndicies.add(val.get(name).toString());
    return matchingIndicies.toArray();
  }

  /**
   * Removes all {@link CompositeData} values from the table.
   */
  public void clear()
  {
    dataMap.clear();
  }

  /**
   * Returns a shallow clone of the information, as obtained by the
   * {@link Object} implementation of {@link Object#clone()}.  The map
   * is also cloned, but it still references the same objects.
   *
   * @return a shallow clone of this {@link TabularDataSupport}.
   */
  @SuppressWarnings("unchecked")
  public Object clone()
  {
    TabularDataSupport clone = null;
    try
      {
	clone = (TabularDataSupport) super.clone();
	clone.setMap((HashMap<Object,Object>) dataMap.clone());
      }
    catch (CloneNotSupportedException e)
      {
	/* This won't happen as we implement Cloneable */
      }
    return clone;
  }

  /**
   * Returns true iff this instance of the {@link TabularData} class
   * contains a {@link CompositeData} value at the specified index.
   * The method returns <code>false</code> if the given key can
   * not be cast to an {@link java.lang.Object} array; otherwise
   * it returns the result of {@link #containsKey(java.lang.Object[])}.
   *
   *
   * @param key the key to test for.
   * @return true if the key maps to a {@link CompositeData} value.
   */
  public boolean containsKey(Object key)
  {
    if (key instanceof Object[])
      return containsKey((Object[]) key);
    else
      return false;
  }

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
  public boolean containsKey(Object[] key)
  {
    if (key == null)
      return false;
    if (!(isKeyValid(key)))
      return false;
    return dataMap.containsKey(key);
  }

  /**
   * Returns true iff this instance of the {@link TabularData} class
   * contains the specified {@link CompositeData} value.  If the given
   * value is not an instance of {@link CompositeData}, this method
   * simply returns false.
   *
   * @param val the value to test for.
   * @return true if the value exists.
   */
  public boolean containsValue(Object val)
  {
    if (val instanceof CompositeData)
      return containsValue((CompositeData) val);
    else
      return false;
  }

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
  public boolean containsValue(CompositeData val)
  {
    if (val == null)
      return false;
    if (!(val.getCompositeType().equals(tabularType.getRowType())))
      return false;
    return dataMap.containsValue(val);
  }

  /**
   * <p>
   * Returns a set view of the mappings in this Map.  Each element in the
   * set is a Map.Entry.  The set is backed by the map, so that changes in
   * one show up in the other.  Modifications made while an iterator is
   * in progress cause undefined behavior.  If the set supports removal,
   * these methods remove the underlying mapping from the map:
   * <code>Iterator.remove</code>, <code>Set.remove</code>,
   * <code>removeAll</code>, <code>retainAll</code>, and <code>clear</code>.
   * Element addition, via <code>add</code> or <code>addAll</code>, is
   * not supported via this set.
   * </p>
   * <p>
   * <strong>Note</strong>: using the
   * {@link java.util.Map.Entry#setValue(Object) will cause corruption of
   * the index to row mappings.
   * </p>
   *
   * @return the set view of all mapping entries
   * @see java.util.Map.Entry
   */
  public Set<Map.Entry<Object,Object>> entrySet()
  {
    return dataMap.entrySet();
  }

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
  public boolean equals(Object obj)
  {
    if (!(obj instanceof TabularData))
      return false;
    TabularData data = (TabularData) obj;
    return tabularType.equals(data.getTabularType()) &&
      dataMap.values().equals(data.values());
  }

  /**
   * Retrieves the value for the specified key by simply
   * calling <code>get((Object[]) key)</code>.
   *
   * @param key the key whose value should be returned.
   * @return the matching {@link CompositeData} value, or
   *         <code>null</code> if one does not exist.
   * @throws NullPointerException if the key is <code>null</code>.
   * @throws ClassCastException if the key is not an instance
   *                            of <code>Object[]</code>.
   * @throws InvalidKeyException if the key does not match
   *                             the {@link TabularType} of this
   *                             instance.
   */
  public Object get(Object key)
  {
    return get((Object[]) key);
  }

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
  public CompositeData get(Object[] key)
  {
    if (!(isKeyValid(key)))
      throw new InvalidKeyException("The key does not match the " +
				    "tabular type of this instance.");
    return (CompositeData) dataMap.get(key);
  }

  /**
   * Returns the tabular type which corresponds to this instance
   * of {@link TabularData}.
   *
   * @return the tabular type for this instance.
   */
  public TabularType getTabularType()
  {
    return tabularType;
  }

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
  public int hashCode()
  {
    return tabularType.hashCode() + dataMap.values().hashCode();
  }

  /**
   * Returns true if this {@link TabularData} instance
   * contains no {@link CompositeData} values.
   * 
   * @return true if the instance is devoid of rows.
   */
  public boolean isEmpty()
  {
    return dataMap.isEmpty();
  }

  /**
   * Returns true if the given key is valid for the
   * @link{TabularType} of this instance.
   *
   * @return true if the key is valid.
   * @throws NullPointerException if <code>key</code>
   *                              is null.
   */
  private boolean isKeyValid(Object[] key)
  {
    Iterator<String> it = tabularType.getIndexNames().iterator();
    CompositeType rowType = tabularType.getRowType();
    for (int a = 0; it.hasNext(); ++a)
      {
	OpenType<?> type = rowType.getType(it.next());
	if (!(type.isValue(key[a])))
	  return false;
      }
    return true;
  }

  /**
   * Returns a set view of the keys in this Map.  The set is backed by the
   * map, so that changes in one show up in the other.  Modifications made
   * while an iterator is in progress cause undefined behavior.  If the set
   * supports removal, these methods remove the underlying mapping from
   * the map: <code>Iterator.remove</code>, <code>Set.remove</code>,
   * <code>removeAll</code>, <code>retainAll</code>, and <code>clear</code>.
   * Element addition, via <code>add</code> or <code>addAll</code>, is
   * not supported via this set.
   *
   * @return the set view of all keys
   */
  public Set<Object> keySet()
  {
    return dataMap.keySet();
  }

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
  public void put(CompositeData val)
  {
    Object[] key = calculateIndex(val);
    if (dataMap.containsKey(key))
      throw new KeyAlreadyExistsException("A value with this index " +
					  "already exists.");
    dataMap.put(key, val);
  }

  /**
   * Adds the specified {@link CompositeData} value to the
   * table, ignoring the supplied key, by simply calling
   * <code>put((CompositeData) val)</code>.
   *
   * @param key ignored.
   * @param val the {@link CompositeData} value to add.
   * @return the {@link CompositeData} value.
   * @throws NullPointerException if <code>val</code> is
   *                              <code>null</code>.
   * @throws InvalidOpenTypeException if the type of the
   *                                  given value does not
   *                                  match the row type.
   * @throws KeyAlreadyExistsException if the value has the
   *                                   same calculated index
   *                                   as an existing value.
   */
  public Object put(Object key, Object val)
  {
    put((CompositeData) val);
    return val;
  }

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
  public void putAll(CompositeData[] vals)
  {
    if (vals == null || vals.length == 0)
      return;
    Map<Object,Object> mapToAdd = new HashMap<Object,Object>(vals.length);
    for (int a = 0; a < vals.length; ++a)
      {
	Object[] key = calculateIndex(vals[a]);
	if (dataMap.containsKey(key))
	  throw new KeyAlreadyExistsException("Element " + a + ": A " +
					      "value with this index " +
					      "already exists.");
	mapToAdd.put(key, vals[a]);
      }
    dataMap.putAll(mapToAdd);
  }

  /**
   * Converts each value from the specified map to a member of an
   * array of {@link CompositeData} values and adds them using {@link
   * #put(CompositeData[])}, if possible.  As in {@link
   * #put(Object,Object)}, the keys are simply ignored.  This method
   * is useful for adding the {@link CompositeData} values from a
   * different {@link TabularData} instance, which uses the same
   * {@link TabularType} but a different selection of index names, to
   * this one.  If the map is <code>null</code> or empty, the method
   * simply returns.
   *
   * @param m the map to add.  Only the values are used and must
   *          all be instances of {@link CompositeData}.
   * @throws NullPointerException if a value from the map is
   *                              <code>null</code>.
   * @throws ClassCastException if a value from the map is not
   *                            an instance of {@link CompositeData}.
   * @throws InvalidOpenTypeException if the type of the
   *                                  given value does not
   *                                  match the row type.
   * @throws KeyAlreadyExistsException if the value has the
   *                                   same calculated index
   *                                   as an existing value or
   *                                   of one of the other
   *                                   specified values.
   */
  public void putAll(Map<?,?> m)
  {
    if (m == null || m.size() == 0)
      return;
    Collection<?> vals = m.values();
    CompositeData[] data = new CompositeData[vals.size()];
    Iterator<?> it = vals.iterator();
    for (int a = 0; it.hasNext(); ++a)
      {
	data[a] = (CompositeData) it.next();
      }
    putAll(data);
  }

  /**
   * Removes the value for the specified key by simply
   * calling <code>remove((Object[]) key)</code>.
   *
   * @param key the key whose value should be removed.
   * @return the removed value, or <code>null</code> if
   *         there is no value for the given key.
   * @throws NullPointerException if the key is <code>null</code>.
   * @throws ClassCastException if the key is not an instance
   *                            of <code>Object[]</code>.
   * @throws InvalidOpenTypeException if the key does not match
   *                                  the {@link TabularType} of this
   *                                  instance.
   */
  public Object remove(Object key)
  {
    return remove((Object[]) key);
  }

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
  public CompositeData remove(Object[] key)
  {
    if (!(isKeyValid(key)))
      throw new InvalidKeyException("The key does not match the " +
				    "tabular type of this instance.");
    return (CompositeData) dataMap.remove(key);
  }

  /**
   * Private method to set the internal {@link java.util.Map}
   * instance (used in cloning).
   *
   * @param map the new map used.
   */
  private void setMap(HashMap<Object,Object> map)
  {
    dataMap = map;
  }

  /**
   * Returns the number of {@link CompositeData} values or rows
   * in the table.
   *
   * @return the number of rows in the table.
   */
  public int size()
  {
    return dataMap.size();
  }

  /**
   * Returns a textual representation of this instance.  This
   * is constructed using the class name
   * (<code>javax.management.openmbean.TabularDataSupport</code>)
   * and the result of calling <code>toString()</code> on the
   * tabular type and underlying hash map instance.
   *
   * @return a {@link java.lang.String} representation of the
   *         object.
   */
  public String toString()
  {
    return getClass().getName()
      + "[tabularType=" + tabularType 
      + ",dataMap=" + dataMap
      + "]";
  }
 
  /**
   * Returns a collection (or bag) view of the values in this Map.  The
   * collection is backed by the map, so that changes in one show up in
   * the other.  Modifications made while an iterator is in progress cause
   * undefined behavior.  If the collection supports removal, these methods
   * remove the underlying mapping from the map: <code>Iterator.remove</code>,
   * <code>Collection.remove</code>, <code>removeAll</code>,
   * <code>retainAll</code>, and <code>clear</code>. Element addition, via
   * <code>add</code> or <code>addAll</code>, is not supported via this
   * collection.
   *
   * @return the collection view of all values
   */
  public Collection<Object> values()
  {
    return dataMap.values();
  }

}

