/* Map.java: interface Map -- An object that maps keys to values
             interface Map.Entry -- an Entry in a Map
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
 * An object that maps keys onto values. Keys cannot be duplicated. This
 * interface replaces the obsolete {@link Dictionary} abstract class.
 * <p>
 *
 * The map has three collection views, which are backed by the map
 * (modifications on one show up on the other): a set of keys, a collection
 * of values, and a set of key-value mappings. Some maps have a guaranteed
 * order, but not all do.
 * <p>
 *
 * Note: Be careful about using mutable keys.  Behavior is unspecified if
 * a key's comparison behavior is changed after the fact.  As a corollary
 * to this rule, don't use a Map as one of its own keys or values, as it makes
 * hashCode and equals have undefined behavior.
 * <p>
 *
 * All maps are recommended to provide a no argument constructor, which builds
 * an empty map, and one that accepts a Map parameter and copies the mappings
 * (usually by putAll), to create an equivalent map.  Unfortunately, Java
 * cannot enforce these suggestions.
 * <p>
 *
 * The map may be unmodifiable, in which case unsupported operations will
 * throw an UnsupportedOperationException.  Note that some operations may be
 * safe, such as putAll(m) where m is empty, even if the operation would
 * normally fail with a non-empty argument.
 *
 * @author Original author unknown
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see HashMap
 * @see TreeMap
 * @see Hashtable
 * @see SortedMap
 * @see Collection
 * @see Set
 * @since 1.2
 * @status updated to 1.4
 */
public interface Map
{
  /**
   * Remove all entries from this Map (optional operation).
   *
   * @throws UnsupportedOperationException if clear is not supported
   */
  public void clear();

  /**
   * Returns true if this contains a mapping for the given key.
   *
   * @param key the key to search for
   * @return true if the map contains the key
   * @throws ClassCastException if the key is of an inappropriate type
   * @throws NullPointerException if key is <code>null</code> but the map
   *         does not permit null keys
   */
  public boolean containsKey(Object key);

  /**
   * Returns true if this contains at least one mapping with the given value.
   * In other words, returns true if a value v exists where
   * <code>(value == null ? v == null : value.equals(v))</code>. This usually
   * requires linear time.
   *
   * @param value the value to search for
   * @return true if the map contains the value
   */
  public boolean containsValue(Object value);

  /**
   * Returns a set view of the mappings in this Map.  Each element in the
   * set is a Map.Entry.  The set is backed by the map, so that changes in
   * one show up in the other.  Modifications made while an iterator is
   * in progress cause undefined behavior.  If the set supports removal,
   * these methods remove the underlying mapping from the map:
   * <code>Iterator.remove</code>, <code>Set.remove</code>,
   * <code>removeAll</code>, <code>retainAll</code>, and <code>clear</code>.
   * Element addition, via <code>add</code> or <code>addAll</code>, is
   * not supported via this set.
   *
   * @return the set view of all mapping entries
   * @see Map.Entry
   */
  public Set entrySet();

  /**
   * Compares the specified object with this map for equality. Returns
   * <code>true</code> if the other object is a Map with the same mappings,
   * that is,<br>
   * <code>o instanceof Map && entrySet().equals(((Map) o).entrySet();</code>
   * This allows comparison of maps, regardless of implementation.
   *
   * @param o the object to be compared
   * @return true if the object equals this map
   * @see Set#equals(Object)
   */
  public boolean equals(Object o);

  /**
   * Returns the value mapped by the given key. Returns <code>null</code> if
   * there is no mapping.  However, in Maps that accept null values, you
   * must rely on <code>containsKey</code> to determine if a mapping exists.
   *
   * @param key the key to look up
   * @return the value associated with the key, or null if key not in map
   * @throws ClassCastException if the key is an inappropriate type
   * @throws NullPointerException if this map does not accept null keys
   * @see #containsKey(Object)
   */
  public Object get(Object key);

  /**
   * Associates the given key to the given value (optional operation). If the
   * map already contains the key, its value is replaced. Be aware that in
   * a map that permits <code>null</code> values, a null return does not
   * always imply that the mapping was created.
   *
   * @param key the key to map
   * @param value the value to be mapped
   * @return the previous value of the key, or null if there was no mapping
   * @throws UnsupportedOperationException if the operation is not supported
   * @throws ClassCastException if the key or value is of the wrong type
   * @throws IllegalArgumentException if something about this key or value
   *         prevents it from existing in this map
   * @throws NullPointerException if the map forbids null keys or values
   * @see #containsKey(Object)
   */
  public Object put(Object key, Object value);

  /**
   * Returns the hash code for this map. This is the sum of all hashcodes
   * for each Map.Entry object in entrySet.  This allows comparison of maps,
   * regardless of implementation, and satisfies the contract of
   * Object.hashCode.
   *
   * @return the hash code
   * @see Map.Entry#hashCode()
   */
  public int hashCode();

  /**
   * Returns true if the map contains no mappings.
   *
   * @return true if the map is empty
   */
  public boolean isEmpty();

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
  public Set keySet();

  /**
   * Copies all entries of the given map to this one (optional operation). If
   * the map already contains a key, its value is replaced.
   *
   * @param m the mapping to load into this map
   * @throws UnsupportedOperationException if the operation is not supported
   * @throws ClassCastException if a key or value is of the wrong type
   * @throws IllegalArgumentException if something about a key or value
   *         prevents it from existing in this map
   * @throws NullPointerException if the map forbids null keys or values, or
   *         if <code>m</code> is null.
   * @see #put(Object, Object)
   */
  public void putAll(Map m);

  /**
   * Removes the mapping for this key if present (optional operation). If
   * the key is not present, this returns null. Note that maps which permit
   * null values may also return null if the key was removed.
   *
   * @param key the key to remove
   * @return the value the key mapped to, or null if not present
   * @throws UnsupportedOperationException if deletion is unsupported
   */
  public Object remove(Object o);

  /**
   * Returns the number of key-value mappings in the map. If there are more
   * than Integer.MAX_VALUE mappings, return Integer.MAX_VALUE.
   *
   * @return the number of mappings
   */
  public int size();

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
  public Collection values();

  /**
   * A map entry (key-value pair). The Map.entrySet() method returns a set
   * view of these objects; there is no other valid way to come across them.
   * These objects are only valid for the duration of an iteration; in other
   * words, if you mess with one after modifying the map, you are asking
   * for undefined behavior.
   *
   * @author Original author unknown
   * @author Eric Blake <ebb9@email.byu.edu>
   * @see Map
   * @see Map#entrySet()
   * @since 1.2
   * @status updated to 1.4
   */
  public static interface Entry
  {
    /**
     * Get the key corresponding to this entry.
     *
     * @return the key
     */
    public Object getKey();

    /**
     * Get the value corresponding to this entry. If you already called
     * Iterator.remove(), this is undefined.
     *
     * @return the value
     */
    public Object getValue();

    /**
     * Replaces the value with the specified object (optional operation).
     * This writes through to the map, and is undefined if you already
     * called Iterator.remove().
     *
     * @param value the new value to store
     * @return the old value
     * @throws UnsupportedOperationException if the operation is not supported
     * @throws ClassCastException if the value is of the wrong type
     * @throws IllegalArgumentException if something about the value
     *         prevents it from existing in this map
     * @throws NullPointerException if the map forbids null values
     */
    public Object setValue(Object value);

    /**
     * Returns the hash code of the entry.  This is defined as the exclusive-or
     * of the hashcodes of the key and value (using 0 for null). In other
     * words, this must be:
     * <pre>
     *  (getKey() == null ? 0 : getKey().hashCode()) ^
     *  (getValue() == null ? 0 : getValue().hashCode())
     * </pre>
     *
     * @return the hash code
     */
    public int hashCode();

    /**
     * Compares the specified object with this entry. Returns true only if
     * the object is a mapping of identical key and value. In other words,
     * this must be:
     * <pre>
     * (o instanceof Map.Entry)
     * && (getKey() == null ? ((HashMap) o).getKey() == null
     *                      : getKey().equals(((HashMap) o).getKey()))
     * && (getValue() == null ? ((HashMap) o).getValue() == null
     *                        : getValue().equals(((HashMap) o).getValue()))
     * </pre>
     *
     * @param o the object to compare
     * @return true if it is equal
     */
    public boolean equals(Object o);
  }
}
