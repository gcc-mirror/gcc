/* SortedMap.java -- A map that makes guarantees about the order of its keys
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
 * A map which guarantees its key's iteration order. The entries in the
 * map are related by the <i>natural ordering</i> of the keys if they
 * are Comparable, or by the provided Comparator.  Additional operations
 * take advantage of the sorted nature of the map.
 * <p>
 *
 * All keys entered in the map must be mutually comparable; in other words,
 * <code>k1.compareTo(k2)</code> or <code>comparator.compare(k1, k2)</code>
 * must not throw a ClassCastException. The ordering must be <i>consistent
 * with equals</i> (see {@link Comparator} for this definition), if the
 * map is to obey the general contract of the Map interface.  If not,
 * the results are well-defined, but probably not what you wanted.
 * <p>
 *
 * It is recommended that all implementing classes provide four constructors:
 * 1) one that takes no arguments and builds an empty map sorted by natural
 * order of the keys; 2) one that takes a Comparator for the sorting order;
 * 3) one that takes a Map and sorts according to the natural order of its
 * keys; and 4) one that takes a SortedMap and sorts by the same comparator.
 * Unfortunately, the Java language does not provide a way to enforce this.
 *
 * @author Original author unknown
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Map
 * @see TreeMap
 * @see SortedSet
 * @see Comparable
 * @see Comparator
 * @see Collection
 * @see ClassCastException
 * @since 1.2
 * @status updated to 1.4
 */
public interface SortedMap extends Map
{
  /**
   * Returns the comparator used in sorting this map, or null if it is
   * the keys' natural ordering.
   *
   * @return the sorting comparator
   */
  Comparator comparator();

  /**
   * Returns the first (lowest sorted) key in the map.
   *
   * @return the first key
   */
  Object firstKey();

  /**
   * Returns a view of the portion of the map strictly less than toKey. The
   * view is backed by this map, so changes in one show up in the other.
   * The submap supports all optional operations of the original.
   * <p>
   *
   * The returned map throws an IllegalArgumentException any time a key is
   * used which is out of the range of toKey. Note that the endpoint is not
   * included; if you want the endpoint, pass the successor object in to
   * toKey.  For example, for Strings, you can request
   * <code>headMap(limit + "\0")</code>.
   *
   * @param toKey the exclusive upper range of the submap
   * @return the submap
   * @throws ClassCastException if toKey is not comparable to the map contents
   * @throws IllegalArgumentException if this is a subMap, and toKey is out
   *         of range
   * @throws NullPointerException if toKey is null but the map does not allow
   *         null keys
   */
  SortedMap headMap(Object toKey);

  /**
   * Returns the last (highest sorted) key in the map.
   *
   * @return the last key
   */
  Object lastKey();

  /**
   * Returns a view of the portion of the map greater than or equal to
   * fromKey, and strictly less than toKey. The view is backed by this map,
   * so changes in one show up in the other. The submap supports all
   * optional operations of the original.
   * <p>
   *
   * The returned map throws an IllegalArgumentException any time a key is
   * used which is out of the range of fromKey and toKey. Note that the
   * lower endpoint is included, but the upper is not; if you want to
   * change the inclusion or exclusion of an endpoint, pass the successor
   * object in instead.  For example, for Strings, you can request
   * <code>subMap(lowlimit + "\0", highlimit + "\0")</code> to reverse
   * the inclusiveness of both endpoints.
   *
   * @param fromKey the inclusive lower range of the submap
   * @param toKey the exclusive upper range of the submap
   * @return the submap
   * @throws ClassCastException if fromKey or toKey is not comparable to
   *         the map contents
   * @throws IllegalArgumentException if this is a subMap, and fromKey or
   *         toKey is out of range
   * @throws NullPointerException if fromKey or toKey is null but the map
   *         does not allow null keys
   */
  SortedMap subMap(Object fromKey, Object toKey);

  /**
   * Returns a view of the portion of the map greater than or equal to
   * fromKey. The view is backed by this map, so changes in one show up
   * in the other. The submap supports all optional operations of the original.
   * <p>
   *
   * The returned map throws an IllegalArgumentException any time a key is
   * used which is out of the range of fromKey. Note that the endpoint is
   * included; if you do not want the endpoint, pass the successor object in
   * to fromKey.  For example, for Strings, you can request
   * <code>tailMap(limit + "\0")</code>.
   *
   * @param fromKey the inclusive lower range of the submap
   * @return the submap
   * @throws ClassCastException if fromKey is not comparable to the map
   *         contents
   * @throws IllegalArgumentException if this is a subMap, and fromKey is out
   *         of range
   * @throws NullPointerException if fromKey is null but the map does not allow
   *         null keys
   */
  SortedMap tailMap(Object fromKey);
}
