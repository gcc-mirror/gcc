/* BasicMapEntry.java -- a class providing a plain-vanilla implementation of
   the Map.Entry interface; could be used anywhere in java.util
   Copyright (C) 1998, 2000, 2001 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.util;

/**
 * A class which implements Map.Entry. It is shared by HashMap, TreeMap,
 * Hashtable, and Collections. It is not specified by the JDK, but makes
 * life much easier.
 *
 * @author Jon Zeppieri
 * @author Eric Blake <ebb9@email.byu.edu>
 */
class BasicMapEntry implements Map.Entry
{
  /**
   * The key. Package visible for direct manipulation.
   */
  Object key;

  /**
   * The value. Package visible for direct manipulation.
   */
  Object value;

  /**
   * Basic constructor initializes the fields.
   * @param newKey the key
   * @param newValue the value
   */
  BasicMapEntry(Object newKey, Object newValue)
  {
    key = newKey;
    value = newValue;
  }

  /**
   * Compares the specified object with this entry. Returns true only if
   * the object is a mapping of identical key and value. In other words,
   * this must be:
   * <pre>
   * (o instanceof Map.Entry) &&
   * (getKey() == null ? ((HashMap) o).getKey() == null
   *                   : getKey().equals(((HashMap) o).getKey())) &&
   * (getValue() == null ? ((HashMap) o).getValue() == null
   *                   : getValue().equals(((HashMap) o).getValue()))
   * </pre>
   *
   * @param o the object to compare
   * @return true if it is equal
   */
  public final boolean equals(Object o)
  {
    if (! (o instanceof Map.Entry))
      return false;
    // Optimize for our own entries.
    if (o instanceof BasicMapEntry)
      {
        BasicMapEntry e = (BasicMapEntry) o;
        return (AbstractCollection.equals(key, e.key)
                && AbstractCollection.equals(value, e.value));
      }
    Map.Entry e = (Map.Entry) o;
    return (AbstractCollection.equals(key, e.getKey())
            && AbstractCollection.equals(value, e.getValue()));
  }

  /**
   * Get the key corresponding to this entry.
   *
   * @return the key
   */
  public final Object getKey()
  {
    return key;
  }

  /**
   * Get the value corresponding to this entry. If you already called
   * Iterator.remove(), the behavior undefined, but in this case it works.
   *
   * @return the value
   */
  public final Object getValue()
  {
    return value;
  }

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
  public final int hashCode()
  {
    return (AbstractCollection.hashCode(key)
            ^ AbstractCollection.hashCode(value));
  }

  /**
   * Replaces the value with the specified object. This writes through
   * to the map, unless you have already called Iterator.remove(). It
   * may be overridden to restrict a null value.
   *
   * @param newVal the new value to store
   * @return the old value
   * @throws NullPointerException if the map forbids null values
   */
  public Object setValue(Object newVal)
  {
    Object r = value;
    value = newVal;
    return r;
  }

  /**
   * This provides a string representation of the entry. It is of the form
   * "key=value", where string concatenation is used on key and value.
   *
   * @return the string representation
   */
  public final String toString()
  {
    return key + "=" + value;
  }
}
