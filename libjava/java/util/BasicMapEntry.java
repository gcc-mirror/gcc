/* BasicMapEntry.java -- a class providing a plain-vanilla implementation of
   the Map.Entry interface; could be used anywhere in java.util
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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
 * A class which implements Map.Entry. It is shared by HashMap, TreeMap, and
 * Hashtable.
 *
 * @author      Jon Zeppieri
 */
class BasicMapEntry implements Map.Entry
{
  Object key;
  Object value;

  BasicMapEntry(Object newKey, Object newValue)
  {
    key = newKey;
    value = newValue;
  }

  public final boolean equals(Object o)
  {
    if (!(o instanceof Map.Entry))
      return false;
    Map.Entry e = (Map.Entry) o;
    return (key == null ? e.getKey() == null : key.equals(e.getKey())
            && value == null ? e.getValue() == null 
			     : value.equals(e.getValue()));
  }

  public final Object getKey()
  {
    return key;
  }

  public final Object getValue()
  {
    return value;
  }

  public final int hashCode()
  {
    int kc = (key == null ? 0 : key.hashCode());
    int vc = (value == null ? 0 : value.hashCode());
    return kc ^ vc;
  }

  /** 
   * sets the value of this Map.Entry. Note that this is overriden by 
   * Hashtable.Entry, which does not permit a null value.
   */
  public Object setValue(Object newVal)
  {
    Object r = value;
    value = newVal;
    return r;
  }

  public final String toString()
  {
    return key + "=" + value;
  }
}
