/* HashSet.java -- a class providing a HashMap-backet Set
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.Serializable;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

/**
 * This class provides a HashMap-backed implementation of the 
 * Set interface.
 *
 * Each element in the Set is a key in the backing HashMap; each key
 * maps to a static token, denoting that the key does, in fact, exist.
 *
 * Most operations are O(1), assuming no hash collisions.  In the worst
 * case (where all hases collide), operations are O(n).
 *
 * HashSet is a part of the JDK1.2 Collections API.
 *
 * @author      Jon Zeppieri
 */
public class HashSet extends AbstractSet
  implements Set, Cloneable, Serializable
{
  /** the HashMap which backs this Set */
  transient HashMap map;
  static final long serialVersionUID = -5024744406713321676L;

  /**
   * construct a new, empty HashSet whose backing HashMap has the default 
   * capacity and loadFacor
   */
  public HashSet()
  {
    map = new HashMap();
  }

  /**
   * construct a new, empty HashSet whose backing HashMap has the supplied
   * capacity and the default load factor
   *
   * @param          initialCapacity          the initial capacity of the backing
   *                                          HashMap
   */
  public HashSet(int initialCapacity)
  {
    map = new HashMap(initialCapacity);
  }

  /**
   * construct a new, empty HashSet whose backing HashMap has the supplied
   * capacity and load factor
   *
   * @param          initialCapacity          the initial capacity of the backing
   *                                          HashMap
   * @param          loadFactor               the load factor of the backing HashMap
   */
  public HashSet(int initialCapacity, float loadFactor)
  {
    map = new HashMap(initialCapacity, loadFactor);
  }

  /**
   * construct a new HashSet with the same elements as are in the supplied
   * collection (eliminating any duplicates, of course; the backing HashMap
   * will have the default capacity and load factor
   *
   * @param          c          a collection containing the elements with
   *                            which this set will be initialized
   */
  public HashSet(Collection c)
  {
    map = new HashMap();
    addAll(c);
  }

  /**
   * adds the given Object to the set if it is not already in the Set,
   * returns true if teh element was added, false otherwise
   *
   * @param       o       the Object to add to this Set
   */
  public boolean add(Object o)
  {
    return (map.put(o, Boolean.TRUE) == null);
  }

  /**
   * empties this Set of all elements; this is a fast operation [O(1)]
   */
  public void clear()
  {
    map.clear();
  }

  /**
   * returns a shallow copy of this Set (the Set itself is cloned; its 
   * elements are not)
   */
  public Object clone()
  {
    HashSet copy = null;
    try
      {
	copy = (HashSet) super.clone();
      }
    catch (CloneNotSupportedException x)
      {
      }
    copy.map = (HashMap) map.clone();
    return copy;
  }

  /**
   * returns true if the supplied element is in this Set, false otherwise
   *
   * @param        o         the Object whose presence in this Set we are testing for
   */
  public boolean contains(Object o)
  {
    return map.containsKey(o);
  }

  /** 
   * returns true if this set has no elements in it (size() == 0)
   */
  public boolean isEmpty()
  {
    return map.isEmpty();
  }

  /**
   * returns an Iterator over the elements of this Set; the Iterator allows
   * removal of elements
   */
  public Iterator iterator()
  {
    return map.keySet().iterator();
  }

  /**
   * removes the supplied Object from this Set if it is in the Set; returns
   * true if an element was removed, false otherwise
   */
  public boolean remove(Object o)
  {
    return (map.remove(o) != null);
  }

  /**
   * returns the number of elements in this Set
   */
  public int size()
  {
    return map.size();
  }

  /** Serialize this Object in a manner which is binary-compatible with the 
    * JDK */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    Iterator it = iterator();
    s.writeInt(map.buckets.length);
    s.writeFloat(map.loadFactor);
    s.writeInt(map.size);
    while (it.hasNext())
      s.writeObject(it.next());
  }

  /** Deserialize this Object in a manner which is binary-compatible with 
    * the JDK */
  private void readObject(ObjectInputStream s) throws IOException,
    ClassNotFoundException
  {
    int i, size, capacity;
    float loadFactor;
    Object element;

    capacity = s.readInt();
    loadFactor = s.readFloat();
    size = s.readInt();

    map = new HashMap(capacity, loadFactor);

    for (i = 0; i < size; i++)
      {
	element = s.readObject();
	map.put(element, Boolean.TRUE);
      }
  }
}
