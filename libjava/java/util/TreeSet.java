/* TreeSet.java -- a class providing a TreeMap-backet SortedSet
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.

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
 * This class provides a TreeMap-backed implementation of the 
 * SortedSet interface.
 *
 * Each element in the Set is a key in the backing TreeMap; each key
 * maps to a static token, denoting that the key does, in fact, exist.
 *
 * Most operations are O(log n).
 *
 * TreeSet is a part of the JDK1.2 Collections API.
 *
 * @author      Jon Zeppieri
 */

public class TreeSet extends AbstractSet
  implements SortedSet, Cloneable, Serializable
{
  /** The TreeMap which backs this Set */
  transient SortedMap map;

  static final long serialVersionUID = -2479143000061671589L;

  /**
   * Construct a new TreeSet whose backing TreeMap using the "natural" 
   * ordering of keys.
   */
  public TreeSet()
  {
    map = new TreeMap();
  }

  /** 
   * Construct a new TreeSet whose backing TreeMap uses the supplied 
   * Comparator.
   *
   * @param     oComparator      the Comparator this Set will use
   */
  public TreeSet(Comparator comparator)
  {
    map = new TreeMap(comparator);
  }

  /** 
   * Construct a new TreeSet whose backing TreeMap uses the "natural"
   * orering of the keys and which contains all of the elements in the
   * supplied Collection.
   *
   * @param     oCollection      the new Set will be initialized with all
   *                             of the elements in this Collection
   */
  public TreeSet(Collection collection)
  {
    map = new TreeMap();
    addAll(collection);
  }

  /**
   * Construct a new TreeSet, using the same key ordering as the supplied
   * SortedSet and containing all of the elements in the supplied SortedSet.
   * This constructor runs in linear time.
   *
   * @param     sortedSet       the new TreeSet will use this SortedSet's
   *                            comparator and will initialize itself
   *                            with all of the elements in this SortedSet
   */
  public TreeSet(SortedSet sortedSet)
  {
    TreeMap map = new TreeMap(sortedSet.comparator());
    int i = 0;
    Iterator itr = sortedSet.iterator();
    map.putKeysLinear(itr, sortedSet.size());
    this.map = map;
  }
  
  /* This private constructor is used to implement the subSet() calls around
    a backing TreeMap.SubMap. */
  TreeSet(SortedMap backingMap)
  {
    map = backingMap;
  }

  /** 
   * Adds the spplied Object to the Set if it is not already in the Set;
   * returns true if the element is added, false otherwise
   *
   * @param       obj       the Object to be added to this Set
   */
  public boolean add(Object obj)
  {
    return (map.put(obj, Boolean.TRUE) == null);
  }

  /**
   * Adds all of the elements in the supplied Collection to this TreeSet.
   *
   * @param        c         All of the elements in this Collection
   *                         will be added to the Set.
   *
   * @return       true if the Set is altered, false otherwise
   */
  public boolean addAll(Collection c)
  {
    boolean result = false;
    int size = c.size();
    Iterator itr = c.iterator();

    for (int i = 0; i < size; i++)
      result |= (map.put(itr.next(), Boolean.TRUE) == null);

    return result;
  }

  /**
   * Removes all elements in this Set.
   */
  public void clear()
  {
    map.clear();
  }

  /** Returns a shallow copy of this Set. */
  public Object clone()
  {
    TreeSet copy = null;
    try
      {
        copy = (TreeSet) super.clone();
      }
    catch (CloneNotSupportedException x)
      {      
      }
    copy.map = (SortedMap) ((TreeMap) map).clone();
    return copy;
  }

  /** Returns this Set's comparator */
  public Comparator comparator()
  {
    return map.comparator();
  }

  /** 
   * Returns true if this Set contains the supplied Object, 
   * false otherwise 
   *
   * @param       oObject        the Object whose existence in the Set is
   *                             being tested
   */
  public boolean contains(Object obj)
  {
    return map.containsKey(obj);
  }

  /** Returns true if this Set has size 0, false otherwise */
  public boolean isEmpty()
  {
    return map.isEmpty();
  }

  /** Returns the number of elements in this Set */
  public int size()
  {
    return map.size();
  }

  /** 
   * If the supplied Object is in this Set, it is removed, and true is
   * returned; otherwise, false is returned.
   *
   * @param         obj        the Object we are attempting to remove
   *                           from this Set
   */
  public boolean remove(Object obj)
  {
    return (map.remove(obj) != null);
  }

  /** Returns the first (by order) element in this Set */
  public Object first()
  {
    return map.firstKey();
  }

  /** Returns the last (by order) element in this Set */
  public Object last()
  {
    return map.lastKey();
  }

  /**
   * Returns a view of this Set including all elements in the interval
   * [oFromElement, oToElement).
   *
   * @param       from  the resultant view will contain all
   *                    elements greater than or equal to this element
   * @param       to    the resultant view will contain all
   *                    elements less than this element
   */
  public SortedSet subSet(Object from, Object to)
  {
    return new TreeSet(map.subMap(from, to));
  }

  /**
   * Returns a view of this Set including all elements less than oToElement
   *
   * @param       toElement    the resultant view will contain all
   *                            elements less than this element
   */
  public SortedSet headSet(Object to)
  {
    return new TreeSet(map.headMap(to));
  }

  /**
   * Returns a view of this Set including all elements greater than or
   * equal to oFromElement.
   *
   * @param       from  the resultant view will contain all
   *              elements greater than or equal to this element
   */
  public SortedSet tailSet(Object from)
  {
    return new TreeSet(map.tailMap(from));
  }

  /** Returns in Iterator over the elements in this TreeSet */
  public Iterator iterator()
  {
    return map.keySet().iterator();
  }

  private void writeObject(ObjectOutputStream out) throws IOException
  {
    Iterator itr = map.keySet().iterator();
    int size = map.size();

    out.writeObject(map.comparator());
    out.writeInt(size);

    for (int i = 0; i < size; i++)
      out.writeObject(itr.next());
  }

  private void readObject(ObjectInputStream in)
    throws IOException, ClassNotFoundException
  {
    Comparator comparator = (Comparator) in.readObject();
    int size = in.readInt();
    TreeMap map = new TreeMap(comparator);    
    map.putFromObjStream(in, size, false);
    this.map = map;
  }
}
