/* Hashtable.java -- a class providing a basic hashtable data structure,
   mapping Object --> Object
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

// NOTE: This implementation is very similar to that of HashMap. If you fix
// a bug in here, chances are you should make a similar change to the HashMap
// code.

/**
 * a class which implements a Hashtable data structure
 *
 * This implementation of Hashtable uses a hash-bucket approach. That is:
 * linear probing and rehashing is avoided; instead, each hashed value maps
 * to a simple linked-list which, in the best case, only has one node.
 * Assuming a large enough table, low enough load factor, and / or well
 * implemented hashCode() methods, Hashtable should provide O(1) 
 * insertion, deletion, and searching of keys.  Hashtable is O(n) in
 * the worst case for all of these (if all keys has to the same bucket).
 *
 * This is a JDK-1.2 compliant implementation of Hashtable.  As such, it 
 * belongs, partially, to the Collections framework (in that it implements
 * Map).  For backwards compatibility, it inherits from the obsolete and 
 * utterly useless Dictionary class.
 *
 * Being a hybrid of old and new, Hashtable has methods which provide redundant
 * capability, but with subtle and even crucial differences.
 * For example, one can iterate over various aspects of a Hashtable with
 * either an Iterator (which is the JDK-1.2 way of doing things) or with an
 * Enumeration.  The latter can end up in an undefined state if the Hashtable
 * changes while the Enumeration is open.
 *
 * Unlike HashMap, Hashtable does not accept `null' as a key value.
 *
 * @author      Jon Zeppieri
 * @author	Warren Levy
 * @author      Bryce McKinlay
 */
public class Hashtable extends Dictionary 
  implements Map, Cloneable, Serializable
{
  /** Default number of buckets. This is the value the JDK 1.3 uses. Some 
    * early documentation specified this value as 101. That is incorrect. */
  private static final int DEFAULT_CAPACITY = 11;  
  /** The defaulty load factor; this is explicitly specified by the spec. */
  private static final float DEFAULT_LOAD_FACTOR = 0.75f;

  private static final long serialVersionUID = 1421746759512286392L;

  /** 
   * The rounded product of the capacity and the load factor; when the number 
   * of elements exceeds the threshold, the Hashtable calls <pre>rehash()</pre>.
   * @serial
   */
  int threshold;

  /** Load factor of this Hashtable:  used in computing the threshold.
   * @serial
   */
  float loadFactor = DEFAULT_LOAD_FACTOR;

  /** 
   * Array containing the actual key-value mappings
   */
  transient Entry[] buckets;

  /** 
   * counts the number of modifications this Hashtable has undergone, used 
   * by Iterators to know when to throw ConcurrentModificationExceptions. 
   */
  transient int modCount;

  /** the size of this Hashtable:  denotes the number of key-value pairs */
  transient int size;

  /**
   * Class to represent an entry in the hash table. Holds a single key-value
   * pair. A Hashtable Entry is identical to a HashMap Entry, except that
   * `null' is not allowed for keys and values. 
   */
  static class Entry extends BasicMapEntry
  {
    Entry next;
      
    Entry(Object key, Object value)
    {
      super(key, value);
    }

    public final Object setValue(Object newVal)
    {
      if (newVal == null)
        throw new NullPointerException();
      return super.setValue(newVal);
    }
  }

  /**
   * construct a new Hashtable with the default capacity (11) and the default
   * load factor (0.75).
   */
  public Hashtable()
  {
    this(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR);
  }

  /**
   * construct a new Hashtable from the given Map
   * 
   * every element in Map t will be put into this new Hashtable
   *
   * @param     t        a Map whose key / value pairs will be put into
   *                     the new Hashtable.  <b>NOTE: key / value pairs
   *                     are not cloned in this constructor</b>
   */
  public Hashtable(Map m)
  {
    int size = Math.max(m.size() * 2, DEFAULT_CAPACITY);
    buckets = new Entry[size];
    threshold = (int) (size * loadFactor);
    putAll(m);
  }

  /**
   * construct a new Hashtable with a specific inital capacity 
   *
   * @param   initialCapacity     the initial capacity of this Hashtable (>=0)
   *
   * @throws   IllegalArgumentException    if (initialCapacity < 0)
   */
  public Hashtable(int initialCapacity) throws IllegalArgumentException
  {
    this(initialCapacity, DEFAULT_LOAD_FACTOR);
  }

  /**
   * construct a new Hashtable with a specific inital capacity and load factor
   *
   * @param   initialCapacity  the initial capacity (>=0)
   * @param   loadFactor       the load factor
   * 
   * @throws   IllegalArgumentException    if (initialCapacity < 0) ||
   *                                          (initialLoadFactor <= 0.0)
   */
  public Hashtable(int initialCapacity, float loadFactor)
    throws IllegalArgumentException
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException("Illegal Initial Capacity: " 
      					 + initialCapacity);    
    if (loadFactor <= 0)
      throw new IllegalArgumentException("Illegal Load Factor: " + loadFactor);
    
    if (initialCapacity == 0)
      initialCapacity = 1;    
    buckets = new Entry[initialCapacity];
    this.loadFactor = loadFactor;
    this.threshold = (int) (initialCapacity * loadFactor);
  }

  /** Returns the number of key-value mappings currently in this Map */
  public int size()
  {
    return size;
  }

  /** returns true if there are no key-value mappings currently in this Map */
  public boolean isEmpty()
  {
    return size == 0;
  }

  public synchronized Enumeration keys()
  {
    return new Enumerator(Enumerator.KEYS);
  }
  
  public synchronized Enumeration elements()
  {
    return new Enumerator(Enumerator.VALUES);
  }

  /**
   * returns true if this Hashtable contains a value <pre>o</pre>,
   * such that <pre>o.equals(value)</pre>.
   *
   * Note: this is one of the <i>old</i> Hashtable methods which does
   * not like null values; it throws NullPointerException if the
   * supplied parameter is null.
   *
   * @param     value        the value to search for in this Hashtable
   *
   * @throws NullPointerException if <pre>value</pre> is null 
   */
  public synchronized boolean contains(Object value)
  {
    for (int i = 0; i < buckets.length; i++)
      {
	Entry e = buckets[i];
	while (e != null)
	  {
	    if (value.equals(e.value))
	      return true;
	    e = e.next;
	  }
      }
    return false;
  }

  /**
   * returns true if this Hashtable contains a value <pre>o</pre>, such that
   * <pre>o.equals(value)</pre>.
   *
   * @param      value       the value to search for in this Hashtable
   *
   * @throws NullPointerException if <pre>value</pre> is null 
   */
  public boolean containsValue(Object value)
  {
    return contains(value);
  }

  /** 
   * returns true if the supplied object equals (<pre>equals()</pre>) a key
   * in this Hashtable 
   *
   * @param       key        the key to search for in this Hashtable
   */
  public synchronized boolean containsKey(Object key)
  {
    int idx = hash(key);
    Entry e = buckets[idx];
    while (e != null)
      {
        if (key.equals(e.key))
	  return true;
	e = e.next;
      }
    return false;
  }

  /**
   * return the value in this Hashtable associated with the supplied key, or <pre>null</pre>
   * if the key maps to nothing
   *
   * @param     key      the key for which to fetch an associated value
   */
  public synchronized Object get(Object key)
  {
    int idx = hash(key);
    Entry e = buckets[idx];
    while (e != null)
      {
        if (key.equals(e.key))
	  return e.value;
	e = e.next;
      }
    return null;
  }

  /**
   * puts the supplied value into the Map, mapped by the supplied key
   *
   * @param       key        the key used to locate the value
   * @param       value      the value to be stored in the table
   */
  public synchronized Object put(Object key, Object value)
  {
    modCount++;
    int idx = hash(key);
    Entry e = buckets[idx];
    
    // Hashtable does not accept null values. This method doesn't dereference 
    // `value' anywhere, so check for it explicitly.
    if (value == null)
      throw new NullPointerException();

    while (e != null)
      {
        if (key.equals(e.key))
	  {
	    Object r = e.value;
	    e.value = value;
	    return r;
	  }
	else
	  {
	    e = e.next;
	  }
      }
    
    // At this point, we know we need to add a new entry.
    if (++size > threshold)
      {
	rehash();
	// Need a new hash value to suit the bigger table.
	idx = hash(key);
      }

    e = new Entry(key, value);
    
    e.next = buckets[idx];
    buckets[idx] = e;
    
    return null;
  }

  /**
   * removes from the table and returns the value which is mapped by the 
   * supplied key; if the key maps to nothing, then the table remains 
   * unchanged, and <pre>null</pre> is returned
   *
   * @param    key     the key used to locate the value to remove
   */
  public synchronized Object remove(Object key)
  {
    modCount++;
    int idx = hash(key);
    Entry e = buckets[idx];
    Entry last = null;

    while (e != null)
      {
        if (key.equals(e.key))
	  {
	    if (last == null)
	      buckets[idx] = e.next;
	    else
	      last.next = e.next;
	    size--;
	    return e.value;
	  }
	last = e;
	e = e.next;
      }
    return null;
  }

  public synchronized void putAll(Map m)
  {
    int msize = m.size();
    Iterator itr = m.entrySet().iterator();
    
    for (int i=0; i < msize; i++)
      {
        Map.Entry e = (Map.Entry) itr.next();
	// Optimize in case the Entry is one of our own.
	if (e instanceof BasicMapEntry)
	  {
	    BasicMapEntry entry = (BasicMapEntry) e;
	    put(entry.key, entry.value);
	  }
	else
	  {
            put(e.getKey(), e.getValue());
	  }
      }
  }
  
  public synchronized void clear()
  {
    modCount++;
    for (int i=0; i < buckets.length; i++)
      {
        buckets[i] = null;
      }
    size = 0;
  }

  /** 
   * returns a shallow clone of this Hashtable (i.e. the Map itself is cloned, 
   * but its contents are not)
   */
  public synchronized Object clone()
  {
    Hashtable copy = null;
    try
      {
        copy = (Hashtable) super.clone();
      }
    catch (CloneNotSupportedException x)
      {
      }
    copy.buckets = new Entry[buckets.length];
    
    for (int i=0; i < buckets.length; i++)
      {
        Entry e = buckets[i];
	Entry last = null;
	
	while (e != null)
	  {
	    if (last == null)
	      {
		copy.buckets[i] = new Entry(e.key, e.value);
		last = copy.buckets[i];
              }
	    else		
              {
	        last.next = new Entry(e.key, e.value);
		last = last.next;
	      }
	    e = e.next;
	  }
      }
    return copy;
  }
  
  public synchronized String toString()
  {
    Iterator entries = entrySet().iterator();
    StringBuffer r = new StringBuffer("{");
    for (int pos = 0; pos < size; pos++)
      {
        r.append(entries.next());
	if (pos < size - 1)
	  r.append(", ");
      }
    r.append("}");
    return r.toString();    
  }

  /** returns a "set view" of this Hashtable's keys */
  public Set keySet()
  {
    // Create a synchronized AbstractSet with custom implementations of those 
    // methods that can be overriden easily and efficiently.
    Set r = new AbstractSet()
    {
      public int size()
      {
        return size;
      }
      
      public Iterator iterator()
      {
        return new HashIterator(HashIterator.KEYS);
      }
            
      public void clear()
      {
        Hashtable.this.clear();
      }

      public boolean contains(Object o)
      {
        return Hashtable.this.containsKey(o);
      }
      
      public boolean remove(Object o)
      {
        return (Hashtable.this.remove(o) != null);
      }
    };

    return Collections.synchronizedSet(r);
  }
  
  /** Returns a "collection view" (or "bag view") of this Hashtable's values. 
    */
  public Collection values()
  {
    // We don't bother overriding many of the optional methods, as doing so
    // wouldn't provide any significant performance advantage.
    Collection r = new AbstractCollection()
    {
      public int size()
      {
        return size;
      }
      
      public Iterator iterator()
      {
        return new HashIterator(HashIterator.VALUES);
      }
      
      public void clear()
      {
        Hashtable.this.clear();
      }
    };
    
    return Collections.synchronizedCollection(r);
  }

  /** Returns a "set view" of this Hashtable's entries. */
  public Set entrySet()
  {
    // Create an AbstractSet with custom implementations of those methods that 
    // can be overriden easily and efficiently.
    Set r = new AbstractSet()
    {
      public int size()
      {
        return size;
      }
      
      public Iterator iterator()
      {
        return new HashIterator(HashIterator.ENTRIES);
      }
            
      public void clear()
      {
        Hashtable.this.clear();
      }

      public boolean contains(Object o)
      {
        if (!(o instanceof Map.Entry))
	  return false;
	Map.Entry me = (Map.Entry) o;
	Entry e = getEntry(me);
	return (e != null);
      }
      
      public boolean remove(Object o)
      {
        if (!(o instanceof Map.Entry))
	  return false;
	Map.Entry me = (Map.Entry) o;
	Entry e = getEntry(me);
	if (e != null)
	  {
	    Hashtable.this.remove(e.key);
	    return true;
	  }
	return false;
      }
    };
    
    return Collections.synchronizedSet(r);
  }
  
  /** returns true if this Hashtable equals the supplied Object <pre>o</pre>;
   * that is:
   * <pre>
   * if (o instanceof Map)
   * and
   * o.keySet().equals(keySet())
   * and
   * for each key in o.keySet(), o.get(key).equals(get(key))
   *</pre>
   */
  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    if (!(o instanceof Map))
      return false;

    Map m = (Map) o;
    Set s = m.entrySet();
    Iterator itr = entrySet().iterator();

    if (m.size() != size)
      return false;

    for (int pos = 0; pos < size; pos++)
      {
	if (!s.contains(itr.next()))
	  return false;
      }
    return true;    
  }
  
  /** a Map's hashCode is the sum of the hashCodes of all of its
      Map.Entry objects */
  public int hashCode()
  {
    int hashcode = 0;
    Iterator itr = entrySet().iterator();
    for (int pos = 0; pos < size; pos++)
      {
	hashcode += itr.next().hashCode();
      }
    return hashcode;  
  }
  
  /** Return an index in the buckets array for `key' based on its hashCode() */
  private int hash(Object key)
  {
    return Math.abs(key.hashCode() % buckets.length);
  }

  private Entry getEntry(Map.Entry me)
  {
    int idx = hash(me.getKey());
    Entry e = buckets[idx];
    while (e != null)
      {
        if (e.equals(me))
	  return e;
	e = e.next;
      }
    return null;
  }
  
  /** 
   * increases the size of the Hashtable and rehashes all keys to new array 
   * indices; this is called when the addition of a new value would cause 
   * size() > threshold. Note that the existing Entry objects are reused in 
   * the new hash table.
   */
  protected void rehash()
  {
    Entry[] oldBuckets = buckets;
    
    int newcapacity = (buckets.length * 2) + 1;
    threshold = (int) (newcapacity * loadFactor);
    buckets = new Entry[newcapacity];
    
    for (int i = 0; i < oldBuckets.length; i++)
      {
	Entry e = oldBuckets[i];
        while (e != null)
	  {
	    int idx = hash(e.key);
	    Entry dest = buckets[idx];

	    if (dest != null)
	      {
		while (dest.next != null)
        	  dest = dest.next;
		dest.next = e;
	      }
	    else
	      {
        	buckets[idx] = e;
	      }

	    Entry next = e.next;
	    e.next = null;
	    e = next;
	  }
      }
  }

  /**
   * Serializes this object to the given stream.
   * @serialdata the <i>capacity</i>(int) that is the length of the
   * bucket array, the <i>size</i>(int) of the hash map are emitted
   * first.  They are followed by size entries, each consisting of
   * a key (Object) and a value (Object).
   */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    // the threshold and loadFactor fields
    s.defaultWriteObject();

    s.writeInt(buckets.length);
    s.writeInt(size);
    Iterator it = entrySet().iterator();
    while (it.hasNext())
      {
	Map.Entry entry = (Map.Entry) it.next();
	s.writeObject(entry.getKey());
	s.writeObject(entry.getValue());
      }
  }

  /**
   * Deserializes this object from the given stream.
   * @serialdata the <i>capacity</i>(int) that is the length of the
   * bucket array, the <i>size</i>(int) of the hash map are emitted
   * first.  They are followed by size entries, each consisting of
   * a key (Object) and a value (Object).
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    // the threshold and loadFactor fields
    s.defaultReadObject();

    int capacity = s.readInt();
    int len = s.readInt();
    size = 0;
    modCount = 0;
    buckets = new Entry[capacity];

    for (int i = 0; i < len; i++)
      {
	Object key = s.readObject();
	Object value = s.readObject();
	put(key, value);
      }
  }

  /**
   * a class which implements the Iterator interface and is used for
   * iterating over Hashtables;
   * this implementation is parameterized to give a sequential view of
   * keys, values, or entries; it also allows the removal of elements, 
   * as per the Javasoft spec.
   *
   * @author       Jon Zeppieri
   */
  class HashIterator implements Iterator
  {
    static final int KEYS = 0,
                     VALUES = 1,
		     ENTRIES = 2;
		    
    // The type of this Iterator: KEYS, VALUES, or ENTRIES.
    int type;
    // The number of modifications to the backing Hashtable that we know about.
    int knownMod;
    // The total number of elements returned by next(). Used to determine if
    // there are more elements remaining.
    int count;
    // Current index in the physical hash table.
    int idx;
    // The last Entry returned by a next() call.
    Entry last;
    // The next entry that should be returned by next(). It is set to something
    // if we're iterating through a bucket that contains multiple linked 
    // entries. It is null if next() needs to find a new bucket.
    Entry next;

    /* Construct a new HashIterator with the supplied type: 
       KEYS, VALUES, or ENTRIES */
    HashIterator(int type)
    {
      this.type = type;
      knownMod = Hashtable.this.modCount;
      count = 0;
      idx = buckets.length;
    }

    /** returns true if the Iterator has more elements */
    public boolean hasNext()
    {
      if (knownMod != Hashtable.this.modCount)
	throw new ConcurrentModificationException();
      return count < size;
    }

    /** Returns the next element in the Iterator's sequential view. */
    public Object next()
    {
      if (knownMod != Hashtable.this.modCount)
	throw new ConcurrentModificationException();
      if (count == size)
        throw new NoSuchElementException();
      count++;
      Entry e = null;
      if (next != null)
        e = next;

      while (e == null)
        {
	  e = buckets[--idx];
	}

      next = e.next;
      last = e;
      if (type == VALUES)
        return e.value;
      else if (type == KEYS)
        return e.key;
      return e;
    }

    /** 
     * Removes from the backing Hashtable the last element which was fetched 
     * with the <pre>next()</pre> method.
     */
    public void remove()
    {
      if (knownMod != Hashtable.this.modCount)
	throw new ConcurrentModificationException();
      if (last == null)
	{
	  throw new IllegalStateException();
	}
      else
	{
	  Hashtable.this.remove(last.key);
	  knownMod++;
	  count--;
	  last = null;
	}
    }
  }


  /**
   * Enumeration view of this Hashtable, providing sequential access to its 
   * elements; this implementation is parameterized to provide access either 
   * to the keys or to the values in the Hashtable.
   *
   * <b>NOTE</b>: Enumeration is not safe if new elements are put in the table
   * as this could cause a rehash and we'd completely lose our place.  Even
   * without a rehash, it is undetermined if a new element added would
   * appear in the enumeration.  The spec says nothing about this, but
   * the "Java Class Libraries" book infers that modifications to the
   * hashtable during enumeration causes indeterminate results.  Don't do it!
   *
   * @author       Jon Zeppieri
   */
  class Enumerator implements Enumeration
  {
    static final int KEYS = 0;
    static final int VALUES = 1;
    
    int type;
    // The total number of elements returned by nextElement(). Used to 
    // determine if there are more elements remaining.
    int count;
    // current index in the physical hash table.
    int idx;
    // the last Entry returned.
    Entry last;
    
    Enumerator(int type)
    {
      this.type = type;
      this.count = 0;
      this.idx = buckets.length;
    }

    public boolean hasMoreElements()
    {
      return count < Hashtable.this.size;    
    }

    public Object nextElement()
    {
      if (count >= size)
        throw new NoSuchElementException();
      count++;
      Entry e = null;
      if (last != null)
        e = last.next;

      while (e == null)
        {
	  e = buckets[--idx];
	}

      last = e;
      if (type == VALUES)
        return e.value;
      return e.key;
    }
  }  
}
