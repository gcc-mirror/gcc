/* HashMap.java -- a class providing a basic hashtable data structure,
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

// NOTE: This implementation is very similar to that of Hashtable. If you fix
// a bug in here, chances are you should make a similar change to the Hashtable
// code.

/**
 * This class provides a hashtable-backed implementation of the
 * Map interface.  
 *
 * It uses a hash-bucket approach; that is, hash
 * collisions are handled by linking the new node off of the
 * pre-existing node (or list of nodes).  In this manner, techniques 
 * such as linear probing (which can casue primary clustering) and 
 * rehashing (which does not fit very well with Java's method of 
 * precomputing hash codes) are avoided.  
 *
 * Under ideal circumstances (no collisions, HashMap offers O(1) 
 * performance on most operations (<pre>containsValue()</pre> is,
 * of course, O(n)).  In the worst case (all keys map to the same 
 * hash code -- very unlikely), most operations are O(n).
 *
 * HashMap is part of the JDK1.2 Collections API.  It differs from 
 * Hashtable in that it accepts the null key and null values, and it
 * does not support "Enumeration views."
 *
 * @author         Jon Zeppieri
 * @author         Jochen Hoenicke
 * @author	   Bryce McKinlay
 */
public class HashMap extends AbstractMap
  implements Map, Cloneable, Serializable
{
  /** Default number of buckets. This is the value the JDK 1.3 uses. Some 
    * early documentation specified this value as 101. That is incorrect. */
  private static final int DEFAULT_CAPACITY = 11;  
  /** The defaulty load factor; this is explicitly specified by the spec. */
  private static final float DEFAULT_LOAD_FACTOR = 0.75f;

  private static final long serialVersionUID = 362498820763181265L;

  /** 
   * The rounded product of the capacity and the load factor; when the number 
   * of elements exceeds the threshold, the HashMap calls <pre>rehash()</pre>.
   * @serial
   */
  int threshold;

  /** Load factor of this HashMap:  used in computing the threshold.
   * @serial
   */
  float loadFactor = DEFAULT_LOAD_FACTOR;

  /** 
   * Array containing the actual key-value mappings
   */
  transient Entry[] buckets;

  /** 
   * counts the number of modifications this HashMap has undergone, used 
   * by Iterators to know when to throw ConcurrentModificationExceptions.
   */
  transient int modCount;

  /** the size of this HashMap:  denotes the number of key-value pairs */
  transient int size;

  /**
   * Class to represent an entry in the hash table. Holds a single key-value
   * pair.
   */
  static class Entry extends BasicMapEntry
  {
    Entry next;
    
    Entry(Object key, Object value)
    {
      super(key, value);
    }
  }

  /**
   * construct a new HashMap with the default capacity (11) and the default
   * load factor (0.75).
   */
  public HashMap()
  {
    this(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR);
  }

  /**
   * construct a new HashMap from the given Map
   * 
   * every element in Map t will be put into this new HashMap
   *
   * @param     t        a Map whose key / value pairs will be put into
   *                     the new HashMap.  <b>NOTE: key / value pairs
   *                     are not cloned in this constructor</b>
   */
  public HashMap(Map m)
  {
    int size = Math.max(m.size() * 2, DEFAULT_CAPACITY);
    buckets = new Entry[size];
    threshold = (int) (size * loadFactor);
    putAll(m);
  }

  /**
   * construct a new HashMap with a specific inital capacity 
   *
   * @param   initialCapacity     the initial capacity of this HashMap (>=0)
   *
   * @throws   IllegalArgumentException    if (initialCapacity < 0)
   */
  public HashMap(int initialCapacity) throws IllegalArgumentException
  {
    this(initialCapacity, DEFAULT_LOAD_FACTOR);
  }

  /**
   * construct a new HashMap with a specific inital capacity and load factor
   *
   * @param   initialCapacity  the initial capacity (>=0)
   * @param   loadFactor       the load factor
   * 
   * @throws   IllegalArgumentException    if (initialCapacity < 0) ||
   *                                          (loadFactor <= 0)
   */
  public HashMap(int initialCapacity, float loadFactor)
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

  /** returns the number of kay-value mappings currently in this Map */
  public int size()
  {
    return size;
  }

  /** returns true if there are no key-value mappings currently in this Map */
  public boolean isEmpty()
  {
    return size == 0;
  }

  /**
   * returns true if this HashMap contains a value <pre>o</pre>, such that
   * <pre>o.equals(value)</pre>.
   *
   * @param      value       the value to search for in this Hashtable
   */
  public boolean containsValue(Object value)
  {
    for (int i = 0; i < buckets.length; i++)
      {
	Entry e = buckets[i];
	while (e != null)
	  {
	    if (value == null ? e.value == null : value.equals(e.value))
	      return true;
	    e = e.next;
	  }
      }
    return false;
  }

  /** 
   * returns true if the supplied object equals (<pre>equals()</pre>) a key
   * in this HashMap 
   *
   * @param       key        the key to search for in this HashMap
   */
  public boolean containsKey(Object key)
  {
    int idx = hash(key);
    Entry e = buckets[idx];
    while (e != null)
      {
        if (key == null ? e.key == null : key.equals(e.key))
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
  public Object get(Object key)
  {
    int idx = hash(key);
    Entry e = buckets[idx];
    while (e != null)
      {
        if (key == null ? e.key == null : key.equals(e.key))
	  return e.value;
	e = e.next;
      }
    return null;
  }

  /**
   * puts the supplied value into the Map, mapped by the supplied key
   *
   * @param       key        the HashMap key used to locate the value
   * @param       value      the value to be stored in the HashMap
   */
  public Object put(Object key, Object value)
  {
    modCount++;
    int idx = hash(key);
    Entry e = buckets[idx];
    
    while (e != null)
      {
        if (key == null ? e.key == null : key.equals(e.key))
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
   * removes from the HashMap and returns the value which is mapped by the 
   * supplied key; if the key maps to nothing, then the HashMap remains unchanged,
   * and <pre>null</pre> is returned
   *
   * @param    key     the key used to locate the value to remove from the HashMap
   */
  public Object remove(Object key)
  {
    modCount++;
    int idx = hash(key);
    Entry e = buckets[idx];
    Entry last = null;

    while (e != null)
      {
        if (key == null ? e.key == null : key.equals(e.key))
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

  public void putAll(Map m)
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
  
  public void clear()
  {
    modCount++;
    for (int i=0; i < buckets.length; i++)
      {
        buckets[i] = null;
      }
    size = 0;
  }

  /** 
   * returns a shallow clone of this HashMap (i.e. the Map itself is cloned, but
   * its contents are not)
   */
  public Object clone()
  {
    HashMap copy = null;
    try
      {
        copy = (HashMap) super.clone();
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

  /** returns a "set view" of this HashMap's keys */
  public Set keySet()
  {
    // Create an AbstractSet with custom implementations of those methods that 
    // can be overriden easily and efficiently.
    return new AbstractSet()
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
        HashMap.this.clear();
      }

      public boolean contains(Object o)
      {
        return HashMap.this.containsKey(o);
      }
      
      public boolean remove(Object o)
      {
        // Test against the size of the HashMap to determine if anything
	// really got removed. This is neccessary because the return value of
	// HashMap.remove() is ambiguous in the null case.
        int oldsize = size;
        HashMap.this.remove(o);
	return (oldsize != size);
      }
    };
  }
  
  /** Returns a "collection view" (or "bag view") of this HashMap's values. */
  public Collection values()
  {
    // We don't bother overriding many of the optional methods, as doing so
    // wouldn't provide any significant performance advantage.
    return new AbstractCollection()
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
        HashMap.this.clear();
      }
    };
  }

  /** Returns a "set view" of this HashMap's entries. */
  public Set entrySet()
  {
    // Create an AbstractSet with custom implementations of those methods that 
    // can be overriden easily and efficiently.
    return new AbstractSet()
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
        HashMap.this.clear();
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
	    HashMap.this.remove(e.key);
	    return true;
	  }
	return false;
      }
    };
  }
  
  /** Return an index in the buckets array for `key' based on its hashCode() */
  private int hash(Object key)
  {
    if (key == null)
      return 0;
    else
      return Math.abs(key.hashCode() % buckets.length);
  }

  /** Return an Entry who's key and value equal the supplied Map.Entry. 
    * This is used by entrySet's contains() and remove() methods. They can't
    * use contains(key) and remove(key) directly because that would result
    * in entries with the same key but a different value being matched. */
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
   * increases the size of the HashMap and rehashes all keys to new array 
   * indices; this is called when the addition of a new value would cause 
   * size() > threshold. Note that the existing Entry objects are reused in 
   * the new hash table.
   */
  private void rehash()
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
   * Iterate over HashMap's entries.
   * This implementation is parameterized to give a sequential view of
   * keys, values, or entries.
   *
   * @author       Jon Zeppieri
   */
  class HashIterator implements Iterator
  {
    static final int KEYS = 0,
                     VALUES = 1,
		     ENTRIES = 2;
		    
    // the type of this Iterator: KEYS, VALUES, or ENTRIES.
    int type;
    // the number of modifications to the backing Hashtable that we know about.
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

    /* construct a new HashtableIterator with the supllied type: 
       KEYS, VALUES, or ENTRIES */
    HashIterator(int type)
    {
      this.type = type;
      knownMod = HashMap.this.modCount;
      count = 0;
      idx = buckets.length;
    }

    /** returns true if the Iterator has more elements */
    public boolean hasNext()
    {
      if (knownMod != HashMap.this.modCount)
	throw new ConcurrentModificationException();
      return count < size;
    }

    /** returns the next element in the Iterator's sequential view */
    public Object next()
    {
      if (knownMod != HashMap.this.modCount)
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
     * removes from the backing HashMap the last element which was fetched with the
     * <pre>next()</pre> method
     */
    public void remove()
    {
      if (knownMod != HashMap.this.modCount)
	throw new ConcurrentModificationException();
      if (last == null)
	{
	  throw new IllegalStateException();
	}
      else
	{
	  HashMap.this.remove(last.key);
	  knownMod++;
	  count--;
	  last = null;
	}
    }
  }
}
