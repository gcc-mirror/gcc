/* Hashtable.java -- a class providing a basic hashtable data structure,
   mapping Object --> Object
   Copyright (C) 1998, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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

import java.io.IOException;
import java.io.Serializable;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

// NOTE: This implementation is very similar to that of HashMap. If you fix
// a bug in here, chances are you should make a similar change to the HashMap
// code.

/**
 * A class which implements a hashtable data structure.
 * <p>
 *
 * This implementation of Hashtable uses a hash-bucket approach. That is:
 * linear probing and rehashing is avoided; instead, each hashed value maps
 * to a simple linked-list which, in the best case, only has one node.
 * Assuming a large enough table, low enough load factor, and / or well
 * implemented hashCode() methods, Hashtable should provide O(1)
 * insertion, deletion, and searching of keys.  Hashtable is O(n) in
 * the worst case for all of these (if all keys hash to the same bucket).
 * <p>
 *
 * This is a JDK-1.2 compliant implementation of Hashtable.  As such, it
 * belongs, partially, to the Collections framework (in that it implements
 * Map).  For backwards compatibility, it inherits from the obsolete and
 * utterly useless Dictionary class.
 * <p>
 *
 * Being a hybrid of old and new, Hashtable has methods which provide redundant
 * capability, but with subtle and even crucial differences.
 * For example, one can iterate over various aspects of a Hashtable with
 * either an Iterator (which is the JDK-1.2 way of doing things) or with an
 * Enumeration.  The latter can end up in an undefined state if the Hashtable
 * changes while the Enumeration is open.
 * <p>
 *
 * Unlike HashMap, Hashtable does not accept `null' as a key value. Also,
 * all accesses are synchronized: in a single thread environment, this is
 * expensive, but in a multi-thread environment, this saves you the effort
 * of extra synchronization. However, the old-style enumerators are not
 * synchronized, because they can lead to unspecified behavior even if
 * they were synchronized. You have been warned.
 * <p>
 *
 * The iterators are <i>fail-fast</i>, meaning that any structural
 * modification, except for <code>remove()</code> called on the iterator
 * itself, cause the iterator to throw a
 * <code>ConcurrentModificationException</code> rather than exhibit
 * non-deterministic behavior.
 *
 * @author Jon Zeppieri
 * @author Warren Levy
 * @author Bryce McKinlay
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see HashMap
 * @see TreeMap
 * @see IdentityHashMap
 * @see LinkedHashMap
 * @since 1.0
 * @status updated to 1.4
 */
public class Hashtable extends Dictionary
  implements Map, Cloneable, Serializable
{
  // WARNING: Hashtable is a CORE class in the bootstrap cycle. See the
  // comments in vm/reference/java/lang/Runtime for implications of this fact.

  /** Default number of buckets. This is the value the JDK 1.3 uses. Some
   * early documentation specified this value as 101. That is incorrect.
   */
  private static final int DEFAULT_CAPACITY = 11;

  /** An "enum" of iterator types. */
  // Package visible for use by nested classes.
  static final int KEYS = 0,
                   VALUES = 1,
                   ENTRIES = 2;

  /**
   * The default load factor; this is explicitly specified by the spec.
   */
  private static final float DEFAULT_LOAD_FACTOR = 0.75f;

  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 1421746759512286392L;

  /**
   * The rounded product of the capacity and the load factor; when the number
   * of elements exceeds the threshold, the Hashtable calls
   * <code>rehash()</code>.
   * @serial
   */
  private int threshold;

  /**
   * Load factor of this Hashtable:  used in computing the threshold.
   * @serial
   */
  private final float loadFactor;

  /**
   * Array containing the actual key-value mappings.
   */
  // Package visible for use by nested classes.
  transient HashEntry[] buckets;

  /**
   * Counts the number of modifications this Hashtable has undergone, used
   * by Iterators to know when to throw ConcurrentModificationExceptions.
   */
  // Package visible for use by nested classes.
  transient int modCount;

  /**
   * The size of this Hashtable:  denotes the number of key-value pairs.
   */
  // Package visible for use by nested classes.
  transient int size;

  /**
   * The cache for {@link #keySet()}.
   */
  private transient Set keys;

  /**
   * The cache for {@link #values()}.
   */
  private transient Collection values;

  /**
   * The cache for {@link #entrySet()}.
   */
  private transient Set entries;

  /**
   * Class to represent an entry in the hash table. Holds a single key-value
   * pair. A Hashtable Entry is identical to a HashMap Entry, except that
   * `null' is not allowed for keys and values.
   */
  private static final class HashEntry extends AbstractMap.BasicMapEntry
  {
    /** The next entry in the linked list. */
    HashEntry next;

    /**
     * Simple constructor.
     * @param key the key, already guaranteed non-null
     * @param value the value, already guaranteed non-null
     */
    HashEntry(Object key, Object value)
    {
      super(key, value);
    }

    /**
     * Resets the value.
     * @param newValue the new value
     * @return the prior value
     * @throws NullPointerException if <code>newVal</code> is null
     */
    public Object setValue(Object newVal)
    {
      if (newVal == null)
        throw new NullPointerException();
      return super.setValue(newVal);
    }
  }

  /**
   * Construct a new Hashtable with the default capacity (11) and the default
   * load factor (0.75).
   */
  public Hashtable()
  {
    this(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR);
  }

  /**
   * Construct a new Hashtable from the given Map, with initial capacity
   * the greater of the size of <code>m</code> or the default of 11.
   * <p>
   *
   * Every element in Map m will be put into this new Hashtable.
   *
   * @param m a Map whose key / value pairs will be put into
   *          the new Hashtable.  <b>NOTE: key / value pairs
   *          are not cloned in this constructor.</b>
   * @throws NullPointerException if m is null, or if m contains a mapping
   *         to or from `null'.
   * @since 1.2
   */
  public Hashtable(Map m)
  {
    this(Math.max(m.size() * 2, DEFAULT_CAPACITY), DEFAULT_LOAD_FACTOR);
    putAllInternal(m);
  }

  /**
   * Construct a new Hashtable with a specific inital capacity and
   * default load factor of 0.75.
   *
   * @param initialCapacity the initial capacity of this Hashtable (&gt;= 0)
   * @throws IllegalArgumentException if (initialCapacity &lt; 0)
   */
  public Hashtable(int initialCapacity)
  {
    this(initialCapacity, DEFAULT_LOAD_FACTOR);
  }

  /**
   * Construct a new Hashtable with a specific initial capacity and
   * load factor.
   *
   * @param initialCapacity the initial capacity (&gt;= 0)
   * @param loadFactor the load factor (&gt; 0, not NaN)
   * @throws IllegalArgumentException if (initialCapacity &lt; 0) ||
   *                                     ! (loadFactor &gt; 0.0)
   */
  public Hashtable(int initialCapacity, float loadFactor)
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException("Illegal Capacity: "
                                         + initialCapacity);
    if (! (loadFactor > 0)) // check for NaN too
      throw new IllegalArgumentException("Illegal Load: " + loadFactor);

    if (initialCapacity == 0)
      initialCapacity = 1;
    buckets = new HashEntry[initialCapacity];
    this.loadFactor = loadFactor;
    threshold = (int) (initialCapacity * loadFactor);
  }

  /**
   * Returns the number of key-value mappings currently in this hashtable.
   * @return the size
   */
  public synchronized int size()
  {
    return size;
  }

  /**
   * Returns true if there are no key-value mappings currently in this table.
   * @return <code>size() == 0</code>
   */
  public synchronized boolean isEmpty()
  {
    return size == 0;
  }

  /**
   * Return an enumeration of the keys of this table. There's no point
   * in synchronizing this, as you have already been warned that the
   * enumeration is not specified to be thread-safe.
   *
   * @return the keys
   * @see #elements()
   * @see #keySet()
   */
  public Enumeration keys()
  {
    return new Enumerator(KEYS);
  }

  /**
   * Return an enumeration of the values of this table. There's no point
   * in synchronizing this, as you have already been warned that the
   * enumeration is not specified to be thread-safe.
   *
   * @return the values
   * @see #keys()
   * @see #values()
   */
  public Enumeration elements()
  {
    return new Enumerator(VALUES);
  }

  /**
   * Returns true if this Hashtable contains a value <code>o</code>,
   * such that <code>o.equals(value)</code>.  This is the same as
   * <code>containsValue()</code>, and is O(n).
   * <p>
   *
   * @param value the value to search for in this Hashtable
   * @return true if at least one key maps to the value
   * @throws NullPointerException if <code>value</code> is null
   * @see #containsValue(Object)
   * @see #containsKey(Object)
   */
  public synchronized boolean contains(Object value)
  {
    return containsValue(value);
  }

  /**
   * Returns true if this Hashtable contains a value <code>o</code>, such that
   * <code>o.equals(value)</code>. This is the new API for the old
   * <code>contains()</code>.
   *
   * @param value the value to search for in this Hashtable
   * @return true if at least one key maps to the value
   * @see #contains(Object)
   * @see #containsKey(Object)
   * @throws NullPointerException if <code>value</code> is null
   * @since 1.2
   */
  public boolean containsValue(Object value)
  {
    for (int i = buckets.length - 1; i >= 0; i--)
      {
        HashEntry e = buckets[i];
        while (e != null)
          {
            if (value.equals(e.value))
              return true;
            e = e.next;
          }
      }

    // Must throw on null argument even if the table is empty
    if (value == null)
      throw new NullPointerException();
 
    return false;
  }

  /**
   * Returns true if the supplied object <code>equals()</code> a key
   * in this Hashtable.
   *
   * @param key the key to search for in this Hashtable
   * @return true if the key is in the table
   * @throws NullPointerException if key is null
   * @see #containsValue(Object)
   */
  public synchronized boolean containsKey(Object key)
  {
    int idx = hash(key);
    HashEntry e = buckets[idx];
    while (e != null)
      {
        if (key.equals(e.key))
          return true;
        e = e.next;
      }
    return false;
  }

  /**
   * Return the value in this Hashtable associated with the supplied key,
   * or <code>null</code> if the key maps to nothing.
   *
   * @param key the key for which to fetch an associated value
   * @return what the key maps to, if present
   * @throws NullPointerException if key is null
   * @see #put(Object, Object)
   * @see #containsKey(Object)
   */
  public synchronized Object get(Object key)
  {
    int idx = hash(key);
    HashEntry e = buckets[idx];
    while (e != null)
      {
        if (key.equals(e.key))
          return e.value;
        e = e.next;
      }
    return null;
  }

  /**
   * Puts the supplied value into the Map, mapped by the supplied key.
   * Neither parameter may be null.  The value may be retrieved by any
   * object which <code>equals()</code> this key.
   *
   * @param key the key used to locate the value
   * @param value the value to be stored in the table
   * @return the prior mapping of the key, or null if there was none
   * @throws NullPointerException if key or value is null
   * @see #get(Object)
   * @see Object#equals(Object)
   */
  public synchronized Object put(Object key, Object value)
  {
    int idx = hash(key);
    HashEntry e = buckets[idx];

    // Check if value is null since it is not permitted.
    if (value == null)
      throw new NullPointerException();

    while (e != null)
      {
        if (key.equals(e.key))
          {
            // Bypass e.setValue, since we already know value is non-null.
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
    modCount++;
    if (++size > threshold)
      {
        rehash();
        // Need a new hash value to suit the bigger table.
        idx = hash(key);
      }

    e = new HashEntry(key, value);

    e.next = buckets[idx];
    buckets[idx] = e;

    return null;
  }

  /**
   * Removes from the table and returns the value which is mapped by the
   * supplied key. If the key maps to nothing, then the table remains
   * unchanged, and <code>null</code> is returned.
   *
   * @param key the key used to locate the value to remove
   * @return whatever the key mapped to, if present
   */
  public synchronized Object remove(Object key)
  {
    int idx = hash(key);
    HashEntry e = buckets[idx];
    HashEntry last = null;

    while (e != null)
      {
        if (key.equals(e.key))
          {
            modCount++;
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

  /**
   * Copies all elements of the given map into this hashtable.  However, no
   * mapping can contain null as key or value.  If this table already has
   * a mapping for a key, the new mapping replaces the current one.
   *
   * @param m the map to be hashed into this
   * @throws NullPointerException if m is null, or contains null keys or values
   */
  public synchronized void putAll(Map m)
  {
    Iterator itr = m.entrySet().iterator();

    for (int msize = m.size(); msize > 0; msize--)
      {
        Map.Entry e = (Map.Entry) itr.next();
        // Optimize in case the Entry is one of our own.
        if (e instanceof AbstractMap.BasicMapEntry)
          {
            AbstractMap.BasicMapEntry entry = (AbstractMap.BasicMapEntry) e;
            put(entry.key, entry.value);
          }
        else
          {
            put(e.getKey(), e.getValue());
          }
      }
  }

  /**
   * Clears the hashtable so it has no keys.  This is O(1).
   */
  public synchronized void clear()
  {
    if (size > 0)
      {
        modCount++;
        Arrays.fill(buckets, null);
        size = 0;
      }
  }

  /**
   * Returns a shallow clone of this Hashtable. The Map itself is cloned,
   * but its contents are not.  This is O(n).
   *
   * @return the clone
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
        // This is impossible.
      }
    copy.buckets = new HashEntry[buckets.length];
    copy.putAllInternal(this);
    // Clear the caches.
    copy.keys = null;
    copy.values = null;
    copy.entries = null;
    return copy;
  }

  /**
   * Converts this Hashtable to a String, surrounded by braces, and with
   * key/value pairs listed with an equals sign between, separated by a
   * comma and space. For example, <code>"{a=1, b=2}"</code>.<p>
   *
   * NOTE: if the <code>toString()</code> method of any key or value
   * throws an exception, this will fail for the same reason.
   *
   * @return the string representation
   */
  public synchronized String toString()
  {
    // Since we are already synchronized, and entrySet().iterator()
    // would repeatedly re-lock/release the monitor, we directly use the
    // unsynchronized HashIterator instead.
    Iterator entries = new HashIterator(ENTRIES);
    StringBuffer r = new StringBuffer("{");
    for (int pos = size; pos > 0; pos--)
      {
        r.append(entries.next());
        if (pos > 1)
          r.append(", ");
      }
    r.append("}");
    return r.toString();
  }

  /**
   * Returns a "set view" of this Hashtable's keys. The set is backed by
   * the hashtable, so changes in one show up in the other.  The set supports
   * element removal, but not element addition.  The set is properly
   * synchronized on the original hashtable.  Sun has not documented the
   * proper interaction of null with this set, but has inconsistent behavior
   * in the JDK. Therefore, in this implementation, contains, remove,
   * containsAll, retainAll, removeAll, and equals just ignore a null key
   * rather than throwing a {@link NullPointerException}.
   *
   * @return a set view of the keys
   * @see #values()
   * @see #entrySet()
   * @since 1.2
   */
  public Set keySet()
  {
    if (keys == null)
      {
        // Create a synchronized AbstractSet with custom implementations of
        // those methods that can be overridden easily and efficiently.
        Set r = new AbstractSet()
        {
          public int size()
          {
            return size;
          }

          public Iterator iterator()
          {
            return new HashIterator(KEYS);
          }

          public void clear()
          {
            Hashtable.this.clear();
          }

          public boolean contains(Object o)
          {
            if (o == null)
              return false;
            return containsKey(o);
          }

          public boolean remove(Object o)
          {
            return Hashtable.this.remove(o) != null;
          }
        };
        // We must specify the correct object to synchronize upon, hence the
        // use of a non-public API
        keys = new Collections.SynchronizedSet(this, r);
      }
    return keys;
  }

  /**
   * Returns a "collection view" (or "bag view") of this Hashtable's values.
   * The collection is backed by the hashtable, so changes in one show up
   * in the other.  The collection supports element removal, but not element
   * addition.  The collection is properly synchronized on the original
   * hashtable.  Sun has not documented the proper interaction of null with
   * this set, but has inconsistent behavior in the JDK. Therefore, in this
   * implementation, contains, remove, containsAll, retainAll, removeAll, and
   * equals just ignore a null value rather than throwing a
   * {@link NullPointerException}.
   *
   * @return a bag view of the values
   * @see #keySet()
   * @see #entrySet()
   * @since 1.2
   */
  public Collection values()
  {
    if (values == null)
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
            return new HashIterator(VALUES);
          }

          public void clear()
          {
            Hashtable.this.clear();
          }
        };
        // We must specify the correct object to synchronize upon, hence the
        // use of a non-public API
        values = new Collections.SynchronizedCollection(this, r);
      }
    return values;
  }

  /**
   * Returns a "set view" of this Hashtable's entries. The set is backed by
   * the hashtable, so changes in one show up in the other.  The set supports
   * element removal, but not element addition.  The set is properly
   * synchronized on the original hashtable.  Sun has not documented the
   * proper interaction of null with this set, but has inconsistent behavior
   * in the JDK. Therefore, in this implementation, contains, remove,
   * containsAll, retainAll, removeAll, and equals just ignore a null entry,
   * or an entry with a null key or value, rather than throwing a
   * {@link NullPointerException}. However, calling entry.setValue(null)
   * will fail.
   * <p>
   *
   * Note that the iterators for all three views, from keySet(), entrySet(),
   * and values(), traverse the hashtable in the same sequence.
   *
   * @return a set view of the entries
   * @see #keySet()
   * @see #values()
   * @see Map.Entry
   * @since 1.2
   */
  public Set entrySet()
  {
    if (entries == null)
      {
        // Create an AbstractSet with custom implementations of those methods
        // that can be overridden easily and efficiently.
        Set r = new AbstractSet()
        {
          public int size()
          {
            return size;
          }

          public Iterator iterator()
          {
            return new HashIterator(ENTRIES);
          }

          public void clear()
          {
            Hashtable.this.clear();
          }

          public boolean contains(Object o)
          {
            return getEntry(o) != null;
          }

          public boolean remove(Object o)
          {
            HashEntry e = getEntry(o);
            if (e != null)
              {
                Hashtable.this.remove(e.key);
                return true;
              }
            return false;
          }
        };
        // We must specify the correct object to synchronize upon, hence the
        // use of a non-public API
        entries = new Collections.SynchronizedSet(this, r);
      }
    return entries;
  }

  /**
   * Returns true if this Hashtable equals the supplied Object <code>o</code>.
   * As specified by Map, this is:
   * <code>
   * (o instanceof Map) && entrySet().equals(((Map) o).entrySet());
   * </code>
   *
   * @param o the object to compare to
   * @return true if o is an equal map
   * @since 1.2
   */
  public boolean equals(Object o)
  {
    // no need to synchronize, entrySet().equals() does that
    if (o == this)
      return true;
    if (!(o instanceof Map))
      return false;

    return entrySet().equals(((Map) o).entrySet());
  }

  /**
   * Returns the hashCode for this Hashtable.  As specified by Map, this is
   * the sum of the hashCodes of all of its Map.Entry objects
   *
   * @return the sum of the hashcodes of the entries
   * @since 1.2
   */
  public synchronized int hashCode()
  {
    // Since we are already synchronized, and entrySet().iterator()
    // would repeatedly re-lock/release the monitor, we directly use the
    // unsynchronized HashIterator instead.
    Iterator itr = new HashIterator(ENTRIES);
    int hashcode = 0;
    for (int pos = size; pos > 0; pos--)
      hashcode += itr.next().hashCode();

    return hashcode;
  }

  /**
   * Helper method that returns an index in the buckets array for `key'
   * based on its hashCode().
   *
   * @param key the key
   * @return the bucket number
   * @throws NullPointerException if key is null
   */
  private int hash(Object key)
  {
    // Note: Inline Math.abs here, for less method overhead, and to avoid
    // a bootstrap dependency, since Math relies on native methods.
    int hash = key.hashCode() % buckets.length;
    return hash < 0 ? -hash : hash;
  }

  /**
   * Helper method for entrySet(), which matches both key and value
   * simultaneously. Ignores null, as mentioned in entrySet().
   *
   * @param o the entry to match
   * @return the matching entry, if found, or null
   * @see #entrySet()
   */
  // Package visible, for use in nested classes.
  HashEntry getEntry(Object o)
  {
    if (! (o instanceof Map.Entry))
      return null;
    Object key = ((Map.Entry) o).getKey();
    if (key == null)
      return null;

    int idx = hash(key);
    HashEntry e = buckets[idx];
    while (e != null)
      {
        if (o.equals(e))
          return e;
        e = e.next;
      }
    return null;
  }

  /**
   * A simplified, more efficient internal implementation of putAll(). The 
   * Map constructor and clone() should not call putAll or put, in order to 
   * be compatible with the JDK implementation with respect to subclasses.
   *
   * @param m the map to initialize this from
   */
  void putAllInternal(Map m)
  {
    Iterator itr = m.entrySet().iterator();
    int msize = m.size();
    this.size = msize;

    for (; msize > 0; msize--)
      {
	Map.Entry e = (Map.Entry) itr.next();
	Object key = e.getKey();
	int idx = hash(key);
	HashEntry he = new HashEntry(key, e.getValue());
	he.next = buckets[idx];
	buckets[idx] = he;
      }
  }

  /**
   * Increases the size of the Hashtable and rehashes all keys to new array
   * indices; this is called when the addition of a new value would cause
   * size() &gt; threshold. Note that the existing Entry objects are reused in
   * the new hash table.
   * <p>
   *
   * This is not specified, but the new size is twice the current size plus
   * one; this number is not always prime, unfortunately. This implementation
   * is not synchronized, as it is only invoked from synchronized methods.
   */
  protected void rehash()
  {
    HashEntry[] oldBuckets = buckets;

    int newcapacity = (buckets.length * 2) + 1;
    threshold = (int) (newcapacity * loadFactor);
    buckets = new HashEntry[newcapacity];

    for (int i = oldBuckets.length - 1; i >= 0; i--)
      {
        HashEntry e = oldBuckets[i];
        while (e != null)
          {
            int idx = hash(e.key);
            HashEntry dest = buckets[idx];

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

            HashEntry next = e.next;
            e.next = null;
            e = next;
          }
      }
  }

  /**
   * Serializes this object to the given stream.
   *
   * @param s the stream to write to
   * @throws IOException if the underlying stream fails
   * @serialData the <i>capacity</i> (int) that is the length of the
   *             bucket array, the <i>size</i> (int) of the hash map
   *             are emitted first.  They are followed by size entries,
   *             each consisting of a key (Object) and a value (Object).
   */
  private synchronized void writeObject(ObjectOutputStream s)
    throws IOException
  {
    // Write the threshold and loadFactor fields.
    s.defaultWriteObject();

    s.writeInt(buckets.length);
    s.writeInt(size);
    // Since we are already synchronized, and entrySet().iterator()
    // would repeatedly re-lock/release the monitor, we directly use the
    // unsynchronized HashIterator instead.
    Iterator it = new HashIterator(ENTRIES);
    while (it.hasNext())
      {
        HashEntry entry = (HashEntry) it.next();
        s.writeObject(entry.key);
        s.writeObject(entry.value);
      }
  }

  /**
   * Deserializes this object from the given stream.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if the underlying stream fails
   * @throws IOException if the underlying stream fails
   * @serialData the <i>capacity</i> (int) that is the length of the
   *             bucket array, the <i>size</i> (int) of the hash map
   *             are emitted first.  They are followed by size entries,
   *             each consisting of a key (Object) and a value (Object).
   */
  private void readObject(ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    // Read the threshold and loadFactor fields.
    s.defaultReadObject();

    // Read and use capacity.
    buckets = new HashEntry[s.readInt()];
    int len = s.readInt();

    // Read and use key/value pairs.
    // TODO: should we be defensive programmers, and check for illegal nulls?
    while (--len >= 0)
      put(s.readObject(), s.readObject());
  }

  /**
   * A class which implements the Iterator interface and is used for
   * iterating over Hashtables.
   * This implementation is parameterized to give a sequential view of
   * keys, values, or entries; it also allows the removal of elements,
   * as per the Javasoft spec.  Note that it is not synchronized; this is
   * a performance enhancer since it is never exposed externally and is
   * only used within synchronized blocks above.
   *
   * @author Jon Zeppieri
   */
  private final class HashIterator implements Iterator
  {
    /**
     * The type of this Iterator: {@link #KEYS}, {@link #VALUES},
     * or {@link #ENTRIES}.
     */
    final int type;
    /**
     * The number of modifications to the backing Hashtable that we know about.
     */
    int knownMod = modCount;
    /** The number of elements remaining to be returned by next(). */
    int count = size;
    /** Current index in the physical hash table. */
    int idx = buckets.length;
    /** The last Entry returned by a next() call. */
    HashEntry last;
    /**
     * The next entry that should be returned by next(). It is set to something
     * if we're iterating through a bucket that contains multiple linked
     * entries. It is null if next() needs to find a new bucket.
     */
    HashEntry next;

    /**
     * Construct a new HashIterator with the supplied type.
     * @param type {@link #KEYS}, {@link #VALUES}, or {@link #ENTRIES}
     */
    HashIterator(int type)
    {
      this.type = type;
    }

    /**
     * Returns true if the Iterator has more elements.
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the hashtable was modified
     */
    public boolean hasNext()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
      return count > 0;
    }

    /**
     * Returns the next element in the Iterator's sequential view.
     * @return the next element
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws NoSuchElementException if there is none
     */
    public Object next()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
      if (count == 0)
        throw new NoSuchElementException();
      count--;
      HashEntry e = next;

      while (e == null)
        e = buckets[--idx];

      next = e.next;
      last = e;
      if (type == VALUES)
        return e.value;
      if (type == KEYS)
        return e.key;
      return e;
    }

    /**
     * Removes from the backing Hashtable the last element which was fetched
     * with the <code>next()</code> method.
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws IllegalStateException if called when there is no last element
     */
    public void remove()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
      if (last == null)
        throw new IllegalStateException();

      Hashtable.this.remove(last.key);
      last = null;
      knownMod++;
    }
  } // class HashIterator


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
   * @author Jon Zeppieri
   */
  private final class Enumerator implements Enumeration
  {
    /**
     * The type of this Iterator: {@link #KEYS} or {@link #VALUES}.
     */
    final int type;
    /** The number of elements remaining to be returned by next(). */
    int count = size;
    /** Current index in the physical hash table. */
    int idx = buckets.length;
    /**
     * Entry which will be returned by the next nextElement() call. It is
     * set if we are iterating through a bucket with multiple entries, or null
     * if we must look in the next bucket.
     */
    HashEntry next;

    /**
     * Construct the enumeration.
     * @param type either {@link #KEYS} or {@link #VALUES}.
     */
    Enumerator(int type)
    {
      this.type = type;
    }

    /**
     * Checks whether more elements remain in the enumeration.
     * @return true if nextElement() will not fail.
     */
    public boolean hasMoreElements()
    {
      return count > 0;
    }

    /**
     * Returns the next element.
     * @return the next element
     * @throws NoSuchElementException if there is none.
     */
    public Object nextElement()
    {
      if (count == 0)
        throw new NoSuchElementException("Hashtable Enumerator");
      count--;
      HashEntry e = next;

      while (e == null)
        e = buckets[--idx];

      next = e.next;
      return type == VALUES ? e.value : e.key;
    }
  } // class Enumerator
} // class Hashtable
