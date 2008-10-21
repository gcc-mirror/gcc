/* Hashtable.java -- a class providing a basic hashtable data structure,
   mapping Object --> Object
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2004, 2005, 2006
   Free Software Foundation, Inc.

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

package java.util;

import gnu.java.lang.CPStringBuilder;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

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
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see HashMap
 * @see TreeMap
 * @see IdentityHashMap
 * @see LinkedHashMap
 * @since 1.0
 * @status updated to 1.4
 */
public class Hashtable<K, V> extends Dictionary<K, V>
  implements Map<K, V>, Cloneable, Serializable
{
  // WARNING: Hashtable is a CORE class in the bootstrap cycle. See the
  // comments in vm/reference/java/lang/Runtime for implications of this fact.

  /** Default number of buckets. This is the value the JDK 1.3 uses. Some
   * early documentation specified this value as 101. That is incorrect.
   */
  private static final int DEFAULT_CAPACITY = 11;

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
  transient HashEntry<K, V>[] buckets;

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
  private transient Set<K> keys;

  /**
   * The cache for {@link #values()}.
   */
  private transient Collection<V> values;

  /**
   * The cache for {@link #entrySet()}.
   */
  private transient Set<Map.Entry<K, V>> entries;

  /**
   * Class to represent an entry in the hash table. Holds a single key-value
   * pair. A Hashtable Entry is identical to a HashMap Entry, except that
   * `null' is not allowed for keys and values.
   */
  private static final class HashEntry<K, V>
    extends AbstractMap.SimpleEntry<K, V>
  {
    /** The next entry in the linked list. */
    HashEntry<K, V> next;

    /**
     * Simple constructor.
     * @param key the key, already guaranteed non-null
     * @param value the value, already guaranteed non-null
     */
    HashEntry(K key, V value)
    {
      super(key, value);
    }

    /**
     * Resets the value.
     * @param newVal the new value
     * @return the prior value
     * @throws NullPointerException if <code>newVal</code> is null
     */
    public V setValue(V newVal)
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
  public Hashtable(Map<? extends K, ? extends V> m)
  {
    this(Math.max(m.size() * 2, DEFAULT_CAPACITY), DEFAULT_LOAD_FACTOR);
    putAll(m);
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
    buckets = (HashEntry<K, V>[]) new HashEntry[initialCapacity];
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
  public Enumeration<K> keys()
  {
    return new KeyEnumerator();
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
  public Enumeration<V> elements()
  {
    return new ValueEnumerator();
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
    if (value == null)
      throw new NullPointerException();

    for (int i = buckets.length - 1; i >= 0; i--)
      {
        HashEntry<K, V> e = buckets[i];
        while (e != null)
          {
            if (e.value.equals(value))
              return true;
            e = e.next;
          }
      }

    return false;  
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
    // Delegate to older method to make sure code overriding it continues 
    // to work.
    return contains(value);
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
    HashEntry<K, V> e = buckets[idx];
    while (e != null)
      {
        if (e.key.equals(key))
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
  public synchronized V get(Object key)
  {
    int idx = hash(key);
    HashEntry<K, V> e = buckets[idx];
    while (e != null)
      {
        if (e.key.equals(key))
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
  public synchronized V put(K key, V value)
  {
    int idx = hash(key);
    HashEntry<K, V> e = buckets[idx];

    // Check if value is null since it is not permitted.
    if (value == null)
      throw new NullPointerException();

    while (e != null)
      {
        if (e.key.equals(key))
          {
            // Bypass e.setValue, since we already know value is non-null.
            V r = e.value;
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

    e = new HashEntry<K, V>(key, value);

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
  public synchronized V remove(Object key)
  {
    int idx = hash(key);
    HashEntry<K, V> e = buckets[idx];
    HashEntry<K, V> last = null;

    while (e != null)
      {
        if (e.key.equals(key))
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
  public synchronized void putAll(Map<? extends K, ? extends V> m)
  {
    final Map<K,V> addMap = (Map<K,V>) m;
    final Iterator<Map.Entry<K,V>> it = addMap.entrySet().iterator();
    while (it.hasNext())
      {
	final Map.Entry<K,V> e = it.next();
        // Optimize in case the Entry is one of our own.
        if (e instanceof AbstractMap.SimpleEntry)
          {
            AbstractMap.SimpleEntry<? extends K, ? extends V> entry
	      = (AbstractMap.SimpleEntry<? extends K, ? extends V>) e;
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
    Hashtable<K, V> copy = null;
    try
      {
        copy = (Hashtable<K, V>) super.clone();
      }
    catch (CloneNotSupportedException x)
      {
        // This is impossible.
      }
    copy.buckets = (HashEntry<K, V>[]) new HashEntry[buckets.length];
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
    // unsynchronized EntryIterator instead.
    Iterator<Map.Entry<K, V>> entries = new EntryIterator();
    CPStringBuilder r = new CPStringBuilder("{");
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
  public Set<K> keySet()
  {
    if (keys == null)
      {
        // Create a synchronized AbstractSet with custom implementations of
        // those methods that can be overridden easily and efficiently.
        Set<K> r = new AbstractSet<K>()
        {
          public int size()
          {
            return size;
          }

          public Iterator<K> iterator()
          {
            return new KeyIterator();
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
        keys = new Collections.SynchronizedSet<K>(this, r);
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
  public Collection<V> values()
  {
    if (values == null)
      {
        // We don't bother overriding many of the optional methods, as doing so
        // wouldn't provide any significant performance advantage.
        Collection<V> r = new AbstractCollection<V>()
        {
          public int size()
          {
            return size;
          }

          public Iterator<V> iterator()
          {
            return new ValueIterator();
          }

          public void clear()
          {
            Hashtable.this.clear();
          }
        };
        // We must specify the correct object to synchronize upon, hence the
        // use of a non-public API
        values = new Collections.SynchronizedCollection<V>(this, r);
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
  public Set<Map.Entry<K, V>> entrySet()
  {
    if (entries == null)
      {
        // Create an AbstractSet with custom implementations of those methods
        // that can be overridden easily and efficiently.
        Set<Map.Entry<K, V>> r = new AbstractSet<Map.Entry<K, V>>()
        {
          public int size()
          {
            return size;
          }

          public Iterator<Map.Entry<K, V>> iterator()
          {
            return new EntryIterator();
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
            HashEntry<K, V> e = getEntry(o);
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
        entries = new Collections.SynchronizedSet<Map.Entry<K, V>>(this, r);
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
    // no need to synchronize, entrySet().equals() does that.
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
    // unsynchronized EntryIterator instead.
    Iterator<Map.Entry<K, V>> itr = new EntryIterator();
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
  HashEntry<K, V> getEntry(Object o)
  {
    if (! (o instanceof Map.Entry))
      return null;
    K key = ((Map.Entry<K, V>) o).getKey();
    if (key == null)
      return null;

    int idx = hash(key);
    HashEntry<K, V> e = buckets[idx];
    while (e != null)
      {
        if (e.equals(o))
          return e;
        e = e.next;
      }
    return null;
  }

  /**
   * A simplified, more efficient internal implementation of putAll(). clone() 
   * should not call putAll or put, in order to be compatible with the JDK 
   * implementation with respect to subclasses.
   *
   * @param m the map to initialize this from
   */
  void putAllInternal(Map<? extends K, ? extends V> m)
  {
    final Map<K,V> addMap = (Map<K,V>) m;
    final Iterator<Map.Entry<K,V>> it = addMap.entrySet().iterator();
    size = 0;
    while (it.hasNext())
      {
	final Map.Entry<K,V> e = it.next();
        size++;
	K key = e.getKey();
	int idx = hash(key);
	HashEntry<K, V> he = new HashEntry<K, V>(key, e.getValue());
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
    HashEntry<K, V>[] oldBuckets = buckets;

    int newcapacity = (buckets.length * 2) + 1;
    threshold = (int) (newcapacity * loadFactor);
    buckets = (HashEntry<K, V>[]) new HashEntry[newcapacity];

    for (int i = oldBuckets.length - 1; i >= 0; i--)
      {
        HashEntry<K, V> e = oldBuckets[i];
        while (e != null)
          {
            int idx = hash(e.key);
            HashEntry<K, V> dest = buckets[idx];

            if (dest != null)
              {
                HashEntry next = dest.next;
                while (next != null)
                  {
                    dest = next;
                    next = dest.next;
                  }
                dest.next = e;
              }
            else
              {
                buckets[idx] = e;
              }

            HashEntry<K, V> next = e.next;
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
    // unsynchronized EntryIterator instead.
    Iterator<Map.Entry<K, V>> it = new EntryIterator();
    while (it.hasNext())
      {
        HashEntry<K, V> entry = (HashEntry<K, V>) it.next();
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
    buckets = (HashEntry<K, V>[]) new HashEntry[s.readInt()];
    int len = s.readInt();

    // Read and use key/value pairs.
    // TODO: should we be defensive programmers, and check for illegal nulls?
    while (--len >= 0)
      put((K) s.readObject(), (V) s.readObject());
  }

  /**
   * A class which implements the Iterator interface and is used for
   * iterating over Hashtables.
   * This implementation iterates entries. Subclasses are used to
   * iterate key and values. It also allows the removal of elements,
   * as per the Javasoft spec.  Note that it is not synchronized; this
   * is a performance enhancer since it is never exposed externally
   * and is only used within synchronized blocks above.
   *
   * @author Jon Zeppieri
   * @author Fridjof Siebert
   */
  private class EntryIterator 
      implements Iterator<Entry<K,V>>
  {
    /**
     * The number of modifications to the backing Hashtable that we know about.
     */
    int knownMod = modCount;
    /** The number of elements remaining to be returned by next(). */
    int count = size;
    /** Current index in the physical hash table. */
    int idx = buckets.length;
    /** The last Entry returned by a next() call. */
    HashEntry<K, V> last;
    /**
     * The next entry that should be returned by next(). It is set to something
     * if we're iterating through a bucket that contains multiple linked
     * entries. It is null if next() needs to find a new bucket.
     */
    HashEntry<K, V> next;

    /**
     * Construct a new EntryIterator
     */
    EntryIterator()
    {
    }


    /**
     * Returns true if the Iterator has more elements.
     * @return true if there are more elements
     */
    public boolean hasNext()
    {
      return count > 0;
    }

    /**
     * Returns the next element in the Iterator's sequential view.
     * @return the next element
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws NoSuchElementException if there is none
     */
    public Map.Entry<K,V> next()
    {
      if (knownMod != modCount)
        throw new ConcurrentModificationException();
      if (count == 0)
        throw new NoSuchElementException();
      count--;
      HashEntry<K, V> e = next;

      while (e == null)
	if (idx <= 0)
	  return null;
	else
	  e = buckets[--idx];

      next = e.next;
      last = e;
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
  } // class EntryIterator

  /**
   * A class which implements the Iterator interface and is used for
   * iterating over keys in Hashtables.  This class uses an
   * <code>EntryIterator</code> to obtain the keys of each entry.
   *
   * @author Fridtjof Siebert
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private class KeyIterator 
      implements Iterator<K>
  {

    /**
     * This entry iterator is used for most operations.  Only
     * <code>next()</code> gives a different result, by returning just
     * the key rather than the whole element.
     */
    private final EntryIterator iterator;

    /**
     * Construct a new KeyIterator
     */
    KeyIterator()
    {
	iterator = new EntryIterator();
    }


    /**
     * Returns true if the entry iterator has more elements.
     *
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the hashtable was modified
     */
    public boolean hasNext()
    {
	return iterator.hasNext();
    }

    /**
     * Returns the next element in the Iterator's sequential view.
     *
     * @return the next element
     *
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws NoSuchElementException if there is none
     */
    public K next()
    {
      return ((HashEntry<K,V>) iterator.next()).key;
    }

    /**
     * Removes the last element used by the <code>next()</code> method
     * using the entry iterator.
     *
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws IllegalStateException if called when there is no last element
     */
    public void remove()
    {
      iterator.remove();
    }
  } // class KeyIterator
 
  /**
   * A class which implements the Iterator interface and is used for
   * iterating over values in Hashtables.  This class uses an
   * <code>EntryIterator</code> to obtain the values of each entry.
   *
   * @author Fridtjof Siebert
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private class ValueIterator
      implements Iterator<V>
  {

    /**
     * This entry iterator is used for most operations.  Only
     * <code>next()</code> gives a different result, by returning just
     * the value rather than the whole element.
     */
    private final EntryIterator iterator;

    /**
     * Construct a new KeyIterator
     */
    ValueIterator()
    {
	iterator = new EntryIterator();
    }


    /**
     * Returns true if the entry iterator has more elements.
     *
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the hashtable was modified
     */
    public boolean hasNext()
    {
	return iterator.hasNext();
    }

    /**
     * Returns the value of the next element in the iterator's sequential view.
     *
     * @return the next value
     *
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws NoSuchElementException if there is none
     */
    public V next()
    {
      return ((HashEntry<K,V>) iterator.next()).value;
    }

    /**
     * Removes the last element used by the <code>next()</code> method
     * using the entry iterator.
     *
     * @throws ConcurrentModificationException if the hashtable was modified
     * @throws IllegalStateException if called when there is no last element
     */
    public void remove()
    {
      iterator.remove();
    }

  } // class ValueIterator

  /**
   * Enumeration view of the entries in this Hashtable, providing
   * sequential access to its elements.
   *
   * <b>NOTE</b>: Enumeration is not safe if new elements are put in the table
   * as this could cause a rehash and we'd completely lose our place.  Even
   * without a rehash, it is undetermined if a new element added would
   * appear in the enumeration.  The spec says nothing about this, but
   * the "Java Class Libraries" book implies that modifications to the
   * hashtable during enumeration causes indeterminate results.  Don't do it!
   *
   * @author Jon Zeppieri
   * @author Fridjof Siebert
   */
  private class EntryEnumerator 
      implements Enumeration<Entry<K,V>>
  {
    /** The number of elements remaining to be returned by next(). */
    int count = size;
    /** Current index in the physical hash table. */
    int idx = buckets.length;
    /**
     * Entry which will be returned by the next nextElement() call. It is
     * set if we are iterating through a bucket with multiple entries, or null
     * if we must look in the next bucket.
     */
    HashEntry<K, V> next;

    /**
     * Construct the enumeration.
     */
    EntryEnumerator()
    {
      // Nothing to do here.
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
    public Map.Entry<K,V> nextElement()
    {
      if (count == 0)
        throw new NoSuchElementException("Hashtable Enumerator");
      count--;
      HashEntry<K, V> e = next;

      while (e == null)
        if (idx <= 0)
          return null;
        else
          e = buckets[--idx];

      next = e.next;
      return e;
    }
  } // class EntryEnumerator


  /**
   * Enumeration view of this Hashtable, providing sequential access to its
   * elements.
   *
   * <b>NOTE</b>: Enumeration is not safe if new elements are put in the table
   * as this could cause a rehash and we'd completely lose our place.  Even
   * without a rehash, it is undetermined if a new element added would
   * appear in the enumeration.  The spec says nothing about this, but
   * the "Java Class Libraries" book implies that modifications to the
   * hashtable during enumeration causes indeterminate results.  Don't do it!
   *
   * @author Jon Zeppieri
   * @author Fridjof Siebert
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private final class KeyEnumerator
      implements Enumeration<K>
  {
    /**
     * This entry enumerator is used for most operations.  Only
     * <code>nextElement()</code> gives a different result, by returning just
     * the key rather than the whole element.
     */
    private final EntryEnumerator enumerator;

    /**
     * Construct a new KeyEnumerator
     */
    KeyEnumerator()
    {
      enumerator = new EntryEnumerator();
    }


    /**
     * Returns true if the entry enumerator has more elements.
     *
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the hashtable was modified
     */
    public boolean hasMoreElements()
    {
	return enumerator.hasMoreElements();
    }

    /**
     * Returns the next element.
     * @return the next element
     * @throws NoSuchElementException if there is none.
     */
    public K nextElement()
    {
      HashEntry<K,V> entry = (HashEntry<K,V>) enumerator.nextElement();
      K retVal = null;
      if (entry != null)
        retVal = entry.key;
      return retVal;
    }
  } // class KeyEnumerator


  /**
   * Enumeration view of this Hashtable, providing sequential access to its
   * values.
   *
   * <b>NOTE</b>: Enumeration is not safe if new elements are put in the table
   * as this could cause a rehash and we'd completely lose our place.  Even
   * without a rehash, it is undetermined if a new element added would
   * appear in the enumeration.  The spec says nothing about this, but
   * the "Java Class Libraries" book implies that modifications to the
   * hashtable during enumeration causes indeterminate results.  Don't do it!
   *
   * @author Jon Zeppieri
   * @author Fridjof Siebert
   * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
   */
  private final class ValueEnumerator
      implements Enumeration<V>
  {
    /**
     * This entry enumerator is used for most operations.  Only
     * <code>nextElement()</code> gives a different result, by returning just
     * the value rather than the whole element.
     */
    private final EntryEnumerator enumerator;

    /**
     * Construct a new ValueEnumerator
     */
    ValueEnumerator()
    {
      enumerator = new EntryEnumerator();
    }


    /**
     * Returns true if the entry enumerator has more elements.
     *
     * @return true if there are more elements
     * @throws ConcurrentModificationException if the hashtable was modified
     */
    public boolean hasMoreElements()
    {
	return enumerator.hasMoreElements();
    }

    /**
     * Returns the next element.
     * @return the next element
     * @throws NoSuchElementException if there is none.
     */
    public V nextElement()
    {
      HashEntry<K,V> entry = (HashEntry<K,V>) enumerator.nextElement();
      V retVal = null;
      if (entry != null)
        retVal = entry.value;
      return retVal;
    }
  } // class ValueEnumerator

} // class Hashtable
