/* LinkedHashMap.java -- a class providing hashtable data structure,
   mapping Object --> Object, with linked list traversal
   Copyright (C) 2001 Free Software Foundation, Inc.

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
 * This class provides a hashtable-backed implementation of the
 * Map interface, with predictable traversal order.
 * <p>
 *
 * It uses a hash-bucket approach; that is, hash collisions are handled
 * by linking the new node off of the pre-existing node (or list of
 * nodes).  In this manner, techniques such as linear probing (which
 * can cause primary clustering) and rehashing (which does not fit very
 * well with Java's method of precomputing hash codes) are avoided.  In
 * addition, this maintains a doubly-linked list which tracks either
 * insertion or access order.  Note that the insertion order is not
 * modified if a <code>put</code> simply reinserts a key in the map.
 * <p>
 *
 * One of the nice features of tracking insertion order is that you can
 * copy a hashtable, and regardless of the implementation of the original,
 * produce the same results when iterating over the copy.  This is possible
 * without needing the overhead of <code>TreeMap</code>.
 * <p>
 *
 * When using this {@link #LinkedHashMap(int, float, boolean) constructor},
 * you build an access-order mapping.  This can be used to implement LRU
 * caches, for example.  In this case, every invocation of <code>put</code>,
 * <code>putAll</code>, or <code>get</code> moves the accessed entry to
 * the end of the iteration list.  By overriding
 * {@link #removeEldestEntry(Map.Entry)}, you can also control the
 * removal of the oldest entry, and thereby do things like keep the map
 * at a fixed size.
 * <p>
 *
 * Under ideal circumstances (no collisions), LinkedHashMap offers O(1) 
 * performance on most operations (<pre>containsValue()</pre> is,
 * of course, O(n)).  In the worst case (all keys map to the same 
 * hash code -- very unlikely), most operations are O(n).
 * <p>
 *
 * LinkedHashMap accepts the null key and null values.  It is not
 * synchronized, so if you need multi-threaded access, consider using:<br>
 * <code>Map m = Collections.synchronizedMap(new LinkedHashMap(...));</code>
 * <p>
 *
 * The iterators are <i>fail-fast</i>, meaning that any structural
 * modification, except for <code>remove()</code> called on the iterator
 * itself, cause the iterator to throw a
 * {@link ConcurrentModificationException} rather than exhibit
 * non-deterministic behavior.
 *
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Object#hashCode()
 * @see Collection
 * @see Map
 * @see HashMap
 * @see TreeMap
 * @see Hashtable
 * @since 1.4
 * @status updated to 1.4
 */
public class LinkedHashMap extends HashMap
{
  /**
   * Compatible with JDK 1.4.
   */
  private static final long serialVersionUID = 3801124242820219131L;

  /**
   * The first Entry to iterate over.
   */
  transient LinkedHashEntry head;

  /**
   * The last Entry to iterate over.
   */
  transient LinkedHashEntry tail;

  /**
   * The iteration order of this linked hash map: <code>true</code> for
   * access-order, <code>false</code> for insertion-order.
   * @serial
   */
  final boolean accessOrder;

  /**
   * Class to represent an entry in the hash table. Holds a single key-value
   * pair and the doubly-linked insertion order list.
   */
  class LinkedHashEntry extends HashEntry
  {
    /** The predecessor in the iteration list, null if this is the eldest. */
    LinkedHashEntry pred;
    /** The successor in the iteration list, null if this is the newest. */
    LinkedHashEntry succ;

    /**
     * Simple constructor.
     * @param key the key
     * @param value the value
     */
    LinkedHashEntry(Object key, Object value)
    {
      super(key, value);
      if (head == null)
        head = this;
      pred = tail;
      tail = this;
      if (pred != null)
        pred.succ = this;
    }

    /**
     * Sets the value of this entry, and shuffles it to the end of
     * the list if this is in access-order.
     * @param value the new value
     * @return the prior value
     */
    public Object setValue(Object value)
    {
      if (accessOrder && succ != null)
        {
          succ.pred = pred;
          if (pred == null)
            head = succ;
          else
            pred.succ = succ;
          succ = null;
          pred = tail;
          pred.succ = this;
          tail = this;
        }
      return super.setValue(value);
    }

    /**
     * Called when this entry is removed from the map. This version does
     * the necessary bookkeeping to keep the doubly-linked list in order.
     * @return the value of this key as it is removed
     */
    Object cleanup()
    {
      if (pred == null)
        head = succ;
      else
        pred.succ = succ;
      if (succ == null)
        tail = pred;
      else
        succ.pred = pred;
      
      return value;
    }
  }

  /**
   * Construct a new insertion-ordered LinkedHashMap with the default
   * capacity (11) and the default load factor (0.75).
   */
  public LinkedHashMap()
  {
    super();
    accessOrder = false;
  }

  /**
   * Construct a new insertion-ordered LinkedHashMap from the given Map,
   * with initial capacity the greater of the size of <code>m</code> or
   * the default of 11.
   * <p>
   *
   * Every element in Map m will be put into this new HashMap, in the
   * order of m's iterator.
   *
   * @param m a Map whose key / value pairs will be put into
   *          the new HashMap.  <b>NOTE: key / value pairs
   *          are not cloned in this constructor.</b>
   * @throws NullPointerException if m is null
   */
  public LinkedHashMap(Map m)
  {
    super(m);
    accessOrder = false;
  }

  /**
   * Construct a new insertion-ordered LinkedHashMap with a specific
   * inital capacity and default load factor of 0.75.
   *
   * @param initialCapacity the initial capacity of this HashMap (&gt;= 0)
   * @throws IllegalArgumentException if (initialCapacity &lt; 0)
   */
  public LinkedHashMap(int initialCapacity)
  {
    super(initialCapacity);
    accessOrder = false;
  }

  /**
   * Construct a new insertion-orderd LinkedHashMap with a specific
   * inital capacity and load factor.
   *
   * @param initialCapacity the initial capacity (&gt;= 0)
   * @param loadFactor the load factor (&gt; 0, not NaN)
   * @throws IllegalArgumentException if (initialCapacity &lt; 0) ||
   *                                     ! (loadFactor &gt; 0.0)
   */
  public LinkedHashMap(int initialCapacity, float loadFactor)
  {
    super(initialCapacity, loadFactor);
    accessOrder = false;
  }

  /**
   * Construct a new LinkedHashMap with a specific inital capacity, load
   * factor, and ordering mode.
   *
   * @param initialCapacity the initial capacity (>=0)
   * @param loadFactor the load factor (>0, not NaN)
   * @param accessOrder true for access-order, false for insertion-order
   * @throws IllegalArgumentException if (initialCapacity < 0) ||
   *                                     ! (loadFactor > 0.0)
   */
  public LinkedHashMap(int initialCapacity, float loadFactor,
                       boolean accessOrder)
  {
    super(initialCapacity, loadFactor);
    this.accessOrder = accessOrder;
  }

  /**
   * Clears the Map so it has no keys. This is O(1).
   */
  public void clear()
  {
    super.clear();
    head = null;
    tail = null;
  }

  /**
   * Returns true if this HashMap contains a value <pre>o</pre>, such that
   * <pre>o.equals(value)</pre>.
   *
   * @param value the value to search for in this HashMap
   * @return true if at least one key maps to the value
   */
  public boolean containsValue(Object value)
  {
    LinkedHashEntry e = head;
    while (e != null)
      {
        if (equals(value, e.value))
          return true;
        e = e.succ;
      }
    return false;
  }

  /**
   * Return the value in this Map associated with the supplied key,
   * or <pre>null</pre> if the key maps to nothing.  If this is an
   * access-ordered Map and the key is found, this performs structural
   * modification, moving the key to the newest end of the list. NOTE:
   * Since the value could also be null, you must use containsKey to
   * see if this key actually maps to something.
   *
   * @param key the key for which to fetch an associated value
   * @return what the key maps to, if present
   * @see #put(Object, Object)
   * @see #containsKey(Object)
   */
  public Object get(Object key)
  {
    int idx = hash(key);
    HashEntry e = buckets[idx];
    while (e != null)
      {
        if (equals(key, e.key))
          {
            if (accessOrder)
              {
                modCount++;
                LinkedHashEntry l = (LinkedHashEntry) e;
                if (l.succ != null)
                  {
                    l.succ.pred = l.pred;
                    if (l.pred == null)
                      head = l.succ;
                    else
                      l.pred.succ = l.succ;
                    l.succ = null;
                    l.pred = tail;
                    tail.succ = l;
                    tail = l;
                  }
              }
            return e.value;
          }
        e = e.next;
      }
    return null;
  }

  /**
   * Returns <code>true</code> if this map should remove the eldest entry.
   * This method is invoked by all calls to <code>put</code> and
   * <code>putAll</code> which place a new entry in the map, providing
   * the implementer an opportunity to remove the eldest entry any time
   * a new one is added.  This can be used to save memory usage of the
   * hashtable, as well as emulating a cache, by deleting stale entries.
   * <p>
   *
   * For example, to keep the Map limited to 100 entries, override as follows:
   * <pre>
   * private static final int MAX_ENTRIES = 100;
   *
   * protected boolean removeEldestEntry(Map.Entry eldest)
   * {
   *   return size() > MAX_ENTRIES;
   * }
   * </pre><p>
   *
   * Typically, this method does not modify the map, but just uses the
   * return value as an indication to <code>put</code> whether to proceed.
   * However, if you override it to modify the map, you must return false
   * (indicating that <code>put</code> should do nothing), or face
   * unspecified behavior.
   * <p>
   *
   * This method is called after the eldest entry has been inserted, so
   * if <code>put</code> was called on a previously empty map, the eldest
   * entry is the one you just put in! The default implementation just
   * returns <code>false</code>, so that this map always behaves like
   * a normal one with unbounded growth.
   *
   * @param eldest the eldest element which would be removed if this
   *        returns true. For an access-order map, this is the least
   *        recently accessed; for an insertion-order map, this is the
   *        earliest element inserted.
   * @return true if <code>eldest</code> should be removed
   */
  protected boolean removeEldestEntry(Map.Entry eldest)
  {
    return false;
  }

  /**
   * Helper method called by <code>put</code>, which creates and adds a
   * new Entry, followed by performing bookkeeping (like removeEldestEntry).
   *
   * @param key the key of the new Entry
   * @param value the value
   * @param idx the index in buckets where the new Entry belongs
   * @param callRemove whether to call the removeEldestEntry method
   * @see #put(Object, Object)
   * @see #removeEldestEntry(Map.Entry)
   */
  void addEntry(Object key, Object value, int idx, boolean callRemove)
  {
    LinkedHashEntry e = new LinkedHashEntry(key, value);

    e.next = buckets[idx];
    buckets[idx] = e;

    if (callRemove && removeEldestEntry(head))
      remove(head);
  }

  /**
   * Helper method, called by clone() to reset the doubly-linked list.
   * @param m the map to add entries from
   * @see #clone()
   */
  void putAllInternal(Map m)
  {
    head = null;
    tail = null;
    super.putAllInternal(m);
  }

  /**
   * Generates a parameterized iterator. This allows traversal to follow
   * the doubly-linked list instead of the random bin order of HashMap.
   * @param type {@link #KEYS}, {@link #VALUES}, or {@link #ENTRIES}
   * @return the appropriate iterator
   */
  Iterator iterator(final int type)
  {
    return new Iterator()
    {
      /** The current Entry */
      LinkedHashEntry current = head;

      /** The previous Entry returned by next() */
      LinkedHashEntry last;

      /** The number of known modifications to the backing HashMap */
      int knownMod = modCount;

      /**
       * Returns true if the Iterator has more elements.
       * @return true if there are more elements
       * @throws ConcurrentModificationException if the HashMap was modified
       */
      public boolean hasNext()
      {
        if (knownMod != modCount)
          throw new ConcurrentModificationException();
        return current != null;
      }

      /**
       * Returns the next element in the Iterator's sequential view.
       * @return the next element
       * @throws ConcurrentModificationException if the HashMap was modified
       * @throws NoSuchElementException if there is none
       */
      public Object next()
      {
        if (knownMod != modCount)
          throw new ConcurrentModificationException();
        if (current == null)
          throw new NoSuchElementException();
        last = current;
        current = current.succ;
        return type == VALUES ? last.value : type == KEYS ? last.key : last;
      }
      
      /**
       * Removes from the backing HashMap the last element which was fetched
       * with the <pre>next()</pre> method.
       * @throws ConcurrentModificationException if the HashMap was modified
       * @throws IllegalStateException if called when there is no last element
       */
      public void remove()
      {
        if (knownMod != modCount)
          throw new ConcurrentModificationException();
        if (last == null)
          throw new IllegalStateException();

        LinkedHashMap.this.remove(last.key);
        last = null;
        knownMod++;
      }
    };
  }
}
