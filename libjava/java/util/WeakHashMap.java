/* java.util.WeakHashMap
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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
import java.lang.ref.WeakReference;
import java.lang.ref.ReferenceQueue;

/**
 * A weak hash map has only weak references to the key.  This means
 * that it allows the key to be garbage collected if they are not used
 * otherwise.  If this happens, the weak hash map will eventually
 * remove the whole entry from this map. <br>
 *
 * A weak hash map makes most sense, if the keys doesn't override the
 * <code>equals</code>-method: If there is no other reference to the
 * key nobody can ever look up the key in this table and so the entry
 * can be removed.  This table also works, if the <code>equals</code>
 * method is overloaded, e.g. with Strings as keys, but you should be
 * prepared that some entries disappear spontaneously. <br>
 *
 * You should also be prepared that this hash map behaves very
 * strange: The size of this map may spontaneously shrink (even if you
 * use a synchronized map and synchronize it); it behaves as if
 * another thread removes entries from this table without
 * synchronizations.  The entry set returned by <code>entrySet</code>
 * has similar phenomenons: The size may spontaneously shrink, or an
 * entry, that was in the set before, suddenly disappears. <br>
 *
 * A weak hash map is not meant for caches; use a normal map, with 
 * soft references as values instead.  <br>
 *
 * The weak hash map supports null values and null keys.  Null keys
 * are never deleted from the map (except explictly of course). 
 * The performance of the methods are similar to that of a hash map. <br>
 *
 * The value object are strongly referenced by this table.  So if a
 * value object maintains a strong reference to the key (either direct
 * or indirect) the key will never be removed from this map.  According
 * to Sun, this problem may be fixed in a future release.  It is not 
 * possible to do it with the jdk 1.2 reference model, though.
 *
 * @since jdk1.2
 * @author Jochen Hoenicke 
 * @see HashMap
 * @see WeakReference */
public class WeakHashMap extends AbstractMap implements Map
{
  /** 
   * The default capacity for an instance of HashMap.  
   * Sun's documentation mildly suggests that this (11) is the correct
   * value.  
   */
  private static final int DEFAULT_CAPACITY = 11;

  /** 
   * The default load factor of a HashMap 
   */
  private static final float DEFAULT_LOAD_FACTOR = 0.75F;

  /**
   * This is used instead of the key value <i>null</i>.  It is needed
   * to distinguish between an null key and a removed key.  
   */
  private static final Object NULL_KEY = new Object();

  /**
   * The reference queue where our buckets (which are WeakReferences) are
   * registered to.
   */
  private ReferenceQueue queue;

  /**
   * The number of entries in this hash map.
   */
  private int size;

  /**
   * The load factor of this WeakHashMap.  This is the maximum ratio of
   * size versus number of buckets.  If size grows the number of buckets
   * must grow, too.
   */
  private float loadFactor;

  /**
   * The rounded product of the capacity (i.e. number of buckets) and
   * the load factor. When the number of elements exceeds the
   * threshold, the HashMap calls <pre>rehash()</pre>.  
   */
  private int threshold;

  /**
   * The number of structural modifications.  This is used by
   * iterators, to see if they should fail.  This doesn't count
   * the silent key removals, when a weak reference is cleared
   * by the garbage collection.  Instead the iterators must make
   * sure to have strong references to the entries they rely on.
   */
  private int modCount;

  /** 
   * The entry set.  There is only one instance per hashmap, namely
   * theEntrySet.  Note that the entry set may silently shrink, just
   * like the WeakHashMap.
   */
  private class WeakEntrySet extends AbstractSet
  {
    /**
     * Returns the size of this set. 
     */
    public int size()
    {
      return size;
    }

    /**
     * Returns an iterator for all entries.
     */
    public Iterator iterator()
    {
      return new Iterator()
      {
	/** 
	 * The entry that was returned by the last
	 * <code>next()</code> call.  This is also the entry whose
	 * bucket should be removed by the <code>remove</code> call. <br>
	 *
	 * It is null, if the <code>next</code> method wasn't 
	 * called yet, or if the entry was already removed.  <br>
	 *
	 * Remembering this entry here will also prevent it from
	 * being removed under us, since the entry strongly refers
	 * to the key.
	 */
	WeakBucket.Entry lastEntry;

	/** 
	 * The entry that will be returned by the next
	 * <code>next()</code> call.  It is <code>null</code> if there
	 * is no further entry. <br>
	 *
	 * Remembering this entry here will also prevent it from
	 * being removed under us, since the entry strongly refers
	 * to the key.
	 */
	WeakBucket.Entry nextEntry = findNext(null);

	/**
	 * The known number of modification to the list, if it differs
	 * from the real number, we through an exception.
	 */
	int knownMod = modCount;

	/** 
	 * Check the known number of modification to the number of
	 * modifications of the table.  If it differs from the real
	 * number, we throw an exception.
	 * @exception ConcurrentModificationException if the number
	 * of modifications doesn't match.
	 */
	private void checkMod()
	{
	  /* This method will get inlined */
	  if (knownMod != modCount)
	    throw new ConcurrentModificationException();
	}

	/**
	 * Get a strong reference to the next entry after
	 * lastBucket.
	 * @param lastBucket the previous bucket, or null if we should
	 * get the first entry.
	 * @return the next entry.
	 */
	private WeakBucket.Entry findNext(WeakBucket.Entry lastEntry)
	{
	  int slot;
	  WeakBucket nextBucket;
	  if (lastEntry != null)
	    {
	      nextBucket = lastEntry.getBucket().next;
	      slot = lastEntry.getBucket().slot;
	    }
	  else
	    {
	      nextBucket = buckets[0];
	      slot = 0;
	    }

	  while (true)
	    {
	      while (nextBucket != null)
		{
		  WeakBucket.Entry entry = nextBucket.getEntry();
		  if (entry != null)
		    /* This is the next entry */
		    return entry;

		  /* entry was cleared, try next */
		  nextBucket = nextBucket.next;
		}

	      slot++;
	      if (slot == buckets.length)
		/* No more buckets, we are through */
		return null;

	      nextBucket = buckets[slot];
	    }
	}


	/**
	 * Checks if there are more entries.
	 * @return true, iff there are more elements.
	 * @exception IllegalModificationException if the hash map was
	 * modified.
	 */
	public boolean hasNext()
	{
	  cleanQueue();
	  checkMod();
	  return (nextEntry != null);
	}

	/**
	 * Returns the next entry.
	 * @return the next entry.
	 * @exception IllegalModificationException if the hash map was
	 * modified.
	 * @exception NoSuchElementException if there is no entry.
	 */
	public Object next()
	{
	  cleanQueue();
	  checkMod();
	  if (nextEntry == null)
	    throw new NoSuchElementException();
	  lastEntry = nextEntry;
	  nextEntry = findNext(lastEntry);
	  return lastEntry;
	}

	/**
	 * Removes the last returned entry from this set.  This will
	 * also remove the bucket of the underlying weak hash map.
	 * @exception IllegalModificationException if the hash map was
	 * modified.
	 * @exception IllegalStateException if <code>next()</code> was
	 * never called or the element was already removed. 
	 */
	public void remove()
	{
	  cleanQueue();
	  checkMod();
	  if (lastEntry == null)
	    throw new IllegalStateException();
	  internalRemove(lastEntry.getBucket());
	  lastEntry = null;
	  modCount++;
	  knownMod = modCount;
	}
      };
    }
  }

  /**
   * A bucket is a weak reference to the key, that contains a strong
   * reference to the value, a pointer to the next bucket and its slot
   * number. <br>
   *
   * It would be cleaner to have a WeakReference as field, instead of
   * extending it, but if a weak reference get cleared, we only get
   * the weak reference (by queue.poll) and wouldn't know where to
   * look for this reference in the hashtable, to remove that entry.
   *
   * @author Jochen Hoenicke 
   */
  private static class WeakBucket extends WeakReference
  {
    /**
     * The value of this entry.  The key is stored in the weak
     * reference that we extend.  
     */
    Object value;

    /**
     * The next bucket describing another entry that uses the same
     * slot.  
     */
    WeakBucket next;

    /**
     * The slot of this entry. This should be 
     * <pre>
     *  Math.abs(key.hashCode() % buckets.length)
     * </pre>
     * But since the key may be silently removed we have to remember
     * the slot number.
     * If this bucket was removed the slot is -1.  This marker will
     * prevent the bucket from being removed twice.
     */
    int slot;

    /**
     * Creates a new bucket for the given key/value pair and the specified
     * slot.
     * @param key the key
     * @param value the value
     * @param slot the slot.  This must match the slot where this bucket
     * will be enqueued.
     */
    public WeakBucket(Object key, ReferenceQueue queue, Object value,
		      int slot)
    {
      super(key, queue);
      this.value = value;
      this.slot = slot;
    }

    /**
     * This class gives the <code>Entry</code> representation of the 
     * current bucket.  It also keeps a strong reference to the
     * key; bad things may happen otherwise.
     */
    class Entry implements Map.Entry
    {
      /**
       * The strong ref to the key.
       */
      Object key;

      /**
       * Creates a new entry for the key.
       */
      public Entry(Object key)
      {
	this.key = key;
      }

      /**
       * Returns the underlying bucket.
       */
      public WeakBucket getBucket()
      {
	return WeakBucket.this;
      }

      /**
       * Returns the key.
       */
      public Object getKey()
      {
	return key == NULL_KEY ? null : key;
      }

      /**
       * Returns the value.
       */
      public Object getValue()
      {
	return value;
      }

      /**
       * This changes the value.  This change takes place in 
       * the underlying hash map.
       */
      public Object setValue(Object newVal)
      {
	Object oldVal = value;
	value = newVal;
	return oldVal;
      }

      /**
       * The hashCode as specified in the Entry interface.
       */
      public int hashCode()
      {
	return (key == NULL_KEY ? 0 : key.hashCode())
	  ^ (value == null ? 0 : value.hashCode());
      }

      /**
       * The equals method as specified in the Entry interface.
       */
      public boolean equals(Object o)
      {
	if (o instanceof Map.Entry)
	  {
	    Map.Entry e = (Map.Entry) o;
	    return (key == NULL_KEY
		    ? e.getKey() == null : key.equals(e.getKey()))
	      && (value == null
		  ? e.getValue() == null : value.equals(e.getValue()));
	  }
	return false;
      }
    }

    /**
     * This returns the entry stored in this bucket, or null, if the
     * bucket got cleared in the mean time.
     */
    Entry getEntry()
    {
      final Object key = this.get();
      if (key == null)
	return null;
      return new Entry(key);
    }
  }

  /**
   * The entry set returned by <code>entrySet()</code>.
   */
  private WeakEntrySet theEntrySet;

  /**
   * The hash buckets.  This are linked lists.
   */
  private WeakBucket[] buckets;

  /**
   * Creates a new weak hash map with default load factor and default
   * capacity.
   */
  public WeakHashMap()
  {
    this(DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR);
  }

  /**
   * Creates a new weak hash map with default load factor and the given
   * capacity.
   * @param initialCapacity the initial capacity 
   */
  public WeakHashMap(int initialCapacity)
  {
    this(initialCapacity, DEFAULT_LOAD_FACTOR);
  }

  /**
   * Creates a new weak hash map with the given initial capacity and
   * load factor.  
   * @param initialCapacity the initial capacity.
   * @param loadFactor the load factor (see class description of HashMap).
   */
  public WeakHashMap(int initialCapacity, float loadFactor)
  {
    if (initialCapacity < 0 || loadFactor <= 0 || loadFactor > 1)
      throw new IllegalArgumentException();
    this.loadFactor = loadFactor;
    threshold = (int) (initialCapacity * loadFactor);
    theEntrySet = new WeakEntrySet();
    queue = new ReferenceQueue();
    buckets = new WeakBucket[initialCapacity];
  }

  /** 
   * simply hashes a non-null Object to its array index 
   */
  private int hash(Object key)
  {
    return Math.abs(key.hashCode() % buckets.length);
  }

  /**
   * Cleans the reference queue.  This will poll all references (which
   * are WeakBuckets) from the queue and remove them from this map.
   * This will not change modCount, even if it modifies the map.  The
   * iterators have to make sure that nothing bad happens.  <br>
   *
   * Currently the iterator maintains a strong reference to the key, so
   * that is no problem.
   */
  private void cleanQueue()
  {
    Object bucket = queue.poll();
    while (bucket != null)
      {
	internalRemove((WeakBucket) bucket);
	bucket = queue.poll();
      }
  }

  /**
   * Rehashes this hashtable.  This will be called by the
   * <code>add()</code> method if the size grows beyond the threshold.  
   * It will grow the bucket size at least by factor two and allocates
   * new buckets.
   */
  private void rehash()
  {
    WeakBucket[] oldBuckets = buckets;
    int newsize = buckets.length * 2 + 1;	// XXX should be prime.
    threshold = (int) (newsize * loadFactor);
    buckets = new WeakBucket[newsize];

    /* Now we have to insert the buckets again.
     */
    for (int i = 0; i < oldBuckets.length; i++)
      {
	WeakBucket bucket = oldBuckets[i];
	WeakBucket nextBucket;
	while (bucket != null)
	  {
	    nextBucket = bucket.next;

	    Object key = bucket.get();
	    if (key == null)
	      {
		/* This bucket should be removed; it is probably
		 * already on the reference queue.  We don't insert it
		 * at all, and mark it as cleared.  */
		bucket.slot = -1;
		size--;
	      }
	    else
	      {
		/* add this bucket to its new slot */
		int slot = hash(key);
		bucket.slot = slot;
		bucket.next = buckets[slot];
		buckets[slot] = bucket;
	      }
	    bucket = nextBucket;
	  }
      }
  }

  /**
   * Finds the entry corresponding to key.  Since it returns an Entry
   * it will also prevent the key from being removed under us.
   * @param key the key. It may be null.
   * @return The WeakBucket.Entry or null, if the key wasn't found.
   */
  private WeakBucket.Entry internalGet(Object key)
  {
    if (key == null)
      key = NULL_KEY;
    int slot = hash(key);
    WeakBucket bucket = buckets[slot];
    while (bucket != null)
      {
	WeakBucket.Entry entry = bucket.getEntry();
	if (entry != null && key.equals(entry.key))
	  return entry;

	bucket = bucket.next;
      }
    return null;
  }

  /**
   * Adds a new key/value pair to the hash map.
   * @param key the key. This mustn't exists in the map. It may be null.
   * @param value the value.
   */
  private void internalAdd(Object key, Object value)
  {
    if (key == null)
      key = NULL_KEY;
    int slot = hash(key);
    WeakBucket bucket = new WeakBucket(key, queue, value, slot);
    bucket.next = buckets[slot];
    buckets[slot] = bucket;
    size++;
  }

  /**
   * Removes a bucket from this hash map, if it wasn't removed before
   * (e.g. one time through rehashing and one time through reference queue)
   * @param bucket the bucket to remove.  
   */
  private void internalRemove(WeakBucket bucket)
  {
    int slot = bucket.slot;
    if (slot == -1)
      /* this bucket was already removed. */
      return;

    /* mark the bucket as removed.  This is necessary, since the
     * bucket may be enqueued later by the garbage collection and
     * internalRemove, will be called a second time.  
     */
    bucket.slot = -1;
    if (buckets[slot] == bucket)
      buckets[slot] = bucket.next;
    else
      {
	WeakBucket prev = buckets[slot];
	/* This may throw a NullPointerException.  It shouldn't but if
	 * a race condition occured (two threads removing the same
	 * bucket at the same time) it may happen.  <br>
	 * But with race condition many much worse things may happen
	 * anyway.
	 */
	while (prev.next != bucket)
	  prev = prev.next;
	prev.next = bucket.next;
      }
    size--;
  }

  /**
   * Returns the size of this hash map.  Note that the size() may shrink
   * spontanously, if the some of the keys were only weakly reachable.
   * @return the number of entries in this hash map.
   */
  public int size()
  {
    cleanQueue();
    return size;
  }

  /**
   * Tells if the map is empty.  Note that the result may change
   * spontanously, if all of the keys were only weakly reachable.
   * @return true, iff the map is empty.
   */
  public boolean isEmpty()
  {
    cleanQueue();
    return size == 0;
  }

  /**
   * Tells if the map contains the given key.  Note that the result
   * may change spontanously, if all the key was only weakly
   * reachable.  
   * @return true, iff the map contains an entry for the given key. 
   */
  public boolean containsKey(Object key)
  {
    cleanQueue();
    return internalGet(key) != null;
  }

  /**
   * Gets the value the key will be mapped to.
   * @return the value the key was mapped to.  It returns null if
   * the key wasn't in this map, or if the mapped value was explicitly
   * set to null.  
   */
  public Object get(Object key)
  {
    cleanQueue();
    WeakBucket.Entry entry = internalGet(key);
    return entry == null ? null : entry.getValue();
  }

  /**
   * Adds a new key/value mapping to this map.
   * @param key the key. This may be null.
   * @param value the value. This may be null.
   * @return the value the key was mapped to previously.  It returns
   * null if the key wasn't in this map, or if the mapped value was
   * explicitly set to null.  
   */
  public Object put(Object key, Object value)
  {
    cleanQueue();
    WeakBucket.Entry entry = internalGet(key);
    if (entry != null)
      return entry.setValue(value);

    if (size >= threshold)
      rehash();

    internalAdd(key, value);
    modCount++;
    return null;
  }

  /**
   * Removes the key and the corresponding value from this map.
   * @param key the key. This may be null.
   * @return the value the key was mapped to previously.  It returns
   * null if the key wasn't in this map, or if the mapped value was
   * explicitly set to null.  */
  public Object remove(Object key)
  {
    cleanQueue();
    WeakBucket.Entry entry = internalGet(key);
    if (entry == null)
      {
	return null;
      }
    internalRemove(entry.getBucket());
    modCount++;
    return entry.getValue();
  }

  /**
   * Returns a set representation of the entries in this map.  This
   * set will not have strong references to the keys, so they can be
   * silently removed.  The returned set has therefore the same
   * strange behaviour (shrinking size(), disappearing entries) as
   * this weak hash map.  
   * @return a set representation of the entries.  */
  public Set entrySet()
  {
    cleanQueue();
    return theEntrySet;
  }
}
