/* ThreadLocal -- a variable with a unique value per thread
   Copyright (C) 2000, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc.

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

package java.lang;

import java.lang.ref.WeakReference;

/**
 * ThreadLocalMap is the basic storage for the map of ThreadLocal instance
 * to a thread's current value.
 *
 * Some applications really work out ThreadLocals, leading to this
 * optimized implementation.
 */
final class ThreadLocalMap
{
  /**
   * The log (base 2) of the initial size of the map
   */
  private static final int LOG_INITIAL_SIZE = 3;

  /**
   * The maximum occupancy rate (after which we grow)
   */
  private static final float MAX_OCCUPANCY = 0.7f;

  /**
   * The target occupancy rate.
   */
  private static final float TARGET_OCCUPANCY = 0.5f;

  /**
   * The deleted entry sentinel value.
   */
  private static final Entry deletedEntry = new Entry(null);

  /**
   * Constructor
   */
  ThreadLocalMap()
  {
    /* Dummy value to ensure fast path can be optimized */
    entries = new Entry[1];
    hashMask = 0;
    count = 0;
  }

  /**
   * The map entries
   */
  private Entry[] entries;

  /**
   * Used for start index computation
   */
  private int hashMask;

  /**
   * The number of entries currently in the map
   */
  private int count;

  /**
   * Create or grow the table to the specified size. The size must be a
   * power of two for the efficient mask/hash computation.
   *
   * @param newSize The new table size.
   */
  private void newEntryArray(int newSize)
  {
    int mask = newSize - 1;
    Entry[] oldEntries = this.entries;
    this.entries = new Entry[newSize];
    this.hashMask = mask;

    /* Copy old entries to new table */
    count = 0;
    if (oldEntries != null)
      {
        for(Entry e: oldEntries)
          {
            if (e != null)
              {
                ThreadLocal<?> key = e.get();
                if (e != deletedEntry && key != null)
                  {
                    for(int i = key.fastHash & mask;; i = (i + 1) & mask)
                      {
                        if (entries[i] == null)
                          {
                            entries[i] = e;
                            count++;
                            break;
                          }
                      }
                  }
              }
          }
      }
  }

  /**
   * We have run out of space in our locals. We use this as the
   * trigger to attempt to find unused slots as ThreadLocals have
   * died. If we recover any slots this way then we do not grow.
   */
  private void overflow()
  {
    /* First 'actual' use */
    if (entries.length == 1)
      {
        newEntryArray(1 << LOG_INITIAL_SIZE);
        return;
      }

    /* Attempt to recover unused slots */
    int deleted = 0;
    for(int i=0; i < entries.length; i++)
      {
        Entry e = entries[i];
        if (e != null)
          {
            if (e == deletedEntry)
              {
                deleted++;
              }
            else if (e.get() == null)
              {
                entries[i] = deletedEntry;
                deleted++;
              }
          }
      }

    if ((count-deleted) <= (TARGET_OCCUPANCY * entries.length))
      {
        /* We currently rehash by simple reallocating into a same-sized table.
         * An alternative would be to implement a clever hashing algorithm but
         * as this happens infrequently this seems preferred */
        newEntryArray(entries.length);
        return;
      }

    /* Double the size */
    newEntryArray(entries.length << 1);
  }

  /**
   * This is the class that is used to refer to a thread local weakly.
   *
   * As we want to minimize indirections we extend WeakReference.
   */
  static final class Entry extends WeakReference<ThreadLocal<?>> {
    /**
     * The value stored in this slot
     */
    Object value;

    /**
     * Constructor
     */
    Entry(ThreadLocal<?> threadLocal) {
      super(threadLocal);
    }
  }

  /**
   * Gets the value associated with the ThreadLocal object for the currently
   * executing Thread. If this is the first time the current thread has called
   * get(), and it has not already called set(), the sentinel value is returned.
   *
   * @return the value of the variable in this thread, or sentinel if not present.
   */
  public Object get(ThreadLocal<?> key)
  {
    int mask = this.hashMask;
    for(int i = key.fastHash & mask;; i = (i + 1) & mask) {
      Entry e = entries[i];
      if (e != null) {
        if (e.get() == key) {
          return e.value;
        }
      } else {
        return ThreadLocal.sentinel;
      }
    }
  }

  /**
   * Sets the value associated with the ThreadLocal object for the currently
   * executing Thread. This overrides any existing value associated with the
   * current Thread and prevents <code>initialValue()</code> from being
   * called if this is the first access to this ThreadLocal in this Thread.
   *
   * @param value the value to set this thread's view of the variable to
   */
  public void set(ThreadLocal<?> key, Object value)
  {
    /* Overflow ? */
    if ((count+1) >= (MAX_OCCUPANCY * entries.length))
      {
        overflow();
      }

    /* Set the entry */
    int mask = this.hashMask;
    for(int i = key.fastHash & mask;; i = (i + 1) & mask)
      {
        Entry e = entries[i];
        if (e == null || e == deletedEntry)
          {
            /* Create entry */
            if (e == null) count++;
            entries[i] = e = new Entry(key);
            e.value = value;
            return;
          }
        else
          {
            ThreadLocal<?> entryKey = e.get();
            if (entryKey == null)
              {
              entries[i] = deletedEntry;
              }
            else if (entryKey == key)
              {
                /* Update entry */
                e.value = value;
                return;
              }
          }
      }
  }

  /**
   * Removes the value associated with the ThreadLocal object for the
   * currently executing Thread.
   * @since 1.5
   */
  public void remove(ThreadLocal<?> key)
  {
    int mask = this.hashMask;
    for(int i = key.fastHash & mask;; i = (i + 1) & mask)
      {
        Entry e = entries[i];
        if (e != null)
          {
            ThreadLocal<?> entryKey = e.get();
            if (entryKey != key)
              {
                if (entryKey == null) {
                  entries[i] = deletedEntry;
                }
                continue;
              }
            else
              {
                /* Remove from the table */
                entries[i] = deletedEntry;
              }
          }
          return;
      }
  }

  /**
   * Clear out the map. Done once during thread death.
   */
  void clear() {
    entries = null;
  }

  /**
   * Inherit all the InheritableThreadLocal instances from the given parent.
   *
   * @param parentMap The map to inherit from.
   */
  public void inherit(ThreadLocalMap parentMap) {
    for(Entry e: parentMap.entries)
      {
        if (e != null && e != deletedEntry)
          {
            ThreadLocal<?> key = e.get();
            if (key instanceof InheritableThreadLocal)
              {
                set(key, ((InheritableThreadLocal)key).childValue(e.value));
              }
          }
      }
  }
}
