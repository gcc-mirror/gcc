/* IdentityHashMap.java -- a class providing a hashtable data structure,
   mapping Object --> Object, which uses object identity for hashing.
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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.util;

import java.io.*;

/**
 * This class provides a hashtable-backed implementation of the
 * Map interface.  Unlike HashMap, it uses object identity to
 * do its hashing.  Also, it uses a linear-probe hash table.
 *
 * @author Tom Tromey <tromey@redhat.com>
 * @since 1.4
 */
public class IdentityHashMap extends AbstractMap
  implements Map, Serializable, Cloneable
{
  private static final int DEFAULT_CAPACITY = 21;

  /** Create a new IdentityHashMap with the default capacity (21
   * entries).
   */
  public IdentityHashMap ()
  {
    this (DEFAULT_CAPACITY);
  }

  /** Create a new IdentityHashMap with the indicated number of
   * entries.  If the number of elements added to this hash map
   * exceeds this maximum, the map will grow itself; however, that
   * incurs a performance penalty.
   * @param max Initial size
   */
  public IdentityHashMap (int max)
  {
    if (max < 0)
      throw new IllegalArgumentException ();
    table = new Object[2 * max];
    Arrays.fill (table, emptyslot);
    size = 0;
  }

  /** Create a new IdentityHashMap whose contents are taken from the
   * given Map.
   * @param m The map whose elements are to be put in this map.
   */
  public IdentityHashMap (Map m)
  {
    int len = 2 * Math.max (m.size (), DEFAULT_CAPACITY);
    table = new Object[len];
    Arrays.fill (table, emptyslot);
    putAll (m);
  }

  public void clear ()
  {
    Arrays.fill (table, emptyslot);
    size = 0;
  }

  /**
   * Creates a shallow copy where keys and values are not cloned.
   */
  public Object clone ()
  {
    try 
      {
        IdentityHashMap copy = (IdentityHashMap) super.clone ();
	copy.table = (Object[]) table.clone ();
	return copy;
      }
    catch (CloneNotSupportedException e) 
      {
	// Can't happen.
	return null;
      }
  }

  public boolean containsKey (Object key)
  {
    int h = getHash (key);
    int save = h;
    while (true)
      {
	if (table[h] == key)
	  return true;
	if (table[h] == emptyslot)
	  return false;
	h += 2;
	if (h >= table.length)
	  h = 0;
	if (h == save)
	  return false;
      }
  }

  public boolean containsValue (Object value)
  {
    for (int i = 1; i < table.length; i += 2)
      if (table[i] == value)
	return true;
    return false;
  }

  public Set entrySet ()
  {
    return new AbstractSet ()
    {
      public int size ()
      {
	return size;
      }

      public Iterator iterator ()
      {
	return new IdentityIterator (IdentityIterator.ENTRIES);
      }

      public void clear ()
      {
	IdentityHashMap.this.clear ();
      }

      public boolean contains (Object o)
      {
	if (! (o instanceof Map.Entry))
	  return false;
	Map.Entry m = (Map.Entry) o;
	return (IdentityHashMap.this.containsKey (m.getKey ())
		&& IdentityHashMap.this.get (m.getKey ()) == m.getValue ());
      }

      public boolean remove (Object o)
      {
	if (! (o instanceof Map.Entry))
	  return false;
	Map.Entry m = (Map.Entry) o;
	if (IdentityHashMap.this.containsKey (m.getKey ())
	    && IdentityHashMap.this.get (m.getKey ()) == m.getValue ())
	  {
	    int oldsize = size;
	    IdentityHashMap.this.remove (m.getKey ());
	    return oldsize != size;
	  }
	return false;
      }
    };
  }

  public Object get (Object key)
  {
    int h = getHash (key);
    int save = h;
    while (true)
      {
	if (table[h] == key)
	  return table[h + 1];
	if (table[h] == emptyslot)
	  return null;
	h += 2;
	if (h >= table.length)
	  h = 0;
	if (h == save)
	  return null;
      }
  }

  public boolean isEmpty ()
  {
    return size == 0;
  }

  public Set keySet ()
  {
    return new AbstractSet ()
    {
      public int size ()
      {
	return size;
      }

      public Iterator iterator ()
      {
	return new IdentityIterator (IdentityIterator.KEYS);
      }

      public void clear ()
      {
	IdentityHashMap.this.clear ();
      }

      public boolean contains (Object o)
      {
	return IdentityHashMap.this.containsKey (o);
      }

      public boolean remove (Object o)
      {
	int oldsize = size;
	IdentityHashMap.this.remove (o);
	return oldsize != size;
      }
    };
  }

  public Object put (Object key, Object value)
  {
    // Rehash if the load factor is too high.  We use a factor of 1.5
    // -- the division by 2 is implicit on both sides.
    if (size * 3 > table.length)
      {
	Object[] old = table;
	table = new Object[old.length * 2];
	Arrays.fill (table, emptyslot);
	size = 0;
	for (int i = 0; i < old.length; i += 2)
	  {
	    if (old[i] != tombstone && old[i] != emptyslot)
	      {
		// Just use put.  This isn't very efficient, but it is
		// ok.
		put (old[i], old[i + 1]);
	      }
	  }
      }

    int h = getHash (key);
    int save = h;
    int del = -1;
    while (true)
      {
	if (table[h] == key)
	  {
	    Object r = table[h + 1];
	    table[h + 1] = value;
	    return r;
	  }
	else if (table[h] == tombstone && del == -1)
	  del = h;
	else if (table[h] == emptyslot)
	  {
	    if (del == -1)
	      del = h;
	    break;
	  }
	h += 2;
	if (h >= table.length)
	  h = 0;
	if (h == save)
	  break;
      }

    if (del != -1)
      {
	table[del] = key;
	table[del + 1] = value;
	++size;
	return null;
      }

    // This is an error.
    return null;
  }

  public Object remove (Object key)
  {
    int h = getHash (key);
    int save = h;
    while (true)
      {
	if (table[h] == key)
	  {
	    Object r = table[h + 1];
	    table[h] = tombstone;
	    table[h + 1] = tombstone;
	    --size;
	    return r;
	  }
	h += 2;
	if (h >= table.length)
	  h = 0;
	if (h == save)
	  break;
      }

    return null;
  }

  public int size ()
  {
    return size;
  }

  public Collection values ()
  {
    return new AbstractCollection ()
    {
      public int size ()
      {
	return size;
      }

      public Iterator iterator ()
      {
	return new IdentityIterator (IdentityIterator.VALUES);
      }

      public void clear ()
      {
	IdentityHashMap.this.clear ();
      }
    };
  }

  private class IdentityIterator implements Iterator
  {
    static final int KEYS = 0;
    static final int VALUES = 1;
    static final int ENTRIES = 2;

    // Type of iterator.
    int type;
    // Location in the table.
    int loc;
    // How many items we've seen.
    int seen;

    IdentityIterator (int type)
    {
      this.type = type;
      loc = 0;
      seen = 0;
    }

    public boolean hasNext ()
    {
      return seen < size;
    }

    public Object next ()
    {
      while (true)
	{
	  loc += 2;
	  if (loc >= table.length)
	    throw new NoSuchElementException ();
	  if (table[loc] != tombstone && table[loc] != emptyslot)
	    {
	      ++seen;
	      return table[loc];
	    }
	}
    }

    public void remove ()
    {
      if (loc >= table.length
	  || table[loc] == tombstone
	  || table[loc] == emptyslot)
	throw new IllegalStateException ();
      table[loc] = tombstone;
      table[loc + 1] = tombstone;
      --size;
    }
  }

  private void readObject (ObjectInputStream s)
    throws IOException, ClassNotFoundException
  {
    int num = s.readInt ();
    for (int i = 0; i < num; ++i)
      {
	Object key = s.readObject ();
	Object value = s.readObject ();
	put (key, value);
      }
  }

  private void writeObject (ObjectOutputStream s)
    throws IOException
  {
    s.writeInt (size);
    Iterator it = entrySet ().iterator ();
    while (it.hasNext ())
      {
	Map.Entry entry = (Map.Entry) it.next ();
	s.writeObject (entry.getKey ());
	s.writeObject (entry.getValue ());
      }
  }

  // Compute the hash value we will use for an object.
  private int getHash (Object o)
  {
    return 2 * Math.abs (System.identityHashCode (o) % (table.length / 2));
  }

  // Number of items in hash table.
  private int size;
  // The table itself.
  private Object[] table;

  // This object is used to mark deleted items.
  private static final Object tombstone = new Object ();
  // This object is used to mark empty slots.  We need this because
  // using null is ambiguous.
  private static final Object emptyslot = new Object ();
}
