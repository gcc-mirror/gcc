/* Copyright (C) 1998, 1999, 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util;

import java.io.Serializable;

/**
 * @author Warren Levy <warrenl@cygnus.com>
 * @date September 24, 1998.
 */
/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct
 */

final class HashtableEntry
{
  public Object key;
  public Object value;
  public HashtableEntry nextEntry = null;

  public HashtableEntry(Object key, Object value)
  {
    this.key = key;
    this.value = value;
  }
}

final class HashtableEnumeration implements Enumeration
{
  // TBD: Enumeration is not safe if new elements are put in the table as
  // this could cause a rehash and we'd completely lose our place.  Even
  // without a rehash, it is undetermined if a new element added would
  // appear in the enumeration.  The spec says nothing about this, but
  // the "Java Class Libraries" book infers that modifications to the
  // hashtable during enumeration causes indeterminate results.  Don't do it!
  // A safer way would be to make a copy of the table (e.g. into a vector)
  // but this is a fair bit  more expensive.
  private HashtableEntry[] bucket;
  private int bucketIndex;
  private HashtableEntry elem;
  private int enumCount;
  private int size;
  private boolean values;

  public HashtableEnumeration(HashtableEntry[] bkt, int sz, boolean isValues)
  {
    bucket = bkt;
    bucketIndex = -1;
    enumCount = 0;
    elem = null;
    size = sz;
    values = isValues;
  }

  public boolean hasMoreElements()
  {
    return enumCount < size;
  }

  public Object nextElement()
  {
    if (!hasMoreElements())
      throw new NoSuchElementException();

    // Find next element
    if (elem != null)		// In the middle of a bucket
      elem = elem.nextEntry;
    while (elem == null)	// Find the next non-empty bucket
      elem = bucket[++bucketIndex];

    enumCount++;
    return values ? elem.value : elem.key;
  }
}

// TBD: The algorithm used here closely reflects what is described in
// the "Java Class Libraries" book.  The "Java Language Spec" is much
// less specific about the implementation.  Because of this freedom
// provided by the actual spec, hash table algorithms should be
// investigated to see if there is a better alternative to this one.

// TODO12:
// public class Hashtable extends Dictionary
//			implements Map, Cloneable, Serializable

public class Hashtable extends Dictionary implements Cloneable, Serializable
{
  private HashtableEntry bucket[];
  private float loadFactor;
  private int hsize = 0;

  public Hashtable()
  {
    // The "Java Class Libraries" book (p. 919) says that initial size in this
    // case is 101 (a prime number to increase the odds of even distribution).
    this(101, 0.75F);
  }

  public Hashtable(int initialSize)
  {
    this(initialSize, 0.75F);
  }

  public Hashtable(int initialSize, float loadFactor)
  {
    if (initialSize < 0 || loadFactor <= 0.0 || loadFactor > 1.0)
      throw new IllegalArgumentException();

    bucket = new HashtableEntry[initialSize];
    this.loadFactor = loadFactor;
  }

  // TODO12:
  // public Hashtable(Map t)
  // {
  // }

  public synchronized void clear()
  {
    // Aid the GC by nulling out the entries in the hash table.
    for (int i = 0; i < bucket.length; i++)
      {
        HashtableEntry elem = bucket[i];
	bucket[i] = null;			// May already be null.
	while (elem != null)
	  {
	    HashtableEntry next = elem.nextEntry;
	    elem.nextEntry = null;		// May already be null.
	    elem = next;
	  }
      }
    hsize = 0;
  }

  public synchronized Object clone()
  {
    // New hashtable will have same initialCapacity and loadFactor.
    Hashtable newTable = new Hashtable(bucket.length, loadFactor);

    HashtableEntry newElem, prev = null;
    for (int i = 0; i < bucket.length; i++)
      for (HashtableEntry elem = bucket[i]; elem != null; elem = elem.nextEntry)
	{
	  // An easy but expensive method is newTable.put(elem.key, elem.value);
	  // Since the hash tables are the same size, the buckets and collisions
	  // will be the same in the new one, so we can just clone directly.
	  // This is much cheaper than using put.
	  newElem = new HashtableEntry(elem.key, elem.value);
	  if (newTable.bucket[i] == null)
	    prev = newTable.bucket[i] = newElem;
	  else
	    prev = prev.nextEntry = newElem;
	}

    newTable.hsize = this.hsize;
    return newTable;
  }

  public synchronized boolean contains(Object value)
    throws NullPointerException
  {
    // An exception is thrown here according to the JDK 1.2 doc.
    if (value == null)
      throw new NullPointerException();

    for (int i = 0; i < bucket.length; i++)
      for (HashtableEntry elem = bucket[i]; elem != null; elem = elem.nextEntry)
	if (elem.value.equals(value))
	  return true;

    return false;
  }

  public synchronized boolean containsKey(Object key)
  {
    for (HashtableEntry elem = bucket[Math.abs(key.hashCode()
					       % bucket.length)];
	 elem != null; elem = elem.nextEntry)
      if (elem.key.equals(key))
	return true;

    return false;
  }

  public synchronized Enumeration elements()
  {
    return new HashtableEnumeration(bucket, hsize, true);
  }

  public synchronized Object get(Object key)
  {
    for (HashtableEntry elem = bucket[Math.abs (key.hashCode()
						% bucket.length)];
	 elem != null; elem = elem.nextEntry)
      if (elem.key.equals(key))
	return elem.value;

    return null;
  }

  public boolean isEmpty()
  {
    return this.hsize <= 0;
  }

  public synchronized Enumeration keys()
  {
    return new HashtableEnumeration(bucket, hsize, false);
  }

  public synchronized Object put(Object key, Object value)
    throws NullPointerException
  {
    // We could really just check `value == null', but checking both
    // is a bit clearer.
    if (key == null || value == null)
      throw new NullPointerException();

    HashtableEntry prevElem = null;
    final int index = Math.abs(key.hashCode() % bucket.length);

    for (HashtableEntry elem = bucket[index]; elem != null;
	 prevElem = elem, elem = elem.nextEntry)
      if (elem.key.equals(key))
	{
	  // Update with the new value and then return the old one.
	  Object oldVal = elem.value;
	  elem.value = value;
	  return oldVal;
	}

    // At this point, we know we need to add a new element.
    HashtableEntry newElem = new HashtableEntry(key, value);
    if (bucket[index] == null)
      bucket[index] = newElem;
    else
      prevElem.nextEntry = newElem;

    if (++hsize > loadFactor * bucket.length)
      rehash();

    return null;
  }

  protected void rehash()
  {
    // Create a new table which is twice the size (plus one) of the old.
    // One is added to make the new array length odd so it thus has at least
    // a (small) possibility of being a prime number.
    HashtableEntry oldBucket[] = bucket;
    bucket = new HashtableEntry[bucket.length * 2 + 1];

    // Copy over each entry into the new table
    HashtableEntry elem;
    for (int i = 0; i < oldBucket.length; i++)
      for (elem = oldBucket[i]; elem != null; elem = elem.nextEntry)
	{
	  // Calling put(elem.key, elem.value); would seem like the easy way
	  // but it is dangerous since put increases 'hsize' and calls rehash!
	  // This could become infinite recursion under the right
	  // circumstances.  Instead, we'll add the element directly; this is a
	  // bit more efficient than put since the data is already verified.
    	  final int index = Math.abs(elem.key.hashCode() % bucket.length);
	  HashtableEntry newElem = new HashtableEntry(elem.key, elem.value);
	  if (bucket[index] == null)
	    bucket[index] = newElem;
	  else
	    {
	      // Since this key can't already be in the table, just add this
	      // in at the top of the bucket.
	      newElem.nextEntry = bucket[index];
	      bucket[index] = newElem;
	    }
	}
  }

  public synchronized Object remove(Object key)
  {
    // TBD: Hmm, none of the various docs say to throw an exception here.
    if (key == null)
      return null;

    Object retval;
    HashtableEntry prevElem = null;
    final int index = Math.abs(key.hashCode() % bucket.length);

    for (HashtableEntry elem = bucket[index]; elem != null;
	 prevElem = elem, elem = elem.nextEntry)
      if (elem.key.equals(key))
	{
	  retval = elem.value;
	  if (prevElem == null)
	    bucket[index] = elem.nextEntry;
	  else
	    prevElem.nextEntry = elem.nextEntry;
	  --hsize;
	  return retval;
	}

    return null;
  }

  public int size()
  {
    return this.hsize;
  }

  public synchronized String toString()
  {
    // Following the Java Lang Spec 21.5.4 (p. 636).

    Enumeration keys = keys();
    Enumeration values = elements();

    // Prepend first element with open bracket
    StringBuffer result = new StringBuffer("{");

    // add first element if one exists
    // TBD: Seems like it is more efficient to catch the exception than
    // to call hasMoreElements each time around.
    try
    {
      result.append(keys.nextElement().toString() + "=" +
		values.nextElement().toString());
    }
    catch (NoSuchElementException ex)
    {
    }

    // Prepend subsequent elements with ", "
    try
    {
      while (true)
        result.append(", " + keys.nextElement().toString() + "=" +
		values.nextElement().toString());
    }
    catch (NoSuchElementException ex)
    {
    }

    // Append last element with closing bracket
    result.append("}");
    return result.toString();
  }

  // TODO12:
  // public Set entrySet()
  // {
  // }

  // TODO12:
  // public Set keySet()
  // {
  // }

  // Since JDK 1.2:
  // This method is identical to contains but is part of the 1.2 Map interface.
  // TBD: Should contains return containsValue instead?  Depends on which
  // will be called more typically.
  public synchronized boolean containsValue(Object value)
  {
    return this.contains(value);
  }

  // TODO12:
  // public boolean equals(Object o)
  // {
  // }

  // TODO12:
  // public boolean hashCode()
  // {
  // }

  // TODO12:
  // public void putAll(Map t)
  // {
  // }

  // TODO12:
  // public Collection values()
  // {
  // }
}
