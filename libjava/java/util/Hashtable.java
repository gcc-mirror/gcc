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
import java.io.ObjectStreamField;

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
 * @author      Jon Zeppieri
 * @version     $Revision: 1.7 $
 * @modified    $Id: Hashtable.java,v 1.7 2000/03/15 21:59:13 rao Exp $
 */
public class Hashtable extends Dictionary 
  implements Map, Cloneable, Serializable
{
  // STATIC VARIABLES
  // ----------------
  
  /**
   * the default capacity of a Hashtable
   *
   * This value strikes me as absurdly low, an invitation to all manner of
   * hash collisions.  Perhaps it should be raised.  I set it to 11 since the
   * JDK-1.2b4 specification uses that value in the third constructor 
   * Hashtable(Map t) if the given Map is small. */
  private static final int DEFAULT_CAPACITY = 11;
  
  /** the defaulty load factor; this is explicitly specified by Sun */
  private static final float DEFAULT_LOAD_FACTOR = 0.75F;
  
  // used internally for parameterizing inner classes
  private static final int KEYS = 0;
  private static final int VALUES = 1;
  private static final int ENTRIES = 2; 
  
  // used for serializing instances of this class
  private static final ObjectStreamField[] serialPersistentFields =
  { new ObjectStreamField("loadFactor", float.class),
    new ObjectStreamField("threshold", int.class) };
  private static final long serialVersionUID = 1421746759512286392L;
  
  // INSTANCE VARIABLES
  // ------------------
  
  /** the capacity of this Hashtable:  denotes the size of the bucket array */
  private int capacity;
  
  /** the size of this Hashtable:  denotes the number of elements currently in
   * <pre>this</pre> */
  private int size;
  
  /** the load factor of this Hashtable:  used in computing the threshold */
  private float loadFactor;
  
  /* the rounded product of the capacity and the load factor; when the
   * number of elements exceeds the threshold, the Hashtable calls
   * <pre>rehash()</pre> */
  private int threshold;
  
  /** where the data is actually stored; Bucket implements
   * a very simple, lightweight (and hopefully fast) linked-list */
  Bucket[] buckets;
  
  /** counts the number of modifications this Hashtable has undergone;
   * used by Iterators to know when to throw
   * ConcurrentModificationExceptions (idea ripped-off from Stuart
   * Ballard's AbstractList implementation) */
  int modCount;
  
    /**
     * construct a new Hashtable with the default capacity and the
     * default load factor */
  public Hashtable()
  {
    init (DEFAULT_CAPACITY, DEFAULT_LOAD_FACTOR);
  }
  
  /**
   * construct a new Hashtable with a specific inital capacity and load factor
   *
   * @param   initialCapacity     the initial capacity of this Hashtable (>=0)
   * @param   initialLoadFactor   the load factor of this Hashtable 
   *                              (a misnomer, really, since the load factor of
   *                              a Hashtable does not change)
   * 
   * @throws   IllegalArgumentException    if (initialCapacity < 0) ||
   *                                          (initialLoadFactor > 1.0) ||
   *                                          (initialLoadFactor <= 0.0)
   */
  public Hashtable(int initialCapacity, float initialLoadFactor)
    throws IllegalArgumentException
  {
    if (initialCapacity < 0 || initialLoadFactor <= 0 || initialLoadFactor > 1)
      throw new IllegalArgumentException();
    else
      init(initialCapacity, initialLoadFactor);
  }
  
  /**
   * construct a new Hashtable with a specific inital capacity 
   *
   * @param   initialCapacity     the initial capacity of this Hashtable (>=0)
   *
   * @throws   IllegalArgumentException    if (initialCapacity < 0)
   */
  public Hashtable(int initialCapacity)	
    throws IllegalArgumentException 
  {
    if (initialCapacity < 0)
      throw new IllegalArgumentException();
    else
      init(initialCapacity, DEFAULT_LOAD_FACTOR);
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
  public Hashtable(Map t)
  {
    int mapSize = t.size() * 2;
    init (((mapSize > DEFAULT_CAPACITY) ? mapSize : DEFAULT_CAPACITY), 
	  DEFAULT_LOAD_FACTOR);
    putAll (t);
  }
  
  
  /** returns the number of key / value pairs stored in this Hashtable */
  public synchronized int size()
  {
    return size;
  }
  
  /** returns true if this Hashtable is empty (size() == 0), false otherwise */
  public synchronized boolean isEmpty()
  {
    return size == 0;
  }
  
  /** returns an Enumeration of the keys in this Hashtable
   *
   * <b>WARNING: if a Hashtable is changed while an Enumeration is
   * iterating over it, the behavior of the Enumeration is undefined.
   * Use keySet().iterator() if you want to be safe.</b> */
  public synchronized Enumeration keys()
  {
    return new HashtableEnumeration(KEYS);
  }
  
  /**
   * returns an Enumeration of the values in this Hashtable
   *
   * <b>WARNING: if a Hashtable is changed while an Enumeration is
   * iterating over it, the behavior of the Enumeration is undefined.
   * Use values().ieterator() if you want to be safe.</b> */
  public synchronized Enumeration elements()
  {
    return new HashtableEnumeration(VALUES);
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
   * @throws NullPointerException if <pre>value</pre> is null */
  public boolean contains(Object value) throws NullPointerException
  {
    if (value == null)
      throw new NullPointerException();
    else
      return containsValue(value);
  }
  
  /**
   * behaves identically to <pre>contains()</pre>, except it does not
   * throw a NullPointerException when given a null argument (Note:
   * Sun's implementation (JDK1.2beta4) <i>does</i> throw a
   * NullPointerException when given a null argument, but this seems
   * to go against the Collections Framework specifications, so I have
   * not reproduced this behavior.  I have submitted a bug report to
   * Sun on the mater, but have not received any response yet (26
   * September 1998)
   *
   * @param value the value to search for in this Hashtable */
  public synchronized boolean containsValue(Object value)
  {
    int i;
    Bucket list;
    
    for (i = 0; i < capacity; i++)
      {
	list = buckets[i];
	if (list != null && list.containsValue(value))
	  return true;
      }
    return false;
  }
  
  /**
   * returns true if the supplied key is found in this Hashtable
   * (strictly, if there exists a key <pre>k</pre> in the Hashtable,
   * such that <pre>k.equals(key)</pre>)
   *
   * @param key the key to search for in this Hashtable */
  public synchronized boolean containsKey(Object key)
  {
    return (internalGet(key) != null);
  }
  
  /**
   * a private method used by inner class HashtableSet to implement
   * its own <pre>contains(Map.Entry)</pre> method; returns true if
   * the supplied key / value pair is found in this Hashtable (again,
   * using <pre>equals()</pre>, rather than <pre>==</pre>)
   *
   * @param      entry      a Map.Entry to match against key / value pairs in 
   *                        this Hashtable */
  private synchronized boolean containsEntry(Map.Entry entry)
  {
    Object o;
    if (entry == null)
      {
	return false;
      }
    else
      {
	o = internalGet(entry.getKey());
	return (o != null && o.equals(entry.getValue()));
      }
  }
  
  /*
   * return the value in this Hashtable associated with the supplied
   * key, or <pre>null</pre> if the key maps to nothing
   *
   * @param key the key for which to fetch an associated value */
  public synchronized Object get(Object key)
  {
    return internalGet(key);
  }
  
  /** 
   * a private method which does the "dirty work" (or some of it
   * anyway) of fetching a value with a key
   *
   *  @param key the key for which to fetch an associated value */
  private Object internalGet(Object key)
  {
    Bucket list;
    if (key == null || size == 0)
      {
	return null;
      }
    else
      {
	list = buckets[hash(key)];
	return (list == null) ? null : list.getValueByKey(key);
      }
  }
  
  /** 
   * increases the size of the Hashtable and rehashes all keys to new
   * array indices; this is called when the addition of a new value
   * would cause size() > threshold */
  protected void rehash()
  {
    int i;
    Bucket[] data = buckets;
    Bucket.Node node;
    
    modCount++;
    capacity = (capacity * 2) + 1;
    size = 0;
    threshold = (int) ((float) capacity * loadFactor);
    buckets = new Bucket[capacity];
    for (i = 0; i < data.length; i++)
      {
	if (data[i] != null)
	  {
	    node = data[i].first;
	    while (node != null)
	      {
		internalPut(node.getKey(), node.getValue());
		node = node.next;
	      }
	  }
      }
  }
  
  /**
   * puts the supplied value into the Hashtable, mapped by the
   * supplied key; neither the key nore the value is allowed to be
   * <pre>null</pre>, otherwise a <pre>NullPointerException</pre> will
   * be thrown
   *
   * @param       key        the Hashtable key used to locate the value
   * @param value the value to be stored in the Hashtable */
  public synchronized Object put(Object key, Object value) 
    throws NullPointerException
  {
    if (key == null || value == null)
      throw new NullPointerException();
    else
      return internalPut(key, value);
  }
  
  /** 
   * A private method with a semi-interesting history (it's at least
   * two hours old now); orginally, this functionality was in the
   * public <pre>put()</pre> method, but while searching (fruitlessly)
   * on the JDC for some clarification of Javasoft's bizarre
   * Serialization documentation, I instead came across a JDK bug
   * which had been fixed in JDK-1.2b3.  Extending Hashtable was a
   * pain, because <pre>put()</pre> was apparently being used
   * internally by the class when the Hashtable was rehashed, and this
   * was causing odd behavior for people who had overridden
   * <pre>put()</pre> in a Hashtable subclass.  Well, I was also
   * calling <pre>put()</pre> internally, and realized that my code
   * would have the same problem. [No, I have never looked at the
   * Javasoft code; it was just the easiest thing to do].  So I put
   * the real work in a private method, and I call <i>this</i> for
   * internal use.  Except...not all the time.  What about
   * <pre>putAll()</pre>?  Well, it seems reasonably clear from the
   * Collections spec that <pre>putAll()</pre> is <i>supposed</i> to
   * call <pre>put()</pre>.  So, it still does.  Confused yet?
   *
   * @param       key        the Hashtable key used to locate the value
   * @param value the value to be stored in the Hashtable */
  private Object internalPut(Object key, Object value)
  {
    HashtableEntry entry;
    Bucket list;
    int hashIndex;
    Object oResult;
    
    modCount++;
    if (size == threshold)
      rehash();
    entry = new HashtableEntry(key, value);
    hashIndex = hash(key);
    list = buckets[hashIndex];
    if (list == null)
      {
	list = new Bucket();
	buckets[hashIndex] = list;
      }
    oResult = list.add(entry);
    if (oResult == null)
      {
	size++;
	return null;
      }
    else
      {
	return oResult;
      }
  }
  
  /**
   * removes from the Hashtable and returns the value which is mapped
   * by the supplied key; if the key maps to nothing, then the
   * Hashtable remains unchanged, and <pre>null</pre> is returned
   *
   * @param key the key used to locate the value to remove from the Hashtable */
  public synchronized Object remove(Object key)
  {
    Bucket list;
    int index;
    Object result = null;
    if (key != null && size > 0)
      {
	index = hash(key);
	list = buckets[index];
	if (list != null)
	  {
	    result = list.removeByKey(key);
	    if (result != null)
	      {
		size--;
		modCount++;
		if (list.first == null)
		  buckets[index] = null;
	      }
	  }
      }
    return result;
  }
  
  /**
   * part of the Map interface; for each Map.Entry in t, the key/value
   * pair is added to this Hashtable, <b>using the <pre>put()</pre>
   * method -- this may not be you want, so be warned (see notes to
   * <pre>internalPut()</pre>, above</b>
   *
   * @param t a Map whose key/value pairs will be added to this Hashtable */
  public synchronized void putAll(Map t) throws NullPointerException
  {
    Map.Entry entry;
    Iterator it = t.entrySet().iterator();
    while (it.hasNext())
      {
	entry = (Map.Entry) it.next();
	put(entry.getKey(), entry.getValue());
      }
  }
  
  
  /** empties this Hashtable of all elements */
  public synchronized void clear()
  {
    size = 0;
    modCount++;
    buckets = new Bucket[capacity];
  }
  
  /** 
   * returns a shallow clone of this Hashtable (i.e. the Hashtable
   * itself is cloned, but its contents are not) */
  public synchronized Object clone()
  {
    Map.Entry entry;
    Iterator it = entrySet().iterator();
    Hashtable clone = new Hashtable(capacity, loadFactor);
    while (it.hasNext())
      {
	entry = (Map.Entry) it.next();
	clone.internalPut(entry.getKey(), entry.getValue());
      }
    return clone;
  }
  
  /**
   * returns a String representation of this Hashtable
   *
   * the String representation of a Hashtable is defined by Sun and
   * looks like this:
   * <pre>
   * {name_1=value_1, name_2=value_2, name_3=value_3, ..., name_N=value_N}
   * </pre>
   * for N elements in this Hashtable */
  public synchronized String toString()
  {
    Map.Entry entry;
    Iterator it = entrySet().iterator();
    StringBuffer sb = new StringBuffer("{");
    boolean isFirst = true;
    while (it.hasNext())
      {
	entry = (Map.Entry) it.next();
	if (isFirst)
	  isFirst = false;
	else
	  sb.append(", ");
	sb.append(entry.getKey().toString()).append("=").append(entry.getValue().toString());
      }
    sb.append("}");
    return sb.toString();
  }
  
  /** returns a Set of Keys in this Hashtable */
  public synchronized Set keySet()
  {
    return new HashtableSet(KEYS);
  }
  
  /** 
   * returns a Set of Map.Entry objects in this Hashtable;
   * note, this was called <pre>entries()</pre> prior to JDK-1.2b4 */
  public synchronized Set entrySet()
  {
    return new HashtableSet(ENTRIES);
  }
  
  // This is the pre JDK1.2b4 named method for the above
  //     public Set entries()
  //     {
  // 	return entrySet();
  //     }
  
  /** returns a Collection of values in this Hashtable */
  public synchronized Collection values()
  {
    return new HashtableCollection();
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
  public synchronized boolean equals(Object o)
  {
    Map other;
    Set keys = keySet();
    Object currentKey;
    Iterator it;
    if (o instanceof Map)
      {
	other = (Map) o;
	if (other.keySet().equals(keys))
	  {
	    it = keys.iterator();
	    while (it.hasNext())
	      {
		currentKey = it.next();
		if (!get(currentKey).equals(other.get(currentKey)))
		  return false;
	      }
	    return true;
	  }
      }
    return false;
  }
  
  /** a Map's hashCode is the sum of the hashCodes of all of its
      Map.Entry objects */
  public synchronized int hashCode()
  {
    Iterator it = entrySet().iterator();
    int result = 0;
    while (it.hasNext())
      result += it.next().hashCode();
    return result;
  }
  
  /** 
   * a private method, called by all of the constructors to initialize a new Hashtable
   *
   * @param   initialCapacity     the initial capacity of this Hashtable (>=0)
   * @param   initialLoadFactor   the load factor of this Hashtable 
   *                              (a misnomer, really, since the load factor of
   *                              a Hashtable does not change)
   */ 
  private void init(int initialCapacity, float initialLoadFactor)
  {
    size = 0;
    modCount = 0;
    capacity = initialCapacity;
    loadFactor = initialLoadFactor;
    threshold = (int) ((float) capacity * loadFactor);
    buckets = new Bucket[capacity];
  }
  
  /** private -- simply hashes a non-null Object to its array index */
  private int hash(Object key)
  {
    return Math.abs(key.hashCode() % capacity);
  }
  
  /** Serialize this Object in a manner which is binary-compatible
      with the JDK */
  private void writeObject(ObjectOutputStream s) throws IOException
  {
    ObjectOutputStream.PutField oFields;
    Iterator it = entrySet().iterator();
    Map.Entry oEntry;
    oFields = s.putFields();
    oFields.put("loadFactor", loadFactor);
    oFields.put("threshold", threshold);
    s.writeFields();
    
    s.writeInt(capacity);
    s.writeInt(size);
    while (it.hasNext())
      {
	oEntry = (Map.Entry) it.next();
	s.writeObject(oEntry.getKey());
	s.writeObject(oEntry.getValue());
      }
  }
  
  /** Deserialize this Object in a manner which is binary-compatible
      with the JDK */
  private void readObject(ObjectInputStream s) 
    throws IOException, ClassNotFoundException
  {
    int i;
    int iLen;
    Object oKey, oValue;
    ObjectInputStream.GetField oFields;
    oFields = s.readFields();
    loadFactor = oFields.get("loadFactor", DEFAULT_LOAD_FACTOR);
    threshold = oFields.get("threshold", 
			    (int) (DEFAULT_LOAD_FACTOR 
				   * (float) DEFAULT_CAPACITY));
    
    capacity = s.readInt();
    iLen = s.readInt();
    size = 0;
    modCount = 0;
    buckets = new Bucket[capacity];
    
    for (i = 0; i < iLen; i++)
      {
	oKey = s.readObject();
	oValue = s.readObject();
	internalPut(oKey, oValue);
      }
  }
  
  /**
   * a Hashtable version of Map.Entry -- one thing in this implementation is
   * Hashtable-specific:  a NullPointerException is thrown if someone calls
   * <pre>setValue(null)</pre>
   *
   * Simply, a key / value pair
   *
   * @author      Jon Zeppieri
   * @version     $Revision: 1.7 $
   * @modified    $Id: Hashtable.java,v 1.7 2000/03/15 21:59:13 rao Exp $
   */
  private static class HashtableEntry extends Bucket.Node implements Map.Entry
  {
    /** construct a new HastableEntry with the given key and value */
    public HashtableEntry(Object key, Object value)
    {
      super(key, value);
    }
    
    /** sets the value of this Map.Entry; throws NullPointerException if 
     * <pre>newValue</pre> is null 
     *
     * @throws     NullPointerException   if <pre>newValue</pre> is null 
     */
    public Object setValue(Object newValue)
      throws UnsupportedOperationException, ClassCastException, 
      IllegalArgumentException, NullPointerException
    {
      if (newValue == null)
	throw new NullPointerException();
      else
	return super.setValue(newValue);
    }
  }
  
  
  /**
   * an inner class representing an Enumeration view of this
   * Hashtable, providing sequential access to its elements; this
   * implementation is parameterized to provide access either to the
   * keys or to the values in the Hashtable
   *
   * @author       Jon Zeppieri
   * @version      $Revision: 1.7 $
   * @modified $Id: Hashtable.java,v 1.7 2000/03/15 21:59:13 rao Exp $ */
  private class HashtableEnumeration implements Enumeration
  {
    /** the type of Enumeration:  KEYS or VALUES */
    private int myType;
    /** where are we in our iteration over the elements of this Hashtable */
    private int position;
    /** our current index into the BucketList array */
    private int bucketIndex;
    /** a reference to the specific Bucket at which our "cursor" is positioned */
    private Bucket.Node currentNode;
    
    /**
     * construct a new HashtableEnumeration with the given type of view
     *
     * @param      type        KEYS or VALUES:  the type of view this Enumeration is
     *                         providing
     */
    HashtableEnumeration(int type)
    {
      myType = type;
      position = 0;
      bucketIndex = -1;
      currentNode = null;
    }
    
    /**
     * returns true if not all elements have been retrived from the Enuemration
     *
     * <b>NOTE: modifications to the backing Hashtable while iterating
     * through an Enumeration can result in undefined behavior, as the
     * cursor may no longer be appropriately positioned</b> */
	public boolean hasMoreElements()
    {
      return position < Hashtable.this.size();
    }
    
    /**
     * returns the next element from the Enuemration
     *
     * <b>NOTE: modifications to the backing Hashtable while iterating
     * through an Enumeration can result in undefined behavior, as the
     * cursor may no longer be appropriately positioned</b>
     *
     * @throws    NoSuchElementException     if there are no more elements left in
     *                                       the sequential view */
    public Object nextElement()
    {
      Bucket list = null;
      Object result;
      try
	{
	  while (currentNode == null)
	    {
	      while (list == null)
		list = Hashtable.this.buckets[++bucketIndex];
	      currentNode = list.first;
	    }
	  result = (myType == KEYS) ? currentNode.getKey() : 
	    currentNode.getValue();
	  currentNode = currentNode.next;
	}
      catch(Exception e)
	{
	  throw new NoSuchElementException();
	}
      position++;
      return result;
    }
  }
  
  /**
   * an inner class providing a Set view of a Hashtable; this
   * implementation is parameterized to view either a Set of keys or a
   * Set of Map.Entry objects
   *
   * Note: a lot of these methods are implemented by AbstractSet, and
   * would work just fine without any meddling, but far greater
   * efficiency can be gained by overriding a number of them.  And so
   * I did.
   *
   * @author      Jon Zeppieri
   * @version     $Revision: 1.7 $
   * @modified $Id: Hashtable.java,v 1.7 2000/03/15 21:59:13 rao Exp $ */
  private class HashtableSet extends AbstractSet
  {
    /** the type of this Set view:  KEYS or ENTRIES */
    private int setType;
    
    /** construct a new HashtableSet with the supplied view type */
    HashtableSet(int type)
    {
      setType = type;
    }
    
    /**
     * adding an element is unsupported; this method simply throws an
     * exception
     *
     * @throws UnsupportedOperationException */
    public boolean add(Object o) throws UnsupportedOperationException
    {
      throw new UnsupportedOperationException();
    }
    
    /**
     * adding an element is unsupported; this method simply throws an
     * exception
     *
     * @throws UnsupportedOperationException */
    public boolean addAll(Collection c) throws UnsupportedOperationException
    {
      throw new UnsupportedOperationException();
    }
    
    /**
     * clears the backing Hashtable; this is a prime example of an
     * overridden implementation which is far more efficient than its
     * superclass implementation (which uses an iterator and is O(n)
     * -- this is an O(1) call) */
    public void clear()
    {
      Hashtable.this.clear();
    }
    
    /**
     * returns true if the supplied object is contained by this Set
     *
     * @param     o       an Object being testing to see if it is in this Set
     */
    public boolean contains(Object o)
    {
      if (setType == KEYS)
	return Hashtable.this.containsKey(o);
      else
	return (o instanceof Map.Entry) ? Hashtable.this.containsEntry((Map.Entry) o) : false;
    }
    
    /** 
     * returns true if the backing Hashtable is empty (which is the
     * only case either a KEYS Set or an ENTRIES Set would be empty) */
    public boolean isEmpty()
    {
      return Hashtable.this.isEmpty();
    }
    
    /**
     * removes the supplied Object from the Set
     *
     * @param      o       the Object to be removed
     */
    public boolean remove(Object o)
    {
      if (setType == KEYS)
	return (Hashtable.this.remove(o) != null);
      else
	return (o instanceof Map.Entry) ? 
	  (Hashtable.this.remove(((Map.Entry) o).getKey()) != null) : false;
    }
    
    /** returns the size of this Set (always equal to the size of the
        backing Hashtable) */
    public int size()
    {
      return Hashtable.this.size();
    }
    
    /** returns an Iterator over the elements of this Set */
    public Iterator iterator()
    {
      return new HashtableIterator(setType);
    }
  }
  
  /**
   * Like the above Set view, except this one if for values, which are not
   * guaranteed to be unique in a Hashtable; this prvides a Bag of values
   * in the Hashtable
   *
   * @author       Jon Zeppieri
   * @version      $Revision: 1.7 $
   * @modified     $Id: Hashtable.java,v 1.7 2000/03/15 21:59:13 rao Exp $
   */
  private class HashtableCollection extends AbstractCollection
  {
    /** a trivial contructor for HashtableCollection */
    HashtableCollection()
    {
    }
    
    /** 
     * adding elements is not supported by this Collection;
     * this method merely throws an exception
     *
     * @throws     UnsupportedOperationException
     */
    public boolean add(Object o) throws UnsupportedOperationException
    {
      throw new UnsupportedOperationException();
    }
    
    /** 
     * adding elements is not supported by this Collection;
     * this method merely throws an exception
     *
     * @throws     UnsupportedOperationException
     */
    public boolean addAll(Collection c) throws UnsupportedOperationException
    {
      throw new UnsupportedOperationException();
    }
    
    /** removes all elements from this Set (and from the backing Hashtable) */
    public void clear()
    {
      Hashtable.this.clear();
    }
    
    /** 
     * returns true if this Collection contains at least one Object which equals() the
     * supplied Object
     *
     * @param         o        the Object to compare against those in the Set
     */
    public boolean contains(Object o)
    {
      return Hashtable.this.containsValue(o);
    }
    
    /** returns true IFF the Collection has no elements */
    public boolean isEmpty()
    {
      return Hashtable.this.isEmpty();
    }
    
    /** returns the size of this Collection */
    public int size()
    {
      return Hashtable.this.size();
    }
    
    /** returns an Iterator over the elements in this Collection */
    public Iterator iterator()
    {
      return new HashtableIterator(VALUES);
    }
  }
  
  /**
   * Hashtable's version of the JDK-1.2 counterpart to the Enumeration;
   * this implementation is parameterized to give a sequential view of
   * keys, values, or entries; it also allows the removal of elements, 
   * as per the Javasoft spec.
   *
   * @author       Jon Zeppieri
   * @version      $Revision: 1.7 $
   * @modified     $Id: Hashtable.java,v 1.7 2000/03/15 21:59:13 rao Exp $
   */
  class HashtableIterator implements Iterator
  {
    /** the type of this Iterator: KEYS, VALUES, or ENTRIES */
    private int myType;
    /** 
     * the number of modifications to the backing Hashtable for which
     * this Iterator can account (idea ripped off from Stuart Ballard)
     */
    private int knownMods;
    /** the location of our sequential "cursor" */
    private int position;
    /** the current index of the BucketList array */
    private int bucketIndex;
    /** a reference, originally null, to the specific Bucket our
        "cursor" is pointing to */
    private Bucket.Node currentNode;
    /** a reference to the current key -- used fro removing elements
        via the Iterator */
    private Object currentKey;
    
    /** construct a new HashtableIterator with the supllied type:
        KEYS, VALUES, or ENTRIES */
    HashtableIterator(int type)
    {
      myType = type;
      knownMods = Hashtable.this.modCount;
      position = 0;
      bucketIndex = -1;
      currentNode = null;
      currentKey = null;
    }
    
    /** 
     * Stuart Ballard's code: if the backing Hashtable has been
     * altered through anything but <i>this</i> Iterator's
     * <pre>remove()</pre> method, we will give up right here, rather
     * than risking undefined behavior
     *
     * @throws ConcurrentModificationException */
    private void checkMod() 
    {
      if (knownMods != Hashtable.this.modCount)
	throw new ConcurrentModificationException();
    }
    
    /** returns true if the Iterator has more elements */
    public boolean hasNext()
    {
      checkMod();
      return position < Hashtable.this.size();
    }
    
    /** returns the next element in the Iterator's sequential view */
    public Object next()
    {
      Bucket list = null;
      Object result;
      checkMod();	    
      try
	{
	  while (currentNode == null)
	    {
	      while (list == null)
		list = Hashtable.this.buckets[++bucketIndex];
	      currentNode = list.first;
	    }
	  currentKey = currentNode.getKey();
	  result = (myType == KEYS) ? currentKey : 
	    ((myType == VALUES) ? currentNode.getValue() : currentNode);
	  currentNode = currentNode.next;
	}
      catch(Exception e)
	{
	  throw new NoSuchElementException();
	}
      position++;
      return result;
    }
    
    /** 
     * removes from the backing Hashtable the last element which was
     * fetched with the <pre>next()</pre> method */
    public void remove()
    {
      checkMod();
      if (currentKey == null)
	{
	  throw new IllegalStateException();
	}
      else
	{
	  Hashtable.this.remove(currentKey);
	  knownMods++;
	  position--;
	  currentKey = null;
	}
    }
  }
}








