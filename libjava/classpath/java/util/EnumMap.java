/* EnumMap.java - Map where keys are enum constants
   Copyright (C) 2004, 2005, 2007 Free Software Foundation, Inc.

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

import java.io.Serializable;

/** 
 * @author Tom Tromey (tromey@redhat.com)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5 
 */

public class EnumMap<K extends Enum<K>, V>
  extends AbstractMap<K, V>
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = 458661240069192865L;

  V[] store;
  int cardinality;
  Class<K> enumClass;

  /**
   * The cache for {@link #entrySet()}.
   */
  transient Set<Map.Entry<K, V>> entries;

  static final Object emptySlot = new Object();

  public EnumMap(Class<K> keyType)
  {
    store = (V[]) new Object[keyType.getEnumConstants().length];
    Arrays.fill(store, emptySlot);
    cardinality = 0;
    enumClass = keyType;
  }

  public EnumMap(EnumMap<K, ? extends V> map)
  {
    store = (V[]) map.store.clone();
    cardinality = map.cardinality;
    enumClass = map.enumClass;
  }

  public EnumMap(Map<K, ? extends V> map)
  {
    if (map instanceof EnumMap)
      {
	EnumMap<K, ? extends V> other = (EnumMap<K, ? extends V>) map;
	store = (V[]) other.store.clone();
	cardinality = other.cardinality;
	enumClass = other.enumClass;
      }
    else
      {
	for (K key : map.keySet())
	  {
	    V value = map.get(key);
	    if (store == null)
	      {
		enumClass = key.getDeclaringClass();
		store = (V[]) new Object[enumClass.getEnumConstants().length];
	      }
	    int o = key.ordinal();
	    if (store[o] == emptySlot)
	      ++cardinality;
	    store[o] = value;
	  }
	// There must be a single element.
	if (store == null)
	  throw new IllegalArgumentException("no elements in map");
      }
  }

  public int size()
  {
    return cardinality;
  }

  public boolean containsValue(Object value)
  {
    for (V i : store)
      {
	if (i != emptySlot && AbstractCollection.equals(i , value))
	  return true;
      }
    return false;
  }

  public boolean containsKey(Object key)
  {
    if (! (key instanceof Enum))
      return false;
    Enum<K> e = (Enum<K>) key;
    if (e.getDeclaringClass() != enumClass)
      return false;
    return store[e.ordinal()] != emptySlot;
  }

  public V get(Object key)
  {
    if (! (key instanceof Enum))
      return null;
    Enum<K> e = (Enum<K>) key;
    if (e.getDeclaringClass() != enumClass)
      return null;
    V o = store[e.ordinal()];
    return o == emptySlot ? null : o;
  }

  public V put(K key, V value)
  {
    int o = key.ordinal();
    V result;
    if (store[o] == emptySlot)
      {
	result = null;
	++cardinality;
      }
    else
      result = store[o];
    store[o] = value;
    return result;
  }

  public V remove(Object key)
  {
    if (! (key instanceof Enum))
      return null;
    Enum<K> e = (Enum<K>) key;
    if (e.getDeclaringClass() != enumClass)
      return null;
    V result = store[e.ordinal()];
    if (result == emptySlot)
      result = null;
    else
      --cardinality;
    store[e.ordinal()] = (V) emptySlot;
    return result;
  }

  public void putAll(Map<? extends K, ? extends V> map)
  {
    for (K key : map.keySet())
      {
	V value = map.get(key);

	int o = key.ordinal();
	if (store[o] == emptySlot)
	  ++cardinality;
	store[o] = value;
      }
  }

  public void clear()
  {
    Arrays.fill(store, emptySlot);
    cardinality = 0;
  }

  public Set<K> keySet()
  {
    if (keys == null)
      {
	keys = new AbstractSet<K>()
	{
	  public int size()
	  {
	    return cardinality;
	  }

	  public Iterator<K> iterator()
	  {
	    return new Iterator<K>()
	    {
	      int count = 0;
	      int index = -1;

	      public boolean hasNext()
	      {
		return count < cardinality;
	      }

	      public K next()
	      {
		++count;
		for (++index; store[index] == emptySlot; ++index)
		  ;
		return enumClass.getEnumConstants()[index];
	      }

	      public void remove()
	      {
		--cardinality;
		store[index] = (V) emptySlot;
	      }
	    };
	  }

	  public void clear()
	  {
	    EnumMap.this.clear();
	  }

	  public boolean contains(Object o)
	  {
	    return contains(o);
	  }

	  public boolean remove(Object o)
	  {
	    return EnumMap.this.remove(o) != null;
	  }
	};
      }
    return keys;
  }

  public Collection<V> values()
  {
    if (values == null)
      {
	values = new AbstractCollection<V>()
	{
	  public int size()
	  {
	    return cardinality;
	  }

	  public Iterator<V> iterator()
	  {
	    return new Iterator<V>()
	    {
	      int count = 0;
	      int index = -1;

	      public boolean hasNext()
	      {
		return count < cardinality;
	      }

	      public V next()
	      {
		++count;
		for (++index; store[index] == emptySlot; ++index)
		  ;
		return store[index];
	      }

	      public void remove()
	      {
		--cardinality;
		store[index] = (V) emptySlot;
	      }
	    };
	  }

	  public void clear()
	  {
	    EnumMap.this.clear();
	  }
	};
      }
    return values;
  }

  public Set<Map.Entry<K, V>> entrySet()
  {
    if (entries == null)
      {
	entries = new AbstractSet<Map.Entry<K, V>>()
	{
	  public int size()
	  {
	    return cardinality;
	  }

	  public Iterator<Map.Entry<K, V>> iterator()
	  {
	    return new Iterator<Map.Entry<K, V>>()
	    {
	      int count = 0;
	      int index = -1;

	      public boolean hasNext()
	      {
		return count < cardinality;
	      }

	      public Map.Entry<K,V> next()
	      {
		++count;
		for (++index; store[index] == emptySlot; ++index)
		  ;
		// FIXME: we could just return something that
		// only knows the index.  That would be cleaner.
		return new AbstractMap.SimpleEntry<K, V>(enumClass.getEnumConstants()[index],
							   store[index])
		{
		  public V setValue(V newVal)
		  {
		    value = newVal;
		    return put(key, newVal);
		  }
		};
	      }

	      public void remove()
	      {
		--cardinality;
		store[index] = (V) emptySlot;
	      }
	    };
	  }

	  public void clear()
	  {
	    EnumMap.this.clear();
	  }

	  public boolean contains(Object o)
	  {
	    if (! (o instanceof Map.Entry))
	      return false;
	    Map.Entry<K, V> other = (Map.Entry<K, V>) o;
	    return (containsKey(other.getKey())
		    && AbstractCollection.equals(get(other.getKey()),
						 other.getValue()));
	  }

	  public boolean remove(Object o)
	  {
	    if (! (o instanceof Map.Entry))
	      return false;
	    Map.Entry<K, V> other = (Map.Entry<K, V>) o;
	    return EnumMap.this.remove(other.getKey()) != null;
	  }
	};
      }
    return entries;
  }

  public boolean equals(Object o)
  {
    if (! (o instanceof EnumMap))
      return false;
    EnumMap<K, V> other = (EnumMap<K, V>) o;
    if (other.enumClass != enumClass || other.cardinality != cardinality)
      return false;
    return Arrays.equals(store, other.store);
  }

  public EnumMap<K, V> clone()
  {
    EnumMap<K, V> result;
    try
      {
	result = (EnumMap<K, V>) super.clone();
      }
    catch (CloneNotSupportedException ignore)
      {
	// Can't happen.
	result = null;
      }
    result.store = (V[]) store.clone();
    return result;
  }

}
