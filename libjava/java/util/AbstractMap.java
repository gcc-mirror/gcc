/* AbstractMap.java -- Abstract implementation of most of Map
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


// TO DO:
// comments
// test suite

package java.util;

public abstract class AbstractMap implements Map
{
  /**
   * Remove all entries from this Map. This default implementation calls
   * entrySet().clear().
   *
   * @throws UnsupportedOperationException
   * @specnote The JCL book claims that this implementation always throws 
   *           UnsupportedOperationException, while the online docs claim it
   *           calls entrySet().clear(). We take the later to be correct.
   */
  public void clear()
  {
    entrySet().clear();
  }

  public boolean containsKey(Object key)
  {
    Object k;
    Set es = entrySet();
    Iterator entries = es.iterator();
    int size = size();
    for (int pos = 0; pos < size; pos++)
      {
	k = ((Map.Entry) entries.next()).getKey();
	if (key == null ? k == null : key.equals(k))
	  return true;
      }
    return false;
  }

  public boolean containsValue(Object value)
  {
    Object v;
    Set es = entrySet();
    Iterator entries = es.iterator();
    int size = size();
    for (int pos = 0; pos < size; pos++)
      {
	v = ((Map.Entry) entries.next()).getValue();
	if (value == null ? v == null : value.equals(v))
	  return true;
      }
    return false;
  }

  public abstract Set entrySet();

  public boolean equals(Object o)
  {
    if (o == this)
      return true;
    if (!(o instanceof Map))
      return false;

    Map m = (Map) o;
    Set s = m.entrySet();
    Iterator itr = entrySet().iterator();
    int size = size();

    if (m.size() != size)
      return false;

    for (int pos = 0; pos < size; pos++)
      {
	if (!s.contains(itr.next()))
	  return false;
      }
    return true;
  }

  public Object get(Object key)
  {
    Set s = entrySet();
    Iterator entries = s.iterator();
    int size = size();

    for (int pos = 0; pos < size; pos++)
      {
	Map.Entry entry = (Map.Entry) entries.next();
	Object k = entry.getKey();
	if (key == null ? k == null : key.equals(k))
	  return entry.getValue();
      }

    return null;
  }

  public int hashCode()
  {
    int hashcode = 0;
    Iterator itr = entrySet().iterator();
    int size = size();
    for (int pos = 0; pos < size; pos++)
      {
	hashcode += itr.next().hashCode();
      }
    return hashcode;
  }

  public boolean isEmpty()
  {
    return size() == 0;
  }

  public Set keySet()
  {
    if (this.keySet == null)
      {
	this.keySet = new AbstractSet()
	{
	  public int size()
	  {
	    return AbstractMap.this.size();
	  }

	  public boolean contains(Object key)
	  {
	    return AbstractMap.this.containsKey(key);
	  }

	  public Iterator iterator()
	  {
	    return new Iterator()
	    {
	      Iterator map_iterator = AbstractMap.this.entrySet().iterator();

	      public boolean hasNext()
	      {
		return map_iterator.hasNext();
	      }

	      public Object next()
	      {
		return ((Map.Entry) map_iterator.next()).getKey();
	      }

	      public void remove()
	      {
		map_iterator.remove();
	      }
	    };
	  }
	};
      }

    return this.keySet;
  }

  public Object put(Object key, Object value)
  {
    throw new UnsupportedOperationException();
  }

  public void putAll(Map m)
  {
    Map.Entry entry;
    Iterator entries = m.entrySet().iterator();
    int size = m.size();

    for (int pos = 0; pos < size; pos++)
      {
	entry = (Map.Entry) entries.next();
	put(entry.getKey(), entry.getValue());
      }
  }

  public Object remove(Object key)
  {
    Iterator entries = entrySet().iterator();
    int size = size();

    for (int pos = 0; pos < size; pos++)
      {
	Map.Entry entry = (Map.Entry) entries.next();
	Object k = entry.getKey();
	if (key == null ? k == null : key.equals(k))
	  {
	    Object value = entry.getValue();
	    entries.remove();
	    return value;
	  }
      }

    return null;
  }

  public int size()
  {
    return entrySet().size();
  }

  public String toString()
  {
    Iterator entries = entrySet().iterator();
    int size = size();
    String r = "{";
    for (int pos = 0; pos < size; pos++)
      {
	r += entries.next();
	if (pos < size - 1)
	  r += ", ";
      }
    r += "}";
    return r;
  }

  public Collection values()
  {
    if (this.valueCollection == null)
      {
	this.valueCollection = new AbstractCollection()
	{
	  public int size()
	  {
	    return AbstractMap.this.size();
	  }

	  public Iterator iterator()
	  {
	    return new Iterator()
	    {
	      Iterator map_iterator = AbstractMap.this.entrySet().iterator();

	      public boolean hasNext()
	      {
		return map_iterator.hasNext();
	      }

	      public Object next()
	      {
		return ((Map.Entry) map_iterator.next()).getValue();
	      }

	      public void remove()
	      {
		map_iterator.remove();
	      }
	    };
	  }
	};
      }

    return this.valueCollection;
  }

  private Collection valueCollection = null;
  private Set keySet = null;
}
