/* EnumSet.java - Set of enum objects
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.

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
 * @author Dalibor Topic (robilad@kaffe.org)
 * @since 1.5 
 */

// FIXME: serialization is special, uses SerializationProxy.
// of(E e) is the 'bottom' method that creates a real EnumSet.
public abstract class EnumSet<T extends Enum<T>>
  extends AbstractSet<T>
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = 4782406773684236311L;

  // These fields could go into the anonymous inner class in of(E),
  // complementOf would need to be refactored then, though.
  BitSet store;
  int cardinality;
  Class<T> enumClass;

  EnumSet()
  {
  }

  public EnumSet<T> clone()
  {
    EnumSet<T> r;

    try
      {
	r = (EnumSet<T>) super.clone();
      }
    catch (CloneNotSupportedException _)
      {
	/* Can't happen */
	return null;
      }
    r.store = (BitSet) store.clone();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> allOf(Class<T> eltType)
  {
    // create an EnumSet from the list of values of the type
    return copyOf(Arrays.asList(eltType.getEnumConstants()));
  }

  public static <T extends Enum<T>> EnumSet<T> noneOf(Class<T> eltType)
  {
    return complementOf(allOf(eltType));
  }

  public static <T extends Enum<T>> EnumSet<T> copyOf(EnumSet<T> other)
  {
    return other.clone();
  }

  public static <T extends Enum<T>> EnumSet<T> copyOf(Collection<T> other)
  {
    if (other instanceof EnumSet)
      return copyOf((EnumSet<T>) other);
    if (other.isEmpty())
	throw new IllegalArgumentException("Collection is empty");

    EnumSet<T> r = null;

    for (T val : other)
      {
	if (r == null)
	  r = of(val);
	else
	  r.add(val);
      }

    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> complementOf(EnumSet<T> other)
  {
    EnumSet<T> r = other.clone();
    r.store.flip(0, r.store.size());
    r.cardinality = r.store.size() - other.cardinality;
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first)
  {
    EnumSet<T> r = new EnumSet<T>()
    {
      public boolean add(T val)
      {
	if (store.get(val.ordinal()))
	  return false;

	store.set(val.ordinal());
	++cardinality;
	return true;
      }

      public boolean addAll(Collection<? extends T> c)
      {
	boolean result = false;
	if (c instanceof EnumSet)
	{
	  EnumSet<T> other = (EnumSet<T>) c;
	  if (enumClass == other.enumClass)
	  {
	    store.or(other.store);
	    int save = cardinality;
	    cardinality = store.cardinality();
	    result = save != cardinality;
	  }
	}
	else
	{
	  for (T val : c)
	  {
	    if (add (val))
	    result = true;
	  }
	}
	return result;
      }

      public void clear()
      {
	store.clear();
	cardinality = 0;
      }

      public boolean contains(Object o)
      {
	if (! (o instanceof Enum))
	  return false;

	Enum<T> e = (Enum<T>) o;
	if (e.getDeclaringClass() != enumClass)
	  return false;

	return store.get(e.ordinal());
      }

      public boolean containsAll(Collection<?> c)
      {
	if (c instanceof EnumSet)
	{
	  EnumSet<T> other = (EnumSet<T>) c;
	  if (enumClass == other.enumClass)
	    return store.containsAll(other.store);

	  return false;
	}
	return super.containsAll(c);
      }

      public Iterator<T> iterator()
      {
	return new Iterator<T>()
	{
	  int next = -1;
	  int count = 0;

	  public boolean hasNext()
	  {
	    return count < cardinality;
	  }

	  public T next()
	  {
	    next = store.nextSetBit(next + 1);
	    ++count;
	    return enumClass.getEnumConstants()[next];
	  }

	  public void remove()
	  {
	    if (! store.get(next))
	    {
		store.clear(next);
		--cardinality;
	    }
	  }
	};
      }

      public boolean remove(Object o)
      {
	if (! (o instanceof Enum))
	  return false;

	Enum<T> e = (Enum<T>) o;
	if (e.getDeclaringClass() != enumClass)
	  return false;

	store.clear(e.ordinal());
	--cardinality;
	return true;
      }

      public boolean removeAll(Collection<?> c)
      {
	if (c instanceof EnumSet)
	{
	  EnumSet<T> other = (EnumSet<T>) c;
	  if (enumClass != other.enumClass)
	    return false;

	  store.andNot(other.store);
	  int save = cardinality;
	  cardinality = store.cardinality();
	  return save != cardinality;
	}
	return super.removeAll(c);
      }

      public boolean retainAll(Collection<?> c)
      {
	if (c instanceof EnumSet)
	{
	  EnumSet<T> other = (EnumSet<T>) c;
	  if (enumClass != other.enumClass)
	    return false;

	  store.and(other.store);
	  int save = cardinality;
	  cardinality = store.cardinality();
	  return save != cardinality;
	}
	return super.retainAll(c);
      }

      public int size()
      {
	return cardinality;
      }
    };

    // initialize the class
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);

    r.add(first);
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second)
  {
    EnumSet<T> r = of(first);
    r.add(second);
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third)
  {
    EnumSet<T> r = of(first, second);
    r.add(third);
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third,
						  T fourth)
  {
    EnumSet<T> r = of(first, second, third);
    r.add(fourth);
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third,
						  T fourth, T fifth)
  {
    EnumSet<T> r = of(first, second, third, fourth);
    r.add(fifth);
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T... rest)
  {
    EnumSet<T> r = noneOf(first.getDeclaringClass());
    r.add(first);
    for (T val : rest)
      r.add(val);
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> range(T from, T to)
  {
    if (from.compareTo(to) > 0)
      throw new IllegalArgumentException();
    Class<T> type = from.getDeclaringClass();
    EnumSet<T> r = noneOf(type);

    T[] values = type.getEnumConstants();
    // skip over values until start of range is found
    int i = 0;
    while (from != values[i])
      i++;

    // add values until end of range is found
    while (to != values[i]) {
      r.add(values[i]);
      i++;
    }

    // add end of range
    r.add(to);

    return r;
  }
}
