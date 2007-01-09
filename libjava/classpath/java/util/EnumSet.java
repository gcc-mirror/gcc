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
 * @since 1.5 
 */

// FIXME: serialization is special.
public class EnumSet<T extends Enum<T>>
  extends AbstractSet<T>
  implements Cloneable, Serializable
{
  private static final long serialVersionUID = 4782406773684236311L;

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

  public int size()
  {
    return cardinality;
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

  public static <T extends Enum<T>> EnumSet<T> allOf(Class<T> eltType)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.store = new BitSet(eltType.getEnumConstants().length);
    r.store.set(0, r.store.size());
    r.cardinality = r.store.size();
    r.enumClass = eltType;
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> noneOf(Class<T> eltType)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.store = new BitSet(eltType.getEnumConstants().length);
    r.enumClass = eltType;
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> copyOf(EnumSet<T> other)
  {
    // We can't just use `other.clone' since we don't want to make a
    // subclass.
    EnumSet<T> r = new EnumSet<T>();
    r.store = (BitSet) other.store.clone();
    r.cardinality = other.cardinality;
    r.enumClass = other.enumClass;
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> copyOf(Collection<T> other)
  {
    if (other instanceof EnumSet)
      return copyOf((EnumSet<T>) other);
    EnumSet<T> r = new EnumSet<T>();
    for (T val : other)
      {
	if (r.store == null)
	  {
	    r.enumClass = val.getDeclaringClass();
	    r.store = new BitSet(r.enumClass.getEnumConstants().length);
	  }
	r.store.set(val.ordinal());
      }
    // The collection must contain at least one element.
    if (r.store == null)
      throw new IllegalArgumentException();
    r.cardinality = r.store.cardinality();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> complementOf(EnumSet<T> other)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.store = (BitSet) other.store.clone();
    r.store.flip(0, r.store.size());
    r.cardinality = r.store.size() - other.cardinality;
    r.enumClass = other.enumClass;
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);
    r.store.set(first.ordinal());
    r.cardinality = 1;
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);
    r.store.set(first.ordinal());
    r.store.set(second.ordinal());
    r.cardinality = r.store.cardinality();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);
    r.store.set(first.ordinal());
    r.store.set(second.ordinal());
    r.store.set(third.ordinal());
    r.cardinality = r.store.cardinality();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third,
						  T fourth)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);
    r.store.set(first.ordinal());
    r.store.set(second.ordinal());
    r.store.set(third.ordinal());
    r.store.set(fourth.ordinal());
    r.cardinality = r.store.cardinality();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third,
						  T fourth, T fifth)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);
    r.store.set(first.ordinal());
    r.store.set(second.ordinal());
    r.store.set(third.ordinal());
    r.store.set(fourth.ordinal());
    r.store.set(fifth.ordinal());
    r.cardinality = r.store.cardinality();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> of(T first, T... rest)
  {
    EnumSet<T> r = new EnumSet<T>();
    r.enumClass = first.getDeclaringClass();
    r.store = new BitSet(r.enumClass.getEnumConstants().length);
    r.store.set(first.ordinal());
    for (T val : rest)
      r.store.set(val.ordinal());
    r.cardinality = r.store.cardinality();
    return r;
  }

  public static <T extends Enum<T>> EnumSet<T> range(T from, T to)
  {
    if (from.compareTo(to) > 0)
      throw new IllegalArgumentException();
    EnumSet<T> r = new EnumSet<T>();
    r.store = new BitSet(from.getDeclaringClass().getEnumConstants().length);
    r.store.set(from.ordinal(), to.ordinal() + 1);
    r.enumClass = from.getDeclaringClass();
    r.cardinality = to.ordinal() - from.ordinal() + 1;
    return r;
  }
}
