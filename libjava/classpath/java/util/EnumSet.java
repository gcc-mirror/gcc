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
 * <p>
 * Provides an efficient mechanism for recording a set of enumeration
 * constants.  As enumerations have a known set of possible values, certain
 * assumptions can be made when creating a set of constants.  The maximum
 * size of the set will always be equal to the number of constants, and each
 * value will always be one of these constants.  As a result, the set only needs
 * to store whether a particular constant is present or not rather than the
 * values themselves.  Each constant can thus be represented by a single bit.
 * </p>
 * <p>
 * This class is designed to provide an alternative to using integer bit flags
 * by providing a typesafe {@link Collection} interface with an underlying 
 * implementation that utilises the assumptions above to give an equivalent level
 * of efficiency.  The values in a {@link EnumSet} must all be from the same
 * {@link Enum} type, which allows the contents to be packed into a bit vector.
 * A containment test is then simply a matter of inspecting the appropriate bit, while
 * addition involves setting the same.  Such basic operations take place in constant
 * time.
 * </p>
 * <p>
 * The {@link Iterator} implementation traverses the values in the natural order
 * of the enumeration provided by each constant's {@link Enum#ordinal()}.  It is
 * <emph>weakly consistent</emph> and will not throw a {@link ConcurrentModificationException}.
 * This means that concurrent changes to the set may or may not be noticeable during
 * traversal.
 * </p>
 * <p>
 * As is usual with most collections, the set is not synchronized by default.  This
 * can be remedied by using the {@link Collections#synchronizedSet(Set)} method.  Null
 * elements are not supported and attempts to add one will throw a {@link NullPointerException}.
 * </p>
 *
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
  /**
   * The store which maintains the bits used to represent
   * the enumeration constants.
   */
  BitSet store;

  /**
   * The cardinality of the set (the current number
   * of bits set).
   */
  int cardinality;

  /**
   * The enumeration used by this set.
   */
  Class<T> enumClass;

  /**
   * Empty package-private constructor
   */
  EnumSet()
  {
  }

  /**
   * Returns a clone of the set.
   * 
   * @return a clone of the set.
   */
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

  /**
   * Returns a set for the given enumeration type where
   * all the constants are present.
   *
   * @param eltType the type of enumeration to use for the set.
   * @return an {@link EnumSet} with all the bits set.
   * @throws NullPointerException if the element type is <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> allOf(Class<T> eltType)
  {
    // create an EnumSet from the list of values of the type
    return copyOf(Arrays.asList(eltType.getEnumConstants()));
  }

  /**
   * Returns a set for the given enumeration type where
   * none of the constants are present.
   *
   * @param eltType the type of enumeration to use for the set.
   * @return an {@link EnumSet} with none of the bits set.
   * @throws NullPointerException if the element type is <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> noneOf(Class<T> eltType)
  {
    return complementOf(allOf(eltType));
  }

  /**
   * Returns a clone of the given set.
   *
   * @param other the set to clone.
   * @return an {@link EnumSet} that is a clone of the given set.
   * @throws NullPointerException if <code>other</code> is <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> copyOf(EnumSet<T> other)
  {
    return other.clone();
  }

  /**
   * Creates an {@link EnumSet} using the contents of the given collection.
   * If the collection is also an {@link EnumSet}, this method works the
   * same as {@link #copyOf(EnumSet)}.  Otherwise, the elements of the collection
   * are inspected and used to populate the new set.
   *
   * @param other the collection to use to populate the new set.
   * @return an {@link EnumSet} containing elements from the given collection.
   * @throws NullPointerException if <code>other</code> is <code>null</code>.
   * @throws IllegalArgumentException if the collection is empty.
   */
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

  /**
   * Returns a set which is the inverse of the supplied set.
   * If a constant is present in the current set, it will not be
   * present in the new set and vice versa.
   *
   * @param other the set to provide the complement of.
   * @return an {@link EnumSet} which is the inverse of the current one.
   * @throws NullPointerException if <code>other</code> is <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> complementOf(EnumSet<T> other)
  {
    EnumSet<T> r = other.clone();
    int numConstants = r.enumClass.getEnumConstants().length;
    r.store.flip(0, numConstants);
    r.cardinality = numConstants - other.cardinality;
    return r;
  }

  /**
   * Creates a new {@link EnumSet} populated with the given element.
   * 
   * @param first the element to use to populate the new set.
   * @return an {@link EnumSet} containing the element.
   * @throws NullPointerException if <code>first</code> is <code>null</code>.
   */
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

  /**
   * Creates a new {@link EnumSet} populated with the given two elements.
   * 
   * @param first the first element to use to populate the new set.
   * @param second the second element to use.
   * @return an {@link EnumSet} containing the elements.
   * @throws NullPointerException if any of the parameters are <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> of(T first, T second)
  {
    EnumSet<T> r = of(first);
    r.add(second);
    return r;
  }

  /**
   * Creates a new {@link EnumSet} populated with the given three elements.
   * 
   * @param first the first element to use to populate the new set.
   * @param second the second element to use.
   * @param third the third element to use.
   * @return an {@link EnumSet} containing the elements.
   * @throws NullPointerException if any of the parameters are <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third)
  {
    EnumSet<T> r = of(first, second);
    r.add(third);
    return r;
  }

  /**
   * Creates a new {@link EnumSet} populated with the given four elements.
   * 
   * @param first the first element to use to populate the new set.
   * @param second the second element to use.
   * @param third the third element to use.
   * @param fourth the fourth element to use.
   * @return an {@link EnumSet} containing the elements.
   * @throws NullPointerException if any of the parameters are <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third,
						  T fourth)
  {
    EnumSet<T> r = of(first, second, third);
    r.add(fourth);
    return r;
  }

  /**
   * Creates a new {@link EnumSet} populated with the given five elements.
   * 
   * @param first the first element to use to populate the new set.
   * @param second the second element to use.
   * @param third the third element to use.
   * @param fourth the fourth element to use.
   * @param fifth the fifth element to use.
   * @return an {@link EnumSet} containing the elements.
   * @throws NullPointerException if any of the parameters are <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> of(T first, T second, T third,
						  T fourth, T fifth)
  {
    EnumSet<T> r = of(first, second, third, fourth);
    r.add(fifth);
    return r;
  }

  /**
   * Creates a new {@link EnumSet} populated with the given elements.
   * 
   * @param first the first element to use to populate the new set.
   * @param rest the other elements to use.
   * @return an {@link EnumSet} containing the elements.
   * @throws NullPointerException if any of the parameters are <code>null</code>.
   */
  public static <T extends Enum<T>> EnumSet<T> of(T first, T... rest)
  {
    EnumSet<T> r = noneOf(first.getDeclaringClass());
    r.add(first);
    for (T val : rest)
      r.add(val);
    return r;
  }

  /**
   * Creates a new {@link EnumSet} using the enumeration constants
   * starting from {@code from} and ending at {@code to} inclusive.
   * The two may be the same, but they must be in the correct order.
   * So giving the first constant twice would give a set with just that
   * constant set, while supplying the first and second constant will give
   * a set with those two elements.  However, specifying the second as
   * the {@code from} element followed by an earlier element as the
   * {@code to} element will result in an error.
   *
   * @param from the element to start from.
   * @param to the element to end at (may be the same as {@code from}.
   * @return an {@link EnumSet} containing the specified range of elements.
   * @throws NullPointerException if any of the parameters are <code>null</code>.
   * @throws IllegalArgumentException if {@code first.compareTo(last) > 0}.
   */
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
