/* Set.java -- A collection that prohibits duplicates
   Copyright (C) 1998, 2001, 2004, 2005
   Free Software Foundation, Inc.

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

/**
 * A collection that contains no duplicates. In other words, for two set
 * elements e1 and e2, <code>e1.equals(e2)</code> returns false. There
 * are additional stipulations on <code>add</code>, <code>equals</code>
 * and <code>hashCode</code>, as well as the requirements that constructors
 * do not permit duplicate elements. The Set interface is incompatible with
 * List; you cannot implement both simultaneously.
 * <p>
 *
 * Note: Be careful about using mutable objects in sets.  In particular,
 * if a mutable object changes to become equal to another set element, you
 * have violated the contract.  As a special case of this, a Set is not
 * allowed to be an element of itself, without risking undefined behavior.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see List
 * @see SortedSet
 * @see HashSet
 * @see TreeSet
 * @see LinkedHashSet
 * @see AbstractSet
 * @see Collections#singleton(Object)
 * @see Collections#EMPTY_SET
 * @since 1.2
 * @status updated to 1.4
 */
public interface Set<E> extends Collection<E>
{
  /**
   * Adds the specified element to the set if it is not already present
   * (optional operation). In particular, the comparison algorithm is
   * <code>o == null ? e == null : o.equals(e)</code>. Sets need not permit
   * all values, and may document what exceptions will be thrown if
   * a value is not permitted.
   *
   * @param o the object to add
   * @return true if the object was not previously in the set
   * @throws UnsupportedOperationException if this operation is not allowed
   * @throws ClassCastException if the class of o prevents it from being added
   * @throws IllegalArgumentException if some aspect of o prevents it from
   *         being added
   * @throws NullPointerException if null is not permitted in this set
   */
  boolean add(E o);

  /**
   * Adds all of the elements of the given collection to this set (optional
   * operation). If the argument is also a Set, this returns the mathematical
   * <i>union</i> of the two. The behavior is unspecified if the set is
   * modified while this is taking place.
   *
   * @param c the collection to add
   * @return true if the set changed as a result
   * @throws UnsupportedOperationException if this operation is not allowed
   * @throws ClassCastException if the class of an element prevents it from
   *         being added
   * @throws IllegalArgumentException if something about an element prevents
   *         it from being added
   * @throws NullPointerException if null is not permitted in this set, or
   *         if the argument c is null
   * @see #add(Object)
   */
  boolean addAll(Collection<? extends E> c);

  /**
   * Removes all elements from this set (optional operation). This set will
   * be empty afterwords, unless an exception occurs.
   *
   * @throws UnsupportedOperationException if this operation is not allowed
   */
  void clear();

  /**
   * Returns true if the set contains the specified element. In other words,
   * this looks for <code>o == null ? e == null : o.equals(e)</code>.
   *
   * @param o the object to look for
   * @return true if it is found in the set
   * @throws ClassCastException if the type of o is not a valid type
   *         for this set.
   * @throws NullPointerException if o is null and this set doesn't
   *         support null values.
   */
  boolean contains(Object o);

  /**
   * Returns true if this set contains all elements in the specified
   * collection. If the argument is also a set, this is the <i>subset</i>
   * relationship.
   *
   * @param c the collection to check membership in
   * @return true if all elements in this set are in c
   * @throws NullPointerException if c is null
   * @throws ClassCastException if the type of any element in c is not
   *         a valid type for this set.
   * @throws NullPointerException if some element of c is null and this
   *         set doesn't support null values.
   * @see #contains(Object)
   */
  boolean containsAll(Collection<?> c);

  /**
   * Compares the specified object to this for equality. For sets, the object
   * must be a set, the two must have the same size, and every element in
   * one must be in the other.
   *
   * @param o the object to compare to
   * @return true if it is an equal set
   */
  boolean equals(Object o);

  /**
   * Returns the hash code for this set. In order to satisfy the contract of
   * equals, this is the sum of the hashcode of all elements in the set.
   *
   * @return the sum of the hashcodes of all set elements
   * @see #equals(Object)
   */
  int hashCode();

  /**
   * Returns true if the set contains no elements.
   *
   * @return true if the set is empty
   */
  boolean isEmpty();

  /**
   * Returns an iterator over the set.  The iterator has no specific order,
   * unless further specified.
   *
   * @return a set iterator
   */
  Iterator<E> iterator();

  /**
   * Removes the specified element from this set (optional operation). If
   * an element e exists, <code>o == null ? e == null : o.equals(e)</code>,
   * it is removed from the set.
   *
   * @param o the object to remove
   * @return true if the set changed (an object was removed)
   * @throws UnsupportedOperationException if this operation is not allowed
   * @throws ClassCastException if the type of o is not a valid type
   *         for this set.
   * @throws NullPointerException if o is null and this set doesn't allow
   *         the removal of a null value.
   */
  boolean remove(Object o);

  /**
   * Removes from this set all elements contained in the specified collection
   * (optional operation). If the argument is a set, this returns the
   * <i>asymmetric set difference</i> of the two sets.
   *
   * @param c the collection to remove from this set
   * @return true if this set changed as a result
   * @throws UnsupportedOperationException if this operation is not allowed
   * @throws NullPointerException if c is null
   * @throws ClassCastException if the type of any element in c is not
   *         a valid type for this set.
   * @throws NullPointerException if some element of c is null and this
   *         set doesn't support removing null values.
   * @see #remove(Object)
   */
  boolean removeAll(Collection<?> c);

  /**
   * Retains only the elements in this set that are also in the specified
   * collection (optional operation). If the argument is also a set, this
   * performs the <i>intersection</i> of the two sets.
   *
   * @param c the collection to keep
   * @return true if this set was modified
   * @throws UnsupportedOperationException if this operation is not allowed
   * @throws NullPointerException if c is null
   * @throws ClassCastException if the type of any element in c is not
   *         a valid type for this set.
   * @throws NullPointerException if some element of c is null and this
   *         set doesn't support retaining null values.
   * @see #remove(Object)
   */
  boolean retainAll(Collection<?> c);

  /**
   * Returns the number of elements in the set. If there are more
   * than Integer.MAX_VALUE mappings, return Integer.MAX_VALUE. This is
   * the <i>cardinality</i> of the set.
   *
   * @return the number of elements
   */
  int size();

  /**
   * Returns an array containing the elements of this set. If the set
   * makes a guarantee about iteration order, the array has the same
   * order. The array is distinct from the set; modifying one does not
   * affect the other.
   *
   * @return an array of this set's elements
   * @see #toArray(Object[])
   */
  Object[] toArray();

  /**
   * Returns an array containing the elements of this set, of the same runtime
   * type of the argument. If the given set is large enough, it is reused,
   * and null is inserted in the first unused slot. Otherwise, reflection
   * is used to build a new array. If the set makes a guarantee about iteration
   * order, the array has the same order. The array is distinct from the set;
   * modifying one does not affect the other.
   *
   * @param a the array to determine the return type; if it is big enough
   *        it is used and returned
   * @return an array holding the elements of the set
   * @throws ArrayStoreException if the runtime type of a is not a supertype
   *         of all elements in the set
   * @throws NullPointerException if a is null
   * @see #toArray()
   */
  <T> T[] toArray(T[] a);
}
