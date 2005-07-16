/* Collection.java -- Interface that represents a collection of objects
   Copyright (C) 1998, 2001, 2005  Free Software Foundation, Inc.

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
 * Interface that represents a collection of objects. This interface is the
 * root of the collection hierarchy, and does not provide any guarantees about
 * the order of its elements or whether or not duplicate elements are
 * permitted.
 * <p>
 * All methods of this interface that are defined to modify the collection are
 * defined as <dfn>optional</dfn>. An optional operation may throw an
 * UnsupportedOperationException if the data backing this collection does not
 * support such a modification. This may mean that the data structure is
 * immutable, or that it is read-only but may change ("unmodifiable"), or
 * that it is modifiable but of fixed size (such as an array), or any number
 * of other combinations.
 * <p>
 * A class that wishes to implement this interface should consider subclassing
 * AbstractCollection, which provides basic implementations of most of the
 * methods of this interface. Classes that are prepared to make guarantees
 * about ordering or about absence of duplicate elements should consider
 * implementing List or Set respectively, both of which are subinterfaces of
 * Collection.
 * <p>
 * A general-purpose implementation of the Collection interface should in most
 * cases provide at least two constructors: One which takes no arguments and
 * creates an empty collection, and one which takes a Collection as an argument
 * and returns a collection containing the same elements (that is, creates a
 * copy of the argument using its own implementation).
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see List
 * @see Set
 * @see Map
 * @see SortedSet
 * @see SortedMap
 * @see HashSet
 * @see TreeSet
 * @see ArrayList
 * @see LinkedList
 * @see Vector
 * @see Collections
 * @see Arrays
 * @see AbstractCollection
 * @since 1.2
 * @status updated to 1.4
 */
public interface Collection
{
  /**
   * Add an element to this collection.
   *
   * @param o the object to add.
   * @return true if the collection was modified as a result of this action.
   * @throws UnsupportedOperationException if this collection does not
   *   support the add operation.
   * @throws ClassCastException if o cannot be added to this collection due
   *   to its type.
   * @throws NullPointerException if o is null and this collection doesn't
   *   support the addition of null values.
   * @throws IllegalArgumentException if o cannot be added to this
   *   collection for some other reason.
   */
  boolean add(Object o);

  /**
   * Add the contents of a given collection to this collection.
   *
   * @param c the collection to add.
   * @return true if the collection was modified as a result of this action.
   * @throws UnsupportedOperationException if this collection does not
   *   support the addAll operation.
   * @throws ClassCastException if some element of c cannot be added to this
   *   collection due to its type.
   * @throws NullPointerException if some element of c is null and this
   *   collection does not support the addition of null values.
   * @throws NullPointerException if c itself is null.
   * @throws IllegalArgumentException if some element of c cannot be added
   *   to this collection for some other reason.
   */
  boolean addAll(Collection c);

  /**
   * Clear the collection, such that a subsequent call to isEmpty() would
   * return true.
   *
   * @throws UnsupportedOperationException if this collection does not
   *   support the clear operation.
   */
  void clear();

  /**
   * Test whether this collection contains a given object as one of its
   * elements.
   *
   * @param o the element to look for.
   * @return true if this collection contains at least one element e such that
   *   <code>o == null ? e == null : o.equals(e)</code>.
   * @throws ClassCastException if the type of o is not a valid type for this
   *   collection.
   * @throws NullPointerException if o is null and this collection doesn't
   *   support null values.
   */
  boolean contains(Object o);

  /**
   * Test whether this collection contains every element in a given collection.
   *
   * @param c the collection to test for.
   * @return true if for every element o in c, contains(o) would return true.
   * @throws ClassCastException if the type of any element in c is not a valid
   *   type for this collection.
   * @throws NullPointerException if some element of c is null and this
   *   collection does not support null values.
   * @throws NullPointerException if c itself is null.
   */
  boolean containsAll(Collection c);

  /**
   * Test whether this collection is equal to some object. The Collection
   * interface does not explicitly require any behaviour from this method, and
   * it may be left to the default implementation provided by Object. The Set
   * and List interfaces do, however, require specific behaviour from this
   * method.
   * <p>
   * If an implementation of Collection, which is not also an implementation of
   * Set or List, should choose to implement this method, it should take care
   * to obey the contract of the equals method of Object. In particular, care
   * should be taken to return false when o is a Set or a List, in order to
   * preserve the symmetry of the relation.
   *
   * @param o the object to compare to this collection.
   * @return true if the o is equal to this collection.
   */
  boolean equals(Object o);

  /**
   * Obtain a hash code for this collection. The Collection interface does not
   * explicitly require any behaviour from this method, and it may be left to
   * the default implementation provided by Object. The Set and List interfaces
   * do, however, require specific behaviour from this method.
   * <p>
   * If an implementation of Collection, which is not also an implementation of
   * Set or List, should choose to implement this method, it should take care
   * to obey the contract of the hashCode method of Object. Note that this
   * method renders it impossible to correctly implement both Set and List, as
   * the required implementations are mutually exclusive.
   *
   * @return a hash code for this collection.
   */
  int hashCode();

  /**
   * Test whether this collection is empty, that is, if size() == 0.
   *
   * @return true if this collection contains no elements.
   */
  boolean isEmpty();

  /**
   * Obtain an Iterator over this collection.
   *
   * @return an Iterator over the elements of this collection, in any order.
   */
  Iterator iterator();

  /**
   * Remove a single occurrence of an object from this collection. That is,
   * remove an element e, if one exists, such that <code>o == null ? e == null
   *   : o.equals(e)</code>.
   *
   * @param o the object to remove.
   * @return true if the collection changed as a result of this call, that is,
   *   if the collection contained at least one occurrence of o.
   * @throws UnsupportedOperationException if this collection does not
   *   support the remove operation.
   * @throws ClassCastException if the type of o is not a valid type
   *   for this collection.
   * @throws NullPointerException if o is null and the collection doesn't
   *   support null values.
   */
  boolean remove(Object o);

  /**
   * Remove all elements of a given collection from this collection. That is,
   * remove every element e such that c.contains(e).
   *
   * @param c The collection of objects to be removed.
   * @return true if this collection was modified as a result of this call.
   * @throws UnsupportedOperationException if this collection does not
   *   support the removeAll operation.
   * @throws ClassCastException if the type of any element in c is not a valid
   *   type for this collection.
   * @throws NullPointerException if some element of c is null and this
   *   collection does not support removing null values.
   * @throws NullPointerException if c itself is null.
   */
  boolean removeAll(Collection c);

  /**
   * Remove all elements of this collection that are not contained in a given
   * collection. That is, remove every element e such that !c.contains(e).
   *
   * @param c The collection of objects to be retained.
   * @return true if this collection was modified as a result of this call.
   * @throws UnsupportedOperationException if this collection does not
   *   support the retainAll operation.
   * @throws ClassCastException if the type of any element in c is not a valid
   *   type for this collection.
   * @throws NullPointerException if some element of c is null and this
   *   collection does not support retaining null values.
   * @throws NullPointerException if c itself is null.
   */
  boolean retainAll(Collection c);

  /**
   * Get the number of elements in this collection.
   *
   * @return the number of elements in the collection.
   */
  int size();

  /**
   * Copy the current contents of this collection into an array.
   *
   * @return an array of type Object[] and length equal to the size of this
   *   collection, containing the elements currently in this collection, in
   *   any order.
   */
  Object[] toArray();

  /**
   * Copy the current contents of this collection into an array. If the array
   * passed as an argument has length less than the size of this collection, an
   * array of the same run-time type as a, and length equal to the size of this
   * collection, is allocated using Reflection. Otherwise, a itself is used.
   * The elements of this collection are copied into it, and if there is space
   * in the array, the following element is set to null. The resultant array is
   * returned.
   * Note: The fact that the following element is set to null is only useful
   * if it is known that this collection does not contain any null elements.
   *
   * @param a the array to copy this collection into.
   * @return an array containing the elements currently in this collection, in
   *   any order.
   * @throws ArrayStoreException if the type of any element of the
   *   collection is not a subtype of the element type of a.
   */
  Object[] toArray(Object[] a);
}
