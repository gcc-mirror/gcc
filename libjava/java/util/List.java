/* List.java -- An ordered collection which allows indexed access
   Copyright (C) 1998 Free Software Foundation, Inc.

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
// ~ Doc comment for the interface itself needs to be put into english.
// ~ Some more @see clauses might be nice.

package java.util;

/**
 * [This is what this doc comment will mention:
 * ~ Additional restrictions on some methods. Others included for completeness.
 * ~ ListIterator and what it can do
 * ~ Positional and iterated access
 * ~ search (but linear time)
 * ~ be careful when containing self as an element, because equals and hashCode
 *   loop.]
 */
public interface List extends Collection
{
  /**
   * Insert an element into the list at a given position.
   *
   * @param index the location to insert the item.
   * @param o the object to insert.
   * @exception UnsupportedOperationException if this list does not support the
   *   add operation.
   * @exception IndexOutOfBoundsException if index < 0 || index > size()
   * @exception ClassCastException if o cannot be added to this list due to its
   *   type.
   * @exception IllegalArgumentException if o cannot be added to this list for
   *   some other reason.
   */
  void add(int index, Object o);

  /**
   * Add an element to the end of the list.
   *
   * @param o the object to add.
   * @returns true, as Collection defines this method as returning true if the
   *   list was modified as a result of this action, and it always is for a
   *   list.
   * @exception UnsupportedOperationException if this list does not support the
   *   add operation.
   * @exception ClassCastException if o cannot be added to this list due to its
   *   type.
   * @exception IllegalArgumentException if o cannot be added to this list for
   *   some other reason.
   */
  boolean add(Object o);

  /**
   * Insert the contents of a collection into the list at a given position.
   *
   * @param index the location to insert the collection.
   * @param c the collection to insert.
   * @returns true if the list was modified by this action, that is, if c is
   *   non-empty.
   * @exception UnsupportedOperationException if this list does not support the
   *   addAll operation.
   * @exception IndexOutOfBoundsException if index < 0 || index > size()
   * @exception ClassCastException if some element of c cannot be added to this
   *   list due to its type.
   * @exception IllegalArgumentException if some element of c cannot be added
   *   to this list for some other reason.
   */
  boolean addAll(int index, Collection c);

  /**
   * Add the contents of a collection to the end of the list.
   *
   * @param c the collection to add.
   * @returns true if the list was modified by this action, that is, if c is
   *   non-empty.
   * @exception UnsupportedOperationException if this list does not support the
   *   addAll operation.
   * @exception ClassCastException if some element of c cannot be added to this
   *   list due to its type.
   * @exception IllegalArgumentException if some element of c cannot be added
   *   to this list for some other reason.
   */
  boolean addAll(Collection c);

  /**
   * Clear the list, such that a subsequent call to isEmpty() would return
   * true.
   *
   * @exception UnsupportedOperationException if this list does not support the
   *   clear operation.
   */
  void clear();

  /**
   * Test whether this list contains a given object as one of its elements.
   *
   * @param o the element to look for.
   * @returns true if this list contains an element e such that <code>o ==
   *   null ? e == null : o.equals(e)</code>.
   */
  boolean contains(Object o);

  /**
   * Test whether this list contains every element in a given collection.
   *
   * @param c the collection to test for.
   * @returns true if for every element o in c, contains(o) would return true.
   */
  boolean containsAll(Collection c);

  /**
   * Test whether this list is equal to another object. A List is defined to be
   * equal to an object if and only if that object is also a List, and the two
   * lists are equal. Two lists l1 and l2 are defined to be equal if and only
   * if <code>l1.size() == l2.size()</code>, and for every integer n between 0
   * and <code>l1.size() - 1</code> inclusive, <code>l1.get(n) == null ?
   * l2.get(n) == null : l1.get(n).equals(l2.get(n))</code>.
   *
   * @param o the object to test for equality with this list.
   * @returns true if o is equal to this list.
   */
  boolean equals(Object o);

  /**
   * Get the element at a given index in this list.
   *
   * @param index the index of the element to be returned.
   * @returns the element at index index in this list.
   * @exception IndexOutOfBoundsException if index < 0 || index >= size()
   */
  Object get(int index);

  /**
   * Obtain a hash code for this list. In order to obey the general contract of
   * the hashCode method of class Object, this value is calculated as follows:
   * <pre>
   *   hashCode = 1;
   *   Iterator i = list.iterator();
   *   while (i.hasNext()) {
   *     Object obj = i.next();
   *     hashCode = 31*hashCode + (obj==null ? 0 : obj.hashCode());
   *   }
   * </pre>
   * This ensures that the general contract of Object.hashCode() is adhered to.
   *
   * @returns the hash code of this list.
   */
  int hashCode();

  /**
   * Obtain the first index at which a given object is to be found in this
   * list.
   *
   * @returns the least integer n such that <code>o == null ? get(n) == null :
   *   o.equals(get(n))</code>, or -1 if there is no such index.
   */
  int indexOf(Object o);

  /**
   * Test whether this list is empty, that is, if size() == 0.
   *
   * @returns true if this list contains no elements.
   */
  boolean isEmpty();

  /**
   * Obtain an Iterator over this list.
   *
   * @returns an Iterator over the elements of this list, in order.
   */
  Iterator iterator();

  /**
   * Obtain the last index at which a given object is to be found in this
   * list.
   *
   * @returns the greatest integer n such that <code>o == null ? get(n) == null
   *   : o.equals(get(n))</code>.
   */
  int lastIndexOf(Object o);

  /**
   * Obtain a ListIterator over this list, starting at the beginning.
   *
   * @returns a ListIterator over the elements of this list, in order, starting
   *   at the beginning.
   */
  ListIterator listIterator();

  /**
   * Obtain a ListIterator over this list, starting at a given position.
   *
   * @param index the position, between 0 and size() inclusive, to begin the
   *   iteration from.
   * @returns a ListIterator over the elements of this list, in order, starting
   *   at index.
   * @exception IndexOutOfBoundsException if index < 0 || index > size()
   */
  ListIterator listIterator(int index);

  /**
   * Remove the element at a given position in this list.
   *
   * @param index the position within the list of the object to remove.
   * @returns the object that was removed.
   * @exception UnsupportedOperationException if this list does not support the
   *   remove operation.
   * @exception IndexOutOfBoundsException if index < 0 || index > size()
   */
  Object remove(int index);

  /**
   * Remove the first occurence of an object from this list. That is, remove
   * the first element e such that <code>o == null ? e == null :
   * o.equals(e)</code>.
   *
   * @param o the object to remove.
   * @returns true if the list changed as a result of this call, that is, if
   *   the list contained at least one occurrence of o.
   * @exception UnsupportedOperationException if this list does not support the
   *   remove operation.
   */
  boolean remove(Object o);

  /**
   * Remove all elements of a given collection from this list. That is, remove
   * every element e such that c.contains(e).
   *
   * @returns true if this list was modified as a result of this call.
   * @exception UnsupportedOperationException if this list does not support the
   *   removeAll operation.
   */
  boolean removeAll(Collection c);

  /**
   * Remove all elements of this list that are not contained in a given
   * collection. That is, remove every element e such that !c.contains(e).
   *
   * @returns true if this list was modified as a result of this call.
   * @exception UnsupportedOperationException if this list does not support the
   *   retainAll operation.
   */
  boolean retainAll(Collection c);

  /**
   * Replace an element of this list with another object.
   *
   * @param index the position within this list of the element to be replaced.
   * @param o the object to replace it with.
   * @returns the object that was replaced.
   * @exception UnsupportedOperationException if this list does not support the
   *   set operation.
   * @exception IndexOutOfBoundsException if index < 0 || index >= size()
   * @exception ClassCastException if o cannot be added to this list due to its
   *   type.
   * @exception IllegalArgumentException if o cannot be added to this list for
   *   some other reason.
   */
  Object set(int index, Object o);

  /**
   * Get the number of elements in this list.
   *
   * @returns the number of elements in the list.
   */
  int size();

  /**
   * Obtain a List view of a subsection of this list, from fromIndex
   * (inclusive) to toIndex (exclusive). The returned list should be modifiable
   * if and only if this list is modifiable. Changes to the returned list
   * should be reflected in this list. If this list is structurally modified in
   * any way other than through the returned list, the result of any subsequent
   * operations on the returned list is undefined.
   *
   * @param fromIndex the index that the returned list should start from
   *    (inclusive).
   * @param toIndex the index that the returned list should go to (exclusive).
   * @returns a List backed by a subsection of this list.
   * @exception IndexOutOfBoundsException if fromIndex < 0 || toIndex > size()
   *   || fromIndex > toIndex.
   */
  List subList(int fromIndex, int toIndex);

  /**
   * Copy the current contents of this list into an array.
   *
   * @returns an array of type Object[] and length equal to the length of this
   *   list, containing the elements currently in this list, in order.
   */
  Object[] toArray();

  /**
   * Copy the current contents of this list into an array. If the array passed
   * as an argument has length less than that of this list, an array of the
   * same run-time type as a, and length equal to the length of this list, is
   * allocated using Reflection. Otherwise, a itself is used. The elements of
   * this list are copied into it, and if there is space in the array, the
   * following element is set to null. The resultant array is returned.
   * Note: The fact that the following element is set to null is only useful
   * if it is known that this list does not contain any null elements.
   *
   * @param a the array to copy this list into.
   * @returns an array containing the elements currently in this list, in
   *    order.
   * @exception ArrayStoreException if the type of any element of the
   *   collection is not a subtype of the element type of a.
   */
  Object[] toArray(Object[] a);
}
