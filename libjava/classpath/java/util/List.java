/* List.java -- An ordered collection which allows indexed access
   Copyright (C) 1998, 2001, 2004, 2005 Free Software Foundation, Inc.

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
 * An ordered collection (also known as a list). This collection allows
 * access to elements by position, as well as control on where elements
 * are inserted. Unlike sets, duplicate elements are permitted by this
 * general contract (if a subclass forbids duplicates, this should be
 * documented).
 * <p>
 *
 * List places additional requirements on <code>iterator</code>,
 * <code>add</code>, <code>remove</code>, <code>equals</code>, and
 * <code>hashCode</code>, in addition to requiring more methods. List
 * indexing is 0-based (like arrays), although some implementations may
 * require time proportional to the index to obtain an arbitrary element.
 * The List interface is incompatible with Set; you cannot implement both
 * simultaneously.
 * <p>
 *
 * Lists also provide a <code>ListIterator</code> which allows bidirectional
 * traversal and other features atop regular iterators. Lists can be
 * searched for arbitrary elements, and allow easy insertion and removal
 * of multiple elements in one method call.
 * <p>
 *
 * Note: While lists may contain themselves as elements, this leads to
 * undefined (usually infinite recursive) behavior for some methods like
 * hashCode or equals.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see Set
 * @see ArrayList
 * @see LinkedList
 * @see Vector
 * @see Arrays#asList(Object[])
 * @see Collections#nCopies(int, Object)
 * @see Collections#EMPTY_LIST
 * @see AbstractList
 * @see AbstractSequentialList
 * @since 1.2
 * @status updated to 1.4
 */
public interface List<E> extends Collection<E>
{
  /**
   * Insert an element into the list at a given position (optional operation).
   * This shifts all existing elements from that position to the end one
   * index to the right. This version of add has no return, since it is
   * assumed to always succeed if there is no exception.
   *
   * @param index the location to insert the item
   * @param o the object to insert
   * @throws UnsupportedOperationException if this list does not support the
   *         add operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and this list doesn't support
   *         the addition of null values.
   */
  void add(int index, E o);

  /**
   * Add an element to the end of the list (optional operation). If the list
   * imposes restraints on what can be inserted, such as no null elements,
   * this should be documented.
   *
   * @param o the object to add
   * @return true, as defined by Collection for a modified list
   * @throws UnsupportedOperationException if this list does not support the
   *         add operation
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and this list doesn't support
   *         the addition of null values.
   */
  boolean add(E o);

  /**
   * Insert the contents of a collection into the list at a given position
   * (optional operation). Shift all elements at that position to the right
   * by the number of elements inserted. This operation is undefined if
   * this list is modified during the operation (for example, if you try
   * to insert a list into itself).
   *
   * @param index the location to insert the collection
   * @param c the collection to insert
   * @return true if the list was modified by this action, that is, if c is
   *         non-empty
   * @throws UnsupportedOperationException if this list does not support the
   *         addAll operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   * @throws ClassCastException if some element of c cannot be added to this
   *         list due to its type
   * @throws IllegalArgumentException if some element of c cannot be added
   *         to this list for some other reason
   * @throws NullPointerException if some element of c is null and this list
   *         doesn't support the addition of null values.
   * @throws NullPointerException if the specified collection is null
   * @see #add(int, Object)
   */
  boolean addAll(int index, Collection<? extends E> c);

  /**
   * Add the contents of a collection to the end of the list (optional
   * operation).  This operation is undefined if this list is modified
   * during the operation (for example, if you try to insert a list into
   * itself).
   *
   * @param c the collection to add
   * @return true if the list was modified by this action, that is, if c is
   *         non-empty
   * @throws UnsupportedOperationException if this list does not support the
   *         addAll operation
   * @throws ClassCastException if some element of c cannot be added to this
   *         list due to its type
   * @throws IllegalArgumentException if some element of c cannot be added
   *         to this list for some other reason
   * @throws NullPointerException if the specified collection is null
   * @throws NullPointerException if some element of c is null and this list
   *         doesn't support the addition of null values.
   * @see #add(Object)
   */
  boolean addAll(Collection<? extends E> c);

  /**
   * Clear the list, such that a subsequent call to isEmpty() would return
   * true (optional operation).
   *
   * @throws UnsupportedOperationException if this list does not support the
   *         clear operation
   */
  void clear();

  /**
   * Test whether this list contains a given object as one of its elements.
   * This is defined as the existence of an element e such that
   * <code>o == null ? e == null : o.equals(e)</code>.
   *
   * @param o the element to look for
   * @return true if this list contains the element
   * @throws ClassCastException if the type of o is not a valid type
   *         for this list.
   * @throws NullPointerException if o is null and the list doesn't
   *         support null values.
   */
  boolean contains(Object o);

  /**
   * Test whether this list contains every element in a given collection.
   *
   * @param c the collection to test for
   * @return true if for every element o in c, contains(o) would return true
   * @throws NullPointerException if the collection is null
   * @throws ClassCastException if the type of any element in c is not a valid
   *         type for this list.
   * @throws NullPointerException if some element of c is null and this
   *         list does not support null values.
   * @see #contains(Object)
   */
  boolean containsAll(Collection<?> c);

  /**
   * Test whether this list is equal to another object. A List is defined to be
   * equal to an object if and only if that object is also a List, and the two
   * lists have the same sequence. Two lists l1 and l2 are equal if and only
   * if <code>l1.size() == l2.size()</code>, and for every integer n between 0
   * and <code>l1.size() - 1</code> inclusive, <code>l1.get(n) == null ?
   * l2.get(n) == null : l1.get(n).equals(l2.get(n))</code>.
   *
   * @param o the object to test for equality with this list
   * @return true if o is equal to this list
   * @see Object#equals(Object)
   * @see #hashCode()
   */
  boolean equals(Object o);

  /**
   * Get the element at a given index in this list.
   *
   * @param index the index of the element to be returned
   * @return the element at index index in this list
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   */
  E get(int index);

  /**
   * Obtains a hash code for this list. In order to obey the general
   * contract of the hashCode method of class Object, this value is
   * calculated as follows:
   * 
<p><pre>hashCode = 1;
Iterator i = list.iterator();
while (i.hasNext())
{
  Object obj = i.next();
  hashCode = 31 * hashCode + (obj == null ? 0 : obj.hashCode());
}</pre>
   *
   * <p>This ensures that the general contract of Object.hashCode()
   * is adhered to.
   *
   * @return the hash code of this list
   * @see Object#hashCode()
   * @see #equals(Object)
   */
  int hashCode();

  /**
   * Obtain the first index at which a given object is to be found in this
   * list.
   *
   * @param o the object to search for
   * @return the least integer n such that <code>o == null ? get(n) == null :
   *         o.equals(get(n))</code>, or -1 if there is no such index.
   * @throws ClassCastException if the type of o is not a valid
   *         type for this list.
   * @throws NullPointerException if o is null and this
   *         list does not support null values.
   */
  int indexOf(Object o);

  /**
   * Test whether this list is empty, that is, if size() == 0.
   *
   * @return true if this list contains no elements
   */
  boolean isEmpty();

  /**
   * Obtain an Iterator over this list, whose sequence is the list order.
   *
   * @return an Iterator over the elements of this list, in order
   */
  Iterator<E> iterator();

  /**
   * Obtain the last index at which a given object is to be found in this
   * list.
   *
   * @return the greatest integer n such that <code>o == null ? get(n) == null
   *         : o.equals(get(n))</code>, or -1 if there is no such index.
   * @throws ClassCastException if the type of o is not a valid
   *         type for this list.
   * @throws NullPointerException if o is null and this
   *         list does not support null values.
   */
  int lastIndexOf(Object o);

  /**
   * Obtain a ListIterator over this list, starting at the beginning.
   *
   * @return a ListIterator over the elements of this list, in order, starting
   *         at the beginning
   */
  ListIterator<E> listIterator();

  /**
   * Obtain a ListIterator over this list, starting at a given position.
   * A first call to next() would return the same as get(index), and a
   * first call to previous() would return the same as get(index - 1).
   *
   * @param index the position, between 0 and size() inclusive, to begin the
   *        iteration from
   * @return a ListIterator over the elements of this list, in order, starting
   *         at index
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt; size()
   */
  ListIterator<E> listIterator(int index);

  /**
   * Remove the element at a given position in this list (optional operation).
   * Shifts all remaining elements to the left to fill the gap.
   *
   * @param index the position within the list of the object to remove
   * @return the object that was removed
   * @throws UnsupportedOperationException if this list does not support the
   *         remove operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   */
  E remove(int index);

  /**
   * Remove the first occurence of an object from this list (optional
   * operation). That is, remove the first element e such that
   * <code>o == null ? e == null : o.equals(e)</code>.
   *
   * @param o the object to remove
   * @return true if the list changed as a result of this call, that is, if
   *         the list contained at least one occurrence of o
   * @throws UnsupportedOperationException if this list does not support the
   *         remove operation
   * @throws ClassCastException if the type of o is not a valid
   *         type for this list.
   * @throws NullPointerException if o is null and this
   *         list does not support removing null values.
   */
  boolean remove(Object o);

  /**
   * Remove all elements of a given collection from this list (optional
   * operation). That is, remove every element e such that c.contains(e).
   *
   * @param c the collection to filter out
   * @return true if this list was modified as a result of this call
   * @throws UnsupportedOperationException if this list does not support the
   *         removeAll operation
   * @throws NullPointerException if the collection is null
   * @throws ClassCastException if the type of any element in c is not a valid
   *         type for this list.
   * @throws NullPointerException if some element of c is null and this
   *         list does not support removing null values.
   * @see #remove(Object)
   * @see #contains(Object)
   */
  boolean removeAll(Collection<?> c);

  /**
   * Remove all elements of this list that are not contained in a given
   * collection (optional operation). That is, remove every element e such
   * that !c.contains(e).
   *
   * @param c the collection to retain
   * @return true if this list was modified as a result of this call
   * @throws UnsupportedOperationException if this list does not support the
   *         retainAll operation
   * @throws NullPointerException if the collection is null
   * @throws ClassCastException if the type of any element in c is not a valid
   *         type for this list.
   * @throws NullPointerException if some element of c is null and this
   *         list does not support retaining null values.
   * @see #remove(Object)
   * @see #contains(Object)
   */
  boolean retainAll(Collection<?> c);

  /**
   * Replace an element of this list with another object (optional operation).
   *
   * @param index the position within this list of the element to be replaced
   * @param o the object to replace it with
   * @return the object that was replaced
   * @throws UnsupportedOperationException if this list does not support the
   *         set operation
   * @throws IndexOutOfBoundsException if index &lt; 0 || index &gt;= size()
   * @throws ClassCastException if o cannot be added to this list due to its
   *         type
   * @throws IllegalArgumentException if o cannot be added to this list for
   *         some other reason
   * @throws NullPointerException if o is null and this
   *         list does not support null values.
   */
  E set(int index, E o);

  /**
   * Get the number of elements in this list. If the list contains more
   * than Integer.MAX_VALUE elements, return Integer.MAX_VALUE.
   *
   * @return the number of elements in the list
   */
  int size();

  /**
   * Obtain a List view of a subsection of this list, from fromIndex
   * (inclusive) to toIndex (exclusive). If the two indices are equal, the
   * sublist is empty. The returned list should be modifiable if and only
   * if this list is modifiable. Changes to the returned list should be
   * reflected in this list. If this list is structurally modified in
   * any way other than through the returned list, the result of any subsequent
   * operations on the returned list is undefined.
   *
   * @param fromIndex the index that the returned list should start from
   *        (inclusive)
   * @param toIndex the index that the returned list should go to (exclusive)
   * @return a List backed by a subsection of this list
   * @throws IndexOutOfBoundsException if fromIndex &lt; 0
   *         || toIndex &gt; size() || fromIndex &gt; toIndex
   */
  List<E> subList(int fromIndex, int toIndex);

  /**
   * Copy the current contents of this list into an array.
   *
   * @return an array of type Object[] and length equal to the length of this
   *         list, containing the elements currently in this list, in order
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
   * @param a the array to copy this list into
   * @return an array containing the elements currently in this list, in
   *         order
   * @throws ArrayStoreException if the type of any element of the
   *         collection is not a subtype of the element type of a
   * @throws NullPointerException if the specified array is null
   */
  <T> T[] toArray(T[] a);
}
