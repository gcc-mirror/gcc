/* AbstractSet.java -- Abstract implementation of most of Set
   Copyright (C) 1998, 2000, 2001, 2005  Free Software Foundation, Inc.

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
 * An abstract implementation of Set to make it easier to create your own
 * implementations. In order to create a Set, subclass AbstractSet and
 * implement the same methods that are required for AbstractCollection
 * (although these methods must of course meet the requirements that Set puts
 * on them - specifically, no element may be in the set more than once). This
 * class simply provides implementations of equals() and hashCode() to fulfil
 * the requirements placed on them by the Set interface.
 *
 * @author Original author unknown
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Collection
 * @see AbstractCollection
 * @see Set
 * @see HashSet
 * @see TreeSet
 * @see LinkedHashSet
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class AbstractSet extends AbstractCollection implements Set
{
  /**
   * The main constructor, for use by subclasses.
   */
  protected AbstractSet()
  {
  }

  /**
   * Tests whether the given object is equal to this Set. This implementation
   * first checks whether this set <em>is</em> the given object, and returns
   * true if so. Otherwise, if o is a Set and is the same size as this one, it
   * returns the result of calling containsAll on the given Set. Otherwise, it
   * returns false.
   *
   * @param o the Object to be tested for equality with this Set
   * @return true if the given object is equal to this Set
   */
  public boolean equals(Object o)
  {
    return (o == this ||
            (o instanceof Set && ((Set) o).size() == size()
             && containsAll((Collection) o)));
  }

  /**
   * Returns a hash code for this Set. The hash code of a Set is the sum of the
   * hash codes of all its elements, except that the hash code of null is
   * defined to be zero. This implementation obtains an Iterator over the Set,
   * and sums the results.
   *
   * @return a hash code for this Set
   */
  public int hashCode()
  {
    Iterator itr = iterator();
    int hash = 0;
    int pos = size();
    while (--pos >= 0)
      hash += hashCode(itr.next());
    return hash;
  }

  /**
   * Removes from this set all elements in the given collection (optional
   * operation). This implementation uses <code>size()</code> to determine
   * the smaller collection.  Then, if this set is smaller, it iterates
   * over the set, calling Iterator.remove if the collection contains
   * the element.  If this set is larger, it iterates over the collection,
   * calling Set.remove for all elements in the collection. Note that
   * this operation will fail if a remove methods is not supported.
   *
   * @param c the collection of elements to remove
   * @return true if the set was modified as a result
   * @throws UnsupportedOperationException if remove is not supported
   * @throws NullPointerException if the collection is null
   * @see AbstractCollection#remove(Object)
   * @see Collection#contains(Object)
   * @see Iterator#remove()
   */
  public boolean removeAll(Collection c)
  {
    int oldsize = size();
    int count = c.size();
    Iterator i;
    if (oldsize < count)
      {
	for (i = iterator(), count = oldsize; count > 0; count--)
          if (c.contains(i.next()))
            i.remove();
      }
    else
      for (i = c.iterator(); count > 0; count--)
        remove(i.next());
    return oldsize != size();
  }

}
