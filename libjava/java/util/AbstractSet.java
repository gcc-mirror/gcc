/* AbstractSet.java -- Abstract implementation of most of Set
   Copyright (C) 1998, 2000 Free Software Foundation, Inc.

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


package java.util;

/**
 * An abstract implementation of Set to make it easier to create your own
 * implementations. In order to create a Set, subclass AbstractSet and
 * implement the same methods that are required for AbstractCollection
 * (although these methods must of course meet the requirements that Set puts
 * on them - specifically, no element may be in the set more than once). This
 * class simply provides implementations of equals() and hashCode() to fulfil
 * the requirements placed on them by the Set interface.
 */
public abstract class AbstractSet extends AbstractCollection implements Set
{

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
    if (o == this)
      return true;
    else if (o instanceof Set && ((Set) o).size() == size())
      return containsAll((Collection) o);
    else
      return false;
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
    int size = size();
    int hash = 0;
    for (int pos = 0; pos < size; pos++)
      {
	Object obj = itr.next();
	if (obj != null)
	  hash += obj.hashCode();
      }
    return hash;
  }
}
