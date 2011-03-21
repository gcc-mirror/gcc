/* SimpleList.java -- simple way to make tuples.
   Copyright (C) 2004, 2006 Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.java.security.util;

import java.util.AbstractList;
import java.util.Collection;
import java.util.Iterator;

/**
 * A simple way to create immutable n-tuples. This class can be created with up
 * to four elements specified via one of the constructors, or with a collection
 * of arbitrary size.
 */
public final class SimpleList
    extends AbstractList
{
  private final Object[] elements;

  /**
   * Create a singleton list.
   *
   * @param element The first element.
   */
  public SimpleList(final Object element)
  {
    elements = new Object[1];
    elements[0] = element;
  }

  /**
   * Create an ordered pair (2-tuple).
   *
   * @param e1 The first element.
   * @param e2 The second element.
   */
  public SimpleList(final Object e1, final Object e2)
  {
    elements = new Object[2];
    elements[0] = e1;
    elements[1] = e2;
  }

  /**
   * Create a 3-tuple.
   *
   * @param e1 The first element.
   * @param e2 The second element.
   * @param e3 The third element.
   */
  public SimpleList(final Object e1, final Object e2, final Object e3)
  {
    elements = new Object[3];
    elements[0] = e1;
    elements[1] = e2;
    elements[2] = e3;
  }

  /**
   * Create a 4-tuple.
   *
   * @param e1 The first element.
   * @param e2 The second element.
   * @param e3 The third element.
   * @param e4 The fourth element.
   */
  public SimpleList(final Object e1, final Object e2, final Object e3,
                    final Object e4)
  {
    elements = new Object[4];
    elements[0] = e1;
    elements[1] = e2;
    elements[2] = e3;
    elements[3] = e4;
  }

  /**
   * Create the empty list.
   */
  public SimpleList()
  {
    elements = null;
  }

  /**
   * Create an n-tuple of arbitrary size. Even if the supplied collection has no
   * natural order, the created n-tuple will have the order that the elements
   * are returned by the collection's iterator.
   *
   * @param c The collection.
   */
  public SimpleList(Collection c)
  {
    elements = new Object[c.size()];
    int i = 0;
    for (Iterator it = c.iterator(); it.hasNext() && i < elements.length;)
      elements[i++] = it.next();
  }

  public int size()
  {
    if (elements == null)
      return 0;
    return elements.length;
  }

  public Object get(int index)
  {
    if (elements == null)
      throw new IndexOutOfBoundsException("list is empty");
    if (index < 0 || index >= elements.length)
      throw new IndexOutOfBoundsException("index=" + index + ", size=" + size());
    return elements[index];
  }

  public String toString()
  {
    return SimpleList.class.getName() + "(" + size() + ") " + super.toString();
  }
}
