/* AttributeList.java -- A list of MBean attributes.
   Copyright (C) 2006 Free Software Foundation, Inc.

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

package javax.management;

import java.util.ArrayList;

/**
 * Represents a list of MBean {@link Attribute}s, with their
 * names and values.  This is implemented as an
 * {@link java.util.ArrayList} extension, with additional
 * methods typed to only allow the addition of {@link Attribute}s.
 *
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 * @since 1.5
 */
public class AttributeList
  extends ArrayList<Object>
{

  /**
   * Compatible with JDK 1.5
   */
  private static final long serialVersionUID = -4077085769279709076L;

  /**
   * Constructs an empty list with an initial capacity of ten.
   *
   * @see java.util.ArrayList#ArrayList()
   */
  public AttributeList()
  {
    super();
  }

  /**
   * Constructs an {@link AttributeList} using the contents
   * of an existing list.  The initial capacity is 110% of the
   * size of the specified list.
   *
   * @param list the list to use to fill this list.
   * @see java.util.ArrayList#ArrayList(java.util.Collection)
   */
  public AttributeList(AttributeList list)
  {
    super(list);
  }

  /**
   * Constructs an empty list with the specified initial capacity.
   *
   * @param capacity the initial capacity of the list.
   * @see java.util.ArrayList#ArrayList(int)
   */
  public AttributeList(int capacity)
  {
    super(capacity);
  }

  /**
   * Adds the specified {@link Attribute} to the end of the list.
   *
   * @param attribute the attribute to add.
   * @see java.util.Arraylist#add(Object)
   */
  public void add(Attribute attribute)
  {
    super.add(attribute);
  }

  /**
   * <p>
   * Adds the specified {@link Attribute} at the supplied index.
   * Any attribute already at that index is moved up one place
   * in the list to the position <code>(index + 1)</code>.
   * Likewise, the attribute at <code>(index + 1)</code> is
   * also moved up one place, continuing until the final
   * attribute in the list moves to a new position, increasing
   * the size of the list.
   * </p>
   * <p>
   * If the index is invalid (i.e. it is smaller than zero, or
   * greater than the current size of the list), a
   * @link{RuntimeOperationsException} is thrown, which wraps
   * the @link{IndexOutOfBoundsException} from the underlying
   * array list.
   * </p>
   *
   * @param index the index at which to place the new attribute.
   * @param attribute the new attribute to add.
   * @throws RuntimeOperationsException if <code>index < 0</code>
   *                                    or <code>index > size()</code>
   * @see java.util.ArrayList#add(int, Object)
   */
  public void add(int index, Attribute attribute)
  {
    try
      {
        super.add(index, attribute);
      }
    catch (IndexOutOfBoundsException e)
      {
        throw new RuntimeOperationsException(e, "Invalid index.");
      }
  }

  /**
   * Adds all the {@link Attribute}s from the supplied list
   * to the end of this list, in the order they are returned
   * by the list's {@link java.util.Iterator}.
   *
   * @param list the list of attributes to add.
   * @return true if the list changed.
   * @see java.util.ArrayList#addAll(Collection)
   */
  public boolean addAll(AttributeList list)
  {
    return super.addAll(list);
  }

  /**
   * <p>
   * Adds all the {@link Attribute}s from the supplied list
   * to this list, at the specified index.  The attributes
   * are added in the order they are returned by the
   * list's {@link java.util.Iterator}.  Any attribute already
   * at that index is moved up one place in the list to the
   * position <code>(index + list.size())</code>.
   * Likewise, the attribute at <code>(index + list.size())</code>
   * is also moved up one place, continuing until the final
   * attribute in the original list.
   * </p>
   * <p>
   * If the index is invalid (i.e. it is smaller than zero, or
   * greater than the current size of the list), a
   * @link{RuntimeOperationsException} is thrown, which wraps
   * the @link{IndexOutOfBoundsException} from the underlying
   * array list.
   * </p>
   *
   * @param index the index at which to place the new attribute.
   * @param list the list of attributes to add.
   * @return true if the list changed.
   * @throws RuntimeOperationsException if <code>index < 0</code>
   *                                    or <code>index > size()</code>
   * @see java.util.ArrayList#addAll(int, Collection)
   */
  public boolean addAll(int index, AttributeList list)
  {
    try
      {
        return super.addAll(index, list);
      }
    catch (IndexOutOfBoundsException e)
      {
        throw new RuntimeOperationsException(e, "Invalid index.");
      }
  }

  /**
   * Replaces the attribute at the specified index with the one
   * supplied. If the index is invalid (i.e. it is smaller than
   * zero, or greater than the current size of the list), a
   * @link{RuntimeOperationsException} is thrown, which wraps
   * the @link{IndexOutOfBoundsException} from the underlying
   * array list.
   *
   * @param index the index at which to place the new attribute.
   * @param attribute the new attribute to add.
   * @throws RuntimeOperationsException if <code>index < 0</code>
   *                                    or <code>index > size()</code>
   * @see java.util.ArrayList#set(int, Object)
   */
  public void set(int index, Attribute attribute)
  {
    try
      {
        super.set(index, attribute);
      }
    catch (IndexOutOfBoundsException e)
      {
        throw new RuntimeOperationsException(e, "Invalid index.");
      }
  }

}
