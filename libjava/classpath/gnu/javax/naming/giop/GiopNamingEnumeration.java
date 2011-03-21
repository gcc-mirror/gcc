/* GiopNamingEnumeration.java -- handles corbaname: urls
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


package gnu.javax.naming.giop;

import java.util.NoSuchElementException;

import javax.naming.NamingEnumeration;
import javax.naming.NamingException;

import org.omg.CosNaming.Binding;
import org.omg.CosNaming.BindingIterator;
import org.omg.CosNaming.BindingIteratorHolder;
import org.omg.CosNaming.BindingListHolder;

/**
 * Iterates over name class pairs, obtaining values first from the binding list
 * and then from the binding iterator.
 *
 * @author Audrius Meskauskas
 */
public abstract class GiopNamingEnumeration implements NamingEnumeration
{
  /**
   * The array of bindings, returned at once.
   */
  Binding[] list;

  /**
   * The binding iterator to obtain the subsequent bindings. May be null,
   * if all values are stored in the <code>list</code>.
   */
  BindingIterator iterator;

  /**
   * The batch size.
   */
  int batch;

  /**
   * The position of the element in the binding list, that must be returned
   * during the subsequent call of the next(). If this field is grater or equal
   * to the lenght of the list, the subsequent values must be requested from the
   * iterator.
   */
  int p;

  GiopNamingEnumeration(BindingListHolder bh, BindingIteratorHolder bih, int batchSize)
  {
    list = bh.value;
    iterator = bih.value;
    batch = batchSize;
  }

  /**
   * Convert from the CORBA binding into that this enumeration should return.
   *
   * @param binding
   *          the binding to convert
   * @return the value, that must be returned by the {@link #next()}.
   */
  public abstract Object convert(Binding binding);

  public void close() throws NamingException
  {
    if (iterator != null)
      {
        iterator.destroy();
        iterator = null;
      }
  }

  /**
   * Checks if there are more elements to return.
   *
   * @throws NamingException
   *           never
   */
  public boolean hasMore() throws NamingException
  {
    return hasMoreElements();
  }

  /**
   * Returns the next element.
   *
   * @throws NamingException
   *           never
   */
  public Object next() throws NamingException
  {
    return nextElement();
  }

  /**
   * Checks if there are more elements to return.
   */
  public boolean hasMoreElements()
  {
    if (p < 0)
      return false;
    else if (p < list.length)
      return true;
    else
      return getMore();
  }

  /**
   * Returns the next element.
   */
  public Object nextElement()
  {
    if (p < 0)
      throw new NoSuchElementException();
    else if (p < list.length)
      return convert(list[p++]);
    else if (getMore())
      // getMore updates p
      return convert(list[p++]);
    else
      throw new NoSuchElementException();
  }

  /**
   * Tries to obtain more elements, return true on success. Updates the fields
   * accordingly.
   */
  boolean getMore()
  {
    if (iterator != null)
      {
        BindingListHolder holder = new BindingListHolder();
        boolean rt = iterator.next_n(batch, holder);
        if (rt)
          {
            // The new pack of the bindings arrived.
            p = 0;
            list = holder.value;
            return true;
          }
        else
          {
            iterator.destroy();
            iterator = null;
            p = -1;
            return false;
          }
      }
    else
      return false;
  }
}
