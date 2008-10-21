/* EmptyEnumeration.java -- a constant empty enumeration
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

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

package gnu.java.util;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.NoSuchElementException;

/**
 * This is a helper class that produces an empty Enumerations. There is only
 * one instance of this class that can be used whenever one needs a
 * non-null but empty enumeration. Using this class prevents multiple
 * small objects and inner classes. <code>getInstance()</code> returns
 * the only instance of this class. It can be shared by multiple objects and
 * threads.
 *
 * @author Mark Wielaard (mark@klomp.org)
 */
public final class EmptyEnumeration<T> implements Enumeration<T>, Serializable
{
  /** The only instance of this class */
  private static final EmptyEnumeration<Object> instance = 
    new EmptyEnumeration<Object>();

  /**
   * Returns an instance of this class for Object.
   * It can be shared by multiple objects and threads.
   *
   * @return the common empty enumeration
   */
  public static EmptyEnumeration<Object> getInstance()
  {
    return instance;
  }

  /**
   * Returns false, since there are no elements.
   *
   * @return false
   */
  public boolean hasMoreElements()
  {
    return false;
  }

  /**
   * Always throws <code>NoSuchElementException</code>, since it is empty.
   *
   * @throws NoSuchElementException this is empty
   */
  public T nextElement()
  {
    throw new NoSuchElementException();
  }
}
