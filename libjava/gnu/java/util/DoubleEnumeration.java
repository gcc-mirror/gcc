/* gnu.java.util.DoubleEnumeration
   Copyright (C) 1998, 1999, 2001 Free Software Foundation, Inc.

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

import java.util.Enumeration;
import java.util.NoSuchElementException;


/**
 * This is a helper class that combines two Enumerations.
 * It returns the elements of the first Enumeration until it has
 * no more elements and then returns the elements of the second
 * Enumeration.<br>
 * 
 * In the default case:
 * <pre>
 * doubleEnum = new DoubleEnumeration(enum1, enum2);
 * while (doubleEnum.hasMoreElements()) {
 *    Object o = doubleEnum.nextElement();
 *    do_something(o);
 * }
 * </pre>
 * it calls hasMoreElements of the Enumerations as few times as
 * possible.
 * The references to the Enumerations are cleared as soon as they have no
 * more elements to help garbage collecting.
 *
 * @author Jochen Hoenicke
 * @author Mark Wielaard (mark@klomp.org)
 */
public class DoubleEnumeration implements Enumeration
{
  /**
   * This is true as long as one of the enumerations has more
   * elements.  
   * Only valid when hasChecked is true.
   * Set in <code>hasMoreElements()</code>
   */
  private boolean hasMore;
  /**
   * This is true, if it is sure that hasMore indicates wether there are
   * more elements.
   * Set to true in <code>hasMoreElements()</code>.
   * Set to false in <code>getNextElement()</code>.
   */
  private boolean hasChecked;
  /**
   * The first enumeration.
   */
  private Enumeration e1;
  /**
   * The second enumeration.
   */
  private Enumeration e2;

  /**
   * Creates a new Enumeration combining the given two enumerations.
   * The enumerations mustn't be accessed by other classes.
   */
  public DoubleEnumeration(Enumeration e1, Enumeration e2)
  {
    this.e1 = e1;
    this.e2 = e2;
    hasChecked = false;
  }

  /**
   * Returns true, if at least one of the two enumerations has more
   * elements.
   */
  public boolean hasMoreElements()
  {
    if (hasChecked)
      return hasMore;

    hasMore = (e1 != null && e1.hasMoreElements());

    if (!hasMore) {
      e1 = e2;
      e2 = null;
      hasMore = (e1 != null && e1.hasMoreElements());
    }

    hasChecked = true;
    return hasMore;
  }

  /**
   * Returns the next element.  This returns the next element of the
   * first enumeration, if it has more elements, otherwise the next
   * element of the second enumeration. If both enumeration don't have
   * any elements it throws a <code>NoSuchElementException</code>.
   */
  public Object nextElement()
  {
    if (!hasMoreElements())
      throw new NoSuchElementException();
    else {
      hasChecked = false;
      return e1.nextElement();
    }
  }
}
