/* Insets.java -- information about a container border
   Copyright (C) 1999, 2000, 2002, 2005, 2006  Free Software Foundation, Inc.

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


package java.awt;

import java.io.Serializable;

/**
 * This class represents the "margin" or space around a container.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @status 
 */
public class Insets implements Cloneable, Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -2272572637695466749L;

  /**
   * The gap from the top.
   *
   * @serial the top inset
   */
  public int top;

  /**
   * The gap from the left.
   *
   * @serial the left inset
   */
  public int left;

  /**
   * The gap from the bottom.
   *
   * @serial the bottom inset
   */
  public int bottom;

  /**
   * The gap from the right.
   *
   * @serial the right inset
   */
  public int right;

  /**
   * Initializes a new instance of <code>Inset</code> with the specified
   * inset values.
   *
   * @param top the top inset
   * @param left the left inset
   * @param bottom the bottom inset
   * @param right the right inset
   */
  public Insets(int top, int left, int bottom, int right)
  {
    this.top = top;
    this.left = left;
    this.bottom = bottom;
    this.right = right;
  }

  /**
   * Set the contents of this Insets object to the specified values.
   *
   * @param top the top inset
   * @param left the left inset
   * @param bottom the bottom inset
   * @param right the right inset
   *
   * @since 1.5
   */
  public void set(int top, int left, int bottom, int right)
  {
    this.top = top;
    this.left = left;
    this.bottom = bottom;
    this.right = right;
  }

  /**
   * Tests whether this object is equal to the specified object. The other
   * object must be an instance of Insets with identical field values.
   *
   * @param obj the object to test against
   * @return true if the specified object is equal to this one
   *
   * @since 1.1
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof Insets))
      return false;
    Insets i = (Insets) obj;
    return top == i.top && bottom == i.bottom
      && left == i.left && right == i.right;
  }

  /**
   * Returns a hashcode for this instance. The formula is unspecified, but
   * appears to be <code>XXX what is it? </code>.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    // This can't be right...
    return top + bottom + left + right;
  }

  /**
   * Returns a string representation of this object, which will be non-null.
   *
   * @return a string representation of this object
   */
  public String toString()
  {
    return getClass().getName() + "[top=" + top + ",left=" + left
      + ",bottom=" + bottom + ",right=" + right + ']';
  }

  /**
   * Returns a copy of this object.
   *
   * @return a copy of this object
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // Impossible
      }
  }
} // class Insets
