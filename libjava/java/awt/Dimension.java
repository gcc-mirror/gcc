/* Dimension.java -- represents a 2-dimensional span
   Copyright (C) 1999, 2000, 2002 Free Software Foundation

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


package java.awt;

import java.awt.geom.Dimension2D;
import java.io.Serializable;

/**
 * This class holds a width and height value pair. This is used in plenty
 * of windowing classes, but also has geometric meaning.
 *
 * <p>It is valid for a dimension to have negative width or height; but it
 * is considered to have no area. Therefore, the behavior in various methods
 * is undefined in such a case.
 *
 * <p>There are some public fields; if you mess with them in an inconsistent
 * manner, it is your own fault when you get invalid results. Also, this
 * class is not threadsafe.
 *
 * @author Per Bothner <bothner@cygnus.com>
 * @author Aaron M. Renn <arenn@urbanophile.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @see Component
 * @see LayoutManager
 * @since 1.0
 * @status updated to 1.14
 */
public class Dimension extends Dimension2D implements Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = 4723952579491349524L;

  /**
   * The width of this object.
   *
   * @see #getSize()
   * @see #setSize(double, double)
   * @serial the width
   */
  public int width;

  /**
   * The height of this object.
   *
   * @see #getSize()
   * @see #setSize(double, double)
   * @serial the height
   */
  public int height;

  /**
   * Create a new Dimension with a width and height of zero.
   */
  public Dimension()
  {
  }

  /**
   * Create a new Dimension with width and height identical to that of the
   * specified dimension.
   *
   * @param d the Dimension to copy
   * @throws NullPointerException if d is null
   */
  public Dimension(Dimension d)
  {
    width = d.width;
    height = d.height;
  }

  /**
   * Create a new Dimension with the specified width and height.
   *
   * @param w the width of this object
   * @param h the height of this object
   */
  public Dimension(int w, int h)
  {
    width = w;
    height = h;
  }

  /**
   * Gets the width of this dimension.
   *
   * @return the width, as a double
   */
  public double getWidth()
  {
    return width;
  }

  /**
   * Gets the height of this dimension.
   *
   * @return the height, as a double
   */
  public double getHeight()
  {
    return height;
  }

  /**
   * Sets the size of this dimension. The values are rounded to int.
   *
   * @param w the new width
   * @param h the new height
   * @since 1.2
   */
  public void setSize(double w, double h)
  {
    width = (int) w;
    height = (int) h;
  }

  /**
   * Returns the size of this dimension. A pretty useless method, as this is
   * already a dimension.
   *
   * @return a copy of this dimension
   * @see #setSize(Dimension)
   * @since 1.1
   */
  public Dimension getSize()
  {
    return new Dimension(width, height);
  }

  /**
   * Sets the width and height of this object to match that of the
   * specified object.
   *
   * @param d the Dimension to get the new width and height from
   * @throws NullPointerException if d is null
   * @see #getSize()
   * @since 1.1
   */
  public void setSize(Dimension d)
  {
    width = d.width;
    height = d.height;
  }

  /**
   * Sets the width and height of this object to the specified values.
   *
   * @param w the new width value
   * @param h the new height value
   */
  public void setSize(int w, int h)
  {
    width = w;
    height = h;
  }

  /**
   * Tests this object for equality against the specified object.  This will
   * be true if and only if the specified object is an instance of
   * Dimension2D, and has the same width and height.
   *
   * @param obj the object to test against
   * @return true if the object is equal to this
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof Dimension))
      return false;
    Dimension dim = (Dimension) obj;
    return height == dim.height && width == dim.width;
  }

  /**
   * Return the hashcode for this object. It is not documented, but appears
   * to be <code>((width + height) * (width + height + 1) / 2) + width</code>.
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    // Reverse engineering this was fun!
    return (width + height) * (width + height + 1) / 2 + width;
  }

  /**
   * Returns a string representation of this object. The format is:
   * <code>getClass().getName() + "[width=" + width + ",height=" + height
   * + ']'</code>.
   *
   * @return a string representation of this object
   */
  public String toString()
  {
    return getClass().getName()
      + "[width=" + width + ",height=" + height + ']';
  }
} // class Dimension
