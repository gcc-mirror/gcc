/* Copyright (C) 1999, 2000, 2002  Free Software Foundation

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

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, except that neither toString
 * has not been compared with JDK output.
 */

/**
  * This class holds a width and height value pair.
  *
  * @author Per Bothner <bothner@cygnus.com>
  * @author Aaron M. Renn (arenn@urbanophile.com)
  * @date Fenruary 8, 1999.
  */
public class Dimension extends java.awt.geom.Dimension2D
  implements java.io.Serializable
{
  /**
   * This width of this object.
   */
  public int width;

  /**
   * The height of this object.
   */
  public int height;

  /**
   * Initializes a new instance of <code>Dimension</code> with a width
   * and height of zero.
   */
  public Dimension () { }

  /**
   * Initializes a new instance of <code>Dimension</code> to have a width
   * and height identical to that of the specified dimension object.
   *
   * @param dim The <code>Dimension</code> to take the width and height from.
   */
  public Dimension (Dimension dim)
  {
    this.width = dim.width;
    this.height = dim.height;
  }

  /**
   * Initializes a new instance of <code>Dimension</code> with the
   * specified width and height.
   *
   * @param width The width of this object.
   * @param height The height of this object.
   */
  public Dimension (int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  /**
   * Tests this object for equality against the specified object.  This will
   * be true if and only if the specified object:
   * <p>
   * <ul>
   * <li>Is not <code>null</code>.
   * <li>Is an instance of <code>Dimension</code>.
   * <li>Has width and height values identical to this object.
   * </ul>
   *
   * @param obj The object to test against.
   *
   * @return <code>true</code> if the specified object is equal to this
   * object, <code>false</code> otherwise.
   */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof Dimension))
      return false;
    Dimension dim = (Dimension) obj;
    return height == dim.height && width == dim.width;
  }

  /**
   * Returns the size of this object.  Not very useful.
   *
   * @return This object.
   */
  public Dimension getSize () { return new Dimension(this); }

  /**
   * Sets the width and height of this object to match that of the
   * specified object.
   *
   * @param dim The <code>Dimension</code> object to get the new width and
   * height from.
   */
  public void setSize (Dimension dim)
  {
    this.width = dim.width;
    this.height = dim.height;
  }

  /**
   * Sets the width and height of this object to the specified values.
   *
   * @param width The new width value.
   * @param height The new height value.
   */
  public void setSize (int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  /**
   * Returns a string representation of this object.
   *
   * @return A string representation of this object.
   */
  public String toString ()
  {
    return "Dimension[w:"+width+",h:"+height+']';
  }

  /* Note:  There is no Dimension.hashCode. */

  public double getWidth() { return width; }
  public double getHeight() { return height; }

  public void setSize (double width, double height)
  {
    this.width = (int) width;
    this.height = (int) height;
  }
}
