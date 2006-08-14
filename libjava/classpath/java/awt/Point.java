/* Point.java -- represents a point in 2-D space
   Copyright (C) 1999, 2002, 2006 Free Software Foundation

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

import java.awt.geom.Point2D;
import java.io.Serializable;

/**
 * This class represents a point on the screen using cartesian coordinates.
 * Remember that in screen coordinates, increasing x values go from left to
 * right, and increasing y values go from top to bottom.
 *
 * <p>There are some public fields; if you mess with them in an inconsistent
 * manner, it is your own fault when you get invalid results. Also, this
 * class is not threadsafe.
 *
 * @author Per Bothner (bothner@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.0
 * @status updated to 1.4
 */
public class Point extends Point2D implements Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -5276940640259749850L;

  /**
   * The x coordinate.
   *
   * @see #getLocation()
   * @see #move(int, int)
   * @serial the X coordinate of the point
   */
  public int x;

  /**
   * The y coordinate.
   *
   * @see #getLocation()
   * @see #move(int, int)
   * @serial The Y coordinate of the point
   */
  public int y;

  /**
   * Initializes a new instance of <code>Point</code> representing the
   * coordinates (0, 0).
   *
   * @since 1.1
   */
  public Point()
  {
  }

  /**
   * Initializes a new instance of <code>Point</code> with coordinates
   * identical to the coordinates of the specified point.
   *
   * @param p the point to copy the coordinates from
   * @throws NullPointerException if p is null
   */
  public Point(Point p)
  {
    x = p.x;
    y = p.y;
  }

  /**
   * Initializes a new instance of <code>Point</code> with the specified
   * coordinates.
   *
   * @param x the X coordinate
   * @param y the Y coordinate
   */
  public Point(int x, int y)
  {
    this.x = x;
    this.y = y;
  }

  /**
   * Get the x coordinate.
   *
   * @return the value of x, as a double
   */
  public double getX()
  {
    return x;
  }

  /**
   * Get the y coordinate.
   *
   * @return the value of y, as a double
   */
  public double getY()
  {
    return y;
  }

  /**
   * Returns the location of this point. A pretty useless method, as this
   * is already a point.
   *
   * @return a copy of this point
   * @see #setLocation(Point)
   * @since 1.1
   */
  public Point getLocation()
  {
    return new Point(x, y);
  }

  /**
   * Sets this object's coordinates to match those of the specified point.
   *
   * @param p the point to copy the coordinates from
   * @throws NullPointerException if p is null
   * @since 1.1
   */
  public void setLocation(Point p)
  {
    x = p.x;
    y = p.y;
  }

  /**
   * Sets this object's coordinates to the specified values.  This method
   * is identical to the <code>move()</code> method.
   *
   * @param x the new X coordinate
   * @param y the new Y coordinate
   */
  public void setLocation(int x, int y)
  {
    this.x = x;
    this.y = y;
  }

  /**
   * Sets this object's coordinates to the specified values.  This method
   * rounds to the nearest integer coordinates by adding 0.5 and calling 
   * {@link Math#floor(double)}.
   *
   * @param x the new X coordinate
   * @param y the new Y coordinate
   */
  public void setLocation(double x, double y)
  {
    this.x = (int) Math.floor(x + 0.5);
    this.y = (int) Math.floor(y + 0.5);
  }

  /**
   * Sets this object's coordinates to the specified values.  This method
   * is identical to the <code>setLocation(int, int)</code> method.
   *
   * @param x the new X coordinate
   * @param y the new Y coordinate
   */
  public void move(int x, int y)
  {
    this.x = x;
    this.y = y;
  }

  /**
   * Changes the coordinates of this point such that the specified
   * <code>dx</code> parameter is added to the existing X coordinate and
   * <code>dy</code> is added to the existing Y coordinate.
   *
   * @param dx the amount to add to the X coordinate
   * @param dy the amount to add to the Y coordinate
   */
  public void translate(int dx, int dy)
  {
    x += dx;
    y += dy;
  }

  /**
   * Tests whether or not this object is equal to the specified object.
   * This will be true if and only if the specified object is an instance
   * of Point2D and has the same X and Y coordinates.
   *
   * @param obj the object to test against for equality
   * @return true if the specified object is equal
  */
  public boolean equals(Object obj)
  {
    // NOTE: No special hashCode() method is required for this class,
    // as this equals() implementation is functionally equivalent to
    // super.equals(), which does define a proper hashCode().

    if (! (obj instanceof Point2D))
      return false;
    Point2D p = (Point2D) obj;
    return x == p.getX() && y == p.getY();
  }

  /**
   * Returns a string representation of this object. The format is:
   * <code>getClass().getName() + "[x=" + x + ",y=" + y + ']'</code>.
   *
   * @return a string representation of this object
   */
  public String toString()
  {
    return getClass().getName() + "[x=" + x + ",y=" + y + ']';
  }
} // class Point
