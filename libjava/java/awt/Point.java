/* Copyright (C) 1999, 2002  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;
import java.awt.geom.Point2D;

/* Written using "Java Class Libraries", 2nd edition, plus online
 * API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status:  Believed complete and correct, except that neither toString
 * nor hashCode have been compared with JDK output.
 */

/**
 * This class represents a point on the screen using cartesian coordinates.
 *
 * @author Per Bothner <bothner@cygnus.com>
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @date February 8, 1999.
 */
public class Point extends Point2D implements java.io.Serializable
{
  /**
   * @serial The X coordinate of the point.
   */
  public int x;

  /**
   * @serial The Y coordinate of the point.
   */
  public int y;

  /**
   * Initializes a new instance of <code>Point</code> representing the
   * coordiates (0,0).
   */
  public Point () { }

  /**
   * Initializes a new instance of <code>Point</code> with coordinates
   * identical to the coordinates of the specified points.
   *
   * @param point The point to copy the coordinates from.
   */
  public Point (Point p) { this.x = p.x;  this.y = p.y; }

  /**
   * Initializes a new instance of <code>Point</code> with the specified
   * coordinates.
   *
   * @param x The X coordinate of this point.
   * @param y The Y coordinate of this point.
   */
  public Point (int x, int y) { this.x = x;  this.y = y; }

  /**
   * Tests whether or not this object is equal to the specified object.
   * This will be true if and only if the specified objectj:
   * <p>
   * <ul>
   * <li>Is not <code>null</code>.
   * <li>Is an instance of <code>Point</code>.
   * <li>Has X and Y coordinates equal to this object's.
   * </ul>
   *
   * @param obj The object to test against for equality.
   *
   * @return <code>true</code> if the specified object is equal to this
   * object, <code>false</code> otherwise.
  */
  public boolean equals (Object obj)
  {
    if (! (obj instanceof Point))
      return false;
    Point p = (Point) obj;
    return this.x == p.x && this.y == p.y;
  }

  /**
   * Returns a hash value for this point.
   *
   * @param A hash value for this point.
   */
  public int hashCode () { return x ^ y; }

  /**
   * Returns the location of this object as a point.  A pretty useless
   * method.  It is included to mimic the <code>getLocation</code> method
   * in component.
   *
   * @return This point.
   */
  public Point getLocation () { return new Point(this); }

  /**
   * Sets this object's coordinates to the specified values.  This method
   * is identical to the <code>setLocation(int, int)</code> method.
   *
   * @param x The new X coordinate.
   * @param y The new Y coordinate.
   */
  public void move (int x, int y) { this.x = x;  this.y = y; }

  /**
   * Sets this object's coordinates to the specified values.  This method
   * is identical to the <code>move()</code> method.
   *
   * @param x The new X coordinate.
   * @param y The new Y coordinate.
   */
  public void setLocation (int x, int y) { this.x = x;  this.y = y; }

  /**
   * Sets this object's coordinates to match those of the specified point.
   *
   * @param point The point to copy the coordinates from.
   */
  public void setLocation (Point pt) { this.x = pt.x;  this.y = pt.y; }

  /**
   * Changes the coordinates of this point such that the specified 
   * <code>dx</code> parameter is added to the existing X coordinate and
   * <code>dy</code> is added to the existing Y coordinate.
   *
   * @param dx The amount to add to the X coordinate.
   * @param dy The amount to add to the Y coordinate.
   */
  public void translate (int x, int y) { this.x += x;  this.y += y; }

  /**
   * Returns a string representation of this object.
   *
   * @return A string representation of this object.
   */
  public String toString ()
  {
    return getClass().getName() + "[x:"+x+",y:"+y+']';
  }

  public double getX() { return x; }
  public double getY() { return y; }

  public void setLocation (double x, double y)
  { this.x = (int) x;  this.y = (int) y; }

}
