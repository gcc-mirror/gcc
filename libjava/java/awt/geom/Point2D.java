/* Point2D.java -- generic point in 2-D space
   Copyright (C) 1999, 2000, 2002, 2004  Free Software Foundation

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


package java.awt.geom;

/**
 * This class implements a generic point in 2D Cartesian space. The storage
 * representation is left up to the subclass. Point includes two useful
 * nested classes, for float and double storage respectively.
 *
 * @author Per Bothner (bothner@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class Point2D implements Cloneable
{
  /**
   * The default constructor.
   *
   * @see java.awt.Point
   * @see Point2D.Float
   * @see Point2D.Double
   */
  protected Point2D()
  {
  }

  /**
   * Get the X coordinate, in double precision.
   *
   * @return the x coordinate
   */
  public abstract double getX();

  /**
   * Get the Y coordinate, in double precision.
   *
   * @return the y coordinate
   */
  public abstract double getY();

  /**
   * Set the location of this point to the new coordinates. There may be a
   * loss of precision.
   *
   * @param x the new x coordinate
   * @param y the new y coordinate
   */
  public abstract void setLocation(double x, double y);

  /**
   * Set the location of this point to the new coordinates. There may be a
   * loss of precision.
   *
   * @param p the point to copy
   * @throws NullPointerException if p is null
   */
  public void setLocation(Point2D p)
  {
    setLocation(p.getX(), p.getY());
  }

  /**
   * Return the square of the distance between two points.
   *
   * @param x1 the x coordinate of point 1
   * @param y1 the y coordinate of point 1
   * @param x2 the x coordinate of point 2
   * @param y2 the y coordinate of point 2
   * @return (x2 - x1)^2 + (y2 - y1)^2
   */
  public static double distanceSq(double x1, double y1, double x2, double y2)
  {
    x2 -= x1;
    y2 -= y1;
    return x2 * x2 + y2 * y2;
  }

  /**
   * Return the distance between two points.
   *
   * @param x1 the x coordinate of point 1
   * @param y1 the y coordinate of point 1
   * @param x2 the x coordinate of point 2
   * @param y2 the y coordinate of point 2
   * @return the distance from (x1,y1) to (x2,y2)
   */
  public static double distance(double x1, double y1, double x2, double y2)
  {
    return Math.sqrt(distanceSq(x1, y1, x2, y2));
  }

  /**
   * Return the square of the distance from this point to the given one.
   *
   * @param x the x coordinate of the other point
   * @param y the y coordinate of the other point
   * @return the square of the distance
   */
  public double distanceSq(double x, double y)
  {
    return distanceSq(getX(), x, getY(), y);
  }

  /**
   * Return the square of the distance from this point to the given one.
   *
   * @param p the other point
   * @return the square of the distance
   * @throws NullPointerException if p is null
   */
  public double distanceSq(Point2D p)
  {
    return distanceSq(getX(), p.getX(), getY(), p.getY());
  }

  /**
   * Return the distance from this point to the given one.
   *
   * @param x the x coordinate of the other point
   * @param y the y coordinate of the other point
   * @return the distance
   */
  public double distance(double x, double y)
  {
    return distance(getX(), x, getY(), y);
  }

  /**
   * Return the distance from this point to the given one.
   *
   * @param p the other point
   * @return the distance
   * @throws NullPointerException if p is null
   */
  public double distance(Point2D p)
  {
    return distance(getX(), p.getX(), getY(), p.getY());
  }

  /**
   * Create a new point of the same run-time type with the same contents as
   * this one.
   *
   * @return the clone
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

  /**
   * Return the hashcode for this point. The formula is not documented, but
   * appears to be the same as:
   * <pre>
   * long l = Double.doubleToLongBits(getY());
   * l = l * 31 ^ Double.doubleToLongBits(getX());
   * return (int) ((l >> 32) ^ l);
   * </pre>
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    // Talk about a fun time reverse engineering this one!
    long l = java.lang.Double.doubleToLongBits(getY());
    l = l * 31 ^ java.lang.Double.doubleToLongBits(getX());
    return (int) ((l >> 32) ^ l);
  }

  /**
   * Compares two points for equality. This returns true if they have the
   * same coordinates.
   *
   * @param o the point to compare
   * @return true if it is equal
   */
  public boolean equals(Object o)
  {
    if (! (o instanceof Point2D))
      return false;
    Point2D p = (Point2D) o;
    return getX() == p.getX() && getY() == p.getY();
  }

  /**
   * This class defines a point in <code>double</code> precision.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.2
   * @status updated to 1.4
   */
  public static class Double extends Point2D
  {
    /** The X coordinate. */
    public double x;

    /** The Y coordinate. */
    public double y;

    /**
     * Create a new point at (0,0).
     */
    public Double()
    {
    }

    /**
     * Create a new point at (x,y).
     *
     * @param x the x coordinate
     * @param y the y coordinate
     */
    public Double(double x, double y)
    {
      this.x = x;
      this.y = y;
    }

    /**
     * Return the x coordinate.
     *
     * @return the x coordinate
     */
    public double getX()
    {
      return x;
    }

    /**
     * Return the y coordinate.
     *
     * @return the y coordinate
     */
    public double getY()
    {
      return y;
    }

    /**
     * Sets the location of this point.
     *
     * @param x the new x coordinate
     * @param y the new y coordinate
     */
    public void setLocation(double x, double y)
    {
      this.x = x;
      this.y = y;
    }

    /**
     * Returns a string representation of this object. The format is:
     * <code>"Point2D.Double[" + x + ", " + y + ']'</code>.
     *
     * @return a string representation of this object
     */
    public String toString()
    {
      return "Point2D.Double[" + x + ", " + y + ']';
    }
  } // class Double

  /**
   * This class defines a point in <code>float</code> precision.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.2
   * @status updated to 1.4
   */
  public static class Float extends Point2D
  {
    /** The X coordinate. */
    public float x;

    /** The Y coordinate. */
    public float y;

    /**
     * Create a new point at (0,0).
     */
    public Float()
    {
    }

    /**
     * Create a new point at (x,y).
     *
     * @param x the x coordinate
     * @param y the y coordinate
     */
    public Float(float x, float y)
    {
      this.x = x;
      this.y = y;
    }

    /**
     * Return the x coordinate.
     *
     * @return the x coordinate
     */
    public double getX()
    {
      return x;
    }

    /**
     * Return the y coordinate.
     *
     * @return the y coordinate
     */
    public double getY()
    {
      return y;
    }

    /**
     * Sets the location of this point.
     *
     * @param x the new x coordinate
     * @param y the new y coordinate
     */
    public void setLocation(double x, double y)
    {
      this.x = (float) x;
      this.y = (float) y;
    }

    /**
     * Sets the location of this point.
     *
     * @param x the new x coordinate
     * @param y the new y coordinate
     */
    public void setLocation(float x, float y)
    {
      this.x = x;
      this.y = y;
    }

    /**
     * Returns a string representation of this object. The format is:
     * <code>"Point2D.Float[" + x + ", " + y + ']'</code>.
     *
     * @return a string representation of this object
     */
    public String toString()
    {
      return "Point2D.Float[" + x + ", " + y + ']';
    }
  } // class Float
} // class Point2D
