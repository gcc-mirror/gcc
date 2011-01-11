/* Line2D.java -- represents a line in 2-D space, plus operations on a line
   Copyright (C) 2000, 2001, 2002 Free Software Foundation

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

package java.awt.geom;

import java.awt.Rectangle;
import java.awt.Shape;
import java.util.NoSuchElementException;

/**
 * Represents a directed line bewteen two points in (x,y) Cartesian space.
 * Remember, on-screen graphics have increasing x from left-to-right, and
 * increasing y from top-to-bottom. The storage is left to subclasses.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @author David Gilbert
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class Line2D implements Shape, Cloneable
{
  /**
   * The default constructor.
   */
  protected Line2D()
  {
  }

  /**
   * Return the x coordinate of the first point.
   *
   * @return the starting x coordinate
   */
  public abstract double getX1();

  /**
   * Return the y coordinate of the first point.
   *
   * @return the starting y coordinate
   */
  public abstract double getY1();

  /**
   * Return the first point.
   *
   * @return the starting point
   */
  public abstract Point2D getP1();

  /**
   * Return the x coordinate of the second point.
   *
   * @return the ending x coordinate
   */
  public abstract double getX2();

  /**
   * Return the y coordinate of the second point.
   *
   * @return the ending y coordinate
   */
  public abstract double getY2();

  /**
   * Return the second point.
   *
   * @return the ending point
   */
  public abstract Point2D getP2();

  /**
   * Set the coordinates of the line to the given coordinates. Loss of
   * precision may occur due to rounding issues.
   *
   * @param x1 the first x coordinate
   * @param y1 the first y coordinate
   * @param x2 the second x coordinate
   * @param y2 the second y coordinate
   */
  public abstract void setLine(double x1, double y1, double x2, double y2);

  /**
   * Set the coordinates to the given points.
   *
   * @param p1 the first point
   * @param p2 the second point
   * @throws NullPointerException if either point is null
   */
  public void setLine(Point2D p1, Point2D p2)
  {
    setLine(p1.getX(), p1.getY(), p2.getX(), p2.getY());
  }

  /**
   * Set the coordinates to those of the given line.
   *
   * @param l the line to copy
   * @throws NullPointerException if l is null
   */
  public void setLine(Line2D l)
  {
    setLine(l.getX1(), l.getY1(), l.getX2(), l.getY2());
  }

  /**
   * Computes the relative rotation direction needed to pivot the line about
   * the first point in order to have the second point colinear with point p.
   * Because of floating point rounding, don't expect this to be a perfect
   * measure of colinearity. The answer is 1 if the line has a shorter rotation
   * in the direction of the positive X axis to the negative Y axis
   * (counter-clockwise in the default Java coordinate system), or -1 if the
   * shortest rotation is in the opposite direction (clockwise). If p
   * is already colinear, the return value is -1 if it lies beyond the first
   * point, 0 if it lies in the segment, or 1 if it lies beyond the second
   * point. If the first and second point are coincident, this returns 0.
   *
   * @param x1 the first x coordinate
   * @param y1 the first y coordinate
   * @param x2 the second x coordinate
   * @param y2 the second y coordinate
   * @param px the reference x coordinate
   * @param py the reference y coordinate
   * @return the relative rotation direction
   */
  public static int relativeCCW(double x1, double y1, double x2, double y2,
                                double px, double py)
  {
    if ((x1 == x2 && y1 == y2)
        || (x1 == px && y1 == py))
      return 0; // Coincident points.
    // Translate to the origin.
    x2 -= x1;
    y2 -= y1;
    px -= x1;
    py -= y1;
    double slope2 = y2 / x2;
    double slopep = py / px;
    if (slope2 == slopep || (x2 == 0 && px == 0))
      return y2 > 0 // Colinear.
        ? (py < 0 ? -1 : py > y2 ? 1 : 0)
        : (py > 0 ? -1 : py < y2 ? 1 : 0);
    if (x2 >= 0 && slope2 >= 0)
      return px >= 0 // Quadrant 1.
        ? (slope2 > slopep ? 1 : -1)
        : (slope2 < slopep ? 1 : -1);
    if (y2 > 0)
      return px < 0 // Quadrant 2.
        ? (slope2 > slopep ? 1 : -1)
        : (slope2 < slopep ? 1 : -1);
    if (slope2 >= 0.0)
      return px >= 0 // Quadrant 3.
        ? (slope2 < slopep ? 1 : -1)
        : (slope2 > slopep ? 1 : -1);
    return px < 0 // Quadrant 4.
      ? (slope2 < slopep ? 1 : -1)
      : (slope2 > slopep ? 1 : -1);
  }

  /**
   * Computes the relative rotation direction needed to pivot this line about
   * the first point in order to have the second point colinear with point p.
   * Because of floating point rounding, don't expect this to be a perfect
   * measure of colinearity. The answer is 1 if the line has a shorter rotation
   * in the direction of the positive X axis to the negative Y axis
   * (counter-clockwise in the default Java coordinate system), or -1 if the
   * shortest rotation is in the opposite direction (clockwise). If p
   * is already colinear, the return value is -1 if it lies beyond the first
   * point, 0 if it lies in the segment, or 1 if it lies beyond the second
   * point. If the first and second point are coincident, this returns 0.
   *
   * @param px the reference x coordinate
   * @param py the reference y coordinate
   * @return the relative rotation direction
   * @see #relativeCCW(double, double, double, double, double, double)
   */
  public int relativeCCW(double px, double py)
  {
    return relativeCCW(getX1(), getY1(), getX2(), getY2(), px, py);
  }

  /**
   * Computes the relative rotation direction needed to pivot this line about
   * the first point in order to have the second point colinear with point p.
   * Because of floating point rounding, don't expect this to be a perfect
   * measure of colinearity. The answer is 1 if the line has a shorter rotation
   * in the direction of the positive X axis to the negative Y axis
   * (counter-clockwise in the default Java coordinate system), or -1 if the
   * shortest rotation is in the opposite direction (clockwise). If p
   * is already colinear, the return value is -1 if it lies beyond the first
   * point, 0 if it lies in the segment, or 1 if it lies beyond the second
   * point. If the first and second point are coincident, this returns 0.
   *
   * @param p the reference point
   * @return the relative rotation direction
   * @throws NullPointerException if p is null
   * @see #relativeCCW(double, double, double, double, double, double)
   */
  public int relativeCCW(Point2D p)
  {
    return relativeCCW(getX1(), getY1(), getX2(), getY2(), p.getX(), p.getY());
  }

  /**
   * Computes twice the (signed) area of the triangle defined by the three
   * points.  This method is used for intersection testing.
   *
   * @param x1  the x-coordinate of the first point.
   * @param y1  the y-coordinate of the first point.
   * @param x2  the x-coordinate of the second point.
   * @param y2  the y-coordinate of the second point.
   * @param x3  the x-coordinate of the third point.
   * @param y3  the y-coordinate of the third point.
   *
   * @return Twice the area.
   */
  private static double area2(double x1, double y1,
                             double x2, double y2,
                             double x3, double y3)
  {
    return (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1);
  }

  /**
   * Returns <code>true</code> if (x3, y3) lies between (x1, y1) and (x2, y2),
   * and false otherwise,  This test assumes that the three points are
   * collinear, and is used for intersection testing.
   *
   * @param x1  the x-coordinate of the first point.
   * @param y1  the y-coordinate of the first point.
   * @param x2  the x-coordinate of the second point.
   * @param y2  the y-coordinate of the second point.
   * @param x3  the x-coordinate of the third point.
   * @param y3  the y-coordinate of the third point.
   *
   * @return A boolean.
   */
  private static boolean between(double x1, double y1,
                                double x2, double y2,
                                double x3, double y3)
  {
    if (x1 != x2) {
      return (x1 <= x3 && x3 <= x2) || (x1 >= x3 && x3 >= x2);
    }
    else {
      return (y1 <= y3 && y3 <= y2) || (y1 >= y3 && y3 >= y2);
    }
  }

  /**
   * Test if the line segment (x1,y1)-&gt;(x2,y2) intersects the line segment
   * (x3,y3)-&gt;(x4,y4).
   *
   * @param x1 the first x coordinate of the first segment
   * @param y1 the first y coordinate of the first segment
   * @param x2 the second x coordinate of the first segment
   * @param y2 the second y coordinate of the first segment
   * @param x3 the first x coordinate of the second segment
   * @param y3 the first y coordinate of the second segment
   * @param x4 the second x coordinate of the second segment
   * @param y4 the second y coordinate of the second segment
   * @return true if the segments intersect
   */
  public static boolean linesIntersect(double x1, double y1,
                                      double x2, double y2,
                                      double x3, double y3,
                                      double x4, double y4)
  {
    double a1, a2, a3, a4;

    // deal with special cases
    if ((a1 = area2(x1, y1, x2, y2, x3, y3)) == 0.0)
    {
      // check if p3 is between p1 and p2 OR
      // p4 is collinear also AND either between p1 and p2 OR at opposite ends
      if (between(x1, y1, x2, y2, x3, y3))
      {
        return true;
      }
      else
      {
        if (area2(x1, y1, x2, y2, x4, y4) == 0.0)
        {
          return between(x3, y3, x4, y4, x1, y1)
                 || between (x3, y3, x4, y4, x2, y2);
        }
        else {
          return false;
        }
      }
    }
    else if ((a2 = area2(x1, y1, x2, y2, x4, y4)) == 0.0)
    {
      // check if p4 is between p1 and p2 (we already know p3 is not
      // collinear)
      return between(x1, y1, x2, y2, x4, y4);
    }

    if ((a3 = area2(x3, y3, x4, y4, x1, y1)) == 0.0) {
      // check if p1 is between p3 and p4 OR
      // p2 is collinear also AND either between p1 and p2 OR at opposite ends
      if (between(x3, y3, x4, y4, x1, y1)) {
        return true;
      }
      else {
        if (area2(x3, y3, x4, y4, x2, y2) == 0.0) {
          return between(x1, y1, x2, y2, x3, y3)
                 || between (x1, y1, x2, y2, x4, y4);
        }
        else {
          return false;
        }
      }
    }
    else if ((a4 = area2(x3, y3, x4, y4, x2, y2)) == 0.0) {
      // check if p2 is between p3 and p4 (we already know p1 is not
      // collinear)
      return between(x3, y3, x4, y4, x2, y2);
    }
    else {  // test for regular intersection
      return ((a1 > 0.0) ^ (a2 > 0.0)) && ((a3 > 0.0) ^ (a4 > 0.0));
    }
  }

  /**
   * Test if this line intersects the line given by (x1,y1)-&gt;(x2,y2).
   *
   * @param x1 the first x coordinate of the other segment
   * @param y1 the first y coordinate of the other segment
   * @param x2 the second x coordinate of the other segment
   * @param y2 the second y coordinate of the other segment
   * @return true if the segments intersect
   * @see #linesIntersect(double, double, double, double,
   *                      double, double, double, double)
   */
  public boolean intersectsLine(double x1, double y1, double x2, double y2)
  {
    return linesIntersect(getX1(), getY1(), getX2(), getY2(),
                          x1, y1, x2, y2);
  }

  /**
   * Test if this line intersects the given line.
   *
   * @param l the other segment
   * @return true if the segments intersect
   * @throws NullPointerException if l is null
   * @see #linesIntersect(double, double, double, double,
   *                      double, double, double, double)
   */
  public boolean intersectsLine(Line2D l)
  {
    return linesIntersect(getX1(), getY1(), getX2(), getY2(),
                          l.getX1(), l.getY1(), l.getX2(), l.getY2());
  }

  /**
   * Measures the square of the shortest distance from the reference point
   * to a point on the line segment. If the point is on the segment, the
   * result will be 0.
   *
   * @param x1 the first x coordinate of the segment
   * @param y1 the first y coordinate of the segment
   * @param x2 the second x coordinate of the segment
   * @param y2 the second y coordinate of the segment
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the square of the distance from the point to the segment
   * @see #ptSegDist(double, double, double, double, double, double)
   * @see #ptLineDistSq(double, double, double, double, double, double)
   */
  public static double ptSegDistSq(double x1, double y1, double x2, double y2,
                                   double px, double py)
  {
    double pd2 = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);

    double x, y;
    if (pd2 == 0)
      {
        // Points are coincident.
        x = x1;
        y = y2;
      }
    else
      {
        double u = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1)) / pd2;

        if (u < 0)
          {
            // "Off the end"
            x = x1;
            y = y1;
          }
        else if (u > 1.0)
          {
            x = x2;
            y = y2;
          }
        else
          {
            x = x1 + u * (x2 - x1);
            y = y1 + u * (y2 - y1);
          }
      }

    return (x - px) * (x - px) + (y - py) * (y - py);
  }

  /**
   * Measures the shortest distance from the reference point to a point on
   * the line segment. If the point is on the segment, the result will be 0.
   *
   * @param x1 the first x coordinate of the segment
   * @param y1 the first y coordinate of the segment
   * @param x2 the second x coordinate of the segment
   * @param y2 the second y coordinate of the segment
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the distance from the point to the segment
   * @see #ptSegDistSq(double, double, double, double, double, double)
   * @see #ptLineDist(double, double, double, double, double, double)
   */
  public static double ptSegDist(double x1, double y1, double x2, double y2,
                                 double px, double py)
  {
    return Math.sqrt(ptSegDistSq(x1, y1, x2, y2, px, py));
  }

  /**
   * Measures the square of the shortest distance from the reference point
   * to a point on this line segment. If the point is on the segment, the
   * result will be 0.
   *
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the square of the distance from the point to the segment
   * @see #ptSegDistSq(double, double, double, double, double, double)
   */
  public double ptSegDistSq(double px, double py)
  {
    return ptSegDistSq(getX1(), getY1(), getX2(), getY2(), px, py);
  }

  /**
   * Measures the square of the shortest distance from the reference point
   * to a point on this line segment. If the point is on the segment, the
   * result will be 0.
   *
   * @param p the point
   * @return the square of the distance from the point to the segment
   * @throws NullPointerException if p is null
   * @see #ptSegDistSq(double, double, double, double, double, double)
   */
  public double ptSegDistSq(Point2D p)
  {
    return ptSegDistSq(getX1(), getY1(), getX2(), getY2(), p.getX(), p.getY());
  }

  /**
   * Measures the shortest distance from the reference point to a point on
   * this line segment. If the point is on the segment, the result will be 0.
   *
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the distance from the point to the segment
   * @see #ptSegDist(double, double, double, double, double, double)
   */
  public double ptSegDist(double px, double py)
  {
    return ptSegDist(getX1(), getY1(), getX2(), getY2(), px, py);
  }

  /**
   * Measures the shortest distance from the reference point to a point on
   * this line segment. If the point is on the segment, the result will be 0.
   *
   * @param p the point
   * @return the distance from the point to the segment
   * @throws NullPointerException if p is null
   * @see #ptSegDist(double, double, double, double, double, double)
   */
  public double ptSegDist(Point2D p)
  {
    return ptSegDist(getX1(), getY1(), getX2(), getY2(), p.getX(), p.getY());
  }

  /**
   * Measures the square of the shortest distance from the reference point
   * to a point on the infinite line extended from the segment. If the point
   * is on the segment, the result will be 0. If the segment is length 0,
   * the distance is to the common endpoint.
   *
   * @param x1 the first x coordinate of the segment
   * @param y1 the first y coordinate of the segment
   * @param x2 the second x coordinate of the segment
   * @param y2 the second y coordinate of the segment
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the square of the distance from the point to the extended line
   * @see #ptLineDist(double, double, double, double, double, double)
   * @see #ptSegDistSq(double, double, double, double, double, double)
   */
  public static double ptLineDistSq(double x1, double y1, double x2, double y2,
                                    double px, double py)
  {
    double pd2 = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);

    double x, y;
    if (pd2 == 0)
      {
        // Points are coincident.
        x = x1;
        y = y2;
      }
    else
      {
        double u = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1)) / pd2;
        x = x1 + u * (x2 - x1);
        y = y1 + u * (y2 - y1);
      }

    return (x - px) * (x - px) + (y - py) * (y - py);
  }

  /**
   * Measures the shortest distance from the reference point to a point on
   * the infinite line extended from the segment. If the point is on the
   * segment, the result will be 0. If the segment is length 0, the distance
   * is to the common endpoint.
   *
   * @param x1 the first x coordinate of the segment
   * @param y1 the first y coordinate of the segment
   * @param x2 the second x coordinate of the segment
   * @param y2 the second y coordinate of the segment
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the distance from the point to the extended line
   * @see #ptLineDistSq(double, double, double, double, double, double)
   * @see #ptSegDist(double, double, double, double, double, double)
   */
  public static double ptLineDist(double x1, double y1,
                                   double x2, double y2,
                                   double px, double py)
  {
    return Math.sqrt(ptLineDistSq(x1, y1, x2, y2, px, py));
  }

  /**
   * Measures the square of the shortest distance from the reference point
   * to a point on the infinite line extended from this segment. If the point
   * is on the segment, the result will be 0. If the segment is length 0,
   * the distance is to the common endpoint.
   *
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the square of the distance from the point to the extended line
   * @see #ptLineDistSq(double, double, double, double, double, double)
   */
  public double ptLineDistSq(double px, double py)
  {
    return ptLineDistSq(getX1(), getY1(), getX2(), getY2(), px, py);
  }

  /**
   * Measures the square of the shortest distance from the reference point
   * to a point on the infinite line extended from this segment. If the point
   * is on the segment, the result will be 0. If the segment is length 0,
   * the distance is to the common endpoint.
   *
   * @param p the point
   * @return the square of the distance from the point to the extended line
   * @throws NullPointerException if p is null
   * @see #ptLineDistSq(double, double, double, double, double, double)
   */
  public double ptLineDistSq(Point2D p)
  {
    return ptLineDistSq(getX1(), getY1(), getX2(), getY2(),
                        p.getX(), p.getY());
  }

  /**
   * Measures the shortest distance from the reference point to a point on
   * the infinite line extended from this segment. If the point is on the
   * segment, the result will be 0. If the segment is length 0, the distance
   * is to the common endpoint.
   *
   * @param px the x coordinate of the point
   * @param py the y coordinate of the point
   * @return the distance from the point to the extended line
   * @see #ptLineDist(double, double, double, double, double, double)
   */
  public double ptLineDist(double px, double py)
  {
    return ptLineDist(getX1(), getY1(), getX2(), getY2(), px, py);
  }

  /**
   * Measures the shortest distance from the reference point to a point on
   * the infinite line extended from this segment. If the point is on the
   * segment, the result will be 0. If the segment is length 0, the distance
   * is to the common endpoint.
   *
   * @param p the point
   * @return the distance from the point to the extended line
   * @throws NullPointerException if p is null
   * @see #ptLineDist(double, double, double, double, double, double)
   */
  public double ptLineDist(Point2D p)
  {
    return ptLineDist(getX1(), getY1(), getX2(), getY2(), p.getX(), p.getY());
  }

  /**
   * Test if a point is contained inside the line. Since a line has no area,
   * this returns false.
   *
   * @param x the x coordinate
   * @param y the y coordinate
   * @return false; the line does not contain points
   */
  public boolean contains(double x, double y)
  {
    return false;
  }

  /**
   * Test if a point is contained inside the line. Since a line has no area,
   * this returns false.
   *
   * @param p the point
   * @return false; the line does not contain points
   */
  public boolean contains(Point2D p)
  {
    return false;
  }

  /**
   * Tests if this line intersects the interior of the specified rectangle.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @return true if the line intersects the rectangle
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    if (w <= 0 || h <= 0)
      return false;
    double x1 = getX1();
    double y1 = getY1();
    double x2 = getX2();
    double y2 = getY2();

    if (x1 >= x && x1 <= x + w && y1 >= y && y1 <= y + h)
      return true;
    if (x2 >= x && x2 <= x + w && y2 >= y && y2 <= y + h)
      return true;

    double x3 = x + w;
    double y3 = y + h;

    return (linesIntersect(x1, y1, x2, y2, x, y, x, y3)
            || linesIntersect(x1, y1, x2, y2, x, y3, x3, y3)
            || linesIntersect(x1, y1, x2, y2, x3, y3, x3, y)
            || linesIntersect(x1, y1, x2, y2, x3, y, x, y));
  }

  /**
   * Tests if this line intersects the interior of the specified rectangle.
   *
   * @param r the rectangle
   * @return true if the line intersects the rectangle
   * @throws NullPointerException if r is null
   */
  public boolean intersects(Rectangle2D r)
  {
    return intersects(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }

  /**
   * Tests if the line contains a rectangle. Since lines have no area, this
   * always returns false.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @return false; the line does not contain points
   */
  public boolean contains(double x, double y, double w, double h)
  {
    return false;
  }

  /**
   * Tests if the line contains a rectangle. Since lines have no area, this
   * always returns false.
   *
   * @param r the rectangle
   * @return false; the line does not contain points
   */
  public boolean contains(Rectangle2D r)
  {
    return false;
  }

  /**
   * Gets a bounding box (not necessarily minimal) for this line.
   *
   * @return the integer bounding box
   * @see #getBounds2D()
   */
  public Rectangle getBounds()
  {
    return getBounds2D().getBounds();
  }

  /**
   * Return a path iterator, possibly applying a transform on the result. This
   * iterator is not threadsafe.
   *
   * @param at the transform, or null
   * @return a new path iterator
   */
  public PathIterator getPathIterator(final AffineTransform at)
  {
    return new PathIterator()
    {
      /** Current coordinate. */
      private int current = 0;

      public int getWindingRule()
      {
        return WIND_NON_ZERO;
      }

      public boolean isDone()
      {
        return current >= 2;
      }

      public void next()
      {
        current++;
      }

      public int currentSegment(float[] coords)
      {
        int result;
        switch (current)
          {
          case 0:
            coords[0] = (float) getX1();
            coords[1] = (float) getY1();
            result = SEG_MOVETO;
            break;
          case 1:
            coords[0] = (float) getX2();
            coords[1] = (float) getY2();
            result = SEG_LINETO;
            break;
          default:
            throw new NoSuchElementException("line iterator out of bounds");
          }
        if (at != null)
          at.transform(coords, 0, coords, 0, 1);
        return result;
      }

      public int currentSegment(double[] coords)
      {
        int result;
        switch (current)
          {
          case 0:
            coords[0] = getX1();
            coords[1] = getY1();
            result = SEG_MOVETO;
            break;
          case 1:
            coords[0] = getX2();
            coords[1] = getY2();
            result = SEG_LINETO;
            break;
          default:
            throw new NoSuchElementException("line iterator out of bounds");
          }
        if (at != null)
          at.transform(coords, 0, coords, 0, 1);
        return result;
      }
    };
  }

  /**
   * Return a flat path iterator, possibly applying a transform on the result.
   * This iterator is not threadsafe.
   *
   * @param at the transform, or null
   * @param flatness ignored, since lines are already flat
   * @return a new path iterator
   * @see #getPathIterator(AffineTransform)
   */
  public PathIterator getPathIterator(AffineTransform at, double flatness)
  {
    return getPathIterator(at);
  }

  /**
   * Create a new line of the same run-time type with the same contents as
   * this one.
   *
   * @return the clone
   *
   * @exception OutOfMemoryError If there is not enough memory available.
   *
   * @since 1.2
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
   * This class defines a point in <code>double</code> precision.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.2
   * @status updated to 1.4
   */
  public static class Double extends Line2D
  {
    /** The x coordinate of the first point. */
    public double x1;

    /** The y coordinate of the first point. */
    public double y1;

    /** The x coordinate of the second point. */
    public double x2;

    /** The y coordinate of the second point. */
    public double y2;

    /**
     * Construct the line segment (0,0)-&gt;(0,0).
     */
    public Double()
    {
    }

    /**
     * Construct the line segment with the specified points.
     *
     * @param x1 the x coordinate of the first point
     * @param y1 the y coordinate of the first point
     * @param x2 the x coordinate of the second point
     * @param y2 the y coordinate of the second point
     */
    public Double(double x1, double y1, double x2, double y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }

    /**
     * Construct the line segment with the specified points.
     *
     * @param p1 the first point
     * @param p2 the second point
     * @throws NullPointerException if either point is null
     */
    public Double(Point2D p1, Point2D p2)
    {
      x1 = p1.getX();
      y1 = p1.getY();
      x2 = p2.getX();
      y2 = p2.getY();
    }

    /**
     * Return the x coordinate of the first point.
     *
     * @return the value of x1
     */
    public double getX1()
    {
      return x1;
    }

    /**
     * Return the y coordinate of the first point.
     *
     * @return the value of y1
     */
    public double getY1()
    {
      return y1;
    }

    /**
     * Return the first point.
     *
     * @return the point (x1,y1)
     */
    public Point2D getP1()
    {
      return new Point2D.Double(x1, y1);
    }

    /**
     * Return the x coordinate of the second point.
     *
     * @return the value of x2
     */
    public double getX2()
    {
      return x2;
    }

    /**
     * Return the y coordinate of the second point.
     *
     * @return the value of y2
     */
    public double getY2()
    {
      return y2;
    }

    /**
     * Return the second point.
     *
     * @return the point (x2,y2)
     */
    public Point2D getP2()
    {
      return new Point2D.Double(x2, y2);
    }

    /**
     * Set this line to the given points.
     *
     * @param x1 the new x coordinate of the first point
     * @param y1 the new y coordinate of the first point
     * @param x2 the new x coordinate of the second point
     * @param y2 the new y coordinate of the second point
     */
    public void setLine(double x1, double y1, double x2, double y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }

    /**
     * Return the exact bounds of this line segment.
     *
     * @return the bounding box
     */
    public Rectangle2D getBounds2D()
    {
      double x = Math.min(x1, x2);
      double y = Math.min(y1, y2);
      double w = Math.abs(x1 - x2);
      double h = Math.abs(y1 - y2);
      return new Rectangle2D.Double(x, y, w, h);
    }
  } // class Double

  /**
   * This class defines a point in <code>float</code> precision.
   *
   * @author Eric Blake (ebb9@email.byu.edu)
   * @since 1.2
   * @status updated to 1.4
   */
  public static class Float extends Line2D
  {
    /** The x coordinate of the first point. */
    public float x1;

    /** The y coordinate of the first point. */
    public float y1;

    /** The x coordinate of the second point. */
    public float x2;

    /** The y coordinate of the second point. */
    public float y2;

    /**
     * Construct the line segment (0,0)-&gt;(0,0).
     */
    public Float()
    {
    }

    /**
     * Construct the line segment with the specified points.
     *
     * @param x1 the x coordinate of the first point
     * @param y1 the y coordinate of the first point
     * @param x2 the x coordinate of the second point
     * @param y2 the y coordinate of the second point
     */
    public Float(float x1, float y1, float x2, float y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }

    /**
     * Construct the line segment with the specified points.
     *
     * @param p1 the first point
     * @param p2 the second point
     * @throws NullPointerException if either point is null
     */
    public Float(Point2D p1, Point2D p2)
    {
      x1 = (float) p1.getX();
      y1 = (float) p1.getY();
      x2 = (float) p2.getX();
      y2 = (float) p2.getY();
    }

    /**
     * Return the x coordinate of the first point.
     *
     * @return the value of x1
     */
    public double getX1()
    {
      return x1;
    }

    /**
     * Return the y coordinate of the first point.
     *
     * @return the value of y1
     */
    public double getY1()
    {
      return y1;
    }

    /**
     * Return the first point.
     *
     * @return the point (x1,y1)
     */
    public Point2D getP1()
    {
      return new Point2D.Float(x1, y1);
    }

    /**
     * Return the x coordinate of the second point.
     *
     * @return the value of x2
     */
    public double getX2()
    {
      return x2;
    }

    /**
     * Return the y coordinate of the second point.
     *
     * @return the value of y2
     */
    public double getY2()
    {
      return y2;
    }

    /**
     * Return the second point.
     *
     * @return the point (x2,y2)
     */
    public Point2D getP2()
    {
      return new Point2D.Float(x2, y2);
    }

    /**
     * Set this line to the given points.
     *
     * @param x1 the new x coordinate of the first point
     * @param y1 the new y coordinate of the first point
     * @param x2 the new x coordinate of the second point
     * @param y2 the new y coordinate of the second point
     */
    public void setLine(double x1, double y1, double x2, double y2)
    {
      this.x1 = (float) x1;
      this.y1 = (float) y1;
      this.x2 = (float) x2;
      this.y2 = (float) y2;
    }

    /**
     * Set this line to the given points.
     *
     * @param x1 the new x coordinate of the first point
     * @param y1 the new y coordinate of the first point
     * @param x2 the new x coordinate of the second point
     * @param y2 the new y coordinate of the second point
     */
    public void setLine(float x1, float y1, float x2, float y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }

    /**
     * Return the exact bounds of this line segment.
     *
     * @return the bounding box
     */
    public Rectangle2D getBounds2D()
    {
      float x = Math.min(x1, x2);
      float y = Math.min(y1, y2);
      float w = Math.abs(x1 - x2);
      float h = Math.abs(y1 - y2);
      return new Rectangle2D.Float(x, y, w, h);
    }
  } // class Float
} // class Line2D
