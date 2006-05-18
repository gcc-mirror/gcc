/* CubicSegment.java -- Cubic segment used for BasicStroke
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


package gnu.java.awt.java2d;


import java.awt.geom.Point2D;

/**
 * Cubic Bezier curve segment
 */
public class CubicSegment extends Segment
{
  public Point2D cp1; // control points
  public Point2D cp2; // control points

  /**
   * Constructor - takes coordinates of the starting point,
   * first control point, second control point and end point,
   * respecively.
   */
  public CubicSegment(double x1, double y1, double c1x, double c1y,
                      double c2x, double c2y, double x2, double y2)
  {
    super();
    P1 = new Point2D.Double(x1, y1);
    P2 = new Point2D.Double(x2, y2);
    cp1 = new Point2D.Double(c1x, c1y);
    cp2 = new Point2D.Double(c2x, c2y);
  }

  public CubicSegment(Point2D p1, Point2D cp1, Point2D cp2, Point2D p2)
  {
    super();
    P1 = p1;
    P2 = p2;
    this.cp1 = cp1;
    this.cp2 = cp2;
  }

  /**
   * Clones this segment
   */
  public Object clone()
  {
    return new CubicSegment(P1.getX(), P1.getY(), cp1.getX(), cp1.getY(),
                            cp2.getX(), cp2.getY(), P2.getX(), P2.getY());
  }

  /**
   * Get the "top" and "bottom" segments of this segment.
   * First array element is p0 + normal, second is p0 - normal.
   */
  public Segment[] getDisplacedSegments(double radius)
  {
    this.radius = radius;
    double x0 = P1.getX();
    double y0 = P1.getY();
    double x1 = cp1.getX();
    double y1 = cp1.getY();
    double x2 = cp2.getX();
    double y2 = cp2.getY();
    double x3 = P2.getX();
    double y3 = P2.getY();
    double[] p1 = normal(x0, y0, x1, y1);
    double[] p2 = normal(x2, y2, x3, y3);

    
    // FIXME: Doesn't compile.
    // return new Segment[]{s1, s2};
    return new Segment[0];
  }

  public void reverse()
  {
    Point2D temp = P1;
    P1 = P2;
    P2 = temp;
    temp = cp1;
    cp1 = cp2;
    cp2 = temp;
  }

  public double[] first()
  {
    return new double[]{cp1.getX(), cp1.getY()}; 
  }

  public double[] last()
  {
    return new double[]{cp2.getX(), cp2.getY()}; 
  }
} // class CubicSegment
