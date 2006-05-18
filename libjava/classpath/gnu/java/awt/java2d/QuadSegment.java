/* QuadSegment.java -- QuadCurve segment used for BasicStroke
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
import java.awt.geom.QuadCurve2D;

/**
 * Quadratic Bezier curve segment
 *
 * Note: Most peers don't support quadratics directly, so it might make
 * sense to represent them as cubics internally and just be done with it.
 * I think we should be peer-agnostic, however, and stay faithful to the
 * input geometry types as far as possible.
 */
public class QuadSegment extends Segment
{
  public Point2D cp; // control point

  /**
   * Constructor, takes the coordinates of the start, control,
   * and end point, respectively.
   */
  public QuadSegment(double x1, double y1, double cx, double cy, double x2,
                     double y2)
  {
    super();
    P1 = new Point2D.Double(x1, y1);
    P2 = new Point2D.Double(x2, y2);
    cp = new Point2D.Double(cx, cy);
  }

  public QuadSegment(Point2D p1, Point2D cp, Point2D p2)
  {
    super();
    P1 = p1;
    P2 = p2;
    this.cp = cp;
  }

  public QuadSegment(QuadCurve2D curve)
  {
    super();
    P1 = curve.getP1();
    P2 = curve.getP2();
    this.cp = curve.getCtrlPt();
  }

  /**
   * Clones this segment
   */
  public Object clone()
  {
    return new QuadSegment(P1.getX(), P1.getY(), cp.getX(), cp.getY(),
                           P2.getX(), P2.getY());
  }

  /**
   * Get the "top" and "bottom" segments of a given segment.
   * First array element is p0 + normal, second is p0 - normal.
   */
  public Segment[] getDisplacedSegments(double radius)
  {
    this.radius = radius;
    double x0 = P1.getX();
    double y0 = P1.getY();
    double x1 = cp.getX();
    double y1 = cp.getY();
    double x2 = P2.getX();
    double y2 = P2.getY();

    QuadCurve2D left = new QuadCurve2D.Double();
    QuadCurve2D right = new QuadCurve2D.Double();
    QuadCurve2D orig = new QuadCurve2D.Double(x0, y0, x1, y1, x2, y2);
    orig.subdivide(left, right);

    QuadSegment s1 = offsetSubdivided(left, true);
    QuadSegment s2 = offsetSubdivided(left, false);

    s1.add( offsetSubdivided(right, true) );
    s2.add( offsetSubdivided(right, false) );

    return new Segment[]{s1, s2};
  }
  
  private QuadSegment offsetSubdivided(QuadCurve2D curve, boolean plus)
  {
    double[] n1 = normal(curve.getX1(), curve.getY1(), 
                         curve.getCtrlX(), curve.getCtrlY());
    double[] n2 = normal(curve.getCtrlX(), curve.getCtrlY(),
                         curve.getX2(), curve.getY2());

    Point2D cp;
    QuadSegment s;
    if( plus )
      {
        cp = lineIntersection(curve.getX1() + n1[0], 
                              curve.getY1() + n1[1],
                              curve.getCtrlX() + n1[0],
                              curve.getCtrlY() + n1[1],
                              curve.getCtrlX() + n2[0],
                              curve.getCtrlY() + n2[1],
                              curve.getX2() + n2[0], 
                              curve.getY2() + n2[1], true);
        s = new QuadSegment(curve.getX1() + n1[0], curve.getY1() + n1[1],
                            cp.getX(), cp.getY(),
                            curve.getX2() + n2[0], curve.getY2() + n2[1]);
      }
    else
      {
        cp = lineIntersection(curve.getX1() - n1[0], 
                              curve.getY1() - n1[1],
                              curve.getCtrlX() - n1[0],
                              curve.getCtrlY() - n1[1],
                              curve.getCtrlX() - n2[0],
                              curve.getCtrlY() - n2[1],
                              curve.getX2() - n2[0], 
                              curve.getY2() - n2[1], true);

        s = new QuadSegment(curve.getX1() - n1[0], curve.getY1() - n1[1],
                            cp.getX(), cp.getY(),
                            curve.getX2() - n2[0], curve.getY2() - n2[1]);
      }

    return s;
  }

  private Point2D lineIntersection(double X1, double Y1, 
                                   double X2, double Y2, 
                                   double X3, double Y3, 
                                   double X4, double Y4,
                                   boolean infinite)
  {
    double x1 = X1;
    double y1 = Y1;
    double rx = X2 - x1;
    double ry = Y2 - y1;

    double x2 = X3;
    double y2 = Y3;
    double sx = X4 - x2;
    double sy = Y4 - y2;

    double determinant = sx * ry - sy * rx;
    double nom = (sx * (y2 - y1) + sy * (x1 - x2));

    // lines can be considered parallel.
    if (Math.abs(determinant) < 1E-6)
      return null;

    nom = nom / determinant;

    // check if lines are within the bounds
    if(!infinite && (nom > 1.0 || nom < 0.0))
      return null;

    return new Point2D.Double(x1 + nom * rx, y1 + nom * ry);
  }

  public void reverse()
  {
    Point2D p = P1;
    P1 = P2;
    P2 = p;
  }

  public double[] first()
  {
    return new double[]{cp.getX(), cp.getY()}; 
  }

  public double[] last()
  {
    return new double[]{cp.getX(), cp.getY()}; 
  }
}
