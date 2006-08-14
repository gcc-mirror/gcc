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


import java.awt.geom.CubicCurve2D;
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
    CubicSegment segment = null;
    
    try
      {
        segment = (CubicSegment) super.clone();
        
        segment.P1 = (Point2D) P1.clone();
        segment.P2 = (Point2D) P2.clone();
        segment.cp1 = (Point2D) cp1.clone();
        segment.cp2 = (Point2D) cp2.clone();
      }
    catch (CloneNotSupportedException cnse)
      {
        InternalError ie = new InternalError();
        ie.initCause(cnse);
        throw ie;
      }
  
    return segment;
  }

  /**
   * Get the "top" and "bottom" segments of this segment. First array element is
   * p0 + normal, second is p0 - normal.
   */
  public Segment[] getDisplacedSegments(double radius)
  {
    // It is, apparently, impossible to derive a curve parallel to a bezier
    // curve (unless it's a straight line), so we have no choice but to
    // approximate the displaced segments. Similar to FlattenPathIterator.

    Segment segmentTop = null;
    Segment segmentBottom = null;
    this.radius = radius;

    CubicCurve2D[] curves = new CubicCurve2D[10];
    curves[0] = new CubicCurve2D.Double(P1.getX(), P1.getY(), cp1.getX(),
                                        cp1.getY(), cp2.getX(), cp2.getY(),
                                        P2.getX(), P2.getY());
    int numCurves = 1;

    // Hard-coded a recursion limit of 10 and flatness of 1... should we make
    // this an option somewhere?
    while (numCurves > 0)
      {
        // The curve is flat enough, or we've reached our recursion limit,
        // so take the current start/end points and add it as a line segment
        // to our final approximated curves
        if (curves[numCurves - 1].getFlatnessSq() <= (radius / 3) || numCurves == 10)
          {
            Segment[] displaced = new LineSegment(
                                                  curves[numCurves - 1].getP1(),
                                                  curves[numCurves - 1].getP2()).getDisplacedSegments(radius);
            if (segmentTop == null)
              {
                segmentTop = displaced[0];
                segmentBottom = displaced[1];
              }
            else
              {
                segmentTop.add(displaced[0]);
                segmentBottom.add(displaced[1]);
              }
            numCurves--;
          }

        // Otherwise, subdivide again and continue
        else
          {
            CubicCurve2D left = new CubicCurve2D.Double();
            CubicCurve2D right = new CubicCurve2D.Double();
            curves[numCurves - 1].subdivide(left, right);
            curves[numCurves - 1] = right;
            curves[numCurves] = left;
            curves[numCurves - 1] = right;
            curves[numCurves] = left;
            numCurves++;
          }
      }

    return new Segment[] { segmentTop, segmentBottom };
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

  public double[] cp1()
  {
    return new double[]{cp1.getX(), cp1.getY()}; 
  }

  public double[] cp2()
  {
    return new double[]{cp2.getX(), cp2.getY()}; 
  }
} // class CubicSegment
