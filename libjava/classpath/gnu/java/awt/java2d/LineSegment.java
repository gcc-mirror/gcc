/* LineSegment.java -- Line segment used for BasicStroke
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

public class LineSegment extends Segment
{
  public LineSegment(double x1, double y1, double x2, double y2)
  {
    super();
    P1 = new Point2D.Double(x1, y1);
    P2 = new Point2D.Double(x2, y2);
  }

  public LineSegment(Point2D p1, Point2D p2)
  {
    super();
    P1 = (Point2D) p1.clone();
    P2 = (Point2D) p2.clone();
  }

  /**
   * Clones this segment
   */
  public Object clone()
  {
    LineSegment segment = null;
    
    try
      {
        segment = (LineSegment) super.clone();
        segment.P1 = (Point2D) P1.clone();
        segment.P2 = (Point2D) P2.clone();
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
   * Get the "top" and "bottom" segments of this segment.
   * First array element is p0 + normal, second is p0 - normal.
   */
  public Segment[] getDisplacedSegments(double radius)
  {
    this.radius = radius;
    double x0 = P1.getX();
    double y0 = P1.getY();
    double x1 = P2.getX();
    double y1 = P2.getY();
    double[] p = normal(x0, y0, x1, y1);
    Segment s1 = (new LineSegment(x0 + p[0], y0 + p[1],
                                  x1 + p[0], y1 + p[1] ));
    Segment s2 = (new LineSegment(x0 - p[0], y0 - p[1],
                                  x1 - p[0], y1 - p[1] ));
    return new Segment[]{s1, s2};
  }

  public void reverse()
  {
    Point2D p = P1;
    P1 = P2;
    P2 = p;
  }

  public double[] cp1()
  {
    return new double[]{P2.getX(), P2.getY()}; 
  }

  public double[] cp2()
  {
    return new double[]{P1.getX(), P1.getY()}; 
  }
} // class LineSegment
