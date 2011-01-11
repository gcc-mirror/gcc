/* Segment.java -- Abstract segment used for BasicStroke
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

public abstract class Segment implements Cloneable
{
  // Start and end points of THIS segment
  public Point2D P1;
  public Point2D P2;

  // Segments can be linked together internally as a linked list
  public Segment first;
  public Segment next;
  public Segment last;

  // Half the stroke width
  protected double radius;

  /**
   * Create a new, empty segment
   */
  public Segment()
  {
    P1 = P2 = null;
    first = this;
    next = null;
    last = this;
  }

  /**
   * Add a segment to the polygon
   * @param newsegment segment to add
   */
  public void add(Segment newsegment)
  {
    newsegment.first = first;
    last.next = newsegment;
    last = last.next.last;
  }

  /**
   * Reverses the orientation of the whole polygon
   */
  public void reverseAll()
  {
    reverse();
    first = last;
    Segment v = next;
    Segment former = this;
    next = null;

    while (v != null)
      {
        v.reverse();
        v.last = this;
        Segment oldnext = v.next;
        v.next = former;

        former = v;
        v = oldnext; // move to the next in list
      }
  }

  public String toString()
  {
    return "Segment:"+P1+", "+P2;
  }

  /**
   * Get the normal vector to the slope of the line.
   * @return vector of length radius, normal to the (x0,y0)-(x1,y1) vector)
   */
  protected double[] normal(double x0, double y0, double x1, double y1)
  {
    double dx = (x1 - x0);
    double dy = (y1 - y0);
    if( dy == 0 )
      {
        dy = radius * ((dx > 0)?1:-1);
        dx = 0;
      }
    else if( dx == 0 )
      {
        dx = radius * ((dy > 0)?-1:1);
        dy = 0;
      }
    else
      {
        double N = Math.sqrt(dx * dx + dy * dy);
        double odx = dx;
        dx = -radius * dy / N;
        dy = radius * odx / N;
      }
    return new double[]{ dx, dy };
  }

  /**
   * Reverse the current segment
   */
  public abstract void reverse();

  /**
   * Get the "top" and "bottom" segments of a segment.
   * First array element is p0 + normal, second is p0 - normal.
   */
  public abstract Segment[] getDisplacedSegments(double radius);

  /**
   * Returns the coordinates of the first control point, or the start point
   * for a line segment.
   */
  public abstract double[] cp1();

  /**
   * Returns the coordinates of the second control point, or the end point
   * for a line segment.
   */
  public abstract double[] cp2();

}
