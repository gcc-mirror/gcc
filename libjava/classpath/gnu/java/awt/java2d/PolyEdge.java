/* PolyEdge.java -- An edge in a polygon, used for polygon filling
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

import gnu.java.math.Fixed;

/**
 * An edge in a polygon.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
final class PolyEdge
  implements Comparable
{

  /**
   * The start and end coordinates of the edge. y0 is always smaller or equal
   * than y1.
   *
   * These values are stored as fixed-point decimals.
   */
  public int x0, y0, x1, y1;

  /**
   * The slope of the edge. This is dx / dy.
   *
   * This is a fixed point decimal.
   */
  private int slope;

  /**
   * The intersection of this edge with the current scanline.
   *
   * This is a fixed point decimal.
   */
  int xIntersection;

  /**
   * Indicates whether this edge is from the clip or from the target shape.
   */
  boolean isClip;

  /**
   * Implements a linked list for the edge pool.
   */
  PolyEdge poolNext;

  /**
   * Implements a linked list for the scanline edge lists.
   */
  PolyEdge scanlineNext;

  /**
   * Create an uninitialized edge.
   */
  PolyEdge()
  {
    // Nothing to do here.
  }

  /**
   * Creates a new PolyEdge with the specified coordinates.
   *
   * @param x0 the starting point, x coordinate
   * @param y0 the starting point, y coordinate
   * @param x1 the end point, x coordinate
   * @param y1 the end point, y coordinate
   */
  PolyEdge(int n, int x0, int y0, int x1, int y1, boolean clip)
  {
    init(n, x0, y0, x1, y1, clip);
  }

  /**
   * (Re-) Initializes this edge.
   *
   * @param x0
   * @param y0
   * @param x1
   * @param y1
   */
  void init(int n, int x0, int y0, int x1, int y1, boolean clip)
  {
    isClip = clip;
    if (y0 < y1)
      {
        this.x0 = x0;
        this.y0 = y0;
        this.x1 = x1;
        this.y1 = y1;
      }
    else
      {
        this.x0 = x1;
        this.y0 = y1;
        this.x1 = x0;
        this.y1 = y0;
      }
    slope = Fixed.div(n, this.x1 - this.x0, this.y1 - this.y0);
  }

  /**
   * Sorts PolyEdges by the x coordinate from the minimum x value.
   */
  public int compareTo(Object o)
  {
    PolyEdge other = (PolyEdge) o;
    int comp = 0;
    if (x0 < other.x0)
      comp = -1;
    else if (x0 > other.x0)
      comp = 1;
    return comp;
  }

  /**
   * Intersects this edge with the scanline at height y. The result is
   * stored in {@link #xIntersection}.
   *
   * @param y the scanline
   */
  void intersect(int n, int y)
  {
    int dy = y - y0;
    int dx = Fixed.mul(n, slope, dy);
    xIntersection = x0 + dx;
  }

  public String toString()
  {
    return "Edge: " + x0 + ", " + y0 + ", " + x1 + ", " + y1 + ", slope: "
           + slope + ", xIntersection: " + xIntersection
           + ", isClip: " + isClip;
  }
}
