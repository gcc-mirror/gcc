/* Rectangle2D.java -- generic rectangles in 2-D space
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

import java.util.NoSuchElementException;

/**
 * This class describes a rectangle by a point (x,y) and dimension (w x h).
 * The actual storage is left up to subclasses.
 *
 * <p>It is valid for a rectangle to have negative width or height; but it
 * is considered to have no area or internal points. Therefore, the behavior
 * in methods like <code>contains</code> or <code>intersects</code> is
 * undefined unless the rectangle has positive width and height.
 *
 * @author Tom Tromey <tromey@cygnus.com>
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.2
 * @status updated to 1.4
 */
public abstract class Rectangle2D extends RectangularShape
{
  /**
   * The point lies left of the rectangle (p.x < r.x).
   *
   * @see #outcode()
   */
  public static final int OUT_LEFT = 1;

  /**
   * The point lies above the rectangle (p.y < r.y).
   *
   * @see #outcode()
   */
  public static final int OUT_TOP = 2;

  /**
   * The point lies right of the rectangle (p.x > r.maxX).
   *
   * @see #outcode()
   */
  public static final int OUT_RIGHT = 4;

  /**
   * The point lies below of the rectangle (p.y > r.maxY).
   *
   * @see #outcode()
   */
  public static final int OUT_BOTTOM = 8;

  /**
   * Default constructor.
   */
  protected Rectangle2D()
  {
  }

  /**
   * Set the bounding box of this rectangle.
   *
   * @param x the new X coordinate
   * @param y the new Y coordinate
   * @param w the new width
   * @param h the new height
   */
  public abstract void setRect(double x, double y, double w, double h);

  /**
   * Set the bounding box of this rectangle from the given one.
   *
   * @param r rectangle to copy
   * @throws NullPointerException if r is null
   */
  public void setRect(Rectangle2D r)
  {
    setRect(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }

  /**
   * Tests if the specified line intersects the interior of this rectangle.
   *
   * @param x1 the first x coordinate of line segment
   * @param y1 the first y coordinate of line segment
   * @param x2 the second x coordinate of line segment
   * @param y2 the second y coordinate of line segment
   * @return true if the line intersects the rectangle
   */
  public boolean intersectsLine(double x1, double y1, double x2, double y2)
  {
    double x = getX();
    double y = getY();
    double w = getWidth();
    double h = getHeight();
    if (w <= 0 || h <= 0)
      return false;

    if (x1 >= x && x1 <= x + w && y1 >= y && y1 <= y + h)
      return true;
    if (x2 >= x && x2 <= x + w && y2 >= y && y2 <= y + h)
      return true;

    double x3 = x + w;
    double y3 = y + h;

    return (Line2D.linesIntersect(x1, y1, x2, y2, x, y, x, y3)
            || Line2D.linesIntersect(x1, y1, x2, y2, x, y3, x3, y3)
            || Line2D.linesIntersect(x1, y1, x2, y2, x3, y3, x3, y)
            || Line2D.linesIntersect(x1, y1, x2, y2, x3, y, x, y));
  }

  /**
   * Tests if the specified line intersects the interior of this rectangle.
   *
   * @param l the line segment
   * @return true if the line intersects the rectangle
   * @throws NullPointerException if l is null
   */
  public boolean intersectsLine(Line2D l)
  {
    return intersectsLine(l.getX1(), l.getY1(), l.getX2(), l.getY2());
  }

  /**
   * Determine where the point lies with respect to this rectangle. The
   * result will be the binary OR of the appropriate bit masks.
   *
   * @param x the x coordinate to check
   * @param y the y coordinate to check
   * @return the binary OR of the result
   * @see #OUT_LEFT
   * @see #OUT_TOP
   * @see #OUT_RIGHT
   * @see #OUT_BOTTOM
   */
  public abstract int outcode(double x, double y);

  /**
   * Determine where the point lies with respect to this rectangle. The
   * result will be the binary OR of the appropriate bit masks.
   *
   * @param p the point to check
   * @return the binary OR of the result
   * @throws NullPointerException if p is null
   * @see #OUT_LEFT
   * @see #OUT_TOP
   * @see #OUT_RIGHT
   * @see #OUT_BOTTOM
   */
  public int outcode(Point2D p)
  {
    return outcode(p.getX(), p.getY());
  }

  /**
   * Set the bounding box of this rectangle.
   *
   * @param x the new X coordinate
   * @param y the new Y coordinate
   * @param w the new width
   * @param h the new height
   */
  public void setFrame(double x, double y, double w, double h)
  {
    setRect(x, y, w, h);
  }

  /**
   * Returns the bounds of this rectangle. A pretty useless method, as this
   * is already a rectangle.
   *
   * @return a copy of this rectangle
   */
  public Rectangle2D getBounds2D()
  {
    return (Rectangle2D) clone();
  }

  /**
   * Test if the given point is contained in the rectangle.
   *
   * @param x the x coordinate of the point
   * @param y the y coordinate of the point
   * @return true if (x,y) is in the rectangle
   */
  public boolean contains(double x, double y)
  {
    double mx = getX();
    double my = getY();
    double w = getWidth();
    double h = getHeight();
    return w > 0 && h > 0 && x >= mx && x < mx + w && y >= my && y < my + h;
  }

  /**
   * Tests if the given rectangle intersects this one. In other words, test if
   * the two rectangles share at least one internal point.
   *
   * @param x the x coordinate of the other rectangle
   * @param y the y coordinate of the other rectangle
   * @param w the width of the other rectangle
   * @param h the height of the other rectangle
   * @return true if the rectangles intersect
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    double mx = getX();
    double my = getY();
    double mw = getWidth();
    double mh = getHeight();
    return w > 0 && h > 0 && mw > 0 && mh > 0
      && x < mx + mw && x + w > mx && y < my + mh && y + h > my;
  }

  /**
   * Tests if this rectangle contains the given one. In other words, test if
   * this rectangle contains all points in the given one.
   *
   * @param x the x coordinate of the other rectangle
   * @param y the y coordinate of the other rectangle
   * @param w the width of the other rectangle
   * @param h the height of the other rectangle
   * @return true if this rectangle contains the other
   */
  public boolean contains(double x, double y, double w, double h)
  {
    double mx = getX();
    double my = getY();
    double mw = getWidth();
    double mh = getHeight();
    return w > 0 && h > 0 && mw > 0 && mh > 0
      && x >= mx && x + w <= mx + mw && y >= my && y + h <= my + mh;
  }

  /**
   * Return a new rectangle which is the intersection of this and the given
   * one. The result will be empty if there is no intersection.
   *
   * @param r the rectangle to be intersected
   * @return the intersection
   * @throws NullPointerException if r is null
   */
  public abstract Rectangle2D createIntersection(Rectangle2D r);

  /**
   * Intersects a pair of rectangles, and places the result in the
   * destination; this can be used to avoid object creation. This method
   * even works when the destination is also a source, although you stand
   * to lose the original data.
   *
   * @param src1 the first source
   * @param src2 the second source
   * @param dest the destination for the intersection
   * @throws NullPointerException if any rectangle is null
   */
  public static void intersect(Rectangle2D src1, Rectangle2D src2,
                               Rectangle2D dest)
  {
    double x = Math.max(src1.getX(), src2.getX());
    double y = Math.max(src1.getY(), src2.getY());
    double maxx = Math.min(src1.getMaxX(), src2.getMaxX());
    double maxy = Math.min(src1.getMaxY(), src2.getMaxY());
    dest.setRect(x, y, maxx - x, maxy - y);
  }

  /**
   * Return a new rectangle which is the union of this and the given one.
   *
   * @param r the rectangle to be merged
   * @return the union
   * @throws NullPointerException if r is null
   */
  public abstract Rectangle2D createUnion(Rectangle2D r);

  /**
   * Joins a pair of rectangles, and places the result in the destination;
   * this can be used to avoid object creation. This method even works when
   * the destination is also a source, although you stand to lose the
   * original data.
   *
   * @param src1 the first source
   * @param src2 the second source
   * @param dest the destination for the union
   * @throws NullPointerException if any rectangle is null
   */
  public static void union(Rectangle2D src1, Rectangle2D src2,
                           Rectangle2D dest)
  {
    double x = Math.min(src1.getX(), src2.getX());
    double y = Math.min(src1.getY(), src2.getY());
    double maxx = Math.max(src1.getMaxX(), src2.getMaxX());
    double maxy = Math.max(src1.getMaxY(), src2.getMaxY());
    dest.setRect(x, y, maxx - x, maxy - y);
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle
   * that contains both the existing rectangle and the specified point.
   * However, if the point falls on one of the two borders which are not
   * inside the rectangle, a subsequent call to <code>contains</code> may
   * return false.
   *
   * @param x the X coordinate of the point to add to this rectangle
   * @param y the Y coordinate of the point to add to this rectangle
   */
  public void add(double newx, double newy)
  {
    double minx = Math.min(getX(), newx);
    double maxx = Math.max(getMaxX(), newx);
    double miny = Math.min(getY(), newy);
    double maxy = Math.max(getMaxY(), newy);
    setRect(minx, miny, maxx - minx, maxy - miny);
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle
   * that contains both the existing rectangle and the specified point.
   * However, if the point falls on one of the two borders which are not
   * inside the rectangle, a subsequent call to <code>contains</code> may
   * return false.
   *
   * @param p the point to add to this rectangle
   * @throws NullPointerException if p is null
   */
  public void add(Point2D p)
  {
    add(p.getX(), p.getY());
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle
   * that contains both the existing rectangle and the specified rectangle.
   *
   * @param r the rectangle to add to this rectangle
   * @throws NullPointerException if r is null
   * @see #union(Rectangle2D)
   */
  public void add(Rectangle2D r)
  {
    union(this, r, this);
  }

  /**
   * Return an iterator along the shape boundary. If the optional transform
   * is provided, the iterator is transformed accordingly. Each call returns
   * a new object, independent from others in use. This iterator is thread
   * safe; modifications to the rectangle do not affect the results of this
   * path instance.
   *
   * @param transform an optional transform to apply to the iterator
   * @return a new iterator over the boundary
   * @since 1.2
   */
  public PathIterator getPathIterator(final AffineTransform at)
  {
    final double minx = getX();
    final double miny = getY();
    final double maxx = minx + getWidth();
    final double maxy = miny + getHeight();
    return new PathIterator()
    {
      /** Current coordinate. */
      private int current = (maxx <= minx && maxy <= miny) ? 6 : 0;

      public int getWindingRule()
      {
        // A test program showed that Sun J2SE 1.3.1 and 1.4.1_01
        // return WIND_NON_ZERO paths.  While this does not really
        // make any difference for rectangles (because they are not
        // self-intersecting), it seems appropriate to behave
        // identically.

        return WIND_NON_ZERO;
      }

      public boolean isDone()
      {
        return current > 5;
      }

      public void next()
      {
        current++;
      }

      public int currentSegment(float[] coords)
      {
        switch (current)
          {
          case 1:
            coords[0] = (float) maxx;
            coords[1] = (float) miny;
            break;
          case 2:
            coords[0] = (float) maxx;
            coords[1] = (float) maxy;
            break;
          case 3:
            coords[0] = (float) minx;
            coords[1] = (float) maxy;
            break;
          case 0:
          case 4:
            coords[0] = (float) minx;
            coords[1] = (float) miny;
            break;
          case 5:
            return SEG_CLOSE;
          default:
            throw new NoSuchElementException("rect iterator out of bounds");
          }
        if (at != null)
          at.transform(coords, 0, coords, 0, 1);
        return current == 0 ? SEG_MOVETO : SEG_LINETO;
      }

      public int currentSegment(double[] coords)
      {
        switch (current)
          {
          case 1:
            coords[0] = maxx;
            coords[1] = miny;
            break;
          case 2:
            coords[0] = maxx;
            coords[1] = maxy;
            break;
          case 3:
            coords[0] = minx;
            coords[1] = maxy;
            break;
          case 0:
          case 4:
            coords[0] = minx;
            coords[1] = miny;
            break;
          case 5:
            return SEG_CLOSE;
          default:
            throw new NoSuchElementException("rect iterator out of bounds");
          }
        if (at != null)
          at.transform(coords, 0, coords, 0, 1);
        return current == 0 ? SEG_MOVETO : SEG_LINETO;
      }
    };
  }

  /**
   * Return an iterator along the shape boundary. If the optional transform
   * is provided, the iterator is transformed accordingly. Each call returns
   * a new object, independent from others in use. This iterator is thread
   * safe; modifications to the rectangle do not affect the results of this
   * path instance. As the rectangle is already flat, the flatness parameter
   * is ignored.
   *
   * @param transform an optional transform to apply to the iterator
   * @param double the maximum distance for deviation from the real boundary
   * @return a new iterator over the boundary
   * @since 1.2
   */
  public PathIterator getPathIterator(AffineTransform at, double flatness)
  {
    return getPathIterator(at);
  }

  /**
   * Return the hashcode for this rectangle. The formula is not documented, but
   * appears to be the same as:
   * <pre>
   * long l = Double.doubleToLongBits(getX())
   *   + 37 * Double.doubleToLongBits(getY())
   *   + 43 * Double.doubleToLongBits(getWidth())
   *   + 47 * Double.doubleToLongBits(getHeight());
   * return (int) ((l >> 32) ^ l);
   * </pre>
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    // Talk about a fun time reverse engineering this one!
    long l = java.lang.Double.doubleToLongBits(getX())
      + 37 * java.lang.Double.doubleToLongBits(getY())
      + 43 * java.lang.Double.doubleToLongBits(getWidth())
      + 47 * java.lang.Double.doubleToLongBits(getHeight());
    return (int) ((l >> 32) ^ l);
  }

  /**
   * Tests this rectangle for equality against the specified object.  This
   * will be true if an only if the specified object is an instance of
   * Rectangle2D with the same coordinates and dimensions.
   *
   * @param obj the object to test against for equality
   * @return true if the specified object is equal to this one
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof Rectangle2D))
      return false;
    Rectangle2D r = (Rectangle2D) obj;
    return r.getX() == getX() && r.getY() == getY()
      && r.getWidth() == getWidth() && r.getHeight() == getHeight();
  }

  /**
   * This class defines a rectangle in <code>double</code> precision.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   * @since 1.2
   * @status updated to 1.4
   */
  public static class Double extends Rectangle2D
  {
    /** The x coordinate of the lower left corner. */
    public double x;

    /** The y coordinate of the lower left corner. */
    public double y;

    /** The width of the rectangle. */
    public double width;

    /** The height of the rectangle. */
    public double height;

    /**
     * Create a rectangle at (0,0) with width 0 and height 0.
     */
    public Double()
    {
    }

    /**
     * Create a rectangle with the given values.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    public Double(double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      width = w;
      height = h;
    }

    /**
     * Return the X coordinate.
     *
     * @return the value of x
     */
    public double getX()
    {
      return x;
    }

    /**
     * Return the Y coordinate.
     *
     * @return the value of y
     */
    public double getY()
    {
      return y;
    }

    /**
     * Return the width.
     *
     * @return the value of width
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Return the height.
     *
     * @return the value of height
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Test if the rectangle is empty.
     *
     * @return true if width or height is not positive
     */
    public boolean isEmpty()
    {
      return width <= 0 || height <= 0;
    }

    /**
     * Set the contents of this rectangle to those specified.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    public void setRect(double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      width = w;
      height = h;
    }

    /**
     * Set the contents of this rectangle to those specified.
     *
     * @param r the rectangle to copy
     * @throws NullPointerException if r is null
     */
    public void setRect(Rectangle2D r)
    {
      x = r.getX();
      y = r.getY();
      width = r.getWidth();
      height = r.getHeight();
    }

    /**
     * Determine where the point lies with respect to this rectangle. The
     * result will be the binary OR of the appropriate bit masks.
     *
     * @param x the x coordinate to check
     * @param y the y coordinate to check
     * @return the binary OR of the result
     * @see #OUT_LEFT
     * @see #OUT_TOP
     * @see #OUT_RIGHT
     * @see #OUT_BOTTOM
     * @since 1.2
     */
    public int outcode(double x, double y)
    {
      int result = 0;
      if (width <= 0)
        result |= OUT_LEFT | OUT_RIGHT;
      else if (x < this.x)
        result |= OUT_LEFT;
      else if (x > this.x + width)
        result |= OUT_RIGHT;
      if (height <= 0)
        result |= OUT_BOTTOM | OUT_TOP;
      else if (y < this.y) // Remember that +y heads top-to-bottom.
        result |= OUT_TOP;
      else if (y > this.y + height)
        result |= OUT_BOTTOM;
      return result;
    }

    /**
     * Returns the bounds of this rectangle. A pretty useless method, as this
     * is already a rectangle.
     *
     * @return a copy of this rectangle
     */
    public Rectangle2D getBounds2D()
    {
      return new Double(x, y, width, height);
    }

    /**
     * Return a new rectangle which is the intersection of this and the given
     * one. The result will be empty if there is no intersection.
     *
     * @param r the rectangle to be intersected
     * @return the intersection
     * @throws NullPointerException if r is null
     */
    public Rectangle2D createIntersection(Rectangle2D r)
    {
      Double res = new Double();
      intersect(this, r, res);
      return res;
    }

    /**
     * Return a new rectangle which is the union of this and the given one.
     *
     * @param r the rectangle to be merged
     * @return the union
     * @throws NullPointerException if r is null
     */
    public Rectangle2D createUnion(Rectangle2D r)
    {
      Double res = new Double();
      union(this, r, res);
      return res;
    }

    /**
     * Returns a string representation of this rectangle. This is in the form
     * <code>getClass().getName() + "[x=" + x + ",y=" + y + ",w=" + width
     * + ",h=" + height + ']'</code>.
     *
     * @return a string representation of this rectangle
     */
    public String toString()
    {
      return getClass().getName() + "[x=" + x + ",y=" + y + ",w=" + width
        + ",h=" + height + ']';
    }
  } // class Double

  /**
   * This class defines a rectangle in <code>float</code> precision.
   *
   * @author Eric Blake <ebb9@email.byu.edu>
   * @since 1.2
   * @status updated to 1.4
   */
  public static class Float extends Rectangle2D
  {
    /** The x coordinate of the lower left corner. */
    public float x;

    /** The y coordinate of the lower left corner. */
    public float y;

    /** The width of the rectangle. */
    public float width;

    /** The height of the rectangle. */
    public float height;

    /**
     * Create a rectangle at (0,0) with width 0 and height 0.
     */
    public Float()
    {
    }

    /**
     * Create a rectangle with the given values.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    public Float(float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      width = w;
      height = h;
    }

    /**
     * Create a rectangle with the given values.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    Float(double x, double y, double w, double h)
    {
      this.x = (float) x;
      this.y = (float) y;
      width = (float) w;
      height = (float) h;
    }

    /**
     * Return the X coordinate.
     *
     * @return the value of x
     */
    public double getX()
    {
      return x;
    }

    /**
     * Return the Y coordinate.
     *
     * @return the value of y
     */
    public double getY()
    {
      return y;
    }

    /**
     * Return the width.
     *
     * @return the value of width
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Return the height.
     *
     * @return the value of height
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Test if the rectangle is empty.
     *
     * @return true if width or height is not positive
     */
    public boolean isEmpty()
    {
      return width <= 0 || height <= 0;
    }

    /**
     * Set the contents of this rectangle to those specified.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    public void setRect(float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      width = w;
      height = h;
    }

    /**
     * Set the contents of this rectangle to those specified.
     *
     * @param x the x coordinate
     * @param y the y coordinate
     * @param w the width
     * @param h the height
     */
    public void setRect(double x, double y, double w, double h)
    {
      this.x = (float) x;
      this.y = (float) y;
      width = (float) w;
      height = (float) h;
    }

    /**
     * Set the contents of this rectangle to those specified.
     *
     * @param r the rectangle to copy
     * @throws NullPointerException if r is null
     */
    public void setRect(Rectangle2D r)
    {
      x = (float) r.getX();
      y = (float) r.getY();
      width = (float) r.getWidth();
      height = (float) r.getHeight();
    }

    /**
     * Determine where the point lies with respect to this rectangle. The
     * result will be the binary OR of the appropriate bit masks.
     *
     * @param x the x coordinate to check
     * @param y the y coordinate to check
     * @return the binary OR of the result
     * @see #OUT_LEFT
     * @see #OUT_TOP
     * @see #OUT_RIGHT
     * @see #OUT_BOTTOM
     * @since 1.2
     */
    public int outcode(double x, double y)
    {
      int result = 0;
      if (width <= 0)
        result |= OUT_LEFT | OUT_RIGHT;
      else if (x < this.x)
        result |= OUT_LEFT;
      else if (x > this.x + width)
        result |= OUT_RIGHT;
      if (height <= 0)
        result |= OUT_BOTTOM | OUT_TOP;
      else if (y < this.y) // Remember that +y heads top-to-bottom.
        result |= OUT_TOP;
      else if (y > this.y + height)
        result |= OUT_BOTTOM;
      return result;
    }

    /**
     * Returns the bounds of this rectangle. A pretty useless method, as this
     * is already a rectangle.
     *
     * @return a copy of this rectangle
     */
    public Rectangle2D getBounds2D()
    {
      return new Float(x, y, width, height);
    }

    /**
     * Return a new rectangle which is the intersection of this and the given
     * one. The result will be empty if there is no intersection.
     *
     * @param r the rectangle to be intersected
     * @return the intersection
     * @throws NullPointerException if r is null
     */
    public Rectangle2D createIntersection(Rectangle2D r)
    {
      Float res = new Float();
      intersect(this, r, res);
      return res;
    }

    /**
     * Return a new rectangle which is the union of this and the given one.
     *
     * @param r the rectangle to be merged
     * @return the union
     * @throws NullPointerException if r is null
     */
    public Rectangle2D createUnion(Rectangle2D r)
    {
      Float res = new Float();
      union(this, r, res);
      return res;
    }

    /**
     * Returns a string representation of this rectangle. This is in the form
     * <code>getClass().getName() + "[x=" + x + ",y=" + y + ",w=" + width
     * + ",h=" + height + ']'</code>.
     *
     * @return a string representation of this rectangle
     */
    public String toString()
    {
      return getClass().getName() + "[x=" + x + ",y=" + y + ",w=" + width
        + ",h=" + height + ']';
    }
  } // class Float
} // class Rectangle2D
