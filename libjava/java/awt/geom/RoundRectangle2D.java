/* RoundRectangle2D.java -- represents a rectangle with rounded corners
   Copyright (C) 2000, 2002, 2003 Free Software Foundation

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

/** This class implements a rectangle with rounded corners.
 * @author Tom Tromey <tromey@cygnus.com>
 * @date December 3, 2000
 */
public abstract class RoundRectangle2D extends RectangularShape
{
  /** Return the arc height of this round rectangle.  */
  public abstract double getArcHeight();

  /** Return the arc width of this round rectangle.  */
  public abstract double getArcWidth();

  /** Set the values of this round rectangle
   * @param x The x coordinate
   * @param y The y coordinate
   * @param w The width
   * @param h The height
   * @param arcWidth The arc width
   * @param arcHeight The arc height
   */
  public abstract void setRoundRect(double x, double y, double w, double h,
                                     double arcWidth, double arcHeight);

  /** Create a RoundRectangle2D.  This is protected because this class
   * is abstract and cannot be instantiated.
   */
  protected  RoundRectangle2D()
  {
  }

  /** Return true if this object contains the specified point.
   * @param x The x coordinate
   * @param y The y coordinate
   */
  public boolean contains(double x, double y)
  {
    double mx = getX();
    double mw = getWidth();
    if (x < mx || x >= mx + mw)
      return false;
    double my = getY();
    double mh = getHeight();
    if (y < my || y >= my + mh)
      return false;

    // Now check to see if the point is in range of an arc.
    double dy = Math.min(Math.abs(my - y), Math.abs(my + mh - y));
    double dx = Math.min(Math.abs(mx - x), Math.abs(mx + mw - x));
    double aw = getArcWidth();
    double ah = getArcHeight();
    if (dx > aw || dy > ah)
      return true;

    // At this point DX represents the distance from the nearest edge
    // of the rectangle.  But we want to transform it to represent the
    // scaled distance from the center of the ellipse that forms the
    // arc.  Hence this code:
    dy = (ah - dy) / ah;
    dx = (aw - dx) / aw;

    return dx * dx + dy * dy <= 1.0;
  }

  /** Return true if this object contains the specified rectangle
   * @param x The x coordinate
   * @param y The y coordinate
   * @param w The width
   * @param h The height
   */
  public boolean contains(double x, double y, double w, double h)
  {
    // We have to check all four points here (for ordinary rectangles
    // we can just check opposing corners).
    return (contains(x, y) && contains(x + w, h)
            && contains(x, y + h) && contains(x + w, y + h));
  }

  /** Return a new path iterator which iterates over this rectangle.
   * @param at An affine transform to apply to the object
   */
  public PathIterator getPathIterator(final AffineTransform at)
  {
    final double minx = getX();
    final double miny = getY();
    final double maxx = minx + getWidth();
    final double maxy = miny + getHeight();
    final double arcwidth = getArcWidth();
    final double archeight = getArcHeight();
    return new PathIterator()
    {
      /** We iterate clockwise around the rectangle, starting in the
       * upper left.  This variable tracks our current point, which
       * can be on either side of a given corner.  */
      private int current = 0;

      /** Child path iterator, used for corners.  */
      private PathIterator corner;

      /** This is used when rendering the corners.  We re-use the arc
       * for each corner.  */
      private Arc2D arc = new Arc2D.Double();

      /** Temporary array used by getPoint.  */
      private double[] temp = new double[2];

      public int getWindingRule()
      {
	return WIND_NON_ZERO;
      }

      public boolean isDone()
      {
	return current > 9;
      }

      private void getPoint(int val)
      {
	switch (val)
	  {
	  case 0:
	  case 8:
	    temp[0] = minx;
	    temp[1] = miny + archeight;
	    break;
	  case 1:
	    temp[0] = minx + arcwidth;
	    temp[1] = miny;
	    break;
	  case 2:
	    temp[0] = maxx - arcwidth;
	    temp[1] = maxy;
	    break;
	  case 3:
	    temp[0] = maxx;
	    temp[1] = miny + archeight;
	    break;
	  case 4:
	    temp[0] = maxx;
	    temp[1] = maxy - archeight;
	    break;
	  case 5:
	    temp[0] = maxx - arcwidth;
	    temp[1] = maxy;
	    break;
	  case 6:
	    temp[0] = minx + arcwidth;
	    temp[1] = maxy;
	    break;
	  case 7:
	    temp[0] = minx;
	    temp[1] = maxy - archeight;
	    break;
	  }
      }

      public void next()
      {
	if (current >= 8)
	  ++current;
	else if (corner != null)
	  {
	    // We're iterating through the corner.  Work on the child
	    // iterator; if it finishes, reset and move to the next
	    // point along the rectangle.
	    corner.next();
	    if (corner.isDone())
	      {
		corner = null;
		++current;
	      }
	  }
	else
	  {
	    // Make an arc between this point on the rectangle and
	    // the next one, and then iterate over this arc.
	    getPoint(current);
	    double x1 = temp[0];
	    double y1 = temp[1];
	    getPoint(current + 1);
	    arc.setFrameFromDiagonal(x1, y1, temp[0], temp[1]);
	    arc.setAngles(x1, y1, temp[0], temp[1]);
	    corner = arc.getPathIterator(at);
	  }
      }

      public int currentSegment(float[] coords)
      {
	if (corner != null)
	  {
	    int r = corner.currentSegment(coords);
	    if (r == SEG_MOVETO)
	      r = SEG_LINETO;
	    return r;
	  }

	if (current < 9)
	  {
	    getPoint(current);
	    coords[0] = (float) temp[0];
	    coords[1] = (float) temp[1];
	  }
	else if (current == 9)
	  return SEG_CLOSE;
	else
	  throw new NoSuchElementException("rect iterator out of bounds");

	if (at != null)
	  at.transform(coords, 0, coords, 0, 1);
	return current == 0 ? SEG_MOVETO : SEG_LINETO;
      }

      public int currentSegment(double[] coords)
      {
	if (corner != null)
	  {
	    int r = corner.currentSegment(coords);
	    if (r == SEG_MOVETO)
	      r = SEG_LINETO;
	    return r;
	  }

	if (current < 9)
	  {
	    getPoint(current);
	    coords[0] = temp[0];
	    coords[1] = temp[1];
	  }
	else if (current == 9)
	  return SEG_CLOSE;
	else
	  throw new NoSuchElementException("rect iterator out of bounds");

	if (at != null)
	  at.transform(coords, 0, coords, 0, 1);
	return current == 0 ? SEG_MOVETO : SEG_LINETO;
      }
    };
  }

  /** Return true if the given rectangle intersects this shape.
   * @param x The x coordinate
   * @param y The y coordinate
   * @param w The width
   * @param h The height
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    // Here we can use the same code we use for an ordinary rectangle.
    double mx = getX();
    double mw = getWidth();
    if (x < mx || x >= mx + mw || x + w < mx || x + w >= mx + mw)
      return false;
    double my = getY();
    double mh = getHeight();
    return y >= my && y < my + mh && y + h >= my && y + h < my + mh;
  }

  /** Set the boundary of this round rectangle.
   * @param x The x coordinate
   * @param y The y coordinate
   * @param w The width
   * @param h The height
   */
  public void setFrame(double x, double y, double w, double h)
  {
    // This is a bit lame.
    setRoundRect(x, y, w, h, getArcWidth(), getArcHeight());
  }

  /** Set the values of this round rectangle to be the same as those
   * of the argument.
   * @param rr The round rectangle to copy
   */
  public void setRoundRect(RoundRectangle2D rr)
  {
    setRoundRect(rr.getX(), rr.getY(), rr.getWidth(), rr.getHeight(),
                  rr.getArcWidth(), rr.getArcHeight());
  }

  /** A subclass of RoundRectangle which keeps its parameters as
   * doubles.  */
  public static class Double extends RoundRectangle2D
  {
    /** The height of the corner arc.  */
    public double archeight;

    /** The width of the corner arc.  */
    public double arcwidth;

    /** The x coordinate of this object.  */
    public double x;

    /** The y coordinate of this object.  */
    public double y;

    /** The width of this object.  */
    public double width;

    /** The height of this object.  */
    public double height;

    /** Construct a new instance, with all parameters set to 0.  */
    public Double()
    {
    }

    /** Construct a new instance with the given arguments.
     * @param x The x coordinate
     * @param y The y coordinate
     * @param w The width
     * @param h The height
     * @param arcWidth The arc width
     * @param arcHeight The arc height
     */
    public Double(double x, double y, double w, double h,
                   double arcWidth, double arcHeight)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
      this.arcwidth = arcWidth;
      this.archeight = arcHeight;
    }

    public double getArcHeight()
    {
      return archeight;
    }

    public double getArcWidth()
    {
      return arcwidth;
    }

    public Rectangle2D getBounds2D()
    {
      return new Rectangle2D.Double(x, y, width, height);
    }

    public double getX()
    {
      return x;
    }

    public double getY()
    {
      return y;
    }

    public double getWidth()
    {
      return width;
    }

    public double getHeight()
    {
      return height;
    }

    public boolean isEmpty()
    {
      return width <= 0 || height <= 0;
    }

    public void setRoundRect(double x, double y, double w, double h,
                              double arcWidth, double arcHeight)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
      this.arcwidth = arcWidth;
      this.archeight = arcHeight;
    }
  } // class Double

  /** A subclass of RoundRectangle which keeps its parameters as
   * floats.  */
  public static class Float extends RoundRectangle2D
  {
    /** The height of the corner arc.  */
    public float archeight;

    /** The width of the corner arc.  */
    public float arcwidth;

    /** The x coordinate of this object.  */
    public float x;

    /** The y coordinate of this object.  */
    public float y;

    /** The width of this object.  */
    public float width;

    /** The height of this object.  */
    public float height;

    /** Construct a new instance, with all parameters set to 0.  */
    public Float()
    {
    }

    /** Construct a new instance with the given arguments.
     * @param x The x coordinate
     * @param y The y coordinate
     * @param w The width
     * @param h The height
     * @param arcWidth The arc width
     * @param arcHeight The arc height
     */
    public Float(float x, float y, float w, float h,
                  float arcWidth, float arcHeight)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
      this.arcwidth = arcWidth;
      this.archeight = arcHeight;
    }

    public double getArcHeight()
    {
      return archeight;
    }

    public double getArcWidth()
    {
      return arcwidth;
    }

    public Rectangle2D getBounds2D()
    {
      return new Rectangle2D.Float(x, y, width, height);
    }

    public double getX()
    {
      return x;
    }

    public double getY()
    {
      return y;
    }

    public double getWidth()
    {
      return width;
    }

    public double getHeight()
    {
      return height;
    }

    public boolean isEmpty()
    {
      return width <= 0 || height <= 0;
    }

    public void setRoundRect(float x, float y, float w, float h,
                              float arcWidth, float arcHeight)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
      this.arcwidth = arcWidth;
      this.archeight = arcHeight;
    }

    public void setRoundRect(double x, double y, double w, double h,
                              double arcWidth, double arcHeight)
    {
      this.x = (float) x;
      this.y = (float) y;
      this.width = (float) w;
      this.height = (float) h;
      this.arcwidth = (float) arcWidth;
      this.archeight = (float) arcHeight;
    }
  } // class Float
} // class RoundRectangle2D
