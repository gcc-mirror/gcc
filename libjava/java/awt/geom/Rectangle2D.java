/* Copyright (C) 2000, 2001, 2002  Free Software Foundation

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
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 16, 2000
 */

public abstract class Rectangle2D extends RectangularShape
{
  public static final int OUT_LEFT = 1;
  public static final int OUT_TOP = 2;
  public static final int OUT_RIGHT = 4;
  public static final int OUT_BOTTOM = 8;

  protected Rectangle2D ()
  {
  }

  /** Set the bounding box of this rectangle.
   * @param x X coordinate
   * @param y Y coordinate
   * @param w Width
   * @param h Height
   */
  public abstract void setRect (double x, double y, double w, double h);

  /** Set the bounding box of this rectangle.
   * @param r  Bounding rectangle.
   */
  public void setRect (Rectangle2D r)
  {
    setRect (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
  }

  /** Returns true if line segment intersects interior of this
   * rectangle.
   * @param x1 X coordinate of first end of line segment
   * @param y1 Y coordinate of first end of line segment
   * @param x2 X coordinate of second end of line segment
   * @param y2 Y coordinate of segment end of line segment
   */
  public boolean intersectsLine (double x1, double y1, double x2, double y2)
  {
    int o1 = outcode (x1, y1);
    int o2 = outcode (x2, y2);

    double mx = getX ();
    double my = getY ();
    double mx2 = getWidth ();
    double my2 = getHeight ();
    x1 -= mx;
    x2 -= mx;
    y1 -= my;
    y2 -= my;

    // Here we handle all lines that stay entirely on one side of the
    // rectangle.  We also handle some lines that intersect the
    // rectangle.  All vertical and horizontal lines are handled here.
    int xor = o1 ^ o2;
    int or  = o1 | o2;
    if ((xor & (OUT_BOTTOM | OUT_TOP)) == 0)
      {
	if ((or & (OUT_BOTTOM | OUT_TOP)) != 0)
	  return false;
	return ((o1 & (OUT_LEFT | OUT_RIGHT)) != 0
		|| (o2 & (OUT_LEFT | OUT_RIGHT)) != 0);
      }
    else if ((xor & (OUT_LEFT | OUT_RIGHT)) == 0)
      {
	if ((or & (OUT_LEFT | OUT_RIGHT)) != 0)
	  return false;
	return ((o1 & (OUT_BOTTOM | OUT_TOP)) != 0
		|| (o2 & (OUT_BOTTOM | OUT_TOP)) != 0);
      }

    double dx = x2 - x1;
    double dy = y2 - y1;

    double t1l = - x1 / dx;
    double t1h = (mx2 - x1) / dx;

    if (t1l >= t1h)
      return false;
    double t2l = - y1 / dy;
    double t2h = (my2 - y1) / dy;

    if (t2l >= t2h || t2l >= t1h || t2h < t1l)
      return false;
    return true;
  }

  /** Return true if line segment intersects interior of this
   * rectangle.
   * @param l The line segment
   */
//    public boolean intersectsLine (Line2D l)
//    {
//    }

  public abstract int outcode (double x, double y);

  public int outcode (Point2D p)
  {
    return outcode (p.getX (), p.getY ());
  }

  /** Set bounding frame for this rectangle.
   * @param x X coordinate
   * @param y Y coordinate
   * @param w Width
   * @param h Height
   */
  public void setFrame (double x, double y, double w, double h)
  {
    setRect (x, y, w, h);
  }

  public Rectangle2D getBounds2D ()
  {
    // FIXME.
    return (Rectangle2D) clone ();
  }

  public boolean contains (double x, double y)
  {
    double mx = getX ();
    double mw = getWidth ();
    if (x < mx || x >= mx + mw)
      return false;
    double my = getY ();
    double mh = getHeight ();
    return y >= my && y < my + mh;
  }

  public boolean intersects (double x, double y, double w, double h)
  {
    double mx = getX ();
    double mw = getWidth ();
    if (x < mx || x >= mx + mw || x + w < mx || x + w >= mx + mw)
      return false;
    double my = getY ();
    double mh = getHeight ();
    return y >= my && y < my + mh && y + h >= my && y + h < my + mh;
  }

  public boolean contains (double x, double y, double w, double h)
  {
    return contains (x, y) && contains (x + w, y + h);
  }

  public abstract Rectangle2D createIntersection (Rectangle2D r);

  public static void intersect (Rectangle2D src1, Rectangle2D src2,
				Rectangle2D dest)
  {
    double x1l = src1.getMinX ();
    double x2l = src2.getMinX ();
    double nxl = Math.max (x1l, x2l);
    double x1h = src1.getMaxX ();
    double x2h = src2.getMaxX ();
    double nxh = Math.min (x1h, x2h);
    if (nxh < nxl)
      nxh = nxl;
    double y1l = src1.getMinY ();
    double y2l = src2.getMinY ();
    double nyl = Math.max (y1l, y2l);
    double y1h = src1.getMaxY ();
    double y2h = src2.getMaxY ();
    double nyh = Math.min (y1h, y2h);
    if (nyh < nyl)
      nyh = nyl;
    dest.setFrameFromDiagonal (nxl, nyl, nxh, nyh);
  }

  public abstract Rectangle2D createUnion (Rectangle2D r);

  public static void union (Rectangle2D src1, Rectangle2D src2,
			    Rectangle2D dest)
  {
    double x1l = src1.getMinX ();
    double x2l = src2.getMinX ();
    double nxl = Math.max (x1l, x2l);
    double x1h = src1.getMaxX ();
    double x2h = src2.getMaxX ();
    double nxh = Math.min (x1h, x2h);
    double y1l = src1.getMinY ();
    double y2l = src2.getMinY ();
    double nyl = Math.max (y1l, y2l);
    double y1h = src1.getMaxY ();
    double y2h = src2.getMaxY ();
    double nyh = Math.min (y1h, y2h);
    dest.setFrameFromDiagonal (nxl, nyl, nxh, nyh);
  }

  public void add (double newx, double newy)
  {
    double minx = Math.min (getMinX (), newx);
    double maxx = Math.max (getMaxX (), newx);
    double miny = Math.min (getMinY (), newy);
    double maxy = Math.max (getMaxY (), newy);
    setFrameFromDiagonal (minx, miny, maxx, maxy);
  }

  public void add (Point2D p)
  {
    add (p.getX (), p.getY ());
  }

  public void add (Rectangle2D r)
  {
    add (r.getMinX (), r.getMinY ());
    add (r.getMaxX (), r.getMaxY ());
  }

  public PathIterator getPathIterator (AffineTransform at)
  {
    // We know the superclass just ignores the flatness parameter.
    return getPathIterator (at, 0);
  }

  public static class Double extends Rectangle2D
  {
    public double height;
    public double width;
    public double x;
    public double y;

    public Double ()
    {
      height = width = x = y = 0;
    }

    public Double (double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
    }

    public double getX ()
    {
      return x;
    }

    public double getY ()
    {
      return y;
    }

    public double getWidth ()
    {
      return width;
    }

    public double getHeight ()
    {
      return height;
    }

    public boolean isEmpty ()
    {
      return width <= 0 || height <= 0;
    }

    public void setRect (double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
    }

    public void setRect (Rectangle2D r)
    {
      this.x = r.getX ();
      this.y = r.getY ();
      this.width = r.getWidth ();
      this.height = r.getHeight ();
    }

    public int outcode (double x, double y)
    {
      int code = 0;
      if (x < this.x)
	code |= OUT_LEFT;
      else if (x >= this.x + this.width)
	code |= OUT_RIGHT;
      if (y < this.y)
	code |= OUT_TOP;
      else if (y >= this.y + this.height)
	code |= OUT_BOTTOM;
      return code;
    }

    public Rectangle2D getBounds2D ()
    {
      return new Rectangle2D.Double (x, y, width, height);
    }

    public Rectangle2D createIntersection (Rectangle2D r)
    {
      Double res = new Double ();
      intersect (this, r, res);
      return res;
    }

    public Rectangle2D createUnion (Rectangle2D r)
    {
      Double res = new Double ();
      union (this, r, res);
      return res;
    }

    public String toString ()
    {
      return "fixme";
    }
  }

  public static class Float extends Rectangle2D
  {
    public float height;
    public float width;
    public float x;
    public float y;

    public Float ()
    {
      height = width = x = y = 0;
    }

    public Float (float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
    }

    public double getX ()
    {
      return x;
    }

    public double getY ()
    {
      return y;
    }

    public double getWidth ()
    {
      return width;
    }

    public double getHeight ()
    {
      return height;
    }

    public boolean isEmpty ()
    {
      return width <= 0 || height <= 0;
    }

    public void setRect (double x, double y, double w, double h)
    {
      this.x = (float) x;
      this.y = (float) y;
      this.width = (float) w;
      this.height = (float) h;
    }

    public void setRect (float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      this.width = w;
      this.height = h;
    }

    public void setRect (Rectangle2D r)
    {
      this.x = (float) r.getX ();
      this.y = (float) r.getY ();
      this.width = (float) r.getWidth ();
      this.height = (float) r.getHeight ();
    }

    public int outcode (double x, double y)
    {
      int code = 0;
      if (x < this.x)
	code |= OUT_LEFT;
      else if (x >= this.x + this.width)
	code |= OUT_RIGHT;
      if (y < this.y)
	code |= OUT_TOP;
      else if (y >= this.y + this.height)
	code |= OUT_BOTTOM;
      return code;
    }

    public Rectangle2D getBounds2D ()
    {
      return new Rectangle2D.Float (x, y, width, height);
    }

    public Rectangle2D createIntersection (Rectangle2D r)
    {
      Float res = new Float ();
      intersect (this, r, res);
      return res;
    }

    public Rectangle2D createUnion (Rectangle2D r)
    {
      Float res = new Float ();
      union (this, r, res);
      return res;
    }

    public String toString ()
    {
      return "fixme";
    }
  }
}
