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

import java.awt.Rectangle;
import java.awt.Shape;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 21, 2001
 */

public abstract class Line2D implements Shape, Cloneable
{
  protected Line2D ()
  {
  }

  public Object clone ()
  {
    try
      {
	return super.clone ();
      }
    catch (CloneNotSupportedException _)
      {
	// Can't happen.
	return null;
      }
  }

  public boolean contains (double x, double y)
  {
    double x1 = getX1 ();
    double t1 = (x - x1) / (getX2 () - x1);
    if (t1 < 0 || t1 > 1)
      return false;
    double y1 = getY1 ();
    double t2 = (y - y1) / (getY2 () - y1);
    // FIXME: use of == here is bogus
    return t2 >= 0 && t2 <= 1 && t1 == t2;
  }

  public boolean contains (double x, double y, double w, double h)
  {
    return false;
  }

  public boolean contains (Point2D p)
  {
    return contains (p.getX (), p.getY ());
  }

  public boolean contains (Rectangle2D r)
  {
    return false;
  }

  public Rectangle getBounds ()
  {
    double x1 = getX1 ();
    double y1 = getY1 ();
    double x2 = getX2 ();
    double y2 = getY2 ();

    double x = Math.min (x1, x2);
    double y = Math.min (y1, y2);
    double w = Math.abs (x1 - x2);
    double h = Math.abs (y1 - y2);

    return new Rectangle ((int) x, (int) y, (int) w, (int) h);
  }

  public abstract Point2D getP1 ();
  public abstract Point2D getP2 ();

  public PathIterator getPathIterator (AffineTransform at)
  {
    return getPathIterator (at, 0);
  }

  public PathIterator getPathIterator (AffineTransform at, double flatness)
  {
    return at.new Iterator (new Iterator ());
  }

  public abstract double getX1 ();
  public abstract double getY1 ();
  public abstract double getX2 ();
  public abstract double getY2 ();

  public boolean intersects (double x, double y, double w, double h)
  {
    double x1 = getX1 ();
    double y1 = getY1 ();
    double x2 = getX2 ();
    double y2 = getY2 ();

    if (x1 >= x && x1 <= x + w && y1 >= y && y1 <= y +h)
      return true;
    if (x2 >= x && x2 <= x + w && y2 >= y && y2 <= y +h)
      return true;

    double x3 = x + w;
    double y3 = y + h;

    return (linesIntersect (x1, y1, x2, y2, x, y, x, y3)
	    || linesIntersect (x1, y1, x2, y2, x, y3, x3, y3)
	    || linesIntersect (x1, y1, x2, y2, x3, y3, x3, y)
	    || linesIntersect (x1, y1, x2, y2, x3, y, x, y));
  }

  public boolean intersects (Rectangle2D r)
  {
    return intersects (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
  }

  public boolean intersectsLine (double x1, double y1, double x2, double y2)
  {
    return linesIntersect (getX1 (), getY1 (), getX2 (), getY2(),
			   x1, y1, x2, y2);
  }

  public boolean intersectsLine (Line2D l)
  {
    return linesIntersect (getX1 (), getY1 (), getX2 (), getY2(),
			   l.getX1 (), l.getY1 (), l.getX2 (), l.getY2 ());
  }

  public static boolean linesIntersect (double x1, double y1,
					double x2, double y2,
					double x3,double y3,
					double x4, double y4)
  {
    double beta = (((y1 - y3) * (x4 - x3) + (x1 - x3) * (y4 - y3))
		   / ((y2 - y1) * (x4 - x3) + (x2 - x1) * (y4 - y3)));
    if (beta < 0.0 || beta > 1.0)
      return false;
    double alpha = (x1 + beta * (x2 - x1) - x3) / (x4 - x3);
    return alpha >= 0.0 && alpha <= 1.0;
  }

  public double ptLineDist (double px, double py)
  {
    return ptLineDist (getX1 (), getY1 (), getX2 (), getY2 (),
		       px, py);
  }

  public static double ptLineDist (double x1, double y1,
				   double x2, double y2,
				   double px, double py)
  {
    return Math.sqrt (ptLineDistSq (x1, y1, x2, y2, px, py));
  }

  public double ptLineDist (Point2D p)
  {
    return ptLineDist (getX1 (), getY1 (), getX2 (), getY2 (),
		       p.getX (), p.getY ());
  }

  public double ptLineDistSq (double px, double py)
  {
    return ptLineDistSq (getX1 (), getY1 (), getX2 (), getY2 (),
			 px, py);
  }

  public static double ptLineDistSq (double x1, double y1,
				     double x2, double y2,
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

  public double ptLineDistSq (Point2D p)
  {
    return ptLineDistSq (getX1 (), getY1 (), getX2 (), getY2 (),
			 p.getX (), p.getY ());
  }

  public double ptSegDist (double px, double py)
  {
    return ptSegDist (getX1 (), getY1 (), getX2 (), getY2 (),
		      px, py);
  }

  public static double ptSegDist (double x1, double y1,
				   double x2, double y2,
				   double px, double py)
  {
    return Math.sqrt (ptSegDistSq (x1, y1, x2, y2, px, py));
  }

  public double ptSegDist (Point2D p)
  {
    return ptSegDist (getX1 (), getY1 (), getX2 (), getY2 (),
		      p.getX (), p.getY ());
  }

  public double ptSegDistSq (double px, double py)
  {
    return ptSegDistSq (getX1 (), getY1 (), getX2 (), getY2 (),
			px, py);
  }

  public static double ptSegDistSq (double x1, double y1,
				     double x2, double y2,
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

  public double ptSegDistSq (Point2D p)
  {
    return ptSegDistSq (getX1 (), getY1 (), getX2 (), getY2 (),
			p.getX (), p.getY ());
  }

  public int relativeCCW (double px, double py)
  {
    return relativeCCW (getX1 (), getY1 (),
			getX2 (), getY2 (),
			px, py);
  }

  public static int relativeCCW (double x1, double y1,
				 double x2, double y2,
				 double px, double py)
  {
    // This is a somewhat silly way to compute this.
    // Please write a better one.
    double a1 = Math.atan2 (y2 - y1, x2 - x1);
    double a2 = Math.atan2 (py - y1, px - x1);

    double a = (a1 - a2) % (2 * Math.PI);
    if (a == 0 || a == Math.PI)
      {
	double u = ((px - x1) * (x2 - x1) + (py - y1) * (y2 - y1));
	if (u < 0.0)
	  return 1;
	else if (u > 1.0)
	  return -1;
	else
	  return 0;
      }

    return (a > 0 && a < Math.PI) ? 1 : -1;
  }

  public int relativeCCW (Point2D p)
  {
    return relativeCCW (getX1 (), getY1 (),
			getX2 (), getY2 (),
			p.getX (), p.getY ());
  }

  public abstract void setLine (double x1, double y1, double x2, double y2);

  public void setLine (Line2D l)
  {
    setLine (l.getX1 (), l.getY1 (), l.getX2 (), l.getY2 ());
  }

  public void setLine (Point2D p1, Point2D p2)
  {
    setLine (p1.getX (), p1.getY (), p2.getX (), p2.getY ());
  }					     

  public static class Float extends Line2D
  {
    float x1, y1, x2, y2;

    public Float ()
    {
      this (0.0F, 0.0F, 0.0F, 0.0F);
    }

    public Float (float x1, float y1, float x2, float y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }

    public Float (Point2D p1, Point2D p2)
    {
      this.x1 = (float) p1.getX ();
      this.y1 = (float) p1.getY ();
      this.x2 = (float) p2.getX ();
      this.y2 = (float) p2.getY ();
    }

    public Rectangle2D getBounds2D ()
    {
      float x = Math.min (x1, x2);
      float w = Math.abs (x1 - x2);
      float y = Math.min (y1, y2);
      float h = Math.abs (y1 - y2);
      return new Rectangle2D.Float (x, y, w, h);
    }

    public Point2D getP1 ()
    {
      return new Point2D.Float (x1, y1);
    }

    public Point2D getP2 ()
    {
      return new Point2D.Float (x2, y2);
    }

    public double getX1 ()
    {
      return x1;
    }

    public double getY1 ()
    {
      return y1;
    }

    public double getX2 ()
    {
      return x2;
    }

    public double getY2 ()
    {
      return y2;
    }

    public void setLine (double x1, double y1, double x2, double y2)
    {
      this.x1 = (float) x1;
      this.y1 = (float) y1;
      this.x2 = (float) x2;
      this.y2 = (float) y2;
    }

    public void setLine (float x1, float y1, float x2, float y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }
  }

  public static class Double extends Line2D
  {
    double x1, y1, x2, y2;

    public Double ()
    {
      this (0.0, 0.0, 0.0, 0.0);
    }

    public Double (double x1, double y1, double x2, double y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }

    public Double (Point2D p1, Point2D p2)
    {
      this.x1 = (double) p1.getX ();
      this.y1 = p1.getY ();
      this.x2 = p2.getX ();
      this.y2 = p2.getY ();
    }

    public Rectangle2D getBounds2D ()
    {
      double x = Math.min (x1, x2);
      double w = Math.abs (x1 - x2);
      double y = Math.min (y1, y2);
      double h = Math.abs (y1 - y2);
      return new Rectangle2D.Double (x, y, w, h);
    }

    public Point2D getP1 ()
    {
      return new Point2D.Double (x1, y1);
    }

    public Point2D getP2 ()
    {
      return new Point2D.Double (x2, y2);
    }

    public double getX1 ()
    {
      return x1;
    }

    public double getY1 ()
    {
      return y1;
    }

    public double getX2 ()
    {
      return x2;
    }

    public double getY2 ()
    {
      return y2;
    }

    public void setLine (double x1, double y1, double x2, double y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      this.x2 = x2;
      this.y2 = y2;
    }
  }

  // This implements the PathIterator for all line objects that don't
  // override getPathIterator.
  private class Iterator implements PathIterator
  {
    // Current coordinate.
    private int coord;

    private static final int START = 0;
    private static final int END_PLUS_ONE = 2;

    public Iterator ()
    {
      coord = START;
    }

    public int currentSegment (double[] coords)
    {
      int r = SEG_MOVETO;
      if (coord == 0)
	{
	  coords[0] = getX1 ();
	  coords[1] = getY1 ();
	}
      else if (coord == 1)
	{
	  coords[0] = getX2 ();
	  coords[1] = getY2 ();
	}
      else
	r = SEG_CLOSE;

      return r;
    }

    public int currentSegment (float[] coords)
    {
      int r = SEG_MOVETO;
      if (coord == 0)
	{
	  coords[0] = (float) getX1 ();
	  coords[1] = (float) getY1 ();
	}
      else if (coord == 1)
	{
	  coords[0] = (float) getX2 ();
	  coords[1] = (float) getY2 ();
	}
      else
	r = SEG_CLOSE;

      return r;
    }

    public int getWindingRule ()
    {
      return WIND_NON_ZERO;
    }

    public boolean isDone ()
    {
      return coord == END_PLUS_ONE;
    }

    public void next ()
    {
      if (coord < END_PLUS_ONE)
	++coord;
    }
  }
}
