/* Copyright (C) 2000, 2002  Free Software Foundation

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
import java.awt.*;
import java.awt.geom.Rectangle2D;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 16, 2000
 */

public abstract class RectangularShape implements Shape, Cloneable
{
  protected RectangularShape ()
  {
  }

  public abstract double getX ();
  public abstract double getY ();
  public abstract double getWidth ();
  public abstract double getHeight ();

  public double getMinX ()
  {    
    return Math.min (getX (), getX () + getWidth ());
  }

  public double getMinY ()
  {
    return Math.min (getY (), getY () + getHeight ());
  }

  public double getMaxX ()
  {    
    return Math.max (getX (), getX () + getWidth ());
  }

  public double getMaxY ()
  {
    return Math.max (getY (), getY () + getHeight ());
  }

  public double getCenterX ()
  {
    return getX () + getWidth () / 2;
  }

  public double getCenterY ()
  {
    return getY () + getHeight () / 2;
  }

  public Rectangle2D getFrame ()
  {
    return new Rectangle2D.Double (getX (), getY (),
				   getWidth (), getHeight ());
  }

  public abstract boolean isEmpty ();
  public abstract void setFrame (double x, double y, double w, double h);

  public void setFrame (Point2D loc, Dimension2D size)
  {
    setFrame (loc.getX (), loc.getY (), size.getWidth (), size.getHeight ());
  }

  public void setFrame (Rectangle2D r)
  {
    setFrame (r.getX (), r.getY (), r.getWidth (), r.getHeight ());
  }

  public void setFrameFromDiagonal (double x1, double y1,
				    double x2, double y2)
  {
    if (x1 > x2)
      {
	double t = x2;
	x2 = x1;
	x1 = t;
      }
    if (y1 > y2)
      {
	double t = y2;
	y2 = y1;
	y1 = t;
      }
    setFrame (x1, y1, x2 - x1, y2 - y1);
  }

  public void setFrameFromDiagonal (Point2D p1, Point2D p2)
  {
    setFrameFromDiagonal (p1.getX (), p1.getY (),
			  p2.getX (), p2.getY ());
  }

  public void setFrameFromCenter (double centerX, double centerY,
				  double cornerX, double cornerY)
  {
    double halfw = Math.abs (cornerX - centerX);
    double halfh = Math.abs (cornerY - centerY);
    setFrame (centerX - halfw, centerY - halfh,
	      2 * halfw, 2 * halfh);
  }

  public void setFrameFromCenter (Point2D center, Point2D corner)
  {
    setFrameFromCenter (center.getX (), center.getY (),
			corner.getX (), corner.getY ());
  }

  public boolean contains (Point2D p)
  {
    double x = p.getX ();
    double y = p.getY ();
    double rx = getX ();
    double ry = getY ();
    double w = getWidth ();
    double h = getHeight ();
    return x >= rx && x < rx + w && y >= ry && y < ry + h;
  }

  public boolean intersects (Rectangle2D r)
  {
    double x = getX ();
    double w = getWidth ();
    double mx = r.getX ();
    double mw = r.getWidth ();
    if (x < mx || x >= mx + mw || x + w < mx || x + w >= mx + mw)
      return false;
    double y = getY ();
    double h = getHeight ();
    double my = r.getY ();
    double mh = r.getHeight ();
    return y >= my && y < my + mh && y + h >= my && y + h < my + mh;
  }

  private boolean containsPoint (double x, double y)
  {
    double mx = getX ();
    double mw = getWidth ();
    if (x < mx || x >= mx + mw)
      return false;
    double my = getY ();
    double mh = getHeight ();
    return y >= my && y < my + mh;
  }

  public boolean contains (Rectangle2D r)
  {
    return (containsPoint (r.getMinX (), r.getMinY ())
	    && containsPoint (r.getMaxX (), r.getMaxY ()));
  }

  public Rectangle getBounds ()
  {
    return new Rectangle ((int) getX (), (int) getY (),
			  (int) getWidth (), (int) getHeight ());
  }

  public PathIterator getPathIterator (AffineTransform at, double flatness)
  {
    return at.new Iterator (new Iterator ());
  }

  public Object clone ()
  {
    try
    {
      return super.clone ();
    } 
    catch (CloneNotSupportedException _) {return null;}
  }

  // This implements the PathIterator for all RectangularShape objects
  // that don't override getPathIterator.
  private class Iterator implements PathIterator
  {
    // Our current coordinate.
    private int coord;

    private static final int START = 0;
    private static final int END_PLUS_ONE = 5;

    public Iterator ()
    {
      coord = START;
    }

    public int currentSegment (double[] coords)
    {
      int r;
      switch (coord)
	{
	case 0:
	  coords[0] = getX ();
	  coords[1] = getY ();
	  r = SEG_MOVETO;
	  break;

	case 1:
	  coords[0] = getX () + getWidth ();
	  coords[1] = getY ();
	  r = SEG_LINETO;
	  break;

	case 2:
	  coords[0] = getX () + getWidth ();
	  coords[1] = getY () + getHeight ();
	  r = SEG_LINETO;
	  break;

	case 3:
	  coords[0] = getX ();
	  coords[1] = getY () + getHeight ();
	  r = SEG_LINETO;
	  break;

	case 4:
	  r = SEG_CLOSE;
	  break;	  

	default:
	  // It isn't clear what to do if the caller calls us after
	  // isDone returns true.
	  r = SEG_CLOSE;
	  break;
	}

      return r;
    }

    public int currentSegment (float[] coords)
    {
      int r;
      switch (coord)
	{
	case 0:
	  coords[0] = (float) getX ();
	  coords[1] = (float) getY ();
	  r = SEG_MOVETO;
	  break;

	case 1:
	  coords[0] = (float) (getX () + getWidth ());
	  coords[1] = (float) getY ();
	  r = SEG_LINETO;
	  break;

	case 2:
	  coords[0] = (float) (getX () + getWidth ());
	  coords[1] = (float) (getY () + getHeight ());
	  r = SEG_LINETO;
	  break;

	case 3:
	  coords[0] = (float) getX ();
	  coords[1] = (float) (getY () + getHeight ());
	  r = SEG_LINETO;
	  break;

	case 4:
	default:
	  // It isn't clear what to do if the caller calls us after
	  // isDone returns true.  We elect to keep returning
	  // SEG_CLOSE.
	  r = SEG_CLOSE;
	  break;	  
	}

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
