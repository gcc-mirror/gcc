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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.awt.geom;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 16, 2000
 */

public abstract class Ellipse2D extends RectangularShape
{
  protected Ellipse2D ()
  {
  }

  public boolean contains (double x, double y)
  {
    double rx = getWidth () / 2;
    double ry = getHeight () / 2;
    double tx = (x - getCenterX ()) / rx;
    double ty = (y - getCenterY ()) / ry;
    return tx * tx + ty * ty <= 1.0;
  }

  public boolean contains (double x, double y, double w, double h)
  {
    double x2 = x + w;
    double y2 = y + h;
    return (contains (x, y) && contains (x, y2)
	    && contains (x2, y) && contains (x2, y2));
  }

  public PathIterator getPathIterator (AffineTransform at)
  {
    // fixme;
    return null;
  }

  public boolean intersects (double x, double y, double w, double h)
  {
    // fixme
    return false;
  }

  public static class Double extends Ellipse2D
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
      this.height = h;
      this.width = w;
    }

    public Rectangle2D getBounds2D ()
    {
      return new Rectangle2D.Double (x, y, width, height);
    }

    public double getHeight ()
    {
      return height;
    }

    public double getWidth ()
    {
      return width;
    }

    public double getX ()
    {
      return x;
    }

    public double getY ()
    {
      return y;
    }

    public boolean isEmpty ()
    {
      return height <= 0 || width <= 0;
    }

    public void setFrame (double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      this.height = h;
      this.width = w;
    }
  }

  public static class Float extends Ellipse2D
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
      this.height = h;
      this.width = w;
    }

    public Rectangle2D getBounds2D ()
    {
      return new Rectangle2D.Float (x, y, width, height);
    }

    public double getHeight ()
    {
      return height;
    }

    public double getWidth ()
    {
      return width;
    }

    public double getX ()
    {
      return x;
    }

    public double getY ()
    {
      return y;
    }

    public boolean isEmpty ()
    {
      return height <= 0 || width <= 0;
    }

    public void setFrame (float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      this.height = h;
      this.width = w;
    }

    public void setFrame (double x, double y, double w, double h)
    {
      this.x = (float) x;
      this.y = (float) y;
      this.height = (float) h;
      this.width = (float) w;
    }
  }
}
