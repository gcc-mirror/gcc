/* Ellipse2D.java -- represents an ellipse in 2-D space
   Copyright (C) 2000, 2002 Free Software Foundation

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
 * @author Eric Blake <ebb9@email.byu.edu>
 * @since 1.2
 * @status still needs documentation
 */
public abstract class Ellipse2D extends RectangularShape
{
  protected Ellipse2D()
  {
  }

  public boolean contains(double x, double y)
  {
    double rx = getWidth() / 2;
    double ry = getHeight() / 2;
    double tx = (x - getCenterX()) / rx;
    double ty = (y - getCenterY()) / ry;
    return tx * tx + ty * ty <= 1.0;
  }

  public boolean contains(double x, double y, double w, double h)
  {
    double x2 = x + w;
    double y2 = y + h;
    return (contains(x, y) && contains(x, y2)
            && contains(x2, y) && contains(x2, y2));
  }

  public PathIterator getPathIterator(AffineTransform at)
  {
    // An ellipse is just a complete arc.
    return new Arc2D.ArcIterator(this, at);
  }

  public boolean intersects(double x, double y, double w, double h)
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

    public Double()
    {
    }

    public Double(double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      height = h;
      width = w;
    }

    public Rectangle2D getBounds2D()
    {
      return new Rectangle2D.Double(x, y, width, height);
    }

    public double getHeight()
    {
      return height;
    }

    public double getWidth()
    {
      return width;
    }

    public double getX()
    {
      return x;
    }

    public double getY()
    {
      return y;
    }

    public boolean isEmpty()
    {
      return height <= 0 || width <= 0;
    }

    public void setFrame(double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      height = h;
      width = w;
    }
  } // class Double

  public static class Float extends Ellipse2D
  {
    public float height;
    public float width;
    public float x;
    public float y;

    public Float()
    {
    }

    public Float(float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      this.height = h;
      this.width = w;
    }

    public Rectangle2D getBounds2D()
    {
      return new Rectangle2D.Float(x, y, width, height);
    }

    public double getHeight()
    {
      return height;
    }

    public double getWidth()
    {
      return width;
    }

    public double getX()
    {
      return x;
    }

    public double getY()
    {
      return y;
    }

    public boolean isEmpty()
    {
      return height <= 0 || width <= 0;
    }

    public void setFrame(float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      height = h;
      width = w;
    }

    public void setFrame(double x, double y, double w, double h)
    {
      this.x = (float) x;
      this.y = (float) y;
      height = (float) h;
      width = (float) w;
    }
  } // class Float
} // class Ellipse2D
