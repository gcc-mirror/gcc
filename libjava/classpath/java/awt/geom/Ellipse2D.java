/* Ellipse2D.java -- represents an ellipse in 2-D space
   Copyright (C) 2000, 2002, 2004 Free Software Foundation

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

package java.awt.geom;


/**
 * Ellipse2D is the shape of an ellipse.
 * <BR>
 * <img src="doc-files/Ellipse-1.png" width="347" height="221"
 * alt="A drawing of an ellipse" /><BR>
 * The ellipse is defined by it's bounding box (shown in red),
 * and is defined by the implicit curve:<BR>
 * <blockquote>(<i>x</i>/<i>a</i>)<sup>2</sup> +
 * (<i>y</i>/<i>b</i>)<sup>2</sup> = 1<BR><BR></blockquote>
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 *
 * @since 1.2
 */
public abstract class Ellipse2D extends RectangularShape
{
  /**
   * Ellipse2D is defined as abstract.
   * Implementing classes are Ellipse2D.Float and Ellipse2D.Double.
   */
  protected Ellipse2D()
  {
  }

  /**
   * Determines if a point is contained within the ellipse. <P>
   * @param x - x coordinate of the point.
   * @param y - y coordinate of the point.
   * @return true if the point is within the ellipse, false otherwise.
   */
  public boolean contains(double x, double y)
  {
    double rx = getWidth() / 2;
    double ry = getHeight() / 2;
    double tx = (x - (getX() + rx)) / rx;
    double ty = (y - (getY() + ry)) / ry;
    return tx * tx + ty * ty < 1.0;
  }

  /**
   * Determines if a rectangle is completely contained within the
   * ellipse. <P>
   * @param x - x coordinate of the upper-left corner of the rectangle
   * @param y - y coordinate of the upper-left corner of the rectangle
   * @param w - width of the rectangle
   * @param h - height of the rectangle
   * @return true if the rectangle is completely contained, false otherwise.
   */
  public boolean contains(double x, double y, double w, double h)
  {
    double x2 = x + w;
    double y2 = y + h;
    return (contains(x, y) && contains(x, y2) && contains(x2, y)
           && contains(x2, y2));
  }

  /**
   * Returns a PathIterator object corresponding to the ellipse.<P>
   *
   * Note: An ellipse cannot be represented exactly in PathIterator
   * segments, the outline is thefore approximated with cubic
   * Bezier segments.
   *
   * @param at an optional transform.
   * @return A path iterator.
   */
  public PathIterator getPathIterator(AffineTransform at)
  {
    // An ellipse is just a complete arc.
    return new Arc2D.ArcIterator(this, at);
  }

  /**
   * Determines if a rectangle intersects any part of the ellipse.<P>
   * @param x - x coordinate of the upper-left corner of the rectangle
   * @param y - y coordinate of the upper-left corner of the rectangle
   * @param w - width of the rectangle
   * @param h - height of the rectangle
   * @return true if the rectangle intersects the ellipse, false otherwise.
   */
  public boolean intersects(double x, double y, double w, double h)
  {
    Rectangle2D r = new Rectangle2D.Double(x, y, w, h);
    if (! r.intersects(getX(), getY(), getWidth(), getHeight()))
      return false;

    if (contains(x, y) || contains(x, y + h) || contains(x + w, y)
        || contains(x + w, y + h))
      return true;

    Line2D l1 = new Line2D.Double(getX(), getY() + (getHeight() / 2),
                                  getX() + getWidth(),
                                  getY() + (getHeight() / 2));
    Line2D l2 = new Line2D.Double(getX() + (getWidth() / 2), getY(),
                                  getX() + (getWidth() / 2),
                                  getY() + getHeight());

    if (l1.intersects(r) || l2.intersects(r))
      return true;

    return false;
  }

  /**
   * An {@link Ellipse2D} that stores its coordinates using <code>double</code>
   * primitives.
   */
  public static class Double extends Ellipse2D
  {
    /**
     * The height of the ellipse.
     */
    public double height;

    /**
     * The width of the ellipse.
     */
    public double width;

    /**
     * The upper-left x coordinate of the bounding-box
     */
    public double x;

    /**
     * The upper-left y coordinate of the bounding-box
     */
    public double y;

    /**
     * Creates a new Ellipse2D with an upper-left coordinate of (0,0)
     * and a zero size.
     */
    public Double()
    {
    }

    /**
     * Creates a new Ellipse2D within a given rectangle
     * using double-precision coordinates.<P>
     * @param x - x coordinate of the upper-left of the bounding rectangle
     * @param y - y coordinate of the upper-left of the bounding rectangle
     * @param w - width of the ellipse
     * @param h - height of the ellipse
     */
    public Double(double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      height = h;
      width = w;
    }

    /**
     * Returns the bounding-box of the ellipse.
     * @return The bounding box.
     */
    public Rectangle2D getBounds2D()
    {
      return new Rectangle2D.Double(x, y, width, height);
    }

    /**
     * Returns the height of the ellipse.
     * @return The height of the ellipse.
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Returns the width of the ellipse.
     * @return The width of the ellipse.
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Returns x coordinate of the upper-left corner of
     * the ellipse's bounding-box.
     * @return The x coordinate.
     */
    public double getX()
    {
      return x;
    }

    /**
     * Returns y coordinate of the upper-left corner of
     * the ellipse's bounding-box.
     * @return The y coordinate.
     */
    public double getY()
    {
      return y;
    }

    /**
     * Returns <code>true</code> if the ellipse encloses no area, and
     * <code>false</code> otherwise.
     *
     * @return A boolean.
     */
    public boolean isEmpty()
    {
      return height <= 0 || width <= 0;
    }

    /**
     * Sets the geometry of the ellipse's bounding box.<P>
     *
     * @param x - x coordinate of the upper-left of the bounding rectangle
     * @param y - y coordinate of the upper-left of the bounding rectangle
     * @param w - width of the ellipse
     * @param h - height of the ellipse
     */
    public void setFrame(double x, double y, double w, double h)
    {
      this.x = x;
      this.y = y;
      height = h;
      width = w;
    }
  } // class Double

  /**
   * An {@link Ellipse2D} that stores its coordinates using <code>float</code>
   * primitives.
   */
  public static class Float extends Ellipse2D
  {
    /**
     * The height of the ellipse.
     */
    public float height;

    /**
     * The width of the ellipse.
     */
    public float width;

    /**
     * The upper-left x coordinate of the bounding-box
     */
    public float x;

    /**
     * The upper-left y coordinate of the bounding-box
     */
    public float y;

    /**
     * Creates a new Ellipse2D with an upper-left coordinate of (0,0)
     * and a zero size.
     */
    public Float()
    {
    }

    /**
     * Creates a new Ellipse2D within a given rectangle
     * using floating-point precision.<P>
     * @param x - x coordinate of the upper-left of the bounding rectangle
     * @param y - y coordinate of the upper-left of the bounding rectangle
     * @param w - width of the ellipse
     * @param h - height of the ellipse
     *
     */
    public Float(float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      this.height = h;
      this.width = w;
    }

    /**
     * Returns the bounding-box of the ellipse.
     * @return The bounding box.
     */
    public Rectangle2D getBounds2D()
    {
      return new Rectangle2D.Float(x, y, width, height);
    }

    /**
     * Returns the height of the ellipse.
     * @return The height of the ellipse.
     */
    public double getHeight()
    {
      return height;
    }

    /**
     * Returns the width of the ellipse.
     * @return The width of the ellipse.
     */
    public double getWidth()
    {
      return width;
    }

    /**
     * Returns x coordinate of the upper-left corner of
     * the ellipse's bounding-box.
     * @return The x coordinate.
     */
    public double getX()
    {
      return x;
    }

    /**
     * Returns y coordinate of the upper-left corner of
     * the ellipse's bounding-box.
     * @return The y coordinate.
     */
    public double getY()
    {
      return y;
    }

    /**
     * Returns <code>true</code> if the ellipse encloses no area, and
     * <code>false</code> otherwise.
     *
     * @return A boolean.
     */
    public boolean isEmpty()
    {
      return height <= 0 || width <= 0;
    }

    /**
     * Sets the geometry of the ellipse's bounding box.<P>
     *
     * @param x - x coordinate of the upper-left of the bounding rectangle
     * @param y - y coordinate of the upper-left of the bounding rectangle
     * @param w - width of the ellipse
     * @param h - height of the ellipse
     */
    public void setFrame(float x, float y, float w, float h)
    {
      this.x = x;
      this.y = y;
      height = h;
      width = w;
    }

    /**
     * Sets the geometry of the ellipse's bounding box.
     *
     * Note: This leads to a loss of precision.<P>
     *
     * @param x - x coordinate of the upper-left of the bounding rectangle
     * @param y - y coordinate of the upper-left of the bounding rectangle
     * @param w - width of the ellipse
     * @param h - height of the ellipse
     */
    public void setFrame(double x, double y, double w, double h)
    {
      this.x = (float) x;
      this.y = (float) y;
      height = (float) h;
      width = (float) w;
    }
  } // class Float
} // class Ellipse2D
