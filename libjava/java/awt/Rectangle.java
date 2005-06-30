/* Rectangle.java -- represents a graphics rectangle
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation

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


package java.awt;

import java.awt.geom.Rectangle2D;
import java.io.Serializable;

/**
 * This class represents a rectangle and all the interesting things you
 * might want to do with it.  Note that the coordinate system uses
 * the origin (0,0) as the top left of the screen, with the x and y
 * values increasing as they move to the right and down respectively.
 *
 * <p>It is valid for a rectangle to have negative width or height; but it
 * is considered to have no area or internal points. Therefore, the behavior
 * in methods like <code>contains</code> or <code>intersects</code> is
 * undefined unless the rectangle has positive width and height.
 *
 * <p>There are some public fields; if you mess with them in an inconsistent
 * manner, it is your own fault when you get NullPointerException,
 * ArrayIndexOutOfBoundsException, or invalid results. Also, this class is
 * not threadsafe.
 *
 * @author Warren Levy  (warrenl@cygnus.com)
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.0
 * @status updated to 1.4
 */
public class Rectangle extends Rectangle2D implements Shape, Serializable
{
  /**
   * Compatible with JDK 1.0+.
   */
  private static final long serialVersionUID = -4345857070255674764L;

  /**
   * The X coordinate of the top-left corner of the rectangle.
   *
   * @see #setLocation(int, int)
   * @see #getLocation()
   * @serial the x coordinate
   */
  public int x;

  /**
   * The Y coordinate of the top-left corner of the rectangle.
   *
   * @see #setLocation(int, int)
   * @see #getLocation()
   * @serial the y coordinate
   */
  public int y;

  /**
   * The width of the rectangle.
   *
   * @see #setSize(int, int)
   * @see #getSize()
   * @serial
   */
  public int width;

  /**
   * The height of the rectangle.
   *
   * @see #setSize(int, int)
   * @see #getSize()
   * @serial
   */
  public int height;

  /**
   * Initializes a new instance of <code>Rectangle</code> with a top
   * left corner at (0,0) and a width and height of 0.
   */
  public Rectangle()
  {
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> from the
   * coordinates of the specified rectangle.
   *
   * @param r the rectangle to copy from
   * @throws NullPointerException if r is null
   * @since 1.1
   */
  public Rectangle(Rectangle r)
  {
    x = r.x;
    y = r.y;
    width = r.width;
    height = r.height;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> from the specified
   * inputs.
   *
   * @param x the X coordinate of the top left corner
   * @param y the Y coordinate of the top left corner
   * @param width the width of the rectangle
   * @param height the height of the rectangle
   */
  public Rectangle(int x, int y, int width, int height)
  {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> with the specified
   * width and height. The upper left corner of the rectangle will be at
   * the origin (0,0).
   *
   * @param width the width of the rectangle
   * @param height the height of the rectange
   */
  public Rectangle(int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> with a top-left
   * corner represented by the specified point and the width and height
   * represented by the specified dimension.
   *
   * @param p the upper left corner of the rectangle
   * @param d the width and height of the rectangle
   * @throws NullPointerException if p or d is null
   */
  public Rectangle(Point p, Dimension d)
  {
    x = p.x;
    y = p.y;
    width = d.width;
    height = d.height;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> with a top left
   * corner at the specified point and a width and height of zero.
   *
   * @param p the upper left corner of the rectangle
   */
  public Rectangle(Point p)
  {
    x = p.x;
    y = p.y;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> with an
   * upper left corner at the origin (0,0) and a width and height represented
   * by the specified dimension.
   *
   * @param d the width and height of the rectangle
   */
  public Rectangle(Dimension d)
  {
    width = d.width;
    height = d.height;
  }

  /**
   * Get the X coordinate of the upper-left corner.
   *
   * @return the value of x, as a double
   */
  public double getX()
  {
    return x;
  }

  /**
   * Get the Y coordinate of the upper-left corner.
   *
   * @return the value of y, as a double
   */
  public double getY()
  {
    return y;
  }

  /**
   * Get the width of the rectangle.
   *
   * @return the value of width, as a double
   */
  public double getWidth()
  {
    return width;
  }

  /**
   * Get the height of the rectangle.
   *
   * @return the value of height, as a double
   */
  public double getHeight()
  {
    return height;
  }

  /**
   * Returns the bounds of this rectangle. A pretty useless method, as this
   * is already a rectangle; it is included to mimic the
   * <code>getBounds</code> method in Component.
   *
   * @return a copy of this rectangle
   * @see #setBounds(Rectangle)
   * @since 1.1
   */
  public Rectangle getBounds()
  {
    return new Rectangle(this);
  }

  /**
   * Returns the high-precision bounds of this rectangle. A pretty useless
   * method, as this is already a rectangle.
   *
   * @return a copy of this rectangle
   * @see #setBounds(Rectangle)
   * @since 1.2
   */
  public Rectangle2D getBounds2D()
  {
    return new Rectangle(x, y, width, height);
  }

  /**
   * Updates this rectangle to match the dimensions of the specified
   * rectangle.
   *
   * @param r the rectangle to update from
   * @throws NullPointerException if r is null
   * @see #setBounds(int, int, int, int)
   * @since 1.1
   */
  public void setBounds(Rectangle r)
  {
    setBounds (r.x, r.y, r.width, r.height);
  }

  /**
   * Updates this rectangle to have the specified dimensions.
   *
   * @param x the new X coordinate of the upper left hand corner
   * @param y the new Y coordinate of the upper left hand corner
   * @param width the new width of this rectangle
   * @param height the new height of this rectangle
   * @since 1.1
   */
  public void setBounds(int x, int y, int width, int height)
  {
    reshape (x, y, width, height);
  }

  /**
   * Updates this rectangle to have the specified dimensions, as rounded to
   * integers.
   *
   * @param x the new X coordinate of the upper left hand corner
   * @param y the new Y coordinate of the upper left hand corner
   * @param width the new width of this rectangle
   * @param height the new height of this rectangle
   * @since 1.2
   */
  public void setRect(double x, double y, double width, double height)
  {
    this.x = (int) x;
    this.y = (int) y;
    this.width = (int) width;
    this.height = (int) height;
  }

  /**
   * Updates this rectangle to have the specified dimensions.
   *
   * @param x the new X coordinate of the upper left hand corner
   * @param y the new Y coordinate of the upper left hand corner
   * @param width the new width of this rectangle
   * @param height the new height of this rectangle
   * @deprecated use {@link #setBounds(int, int, int, int)} instead
   */
  public void reshape(int x, int y, int width, int height)
  {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  /**
   * Returns the location of this rectangle, which is the coordinates of
   * its upper left corner.
   *
   * @return the point where this rectangle is located
   * @see #setLocation(Point)
   * @since 1.1
   */
  public Point getLocation()
  {
    return new Point(x,y);
  }

  /**
   * Moves the location of this rectangle by setting its upper left
   * corner to the specified point.
   *
   * @param p the point to move the rectangle to
   * @throws NullPointerException if p is null
   * @see #getLocation()
   * @since 1.1
   */
  public void setLocation(Point p)
  {
    setLocation (p.x, p.y);
  }

  /**
   * Moves the location of this rectangle by setting its upper left
   * corner to the specified coordinates.
   *
   * @param x the new X coordinate for this rectangle
   * @param y the new Y coordinate for this rectangle
   * @since 1.1
   */
  public void setLocation(int x, int y)
  {
    move (x, y);
  }

  /**
   * Moves the location of this rectangle by setting its upper left
   * corner to the specified coordinates.
   *
   * @param x the new X coordinate for this rectangle
   * @param y the new Y coordinate for this rectangle
   * @deprecated use {@link #setLocation(int, int)} instead
   */
  public void move(int x, int y)
  {
    this.x = x;
    this.y = y;
  }

  /**
   * Translate the location of this rectangle by the given amounts.
   *
   * @param dx the x distance to move by
   * @param dy the y distance to move by
   * @see #setLocation(int, int)
   */
  public void translate(int dx, int dy)
  {
    x += dx;
    y += dy;
  }

  /**
   * Returns the size of this rectangle.
   *
   * @return the size of this rectangle
   * @see #setSize(Dimension)
   * @since 1.1
   */
  public Dimension getSize()
  {
    return new Dimension(width, height);
  }

  /**
   * Sets the size of this rectangle based on the specified dimensions.
   *
   * @param d the new dimensions of the rectangle
   * @throws NullPointerException if d is null
   * @see #getSize()
   * @since 1.1
   */
  public void setSize(Dimension d)
  {
    setSize (d.width, d.height);
  }

  /**
   * Sets the size of this rectangle based on the specified dimensions.
   *
   * @param width the new width of the rectangle
   * @param height the new height of the rectangle
   * @since 1.1
   */
  public void setSize(int width, int height)
  {
    resize (width, height);
  }

  /**
   * Sets the size of this rectangle based on the specified dimensions.
   *
   * @param width the new width of the rectangle
   * @param height the new height of the rectangle
   * @deprecated use {@link #setSize(int, int)} instead
   */
  public void resize(int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  /**
   * Tests whether or not the specified point is inside this rectangle.
   * According to the contract of Shape, a point on the border is in only if
   * it has an adjacent point inside the rectangle in either the increasing
   * x or y direction.
   *
   * @param p the point to test
   * @return true if the point is inside the rectangle
   * @throws NullPointerException if p is null
   * @see #contains(int, int)
   * @since 1.1
   */
  public boolean contains(Point p)
  {
    return contains (p.x, p.y);
  }

  /**
   * Tests whether or not the specified point is inside this rectangle.
   * According to the contract of Shape, a point on the border is in only if
   * it has an adjacent point inside the rectangle in either the increasing
   * x or y direction.
   *
   * @param x the X coordinate of the point to test
   * @param y the Y coordinate of the point to test
   * @return true if the point is inside the rectangle
   * @since 1.1
   */
  public boolean contains(int x, int y)
  {
    return inside (x, y);
  }

  /**
   * Checks whether all points in the given rectangle are contained in this
   * rectangle.
   *
   * @param r the rectangle to check
   * @return true if r is contained in this rectangle
   * @throws NullPointerException if r is null
   * @see #contains(int, int, int, int)
   * @since 1.1
   */
  public boolean contains(Rectangle r)
  {
    return contains (r.x, r.y, r.width, r.height);
  }

  /**
   * Checks whether all points in the given rectangle are contained in this
   * rectangle.
   *
   * @param x the x coordinate of the rectangle to check
   * @param y the y coordinate of the rectangle to check
   * @param w the width of the rectangle to check
   * @param h the height of the rectangle to check
   * @return true if the parameters are contained in this rectangle
   * @since 1.1
   */
  public boolean contains(int x, int y, int w, int h)
  {
    return width > 0 && height > 0 && w > 0 && h > 0
      && x >= this.x && x + w <= this.x + this.width
      && y >= this.y && y + h <= this.y + this.height;
  }

  /**
   * Tests whether or not the specified point is inside this rectangle.
   *
   * @param x the X coordinate of the point to test
   * @param y the Y coordinate of the point to test
   * @return true if the point is inside the rectangle
   * @deprecated use {@link #contains(int, int)} instead
   */
  public boolean inside(int x, int y)
  {
    return width > 0 && height > 0
      && x >= this.x && x < this.x + width
      && y >= this.y && y < this.y + height;
  }

  /**
   * Tests whether or not the specified rectangle intersects this rectangle.
   * This means the two rectangles share at least one internal point.
   *
   * @param r the rectangle to test against
   * @return true if the specified rectangle intersects this one
   * @throws NullPointerException if r is null
   * @since 1.2
   */
  public boolean intersects(Rectangle r)
  {
    return r.width > 0 && r.height > 0 && width > 0 && height > 0
      && r.x < x + width && r.x + r.width > x
      && r.y < y + height && r.y + r.height > y;
  }

  /**
   * Determines the rectangle which is formed by the intersection of this
   * rectangle with the specified rectangle. If the two do not intersect,
   * an empty rectangle will be returned (meaning the width and/or height
   * will be non-positive).
   *
   * @param r the rectange to calculate the intersection with
   * @return a new rectangle bounding the intersection
   * @throws NullPointerException if r is null
   */
  public Rectangle intersection(Rectangle r)
  {
    Rectangle res = new Rectangle();
    intersect(this, r, res);
    return res;
  }

  /**
   * Returns the smallest rectangle that contains both this rectangle
   * and the specified rectangle.
   *
   * @param r the rectangle to compute the union with
   * @return the smallest rectangle containing both rectangles
   * @throws NullPointerException if r is null
   */
  public Rectangle union(Rectangle r)
  {
    Rectangle res = new Rectangle();
    union(this, r, res);
    return res;
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
  public void add(int x, int y)
  {
    add((double) x, (double) y);
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
  public void add(Point p)
  {
    add((double) p.x, (double) p.y);
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle
   * that contains both the existing rectangle and the specified rectangle.
   *
   * @param r the rectangle to add to this rectangle
   * @throws NullPointerException if r is null
   * @see #union(Rectangle)
   */
  public void add(Rectangle r)
  {
    union(this, r, this);
  }

  /**
   * Expands the rectangle by the specified amount.  The horizontal
   * and vertical expansion values are applied both to the X,Y coordinate
   * of this rectangle, and its width and height.  Thus the width and
   * height will increase by 2h and 2v accordingly.
   *
   * @param h the horizontal expansion value
   * @param v the vertical expansion value
   */
  public void grow(int h, int v)
  {
    x -= h;
    y -= v;
    width += h + h;
    height += v + v;
  }

  /**
   * Tests whether or not this rectangle is empty.  An empty rectangle
   * has a non-positive width or height.
   *
   * @return true if the rectangle is empty
   */
  public boolean isEmpty()
  {
    return width <= 0 || height <= 0;
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
   * Determines the rectangle which is formed by the intersection of this
   * rectangle with the specified rectangle. If the two do not intersect,
   * an empty rectangle will be returned (meaning the width and/or height
   * will be non-positive).
   *
   * @param r the rectange to calculate the intersection with
   * @return a new rectangle bounding the intersection
   * @throws NullPointerException if r is null
   * @since 1.2
   */
  public Rectangle2D createIntersection(Rectangle2D r)
  {
    // Favor runtime type of other rectangle.
    Rectangle2D res = r.getBounds2D();
    intersect(this, r, res);
    return res;
  }

  /**
   * Returns the smallest rectangle that contains both this rectangle
   * and the specified rectangle.
   *
   * @param r the rectangle to compute the union with
   * @return the smallest rectangle containing both rectangles
   * @throws NullPointerException if r is null
   * @since 1.2
   */
  public Rectangle2D createUnion(Rectangle2D r)
  {
    // Favor runtime type of other rectangle.
    Rectangle2D res = r.getBounds2D();
    union(this, r, res);
    return res;
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
    return r.getX() == x && r.getY() == y
      && r.getWidth() == width && r.getHeight() == height;
  }

  /**
   * Returns a string representation of this rectangle. This is in the form
   * <code>getClass().getName() + "[x=" + x + ",y=" + y + ",width=" + width
   * + ",height=" + height + ']'</code>.
   *
   * @return a string representation of this rectangle
   */
  public String toString()
  {
    return getClass().getName() + "[x=" + x + ",y=" + y + ",width=" + width
      + ",height=" + height + ']';
  }
} // class Rectangle
