/* Copyright (C) 1999, 2000, 2001, 2002  Free Software Foundation

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


package java.awt;

import java.awt.geom.*;
import java.io.Serializable;

/* Status:  Mostly complete. Some of the Java2D stuff is commented out. */

/**
 * This class represents a rectangle and all the interesting things you
 * might want to do with it.  Note that the coordinate system uses
 * the origin (0,0) as the top left of the screen, with the x and y
 * values increasing as they move to the right and down respectively.
 *
 * @author Warren Levy  <warrenl@cygnus.com>
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class Rectangle extends Rectangle2D
  implements Cloneable, Shape, Serializable
{
  /**
  * The X coordinate of the top-left corner of the rectangle.
  */
  public int x;

  /**
  * The Y coordinate of the top-left corner of the rectangle;
  */
  public int y;

  /**
  * The width of the rectangle
  */
  public int width;

  /**
  * The height of the rectangle
  */
  public int height;

  /**
   * Initializes a new instance of <code>Rectangle</code> with a top
   * left corner at (0,0) and a width and height of 0.
   */
  public Rectangle()
  {
    x = 0;
    y = 0;
    width = 0;
    height = 0;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> from the
   * coordinates of the specified rectangle.
   *
   * @param rect The rectangle to copy from.
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
   * @param x The X coordinate of the top left corner of the rectangle.
   * @param y The Y coordinate of the top left corner of the rectangle.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
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
   * width and height.  The upper left corner of the rectangle will be at
   * the origin (0,0).
   *
   * @param width The width of the rectangle.
   * @param height the height of the rectange.
   */
  public Rectangle(int width, int height)
  {
    x = 0;
    y = 0;
    this.width = width;
    this.height = height;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> with a top-left
   * corner represented by the specified point and the width and height
   * represented by the specified dimension.
   *
   * @param point The upper left corner of the rectangle.
   * @param dim The width and height of the rectangle.
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
   * @param poin The upper left corner of the rectangle.
   */
  public Rectangle(Point p)
  {
    x = p.x;
    y = p.y;
    width = 0;
    height = 0;
  }

  /**
   * Initializes a new instance of <code>Rectangle</code> with an
   * upper left corner at the origin (0,0) and a width and height represented
   * by the specified dimension.
   *
   * @param dim The width and height of the rectangle.
   */
  public Rectangle(Dimension d)
  {
    x = 0;
    y = 0;
    width = d.width;
    height = d.height;
  }

  /**
   * Returns the bounding rectangle for this rectangle, which is simply
   * this rectange itself.
   *
   * @return This rectangle.
   */
  public Rectangle getBounds ()
  {
    return (Rectangle) this.clone();
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle 
   * that contains both the existing rectangle and the specified point.
   *
   * @param x The X coordinate of the point to add to this rectangle.
   * @param y The Y coordinate of the point to add to this rectangle.
   */
  public void add(int newx, int newy)
  {
    int x = this.x > newx ? newx : this.x;
    int y = this.y > newy ? newy : this.y;
    width = (this.x + width > newx ? this.x + width : newx) - x;
    height = (this.y + height > newy ? this.y + height : newy) - y;
    this.x = x;
    this.y = y;
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle 
   * that contains both the existing rectangle and the specified point.
   *
   * @param point The point to add to this rectangle.
   */
  public void add(Point pt)
  {
    add (pt.x, pt.y);
  }

  /**
   * Modifies this rectangle so that it represents the smallest rectangle 
   * that contains both the existing rectangle and the specified rectangle.
   *
   * @param rect The rectangle to add to this rectangle.
   */
  public void add(Rectangle r)
  {
    int x = this.x > r.x ? r.x : this.x;
    int y = this.y > r.y ? r.y : this.y;
    width = (this.x + width > r.x + r.width ? 
             this.x + width : r.x + r.width) - x;
    height = (this.y + height > r.y + r.height ?
              this.y + height : r.y + r.height) - y;
    this.x = x;
    this.y = y;
  }

  /**
   * Tests whether or not the specified point is inside this rectangle.
   *
   * @param x The X coordinate of the point to test.
   * @param y The Y coordinate of the point to test.
   *
   * @return <code>true</code> if the point is inside the rectangle,
   * <code>false</code> otherwise.
   */
  public boolean contains(int x, int y)
  {
    return (x >= this.x && x <= this.x + this.width
            && y >= this.y && y <= this.y + this.height);
  }   

  public boolean contains(int x, int y, int w, int h)
  {
    return (x >= this.x && x + w <= this.x + this.width
            && y >= this.y && y + h <= this.y + this.height);
  }

  /**
   * Tests whether or not the specified point is inside this rectangle.
   *
   * @param point The point to test.
   *
   * @return <code>true</code> if the point is inside the rectangle,
   * <code>false</code> otherwise.
   */
  public boolean contains(Point p)
  {
    return contains(p.x, p.y);
  }

  public boolean contains(Rectangle r)
  {
    return contains(r.x, r.y, r.width, r.height);
  }

  /**
   * Tests this rectangle for equality against the specified object.  This
   * will be true if an only if the specified object:
   * <p>
   * <ul>
   * <li>Is not <code>null</code>.
   * <li>Is an instance of <code>Rectangle</code>.
   * <li>Has X and Y coordinates identical to this rectangle.
   * <li>Has a width and height identical to this rectangle.
   * </ul>
   *
   * @param obj The object to test against for equality.
   *
   * @return <code>true</code> if the specified object is equal to this one,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (obj instanceof Rectangle)
      {
	Rectangle r = (Rectangle) obj;
	return (r.x == x 
	        && r.y == y 
		&& r.width == width 
		&& r.height == height);
      }
    return false;
  }

  public double getHeight()
  {
    return (double) this.height;     
  }

  /**
   * Returns the location of this rectangle, which is the coordinates of
   * its upper left corner.
   *
   * @return The point where this rectangle is located.
   */
  public Point getLocation()
  {
    return new Point(x,y);
  }

  /**
   * Returns the size of this rectangle.
   *
   * @return The size of this rectangle.
   */
  public Dimension getSize()
  {
    return new Dimension(width, height);
  }

  public double getWidth()
  {
    return (double) this.width;
  }

  public double getX()
  {
    return (double) x;
  }

  public double getY()
  {
    return (double) y;
  }

  /**
   * Expands the rectangle by the specified amount.  The horizontal
   * and vertical expansion values are applied both to the X,Y coordinate
   * of this rectangle, and its width and height.  Thus the width and
   * height will increase by 2h and 2v accordingly.
   *
   * @param h The horizontal expansion value.
   * @param v The vertical expansion value.
   */
  public void grow(int h, int v)
  {
    width += h;
    height += v;
  }

  /**
   * Tests whether or not the specified point is inside this rectangle.
   *
   * @param x The X coordinate of the point to test.
   * @param y The Y coordinate of the point to test.
   *
   * @return <code>true</code> if the point is inside the rectangle,
   * <code>false</code> otherwise.
   *
   * @deprecated This method is deprecated in favor of
   * <code>contains(int, int)</code>.
   */
  public boolean inside(int x, int y)
  {
    return contains(x, y);
  }

  /**
   * Determines the rectange which is formed by the intersection of this
   * rectangle with the specified rectangle.
   *
   * @param rect The rectange to calculate the intersection with.
   *
   * @return The rectangle bounding the intersection.
   *
   * @specnote If there is no intersection, an empty rectangle at 0,0 
   *           is returned.
   */
  public Rectangle intersection(Rectangle r)
  {
    int newx = x < r.x ? r.x : x;
    int newy = y < r.y ? r.y : y;
    int neww = (x + width < r.x + r.width ?
        	x + width : r.x + r.width) - newx;
    int newh = (y + height < r.y + r.height ?
        	y + height : r.y + r.height) - newy;
    if (neww >= 0 && newh >= 0)
      return new Rectangle(newx, newy, neww, newh);
    else
      return new Rectangle(0, 0, 0, 0);
  }

  /**
   * Tests whether or not the specified rectangle intersects this rectangle.
   *
   * @param rect The rectangle to test against.
   *
   * @return <code>true</code> if the specified rectangle intersects this
   * one, <code>false</code> otherwise.
   *
   * @specnote If the intersection is at an edge or corner only (an empty
   *           intersection with a non-zero location), false is returned.
   */
  public boolean intersects(Rectangle r)
  {
    int neww = (x + width < r.x + r.width ?
        	x + width : r.x + r.width) - (x < r.x ? r.x : x);
    int newh = (y + height < r.y + r.height ?
        	y + height : r.y + r.height) - (y < r.y ? r.y : y);
    return (neww > 0 && newh > 0);
  }

  /**
   * Tests whether or not this rectangle is empty.  An empty rectangle
   * has a width or height of zero.
   *
   * @return <code>true</code> if the rectangle is empty, <code>false</code>
   * otherwise.
   */
  public boolean isEmpty()
  {
    return !(width > 0 && height > 0);
  }

  /**
   * Moves the location of this rectangle by setting its upper left
   * corner to the specified coordinates.
   * // FIXME: Is this true?
   *
   * @param x The new X coordinate for this rectangle.
   * @param y The new Y coordinate for this rectangle.
   *
   * @deprecated This method is deprecated in favor of
   * <code>setLocation(int, int)</code>.
   */
  public void move(int x, int y)
  {
    setLocation(x, y);
  }

  public int outcode(double x, double y)
  {
    // FIXME
    return 0;
  }

  /**
   * Updates this rectangle to have the specified dimensions.
   *
   * @param x The new X coordinate of the upper left hand corner.
   * @param y The new Y coordinate of the upper left hand corner.
   * @param width The new width of this rectangle.
   * @param height The new height of this rectangle.
   *
   * @deprecated This method is deprecated in favor of 
   * <code>setBounds(int, int, int, int)</code>.
   */
  public void reshape(int x, int y, int width, int height)
  {
    setBounds(x, y, width, height);
  }

  /**
   * Sets the size of this rectangle based on the specified dimensions.
   *
   * @param width The new width of the rectangle.
   * @param height The new height of the rectangle.
   *
   * @deprecated This method is deprecated in favor of
   * <code>setSize(int, int)</code>.
   */
  public void resize(int width, int height)
  {
    setSize(width, height);
  }

  /**
   * Updates this rectangle to have the specified dimensions.
   *
   * @param x The new X coordinate of the upper left hand corner.
   * @param y The new Y coordinate of the upper left hand corner.
   * @param width The new width of this rectangle.
   * @param height The new height of this rectangle.
   */
  public void setBounds(int x, int y, int width, int height)
  {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  /**
   * Updates this rectangle to match the dimensions of the specified 
   * rectangle.
   *
   * @param rect The rectangle to update from.
   */
  public void setBounds(Rectangle r)
  {
    this.x = r.x;
    this.y = r.y;
    this.width = r.width;
    this.height = r.height;     
  }

  /**
   * Moves the location of this rectangle by setting its upper left
   * corner to the specified coordinates.
   * // FIXME: Is this true?
   *
   * @param x The new X coordinate for this rectangle.
   * @param y The new Y coordinate for this rectangle.
   */
  public void setLocation(int x, int y)
  {
    this.x = x;
    this.y = y;
  }

  /**
   * Moves the location of this rectangle by setting its upper left
   * corner to the specified point.
   * // FIXME: Is this true?
   *
   * @param point The point to move the rectange to.
   */
  public void setLocation(Point p)
  {
    this.x = p.x;
    this.y = p.y;
  }

  public void setRect(double x, double y, double width, double height)
  {
    this.x = (int) x;
    this.y = (int) y;
    this.width = (int) width;
    this.height = (int) height;
  }

  /**
   * Sets the size of this rectangle based on the specified dimensions.
   *
   * @param dim The new dimensions of the rectangle.
   */
  public void setSize(Dimension d)
  {
    this.width = d.width;
    this.height = d.height;
  }

  /**
   * Sets the size of this rectangle based on the specified dimensions.
   *
   * @param width The new width of the rectangle.
   * @param height The new height of the rectangle.
   */
  public void setSize(int width, int height)
  {
    this.width = width;
    this.height = height;
  }

  public void translate(int x, int y)
  {
    x += x;
    y += y;
  }

  /**
   * Returns the smallest rectangle that contains both this rectangle
   * and the specified rectangle.
   *
   * @param rect The rectangle to compute the union with.
   *
   * @return The smallest rectangle containing both rectangles.
   */
  public Rectangle union(Rectangle r)
  {
    int newx = x > r.x ? r.x : x;
    int newy = y > r.y ? r.y : y;     
    int neww = (this.x + width > r.x + r.width ? 
               this.x + width : r.x + r.width) - newx;
    int newh = (this.y + height > r.y + r.height ?
        	this.y + height : r.y + r.height) - newy;
    return new Rectangle(newx, newy, neww, newh);
  }

  // Commented out until we have Rectangle2D
  public Rectangle2D createIntersection(Rectangle2D r)
  {
    // FIXME: maybe we should consider returning a Rectangle or
    // Rectangle2D.Float depending on type of R.
    Rectangle2D.Double res = new Rectangle2D.Double ();
    intersect (this, r, res);
    return res;
  }

  public Rectangle2D createUnion(Rectangle2D r)
  {
    // FIXME: maybe we should consider returning a Rectangle or
    // Rectangle2D.Float depending on type of R.
    Rectangle2D.Double res = new Rectangle2D.Double ();
    union (this, r, res);
    return res;
  }

  public Rectangle2D getBounds2D()
  {
    return new Rectangle (x, y, width, height);
  }

  /**
   * Returns a string representation of this rectangle.
   *
   * @return A string representation of this rectangle.
   */
  public String toString()
  {
    return getClass().getName() + "[x=" + x + ",y=" + y + ",width=" + width + 
           ",height=" + height + "]";
  }

  /**
   * Returns a hash value for this object.
   *
   * @return A hash value for this object.
   */
  public int hashCode()
  {
    return x * y * width * height * 37;
  }
}
