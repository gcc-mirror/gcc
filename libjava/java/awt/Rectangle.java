/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

   This file is part of libjava.

This software is copyrighted work licensed under the terms of the
Libjava License.  Please consult the file "LIBJAVA_LICENSE" for
details.  */

package java.awt;

import java.awt.geom.*;
import java.io.Serializable;

/* Status:  Mostly complete. Some of the Java2D stuff is commented out. */

public class Rectangle extends Rectangle2D
  implements Cloneable, Shape, Serializable
{
  public int x;
  public int y;
  public int width;
  public int height;

  public Rectangle()
  {
    x = 0;
    y = 0;
    width = 0;
    height = 0;
  }

  public Rectangle(Rectangle r)
  {
    x = r.x;
    y = r.y;
    width = r.width;
    height = r.height;
  }

  public Rectangle(int x, int y, int width, int height)
  {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  public Rectangle(int width, int height)
  {
    x = 0;
    y = 0;
    this.width = width;
    this.height = height;
  }

  public Rectangle(Point p, Dimension d)
  {
    x = p.x;
    y = p.y;
    width = d.width;
    height = d.height;
  }

  public Rectangle(Point p)
  {
    x = p.x;
    y = p.y;
    width = 0;
    height = 0;
  }

  public Rectangle(Dimension d)
  {
    x = 0;
    y = 0;
    width = d.width;
    height = d.height;
  }

  public Rectangle getBounds ()
  {
    return (Rectangle) this.clone();
  }

  public void add(int newx, int newy)
  {
    int x = this.x > newx ? newx : this.x;
    int y = this.y > newy ? newy : this.y;
    width = (this.x + width > newx ? this.x + width : newx) - x;
    height = (this.y + height > newy ? this.y + height : newy) - y;
    this.x = x;
    this.y = y;
  }

  public void add(Point pt)
  {
    add (pt.x, pt.y);
  }

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

  public boolean contains(Point p)
  {
    return contains(p.x, p.y);
  }

  public boolean contains(Rectangle r)
  {
    return contains(r.x, r.y, r.width, r.height);
  }

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

  public Point getLocation()
  {
    return new Point(x,y);
  }

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

  public void grow(int h, int v)
  {
    width += h;
    height += v;
  }

  /** @deprecated Use contains() instead. */
  public boolean inside(int x, int y)
  {
    return contains(x, y);
  }

  /** @specnote If there is no intersection, an empty rectangle at 0,0 
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

  /** @specnote If the intersection is at an edge or corner only (an empty
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

  public boolean isEmpty()
  {
    return !(width > 0 && height > 0);
  }

  /** @deprecated Use setLocation() instead. */
  public void move(int x, int y)
  {
    setLocation(x, y);
  }

  public int outcode(double x, double y)
  {
    // FIXME
    return 0;
  }

  /** @deprecated Use setBounds() instead. */
  public void reshape(int x, int y, int width, int height)
  {
    setBounds(x, y, width, height);
  }

  /** @deprecated Use setSize() instead. */
  public void resize(int width, int height)
  {
    setSize(width, height);
  }

  public void setBounds(int x, int y, int width, int height)
  {
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
  }

  public void setBounds(Rectangle r)
  {
    this.x = r.x;
    this.y = r.y;
    this.width = r.width;
    this.height = r.height;     
  }

  public void setLocation(int x, int y)
  {
    this.x = x;
    this.y = y;
  }

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

  public void setSize(Dimension d)
  {
    this.width = d.width;
    this.height = d.height;
  }

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

  public String toString()
  {
    return getClass().getName() + "[x=" + x + ",y=" + y + ",width=" + width + 
           ",height=" + height + "]";
  }
}
