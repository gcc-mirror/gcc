/* Area.java -- represents a shape built by constructive area geometry
   Copyright (C) 2002 Free Software Foundation

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
 * STUBS ONLY
 * XXX Implement and document.
 */
public class Area implements Shape, Cloneable
{
  public Area()
  {
  }
  public Area(Shape s)
  {
  }
  public void add(Area a)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public void subtract(Area a)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public void intersect(Area a)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public void exclusiveOr(Area a)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public void reset()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean isEmpty()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean isPolygonal()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean isRectangular()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean isSingular()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public Rectangle2D getBounds2D()
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public Rectangle getBounds()
  {
    return getBounds2D().getBounds();
  }

  /**
   * Create a new area of the same run-time type with the same contents as
   * this one.
   *
   * @return the clone
   */
  public Object clone()
  {
    try
      {
        return super.clone();
      }
    catch (CloneNotSupportedException e)
      {
        throw (Error) new InternalError().initCause(e); // Impossible
      }
  }

  public boolean equals(Area a)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }

  public void transform(AffineTransform at)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public Area createTransformedArea(AffineTransform at)
  {
    Area a = (Area) clone();
    a.transform(at);
    return a;
  }
  public boolean contains(double x, double y)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean contains(Point2D p)
  {
    return contains(p.getX(), p.getY());
  }
  public boolean contains(double x, double y, double w, double h)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean contains(Rectangle2D r)
  {
    return contains(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }

  public boolean intersects(double x, double y, double w, double h)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean intersects(Rectangle2D r)
  {
    return intersects(r.getX(), r.getY(), r.getWidth(), r.getHeight());
  }
  public PathIterator getPathIterator(AffineTransform at)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public PathIterator getPathIterator(AffineTransform at, double flatness)
  {
    return new FlatteningPathIterator(getPathIterator(at), flatness);
  }
} // class Area
