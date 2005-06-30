/* Shape.java -- the classic Object-Oriented shape interface
   Copyright (C) 1999, 2002, 2005  Free Software Foundation, Inc.

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

import java.awt.geom.AffineTransform;
import java.awt.geom.PathIterator;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * This interface represents an abstract shape. The shape is described by
 * a {@link PathIterator}, and has callbacks for determining bounding box,
 * where points and rectangles lie in relation to the shape, and tracing
 * the trajectory.
 *
 * <p>A point is inside if it is completely inside, or on the boundary and
 * adjacent points in the increasing x or y direction are completely inside.
 * Unclosed shapes are considered as implicitly closed when performing
 * <code>contains</code> or <code>intersects</code>.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @see PathIterator
 * @see AffineTransform
 * @see java.awt.geom.FlatteningPathIterator
 * @see java.awt.geom.GeneralPath
 * @since 1.0
 * @status updated to 1.4
 */
public interface Shape
{
  /**
   * Returns a <code>Rectange</code> that bounds the shape. There is no
   * guarantee that this is the minimum bounding box, particularly if
   * the shape overflows the finite integer range of a bound. Generally,
   * <code>getBounds2D</code> returns a tighter bound.
   *
   * @return the shape's bounding box
   * @see #getBounds2D()
   */
  Rectangle getBounds();

  /**
   * Returns a high precision bounding box of the shape. There is no guarantee
   * that this is the minimum bounding box, but at least it never overflows.
   *
   * @return the shape's bounding box
   * @see #getBounds()
   * @since 1.2
   */
  Rectangle2D getBounds2D();

  /**
   * Test if the coordinates lie in the shape.
   *
   * @param x the x coordinate
   * @param y the y coordinate
   * @return true if (x,y) lies inside the shape
   * @since 1.2
   */
  boolean contains(double x, double y);

  /**
   * Test if the point lie in the shape.
   *
   * @param p the high-precision point
   * @return true if p lies inside the shape
   * @throws NullPointerException if p is null
   * @since 1.2
   */
  boolean contains(Point2D p);

  /**
   * Test if a high-precision rectangle intersects the shape. This is true
   * if any point in the rectangle is in the shape, with the caveat that the
   * operation may include high probability estimates when the actual
   * calculation is prohibitively expensive. The {@link java.awt.geom.Area} 
   * class can be used for more precise answers.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle, undefined results if negative
   * @param h the height of the rectangle, undefined results if negative
   * @return true if the rectangle intersects this shape
   * @see java.awt.geom.Area
   * @since 1.2
   */
  boolean intersects(double x, double y, double w, double h);

  /**
   * Test if a high-precision rectangle intersects the shape. This is true
   * if any point in the rectangle is in the shape, with the caveat that the
   * operation may include high probability estimates when the actual
   * calculation is prohibitively expensive. The {@link java.awt.geom.Area} 
   * class can be used for more precise answers.
   *
   * @param r the rectangle
   * @return true if the rectangle intersects this shape
   * @throws NullPointerException if r is null
   * @see #intersects(double, double, double, double)
   * @since 1.2
   */
  boolean intersects(Rectangle2D r);

  /**
   * Test if a high-precision rectangle lies completely in the shape. This is
   * true if all points in the rectangle are in the shape, with the caveat
   * that the operation may include high probability estimates when the actual
   * calculation is prohibitively expensive. The {@link java.awt.geom.Area} 
   * class can be used for more precise answers.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle, undefined results if negative
   * @param h the height of the rectangle, undefined results if negative
   * @return true if the rectangle is contained in this shape
   * @see java.awt.geom.Area
   * @since 1.2
   */
  boolean contains(double x, double y, double w, double h);

  /**
   * Test if a high-precision rectangle lies completely in the shape. This is
   * true if all points in the rectangle are in the shape, with the caveat
   * that the operation may include high probability estimates when the actual
   * calculation is prohibitively expensive. The {@link java.awt.geom.Area} 
   * class can be used for more precise answers.
   *
   * @param r the rectangle
   * @return true if the rectangle is contained in this shape
   * @throws NullPointerException if r is null
   * @see #contains(double, double, double, double)
   * @since 1.2
   */
  boolean contains(Rectangle2D r);

  /**
   * Return an iterator along the shape boundary. If the optional transform
   * is provided, the iterator is transformed accordingly. Each call returns
   * a new object, independent from others in use. It is recommended, but
   * not required, that the Shape isolate iterations from future changes to
   * the boundary, and document this fact.
   *
   * @param transform an optional transform to apply to the iterator
   * @return a new iterator over the boundary
   * @since 1.2
   */
  PathIterator getPathIterator(AffineTransform transform);

  /**
   * Return an iterator along the flattened version of the shape boundary.
   * Only SEG_MOVETO, SEG_LINETO, and SEG_CLOSE points are returned in the
   * iterator. The flatness paramter controls how far points are allowed to
   * differ from the real curve; although a limit on accuracy may cause this
   * parameter to be enlarged if needed.
   *
   * <p>If the optional transform is provided, the iterator is transformed
   * accordingly. Each call returns a new object, independent from others in
   * use. It is recommended, but not required, that the Shape isolate
   * iterations from future changes to the boundary, and document this fact.
   *
   * @param transform an optional transform to apply to the iterator
   * @param flatness the maximum distance for deviation from the real boundary
   * @return a new iterator over the boundary
   * @since 1.2
   */
  PathIterator getPathIterator(AffineTransform transform, double flatness);
} // interface Shape
