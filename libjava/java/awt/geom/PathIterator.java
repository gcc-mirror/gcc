/* PathIterator.java -- describes a shape by iterating over its vertices
   Copyright (C) 2000, 2002, 2003 Free Software Foundation

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
 * This interface provides a directed path over the boundary of a shape. The
 * path can contain 1st through 3rd order Bezier curves (lines, and quadratic
 * and cubic splines). A shape can have multiple disjoint paths via the
 * MOVETO directive, and can close a circular path back to the previos
 * MOVETO via the CLOSE directive.
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see java.awt.Shape
 * @see java.awt.Stroke
 * @see FlatteningPathIterator
 * @since 1.2
 * @status updated to 1.4
 */
public interface PathIterator
{
  /**
   * The even-odd winding mode: a point is internal to the shape if a ray
   * from the point to infinity (in any direction) crosses an odd number of
   * segments.
   */
  int WIND_EVEN_ODD = 0;

  /**
   * The non-zero winding mode: a point is internal to the shape if a ray
   * from the point to infinity (in any direction) crosses a different number
   * of segments headed clockwise than those headed counterclockwise.
   */
  int WIND_NON_ZERO = 1;

  /**
   * Starts a new subpath. There is no segment from the previous vertex.
   */
  int SEG_MOVETO = 0;

  /**
   * The current segment is a line.
   */
  int SEG_LINETO = 1;

  /**
   * The current segment is a quadratic parametric curve. It is interpolated
   * as t varies from 0 to 1 over the current point (CP), first control point
   * (P1), and final interpolated control point (P2):
   * <pre>
   *  P(t) = B(2,0)*CP + B(2,1)*P1 + B(2,2)*P2
   *    0 &lt;= t &lt;= 1
   *  B(n,m) = mth coefficient of nth degree Bernstein polynomial
   *         = C(n,m) * t^(m) * (1 - t)^(n-m)
   *  C(n,m) = Combinations of n things, taken m at a time
   *         = n! / (m! * (n-m)!)
   * </pre>
   */
  int SEG_QUADTO = 2;

  /**
   * The current segment is a cubic parametric curve (more commonly known as
   * a Bezier curve). It is interpolated as t varies from 0 to 1 over the
   * current point (CP), first control point (P1), the second control point
   * (P2), and final interpolated control point (P3):
   * <pre>
   *  P(t) = B(3,0)*CP + B(3,1)*P1 + B(3,2)*P2 + B(3,3)*P3
   *    0 &lt;= t &lt;= 1
   *  B(n,m) = mth coefficient of nth degree Bernstein polynomial
   *         = C(n,m) * t^(m) * (1 - t)^(n-m)
   *  C(n,m) = Combinations of n things, taken m at a time
   *         = n! / (m! * (n-m)!)
   * </pre>
   */
  int SEG_CUBICTO = 3;

  /**
   * The current segment closes a loop by an implicit line to the previous
   * SEG_MOVETO coordinate.
   */
  int SEG_CLOSE = 4;

  /**
   * Returns the winding rule to determine which points are inside this path.
   *
   * @return the winding rule
   * @see #WIND_EVEN_ODD
   * @see #WIND_NON_ZERO
   */
  int getWindingRule();

  /**
   * Tests if the iterator is exhausted. If this returns true, currentSegment
   * and next may throw a NoSuchElementException (although this is not
   * required).
   *
   * @return true if the iteration is complete
   */
  boolean isDone();

  /**
   * Advance to the next segment in the iteration. It is not specified what
   * this does if called when isDone() returns true.
   *
   * @throws java.util.NoSuchElementException optional when isDone() is true
   */
  void next();

  /**
   * Returns the coordinates of the next point(s), as well as the type of
   * line segment. The input array must be at least a float[6], to accomodate
   * up to three (x,y) point pairs (although if you know the iterator is
   * flat, you can probably get by with a float[2]). If the returned type is
   * SEG_MOVETO or SEG_LINETO, the first point in the array is modified; if
   * the returned type is SEG_QUADTO, the first two points are modified; if
   * the returned type is SEG_CUBICTO, all three points are modified; and if
   * the returned type is SEG_CLOSE, the array is untouched.
   *
   * @param coords the array to place the point coordinates in
   * @return the segment type
   * @throws NullPointerException if coords is null
   * @throws ArrayIndexOutOfBoundsException if coords is too small
   * @throws java.util.NoSuchElementException optional when isDone() is true
   * @see #SEG_MOVETO
   * @see #SEG_LINETO
   * @see #SEG_QUADTO
   * @see #SEG_CUBICTO
   * @see #SEG_CLOSE
   */
  int currentSegment(float[] coords);

  /**
   * Returns the coordinates of the next point(s), as well as the type of
   * line segment. The input array must be at least a double[6], to accomodate
   * up to three (x,y) point pairs (although if you know the iterator is
   * flat, you can probably get by with a double[2]). If the returned type is
   * SEG_MOVETO or SEG_LINETO, the first point in the array is modified; if
   * the returned type is SEG_QUADTO, the first two points are modified; if
   * the returned type is SEG_CUBICTO, all three points are modified; and if
   * the returned type is SEG_CLOSE, the array is untouched.
   *
   * @param coords the array to place the point coordinates in
   * @return the segment type
   * @throws NullPointerException if coords is null
   * @throws ArrayIndexOutOfBoundsException if coords is too small
   * @throws java.util.NoSuchElementException optional when isDone() is true
   * @see #SEG_MOVETO
   * @see #SEG_LINETO
   * @see #SEG_QUADTO
   * @see #SEG_CUBICTO
   * @see #SEG_CLOSE
   */
  int currentSegment(double[] coords);
} // interface PathIterator
