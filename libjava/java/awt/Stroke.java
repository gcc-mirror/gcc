/* Stroke.java -- a stroked outline of a shape
   Copyright (C) 2002, 2005  Free Software Foundation, Inc.

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

/**
 * This interface allows a Graphics2D to grab the outline of a shape, as if
 * stroked by a marking pen of appropriate size and shape. The area inked
 * by the pen is the area of this stroke. Anything in the graphic which
 * traces an outline will use this stroke, such as <code>drawLine</code>.
 * Strokes must be immutable, because the graphics object does not clone
 * them.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see BasicStroke
 * @see Graphics2D#setStroke(Stroke)
 * @since 1.1
 * @status updated to 1.4
 */
public interface Stroke
{
  /**
   * Returns a shape which outlines the boundary of the given shape, in
   * effect converting the infinitely thin line into a new shape.
   *
   * @param s the shape to stroke
   * @return the stroked outline shape
   */
  Shape createStrokedShape(Shape s);
} // interface Stroke
