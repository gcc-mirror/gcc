/* ShapeGraphicAttribute.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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


package java.awt.font;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;

/**
 * This is an implementation of GraphicAttribute that draws shapes in a TextLayout.
 *
 * @author Lillian Angel (langel at redhat dot com)
 */
public final class ShapeGraphicAttribute extends GraphicAttribute
{
  /** True if the shape should be filled. */
  public static final boolean FILL = false;

  /** True if the shape should be stroked with a 1-pixel wide stroke. */
  public static final boolean STROKE = true;

  private Shape shape;
  private boolean stroke;
  private Rectangle2D bounds;

  /**
   * Constructor.
   *
   * @param shape - the Shape to render. The Shape is rendered with its origin.
   * @param alignment - the alignment
   * @param stroke - true if the Shape should be stroked; false if the Shape
   *          should be filled.
   */
  public ShapeGraphicAttribute(Shape shape, int alignment, boolean stroke)
  {
    super(alignment);
    this.shape = shape;
    this.stroke = stroke;
    this.bounds = shape.getBounds2D();
  }

  /**
   * Draws the graphic at the given location.
   *
   * @param graphics - the graphics to use.
   * @param x - the x location to draw at.
   * @param y - the y location to draw at.
   */
  public void draw(Graphics2D graphics, float x, float y)
  {
    graphics.translate(x, y);
    if (stroke == STROKE)
      graphics.draw(shape);
    else
      graphics.fill(shape);
    graphics.translate(- x, - y);
  }

  /**
   * Compares this ShapeGraphicAttribute to obj.
   *
   * @param obj - the object to compare.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof ShapeGraphicAttribute))
      return false;

    return equals((ShapeGraphicAttribute) obj);
  }

  /**
   * Compares this ShapeGraphicAttribute to rhs.
   *
   * @param rhs - the ShapeGraphicAttribute to compare.
   */
  public boolean equals(ShapeGraphicAttribute rhs)
  {
    return (this == rhs || (this.shape.equals(rhs.shape)
                            && getAlignment() == rhs.getAlignment()
                            && stroke == rhs.stroke
                            && getAdvance() == rhs.getAdvance()
                            && getAscent() == rhs.getAscent()
                            && getBounds().equals(rhs.getBounds())
                            && getDescent() == rhs.getDescent()
                            && hashCode() == rhs.hashCode()));
  }

  /**
   * Gets the distance from the origin of its Shape to the right side of the
   * bounds of its Shape.
   *
   * @return the advance
   */
  public float getAdvance()
  {
    return Math.max(0, (float) bounds.getMaxX());
  }

  /**
   * Gets the positive distance from the origin of its Shape to the top of
   * bounds.
   *
   * @return the ascent
   */
  public float getAscent()
  {
    return Math.max(0, -(float) bounds.getMinY());
  }

  /**
   * Gets the distance from the origin of its Shape to the bottom of the bounds.
   *
   * @return the descent
   */
  public float getDescent()
  {
    return Math.max(0, (float) bounds.getMaxY());
  }

  /**
   * Returns a Rectangle2D that encloses all of the bits drawn by this shape.
   *
   * @return the bounds of the shape.
   */
  public Rectangle2D getBounds()
  {
    Rectangle2D.Float bounds = new Rectangle2D.Float();
    bounds.setRect(this.bounds);

    if (stroke == STROKE)
      {
        bounds.width++;
        bounds.height++;
      }

    return bounds;
  }

  /**
   * Gets the hash code.
   *
   * @return the hash code.
   */
  public int hashCode()
  {
    return shape.hashCode();
  }
}
