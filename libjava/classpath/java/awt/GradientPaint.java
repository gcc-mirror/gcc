/* GradientPaint.java -- 
   Copyright (C) 2002, 2005, Free Software Foundation, Inc.

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
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;
import gnu.java.awt.GradientPaintContext;

/**
 * A paint object that can be used to color a region by blending two colors. 
 * Instances of this class are immutable.
 */
public class GradientPaint implements Paint
{
  private final float x1;
  private final float y1;
  private final Color c1;
  private final float x2;
  private final float y2;
  private final Color c2;
  private final boolean cyclic;

  /**
   * Creates a new acyclic <code>GradientPaint</code>.
   * 
   * @param x1  the x-coordinate of the anchor point for color 1.
   * @param y1  the y-coordinate of the anchor point for color 1.
   * @param c1  color 1 (<code>null</code> not permitted).
   * @param x2  the x-coordinate of the anchor point for color 2.
   * @param y2  the y-coordinate of the anchor point for color 2.
   * @param c2  the second color (<code>null</code> not permitted).
   */
  public GradientPaint(float x1, float y1, Color c1,
                       float x2, float y2, Color c2)
  {
    this(x1, y1, c1, x2, y2, c2, false);
  }

  /**
   * Creates a new acyclic <code>GradientPaint</code>.
   * 
   * @param p1  anchor point 1 (<code>null</code> not permitted).
   * @param c1  color 1 (<code>null</code> not permitted).
   * @param p2  anchor point 2 (<code>null</code> not permitted).
   * @param c2  color 2 (<code>null</code> not permitted).
   */
  public GradientPaint(Point2D p1, Color c1, Point2D p2, Color c2)
  {
    this((float) p1.getX(), (float) p1.getY(), c1,
         (float) p2.getX(), (float) p2.getY(), c2, false);
  }

  /**
   * Creates a new cyclic or acyclic <code>GradientPaint</code>.
   * 
   * @param x1  the x-coordinate of the anchor point for color 1.
   * @param y1  the y-coordinate of the anchor point for color 1.
   * @param c1  color 1 (<code>null</code> not permitted).
   * @param x2  the x-coordinate of the anchor point for color 2.
   * @param y2  the y-coordinate of the anchor point for color 2.
   * @param c2  the second color (<code>null</code> not permitted).
   * @param cyclic  a flag that controls whether the gradient is cyclic or
   *                acyclic.
   */
  public GradientPaint(float x1, float y1, Color c1,
                       float x2, float y2, Color c2, boolean cyclic)
  {
    if (c1 == null || c2 == null)
      throw new NullPointerException();
    this.x1 = x1;
    this.y1 = y1;
    this.c1 = c1;
    this.x2 = x2;
    this.y2 = y2;
    this.c2 = c2;
    this.cyclic = cyclic;
  }

  /**
   * Creates a new cyclic or acyclic <code>GradientPaint</code>.
   * 
   * @param p1  anchor point 1 (<code>null</code> not permitted).
   * @param c1  color 1 (<code>null</code> not permitted).
   * @param p2  anchor point 2 (<code>null</code> not permitted).
   * @param c2  color 2 (<code>null</code> not permitted).
   * @param cyclic  a flag that controls whether the gradient is cyclic or
   *                acyclic.
   */
  public GradientPaint(Point2D p1, Color c1, Point2D p2, Color c2,
                       boolean cyclic)
  {
    this((float) p1.getX(), (float) p1.getY(), c1,
         (float) p2.getX(), (float) p2.getY(), c2, cyclic);
  }

  /**
   * Returns a point with the same coordinates as the anchor point for color 1.
   * Note that if you modify this point, the <code>GradientPaint</code> remains
   * unchanged.
   * 
   * @return A point with the same coordinates as the anchor point for color 1.
   */
  public Point2D getPoint1()
  {
    return new Point2D.Float(x1, y1);
  }

  /**
   * Returns the first color.
   * 
   * @return The color (never <code>null</code>).
   */
  public Color getColor1()
  {
    return c1;
  }

  /**
   * Returns a point with the same coordinates as the anchor point for color 2.
   * Note that if you modify this point, the <code>GradientPaint</code> remains
   * unchanged.
   * 
   * @return A point with the same coordinates as the anchor point for color 2.
   */
  public Point2D getPoint2()
  {
    return new Point2D.Float(x2, y2);
  }

  /**
   * Returns the second color.
   * 
   * @return The color (never <code>null</code>).
   */
  public Color getColor2()
  {
    return c2;
  }

  /**
   * Returns <code>true</code> if this <code>GradientPaint</code> instance is
   * cyclic, and <code>false</code> otherwise.
   * 
   * @return A boolean.
   */
  public boolean isCyclic()
  {
    return cyclic;
  }

  /**
   * Returns the {@link PaintContext} used to generate the color pattern.
   * 
   * @param cm  the color model, used as a hint (ignored in this 
   *            implementation).
   * @param deviceBounds  the device space bounding box of the painted area.
   * @param userBounds  the user space bounding box of the painted area.
   * @param xform  the transformation from user space to device space.
   * @param hints  any hints for choosing between rendering alternatives.
   * 
   * @return The context for performing the paint
   */
  public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
                                    Rectangle2D userBounds,
                                    AffineTransform xform,
                                    RenderingHints hints)
  {
    Point2D xp1 = xform.transform(getPoint1(), null);
    Point2D xp2 = xform.transform(getPoint2(), null);
    return new GradientPaintContext((float) xp1.getX(), (float) xp1.getY(), c1, 
            (float) xp2.getX(), (float) xp2.getY(), c2, cyclic);
  }

  /**
   * Returns the transparency code for this <code>GradientPaint</code> instance.
   * This is derived from the two {@link Color} objects used in creating this
   * object:  if both colors are opaque, this method returns 
   * {@link Transparency#OPAQUE}, otherwise it returns 
   * {@link Transparency#TRANSLUCENT}.
   * 
   * @return {@link Transparency#OPAQUE} or {@link Transparency#TRANSLUCENT}.
   */
  public int getTransparency()
  {
    if (c1.getAlpha() == 255 && c2.getAlpha() == 255)
      return Transparency.OPAQUE;
    else
      return Transparency.TRANSLUCENT;   
  }
  
} // class GradientPaint
