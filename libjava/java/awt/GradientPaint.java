/* GradientPaint.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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

import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;

/**
 * STUB CLASS ONLY
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

  public GradientPaint(float x1, float y1, Color c1,
                       float x2, float y2, Color c2)
  {
    this(x1, y1, c1, x2, y2, c2, false);
  }

  public GradientPaint(Point2D p1, Color c1, Point2D p2, Color c2)
  {
    this((float) p1.getX(), (float) p1.getY(), c1,
         (float) p2.getX(), (float) p2.getY(), c2, false);
  }

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

  public GradientPaint(Point2D p1, Color c1, Point2D p2, Color c2,
                       boolean cyclic)
  {
    this((float) p1.getX(), (float) p1.getY(), c1,
         (float) p2.getX(), (float) p2.getY(), c2, cyclic);
  }

  public Point2D getPoint1()
  {
    return new Point2D.Float(x1, y1);
  }

  public Color getColor1()
  {
    return c1;
  }

  public Point2D getPoint2()
  {
    return new Point2D.Float(x2, y2);
  }

  public Color getColor2()
  {
    return c2;
  }

  public boolean isCyclic()
  {
    return cyclic;
  }

  public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
                                    Rectangle2D userBounds,
                                    AffineTransform xform,
                                    RenderingHints hints)
  {
    throw new Error("not implemented");
  }

  public int getTransparency()
  {
    throw new Error("not implemented");
  }
} // class GradientPaint
