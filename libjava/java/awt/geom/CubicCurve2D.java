/* CubicCurve2D.java -- represents a parameterized cubic curve in 2-D space
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
import java.util.NoSuchElementException;

/**
 * STUBS ONLY
 * XXX Implement and document.
 */
public abstract class CubicCurve2D implements Shape, Cloneable
{
  protected CubicCurve2D()
  {
  }

  public abstract double getX1();
  public abstract double getY1();
  public abstract Point2D getP1();
  public abstract double getCtrlX1();
  public abstract double getCtrlY1();
  public abstract Point2D getCtrlP1();
  public abstract double getCtrlX2();
  public abstract double getCtrlY2();
  public abstract Point2D getCtrlP2();
  public abstract double getX2();
  public abstract double getY2();
  public abstract Point2D getP2();

  public abstract void setCurve(double x1, double y1, double cx1, double cy1,
                                double cx2, double cy2, double x2, double y2);
  public void setCurve(double[] coords, int offset)
  {
    setCurve(coords[offset++], coords[offset++],
             coords[offset++], coords[offset++],
             coords[offset++], coords[offset++],
             coords[offset++], coords[offset++]);
  }
  public void setCurve(Point2D p1, Point2D c1, Point2D c2, Point2D p2)
  {
    setCurve(p1.getX(), p1.getY(), c1.getX(), c1.getY(),
             c2.getX(), c2.getY(), p2.getX(), p2.getY());
  }
  public void setCurve(Point2D[] pts, int offset)
  {
    setCurve(pts[offset].getX(), pts[offset++].getY(),
             pts[offset].getX(), pts[offset++].getY(),
             pts[offset].getX(), pts[offset++].getY(),
             pts[offset].getX(), pts[offset++].getY());
  }
  public void setCurve(CubicCurve2D c)
  {
    setCurve(c.getX1(), c.getY1(), c.getCtrlX1(), c.getCtrlY1(),
             c.getCtrlX2(), c.getCtrlY2(), c.getX2(), c.getY2());
  }
  public static double getFlatnessSq(double x1, double y1, double cx1,
                                     double cy1, double cx2, double cy2,
                                     double x2, double y2)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public static double getFlatness(double x1, double y1, double cx1,
                                   double cy1, double cx2, double cy2,
                                   double x2, double y2)
  {
    return Math.sqrt(getFlatnessSq(x1, y1, cx1, cy1, cx2, cy2, x2, y2));
  }
  public static double getFlatnessSq(double[] coords, int offset)
  {
    return getFlatnessSq(coords[offset++], coords[offset++],
                         coords[offset++], coords[offset++],
                         coords[offset++], coords[offset++],
                         coords[offset++], coords[offset++]);
  }
  public static double getFlatness(double[] coords, int offset)
  {
    return Math.sqrt(getFlatnessSq(coords[offset++], coords[offset++],
                                   coords[offset++], coords[offset++],
                                   coords[offset++], coords[offset++],
                                   coords[offset++], coords[offset++]));
  }
  public double getFlatnessSq()
  {
    return getFlatnessSq(getX1(), getY1(), getCtrlX1(), getCtrlY1(),
                         getCtrlX2(), getCtrlY2(), getX2(), getY2());
  }
  public double getFlatness()
  {
    return Math.sqrt(getFlatnessSq(getX1(), getY1(), getCtrlX1(),
                                   getCtrlY1(), getCtrlX2(), getCtrlY2(),
                                   getX2(), getY2()));
  }

  public void subdivide(CubicCurve2D l, CubicCurve2D r)
  {
    if (l == null)
      l = new CubicCurve2D.Double();
    if (r == null)
      r = new CubicCurve2D.Double();
    // Use empty slots at end to share single array.
    double[] d = new double[] { getX1(), getY1(), getCtrlX1(), getCtrlY1(),
                                getCtrlX2(), getCtrlY2(), getX2(), getY2(),
                                0, 0, 0, 0, 0, 0 };
    subdivide(d, 0, d, 0, d, 6);
    l.setCurve(d, 0);
    r.setCurve(d, 6);
  }
  public static void subdivide(CubicCurve2D src,
                               CubicCurve2D l, CubicCurve2D r)
  {
    src.subdivide(l, r);
  }
  public static void subdivide(double[] src, int srcOff,
                               double[] left, int leftOff,
                               double[] right, int rightOff)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public static int solveCubic(double[] eqn)
  {
    return solveCubic(eqn, eqn);
  }
  public static int solveCubic(double[] eqn, double[] res)
  {
    if (eqn[3] == 0)
      return QuadCurve2D.solveQuadratic(eqn, res);
    // XXX Implement.
    throw new Error("not implemented");
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
  public boolean intersects(double x, double y, double w, double h)
  {
    // XXX Implement.
    throw new Error("not implemented");
  }
  public boolean intersects(Rectangle2D r)
  {
    return intersects(r.getX(), r.getY(), r.getWidth(), r.getHeight());
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
  public Rectangle getBounds()
  {
    return getBounds2D().getBounds();
  }
  public PathIterator getPathIterator(final AffineTransform at)
  {
    return new PathIterator()
    {
      /** Current coordinate. */
      private int current;

      public int getWindingRule()
      {
        return WIND_NON_ZERO;
      }

      public boolean isDone()
      {
        return current < 2;
      }

      public void next()
      {
        current++;
      }

      public int currentSegment(float[] coords)
      {
        if (current == 0)
          {
            coords[0] = (float) getX1();
            coords[1] = (float) getY1();
            if (at != null)
              at.transform(coords, 0, coords, 0, 1);
            return SEG_MOVETO;
          }
        if (current == 1)
          {
            coords[0] = (float) getCtrlX1();
            coords[1] = (float) getCtrlY1();
            coords[2] = (float) getCtrlX2();
            coords[3] = (float) getCtrlY2();
            coords[4] = (float) getX2();
            coords[5] = (float) getY2();
            if (at != null)
              at.transform(coords, 0, coords, 0, 3);
            return SEG_CUBICTO;
          }
        throw new NoSuchElementException("cubic iterator out of bounds");
      }

      public int currentSegment(double[] coords)
      {
        if (current == 0)
          {
            coords[0] = getX1();
            coords[1] = getY1();
            if (at != null)
              at.transform(coords, 0, coords, 0, 1);
            return SEG_MOVETO;
          }
        if (current == 1)
          {
            coords[0] = getCtrlX1();
            coords[1] = getCtrlY1();
            coords[2] = getCtrlX2();
            coords[3] = getCtrlY2();
            coords[4] = getX2();
            coords[5] = getY2();
            if (at != null)
              at.transform(coords, 0, coords, 0, 3);
            return SEG_CUBICTO;
          }
        throw new NoSuchElementException("cubic iterator out of bounds");
      }
    };
  }
  public PathIterator getPathIterator(AffineTransform at, double flatness)
  {
    return new FlatteningPathIterator(getPathIterator(at), flatness);
  }

  /**
   * Create a new curve of the same run-time type with the same contents as
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

  /**
   * STUBS ONLY
   */
  public static class Double extends CubicCurve2D
  {
    public double x1;
    public double y1;
    public double ctrlx1;
    public double ctrly1;
    public double ctrlx2;
    public double ctrly2;
    public double x2;
    public double y2;

    public Double()
    {
    }

    public Double(double x1, double y1, double cx1, double cy1,
                  double cx2, double cy2, double x2, double y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      ctrlx1 = cx1;
      ctrly1 = cy1;
      ctrlx2 = cx2;
      ctrly2 = cy2;
      this.x2 = x2;
      this.y2 = y2;
    }

    public double getX1()
    {
      return x1;
    }
    public double getY1()
    {
      return y1;
    }
    public Point2D getP1()
    {
      return new Point2D.Double(x1, y1);
    }

    public double getCtrlX1()
    {
      return ctrlx1;
    }
    public double getCtrlY1()
    {
      return ctrly1;
    }
    public Point2D getCtrlP1()
    {
      return new Point2D.Double(ctrlx1, ctrly1);
    }

    public double getCtrlX2()
    {
      return ctrlx2;
    }
    public double getCtrlY2()
    {
      return ctrly2;
    }
    public Point2D getCtrlP2()
    {
      return new Point2D.Double(ctrlx2, ctrly2);
    }

    public double getX2()
    {
      return x2;
    }
    public double getY2()
    {
      return y2;
    }
    public Point2D getP2()
    {
      return new Point2D.Double(x2, y2);
    }

    public void setCurve(double x1, double y1, double cx1, double cy1,
                         double cx2, double cy2, double x2, double y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      ctrlx1 = cx1;
      ctrly1 = cy1;
      ctrlx2 = cx2;
      ctrly2 = cy2;
      this.x2 = x2;
      this.y2 = y2;
    }
    public Rectangle2D getBounds2D()
    {
      double nx1 = Math.min(Math.min(x1, ctrlx1), Math.min(ctrlx2, x2));
      double ny1 = Math.min(Math.min(y1, ctrly1), Math.min(ctrly2, y2));
      double nx2 = Math.max(Math.max(x1, ctrlx1), Math.max(ctrlx2, x2));
      double ny2 = Math.max(Math.max(y1, ctrly1), Math.max(ctrly2, y2));
      return new Rectangle2D.Double(nx1, ny1, nx2 - nx1, ny2 - ny1);
    }
  } // class Double

  /**
   * STUBS ONLY
   */
  public static class Float extends CubicCurve2D
  {
    public float x1;
    public float y1;
    public float ctrlx1;
    public float ctrly1;
    public float ctrlx2;
    public float ctrly2;
    public float x2;
    public float y2;

    public Float()
    {
    }

    public Float(float x1, float y1, float cx1, float cy1,
                 float cx2, float cy2, float x2, float y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      ctrlx1 = cx1;
      ctrly1 = cy1;
      ctrlx2 = cx2;
      ctrly2 = cy2;
      this.x2 = x2;
      this.y2 = y2;
    }

    public double getX1()
    {
      return x1;
    }
    public double getY1()
    {
      return y1;
    }
    public Point2D getP1()
    {
      return new Point2D.Float(x1, y1);
    }

    public double getCtrlX1()
    {
      return ctrlx1;
    }
    public double getCtrlY1()
    {
      return ctrly1;
    }
    public Point2D getCtrlP1()
    {
      return new Point2D.Float(ctrlx1, ctrly1);
    }

    public double getCtrlX2()
    {
      return ctrlx2;
    }
    public double getCtrlY2()
    {
      return ctrly2;
    }
    public Point2D getCtrlP2()
    {
      return new Point2D.Float(ctrlx2, ctrly2);
    }

    public double getX2()
    {
      return x2;
    }
    public double getY2()
    {
      return y2;
    }
    public Point2D getP2()
    {
      return new Point2D.Float(x2, y2);
    }

    public void setCurve(double x1, double y1, double cx1, double cy1,
                         double cx2, double cy2, double x2, double y2)
    {
      this.x1 = (float) x1;
      this.y1 = (float) y1;
      ctrlx1 = (float) cx1;
      ctrly1 = (float) cy1;
      ctrlx2 = (float) cx2;
      ctrly2 = (float) cy2;
      this.x2 = (float) x2;
      this.y2 = (float) y2;
    }
    public void setCurve(float x1, float y1, float cx1, float cy1,
                         float cx2, float cy2, float x2, float y2)
    {
      this.x1 = x1;
      this.y1 = y1;
      ctrlx1 = cx1;
      ctrly1 = cy1;
      ctrlx2 = cx2;
      ctrly2 = cy2;
      this.x2 = x2;
      this.y2 = y2;
    }
    public Rectangle2D getBounds2D()
    {
      float nx1 = (float) Math.min(Math.min(x1, ctrlx1), Math.min(ctrlx2, x2));
      float ny1 = (float) Math.min(Math.min(y1, ctrly1), Math.min(ctrly2, y2));
      float nx2 = (float) Math.max(Math.max(x1, ctrlx1), Math.max(ctrlx2, x2));
      float ny2 = (float) Math.max(Math.max(y1, ctrly1), Math.max(ctrly2, y2));
      return new Rectangle2D.Float(nx1, ny1, nx2 - nx1, ny2 - ny1);
    }
  } // class Float
} // class CubicCurve2D
