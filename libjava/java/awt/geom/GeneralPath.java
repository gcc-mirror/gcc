/* GeneralPath.java -- represents a shape built from subpaths
   Copyright (C) 2002, 2003 Free Software Foundation

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
 * XXX Implement and document. Note that Sun's implementation only expects
 * float precision, not double.
 */
public final class GeneralPath implements Shape, Cloneable
{
  public static final int WIND_EVEN_ODD = PathIterator.WIND_EVEN_ODD;
  public static final int WIND_NON_ZERO = PathIterator.WIND_NON_ZERO;

  /** Initial size if not specified. */
  private static final int INIT_SIZE = 20;

  /** The winding rule. */
  private int rule;
  /**
   * The path type in points. Note that points[index] maps to
   * types[index >> 1]; the control points of quad and cubic paths map as
   * well but are ignored.
   */
  private byte[] types;
  /**
   * The list of all points seen. Since you can only append floats, it makes
   * sense for this to be a float[]. I have no idea why Sun didn't choose to
   * allow a general path of double precision points.
   */
  private float[] points;
  /** The index of the most recent moveto point, or null. */
  private int subpath = -1;
  /** The next available index into points. */
  private int index;

  public GeneralPath()
  {
    this(WIND_NON_ZERO, INIT_SIZE);
  }
  public GeneralPath(int rule)
  {
    this(rule, INIT_SIZE);
  }
  public GeneralPath(int rule, int capacity)
  {
    if (rule != WIND_EVEN_ODD && rule != WIND_NON_ZERO)
      throw new IllegalArgumentException();
    this.rule = rule;
    if (capacity < INIT_SIZE)
      capacity = INIT_SIZE;
    types = new byte[capacity >> 1];
    points = new float[capacity];
  }
  public GeneralPath(Shape s)
  {
    types = new byte[INIT_SIZE >> 1];
    points = new float[INIT_SIZE];
    PathIterator pi = s.getPathIterator(null);
    setWindingRule(pi.getWindingRule());
    append(pi, false);
  }

  public void moveTo(float x, float y)
  {
    subpath = index;
    ensureSize(index + 2);
    types[index >> 1] = PathIterator.SEG_MOVETO;
    points[index++] = x;
    points[index++] = y;
  }
  public void lineTo(float x, float y)
  {
    ensureSize(index + 2);
    types[index >> 1] = PathIterator.SEG_LINETO;
    points[index++] = x;
    points[index++] = y;
  }
  public void quadTo(float x1, float y1, float x2, float y2)
  {
    ensureSize(index + 4);
    types[index >> 1] = PathIterator.SEG_QUADTO;
    points[index++] = x1;
    points[index++] = y1;
    points[index++] = x2;
    points[index++] = y2;
  }
  public void curveTo(float x1, float y1, float x2, float y2,
                      float x3, float y3)
  {
    ensureSize(index + 6);
    types[index >> 1] = PathIterator.SEG_CUBICTO;
    points[index++] = x1;
    points[index++] = y1;
    points[index++] = x2;
    points[index++] = y2;
    points[index++] = x3;
    points[index++] = y3;
  }
  public void closePath()
  {
    ensureSize(index + 2);
    types[index >> 1] = PathIterator.SEG_CLOSE;
    points[index++] = points[subpath];
    points[index++] = points[subpath + 1];
  }

  public void append(Shape s, boolean connect)
  {
    append(s.getPathIterator(null), connect);
  }


  /**
   * Appends the segments of a PathIterator to this GeneralPath.
   * Optionally, the initial {@link PathIterator#SEG_MOVETO} segment
   * of the appended path is changed into a {@link
   * PathIterator#SEG_LINETO} segment.
   *
   * @param iter the PathIterator specifying which segments shall be
   * appended.
   * 
   * @param connect <code>true</code> for substituting the initial
   * {@link PathIterator#SEG_MOVETO} segment by a {@link
   * PathIterator#SEG_LINETO}, or <code>false</code> for not
   * performing any substitution. If this GeneralPath is currently
   * empty, <code>connect</code> is assumed to be <code>false</code>,
   * thus leaving the initial {@link PathIterator#SEG_MOVETO}
   * unchanged.
   */
  public void append(PathIterator iter, boolean connect)
  {
    // A bad implementation of this method had caused Classpath bug #6076.
    float[] f = new float[6];
    while (!iter.isDone())
    {
      switch (iter.currentSegment(f))
      {
      case PathIterator.SEG_MOVETO:
        if (!connect || (index == 0))
        {
          moveTo(f[0], f[1]);
          break;
        }

        if ((index >= 2) && (types[(index - 2) >> 2] == PathIterator.SEG_CLOSE)
            && (f[0] == points[index - 2]) && (f[1] == points[index - 1]))
          break;
        
        // Fall through.

      case PathIterator.SEG_LINETO:
        lineTo(f[0], f[1]);
        break;

      case PathIterator.SEG_QUADTO:
        quadTo(f[0], f[1], f[2], f[3]);
        break;

      case PathIterator.SEG_CUBICTO:
        curveTo(f[0], f[1], f[2], f[3], f[4], f[5]);
        break;

      case PathIterator.SEG_CLOSE:
        closePath();
        break;
      }

      connect = false;
      iter.next();
    }
  }


  public int getWindingRule()
  {
    return rule;
  }
  public void setWindingRule(int rule)
  {
    if (rule != WIND_EVEN_ODD && rule != WIND_NON_ZERO)
      throw new IllegalArgumentException();
    this.rule = rule;
  }

  public Point2D getCurrentPoint()
  {
    if (subpath < 0)
      return null;
    return new Point2D.Float(points[index - 2], points[index - 1]);
  }
  public void reset()
  {
    subpath = -1;
    index = 0;
  }

  public void transform(AffineTransform xform)
  {
    xform.transform(points, 0, points, 0, index >> 1);
  }
  public Shape createTransformedShape(AffineTransform xform)
  {
    GeneralPath p = new GeneralPath(this);
    p.transform(xform);
    return p;
  }

  public Rectangle getBounds()
  {
    return getBounds2D().getBounds();
  }
  public Rectangle2D getBounds2D()
  {
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


  /**
   * A PathIterator that iterates over the segments of a GeneralPath.
   *
   * @author Sascha Brawer (brawer@dandelis.ch)
   */
  private static class GeneralPathIterator
    implements PathIterator
  {
    /**
     * The number of coordinate values for each segment type.
     */
    private static final int[] NUM_COORDS =
    {
      /* 0: SEG_MOVETO */ 2,
      /* 1: SEG_LINETO */ 2,
      /* 2: SEG_QUADTO */ 4,
      /* 3: SEG_CUBICTO */ 6,
      /* 4: SEG_CLOSE */ 0
    };


    /**
     * The GeneralPath whose segments are being iterated.
     */
    private final GeneralPath path;


    /**
     * The affine transformation used to transform coordinates.
     */
    private final AffineTransform transform;


    /**
     * The current position of the iterator.
     */
    private int pos;


    /**
     * Constructs a new iterator for enumerating the segments of a
     * GeneralPath.
     *
     * @param at an affine transformation for projecting the returned
     * points, or <code>null</code> to return the original points
     * without any mapping.
     */
    GeneralPathIterator(GeneralPath path, AffineTransform transform)
    {
      this.path = path;
      this.transform = transform;
    }


    /**
     * Returns the current winding rule of the GeneralPath.
     */
    public int getWindingRule()
    {
      return path.rule;
    }


    /**
     * Determines whether the iterator has reached the last segment in
     * the path.
     */
    public boolean isDone()
    {
      return pos >= path.index;
    }


    /**
     * Advances the iterator position by one segment.
     */
    public void next()
    {
      int seg;

      /* Increment pos by the number of coordinate values. Note that
       * we store two values even for a SEG_CLOSE segment, which is
       * why we increment pos at least by 2. 
       */
      seg = path.types[pos >> 1];
      if (seg == SEG_CLOSE)
        pos += 2;
      else
        pos += NUM_COORDS[seg];
    }


    /**
     * Returns the current segment in float coordinates.
     */
    public int currentSegment(float[] coords)
    {
      int seg, numCoords;

      seg = path.types[pos >> 1];
      numCoords = NUM_COORDS[seg];
      if (numCoords > 0)
      {
        if (transform == null)
          System.arraycopy(path.points, pos, coords, 0, numCoords);
        else
          transform.transform(/* src */ path.points, /* srcOffset */ pos,
                              /* dest */ coords, /* destOffset */ 0,
                              /* numPoints */ numCoords >> 1);
      }
      return seg;
    }


    /**
     * Returns the current segment in double coordinates.
     */
    public int currentSegment(double[] coords)
    {
      int seg, numCoords;

      seg = path.types[pos >> 1];
      numCoords = NUM_COORDS[seg];
      if (numCoords > 0)
      {
        if (transform == null)
        {
          // System.arraycopy throws an exception if the source and destination
          // array are not of the same primitive type.
          for (int i = 0; i < numCoords; i++)
            coords[i] = (double) path.points[pos + i];
        }
        else
          transform.transform(/* src */ path.points, /* srcOffset */ pos,
                              /* dest */ coords, /* destOffset */ 0,
                              /* numPoints */ numCoords >> 1);
      }
      return seg;
    }
  }


  /**
   * Creates a PathIterator for iterating along the segments of this path.
   *
   * @param at an affine transformation for projecting the returned
   * points, or <code>null</code> to let the created iterator return
   * the original points without any mapping.
   */
  public PathIterator getPathIterator(AffineTransform at)
  {
    return new GeneralPathIterator(this, at);
  }


  public PathIterator getPathIterator(AffineTransform at, double flatness)
  {
    return new FlatteningPathIterator(getPathIterator(at), flatness);
  }

  /**
   * Create a new shape of the same run-time type with the same contents as
   * this one.
   *
   * @return the clone
   *
   * @exception OutOfMemoryError If there is not enough memory available.
   *
   * @since 1.2
   */
  public Object clone()
  {
    // This class is final; no need to use super.clone().
    return new GeneralPath(this);
  }

  private void ensureSize(int size)
  {
    if (subpath < 0)
      throw new IllegalPathStateException("need initial moveto");
    if (size <= points.length)
      return;
    byte[] b = new byte[points.length];
    System.arraycopy(types, 0, b, 0, index >> 1);
    types = b;
    float[] f = new float[points.length << 1];
    System.arraycopy(points, 0, f, 0, index);
    points = f;
  }
} // class GeneralPath
