/* Copyright (C) 2000, 2001, 2002  Free Software Foundation

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
import java.awt.*;
import java.io.Serializable;

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date April 16, 2000
 */

/* Status: mostly complete.  Search for fixme to see problems.
   Also, TYPE_ returns are not handled correctly.  */

public class AffineTransform implements Cloneable, Serializable
{
  public static final int TYPE_IDENTITY = 0;
  public static final int TYPE_FLIP = 64;
  public static final int TYPE_GENERAL_ROTATION = 16;
  public static final int TYPE_GENERAL_SCALE = 4;
  public static final int TYPE_GENERAL_TRANSFORM = 32;
  public static final int TYPE_MASK_ROTATION = 24;
  public static final int TYPE_MASK_SCALE = 6;
  public static final int TYPE_QUADRANT_ROTATION = 8;
  public static final int TYPE_TRANSLATION = 1;
  public static final int TYPE_UNIFORM_SCALE = 2;

  public AffineTransform ()
  {
    setToIdentity ();
  }

  public AffineTransform (AffineTransform tx)
  {
    setTransform (tx);
  }

  public AffineTransform (float m00, float m10,
			  float m01, float m11,
			  float m02, float m12)
  {
    this.m00 = m00;
    this.m10 = m10;
    this.m01 = m01;
    this.m11 = m11;
    this.m02 = m02;
    this.m12 = m12;
    this.type = TYPE_GENERAL_TRANSFORM;
  }

  public AffineTransform (float[] flatmatrix)
  {
    m00 = flatmatrix[0];
    m10 = flatmatrix[1];
    m01 = flatmatrix[2];
    m11 = flatmatrix[3];
    if (flatmatrix.length >= 6)
      {
	m02 = flatmatrix[4];
	m12 = flatmatrix[5];
      }
  }

  public AffineTransform (double m00, double m10, double m01,
			  double m11, double m02, double m12)
  {
    this.m00 = m00;
    this.m10 = m10;
    this.m01 = m01;
    this.m11 = m11;
    this.m02 = m02;
    this.m12 = m12;
    this.type = TYPE_GENERAL_TRANSFORM;
  }

  public AffineTransform (double[] flatmatrix)
  {
    m00 = flatmatrix[0];
    m10 = flatmatrix[1];
    m01 = flatmatrix[2];
    m11 = flatmatrix[3];
    if (flatmatrix.length >= 6)
      {
	m02 = flatmatrix[4];
	m12 = flatmatrix[5];
      }
  }

  public static AffineTransform getTranslateInstance (double tx, double ty)
  {
    AffineTransform t = new AffineTransform ();
    t.setToTranslation (tx, ty);
    return t;
  }

  public static AffineTransform getRotateInstance (double theta)
  {
    AffineTransform t = new AffineTransform ();
    t.setToRotation (theta);
    return t;
  }

  public static AffineTransform getRotateInstance (double theta,
						   double x, double y)
  {
    AffineTransform t = new AffineTransform ();
    t.rotate (theta, x, y);
    return t;
  }

  public static AffineTransform getScaleInstance (double sx, double sy)
  {
    AffineTransform t = new AffineTransform ();
    t.setToScale (sx, sy);
    return t;
  }

  public static AffineTransform getShearInstance (double shx, double shy)
  {
    AffineTransform t = new AffineTransform ();
    t.setToShear (shx, shy);
    return t;
  }

  public int getType ()
  {
    return type;
  }

  public double getDeterminant ()
  {
    return m00 * m11 - m01 * m10;
  }

  public void getMatrix (double[] flatmatrix)
  {
    flatmatrix[0] = m00;
    flatmatrix[1] = m10;
    flatmatrix[2] = m01;
    flatmatrix[3] = m11;
    if (flatmatrix.length >= 6)
      {
	flatmatrix[4] = m02;
	flatmatrix[5] = m12;
      }
  }

  public double getScaleX ()
  {
    return m00;
  }

  public double getScaleY ()
  {
    return m11;
  }

  public double getShearX ()
  {
    return m01;
  }

  public double getShearY ()
  {
    return m10;
  }

  public double getTranslateX ()
  {
    return m02;
  }

  public double getTranslateY ()
  {
    return m12;
  }

  public void translate (double tx, double ty)
  {
    m02 += tx * m00 + ty * m01;
    m12 += tx * m10 + ty * m11;
  }

  public void rotate (double theta)
  {
    double c = Math.cos (theta);
    double s = Math.sin (theta);
    double n00 = m00 *  c + m01 * s;
    double n01 = m00 * -s + m01 * c;
    double n10 = m10 *  c + m11 * s;
    double n11 = m10 * -s + m11 * c;

    m00 = n00;
    m01 = n01;
    m10 = n10;
    m11 = n11;
  }

  public void rotate (double theta, double x, double y)
  {
    translate (x, y);
    rotate (theta);
    translate (-x, -y);
  }

  public void scale (double sx, double sy)
  {
    m00 *= sx;
    m01 *= sy;
    m10 *= sx;
    m11 *= sy;
  }

  public void shear (double shx, double shy)
  {
    double n00 = m00 + shx * m01;
    double n01 = shx * m00 + m01;
    double n10 = m10 * shy + m11;
    double n11 = shx * m10 + m11;

    m00 = n00;
    m01 = n01;
    m10 = n10;
    m11 = n11;
  }

  public void setToIdentity ()
  {
    m00 = m11 = 1;
    m01 = m02 = m10 = m12 = 0;
    type = TYPE_IDENTITY;
  }

  public void setToTranslation (double tx, double ty)
  {
    m00 = m11 = 1;
    m01 = m10 = 0;
    m02 = tx;
    m12 = ty;
    type = TYPE_TRANSLATION;
  }

  public void setToRotation (double theta)
  {
    double c = Math.cos (theta);
    double s = Math.sin (theta);

    m00 = c;
    m01 = -s;
    m02 = 0;
    m10 = s;
    m11 = c;
    m12 = 0;
    type = TYPE_GENERAL_ROTATION;
  }

  public void setToRotation (double theta, double x, double y)
  {
    double c = Math.cos (theta);
    double s = Math.sin (theta);

    m00 = c;
    m01 = -s;
    m02 = x - x * c + y * s;
    m10 = s;
    m11 = c;
    m12 = y - x * s - y * c;
    type = TYPE_GENERAL_TRANSFORM;
  }

  public void setToScale (double sx, double sy)
  {
    m00 = sx;
    m01 = m02 = m10 = m12 = 0;
    m11 = sy;
    type = (sx == sy) ? TYPE_UNIFORM_SCALE : TYPE_GENERAL_SCALE;
  }

  public void setToShear (double shx, double shy)
  {
    m00 = m11 = 1;
    m01 = shx;
    m10 = shy;
    m02 = m12 = 0;
    type = TYPE_GENERAL_TRANSFORM;
  }

  public void setTransform (AffineTransform tx)
  {
    m00 = tx.m00;
    m01 = tx.m01;
    m02 = tx.m02;
    m10 = tx.m10;
    m11 = tx.m11;
    m12 = tx.m12;
    type = tx.type;
  }

  public void setTransform (double m00, double m10, double m01,
			    double m11, double m02, double m12)
  {
    this.m00 = m00;
    this.m10 = m10;
    this.m01 = m01;
    this.m11 = m11;
    this.m02 = m02;
    this.m12 = m12;
    this.type = 0;		// FIXME
  }

  public void concatenate (AffineTransform tx)
  {
    double n00 = m00 * tx.m00 + m01 * tx.m10;
    double n01 = m00 * tx.m01 + m01 * tx.m11;
    double n02 = m00 * tx.m02 + m01 * tx.m12 + m02;
    double n10 = m10 * tx.m00 + m11 * tx.m10;
    double n11 = m10 * tx.m01 + m11 * tx.m11;
    double n12 = m10 * tx.m02 + m11 * tx.m12 + m12;

    m00 = n00;
    m01 = n01;
    m02 = n02;
    m10 = n10;
    m11 = n11;
    m12 = n12;
  }

  public void preConcatenate (AffineTransform tx)
  {
    double n00 = tx.m00 * m00 + tx.m01 * m10;
    double n01 = tx.m00 * m01 + tx.m01 * m11;
    double n02 = tx.m00 * m02 + tx.m01 * m12 + tx.m02;
    double n10 = tx.m10 * m00 + tx.m11 * m10;
    double n11 = tx.m10 * m01 + tx.m11 * m11;
    double n12 = tx.m10 * m02 + tx.m11 * m12 + tx.m12;

    m00 = n00;
    m01 = n01;
    m02 = n02;
    m10 = n10;
    m11 = n11;
    m12 = n12;
  }

  public AffineTransform createInverse ()
    throws NoninvertibleTransformException
  {
    double det = getDeterminant ();
    if (det == 0)
      throw new NoninvertibleTransformException ("can't invert transform");

    double i00 = m11 / det;
    double i01 = -m10 / det;
    double i02 = 0;
    double i10 = m01 / det;
    double i11 = -m00 / det;
    double i12 = 0;

    return new AffineTransform (i00, i01, i02,
				i10, i11, i12);
  }

  public Point2D transform (Point2D src, Point2D dst)
  {
    if (dst == null)
      dst = new Point2D.Double ();

    // We compute and set separately to correctly overwrite if
    // src==dst.
    double x = src.getX ();
    double y = src.getY ();
    double nx = m00 * x + m01 * y + m02;
    double ny = m10 * x + m11 * y + m12;

    dst.setLocation (nx, ny);

    return dst;
  }

  public void transform (Point2D[] src, int srcOff,
			 Point2D[] dst, int dstOff,
			 int num)
  {
    while (num-- > 0)
      {
	dst[dstOff] = transform (src[srcOff], dst[dstOff]);
	++srcOff;
	++dstOff;
      }
  }

  public void transform (float[] srcPts, int srcOff,
			 float[] dstPts, int dstOff,
			 int num)
  {
    while (num-- > 0)
      {
	float x = srcPts[srcOff];
	float y = srcPts[srcOff + 1];
	srcOff += 2;
	float nx = (float) (m00 * x + m01 * y + m02);
	float ny = (float) (m10 * x + m10 * y + m12);
	dstPts[dstOff] = nx;
	dstPts[dstOff + 1] = ny;
	dstOff += 2;
      }
  }

  public void transform (double[] srcPts, int srcOff,
			 double[] dstPts, int dstOff,
			 int num)
  {
    while (num-- > 0)
      {
	double x = srcPts[srcOff];
	double y = srcPts[srcOff + 1];
	srcOff += 2;
	double nx = m00 * x + m01 * y + m02;
	double ny = m10 * x + m10 * y + m12;
	dstPts[dstOff] = nx;
	dstPts[dstOff + 1] = ny;
	dstOff += 2;
      }
  }

  public void transform (float[] srcPts, int srcOff,
			 double[] dstPts, int dstOff,
			 int num)
  {
    while (num-- > 0)
      {
	float x = srcPts[srcOff];
	float y = srcPts[srcOff + 1];
	srcOff += 2;
	double nx = m00 * x + m01 * y + m02;
	double ny = m10 * x + m10 * y + m12;
	dstPts[dstOff] = nx;
	dstPts[dstOff + 1] = ny;
	dstOff += 2;
      }
  }

  public void transform (double[] srcPts, int srcOff,
			 float[] dstPts, int dstOff,
			 int num)
  {
    while (num-- > 0)
      {
	double x = srcPts[srcOff];
	double y = srcPts[srcOff + 1];
	srcOff += 2;
	float nx = (float) (m00 * x + m01 * y + m02);
	float ny = (float) (m10 * x + m10 * y + m12);
	dstPts[dstOff] = nx;
	dstPts[dstOff + 1] = ny;
	dstOff += 2;
      }
  }

  public Point2D inverseTransform (Point2D src, Point2D dst)
    throws NoninvertibleTransformException
  {
    double det = getDeterminant ();
    if (det == 0)
      throw new NoninvertibleTransformException ("couldn't invert transform");

    if (dst == null)
      dst = new Point2D.Double ();
    double x = src.getX ();
    double y = src.getY ();
    double nx = (m11 * x + - m10 * y) / det;
    double ny = (m01 * x + - m00 * y) / det;
    dst.setLocation (nx, ny);
    return dst;
  }

  public void inverseTransform (double[] srcPts, int srcOff,
				double[] dstPts, int dstOff,
				int num)
    throws NoninvertibleTransformException
  {
    double det = getDeterminant ();
    if (det == 0)
      throw new NoninvertibleTransformException ("couldn't invert transform");

    while (num-- > 0)
      {
	double x = srcPts[srcOff];
	double y = srcPts[srcOff + 1];
	double nx = (m11 * x + - m10 * y) / det;
	double ny = (m01 * x + - m00 * y) / det;
	dstPts[dstOff] = nx;
	dstPts[dstOff + 1] = ny;
	dstOff += 2;
	srcOff += 2;
      }
  }

  public Point2D deltaTransform (Point2D src, Point2D dst)
  {
    if (dst == null)
      dst = new Point2D.Double ();
    double x = src.getX ();
    double y = src.getY ();
    double nx = m00 * x + m01 * y;
    double ny = m10 * x + m11 * y;
    dst.setLocation (nx, ny);
    return dst;
  }

  public void deltaTransform (double[] srcPts, int srcOff,
			      double[] dstPts, int dstOff,
			      int num)
  {
    while (num-- > 0)
      {
	double x = srcPts[srcOff];
	double y = srcPts[srcOff + 1];
	double nx = m00 * x + m01 * y;
	double ny = m10 * x + m11 * y;
	dstPts[dstOff] = nx;
	dstPts[dstOff + 1] = ny;
	dstOff += 2;
	srcOff += 2;
      }
  }

  public Shape createTransformedShape (Shape pSrc)
  {
    // FIXME
    return null;
  }

  public String toString ()
  {
    // FIXME
    return null;
  }

  public boolean isIdentity ()
  {
    return (m00 == 1 && m01 == 0 && m02 == 0
	    && m10 == 0 && m11 == 1 && m12 == 0);
  }

  public Object clone ()
  {
    return new AffineTransform (this);
  }

  public int hashCode ()
  {
    // FIXME
    return 23;
  }

  public boolean equals (Object obj)
  {
    if (! (obj instanceof AffineTransform))
      return false;
    AffineTransform t = (AffineTransform) obj;
    return (m00 == t.m00 && m01 == t.m01 && m02 == t.m02
	    && m10 == t.m10 && m11 == t.m11 && m12 == t.m12);
  }

  // This iterator is used to apply an AffineTransform to some other
  // iterator.  It is not private because we want to be able to access
  // it from the rest of this package.
  class Iterator implements PathIterator
  {
    // The iterator we are applied to.
    private PathIterator subIterator;

    public Iterator (PathIterator subIterator)
    {
      this.subIterator = subIterator;
    }

    public int currentSegment (double[] coords)
    {
      int r = subIterator.currentSegment (coords);
      int count = 0;

      switch (r)
	{
	case SEG_CUBICTO:
	  count = 3;
	  break;

	case SEG_QUADTO:
	  count = 2;
	  break;

	case SEG_LINETO:
	case SEG_MOVETO:
	  count = 1;
	  break;

	default:
	  // Error.  But how to report?
	case SEG_CLOSE:
	  break;
	}

      transform (coords, 0, coords, 0, count);

      return r;
    }

    public int currentSegment (float[] coords)
    {
      int r = subIterator.currentSegment (coords);
      int count = 0;

      switch (r)
	{
	case SEG_CUBICTO:
	  count = 3;
	  break;

	case SEG_QUADTO:
	  count = 2;
	  break;

	case SEG_LINETO:
	case SEG_MOVETO:
	  count = 1;
	  break;

	default:
	  // Error.  But how to report?
	case SEG_CLOSE:
	  break;
	}

      transform (coords, 0, coords, 0, count);

      return r;
    }

    public int getWindingRule ()
    {
      return subIterator.getWindingRule ();
    }

    public boolean isDone ()
    {
      return subIterator.isDone ();
    }

    public void next ()
    {
      subIterator.next ();
    }
  }

  private double m00, m01, m02;
  private double m10, m11, m12;
  private int type;
}
