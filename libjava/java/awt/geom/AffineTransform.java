/* AffineTransform.java -- transform coordinates between two 2-D spaces
   Copyright (C) 2000, 2001, 2002, 2004 Free Software Foundation

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

import java.awt.Shape;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;

/**
 * This class represents an affine transformation between two coordinate
 * spaces in 2 dimensions. Such a transform preserves the "straightness"
 * and "parallelness" of lines. The transform is built from a sequence of
 * translations, scales, flips, rotations, and shears.
 *
 * <p>The transformation can be represented using matrix math on a 3x3 array.
 * Given (x,y), the transformation (x',y') can be found by:
 * <pre>
 * [ x']   [ m00 m01 m02 ] [ x ]   [ m00*x + m01*y + m02 ]
 * [ y'] = [ m10 m11 m12 ] [ y ] = [ m10*x + m11*y + m12 ]
 * [ 1 ]   [  0   0   1  ] [ 1 ]   [          1          ]
 * </pre>
 * The bottom row of the matrix is constant, so a transform can be uniquely
 * represented (as in {@link #toString()}) by 
 * "[[m00, m01, m02], [m10, m11, m12]]".
 *
 * @author Tom Tromey (tromey@cygnus.com)
 * @author Eric Blake (ebb9@email.byu.edu)
 * @since 1.2
 * @status partially updated to 1.4, still has some problems
 */
public class AffineTransform implements Cloneable, Serializable
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 1330973210523860834L;

  /**
   * The transformation is the identity (x' = x, y' = y). All other transforms
   * have either a combination of the appropriate transform flag bits for
   * their type, or the type GENERAL_TRANSFORM.
   *
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #getType()
   */
  public static final int TYPE_IDENTITY = 0;

  /**
   * The transformation includes a translation - shifting in the x or y
   * direction without changing length or angles.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #getType()
   */
  public static final int TYPE_TRANSLATION = 1;

  /**
   * The transformation includes a uniform scale - length is scaled in both
   * the x and y directions by the same amount, without affecting angles.
   * This is mutually exclusive with TYPE_GENERAL_SCALE.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #TYPE_MASK_SCALE
   * @see #getType()
   */
  public static final int TYPE_UNIFORM_SCALE = 2;

  /**
   * The transformation includes a general scale - length is scaled in either
   * or both the x and y directions, but by different amounts; without
   * affecting angles. This is mutually exclusive with TYPE_UNIFORM_SCALE.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #TYPE_MASK_SCALE
   * @see #getType()
   */
  public static final int TYPE_GENERAL_SCALE = 4;

  /**
   * This constant checks if either variety of scale transform is performed.
   *
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   */
  public static final int TYPE_MASK_SCALE = 6;

  /**
   * The transformation includes a flip about an axis, swapping between
   * right-handed and left-handed coordinate systems. In a right-handed
   * system, the positive x-axis rotates counter-clockwise to the positive
   * y-axis; in a left-handed system it rotates clockwise.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #getType()
   */
  public static final int TYPE_FLIP = 64;

  /**
   * The transformation includes a rotation of a multiple of 90 degrees (PI/2
   * radians). Angles are rotated, but length is preserved. This is mutually
   * exclusive with TYPE_GENERAL_ROTATION.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #TYPE_MASK_ROTATION
   * @see #getType()
   */
  public static final int TYPE_QUADRANT_ROTATION = 8;

  /**
   * The transformation includes a rotation by an arbitrary angle. Angles are
   * rotated, but length is preserved. This is mutually exclusive with
   * TYPE_QUADRANT_ROTATION.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   * @see #TYPE_MASK_ROTATION
   * @see #getType()
   */
  public static final int TYPE_GENERAL_ROTATION = 16;

  /**
   * This constant checks if either variety of rotation is performed.
   *
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   */
  public static final int TYPE_MASK_ROTATION = 24;

  /**
   * The transformation is an arbitrary conversion of coordinates which
   * could not be decomposed into the other TYPEs.
   *
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_FLIP
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #getType()
   */
  public static final int TYPE_GENERAL_TRANSFORM = 32;

  /**
   * The X coordinate scaling element of the transform matrix.
   *
   * @serial matrix[0,0]
   */
  private double m00;

  /**
   * The Y coordinate shearing element of the transform matrix.
   *
   * @serial matrix[1,0]
   */
  private double m10;

  /**
   * The X coordinate shearing element of the transform matrix.
   *
   * @serial matrix[0,1]
   */
  private double m01;

  /**
   * The Y coordinate scaling element of the transform matrix.
   *
   * @serial matrix[1,1]
   */
  private double m11;

  /**
   * The X coordinate translation element of the transform matrix.
   *
   * @serial matrix[0,2]
   */
  private double m02;

  /**
   * The Y coordinate translation element of the transform matrix.
   *
   * @serial matrix[1,2]
   */
  private double m12;

  /** The type of this transform. */
  private transient int type;

  /**
   * Construct a new identity transform:
   * <pre>
   * [ 1 0 0 ]
   * [ 0 1 0 ]
   * [ 0 0 1 ]
   * </pre>
   */
  public AffineTransform()
  {
    m00 = m11 = 1;
  }

  /**
   * Create a new transform which copies the given one.
   *
   * @param tx the transform to copy
   * @throws NullPointerException if tx is null
   */
  public AffineTransform(AffineTransform tx)
  {
    setTransform(tx);
  }

  /**
   * Construct a transform with the given matrix entries:
   * <pre>
   * [ m00 m01 m02 ]
   * [ m10 m11 m12 ]
   * [  0   0   1  ]
   * </pre>
   *
   * @param m00 the x scaling component
   * @param m10 the y shearing component
   * @param m01 the x shearing component
   * @param m11 the y scaling component
   * @param m02 the x translation component
   * @param m12 the y translation component
   */
  public AffineTransform(float m00, float m10,
                         float m01, float m11,
                         float m02, float m12)
  {
    this.m00 = m00;
    this.m10 = m10;
    this.m01 = m01;
    this.m11 = m11;
    this.m02 = m02;
    this.m12 = m12;
    updateType();
  }

  /**
   * Construct a transform from a sequence of float entries. The array must
   * have at least 4 entries, which has a translation factor of 0; or 6
   * entries, for specifying all parameters:
   * <pre>
   * [ f[0] f[2] (f[4]) ]
   * [ f[1] f[3] (f[5]) ]
   * [  0     0    1    ]
   * </pre>
   *
   * @param f the matrix to copy from, with at least 4 (6) entries
   * @throws NullPointerException if f is null
   * @throws ArrayIndexOutOfBoundsException if f is too small
   */
  public AffineTransform(float[] f)
  {
    m00 = f[0];
    m10 = f[1];
    m01 = f[2];
    m11 = f[3];
    if (f.length >= 6)
      {
        m02 = f[4];
        m12 = f[5];
      }
    updateType();
  }

  /**
   * Construct a transform with the given matrix entries:
   * <pre>
   * [ m00 m01 m02 ]
   * [ m10 m11 m12 ]
   * [  0   0   1  ]
   * </pre>
   *
   * @param m00 the x scaling component
   * @param m10 the y shearing component
   * @param m01 the x shearing component
   * @param m11 the y scaling component
   * @param m02 the x translation component
   * @param m12 the y translation component
   */
  public AffineTransform(double m00, double m10, double m01,
                         double m11, double m02, double m12)
  {
    this.m00 = m00;
    this.m10 = m10;
    this.m01 = m01;
    this.m11 = m11;
    this.m02 = m02;
    this.m12 = m12;
    updateType();
  }

  /**
   * Construct a transform from a sequence of double entries. The array must
   * have at least 4 entries, which has a translation factor of 0; or 6
   * entries, for specifying all parameters:
   * <pre>
   * [ d[0] d[2] (d[4]) ]
   * [ d[1] d[3] (d[5]) ]
   * [  0     0    1    ]
   * </pre>
   *
   * @param d the matrix to copy from, with at least 4 (6) entries
   * @throws NullPointerException if d is null
   * @throws ArrayIndexOutOfBoundsException if d is too small
   */
  public AffineTransform(double[] d)
  {
    m00 = d[0];
    m10 = d[1];
    m01 = d[2];
    m11 = d[3];
    if (d.length >= 6)
      {
        m02 = d[4];
        m12 = d[5];
      }
    updateType();
  }

  /**
   * Returns a translation transform:
   * <pre>
   * [ 1 0 tx ]
   * [ 0 1 ty ]
   * [ 0 0 1  ]
   * </pre>
   *
   * @param tx the x translation distance
   * @param ty the y translation distance
   * @return the translating transform
   */
  public static AffineTransform getTranslateInstance(double tx, double ty)
  {
    AffineTransform t = new AffineTransform();
    t.setToTranslation(tx, ty);
    return t;
  }

  /**
   * Returns a rotation transform. A positive angle (in radians) rotates
   * the positive x-axis to the positive y-axis:
   * <pre>
   * [ cos(theta) -sin(theta) 0 ]
   * [ sin(theta)  cos(theta) 0 ]
   * [     0           0      1 ]
   * </pre>
   *
   * @param theta the rotation angle
   * @return the rotating transform
   */
  public static AffineTransform getRotateInstance(double theta)
  {
    AffineTransform t = new AffineTransform();
    t.setToRotation(theta);
    return t;
  }

  /**
   * Returns a rotation transform about a point. A positive angle (in radians)
   * rotates the positive x-axis to the positive y-axis. This is the same
   * as calling:
   * <pre>
   * AffineTransform tx = new AffineTransform();
   * tx.setToTranslation(x, y);
   * tx.rotate(theta);
   * tx.translate(-x, -y);
   * </pre>
   *
   * <p>The resulting matrix is: 
   * <pre>
   * [ cos(theta) -sin(theta) x-x*cos+y*sin ]
   * [ sin(theta)  cos(theta) y-x*sin-y*cos ]
   * [     0           0            1       ]
   * </pre>
   *
   * @param theta the rotation angle
   * @param x the x coordinate of the pivot point
   * @param y the y coordinate of the pivot point
   * @return the rotating transform
   */
  public static AffineTransform getRotateInstance(double theta,
                                                  double x, double y)
  {
    AffineTransform t = new AffineTransform();
    t.setToTranslation(x, y);
    t.rotate(theta);
    t.translate(-x, -y);
    return t;
  }

  /**
   * Returns a scaling transform:
   * <pre>
   * [ sx 0  0 ]
   * [ 0  sy 0 ]
   * [ 0  0  1 ]
   * </pre>
   *
   * @param sx the x scaling factor
   * @param sy the y scaling factor
   * @return the scaling transform
   */
  public static AffineTransform getScaleInstance(double sx, double sy)
  {
    AffineTransform t = new AffineTransform();
    t.setToScale(sx, sy);
    return t;
  }

  /**
   * Returns a shearing transform (points are shifted in the x direction based
   * on a factor of their y coordinate, and in the y direction as a factor of
   * their x coordinate):
   * <pre>
   * [  1  shx 0 ]
   * [ shy  1  0 ]
   * [  0   0  1 ]
   * </pre>
   *
   * @param shx the x shearing factor
   * @param shy the y shearing factor
   * @return the shearing transform
   */
  public static AffineTransform getShearInstance(double shx, double shy)
  {
    AffineTransform t = new AffineTransform();
    t.setToShear(shx, shy);
    return t;
  }

  /**
   * Returns the type of this transform. The result is always valid, although
   * it may not be the simplest interpretation (in other words, there are
   * sequences of transforms which reduce to something simpler, which this
   * does not always detect). The result is either TYPE_GENERAL_TRANSFORM,
   * or a bit-wise combination of TYPE_TRANSLATION, the mutually exclusive
   * TYPE_*_ROTATIONs, and the mutually exclusive TYPE_*_SCALEs.
   *
   * @return The type.
   * 
   * @see #TYPE_IDENTITY
   * @see #TYPE_TRANSLATION
   * @see #TYPE_UNIFORM_SCALE
   * @see #TYPE_GENERAL_SCALE
   * @see #TYPE_QUADRANT_ROTATION
   * @see #TYPE_GENERAL_ROTATION
   * @see #TYPE_GENERAL_TRANSFORM
   */
  public int getType()
  {
    return type;
  }

  /**
   * Return the determinant of this transform matrix. If the determinant is
   * non-zero, the transform is invertible; otherwise operations which require
   * an inverse throw a NoninvertibleTransformException. A result very near
   * zero, due to rounding errors, may indicate that inversion results do not
   * carry enough precision to be meaningful.
   *
   * <p>If this is a uniform scale transformation, the determinant also
   * represents the squared value of the scale. Otherwise, it carries little
   * additional meaning. The determinant is calculated as:
   * <pre>
   * | m00 m01 m02 |
   * | m10 m11 m12 | = m00 * m11 - m01 * m10
   * |  0   0   1  |
   * </pre>
   *
   * @return the determinant
   * @see #createInverse()
   */
  public double getDeterminant()
  {
    return m00 * m11 - m01 * m10;
  }

  /**
   * Return the matrix of values used in this transform. If the matrix has
   * fewer than 6 entries, only the scale and shear factors are returned;
   * otherwise the translation factors are copied as well. The resulting
   * values are:
   * <pre>
   * [ d[0] d[2] (d[4]) ]
   * [ d[1] d[3] (d[5]) ]
   * [  0     0    1    ]
   * </pre>
   *
   * @param d the matrix to store the results into; with 4 (6) entries
   * @throws NullPointerException if d is null
   * @throws ArrayIndexOutOfBoundsException if d is too small
   */
  public void getMatrix(double[] d)
  {
    d[0] = m00;
    d[1] = m10;
    d[2] = m01;
    d[3] = m11;
    if (d.length >= 6)
      {
        d[4] = m02;
        d[5] = m12;
      }
  }

  /**
   * Returns the X coordinate scaling factor of the matrix.
   *
   * @return m00
   * @see #getMatrix(double[])
   */
  public double getScaleX()
  {
    return m00;
  }

  /**
   * Returns the Y coordinate scaling factor of the matrix.
   *
   * @return m11
   * @see #getMatrix(double[])
   */
  public double getScaleY()
  {
    return m11;
  }

  /**
   * Returns the X coordinate shearing factor of the matrix.
   *
   * @return m01
   * @see #getMatrix(double[])
   */
  public double getShearX()
  {
    return m01;
  }

  /**
   * Returns the Y coordinate shearing factor of the matrix.
   *
   * @return m10
   * @see #getMatrix(double[])
   */
  public double getShearY()
  {
    return m10;
  }

  /**
   * Returns the X coordinate translation factor of the matrix.
   *
   * @return m02
   * @see #getMatrix(double[])
   */
  public double getTranslateX()
  {
    return m02;
  }

  /**
   * Returns the Y coordinate translation factor of the matrix.
   *
   * @return m12
   * @see #getMatrix(double[])
   */
  public double getTranslateY()
  {
    return m12;
  }

  /**
   * Concatenate a translation onto this transform. This is equivalent, but
   * more efficient than
   * <code>concatenate(AffineTransform.getTranslateInstance(tx, ty))</code>.
   *
   * @param tx the x translation distance
   * @param ty the y translation distance
   * @see #getTranslateInstance(double, double)
   * @see #concatenate(AffineTransform)
   */
  public void translate(double tx, double ty)
  {
    m02 += tx * m00 + ty * m01;
    m12 += tx * m10 + ty * m11;
    updateType();
  }

  /**
   * Concatenate a rotation onto this transform. This is equivalent, but
   * more efficient than
   * <code>concatenate(AffineTransform.getRotateInstance(theta))</code>.
   *
   * @param theta the rotation angle
   * @see #getRotateInstance(double)
   * @see #concatenate(AffineTransform)
   */
  public void rotate(double theta)
  {
    double c = Math.cos(theta);
    double s = Math.sin(theta);
    double n00 = m00 *  c + m01 * s;
    double n01 = m00 * -s + m01 * c;
    double n10 = m10 *  c + m11 * s;
    double n11 = m10 * -s + m11 * c;
    m00 = n00;
    m01 = n01;
    m10 = n10;
    m11 = n11;
    updateType();
  }

  /**
   * Concatenate a rotation about a point onto this transform. This is
   * equivalent, but more efficient than
   * <code>concatenate(AffineTransform.getRotateInstance(theta, x, y))</code>.
   *
   * @param theta the rotation angle
   * @param x the x coordinate of the pivot point
   * @param y the y coordinate of the pivot point
   * @see #getRotateInstance(double, double, double)
   * @see #concatenate(AffineTransform)
   */
  public void rotate(double theta, double x, double y)
  {
    translate(x, y);
    rotate(theta);
    translate(-x, -y);
  }

  /**
   * Concatenate a scale onto this transform. This is equivalent, but more
   * efficient than
   * <code>concatenate(AffineTransform.getScaleInstance(sx, sy))</code>.
   *
   * @param sx the x scaling factor
   * @param sy the y scaling factor
   * @see #getScaleInstance(double, double)
   * @see #concatenate(AffineTransform)
   */
  public void scale(double sx, double sy)
  {
    m00 *= sx;
    m01 *= sy;
    m10 *= sx;
    m11 *= sy;
    updateType();
  }

  /**
   * Concatenate a shearing onto this transform. This is equivalent, but more
   * efficient than
   * <code>concatenate(AffineTransform.getShearInstance(sx, sy))</code>.
   *
   * @param shx the x shearing factor
   * @param shy the y shearing factor
   * @see #getShearInstance(double, double)
   * @see #concatenate(AffineTransform)
   */
  public void shear(double shx, double shy)
  {
    double n00 = m00 + (shy * m01);
    double n01 = m01 + (shx * m00);
    double n10 = m10 + (shy * m11);
    double n11 = m11 + (shx * m10);
    m00 = n00;
    m01 = n01;
    m10 = n10;
    m11 = n11;
    updateType();
  }

  /**
   * Reset this transform to the identity (no transformation):
   * <pre>
   * [ 1 0 0 ]
   * [ 0 1 0 ]
   * [ 0 0 1 ]
   * </pre>
   */
  public void setToIdentity()
  {
    m00 = m11 = 1;
    m01 = m02 = m10 = m12 = 0;
    type = TYPE_IDENTITY;
  }

  /**
   * Set this transform to a translation:
   * <pre>
   * [ 1 0 tx ]
   * [ 0 1 ty ]
   * [ 0 0 1  ]
   * </pre>
   *
   * @param tx the x translation distance
   * @param ty the y translation distance
   */
  public void setToTranslation(double tx, double ty)
  {
    m00 = m11 = 1;
    m01 = m10 = 0;
    m02 = tx;
    m12 = ty;
    type = (tx == 0 && ty == 0) ? TYPE_UNIFORM_SCALE : TYPE_TRANSLATION;
  }

  /**
   * Set this transform to a rotation. A positive angle (in radians) rotates
   * the positive x-axis to the positive y-axis:
   * <pre>
   * [ cos(theta) -sin(theta) 0 ]
   * [ sin(theta)  cos(theta) 0 ]
   * [     0           0      1 ]
   * </pre>
   *
   * @param theta the rotation angle
   */
  public void setToRotation(double theta)
  {
    double c = Math.cos(theta);
    double s = Math.sin(theta);
    m00 = c;
    m01 = -s;
    m02 = 0;
    m10 = s;
    m11 = c;
    m12 = 0;
    type = (c == 1 ? TYPE_IDENTITY
            : c == 0 || c == -1 ? TYPE_QUADRANT_ROTATION
            : TYPE_GENERAL_ROTATION);
  }

  /**
   * Set this transform to a rotation about a point. A positive angle (in
   * radians) rotates the positive x-axis to the positive y-axis. This is the
   * same as calling:
   * <pre>
   * tx.setToTranslation(x, y);
   * tx.rotate(theta);
   * tx.translate(-x, -y);
   * </pre>
   *
   * <p>The resulting matrix is: 
   * <pre>
   * [ cos(theta) -sin(theta) x-x*cos+y*sin ]
   * [ sin(theta)  cos(theta) y-x*sin-y*cos ]
   * [     0           0            1       ]
   * </pre>
   *
   * @param theta the rotation angle
   * @param x the x coordinate of the pivot point
   * @param y the y coordinate of the pivot point
   */
  public void setToRotation(double theta, double x, double y)
  {
    double c = Math.cos(theta);
    double s = Math.sin(theta);
    m00 = c;
    m01 = -s;
    m02 = x - x * c + y * s;
    m10 = s;
    m11 = c;
    m12 = y - x * s - y * c;
    updateType();
  }

  /**
   * Set this transform to a scale:
   * <pre>
   * [ sx 0  0 ]
   * [ 0  sy 0 ]
   * [ 0  0  1 ]
   * </pre>
   *
   * @param sx the x scaling factor
   * @param sy the y scaling factor
   */
  public void setToScale(double sx, double sy)
  {
    m00 = sx;
    m01 = m02 = m10 = m12 = 0;
    m11 = sy;
    type = (sx != sy ? TYPE_GENERAL_SCALE
            : sx == 1 ? TYPE_IDENTITY : TYPE_UNIFORM_SCALE);
  }

  /**
   * Set this transform to a shear (points are shifted in the x direction based
   * on a factor of their y coordinate, and in the y direction as a factor of
   * their x coordinate):
   * <pre>
   * [  1  shx 0 ]
   * [ shy  1  0 ]
   * [  0   0  1 ]
   * </pre>
   *
   * @param shx the x shearing factor
   * @param shy the y shearing factor
   */
  public void setToShear(double shx, double shy)
  {
    m00 = m11 = 1;
    m01 = shx;
    m10 = shy;
    m02 = m12 = 0;
    updateType();
  }

  /**
   * Set this transform to a copy of the given one.
   *
   * @param tx the transform to copy
   * @throws NullPointerException if tx is null
   */
  public void setTransform(AffineTransform tx)
  {
    m00 = tx.m00;
    m01 = tx.m01;
    m02 = tx.m02;
    m10 = tx.m10;
    m11 = tx.m11;
    m12 = tx.m12;
    type = tx.type;
  }

  /**
   * Set this transform to the given values:
   * <pre>
   * [ m00 m01 m02 ]
   * [ m10 m11 m12 ]
   * [  0   0   1  ]
   * </pre>
   *
   * @param m00 the x scaling component
   * @param m10 the y shearing component
   * @param m01 the x shearing component
   * @param m11 the y scaling component
   * @param m02 the x translation component
   * @param m12 the y translation component
   */
  public void setTransform(double m00, double m10, double m01,
                           double m11, double m02, double m12)
  {
    this.m00 = m00;
    this.m10 = m10;
    this.m01 = m01;
    this.m11 = m11;
    this.m02 = m02;
    this.m12 = m12;
    updateType();
  }

  /**
   * Set this transform to the result of performing the original version of
   * this followed by tx. This is commonly used when chaining transformations
   * from one space to another. In matrix form:
   * <pre>
   * [ this ] = [ this ] x [ tx ]
   * </pre>
   *
   * @param tx the transform to concatenate
   * @throws NullPointerException if tx is null
   * @see #preConcatenate(AffineTransform)
   */
  public void concatenate(AffineTransform tx)
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
    updateType();
  }

  /**
   * Set this transform to the result of performing tx followed by the
   * original version of this. This is less common than normal concatenation,
   * but can still be used to chain transformations from one space to another.
   * In matrix form:
   * <pre>
   * [ this ] = [ tx ] x [ this ]
   * </pre>
   *
   * @param tx the transform to concatenate
   * @throws NullPointerException if tx is null
   * @see #concatenate(AffineTransform)
   */
  public void preConcatenate(AffineTransform tx)
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
    updateType();
  }

  /**
   * Returns a transform, which if concatenated to this one, will result in
   * the identity transform. This is useful for undoing transformations, but
   * is only possible if the original transform has an inverse (ie. does not
   * map multiple points to the same line or point). A transform exists only
   * if getDeterminant() has a non-zero value.
   *
   * The inverse is calculated as:
   * 
   * <pre>
   *
   * Let A be the matrix for which we want to find the inverse:
   *
   * A = [ m00 m01 m02 ]
   *     [ m10 m11 m12 ]
   *     [ 0   0   1   ] 
   *
   *
   *                 1    
   * inverse (A) =  ---   x  adjoint(A) 
   *                det 
   *
   *
   *
   *             =   1       [  m11  -m01   m01*m12-m02*m11  ]
   *                ---   x  [ -m10   m00  -m00*m12+m10*m02  ]
   *                det      [  0     0     m00*m11-m10*m01  ]
   *
   *
   *
   *             = [  m11/det  -m01/det   m01*m12-m02*m11/det ]
   *               [ -m10/det   m00/det  -m00*m12+m10*m02/det ]
   *               [   0           0          1               ]
   *
   *
   * </pre>
   *
   *
   *
   * @return a new inverse transform
   * @throws NoninvertibleTransformException if inversion is not possible
   * @see #getDeterminant()
   */
  public AffineTransform createInverse()
    throws NoninvertibleTransformException
  {
    double det = getDeterminant();
    if (det == 0)
      throw new NoninvertibleTransformException("can't invert transform");
    
    double im00 = m11 / det;
    double im10 = -m10 / det;
    double im01 = -m01 / det;
    double im11 = m00 / det;
    double im02 = (m01 * m12 - m02 * m11) / det;
    double im12 = (-m00 * m12 + m10 * m02) / det;
    
    return new AffineTransform (im00, im10, im01, im11, im02, im12);
  }

  /**
   * Perform this transformation on the given source point, and store the
   * result in the destination (creating it if necessary). It is safe for
   * src and dst to be the same.
   *
   * @param src the source point
   * @param dst the destination, or null
   * @return the transformation of src, in dst if it was non-null
   * @throws NullPointerException if src is null
   */
  public Point2D transform(Point2D src, Point2D dst)
  {
    if (dst == null)
      dst = new Point2D.Double();
    double x = src.getX();
    double y = src.getY();
    double nx = m00 * x + m01 * y + m02;
    double ny = m10 * x + m11 * y + m12;
    dst.setLocation(nx, ny);
    return dst;
  }

  /**
   * Perform this transformation on an array of points, storing the results
   * in another (possibly same) array. This will not create a destination
   * array, but will create points for the null entries of the destination.
   * The transformation is done sequentially. While having a single source
   * and destination point be the same is safe, you should be aware that
   * duplicate references to the same point in the source, and having the
   * source overlap the destination, may result in your source points changing
   * from a previous transform before it is their turn to be evaluated.
   *
   * @param src the array of source points
   * @param srcOff the starting offset into src
   * @param dst the array of destination points (may have null entries)
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null, or src has null
   *         entries
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   * @throws ArrayStoreException if new points are incompatible with dst
   */
  public void transform(Point2D[] src, int srcOff,
                        Point2D[] dst, int dstOff, int num)
  {
    while (--num >= 0)
      dst[dstOff] = transform(src[srcOff++], dst[dstOff++]);
  }

  /**
   * Perform this transformation on an array of points, in (x,y) pairs,
   * storing the results in another (possibly same) array. This will not
   * create a destination array. All sources are copied before the
   * transformation, so that no result will overwrite a point that has not yet
   * been evaluated.
   *
   * @param srcPts the array of source points
   * @param srcOff the starting offset into src
   * @param dstPts the array of destination points
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   */
  public void transform(float[] srcPts, int srcOff,
                        float[] dstPts, int dstOff, int num)
  {
    if (srcPts == dstPts && dstOff > srcOff
        && num > 1 && srcOff + 2 * num > dstOff)
      {
        float[] f = new float[2 * num];
        System.arraycopy(srcPts, srcOff, f, 0, 2 * num);
        srcPts = f;
      }
    while (--num >= 0)
      {
        float x = srcPts[srcOff++];
        float y = srcPts[srcOff++];
        dstPts[dstOff++] = (float) (m00 * x + m01 * y + m02);
        dstPts[dstOff++] = (float) (m10 * x + m11 * y + m12);
      }
  }

  /**
   * Perform this transformation on an array of points, in (x,y) pairs,
   * storing the results in another (possibly same) array. This will not
   * create a destination array. All sources are copied before the
   * transformation, so that no result will overwrite a point that has not yet
   * been evaluated.
   *
   * @param srcPts the array of source points
   * @param srcOff the starting offset into src
   * @param dstPts the array of destination points
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   */
  public void transform(double[] srcPts, int srcOff,
                        double[] dstPts, int dstOff, int num)
  {
    if (srcPts == dstPts && dstOff > srcOff
        && num > 1 && srcOff + 2 * num > dstOff)
      {
        double[] d = new double[2 * num];
        System.arraycopy(srcPts, srcOff, d, 0, 2 * num);
        srcPts = d;
      }
    while (--num >= 0)
      {
        double x = srcPts[srcOff++];
        double y = srcPts[srcOff++];
        dstPts[dstOff++] = m00 * x + m01 * y + m02;
        dstPts[dstOff++] = m10 * x + m11 * y + m12;
      }
  }

  /**
   * Perform this transformation on an array of points, in (x,y) pairs,
   * storing the results in another array. This will not create a destination
   * array.
   *
   * @param srcPts the array of source points
   * @param srcOff the starting offset into src
   * @param dstPts the array of destination points
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   */
  public void transform(float[] srcPts, int srcOff,
                        double[] dstPts, int dstOff, int num)
  {
    while (--num >= 0)
      {
        float x = srcPts[srcOff++];
        float y = srcPts[srcOff++];
        dstPts[dstOff++] = m00 * x + m01 * y + m02;
        dstPts[dstOff++] = m10 * x + m11 * y + m12;
      }
  }

  /**
   * Perform this transformation on an array of points, in (x,y) pairs,
   * storing the results in another array. This will not create a destination
   * array.
   *
   * @param srcPts the array of source points
   * @param srcOff the starting offset into src
   * @param dstPts the array of destination points
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   */
  public void transform(double[] srcPts, int srcOff,
                        float[] dstPts, int dstOff, int num)
  {
    while (--num >= 0)
      {
        double x = srcPts[srcOff++];
        double y = srcPts[srcOff++];
        dstPts[dstOff++] = (float) (m00 * x + m01 * y + m02);
        dstPts[dstOff++] = (float) (m10 * x + m11 * y + m12);
      }
  }

  /**
   * Perform the inverse of this transformation on the given source point,
   * and store the result in the destination (creating it if necessary). It
   * is safe for src and dst to be the same.
   *
   * @param src the source point
   * @param dst the destination, or null
   * @return the inverse transformation of src, in dst if it was non-null
   * @throws NullPointerException if src is null
   * @throws NoninvertibleTransformException if the inverse does not exist
   * @see #getDeterminant()
   */
  public Point2D inverseTransform(Point2D src, Point2D dst)
    throws NoninvertibleTransformException
  {
    return createInverse().transform(src, dst);
  }

  /**
   * Perform the inverse of this transformation on an array of points, in
   * (x,y) pairs, storing the results in another (possibly same) array. This
   * will not create a destination array. All sources are copied before the
   * transformation, so that no result will overwrite a point that has not yet
   * been evaluated.
   *
   * @param srcPts the array of source points
   * @param srcOff the starting offset into src
   * @param dstPts the array of destination points
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   * @throws NoninvertibleTransformException if the inverse does not exist
   * @see #getDeterminant()
   */
  public void inverseTransform(double[] srcPts, int srcOff,
                               double[] dstPts, int dstOff, int num)
    throws NoninvertibleTransformException
  {
    createInverse().transform(srcPts, srcOff, dstPts, dstOff, num);
  }

  /**
   * Perform this transformation, less any translation, on the given source
   * point, and store the result in the destination (creating it if
   * necessary). It is safe for src and dst to be the same. The reduced
   * transform is equivalent to:
   * <pre>
   * [ x' ] = [ m00 m01 ] [ x ] = [ m00 * x + m01 * y ]
   * [ y' ]   [ m10 m11 ] [ y ] = [ m10 * x + m11 * y ]
   * </pre>
   *
   * @param src the source point
   * @param dst the destination, or null
   * @return the delta transformation of src, in dst if it was non-null
   * @throws NullPointerException if src is null
   */
  public Point2D deltaTransform(Point2D src, Point2D dst)
  {
    if (dst == null)
      dst = new Point2D.Double();
    double x = src.getX();
    double y = src.getY();
    double nx = m00 * x + m01 * y;
    double ny = m10 * x + m11 * y;
    dst.setLocation(nx, ny);
    return dst;
  }

  /**
   * Perform this transformation, less any translation, on an array of points,
   * in (x,y) pairs, storing the results in another (possibly same) array.
   * This will not create a destination array. All sources are copied before
   * the transformation, so that no result will overwrite a point that has
   * not yet been evaluated. The reduced transform is equivalent to:
   * <pre>
   * [ x' ] = [ m00 m01 ] [ x ] = [ m00 * x + m01 * y ]
   * [ y' ]   [ m10 m11 ] [ y ] = [ m10 * x + m11 * y ]
   * </pre>
   *
   * @param srcPts the array of source points
   * @param srcOff the starting offset into src
   * @param dstPts the array of destination points
   * @param dstOff the starting offset into dst
   * @param num the number of points to transform
   * @throws NullPointerException if src or dst is null
   * @throws ArrayIndexOutOfBoundsException if array bounds are exceeded
   */
  public void deltaTransform(double[] srcPts, int srcOff,
                              double[] dstPts, int dstOff,
                              int num)
  {
    if (srcPts == dstPts && dstOff > srcOff
        && num > 1 && srcOff + 2 * num > dstOff)
      {
        double[] d = new double[2 * num];
        System.arraycopy(srcPts, srcOff, d, 0, 2 * num);
        srcPts = d;
      }
    while (--num >= 0)
      {
        double x = srcPts[srcOff++];
        double y = srcPts[srcOff++];
        dstPts[dstOff++] = m00 * x + m01 * y;
        dstPts[dstOff++] = m10 * x + m11 * y;
      }
  }

  /**
   * Return a new Shape, based on the given one, where the path of the shape
   * has been transformed by this transform. Notice that this uses GeneralPath,
   * which only stores points in float precision.
   *
   * @param src the shape source to transform
   * @return the shape, transformed by this, <code>null</code> if src is 
   * <code>null</code>.
   * @see GeneralPath#transform(AffineTransform)
   */
  public Shape createTransformedShape(Shape src)
  {
    if(src == null) 
      return null;
    GeneralPath p = new GeneralPath(src);
    p.transform(this);
    return p;
  }

  /**
   * Returns a string representation of the transform, in the format:
   * <code>"AffineTransform[[" + m00 + ", " + m01 + ", " + m02 + "], ["
   *   + m10 + ", " + m11 + ", " + m12 + "]]"</code>.
   *
   * @return the string representation
   */
  public String toString()
  {
    return "AffineTransform[[" + m00 + ", " + m01 + ", " + m02 + "], ["
      + m10 + ", " + m11 + ", " + m12 + "]]";
  }

  /**
   * Tests if this transformation is the identity:
   * <pre>
   * [ 1 0 0 ]
   * [ 0 1 0 ]
   * [ 0 0 1 ]
   * </pre>
   *
   * @return true if this is the identity transform
   */
  public boolean isIdentity()
  {
    // Rather than rely on type, check explicitly.
    return (m00 == 1 && m01 == 0 && m02 == 0
            && m10 == 0 && m11 == 1 && m12 == 0);
  }

  /**
   * Create a new transform of the same run-time type, with the same
   * transforming properties as this one.
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
   * Return the hashcode for this transformation. The formula is not
   * documented, but appears to be the same as:
   * <pre>
   * long l = Double.doubleToLongBits(getScaleX());
   * l = l * 31 + Double.doubleToLongBits(getShearY());
   * l = l * 31 + Double.doubleToLongBits(getShearX());
   * l = l * 31 + Double.doubleToLongBits(getScaleY());
   * l = l * 31 + Double.doubleToLongBits(getTranslateX());
   * l = l * 31 + Double.doubleToLongBits(getTranslateY());
   * return (int) ((l >> 32) ^ l);
   * </pre>
   *
   * @return the hashcode
   */
  public int hashCode()
  {
    long l = Double.doubleToLongBits(m00);
    l = l * 31 + Double.doubleToLongBits(m10);
    l = l * 31 + Double.doubleToLongBits(m01);
    l = l * 31 + Double.doubleToLongBits(m11);
    l = l * 31 + Double.doubleToLongBits(m02);
    l = l * 31 + Double.doubleToLongBits(m12);
    return (int) ((l >> 32) ^ l);
  }

  /**
   * Compares two transforms for equality. This returns true if they have the
   * same matrix values.
   *
   * @param obj the transform to compare
   * @return true if it is equal
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof AffineTransform))
      return false;
    AffineTransform t = (AffineTransform) obj;
    return (m00 == t.m00 && m01 == t.m01 && m02 == t.m02
            && m10 == t.m10 && m11 == t.m11 && m12 == t.m12);
  }

  /**
   * Helper to decode the type from the matrix. This is not guaranteed
   * to find the optimal type, but at least it will be valid.
   */
  private void updateType()
  {
    double det = getDeterminant();
    if (det == 0)
      {
        type = TYPE_GENERAL_TRANSFORM;
        return;
      }
    // Scale (includes rotation by PI) or translation.
    if (m01 == 0 && m10 == 0)
      {
        if (m00 == m11)
          type = m00 == 1 ? TYPE_IDENTITY : TYPE_UNIFORM_SCALE;
        else
          type = TYPE_GENERAL_SCALE;
        if (m02 != 0 || m12 != 0)
          type |= TYPE_TRANSLATION;
      }
    // Rotation.
    else if (m00 == m11 && m01 == -m10)
      {
        type = m00 == 0 ? TYPE_QUADRANT_ROTATION : TYPE_GENERAL_ROTATION;
        if (det != 1)
          type |= TYPE_UNIFORM_SCALE;
        if (m02 != 0 || m12 != 0)
          type |= TYPE_TRANSLATION;
      }
    else
      type = TYPE_GENERAL_TRANSFORM;
  }

  /**
   * Reads a transform from an object stream.
   *
   * @param s the stream to read from
   * @throws ClassNotFoundException if there is a problem deserializing
   * @throws IOException if there is a problem deserializing
   */
  private void readObject(ObjectInputStream s)
    throws ClassNotFoundException, IOException
  {
    s.defaultReadObject();
    updateType();
  }
} // class AffineTransform
