/* Copyright (C) 2004  Free Software Foundation

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

package java.awt.image;

import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * Filter Raster pixels by applying a matrix.
 * 
 * BandCombineOp applies a matrix to each pixel to produce new pixel values.
 * The width of the matrix must be the same or one more than the number of
 * bands in the source Raster.  If one more, the pixels in the source are
 * assumed to contain an implicit 1.0 at the end.
 * 
 * The rows of the matrix are multiplied by the pixel to produce the values
 * for the destination.  Therefore the destination Raster must contain the
 * same number of bands as the number of rows in the filter matrix.
 * 
 * @author Jerry Quinn (jlquinn@optonline.net)
 */
public class BandCombineOp implements RasterOp
{
  private RenderingHints hints;
  private float[][] matrix;
  
  /**
   * Construct a BandCombineOp.
   * 
   * @param matrix The matrix to filter pixels with.
   * @param hints Rendering hints to apply.  Ignored.
   */
  public BandCombineOp(float[][] matrix, RenderingHints hints)
  {
    this.matrix = matrix;
    this.hints = hints;
  }

  /**
   * Filter Raster pixels through a matrix.
   * 
   * Applies the Op matrix to source pixes to produce dest pixels.  Each row
   * of the matrix is multiplied by the src pixel components to produce the
   * dest pixel.  If matrix is one more than the number of bands in the src,
   * the last element is implicitly multiplied by 1, i.e. added to the sum
   * for that dest component.
   * 
   * If dest is null, a suitable Raster is created.  This implementation uses
   * createCompatibleDestRaster.  
   * 
   * @param src The source Raster.
   * @param dest  The destination Raster, or null.
   * @returns The destination Raster or an allocated Raster.
   * @see java.awt.image.RasterOp#filter(java.awt.image.Raster,
   *java.awt.image.WritableRaster)
   */
  public WritableRaster filter(Raster src, WritableRaster dest) {
    if (dest == null)
      dest = createCompatibleDestRaster(src);
    
    // Filter the pixels
    float[] spix = new float[matrix[0].length];
    float[] dpix = new float[matrix.length];
    for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
      for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
      {
        // In case matrix rows have implicit translation
        spix[spix.length - 1] = 1.0f;
        src.getPixel(x, y, spix);
        for (int i = 0; i < matrix.length; i++)
        {
          dpix[i] = 0;
          for (int j = 0; j < matrix[0].length; j++)
            dpix[i] += spix[j] * matrix[i][j];
        }
        dest.setPixel(x, y, dpix);
      }

    return dest;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getBounds2D(java.awt.image.Raster)
   */
  public Rectangle2D getBounds2D(Raster src)
  {
    return src.getBounds();
  }

  /**
   * Creates a new WritableRaster that can be used as the destination for this
   * Op.  This implementation creates a Banded Raster with data type FLOAT.
   * @see
   *java.awt.image.RasterOp#createCompatibleDestRaster(java.awt.image.Raster) 
   */
  public WritableRaster createCompatibleDestRaster(Raster src)
  {
    return Raster.createBandedRaster(DataBuffer.TYPE_FLOAT, src.getWidth(),
        src.getHeight(), matrix.length,
        new Point(src.getMinX(), src.getMinY()));
  }

  /** Return corresponding destination point for source point.
   * 
   * LookupOp will return the value of src unchanged.
   * @param src The source point.
   * @param dst The destination point.
   * @see java.awt.image.RasterOp#getPoint2D(java.awt.geom.Point2D,
   *java.awt.geom.Point2D) 
   */
  public Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null) return (Point2D)src.clone();
    dst.setLocation(src);
    return dst;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getRenderingHints()
   */
  public RenderingHints getRenderingHints()
  {
    return hints;
  }
  
  /** Return the matrix for this Op. */
  public float[][] getMatrix()
  {
    return matrix;
  }

}
