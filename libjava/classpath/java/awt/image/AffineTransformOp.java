/* AffineTransformOp.java --  This class performs affine 
   transformation between two images or rasters in 2 dimensions.
   Copyright (C) 2004, 2006 Free Software Foundation

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

package java.awt.image;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Arrays;

/**
 * AffineTransformOp performs matrix-based transformations (translations,
 * scales, flips, rotations, and shears).
 * 
 * If interpolation is required, nearest neighbour, bilinear, and bicubic
 * methods are available.
 *
 * @author Olga Rodimina (rodimina@redhat.com) 
 * @author Francis Kung (fkung@redhat.com)
 */
public class AffineTransformOp implements BufferedImageOp, RasterOp
{
    public static final int TYPE_NEAREST_NEIGHBOR = 1;
    
    public static final int TYPE_BILINEAR = 2;
    
    /**
     * @since 1.5.0
     */
    public static final int TYPE_BICUBIC = 3;

    private AffineTransform transform;
    private RenderingHints hints;
    
    /**
     * Construct AffineTransformOp with the given xform and interpolationType.
     * Interpolation type can be TYPE_BILINEAR, TYPE_BICUBIC or
     * TYPE_NEAREST_NEIGHBOR.
     *
     * @param xform AffineTransform that will applied to the source image 
     * @param interpolationType type of interpolation used
     * @throws ImagingOpException if the transform matrix is noninvertible
     */
    public AffineTransformOp (AffineTransform xform, int interpolationType)
    {
      this.transform = xform;
      if (xform.getDeterminant() == 0)
        throw new ImagingOpException(null);

      switch (interpolationType)
      {
      case TYPE_BILINEAR:
        hints = new RenderingHints (RenderingHints.KEY_INTERPOLATION, 
                                    RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        break;
      case TYPE_BICUBIC:
        hints = new RenderingHints (RenderingHints.KEY_INTERPOLATION, 
				    RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        break;
      default:
        hints = new RenderingHints (RenderingHints.KEY_INTERPOLATION,
                                    RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR);
      }
    }

    /**
     * Construct AffineTransformOp with the given xform and rendering hints.
     * 
     * @param xform AffineTransform that will applied to the source image
     * @param hints rendering hints that will be used during transformation
     * @throws ImagingOpException if the transform matrix is noninvertible
     */
    public AffineTransformOp (AffineTransform xform, RenderingHints hints)
    {
      this.transform = xform;
      this.hints = hints;
      if (xform.getDeterminant() == 0)
        throw new ImagingOpException(null);
    }

    /**
     * Creates a new BufferedImage with the size equal to that of the 
     * transformed image and the correct number of bands. The newly created 
     * image is created with the specified ColorModel. 
     * If a ColorModel is not specified, an appropriate ColorModel is used.
     *
     * @param src the source image.
     * @param destCM color model for the destination image (can be null).
     * @return a new compatible destination image.
     */
    public BufferedImage createCompatibleDestImage (BufferedImage src,
                                                    ColorModel destCM)
    {
      if (destCM != null)
        return new BufferedImage(destCM,
                                 createCompatibleDestRaster(src.getRaster()),
                                 src.isAlphaPremultiplied(), null);

      // This behaviour was determined by Mauve testcases, and is compatible
      // with the reference implementation
      if (src.getType() == BufferedImage.TYPE_INT_ARGB_PRE
          || src.getType() == BufferedImage.TYPE_4BYTE_ABGR
          || src.getType() == BufferedImage.TYPE_4BYTE_ABGR_PRE)
        return new BufferedImage(src.getWidth(), src.getHeight(), src.getType());

      else
        return new BufferedImage(src.getWidth(), src.getHeight(),
                                 BufferedImage.TYPE_INT_ARGB);
    }

    /**
     * Creates a new WritableRaster with the size equal to the transformed 
     * source raster and correct number of bands .
     *
     * @param src the source raster.
     * @throws RasterFormatException if resulting width or height of raster is 0.
     * @return a new compatible raster.
     */
    public WritableRaster createCompatibleDestRaster (Raster src)
    {
      Rectangle2D rect = getBounds2D(src);
      
      if (rect.getWidth() == 0 || rect.getHeight() == 0) 
        throw new RasterFormatException("width or height is 0");

      return src.createCompatibleWritableRaster((int) rect.getWidth(), 
                                                (int) rect.getHeight());
    }

    /**
     * Transforms source image using transform specified at the constructor.
     * The resulting transformed image is stored in the destination image if one
     * is provided; otherwise a new BufferedImage is created and returned. 
     *
     * @param src source image
     * @param dst destination image
     * @throws IllegalArgumentException if the source and destination image are
     *          the same
     * @return transformed source image.
     */
    public final BufferedImage filter (BufferedImage src, BufferedImage dst)
    {
      if (dst == src)
        throw new IllegalArgumentException("src image cannot be the same as "
                                         + "the dst image");

      // If the destination image is null, then use a compatible BufferedImage
      if (dst == null)
        dst = createCompatibleDestImage(src, null);

      Graphics2D gr = (Graphics2D) dst.createGraphics();
      gr.setRenderingHints(hints);
      gr.drawImage(src, transform, null);
      return dst;
    }

    /**
     * Transforms source raster using transform specified at the constructor.
     * The resulting raster is stored in the destination raster if it is not
     * null, otherwise a new raster is created and returned.
     *
     * @param src source raster
     * @param dst destination raster
     * @throws IllegalArgumentException if the source and destination are not
     *          compatible
     * @return transformed raster.
     */
    public final WritableRaster filter(Raster src, WritableRaster dst)
    {
      // Initial checks
      if (dst == src)
        throw new IllegalArgumentException("src image cannot be the same as"
                                           + " the dst image");

      if (dst == null)
        dst = createCompatibleDestRaster(src);

      if (src.getNumBands() != dst.getNumBands())
        throw new IllegalArgumentException("src and dst must have same number"
                                           + " of bands");
      
      // Optimization for rasters that can be represented in the RGB colormodel:
      // wrap the rasters in images, and let Cairo do the transformation
      if (ColorModel.getRGBdefault().isCompatibleSampleModel(src.getSampleModel())
          && ColorModel.getRGBdefault().isCompatibleSampleModel(dst.getSampleModel()))
        {
          WritableRaster src2 = Raster.createWritableRaster(src.getSampleModel(),
                                                            src.getDataBuffer(),
                                                            new Point(src.getMinX(),
                                                                      src.getMinY()));
          BufferedImage iSrc = new BufferedImage(ColorModel.getRGBdefault(),
                                                 src2, false, null);
          BufferedImage iDst = new BufferedImage(ColorModel.getRGBdefault(), dst,
                                                 false, null);
  
          return filter(iSrc, iDst).getRaster();
        }

      // Otherwise, we need to do the transformation in java code...
      // Create arrays to hold all the points
      double[] dstPts = new double[dst.getHeight() * dst.getWidth() * 2];
      double[] srcPts = new double[dst.getHeight() * dst.getWidth() * 2];

      // Populate array with all points in the *destination* raster
      int i = 0;
      for (int x = 0; x < dst.getWidth(); x++)
        {
          for (int y = 0; y < dst.getHeight(); y++)
            {
              dstPts[i++] = x;
              dstPts[i++] = y;
            }
        }
      Rectangle srcbounds = src.getBounds();

      // Use an inverse transform to map each point in the destination to
      // a point in the source.  Note that, while all points in the destination
      // matrix are integers, this is not necessarily true for points in the
      // source (hence why interpolation is required) 
      try
        {
          AffineTransform inverseTx = transform.createInverse();
          inverseTx.transform(dstPts, 0, srcPts, 0, dstPts.length / 2);
        }
      catch (NoninvertibleTransformException e)
        {
          // Shouldn't happen since the constructor traps this
          throw new ImagingOpException(e.getMessage());
        }

      // Different interpolation methods...
      if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR))
        filterNearest(src, dst, dstPts, srcPts);
      
      else if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR))
        filterBilinear(src, dst, dstPts, srcPts);
    
      else          // bicubic
        filterBicubic(src, dst, dstPts, srcPts);

      return dst;  
    }

    /**
     * Transforms source image using transform specified at the constructor and 
     * returns bounds of the transformed image.
     *
     * @param src image to be transformed
     * @return bounds of the transformed image.
     */
    public final Rectangle2D getBounds2D (BufferedImage src)
    {
      return getBounds2D (src.getRaster());
    }
   
    /**
     * Returns bounds of the transformed raster.
     *
     * @param src raster to be transformed
     * @return bounds of the transformed raster.
     */
    public final Rectangle2D getBounds2D (Raster src)
    {
      return transform.createTransformedShape(src.getBounds()).getBounds2D();
    }

    /**
     * Returns interpolation type used during transformations.
     *
     * @return interpolation type
     */
    public final int getInterpolationType ()
    {
      if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_BILINEAR))
        return TYPE_BILINEAR;
      
      else if (hints.containsValue(RenderingHints.VALUE_INTERPOLATION_BICUBIC))
        return TYPE_BICUBIC;
      
      else 
        return TYPE_NEAREST_NEIGHBOR;
    }

    /** 
     * Returns location of the transformed source point. The resulting point 
     * is stored in the dstPt if one is specified.
     *  
     * @param srcPt point to be transformed
     * @param dstPt destination point
     * @return the location of the transformed source point.
     */
    public final Point2D getPoint2D (Point2D srcPt, Point2D dstPt)
    {
      return transform.transform (srcPt, dstPt);
    }

    /**
     * Returns rendering hints that are used during transformation.
     *
     * @return the rendering hints used in this Op.
     */
    public final RenderingHints getRenderingHints ()
    {
      return hints;
    }

    /**
     * Returns transform used in transformation between source and destination
     * image.
     *
     * @return the transform used in this Op.
     */
    public final AffineTransform getTransform ()
    {
      return transform;
    }
    
    /**
     * Perform nearest-neighbour filtering
     * 
     * @param src the source raster
     * @param dst the destination raster
     * @param dpts array of points on the destination raster
     * @param pts array of corresponding points on the source raster
     */
    private void filterNearest(Raster src, WritableRaster dst, double[] dpts,
                               double[] pts)
    {
      Rectangle srcbounds = src.getBounds();
  
      // For all points on the destination raster, copy the value from the
      // corrosponding (rounded) source point
      for (int i = 0; i < dpts.length; i += 2)
        {
          int srcX = (int) Math.round(pts[i]) + src.getMinX();
          int srcY = (int) Math.round(pts[i + 1]) + src.getMinY();
          
          if (srcbounds.contains(srcX, srcY))
            dst.setDataElements((int) dpts[i] + dst.getMinX(),
                                (int) dpts[i + 1] + dst.getMinY(),
                                src.getDataElements(srcX, srcY, null));
        }
    }

    /**
     * Perform bilinear filtering
     * 
     * @param src the source raster
     * @param dst the destination raster
     * @param dpts array of points on the destination raster
     * @param pts array of corresponding points on the source raster
     */
    private void filterBilinear(Raster src, WritableRaster dst, double[] dpts,
                              double[] pts)
    {
      Rectangle srcbounds = src.getBounds();
  
      Object xyarr = null;
      Object xp1arr = null;
      Object yp1arr = null;
      Object xyp1arr = null;
      
      double xy;
      double xp1;
      double yp1;
      double xyp1;

      double[] result = new double[src.getNumBands()];
      
      // For all points in the destination raster, use bilinear interpolation
      // to find the value from the corrosponding source points
      for (int i = 0; i < dpts.length; i += 2)
        {
          int srcX = (int) Math.round(pts[i]) + src.getMinX();
          int srcY = (int) Math.round(pts[i + 1]) + src.getMinY();
          
          if (srcbounds.contains(srcX, srcY))
            {
              // Corner case at the bottom or right edge; use nearest neighbour
              if (pts[i] >= src.getWidth() - 1
                  || pts[i + 1] >= src.getHeight() - 1)
                dst.setDataElements((int) dpts[i] + dst.getMinX(),
                                    (int) dpts[i + 1] + dst.getMinY(),
                                    src.getDataElements(srcX, srcY, null));
  
              // Standard case, apply the bilinear formula
              else
                {
                  int x = (int) Math.floor(pts[i] + src.getMinX());
                  int y = (int) Math.floor(pts[i + 1] + src.getMinY());
                  double xdiff = pts[i] + src.getMinX() - x;
                  double ydiff = pts[i + 1] + src.getMinY() - y;

                  // Get surrounding pixels used in interpolation... optimized
                  // to use the smallest datatype possible.
                  if (src.getTransferType() == DataBuffer.TYPE_DOUBLE
                      || src.getTransferType() == DataBuffer.TYPE_FLOAT)
                    {
                      xyarr = src.getPixel(x, y, (double[])xyarr);
                      xp1arr  = src.getPixel(x+1, y, (double[])xp1arr);
                      yp1arr = src.getPixel(x, y+1, (double[])yp1arr);
                      xyp1arr = src.getPixel(x+1, y+1, (double[])xyp1arr);
                    }
                  else
                    {
                      xyarr = src.getPixel(x, y, (int[])xyarr);
                      xp1arr  = src.getPixel(x+1, y, (int[])xp1arr);
                      yp1arr = src.getPixel(x, y+1, (int[])yp1arr);
                      xyp1arr = src.getPixel(x+1, y+1, (int[])xyp1arr);
                    }
                  // using 
                  // array[] pixels = src.getPixels(x, y, 2, 2, pixels);
                  // instead of doing four individual src.getPixel() calls
                  // should be faster, but benchmarking shows that it's not...
                  
                  // Run interpolation for each band
                  for (int j = 0; j < src.getNumBands(); j++)
                    {
                      // Pull individual sample values out of array
                      if (src.getTransferType() == DataBuffer.TYPE_DOUBLE
                          || src.getTransferType() == DataBuffer.TYPE_FLOAT)
                        {
                          xy = ((double[])xyarr)[j];
                          xp1  = ((double[])xp1arr)[j];
                          yp1 = ((double[])yp1arr)[j];
                          xyp1 = ((double[])xyp1arr)[j];
                        }
                      else
                        {
                          xy = ((int[])xyarr)[j];
                          xp1  = ((int[])xp1arr)[j];
                          yp1 = ((int[])yp1arr)[j];
                          xyp1 = ((int[])xyp1arr)[j];
                        }
                      
                      // If all four samples are identical, there's no need to 
                      // calculate anything
                      if (xy == xp1 && xy == yp1 && xy == xyp1)
                        result[j] = xy;
                      
                      // Run bilinear interpolation formula
                      else
                        result[j] = (xy * (1-xdiff) + xp1 * xdiff) 
                                      * (1-ydiff) 
                                    + (yp1 * (1-xdiff) + xyp1 * xdiff)
                                      * ydiff;
                    }

                  dst.setPixel((int)dpts[i] + dst.getMinX(),
                               (int)dpts[i+1] + dst.getMinY(),
                               result);
                }
            }
        }
    }

    /**
     * Perform bicubic filtering
     * based on http://local.wasp.uwa.edu.au/~pbourke/colour/bicubic/
     * 
     * @param src the source raster
     * @param dst the destination raster
     * @param dpts array of points on the destination raster
     * @param pts array of corresponding points on the source raster
     */
    private void filterBicubic(Raster src, WritableRaster dst, double[] dpts,
                               double[] pts)
    {
      Rectangle srcbounds = src.getBounds();
      double[] result = new double[src.getNumBands()];
      Object pixels = null;

      // For all points on the destination raster, perform bicubic interpolation
      // from corrosponding source points
      for (int i = 0; i < dpts.length; i += 2)
        {
          if (srcbounds.contains((int) Math.round(pts[i]) + src.getMinX(),
                                 (int) Math.round(pts[i + 1]) + src.getMinY()))
            {
              int x = (int) Math.floor(pts[i] + src.getMinX());
              int y = (int) Math.floor(pts[i + 1] + src.getMinY());
              double dx = pts[i] + src.getMinX() - x;
              double dy = pts[i + 1] + src.getMinY() - y;
              Arrays.fill(result, 0);
  
              for (int m = - 1; m < 3; m++)
                for (int n = - 1; n < 3; n++)
                  {
                    // R(x) = ( P(x+2)^3 - 4 P(x+1)^3 + 6 P(x)^3 - 4 P(x-1)^3 ) / 6
                    double r1 = 0;
                    double r2 = 0;

                    // Calculate R(m - dx)
                    double rx = m - dx + 2;
                    r1 += rx * rx * rx;

                    rx = m - dx + 1;
                    if (rx > 0)
                      r1 -= 4 * rx * rx * rx;

                    rx = m - dx;
                    if (rx > 0)
                      r1 += 6 * rx * rx * rx;

                    rx = m - dx - 1;
                    if (rx > 0)
                      r1 -= 4 * rx * rx * rx;

                    r1 /= 6;

                    // Calculate R(dy - n);
                    rx = dy - n + 2;
                    if (rx > 0)
                      r2 += rx * rx * rx;

                    rx = dy - n + 1;
                    if (rx > 0)
                      r2 -= 4 * rx * rx * rx;

                    rx = dy - n;
                    if (rx > 0)
                      r2 += 6 * rx * rx * rx;

                    rx = dy - n - 1;
                    if (rx > 0)
                      r2 -= 4 * rx * rx * rx;

                    r2 /= 6;

                    // Calculate F(i+m, j+n) R(m - dx) R(dy - n)
                    // Check corner cases
                    int srcX = x + m;
                    if (srcX >= src.getMinX() + src.getWidth())
                      srcX = src.getMinX() + src.getWidth() - 1;
                    else if (srcX < src.getMinX())
                      srcX = src.getMinX();

                    int srcY = y + n;
                    if (srcY >= src.getMinY() + src.getHeight())
                      srcY = src.getMinY() + src.getHeight() - 1;
                    else if (srcY < src.getMinY())
                      srcY = src.getMinY();

                    // Calculate once for each band, using the smallest
                    // datatype possible
                    if (src.getTransferType() == DataBuffer.TYPE_DOUBLE
                        || src.getTransferType() == DataBuffer.TYPE_FLOAT)
                      {
                        pixels = src.getPixel(srcX, srcY, (double[])pixels);
                        for (int j = 0; j < result.length; j++)
                          result[j] += ((double[])pixels)[j] * r1 * r2;
                      }
                    else
                      {
                        pixels = src.getPixel(srcX, srcY, (int[])pixels);
                        for (int j = 0; j < result.length; j++)
                          result[j] += ((int[])pixels)[j] * r1 * r2;
                      }
                  }
  
              // Put it all together
              dst.setPixel((int)dpts[i] + dst.getMinX(),
                           (int)dpts[i+1] + dst.getMinY(),
                           result);
            }
        }
    }
}
