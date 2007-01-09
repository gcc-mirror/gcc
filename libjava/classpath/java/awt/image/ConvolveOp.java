/* ConvolveOp.java --
   Copyright (C) 2004, 2005, 2006, Free Software Foundation -- ConvolveOp

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

import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * Convolution filter.
 * 
 * ConvolveOp convolves the source image with a Kernel to generate a
 * destination image.  This involves multiplying each pixel and its neighbors
 * with elements in the kernel to compute a new pixel.
 * 
 * Each band in a Raster is convolved and copied to the destination Raster.
 * For BufferedImages, convolution is applied to all components.  Color 
 * conversion will be applied if needed.
 * 
 * Note that this filter ignores whether the source or destination is alpha
 * premultiplied.  The reference spec states that data will be premultiplied
 * prior to convolving and divided back out afterwards (if needed), but testing
 * has shown that this is not the case with their implementation.
 * 
 * @author jlquinn@optonline.net
 */
public class ConvolveOp implements BufferedImageOp, RasterOp
{
  /** Edge pixels are set to 0. */
  public static final int EDGE_ZERO_FILL = 0;
  
  /** Edge pixels are copied from the source. */
  public static final int EDGE_NO_OP = 1;
  
  private Kernel kernel;
  private int edge;
  private RenderingHints hints;

  /**
   * Construct a ConvolveOp.
   * 
   * The edge condition specifies that pixels outside the area that can be
   * filtered are either set to 0 or copied from the source image.
   * 
   * @param kernel The kernel to convolve with.
   * @param edgeCondition Either EDGE_ZERO_FILL or EDGE_NO_OP.
   * @param hints Rendering hints for color conversion, or null.
   */
  public ConvolveOp(Kernel kernel,
      				int edgeCondition,
      				RenderingHints hints)
  {
    this.kernel = kernel;
    edge = edgeCondition;
    this.hints = hints;
  }
  
  /**
   * Construct a ConvolveOp.
   * 
   * The edge condition defaults to EDGE_ZERO_FILL.
   * 
   * @param kernel The kernel to convolve with.
   */
  public ConvolveOp(Kernel kernel)
  {
    this.kernel = kernel;
    edge = EDGE_ZERO_FILL;
    hints = null;
  }

  /**
   * Converts the source image using the kernel specified in the
   * constructor.  The resulting image is stored in the destination image if one
   * is provided; otherwise a new BufferedImage is created and returned. 
   * 
   * The source and destination BufferedImage (if one is supplied) must have
   * the same dimensions.
   *
   * @param src The source image.
   * @param dst The destination image.
   * @throws IllegalArgumentException if the rasters and/or color spaces are
   *            incompatible.
   * @return The convolved image.
   */
  public final BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    if (src == dst)
      throw new IllegalArgumentException("Source and destination images " +
            "cannot be the same.");
    
    if (dst == null)
      dst = createCompatibleDestImage(src, src.getColorModel());
    
    // Make sure source image is premultiplied
    BufferedImage src1 = src;
    // The spec says we should do this, but mauve testing shows that Sun's
    // implementation does not check this.
    /*
    if (!src.isAlphaPremultiplied())
    {
      src1 = createCompatibleDestImage(src, src.getColorModel());
      src.copyData(src1.getRaster());
      src1.coerceData(true);
    }
    */

    BufferedImage dst1 = dst;
    if (src1.getColorModel().getColorSpace().getType() != dst.getColorModel().getColorSpace().getType())
      dst1 = createCompatibleDestImage(src, src.getColorModel());

    filter(src1.getRaster(), dst1.getRaster());
    
    // Since we don't coerceData above, we don't need to divide it back out.
    // This is wrong (one mauve test specifically tests converting a non-
    // premultiplied image to a premultiplied image, and it shows that Sun
    // simply ignores the premultipled flag, contrary to the spec), but we
    // mimic it for compatibility.
    /*
	if (! dst.isAlphaPremultiplied())
	  dst1.coerceData(false);
    */

    // Convert between color models if needed
    if (dst1 != dst)
      new ColorConvertOp(hints).filter(dst1, dst);

    return dst;
  }

  /**
   * Creates an empty BufferedImage with the size equal to the source and the
   * correct number of bands. The new image is created with the specified 
   * ColorModel, or if no ColorModel is supplied, an appropriate one is chosen.
   *
   * @param src The source image.
   * @param dstCM A color model for the destination image (may be null).
   * @return The new compatible destination image.
   */
  public BufferedImage createCompatibleDestImage(BufferedImage src,
                                                 ColorModel dstCM)
  {
    if (dstCM != null)
      return new BufferedImage(dstCM,
                               src.getRaster().createCompatibleWritableRaster(),
                               src.isAlphaPremultiplied(), null);

    return new BufferedImage(src.getWidth(), src.getHeight(), src.getType());
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getRenderingHints()
   */
  public final RenderingHints getRenderingHints()
  {
    return hints;
  }
  
  /**
   * Get the edge condition for this Op.
   * 
   * @return The edge condition.
   */
  public int getEdgeCondition()
  {
    return edge;
  }
  
  /**
   * Returns (a clone of) the convolution kernel.
   *
   * @return The convolution kernel.
   */
  public final Kernel getKernel()
  {
    return (Kernel) kernel.clone();
  }

  /**
   * Converts the source raster using the kernel specified in the constructor.  
   * The resulting raster is stored in the destination raster if one is 
   * provided; otherwise a new WritableRaster is created and returned.
   * 
   * If the convolved value for a sample is outside the range of [0-255], it
   * will be clipped.
   * 
   * The source and destination raster (if one is supplied) cannot be the same,
   * and must also have the same dimensions.
   *
   * @param src The source raster.
   * @param dest The destination raster.
   * @throws IllegalArgumentException if the rasters identical.
   * @throws ImagingOpException if the convolution is not possible.
   * @return The transformed raster.
   */
  public final WritableRaster filter(Raster src, WritableRaster dest)
  {
    if (src == dest)
      throw new IllegalArgumentException("src == dest is not allowed.");
    if (kernel.getWidth() > src.getWidth() 
        || kernel.getHeight() > src.getHeight())
      throw new ImagingOpException("The kernel is too large.");
    if (dest == null)
      dest = createCompatibleDestRaster(src);
    else if (src.getNumBands() != dest.getNumBands())
      throw new ImagingOpException("src and dest have different band counts.");

    // calculate the borders that the op can't reach...
    int kWidth = kernel.getWidth();
    int kHeight = kernel.getHeight();
    int left = kernel.getXOrigin();
    int right = Math.max(kWidth - left - 1, 0);
    int top = kernel.getYOrigin();
    int bottom = Math.max(kHeight - top - 1, 0);
    
    // Calculate max sample values for clipping
    int[] maxValue = src.getSampleModel().getSampleSize();
    for (int i = 0; i < maxValue.length; i++)
      maxValue[i] = (int)Math.pow(2, maxValue[i]) - 1;
    
    // process the region that is reachable...
    int regionW = src.width - left - right;
    int regionH = src.height - top - bottom;
    float[] kvals = kernel.getKernelData(null);
    float[] tmp = new float[kWidth * kHeight];

    for (int x = 0; x < regionW; x++)
      {
        for (int y = 0; y < regionH; y++)
          {
            // FIXME: This needs a much more efficient implementation
            for (int b = 0; b < src.getNumBands(); b++)
            {
              float v = 0;
              src.getSamples(x, y, kWidth, kHeight, b, tmp);
              for (int i = 0; i < tmp.length; i++)
                v += tmp[tmp.length - i - 1] * kvals[i];
                // FIXME: in the above line, I've had to reverse the order of 
                // the samples array to make the tests pass.  I haven't worked 
                // out why this is necessary.

              // This clipping is is undocumented, but determined by testing.
              if (v > maxValue[b])
                v = maxValue[b];
              else if (v < 0)
                v = 0;

              dest.setSample(x + kernel.getXOrigin(), y + kernel.getYOrigin(), 
                             b, v);
            }
          }
      }
    
    // fill in the top border
    fillEdge(src, dest, 0, 0, src.width, top, edge);
    
    // fill in the bottom border
    fillEdge(src, dest, 0, src.height - bottom, src.width, bottom, edge);
    
    // fill in the left border
    fillEdge(src, dest, 0, top, left, regionH, edge);
    
    // fill in the right border
    fillEdge(src, dest, src.width - right, top, right, regionH, edge);
    
    return dest;  
  }
  
  /**
   * Fills a range of pixels (typically at the edge of a raster) with either
   * zero values (if <code>edgeOp</code> is <code>EDGE_ZERO_FILL</code>) or the 
   * corresponding pixel values from the source raster (if <code>edgeOp</code>
   * is <code>EDGE_NO_OP</code>).  This utility method is called by the 
   * {@link #fillEdge(Raster, WritableRaster, int, int, int, int, int)} method.
   * 
   * @param src  the source raster.
   * @param dest  the destination raster.
   * @param x  the x-coordinate of the top left pixel in the range.
   * @param y  the y-coordinate of the top left pixel in the range.
   * @param w  the width of the pixel range.
   * @param h  the height of the pixel range.
   * @param edgeOp  indicates how to determine the values for the range
   *     (either {@link #EDGE_ZERO_FILL} or {@link #EDGE_NO_OP}).
   */
  private void fillEdge(Raster src, WritableRaster dest, int x, int y, int w, 
                        int h, int edgeOp) 
  {
    if (w <= 0)
      return;
    if (h <= 0)
      return;
    if (edgeOp == EDGE_ZERO_FILL)  // fill region with zeroes
      {
        float[] zeros = new float[src.getNumBands() * w * h];
        dest.setPixels(x, y, w, h, zeros); 
      }
    else  // copy pixels from source
      {
        float[] pixels = new float[src.getNumBands() * w * h];
        src.getPixels(x, y, w, h, pixels);
        dest.setPixels(x, y, w, h, pixels);
      }
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#createCompatibleDestRaster(java.awt.image.Raster)
   */
  public WritableRaster createCompatibleDestRaster(Raster src)
  {
    return src.createCompatibleWritableRaster();
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#getBounds2D(java.awt.image.BufferedImage)
   */
  public final Rectangle2D getBounds2D(BufferedImage src)
  {
    return src.getRaster().getBounds();
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getBounds2D(java.awt.image.Raster)
   */
  public final Rectangle2D getBounds2D(Raster src)
  {
    return src.getBounds();
  }

  /**
   * Returns the corresponding destination point for a source point. Because
   * this is not a geometric operation, the destination and source points will
   * be identical.
   * 
   * @param src The source point.
   * @param dst The transformed destination point.
   * @return The transformed destination point.
   */
  public final Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null) return (Point2D)src.clone();
    dst.setLocation(src);
    return dst;
  }
}
