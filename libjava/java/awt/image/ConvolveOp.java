/* Copyright (C) 2004 Free Software Foundation -- ConvolveOp

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

/*
 * Created on Nov 1, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package java.awt.image;

import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Arrays;

/**
 * Convolution filter.
 * 
 * ConvolveOp convolves the source image with a Kernel to generate a
 * destination image.  This involves multiplying each pixel and its neighbors
 * with elements in the kernel to compute a new pixel.
 * 
 * Each band in a Raster is convolved and copied to the destination Raster.
 * 
 * For BufferedImages, convolution is applied to all components.  If the
 * source is not premultiplied, the data will be premultiplied before
 * convolving.  Premultiplication will be undone if the destination is not
 * premultiplied.  Color conversion will be applied if needed.
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

  
  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#filter(java.awt.image.BufferedImage,
   * java.awt.image.BufferedImage)
   */
  public BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    if (src == dst)
      throw new IllegalArgumentException();
    
    if (dst == null)
      dst = createCompatibleDestImage(src, src.getColorModel());
    
    // Make sure source image is premultiplied
    BufferedImage src1 = src;
    if (!src.isPremultiplied)
    {
      src1 = createCompatibleDestImage(src, src.getColorModel());
      src.copyData(src1.getRaster());
      src1.coerceData(true);
    }

    BufferedImage dst1 = dst;
    if (!src.getColorModel().equals(dst.getColorModel()))
      dst1 = createCompatibleDestImage(src, src.getColorModel());

    filter(src1.getRaster(), dst1.getRaster());
    
    if (dst1 != dst)
    {
      // Convert between color models.
      // TODO Check that premultiplied alpha is handled correctly here.
      Graphics2D gg = dst.createGraphics();
      gg.setRenderingHints(hints);
      gg.drawImage(dst1, 0, 0, null);
      gg.dispose();
    }
    
    return dst;
  }

  /* (non-Javadoc)
   * @see
   * java.awt.image.BufferedImageOp#createCompatibleDestImage(java.awt.image.BufferedImage,
   * java.awt.image.ColorModel)
   */
  public BufferedImage createCompatibleDestImage(BufferedImage src,
						 ColorModel dstCM)
  {
    // FIXME: set properties to those in src
    return new BufferedImage(dstCM,
			     src.getRaster().createCompatibleWritableRaster(),
			     src.isPremultiplied, null);
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getRenderingHints()
   */
  public RenderingHints getRenderingHints()
  {
    return hints;
  }
  
  /**
   * @return The edge condition.
   */
  public int getEdgeCondition()
  {
    return edge;
  }
  
  /**
   * @return The convolution kernel.
   */
  public Kernel getKernel()
  {
    return kernel;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#filter(java.awt.image.Raster,
   * java.awt.image.WritableRaster)
   */
  public WritableRaster filter(Raster src, WritableRaster dest) {
    if (src.numBands != dest.numBands)
      throw new ImagingOpException(null);
    if (src == dest)
      throw new IllegalArgumentException();
    if (src.getWidth() < kernel.getWidth() ||
        src.getHeight() < kernel.getHeight())
      throw new ImagingOpException(null);
    
    if (dest == null)
      dest = createCompatibleDestRaster(src);

    // Deal with bottom edge
    if (edge == EDGE_ZERO_FILL)
    {
      float[] zeros = new float[src.getNumBands() * src.getWidth()
				* (kernel.getYOrigin() - 1)];
      Arrays.fill(zeros, 0);
      dest.setPixels(src.getMinX(), src.getMinY(), src.getWidth(),
		     kernel.getYOrigin() - 1, zeros);
    }
    else
    {
      float[] vals = new float[src.getNumBands() * src.getWidth()
			       * (kernel.getYOrigin() - 1)];
      src.getPixels(src.getMinX(), src.getMinY(), src.getWidth(),
		    kernel.getYOrigin() - 1, vals);
      dest.setPixels(src.getMinX(), src.getMinY(), src.getWidth(),
		     kernel.getYOrigin() - 1, vals);
    }
    
    // Handle main section
    float[] kvals = kernel.getKernelData(null);

    float[] tmp = new float[kernel.getWidth() * kernel.getHeight()];
    for (int y = src.getMinY() + kernel.getYOrigin();
    	 y < src.getMinY() + src.getHeight() - kernel.getYOrigin() / 2; y++)
    {
      // Handle unfiltered edge pixels at start of line
      float[] t1 = new float[(kernel.getXOrigin() - 1) * src.getNumBands()];
      if (edge == EDGE_ZERO_FILL)
        Arrays.fill(t1, 0);
      else
        src.getPixels(src.getMinX(), y, kernel.getXOrigin() - 1, 1, t1);
      dest.setPixels(src.getMinX(), y, kernel.getXOrigin() - 1, 1, t1);
      
      for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
      {
        // FIXME: This needs a much more efficient implementation
        for (int b = 0; b < src.getNumBands(); b++)
        {
          float v = 0;
          src.getSamples(x, y, kernel.getWidth(), kernel.getHeight(), b, tmp);
          for (int i=0; i < tmp.length; i++)
            v += tmp[i] * kvals[i];
          dest.setSample(x, y, b, v);
        }
      }

      // Handle unfiltered edge pixels at end of line
      float[] t2 = new float[(kernel.getWidth() / 2) * src.getNumBands()];
      if (edge == EDGE_ZERO_FILL)
        Arrays.fill(t2, 0);
      else
        src.getPixels(src.getMinX() + src.getWidth()
		      - (kernel.getWidth() / 2),
		      y, kernel.getWidth() / 2, 1, t2);
      dest.setPixels(src.getMinX() + src.getWidth() - (kernel.getWidth() / 2),
		     y, kernel.getWidth() / 2, 1, t2);
    }
    for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
      for (int x = src.getMinX(); x< src.getWidth() + src.getMinX(); x++)
      {
        
      }
    for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
      for (int x = src.getMinX(); x< src.getWidth() + src.getMinX(); x++)
      {
        
      }
      
    // Handle top edge
    if (edge == EDGE_ZERO_FILL)
    {
      float[] zeros = new float[src.getNumBands() * src.getWidth() *
                                (kernel.getHeight() / 2)];
      Arrays.fill(zeros, 0);
      dest.setPixels(src.getMinX(),
          src.getHeight() + src.getMinY() - (kernel.getHeight() / 2),
          src.getWidth(), kernel.getHeight() / 2, zeros);
    }
    else
    {
      float[] vals = new float[src.getNumBands() * src.getWidth() *
                               (kernel.getHeight() / 2)];
      src.getPixels(src.getMinX(),
		    src.getHeight() + src.getMinY()
		    - (kernel.getHeight() / 2),
		    src.getWidth(), kernel.getHeight() / 2, vals);
      dest.setPixels(src.getMinX(),
		     src.getHeight() + src.getMinY()
		     - (kernel.getHeight() / 2),
		     src.getWidth(), kernel.getHeight() / 2, vals);
    }
    
    return dest;
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
  public Rectangle2D getBounds2D(BufferedImage src)
  {
    return src.getRaster().getBounds();
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getBounds2D(java.awt.image.Raster)
   */
  public Rectangle2D getBounds2D(Raster src)
  {
    return src.getBounds();
  }

  /** Return corresponding destination point for source point.
   * 
   * ConvolveOp will return the value of src unchanged.
   * @param src The source point.
   * @param dst The destination point.
   * @see java.awt.image.RasterOp#getPoint2D(java.awt.geom.Point2D,
   * java.awt.geom.Point2D)
   */
  public Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null) return (Point2D)src.clone();
    dst.setLocation(src);
    return dst;
  }
}
