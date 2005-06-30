/* LookupOp.java -- Filter that converts each pixel using a lookup table.
   Copyright (C) 2004  Free Software Foundation

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
import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * LookupOp is a filter that converts each pixel using a lookup table.
 * 
 * For filtering Rasters, the lookup table must have either one component
 * that is applied to all bands, or one component for every band in the
 * Rasters.
 * 
 * For BufferedImages, the lookup table may apply to both color and alpha
 * components.  If the lookup table contains one component, or if there are
 * the same number of components as color components in the source, the table
 * applies to all color components.  Otherwise the table applies to all
 * components including alpha.  Alpha premultiplication is ignored during the
 * lookup filtering.
 * 
 * After filtering, if color conversion is necessary, the conversion happens,
 * taking alpha premultiplication into account.
 * 
 * @author jlquinn
 */
public class LookupOp implements BufferedImageOp, RasterOp
{
  private LookupTable lut;
  private RenderingHints hints;
  
  /** Construct a new LookupOp.
   * 
   * @param lookup LookupTable to use.
   * @param hints Rendering hints (can be null).
   */
  public LookupOp(LookupTable lookup, RenderingHints hints)
  {
    lut = lookup;
    this.hints = hints;
  }
  
  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#filter(java.awt.image.BufferedImage, java.awt.image.BufferedImage)
   */
  public BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    if (src.getColorModel() instanceof IndexColorModel)
      throw new IllegalArgumentException("LookupOp.filter: IndexColorModel "
					 + "not allowed");
    if (dst == null)
      dst = createCompatibleDestImage(src, src.getColorModel());

    // Set up for potential colormodel mismatch
    BufferedImage tgt;
    if (dst.getColorModel().equals(src.getColorModel()))
      tgt = dst;
    else
      tgt = createCompatibleDestImage(src, src.getColorModel());

    Raster sr = src.getRaster();
    WritableRaster dr = tgt.getRaster();

    if (src.getColorModel().hasAlpha() &&
        (lut.getNumComponents() == 1 ||
         lut.getNumComponents() == src.getColorModel().getNumColorComponents()))
    {
      // Need to ignore alpha for lookup
      int[] dbuf = new int[src.getColorModel().getNumComponents()];
      int tmpBands = src.getColorModel().getNumColorComponents();
      int[] tmp = new int[tmpBands];
        
      // Filter the pixels
      for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
        for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
        {	
          // Filter only color components, but also copy alpha
          sr.getPixel(x, y, dbuf);
          System.arraycopy(dbuf, 0, tmp, 0, tmpBands);
          dr.setPixel(x, y, lut.lookupPixel(tmp, dbuf));
        }
    }
    else if (lut.getNumComponents() != 1
	     &&
	     lut.getNumComponents() != src.getColorModel().getNumComponents())
      throw new IllegalArgumentException("LookupOp.filter: "
					 + "Incompatible lookup "
					 + "table and source image");

    // No alpha to ignore
    int[] dbuf = new int[src.getColorModel().getNumComponents()];
        
    // Filter the pixels
    for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
      for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
        dr.setPixel(x, y, lut.lookupPixel(sr.getPixel(x, y, dbuf), dbuf));

    if (tgt != dst)
    {
      // Convert between color models.
      // TODO Check that premultiplied alpha is handled correctly here.
      Graphics2D gg = dst.createGraphics();
      gg.setRenderingHints(hints);
      gg.drawImage(tgt, 0, 0, null);
      gg.dispose();
    }
    
    return dst;
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#getBounds2D(java.awt.image.BufferedImage)
   */
  public Rectangle2D getBounds2D(BufferedImage src)
  {
    return src.getRaster().getBounds();
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#createCompatibleDestImage(java.awt.image.BufferedImage, java.awt.image.ColorModel)
   */
  public BufferedImage createCompatibleDestImage(BufferedImage src,
						 ColorModel dstCM)
  {
    // FIXME: set properties to those in src
    return new BufferedImage(dstCM,
			     src.getRaster().createCompatibleWritableRaster(),
			     src.isPremultiplied, null);
  }

  /** Return corresponding destination point for source point.
   * 
   * LookupOp will return the value of src unchanged.
   * @param src The source point.
   * @param dst The destination point.
   * @see java.awt.image.RasterOp#getPoint2D(java.awt.geom.Point2D, java.awt.geom.Point2D)
   */
  public Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null)
      return (Point2D) src.clone();

    dst.setLocation(src);
    return dst;
  }

  /** Return the LookupTable for this op. */
  public LookupTable getTable()
  {
    return lut;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getRenderingHints()
   */
  public RenderingHints getRenderingHints()
  {
    return hints;
  }

  /** Filter a raster through a lookup table.
   * 
   * Applies the lookup table for this Rasterop to each pixel of src and
   * puts the results in dest.  If dest is null, a new Raster is created and
   * returned.
   * 
   * @param src The source raster.
   * @param dest The destination raster.
   * @return The WritableRaster with the filtered pixels.
   * @throws IllegalArgumentException if lookup table has more than one
   * component but not the same as src and dest.
   * @see java.awt.image.RasterOp#filter(java.awt.image.Raster, java.awt.image.WritableRaster)
   */
  public WritableRaster filter(Raster src, WritableRaster dest)
  {
    if (dest == null) 
      // Allocate a raster if needed
      dest = createCompatibleDestRaster(src);
    else
      if (src.getNumBands() != dest.getNumBands())
        throw new IllegalArgumentException();

    if (lut.getNumComponents() != 1
	&& lut.getNumComponents() != src.getNumBands())
      throw new IllegalArgumentException();

   
    // Allocate pixel storage. 
    int[] tmp = new int[src.getNumBands()];
    
    // Filter the pixels
    for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
      for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
        dest.setPixel(x, y, lut.lookupPixel(src.getPixel(x, y, tmp), tmp));
    return dest;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#getBounds2D(java.awt.image.Raster)
   */
  public Rectangle2D getBounds2D(Raster src)
  {
    return src.getBounds();
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#createCompatibleDestRaster(java.awt.image.Raster)
   */
  public WritableRaster createCompatibleDestRaster(Raster src)
  {
    return src.createCompatibleWritableRaster();
  }

}
