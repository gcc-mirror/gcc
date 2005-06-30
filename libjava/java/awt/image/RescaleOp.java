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
import java.util.Arrays;

/**
 * @author Jerry Quinn (jlquinn@optonline.net)
 */
public class RescaleOp implements BufferedImageOp, RasterOp
{
  private float[] scale;
  private float[] offsets;
  private RenderingHints hints = null;
  
  public RescaleOp(float[] scaleFactors,
		   float[] offsets,
		   RenderingHints hints)
  {
    this.scale = scaleFactors;
    this.offsets = offsets;
    this.hints = hints;
  }
  
  public RescaleOp(float scaleFactor,
		   float offset,
		   RenderingHints hints)
  {
    scale = new float[]{ scaleFactor };
    offsets = new float[]{offset};
    this.hints = hints;
  }

  public final float[] getScaleFactors(float[] scaleFactors)
  {
    if (scaleFactors == null)
      scaleFactors = new float[scale.length];
    System.arraycopy(scale, 0, scaleFactors, 0, scale.length);
    return scaleFactors;
  }

  public final float[] getOffsets(float[] offsets)
  {
    if (offsets == null)
      offsets = new float[this.offsets.length];
    System.arraycopy(this.offsets, 0, offsets, 0, this.offsets.length);
    return offsets;
  }

  public final int getNumFactors()
  {
    return scale.length;
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#getRenderingHints()
   */
  public RenderingHints getRenderingHints()
  {
    return hints;
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#filter(java.awt.image.BufferedImage, java.awt.image.BufferedImage)
   */
  public final BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    // TODO Make sure premultiplied alpha is handled correctly.
    // TODO See that color conversion is handled.
    // TODO figure out how to use rendering hints.
    if (scale.length != offsets.length)
      throw new IllegalArgumentException();

    ColorModel scm = src.getColorModel();
    if (dst == null) dst = createCompatibleDestImage(src, null);

    WritableRaster wsrc = src.getRaster();
    WritableRaster wdst = dst.getRaster();
    
    // Share constant across colors except alpha
    if (scale.length == 1 || scale.length == scm.getNumColorComponents())
      {
	// Construct a raster that doesn't include an alpha band.
	int[] subbands = new int[scm.getNumColorComponents()];
	for (int i=0; i < subbands.length; i++) subbands[i] = i;
	wsrc = 
	  wsrc.createWritableChild(wsrc.minX, wsrc.minY, wsrc.width, wsrc.height,
				   wsrc.minX, wsrc.minY, subbands);
      }
    // else all color bands

    filter(wsrc, wdst);
    return dst;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#filter(java.awt.image.Raster, java.awt.image.WritableRaster)
   */
  public final WritableRaster filter(Raster src, WritableRaster dest)
  {
    if (dest == null) dest = src.createCompatibleWritableRaster();

    // Required sanity checks
    if (src.numBands != dest.numBands || scale.length != offsets.length)
      throw new IllegalArgumentException();
    if (scale.length != 1 && scale.length != src.numBands)
      throw new IllegalArgumentException();

    // Create scaling arrays if needed
    float[] lscale = scale;
    float[] loff = offsets;
    if (scale.length == 1)
      {
	lscale = new float[src.numBands];
	Arrays.fill(lscale, scale[0]);
	loff = new float[src.numBands];
	Arrays.fill(loff, offsets[0]);
      }

    // TODO The efficiency here can be improved for various data storage
    // patterns, aka SampleModels.
    float[] pixel = new float[src.numBands];
    for (int y = src.minY; y < src.height + src.minY; y++)
      for (int x = src.minX; x < src.width + src.minX; x++)
	{
	  src.getPixel(x, y, pixel);
	  for (int b = 0; b < src.numBands; b++)
	    pixel[b] = pixel[b] * lscale[b] + loff[b];
	  dest.setPixel(x, y, pixel);
	}
    return dest;
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#createCompatibleDestImage(java.awt.image.BufferedImage, java.awt.image.ColorModel)
   */
  public BufferedImage createCompatibleDestImage(BufferedImage src,
						 ColorModel dstCM)
  {
    if (dstCM == null) dstCM = src.getColorModel();
    WritableRaster wr = src.getRaster().createCompatibleWritableRaster();
    BufferedImage image
      = new BufferedImage(dstCM, wr, src.isPremultiplied, null);
    return image;
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

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#getPoint2D(java.awt.geom.Point2D, java.awt.geom.Point2D)
   */
  public final Point2D getPoint2D(Point2D src, Point2D dst) {
    if (dst == null) dst = (Point2D) src.clone();
    else dst.setLocation(src);
    return dst;
  }

}
