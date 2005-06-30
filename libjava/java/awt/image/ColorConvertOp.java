/* ColorModel.java --
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
import java.awt.color.ColorSpace;
import java.awt.color.ICC_ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * ColorConvertOp is a filter for converting an image from one colorspace to
 * another colorspace.  The filter can convert the image through a sequence
 * of colorspaces or just from source to destination.
 * 
 * Color conversion is done on the color components without alpha.  Thus
 * if a BufferedImage has alpha premultiplied, this is divided out before
 * color conversion, and premultiplication applied if the destination
 * requires it.
 * 
 * Color rendering and dithering hints may be applied if specified.  This is
 * likely platform-dependent.
 * 
 * @author jlquinn@optonline.net
 */
public class ColorConvertOp implements BufferedImageOp, RasterOp
{
  private ColorSpace srccs;
  private ColorSpace dstcs;
  private RenderingHints hints;
  private ICC_Profile[] profiles;
  private ColorSpace[] spaces;
  private boolean rasterValid;
  

  /**
   * Convert BufferedImage through a ColorSpace.
   * 
   * This filter version is only valid for BufferedImages.  The source image
   * is converted to cspace.  If the destination is not null, it is then
   * converted to the destination colorspace.  Normally this filter will only
   * be used with a null destination.
   * 
   * @param cspace The target color space.
   * @param hints Rendering hints to use in conversion, or null.
   */
  public ColorConvertOp(ColorSpace cspace, RenderingHints hints)
  {
    if (cspace == null)
      throw new NullPointerException();
    spaces = new ColorSpace[]{cspace};
    this.hints = hints;
    rasterValid = false;
  }
  
  public ColorConvertOp(ColorSpace srcCspace, ColorSpace dstCspace,
			RenderingHints hints)
  {
    if (srcCspace == null || dstCspace == null)
      throw new NullPointerException();
    spaces = new ColorSpace[]{srcCspace, dstCspace};
    this.hints = hints;     
  }

  /**
   * Convert from a source image destination image color space.
   * 
   * This constructor builds a ColorConvertOp from an array of ICC_Profiles.
   * The source image will be converted through the sequence of color spaces
   * defined by the profiles.  If the sequence of profiles doesn't give a
   * well-defined conversion, throws IllegalArgumentException.
   * 
   * NOTE: Sun's docs don't clearly define what a well-defined conversion is
   *  - or perhaps someone smarter can come along and sort it out.  
   * 
   * For BufferedImages, when the first and last profiles match the
   * requirements of the source and destination color space respectively, the
   * corresponding conversion is unnecessary.  TODO: code this up.  I don't
   * yet understand how you determine this.
   * 
   * For Rasters, the first and last profiles must have the same number of
   * bands as the source and destination Rasters, respectively.  If this is
   * not the case, or there fewer than 2 profiles, an IllegalArgumentException
   * will be thrown. 
   * 
   * @param profiles
   * @param hints
   */
  public ColorConvertOp(ICC_Profile[] profiles, RenderingHints hints)
  {
    if (profiles == null)
      throw new NullPointerException();
    this.hints = hints; 
    this.profiles = profiles;
    // TODO: Determine if this is well-defined.
    // Create colorspace array with space for src and dest colorspace
    spaces = new ColorSpace[profiles.length];
    for (int i = 0; i < profiles.length; i++)
      spaces[i] = new ICC_ColorSpace(profiles[i]);
  }
  
  /** Convert from source image color space to destination image color space.
   * 
   * Only valid for BufferedImage objects, this Op converts from the source
   * color space to the destination color space.  The destination can't be
   * null for this operation.
   * 
   * @param hints Rendering hints to use during conversion, or null.
   */
  public ColorConvertOp(RenderingHints hints)
  {
    this.hints = hints; 
    srccs = null;
    dstcs = null;
    rasterValid = false;
  }
  
  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#filter(java.awt.image.BufferedImage,
   java.awt.image.BufferedImage)
   */
  public final BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    // TODO: The plan is to create a scanline buffer for intermediate buffers.
    // For now we just suck it up and create intermediate buffers.
    
    if (dst == null && spaces.length == 0)
      throw new IllegalArgumentException();

    // Make sure input isn't premultiplied by alpha
    if (src.isAlphaPremultiplied())
    {
      BufferedImage tmp = createCompatibleDestImage(src, src.getColorModel());
      copyimage(src, tmp);
      tmp.coerceData(false);
      src = tmp;
    }

    ColorModel scm = src.getColorModel();
    for (int i = 0; i < spaces.length; i++)
    {
      ColorModel cm = scm.cloneColorModel(spaces[i]);
      BufferedImage tmp = createCompatibleDestImage(src, cm);
      copyimage(src, tmp);
      src = tmp;
    }

    // Intermediate conversions leave result in src
    if (dst == null)
      return src;
    
    // Apply final conversion
    copyimage(src, dst);
    return dst;
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
			     src.isPremultiplied,
			     null);
  }
  
  public final ICC_Profile[] getICC_Profiles()
  {
    return profiles;
  }

  /** Return the rendering hints for this op. */
  public final RenderingHints getRenderingHints()
  {
    return hints;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#filter(java.awt.image.Raster, java.awt.image.WritableRaster)
   */
  public final WritableRaster filter(Raster src, WritableRaster dest)
  {
    if (!rasterValid)
      throw new IllegalArgumentException();
    
    // Need to iterate through each color space - there must be at least 2
    for (int i = 1; i < spaces.length - 1; i++)
    {
      // FIXME: this is wrong.  tmp needs to have the same number of bands as
      // spaces[i] has.
      WritableRaster tmp = createCompatibleDestRaster(src);
      copyraster(src, spaces[i - 1], tmp, spaces[i]);
      src = tmp;
    }
    
    // FIXME: this is wrong.  dst needs to have the same number of bands as
    // spaces[i] has.
    if (dest == null)
      dest = createCompatibleDestRaster(src);
    copyraster(src, spaces[spaces.length - 2],
	       dest, spaces[spaces.length - 1]);
    
    return dest;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#createCompatibleDestRaster(java.awt.image.Raster)
   */
  public WritableRaster createCompatibleDestRaster(Raster src)
  {
    return src.createCompatibleWritableRaster();
  }

  /** Return corresponding destination point for source point.
   * 
   * LookupOp will return the value of src unchanged.
   * @param src The source point.
   * @param dst The destination point.
   * @see java.awt.image.RasterOp#getPoint2D(java.awt.geom.Point2D, java.awt.geom.Point2D)
   */
  public final Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null) return (Point2D)src.clone();
    dst.setLocation(src);
    return dst;
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
  
  // According to Sven de Marothy, we need to copy the src into the dest
  // using Graphics2D, in order to use the rendering hints.
  private void copyimage(BufferedImage src, BufferedImage dst)
  {
    Graphics2D gg = dst.createGraphics();
    gg.setRenderingHints(hints);
    gg.drawImage(src, 0, 0, null);
    gg.dispose();
  }
  
  private void copyraster(Raster src, ColorSpace scs, WritableRaster dst,
      					  ColorSpace dcs)
  {
    float[] sbuf = new float[src.getNumBands()];
    
    if (hints.get(RenderingHints.KEY_COLOR_RENDERING) ==
        RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    {
      // use cie for accuracy
      for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
        for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
          dst.setPixel(x, y,
		       dcs.fromCIEXYZ(scs.toCIEXYZ(src.getPixel(x, y, sbuf))));
    }
    else
    {
      // use rgb - it's probably faster
      for (int y = src.getMinY(); y < src.getHeight() + src.getMinY(); y++)
        for (int x = src.getMinX(); x < src.getWidth() + src.getMinX(); x++)
          dst.setPixel(x, y,
		       dcs.fromRGB(scs.toRGB(src.getPixel(x, y, sbuf))));
    }
  }

}
