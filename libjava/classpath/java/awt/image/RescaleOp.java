/* Copyright (C) 2004, 2006  Free Software Foundation

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
 * RescaleOp is a filter that changes each pixel by a scaling factor and offset.
 * 
 * For filtering Rasters, either one scaling factor and offset can be specified,
 * which will be applied to all bands; or a scaling factor and offset can be
 * specified for each band.
 * 
 * For BufferedImages, the scaling may apply to both color and alpha components.
 * If only one scaling factor is provided, or if the number of factors provided
 * equals the number of color components, the scaling is performed on all color
 * components.  Otherwise, the scaling is performed on all components including
 * alpha.  Alpha premultiplication is ignored.
 * 
 * After filtering, if color conversion is necessary, the conversion happens,
 * taking alpha premultiplication into account.
 * 
 * @author Jerry Quinn (jlquinn@optonline.net)
 * @author Francis Kung (fkung@redhat.com)
 */
public class RescaleOp implements BufferedImageOp, RasterOp
{
  private float[] scale;
  private float[] offsets;
  private RenderingHints hints = null;
  
  /**
   * Create a new RescaleOp object using the given scale factors and offsets.
   * 
   * The length of the arrays must be equal to the number of bands (or number of
   * data or color components) of the raster/image that this Op will be used on,
   * otherwise an IllegalArgumentException will be thrown when calling the
   * filter method.
   *  
   * @param scaleFactors an array of scale factors.
   * @param offsets an array of offsets.
   * @param hints any rendering hints to use (can be null).
   * @throws NullPointerException if the scaleFactors or offsets array is null.
   */
  public RescaleOp(float[] scaleFactors,
		   float[] offsets,
		   RenderingHints hints)
  {
    int length = Math.min(scaleFactors.length, offsets.length);
    
    scale = new float[length];
    System.arraycopy(scaleFactors, 0, this.scale, 0, length);
    
    this.offsets = new float[length];
    System.arraycopy(offsets, 0, this.offsets, 0, length);
    
    this.hints = hints;
  }
  
  /**
   * Create a new RescaleOp object using the given scale factor and offset.
   * 
   * The same scale factor and offset will be used on all bands/components.
   *  
   * @param scaleFactor the scale factor to use.
   * @param offset the offset to use.
   * @param hints any rendering hints to use (can be null).
   */
  public RescaleOp(float scaleFactor,
		   float offset,
		   RenderingHints hints)
  {
    scale = new float[]{ scaleFactor };
    offsets = new float[]{offset};
    this.hints = hints;
  }

  /**
   * Returns the scaling factors.  This method accepts an optional array, which
   * will be used to store the factors if not null (this avoids allocating a 
   * new array).  If this array is too small to hold all the scaling factors,
   * the array will be filled and the remaining factors discarded.
   * 
   * @param scaleFactors array to store the scaling factors in (can be null).
   * @return an array of scaling factors.
   */
  public final float[] getScaleFactors(float[] scaleFactors)
  {
    if (scaleFactors == null)
      scaleFactors = new float[scale.length];
    System.arraycopy(scale, 0, scaleFactors, 0, Math.min(scale.length,
                                                         scaleFactors.length));
    return scaleFactors;
  }

  /**
   * Returns the offsets.  This method accepts an optional array, which
   * will be used to store the offsets if not null (this avoids allocating a 
   * new array).  If this array is too small to hold all the offsets, the array 
   * will be filled and the remaining factors discarded.
   * 
   * @param offsets array to store the offsets in (can be null).
   * @return an array of offsets.
   */
  public final float[] getOffsets(float[] offsets)
  {
    if (offsets == null)
      offsets = new float[this.offsets.length];
    System.arraycopy(this.offsets, 0, offsets, 0, Math.min(this.offsets.length,
                                                           offsets.length));
    return offsets;
  }

  /**
   * Returns the number of scaling factors / offsets.
   * 
   * @return the number of scaling factors / offsets.
   */
  public final int getNumFactors()
  {
    return scale.length;
  }

  /* (non-Javadoc)
   * @see java.awt.image.BufferedImageOp#getRenderingHints()
   */
  public final RenderingHints getRenderingHints()
  {
    return hints;
  }

  /**
   * Converts the source image using the scale factors and offsets specified in 
   * the constructor.  The resulting image is stored in the destination image if 
   * one is provided; otherwise a new BufferedImage is created and returned. 
   * 
   * The source image cannot use an IndexColorModel, and the destination image
   * (if one is provided) must have the same size.
   * 
   * If the final value of a sample is beyond the range of the color model, it
   * will be clipped to the appropriate maximum / minimum.
   *
   * @param src The source image.
   * @param dst The destination image.
   * @throws IllegalArgumentException if the rasters and/or color spaces are
   *            incompatible.
   * @return The rescaled image.
   */
  public final BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    // Initial checks
    if (scale.length != 1
        && scale.length != src.getColorModel().getNumComponents()
        && (scale.length != src.getColorModel().getNumColorComponents()))
      throw new IllegalArgumentException("Source image has wrong number of "
                                         + "bands for these scaling factors.");

    if (dst == null)
      dst = createCompatibleDestImage(src, null);
    else if (src.getHeight() != dst.getHeight()
             || src.getWidth() != dst.getWidth())
      throw new IllegalArgumentException("Source and destination images are "
                                         + "different sizes.");

    // Prepare for possible colorspace conversion
    BufferedImage dst2 = dst;
    if (dst.getColorModel().getColorSpace().getType() != src.getColorModel().getColorSpace().getType())
      dst2 = createCompatibleDestImage(src, src.getColorModel());

    // Figure out how many bands to scale
    int numBands = scale.length;
    if (scale.length == 1)
      numBands = src.getColorModel().getNumColorComponents();
    boolean[] bands = new boolean[numBands];
    // this assumes the alpha, if present, is the last band
    Arrays.fill(bands, true);

    // Perform rescaling
    filter(src.getRaster(), dst2.getRaster(), bands);

    // Copy alpha band if needed (ie if it exists and wasn't scaled)
    // NOTE: This assumes the alpha component is the last band!
    if (src.getColorModel().hasAlpha()
        && numBands == src.getColorModel().getNumColorComponents())
      {

        dst2.getRaster().setSamples(0, 0, src.getWidth(), src.getHeight(),
                                    numBands,
                                    src.getRaster().getSamples(0, 0,
                                                               src.getWidth(),
                                                               src.getHeight(),
                                                               numBands,
                                                               (int[]) null));
      }

    // Perform colorspace conversion if needed
    if (dst != dst2)
      new ColorConvertOp(hints).filter(dst2, dst);

    return dst;
  }

  /* (non-Javadoc)
   * @see java.awt.image.RasterOp#filter(java.awt.image.Raster, java.awt.image.WritableRaster)
   */
  public final WritableRaster filter(Raster src, WritableRaster dest)
  {
    // Required sanity checks
    if (scale.length != 1 && scale.length != src.numBands)
      throw new IllegalArgumentException("Number of rasters is incompatible "
                                             + "with the number of scaling "
                                             + "factors provided.");

    if (dest == null)
      dest = src.createCompatibleWritableRaster();
    else if (src.getHeight() != dest.getHeight()
             || src.getWidth() != dest.getWidth())
      throw new IllegalArgumentException("Source and destination rasters are "
                                         + "different sizes.");
    else if (src.numBands != dest.numBands)
      throw new IllegalArgumentException("Source and destination rasters "
                                         + "are incompatible.");

    // Filter all bands
    boolean[] bands = new boolean[src.getNumBands()];
    Arrays.fill(bands, true);
    return filter(src, dest, bands);
  }
  
  /**
   * Perform raster-based filtering on a selected number of bands.
   * 
   * The length of the bands array should equal the number of bands; a true
   * element indicates filtering should happen on the corresponding band, while
   * a false element will skip the band.
   * 
   * The rasters are assumed to be compatible and non-null.
   * 
   * @param src the source raster.
   * @param dest the destination raster.
   * @param bands an array indicating which bands to filter.
   * @throws NullPointerException if any parameter is null.
   * @throws ArrayIndexOutOfBoundsException if the bands array is too small.
   * @return the destination raster.
   */
  private WritableRaster filter(Raster src, WritableRaster dest, boolean[] bands)
  {
    int[] values = new int[src.getHeight() * src.getWidth()];
    float scaleFactor, offset;
    
    // Find max sample value, to be used for clipping later
    int[] maxValue = src.getSampleModel().getSampleSize();
    for (int i = 0; i < maxValue.length; i++)
      maxValue[i] = (int)Math.pow(2, maxValue[i]) - 1;
    
    // TODO: can this be optimized further?
    // Filter all samples of all requested bands
    for (int band = 0; band < bands.length; band++)
      if (bands[band])
        {
          values = src.getSamples(src.getMinX(), src.getMinY(), src.getWidth(),
                                  src.getHeight(), band, values);

          if (scale.length == 1)
            {
              scaleFactor = scale[0];
              offset = offsets[0];
            }
          else
            {
              scaleFactor = scale[band];
              offset = offsets[band];
            }

          for (int i = 0; i < values.length; i++)
            {
              values[i] = (int) (values[i] * scaleFactor + offset);

              // Clip if needed
              if (values[i] < 0)
                values[i] = 0;
              if (values[i] > maxValue[band])
                values[i] = maxValue[band];
            }

          dest.setSamples(dest.getMinX(), dest.getMinY(), dest.getWidth(),
                          dest.getHeight(), band, values);
        }
    
    return dest;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.awt.image.BufferedImageOp#createCompatibleDestImage(java.awt.image.BufferedImage,
   *      java.awt.image.ColorModel)
   */
  public BufferedImage createCompatibleDestImage(BufferedImage src,
						 ColorModel dstCM)
  {
    if (dstCM == null)
      return new BufferedImage(src.getWidth(), src.getHeight(), src.getType());
    
    return new BufferedImage(dstCM,
                             src.getRaster().createCompatibleWritableRaster(),
                             src.isAlphaPremultiplied(), null);
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
  public final Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null)
      dst = (Point2D) src.clone();
    else
      dst.setLocation(src);
    
    return dst;
  }

}
