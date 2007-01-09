/* ColorConvertOp.java --
   Copyright (C) 2004, 2006  Free Software Foundation

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

import gnu.java.awt.Buffers;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.color.ColorSpace;
import java.awt.color.ICC_ColorSpace;
import java.awt.color.ICC_Profile;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * ColorConvertOp is a filter for converting images or rasters between
 * colorspaces, either through a sequence of colorspaces or just from source to 
 * destination.
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
  private RenderingHints hints;
  private ICC_Profile[] profiles = null;
  private ColorSpace[] spaces;
  

  /**
   * Convert a BufferedImage through a ColorSpace.
   * 
   * Objects created with this constructor can be used to convert
   * BufferedImage's to a destination ColorSpace.  Attempts to convert Rasters
   * with this constructor will result in an IllegalArgumentException when the
   * filter(Raster, WritableRaster) method is called.  
   * 
   * @param cspace The target color space.
   * @param hints Rendering hints to use in conversion, if any (may be null)
   * @throws NullPointerException if the ColorSpace is null.
   */
  public ColorConvertOp(ColorSpace cspace, RenderingHints hints)
  {
    if (cspace == null)
      throw new NullPointerException();
    spaces = new ColorSpace[]{cspace};
    this.hints = hints;
  }
  
  /**
   * Convert from a source colorspace to a destination colorspace.
   * 
   * This constructor takes two ColorSpace arguments as the source and
   * destination color spaces.  It is usually used with the
   * filter(Raster, WritableRaster) method, in which case the source colorspace 
   * is assumed to correspond to the source Raster, and the destination 
   * colorspace with the destination Raster.
   * 
   * If used with BufferedImages that do not match the source or destination 
   * colorspaces specified here, there is an implicit conversion from the 
   * source image to the source ColorSpace, or the destination ColorSpace to 
   * the destination image.
   * 
   * @param srcCspace The source ColorSpace.
   * @param dstCspace The destination ColorSpace.
   * @param hints Rendering hints to use in conversion, if any (may be null).
   * @throws NullPointerException if any ColorSpace is null.
   */
  public ColorConvertOp(ColorSpace srcCspace, ColorSpace dstCspace,
			RenderingHints hints)
  {
    if (srcCspace == null || dstCspace == null)
      throw new NullPointerException();
    spaces = new ColorSpace[]{srcCspace, dstCspace};
    this.hints = hints;     
  }

  /**
   * Convert from a source colorspace to a destinatino colorspace.
   * 
   * This constructor builds a ColorConvertOp from an array of ICC_Profiles.
   * The source will be converted through the sequence of color spaces
   * defined by the profiles.  If the sequence of profiles doesn't give a
   * well-defined conversion, an IllegalArgumentException is thrown.
   * 
   * If used with BufferedImages that do not match the source or destination 
   * colorspaces specified here, there is an implicit conversion from the 
   * source image to the source ColorSpace, or the destination ColorSpace to 
   * the destination image.
   * 
   * For Rasters, the first and last profiles must have the same number of
   * bands as the source and destination Rasters, respectively.  If this is
   * not the case, or there fewer than 2 profiles, an IllegalArgumentException
   * will be thrown. 
   * 
   * @param profiles An array of ICC_Profile's to convert through.
   * @param hints Rendering hints to use in conversion, if any (may be null).
   * @throws NullPointerException if the profile array is null.
   * @throws IllegalArgumentException if the array is not a well-defined
   *            conversion.
   */
  public ColorConvertOp(ICC_Profile[] profiles, RenderingHints hints)
  {
    if (profiles == null)
      throw new NullPointerException();
    
    this.hints = hints; 
    this.profiles = profiles;
    
    // Create colorspace array with space for src and dest colorspace
    // Note that the ICC_ColorSpace constructor will throw an
    // IllegalArgumentException if the profile is invalid; thus we check
    // for a "well defined conversion"
    spaces = new ColorSpace[profiles.length];
    for (int i = 0; i < profiles.length; i++)
      spaces[i] = new ICC_ColorSpace(profiles[i]);
  }
  
  /**
   * Convert from source color space to destination color space.
   * 
   * Only valid for BufferedImage objects, this Op converts from the source
   * image's color space to the destination image's color space.
   * 
   * The destination in the filter(BufferedImage, BufferedImage) method cannot 
   * be null for this operation, and it also cannot be used with the
   * filter(Raster, WritableRaster) method.
   * 
   * @param hints Rendering hints to use in conversion, if any (may be null).
   */
  public ColorConvertOp(RenderingHints hints)
  {
    this.hints = hints;
    spaces = new ColorSpace[0];
  }
  
  /**
   * Converts the source image using the conversion path specified in the
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
   * @return The transformed image.
   */
  public final BufferedImage filter(BufferedImage src, BufferedImage dst)
  {
    // TODO: The plan is to create a scanline buffer for intermediate buffers.
    // For now we just suck it up and create intermediate buffers.
    
    if (dst == null && spaces.length == 0)
      throw new IllegalArgumentException("Not enough color space information "
                                         + "to complete conversion.");

    if (dst != null
        && (src.getHeight() != dst.getHeight() || src.getWidth() != dst.getWidth()))
      throw new IllegalArgumentException("Source and destination images have "
                                         + "different dimensions");

    // Make sure input isn't premultiplied by alpha
    if (src.isAlphaPremultiplied())
      {
        BufferedImage tmp = createCompatibleDestImage(src, src.getColorModel());
        copyimage(src, tmp);
        tmp.coerceData(false);
        src = tmp;
      }

    // Convert through defined intermediate conversions
    BufferedImage tmp;
    for (int i = 0; i < spaces.length; i++)
      {
        if (src.getColorModel().getColorSpace().getType() != spaces[i].getType())
          {
            tmp = createCompatibleDestImage(src,
                                            createCompatibleColorModel(src,
                                                                       spaces[i]));
            copyimage(src, tmp);
            src = tmp;
          }
      }

    // No implicit conversion to destination type needed; return result from the
    // last intermediate conversions (which was left in src)
    if (dst == null)
      dst = src;

    // Implicit conversion to destination image's color space
    else
      copyimage(src, dst);

    return dst;
  }

  /**
   * Converts the source raster using the conversion path specified in the
   * constructor.  The resulting raster is stored in the destination raster if
   * one is provided; otherwise a new WritableRaster is created and returned.
   * 
   * This operation is not valid with every constructor of this class; see
   * the constructors for details.  Further, the source raster must have the
   * same number of bands as the source ColorSpace, and the destination raster
   * must have the same number of bands as the destination ColorSpace.
   * 
   * The source and destination raster (if one is supplied) must also have the
   * same dimensions.
   *
   * @param src The source raster.
   * @param dest The destination raster.
   * @throws IllegalArgumentException if the rasters and/or color spaces are
   *            incompatible.
   * @return The transformed raster.
   */
  public final WritableRaster filter(Raster src, WritableRaster dest)
  {
    // Various checks to ensure that the rasters and color spaces are compatible
    if (spaces.length < 2)
      throw new IllegalArgumentException("Not enough information about " +
            "source and destination colorspaces.");

    if (spaces[0].getNumComponents() != src.getNumBands()
        || (dest != null && spaces[spaces.length - 1].getNumComponents() != dest.getNumBands()))
      throw new IllegalArgumentException("Source or destination raster " +
            "contains the wrong number of bands.");

    if (dest != null
        && (src.getHeight() != dest.getHeight() || src.getWidth() != dest.getWidth()))
      throw new IllegalArgumentException("Source and destination rasters " +
            "have different dimensions");

    // Need to iterate through each color space.
    // spaces[0] corresponds to the ColorSpace of the source raster, and
    // spaces[spaces.length - 1] corresponds to the ColorSpace of the
    // destination, with any number (or zero) of intermediate conversions.

    for (int i = 0; i < spaces.length - 2; i++)
      {
        WritableRaster tmp = createCompatibleDestRaster(src, spaces[i + 1],
                                                        false,
                                                        src.getTransferType());
        copyraster(src, spaces[i], tmp, spaces[i + 1]);
        src = tmp;
      }

    // The last conversion is done outside of the loop so that we can
    // use the dest raster supplied, instead of creating our own temp raster
    if (dest == null)
      dest = createCompatibleDestRaster(src, spaces[spaces.length - 1], false,
                                        DataBuffer.TYPE_BYTE);
    copyraster(src, spaces[spaces.length - 2], dest, spaces[spaces.length - 1]);

    return dest;
  }

  /**
   * Creates an empty BufferedImage with the size equal to the source and the
   * correct number of bands for the conversion defined in this Op. The newly 
   * created image is created with the specified ColorModel, or if no ColorModel
   * is supplied, an appropriate one is chosen.
   *
   * @param src The source image.
   * @param dstCM A color model for the destination image (may be null).
   * @throws IllegalArgumentException if an appropriate colormodel cannot be
   *            chosen with the information given.
   * @return The new compatible destination image.
   */
  public BufferedImage createCompatibleDestImage(BufferedImage src,
                         ColorModel dstCM)
  {
    if (dstCM == null && spaces.length == 0)
      throw new IllegalArgumentException("Don't know the destination " +
            "colormodel");

    if (dstCM == null)
      {
        dstCM = createCompatibleColorModel(src, spaces[spaces.length - 1]);
      }

    return new BufferedImage(dstCM,
                             createCompatibleDestRaster(src.getRaster(),
                                                        dstCM.getColorSpace(),
                                                        src.getColorModel().hasAlpha,
                                                        dstCM.getTransferType()),
                             src.isPremultiplied, null);
  }
  
  /**
   * Creates a new WritableRaster with the size equal to the source and the
   * correct number of bands.
   * 
   * Note, the new Raster will always use a BYTE storage size, regardless of
   * the color model or defined destination; this is for compatibility with
   * the reference implementation.
   *
   * @param src The source Raster.
   * @throws IllegalArgumentException if there isn't enough colorspace
   *            information to create a compatible Raster.
   * @return The new compatible destination raster.
   */
  public WritableRaster createCompatibleDestRaster(Raster src)
  {
    if (spaces.length < 2)
      throw new IllegalArgumentException("Not enough destination colorspace " +
            "information");

    // Create a new raster with the last ColorSpace in the conversion
    // chain, and with no alpha (implied)
    return createCompatibleDestRaster(src, spaces[spaces.length-1], false,
                                      DataBuffer.TYPE_BYTE);
  }

  /**
   * Returns the array of ICC_Profiles used to create this Op, or null if the
   * Op was created using ColorSpace arguments.
   * 
   * @return The array of ICC_Profiles, or null.
   */
  public final ICC_Profile[] getICC_Profiles()
  {
    return profiles;
  }

  /**
   * Returns the rendering hints for this op.
   * 
   * @return The rendering hints for this Op, or null.
   */
  public final RenderingHints getRenderingHints()
  {
    return hints;
  }

  /**
   * Returns the corresponding destination point for a source point.
   * Because this is not a geometric operation, the destination and source
   * points will be identical.
   * 
   * @param src The source point.
   * @param dst The transformed destination point.
   * @return The transformed destination point.
   */
  public final Point2D getPoint2D(Point2D src, Point2D dst)
  {
    if (dst == null)
      return (Point2D)src.clone();
    
    dst.setLocation(src);
    return dst;
  }

  /**
   * Returns the corresponding destination boundary of a source boundary.
   * Because this is not a geometric operation, the destination and source
   * boundaries will be identical.
   * 
   * @param src The source boundary.
   * @return The boundaries of the destination.
   */
  public final Rectangle2D getBounds2D(BufferedImage src)
  {
    return src.getRaster().getBounds();
  }

  /**
   * Returns the corresponding destination boundary of a source boundary.
   * Because this is not a geometric operation, the destination and source
   * boundaries will be identical.
   * 
   * @param src The source boundary.
   * @return The boundaries of the destination.
   */
  public final Rectangle2D getBounds2D(Raster src)
  {
    return src.getBounds();
  }

  /**
   * Copy a source image to a destination image, respecting their colorspaces 
   * and performing colorspace conversions if necessary.  
   * 
   * @param src The source image.
   * @param dst The destination image.
   */
  private void copyimage(BufferedImage src, BufferedImage dst)
  {
    // This is done using Graphics2D in order to respect the rendering hints.
    Graphics2D gg = dst.createGraphics();
    
    // If no hints are set there is no need to call
    // setRenderingHints on the Graphics2D object.
    if (hints != null)
      gg.setRenderingHints(hints);
    
    gg.drawImage(src, 0, 0, null);
    gg.dispose();
  }
  
  /**
   * Copy a source raster to a destination raster, performing a colorspace
   * conversion between the two.  The conversion will respect the
   * KEY_COLOR_RENDERING rendering hint if one is present.
   * 
   * @param src The source raster.
   * @param scs The colorspace of the source raster.
   * @dst The destination raster.
   * @dcs The colorspace of the destination raster.
   */
  private void copyraster(Raster src, ColorSpace scs, WritableRaster dst, ColorSpace dcs)
  {
    float[] sbuf = new float[src.getNumBands()];
    
    if (hints != null
        && hints.get(RenderingHints.KEY_COLOR_RENDERING) ==
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

  /**
   * This method creates a color model with the same colorspace and alpha
   * settings as the source image.  The created color model will always be a
   * ComponentColorModel and have a BYTE transfer type.
   * 
   * @param img The source image.
   * @param cs The ColorSpace to use.
   * @return A color model compatible with the source image.
   */ 
  private ColorModel createCompatibleColorModel(BufferedImage img, ColorSpace cs)
  {
    // The choice of ComponentColorModel and DataBuffer.TYPE_BYTE is based on
    // Mauve testing of the reference implementation.
    return new ComponentColorModel(cs,
                                   img.getColorModel().hasAlpha(), 
                                   img.isAlphaPremultiplied(),
                                   img.getColorModel().getTransparency(),
                                   DataBuffer.TYPE_BYTE);    
  }

  /**
   * This method creates a compatible Raster, given a source raster, colorspace,
   * alpha value, and transfer type.
   * 
   * @param src The source raster.
   * @param cs The ColorSpace to use.
   * @param hasAlpha Whether the raster should include a component for an alpha.
   * @param transferType The size of a single data element.
   * @return A compatible WritableRaster. 
   */
  private WritableRaster createCompatibleDestRaster(Raster src, ColorSpace cs,
                                                    boolean hasAlpha,
                                                    int transferType)
  {
    // The use of a PixelInterleavedSampleModel weas determined using mauve
    // tests, based on the reference implementation
    
    int numComponents = cs.getNumComponents();
    if (hasAlpha)
      numComponents++;
    
    int[] offsets = new int[numComponents];
    for (int i = 0; i < offsets.length; i++)
      offsets[i] = i;

    DataBuffer db = Buffers.createBuffer(transferType,
                                         src.getWidth() * src.getHeight() * numComponents,
                                         1);
    return new WritableRaster(new PixelInterleavedSampleModel(transferType,
                                                              src.getWidth(),
                                                              src.getHeight(),
                                                              numComponents,
                                                              numComponents * src.getWidth(),
                                                              offsets),
                              db, new Point(src.getMinX(), src.getMinY()));
  }
}
