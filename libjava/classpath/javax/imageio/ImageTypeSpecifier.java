/* ImageTypeSpecifier.java --
   Copyright (C) 2004  Free Software Foundation, Inc.

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


package javax.imageio;

import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.DataBuffer;
import java.awt.image.BandedSampleModel;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.IndexColorModel;
import java.awt.image.MultiPixelPackedSampleModel;
import java.awt.image.PixelInterleavedSampleModel;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;

/**
 * ImageTypeSpecifier store the color and sample models associated
 * with an IIOImage.
 */
public class ImageTypeSpecifier
{
  /**
   * The image's color model.
   */
  protected ColorModel colorModel;

  /**
   * The image's sample model.
   */
  protected SampleModel sampleModel;

  /**
   * Construct an image type specifier with the given models.
   *
   * @param colorModel the color model
   * @param sampleModel the sample model
   *
   * @exception IllegalArgumentException if either model argument is
   * null
   * @exception IllegalArgumentException if the models are
   * incompatible with one another
   */
  public ImageTypeSpecifier(ColorModel colorModel, SampleModel sampleModel)
  {
    if (colorModel == null)
      throw new IllegalArgumentException("colorModel may not be null");

    if (sampleModel == null)
      throw new IllegalArgumentException("sampleModel may not be null");

    if (!colorModel.isCompatibleSampleModel(sampleModel))
      throw new IllegalArgumentException
        ("sample Model not compatible with colorModel");
    
    this.colorModel = colorModel;
    this.sampleModel = sampleModel;
  }

  /**
   * Construct an image type specifier that describes the given
   * rendered image.
   *
   * @param image a rendered image
   *
   * @exception IllegalArgumentException if image is null
   */
  public ImageTypeSpecifier(RenderedImage image)
  {
    if (image == null)
      throw new IllegalArgumentException("image may not be null");
    
    this.colorModel = image.getColorModel();
    this.sampleModel = image.getSampleModel();
  }

  /**
   * Create an image type specifier for a banded image using a
   * component color model and a banded sample model.
   *
   * @param colorSpace the color space
   * @param bankIndices the bank indices at which each band will be
   * stored
   * @param bandOffsets the starting band offset for each band within
   * its bank
   * @param dataType the data type, a DataBuffer constant
   * @param hasAlpha true if this image type specifier should have an
   * alpha component, false otherwise
   * @param isAlphaPremultiplied true if other color components should
   * be premultiplied by the alpha component, false otherwise
   *
   * @return a banded image type specifier
   *
   * @exception IllegalArgumentException if any of colorSpace,
   * bankIndices or bankOffsets is null
   * @exception IllegalArgumentException if bankIndices and
   * bankOffsets differ in length
   * @excpetion IllegalArgumentException if the number of color space
   * components, including the alpha component if requested, is
   * different from bandOffsets.length
   * @exception if dataType is not a valid DataBuffer constant
   */
  public static ImageTypeSpecifier createBanded (ColorSpace colorSpace,
                                                 int[] bankIndices,
                                                 int[] bankOffsets,
                                                 int dataType,
                                                 boolean hasAlpha,
                                                 boolean isAlphaPremultiplied)
  {
    if (colorSpace == null || bankIndices == null || bankOffsets == null)
      throw new IllegalArgumentException ("null argument");

    if (bankIndices.length != bankOffsets.length)
      throw new IllegalArgumentException ("array lengths differ");

    if (bankOffsets.length != (colorSpace.getNumComponents() + (hasAlpha ? 1 : 0)))
      throw new IllegalArgumentException ("invalid bankOffsets length");

    return new ImageTypeSpecifier (new ComponentColorModel (colorSpace,
                                                            hasAlpha,
                                                            isAlphaPremultiplied,
                                                            hasAlpha ? Transparency.TRANSLUCENT : Transparency.OPAQUE,
                                                            dataType),
                                   new BandedSampleModel (dataType, 1, 1, 1,
                                                          bankIndices,
                                                          bankOffsets));
  }

  /**
   * Create a buffered image with the given dimensions using that has
   * the characteristics specified by this image type specifier.
   *
   * @param the width of the buffered image, in pixels
   * @param the height of the buffered image, in pixels
   *
   * @return a buffered image
   *
   * @exception IllegalArgumentException if either width or height is
   * less than or equal to zero
   * @exception IllegalArgumentException if width * height is greater
   * than Integer.MAX_VALUE or if the storage required is greater than
   * Integer.MAX_VALUE
   */
  public BufferedImage createBufferedImage (int width, int height)
  {
    if (width <= 0 || height <= 0)
      throw new IllegalArgumentException ("dimension <= 0");

    // test for overflow
    if (width * height < Math.min (width, height))
      throw new IllegalArgumentException ("width * height > Integer.MAX_VALUE");

    if (width * height * sampleModel.getNumBands() < Math.min (width, height))
      throw new IllegalArgumentException ("storage required >"
                                          + " Integer.MAX_VALUE");

    // FIXME: this is probably wrong:
    return new BufferedImage (width, height, BufferedImage.TYPE_INT_RGB);
  }

  /**
   * Create an image type specifier that describes the given buffered
   * image type.
   *
   * @param bufferedImageType the buffered image type to represent
   * with the returned image type specifier
   *
   * @return a new image type specifier
   *
   * @exception IllegalArgumentException if bufferedImageType is not a
   * BufferedImage constant or is BufferedImage.TYPE_CUSTOM
   */
  public static ImageTypeSpecifier createFromBufferedImageType (int bufferedImageType)
  {
    if (bufferedImageType <= BufferedImage.TYPE_CUSTOM
        || bufferedImageType > BufferedImage.TYPE_BYTE_INDEXED)
      throw new IllegalArgumentException ("invalid buffered image type");

    return new ImageTypeSpecifier (new BufferedImage (1, 1, bufferedImageType));
  }

  /**
   * Create an image type specifier that describes the given rendered
   * image's type.
   *
   * @param image the rendered image
   *
   * @return a new image type specifier
   *
   * @exception IllegalArgumentException if image is null
   */
  public static ImageTypeSpecifier createFromRenderedImage (RenderedImage image)
  {
    if (image == null)
      throw new IllegalArgumentException ("image null");

    return new ImageTypeSpecifier (image);
  }

  /**
   * Create a grayscale image type specifier, given the number of
   * bits, data type and whether or not the data is signed.
   *
   * @param bits the number of bits used to specify a greyscale value
   * @param dataType a DataBuffer type constant
   * @param isSigned true if this type specifier should support
   * negative values, false otherwise
   *
   * @return a greyscal image type specifier
   *
   * @exception IllegalArgumentException if bits is not 1, 2, 4, 8 or 16
   * @exception IllegalArgumentException if dataType is not
   * DataBuffer.TYPE_BYTE, DataBuffer.TYPE_SHORT or
   * DataBuffer.TYPE_USHORT
   * @exception if bits is larger than the number of bits in the given
   * data type
   */
  public static ImageTypeSpecifier createGrayscale (int bits, int dataType, boolean isSigned)
  {
    return createGrayscale (bits, dataType, isSigned, false);
  }

  /**
   * Create a grayscale image type specifier, given the number of
   * bits, data type and whether or not the data is signed.
   *
   * @param bits the number of bits used to specify a greyscale value
   * @param dataType a DataBuffer type constant
   * @param isSigned true if this type specifier should support
   * negative values, false otherwise
   *
   * @return a greyscal image type specifier
   *
   * @exception IllegalArgumentException if bits is not 1, 2, 4, 8 or
   * 16
   * @exception IllegalArgumentException if dataType is not
   * DataBuffer.TYPE_BYTE, DataBuffer.TYPE_SHORT or
   * DataBuffer.TYPE_USHORT
   * @exception if bits is larger than the number of bits in the given
   * data type
   */
  public static ImageTypeSpecifier createGrayscale (int bits, int dataType,
                                                    boolean isSigned,
                                                    boolean isAlphaPremultiplied)
  {
    if (bits != 1 && bits != 2 && bits != 4 && bits != 8 && bits != 16)
      throw new IllegalArgumentException ("invalid bit size");

    if (dataType != DataBuffer.TYPE_BYTE && dataType != DataBuffer.TYPE_SHORT
        && dataType != DataBuffer.TYPE_USHORT)
      throw new IllegalArgumentException ("invalid data type");

    if (dataType == DataBuffer.TYPE_BYTE && bits > 8)
      throw new IllegalArgumentException ("number of bits too large for data type");

    // FIXME: this is probably wrong:
    return new ImageTypeSpecifier (new DirectColorModel (bits, 0xff, 0x0,
                                                         0x0, 0xff),
                                   new MultiPixelPackedSampleModel (dataType,
                                                                    1, 1,
                                                                    bits));
  }

  /**
   * Return an image type specifier for an image that uses an indexed
   * colour model where each colour value has the specified number of
   * bits and type and where the colour tables are those given.
   *
   * @param redLUT the red index values
   * @param greenLUT the green index values
   * @param blueLUT the blue index values
   * @param alphaLUT the alpha index values
   * @param bits the number of bits per index value
   * @param dataType the type of each index value
   *
   * @return an indexed image type specifier
   *
   * @exception IllegalArgumentException if any of the colour arrays,
   * not including alphaLUT, is null
   * @exception IllegalArgumentException if bits is not 1, 2, 4, 8 or
   * 16
   * @exception IllegalArgumentException if dataType is not
   * DataBuffer.TYPE_BYTE, DataBuffer.TYPE_SHORT or
   * DataBuffer.TYPE_USHORT
   * @exception if bits is larger than the number of bits in the given
   * data type
   */
  public static ImageTypeSpecifier createIndexed (byte[] redLUT,
						  byte[] greenLUT,
						  byte[] blueLUT,
						  byte[] alphaLUT,
						  int bits,
						  int dataType)
  {
    if (redLUT == null || greenLUT == null || blueLUT == null)
      throw new IllegalArgumentException ("null colour table");

    if (bits != 1 && bits != 2 && bits != 4 && bits != 8 && bits != 16)
      throw new IllegalArgumentException ("invalid bit size");

    if (dataType != DataBuffer.TYPE_BYTE && dataType != DataBuffer.TYPE_SHORT
        && dataType != DataBuffer.TYPE_USHORT)
      throw new IllegalArgumentException ("invalid data type");

    if (dataType == DataBuffer.TYPE_BYTE && bits > 8)
      throw new IllegalArgumentException ("number of bits too large for data type");

    // FIXME: this is probably wrong:
    return new ImageTypeSpecifier (new IndexColorModel (bits, redLUT.length,
                                                        redLUT, greenLUT, blueLUT,
                                                        alphaLUT),
                                   new MultiPixelPackedSampleModel (dataType,
                                                                    1, 1,
                                                                    bits));
  }

  /**
   * Create an image type specifier that uses a component colour model
   * and a pixel interleaved sample model.  Each pixel component will
   * be stored in a separate value of the given data type.
   *
   * @param colorSpace the colour space used by the colour model
   * @param bandOffsets the starting band offset for each band within
   * its bank
   * @param dataType the type of each pixel value
   * @param hasAlpha true if an alpha channel should be specified,
   * false otherwise
   * @param isAlphaPremultiplied true if other colour channels should
   * be premultiplied by the alpha value, false otherwise
   *
   * @return an interleaved image type specifier
   *
   * @exception IllegalArgumentException if either colorSpace or
   * bandOffsets is null
   * @excpetion IllegalArgumentException if the number of color space
   * components, including the alpha component if requested, is
   * different from bandOffsets.length
   * @exception if dataType is not a valid DataBuffer constant
   */
  public static ImageTypeSpecifier createInterleaved (ColorSpace colorSpace,
                                                      int[] bandOffsets,
                                                      int dataType,
                                                      boolean hasAlpha,
                                                      boolean isAlphaPremultiplied)
  {
    if (colorSpace == null || bandOffsets == null)
      throw new IllegalArgumentException ("null argument");

    if (bandOffsets.length != (colorSpace.getNumComponents() + (hasAlpha ? 1 : 0)))
      throw new IllegalArgumentException ("invalid bankOffsets length");

    return new ImageTypeSpecifier (new ComponentColorModel (colorSpace,
                                                            hasAlpha,
                                                            isAlphaPremultiplied,
                                                            hasAlpha ? Transparency.TRANSLUCENT : Transparency.OPAQUE,
                                                            dataType),
                                   new PixelInterleavedSampleModel (dataType, 1, 1, 1, 1,
                                                                    bandOffsets));
  }

  /**
   * Create an image type specifier using a direct color model and a
   * packed sample model.  All pixel components will be packed into
   * one value of the given data type.
   *
   * @param colorSpace the color space to use in the color model
   * @param redMask the bitmask for the red bits 
   * @param greenMask the bitmask for the green bits 
   * @param blueMask the bitmask for the blue bits 
   * @param alphaMask the bitmask for the alpha bits 
   * @param transferType the data type used to store pixel values
   * @param isAlphaPremultiplied true if other colour channels should
   * be premultiplied by the alpha value, false otherwise
   *
   * @return a packed image type specifier
   *
   * @exception IllegalArgumentException if colorSpace is null
   * @exception IllegalArgumentException if colorSpace does not have
   * type ColorSpace.TYPE_RGB
   * @exception IllegalArgumentException if all masks are 0
   * @exception IllegalArgumentException if dataType is not
   * DataBuffer.TYPE_BYTE, DataBuffer.TYPE_SHORT or
   * DataBuffer.TYPE_INT
   */
  public static ImageTypeSpecifier createPacked (ColorSpace colorSpace,
                                                 int redMask,
                                                 int greenMask,
                                                 int blueMask,
                                                 int alphaMask,
                                                 int transferType,
                                                 boolean isAlphaPremultiplied)
  {
    if (colorSpace == null)
      throw new IllegalArgumentException ("null color space");

    if (colorSpace.getType() != ColorSpace.TYPE_RGB)
      throw new IllegalArgumentException ("invalid color space type");

    if (redMask == 0 && greenMask == 0 && blueMask == 0 && alphaMask == 0)
      throw new IllegalArgumentException ("no non-zero mask");

    if (transferType != DataBuffer.TYPE_BYTE && transferType != DataBuffer.TYPE_USHORT
        && transferType != DataBuffer.TYPE_INT)
      throw new IllegalArgumentException ("invalid data type");

    // Assume DataBuffer.TYPE_BYTE.
    int numBits = 8;

    if (transferType == DataBuffer.TYPE_SHORT)
      numBits = 16;
    else if (transferType == DataBuffer.TYPE_INT)
      numBits = 32;

    return new ImageTypeSpecifier (new DirectColorModel (colorSpace,
                                                         numBits,
                                                         redMask,
                                                         greenMask,
                                                         blueMask,
                                                         alphaMask,
                                                         isAlphaPremultiplied,
                                                         transferType),
                                   new MultiPixelPackedSampleModel (transferType,
                                                                    1, 1, numBits));
  }

  /**
   * Get the number of bits per sample in the given band.
   *
   * @param band the band from which to get the number of bits
   *
   * @return the number of bits in the given band
   *
   * @exception IllegalArgumentException if band is out-of-bounds
   */
  public int getBitsPerBand (int band)
  {
    if (band < 0 || band > sampleModel.getNumBands())
      throw new IllegalArgumentException ("band out-of-bounds");

    return sampleModel.getSampleSize (band);
  }

  /**
   * Get the buffered image constant specified by this image type
   * specifier.
   *
   * @return a buffered image constant
   */
  public int getBufferedImageType ()
  {
    // FIXME:
    return BufferedImage.TYPE_INT_RGB;
  }

  /**
   * Create a sample model that is compatible with the one specified
   * by this image type specifier, with the given dimensions.
   *
   * @param width the width of the returned sample model
   * @param height the height of the returned sample model
   *
   * @return a sample model compatible with the one in this image type
   * specifier, with the given dimensions
   *
   * @exception IllegalArgumentException if either width or height is
   * less than or equal to 0
   * @exception IllegalArgumentException if width * height is greater
   * than Intere.MAX_VALUE
   */
  public SampleModel getSampleModel (int width, int height)
  {
    if (width <= 0 || height <= 0)
      throw new IllegalArgumentException ("invalid dimension");

    // test for overflow
    if (width * height < Math.min (width, height))
      throw new IllegalArgumentException ("width * height > Integer.MAX_VALUE");

    return sampleModel.createCompatibleSampleModel (width, height);
  }

  /**
   * Get the color model specified by this image type specifier.
   *
   * @return the color model
   */
  public ColorModel getColorModel()
  {
    return colorModel;
  }

  /**
   * Get the number of bands specified by this image type specifier's
   * sample model.
   *
   * @return the number of bands in the sample model
   */
  public int getNumBands()
  {
    return sampleModel.getNumBands();
  }

  /**
   * Get the number of components specified by this image type
   * specifier's color model.
   *
   * @return the number of color components per pixel
   */
  public int getNumComponents()
  {
    return colorModel.getNumComponents();
  }

  /**
   * Get the sample model specified by this image type specifier.
   *
   * @return the sample model
   */
  public SampleModel getSampleModel()
  {
    return sampleModel;
  }
}
