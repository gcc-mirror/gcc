/* Copyright (C) 1999, 2000, 2002, 2003  Free Software Foundation

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

import java.util.Arrays;
import java.awt.Point;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import gnu.java.awt.Buffers;

/**
 * A color model operates with colors in several formats:
 *
 * <ul>
 * <li>normalized: component samples are in range [0.0, 1.0].</li>
 *
 * <li>color model pixel value: all the color component samples for a
 * sigle pixel packed/encoded in a way natural for the color
 * model.</li>
 *
 * <li>color model pixel int value: only makes sense if the natural
 * encoding of a single pixel can fit in a single int value.</li>
 *
 * <li>array of transferType containing a single pixel: the pixel is
 * encoded in the natural way of the color model, taking up as many
 * array elements as needed.</li>
 *
 * <li>sRGB pixel int value: a pixel in sRGB color space, encoded in
 * default 0xAARRGGBB format, assumed not alpha premultiplied.</li>
 * 
 * <li>single [0, 255] scaled int samples from default sRGB color
 * space. These are always assumed to be alpha non-premultiplied.</li>
 *
 * <li>arrays of unnormalized component samples of single pixel: these
 * samples are scaled and multiplied according to the color model, but
 * is otherwise not packed or encoded. Each element of the array is one
 * separate component sample. The color model only operate on the
 * components from one pixel at a time, but using offsets, allows
 * manipulation of arrays that contain the components of more than one
 * pixel.</li>
 *
 * </ul>
 *
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 * @author C. Brian Jones <cbj@gnu.org>
 */
public abstract class ColorModel implements Transparency
{
  protected int pixel_bits;
  protected int transferType;

  int[] bits;
  ColorSpace cspace;
  int transparency;
  boolean hasAlpha;
  boolean isAlphaPremultiplied;
    
  static int[] nArray(int value, int times)
  {
    int[] array = new int[times];
    java.util.Arrays.fill(array, value);
    return array;
  }

  static byte[] nArray(byte value, int times)
  {
    byte[] array = new byte[times];
    java.util.Arrays.fill(array, value);
    return array;
  } 

  /**
   * Constructs the default color model.  The default color model 
   * can be obtained by calling <code>getRGBdefault</code> of this
   * class.
   * @param b the number of bits wide used for bit size of pixel values
   */
  public ColorModel(int bits)
  {
    this(bits * 4, // total bits, sRGB, four channels
	 nArray(bits, 4), // bits for each channel
	 ColorSpace.getInstance(ColorSpace.CS_sRGB), // sRGB
	 true, // has alpha
	 false, // not premultiplied
	 TRANSLUCENT,
	 Buffers.smallestAppropriateTransferType(bits * 4));
  }

  /**
   * Constructs a ColorModel that translates pixel values to
   * color/alpha components.
   *
   * @exception IllegalArgumentException If the length of the bit array is less
   * than the number of color or alpha components in this ColorModel, or if the
   * transparency is not a valid value, or if the sum of the number of bits in
   * bits is less than 1 or if any of the elements in bits is less than 0.
   */
  protected ColorModel(int pixel_bits, int[] bits, ColorSpace cspace,
		       boolean hasAlpha, boolean isAlphaPremultiplied,
		       int transparency, int transferType)
  {
    int bits_sum = 0;
    for (int i = 0; i < bits.length; i++)
      {
        if (bits [i] < 0)
          throw new IllegalArgumentException ();

        bits_sum |= bits [i];
      }
    
    if ((bits.length < cspace.getNumComponents())
        || (bits_sum < 1))
      throw new IllegalArgumentException ();

    this.pixel_bits = pixel_bits;
    this.bits = bits;
    this.cspace = cspace;
    this.hasAlpha = hasAlpha;
    this.isAlphaPremultiplied = isAlphaPremultiplied;
    this.transparency = transparency;
    this.transferType = transferType;
  }

  public void finalize()
  {
    // Do nothing here.
  }

  /**
   * Returns the default color model which in Sun's case is an instance
   * of <code>DirectColorModel</code>.
   */
  public static ColorModel getRGBdefault()
  {
    return new DirectColorModel(32, 0xff0000, 0xff00, 0xff, 0xff000000);
  }

  public final boolean hasAlpha()
  {
    return hasAlpha;
  }

  public final boolean isAlphaPremultiplied()
  {
    return isAlphaPremultiplied;
  }

  /**
   * Get get number of bits wide used for the bit size of pixel values
   */
  public int getPixelSize()
  {
    return pixel_bits;
  }
    
  public int getComponentSize(int componentIdx)
  {
    return bits[componentIdx];
  }
    
  public int[] getComponentSize()
  {
    return bits;
  }

  public int getTransparency()
  {
    return transparency;
  }

  public int getNumComponents()
  {
    return getNumColorComponents() + (hasAlpha ? 1 : 0);
  }

  public int getNumColorComponents()
  {
    return cspace.getNumComponents();
  }

  /**
   * Converts pixel value to sRGB and extract red int sample scaled
   * to range [0, 255].
   *
   * @param pixel pixel value that will be interpreted according to
   * the color model, (assumed alpha premultiplied if color model says
   * so.)
   *
   * @return red sample scaled to range [0, 255], from default color
   * space sRGB, alpha non-premultiplied.
   */
  public abstract int getRed(int pixel);

  /**
   * Converts pixel value to sRGB and extract green int sample
   * scaled to range [0, 255].
   *
   * @see #getRed(int)
   */
  public abstract int getGreen(int pixel);
    
  /**
   * Converts pixel value to sRGB and extract blue int sample
   * scaled to range [0, 255].
   *
   * @see #getRed(int)
   */
  public abstract int getBlue(int pixel);

  /**
   * Extract alpha int sample from pixel value, scaled to [0, 255].
   *
   * @param pixel pixel value that will be interpreted according to
   * the color model.
   *
   * @return alpha sample, scaled to range [0, 255].
   */
  public abstract int getAlpha(int pixel);

  /**
   * Converts a pixel int value of the color space of the color
   * model to a sRGB pixel int value.
   *
   * This method is typically overriden in subclasses to provide a
   * more efficient implementation.
   * 
   * @param pixel pixel value that will be interpreted according to
   * the color model.
   *
   * @return a pixel in sRGB color space, encoded in default
   * 0xAARRGGBB format.  */
  public int getRGB(int pixel)
  {
    return 
      ((getAlpha(pixel) & 0xff) << 24) |
      ((  getRed(pixel) & 0xff) << 16) |
      ((getGreen(pixel) & 0xff) <<  8) |
      (( getBlue(pixel) & 0xff) <<  0);
  }
  

  /**
   * In this color model we know that the whole pixel value will
   * always be contained within the first element of the pixel
   * array.
   */
  final int getPixelFromArray(Object inData) {
    DataBuffer data =
      Buffers.createBufferFromData(transferType, inData, 1);
    Object da = Buffers.getData(data);

    return data.getElem(0);
  }

  /** 
   * Converts pixel in the given array to sRGB and extract blue int
   * sample scaled to range [0-255].
   *
   * This method is typically overriden in subclasses to provide a
   * more efficient implementation.
   * 
   * @param array of transferType containing a single pixel.  The
   * pixel should be encoded in the natural way of the color model.
   */
  public int getRed(Object inData)
  {
    return getRed(getPixelFromArray(inData));
  }

  /**
   * @see #getRed(Object)
   */
  public int getGreen(Object inData)
  {
    return getGreen(getPixelFromArray(inData));
  }

  /**
   * @see #getRed(Object)
   */
  public int getBlue(Object inData) {
    return getBlue(getPixelFromArray(inData));
  }

  /**
   * @see #getRed(Object)
   */
  public int getAlpha(Object inData) {
    return getAlpha(getPixelFromArray(inData));
  }

  /**
   * Converts a pixel in the given array of the color space of the
   * color model to an sRGB pixel int value.
   *
   * <p>This method performs the inverse function of
   * <code>getDataElements(int rgb, Object pixel)</code>.
   * I.e. <code>(rgb == cm.getRGB(cm.getDataElements(rgb,
   * null)))</code>.
   *
   * @param inData array of transferType containing a single pixel. The
   * pixel should be encoded in the natural way of the color model.
   *
   * @return a pixel in sRGB color space, encoded in default
   * 0xAARRGGBB format.
   *
   * @see #getDataElements(int, Object)
   */
  public int getRGB(Object inData)
  {
    return 
      ((getAlpha(inData) & 0xff) << 24) |
      ((  getRed(inData) & 0xff) << 16) |
      ((getGreen(inData) & 0xff) <<  8) |
      (( getBlue(inData) & 0xff) <<  0);
  }

  /**
   * Converts an sRGB pixel int value to an array containing a
   * single pixel of the color space of the color model.
   * 
   * <p>This method performs the inverse function of
   * <code>getRGB(Object inData)</code>.
   *
   * Outline of conversion process:
   *
   * <ol>
   *
   * <li>Convert rgb to normalized [0.0, 1.0] sRGB values.</li>
   *
   * <li>Convert to color space components using fromRGB in
   * ColorSpace.</li>
   *
   * <li>If color model has alpha and should be premultiplied,
   * multiply color space components with alpha value</li>
   *
   * <li>Scale the components to the correct number of bits.</li>
   *
   * <li>Arrange the components in the output array</li>
   * 
   * </ol>
   *
   * @param rgb The color to be converted to dataElements.  A pixel
   * in sRGB color space, encoded in default 0xAARRGGBB format,
   * assumed not alpha premultiplied.
   *
   * @param pixel to avoid needless creation of arrays, an array to
   * use to return the pixel can be given. If null, a suitable array
   * will be created.
   *
   * @return An array of transferType values representing the color,
   * in the color model format. The color model defines whether the
   *  
   * @see #getRGB(Object)
   */
  public Object getDataElements(int rgb, Object pixel)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }

  /**
   * Fills an array with the unnormalized component samples from a
   * pixel value. I.e. decompose the pixel, but not perform any
   * color conversion. 
   *
   * This method is typically overriden in subclasses to provide a
   * more efficient implementation.
   * 
   * @param pixel pixel value encoded according to the color model.
   *
   * @return arrays of unnormalized component samples of single
   * pixel.  The scale and multiplication state of the samples are
   * according to the color model. Each component sample is stored
   * as a separate element in the array.
   */
  public int[] getComponents(int pixel, int[] components, int offset)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }
  
  /**
   * Fills an array with the unnormalized component samples from an
   * array of transferType containing a single pixel. I.e. decompose
   * the pixel, but not perform any color conversion.
   *
   * This method is typically overriden in subclasses to provide a
   * more efficient implementation.
   *
   * @param array of transferType containing a single pixel.  The
   * pixel should be encoded in the natural way of the color model.
   * 
   * @return arrays of unnormalized component samples of single
   * pixel.  The scale and multiplication state of the samples are
   * according to the color model. Each component sample is stored
   * as a separate element in the array.
   */
  public int[] getComponents(Object pixel, int[] components, int offset)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }

  /**
   * Convert normalized components to unnormalized components.
   */
  public int[] getUnnormalizedComponents(float[] normComponents,
					 int normOffset,
					 int[] components,
					 int offset)
  {
    int numComponents = getNumComponents();
    if (components == null)
    {
      components = new int[offset + numComponents];
    }
    
    for (int i=0; i<numComponents; i++)
    {
      float in = normComponents[normOffset++];
      int out = (int) (in * ((1<<getComponentSize(i)) - 1));
      components[offset++] = out;
    }
    return components;
  }

  /**
   * Convert unnormalized components to normalized components.
   */
  public float[] getNormalizedComponents(int[] components,
					 int offset,
					 float[] normComponents,
					 int normOffset)
  {
    int numComponents = getNumComponents();
    if (normComponents == null)
    {
      normComponents = new float[normOffset + numComponents];
    }

    for (int i=0; i<numComponents; i++)
    {
      float in = components[offset++];
      float out = in / ((1<<getComponentSize(i)) - 1);
      normComponents[normOffset++] = out;
    }
    return normComponents;
  }

  /**
   * Convert unnormalized components to normalized components.
   *
   * @since 1.4
   */
  public float[] getNormalizedComponents (Object pixel,
                                          float[] normComponents,
                                          int normOffset)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }

  /**
   * Converts the unnormalized component samples from an array to a
   * pixel value. I.e. composes the pixel from component samples, but
   * does not perform any color conversion or scaling of the samples.
   * 
   * This method performs the inverse function of
   * <code>getComponents(int pixel, int[] components,
   *			       int offset)</code>. I.e.
   *
   * <code>(pixel == cm.getDataElement(cm.getComponents(pixel, null,
   * 0), 0))</code>.
   *
   * This method is typically overriden in subclasses to provide a
   * more efficient implementation.
   *
   * @param arrays of unnormalized component samples of single
   * pixel.  The scale and multiplication state of the samples are
   * according to the color model. Each component sample is stored
   * as a separate element in the array.
   *
   * @return pixel value encoded according to the color model.
   */
  public int getDataElement(int[] components, int offset)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }

  public int getDataElement (float[] components, int offset)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }
  
  public Object getDataElements(int[] components, int offset, Object obj)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }

  public int getDataElements (float[] components, Object obj)
  {
    // subclasses has to implement this method.
    throw new UnsupportedOperationException();
  }
  
  public boolean equals(Object obj)
  {
    if (!(obj instanceof ColorModel)) return false;

    ColorModel o = (ColorModel) obj;
    return 
      (pixel_bits == o.pixel_bits) &&
      (transferType == o.transferType) &&
      (transparency == o.transparency) &&
      (hasAlpha == o.hasAlpha) &&
      (isAlphaPremultiplied == o.isAlphaPremultiplied) &&
      Arrays.equals(bits, o.bits) &&
      (cspace.equals(o.cspace));
  }

  public final ColorSpace getColorSpace()
  {
    return cspace;
  }

  // Typically overridden
  public ColorModel coerceData(WritableRaster raster,
			       boolean isAlphaPremultiplied)
  {
    if (this.isAlphaPremultiplied == isAlphaPremultiplied)
      return this;

    int w = raster.getWidth();
    int h = raster.getHeight();
    int x = raster.getMinX();
    int y = raster.getMinY();
    int size = w*h;
    int numColors = getNumColorComponents();
    int numComponents = getNumComponents();
    int alphaScale = (1<<getComponentSize(numColors)) - 1;
    double[] pixels = raster.getPixels(x, y, w, h, (double[]) null);

    for (int i=0; i<size; i++)
      {
	double alpha = pixels[i*numComponents+numColors]*alphaScale;
	for (int c=0; c<numColors; c++)
	  {
	    int offset = i*numComponents+c;
	    if (isAlphaPremultiplied)
		pixels[offset] = pixels[offset]/alpha;
	    else
	      pixels[offset] = pixels[offset]*alpha;
	  }
      }
    
    raster.setPixels(0, 0, w, h, pixels);

    // FIXME: what can we return?
    return null;
  }
    
  /**
   * Checks if the given raster has a compatible data-layout (SampleModel).
   * @param raster The Raster to test.
   * @return true if raster is compatible.
   */ 
  public boolean isCompatibleRaster(Raster raster)
  {
    SampleModel sampleModel = raster.getSampleModel();
    return isCompatibleSampleModel(sampleModel);
  }

  // Typically overridden
  public WritableRaster createCompatibleWritableRaster(int w, int h)
  {
    return new WritableRaster(createCompatibleSampleModel(w, h),
			      new Point(0, 0));
  }

  // Typically overridden
  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    throw new UnsupportedOperationException();
  }

  // Typically overridden
  public boolean isCompatibleSampleModel(SampleModel sm)
  {
    return sm.getTransferType() == transferType;
  }

  public final int getTransferType ()
  {
    return transferType;
  }

  /**
   * Subclasses must override this method if it is possible for the
   * color model to have an alpha channel.
   *
   * @return null, as per JDK 1.3 doc. Subclasses will only return
   * null if no alpha raster exists.
   */
  public WritableRaster getAlphaRaster(WritableRaster raster)
  {
    return null;
    
    /* It is a mystery to me why we couldn't use the following code...
       
       
       if (!hasAlpha()) return null;
       
       SampleModel sm = raster.getSampleModel();
       int[] alphaBand = { sm.getNumBands() - 1 };
       SampleModel alphaModel = sm.createSubsetSampleModel(alphaBand);
       DataBuffer buffer = raster.getDataBuffer();
       Point origin = new Point(0, 0);
       return Raster.createWritableRaster(alphaModel, buffer, origin);
       

       ...here, and avoided overriding the method in subclasses,
       but the Sun docs state that this method always will return
       null, and that overriding is required. Oh, well.
    */
  }

  String stringParam()
  {
    return "pixel_bits=" + pixel_bits +
      ", cspace=" + cspace +
      ", transferType=" + transferType +
      ", transparency=" + transparency +
      ", hasAlpha=" + hasAlpha +
      ", isAlphaPremultiplied=" + isAlphaPremultiplied;
  }

  public String toString()
  {
    return getClass().getName() + "[" + stringParam() + "]";
  }
}
