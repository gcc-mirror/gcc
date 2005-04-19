/* IndexColorModel.java -- Java class for interpreting Pixel objects
   Copyright (C) 1999 Free Software Foundation, Inc.

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

import java.awt.color.ColorSpace;
import java.math.BigInteger;

/**
 * Color model similar to pseudo visual in X11.
 *
 * This color model maps linear pixel values to actual RGB and alpha colors.
 * Thus, pixel values are indexes into the color map.  Each color component is
 * an 8-bit unsigned value.
 *
 * The IndexColorModel supports a map of valid pixels, allowing the
 * representation of holes in the the color map.  The valid map is represented
 * as a BigInteger where each bit indicates the validity of the map entry with
 * the same index.
 * 
 * Colors can have alpha components for transparency support.  If alpha
 * component values aren't given, color values are opaque.  The model also
 * supports a reserved pixel value to represent completely transparent colors,
 * no matter what the actual color component values are.
 *
 * IndexColorModel supports anywhere from 1 to 16 bit index values.  The
 * allowed transfer types are DataBuffer.TYPE_BYTE and DataBuffer.TYPE_USHORT.
 *
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class IndexColorModel extends ColorModel
{
  private int map_size;
  private boolean opaque;
  private int trans = -1;
  private int[] rgb;
  private BigInteger validBits = BigInteger.ZERO;

  /**
   * Each array much contain <code>size</code> elements.  For each 
   * array, the i-th color is described by reds[i], greens[i], 
   * blues[i], alphas[i], unless alphas is not specified, then all the 
   * colors are opaque except for the transparent color. 
   *
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param reds the red component of all colors
   * @param greens the green component of all colors
   * @param blues the blue component of all colors
   */
  public IndexColorModel(int bits, int size, byte[] reds, byte[] greens,
                         byte[] blues)
  {
    this (bits, size, reds, greens, blues, (byte[]) null);
  }

  /**
   * Each array much contain <code>size</code> elements.  For each 
   * array, the i-th color is described by reds[i], greens[i], 
   * blues[i], alphas[i], unless alphas is not specified, then all the 
   * colors are opaque except for the transparent color. 
   *
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param reds the red component of all colors
   * @param greens the green component of all colors
   * @param blues the blue component of all colors
   * @param trans the index of the transparent color
   */
  public IndexColorModel(int bits, int size, byte[] reds, byte[] greens,
                         byte[] blues, int trans)
  {
    this (bits, size, reds, greens, blues, (byte[]) null);
    this.trans = trans;
  }

  /**
   * Each array much contain <code>size</code> elements.  For each 
   * array, the i-th color is described by reds[i], greens[i], 
   * blues[i], alphas[i], unless alphas is not specified, then all the 
   * colors are opaque except for the transparent color. 
   *
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param reds the red component of all colors
   * @param greens the green component of all colors
   * @param blues the blue component of all colors
   * @param alphas the alpha component of all colors
   */
  public IndexColorModel(int bits, int size, byte[] reds, byte[] greens,
                         byte[] blues, byte[] alphas)
  {
    // FIXME: This super() constructor should not be used since it can give
    // the wrong value for hasAlpha() which is final and cannot be overloaded
    super(bits); 
    map_size = size;
    opaque = (alphas == null);

    rgb = new int[size];
    if (alphas == null)
      {
        for (int i = 0; i < size; i++)
          {
            rgb[i] = (0xff000000
                      | ((reds[i] & 0xff) << 16)
                      | ((greens[i] & 0xff) << 8)
                      | (blues[i] & 0xff));
          }
      }
    else
      {
        for (int i = 0; i < size; i++)
          {
            rgb[i] = ((alphas[i] & 0xff) << 24
                      | ((reds[i] & 0xff) << 16)
                      | ((greens[i] & 0xff) << 8)
                      | (blues[i] & 0xff));
          }
      }

    // Generate a bigint with 1's for every pixel
    validBits = validBits.setBit(size).subtract(BigInteger.ONE);
  }

  /**
   * Each array much contain <code>size</code> elements.  For each 
   * array, the i-th color is described by reds[i], greens[i], 
   * blues[i], alphas[i], unless alphas is not specified, then all the 
   * colors are opaque except for the transparent color. 
   *
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param cmap packed color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param hasAlpha <code>cmap</code> has alpha values
   * @throws IllegalArgumentException if bits < 1, bits > 16, or size < 1.
   */
  public IndexColorModel (int bits, int size, byte[] cmap, int start, 
                          boolean hasAlpha)
  {
    this (bits, size, cmap, start, hasAlpha, -1);
  }

  /**
   * Construct an IndexColorModel from an array of red, green, blue, and
   * optional alpha components.  The component values are interleaved as RGB(A).
   * 
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param cmap interleaved color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param hasAlpha <code>cmap</code> has alpha values
   * @param trans the index of the transparent color
   * @throws IllegalArgumentException if bits < 1, bits > 16, or size < 1.
   */
  public IndexColorModel (int bits, int size, byte[] cmap, int start, 
                          boolean hasAlpha, int trans)
  {
    super (bits);
    if (bits > 16)
      throw new IllegalArgumentException("bits > 16");
    if (size < 1)
      throw new IllegalArgumentException("size < 1");
    map_size = size;
    opaque = !hasAlpha;
    this.trans = trans;

    rgb = new int[size];
    if (hasAlpha)
    {
      for (int i = 0; i < size; i++)
        rgb[i] =
	  // alpha
	  ((cmap[4 * i + 3 + start] & 0xff) << 24
	   // red
	   | ((cmap[4 * i + start] & 0xff) << 16)
	   // green
	   | ((cmap[4 * i + 1 + start] & 0xff) << 8)
	   // blue
	   | (cmap[4 * i + 2 + start] & 0xff));
    }
    else
    {
      for (int i = 0; i < size; i++)
	rgb[i] = (0xff000000
		  // red
		  | ((cmap[3 * i + start] & 0xff) << 16)
		  // green
		  | ((cmap[3 * i + 1 + start] & 0xff) << 8)
		  // blue
		  | (cmap[3 * i + 2 + start] & 0xff));
    }

    // Generate a bigint with 1's for every pixel
    validBits = validBits.setBit(size).subtract(BigInteger.ONE);
  }

  /**
   * Construct an IndexColorModel from an array of <code>size</code> packed
   * colors.  Each int element contains 8-bit red, green, blue, and optional
   * alpha values packed in order.  If hasAlpha is false, then all the colors
   * are opaque except for the transparent color.
   *
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param cmap packed color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param hasAlpha <code>cmap</code> has alpha values
   * @param trans the index of the transparent color
   * @param transferType DataBuffer.TYPE_BYTE or DataBuffer.TYPE_USHORT
   * @throws IllegalArgumentException if bits < 1, bits > 16, or size < 1.
   * @throws IllegalArgumentException if transferType is something other than
   * TYPE_BYTE or TYPE_USHORT.
   */
  public IndexColorModel (int bits, int size, int[] cmap, int start, 
                          boolean hasAlpha, int trans, int transferType)
  {
    super(bits * 4, // total bits, sRGB, four channels
	  nArray(bits, 4), // bits for each channel
	  ColorSpace.getInstance(ColorSpace.CS_sRGB), // sRGB
	  true, // has alpha
	  false, // not premultiplied
	  TRANSLUCENT, transferType);
    if (transferType != DataBuffer.TYPE_BYTE
        && transferType != DataBuffer.TYPE_USHORT)
      throw new IllegalArgumentException();
    if (bits > 16)
      throw new IllegalArgumentException("bits > 16");
    if (size < 1)
      throw new IllegalArgumentException("size < 1");
    map_size = size;
    opaque = !hasAlpha;
    this.trans = trans;

    rgb = new int[size];
    if (!hasAlpha)
      for (int i = 0; i < size; i++)
	rgb[i] = cmap[i + start] | 0xff000000;
    else
      System.arraycopy(cmap, start, rgb, 0, size);

    // Generate a bigint with 1's for every pixel
    validBits = validBits.setBit(size).subtract(BigInteger.ONE);
  }

  /**
   * Construct an IndexColorModel using a colormap with holes.
   * 
   * The IndexColorModel is built from the array of ints defining the
   * colormap.  Each element contains red, green, blue, and alpha
   * components.    The ColorSpace is sRGB.  The transparency value is
   * automatically determined.
   * 
   * This constructor permits indicating which colormap entries are valid,
   * using the validBits argument.  Each entry in cmap is valid if the
   * corresponding bit in validBits is set.  
   * 
   * @param bits the number of bits needed to represent <code>size</code> colors
   * @param size the number of colors in the color map
   * @param cmap packed color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param transferType DataBuffer.TYPE_BYTE or DataBuffer.TYPE_USHORT
   * @throws IllegalArgumentException if bits < 1, bits > 16, or size < 1.
   * @throws IllegalArgumentException if transferType is something other than
   * TYPE_BYTE or TYPE_USHORT.
   */
  public IndexColorModel (int bits, int size, int[] cmap, int start, 
                          int transferType, BigInteger validBits)
  {
    super(bits * 4, // total bits, sRGB, four channels
	  nArray(bits, 4), // bits for each channel
	  ColorSpace.getInstance(ColorSpace.CS_sRGB), // sRGB
	  true, // has alpha
	  false, // not premultiplied
	  TRANSLUCENT, transferType);
    if (transferType != DataBuffer.TYPE_BYTE
        && transferType != DataBuffer.TYPE_USHORT)
      throw new IllegalArgumentException();
    if (bits > 16)
      throw new IllegalArgumentException("bits > 16");
    if (size < 1)
      throw new IllegalArgumentException("size < 1");
    map_size = size;
    opaque = false;
    this.trans = -1;
    this.validBits = validBits;

    rgb = new int[size];
    if (!hasAlpha)
      for (int i = 0; i < size; i++)
	rgb[i] = cmap[i + start] | 0xff000000;
    else
      System.arraycopy(cmap, start, rgb, 0, size);
  }

  public final int getMapSize ()
  {
    return map_size;
  }

  /**
   * Get the index of the transparent color in this color model
   */
  public final int getTransparentPixel ()
  {
    return trans;
  }

  /**
   * <br>
   */
  public final void getReds (byte[] r)
  {
    getComponents (r, 2);
  }

  /**
   * <br>
   */
  public final void getGreens (byte[] g)
  {
    getComponents (g, 1);
  }

  /**
   * <br>
   */
  public final void getBlues (byte[] b)
  {
    getComponents (b, 0);
  }

  /**
   * <br>
   */
  public final void getAlphas (byte[] a)
  {
    getComponents (a, 3);
  }

  private void getComponents (byte[] c, int ci)
  {
    int i, max = (map_size < c.length) ? map_size : c.length;
    for (i = 0; i < max; i++)
	    c[i] = (byte) ((generateMask (ci)  & rgb[i]) >> (ci * pixel_bits));
  } 

  /**
   * Get the red component of the given pixel.
   */
  public final int getRed (int pixel)
  {
    if (pixel < map_size)
	    return (int) ((generateMask (2) & rgb[pixel]) >> (2 * pixel_bits));
    
    return 0;
  }

  /**
   * Get the green component of the given pixel.
   */
  public final int getGreen (int pixel)
  {
    if (pixel < map_size)
	    return (int) ((generateMask (1) & rgb[pixel]) >> (1 * pixel_bits));
    
    return 0;
  }

  /**
   * Get the blue component of the given pixel.
   */
  public final int getBlue (int pixel)
  {
    if (pixel < map_size) 
	    return (int) (generateMask (0) & rgb[pixel]);
    
    return 0;
  }

  /**
   * Get the alpha component of the given pixel.
   */
  public final int getAlpha (int pixel)
  {
    if (opaque || pixel >= map_size)
      return 255;

    return (int) ((generateMask (3) & rgb[pixel]) >> (3 * pixel_bits));
  }

  /**
   * Get the RGB color value of the given pixel using the default
   * RGB color model. 
   *
   * @param pixel a pixel value
   */
  public final int getRGB (int pixel)
  {
    if (pixel >= 0 && pixel < map_size)
	    return rgb[pixel];
    
    return 0;
  }
    
  /**
   * Get the RGB color values of all pixels in the map using the default
   * RGB color model. 
   *
   * @param rgb The destination array.
   */
  public final void getRGBs (int[] rgb)
  {
    System.arraycopy(this.rgb, 0, rgb, 0, map_size);
  }
    
   //pixel_bits is number of bits to be in generated mask
  private int generateMask (int offset)
  {
    return (((2 << pixel_bits ) - 1) << (pixel_bits * offset));
  }

  /** Return true if pixel is valid, false otherwise. */
  public boolean isValid(int pixel)
  {
    return validBits.testBit(pixel);
  }
  
  /** Return true if all pixels are valid, false otherwise. */
  public boolean isValid()
  {
    // Generate a bigint with 1's for every pixel
    BigInteger allbits = new BigInteger("0");
    allbits.setBit(map_size);
    allbits.subtract(new BigInteger("1"));
    return allbits.equals(validBits);
  }
  
  /** 
   * Returns a BigInteger where each bit represents an entry in the color
   * model.  If the bit is on, the entry is valid.
   */
  public BigInteger getValidPixels()
  {
    return validBits;
  }
  
  /**
   * Construct a BufferedImage with rgb pixel values from a Raster.
   * 
   * Constructs a new BufferedImage in which each pixel is an RGBA int from
   * a Raster with index-valued pixels.  If this model has no alpha component
   * or transparent pixel, the type of the new BufferedImage is TYPE_INT_RGB.
   * Otherwise the type is TYPE_INT_ARGB.  If forceARGB is true, the type is
   * forced to be TYPE_INT_ARGB no matter what.
   * 
   * @param raster The source of pixel values.
   * @param forceARGB True if type must be TYPE_INT_ARGB.
   * @return New BufferedImage with RBGA int pixel values.
   */
  public BufferedImage convertToIntDiscrete(Raster raster, boolean forceARGB)
  {
    int type = forceARGB ? BufferedImage.TYPE_INT_ARGB
      : ((opaque && trans == -1) ? BufferedImage.TYPE_INT_RGB :
	 BufferedImage.TYPE_INT_ARGB); 

    // FIXME: assuming that raster has only 1 band since pixels are supposed
    // to be int indexes.
    // FIXME: it would likely be more efficient to fetch a complete array,
    // but it would take much more memory.
    // FIXME: I'm not sure if transparent pixels or alpha values need special
    // handling here.
    BufferedImage im = new BufferedImage(raster.width, raster.height, type);
    for (int x = raster.minX; x < raster.width + raster.minX; x++)
      for (int y = raster.minY; y < raster.height + raster.minY; y++)
        im.setRGB(x, y, rgb[raster.getSample(x, y, 0)]);

    return im;
  }
}

