/* IndexColorModel.java -- Java class for interpreting Pixel objects
   Copyright (C) 1999, 2005 Free Software Foundation, Inc.

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

import java.awt.color.ColorSpace;
import java.math.BigInteger;

/**
 * Color model similar to pseudo visual in X11.
 * <br><br>
 * This color model maps linear pixel values to actual RGB and alpha colors.
 * Thus, pixel values are indexes into the color map.  Each color component is
 * an 8-bit unsigned value.
 * <br><br>
 * The <code>IndexColorModel</code> supports a map of valid pixels, allowing 
 * the representation of holes in the the color map.  The valid map is 
 * represented as a {@link BigInteger} where each bit indicates the validity 
 * of the map entry with the same index.
 * <br><br>
 * Colors can have alpha components for transparency support.  If alpha
 * component values aren't given, color values are opaque.  The model also
 * supports a reserved pixel value to represent completely transparent colors,
 * no matter what the actual color component values are.
 * <br><br>
 * <code>IndexColorModel</code> supports anywhere from 1 to 16 bit index 
 * values.  The allowed transfer types are {@link DataBuffer#TYPE_BYTE} and 
 * {@link DataBuffer#TYPE_USHORT}.
 *
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class IndexColorModel extends ColorModel
{
  private int map_size;
  private boolean opaque;  // no alpha, but doesn't account for trans
  private int trans = -1;
  private int[] rgb;
  private BigInteger validBits = BigInteger.ZERO;

  /**
   * Creates a new indexed color model for <code>size</code> color elements 
   * with no alpha component.  Each array must contain at least 
   * <code>size</code> elements.  For each array, the i-th color is described 
   * by reds[i], greens[i] and blues[i]. 
   *
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors.
   * @param size the number of colors in the color map.
   * @param reds the red component of all colors.
   * @param greens the green component of all colors.
   * @param blues the blue component of all colors.
   *
   * @throws IllegalArgumentException if <code>bits</code> &lt; 1 or 
   *         <code>bits</code> &gt; 16.
   * @throws NullPointerException if any of the arrays is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>size</code> is greater 
   *         than the length of the component arrays.
   */
  public IndexColorModel(int bits, int size, byte[] reds, byte[] greens,
                         byte[] blues)
  {
    this(bits, size, reds, greens, blues, (byte[]) null);
  }

  /**
   * Creates a new indexed color model for <code>size</code> color elements.
   * Each array must contain at least <code>size</code> elements.  For each 
   * array, the i-th color is described by reds[i], greens[i] and blues[i]. 
   * All the colors are opaque except for the transparent color. 
   *
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors
   * @param size the number of colors in the color map
   * @param reds the red component of all colors
   * @param greens the green component of all colors
   * @param blues the blue component of all colors
   * @param trans the index of the transparent color (use -1 for no 
   *              transparent color).
   * 
   * @throws IllegalArgumentException if <code>bits</code> &lt; 1 or 
   *         <code>bits</code> &gt; 16.
   * @throws NullPointerException if any of the arrays is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>size</code> is greater 
   *         than the length of the component arrays.
   */
  public IndexColorModel(int bits, int size, byte[] reds, byte[] greens,
                         byte[] blues, int trans)
  {
    super(bits, nArray(8, (0 <= trans && trans < size) ? 4 : 3), 
        ColorSpace.getInstance(ColorSpace.CS_sRGB), 
        (0 <= trans && trans < size),  // hasAlpha 
        false, OPAQUE, 
        Buffers.smallestAppropriateTransferType(bits)); 
    if (bits < 1) 
      throw new IllegalArgumentException("bits < 1");
    if (bits > 16)
      throw new IllegalArgumentException("bits > 16");
    if (size < 1)
      throw new IllegalArgumentException("size < 1");
    map_size = size;
    if (0 <= trans && trans < size) {
      this.trans = trans;
      transparency = BITMASK;
    }
    rgb = new int[size];
    for (int i = 0; i < size; i++)
      {
        rgb[i] = (0xff000000
                  | ((reds[i] & 0xff) << 16)
                  | ((greens[i] & 0xff) << 8)
                  | (blues[i] & 0xff));
      }
    // Generate a bigint with 1's for every pixel
    validBits = validBits.setBit(size).subtract(BigInteger.ONE);
  }

  /**
   * Creates a new indexed color model for <code>size</code> color elements 
   * including alpha.  Each array must contain at least <code>size</code> 
   * elements.  For each array, the i-th color is described 
   * by reds[i], greens[i], blues[i] and alphas[i]. 
   *
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors.
   * @param size the number of colors in the color map.
   * @param reds the red component of all colors.
   * @param greens the green component of all colors.
   * @param blues the blue component of all colors.
   * @param alphas the alpha component of all colors (<code>null</code> 
   *               permitted).
   *
   * @throws IllegalArgumentException if <code>bits</code> &lt; 1 or 
   *           <code>bits</code> &gt; 16.
   * @throws NullPointerException if <code>reds</code>, <code>greens</code> or
   *         <code>blues</code> is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>size</code> is greater 
   *         than the length of the component arrays.
   */
  public IndexColorModel(int bits, int size, byte[] reds, byte[] greens,
                         byte[] blues, byte[] alphas)
  {
    super(bits, nArray(8, (alphas == null ? 3 : 4)), 
        ColorSpace.getInstance(ColorSpace.CS_sRGB), 
        (alphas != null), false, TRANSLUCENT, 
        Buffers.smallestAppropriateTransferType(bits)); 
    if (bits < 1) 
      throw new IllegalArgumentException("bits < 1");
    if (bits > 16)
      throw new IllegalArgumentException("bits > 16");
    if (size < 1)
      throw new IllegalArgumentException("size < 1");
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
        transparency = OPAQUE;
      }
    else
      {
	byte alphaZero = (byte) 0x00;
        byte alphaOne = (byte) 0xFF;
        for (int i = 0; i < size; i++)
          {
	    alphaZero = (byte) (alphaZero | alphas[i]);
            alphaOne = (byte) (alphaOne & alphas[i]);
            rgb[i] = ((alphas[i] & 0xff) << 24
                      | ((reds[i] & 0xff) << 16)
                      | ((greens[i] & 0xff) << 8)
                      | (blues[i] & 0xff));
          }
        if ((alphaZero == (byte) 0x00) || (alphaOne == (byte) 0xFF))
	  transparency = BITMASK;
	else
	  transparency = TRANSLUCENT;
      }

    // Generate a bigint with 1's for every pixel
    validBits = validBits.setBit(size).subtract(BigInteger.ONE);
  }

  /**
   * Creates a new indexed color model using the color components in 
   * <code>cmap</code>. If <code>hasAlpha</code> is <code>true</code> then
   * <code>cmap</code> contains an alpha component after each of the red, green
   * and blue components.
   *
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors
   * @param size the number of colors in the color map
   * @param cmap packed color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param hasAlpha <code>cmap</code> has alpha values
   * @throws IllegalArgumentException if bits &lt; 1, bits &gt; 16, or size 
   *         &lt; 1.
   * @throws NullPointerException if <code>cmap</code> is <code>null</code>.
   */
  public IndexColorModel(int bits, int size, byte[] cmap, int start, 
                         boolean hasAlpha)
  {
    this(bits, size, cmap, start, hasAlpha, -1);
  }

  /**
   * Construct an IndexColorModel from an array of red, green, blue, and
   * optional alpha components. The component values are interleaved as RGB(A).
   * 
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors
   * @param size the number of colors in the color map
   * @param cmap interleaved color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param hasAlpha <code>cmap</code> has alpha values
   * @param trans the index of the transparent color
   * @throws IllegalArgumentException if bits &lt; 1, bits &gt; 16, or size
   *         &lt; 1.
   * @throws NullPointerException if <code>cmap</code> is <code>null</code>.
   */
  public IndexColorModel(int bits, int size, byte[] cmap, int start, 
                         boolean hasAlpha, int trans)
  {
    super(bits, nArray(8, hasAlpha || (0 <= trans && trans < size) ? 4 : 3), 
        ColorSpace.getInstance(ColorSpace.CS_sRGB),
        hasAlpha || (0 <= trans && trans < size), false, OPAQUE, 
        Buffers.smallestAppropriateTransferType(bits));
    if (bits < 1)
      throw new IllegalArgumentException("bits < 1");
    if (bits > 16)
      throw new IllegalArgumentException("bits > 16");
    if (size < 1)
      throw new IllegalArgumentException("size < 1");
    map_size = size;
    opaque = !hasAlpha;
    if (0 <= trans && trans < size)
      this.trans = trans;

    rgb = new int[size];
    if (hasAlpha)
    {
      int alpha;
      int alphaZero = 0x00;  // use to detect all zeros
      int alphaOne = 0xff;   // use to detect all ones
      for (int i = 0; i < size; i++) {
	alpha = cmap[4 * i + 3 + start] & 0xff;  
        alphaZero = alphaZero | alpha;
        alphaOne = alphaOne & alpha;
        rgb[i] =
	  ( alpha << 24
	   // red
	   | ((cmap[4 * i + start] & 0xff) << 16)
	   // green
	   | ((cmap[4 * i + 1 + start] & 0xff) << 8)
	   // blue
	   | (cmap[4 * i + 2 + start] & 0xff));
      }
      if (alphaZero == 0) 
	transparency = BITMASK;
      else if (alphaOne == 255) 
        transparency = (trans != -1 ? BITMASK : OPAQUE);
      else
	transparency = TRANSLUCENT;
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
      if (trans != -1)
	transparency = BITMASK;
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
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors
   * @param size the number of colors in the color map
   * @param cmap packed color components
   * @param start the offset of the first color component in <code>cmap</code>
   * @param hasAlpha <code>cmap</code> has alpha values
   * @param trans the index of the transparent color
   * @param transferType {@link DataBuffer#TYPE_BYTE} or 
            {@link DataBuffer#TYPE_USHORT}.
   * @throws IllegalArgumentException if bits &lt; 1, bits &gt; 16, or size
   *         &lt; 1.
   * @throws IllegalArgumentException if <code>transferType</code> is something
   *         other than {@link DataBuffer#TYPE_BYTE} or 
   *         {@link DataBuffer#TYPE_USHORT}.
   */
  public IndexColorModel(int bits, int size, int[] cmap, int start, 
                         boolean hasAlpha, int trans, int transferType)
  {
    super(bits, 
	  nArray(8, 4), // bits for each channel
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
    if (0 <= trans && trans < size)
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
   * <br><br>
   * The IndexColorModel is built from the array of ints defining the
   * colormap.  Each element contains red, green, blue, and alpha
   * components.    The ColorSpace is sRGB.  The transparency value is
   * automatically determined.
   * <br><br>
   * This constructor permits indicating which colormap entries are valid,
   * using the validBits argument.  Each entry in cmap is valid if the
   * corresponding bit in validBits is set.  
   * 
   * @param bits the number of bits needed to represent <code>size</code> 
   *             colors.
   * @param size the number of colors in the color map.
   * @param cmap packed color components.
   * @param start the offset of the first color component in <code>cmap</code>.
   * @param transferType {@link DataBuffer#TYPE_BYTE} or 
   *                     {@link DataBuffer#TYPE_USHORT}.
   * @param validBits a map of the valid entries in <code>cmap</code>.
   * @throws IllegalArgumentException if bits &lt; 1, bits &gt; 16, or size
   *         &lt; 1.
   * @throws IllegalArgumentException if transferType is something other than
   *         {@link DataBuffer#TYPE_BYTE} or {@link DataBuffer#TYPE_USHORT}.
   */
  public IndexColorModel(int bits, int size, int[] cmap, int start, 
                         int transferType, BigInteger validBits)
  {
    super(bits, // total bits, sRGB, four channels
	  nArray(8, 4), // bits for each channel
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

  /**
   * Returns the size of the color lookup table.
   *
   * @return The size of the color lookup table.
   */
  public final int getMapSize()
  {
    return map_size;
  }

  /**
   * Get the index of the transparent color in this color model.
   *
   * @return The index of the color that is considered transparent, or -1 if 
   *         there is no transparent color.
   */
  public final int getTransparentPixel()
  {
    return trans;
  }

  /**
   * Fills the supplied array with the red component of each color in the 
   * lookup table.
   *
   * @param r an array that is at least as large as {@link #getMapSize()}.
   * @throws NullPointerException if <code>r</code> is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>r</code> has less 
   *         than {@link #getMapSize()} elements. 
   */
  public final void getReds(byte[] r)
  {
    int i;
    for (i = 0; i < map_size; i++)
      r[i] = (byte) ((0x00FF0000  & rgb[i]) >> 16);
  }

  /**
   * Fills the supplied array with the green component of each color in the 
   * lookup table.
   *
   * @param g an array that is at least as large as {@link #getMapSize()}.
   * @throws NullPointerException if <code>g</code> is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>g</code> has less 
   *         than {@link #getMapSize()} elements. 
   */
  public final void getGreens(byte[] g)
  {
    int i;
    for (i = 0; i < map_size; i++)
      g[i] = (byte) ((0x0000FF00  & rgb[i]) >> 8);
  }

  /**
   * Fills the supplied array with the blue component of each color in the 
   * lookup table.
   *
   * @param b an array that is at least as large as {@link #getMapSize()}.
   * @throws NullPointerException if <code>b</code> is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>b</code> has less 
   *         than {@link #getMapSize()} elements. 
   */
  public final void getBlues(byte[] b)
  {
    int i;
    for (i = 0; i < map_size; i++)
      b[i] = (byte) (0x000000FF & rgb[i]);
  }

  /**
   * Fills the supplied array with the alpha component of each color in the 
   * lookup table.  If the model has a transparent pixel specified, the alpha
   * for that pixel will be 0.
   *
   * @param a an array that is at least as large as {@link #getMapSize()}.
   * @throws NullPointerException if <code>a</code> is <code>null</code>.
   * @throws ArrayIndexOutOfBoundsException if <code>a</code> has less 
   *         than {@link #getMapSize()} elements. 
   */
  public final void getAlphas(byte[] a)
  {
    int i;
    for (i = 0; i < map_size; i++)
      if (i == trans) 
	a[i] = (byte) 0;
      else 
        a[i] = (byte) ((0xFF000000  & rgb[i]) >> 24);
  }

  /**
   * Returns the red component of the color in the lookup table for the 
   * given pixel value.
   *
   * @param pixel  the pixel lookup value.
   *
   * @return The red component of the color in the lookup table.
   * @throws ArrayIndexOutOfBoundsException if <code>pixel</code> is negative.
   */
  public final int getRed(int pixel)
  {
    if (pixel < map_size)
      return (0x00FF0000 & rgb[pixel]) >> 16;
    
    return 0;
  }

  /**
   * Returns the green component of the color in the lookup table for the 
   * given pixel value.
   *
   * @param pixel  the pixel lookup value.
   *
   * @return The green component of the color in the lookup table.
   * @throws ArrayIndexOutOfBoundsException if <code>pixel</code> is negative.
   */
  public final int getGreen(int pixel)
  {
    if (pixel < map_size)
      return (0x0000FF00 & rgb[pixel]) >> 8;
    
    return 0;
  }

  /**
   * Returns the blue component of the color in the lookup table for the 
   * given pixel value.
   *
   * @param pixel  the pixel lookup value.
   *
   * @return The blue component of the color in the lookup table.
   * @throws ArrayIndexOutOfBoundsException if <code>pixel</code> is negative.
   */
  public final int getBlue(int pixel)
  {
    if (pixel < map_size)
      return 0x000000FF & rgb[pixel];
    
    return 0;
  }

  /**
   * Returns the alpha component of the color in the lookup table for the 
   * given pixel value. If no alpha channel was specified when the color model
   * was created, then 255 is returned for all pixels except the transparent
   * pixel (if one is defined - see {@link #getTransparentPixel()}) which
   * returns an alpha of 0.
   *
   * @param pixel  the pixel lookup value.
   *
   * @return The alpha component of the color in the lookup table (in the 
   *         range 0 to 255).
   * @throws ArrayIndexOutOfBoundsException if <code>pixel</code> is negative.
   */
  public final int getAlpha(int pixel)
  {
    if (opaque && pixel != trans) 
      return 255;
    if ((pixel == trans && trans != -1) || pixel >= map_size)
      return 0;

    return (0xFF000000 & rgb[pixel]) >> 24;
  }

  /**
   * Get the RGB color value of the given pixel using the default
   * RGB color model. 
   *
   * @param pixel the pixel lookup value.
   * @return The RGB color value.
   * @throws ArrayIndexOutOfBoundsException if <code>pixel</code> is negative.
   */
  public final int getRGB(int pixel)
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
  public final void getRGBs(int[] rgb)
  {
    System.arraycopy(this.rgb, 0, rgb, 0, map_size);
  }
    
  /** 
   * Return <code>true</code> if the lookup table contains valid data for 
   * <code>pixel</code>, and <code>false</code> otherwise.
   *
   * @param pixel  the pixel value used to index the color lookup table.
   * @return <code>true</code> if <code>pixel</code> is valid, 
   *         <code>false</code> otherwise.
   */
  public boolean isValid(int pixel)
  {
    if (pixel >= 0)
      return validBits.testBit(pixel);
    return false;
  }
  
  /** 
   * Return <code>true</code> if all pixels are valid, <code>false</code> 
   * otherwise.
   *
   * @return <code>true</code> if all pixels are valid, <code>false</code> 
   * otherwise.
   */
  public boolean isValid()
  {
    // Generate a bigint with 1's for every pixel
    BigInteger allbits = new BigInteger("0");
    allbits = allbits.setBit(map_size);
    allbits = allbits.subtract(new BigInteger("1"));
    return allbits.equals(validBits);
  }
  
  /** 
   * Returns a binary value ({@link BigInteger}) where each bit represents an 
   * entry in the color lookup table.  If the bit is on, the entry is valid.
   * 
   * @return The binary value.
   */
  public BigInteger getValidPixels()
  {
    return validBits;
  }
  
  /**
   * Construct a {@link BufferedImage} with rgb pixel values from a 
   * {@link Raster}.
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
