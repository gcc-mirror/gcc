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

/**
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class IndexColorModel extends ColorModel
{
  private int map_size;
  private boolean opaque;
  private int trans = -1;
  private int[] rgb;

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
    super (bits);
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
   */
  public IndexColorModel (int bits, int size, byte[] cmap, int start, 
                          boolean hasAlpha)
  {
    this (bits, size, cmap, start, hasAlpha, -1);
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
   * @param trans the index of the transparent color
   */
  public IndexColorModel (int bits, int size, byte[] cmap, int start, 
                          boolean hasAlpha, int trans)
  {
    super (bits);
    map_size = size;
    opaque = !hasAlpha;
    this.trans = trans;
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
    if (pixel < map_size)
	    return (int) ((generateMask (3) & rgb[pixel]) >> (3 * pixel_bits));
    
    return 0;
  }

  /**
   * Get the RGB color value of the given pixel using the default
   * RGB color model. 
   *
   * @param pixel a pixel value
   */
  public final int getRGB (int pixel)
  {
    if (pixel < map_size)
	    return rgb[pixel];
    
    return 0;
  }
    
  //pixel_bits is number of bits to be in generated mask
  private int generateMask (int offset)
  {
    return (((2 << pixel_bits ) - 1) << (pixel_bits * offset));
  }

}

