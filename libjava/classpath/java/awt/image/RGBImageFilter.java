/* RGBImageFilter.java -- Java class for filtering Pixels by RGB values
   Copyright (C) 1999, 2005  Free Software Foundation, Inc.

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

/**
 * A filter designed to filter images in the default RGBColorModel regardless of 
 * the ImageProducer's ColorModel.
 *
 * @author Mark Benvenuto (mcb54@columbia.edu)
 */
public abstract class RGBImageFilter extends ImageFilter
{
  protected ColorModel origmodel;

  protected ColorModel newmodel;
    
  /**
   * Specifies whether to apply the filter to the index entries of the
   * IndexColorModel. Subclasses should set this to true if the filter
   * does not depend on the pixel's coordinate.
   */
  protected boolean canFilterIndexColorModel = false;

  /**
   * Construct new RGBImageFilter.
   */
  public RGBImageFilter() 
  {
  }

  /**
   * Sets the ColorModel used to filter with. If the specified ColorModel is
   * IndexColorModel and canFilterIndexColorModel is true, we subsitute the
   * ColorModel for a filtered one here and in setPixels whenever the original
   * one appears. Otherwise overrides the default ColorModel of ImageProducer
   * and specifies the default RGBColorModel
   *
   * @param model the color model to be used most often by setPixels
   *
   * @see ColorModel
   */
  public void setColorModel(ColorModel model) 
  {
    if ((model instanceof IndexColorModel) && canFilterIndexColorModel)
      {
        ColorModel newCM = filterIndexColorModel((IndexColorModel) model);
        substituteColorModel(model, newCM);
        consumer.setColorModel(newmodel);
      }
    else
      {
        consumer.setColorModel(ColorModel.getRGBdefault());
      }
  }

  /**
   * Registers a new ColorModel to subsitute for the old ColorModel when 
   * setPixels encounters the a pixel with the old ColorModel. The pixel 
   * remains unchanged except for a new ColorModel.
   * 
   * @param oldcm the old ColorModel
   * @param newcm the new ColorModel
   */
  public void substituteColorModel(ColorModel oldcm, ColorModel newcm)
  {
    origmodel = oldcm;
    newmodel = newcm;
  }

  /**
   * Filters an IndexColorModel through the filterRGB function. Uses
   * coordinates of -1 to indicate its filtering an index and not a pixel.
   *
   * @param icm an IndexColorModel to filter
   */
  public IndexColorModel filterIndexColorModel(IndexColorModel icm) 
  {
    int len = icm.getMapSize();
    byte[] reds = new byte[len];
    byte[] greens = new byte[len];
    byte[] blues = new byte[len];
    byte[] alphas = new byte[len];

    icm.getAlphas( alphas );
    icm.getReds( reds );
    icm.getGreens( greens );
    icm.getBlues( blues );

    int transparent = icm.getTransparentPixel();
    boolean needAlpha = false;
    for( int i = 0; i < len; i++ )
      {
        int rgb = filterRGB(-1, -1, icm.getRGB(i));
        alphas[i] = (byte) (rgb >> 24);
        if (alphas[i] != ((byte) 0xff) && i != transparent)
          needAlpha = true;
        reds[i] = (byte) (rgb >> 16);
        greens[i] = (byte) (rgb >> 8);
        blues[i] = (byte) (rgb);
      }
    IndexColorModel newIcm;
    if (needAlpha)
      newIcm = new IndexColorModel(icm.getPixelSize(), len, reds, greens,
                                   blues, alphas);
    else
      newIcm = new IndexColorModel(icm.getPixelSize(), len, reds, greens,
                                   blues, transparent);
    return newIcm;
  }

  /**
   * This functions filters a set of RGB pixels through filterRGB.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param pixels the array of pixel values
   * @param offset the index of the first pixels in the
   *        <code>pixels</code> array
   * @param scansize the width to use in extracting pixels from the
   *        <code>pixels</code> array
   */
  public void filterRGBPixels(int x, int y, int w, int h, int[] pixels,
                              int offset, int scansize)
  {
    int index = offset;
    for (int yp = 0; yp < h; yp++)
      {
        for (int xp = 0; xp < w; xp++)
          {
            pixels[index] = filterRGB(xp + x, yp + y, pixels[index]);
            index++;
          }
        index += scansize - w;
      }
    consumer.setPixels(x, y, w, h, ColorModel.getRGBdefault(), pixels, offset,
                       scansize);
  }

  /**
   * If the ColorModel is the same ColorModel which as already converted 
   * then it converts it the converted ColorModel. Otherwise it passes the 
   * array of pixels through filterRGBpixels.
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param model the <code>ColorModel</code> used to translate the pixels
   * @param pixels the array of pixel values
   * @param offset the index of the first pixels in the <code>pixels</code>
   *        array
   * @param scansize the width to use in extracting pixels from the
   *        <code>pixels</code> array
   */
  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        byte[] pixels, int offset, int scansize)
  {
    if (model == origmodel)
      {
        consumer.setPixels(x, y, w, h, newmodel, pixels, offset, scansize);
      }
    else
      {
        int[] filtered = new int[w];
        int index = offset;
        for (int yp = 0; yp < h; yp++)
          {
            for (int xp = 0; xp < w; xp++)
              {
                filtered[xp] = model.getRGB((pixels[index] & 0xff));
                index++;
              }
            index += scansize - w;
            filterRGBPixels(x, y + yp, w, 1, filtered, 0, w);
          }
      }
  }

  /**
   * This function delivers a rectangle of pixels where any
   * pixel(m,n) is stored in the array as an <code>int</code> at
   * index (n * scansize + m + offset).  
   *
   * @param x the x coordinate of the rectangle
   * @param y the y coordinate of the rectangle
   * @param w the width of the rectangle
   * @param h the height of the rectangle
   * @param model the <code>ColorModel</code> used to translate the pixels
   * @param pixels the array of pixel values
   * @param offset the index of the first pixels in the <code>pixels</code>
   *        array
   * @param scansize the width to use in extracting pixels from the
   *        <code>pixels</code> array
   */
  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        int[] pixels, int offset, int scansize)
  {
    if (model == origmodel)
      {
        consumer.setPixels(x, y, w, h, newmodel, pixels, offset, scansize);
      }
    else
      {
        int[] filtered = new int[w];
        int index = offset;
        for (int yp = 0; yp < h; yp++)
          {
            for (int xp = 0; xp < w; xp++)
              {
                filtered[xp] = model.getRGB((pixels[index] & 0xff));
                index++;
              }
            index += scansize - w;
            filterRGBPixels(x, y + yp, w, 1, filtered, 0, w);
          }
      }
  }

  /**
   * Filters a single pixel from the default ColorModel.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   * @param rgb color
   */
  public abstract int filterRGB(int x, int y, int rgb);
}
