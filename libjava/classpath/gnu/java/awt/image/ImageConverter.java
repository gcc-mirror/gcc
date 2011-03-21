/* ImageConverter.java -- Loads images asynchronously
   Copyright (C) 2008 Free Software Foundation, Inc.

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


package gnu.java.awt.image;

import gnu.java.awt.image.AsyncImage;

import java.awt.GraphicsEnvironment;
import java.awt.Image;
import java.awt.Transparency;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.ImageConsumer;
import java.awt.image.IndexColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.util.Hashtable;

/**
 * Convert an Image to a BufferedImage.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ImageConverter implements ImageConsumer
{

  public static final String IMAGE_TRANSPARENCY_PROPERTY =
    "gnu.awt.image.transparency";

  public static final String IMAGE_PROPERTIES_PROPERTY =
    "gnu.awt.image.properties";

  private AsyncImage image;
  private BufferedImage bImage;
  private Hashtable imageProperties;
  private int width, height;
  private ColorModel colorModel;
  private ColorModel targetColorModel;

  public ImageConverter()
  {
    width = 0;
    height = 0;
    image = new AsyncImage();
  }

  public void setDimensions(int w, int h)
  {
    width = w;
    height = h;
  }

  public void setProperties(Hashtable props)
  {
    // Ignore for now.
  }

  public void setColorModel(ColorModel model)
  {
    colorModel = model;
  }

  public void setHints(int flags)
  {
    // Ignore for now.
  }

  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        byte[] pixels, int offset, int scansize)
  {
    model = setupColorModel(model);

    if (bImage == null)
      {
        createImage();
      }

    Integer t = (Integer) imageProperties.get("gnu.awt.image.transparency");
    int transparency = t.intValue();

    if(targetColorModel.equals(model))
      {
        transparency = transferPixels(x, y, w, h, model, pixels, offset,
                                      scansize, transparency);
      }
    else if (model instanceof IndexColorModel
             && targetColorModel.equals(ColorModel.getRGBdefault()))
      {
        transparency = convertIndexColorModelToSRGB(x, y, w, h,
                                                    (IndexColorModel) model,
                                                    pixels, offset, scansize,
                                                    transparency);
      }
    else
      {
        transparency = convertPixels(x, y, w, h, model, pixels, offset,
                                     scansize, transparency);
      }

    imageProperties.put("gnu.awt.image.transparency",
                        Integer.valueOf(transparency));
  }

  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        int[] pixels, int offset, int scansize)
  {
    model = setupColorModel(model);
    if (bImage == null)
      {
        createImage();
      }

    Integer t = (Integer) imageProperties.get(IMAGE_TRANSPARENCY_PROPERTY);
    int transparency= t.intValue();

    if (targetColorModel.equals(model))
      {
        transparency = transferPixels(x, y, w, h, model, pixels, offset,
                                      scansize, transparency);
      }
    else if (model instanceof IndexColorModel
             && targetColorModel.equals(ColorModel.getRGBdefault()))
      {
        transparency = convertIndexColorModelToSRGB(x, y, w, h,
                                                    (IndexColorModel) model,
                                                    pixels, offset, scansize,
                                                    transparency);
      }
    else
      {
        transparency = convertPixels(x, y, w, h, model, pixels, offset,
                                     scansize, transparency);
      }

    imageProperties.put(IMAGE_TRANSPARENCY_PROPERTY,
                        Integer.valueOf(transparency));

  }

  /**
   * Initialize the color model for this setPixels run: <br/>
   * 1. if no color model was given use the hinted color model <br/>
   * 2. if no color model was given and non was hinted use the default sRGB color model. <br/>
   * Also:<br/>
   * If no target color model was set use the color model of the given pixels.
   * @param model
   * @return
   */
  private ColorModel setupColorModel(ColorModel model)
  {
    // If the given color model is null use the previously hinted color model.
    if (model == null)
      model = colorModel;

    // If no color model was given or hinted use default sRGB.
    if (model == null)
      model = ColorModel.getRGBdefault();

    // If no specific color model was requested for the target use the current
    // pixels model.
    if (targetColorModel == null)
      targetColorModel = model;
    targetColorModel = ColorModel.getRGBdefault();
    return model;
  }

  /**
   * Creates the image instance into which the pixel data is converted.
   */
  private void createImage()
  {
    if (imageProperties == null)
      {
        imageProperties = new Hashtable();
      }

    imageProperties.put(IMAGE_TRANSPARENCY_PROPERTY,
                        Integer.valueOf(Transparency.OPAQUE));
    imageProperties.put(IMAGE_PROPERTIES_PROPERTY, imageProperties);

    // For the sRGB case let the GraphicsEnvironment create an image for us.
    if (ColorModel.getRGBdefault().equals(targetColorModel))
      {
        bImage = GraphicsEnvironment.getLocalGraphicsEnvironment()
                                    .getDefaultScreenDevice()
                                    .getDefaultConfiguration()
             .createCompatibleImage(width, height, Transparency.TRANSLUCENT);
      }
    else
      {
        WritableRaster raster =
          targetColorModel.createCompatibleWritableRaster(width, height);
        bImage = new BufferedImage(targetColorModel, raster, false,
                                   imageProperties);
      }
    image.setRealImage(bImage);
    return;
  }

  /**
   * Transfers pixels into a raster of the same color model.
   *
   * @param x the X coordinate of the source pixel rectangle
   * @param y the Y coordinate of the source pixel rectangle
   * @param w the width of the source pixel rectangle
   * @param h the height of the source pixel rectangle
   * @param model the color model of the source pixels
   * @param pixels the pixel data
   * @param offset the offset in the pixel array
   * @param scansize the scanline size
   * @param transparency the assumed transparency
   *
   * @return the determined transparency
   */
  private int transferPixels(int x, int y, int w, int h, ColorModel model,
                             byte[] pixels, int offset, int scansize,
                             int transparency)
  {
    // If we have the same color model, then we can simply drop
    // the pixel value into the target raster.
    bImage.getRaster().setDataElements(x, y, w, h, pixels);

    for (int yy = 0; yy < h; yy++)
      {
        for (int xx = 0; xx < w; xx++)
          {
            int pixel = 0xFF & pixels[yy * scansize + xx + offset];
            int alpha = model.getAlpha(pixel);
            transparency = updateTransparency(alpha, transparency);
          }
      }
    return transparency;
  }

  /**
   * Transfers pixels into a raster of the same color model.
   *
   * @param x the X coordinate of the source pixel rectangle
   * @param y the Y coordinate of the source pixel rectangle
   * @param w the width of the source pixel rectangle
   * @param h the height of the source pixel rectangle
   * @param model the color model of the source pixels
   * @param pixels the pixel data
   * @param offset the offset in the pixel array
   * @param scansize the scanline size
   * @param transparency the assumed transparency
   *
   * @return the determined transparency
   */
  private int transferPixels(int x, int y, int w, int h, ColorModel model,
                             int[] pixels, int offset, int scansize,
                             int transparency)
  {
    // If we have the same color model, then we can simply drop
    // the pixel value into the target raster.
    bImage.getRaster().setDataElements(x, y, w, h, pixels);

    for (int yy = 0; yy < h; yy++)
      {
        for (int xx = 0; xx < w; xx++)
          {
            int pixel = pixels[yy * scansize + xx + offset];
            int alpha = model.getAlpha(pixel);
            transparency = updateTransparency(alpha, transparency);
          }
      }
    return transparency;
  }

  /**
   * Converts pixel from one color model to another, and stores them in the
   * target image.
   *
   * @param x the X coordinate of the source pixel rectangle
   * @param y the Y coordinate of the source pixel rectangle
   * @param w the width of the source pixel rectangle
   * @param h the height of the source pixel rectangle
   * @param model the color model of the source pixels
   * @param pixels the pixel data
   * @param offset the offset in the pixel array
   * @param scansize the scanline size
   * @param transparency the assumed transparency
   *
   * @return the determined transparency
   */
  private int convertPixels(int x, int y, int w, int h, ColorModel model,
                            byte[] pixels, int offset, int scansize,
                            int transparency)
  {
    // If the color models are not the same, we must convert the
    // pixel values from one model to the other.
    Object dataEl = null;
    // Convert pixels to the destination color model.
    for (int yy = 0; yy < h; yy++)
      {
        for (int xx = 0; xx < w; xx++)
          {
            int pixel = 0xFF & pixels[yy * scansize + xx + offset];
            int rgb = model.getRGB(pixel);
            int alpha = model.getAlpha(pixel);
            transparency = updateTransparency(alpha, transparency);
            dataEl = targetColorModel.getDataElements(rgb, dataEl);
            bImage.getRaster().setDataElements(x + xx, y + yy, dataEl);
          }
      }
    return transparency;
  }

  /**
   * Converts pixel from one color model to another, and stores them in the
   * target image.
   *
   * @param x the X coordinate of the source pixel rectangle
   * @param y the Y coordinate of the source pixel rectangle
   * @param w the width of the source pixel rectangle
   * @param h the height of the source pixel rectangle
   * @param model the color model of the source pixels
   * @param pixels the pixel data
   * @param offset the offset in the pixel array
   * @param scansize the scanline size
   * @param transparency the assumed transparency
   *
   * @return the determined transparency
   */
  private int convertPixels(int x, int y, int w, int h, ColorModel model,
                            int[] pixels, int offset, int scansize,
                            int transparency)
  {
    // If the color models are not the same, we must convert the
    // pixel values from one model to the other.
    Object dataEl = null;
    // Convert pixels to the destination color model.
    for (int yy = 0; yy < h; yy++)
      {
        for (int xx = 0; xx < w; xx++)
          {
            int pixel = pixels[yy * scansize + xx + offset];
            int rgb = model.getRGB(pixel);
            int alpha = model.getAlpha(pixel);
            transparency = updateTransparency(alpha, transparency);
            dataEl = targetColorModel.getDataElements(rgb, dataEl);
            bImage.getRaster().setDataElements(x + xx, y + yy, dataEl);
          }
      }
    return transparency;
  }

  /**
   * Converts pixels from an index color model to the target image.
   *
   * @param x the X coordinate of the source pixel rectangle
   * @param y the Y coordinate of the source pixel rectangle
   * @param w the width of the source pixel rectangle
   * @param h the height of the source pixel rectangle
   * @param model the color model of the source pixels
   * @param pixels the pixel data
   * @param offset the offset in the pixel array
   * @param scansize the scanline size
   * @param transparency the assumed transparency
   *
   * @return the determined transparency
   */
  private int convertIndexColorModelToSRGB(int x, int y, int w, int h,
                                           IndexColorModel model,
                                           byte[] pixels, int offset,
                                           int scansize, int transparency)
  {

    int mapSize = model.getMapSize();
    int[] colorMap = new int[mapSize];
    for(int i=0; i < mapSize; i++)
      {
        colorMap[i] = model.getRGB(i);
      }

    WritableRaster raster = bImage.getRaster();
    SinglePixelPackedSampleModel sampleMode =
      (SinglePixelPackedSampleModel) raster.getSampleModel();
    DataBuffer dataBuffer = (DataBuffer) raster.getDataBuffer();

    int rasterOffset = sampleMode.getOffset(x,y)+dataBuffer.getOffset();
    int rasterScanline = sampleMode.getScanlineStride();

    for (int yy = 0; yy < h; yy++)
      {
        int xoffset = offset;
        for (int xx = 0; xx < w; xx++)
          {
            int argb  = colorMap[(pixels[xoffset++] & 0xFF)];
            dataBuffer.setElem(rasterOffset+xx, argb);
            int alpha = (argb >>> 24);
            transparency = updateTransparency(alpha, transparency);
          }
        offset += scansize;
        rasterOffset += rasterScanline;
      }

    return transparency;
  }

  /**
   * Converts pixels from an index color model to the target image.
   *
   * @param x the X coordinate of the source pixel rectangle
   * @param y the Y coordinate of the source pixel rectangle
   * @param w the width of the source pixel rectangle
   * @param h the height of the source pixel rectangle
   * @param model the color model of the source pixels
   * @param pixels the pixel data
   * @param offset the offset in the pixel array
   * @param scansize the scanline size
   * @param transparency the assumed transparency
   *
   * @return the determined transparency
   */
  private int convertIndexColorModelToSRGB(int x, int y, int w, int h,
                                           IndexColorModel model, int[] pixels,
                                           int offset, int scansize,
                                           int transparency)
  {
    int mapSize = model.getMapSize();
    int[] colorMap = new int[mapSize];
    for(int i=0; i < mapSize; i++)
      {
        colorMap[i] = model.getRGB(i);
      }

    WritableRaster raster = bImage.getRaster();
    SinglePixelPackedSampleModel sampleMode =
      (SinglePixelPackedSampleModel) raster.getSampleModel();
    DataBuffer dataBuffer = (DataBuffer)raster.getDataBuffer();

    int rasterOffset = sampleMode.getOffset(x, y) + dataBuffer.getOffset();
    int rasterScanline = sampleMode.getScanlineStride();

    for (int yy = 0; yy < h; yy++)
      {
        int xoffset = offset;
        for (int xx = 0; xx < w; xx++)
          {
            int argb  = colorMap[pixels[xoffset++]];
            dataBuffer.setElem(rasterOffset + xx, argb);
            int alpha = (argb >>> 24);
            transparency = updateTransparency(alpha, transparency);
          }
        offset += scansize;
        rasterOffset += rasterScanline;
      }

    return transparency;
  }

  /**
   * Updates the transparency information according to the alpha pixel value.
   *
   * @param alpha the alpha pixel value
   * @param transparency the old transparency
   *
   * @return the updated transparency
   */
  private int updateTransparency(int alpha, int transparency)
  {
    if (alpha != 0xFF)
      {
        if (alpha == 0x00 && transparency <= Transparency.BITMASK)
          {
            transparency = Transparency.BITMASK;
          }
        else if (transparency < Transparency.TRANSLUCENT)
          {
            transparency = Transparency.TRANSLUCENT;
          }
      }
    return transparency;
  }

  public void imageComplete(int status)
  {
    image.notifyObservers(ImageObserver.ALLBITS, 0, 0, width, height);
  }

  public void setTargetColorModel(ColorModel model)
  {
    targetColorModel = model;
  }

  public Image getImage()
  {
    return image;
  }
}
