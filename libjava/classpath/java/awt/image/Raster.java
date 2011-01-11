/* Copyright (C) 2000, 2002, 2003, 2006,  Free Software Foundation

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

import gnu.java.lang.CPStringBuilder;

import java.awt.Point;
import java.awt.Rectangle;

/**
 * A rectangular collection of pixels composed from a {@link DataBuffer} which
 * stores the pixel values, and a {@link SampleModel} which is used to retrieve
 * the pixel values.
 *
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public class Raster
{
  /** The sample model used to access the pixel values. */
  protected SampleModel sampleModel;

  /** The data buffer used to store the pixel values. */
  protected DataBuffer dataBuffer;

  /** The x-coordinate of the top left corner of the raster. */
  protected int minX;

  /** The y-coordinate of the top left corner of the raster. */
  protected int minY;

  /** The width of the raster. */
  protected int width;

  /** The height of the raster. */
  protected int height;

  protected int sampleModelTranslateX;

  protected int sampleModelTranslateY;

  /** The number of bands. */
  protected int numBands;

  protected int numDataElements;

  /** The raster's parent. */
  protected Raster parent;

  /**
   * Creates a new raster.
   *
   * @param sampleModel  the sample model.
   * @param origin  the origin.
   */
  protected Raster(SampleModel sampleModel, Point origin)
  {
    this(sampleModel, sampleModel.createDataBuffer(), origin);
  }

  /**
   * Creates a new raster.
   *
   * @param sampleModel  the sample model.
   * @param dataBuffer  the data buffer.
   * @param origin  the origin.
   */
  protected Raster(SampleModel sampleModel, DataBuffer dataBuffer,
                   Point origin)
  {
    this(sampleModel, dataBuffer, new Rectangle(origin.x, origin.y,
         sampleModel.getWidth(), sampleModel.getHeight()), origin, null);
  }

  /**
   * Creates a new raster.
   *
   * @param sampleModel  the sample model.
   * @param dataBuffer  the data buffer.
   * @param aRegion  the raster's bounds.
   * @param sampleModelTranslate  the translation (<code>null</code> permitted).
   * @param parent  the raster's parent.
   */
  protected Raster(SampleModel sampleModel, DataBuffer dataBuffer,
      Rectangle aRegion, Point sampleModelTranslate, Raster parent)
  {
    this.sampleModel = sampleModel;
    this.dataBuffer = dataBuffer;
    this.minX = aRegion.x;
    this.minY = aRegion.y;
    this.width = aRegion.width;
    this.height = aRegion.height;

    // If sampleModelTranslate is null, use (0,0).  Methods such as
    // Raster.createRaster are specified to allow for a null argument.
    if (sampleModelTranslate != null)
    {
      this.sampleModelTranslateX = sampleModelTranslate.x;
      this.sampleModelTranslateY = sampleModelTranslate.y;
    }

    this.numBands = sampleModel.getNumBands();
    this.numDataElements = sampleModel.getNumDataElements();
    this.parent = parent;
  }

  /**
   * Creates an interleaved raster using the specified data type.
   *
   * @param dataType  the data type.
   * @param w  the width.
   * @param h  the height.
   * @param bands  the number of bands.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createInterleavedRaster(int dataType,
      int w, int h, int bands, Point location)
  {
    int[] bandOffsets = new int[bands];
    // TODO: Maybe not generate this every time.
    for (int b = 0; b < bands; b++)
      bandOffsets[b] = b;

    int scanlineStride = bands * w;
    return createInterleavedRaster(dataType, w, h, scanlineStride, bands,
                                   bandOffsets, location);
  }

  /**
   * Creates an interleaved raster.
   *
   * @param dataType  the data type.
   * @param w  the width.
   * @param h  the height.
   * @param scanlineStride  the number of data elements from a sample on one
   *     row to the corresponding sample on the next row.
   * @param pixelStride  the number of elements from a sample in one pixel to
   *     the corresponding sample in the next pixel.
   * @param bandOffsets  the band offsets.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createInterleavedRaster(int dataType,
      int w, int h, int scanlineStride, int pixelStride, int[] bandOffsets,
      Point location)
  {
    SampleModel sm = new ComponentSampleModel(dataType, w, h, pixelStride,
        scanlineStride, bandOffsets);
    return createWritableRaster(sm, location);
  }

  /**
   * Creates a new banded raster.
   *
   * @param dataType  the data type.
   * @param w  the width.
   * @param h  the height.
   * @param bands  the number of bands.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createBandedRaster(int dataType, int w, int h,
      int bands, Point location)
  {
    SampleModel sm = new BandedSampleModel(dataType, w, h, bands);
    return createWritableRaster(sm, location);
  }

  /**
   * Creates a new banded raster.
   *
   * @param dataType  the data type.
   * @param w  the width.
   * @param h  the height.
   * @param scanlineStride  the number of data elements from a sample on one
   *     row to the corresponding sample on the next row.
   * @param bankIndices  the index for each bank.
   * @param bandOffsets  the offset for each band.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createBandedRaster(int dataType, int w, int h,
      int scanlineStride, int[] bankIndices, int[] bandOffsets, Point location)
  {
    SampleModel sm = new BandedSampleModel(dataType, w, h, scanlineStride,
                                           bankIndices, bandOffsets);
    return createWritableRaster(sm, location);
  }

  /**
   * Creates a new packed raster.
   *
   * @param dataType  the data type.
   * @param w  the width.
   * @param h  the height.
   * @param bandMasks  the bit mask for each band.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createPackedRaster(int dataType, int w, int h,
      int[] bandMasks, Point location)
  {
    SampleModel sm = new SinglePixelPackedSampleModel(dataType, w, h,
                                                     bandMasks);
    return createWritableRaster(sm, location);
  }

  /**
   * Creates a new raster.
   *
   * @param dataType  the data type.
   * @param w  the width.
   * @param h  the height.
   * @param bands  the number of bands.
   * @param bitsPerBand  the number of bits per band.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createPackedRaster(int dataType,
      int w, int h, int bands, int bitsPerBand, Point location)
  {
    if (bands <= 0 || (bands * bitsPerBand > getTypeBits(dataType)))
      throw new IllegalArgumentException();

    SampleModel sm;

    if (bands == 1)
      sm = new MultiPixelPackedSampleModel(dataType, w, h, bitsPerBand);
    else
      {
        int[] bandMasks = new int[bands];
        int mask = 0x1;
        for (int bits = bitsPerBand; --bits != 0;)
          mask = (mask << 1) | 0x1;
        for (int i = 0; i < bands; i++)
          {
            bandMasks[i] = mask;
            mask <<= bitsPerBand;
          }

        sm = new SinglePixelPackedSampleModel(dataType, w, h, bandMasks);
      }
    return createWritableRaster(sm, location);
  }

  /**
   * Creates a new interleaved raster.
   *
   * @param dataBuffer  the data buffer.
   * @param w  the width.
   * @param h  the height.
   * @param scanlineStride  the number of data elements from a sample on one
   *     row to the corresponding sample on the next row.
   * @param pixelStride  the number of elements from a sample in one pixel to
   *     the corresponding sample in the next pixel.
   * @param bandOffsets  the offset for each band.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createInterleavedRaster(DataBuffer dataBuffer,
      int w, int h, int scanlineStride, int pixelStride, int[] bandOffsets,
      Point location)
  {
    SampleModel sm = new ComponentSampleModel(dataBuffer.getDataType(),
        w, h, pixelStride, scanlineStride, bandOffsets);
    return createWritableRaster(sm, dataBuffer, location);
  }

  /**
   * Creates a new banded raster.
   *
   * @param dataBuffer  the data buffer.
   * @param w  the width.
   * @param h  the height.
   * @param scanlineStride  the number of data elements from a sample on one
   *     row to the corresponding sample on the next row.
   * @param bankIndices  the index for each bank.
   * @param bandOffsets  the band offsets.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createBandedRaster(DataBuffer dataBuffer,
      int w, int h, int scanlineStride, int[] bankIndices, int[] bandOffsets,
      Point location)
  {
    SampleModel sm = new BandedSampleModel(dataBuffer.getDataType(),
        w, h, scanlineStride, bankIndices, bandOffsets);
    return createWritableRaster(sm, dataBuffer, location);
  }

  /**
   * Creates a new packed raster.
   *
   * @param dataBuffer  the data buffer.
   * @param w  the width.
   * @param h  the height.
   * @param scanlineStride  the number of data elements from a sample on one
   *     row to the corresponding sample on the next row.
   * @param bandMasks  the bit mask for each band.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createPackedRaster(DataBuffer dataBuffer,
      int w, int h, int scanlineStride, int[] bandMasks, Point location)
 {
    SampleModel sm = new SinglePixelPackedSampleModel(dataBuffer.getDataType(),
        w, h, scanlineStride, bandMasks);
    return createWritableRaster(sm, dataBuffer, location);
  }

  /**
   * Creates a new packed raster.
   *
   * @param dataBuffer  the data buffer.
   * @param w  the width.
   * @param h  the height.
   * @param bitsPerPixel  the number of bits per pixel.
   * @param location
   *
   * @return The new raster.
   */
  public static WritableRaster createPackedRaster(DataBuffer dataBuffer,
      int w, int h, int bitsPerPixel, Point location)
  {
    SampleModel sm = new MultiPixelPackedSampleModel(dataBuffer.getDataType(),
        w, h, bitsPerPixel);
    return createWritableRaster(sm, dataBuffer, location);
  }

  /**
   * Creates a new raster.
   *
   * @param sm  the sample model.
   * @param db  the data buffer.
   * @param location
   *
   * @return The new raster.
   */
  public static Raster createRaster(SampleModel sm, DataBuffer db,
                                    Point location)
  {
    return new Raster(sm, db, location);
  }

  /**
   * Creates a new writable raster.
   *
   * @param sm  the sample model.
   * @param location
   *
   * @return The new writable raster.
   */
  public static WritableRaster createWritableRaster(SampleModel sm,
                                                    Point location)
  {
    return new WritableRaster(sm, location);
  }

  /**
   * Creates a new writable raster.
   *
   * @param sm  the sample model.
   * @param db  the data buffer.
   * @param location
   *
   * @return The new writable raster.
   */
  public static WritableRaster createWritableRaster(SampleModel sm,
      DataBuffer db, Point location)
  {
    return new WritableRaster(sm, db, location);
  }

  /**
   * Returns the raster's parent.
   *
   * @return The raster's parent.
   */
  public Raster getParent()
  {
    return parent;
  }

  /**
   * Returns the x-translation.
   *
   * @return The x-translation.
   */
  public final int getSampleModelTranslateX()
  {
    return sampleModelTranslateX;
  }

  /**
   * Returns the y-translation.
   *
   * @return The y-translation.
   */
  public final int getSampleModelTranslateY()
  {
    return sampleModelTranslateY;
  }

  /**
   * Creates a new writable raster that is compatible with this raster.
   *
   * @return A new writable raster.
   */
  public WritableRaster createCompatibleWritableRaster()
  {
    return new WritableRaster(getSampleModel(), new Point(minX, minY));
  }

  /**
   * Creates a new writable raster that is compatible with this raster.
   *
   * @param w  the width.
   * @param h  the height.
   *
   * @return A new writable raster.
   */
  public WritableRaster createCompatibleWritableRaster(int w, int h)
  {
    return createCompatibleWritableRaster(minX, minY, w, h);
  }

  /**
   * Creates a new writable raster that is compatible with this raster, with
   * the specified bounds.
   *
   * @param rect  the raster bounds.
   *
   * @return A new writable raster.
   */
  public WritableRaster createCompatibleWritableRaster(Rectangle rect)
  {
    return createCompatibleWritableRaster(rect.x, rect.y,
                                          rect.width, rect.height);
  }

  /**
   * Creates a new writable raster that is compatible with this raster, with
   * the specified bounds.
   *
   * @param x  the x-coordinate of the top-left corner of the raster.
   * @param y  the y-coordinate of the top-left corner of the raster.
   * @param w  the raster width.
   * @param h  the raster height.
   *
   * @return A new writable raster.
   */
  public WritableRaster createCompatibleWritableRaster(int x, int y,
                                                       int w, int h)
  {
    SampleModel sm = getSampleModel().createCompatibleSampleModel(w, h);
    return new WritableRaster(sm, sm.createDataBuffer(), new Point(x, y));
  }

  public Raster createTranslatedChild(int childMinX, int childMinY) {
    int tcx = sampleModelTranslateX - minX + childMinX;
    int tcy = sampleModelTranslateY - minY + childMinY;

    return new Raster(sampleModel, dataBuffer,
                      new Rectangle(childMinX, childMinY, width, height),
                      new Point(tcx, tcy), this);
  }

  public Raster createChild(int parentX, int parentY, int width,
                            int height, int childMinX, int childMinY,
                            int[] bandList)
  {
    if (parentX < minX || parentX + width > minX + this.width
        || parentY < minY || parentY + height > minY + this.height)
      throw new RasterFormatException("Child raster extends beyond parent");

    SampleModel sm = (bandList == null) ?
      sampleModel :
      sampleModel.createSubsetSampleModel(bandList);

    /*
        data origin
       /
      +-------------------------
      |\. __ parent trans
      | \`.
      |  \ `.    parent origin
      |   \  `. /
      |   /\   +-------- - -
      |trans\ /<\-- deltaTrans
      |child +-+-\---- - -
      |     /|`|  \__ parent [x, y]
      |child | |`. \
      |origin| :  `.\
      |      |    / `\
      |      :   /    +
      | child [x, y]

      parent_xy - parent_trans = child_xy - child_trans

      child_trans = parent_trans + child_xy - parent_xy
    */

    return new Raster(sm, dataBuffer,
        new Rectangle(childMinX, childMinY, width, height),
        new Point(sampleModelTranslateX + childMinX - parentX,
                  sampleModelTranslateY + childMinY - parentY),
        this);
  }

  /**
   * Returns a new rectangle containing the bounds of this raster.
   *
   * @return A new rectangle containing the bounds of this raster.
   */
  public Rectangle getBounds()
  {
    return new Rectangle(minX, minY, width, height);
  }

  /**
   * Returns the x-coordinate of the top left corner of the raster.
   *
   * @return The x-coordinate of the top left corner of the raster.
   */
  public final int getMinX()
  {
    return minX;
  }

  /**
   * Returns the t-coordinate of the top left corner of the raster.
   *
   * @return The t-coordinate of the top left corner of the raster.
   */
  public final int getMinY()
  {
    return minY;
  }

  /**
   * Returns the width of the raster.
   *
   * @return The width of the raster.
   */
  public final int getWidth()
  {
    return width;
  }

  /**
   * Returns the height of the raster.
   *
   * @return The height of the raster.
   */
  public final int getHeight()
  {
    return height;
  }

  /**
   * Returns the number of bands for this raster.
   *
   * @return The number of bands.
   */
  public final int getNumBands()
  {
    return numBands;
  }

  public final int getNumDataElements()
  {
    return numDataElements;
  }

  /**
   * Returns the transfer type for the raster (this is determined by the
   * raster's sample model).
   *
   * @return The transfer type.
   */
  public final int getTransferType()
  {
    return sampleModel.getTransferType();
  }

  /**
   * Returns the data buffer that stores the pixel data for this raster.
   *
   * @return The data buffer.
   */
  public DataBuffer getDataBuffer()
  {
    return dataBuffer;
  }

  /**
   * Returns the sample model that accesses the data buffer (to extract pixel
   * data) for this raster.
   *
   * @return The sample model.
   */
  public SampleModel getSampleModel()
  {
    return sampleModel;
  }

  public Object getDataElements(int x, int y, Object outData)
  {
    return sampleModel.getDataElements(x - sampleModelTranslateX,
        y - sampleModelTranslateY, outData, dataBuffer);
  }

  public Object getDataElements(int x, int y, int w, int h, Object outData)
  {
    return sampleModel.getDataElements(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, outData, dataBuffer);
  }

  /**
   * Returns an array containing the samples for the pixel at (x, y) in the
   * raster.  If <code>iArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of
   * this function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param iArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The pixel sample values.
   */
  public int[] getPixel(int x, int y, int[] iArray)
  {
    return sampleModel.getPixel(x - sampleModelTranslateX,
        y - sampleModelTranslateY, iArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples for the pixel at (x, y) in the
   * raster.  If <code>fArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of
   * this function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param fArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The pixel sample values.
   */
  public float[] getPixel(int x, int y, float[] fArray)
  {
    return sampleModel.getPixel(x - sampleModelTranslateX,
        y - sampleModelTranslateY, fArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples for the pixel at (x, y) in the
   * raster.  If <code>dArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of
   * this function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param dArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The pixel sample values.
   */
  public double[] getPixel(int x, int y, double[] dArray)
  {
    return sampleModel.getPixel(x - sampleModelTranslateX,
        y - sampleModelTranslateY, dArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples for the pixels in the region
   * specified by (x, y, w, h) in the raster.  The array is ordered by pixels
   * (that is, all the samples for the first pixel are grouped together,
   * followed by all the samples for the second pixel, and so on).
   * If <code>iArray</code> is not <code>null</code>, it will be populated
   * with the sample values and returned as the result of this function (this
   * avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param iArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The pixel sample values.
   */
  public int[] getPixels(int x, int y, int w, int h, int[] iArray)
  {
    return sampleModel.getPixels(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, iArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples for the pixels in the region
   * specified by (x, y, w, h) in the raster.  The array is ordered by pixels
   * (that is, all the samples for the first pixel are grouped together,
   * followed by all the samples for the second pixel, and so on).
   * If <code>fArray</code> is not <code>null</code>, it will be populated
   * with the sample values and returned as the result of this function (this
   * avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param fArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The pixel sample values.
   */
  public float[] getPixels(int x, int y, int w, int h, float[] fArray)
  {
    return sampleModel.getPixels(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, fArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples for the pixels in the region
   * specified by (x, y, w, h) in the raster.  The array is ordered by pixels
   * (that is, all the samples for the first pixel are grouped together,
   * followed by all the samples for the second pixel, and so on).
   * If <code>dArray</code> is not <code>null</code>, it will be populated
   * with the sample values and returned as the result of this function (this
   * avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param dArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The pixel sample values.
   */
  public double[] getPixels(int x, int y, int w, int h, double[] dArray)
  {
    return sampleModel.getPixels(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, dArray, dataBuffer);
  }

  /**
   * Returns the sample value for the pixel at (x, y) in the raster.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   *
   * @return The sample value.
   */
  public int getSample(int x, int y, int b)
  {
    return sampleModel.getSample(x - sampleModelTranslateX,
        y - sampleModelTranslateY, b, dataBuffer);
  }

  /**
   * Returns the sample value for the pixel at (x, y) in the raster.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   *
   * @return The sample value.
   *
   * @see #getSample(int, int, int)
   */
  public float getSampleFloat(int x, int y, int b)
  {
    return sampleModel.getSampleFloat(x - sampleModelTranslateX,
        y - sampleModelTranslateY, b, dataBuffer);
  }

  /**
   * Returns the sample value for the pixel at (x, y) in the raster.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   *
   * @return The sample value.
   *
   * @see #getSample(int, int, int)
   */
  public double getSampleDouble(int x, int y, int b)
  {
    return sampleModel.getSampleDouble(x - sampleModelTranslateX,
        y - sampleModelTranslateY, b, dataBuffer);
  }

  /**
   * Returns an array containing the samples from one band for the pixels in
   * the region specified by (x, y, w, h) in the raster.  If
   * <code>iArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of this
   * function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param b  the band (in the range <code>0</code> to
   *     </code>getNumBands() - 1</code>).
   * @param iArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The sample values.
   */
  public int[] getSamples(int x, int y, int w, int h, int b,
                          int[] iArray)
  {
    return sampleModel.getSamples(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, b, iArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples from one band for the pixels in
   * the region specified by (x, y, w, h) in the raster.  If
   * <code>fArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of this
   * function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param b  the band (in the range <code>0</code> to
   *     </code>getNumBands() - 1</code>).
   * @param fArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The sample values.
   */
  public float[] getSamples(int x, int y, int w, int h, int b, float[] fArray)
  {
    return sampleModel.getSamples(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, b, fArray, dataBuffer);
  }

  /**
   * Returns an array containing the samples from one band for the pixels in
   * the region specified by (x, y, w, h) in the raster.  If
   * <code>dArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of this
   * function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param b  the band (in the range <code>0</code> to
   *     </code>getNumBands() - 1</code>).
   * @param dArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   *
   * @return The sample values.
   */
  public double[] getSamples(int x, int y, int w, int h, int b,
                             double[] dArray)
  {
    return sampleModel.getSamples(x - sampleModelTranslateX,
        y - sampleModelTranslateY, w, h, b, dArray, dataBuffer);
  }

  /**
   * Create a String representing the state of this Raster.
   *
   * @return A String representing the stat of this Raster.
   */
  public String toString()
  {
    CPStringBuilder result = new CPStringBuilder();

    result.append(getClass().getName());
    result.append("[(");
    result.append(minX).append(",").append(minY).append("), ");
    result.append(width).append(" x ").append(height).append(",");
    result.append(sampleModel).append(",");
    result.append(dataBuffer);
    result.append("]");

    return result.toString();
  }

  /**
   * Returns the number of bits used to represent the specified data type.
   * Valid types are:
   * <ul>
   *   <li>{@link DataBuffer#TYPE_BYTE};</li>
   *   <li>{@link DataBuffer#TYPE_USHORT};</li>
   *   <li>{@link DataBuffer#TYPE_SHORT};</li>
   *   <li>{@link DataBuffer#TYPE_INT};</li>
   *   <li>{@link DataBuffer#TYPE_FLOAT};</li>
   *   <li>{@link DataBuffer#TYPE_DOUBLE};</li>
   * </ul>
   * This method returns 0 for invalid data types.
   *
   * @param dataType  the data type.
   *
   * @return The number of bits used to represent the specified data type.
   */
  private static int getTypeBits(int dataType)
  {
    switch (dataType)
      {
      case DataBuffer.TYPE_BYTE:
        return 8;
      case DataBuffer.TYPE_USHORT:
      case DataBuffer.TYPE_SHORT:
        return 16;
      case DataBuffer.TYPE_INT:
      case DataBuffer.TYPE_FLOAT:
        return 32;
      case DataBuffer.TYPE_DOUBLE:
        return 64;
      default:
        return 0;
      }
  }
}
