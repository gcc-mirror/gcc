/* Copyright (C) 2000, 2001, 2002, 2005, 2006,  Free Software Foundation

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
 * A <code>SampleModel</code> is used to access pixel data from a
 * {@link DataBuffer}.  This is used by the {@link Raster} class.
 *
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public abstract class SampleModel
{
  /** Width of image described. */
  protected int width;

  /** Height of image described. */
  protected int height;

  /** Number of bands in the image described.  Package-private here,
      shadowed by ComponentSampleModel. */
  protected int numBands;

  /**
   * The DataBuffer type that is used to store the data of the image
   * described.
   */
  protected int dataType;

  /**
   * Creates a new sample model with the specified attributes.
   *
   * @param dataType  the data type (one of {@link DataBuffer#TYPE_BYTE},
   *   {@link DataBuffer#TYPE_USHORT}, {@link DataBuffer#TYPE_SHORT},
   *   {@link DataBuffer#TYPE_INT}, {@link DataBuffer#TYPE_FLOAT},
   *   {@link DataBuffer#TYPE_DOUBLE} or {@link DataBuffer#TYPE_UNDEFINED}).
   * @param w  the width in pixels (must be greater than zero).
   * @param h  the height in pixels (must be greater than zero).
   * @param numBands  the number of bands (must be greater than zero).
   *
   * @throws IllegalArgumentException if <code>dataType</code> is not one of
   *   the listed values.
   * @throws IllegalArgumentException if <code>w</code> is less than or equal
   *   to zero.
   * @throws IllegalArgumentException if <code>h</code> is less than or equal
   *   to zero.
   * @throws IllegalArgumentException if <code>w * h</code> is greater than
   *   {@link Integer#MAX_VALUE}.
   */
  public SampleModel(int dataType, int w, int h, int numBands)
  {
    if (dataType != DataBuffer.TYPE_UNDEFINED)
      if (dataType < DataBuffer.TYPE_BYTE || dataType > DataBuffer.TYPE_DOUBLE)
        throw new IllegalArgumentException("Unrecognised 'dataType' argument.");

    if ((w <= 0) || (h <= 0))
      throw new IllegalArgumentException((w <= 0 ? " width<=0" : " width is ok")
          + (h <= 0 ? " height<=0" : " height is ok"));

    long area = (long) w * (long) h;
    if (area > Integer.MAX_VALUE)
      throw new IllegalArgumentException("w * h exceeds Integer.MAX_VALUE.");

    if (numBands <= 0)
      throw new IllegalArgumentException("Requires numBands > 0.");

    this.dataType = dataType;
    this.width = w;
    this.height = h;
    this.numBands = numBands;
  }

  /**
   * Returns the width of the pixel data accessible via this
   * <code>SampleModel</code>.
   *
   * @return The width.
   *
   * @see #getHeight()
   */
  public final int getWidth()
  {
    return width;
  }

  /**
   * Returns the height of the pixel data accessible via this
   * <code>SampleModel</code>.
   *
   * @return The height.
   *
   * @see #getWidth()
   */
  public final int getHeight()
  {
    return height;
  }

  /**
   * Returns the number of bands for this <code>SampleModel</code>.
   *
   * @return The number of bands.
   */
  public final int getNumBands()
  {
    return numBands;
  }

  public abstract int getNumDataElements();

  /**
   * Returns the type of the {@link DataBuffer} that this
   * <code>SampleModel</code> accesses.
   *
   * @return The data buffer type.
   */
  public final int getDataType()
  {
    return dataType;
  }

  public int getTransferType()
  {
    // FIXME: Is this a reasonable default implementation?
    return dataType;
  }

  /**
   * Returns an array containing the samples for the pixel at (x, y) in the
   * specified data buffer.  If <code>iArray</code> is not <code>null</code>,
   * it will be populated with the sample values and returned as the result of
   * this function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param iArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The pixel sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    if (iArray == null)
      iArray = new int[numBands];
    for (int b = 0; b < numBands; b++)
      iArray[b] = getSample(x, y, b, data);
    return iArray;
  }

  /**
   *
   * This method is provided as a faster alternative to getPixel(),
   * that can be used when there is no need to decode the pixel into
   * separate sample values.
   *
   * @param obj An array to return the pixel data in. If null, an
   * array of the right type and size will be created.
   *
   * @return A single pixel as an array object of a primitive type,
   * based on the transfer type. Eg. if transfer type is
   * DataBuffer.TYPE_USHORT, then a short[] object is returned.
   */
  public abstract Object getDataElements(int x, int y, Object obj,
                                         DataBuffer data);


  public Object getDataElements(int x, int y, int w, int h, Object obj,
                                DataBuffer data)
  {
    int size = w * h;
    int numDataElements = getNumDataElements();
    int dataSize = numDataElements * size;

    if (obj == null)
      {
        switch (getTransferType())
          {
          case DataBuffer.TYPE_BYTE:
            obj = new byte[dataSize];
            break;
          case DataBuffer.TYPE_USHORT:
            obj = new short[dataSize];
            break;
          case DataBuffer.TYPE_INT:
            obj = new int[dataSize];
            break;
          default:
            // Seems like the only sensible thing to do.
            throw new ClassCastException();
          }
      }
    Object pixelData = null;
    int outOffset = 0;
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            pixelData = getDataElements(xx, yy, pixelData, data);
            System.arraycopy(pixelData, 0, obj, outOffset,
                             numDataElements);
            outOffset += numDataElements;
          }
      }
    return obj;
  }

  public abstract void setDataElements(int x, int y, Object obj,
                                       DataBuffer data);

  public void setDataElements(int x, int y, int w, int h,
                              Object obj, DataBuffer data)
  {
    int numDataElements = getNumDataElements();

    Object pixelData;
    switch (getTransferType())
      {
      case DataBuffer.TYPE_BYTE:
        pixelData = new byte[numDataElements];
        break;
      case DataBuffer.TYPE_USHORT:
      case DataBuffer.TYPE_SHORT:
        pixelData = new short[numDataElements];
        break;
      case DataBuffer.TYPE_INT:
        pixelData = new int[numDataElements];
        break;
      case DataBuffer.TYPE_FLOAT:
        pixelData = new float[numDataElements];
        break;
      case DataBuffer.TYPE_DOUBLE:
        pixelData = new double[numDataElements];
        break;
      default:
        // The RI silently igores invalid types.
        pixelData = null;
      }

    int inOffset = 0;
    if (pixelData != null)
      {
        for (int yy=y; yy<(y+h); yy++)
          {
            for (int xx=x; xx<(x+w); xx++)
              {
                System.arraycopy(obj, inOffset, pixelData, 0, numDataElements);
                setDataElements(xx, yy, pixelData, data);
                inOffset += numDataElements;
              }
          }
      }
  }

  /**
   * Returns an array containing the samples for the pixel at (x, y) in the
   * specified data buffer.  If <code>fArray</code> is not <code>null</code>,
   * it will be populated with the sample values and returned as the result of
   * this function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param fArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The pixel sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public float[] getPixel(int x, int y, float[] fArray, DataBuffer data)
  {
    if (fArray == null)
      fArray = new float[numBands];

    for (int b = 0; b < numBands; b++)
      {
        fArray[b] = getSampleFloat(x, y, b, data);
      }
    return fArray;
  }

  /**
   * Returns an array containing the samples for the pixel at (x, y) in the
   * specified data buffer.  If <code>dArray</code> is not <code>null</code>,
   * it will be populated with the sample values and returned as the result of
   * this function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param dArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The pixel sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public double[] getPixel(int x, int y, double[] dArray, DataBuffer data) {
    if (dArray == null)
      dArray = new double[numBands];
    for (int b = 0; b < numBands; b++)
      {
        dArray[b] = getSampleDouble(x, y, b, data);
      }
    return dArray;
  }

  /**
   * Returns an array containing the samples for the pixels in the region
   * specified by (x, y, w, h) in the specified data buffer.  The array is
   * ordered by pixels (that is, all the samples for the first pixel are
   * grouped together, followed by all the samples for the second pixel, and so
   * on).  If <code>iArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of this
   * function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param iArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The pixel sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
                         DataBuffer data)
  {
    int size = w * h;
    int outOffset = 0;
    int[] pixel = null;
    if (iArray == null)
      iArray = new int[w * h * numBands];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            pixel = getPixel(xx, yy, pixel, data);
            System.arraycopy(pixel, 0, iArray, outOffset, numBands);
            outOffset += numBands;
          }
      }
    return iArray;
  }

  /**
   * Returns an array containing the samples for the pixels in the region
   * specified by (x, y, w, h) in the specified data buffer.  The array is
   * ordered by pixels (that is, all the samples for the first pixel are
   * grouped together, followed by all the samples for the second pixel, and so
   * on).  If <code>fArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of this
   * function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param fArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The pixel sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public float[] getPixels(int x, int y, int w, int h, float[] fArray,
                           DataBuffer data)
  {
    int size = w * h;
    int outOffset = 0;
    float[] pixel = null;
    if (fArray == null) fArray = new float[w * h * numBands];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            pixel = getPixel(xx, yy, pixel, data);
            System.arraycopy(pixel, 0, fArray, outOffset, numBands);
            outOffset += numBands;
          }
      }
    return fArray;
  }

  /**
   * Returns an array containing the samples for the pixels in the region
   * specified by (x, y, w, h) in the specified data buffer.  The array is
   * ordered by pixels (that is, all the samples for the first pixel are
   * grouped together, followed by all the samples for the second pixel, and so
   * on).  If <code>dArray</code> is not <code>null</code>, it will be
   * populated with the sample values and returned as the result of this
   * function (this avoids allocating a new array instance).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param dArray  an array to populate with the sample values and return as
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The pixel sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public double[] getPixels(int x, int y, int w, int h, double[] dArray,
                            DataBuffer data)
  {
    int size = w * h;
    int outOffset = 0;
    double[] pixel = null;
    if (dArray == null)
      dArray = new double[w * h * numBands];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            pixel = getPixel(xx, yy, pixel, data);
            System.arraycopy(pixel, 0, dArray, outOffset, numBands);
            outOffset += numBands;
          }
      }
    return dArray;
  }

  /**
   * Returns the sample value for the pixel at (x, y) in the specified data
   * buffer.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The sample value.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public abstract int getSample(int x, int y, int b, DataBuffer data);

  /**
   * Returns the sample value for the pixel at (x, y) in the specified data
   * buffer.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The sample value.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   *
   * @see #getSample(int, int, int, DataBuffer)
   */
  public float getSampleFloat(int x, int y, int b, DataBuffer data)
  {
    return getSample(x, y, b, data);
  }

  /**
   * Returns the sample value for the pixel at (x, y) in the specified data
   * buffer.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The sample value.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   *
   * @see #getSample(int, int, int, DataBuffer)
   */
  public double getSampleDouble(int x, int y, int b, DataBuffer data)
  {
    return getSampleFloat(x, y, b, data);
  }

  /**
   * Returns an array containing the samples from one band for the pixels in
   * the region specified by (x, y, w, h) in the specified data buffer.  If
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
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public int[] getSamples(int x, int y, int w, int h, int b,
                          int[] iArray, DataBuffer data)
  {
    int size = w * h;
    int outOffset = 0;
    if (iArray == null)
      iArray = new int[size];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            iArray[outOffset++] = getSample(xx, yy, b, data);
          }
      }
    return iArray;
  }

  /**
   * Returns an array containing the samples from one band for the pixels in
   * the region specified by (x, y, w, h) in the specified data buffer.  If
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
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public float[] getSamples(int x, int y, int w, int h, int b,
                            float[] fArray, DataBuffer data)
  {
    int size = w * h;
    int outOffset = 0;
    if (fArray == null)
      fArray = new float[size];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            fArray[outOffset++] = getSampleFloat(xx, yy, b, data);
          }
      }
    return fArray;
  }

  /**
   * Returns an array containing the samples from one band for the pixels in
   * the region specified by (x, y, w, h) in the specified data buffer.  If
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
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @return The sample values.
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public double[] getSamples(int x, int y, int w, int h, int b,
                             double[] dArray, DataBuffer data)
  {
    int size = w * h;
    int outOffset = 0;
    if (dArray == null)
      dArray = new double[size];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            dArray[outOffset++] = getSampleDouble(xx, yy, b, data);
          }
      }
    return dArray;
  }

  /**
   * Sets the samples for the pixel at (x, y) in the specified data buffer to
   * the specified values.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param iArray  the sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>iArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    for (int b = 0; b < numBands; b++)
      setSample(x, y, b, iArray[b], data);
  }

  /**
   * Sets the samples for the pixel at (x, y) in the specified data buffer to
   * the specified values.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param fArray  the sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>fArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setPixel(int x, int y, float[] fArray, DataBuffer data)
  {
    for (int b = 0; b < numBands; b++)
      setSample(x, y, b, fArray[b], data);
  }

  /**
   * Sets the samples for the pixel at (x, y) in the specified data buffer to
   * the specified values.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param dArray  the sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>dArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setPixel(int x, int y, double[] dArray, DataBuffer data)
  {
    for (int b = 0; b < numBands; b++)
      setSample(x, y, b, dArray[b], data);
  }

  /**
   * Sets the sample values for the pixels in the region specified by
   * (x, y, w, h) in the specified data buffer.  The array is
   * ordered by pixels (that is, all the samples for the first pixel are
   * grouped together, followed by all the samples for the second pixel, and so
   * on).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param iArray  the pixel sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>iArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setPixels(int x, int y, int w, int h, int[] iArray,
                        DataBuffer data)
  {
    int inOffset = 0;
    int[] pixel = new int[numBands];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            System.arraycopy(iArray, inOffset, pixel, 0, numBands);
            setPixel(xx, yy, pixel, data);
            inOffset += numBands;
          }
      }
  }

  /**
   * Sets the sample values for the pixels in the region specified by
   * (x, y, w, h) in the specified data buffer.  The array is
   * ordered by pixels (that is, all the samples for the first pixel are
   * grouped together, followed by all the samples for the second pixel, and so
   * on).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param fArray  the pixel sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>fArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setPixels(int x, int y, int w, int h, float[] fArray,
                        DataBuffer data)
  {
    int inOffset = 0;
    float[] pixel = new float[numBands];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            System.arraycopy(fArray, inOffset, pixel, 0, numBands);
            setPixel(xx, yy, pixel, data);
            inOffset += numBands;
          }
      }
  }

  /**
   * Sets the sample values for the pixels in the region specified by
   * (x, y, w, h) in the specified data buffer.  The array is
   * ordered by pixels (that is, all the samples for the first pixel are
   * grouped together, followed by all the samples for the second pixel, and so
   * on).
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param dArray  the pixel sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>dArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setPixels(int x, int y, int w, int h, double[] dArray,
                        DataBuffer data)
  {
    int inOffset = 0;
    double[] pixel = new double[numBands];
    for (int yy = y; yy < (y + h); yy++)
      {
        for (int xx = x; xx < (x + w); xx++)
          {
            System.arraycopy(dArray, inOffset, pixel, 0, numBands);
            setPixel(xx, yy, pixel, data);
            inOffset += numBands;
          }
      }
  }

  /**
   * Sets the sample value for a band for the pixel at (x, y) in the
   * specified data buffer.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   * @param s  the sample value.
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public abstract void setSample(int x, int y, int b, int s,
                                 DataBuffer data);

  /**
   * Sets the sample value for a band for the pixel at (x, y) in the
   * specified data buffer.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   * @param s  the sample value.
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public void setSample(int x, int y, int b, float s,
                        DataBuffer data)
  {
    setSample(x, y, b, (int) s, data);
  }

  /**
   * Sets the sample value for a band for the pixel at (x, y) in the
   * specified data buffer.
   *
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param b  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   * @param s  the sample value.
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public void setSample(int x, int y, int b, double s,
                        DataBuffer data)
  {
    setSample(x, y, b, (float) s, data);
  }

  /**
   * Sets the sample values for one band for the pixels in the region
   * specified by (x, y, w, h) in the specified data buffer.
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param b  the band (in the range <code>0</code> to
   *     </code>getNumBands() - 1</code>).
   * @param iArray  the sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>iArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setSamples(int x, int y, int w, int h, int b,
                         int[] iArray, DataBuffer data)
  {
    int size = w * h;
    int inOffset = 0;
    for (int yy = y; yy < (y + h); yy++)
      for (int xx = x; xx < (x + w); xx++)
        setSample(xx, yy, b, iArray[inOffset++], data);
  }

  /**
   * Sets the sample values for one band for the pixels in the region
   * specified by (x, y, w, h) in the specified data buffer.
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param b  the band (in the range <code>0</code> to
   *     </code>getNumBands() - 1</code>).
   * @param fArray  the sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>iArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setSamples(int x, int y, int w, int h, int b,
                         float[] fArray, DataBuffer data)
  {
    int size = w * h;
    int inOffset = 0;
    for (int yy = y; yy < (y + h); yy++)
      for (int xx = x; xx < (x + w); xx++)
        setSample(xx, yy, b, fArray[inOffset++], data);

  }

  /**
   * Sets the sample values for one band for the pixels in the region
   * specified by (x, y, w, h) in the specified data buffer.
   *
   * @param x  the x-coordinate of the top-left pixel.
   * @param y  the y-coordinate of the top-left pixel.
   * @param w  the width of the region of pixels.
   * @param h  the height of the region of pixels.
   * @param b  the band (in the range <code>0</code> to
   *     </code>getNumBands() - 1</code>).
   * @param dArray  the sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   *
   * @throws NullPointerException if either <code>iArray</code> or
   *     <code>data</code> is <code>null</code>.
   */
  public void setSamples(int x, int y, int w, int h, int b,
                         double[] dArray, DataBuffer data) {
    int size = w * h;
    int inOffset = 0;
    for (int yy = y; yy < (y + h); yy++)
      for (int xx = x; xx < (x + w); xx++)
        setSample(xx, yy, b, dArray[inOffset++], data);
  }

  /**
   * Creates a new <code>SampleModel</code> that is compatible with this
   * model and has the specified width and height.
   *
   * @param w  the width (in pixels).
   * @param h  the height (in pixels).
   *
   * @return The new sample model.
   */
  public abstract SampleModel createCompatibleSampleModel(int w, int h);

  /**
   * Return a SampleModel with a subset of the bands in this model.
   *
   * Selects bands.length bands from this sample model.  The bands chosen
   * are specified in the indices of bands[].  This also permits permuting
   * the bands as well as taking a subset.  Thus, giving an array with
   * 1, 2, 3, ..., numbands, will give an identical sample model.
   *
   * @param bands Array with band indices to include.
   * @return A new sample model
   */
  public abstract SampleModel createSubsetSampleModel(int[] bands);

  /**
   * Creates a new {@link DataBuffer} of the correct type and size for this
   * <code>SampleModel</code>.
   *
   * @return The data buffer.
   */
  public abstract DataBuffer createDataBuffer();

  /**
   * Returns an array containing the size (in bits) for each band accessed by
   * the <code>SampleModel</code>.
   *
   * @return An array.
   *
   * @see #getSampleSize(int)
   */
  public abstract int[] getSampleSize();

  /**
   * Returns the size (in bits) of the samples for the specified band.
   *
   * @param band  the band (in the range <code>0</code> to
   *     <code>getNumBands() - 1</code>).
   *
   * @return The sample size (in bits).
   */
  public abstract int getSampleSize(int band);
}
