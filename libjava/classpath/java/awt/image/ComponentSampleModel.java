/* Copyright (C) 2000, 2002, 2006,  Free Software Foundation

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

import java.util.Arrays;

/**
 * ComponentSampleModel supports a flexible organization of pixel samples in
 * memory, permitting pixel samples to be interleaved by band, by scanline,
 * and by pixel.
 *
 * A DataBuffer for this sample model has K banks of data.  Pixels have N
 * samples, so there are N bands in the DataBuffer.  Each band is completely
 * contained in one bank of data, but a bank may contain more than one band.
 * Each pixel sample is stored in a single data element.
 *
 * Within a bank, each band begins at an offset stored in bandOffsets.  The
 * banks containing the band is given by bankIndices.  Within the bank, there
 * are three dimensions - band, pixel, and scanline.  The dimension ordering
 * is controlled by bandOffset, pixelStride, and scanlineStride, which means
 * that any combination of interleavings is supported.
 *
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public class ComponentSampleModel extends SampleModel
{
  /** The offsets to the first sample for each band. */
  protected int[] bandOffsets;
  
  /** The indices of the bank used to store each band in a data buffer. */
  protected int[] bankIndices;
  
  /** 
   * The number of bands in the image.
   * @specnote This field shadows the protected numBands in SampleModel.
   */
  protected int numBands;
  
  /** Used when creating data buffers. */
  protected int numBanks;

  /** 
   * The number of data elements between a sample in one row and the 
   * corresponding sample in the next row.
   */
  protected int scanlineStride;
  
  /**
   * The number of data elements between a sample for one pixel and the 
   * corresponding sample for the next pixel in the same row.
   */
  protected int pixelStride;

  /**
   * Creates a new sample model that assumes that all bands are stored in a 
   * single bank of the {@link DataBuffer}.
   * <p>
   * Note that the <code>bandOffsets</code> array is copied to internal storage
   * to prevent subsequent changes to the array from affecting this object.
   * 
   * @param dataType  the data type (one of {@link DataBuffer#TYPE_BYTE},
   *   {@link DataBuffer#TYPE_USHORT}, {@link DataBuffer#TYPE_SHORT},
   *   {@link DataBuffer#TYPE_INT}, {@link DataBuffer#TYPE_FLOAT} or 
   *   {@link DataBuffer#TYPE_DOUBLE}).
   * @param w  the width in pixels.
   * @param h  the height in pixels.
   * @param pixelStride  the number of data elements in the step from a sample
   *   in one pixel to the corresponding sample in the next pixel.
   * @param scanlineStride  the number of data elements in the step from a 
   *   sample in a pixel to the corresponding sample in the pixel in the next
   *   row.
   * @param bandOffsets  the offset to the first element for each band, with 
   *   the size of the array defining the number of bands (<code>null</code>
   *   not permitted).
   *   
   * @throws IllegalArgumentException if <code>dataType</code> is not one of
   *   the specified values.
   * @throws IllegalArgumentException if <code>w</code> is less than or equal
   *   to zero.
   * @throws IllegalArgumentException if <code>h</code> is less than or equal 
   *   to zero.
   * @throws IllegalArgumentException if <code>w * h</code> exceeds
   *   {@link Integer#MAX_VALUE}.
   * @throws IllegalArgumentException if <code>pixelStride</code> is negative.
   * @throws IllegalArgumentException if <code>scanlineStride</code> is less 
   *   than or equal to zero.
   * @throws IllegalArgumentException if <code>bandOffsets</code> has zero 
   *   length.
   */
  public ComponentSampleModel(int dataType,
                              int w, int h,
                              int pixelStride,
                              int scanlineStride,
                              int[] bandOffsets)
  {
    this(dataType, w, h, pixelStride, scanlineStride,
         new int[bandOffsets.length], bandOffsets);
  }
    
  /**
   * Creates a new sample model that assumes that all bands are stored in a 
   * single bank of the {@link DataBuffer}.
   * 
   * @param dataType  the data type (one of {@link DataBuffer#TYPE_BYTE},
   *   {@link DataBuffer#TYPE_USHORT}, {@link DataBuffer#TYPE_SHORT},
   *   {@link DataBuffer#TYPE_INT}, {@link DataBuffer#TYPE_FLOAT} or 
   *   {@link DataBuffer#TYPE_DOUBLE}).
   * @param w  the width in pixels.
   * @param h  the height in pixels.
   * @param pixelStride  the number of data elements in the step from a sample
   *   in one pixel to the corresponding sample in the next pixel.
   * @param scanlineStride  the number of data elements in the step from a 
   *   sample in a pixel to the corresponding sample in the pixel in the next
   *   row.
   * @param bankIndices  the index of the bank in which each band is stored 
   *   (<code>null</code> not permitted).  This array is copied to internal
   *   storage so that subsequent updates to the array do not affect the sample 
   *   model.
   * @param bandOffsets  the offset to the first element for each band, with 
   *   the size of the array defining the number of bands (<code>null</code>
   *   not permitted).  This array is copied to internal storage so that 
   *   subsequent updates to the array do not affect the sample model.
   *   
   * @throws IllegalArgumentException if <code>dataType</code> is not one of
   *   the specified values.
   * @throws IllegalArgumentException if <code>w</code> is less than or equal
   *   to zero.
   * @throws IllegalArgumentException if <code>h</code> is less than or equal 
   *   to zero.
   * @throws IllegalArgumentException if <code>w * h</code> exceeds
   *   {@link Integer#MAX_VALUE}.
   * @throws IllegalArgumentException if <code>pixelStride</code> is negative.
   * @throws IllegalArgumentException if <code>scanlineStride</code> is less 
   *   than or equal to zero.
   * @throws IllegalArgumentException if <code>bandOffsets</code> has zero 
   *   length.
   */
  public ComponentSampleModel(int dataType,
                              int w, int h,
                              int pixelStride,
                              int scanlineStride,
                              int[] bankIndices,
                              int[] bandOffsets)
  {
    super(dataType, w, h, bandOffsets.length);
    
    // super permits DataBuffer.TYPE_UNDEFINED but this class doesn't...
    if (dataType == DataBuffer.TYPE_UNDEFINED)
      throw new IllegalArgumentException("Unsupported dataType.");
    
    if ((pixelStride < 0) || (scanlineStride < 0) || (bandOffsets.length < 1) 
        || (bandOffsets.length != bankIndices.length))
      throw new IllegalArgumentException();
    
    this.bandOffsets = (int[]) bandOffsets.clone();
    this.bankIndices = (int[]) bankIndices.clone();
    this.numBands = bandOffsets.length;

    this.numBanks = 0;
    for (int b = 0; b < bankIndices.length; b++)
      this.numBanks = Math.max(this.numBanks, bankIndices[b] + 1);

    this.scanlineStride = scanlineStride;
    this.pixelStride = pixelStride;

  }             

  /**
   * Creates a new sample model that is compatible with this one, but with the
   * specified dimensions.
   * 
   * @param w  the width (must be greater than zero).
   * @param h  the height (must be greater than zero).
   * 
   * @return A new sample model.
   */
  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    return new ComponentSampleModel(dataType, w, h, pixelStride,
                                    scanlineStride, bankIndices,
                                    bandOffsets);
  }

  /**
   * Creates a new sample model that provides access to a subset of the bands
   * that this sample model supports.
   * 
   * @param bands  the bands (<code>null</code> not permitted).
   * 
   * @return The new sample model.
   */
  public SampleModel createSubsetSampleModel(int[] bands)
  {
    int numBands = bands.length;
    
    int[] bankIndices = new int[numBands];
    int[] bandOffsets = new int[numBands];
    for (int b = 0; b < numBands; b++)
      {
        bankIndices[b] = this.bankIndices[bands[b]];
        bandOffsets[b] = this.bandOffsets[bands[b]];
      }

    return new ComponentSampleModel(dataType, width, height, pixelStride,
                                    scanlineStride, bankIndices,
                                    bandOffsets);
  }

  /**
   * Creates a new data buffer that is compatible with this sample model.
   * 
   * @return The new data buffer.
   */
  public DataBuffer createDataBuffer()
  {
    // Maybe this value should be precalculated in the constructor?
    int highestOffset = 0;
    for (int b = 0; b < numBands; b++)
      highestOffset = Math.max(highestOffset, bandOffsets[b]);    
    int size = pixelStride * (width - 1) + scanlineStride * (height - 1) 
        + highestOffset + 1;

    DataBuffer buffer = null;
    switch (getTransferType())
      {
      case DataBuffer.TYPE_BYTE:
        buffer = new DataBufferByte(size, numBanks);
        break;
      case DataBuffer.TYPE_SHORT:
        buffer = new DataBufferShort(size, numBanks);
        break;
      case DataBuffer.TYPE_USHORT:
        buffer = new DataBufferUShort(size, numBanks);
        break;
      case DataBuffer.TYPE_INT:
        buffer = new DataBufferInt(size, numBanks);
        break;
      case DataBuffer.TYPE_FLOAT:
        buffer = new DataBufferFloat(size, numBanks);
        break;
      case DataBuffer.TYPE_DOUBLE:
        buffer = new DataBufferDouble(size, numBanks);
        break;
      }
    return buffer;
  }

  /**
   * Returns the offset of the sample in band 0 for the pixel at location
   * <code>(x, y)</code>.  This offset can be used to read a sample value from
   * a {@link DataBuffer}.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * 
   * @return The offset.
   * 
   * @see #getOffset(int, int, int)
   */
  public int getOffset(int x, int y)
  {
    return getOffset(x, y, 0);
  }

  /**
   * Returns the offset of the sample in band <code>b</code> for the pixel at
   * location <code>(x, y)</code>.  This offset can be used to read a sample
   * value from a {@link DataBuffer}.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param b  the band index.
   * 
   * @return The offset.
   */
  public int getOffset(int x, int y, int b)
  {
    return bandOffsets[b] + pixelStride * x + scanlineStride * y;
  }

  /**
   * Returns the size in bits for each sample (one per band).  For this sample
   * model, each band has the same sample size and this is determined by the
   * data type for the sample model.
   * 
   * @return The sample sizes.
   * 
   * @see SampleModel#getDataType()
   */
  public final int[] getSampleSize()
  {
    int size = DataBuffer.getDataTypeSize(getDataType());
    int[] sizes = new int[numBands];

    java.util.Arrays.fill(sizes, size);
    return sizes;
  }

  /**
   * Returns the size in bits for the samples in the specified band.  In this
   * class, the sample size is the same for every band and is determined from 
   * the data type for the model.
   * 
   * @param band  the band index (ignored here).
   * 
   * @return The sample size in bits.
   * 
   * @see SampleModel#getDataType()
   */
  public final int getSampleSize(int band)
  {
    return DataBuffer.getDataTypeSize(getDataType());
  }

  /**
   * Returns the indices of the bank(s) in the {@link DataBuffer} used to 
   * store the samples for each band.  The returned array is a copy, so that
   * altering it will not impact the sample model.
   * 
   * @return The bank indices.
   */
  public final int[] getBankIndices()
  {
    return (int[]) bankIndices.clone();
  }

  /**
   * Returns the offsets to the first sample in each band.  The returned array
   * is a copy, so that altering it will not impact the sample model.
   * 
   * @return The offsets.
   */
  public final int[] getBandOffsets()
  {
    return (int[]) bandOffsets.clone();
  }

  /**
   * Returns the distance (in terms of element indices) between the sample for
   * one pixel and the corresponding sample for the equivalent pixel in the 
   * next row.  This is used in the calculation of the element offset for
   * retrieving samples from a {@link DataBuffer}.
   * 
   * @return The distance between pixel samples in consecutive rows.
   */
  public final int getScanlineStride()
  {
    return scanlineStride;
  }

  /**
   * Returns the distance (in terms of element indices) between the sample for 
   * one pixel and the corresponding sample for the next pixel in a row.  This 
   * is used in the calculation of the element offset for retrieving samples 
   * from a {@link DataBuffer}.
   * 
   * @return The distance between pixel samples in the same row.
   */
  public final int getPixelStride()
  {
    return pixelStride;
  }

  /**
   * Returns the number of data elements used to store the samples for one 
   * pixel.  In this model, this is the same as the number of bands.
   * 
   * @return The number of data elements used to store the samples for one 
   *   pixel.
   */
  public final int getNumDataElements()
  {
    return numBands;
  }

  /**
   * Returns the samples for the pixel at location <code>(x, y)</code> in
   * a primitive array (the array type is determined by the data type for 
   * this model).  The <code>obj</code> argument provides an option to supply
   * an existing array to hold the result, if this is <code>null</code> a new
   * array will be allocated.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param obj  a primitive array that, if not <code>null</code>, will be 
   *   used to store and return the sample values.
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @return An array of sample values for the specified pixel.
   */
  public Object getDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int type = getTransferType();
    int numDataEls = getNumDataElements();
    int offset = y * scanlineStride + x * pixelStride;
    switch (type)
      {
      case DataBuffer.TYPE_BYTE:
        byte[] bData;
        if (obj == null)
          bData = new byte[numDataEls];
        else
          bData = (byte[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            bData[i] = (byte) data.getElem(bankIndices[i],
                                           offset + bandOffsets[i]);
          }
        obj = bData;
        break;
      case DataBuffer.TYPE_SHORT:
      case DataBuffer.TYPE_USHORT:
        short[] sData;
        if (obj == null)
          sData = new short[numDataEls];
        else
          sData = (short[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            sData[i] = (short) data.getElem(bankIndices[i],
                                            offset + bandOffsets[i]);
          }
        obj = sData;
        break;
      case DataBuffer.TYPE_INT:
        int[] iData;
        if (obj == null)
          iData = new int[numDataEls];
        else
          iData = (int[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            iData[i] = data.getElem(bankIndices[i], offset + bandOffsets[i]);
          }
        obj = iData;
        break;
      case DataBuffer.TYPE_FLOAT:
        float[] fData;
        if (obj == null)
          fData = new float[numDataEls];
        else
          fData = (float[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            fData[i] = data.getElemFloat(bankIndices[i],
                                         offset + bandOffsets[i]);
          }
        obj = fData;
        break;
      case DataBuffer.TYPE_DOUBLE:
        double[] dData;
        if (obj == null)
          dData = new double[numDataEls];
        else
          dData = (double[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            dData[i] = data.getElemDouble(bankIndices[i],
                                          offset + bandOffsets[i]);
          }
        obj = dData;
        break;
      }
    return obj;
  }


  /**
   * Returns all the samples for the pixel at location <code>(x, y)</code>
   * stored in the specified data buffer.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param iArray  an array that will be populated with the sample values and
   *   returned as the result.  The size of this array should be equal to the 
   *   number of bands in the model.  If the array is <code>null</code>, a new
   *   array is created.
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @return The samples for the specified pixel.
   * 
   * @see #setPixel(int, int, int[], DataBuffer)
   */
  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    if (x < 0 || x >= width || y < 0 || y >= height)
      throw new ArrayIndexOutOfBoundsException("Pixel (" + x + ", " + y 
                                               + ") is out of bounds.");
    int offset = pixelStride * x + scanlineStride * y;
    if (iArray == null)
      iArray = new int[numBands];
    for (int b = 0; b < numBands; b++)
      {
        iArray[b] = data.getElem(bankIndices[b], offset + bandOffsets[b]);
      }
    return iArray;
  }

  /**
   * Returns the samples for all the pixels in a rectangular region.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param w  the width.
   * @param h  the height.
   * @param iArray  an array that if non-<code>null</code> will be populated 
   *   with the sample values and returned as the result.
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @return The samples for all the pixels in the rectangle.
   */
  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
                         DataBuffer data)
  {
    int offset = pixelStride * x + scanlineStride * y;
    if (iArray == null) 
      iArray = new int[numBands * w * h];
    int outOffset = 0;
    for (y = 0; y < h; y++)
      {
        int lineOffset = offset;
        for (x = 0; x < w; x++)
          {
            for (int b = 0; b < numBands; b++)
              {
                iArray[outOffset++] 
                    = data.getElem(bankIndices[b], lineOffset+bandOffsets[b]);
              }
            lineOffset += pixelStride;
          }
        offset += scanlineStride;
      }
    return iArray;
  }
 
  /**
   * Returns the sample for band <code>b</code> of the pixel at 
   * <code>(x, y)</code> that is stored in the specified data buffer.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param b  the band index.
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @return The sample value.
   * 
   * @throws ArrayIndexOutOfBoundsException if <code>(x, y)</code> is outside 
   *     the bounds <code>[0, 0, width, height]</code>.
   *     
   * @see #setSample(int, int, int, int, DataBuffer)
   */
  public int getSample(int x, int y, int b, DataBuffer data)
  {
    if (x < 0 || x >= width || y < 0 || y >= height)
      throw new ArrayIndexOutOfBoundsException("Sample (" + x + ", " + y 
                                               + ") is out of bounds.");
    return data.getElem(bankIndices[b], getOffset(x, y, b));
  }

  /**
   * Sets the samples for the pixel at location <code>(x, y)</code> from the 
   * supplied primitive array (the array type must be consistent with the data 
   * type for this model).
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param obj  a primitive array containing the pixel's sample values.
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @see #setDataElements(int, int, Object, DataBuffer)
   */
  public void setDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int type = getTransferType();
    int numDataEls = getNumDataElements();
    int offset = y * scanlineStride + x * pixelStride;
    switch (type)
      {
      case DataBuffer.TYPE_BYTE:
        byte[] bData = (byte[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            data.setElem(bankIndices[i], offset + bandOffsets[i],
                         ((int) bData[i]) & 0xFF);
          }
        break;
      case DataBuffer.TYPE_SHORT:
      case DataBuffer.TYPE_USHORT:
        short[] sData = (short[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            data.setElem(bankIndices[i], offset + bandOffsets[i],
                         ((int) sData[i]) & 0xFFFF);
          }
        break;
      case DataBuffer.TYPE_INT:
        int[] iData = (int[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            data.setElem(bankIndices[i], offset + bandOffsets[i], iData[i]);
          }
        break;
      case DataBuffer.TYPE_FLOAT:
        float[] fData = (float[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            data.setElemFloat(bankIndices[i], offset + bandOffsets[i],
                              fData[i]);
          }
        break;
      case DataBuffer.TYPE_DOUBLE:
        double[] dData = (double[]) obj;
        for (int i = 0; i < numDataEls; i++)
          {
            data.setElemDouble(bankIndices[i], offset + bandOffsets[i],
                               dData[i]);
          }
        break;
      }
  }
  
  /**
   * Sets the sample values for the pixel at location <code>(x, y)</code>
   * stored in the specified data buffer.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param iArray  the pixel sample values (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @see #getPixel(int, int, int[], DataBuffer)
   */
  public void setPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    int offset = pixelStride * x + scanlineStride * y;
    for (int b = 0; b < numBands; b++)
      data.setElem(bankIndices[b], offset + bandOffsets[b], iArray[b]);
  }
    
  /**
   * Sets the sample value for band <code>b</code> of the pixel at location
   * <code>(x, y)</code> in the specified data buffer.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param b  the band index.
   * @param s  the sample value.
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @see #getSample(int, int, int, DataBuffer)
   */
  public void setSample(int x, int y, int b, int s, DataBuffer data)
  {
    data.setElem(bankIndices[b], getOffset(x, y, b), s);
  }
  
  /**
   * Tests this sample model for equality with an arbitrary object.  Returns
   * <code>true</code> if and only if:
   * <ul>
   * <li><code>obj</code> is not <code>null</code>;</li>
   * <li><code>obj</code> is an instance of <code>ComponentSampleModel</code>;
   *   </li>
   * <li>both models have the same values for the <code>dataType</code>,
   *   <code>width</code>, <code>height</code>, <code>pixelStride</code>,
   *   <code>scanlineStride</code>, <code>bandOffsets</code> and
   *   <code>bankIndices</code> fields.</li>
   * </ul>
   * 
   * @param obj  the object to test (<code>null</code> permitted).
   * 
   * @return <code>true</code> if this sample model is equal to 
   *   <code>obj</code>, and <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (obj == null)
      return false;
    if (! (obj instanceof ComponentSampleModel))
      return false;
    ComponentSampleModel that = (ComponentSampleModel) obj;
    if (this.dataType != that.dataType)
      return false;
    if (this.width != that.width)
      return false;
    if (this.height != that.height)
      return false;
    if (this.pixelStride != that.pixelStride)
      return false;
    if (this.scanlineStride != that.scanlineStride)
      return false;
    if (! Arrays.equals(this.bandOffsets, that.bandOffsets))
      return false;
    if (! Arrays.equals(this.bankIndices, that.bankIndices))
      return false;
    // couldn't find any difference, so...
    return true;
  }
  
  /**
   * Returns a hash code for this sample model.
   * 
   * @return The hash code.
   */
  public int hashCode()
  {
    // this computation is based on the method described in Chapter 3
    // of Joshua Bloch's Effective Java...
    int result = 17;
    result = 37 * result + dataType;
    result = 37 * result + width;
    result = 37 * result + height;
    result = 37 * result + pixelStride;
    result = 37 * result + scanlineStride;
    for (int i = 0; i < bandOffsets.length; i++)
      result = 37 * result + bandOffsets[i];
    for (int i = 0; i < bankIndices.length; i++)
      result = 37 * result + bankIndices[i];
    return result;
  }
}
