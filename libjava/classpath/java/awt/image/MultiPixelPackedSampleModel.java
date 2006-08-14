/* Copyright (C) 2004, 2006,  Free Software Foundation

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

/**
 * MultiPixelPackedSampleModel provides a single band model that supports
 * multiple pixels in a single unit.  Pixels have 2^n bits and 2^k pixels fit
 * per data element.
 *
 * @author Jerry Quinn (jlquinn@optonline.net)
 */
public class MultiPixelPackedSampleModel extends SampleModel
{
  private int scanlineStride;
  private int[] bitMasks;
  private int[] bitOffsets;
  private int[] sampleSize;
  private int dataBitOffset;
  private int elemBits;
  private int numberOfBits;
  private int numElems;

  /**
   * Creates a new <code>MultiPixelPackedSampleModel</code> with the specified
   * data type, which should be one of:
   * <ul>
   *   <li>{@link DataBuffer#TYPE_BYTE};</li>
   *   <li>{@link DataBuffer#TYPE_USHORT};</li>
   *   <li>{@link DataBuffer#TYPE_INT};</li>
   * </ul>
   * 
   * @param dataType  the data type.
   * @param w  the width (in pixels).
   * @param h  the height (in pixels).
   * @param numberOfBits  the number of bits per pixel (must be a power of 2).
   */
  public MultiPixelPackedSampleModel(int dataType, int w, int h,
				     int numberOfBits)
  {
    this(dataType, w, h, numberOfBits, 0, 0);
  }

  /**
   * Creates a new <code>MultiPixelPackedSampleModel</code> with the specified
   * data type, which should be one of:
   * <ul>
   *   <li>{@link DataBuffer#TYPE_BYTE};</li>
   *   <li>{@link DataBuffer#TYPE_USHORT};</li>
   *   <li>{@link DataBuffer#TYPE_INT};</li>
   * </ul>
   * 
   * @param dataType  the data type.
   * @param w  the width (in pixels).
   * @param h  the height (in pixels).
   * @param numberOfBits  the number of bits per pixel (must be a power of 2).
   * @param scanlineStride  the number of data elements from a pixel on one 
   *     row to the corresponding pixel in the next row.
   * @param dataBitOffset  the offset to the first data bit.
   */
  public MultiPixelPackedSampleModel(int dataType, int w, int h,
				     int numberOfBits, int scanlineStride,
				     int dataBitOffset)
  {
    super(dataType, w, h, 1);

    switch (dataType)
      {
      case DataBuffer.TYPE_BYTE:
	elemBits = 8;
	break;
      case DataBuffer.TYPE_USHORT:
	elemBits = 16;
	break;
      case DataBuffer.TYPE_INT:
	elemBits = 32;
	break;
      default:
	throw new IllegalArgumentException("MultiPixelPackedSampleModel"
					   + " unsupported dataType");
      }

    this.dataBitOffset = dataBitOffset;

    this.numberOfBits = numberOfBits;
    if (numberOfBits > elemBits)
      throw new RasterFormatException("MultiPixelPackedSampleModel pixel size"
				      + " larger than dataType");
    switch (numberOfBits)
      {
      case 1: case 2: case 4: case 8: case 16: case 32: break;
      default:
	throw new RasterFormatException("MultiPixelPackedSampleModel pixel"
					+ " size not 2^n bits");
      }
    numElems = elemBits / numberOfBits;

    // Compute scan line large enough for w pixels.
    if (scanlineStride == 0)
      scanlineStride = ((dataBitOffset + w * numberOfBits) - 1) / elemBits + 1;
    this.scanlineStride = scanlineStride;

    
    sampleSize = new int[1];
    sampleSize[0] = numberOfBits;

    bitMasks = new int[numElems];
    bitOffsets = new int[numElems];
    for (int i=0; i < numElems; i++)
      {
	bitOffsets[numElems - i- 1] = numberOfBits * i;
	bitMasks[numElems - i - 1] = ((1 << numberOfBits) - 1) << 
	    bitOffsets[numElems - i - 1];
      }
  }

  /**
   * Creates a new <code>MultiPixelPackedSample</code> model with the same
   * data type and bits per pixel as this model, but with the specified
   * dimensions.
   * 
   * @param w  the width (in pixels).
   * @param h  the height (in pixels).
   * 
   * @return The new sample model.
   */
  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    /* FIXME: We can avoid recalculation of bit offsets and sample
       sizes here by passing these from the current instance to a
       special private constructor. */
    return new MultiPixelPackedSampleModel(dataType, w, h, numberOfBits);
  }

  /**
   * Creates a DataBuffer for holding pixel data in the format and
   * layout described by this SampleModel. The returned buffer will
   * consist of one single bank.
   * 
   * @return A new data buffer.
   */
  public DataBuffer createDataBuffer()
  {
    int size = scanlineStride * height;
    if (dataBitOffset > 0)
      size += (dataBitOffset - 1) / elemBits + 1;
    return Buffers.createBuffer(getDataType(), size);
  }

  /**
   * Returns the number of data elements required to transfer a pixel in the
   * get/setDataElements() methods.
   * 
   * @return <code>1</code>.
   */
  public int getNumDataElements()
  {
    return 1;
  }

  /**
   * Returns an array containing the size (in bits) of the samples in each 
   * band.  The <code>MultiPixelPackedSampleModel</code> class supports only
   * one band, so this method returns an array with length <code>1</code>. 
   * 
   * @return An array containing the size (in bits) of the samples in band zero. 
   *     
   * @see #getSampleSize(int)
   */
  public int[] getSampleSize()
  {
    return (int[]) sampleSize.clone();
  }
  
  /**
   * Returns the size of the samples in the specified band.  Note that the
   * <code>MultiPixelPackedSampleModel</code> supports only one band -- this
   * method ignored the <code>band</code> argument, and always returns the size
   * of band zero.
   * 
   * @param band  the band (this parameter is ignored).
   * 
   * @return The size of the samples in band zero.
   * 
   * @see #getSampleSize()
   */
  public int getSampleSize(int band)
  {
    return sampleSize[0];
  }

  /**
   * Returns the index in the data buffer that stores the pixel at (x, y).
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * 
   * @return The index in the data buffer that stores the pixel at (x, y).
   * 
   * @see #getBitOffset(int)
   */
  public int getOffset(int x, int y)
  {
    return scanlineStride * y + ((dataBitOffset + x * numberOfBits) / elemBits);
  }

  /**
   * The bit offset (within an element in the data buffer) of the pixels with 
   * the specified x-coordinate.
   * 
   * @param x  the x-coordinate.
   * 
   * @return The bit offset.
   */
  public int getBitOffset(int x)
  {
    return (dataBitOffset + x * numberOfBits) % elemBits;
  }

  /**
   * Returns the offset to the first data bit.
   * 
   * @return The offset to the first data bit.
   */
  public int getDataBitOffset()
  {
    return dataBitOffset;
  }

  /**
   * Returns the number of data elements from a pixel in one row to the
   * corresponding pixel in the next row.
   * 
   * @return The scanline stride.
   */
  public int getScanlineStride()
  {
    return scanlineStride;
  }

  /**
   * Returns the number of bits per pixel.
   * 
   * @return The number of bits per pixel.
   */
  public int getPixelBitStride()
  {
    return numberOfBits;
  }
  
  /**
   * Returns the transfer type, which is one of the following (depending on
   * the number of bits per sample for this model):
   * <ul>
   *   <li>{@link DataBuffer#TYPE_BYTE};</li>
   *   <li>{@link DataBuffer#TYPE_USHORT};</li>
   *   <li>{@link DataBuffer#TYPE_INT};</li>
   * </ul>
   * 
   * @return The transfer type.
   */
  public int getTransferType()
  {
    if (numberOfBits <= DataBuffer.getDataTypeSize(DataBuffer.TYPE_BYTE))
      return DataBuffer.TYPE_BYTE;
    else if (numberOfBits <= DataBuffer.getDataTypeSize(DataBuffer.TYPE_USHORT))
      return DataBuffer.TYPE_USHORT;
    return DataBuffer.TYPE_INT;
  }

  /**
   * Normally this method returns a sample model for accessing a subset of
   * bands of image data, but since <code>MultiPixelPackedSampleModel</code>
   * only supports a single band, this overridden implementation just returns
   * a new instance of <code>MultiPixelPackedSampleModel</code>, with the same
   * attributes as this instance.
   * 
   * @param bands  the bands to include in the subset (this is ignored, except
   *     that if it is non-<code>null</code> a check is made to ensure that the
   *     array length is equal to <code>1</code>).
   *     
   * @throws RasterFormatException if <code>bands</code> is not 
   *     <code>null</code> and <code>bands.length != 1</code>.
   */
  public SampleModel createSubsetSampleModel(int[] bands)
  {
    if (bands != null && bands.length != 1)
      throw new RasterFormatException("MultiPixelPackedSampleModel only"
          + " supports one band");
    return new MultiPixelPackedSampleModel(dataType, width, height, 
        numberOfBits, scanlineStride, dataBitOffset);
  }

  /**
   * Extract one pixel and return in an array of transfer type.
   *
   * Extracts the pixel at x, y from data and stores into the 0th index of the
   * array obj, since there is only one band.  If obj is null, a new array of
   * getTransferType() is created.
   *
   * @param x The x-coordinate of the pixel rectangle to store in 
   *     <code>obj</code>.
   * @param y The y-coordinate of the pixel rectangle to store in 
   *     <code>obj</code>.
   * @param obj The primitive array to store the pixels into or null to force 
   *     creation.
   * @param data The DataBuffer that is the source of the pixel data.
   * @return The primitive array containing the pixel data.
   * @see java.awt.image.SampleModel#getDataElements(int, int, Object, 
   *     DataBuffer)
   */
  public Object getDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int pixel = getSample(x, y, 0, data);
    switch (getTransferType())
      {
        case DataBuffer.TYPE_BYTE:
          if (obj == null) 
            obj = new byte[1];
          ((byte[]) obj)[0] = (byte) pixel;
          return obj;
        case DataBuffer.TYPE_USHORT:
          if (obj == null) 
            obj = new short[1];
          ((short[]) obj)[0] = (short) pixel;
          return obj;
        case DataBuffer.TYPE_INT:
          if (obj == null) 
            obj = new int[1];
          ((int[]) obj)[0] = pixel;
          return obj;
        default:
          // Seems like the only sensible thing to do.
          throw new ClassCastException();
      }
  }

  /**
   * Returns an array (of length 1) containing the sample for the pixel at 
   * (x, y) in the specified data buffer.  If <code>iArray</code> is not 
   * <code>null</code>, it will be populated with the sample value and 
   * returned as the result of this function (this avoids allocating a new 
   * array instance).
   * 
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param iArray  an array to populate with the sample values and return as 
   *     the result (if <code>null</code>, a new array will be allocated).
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @return An array containing the pixel sample value.
   * 
   * @throws NullPointerException if <code>data</code> is <code>null</code>.
   */
  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    if (iArray == null) 
      iArray = new int[1];
    iArray[0] = getSample(x, y, 0, data);
    return iArray;
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
  public int getSample(int x, int y, int b, DataBuffer data)
  {
    int pos =
      ((dataBitOffset + x * numberOfBits) % elemBits) / numberOfBits;
    int offset = getOffset(x, y);
    int samples = data.getElem(offset);
    return (samples & bitMasks[pos]) >>> bitOffsets[pos];
  }
  
  /**
   * Set the pixel at x, y to the value in the first element of the primitive
   * array obj.
   *
   * @param x The x-coordinate of the data elements in <code>obj</code>.
   * @param y The y-coordinate of the data elements in <code>obj</code>.
   * @param obj The primitive array containing the data elements to set.
   * @param data The DataBuffer to store the data elements into.
   */
  public void setDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int transferType = getTransferType();
    try
      {
        switch (transferType)
          {
            case DataBuffer.TYPE_BYTE:
              {
                byte[] in = (byte[]) obj;
                setSample(x, y, 0, in[0] & 0xFF, data);
                return;
              }
            case DataBuffer.TYPE_USHORT:
              {
                short[] in = (short[]) obj;
                setSample(x, y, 0, in[0] & 0xFFFF, data);
                return;
              }
            case DataBuffer.TYPE_INT:
              {
                int[] in = (int[]) obj;
                setSample(x, y, 0, in[0], data);
                return;
              }
            default:
              throw new ClassCastException("Unsupported data type");
          }
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
        String msg = "While writing data elements" +
          ", x=" + x + ", y=" + y +
          ", width=" + width + ", height=" + height +
          ", scanlineStride=" + scanlineStride +
          ", offset=" + getOffset(x, y) +
          ", data.getSize()=" + data.getSize() +
          ", data.getOffset()=" + data.getOffset() +
          ": " + aioobe;
        throw new ArrayIndexOutOfBoundsException(msg);
      }
  }

  /**
   * Sets the sample value for the pixel at (x, y) in the specified data 
   * buffer to the specified value. 
   * 
   * @param x  the x-coordinate of the pixel.
   * @param y  the y-coordinate of the pixel.
   * @param iArray  the sample value (<code>null</code> not permitted).
   * @param data  the data buffer (<code>null</code> not permitted).
   * 
   * @throws NullPointerException if either <code>iArray</code> or 
   *     <code>data</code> is <code>null</code>.
   *     
   * @see #setSample(int, int, int, int, DataBuffer)
   */
  public void setPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    setSample(x, y, 0, iArray[0], data);
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
  public void setSample(int x, int y, int b, int s, DataBuffer data)
  {
    int bitpos =
      ((dataBitOffset + x * numberOfBits) % elemBits) / numberOfBits;
    int offset = getOffset(x, y);

    s = s << bitOffsets[bitpos];
    s = s & bitMasks[bitpos];

    int sample = data.getElem(offset);
    sample |= s;
    data.setElem(offset, sample);
  }
  
  /**
   * Tests this sample model for equality with an arbitrary object.  This 
   * method returns <code>true</code> if and only if:
   * <ul>
   *   <li><code>obj</code> is not <code>null</code>;
   *   <li><code>obj</code> is an instance of 
   *       <code>MultiPixelPackedSampleModel</code>;
   *   <li>both models have the same:
   *     <ul>
   *       <li><code>dataType</code>;
   *       <li><code>width</code>;
   *       <li><code>height</code>;
   *       <li><code>numberOfBits</code>;
   *       <li><code>scanlineStride</code>;
   *       <li><code>dataBitOffsets</code>.
   *     </ul>
   *   </li>
   * </ul>
   * 
   * @param obj  the object (<code>null</code> permitted)
   * 
   * @return <code>true</code> if this model is equal to <code>obj</code>, and
   *     <code>false</code> otherwise.
   */
  public boolean equals(Object obj) 
  {
    if (this == obj) 
      return true;
    if (! (obj instanceof MultiPixelPackedSampleModel)) 
      return false;
    MultiPixelPackedSampleModel that = (MultiPixelPackedSampleModel) obj;
    if (this.dataType != that.dataType)
      return false;
    if (this.width != that.width)
      return false;
    if (this.height != that.height)
      return false;
    if (this.numberOfBits != that.numberOfBits)
      return false;
    if (this.scanlineStride != that.scanlineStride)
      return false;
    if (this.dataBitOffset != that.dataBitOffset)
      return false;
    return true;
  }
  
  /**
   * Returns a hash code for this <code>MultiPixelPackedSampleModel</code>.
   * 
   * @return A hash code.
   */
  public int hashCode()
  {
    // this hash code won't match Sun's, but that shouldn't matter...
    int result = 193;
    result = 37 * result + dataType;
    result = 37 * result + width;
    result = 37 * result + height;
    result = 37 * result + numberOfBits;
    result = 37 * result + scanlineStride;
    result = 37 * result + dataBitOffset;
    return result;
  }
  
  /**
   * Creates a String with some information about this SampleModel.
   * @return A String describing this SampleModel.
   * @see java.lang.Object#toString()
   */
  public String toString()
  {
    StringBuffer result = new StringBuffer();
    result.append(getClass().getName());
    result.append("[");
    result.append("scanlineStride=").append(scanlineStride);
    for(int i=0; i < bitMasks.length; i+=1)
    {
      result.append(", mask[").append(i).append("]=0x").append(Integer.toHexString(bitMasks[i]));
    }
    
    result.append("]");
    return result.toString();
  }
}
