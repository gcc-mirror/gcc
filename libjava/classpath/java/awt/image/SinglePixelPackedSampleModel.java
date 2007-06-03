/* Copyright (C) 2000, 2002, 2003, 2004, 2006,  Free Software Foundation

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

import gnu.java.awt.BitMaskExtent;

/**
 * A <code>SampleModel</code> used when all samples are stored in a single
 * data element in the {@link DataBuffer}, and each data element contains 
 * samples for one pixel only.
 * 
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public class SinglePixelPackedSampleModel extends SampleModel
{
  private int scanlineStride;
  private int[] bitMasks;
  private int[] bitOffsets;
  private int[] sampleSize;
  
  /**
   * Creates a new <code>SinglePixelPackedSampleModel</code>.
   * 
   * @param dataType  the data buffer type.
   * @param w  the width (in pixels).
   * @param h  the height (in pixels).
   * @param bitMasks  an array containing the bit mask used to extract the
   *     sample value for each band.
   */
  public SinglePixelPackedSampleModel(int dataType, int w, int h,
				      int[] bitMasks)
  {
    this(dataType, w, h, w, bitMasks);
  }

  /**
   * Creates a new <code>SinglePixelPackedSampleModel</code>.
   * 
   * @param dataType  the data buffer type.
   * @param w  the width (in pixels).
   * @param h  the height (in pixels).
   * @param scanlineStride  the number of data elements between a pixel on one
   *     row and the corresponding pixel on the next row.
   * @param bitMasks  an array containing the bit mask used to extract the
   *     sample value for each band.
   */
  public SinglePixelPackedSampleModel(int dataType, int w, int h,
				      int scanlineStride, int[] bitMasks)
  {
    super(dataType, w, h, bitMasks.length);

    switch (dataType)
      {
      case DataBuffer.TYPE_BYTE:
      case DataBuffer.TYPE_USHORT:
      case DataBuffer.TYPE_INT:
	break;
      default:
        throw new IllegalArgumentException(
            "SinglePixelPackedSampleModel unsupported dataType");
      }
    
    this.scanlineStride = scanlineStride;
    this.bitMasks = bitMasks;
    
    bitOffsets = new int[numBands];
    sampleSize = new int[numBands];
    
    BitMaskExtent extent = new BitMaskExtent();
    for (int b = 0; b < numBands; b++)
      {
        // the mask is an unsigned integer
        long mask = bitMasks[b] & 0xFFFFFFFFL;
        extent.setMask(mask);
        sampleSize[b] = extent.bitWidth;
        bitOffsets[b] = extent.leastSignificantBit;
      }
  }

  /**
   * Returns the number of data elements.
   * 
   * @return <code>1</code>.
   */
  public int getNumDataElements()
  {
    return 1;
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
  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    /* FIXME: We can avoid recalculation of bit offsets and sample
       sizes here by passing these from the current instance to a
       special private constructor. */
    return new SinglePixelPackedSampleModel(dataType, w, h, bitMasks);
  }


  /**
   * Creates a DataBuffer for holding pixel data in the format and
   * layout described by this SampleModel. The returned buffer will
   * consist of one single bank.
   * 
   * @return The data buffer.
   */
  public DataBuffer createDataBuffer()
  {
    // We can save (scanlineStride - width) pixels at the very end of
    // the buffer. The Sun reference implementation (J2SE 1.3.1 and
    // 1.4.1_01) seems to do this; tested with Mauve test code.
    int size = scanlineStride * (height - 1) + width;

    DataBuffer buffer = null;
    switch (getTransferType())
      {
      case DataBuffer.TYPE_BYTE:
        buffer = new DataBufferByte(size);
        break;
      case DataBuffer.TYPE_USHORT:
        buffer = new DataBufferUShort(size);
        break;
      case DataBuffer.TYPE_INT:
        buffer = new DataBufferInt(size);
        break;
      }
    return buffer;
  }

  /**
   * Returns an array containing the size (in bits) for each band accessed by
   * the <code>SampleModel</code>.
   * 
   * @return An array.
   * 
   * @see #getSampleSize(int)
   */
  public int[] getSampleSize()
  {
    return (int[]) sampleSize.clone();
  }
  
  /**
   * Returns the size (in bits) of the samples for the specified band.
   * 
   * @param band  the band (in the range <code>0</code> to 
   *     <code>getNumBands() - 1</code>).
   *     
   * @return The sample size (in bits).
   */
  public int getSampleSize(int band)
  {
    return sampleSize[band];
  }

  /**
   * Returns the index in the data buffer that stores the pixel at (x, y).
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * 
   * @return The index in the data buffer that stores the pixel at (x, y).
   */
  public int getOffset(int x, int y)
  {
    return scanlineStride*y + x;
  }

  public int[] getBitOffsets()
  {
    return bitOffsets;
  }

  public int[] getBitMasks()
  {
    return bitMasks;
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
   * Creates a new <code>SinglePixelPackedSampleModel</code> that accesses
   * the specified subset of bands.
   * 
   * @param bands  an array containing band indices (<code>null</code> not
   *     permitted).
   * 
   * @return A new sample model.
   * 
   * @throws NullPointerException if <code>bands</code> is <code>null</code>.
   * @throws RasterFormatException if <code>bands.length</code> is greater
   *     than the number of bands in this model.
   */
  public SampleModel createSubsetSampleModel(int[] bands)
  {
    if (bands.length > numBands)
      throw new RasterFormatException("Too many bands.");
    
    int numBands = bands.length;
    
    int[] bitMasks = new int[numBands];

    for (int b = 0; b < numBands; b++)
      bitMasks[b] = this.bitMasks[bands[b]];

    return new SinglePixelPackedSampleModel(dataType, width, height,
					    scanlineStride, bitMasks);
  }

  public Object getDataElements(int x, int y, Object obj,
				DataBuffer data)
  {
    int type = getTransferType();
    Object ret = null;
    switch (type)
      {
      case DataBuffer.TYPE_BYTE:
        {
          byte[] in = (byte[]) obj;
          if (in == null)
            in = new byte[1];
          in[0] = (byte) data.getElem(x + y * scanlineStride);
          ret = in;
        }
        break;
      case DataBuffer.TYPE_USHORT:
        {
          short[] in = (short[]) obj;
          if (in == null)
            in = new short[1];
          in[0] = (short) data.getElem(x + y * scanlineStride);
          ret = in;
        }
        break;
      case DataBuffer.TYPE_INT:
        {
          int[] in = (int[]) obj;
          if (in == null)
            in = new int[1];
          in[0] = data.getElem(x + y * scanlineStride);
          ret = in;
        }
        break;
      }
    return ret;
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
    int offset = scanlineStride*y + x;
    if (iArray == null) iArray = new int[numBands];
    int samples = data.getElem(offset);

    for (int b = 0; b < numBands; b++)
      iArray[b] = (samples & bitMasks[b]) >>> bitOffsets[b];
	
    return iArray;
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
    int offset = scanlineStride*y + x;
    if (iArray == null) iArray = new int[numBands*w*h];
    int outOffset = 0;
    for (y = 0; y < h; y++)
      {
	int lineOffset = offset;
	for (x = 0; x < w; x++)
	  {
	    int samples = data.getElem(lineOffset++);
	    for (int b = 0; b < numBands; b++)
	      iArray[outOffset++] = (samples & bitMasks[b]) >>> bitOffsets[b];
	  }
	offset += scanlineStride;
      }
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
    int offset = scanlineStride*y + x;
    int samples = data.getElem(offset);
    return (samples & bitMasks[b]) >>> bitOffsets[b];
  }
  
  public void setDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int transferType = getTransferType();
    switch (transferType)
      {
      case DataBuffer.TYPE_BYTE:
        {
          byte[] in = (byte[]) obj;
          data.setElem(y * scanlineStride + x, ((int) in[0]) & 0xff);
        }
        break;
      case DataBuffer.TYPE_USHORT:
        {
          short[] in = (short[]) obj;
          data.setElem(y * scanlineStride + x, ((int) in[0]) & 0xffff);
        }
        break;
      case DataBuffer.TYPE_INT:
        {
          int[] in = (int[]) obj;
          data.setElem(y * scanlineStride + x, in[0]);
          break;
        }
      }
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
    int offset = scanlineStride*y + x;
    
    int samples = 0;
    for (int b = 0; b < numBands; b++)
      samples |= (iArray[b] << bitOffsets[b]) & bitMasks[b];

    data.setElem(offset, samples);
  }

  /**
   * This method implements a more efficient way to set pixels than the default
   * implementation of the super class. It copies the pixel components directly
   * from the input array instead of creating a intermediate buffer.
   * @param x The x-coordinate of the pixel rectangle in <code>obj</code>.
   * @param y The y-coordinate of the pixel rectangle in <code>obj</code>.
   * @param w The width of the pixel rectangle in <code>obj</code>.
   * @param h The height of the pixel rectangle in <code>obj</code>.
   * @param iArray The primitive array containing the pixels to set.
   * @param data The DataBuffer to store the pixels into.
   * @see java.awt.image.SampleModel#setPixels(int, int, int, int, int[], 
   *     java.awt.image.DataBuffer)
   */
  public void setPixels(int x, int y, int w, int h, int[] iArray,
						DataBuffer data)
  {
    int inOffset = 0;
    for (int yy=y; yy<(y+h); yy++)
     {
      int offset = scanlineStride*yy + x;
      for (int xx=x; xx<(x+w); xx++)
       { 
        int samples = 0;
        for (int b = 0; b < numBands; b++)
          samples |= (iArray[inOffset+b] << bitOffsets[b]) & bitMasks[b];
        data.setElem(0, offset, samples);
        inOffset += numBands;
        offset += 1;
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
  public void setSample(int x, int y, int b, int s, DataBuffer data)
  {
    int offset = scanlineStride*y + x;
    int samples = data.getElem(offset);
    int bitMask = bitMasks[b];
    samples &= ~bitMask;
    samples |= (s << bitOffsets[b]) & bitMask;
    data.setElem(offset, samples);
  }
  
  /**
   * Tests this sample model for equality with an arbitrary object.  This 
   * method returns <code>true</code> if and only if:
   * <ul>
   *   <li><code>obj</code> is not <code>null</code>;
   *   <li><code>obj</code> is an instance of 
   *       <code>SinglePixelPackedSampleModel</code>;
   *   <li>both models have the same:
   *     <ul>
   *       <li><code>dataType</code>;
   *       <li><code>width</code>;
   *       <li><code>height</code>;
   *       <li><code>numBands</code>;
   *       <li><code>scanlineStride</code>;
   *       <li><code>bitMasks</code>;
   *       <li><code>bitOffsets</code>.
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
    if (! (obj instanceof SinglePixelPackedSampleModel)) 
      return false;
    SinglePixelPackedSampleModel that = (SinglePixelPackedSampleModel) obj;
    if (this.dataType != that.dataType)
      return false;
    if (this.width != that.width)
      return false;
    if (this.height != that.height)
      return false;
    if (this.numBands != that.numBands)
      return false;
    if (this.scanlineStride != that.scanlineStride)
      return false;
    if (!Arrays.equals(this.bitMasks, that.bitMasks))
      return false;
    if (!Arrays.equals(this.bitOffsets, that.bitOffsets)) 
      return false;
    return true;
  }
  
  /**
   * Returns a hash code for this <code>SinglePixelPackedSampleModel</code>.
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
    result = 37 * result + numBands;
    result = 37 * result + scanlineStride;
    for (int i = 0; i < bitMasks.length; i++)
      result = 37 * result + bitMasks[i];
    for (int i = 0; i < bitOffsets.length; i++)
      result = 37 * result + bitOffsets[i];
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
    for(int i = 0; i < bitMasks.length; i+=1)
    {
      result.append(", mask[").append(i).append("]=0x").append(
          Integer.toHexString(bitMasks[i]));
    }
    
    result.append("]");
    return result.toString();
  }
}
