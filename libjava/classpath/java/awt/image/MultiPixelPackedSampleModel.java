/* Copyright (C) 2004  Free Software Foundation

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

  public MultiPixelPackedSampleModel(int dataType, int w, int h,
				     int numberOfBits)
  {
    this(dataType, w, h, numberOfBits, 0, 0);
  }

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
      scanlineStride = ((dataBitOffset + w * numberOfBits) / elemBits);
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
   */
  public DataBuffer createDataBuffer()
  {
    int size;

    // FIXME:  The comment refers to SinglePixelPackedSampleModel.  See if the
    // same can be done for MultiPixelPackedSampleModel.
    // We can save (scanlineStride - width) pixels at the very end of
    // the buffer. The Sun reference implementation (J2SE 1.3.1 and
    // 1.4.1_01) seems to do this; tested with Mauve test code.
    size = scanlineStride * height;

    return Buffers.createBuffer(getDataType(), size);
  }


  public int getNumDataElements()
  {
    return 1;
  }

  public int[] getSampleSize()
  {
    return sampleSize;
  }
  
  public int getSampleSize(int band)
  {
    return sampleSize[0];
  }

  public int getOffset(int x, int y)
  {
    return scanlineStride * y + ((dataBitOffset + x*numberOfBits) / elemBits);
  }

  public int getBitOffset(int x)
  {
    return (dataBitOffset + x*numberOfBits) % elemBits;
  }

  public int getDataBitOffset()
  {
    return dataBitOffset;
  }

  public int getScanlineStride()
  {
    return scanlineStride;
  }

  public int getPixelBitStride()
  {
    return numberOfBits;
  }


  public SampleModel createSubsetSampleModel(int[] bands)
  {
    int numBands = bands.length;
    if (numBands != 1)
      throw new RasterFormatException("MultiPixelPackedSampleModel only"
				      + " supports one band");
    
    return new MultiPixelPackedSampleModel(dataType, width, height,
					   numberOfBits, scanlineStride,
					   dataBitOffset);
  }

  /**
   * Extract one pixel and return in an array of transfer type.
   *
   * Extracts the pixel at x, y from data and stores into the 0th index of the
   * array obj, since there is only one band.  If obj is null, a new array of
   * getTransferType() is created.
   *
   * @param x The x-coordinate of the pixel rectangle to store in <code>obj</code>.
   * @param y The y-coordinate of the pixel rectangle to store in <code>obj</code>.
   * @param obj The primitive array to store the pixels into or null to force creation.
   * @param data The DataBuffer that is the source of the pixel data.
   * @return The primitive array containing the pixel data.
   * @see java.awt.image.SampleModel#getDataElements(int, int, java.lang.Object, java.awt.image.DataBuffer)
   */
  public Object getDataElements(int x, int y, Object obj,
				DataBuffer data)
  {
    int pixel = getSample(x, y, 0, data);
    switch (getTransferType())
    {
    case DataBuffer.TYPE_BYTE:
      if (obj == null) obj = new byte[1];
      ((byte[])obj)[0] = (byte)pixel;
      return obj;
    case DataBuffer.TYPE_USHORT:
      if (obj == null) obj = new short[1];
      ((short[])obj)[0] = (short)pixel;
      return obj;
    case DataBuffer.TYPE_INT:
      if (obj == null) obj = new int[1];
      ((int[])obj)[0] = pixel;
      return obj;
    default:
      // Seems like the only sensible thing to do.
      throw new ClassCastException();
    }
  }

  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    if (iArray == null) iArray = new int[1];
    iArray[0] = getSample(x, y, 0, data);
	
    return iArray;
  }

  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
			 DataBuffer data)
  {
    int offset = getOffset(x, y);
    if (iArray == null) iArray = new int[w*h];
    int outOffset = 0;
    for (y=0; y<h; y++)
      {
	int lineOffset = offset;
	for (x=0; x<w;)
	  {
	    int samples = data.getElem(lineOffset++);
	    for (int b=0; b<numElems && x<w; b++)
	      {
		iArray[outOffset++] = (samples & bitMasks[b]) >>> bitOffsets[b];
		x++;
	      }
	  }
	offset += scanlineStride;
      }
    return iArray;	
  }

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
   * @see java.awt.image.SampleModel#setDataElements(int, int, int, int, java.lang.Object, java.awt.image.DataBuffer)
   */
  public void setDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int transferType = getTransferType();
    if (getTransferType() != data.getDataType())
      {
	throw new IllegalArgumentException("transfer type ("+
					   getTransferType()+"), "+
					   "does not match data "+
					   "buffer type (" +
					   data.getDataType() +
					   ").");
      }

    int offset = getOffset(x, y);
    
    try
      {
	switch (transferType)
	  {
	  case DataBuffer.TYPE_BYTE:
	    {
	      DataBufferByte out = (DataBufferByte) data;
	      byte[] in = (byte[]) obj;
	      out.getData()[offset] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_USHORT:
	    {
	      DataBufferUShort out = (DataBufferUShort) data;
	      short[] in = (short[]) obj;
	      out.getData()[offset] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_INT:
	    {
	      DataBufferInt out = (DataBufferInt) data;
	      int[] in = (int[]) obj;
	      out.getData()[offset] = in[0];
	      return;
	    }
	  default:
	    throw new ClassCastException("Unsupported data type");
	  }
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
	String msg = "While writing data elements" +
	  ", x="+x+", y="+y+
	  ", width="+width+", height="+height+
	  ", scanlineStride="+scanlineStride+
	  ", offset="+offset+
	  ", data.getSize()="+data.getSize()+
	  ", data.getOffset()="+data.getOffset()+
	  ": " +
	  aioobe;
	throw new ArrayIndexOutOfBoundsException(msg);
      }
    }

  public void setPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    setSample(x, y, 0, iArray[0], data);
  }

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
