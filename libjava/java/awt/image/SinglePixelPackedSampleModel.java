/* Copyright (C) 2000, 2002, 2003, 2004  Free Software Foundation

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

import gnu.java.awt.BitMaskExtent;
import gnu.java.awt.Buffers;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class SinglePixelPackedSampleModel extends SampleModel
{
  private int scanlineStride;
  private int[] bitMasks;
  private int[] bitOffsets;
  private int[] sampleSize;
  
  public SinglePixelPackedSampleModel(int dataType, int w, int h,
				      int[] bitMasks)
  {
    this(dataType, w, h, w, bitMasks);
  }

  public SinglePixelPackedSampleModel(int dataType, int w, int h,
				      int scanlineStride, int[] bitMasks)
  {
    super(dataType, w, h, bitMasks.length);
    
    this.scanlineStride = scanlineStride;
    this.bitMasks = bitMasks;
    
    bitOffsets = new int[numBands];
    sampleSize = new int[numBands];
    
    BitMaskExtent extent = new BitMaskExtent();
    for (int b=0; b<numBands; b++)
      {
	extent.setMask(bitMasks[b]);
	sampleSize[b] = extent.bitWidth;
	bitOffsets[b] = extent.leastSignificantBit;
      }
  }

  public int getNumDataElements()
  {
    return 1;
  }

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
   */
  public DataBuffer createDataBuffer()
  {
    int size;

    // We can save (scanlineStride - width) pixels at the very end of
    // the buffer. The Sun reference implementation (J2SE 1.3.1 and
    // 1.4.1_01) seems to do this; tested with Mauve test code.
    size = scanlineStride * (height - 1) + width;

    return Buffers.createBuffer(getDataType(), size);
  }


  public int[] getSampleSize()
  {
    return sampleSize;
  }
  
  public int getSampleSize(int band)
  {
    return sampleSize[band];
  }

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

  public int getScanlineStride()
  {
    return scanlineStride;
  }

  public SampleModel createSubsetSampleModel(int[] bands)
  {
    // FIXME: Is this the right way to interpret bands?
    
    int numBands = bands.length;
    
    int[] bitMasks = new int[numBands];

    for (int b=0; b<numBands; b++)
      bitMasks[b] = this.bitMasks[bands[b]];

    return new SinglePixelPackedSampleModel(dataType, width, height,
					    scanlineStride, bitMasks);
  }

  public Object getDataElements(int x, int y, Object obj,
				DataBuffer data)
  {
    int offset = scanlineStride*y + x + data.getOffset();
    
    return Buffers.getData(data, offset, obj,
			   0, // destination offset,
			   1  // length
			   );
  }
  
  /**
   * This is a more efficient implementation of the default implementation in the super
   * class. 
   * @param x The x-coordinate of the pixel rectangle to store in <code>obj</code>.
   * @param y The y-coordinate of the pixel rectangle to store in <code>obj</code>.
   * @param w The width of the pixel rectangle to store in <code>obj</code>.
   * @param h The height of the pixel rectangle to store in <code>obj</code>.
   * @param obj The primitive array to store the pixels into or null to force creation.
   * @param data The DataBuffer that is the source of the pixel data.
   * @return The primitive array containing the pixel data.
   * @see java.awt.image.SampleModel#getDataElements(int, int, int, int, java.lang.Object, java.awt.image.DataBuffer)
   */
  public Object getDataElements(int x, int y, int w, int h, Object obj,
							DataBuffer data)
  {
    int size = w*h;
    int dataSize = size;
    Object pixelData = null;
    switch (getTransferType())
    {
      case DataBuffer.TYPE_BYTE:
        pixelData = ((DataBufferByte) data).getData();
        if (obj == null) obj = new byte[dataSize];
        break;
       case DataBuffer.TYPE_USHORT:
         pixelData = ((DataBufferUShort) data).getData();
         if (obj == null) obj = new short[dataSize];
         break;
        case DataBuffer.TYPE_INT:
          pixelData = ((DataBufferInt) data).getData();
          if (obj == null) obj = new int[dataSize];
          break;
         default:
             // Seems like the only sensible thing to do.
           throw new ClassCastException();
      }
      if(x==0 && scanlineStride == w)
      { 
        // The full width need to be copied therefore we can copy in one shot.
        System.arraycopy(pixelData, scanlineStride*y + data.getOffset(), obj, 0, size);
      }
      else
      {  
        // Since we do not need the full width we need to copy line by line.
        int outOffset = 0;
        int dataOffset = scanlineStride*y + x + data.getOffset();
        for (int yy = y; yy<(y+h); yy++)
        {
          System.arraycopy(pixelData, dataOffset, obj, outOffset, w);
          dataOffset += scanlineStride;
          outOffset += w;
        }
      }
    return obj;
  }
  

  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    int offset = scanlineStride*y + x;
    if (iArray == null) iArray = new int[numBands];
    int samples = data.getElem(offset);

    for (int b=0; b<numBands; b++)
      iArray[b] = (samples & bitMasks[b]) >>> bitOffsets[b];
	
    return iArray;
  }

  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
			 DataBuffer data)
  {
    int offset = scanlineStride*y + x;
    if (iArray == null) iArray = new int[numBands*w*h];
    int outOffset = 0;
    for (y=0; y<h; y++)
      {
	int lineOffset = offset;
	for (x=0; x<w; x++)
	  {
	    int samples = data.getElem(lineOffset++);
	    for (int b=0; b<numBands; b++)
	      iArray[outOffset++] = (samples & bitMasks[b]) >>> bitOffsets[b];
	  }
	offset += scanlineStride;
      }
    return iArray;	
  }

  public int getSample(int x, int y, int b, DataBuffer data)
  {
    int offset = scanlineStride*y + x;
    int samples = data.getElem(offset);
    return (samples & bitMasks[b]) >>> bitOffsets[b];
  }
  
  /**
   * This method implements a more efficient way to set data elements than the default
   * implementation of the super class. It sets the data elements line by line instead 
   * of pixel by pixel.
   * @param x The x-coordinate of the data elements in <code>obj</code>.
   * @param y The y-coordinate of the data elements in <code>obj</code>.
   * @param w The width of the data elements in <code>obj</code>.
   * @param h The height of the data elements in <code>obj</code>.
   * @param obj The primitive array containing the data elements to set.
   * @param data The DataBuffer to store the data elements into.
   * @see java.awt.image.SampleModel#setDataElements(int, int, int, int, java.lang.Object, java.awt.image.DataBuffer)
   */
  public void setDataElements(int x, int y, int w, int h,
				Object obj, DataBuffer data)
  {
    
    Object pixelData;
    switch (getTransferType())
    {
      case DataBuffer.TYPE_BYTE:
        pixelData = ((DataBufferByte) data).getData();
        break;
       case DataBuffer.TYPE_USHORT:
         pixelData = ((DataBufferUShort) data).getData();
         break;
       case DataBuffer.TYPE_INT:
         pixelData = ((DataBufferInt) data).getData();
         break;
       default:
          // Seems like the only sensible thing to do.
          throw new ClassCastException();
    }
    
    int inOffset = 0;
    int dataOffset = scanlineStride*y + x + data.getOffset();
    for (int yy=y; yy<(y+h); yy++)
    {
      System.arraycopy(obj,inOffset,pixelData,dataOffset,w);
      dataOffset += scanlineStride;
      inOffset += w;
    }
  }
  
  
  public void setDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int offset = scanlineStride*y + x + data.getOffset();
    
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
	    // FIXME: Fill in the other possible types.
	  default:
	    throw new InternalError();
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
    int offset = scanlineStride*y + x;
    
    int samples = 0;
    for (int b=0; b<numBands; b++)
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
   * @param obj The primitive array containing the pixels to set.
   * @param data The DataBuffer to store the pixels into.
   * @see java.awt.image.SampleModel#setPixels(int, int, int, int, int[], java.awt.image.DataBuffer)
   */
  public void setPixels(int x, int y, int w, int h, int[] iArray,
						DataBuffer data)
  {
    int inOffset = 0;
    int[] pixel = new int[numBands];
    for (int yy=y; yy<(y+h); yy++)
     {
      int offset = scanlineStride*yy + x;
      for (int xx=x; xx<(x+w); xx++)
       { 
        int samples = 0;
        for (int b=0; b<numBands; b++)
          samples |= (iArray[inOffset+b] << bitOffsets[b]) & bitMasks[b];
        data.setElem(0, offset, samples);
        inOffset += numBands;
        offset += 1;
      }
    }
  }
  
  
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
