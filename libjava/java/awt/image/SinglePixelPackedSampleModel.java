/* Copyright (C) 2000, 2002, 2003  Free Software Foundation

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

  public void setSample(int x, int y, int b, int s, DataBuffer data)
  {
    int offset = scanlineStride*y + x;
    int samples = data.getElem(offset);
    int bitMask = bitMasks[b];
    samples &= ~bitMask;
    samples |= (s << bitOffsets[b]) & bitMask;
    data.setElem(offset, samples);
  }
}
