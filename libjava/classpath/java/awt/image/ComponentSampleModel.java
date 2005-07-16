/* Copyright (C) 2000, 2002  Free Software Foundation

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

/* FIXME: This class does not yet support data type TYPE_SHORT */

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
  protected int[] bandOffsets;
  protected int[] bankIndices;
  
  // FIXME: Should we really shadow the numBands in the superclass?
  //protected int numBands;
  
  /** Used when creating data buffers. */
  protected int numBanks;

  protected int scanlineStride;
  
  protected int pixelStride;
  
  private boolean tightPixelPacking = false;
  
  public ComponentSampleModel(int dataType,
			      int w, int h,
			      int pixelStride,
			      int scanlineStride,
			      int[] bandOffsets)
  {
    this(dataType, w, h, pixelStride, scanlineStride,
	 new int[bandOffsets.length], bandOffsets);
  }
    
  public ComponentSampleModel(int dataType,
			      int w, int h,
			      int pixelStride,
			      int scanlineStride,
			      int[] bankIndices,
			      int[] bandOffsets)
  {
    super(dataType, w, h, bandOffsets.length);
    if ((pixelStride<0) || (scanlineStride<0) || 
	(bandOffsets.length<1) ||
	(bandOffsets.length != bankIndices.length))
      throw new IllegalArgumentException();
    
    this.bandOffsets = bandOffsets;
    this.bankIndices = bankIndices;

    this.numBanks = 0;
    for (int b=0; b<bankIndices.length; b++)
      this.numBanks = Math.max(this.numBanks, bankIndices[b]+1);

    this.scanlineStride = scanlineStride;
    this.pixelStride = pixelStride;

    // See if we can use some speedups

    /* FIXME: May these checks should be reserved for the
       PixelInterleavedSampleModel? */
	
    if (pixelStride == numBands)
      {
	tightPixelPacking = true;
	for (int b=0; b<numBands; b++) {
	  if ((bandOffsets[b] != b) || (bankIndices[b] !=0))
	    {
	      tightPixelPacking = false;
	      break;
	    }
	}
      }
  }		

  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    return new ComponentSampleModel(dataType, w, h, pixelStride,
				    scanlineStride, bankIndices,
				    bandOffsets);
  }

  public SampleModel createSubsetSampleModel(int[] bands)
  {
    int numBands = bands.length;
    
    int[] bankIndices = new int[numBands];
    int[] bandOffsets = new int[numBands];
    for (int b=0; b<numBands; b++)
      {
	bankIndices[b] = this.bankIndices[bands[b]];
	bandOffsets[b] = this.bandOffsets[bands[b]];
      }

    return new ComponentSampleModel(dataType, width, height, pixelStride,
				    scanlineStride, bankIndices,
				    bandOffsets);
  }

  public DataBuffer createDataBuffer()
  {
    // Maybe this value should be precalculated in the constructor?
    int highestOffset = 0;
    for (int b=0; b<numBands; b++)
      {
	highestOffset = Math.max(highestOffset, bandOffsets[b]);
      }
    int size = pixelStride*(width-1) + scanlineStride*(height-1) +
      highestOffset + 1;
    
    return Buffers.createBuffer(getDataType(), size, numBanks);
  }

  public int getOffset(int x, int y)
  {
    return getOffset(x, y, 0);
  }

  public int getOffset(int x, int y, int b)
  {
    return bandOffsets[b] + pixelStride*x + scanlineStride*y;
  }

  public final int[] getSampleSize()
  {
    int size = DataBuffer.getDataTypeSize(getDataType());
    int[] sizes = new int[numBands];

    java.util.Arrays.fill(sizes, size);
    return sizes;
  }

  public final int getSampleSize(int band)
  {
    return DataBuffer.getDataTypeSize(getDataType());
  }

  public final int[] getBankIndices()
  {
    return bankIndices;
  }

  public final int[] getBandOffsets()
  {
    return bandOffsets;
  }

  public final int getScanlineStride()
  {
    return scanlineStride;
  }

  public final int getPixelStride()
  {
    return pixelStride;
  }

  public final int getNumDataElements()
  {
    return numBands;
  }

  public Object getDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int xyOffset = pixelStride*x + scanlineStride*y;
    
    int[] totalBandDataOffsets = new int[numBands];
    
    /* Notice that band and bank offsets are different. Band offsets
       are managed by the sample model, and bank offsets are managed
       by the data buffer. Both must be accounted for. */
    
    /* FIXME: For single pixels, it is probably easier to simple
       call getElem instead of calculating the bank offset ourself.
       
       On the other hand, then we need to push the value through
       the int type returned by the getElem method.  */
    
    int[] bankOffsets = data.getOffsets();
    
    for (int b=0; b<numBands; b++)
      {
	totalBandDataOffsets[b] = 
	  bandOffsets[b]+bankOffsets[bankIndices[b]] + xyOffset;
      }
	
    try
      {
	switch (getTransferType())
	  {
	  case DataBuffer.TYPE_BYTE:
	    DataBufferByte inByte = (DataBufferByte) data;
	    byte[] outByte = (byte[]) obj;
	    if (outByte == null) outByte = new byte[numBands];
		
	    for (int b=0; b<numBands; b++)
	      {
		int dOffset = totalBandDataOffsets[b];
		outByte[b] = inByte.getData(bankIndices[b])[dOffset];
	      }
	    return outByte;
		
	  case DataBuffer.TYPE_USHORT:
	    DataBufferUShort inUShort = (DataBufferUShort) data;
	    short[] outUShort = (short[]) obj;
	    if (outUShort == null) outUShort = new short[numBands];
		
	    for (int b=0; b<numBands; b++)
	      {
		int dOffset = totalBandDataOffsets[b];
		outUShort[b] = inUShort.getData(bankIndices[b])[dOffset];
	      }
	    return outUShort;

	  case DataBuffer.TYPE_SHORT:
	    DataBufferShort inShort = (DataBufferShort) data;
	    short[] outShort = (short[]) obj;
	    if (outShort == null) outShort = new short[numBands];
		
	    for (int b=0; b<numBands; b++)
	      {
		int dOffset = totalBandDataOffsets[b];
		outShort[b] = inShort.getData(bankIndices[b])[dOffset];
	      }
	    return outShort;

	  case DataBuffer.TYPE_INT:
	    DataBufferInt inInt = (DataBufferInt) data;
	    int[] outInt = (int[]) obj;
	    if (outInt == null) outInt = new int[numBands];
		
	    for (int b=0; b<numBands; b++)
	      {
		int dOffset = totalBandDataOffsets[b];
		outInt[b] = inInt.getData(bankIndices[b])[dOffset];
	      }
	    return outInt;

	  case DataBuffer.TYPE_FLOAT:
	    DataBufferFloat inFloat = (DataBufferFloat) data;
	    float[] outFloat = (float[]) obj;
	    if (outFloat == null) outFloat = new float[numBands];

	    for (int b=0; b<numBands; b++)
	      {
		int dOffset = totalBandDataOffsets[b];
		outFloat[b] = inFloat.getData(bankIndices[b])[dOffset];
	      }
	    return outFloat;
	    
	  case DataBuffer.TYPE_DOUBLE:
	    DataBufferDouble inDouble = (DataBufferDouble) data;
	    double[] outDouble = (double[]) obj;
	    if (outDouble == null) outDouble = new double[numBands];

	    for (int b=0; b<numBands; b++)
	      {
		int dOffset = totalBandDataOffsets[b];
		outDouble[b] = inDouble.getData(bankIndices[b])[dOffset];
	      }
	    return outDouble;
	    
	  default:
	      throw new IllegalStateException("unknown transfer type " +
					      getTransferType());
	  }
      }
    catch (ArrayIndexOutOfBoundsException aioobe)
      {
	String msg = "While reading data elements, " +
	  "x=" + x + ", y=" + y +", " + ", xyOffset=" + xyOffset +
	  ", data.getSize()=" + data.getSize() + ": " + aioobe;
	throw new ArrayIndexOutOfBoundsException(msg);
      }
  }

  public Object getDataElements(int x, int y, int w, int h, Object obj,
				DataBuffer data)
  {
    if (!tightPixelPacking)
      {
	return super.getDataElements(x, y, w, h, obj, data);
      }

    // using get speedup
    
    // We can copy whole rows
    int rowSize = w*numBands;
    int dataSize = rowSize*h;
    
    DataBuffer transferBuffer =
      Buffers.createBuffer(getTransferType(), obj, dataSize);
    obj = Buffers.getData(transferBuffer);

    int inOffset =
      pixelStride*x +
      scanlineStride*y +
      data.getOffset(); // Assumes only one band is used

    /* We don't add band offsets since we assume that bands have
       offsets 0, 1, 2, ... */

    // See if we can copy everything in one go
    if (scanlineStride == rowSize)
      {
	// Collapse scan lines:
	rowSize *= h;
	// We ignore scanlineStride since it won't be of any use
	h = 1;
      }

    int outOffset = 0;
    Object inArray = Buffers.getData(data);
    for (int yd = 0; yd<h; yd++)
      {
	System.arraycopy(inArray, inOffset, obj, outOffset, rowSize);
	inOffset  += scanlineStride;
	outOffset += rowSize;
      }
    return obj;
  }

  public void setDataElements(int x, int y, int w, int h,
			      Object obj, DataBuffer data)
  {
    if (!tightPixelPacking)
      {
	super.setDataElements(x, y, w, h, obj, data);
	return;
      }

    // using set speedup, we can copy whole rows
    int rowSize = w*numBands;
    int dataSize = rowSize*h;
    
    DataBuffer transferBuffer =
      Buffers.createBufferFromData(getTransferType(), obj, dataSize);

    int[] bankOffsets = data.getOffsets();

    int outOffset =
      pixelStride*x +
      scanlineStride*y +
      bankOffsets[0]; // same assuptions as in get...

    // See if we can copy everything in one go
    if (scanlineStride == rowSize)
      {
	// Collapse scan lines:
	rowSize *= h;
	h = 1;
      }

    int inOffset = 0;
    Object outArray = Buffers.getData(data);
    for (int yd = 0; yd<h; yd++)
      {
	System.arraycopy(obj, inOffset, outArray, outOffset, rowSize);
	outOffset += scanlineStride;
	inOffset  += rowSize;
      }
  }

  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    int offset = pixelStride*x + scanlineStride*y;
    if (iArray == null) iArray = new int[numBands];
    for (int b=0; b<numBands; b++)
      {
	iArray[b] = data.getElem(bankIndices[b], offset+bandOffsets[b]);
      }
    return iArray;
  }

  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
			 DataBuffer data)
  {
    int offset = pixelStride*x + scanlineStride*y;
    if (iArray == null) iArray = new int[numBands*w*h];
    int outOffset = 0;
    for (y=0; y<h; y++)
      {
	int lineOffset = offset;
	for (x=0; x<w; x++)
	  {
	    for (int b=0; b<numBands; b++)
	      {
		iArray[outOffset++] = 
		  data.getElem(bankIndices[b], lineOffset+bandOffsets[b]);
	      }
	    lineOffset += pixelStride;
	  }
	offset += scanlineStride;
      }
    return iArray;
  }
    
  public int getSample(int x, int y, int b, DataBuffer data)
  {
    return data.getElem(bankIndices[b], getOffset(x, y, b));
  }

  public void setDataElements(int x, int y, Object obj, DataBuffer data)
  {
    int offset = pixelStride*x + scanlineStride*y;
    int[] totalBandDataOffsets = new int[numBands];
    int[] bankOffsets = data.getOffsets();
    for (int b=0; b<numBands; b++)
      totalBandDataOffsets[b] =
	bandOffsets[b]+bankOffsets[bankIndices[b]] + offset;

    switch (getTransferType())
      {
      case DataBuffer.TYPE_BYTE:
	{
	  DataBufferByte out = (DataBufferByte) data;
	  byte[] in = (byte[]) obj;
	  
	  for (int b=0; b<numBands; b++)
	    out.getData(bankIndices[b])[totalBandDataOffsets[b]] = in[b];
	  
	  return;
	}
      case DataBuffer.TYPE_USHORT:
	{
	  DataBufferUShort out = (DataBufferUShort) data;
	  short[] in = (short[]) obj;
	  
	  for (int b=0; b<numBands; b++)
	    out.getData(bankIndices[b])[totalBandDataOffsets[b]] = in[b];
	  
	  return;
	}
      case DataBuffer.TYPE_SHORT:
	{
	  DataBufferShort out = (DataBufferShort) data;
	  short[] in = (short[]) obj;
	  
	  for (int b=0; b<numBands; b++)
	    out.getData(bankIndices[b])[totalBandDataOffsets[b]] = in[b];
	  
	  return;
	}
      case DataBuffer.TYPE_INT:
	{
	  DataBufferInt out = (DataBufferInt) data;
	  int[] in = (int[]) obj;
	  
	  for (int b=0; b<numBands; b++)
	    out.getData(bankIndices[b])[totalBandDataOffsets[b]] = in[b];
	  
	  return;
	}
      case DataBuffer.TYPE_FLOAT:
	{
	  DataBufferFloat out = (DataBufferFloat) data;
	  float[] in = (float[]) obj;
	  
	  for (int b=0; b<numBands; b++)
	    out.getData(bankIndices[b])[totalBandDataOffsets[b]] = in[b];
	  
	  return;
	}
      case DataBuffer.TYPE_DOUBLE:
	{
	  DataBufferDouble out = (DataBufferDouble) data;
	  double[] in = (double[]) obj;
	  
	  for (int b=0; b<numBands; b++)
	    out.getData(bankIndices[b])[totalBandDataOffsets[b]] = in[b];
	  
	  return;
	}
      default:
	throw new UnsupportedOperationException("transfer type not " +
						"implemented");
      }
  }
  
  public void setPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    int offset = pixelStride*x + scanlineStride*y;
    for (int b=0; b<numBands; b++)
      data.setElem(bankIndices[b], offset+bandOffsets[b], iArray[b]);
  }
    
  public void setSample(int x, int y, int b, int s, DataBuffer data)
  {
    data.setElem(bankIndices[b], getOffset(x, y, b), s);
  }
}
