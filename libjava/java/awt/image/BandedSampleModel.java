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

/**
 * MultiPixelPackedSampleModel provides a single band model that supports
 * multiple pixels in a single unit.  Pixels have 2^n bits and 2^k pixels fit
 * per data element.
 *
 * @author Jerry Quinn (jlquinn@optonline.net)
 */
public final class BandedSampleModel extends ComponentSampleModel
{
  private int[] bitMasks;
  private int[] bitOffsets;
  private int[] sampleSize;
  private int dataBitOffset;
  private int elemBits;
  private int numberOfBits;
  private int numElems;

  public BandedSampleModel(int dataType, int w, int h, int numBands)
  {
    super(dataType, w, h, 1, w, new int[numBands]);
  }

  public BandedSampleModel(int dataType, int w, int h, int scanlineStride,
			   int[] bankIndices, int[] bandOffsets)
  {
    super(dataType, w, h, 1, scanlineStride, bankIndices, bandOffsets);
  }

  public SampleModel createCompatibleSampleModel(int w, int h)
  {
    // NOTE: blackdown 1.4.1 sets all offsets to 0.  Sun's 1.4.2 docs
    // disagree.

    // Compress offsets so minimum is 0, others w*scanlineStride
    int[] newoffsets = new int[bandOffsets.length];
    int[] order = new int[bandOffsets.length];
    for (int i=0; i < bandOffsets.length; i++)
      order[i] = i;
    // FIXME: This is N^2, but not a big issue, unless there's a lot of
    // bands...
    for (int i=0; i < bandOffsets.length; i++)
      for (int j=i+1; j < bandOffsets.length; i++)
	if (bankIndices[order[i]] > bankIndices[order[j]]
	    || (bankIndices[order[i]] == bankIndices[order[j]]
		&& bandOffsets[order[i]] > bandOffsets[order[j]]))
	  {
	    int t = order[i]; order[i] = order[j]; order[j] = t;
	  }
    int bank = 0;
    int offset = 0;
    for (int i=0; i < bandOffsets.length; i++)
      {
	if (bankIndices[order[i]] != bank)
	  {
	    bank = bankIndices[order[i]];
	    offset = 0;
	  }
	newoffsets[order[i]] = offset;
	offset += w * scanlineStride;
      }
    
    return new BandedSampleModel(dataType, w, h, scanlineStride, bankIndices, newoffsets);
  }


  public SampleModel createSubsetSampleModel(int[] bands)
  {
    int[] newoff = new int[bands.length];
    int[] newbanks = new int[bands.length];
    for (int i=0; i < bands.length; i++)
      {
	int b = bands[i];
	newoff[i] = bandOffsets[b];
	newbanks[i] = bankIndices[b];
      }

    if (bands.length > bankIndices.length)
      throw new
	RasterFormatException("BandedSampleModel createSubsetSampleModel too"
			      +" many bands");
    
    return new BandedSampleModel(dataType, width, height, scanlineStride,
				 newbanks, newoff);
  }

  /**
   * Extract all samples of one pixel and return in an array of transfer type.
   *
   * Extracts the pixel at x, y from data and stores samples into the array
   * obj.  If obj is null, a new array of getTransferType() is created.
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
      {
	byte[] b = (byte[])obj;
	if (b == null) b = new byte[numBands];
	for (int i=0; i < numBands; i++)
	  b[i] = (byte)getSample(x, y, i, data);
	return b;
      }
    case DataBuffer.TYPE_SHORT:
    case DataBuffer.TYPE_USHORT:
      {
	short[] b = (short[])obj;
	if (b == null) b = new short[numBands];
	for (int i=0; i < numBands; i++)
	  b[i] = (short)getSample(x, y, i, data);
	return b;
      }
    case DataBuffer.TYPE_INT:
      {
	int[] b = (int[])obj;
	if (b == null) b = new int[numBands];
	for (int i=0; i < numBands; i++)
	  b[i] = getSample(x, y, i, data);
	return b;
      }
    case DataBuffer.TYPE_FLOAT:
      {
	float[] b = (float[])obj;
	if (b == null) b = new float[numBands];
	for (int i=0; i < numBands; i++)
	  b[i] = getSampleFloat(x, y, i, data);
	return b;
      }
    case DataBuffer.TYPE_DOUBLE:
      {
	double[] b = (double[])obj;
	if (b == null) b = new double[numBands];
	for (int i=0; i < numBands; i++)
	  b[i] = getSample(x, y, i, data);
	return b;
      }

    default:
      // Seems like the only sensible thing to do.
      throw new ClassCastException();
    }
  }

  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    if (iArray == null) iArray = new int[numBands];
    for (int i=0; i < numBands; i++)
      iArray[i] = getSample(x, y, 0, data);
	
    return iArray;
  }

  /**
   * Copy pixels from a region into an array.
   *
   * Copies the samples of the pixels in the rectangle starting at x, y that
   * is w pixels wide and h scanlines high.  When there is more than one band,
   * the samples stored in order before the next pixel.  This ordering isn't
   * well specified in Sun's docs as of 1.4.2.
   *
   * If iArray is null, a new array is allocated, filled, and returned.
   *
   * @param x The x-coordinate of the pixel rectangle to store in
   * <code>iArray</code>.
   * @param y The y-coordinate of the pixel rectangle to store in
   * <code>iArray</code>.
   * @param w The width in pixels of the rectangle.
   * @param h The height in pixels of the rectangle.
   * @param iArray The int array to store the pixels into or null to force
   * creation.
   * @param data The DataBuffer that is the source of the pixel data.
   * @return The primitive array containing the pixel data.
   */
  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
			 DataBuffer data)
  {
    if (iArray == null) iArray = new int[w*h*numBands];
    int outOffset = 0;
    for (y=0; y<h; y++)
      {
	for (x=0; x<w;)
	  {
	    for (int b=0; b < numBands; b++)
	      {
		int offset = bandOffsets[b] + y * scanlineStride + x;
		iArray[outOffset++] =
		  data.getElem(bankIndices[b], offset);
	      }
	  }
      }
    return iArray;	
  }

  public int getSample(int x, int y, int b, DataBuffer data)
  {
    int offset = bandOffsets[b] + y * scanlineStride + x;
    return data.getElem(bankIndices[b], offset);
  }
  
  public float getSampleFloat(int x, int y, int b, DataBuffer data)
  {
    int offset = bandOffsets[b] + y * scanlineStride + x;
    return data.getElemFloat(bankIndices[b], offset);
  }
  
  public double getSampleDouble(int x, int y, int b, DataBuffer data)
  {
    int offset = bandOffsets[b] + y * scanlineStride + x;
    return data.getElemDouble(bankIndices[b], offset);
  }
  
  /**
   * Copy one band's samples from a region into an array.
   *
   * Copies from one band the samples of the pixels in the rectangle starting
   * at x, y that is w pixels wide and h scanlines high.
   *
   * If iArray is null, a new array is allocated, filled, and returned.
   *
   * @param x The x-coordinate of the pixel rectangle to store in
   * <code>iArray</code>.
   * @param y The y-coordinate of the pixel rectangle to store in
   * <code>iArray</code>.
   * @param w The width in pixels of the rectangle.
   * @param h The height in pixels of the rectangle.
   * @param b The band to retrieve.
   * @param iArray The int array to store the pixels into or null to force
   * creation.
   * @param data The DataBuffer that is the source of the pixel data.
   * @return The primitive array containing the pixel data.
   */
  public int[] getSamples(int x, int y, int w, int h, int b, int[] iArray,
			  DataBuffer data)
  {
    if (iArray == null) iArray = new int[w*h];
    int outOffset = 0;
    for (y=0; y<h; y++)
      {
	for (x=0; x<w;)
	  {
	    int offset = bandOffsets[b] + y * scanlineStride + x;
	    iArray[outOffset++] =
	      data.getElem(bankIndices[b], offset);
	  }
      }
    return iArray;	
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

    int offset = y * scanlineStride + x;
    
    try
      {
	switch (transferType)
	  {
	  case DataBuffer.TYPE_BYTE:
	    {
	      DataBufferByte out = (DataBufferByte) data;
	      byte[] in = (byte[]) obj;
	      for (int i=0; i < numBands; i++)
		out.getData(bankIndices[i])[offset + bandOffsets[i]] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_SHORT:
	    {
	      DataBufferShort out = (DataBufferShort) data;
	      short[] in = (short[]) obj;
	      for (int i=0; i < numBands; i++)
		out.getData(bankIndices[i])[offset + bandOffsets[i]] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_USHORT:
	    {
	      DataBufferUShort out = (DataBufferUShort) data;
	      short[] in = (short[]) obj;
	      for (int i=0; i < numBands; i++)
		out.getData(bankIndices[i])[offset + bandOffsets[i]] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_INT:
	    {
	      DataBufferInt out = (DataBufferInt) data;
	      int[] in = (int[]) obj;
	      for (int i=0; i < numBands; i++)
		out.getData(bankIndices[i])[offset + bandOffsets[i]] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_FLOAT:
	    {
	      DataBufferFloat out = (DataBufferFloat) data;
	      float[] in = (float[]) obj;
	      for (int i=0; i < numBands; i++)
		out.getData(bankIndices[i])[offset + bandOffsets[i]] = in[0];
	      return;
	    }
	  case DataBuffer.TYPE_DOUBLE:
	    {
	      DataBufferDouble out = (DataBufferDouble) data;
	      double[] in = (double[]) obj;
	      for (int i=0; i < numBands; i++)
		out.getData(bankIndices[i])[offset + bandOffsets[i]] = in[0];
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
    for (int b=0; b < numBands; b++)
      data.setElem(bankIndices[b], bandOffsets[b] + y * scanlineStride + x,
		   iArray[b]);
  }

  public void setPixels(int x, int y, int w, int h, int[] iArray,
			DataBuffer data)
  {
    int inOffset = 0;
    for (int hh = 0; hh < h; hh++)
      {
	for (int ww = 0; ww < w; ww++)
	  {
	    int offset = y * scanlineStride + (x + ww);
	    for (int b=0; b < numBands; b++)
	      data.setElem(bankIndices[b], bandOffsets[b] + offset,
			   iArray[inOffset++]);
	  }
	y++;
      }
  }

  public void setSample(int x, int y, int b, int s, DataBuffer data)
  {
    data.setElem(bankIndices[b], bandOffsets[b] + y * scanlineStride + x, s);
  }
  
  public void setSample(int x, int y, int b, float s, DataBuffer data)
  {
    data.setElemFloat(bankIndices[b], bandOffsets[b] + y * scanlineStride + x, s);
  }
  
  public void setSample(int x, int y, int b, double s, DataBuffer data)
  {
    data.setElemDouble(bankIndices[b], bandOffsets[b] + y * scanlineStride + x, s);
  }
  
  public void setSamples(int x, int y, int w, int h, int b, int[] iArray,
			 DataBuffer data)
  {
    int inOffset = 0;

    switch (getTransferType())
      {
      case DataBuffer.TYPE_BYTE:
	{
	  DataBufferByte out = (DataBufferByte) data;
	  byte[] bank = out.getData(bankIndices[b]);
	  for (int hh = 0; hh < h; hh++)
	    {
	      for (int ww = 0; ww < w; ww++)
		{
		  int offset = bandOffsets[b] + y * scanlineStride + (x + ww);
		  bank[offset] = (byte)iArray[inOffset++];
		}
	      y++;
	    }
	  return;
	}
      case DataBuffer.TYPE_SHORT:
	{
	  DataBufferShort out = (DataBufferShort) data;
	  short[] bank = out.getData(bankIndices[b]);
	  for (int hh = 0; hh < h; hh++)
	    {
	      for (int ww = 0; ww < w; ww++)
		{
		  int offset = bandOffsets[b] + y * scanlineStride + (x + ww);
		  bank[offset] = (short)iArray[inOffset++];
		}
	      y++;
	    }
	  return;
	}
      case DataBuffer.TYPE_USHORT:
	{
	  DataBufferShort out = (DataBufferShort) data;
	  short[] bank = out.getData(bankIndices[b]);
	  for (int hh = 0; hh < h; hh++)
	    {
	      for (int ww = 0; ww < w; ww++)
		{
		  int offset = bandOffsets[b] + y * scanlineStride + (x + ww);
		  bank[offset] = (short)iArray[inOffset++];
		}
	      y++;
	    }
	  return;
	}
      case DataBuffer.TYPE_INT:
	{
	  DataBufferInt out = (DataBufferInt) data;
	  int[] bank = out.getData(bankIndices[b]);
	  for (int hh = 0; hh < h; hh++)
	    {
	      for (int ww = 0; ww < w; ww++)
		{
		  int offset = bandOffsets[b] + y * scanlineStride + (x + ww);
		  bank[offset] = iArray[inOffset++];
		}
	      y++;
	    }
	  return;
	}
      case DataBuffer.TYPE_FLOAT:
      case DataBuffer.TYPE_DOUBLE:
	break;
      default:
	throw new ClassCastException("Unsupported data type");
      }

    // Default implementation probably slower for float and double
    for (int hh = 0; hh < h; hh++)
      {
	for (int ww = 0; ww < w; ww++)
	  {
	    int offset = bandOffsets[b] + y * scanlineStride + (x + ww);
	    data.setElem(bankIndices[b], offset, iArray[inOffset++]);
	  }
	y++;
      }
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
