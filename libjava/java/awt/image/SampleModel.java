/* Copyright (C) 2000, 2001, 2002  Free Software Foundation

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
 * @author Rolf W. Rasmussen (rolfwr@ii.uib.no)
 */
public abstract class SampleModel
{
  /** Width of image described. */
  protected int width;
  
  /** Height of image described. */
  protected int height;
  
  /** Number of bands in the image described. */
  protected int numBands;

  /** 
   * The DataBuffer type that is used to store the data of the image
   * described.
   */
  protected int dataType;

  public SampleModel(int dataType, int w, int h, int numBands)
  {
    if ((w <= 0) || (h <= 0)) 
      throw new IllegalArgumentException((w <= 0 ? " width<=0" : " width is ok")
                                         +(h <= 0 ? " height<=0" : " height is ok"));
	
    // FIXME: How can an int be greater than Integer.MAX_VALUE?
    // FIXME: How do we identify an unsupported data type?

    this.dataType = dataType;
    this.width = w;
    this.height = h;
    this.numBands = numBands;  
  }
  
  public final int getWidth()
  {
    return width;
  }

  public final int getHeight()
  {
    return height;
  }

  public final int getNumBands()
  {
    return numBands;
  }
    
  public abstract int getNumDataElements();
  
  public final int getDataType()
  {
    return dataType;
  }

  public int getTransferType()
  {
    // FIXME: Is this a reasonable default implementation?
    return dataType;
  }

  public int[] getPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    if (iArray == null) iArray = new int[numBands];
    for (int b=0; b<numBands; b++) iArray[b] = getSample(x, y, b, data);
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
    int size = w*h;
    int numDataElements = getNumDataElements();
    int dataSize = numDataElements*size;
    
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
    for (int yy = y; yy<(y+h); yy++)
      {
	for (int xx = x; xx<(x+w); xx++)
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
    int size = w*h;
    int numDataElements = getNumDataElements();
    int dataSize = numDataElements*size;
    
    Object pixelData;
    switch (getTransferType())
      {
      case DataBuffer.TYPE_BYTE:
	pixelData = new byte[numDataElements];
	break;
      case DataBuffer.TYPE_USHORT:
	pixelData = new short[numDataElements];
	break;
      case DataBuffer.TYPE_INT:
	pixelData = new int[numDataElements];
	break;
      default:
	// Seems like the only sensible thing to do.
	throw new ClassCastException();
      }
    int inOffset = 0;

    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    System.arraycopy(obj, inOffset, pixelData, 0,
			     numDataElements);
	    setDataElements(xx, yy, pixelData, data);
	    inOffset += numDataElements;
	  }
      }
  }

  public float[] getPixel(int x, int y, float[] fArray, DataBuffer data)
  {
    if (fArray == null) fArray = new float[numBands];
    
    for (int b=0; b<numBands; b++)
      {
        fArray[b] = getSampleFloat(x, y, b, data);
      }
    return fArray;
  }

  public double[] getPixel(int x, int y, double[] dArray, DataBuffer data) {
    if (dArray == null) dArray = new double[numBands];
    for (int b=0; b<numBands; b++)
      {
	dArray[b] = getSampleDouble(x, y, b, data);
      }
    return dArray;
  }

  /* FIXME: Should it return a banded or pixel interleaved array of
     samples? (Assume interleaved.) */
  public int[] getPixels(int x, int y, int w, int h, int[] iArray,
			 DataBuffer data)
  {
    int size = w*h;
    int outOffset = 0;
    int[] pixel = null;
    if (iArray == null) iArray = new int[w*h*numBands];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    getPixel(xx, yy, pixel, data);
	    System.arraycopy(pixel, 0, iArray, outOffset, numBands);
	    outOffset += numBands;
	  }
      }
    return iArray;
  }

  /* FIXME: Should it return a banded or pixel interleaved array of
     samples? (Assume interleaved.) */
  public float[] getPixels(int x, int y, int w, int h, float[] fArray,
			   DataBuffer data)
  {
    int size = w*h;
    int outOffset = 0;
    float[] pixel = null;
    if (fArray == null) fArray = new float[w*h*numBands];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    getPixel(xx, yy, pixel, data);
	    System.arraycopy(pixel, 0, fArray, outOffset, numBands);
	    outOffset += numBands;
	  }
      }
    return fArray;
  }
    
  /* FIXME: Should it return a banded or pixel interleaved array of
     samples? (Assume interleaved.) */
  public double[] getPixels(int x, int y, int w, int h, double[] dArray,
			    DataBuffer data)
  {
    int size = w*h;
    int outOffset = 0;
    double[] pixel = null;
    if (dArray == null) dArray = new double[w*h*numBands];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    getPixel(xx, yy, pixel, data);
	    System.arraycopy(pixel, 0, dArray, outOffset, numBands);
	    outOffset += numBands;
	  }
      }
    return dArray;
  }

  public abstract int getSample(int x, int y, int b, DataBuffer data);

  public float getSampleFloat(int x, int y, int b, DataBuffer data)
  {
    return getSample(x, y, b, data);
  }

  public double getSampleDouble(int x, int y, int b, DataBuffer data)
  {
    return getSampleFloat(x, y, b, data);
  }

  public int[] getSamples(int x, int y, int w, int h, int b,
			  int[] iArray, DataBuffer data)
  {
    int size = w*h;
    int outOffset = 0;
    if (iArray == null) iArray = new int[size];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    iArray[outOffset++] = getSample(xx, yy, b, data);
	  }
      }
    return iArray;
  }

  public float[] getSamples(int x, int y, int w, int h, int b,
			    float[] fArray, DataBuffer data)
  {
    int size = w*h;
    int outOffset = 0;
    if (fArray == null) fArray = new float[size];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    fArray[outOffset++] = getSampleFloat(xx, yy, b, data);
	  }
      }
    return fArray;
  }

  public double[] getSamples(int x, int y, int w, int h, int b,
			     double[] dArray, DataBuffer data)
  {
    int size = w*h;
    int outOffset = 0;
    if (dArray == null) dArray = new double[size];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    dArray[outOffset++] = getSampleDouble(xx, yy, b, data);
	  }
      }
    return dArray;
  }
  
  public void setPixel(int x, int y, int[] iArray, DataBuffer data)
  {
    for (int b=0; b<numBands; b++) setSample(x, y, b, iArray[b], data);
  }

  public void setPixel(int x, int y, float[] fArray, DataBuffer data)
  {
    for (int b=0; b<numBands; b++) setSample(x, y, b, fArray[b], data);
  }

  public void setPixel(int x, int y, double[] dArray, DataBuffer data)
  {
    for (int b=0; b<numBands; b++) setSample(x, y, b, dArray[b], data);
  }

  public void setPixels(int x, int y, int w, int h, int[] iArray,
			DataBuffer data)
  {
    int inOffset = 0;
    int[] pixel = new int[numBands];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    System.arraycopy(iArray, inOffset, pixel, 0, numBands);
	    setPixel(xx, yy, pixel, data);
	    inOffset += numBands;
	  }
      }
  }

  public void setPixels(int x, int y, int w, int h, float[] fArray,
			DataBuffer data)
  {
    int inOffset = 0;
    float[] pixel = new float[numBands];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    System.arraycopy(fArray, inOffset, pixel, 0, numBands);
	    setPixel(xx, yy, pixel, data);
	    inOffset += numBands;
	  }
      }
  }

  public void setPixels(int x, int y, int w, int h, double[] dArray,
			DataBuffer data)
  {
    int inOffset = 0;
    double[] pixel = new double[numBands];
    for (int yy=y; yy<(y+h); yy++)
      {
	for (int xx=x; xx<(x+w); xx++)
	  {
	    System.arraycopy(dArray, inOffset, pixel, 0, numBands);
	    setPixel(xx, yy, pixel, data);
	    inOffset += numBands;
	  }
      }
  }

  public abstract void setSample(int x, int y, int b, int s,
				 DataBuffer data);

  public void setSample(int x, int y, int b, float s,
			DataBuffer data)
  {
    setSample(x, y, b, (int) s, data);
  }

  public void setSample(int x, int y, int b, double s,
			DataBuffer data)
  {
    setSample(x, y, b, (float) s, data);
  }

  public void setSamples(int x, int y, int w, int h, int b,
			 int[] iArray, DataBuffer data)
  {
    int size = w*h;
    int inOffset = 0;
    for (int yy=y; yy<(y+h); yy++)
      for (int xx=x; xx<(x+w); xx++)
	setSample(xx, yy, b, iArray[inOffset++], data);
  }

  public void setSamples(int x, int y, int w, int h, int b,
			 float[] fArray, DataBuffer data)
  {
    int size = w*h;
    int inOffset = 0;
    for (int yy=y; yy<(y+h); yy++)
      for (int xx=x; xx<(x+w); xx++)
	setSample(xx, yy, b, fArray[inOffset++], data);

    }

    public void setSamples(int x, int y, int w, int h, int b,
			   double[] dArray, DataBuffer data) {
      int size = w*h;
      int inOffset = 0;
      for (int yy=y; yy<(y+h); yy++)
	for (int xx=x; xx<(x+w); xx++)
	  setSample(xx, yy, b, dArray[inOffset++], data);
    }

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

    public abstract DataBuffer createDataBuffer();

    public abstract int[] getSampleSize();

    public abstract int getSampleSize(int band);
}
