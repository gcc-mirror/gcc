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

import java.awt.*;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class Raster
{
  protected SampleModel sampleModel;
  protected DataBuffer dataBuffer;
  protected int minX;
  protected int minY;
  protected int width;
  protected int height;
  protected int sampleModelTranslateX;
  protected int sampleModelTranslateY;
  protected int numBands;
  protected int numDataElements;
  protected Raster parent;
  
  protected Raster(SampleModel sampleModel, Point origin)
  {
    this(sampleModel, sampleModel.createDataBuffer(), origin);
  }
  
  protected Raster(SampleModel sampleModel, DataBuffer dataBuffer,
		   Point origin)
  {
    this(sampleModel, dataBuffer,
	 new Rectangle(origin.x, origin.y,
		       sampleModel.getWidth(), sampleModel.getHeight()),
	 origin, null);
  }

  protected Raster(SampleModel sampleModel, DataBuffer dataBuffer,
		   Rectangle aRegion,
		   Point sampleModelTranslate, Raster parent)
  {
    this.sampleModel = sampleModel;
    this.dataBuffer = dataBuffer;
    this.minX = aRegion.x;
    this.minY = aRegion.y;
    this.width = aRegion.width;
    this.height = aRegion.height;
    this.sampleModelTranslateX = sampleModelTranslate.x;
    this.sampleModelTranslateY = sampleModelTranslate.y;
    this.numBands = sampleModel.getNumBands();
    this.numDataElements = sampleModel.getNumDataElements();
    this.parent = parent;
  }
    
  public static WritableRaster createInterleavedRaster(int dataType,
						       int w, int h,
						       int bands, 
						       Point location)
  {
    int[] bandOffsets = new int[bands];
    // TODO: Maybe not generate this every time.
    for (int b=0; b<bands; b++) bandOffsets[b] = b;
    
    int scanlineStride = bands*w;
    return createInterleavedRaster(dataType, w, h, scanlineStride, bands,
				   bandOffsets, location);
  }

  public static WritableRaster createInterleavedRaster(int dataType,
						       int w, int h,
						       int scanlineStride,
						       int pixelStride,
						       int[] bandOffsets,
						       Point location)
  {
    SampleModel sm = new ComponentSampleModel(dataType,
					      w, h,
					      pixelStride,
					      scanlineStride,
					      bandOffsets);
    return createWritableRaster(sm, location);
  }

  public static WritableRaster createBandedRaster(int dataType, 
						  int w, int h, int bands,
						  Point location)
  {
    // FIXME: Implement;
    throw new UnsupportedOperationException("not implemented yet");
  }

  public static WritableRaster createBandedRaster(int dataType,
						  int w, int h,
						  int scanlineStride,
						  int[] bankIndices,
						  int[] bandOffsets,
						  Point location)
  {
    // FIXME: Implement;
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public static WritableRaster createPackedRaster(int dataType,
						  int w, int h,
						  int[] bandMasks,
						  Point location)
  {
    SampleModel sm = new SinglePixelPackedSampleModel(dataType,
						      w, h,
						      bandMasks);
    return createWritableRaster(sm, location);
  }

  public static WritableRaster
  createInterleavedRaster(DataBuffer dataBuffer, int w, int h,
			  int scanlineStride, int pixelStride,
			  int[] bandOffsets, Point location)
  {
    SampleModel sm = new ComponentSampleModel(dataBuffer.getDataType(),
					      w, h,
					      scanlineStride,
					      pixelStride,
					      bandOffsets);
    return createWritableRaster(sm, dataBuffer, location);
  }

  public static
  WritableRaster createBandedRaster(DataBuffer dataBuffer,
				    int w, int h,
				    int scanlineStride,
				    int[] bankIndices,
				    int[] bandOffsets,
				    Point location)
  {
    // FIXME: Implement;
    throw new UnsupportedOperationException("not implemented yet");
  }
  
  public static WritableRaster
  createPackedRaster(DataBuffer dataBuffer,
		     int w, int h,
		     int scanlineStride,
		     int[] bandMasks,
		     Point location) {
    SampleModel sm =
      new SinglePixelPackedSampleModel(dataBuffer.getDataType(),
				       w, h,
				       scanlineStride,
				       bandMasks);
    return createWritableRaster(sm, dataBuffer, location);
  }
    
  public static Raster createRaster(SampleModel sm, DataBuffer db,
				    Point location)
  {
    return new Raster(sm, db, location);
  }

  public static WritableRaster createWritableRaster(SampleModel sm,
						    Point location)
  {
    return new WritableRaster(sm, location);
  }

  public static WritableRaster createWritableRaster(SampleModel sm,
						    DataBuffer db,
						    Point location)
  {
    return new WritableRaster(sm, db, location);
  }

  public Raster getParent()
  {
    return parent;
  }

  public final int getSampleModelTranslateX()
  {
    return sampleModelTranslateX;
  }

  public final int getSampleModelTranslateY()
  {
    return sampleModelTranslateY;
  }

  public WritableRaster createCompatibleWritableRaster()
  {
    return new WritableRaster(getSampleModel(), new Point(minX, minY));
  }

  public WritableRaster createCompatibleWritableRaster(int w, int h)
  {
    return createCompatibleWritableRaster(minX, minY, w, h);
  }

  public WritableRaster createCompatibleWritableRaster(Rectangle rect)
  {
    return createCompatibleWritableRaster(rect.x, rect.y,
					  rect.width, rect.height);
  }

  public WritableRaster createCompatibleWritableRaster(int x, int y,
						       int w, int h)
  {
    SampleModel sm = getSampleModel().createCompatibleSampleModel(w, h);
    return new WritableRaster(sm, sm.createDataBuffer(),
			      new Point(x, y));
  }

  public Raster createTranslatedChild(int childMinX, int childMinY) {
    int tcx = sampleModelTranslateX - minX + childMinX;
    int tcy = sampleModelTranslateY - minY + childMinY;
    
    return new Raster(sampleModel, dataBuffer,
		      new Rectangle(childMinX, childMinY,
				    width, height),
		      new Point(tcx, tcy),
		      this);
  }

  public Raster createChild(int parentX, int parentY, int width,
			    int height, int childMinX, int childMinY,
			    int[] bandList)
  {
    /* FIXME: Throw RasterFormatException if child bounds extends
       beyond the bounds of this raster. */

    SampleModel sm = (bandList == null) ?
      sampleModel :
      sampleModel.createSubsetSampleModel(bandList);

    /*
        data origin
       /
      +-------------------------
      |\. __ parent trans
      | \`.  
      |  \ `.    parent origin
      |   \  `. /
      |   /\   +-------- - -
      |trans\ /<\-- deltaTrans
      |child +-+-\---- - - 
      |     /|`|  \__ parent [x, y]
      |child | |`. \
      |origin| :  `.\
      |      |    / `\
      |      :   /    +
      | child [x, y] 

      parent_xy - parent_trans = child_xy - child_trans

      child_trans = parent_trans + child_xy - parent_xy
    */

    return new Raster(sm, dataBuffer,
		      new Rectangle(childMinX, childMinY,
				    width, height),
		      new Point(sampleModelTranslateX+childMinX-parentX,
				sampleModelTranslateY+childMinY-parentY),
		      this);
  }

  public Rectangle getBounds()
  {
    return new Rectangle(minX, minY, width, height);
  }

  public final int getMinX()
  {
    return minX;
  }

  public final int getMinY()
  {
    return minY;
  }

  public final int getWidth()
  {
    return width;
  }

  public final int getHeight()
  {
    return height;
  }

  public final int getNumDataElements()
  {
    return numDataElements;
  }
    
  public final int getTransferType()
  {
    return sampleModel.getTransferType();
  }

  public DataBuffer getDataBuffer()
  {
    return dataBuffer;
  }

  public SampleModel getSampleModel()
  {
    return sampleModel;
  }

  public Object getDataElements(int x, int y, Object outData)
  {
    return sampleModel.getDataElements(x-sampleModelTranslateX,
				       y-sampleModelTranslateY,
				       outData, dataBuffer);
  }

  public Object getDataElements(int x, int y, int w, int h,
				Object outData)
  {
    return sampleModel.getDataElements(x-sampleModelTranslateX,
				       y-sampleModelTranslateY,
				       w, h, outData, dataBuffer);
  }

  public int[] getPixel(int x, int y, int[] iArray)
  {
    return sampleModel.getPixel(x-sampleModelTranslateX,
				y-sampleModelTranslateY,
				iArray, dataBuffer);
  }

  public float[] getPixel(int x, int y, float[] fArray)
  {
    return sampleModel.getPixel(x-sampleModelTranslateX,
				y-sampleModelTranslateY,
				fArray, dataBuffer);
  }

  public double[] getPixel(int x, int y, double[] dArray)
  {
    return sampleModel.getPixel(x-sampleModelTranslateX,
				y-sampleModelTranslateY,
				dArray, dataBuffer);
  }

  public int[] getPixels(int x, int y, int w, int h, int[] iArray)
  {
    return sampleModel.getPixels(x-sampleModelTranslateX,
				 y-sampleModelTranslateY,
				 w, h, iArray, dataBuffer);
  }

  public float[] getPixels(int x, int y, int w, int h,
			   float[] fArray)
  {
    return sampleModel.getPixels(x-sampleModelTranslateX,
				 y-sampleModelTranslateY,
				 w, h, fArray, dataBuffer);
  }

  public double[] getPixels(int x, int y, int w, int h,
			    double[] dArray)
  {
    return sampleModel.getPixels(x-sampleModelTranslateX,
				 y-sampleModelTranslateY,
				 w, h, dArray, dataBuffer);
  }

  public int getSample(int x, int y, int b)
  {
    return sampleModel.getSample(x-sampleModelTranslateX,
				 y-sampleModelTranslateY,
				 b, dataBuffer);
  }

  public float getSampleFloat(int x, int y, int b)
  {
    return sampleModel.getSampleFloat(x-sampleModelTranslateX,
				      y-sampleModelTranslateY,
				      b, dataBuffer);
  }

  public double getSampleDouble(int x, int y, int b)
  {
    return sampleModel.getSampleDouble(x-sampleModelTranslateX,
				       y-sampleModelTranslateY,
				       b, dataBuffer);
  }

  public int[] getSamples(int x, int y, int w, int h, int b,
			  int[] iArray)
  {
    return sampleModel.getSamples(x-sampleModelTranslateX,
				  y-sampleModelTranslateY,
				  w, h, b, iArray, dataBuffer);
  }

  public float[] getSamples(int x, int y, int w, int h, int b,
			    float[] fArray)
  {
    return sampleModel.getSamples(x-sampleModelTranslateX,
				  y-sampleModelTranslateY,
				  w, h, b, fArray, dataBuffer);
  }

  public double[] getSamples(int x, int y, int w, int h, int b,
			     double[] dArray)
  {
    return sampleModel.getSamples(x-sampleModelTranslateX,
				  y-sampleModelTranslateY,
				  w, h, b, dArray, dataBuffer);
  }
}
