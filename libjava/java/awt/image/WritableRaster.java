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

import java.awt.Point;
import java.awt.Rectangle;

/**
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class WritableRaster extends Raster
{
  protected WritableRaster(SampleModel sampleModel, Point origin) 
  {
    this(sampleModel, sampleModel.createDataBuffer(), origin);
  }
  
  protected WritableRaster(SampleModel sampleModel,
			   DataBuffer dataBuffer, Point origin)
  {
    this(sampleModel, dataBuffer,
	 new Rectangle(origin != null ? origin.x : 0,
                       origin != null ? origin.y : 0,
		       sampleModel.getWidth(), sampleModel.getHeight()),
	 origin,
	 null);
  }

  protected WritableRaster(SampleModel sampleModel, 
			   DataBuffer dataBuffer,
			   Rectangle aRegion,
			   Point sampleModelTranslate,
			   WritableRaster parent)
  {
    super(sampleModel, dataBuffer, aRegion, sampleModelTranslate,
	  parent);
  }

  public WritableRaster getWritableParent()
  {
    return (WritableRaster) getParent();
  }
  
  public WritableRaster createWritableTranslatedChild(int childMinX,
						      int childMinY)
  {
    // This mirrors the code from the super class
    int tcx = sampleModelTranslateX - minX + childMinX;
    int tcy = sampleModelTranslateY - minY + childMinY;
    
    return new WritableRaster(sampleModel, dataBuffer,
			      new Rectangle(childMinX, childMinY,
					    width, height),
			      new Point(tcx, tcy),
			      this);
  }

  public WritableRaster createWritableChild(int parentX,
					    int parentY,
					    int w, int h,
					    int childMinX,
					    int childMinY,
					    int[] bandList)
  {
    // This mirrors the code from the super class
    
    // FIXME: Throw RasterFormatException if child bounds extends
    // beyond the bounds of this raster.
    
    SampleModel sm = (bandList == null) ?
      sampleModel :
      sampleModel.createSubsetSampleModel(bandList);
    
    return new
      WritableRaster(sm, dataBuffer,
		     new Rectangle(childMinX, childMinY,
				   w, h),
		     new Point(sampleModelTranslateX+childMinX-parentX,
			       sampleModelTranslateY+childMinY-parentY),
		     this);
  }

  public void setDataElements(int x, int y, Object inData)
  {
    sampleModel.setDataElements(x-sampleModelTranslateX,
				y-sampleModelTranslateY,
				inData, dataBuffer);
  }

  public void setDataElements(int x, int y, Raster inRaster)
  {
    Object dataElements = getDataElements(0, 0,
					  inRaster.getWidth(),
					  inRaster.getHeight(),
					  null);
    setDataElements(x, y, dataElements);
  }

  public void setDataElements(int x, int y, int w, int h,
			      Object inData)
  {
    sampleModel.setDataElements(x-sampleModelTranslateX,
				y-sampleModelTranslateY,
				w, h, inData, dataBuffer);
  }

  public void setRect(Raster srcRaster)
  {
    setRect(0, 0, srcRaster);
  }

  public void setRect(int dx, int dy, Raster srcRaster) 
  {
    Rectangle targetUnclipped = new Rectangle(srcRaster.getMinX()+dx,
					      srcRaster.getMinY()+dy,
					      srcRaster.getWidth(),
					      srcRaster.getHeight());
	
    Rectangle target = getBounds().intersection(targetUnclipped);

    if (target.isEmpty()) return;
    
    int sx = target.x - dx;
    int sy = target.y - dy;
    
    // FIXME: Do tests on rasters and use get/set data instead.
    
    /* The JDK documentation seems to imply this implementation.
       (the trucation of higher bits), but an implementation using
       get/setDataElements would be more efficient. None of the
       implementations would do anything sensible when the sample
       models don't match.
       
       But this is probably not the place to consider such
       optimizations.*/

    int[] pixels = srcRaster.getPixels(sx, sy,
				       target.width, target.height,
				       (int[]) null);

    setPixels(target.x, target.y, target.width, target.height, pixels);
  }

  public void setPixel(int x, int y, int[] iArray)
  {
    sampleModel.setPixel(x-sampleModelTranslateX,
			 y-sampleModelTranslateY,
			 iArray, dataBuffer);
  }

  public void setPixel(int x, int y, float[] fArray)
  {
    sampleModel.setPixel(x-sampleModelTranslateX,
			 y-sampleModelTranslateY,
			 fArray, dataBuffer);
  }

  public void setPixel(int x, int y, double[] dArray)
  {
    sampleModel.setPixel(x-sampleModelTranslateX,
			 y-sampleModelTranslateY,
			 dArray, dataBuffer);
  }

  public void setPixels(int x, int y, int w, int h, int[] iArray)
  {
    sampleModel.setPixels(x-sampleModelTranslateX,
			  y-sampleModelTranslateY,
			  w, h, iArray, dataBuffer);
  }

  public void setPixels(int x, int y, int w, int h, float[] fArray)
  {
    sampleModel.setPixels(x-sampleModelTranslateX,
			  y-sampleModelTranslateY,
			  w, h, fArray, dataBuffer);
  }

  public void setPixels(int x, int y, int w, int h, double[] dArray)
  {
    sampleModel.setPixels(x-sampleModelTranslateX,
			  y-sampleModelTranslateY,
			  w, h, dArray, dataBuffer);
  }

  public void setSample(int x, int y, int b, int s)
  {
    sampleModel.setSample(x-sampleModelTranslateX,
			  y-sampleModelTranslateY,
			  b, s, dataBuffer);
  }

  public void setSample(int x, int y, int b, float s)
  {
    sampleModel.setSample(x-sampleModelTranslateX,
			  y-sampleModelTranslateY,
			  b, s, dataBuffer);
  }

  public void setSample(int x, int y, int b, double s)
  {
    sampleModel.setSample(x-sampleModelTranslateX,
			  y-sampleModelTranslateY,
			  b, s, dataBuffer);
  }

  public void setSamples(int x, int y, int w, int h, int b,
			 int[] iArray)
  {
    sampleModel.setSamples(x-sampleModelTranslateX,
			   y-sampleModelTranslateY,
			   w, h, b, iArray, dataBuffer);
  }

  public void setSamples(int x, int y, int w, int h, int b,
			 float[] fArray)
  {
    sampleModel.setSamples(x-sampleModelTranslateX,
			   y-sampleModelTranslateY,
			   w, h, b, fArray, dataBuffer);
  }

  public void setSamples(int x, int y, int w, int h, int b,
			 double[] dArray)
  {
    sampleModel.setSamples(x-sampleModelTranslateX,
			   y-sampleModelTranslateY,
			   w, h, b, dArray, dataBuffer);
  }
}
