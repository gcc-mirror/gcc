/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.awt;

import java.awt.geom.*;
import java.awt.image.*;
import java.awt.RenderingHints;

/**
 * This raster copy operation assumes that both source and destination
 * sample models are tightly pixel packed and contain the same number
 * of bands.
 *
 * @throws java.lang.ClassCastException if the sample models of the
 * rasters are not of type ComponentSampleModel.
 * 
 * @author Rolf W. Rasmussen <rolfwr@ii.uib.no>
 */
public class ComponentDataBlitOp implements RasterOp
{
  public static ComponentDataBlitOp INSTANCE = new ComponentDataBlitOp();

  public WritableRaster filter(Raster src, WritableRaster dest)
  {
    if (dest == null)
      dest = createCompatibleDestRaster(src);
    
    DataBuffer  srcDB =  src.getDataBuffer();
    DataBuffer destDB = dest.getDataBuffer();
    
    ComponentSampleModel  srcSM = (ComponentSampleModel)  src.getSampleModel();
    ComponentSampleModel destSM = (ComponentSampleModel) dest.getSampleModel();

    
    // Calculate offset to data in the underlying arrays:

    int  srcScanlineStride =  srcSM.getScanlineStride();
    int destScanlineStride = destSM.getScanlineStride();
    int srcX  =  src.getMinX() -  src.getSampleModelTranslateX();
    int srcY  =  src.getMinY() -  src.getSampleModelTranslateY();
    int destX = dest.getMinX() - dest.getSampleModelTranslateX();
    int destY = dest.getMinY() - dest.getSampleModelTranslateY();

    int numBands = srcSM.getNumBands();

    /* We can't use getOffset(x, y) from the sample model since we
       don't want the band offset added in. */
	
    int srcOffset = 
      numBands*srcX + srcScanlineStride*srcY +    // from sample model
      srcDB.getOffset();                          // from data buffer

    int destOffset =
      numBands*destX + destScanlineStride*destY + // from sample model
      destDB.getOffset();                         // from data buffer

    // Determine how much, and how many times to blit.
    
    int rowSize = src.getWidth()*numBands;
    int h = src.getHeight();
    
    if ((rowSize == srcScanlineStride) &&
	(rowSize == destScanlineStride))
      {
	// collapse scan line blits to one large blit.
	rowSize *= h;
	h = 1;
      }

	
    // Do blitting
    
    Object srcArray  = Buffers.getData(srcDB);
    Object destArray = Buffers.getData(destDB);
    
    for (int yd = 0; yd<h; yd++)
      {
	System.arraycopy(srcArray, srcOffset, 
			 destArray, destOffset,
			 rowSize);
	srcOffset  +=  srcScanlineStride;
	destOffset += destScanlineStride;
      }
    

    return dest;
  }

  public Rectangle2D getBounds2D(Raster src) 
  {
    return src.getBounds();
  }

  public WritableRaster createCompatibleDestRaster(Raster src) {
    
    /* FIXME: Maybe we should explicitly create a raster with a
       tightly pixel packed sample model, rather than assuming
       that the createCompatibleWritableRaster() method in Raster
       will create one. */

    return src.createCompatibleWritableRaster();
  }

  public Point2D getPoint2D(Point2D srcPoint, Point2D destPoint) 
  {
    if (destPoint == null)
      return (Point2D) srcPoint.clone();

    destPoint.setLocation(srcPoint);
    return destPoint;
  }

  public RenderingHints getRenderingHints() 
  {
    throw new UnsupportedOperationException("not implemented");
  }
}
