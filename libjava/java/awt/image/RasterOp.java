/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.awt.image;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.RenderingHints;

public interface RasterOp {

  WritableRaster filter(Raster src, WritableRaster dest);

  Rectangle2D getBounds2D(Raster src);

  WritableRaster createCompatibleDestRaster(Raster src);

  Point2D getPoint2D(Point2D srcPoint, Point2D destPoint);

  public RenderingHints getRenderingHints();
}

