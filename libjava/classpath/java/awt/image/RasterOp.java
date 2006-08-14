/* RasterOp.java --
   Copyright (C) 2000, 2002, 2004, 2005, 2006,  Free Software Foundation

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

import java.awt.RenderingHints;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;

/**
 * An operation that is performed on one raster (the source) producing a new
 * raster (the destination).
 */
public interface RasterOp
{
  /**
   * Performs an operation on the source raster, returning the result in a
   * writable raster.  If <code>dest</code> is <code>null</code>, a new
   * <code>WritableRaster</code> will be created by calling the
   * {@link #createCompatibleDestRaster(Raster)} method.  If <code>dest</code>
   * is not <code>null</code>, the result is written to <code>dest</code> then 
   * returned (this avoids creating a new <code>WritableRaster</code> each 
   * time this method is called).
   * 
   * @param src  the source raster.
   * @param dest  the destination raster (<code>null</code> permitted).
   * 
   * @return The filtered raster.
   */
  WritableRaster filter(Raster src, WritableRaster dest);

  /**
   * Returns the bounds of the destination raster on the basis of this
   * <code>RasterOp</code> being applied to the specified source raster.
   * 
   * @param src  the source raster.
   * 
   * @return The destination bounds.
   */
  Rectangle2D getBounds2D(Raster src);

  /**
   * Returns a raster that can be used by this <code>RasterOp</code> as the
   * destination raster when operating on the specified source raster.
   * 
   * @param src  the source raster.
   * 
   * @return A new writable raster that can be used as the destination raster.
   */
  WritableRaster createCompatibleDestRaster(Raster src);

  /**
   * Returns the point on the destination raster that corresponds to the given
   * point on the source raster.
   * 
   * @param srcPoint  the source point.
   * @param destPoint  the destination point (<code>null</code> permitted).
   * 
   * @return The destination point.
   */
  Point2D getPoint2D(Point2D srcPoint, Point2D destPoint);

  /**
   * Returns the rendering hints for this operation.
   * 
   * @return The rendering hints.
   */
  RenderingHints getRenderingHints();
}

