/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.j2d;

import java.awt.image.WritableRaster;
import java.awt.image.ColorModel;

/* The raster and associated properties of a mapped screen region.
 * The compositing capabilities of backends are often insufficient.
 * The backend may not support alpha blending, or may not support some
 * other special compositing rule.  This means that compositing must
 * sometimes be done within the rendering pipeline.  The general
 * compositing operation consists of combining new color and alpha
 * values with existing color values on the drawing surface, to find
 * the new color values for the drawing surface. The way the values
 * are combined, determines what kind of compositing operation that is
 * performed.  The default compositing operation is alpha compositing.
 *
 * <p>In order to perform alpha compositing and other compositing
 * operations, we need access to the color values of the imagery that
 * has already been drawn on the drawing surface.  The
 * DirectRasterGraphics interface must therefore contain methods that
 * makes it possible to gain access to the pixel values of the drawing
 * surface.  The methods are modeled after the POSIX mmap() and
 * munmap() functions.  But, instead of mapping and unmapping portions
 * of data from a file descriptor to memory, the methods in
 * DirectRasterGraphics maps and unmaps portions of the drawing
 * surface to data arrays within writable raster objects.  A call to
 * mapRaster() will return a writable raster object, encapsulating the
 * image data of the drawing surface in the requested domain. The data
 * encapsulated by this raster object can be modified using the
 * WritableRaster API, or the data buffers can be retrieved from the
 * raster, so that the data arrays can be manipulated directly.  When
 * the raster image has been modified as desired, the data can be
 * resynchronized with the drawing surface by calling mapRaster().
 *
 * <p>As with mmap() and munmap() the methods may work by direct
 * manipulation of shared memory, (i.e. the raster object directly
 * wraps the actual image data of the drawing surface), or may make a
 * private copy that is resynched when the raster is unmapped. The
 * backend may choose to implement either mechanism, and the pipeline
 * code should not care what mechanism is actually used.  This design
 * allows us to make full use of speedups such as X shared memory
 * extentions when available.
 */
public class MappedRaster
{
  WritableRaster raster;
  ColorModel cm;
  
  public MappedRaster(WritableRaster raster, ColorModel cm)
  {
    this.raster = raster;
    this.cm = cm;
  }

  public final WritableRaster getRaster()
  {
    return raster;
  }

  public final ColorModel getColorModel()
  {
    return cm;
  }
}
