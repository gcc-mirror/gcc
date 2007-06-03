/* CairoSurface.java
   Copyright (C) 2006, 2007 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import gnu.java.awt.Buffers;

import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.color.ColorSpace;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.awt.image.DirectColorModel;
import java.awt.image.Raster;
import java.awt.image.RasterFormatException;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.nio.ByteOrder;
import java.util.Arrays;
import java.util.Hashtable;

/**
 * CairoSurface - wraps a Cairo surface.
 *
 * @author Sven de Marothy
 */
public class CairoSurface extends WritableRaster
{
  int width = -1, height = -1;

  /**
   * The native pointer to the Cairo surface. 
   */
  long surfacePointer;

  /**
   * Whether the data buffer is shared between java and cairo.
   */
  boolean sharedBuffer;

  // FIXME: use only the cairoCM_pre colormodel
  // since that's what Cairo really uses (is there a way to do this cheaply?
  // we use a non-multiplied model most of the time to avoid costly coercion
  // operations...)
  static ColorModel cairoColorModel = new DirectColorModel(32, 0x00FF0000,
                                                           0x0000FF00,
                                                           0x000000FF,
                                                           0xFF000000);

  static ColorModel cairoCM_pre = new DirectColorModel(ColorSpace.getInstance(ColorSpace.CS_sRGB),
                                                       32, 0x00FF0000,
                                                       0x0000FF00,
                                                       0x000000FF,
                                                       0xFF000000,
                                                       true,
                                                       Buffers.smallestAppropriateTransferType(32));
  
  // This CM corresponds to the CAIRO_FORMAT_RGB24 type in Cairo 
  static ColorModel cairoCM_opaque = new DirectColorModel(24, 0x00FF0000,
                                                          0x0000FF00,
                                                          0x000000FF);
  /**
   * Allocates and clears the buffer and creates the cairo surface.
   * @param width, height - the image size
   * @param stride - the buffer row stride. (in ints)
   */
  private native void create(int width, int height, int stride, int[] buf);

  /**
   * Destroys the cairo surface and frees the buffer.
   */
  private native void destroy(long surfacePointer, int[] buf);

  /**
   * Draws this image to a given CairoGraphics context, 
   * with an affine transform given by i2u.
   */
  public native void nativeDrawSurface(long surfacePointer, long contextPointer,
                                       double[] i2u, double alpha,
                                       int interpolation);

  /**
   * Synchronizes the image's data buffers, copying any changes made in the
   * Java array into the native array.
   * 
   * This method should only be called if (sharedBuffers == false).
   */
  native void syncNativeToJava(long surfacePointer, int[] buffer);
  
  /**
   * Synchronizes the image's data buffers, copying any changes made in the
   * native array into the Java array.
   * 
   * This method should only be called if (sharedBuffers == false).
   */
  native void syncJavaToNative(long surfacePointer, int[] buffer);
  
  /**
   * Return the buffer, with the sample values of each pixel reversed
   * (ie, in ABGR instead of ARGB). 
   * 
   * @return A pointer to a flipped buffer.  The memory is allocated in native
   *        code, and must be explicitly freed when it is no longer needed.
   */
  native long getFlippedBuffer(long surfacePointer);

  /**
   * Create a cairo_surface_t with specified width and height.
   * The format will be ARGB32 with premultiplied alpha and native bit 
   * and word ordering.
   */
  public CairoSurface(int width, int height)
  {
    this(0, 0, width, height);
  }
  
  public CairoSurface(int x, int y, int width, int height)
  {
    super(createCairoSampleModel(width, height), null, new Point(x, y));

    if(width <= 0 || height <= 0)
      throw new IllegalArgumentException("Image must be at least 1x1 pixels.");
    
    this.width = width;
    this.height = height;
    dataBuffer = new DataBufferInt(width * height);
    create(width, height, width, getData());

    if(surfacePointer == 0)
      throw new Error("Could not allocate bitmap.");
  }
  
  /**
   * Create a Cairo Surface that is a subimage of another Cairo Surface
   */
  public CairoSurface(SampleModel sm, CairoSurface parent, Rectangle bounds,
                      Point origin)
  {
    super(sm, parent.dataBuffer, bounds, origin, parent);
    
    this.width = super.width;
    this.height = super.height;
    this.surfacePointer = parent.surfacePointer;
    this.sharedBuffer = parent.sharedBuffer;
    this.dataBuffer = parent.dataBuffer;
  }

  /**
   * Create a cairo_surface_t from a GtkImage instance.
   * (data is copied, not shared)
   */
  CairoSurface(GtkImage image)
  {
    this(image.width, image.height);

    // Copy the pixel data from the GtkImage.
    int[] data = image.getPixels();

    // Swap ordering from GdkPixbuf to Cairo
    if (ByteOrder.nativeOrder() == ByteOrder.BIG_ENDIAN)
      {
        for (int i = 0; i < data.length; i++ )
          {
            // On a big endian system we get a RRGGBBAA data array.
            int alpha = data[i] & 0xFF;
            if( alpha == 0 ) // I do not know why we need this, but it works.
              data[i] = 0;
            else
              {
                // Cairo needs a ARGB32 native array.
                data[i] = (data[i] >>> 8) | (alpha << 24);
              }
          }
      }
    else
      {
        for (int i = 0; i < data.length; i++ )
          {
            // On a little endian system we get a AABBGGRR data array.
            int alpha = data[i] & 0xFF000000;
            if( alpha == 0 ) // I do not know why we need this, but it works.
              data[i] = 0;
            else
              {
                int b = (data[i] & 0xFF0000) >> 16;
                int g = (data[i] & 0xFF00);
                int r = (data[i] & 0xFF) << 16;
                // Cairo needs a ARGB32 native array.
                data[i] = alpha | r | g | b;
              }
          }
      }

    System.arraycopy(data, 0, getData(), 0, data.length);
  }

  /**
   * Dispose of the native data.
   */
  public void dispose()
  {
    if(surfacePointer != 0 && parent == null)
      destroy(surfacePointer, getData());
  }

  /**
   * Call dispose() to clean up any native resources allocated.
   */
  protected void finalize()
  {
    dispose();
  }

  /**
   * Return a GtkImage from this Cairo surface.
   */
  public GtkImage getGtkImage()
  {
    return new GtkImage(width, height, getFlippedBuffer(surfacePointer));
  }
  
  /**
   * Convenience method to quickly grab the data array backing this Raster.
   * 
   * @return The array behind the databuffer.
   */
  public int[] getData()
  {
    return ((DataBufferInt)dataBuffer).getData();
  }

  /**
   * Returns a BufferedImage backed by a Cairo surface.
   */    
  public static BufferedImage getBufferedImage(int width, int height)
  {
    return getBufferedImage(new CairoSurface(width, height));
  }

  /**
   * Returns a BufferedImage backed by a Cairo surface, 
   * created from a GtkImage.
   */    
  public static BufferedImage getBufferedImage(GtkImage image)
  {
    return getBufferedImage(new CairoSurface(image));
  }

  /**
   * Returns a BufferedImage backed by a Cairo surface.
   */    
  public static BufferedImage getBufferedImage(CairoSurface surface)
  {
    return new BufferedImage(cairoColorModel, surface,
                             cairoColorModel.isAlphaPremultiplied(),
                             new Hashtable());
  }

  /**
   * Return a Graphics2D drawing to the CairoSurface.
   */
  public Graphics2D getGraphics()
  {
    return new CairoSurfaceGraphics(this);
  } 

  ///// Methods used by CairoSurfaceGraphics /////
  /**
   * Creates a cairo_t drawing context, returns the pointer as a long.
   * Used by CairoSurfaceGraphics.
   */
  native long nativeNewCairoContext(long surfacePointer);

  public long newCairoContext()
  {
    return nativeNewCairoContext(surfacePointer);
  }

  /**
   * Copy a portion of this surface to another area on the surface.  The given
   * parameters must be within bounds - count on a segfault otherwise.
   * 
   * @param x The x coordinate of the area to be copied from.
   * @param y The y coordinate of the area to be copied from.
   * @param width The width of the area to be copied.
   * @param height The height of the area to be copied.
   * @param dx The destination x coordinate.
   * @param dy The destination y coordinate.
   * @param stride The scanline stride.
   */
  public void copyAreaNative(int x, int y, int width,
                             int height, int dx, int dy, int stride)
  {
    copyAreaNative2(surfacePointer, x, y, width, height, dx, dy, stride);
  }
  native void copyAreaNative2(long surfacePointer,
                              int x, int y, int width, int height,
                              int dx, int dy, int stride);
  
  /**
   * Creates a SampleModel that matches Cairo's native format
   */
  protected static SampleModel createCairoSampleModel(int w, int h)
  {
    return new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT, w, h,
                                            new int[]{0x00FF0000, 0x0000FF00,
                                                      0x000000FF, 0xFF000000});    
  }
  
  /**
   * Returns whether this ColorModel is compatible with Cairo's native types.
   * 
   * @param cm The color model to check.
   * @return Whether it is compatible.
   */
  public static boolean isCompatibleColorModel(ColorModel cm)
  {
    return (cm.equals(cairoCM_pre) || cm.equals(cairoCM_opaque) ||
            cm.equals(cairoColorModel));
  }
  
  /**
   * Returns whether this SampleModel is compatible with Cairo's native types.
   * 
   * @param sm The sample model to check.
   * @return Whether it is compatible.
   */
  public static boolean isCompatibleSampleModel(SampleModel sm)
  {
    return (sm instanceof SinglePixelPackedSampleModel
        && sm.getDataType() == DataBuffer.TYPE_INT
        && Arrays.equals(((SinglePixelPackedSampleModel)sm).getBitMasks(),
                         new int[]{0x00FF0000, 0x0000FF00,
                                   0x000000FF, 0xFF000000}));
  }

  ///// Methods interhited from Raster and WritableRaster /////
  public Raster createChild(int parentX, int parentY, int width, int height,
                            int childMinX, int childMinY, int[] bandList)
  {
    return createWritableChild(parentX, parentY, width, height,
                               childMinX, childMinY, bandList);
  }
  
  public WritableRaster createCompatibleWritableRaster()
  {
    return new CairoSurface(width, height);
  }
  
  public WritableRaster createCompatibleWritableRaster (int x, int y,
                                                        int w, int h)
  {
    return new CairoSurface(x, y, w, h);
  }
  
  public Raster createTranslatedChild(int childMinX, int childMinY)
  {
    return createWritableTranslatedChild(childMinX, childMinY);
  }
  
  public WritableRaster createWritableChild(int parentX, int parentY,
                                            int w, int h, int childMinX,
                                            int childMinY, int[] bandList)
  {
    if (parentX < minX || parentX + w > minX + width
        || parentY < minY || parentY + h > minY + height)
      throw new RasterFormatException("Child raster extends beyond parent");
    
    SampleModel sm = (bandList == null) ?
      sampleModel :
      sampleModel.createSubsetSampleModel(bandList);

    return new CairoSurface(sm, this,
                            new Rectangle(childMinX, childMinY, w, h),
                            new Point(sampleModelTranslateX + childMinX - parentX,
                                      sampleModelTranslateY + childMinY - parentY));
  }
  
  public WritableRaster createWritableTranslatedChild(int x, int y)
  {
    int tcx = sampleModelTranslateX - minX + x;
    int tcy = sampleModelTranslateY - minY + y;
    
    return new CairoSurface(sampleModel, this,
                      new Rectangle(x, y, width, height),
                      new Point(tcx, tcy));
  }
}
