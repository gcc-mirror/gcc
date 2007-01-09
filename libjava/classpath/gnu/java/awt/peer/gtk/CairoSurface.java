/* CairoSurface.java
   Copyright (C) 2006 Free Software Foundation, Inc.

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
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.WritableRaster;
import java.nio.ByteOrder;
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
   * The native pointer to the image's data buffer
   */
  long bufferPointer;

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
  private native void create(int width, int height, int stride);

  /**
   * Destroys the cairo surface and frees the buffer.
   */
  private native void destroy(long surfacePointer, long bufferPointer);

  /**
   * Gets buffer elements
   */
  private native int nativeGetElem(long bufferPointer, int i);
  
  /**
   * Sets buffer elements.
   */
  private native void nativeSetElem(long bufferPointer, int i, int val);

  /**
   * Draws this image to a given CairoGraphics context, 
   * with an affine transform given by i2u.
   */
  public native void nativeDrawSurface(long surfacePointer, long contextPointer,
                                       double[] i2u, double alpha,
                                       int interpolation);

  public void drawSurface(long contextPointer, double[] i2u, double alpha,
                          int interpolation)
  {
    nativeDrawSurface(surfacePointer, contextPointer, i2u, alpha, interpolation);
  }

  /**
   * getPixels -return the pixels as a java array.
   */
  native int[] nativeGetPixels(long bufferPointer, int size);

  public int[] getPixels(int size)
  {
    return nativeGetPixels(bufferPointer, size);
  }

  /**
   * getPixels -return the pixels as a java array.
   */
  native void nativeSetPixels(long bufferPointer, int[] pixels);

  public void setPixels(int[] pixels)
  {
    nativeSetPixels(bufferPointer, pixels);
  }

  native long getFlippedBuffer(long bufferPointer, int size);

  /**
   * Create a cairo_surface_t with specified width and height.
   * The format will be ARGB32 with premultiplied alpha and native bit 
   * and word ordering.
   */
  public CairoSurface(int width, int height)
  {
    super(createCairoSampleModel(width, height),
	      null, new Point(0, 0));

    if(width <= 0 || height <= 0)
      throw new IllegalArgumentException("Image must be at least 1x1 pixels.");
    
    this.width = width;
    this.height = height;
    create(width, height, width);

    if(surfacePointer == 0 || bufferPointer == 0)
      throw new Error("Could not allocate bitmap.");

    dataBuffer = new CairoDataBuffer();
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

    setPixels( data );
  }

  /**
   * Dispose of the native data.
   */
  public void dispose()
  {
    if(surfacePointer != 0)
      destroy(surfacePointer, bufferPointer);
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
    return new GtkImage( width, height,
                         getFlippedBuffer(bufferPointer, width * height ));
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

  private class CairoDataBuffer extends DataBuffer
  {
    public CairoDataBuffer()
    {
      super(DataBuffer.TYPE_INT, width * height);
    }

    /**
     * DataBuffer.getElem implementation
     */
    public int getElem(int bank, int i)
    {
      if(bank != 0 || i < 0 || i >= width * height)
	throw new IndexOutOfBoundsException(i+" size: "+width * height);
      return nativeGetElem(bufferPointer, i);
    }
  
    /**
     * DataBuffer.setElem implementation
     */
    public void setElem(int bank, int i, int val)
    {
      if(bank != 0 || i < 0 || i >= width*height)
	throw new IndexOutOfBoundsException(i+" size: "+width * height);
      nativeSetElem(bufferPointer, i, val);
    }
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
   * Copy an area of the surface. Expects parameters must be within bounds. 
   * Count on a segfault otherwise.
   */
  native void copyAreaNative2(long bufferPointer, int x, int y, int width,
                             int height, int dx, int dy, int stride);
  public void copyAreaNative(int x, int y, int width,
                             int height, int dx, int dy, int stride)
  {
    copyAreaNative2(bufferPointer, x, y, width, height, dx, dy, stride);
  }
  
  /**
   * Creates a SampleModel that matches Cairo's native format
   */
  protected static SampleModel createCairoSampleModel(int w, int h)
  {
    return new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT, w, h,
                                            new int[]{0x00FF0000, 0x0000FF00,
                                                      0x000000FF, 0xFF000000});    
  }
}
