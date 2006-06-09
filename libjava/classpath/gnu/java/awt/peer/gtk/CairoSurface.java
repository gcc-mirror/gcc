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

import java.awt.Graphics;
import java.awt.Color;
import java.awt.Image;
import java.awt.Point;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Vector;
import java.io.ByteArrayOutputStream;
import java.io.BufferedInputStream;
import java.net.URL;
import gnu.classpath.Pointer;

/**
 * CairoSurface - wraps a Cairo surface.
 *
 * @author Sven de Marothy
 */
public class CairoSurface extends DataBuffer
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


  static ColorModel nativeModel = new DirectColorModel(32, 
						       0x000000FF,
						       0x0000FF00,
						       0x00FF0000,
						       0xFF000000);

  /**
   * Allocates and clears the buffer and creates the cairo surface.
   * @param width, height - the image size
   * @param stride - the buffer row stride.
   */
  private native void create(int width, int height, int stride);

  /**
   * Destroys the cairo surface and frees the buffer.
   */
  private native void destroy();

  /**
   * Gets buffer elements
   */
  private native int nativeGetElem(int i);
  
  /**
   * Sets buffer elements.
   */
  private native void nativeSetElem(int i, int val);

  /**
   * Draws this image to a given CairoGraphics context, 
   * with an affine transform given by i2u.
   */
  public native void drawSurface(CairoGraphics2D context, double[] i2u);

  /**
   * getPixels -return the pixels as a java array.
   */
  native int[] getPixels(int size);

  /**
   * getPixels -return the pixels as a java array.
   */
  native void setPixels(int[] pixels);

  native long getFlippedBuffer(int size);

  /**
   * Create a cairo_surface_t with specified width and height.
   * The format will be ARGB32 with premultiplied alpha and native bit 
   * and word ordering.
   */
  CairoSurface(int width, int height)
  {
    super(DataBuffer.TYPE_INT, width * height);

    if(width <= 0 || height <= 0)
      throw new IllegalArgumentException("Image must be at least 1x1 pixels.");

    this.width = width;
    this.height = height;

    create(width, height, width * 4);

    if(surfacePointer == 0 || bufferPointer == 0)
      throw new Error("Could not allocate bitmap.");
  }

  /**
   * Create a cairo_surface_t from a GtkImage instance.
   * (data is copied, not shared)
   */
  CairoSurface(GtkImage image)
  {
    super(DataBuffer.TYPE_INT, image.width * image.height);

    if(image.width <= 0 || image.height <= 0)
      throw new IllegalArgumentException("Image must be at least 1x1 pixels.");

    width = image.width;
    height = image.height;

    create(width, height, width * 4);
    
    if(surfacePointer == 0 || bufferPointer == 0)
      throw new Error("Could not allocate bitmap.");
    
    // Copy the pixel data from the GtkImage.
    int[] data = image.getPixels();

    // Swap ordering from GdkPixbuf to Cairo
    for(int i = 0; i < data.length; i++ )
      {
	int alpha = (data[i] & 0xFF000000) >> 24;
	if( alpha == 0 ) // I do not know why we need this, but it works.
	  data[i] = 0;
	else
	  {
	    int r = (((data[i] & 0x00FF0000) >> 16) );
	    int g = (((data[i] & 0x0000FF00) >> 8) );
	    int b = ((data[i] & 0x000000FF) );
	    data[i] = (( alpha << 24 ) & 0xFF000000) 
	      | (( b << 16 ) & 0x00FF0000)
	      | (( g << 8 )  & 0x0000FF00)
	      | ( r  & 0x000000FF);
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
      destroy();
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
    return new GtkImage( width, height, getFlippedBuffer( width * height ));
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
    WritableRaster raster = Raster.createPackedRaster
      (surface, surface.width, surface.height, surface.width, 
       new int[]{ 0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000 },
       new Point(0,0));

    return new BufferedImage(nativeModel, raster, true, new Hashtable());
  }

  /**
   * DataBank.getElem implementation
   */
  public int getElem(int bank, int i)
  {
    if(bank != 0 || i < 0 || i >= width*height)
      throw new IndexOutOfBoundsException(i+" size: "+width*height);
    return nativeGetElem(i);
  }
  
  /**
   * DataBank.setElem implementation
   */
  public void setElem(int bank, int i, int val)
  {
    if(bank != 0 || i < 0 || i >= width*height)
      throw new IndexOutOfBoundsException(i+" size: "+width*height);
    nativeSetElem(i, val);
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
  native long newCairoContext();

  /**
   * Copy an area of the surface. Expects parameters must be within bounds. 
   * Count on a segfault otherwise.
   */
  native void copyAreaNative(int x, int y, int width, int height, 
			     int dx, int dy, int stride);
}
