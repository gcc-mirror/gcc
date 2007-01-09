/* GtkVolatileImage.java -- wraps an X pixmap
   Copyright (C) 2005  Free Software Foundation, Inc.

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
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.ImageCapabilities;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DirectColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.SinglePixelPackedSampleModel;
import java.awt.image.VolatileImage;
import java.awt.image.WritableRaster;

public class GtkVolatileImage extends VolatileImage
{
  int width, height;
  private ImageCapabilities caps;

  final GtkComponentPeer component;

  static ColorModel gdkColorModel = new DirectColorModel(32,
                                                         0x000000FF,
                                                         0x0000FF00,
                                                         0x00FF0000,
                                                         0xFF000000);
                                                         
  /**
   * Don't touch, accessed from native code.
   */
  long nativePointer;

  native long init(GtkComponentPeer component, int width, int height);

  native void destroy(long pointer);

  native int[] nativeGetPixels(long pointer);
  
  /**
   * Gets the pixels in the current image from GDK.
   * 
   * Note that pixels are in 32-bit RGBA, non-premultiplied, which is different
   * from Cairo's premultiplied ARGB, which is different from Java's standard
   * non-premultiplied ARGB.  Caution is advised when using this method, to
   * ensure that the data format remains consistent with what you expect.
   *  
   * @return the current pixels, as reported by GDK.
   */
  public int[] getPixels()
  {
    return nativeGetPixels(nativePointer);
  }

  native void nativeCopyArea(long pointer, int x, int y, int w, int h, int dx,
                             int dy );
  public void copyArea(int x, int y, int w, int h, int dx, int dy)
  {
    nativeCopyArea(nativePointer, x, y, w, h, dx, dy);
  }

  native void nativeDrawVolatile(long pointer, long srcPtr, int x, int y,
                                 int w, int h );
  public void drawVolatile(long srcPtr, int x, int y, int w, int h )
  {
    nativeDrawVolatile(nativePointer, srcPtr, x, y, w, h);
  }

  public GtkVolatileImage(GtkComponentPeer component, 
			  int width, int height, ImageCapabilities caps)
  {
    this.width = width;
    this.height = height;
    this.caps = caps;
    this.component = component;
    nativePointer = init( component, width, height );
  }

  public GtkVolatileImage(int width, int height, ImageCapabilities caps)
  {
    this(null, width, height, caps);
  }

  public GtkVolatileImage(int width, int height)
  {
    this(null, width, height, null);
  }

  public void finalize()
  {
    dispose();
  }

  public void dispose()
  {
    destroy(nativePointer);
  }

  public BufferedImage getSnapshot()
  {
    WritableRaster raster = Raster.createWritableRaster(createGdkSampleModel(width, height),
                                                        new Point(0, 0));
    raster.setDataElements(0, 0, getPixels());
    return new BufferedImage(gdkColorModel, raster,
                             gdkColorModel.isAlphaPremultiplied(), null);
  }

  public Graphics getGraphics()
  {
    return createGraphics();
  }

  public Graphics2D createGraphics()
  {
    return new VolatileImageGraphics( this );
  }

  public int validate(GraphicsConfiguration gc)
  {
    return VolatileImage.IMAGE_OK;
  }

  public boolean contentsLost()
  {
    return false;
  }

  public ImageCapabilities getCapabilities()
  {
    return caps;
  }

  public int getWidth()
  {
    return width;
  }

  public int getHeight()
  {
    return height;
  }

  public int getWidth(java.awt.image.ImageObserver observer)
  {
    return width;
  }
  
  public int getHeight(java.awt.image.ImageObserver observer)
  {
    return height;
  }

  public Object getProperty(String name, ImageObserver observer)
  {
    return null;
  }
  
  /**
   * Creates a SampleModel that matches GDK's native format
   */
  protected static SampleModel createGdkSampleModel(int w, int h)
  {
    return new SinglePixelPackedSampleModel(DataBuffer.TYPE_INT, w, h,
                                            new int[]{0x000000FF, 0x0000FF00,
                                                      0x00FF0000, 0xFF000000});
  }
}
