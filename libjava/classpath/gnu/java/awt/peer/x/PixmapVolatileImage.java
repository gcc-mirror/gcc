/* PixmapVolatileImage.java -- VolatileImage implementation around a Pixmap
   Copyright (C) 2007 Free Software Foundation, Inc.

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

package gnu.java.awt.peer.x;

import gnu.x11.GC;
import gnu.x11.Pixmap;
import gnu.x11.image.Image;
import gnu.x11.image.ZPixmap;

import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsEnvironment;
import java.awt.ImageCapabilities;
import java.awt.Point;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.VolatileImage;
import java.awt.image.WritableRaster;

/**
 * A {@link VolatileImage} implementation that wraps an X Pixmap.
 */
class PixmapVolatileImage
  extends VolatileImage
{

  /**
   * The shared capabilities instance.
   */
  private static final ImageCapabilities caps = new ImageCapabilities(true);

  /**
   * The underlying pixmap.
   */
  private Pixmap pixmap;

  /**
   * Creates a new PixmapVolatileImage.
   *
   * @param w the width of the image
   * @param h the height of the image
   */
  public PixmapVolatileImage(int w, int h)
  {
    GraphicsEnvironment env =
      GraphicsEnvironment.getLocalGraphicsEnvironment();
    XGraphicsDevice dev = (XGraphicsDevice) env.getDefaultScreenDevice();
    pixmap = new Pixmap(dev.getDisplay(), w, h);

    // Clear pixmap.
    GC gc = new GC(pixmap);
    gc.set_foreground(0xffffffff);
    pixmap.fill_rectangle(gc, 0, 0, w, h);

  }

  @Override
  public boolean contentsLost()
  {
    return false;
  }

  @Override
  public Graphics2D createGraphics()
  {
    return new XGraphics2D(pixmap);
  }

  @Override
  public ImageCapabilities getCapabilities()
  {
    return caps;
  }

  @Override
  public int getHeight()
  {
    return pixmap.height;
  }

  @Override
  public BufferedImage getSnapshot()
  {
    // TODO: Support non-24-bit resolutions.
    int w = pixmap.width;
    int h = pixmap.height;
    ZPixmap zpixmap = (ZPixmap) pixmap.image(0, 0, w, h, 0xffffffff,
                                             Image.Format.ZPIXMAP);
    DataBuffer buffer = new ZPixmapDataBuffer(zpixmap);
    SampleModel sm = new ComponentSampleModel(DataBuffer.TYPE_BYTE, w, h, 4,
                                              w * 4,
                                              new int[]{0, 1, 2, 3 });
    ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_LINEAR_RGB);
    ColorModel cm = new ComponentColorModel(cs, true, false,
                                            Transparency.OPAQUE,
                                            DataBuffer.TYPE_BYTE);
    WritableRaster raster = Raster.createWritableRaster(sm, buffer,
                                                        new Point(0, 0));
    return new BufferedImage(cm, raster, false, null);
  }

  @Override
  public int getWidth()
  {
    return pixmap.width;
  }

  @Override
  public int validate(GraphicsConfiguration gc)
  {
    // TODO: Check compatibility with gc.
    return IMAGE_OK;
  }

  @Override
  public int getHeight(ImageObserver observer)
  {
    return getHeight();
  }

  @Override
  public Object getProperty(String name, ImageObserver observer)
  {
    return null;
  }

  @Override
  public int getWidth(ImageObserver observer)
  {
    return getWidth();
  }

  /**
   * Returns the underlying X pixmap. This is used for the graphics code.
   *
   * @return the underlying X pixmap
   */
  Pixmap getPixmap()
  {
    return pixmap;
  }
}
