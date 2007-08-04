/* XGraphicsConfiguration.java -- GraphicsConfiguration for X
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

package gnu.java.awt.peer.x;

import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Transparency;
import java.awt.color.ColorSpace;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.ComponentColorModel;
import java.awt.image.ComponentSampleModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.SampleModel;
import java.awt.image.VolatileImage;
import java.awt.image.WritableRaster;

public class XGraphicsConfiguration
    extends GraphicsConfiguration
{

  XGraphicsDevice device;

  XGraphicsConfiguration(XGraphicsDevice dev)
  {
    device = dev;
  }

  public GraphicsDevice getDevice()
  {
    return device;
  }

  public BufferedImage createCompatibleImage(int w, int h)
  {
    return createCompatibleImage(w, h, Transparency.OPAQUE);
  }

  public BufferedImage createCompatibleImage(int w, int h, int transparency)
  {
    BufferedImage bi;
    switch (transparency)
      {
        case Transparency.OPAQUE:
          DataBuffer buffer = new ZPixmapDataBuffer(w, h);
          SampleModel sm = new ComponentSampleModel(DataBuffer.TYPE_BYTE, w, h,
                                                    4, w * 4,
                                                    new int[]{0, 1, 2, 3 });
          ColorSpace cs = ColorSpace.getInstance(ColorSpace.CS_LINEAR_RGB);
          ColorModel cm = new ComponentColorModel(cs, true, false,
                                                  Transparency.OPAQUE,
                                                  DataBuffer.TYPE_BYTE);
          WritableRaster raster = Raster.createWritableRaster(sm, buffer,
                                                              new Point(0, 0));
          bi = new BufferedImage(cm, raster, false, null);
          break;
        case Transparency.BITMASK:
        case Transparency.TRANSLUCENT:
          bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
          break;
        default:
          throw new IllegalArgumentException("Illegal transparency: "
                                             + transparency);
      }
    return bi;
  }

  public VolatileImage createCompatibleVolatileImage(int w, int h)
  {
    return createCompatibleVolatileImage(w, h, Transparency.OPAQUE);
  }

  public VolatileImage createCompatibleVolatileImage(int width, int height,
                                                     int transparency)
  {
    VolatileImage im;
    switch (transparency)
      {
      case Transparency.OPAQUE:
        im = new PixmapVolatileImage(width, height);
        break;
      case Transparency.BITMASK:
      case Transparency.TRANSLUCENT:
        throw new UnsupportedOperationException("Not yet implemented");
      default:
        throw new IllegalArgumentException("Unknown transparency type: "
                                           + transparency);  
      }
    return im;
  }

  public ColorModel getColorModel()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public ColorModel getColorModel(int transparency)
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public AffineTransform getDefaultTransform()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public AffineTransform getNormalizingTransform()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  public Rectangle getBounds()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

}
