/* QtScreenDeviceConfiguration.java --
   Copyright (C)  2005  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.DisplayMode;
import java.awt.ImageCapabilities;
import java.awt.GraphicsConfiguration;
import java.awt.GraphicsDevice;
import java.awt.GraphicsConfigTemplate;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.VolatileImage;
import java.awt.geom.AffineTransform;

public class QtScreenDeviceConfiguration extends GraphicsConfiguration {
  
  private QtScreenDevice owner;
  private Rectangle bounds;
  private double dpiX, dpiY;
  private int depth;

  public QtScreenDeviceConfiguration(QtScreenDevice owner)
  {
    this.owner = owner;
    bounds = owner.getBounds();
    dpiX = owner.getDpiX();
    dpiY = owner.getDpiY();
    depth = owner.depth();
  }

  public BufferedImage createCompatibleImage(int width, int height)
  {
    switch( depth )
      {
      case 24:
	return new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
      case 16:
	return new BufferedImage(width, height, 
				 BufferedImage.TYPE_USHORT_565_RGB);
      case 8:
	return new BufferedImage(width, height, BufferedImage.TYPE_BYTE_INDEXED);
      default:
      case 32:
	return new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
      }
  }

  public BufferedImage createCompatibleImage(int width, int height, int transparency)
  {
    // FIXME: Take the transpareny flag into account? 
    // For now, ignore it and just use an alpha channel.
    if(depth == 32)
      return new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    return createCompatibleImage(width, height);
  }

  public VolatileImage createCompatibleVolatileImage(int width, int height)
  {
    return new QtVolatileImage( width, height );
  }
  
  public VolatileImage createCompatibleVolatileImage(int width, int height, 
						     ImageCapabilities caps)
  {
    return createCompatibleVolatileImage( width, height );
  }

  public Rectangle getBounds()
  {
    return bounds;
  }

  public ColorModel getColorModel()
  {
    // FIXME?
    return QtToolkit.getDefaultToolkit().getColorModel();
  }

  public ColorModel getColorModel(int transparency)
  {
    // FIXME?
    return QtToolkit.getDefaultToolkit().getColorModel();
  }

  public AffineTransform getDefaultTransform()
  {
    return new AffineTransform();
  }

  public GraphicsDevice	getDevice()
  {
    return owner;
  }

  /**
   * Returns the transform which transforms from this display's resolution
   * to a 72 DPI resolution.
   */
  public AffineTransform getNormalizingTransform()
  {
    AffineTransform nTrans = new AffineTransform();
    nTrans.scale( 72.0 / dpiX, 72.0 / dpiY );
    return nTrans;
  }

  public VolatileImage createCompatibleVolatileImage(int width, int height, 
						     int transparency)
  {
    return createCompatibleVolatileImage(width, height);
  }
}
