/* GraphicsConfiguration.java -- describes characteristics of graphics
   Copyright (C) 2000, 2001, 2002 Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt;

import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.VolatileImage;

/**
 * This class describes the configuration of various graphics devices, such
 * as a monitor or printer. Different configurations may exist for the same
 * device, according to the different native modes supported.
 *
 * <p>Virtual devices are supported (for example, in a multiple screen
 * environment, a virtual device covers all screens simultaneously); the
 * configuration will have a non-zero relative coordinate system in such
 * a case.
 *
 * @author Eric Blake (ebb9@email.byu.edu)
 * @see Window
 * @see Frame
 * @see GraphicsEnvironment
 * @see GraphicsDevice
 * @since 1.0
 * @status updated to 1.4
 */
public abstract class GraphicsConfiguration
{
  /**
   * The default constructor.
   *
   * @see GraphicsDevice#getConfigurations()
   * @see GraphicsDevice#getDefaultConfiguration()
   * @see GraphicsDevice#getBestConfiguration(GraphicsConfigTemplate)
   * @see Graphics2D#getDeviceConfiguration()
   */
  protected GraphicsConfiguration ()
  {
  }

  /**
   * Gets the associated device that this configuration describes.
   *
   * @return the device
   */
  public abstract GraphicsDevice getDevice();

  /**
   * Returns a buffered image optimized to this device, so that blitting can
   * be supported in the buffered image.
   *
   * @param w the width of the buffer
   * @param h the height of the buffer
   * @return the buffered image, or null if none is supported
   */
  public abstract BufferedImage createCompatibleImage(int w, int h);

  /**
   * Returns a buffered volatile image optimized to this device, so that
   * blitting can be supported in the buffered image. Because the buffer is
   * volatile, it can be optimized by native graphics accelerators.
   *
   * @param w the width of the buffer
   * @param h the height of the buffer
   * @return the buffered image, or null if none is supported
   * @see Component#createVolatileImage(int, int)
   * @since 1.4
   */
  public abstract VolatileImage createCompatibleVolatileImage(int w, int h);

  /**
   * Returns a buffered volatile image optimized to this device, and with the
   * given capabilities, so that blitting can be supported in the buffered
   * image. Because the buffer is volatile, it can be optimized by native
   * graphics accelerators.
   *
   * @param w the width of the buffer
   * @param h the height of the buffer
   * @param caps the desired capabilities of the image buffer
   * @return the buffered image, or null if none is supported
   * @throws AWTException if the capabilities cannot be met
   * @since 1.4
   */
  public VolatileImage createCompatibleVolatileImage(int w, int h,
                                                     ImageCapabilities caps)
    throws AWTException
  {
    throw new AWTException("not implemented");
  }

  /**
   * Returns a buffered image optimized to this device, and with the specified
   * transparency, so that blitting can be supported in the buffered image.
   *
   * @param w the width of the buffer
   * @param h the height of the buffer
   * @param transparency the transparency of the buffer
   * @return the buffered image, or null if none is supported
   * @see Transparency#OPAQUE
   * @see Transparency#BITMASK
   * @see Transparency#TRANSLUCENT
   */
  public abstract BufferedImage createCompatibleImage(int w, int h,
                                                      int transparency);

  /**
   * Gets the color model of the corresponding device.
   *
   * @return the color model
   */
  public abstract ColorModel getColorModel();

  /**
   * Gets a color model for the corresponding device which supports the desired
   * transparency level.
   *
   * @param transparency the transparency of the model
   * @return the color model, with transparency
   * @see Transparency#OPAQUE
   * @see Transparency#BITMASK
   * @see Transparency#TRANSLUCENT
   */
  public abstract ColorModel getColorModel(int transparency);

  /**
   * Returns a transform that maps user coordinates to device coordinates. The
   * preferred mapping is about 72 user units to 1 inch (2.54 cm) of physical
   * space. This is often the identity transform. The device coordinates have
   * the origin at the upper left, with increasing x to the right, and
   * increasing y to the bottom.
   *
   * @return the transformation from user space to device space
   * @see #getNormalizingTransform()
   */
  public abstract AffineTransform getDefaultTransform();

  /**
   * Returns a transform that maps user coordinates to device coordinates. The
   * exact mapping is 72 user units to 1 inch (2.54 cm) of physical space.
   * This is often the identity transform. The device coordinates have the
   * origin at the upper left, with increasing x to the right, and increasing
   * y to the bottom. Note that this is more accurate (and thus, sometimes more
   * costly) than the default transform.
   *
   * @return the normalized transformation from user space to device space
   * @see #getDefaultTransform()
   */
  public abstract AffineTransform getNormalizingTransform();

  /**
   * Returns the bounds of the configuration, in device coordinates. If this
   * is a virtual device (for example, encompassing several screens), the
   * bounds may have a non-zero origin.
   *
   * @return the device bounds
   * @since 1.3
   */
  public abstract Rectangle getBounds();

  /**
   * Returns the buffering capabilities of this configuration.
   *
   * @return the buffer capabilities
   * @since 1.4
   */
  public BufferCapabilities getBufferCapabilities()
  {
    throw new Error("not implemented");
  }

  /**
   * Returns the imaging capabilities of this configuration.
   *
   * @return the image capabilities
   * @since 1.4
   */
  public ImageCapabilities getImageCapabilities()
  {
    throw new Error("not implemented");
  }
} // class GraphicsConfiguration
