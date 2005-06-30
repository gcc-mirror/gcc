/* VolatileImage.java -- a hardware-accelerated image buffer
   Copyright (C) 2002, 2005 Free Software Foundation, Inc.

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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Transparency;
import java.awt.ImageCapabilities;

/**
 * VolatileImage represents a hardware-accelerated graphics buffer.
 * The native graphics system may free or damage the resources
 * occupied by a VolatileImage at any time.  As such, one must
 * frequently check the "validity" of the image buffer's resources.
 *
 * A volatile image's "validity" depends on multiple factors.  Its
 * resources may have become unavailble in which case you must
 * reallocate them.  If you move the image from one output device to
 * another, you may need to recreate the image's resources if the new
 * output device's capabilities don't match the old one's.  Finally,
 * if the contents of the image's buffer have been damaged you must
 * re-render the image.
 *
 * VolatileImages should always be created using either
 * Component.createVolatileImage or
 * GraphicsConfiguration.createCompatibleVolatileImage.
 */
public abstract class VolatileImage extends Image
  implements Transparency
{
  /**
   * One of validate's possible return values.  Indicates that the
   * image buffer matches its graphics configuration's capabilities
   * and that its resources are initialized and ready to be drawn
   * into.  Also implies that any existing image rendered to the
   * buffer is intact and need not be re-rendered.
   */
  public static final int IMAGE_OK = 0;

  /**
   * One of validate's possible return values.  Indicates that the
   * image buffer has been restored, meaning that it is valid and
   * ready-to-use but that its previous contents have been lost.  This
   * return value implies that the image needs to be re-rendered.
   */
  public static final int IMAGE_RESTORED = 1;

  /**
   * One of validate's possible return values.  Indicates that the
   * image buffer type is unsupported by the current graphics
   * configuration.  The graphics configuration may have changed, for
   * example if the image moved from one output device to another.
   * This return value implies that the image buffer's resources
   * should be re-acquired.
   */
  public static final int IMAGE_INCOMPATIBLE = 2;

  /**
   * This image's transparency type.  One of Transparency.BITMASK,
   * Transparency.OPAQUE or Transparency.TRANSLUCENT.
   *
   * @since 1.5
   */
  protected int transparency;

  /**
   * Default constructor.  VolatileImages should not be created
   * directly.  Rather, you should use Component.createVolatileImage
   * or GraphicsConfiguration.createCompatibleVolatileImage.
   */
  public VolatileImage()
  {
  }

  /**
   * Returns an image representing the current state of the volatile
   * image buffer.  The returned image is static meaning that it is
   * not updated after being created.  It is a snapshot of the
   * volatile image buffer at the time getSnapshot is called.
   *
   * This method, which reads pixels from the volatile image buffer,
   * may be less-performant than methods that write pixels since
   * VolatileImages are typically optimized for writing.
   *
   * @return a BufferedImage representing this VolatileImage
   */
  public abstract BufferedImage getSnapshot();

  /**
   * Returns the width of this image buffer.
   *
   * @return the width of this VolatileImage
   */
  public abstract int getWidth();

  /**
   * Returns the height of this image buffer.
   *
   * @return the height of this VolatileImage
   */
  public abstract int getHeight();

  /**
   * Calling this method is equivalent to calling
   * getSnapshot().getSource().  The ImageProducer produces pixels
   * from the BufferedImage snapshot and not from the VolatileImage
   * itself.  Thus, changes to the VolatileImage that occur after this
   * ImageProducer has been retrieved will not be reflected in the
   * produced pixels.
   *
   * This method, which reads pixels from the volatile image buffer,
   * may be less-performant than methods that write pixels since
   * VolatileImages are typically optimized for writing.
   *
   * @return an ImageProducer for a static BufferedImage snapshot of
   * this image buffer
   */
  public ImageProducer getSource()
  {
    return getSnapshot().getSource();
  }

  /**
   * Releases the system resources taken by this image.
   */
  public void flush()
  {
  }

  /**
   * Returns a Graphics2D object that can be used to draw onto this
   * image.  This method is present for backwards-compatibility.  It
   * simply returns the result of createGraphics.
   *
   * @return a Graphics2D object that can be used to draw onto this
   * image
   */
  public Graphics getGraphics()
  {
    return createGraphics();
  }

  /**
   * Returns a Graphics2D object that can be used to draw onto this
   * image.
   *
   * @return a Graphics2D object that can be used to draw onto this
   * image
   */
  public abstract Graphics2D createGraphics();

  /**
   * Validates and restores this image.  If the image buffer has
   * become unavailable for further use since the last call to
   * validate, validate will allocate a new image buffer.  The image
   * is also "validated" against the GraphicsConfiguration parameter.
   *
   * "Validating" the image means checking that the capabilities it
   * requires of the output device are indeed supported by the given
   * output device.  If the image's characteristics, which can be
   * highly output device-specific, are not supported by the graphics
   * configuration, then IMAGE_INCOMPATIBLE will be returned.  This
   * can happen, for example, if this image was created on one output
   * device, then validated against a different output device with
   * different capabilities.  Calling validate with a NULL gc argument
   * causes validate to skip the validation test.
   *
   * @param gc graphics configuration against which to validate or
   * NULL
   *
   * @return a code indicating the result of validation. One of:
   * <ul>
   *   <li><code>IMAGE_OK</code> if the image did not need to be
   *   validated and didn't need to be restored</li>
   *   <li><code>IMAGE_RESTORED</code> if the image may need to be
   *   re-rendered.</li>
   *   <li><code>IMAGE_INCOMPATIBLE</code> if this image's
   *   requirements are not fulfilled by the graphics configuration
   *   parameter.  This implies that you need to create a new
   *   VolatileImage for the different GraphicsConfiguration or
   *   Component. This return value implies nothing about whether the
   *   image is valid or needs to be re-rendered.</li>
   * </ul>
   */
  public abstract int validate(GraphicsConfiguration gc);

  /**
   * Returns true if the contents of the image buffer have been
   * damaged or if the image buffer's resources have been reclaimed by
   * the graphics system.  You should call this method after a series
   * of rendering operations to or from the image, to see if the image
   * buffer needs to be revalidated or the image re-rendered.
   *
   * @return true if the validate should be called, false otherwise
   */
  public abstract boolean contentsLost();

  /**
   * Returns the capabilities of this image buffer.
   *
   * @return the capabilities of this image buffer
   */
  public abstract ImageCapabilities getCapabilities();

  /**
   * Returns the transparency type of this image.
   *
   * @return Transparency.OPAQUE, Transparency.BITMASK or
   * Transparency.TRANSLUCENT
   */
  public int getTransparency()
  {
    return transparency;
  }
}
