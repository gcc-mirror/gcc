/* Image.java -- superclass for images
   Copyright (C) 1999, 2002, 2004, 2005  Free Software Foundation, Inc.

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

import java.awt.image.FilteredImageSource;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.ReplicateScaleFilter;

/**
 * This is the abstract superclass of all image objects in Java.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 * @since 1.0
 * @status updated to 1.4
 */
public abstract class Image
{
  /**
   * This variable is returned whenever a property that is not defined
   * is requested.
   */
  // For debug purposes, this might as well be a unique string.
  public static final Object UndefinedProperty
    = new String("undefined property");

  /**
   * Constant indicating that the default scaling algorithm should be used.
   *
   * @since 1.1
   */
  public static final int SCALE_DEFAULT = 1;

  /**
   * Constant indicating that a fast scaling algorithm should be used.
   *
   * @since 1.1
   */
  public static final int SCALE_FAST = 2;

  /**
   * Constant indicating that a smooth scaling algorithm should be used.
   *
   * @since 1.1
   */
  public static final int SCALE_SMOOTH = 4;

  /**
   * Constant indicating that the <code>ReplicateScaleFilter</code> class
   * algorithm should be used for scaling.
   *
   * @see ReplicateScaleFilter
   * @since 1.1
   */
  public static final int SCALE_REPLICATE = 8;

  /**
   * Constant indicating that the area averaging scaling algorithm should be
   * used.
   *
   * @see AreaAveragingScaleFilter
   * @since 1.1
   */
  public static final int SCALE_AREA_AVERAGING = 16;

  /**
   * A default constructor for subclasses.
   */
  public Image()
  {
  }

  /**
   * Returns the width of the image, or -1 if it is unknown.  If the
   * image width is unknown, the observer object will be notified when
   * the value is known.
   *
   * @param observer the image observer for this object
   * @return the width in pixels
   * @see #getHeight(ImageObserver)
   */
  public abstract int getWidth(ImageObserver observer);

  /**
   * Returns the height of the image, or -1 if it is unknown.  If the
   * image height is unknown, the observer object will be notified when
   * the value is known.
   *
   * @param observer the image observer for this object
   * @return the height in pixels
   * @see #getWidth(ImageObserver)
   */
  public abstract int getHeight(ImageObserver observer);

  /**
   * Returns the image producer object for this object. The producer is the
   * object which generates pixels for this image.
   *
   * @return the image producer for this object
   */
  public abstract ImageProducer getSource();

  /**
   * Returns a graphics context object for drawing an off-screen object.
   * This method is only valid for off-screen objects.
   *
   * @return a graphics context object for an off-screen object
   * @see Graphics#createImage(int, int)
   */
  public abstract Graphics getGraphics();

  /**
   * This method requests a named property for an object.  The value of the
   * property is returned. The value <code>UndefinedProperty</code> is
   * returned if there is no property with the specified name.  The value
   * <code>null</code> is returned if the properties for the object are
   * not yet known.  In this case, the specified image observer is notified
   * when the properties are known.
   *
   * @param name the requested property name
   * @param observer the image observer for this object
   * @return the named property, if available
   * @see #UndefinedProperty
   */
  public abstract Object getProperty(String name, ImageObserver observer);

  /**
   * Scales the image to the requested dimension. A new Image with asynchronous
   * loading will be produced according to the hints of the algorithm
   * requested. If either the width or height is non-positive, it is adjusted
   * to preserve the original aspect ratio.
   *
   * @param width the width of the scaled image
   * @param height the height of the scaled image
   * @param flags a value indicating the algorithm to use
   * @return the scaled <code>Image</code> object
   * @see #SCALE_DEFAULT
   * @see #SCALE_FAST
   * @see #SCALE_SMOOTH
   * @see #SCALE_REPLICATE
   * @see #SCALE_AREA_AVERAGING
   * @since 1.1
   */
  public Image getScaledInstance(int width, int height, int flags)
  {
    switch (flags)
    {
      case SCALE_DEFAULT:
      case SCALE_FAST:
      case SCALE_REPLICATE:
        ImageProducer producer =
          new FilteredImageSource(this.getSource(),
                                  new ReplicateScaleFilter(width, height));
        return Toolkit.getDefaultToolkit().createImage(producer);
      case SCALE_SMOOTH:
      case SCALE_AREA_AVERAGING:
      default:
        throw new Error("not implemented");
    }
  }

  /**
   * Flushes (that is, destroys) any resources used for this image.  This
   * includes the actual image data.
   */
  public abstract void flush();
} // class Image
