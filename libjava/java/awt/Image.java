/* Image.java -- Java class for images
   Copyright (C) 1999 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */


package java.awt;

import java.awt.image.*;

/**
  * This is the abstract superclass of all image objects in Java.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public abstract class Image
{

/*
 * Static Variables
 */

/**
  * Constant indicating that the default scaling algorithm should be used.
  */
public static final int SCALE_DEFAULT = 1;

/**
  * Constant indicating that a fast scaling algorithm should be used.
  */
public static final int SCALE_FAST = 2;

/**
  * Constant indicating that a smooth scaling algorithm should be used.
  */
public static final int SCALE_SMOOTH = 4;

/**
  * Constant indicating that the <code>ReplicateScaleFilter</code> class
  * algorithm should be used for scaling.
  */
public static final int SCALE_REPLICATE = 8; 

/**
  * Constant indicating that the area averaging scaling algorithm should be
  * used.
  */
public static final int SCALE_AREA_AVERAGING = 16;

/**
  * This variable is returned whenever a property that is not defined
  * is requested.
  */
public static final Object UndefinedProperty = Image.class;

/*************************************************************************/

/*
 * Constructors
 */

/**
  * A default constructor for subclasses.
  */
public 
Image()
{
}

/*************************************************************************/

/*
 * Instance Methods
 */

/**
  * Returns the width of the image, or -1 if it is unknown.  If the
  * image width is unknown, the observer object will be notified when
  * the value is known.
  *
  * @param observer The image observer for this object.
  */
public abstract int
getWidth(ImageObserver observer);

/*************************************************************************/

/**
  * Returns the height of the image, or -1 if it is unknown.  If the
  * image height is unknown, the observer object will be notified when
  * the value is known.
  *
  * @param observer The image observer for this object.
  */
public abstract int
getHeight(ImageObserver observer);

/*************************************************************************/

/**
  * Returns the image producer object for this object.
  *
  * @return The image producer for this object.
  */
public abstract ImageProducer
getSource();

/*************************************************************************/

/**
  * Returns a graphics context object for drawing an off-screen object.
  * This method is only valid for off-screen objects.
  *
  * @return A graphics context object for an off-screen object.
  */
public abstract Graphics
getGraphics();

/*************************************************************************/

/**
  * This method requests a named property for an object.  The value of the
  * property is returned. The value <code>UndefinedProperty</code> is
  * returned if there is no property with the specified name.  The value
  * <code>null</code> is returned if the properties for the object are
  * not yet known.  In this case, the specified image observer is notified
  * when the properties are known.
  *
  * @param name The requested property name.
  * @param observer The image observer for this object.
  */
public abstract Object
getProperty(String name, ImageObserver observer);

/*************************************************************************/

/**
  * Scales the image to the requested dimension.
  * 
  * XXX: FIXME
  * 
  * @param width The width of the scaled image.
  * @param height The height of the scaled image.
  * @param flags A value indicating the algorithm to use, which will be
  * set from contants defined in this class.
  *
  * @return The scaled <code>Image</code> object.
  */
public Image
getScaledInstance(int width, int height, int flags)
  {
    return null;
  }

/*************************************************************************/

/**
  * Flushes (that is, destroys) any resources used for this image.  This
  * includes the actual image data.
  */
public abstract void
flush();

} // class Image

