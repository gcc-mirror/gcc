/* ImageObserver.java -- Java interface for asynchronous updates to an image
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

import java.awt.Image;

/**
 * An object implementing the <code>ImageObserver</code> interface can
 * receive updates on image construction from an
 * <code>ImageProducer</code> asynchronously.
 *
 * @see ImageProducer
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public interface ImageObserver
{
    /**
     * The width of the image has been provided as the
     * <code>width</code> argument to <code>imageUpdate</code>.
     *
     * @see #imageUpdate 
     */
    public static final int WIDTH = 1;

    /**
     * The height of the image has been provided as the
     * <code>height</code> argument to <code>imageUpdate</code>.
     *
     * @see #imageUpdate 
     */
    public static final int HEIGHT = 2;

    /**
     * The properties of the image have been provided.
     *
     * @see #imageUpdate
     * @see java.awt.Image#getProperty (java.lang.String, java.awt.image.ImageObserver)
     */
    public static final int PROPERTIES = 4;

    /**
     * More pixels are now available for drawing a scaled variation of
     * the image.
     *
     * @see #imageUpdate 
     */
    public static final int SOMEBITS = 8;

    /**
     * All the pixels needed to draw a complete frame of a multi-frame
     * image are available.
     *
     * @see #imageUpdate 
     */
    public static final int FRAMEBITS = 16;

    /**
     * An image with a single frame, a static image, is complete.
     *
     * @see #imageUpdate
     */
    public static final int ALLBITS = 32;

    /**
     * An error was encountered while producing the image.
     *
     * @see #imageUpdate
     */
    public static final int ERROR = 64;

    /**
     * Production of the image was aborted.
     *
     * @see #imageUpdate
     */
    public static final int ABORT = 128;

    /**
     * This is a callback method for an asynchronous image producer to
     * provide updates on the production of the image as it happens.
     *
     * @param image the image the update refers to
     * @param flags a bit mask indicating what is provided with this update
     * @param x the x coordinate of the image
     * @param y the y coordinate of the image
     * @param width the width of the image
     * @param height the height of the image
     * 
     * @see java.awt.Image 
     */
    public abstract boolean imageUpdate(Image image, int flags, int x, 
					int y, int width, int height);
}
