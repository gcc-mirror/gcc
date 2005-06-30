/* ImageConsumer.java -- Java interface for image consumption
   Copyright (C) 1999, 2003 Free Software Foundation, Inc.

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

import java.util.Hashtable;

/**
 * An object implementing the <code>ImageProducer</code> interface can
 * use objects implementing this interface to deliver the image data.
 * 
 * @author C. Brian Jones (cbj@gnu.org)
 */
public interface ImageConsumer
{
    /**
     * The pixel order may be random.  This should be
     * the default assumption of the <code>ImageConsumer</code>.
     *
     * @see #setHints 
     */
    int RANDOMPIXELORDER = 1;

    /**
     * The pixel order is top-down, left-right.
     *
     * @see #setHints
     */
    int TOPDOWNLEFTRIGHT = 2;

    /**
     * The pixel order is in multiples of complete scanlines.
     *
     * @see #setHints
     */
    int COMPLETESCANLINES = 4;

    /**
     * The pixels will be delivered in a single pass.  There is at
     * most one call to <code>setPixels</code> for any single pixel.
     *
     * @see #setHints
     * @see #setPixels 
     */
    int SINGLEPASS = 8;

    /**
     * The pixels will be delivered with multiple calls to
     * <code>setPixels</code>.  The image contains a single frame
     * which ends when <code>imageComplete</code> is called with the
     * <code>STATICIMAGEDONE</code> flag.  If the image is constantly
     * changing such as with video then the end of each frame is
     * marked by a similar call to <code>imageComplete</code> with the
     * <code>SINGLEFRAMEDONE</code> flag.
     * 
     * @see #setHints
     * @see #imageComplete 
     */
    int SINGLEFRAME = 16;

    /**
     * Indicates an error occurred while producing an image.
     *
     * @see #imageComplete
     */
    int IMAGEERROR = 1;

    /**
     * A single frame is complete but more will follow.
     * 
     * @see #imageComplete
     */
    int SINGLEFRAMEDONE = 2;

    /**
     * The image is complete and no more pixels or frames will follow.
     *
     * @see #imageComplete
     */
    int STATICIMAGEDONE = 3;

    /**
     * Production of the image has been aborted.
     *
     * @see #imageComplete
     */
    int IMAGEABORTED = 4;

    /**
     * An <code>ImageProducer</code> indicates the size of the image
     * being produced using this method.
     * 
     * @param width the width of the image
     * @param height the height of the image 
     */
    void setDimensions(int width, int height);

    /**
     * An <code>ImageProducer</code> can set a list of properties
     * associated with this image by using this method.
     *
     * @param props the list of properties associated with this image 
     */
    void setProperties(Hashtable props);

    /**
     * This <code>ColorModel</code> should indicate the model used by
     * the majority of calls to <code>setPixels</code>.  Each call to
     * <code>setPixels</code> could however indicate a different
     * <code>ColorModel</code>.
     *
     * @param model the color model to be used most often by setPixels
     * @see ColorModel 
     */
    void setColorModel(ColorModel model);

    /**
     * The <code>ImageProducer</code> should call this method with a
     * bit mask of hints from any of <code>RANDOMPIXELORDER</code>,
     * <code>TOPDOWNLEFTRIGHT</code>, <code>COMPLETESCANLINES</code>,
     * <code>SINGLEPASS</code>, <code>SINGLEFRAME</code>.
     * 
     * @param flags a bit mask of hints
     */
    void setHints(int flags);

    /**
     * Deliver a subset of an ImageProducer's pixels to this ImageConsumer.
     *
     * Each element of the pixels array represents one pixel.  The
     * pixel data is formatted according to the color model model.
     * The x and y parameters are the coordinates of the block of
     * pixels being delivered to this ImageConsumer.  They are
     * specified relative to the top left corner of the image being
     * produced.  Likewise, w and h are the pixel block's dimensions.
     *
     * @param x x coordinate of pixel block
     * @param y y coordinate of pixel block
     * @param w width of pixel block
     * @param h height of pixel block
     * @param model color model used to interpret pixel data
     * @param pixels pixel block data
     * @param offset offset into pixels array
     * @param scansize width of one row in the pixel block
     */
    void setPixels(int x, int y, int w, int h, 
	   ColorModel model, byte[] pixels, int offset, int scansize);

    /**
     * Deliver a subset of an ImageProducer's pixels to this ImageConsumer.
     *
     * Each element of the pixels array represents one pixel.  The
     * pixel data is formatted according to the color model model.
     * The x and y parameters are the coordinates of the rectangular
     * region of pixels being delivered to this ImageConsumer,
     * specified relative to the top left corner of the image being
     * produced.  Likewise, w and h are the pixel region's dimensions.
     *
     * @param x x coordinate of pixel block
     * @param y y coordinate of pixel block
     * @param w width of pixel block
     * @param h height of pixel block
     * @param model color model used to interpret pixel data
     * @param pixels pixel block data
     * @param offset offset into pixels array
     * @param scansize width of one row in the pixel block
     */
    void setPixels(int x, int y, int w, int h, 
           ColorModel model, int[] pixels, int offset, int scansize);

    /**
     * The <code>ImageProducer</code> calls this method to indicate a
     * single frame or the entire image is complete.  The method is
     * also used to indicate an error in loading or producing the
     * image.  
     *
     * @param status the status of image production, represented by a
     * bitwise OR of ImageConsumer flags
     */
    void imageComplete(int status);
}
