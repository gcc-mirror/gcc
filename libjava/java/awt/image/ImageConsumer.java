/* ImageConsumer.java -- Java interface for image consumption
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
    public static final int RANDOMPIXELORDER = 1;

    /**
     * The pixel order is top-down, left-right.
     *
     * @see #setHints
     */
    public static final int TOPDOWNLEFTRIGHT = 2;

    /**
     * The pixel order is in multiples of complete scanlines.
     *
     * @see #setHints
     */
    public static final int COMPLETESCANLINES = 4;

    /**
     * The pixels will be delivered in a single pass.  There is at
     * most one call to <code>setPixels</code> for any single pixel.
     *
     * @see #setHints
     * @see #setPixels 
     */
    public static final int SINGLEPASS = 8;

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
    public static final int SINGLEFRAME = 16;

    /**
     * Indicates an error occurred while producing an image.
     *
     * @see #imageComplete
     */
    public static final int IMAGEERROR = 1;

    /**
     * A single frame is complete but more will follow.
     * 
     * @see #imageComplete
     */
    public static final int SINGLEFRAMEDONE = 2;

    /**
     * The image is complete and no more pixels or frames will follow.
     *
     * @see #imageComplete
     */
    public static final int STATICIMAGEDONE = 3;

    /**
     * Production of the image has been aborted.
     *
     * @see #imageComplete
     */
    public static final int IMAGEABORTED = 4;

    /**
     * An <code>ImageProducer</code> indicates the size of the image
     * being produced using this method.
     * 
     * @param width the width of the image
     * @param height the height of the image 
     */
    public abstract void setDimensions(int width, int height);

    /**
     * An <code>ImageProducer</code> can set a list of properties
     * associated with this image by using this method.
     *
     * @param props the list of properties associated with this image 
     */
    public abstract void setProperties(Hashtable props);

    /**
     * This <code>ColorModel</code> should indicate the model used by
     * the majority of calls to <code>setPixels</code>.  Each call to
     * <code>setPixels</code> could however indicate a different
     * <code>ColorModel</code>.
     *
     * @param model the color model to be used most often by setPixels
     * @see ColorModel 
     */
    public abstract void setColorModel(ColorModel model);

    /**
     * The <code>ImageProducer</code> should call this method with a
     * bit mask of hints from any of <code>RANDOMPIXELORDER</code>,
     * <code>TOPDOWNLEFTRIGHT</code>, <code>COMPLETESCANLINES</code>,
     * <code>SINGLEPASS</code>, <code>SINGLEFRAME</code>.
     * 
     * @param flags a bit mask of hints
     */
    public abstract void setHints(int flags);

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as a <code>byte</code> at
     * index (n * scansize + m + offset).  
     */
    public abstract void setPixels(int x, int y, int w, int h, 
	   ColorModel model, byte[] pixels, int offset, int scansize);

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as an <code>int</code> at
     * index (n * scansize + m + offset).  
     */
    public abstract void setPixels(int x, int y, int w, int h, 
           ColorModel model, int[] pixels, int offset, int scansize);

    /**
     * The <code>ImageProducer</code> calls this method to indicate a
     * single frame or the entire image is complete.  The method is
     * also used to indicate an error in loading or producing the
     * image.  
     */
    public abstract void imageComplete(int status);
}

