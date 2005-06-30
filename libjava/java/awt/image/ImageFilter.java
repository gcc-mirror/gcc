/* ImageFilter.java -- Java class for filtering images
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
 * The <code>ImageFilter</code> class is a base class which can be
 * extended to provide different types of filters for an image.  By
 * default this class does nothing to an image passing through it.
 *
 * @author C. Brian Jones (cbj@gnu.org) 
 */
public class ImageFilter implements ImageConsumer, Cloneable
{
    /**
     * The consumer this filter is filtering an image data stream for.
     * It is initialized in the method <code>getFilterInstance</code>.  
     */
    protected ImageConsumer consumer = null;

    /**
     * The <code>ImageConsumer</code> can use this method to request
     * the pixels be delivered in top-down, left-right order.  
     * <br> 
     * The filter can respond in three different ways.  
     * <ul>
     *   <li>The default behavior is to forward the request to the 
     *       <code>ImageProducer</code> 
     *       using the method <code>requestTopDownLeftRightResend</code>
     *       and using the filter as the consumer.</li>
     *   <li>The filter has the pixels and can retransmit them in the
     *       top-down, left-right order.</li>
     *   <li>The filter can do nothing when this method is called.</li>
     * </ul>
     */
    public void resendTopDownLeftRight(ImageProducer ip)
    {
	ip.requestTopDownLeftRightResend(this);
    }

    /**
     * By default, returns a shallow copy of the object created by
     * <code>Object.clone()</code> 
     *
     * @see java.lang.Object#clone ()
     */
    public Object clone()
    {
      try
        {
          return super.clone();
        }
      catch (CloneNotSupportedException e)
        {
          // This should never happen as this class implements the
          // Cloneable interface.
          throw new InternalError ();
        }
    }

    /**
     * This is the only method which can set the
     * <code>ImageConsumer</code> for this filter.  By default a clone
     * of this filter with the appropriate consumer set is returned.  
     *
     * @see #clone ()
     */
    public ImageFilter getFilterInstance(ImageConsumer ic)
    {
	if ( ic == null )
	    throw new IllegalArgumentException("null argument for ImageFilter.getFilterInstance(ImageConsumer)");

	consumer = ic;
	ImageFilter f = (ImageFilter)clone();
	consumer = null;
	return f;
    }

    /**
     * An <code>ImageProducer</code> indicates the size of the image
     * being produced using this method.  A filter can override this 
     * method to intercept these calls from the producer in order to
     * change either the width or the height before in turn calling
     * the consumer's <code>setDimensions</code> method.
     * 
     * @param width the width of the image
     * @param height the height of the image 
     */
    public void setDimensions(int width, int height)
    {
	consumer.setDimensions(width, height);
    }

    /**
     * An <code>ImageProducer</code> can set a list of properties
     * associated with this image by using this method.
     *
     * @param props the list of properties associated with this image 
     */
    public void setProperties(Hashtable props)
    {
	props.put("filters", "ImageFilter");
	consumer.setProperties(props);
    }

    /**
     * Override this method to process calls to this method from the
     * <code>ImageProducer</code>.  By default the <code>setColorModel</code>
     * method of the consumer is called with the specified <code>model</code>.
     *
     * @param model the color model to be used most often by setPixels
     * @see ColorModel */
    public void setColorModel(ColorModel model)
    {
	consumer.setColorModel(model);
    }

    /**
     * The <code>ImageProducer</code> should call this method with a
     * bit mask of hints from any of <code>RANDOMPIXELORDER</code>,
     * <code>TOPDOWNLEFTRIGHT</code>, <code>COMPLETESCANLINES</code>,
     * <code>SINGLEPASS</code>, <code>SINGLEFRAME</code> from the 
     * <code>ImageConsumer</code> interface.
     * 
     * @param flags a bit mask of hints
     * @see ImageConsumer
     */
    public void setHints(int flags)
    {
	consumer.setHints(flags);
    }

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as a <code>byte</code> at
     * index (n * scansize + m + offset).  
     *
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param model the <code>ColorModel</code> used to translate the pixels
     * @param pixels the array of pixel values
     * @param offset the index of the first pixels in the <code>pixels</code> array
     * @param scansize the width to use in extracting pixels from the <code>pixels</code> array
     */
    public void setPixels(int x, int y, int w, int h, 
	   ColorModel model, byte[] pixels, int offset, int scansize)
    {
	consumer.setPixels(x, y, w, h, model, pixels, offset, scansize);
    }

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as an <code>int</code> at
     * index (n * scansize + m + offset).  
     *
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param model the <code>ColorModel</code> used to translate the pixels
     * @param pixels the array of pixel values
     * @param offset the index of the first pixels in the <code>pixels</code> array
     * @param scansize the width to use in extracting pixels from the <code>pixels</code> array
     */
    public void setPixels(int x, int y, int w, int h, 
           ColorModel model, int[] pixels, int offset, int scansize)
    {
	consumer.setPixels(x, y, w, h, model, pixels, offset, scansize);
    }

    /**
     * The <code>ImageProducer</code> calls this method to indicate a
     * single frame or the entire image is complete.  The method is
     * also used to indicate an error in loading or producing the
     * image.  
     */
    public void imageComplete(int status)
    {
	consumer.imageComplete(status);
    }
}

