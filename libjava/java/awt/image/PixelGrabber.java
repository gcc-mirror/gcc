/* PixelGrabber.java -- Java class for providing image data 
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
import java.util.Hashtable;

/**
   PixelGrabber is an ImageConsumer designed to extract a rectangular region of pixels
   from an Image
 */
public class PixelGrabber implements ImageConsumer 
{
    int x, y, width, height, status, scansize, offset;
    ColorModel model = ColorModel.getRGBdefault();
    //int hints;
    //Hashtable props;
    int pixel_bufferi[];
    byte pixel_bufferb[];
    boolean grabbing;
    ImageProducer ip;

    /**
     * Create a PixelGrabber used to grab pixels from the specified Image 
     * in the specified rectangle
     *
     * @param img the Image to grab pixels from
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param pixels the array of pixel values
     * @param offset the index of the first pixels in the <code>pixels</code> array
     * @param scansize the width to use in extracting pixels from the <code>pixels</code> array
     */
    public PixelGrabber(Image img, int x, int y, int w, int h,
			int pix[], int off, int scansize)
    {
	this( img.getSource(), x, y, w, h, pix, off, scansize );
    }

    /**
     * Create a PixelGrabber used to grab pixels from the specified ImageProducer
     * in the specified rectangle
     *
     * @param ip the ImageProducer to grab pixels from
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param pixels the array of pixel values
     * @param offset the index of the first pixels in the <code>pixels</code> array
     * @param scansize the width to use in extracting pixels from the <code>pixels</code> array
     */
    public PixelGrabber(ImageProducer ip, int x, int y, int w, int h,
			int pix[], int off, int scansize)
    {
	this.ip = ip;
	this.x = x;
	this.y = y;
	this.width = w;
	this.height = h;
	this.pixel_bufferi = pix;
	this.offset = off;
	this.scansize = scansize;
	pixel_bufferb = new byte[pix.length * 4];
    }


    /**
     * Create a PixelGrabber used to grab pixels from the specified Image 
     * in the specified rectangle
     *
     * @param img the Image to grab pixels from
     * @param x the x coordinate of the rectangle
     * @param y the y coordinate of the rectangle
     * @param w the width of the rectangle
     * @param h the height of the rectangle
     * @param forceRGB true to force conversion to RGB
     */
    public PixelGrabber(Image img,
			int x, int y,
			int w, int h,
			boolean forceRGB)
    {
	//FIXME
    }

    /**
       Start Grabbing Pixels
     */
    public synchronized void startGrabbing()
    {
	if ( grabbing == false )
	    {
		grabbing = true;
		ip.startProduction( this );
	    }
    }

    /**
       Abort the grabbing of pixels
     */
    public synchronized void abortGrabbing()
    {
	if ( grabbing == true )
	    {
		grabbing = false;
		ip.removeConsumer( this );
	    }
    }

    /**
       Grab the Pixels.

       @return true if successful

       @throws InterruptedException if interrupted by another thread.
     */
    public boolean grabPixels() throws InterruptedException
    {
      return grabPixels(0);
    }

    /**
       Grab the Pixels and abort if it takes too long

       @return true if successful

       @throws InterruptedException if interrupted by another thread.
               or time runs out
     */
    public synchronized boolean grabPixels(long ms) throws InterruptedException
    {
	startGrabbing();
	
	if (ms < 0)
	  return (status == ImageObserver.ALLBITS);
	
	wait(ms);
	
	if (status == ImageObserver.ALLBITS)
	    return true;
	else
	    return false;
    }

    /**
       Get the status of the pixel grabbing representing by ImageObserver flags

       @return the status
    */
    public synchronized int getStatus()
    {
	return status;
    }

    /**
       Return width of pixel region

       @return width of region
    */
    public synchronized int getWidth()
    {
	return width;
    }

    /**
       Return height of pixel region
       
       @return height of region
    */
    public synchronized int getHeight()
    {
	return height;
    }

    /**
       Returns the grabbed pixel buffer 

       @return a byte or int array
    */
    public synchronized Object getPixels()
    {
	if( pixel_bufferi != null )
	    return pixel_bufferi;
	return pixel_bufferb;
    }

    /**
       Get the ColorModel of the image
       
       @return the ColorModel
    */
    public synchronized ColorModel getColorModel()
    {
	return model;
    }

    /**
     * An <code>ImageProducer</code> indicates the size of the image
     * being produced using this method.
     * 
     * @param width the width of the image
     * @param height the height of the image 
     */
    public  void setDimensions(int width, int height)
    {
    }

    /**
     * An <code>ImageProducer</code> can set a list of properties
     * associated with this image by using this method.
     *
     * @param props the list of properties associated with this image 
     */
    public  void setProperties(Hashtable props)
    {
	//this.props = props; //FIXME - DO WE NEED THIS
    }

    /**
     * This <code>ColorModel</code> should indicate the model used by
     * the majority of calls to <code>setPixels</code>.  Each call to
     * <code>setPixels</code> could however indicate a different
     * <code>ColorModel</code>.
     *
     * @param model the color model to be used most often by setPixels
     * @see ColorModel 
     */
    public  void setColorModel(ColorModel model)
    {
	this.model = model;
    }

    /**
     * The <code>ImageProducer</code> should call this method with a
     * bit mask of hints from any of <code>RANDOMPIXELORDER</code>,
     * <code>TOPDOWNLEFTRIGHT</code>, <code>COMPLETESCANLINES</code>,
     * <code>SINGLEPASS</code>, <code>SINGLEFRAME</code>.
     * 
     * @param flags a bit mask of hints
     */
    public  void setHints(int flags)
    {
	//hints = flags; // FIXME - DO NOT KNOW WHAT TO DO WITH THE HINTS
    }

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as a <code>byte</code> at
     * index (n * scansize + m + offset).  
     */
    public  void setPixels(int x, int y, int w, int h, 
			   ColorModel model, byte[] pixels, int offset, int scansize)
    {
	//FIXME - I hate bytes
	int xp, yp;
	for( xp = x; xp < ( x + w); xp++ )
	    for( yp = y; yp < (y + h); yp++ )
		if( xp >= this.x && 
		    yp >= this.y && 
		    xp <= ( this.x + this.width ) && 
		    yp <= ( this.y + this.height ) ) {
		    pixel_bufferb[(yp - this.y) * this.scansize + (xp - this.x) + this.offset] =
			pixels[ offset + yp * scansize + xp ];
		}

    }

    /**
     * This function delivers a rectangle of pixels where any
     * pixel(m,n) is stored in the array as an <code>int</code> at
     * index (n * scansize + m + offset).  
     */
    public  void setPixels(int x, int y, int w, int h, 
			   ColorModel model, int[] pixels, int offset, int scansize)
    {
	int xp, yp;
	for( xp = x; xp < ( x + w); xp++ )
	    for( yp = y; yp < (y + h); yp++ )
		if( xp >= this.x && 
		    yp >= this.y && 
		    xp <= ( this.x + this.width ) && 
		    yp <= ( this.y + this.height ) ) {
		    pixel_bufferi[(yp - this.y) * this.scansize + (xp - this.x) + this.offset] =
			pixels[ offset + yp * scansize + xp ];
		}
    }

    /**
     * The <code>ImageProducer</code> calls this method to indicate a
     * single frame or the entire image is complete.  The method is
     * also used to indicate an error in loading or producing the
     * image.  
     */
    public synchronized void imageComplete(int status)
    {
	this.status = status;
    }

    /**
       @deprecated by getStatus
    */
    public synchronized int status()
    {
	return getStatus();
    }

}
