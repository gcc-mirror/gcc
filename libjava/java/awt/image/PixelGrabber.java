/* PixelGrabber.java -- retrieve a subset of an image's data
   Copyright (C) 1999, 2003, 2004  Free Software Foundation, Inc.

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
 * PixelGrabber is an ImageConsumer that extracts a rectangular region
 * of pixels from an Image.
 */
public class PixelGrabber implements ImageConsumer
{
  int x, y, offset;
  int width = -1;
  int height = -1;
  int scansize = -1;
  boolean forceRGB = true;

  ColorModel model = ColorModel.getRGBdefault();
  int hints;
  Hashtable props;

  int int_pixel_buffer[];
  boolean ints_delivered = false;
  byte byte_pixel_buffer[];
  boolean bytes_delivered = false;

  ImageProducer ip;
  int observerStatus;
  int consumerStatus;

  private Thread grabberThread;
  boolean grabbing = false;

  /**
   * Construct a PixelGrabber that will retrieve RGB data from a given
   * Image.
   *
   * The RGB data will be retrieved from a rectangular region
   * <code>(x, y, w, h)</code> within the image.  The data will be
   * stored in the provided <code>pix</code> array, which must have
   * been initialized to a size of at least <code>w * h</code>.  The
   * data for a pixel (m, n) in the grab rectangle will be stored at
   * <code>pix[(n - y) * scansize + (m - x) + off]</code>.
   *
   * @param img the Image from which to grab pixels
   * @param x the x coordinate, relative to <code>img</code>'s
   * top-left corner, of the grab rectangle's top-left pixel
   * @param y the y coordinate, relative to <code>img</code>'s
   * top-left corner, of the grab rectangle's top-left pixel
   * @param w the width of the grab rectangle, in pixels
   * @param h the height of the grab rectangle, in pixels
   * @param pix the array in which to store grabbed RGB pixel data
   * @param off the offset into the <code>pix</code> array at which to
   * start storing RGB data
   * @param scansize a set of <code>scansize</code> consecutive
   * elements in the <code>pix</code> array represents one row of
   * pixels in the grab rectangle
   */
  public PixelGrabber(Image img, int x, int y, int w, int h,
		      int pix[], int off, int scansize)
  {
    this (img.getSource(), x, y, w, h, pix, off, scansize);
  }

  /**
   * Construct a PixelGrabber that will retrieve RGB data from a given
   * ImageProducer.
   *
   * The RGB data will be retrieved from a rectangular region
   * <code>(x, y, w, h)</code> within the image produced by
   * <code>ip</code>.  The data will be stored in the provided
   * <code>pix</code> array, which must have been initialized to a
   * size of at least <code>w * h</code>.  The data for a pixel (m, n)
   * in the grab rectangle will be stored at
   * <code>pix[(n - y) * scansize + (m - x) + off]</code>.
   *
   * @param ip the ImageProducer from which to grab pixels
   * @param x the x coordinate of the grab rectangle's top-left pixel,
   * specified relative to the top-left corner of the image produced
   * by <code>ip</code>
   * @param y the y coordinate of the grab rectangle's top-left pixel,
   * specified relative to the top-left corner of the image produced
   * by <code>ip</code>
   * @param w the width of the grab rectangle, in pixels
   * @param h the height of the grab rectangle, in pixels
   * @param pix the array in which to store grabbed RGB pixel data
   * @param off the offset into the <code>pix</code> array at which to
   * start storing RGB data
   * @param scansize a set of <code>scansize</code> consecutive
   * elements in the <code>pix</code> array represents one row of
   * pixels in the grab rectangle
   */
  public PixelGrabber(ImageProducer ip, int x, int y, int w, int h,
		      int pix[], int off, int scansize)
  {
    this.ip = ip;
    this.x = x;
    this.y = y;
    this.width = w;
    this.height = h;
    this.offset = off;
    this.scansize = scansize;

    int_pixel_buffer = pix;
    // Initialize the byte array in case ip sends us byte-formatted
    // pixel data.
    byte_pixel_buffer = new byte[pix.length * 4];
  }

  /**
   * Construct a PixelGrabber that will retrieve data from a given
   * Image.
   *
   * The RGB data will be retrieved from a rectangular region
   * <code>(x, y, w, h)</code> within the image.  The data will be
   * stored in an internal array which can be accessed by calling
   * <code>getPixels</code>.  The data for a pixel (m, n) in the grab
   * rectangle will be stored in the returned array at index
   * <code>(n - y) * scansize + (m - x) + off</code>.
   * If forceRGB is false, then the returned data will be not be
   * converted to RGB from its format in <code>img</code>.
   *
   * If <code>w</code> is negative, the width of the grab region will
   * be from x to the right edge of the image.  Likewise, if
   * <code>h</code> is negative, the height of the grab region will be
   * from y to the bottom edge of the image.
   *
   * @param img the Image from which to grab pixels
   * @param x the x coordinate, relative to <code>img</code>'s
   * top-left corner, of the grab rectangle's top-left pixel
   * @param y the y coordinate, relative to <code>img</code>'s
   * top-left corner, of the grab rectangle's top-left pixel
   * @param w the width of the grab rectangle, in pixels
   * @param h the height of the grab rectangle, in pixels
   * @param forceRGB true to force conversion of the rectangular
   * region's pixel data to RGB
   */
  public PixelGrabber(Image img,
		      int x, int y,
		      int w, int h,
		      boolean forceRGB)
  {
    this.ip = img.getSource();
    this.x = x;
    this.y = y;
    width = w;
    height = h;
    // If width or height is negative, postpone pixel buffer
    // initialization until setDimensions is called back by ip.
    if (width >= 0 && height >= 0)
      {
	int_pixel_buffer = new int[width * height];
	byte_pixel_buffer = new byte[width * height];
      }
    this.forceRGB = forceRGB;
  }

  /**
   * Start grabbing pixels.
   *
   * Spawns an image production thread that calls back to this
   * PixelGrabber's ImageConsumer methods.
   */
  public synchronized void startGrabbing()
  {
    // Make sure we're not already grabbing.
    if (grabbing == false)
      {
	grabbing = true;
	grabberThread = new Thread ()
	  {
	    public void run ()
	    {
	      ip.startProduction (PixelGrabber.this);
	    }
	  };
	grabberThread.start ();
      }
  }

  /**
   * Abort pixel grabbing.
   */
  public synchronized void abortGrabbing()
  {
    if (grabbing)
      {
	// Interrupt the grabbing thread.
        Thread moribund = grabberThread;
        grabberThread = null;
        moribund.interrupt();

	imageComplete (ImageConsumer.IMAGEABORTED);
      }
  }

  /**
   * Have our Image or ImageProducer start sending us pixels via our
   * ImageConsumer methods and wait for all pixels in the grab
   * rectangle to be delivered.
   *
   * @return true if successful, false on abort or error
   *
   * @throws InterruptedException if interrupted by another thread.
   */
  public synchronized boolean grabPixels() throws InterruptedException
  {
    return grabPixels(0);
  }

  /**
   * grabPixels's behavior depends on the value of <code>ms</code>.
   *
   * If ms < 0, return true if all pixels from the source image have
   * been delivered, false otherwise.  Do not wait.
   *
   * If ms >= 0 then we request that our Image or ImageProducer start
   * delivering pixels to us via our ImageConsumer methods.
   *
   * If ms > 0, wait at most <code>ms</code> milliseconds for
   * delivery of all pixels within the grab rectangle.
   *
   * If ms == 0, wait until all pixels have been delivered.
   *
   * @return true if all pixels from the source image have been
   * delivered, false otherwise
   *
   * @throws InterruptedException if this thread is interrupted while
   * we are waiting for pixels to be delivered
   */
  public synchronized boolean grabPixels(long ms) throws InterruptedException
  {
    if (ms < 0)
      return ((observerStatus & (ImageObserver.FRAMEBITS
				 | ImageObserver.ALLBITS)) != 0);

    // Spawn a new ImageProducer thread to send us the image data via
    // our ImageConsumer methods.
    startGrabbing();

    if (ms > 0)
      {
	long stop_time = System.currentTimeMillis() + ms;
	long time_remaining;
	while (grabbing)
	  {
	    time_remaining = stop_time - System.currentTimeMillis();
	    if (time_remaining <= 0)
	      break;
	    wait (time_remaining);
	  }
	abortGrabbing ();
      }
    else
      wait ();

    // If consumerStatus is non-zero then the image is done loading or
    // an error has occurred.
    if (consumerStatus != 0)
      return setObserverStatus ();

    return ((observerStatus & (ImageObserver.FRAMEBITS
			       | ImageObserver.ALLBITS)) != 0);
  }

  // Set observer status flags based on the current consumer status
  // flags.  Return true if the consumer flags indicate that the
  // image was loaded successfully, or false otherwise.
  private synchronized boolean setObserverStatus ()
  {
    boolean retval = false;

    if ((consumerStatus & IMAGEERROR) != 0)
      observerStatus |= ImageObserver.ERROR;

    if ((consumerStatus & IMAGEABORTED) != 0)
      observerStatus |= ImageObserver.ABORT;

    if ((consumerStatus & STATICIMAGEDONE) != 0)
      {
	observerStatus |= ImageObserver.ALLBITS;
	retval = true;
      }

    if ((consumerStatus & SINGLEFRAMEDONE) != 0)
      {
	observerStatus |= ImageObserver.FRAMEBITS;
	retval = true;
      }

    return retval;
  }

  /**
   * @return the status of the pixel grabbing thread, represented by a
   * bitwise OR of ImageObserver flags
   */
  public synchronized int getStatus()
  {
    return observerStatus;
  }

  /**
   * @return the width of the grab rectangle in pixels, or a negative
   * number if the ImageProducer has not yet called our setDimensions
   * method
   */
  public synchronized int getWidth()
  {
    return width;
  }

  /**
   * @return the height of the grab rectangle in pixels, or a negative
   * number if the ImageProducer has not yet called our setDimensions
   * method
   */
  public synchronized int getHeight()
  {
    return height;
  }

  /**
   * @return a byte array of pixel data if ImageProducer delivered
   * pixel data using the byte[] variant of setPixels, or an int array
   * otherwise
   */
  public synchronized Object getPixels()
  {
    if (ints_delivered)
      return int_pixel_buffer;
    else if (bytes_delivered)
      return byte_pixel_buffer;
    else
      return null;
  }

  /**
   * @return the ColorModel currently being used for the majority of
   * pixel data conversions
   */
  public synchronized ColorModel getColorModel()
  {
    return model;
  }

  /**
   * Our <code>ImageProducer</code> calls this method to indicate the
   * size of the image being produced.
   *
   * setDimensions is an ImageConsumer method.  None of PixelGrabber's
   * ImageConsumer methods should be called by code that instantiates
   * a PixelGrabber.  They are only made public so they can be called
   * by the PixelGrabber's ImageProducer.
   * 
   * @param width the width of the image
   * @param height the height of the image
   */
  public synchronized void setDimensions(int width, int height)
  {
    // Our width wasn't set when we were constructed.  Set our width
    // so that the grab region includes all pixels from x to the right
    // edge of the source image.
    if (this.width < 0)
      this.width = width - x;

    // Our height wasn't set when we were constructed.  Set our height
    // so that the grab region includes all pixels from y to the
    // bottom edge of the source image.
    if (this.height < 0)
      this.height = height - y;

    if (scansize < 0)
      scansize = this.width;

    if (int_pixel_buffer == null)
      int_pixel_buffer = new int[this.width * this.height];

    if (byte_pixel_buffer == null)
      byte_pixel_buffer = new byte[this.width * this.height];
  }

  /**
   * Our <code>ImageProducer</code> may call this method to send us a
   * list of its image's properties.
   *
   * setProperties is an ImageConsumer method.  None of PixelGrabber's
   * ImageConsumer methods should be called by code that instantiates
   * a PixelGrabber.  They are only made public so they can be called
   * by the PixelGrabber's ImageProducer.
   *
   * @param props a list of properties associated with the image being
   * produced
   */
  public synchronized void setProperties(Hashtable props)
  {
    this.props = props;
  }

  /**
   * Our ImageProducer will call <code>setColorModel</code> to
   * indicate the model used by the majority of calls to
   * <code>setPixels</code>.  Each call to <code>setPixels</code>
   * could however indicate a different <code>ColorModel</code>.
   *
   * setColorModel is an ImageConsumer method.  None of PixelGrabber's
   * ImageConsumer methods should be called by code that instantiates
   * a PixelGrabber.  They are only made public so they can be called
   * by the PixelGrabber's ImageProducer.
   *
   * @param model the color model to be used most often by setPixels
   *
   * @see ColorModel
   */
  public synchronized void setColorModel(ColorModel model)
  {
    this.model = model;
  }

  /**
   * Our <code>ImageProducer</code> may call this method with a
   * bit mask of hints from any of <code>RANDOMPIXELORDER</code>,
   * <code>TOPDOWNLEFTRIGHT</code>, <code>COMPLETESCANLINES</code>,
   * <code>SINGLEPASS</code>, <code>SINGLEFRAME</code>.
   * 
   * setHints is an ImageConsumer method.  None of PixelGrabber's
   * ImageConsumer methods should be called by code that instantiates
   * a PixelGrabber.  They are only made public so they can be called
   * by the PixelGrabber's ImageProducer.
   *
   * @param flags a bit mask of hints
   */
  public synchronized void setHints(int flags)
  {
    hints = flags;
  }

  /**
   * Our ImageProducer calls setPixels to deliver a subset of its
   * pixels.
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
  public synchronized void setPixels(int x, int y, int w, int h, 
				     ColorModel model, byte[] pixels,
				     int offset, int scansize)
  {
    ColorModel currentModel;
    if (model != null)
      currentModel = model;
    else
      currentModel = this.model;

    for(int yp = y; yp < (y + h); yp++)
      {
	for(int xp = x; xp < (x + w); xp++)
	  {
	    // Check if the coordinates (xp, yp) are within the
	    // pixel block that we are grabbing.
	    if(xp >= this.x
	       && yp >= this.y
	       && xp < (this.x + this.width)
	       && yp < (this.y + this.height))
	      {
		int i = (yp - this.y) * this.scansize + (xp - this.x) + this.offset;
		int p = (yp - y) * scansize + (xp - x) + offset;
		if (forceRGB)
		  {
		    ints_delivered = true;

		    assert (i >= 0 && i < int_pixel_buffer.length);
		    assert (p >= 0 && p < pixels.length);
		    int_pixel_buffer[i] = currentModel.getRGB (pixels[p]);
		  }
		else
		  {
		    bytes_delivered = true;

		    assert (i >= 0 && i < byte_pixel_buffer.length);
		    assert (p >= 0 && p < pixels.length);
		    byte_pixel_buffer[i] = pixels[p];
		  }
	      }
	  }
      }
  }

  /**
   * Our ImageProducer calls setPixels to deliver a subset of its
   * pixels.
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
  public synchronized void setPixels(int x, int y, int w, int h, 
				     ColorModel model, int[] pixels,
				     int offset, int scansize)
  {
    ColorModel currentModel;
    if (model != null)
      currentModel = model;
    else
      currentModel = this.model;

    ints_delivered = true;

    for(int yp = y; yp < (y + h); yp++)
      {
	for(int xp = x; xp < (x + w); xp++)
	  {
	    // Check if the coordinates (xp, yp) are within the
	    // pixel block that we are grabbing.
	    if(xp >= this.x
	       && yp >= this.y
	       && xp < (this.x + this.width)
	       && yp < (this.y + this.height))
	      {
		int i = (yp - this.y) * this.scansize + (xp - this.x) + this.offset;
		int p = (yp - y) * scansize + (xp - x) + offset;
		assert (i >= 0 && i < int_pixel_buffer.length);
		assert (p >= 0 && p < pixels.length);
		if (forceRGB)
		  int_pixel_buffer[i] = currentModel.getRGB (pixels[p]);
		else
		  int_pixel_buffer[i] = pixels[p];
	      }
	  }
      }
  }

  /**
   * Our <code>ImageProducer</code> calls this method to inform us
   * that a single frame or the entire image is complete.  The method
   * is also used to inform us of an error in loading or producing the
   * image.
   *
   * @param status the status of image production, represented by a
   * bitwise OR of ImageConsumer flags
   */
  public synchronized void imageComplete(int status)
  {
    consumerStatus = status;
    setObserverStatus ();
    grabbing = false;
    ip.removeConsumer (this);

    notifyAll ();
  }

  /**
   * @return the return value of getStatus
   *
   * @specnote The newer getStatus should be used in place of status.
   */
  public synchronized int status()
  {
    return getStatus();
  }
}
