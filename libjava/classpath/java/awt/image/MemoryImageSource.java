/* MemoryImageSource.java -- Java class for providing image data
   Copyright (C) 1999, 2004, 2006,  Free Software Foundation, Inc.

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
import java.util.Vector;

/**
 * An image producer that delivers image data from an array.
 */
public class MemoryImageSource implements ImageProducer
{
  private boolean animated = false;
  private boolean fullbuffers = false;
  private int[] pixeli;
  private int width;
  private int height;
  private int offset;
  private int scansize;
  private byte[] pixelb;
  private ColorModel cm;
  private Hashtable props = new Hashtable();
  private Vector consumers = new Vector();

  /**
   * Construct an image producer that reads image data from a byte
   * array.
   *
   * @param w width of image
   * @param h height of image
   * @param cm the color model used to represent pixel values
   * @param pix a byte array of pixel values
   * @param off the offset into the array at which the first pixel is stored
   * @param scan the number of array elements that represents a single pixel row
   */
  public MemoryImageSource(int w, int h, ColorModel cm, byte[] pix, int off,
                           int scan)
  {
    this(w, h, cm, pix, off, scan, null);
  }

  /**
   * Constructs an ImageProducer from memory.
   * 
   * @param w  the image width.
   * @param h  the image height.
   * @param cm  the color model.
   * @param pix  the image data.
   * @param off  the offset to the first pixel in the array.
   * @param scan  the number of array elements from a pixel on one row to the
   *     corresponding pixel on the next row.
   * @param props  image properties (<code>null</code> permitted). 
   */
  public MemoryImageSource(int w, int h, ColorModel cm, byte[] pix, int off,
                           int scan, Hashtable<?,?> props)
  {
    width = w;
    height = h;
    this.cm = cm;
    offset = off;
    scansize = scan;
    this.props = props;
    int max = ((scansize > width) ? scansize : width);
    pixelb = pix;
  }

  /**
   * Construct an image producer that reads image data from an
   * integer array.
   *
   * @param w width of image
   * @param h height of image
   * @param cm the color model used to represent pixel values
   * @param pix an integer array of pixel values
   * @param off the offset into the array at which the first pixel is stored
   * @param scan the number of array elements that represents a single pixel row
   */
  public MemoryImageSource(int w, int h, ColorModel cm, int[] pix, int off,
                           int scan)
  {
    this(w, h, cm, pix, off, scan, null);
  }

  /**
   * Constructs an ImageProducer from memory
   * 
   * @param w  the image width.
   * @param h  the image height.
   * @param cm  the color model.
   * @param pix  the image data.
   * @param off  the offset to the first pixel in the array.
   * @param scan  the number of array elements from a pixel on one row to the
   *     corresponding pixel on the next row.
   * @param props  image properties (<code>null</code> permitted). 
   */
  public MemoryImageSource(int w, int h, ColorModel cm, int[] pix, int off,
                           int scan, Hashtable<?,?> props)
  {
    width = w;
    height = h;
    this.cm = cm;
    offset = off;
    scansize = scan;
    this.props = props;
    int max = ((scansize > width) ? scansize : width);
    pixeli = pix;
  }

  /**
   * Constructs an ImageProducer from memory using the default RGB ColorModel.
   * 
   * @param w  the image width.
   * @param h  the image height.
   * @param pix  the image data.
   * @param off  the offset to the first pixel in the array.
   * @param scan  the number of array elements from a pixel on one row to the
   *     corresponding pixel on the next row.
   * @param props  image properties (<code>null</code> permitted). 

   */
  public MemoryImageSource(int w, int h, int[] pix, int off, int scan,
                           Hashtable<?,?> props)
  {
    this(w, h, ColorModel.getRGBdefault(), pix, off, scan, props);
  }

  /**
   * Constructs an ImageProducer from memory using the default RGB ColorModel.
   * 
   * @param w  the image width.
   * @param h  the image height.
   * @param pix  the image data.
   * @param off  the offset to the first pixel in the array.
   * @param scan  the number of array elements from a pixel on one row to the
   *     corresponding pixel on the next row. 
   */
  public MemoryImageSource(int w, int h, int[] pix, int off, int scan)
  {
    this(w, h, ColorModel.getRGBdefault(), pix, off, scan, null);
  }

  /**
   * Used to register an <code>ImageConsumer</code> with this
   * <code>ImageProducer</code>.
   * 
   * @param ic  the image consumer.
   */
  public synchronized void addConsumer(ImageConsumer ic)
  {
    if (consumers.contains(ic))
      return;

    consumers.addElement(ic);
  }

  /**
   * Used to determine if the given <code>ImageConsumer</code> is
   * already registered with this <code>ImageProducer</code>.
   * 
   * @param ic  the image consumer.
   */
  public synchronized boolean isConsumer(ImageConsumer ic)
  {
    if (consumers.contains(ic))
      return true;
    return false;
  }

  /**
   * Used to remove an <code>ImageConsumer</code> from the list of
   * registered consumers for this <code>ImageProducer</code>.
   * 
   * @param ic  the image consumer.
   */
  public synchronized void removeConsumer(ImageConsumer ic)
  {
    consumers.removeElement(ic);
  }

  /**
   * Used to register an <code>ImageConsumer</code> with this
   * <code>ImageProducer</code> and then immediately start
   * reconstruction of the image data to be delivered to all
   * registered consumers.
   */
  public void startProduction(ImageConsumer ic)
  {
    if (! (consumers.contains(ic)))
      consumers.addElement(ic);

    Vector list = (Vector) consumers.clone();
    for (int i = 0; i < list.size(); i++)
      {
	ic = (ImageConsumer) list.elementAt(i);
	sendPicture(ic);
	if (animated)
	  ic.imageComplete(ImageConsumer.SINGLEFRAMEDONE);
	else
	  ic.imageComplete(ImageConsumer.STATICIMAGEDONE);
      }
  }

  /**
   * Used to register an <code>ImageConsumer</code> with this
   * <code>ImageProducer</code> and then request that this producer
   * resend the image data in the order top-down, left-right.
   * 
   * @param ic  the image consumer.
   */
  public void requestTopDownLeftRightResend(ImageConsumer ic)
  {
    startProduction(ic);
  }

  /**
   * Changes a flag to indicate whether this MemoryImageSource supports
   * animations.
   *
   * @param animated A flag indicating whether this class supports animations
   */
  public synchronized void setAnimated(boolean animated)
  {
    this.animated = animated;
  }

  /**
   * A flag to indicate whether or not to send full buffer updates when
   * sending animation. If this flag is set then full buffers are sent
   * in the newPixels methods instead of just regions.
   *
   * @param fullbuffers a flag indicating whether to send the full buffers
   */
  public synchronized void setFullBufferUpdates(boolean fullbuffers)
  {
    this.fullbuffers = fullbuffers;
  }

  /**
   * Send an animation frame to the image consumers.
   */
  public void newPixels()
  {
    if (animated == true)
      {
	ImageConsumer ic;
	Vector list = (Vector) consumers.clone();
	for (int i = 0; i < list.size(); i++)
	  {
	    ic = (ImageConsumer) list.elementAt(i);
	    sendPicture(ic);
	    ic.imageComplete(ImageConsumer.SINGLEFRAME);
	  }
      }
  }

  private void sendPicture(ImageConsumer ic)
  {
    ic.setHints(ImageConsumer.TOPDOWNLEFTRIGHT);
    if (props != null)
      ic.setProperties(props);
    ic.setDimensions(width, height);
    ic.setColorModel(cm);
    if (pixeli != null)
      ic.setPixels(0, 0, width, height, cm, pixeli, offset, scansize);
    else
      ic.setPixels(0, 0, width, height, cm, pixelb, offset, scansize);
  }

  /**
   * Send an animation frame to the image consumers containing the specified
   * pixels unless setFullBufferUpdates is set.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param w  the width.
   * @param h  the height.
   */
  public synchronized void newPixels(int x, int y, int w, int h)
  {
    if (animated == true)
      {
	if (fullbuffers)
	  newPixels();
	else
	  {
	    ImageConsumer ic;
	    Vector list = (Vector) consumers.clone();
	    for (int i = 0; i < list.size(); i++)
	      {
		ic = (ImageConsumer) list.elementAt(i);
		ic.setHints(ImageConsumer.TOPDOWNLEFTRIGHT);
		if (props != null)
		  ic.setProperties(props);
		if (pixeli != null)
		  {
		    int[] pixelbuf = new int[w * h];
		    for (int row = y; row < y + h; row++)
		      System.arraycopy(pixeli, row * scansize + x + offset,
		                       pixelbuf, 0, w * h);
		    ic.setPixels(x, y, w, h, cm, pixelbuf, 0, w);
		  }
		else
		  {
		    byte[] pixelbuf = new byte[w * h];
		    for (int row = y; row < y + h; row++)
		      System.arraycopy(pixelb, row * scansize + x + offset,
		                       pixelbuf, 0, w * h);

		    ic.setPixels(x, y, w, h, cm, pixelbuf, 0, w);
		  }
		ic.imageComplete(ImageConsumer.SINGLEFRAME);
	      }
	  }
      }
  }

  /**
   * Send an animation frame to the image consumers containing the specified
   * pixels unless setFullBufferUpdates is set.
   *
   * If framenotify is set then a notification is sent when the frame
   * is sent otherwise no status is sent.
   * 
   * @param x  the x-coordinate.
   * @param y  the y-coordinate.
   * @param w  the width.
   * @param h  the height.
   * @param framenotify  send notification?
   */
  public synchronized void newPixels(int x, int y, int w, int h,
                                     boolean framenotify)
  {
    if (animated == true)
      {
	if (fullbuffers)
	  newPixels();
	else
	  {
	    ImageConsumer ic;
	    Vector list = (Vector) consumers.clone();
	    for (int i = 0; i < list.size(); i++)
	      {
		ic = (ImageConsumer) list.elementAt(i);
		ic.setHints(ImageConsumer.TOPDOWNLEFTRIGHT);
		if (props != null)
		  ic.setProperties(props);
		if (pixeli != null)
		  {
		    int[] pixelbuf = new int[w * h];
		    for (int row = y; row < y + h; row++)
		      System.arraycopy(pixeli, row * scansize + x + offset,
		                       pixelbuf, 0, w * h);
		    ic.setPixels(x, y, w, h, cm, pixelbuf, 0, w);
		  }
		else
		  {
		    byte[] pixelbuf = new byte[w * h];
		    for (int row = y; row < y + h; row++)
		      System.arraycopy(pixelb, row * scansize + x + offset,
		                       pixelbuf, 0, w * h);
		    ic.setPixels(x, y, w, h, cm, pixelbuf, 0, w);
		  }
		if (framenotify == true)
		  ic.imageComplete(ImageConsumer.SINGLEFRAME);
	      }
	  }
      }
  }

  public synchronized void newPixels(byte[] newpix, ColorModel newmodel,
                                     int offset, int scansize)
  {
    pixeli = null;
    pixelb = newpix;
    cm = newmodel;
    this.offset = offset;
    this.scansize = scansize;
    if (animated == true)
      newPixels();
  }

  public synchronized void newPixels(int[] newpix, ColorModel newmodel,
                                     int offset, int scansize)
  {
    pixelb = null;
    pixeli = newpix;
    cm = newmodel;
    this.offset = offset;
    this.scansize = scansize;
    if (animated == true)
      newPixels();
  }
}
