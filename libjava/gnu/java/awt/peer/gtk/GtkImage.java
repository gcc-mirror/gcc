/* GtkImage.java
   Copyright (C) 1999, 2002, 2003 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.util.Hashtable;
import java.util.Vector;

public class GtkImage extends Image implements ImageConsumer
{
  int width = -1, height = -1;
  Hashtable props = null;
  boolean isLoaded = false;
  boolean isCacheable = true;
  boolean loading = false;

  Vector widthObservers = new Vector ();
  Vector heightObservers = new Vector ();
  Vector propertyObservers = new Vector ();

  ImageProducer source;
  ImageObserver observer;
  Graphics g;

  /* Variables in which we stored cached data, if possible.

     An image is cached if the following properties are true:
     1. The ColorModel passed into setColorModel is the same ColorModel
        passed to all invocations of setPixels.
     2. The image contains a single frame.

  */
  int[] pixelCache;
  ColorModel model;

  public 
  GtkImage (ImageProducer producer, Graphics g)
  {
    source = producer;
    this.g = g;

    if (source != null)
      source.addConsumer (this);
  }

  public void setObserver (ImageObserver observer)
  {
    this.observer = observer;
  }

  public synchronized int[]
  getPixelCache ()
  {
    return pixelCache;
  }

  public synchronized ColorModel
  getColorModel ()
  {
    return model;
  }

  public synchronized int 
  getWidth (ImageObserver observer)
  {
    if (width == -1)
      widthObservers.addElement (observer);
    
    return width;
  }
  
  public synchronized int 
  getHeight (ImageObserver observer)
  {
    if (height == -1)
      heightObservers.addElement (observer);
    
    return height;
  }
  
  public ImageProducer 
  getSource ()
  {
    return source;
  }

  public Graphics 
  getGraphics ()
  {
    return g;
  }
  
  public synchronized Object 
  getProperty (String name, ImageObserver observer)
  {
    if (props == null)
      {
	propertyObservers.addElement (observer);
	return null;
      }
    
    Object value = props.get (name);
    return (value == null) ? UndefinedProperty : value;
  }

  public synchronized void 
  flush ()
  {
    isLoaded = false;
    isCacheable = true;
    width = height = -1;
    props = null;
    pixelCache = null;
    model = null;

    if (source != null)
      {
	source.removeConsumer (this);
	source.addConsumer (this);
      }
  }

  public boolean
  isLoaded ()
  {
    return isLoaded;
  }

  /* ImageConsumer methods */

  public synchronized void 
  setDimensions (int width, int height)
  {
    pixelCache = new int[width*height];

    this.width = width;
    this.height = height;
    
    for (int i = 0; i < widthObservers.size (); i++)
      {
	ImageObserver io = (ImageObserver) widthObservers.elementAt (i);
	if (io != null)
	  io.imageUpdate (this, ImageObserver.WIDTH, -1, -1, width, height);
      }

    for (int i = 0; i < heightObservers.size (); i++)
      {
	ImageObserver io = (ImageObserver) heightObservers.elementAt (i);
	if (io != null)
	  io.imageUpdate (this, ImageObserver.HEIGHT, -1, -1, width, height);
      }

    if (observer != null)
      observer.imageUpdate (this,
			    (ImageObserver.WIDTH
			     | ImageObserver.HEIGHT),
			    -1, -1, width, height);
  }

  public synchronized void 
  setProperties (Hashtable props)
  {
    this.props = props;
    
    for (int i = 0; i < propertyObservers.size (); i++)
      {
	ImageObserver io = (ImageObserver) propertyObservers.elementAt (i);
	if (io != null)
	  io.imageUpdate (this, ImageObserver.PROPERTIES, -1, -1, width, height);
      }
  }

  public synchronized void 
  setColorModel (ColorModel model)
  {
    if (this.model == null || this.model.equals(model))
      this.model = model;
    else
      isCacheable = false;
  }

  public synchronized void 
  setHints (int flags)
  {
  }

  public synchronized void 
  setPixels (int x, int y, int width, int height, ColorModel cm, byte[] pixels,
	     int offset, int scansize)
  {
    setPixels (x, y, width, height, cm, convertPixels (pixels), offset,
               scansize);

    if (observer != null)
      observer.imageUpdate (this,
			    ImageObserver.SOMEBITS,
			    x, y, width, height);
  }

  public synchronized void 
  setPixels (int x, int y, int width, int height, ColorModel cm, int[] pixels,
	     int offset, int scansize)
  {
    loading = true;

    if (!isCacheable)
      return;

    if (!cm.equals(model) || pixelCache == null)
      {
	isCacheable = false;
	return;
      }

    if (scansize == width && height == 1)
      {
        // Copy contents of pixels array into pixel cache.
	System.arraycopy (pixels, offset,
			  pixelCache, y * this.width + x,
			  pixels.length - offset);
      }
    else			// skip over scansize-width for each row
      {
	for (int i = 0; i < height; i++)
	  System.arraycopy (pixels, offset + (i * scansize),
			    pixelCache, (y + i) * this.width + x,
			    width);
      }
  }

  public synchronized void 
  imageComplete (int status)
  {
    if (status == ImageConsumer.STATICIMAGEDONE && isCacheable)
      isLoaded = true;

    if (status == ImageConsumer.SINGLEFRAME)
      isCacheable = false;

    if (observer != null)
      {
	if (status == ImageConsumer.IMAGEERROR)
	  observer.imageUpdate (null,
				ImageObserver.ERROR,
				-1, -1, -1, -1);
	else
	  observer.imageUpdate (null,
				ImageObserver.ALLBITS,
				-1, -1, -1, -1);
      }

    if (source != null && status != ImageConsumer.SINGLEFRAME)
      source.removeConsumer (this);
  }

  public synchronized void
  startProduction (GtkImagePainter painter)
  {
    if (isLoaded)
      {
	painter.setDimensions (width, height);
	painter.setPixels (0, 0, width, height, model, pixelCache, 0, width);
      }
    else
      {
	if (source != null)
	  {
	    source.startProduction (painter);
	    source.removeConsumer (painter);
	  }
      }
  }

  private int[]
  convertPixels (byte[] pixels)
  {
    int ret[] = new int[pixels.length];

    for (int i = 0; i < pixels.length; i++)
      ret[i] = pixels[i];
    
    return ret;
  }

  synchronized int 
  checkImage ()
  {
    int bits = 0;

    if (width != -1)
      bits |= ImageObserver.WIDTH;
    if (height != -1)
      bits |= ImageObserver.HEIGHT;
    if (props != null)
      bits |= ImageObserver.PROPERTIES;
    if (loading)
      bits |= ImageObserver.SOMEBITS;
    if (isLoaded)
      bits |= ImageObserver.ALLBITS;

    return bits;
  }
}
