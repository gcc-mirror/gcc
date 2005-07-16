/* GtkImageConsumer.java
   Copyright (C) 2005 Free Software Foundation, Inc.

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


package gnu.java.awt.peer.gtk;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.util.Hashtable;
import java.util.Vector;

/**
 * Helper class to GtkImage. Sits and gathers pixels for a GtkImage and then
 * calls GtkImage.setImage().
 *
 * @author Sven de Marothy
 */
public class GtkImageConsumer implements ImageConsumer
{
  private GtkImage target;
  private int width, height;
  private Hashtable properties;
  private int[] pixelCache = null;
  private ImageProducer source;

  public GtkImageConsumer(GtkImage target, ImageProducer source)
  {
    this.target = target;
    this.source = source;
  }

  public synchronized void imageComplete (int status)
  {
    source.removeConsumer(this);
    target.setImage(width, height, pixelCache, properties);
  }

  public synchronized void setColorModel (ColorModel model)
  {
    // This method is to inform on what the most used color model
    // in the image is, for optimization reasons. We ignore this
    // information.
  }

  public synchronized void setDimensions (int width, int height)
  {
    pixelCache = new int[width*height];

    this.width = width;
    this.height = height;
  }

  public synchronized void setHints (int flags)
  {
    // This method informs us in which order the pixels are
    // delivered, for progressive-loading support, etc. 
    // Since we wait until it's all loaded, we can ignore the hints.
  }

  public synchronized void setPixels (int x, int y, int width, int height, 
				      ColorModel cm, byte[] pixels,
				      int offset, int scansize)
  {
    setPixels (x, y, width, height, cm, convertPixels (pixels), offset,
               scansize);
  }

  public synchronized void setPixels (int x, int y, int width, int height, 
				      ColorModel cm, int[] pixels,
				      int offset, int scansize)
  {
    if (pixelCache == null)
      return; // Not sure this should ever happen.

    if (cm.equals(GtkImage.nativeModel))
      for (int i = 0; i < height; i++)
	System.arraycopy (pixels, offset + (i * scansize),
			  pixelCache, (y + i) * this.width + x,
			  width);
    else
      {
	for (int i = 0; i < height; i++)
	  for (int j = 0; j < width; j++)
	    {
	      // get in AARRGGBB and convert to AABBGGRR
	      int pix = cm.getRGB(pixels[offset + (i * scansize) + x + j]);
	      byte b = (byte)(pix & 0xFF);
	      byte r = (byte)(((pix & 0x00FF0000) >> 16) & 0xFF);
	      pix &= 0xFF00FF00;
	      pix |= ((b & 0xFF) << 16);
	      pix |= (r & 0xFF);
	      pixelCache[(y + i) * this.width + x + j] = pix;
	    }
      }
  }

  /**
   * This is an old method, no idea if it's correct.
   */
  private int[] convertPixels (byte[] pixels)
  {
    int ret[] = new int[pixels.length];

    for (int i = 0; i < pixels.length; i++)
      ret[i] = pixels[i] & 0xFF;
    
    return ret;
  }

  public synchronized void setProperties (Hashtable props)
  {
    this.properties = props;
  }
}


