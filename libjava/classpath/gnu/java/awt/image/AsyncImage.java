/* AsyncImage.java -- Loads images asynchronously
   Copyright (C) 2008 Free Software Foundation, Inc.

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


package gnu.java.awt.image;


import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.util.HashSet;
import java.util.Iterator;

/**
 * Supports asynchronous loading of images.
 */
public class AsyncImage
  extends Image
{

  /**
   * The image source for AsyncImages.
   */
  private class AsyncImageSource
    implements ImageProducer
  {
    /**
     * The real image source, if already present, or <code>null</code>
     * otherwise.
     */
    private ImageProducer realSource;

    public void addConsumer(ImageConsumer ic)
    {
      startProduction(ic);
    }

    public boolean isConsumer(ImageConsumer ic)
    {
      return false;
    }

    public void removeConsumer(ImageConsumer ic)
    {
      // Nothing to do here.
    }

    public void requestTopDownLeftRightResend(ImageConsumer ic)
    {
      startProduction(ic);
    }

    public void startProduction(ImageConsumer ic)
    {
      ImageProducer ip = getRealSource();
      if (ip == null)
        {
          ic.setDimensions(1, 1);
          ic.imageComplete(ImageConsumer.SINGLEFRAMEDONE);
        }
      else
        {
          ip.startProduction(ic);
        }
    }

    /**
     * Returns the real image source, if already present. Otherwise, this
     * returns <code>null</code>.
     *
     * @return the real image source, or <code>null</code> if not present
     */
    private ImageProducer getRealSource()
    {
      synchronized (AsyncImage.this)
        {
          ImageProducer source = realSource;
          if (source == null)
            {
              Image ri = realImage;
              if (ri != null)
                {
                  realSource = source = ri.getSource();
                }
            }
          return source;
        }
    }
  }

  /**
   * The real image. This is null as long as the image is not complete.
   */
  private volatile Image realImage;

  /**
   * The image observers.
   *
   * This is package private to avoid accessor methods.
   */
  HashSet<ImageObserver> observers;

  private volatile boolean complete = false;

  /**
   * Creates a new AsyncImage.
   */
  AsyncImage()
  {
    observers = new HashSet<ImageObserver>();
  }

  public void flush()
  {
    // Nothing to do here.
  }

  public Graphics getGraphics()
  {
    Image r = realImage;
    Graphics g = null;
    if (r != null)
      g = r.getGraphics(); // Should we return some dummy graphics instead?
    return g;
  }

  public boolean isComplete() {
        return complete;
  }

  public int getHeight(ImageObserver observer)
  {
    addObserver(observer);
    int height = -1;
    waitForImage(observer);
    Image r = realImage;
    if (r != null)
      height = r.getHeight(observer);
    return height;
  }

  public Object getProperty(String name, ImageObserver observer)
  {
    addObserver(observer);
    Image r = realImage;
    Object prop = null;
    if (r != null)
      prop = r.getProperty(name, observer);
    return prop;
  }

  public ImageProducer getSource()
  {
    return new AsyncImageSource();
  }

  public int getWidth(ImageObserver observer)
  {
    addObserver(observer);
    int width = -1;
    waitForImage(observer);
    Image r = realImage;
    if (r != null)
      width = r.getWidth(observer);
    return width;
  }

  public void addObserver(ImageObserver obs)
  {
    if (obs != null)
      {
        synchronized (this)
          {
            // This field gets null when image loading is complete and we don't
            // need to store any more observers.
            HashSet<ImageObserver> observs = observers;
            if (observs != null)
              {
                observs.add(obs);
              }
          }
      }
  }

  public boolean prepareImage(int w, int h, ImageObserver obs)
  {
    addObserver(obs);
    return realImage != null;
  }

  public int checkImage(int w, int h, ImageObserver obs)
  {
    addObserver(obs);
    int flags = 0;
    if (realImage != null)
      flags = ImageObserver.ALLBITS | ImageObserver.WIDTH
              | ImageObserver.HEIGHT | ImageObserver.PROPERTIES;
    return flags;
  }

  public Image getRealImage()
  {
    return realImage;
  }

  public void setRealImage(Image im)
  {
    realImage = im;
    int status = ImageObserver.HEIGHT | ImageObserver.WIDTH;
    notifyObservers(status, 0, 0, im.getWidth(null), im.getHeight(null));
  }

  public void notifyObservers(int status, int x, int y, int w, int h)
  {
    synchronized (this)
    {
      HashSet observs = observers;
      if (observs != null)
        {
          Iterator i = observs.iterator();
          while (i.hasNext())
            {
              ImageObserver obs = (ImageObserver) i.next();
              boolean complete = obs.imageUpdate(this, status, x, y, realImage.getWidth(obs), realImage.getHeight(obs));
              if (complete) // Remove completed observers.
                i.remove();
            }
        }
      if ((status & ImageObserver.ALLBITS) != 0)
        {
          complete = true;
          notifyAll();
        }
    }
  }

  /**
   * Waits for the image to be loaded completely, if the image observer
   * is <code>null</code>. Otherwise this is not necessary, because the
   * image observer can be notified about later completion.
   *
   * @param observer the image observer
   */
  public void waitForImage(ImageObserver observer)
  {
    if (!complete && observer == null)
      {
        synchronized (this)
          {
            while (! complete)
              {
                try
                  {
                    wait();
                  }
                catch (InterruptedException ex)
                  {
                    Thread.currentThread().interrupt();
                  }
              }
          }
      }
  }
}
