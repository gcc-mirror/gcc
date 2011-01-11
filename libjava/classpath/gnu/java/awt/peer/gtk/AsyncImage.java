/* AsyncImage.java -- Loads images asynchronously
   Copyright (C) 2006 Free Software Foundation, Inc.

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
import java.awt.image.ImageConsumer;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

/**
 * Supports asynchronous loading of images.
 */
public class AsyncImage
  extends Image
{

  /**
   * Returned as source as long as the image is not complete.
   */
  private class NullImageSource
    implements ImageProducer
  {
    private ArrayList<ImageConsumer> consumers;

    NullImageSource()
    {
      consumers = new ArrayList<ImageConsumer>();
    }

    public void addConsumer(ImageConsumer ic)
    {
      consumers.add(ic);
    }

    public boolean isConsumer(ImageConsumer ic)
    {
      return consumers.contains(ic);
    }

    public void removeConsumer(ImageConsumer ic)
    {
      consumers.remove(ic);
    }

    public void requestTopDownLeftRightResend(ImageConsumer ic)
    {
      startProduction(ic);
    }

    public void startProduction(ImageConsumer ic)
    {
      consumers.add(ic);
      for (int i = consumers.size() - 1; i >= 0; i--)
        {
          ImageConsumer c = (ImageConsumer) consumers.get(i);
          c.setDimensions(1, 1);
          ic.imageComplete(ImageConsumer.SINGLEFRAMEDONE);
        }
    }

  }

  /**
   * Loads the image asynchronously.
   */
  private class Loader
    implements Runnable
  {
    private URL url;
    Loader(URL u)
    {
      url = u;
    }

    public void run()
    {
      Image image;
      try
        {
          GtkImage gtkImage = new GtkImage(url);
          image = CairoSurface.getBufferedImage(gtkImage);
        }
      catch (IllegalArgumentException iae)
        {
          image = null;
        }
      realImage = GtkToolkit.imageOrError(image);
      synchronized (AsyncImage.this)
        {
          notifyObservers(ImageObserver.ALLBITS | ImageObserver.HEIGHT
                          | ImageObserver.WIDTH | ImageObserver.PROPERTIES);
          observers = null; // Not needed anymore.
        }
    }
  }

  /**
   * The real image. This is null as long as the image is not complete.
   */
  Image realImage;

  /**
   * The image observers.
   *
   * This is package private to avoid accessor methods.
   */
  HashSet<ImageObserver> observers;

  /**
   * Creates a new AsyncImage that loads from the specified URL.
   */
  AsyncImage(URL url)
  {
    observers = new HashSet<ImageObserver>();
    Loader l = new Loader(url);
    Thread t = new Thread(l);
    t.start();
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

  public int getHeight(ImageObserver observer)
  {
    addObserver(observer);
    int height = 0;
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
    Image r = realImage;
    ImageProducer source;
    if (r == null)
      source = new NullImageSource();
    else
      source = r.getSource();
    return source;
  }

  public int getWidth(ImageObserver observer)
  {
    addObserver(observer);
    int width = 0;
    Image r = realImage;
    if (r != null)
      width = r.getWidth(observer);
    return width;
  }

  void addObserver(ImageObserver obs)
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
            else
              {
                // When the image is complete, notify the observer. Dunno if
                // that's really needed, but to be sure.
                obs.imageUpdate(this, ImageObserver.WIDTH
                                | ImageObserver.HEIGHT
                                |ImageObserver.ALLBITS
                                | ImageObserver.PROPERTIES, 0, 0,
                                realImage.getWidth(null),
                                realImage.getHeight(null));
              }
          }
      }
  }

  static Image realImage(Image img, ImageObserver obs)
  {
    if (img instanceof AsyncImage)
      {
        ((AsyncImage) img).addObserver(obs);
        Image r = ((AsyncImage) img).realImage;
        if (r != null)
          img = r;
      }
    return img;
  }

  void notifyObservers(int status)
  {
    assert Thread.holdsLock(this);
    // This field gets null when image loading is complete.
    HashSet observs = observers;
    if (observs != null)
      {
        Image r = realImage;
        Iterator i = observs.iterator();
        while (i.hasNext())
          {
            ImageObserver obs = (ImageObserver) i.next();
            obs.imageUpdate(this, status, 0, 0, r.getWidth(null),
                            r.getHeight(null));
          }
      }
  }

  int checkImage(ImageObserver obs)
  {
    addObserver(obs);
    int flags = 0;
    if (realImage != null)
      flags = ImageObserver.ALLBITS | ImageObserver.WIDTH
              | ImageObserver.HEIGHT | ImageObserver.PROPERTIES;
    return flags;
  }
}
