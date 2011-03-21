/* XImage.java -- Image impl for X Pixmaps
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


package gnu.java.awt.peer.x;

import gnu.x11.Pixmap;
import gnu.x11.image.ZPixmap;

import java.awt.Graphics;
import java.awt.GraphicsEnvironment;
import java.awt.Image;

import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;

import java.util.Hashtable;
import java.util.Vector;

public class XImage
  extends Image
{

  Pixmap pixmap;

  private Hashtable properties;

  XImage(int w, int h)
  {
    GraphicsEnvironment env =
      GraphicsEnvironment.getLocalGraphicsEnvironment();
    XGraphicsDevice dev = (XGraphicsDevice) env.getDefaultScreenDevice();
    pixmap = new Pixmap(dev.getDisplay(), w, h);
  }

  public int getWidth(ImageObserver observer)
  {
    return pixmap.width;
  }

  public int getHeight(ImageObserver observer)
  {
    return pixmap.height;
  }

  public ImageProducer getSource()
  {
    return new XImageProducer();
  }

  /**
   * Creates an XGraphics for drawing on this XImage.
   *
   * @return an XGraphics for drawing on this XImage
   */
  public Graphics getGraphics()
  {
    XGraphics2D g = new XGraphics2D(pixmap);
    return g;
  }

  public Object getProperty(String name, ImageObserver observer)
  {
    Object val = null;
    if (properties != null)
      val = properties.get(val);
    return val;
  }

  public void flush()
  {
    // TODO: Implement this.
    throw new UnsupportedOperationException("Not yet implemented.");
  }

  protected void finalize()
  {
    pixmap.free();
  }

  protected class XImageProducer implements ImageProducer
  {
    private Vector<ImageConsumer> consumers = new Vector<ImageConsumer>();

    public void addConsumer(ImageConsumer ic)
    {
      if (ic != null && !isConsumer(ic))
        this.consumers.add(ic);
    }

    public boolean isConsumer(ImageConsumer ic)
    {
      return this.consumers.contains(ic);
    }

    public void removeConsumer(ImageConsumer ic)
    {
      if (ic != null)
        this.consumers.remove(ic);
    }

    public void requestTopDownLeftRightResend(ImageConsumer ic)
    {
      /* just ignore the call */
    }

    public void startProduction(ImageConsumer ic)
    {
      this.addConsumer(ic);

      for (ImageConsumer consumer : this.consumers)
        {
          int width = XImage.this.getWidth(null);
          int height = XImage.this.getHeight(null);

          XGraphics2D graphics = (XGraphics2D) getGraphics();
          ColorModel model = graphics.getColorModel();
          graphics.dispose();

          ZPixmap zpixmap = (ZPixmap)
            XImage.this.pixmap.image(0, 0, width, height,
                                     0xffffffff,
                                     gnu.x11.image.Image.Format.ZPIXMAP);

          int size = zpixmap.get_data_length();
          System.out.println("size: " + size + ", w = " + width + ", h = " + height);

          int [] pixel = new int[size];
          for (int i = 0; i < size; i++)
            pixel[i] = zpixmap.get_data_element(i);

          consumer.setHints(ImageConsumer.SINGLEPASS);

          consumer.setDimensions(width, height);
          consumer.setPixels(0, 0, width, height, model, pixel, 0, width);
          consumer.imageComplete(ImageConsumer.STATICIMAGEDONE);
        }

      System.out.println("done!");
    }
  }
}
