/* RenderableImageProducer.java --
   Copyright (C) 2002, 2006 Free Software Foundation, Inc.

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


package java.awt.image.renderable;

import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.ImageConsumer;
import java.awt.image.ImageProducer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SampleModel;
import java.util.ArrayList;
import java.util.Iterator;

public class RenderableImageProducer implements ImageProducer, Runnable
{
  private RenderableImage image;
  private RenderContext context;
  private ArrayList consumers = new ArrayList();

  public RenderableImageProducer(RenderableImage image, RenderContext context)
  {
    this.image = image;
    this.context = context;
  }

  public void setRenderContext(RenderContext context)
  {
    this.context = context;
  }

  public void addConsumer(ImageConsumer consumer)
  {
    synchronized (consumers)
      {
        if (! consumers.contains(consumer))
          consumers.add(consumer);
      }
  }

  public boolean isConsumer(ImageConsumer consumer)
  {
    synchronized (consumers)
      {
        return consumers.contains(consumer);
      }
  }

  public void removeConsumer(ImageConsumer consumer)
  {
    synchronized (consumers)
      {
        consumers.remove(consumer);
      }
  }

  public void startProduction(ImageConsumer consumer)
  {
    addConsumer(consumer);
    Thread t = new Thread(this, "RenderableImageProducerWorker");
    t.start();
  }

  public void requestTopDownLeftRightResend(ImageConsumer consumer)
  {
    // Do nothing.  The contract says we can ignore this call, so we do.
  }

  public void run()
  {
    // This isn't ideal but it avoids fail-fast problems.
    // Alternatively, we could clone 'consumers' here.
    synchronized (consumers)
      {
        RenderedImage newImage;
        if (context == null)
          newImage = image.createDefaultRendering();
        else
          newImage = image.createRendering(context);
        Raster newData = newImage.getData();
        ColorModel colorModel = newImage.getColorModel();
        if (colorModel == null)
          colorModel = ColorModel.getRGBdefault();
        SampleModel sampleModel = newData.getSampleModel();
        DataBuffer dataBuffer = newData.getDataBuffer();
        int width = newData.getWidth();
        int height = newData.getHeight();

        // Initialize the consumers.
        Iterator it = consumers.iterator();
        while (it.hasNext())
          {
            ImageConsumer target = (ImageConsumer) it.next();
            target.setHints(ImageConsumer.COMPLETESCANLINES
                            | ImageConsumer.SINGLEFRAME
                            | ImageConsumer.SINGLEPASS
                            | ImageConsumer.TOPDOWNLEFTRIGHT);
            target.setDimensions(width, height);
          }

        // Work in scan-line order.
        int[] newLine = new int[width];
        int[] bands = new int[sampleModel.getNumBands()];
        for (int y = 0; y < height; ++y)
          {
            for (int x = 0; x < width; ++x)
              {
                sampleModel.getPixel(x, y, bands, dataBuffer);
                newLine[x] = colorModel.getDataElement(bands, 0);
              }

            // Tell the consumers about the new scan line.
            it = consumers.iterator();
            while (it.hasNext())
              {
                ImageConsumer target = (ImageConsumer) it.next();
                target.setPixels(0, y, width, 1, colorModel, newLine, 0, width);
              }
          }

        // Tell the consumers that we're done.
        it = consumers.iterator();
        while (it.hasNext())
          {
            ImageConsumer target = (ImageConsumer) it.next();
            target.imageComplete(ImageConsumer.STATICIMAGEDONE);
          }
      }
  }
} // class RenderableImageProducer
