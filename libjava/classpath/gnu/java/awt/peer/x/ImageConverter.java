/* ImageConverter.java -- Convert arbitrary Image impl to XImage
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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.ColorModel;
import java.awt.image.ImageConsumer;
import java.util.Hashtable;

/**
 * Convert a non-XImage to an XImage.
 * 
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ImageConverter implements ImageConsumer
{

  private XImage image;
  private Graphics imageGraphics;

  public void setDimensions(int width, int height)
  {
    image = new XImage(width, height);
  }

  public void setProperties(Hashtable props)
  {
    // Ignore for now.
  }

  public void setColorModel(ColorModel model)
  {
    // Ignore for now.
  }

  public void setHints(int flags)
  {
    // Ignore for now.
  }

  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        byte[] pixels, int offset, int scansize)
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public void setPixels(int x, int y, int w, int h, ColorModel model,
                        int[] pixels, int offset, int scansize)
  {
    System.err.println("transferType: " + model.getTransferType());
    System.err.println("colorModel: " + model);
    if (imageGraphics == null)
      imageGraphics = image.getGraphics();
    int xend = x + w;
    int yend = y + h;
    for (int yy = y; yy < yend; yy++)
      {
        for (int xx = x; xx < xend; xx++)
          {
            int pixel = pixels[yy * scansize + xx + offset];
            imageGraphics.setColor(new Color(model.getRGB(pixel)));
            imageGraphics.fillRect(xx, yy, 1, 1);
          }
      }
  }

  public void imageComplete(int status)
  {
    // Nothing to do here.
  }

  XImage getXImage()
  {
    return image;
  }
}
