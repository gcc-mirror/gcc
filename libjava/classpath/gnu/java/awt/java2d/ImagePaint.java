/* ImagePaint.java -- Supplies the pixels for image rendering
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


package gnu.java.awt.java2d;

import java.awt.Paint;
import java.awt.PaintContext;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Transparency;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.WritableRaster;

/**
 * This class is used as a temporary Paint object to supply the pixel values
 * for image rendering using the normal scanline conversion implementation.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class ImagePaint
  implements Paint
{

  /**
   * The PaintContext implementation for the ImagePaint.
   */
  private class ImagePaintContext
    implements PaintContext
  {

    /**
     * The target raster.
     */
    private WritableRaster target;

    /**
     * Nothing to do here.
     */
    public void dispose()
    {
      // Nothing to do here.
    }

    /**
     * Returns the color model.
     *
     * @return the color model
     */
    public ColorModel getColorModel()
    {
      return image.getColorModel();
    }

    /**
     * Supplies the pixel to be rendered.
     *
     * @see PaintContext#getRaster(int, int, int, int)
     */
    public Raster getRaster(int x1, int y1, int w, int h)
    {
      ensureRasterSize(w, h);
      int x2 = x1 + w;
      int y2 = y1 + h;
      float[] src = new float[2];
      float[] dest = new float[2];
      Raster source = image.getData();
      int minX = source.getMinX();
      int maxX = source.getWidth() + minX;
      int minY = source.getMinY();
      int maxY = source.getHeight() + minY;
      Object pixel = null;
      for (int y = y1; y < y2; y++)
        {
          for (int x = x1; x < x2; x++)
            {
              src[0] = x;
              src[1] = y;
              transform.transform(src, 0, dest, 0, 1);
              int dx = (int) dest[0];
              int dy = (int) dest[1];
              // Pixels outside the source image are not of interest, skip
              // them.
              if (dx >= minX && dx < maxX && dy >= minY && dy < maxY)
                {
                  pixel = source.getDataElements(dx, dy, pixel);
                  target.setDataElements(x - x1, y - y1, pixel);
                }
            }
        }
      return target;
    }

    /**
     * Ensures that the target raster exists and has at least the specified
     * size.
     *
     * @param w the requested target width
     * @param h the requested target height
     */
    private void ensureRasterSize(int w, int h)
    {
      if (target == null || target.getWidth() < w || target.getHeight() < h)
        {
          Raster s = image.getData();
          target = s.createCompatibleWritableRaster(w, h);
        }
    }
  }

  /**
   * The image to render.
   */
  RenderedImage image;

  /**
   * The transform from image space to device space. This is the inversed
   * transform of the concatenated
   * transform image space -> user space -> device space transform.
   */
  AffineTransform transform;

  /**
   * Creates a new ImagePaint for rendering the specified image using the
   * specified device space -> image space transform. This transform
   * is the inversed transform of the usual image space -> user space -> device
   * space transform.
   *
   * The ImagePaint will only render the image in the specified area of
   * interest (which is specified in image space).
   *
   * @param i the image to render
   * @param t the device space to user space transform
   */
  ImagePaint(RenderedImage i, AffineTransform t)
  {
    image = i;
    transform = t;
  }

  public PaintContext createContext(ColorModel cm, Rectangle deviceBounds,
                                    Rectangle2D userBounds,
                                    AffineTransform xform,
                                    RenderingHints hints)
  {
    return new ImagePaintContext();
  }

  public int getTransparency()
  {
    return Transparency.OPAQUE;
  }

}
