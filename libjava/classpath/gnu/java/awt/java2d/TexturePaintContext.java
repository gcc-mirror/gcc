/* TexturePaintContext.java -- PaintContext impl for TexturePaint
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

import java.awt.AWTError;
import java.awt.PaintContext;
import java.awt.Rectangle;
import java.awt.TexturePaint;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;

/**
 * A {@link PaintContext} implementation for {@link TexturePaint}, done in
 * pure Java.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class TexturePaintContext
  implements PaintContext
{

  /**
   * The TexturePaint object.
   */
  private BufferedImage image;

  /**
   * The Raster that holds the texture.
   */
  private WritableRaster paintRaster;

  /**
   * The transform from userspace into device space.
   */
  private AffineTransform transform;

  /**
   * Creates a new TexturePaintContext for the specified TexturePaint object.
   * This initializes the Raster which is returned by
   * {@link #getRaster(int, int, int, int)}.
   *
   * @param t the texture paint object
   * @param db the bounds of the target raster in device space
   * @param ub the bounds of the target raster in user space
   * @param xform the transformation from user space to device space
   */
  public TexturePaintContext(TexturePaint t, Rectangle db,
                             Rectangle2D ub, AffineTransform xform)
  {
    image = t.getImage();

    try
      {
        // Prepare transform for mapping from device space into image space.
        // In order to achieve this we take the transform for userspace->
        // devicespace, append the correct scale and translation according
        // to the anchor (which gives us the image->userspace->devicespace
        // transform), and invert that (which gives use the device->user->image
        // transform).
        Rectangle2D anchor = t.getAnchorRect();
        BufferedImage image = t.getImage();
        double scaleX = anchor.getWidth() / image.getWidth();
        double scaleY = anchor.getHeight() / image.getHeight();
        transform = (AffineTransform) xform.clone();
        transform.scale(scaleX, scaleY);
        transform.translate(-anchor.getMinX(), -anchor.getMinY());
        transform = transform.createInverse();
      }
    catch (NoninvertibleTransformException ex)
      {
        AWTError err =
          new AWTError("Unexpected NoninvertibleTransformException");
        err.initCause(ex);
        throw err;
      }
  }

  /**
   * Disposes the PaintContext. Nothing is to do here, since we don't use
   * any native resources in that implementation.
   */
  public void dispose()
  {
    // Nothing to do here.
  }

  /**
   * Returns the color model of this PaintContext. This implementation returnes
   * the color model used by the BufferedImage in the TexturePaint object,
   * this avoids costly color model transformations (at least at this point).
   *
   * @return the color model of this PaintContext
   */
  public ColorModel getColorModel()
  {
    return image.getColorModel();
  }

  /**
   * Returns the Raster that is used for painting.
   *
   * @param x1 the x location of the raster inside the user bounds of this paint
   *        context
   * @param y1 the y location of the raster inside the user bounds of this paint
   *        context
   * @param w the width
   * @param h the height
   *
   * @return the Raster that is used for painting
   * 
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
    int width = source.getWidth();
    int minY = source.getMinY();
    int height = source.getHeight();
    Object pixel = null;
    for (int y = y1; y < y2; y++)
      {
        for (int x = x1; x < x2; x++)
          {
            // Transform the coordinates from device space into image space.
            src[0] = x;
            src[1] = y;
            transform.transform(src, 0, dest, 0, 1);
            int dx = (int) dest[0];
            int dy = (int) dest[1];

            // The modulo operation gives us the replication effect.
            dx = ((dx - minX) % width) + minX;  
            dy = ((dy - minY) % height) + minY;
            
            // Handle possible negative values (replicating above the top-left)
            if (dx < 0)
              dx += width;
            if (dy < 0)
              dy += height;

            // Copy the pixel.
            pixel = source.getDataElements(dx, dy, pixel);
            paintRaster.setDataElements(x - x1, y - y1, pixel);
          }
      }
    return paintRaster;
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
    if (paintRaster == null || paintRaster.getWidth() < w
        || paintRaster.getHeight() < h)
      {
        Raster s = image.getData();
        paintRaster = s.createCompatibleWritableRaster(w, h);
      }
  }
}
