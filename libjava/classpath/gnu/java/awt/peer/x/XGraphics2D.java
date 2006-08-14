/* XGraphics2D.java -- A Java based Graphics2D impl for X
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

import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.image.ColorModel;
import java.awt.image.Raster;

import gnu.java.awt.java2d.AbstractGraphics2D;
import gnu.x11.Drawable;
import gnu.x11.GC;
import gnu.x11.image.ZPixmap;

public class XGraphics2D
  extends AbstractGraphics2D
{

  /**
   * The X Drawable to draw on.
   */
  private Drawable xdrawable;

  /**
   * The X graphics context (GC).
   */
  private GC xgc;

  /**
   * Indicates if this graphics has already been disposed.
   */
  private boolean disposed;

  XGraphics2D(Drawable d)
  {
    super();
    xdrawable = d;
    xgc = new GC(d);
    init();
    disposed = false;
    //setClip(new Rectangle(0, 0, xdrawable.width, xdrawable.height));
  }

  /**
   * Draws a pixel in the target coordinate space using the specified color.
   * 
   * @param x the x coordinate
   * @param y the y coordinate
   */
  protected void rawSetPixel(int x, int y)
  {
    xdrawable.point(xgc, x, y);
  }

//  protected void rawFillPolygon(double[] xpoints, double[] ypoints, int npoints)
//  {
//    Point[] points = new Point[npoints];
//    for (int n = 0; n < npoints; n++)
//      {
//        points[n] = new Point((int) xpoints[n], (int) ypoints[n]);
//      }
//    xdrawable.fill_poly(xgc, points, Drawable.COMPLEX, Drawable.ORIGIN);
//    xdrawable.display.flush();
//  }

  protected void rawDrawLine(int x0, int y0, int x1, int y1)
  {
    xdrawable.line(xgc, x0, y0, x1, y1);
  }

  protected void rawFillRect(int x, int y, int w, int h)
  {
    xdrawable.rectangle(xgc, x, y, w, h, true);
  }

  protected void rawSetForeground(java.awt.Color c)
  {
    if (c != null)
      xgc.set_foreground(c.getRGB());
  }

  protected void rawSetForeground(int r, int g, int b)
  {
    xgc.set_foreground( r << 16 | g << 8 | b );
  }

  /**
   * Returns the color model of this Graphics object.
   *
   * @return the color model of this Graphics object
   */
  protected ColorModel getColorModel()
  {
    return Toolkit.getDefaultToolkit().getColorModel();
  }

  /**
   * Returns the color model of the target device.
   *
   * @return the color model of the target device
   */
  protected ColorModel getDestinationColorModel()
  {
    return Toolkit.getDefaultToolkit().getColorModel();
  }

  /**
   * Returns the bounds of the target.
   *
   * @return the bounds of the target
   */
  protected Rectangle getDeviceBounds()
  {
    return new Rectangle(0, 0, xdrawable.width, xdrawable.height);
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    // FIXME: Implement this.
    throw new UnsupportedOperationException("Not yet implemented");
  }

  public void dispose()
  {
    if (!disposed)
      {
        xgc.free();
        xdrawable.display.flush();
        disposed = true;
      }
  }

  public Graphics create()
  {
    // super.create() returns a copy created by clone(), so it should
    // be a XGraphics2D.
    XGraphics2D copy = (XGraphics2D) super.create();
    copy.xgc = xgc.copy();
    return copy;
  }

//  /**
//   * Draws the specified image on the drawable at position (x,y).
//   */
//
//  public boolean drawImage(Image image, int x, int y, ImageObserver observer)
//  {
//    AffineTransform transform = getTransform();
//    int translateX = (int) transform.getTranslateX();
//    int translateY = (int) transform.getTranslateY();
//    if (image instanceof XImage)
//      {
//        XImage xim = (XImage) image;
//        Pixmap pm = xim.pixmap;
//        xdrawable.copy_area(pm, xgc, 0, 0, pm.width, pm.height,
//                            x + translateX, y + translateY);
//      }
//    else if (image instanceof BufferedImage)
//      {
//        BufferedImage bufferedImage = (BufferedImage) image;
//        Raster raster = bufferedImage.getData();
//        int w = bufferedImage.getWidth();
//        int h = bufferedImage.getHeight();
//        // Push data to X server.
//        ZPixmap zPixmap = new ZPixmap(xdrawable.display, w, h,
//                                      xdrawable.display.default_pixmap_format);
//        System.err.println("data buffer length: " + zPixmap.data.length);
//        int[] pixel = new int[4];
//        for (int tx = 0; tx < w; tx++)
//          {
//            for (int ty = 0; ty < h; ty++)
//              {
//                pixel = raster.getPixel(tx, ty, pixel);
////                System.err.print("r: " + pixel[0]);
////                System.err.print(", g: " + pixel[1]);
////                System.err.println(", b: " + pixel[2]);
//                zPixmap.set_red(tx, ty, pixel[0]);
//                zPixmap.set_green(tx, ty, pixel[1]);
//                zPixmap.set_blue(tx, ty, pixel[2]);
//              }
//          }
//        xdrawable.put_image(xgc, zPixmap, x, y);
//      }
//    else
//      {
//        throw new UnsupportedOperationException("Not yet implemented.");
//      }
//    return true;
//  }
//
  public void setClip(Shape c)
  {
    super.setClip(c);
    if (c instanceof Rectangle)
      {
        Rectangle r = (Rectangle) c;
        AffineTransform t = getTransform();
        int translateX = (int) t.getTranslateX();
        //System.err.println("translateX: " + translateX);
        int translateY = (int) t.getTranslateY();
        //System.err.println("translateY: " + translateY);
        //System.err.println("clip: " + c);
        gnu.x11.Rectangle clip = new gnu.x11.Rectangle(r.x, r.y, r.width,
                                                       r.height);
        xgc.set_clip_rectangles(translateX, translateY,
                                new gnu.x11.Rectangle[]{clip}, GC.UN_SORTED);
      }
  }

  /**
   * Notifies the backend that the raster has changed in the specified
   * rectangular area. The raster that is provided in this method is always
   * the same as the one returned in {@link #getDestinationRaster}.
   * Backends that reflect changes to this raster directly don't need to do
   * anything here.
   *
   * @param raster the updated raster, identical to the raster returned
   *        by {@link #getDestinationRaster()}
   * @param x the upper left corner of the updated region, X coordinate
   * @param y the upper lef corner of the updated region, Y coordinate
   * @param w the width of the updated region
   * @param h the height of the updated region
   */
  protected void updateRaster(Raster raster, int x, int y, int w, int h)
  {
    if (w > 0 && h > 0)
      {
        ZPixmap zPixmap = new ZPixmap(xdrawable.display, w, h,
                                      xdrawable.display.default_pixmap_format);
        int[] pixel = null;
        int x1 = x + w;
        int y1 = y + h;
        for (int tx = x; tx < x1; tx++)
          {
            for (int ty = y; ty < y1; ty++)
              {
                pixel = raster.getPixel(tx, ty, pixel);
                //System.err.println("tx: " + tx + ", ty: " + ty + ", pixel: " + pixel[0] + ", " + pixel[1] + ", " + pixel[2]);
//              System.err.print("r: " + pixel[0]);
//              System.err.print(", g: " + pixel[1]);
//              System.err.println(", b: " + pixel[2]);
                zPixmap.set_red(tx - x, ty - y, pixel[0]);
                zPixmap.set_green(tx - x, ty - y, pixel[1]);
                zPixmap.set_blue(tx - x, ty - y, pixel[2]);
              }
          }
        xdrawable.put_image(xgc, zPixmap, x, y);
      }
  }


  protected void init()
  {
    super.init();
  }
}
