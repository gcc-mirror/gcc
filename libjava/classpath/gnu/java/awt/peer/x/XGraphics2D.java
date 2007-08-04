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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.geom.AffineTransform;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.util.HashMap;

import gnu.java.awt.java2d.AbstractGraphics2D;
import gnu.java.awt.java2d.ScanlineCoverage;
import gnu.x11.Colormap;
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

  /**
   * The current foreground color, possibly null.
   */
  private Color foreground;

  XGraphics2D(Drawable d)
  {
    super();
    xdrawable = d;
    xgc = new GC(d);
    init();
    disposed = false;
    //setClip(new Rectangle(0, 0, xdrawable.width, xdrawable.height));
  }

  protected void rawDrawLine(int x0, int y0, int x1, int y1)
  {
    xdrawable.segment(xgc, x0, y0, x1, y1);
  }

  protected void rawFillRect(int x, int y, int w, int h)
  {
    xdrawable.rectangle(xgc, x, y, w, h, true);
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

  public void renderScanline(int y, ScanlineCoverage c)
  {
    ScanlineCoverage.Iterator iter = c.iterate();
    float coverageAlpha = 0;
    int maxCoverage = c.getMaxCoverage();
    Color old = getColor();
    Color col = getColor();
    if (col == null)
      col = Color.BLACK;
    while (iter.hasNext())
      {
        ScanlineCoverage.Range range = iter.next();
        // TODO: Dumb implementation for testing.
        coverageAlpha = range.getCoverage();
        if (coverageAlpha > 0)
          {
            int red = col.getRed();
            int green = col.getGreen();
            int blue = col.getBlue();
            if (coverageAlpha < c.getMaxCoverage())
              {
                float alpha = coverageAlpha / maxCoverage;
                red = 255 - (int) ((255 - red) * alpha);
                green = 255 - (int) ((255 - green) * alpha);
                blue = 255 - (int) ((255 - blue) * alpha);
              }
            xgc.set_foreground(red << 16 | green << 8 | blue);
            int x0 = range.getXPos();
            int l = range.getLength();
            xdrawable.fill_rectangle(xgc, x0, y, l, 1);
          }
      }
    if (old != null)
      xgc.set_foreground(old.getRGB());
  }

  protected void fillScanline(int x0, int x1, int y)
  {
    xdrawable.segment(xgc, x0, y, x1, y);
  }

  protected void fillScanlineAA(int x0, int x1, int y, int alpha)
  {
    //System.err.println("fillScanlineAA: " + x0 + ", " + x1 + ", " + y + ", " + alpha);
    // FIXME: This is for testing only.
    Color c = getColor();
    setColor(new Color(255-alpha, 255-alpha, 255-alpha));
    xdrawable.segment(xgc, x0, y, x1, y);
    setColor(c);
  }

  protected void init()
  {
    super.init();
  }

  public void setPaint(Paint p)
  {
    super.setPaint(p);
    if (p instanceof Color)
      {
        Color c = (Color) p;
        XToolkit tk = (XToolkit) Toolkit.getDefaultToolkit();
        HashMap colorMap = tk.colorMap;
        gnu.x11.Color col = (gnu.x11.Color) colorMap.get(c);
        if (col == null)
          {
            Colormap map = xdrawable.display.default_colormap;
            col = map.alloc_color (c.getRed() * 256,
                                   c.getGreen() * 256,
                                   c.getBlue() * 256);
            colorMap.put(c, col);
          }
        xgc.set_foreground(col);
        foreground = c;
      }
  }

  protected void fillShape(Shape s, boolean isFont)
  {
    synchronized (xdrawable.display) {
      super.fillShape(s, isFont);
    }
  }

  protected boolean rawDrawImage(Image image, int x, int y, ImageObserver obs)
  {
    boolean ret;
    if (image instanceof XImage)
      {
        XImage xImage = (XImage) image;
        xdrawable.copy_area(xImage.pixmap, xgc, 0, 0, xImage.getWidth(obs),
                            xImage.getHeight(obs), x, y);
        ret = true;
      }
    else if (image instanceof PixmapVolatileImage)
      {
        PixmapVolatileImage pvi = (PixmapVolatileImage) image;
        xdrawable.copy_area(pvi.getPixmap(), xgc, 0, 0, pvi.getWidth(obs),
                            pvi.getHeight(obs), x, y);
        ret = true;
      }
    else
      {
        ret = super.rawDrawImage(image, x, y, obs);
      }
    return ret;
  }


}

