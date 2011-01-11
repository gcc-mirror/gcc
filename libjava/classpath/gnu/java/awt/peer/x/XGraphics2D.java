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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.Transparency;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.peer.FontPeer;
import java.util.HashMap;
import java.util.WeakHashMap;

import gnu.java.awt.image.AsyncImage;
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
   * When this property is set to true, then images are always rendered as
   * opaque images, ignoring their translucence. This is intended for
   * debugging and demonstration purposes.
   */
  private static final boolean RENDER_OPAQUE =
    Boolean.getBoolean("escherpeer.renderopaque");

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

  @Override
  protected void rawDrawLine(int x0, int y0, int x1, int y1)
  {
    xdrawable.segment(xgc, x0, y0, x1, y1);
  }

  @Override
  protected void rawDrawRect(int x, int y, int w, int h)
  {
    xdrawable.rectangle(xgc, x, y, w, h, false);
  }

  @Override
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

  @Override
  public void renderScanline(int y, ScanlineCoverage c)
  {
    if (y >= xdrawable.height)
      return;

    // TODO: Handle Composite and Paint.
    ScanlineCoverage.Iterator iter = c.iterate();
    int coverageAlpha = 0;
    int maxCoverage = c.getMaxCoverage();
    while (iter.hasNext())
      {
        ScanlineCoverage.Range range = iter.next();

        coverageAlpha = range.getCoverage();
        int x0 = range.getXPos();
        int l = range.getLength();
        if (coverageAlpha == c.getMaxCoverage())
          {
            // Simply paint the current color over the existing pixels.
            xdrawable.fill_rectangle(xgc, x0, y, l, 1);
          }
        else if (coverageAlpha > 0)
          {
            // Composite the current color with the existing pixels.
            int x1 = x0 + l;
            x0 = Math.min(Math.max(0, x0), xdrawable.width - 1);
            x1 = Math.min(Math.max(0, x1), xdrawable.width - 1);
            if ((x1 - x0) < 1)
              continue;
            l = x1 - x0;
            gnu.x11.image.ZPixmap existing = (ZPixmap)
            xdrawable.image(x0, y, l, 1, 0xFFFFFFFF,
                            gnu.x11.image.Image.Format.ZPIXMAP);
            for (int x = 0; x < l; x++)
              {
                Color col = getColor();
                if (col == null)
                  {
                    col = Color.BLACK;
                  }
                int red = col.getRed();
                int green = col.getGreen();
                int blue = col.getBlue();
                int redOut = existing.get_red(x, 0);
                int greenOut = existing.get_green(x, 0);
                int blueOut = existing.get_blue(x, 0);
                int outAlpha = maxCoverage - coverageAlpha;
                redOut = redOut * outAlpha + red * coverageAlpha;
                redOut = redOut / maxCoverage;
                greenOut = greenOut * outAlpha + green * coverageAlpha;
                greenOut = greenOut / maxCoverage;
                blueOut = blueOut * outAlpha + blue * coverageAlpha;
                blueOut = blueOut / maxCoverage;
                existing.set(x, 0, redOut, greenOut, blueOut);
              }
            xdrawable.put_image(xgc, existing, x0, y);
          }
      }
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
        // TODO: Optimize for different standard bit-depths.
        Color c = (Color) p;
       /* XToolkit tk = (XToolkit) Toolkit.getDefaultToolkit();
        HashMap colorMap = tk.colorMap;
        gnu.x11.Color col = (gnu.x11.Color) colorMap.get(c);
        if (col == null)
          {
            Colormap map = xdrawable.display.default_colormap;
            col = map.alloc_color (c.getRed() * 256,
                                   c.getGreen() * 256,
                                   c.getBlue() * 256);
            colorMap.put(c, col);
          }*/
        //xgc.set_foreground(col);

        xgc.set_foreground(c.getRGB());
        foreground = c;
      }
  }

  protected void fillShape(Shape s, boolean isFont)
  {
    synchronized (xdrawable.display) {
      super.fillShape(s, isFont);
    }
  }

  private static WeakHashMap<Image,ZPixmap> imageCache = new WeakHashMap<Image,ZPixmap>();

  protected boolean rawDrawImage(Image image, int x, int y, ImageObserver obs)
  {
    image = unwrap(image);
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
    else if (image instanceof BufferedImage)
      {
        BufferedImage bi = (BufferedImage) image;
        DataBuffer db = bi.getRaster().getDataBuffer();
        if (db instanceof ZPixmapDataBuffer)
          {
            ZPixmapDataBuffer zpmdb = (ZPixmapDataBuffer) db;
            ZPixmap zpixmap = zpmdb.getZPixmap();
            xdrawable.put_image(xgc, zpixmap, x, y);
            ret = true;
          }
        else
          {
            int transparency = bi.getTransparency();
            int w = bi.getWidth();
            int h = bi.getHeight();
            if (imageCache.containsKey(image))
              {
                ZPixmap zpixmap = imageCache.get(image);
                xdrawable.put_image(xgc, zpixmap, x, y);
              }
            else if (transparency == Transparency.OPAQUE || RENDER_OPAQUE)
              {
                XGraphicsDevice gd = XToolkit.getDefaultDevice();
                ZPixmap zpixmap = new ZPixmap(gd.getDisplay(), w, h);
                for (int yy = 0; yy < h; yy++)
                  {
                    for (int xx = 0; xx < w; xx++)
                      {
                        int rgb = bi.getRGB(xx, yy);
                        zpixmap.set(xx, yy, rgb);
                      }
                  }
                xdrawable.put_image(xgc, zpixmap, x, y);
                imageCache.put(image, zpixmap);
              } else {

                // TODO optimize reusing the rectangles
                Rectangle source =
                  new Rectangle(0, 0, xdrawable.width, xdrawable.height);
                Rectangle target = new Rectangle(x, y, w, h);

                Rectangle destination = source.intersection(target);

                x = destination.x;
                y = destination.y;
                w = destination.width;
                h = destination.height;

                ZPixmap zpixmap =
                  (ZPixmap) xdrawable.image(x, y, w, h,
                                            0xffffffff,
                                            gnu.x11.image.Image.Format.ZPIXMAP);
                for (int yy = 0; yy < h; yy++)
                  {
                    for (int xx = 0; xx < w; xx++)
                      {
                        int rgb = bi.getRGB(xx, yy);
                        int alpha = 0xff & (rgb >> 24);
                        if (alpha == 0)
                          {
                            // Completely translucent.
                            rgb = zpixmap.get_red(xx, yy) << 16
                                  | zpixmap.get_green(xx, yy) << 8
                                  | zpixmap.get_blue(xx, yy);
                          }
                        else if (alpha < 255)
                          {
                            // Composite pixels.
                            int red = 0xff & (rgb >> 16);
                            red = red * alpha
                                     + (255 - alpha) * zpixmap.get_red(xx, yy);
                            red = red / 255;
                            int green = 0xff & (rgb >> 8);
                            green = green * alpha
                                   + (255 - alpha) * zpixmap.get_green(xx, yy);
                            green = green / 255;
                            int blue = 0xff & rgb;
                            blue = blue * alpha
                                    + (255 - alpha) * zpixmap.get_blue(xx, yy);
                            blue = blue / 255;
                            rgb = red << 16 | green << 8 | blue;
                          }
                        // else keep rgb value from source image.

                        zpixmap.set(xx, yy, rgb);
                      }
                  }
                xdrawable.put_image(xgc, zpixmap, x, y);
                // We can't cache prerendered translucent images, because
                // we never know how the background changes.
              }
            ret = true;
          }
      }
    else
      {
        ret = super.rawDrawImage(image, x, y, obs);
      }
    return ret;
  }

  public void setFont(Font f)
  {
    super.setFont(f);
    FontPeer p = getFont().getPeer();
    if (p instanceof XFontPeer)
      {
        XFontPeer xFontPeer = (XFontPeer) p;
        xgc.set_font(xFontPeer.getXFont());
      }
  }

  public void drawString(String s, int x, int y)
  {
    FontPeer p = getFont().getPeer();
    if (p instanceof XFontPeer)
      {
        int tx = (int) transform.getTranslateX();
        int ty = (int) transform.getTranslateY();
        xdrawable.text(xgc, x + tx, y + ty, s);
      }
    else
      {
        super.drawString(s, x, y);
      }
  }

  /**
   * Extracts an image instance out of an AsyncImage. If the image isn't
   * an AsyncImage, then the original instance is returned.
   *
   * @param im the image
   *
   * @return the image to render
   */
  private Image unwrap(Image im)
  {
    Image image = im;
    if (image instanceof AsyncImage)
      {
        AsyncImage aIm = (AsyncImage) image;
        image = aIm.getRealImage();
      }
    return image;
  }

}
