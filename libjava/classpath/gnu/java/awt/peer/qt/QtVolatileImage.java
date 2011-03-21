/* QtVolatileImage.java --
   Copyright (C)  2005, 2006  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.qt;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Color;
import java.awt.Image;
import java.awt.ImageCapabilities;
import java.awt.GraphicsConfiguration;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.MemoryImageSource;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.util.Hashtable;
import java.util.WeakHashMap;

/**
 * QtVolatileImage - wraps a QImage
 *
 */
public class QtVolatileImage extends VolatileImage
{
  int width = -1, height = -1;

  /**
   * Properties.
   */
  Hashtable props;

  /**
   * Pointer to the QImage
   */
  long nativeObject;

  /*
   * The 32-bit AARRGGBB format the  uses.
   */
  static ColorModel nativeModel = new DirectColorModel(32,
                                                       0x00FF0000,
                                                       0x0000FF00,
                                                       0x000000FF,
                                                       0xFF000000);

  /**
   * Clears the image to RGBA 0
   */
  public native void clear();

  /**
   * Returns a copy of the pixel data as a java array.
   */
  private native int[] getPixels();

  /**
   * Allocates a QImage
   */
  private native void createImage();

  /**
   * HashMap of Graphics objects painting on this Image.
   */
  WeakHashMap painters;

  /**
   * Flags if this image is to be destroyed.
   */
  boolean killFlag;

  /**
   * Frees the above.
   */
  private native void freeImage();

  /**
   * Blit a QImage
   */
  public native void blit(QtImage i);
  public native void blit(QtImage i, int x, int y, int w, int h);

  /**
   * Sets the image to scaled copy of src image. hints are rendering hints.
   */
  private native void createScaledImage(QtVolatileImage src, int hints);

  /**
   * Draws the image optionally composited.
   */
  private native void drawPixels (QtGraphics gc,
                                  int bg_red, int bg_green, int bg_blue,
                                  int x, int y,
                                  boolean composite);
  /**
   * Draws the image, optionally scaled and composited.
   */
  private native void drawPixelsScaled (QtGraphics gc,
                                        int bg_red, int bg_green, int bg_blue,
                                        int x, int y, int width, int height,
                                        boolean composite);

  /**
   * Draws the image transformed.
   */
  private native void drawPixelsTransformed (QtGraphics gc, QMatrix transform);

  /**
   * Draws the image scaled flipped and optionally composited.
   */
  native void drawPixelsScaledFlipped (QtGraphics gc,
                                       int bg_red, int bg_green,
                                       int bg_blue,
                                       boolean flipX, boolean flipY,
                                       int srcX, int srcY,
                                       int srcWidth, int srcHeight,
                                       int dstX, int dstY,
                                       int dstWidth, int dstHeight,
                                       boolean composite);

  /**
   * Constructs an empty QtVolatileImage.
   */
  public QtVolatileImage (int width, int height)
  {
    this.width = width;
    this.height = height;
    props = new Hashtable();
    createImage();
    clear();
  }

  /**
   * Constructs a scaled version of the src bitmap, using Qt
   */
  private QtVolatileImage (QtVolatileImage src, int width, int height,
                           int hints)
  {
    this.width = width;
    this.height = height;
    props = new Hashtable();

    createScaledImage(src, hints);
  }


  public void finalize()
  {
    dispose();
  }

  public void dispose()
  {
    if( painters == null || painters.isEmpty() )
      freeImage();
    else
      killFlag = true; // can't destroy image yet.
    // Do so when all painters are gone.
  }

  // java.awt.Image methods ////////////////////////////////////////////////

  public int getWidth (ImageObserver observer)
  {
    return getWidth();
  }

  public int getHeight (ImageObserver observer)
  {
    return getHeight();
  }

  public Object getProperty (String name, ImageObserver observer)
  {
    Object value = props.get (name);
    return (value == null) ? UndefinedProperty : value;
  }

  /**
   * Returns the source of this image.
   */
  public ImageProducer getSource ()
  {
    return new MemoryImageSource(width, height, nativeModel, getPixels(),
                                 0, width);
  }

  void putPainter(QtImageGraphics g)
  {
    if( painters == null )
      painters = new WeakHashMap();
    painters.put( g, "dummy" );
  }

  void removePainter(QtImageGraphics g)
  {
    painters.remove( g );
    if( killFlag && painters.isEmpty() )
      freeImage();
  }

  /**
   * Creates a Graphics context for this image.
   */
  public Graphics getGraphics ()
  {
    QtImageGraphics g = new QtImageGraphics( this );
    putPainter( g );
    return g;
  }

  /**
   * Returns a scaled instance of this image.
   */
  public Image getScaledInstance(int width,
                                 int height,
                                 int hints)
  {
    if (width <= 0 || height <= 0)
      throw new IllegalArgumentException("Width and height of scaled bitmap"+
                                         "must be >= 0");

    return new QtVolatileImage(this, width, height, hints);
  }

  /**
   */
  public void flush ()
  {
    // FIXME ?
  }

  /**
   * Returns the image status, used by QtToolkit
   */
  public int checkImage (ImageObserver observer)
  {
    return ImageObserver.ALLBITS | ImageObserver.WIDTH | ImageObserver.HEIGHT;
  }

  // Drawing methods ////////////////////////////////////////////////

  /**
   * Draws an image with eventual scaling/transforming.
   */
  public boolean drawImage (QtGraphics g, QMatrix matrix,
                            ImageObserver observer)
  {
    drawPixelsTransformed (g, matrix);
    return true;
  }

  /**
   * Draws an image to the QtGraphics context, at (x,y) with optional
   * compositing with a background color.
   */
  public boolean drawImage (QtGraphics g, int x, int y,
                            Color bgcolor, ImageObserver observer)
  {
    if(bgcolor != null)
      drawPixels(g, bgcolor.getRed (), bgcolor.getGreen (),
                 bgcolor.getBlue (), x, y, true);
    else
      drawPixels(g, 0, 0, 0, x, y, false);

    return true;
  }

  /**
   * Draws an image to the QtGraphics context, at (x,y) scaled to
   * width and height, with optional compositing with a background color.
   */
  public boolean drawImage (QtGraphics g, int x, int y, int width, int height,
                            Color bgcolor, ImageObserver observer)
  {
    if(bgcolor != null)
      drawPixelsScaled(g, bgcolor.getRed (), bgcolor.getGreen (),
                       bgcolor.getBlue (), x, y, width, height, true);
    else
      drawPixelsScaled(g, 0, 0, 0, x, y, width, height, false);

    return true;
  }

  /**
   * Draws an image with eventual scaling/transforming.
   */
  public boolean drawImage (QtGraphics g, int dx1, int dy1, int dx2, int dy2,
                            int sx1, int sy1, int sx2, int sy2,
                            Color bgcolor, ImageObserver observer)
  {
    boolean flipX = (dx1 > dx2)^(sx1 > sx2);
    boolean flipY = (dy1 > dy2)^(sy1 > sy2);
    int dstWidth = Math.abs (dx2 - dx1);
    int dstHeight = Math.abs (dy2 - dy1);
    int srcWidth = Math.abs (sx2 - sx1);
    int srcHeight = Math.abs (sy2 - sy1);
    int srcX = (sx1 < sx2) ? sx1 : sx2;
    int srcY = (sy1 < sy2) ? sy1 : sy2;
    int dstX = (dx1 < dx2) ? dx1 : dx2;
    int dstY = (dy1 < dy2) ? dy1 : dy2;

    // Clipping. This requires the dst to be scaled as well,
    if (srcWidth > width)
      {
        dstWidth = (int)((double)dstWidth*((double)width/(double)srcWidth));
        srcWidth = width - srcX;
      }

    if (srcHeight > height)
      {
        dstHeight = (int)((double)dstHeight*((double)height/(double)srcHeight));
        srcHeight = height - srcY;
      }

    if (srcWidth + srcX > width)
      {
        dstWidth = (int)((double)dstWidth * (double)(width - srcX)/(double)srcWidth);
        srcWidth = width - srcX;
      }

    if (srcHeight + srcY > height)
      {
        dstHeight = (int)((double)dstHeight * (double)(width - srcY)/(double)srcHeight);
        srcHeight = height - srcY;
      }

    if ( srcWidth <= 0 || srcHeight <= 0 || dstWidth <= 0 || dstHeight <= 0)
      return true;

    if(bgcolor != null)
      drawPixelsScaledFlipped (g, bgcolor.getRed (), bgcolor.getGreen (),
                               bgcolor.getBlue (),
                               flipX, flipY,
                               srcX, srcY,
                               srcWidth, srcHeight,
                               dstX,  dstY,
                               dstWidth, dstHeight,
                               true);
    else
      drawPixelsScaledFlipped (g, 0, 0, 0, flipX, flipY,
                               srcX, srcY, srcWidth, srcHeight,
                               dstX,  dstY, dstWidth, dstHeight,
                               false);
    return true;
  }

  public native void copyArea(int x, int y, int width, int height,
                              int dx, int dy);

  //******************** VolatileImage stuff ********************

  public boolean contentsLost()
  {
    return false;
  }

  public Graphics2D createGraphics()
  {
    QtImageGraphics g = new QtImageGraphics(this);
    putPainter( g );
    return g;
  }

  public ImageCapabilities getCapabilities()
  {
    return new ImageCapabilities(false)
      {
        public boolean isTrueVolatile()
        {
          return false;
        }
      };
  }

  public int getHeight()
  {
    return height;
  }

  public BufferedImage getSnapshot()
  {
    BufferedImage bi = new BufferedImage(width, height,
                                         BufferedImage.TYPE_INT_ARGB_PRE);
    bi.setRGB( 0, 0, width, height, getPixels(), 0, width);
    return bi;
  }

  public int getWidth()
  {
    return width;
  }

  public int validate(GraphicsConfiguration gc)
  {
    return IMAGE_OK;
  }
}
