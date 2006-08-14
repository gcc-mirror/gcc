/* BufferedImageGraphics.java
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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.awt.image.ColorModel;
import java.awt.image.DirectColorModel;
import java.awt.image.RenderedImage;
import java.awt.image.ImageObserver;
import java.util.WeakHashMap;

/**
 * Implementation of Graphics2D on a Cairo surface.
 *
 * Simutanously maintains a CairoSurface and updates the 
 * BufferedImage from that after each drawing operation.
 */
public class BufferedImageGraphics extends CairoGraphics2D
{
  /**
   * the buffered Image.
   */
  private BufferedImage image;

  /**
   * Image size.
   */
  private int imageWidth, imageHeight;

  /**
   * The cairo surface that we actually draw on.
   */
  CairoSurface surface;

  /**
   * Cache BufferedImageGraphics surfaces.
   */
  static WeakHashMap bufferedImages = new WeakHashMap();

  /**
   * Its corresponding cairo_t.
   */
  private long cairo_t;

  /**
   * Colormodels we recognize for fast copying.
   */  
  static ColorModel rgb32 = new DirectColorModel(32, 0xFF0000, 0xFF00, 0xFF);
  static ColorModel argb32 = new DirectColorModel(32, 0xFF0000, 0xFF00, 0xFF,
						  0xFF000000);
  private boolean hasFastCM;
  private boolean hasAlpha;


  public BufferedImageGraphics(BufferedImage bi)
  {
    this.image = bi;
    imageWidth = bi.getWidth();
    imageHeight = bi.getHeight();
    if(bi.getColorModel().equals(rgb32))
      {
	hasFastCM = true;
	hasAlpha = false;
      }
    else if(bi.getColorModel().equals(argb32))
      {
	hasFastCM = true;
	hasAlpha = false;
      }
    else
      hasFastCM = false;

    // Cache surfaces.
    if( bufferedImages.get( bi ) != null )
      surface = (CairoSurface)bufferedImages.get( bi );
    else
      {
	surface = new CairoSurface( imageWidth, imageHeight );
	bufferedImages.put(bi, surface);
      }

    cairo_t = surface.newCairoContext();

    DataBuffer db = bi.getRaster().getDataBuffer();
    int[] pixels;
    // get pixels

    if(db instanceof CairoSurface)
      pixels = ((CairoSurface)db).getPixels(imageWidth * imageHeight);
    else
      {
	if( hasFastCM )
	  {
	    pixels = ((DataBufferInt)db).getData();
	    if( !hasAlpha )
	      for(int i = 0; i < pixels.length; i++)
		pixels[i] &= 0xFFFFFFFF;
	  }
	else
	  {
	    pixels = CairoGraphics2D.findSimpleIntegerArray
	      (image.getColorModel(),image.getData());
	  }
      }
    surface.setPixels( pixels );

    setup( cairo_t );
    setClip(0, 0, imageWidth, imageHeight);
  }
  
  BufferedImageGraphics(BufferedImageGraphics copyFrom)
  {
    surface = copyFrom.surface;
    cairo_t = surface.newCairoContext();
    imageWidth = copyFrom.imageWidth;
    imageHeight = copyFrom.imageHeight;
    copy( copyFrom, cairo_t );
    setClip(0, 0, surface.width, surface.height);
  }

  /**
   * Update a rectangle of the bufferedImage. This can be improved upon a lot.
   */
  private void updateBufferedImage(int x, int y, int width, int height)
  {  
    int[] pixels = surface.getPixels(imageWidth * imageHeight);

    if( x > imageWidth || y > imageHeight )
      return;
    // Clip edges.
    if( x < 0 ){ width = width + x; x = 0; }
    if( y < 0 ){ height = height + y; y = 0; }
    if( x + width > imageWidth ) 
      width = imageWidth - x;
    if( y + height > imageHeight ) 
      height = imageHeight - y;

    if( !hasFastCM )
      image.setRGB(x, y, width, height, pixels, 
		   x + y * imageWidth, imageWidth);
    else
      System.arraycopy(pixels, y * imageWidth, 
		       ((DataBufferInt)image.getRaster().getDataBuffer()).
		       getData(), y * imageWidth, height * imageWidth);
  }

  /**
   * Abstract methods.
   */  
  public Graphics create()
  {
    return new BufferedImageGraphics(this);
  }
  
  public GraphicsConfiguration getDeviceConfiguration()
  {
    return null;
  }

  protected Rectangle2D getRealBounds()
  {
    return new Rectangle2D.Double(0.0, 0.0, imageWidth, imageHeight);
  }
  
  public void copyAreaImpl(int x, int y, int width, int height, int dx, int dy)
  {
    surface.copyAreaNative(x, y, width, height, dx, dy, surface.width);
    updateBufferedImage(x + dx, y + dy, width, height);
  }

  /**
   * Overloaded methods that do actual drawing need to enter the gdk threads 
   * and also do certain things before and after.
   */
  public void draw(Shape s)
  {
    super.draw(s);
    Rectangle r = s.getBounds();
    updateBufferedImage(r.x, r.y, r.width, r.height);
  }

  public void fill(Shape s)
  {
    super.fill(s);
    Rectangle r = s.getBounds();
    updateBufferedImage(r.x, r.y, r.width, r.height);
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    super.drawRenderedImage(image, xform);
    updateBufferedImage(0, 0, imageWidth, imageHeight);
  }

  protected boolean drawImage(Image img, AffineTransform xform,
			      Color bgcolor, ImageObserver obs)
  {
    boolean rv = super.drawImage(img, xform, bgcolor, obs);
    updateBufferedImage(0, 0, imageWidth, imageHeight);
    return rv;
  }

  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    super.drawGlyphVector(gv, x, y);
    updateBufferedImage(0, 0, imageWidth, imageHeight);
  }
}

