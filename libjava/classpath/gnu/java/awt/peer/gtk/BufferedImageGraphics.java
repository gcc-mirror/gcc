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

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBufferInt;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.SinglePixelPackedSampleModel;
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
  private BufferedImage image, buffer;
  
  /**
   * Allows us to lock the image from updates (if we want to perform a few
   * intermediary operations on the cairo surface, then update it all at once)
   */
  private boolean locked;

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

  private boolean hasFastCM;
  private boolean hasAlpha;


  public BufferedImageGraphics(BufferedImage bi)
  {
    this.image = bi;
    imageWidth = bi.getWidth();
    imageHeight = bi.getHeight();
    locked = false;
    
    if (!(image.getSampleModel() instanceof SinglePixelPackedSampleModel))
      hasFastCM = false;
    else if(bi.getColorModel().equals(CairoSurface.cairoCM_opaque))
      {
	hasFastCM = true;
	hasAlpha = false;
      }
    else if(bi.getColorModel().equals(CairoSurface.cairoColorModel))
      {
	hasFastCM = true;
	hasAlpha = true;
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

    // Get pixels out of buffered image and set in cairo surface
    Raster raster = bi.getRaster();
    int[] pixels;

    if (hasFastCM)
      {
        SinglePixelPackedSampleModel sm = (SinglePixelPackedSampleModel)image.getSampleModel();
        int minX = image.getRaster().getSampleModelTranslateX();
        int minY = image.getRaster().getSampleModelTranslateY();

        // Pull pixels directly out of data buffer
        if(raster instanceof CairoSurface)
          pixels = ((CairoSurface)raster).getPixels(raster.getWidth() * raster.getHeight());
        else
          pixels = ((DataBufferInt)raster.getDataBuffer()).getData();

        // Discard pixels that fall outside of the image's bounds
        // (ie, this image is actually a subimage of a different image)
        if (!(sm.getScanlineStride() == imageWidth && minX == 0 && minY == 0))
          {
            int[] pixels2 = new int[imageWidth * imageHeight];
            int scanline = sm.getScanlineStride();
            
            for (int i = 0; i < imageHeight; i++)
              System.arraycopy(pixels, (i - minY) * scanline - minX, pixels2, i * imageWidth, imageWidth);
            
            pixels = pixels2;
          }

        // Fill the alpha channel as opaque if image does not have alpha
        if( !hasAlpha )
          for(int i = 0; i < pixels.length; i++)
            pixels[i] &= 0xFFFFFFFF;
      }
    else
      {
        pixels = CairoGraphics2D.findSimpleIntegerArray(image.getColorModel(),image.getData());
      }
    
    surface.setPixels( pixels );

    setup( cairo_t );
    setClip(0, 0, imageWidth, imageHeight);
  }
  
  BufferedImageGraphics(BufferedImageGraphics copyFrom)
  {
    image = copyFrom.image;
    surface = copyFrom.surface;
    cairo_t = surface.newCairoContext();
    imageWidth = copyFrom.imageWidth;
    imageHeight = copyFrom.imageHeight;
    locked = false;
    
    hasFastCM = copyFrom.hasFastCM;
    hasAlpha = copyFrom.hasAlpha;

    copy( copyFrom, cairo_t );
  }

  /**
   * Update a rectangle of the bufferedImage. This can be improved upon a lot.
   */
  private void updateBufferedImage(int x, int y, int width, int height)
  {  
    if (locked)
      return;
    
    double[] points = new double[]{x, y, width+x, height+y};
    transform.transform(points, 0, points, 0, 2);
    x = (int)points[0];
    y = (int)points[1];
    width = (int)Math.ceil(points[2] - points[0]);
    height = (int)Math.ceil(points[3] - points[1]);

    int[] pixels = surface.getPixels(imageWidth * imageHeight);

    if( x > imageWidth || y > imageHeight )
      return;
    
    // Deal with negative width/height.
    if (height < 0)
      {
        y += height;
        height = -height;
      }
    if (width < 0)
      {
        x += width;
        width = -width;
      }
    
    // Clip edges.
    if( x < 0 )
      x = 0;
    if( y < 0 )
      y = 0;
    
    if( x + width > imageWidth ) 
      width = imageWidth - x;
    if( y + height > imageHeight ) 
      height = imageHeight - y;
    
    if(!hasFastCM)
      {
        image.setRGB(x, y, width, height, pixels, 
                     x + y * imageWidth, imageWidth);
        // The setRGB method assumes (or should assume) that pixels are NOT
        // alpha-premultiplied, but Cairo stores data with premultiplication
        // (thus the pixels returned in getPixels are premultiplied).
        // This is ignored for consistency, however, since in
        // CairoGrahpics2D.drawImage we also use non-premultiplied data
      
      }
    else
      {
        int[] db = ((DataBufferInt)image.getRaster().getDataBuffer()).
                  getData();
        
        // This should not fail, as we check the image sample model when we
        // set the hasFastCM flag
        SinglePixelPackedSampleModel sm = (SinglePixelPackedSampleModel)image.getSampleModel() ;
        
        int minX = image.getRaster().getSampleModelTranslateX();
        int minY = image.getRaster().getSampleModelTranslateY();
        
        if (sm.getScanlineStride() == imageWidth && minX == 0) 
          {
            System.arraycopy(pixels, y * imageWidth, 
                             db, (y - minY) * imageWidth,
                             height * imageWidth);
          }
        else
          {
            int scanline = sm.getScanlineStride();
            for (int i = y; i < (height + y); i++)
              System.arraycopy(pixels, i * imageWidth + x, db,
                               (i - minY) * scanline + x - minX, width);
              
          }
      }
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
    // Find total bounds of shape
    Rectangle r = findStrokedBounds(s);
    if (shiftDrawCalls)
      {
        r.width++;
        r.height++;
      }
    
    // Do the drawing
    if (comp == null || comp instanceof AlphaComposite)
      {
        super.draw(s);
        updateBufferedImage(r.x, r.y, r.width, r.height);
      }
    else
      {
        createBuffer();
        
        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setStroke(this.getStroke());
        g2d.setColor(this.getColor());
        g2d.setTransform(transform);
        g2d.draw(s);
        
        drawComposite(r.getBounds2D(), null);
      }
  }

  public void fill(Shape s)
  {
    if (comp == null || comp instanceof AlphaComposite)
      {
        super.fill(s);
        Rectangle r = s.getBounds();
        updateBufferedImage(r.x, r.y, r.width, r.height);
      }
    else
      {
        createBuffer();
        
        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setPaint(this.getPaint());
        g2d.setColor(this.getColor());
        g2d.setTransform(transform);
        g2d.fill(s);
        
        drawComposite(s.getBounds2D(), null);
      }
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    if (comp == null || comp instanceof AlphaComposite)
      {
        super.drawRenderedImage(image, xform);
        updateBufferedImage(0, 0, imageWidth, imageHeight);
      }
    else
      {
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setRenderingHints(this.getRenderingHints());
        g2d.setTransform(transform);
        g2d.drawRenderedImage(image, xform);
        
        drawComposite(buffer.getRaster().getBounds(), null);
      }

  }

  protected boolean drawImage(Image img, AffineTransform xform,
			      Color bgcolor, ImageObserver obs)
  {
    if (comp == null || comp instanceof AlphaComposite)
      {
        boolean rv = super.drawImage(img, xform, bgcolor, obs);
        updateBufferedImage(0, 0, imageWidth, imageHeight);
        return rv;
      }
    else
      {
        // Get buffered image of source
        if( !(img instanceof BufferedImage) )
          {
            ImageProducer source = img.getSource();
            if (source == null)
              return false;
            img = Toolkit.getDefaultToolkit().createImage(source);
          }
        BufferedImage bImg = (BufferedImage) img;
        
        // Find translated bounds
        Point2D origin = new Point2D.Double(bImg.getMinX(), bImg.getMinY());
        Point2D pt = new Point2D.Double(bImg.getWidth() + bImg.getMinX(),
                                        bImg.getHeight() + bImg.getMinY());
        if (xform != null)
          {
            origin = xform.transform(origin, origin);
            pt = xform.transform(pt, pt);
          }
        
        // Create buffer and draw image
        createBuffer();
        
        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setRenderingHints(this.getRenderingHints());
        g2d.drawImage(img, xform, obs);

        // Perform compositing
        return drawComposite(new Rectangle2D.Double(origin.getX(),
                                                    origin.getY(),
                                                    pt.getX(), pt.getY()),
                             obs);
      }
  }

  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    // Find absolute bounds, in user-space, of this glyph vector 
    Rectangle2D bounds = gv.getLogicalBounds();
    bounds = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(),
                                    bounds.getWidth(), bounds.getHeight());
    
    // Perform draw operation
    if (comp == null || comp instanceof AlphaComposite)
      {
        super.drawGlyphVector(gv, x, y);
        updateBufferedImage((int)bounds.getX(), (int)bounds.getY(),
                            (int)bounds.getWidth(), (int)bounds.getHeight());
      }
    else
      {
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setPaint(this.getPaint());
        g2d.setStroke(this.getStroke());
        g2d.setTransform(transform);
        g2d.drawGlyphVector(gv, x, y);
        
        drawComposite(bounds, null);
      }
  }
  
  /**
   * Perform composite drawing from the buffer onto the main image.
   * 
   * The image to be composited should already be drawn into the buffer, in the
   * proper place, after all necessary transforms have been applied.
   * 
   * @param bounds The bounds to draw, in user-space.
   * @param observer The image observer, if any (may be null).
   * @return True on success, false on failure.
   */
  private boolean drawComposite(Rectangle2D bounds, ImageObserver observer)
  {
    // Find bounds in device space
    double[] points = new double[] {bounds.getX(), bounds.getY(),
                                    bounds.getMaxX(), bounds.getMaxY()};
    transform.transform(points, 0, points, 0, 2);
    bounds = new Rectangle2D.Double(points[0], points[1],
                                    (points[2] - points[0]),
                                    (points[3] - points[1]));

    // Clip bounds by the stored clip, and by the internal buffer
    Rectangle2D devClip = this.getClipInDevSpace();
    Rectangle2D.intersect(bounds, devClip, bounds);
    devClip = new Rectangle(buffer.getMinX(), buffer.getMinY(),
                            buffer.getWidth(), buffer.getHeight());
    Rectangle2D.intersect(bounds, devClip, bounds);
    
    // Round bounds as needed, but be conservative in our rounding
    // (otherwise it may leave unpainted stripes)
    double x = bounds.getX();
    double y = bounds.getY();
    double w = bounds.getWidth();
    double h = bounds.getHeight();
    if (Math.floor(x) != x)
      w--;
    if (Math.floor(y) != y)
      h--;
    bounds.setRect(Math.ceil(x), Math.ceil(y), Math.floor(w), Math.floor(h));
    
    // Find subimage of internal buffer for updating
    BufferedImage buffer2 = buffer;
    if (!bounds.equals(buffer2.getRaster().getBounds()))
      buffer2 = buffer2.getSubimage((int)bounds.getX(), (int)bounds.getY(),
                                    (int)bounds.getWidth(),
                                    (int)bounds.getHeight());

    // Find subimage of main image for updating
    BufferedImage current = image;
    current = current.getSubimage((int)bounds.getX(), (int)bounds.getY(),
                                  (int)bounds.getWidth(),
                                  (int)bounds.getHeight());

    // Perform actual composite operation
    compCtx.compose(buffer2.getRaster(), current.getRaster(),
                    current.getRaster());
    
    // Prevent the clearRect in CairoGraphics2D.drawImage from clearing
    // our composited image
    locked = true;
    
    // This MUST call directly into the "action" method in CairoGraphics2D,
    // not one of the wrappers, to ensure that the composite isn't processed
    // more than once!
    boolean rv = super.drawImage(current,
                                 AffineTransform.getTranslateInstance(bounds.getX(),
                                                                      bounds.getY()),
                                 new Color(0,0,0,0), null);
    locked = false;
    return rv;
  }
  
  private void createBuffer()
  {
    if (buffer == null)
      {
        buffer = new BufferedImage(image.getWidth(), image.getHeight(),
                                   BufferedImage.TYPE_INT_ARGB);
      }
    else
      {
        Graphics2D g2d = ((Graphics2D)buffer.getGraphics());
        
        g2d.setBackground(new Color(0,0,0,0));
        g2d.clearRect(0, 0, buffer.getWidth(), buffer.getHeight());
      }
  }
  
  protected ColorModel getNativeCM()
  {
    return image.getColorModel();
  }
  
  protected ColorModel getBufferCM()
  {
    return ColorModel.getRGBdefault();
  }
}

