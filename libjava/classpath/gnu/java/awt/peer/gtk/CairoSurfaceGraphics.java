/* CairoSurfaceGraphics.java
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
import java.awt.GraphicsEnvironment;
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
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.RenderedImage;
import java.util.Hashtable;

/**
 * Implementation of Graphics2D on a Cairo surface.
 */
public class CairoSurfaceGraphics extends CairoGraphics2D
{
  protected CairoSurface surface;
  private BufferedImage buffer;
  private long cairo_t;
  
  /**
   * Create a graphics context from a cairo surface
   */
  public CairoSurfaceGraphics(CairoSurface surface)
  {
    this.surface = surface;
    cairo_t = surface.newCairoContext();
    setup( cairo_t );
    setClip(0, 0, surface.width, surface.height);
  }

  /**
   * Creates another context from a surface.
   * Used by create().
   */ 
  private CairoSurfaceGraphics(CairoSurfaceGraphics copyFrom)
  {
    surface = copyFrom.surface;
    cairo_t = surface.newCairoContext();
    copy( copyFrom, cairo_t );
  }
  
  public Graphics create()
  {
    return new CairoSurfaceGraphics(this);
  }
  
  public GraphicsConfiguration getDeviceConfiguration()
  {
    return GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDefaultConfiguration();
  }
  
  protected Rectangle2D getRealBounds()
  {
    return new Rectangle2D.Double(0.0, 0.0, surface.width, surface.height);
  }

  public void copyAreaImpl(int x, int y, int width, int height, int dx, int dy)
  {
    surface.copyAreaNative(x, y, width, height, dx, dy, surface.width);
  }
  
  /**
   * Overloaded methods that do actual drawing need to account for custom
   * composites
   */
  public void draw(Shape s)
  {
    if (comp == null || comp instanceof AlphaComposite)
      super.draw(s);
    
    else
      {
        createBuffer();
        
        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setStroke(this.getStroke());
        g2d.setColor(this.getColor());
        g2d.draw(s);
        
        drawComposite(s.getBounds2D(), null);
      }
  }

  public void fill(Shape s)
  {
    if (comp == null || comp instanceof AlphaComposite)
      super.fill(s);
    
    else
      {
        createBuffer();
        
        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setPaint(this.getPaint());
        g2d.setColor(this.getColor());
        g2d.fill(s);
        
        drawComposite(s.getBounds2D(), null);
      }
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    if (comp == null || comp instanceof AlphaComposite)
      super.drawRenderedImage(image, xform);
    
    else
      {
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setRenderingHints(this.getRenderingHints());
        g2d.drawRenderedImage(image, xform);
        
        drawComposite(buffer.getRaster().getBounds(), null);
      }

  }

  protected boolean drawImage(Image img, AffineTransform xform,
                              Color bgcolor, ImageObserver obs)
  {
    if (comp == null || comp instanceof AlphaComposite)
      return super.drawImage(img, xform, bgcolor, obs);
    
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
    if (comp == null || comp instanceof AlphaComposite)
      super.drawGlyphVector(gv, x, y);
    
    else
      {
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setPaint(this.getPaint());
        g2d.setStroke(this.getStroke());
        g2d.drawGlyphVector(gv, x, y);
        
        Rectangle2D bounds = gv.getLogicalBounds();
        bounds = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(),
                                        bounds.getWidth(), bounds.getHeight());
        drawComposite(bounds, null);
      }
  }
  
  private boolean drawComposite(Rectangle2D bounds, ImageObserver observer)
  {
    // Clip source to visible areas that need updating
    Rectangle2D clip = this.getClipBounds();
    Rectangle2D.intersect(bounds, clip, bounds);
    clip = new Rectangle(buffer.getMinX(), buffer.getMinY(),
                         buffer.getWidth(), buffer.getHeight());
    Rectangle2D.intersect(bounds, clip, bounds);
    
    BufferedImage buffer2 = buffer;
    if (!bounds.equals(buffer2.getRaster().getBounds()))
      buffer2 = buffer2.getSubimage((int)bounds.getX(), (int)bounds.getY(),
                                    (int)bounds.getWidth(),
                                    (int)bounds.getHeight());
    
    // Get destination clip to bounds
    double[] points = new double[] {bounds.getX(), bounds.getY(),
                                    bounds.getMaxX(), bounds.getMaxY()};
    transform.transform(points, 0, points, 0, 2);
    
    Rectangle2D deviceBounds = new Rectangle2D.Double(points[0], points[1],
                                                       points[2] - points[0],
                                                       points[3] - points[1]);
    
    Rectangle2D.intersect(deviceBounds, this.getClipInDevSpace(), deviceBounds);
    
    BufferedImage current = CairoSurface.getBufferedImage(surface);
    current = current.getSubimage((int)deviceBounds.getX(),
                                  (int)deviceBounds.getY(),
                                  (int)deviceBounds.getWidth(),
                                  (int)deviceBounds.getHeight());

    // Perform actual composite operation
    compCtx.compose(buffer2.getRaster(), current.getRaster(),
                    buffer2.getRaster());
    
    // This MUST call directly into the "action" method in CairoGraphics2D,
    // not one of the wrappers, to ensure that the composite isn't processed
    // more than once!
    boolean rv = super.drawImage(buffer2,
                                 AffineTransform.getTranslateInstance(bounds.getX(),
                                                                      bounds.getY()),
                                 new Color(0,0,0,0), null);
    return rv;
  }
  
  private void createBuffer()
  {
    if (buffer == null)
      {
        buffer = new BufferedImage(getBufferCM(),
                                   surface.createCompatibleWritableRaster(),
                                   getBufferCM().isAlphaPremultiplied(),
                                   new Hashtable());
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
    return CairoSurface.cairoCM_pre;
  }
  
  protected ColorModel getBufferCM()
  {
    return CairoSurface.cairoColorModel;
  }
}
