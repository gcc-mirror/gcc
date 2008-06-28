/* VolatileImageGraphics.java
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
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Point;
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
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.util.Hashtable;

public class VolatileImageGraphics extends ComponentGraphics
{
  private GtkVolatileImage owner;
  private BufferedImage buffer;

  public VolatileImageGraphics(GtkVolatileImage img)
  {
    this.owner = img;
    cairo_t = initFromVolatile( owner.nativePointer );
    setup( cairo_t );
  }

  private VolatileImageGraphics(VolatileImageGraphics copy)
  {
    this.owner = copy.owner;
    cairo_t = initFromVolatile(owner.nativePointer);
    copy( copy, cairo_t );
  }

  public void copyAreaImpl(int x, int y, int width, int height, int dx, int dy)
  {
    owner.copyArea(x, y, width, height, dx, dy);
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    GraphicsConfiguration conf;
    if (owner.component != null)
      {
        conf = owner.component.getGraphicsConfiguration();
      }
    else
      {
        return java.awt.GraphicsEnvironment.getLocalGraphicsEnvironment()
          .getDefaultScreenDevice().getDefaultConfiguration();
      }
    return conf;
  }

  public Graphics create()
  {
    return new VolatileImageGraphics( this );
  }

  public void draw(Shape s)
  {
    if (comp == null || comp instanceof AlphaComposite)
      super.draw(s);
    
    // Custom composite
    else
      {
        // Draw operation to temporary buffer
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setColor(this.getColor());
        g2d.setStroke(this.getStroke());
        g2d.draw(s);
        
        drawComposite(s.getBounds2D(), null);
      }
  }
  
  public void fill(Shape s)
  {
    if (comp == null || comp instanceof AlphaComposite)
      super.fill(s);

    // Custom composite
    else
      {
        // Draw operation to temporary buffer
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        g2d.setPaint(this.getPaint());
        g2d.setColor(this.getColor());
        g2d.fill(s);
        
        drawComposite(s.getBounds2D(), null);
      }
  }
  
  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    if (comp == null || comp instanceof AlphaComposite)
      super.drawGlyphVector(gv, x, y);
    
    // Custom composite
    else
      {
        // Draw operation to temporary buffer
        createBuffer();

        Graphics2D g2d = (Graphics2D)buffer.getGraphics();
        
        g2d.setPaint(this.getPaint());
        g2d.setColor(this.getColor());
        g2d.drawGlyphVector(gv, x, y);
        
        Rectangle2D bounds = gv.getLogicalBounds();
        bounds = new Rectangle2D.Double(x + bounds.getX(), y + bounds.getY(),
                                        bounds.getWidth(), bounds.getHeight());
        drawComposite(bounds, null);
      }
  }

  protected boolean drawImage(Image img, AffineTransform xform,
                              Color bgcolor, ImageObserver obs)
    {
      if (comp == null || comp instanceof AlphaComposite)
        return super.drawImage(img, xform, bgcolor, obs);
      
      // Custom composite
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
          
          // Find dimensions of translation
          Point2D origin = new Point2D.Double(bImg.getMinX(), bImg.getMinY());
          Point2D pt = new Point2D.Double(bImg.getWidth(), bImg.getHeight());
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

          // Perform compositing from buffer to screen
          return drawComposite(new Rectangle2D.Double((int)origin.getX(),
                                                      (int)origin.getY(),
                                                      (int)pt.getX(),
                                                      (int)pt.getY()),
                               obs);
        }
    }
  
  public boolean drawImage(Image img, int x, int y, ImageObserver observer)
  {
    if (img instanceof GtkVolatileImage
        && (comp == null || comp instanceof AlphaComposite))
      {
        owner.drawVolatile( ((GtkVolatileImage)img).nativePointer, 
                            x, y,
                            ((GtkVolatileImage)img).width, 
                            ((GtkVolatileImage)img).height );
        return true;
      }      
    return super.drawImage( img, x, y, observer );
  }
  
  public boolean drawImage(Image img, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    if ((img instanceof GtkVolatileImage)
        && (comp == null || comp instanceof AlphaComposite))
      {
        owner.drawVolatile( ((GtkVolatileImage)img).nativePointer, 
                            x, y, width, height );
        return true;
      }      
    return super.drawImage( img, x, y, width, height, observer );
  }

  protected Rectangle2D getRealBounds()
  {
    return new Rectangle2D.Double(0, 0, owner.width, owner.height);
  }
  
  private boolean drawComposite(Rectangle2D bounds, ImageObserver observer)
  {
    // Clip source to visible areas that need updating
    Rectangle2D clip = this.getClipBounds();
    Rectangle2D.intersect(bounds, clip, bounds);
    
    BufferedImage buffer2 = buffer;
    if (!bounds.equals(buffer2.getRaster().getBounds()))
      buffer2 = buffer2.getSubimage((int)bounds.getX(), (int)bounds.getY(),
                                    (int)bounds.getWidth(),
                                    (int)bounds.getHeight());
    
    // Get current on-screen pixels (destination) and clip to bounds
    BufferedImage current = owner.getSnapshot();

    double[] points = new double[] {bounds.getX(), bounds.getY(),
                                    bounds.getMaxX(), bounds.getMaxY()};
    transform.transform(points, 0, points, 0, 2);
    
    Rectangle2D deviceBounds = new Rectangle2D.Double(points[0], points[1],
                                                      points[2] - points[0],
                                                      points[3] - points[1]);
    Rectangle2D.intersect(deviceBounds, this.getClipInDevSpace(), deviceBounds);
    
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
    Composite oldComp = comp;           // so that ComponentGraphics doesn't
    comp = null;                        // process the composite again
    boolean rv = super.drawImage(buffer2,
                           AffineTransform.getTranslateInstance(bounds.getX(),
                                                                bounds.getY()),
                           null, null);
    comp = oldComp;

    return rv;
  }
  
  private void createBuffer()
  {
    if (buffer == null)
      {
        WritableRaster rst;
        rst = Raster.createWritableRaster(GtkVolatileImage.createGdkSampleModel(owner.width,
                                                                  owner.height),
                                          new Point(0,0));
        
        buffer = new BufferedImage(GtkVolatileImage.gdkColorModel, rst,
                                   GtkVolatileImage.gdkColorModel.isAlphaPremultiplied(),
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
    // We should really return GtkVolatileImage.gdkColorModel ,
    // but CairoGraphics2D doesn't handle alpha premultiplication properly (see
    // the fixme in drawImage) so we use the naive Cairo model instead to trick
    // the compositing context.
    // Because getNativeCM() == getBufferCM() for this peer, it doesn't break.
    return CairoSurface.cairoCM_pre;
  }
}

