/* ComponentGraphics.java --
   Copyright (C) 2006  Free Software Foundation, Inc.

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

import gnu.classpath.Pointer;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Point;
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
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.WritableRaster;
import java.util.Hashtable;

/**
 * ComponentGraphics - context for drawing directly to a component,
 * as this is an X drawable, it requires that we use GTK locks.
 *
 * This context draws directly to the drawable and requires xrender.
 */
public class ComponentGraphics extends CairoGraphics2D
{
  private static final boolean hasXRenderExtension = hasXRender();

  private GtkComponentPeer component;
  protected long cairo_t;
  private BufferedImage buffer, componentBuffer;

  private static ThreadLocal<Integer> hasLock = new ThreadLocal<Integer>();
  private static Integer ONE = Integer.valueOf(1);

  ComponentGraphics()
  {
  }
  
  private ComponentGraphics(GtkComponentPeer component)
  {
    this.component = component;
    cairo_t = initState(component);
    setup( cairo_t );
    Rectangle bounds = component.awtComponent.getBounds();
    setClip( new Rectangle( 0, 0, bounds.width, bounds.height) );
    setBackground(component.awtComponent.getBackground());
    setColor(component.awtComponent.getForeground());
  }

  private ComponentGraphics(ComponentGraphics cg)
  {
    component = cg.component;
    cairo_t = initState(component);
    copy( cg, cairo_t );
    Rectangle bounds = component.awtComponent.getBounds();
    setClip( new Rectangle( 0, 0, bounds.width, bounds.height) );
    setBackground(component.awtComponent.getBackground());
    setColor(component.awtComponent.getForeground());
  }

  /**
   * Creates a cairo_t for the component surface and return it.
   */
  private native long initState(GtkComponentPeer component);

  /**
   * Obtain and hold a GDK lock, which is required for all drawing operations
   * in this graphics context (since it is backed by an X surface).
   * 
   * This method causes the GDK locking behaviour to be re-entrant.  No race
   * conditions are caused since a ThreadLocal is used and each thread has its
   * own lock counter.
   */
  private void lock()
  {
    Integer i = hasLock.get();
    if (i == null)
      {
        start_gdk_drawing();
        hasLock.set(ONE);
      }
    else
      hasLock.set(Integer.valueOf(i.intValue() + 1));
  }

  /**
   * Release the re-entrant GDK lock.
   */
  private void unlock()
  {
    Integer i = hasLock.get();
    if (i == null)
      throw new IllegalStateException();
    if (i == ONE)
      {
        hasLock.set(null);
        end_gdk_drawing();
      }
    else if (i.intValue() == 2)
      hasLock.set(ONE);
    else
      hasLock.set(Integer.valueOf(i.intValue() - 1));
  }

  /**
   * Creates a cairo_t for a volatile image
   */
  protected native long initFromVolatile( long pixmapPtr, int width, int height);

  /**
   * Grab lock
   */
  private native void start_gdk_drawing();

  /**
   * Release lock
   */
  private native void end_gdk_drawing();

  /**
   * Query if the system has the XRender extension.
   */
  public static native boolean hasXRender();

  /**
   * This is a utility method (used by GtkComponentPeer) for grabbing the
   * image of a component.
   */
  private static native Pointer nativeGrab(GtkComponentPeer component);

  private native void copyAreaNative(GtkComponentPeer component, int x, int y, 
                                     int width, int height, int dx, int dy);

  private native void drawVolatile(GtkComponentPeer component,
                                   long vimg, int x, int y, 
                                   int width, int height, int cx, int cy,
                                   int cw, int ch);

  /**
   * Not really related (moveme?). Utility method used by GtkComponent.
   */
  public static GtkImage grab( GtkComponentPeer component )
  {
    return new GtkImage( nativeGrab( component ) );
  }

  /**
   * Returns a Graphics2D object for a component, either an instance of this 
   * class (if xrender is supported), or a context which copies.
   */
  public static Graphics2D getComponentGraphics(GtkComponentPeer component)
  {
    if( hasXRenderExtension )
      return new ComponentGraphics(component);

    Rectangle r = component.awtComponent.getBounds();
    return new ComponentGraphicsCopy(r.width, r.height, component);
  }

  public GraphicsConfiguration getDeviceConfiguration()
  {
    return component.getGraphicsConfiguration();
  }

  public Graphics create()
  {
    return new ComponentGraphics(this);
  }
  
  protected Rectangle2D getRealBounds()
  {
    return component.awtComponent.getBounds();
  }

  public void copyAreaImpl(int x, int y, int width, int height, int dx, int dy)
  {
    copyAreaNative(component, x, y, width, height, dx, dy);
  }

  /**
   * Overloaded methods that do actual drawing need to enter the gdk threads 
   * and also do certain things before and after.
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
    boolean rv;
    if (comp == null || comp instanceof AlphaComposite)
      rv = super.drawImage(img, xform, bgcolor, obs);
    
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
        rv = drawComposite(new Rectangle2D.Double(origin.getX(),
                                                    origin.getY(),
                                                    pt.getX(), pt.getY()),
                           obs);
      }
    return rv;
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
  
  public boolean drawImage(Image img, int x, int y, ImageObserver observer)
  {
    // If it is a GtkVolatileImage with an "easy" transform then
    // draw directly. Always pass a BufferedImage to super to avoid
    // deadlock (see Note in CairoGraphics.drawImage()).
    if (img instanceof GtkVolatileImage)
      {
        GtkVolatileImage vimg = (GtkVolatileImage) img;
        int type = transform.getType();
        if ((type == AffineTransform.TYPE_IDENTITY
             || type == AffineTransform.TYPE_TRANSLATION)
             && (clip == null || clip instanceof Rectangle2D))
          {
            Rectangle2D r = (Rectangle2D) clip;
            if (r == null)
              r = getRealBounds();
            x += transform.getTranslateX();
            y += transform.getTranslateY();
            drawVolatile(component, vimg.nativePointer,
                         x, y, vimg.width, vimg.height,
                         (int) (r.getX() + transform.getTranslateX()),
                         (int) (r.getY() + transform.getTranslateY()),
                         (int) r.getWidth(),
                         (int) r.getHeight());
            return true;
          }
        else
          return super.drawImage(vimg.getSnapshot(), x, y, observer);
      }

    BufferedImage bimg;
    if (img instanceof BufferedImage)
      bimg = (BufferedImage) img;
    else
      {
        ImageProducer source = img.getSource();
        if (source == null)
          return false;
        bimg = (BufferedImage) Toolkit.getDefaultToolkit().createImage(source);
      }
    return super.drawImage(bimg, x, y, observer);
  }
  
  public boolean drawImage(Image img, int x, int y, int width, int height,
                           ImageObserver observer)
  {
    // If it is a GtkVolatileImage with an "easy" transform then
    // draw directly. Always pass a BufferedImage to super to avoid
    // deadlock (see Note in CairoGraphics.drawImage()).
    if (img instanceof GtkVolatileImage
        && (clip == null || clip instanceof Rectangle2D))
      {
        GtkVolatileImage vimg = (GtkVolatileImage) img;
        int type = transform.getType();
        if ((type == AffineTransform.TYPE_IDENTITY
             || type == AffineTransform.TYPE_TRANSLATION)
             && (clip == null || clip instanceof Rectangle2D))
          {
            Rectangle2D r = (Rectangle2D) clip;
            if (r == null)
              r = getRealBounds();
            x += transform.getTranslateX();
            y += transform.getTranslateY();
            drawVolatile(component, vimg.nativePointer,
                         x, y, width, height,
                         (int) (r.getX() + transform.getTranslateX()),
                         (int) (r.getY() + transform.getTranslateY()),
                         (int) r.getWidth(),
                         (int) r.getHeight());
            return true;
          }
        else
          return super.drawImage(vimg.getSnapshot(), x, y,
                                 width, height, observer);
      }

    BufferedImage bimg;
    img = AsyncImage.realImage(img, observer);
    if (img instanceof BufferedImage)
      bimg = (BufferedImage) img;
    else
      {
        ImageProducer source = img.getSource();
        if (source == null)
          return false;
        bimg = (BufferedImage) Toolkit.getDefaultToolkit().createImage(source);
      }
    return super.drawImage(bimg, x, y, width, height, observer);
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
    
    // Get current image on the component
    GtkImage img = grab(component);
    Graphics gr = componentBuffer.createGraphics();
    gr.drawImage(img, 0, 0, null);
    gr.dispose();
    
    BufferedImage cBuffer = componentBuffer;
    if (!deviceBounds.equals(cBuffer.getRaster().getBounds()))
      cBuffer = cBuffer.getSubimage((int)deviceBounds.getX(),
                                    (int)deviceBounds.getY(),
                                    (int)deviceBounds.getWidth(),
                                    (int)deviceBounds.getHeight());
    
    // Perform actual composite operation
    compCtx.compose(buffer2.getRaster(), cBuffer.getRaster(),
                    cBuffer.getRaster());
    
    // This MUST call directly into the "action" method in CairoGraphics2D,
    // not one of the wrappers, to ensure that the composite isn't processed
    // more than once!
    boolean rv = super.drawImage(cBuffer,
                                 AffineTransform.getTranslateInstance(bounds.getX(),
                                                                      bounds.getY()),
                                 null, null);
    return rv;
  }
  
  private void createBuffer()
  {
    if (buffer == null)
      {
        WritableRaster rst;
        rst = Raster.createWritableRaster(GtkVolatileImage.createGdkSampleModel(component.awtComponent.getWidth(),
                                                                                component.awtComponent.getHeight()),
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
    
    if (componentBuffer == null)
      {
        WritableRaster rst;
        rst = Raster.createWritableRaster(GtkVolatileImage.createGdkSampleModel(component.awtComponent.getWidth(),
                                                                                component.awtComponent.getHeight()),
                                          new Point(0,0));
        
        componentBuffer = new BufferedImage(GtkVolatileImage.gdkColorModel, rst,
                                            GtkVolatileImage.gdkColorModel.isAlphaPremultiplied(),
                                            new Hashtable());
      }
  }
  
  protected ColorModel getNativeCM()
  {
    return GtkVolatileImage.gdkColorModel;
  }
  
  /* --- START OVERRIDDEN NATIVE METHODS ----
   * All native methods in CairoGraphics2D should be overridden here and
   * enclosed in locks, since the cairo surface is backed by an X surface
   * in this graphics context and the X surface requires external locking.
   * 
   * We lock everything "just in case", since it's difficult to know which
   * calls are and aren't thread-safe.  Overriding and locking the native
   * methods allows superclass code in CairoGraphics2D to execute properly, 
   * without the need to override every single method.
   * 
   * CAVEAT: if native code obtains a lock (using gdk_threads_enter(), not the
   * lock() method provided here) and then calls back into Java and one of these
   * methods ends up being called, we will deadlock.  The lock is only reentrant
   * when called via our lock() method. 
   */
  
  /* These methods are already locked in the superclass CairoGraphics2D
   * so they do not need to be overridden:
   * 
   * public void disposeNative
   *
   * protected void cairoDrawGlyphVector
   * 
   * protected void cairoSetFont
   */
  
  @Override
  protected long init(long pointer)
  {
    long ret;
    
    try
    {
      lock();
      ret = super.init(pointer);
    }
    finally
    {
      unlock();
    }
    
    return ret;
  }
  
  @Override
  protected void drawPixels(long pointer, int[] pixels, int w, int h,
                            int stride, double[] i2u, double alpha,
                            int interpolation)
  {
    try
    {
      lock();
      super.drawPixels(pointer, pixels, w, h, stride, i2u, alpha,
                       interpolation);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void setGradient(long pointer, double x1, double y1, 
                             double x2, double y2, 
                             int r1, int g1, int b1, int a1,
                             int r2, int g2, int b2, int a2, boolean cyclic)
  {
    try
    {
      lock();
      super.setGradient(pointer, x1, y1, x2, y2, r1, g1, b1, a1, r2, g2, b2, a2,
                        cyclic);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void setPaintPixels(long pointer, int[] pixels, int w, int h,
                                int stride, boolean repeat, int x, int y)
  {
    try
    {
      lock();
      super.setPaintPixels(pointer, pixels, w, h, stride, repeat, x, y);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetMatrix(long pointer, double[] m)
  {
    try
    {
      lock();
      super.cairoSetMatrix(pointer, m);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoScale(long pointer, double x, double y)
  {
    try
    {
      lock();
      super.cairoScale(pointer, x, y);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetOperator(long pointer, int cairoOperator)
  {
    try
    {
      lock();
      super.cairoSetOperator(pointer, cairoOperator);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetRGBAColor(long pointer, double red, double green,
                                   double blue, double alpha)
  {
    try
    {
      lock();
      super.cairoSetRGBAColor(pointer, red, green, blue, alpha);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetFillRule(long pointer, int cairoFillRule)
  {
    try
    {
      lock();
      super.cairoSetFillRule(pointer, cairoFillRule);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetLine(long pointer, double width, int cap, int join,
                              double miterLimit)
  {
    try
    {
      lock();
      super.cairoSetLine(pointer, width, cap, join, miterLimit);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetDash(long pointer, double[] dashes, int ndash, 
                              double offset)
  {
    try
    {
      lock();
      super.cairoSetDash(pointer, dashes, ndash, offset);
    }
    finally
    {
      unlock();
    }
  }

  @Override
  protected void cairoRectangle(long pointer, double x, double y,
                                double width, double height)
  {
    try
    {
      lock();
      super.cairoRectangle(pointer, x, y, width, height);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoArc(long pointer, double x, double y, 
                          double radius, double angle1, double angle2)
  {
    try
    {
      lock();
      super.cairoArc(pointer, x, y, radius, angle1, angle2);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSave(long pointer)
  {
    try
    {
      lock();
      super.cairoSave(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoRestore(long pointer)
  {
    try
    {
      lock();
      super.cairoRestore(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoNewPath(long pointer)
  {
    try
    {
      lock();
      super.cairoNewPath(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoClosePath(long pointer)
  {
    try
    {
      lock();
      super.cairoClosePath(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoMoveTo(long pointer, double x, double y)
  {
    try
    {
      lock();
      super.cairoMoveTo(pointer, x, y);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoLineTo(long pointer, double x, double y)
  {
    try
    {
      lock();
      super.cairoLineTo(pointer, x, y);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoCurveTo(long pointer, double x1, double y1, double x2,
                              double y2, double x3, double y3)
  {
    try
    {
      lock();
      super.cairoCurveTo(pointer, x1, y1, x2, y2, x3, y3);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoStroke(long pointer)
  {
    try
    {
      lock();
      super.cairoStroke(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoFill(long pointer, double alpha)
  {
    try
    {
      lock();
      super.cairoFill(pointer, alpha);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoClip(long pointer)
  {
    try
    {
      lock();
      super.cairoClip(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoResetClip(long pointer)
  {
    try
    {
      lock();
      super.cairoResetClip(pointer);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void cairoSetAntialias(long pointer, boolean aa)
  {
    try
    {
      lock();
      super.cairoSetAntialias(pointer, aa);
    }
    finally
    {
      unlock();
    }
  }
  
  @Override
  protected void drawCairoSurface(CairoSurface surface, AffineTransform tx,
                                  double alpha, int interpolation)
  {
    try
    {
      lock();
      super.drawCairoSurface(surface, tx, alpha, interpolation);
    }
    finally
    {
      unlock();
    }
  }
}