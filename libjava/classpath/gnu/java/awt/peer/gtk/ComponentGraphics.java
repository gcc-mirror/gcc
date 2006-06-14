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

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Toolkit;
import java.awt.Point;
import java.awt.font.FontRenderContext;
import java.awt.font.GlyphVector;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.ImagingOpException;
import java.awt.image.RenderedImage;

/**
 * ComponentGraphics - context for drawing directly to a component,
 * as this is an X drawable, it requires that we use GTK locks.
 *
 * This context draws directly to the drawable and requires xrender.
 */
public class ComponentGraphics extends CairoGraphics2D
{
  private GtkComponentPeer component;
  protected long cairo_t;

  private static ThreadLocal hasLock = new ThreadLocal();
  private static Integer ONE = Integer.valueOf(1);

  private void lock()
  {
    Integer i = (Integer) hasLock.get();
    if (i == null)
      {
	start_gdk_drawing();
	hasLock.set(ONE);
      }
    else
      hasLock.set(Integer.valueOf(i.intValue() + 1));
  }

  private void unlock()
  {
    Integer i = (Integer) hasLock.get();
    if (i == null)
      throw new IllegalStateException();
    if (i == ONE)
      {
	hasLock.set(null);
	end_gdk_drawing();
      }
    else
      hasLock.set(Integer.valueOf(i.intValue() - 1));
  }

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
   * Destroys the component surface and calls dispose on the cairo
   * graphics2d to destroy any super class resources.
   */
  public void dispose()
  {
    super.dispose();
    disposeSurface(nativePointer);
  }

  /**
   * Destroys the component surface.
   */
  private native void disposeSurface(long nativePointer);

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


  private native void copyAreaNative(GtkComponentPeer component, int x, int y, 
				     int width, int height, int dx, int dy);

  private native void drawVolatile(GtkComponentPeer component,
				   long vimg, int x, int y, 
				   int width, int height);

  /**
   * Returns a Graphics2D object for a component, either an instance of this 
   * class (if xrender is supported), or a context which copies.
   */
  public static Graphics2D getComponentGraphics(GtkComponentPeer component)
  {
    if( hasXRender() )
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
    lock();
    try
      {
	super.draw(s);
      }
    finally
      {
	unlock();
      }
  }

  public void fill(Shape s)
  {
    lock();
    try
      {
	super.fill(s);
      }
    finally
      {
	unlock();
      }
  }

  public void drawRenderedImage(RenderedImage image, AffineTransform xform)
  {
    lock();
    try
      {
	super.drawRenderedImage(image, xform);
      }
    finally
      {
	unlock();
      }
  }

  protected boolean drawImage(Image img, AffineTransform xform,
			      Color bgcolor, ImageObserver obs)
  {
    boolean rv;
    lock();
    try
      {
	rv = super.drawImage(img, xform, bgcolor, obs);
      }
    finally
      {
	unlock();
      }
    return rv;
  }

  public void drawGlyphVector(GlyphVector gv, float x, float y)
  {
    lock();
    try
      {
	super.drawGlyphVector(gv, x, y);
      }
    finally
      {
	unlock();
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
	if (type == AffineTransform.TYPE_IDENTITY)
	  {
	    drawVolatile(component, vimg.nativePointer,
			 x, y, vimg.width, vimg.height);
	    return true;
	  }
	  else if (type == AffineTransform.TYPE_TRANSLATION)
	  {
	    x += transform.getTranslateX();
	    y += transform.getTranslateY();
	    drawVolatile(component, vimg.nativePointer,
			 x, y, vimg.width, vimg.height);
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
    if (img instanceof GtkVolatileImage)
      {
        GtkVolatileImage vimg = (GtkVolatileImage) img;
	int type = transform.getType();
	if (type == AffineTransform.TYPE_IDENTITY)
	  {
	    drawVolatile(component, vimg.nativePointer,
			 x, y, width, height);
	    return true;
	  }
	  else if (type == AffineTransform.TYPE_TRANSLATION)
	  {
	    x += transform.getTranslateX();
	    y += transform.getTranslateY();
	    drawVolatile(component, vimg.nativePointer,
			 x, y, width, height);
	    return true;
	  }
	else
	  return super.drawImage(vimg.getSnapshot(), x, y,
				 width, height, observer);
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
    return super.drawImage(bimg, x, y, width, height, observer);
  }

}

