/* Copyright (C) 2000, 2002, 2003  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.awt.xlib;

import java.awt.Dimension;
import java.awt.BufferCapabilities;
import java.awt.Component;
import java.awt.EventQueue;
import java.awt.Rectangle;
import java.awt.Color;
import java.awt.Container;
import java.awt.Image;
import java.awt.GraphicsConfiguration;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.AWTEvent;
import java.awt.Cursor;
import java.awt.Shape;

import java.awt.peer.*;
import java.awt.image.*;

import java.awt.event.MouseListener;
import java.awt.event.PaintEvent;

import java.util.EventListener;

import gnu.gcj.xlib.WMSizeHints;
import gnu.gcj.xlib.Window;
import gnu.gcj.xlib.WindowAttributes;
import gnu.gcj.xlib.Display;
import gnu.gcj.xlib.Visual;
import gnu.gcj.xlib.Screen;
import gnu.gcj.xlib.XImage;

import gnu.awt.j2d.*;

public class XCanvasPeer implements CanvasPeer
{
  static final Dimension MIN_SIZE = new Dimension(1, 1);
  
  public // temporary
  
  Window window;
  Window parent;

  Component component;
  XGraphicsConfiguration config;
  
  public XCanvasPeer(Component component)
  {
    this.component = component;
    
    // Set up graphics configuration (ie. screen + visual):

    config = (XGraphicsConfiguration)
      component.getGraphicsConfiguration();

    if (config == null)
      {
	// This will usually only happen for toplevel windows
	config = getXToolkit().getDefaultXGraphicsConfiguration();
      } 

    Rectangle bounds = component.getBounds();
    parent = locateParentWindow(bounds);
	
    // Windows in X must atleast be of size 1x1
    boolean boundsChanged = false;
    if (bounds.width < 1)
      {
	boundsChanged = true;
	bounds.width = 1;
      }
    if (bounds.height < 1)
      {
	boundsChanged = true;
	bounds.height = 1;
      }
	
    /* don't worry about this calling back to us, since the real
       component object has not yet received a reference to this peer
       object. */
    component.setBounds(bounds);
	    
    WindowAttributes attributes = new WindowAttributes();

    /* Set background color */
    Color bg = component.getBackground();
    if (bg != null)
      {
	int[] components =
	{
	  bg.getRed(),
	  bg.getGreen(),
	  bg.getBlue(),
	  0xff
	};

	ColorModel cm = config.getColorModel();
	long pixel = cm.getDataElement(components, 0);
	attributes.setBackground(pixel);
      }
	
    /* Set exposure mask so that we get exposure events
       that can be translated into paint() calls. */
    long eventMask = WindowAttributes.MASK_EXPOSURE;

    /* It would be nice to set up all other required events here, but
       it is not possible to do so before after all the children of
       this component has been realized.  The reason is that it is not
       determined whether a component is lightweight before after the
       addNotify() method has been called.  Thus, it is not possible
       for parent component to determine what events it needs to
       furnish for lightweight children.  Instead, we currently rely
       on the component calling our setEventMask() method after the
       correct event mask has been determined. */

    attributes.setEventMask(eventMask);
	
	    
    // TODO: set more window attributes?

    /* don't allow event queue to process events from the newly
       created window before this peer has been registered as client
       data. */
    synchronized (getXToolkit().eventLoop)
      {
	window = new gnu.gcj.xlib.Window(parent, bounds, attributes);
	window.setClientData(this); /* make it possible to find back
				       to this peer object. Used by
				       XEventQueue. */
      }
    
    initWindowProperties();

    if (component.isVisible())
      EventQueue.invokeLater(new DoMap(window));
  }

  /**
   * Override this in subclasses to implement other ways of obtaining
   * parent windows.  Toplevel windows will typically have a different
   * implementation.
   */
  gnu.gcj.xlib.Window locateParentWindow(Rectangle bounds)
  {
    Container parent = component.getParent();
    while (parent.isLightweight())
      {
	bounds.x += parent.getX();
	bounds.y += parent.getY();
	parent = parent.getParent();
	// a null pointer here is a genuine error
      }
    
    XCanvasPeer parentPeer = (XCanvasPeer) parent.getPeer();
    if (parentPeer == null)
      throw new NullPointerException("Parent has no peer. This should " +
				     "not be possible, since the " +
				     "calls leading here should come " +
				     "from parent, after it has " +
				     "set the parent peer.");
    return parentPeer.window;
  }
    

  /** 
   * Template method to allow subclasses to apply properties to X11
   * window right after creation.
   */
  void initWindowProperties()
  {
  }
    
  XToolkit getXToolkit()
  {
    return XToolkit.INSTANCE;
  }

  protected void ensureFlush()
  {
    getXToolkit().flushIfIdle();
  }

  public Component getComponent()
  {
    return component;
  }
  
  long getBasicEventMask()
  {
    return WindowAttributes.MASK_EXPOSURE;
  }
    
  // -------- java.awt.peer.ComponentPeer implementation

  public int checkImage(Image img, int width, int height, ImageObserver o)
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }
  public Image createImage(ImageProducer prod)
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }
  public Image createImage(int width, int height)
  {
    return new XOffScreenImage (config, window, width, height);
  }
  public void dispose()
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public GraphicsConfiguration getGraphicsConfiguration()
  {
    return config;
  }

  public FontMetrics getFontMetrics(Font f)
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public ColorModel getColorModel ()
  {
    return null;
  }

  public Graphics getGraphics()
  {
    DirectRasterGraphics gfxDevice = new XGraphics(window, config);
    IntegerGraphicsState igState = new IntegerGraphicsState(gfxDevice);
    Graphics2DImpl gfx2d = new Graphics2DImpl(config);

    gfx2d.setState(igState);
    gfx2d.setColor(component.getBackground());
    return gfx2d;
  }
    
  public Point getLocationOnScreen()
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public Dimension getMinimumSize ()
  {
    return MIN_SIZE;
  }

  public Dimension minimumSize ()
  {
    return getMinimumSize ();
  }

  public Dimension getPreferredSize ()
  {
    return component.getSize();
  }
    
  public Dimension preferredSize ()
  {
    return getPreferredSize();
  }
    
  public Toolkit getToolkit()
  {
    return getXToolkit();
  }

  public void handleEvent(AWTEvent event)
  {
  }

  public boolean isFocusTraversable()
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public void paint(Graphics gfx)
  {
    // do nothing by default
  }
    
  public boolean prepareImage(Image img, int width, int height,
			      ImageObserver o)
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public void print(Graphics graphics)
  {
    paint(graphics);
  }

  public void repaint(long tm, int x, int y, int w, int h)
  {
    /* TODO?

       X allows intelligent X servers to do smart
       refreshing. Perhaps involve X in repainting of components,
       rather that keeping it all within the local event queue. */
    
    PaintEvent updateEvent = new PaintEvent(component,
					    PaintEvent.UPDATE,
					    new Rectangle(x, y, w, h));
    getXToolkit().queue.postEvent(updateEvent);
  }
    
  public void requestFocus()
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public void setBackground(Color color)
  {
    throw new UnsupportedOperationException("not implemented");
  }

  public void setBounds(int x, int y, int width, int height)
  {
    width  = Math.max(width,  1);
    height = Math.max(height, 1);
    window.setBounds(x, y, width, height);
    ensureFlush();	    
  }
    
  public void reshape (int x, int y, int width, int height)
  {
    setBounds (x, y, width, height);
  }

  public void setCursor(Cursor cursor)
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public void setEnabled(boolean enabled)
  {
    throw new UnsupportedOperationException("FIXME, not implemented");
  }

  public void enable ()
  {
    setEnabled (true);
  }

  public void disable ()
  {
    setEnabled (false);
  }

  public void setEventMask(long eventMask)
  {
    WindowAttributes attributes = new WindowAttributes();

    long xEventMask = getBasicEventMask();
	
    if ((eventMask & AWTEvent.MOUSE_EVENT_MASK) != 0)
      {
	xEventMask |=
	  WindowAttributes.MASK_BUTTON_PRESS |
	  WindowAttributes.MASK_BUTTON_RELEASE;
      }
	    
    attributes.setEventMask(xEventMask);
    window.setAttributes(attributes);
    ensureFlush();
  }

  public void setFont(Font font)
  {
    /* default canvas peer does keep track of font, since it won't
       write anything. */
  }

  public void setForeground(Color color)
  {
    /* default canvas peer does keep track of foreground, since it won't
       paint anything. */
  }
	
  public void setVisible(boolean visible)
  {
    if (visible)
      {
	window.map();
	ensureFlush();	    
      }
    else
      {
	throw new UnsupportedOperationException("unmap not implemented");
      }
  }
	
  public void show ()
  {
    setVisible (true);
  }

  public void hide ()
  {
    setVisible (false);
  }

  public boolean isFocusable ()
  {
    return false;
  }

  public boolean requestFocus (Component source, boolean b1, 
                               boolean b2, long x)
  {
    return false;
  }

  public boolean isObscured ()
  {
    return false;
  }

  public boolean canDetermineObscurity ()
  {
    return false;
  }

  public void coalescePaintEvent (PaintEvent e)
  {
  }

  public void updateCursorImmediately ()
  {
  }

  public VolatileImage createVolatileImage (int width, int height)
  {
    return null;
  }

  public boolean handlesWheelScrolling ()
  {
    return false;
  }

  public void createBuffers (int x, BufferCapabilities capabilities)
    throws java.awt.AWTException

  {
  }

  public Image getBackBuffer ()
  {
    return null;
  }

  public void flip (BufferCapabilities.FlipContents contents)
  {
  }

  public void destroyBuffers ()
  {
  }

  static class DoMap implements Runnable 
  {
    Window window;
    public DoMap(Window w) 
    {
      this.window = w;
    }
    
    public void run() 
    {
      window.map();
    }
  }
}

