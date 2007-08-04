/* SwingComponentPeer.java -- An abstract base class for Swing based peers
   Copyright (C)  2006, 2007  Free Software Foundation, Inc.

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

package gnu.java.awt.peer.swing;

import java.awt.AWTEvent;
import java.awt.AWTException;
import java.awt.BufferCapabilities;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.EventQueue;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.GraphicsConfiguration;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.BufferCapabilities.FlipContents;
import java.awt.event.FocusEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.awt.event.PaintEvent;
import java.awt.image.ColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.awt.image.VolatileImage;
import java.awt.peer.ComponentPeer;
import java.awt.peer.ContainerPeer;
import java.awt.peer.LightweightPeer;

import javax.swing.JComponent;
import javax.swing.RepaintManager;

/**
 * The base class for Swing based component peers. This provides the basic
 * functionality needed for Swing based component peers. Many methods are
 * implemented to forward to the Swing component. Others however forward
 * to the component's parent and expect the toplevel component peer to provide
 * a real implementation of it. These are for example the key methods
 * {@link #getGraphics()} and {@link #createImage(int, int)}, as well as
 * {@link #getLocationOnScreen()}.
 *
 * This class also provides the necesary hooks into the Swing painting and
 * event handling system. In order to achieve this, it traps paint, mouse and
 * key events in {@link #handleEvent(AWTEvent)} and calls some special methods
 * ({@link #peerPaint(Graphics)}, {@link #handleKeyEvent(KeyEvent)},
 * {@link #handleMouseEvent(MouseEvent)} and
 * {@link #handleMouseMotionEvent(MouseEvent)}) that call the corresponding
 * Swing methods.
 *
 * @author Roman Kennke (kennke@aicas.com)
 */
public class SwingComponentPeer
  implements ComponentPeer
{

  /**
   * The AWT component for this peer.
   */
  protected Component awtComponent;

  /**
   * The Swing component for this peer.
   */
  protected SwingComponent swingComponent;

  /**
   * The font that is set for this peer.
   */
  protected Font peerFont;

  /**
   * The current repaint area.
   */
  protected Rectangle paintArea;

 /**
   * Creates a SwingComponentPeer instance. Subclasses are expected to call
   * this constructor and thereafter call {@link #init(Component, JComponent)}
   * in order to setup the AWT and Swing components properly.
   */
  protected SwingComponentPeer()
  {
    // Nothing to do here.
  }

  /**
   * Initializes the AWT and Swing component for this peer. It is expected that
   * subclasses call this from within their constructor.
   *
   * @param awtComp the AWT component for this peer
   * @param swingComp the Swing component for this peer
   */
  protected void init(Component awtComp, SwingComponent swingComp)
  {
    awtComponent = awtComp;
    swingComponent = swingComp;
    if (swingComponent != null)
      {
        JComponent c = swingComponent.getJComponent();
        if (c != null)
          {
            c.addNotify();
            RepaintManager.currentManager(c).setDoubleBufferingEnabled(false);
            System.setProperty("gnu.awt.swing.doublebuffering", "true");
          }
      }

    // Register this heavyweight component with the nearest heavyweight
    // container, so we get peerPaint() triggered by that container.
    if (! (this instanceof LightweightPeer))
      {
        Component comp = awtComponent;
        Container parent = comp.getParent();
        while (parent != null &&
               ! (parent.getPeer() instanceof SwingContainerPeer))
          {
            comp = parent;
            parent = comp.getParent();
          }

        // At this point we have the ancestor with a SwingContainerPeer
        // (or null peer).
        if (parent != null && parent.getPeer() instanceof SwingContainerPeer)
          {
            SwingContainerPeer p = (SwingContainerPeer) parent.getPeer();
            p.addHeavyweightDescendent(awtComponent);
          }
      }
  }

  /**
   * Returns the construction status of the specified image. This is called
   * by {@link Component#checkImage(Image, int, int, ImageObserver)}.
   *
   * @param img the image
   * @param width the width of the image
   * @param height the height of the image
   * @param ob the image observer to be notified of updates of the status
   *
   * @return a bitwise ORed set of ImageObserver flags
   */
  public int checkImage(Image img, int width, int height, ImageObserver ob)
  {
    return Toolkit.getDefaultToolkit().checkImage(img, width, height, ob);
  }

  /**
   * Creates an image by starting the specified image producer. This is called
   * by {@link Component#createImage(ImageProducer)}.
   *
   * @param prod the image producer to be used to create the image
   *
   * @return the created image
   */
  public Image createImage(ImageProducer prod)
  {
    Image image = Toolkit.getDefaultToolkit().createImage(prod);
	return image;
  }

  /**
   * Creates an empty image with the specified <code>width</code> and
   * <code>height</code>.
   *
   * This is implemented to let the parent component create the image. This
   * eventually goes up to the top-level component peer, which is then expected
   * to deliver the image.
   *
   * @param width the width of the image to be created
   * @param height the height of the image to be created
   *
   * @return the created image
   */
  public Image createImage(int width, int height)
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    return parentPeer.createImage(width, height);
  }

  /**
   * Disables the component. This is called by {@link Component#disable()}.
   */
  public void disable()
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setEnabled(false);
  }

  /**
   * Disposes the component peer. This should release all resources held by the
   * peer. This is called when the component is no longer in use.
   */
  public void dispose()
  {
    // Unregister this heavyweight component from the nearest heavyweight
    // container.
    if (! (this instanceof LightweightPeer))
      {
        Component comp = awtComponent;
        Container parent = comp.getParent();
        while (parent != null &&
               ! (parent.getPeer() instanceof SwingContainerPeer))
          {
            comp = parent;
            parent = comp.getParent();
          }

        // At this point we have the ancestor with a SwingContainerPeer
        // (or null peer).
        if (parent != null && parent.getPeer() instanceof SwingContainerPeer)
          {
            SwingContainerPeer p = (SwingContainerPeer) parent.getPeer();
            p.removeHeavyweightDescendent(awtComponent);
          }
      }

    awtComponent = null;
    swingComponent = null;
  }

  /**
   * Enables the component. This is called by {@link Component#enable()}.
   */
  public void enable()
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setEnabled(true);
  }

  /**
   * Returns the color model of the component. This is currently not used.
   *
   * @return the color model of the component
   */
  public ColorModel getColorModel()
  {
    // FIXME: When this peer method will be used, we need to provide an
    // implementation of this, probably forwarding to the toplevel peer, like
    // in the other methods.
    return null;
  }

  /**
   * Returns the font metrics for the specified font. This is called by
   * {@link Component#getFontMetrics(Font)}.
   *
   * This is implemented to query the font metrics from the parent component.
   * This will eventually call the top-level component peer, which is then
   * expected to deliver a font metrics object.
   *
   * @param f the font for which to query the font metrics
   *
   * @return the font metrics for the specified font
   */
  public FontMetrics getFontMetrics(Font f)
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    return parentPeer.getFontMetrics(f);
  }

  /**
   * Returns a {@link Graphics} object suitable for drawing on this component.
   * This is called by {@link Component#getGraphics()}.
   *
   * This is implemented to query the graphics from the parent component and
   * adjust the clip and translation to match this component.
   * This will eventually call the top-level component peer, which is then
   * expected to deliver a graphics object.
   *
   * @return a graphics object suitable for drawing on this component
   */
  public Graphics getGraphics()
  {
    Component parent = awtComponent.getParent();
    Graphics g = parent.getGraphics();
    g.translate(awtComponent.getX(), awtComponent.getY());
    g.setClip(0, 0, awtComponent.getWidth(), awtComponent.getHeight());
    return g;
  }

  /**
   * Returns the location of this component in screen coordinates. This is
   * called by {@link Component#getLocationOnScreen()}.
   *
   * This is implemented to query the parent component peer for its screen
   * location and adds the offset of this component to it. This will eventually
   * call the top-level component's peer, which is then expected to provide
   * it's screen location.
   *
   * @return the location of this component in screen coordinates
   */
  public Point getLocationOnScreen()
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    Point location = parentPeer.getLocationOnScreen();
    location.x += awtComponent.getX();
    location.y += awtComponent.getY();
    return location;
  }

  /**
   * Returns the minimum size for the component. This is called by
   * {@link Component#getMinimumSize()}.
   *
   * This is implemented to return the Swing component's minimum size.
   *
   * @return the minimum size for the component
   */
  public Dimension getMinimumSize()
  {
    return minimumSize();
  }

  /**
   * Returns the preferred size for the component. This is called by
   * {@link Component#getPreferredSize()}.
   *
   * This is implemented to return the Swing component's preferred size.
   *
   * @return the preferred size for the component
   */
  public Dimension getPreferredSize()
  {
    return preferredSize();
  }

  /**
   * Returns the toolkit that created this peer.
   *
   * @return the toolkit that created this peer
   */
  public Toolkit getToolkit()
  {
    return Toolkit.getDefaultToolkit();
  }

  /**
   * Handles the given event. This is called from
   * {@link Component#dispatchEvent(AWTEvent)} to give the peer a chance to 
   * react to events for the component.
   *
   * @param e the event
   */
  public void handleEvent(AWTEvent e)
  {
    switch (e.getID())
      {
      case PaintEvent.UPDATE:
      case PaintEvent.PAINT:
        if (awtComponent.isShowing())
          {
            Rectangle clip ;
            synchronized (this)
              {
                coalescePaintEvent((PaintEvent) e);
                assert paintArea != null;
                clip = paintArea;
                paintArea = null;
              }
            Graphics g = awtComponent.getGraphics();
            try
              {
                g.clipRect(clip.x, clip.y, clip.width, clip.height);
                peerPaint(g, e.getID() == PaintEvent.UPDATE);
              }
            finally
              {
                g.dispose();
              }
          }
        break;
      case MouseEvent.MOUSE_PRESSED:
      case MouseEvent.MOUSE_RELEASED:
      case MouseEvent.MOUSE_CLICKED:
      case MouseEvent.MOUSE_ENTERED:
      case MouseEvent.MOUSE_EXITED:
        handleMouseEvent((MouseEvent) e);
        break;
      case MouseEvent.MOUSE_MOVED:
      case MouseEvent.MOUSE_DRAGGED:
        handleMouseMotionEvent((MouseEvent) e);
        break;
      case KeyEvent.KEY_PRESSED:
      case KeyEvent.KEY_RELEASED:
      case KeyEvent.KEY_TYPED:
        handleKeyEvent((KeyEvent) e);
        break;
      case FocusEvent.FOCUS_GAINED:
      case FocusEvent.FOCUS_LOST:
        handleFocusEvent((FocusEvent)e);
        break;
      default:
        // Other event types are not handled here.
        break;
      }
  }

  /**
   * Makes the component invisible. This is called from
   * {@link Component#hide()}.
   *
   * This is implemented to call setVisible(false) on the Swing component.
   */
  public void hide()
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setVisible(false);

    Component parent = awtComponent.getParent();
    if (parent != null)
      parent.repaint(awtComponent.getX(), awtComponent.getY(),
                     awtComponent.getWidth(), awtComponent.getHeight());
  }

  /**
   * Returns <code>true</code> if the component can receive keyboard input
   * focus. This is called from {@link Component#isFocusTraversable()}.
   * 
   * This is implemented to return isFocusable() from the Swing component.
   *
   * @specnote Part of the earlier 1.1 API, replaced by isFocusable().
   */
  public boolean isFocusTraversable()
  {
    return swingComponent != null ?
             swingComponent.getJComponent().isFocusable() : false;
  }

  /**
   * Returns <code>true</code> if the component can receive keyboard input
   * focus. This is called from {@link Component#isFocusable()}.
   *
   * This is implemented to return isFocusable() from the Swing component.
   */
  public boolean isFocusable()
  {
    return swingComponent != null ?
             swingComponent.getJComponent().isFocusable() : false;
  }

  /**
   * Returns the minimum size for the component. This is called by
   * {@link Component#minimumSize()}.
   *
   * This is implemented to return the Swing component's minimum size.
   *
   * @return the minimum size for the component
   */
  public Dimension minimumSize()
  {
    Dimension retVal;
    if (swingComponent != null)
      retVal = swingComponent.getJComponent().getMinimumSize();
    else
      retVal = new Dimension(0, 0);
    return retVal;
  }

  /**
   * Returns the preferred size for the component. This is called by
   * {@link Component#getPreferredSize()}.
   *
   * This is implemented to return the Swing component's preferred size.
   *
   * @return the preferred size for the component
   */
  public Dimension preferredSize()
  {
    Dimension retVal;
    if (swingComponent != null)
      retVal = swingComponent.getJComponent().getPreferredSize();
    else
      retVal = new Dimension(0, 0);
    return retVal;
  }

  /**
   * Paints the component. This is triggered by
   * {@link Component#paintAll(Graphics)}.
   *
   * @param graphics the graphics to paint with
   */
  public void paint(Graphics graphics)
  {
    peerPaint(graphics, false);
  }

  /**
   * Prepares an image for rendering on this component. This is called by
   * {@link Component#prepareImage(Image, int, int, ImageObserver)}.
   *
   * @param img the image to prepare
   * @param width the desired width of the rendered image
   * @param height the desired height of the rendered image
   * @param ob the image observer to be notified of updates in the preparation
   *        process
   *
   * @return <code>true</code> if the image has been fully prepared,
   *         <code>false</code> otherwise (in which case the image observer
   *         receives updates)
   */
  public boolean prepareImage(Image img, int width, int height, ImageObserver ob)
  {
    Component parent = awtComponent.getParent();
    if(parent != null)
    {
      ComponentPeer parentPeer = parent.getPeer();
      return parentPeer.prepareImage(img, width, height, ob);
    }
    else
    {
      return Toolkit.getDefaultToolkit().prepareImage(img, width, height, ob);
    }
  }

  public void print(Graphics graphics)
  {
    // FIXME: I don't know what this method is supposed to do.
  }

  /**
   * Repaints the specified rectangle of this component. This is called from
   * {@link Component#repaint(long, int, int, int, int)}.
   *
   * This is implemented to call repaint() on the Swing component.
   *
   * @param tm number of milliseconds to wait with repainting
   * @param x the X coordinate of the upper left corner of the damaged
   *        rectangle
   * @param y the Y coordinate of the upper left corner of the damaged
   *        rectangle
   * @param width the width of the damaged rectangle
   * @param height the height of the damaged rectangle
   */
  public void repaint(long tm, int x, int y, int width, int height)
  {
    // NOTE: This is never called by AWT but is mandated by the peer interface.
    if (swingComponent != null)
      swingComponent.getJComponent().repaint(tm, x, y, width, height);
    else
      {
        PaintEvent ev = new PaintEvent(awtComponent, PaintEvent.UPDATE,
                                       new Rectangle(x, y, width, height));
        Toolkit.getDefaultToolkit().getSystemEventQueue().postEvent(ev);
      }
  }

  /**
   * Requests that this component receives the focus. This is called from
   * {@link Component#requestFocus()}.
   * 
   * This calls requestFocus() on the Swing component.
   *
   * @specnote Part of the earlier 1.1 API, apparently replaced by argument 
   *           form of the same method.
   */
  public void requestFocus()
  {
    // NOTE: This is never called by AWT but is mandated by the peer interface.
    Toolkit tk = Toolkit.getDefaultToolkit();
    EventQueue q = tk.getSystemEventQueue();
    q.postEvent(new FocusEvent(awtComponent, FocusEvent.FOCUS_GAINED, false));
  }

  /**
   * Requests that this component receives the focus. This is called from
   * {@link Component#requestFocus()}.
   *
   * This calls requestFocus() on the Swing component.
   *
   * @param source the actual component that requests focus (may be a
   *        lightweight descendant of the heavyweight container)
   * @param tmp true when the change is temporary
   * @param allowWindowFocus
   * @param tm the timestamp of the focus change
   *
   * @return true when the focus change is guaranteed to be granted, false
   *         otherwise
   */
  public boolean requestFocus(Component source, boolean tmp,
                              boolean allowWindowFocus, long tm)
  {
    Toolkit tk = Toolkit.getDefaultToolkit();
    EventQueue q = tk.getSystemEventQueue();
    q.postEvent(new FocusEvent(source, FocusEvent.FOCUS_GAINED, tmp));
    return true;
  }

  /**
   * Notifies the peer that the bounds of this component have changed. This
   * is called by {@link Component#reshape(int, int, int, int)}.
   *
   * This is implemented to call setBounds() on the Swing component.
   *
   * @param x the X coordinate of the upper left corner of the component
   * @param y the Y coordinate of the upper left corner of the component
   * @param width the width of the component
   * @param height the height of the component
   */
  public void reshape(int x, int y, int width, int height)
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setBounds(x, y, width, height);
  }

  /**
   * Sets the background color of the component. This is called by
   * {@link Component#setBackground(Color)}.
   *
   * This is implemented to call setBackground() on the Swing component.
   *
   * @param color the background color to set
   */
  public void setBackground(Color color)
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setBackground(color);
  }

  /**
   * Notifies the peer that the bounds of this component have changed. This
   * is called by {@link Component#setBounds(int, int, int, int)}.
   *
   * This is implemented to call setBounds() on the Swing component.
   *
   * @param x the X coordinate of the upper left corner of the component
   * @param y the Y coordinate of the upper left corner of the component
   * @param width the width of the component
   * @param height the height of the component
   */
  public void setBounds(int x, int y, int width, int height)
  {
    reshape(x, y, width, height);
  }

  /**
   * Sets the cursor of the component. This is called by
   * {@link Component#setCursor(Cursor)}.
   *
   * This is implemented to call setCursor() on the Swing component.
   *
   * @specnote Part of the earlier 1.1 API, apparently no longer needed.
   */
  public void setCursor(Cursor cursor)
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setCursor(cursor);
  }

  /**
   * Sets the enabled/disabled state of this component. This is called by
   * {@link Component#setEnabled(boolean)}.
   *
   * This is implemented to call setEnabled() on the Swing component.
   *
   * @param enabled <code>true</code> to enable the component,
   *        <code>false</code> to disable it
   */
  public void setEnabled(boolean enabled)
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setEnabled(enabled);
  }

  /**
   * Sets the font of the component. This is called by
   * {@link Component#setFont(Font)}.
   *
   * This is implemented to call setFont() on the Swing component.
   *
   * @param font the font to set
   */
  public void setFont(Font font)
  {
    peerFont = font;
    if (swingComponent != null)
      swingComponent.getJComponent().setFont(font);
  }

  /**
   * Sets the foreground color of the component. This is called by
   * {@link Component#setForeground(Color)}.
   *
   * This is implemented to call setForeground() on the Swing component.
   *
   * @param color the foreground color to set
   */
  public void setForeground(Color color)
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setForeground(color);
  }

  /**
   * Sets the visibility state of the component. This is called by
   * {@link Component#setVisible(boolean)}.
   *
   * This is implemented to call setVisible() on the Swing component.
   *
   * @param visible <code>true</code> to make the component visible,
   *        <code>false</code> to make it invisible
   */
  public void setVisible(boolean visible)
  {
    if (visible)
      show();
    else
      hide();
  }

  /**
   * Makes the component visible. This is called by {@link Component#show()}.
   *
   * This is implemented to call setVisible(true) on the Swing component.
   */
  public void show()
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setVisible(true);
  }

  /** 
   * Get the graphics configuration of the component. The color model
   * of the component can be derived from the configuration.
   *
   * This is implemented to return the GraphicsConfiguration of the parent
   * component. This will eventually call the toplevel component peer, which
   * is expected to provide a real implementation.
   *
   * @return the graphics configuration of the component
   */
  public GraphicsConfiguration getGraphicsConfiguration()
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    return parentPeer.getGraphicsConfiguration();
  }

  /**
   * Part of an older API, no longer needed.
   */
  public void setEventMask(long mask)
  {
    // Nothing to do here.
  }

  /**
   * Returns <code>true</code> if this component has been obscured,
   * <code>false</code> otherwise. This will only work if
   * {@link #canDetermineObscurity()} also returns <code>true</code>.
   *
   * This is not yet implemented.
   *
   * @return <code>true</code> if this component has been obscured,
   *         <code>false</code> otherwise.
   */
  public boolean isObscured()
  {
    return false;
  }

  /**
   * Returns <code>true</code> if this component peer can determine if the
   * component has been obscured, <code>false</code> otherwise.
   *
   * This is not yet implemented.
   *
   * @return <code>true</code> if this component peer can determine if the
   *         component has been obscured, <code>false</code> otherwise
   */
  public boolean canDetermineObscurity()
  {
    return false;
  }

  /**
   * Coalesces the specified paint event.
   *
   * @param e the paint event
   */
  public void coalescePaintEvent(PaintEvent e)
  {
    synchronized (this)
      {
        Rectangle newRect = e.getUpdateRect();
        if (paintArea == null)
          paintArea = newRect;
        else
          Rectangle.union(paintArea, newRect, paintArea);
      }
  }

  /**
   * Updates the cursor. This is not yet implemented.
   */
  public void updateCursorImmediately()
  {
    // Nothing to do here yet.
  }

  /**
   * Returns true, if this component can handle wheel scrolling,
   * <code>false</code> otherwise.
   *
   * This is not yet implemented and returns <code>false</code>. 
   *
   * @return true, if this component can handle wheel scrolling,
   *         <code>false</code> otherwise
   */
  public boolean handlesWheelScrolling()
  {
    return false;
  }

  /**
   * A convenience method that creates a volatile image.  The volatile
   * image is created on the screen device on which this component is
   * displayed, in the device's current graphics configuration.
   *
   * This is implemented to let the parent component peer create an image.
   * This eventually ends up in the toplevel component peer, which is then
   * responsible for creating the real image.
   *
   * @param width width of the image
   * @param height height of the image
   *
   * @see VolatileImage
   *
   * @since 1.2
   */
  public VolatileImage createVolatileImage(int width, int height)
  {
    Component parent = awtComponent.getParent();
    VolatileImage im = null;
    if (parent != null)
      {
        ComponentPeer parentPeer = parent.getPeer();
        im = parentPeer.createVolatileImage(width, height);
      }
    return im;
  }

  /**
   * Create a number of image buffers that implement a buffering
   * strategy according to the given capabilities.
   *
   * This is implemented to forward to the parent component peer. Eventually
   * this ends up in the top level component peer, which is then responsible
   * for doing the real work.
   *
   * @param numBuffers the number of buffers
   * @param caps the buffering capabilities
   *
   * @throws AWTException if the specified buffering strategy is not
   * implemented
   *
   * @since 1.2
   */
  public void createBuffers(int numBuffers, BufferCapabilities caps) throws AWTException
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    parentPeer.createBuffers(numBuffers, caps);
  }

  /**
   * Return the back buffer of this component.
   *
   * This is implemented to forward to the parent. Eventually this ends
   * up in the toplevel component, which is then responsible for providing
   * a back buffer.
   *
   * @return the back buffer of this component.
   *
   * @since 1.2
   */
  public Image getBackBuffer()
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    return parentPeer.getBackBuffer();
  }

  /**
   * Perform a page flip, leaving the contents of the back buffer in
   * the specified state.
   *
   * This is implemented to forward to the parent. Eventually this ends
   * up in the toplevel component, which is then responsible for doing the real
   * work.
   *
   * @param contents the state in which to leave the back buffer
   *
   * @since 1.2
   */
  public void flip(FlipContents contents)
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    parentPeer.flip(contents);
  }

  /**
   * Destroy the resources created by createBuffers.
   *
   * This is implemented to forward to the parent component peer. Eventually
   * this ends up in the top level component peer, which is then responsible
   * for doing the real work.
   *
   * @since 1.2
   */
  public void destroyBuffers()
  {
    Component parent = awtComponent.getParent();
    ComponentPeer parentPeer = parent.getPeer();
    parentPeer.destroyBuffers();
  }

  /**
   * Get the bounds of this component peer.
   *
   * This is implemented to forward to the Swing component.
   *
   * @return component peer bounds
   * @since 1.5
   */
  public Rectangle getBounds()
  {
    Rectangle retVal;
    if (swingComponent != null)
      retVal = swingComponent.getJComponent().getBounds();
    else
      retVal = new Rectangle();
    return retVal;
  }

  /**
   * Reparent this component under another container.
   * 
   * @param parent
   * @since 1.5
   */
  public void reparent(ContainerPeer parent)
  {
    // Nothing to do here.
  }

  /**
   * Set the bounds of this component peer.
   *
   * This is implemented to forward to the swing component.
   *
   * @param x the new x co-ordinate
   * @param y the new y co-ordinate
   * @param width the new width
   * @param height the new height
   * @param z the new stacking level
   * @since 1.5
   */
  public void setBounds(int x, int y, int width, int height, int z)
  {
    if (swingComponent != null)
      swingComponent.getJComponent().setBounds(x, y, width, height);
    // FIXME: Somehow handle the Z order.
  }

  /**
   * Check if this component supports being reparented.
   * 
   * @return true if this component can be reparented,
   * false otherwise.
   * @since 1.5
   */
  public boolean isReparentSupported()
  {
    return true;
  }


  /**
   * Layout this component peer.
   *
   * @since 1.5
   */
  public void layout()
  {
    if (swingComponent != null)
      swingComponent.getJComponent().doLayout();
  }

  /**
   * Triggers 'heavyweight' painting of the components. This usually calls
   * paint() on the Swing component.
   *
   * @param g the graphics context to use for painting
   * @param update wether we need to call update or paint on the AWT component
   */
  protected void peerPaint(Graphics g, boolean update)
  {
    peerPaintComponent(g);

    Graphics userGraphics = g.create();
    try{
    if (update)
      awtComponent.update(userGraphics);
    else
      awtComponent.paint(userGraphics);
    } finally {
        userGraphics.dispose();
    }
    
  }

  /**
   * Paints the actual 'heavyweight' swing component, if there is one
   * associated to this peer.
   * 
   * @param g the graphics to paint the component with
   */
  protected void peerPaintComponent(Graphics g)
  {
    // Paint the actual Swing component if this peer has one.
    if (swingComponent != null)
      swingComponent.getJComponent().paint(g);
  }

  /**
   * Handles mouse events on the component. This is usually forwarded to the
   * SwingComponent's processMouseEvent() method.
   *
   * @param e the mouse event
   */
  protected void handleMouseEvent(MouseEvent e)
  {
    if (swingComponent != null)
      swingComponent.handleMouseEvent(e);
  }

  /**
   * Handles mouse motion events on the component. This is usually forwarded
   * to the SwingComponent's processMouseMotionEvent() method.
   *
   * @param e the mouse motion event
   */
  protected void handleMouseMotionEvent(MouseEvent e)
  {
    if (swingComponent != null)
      swingComponent.handleMouseMotionEvent(e);
  }

  /**
   * Handles key events on the component. This is usually forwarded to the
   * SwingComponent's processKeyEvent() method.
   *
   * @param e the key event
   */
  protected void handleKeyEvent(KeyEvent e)
  {
    if (swingComponent != null)
      swingComponent.handleKeyEvent(e);
  }

  /**
   * Handles focus events on the component. This is usually forwarded to the
   * SwingComponent's processFocusEvent() method.
   *
   * @param e the key event
   */
  protected void handleFocusEvent(FocusEvent e)
  {
    if (swingComponent != null)
      swingComponent.handleFocusEvent(e);
  }

  
  /**
   * Returns the AWT component for this peer.
   *
   * @return the AWT component for this peer
   */
  public Component getComponent()
  {
    return awtComponent;
  }

  public boolean requestFocus(Component lightweightChild, boolean temporary,
                              boolean focusedWindowChangeAllowed,
                              long time, sun.awt.CausedFocusEvent.Cause cause)
  {
    return true;
  }

}
