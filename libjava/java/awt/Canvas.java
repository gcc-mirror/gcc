/* Canvas.java --
   Copyright (C) 1999, 2000, 2002, 2004  Free Software Foundation

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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


package java.awt;

import java.awt.image.BufferStrategy;
import java.awt.peer.ComponentPeer;
import java.io.Serializable;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;

/**
 * The <code>Canvas</code> component provides a blank rectangular
 * area, which the client application can use for drawing and for
 * capturing events.  By overriding the <code>paint()</code> method,
 * the canvas can be used for anything from simple line drawings to
 * full-scale custom components.
 *
 * @author Original author unknown
 * @author Tom Tromey  (tromey@redhat.com)
 * @author Andrew John Hughes  (gnu_andrew@member.fsf.org)
 * @since 1.0
 */

public class Canvas
  extends Component
  implements Serializable, Accessible
{

  /**
   * Compatible with Sun's JDK.
   */
  private static final long serialVersionUID = -2284879212465893870L;

  /**
   * The graphics configuration associated with the canvas.
   */
  transient GraphicsConfiguration graphicsConfiguration;

  /**
   * The buffer strategy associated with this canvas.
   */
  transient BufferStrategy bufferStrategy;

  /**
   * Initializes a new instance of <code>Canvas</code>.
   */
  public Canvas() 
  { 
  }

  /**
   * Initializes a new instance of <code>Canvas</code>
   * with the supplied graphics configuration.
   *
   * @param graphicsConfiguration the graphics configuration to use
   *        for this particular canvas.
   */
  public Canvas(GraphicsConfiguration graphicsConfiguration)
  {
    this.graphicsConfiguration = graphicsConfiguration;
  }

  GraphicsConfiguration getGraphicsConfigurationImpl()
  {
    if (graphicsConfiguration != null)
      return graphicsConfiguration;
    return super.getGraphicsConfigurationImpl();
  }

  /**
   * Creates the native peer for this object.
   */
  public void addNotify()
  {
    if (peer == null)
      peer = (ComponentPeer) getToolkit().createCanvas(this);
    super.addNotify();
  }

  /**
   * Repaints the canvas window.  This method should be overridden by 
   * a subclass to do something useful, as this method simply paints
   * the window with the background color.
   *
   * @param gfx the <code>Graphics</code> to use for painting
   */
  public void paint(Graphics gfx)
  {
    /* This implementation doesn't make much sense since the filling
      of background color is guaranteed for heavyweight components
      such as this.  But there's no need to worry, since paint() is
      usually overridden anyway.  */
    gfx.setColor(getBackground());
    Dimension size = getSize();
    gfx.fillRect(0, 0, size.width, size.height);
  }

  /**
   * This class provides accessibility support for the canvas.
   */
  protected class AccessibleAWTCanvas
    extends AccessibleAWTComponent
  {
    /**
     * For compatability with Sun's JDK
     */
    private static final long serialVersionUID = -6325592262103146699L;

    /**
     * Constructor for the accessible canvas.
     */
    protected AccessibleAWTCanvas()
    {
    }

    /**
     * Returns the accessible role for the canvas.
     *
     * @return an instance of <code>AccessibleRole</code>, describing
     *         the role of the canvas.
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.CANVAS;
    }
    
  }

  /**
   * Gets the AccessibleContext associated with this <code>Canvas</code>.
   * The context is created, if necessary.
   *
   * @return the associated context
   */
  public AccessibleContext getAccessibleContext()
  {
    /* Create the context if this is the first request */
    if (accessibleContext == null)
      accessibleContext = new AccessibleAWTCanvas();
    return accessibleContext;
  }

  /**
   * A BltBufferStrategy for canvases.
   */
  private class CanvasBltBufferStrategy extends BltBufferStrategy
  {
    /**
     * Creates a block transfer strategy for this canvas.
     *
     * @param numBuffers the number of buffers in this strategy
     * @param accelerated true if the buffer should be accelerated,
     * false otherwise
     */
    CanvasBltBufferStrategy(int numBuffers, boolean accelerated)
    {
      super(numBuffers,
	    new BufferCapabilities(new ImageCapabilities(accelerated),
				   new ImageCapabilities(accelerated),
				   BufferCapabilities.FlipContents.COPIED));
    }
  }

  /**
   * A FlipBufferStrategy for canvases.
   */
  private class CanvasFlipBufferStrategy extends FlipBufferStrategy
  {
    /**
     * Creates a flip buffer strategy for this canvas.
     *
     * @param numBuffers the number of buffers in this strategy
     *
     * @throws AWTException if the requested number of buffers is not
     * supported
     */
    CanvasFlipBufferStrategy(int numBuffers)
      throws AWTException
    {
      super(numBuffers,
	    new BufferCapabilities(new ImageCapabilities(true),
				   new ImageCapabilities(true),
				   BufferCapabilities.FlipContents.COPIED));
    }
  }

  /**
   * Creates a buffering strategy that manages how this canvas is
   * repainted.  This method attempts to create the optimum strategy
   * based on the desired number of buffers.  Hardware or software
   * acceleration may be used.
   *
   * createBufferStrategy attempts different levels of optimization,
   * but guarantees that some strategy with the requested number of
   * buffers will be created even if it is not optimal.  First it
   * attempts to create a page flipping strategy, then an accelerated
   * blitting strategy, then an unaccelerated blitting strategy.
   *
   * Calling this method causes any existing buffer strategy to be
   * destroyed.
   *
   * @param numBuffers the number of buffers in this strategy
   *
   * @throws IllegalArgumentException if requested number of buffers
   * is less than one
   * @throws IllegalStateException if this canvas is not displayable
   *
   * @since 1.4
   */
  public void createBufferStrategy(int numBuffers)
  {
    if (numBuffers < 1)
      throw new IllegalArgumentException("Canvas.createBufferStrategy: number"
					 + " of buffers is less than one");

    if (!isDisplayable())
      throw new IllegalStateException("Canvas.createBufferStrategy: canvas is"
				      + " not displayable");

    // try a flipping strategy
    try
      {
	bufferStrategy = new CanvasFlipBufferStrategy(numBuffers);
	return;
      }
    catch (AWTException e)
      {
      }

    // try an accelerated blitting strategy
    try
      {
	bufferStrategy = new CanvasBltBufferStrategy(numBuffers, true);
      }
    catch (AWTException e)
      {
      }

    // fall back to an unaccelerated blitting strategy
    try
      {
	bufferStrategy = new CanvasBltBufferStrategy(numBuffers, false);
      }
    catch (AWTException e)
      {
      }
  }

  /**
   * Creates a buffering strategy that manages how this canvas is
   * repainted.  This method attempts to create a strategy based on
   * the specified capabilities and throws an exception if the
   * requested strategy is not supported.
   *
   * Calling this method causes any existing buffer strategy to be
   * destroyed.
   *
   * @param numBuffers the number of buffers in this strategy
   * @param caps the requested buffering capabilities
   *
   * @throws AWTException if the requested capabilities are not
   * supported
   * @throws IllegalArgumentException if requested number of buffers
   * is less than one or if caps is null
   *
   * @since 1.4
   */
  public void createBufferStrategy(int numBuffers,
				   BufferCapabilities caps)
  {
    if (numBuffers < 1)
      throw new IllegalArgumentException("Canvas.createBufferStrategy: number"
					 + " of buffers is less than one");

    if (caps == null)
      throw new IllegalArgumentException("Canvas.createBufferStrategy:"
					 + " capabilities object is null");

    // a flipping strategy was requested
    if (caps.isPageFlipping())
      {
	try
	  {
	    bufferStrategy = new CanvasFlipBufferStrategy(numBuffers);
	  }
	catch (AWTException e)
	  {
	  }
      }
    else
      bufferStrategy = new CanvasBltBufferStrategy(numBuffers, true);
  }

  /**
   * Returns the buffer strategy used by the canvas.
   *
   * @return the buffer strategy.
   * @since 1.4
   */
  public BufferStrategy getBufferStrategy()
  {
    return bufferStrategy;
  }

  /**
   * Updates the canvas in response to a request to
   * <code>repaint()</code> it.  The canvas is cleared
   * with the current background colour, before <code>paint()</code>
   * is called to add the new contents.  Subclasses
   * which override this method should either call this
   * method via <code>super.update(graphics)</code> or re-implement
   * this behaviour, so as to ensure that the canvas is
   * clear before painting takes place.
   *
   * @param graphics the graphics context.
   */
  public void update(Graphics graphics)
  {
    Dimension size;

    /* Clear the canvas */
    size = getSize();
    graphics.clearRect(0, 0, size.width, size.height);
    /* Call the paint method */
    paint(graphics);
  }
}
