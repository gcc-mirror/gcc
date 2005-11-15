/* JViewport.java -- 
   Copyright (C) 2002, 2004, 2005  Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.Serializable;

import javax.accessibility.Accessible;
import javax.accessibility.AccessibleContext;
import javax.accessibility.AccessibleRole;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ViewportUI;

/**
 *  
 * <pre>
 *                                                     _
 *   +-------------------------------+    ...........Y1 \
 *   |  view                         |                .  \
 *   |  (this component's child)     |                .   > VY
 *   |                               |                .  / = Y2-Y1
 *   |         +------------------------------+  ....Y2_/
 *   |         | viewport            |        |       .
 *   |         | (this component)    |        |       .
 *   |         |                     |        |       .
 *   |         |                     |        |       .
 *   |         |                     |        |       .
 *   |         |                     |        |       .
 *   |         +------------------------------+  ....Y3
 *   |                               |                .
 *   |         .                     |        .       .
 *   |         .                     |        .       .
 *   +---------.---------------------+    ...........Y4
 *   .         .                     .        .
 *   .         .                     .        .
 *   .         .                     .        .
 *   X1.......X2.....................X3.......X4
 *   \____  ___/
 *        \/
 *        VX = X2-X1
 *</pre>
 *  
 * <p>A viewport is, like all swing components, located at some position in
 * the swing component tree; that location is exactly the same as any other
 * components: the viewport's "bounds".</p>
 *
 * <p>But in terms of drawing its child, the viewport thinks of itself as
 * covering a particular position <em>of the view's coordinate space</em>.
 * For example, the {@link #getViewPosition} method returns
 * the position <code>(VX,VY)</code> shown above, which is an position in
 * "view space", even though this is <em>implemented</em> by positioning
 * the underlying child at position <code>(-VX,-VY)</code></p>
 *
 */
public class JViewport extends JComponent implements Accessible
{
  /**
   * Provides accessibility support for <code>JViewport</code>.
   *
   * @author Roman Kennke (roman@kennke.org)
   */
  protected class AccessibleJViewport extends AccessibleJComponent
  {
    /**
     * Creates a new instance of <code>AccessibleJViewport</code>.
     */
    public AccessibleJViewport()
    {
      // Nothing to do here.
    }

    /**
     * Returns the accessible role of <code>JViewport</code>, which is
     * {@link AccessibleRole#VIEWPORT}.
     *
     * @return the accessible role of <code>JViewport</code>
     */
    public AccessibleRole getAccessibleRole()
    {
      return AccessibleRole.VIEWPORT;
    }
  }

  /**
   * A {@link java.awt.event.ComponentListener} that listens for
   * changes of the view's size. This triggers a revalidate() call on the
   * viewport.
   */
  protected class ViewListener extends ComponentAdapter implements Serializable
  {
    private static final long serialVersionUID = -2812489404285958070L;

    /**
     * Creates a new instance of ViewListener.
     */
    protected ViewListener()
    {
      // Nothing to do here.
    }

    /**
     * Receives notification when a component (in this case: the view
     * component) changes it's size. This simply triggers a revalidate() on the
     * viewport.
     *
     * @param ev the ComponentEvent describing the change
     */
    public void componentResized(ComponentEvent ev)
    {
      revalidate();
    }
  }

  public static final int SIMPLE_SCROLL_MODE = 0;
  public static final int BLIT_SCROLL_MODE = 1;
  public static final int BACKINGSTORE_SCROLL_MODE = 2;

  private static final long serialVersionUID = -6925142919680527970L;
  
  protected boolean scrollUnderway;
  protected boolean isViewSizeSet;

  /**
   * This flag indicates whether we use a backing store for drawing.
   *
   * @deprecated since JDK 1.3
   */
  protected boolean backingStore;

  /**
   * The backingstore image used for the backingstore and blit scroll methods.
   */
  protected Image backingStoreImage;

  /**
   * The position at which the view has been drawn the last time. This is used
   * to determine the bittable area.
   */
  protected Point lastPaintPosition;

  ChangeEvent changeEvent = new ChangeEvent(this);

  int scrollMode;

  /** 
   * The width and height of the Viewport's area in terms of view
   * coordinates.  Typically this will be the same as the width and height
   * of the viewport's bounds, unless the viewport transforms units of
   * width and height, which it may do, for example if it magnifies or
   * rotates its view.
   *
   * @see #toViewCoordinates(Dimension)
   */
  Dimension extentSize;

  /**
   * The width and height of the view in its own coordinate space.
   */
  Dimension viewSize;

  /**
   * The ViewListener instance.
   */
  ViewListener viewListener;

  /**
   * Stores the location from where to blit. This is a cached Point object used
   * in blitting calculations.
   */
  Point cachedBlitFrom;

  /**
   * Stores the location where to blit to. This is a cached Point object used
   * in blitting calculations.
   */
  Point cachedBlitTo;

  /**
   * Stores the width of the blitted area. This is a cached Dimension object
   * used in blitting calculations.
   */
  Dimension cachedBlitSize;

  /**
   * Stores the bounds of the area that needs to be repainted. This is a cached
   * Rectangle object used in blitting calculations. 
   */
  Rectangle cachedBlitPaint;

  boolean damaged = true;

  /**
   * A flag indicating if the size of the viewport has changed since the
   * last repaint. This is used in double buffered painting to check if we
   * need a new double buffer, or can reuse the old one.
   */
  boolean sizeChanged = true;

  public JViewport()
  {
    setOpaque(true);
    String scrollModeProp =
      System.getProperty("gnu.javax.swing.JViewport.scrollMode",
                         "BLIT");
    int myScrollMode;
    if (scrollModeProp.equalsIgnoreCase("simple"))
      myScrollMode = SIMPLE_SCROLL_MODE;
    else if (scrollModeProp.equalsIgnoreCase("backingstore"))
      myScrollMode = BACKINGSTORE_SCROLL_MODE;
    else
      myScrollMode = BLIT_SCROLL_MODE;
    setScrollMode(myScrollMode);

    updateUI();
    setLayout(createLayoutManager());
    lastPaintPosition = new Point();
    cachedBlitFrom = new Point();
    cachedBlitTo = new Point();
    cachedBlitSize = new Dimension();
    cachedBlitPaint = new Rectangle();
  }

  public Dimension getExtentSize()
  {
    if (extentSize == null)
      return toViewCoordinates(getSize());
    else
      return extentSize;
  }

  public Dimension toViewCoordinates(Dimension size)
  {
    return size;
  }

  public Point toViewCoordinates(Point p)
  {
    Point pos = getViewPosition();
    return new Point(p.x + pos.x,
                     p.y + pos.y);
  }

  public void setExtentSize(Dimension newSize)
  {
    extentSize = newSize;
    fireStateChanged();
  }

  /**
   * Returns the viewSize when set, or the preferred size of the set
   * Component view.  If no viewSize and no Component view is set an
   * empty Dimension is returned.
   */
  public Dimension getViewSize()
  {
    if (isViewSizeSet)
      return viewSize;
    else
      {
	Component view = getView();
	if (view != null)
	  return view.getPreferredSize();
	else
	  return new Dimension();
      }
  }


  public void setViewSize(Dimension newSize)
  {
    viewSize = newSize;
    Component view = getView();
    if (view != null)
      {
        if (newSize != view.getSize())
          {
            view.setSize(viewSize);
            fireStateChanged();
          }
      }
    isViewSizeSet = true;
  }

  /**
   * Get the viewport's position in view space. Despite confusing name,
   * this really does return the viewport's (0,0) position in view space,
   * not the view's position.
   */

  public Point getViewPosition()
  {
    Component view = getView();
    if (view == null)
      return new Point(0,0);
    else
      {
        Point p = view.getLocation();
        p.x = -p.x;
        p.y = -p.y;
        return p;
      }
  }

  public void setViewPosition(Point p)
  {
    if (getViewPosition().equals(p))
      return;
    Component view = getView();
    if (view != null)
      {
        Point q = new Point(-p.x, -p.y);
        view.setLocation(q);
        isViewSizeSet = false;
        fireStateChanged();
      }
    repaint();
  }

  public Rectangle getViewRect()
  {
    return new Rectangle(getViewPosition(), 
                         getExtentSize());
  }

  /**
   * @deprecated 1.4
   */
  public boolean isBackingStoreEnabled()
  {
    return scrollMode == BACKINGSTORE_SCROLL_MODE;
  }

  /**
   * @deprecated 1.4
   */
  public void setBackingStoreEnabled(boolean b)
  {
    if (b && scrollMode != BACKINGSTORE_SCROLL_MODE)
      {
        scrollMode = BACKINGSTORE_SCROLL_MODE;
        fireStateChanged();
      }
  }

  public void setScrollMode(int mode)
  {
    scrollMode = mode;
    fireStateChanged();
  }

  public int getScrollMode()
  {
    return scrollMode;
  }

  public Component getView()
  {
    if (getComponentCount() == 0)
      return null;
  
    return getComponents()[0];
  }

  public void setView(Component v)
  {
    if (viewListener != null)
      getView().removeComponentListener(viewListener);

    if (v != null)
      {
        if (viewListener == null)
          viewListener = createViewListener();
        v.addComponentListener(viewListener);
        add(v);
        fireStateChanged();
      }
    revalidate();
    repaint();
  }

  public void reshape(int x, int y, int w, int h)
  {
    if (w != getWidth() || h != getHeight())
      sizeChanged = true;
    super.reshape(x, y, w, h);
    if (sizeChanged)
      {
        damaged = true;
        fireStateChanged();
      }
  }

  public final Insets getInsets()
  {
    return new Insets(0, 0, 0, 0);
  }

  public final Insets getInsets(Insets insets)
  {
    if (insets == null)
      return getInsets();
    insets.top = 0;
    insets.bottom = 0;
    insets.left = 0;
    insets.right = 0;
    return insets;
  }
    

  /**
   * Overridden to return <code>false</code>, so the JViewport's paint method
   * gets called instead of directly calling the children. This is necessary
   * in order to get a useful clipping and translation on the children.
   *
   * @return <code>false</code>
   */
  public boolean isOptimizedDrawingEnabled()
  {
    return false;
  }

  public void paint(Graphics g)
  {
    Component view = getView();

    if (view == null)
      return;

    Point pos = getViewPosition();
    Rectangle viewBounds = view.getBounds();
    Rectangle portBounds = getBounds();

    if (viewBounds.width == 0 
        || viewBounds.height == 0
        || portBounds.width == 0
        || portBounds.height == 0)
      return;

    switch (getScrollMode())
      {

      case JViewport.BACKINGSTORE_SCROLL_MODE:
        paintBackingStore(g);
        break;
      case JViewport.BLIT_SCROLL_MODE:
        paintBlit(g);
        break;
      case JViewport.SIMPLE_SCROLL_MODE:
      default:
        paintSimple(g);
        break;
      }
    damaged = false;
  }

  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  public ChangeListener[] getChangeListeners() 
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }

  /**
   * This method returns the String ID of the UI class of  Separator.
   *
   * @return The UI class' String ID.
   */
  public String getUIClassID()
  {
    return "ViewportUI";
  }

  /**
   * This method resets the UI used to the Look and Feel defaults..
   */
  public void updateUI()
  {
    setUI((ViewportUI) UIManager.getUI(this));
  }            

  /**
   * This method returns the viewport's UI delegate.
   *
   * @return The viewport's UI delegate.
   */
  public ViewportUI getUI()
  {
    return (ViewportUI) ui;
  }

  /**
   * This method sets the viewport's UI delegate.
   *
   * @param ui The viewport's UI delegate.
   */
  public void setUI(ViewportUI ui)
  {
    super.setUI(ui);
  }

  public final void setBorder(Border border)
  {
    if (border != null)
      throw new IllegalArgumentException();
  }

  /**
   * Scrolls the view so that contentRect becomes visible.
   *
   * @param contentRect the rectangle to make visible within the view
   */
  public void scrollRectToVisible(Rectangle contentRect)
  {
    Component view = getView();
    if (view == null)
      return;    
      
    Point pos = getViewPosition();
    Rectangle viewBounds = getView().getBounds();
    Rectangle portBounds = getBounds();
    
    if (isShowing())
      getView().validate();

    // If the bottom boundary of contentRect is below the port
    // boundaries, scroll up as necessary.
    if (contentRect.y + contentRect.height + viewBounds.y > portBounds.height)
      pos.y = contentRect.y + contentRect.height - portBounds.height;
    // If contentRect.y is above the port boundaries, scroll down to
    // contentRect.y.
    if (contentRect.y + viewBounds.y < 0)
      pos.y = contentRect.y;
    // If the right boundary of contentRect is right from the port
    // boundaries, scroll left as necessary.
    if (contentRect.x + contentRect.width + viewBounds.x > portBounds.width)
      pos.x = contentRect.x + contentRect.width - portBounds.width;
    // If contentRect.x is left from the port boundaries, scroll right to
    // contentRect.x.
    if (contentRect.x + viewBounds.x < 0)
      pos.x = contentRect.x;
    setViewPosition(pos);
  }

  /**
   * Returns the accessible context for this <code>JViewport</code>. This
   * will be an instance of {@link AccessibleJViewport}.
   *
   * @return the accessible context for this <code>JViewport</code>
   */
  public AccessibleContext getAccessibleContext()
  {
    if (accessibleContext == null)
      accessibleContext = new AccessibleJViewport();
    return accessibleContext;
  }

  /**
   * Forward repaint to parent to make sure only one paint is performed by the
   * RepaintManager.
   *
   * @param tm number of milliseconds to defer the repaint request
   * @param x the X coordinate of the upper left corner of the dirty area
   * @param y the Y coordinate of the upper left corner of the dirty area
   * @param w the width of the dirty area
   * @param h the height of the dirty area
   */
  public void repaint(long tm, int x, int y, int w, int h)
  {
    Component parent = getParent();
    if (parent != null)
      {
        parent.repaint(tm, x + getX(), y + getY(), w, h);
      }
  }

  protected void addImpl(Component comp, Object constraints, int index)
  {
    if (getComponentCount() > 0)
      remove(getComponents()[0]);
    
    super.addImpl(comp, constraints, index);
  }

  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();
    for (int i = 0; i < listeners.length; ++i)
      listeners[i].stateChanged(changeEvent);
  }

  /**
   * Creates a {@link ViewListener} that is supposed to listen for
   * size changes on the view component.
   *
   * @return a ViewListener instance
   */
  protected ViewListener createViewListener()
  {
    return new ViewListener();
  }

  /**
   * Creates the LayoutManager that is used for this viewport. Override
   * this method if you want to use a custom LayoutManager.
   *
   * @return a LayoutManager to use for this viewport
   */
  protected LayoutManager createLayoutManager()
  {
    return new ViewportLayout();
  }

  /**
   * Computes the parameters for the blitting scroll method. <code>dx</code>
   * and <code>dy</code> specifiy the X and Y offset by which the viewport
   * is scrolled. All other arguments are output parameters and are filled by
   * this method.
   *
   * <code>blitFrom</code> holds the position of the blit rectangle in the
   * viewport rectangle before scrolling, <code>blitTo</code> where the blitArea
   * is copied to.
   *
   * <code>blitSize</code> holds the size of the blit area and
   * <code>blitPaint</code> is the area of the view that needs to be painted.
   *
   * This method returns <code>true</code> if blitting is possible and
   * <code>false</code> if the viewport has to be repainted completetly without
   * blitting.
   *
   * @param dx the horizontal delta
   * @param dy the vertical delta
   * @param blitFrom the position from where to blit; set by this method
   * @param blitTo the position where to blit area is copied to; set by this
   *        method
   * @param blitSize the size of the blitted area; set by this method
   * @param blitPaint the area that needs repainting; set by this method
   *
   * @return <code>true</code> if blitting is possible,
   *         <code>false</code> otherwise
   */
  protected boolean computeBlit(int dx, int dy, Point blitFrom, Point blitTo,
                                Dimension blitSize, Rectangle blitPaint)
  {
    if ((dx != 0 && dy != 0) || damaged)
      // We cannot blit if the viewport is scrolled in both directions at
      // once.
      return false;

    Rectangle portBounds = SwingUtilities.calculateInnerArea(this, getBounds());

    // Compute the blitFrom and blitTo parameters.
    blitFrom.x = portBounds.x;
    blitFrom.y = portBounds.y;
    blitTo.x = portBounds.x;
    blitTo.y = portBounds.y;

    if (dy > 0)
      {
        blitFrom.y = portBounds.y + dy;
      }
    else if (dy < 0)
      {
        blitTo.y = portBounds.y - dy;
      }
    else if (dx > 0)
      {
        blitFrom.x = portBounds.x + dx;
      }
    else if (dx < 0)
      {
        blitTo.x = portBounds.x - dx;
      }

    // Compute size of the blit area.
    if (dx != 0)
      {
        blitSize.width = portBounds.width - Math.abs(dx);
        blitSize.height = portBounds.height;
      }
    else if (dy != 0)
      {
        blitSize.width = portBounds.width;
        blitSize.height = portBounds.height - Math.abs(dy);
      }

    // Compute the blitPaint parameter.
    blitPaint.setBounds(portBounds);
    if (dy > 0)
      {
        blitPaint.y = portBounds.y + portBounds.height - dy;
        blitPaint.height = dy;
      }
    else if (dy < 0)
      {
        blitPaint.height = -dy;
      }
    if (dx > 0)
      {
        blitPaint.x = portBounds.x + portBounds.width - dx;
        blitPaint.width = dx;
      }
    else if (dx < 0)
      {
        blitPaint.width = -dx;
      }

    return true;
  }

  /**
   * Paints the viewport in case we have a scrollmode of
   * {@link #SIMPLE_SCROLL_MODE}.
   *
   * This simply paints the view directly on the surface of the viewport.
   *
   * @param g the graphics context to use
   */
  void paintSimple(Graphics g)
  {
    Point pos = getViewPosition();
    Component view = getView();
    boolean translated = false;
    try
      {
        g.translate(-pos.x, -pos.y);
        translated = true;
        view.paint(g);
      } 
    finally
      {
        if (translated)
          g.translate (pos.x, pos.y);
      }
  }

  /**
   * Paints the viewport in case we have a scroll mode of
   * {@link #BACKINGSTORE_SCROLL_MODE}.
   *
   * This method uses a backing store image to paint the view to, which is then
   * subsequently painted on the screen. This should make scrolling more
   * smooth.
   *
   * @param g the graphics context to use
   */
  void paintBackingStore(Graphics g)
  {
    // If we have no backing store image yet or the size of the component has
    // changed, we need to rebuild the backing store.
    if (backingStoreImage == null || sizeChanged)
      {
        backingStoreImage = createImage(getWidth(), getHeight());
        sizeChanged = false;
        Graphics g2 = backingStoreImage.getGraphics();
        paintSimple(g2);
        g2.dispose();
      }
    // Otherwise we can perform the blitting on the backing store image:
    // First we move the part that remains visible after scrolling, then
    // we only need to paint the bit that becomes newly visible.
    else
      {
        Graphics g2 = backingStoreImage.getGraphics();
        Point viewPosition = getViewPosition();
        int dx = viewPosition.x - lastPaintPosition.x;
        int dy = viewPosition.y - lastPaintPosition.y;
        boolean canBlit = computeBlit(dx, dy, cachedBlitFrom, cachedBlitTo,
                                      cachedBlitSize, cachedBlitPaint);
        if (canBlit)
          {
            // Copy the part that remains visible during scrolling.
            g2.copyArea(cachedBlitFrom.x, cachedBlitFrom.y,
                        cachedBlitSize.width, cachedBlitSize.height,
                        cachedBlitTo.x - cachedBlitFrom.x,
                        cachedBlitTo.y - cachedBlitFrom.y);
            // Now paint the part that becomes newly visible.
            g2.setClip(cachedBlitPaint.x, cachedBlitPaint.y,
                       cachedBlitPaint.width, cachedBlitPaint.height);
            paintSimple(g2);
          }
        // If blitting is not possible for some reason, fall back to repainting
        // everything.
        else
          {
            paintSimple(g2);
          }
        g2.dispose();
      }
    // Actually draw the backingstore image to the graphics context.
    g.drawImage(backingStoreImage, 0, 0, this);
    // Update the lastPaintPosition so that we know what is already drawn when
    // we paint the next time.
    lastPaintPosition.setLocation(getViewPosition());
  }

  /**
   * Paints the viewport in case we have a scrollmode of
   * {@link #BLIT_SCROLL_MODE}.
   *
   * This paints the viewport using a backingstore and a blitting algorithm.
   * Only the newly exposed area of the view is painted from the view painting
   * methods, the remainder is copied from the backing store.
   *
   * @param g the graphics context to use
   */
  void paintBlit(Graphics g)
  {
    // We cannot perform blitted painting as it is described in Sun's API docs.
    // There it is suggested that this painting method should blit directly
    // on the parent window's surface. This is not possible because when using
    // Swing's double buffering (at least our implementation), it would
    // immediatly be painted when the buffer is painted on the screen. For this
    // to work we would need a kind of hole in the buffer image. And honestly
    // I find this method not very elegant.
    // The alternative, blitting directly on the buffer image, is also not
    // possible because the buffer image gets cleared everytime when an opaque
    // parent component is drawn on it.

    // What we do instead is falling back to the backing store approach which
    // is in fact a mixed blitting/backing store approach where the blitting
    // is performed on the backing store image and this is then drawn to the
    // graphics context. This is very robust and works independent of the
    // painting mechanism that is used by Swing. And it should have comparable
    // performance characteristics as the blitting method.
    paintBackingStore(g);
  }
}
