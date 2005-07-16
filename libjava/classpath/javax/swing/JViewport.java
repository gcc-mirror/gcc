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
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.io.Serializable;

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
 * For example, the {@link javax.JViewPort.getViewPosition} method returns
 * the position <code>(VX,VY)</code> shown above, which is an position in
 * "view space", even though this is <em>implemented</em> by positioning
 * the underlying child at position <code>(-VX,-VY)</code></p>
 *
 */
public class JViewport extends JComponent
{

  /**
   * A {@link java.awt.event.ComponentListener} that listens for
   * changes of the view's size. This class forbids changes of the view
   * component's size that would exceed the viewport's size.
   */
  protected class ViewListener
    extends ComponentAdapter
    implements Serializable
  {
    private static final long serialVersionUID = -2812489404285958070L;

    /**
     * Creates a new instance of ViewListener.
     */
    protected ViewListener()
    {
    }

    /**
     * Receives notification when a component (in this case: the view
     * component) changes it's size.
     *
     * @param ev the ComponentEvent describing the change
     */
    public void componentResized(ComponentEvent ev)
    {
      // According to some tests that I did with Sun's implementation
      // this class is supposed to make sure that the view component
      // is not resized to a larger size than the viewport.
      // This is not documented anywhere. What I did is: I subclassed JViewport
      // and ViewListener and 'disabled' the componentResized method by
      // overriding it and not calling super.componentResized().
      // When this method is disabled I can set the size on the view component
      // normally, when it is enabled, it gets immediatly resized back,
      // after a resize attempt that would exceed the Viewport's size.
      Component comp = ev.getComponent();
      Dimension newSize = comp.getSize();
      Dimension viewportSize = getSize();
      boolean revert = false;
      if (newSize.width > viewportSize.width)
        {
          newSize.width = viewportSize.width;
          revert = true;
        }
      if (newSize.height > viewportSize.height)
        {
          newSize.height = viewportSize.height;
          revert = true;
        }
      if (revert == true)
        comp.setSize(newSize);
    }
  }

  private static final long serialVersionUID = -6925142919680527970L;
  
  public static final int SIMPLE_SCROLL_MODE = 0;
  public static final int BLIT_SCROLL_MODE = 1;
  public static final int BACKINGSTORE_SCROLL_MODE = 2;

  ChangeEvent changeEvent = new ChangeEvent(this);

  int scrollMode;

  protected boolean scrollUnderway;
  protected boolean isViewSizeSet;

  /** 
   * The width and height of the Viewport's area in terms of view
   * coordinates.  Typically this will be the same as the width and height
   * of the viewport's bounds, unless the viewport transforms units of
   * width and height, which it may do, for example if it magnifies or
   * rotates its view.
   *
   * @see #toViewCoordinates
   */
  Dimension extentSize;

  /**
   * The width and height of the view in its own coordinate space.
   */

  Dimension viewSize;

  Point lastPaintPosition;

  /**
   * The ViewListener instance.
   */
  ViewListener viewListener;

  public JViewport()
  {
    setOpaque(true);
    setScrollMode(BLIT_SCROLL_MODE);
    setLayout(createLayoutManager());
    updateUI();
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
      view.setSize(viewSize);
    isViewSizeSet = true;
    fireStateChanged();
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
    Component view = getView();
    if (view != null)
      {
        Point q = new Point(-p.x, -p.y);
        view.setLocation(q);
        fireStateChanged();
      }
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
    while (getComponentCount() > 0)
      {
        if (viewListener != null)
          getView().removeComponentListener(viewListener);
        remove(0);
      }

    if (v != null)
      {
        if (viewListener == null)
          viewListener = createViewListener();
        v.addComponentListener(viewListener);
        add(v);
        fireStateChanged();
      }
  }

  public void revalidate()
  {
    fireStateChanged();
    super.revalidate();
  }

  public void reshape(int x, int y, int w, int h)
  {
    boolean changed = 
      (x != getX()) 
      || (y != getY()) 
      || (w != getWidth())
      || (h != getHeight());
    super.reshape(x, y, w, h);
    if (changed)
      fireStateChanged();
  }
    
  protected void addImpl(Component comp, Object constraints, int index)
  {
    if (getComponentCount() > 0)
      remove(getComponents()[0]);
    
    super.addImpl(comp, constraints, index);
  }

  public final Insets getInsets() 
  {
    return new Insets(0,0,0,0);
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
    
  public boolean isOptimizedDrawingEnabled()
  {
    return false;
  }

  public void paint(Graphics g)
  {
    paintComponent(g);
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

  protected void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();
    for (int i = 0; i < listeners.length; ++i)
      listeners[i].stateChanged(changeEvent);
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
}
