/* JViewport.java -- 
   Copyright (C) 2002, 2004  Free Software Foundation, Inc.

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


package javax.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;

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
  public static int BACKINGSTORE_SCROLL_MODE = 1;
  public static int BLIT_SCROLL_MODE = 2;
  public static int SIMPLE_SCROLL_MODE = 3;

  ChangeEvent changeEvent = new ChangeEvent(this);

  int scrollMode;

  boolean scrollUnderway;
  boolean isViewSizeSet;

  /** 
   * The width and height of the Viewport's area in terms of view
   * coordinates.  Typically this will be the same as the width and height
   * of the viewport's bounds, unless the viewport transforms units of
   * width and height, which it may do, for example if it magnifies or
   * rotates its view.
   *
   * @see #toViewCoordinates
   */
  Dimension viewExtent;

  Point lastPaintPosition;

  public JViewport()
  {
    setOpaque(true);
    updateUI();
  }

  public Dimension getViewSize()
  {
    if (viewExtent == null)
      return getPreferredSize();
    else
      return viewExtent;
  }

  public void setViewSize(Dimension newSize)
  {
    viewExtent = newSize;
    fireStateChanged();
  }

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
                         getViewSize());
  }

  public boolean isBackingStoreEnabled()
  {
    return scrollMode == BACKINGSTORE_SCROLL_MODE;
  }

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
    add(v);
    fireStateChanged();
  }
    
    
  public void addImpl(Component comp, Object constraints, int index)
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

  public ChangeListener[] getChangeListeners() 
  {
    return (ChangeListener[]) getListeners(ChangeListener.class);
  }

  public void paint(Graphics g)
  {
    paintComponent(g);
  }

  void fireStateChanged()
  {
    ChangeListener[] listeners = getChangeListeners();
    for (int i = 0; i < listeners.length; ++i)
      listeners[i].stateChanged(changeEvent);
  }

  public void addChangeListener(ChangeListener listener)
  {
    listenerList.add(ChangeListener.class, listener);
  }

  public void removeChangeListener(ChangeListener listener)
  {
    listenerList.remove(ChangeListener.class, listener);
  }

  public String getUIClassID()
  {
    return "ViewportUI";
  }

  public void updateUI()
  {
    setUI((ViewportUI) UIManager.getUI(this));
  }            
}
