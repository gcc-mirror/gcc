/* DropTarget.java -- 
   Copyright (C) 2002, 2003, 2004  Free Software Foundation, Inc.

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


package java.awt.dnd;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.datatransfer.FlavorMap;
import java.awt.datatransfer.SystemFlavorMap;
import java.awt.dnd.peer.DropTargetPeer;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.peer.ComponentPeer;
import java.awt.peer.LightweightPeer;
import java.io.Serializable;
import java.util.EventListener;
import java.util.TooManyListenersException;

import javax.swing.Timer;

/**
 * @author Michael Koch
 * @since 1.2
 */
public class DropTarget
  implements DropTargetListener, EventListener, Serializable
{
  /**
   * Compatible with JDK 1.2+
   */
  private static final long serialVersionUID = -6283860791671019047L;

  protected static class DropTargetAutoScroller
    implements ActionListener
  {
    /**
     * The threshold that keeps the autoscroller running.
     */
    private static final int HYSTERESIS = 10;

    /**
     * The initial timer delay.
     */
    private static final int DELAY = 100;

    private Component component;
    private Point point;

    /**
     * The timer that triggers autoscrolling.
     */
    private Timer timer;

    /**
     * The outer region of the scroller. This is the component's size.
     */
    private Rectangle outer;

    /**
     * The inner region of the scroller. This is the component size without
     * the autoscroll insets.
     */
    private Rectangle inner;

    protected DropTargetAutoScroller (Component c, Point p)
    {
      component = c;
      point = p;
      timer = new Timer(DELAY, this);
      timer.setCoalesce(true);
      timer.start();
    }

    protected void updateLocation (Point newLocn)
    {
      Point previous = point;
      point = newLocn;
      if (Math.abs(point.x - previous.x) > HYSTERESIS
          || Math.abs(point.y - previous.y) > HYSTERESIS)
        {
          if (timer.isRunning())
            timer.stop();
        }
      else
        {
          if (! timer.isRunning())
            timer.start();
        }
    }

    protected void stop ()
    {
      timer.start();
    }

    public void actionPerformed (ActionEvent e)
    {
      Autoscroll autoScroll = (Autoscroll) component;

      // First synchronize the inner and outer rectangles.
      Insets i = autoScroll.getAutoscrollInsets();
      int width = component.getWidth();
      int height = component.getHeight();
      if (width != outer.width || height != outer.height)
        outer.setBounds(0, 0, width, height);
      if (inner.x != i.left || inner.y != i.top)
        inner.setLocation(i.left, i.top);
      int inWidth = width - i.left - i.right;
      int inHeight = height - i.top - i.bottom;
      if (inWidth != inner.width || inHeight != inner.height)
        inner.setSize(inWidth, inHeight);

      // Scroll if the outer rectangle contains the location, but the
      // inner doesn't.
      if (outer.contains(point) && ! inner.contains(point))
        autoScroll.autoscroll(point);
    }
  }

  private Component component;
  private FlavorMap flavorMap;
  private int actions;
  private DropTargetPeer peer;
  private DropTargetContext dropTargetContext;
  private DropTargetListener dropTargetListener;
  private DropTarget.DropTargetAutoScroller autoscroller;
  private boolean active = true;
    
  /**
   * Creates a <code>DropTarget</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public DropTarget ()
  {
    this (null, DnDConstants.ACTION_COPY_OR_MOVE, null, true, null);
  }
  
  /**
   * Creates a <code>DropTarget</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public DropTarget (Component c, DropTargetListener dtl)
  {
    this (c, DnDConstants.ACTION_COPY_OR_MOVE, dtl, true, null);
  }
  
  /**
   * Creates a <code>DropTarget</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public DropTarget (Component c, int i, DropTargetListener dtl)
  {
    this (c, i, dtl, true, null);
  }
  
  /**
   * Creates a <code>DropTarget</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public DropTarget (Component c, int i, DropTargetListener dtl, boolean b)
  {
    this (c, i, dtl, b, null);
  }
  
  /**
   * Creates a <code>DropTarget</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public DropTarget (Component c, int i, DropTargetListener dtl, boolean b,
                     FlavorMap fm)
  {
    if (GraphicsEnvironment.isHeadless ())
      throw new HeadlessException ();

    setComponent(c);
    setDefaultActions(i);
    dropTargetListener = dtl;
    
    if (fm == null)
      flavorMap = SystemFlavorMap.getDefaultFlavorMap();
    else
      flavorMap = fm;
    
    setActive (b);
    
    if (c != null)
      c.setDropTarget(this);
  }

  /**
   * Sets the component associated with this drop target object.
   */
  public void setComponent (Component c)
  {
    if (component != null)
      clearAutoscroll();
    component = c;
  }

  /**
   * Returns the component associated with this drop target object.
   */
  public Component getComponent ()
  {
    return component;
  }

  /**
   * Sets the default actions.
   */
  public void setDefaultActions (int ops)
  {
    actions = ops;
  }

  /**
   * Returns the default actions.
   */
  public int getDefaultActions ()
  {
    return actions;
  }

  public void setActive (boolean active)
  {
    this.active = active;
    if (! active)
      clearAutoscroll();
  }

  public boolean isActive()
  {
    return active;
  }

  /**
   * Adds a new <code>DropTargetListener</code>.
   * 
   * @exception TooManyListenersException Sun's JDK does not, despite
   * documentation, throw this exception here when you install an additional
   * <code>DropTargetListener</code>.  So to be compatible, we do the same
   * thing.
   */
  public void addDropTargetListener (DropTargetListener dtl)
    throws TooManyListenersException
  {
    if (dtl == null)
      return;
    
    if (dtl.equals(this))
      throw new IllegalArgumentException();
    
    if (dropTargetListener != null)
      throw new TooManyListenersException();
    
    dropTargetListener = dtl;
  }

  public void removeDropTargetListener(DropTargetListener dtl)
  {
    if (dropTargetListener != null)
      dropTargetListener = null;
  }

  public void dragEnter(DropTargetDragEvent dtde)
  {
    if (active)
      {
        if (dropTargetListener != null)
          dropTargetListener.dragEnter(dtde);
        initializeAutoscrolling(dtde.getLocation());
      }
  }

  public void dragOver(DropTargetDragEvent dtde)
  {
    if (active)
      {
        if (dropTargetListener != null)
          dropTargetListener.dragOver(dtde);
        updateAutoscroll(dtde.getLocation());
      }
  }

  public void dropActionChanged(DropTargetDragEvent dtde)
  {
    if (active)
      {
        if (dropTargetListener != null)
          dropTargetListener.dropActionChanged(dtde);
        updateAutoscroll(dtde.getLocation());
      }
  }

  public void dragExit(DropTargetEvent dte)
  {
    if (active)
      {
        if (dropTargetListener != null)
          dropTargetListener.dragExit(dte);
        clearAutoscroll();
      }
  }

  public void drop(DropTargetDropEvent dtde)
  {
    clearAutoscroll();
    if (dropTargetListener != null)
      dropTargetListener.drop(dtde);
  }

  public FlavorMap getFlavorMap()
  {
    return flavorMap;
  }

  public void setFlavorMap(FlavorMap fm)
  {
    flavorMap = fm;
  }

  public void addNotify(ComponentPeer p)
  {
    Component c = component;
    while (c != null && p instanceof LightweightPeer)
      {
        p = c.getPeer();
        c = c.getParent();
      }

    if (p instanceof DropTargetPeer)
      {
        peer = ((DropTargetPeer) p);
        peer.addDropTarget(this);
      }
    else
      peer = null;
  }

  public void removeNotify(ComponentPeer p)
  {
    ((DropTargetPeer) peer).removeDropTarget(this);
    peer = null;
    p = null;
  }

  public DropTargetContext getDropTargetContext()
  {
    if (dropTargetContext == null)
      dropTargetContext = createDropTargetContext ();
    
    return dropTargetContext;
  }

  protected DropTargetContext createDropTargetContext()
  {
    if (dropTargetContext == null)
      dropTargetContext = new DropTargetContext (this);
    
    return dropTargetContext;
  }

  protected DropTarget.DropTargetAutoScroller createDropTargetAutoScroller
                                                       (Component c, Point p)
  {
    return new DropTarget.DropTargetAutoScroller (c, p);
  }

  protected void initializeAutoscrolling(Point p)
  {
    if (component instanceof Autoscroll) // Checks for null too.
      autoscroller = createDropTargetAutoScroller (component, p);
  }

  protected void updateAutoscroll(Point dragCursorLocn)
  {
    if (autoscroller != null)
      autoscroller.updateLocation(dragCursorLocn);
  }

  protected void clearAutoscroll()
  {
    if (autoscroller != null)
      {
        autoscroller.stop();
        autoscroller = null;
      }
  }
} // class DropTarget
