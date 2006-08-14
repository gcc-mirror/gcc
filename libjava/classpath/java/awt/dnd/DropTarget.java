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

import gnu.classpath.NotImplementedException;

import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Point;
import java.awt.datatransfer.FlavorMap;
import java.awt.dnd.peer.DropTargetPeer;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.peer.ComponentPeer;
import java.awt.peer.LightweightPeer;
import java.io.Serializable;
import java.util.EventListener;
import java.util.TooManyListenersException;

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
    private Component component;
    private Point point;
    
    protected DropTargetAutoScroller (Component c, Point p)
    {
      component = c;
      point = p;
    }

    protected void updateLocation (Point newLocn)
    {
      point = newLocn;
    }

    protected void stop ()
      throws NotImplementedException
    {
      // FIXME: implement this
    }

    public void actionPerformed (ActionEvent e)
      throws NotImplementedException
    {
      // FIXME: implement this
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
    this (null, 0, null, true, null);
  }
  
  /**
   * Creates a <code>DropTarget</code> object.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless()
   * returns true.
   */
  public DropTarget (Component c, DropTargetListener dtl)
  {
    this (c, 0, dtl, true, null);
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
    if (dropTargetListener != null)
      throw new TooManyListenersException ();
    
    dropTargetListener = dtl;
  }

  public void removeDropTargetListener(DropTargetListener dtl)
  {
    if (dropTargetListener != null)
      dropTargetListener = null;
  }

  public void dragEnter(DropTargetDragEvent dtde)
  {
    if (dropTargetListener != null)
      dropTargetListener.dragEnter(dtde);
  }

  public void dragOver(DropTargetDragEvent dtde)
  {
    if (dropTargetListener != null)
      dropTargetListener.dragOver(dtde);
  }

  public void dropActionChanged(DropTargetDragEvent dtde)
  {
    if (dropTargetListener != null)
      dropTargetListener.dropActionChanged(dtde);
  }

  public void dragExit(DropTargetEvent dte)
  {
    if (dropTargetListener != null)
      dropTargetListener.dragExit(dte);
  }

  public void drop(DropTargetDropEvent dtde)
  {
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
    if (autoscroller == null)
      autoscroller = new DropTarget.DropTargetAutoScroller (c, p);
    
    return autoscroller;
  }

  protected void initializeAutoscrolling(Point p)
  {
    createDropTargetAutoScroller (component, p);
  }

  protected void updateAutoscroll(Point dragCursorLocn)
  {
    if (autoscroller != null)
      autoscroller.updateLocation(dragCursorLocn);
  }

  protected void clearAutoscroll()
  {
    autoscroller = null;
  }
} // class DropTarget
