/* DropTarget.java -- 
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.

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

package java.awt.dnd;

import java.awt.Point;
import java.awt.Component;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.datatransfer.FlavorMap;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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

  /** @specnote According to the online documentation, this is
   * protected, but in reality it is public.  */
  public static class DropTargetAutoScroller
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
    {
    }

    public void actionPerformed (ActionEvent e)
    {
    }
  }

  private Component component;
  private FlavorMap flavorMap;
  private int actions;
  private DropTargetContext dropTargetContext;
  private DropTargetListener dropTargetListener;
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

    component = c;
    actions = i;
    dropTargetListener = dtl;
    flavorMap = fm;
    
    setActive (b);
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
   * @exception TooManyListenersException If there is already a
   * <code>DropTargetListener</code>.
   */
  public void addDropTargetListener (DropTargetListener dtl)
    throws TooManyListenersException
  {
    if (dtl != null)
      throw new TooManyListenersException ();
    
    dropTargetListener = dtl;
  }

  public void removeDropTargetListener(DropTargetListener dtl)
  {
    // FIXME: Do we need to do something with dtl ?
    dropTargetListener = null;
  }

  public void dragEnter(DropTargetDragEvent dtde)
  {
  }

  public void dragOver(DropTargetDragEvent dtde)
  {
  }

  public void dropActionChanged(DropTargetDragEvent dtde)
  {
  }

  public void dragExit(DropTargetEvent dte)
  {
  }

  public void drop(DropTargetDropEvent dtde)
  {
  }

  public FlavorMap getFlavorMap()
  {
    return flavorMap;
  }

  public void setFlavorMap(FlavorMap fm)
  {
    flavorMap = fm;
  }

  public void addNotify(java.awt.peer.ComponentPeer peer)
  {
  }

  public void removeNotify(java.awt.peer.ComponentPeer peer)
  {
  }

  public DropTargetContext getDropTargetContext()
  {
    if (dropTargetContext == null)
      dropTargetContext = createDropTargetContext ();
    
    return dropTargetContext;
  }

  protected DropTargetContext createDropTargetContext()
  {
    return new DropTargetContext (this);
  }

  protected DropTarget.DropTargetAutoScroller createDropTargetAutoScroller
                                                       (Component c, Point p)
  {
    return new DropTarget.DropTargetAutoScroller (c, p);
  }

  protected void initializeAutoscrolling(Point p)
  {
  }

  protected void updateAutoscroll(Point dragCursorLocn)
  {
  }

  protected void clearAutoscroll()
  {
  }
} // class DropTarget
