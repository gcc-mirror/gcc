/* DropTarget.java -- 
   Copyright (C) 2002 Free Software Foundation, Inc.

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
import java.awt.datatransfer.FlavorMap;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.Serializable;
import java.util.EventListener;
import java.util.TooManyListenersException;

/** STUB CLASS ONLY */
public class DropTarget
  implements DropTargetListener, EventListener, Serializable
{
  protected static class DropTargetAutoScroller
    implements ActionListener
  {
    protected DropTargetAutoScroller (Component c, Point p)
    {
    }

    protected void updateLocation (Point newLocn)
    {
    }

    protected void stop ()
    {
    }

    public void actionPerformed (ActionEvent e)
    {
    }
  }
  
  // FIXME: check the correctness of default value.
  private boolean isActive = false;
    
  /**
   * FIXME
   *
   * @exception HeadlessException FIXME
   */
  public DropTarget ()
  {
  }
  
  /**
   * FIXME
   *
   * @exception HeadlessException FIXME
   */
  public DropTarget (Component c, DropTargetListener dtl)
  {
  }
  
  /**
   * FIXME
   *
   * @exception HeadlessException FIXME
   */
  public DropTarget (Component c, int i, DropTargetListener dtl)
  {
  }
  
  /**
   * FIXME
   *
   * @exception HeadlessException FIXME
   */
  public DropTarget (Component c, int i, DropTargetListener dtl, boolean b)
  {
  }
  
  /**
   * FIXME
   *
   * @exception HeadlessException FIXME
   */
  public DropTarget (Component c, int i, DropTargetListener dtl, boolean b,
		     FlavorMap fm)
  {
  }

  public void setComponent (Component c)
  {
  }

  public Component getComponent ()
  {
    return null;
  }

  public void setDefaultActions (int ops)
  {
  }

  public int getDefaultActions ()
  {
    return 0;
  }

  public void setActive(boolean isActive)
  {
    this.isActive = isActive;
  }

  public boolean isActive()
  {
    return this.isActive;
  }

  /**
   * @exception TooManyListenersException FIXME
   */
  public void addDropTargetListener (DropTargetListener dtl)
    throws TooManyListenersException
  {
  }

  public void removeDropTargetListener(DropTargetListener dtl)
  {
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
    return null;
  }

  public void setFlavorMap(FlavorMap fm)
  {
  }

  public void addNotify(java.awt.peer.ComponentPeer peer)
  {
  }

  public void removeNotify(java.awt.peer.ComponentPeer peer)
  {
  }

  public DropTargetContext getDropTargetContext()
  {
    return null;
  }

  protected DropTargetContext createDropTargetContext()
  {
    return null;
  }

  protected DropTarget.DropTargetAutoScroller createDropTargetAutoScroller
                                                       (Component c, Point p)
  {
    return null;
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
