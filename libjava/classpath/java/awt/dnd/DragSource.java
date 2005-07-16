/* DragSource.java --
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
import java.awt.Cursor;
import java.awt.GraphicsEnvironment;
import java.awt.HeadlessException;
import java.awt.Image;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.datatransfer.FlavorMap;
import java.awt.datatransfer.SystemFlavorMap;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.peer.DragSourceContextPeer;
import java.io.Serializable;
import java.util.EventListener;

/**
 * @since 1.2
 */
public class DragSource implements Serializable
{
  /**
   * Compatible with JDK 1.2+.
   */
  private static final long serialVersionUID = 6236096958971414066L;

  public static final Cursor DefaultCopyDrop = null;
  public static final Cursor DefaultMoveDrop = null;
  public static final Cursor DefaultLinkDrop = null;
  public static final Cursor DefaultCopyNoDrop = null;
  public static final Cursor DefaultMoveNoDrop = null;
  public static final Cursor DefaultLinkNoDrop = null;

  private transient FlavorMap flavorMap = SystemFlavorMap.getDefaultFlavorMap ();

  private transient DragSourceListener dragSourceListener;
  private transient DragSourceMotionListener dragSourceMotionListener;

  /**
   * Initializes the drag source.
   *
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public DragSource()
  {
    if (GraphicsEnvironment.isHeadless())
      throw new HeadlessException ();
  }

  /**
   * @exception HeadlessException If GraphicsEnvironment.isHeadless() is true.
   */
  public static DragSource getDefaultDragSource()
  {
    return null;
  }

  public static boolean isDragImageSupported()
  {
    return false;
  }

  /**
   * Start a drag, given the DragGestureEvent that initiated the drag.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(DragGestureEvent trigger, Cursor dragCursor,
                        Image dragImage, Point imageOffset,
                        Transferable trans, DragSourceListener dsl,
                        FlavorMap map)
  {
  }

  /**
   * Start a drag, given the DragGestureEvent that initiated the drag.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(DragGestureEvent trigger, Cursor dragCursor,
                        Transferable trans, DragSourceListener dsl,
                        FlavorMap map)
  {
    startDrag(trigger, dragCursor, null, null, trans, dsl, map);
  }

  /**
   * Start a drag, given the DragGestureEvent that initiated the drag.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(DragGestureEvent trigger, Cursor dragCursor,
                        Image dragImage, Point imageOffset,
                        Transferable trans, DragSourceListener dsl)
  {
    startDrag(trigger, dragCursor, dragImage, imageOffset, trans, dsl, null);
  }

  /**
   * Start a drag, given the DragGestureEvent that initiated the drag.
   *
   * @exception InvalidDnDOperationException If the Drag and Drop system is
   * unable to initiate a drag operation, or if the user attempts to start
   * a drag while an existing drag operation is still executing.
   */
  public void startDrag(DragGestureEvent trigger, Cursor dragCursor,
                        Transferable trans, DragSourceListener dsl)
  {
    startDrag(trigger, dragCursor, null, null, trans, dsl, null);
  }

  /**
   * Creates the DragSourceContext to handle this drag.
   *
   * @exception IllegalArgumentException FIXME
   * @exception NullPointerException If dscp, dgl, dragImage or t is null.
   */
  protected DragSourceContext
    createDragSourceContext(DragSourceContextPeer peer, DragGestureEvent dge,
                            Cursor cursor, Image image, Point offset,
                            Transferable t, DragSourceListener dsl)
  {
    return null;
  }

  public FlavorMap getFlavorMap()
  {
    return flavorMap;
  }

  public DragGestureRecognizer
    createDragGestureRecognizer(Class recognizer, Component c, int actions,
                                DragGestureListener dgl)
  {
    return Toolkit.getDefaultToolkit ()
                  .createDragGestureRecognizer (recognizer, this, c, actions,
                                                dgl);
  }

  public DragGestureRecognizer
    createDefaultDragGestureRecognizer(Component c, int actions,
                                       DragGestureListener dgl)
  {
    return createDragGestureRecognizer (MouseDragGestureRecognizer.class, c,
                                        actions, dgl);
  }

  /**
   * @since 1.4
   */
  public void addDragSourceListener(DragSourceListener l)
  {
    DnDEventMulticaster.add (dragSourceListener, l);
  }

  /**
   * @since 1.4
   */
  public void removeDragSourceListener(DragSourceListener l)
  {
    DnDEventMulticaster.remove (dragSourceListener, l);
  }

  /**
   * @since 1.4
   */
  public DragSourceListener[] getDragSourceListeners()
  {
    return (DragSourceListener[]) getListeners (DragSourceListener.class);
  }

  /**
   * @since 1.4
   */
  public void addDragSourceMotionListener(DragSourceMotionListener l)
  {
    DnDEventMulticaster.add (dragSourceMotionListener, l);
  }

  /**
   * @since 1.4
   */
  public void removeDragSourceMotionListener(DragSourceMotionListener l)
  {
    DnDEventMulticaster.remove (dragSourceMotionListener, l);
  }

  /**
   * @since 1.4
   */
  public DragSourceMotionListener[] getDragSourceMotionListeners ()
  {
    return (DragSourceMotionListener[]) getListeners
                                         (DragSourceMotionListener.class);
  }

  /**
   * @since 1.4
   */
  public EventListener[] getListeners (Class listenerType)
  {
    if (listenerType == DragSourceListener.class)
      return DnDEventMulticaster.getListeners (dragSourceListener,
                                               listenerType);

    if (listenerType == DragSourceMotionListener.class)
      return DnDEventMulticaster.getListeners (dragSourceMotionListener,
                                               listenerType);

    // Return an empty EventListener array.
    return new EventListener [0];
  }
} // class DragSource
